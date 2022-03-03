(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

(* TODO: instead of storing all zones flat, maybe put them into subdirectories *)

open Lwt.Infix

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Tcpip.Stack.V4V6) (_ : sig end) = struct

  module Store = Irmin_mirage_git.Mem.KV(Irmin.Contents.String)
  module Sync = Irmin.Sync(Store)

  let connect_store ctx =
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >|= fun repo ->
    repo, Store.remote ~ctx (Key_gen.remote ())

  let pull_store repo upstream =
    Logs.debug (fun m -> m "pulling from remote!");
    Sync.pull ~depth:1 repo upstream `Set >|= function
    | Ok `Empty -> Error (`Msg "pull_store: pulled empty repository")
    | Ok (`Head _ as s) ->
      Logs.debug (fun m -> m "pull_store: ok, pulled %a!" Sync.pp_status s);
      Ok ()
    | Error (`Msg e) -> Error (`Msg ("pull_store: error " ^ e))
    | Error (`Conflict msg) -> Error (`Msg ("pull_store: conflict " ^ msg))

  let load_zones store upstream =
    pull_store store upstream >>= function
    | Error _ as e -> Lwt.return e
    | Ok () ->
      Store.list store [] >>= fun files ->
      Lwt_list.fold_left_s (fun acc (name, kind) ->
          match acc with
          | Error _ as e -> Lwt.return e
          | Ok acc -> match Store.Tree.destruct kind, Domain_name.of_string name with
            | `Node _, _ ->
              let msg = Fmt.str "load_zones %S: expected contents, got node" name in
              Lwt.return (Error (`Msg msg))
            | `Contents _, Error (`Msg e) ->
              let msg = Fmt.str "load_zones %S: noe a domain name %s" name e in
              Lwt.return (Error (`Msg msg))
            | `Contents _, Ok zone ->
              Store.get store [name] >|= fun data ->
              Ok ((zone, data) :: acc))
        (Ok []) files

  let load_git old_trie store upstream =
    load_zones store upstream >|= function
    | Error _ as e -> e
    | Ok bindings ->
      let ( let* ) = Result.bind in
      Logs.debug (fun m -> m "found %d bindings: %a" (List.length bindings)
                     Fmt.(list ~sep:(any ",@ ") (pair ~sep:(any ": ") Domain_name.pp int))
                     (List.map (fun (k, v) -> k, String.length v) bindings)) ;
      (* split into keys and zones *)
      let keys, data =
        let is_key subdomain =
          Domain_name.is_subdomain ~domain:(Domain_name.of_string_exn "_keys") ~subdomain
        in
        let keys, data = List.partition (fun (name, _) -> is_key name) bindings in
        List.map (fun (n, v) -> Domain_name.drop_label_exn ~rev:true n, v) keys,
        data
      in
      let zones = Domain_name.Set.of_list (fst (List.split data)) in
      let parse_and_maybe_add trie zone data =
        Logs.debug (fun m -> m "parsing %a: %s" Domain_name.pp zone data);
        let* rrs = Dns_zone.parse data in
        (* we take all resource records within the zone *)
        let in_zone subdomain = Domain_name.is_subdomain ~domain:zone ~subdomain in
        let zone_rrs = Domain_name.Map.filter (fun name _ -> in_zone name) rrs in
        let trie' = Dns_trie.insert_map zone_rrs trie in
        let* _ =
          Result.map_error
            (fun _ -> `Msg (Fmt.str "no SOA for %a" Domain_name.pp zone))
            (Dns_trie.lookup zone Dns.Rr_map.Soa trie')
        in
        let* () =
          Result.map_error
            (fun e -> `Msg (Fmt.to_to_string Dns_trie.pp_zone_check e))
            (Dns_trie.check trie')
        in
        let* () =
          match old_trie with
          | None -> Ok ()
          | Some old_trie ->
            match Dns_trie.entries zone old_trie, Dns_trie.lookup zone Dns.Rr_map.Soa trie' with
            | Ok (old_soa, old_entries), Ok soa ->
              (* good if old_soa = soa && old_entries ++ old_soa == zone_rrs
                 or soa is newer than old_soa *)
              (* TODO error recovery could be to increment the SOA serial, followed
                 by a push to git (the other errors above and below can't be fixed
                 automatically - thus a git pull can always fail :/) *)
              if Dns.Soa.newer ~old:old_soa soa then
                Ok ()
              else if
                Dns.Name_rr_map.(equal zone_rrs
                                   (add zone Dns.Rr_map.Soa old_soa old_entries))
              then
                Ok ()
              else
                Error (`Msg (Fmt.str "SOA serial has not been incremented for %a"
                               Domain_name.pp zone))
            | Error _, _ -> Ok ()
            | _, Error _ -> Ok ()
        in
        (* collect potential glue:
           - find NS entries for zone
           - find A records for name servers in other zones
             (Dns_trie.check ensures that the NS in zone have an address record)
           - only if the other names are not in zones, they are picked from
             this zone file *)
        let* (_, name_servers) =
          Result.map_error
            (fun e -> `Msg (Fmt.to_to_string Dns_trie.pp_e e))
            (Dns_trie.lookup zone Dns.Rr_map.Ns trie')
        in
        let not_in_zones nameserver =
          let in_this_zone = in_zone nameserver
          and in_other_zones =
            if Domain_name.Set.exists
                (fun domain -> Domain_name.is_subdomain ~domain ~subdomain:nameserver)
                zones
            then begin
              Logs.debug (fun m -> m "ignoring glue for NS %a in %a since authoritative for that zone"
                            Domain_name.pp nameserver Domain_name.pp zone);
              true
            end else false
          in
          not (in_this_zone || in_other_zones)
        in
        let need_glue = Domain_name.Host_set.filter not_in_zones name_servers in
        let trie' =
          Domain_name.Host_set.fold (fun ns trie ->
              match Dns.Name_rr_map.find (Domain_name.raw ns) Dns.Rr_map.A rrs with
              | Some rr -> Dns_trie.insert ns Dns.Rr_map.A rr trie
              | None ->
                Logs.warn (fun m -> m "unknown IP for NS %a, it won't get notified"
                              Domain_name.pp ns);
                trie) need_glue trie'
        in
        (* this prints all zones of the trie' *)
        let* zone_data = Dns_server.text zone trie' in
        Logs.info (fun m -> m "loaded zone %a" Domain_name.pp zone);
        Logs.debug (fun m -> m "loaded zone@.%s" zone_data);
        Ok trie'
      in
      let data_trie =
        List.fold_left (fun trie (zone, data) ->
            match parse_and_maybe_add trie zone data with
            | Ok trie' -> trie'
            | Error (`Msg msg) ->
              Logs.warn (fun m -> m "ignoring malformed zone %a: %s" Domain_name.pp zone msg);
              trie)
          Dns_trie.empty data
      in
      let parse_keys zone keys =
        let* rrs = Dns_zone.parse keys in
        let tst subdomain = Domain_name.is_subdomain ~domain:zone ~subdomain in
        if not (Domain_name.Map.for_all (fun name _ -> tst name) rrs) then
          Error (`Msg "key name not in zone")
        else
          (* from the name_rr_map extract a (Domain_name.t * Dnskey) list *)
          let keys =
            Domain_name.Map.fold (fun n data acc ->
                match Dns.Rr_map.(find Dnskey data) with
                | None ->
                  Logs.warn (fun m -> m "no dnskey found %a" Domain_name.pp n);
                  acc
                | Some (_, keys) ->
                  match Dns.Rr_map.Dnskey_set.elements keys with
                  | [ x ] ->
                    Logs.debug (fun m -> m "inserting dnskey %a" Domain_name.pp n);
                    (n, x) :: acc
                  | xs ->
                    Logs.warn (fun m -> m "%d dnskeys for %a (expected exactly one)"
                                  (List.length xs) Domain_name.pp n);
                    acc)
              rrs []
          in
          Ok keys
      in
      let keys =
        List.fold_left (fun acc (zone, keys) ->
            match parse_keys zone keys with
            | Ok keys -> keys @ acc
            | Error (`Msg msg) ->
              Logs.warn (fun m -> m "ignoring key %a %s" Domain_name.pp zone msg);
              acc)
          [] keys
      in
      Ok (data_trie, keys)

  let store_zone key ip t store zone =
    match Dns_server.text zone (Dns_server.Primary.data t) with
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error while converting zone %a: %s" Domain_name.pp zone msg) ;
      Lwt.return_unit
    | Ok data ->
      let info () =
        let date = Int64.of_float Ptime.Span.(to_float_s (v (P.now_d_ps ())))
        and commit = Fmt.str "%a changed %a" Ipaddr.pp ip Domain_name.pp zone
        and author = Fmt.str "%a via pimary git" Fmt.(option ~none:(any "no key") Domain_name.pp) key
        in
        Irmin.Info.v ~date ~author commit
      in
      Store.set ~info store [Domain_name.to_string zone] data >|= function
      | Ok () -> ()
      | Error _ -> Logs.err (fun m -> m "error while writing to store")

  let store_zones ~old key ip t store upstream =
    (* TODO do a single commit!
       - either KV and batch (but no commit context)
       - or Store.set_tree but dunno what the tree should be? all zones? *)
    let data = Dns_server.Primary.data t in
    let zones =
      Dns_trie.fold Dns.Rr_map.Soa data
        (fun dname soa acc ->
           match Dns_trie.lookup dname Dns.Rr_map.Soa old with
           | Ok old when Dns.Soa.newer ~old soa -> dname :: acc
           | Ok _ -> acc
           | Error _ -> dname :: acc)
        []
    in
    Lwt_list.iter_s (store_zone key ip t store) zones >>= fun () ->
    (* TODO removal of a zone should lead to dropping this zone from git! *)
    Logs.debug (fun m -> m "pushing to remote!");
    Sync.push store upstream  >|= function
    | Ok `Empty -> Logs.warn (fun m -> m "pushed empty zonefiles")
    | Ok (`Head _ as s) -> Logs.info (fun m -> m "pushed zonefile commit %a" Sync.pp_status s)
    | Error pe -> Logs.err (fun m -> m "push error %a" Sync.pp_push_error pe)

  module D = Dns_server_mirage.Make(P)(M)(T)(S)

  let start _rng _pclock _mclock _time s ctx =
    connect_store ctx >>= fun (store, upstream) ->
    load_git None store upstream >>= function
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error during loading git %s" msg);
      Lwt.fail_with "git pull failed"
    | Ok (trie, keys) ->
      let on_update ~old ~authenticated_key ~update_source t =
        store_zones ~old authenticated_key update_source t store upstream
      and on_notify n t =
        match n with
        | `Notify soa ->
          Logs.err (fun m -> m "ignoring normal notify %a" Fmt.(option ~none:(any "no soa") Dns.Soa.pp) soa);
          Lwt.return None
        | `Signed_notify soa ->
          Logs.debug (fun m -> m "got notified, checking out %a" Fmt.(option ~none:(any "no soa") Dns.Soa.pp) soa);
          load_git (Some (Dns_server.Primary.data t)) store upstream >|= function
          | Error (`Msg msg) ->
            Logs.err (fun m -> m "error %s while loading git while in notify, continuing with old data" msg);
            None
          | Ok trie ->
            Logs.debug (fun m -> m "loaded a new trie from git!");
            Some trie
      in
      let t =
        let unauthenticated_zone_transfer = Key_gen.axfr () in
        Dns_server.Primary.create ~keys ~unauthenticated_zone_transfer
          ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign ~rng:R.generate
          trie
      in
      D.primary ~on_update ~on_notify s t ;
      S.listen s
end
