(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

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
          match Store.Tree.destruct kind with
          | `Node _ ->
            Logs.info (fun m -> m "load_zones ignoring %S (expected content)"
                          name);
            Lwt.return acc
          | `Contents _ ->
            Store.get store [name] >|= fun data ->
            (name, data) :: acc)
        [] files >|= fun zones ->
      Ok zones

  let load_git old_trie store upstream =
    load_zones store upstream >|= function
    | Error _ as e -> e
    | Ok bindings ->
      let zones, trie, keys = Dns_zone.decode_zones_keys bindings in
      (match old_trie with
       | None -> ()
       | Some old_trie ->
         Domain_name.Set.iter (fun zone ->
             match
               Dns_trie.entries zone old_trie,
               Dns_trie.entries zone trie
             with
             | Ok (old_soa, old_entries), Ok (soa, entries) ->
               (* good if old_soa = soa && old_entries ++ old_soa == zone_rrs
                  or soa is newer than old_soa *)
               (* TODO error recovery could be to increment the SOA serial,
                  followed by a push to git (the other errors above and below
                  can't be fixed automatically - thus a git pull can always
                  fail :/) *)
               let equal =
                 Dns.Name_rr_map.(equal
                                   (add zone Dns.Rr_map.Soa soa entries)
                                   (add zone Dns.Rr_map.Soa old_soa old_entries))
               in
               if not (Dns.Soa.newer ~old:old_soa soa) && not equal then
                 Logs.warn (fun m -> m "SOA serial not incremented for %a"
                               Domain_name.pp zone)
                 | Error _, Ok _ | Ok _, Error _ | Error _, Error _ -> ())
           zones);
      Ok (trie, keys)

  let store_zone key ip t store zone =
    match Dns_server.text zone (Dns_server.Primary.data t) with
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error while converting zone %a: %s" Domain_name.pp zone msg) ;
      Lwt.return_unit
    | Ok data ->
      let info () =
        let date = Int64.of_float Ptime.Span.(to_float_s (v (P.now_d_ps ())))
        and commit = Fmt.str "%a changed %a" Ipaddr.pp ip Domain_name.pp zone
        and author = Fmt.str "%a via primary git" Fmt.(option ~none:(any "no key") Domain_name.pp) key
        in
        Irmin.Info.v ~date ~author commit
      in
      Store.set ~info store [Domain_name.to_string zone] data >|= function
      | Ok () -> ()
      | Error _ -> Logs.err (fun m -> m "error while writing to store")

  let store_zones ~old key ip t store upstream =
    (* TODO do a single commit (maybe) *)
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
