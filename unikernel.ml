(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Tcpip.Stack.V4V6) (_ : sig end) = struct

  module Store = Git_kv.Make(P)

  let zones store =
    Store.list store Mirage_kv.Key.empty >|= function
    | Error e ->
      Logs.warn (fun m -> m "error %a while listing store" Store.pp_error e);
      []
    | Ok files ->
      List.fold_left (fun acc (name, kind) ->
          match kind with
          | `Dictionary ->
            Logs.info (fun m -> m "zones ignoring dictionary %a (expected value)"
                          Mirage_kv.Key.pp name);
            acc
          | `Value -> name :: acc)
        [] files

  let load_zones old_trie store =
    zones store >>= fun zone_keys ->
    Lwt_list.fold_left_s (fun acc key ->
        Store.get store key >|= function
        | Error e ->
          Logs.warn (fun m -> m "error %a while retrieving %a from store"
                        Store.pp_error e Mirage_kv.Key.pp key);
          acc
        | Ok data -> (Mirage_kv.Key.basename key, data) :: acc)
      [] zone_keys >|= fun bindings ->
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

  let store_zones ~old key ip t store =
    zones store >>= fun old_zones ->
    let old_zones =
      let key_domain = Domain_name.of_string_exn "_keys" in
      List.fold_left (fun acc key ->
          let zone = Mirage_kv.Key.basename key in
          match Domain_name.of_string zone with
          | Error (`Msg msg) ->
            Logs.warn (fun m -> m "couldn't convert %s to a domain name: %s"
                          zone msg);
            acc
          | Ok dn ->
            if Domain_name.is_subdomain ~domain:key_domain ~subdomain:dn then
              acc
            else
              Domain_name.Set.add dn acc)
        Domain_name.Set.empty old_zones
    in
    let data = Dns_server.Primary.data t in
    let old_zones, zones =
      Dns_trie.fold Dns.Rr_map.Soa data
        (fun dname soa (old_zones, acc) ->
           Domain_name.Set.remove dname old_zones,
           match Dns_trie.lookup dname Dns.Rr_map.Soa old with
           | Ok old when Dns.Soa.newer ~old soa -> dname :: acc
           | Ok _ -> acc
           | Error _ -> dname :: acc)
        (old_zones, [])
    in
    let message = Fmt.str "changed by %a" Ipaddr.pp ip
    and author = Fmt.str "%a via primary git" Fmt.(option ~none:(any "no key") Domain_name.pp) key
    in
    Store.change_and_push store ~author ~message
      (fun s ->
         Domain_name.Set.fold (fun name acc ->
             acc >>= fun () ->
             let key = Mirage_kv.Key.v (Domain_name.to_string name) in
             Store.remove s key >|= function
             | Ok () -> ()
             | Error e -> Logs.err (fun m -> m "error %a while removing %a"
                                       Store.pp_write_error e Mirage_kv.Key.pp key))
           old_zones Lwt.return_unit >>= fun () ->
         Lwt_list.iter_s (fun zone ->
             match Dns_server.text zone data with
             | Error (`Msg msg) ->
               Logs.err (fun m -> m "error while converting zone %a: %s" Domain_name.pp zone msg) ;
               Lwt.return_unit
             | Ok data ->
               Store.set s (Mirage_kv.Key.v (Domain_name.to_string zone)) data >|= function
               | Ok () -> ()
               | Error e ->
                 Logs.err (fun m -> m "error while writing %a to store: %a"
                              Domain_name.pp zone Store.pp_write_error e))
           zones) >|= function
    | Ok () -> ()
    | Error e ->
      Logs.err (fun m -> m "change_and_push failed with %a" Store.pp_write_error e)

  module D = Dns_server_mirage.Make(P)(M)(T)(S)

  let start _rng _pclock _mclock _time s ctx =
    Lwt.catch (fun () ->
        Git_kv.connect ctx (Key_gen.remote ()))
      (function
        | Invalid_argument err ->
          Logs.err (fun m -> m "couldn't initialize git repository %s: %s"
                       (Key_gen.remote ()) err);
          exit Mirage_runtime.argument_error
        | e -> raise e) >>= fun store ->
    load_zones None store >>= function
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error during loading git %s" msg);
      Lwt.fail_with "git pull failed"
    | Ok (trie, keys) ->
      let on_update ~old ~authenticated_key ~update_source t =
        store_zones ~old authenticated_key update_source t store
      and on_notify n t =
        match n with
        | `Notify soa ->
          Logs.err (fun m -> m "ignoring normal notify %a" Fmt.(option ~none:(any "no soa") Dns.Soa.pp) soa);
          Lwt.return None
        | `Signed_notify soa ->
          Logs.debug (fun m -> m "got notified, checking out %a" Fmt.(option ~none:(any "no soa") Dns.Soa.pp) soa);
          Git_kv.pull store >>= function
          | Error (`Msg msg) ->
            Logs.err (fun m -> m "error %s while pulling git in notify, continuing with old data" msg);
            Lwt.return None
          | Ok diff ->
            (match diff with [] -> Logs.info (fun m -> m "pulled, empty diff") | _ -> ());
            load_zones (Some (Dns_server.Primary.data t)) store >|= function
            | Error (`Msg msg) ->
              Logs.err (fun m -> m "error %s while loading git in notify, continuing with old data" msg);
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
