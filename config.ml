(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

(* boilerplate from https://github.com/mirage/ocaml-git.git unikernel/config.ml
   (commit #3bfcf215f959b71580e5c0b655700bb9484aee8c) *)
type mimic = Mimic

let mimic = typ Mimic

let mimic_conf =
  let packages = [ package "mimic" ] in
  let connect _ _modname = function
    | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
    | [ x ] -> Fmt.str "%s.ctx" x
    | _ -> Fmt.str "Lwt.return Mimic.empty" in
  impl ~packages ~connect "Mimic.Merge" (mimic @-> mimic @-> mimic)

let merge ctx0 ctx1 = mimic_conf $ ctx0 $ ctx1

let mimic_tcp_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  let connect _ modname = function
    | [ stack ] ->
      Fmt.str {ocaml|Lwt.return (%s.with_stack %s %s.ctx)|ocaml}
        modname stack modname
    | _ -> assert false in
  impl ~packages ~connect "Git_mirage_tcp.Make" (stackv4v6 @-> mimic)

let mimic_tcp_impl stackv4v6 = mimic_tcp_conf $ stackv4v6

let mimic_ssh_conf ~kind ~seed ~auth =
  let seed = Key.v seed in
  let auth = Key.v auth in
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  let connect _ modname = function
    | [ _; tcp_ctx; _ ] ->
        let with_key = match kind with
          | `Rsa -> "with_rsa_key"
          | `Ed25519 -> "with_ed25519_key" in
        Fmt.str
          {ocaml|let ssh_ctx00 = Mimic.merge %s %s.ctx in
                 let ssh_ctx01 = Option.fold ~none:ssh_ctx00 ~some:(fun v -> %s.%s v ssh_ctx00) %a in
                 let ssh_ctx02 = Option.fold ~none:ssh_ctx01 ~some:(fun v -> %s.with_authenticator v ssh_ctx01) %a in
                 Lwt.return ssh_ctx02|ocaml}
          tcp_ctx modname
          modname with_key Key.serialize_call seed
          modname Key.serialize_call auth
    | _ -> assert false in
  impl ~packages ~connect ~keys:[ seed; auth; ] "Git_mirage_ssh.Make" (stackv4v6 @-> mimic @-> mclock @-> mimic)

let mimic_ssh_impl ~kind ~seed ~auth stackv4v6 mimic_git mclock =
  mimic_ssh_conf ~kind ~seed ~auth $ stackv4v6 $ mimic_git $ mclock

let mimic_dns_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "dns" ] ] in
  let connect _ modname = function
    | [ _; _; _; stack; tcp_ctx ] ->
        Fmt.str
          {ocaml|let dns_ctx00 = Mimic.merge %s %s.ctx in
                 let dns_ctx01 = %s.with_dns %s dns_ctx00 in
                 Lwt.return dns_ctx01|ocaml}
          tcp_ctx modname
          modname stack
    | _ -> assert false in
  impl ~packages ~connect "Git_mirage_dns.Make" (random @-> mclock @-> time @-> stackv4v6 @-> mimic @-> mimic)

let mimic_dns_impl random mclock time stackv4v6 mimic_tcp =
  mimic_dns_conf $ random $ mclock $ time $ stackv4v6 $ mimic_tcp

type paf = Paf
let paf = typ Paf

let paf_conf () =
  let packages = [ package "paf" ~sublibs:[ "mirage" ] ] in
  impl ~packages "Paf_mirage.Make" (time @-> stackv4v6 @-> paf)

let paf_impl time stackv4v6 = paf_conf () $ time $ stackv4v6

let mimic_paf_conf () =
  let packages = [ package "git-paf" ] in
  let connect _ modname = function
    | [ _; _; _; _; tcp_ctx; ] ->
        Fmt.str
          {ocaml|let paf_ctx00 = Mimic.merge %s %s.ctx in
                 Lwt.return paf_ctx00|ocaml}
          tcp_ctx modname
    | _ -> assert false in
  impl ~packages ~connect "Git_paf.Make" (time @-> pclock @-> stackv4v6 @-> paf @-> mimic @-> mimic)

let mimic_paf_impl time pclock stackv4v6 paf mimic_tcp =
  mimic_paf_conf ()
  $ time
  $ pclock
  $ stackv4v6
  $ paf
  $ mimic_tcp

(* --- end of copied code --- *)

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote git repository." ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/roburio/udns.git" doc))

let axfr =
  let doc = Key.Arg.info ~doc:"Allow unauthenticated zone transfer." ["axfr"] in
  Key.(create "axfr" Arg.(flag doc))

let seed =
  let doc = Key.Arg.info ~doc:"Seed for private key." ["seed"] in
  Key.(create "seed" Arg.(opt (some string) None doc))

let authenticator =
  let doc = Key.Arg.info ~doc:"Authenticator." ["authenticator"] in
  Key.(create "authenticator" Arg.(opt (some string) None doc))

let mimic_impl ~kind ~seed ~authenticator stackv4v6 random mclock pclock time paf =
  let mtcp = mimic_tcp_impl stackv4v6 in
  let mdns = mimic_dns_impl random mclock time stackv4v6 mtcp in
  let mssh = mimic_ssh_impl ~kind ~seed ~auth:authenticator stackv4v6 mtcp mclock in
  let mpaf = mimic_paf_impl time pclock stackv4v6 paf mtcp in
  merge mpaf (merge mssh mdns)

let net = generic_stackv4v6 default_network

let mimic_impl =
  mimic_impl ~kind:`Rsa ~seed ~authenticator net
    default_random default_monotonic_clock default_posix_clock default_time
    (paf_impl default_time net)

let dns_handler =
  let packages = [
    package "logs" ;
    package ~min:"5.0.0" ~sublibs:["mirage"; "zone"] "dns-server";
    package "dns-tsig";
    package ~min:"2.6.0" "irmin-mirage";
    package ~min:"2.6.0" "irmin-mirage-git";
    package "git-paf";
    package ~sublibs:["cohttp"] "paf";
  ] in
  foreign
    ~keys:[Key.v remote_k ; Key.v axfr]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> mimic @-> job)

let () =
  register "primary-git"
    [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $
     default_time $ net $ mimic_impl]
