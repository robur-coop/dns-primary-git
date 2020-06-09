(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote git repository." ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/roburio/udns.git" doc))

let axfr =
  let doc = Key.Arg.info ~doc:"Allow unauthenticated zone transfer." ["axfr"] in
  Key.(create "axfr" Arg.(flag doc))

let seed =
  let doc = Key.Arg.info ~doc:"Seed for private key." ["seed"] in
  Key.(create "seed" Arg.(opt string "" doc))

let authenticator =
  let doc = Key.Arg.info ~doc:"Authenticator." ["authenticator"] in
  Key.(create "authenticator" Arg.(opt string "" doc))

let awa_pin = "git+https://github.com/hannesm/awa-ssh.git#future"
and git_pin = "git+https://github.com/hannesm/ocaml-git.git#awa-future"
and conduit_pin = "git+https://github.com/hannesm/ocaml-conduit.git#awa-future"

let dns_handler =
  let packages = [
    package "logs" ;
    package ~min:"4.3.0" ~sublibs:["mirage"; "zone"] "dns-server";
    package "dns-tsig";
    package ~min:"2.0.0" "irmin-mirage";
    package ~min:"2.0.0" "irmin-mirage-git";
    package ~pin:awa_pin "awa";
    package ~pin:awa_pin "awa-mirage";
    package ~pin:conduit_pin "conduit";
    package ~pin:conduit_pin "conduit-lwt";
    package ~pin:conduit_pin "conduit-mirage";
    package ~pin:git_pin "git";
    package ~pin:git_pin "git-http";
    package ~pin:git_pin "git-mirage";
  ] in
  foreign
    ~keys:[Key.abstract remote_k ; Key.abstract axfr ; Key.abstract seed ; Key.abstract authenticator]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4 @-> resolver @-> conduit @-> job)

let () =
  let net = generic_stackv4 default_network in
  register "primary-git"
    [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $
     default_time $ net $ resolver_dns net $ conduit_direct ~tls:true net]
