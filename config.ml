(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote git repository. Use '#' as a separator for a branch name." ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/roburio/udns.git" doc))

let axfr =
  let doc = Key.Arg.info ~doc:"Allow unauthenticated zone transfer." ["axfr"] in
  Key.(create "axfr" Arg.(flag doc))

let ssh_key =
  let doc = Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

let authenticator =
  let doc = Key.Arg.info ~doc:"SSH authenticator." ["authenticator"] in
  Key.(create "authenticator" Arg.(opt (some string) None doc))

let dns_handler =
  let packages = [
    package "logs";
    package ~min:"6.2.2" ~sublibs:["mirage"; "zone"] "dns-server";
    package "dns-tsig";
    package ~min:"0.0.2" "git-kv";
    package ~min:"4.3.1" "mirage-runtime";
  ] in
  foreign
    ~keys:[ Key.v remote_k; Key.v axfr ]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> git_client @-> job)

let net = generic_stackv4v6 default_network

let git =
  let dns = generic_dns_client net in
  let tcp = tcpv4v6_of_stackv4v6 net in
  let git = git_happy_eyeballs net dns (generic_happy_eyeballs net dns) in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~authenticator tcp git)
                       (git_http tcp git))

let () =
  register "primary-git"
    [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $
     default_time $ net $ git]
