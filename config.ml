(* mirage >= 4.5.0 & < 4.6.0 *)
(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
open Mirage

let ssh_key =
  Runtime_arg.create ~pos:__POS__ ~name:"ssh_key"
    {|let open Cmdliner in
      let doc = Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
      Arg.(value & opt (some string) None doc)|}

let ssh_password =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
     let doc = Arg.info ~doc:"The private SSH password." [ "ssh-password" ] in
      Arg.(value & opt (some string) None doc)|}

let authenticator =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
     let doc = Arg.info ~doc:"SSH authenticator." ["authenticator"] in
      Arg.(value & opt (some string) None doc)|}

let remote = runtime_arg ~pos:__POS__ "Unikernel.K.remote"
let axfr = runtime_arg ~pos:__POS__ "Unikernel.K.axfr"

let dns_handler =
  let packages = [
    package "logs";
    package ~min:"6.2.2" ~sublibs:["mirage"; "zone"] "dns-server";
    package "dns-tsig";
    package ~min:"0.0.3" "git-kv";
    package ~min:"4.3.1" "mirage-runtime";
  ] in
  main
    ~runtime_args:[ remote; axfr ]
    ~packages
    ~pos:__POS__ "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> git_client @-> job)

let stack = generic_stackv4v6 default_network

let git =
  let dns = generic_dns_client stack in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  let git = mimic_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~password:ssh_password ~authenticator tcp git)
                       (git_http tcp git))

let enable_monitoring =
  let doc = Key.Arg.info
      ~doc:"Enable monitoring (syslog, metrics to influx, log level, statmemprof tracing)"
      [ "enable-monitoring" ]
  in
  Key.(create "enable-monitoring" Arg.(flag doc))

let management_stack =
  if_impl
    (Key.value enable_monitoring)
    (generic_stackv4v6 ~group:"management" (netif ~group:"management" "management"))
    stack

let docs = "MONITORING PARAMETERS"

let name =
  runtime_arg ~pos:__POS__ ~name:"name"
    {|(let doc = Cmdliner.Arg.info ~doc:"Name of the unikernel" ~docs:%S [ "name" ] in
       Cmdliner.Arg.(value & opt string "a.ns.robur.coop" doc))|} docs

let monitoring =
  let monitor = Runtime_arg.(v (monitor ~docs None)) in
  let connect _ modname = function
    | [ _ ; _ ; stack ; name ; monitor ] ->
      code ~pos:__POS__
        "Lwt.return (match %s with\
         | None -> Logs.warn (fun m -> m \"no monitor specified, not outputting statistics\")\
         | Some ip -> %s.create ip ~hostname:%s %s)"
        monitor modname name stack
    | _ -> assert false
  in
  impl
    ~packages:[ package "mirage-monitoring" ]
    ~runtime_args:[ name ; monitor ]
    ~connect "Mirage_monitoring.Make"
    (time @-> pclock @-> stackv4v6 @-> job)

let syslog =
  let syslog = Runtime_arg.(v (syslog ~docs None)) in
  let connect _ modname = function
    | [ _ ; stack ; name ; syslog ] ->
      code ~pos:__POS__
        "Lwt.return (match %s with\
         | None -> Logs.warn (fun m -> m \"no syslog specified, dumping on stdout\")\
         | Some ip -> Logs.set_reporter (%s.create %s ip ~hostname:%s ()))"
        syslog modname stack name
    | _ -> assert false
  in
  impl
    ~packages:[ package ~sublibs:["mirage"] ~min:"0.4.0" "logs-syslog" ]
    ~runtime_args:[ name ; syslog ]
    ~connect "Logs_syslog_mirage.Udp"
    (pclock @-> stackv4v6 @-> job)

let optional_monitoring time pclock stack =
  if_impl (Key.value enable_monitoring)
    (monitoring $ time $ pclock $ stack)
    noop

let optional_syslog pclock stack =
  if_impl (Key.value enable_monitoring)
    (syslog $ pclock $ stack)
    noop

let () =
  register "primary-git"
    [
      optional_syslog default_posix_clock management_stack ;
      optional_monitoring default_time default_posix_clock management_stack ;
      dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ stack $ git
    ]
