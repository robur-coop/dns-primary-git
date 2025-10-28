(* mirage >= 4.10.0 & < 4.11.0 *)
(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
open Mirage

let dns_handler =
  let packages = [
    package "logs";
    package ~min:"6.2.2" ~sublibs:["mirage"; "zone"] "dns-server";
    package "dns-tsig";
    package ~min:"0.2.0" "git-kv";
    package ~min:"4.3.1" "mirage-runtime";
  ] in
  main
    ~packages
    ~pos:__POS__ "Unikernel.Main"
    (stackv4v6 @-> git_client @-> job)

(* uTCP *)

let tcpv4v6_direct_conf id =
  let packages_v = Key.pure [ package "utcp" ~sublibs:[ "mirage" ] ] in
  let connect _ modname = function
    | [ip] ->
      code ~pos:__POS__ "Lwt.return (%s.connect %S %s)" modname id ip
    | _ -> failwith "direct tcpv4v6"
  in
  impl ~packages_v ~connect "Utcp_mirage.Make"
    (ipv4v6 @-> (tcp: 'a tcp typ))

let direct_tcpv4v6 id ip =
  tcpv4v6_direct_conf id $ ip

let net ?group name netif =
  let ethernet = ethif netif in
  let arp = arp ethernet in
  let i4 = create_ipv4 ?group ethernet arp in
  let i6 = create_ipv6 ?group netif ethernet in
  let i4i6 = create_ipv4v6 ?group i4 i6 in
  let tcpv4v6 = direct_tcpv4v6 name i4i6 in
  direct_stackv4v6 ?group ~tcp:tcpv4v6 netif ethernet arp i4 i6

let use_utcp =
  let doc = Key.Arg.info ~doc:"Use uTCP" [ "use-utcp" ] in
  Key.(create "use-utcp" Arg.(flag doc))

let stack =
  if_impl
    (Key.value use_utcp)
    (net "service" default_network)
    (generic_stackv4v6 default_network)

let git =
  let he = generic_happy_eyeballs stack in
  let dns = generic_dns_client stack he in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  let git = mimic_happy_eyeballs stack he dns in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh tcp git) (git_http tcp git))

let enable_monitoring =
  let doc = Key.Arg.info
      ~doc:"Enable monitoring (syslog, metrics to influx, log level, statmemprof tracing)"
      [ "enable-monitoring" ]
  in
  Key.(create "enable-monitoring" Arg.(flag doc))

let management_stack =
  if_impl
    (Key.value enable_monitoring)
    (if_impl
       (Key.value use_utcp)
       (net ~group:"management" "management" (netif ~group:"management" "management"))
       (generic_stackv4v6 ~group:"management" (netif ~group:"management" "management")))
    stack

let monitoring =
  let monitor = Runtime_arg.(v (monitor None)) in
  let connect _ modname = function
    | [ stack ; monitor ] ->
      code ~pos:__POS__
        "Lwt.return (match %s with\
         | None -> Logs.warn (fun m -> m \"no monitor specified, not outputting statistics\")\
         | Some ip -> %s.create ip ~hostname:(Mirage_runtime.name ()) %s)"
        monitor modname stack
    | _ -> assert false
  in
  impl
    ~packages:[ package ~min:"0.0.6" "mirage-monitoring" ]
    ~runtime_args:[ monitor ]
    ~connect "Mirage_monitoring.Make"
    (stackv4v6 @-> job)

let syslog =
  let syslog = Runtime_arg.(v (syslog None)) in
  let connect _ modname = function
    | [ stack ; syslog ] ->
      code ~pos:__POS__
        "Lwt.return (match %s with\
         | None -> Logs.warn (fun m -> m \"no syslog specified, dumping on stdout\")\
         | Some ip -> Logs.set_reporter (%s.create %s ip ~hostname:(Mirage_runtime.name ()) ()))"
        syslog modname stack
    | _ -> assert false
  in
  impl
    ~packages:[ package ~sublibs:["mirage"] ~min:"0.5.0" "logs-syslog" ]
    ~runtime_args:[ syslog ]
    ~connect "Logs_syslog_mirage.Udp"
    (stackv4v6 @-> job)

let optional_monitoring stack =
  if_impl (Key.value enable_monitoring)
    (monitoring $ stack)
    noop

let optional_syslog stack =
  if_impl (Key.value enable_monitoring)
    (syslog $ stack)
    noop

let () =
  register "primary-git"
    [
      optional_syslog management_stack ;
      optional_monitoring management_stack ;
      dns_handler $ stack $ git
    ]
