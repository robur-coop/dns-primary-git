(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

(* boilerplate from https://github.com/mirage/ocaml-git.git
   unikernel/empty-commit/config.ml
   commit #015b4400fb65488c12f988a167da52a104c2e603 *)
type mimic = Mimic

let mimic = typ Mimic

let mimic_impl =
  let packages = [ package "mimic" ] in
  let connect _ _modname = function
    | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
    | [ x ] -> Fmt.str "%s.ctx" x
    | _ -> Fmt.str "Lwt.return Mimic.empty" in
  impl ~packages ~connect "Mimic.Merge" (mimic @-> mimic @-> mimic)

let merge ctx0 ctx1 = mimic_impl $ ctx0 $ ctx1

let git_happy_eyeballs =
  let packages = [ package "git-mirage"
                      ~sublibs:[ "happy-eyeballs" ]
                      ~min:"3.8.0" ] in
  let connect _ modname = function
    | [ _random; _time; _mclock; _pclock; stackv4v6; ] ->
      Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
    | _ -> assert false in
  impl ~packages ~connect "Git_mirage_happy_eyeballs.Make"
    (random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> mimic)

let git_tcp =
  let packages = [ package "git-mirage"
                      ~sublibs:[ "tcp" ]
                      ~min:"3.8.0" ] in
  let connect _ modname = function
    | [ _tcpv4v6; ctx ] ->
      Fmt.str {ocaml|%s.connect %s|ocaml} modname ctx
    | _ -> assert false in
  impl ~packages ~connect "Git_mirage_tcp.Make"
    (tcpv4v6 @-> mimic @-> mimic)

let git_ssh ?authenticator key =
  let packages = [ package "git-mirage"
                      ~sublibs:[ "ssh" ]
                      ~min:"3.8.0" ] in
  let connect _ modname = function
    | [ _mclock; _tcpv4v6; _time; ctx ] ->
      ( match authenticator with
      | None ->
        Fmt.str {ocaml|%s.connect %s >>= %s.with_optionnal_key ~key:%a|ocaml}
          modname ctx modname Key.serialize_call (Key.v key)
      | Some authenticator ->
        Fmt.str {ocaml|%s.connect %s >>= %s.with_optionnal_key ?authenticator:%a ~key:%a|ocaml}
          modname ctx modname
          Key.serialize_call (Key.v authenticator)
          Key.serialize_call (Key.v key) )
    | _ -> assert false in
  let keys = match authenticator with
    | Some authenticator -> [ Key.v key; Key.v authenticator ]
    | None -> [ Key.v key ] in
  impl ~packages ~connect ~keys "Git_mirage_ssh.Make"
    (mclock @-> tcpv4v6 @-> time @-> mimic @-> mimic)

let git_http ?authenticator headers =
  let packages = [ package "git-mirage"
                      ~sublibs:[ "http" ]
                      ~min:"3.8.0" ] in
  let keys =
    let keys = [] in
    let keys = match headers with Some headers -> Key.v headers :: keys | None -> keys in
    let keys = match authenticator with Some authenticator -> Key.v authenticator :: keys | None -> [] in
    keys in
  let connect _ modname = function
    | [ _time; _pclock; _tcpv4v6; ctx; ] ->
      let serialize_headers ppf = function
        | None -> ()
        | Some headers -> Fmt.pf ppf " ?headers:%a" Key.serialize_call (Key.v headers) in
      let serialize_authenticator ppf = function
        | None -> ()
        | Some authenticator -> Fmt.pf ppf " ?authenticator:%a" Key.serialize_call (Key.v authenticator) in
      Fmt.str {ocaml|%s.connect %s >>= fun ctx -> %s.with_optional_tls_config_and_headers%a%a ctx|ocaml}
        modname ctx modname
        serialize_authenticator authenticator
        serialize_headers headers
    | _ -> assert false in
  impl ~packages ~connect ~keys "Git_mirage_http.Make"
    (time @-> pclock @-> tcpv4v6 @-> mimic @-> mimic)

let tcpv4v6_of_stackv4v6 =
  let connect _ modname = function
    | [ stackv4v6 ] -> Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
    | _ -> assert false in
  impl ~connect "Git_mirage_happy_eyeballs.TCPV4V6"
    (stackv4v6 @-> tcpv4v6)

type hash = Hash

let hash = typ Hash

let sha1 = impl ~packages:[ package "digestif" ] "Digestif.SHA1" hash

type git = Git

let git = typ Git

let git_impl path =
  let packages = [ package "git" ~min:"3.8.0" ] in
  let keys = match path with
    | None -> []
    | Some path -> [ Key.v path ] in
  let connect _ modname _ = match path with
    | None ->
        Fmt.str
          {ocaml|%s.v (Fpath.v ".") >>= function
                  | Ok v -> Lwt.return v
                  | Error err -> Fmt.failwith "%%a" %s.pp_error err|ocaml}
          modname modname
    | Some key ->
        Fmt.str
          {ocaml|( match Option.map Fpath.of_string %a with
                  | Some (Ok path) -> %s.v path
                  | Some (Error (`Msg err)) -> failwith err
                  | None -> %s.v (Fpath.v ".") ) >>= function
                  | Ok v -> Lwt.return v
                  | Error err -> Fmt.failwith "%%a" %s.pp_error err|ocaml}
          Key.serialize_call (Key.v key) modname modname modname in
  impl ~packages ~keys ~connect "Git.Mem.Make" (hash @-> git)

(* --- end of copied code --- *)

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote git repository." ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/mirage/ocaml-dns" doc))

let branch =
  let doc = Key.Arg.info ~doc:"Remote git repository branch." ["b"; "branch"] in
  Key.(create "branch" Arg.(opt string "main" doc))

let axfr =
  let doc = Key.Arg.info ~doc:"Allow unauthenticated zone transfer." ["axfr"] in
  Key.(create "axfr" Arg.(flag doc))

let ssh_key =
  let doc = Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH public key of the remote Git repository." [ "ssh-authenticator" ] in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let https_authenticator =
  let doc = Key.Arg.info ~doc:"SSH public key of the remote Git repository." [ "https-authenticator" ] in
  Key.(create "https_authenticator" Arg.(opt (some string) None doc))

let mimic_impl random stackv4v6 mclock pclock time =
  let tcpv4v6 = tcpv4v6_of_stackv4v6 $ stackv4v6 in
  let mhappy_eyeballs = git_happy_eyeballs $ random $ time $ mclock $ pclock $ stackv4v6 in
  let mtcp  = git_tcp
    $ tcpv4v6 $ mhappy_eyeballs in
  let mssh  = git_ssh ~authenticator:ssh_authenticator ssh_key
    $ mclock $ tcpv4v6 $ time $ mhappy_eyeballs in
  let mhttp = git_http ~authenticator:https_authenticator None
    $ time $ pclock $ tcpv4v6 $ mhappy_eyeballs in
  merge mhttp (merge mtcp mssh)

let net = generic_stackv4v6 default_network

let mimic_impl =
  mimic_impl default_random net
    default_monotonic_clock default_posix_clock default_time

let dns_handler =
  let packages = [
    package "logs" ;
    package ~min:"5.0.0" ~sublibs:["mirage"; "zone"] "dns-server";
    package "dns-tsig";
    package ~min:"2.10.0" "irmin-mirage";
    package ~min:"2.10.0" "irmin-mirage-git";
    package ~min:"3.8.0" "git-mirage" ~sublibs:["happy-eyeballs"; "tcp"; "ssh"; "tcp"];
    package ~min:"3.8.0" "git-paf";
    package ~min:"0.0.8" ~sublibs:["mirage"] "paf";
  ] in
  foreign
    ~keys:[Key.v remote_k ; Key.v branch; Key.v axfr]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> mimic @-> job)

let () =
  register "primary-git"
    [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $
     default_time $ net $ mimic_impl]
