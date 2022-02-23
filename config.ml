(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

(* boilerplate from https://github.com/mirage/ocaml-git.git
   unikernel/empty-commit/config.ml
   commit #45d90b8792ab8f3866751f462619c7dd7860e5d5 *)
type mimic = Mimic

let mimic = typ Mimic

let mimic_count =
  let v = ref (-1) in
  fun () -> incr v ; !v

let mimic_conf () =
  let packages = [ package "mimic" ] in
  impl @@ object
       inherit base_configurable
       method ty = mimic @-> mimic @-> mimic
       method module_name = "Mimic.Merge"
       method! packages = Key.pure packages
       method name = Fmt.str "merge_ctx%02d" (mimic_count ())
       method! connect _ _modname =
         function
         | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
         | [ x ] -> Fmt.str "%s.ctx" x
         | _ -> Fmt.str "Lwt.return Mimic.empty"
     end

let merge ctx0 ctx1 = mimic_conf () $ ctx0 $ ctx1

(* TODO(dinosaure): [timeout] and [timer interval]. *)
let mimic_happy_eyeballs =
  let packages = [ package "git-mirage" ~sublibs:[ "happy-eyeballs" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> mimic
       method module_name = "Git_mirage_happy_eyeballs.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_happy_eyeballs"
       method! connect _ modname = function
         | [ _random; _time; _mclock; _pclock; stackv4v6; ] ->
           Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
         | _ -> assert false
     end

let mimic_tcp =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = tcpv4v6 @-> mimic @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_tcp"
       method! connect _ modname = function
         | [ _tcpv4v6; ctx ] ->
           Fmt.str {ocaml|%s.connect %s|ocaml}
             modname ctx
         | _ -> assert false
     end

let mimic_ssh ?authenticator key =
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = mclock @-> tcpv4v6 @-> mimic @-> mimic
       method! keys = match authenticator with
         | Some authenticator -> [ Key.abstract key; Key.abstract authenticator ]
         | None -> [ Key.abstract key ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_ssh"
       method! connect _ modname = function
         | [ _mclock; _tcpv4v6; ctx ] ->
           ( match authenticator with
           | None ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optionnal_key ~key:%a|ocaml}
               modname ctx modname Key.serialize_call (Key.abstract key)
           | Some authenticator ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optionnal_key ?authenticator:%a ~key:%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract authenticator)
               Key.serialize_call (Key.abstract key) )
         | _ -> assert false
     end

let mimic_http ?tls_key_fingerprint ?tls_cert_fingerprint headers =
  let packages = [ package "git-mirage" ~sublibs:[ "http" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = time @-> pclock @-> tcpv4v6 @-> mimic @-> mimic
       method! keys = match tls_key_fingerprint, tls_cert_fingerprint with
         | Some tls_key_fingerprint, None ->
           let keys = match headers with Some headers -> [ Key.abstract headers ] | None -> [] in
           [ Key.abstract tls_key_fingerprint ] @ keys
         | None, Some tls_cert_fingerprint ->
           let keys = match headers with Some headers -> [ Key.abstract headers ] | None -> [] in
           [ Key.abstract tls_cert_fingerprint ] @ keys
         | Some tls_key_fingerprint, Some tls_cert_fingerprint ->
           let keys = match headers with Some headers -> [ Key.abstract headers ] | None -> [] in
           [ Key.abstract tls_key_fingerprint; Key.abstract tls_cert_fingerprint ] @ keys
         | None, None -> ( match headers with Some headers -> [ Key.abstract headers ] | None -> [] )
       method module_name = "Git_mirage_http.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_http"
       method! connect _ modname = function
         | [ _time; _pclock; _tcpv4v6; ctx; ] ->
           let serialize_headers ppf = function
             | None -> ()
             | Some headers -> Fmt.pf ppf "?headers:%a" Key.serialize_call (Key.abstract headers) in
           ( match tls_key_fingerprint, tls_cert_fingerprint with
           | Some tls_key_fingerprint, None ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers ?tls_key_fingerprint:%a%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract tls_key_fingerprint)
               Fmt.((const string " ") ++ serialize_headers) headers
           | None, Some tls_cert_fingerprint ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers ?tls_cert_fingerprint:%a%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract tls_cert_fingerprint)
               Fmt.((const string " ") ++ serialize_headers) headers
           | None, None ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers%a|ocaml}
               modname ctx modname
               Fmt.((const string " ") ++ serialize_headers) headers
           | Some tls_key_fingerprint, Some tls_cert_fingerprint ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers
                              ?tls_key_fingerprint:%a ?tls_cert_fingerprint:%a%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract tls_key_fingerprint)
               Key.serialize_call (Key.abstract tls_cert_fingerprint)
               Fmt.((const string " ") ++ serialize_headers) headers )
         | _ -> assert false
     end

let tcpv4v6_of_stackv4v6 =
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> tcpv4v6
       method module_name = "Git_mirage_happy_eyeballs.TCPV4V6"
       method! packages = Key.pure [ package "git-mirage" ~sublibs:[ "happy-eyeballs" ] ]
       method name = "tcpv4v6"
       method! connect _ modname = function
         | [ stackv4v6 ] -> Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
         | _ -> assert false
     end
(* --- end of copied code --- *)

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote git repository." ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/roburio/udns.git" doc))

let axfr =
  let doc = Key.Arg.info ~doc:"Allow unauthenticated zone transfer." ["axfr"] in
  Key.(create "axfr" Arg.(flag doc))

let ssh_key =
  let doc = Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

let authenticator =
  let doc = Key.Arg.info ~doc:"Authenticator." ["authenticator"] in
  Key.(create "authenticator" Arg.(opt (some string) None doc))

let tls_key_fingerprint =
  let doc = Key.Arg.info ~doc:"The fingerprint of the TLS key." [ "tls-key-fingerprint" ] in
  Key.(create "tls_key_fingerprint" Arg.(opt (some string) None doc))

let tls_cert_fingerprint =
  let doc = Key.Arg.info ~doc:"The fingerprint of the TLS certificate." [ "tls-cert-fingerprint" ] in
  Key.(create "tls_cert_fingerprint" Arg.(opt (some string) None doc))

let mimic_impl random stackv4v6 mclock pclock time =
  let tcpv4v6 = tcpv4v6_of_stackv4v6 $ stackv4v6 in
  let mhappy_eyeballs = mimic_happy_eyeballs $ random $ time $ mclock $ pclock $ stackv4v6 in
  let mtcp  = mimic_tcp
    $ tcpv4v6 $ mhappy_eyeballs in
  let mssh  = mimic_ssh ~authenticator ssh_key
    $ mclock $ tcpv4v6 $ mhappy_eyeballs in
  let mhttp = mimic_http ~tls_key_fingerprint ~tls_cert_fingerprint None
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
    package ~min:"3.7.0" ~max:"3.8.0" "git-mirage";
    package ~min:"3.7.0" "git-paf";
    package ~min:"0.0.8" ~sublibs:["mirage"] "paf";
  ] in
  foreign
    ~keys:[Key.abstract remote_k ; Key.abstract axfr]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> mimic @-> job)

let () =
  register "primary-git"
    [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $
     default_time $ net $ mimic_impl]
