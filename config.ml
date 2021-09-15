(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

(* boilerplate from https://github.com/mirage/ocaml-git.git unikernel/config.ml
   (commit #3bfcf215f959b71580e5c0b655700bb9484aee8c) *)
let pin_git_mirage = "git+https://github.com/mirage/ocaml-git.git#42cd15baa8cb6e82f003f62126cf18f42cce8c63"
let pin_repr = "git+https://github.com/mirage/repr#0c0b7b76bd6531ce3d3adc341bf3df72046f5855"
let pin_irmin = "git+https://github.com/mirage/irmin.git#ae15cc291ce4d6e77c130e1db41e3f92dae00e69"
let pin_dns = "git+https://github.com/mirage/ocaml-dns.git#eb8bac066cdc97e1a12bb1ccda854dd539092cf1"

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

let mimic_tcp_conf =
  let packages = [ package "git-mirage" ~pin:pin_git_mirage ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "tcp_ctx"
       method! connect _ modname = function
         | [ stack ] ->
           Fmt.str {ocaml|Lwt.return (%s.with_stack %s %s.ctx)|ocaml}
             modname stack modname
         | _ -> assert false
     end

let mimic_tcp_impl stackv4v6 = mimic_tcp_conf $ stackv4v6

let mimic_ssh_conf ~kind ~seed ~auth =
  let seed = Key.abstract seed in
  let auth = Key.abstract auth in
  let packages = [ package "git-mirage" ~pin:pin_git_mirage ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> mimic @-> mclock @-> mimic
       method! keys = [ seed; auth; ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = match kind with
         | `Rsa -> "ssh_rsa_ctx"
         | `Ed25519 -> "ssh_ed25519_ctx"
       method! connect _ modname =
         function
         | [ _; tcp_ctx; _ ] ->
             let with_key =
               match kind with
               | `Rsa -> "with_rsa_key"
               | `Ed25519 -> "with_ed25519_key"
             in
             Fmt.str
               {ocaml|let ssh_ctx00 = Mimic.merge %s %s.ctx in
                      let ssh_ctx01 = Option.fold ~none:ssh_ctx00 ~some:(fun v -> %s.%s v ssh_ctx00) %a in
                      let ssh_ctx02 = Option.fold ~none:ssh_ctx01 ~some:(fun v -> %s.with_authenticator v ssh_ctx01) %a in
                      Lwt.return ssh_ctx02|ocaml}
               tcp_ctx modname
               modname with_key Key.serialize_call seed
               modname Key.serialize_call auth
         | _ -> assert false
     end

let mimic_ssh_impl ~kind ~seed ~auth stackv4v6 mimic_git mclock =
  mimic_ssh_conf ~kind ~seed ~auth
  $ stackv4v6
  $ mimic_git
  $ mclock

(* TODO(dinosaure): user-defined nameserver and port. *)

let mimic_dns_conf =
  let packages = [ package "git-mirage" ~pin:pin_git_mirage ~sublibs:[ "dns" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = random @-> mclock @-> time @-> stackv4v6 @-> mimic @-> mimic
       method module_name = "Git_mirage_dns.Make"
       method! packages = Key.pure packages
       method name = "dns_ctx"
       method! connect _ modname =
         function
         | [ _; _; _; stack; tcp_ctx ] ->
             Fmt.str
               {ocaml|let dns_ctx00 = Mimic.merge %s %s.ctx in
                      let dns_ctx01 = %s.with_dns %s dns_ctx00 in
                      Lwt.return dns_ctx01|ocaml}
               tcp_ctx modname
               modname stack
         | _ -> assert false
     end

let mimic_dns_impl random mclock time stackv4v6 mimic_tcp =
  mimic_dns_conf $ random $ mclock $ time $ stackv4v6 $ mimic_tcp

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

let mimic_impl ~kind ~seed ~authenticator stackv4v6 random mclock pclock time =
  let mtcp = mimic_tcp_impl stackv4v6 in
  let mdns = mimic_dns_impl random mclock time stackv4v6 mtcp in
  let mssh = mimic_ssh_impl ~kind ~seed ~auth:authenticator stackv4v6 mtcp mclock in
  merge mssh mdns

let net = generic_stackv4v6 default_network

let mimic_impl =
  mimic_impl ~kind:`Rsa ~seed ~authenticator net
    default_random default_monotonic_clock default_posix_clock default_time

let dns_handler =
  let packages = [
    package "logs" ;
    package ~pin:pin_dns ~sublibs:["mirage"; "zone"] "dns-server";
    package ~pin:pin_dns "dns";
    package ~pin:pin_dns "dns-client";
    package ~pin:pin_dns "dns-mirage";
    package ~pin:pin_dns "dns-tsig";
    package ~pin:pin_repr "repr";
    package ~pin:pin_repr "ppx_repr";
    package ~pin:pin_irmin "ppx_irmin";
    package ~pin:pin_irmin "irmin";
    package ~pin:pin_irmin "irmin-git";
    package ~pin:pin_git_mirage "git-mirage";
    package ~pin:pin_git_mirage "git";
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
