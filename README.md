## Authoritative DNS server

This is a MirageOS unikernel which is an authoritative DNS server on port 53
(TCP and UDP). The data to be served is pulled from a git remote repository
(see the `ssh` branch for git-via-ssh). The server supports dynamic updates
(NSUPDATE), zone transfer (AXFR and IXFR), all cryptographically authenticated
and integrity protected with TSIG (HMAC with a pre-shared secret).

The git remote is expected to contain a flat file hierarchy where each zone
to be served is a separate file. HMAC secrets are stored as DNSKEY entries
in `_keys` zones (i.e. `example.com._keys` or `_keys`).

This can be used with [dns-secondary](https://github.com/roburio/dns-secondary),
and [let's encrypt](https://github.com/roburio/dns-letsencrypt-secondary) for
automated provisioning of let's encrypt certificates.

## Installation from source

To install this unikernel from source, you need to have
[opam](https://opam.ocaml.org) (>= 2.1.0) and
[ocaml](https://ocaml.org) (>= 4.08.0) installed. Also,
[mirage](https://mirageos.org) is required (>= 4.1.0). Please follow the
[installation instructions](https://mirageos.org/wiki/install).

The following steps will clone this git repository and compile the unikernel:

```bash
$ git clone https://github.com/roburio/dns-primary-git.git
$ mirage configure -t <your-favourite-target>
$ make depend
$ mirage build
```

## Installing as binary

Binaries are available at [Reproducible OPAM
builds](https://builds.robur.coop/job/dns-primary-git/), see [Deploying binary MirageOS
unikernels](https://hannes.robur.coop/Posts/Deploy) and [Reproducible MirageOS
unikernel builds](https://hannes.robur.coop/Posts/ReproducibleOPAM) for details.

## Questions?

Please open an issue if you have questions, feature requests, or comments.
