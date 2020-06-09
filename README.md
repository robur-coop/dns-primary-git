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
[opam](https://opam.ocaml.org) (>= 2.0.0) and
[ocaml](https://ocaml.org) (>= 4.07.0) installed. Also,
[mirage](https://mirageos.org) is required (>= 3.7.7). Please follow the
[installation instructions](https://mirageos.org/wiki/install).

The following steps will clone this git repository and compile the unikernel:

```bash
$ git clone https://github.com/roburio/dns-primary-git.git
$ opam repo add git-ssh git+https://github.com/roburio/git-ssh-dns-mirage3-repo.git
$ cd dns-primary-git
$ mirage configure -t <your-favourite-target>
$ make depend
$ make
```

## Installing as binary

There are not yet any binaries available, but work is underway to provide
reproducible binaries.

## Questions?

Please open an issue if you have questions, feature requests, or comments.
