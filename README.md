# Authoritative DNS server

This is a MirageOS unikernel which is an authoritative DNS server on port 53
(TCP and UDP). The data to be served is pulled from a git remote repository. The
server supports dynamic updates (NSUPDATE), zone transfer (AXFR and IXFR), all
cryptographically authenticated and integrity protected with TSIG (HMAC with a
pre-shared secret).

The git remote is expected to contain a flat file hierarchy where each zone
to be served is a separate file. HMAC secrets are stored as DNSKEY entries
in `_keys` zones (i.e. `example.com._keys` or `_keys`).

This can be used with [dns-secondary](https://github.com/robur-coop/dns-secondary),
and [let's encrypt](https://github.com/robur-coop/dns-letsencrypt-secondary) for
automated provisioning of let's encrypt certificates.

## Interoperability

Considering you have a `_keys` file with an example HMAC-SHA256 key:
```
client._update. DNSKEY  0       3       163     0701XCD0muDYZIiLwv6wN/Tyoor/hd9+1zjmZ1mIlzY=
```

Also, take a "mirage" zone as given, and the unikernel running on "10.0.42.2" in the following.

Interoperation with utilities from bind is given:

If a NOTIFY (RFC 1996) is received, which is signed with a known hmac
secret, a git pull is done. This means an update by the operator to the zones in
git can be done, but remember to send a NOTIFY afterwards. Hint:
`dig SOA mirage @10.0.42.2 +opcode=notify -y hmac-sha256:client._update:0701XCD0muDYZIiLwv6wN/Tyoor/hd9+1zjmZ1mIlzY=`

A NSUPDATE (RFC 2136) can trigger zone updates. Launch it with
`nsupdate -y hmac-sha256:client._update:0701XCD0muDYZIiLwv6wN/Tyoor/hd9+1zjmZ1mIlzY=`
and you'll enter an interactive session:
```
> server 10.0.42.2
> zone mirage
> add local.mirage 3600 IN A 127.0.0.1
> send
```

This will trigger:
- (a) an update of the zone mirage,
- (b) increment the serial in the SOA, and
- (c) a commit and push to the git repository.

You can observe by requesting `dig soa mirage @10.0.42.2` before and after
the `nsupdate` execution.

## Installation from source

To install this unikernel from source, you need to have
[opam](https://opam.ocaml.org) (>= 2.1.0) and
[ocaml](https://ocaml.org) (>= 4.08.0) installed. Also,
[mirage](https://mirageos.org) is required (>= 4.5.0). Please follow the
[installation instructions](https://mirageos.org/wiki/install).

The following steps will clone this git repository and compile the unikernel:

```bash
$ git clone https://github.com/robur-coop/dns-primary-git.git
$ mirage configure -t <your-favourite-target>
$ make depend
$ make build
```

## Installing as binary

Binaries are available at [Reproducible OPAM
builds](https://builds.robur.coop/job/dns-primary-git/), see [Deploying binary MirageOS
unikernels](https://hannes.robur.coop/Posts/Deploy) and [Reproducible MirageOS
unikernel builds](https://hannes.robur.coop/Posts/ReproducibleOPAM) for details.

## Questions?

Please open an issue if you have questions, feature requests, or comments.
