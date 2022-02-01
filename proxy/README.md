# Proxy

Copied from nns-dapp/proxy

A proxy that gives each local canister its own port and funnels the requests to
the actual running replica.

An example is worth a thousand words:

```bash
$ npm run start -- --replica-host 'http://localhost:8080' rwlgt-iiaaa-aaaaa-aaa:8086 rrkah-fqaaa-aaaaa-aaaaq-cai:8087 doesnt-exist-aaaa-aaaaa-cai:8088

Forwarding 8086 to http://localhost:8080/?canisterId=rwlgt-iiaaa-aaaaa-aaaaa-cai
[HPM] Proxy created: /  -> http://localhost:8080
Forwarding 8087 to http://localhost:8080/?canisterId=rrkah-fqaaa-aaaaa-aaaaq-cai
[HPM] Proxy created: /  -> http://localhost:8080
Forwarding 8088 to http://localhost:8080/?canisterId=doesnt-exist-aaaa-aaaaa-cai
[HPM] Proxy created: /  -> http://localhost:8080
Canister rwlgt-iiaaa-aaaaa-aaaaa-cai is listening on http://localhost:8086
Canister rrkah-fqaaa-aaaaa-aaaaq-cai is listening on http://localhost:8087
Canister doesnt-exist-aaaa-aaaaa-cai is listening on http://localhost:8088
```

Then, in another terminal:

```bash
$ curl http://localhost:8086
<!DOCTYPE html>
...
$ curl http://localhost:8087/?foo=bar
<!DOCTYPE html>
...
$ curl http://localhost:8088
Could not find a canister id to forward to.
```

The proxy will have logged the following requests:

```bash
rwlgt-iiaaa-aaaaa-aaaaa-cai (8086) 200 GET / -> /?canisterId=rwlgt-iiaaa-aaaaa-aaaaa-cai
rrkah-fqaaa-aaaaa-aaaaq-cai (8087) 200 GET /?foo=bar -> /?foo=bar&canisterId=rrkah-fqaaa-aaaaa-aaaaq-cai
doesnt-exist-aaaa-aaaaa-cai (8088) 400 GET / -> /?canisterId=doesnt-exist-aaaa-aaaaa-cai
```

## Usage

```bash
$ npm run start -- --help

USAGE: proxy --replica-host http://... [<canister-id>:<port>]
```

## Build

```bash
$ npm run build
```

or

```bash
$ npm run start # builds _and_ starts
```
