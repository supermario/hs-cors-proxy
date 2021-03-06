
### Haskell CORS Proxy

A Haskell reverse proxy which adds CORS headers to the proxied request.

Inspired by [cors-anywhere](https://github.com/Rob--W/cors-anywhere).


#### Usage:

Build and run:

```
$ stack install
$ hs-cors-proxy
Starting proxy on 8222...
```

To query, simply prefix the proxy URL to the HTTP call, i.e `http://example.com/` -> `localhost:8222/http://example.com/`:

```
$ curl localhost:8222/http://example.com/
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```
