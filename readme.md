
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

Query:

```
$ curl localhost:8222/http://example.com/
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```
