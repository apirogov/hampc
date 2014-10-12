hampc - MPD web client
======================
Why?
---
The design is mostly a rip-off of [ympd](https://www.github.com/notandy/ympd), for two reasons:

* I like ympd a lot!
* I have no good web design skills. Seriously, I suck at it.

The reason for hampc is:

* ympd lacks some features I would like to have
* I am learning Haskell and wanted to try it in a web setting with scotty

Unfortunately there is still no feasible solution to 
"[The JavaScript Problem](http://www.haskell.org/haskellwiki/The_JavaScript_Problem)", which would
be easy and fun to use, while being efficient and working well as a hybrid (desktop/mobile) app,
so it is raw JS with Bootstrap and JQuery on the client side.

**What differs from ympd:**

* Move items in the Queue with drag&drop
* You can toggle the available outputs
* Also works over HTTPS, protecting your MPD password
* Built-in MPD stream player
* Long-polling instead of websockets (at least now)

Installation
------------
1. Clone this repository
1. `cabal install` it (you need the Haskell Platform)
1. For HTTPS: Create a self-signed certificate as described above (or use another one)
1. Run `hampc` (`~/.cabal/bin` must be in your `$PATH`!)

Create self-signed certificate (optional)
-----------------------------------------
```
openssl genrsa -des3 -out server.key 1024
openssl req -new -key server.key -out server.csr
cp server.key server.key.org
openssl rsa -in server.key.org -out server.key
openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt
```

Missing / TODO
--------------
* Support to create and manage playlists
* Support for queries
* Make usable on mobile devices
* fix stream glitching when toggling stream pause in firefox, if possible
