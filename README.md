hampd - MPD web client with a server written in Haskell
=======================================================
Why?
---
The design is mostly a ripoff of ympd, for two reasons:

* I like ympd a lot!
* I have no good web design skills

The reason for hampd is:

* ympd lacks some features I would like to have
* I am learning Haskell and wanted to try it in a web setting with scotty

Unfortunately there is still no feasible solution to "The JavaScript Problem", which would
be easy and fun to use, while being efficient and working well as a hybrid (desktop/mobile) app,
so it is raw JS with Bootstrap and JQuery on the client side.

**What differs from ympd:**

* also works over HTTPS, protecting your MPD password
* You can toggle the available outputs
* Long-polling instead of websockets (at least now)

Installation
------------
1. Clone this repository
1. `cabal install` it (you need the Haskell Platform)
1. For HTTPS: Create a self-signed certificate as described above (or use another one)
1. Run `hampd` (`~/.cabal/bin` must be in your `$PATH`!)

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
* Support for queries
* Support to create and manage playlists
* Inclusion of the http stream in some way, if possible with FLAC
* Make usable on mobile devices
