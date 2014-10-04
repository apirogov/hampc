hampd
=====

Create self-signed certificate
------------------------------
```
openssl genrsa -des3 -out server.key 1024
openssl req -new -key server.key -out server.csr
cp server.key server.key.org
openssl rsa -in server.key.org -out server.key
openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt
```

Installation
------------
1. Clone this repository
1. Create a self-signed certificate (or use another one)
1. `cabal install` it and you are ready to go!
