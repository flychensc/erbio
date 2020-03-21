erbio
=====

An erlang/OTP library

The `erbio` library encapsulates OpenSSL mem BIO APIs on Windows.

You can develop some encryption protocol (e.g. EAP-TLS, EAP-TTLS, EAP-PEAP) with this library.

Build
-----

    $ rebar3 compile

Usage
-----
1. Run `erbio:start(CertFile, KeyFile)`, e.g.: `erbio:start("cert/cacert.pem", "cert/cakey.pem")`. Certification is required for Server.

2. Create Client or Server. e.g.: `erbio:create(client)`, `erbio:create(server)`.

3. Do handshake until completed. API: `erbio:handshake(Id)` and `erbio:is_init_finished(Id)`. The encrypted data (Got from `erbio:bio_read(Id)`) should be sent to peer. The peer's encrypted data should be written by `erbio:bio_write(Id)`.

3. After handshake, application can send/receive plaintext. API: `erbio:ssl_write(Id, WtData)` and `erbio:ssl_read(Id)`. The encrypted data (Got from `erbio:bio_read(Id)`) should be sent to peer. The peer's encrypted data should be written by `erbio:bio_write(Id)`.

4. Free resource via `erbio:cleanup(Id)`.

Dependency
------
OpenSSL

Sample
------

Refer to [erbio_sample.erl](src/erbio_sample.erl)

OpenSSL
-----
Configure OpenSSL with command: `perl configure VC-WIN64A no-shared --prefix=D:\flych\wsl\source\openssl\`

Refrence
-----
OpenSSL mem sampe refer to [openssl_examples](https://github.com/darrenjs/openssl_examples)
