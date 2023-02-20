This is the docker setup for a nginx proxy server used for Selenium tests.

The setups is as follows:

`Selenium -> Browser -> Nginx Proxy -> dfx (local replica)`

Nginx is used to terminate tls. This allows the use of the actual domain `https://identity.ic0.app` in the tests (dfx does not support `https`). Note that we cannot use `http://identity.ic0.app` because of preloaded HSTS.
We still need the `--ignore-certificate-errors` because we don't add the certificates to the browser truststore.

The certificates are self-signed and created using the following commands:
1. `openssl genrsa -aes256 -passout pass:foo -out server.key 4096`
2. Strip away password protection: `openssl rsa -in server.key -out server.key` (using password `foo`)
3. `openssl req -new -key server.key -out server.csr`
    * This will ask for information. Note that the `Common Name (CN)` must match the host name.
4. `openssl x509 -req -sha256 -days 365 -in server.csr -signkey server.key -out server.crt`
