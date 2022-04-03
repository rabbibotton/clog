CLOG is configured to handle both standard http and https connections.

The following is the configuration I used for Apache on the clogpower.com
site using the rewrite engine to expose my site externally as clogpower.com
but interanlly as localhost on port 8080 (or other port) and is typically 
how I configure my CLOG based servers:
(Note use of *.443 also and use of SSLEngine, if not using SSL remove both)

<VirtualHost *:80 *:443>
    ServerName clogpower.com
    ServerAlias www.clogpower.com
    ServerAdmin david@botton.com

    SSLEngine on
    SSLCertificateFile /etc/ssl/certificate.crt
    SSLCertificateKeyFile /etc/ssl/private/private.key
    SSLCertificateChainFile /etc/ssl/ca_bundle.crt

    ProxyPass / http://127.0.0.1:8080/
    RewriteEngine on
    RewriteCond %{HTTP:Upgrade} websocket [NC]
    RewriteCond %{HTTP:Connection} upgrade [NC]
    RewriteRule ^/?(.*) "ws://127.0.0.1:8080/$1" [P,L]

    ErrorLog ${APACHE_LOG_DIR}/clog.err.log
    CustomLog ${APACHE_LOG_DIR}/clog.log common
</VirtualHost>
