CLOG is configured to handle both standard http and https connections.

The following is the configuration I used for Apache on the clogpower.com
site using the rewrite engine to expose my site externally as clogpower.com
but interanlly as localhost on port 8081 (or other port) and is typically 
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

    ProxyPass / http://127.0.0.1:8081/
    RewriteEngine on
    RewriteCond %{HTTP:Upgrade} websocket [NC]
    RewriteCond %{HTTP:Connection} upgrade [NC]
    RewriteRule ^/?(.*) "ws://127.0.0.1:8081/$1" [P,L]

    ErrorLog ${APACHE_LOG_DIR}/clog.err.log
    CustomLog ${APACHE_LOG_DIR}/clog.log common
</VirtualHost>

I make sure that my start funtion ends with a busy wait like - (loop (sleep 360))
or the process will return and the service end.

Then I setup sbcl with my app to run as a service:

I then create the file /etc/systemd/system/clogpower.service (using sudo):

[Unit]
Description=clogpower site

[Service]
User=dbotton
Group=dbotton
WorkingDirectory=/home/dbotton/common-lisp/clogpower
ExecStart=sbcl --sysinit /home/dbotton/.sbclrc --eval "(ql:quickload :clogpower)" --eval "(clogpower:start-site :port 8081)"
Type=simple
Restart=on-failure

[Install]
WantedBy=network.target


Then I install the service so it comes up after boot:

sudo systemctl enable clogpower.service

If need to start it already:

sudo systemctl start clogpower.service

You can check status with

sudo systemctl starus clogpower.service

You can look at logs with:

journalctl -u clogpower.service
