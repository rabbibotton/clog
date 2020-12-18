(This needs updating)

Q) Is it possible to use a reverse proxies / load balancers with CLOG Apps?

A) CLOG uses websockets which uses an http "upgrade" mechanism which means
the proxy or load balancer must have some knowledge of how to handle websockets. The two most popular Apache and Nginx support it:

For nginx see:
   http://nginx.org/en/docs/http/websocket.html
   http://nginx.com/blog/websocket-nginx/

For Apache 2.4.5+ see:
   http://httpd.apache.org/docs/2.4/mod/mod_proxy_wstunnel.html

Example of CLOG application as its own domain:

<VirtualHost *:80>
    ServerName snake.clog.com
    ServerAdmin david@botton.com

    ProxyPass /clog ws://127.0.0.1:8080/clog
    ProxyPass / http://127.0.0.1:8080/
    ProxyPassReverse / http://127.0.0.1:8080/

    ErrorLog ${APACHE_LOG_DIR}/clog.err.log
    CustomLog ${APACHE_LOG_DIR}/clog.log common
</VirtualHost>

Example of a CLOG application as a subdirectory on a larger site using Apache:

<VirtualHost *:80>
    ServerName clog.com
    ServerAlias www.clog.com
    ServerAdmin david@botton.com
    DocumentRoot /www/clog

    ProxyPass /snake http://127.0.0.1:8080
    ProxyPassReverse /snake http://www.clog.com:8080
    ProxyPass /clog ws://www.clog.com:8080/clog
    ProxyPass /js http://www.clog.com:8080/js

    ErrorLog ${APACHE_LOG_DIR}/clog.err.log
    CustomLog ${APACHE_LOG_DIR}/clog.log common
</VirtualHost>
