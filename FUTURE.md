Post 1.1

Optionaly for local apps allow direct transport of CLOG data to webpage to
remove need for websocket transport.

Some notes
   1) Sending data to the page - https://wiki.gnome.org/Projects/WebKitGtk/ProgrammingGuide/Cookbook - look there for executing JavaScript (CLOG is JS snipits) or better https://webkitgtk.org/reference/webkitgtk/stable/webkitgtk-webkitwebview.html#webkit-web-view-execute-script  also see https://github.com/webview/webview/issues/8
   
   2) Sending the boot.js file - Certainly a local file load will work file:// but nice if also have direct way to send html/js

   3) Way to receive data from the page - This is one way but not ideal - https://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebContext.html#webkit-web-context-register-uri-scheme and seems most all here https://blogs.igalia.com/carlosgc/2013/09/10/webkit2gtk-web-process-extensions/

- clog-data
  - binding html controls directly to fields for database use

- clog-auth
  - tiered authorization, user profiles, etc

- clog-monitor
  - logging, usage, etc.

