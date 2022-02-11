- Future tutorials for builder:

Deploying a CLOG website
Creating a native application
Mobile development
Creating a Bootstrap 5 based app
When to use page vs panel
Running JavaScript
Plug-in panels for use on other sites

- Multi control select and alignments in Builder

- Right click menus CLOG-GUI

- Handle windows beyond browser edge CLOG-GUI

- Project scaffolding generator
    1) Basic builder project
    2) CLOG-GUI builder project
    3) Builder website
    4) Database oriented projects

- Improvement to CLOG-GUI menus to enable/disable or switch menus based on current window/panel

- Menu builder for Builder

- Release tool to handle creating platfom executables, templates for Apache and other webservers

- Training and documentation tool

- Event queuing option to easily avoid event race conditions when desired in CLOG builder

- Implement using long polling to optimize CLOG sites for use with
  search engines. If not long polling at least initial output sent by
  initial html request until first query.

- Optimize communication with browser

- Optionaly, for local apps, allow direct transport of CLOG data to webpage via apis
remove need for websocket transport.

Some notes on this
   1) Sending data to the page - https://wiki.gnome.org/Projects/WebKitGtk/ProgrammingGuide/Cookbook - look there for executing JavaScript (CLOG is JS snipits) or better https://webkitgtk.org/reference/webkitgtk/stable/webkitgtk-webkitwebview.html#webkit-web-view-execute-script  also see https://github.com/webview/webview/issues/8
   
   2) Sending the boot.js file - Certainly a local file load will work file:// but nice if also have direct way to send html/js

   3) Way to receive data from the page - This is one way but not ideal - https://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebContext.html#webkit-web-context-register-uri-scheme and seems most all here https://blogs.igalia.com/carlosgc/2013/09/10/webkit2gtk-web-process-extensions/

- clog-data
  - binding html controls directly to fields for database use

- clog-auth
  - tiered authorization, user profiles, etc

- clog-monitor
  - logging, usage, etc.

