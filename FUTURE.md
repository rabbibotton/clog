- Future tutorials for builder:

Deploying a CLOG website - part in WEBSITE.md
Creating a native application
Mobile development - u/eql5 has working
When to use page vs panel
Plug-in panels for use on other sites
Demo between different models - stateless, webpage, windows

- Expand clog-db-admin to use other available dbi database types

- Multi control select and alignments in Builder

- Right click menus CLOG-GUI

- Project scaffolding generator - done the template system in Builder
    1) Basic builder project - done
    2) CLOG-GUI builder project - done
    3) Builder website

- Improvement to CLOG-GUI menus to enable/disable or switch menus based on current window/panel

- Menu builder for Builder

- Release tool to handle creating platfom executables, templates for Apache and other webservers

- Training and documentation tool

- Optionaly, for local apps, allow direct transport of CLOG data to webpage via apis
remove need for websocket transport.

Some notes on this
   1) Sending data to the page - https://wiki.gnome.org/Projects/WebKitGtk/ProgrammingGuide/Cookbook - look there for executing JavaScript (CLOG is JS snipits) or better https://webkitgtk.org/reference/webkitgtk/stable/webkitgtk-webkitwebview.html#webkit-web-view-execute-script  also see https://github.com/webview/webview/issues/8
   
   2) Sending the boot.js file - Certainly a local file load will work file:// but nice if also have direct way to send html/js

   3) Way to receive data from the page - This is one way but not ideal - https://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebContext.html#webkit-web-context-register-uri-scheme and seems most all here https://blogs.igalia.com/carlosgc/2013/09/10/webkit2gtk-web-process-extensions/

- clog-monitor
  - logging, usage, etc.

