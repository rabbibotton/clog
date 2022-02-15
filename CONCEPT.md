# CLOG - Technical Overview and Purpose

## CLOG's Conceptual Purpose

Provide a framework, CLOG, and visual tools, CLOG Builder, for
graphical user interfaces using web technologes and so allow
leveraging of new UI technologies as they surface in a completely
cross platform manor for applications ranging from small single user
utilities to massive concurrent multiuser applications of which
websites or webapps are just a subcategory.

## Underlying Technologies

** Transport **

CLOG's current transport is a client/server arrangement, http is used
for initial bootstrap and WebSockets for continued communication.
Other transports are planned.

The initial bootstrap of a CLOG application is to establish the
WebSocket connection via an html boot page and a javascript
script. Once WebSocket communication is established all messages
between server and client are in JavaScript (JS over WS). Changes to
the user interface, establishing, and communicating events take place
as fragments of the DOM and JavaSript calls and is transparent to the
user in normal use.

** Establishing initial state of UI **

There are three ways to establish the initial state of a user interface.

1. Using an initial boot file served via http to establish the intial
UI. This is near identical to traditional webservers/website solutions
and every familiar technology offered for website development may be
included in the booy file. Any html file with the boot.js script
included or embedded may serve as a boot file to allow server side
control with in the CLOG framework. The CLOG server and the server
webpages must exist on the same domain do to security restrictions in
WebSockets.

2. To build the initial UI from scratch using CLOG's APIs

3. A hybrid approach as used by CLOG Builder panels. The created
panels are composite components that deliver their base UI in a bulk
write of HTML/JS to the browser.

** Performance considerations **

Method 1 and 3 offer performance advantages to Ajax and similar
performance to long polling. Method 2 is clearly slower but has
advantages for a completely non-html non-js code based.

** Post Initialization ***

CLOG offers an event based framework. As of CLOG 1.2, an additional
"presentations" like system is included for bidirectionally binding
Lisp Objects and CLOG Objects and contiued development in CLOG is to
pursue natural use with no or minimal use of CLOG Apis for data models
and business logic.

CLOG apps can easily be used completely stateless, making use of
post/get in the same way as any web framework and operate as a highly
responsive push UI (something you can not do without JS over WS) with
or with out state maintaining, or as responsive highly state oriented
traditional applications with the goal on focus less on the UI
interaction and more on the purpose of your application.

Even if ignoring the advantages of using CLOG's higher level
framework, CLOG offers a simplified means to affectively use JS over
WS a faster and more flexible technique for client server
communictions, manipulation of elements and responding to events
within webpages.

** Reliability **

CLOG is more reliable than traditional client/server configurations,
rpc based apps, and orbs specifically because it is designed on top of
web technology meant to deal with less than ideal
environments. However the robustness of any application is based on
its design not just the transports being used.

Currently CLOG will reestablish moderate interrupts to
connectivity. On the same machine survive sleep and suspending
etc. Full disconnects need to be planned for in the design of your
system. A current snapshot of the UI state can be taken easily but the
server side state is design dependent, in the end every app must plan
for failures.

** Scalability **

Scale widely ranges based on app design, CLOG can be used to just
serve static pages, for server side constructed pages, server
controlled construction on the client side pages, or a hybrid of all
the above. In all cases the scalability is generally identical to a
similar application on other web based systems.

However, things that make CLOG unique from websites frameworks are in
its ability ot develop systems where concurrent users are interacting,
large scale business applications, or any application needing real
time information delivery, and scaling will then more closely follow
traditional client server applications.

** Conclusion **

I really appreciate your taking the time to dig in to CLOG. I hope I
provided clarifications that best represent what CLOG can provide now
and were it is deficient to your needs please contact me to see if it
makes sense to meet your needs and so for CLOG to reach a broader set
of use cases with it is conceptual purpose, UIs for list applications
from single user utilities to massive concurrent multiuser systems.

