Tutorial 28 is a minimalist project for CLOG Builder

Move the hello-builder directory to ~/common-lisp or another directory
setup for QuickLisp to search. Then excute:

CL-USER> (ql:quickload :hello-builder)
To load "hello-builder":
  Load 1 ASDF system:
    hello-builder
; Loading "hello-builder"
[package hello-builder]
(:HELLO-BUILDER)
CL-USER> (hello-builder:start-app)

The hello.clog can be openned in CLOG Builder
(ql:quickload :clog/tools)(clog-tools:clog-builder)

hello.lisp is a saved render of hello.clog
