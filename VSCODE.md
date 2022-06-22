Using VSCODE instead of emacs:

- Install Common-Lisp (you can skip the emacs portion)

* [Install Common-Lisp for MacOS](MACOS.md)
* [Install Common-Lisp for Win64](WINDOWS.md)
* [Install Common-Lisp for Linux](https://lisp-lang.org/learn/getting-started/)

- Update asdf by git cloning the latest version in to ~/common-lisp or another asdf reachable location

git clone https://github.com/fare/asdf.git  

- Download and install vscode

- Go to - https://marketplace.visualstudio.com/items?itemName=rheller.alive

You need to insure you have pre-requisites installed so open sbcl or slime in Emacs and run;

(ql:quickload :cl-json)
(ql:quickload :bordeaux-threads)
(ql:quickload :usocket)
(ql:quickload :flexi-streams)

Click install and follow directions

- Cmd+Shift+P - type shell command <enter>

Go through install

- load up a .lisp file and the REPL should load

More info on using VSCode - https://lispcookbook.github.io/cl-cookbook/vscode-alive.html