## CLOGFrame

CLOG Frame uses the native browser control on your platform to create
a simple app the displays a 640x480 frame ready to recieve your app.

clogframe[.exe] "Window Title" port

in the clogframe directory run:


On Mac (all requirements for build part of OS):

./make-mac
./make-snake

On Linux

Development: apt install libgtk-3-dev libwebkit2gtk-4.0-dev
Production: apt install libgtk-3-0 libwebkit2gtk-4.0-37

./make-linux
./make-snake

On Windows

to do (see https://github.com/webview/webview)

On All Platfroms:

This will create two executable clogframe and snake. clogframe
must be in the same directory and the sname executable. Then
run:

./snake
