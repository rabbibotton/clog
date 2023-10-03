rem Instructions modified from https://github.com/webview/webview

mkdir libs\webview2
curl -sSL "https://www.nuget.org/api/v2/package/Microsoft.Web.WebView2" | tar -xf - -C libs\webview2
mkdir build

rem You may have to modify this for your system, depending on your version of Visual Studio
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"

cl clogframe.cpp /std:c++17 /EHsc /Fobuild\ ^
    /I libs\webview2\build\native\include ^
    libs\webview2\build\native\x64\WebView2LoaderStatic.lib ^
    /link advapi32.lib /OUT:build\clogframe.exe
