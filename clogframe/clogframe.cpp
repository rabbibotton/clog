#include <iostream>
#include <sstream>
#include <string>
#include "webview.h"
int main(int argc,char* argv[]) {
  webview::webview w(true, nullptr);
  w.set_title(argv[1]);
  w.set_size(std::stoi(argv[3]), std::stoi(argv[4]), WEBVIEW_HINT_NONE);
  std::ostringstream o;
  o << "http://127.0.0.1:" << argv[2];
  w.navigate(o.str());
  w.run();
  return 0;
}
