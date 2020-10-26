#include "clang/Basic/JennyJIT.h"

#include <string>

namespace clang {

using namespace llvm;

std::string strErrorAndConsume(llvm::Error&& error) {
  std::string ss;
  llvm::raw_string_ostream os(ss);
  os << error;
  llvm::consumeError(std::move(error));
  return ss;
}


}
