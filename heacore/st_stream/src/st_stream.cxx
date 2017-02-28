/** \file st_stream.cxx
    \brief Implementation of globally accessible stream setup and info methods.
    \author James Peachey, HEASARC/GSSC
*/
#include <limits>
#include "st_stream/st_stream.h"

namespace {

  bool & GetNonConstDebugMode() {
    // Global debug mode flag.
    static bool s_debug_mode = false;
    return s_debug_mode;
  }

  std::string & GetNonConstExecName() {
    // Name of the current executable.
    static std::string s_exec_name;
    return s_exec_name;
  }

  unsigned int & GetNonConstMaxChat() {
    // Global chatter maximum.
    static unsigned int s_global_max_chat = std::numeric_limits<unsigned int>::max();
    return s_global_max_chat;
  }

}

namespace st_stream {

  void InitStdStreams(const std::string & exec_name, unsigned int max_chat, bool debug_mode) {
    // Perform initialization only once.
    static bool s_init_done = false;

    if (!s_init_done) {
      // Initialize sterr, stlog and stout.
      OStream::initStdStreams();

      // Set global parameters affecting stream output.
      GetNonConstDebugMode() = debug_mode;
      GetNonConstExecName() = exec_name;
      GetNonConstMaxChat() = max_chat;
      s_init_done = true;
    }
  }

  bool GetDebugMode() {
    return GetNonConstDebugMode();
  }

  const std::string & GetExecName() {
    return GetNonConstExecName();
  }

  unsigned int GetMaximumChatter() {
    return GetNonConstMaxChat();
  }

  void SetDebugMode(bool debug_mode) {
    GetNonConstDebugMode() = debug_mode;
  }

  void SetExecName(const std::string & exec_name) {
    GetNonConstExecName() = exec_name;
  }

  void SetMaximumChatter(unsigned int max_chat) {
    GetNonConstMaxChat() = max_chat;
  }

}
