/** \file st_stream.h
    \brief Declarations of globally accessible stream setup and info methods.
    \author James Peachey, HEASARC/GSSC
*/
#ifndef st_stream_st_stream_h
#define st_stream_st_stream_h

#include <string>

#include "st_stream/Stream.h"

namespace st_stream {

  /** \func InitStdStreams
      \brief Initialize standard streams sterr, stlog, and stout.
      \param exec_name The name of the current executable. Used by formatted streams to create prefix used for each line of output.
      \param max_chat The maximum chatter level. Messages with a chatter level higher than this will not be displayed.
      \param debug_mode Flag indicating whether debugging should be enabled.
  */
  void InitStdStreams(const std::string & exec_name, unsigned int max_chat, bool debug_mode);

  /// \func GetDebugMode
  /// \brief Return the setting of the global debug state flag.
  bool GetDebugMode();

  /// \func GetExecName
  /// \brief Return the name of the current executable.
  const std::string & GetExecName();

  /// \func GetMaximumChatter
  /// \brief Return the maximum chatter which should be displayed.
  unsigned int GetMaximumChatter();

  /// \func SetDebugMode
  /// \brief Set state of the global debug state flag.
  void SetDebugMode(bool debug_mode = true);

  /// \func SetExecName
  /// \brief Set the name of the current executable.
  void SetExecName(const std::string & exec_name);

  /// \func SetMaximumChatter
  /// \brief Set the maximum chatter which should be displayed.
  void SetMaximumChatter(unsigned int max_chat);

}

#endif
