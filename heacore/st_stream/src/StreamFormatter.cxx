/** \file StreamFormatter.cxx
    \brief Implementation of StreamFormatter class.
    \author James Peachey, HEASARC/GSSC
*/
#include <iostream>

#include "st_stream/StreamFormatter.h"
#include "st_stream/st_stream.h"

namespace st_stream {

  StreamFormatter::StreamFormatter(const std::string & class_name, const std::string & method_name,
    unsigned int default_chat_level): m_class_name(class_name), m_method_name(method_name), m_debug_stream(false),
    m_err_stream(false), m_info_stream(true), m_out_stream(false), m_warn_stream(true),
    m_default_chat_level(default_chat_level), m_debug_mode(false) {
    // Make any mandatory connections for all streams.
    m_debug_stream.connect(sterr);
    m_err_stream.connect(sterr);
    m_info_stream.connect(stout);
    m_out_stream.connect(stout);
    m_warn_stream.connect(stlog);

    // Set debugging mode based on the global debugging setting. This will also set up the prefixes for all stream output.
    setDebugMode(GetDebugMode());
  }

  StreamFormatter::~StreamFormatter() throw() {}

  void StreamFormatter::setMethod(const std::string & method_name) {
    m_method_name = method_name;
    setPrefix();
  }

  OStream & StreamFormatter::debug() {
    return m_debug_stream;
  }

  OStream & StreamFormatter::err() {
    // Error stream ignores chatter.
    return m_err_stream;
  }

  OStream & StreamFormatter::info() {
    return m_info_stream.setChatLevel(m_default_chat_level);
  }

  OStream & StreamFormatter::info(unsigned int chat_level) {
    return m_info_stream.setChatLevel(chat_level);
  }

  OStream & StreamFormatter::out() {
    // Output stream ignores chatter.
    return m_out_stream;
  }

  OStream & StreamFormatter::warn() {
    return m_warn_stream.setChatLevel(m_default_chat_level);
  }

  OStream & StreamFormatter::warn(unsigned int chat_level) {
    return m_warn_stream.setChatLevel(chat_level);
  }

  void StreamFormatter::setDebugMode(bool debug_mode) {
    // Reset flag indicating local debug mode.
    m_debug_mode = debug_mode;

    // Enable/disable debug stream.
    m_debug_stream.enable(debug_mode);

    // Reset prefixes, which may be different if debugging mode changed.
    setPrefix();
  }

  void StreamFormatter::setPrefix() {
    // Get the name of the executable.
    const std::string & exec_name = GetExecName();

    // Create appropriate prefix for each stream.
    m_debug_stream.setPrefix(createPrefix(exec_name, m_class_name, m_method_name, "DEBUG"));
    m_err_stream.setPrefix(createPrefix(exec_name, m_class_name, m_method_name, "ERROR"));
    m_info_stream.setPrefix(createPrefix(exec_name, m_class_name, m_method_name, "INFO"));
    m_out_stream.setPrefix(createPrefix(exec_name, "", "", ""));
    m_warn_stream.setPrefix(createPrefix(exec_name, m_class_name, m_method_name, "WARNING"));
  }

  std::string StreamFormatter::createPrefix(const std::string & exec_name, const std::string & class_name,
    const std::string & method_name, const std::string & message_type) {
    // Start with the name of the executable.
    std::string prefix = exec_name;
    if (!exec_name.empty()) prefix += ": ";

    // Add the message type.
    if (!message_type.empty()) {
      prefix += message_type;
      prefix += ": ";
    }

    // Only add class and method names if debug mode is enabled.
    if (m_debug_mode) {

      // Add the class name.
      if (!class_name.empty()) {
        prefix += class_name;

        // If the method name is defined, add two colons, otherwise colon space.
        if (!method_name.empty()) prefix += "::";
        else prefix += ": ";
      }

      // Add the method name.
      if (!method_name.empty()) {
        prefix += method_name;
        prefix += ": ";
      }

    }

    return prefix;
  }

}
