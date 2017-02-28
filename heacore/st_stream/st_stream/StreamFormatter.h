/** \file StreamFormatter.h
    \brief Declaration of StreamFormatter class.
    \author James Peachey, HEASARC/GSSC
*/
#ifndef st_stream_StreamFormatter_h
#define st_stream_StreamFormatter_h

#include <string>

#include "st_stream/Stream.h"

namespace st_stream {
  /** \class StreamFormatter
      \brief Class to assist clients with providing consistently styled output.
  */
  class StreamFormatter {
    public:
      /** \brief Create a stream formatting helper object, with streams for output, info, warnings, errors, etc..

                 This class uses chatter and prefixes appropriately to create uniformly styled output.
          \param class_name The name of the class, used to create a prefix for each line of output.
                 May be blank, in which case it is simply not used.
          \param method_name The name of the method, used to create a prefix for each line of output.
                 May be blank, in which case it is simply not used. The method name may also be reset after construction
                 using the setMethod method.
          \param default_chat_level The chatter level (priority) used for messages if the chatter level is not
                 set explicitly.
                 The chat level may be set explicitly using OStream::setChatLevel or the Chat helper class.
                 Messages with chatter level 0 are always displayed. Otherwise, only messages with
                 chatter levels less than or equal to the maximum chatter currently selected will be displayed.
      */
      StreamFormatter(const std::string & class_name, const std::string & method_name, unsigned int default_chat_level);

      virtual ~StreamFormatter() throw();

      /** \brief Return the name of the method.
      */
      const std::string & getMethod() const;

      /** \brief Set the name of the method. This is used in combination with the class name to set the
                 prefix.
          \param method_name The new value for the method name.
      */
      void setMethod(const std::string & method_name);

      /** \brief Return a stream which is set up for debugging messages which are not suppressible by chatter
                 level, but which appear only if debugging is enabled.

                 If debugging is currently enabled, either through the global debug setting or locally for
                 this object (see setDebugMode method), output to this stream will be sent to sterr, regardless
                 of any chatter levels. Otherwise output to this stream is ignored. If it appears, the output will
                 be preceded by the debug prefix.
      */
      OStream & debug();

      /** \brief Return a stream which is set up for unsuppressible error messages.

                 Output to this stream will be sent to sterr, regardless of any chatter levels. The output will
                 be preceded by the error prefix.
      */
      OStream & err();

      /** \brief Return a stream which is set up for suppressible discretionary output, that is output which may or may
                 not interest a user.

                 Output to this stream will be sent to stlog. If the default chatter level is greater than
                 the maximum chatter level, the warning will not be displayed. If it appears, the output will
                 be preceded by the info prefix.
      */
      OStream & info();

      /** \brief Return a stream which is set up for suppressible discretionary output, that is output which may or may
                 not interest a user, after setting the stream's chat level.

                 Output to this stream will be sent to stlog. If the default chatter level is greater than
                 the maximum chatter level, the warning will not be displayed. If it appears, the output will
                 be preceded by the info prefix.
          \param chat_level The chat level. The default chat level will be unaffected.
      */
      OStream & info(unsigned int chat_level);

      /** \brief Return a stream which is set up for unsuppressible output messages.

                 Output to this stream will be sent to stout, regardless of any chatter levels. The output
                 will be preceded by the output prefix.
      */
      OStream & out();

      /** \brief Return a stream which is set up for suppressible warnings which may or may not interest the user.

                 Output to this stream will be sent to stlog. If the default chatter level is greater than
                 the maximum chatter level, the warning will not be displayed.
                 If it appears, the output will be preceded by the warning prefix.
      */
      OStream & warn();

      /** \brief Return a stream which is set up for suppressible warnings which may or may not interest the user,
                 after setting the stream's chat level.

                 Output to this stream will be sent to stlog. If the default chatter level is greater than
                 the maximum chatter level, the warning will not be displayed.
                 If it appears, the output will be preceded by the warning prefix.
          \param chat_level The new chat level. The default chat level will be unaffected.
      */
      OStream & warn(unsigned int chat_level);

      /** \brief Explicitly turn debugging on or off. Warning: this is for temporary use by developers while
                 actively debugging, and should not be checked in or used in production code.
          \param debug_mode The new setting for the debug mode.
      */
      void setDebugMode(bool debug_mode = true);

    protected:
      /** \brief Set the prefixes used by all streams in this formatter.
      */
      virtual void setPrefix();

      /** \brief Construct a prefix from the given tokens.

                 The prefix is constructed from 4 tokens: the executable name, the class name (see constructor),
                 the method name (see setMethod) and the a string giving the type of message. Any of these
                 which are blank will simply be omitted. Any which are present will be concatenated with
                 a colon and a space between adjacent tokens (or two colons between the class and method names,
                 if both are defined.) The format of the prefix if all four tokens are present is
                 exec_name: message_type: class_name::method_name.
          \param exec_name The name of the executable.
          \param class_name The name of the class.
          \param method_name The name of the method.
          \param message_type The type of the message, e.g. WARNING, ERROR, etc.
      */
      virtual std::string createPrefix(const std::string & exec_name, const std::string & class_name,
        const std::string & method_name, const std::string & message_type);

    private:
      std::string m_class_name;
      std::string m_method_name;
      OStream m_debug_stream;
      OStream m_err_stream;
      OStream m_info_stream;
      OStream m_out_stream;
      OStream m_warn_stream;
      unsigned int m_default_chat_level;
      bool m_debug_mode;
  };

}

#endif
