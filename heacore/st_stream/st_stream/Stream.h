/** \file Stream.h
    \brief Declaration of OStream class.
    \author James Peachey, HEASARC/GSSC
*/
#ifndef st_stream_Stream_h
#define st_stream_Stream_h

#include <iostream>
#include <set>
#include <string>

namespace st_stream {

  /** \class OStream
      \brief Output stream class which connects its output to one or more std::ostreams, and/or to
             one or more other OStreams.
  */
  class OStream {
    public:
      /** \brief Type of container of std::ostreams. */
      typedef std::set<std::ostream *> StdStreamCont_t;

      /** \brief Type of container of OStreams. */
      typedef std::set<OStream *> OStreamCont_t;

      /** \brief Perform initializations of globally accessible streams sterr, stlog and stout.
      */
      static void initStdStreams();

      /** \brief Create an OStream with the given client maximum chatter.
          \param use_chatter Determines whether or not chatter is respected by the stream.
      */
      OStream(bool use_chatter);

      /** \brief Write this stream's prefix, (respecting chatter, if enabled) and return the stream.
      */
      OStream & prefix();

      /** \brief Shift the given object to the destination stream(s), but only if the current
                 message chatter level is less than or equal to the maximum chatter level.
          \param t The object to shift.
      */
      template <typename T>
      OStream & write(const T & t);

      /** \brief Pass the given stream modifier to the destination stream(s), but only if the current
                 message chatter level is less than or equal to the maximum chatter level.
          \param func The stream modifier.
      */
      OStream & operator <<(std::ostream & (*func)(std::ostream &));

      /** \brief Pass the given stream modifier to the destination stream(s), but only if the current
                 message chatter level is less than or equal to the maximum chatter level.
          \param func The stream modifier.
      */
      OStream & operator <<(std::ios & (*func)(std::ios &));

      /** \brief Pass the given stream modifier to the destination stream(s), but only if the current
                 message chatter level is less than or equal to the maximum chatter level.
          \param func The stream modifier.
      */
      OStream & operator <<(std::ios_base & (*func)(std::ios_base &));

      /** \brief Pass the given stream modifier to the destination stream(s), but only if the current
                 message chatter level is less than or equal to the maximum chatter level.
          \param func The stream modifier.
      */
      OStream & operator <<(OStream & (*func)(OStream &));

      /** \brief Change the current chatter level for messages written to the stream. This has no
                 effect on the maximum chatter level currently selected by the user/client.

                 If the message chatter level is greater than the maximum client chatter level, future
                 output will not be sent to any of this stream's destinations.
          \param chat_level The new chatter level for messages to be written to the output stream.
      */
      OStream & setChatLevel(unsigned int chat_level);

      /** \brief Return the prefix string which precedes each new line of output on this stream.
      */
      const std::string & getPrefix() const;

      /** \brief Set the prefix string which precedes each new line of output on this stream.
          \param prefix The new prefix to use.
      */
      void setPrefix(const std::string prefix);

      /** \brief Connect a destination stream to the output of this stream. Output from this stream will
                 be forwarded to the destination.
          \param dest The destination stream being connected.
      */
      void connect(std::ostream & dest);

      /** \brief Disconnect a destination stream to the output of this stream. Output from this stream will
                 no longer be forwarded to the destination.
          \param dest The destination stream being disconnected.
      */
      void disconnect(std::ostream & dest);

      /** \brief Connect a destination stream to the output of this stream. Output from this stream will
                 be forwarded to the destination.
          \param dest The destination stream being connected.
      */
      void connect(OStream & dest);

      /** \brief Disconnect a destination stream to the output of this stream. Output from this stream will
                 no longer be forwarded to the destination.
          \param dest The destination stream being disconnected.
      */
      void disconnect(OStream & dest);

      /** \brief Return current setting of stream format flags. See std::ios_base documentation for more details.
      */
      std::ios_base::fmtflags flags() const;

      /** \brief Set stream format flags, and return flags' previous setting. See std::ios_base documentation for more details.
          \param new_flags The new flags value.
      */
      std::ios_base::fmtflags flags(std::ios_base::fmtflags new_flags);

      /** \brief Add stream format flag(s), and return flags' previous setting. See std::ios_base documentation for more details.
          \param new_flags The new flag value.
      */
      std::ios_base::fmtflags setf(std::ios_base::fmtflags new_flags);

      /** \brief Add stream format flag(s), and return flags' previous setting. See std::ios_base documentation for more details.
          \param new_flags The new flag value.
          \param mask Mask to apply prior to adding new flag(s).
      */
      std::ios_base::fmtflags setf(std::ios_base::fmtflags new_flags, std::ios_base::fmtflags mask);

      /** \brief Unset stream format flags. See std::ios_base documentation for more details.
          \param mask Mask to apply to flags.
      */
      void unsetf(std::ios_base::fmtflags mask);

      /** \brief Return the current precision of this stream.
      */
      std::streamsize precision() const;

      /** \brief Set the precision of all the streams to which this stream forwards its output.
                 Return the original precision.
          \param new_precision The new precision of the stream.
      */
      std::streamsize precision(std::streamsize new_precision);

      /** \brief Return the current width of this stream.
      */
      std::streamsize width() const;

      /** \brief Set the width of all the streams to which this stream forwards its output.
                 Return the original width.
          \param new_width The new width of the stream.
      */
      std::streamsize width(std::streamsize new_width);

      /** \brief Return the current fill character of this stream.
      */
      char fill() const;

      /** \brief Set the fill character of all the streams to which this stream forwards its output.
                 Return the original fill character.
          \param new_fill The new fill character of the stream.
      */
      char fill(char new_fill);

      OStream & operator <<(const std::string & x) { return write(x); }
      OStream & operator <<(const char * x) { return write(x); }
      OStream & operator <<(bool x) { return write(x); }
      OStream & operator <<(char x) { return write(x); }
      OStream & operator <<(signed char x) { return write(x); }
      OStream & operator <<(signed short x) { return write(x); }
      OStream & operator <<(signed int x) { return write(x); }
      OStream & operator <<(signed long x) { return write(x); }
      OStream & operator <<(signed long long x) { return write(x); }
      OStream & operator <<(unsigned char x) { return write(x); }
      OStream & operator <<(unsigned short x) { return write(x); }
      OStream & operator <<(unsigned int x) { return write(x); }
      OStream & operator <<(unsigned long x) { return write(x); }
      OStream & operator <<(unsigned long long x) { return write(x); }
      OStream & operator <<(float x) { return write(x); }
      OStream & operator <<(double x) { return write(x); }
      OStream & operator <<(long double x) { return write(x); }

      // Enable/disable the stream. When enabled, equivalent to chatter > maximum chatter for that stream.
      void enable(bool enable_state = true) { m_enabled = enable_state; }

    private:
      /** \brief Utility method to assist with the family of methods which get stream formatting information,
                 e.g. precision() const, flags() const, etc.

                 This uses pointers to methods in std::ios_base and its subclasses, and pointers to methods
                 in OStream. First template argument is the return type of methods which will be called.
                 Second template argument is the name of the subclass of std::ios_base in which the method
                 is declared.
          \param stdMethod Method to call to get format properties of std::ostream objects referred to by this stream.
          \param method Method to call to get format properties of OStream objects referred to by this stream.
      */
      template <typename T, typename Stream_t>
      T getStreamState(T (Stream_t::*stdMethod)() const, T (OStream::*method)() const) const;

      /** \brief Utility method to assist with the family of methods which set stream formatting information,
                 e.g. precision(std::streamsize), flags(std::ios_base::fmtflags), etc.

                 This uses pointers to methods in std::ios_base and its subclasses, and pointers to methods
                 in OStream. First template argument is the return type of methods which will be called.
                 Second template argument is the name of the subclass of std::ios_base in which the method
                 is declared.
          \param stdMethod Method to call to set format properties of std::ostream objects referred to by this stream.
          \param method Method to call to set format properties of OStream objects referred to by this stream.
          \param method Method to call to get format properties of OStream objects referred to by this stream.
          \param arg Argument of set methods.
      */
      template <typename T, typename Stream_t>
      T setStreamState(T (Stream_t::*stdMethod)(T), T (OStream::*method)(T), T (OStream::*getMethod)() const, T arg);

      StdStreamCont_t m_std_stream_cont;
      OStreamCont_t m_stream_cont;
      std::string m_prefix;
      unsigned int m_chat_level;
      bool m_enabled;
      bool m_use_chatter;
  };

  /** \class Chat
      \brief Functor utility to change the chatter level of a stream at any point in a line of shift operators.
  */
  class Chat {
    public:
      /** \brief Create a Chat object with the given message chatter level.
          \param chat_level The message chatter level.
      */
      Chat(unsigned int chat_level): m_chat_level(chat_level) {}

      /** \brief Functor operator which modifies the message chatter level of the target stream. The maximum
                 chatter level of the stream is unaffected.
          \param os The output stream whose chatter level will be set.
      */
      OStream & operator()(OStream & os) const { return os.setChatLevel(m_chat_level); }

    private:
      unsigned int m_chat_level;
  };

  /** \brief Manipulator which calls the prefix() method of the given stream.
      \param os The stream whose prefix() method to call.
  */
  OStream & prefix(OStream & os);

  /** \brief Shift operator used to shift a Chat object to an OStream object.
      \param os The OStream object acted upon by the Chat object.
      \param chat The Chat object which modifies the OStream object.
  */
  inline OStream & operator <<(OStream & os, const Chat & chat) { return chat(os); }

  template <typename T>
  inline OStream & OStream::write(const T & t) {
    // Only modify destination streams if message chatter is less than or equal to maximum user/client chatter.
    if (m_enabled) {
      // Iterate over std::ostreams, shifting object to each in turn.
      for (StdStreamCont_t::iterator itor = m_std_stream_cont.begin(); itor != m_std_stream_cont.end(); ++itor) {
        *(*itor) << t;
      }
      // Iterate over OStreams, shifting object to each in turn.
      for (OStreamCont_t::iterator itor = m_stream_cont.begin(); itor != m_stream_cont.end(); ++itor) {
        *(*itor) << t;
      }
    }
    return *this;
  }

  inline OStream & OStream::operator <<(std::ostream & (*func)(std::ostream &)) {
    // Only modify destination streams if message chatter is less than or equal to maximum user/client chatter.
    if (m_enabled) {
      // Iterate over std::ostreams, shifting object to each in turn.
      for (StdStreamCont_t::iterator itor = m_std_stream_cont.begin(); itor != m_std_stream_cont.end(); ++itor) {
        *(*itor) << func;
      }
      // Iterate over OStreams, shifting object to each in turn.
      for (OStreamCont_t::iterator itor = m_stream_cont.begin(); itor != m_stream_cont.end(); ++itor) {
        *(*itor) << func;
      }
    }
    return *this;
  }

  inline OStream & OStream::operator <<(std::ios & (*func)(std::ios &)) {
    // Only modify destination streams if message chatter is less than or equal to maximum user/client chatter.
    if (m_enabled) {
      // Iterate over std::ostreams, shifting object to each in turn.
      for (StdStreamCont_t::iterator itor = m_std_stream_cont.begin(); itor != m_std_stream_cont.end(); ++itor) {
        *(*itor) << func;
      }
      // Iterate over OStreams, shifting object to each in turn.
      for (OStreamCont_t::iterator itor = m_stream_cont.begin(); itor != m_stream_cont.end(); ++itor) {
        *(*itor) << func;
      }
    }
    return *this;
  }

  inline OStream & OStream::operator <<(std::ios_base & (*func)(std::ios_base &)) {
    // Only modify destination streams if message chatter is less than or equal to maximum user/client chatter.
    if (m_enabled) {
      // Iterate over std::ostreams, shifting object to each in turn.
      for (StdStreamCont_t::iterator itor = m_std_stream_cont.begin(); itor != m_std_stream_cont.end(); ++itor) {
        *(*itor) << func;
      }
      // Iterate over OStreams, shifting object to each in turn.
      for (OStreamCont_t::iterator itor = m_stream_cont.begin(); itor != m_stream_cont.end(); ++itor) {
        *(*itor) << func;
      }
    }
    return *this;
  }

  inline OStream & OStream::operator <<(OStream & (*func)(OStream &)) { return func(*this); }

  template <typename T, typename Stream_t>
  inline T OStream::getStreamState(T (Stream_t::*stdMethod)() const, T (OStream::*method)() const) const {
    T orig;
    // First try getting the information from the first std::stream object which is referred to by this stream.
    if (!m_std_stream_cont.empty()) orig = ((*m_std_stream_cont.begin())->*stdMethod)();
    // First try getting the information from the first OStream object which is referred to by this stream.
    else if (!m_stream_cont.empty()) orig = ((*m_stream_cont.begin())->*method)();
    return orig;
  }

  template <typename T, typename Stream_t>
  inline T OStream::setStreamState(T (Stream_t::*stdMethod)(T), T (OStream::*method)(T),
    T (OStream::*getMethod)() const, T arg) {
    // Return value is the current value of this particular stream property.
    T orig = (this->*getMethod)();

    // Only modify destination streams if message chatter is less than or equal to maximum user/client chatter.
    if (m_enabled) {
      // Call stdMethod for each std::ostream object.
      for (StdStreamCont_t::iterator itor = m_std_stream_cont.begin(); itor != m_std_stream_cont.end(); ++itor)
        ((*itor)->*stdMethod)(arg);

      // Call method for each OStream object.
      for (OStreamCont_t::iterator itor = m_stream_cont.begin(); itor != m_stream_cont.end(); ++itor)
        ((*itor)->*method)(arg);
    }

    return orig;
  }

  /** \brief Error stream, parallel to std::cerr. This stream has the highest possible maximum chatter, so all
             output sent directly to it will be displayed. This stream has no prefix.
  */
  extern OStream sterr;

  /** \brief Log stream, parallel to std::clog. This stream has the highest possible maximum chatter, so all
             output sent directly to it will be displayed. This stream has no prefix.
  */
  extern OStream stlog;

  /** \brief Output stream, parallel to std::cout. This stream has the highest possible maximum chatter, so all
             output sent directly to it will be displayed. This stream has no prefix.
  */
  extern OStream stout;

}

#endif
