//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// XSstream
#include <XSUtil/Utils/XSstream.h>

#include <XSstreams.h>
#include <XSUtil/Utils/IosHolder.h>
#include <XSUtil/Utils/VerbosityManager.h>
#include <typeinfo>
#include <cstring>
#include <cstddef>
using std::streambuf;

XSstream tpout;
XSstream tperr;
XSstream tpcon;
XSstream tpin;

const size_t NORM_OUTPUT =  10;      


// Class XSstreambuf 

XSstreambuf::XSstreambuf()
      : m_unbuffered(false),
        m_charTaken(false),
        m_ch('\0'),
        m_pbChars(4),
        m_bufLen(0),
        m_buffer(0)
{
   m_channel = 0;
}

XSstreambuf::XSstreambuf (XSchannel* chan, std::streamsize bufferLen)
      : m_unbuffered(false),
        m_charTaken(false),
        m_ch('\0'),
        m_pbChars(4),
        m_bufLen(0),
        m_buffer(0)
{
    m_channel = 0;
    if (bufferLen > 0)
    {
        setbuf(new char[bufferLen],bufferLen);
        bufLen(bufferLen);
    }
    else if (bufferLen < 0)
    {
        setbuf(new char[-bufferLen],-bufferLen);
        bufLen(-bufferLen);
    }
    else
    {
        setbuf(0,0); // no buffering
    }
    sync();
}


XSstreambuf::~XSstreambuf()
{
    sync();

    delete [] m_buffer;
    delete m_channel;
}


int XSstreambuf::sync ()
{
    // if this is the streambuf for an input stream
    if ( gptr() && egptr() > gptr() ) return EOF;


    // if this is the streambuf for an output stream
    // flush waiting output
    if ( pptr() && pptr() > pbase() ) return bufferOut();

    // if there's nothing in the buffers, return 0.
    return 0;
}

int XSstreambuf::overflow (int c)
{

    if ( bufferOut() < 0)
    {
        return traits_type::eof();       

    }    
    else
    {
        // the following handles the case where the buffer is full,
        // which means that unbuffered output always executes starting here.
        char ca[] = {c};
        return channel()->toDevice(ca,1);
        // if the buffer has just been emptied into the channel, the following
        // updates the buffer pointer.

        // if c is not 'end of file', put the character using
        // sputc. If it is, return 0 (do nothing).

        // sputc(c) puts c into the buffer, increments the
        // buffer pointer and returns
        // the contents of the current buffer position.
        // sputc recursively calls overflow if there is no
        // room in the buffer, so this eventually terminates
        // when the EOF character is supplied to sputc.        
        if (!traits_type::eq_int_type(c,traits_type::eof())) 
        {
         	return sputc(c);

        }
        else
        {
                return traits_type::not_eof(c);
        }     
    }


}

std::streambuf* XSstreambuf::setbuf (char* s, std::streamsize n)
{
   // This is intended to be called only once per XSstreambuf
   // object lifetime.  Just to be safe though, delete any
   // previously allocated buffer and reset the pointer
   // positions for the unused direction to 0.


  if (n >= 0)
  {     if (s != 0 && n > 0) 
	{
	   setp(s,s+n); // output, buffered

           delete [] m_buffer;
           m_buffer = s;
           setg(0,0,0);
	}
        else
        {
            setp(0,0); // output, unbuffered
            unbuffered(true);
        }
  }
  else
  {
        if (s != 0) 
        {
           setg(s+pbChars(),s+pbChars(),s+pbChars());

           delete [] m_buffer;
           m_buffer = s;
           setp(0,0);
        }
        else
        {
            setg(0,0,0); // unbuffered input
            unbuffered(true);
        }
  }

  return this;
}

void XSstreambuf::setChannel (XSchannel* channel, std::streamsize bufferLen, XSstream* parent)
{
  m_channel = channel;
  m_channel->stream(parent);

  if (bufferLen != 0)  // buffered I/O
  {
        // if bufferLen < 0, setbuf sets up a get buffer.
        // if bufferLen > 0, setbuf makes a put buffer.
        // bidirectional streams are not supported, but XSstreambuf can be
        // used to construct streams going in either direction.
        if (bufferLen > 0 ) 
        {
            setbuf(new char[bufferLen],bufferLen);
            bufLen(bufferLen);
        }
        else
        {
            setbuf(new char[-bufferLen],bufferLen);
            bufLen(-bufferLen);
        }
  }
  else setbuf(0,0); // do nothing
  sync();  
}

int XSstreambuf::uflow ()
{
        // return the character stored as m_ch if there's a character stored,
        // and set the charTaken flag to false, so that a read will be forced
        // on the next call.
        // otherwise, get a character from the device using channel()->read(&c)
        // ... read is declared as XSchannel::read(char* c, size_t n = 1)...
        // and set the charTaken() flag to true, and then return the character.
        // uflow is not required for buffered input.
        if (unbuffered())
                if (charTaken())
                {
                        charTaken(false);
                        return ch();
                }
                else
                {
                        char c = '\0';
                        if (channel()->read(&c) < 0) return EOF;
                        else
                        {
                                ch(c);
                                return c;
                        }
                }
        // use default implementation for uflow in the case of buffered input.
        else 
        {
                return streambuf::uflow();
        }
}

int XSstreambuf::underflow ()
{
  if (unbuffered())
  {
        // return the character stored as m_ch if there's a character stored.
        // otherwise, get a character from the device using channel()->read(&c)
        // ... read is declared as XSchannel::read(char* c, size_t n = 1)...
        // and set the charTaken() flag to true, and then return the character.
        if (charTaken())
        {
                return ch();
        }
        else
        {
                char c = '\0';
                if ( channel()->read(&c) < 0 ) return EOF;
                else
                {
                        charTaken(true);
                        ch(c);
                        return c;
                }
        }
  }
  else
  {
         if ( gptr() < egptr() ) return *gptr();

         if (fillBuffer() < 0)
         {
                return EOF;
         }
         else
         {
                return *gptr();
         }
  }  
}

int XSstreambuf::pbackfail (int c)
{
  // pbackfail is called when putback operations fail to return
  // a character.

  // This implementation only supports putting back /rereading from the
  // buffer, not the channel.
  if (unbuffered())
  {
        // putback will fail if we reached the start of the buffer.
        // it will return the character or EOF if the putback character
        // has already been retrieved (no buffering allows only one putback).

        if (!charTaken())
        {
                if (c != EOF) ch(c);
                charTaken(true);
                return ch();
        }
        else
        {
                return EOF;
        }

  }
  else
  {
        // if we are here then the last consumed character can be made availabe
        // from the buffer. This is the case of the get area's next pointer is
        // not at the beginning of the buffer. Then, pbackfail() moves the get pointer
        // back one space and puts the input character c there. If c == EOF, it
        // just returns.
        if ( gptr() != eback() )
        {
                gbump(-1);
                if ( c != EOF ) *(gptr()) = c;
                return 0;
        }
        else return EOF;

  }
}

int XSstreambuf::fillBuffer ()
{
    // From Langer & Kreft, p.241
    // fillBuffer() is called by underflow/uflow to refill the buffer
    // in *buffered* input. The putback machinery is provided, and will
    // support limited putback operations on the stream.
    std::streamsize nPutback = std::min(gptr() - eback(),static_cast<std::ptrdiff_t>(m_pbChars));

    // copy up to pbChars() characters into the putback area, of size pbChars()
    std::memmove(m_buffer +  (pbChars() - nPutback)*sizeof(char),
                gptr()  - nPutback*sizeof(char),
                nPutback*sizeof(char) );
    int nchars = channel()->read(m_buffer+pbChars()*sizeof(char), 
                                bufLen() - pbChars());
    if (nchars < 0)
    {
        setg(0,0,0);
        return -1;
    }
    else
    {
        setg(m_buffer+pbChars() - nPutback,
             m_buffer+pbChars(),
             m_buffer+pbChars() + nchars);
    }
    return nchars;  
}

void XSstreambuf::setChannelPrompt (const std::string& ps)
{
  channel()->setPrompt(ps);
}

void XSstreambuf::setChannelPrompt (const std::string& ps, const std::vector<std::string>& prompts, const std::vector<std::string>& infos)
{
}

std::streamsize XSstreambuf::bufferOut ()
{
  if (unbuffered()) return 0;
  else
  {      
        int cw(pptr() - pbase());
        int nchars(channel()->toDevice(pbase(),cw));
        // bump the pointer whether output takes place in toDevice
        // or not: this takes care of terse output.
        pbump(-cw);
        return nchars;
  }
}

// Class XSstream::NotXSstream 

XSstream::NotXSstream::NotXSstream()
  : YellowAlert(" Input stream is not capable of this function ")
{
}


// Class XSstream 

XSstream::XSstream()
  : std::iostream(new XSstreambuf),
    m_consoleChatterLevel(NORM_OUTPUT),
    m_logChatterLevel(NORM_OUTPUT),
    m_verbosity(new VerbosityManager(NORM_OUTPUT, NORM_OUTPUT))
{
    XSstreambuf* buffer = dynamic_cast<XSstreambuf*>(rdbuf());
    init(buffer);
}

XSstream::XSstream (XSchannel* chan, std::streamsize bufferLen)
  : std::iostream(new XSstreambuf),
    m_consoleChatterLevel(NORM_OUTPUT),
    m_logChatterLevel(NORM_OUTPUT),
    m_verbosity(new VerbosityManager(NORM_OUTPUT, NORM_OUTPUT))
{
    XSstreambuf* buffer = dynamic_cast<XSstreambuf*>(rdbuf());
    init(buffer);
    buffer->setChannel(chan,bufferLen,this);
}


XSstream::~XSstream()
{
   delete m_verbosity;
   delete rdbuf();
}


void XSstream::defineChannel (XSchannel* chan, std::streamsize bufLen)
{
    XSstreambuf* buffer = dynamic_cast<XSstreambuf*>(rdbuf());
    buffer->setChannel(chan,bufLen,this);
  //m_buf->setChannel(chan,bufLen,this);


  //m_buf = initbuf(m_buf);
  //m_buf->setChannel(chan,bufLen,this);
}

void XSstream::setPrompter (std::istream& s, const std::string& ps)
{
  // write the prompt to the tied output stream, if there is one,
  // otherwise call the stream channel's setPrompt function.


  // tie() returns a pointer to the stream tied to s. 
  // Doing this this way with a static function allows me to hide the static
  // cast inside this routine.
  // The calling code will compile if s is any object inheriting from
  // istream, and therefore iostream (i.e. an XSstream).

  if ( s.tie() ) 
  {
//           *(s.tie()) << ps;  
        XSstreambuf* buffer = dynamic_cast<XSstreambuf*>(s.rdbuf());
        buffer->setChannelPrompt(ps);
  }
  else
  {
  // note that in the case of tied streams, the XSstream deals with
  // the prompting, but in the case of the GUI output the channel deals
  // with it.

        // This passes the input string down to the channel's prompting mechanism.
        // Actually, this is quite specific for TkIO, for which the
        // output is "prompted" by a tk script. 

        // More accurately it's specific to XSchannels  whose user interface is 
        // implemented by anything that's described by a string.

        // for Tk, there are no channels, so we will end up here.
        // This code however places a requirement that if there is
        // no tied output stream, the input is prompted in this way.
        // this ought to be generalized later.
        XSstream& ss = static_cast<XSstream&>(s);

        XSstreambuf* buffer = dynamic_cast<XSstreambuf*>(ss.rdbuf());
        buffer->setChannelPrompt(ps);

        //ss.m_buf->setChannelPrompt(ps);   
  }
}

void XSstream::setPrompter (std::istream& s, const std::string& ps, const std::vector<std::string>& prompts, const std::vector<std::string>& infos)
{
  // not yet implemented: for use with scripts.
}

void XSstream::setLogger (const std::string& name, bool isErr)
{
        XSstreambuf* buffer = dynamic_cast<XSstreambuf*>(rdbuf());
        buffer->channel()->setLogger(name, isErr);
}

void XSstream::closeLog ()
{
        XSstreambuf* buffer = dynamic_cast<XSstreambuf*>(rdbuf());
        buffer->channel()->closeLog();
}

void XSstream::setVerbose (int level)
{
   if (level < 0)
      m_verbosity->removeVerbose();
   else
      m_verbosity->setVerbose(level, level);
}

void XSstream::setVerbose(int conLevel, int logLevel)
{
   m_verbosity->setVerbose(conLevel, logLevel);
}

void XSstream::setRestrictedVerbose(int level)
{
   m_verbosity->setRestrictedVerbose(level);
}

void XSstream::removeRestrictedVerbose()
{
   m_verbosity->removeRestrictedVerbose();
}

int XSstream::conVerbose() const
{
   return m_verbosity->getConVerbose();
}

int XSstream::logVerbose() const
{
   return m_verbosity->getLogVerbose();
}

void XSstream::defineChannel (std::ostream& s, XSchannel* chan, std::streamsize bufLen)
{
  try
  {     XSstream& xsOut = dynamic_cast<XSstream&>(s);
        xsOut.defineChannel(chan,bufLen);
  }
  catch (std::bad_cast)
  {
        throw NotXSstream();       
  } 
}

void XSstream::defineChannel (std::istream& s, XSchannel* chan, std::streamsize bufLen)
{
  try
  {     XSstream& xsIn = dynamic_cast<XSstream&>(s);
        xsIn.defineChannel(chan,bufLen);
  }
  catch (std::bad_cast)
  {
        throw NotXSstream();       
  }       
}

int XSstream::maxChatter ()
{
  return std::max(m_consoleChatterLevel,m_logChatterLevel);
}

XSchannel* XSstream::getChannel ()
{
   XSstreambuf* buffer = dynamic_cast<XSstreambuf*>(rdbuf());
   return buffer->channel();
}

/////////////////////////////////
//Verbosity stream manipulator functions.

XSstream& XSstream::set_verbose(XSstream& s, int level)
{
   s.setVerbose(level);
   return s;
}

XSstream& XSstream::undo_verbose(XSstream& s, int dummy)
{
   s.setVerbose();
   return s;
}

XSstream& operator<< (std::ostream& os, const VerboseManip& vm)
{
   XSstream* pxs = dynamic_cast<XSstream*>(&os);
   if (!pxs)
      throw RedAlert("Wrong type of stream class for setting verbosity.\n");
   return vm.m_f(*pxs, vm.m_level);
}

///////////////////////////////////

// Class XSchannel::LogFileOpenFailure 

XSchannel::LogFileOpenFailure::LogFileOpenFailure (const std::string& diag)
  : YellowAlert(" cannot open log file: ")
{
  *IosHolder::errHolder() << diag << '\n';
}


// Class XSchannel 

XSchannel::XSchannel()
{
}


XSchannel::~XSchannel()
{
}


void XSchannel::setPrompt (const std::string& ps)
{
  // default implementation: do nothing;
}

void XSchannel::prompts (const std::string& script, const std::vector<std::string>& prompts, const std::vector<std::string>& infos)
{
  // default implementation: do nothing;
}

void XSchannel::setLogger (const std::string& name, bool isErr)
{
        internalSetLogger(name, isErr);
}

void XSchannel::closeLog ()
{
        internalCloseLog();
}

size_t XSchannel::logVerbose () const
{
  return m_stream->logVerbose();
}

size_t XSchannel::conVerbose () const
{
  return m_stream->conVerbose();
}

int XSchannel::toDevice (const char* s, std::streamsize n)
{
  if (write(s,n) != n) return -1;
  else return 0;
}

// Additional Declarations
