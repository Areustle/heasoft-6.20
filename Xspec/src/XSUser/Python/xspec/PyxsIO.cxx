#include <XSUser/Python/xspec/PyxsIO.h>
#include <XSUser/Python/xspec/PyXSutils.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSstreams.h>
#include <algorithm>
#include <cstring>
#include <stdio.h>

PyObject* _pyXspec_allowPrompting(PyObject* self, PyObject *args)
{
   int promptFlag=0;
   if (!PyArg_ParseTuple(args, "i", &promptFlag))
   {
      string msg("Programmer Error: Parsing allowPrompting");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   if (!promptFlag)
       XSparse::executingScript(true);
   else
       XSparse::executingScript(false);
   return Py_BuildValue("i",0);
}

PyObject* _pyXspec_closeLog(PyObject* self, PyObject *args)
{
   PyxsIO* channel = dynamic_cast<PyxsIO*>(tpout.getChannel());
   PyObject* pyFile = channel->logFile();
   if (pyFile)
   {
      tpout.closeLog();
      tperr.closeLog();
      tpout << "Log file closed." << std::endl;
   }
   else
   {
      tpout << "No log file is currently open." << std::endl;
   }
   return Py_BuildValue("i",0);
}

PyObject* _pyXspec_getChatter(PyObject* self, PyObject *args)
{
   int outputID=0;
   if (!PyArg_ParseTuple(args, "i", &outputID))
   {
      string msg("Programmer Error: Parsing getChatter");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const int level = outputID ? tpout.logChatterLevel() :
                                tpout.consoleChatterLevel();
   return Py_BuildValue("i",level);
}

PyObject* _pyXspec_setChatter(PyObject* self, PyObject *args)
{
   int outputID=0;
   int level=0;
   if (!PyArg_ParseTuple(args, "ii", &outputID, &level))
   {
      string msg("Programmer Error: Parsing setChatter");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   if (!outputID)
      tpout.consoleChatterLevel(level);
   else
      tpout.logChatterLevel(level);
   return Py_BuildValue("i",level);
}

PyObject* _pyXspec_getLog(PyObject* self, PyObject *args)
{
   PyxsIO* channel = dynamic_cast<PyxsIO*>(tpout.getChannel());
   PyObject* pyFile = channel->logFile();
   if (!pyFile)
   {
      Py_INCREF(Py_None);
      return Py_None;
   }
   Py_INCREF(pyFile);
   return pyFile;   
}

PyObject* _pyXspec_setLog(PyObject* self, PyObject *args)
{
   const char* fileName=0;
   if (!PyArg_ParseTuple(args, "s", &fileName))
   {
      string msg("Programmer Error: Parsing setLog arguments.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   string nameStr(fileName);
   PyObject* pyFile=0;
   try
   {
      PyxsIO* channel = dynamic_cast<PyxsIO*>(tpout.getChannel());
      if (channel->logFile())
      {
         tpout.closeLog();
         tperr.closeLog();
         tpout << "Will no longer write to previously opened log file." << std::endl;
      }
      tpout << "Logging to file: " << nameStr << std::endl;
      tpout.setLogger(nameStr, false);
      tperr.setLogger(nameStr, true);
      pyFile = channel->logFile();
   }
   catch(...)
   {
      string msg("Error: Unable to set log file: ");
      msg += nameStr;
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   Py_INCREF(pyFile);
   return pyFile;
}
// end setLog


PyxsIO::PyxsIO(std::istream* in, std::ostream* out)
 : XSchannel(),
   m_inChannel(in),
   m_outChannel(out),
   m_logFile(0),
   m_prompt()
{
}

PyxsIO::~PyxsIO()
{
   // Must NOT attempt to decrement the reference count to the 
   // PyFile m_logFile from here.  PyxsIO objects do not get destroyed
   // until exiting from the Python shell, and at that point it is no
   // longer safe to use Py_DECREF.  
}

std::streamsize PyxsIO::read(char* s, std::streamsize n)
{
   std::streamsize nChars = 0;
   *m_outChannel << m_prompt << std::flush;
   string inString;
   
   // This call to clear std::cin (or *m_inChannel - same thing)
   // is a bit of a hack.  When running PyXspec on Linux,
   // if a user hits ctrl-c when the program is waiting for input 
   // inside a getline call on an XSstream object (as happens in the
   // XSparse::basicPrompt function), cin's fail() flag is set to True.
   
   // Now XSstream has an XSstreambuf which owns this PyxsIO XSchannel,
   // so the outer getline(XSstream,str) call ends up in here to make the
   // inner getline(cin,str) call below.  If cin.fail() is True, THIS
   // getline call will immediately exit without ever prompting the user.
   // This leads to an infinite loop when the outer getline call happens
   // to be enclosed in a "while(!Ok_response)" loop.  (It was first
   // discovered during prompting for a new response name during a 'fakeit' run.)
   
   m_inChannel->clear();
   std::getline(*m_inChannel, inString);
   const string::size_type len = inString.length();
   nChars = std::min(static_cast<std::streamsize>(len), n-1);
   memcpy(s,inString.c_str(),nChars);
   memset(s+nChars,'\n',1);
   return nChars+1;
}

std::streamsize PyxsIO::write(const char* s, std::streamsize n)
{
   int conTrivia = stream()->conVerbose();
   int logTrivia = stream()->logVerbose();
   if (conTrivia && (conTrivia <= stream()->consoleChatterLevel()) )      
   {
      string outStr(s, n);         
      *m_outChannel << outStr << std::flush;
   }
   if (m_logFile && logTrivia && logTrivia <= stream()->logChatterLevel())
   {
      // Place a '#' at the start of each line.
      string outStr(s, n);
 /*     string::size_type nlPos = outStr.find_first_of('\n');
      while (nlPos != string::npos)
      {
         ++nlPos;
         outStr = outStr.insert(nlPos,"#");
         nlPos = outStr.find_first_of('\n', nlPos);
      }
 */     if (PyFile_WriteString(outStr.c_str(), m_logFile))
      {
         // A likely way to get in here is if user closed the current
         // log file from their own python script (file.close()).  
         // So, perform same action as if they had called the _pyXspec
         // closeLog() function.
         *m_outChannel <<"***Failed attempt to write to log file.\n"
             <<"***  Log file will be closed."<< std::endl;
         tpout.closeLog();
         tperr.closeLog();
      }
      else
      {
#ifdef PYXSPEC3
         PyObject_CallMethod(m_logFile,"flush",NULL);
#endif
      }
   }
   return n;
}

void PyxsIO::internalSetLogger (const std::string& name, bool isErr)
{
   // Only create the file for tpout.  tperr merely gets a reference to it.
   if (!isErr)
   {
      if (m_logFile)
      {
         // Should never get here if the setLog PyToC wrapper function is
         // explicitly closing earlier log file before opening new one.
         Py_DECREF(m_logFile);
      }
#ifdef PYXSPEC3
      PyObject *ioMod = PyImport_ImportModule("io");
      // This is the equivalent of io.open(args)
      m_logFile = PyObject_CallMethod(ioMod,"open","ss",name.c_str(),"wt");
      Py_DECREF(ioMod);
      
#else
      FILE *fp = fopen(const_cast<char*>(name.c_str()), "w");
      if (!fp)
         throw LogFileOpenFailure(name);

      m_logFile = PyFile_FromFile(fp, "w", const_cast<char*>(name.c_str()), fclose);
#endif
      if (!m_logFile)
         throw LogFileOpenFailure(name);
   }
   else
   {
      PyxsIO* tpoutChan = dynamic_cast<PyxsIO*>(tpout.getChannel());
      if (tpoutChan)
      {
         if (m_logFile)
         {
            Py_DECREF(m_logFile);
         }
         m_logFile = tpoutChan->m_logFile;
         Py_INCREF(m_logFile);
      }
   }
}

void PyxsIO::internalCloseLog ()
{
   if (m_logFile)
   {
      // User may still have an arbitrary number of current references
      // to *m_logFile in their Python script, so there's no guarantee
      // that the log file will be closed after tpout and tperr go through
      // here.  But we can at least make sure the log file buffer is flushed.

#ifdef PYXSPEC3
      PyObject_CallMethod(m_logFile,"flush",NULL);
#else
      FILE* fp = PyFile_AsFile(m_logFile);
      if (fp)
         fflush(fp);
#endif
      Py_DECREF(m_logFile);
      m_logFile = 0;
   }

}

void PyxsIO::setPrompt (const string& ps)
{
   m_prompt = ps;
}
