//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// SignalHandler
#include <XSUtil/Signals/SignalHandler.h>

// Class SignalHandler 
SignalHandler* SignalHandler::s_instance = 0;
std::map<int,EventHandler*> SignalHandler::s_signalHandlers = std::map<int,EventHandler*>();

SignalHandler::SignalHandler()
{
}


SignalHandler::~SignalHandler()
{
   // SignalHandler container doesn't strictly own the EventHandlers
   // residing in its map.  Clients are able to replace EventHandler
   // pointers when they call registerHandler, in which case they
   // are responsible for that EventHandler's memory.  At any
   // rate, if any EventHandlers are still around by this point,
   // zap 'em.
   std::map<int,EventHandler*>::iterator it = s_signalHandlers.begin();
   while (it != s_signalHandlers.end())
   {
      delete it->second;
      ++it;
   }
}


SignalHandler* SignalHandler::instance ()
{
   if (s_instance == 0)
   {
      s_instance = new SignalHandler;
   }
   return s_instance;
}

EventHandler* SignalHandler::registerHandler (int sigNum, EventHandler* eh)
{
  std::map<int,EventHandler*>::iterator it = s_signalHandlers.find(sigNum);
  EventHandler* oldEh = (it == s_signalHandlers.end()) ? 0 : it->second;
  
  // Do not let a NULL EventHandler ever go in s_signalHandlers map.
  // The dispatcher function does not do NULL pointer checking.
  if (!eh)
  {
     if (it != s_signalHandlers.end())
        s_signalHandlers.erase(it);
  }
  else
     s_signalHandlers[sigNum] = eh;

#ifdef HAVE_SIGSET
  sigset(sigNum, SignalHandler::dispatcher);
#else
  signal(sigNum, SignalHandler::dispatcher);
#endif

  return oldEh;
}

int SignalHandler::removeHandler (int sigNum)
{
  std::map<int,EventHandler*>::iterator it = s_signalHandlers.find(sigNum);
  int removed = 0;
  if (it != s_signalHandlers.end())
  {
     delete it->second;
     removed = s_signalHandlers.erase(sigNum);
  }
  return removed;
}

void SignalHandler::dispatcher (int sigNum)
{
  if (s_signalHandlers.find(sigNum) != s_signalHandlers.end())
  {
     s_signalHandlers[sigNum]->handleSignal(sigNum);
  }
}

const EventHandler* SignalHandler::getHandler (int sigNum)
{
  std::map<int,EventHandler*>::const_iterator it = s_signalHandlers.find(sigNum);
  if (it == s_signalHandlers.end())
  {
     return 0;
  }
  else
  {
     return it->second;
  }
}

// Additional Declarations
