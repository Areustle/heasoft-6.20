//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// EventHandlers
#include <XSUtil/Signals/EventHandlers.h>
#include <assert.h>


// Class EventHandler 

EventHandler::EventHandler()
{
}


EventHandler::~EventHandler()
{
}


// Additional Declarations

// Class SIGINT_Handler 

SIGINT_Handler::SIGINT_Handler()
  : EventHandler(), 
    m_interrupted(0)
{
}


SIGINT_Handler::~SIGINT_Handler()
{
}


int SIGINT_Handler::handleSignal (int sigNum)
{
  assert(sigNum == SIGINT);
  m_interrupted = 1;
  return 0;
}

// Additional Declarations
