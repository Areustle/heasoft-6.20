#include <XSUtil/Utils/VerbosityManager.h>
#include <XSUtil/Error/Error.h>

VerbosityManager::VerbosityManager(const int startConLevel, const int startLogLevel)
 : m_conVerbose(),
   m_logVerbose(),
   m_restrictedVerbose()
{
   if (startConLevel < 0 || startLogLevel < 0)
   {
      throw RedAlert("Programmer Error: Initializing with negative verbose level.\n");
   }
   m_conVerbose.push(startConLevel);
   m_logVerbose.push(startLogLevel);
}

VerbosityManager::~VerbosityManager()
{
}

int VerbosityManager::getConVerbose() const
{
   int level = m_restrictedVerbose.size() ?
         m_restrictedVerbose.front() : m_conVerbose.top();
   return level;
}

int VerbosityManager::getLogVerbose() const
{
   int level = m_restrictedVerbose.size() ?
         m_restrictedVerbose.front() : m_logVerbose.top();
   return level;
}

bool VerbosityManager::setVerbose(const int conLevel, const int logLevel)
{
   // This ASSUMES m_conVerbose and m_logVerbose stacks are NEVER empty.
   bool isSet=false;
  
   if (m_restrictedVerbose.empty())
   {
      if (conLevel >= 0)
         m_conVerbose.push(conLevel);
      else
         m_conVerbose.push(m_conVerbose.top());
      if (logLevel >= 0)
         m_logVerbose.push(logLevel);
      else
         m_logVerbose.push(m_logVerbose.top());
      isSet = true;
   }
   return isSet;
}

bool VerbosityManager::removeVerbose()
{
   bool isRemoved=false;   
   if (m_restrictedVerbose.empty())
   {
      // If this is called when con/log verbose stacks are size 1 or 0,
      // it means programmer has mismatched the set/remove call pairs
      // at some point.
      if (m_conVerbose.size() < 2 || m_logVerbose.size() < 2)
      {
         throw RedAlert("Mismatched set/remove verbosity level calls.\n");
      }
      m_conVerbose.pop();
      m_logVerbose.pop();
      isRemoved=true;
   }
   return isRemoved;
}

int VerbosityManager::getRestrictedVerbose() const
{
   // Remember, we want the outermost of nested settings to have precedence,
   // so get value from the front of the deque.
   int level=-1;
   if (m_restrictedVerbose.size())
      level = m_restrictedVerbose.front();
   return level;
}

void VerbosityManager::setRestrictedVerbose(const int level)
{
   if (level < 0)
   {
      throw RedAlert("Programmer Error: Setting a negative restrictedVerbose level.\n");
   }
   m_restrictedVerbose.push_back(level);
}

void VerbosityManager::removeRestrictedVerbose()
{
   if (m_restrictedVerbose.empty())
   {
      throw RedAlert("Mismatched set/remove restricted verbosity calls.\n");
   }
   m_restrictedVerbose.pop_back();
}
