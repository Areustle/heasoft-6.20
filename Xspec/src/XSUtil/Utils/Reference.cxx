//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Reference
#include <XSUtil/Utils/Reference.h>
#include <algorithm>


// Class RefCounter 

RefCounter::RefCounter()
      : m_refCount(0),
        m_shareable(true)
{
}

RefCounter::RefCounter (const RefCounter& )
      : m_refCount(0),
        m_shareable(true)
{
}


RefCounter::~RefCounter()
{
}


RefCounter& RefCounter::operator = (const RefCounter& )
{
  return *this;
}

void RefCounter::addReference ()
{
  ++m_refCount;
}

void RefCounter::removeReference ()
{
  if ( --m_refCount == 0 ) delete this;
}

void RefCounter::markUnshareable ()
{
  m_shareable = false;
}

bool RefCounter::isShared ()
{
  return m_refCount > 1;
}

// Additional Declarations
