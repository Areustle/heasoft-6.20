//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Observer
#include <XSUtil/Utils/Observer.h>

#include <algorithm>


// Class Subject 

Subject::Subject()
{
    m_observers = new std::list<Observer*>;
}


Subject::~Subject()
{
   delete m_observers;
}


void Subject::Attach (Observer* obs)
{
    m_observers->push_back(obs);
}

void Subject::Detach (Observer* obs)
{
    std::list<Observer*>::iterator doomed = std::find(m_observers->begin(),m_observers->end(),obs);
    m_observers->erase(doomed);
}

void Subject::Notify ()
{
        std::list<Observer*>::iterator i;

        for (i = m_observers->begin(); i != m_observers->end(); ++i)
        {
                (*i)->Update(this);       
        }
}

// Additional Declarations

// Class Observer 

Observer::Observer()
{
}


Observer::~Observer()
{
}


// Additional Declarations
