//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Genetic
#include <XSFit/FitMethod/Unimplemented/Genetic/Genetic.h>
#include "Error/Error.h"
#include <typeinfo>


// Class Genetic 

Genetic::Genetic(const Genetic &right)
  : FitMethod(right), 
    m_numberOfGenerations(right.m_numberOfGenerations),
    m_init(right.m_init)
{
}

Genetic::Genetic (const std::string& initString)
   : FitMethod("genetic",initString),m_numberOfGenerations(500),m_init(false)
{
}


void Genetic::doPerform (Fit* fit)
{
}

void Genetic::doProcessMethodString (const string& methodInput)
{
}

Genetic* Genetic::clone () const
{
  return new Genetic(*this);
}

void Genetic::copy (const FitMethod& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  Genetic that(static_cast<const Genetic&>(right));
  swap(that);   
}

void Genetic::swap (FitMethod& right)
{
  FitMethod::swap(right);
  Genetic&  that = static_cast<Genetic&>(right);
  std::swap(m_init,that.m_init);
  std::swap(m_numberOfGenerations,that.m_numberOfGenerations);
}

string Genetic::fullName () const
{
  return string("Genetic");
}

// Additional Declarations
