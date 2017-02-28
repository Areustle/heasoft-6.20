//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Anneal
#include <XSFit/FitMethod/Unimplemented/Anneal/Anneal.h>
#include "Error/Error.h"
#include <typeinfo>


// Class Anneal 

Anneal::Anneal(const Anneal &right)
      : FitMethod(right), 
        m_evaluations(right.m_evaluations),
        m_startTemp(right.m_startTemp),
        m_reductionFactor(right.m_reductionFactor),
        m_cycles(right.m_cycles)
{
}

Anneal::Anneal (const std::string& initString)
      : FitMethod("anneal",initString),
        m_evaluations(10000),
        m_startTemp(1.0),
        m_reductionFactor(0.85),
        m_cycles(20)
{
  processMethodString(initString);
}


void Anneal::doPerform (Fit* fit)
{
}

void Anneal::doProcessMethodString (const string& methodInput)
{
}

Anneal* Anneal::clone () const
{
  return new Anneal(*this);
}

void Anneal::copy (const FitMethod& right)
{
  if ( typeid(*this) != typeid(right) ) 
          throw RedAlert("*** Programmer Error: copying to wrong type ***");  
  Anneal __temp(static_cast<const Anneal&>(right));
  swap(__temp);  
}

void Anneal::swap (FitMethod& right)
{
  FitMethod::swap(right);
  Anneal&  that = static_cast<Anneal&>(right);
  std::swap(m_reductionFactor,that.m_reductionFactor);
  std::swap(m_evaluations,that.m_evaluations);
  std::swap(m_startTemp,that.m_startTemp);
  std::swap(m_cycles,that.m_cycles);
}

string Anneal::fullName () const
{
  return string("Anneal");
}

// Additional Declarations
