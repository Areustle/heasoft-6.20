//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// RandomizerBase
#include <XSFit/Randomizer/RandomizerBase.h>


// Class RandomizerBase 

RandomizerBase::RandomizerBase(const RandomizerBase &right)
  :m_name(right.m_name),
   m_initString(right.m_initString)
{
}

RandomizerBase::RandomizerBase (const string& name)
  :m_name(name),
   m_initString()
{
}


RandomizerBase::~RandomizerBase()
{
}


void RandomizerBase::doInitializeRun (const Fit* fit)
{
}

void RandomizerBase::doInitializeLoad ()
{
}

void RandomizerBase::doAcceptedRejected (const RealArray& parameterValues, bool isAccepted)
{
}

const RealArray* RandomizerBase::getCovariance (const Fit* fit)
{
   return 0;
}

// Additional Declarations
