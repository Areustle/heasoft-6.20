//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// MixUtility
#include <XSModel/Model/Component/MixUtility.h>
#include <XSstreams.h>
#include <iostream>


// Class MixUtility::IncompatibleData 

MixUtility::IncompatibleData::IncompatibleData (const string& diag)
  : YellowAlert(" loaded data has incorrect information for mixing model component ")
{
  tcerr << diag << '\n';
}


// Class MixUtility::NoDataPresent 

MixUtility::NoDataPresent::NoDataPresent (const string& diag)
  : YellowAlert(" no data present: mixing operations cannot be performed ")
{
  if ( diag.length() > 0) tcerr << ": Model" << diag ;
  tcerr << '\n';
}


// Class MixUtility::DataOrderingError 

MixUtility::DataOrderingError::DataOrderingError (const string& diag)
  : YellowAlert(" observations in group have inconsistent initialization data (check order of data entry)  ")
{
  if ( diag.length() > 0) tcerr << ": " << diag ;
  tcerr << '\n';
}


// Class MixUtility 

MixUtility::MixUtility (const string& name)
  : m_name(name),
    m_specNums()
{
}


void MixUtility::initialize (const std::vector<Real>& params, const IntegerArray& specNums, const std::string& modelName)
{
}

void MixUtility::verifyData ()
{
}

void MixUtility::initializeForFit (const std::vector<Real>& params, bool paramsAreFrozen)
{
}

// Additional Declarations
  MixUtility::~MixUtility() {}
