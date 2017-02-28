//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// FitMethod
#include <XSFit/Fit/FitMethod.h>
#include <xsTypes.h>
#include <XSFit/Fit/Fit.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSstreams.h>
#include <sstream>
#include <cmath>
#include <iomanip>
#include <map>


// Class FitMethod::FitError 

FitMethod::FitError::FitError (int errCode, int paramNumber)
    : YellowAlert(" Fit algorithm returned error: ")
{
}


// Class FitMethod 
std::set<FitMethod*> FitMethod::s_fitMethodObjs;
std::map<string,FitMethod*> FitMethod::s_subMethods;

FitMethod::FitMethod(const FitMethod &right)
  : m_secondDerivativeRequired(right.m_secondDerivativeRequired),
    m_pegsParameters(right.m_pegsParameters),
    m_firstDerivativeRequired(right.m_firstDerivativeRequired),
    m_deltaCrit(right.m_deltaCrit),
    m_numberOfTrials(right.m_numberOfTrials),
    m_delayedGratification(right.m_delayedGratification),
    m_betaNormCrit(right.m_betaNormCrit),
    m_selectedSubMethod(right.m_selectedSubMethod),
    m_evalue(right.m_evalue),
    m_evector(right.m_evector),
    m_covariance(right.m_covariance)
{
}

FitMethod::FitMethod (const std::string& initString)
  : m_secondDerivativeRequired(false),
    m_pegsParameters(false),
    m_firstDerivativeRequired(false),
    m_deltaCrit(.01),
    m_numberOfTrials(10),
    m_delayedGratification(false),
    m_betaNormCrit(-10.0),
    m_selectedSubMethod(),
    m_evalue(),
    m_evector(),
    m_covariance()
{
   StringArray params;
   IntegerArray iParams;
   XSparse::collateByWhitespace(params, initString);
   // We control initString and it shouldn't contain commas. 
   // Also note that param 1 (0-based) should be nTrials. 
   // Therefore for iParams...
   for (size_t i=0; i<params.size(); ++i)
      iParams.push_back(i+1);
   // This can throw.
   processMethodString(iParams, params);
}


FitMethod::~FitMethod()
{
}



FitMethod* FitMethod::get (const std::string& name)
{
  FitMethod* selectedObj = 0;
  string lcName(XSutility::lowerCase(name));
  std::map<std::string,FitMethod*>::const_iterator itSubMethod = 
                s_subMethods.lower_bound(lcName);
  std::map<std::string,FitMethod*>::const_iterator itSubMethodEnd = 
                s_subMethods.end();
  if (itSubMethod != itSubMethodEnd && itSubMethod->first.find(lcName)==0)
  {
     selectedObj = itSubMethod->second->clone();
     selectedObj->m_selectedSubMethod = itSubMethod->first;
  }
  return selectedObj;
}

void FitMethod::secondDerivativeRequired (bool value)
{
  m_secondDerivativeRequired = value;
}

void FitMethod::firstDerivativeRequired (bool value)
{
  m_firstDerivativeRequired = value;
}

void FitMethod::registerMethod (const string& name, FitMethod* method)
{
  s_subMethods[name] = method;
  if (s_fitMethodObjs.find(method) == s_fitMethodObjs.end())
     s_fitMethodObjs.insert(method);
}

void FitMethod::processMethodString (const IntegerArray& iParams, const StringArray& params)
{
   using namespace std;

   int testNTrials = m_numberOfTrials;
   Real testDeltaCrit = m_deltaCrit;
   Real testBetaNormCrit = m_betaNormCrit;
   int itrials = 1;
   int idelta = 2;
   int ibeta = 3;

   for (size_t i=0; i<iParams.size(); ++i)
   {

     // test for the presence of the delayed gratification arguments
     
     if ( params[i].find_first_of("dn") == 0 ) {

       if (iParams[i] <= itrials ) itrials++;
       if (iParams[i] <= idelta ) idelta++;
       if (iParams[i] <= ibeta ) ibeta++;
       m_delayedGratification = false;
       if ( params[i].find_first_of("d") == 0 ) m_delayedGratification = true;

     } else {

       istringstream testIss(params[i]);
       if (iParams[i] == itrials) {
	 if (!(testIss >> testNTrials) || !testIss.eof() || testNTrials <= 0) {
	   throw YellowAlert("Improper nTrials value.\n");
	 }
       } else if (iParams[i] == idelta) {
	 if (!(testIss >> testDeltaCrit) || !testIss.eof()) {
	   throw YellowAlert("Improper critical delta value.\n");
	 }
       } else if (iParams[i] == ibeta) {
	 if (!(testIss >> testBetaNormCrit) || !testIss.eof()) {
	   throw YellowAlert("Improper critical beta norm value.\n");
	 }
       }

     }
   }

   // In case FitMethod subclass requires any further argument processing:
   processMethodStringExtras(iParams, params);

   m_numberOfTrials = testNTrials;
   m_deltaCrit = testDeltaCrit;
   m_betaNormCrit = testBetaNormCrit;
   // If this is not the original prototype, then update prototype. 
   map<string,FitMethod*>::iterator itProto = s_subMethods.find(m_selectedSubMethod);
   if (itProto != s_subMethods.end())
   {
      itProto->second->m_numberOfTrials = m_numberOfTrials;
      itProto->second->m_deltaCrit = m_deltaCrit;  
      itProto->second->m_betaNormCrit = m_betaNormCrit;  
   }
}

string FitMethod::fitNames ()
{
  std::map<string,FitMethod*>::const_iterator itFm = s_subMethods.begin();
  std::map<string,FitMethod*>::const_iterator itFmEnd = s_subMethods.end();
  string names;
  while ( itFm != itFmEnd)
  {
          names += itFm->first;
          ++itFm;       
          if (itFm != itFmEnd) names += " | ";
          else names += " ";
  }

  return names;      
}

void FitMethod::reportProgress (std::ostream& s, Fit* fit) const
{
}

void FitMethod::perform (Fit* fit)
{

  doPerform(fit);
  fit->isStillValid(true);
}

const RealArray& FitMethod::evalue () const
{
  return m_evalue;
}

const RealArray& FitMethod::evector () const
{
  return m_evector;
}

bool FitMethod::getErrors (Fit* fit, const IntegerArray& paramNums)
{
  return false;
}

const RealArray& FitMethod::covariance () const
{
  return m_covariance;
}

void FitMethod::reportCovariance () const
{
  using namespace std;
  if (m_covariance.size())
  {
     const int WIDTH(12);
     ios_base::fmtflags saveFlags(tcout.flags());
     int savePrecision(tcout.precision());
     tcout << scientific << setprecision(3) << right;
     // This just assumes m_covariance is of size nPar*nPar. 
     size_t nPar = static_cast<size_t>(sqrt(static_cast<Real>(m_covariance.size())));
     string topBar(nPar*WIDTH, '=');
     string bottomBar(nPar*WIDTH, '-');
     tcout << endl << topBar << endl;
     tcout << "  Covariance Matrix" << endl;
     for (size_t j=1; j<=nPar; ++j)
     {
        tcout << setw(WIDTH-3) << j << "   " ;
     }
     tcout << endl;
     for (size_t i=0; i<nPar; ++i)
     {
        for (size_t j=0; j<nPar; ++j)
        {
           tcout << setw(WIDTH) << m_covariance[i*nPar + j];
        }
        tcout << endl;
     }
     tcout << bottomBar << endl;
     tcout.flags(saveFlags);
     tcout.precision(savePrecision);
  }
}

void FitMethod::clearMethods ()
{
   std::set<FitMethod*>::iterator itFm = s_fitMethodObjs.begin();
   std::set<FitMethod*>::iterator itEnd = s_fitMethodObjs.end();
   while (itFm != itEnd)
   {
      delete *itFm;
      ++itFm;
   }
   s_fitMethodObjs.clear();
}

void FitMethod::processMethodStringExtras (const IntegerArray& iParams, const StringArray& params)
{
}

void FitMethod::improve (Fit* fit)
{
   throw YellowAlert("Improve command is only implemented for Minuit methods.\n");
}

// Additional Declarations
