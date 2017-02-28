//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Parameter/ParameterLink.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSstream.h>
#include <iomanip>

// ResponseParam
#include <XSModel/Parameter/ResponseParam.h>


// Class ResponseParam 

ResponseParam::ResponseParam (const string& initString, Response* responseParent, Response::ResponseParType parType)
  : ModParam(initString),
    m_responseParent(responseParent),
    m_parType(parType),
    m_isPrompt(false)
{
}


ResponseParam::~ResponseParam()
{
}


ResponseParam* ResponseParam::clone (Component* p) const
{
  // p is just a dummy variable in this context.  ResponseParams
  // have no Component parent.
  p=0;
  ResponseParam* cloned = new ResponseParam(*this);
  cloned->parent(p);
  return cloned;
}

void ResponseParam::freeze ()
{
   ModParam::freeze();
}

void ResponseParam::changeValue (const string& parString)
{
  bool isOk = false;
  string newParString = parString;
  // Don't let 'em out of here until either valid input has been
  // entered, or they've accepted the defaults.
  while (!isOk)
  {
     try
     {
        isOk = processValues(newParString);
     }
     catch (YellowAlert&)
     {
        // If ANY problems, have to try again.
     }
     if (!isOk)
     {
        rePrompt(newParString);
        if (newParString == XSparse::SKIP() || 
                        newParString == XSparse::USE_DEFAULT())
        {
           // They've accepted the defaults after all, 
           // make no changes to param values.
           isOk = true;
        }
     }
  }
}

bool ResponseParam::processValues (const string& parString)
{

  // This is a bit different from the regular ModParam::processValues
  // because it's possible for a link expression to find its way down
  // here as a result of reprompting.  (ResponseParam's changeValue
  // reprompts until a valid string is entered.)
  bool isOK = false;
  if (parString.size() && parString[0] == '=')
  {
     thaw();
     link(parString.substr(1));
     // If the above didn't throw, assume everything is OK.
     isOK = true;
  }
  else
     isOK = ModParam::processValues(parString);
  return isOK;
}

void ResponseParam::rePrompt (string& newString) const
{
  newString.erase();
  // Assumes only two parTypes for now...
  tcout <<  "\nInput parameter value, delta, min, bot, top, and max values for ..."
        << std::endl;
  try
  {
     tcout << parameterSetting() << std::endl;
     string promptString(m_responseParent->makeParamPromptString());
     promptString += name();
     promptString += ">";
     // This will throw if user enters "/*", but don't want to
     // print "terminated" message.
     tperr << xsverbose(999);
     XSparse::basicPrompt(promptString, newString);
    tperr << xsverbose();
     if (newString.empty())   newString = XSparse::USE_DEFAULT();
  }
  catch (YellowAlert&)
  {
     newString = XSparse::SKIP();
     tperr << xsverbose();
  }
}

int ResponseParam::setValue (Real val, const char key)
{
  int status = ModParam::setValue(val, key);
  if (!m_isPrompt && (key == 'a' || key == 'z' || key == 'v'))
  {
     m_responseParent->applyGainFromFit(m_parType);
  }
  return status;
}

void ResponseParam::setCompute (bool flag) const
{
  // Find the model object (if any) that points to the response object 
  // that owns this parameter. 
  // (Can't assume models container even exists at this point, as
  //  this can get called through roundabout way (destruction of param
  //  links) during program shut-down.)
  if (XSContainer::models)
  {
     Model* affectedModel = XSContainer::models->lookup(m_responseParent);
     if (affectedModel)
     {
        affectedModel->setComputeFlag(flag);
     }
  }
}

void ResponseParam::reset () throw (Parameter::ResetFailure)
{
}

void ResponseParam::reevaluateLink () const
{
   m_responseParent->applyGainFromFit(m_parType);
}

std::ostream& ResponseParam::put (std::ostream& s) const
{
    using namespace std;
    ios_base::fmtflags saveFlags(s.flags());
    streamsize savePrec(s.precision());

    // Hardcode rmodel name to "gain" until general rmodel interface
    // is implemented.

    s << setw(4) << index()
      << setw(6) << m_responseParent->spectrumNumber() <<"    "
      << setw(9) << left << "gain"
      << setw(11) << left << name()
      << setw(7) << left << unit();

    s.precision(savePrec);
    s.flags(saveFlags);

    return ModParam::put(s);
}

void ResponseParam::setValuesFromString (const string& paramStr)
{
   string::size_type startPos = paramStr.find_first_not_of(" \t");
   string strippedParStr(paramStr.substr(startPos));
   if (strippedParStr.length())
   {
      try
      {
         m_isPrompt = true;
         modify(strippedParStr);
         m_isPrompt = false;
      }
      catch (...)
      {
         m_isPrompt = false;
         throw;
      }
   }
}

string ResponseParam::getParameterLabel () const
{
   std::ostringstream oss;
   oss << "resp " << m_responseParent->sourceNumber() << ":"
        << index();
   return oss.str();
}

// Additional Declarations
