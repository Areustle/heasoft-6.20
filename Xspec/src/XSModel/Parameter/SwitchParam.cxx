//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// SwitchParam
#include <XSModel/Parameter/SwitchParam.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Parameter/ParameterLink.h>
#include <XSstreams.h>
#include <sstream>
#include <iomanip>
#include <cmath>


// Class SwitchParam 

SwitchParam::SwitchParam(const SwitchParam &right)
      : Parameter(right),m_state(right.m_state)
{
}

SwitchParam::SwitchParam (const string& initString, Component* p)
     : Parameter(p),m_state(0)
{
  init(initString);  
}


SwitchParam::~SwitchParam()
{
}


SwitchParam* SwitchParam::clone (Component* p) const
{
  SwitchParam* cloned = new SwitchParam(*this);
  cloned->parent(p);
  return cloned;   
}

std::ostream& SwitchParam::put (std::ostream& s) const
{
  using namespace std;
  ios_base::fmtflags saveFlags(s.flags());
  const Component* component = parent();
  s  << setw(4) << index()   
     << setw(5) << component->index() << "   "
     << setw(11) << left << component->name() 
     << setw(11) << left << name()
     << setw(9) << left << " ";
  int setting = static_cast<int>(value()); 
  s << left << setw(13) << setting;
  if (isLinked())
     thisLink()->putLink(s);
  else
     s << "frozen";
  s.flags(saveFlags);

  return s;
}

bool SwitchParam::compare (const Parameter& right) const
{
  if (!Parameter::compare(right)) return false; 
  const SwitchParam& that = static_cast<const SwitchParam&>(right);
  if ( m_state != that.m_state ) return false;
  return true;  
}

void SwitchParam::init (const string& initString)
{
  std::istringstream s(initString);
  string parname;
  int state = 0;
  s >> parname >> state;
  name(parname.substr(1,parname.length()-1));
  setValue(state);  
}

void SwitchParam::changeValue (const string& parString)
{
  using std::ios_base;
  string::size_type begin = parString.find_first_not_of(" \t");
  if (begin == string::npos)
  {
     return;
  }

  // remove the link if any and reset the parameter to its file values
  // without preserving current setting
  untie(false); 

  string processedStr = XSutility::lowerCase(parString.substr(begin));
  // remove trailing blanks
  string::size_type end = processedStr.find_last_not_of(" \t");
  processedStr = processedStr.substr(0, end+1);
  Real inTest=0.0;
  int newState=0;

  // For backwards compatibility, check for true/false entry.
  if (processedStr == "true")
  {
     newState = 1;
  }
  else if (processedStr == "false")
  {
     newState = 0;
  }
  else
  {
     std::istringstream s(processedStr);
     string errMsg = "Parameter "+ name() + ":  integer value required ";
     if (!(s >> inTest) || !s.eof())
     {
        throw Parameter::InvalidInput(errMsg);
     }
     newState = static_cast<int>(std::floor(inTest));
     if (inTest != static_cast<Real>(newState))
     {
        tcout << "***Warning: Switch parameter requires an integer."
         << "\n    Floating-point value will be truncated." << std::endl;
     }
  }
  setValue(newState);  
}

void SwitchParam::link (const string& toLink)
{
  thisLink(new ParameterLink(toLink,this,true));
  links()->linkList(this,thisLink());
  isLinked(true);  
}

Real SwitchParam::value () const
{

  if (!isLinked()) return static_cast<Real>(m_state);
  else return thisLink()->linkValue();  
}

void SwitchParam::setValue (int state)
{
  if (m_state != state)
  {
          m_state = state;
          changed(true);
          setCompute(true);
          setComputeForLinks(true);
  }
}

void SwitchParam::doPreserve ()
{
  if (isLinked())
  {
        int currentValue = static_cast<int>(thisLink()->linkValue());
        links()->unlink(this);
        reset();
        m_state = currentValue;       
  }  
}

string SwitchParam::stringVal () const
{
  std::ostringstream val;
  val << m_state << " " << name();
  return val.str();
}

string SwitchParam::parameterSetting () const
{
    std::ostringstream val;
    if (!isLinked())
    {
       val << std::setw(15) << m_state;
    }
    else
    {
          val << "= " << thisLink()->linkExpression(false);
    }
    return val.str();
}

// Additional Declarations
