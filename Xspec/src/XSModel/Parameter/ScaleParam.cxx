//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// ScaleParam
#include <XSModel/Parameter/ScaleParam.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Parameter/ParameterLink.h>
#include <XSUtil/Error/Error.h>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <limits>


// Class ScaleParam 

ScaleParam::ScaleParam(const ScaleParam &right)
      : Parameter(right),m_factor(right.m_factor)
{
}

ScaleParam::ScaleParam (const string& initString, Component* p)
      : Parameter(p),m_factor(0.)
{
  init(initString);
}

ScaleParam::ScaleParam (const string& inputName, Real initial, Component* p)
      : Parameter(p), m_factor(initial)
{
  name(inputName);
}


ScaleParam::~ScaleParam()
{
}


ScaleParam* ScaleParam::clone (Component* p) const
{
  ScaleParam* cloned = new ScaleParam(*this);
  cloned->parent(p);
  return cloned;  
}

std::ostream& ScaleParam::put (std::ostream& s) const
{
  using namespace std;
  ios_base::fmtflags saveFlags(s.flags());
  streamsize savePrec(s.precision());
  const Component* component = parent();
  s  << setw(4) << index()   
     << setw(5) << component->index() << "   "
     << setw(11) << left << component->name() 
     << setw(11) << left << name()
     << setw(9) << left << "(scale)"; 
  Real absVal = fabs(value());
  if (absVal > 0)
  {
     int exp = static_cast<int>(floor(log10(absVal)));
     if (exp >= -1 && exp <= 3)
     {
        // Want 6 significant digits, prec gives number
        // of digits after decimal.
        int prec = 5 - exp;
        s << fixed << showpoint << setprecision(prec);
     }
     else
     {
        s << scientific << uppercase << setprecision(5);
     } 
  }
  else
  {
     // Just print 0.0
     s << fixed << showpoint << setprecision(1);
  }  
  s << left << setw(13) << value();

  if (isLinked())
  {
        thisLink()->putLink(s);  
  } 
  s.precision(savePrec);
  s.flags(saveFlags);

  return s;
}

bool ScaleParam::compare (const Parameter& right) const
{
  if (!Parameter::compare(right)) return false; 
  const ScaleParam& that = static_cast<const ScaleParam&>(right);
  if (m_factor != that.m_factor) return false;
  return true;
}

void ScaleParam::init (const string& initString)
{
  if (initString.length())
  {
     std::istringstream s(initString);
     string parname;
     string unitLabel;
     Real factor=0.0;
     s >> parname;
     name(parname.substr(1,parname.length()-1));
     s >> unitLabel;
     if (unitLabel.length() == 1 && unitLabel[0] == '"')
     {
        // If simply a single ", this should be first of a
        // pair of " " enclosing a space, indicating no
        // unit label.
        s >> unitLabel;
        unitLabel.erase();
     }
     s >> factor;
     if (!s)
     {
        string errMsg("Syntax error for scale parameter ");
        errMsg += name() + " in model description file.\n";
        throw YellowAlert(errMsg);
     }
     setValue(factor);
  }
}

void ScaleParam::changeValue (const string& parString)
{
  using std::ios_base;

  // remove the link if any and reset the parameter to its file values
  // without preserving current setting
  untie(false); 

  int begin = parString.find_first_not_of(" \t");
  std::istringstream s(parString.substr(begin));
  s.exceptions(ios_base::badbit | ios_base::failbit | ios_base::eofbit );
  string errMsg = "Parameter "+ name() + ": floating point value required ";
  Real currentValue = value();
  Real newValue;
  try 
  {               
        s >> newValue;
  }
  catch  ( ios_base::failure & )
  {        
            if (s.eof() )  
            {
                // do nothing. continue
            }
            else  if (s.fail())
            {
                            throw Parameter::InvalidInput(errMsg);
            }    
            else 
            {
                    if (s.bad()) 
                    {
                            throw RedAlert(" reading parameter values\n");
                    }
            }
  }  

  if ( std::abs(currentValue - newValue) > std::numeric_limits<float>::min() ) 
  {
        setValue(newValue);   
  }    
  else changed(false);
}

Real ScaleParam::value () const
{
  if (!isLinked()) return m_factor;
  else return thisLink()->linkValue();
}

void ScaleParam::setValue (Real& val)
{
  m_factor = val;
  changed(true);
  setCompute(true);
  setComputeForLinks(true);
}

void ScaleParam::doPreserve ()
{
  if (isLinked())
  {
        Real currentValue (thisLink()->linkValue());
        links()->unlink(this);
        reset();
        m_factor = currentValue;       
  }  
}

string ScaleParam::stringVal () const
{
  std::ostringstream val("");
  val << m_factor << " " << name();
  return val.str();
}

string ScaleParam::parameterSetting () const
{
  std::ostringstream val;

  val.precision(6);
  if (!isLinked())
  {
        val  << std::setw(15) << m_factor;
  }
  else
  {
        val << "= " << thisLink()->linkExpression(false);
  }
  return val.str();  
}

// Additional Declarations
