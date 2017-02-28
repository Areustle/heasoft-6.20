//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// iostream
#include <iostream>
// typeinfo
#include <typeinfo>
// Error
#include <XSUtil/Error/Error.h>
// ModParam
#include <XSModel/Parameter/ModParam.h>
#include <XSContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Parameter/ParameterLink.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <sstream>
#include <iomanip>
#include <cmath>


// Class ModParam 

ModParam::ModParam(const ModParam &right)
      : Parameter(right), 
        m_unit(right.m_unit),
        m_isFrozen(right.m_isFrozen),
        m_epo(right.m_epo),
        m_emn(right.m_emn),
        m_epe(right.m_epe),
        m_gcc(right.m_gcc),
        m_sigma(right.m_sigma),
        m_priorType(right.m_priorType),
        m_hyperParam(right.m_hyperParam),
        m_lastErrorStatus(right.m_lastErrorStatus),
        m_isPeriodic(right.m_isPeriodic),
        m_values(right.m_values)
{
}

ModParam::ModParam (const string& inputName, Component* p, Real val, Real delta, Real high, Real low, Real top, Real bot, const string& unit, bool isPeriodic)
      : Parameter(p), m_unit(unit),m_isFrozen(false),  m_epo(0.),
        m_emn(0.), m_epe(0.), m_gcc(0.), m_sigma(0.),
        m_priorType(CONS), m_hyperParam(), 
        m_lastErrorStatus("FFFFFFFFF"), m_isPeriodic(isPeriodic),
        m_values(val,delta,high,low,top,bot)
{
  // initialize from input argument list members constructed in the base class.
  name(inputName);
  delta = checkDeltaForFrozen(m_values.delta());
  m_values.delta(delta);
}

ModParam::ModParam (const string& initString)
      : Parameter(0), m_unit(), m_isFrozen(false), m_epo(0.),
        m_emn(0.), m_epe(0.), m_gcc(0.), m_sigma(0.),
        m_priorType(CONS), m_hyperParam(),
        m_lastErrorStatus("FFFFFFFFF"), m_isPeriodic(false),
        m_values()
{
   init(initString);
}


ModParam::~ModParam()
{
}


int ModParam::setValue (Real val, const char key)
{
    int status=0;
    const Real& min = m_values.min();
    const Real& max = m_values.max();
    switch(key)
    {
        case 'a':
//        case 'y':
        case 'z':
           {
//           if ( !(name() == "norm" || key == 'z') || (name() == "norm" && key == 'y') )
              // Also check parent pointer - responseParams have
              // no component parents to set computeForLinks flags.
              if (!(name() == "norm" || key == 'z')  )
              {
                 setCompute( true );
                 if (parent())
                    setComputeForLinks(true);
              }
              const Real& top = m_values.top();
              const Real& bot = m_values.bot();
              // For periodic pars, pay no attention to soft limit settings.
              // Treat the same way as for case 'v'.
              if (m_isPeriodic)
              {
                 m_values.value(applyPeriodicity(val));
              }
              else if ( (val <= top) && (val >= bot) )  
              {
                 m_values.value(val);
              }
              else
              {
                 if (val < bot ) 
                 {
                    if (min >= bot )
                    {
                       m_values.value(bot);
                       status = -1;
                    }
                    else  
                    {
                       m_values.value((1./M_PI_2)*atan(bot - val)
                                               *(min - bot) + bot);
                       status = -2;
                    }
                 }                
                 else
                 {
                    if (val > top)
                    {
                       if (max <= top)
                       {
                          m_values.value(top);
                          status = -1;
                       }
                       else 
                       {
                          m_values.value((1./M_PI_2)*atan(val - top)*
                                          (max - top) + top);
                          status = -2;
                       }                               
                    }
                 }
              }
           }
           break;
        case 'v':
           {
               if ( name() != "norm" )
               {
                  setCompute( true );
                  if (parent())
                     setComputeForLinks(true);
               }
	       status = 1;
               if (m_isPeriodic) {
		 m_values.value(applyPeriodicity(val));
	       } else {
		 if ( val <= max && val >= min ) {
		   m_values.value(val);
		 } else {
		   if ( val < min ) m_values.value(min);
		   if ( val > max ) m_values.value(max);
		   status = -1;
		 }
	       }

           } 
           break;
        case 'l': m_values.min(val); break;
        case 'b': m_values.bot(val); break;
        case 't': m_values.top(val); break;
        case 'h': m_values.max(val); break;
        case 'd': m_values.delta(val); break;
        case 's': m_sigma = val; break;
	case 'p': m_epe = val; break;
	case 'm': m_emn = val; break;
	case 'r': m_epo = val; break;
	case 'g': m_gcc = val; break;
        default: status = 2; break;
    }
    return status;
}

bool ModParam::compare (const Parameter& right) const
{
  if (!Parameter::compare(right)) return false; 
  const ModParam& that = static_cast<const ModParam&>(right);
  if (m_values != that.m_values) return false;
  if (m_unit != that.m_unit ) return false;
  if (m_isFrozen != that.m_isFrozen) return false;
  if (m_epo != that.m_epo) return false;
  if (m_emn != that.m_emn) return false;
  if (m_epe != that.m_epe) return false;
  if (m_gcc != that.m_gcc) return false;
  if (m_sigma != that.m_sigma) return false;
  return true;
}

ModParam* ModParam::clone (Component* p) const
{
  ModParam* cloned = new ModParam(*this);
  // if p=0, clone will have same parent as this.
  if (p)
  {
     cloned->parent(p);
  }
  return cloned;
}

std::ostream& ModParam::put (std::ostream& s) const
{
  using namespace std;
  ios_base::fmtflags saveFlags(s.flags());
  streamsize savePrec(s.precision());

  // Need to check parent pointer since ResponseParam may use this
  if (parent())
  {
     const Component* component = parent();
     s  << setw(4) << index()   
        << setw(5) << component->index() << "   "
        << setw(11) << left << component->name() 
        << setw(11) << left << name()
        << setw(9) << left << m_unit; 
  }
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

  // Print the uncertainty         
  if (!isLinked())
  {
     if (m_isFrozen)
        s << "frozen";
     else
     {
        Real absVal = fabs(value('s'));
        if (absVal > 0)
        {
           int exp = static_cast<int>(floor(log10(absVal)));
           if (exp >= -1 && exp <= 3)
           {
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
        s << "+/-  " << left << setw(13) << value('s');
     }
  }
  else
  {
        thisLink()->putLink(s);  
  } 
  s.precision(savePrec);
  s.flags(saveFlags);

  return s;
}

void ModParam::changeValue (const string& parString)
{
  bool isOK = processValues(parString);
  while (!isOK)
  {
     string newParString;
     rePrompt(newParString);
     isOK = processValues(newParString);
  }
}

void ModParam::freeze ()
{
  if (!isLinked())
  {
        m_isFrozen = true;       
  }
  else 
  {
     // Throw silently, message constructed higher up.
     throw YellowAlert();
  }
}

bool ModParam::limitsInvalid ()
{
  if ( value('b') < value('l') ) return true;
  if ( value('t') > value('h') ) return true;
  if ( value('l') > value('h') ) return true;
  return false;  
}

Real ModParam::value () const
{
  if (!isLinked()) return m_values.value();
  else return thisLink()->linkValue();  
}

Real ModParam::adjustedValue () const
{
  const Real& val = m_values.value();
  const Real& top = m_values.top();
  const Real& bot = m_values.bot();
  const Real& min = m_values.min();
  const Real& max = m_values.max();
  Real result (val);
  const Real EPS = 1.0e-15;
  // If periodic, soft limits are irrelevant
  if (!m_isPeriodic)
  {
     if (val < bot ) 
     {
       if (bot > min ) result = bot - tan(M_PI_2*(val - bot + EPS)/(min - bot));
     }
     else if (val >=top)
     {
        if (top < max ) result = top + tan(M_PI_2*(val - top - EPS)/(max - top));
     }
  }
  return result;
}

Real ModParam::value (const char key) const
{
    switch (key)
    {
  	case 't': return m_values.top(); 
  	case 'b': return m_values.bot(); 
  	case 'h': return m_values.max(); 
  	case 'l': return m_values.min(); 
  	case 'd':
           { 
              Real propDelta = XSContainer::models->proportionalDelta();
              if (propDelta > 0.0 && value() != 0.0)
                 return std::abs(propDelta*value());

              return m_values.delta(); 
           }
        case 's': return m_sigma; 
        case 'm': return m_emn; 
        case 'p': return m_epe;
        case 'g': return m_gcc; 
        case 'r': return m_epo; 
        case 'a': return adjustedValue();
        default: 
  	case 'v': 
                return value(); 
    }
}

void ModParam::doPreserve ()
{
  if (isLinked())
  {
     // Preserve the value of the link expression.
     // Also preserve the isFrozen state: isFrozen = false unless
     // every parameter in the link statement is frozen ( or is
     // tied to another which is frozen).
     Real currentValue (thisLink()->linkValue());
     bool isFrozen = Parameter::isLinkedToFrozen(this);
     links()->unlink(this);
     reset();
     setValue(currentValue,'v'); 
     m_isFrozen = isFrozen;      
  }
}

void ModParam::reset () throw (Parameter::ResetFailure)
{
  if (name() == "norm")
  {
        // these are the default values assigned to a norm on
        // construction by Component::addParam. It would be better
        // if the constants passed here were global symbols, but...
        setValue(1,'v');
        setValue(0.01,'d');
        setValue(1.E+20,'t');
        setValue(1.E+24,'h');
        setValue(0,'b');
        setValue(0,'l');

  }      
  else
  {
        Parameter::reset();       
  }      
}

string ModParam::stringVal () const
{
  std::ostringstream val;
   // In case of frozen parameter, report this to user as a negative
   // delta value (though actual delta is still pos).
  Real delta = m_isFrozen ? -m_values.delta() : m_values.delta();
  // hack to avoid awkward printing of "-0"
  if (fabs(delta) < SMALL) delta = .0;
  val << value('v') << " " << delta << " " << value('l') << " "
      << value('b') << " " << value('t') << " " << value('h') << " " 
      << sigma() << " " << name()  << " " << unit();
  return val.str();
}

string ModParam::parameterSetting () const
{
   using namespace std;

   ostringstream val;
   val.precision(6);

   // In case of frozen parameter, report this to user as a negative
   // delta value (though actual delta is still pos).
   Real delta = m_isFrozen ? -m_values.delta() : m_values.delta();
   Real propDelta = XSContainer::models->proportionalDelta();
  // hack to avoid awkward printing of "-0"
  if (fabs(delta) < SMALL) delta = .0;
   if ( !isLinked())
   {
       val << setw(15) << value('v') << ' ' << setw(10) << delta;
       if (propDelta > .0)
       {
          val << "(" << setw(10) << value('d') << ")";
       }       
       val << ' ' 
           << setw(10) << value('l') << ' ' << setw(10) << value('b') << ' ' 
           << setw(10) << value('t') << ' ' << setw(10) << value('h');
   }
   else
   {
       val << "= " << thisLink()->linkExpression(false);
   }

   return val.str();
}

bool ModParam::processValues (const string& parString)
{
  bool isOK = true;

  // This function distinguishes between 2 seperate types of invalid input. 
  // If the input format is wrong, ie. entering gibberish where Reals are
  // expected, it resets ALL values to their previous state and throws an
  // InvalidInput.  If it passes this test but the parameter value is
  // out of the hard limit bounds, the other values retain their new state,
  // function returns false, and the user is prompted again.

  using namespace std;
  static const string doc[4] = {"3rd (lower bound)", "4th (soft lower limit)",
                          "5th (soft upper limit)", "6th (upper bound)"};
  static const char lims[4] = {'l','b','t','h'};
  static const size_t maxN = 6;

  // remove the link if any and reset the parameter to its file values
  // without preserving current setting
  untie(false); 
  changed(false);

  StringArray rawArgs, processedArgs;
  IntegerArray iParams;

  // Break parString into separate space-delimited strings   
  // for use in collectParams function. 
  string::size_type begin = parString.find_first_not_of(" \t");
  while (begin != string::npos)
  {
     string::size_type end = parString.find_first_of(" \t",begin);
     string::size_type len = (end == string::npos) ? string::npos :
                                end - begin;
     rawArgs.push_back(parString.substr(begin, len));
     begin = parString.find_first_not_of(" \t",end);
  }
  XSparse::collectParams(rawArgs, iParams, processedArgs);
  // Do a speedy exit if they've accepted defaults.
  if (!iParams.size() || processedArgs[0] == XSparse::SKIP() ||
                processedArgs[0] == XSparse::USE_DEFAULT())
  {
     return true;
  }
  Real currentValue = value('v');
  Real newValue = currentValue;
  Real currentDelta = m_values.delta();
  Real newDelta = .0;
  bool currentFrozen = m_isFrozen;
  Real newLimit = .0;
  RealArray currentLimits(0.,4);
  for (int i=0; i<4; ++i)
  {
     currentLimits[i] = value(lims[i]);
  }

  string errMsg("Parameter ");      
  errMsg += name();
  errMsg += ": ";

  try
  {
     // Ignore any entered arguments beyond maxN.
     size_t nArgs = min(iParams.size(), maxN);
     for (size_t i=0; i<nArgs; ++i)
     {
        istringstream iss(processedArgs[i]);
        switch (iParams[i])
        {
           case 0:
              if (!(iss >> newValue) || !iss.eof())
              {
                 errMsg += " value: ";
                 errMsg += processedArgs[i];
                 throw Parameter::InvalidInput(errMsg);
              }
              // Don't set 'v' just yet.  Need to check if it's
              // consistent with limits further below.
              break;
           case 1:
              if (!(iss >> newDelta) || !iss.eof())
              {
                 errMsg += " delta: ";
                 errMsg += processedArgs[i];
                 throw Parameter::InvalidInput(errMsg);
              }

              if (fabs(newDelta) < SMALL)
              {
                 // User has entered 0 for delta, toggle
                 // the isFrozen flag but don't change
                 // the actual delta value.
                 m_isFrozen = (!m_isFrozen);
              }
              else
              {
                 m_isFrozen  = (newDelta < 0);
                 setValue(fabs(newDelta),'d');
              }
              break;
           default:
              if (iParams[i] > 1 && iParams[i] < 6)
              {
                 int iLim = iParams[i] - 2;
                 if (!(iss >> newLimit) || !iss.eof())
                 {
                    errMsg += doc[iLim] + " field: " + processedArgs[i];
                    throw Parameter::InvalidInput(errMsg);
                 }
                 setValue(newLimit, lims[iLim]);
              }
              break;
        }
     }
  }
  catch (...)
  {
     setValue(currentValue,'v');
     setValue(currentDelta,'d');
     m_isFrozen = currentFrozen;
     for (int j=0; j<4; ++j)
     {
        setValue(currentLimits[j], lims[j]);
     }
     throw;
  }

  if (limitsInvalid())
  {
     tcerr << "***Error: bounds setting for parameter ranges.  Limits not changed."
           <<endl;
     for (int j = 0; j < 4; j++)  setValue(currentLimits[j],lims[j]); 
  }
  if (!m_isPeriodic && (newValue > value('h') || newValue < value('l')) )
  {
     tcerr << "***Error: Desired Value " << newValue << " is outside hard range "
             <<  value('l') << " - " << value('h') << std::endl;
     // Reset limits to original values and reprompt.
     for (int j=0; j<4; ++j)  setValue(currentLimits[j], lims[j]);
     isOK = false;   
  }
  else if ( std::abs(newValue - currentValue) > SMALL )
  {
          setValue(newValue,'v');   
  }   

  return isOK;
}

void ModParam::rePrompt (string& newString) const
{
  using namespace std;
  tcout << "\n Re-enter parameter value, delta, min, bot, top, and max values for ...\n"
        << parameterSetting() <<endl;

  ostringstream parPrompt;
  const Component* cparent = parent();
  if (cparent)
  {
     const string& modName = cparent->parentName();
     const string& compName = cparent->name();
     if (modName != Model::DEFAULT())
     {
        parPrompt << modName << ':';
     }
     if (XSContainer::datasets->numberOfGroups() > 1)
     {
        parPrompt << " data group " << cparent->dataGroup() << ':';
     }
     if (parPrompt.str().length())
     {
        parPrompt << ':';
     }
     parPrompt << compName << ':'; 
  }
  parPrompt << name() << '>';
  XSstream::setPrompter(tcin, parPrompt.str());
  string rawFromUser("");
  getline(tcin, rawFromUser);
  string::size_type first = rawFromUser.find_first_not_of(" \t\r");
  // strip leading blanks
  if (first != string::npos)
  {
     newString = rawFromUser.substr(first);
     // In this context, a "/*" is equivalen to "" since this
     // only applies to 1 parameter.
     if (newString == XSparse::SKIP())
     {
        newString.clear();
     }
  }
  else
  {
     newString.clear();
  }
}

Real ModParam::checkDeltaForFrozen (Real delta)
{
   // Note that this function assumes parameter name has already been
   // set, which is needed for its warning message.
   if (fabs(delta) < SMALL)
   {
	 tcerr << "*** Warning: default value of delta for parameter: " 
	 << name() << " must be non-zero, reset to 0.001 (frozen) \n";
	 delta = 0.001;
         m_isFrozen = true;
   }
   else if (delta < .0)
   {
      m_isFrozen = true;
      delta *= -1.0;
   }
   return delta;
}

void ModParam::parseModParamString (const string& fullLine, string& parName, Real* pVal, Real* pDelta, Real* pHigh, Real* pLow, Real* pTop, Real* pBot, string& unit, bool* pIsPeriodic)
{
  if (fullLine.size() > 0 )
  {
     const string WS(" \t\r\n");
     string::size_type startPos = fullLine.find_first_not_of(WS);
     if (startPos == string::npos)
        throw InvalidInput("empty parameter line");
     string::size_type endPos = fullLine.find_first_of(WS,startPos+1);
     if (endPos == string::npos)
        throw InvalidInput(fullLine);
     parName = fullLine.substr(startPos, endPos-startPos);

     startPos = fullLine.find_first_not_of(WS,endPos);
     if (startPos == string::npos)
        throw InvalidInput(parName);

     string::size_type beginVals = string::npos;
     if (fullLine[startPos] == '"')
     {
        string::size_type endQuote = fullLine.find_first_of('"',startPos+1);
        if (endQuote == string::npos)
           throw InvalidInput(parName);
        unit = fullLine.substr(startPos+1,endQuote-(startPos+1));
        // If unit string is nothing but space, erase it entirely.
        if (unit.find_first_not_of(WS) == string::npos)
           unit.erase();
        beginVals = endQuote+1;
     }
     else
     {
        string::size_type endUnit = fullLine.find_first_of(WS,startPos+1);
        if (endUnit == string::npos)
           throw InvalidInput(parName);
        unit = fullLine.substr(startPos,endUnit-startPos);
        beginVals = endUnit;
     }

     std::istringstream initString(fullLine.substr(beginVals));
     initString >> *pVal >> *pLow >> *pBot >> *pTop >> *pHigh >> *pDelta;
     if (!initString)
        throw InvalidInput(parName);

     // This is an optional arg at end of line.
     string testPeriodic;
     initString >> testPeriodic;
     if (testPeriodic.length() && 
        (testPeriodic[0] == 'p' || testPeriodic[0] == 'P'))
     {
        *pIsPeriodic = true;
     }
     else
        *pIsPeriodic = false;
  }
}

void ModParam::init (const string& initStr)
{
   // m_isPeriodic and m_unit are consts, so these variables 
   // won't be reset after string is parsed.
   string parName, dummyUnit;
   Real val=.0, delta=.0, high=.0, low=.0, top=.0, bot=.0;
   bool dummyIsPeriodic=false;
   parseModParamString(initStr, parName, &val, &delta, &high, &low,
                &top, &bot, dummyUnit, &dummyIsPeriodic);
   name(parName);
   delta = checkDeltaForFrozen(delta);
   m_values.init(val, delta, high, low, top, bot);
}

Real ModParam::applyPeriodicity (Real value) const
{
  // ASSUME already checked isPeriodic prior to calling this.
  Real range = m_values.max() - m_values.min();
  if (range == 0.0)
  {
     throw Parameter::InvalidInput("Period range = 0.0 for periodic parameter.");
  }
  if (value > m_values.max())
  {
     // Enforce f(max+delta) = f(min+delta)
     value = m_values.min() + std::fmod(value-m_values.min(), range);
  }
  else if (value < m_values.min())
  {
     // f(min-delta) = f(max-delta)
     value = std::fmod(value-m_values.max(), range) + m_values.max();
  } 
  return value;
}

string ModParam::getParameterLabel () const
{
   std::ostringstream oss;
   if (modelName().length() && modelName() != Model::DEFAULT())
      oss << modelName() << ":";
   oss << index();
   return oss.str();
}

// Additional Declarations
