//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSModel/Model/Component/Component.h>

// ModParam
#include <XSModel/Parameter/ModParam.h>
// SwitchParam
#include <XSModel/Parameter/SwitchParam.h>
// ScaleParam
#include <XSModel/Parameter/ScaleParam.h>
// ParamCreator
#include <XSModel/Parameter/ParamCreator.h>


// Class Utility ParamCreator 

Parameter* ParamCreator::MakeParameter (std::string initString, Component* p, size_t paramIndex)
{
  Parameter* q;
  char indicator(initString[0]);
  switch (indicator)
  {
        case '$':
           q = new SwitchParam(initString,p); 
           break;
        case '*': 
           q = new ScaleParam(initString,p); 
           break;
        default: 
           {
              string parName, unit;
              Real val=.0,delta=.0,high=.0,low=.0,top=.0,bot=.0;
              bool isPeriodic = false;
              ModParam::parseModParamString(initString, parName, &val, &delta,
                        &high, &low, &top, &bot, unit, &isPeriodic); 
              q = new ModParam(parName,p,val,delta,high,low,top,bot,
                        unit,isPeriodic);
           }
           break;         
  }
  q->index(paramIndex);
  return q;
}

// Additional Declarations
