//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// ModelContainer
#include <XSModel/GlobalContainer/ModelContainer.h>
// SwitchParam
#include <XSModel/Parameter/SwitchParam.h>
// ScaleParam
#include <XSModel/Parameter/ScaleParam.h>
// XSutility
#include <XSUtil/Utils/XSutility.h>
// ModParam
#include <XSModel/Parameter/ModParam.h>
// ParameterLink
#include <XSModel/Parameter/ParameterLink.h>

#include <XSUtil/Parse/XSparse.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/ModelBase.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Model/Component/ComponentGroup.h>
#include <XSModel/Parameter/Parameter.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUtil/Utils/XSstream.h>
#include <cctype>
#include <sstream>
using std::endl;


// Class ParameterLink::InvalidInput 

ParameterLink::InvalidInput::InvalidInput (const string& msg)
  : YellowAlert("Invalid link expression: ")
{
   tcerr << msg << endl;
}


// Class ParameterLink::ParseFailure 

ParameterLink::ParseFailure::ParseFailure (const string& msg)
  : RedAlert(msg)
{
}


// Class ParameterLink::DivideCheck 

ParameterLink::DivideCheck::DivideCheck (const string& msg)
        : YellowAlert(" Link expression gives divide by zero error: ")
{
   tcerr << msg << endl;
}


// Class ParameterLink::NoValidParametersInExpression 

ParameterLink::NoValidParametersInExpression::NoValidParametersInExpression (const string& msg)
        : YellowAlert("Link expression contains no valid parameters:  = ")
{
  tcerr << msg << endl;
}


// Class ParameterLink 
const int ParameterLink::s_FLAGVALUE = -999;

ParameterLink::ParameterLink(const ParameterLink &right)
      : m_boolean(right.m_boolean), 
        m_linkVal(right.m_linkVal), 
        m_parent(right.m_parent),
        m_members(right.m_members),
        m_linkString(right.m_linkString)
{
}

ParameterLink::ParameterLink (const string& inLinkExpression, const Parameter* p, bool isBoolean)
      : m_boolean(isBoolean),
        m_linkVal(0),
        m_parent(p),     
        m_members(),
        m_linkString(std::make_pair(1.0,2.0),"add")
{
    // first step is to modify linkExpression for backwards-compatibility by 
    // replacing any instances of a parameter specified by # or modelname:# with
    // p# or modelname:p#
    MathExpression testExpr(std::make_pair(1.0,2.0),"add");
    testExpr.init(inLinkExpression);
    const std::vector<AbstractExpression::TokenType>& inTokens = testExpr.tokenList();
    std::vector<AbstractExpression::TokenType> outTokens;
    const ResponseParam* respPar = dynamic_cast<const ResponseParam*>(m_parent);
    bool atLeastOneFound = false;
    for (size_t i=0; i<inTokens.size(); i++) {
      AbstractExpression::TokenType currentToken(inTokens[i]);

      // have to do some special processing to catch cases of #.#e+# or #.#e-# which
      // will have been incorrectly split up into three tokens #.#e + and #. For the
      // positive exponent case we can just remove the + and concatenate back together
      // again. For the - case we need to replace #.#e-# with (#.#/1.0e#).

      string tokstr = currentToken.tokenString;
      size_t lentok = currentToken.tokenString.size();
      std::istringstream iss(tokstr.substr(0,lentok-1));
      Real rtest;
      iss >> rtest;
      if ( (tokstr[lentok-1] == 'e' || tokstr[lentok-1] == 'E') && 
	   iss.rdbuf()->in_avail() == 0 && i < inTokens.size()-2 ) {
	if ( inTokens[i+1].tokenString == "+" ) {
	  currentToken.tokenString += inTokens[i+2].tokenString;
	  i += 2;
	} else if ( inTokens[i+1].tokenString == "-" ) {
	  currentToken.tokenString = "(" + tokstr.substr(0,lentok-1) + "/1.0e" + 
	                             inTokens[i+2].tokenString + ")";
	  i += 2;
	}
      }

      // Now check whether any token flagged as a WordExp is a parameter - if so
      // make sure it is in the correct format and add it to the internal database.

      if ( currentToken.type == AbstractExpression::WordExp ) {
	string testStr = currentToken.tokenString;
	bool isPar = respPar ? wordToResponseParam(testStr) :
                               wordToParam(testStr);
	if ( isPar ) {
	  atLeastOneFound = true;
	  size_t icolon = testStr.find(":");
	  if ( testStr.substr(0,1) != "p" && testStr.substr(0,1) != "P" &&
	       icolon == string::npos ) {
	    currentToken.tokenString = "p" + testStr;
	  } else if ( icolon != string::npos && 
		      testStr.substr(icolon+1,1) != "p" &&
		      testStr.substr(icolon+1,1) != "P" ) {
	    currentToken.tokenString = testStr.substr(0,icolon+1) + "p" 
	      + testStr.substr(icolon+1);
	  }
	}	  
      }

      outTokens.push_back(currentToken);
    }

    string linkExpression;
    for (size_t i=0; i<outTokens.size(); i++) {
      linkExpression += outTokens[i].tokenString;
    }

    if (!atLeastOneFound)
      throw NoValidParametersInExpression(linkExpression);

    m_linkString.init(linkExpression);
    if (isBoolean && m_linkString.tokenList().size() > 1)  
    {
       throw ParameterLink::InvalidInput("Switches may only take links of type par x = par y");
    }
}

ParameterLink::~ParameterLink()
{
  // manages no heap memory: no owning pointers, just stack allocated
  // MathExpression instance.
}


ParameterLink & ParameterLink::operator=(const ParameterLink &right)
{
  ParameterLink __temp(right);
  swap(__temp);
  return *this;
}


void ParameterLink::swap (ParameterLink& right)
{
    XSutility::swap(m_linkString,right.m_linkString);
    XSutility::swap(m_parent,right.m_parent);
    XSutility::swap(m_boolean,right.m_boolean);
    XSutility::swap(m_members,right.m_members);  
}

bool ParameterLink::wordToParam (const string& wordIn)
{
  // "words" that evaluate to parameters are strings of the form:
  // 1) p# or # - where # are integers [no decimal point or exponent] as per XSPEC11
  // 2) modelName:p# or modelName:# - same but with a model name qualifier. Without,
  //    it refers to the default model.

  string word;
  size_t icolon = wordIn.find(":");
  if ( icolon == string::npos ) {
    if ( wordIn.substr(0,1) == "p" || wordIn.substr(0,1) == "P" ) {
      word = wordIn.substr(1);
    } else {
      word = wordIn;
    }
  } else {
    if ( wordIn.substr(icolon+1,1) == "p" || wordIn.substr(icolon+1,1) == "P" ) {
      word = wordIn.substr(0,icolon+1) + wordIn.substr(icolon+2);
    } else {
      word = wordIn;
    }
  }

  bool isAdded = false;
  string modelName;
  size_t parNum = XSutility::isInteger(word);

  if (parNum != string::npos )
  {             
     // If it can't find a parameter named with only an integer,
     // we'll let it pass.  The MathExpression class will treat
     // it as a numerical constant.  But if it can't find a parameter
     // when the string was originally "p<n>", it is an error.
     isAdded = addPointer(parNum);
     if (!isAdded && word != wordIn)
     {
        std::ostringstream msg;
        msg << "No matching parameter for parameter reference " << wordIn
           << " in link expression.";
        throw ParameterLink::InvalidInput(msg.str());
     }
  }
  else
  {
     // If it is not an integer the other possibility for a parameter is to be a
     // string::int pairing.
     XSparse::stringIntPair(word,modelName,parNum);

     if (parNum == string::npos)
     {
        return false;
     }
     else
     {
        if (!(isAdded = addPointer(parNum,modelName)))  
        {
           // the only possible response here is to throw an 
           // InvalidInput exception.
           // because a string has been entered that doesn't correspond to a
           // valid model parameter.
           std::ostringstream msg;
           msg << "No matching parameter for parameter reference " << word
              << " in link expression.";
           throw ParameterLink::InvalidInput(msg.str());
        }    
     }
  }
  return isAdded;
}

bool ParameterLink::addPointer (int num, const string& modelName)
{
  bool added(false);
  const Parameter* ptr = 0;

  // First check if link is to a parameter within same model
  // (and NOT a previously existing model with same name in global
  // container that is about to be replaced).
  const Component* cparent = m_parent->parent();
  const ModelBase* modelParent = 0;
  if(cparent)
  {
      modelParent = cparent->parent()->parent();
      if (modelParent && modelName == modelParent->name())
         ptr = modelParent->getLocalParameter(num);
  }

  if (!ptr)       
     ptr = XSContainer::models->lookupParameter(num,modelName);       

  if (ptr == parent())
  {
        throw ParameterLink::InvalidInput(" Attempt to link parameter to itself");   
  }

  // we apparently have a valid parameter, but not all requested links 
  // make sense. 
  if (ptr != 0)
  {
        // disallow some conditions.
        if (boolean() && !dynamic_cast<const SwitchParam*>(ptr))
        {
            // a boolean may not be linked to anything other than a boolean.
            throw ParameterLink::InvalidInput
                    (" Attempt to create link from switch to real valued parameter");
        }    
        else if (!boolean() && dynamic_cast<const SwitchParam*>(ptr))
        {
            // a real-valued parameter (scaling factor or fitting parameter) may not 
            // (at this point) depend on a boolean.
            throw ParameterLink::InvalidInput
                    (" Attempt to link real valued parameter to a switch");
        } 
        else if (dynamic_cast<const ScaleParam*>(parent()))
        {
             // a scaling factor may not be made variable by being dependent on a 
             // fitting parameter (we already filtered out the case of SwitchParam
             // here).
             if (dynamic_cast<const ModParam*>(ptr)) throw ParameterLink::InvalidInput
                    (" Attempt to link constant scaling factor to variable fitting parameter");   
        }

        // if it got past all of that
	m_members.push_back(ptr);
        added = true;
  }
  return added;
}

Real ParameterLink::linkValue () const
{
   // m_members has one entry for each appearance of a parameter in the expression
   // the MathExpression evaluate method requires as input the parameter values for
   // each distinct parameter, ignoring repeats. So, first we make a vector with
   // just the distinct parameter values

   std::vector<Real> distinctParVals;
   for (size_t i=0; i<m_members.size(); ++i) {
     bool found = false;
     for (size_t j=0; j<i; j++) {
       if ( m_members[i] == m_members[j] ) found = true;
     }
     if (!found) distinctParVals.push_back(m_members[i]->value());
   }

   RealArray parVals(distinctParVals.size());

   for (size_t i=0; i<distinctParVals.size(); ++i) {
     parVals[i] = distinctParVals[i];
   }

   RealArray energies(2);
   energies[0] = 1.0;
   energies[1] = 2.0;
   RealArray flux(1), fluxErr(1);
   m_linkString.evaluate(energies, parVals, flux, fluxErr);  

   return flux[0];
}

void ParameterLink::putLink (std::ostream& s) const
{
  string outputExpression(linkExpression(false));  
  s << "= " << outputExpression;
}

ParameterLink* ParameterLink::clone () const
{

  return new ParameterLink(*this);
}

bool ParameterLink::findParam (const Parameter* param) const
{
  if (std::find(m_members.begin(),m_members.end(),param) 
                != m_members.end()) return true;
  return false;
}

string ParameterLink::linkExpression (bool fullname) const
{

  string outputExpression;
  const string SPACE(" ");
  const std::vector<AbstractExpression::TokenType>& tokens = m_linkString.tokenList();
  const std::vector<size_t>& paramLoc = m_linkString.paramTokenIndex();
  size_t nextParLoc = string::npos;
  size_t iParamLoc=0;
  if ( paramLoc.size() != 0 ) nextParLoc = paramLoc[iParamLoc];
  size_t iPar = 0;
  for (size_t t = 0; t < tokens.size(); ++t)
  {
     const AbstractExpression::TokenType& curTok = tokens[t];
     if (t == nextParLoc)
     {
        const Parameter* par = m_members[iPar];
        const ResponseParam* resppar = dynamic_cast<const ResponseParam*>(par);
        if (fullname && !resppar)
        {
           const string parIdent(XSContainer::models->indexToName(par->index(),par->modelName()));
           // Data group number is appended to the end following a '$'.
           // Need to remove it and possibly add it to the front for display.
           string::size_type dgLoc = parIdent.find('$');
           string modifiedParID = parIdent.substr(0,dgLoc);
           modifiedParID = (modifiedParID.find(Model::DEFAULT()) == string::npos ?
                        modifiedParID : modifiedParID.substr(modifiedParID.find(':')+1));
           if ( XSContainer::datasets->numberOfGroups() > 1)
           {
              string dgNum(parIdent.substr(dgLoc+1));
              outputExpression += dgNum;
              outputExpression += string(":");
           }
           outputExpression += modifiedParID;
        }
        else
        {
           if (curTok.tokenString.find(Model::DEFAULT()) == string::npos)
              outputExpression += curTok.tokenString;
           else
           {
              string::size_type pos = curTok.tokenString.find(':') + 1;
              outputExpression += curTok.tokenString.substr(pos);
           }
        }
        ++iParamLoc;
	nextParLoc = string::npos;
	if ( iParamLoc < paramLoc.size() ) nextParLoc = paramLoc[iParamLoc];
        ++iPar;
     }
     else
     {
        bool isPm = false;
        if (curTok.type == AbstractExpression::Plus)
           isPm = true;
        else if (curTok.type == AbstractExpression::Minus)
        {
           isPm = true;
           // But check if this is part of an exponent.
           if (curTok.location > 0)
           {
              char prevChar = m_linkString.exprString()[curTok.location-1];
              if (prevChar == 'e' || prevChar == 'E')
                 isPm = false;
           }
        }
        if (isPm)  outputExpression += SPACE;
        outputExpression += curTok.tokenString;
        if (isPm)  outputExpression += SPACE;
     }  
  }

  // tackle the special case where a number was input as #.#e-#. We had to convert this
  // to (#.#/1.0e#) so to avoid confusing the user unnecessarily we will convert back

  while ( outputExpression.find("/1.0e") != string::npos ) {
    size_t divpos = outputExpression.find("/1.0e");
    size_t lbrackpos = outputExpression.find_last_of("(", divpos);
    size_t rbrackpos = outputExpression.find_first_of(")", divpos);
    size_t epos = outputExpression.find_first_of("e", divpos);
    string number = outputExpression.substr(lbrackpos+1,divpos-lbrackpos-1) + "e-" +
                    outputExpression.substr(epos+1,rbrackpos-epos-1);
    string tmpExpression = outputExpression.substr(0,lbrackpos);
    if ( lbrackpos != 0 ) {
      string prevChar = outputExpression.substr(lbrackpos-1,1);
      if ( prevChar.find_first_of(" */^)(") == string::npos ) tmpExpression += "*";
    }
    tmpExpression += number;
    if ( rbrackpos != outputExpression.size()-1 ) {
      string nextChar = outputExpression.substr(rbrackpos+1,1);
      if ( nextChar.find_first_of(" */^)(") == string::npos ) tmpExpression += "*";
    }
    tmpExpression += outputExpression.substr(rbrackpos+1);
    outputExpression = tmpExpression;
  }

  return outputExpression;
}

void ParameterLink::rerouteLink (const std::vector<Parameter*>& newPars)
{
   if (newPars.size() != m_members.size())
      throw RedAlert("Parameter reroute mismatch.");
   for (size_t i=0; i<newPars.size(); ++i)
      m_members[i] = newPars[i];
   regenerateExpression();
}

void ParameterLink::regenerateExpression ()
{
  // This is for use in the context of an update after broken
  // links have been rerouted.
  // IMPORTANT: This ASSUMES that only the parameters may have changed,
  // and that the number of parameters remains constant.  
  string newExprStr;
  const std::vector<AbstractExpression::TokenType>& tokens = m_linkString.tokenList();
  const std::vector<size_t>& paramLoc = m_linkString.paramTokenIndex();
  size_t nextParLoc = string::npos;
  size_t iParamLoc = 0;
  if ( paramLoc.size() != 0 ) nextParLoc = paramLoc[iParamLoc];

  size_t iPar = 0;
  for (size_t iTok=0; iTok<tokens.size(); ++iTok)
  {
     const AbstractExpression::TokenType& curTok = tokens[iTok];
     if (iTok == nextParLoc)
     {
        if (iPar >= m_members.size())
           throw RedAlert("Parameter retrieval error in ParameterLink::regenerateExpression");
        std::ostringstream oss;
        const Parameter* newPar = m_members[iPar];
        const ResponseParam* newRespPar = dynamic_cast<const ResponseParam*>(newPar);
        if (newRespPar)
           oss << newRespPar->responseParent()->sourceNumber() << ':';
        else if (newPar->modelName() != Model::DEFAULT())
           oss << newPar->modelName() << ':';
        oss << 'p' << newPar->index();
        newExprStr += oss.str();
        ++iParamLoc;
	nextParLoc = string::npos;
	if ( iParamLoc < paramLoc.size() ) nextParLoc = paramLoc[iParamLoc];
        ++iPar;        
     }
     else
        newExprStr += curTok.tokenString;
  }

  MathExpression newExpr(std::make_pair(1.0,2.0),"add");
  newExpr.init(newExprStr);  
  m_linkString = newExpr;
}

bool ParameterLink::wordToResponseParam (const string& wordIn)
{

  // For response parameters:
  //  [<sourceNum>:][p]<parNum>

  // We will ASSUME word contains no negative integers.  They should
  // already have been dealt with by this point.

  string word;
  size_t icolon = wordIn.find(":");
  if ( icolon == string::npos ) {
    if ( wordIn.substr(0,1) == "p" || wordIn.substr(0,1) == "P" ) {
      word = wordIn.substr(1);
    } else {
      word = wordIn;
    }
  } else {
    if ( wordIn.substr(icolon+1,1) == "p" || wordIn.substr(icolon+1,1) == "P" ) {
      word = wordIn.substr(0,icolon+1) + wordIn.substr(icolon+2);
    } else {
      word = wordIn;
    }
  }


  bool isAdded = false;
  int sourceNum = -1;
  int parNum = -1;
  if (XSparse::integerPair(word, sourceNum, parNum))
  {
     bool justOneInt = false;
     if (parNum == -1)
     {
        // If only one integer, apply it to parNum
        parNum = sourceNum;
        sourceNum = 1;
        justOneInt = true;
     }

     // We have sourceNum and parNum.  Does it point to an existing response
     // parameter?  In its current implementation, new Response parameters
     // are created only after any old ones they replace are removed from the
     // global map, which is in stark contrast to the Model parameters'
     // situation.  Therefore we don't need to worry about linking to 
     // something that is about to be removed.  And since there's nothing
     // analagous here to data group copies, anything we link to ought
     // to already be in the global map.

     const Parameter* respPar = 0;
     const XSContainer::RespParContainer& respPars = 
                XSContainer::responses->respParContainer();
     if (sourceNum && static_cast<size_t>(sourceNum) <= respPars.size())
     {
        if (parNum && static_cast<size_t>(parNum) <= respPars[sourceNum-1].size())
        {
           respPar = respPars[sourceNum-1][parNum-1];
        }
     }

     // As with mod params, we'll let slide the case of a missing par and just one
     // int specifier.  Assume they mean it as a numerical constant. (But not
     // if it was originally entered with "p<n>" notation.)
     if (respPar)
     {
        // Make sure it's not linking to itself (at least not directly).
        if (respPar == m_parent)
           throw InvalidInput(" Attempt to link parameter to itself");
        m_members.push_back(respPar);
        isAdded = true;   
     }
     else if (!justOneInt || word != wordIn)
     {
        string err("No response parameter found for specifier ");
        err += wordIn;
        throw InvalidInput(err);
     }

  }

  return isAdded;
}


// Additional Declarations
