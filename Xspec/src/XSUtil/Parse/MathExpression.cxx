//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Numerics/MathOperator.h>
#include <XSUtil/Utils/IosHolder.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Utils/XSutility.h>
#include <cctype>
#include <cmath>
#include <stack>
#include <utility>

// MathExpression
#include <XSUtil/Parse/MathExpression.h>


// Class MathExpression::MathExpressionError 

MathExpression::MathExpressionError::MathExpressionError (const string& errMsg)
   :YellowAlert("\nMath Expression Error: ")
{
  *IosHolder::errHolder() << errMsg << std::endl;
}


// Class MathExpression 
const string MathExpression::s_allValidChars = string("_#,.+-/*^():abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \t\r\n");
MathExpression::MathOpContainer MathExpression::s_operatorsMap;
std::map<string,int> MathExpression::s_precedenceMap;

MathExpression::MathExpression(const MathExpression &right)
   : AbstractExpression(right),
     m_distinctParNames(right.m_distinctParNames),
     m_paramsToGet(right.m_paramsToGet),
     m_paramTokenIndex(right.m_paramTokenIndex),
     m_numericalConsts(right.m_numericalConsts),
     m_mathOperators(right.m_mathOperators), 
     m_postfixElems(right.m_postfixElems),
     m_infixElems(right.m_infixElems),
     m_eLow(right.m_eLow),
     m_eHigh(right.m_eHigh),
     m_compType(right.m_compType)
{
   if (s_operatorsMap.empty())
      buildOperatorsMap();
}

MathExpression::MathExpression (std::pair<Real,Real> eLimits, const string& compType)
   : AbstractExpression(),
     m_distinctParNames(),
     m_paramsToGet(),
     m_paramTokenIndex(),
     m_numericalConsts(),
     m_mathOperators(), 
     m_postfixElems(),
     m_infixElems(),
     m_eLow(eLimits.first),
     m_eHigh(eLimits.second),
     m_compType(compType)
{
   if (s_operatorsMap.empty())
      buildOperatorsMap();
}


MathExpression::~MathExpression()
{
}


MathExpression & MathExpression::operator=(const MathExpression &right)
{
   if (this != &right)
   {
      MathExpression tmp(right);
      Swap(tmp);
   }
   return *this;
}


void MathExpression::init (const string& exprString, bool removeWhitespace)
{
   AbstractExpression::init(exprString, removeWhitespace);
   convertToInfix();
   convertToPostfix();
}

void MathExpression::Swap (MathExpression& right)
{
   AbstractExpression::Swap(right);
   std::swap(m_distinctParNames,right.m_distinctParNames);
   std::swap(m_numericalConsts,right.m_numericalConsts);
   std::swap(m_postfixElems,right.m_postfixElems);
   std::swap(m_infixElems,right.m_infixElems);
   std::swap(m_mathOperators,right.m_mathOperators);
   std::swap(m_paramsToGet,right.m_paramsToGet);
   std::swap(m_paramTokenIndex,right.m_paramTokenIndex);
   std::swap(m_eLow,right.m_eLow);
   std::swap(m_eHigh,right.m_eHigh);
   std::swap(m_compType,right.m_compType);
}

MathExpression* MathExpression::clone () const
{
   return new MathExpression(*this);
}

const string& MathExpression::allValidChars () const
{
   return s_allValidChars;
}

void MathExpression::buildOperatorsMap ()
{
   using namespace Numerics;
   clearOperatorsMap();
   // s_operatorsMap will own the memory of these objects throughout
   // the program's lifetime.
   s_operatorsMap["+"] = new PlusOp();
   s_operatorsMap["-"] = new MinusOp();
   s_operatorsMap["*"] = new MultOp();
   s_operatorsMap["/"] = new DivideOp();
   s_operatorsMap["^"] = new PowOp();
   s_operatorsMap["max"] = new MaxOp();
   s_operatorsMap["min"] = new MinOp();
   s_operatorsMap["@"] = new UnaryMinusOp();
   s_operatorsMap["exp"] = new ExpOp();
   s_operatorsMap["sin"] = new SinOp();
   s_operatorsMap["sind"] = new SinDOp();
   s_operatorsMap["cos"] = new CosOp();
   s_operatorsMap["cosd"] = new CosDOp();
   s_operatorsMap["tan"] = new TanOp();
   s_operatorsMap["tand"] = new TanDOp();
   s_operatorsMap["log"] = new LogOp();
   s_operatorsMap["ln"] = new LnOp();
   s_operatorsMap["sqrt"] = new SqrtOp();
   s_operatorsMap["abs"] = new AbsOp();
   s_operatorsMap["int"] = new IntOp();
   s_operatorsMap["asin"] = new ASinOp();
   s_operatorsMap["acos"] = new ACosOp();
   s_operatorsMap["mean"] = new MeanOp();
   s_operatorsMap["dim"] = new DimOp();
   s_operatorsMap["smin"] = new SMinOp();
   s_operatorsMap["smax"] = new SMaxOp();

   s_precedenceMap["+"] = 0;
   s_precedenceMap["-"] = 0;
   s_precedenceMap["@"] = 0;
   s_precedenceMap["*"] = 1;
   s_precedenceMap["/"] = 1;
   s_precedenceMap["^"] = 2;
}

void MathExpression::clearOperatorsMap ()
{
   MathOpContainer::iterator itOp = s_operatorsMap.begin();
   MathOpContainer::iterator itOpEnd = s_operatorsMap.end();
   while (itOp != itOpEnd)   
   {
      delete itOp->second;
      ++itOp;
   }
   s_operatorsMap.clear();
   s_precedenceMap.clear();
}

void MathExpression::convertToInfix ()
{
   // Basically convert tokens enumerated by AbstractExpression's Token
   // types into MathExpression's ElementTypes enumerators, which are more
   // useful for analyzing equations.
   std::vector<size_t> nonNumberTokens;
   findTheNumbers(nonNumberTokens,m_numericalConsts);
   int prevIdx = -1;
   int idx = -2;
   size_t numCount = 0;
   m_paramTokenIndex.clear();
   for (size_t i=0; i<nonNumberTokens.size(); ++i)
   {
      idx = static_cast<int>(nonNumberTokens[i]);
      ElementType mathType = ENG; // init is irrelevant.
      // Assume any sequence of skipped tokens have been bundled into
      // one and only one number.
      if (idx > prevIdx+1)
      {
         m_infixElems.push_back(NUM);
         mathType = NUM;
         ++numCount;
      }
      const TokenType& curTok = tokenList()[idx];
      switch (curTok.type)
      {
         case WordExp:
            mathType = classifyWords(curTok.tokenString);
	    if ( mathType == PARAM ) {
	      m_paramTokenIndex.push_back(idx);
	    }
            break;
         case Lbrace:
            // Conditions for implied '*': '(' is preceded by
            // ')' OR Eng OR param name OR NUM.
            if (m_infixElems.size())
            {
               ElementType prev = m_infixElems[m_infixElems.size()-1];
               if (prev == RPAREN || prev == ENG || prev == PARAM 
                        || prev == NUM || prev == ENGC)
               {
                  m_infixElems.push_back(OPER);
                  m_mathOperators.push_back(s_operatorsMap.find("*"));
               }
            }
            mathType = LPAREN;
            break;
         case Rbrace:
            // This may also include an implied '*', which is
            // inserted after exiting the switch block.
            mathType = RPAREN;
            break;
         case Plus:
            mathType = OPER;
            m_mathOperators.push_back(s_operatorsMap.find("+"));
            break;
         case Minus:
            mathType = OPER;
            // Conditions for unary: First token OR preceded by
            // a *,/,(, or Comma.
            if (idx == 0)
               m_mathOperators.push_back(s_operatorsMap.find("@"));
            else
            {
               Token preType = tokenList()[idx-1].type;
               if (preType == Lbrace || preType == Star || 
                        preType == Slash || preType == Comma)
                  m_mathOperators.push_back(s_operatorsMap.find("@"));
               else
                  m_mathOperators.push_back(s_operatorsMap.find("-"));
            }
            break;
         case Star:
            mathType = OPER;
            m_mathOperators.push_back(s_operatorsMap.find("*"));
            break;
         case Slash:
            mathType = OPER;
            m_mathOperators.push_back(s_operatorsMap.find("/"));
            break;
         case Exp:
            mathType = OPER;
            m_mathOperators.push_back(s_operatorsMap.find("^"));
            break;
         case Comma:
            mathType = COMMA;
            break;
         default:
            {
               string errMsg("Unrecognized symbol during infix parsing: ");
               errMsg += curTok.tokenString;
               throw AbstractExpressionError(errMsg);
            }
            break;
      }
      m_infixElems.push_back(mathType);
      // Conditions for implied '*' after ')': ')' is followed by
      // a WordExp of any kind.  
      if (mathType == RPAREN && static_cast<int>(tokenList().size()) > idx+1)
      {
         if (tokenList()[idx+1].type == WordExp)
         {
            m_infixElems.push_back(OPER);
            m_mathOperators.push_back(s_operatorsMap.find("*"));
         }
      }
      prevIdx = idx;
   } // End non-NUM token loop
   // idx may be negative here, so we don't want to cast it into a size_t.
   if (idx+1 < static_cast<int>(tokenList().size()))
   {
      // Assume the last token must be a number.  (There should never
      // be more than one remaining number, a condition which should 
      // already have been filtered out by AbstractExpression parsing.)
      m_infixElems.push_back(NUM);
      ++numCount;
   }
   if (numCount != m_numericalConsts.size())
     throw AbstractExpressionError("Last symbol is not a number");

   verifyInfix();

  XSstream* xscout = dynamic_cast<XSstream*>(IosHolder::outHolder());
  if (xscout)
  {
     *xscout << xsverbose(40) << "Infix elements: ";
     for (size_t i=0; i<m_infixElems.size(); ++i)
        *xscout << m_infixElems[i] << " ";
     *xscout << std::endl<< "Numerical consts: ";
     for (size_t i=0; i<m_numericalConsts.size(); ++i)
        *xscout << m_numericalConsts[i] << " ";
     *xscout << std::endl << "Infix operators: ";
     for (size_t i=0; i<m_mathOperators.size(); ++i)
        *xscout << m_mathOperators[i]->first << " ";
     *xscout << std::endl << xsverbose();
  }
}

void MathExpression::convertToPostfix ()
{
   // This will fill in the m_postfixElems vector and also rearrange m_mathOperators
   // into the order the operators will be called in postfix.  The vectors for
   // pars and numbers need no such reordering since they are called in the
   // same sequence for infix and postfix.
   using namespace std;
   using Numerics::MathOperator;
   // The int in the pair below is for storing operator precedence number.
   stack<pair<int, MathOpContainer::const_iterator> > opStack;
   vector<MathOpContainer::const_iterator> tmpOperators;
   const size_t nElems = m_infixElems.size();
   size_t opPos = 0;
   for (size_t i=0; i<nElems; ++i)
   {
      ElementType curType = m_infixElems[i];
      switch (curType)
      {
         case OPER:
         {
            MathOpContainer::const_iterator curOp(m_mathOperators[opPos]);
            int prec = s_precedenceMap.find(curOp->first)->second;
            if (!opStack.empty() && curOp->first != "^")
            {
               pair<int,MathOpContainer::const_iterator> topOp = opStack.top();
               int testPrec = topOp.first;
               while (testPrec >= prec)
               {
                  m_postfixElems.push_back(OPER);
                  tmpOperators.push_back(topOp.second);
                  opStack.pop();
                  if (opStack.empty())
                     testPrec = -999; // cause immediate exit from loop
                  else
                  {
                     topOp = opStack.top();
                     testPrec = topOp.first;
                  }
               }               
            }
            opStack.push(make_pair(prec, curOp));
            ++opPos;
         }
            break;
         case UFUNC:
         case BFUNC:
            // Unary and binary function calls will be handled the same way.  Treat them 
            // as an LPAREN that also happens to have an actual operator associated with
            // it.  This means giving it a precedence of -1 so that RPAREN thinks it's a 
            // matching LPAREN, BUT also give it the actual function iterator rather 
            // than the end iterator.
            opStack.push(make_pair(-1,m_mathOperators[opPos]));
            ++opPos;
            // Now skip the actual LPAREN that we know is following this.
            ++i;
            break;
         case LPAREN:
            // If in here, this is a stand-alone parenthesis not associated with a 
            // function call.  LPAREN is not an actual operator, but we still need 
            // to store it as a placeholder in the opStack.  Give it a dummy 
            // precedence that's lower than any real operator, and an end iterator.
            opStack.push(make_pair(-1,s_operatorsMap.end()));
            break;
         case RPAREN:
            // Pop the operators stack until we come to the first left parenthesis,
            // which may or may not be tied to a function call.
            {
               int testPrec = 0;
               do 
               {
                  pair<int,MathOpContainer::const_iterator> topOp = opStack.top();
                  testPrec = topOp.first;
                  if (topOp.second != s_operatorsMap.end())
                  {
                    // This could be an LPAREN tied to a function call, if testPrec
                    // = -1.  From this point forward, BFUNC and UFUNC need only be 
                    // classified as an OPER.
                    m_postfixElems.push_back(OPER);
                    tmpOperators.push_back(topOp.second);
                  }
                  opStack.pop();  
               } while (testPrec != -1 && !opStack.empty());
            }
            break;
         case COMMA:
            // Similar to RPAREN case, but do NOT pop the function call LPAREN.
            {
               pair<int,MathOpContainer::const_iterator> topOp = opStack.top();
               int testPrec = topOp.first;
               // We can safely assume the first LPAREN reached is the 
               // crucial function call LPAREN.  Any intervening ones would
               // already have been popped when their RPAREN was processed.
               while (testPrec != -1)
               {
                  m_postfixElems.push_back(OPER);
                  tmpOperators.push_back(topOp.second);
                  opStack.pop();
                  topOp = opStack.top();
                  testPrec = topOp.first;
               }
            }
            break;
         default:
            m_postfixElems.push_back(curType);
            break;
      }
   }

   // Any operators remaining on the stack must now be popped to output.
   while (!opStack.empty())
   {
      m_postfixElems.push_back(OPER);
      tmpOperators.push_back(opStack.top().second);
      opStack.pop();
   }

   // And now the costly copy...
   m_mathOperators = tmpOperators;


  XSstream* xscout = dynamic_cast<XSstream*>(IosHolder::outHolder());
  if (xscout)
  {
     *xscout << xsverbose(40) << "Postfix elements: ";
     for (size_t i=0; i<m_postfixElems.size(); ++i)
        *xscout << m_postfixElems[i] << " ";
     *xscout << std::endl << "Postfix operators: ";
     for (size_t i=0; i<m_mathOperators.size(); ++i)
        *xscout << m_mathOperators[i]->first << " ";
     *xscout << std::endl << xsverbose();
  }
}

MathExpression::ElementType MathExpression::classifyWords (const string& wordStr)
{
   // Can assume wordStr is not a number, those have been taken care of
   // already.  It must either be a function, 'e', 'E', (with optional '.'
   // if convolution) or parameter name (which must start with a letter). 

   ElementType type = COMMA; // Init to something other than ENG/ENGC 
   const bool isCon = (m_compType == string("con")); 
   if (wordStr.size() == 1 && (wordStr[0] == 'e' || wordStr[0] == 'E'))
      type = isCon ? ENGC : ENG;       
   else if (isCon && (wordStr == string(".e") || wordStr == string(".E")))
      type = ENG; 

   if (type != ENG && type != ENGC)
   {  
      // Function match will be case-insensitive, but other than that it
      // must match exactly.
      string lcWord = XSutility::lowerCase(wordStr);
      MathOpContainer::const_iterator itFunc = s_operatorsMap.find(lcWord);
      if (itFunc !=  s_operatorsMap.end() && lcWord == itFunc->first)
      {
         type = (itFunc->second->nArgs() == 1) ? UFUNC : BFUNC;
         m_mathOperators.push_back(itFunc);
      } 
      else
      {
         // At this point, must assume wordStr is a parameter name.
	if (!(isalpha(wordStr[0]) || wordStr[0] == '_' || 
	      wordStr.find(":") != string::npos))
         {
            string errMsg("Illegal parameter name: ");
            errMsg += wordStr;
            throw AbstractExpressionError(errMsg);
         }
         // Determine if this name has already been entered, and if so,
         // what is its index.   This is O(N^2) (ugh), but can't 
         // imagine N > 100.
         size_t idx=0;
         std::vector<string>::const_iterator itName = m_distinctParNames.begin();
         std::vector<string>::const_iterator itNameEnd = m_distinctParNames.end();
         while (itName != itNameEnd)
         {
            if (*itName == wordStr)
               break;
            ++itName, ++idx;
         }
         if (itName == itNameEnd) {
	   m_distinctParNames.push_back(wordStr);
	 }
         m_paramsToGet.push_back(idx);
         type = PARAM;
      } 
   } // end if not ENG/ENGC
   return type;
}

void MathExpression::verifyInfix () const
{
   // AbstractExpression has already verified general-syntax Token
   // ordering, which is really most of the work.  This will perform 
   // some additional checking that is specific to equation requirements:
   // - All function names must immediately be followed by a '('.
   // - Commas can only exist in binary function calls
   // - 1 and only 1 comma must exist in (the top level of) a binary
   //   function call
   // - Comma cannot appear inside any pair of parentheses internal to
   //   the pair specifying the binary function call to which it belongs.

   const size_t nElems = m_infixElems.size();
   size_t funcCounter = 0; // Needed only for output messages.
   // The first rule is easy enough to check with a linear search.
   // While doing this, count up all the commas and binary function
   // calls.  That these be equal is necessary but not sufficient
   // for rules 2 and 3.  But once rule 3 is verified independently,
   // this becomes sufficient for proving rule 2.  Rule 4 is verified
   // internally in verifyBFunc.
   size_t commaCount = 0;
   size_t binaryCount = 0;
   for (size_t i=0; i<nElems; ++i)
   {
      ElementType curType = m_infixElems[i];
      if (curType == UFUNC || curType == BFUNC)
      {
         if (i == nElems-1 || m_infixElems[i+1] != LPAREN)
         {
            string errMsg("A '(' must follow the call to: ");
            errMsg += m_mathOperators[funcCounter]->first;
            throw AbstractExpressionError(errMsg);
         }
         ++funcCounter;
         if (curType == BFUNC)
            ++binaryCount;
      }
      else if (curType == OPER)
         ++funcCounter;
      else if (curType == COMMA)
         ++commaCount;
   }
   if (commaCount > binaryCount)
      throw AbstractExpressionError("Commas can only exist in binary functions, 1 per function.");
   else if (commaCount < binaryCount)
      throw AbstractExpressionError("2 arguments required for binary functions.");

   if (binaryCount)
   {
      size_t idxElem = 0;
      while (idxElem < nElems)
      {
         // When verifyBFunc returns, idxElem will be set to the index
         // of the closing parenthesis to the function for which it was
         // originally called.
         if (m_infixElems[idxElem] == BFUNC)
            verifyBFunc(&idxElem);
         ++idxElem;
      }
   }   
}

void MathExpression::verifyBFunc (size_t* idxElem) const
{
   // This is a recursive function that verifies that one and only
   // one comma is placed in a binary function call.  It also verifies
   // that the comma is not inside nested parentheses.
   // ASSUME idxElem initially is the index of a binary function, and
   // idxElem+1 is the index of an LPAREN.  These things should 
   // already have been verified.
   // When function returns, idxElem will be set to the closing
   // parenthesis of the binary function.
   size_t parCount = 1;
   size_t commasFound = 0;
   size_t idx = *idxElem+1;
   while (parCount)
   {
      ++idx;
      // Can also assume that expression does not end with a '('.
      // Therefore it should always be safe to start 2 elements 
      // further along.
      if (idx >= m_infixElems.size())
         throw RedAlert("Programming error in MathExpression::verifyBFunc()");

      ElementType curType = m_infixElems[idx];
      if (curType == COMMA)
      {
         ++commasFound;
         if (commasFound > 1)
            throw AbstractExpressionError("Binary function called with more than 2 arguments");
         // This is the test which verifies rule 4 stated in verifyInfix.   
         if (parCount != 1)
            throw AbstractExpressionError("Misplaced comma in binary function call.");
      }
      else if (curType == BFUNC)
      {
         verifyBFunc(&idx);
      }
      else if (curType == LPAREN)
      {
         ++parCount;
      }
      else if (curType == RPAREN)
      {
         --parCount;
      }
   }
   if (!commasFound)
   {
      throw AbstractExpressionError("Binary function called with fewer than 2 arguments");
   }
   *idxElem = idx;
}

void MathExpression::evaluate (const RealArray& energies, const RealArray& parameters, RealArray& flux, RealArray& fluxErr) const
{
  using Numerics::MathOperator;

  if (energies.size() < 2)
     throw AbstractExpressionError("Energy array must be at least size 2");
  if (m_compType == string("con"))
     convolveEvaluate(energies, parameters, flux, fluxErr);
  else
  {
     const size_t nBins = energies.size() - 1;

     RealArray avgEngs(nBins);
     RealArray binWidths(nBins);
     for (size_t i=0; i<nBins; ++i)
     {
        avgEngs[i] = (energies[i+1]+energies[i])/2.0;
        binWidths[i] = fabs(energies[i+1]-energies[i]);
     }

     std::stack<RealArray> resultsStack;
     size_t numPos = 0;
     size_t parPos = 0;
     size_t opPos = 0;

     // m_postfixElems will contain the following enum values:
     //    ENG, NUM, PARAM, OPER
     for (size_t i=0; i<m_postfixElems.size(); ++i)
     {
        ElementType curType = m_postfixElems[i];
        // ENGC should never get in here, but if it does just treat
        // it like ENG.
        if (curType == ENG || curType == ENGC)
        {
           resultsStack.push(avgEngs);
        }
        else if (curType == NUM)
        {
           resultsStack.push(RealArray(m_numericalConsts[numPos],nBins));
           ++numPos;
        }
        else if (curType == PARAM)
        {
           Real parVal = parameters[m_paramsToGet[parPos]];
           resultsStack.push(RealArray(parVal,nBins));
           ++parPos;
        }
        else if (curType == OPER)
        {
           const MathOperator& mathFunc = *(m_mathOperators[opPos]->second);
           const size_t nArgs = mathFunc.nArgs();
           if (nArgs == 1)
           {
              if (resultsStack.empty())
                 throw RedAlert("Trying to access empty stack in MathExpression::evaluate()");
              RealArray& top = resultsStack.top();
              mathFunc(top);
           }
           else if (nArgs == 2)
           {
              // Note that second array is not a reference.
              if (resultsStack.size() < 2)
                 throw RedAlert("Programmer error: Too few args in MathExpression::evaluate() stack");
              RealArray second = resultsStack.top();
              resultsStack.pop();
              RealArray& first = resultsStack.top();
              mathFunc(first, second);
           }
           ++opPos;
        }
     } // end m_postfixElems loop

     if (resultsStack.size() != 1)
        throw RedAlert("Programmer error: MathExpression::evaluate() stack should be of size 1 at end.");
     if (flux.size() != nBins)
        flux.resize(nBins);
     flux = resultsStack.top();

     if (m_compType != string("mul"))
     {
        // Integrate over bin, assume val is constant across bin.
        flux *= binWidths;
     }
  }
}

void MathExpression::convolveEvaluate (const RealArray& energies, const RealArray& parameters, RealArray& flux, RealArray& fluxErr) const
{
   using Numerics::MathOperator;

   const size_t nBins = energies.size() - 1;
   if (flux.size() != nBins)
      throw RedAlert("Flux array size mismatch in mdef convolve function.");

   RealArray avgEngs(nBins);
   RealArray binWidths(nBins);
   for (size_t i=0; i<nBins; ++i)
   {
      avgEngs[i] = (energies[i+1]+energies[i])/2.0;
      binWidths[i] = fabs(energies[i+1]-energies[i]);
   }

   // To avoid building the numerical constant and parameter arrays
   // nBin times, do it once and store outside the loop (unlike the
   // standard evaluate function).
   const size_t nConsts = m_numericalConsts.size();
   const size_t nParsToGet = m_paramsToGet.size();
   std::vector<RealArray> constArrays(nConsts);
   std::vector<RealArray> paramArrays(nParsToGet);
   for (size_t i=0; i<nConsts; ++i)
   {
      constArrays[i].resize(nBins, m_numericalConsts[i]);
   }
   for (size_t i=0; i<nParsToGet; ++i)
   {
      paramArrays[i].resize(nBins, parameters[m_paramsToGet[i]]);
   }

   RealArray convFlux(0.0,nBins);
   for (size_t iBin=0; iBin<nBins; ++iBin)
   {
     // If input and convolved fluxes are row vectors [....], then
     // convEngs is the matrix convEngs_ji = avgEngs_i - avgEngs_j.
     RealArray convEngs(avgEngs[iBin] - avgEngs);
     std::stack<RealArray> resultsStack;
     size_t numPos = 0;
     size_t parPos = 0;
     size_t opPos = 0;

     // m_postfixElems will contain the following enum values:
     //    ENG, ENGC, NUM, PARAM, OPER
     for (size_t iElem=0; iElem<m_postfixElems.size(); ++iElem)
     {
        ElementType curType = m_postfixElems[iElem];
        if (curType == ENG)
        {
           resultsStack.push(avgEngs);
        }
        else if (curType == ENGC)
        {
           resultsStack.push(convEngs);
        }
        else if (curType == NUM)
        {
           resultsStack.push(constArrays[numPos]);
           ++numPos;
        }
        else if (curType == PARAM)
        {
           resultsStack.push(paramArrays[parPos]);
           ++parPos;
        }
        else if (curType == OPER)
        {
           const MathOperator& mathFunc = *(m_mathOperators[opPos]->second);
           const size_t nArgs = mathFunc.nArgs();
           if (nArgs == 1)
           {
              if (resultsStack.empty())
                 throw RedAlert("Trying to access empty stack in MathExpression::evaluate()");
              RealArray& top = resultsStack.top();
              mathFunc(top);
           }
           else if (nArgs == 2)
           {
              // Note that second array is not a reference.
              if (resultsStack.size() < 2)
                 throw RedAlert("Programmer error: Too few args in MathExpression::evaluate() stack");
              RealArray second = resultsStack.top();
              resultsStack.pop();
              RealArray& first = resultsStack.top();
              mathFunc(first, second);
           }
           ++opPos;
        }
     } // end m_postfixElems loop

     if (resultsStack.size() != 1)
        throw RedAlert("Programmer error: MathExpression::evaluate() stack should be of size 1 at end.");

     // fact is a column vector
     RealArray& fact = resultsStack.top();
     fact *= binWidths[iBin];
     // Now multiply row and col vectors for new flux.
     convFlux[iBin] = (flux*fact).sum();        
   } // end iBins loop

   flux = convFlux;
}

// Additional Declarations
