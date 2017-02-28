//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/IosHolder.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Utils/XSutility.h>
#include <stack>

// LinkExpression
#include <XSUtil/Parse/LinkExpression.h>


// Class LinkExpression::LinkExpressionError 

LinkExpression::LinkExpressionError::LinkExpressionError (const string& errMsg)
   :YellowAlert("Link Expression Error: ")
{
   *IosHolder::errHolder() << errMsg << std::endl;
}


// Class LinkExpression 
const string LinkExpression::s_allValidChars = string(":_#.+-/*%!~()abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \t\r\n");
std::map<string,int> LinkExpression::s_precedenceMap;

LinkExpression::LinkExpression()
   : AbstractExpression(),
     m_numericalConsts(),
     m_mathOperators(),
     m_postfixElems(),
     m_infixElems(),
     m_parIDs(),
     m_parToNum(),
     m_nonNumberTokens()
{
   if (s_precedenceMap.empty())
   {
      s_precedenceMap["+"] = 0;
      s_precedenceMap["-"] = 0;
      s_precedenceMap["@"] = 0;
      s_precedenceMap["*"] = 1;
      s_precedenceMap["/"] = 1;
   }
}

LinkExpression::LinkExpression(const LinkExpression &right)
   :AbstractExpression(right),
    m_numericalConsts(right.m_numericalConsts),
    m_mathOperators(right.m_mathOperators),
    m_postfixElems(right.m_postfixElems),
    m_infixElems(right.m_infixElems),
    m_parIDs(right.m_parIDs),
    m_parToNum(right.m_parToNum),
    m_nonNumberTokens(right.m_nonNumberTokens)
{
}


LinkExpression::~LinkExpression()
{
}


LinkExpression & LinkExpression::operator=(const LinkExpression &right)
{
   if (this != &right)
   {
      LinkExpression tmp(right);
      Swap(tmp);
   }
   return *this;
}


void LinkExpression::init (const string& exprString, bool removeWhitespace)
{
   AbstractExpression::init(exprString, true);
   findParIDs();

   std::vector<size_t> numberTokens;
   std::vector<Real> numConsts;
   m_numericalConsts.clear();
   m_nonNumberTokens.clear();
   findTheNumbers(m_nonNumberTokens,numConsts);
   std::vector<Real>::const_iterator itNums = numConsts.begin();
   while (itNums != numConsts.end())
   {
      m_numericalConsts.push_back(*itNums);
      ++itNums;
   }

   // Fill the numberTokens array.  Something like -6.25e-12 would
   // consist of 4 consecutive tokens due to its two minus signs.
   const size_t nTokens = tokenList().size();
   std::vector<size_t>::const_iterator itNonNum = m_nonNumberTokens.begin();
   std::vector<size_t>::const_iterator itNonNumEnd = m_nonNumberTokens.end();
   for (size_t i=0; i<nTokens; ++i)
   {
      if (itNonNum != itNonNumEnd)
      {
         if (*itNonNum == i)
            ++itNonNum;
         else
            numberTokens.push_back(i);
      }
      else
         numberTokens.push_back(i);
   }
   connectParsAndInts(numberTokens);

   verifyWordExprs(numberTokens); 
}

LinkExpression* LinkExpression::clone () const
{
   return new LinkExpression(*this);
}

void LinkExpression::Swap (LinkExpression& right)
{
   AbstractExpression::Swap(right);
   std::swap(m_numericalConsts,right.m_numericalConsts);
   std::swap(m_mathOperators,right.m_mathOperators);
   std::swap(m_postfixElems,right.m_postfixElems);
   std::swap(m_infixElems,right.m_infixElems);
   std::swap(m_parIDs,right.m_parIDs);
   std::swap(m_parToNum,right.m_parToNum);
   std::swap(m_nonNumberTokens,right.m_nonNumberTokens);
}

const string& LinkExpression::allValidChars () const
{
   return s_allValidChars;
}

void LinkExpression::convertToInfix ()
{
   m_infixElems.clear();
   const std::vector<TokenType>& tokens = tokenList();
   ParamInfo::const_iterator itPar = m_parIDs.begin();
   ParamInfo::const_iterator itParEnd = m_parIDs.end();
   // These two count variables are for sanity checks:
   size_t numCount = 0;
   size_t parCount = 0;
   int nextParLoc = (itPar == itParEnd) ? -99 : static_cast<int>(itPar->second);
   int prevIdx = -1;
   int idx = -1;
   for (size_t iTok=0; iTok<m_nonNumberTokens.size(); ++iTok)
   {
      idx = static_cast<int>(m_nonNumberTokens[iTok]);
      // Look for gaps in the token sequence.  A gap indicates either a 
      // numerical constant OR an integer parameter specifier with a possible
      // unary minus in front of it.  This is different and more
      // complicated than the corresponding situation in the MathExpression 
      // class.
      if (idx > prevIdx+1)
      {
         if (nextParLoc == idx-1)
         {
            if (idx > prevIdx+2)
            {
               // This ought to be a unary minus in front of a parameter.
               // If not, something's wrong.  Also idx must be >= 2 if in here.
               if (tokens[idx-2].type != Minus)
               {
                  string errMsg("while parsing ");
                  errMsg += tokens[idx-2].tokenString + tokens[idx-1].tokenString
                       + tokens[idx].tokenString;
                  throw LinkExpressionError(errMsg);
               }
               m_infixElems.push_back(OPER);
               m_mathOperators.push_back("@");
             }
             m_infixElems.push_back(PARAM);
             ++parCount;
             ++itPar;
             nextParLoc = (itPar == itParEnd) ? -99 : static_cast<int>(itPar->second);
         } // end if gap corresponds to a par
         else
         {
            // Can only assume gap corresponds to a number.
            m_infixElems.push_back(NUM);
            ++numCount;
         }
      } // end if gap in token sequencing

      const TokenType& curTok = tokens[idx];
      ElementType elType = NUM; // starting val is irrelevant.
      switch (curTok.type)
      {
         case WordExp:
            elType = PARAM;
            ++parCount;
            break;
         case Lbrace:  
            // Conditions for implied '*': '(' is preceded by
            // ')' OR param name OR NUM.
            if (m_infixElems.size())
            {
               ElementType prev = m_infixElems[m_infixElems.size()-1];
               if (prev == RPAREN || prev == PARAM || prev == NUM)
               {
                  m_infixElems.push_back(OPER);
                  m_mathOperators.push_back("*");
               }
            }
            elType = LPAREN;
            break;
         case Rbrace:
            // This may also include an implied '*', which is
            // inserted after exiting the switch block.
            elType = RPAREN;
            break;
         case Plus:
            elType = OPER;
            m_mathOperators.push_back("+");
            break;
         case Minus:
            elType = OPER;
            // Conditions for unary: First token OR preceded by
            // a *,/,or(.
            if (idx == 0)
               m_mathOperators.push_back("@");
            else
            {
               Token preType = tokens[idx-1].type;
               if (preType == Lbrace || preType == Star || 
                        preType == Slash)
                  m_mathOperators.push_back("@");
               else
                  m_mathOperators.push_back("-");
            }
            break;
         case Star:
            elType = OPER;
            m_mathOperators.push_back("*");
            break;
         case Slash:
            elType = OPER;
            m_mathOperators.push_back("/");
            break;             
         default:
            {
               string errMsg("Unrecognized symbol during infix parsing: ");
               errMsg += curTok.tokenString;
               throw LinkExpressionError(errMsg);
            }
            break;
      }
      m_infixElems.push_back(elType);
      // Conditions for implied '*' after ')': ')' is followed by
      // a WordExp of any kind.  
      if (elType == RPAREN && static_cast<int>(tokens.size()) > idx+1)
      {
         if (tokenList()[idx+1].type == WordExp)
         {
            m_infixElems.push_back(OPER);
            m_mathOperators.push_back("*");
         }
      }
      prevIdx = idx;
   } // End non-NUM token loop
   // At this point, idx should point to the last token that is NOT 
   // determined to be part of a number (and remember, a unary minus
   // will be categorized as part of a number).  
   // idx may be negative here, so we don't want to cast it into a size_t.
   if (tokens.size() && idx+1 < static_cast<int>(tokens.size()))
   {
      // Assume the last token(s) must be a number, or par with
      // optional unary minus.
      if (nextParLoc == static_cast<int>(tokens.size()) - 1)
      {
         if (idx+2 == nextParLoc)
         {
            m_infixElems.push_back(OPER);
            m_mathOperators.push_back("@");            
         }
         m_infixElems.push_back(PARAM);
         ++parCount;
      }
      else
      {
         m_infixElems.push_back(NUM);
         ++numCount;
      }
   }
   if (numCount != m_numericalConsts.size() || parCount != m_parIDs.size())
      throw RedAlert("Programming error while parsing infix link expression.");

  XSstream* xscout = dynamic_cast<XSstream*>(IosHolder::outHolder());
  if (xscout)
  {
     *xscout << xsverbose(40) << "Infix elements: ";
     for (size_t i=0; i<m_infixElems.size(); ++i)
        *xscout << m_infixElems[i] << " ";
     *xscout << std::endl<< "Numerical consts: ";
     std::list<Real>::const_iterator itNum = m_numericalConsts.begin();
     std::list<Real>::const_iterator itNumEnd = m_numericalConsts.end();
     while (itNum != itNumEnd)
     {
        *xscout << *itNum << " ";
        ++itNum;
     }
     *xscout << std::endl << "Infix operators: ";
     for (size_t i=0; i<m_mathOperators.size(); ++i)
        *xscout << m_mathOperators[i] << " ";
     *xscout << std::endl << "Params: ";
     while (itPar != itParEnd)
     {
        *xscout << itPar->first << " ";
        ++itPar;
     }
     *xscout << std::endl << xsverbose();
  }
}

void LinkExpression::convertToPostfix ()
{
   // This will fill in the m_postfixElems vector and also rearrange m_mathOperators
   // into the order the operators will be called in postfix.  The vectors for
   // pars and numbers need no such reordering since they are called in the
   // same sequence for infix and postfix.

   using namespace std;
   m_postfixElems.clear();

   // The int in the pair below is for storing operator precedence number.
   stack<pair<int,string> > opStack;
   vector<string> tmpOperators;
   const size_t nElems = m_infixElems.size();
   size_t opPos = 0;
   for (size_t i=0; i<nElems; ++i)
   {
      ElementType curType = m_infixElems[i];
      switch (curType)
      {
         case OPER:
         {
            const string& curOp = m_mathOperators[opPos];
            int prec = s_precedenceMap.find(curOp)->second;
            if (!opStack.empty())
            {
               pair<int,string> topOp = opStack.top();
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
         case LPAREN:
            // LPAREN is not an actual operator, but we still need to store it 
            // as a placeholder in the opStack.  Give it a dummy precedence 
            // that's lower than any real operator, and an empty string.
            opStack.push(make_pair(-1,string("")));
            break;
         case RPAREN:
            // Pop the operators stack until we come to the first left parenthesis.
            {
               int testPrec = 0;
               do
               {
                  pair<int,string> topOp = opStack.top();
                  testPrec = topOp.first;
                  if (topOp.second.length())
                  {
                     m_postfixElems.push_back(OPER);
                     tmpOperators.push_back(topOp.second);
                  }
                  opStack.pop();
               } while (testPrec != -1 && !opStack.empty());
            }
            break;
         default:
            m_postfixElems.push_back(curType);
            break;
      }
   } // end infix elems loop

   // Any operators remaining on the stack must now be popped to output.
   while (!opStack.empty())
   {
      m_postfixElems.push_back(OPER);
      tmpOperators.push_back(opStack.top().second);
      opStack.pop();
   }

   m_mathOperators = tmpOperators;

  XSstream* xscout = dynamic_cast<XSstream*>(IosHolder::outHolder());
  if (xscout)
  {
     *xscout << xsverbose(40) << "Postfix elements: ";
     for (size_t i=0; i<m_postfixElems.size(); ++i)
        *xscout << m_postfixElems[i] << " ";
     *xscout << std::endl << "Postfix operators: ";
     for (size_t i=0; i<m_mathOperators.size(); ++i)
        *xscout << m_mathOperators[i] << " ";
     *xscout << std::endl << xsverbose();
  }

}

Real LinkExpression::evaluate (const RealArray& parVals) const
{
   std::stack<Real> resultsStack;
   std::list<Real>::const_iterator itNum = m_numericalConsts.begin();
   size_t parPos = 0;
   size_t opPos = 0;

   for (size_t i=0; i<m_postfixElems.size(); ++i)
   {
      ElementType curType = m_postfixElems[i];
      if (curType == NUM)
      {
         resultsStack.push(*itNum);
         ++itNum;
      }
      else if (curType == PARAM)
      {
         if (parPos >= parVals.size())
            throw RedAlert("Parameter array size error in LinkExpression::evaluate");
         resultsStack.push(parVals[parPos]);
         ++parPos;
      }
      else if (curType == OPER)
      {
         const string& curOp = m_mathOperators[opPos];
         if (curOp == "@")
         {
            if (resultsStack.empty())
               throw RedAlert("Trying to access empty stack in LinkExpression::evaluate");
            Real& topVal = resultsStack.top();
            topVal *= -1.0; 
         }
         else
         {
            if (resultsStack.size() < 2)
               throw RedAlert("Programmer error: Too few args in LinkExpression::evaluate");
            Real val2 = resultsStack.top();
            resultsStack.pop();
            Real& val1 = resultsStack.top();
            if (curOp == "+")
               val1 += val2;            
            else if (curOp == "-")
               val1 -= val2;            
            else if (curOp == "*")
               val1 *= val2;
            else if (curOp == "/")
               val1 /= val2;
         }
         ++opPos;
      }
   } // end postfix elems loop
   if (resultsStack.size() != 1)
      throw RedAlert("Programmer error: LinkExpression::evaluate() stack should be of size 1 at end.");

   return resultsStack.top();
}

void LinkExpression::resetParamInfo (const std::vector<bool>& isParFound)
{
   if (isParFound.size() != m_parIDs.size())
      throw RedAlert("Parameter mismatch in LinkExpression::resetParamInfo.");
   if (m_parIDs.size() != m_parToNum.size())
      throw RedAlert("Internal parameter array mismatch in LinkExpression::resetParamInfo.");

   size_t iPar=0; 
   std::vector<size_t> numsToRemove; 
   std::vector<size_t> parsToRemove; 
   ParamInfo::iterator itPar = m_parIDs.begin();
   ParamInfo::iterator itParEnd = m_parIDs.end();
   while (itPar != itParEnd)
   {
      if (XSutility::isInteger(itPar->first) != string::npos)
      {
         // Integer ID must be removed from either m_parIDs or 
         // m_numericalConsts.
         if (isParFound[iPar])
         {
            // Remove from m_numericalConsts.
            if (m_parToNum[iPar] >= m_numericalConsts.size())
               throw RedAlert("Improper parToNum value in LinkExpression::resetParamInfo.");
            numsToRemove.push_back(m_parToNum[iPar]);
         }
         else
         {
            *IosHolder::outHolder()<<"Warning: No parameter " << itPar->first 
               <<", it will be interpreted as an integer constant.\n"
               <<"         (To avoid this message add a trailing '.' to integer constants.)"
               << std::endl;
             parsToRemove.push_back(iPar);
         }
      }
      else
      {
         if (!isParFound[iPar])
         {
            string errMsg("Parameter reference ");
            errMsg += itPar->first;
            errMsg += " in link expression is invalid";
            throw LinkExpressionError(errMsg);
         }
      }      
      ++itPar;
      ++iPar;
   }

   size_t iCount = 0;
   itPar = m_parIDs.begin();
   for (size_t i=0; i<parsToRemove.size(); ++i)
   {
      while (iCount < parsToRemove[i])
         ++itPar, ++iCount;
      m_parIDs.erase(itPar++); // Must use postfix.
      ++iCount;
   }

   iCount = 0;
   std::list<Real>::iterator itNum = m_numericalConsts.begin();
   for (size_t i=0; i<numsToRemove.size(); ++i)
   {
      while (iCount < numsToRemove[i])
         ++itNum, ++iCount;
      m_numericalConsts.erase(itNum++);
      ++iCount;
   }
   // m_parToNum has served its purpose and is obsolete after modifications
   // to m_parIDs and m_numericalConsts, therefore clear it.
   m_parToNum.clear();

   convertToInfix();
   convertToPostfix();
}

void LinkExpression::findParIDs ()
{
   m_parIDs.clear();
   const std::vector<TokenType>& tokens = tokenList();
   for (size_t i=0; i<tokens.size(); ++i)
   {
      const TokenType& tok = tokens[i];
      if (tok.type == WordExp)
      {
         // Valid words must be either integer strings, string:int pairs,
         // or (perhaps pieces of) floating-point numbers.  We'll look for 
         // the first two types here.  Ultimate syntax validation will 
         // have to wait till after findTheNumbers call.
         string dummyStr;
         size_t dummyInt=0;
         if (XSutility::isInteger(tok.tokenString) != string::npos)
         {
            // This is an integer but it could just be the trailing
            // end of d.dde-nn notation, in which case it will have to
            // be rectified after findTheNumbers.
            m_parIDs.push_back(std::make_pair(tok.tokenString,i));
         }
         else if (XSparse::stringIntPair(tok.tokenString, dummyStr, dummyInt))
         {
            m_parIDs.push_back(std::make_pair(tok.tokenString,i));
         }
      }
   }
}

void LinkExpression::verifyWordExprs (const std::vector<size_t>& numberedTokens) const
{
   // Check that every token of WordExp type has been categorized as a
   // either a parID or as part of a numerical const.  At this stage of
   // initialization it may even be categorized as both, but that's OK so
   // long as it's in at least one list.
   bool isFound = false;
   ParamInfo::const_iterator itPar = m_parIDs.begin();
   ParamInfo::const_iterator itParEnd = m_parIDs.end();
   size_t iNumTok = 0;
   for (size_t i=0; i<tokenList().size(); ++i)
   {
      if (tokenList()[i].type == WordExp)
      {
         // Is it a parID?
         while (itPar != itParEnd && itPar->second < i)
            ++itPar;
         if (itPar != itParEnd && itPar->second == i)
            isFound = true;

         if (!isFound)
         {
            // Is it a numericalConst?
            while (iNumTok < numberedTokens.size() && 
                        numberedTokens[iNumTok] < i)
               ++iNumTok;
            if (iNumTok < numberedTokens.size() && 
                        numberedTokens[iNumTok] == i)
               isFound = true;
         }

         if (!isFound)
         {
            string errMsg("The string: ");
            errMsg += tokenList()[i].tokenString;
            errMsg += "   is neither a parameter identifier or numerical constant.";
            throw LinkExpressionError(errMsg);
         }
      }
   }
}

void LinkExpression::connectParsAndInts (const std::vector<size_t>& numberTokens)
{
   // Any integers will have been added to both the parIDs and 
   // numericalConsts lists, and they will eventually be removed
   // from one or the other once it is determined if they 
   // correspond to an actual Parameter object.  However, if an 
   // integer is actually the trailing part of a float in
   // d.dde-nn format, it must be removed from the parIDs list NOW.

   // Note that not all parIDs will be numericalConsts (ie. pars
   // belonging to named models), and not all numericalConsts will be parIDs
   // (ie. floats).  

   m_parToNum.clear();
   const size_t nNumTokens = numberTokens.size();
   ParamInfo::iterator itPar = m_parIDs.begin();
   ParamInfo::iterator itParEnd = m_parIDs.end();
   size_t iNumPos = 0; // this is an index to a tokenList index.
   size_t iNumConstPos = 0; // an index to m_numericalConsts
   while (itPar != itParEnd)
   {
      bool isRemoved = false;
      while (iNumPos < nNumTokens && numberTokens[iNumPos] < itPar->second)
      {
         // A gap in numberToken sequence indicates the start of a 
         // new entry in m_numericalConsts.
         if (iNumPos > 0 && numberTokens[iNumPos] > numberTokens[iNumPos-1]+1)
            ++iNumConstPos; 
         ++iNumPos;
      }
      if (iNumPos < nNumTokens)
      {
         if (numberTokens[iNumPos] == itPar->second)
         {
            if (iNumPos > 0 && numberTokens[iNumPos] > numberTokens[iNumPos-1]+1)
               ++iNumConstPos; // start of a new numericalConst.
            if (iNumPos > 1 && 
                numberTokens[iNumPos-1] == numberTokens[iNumPos] - 1 &&
                numberTokens[iNumPos-2] == numberTokens[iNumPos] - 2)
            {
               // Bingo.  We've identified an integer that is actually
               // part of a floating-point exponential format.
               // Must do this with a post-increment
               m_parIDs.erase(itPar++);
               isRemoved = true;
            }
            else
            {
               if (iNumConstPos >= m_numericalConsts.size())
                  throw RedAlert("LinkEpxression m_numericalConsts array bounds calculation.");
               m_parToNum.push_back(iNumConstPos);
            }
            ++iNumPos;
         }
         else
            m_parToNum.push_back(string::npos);
      }
      else
         m_parToNum.push_back(string::npos);

      if (!isRemoved)
         ++itPar;
   }
}

// Additional Declarations
