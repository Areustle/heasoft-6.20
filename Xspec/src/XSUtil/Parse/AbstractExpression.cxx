//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <algorithm>
#include <climits>
#include <sstream>
#include <XSUtil/Utils/IosHolder.h>
#include <XSUtil/Utils/XSutility.h>

// AbstractExpression
#include <XSUtil/Parse/AbstractExpression.h>
 // A '1' indicates that the col symbol can follow the row symbol.
const bool AbstractExpression::s_legalTokenSequences[11][11] = {
   {0,1,1,1,1,1,1,1,1,1,1},  // WordExp
   {1,1,0,0,1,0,0,0,0,0,0},  // Lbrace
   {1,1,1,1,1,1,1,1,0,0,1},  // Rbrace
   {1,1,0,0,0,0,0,0,0,0,0},  // Plus
   {1,1,0,0,0,0,0,0,0,0,0},  // Minus
   {1,1,0,0,1,0,0,0,0,0,0},  // Star
   {1,1,0,0,1,0,0,0,0,0,0},  // Slash  
   {1,1,0,0,0,0,0,0,0,0,0},  // Exp
   {1,0,0,0,0,0,1,0,0,0,0},  // Lcurl  (allows "{/" for table mod path)
   {0,1,1,1,1,1,1,1,0,0,0},  // Rcurl
   {1,1,0,0,1,0,0,0,0,0,0}   // Comma
};


// Class AbstractExpression::AbstractExpressionError 

AbstractExpression::AbstractExpressionError::AbstractExpressionError (const string& errMsg)
  :YellowAlert("Parse Error: ")
{
  *IosHolder::errHolder() << errMsg << '\n';
}


// Class AbstractExpression::TokenType 

AbstractExpression::TokenType::TokenType (AbstractExpression::Token type_, size_t loc, string charToken)
  :type(type_),
   location(loc),
   tokenString(charToken)
{
}


// Class AbstractExpression 
const string AbstractExpression::s_WS = string(" \t\r\n");

AbstractExpression::AbstractExpression()
  : m_exprString(),
    m_tokenList()
{
}

AbstractExpression::AbstractExpression(const AbstractExpression &right)
  :m_exprString(right.m_exprString),
   m_tokenList(right.m_tokenList)
{
}


AbstractExpression::~AbstractExpression()
{
}


bool AbstractExpression::operator == (const AbstractExpression& right) const
{
   // Define equality purely by comparison of tokens.  This makes it
   // insensitive to differences in whitespace.
   bool isEqual = false;
   const size_t nTok = m_tokenList.size();
   if (nTok == right.m_tokenList.size())
   {
      bool diffFound = false;
      for (size_t i=0; !diffFound && i<nTok; ++i)
      {
         const TokenType& thisTok = m_tokenList[i];
         const TokenType& thatTok = right.m_tokenList[i];
         if (thisTok.type != thatTok.type)
            diffFound = true;
         else if (thisTok.type == WordExp)
            diffFound = !(thisTok.tokenString == thatTok.tokenString);
      }
      isEqual = !diffFound;
   }
   return isEqual;
}

void AbstractExpression::init (const string& exprString, bool removeWhitespace)
{
   if (exprString.size() > static_cast<size_t>(INT_MAX))
      throw AbstractExpressionError("Maximum expression string length exceeded.");
   m_exprString = exprString;

   // Note that whitespace removal is not implemented as a template method pattern.
   // The reasoning behind this is that an inheriting class may still want to use the
   // removeAllWhitespace function at some later point even if it doesn't want to
   // use it this early on.  Because it is optional, the other private generic
   // parsing functions can't assume no whitespace.

   verifyAllChars(); // this may throw
   checkBalance(); // this may throw
   standardizeExponentOp();
   removeStarParenCombo();
   if (removeWhitespace)
      removeAllWhitespace();
   // This does nothing unless it is overridden.
   killRedundantParen(0, m_exprString.size()-1);

   makeTokenList();

   if (m_tokenList.size())
   {   
      const TokenType& firstToken = m_tokenList[0];
      const TokenType& lastToken = m_tokenList[m_tokenList.size()-1];
      if (firstToken.type != WordExp && firstToken.type != Lbrace && firstToken.type != Minus)
      {
         string errMsg("Cannot begin an expression with a ");
         errMsg += firstToken.tokenString;
         throw AbstractExpressionError(errMsg);
      }
      if (lastToken.type != WordExp && lastToken.type != Rbrace && lastToken.type != Rcurl)
      {
         string errMsg("Cannot end an expression with a ");
         errMsg += lastToken.tokenString;
         throw AbstractExpressionError(errMsg);
      }
      checkTokenSequence();
   }
}

void AbstractExpression::Swap (AbstractExpression& right)
{
   std::swap(m_exprString,right.m_exprString);
   std::swap(m_tokenList,right.m_tokenList);
}

AbstractExpression::Token AbstractExpression::determineToken (string::size_type* pPos) const
{

   // ASSUME the calling function already checked that position is valid within
   // m_exprString, and that it DOESN'T point to whitespace. 
   Token ttype = Null;
   const string nonWordSyms("()+-*/^{},");
   const Token nonWordToks[] = {Lbrace,Rbrace,Plus,Minus,Star,Slash,Exp,Lcurl,Rcurl,Comma};
   char symbol = m_exprString[*pPos];
   bool found = false;
   for (string::size_type i=0; !found && i<nonWordSyms.size(); ++i)
   {
      if (symbol == nonWordSyms[i])
      {
         ttype = nonWordToks[i];
         found = true;
      }
   }
   if (!found)
   {
      ttype = WordExp;
      // To be consistent with all the other cases above, pPos should 
      // point to the last element of this "word" before returning.
      const string endOfWordIndicator(nonWordSyms + s_WS);
      string::size_type pos = m_exprString.find_first_of(endOfWordIndicator,*pPos);
      // If we're in here, we know that pPos couldn't point to a
      // nonWordSymbol, and that it didn't point to whitespace based on
      // original assumption.  Thus there is no way for pos to be 0, 
      // which makes the following safe to do.  
      *pPos = (pos == string::npos) ? m_exprString.size()-1 : pos - 1;
   }

   return ttype;
}

void AbstractExpression::standardizeExponentOp ()
{
   // Convert any appearance of '**' to '^ '.  Two '*' separated
   // only by whitespace will NOT be considered an exponent
   // operator.  
   const string convertFrom("**");
   const string convertTo("^ "); // Deliberately same length as convertFrom
   const string::size_type len = convertFrom.length();
   string::size_type loc = m_exprString.find(convertFrom);
   while (loc != string::npos)
   {
      m_exprString.replace(loc, len, convertTo);
      loc = m_exprString.find(convertFrom, loc+len);
   }
}

void AbstractExpression::checkBalance () const
{
   // Simple left/right parentheses balance check, not checking 
   // any sort of context.
   // Also check curly brackets for table model notation,
   // which is more strict since no nesting is allowed.
   int count = 0;
   int curlCount = 0;
   string::size_type sz = m_exprString.length();
   for (string::size_type i=0; i<sz; ++i)
   {
      char c = m_exprString[i];
      if (c == '(')
      {
         ++count;
      }
      else if (c == ')')
      {
         --count;
         if (count < 0)
            throw AbstractExpressionError("Unbalanced parentheses.");
      }
      else if (c == '{')
      {
         ++curlCount;
         if (curlCount > 1)
            throw AbstractExpressionError("Cannot nest '{}' brackets.");
      }
      else if (c == '}')
      {
         --curlCount;
         if (curlCount < 0)
            throw AbstractExpressionError("Unbalanced '{}' brackets.");
      }
   }
   if (count)
      throw AbstractExpressionError("Unbalanced parentheses.");
   if (curlCount)
      throw AbstractExpressionError("Unbalbanced '{}' brackets.");
}

void AbstractExpression::removeStarParenCombo ()
{
     // Look for any cases of "*(" and replace with just "(". 
     // Do same for ")*".
     string::size_type loc=m_exprString.find_first_not_of(s_WS);
     string::size_type prevLoc = 0;
     std::vector<string::size_type> toErase;
     char prevChar(0);
     while (loc != string::npos && loc < m_exprString.size()-1)
     {
        if (m_exprString[loc] == '*' && prevChar == ')')
        {
           toErase.push_back(loc);
        }
        else if (m_exprString[loc] == '(' && prevChar == '*')
        {
           toErase.push_back(prevLoc);
        }
        prevChar = m_exprString[loc];
        prevLoc = loc;
        loc = m_exprString.find_first_not_of(s_WS, loc+1);
     }
     for (size_t i=0; i<toErase.size(); ++i)
     {
        m_exprString[toErase[i]] = ' ';
     }
}

void AbstractExpression::verifyAllChars () const
{
   // allValidChars may be overridden by subclasses.
   const string& validChars = allValidChars();
   const string::size_type sz = m_exprString.length();
   for (string::size_type i=0; i<sz; ++i)
      if (validChars.find(m_exprString[i]) == string::npos)
      {
         string errMsg("Invalid character in expression: ");
         errMsg += m_exprString[i];
         throw AbstractExpressionError(errMsg);
      } 
}

void AbstractExpression::makeTokenList ()
{
   m_tokenList.clear();

   string::size_type loc=m_exprString.find_first_not_of(s_WS);
   const string::size_type sz = m_exprString.length();
   while (loc < sz)
   {
      const string::size_type startingLoc = loc;
      const Token ttype = determineToken(&loc);
      TokenType newToken(ttype, startingLoc, 
                m_exprString.substr(startingLoc,loc-startingLoc+1));
      m_tokenList.push_back(newToken);
      loc = m_exprString.find_first_not_of(s_WS,loc+1);
   }
}

void AbstractExpression::checkTokenSequence () const
{
   // Assume that first and last token have already been validated.
   if (m_tokenList.size() > 1)
   {
      for (size_t i=1; i<m_tokenList.size(); ++i)
      {
         const TokenType& prevToken = m_tokenList[i-1];
         const TokenType& currToken = m_tokenList[i];
         if (!s_legalTokenSequences[prevToken.type-1][currToken.type-1])
         {
            const string firstStr = (prevToken.type == WordExp) ? 
                        string("word or number") : prevToken.tokenString;
            const string secondStr = (currToken.type == WordExp) ? 
                        string("word or number") : currToken.tokenString;
            string errMsg("Illegal expression sequence: ");
            errMsg += firstStr + " followed by a " + secondStr;
            throw AbstractExpressionError(errMsg);
         }
      }      
   }
}

void AbstractExpression::removeAllWhitespace ()
{
   string newString;
   string::size_type iPos = m_exprString.find_first_not_of(s_WS);
   while (iPos != string::npos)
   {
      string::size_type endPos = m_exprString.find_first_of(s_WS,iPos);
      string::size_type nChars = (endPos == string::npos) ? string::npos : endPos - iPos;
      newString += m_exprString.substr(iPos, nChars);
      iPos = m_exprString.find_first_not_of(s_WS, endPos);
   }
   m_exprString = newString;
}

void AbstractExpression::killRedundantParen (const string::size_type startPos, const string::size_type endPos)
{
   // Do nothing at this level: AbstractExpression does not have enough
   // information to figure out which parentheses (if any) are redundant.
}

void AbstractExpression::findTheNumbers (std::vector<size_t>& remainingNonNumbers, std::vector<Real>& values) const
{
   // This does not modify the token list (as it is const).  Instead it 
   // fills in the indices of those tokens which are not identified as part of 
   // a number.  

   // This can get very tricky.  For example, -7.3e-2 would initially
   // have tokens Minus,WordExp,Minus,WordExp, all of which will go
   // into the making of a single number.  To be compatible with v11,
   // something like 1.0*e-5 or 1.0*e+5 should not be treated as a number, 
   // but as (1.0*energies)+5.  1.0*e5 though is a single number.

   bool applyPrevMinus = false;
   size_t iTok=0;
   Token prevType = Null;
   remainingNonNumbers.clear();
   values.clear();
   while (iTok < m_tokenList.size())
   {
      const TokenType& curTok = m_tokenList[iTok];
      switch (curTok.type)
      {
         case WordExp:
         {
            // Is WordExp of the form d[.]dde? Unfortunately the
            // (stringstream >> float) test has different results
            // on CC and g++.  The former considers the stream state
            // invalid after doing this, but not the latter.  
            // Therefore must first do a manual test for this case.
            size_t len = curTok.tokenString.length();
            if ((len > 1 && curTok.tokenString[len-1] == 'e') ||
                        curTok.tokenString[len-1] == 'E')
            {
               bool isNum = false;
               Real val = 0.0;
               if (isNonExpFloat(curTok.tokenString.substr(0,len-1), &val))
               {
                  // Is it followed by +ddd or -ddd?
                  if (m_tokenList.size() > 2 && iTok < m_tokenList.size() - 2 && 
                            (m_tokenList[iTok+1].type == Plus || m_tokenList[iTok+1].type == Minus))
                  {
                     if (m_tokenList[iTok+2].type == WordExp)
                     {
                        size_t exp = XSutility::isInteger(m_tokenList[iTok+2].tokenString);
                        if (exp != string::npos)
                        {
                           // We have finally verified that curTok string is
                           // a section of a floating point expression.
                           Real finalVal = val;
                           if (m_tokenList[iTok+1].type == Plus)
                              finalVal *= pow(10.0,static_cast<int>(exp));
                           else
                              finalVal /= pow(10.0,static_cast<int>(exp));
                           if (applyPrevMinus)
                           {
                              finalVal *= -1.0;
                              remainingNonNumbers.pop_back();
                           }
                           values.push_back(finalVal);
                           isNum = true;
                           iTok += 2;                           
                        }
                     }
                  }
               }
               if (!isNum)
                  remainingNonNumbers.push_back(iTok);
            } // end if word ends in 'e'
            else
            {
               Real testFloat = 0.0;
               std::istringstream iss(curTok.tokenString);
               if (!(iss >> testFloat) || !iss.eof())
                  remainingNonNumbers.push_back(iTok);
               else
               {
                  // Still need to determine if it is followed by
                  //   *eddddd, but only if it doesn't already 
                  //   contain exponential notation.
                  Real dummy=0.0;
                  if (isNonExpFloat(curTok.tokenString, &dummy))
                  {
                     if (m_tokenList.size() > 2 && iTok < m_tokenList.size() - 2 && 
                                m_tokenList[iTok+1].type == Star)
                     {
                        const TokenType& testTok = m_tokenList[iTok+2];
                        if ((testTok.type == WordExp &&
                             testTok.tokenString[0] == 'e') ||
                             testTok.tokenString[0] == 'E')
                        {
                           if (testTok.tokenString.length() > 1)
                           {
                              // If in here, the 'e' can't be followed by a '+' or '-'
                              size_t exp = XSutility::isInteger(testTok.tokenString.substr(1));
                              if (exp != string::npos)
                              {
                                 testFloat *= pow(10.0,static_cast<int>(exp));
                                 iTok += 2;
                              }
                           }
                        }
                     } // end if string is followed by a '*' 
                  } // end if tokenString is a float without 'e' format.
                  if (applyPrevMinus)
                  {
                     testFloat *= -1.0;
                     remainingNonNumbers.pop_back();
                  }
                  values.push_back(testFloat);
               } // end if entire WordExp is a number.
            } // end if WordExp does not end in 'e'.
            applyPrevMinus = false;
            prevType = WordExp;
         } // end if curTok is a WordExp
            break;
         case Minus:
            if (prevType == Null || prevType == Lbrace || prevType == Comma
                        || prevType == Star || prevType == Slash)
               applyPrevMinus = true;
            remainingNonNumbers.push_back(iTok);
            prevType = Minus;
            break;
         default:
            remainingNonNumbers.push_back(iTok);
            applyPrevMinus = false;
            prevType = curTok.type;
            break;
      }
      ++iTok;
   }
}

bool AbstractExpression::isNonExpFloat (const string& testStr, Real* value)
{
   // Return true if testStr is of form ddd[.]dddd.... with no 'e'
   // notation and no float suffix specifier.  
   // The optional decimal may be at any position.
   bool isFloat = false;
   std::istringstream iss(testStr);
   Real testFloat = 0.0;
   if ((iss >> testFloat) && iss.eof())
   {
      const string possibleFltLetters("fFlLeE-+");
      if (testStr.find_first_of(possibleFltLetters) == string::npos)
      {
         *value = testFloat;
         isFloat = true;
      }
   }

   return isFloat;
}

// Additional Declarations
