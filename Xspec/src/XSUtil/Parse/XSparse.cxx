//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Signals/SignalHandler.h>
#include <fstream>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <unistd.h>
#include <algorithm>
#include <ios>
#include <cstring>
#include <queue>

// sstream
#include <sstream>
// XSparse
#include <XSUtil/Parse/XSparse.h>


// Class XSparse::SkipThis 

XSparse::SkipThis::SkipThis (const std::string& diag)
  : YellowAlert()
{
  *IosHolder::errHolder() << diag;
}


// Class XSparse::AbortLoop 

XSparse::AbortLoop::AbortLoop (const std::string& diag)
  : YellowAlert()
{
  *IosHolder::errHolder() << diag;
}


// Class XSparse::SyntaxError 

XSparse::SyntaxError::SyntaxError (const std::string& diag)
  : YellowAlert(" - syntax error: ")
{
  *IosHolder::errHolder() << diag << '\n';
}


// Class XSparse::InputError 

XSparse::InputError::InputError (const std::string& diag)
  : YellowAlert(" input error: ")
{
  *IosHolder::errHolder() << diag << '\n';
}


// Class XSparse::InvalidRange 

XSparse::InvalidRange::InvalidRange()
  : YellowAlert()
{
}

XSparse::InvalidRange::InvalidRange (const std::string& diag)
  : YellowAlert("Invalid Range:  ")
{
  *IosHolder::errHolder() << diag << '\n';
}


// Class Utility XSparse 
const string XSparse::STRINGNULLS = " \t";
const string XSparse::INPUT_DELIMITER = "&";
const size_t XSparse::XCM_LEN = 256;
const string XSparse::s_NONE = "none";
const string XSparse::s_SKIP = "/*";
bool XSparse::s_rangeIsReal = false;
const string::size_type XSparse::s_NOTFOUND = string::npos;
char* XSparse::s_augmentedLibraryPath = 0;
bool XSparse::s_executingScript = false;
const string XSparse::s_USE_DEFAULT = "/";

void XSparse::findBatchString (const string& cmdLine, string& command, std::deque<string>& batch)
{
    command.clear();
    string::size_type isBatch = cmdLine.find_first_of(INPUT_DELIMITER);

    if (isBatch != string::npos)
    {
       // Don't assume cmdLine can't begin with whitespace.  User can
       // group args arbitrarily in quotes and assign to single variable.
        string::size_type startPos = cmdLine.find_first_not_of(STRINGNULLS);
        if (startPos != isBatch) 
           command = cmdLine.substr(startPos,isBatch-startPos);
        string rest(cmdLine.substr(isBatch+1));
        string::size_type start (rest.find_first_not_of(STRINGNULLS));
        string::size_type len = rest.length();
        while (start != string::npos)
        {
           string::size_type finish (rest.find_first_of(INPUT_DELIMITER,start));
           if (finish != string::npos)
           {
              // check for case of '&' followed by '&' with nothing
              // except perhaps whitespace in between.  This indicates a
              // blank entry, to be replaced with '/' (skip this).
              if (finish == start)
              {
                 batch.push_back("/");
              }
              else
              {
                 batch.push_back(rest.substr(start, finish-start));
              }
              start = (finish+1 == len) ? string::npos : 
                                rest.find_first_not_of(STRINGNULLS, finish+1);
           }
           else
           {
              batch.push_back(rest.substr(start, len-start));
              start = string::npos;
           } 
        }
    }
    else
    {
        command = cmdLine;   
    }
    return; 
}

IntegerArray XSparse::getRanges (const string& rangeString, int lastIndex)
{
    // returns ranges [of parameter, data set numbers from string of the form "*" or "n1-m1,n2-m2,n3"
    if (rangeString.empty()) return IntegerArray(); 

    // first case: rangeString has size 1 and it's a '*'.

    IntegerArray v;
    if (rangeString.size() == 1 && rangeString[0] == '*')
    {
        // '-2' interprets to 'the full range' ... now in both directions,
        // lower and upper.
        v.resize(2,-2);
        return v;       
    }

    try
    {
	//	int comma = rangeString.find_first_of(','); 
	//	if (comma > 0 )
	{
	    int rBegin = 0; 
	    int rEnd = 0;
	    int rLength = rangeString.length();
	    if (lastIndex != -1) v.reserve(lastIndex);

	    do {
		rEnd = rangeString.find_first_of(',',rBegin);

		if ((rEnd < 0) || (rEnd > rLength)) 
		    rEnd = rLength;

		string subRange = rangeString.substr(rBegin, rEnd - rBegin);

		int b = 0, e = 0;

		if (subRange.length() > 0) {
		    oneRange(subRange,b,e);

		    if(lastIndex > 0 && e < 0)
			e = lastIndex;

		    if(b < 0 || e < 0) {
			v.push_back(b);
			v.push_back(e);
		    }
		    else
			for (int i = 0; i <= e-b; ++i ) 
			    v.push_back(b + i);

		    rBegin = rEnd + 1;
		}
		else 
		    break;
	    } while(rBegin < rLength);

#ifndef STD_COUNT_DEFECT
	    int wilds(std::count(v.begin(),v.end(),-1));
#else
	    int wilds(0); 
	    std::count(v.begin(),v.end(),-1,wilds);
#endif
	    if (wilds > 1)
	    {
		int first = NOTFOUND();
		for (size_t j = 0; j < v.size() ; ++j)
		    if ( v[j] != -1 ) first = std::min(first,v[j]);

		v.resize(2);
		v[0] = first;
		v[1] = -1;
	    }

	    return v;
	}
    }
    catch (InvalidRange)
    {
        // thrown by oneRange.
        v.erase(v.begin(),v.end());
        throw;
    }
    return v;
}

void XSparse::oneRange (const string& inputRange, int& beginRange, int& endRange)
{
  // Valid input: string of form "m-n"or "m" where m,n are positive integers
  // (but not 0) or * or ** wildcards.  Function will THROW if input
  // is not valid, leaving beginRange and endRange undefined.  If * or **
  // are present, beginRange and endRange will be set to -1 or -2 as needed.
  // At completion, endRange will be >= beginRange when they are both
  // positive (they're swapped if not).

    const char delim('-');
    size_t N (inputRange.size());

    if ( N == 0 ) 
	throw InvalidRange(" attempt to parse empty string ");

    string::size_type nDelimLoc = inputRange.find_first_of(delim);

    // generic range error error message.
    std::string errMsg = std::string("range ") + inputRange + " contains invalid characters ";

    // no valid delimiter. There is an error if the range is not a pure integer.
    // if no error, the "range" contains one element.
    if(nDelimLoc == string::npos)
    {
       if (inputRange == "*" || inputRange == "**")
       {
          beginRange = endRange = -2;
       }
       else
       {
          beginRange = validateRangeSpecifier(inputRange);
          endRange = beginRange;
       }
    }
    else
    {
        // Catch cases where delim has been placed at the beginning
        // or end of string, or appears more than once.
        if (nDelimLoc == 0 || nDelimLoc == N)
        {
           throw InvalidRange(errMsg);
        }
        if (inputRange.find_first_of(delim,nDelimLoc+1) != string::npos)
        {
           throw InvalidRange(errMsg);
        }
	// now, read the beginRange argument and the delimiter.
	string BR(inputRange.substr(0,nDelimLoc));

	if (BR == "*") 
	{
	    beginRange = -1;
	}
	else if (BR == "**") 
	{
	    beginRange = -2; 
	}      
	else
	{
           beginRange = validateRangeSpecifier(BR);
	}

	string ER(inputRange.substr(nDelimLoc+1));
	if (ER == "*") 
	{
	    endRange = -1;
	}
	else if (ER == "**") 
	{
	    endRange = -2; 
	}      
	else
	{
           endRange = validateRangeSpecifier(ER);
	}
	if (endRange > 0 && beginRange > 0 && (endRange < beginRange) )
	{
	    int swap = endRange;
	    endRange = beginRange;
	    beginRange = swap;
	} 
    }
}

string XSparse::returnDelimitedArgument (string& inString, const string& xsdelim)
{
    string arg("");

    //   a non-destructive version of strtok. Returns a pointer to an empty
    //   string if the delimiter is not found, and returns an empty string if
    //   if the input string is of zero size. Note that  the C version returned
    //   a NULL in the latter case.


    if (inString.size() == 0) return arg;


    int n = inString.find_first_not_of(STRINGNULLS);
    if (n < 0) return arg;

    inString = inString.substr(n);

    int lenArg = inString.find_first_of(xsdelim);

    if ( lenArg  >=  0)
    {
        if (lenArg > 0) arg = inString.substr(0,lenArg);
        inString = inString.substr(lenArg+1);
    }
    else
    {
        arg = inString;
        inString = "";
    }
    return arg;
}
void XSparse::returnAllDelimitedArguments (const string& inString, const string& xsdelim, StringArray& allArgs)
{
    if (inString.size() == 0) return;

    string inputString = inString;
    while (inputString.size() > 0) {
      string arg = returnDelimitedArgument(inputString, " ");
      if (arg.size() > 0) allArgs.push_back(arg);
    }
    return;
}

string XSparse::getNextLine (std::ifstream& stream)
{
    // Function for getting next line from file.
    // Reads line and strips leading and trailing blanks and tabs.
    // Also ignores commented shell script lines denoted by '#' marks.

    //Edit 7/29/04 - Function now does NOT ignore whitespace and
    //comments at the end of a line, only those lines that begin
    //with a comment - JAM

    string line;
    const string WS(" \t\r\n");
    bool isFound = false;

    while (stream && !isFound)
    {
	string tt;
	// ignore commented lines by reading this loop until we get a line that is
	// not commented. The command processor will deal with
	// blank lines.
	getline(stream,tt);
        isFound = true;
        size_t len = tt.length();
        if (len)
        {
           string::size_type nonWS = tt.find_first_not_of(WS);
           if (nonWS != string::npos && tt[nonWS] == '#')
           {
              // First char is '#' so skip line.
              isFound = false;
           }          
           else
           {
              // If script was edited in DOS mode, newlines are "\r\n", yet
              // getline call only removes the "\n" on Linux/Unix/Mac.  We
              // don't want this trailing "\r" getting through and wreaking
              // havoc in execscript.
              if (tt[len-1] == '\r')
                 tt.erase(len-1,1);
	      line = tt;
           }
        }
    }

    return line;
}

void XSparse::parseInputModelString (const string& inputString, string& stringPar, size_t& intPar, string& commandString)
{


  // intStringPair returns a source number and a model name from an
  // argument of the form n:word. If there is no colon, n = XSparse::NOTFOUND()
  // and the arg. is to be interpreted as either a model name or 
  // a model expression.
  string tmpString(inputString);
  string first = returnDelimitedArgument(tmpString," \t");
  XSparse::intStringPair(first,intPar, stringPar);

  if (intPar == s_NOTFOUND)
  {
          intPar = 1;
          stringPar = "";
          commandString = inputString;
  }
  else
  {
     // If here, at least one colon was already found.  Now enforce the 
     // rule that Model names MAY NOT CONTAIN A COLON.  Otherwise they
     // might interfere with gain parameter identification in commands
     // such as newpar.  

     if (stringPar.find(':') != s_NOTFOUND)
     {
        throw SyntaxError(" <int>:<string> model identifier may contain only 1 colon.");
     }


     if (tmpString.find_first_not_of(STRINGNULLS) == s_NOTFOUND)
     {
             throw SyntaxError (" a model expression is required here ");       
     }
     commandString = tmpString;
  }
}

bool XSparse::integerPair (const string& arg, int& first, int& second)
{
  // integerPair returns true for <n> <n>: :<m> <n>:<m>
  using namespace std;
  bool isInteger = false;
  string::size_type startPos = arg.find_first_not_of(STRINGNULLS);
  if (startPos == string::npos)
  {
     first = -1;
     second = -1;
  }
  else
  {
     string  sarg(arg.substr(startPos));
     string::size_type colon = string::npos;
     if ( (colon = sarg.find_first_of(':')) == string::npos)
     {
        second = -1; 
        size_t j = 0;      
        size_t larg = sarg.length();
        // see if this is an integer argument.
        while (j != larg && std::isdigit(sarg[j],locale()) ) { ++j; }

        if ( j != sarg.length() )
        { 
           first = -1;
        }
        else
        {
           // now we know it's a number, so can leave out the type checking.
           istringstream argStream(sarg);
           argStream >> first;
           isInteger = true;                       
        }
     }
     else
     {
        // string is of form  ":<arg>" which must be an integer
        if (sarg[0] == ':') 
        {
           istringstream argStream(sarg.substr(1));
           if (!(argStream >> second) || !argStream.eof())
           {
              // return with isInteger false, and first,second set to -1 
              first = -1;
              second = -1;
           }
           else
           {
              first = 1;
              isInteger = true;
           }
        }
        else
        {
           // we have a string with a colon inside it, try to read 2 integers.
           istringstream firstArgStream(sarg.substr(0,colon));
              // get first integer from string.
           if (!(firstArgStream >> first) || !firstArgStream.eof())
           {
              // first argument is garbage, string is not an integer pair.
              first = -1;
              second = -1;
           }
           else
           {
              if (colon == sarg.length()-1)
              {
                 // Allow case of <n>:
                 second = 1;
                 isInteger = true;
              }
              else
              {
                 istringstream secondArgStream(sarg.substr(colon+1));
                 if (!(secondArgStream >> second) || !secondArgStream.eof())
                 {
                    first = second = -1;
                 }
                 else
                 {
                    isInteger = true;
                 }
              }
           }

        }
     }
  }
  return isInteger;       
}

void XSparse::spaceString (char** inputString)
{
  // this utility was needed by XSPEC11: it makes sure tokens
  // on the parse line are separated if need be.
  char* end = NULL;
  char* begintmp = NULL;
  char* tmpString = NULL;
  begintmp = (char*)calloc(257,sizeof(inputString));
  end = *inputString + strlen(*inputString);
  tmpString = begintmp;

  while (*inputString < end)
  {
          if (strchr("+-",**inputString) != NULL)
          {
                  if ( strchr("edED",*(*inputString-1)) == NULL)
		  {
			  if ( *(*inputString-1) != ' ')
			  {
				  *tmpString = ' ';
				  tmpString++;
			  }
		  }
          }
	  *tmpString = **inputString;
	  tmpString++;
          (*inputString)++;
  }
  *tmpString = '\0';
  // this looks like a MAJOR problem!
  strcpy(*inputString,begintmp);
  free(begintmp);
  return;
}

int XSparse::checkExponent (char* inputString)
{
  // check for exponents. An XSPEC11 relic.
  char* strPtr = inputString;
  int status = 0;
  while (strPtr < inputString + strlen(inputString)-1)
  {
	  if ( strchr("edED",*strPtr) != NULL)
	  {
	    if ( ( (strchr("+-",*(strPtr+1)) != NULL) 
		   && !isdigit(*(strPtr+2)) ) ||
		 ( (strchr("+-",*(strPtr+1)) == NULL)
		   && !isdigit(*(strPtr+1)) ) )
	      {
		status = -1;
		break;
	      }
	  }
	  strPtr++;
  }
  return status;
}

bool XSparse::hasRange (const string& strg, size_t& beginRng, size_t& endRng)
{
  bool stringHasRange = false;
  int findOpen = strg.find_first_of('{');
  if (findOpen < 0) 
  {
        beginRng = 0;
        endRng  = strg.length();         
  }
  else
  {
        stringHasRange = true;
        beginRng = findOpen;
        int findClose = strg.find_first_of('}',findOpen);
        findClose > 0 ? endRng = findClose : endRng = strg.length();
  }
  return stringHasRange;     
}

void XSparse::getFileNameFromUser (const string& oldName, string& newName, XSutility::fileType type)
{
   // Replace the oldName with a user added newName. We check that the file 
   // exists, but not whether it is valid.  Test the part of newName only
   // up until the first '{' or '[' is found.  (This does not do serious
   // syntax checking on bracket usage.  If something is wrong, it should
   // be caught by one of the later parsing functions.)

   // this code will need to be modified when scripting implementation is
   // considered.
   string reportType;
   static const string SPEC = " spectrum ";
   static const string BACK = " background ";
   static const string CORR = " correction data ";
   static const string RESP = " response ";
   static const string AUXR = " auxiliary response ";
   switch (type)
   {
      case XSutility::PHA: reportType = SPEC; break;
      case XSutility::BCK: reportType = BACK; break;
      case XSutility::COR: reportType = CORR; break;
      case XSutility::RSP: reportType = RESP; break;
      case XSutility::ARF: reportType = AUXR; break;
      default: reportType = " "; break;
   }

   string::size_type squareLoc = string::npos;
   string::size_type plusLoc = string::npos;
   string actualFileName;
   bool wasExtended = checkExtendedSyntax(oldName,squareLoc,plusLoc);
   if (!wasExtended && type == XSutility::RSP)
      wasExtended = getCurlyBracketInt(oldName,actualFileName) > 0;

   *IosHolder::errHolder() << "Error: cannot read" << reportType;   
   wasExtended ? *IosHolder::errHolder() << "file/HDU " :
      *IosHolder::errHolder() << "file ";
   *IosHolder::errHolder() << oldName << "\n";

   struct stat statbuf;
   static const string 
           filePrompt("New filename ( \"none\" or \"/*\" to return to the XSPEC prompt): ");

   newName = "";

   if ( !s_executingScript )
   {
      while ( 1 )
      {        
         string rawFromUser;
         XSstream* xscin = dynamic_cast<XSstream*>(IosHolder::inHolder());
         if (xscin)
         {
            XSstream::setPrompter(*IosHolder::inHolder(),filePrompt);
         }
         getline(*IosHolder::inHolder(), rawFromUser);
         // Allow user to break with ctrl-C (but
         // only if calling code has registered
         // an appropriate event handler).
         const SIGINT_Handler *intHandler = 
                 dynamic_cast<const SIGINT_Handler*>
                 (SignalHandler::instance()->getHandler(SIGINT));
         if (intHandler)
         {
            if (intHandler->interrupted())
            {
               throw AbortLoop();
            }
         }
         const string WS(" \t");
         string::size_type firstNonWS = rawFromUser.find_first_not_of(WS);
         string::size_type lastNonWS = rawFromUser.find_last_not_of(WS);
         string noWS;
         if (firstNonWS != string::npos)
         {
            noWS = rawFromUser.substr(firstNonWS, lastNonWS-firstNonWS+1);
         }
         string lc(XSutility::lowerCase(noWS));
         if (lc.substr(0,4) == NONE() || (lc.length() == 1 && lc[0] == '/') ) 
         {

            throw SkipThis();
         }
         else if (lc.substr(0,2) == "/*")
         {
            throw AbortLoop();
         }
         else
         {
            // newName should include whatever brackets are in the name,
            // except for non-RSP's curly brackets.  actualFileName
            // must leave out all brackets since we can't go through
            // cfitsio (in XSUtil) to check for file existence.

            const int rowNum = getCurlyBracketInt(noWS,actualFileName);
            const bool isCurl = rowNum > 0;

            // If this is not a RESP then curly brackets indicate 
            // row numbers.  These are not relevant here and will be 
            // removed from the new filename.   
            if (isCurl)
            {
               if (type == XSutility::RSP)
                  newName = noWS;
               else
               {
                  newName = actualFileName;
                  *IosHolder::outHolder() << "\n***Warning: Row number specifier " 
                     << '{' << rowNum << "} is ignored in the context of\n"
                     <<"*** file name re-prompting.  The row number(s) (if any) from\n"
                     <<"*** the original entry is retained.\n"
                     << std::endl; 
               }
            }
            else
            {
               actualFileName = addSuffix(noWS,type);
               newName = actualFileName;
            }

            bool isExtended = checkExtendedSyntax(actualFileName,squareLoc,plusLoc);
            if (isExtended)
            {
               string::size_type endLoc = (squareLoc != string::npos) ?
                           squareLoc : plusLoc;
               actualFileName = actualFileName.substr(0,endLoc);
            }

            if (!isCurl && !isExtended)
            {
               actualFileName = addSuffix(noWS, type);
               newName = actualFileName;
            }

            if (stat(actualFileName.c_str(),&statbuf) < 0) 
            {
               *IosHolder::outHolder() << "No such file: " << actualFileName << "\n";
            }
            else break;
         }
      }
   }
   else
   {
      // not implemented yet.
   }         
}

bool XSparse::stringIntPair (const string& arg, string& word, size_t& number)
{

  if (arg.size() == 0) return false;

  size_t colon = arg.find_first_of(':');
  size_t first (0);
  bool isStringInt = true;
  word = "";

  if ( colon != s_NOTFOUND )
  {
          word = arg.substr(0,colon);
          first = colon + 1;           
  }
  else
  {
     isStringInt = false;
  }

  size_t posVe = XSutility::isInteger(arg.substr(first));
  if (posVe != s_NOTFOUND)
  {
     number = posVe;
  }
  else
  {
     isStringInt = false;
  }
  return isStringInt;       
}

void XSparse::stringRangePair (const string& arg, string& word, IntegerArray& range)
{
  using namespace std;
  size_t n(arg.size());
  if (n == 0) return;

  // create a stream from input s, cutting off a leading '$' if present.
  // model names may not start with '$' 
  // istringstream s(arg.substr(arg.find_first_not_of('$')));
  istringstream s(arg);

  // a colon is present, so there's a 'word' to be read.
    if (arg.find_first_of(':') != s_NOTFOUND)        
    {        

        try
        {
                s.exceptions(ios_base::badbit | ios_base::failbit | ios_base::eofbit);
                string input("");
                getline(s,input,':');
                if (input.length()) word = input;
                // some compilers eat the delimiter supplied to getline, some don't.
                if (s.peek() == ':') s.ignore(); 
        }
        catch (ios_base::failure&)
        {
                if (s.eof())
                {
                        string msg("No range in parameter specification: ");
                        msg += arg;
                        throw XSparse::InvalidRange(msg);                    
                }
                else
                {
                        throw;
                }
        }
    }
    else word = "";


    // throws InvalidRange exception, caught by the driver.

    try
    {
        string rest("");
        // reset the exception flags, and then turn on 'badbit'. A 'failbit'
        // here means that the string rest is empty, which condition is caught
        // by getRanges and returns an empty range as would be expected.
        s.exceptions(ios_base::goodbit);
        s.exceptions(ios_base::badbit);
        s >> rest;
        IntegerArray tmp = getRanges(rest,-1); 
        if (!tmp.empty()) range = tmp;
    }
    catch (ios_base::failure& exc)
    {
        throw XSparse::InvalidRange("I/O error parsing string/range specification");
    }





}

void XSparse::catchSkips (const string& input, bool silent)
{
  static const string SKIP = "/";
  string msg("");
  if (input == SKIP)
  {
        if (!silent) msg = "... skipped";  
        throw SkipThis(msg);     
  }
  else if (input == s_SKIP)
  {
        if (!silent) msg = "... terminated";
        throw AbortLoop(msg); 
  }      
}

void XSparse::intStringPair (const string& arg, size_t& number, string& word, char delim)
{
  if (arg.size() == 0) return;

  size_t delimPos = arg.find_first_of(delim);
  number = s_NOTFOUND;
  word = "";

  if ( delimPos == s_NOTFOUND )
  {
     word = arg;
  }
  else
  {
     word = arg.substr(delimPos+1);
     size_t posVe = XSutility::isInteger(arg.substr(0,delimPos) );
     if (posVe == s_NOTFOUND)
     {
        string err(" format of integer");
        err += delim;
        err += "string requried here ";
        throw  SyntaxError(err);
     }
     number = static_cast<int>(posVe);
  }
}

bool XSparse::addToLibraryPath (const string& newDirectory, string& fullDirectory, int accessMode)
{
        // add the requested local model directory
        // to the O/S load path.
        static const string LDPATH("LD_LIBRARY_PATH");
        fullDirectory = expandDirectoryPath(newDirectory);

        if ( fullDirectory.length() == 0) return false;

        if ( !access(fullDirectory.c_str(),accessMode))
        {        
                string libraryPath;
                char* ldpath = getenv(LDPATH.c_str());
                if (ldpath) libraryPath = string(ldpath);
                string newAugmentedPath(LDPATH);
                newAugmentedPath += '=';
                newAugmentedPath += fullDirectory;
                newAugmentedPath += ':';

                if (ldpath)
                   newAugmentedPath += libraryPath;
                char* cnewAugmentedPath = (char *)malloc(newAugmentedPath.size() + 1);
                strcpy(cnewAugmentedPath, newAugmentedPath.c_str());

                if (putenv(cnewAugmentedPath))
                {
                   free(cnewAugmentedPath); 
                   return false;
                }

                // Env now points to the newly allocated cnewAugmentedPath string.
                // Free the old path string (if any)
                if (s_augmentedLibraryPath)
                   free(s_augmentedLibraryPath);
                s_augmentedLibraryPath = cnewAugmentedPath;
        }
        return true;
}

string XSparse::expandDirectoryPath (const string& input)
{
   // Return the absolute path specified by input, including expansion of
   // home dir "~" if necessary.
   if (!input.length())
      return string("");
   string stringDir;
   char* enValue(0);
   size_t slash(input.find_first_of('/'));
   string root(input.substr(0,slash));
   switch ( input[0] )
   {
      case '~':
         if (input.length() > 1 && input[1] != '/')
         {
            // Assume this is specifying another user's home 
            // directory, ie. ~<username>[/.../file]
            // If in here, root.length() >= 2.
            string otherUser(root.substr(1));
            // passwd struct is defined in pwd.h
            struct passwd *pwdInfo = getpwnam(otherUser.c_str());
            if (!pwdInfo)
            {
               *IosHolder::errHolder() << "*** Unable to find home directory "
                  << root << std::endl;
               return string("");
            }
            stringDir = string(pwdInfo->pw_dir);
            if (slash != string::npos)
            {
               stringDir += input.substr(slash);
            }
         }
         else
         {
            // Have something of the form ~[/.../file]
            // Expand to user's own home directory.
            enValue = getenv("HOME");
            if (!enValue || enValue[0] != '/')
            {
               *IosHolder::errHolder() <<"*** Unable to expand path for "
                 << input <<"\n      Make sure HOME environment variable is properly set."
                 << std::endl;
               return string("");
            }
            stringDir = string(enValue);
            if (slash != string::npos)
            {
               stringDir += input.substr(slash);
            }            
         }
         break;
      case '.':
         if (input.length() > 1 && input[1] != '.' && slash != 1)
         {
            *IosHolder::errHolder() << "*** invalid directory path: must begin with absolute path"
                  << "\n*** valid environment variable or shell character "
                  << " {~,./,../}\n";
            return string("");     

         }
         else
         {
            const size_t PATHSZ = 1024;
            if ( (enValue = getcwd(NULL,PATHSZ)) == NULL)
            {
               *IosHolder::errHolder() << "*** XSUtil error while converting to absolute path name."
                   << "\n*** Current dir path name may possibly exceed max size = "
                   << PATHSZ << std::endl; 
               return string("");
            }
            if (input.length() == 1)
            {
               // case of just "."
               stringDir = string(enValue);
            }
            else
            {
               switch (input[1])
               {
                  case '/':
                     // input is "./[...]"
                     stringDir = string(enValue);
                     if (slash != XSparse::NOTFOUND())
                     {
                        stringDir += input.substr(slash);       
                     }
                     break;
                  case '.':
                     {
                        // input is "..[...]"
                        stringDir = string(enValue);
                        // stringDir is an abs path so 
                        // there has to be a slash.
                        size_t lastSlash 
                                = stringDir.find_last_of('/');
                        stringDir = stringDir.substr(0,lastSlash);
                        // this looks right...
                        if (slash != XSparse::NOTFOUND())
                        {
                           stringDir 
                              += input.substr(slash);   
                        }      
                     }
                     break;
                  default:
                     break;       
               }
            } // end input.length() > 1
            free(enValue);       
         }  // end case '.'
         break;
      case '$':
         enValue = getenv(root.substr(1).c_str());
         if ( enValue ) 
         {        
             stringDir = string(enValue);
             if (slash != XSparse::NOTFOUND())
             {
                 stringDir += input.substr(slash);
             }
         }
         else
         {
            *IosHolder::errHolder() << " Invalid environment variable " 
                            << root.substr(1) << '\n';
            return string("");     
         }
         break;
      case '/':
      default:
         stringDir = input;
         break;
   }
   return stringDir;        
}

string XSparse::stripDirectoryPath (const string& input)
{
  size_t lastSlash = input.find_last_of("/");
  if ( lastSlash == string::npos ) {
    return input;
  } else {
    return input.substr(lastSlash+1);
  }
}

void XSparse::collectParams (const std::vector<string>& args, IntegerArray& iParams, StringArray& xsParams)
{
  bool prevIsChar = false;
  int idx = 0;

  // These vectors should be empty on input, but just to be sure...
  iParams.clear();
  xsParams.clear();
  const size_t nArgs = args.size();
  for (size_t i = 0; i < nArgs; ++i)
  {
     // Spaces can get in here if user should enter args in quotes
     // (or does something funny with escape characters).
     // Remove leading and trailing WS from argStr, while any intermediate
     // WS will break argStr into additional sequentially numbered args.
     // The important thing is to get all WS out of the way before dealing
     // with commas.
     StringArray spacedArgs;
     collateByWhitespace(spacedArgs, args[i]);
     const size_t nSpArgs = spacedArgs.size();
     for (size_t j=0; j<nSpArgs; ++j)
     {
        string argStr(spacedArgs[j]);
        size_t startLength = argStr.length();

        // Now handle commas
        while (startLength)
        {
	   string beforeComma = XSparse::returnDelimitedArgument(argStr, string(","));
	   if (beforeComma.length() != startLength)
	   {
	      // 1 or more commas still exist after beforeComma,
	      // though args will be empty for this case:
	      //    "(beforeComma),"
	      if (!beforeComma.length())
	      {
	         // Comma is the leading character
	         if (!prevIsChar)
	         {
		    ++idx;
	         }
	         prevIsChar = false;
	         startLength = argStr.length();
	         continue;
	      }
	      else
	      {
	         // Account for the trailing comma after beforeComa,
	         // and proceed.
	         prevIsChar = false;
	         xsParams.push_back(beforeComma);
	         iParams.push_back(idx);
	      }

	   }
	   else
	   {
	      // beforeComma = remainder of string.  It has
	      // length > 0 and there are no commas left.
	      prevIsChar = true;			   
	      xsParams.push_back(beforeComma);
	      iParams.push_back(idx);
	   }
	   ++idx;
	   startLength = argStr.length();
        } // end while startLength
     } // end spacedArgs loop
  } // end input args loop
}

void XSparse::basicPrompt (const string& promptMsg, string& result, std::deque<string>* batchArgs)
{
  string raw("");
  if (!batchArgs || batchArgs->empty())
  {
     XSstream* xscin = dynamic_cast<XSstream*>(IosHolder::inHolder());
     if (xscin)
     {
        XSstream::setPrompter(*IosHolder::inHolder(),promptMsg);
     }
     getline(*IosHolder::inHolder(), raw);
     string::size_type i = raw.find_first_not_of(" \t\r");
     if (i != string::npos)
     {
        raw.erase(0, i);
     }
     else
     {
        result.clear();
	return;
     }     
  }
  else
  {
     // leading whitespace should already be removed
     raw = batchArgs->front();
     batchArgs->pop_front();
  } 
  // remove any remaining trailing whitespace from either script or
  // command line input.
  string::size_type last = raw.find_last_not_of(" \t\r");
  if (last != string::npos && last < raw.length()-1)
  {
     raw.erase(last+1);
  }       

  if (raw.length() >=2 && raw.substr(0,2) == s_SKIP)
  {
          throw AbortLoop();
  }
  result = raw;
}

void XSparse::promptResponseArf (string& responseName, const string& defaultName, string& arfName, size_t& arfRow, const size_t specNum, bool demand, bool doArf, std::deque<string>* batchArgs)
{

  // Originally designed for use with xsFakeit, this function prompts for
  // a name for the location of a response file to go with spectrum
  // numbered specNum.  If demand is 'true' the user will have to
  // enter a name for a response file.  If demand is 'false' and the
  // user enters a blank, then the response name will be the name
  // of the default singleton dummy response.
  // If the user enters anything other than a blank (or abort "/*")
  // and doArf flag is set to true, it then prompts for an 
  // ancillary name with optional row specifier.
  // No checking is performed as to whether these files
  // actually exist.
   responseName = "";
   while (!responseName.length())
   {
      std::ostringstream msg;
      msg << "For fake spectrum #" << specNum
          << " response file is needed: ";
      XSparse::basicPrompt(msg.str(), responseName, batchArgs);
      if (!responseName.length()||(responseName == s_USE_DEFAULT)) 
      {
         if (demand)
            responseName.clear();
         else
            responseName = defaultName;
      }
   }
   if (doArf && (responseName != defaultName))
   {
      bool isOK = false;
      while (!isOK)
      {
         string arfReply;
         XSparse::basicPrompt("   ...and ancillary file: ", arfReply, batchArgs);
         if (arfReply.length() && arfReply != s_USE_DEFAULT)
         {
	    try
	    {
               // this may throw
               arfRow = XSparse::getCurlyBracketInt(arfReply,arfName);

               // This odd bit of functionality is merely for backwards 
               // compatibility.  The old XSparse processStringToken function
               // seemed to add a default suffix for files without '.', but
               // only for non-zero row numbers.
               string::size_type dummySquare = string::npos;
               string::size_type dummyPlus = string::npos;
               if (XSutility::lowerCase(arfName) != XSparse::NONE() &&
                    arfRow > 0 && 
                    !XSparse::checkExtendedSyntax(arfName,dummySquare,dummyPlus))
                  arfName = XSutility::addSuffix(arfName,XSutility::ARF);

	       isOK = true;
	    }
	    catch (YellowAlert&)
	    {
	       isOK = false;
	    }				
         }
         else
         {
	    isOK = true;
         }
      } 
   }   
}

void XSparse::changeExtension (string& fileName, const string& extName, const bool duplicate)
{

  // This function will remove the extension name of fileName and replace
  // it with extName.  If more than 1 extension exists, only the last is 
  // replaced.  If there originally is no extension to fileName, extName 
  // is simply appended.  If the last extension of fileName is equal to
  // extName, then depending on the duplicate flag, either the extension
  // is repeated or nothing is done. 
  if (!fileName.length())  return;

  string::size_type i = fileName.rfind(".");
  if (i == string::npos)
  {
     fileName += extName;
  }
  else
  {
     string origExt = fileName.substr(i);
     if (origExt == extName)
     {
        if (duplicate)
	{
	   fileName += extName;
	}
	// Else, extName is already there, do not duplicate, do nothing.
     }
     else
     {
        fileName.replace(i, string::npos, extName);
     }
  }
}

string XSparse::promptFilename (const string& defaultName, std::deque<string>* batchArgs)
{
   string outFile("");
   string prompt("");
   const string WS(" \t\r");
   prompt = " Fake data file name (" + defaultName + "): ";
   basicPrompt(prompt, outFile, batchArgs);
   if (!outFile.length()) 
   {
      outFile = defaultName;
   }
   else if (outFile.substr(0,1) == s_USE_DEFAULT)
   {
      if (outFile.length() == 1 || WS.find(outFile[1]) != string::npos)
         outFile = defaultName;
   }
   // Do same behavior as v11: Don't test for pre-existing file if
   // running from batch mode, and only test once if running from
   // command line. 
   if (!batchArgs || batchArgs->empty())
   { 
      std::ifstream testFile(outFile.c_str());
      if (testFile)
      {
         string ans;
         prompt = "File " + outFile + " exists - overwrite? (yY/) or (nN): ";
         basicPrompt(prompt, ans, 0);
         if (ans.length() && ans[0] != 'y' && ans[0] != 'Y' && 
                        ans.substr(0,1) != s_USE_DEFAULT)
         {
            prompt = "Enter new name (" + defaultName + "): ";
            basicPrompt(prompt, outFile, 0);
            if (!outFile.length())
            {
               outFile = defaultName;
            }
            else if (outFile.substr(0,1) == s_USE_DEFAULT)
            {
               if (outFile.length() == 1 || WS.find(outFile[1]) != string::npos)
                  outFile = defaultName;
            }
         } 
      }
   }
   return outFile;
}

void XSparse::collateByWhitespace (StringArray& outStrings, const string& inString)
{
  // This is intended to do the type of parsing that Tcl does with command
  // line arguments:  It should take a line of user input, inString, and
  // break it up into pieces wherever it finds a whitespace.  The pieces
  // are placed in outStrings, and will contain no whitespaces.

  string::size_type i=0, j=0;
  string ws(" \t\r\n");
  outStrings.clear();
  i = inString.find_first_not_of(ws);
  while (i != string::npos)
  {
     j = inString.find_first_of(ws, i);
     string::size_type len = (j == string::npos) ? j : j-i;
     outStrings.push_back(inString.substr(i, len));
     if (j == string::npos)
     {
        break;
     }
     i = inString.find_first_not_of(ws, j);
  }
}

void XSparse::checkBrackets (StringArray& args, char bracketChar)
{
   // Used for checking brackets of type '{}' or '[]' in data-type
   // commands (ie. data, resp, arf, etc...).
   // It will check that brackets are used in pairs properly, with NO 
   // nested brackets of the same type.

   // Any whitespace within brackets that is not explicitly marked
   // on the command line (either through use of quotes or a backslash 
   // escape character), will be removed by concatenating the adjacent
   // args.  Therefore this function may reduce the size of the args array.

   // NOTE:  This function will NOT check the validity of the input
   // between the brackets.  If the user enters {Y#$*&#),38qw9}, this
   // function will pass it.  It is left up to a later function to
   // verify that the input makes sense.  It also will NOT check that
   // the bracket pair's location in the larger command string makes
   // any sense. 

   char lBracket = '{';
   char rBracket = '}';
   if (bracketChar == '[')
   {
      lBracket = '[';
      rBracket = ']';
   }
   char bracketPair[3];
   bracketPair[0] = lBracket;
   bracketPair[1] = rBracket;
   bracketPair[2] = 0;

   // First pass through arg strings.  Check that nowhere do we have
   // these situations: {..{...,  }..}.., leading off with a '}',
   // or ending with a '{'.

   int bracketCount = 0;
   const size_t nArgs = args.size();
   for (size_t i=1; i<nArgs; ++i)
   {
      const string& inString = args[i];
      string::size_type pos = 0;
      while (pos != string::npos)
      {
         pos = inString.find_first_of(bracketPair, pos);
         if (pos != string::npos)
         {
            if (inString[pos] == lBracket)  ++bracketCount;
            if (inString[pos] == rBracket)  --bracketCount;
            if (bracketCount != 0 && bracketCount != 1)
            {
               throw SyntaxError("Improper use of brackets");
            }
            ++pos;   
         }
      }
   }
   if (bracketCount != 0)
   {
      string err("Missing ");
      err += rBracket + string(" specifier");
      throw SyntaxError(err);
   }

   // Second pass.  Remove unnecessary spaces between the brackets and 
   // throw an error if no comma separator is found.  As a result of 
   // this section, the number of input arg strings may be modified.
   bool prevComma = false;
   bool inside = false;
   std::list<string> newArgs;
   for (size_t i=0; i<nArgs; ++i)
   {
      newArgs.push_back(args[i]);
   }
   std::list<string>::iterator prevStr = newArgs.begin();
   std::list<string>::iterator iStr = newArgs.begin();
   while (iStr != newArgs.end())
   {
      string& newString = (*iStr);
      if (inside)
      {
         if (!prevComma && newString[0] != ',' && newString[0] != rBracket
                        && (*prevStr)[(*prevStr).length()-1] != lBracket)
         {
            if (bracketChar == '{')
               throw SyntaxError("Comma missing inside {} brackets.");
         }
         string::size_type rpos = newString.find_first_of(rBracket);
         if (rpos != string::npos)
         {
            *prevStr += newString.substr(0, rpos+1);
            newString.erase(0, rpos+1);
            inside = false;
            if (!newString.length())
            {
               newArgs.erase(iStr++);
            }
         }
         else
         {
            prevComma = (newString[newString.length()-1] == ',');
            *prevStr += newString;
            newArgs.erase(iStr++);
         }
      }
      else
      {
         string::size_type lpos = newString.find_first_of(lBracket);
         if (lpos != string::npos)
         {
            // Note: Avoiding the use of the std::count function
            // here due to compiler non-compliance issues.
            string::const_iterator it = newString.begin()+lpos;
            size_t lcount=0, rcount=0;
            while (it != newString.end())
            {
               if (*it == lBracket)  ++lcount;
               else if (*it == rBracket) ++rcount;
               ++it;
            }
            inside = (lcount > rcount);
            if (inside)
            {
               prevComma = (newString[newString.length()-1] == ',');
            }
         }
         prevStr = iStr;
         ++iStr;
      }
   }

   // Now copy the (possibly) modified strings in the newArgs list
   // back into the resized input args vector.
   args.clear();
   iStr = newArgs.begin();
   while (iStr != newArgs.end())
   {
      args.push_back(*iStr);
      ++iStr;
   }  
}

string XSparse::IntToString (size_t num)
{
    std::ostringstream os;
    os << num;
    return os.str();
}

bool XSparse::promptReal (const string& prompt, Real& real, std::deque<string>* batchArgs)
{
  string input("");
  std::ostringstream fullPrompt;
  fullPrompt << prompt << std::showpoint << " (" 
                << real << "): " << std::noshowpoint;
  basicPrompt(fullPrompt.str(), input, batchArgs);
  // "/" = accept default, which means leave "real" param unchanged.
  if (input.length() && input.substr(0,1) != s_USE_DEFAULT)
  {
     std::istringstream iss(input);
     Real test;
     if (!(iss >> test) || !iss.eof())
     {
        return false;
     }
     real = test;
  }  
  return true;
}

void XSparse::separate (string& operand, const string& separators)
{
  string dummy("");
  size_t N(operand.size());

  for ( size_t  j = 0; j < N; ++j)
  {
        if (separators.find(operand[j]) )
        {
                if (j < N - 1 && STRINGNULLS.find(operand[j+1]) != s_NOTFOUND)
                {
                        dummy += operand[j];
                        dummy += ' ';
                }
                else
                {
                        dummy += operand[j];
                }
        } 
        else
        {
                dummy += operand[j];
        }

  }
  operand = dummy;
}

IntegerArray XSparse::expandRange (const IntegerArray& range)
{
    //this function assumes that no wild cards (-2's in most cases) are
    //sent to this function. 
    //Array size better be a multiple of 2. Otherwise, you are using
    //the function wrong :)
    if(!(range.size() % 2)) 
    {
	IntegerArray expandedRange;

	IntegerArray::iterator i_endRange;

	int arrSize = range.size();

	for(int i = 0; i < arrSize; i += 2) {
	    int nRangeStart = range[i], nRangeEnd = range[i + 1];

	    for(int j = nRangeStart; j <= nRangeEnd; ++j) {
		i_endRange = expandedRange.end();

		if(std::find(expandedRange.begin(), 
			     i_endRange, j) == i_endRange)
		    expandedRange.push_back(j);
	    }
	}
	return expandedRange;
    }
    return range;
}

string XSparse::trimWhiteSpace (const string& value)
{
    string::size_type beg;

    if(value.size() && (beg = value.find_first_not_of(" \t")) != string::npos)
	return value.substr(beg, value.find_last_not_of(" \t") - beg + 1);
    else
	return string("");
}

RangePair XSparse::wildRange (const string& inputRange, const RangePair& maxRange, RangePair& prevRange)
{
  // Input: inputRange is a single range string of form "m-n" or "m"
  // where m,n are positive integers or * or **. maxRange specifies
  // the allowed low-high range.  maxRange.first must be <= maxRange.second.
  // If oneRange returns successfully, prevRange will be updated with
  // the newest values.

  //OneRange performs
  // syntax evaluation and returns the integer low and high ranges, 
  // with -1 or -2 in place of wildcards.  This function replaces
  // -1, -2 with their actual values, determined by prevRange and
  // maxRange respectively.  It also insures that outRange.first 
  // <= outRange.second (this check is performed in oneRange when
  // not dealing with wildcards).

  if (maxRange.first > maxRange.second)
  {
     throw RedAlert("Programming error: XSparse::wildRange function, improper limits ordering.");
  }
  if (prevRange.first < maxRange.first)  prevRange.first = maxRange.first;
  if (prevRange.second > maxRange.second) prevRange.second = maxRange.second;

  RangePair outRange(0,0);
  int tmpBeg=0, tmpEnd=0;
  // this may throw
  oneRange(inputRange, tmpBeg, tmpEnd);
  if (tmpBeg < 0)
  {
     outRange.first = (tmpBeg == -2) ? maxRange.first : prevRange.first;
  }
  else
  {
     outRange.first = static_cast<size_t>(tmpBeg);
  }
  prevRange.first = outRange.first;
  if (tmpEnd < 0)
  {
     outRange.second = (tmpEnd == -2) ? maxRange.second : prevRange.second;
  }
  else
  {
     outRange.second = static_cast<size_t>(tmpEnd);
  }
  prevRange.second = outRange.second;
  if (outRange.first > outRange.second)
  {
     size_t tmp = outRange.first;
     outRange.first = outRange.second;
     outRange.second = tmp;
  }
  return outRange;
}

size_t XSparse::validateRangeSpecifier (const string& rangeString)
{
   std::string errMsg = std::string("range ") + rangeString + " contains invalid characters ";
   // Negative ints will fail the isInteger test.
   size_t rangeVal (XSutility::isInteger(rangeString));
   if(rangeVal == string::npos)
   {
	   throw InvalidRange(errMsg);		
   }
   else if (!rangeVal)
   {
      throw InvalidRange("0 is not a valid range specifier.");
   }
   return rangeVal;
}

IntegerArray XSparse::getRanges (const StringArray& inArgs, RangePair& prevRanges, const RangePair& rangeLimits)
{
  // This will return the UNION of all numbers covered by all the ranges
  // in inArgs, with no duplicate values.
  // Each individual string in inArgs should contain NO whitespace.  This is
  // not a problem if inArgs was built from Tcl's objv array.  Commas ARE
  // allowed at any position in strings. 
  typedef std::map<size_t,size_t> OrderedPairs;
  IntegerArray dummy;
  StringArray filteredArgs;
  // CollectParams will clear out any commas and will collate all into 
  // separate strings in filteredArgs (ie. "l,m,n" becomes "l","m","n".
  // In this context, we are not interested in the parameter numbers
  // returned, hence the name "dummy".  
  collectParams(inArgs, dummy, filteredArgs);
  size_t nRanges = filteredArgs.size();
  OrderedPairs orderedRanges;
  for (size_t i=0; i<nRanges; ++i)
  {
     const string& currString = filteredArgs[i];
     // wildRange may throw
     RangePair currRange = wildRange(currString, rangeLimits, prevRanges);
     if (currRange.first < rangeLimits.first || currRange.second > rangeLimits.second)
     {
        std::ostringstream msg;
        size_t val = (currRange.first < rangeLimits.first) ? currRange.first :
                        currRange.second;
        msg << "Range value " << val << " is outside the allowed range limits ("
           << rangeLimits.first << "-" << rangeLimits.second << ")";
        throw InvalidRange(msg.str());
     }
     // Insert each currRange into a map sorted by the low value.  Expanding
     // these pairs by their order in the map will then produce a sorted
     // selectedNums array.  Also need to check for foolish overlaps such
     // as 1-5, 3 or 1-5, 2-6.  We don't want duplicates in selectedNums array.

     // Note: I'm going through the trouble of sorting by range pairs, rather
     // than simply each number bracketed by the pair, because while the
     // number of range pairs will presumably always be small, a range itself
     // 1-n in theory could be quite enormous.
     size_t key = currRange.first;
     OrderedPairs::iterator itPairs = orderedRanges.find(key);
     if (itPairs == orderedRanges.end() || currRange.second > itPairs->second)
     {
        orderedRanges[key] = currRange.second;
     }
  }
  IntegerArray selectedNums;
  OrderedPairs::const_iterator itPairs = orderedRanges.begin();
  OrderedPairs::const_iterator itEnd = orderedRanges.end();
  OrderedPairs::const_iterator itLast = itEnd;
  if (orderedRanges.size())
  {
     --itLast;
     // This isn't a sure thing, but its a good guess at the total size 
     // needed for the selectedNums array.
     size_t guesstimate = itLast->second - itPairs->first + 1;
     if (guesstimate > 10)
        selectedNums.reserve(guesstimate);
  }
  size_t highest = 0;
  while (itPairs != itEnd)
  {
     size_t low = itPairs->first;
     size_t high = itPairs->second;
     size_t span = high - low + 1;
     if (low > highest)
     {
        for (size_t j=0; j<span; ++j)
        {
          selectedNums.push_back(static_cast<int>(low+j));
        }
        highest = high;
     }
     else if (high > highest)
     {
        span = high - highest; // Note: No +1 here.
        for (size_t j=1; j<=span; ++j)
        {
           selectedNums.push_back(static_cast<int>(highest+j));
        }
        highest = high;
     }
     // else range low-high has already been added to selectedNum, 
     // so do nothing.
     ++itPairs;
  }

  return selectedNums;
}

bool XSparse::promptRealTuple(const string& prompt, std::vector<Real>& vals, std::deque<string>* batchArgs)
{
  // For prompting input of the form "r1  r2", "r1,r2", "  ,r2 ... rn" etc.
  // where r1 - rn are Reals and n = vals.size().  vals should contain
  // default values upon input.  Any args beyond n as returned from prompt
  // will be ignored.  Function returns false if any invalid input is
  // found among the n vals.
   bool isValid = true;
   string input;
   std::ostringstream fullPrompt;
   const size_t nVals = vals.size();
   fullPrompt << prompt << std::showpoint << " (";
   for (size_t i=0; i<nVals; ++i)
   {
      fullPrompt << vals[i];
      if (i < nVals-1)
         fullPrompt << ", ";
   }
   fullPrompt << "): " << std::noshowpoint;    
   basicPrompt(fullPrompt.str(), input, batchArgs);
   if (input.length() && input.substr(0,1) != s_USE_DEFAULT)
   {
      StringArray rawArgs, outputArgs;
      IntegerArray iParams;
      collateByWhitespace(rawArgs, input);
      collectParams(rawArgs, iParams, outputArgs);
      size_t nEntered = std::min(iParams.size(), nVals);
      for (size_t i=0; i<nEntered && isValid; ++i)
      {
         size_t iPar = iParams[i];
         if (iPar >= nVals)  break;
         Real test;
         std::istringstream iss(outputArgs[i]);
         if (!(iss >> test) || !iss.eof())
         {
            isValid = false;
         }
         else
         {
            vals[iPar] = test;
         }
      }
   }      
   return isValid;
} // end promptRealTuple

string XSparse::stringSegment (const string& fullString, const size_t length, string::size_type* pStart)
{
   string segment;
   string::size_type& startPos = *pStart;
   const size_t totalLength = fullString.size();
   // Line breaks are treated differently than regular whitespace.
   const string WS(" \t");

   if (!length)
      return segment;
   if (startPos >= totalLength)
   {
      // Out of range, just return an empty string.
      startPos = string::npos;
      return segment;
   }

   string::size_type breakPos = fullString.find("\n",startPos);
   if (breakPos - startPos <= length)
   {
      // A line break occurs within the requested length (or
      // immediately after).  Return a substring up to
      // but not including the break, and reset startPos to 
      // 1 beyond the break.
      segment = fullString.substr(startPos, breakPos-startPos);
      startPos = breakPos + 1;
      if (startPos >= totalLength)
         startPos = string::npos;
   }
   else if (startPos + length >= totalLength)
   {
      // Simply return the remainder of fullString
      segment = fullString.substr(startPos);
      startPos = string::npos;
   }
   else if (WS.find(fullString[startPos+length-1]) != string::npos)
   {
      // Another easy case: the requested segment ends on whitespace
      segment = fullString.substr(startPos, length);
      startPos += length;
   }
   else
   {
      // The trickiest case: the segment ends on non-ws.  
      // If the following char is ws, return full segment.
      // Else, return up through the last ws BEFORE n=length chars.
      // If no ws, just return the full segment.
      if (WS.find(fullString[startPos+length]) != string::npos)
      {
         segment = fullString.substr(startPos, length);
         startPos += length;
      }
      else
      {
         string testStr(fullString.substr(startPos, length));
         string::size_type lastWSPos = testStr.find_last_of(WS);
         if (lastWSPos == string::npos)
         {
            segment = testStr;
            startPos += length;
         }
         else
         {
            segment = testStr.substr(0,lastWSPos+1);
            startPos += lastWSPos+1;
         }
      }
   }      
   return segment;
}

int XSparse::getCurlyBracketInt (const string& fullName, string& fileName)
{
   const string WS(" \t");
   const string errMsg(string("Invalid curly bracket syntax in filename: ")
                        + fullName + "\n");
   int intVal=0;
   string::size_type bracLoc = fullName.find_first_of('{');
   if (bracLoc == string::npos)
   {
      // Easy case, no brackets
      fileName = fullName;
   }
   else
   {
      // Some form of brackets are specified
      fileName = fullName.substr(0,bracLoc);

      // endLoc must be the last non-WS character in fullName.
      string::size_type endLoc = fullName.find_first_of('}',bracLoc);
      if (endLoc != fullName.find_last_not_of(WS)
                || endLoc == bracLoc+1)
         throw YellowAlert(errMsg);

      // Remove trailing whitespace inside brackets so eof() check
      // doesn't deliver a false hit.  Leading whitespace doesn't matter.
      string intStr(fullName.substr(bracLoc+1, endLoc-bracLoc-1));
      string::size_type lastPos = intStr.find_last_not_of(WS);
      if (lastPos != string::npos)
         intStr = intStr.substr(0,lastPos+1);
      std::istringstream iss(intStr);
      if (!(iss >> intVal) || !iss.eof())
         throw YellowAlert(errMsg);

      if (intVal < 0)
         throw YellowAlert(errMsg);
   }
   return intVal;
}

void XSparse::collateRanges (const StringArray& inArgs, string& prevName, std::map<string,StringArray>& rangeGroups)
{
   StringArray xsParams;
   // This context doesn't care about param numbers, hence...
   IntegerArray dummy;
   XSparse::collectParams(inArgs, dummy, xsParams);

   StringArray prevGroupArgs;
   bool isFirstNameSet = false;
   for (size_t i=0; i<xsParams.size(); ++i)
   {
      string groupName;
      string arg(xsParams[i]);
      string::size_type colonPos = arg.find(':');
      if (colonPos != string::npos)
      {
         if (colonPos == 0 || colonPos == arg.size() - 1)
            throw YellowAlert("Colon pair specification syntax error.\n");
         // OK, we know we have something of the form <blah1>:<blah2>, 
         // ie. something is immediately before and after colon.
         groupName = arg.substr(0, colonPos);
         arg = arg.substr(colonPos+1);
         isFirstNameSet = true;
      }

      if (!isFirstNameSet || groupName == prevName || !groupName.length())
      {
         prevGroupArgs.push_back(arg);
      }
      else
      {
         // Beginning a new name group, store the old one
         // into the StringArray map.
         if (prevGroupArgs.size())
         {
            // This does a map insert if prevName key isn't 
            // already there.
            StringArray& collectedVals = rangeGroups[prevName];
            for (size_t j=0; j<prevGroupArgs.size(); ++j)
               collectedVals.push_back(prevGroupArgs[j]);
         }
         prevGroupArgs.clear();
         prevName = groupName;
         prevGroupArgs.push_back(arg);
      }
   }
   if (prevGroupArgs.size())
   {
      // This does a map insert if prevName key isn't 
      // already there.
      StringArray& collectedVals = rangeGroups[prevName];
      for (size_t j=0; j<prevGroupArgs.size(); ++j)
         collectedVals.push_back(prevGroupArgs[j]);         
   }
}

bool XSparse::checkExtendedSyntax (const string& filename, string::size_type& squareLoc, string::size_type& plusLoc)
{
   bool isExtended = false;
   squareLoc = string::npos;
   plusLoc = string::npos;

   // leading or trailing whitespace is ignored
   const string::size_type endPos = filename.find_last_not_of(" \t");
   if (endPos != string::npos)
   {
      if (filename[endPos] == ']')
      {
         // If missing the matching '[' don't throw.
         // Some other function should perform that test.
         squareLoc = filename.find_last_of('[',endPos);
         if (squareLoc != string::npos)
            isExtended = true;
      }
      else
      {
         plusLoc = filename.find_first_of('+');
         if (plusLoc != string::npos && plusLoc < endPos)
         {
            if (XSutility::isInteger(filename.substr(plusLoc+1,endPos-plusLoc))
                        != string::npos)
               isExtended = true;
            else // not +<n>, reset plusLoc
               plusLoc = string::npos;
         }
      }
   }

   return isExtended;
}

void XSparse::pathToStack (const string& referencePath, std::stack<string>& pathStack)
{
   const string::size_type len = referencePath.length();
   if (len && referencePath[0] != '/')
   {
      string msg("absolute path names should start with \'/\'");
      msg += "\n";
      throw YellowAlert(msg);
   }
   string::size_type iPos = 1;
   while (iPos < len)
   {
      string::size_type endPos = referencePath.find_first_of('/',iPos);
      string dirName = referencePath.substr(iPos, endPos - iPos);
      pathStack.push(dirName);
      iPos = (endPos == string::npos) ? string::npos : endPos+1;         
   }
}

string XSparse::absToRelPath (const string& referencePath, const string& absPath)
{
   using namespace std;

   // This is based on a general algorithm described by Boby Thomas Pazheparampil (2006)
   // (as of Sep 2009 found at http://www.codeproject.com/KB/cpp/path_conversion.aspx)
   string relPath;
   const string UPDIR("../");
   stack<string> refStack;
   stack<string> absStack;
   stack<string> outStack;
   queue<string> matchQueue;

   pathToStack(referencePath, refStack);
   pathToStack(absPath, absStack);
   const size_t absSize = absStack.size();
   const size_t refSize = refStack.size();
   if (absSize > refSize)
   {
      const size_t nDiff = absSize-refSize;
      for (size_t j=0; j<nDiff; ++j)
      {
         outStack.push(absStack.top());
         absStack.pop();
      }
   }
   else if (absSize < refSize)
   {
      const size_t nDiff = refSize-absSize;
      for (size_t j=0; j<nDiff; ++j)
      {
         relPath += UPDIR;
         refStack.pop();
      }
   }

   // absStack and refStack are now the same size

   while (!absStack.empty())
   {
      if (absStack.top() == refStack.top())
      {
         // Add to match queue for safe keeping in case
         // there is a mismatch at higher levels.
         matchQueue.push(absStack.top());
      }
      else
      {
         while (!matchQueue.empty())
         {
            outStack.push(matchQueue.front());
            relPath += UPDIR;
            matchQueue.pop();
         }
         outStack.push(absStack.top());
         relPath += UPDIR;
      }
      absStack.pop();
      refStack.pop();
   }

   while (!outStack.empty())
   {
      relPath += outStack.top();
      outStack.pop();
      if (!outStack.empty())
         relPath += "/";
   }

   return relPath;
}

bool XSparse::isBlank (const char* charStr)
{
   const string WS(" \t\r\n");
   if (!charStr)
      return true;
   const string testStr(charStr);
   return (testStr.find_first_not_of(WS) == string::npos);
}

string::size_type XSparse::checkForRespPar(const string& parSpec)
{
   string::size_type rLoc = string::npos;
   
   const string::size_type colonPos = parSpec.find(':');
   if (colonPos == string::npos)
   {
      if (parSpec[0] == 'r' || parSpec[0] == 'R')
         rLoc = 0;
   }
   else
   {
      if (parSpec.length() > (colonPos+1) &&
           (parSpec[colonPos+1] == 'r' || parSpec[colonPos+1] == 'R'))
         rLoc = colonPos+1;
   }   
   return rLoc;
}

bool XSparse::validateForSystem(const string& input)
{
   bool isValid = true;
   static const string allowedChars("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789.-");
   for (size_t i=0; i<input.length() && isValid; ++i)
   {
      if (allowedChars.find(input[i]) == string::npos)
         isValid = false;
   }
   
   return isValid;
}

// Additional Declarations
