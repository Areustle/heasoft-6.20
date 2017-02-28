#include <XSUser/Handler/HandlerUtils.h>
#include <XSFit/Fit/Fit.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Parameter/Parameter.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSContainer.h>
#include <fstream>
#include <XSUtil/Parse/XSRegEx.h>
#include <XSUser/UserInterface/TclRegEx.h>
#include <stack>

const std::string CmdRetrievalTraits<Parameter>::reqString =
      string("parameter number <n> or <modelName>:<n>");
const std::string CmdRetrievalTraits<Parameter>::objName = string("parameter");

const std::string CmdRetrievalTraits<Component>::reqString =
      string("component number <n> or <modelName>:<n>");
const std::string CmdRetrievalTraits<Component>::objName = string("component");

const std::string CmdRetrievalTraits<Response>::reqString = 
      string("<spectrum number> or <source number>:<spectrum number>");
const std::string CmdRetrievalTraits<Response>::objName = string("response");

const std::string CmdRetrievalTraits<ResponseParam>::reqString = 
      string("<response param number> or <source number>:<response param number>");
const std::string CmdRetrievalTraits<ResponseParam>::objName = 
      string("response parameter");

RangePair HandlerUtils::FreezeThawRange::s_range(1,0);
string HandlerUtils::FreezeThawRange::s_groupName(string(""));

Parameter* LookupPolicy<Parameter>::get(const string& name, size_t num)
{
  Parameter* p = XSContainer::models->lookupParameter(num, name);
  return p;
} 

Model* LookupPolicy<Model>::get(const string& name, size_t num)
{
   string useName = name;
   if (!name.length())
   {
      useName = Model::DEFAULT();
   }
   // For now, this is only able to access models/components from
   // the first data group.
   Model* mod = XSContainer::models->lookup(useName);
   return mod;
} 

Component* LookupPolicy<Component>::get(const string& name, size_t num)
{
   Component* comp=0;
   Model* mod = LookupPolicy<Model>::get(name, num);
   if (mod)
   {
      int dummyGroup=0, dummyOffset=0;
      comp = mod->componentByNumber(num, dummyGroup, dummyOffset);        
   } 
   return comp;
}

Response* LookupPolicy<Response>::get(const size_t defSource, const size_t defSpec,
                        size_t& sourceNum, size_t& specNum)
{
   size_t defaults[2];
   defaults[0] = defSource;
   defaults[1] = defSpec;
   size_t tmpNum[2];
   tmpNum[0] = sourceNum;
   tmpNum[1] = specNum;
   std::vector<string> item(2);
   item[0] = "source";
   item[1] = "spectrum";
   size_t maxRange[2];
   maxRange[0] = XSContainer::datasets->numSourcesForSpectra();
   maxRange[1] = XSContainer::datasets->numberOfSpectra();
   // Don't allow a proper exit from loop until sourceNum and specNum are 
   // both in valid range.  Entering a '/*' though will cause an
   // exit by exception.
   for (size_t i=0; i<2; ++i)
   {
      bool isValid = false;
      while (!isValid)
      {
         string repromptedStr;
         if (tmpNum[i] > 0 && tmpNum[i] <= maxRange[i])
         {
            // Done.
            isValid = true;
         }
         else
         {
            tcout << "The value of " << item[i] << " number, " << tmpNum[i]
               << ", is outside the allowed range (1," 
               << maxRange[i] << ")" << std::endl;
            bool haveInt = false;
            while (!haveInt)
            {
               std::ostringstream  prompt;
               prompt  << "Please re-enter " << item[i] << " number ("
                  << defaults[i] <<"):"; 
               XSparse::basicPrompt(prompt.str(), repromptedStr);
               if (!repromptedStr.length() || repromptedStr == XSparse::USE_DEFAULT())
               {
                  tmpNum[i] = defaults[i];
                  haveInt = true;
               }
               else
               {
                  std::istringstream iss(repromptedStr);
                  if ((iss >> tmpNum[i]) && iss.eof())
                     haveInt = true;
               }
            }         
         } // end if not valid
      } // end while !isValid
   } // end source/spec loop

   // The spectrum is guaranteed to exist, but there doesn't have to be a Response
   // at the requested sourceNum.
   sourceNum = tmpNum[0];
   specNum = tmpNum[1];
   Response* resp = XSContainer::datasets->lookup(specNum)->detector(sourceNum-1);
   return resp;
}

ResponseParam* LookupPolicy<ResponseParam>::get(size_t sourceNum, size_t parNum)
{
   using namespace XSContainer;
   ResponseParam* respPar=0;
   const RespParContainer& respPars = responses->respParContainer();
   if (sourceNum && sourceNum <= respPars.size())
   {
      if (parNum && parNum <= respPars[sourceNum-1].size())
      {
         respPar = respPars[sourceNum-1][parNum-1];
      }
   }
   return respPar;
}

ResponseParam* LookupPolicy<ResponseParam>::get(const size_t defSource, const size_t defPar,
                                size_t& sourceNum, size_t& parNum)
{
   return LookupPolicy<ResponseParam>::get(sourceNum, parNum);
}

ResponseParam* LookupPolicy<ResponseParam>::get(const string& name, size_t num)
{
   size_t sourceNum = string::npos;
   if (name.empty())
      sourceNum = 1;
   else
   {
      // ASSUME name syntax has already been checked and that it is an integer.
      std::istringstream iss(name);
      iss >> sourceNum;
   }
   return LookupPolicy<ResponseParam>::get(sourceNum, num);
}


IntegerArray RangeFindPolicy<Parameter>::getIndices(const string& modName, 
                        const StringArray& rangeStrs, RangePair& prevRange)
{
   using namespace XSContainer;
   string modelName = modName.length() ? modName : Model::DEFAULT();

   // safety check...
   Model* m(models->lookup(modelName));
   if (!m)
   {
      string errMsg("Invalid Model Name in parameter specification: ");
      if (modelName == Model::DEFAULT())
         modelName = string("<unnamed>");
      errMsg += modelName + "\n";
      throw YellowAlert(errMsg);
   }
   RangePair allowedRange(1,1);
   size_t nDataGroups = m->isActive() ? 
                  datasets->getNumberOfGroupsForSource(m->sourceNumber()) : 1;
   allowedRange.second = nDataGroups*m->numberOfParameters();
   if (prevRange.second == 0)
          prevRange.second = allowedRange.second;
   IntegerArray parIndices = XSparse::getRanges(rangeStrs,
          prevRange, allowedRange);

   return parIndices;
}

IntegerArray RangeFindPolicy<ResponseParam>::getIndices(const string& sourceName, 
                        const StringArray& rangeStrs, RangePair& prevRange)
{
   using namespace XSContainer;
   size_t sourceNum=0;
   // sourceName should be an integer, but assume it hasn't been checked yet.
   if (sourceName.empty())
      sourceNum = 1;
   else
      sourceNum = XSutility::isInteger(sourceName);
   if (sourceNum == string::npos || sourceNum == 0)
   {
      string errMsg("Invalid source number specifier: ");
      errMsg += sourceName + '\n';
      throw YellowAlert(errMsg);
   }
   const RespParContainer& respPars = responses->respParContainer();
   if (sourceNum > respPars.size())
   {
      std::ostringstream oss;
      oss << "Source number out of range: " << sourceNum << "\n";
      throw YellowAlert(oss.str());
   }
   size_t nPars = respPars[sourceNum-1].size();
   if (!nPars)
   {
      std::ostringstream oss;
      oss << "No responses parameters for source " << sourceNum << "\n";
      throw YellowAlert(oss.str());
   }
   RangePair allowedRange(1,nPars);
   if (prevRange.second == 0)
      prevRange.second = allowedRange.second;
   IntegerArray parIndices = XSparse::getRanges(rangeStrs,
          prevRange, allowedRange);

   return parIndices;
}



void HandlerUtils::tclArgsToCpp(const int objc, Tcl_Obj* CONST objv[], StringArray& cppArgs)
{
   cppArgs.resize(objc);   
   for (int i=0; i<objc; ++i)
      cppArgs[i] = string(Tcl_GetString(objv[i]));
}

bool HandlerUtils::getSpecNumParameter(const StringArray& rawArgs, const string& opt, 
                        size_t argNum, size_t& specNum)
  {
     using namespace XSContainer;        
     size_t nLoaded = datasets->numberOfSpectra();
     if (!nLoaded)
     {
        tcerr << "***XSPEC Error: No spectra are loaded: Unable to process  " << opt 
           << " option" << std::endl;
        return false;
     }
     if (rawArgs.size() < argNum)
     {
       // default to spectrum 1
        specNum = 1;
     }
     else
     {
        string arg(rawArgs[argNum-1]);
        specNum = XSutility::isInteger(arg);
        if (specNum == XSparse::NOTFOUND())
        {
           tcerr << "***XSPEC Error: Argument for spectrum number is not an integer"
               << std::endl;
           return false;
        }
        if (specNum == 0)
        {
           tcerr << "***XSPEC Error: 0 is not a valid spectrum number." 
                << std::endl;
           return false;
        }
        if (specNum > nLoaded)
        {
           string  isare = nLoaded > 1 ? string("are") : string("is");
	   tcerr << "***XSPEC Error: Invalid spectrum number:  Only " << nLoaded
	         << " spectra " << isare << " loaded." << std::endl;
	   return false;
        }
     }
     return true;
  }

 const Model* HandlerUtils::getModFromArg(const StringArray& rawArgs)
 {
    string modelName = (rawArgs.size() < 3) ? string("") : rawArgs[2];
    return getModFromName(modelName);
 }

 const Model* HandlerUtils::getModFromName(const string& name)
 {
    using namespace XSContainer;
    const Model* pModel=0;
    string useName = name.length() ? name : Model::DEFAULT();
    ModelMapConstIter im = models->modelSet().find(useName);
    if (im != models->modelSet().end())
    {
       pModel = im->second;
    } 
    else
    {
       if (useName == Model::DEFAULT())
       {
          tcerr << "No model found with the default name.  If looking for a loaded named\n"
              << "  model, its name must be specified." << std::endl;
       }
       else
       {
          tcerr << "No model found named " << name << std::endl;
       }
    }
    return pModel;     
 }

bool HandlerUtils::subExpMatched (const Tcl_RegExpInfo& regExpInfo, const string& search, int index, std::string& group)
{
    bool success = false;

    if(index <= regExpInfo.nsubs)
    {
	long 
	    start = ((regExpInfo.matches) + index)->start,
	    end   = ((regExpInfo.matches) + index)->end;

	int matchLength = end - start;

	if(matchLength > 0 && (success = true))
	    group = search.substr(start, matchLength);
    }

    return success;
}

bool HandlerUtils::subExpMatched (const Tcl_RegExpInfo& regExpInfo, const string& search, const IntegerArray& indices, std::map<int, std::string >& group)
{
    string match;

    int size = indices.size();

    group.clear();

    for(int i = 0; i < size; ++i)
	if(subExpMatched(regExpInfo, search, indices[i], match))
	    group[indices[i]] = match;

    return !group.empty();
}

bool HandlerUtils::compileAndMatch(const string& pattern, const string& search, const string& whole, Tcl_RegExpInfo& expInfo)
{
    bool success = true;

    Tcl_Interp* error = Tcl_CreateInterp();
    Tcl_Obj* stringObj = Tcl_NewStringObj(pattern.c_str(), pattern.size());
    Tcl_IncrRefCount(stringObj);
    Tcl_RegExp tclRegExp = 
	Tcl_GetRegExpFromObj(error, 
			     stringObj,
			     TCL_REG_ADVANCED);
    Tcl_DecrRefCount(stringObj);
    if(!tclRegExp && !(success = false))
	tcout << error->result << std::endl;
    else
    {
	const char *ptrSearch = search.c_str();
        // This is a patch to make sure the same pointer is sent for both
        //  args when 'search' equals the 'whole'.  Tcl_RegExpExec does a
        //  internal shallow-copy comparison to determine if they are equal,
        //  yet we're passing deep copies from IgnoreNoticeParse.  We've
        //  been getting away with it (until an obscure case on Mavericks)
        //  presumably because of compiler optimiziation using the same
        //  memory for 'search' and 'whole' in the calling function.  
        //  (The interface to this function should really be changed to
        //  take C-pointers instead of strings.)
        const char *ptrWhole = (search == whole) ? ptrSearch : whole.c_str();

	if(Tcl_RegExpExec(0, tclRegExp, ptrSearch, ptrWhole) > 0)
	    Tcl_RegExpGetInfo(tclRegExp, &expInfo);
	else
	    success = false;
    }
    Tcl_DeleteInterp(error);

    return success;
}

bool HandlerUtils::fillArrays(const Tcl_RegExpInfo& expInfo, const std::string& arg, std::vector<Real> first, std::vector<Real> second, std::vector<Real>& _default)
{
    IntegerArray subs(expInfo.nsubs);

    for(int i = 0; i < expInfo.nsubs; ++i)
	subs[i] = i + 1;

    return fillArrays(expInfo, arg, first, second, _default, subs);
}

bool HandlerUtils::fillArrays(const Tcl_RegExpInfo& expInfo, const std::string& arg, std::vector<Real>& first, std::vector<Real>& second, std::vector<Real>& _default, const IntegerArray& subs)
{
    string strGroup;

    bool hasWild = false;

    for(int j = 1; j <= static_cast<int>(subs.size()); ++j)
    {
	int idx = (j - 1) % 2;

	if(subExpMatched(expInfo, arg, subs[j - 1], strGroup))
	{
	    std::istringstream input(strGroup);

	    if(strGroup == "*" && (hasWild = true))
		(j <= 2 ? first[idx] : second[idx]) = _default[j - 1];
	    else if(strGroup == "**")
		(j <= 2 ? first[idx] : second[idx]) = -2;
	    else
		(input >> (j <= 2 ? first[idx] : second[idx])).seekg(0) >> _default[j - 1];
	}
	else
	    //I know it looks funny: (j - 1) - 1, but I'm just making it
	    //clear that since j starts at 1, (j - 1) - 1 subscripts 
	    //the previous array element
	    (j <= 2 ? first[idx] : second[idx]) = _default[j - 1];
    }

    return hasWild;
}

void HandlerUtils::IgnoreNoticeParse(const StringArray& args, std::vector<Real>& prevRanges, bool& isReal, bool& isRespChanged, bool& isChanged, bool value)
{
    std::vector<Real> newSpecRange(2), newChanRange(2);

    string specRegExp = "(\\d+|\\*{1,2})(?:-(\\d+|\\*{1,2}))?";
    string chanRegExp("((?:(\\.\\d+)|\\d+(\\.\\d*)?)([eE](?:-|\\+)?\\d*)?|\\*{1,2})");
    string fullReg("^(?:" + specRegExp + ":|(?=.+))(?:" + chanRegExp + "(?:-" + chanRegExp + ")?)?$");

    // This section is a patch fix put in to specifically handle the
    // case where the user leaves whitespace after the colon.  If the
    // following arg does not have colon, we'll assume it's the channel range they intended,
    // and append it immediately after the colon.  
    StringArray patchedArgs;
    for (size_t i=0; i<args.size(); ++i)
    {
       const string& inArg = args[i];
       // We know inArg.size() != 0
       if (inArg[inArg.size()-1] == ':')
       {
          string patchedArg = inArg;
          if (i < args.size()-1)
          {
             const string& nextArg = args[i+1];
             if (nextArg.find(':') == string::npos)
             {
                patchedArg += nextArg;
                ++i;
             }
          }
          patchedArgs.push_back(patchedArg);
       }
       else
          patchedArgs.push_back(inArg);       
    }


    //subs corresponds to the groups that will contain the 4 parts
    //of the range we want. arrDot is the group indicies where a 
    //dot can match. This will be tested later to see if the range
    //consists of real numbers or not.
    IntegerArray::value_type 
	subs[] = { 1, 2, 3, 7 }, 
	arrDot[] = { 4, 5, 6, 8, 9, 10 };
    IntegerArray vDotIdx(arrDot, arrDot + 6);

    Tcl_RegExpInfo expInfo;

    for (size_t iArg=0; iArg<patchedArgs.size(); ++iArg)
    {  
       string arg = patchedArgs[iArg];
       string search = arg;
       if (compileAndMatch(fullReg, search, arg, expInfo))
       {
	   try
	   {
	       IntegerArray vSubs,	idx(subs, subs + 4);

	       std::map<int, string> groups;
               bool noChansEntered = false;
	       for(size_t i = 0; i < idx.size(); i += 2)
	       {
		   if(subExpMatched(expInfo, search, 
				    IntegerArray(idx.begin() + i, idx.begin() + i + 2), groups) 
		      && groups.size() == 1)
		   {
		       vSubs.insert(vSubs.end(), 2, idx[i]);
		       if(groups.begin()->second == "*")
			   prevRanges[i] = prevRanges[i + 1] = -2;
		   }
		   else
		   {
                      // If in here, either 0 or 2 of the range limits
                      // have been found.
		       vSubs.insert(vSubs.end(), idx.begin() + i, idx.begin() + i + 2);
                       if (groups.empty() && i >= 2)
                          noChansEntered = true;
		   }

		   groups.clear();
	       }

	       bool wildInRange = fillArrays(expInfo, search, newSpecRange, 
					     newChanRange, prevRanges, vSubs);

	       subExpMatched(expInfo, search, vDotIdx, groups);

	       isReal = (!groups.empty() || (isReal && (wildInRange||noChansEntered)));

	       //this obvious thing to do here instead of saving/converting all this
	       //data, would be to change the definition of the various setChannel
	       //functions and add a parameter indicating whether the range is real,
	       //for instance the bool 'isReal,' above? But that would be too 
	       //tedious right now.
	       IntegerArray spectra(2);
	       spectra[0] = (int)newSpecRange[0];
	       spectra[1] = (int)newSpecRange[1];

	       if(isReal)
	       {

		   std::pair<Real, Real> realChanRange;
		   realChanRange.first = newChanRange[0];
		   realChanRange.second = newChanRange[1];

		   XSContainer::datasets->setChannels(value, realChanRange, spectra);
		   isChanged = true;

		   //values could have changed in setChannels
		   prevRanges[2] = realChanRange.first;
		   prevRanges[3] = realChanRange.second;
	       }
	       else
	       {
		   IntegerArray channels(2);

		   channels[0] = (int)newChanRange[0];
		   channels[1] = (int)newChanRange[1];
                   // Handle case of inadvertant channel = 0 here.
                   if (channels[0]==0) channels[0] = 1;
                   if (channels[1]==0) channels[1] = 1;

		   XSContainer::datasets->setChannels(value, channels, spectra);
                   isChanged = true;

		   prevRanges[2] = (Real)channels[0];
		   prevRanges[3] = (Real)channels[1];
	       }

	       prevRanges[0] = (Real)spectra[0];
	       prevRanges[1] = (Real)spectra[1];

	       isRespChanged = XSContainer::datasets->resetDetectors(spectra);
	   }
	   catch (XSparse::InvalidRange)
	   {
	       // rethrow invalid ranges so that the loop
	       // terminates. 
	       throw;
	   }                                        
	   catch (YellowAlert&)
	   {
	       // any other xspec errors, continue to next
	       // range.
	       // result: all ranges and spectra prior too
	       // the exception are executed, and all
	       // complete ranges beyond the one where
	       // the exception was thrown.
	       continue;
	   }

       }
       else
       {
          tcerr << "***Xspec Error: Invalid ignore/notice string: " 
                << arg << std::endl;
          return;
       }
    }
}

bool HandlerUtils::fileExists(const string& fileName)
{
    using namespace std;

    bool exists = true;
    fstream test(fileName.c_str(), ios_base::in);
    if (!test)
       exists = false;


    return exists;
}

bool HandlerUtils::analyzeInfix(const string& infix, string& postfix) 
{
    //this table enforces precedence rules between operators.
    //refer to 'Tanenbaum, Andrew S., Structured Computer 
    //Organization, Prentice-Hall, 1999' for more info
    static const int action[6][7] = {  { 2, 2, 1, 1, 1, 1, 2 },
				       { 2, 2, 1, 1, 1, 1, 2 },
				       { 2, 2, 2, 2, 1, 1, 2 },
				       { 2, 2, 2, 2, 1, 1, 2 },
				       { 2, 2, 2, 2, 2, 1, 2 },
				       { 1, 1, 1, 1, 1, 1, 3 } };

    std::map<char, int> chart_pos;

    chart_pos['+'] = 0;
    chart_pos['-'] = 1;
    chart_pos['*'] = 2;
    chart_pos['/'] = 3;
    chart_pos['@'] = 4;       //symbol we'll use to represent unary minus
    chart_pos['('] = 5;
    chart_pos[')'] = 6;

    bool error = false, lastWasGroup = false;

    postfix = "";

    const string& real = RegEx::REAL();

    RegEx regex("^(?:(" + real + ")?\\s*((?:[*/(+-])(?!\\s*[*/)+])|\\))\\s*|(" + real + "))");
    RegEx::result_type matches;

    std::stack<char> op_stack;

    std::string::const_iterator
	infix_beg = infix.begin(),
	infix_end = infix.end();

    while(!error && infix_beg != infix_end && 
	  regex.regex_search(infix_beg, infix_end, matches)) 
    {
	bool digitMatched = true;

	if(matches[1].matched)
        {
	    postfix += matches[1].str();
            postfix += ' ';
        }
	else if(matches[5].matched)
        {
	    postfix += matches[5].str();
            postfix += ' ';
        }
	else
	    digitMatched = false;

	if(matches[4].matched)
	{
	    char scanned = matches[4].str()[0];

	    bool unaryPlus = false;

	    //is the operator a unary '+' or '-'
	    if(string("+-").find(scanned) != string::npos 
	       && !digitMatched && !lastWasGroup)
		//discard the unary '+'
		(scanned == '-' ? scanned = '@' : unaryPlus = true);

	    if(!unaryPlus)
	    {
		int col = chart_pos[scanned], to_do;

		bool done = false;

		do 
		{
		    if(op_stack.size() > 0)
		    {
			int row = chart_pos[op_stack.top()];
			to_do = action[row][col];

			lastWasGroup = false;

			switch(to_do)
			{
			case 1:
			    op_stack.push(scanned);
			    break;
			case 2:
			    postfix += op_stack.top();
                            postfix += ' ';
			    op_stack.pop();
			    break;
			case 3:
			    op_stack.pop();
			    lastWasGroup = true;
			    break;
			}
		    }
		    else if(scanned == ')') error = true;
		    else 
		    {
			op_stack.push(scanned);
			done = true;
		    }
		} while(!done && !error && to_do == 2);
	    }
	}

	infix_beg = matches[0].second;
    }

    if(infix_beg != infix_end) error = 1;

    if(!error)
    {
	char ch;

	//pop any remaining op_stack operators
	while(!error && op_stack.size() > 0)
	{
	    if((ch = op_stack.top()) == '(') 
		error = 1;
	    else {
		postfix += ch;
                postfix += ' ';
		op_stack.pop();
	    }
	}
    }

    return !error;
}

void HandlerUtils::commonStepParsing(const StringArray& rawArgs, Grid::ParameterSpec*& prevSettings,
         Grid::SpecContainer& newStepSettings, bool& isBest, bool needModParam)
{
    // If ANYTHING is found wrong with ANY of the entries, ALL memory
    // pointed to by newStepSettings will be deleted and the array
    // will be cleared, prevSettings will be restored to its
    // initial value, and isBest will be undefined.
    const size_t nArgs = rawArgs.size();
    Grid::ParameterSpec * const savePrevSettings = prevSettings;
    std::vector<string> args(nArgs-1,"");
    IntegerArray inputIndex;
    StringArray inputString;
    string strArgString;

    for (size_t j = 1; j < nArgs; ++j)
    {
	args[j-1] = rawArgs[j];
	strArgString += args[j - 1] + ' ';
    }

    static string parIDString("1");

    XSparse::collectParams(args,inputIndex,inputString);  

    // we now have a parsed list of strings inputString
    // indexed by an indirect address array inputIndex.

    const string& real = RegEx::REAL();

    string delim = "(?:\\s*,\\s*|\\s+)";
    string dblock = "(?:" + delim + "|$)";

    // Total of 10 captured subexpressions since RegEx::REAL()
    // contains 2 internal subexpressions.
    string exp;
    exp += "^(?:(?:[bB]\\w*|[cC]\\w*)" + dblock + ")?"; //matches best or current
    exp += "(?:(?:[lL]\\w*|[nN]\\w*)" + dblock + ")?"; //matches log or nolog
    exp += "((?:\\w+:)?r?\\d+)?" + dblock;    //matches [modelname:]param_number
    exp += "(" + real + "|[dD]\\w*)?" + dblock;  //matches range min or delta
    exp += "(" + real + ")?" + dblock;        //matches range max
    exp += "(\\d+)?" + dblock;                //matches # of steps

    RegEx regex(exp);
    RegEx::result_type matches;


    IntegerArray paramIndex;
    StringArray paramString;

    isBest = Grid::retrieveBestSetting(inputString,inputIndex,
				       paramString,paramIndex);

    // okay, now we are dealing only with parameter specifiers.
    StringArray::iterator  is = paramString.begin();
    StringArray::iterator isEnd = paramString.end();
    IntegerArray::iterator  ii = paramIndex.begin();

    // since steppar can set multiple parameters, we need an index
    // to tell the argument count to start again every time we finish
    // one. baseIndex will point into paramIndex.
    int baseIndex(0);
    int flags = 0;

    string::const_iterator
	args_beg = strArgString.begin(),
	args_end = strArgString.end();

    bool success = false;
    try
    {
        while(is != isEnd)
	{
	    success = regex.regex_search(args_beg, args_end, matches, flags);
	    if(!success) {
	      string errMsg(" Error processing step parameter args, check syntax: \'");
	      errMsg += strArgString;
	      errMsg += "\'";
	      throw Grid::InvalidParameter(errMsg);
	    } 

	    // okay the first argument is either a string denoting
	    // log/nolog or a parameter specifier. Can do the following
	    // with random access iterators only.
	    //baseIndex = *ii;

	    ModParam* mp(0);

            int parIndex = -1;
	    int fullParIndex = -1;
	    string parameterName;

	    if(matches[1].matched)
	    {
		string s(matches[1].str());
		if(s.length())
		    parIDString = s;
	    }

	    if (needModParam)
	    {
		Step::retrieveParameter(XSContainer::fit, parIDString, mp, parIndex, fullParIndex, parameterName);
	    }
	    else
	    {
		// NOTE: In this case we're not interested in retrieving an
		// actual parameter object, just a string:int ID.
		// Therefore parameterName will actually be the model name.
		size_t tmpIdx=0;
		XSparse::stringIntPair(parIDString, parameterName, tmpIdx);
		parIndex = static_cast<int>(tmpIdx);
	    }

	    // now read the next set of arguments until
	    // there is enough for a new parameter spec or
	    // to reset the old.
	    bool psDone = false, ls = false, isDelta = false;
	    int levels = 10;
	    std::pair<Real,Real> limits(Grid::FLAG(), Grid::FLAG());

	    if (prevSettings)
	    {
		limits.first = prevSettings->lowRange;
		limits.second = prevSettings->highRange;
		levels = prevSettings->intervals; 
		ls     = prevSettings->log;      
	    }

	    if ( is != isEnd )
	    {              
		do
		{

		    psDone = Grid::parseParameterRange
		      (*is,*ii,baseIndex,limits,levels,ls,isDelta);
		    ++is, ++ii;       
		}  while (is != isEnd && !psDone);
	    }

	    // if isDelta is false then limits contains the range required
	    // and levels the number of iterations. if isDelta is true then
	    // limits.second contains the delta value and levels the number
	    // of steps on each side of the best fit. in this second case
	    // we need to convert to the first case.

	    if ( isDelta ) {
	      ModParam* param = XSContainer::fit->variableParameters(fullParIndex);
	      Real current = param->value('v');
	      Real delta = limits.second;
	      limits.first = current - delta*levels;
	      limits.second = current + delta*levels;
	      levels = 2*levels + 1;
	    }

	    Grid::ParameterSpec* newPar =
		Grid::makeParameterSpec(prevSettings, parameterName,
				parIndex,fullParIndex,limits,ls,levels);
	    newPar->address = mp;
            // Late addition: add optional units string in case needed
            // for steppar contour plot's label.
            if (mp)
               newPar->units = mp->unit();
            else
               newPar->units.clear();

	    prevSettings = newPar;
	    newStepSettings.push_back(newPar);

	    args_beg = matches[0].second;

	    baseIndex += 4;
	}  //end while loop
    }
    catch (...)
    {
	prevSettings = savePrevSettings;
	for (size_t i=0; i<newStepSettings.size(); ++i)
	{
	    delete newStepSettings[i];
	}
	newStepSettings.clear();
	throw;
    }                   
}

bool HandlerUtils::CompareSource::operator() (Model* right, Model* left)
{
    return right->sourceNumber() < left->sourceNumber();
}
