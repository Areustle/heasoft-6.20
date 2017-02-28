//
//  XSPEC12  November 2003
//
//
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Global/Global.h>
#include <XSFit/Fit/Fit.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/Parameter/Parameter.h> 
#include <XSModel/Parameter/ResponseParam.h>      
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Model/Model.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>
#include <XSUtil/Parse/XSRegEx.h>
#include <XSUser/UserInterface/TclRegEx.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <deque>
#include <sstream>
#include <set>

namespace {
   std::set<size_t> checkForMixingPars(string modName, int& nPars);
   string convertToRespID(const string& modName, int iPar);
}

int
XSGlobal::xsNewpar(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
    static string modelName;
    static IntegerArray paramRange;
    if (!paramRange.size())
    {
       paramRange.push_back(1);
    }
    try
    {
	if (objc == 1)
	{
	    printDocs(static_cast<char*>(cdata),"?");
	    return globalData->autoSave(TCL_OK);
	}   
	else
	{
	    string arg(Tcl_GetString(objv[1]));
	    if (arg[0] == '?') 
	    {
		printDocs(static_cast<char*>(cdata),"?");
		return globalData->autoSave(TCL_OK);
	    } 
	    else if ( arg[0] == '0') // newpar 0, backward compatibility
	    {
                char cmd[] = "show par";
		Tcl_Eval(tclInterp,cmd);
                char cmd2[] = "show fit";
                Tcl_Eval(tclInterp,cmd2);
		return globalData->autoSave(TCL_OK);
	    }     
	}

	int i=1;
        const bool isRespPar = (string(Tcl_GetString(objv[0])) == string("rnewpar"));

	string input;

	for(; i < objc; ++i)
	{
	    input += string(Tcl_GetString(objv[i]));
	    input += " ";
	}

	input.resize(input.length() - 1);
        doNewpar(input, isRespPar, modelName, paramRange);

	return globalData->autoSave(TCL_OK);
    }
    catch (YellowAlert&)
    {
	return globalData->autoSave(TCL_ERROR);       
    }
}

// This is declared in XSinterface.h to make available for external use.
void XSGlobal::doNewpar(const string& input, const bool isRespPar, string& modelName, IntegerArray& paramRange)
{
   using HandlerUtils::RegEx;
   string paramID;
   string rest;

   RegEx::result_type matches;
   RegEx eqnExp("^((?:\\w+:)?\\d+)\\s*=\\s*(.+)");

   if(eqnExp.regex_search(input, matches))
   {
       string strEqn = matches[2].str(), postfix;
       // In order to do analyzeInfix test, need to replace any
       // pars of format "name:int" with just a dummy integer.
       string tmpModified = RegEx("\\w+:\\d+").regSub(strEqn, "1");
       paramID = matches[1].str();
       // Remove all whitespace from right side of equation.
       // This makes the job much easier for the underlying
       // numeric expression analyzer later on.  
       tmpModified = RegEx("\\s+").regSub(strEqn, "");
       rest = "=" + tmpModified;
   }
   else
   {
       const string& real = RegEx::REAL();
       const string parRangeStr("(?:\\w+:)?\\d+(?:-\\d+)?");
       // parValStr definition includes a mandatory preceding
       // delimiter (" ,") followed by an optional Real, but
       // if no preceding comma then Real must be there.
       const string parValStr("(?:\\s*,\\s*(?:" + real + ")?"
                       + ")|(?:\\s+" + real + ")");                             
       RegEx rangeExp(string("^(?:(" + parRangeStr + ")?" 
                      + "((?:" + parValStr + ")" + "{0,6}))$"));	    
       string preBatch(input);
       string postBatch;
       const string WS(" \t");
       string::size_type firstAmp = input.find('&');
       if (firstAmp != string::npos)
       {
          // Also need to remove any whitespace that was immediately 
          // before the ampersand.
          string::size_type lastChar = 0;
          if (firstAmp > 0)
          {
             lastChar = input.find_last_not_of(WS, firstAmp-1);
             if (lastChar != string::npos)
             {
                preBatch = input.substr(0, lastChar+1);
             }
          }
          postBatch = input.substr(firstAmp);
       }
       if(!rangeExp.regex_search(preBatch, matches))
	   throw YellowAlert("Invalid syntax.\n");
       paramID = matches[1].str();
       rest = matches[2].str();
       // Passed initial validation, now restore batch args for use below.
       if (postBatch.length())
          rest += string(" ") + postBatch;

       // If a comma was used to delimit the range portion from the
       // par vals, remove it now.  (It would be the leading comma
       // in the "rest" string.) This is done because par vals handler
       // later on expects the 0th arg (as numbered by collectParams)
       // to refer to the parameter value.
       string::size_type firstCh = rest.find_first_not_of(WS);
       if (firstCh != string::npos)
       {
          if (rest[firstCh] == ',')
          {
             if (firstCh == rest.length()-1)
                rest.erase();
             else
                rest = rest.substr(firstCh+1); 
          }                                           
       }	
   }
   // At this point we have paramID which is either of the form
   // <rangeString>, modName:<rangeString>, or is empty.
   // If empty, paramRange retains its previous default values,
   // otherwise it is filled with the new range.        
   XSparse::stringRangePair(paramID,modelName,paramRange);
   // nPars must always be at least 1.
   const size_t nPars = paramRange.size();

   std::deque<string> parameterStrings;
   string paramArg;
   XSparse::findBatchString(rest,paramArg,parameterStrings);

   // paramArg should contain everything in "rest" to the left of 
   // the first "&".   Whatever it contains is applied to the first
   // and only the first parameter in param range.                       
   bool isChanged = false;
   size_t iRange = 0;
   int nParsInMod=0;
   std::set<size_t> mixParIndices;
   if (!isRespPar)
   {
      mixParIndices = checkForMixingPars(modelName, nParsInMod);        
      if (!nParsInMod)
      {
         throw YellowAlert("No parameters found in model.\n");
      }
   }

   if (paramArg.length())
   {
      try
      {
         if (isRespPar)
         {
            // modelName should actually be an int referring to sourceNum.
            // If it is not, getFromIntPair will throw.  If it is empty,
            // the following function will set it to "1:".
            string respParID = convertToRespID(modelName,paramRange[0]);
            int sourceNum=1;
            int parIdx=1;
            ResponseParam* respPar = HandlerUtils::getFromIntPair<ResponseParam>
                                           (respParID,sourceNum,parIdx);
            if (!respPar)
            {
               std::ostringstream errMsg;
               errMsg << "No response parameter "<<parIdx<<" for source "
                   <<sourceNum <<"\n";                    
               throw YellowAlert(errMsg.str());
            }
            // this may throw
            respPar->setValuesFromString(paramArg);
            Response* resp = respPar->responseParent();

            // This is a bit confusing, but setValuesFromString does NOT actually
            // APPLY the new values to the Response.  It merely modifies the
            // ResponseParam's values.  (This was originally designed as a 
            // 2-stage process for the gain fit option so that both
            // parameters could be verified before any gain was applied.)
            // Now to actually apply the changes...
            Real slope = resp->getLinearGain()->value('v');
            Real offset = resp->getConstGain()->value('v');
            resp->applyGainFromPrompt(slope,offset);

            isChanged = true;  
         }
         else
         {
            // Only allow access to mix parameters from 1st data group copy.
            int posInMod = (paramRange[0]-1) % nParsInMod;
            if (mixParIndices.find((size_t)posInMod+1) != mixParIndices.end()
                 && posInMod != paramRange[0]-1)
            {
               tcout << "Can only change mix parameters belonging to the lowest data group model."
                 << std::endl;
            } 
            else
            {         
	       XSContainer::models->setParameterFromPrompt(paramRange[0],modelName,paramArg);
	       isChanged = true;
            }
         }
         ++iRange;
      }
      catch (XSparse::SkipThis&) 
      {
         ++iRange;
      }
      catch (XSparse::AbortLoop&)
      {
         // This will cause it to forgo the nPars loop below.
	 iRange = nPars;
      }
   }
   size_t batchIdx = 0;
   for (size_t j = iRange; j < nPars; ++j)
   {
      paramArg.erase();
      try
      {
         if (batchIdx < parameterStrings.size())
         {
            // There is a batch string for this parameter.
            paramArg = parameterStrings[batchIdx++];
         }

         if (isRespPar)
         {
            string respParID = convertToRespID(modelName,paramRange[j]);
            int sourceNum=1;
            int parIdx=1;
            ResponseParam* respPar = HandlerUtils::getFromIntPair<ResponseParam>
                                           (respParID,sourceNum,parIdx);
            if (!respPar)
            {
               std::ostringstream errMsg;
               errMsg << "No response parameter "<<parIdx<<" for source "
                   <<sourceNum <<"\n";                    
               throw YellowAlert(errMsg.str());
            }
            if (!paramArg.length())
            {
               respPar->rePrompt(paramArg);
               if (paramArg == XSparse::SKIP())
               {
                  throw YellowAlert();
               }
            }
            respPar->setValuesFromString(paramArg);
            Response* resp = respPar->responseParent();
            Real slope = resp->getLinearGain()->value('v');
            Real offset = resp->getConstGain()->value('v');
            resp->applyGainFromPrompt(slope,offset);
            isChanged = true;  
         }
         else
         {
            int posInMod = (paramRange[j]-1) % nParsInMod;
            if (mixParIndices.find((size_t)posInMod) != mixParIndices.end()            
                 && posInMod != paramRange[j]-1)
            {
               tcout << "Can only change mix parameters belonging to the lowest data group model."
                 << std::endl;
            }
            else
            { 
               // If paramArg is empty for this par, they will be prompted.
	       XSContainer::models->setParameterFromPrompt(paramRange[j],
						      modelName,paramArg);
	       isChanged = true;
            }
         }
      }
      catch (XSparse::SkipThis&)
      {
	  continue;       
      }
      catch (XSparse::AbortLoop&)
      {
	  break;
      }
      catch (YellowAlert&)
      {
	  break;       
      }
   } // end nPars loop



   if (isChanged)
   {
      if (isRespPar)
      {
         XSContainer::datasets->Notify();
      }
      else
      {
         string modLookupName = modelName.empty() ? Model::DEFAULT() : modelName;
         // force computation of the models just edited.
         XSContainer::models->setCompute(modLookupName);
         Model* firstMod = XSContainer::models->lookup(modLookupName);
         if (!firstMod->isActive())
         {
            // For inactive models, need to initialize calculation
            // here since it won't be done through Fit::Update.
            std::vector<Component*> newComps;
            firstMod->bundleComponents(newComps);
            for (size_t i=0; i<newComps.size(); ++i)
            {
               newComps[i]->initializeForFit();
            }
            XSContainer::models->calculate(modLookupName);
         }
	 XSContainer::fit->Update();
      }
   }
   tcout << std::flush;
}
// end doNewpar function

namespace {

   std::set<size_t> checkForMixingPars(string modName, int& nPars)
   {
      // Deliberately not passing a string reference so we can modify
      // name in here without it sticking.
      if (modName.empty())
         modName = Model::DEFAULT();
      std::set<size_t> mixParIndices;
      std::vector<Model*> mods = XSContainer::models->lookupModelGroup(modName);
      const size_t nCopies = mods.size();
      if (nCopies)
      {
         nPars = static_cast<int>(mods[0]->numberOfParameters());         
         const XSContainer::MixLocations& mixLocs=mods[0]->mixingLocations();
         if (mixLocs.first.size() || mixLocs.second.size())
         {
            std::vector<Component*> modComps;
            mods[0]->bundleComponents(modComps);
            // Mix comps:
            for (size_t i=0; i<mixLocs.first.size();++i)
            {
               const Component* mixComp = modComps[mixLocs.first[i]];
               const std::vector<Parameter*>& mixPars=mixComp->parameterSet();
               for (size_t j=0; j<mixPars.size(); ++j)
               {
                  mixParIndices.insert(mixPars[j]->index());
               }
            }
            // AMX comps:
            for (size_t i=0; i<mixLocs.second.size();++i)
            {
               const Component* mixComp = modComps[mixLocs.second[i]];
               const std::vector<Parameter*>& mixPars=mixComp->parameterSet();
               for (size_t j=0; j<mixPars.size(); ++j)
               {
                  mixParIndices.insert(mixPars[j]->index());
               }
            }
         }
      }
      return mixParIndices;
   }


   string convertToRespID(const string& modName, int iPar)
   {
      // Helps convert the "square peg" of [<modName>:]<range> as originally
      // returned from stringRangePair function call, to an int pair arg
      // for the "round hole" getFromIntPair<ResponseParam> function.

      // In this context, modName should actually be a sourceNum.  
      // NO CHECKING as to whether it is actually an integer.  That
      // mistake must be caught elsewhere.  An empty string defaults to
      // sourceNum = 1.  

      std::ostringstream intPair;
      if (modName.length())
         intPair << modName << ":" << iPar;
      else
         intPair << "1:" << iPar;
      return intPair.str();      
   }


}
