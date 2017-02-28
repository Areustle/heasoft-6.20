#include <xsTypes.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <XSContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h> 
#include <XSUtil/Parse/XSparse.h>

#include <sstream>

int
XSGlobal::xsAddsource(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   static RangePair prevRange(1,0);
   using namespace XSContainer;
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
      }
      size_t nSpectra = datasets->numberOfSpectra();
      if (!nSpectra)
      {
         tcout << "No spectra are loaded - unable to add sources." << std::endl;
         return globalData->autoSave(TCL_OK);
      }
      // Need to determine if first arg specifies a sourceNum
      // but calling collectParams is overkill at this point,
      // particularly since it gets called from getRanges
      // further below.
      int sourceNum = 0; 
      string firstArg(Tcl_GetString(objv[1]));
      string beforeComma(XSparse::returnDelimitedArgument(firstArg, ","));
      if (beforeComma.length())
      {
         string beforeColon(XSparse::returnDelimitedArgument(beforeComma,":"));
         if (beforeColon.length())
         {
            std::istringstream iss(beforeColon);
            if (!(iss >> sourceNum) || !iss.eof() || sourceNum < 1)
            {
               string errMsg("Invalid or missing source number entered");
               errMsg += " prior to ':'\n";
               throw YellowAlert(errMsg);
            }
         }
      }
      StringArray inArgs;
      if (beforeComma.length())
         inArgs.push_back(beforeComma);
      // firstArg now contains whatever was after the comma
      if (firstArg.length())
         inArgs.push_back(firstArg);
      for (size_t i=2; i<objc; ++i)
      {
         inArgs.push_back(string(Tcl_GetString(objv[i])));
      }
      RangePair allowedRange(1,nSpectra);
      if (prevRange.second == 0)
         prevRange.second = allowedRange.second;
      IntegerArray specNums = XSparse::getRanges(inArgs, prevRange, allowedRange);

      // If a sourceNum is not specified at the start, then simply
      // add 1 additional source onto each given spectrum. 

      if (!sourceNum)
      {
      }
      else
      {
      }      
   }
   catch (YellowAlert&)
   {
      return globalData->autoSave(TCL_ERROR);
   }
   return globalData->autoSave(TCL_OK);
}
