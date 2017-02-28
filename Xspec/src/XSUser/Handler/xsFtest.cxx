//
//  XSPEC12  November 2003
//
//

#include <XSFit/Fit/Fit.h>
#include <XSUser/Global/Global.h>  
#include <XSUser/Global/XSGlobal.h>  
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Numerics/Beta.h>
#include <XSContainer.h>
#include <XSsymbol.h>
#include <XSstreams.h>
#include <sstream>
#include <iomanip>

int
XSGlobal::xsFtest(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doFtest(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doFtest(const StringArray& rawArgs)
{
   Real ftest=0.0;
   const size_t nArgs = rawArgs.size();
   const char* cmd = "ftest";
   if (nArgs <= 1)
   {
      printDocs(cmd,"?");
      return 0;
   }
   StringArray args(nArgs-1);
   StringArray valStrs;
   IntegerArray iParams;
   for (size_t i=1; i<nArgs; ++i)
      args[i-1] = rawArgs[i];
   XSparse::collectParams(args, iParams, valStrs);
   
   const size_t nPars = 4;
   if (valStrs.size() < nPars)
   {
      printDocs(cmd,"?");
      if (valStrs.size() && valStrs[0] == "?") 
      {
         return 0;
      }
      else return -1;
   }
   size_t j=0;
   bool isOK = true;
   Real vals[nPars]={.0,.0,.0,.0};
   while (isOK && j < nPars)
   {
      std::istringstream iss(valStrs[j]);
      Real tmp=.0;
      if (!(iss >> tmp) || !iss.eof())
         isOK = false;
      else
      {
         vals[j] = tmp;
         ++j;
      }
   }
   if (isOK)
   {
      Real& chi2 = vals[0];
      Real& dof2 = vals[1];
      Real& chi1 = vals[2];
      Real& dof1 = vals[3];
      if ( chi2 <= 0 || dof1 <= dof2)
      {
         tcerr << "Bad values for F-Test: \n";
         if ( chi2 <= 0 ) tcerr << "\t Chi2 = " << chi2 << '\n';
         if (dof1 <= dof2) tcerr << "\t dof1 <= dof2 \n";       
         if (dof2*dof1 == 0) tcerr << "\t  degrees of freedom are zero: "
                         << " 1: " << dof1 << " 2: " << dof2 << '\n';     
         return -1;
      }
      Real val = (chi1 - chi2)/(dof1 - dof2)/(chi2/dof2);
      ftest = Numerics::betaI(dof2/2.,(dof1-dof2)/2.,
                      (dof2/(dof2 + (dof1 - dof2)*val)));
      Fit::lastFtest(ftest);
      tcout << " F statistic value = " << val
            << " and probability " << ftest << std::endl;

   }
   else
   {
      tcerr << "Bad arguments to F-Test: requires 4 numerical values.\n";
      return -1;
   }

   return 0;
}
