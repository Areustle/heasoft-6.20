//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/XSGlobal.h>        
#include <XSUser/Global/Global.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSstreams.h>
#include <xsTypes.h>

int
XSGlobal::xsTime(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doTime(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int 
XSGlobal::doTime(const StringArray& rawArgs)
{
   static const Real M6 = 1.E-06;
   static const Real ZERO = 0;

   struct rusage* present = new struct rusage;
   const struct rusage* initTime = globalData->initTime();
   const struct rusage* previousTime = globalData->currentTime();

   Real r0user =  initTime->ru_utime.tv_sec + M6*initTime->ru_utime.tv_usec ;
   Real r0sys  =  initTime->ru_stime.tv_sec + M6*initTime->ru_stime.tv_usec ;

   Real r1user =  previousTime->ru_utime.tv_sec + M6*previousTime->ru_utime.tv_usec ;
   Real r1sys  =  previousTime->ru_stime.tv_sec + M6*previousTime->ru_stime.tv_usec ;

   getrusage(RUSAGE_SELF,present);

   Real r2user =  present->ru_utime.tv_sec + M6*present->ru_utime.tv_usec ;
   Real r2sys  =  present->ru_stime.tv_sec + M6*present->ru_stime.tv_usec ;

   Real totalElapsedUser = std::max(r2user - r0user,ZERO);
   Real totalElapsedSystem  = std::max(r2sys - r0sys,ZERO);

   Real lastElapsedUser = std::max(r2user - r1user,ZERO);
   Real lastElapsedSystem  = std::max(r2sys - r1sys,ZERO);

   globalData->currentTime(present);

   tcout.precision(5);
   tcout << " Time (s) elapsed since last call: User - \t" << lastElapsedUser 
                                             << " System - \t" << lastElapsedSystem << '\n';
   tcout << " Time (s) elapsed since startup:   User - \t" << totalElapsedUser 
                                             << " System - \t" << totalElapsedSystem 
                                             << std::endl;



   delete present;

   return 0;
}
