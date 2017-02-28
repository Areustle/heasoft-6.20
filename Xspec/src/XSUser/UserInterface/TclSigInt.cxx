//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// TclSigInt
#include <XSUser/UserInterface/TclSigInt.h>


// Class TclSigInt 

TclSigInt::TclSigInt()
  : SIGINT_Handler()
{
   m_sig_Token = Tcl_AsyncCreate(TclSigInt::handleTclSafe, NULL);
}


TclSigInt::~TclSigInt()
{
   Tcl_AsyncDelete(m_sig_Token);
}


int TclSigInt::handleSignal (int sigNum)
{
    Tcl_AsyncMark(m_sig_Token);
    return 0;
}

int TclSigInt::handleTclSafe (ClientData cdata, Tcl_Interp* xsInterp, int code)
{
   if (xsInterp == NULL)
   {
      return TCL_OK;
   }
   else
   {
      return code;
   }
}

// Additional Declarations
