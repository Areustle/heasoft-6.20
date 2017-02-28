#include <tcl.h>

#ifdef __WIN32__
int _export Powtcl_Init (interp)
#else
int Powtcl_Init (interp)
#endif
    Tcl_Interp *interp;		/* Interpreter for application. */
{
  return Pow_Init(interp);
}
