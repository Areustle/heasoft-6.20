/* 
 * Driver routine for XSPEC C++ code with TCL command interpreter
 *
 * 
 * Modified 2/99 for signal handling, C++ compatibility, Tk usage
 * Keith Arnaud, Ben Dorman
 *
 * Programs to load Xspec commands into TCL interpreter.  Should
 * also allow for dynamic loading of XSPEC into an existing TCL
 * package.
 */

/*
 * cfortran prototypes for the needed fortran calls in Integral_Init.
 */

 
// #define     XS_START()  CCALLSFSUB0(XS_START, xs_start)

#include "SPI_Data.h"
#include "SPI_Response.h"
#include <XSUser/UserInterface/xstcl.h>
#include <XSModel/DataFactory/XspecRegistry.h>
#include <XSModel/DataFactory/DataFactory.h>
#include <XSModel/Data/BackCorr/OGIP-92aBackground.h>
#include <XSContainer.h>

extern "C" int Integral_Init(Tcl_Interp* tclInterp); 
  
#include <iostream>


/*
 * Integral_Init -- main XSPEC initialization routine.
 */
int
Integral_Init(Tcl_Interp *tclInterp)                
{
   static char INTEGRAL[] = {"Integral"};
   static char VERSION[] = {"1.0"};
   XSContainer::xsRegistry->addToRegistry(new DataPrototype(new SPI_Data, new SPI_Response, new OGIP_92aBackground, new OGIP_92aCorrection));   
   return Tcl_PkgProvide(tclInterp,INTEGRAL,VERSION);
}
