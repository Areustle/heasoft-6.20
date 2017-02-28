/*
// xrrtexternal.hh
//
// Definitions for external functions to access ray tracing library
// Richard L Fink GSFC/631
// 1997/05/20
// 1998/04/08 Ning Gan RSTX
//            Add the test for predefined macro __cpluscplus 
//            Add the C-style comment in order to be called by C routine. 
//            (some  C- compiler do not accept C++ style comment.)    
// 1999/02/26 Add support for Version 2.1 CALDB and Astro-E scattering R Fink
//
// 2008/03/02 Y.ISHISAKI	version 6.5.0
//	add xrrtgetversion(), xrrtgetpname(), xrrtgetcredit()
//	remove xrrtascascatter(), xrrtastroescascatter()
//	modify arguments of xrrtloadmirror()
*/

#ifndef XRRTEXTERNAL_HH
#define XRRTEXTERNAL_HH

#include "xrrt_types.hh"
#include "fitsio.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
// EXTERNAL CALLING FUNTIONS FOR C AND F77
*/

/*
// IF YOU WISH TO USE THE CALDB OR RUN-TIME REFLECTION TABLES READ THIS
// BEFORE CALLING xrrtloadmirror
//     1) When using the CALDB to access the default Mirror file
//        Call xrrtgettdf to get the Mirror file name
//     2) When using the CALDB to access the default Reflection Table file
//        Call xrrtgetreflect to get the Reflection Table file name
//     3) When using the CALDB AND you want to calculate Reflection Tables
//        at run time
//        Call xrrtsetruntimereflect to set the mirror defaults
//     4) When NOT using the CALDB and you want to calculate Reflection Tables
//        at run time
//        Call xrrtsetruntimereflect to set the mirror defaults
//        Call xrrtsetatomicfiles to set the Atomic Data file names
*/

/*
// xrrtgetversion returns version string stored in a static region.
*/
char *xrrtgetversion(void);

/*
// xrrtgetpname returns XRRT-VERSION string stored in a static region.
*/
char *xrrtgetpname(void);

/*
// xrrtgetcredit returns credit string stored in a static region.
*/
char *xrrtgetcredit(void);

/*
// xrrtloadmirror loads the mirror fragment and obstruction fragment
//      descriptions from the XRT description FITS file. It also
//      loads the required reflection tables from the reflection
//      table FITS file. 
//      mirrorfile - the filesystem path and file name of the FITS file
//               that contains the mirror description
//      reflectfile - the filesystem path and file name of the FITS file
//               that contains the reflection tables
//      status  - RETURNED error code for the call
// Add the loading collimator fits file for ASTRO-E2 Pre-Collimator
// (added by H. Mori)
//      collimatorfile - the filesystem path and file name of the FITS file
//               that contains the collimator description
*/
void xrrtloadmirror(char* mirrorFile, 
                    char* mirrorExt,
                    char* obstructExt,
                    char* quadrantExt,
		    char* pcolExt,
                    char* reflectFile, 
		    char* backprofFile,
                    int* status);

/*
// xrrttracephoton is the external interface routine that traces
//      a photon with the given conditions thru the mirrors and
//      returns the trace result.
//      energy - photon energy in keV
//      radius - photon distance from mirror center in MM
//      angle  - photon rotation angle around mirror
//      unitradial - direction vector in the radial direction for the photon
//      unittheta  - direction vector in the theta (rotational) direction
//      unitz      - direction vector in the z (telescope axis) direction
//      fplanex - RETURNED x location if the photon hit the focal plane in
//                units of MM
//      fplaney - RETURNED y location if the photon hit the focal plane in
//                units of MM
//      returncode - RETURNED status of the photon tracing; its end result
//      status  - RETURNED error code for the photon trace in case the
//                trace internally failed for some reason
*/
void xrrtracephoton(double* energy, 
                    double* radius,
                    double* angle,
                    double* unitradial, 
                    double* unittheta, 
                    double* unitz,
                    double* fplanex, double* fplaney,
                    int* returncode, 
                    int* status);

/*
// xrrtscattermode allows the caller to override the scattering mode
//      for the mirror fragments as specified in the XRT description
//      FITS file. 
//      mode   - Default mode used to override the scatter modes found
//               in the mirror description FITS file
//      status  - RETURNED error code for setting the mode
*/
void xrrtscattermode(int* mode, int* status); 

/*
// xrrtsetatomicfiles allows you to set absolute names for the Atomic Data
//     Files when using CALCULATED run time Reflection Tables.
//     Normally, the ray tracing will automatically look in the CALDB
//     for the appropriate files. If the CALDB is not installed or the files
//     have not been cataloged, this will cause an error. Use this function
//     to pass the appropriate file names BEFORE calling xrrtloadmirror.
*/
void xrrtsetatomicfiles(char* atomicdatafile, char* atomicscatterfile);

/*
//  xrrtgettdf returns the name of the Telescope Description File for
//      the given mission and telescope. The mission name and telescope
//      name must match the case and exact form of the names stored in the
//      CALDB configuration file. This routine only returns the current
//      file with times "NOW".
//      Be sure that caldbfile is large enough to hold any possible return
//      from the CALDB
*/
void xrrtgettdf(char* missionname, char* telescopename, 
              char* caldbfile, int* status);

/*
//  xrrtgetreflect returns the name of the Reflection Table file
//      the given mission and telescope. The mission name and telescope
//      name must match the case and exact form of the names stored in the
//      CALDB configuration file. This routine only returns the current
//      file with times "NOW".
//      Be sure that caldbfile is large enough to hold any possible return
//      from the CALDB
*/
void xrrtgetreflect(char* missionname, char* telescopename, 
              char* caldbfile, int* status);

/*
//  xrrtgetatomicdatafile return the name of the Atomic Data file from the
//      CALDB. 
//      Be sure that atomicdatafile is large enough to hold any possible return
//      from the CALDB.
*/
void xrrtgetatomicdatafile(char* atomicdatafile, int* status);

/*
//  xrrtgetatomscatfile return the name of the Atomic Scatter Factors file 
//      from the CALDB.
//      Be sure that atomscatfile is large enough to hold any possible return
//      from the CALDB.
*/
void xrrtgetatomscatfile(char* atomscatfile, int* status);

/*
// xrrtsetruntimereflect sets the defaults for run time calculation of 
//     reflection tables.
//     frontsurface should contain the chemical formula ("Pt" for example)
//     frontdensity should contain the CGS density (21.46 g/cc)
//     frontrough   should contain the surface roughness (0.0)
//     backsurface  should contain the chemical formula ("Al" for example)
//     backdensity  should contain the CGS density (2.7 g/cc)
//     backrough    should contain the surface roughness (0.0)
//
*/
void xrrtsetruntimereflect(char*   frontsurface, 
                           double* frontdensity,
                           double* frontrough,
                           char*   backsurface,
                           double* backdensity,
                           double* backrough,
                           int*    status);
                         
/*
//  xrrtwritefitskeywords provides a pointer to an open FITS file via a
//      CFITSIO pointer with the FITS file at a valid HDU. This function
//      then dumps all the ray tracing meta information available as 
//      FITS keywords to the file.
*/
void xrrtwritefitskeywords(fitsfile* fitsFilePtr, int* status);

#ifdef __cplusplus
}
#endif

#endif
