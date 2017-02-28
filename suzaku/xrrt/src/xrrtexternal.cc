// xrrtexternal.cc
//
// External functions for calling the ray trace library from C
//
// Richard L Fink GSFC/631
// 1997/05/20
// 1998/05/11 Richard L Fink GSFC/631 
//            Integrate Ning Gan changes to eliminate 
//            Reference params and make other
//            changes to support calling from "C".
// 1999/02/26 Richard L Fink GSFC/631
//            Add support for Version 2.1 Astro-E scattering and CALDB access.
//
// 2005/12/20 Y.ISHISAKI	version 6.2.3
//   add scatterIndexFile in xrrtastroescatter()
//   call setScatterIndexFileName, setAstroeBackParams in xrrtastroescatter()
//
// 2005/12/20 Y.ISHISAKI	version 6.2.4
//   argument collimatorExtention -> collimatorExtension in xrrtloadmirror()
//   string collimatorExtension -> collimatorExt in xrrtloadmirror()
//
// 2006/07/17 Y.ISHISAKI	version 6.4.1
//   make xrrtgettdf(), xrrtgetreflect(), xrrtgetatomicdatafile(),
//   xrrtgetatomscatfile() obsolete, always (1000 + caldbError)

#include <string>
#include "xrrtexternal.hh"
#include "xrrt_types.hh"
#include "xrrtraytrace.hh"

char *
xrrtgetversion(void)
{
	static char version[16];
	sprintf(version, "%d.%d.%d",
			XRRT_VERSION_MAJOR, XRRT_VERSION_MINOR, XRRT_VERSION_REVISION);
	return version;
}

char *
xrrtgetpname(void)
{
	static char pname[32];
	sprintf(pname, "%s-%s", XRRT_NAME, xrrtgetversion());
	return pname;
}

char *
xrrtgetcredit(void)
{
	static char credit[64];
	sprintf(credit, XRRT_CREDIT,
			XRRT_VERSION_MAJOR, XRRT_VERSION_MINOR, XRRT_VERSION_REVISION);
	return credit;
}

void
xrrtscattermode(int* mode, int* status)
{
// 
// Set all mirror scatters to be handled by a fixed override mode
// Note that the override mode is not checked. If it is invalid,
// an error will trip later on.
//

	theRaytraceInfo().setOverrideScatterMode(*mode);
	*status = NO_ERROR;
}

// Modified to make use of the Astro-E2 pre-collimator
// (modified for the pre-Collimator missplacement by 
//  Hideyuki MORI : date 2002/08/16)
// (modified for the pre-collimator reflection table by 
//  Hideyuki MORI : date 2003/01/14)
// (modified for the quadrant infomations by Hideyuki MORI : date 2006/03/04)
void
xrrtloadmirror(char* mirrorFile,
			   char* mirrorExt,
			   char* obstructExt,
			   char* quadrantExt,
			   char* pcolExt,
			   char* reflectFile,
			   char* backprofFile,
			   int* status)
{
//
// Load the telescope definition file
// Returns:
// STATUS
//     0 = OK
//     1- 999 FITSIO error code
//     1000-  1000 + XrrtRayTraceErrorCode
//

	try {
        theRaytraceInfo().loadMirrorDesc(mirrorFile, mirrorExt,
										 obstructExt, quadrantExt, pcolExt,
										 reflectFile, backprofFile);
	} catch (int errorCode) {
		*status = errorCode;
		return;
	} catch (XrrtRayTraceErrorCode errorCode) {
		*status = errorCode + 1000;
		return;
	}

	*status = NO_ERROR;
}


// Ning Gan converted energy, radius, angle to pointers although they should 
// not be modified by the called code.
void
xrrtracephoton(double* energy,
               double* radius,  double* angle,
               double* unitradial,   double* unittheta, double* unitz,
               double* fplanex, double* fplaney,
               int* returncode, int* status)
{
//
// Ray trace a photon from the given parameters
//
// Returns:
// RETURNCODE
//     The photon status codes from XrrtPhoton
// STATUS
//     0 = OK
//     1000-  1000 + XrrtRayTraceErrorCode
//

	double zDistance = theStructure().getLayerDistance(0);
	theRaytraceInfo().initPhoton(*energy, *radius, *angle, zDistance,
								 *unitradial, *unittheta, *unitz);
	//
	// Preset the returns
	//
	*returncode = 0;
	*fplanex = 0;
	*fplaney = 0;
	//
	// Do the ray trace if possible
	//
	try {
		*returncode = theRaytraceInfo().rayTrace();
	} catch (XrrtRayTraceErrorCode errorCode) {
		*status = errorCode + 1000;
		return;
	}
	//
	// If no error occured; set the focal plane hit location
	//
	if ( PHOTON_HIT_FOCAL_PLANE == *returncode ) {
        thePhoton().getXY(fplanex, fplaney);
	}

	*status = NO_ERROR;
}

// obsolete, always (1000 + caldbError)
void 
xrrtgettdf(char* missionname, char* telescopename,
           char* caldbfile, int* status)
{
	*status = 1000 + caldbError;
	return;
#if 0
// Error returns in status XrrtRayTraceErrorCode + 1000
// missingCALDBVariable
// caldbError 
	string missionName = missionname;
	string telescopeName = telescopename;
	string tdfFileName;
	try  {
		tdfFileName = theRaytraceInfo().findTDFByMission(missionName, telescopeName);
		*status = 0;
		strcpy(caldbfile, tdfFileName.c_str());
	} catch (XrrtRayTraceErrorCode error) {
		*status = error + 1000;  // 1-999 reserved for FITSIO
		caldbfile[0] = '\0';
	}
	return;
#endif
}

// obsolete, always (1000 + caldbError)
void
xrrtgetreflect(char* missionname, char* telescopename,
			   char* caldbfile, int* status)
{
	*status = 1000 + caldbError;
	return;
#if 0
// Error returns in status XrrtRayTraceErrorCode + 1000
// missingCALDBVariable
// caldbError 
	string missionName = missionname;
	string telescopeName = telescopename;
	string reflectFileName;
	try {
		reflectFileName = 
		theRaytraceInfo().findTDFByMission(missionName, telescopeName);
		*status = 0;
		strcpy(caldbfile, reflectFileName.c_str());
	} catch (XrrtRayTraceErrorCode error) {
		*status = error + 1000;  // 1-999 reserved for FITSIO
		caldbfile[0] = '\0';
	}
	return;
#endif
}

void 
xrrtsetatomicfiles(char* atomicdatafile, char* atomicscatterfile)
{
	string atomicScatterFileName = atomicscatterfile;
	string atomicInfoFileName    = atomicdatafile;
	theRaytraceInfo().setAtomicDataFileNames(atomicScatterFileName,
											 atomicInfoFileName);
	return;
}

// obsolete, always (1000 + caldbError)
void
xrrtgetatomicdatafile(char* atomicdatafile, int* status)
{
	*status = 1000 + caldbError;
	return;
#if 0
// Error returns in status XrrtRayTraceErrorCode + 1000
// missingCALDBVariable
// caldbError
	string atomicDataFileName;
	try {
		atomicDataFileName = theRaytraceInfo().findAtomDataFileName();
		strcpy(atomicdatafile, atomicDataFileName.c_str());
		*status = 0;
	} catch (XrrtRayTraceErrorCode error) {
		*status = error + 1000;  // 1-999 reserved for FITSIO
		atomicdatafile[0] = '\0';
	}
	return;
#endif
}

// obsolete, always (1000 + caldbError)
void
xrrtgetatomscatfile(char* atomscatfile, int* status)
{
	*status = 1000 + caldbError;
	return;
#if 0
// Error returns in status XrrtRayTraceErrorCode + 1000
// missingCALDBVariable
// caldbError
	string atomScatFileName;
	try {
		atomScatFileName = theRaytraceInfo().findAtomScatFactorFileName();
		strcpy(atomscatfile, atomScatFileName.c_str());
		*status = 0;
	} catch (XrrtRayTraceErrorCode error) {
		*status = error + 1000;  // 1-999 reserved for FITSIO
		atomscatfile[0] = '\0';
	}
	return;
#endif
}

void 
xrrtsetruntimereflect(char*   frontsurface,
					  double* frontdensity,
                      double* frontrough,
                      char*   backsurface,
                      double* backdensity,
                      double* backrough,
                      int*    status)
{
// Error returns in status XrrtRayTraceErrorCode + 1000
// invalidChemicalFormula

//
// Create the chemical molecules for the front and back mirror surfaces
//
	XrrtMolecule* frontSurfacePtr = new XrrtMolecule;
	XrrtMolecule* backSurfacePtr  = new XrrtMolecule;
//
// Set them to vaild formulas
//
	string formula = frontsurface;
	try {
		frontSurfacePtr->createMolecule(formula);
	} catch (XrrtMoleculeErrorCode error) {
		*status = invalidChemicalFormula + 1000; // Reserve 1-999 for FITSIO
		return;
	}
	formula = backsurface;
	try {
		backSurfacePtr->createMolecule(formula);
	} catch (XrrtMoleculeErrorCode error) {
		*status = invalidChemicalFormula + 1000; // Reserve 1-999 for FITSIO
		return;
	}
	string frontTableName = "Primary Surface";
	string backTableName  = "Secondary Surface";
	theRaytraceInfo().defineInternalReflectionTables(frontTableName,
													 frontSurfacePtr,
													 *frontdensity,
													 *frontrough,
													 backTableName,
													 backSurfacePtr,
													 *backdensity,
													 *backrough);
	*status = 0;
	return;
}

void 
xrrtwritefitskeywords(fitsfile* fitsFilePtr, int* status)
{
	try {
		theRaytraceInfo().writeHistoricalKeywords(fitsFilePtr);
	} catch (int fitsioStatus) {
		*status = fitsioStatus;
	}
	return;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C++ ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
