// xrrtraytrace.cc
//
// Member functions for XrrtRaytrace class
//
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/25 Upgrade documentation. R. Fink
// 1999/02/03 Added split obstructions by Telescope Quadrant to increase
//            ray tracing speed. R Fink

// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 2.1  2000/11/09 07:46:38  mori
// Add loading collimator description file in loadMirrorDesc()
// Using mirror code for collimator section
// Probably more modification is needed in this file and derived
// other source file
//
// Revision 1.1  2000/10/19 06:32:45  mori
// Initial revision
//
// 2005/12/15 Y.ISHISAKI	version 6.2.2
//    change string& -> string in setScatterIndexFileName()
//
// 2005/12/24 Y.ISHISAKI	version 6.2.4
//    check if precollimatorFileName == "none" in loadMirrorDesc()
//    check if precollimatorReflectFileName == "default" in loadMirrorDesc()
//    print reading messages in loadMirrorDesc()
//    close fitsCollimatorFile in loadMirrorDesc()
//
// 2006/07/17 Y.ISHISAKI	version 6.4.1
//    findAtomScatFactorFileName, findAtomDataFileName move to caldb.cc
//
// 2007/04/06 Y.ISHISAKI	version 6.4.5
//	call reflectTable->setTableIndex() in XrrtRaytrace::loadReflect()
//
// 2007/05/07 Y.ISHISAKI	version 6.4.5
//	allocate scatProbArray only if EXTNAME='AEFront' or SCATPROB column exists
//
// 2008/03/02 Y.ISHISAKI	version 6.5.0
//	read mirrorThickness, precollimatorThickness, etc from CALDB headers
//	find scattering parameters from CALDB headers
//	set scatter mode parameters and read backproffile in loadMirrorDesc()
//	show the XRRT X-Ray Ray-Tracing library credit in loadMirrorDesc()
//	include "xrrtdefaults.hh", "xrrtexternal.hh"
//	strcmp -> strcasecmp("AEFront", extname) in loadReflect()
//	add functions, show_header_card(), upcase_string()
//	upcase string in columns in freflect, breflect in mirror file
//	upcase string of EXTNAME keyword in reflect file
//
// 2009/01/08 Y.ISHISAKI	version 6.5.2
//  check if null obstruction in obstructionLayerTrace()

#include "xrrtraytrace.hh"
#include "xrrtdefaults.hh"
#include "xrrtexternal.hh"
#include <fstream>

//
// Show header keyword to stdout
//
static int
show_header_card(fitsfile *fp, string key)
{
	char card[FLEN_CARD];

	int istat = 0;

	fits_read_card(fp, (char *)key.c_str(), card, &istat);
	if ( istat ) {
		return istat;
	}
	fflush(NULL);
	printf("%s\n", card);
	fflush(NULL);

	return 0;
}

static char *
upcase_string(char *s)
{
	int i;
	char c;

	for (i = 0; '\0' != s[i]; i++) {
		c = s[i];
		if ( 'a' <= c && c <= 'z' ) {
			s[i] = c - 'a' + 'A';
		}
	}

	return s;
}

XrrtRaytrace::XrrtRaytrace():
	defaultPhoton(thePhoton()),
	defaultReflection(theReflectionInfo()),
	defaultScatterInfo(theScatterInfo()),
	raytraceChatter(0),
	overrideCALDBAtomicFiles(false),
	atomicScatterFactorsFileName("atomScatterFactor.fits"),
	atomicDataFileName("atomicData.fits"),
	dynamicReflectionTable(false),
	frontSurfaceTableName(),
	frontSurface(0),
	frontSurfaceCGSDensity(0),
	frontSurfaceRoughness(0),
	backSurfaceTableName(),
	backSurface(0),
	backSurfaceCGSDensity(0),
	backSurfaceRoughness(0),
	frontReflectTables(),
	frontRemap(),
	backReflectTables(),
	backRemap(),
	pcolReflectTables(),
	pcolRemap(),
	structure(theStructure()),
	//
	// For the quadrant information
	// (added by Hideyuki MORI : date 2005/12/17)
	//
	quadrant(theQuadrant()),
	telescopeDefined(false),
	photonDefined(false),
	scatterNoneFound(false),
	scatterAscaFound(false),
	scatterAstroeFound(false),
	assumedFocalPlaneZ(0),
	minimumTelescopePhotonRadiusMM(0),
	maximumTelescopePhotonRadiusMM(0)
{
// A simple constructor
}

XrrtRaytrace&
theRaytraceInfo()
{
//
// Returns static ray trace info for XRRT
//
	static XrrtRaytrace raytraceInfo;
	return raytraceInfo;
}

string
XrrtRaytrace::errorMessage(const XrrtRayTraceErrorCode& errorCode)
{
//
// Convert error codes to error messages
//
	string errorMessage;

	switch (errorCode) {
	case ASCA_PARAMS_PREVIOUSLY_SET:
		errorMessage = "\
Attempt to set ASCA scatter params once already set";
		break;
	case noMirrorExtension:
		errorMessage = "\
The requested mirror description extension could not be found";
		break;
	case noObstructionExtension:
		errorMessage = "\
The requested obstruction description extension could not be found";
		break;
	case missingReflectionTables:
		errorMessage = "\
A reflection table could not be found";
		break;
	case noFitsLayerCol:
		errorMessage = "\
Column layer could not be found in the mirror extension";
		break;
	case noFitsAssemblyCol:
		errorMessage = "\
Column assembly could not be found in the mirror extension";
		break;
	case noFitsNumberCol:
		errorMessage = "\
Column number could not be found in the mirror extension";
		break;
	case noFitsFragmentCol:
		errorMessage = "\
Column fragment could not be found in the mirror extension";
		break;
	case noFitsFunctionCol:
		errorMessage = "\
Column function could not be found in the mirror extension";
		break;
	case noFitsScatterCol:
		errorMessage = "\
Column scatter could not be found in the mirror extension";
		break;
	case noFitsFrelectCol:
		errorMessage = "\
Column freflect could not be found in the mirror extension";
		break;
	case noFitsBreflectCol:
		errorMessage = "\
Column breflect could not be found in the mirror extension";
		break;
	case noFitsFstartCol:
		errorMessage = "\
Column fstart could not be found in the mirror extension";
		break;
	case noFitsScrossCol:
		errorMessage = "\
Column scross could not be found in the mirror extension";
		break;
	case noFitsFendCol:
		errorMessage = "\
Column fend could not be found in the mirror extension";
		break;
	case noFitsEcrossCol:
		errorMessage = "\
Column ecross could not be found in the mirror extension";
		break;
	case noFitsTopinrCol:
		errorMessage = "\
Column topinr could not be found in the mirror extension";
		break;
	case noFitsTopoutrCol:
		errorMessage = "\
Column topoutr could not be found in the mirror extension";
		break;
	case noFitsBotinrCol:
		errorMessage = "\
Column botinr could not be found in the mirror extension";
		break;
	case noFitsBotoutrCol:
		errorMessage = "\
Column botoutr could not be found in the mirror extension";
		break;
	case noFitsTopdCol:
		errorMessage = "\
Column topd could not be found in the mirror extension";
		break;
	case noFitsBotdCol:
		errorMessage = "\
Column botd could not be found in the mirror extension";
		break;
	case noFitsPlayerCol:
		errorMessage = "\
Column layer could not be found in the obstruction extension";
		break;
	case noFitsPolynumCol:
		errorMessage = "\
Column polynum could not be found in the obstruction extension";
		break;
	case noFitsDistanceCol:
		errorMessage = "\
Column distance could not be found in the obstruction extension";
		break;
	case noFitsXvertexCol:
		errorMessage = "\
Column xvertex could not be found in the obstruction extension";
		break;
	case noFitsYvertexCol:
		errorMessage = "\
Column yvertex could not be found in the obstruction extension";
		break;

// Add the cases of missing columns for the quadrant informations
// in the quadrant extension
// (added by Hideyuki MORI : date 2006/03/04)
	case noFitsQuadrantCol:
	    errorMessage = "\
Column quadrant could not be found in the quadrant extension";
	    break;
	case noFitsQlayerCol:
		errorMessage = "\
Column quadrant layer could not be found in the quadrant extension";
	    break;
	case noFitsDeltaxCol:
		errorMessage = "\
Column deltax could not be found in the quadrant extension";
	    break;
	case noFitsDeltayCol:
		errorMessage = "\
Column deltay could not be found in the quadrant extension";
	    break;
	case noFitsDeltazCol:
		errorMessage = "\
Column deltaz could not be found in the quadrant extension";
	    break;
	case noFitsDeltaTxCol:
		errorMessage = "\
Column deltaTx could not be found in the quadrant extension";
	    break;
	case noFitsDeltaTyCol:
		errorMessage = "\
Column deltaTy could not be found in the quadrant extension";
	    break;
	case noFitsDeltaTzCol:
		errorMessage = "\
Column deltaTz could not be found in the quadrant extension";
	    break;
	    // End of the modification
	case noFitsEnergyCol:
		errorMessage = "\
Column energy could not be found in the reflection extension";
		break;
	case noFitsBangleCol:
		errorMessage = "\
Column bangle could not be found in the reflection extension";
		break;
	case noFitsRefprobCol:
		errorMessage = "\
Column refprob could not be found in the reflection extension";
		break;
	case invalidFormatVersionString:
		errorMessage = "\
invalid string for CBD20001 = 'FORMAT_VERSION(n)";
		break;
	case unknownFormatVersion:
		errorMessage = "\
unknown CBD20001 = 'FORMAT_VERSION(n)";
		break;
	case noTelescopeDefined:
		errorMessage = "\
Can not ray trace since telescope description not loaded";
		break;
	case noPhotonDefined:
		errorMessage = "\
Can not ray trace since no photon has been defined";
		break;
	case badStructureLayerType:
		errorMessage = "\
Internal program error in XrrtRayTrace::rayTrace: badStructureLayerType";
		break;
	case unsupportedMirrorType:
		errorMessage = "\
Attempt to use a mirror surface function which is not supported";
		break;

// Add error case for unsupported collimator type
// (added by H. Mori)
	case unsupportedCollimatorType:
		errorMessage = "\
Attempt to use a collimator surface function which is not supported";
		break;
	case invalidScross:
		errorMessage = "\
FITSIO returned non-[t|T|f|F] for the scross column value";
		break;
	case invalidEcross:
		errorMessage = "\
FITSIO returned non-[t|T|f|F] for the ecross column value";
		break;
	case requestedFitsFileNotOpen:
		errorMessage = "\
Can not write historical FITS keywords because file is not open";
		break;
	case missingCALDBVariable:
		errorMessage = "\
Function needed CALDB environment variable and it was not set.";
		break;
	case caldbError:
		errorMessage = "\
General error occured while accessing the CALDB.";
		break;
	case errorLoadingReflectionTables:
		errorMessage = "\
General error while Loading Reflection Tables.";
		break;
	case missingPreCollimatorReflectionTables:
	    errorMessage ="\
A Pre-Collimator reflection table could not be found";
	    break;
	case outOfMemory:
	    errorMessage ="\
Run out of memory";
	    break;
	default:
		char charNumber[1024];
		sprintf(charNumber, "%d",errorCode);
		errorMessage = "\
XrrtRayTrace::errorMessage Unknown error code: ";
		errorMessage.append(charNumber);
	}

	return errorMessage;
}

void
XrrtRaytrace::defineInternalReflectionTables(string& frontTableName,
	                                        XrrtMolecule* frontSurfacePtr,
	                                        double& frontCGSDensity,
	                                        double& frontRoughness,
	                                        string& backTableName,
	                                        XrrtMolecule* backSurfacePtr,
	                                        double& backCGSDensity,
	                                        double& backRoughness)
{
	dynamicReflectionTable = true;
	frontSurfaceTableName = frontTableName;
	frontSurface          = frontSurfacePtr;
	frontSurfaceCGSDensity= frontCGSDensity;
	frontSurfaceRoughness = frontRoughness;
	backSurfaceTableName  = backTableName;
	backSurface           = backSurfacePtr;
	backSurfaceCGSDensity = backCGSDensity;
	backSurfaceRoughness  = backRoughness;
	// pass this along to the reflection table code
	theReflectionInfo().defineInternalReflectionTables(frontTableName,
	                                                   frontSurfacePtr,
	                                                   frontCGSDensity,
	                                                   frontRoughness,
	                                                   backTableName,
	                                                   backSurfacePtr,
	                                                   backCGSDensity,
	                                                   backRoughness);
}

void
XrrtRaytrace::writeHistoricalKeywords(fitsfile* fitsFilePtr)
{
//
// Information about the state of XRRT during ray tracing is scattered
// in several places. This function writes that part of the state
// known to XrrtRayTrace as keywords to the given FITS file header.
//

// Standard FITSIO calling sequence
	int fitsStatus;		// Standard error code return from FITSIO
	int fitsReturn;		// Standard return type from a FITSIO function

	char *comment;
	char *keyword;
	char **charArray;
	char* commentArray[1];

	int ipassValue;
	double passValue;

	if (fitsFilePtr == 0) {
		throw requestedFitsFileNotOpen;
	}

	fitsStatus = 0;
	charArray = new char *[frontReflectTables.size()];
	int frontReflectTableNames = frontReflectTables.size();

	for (int i = 0; i < frontReflectTableNames; i++) {
		charArray[i] = (char *)frontReflectTables[i].c_str();
	}
	comment = "Raytrace Front Reflection Table Name&";
	commentArray[0] = comment;
	fitsReturn = fits_write_keys_str(fitsFilePtr, "FRTN", 1,
									 frontReflectTableNames,
									 charArray, commentArray, &fitsStatus);
	delete [] charArray;
	if (fitsStatus != 0) {
		throw fitsStatus;
	}

	charArray = new char *[backReflectTables.size()];
	int backReflectTableNames = backReflectTables.size();
	for (int i = 0; i < backReflectTableNames; i++) {
		charArray[i] = (char *)backReflectTables[i].c_str();
	}
	comment = "Raytrace Back Reflection Table Name&";
	commentArray[0] = comment;
	fitsReturn = fits_write_keys_str(fitsFilePtr, "BRTN",1,
									 backReflectTableNames,
									 charArray, commentArray, &fitsStatus);
	delete [] charArray;
	if (fitsStatus != 0) {
		throw fitsStatus;
	}

	//
	if (scatterNoneFound) {
		ipassValue = scatterNoneFound;
		keyword = "RTSNF";
		comment = "Ray Trace Scatter None Found on a mirror";
		fits_write_key(fitsFilePtr, TLOGICAL, keyword,
					   &ipassValue, (char*) comment, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}
	}

	//
	if (scatterAscaFound) {
		ipassValue = scatterAscaFound;
		keyword = "RTSAF";
		comment = "Ray Trace Scatter Asca Found on a mirror";
		fits_write_key(fitsFilePtr, TLOGICAL, keyword,
					   &ipassValue, comment, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}
	}

	//
	if (scatterAstroeFound) {
		ipassValue = scatterAstroeFound;
		keyword = "RTSAEF";
		comment = "Ray Trace Scatter Astro-E Found on a mirror";
		fits_write_key(fitsFilePtr, TLOGICAL, keyword,
					   &ipassValue, (char*) comment, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}
	}

	//
	passValue = assumedFocalPlaneZ;
	keyword = "RTAFPZMM";
	comment = "Ray Trace Assumed Focal Plane Z MM";
	fits_write_key(fitsFilePtr, TDOUBLE, keyword, &passValue,
				   comment, &fitsStatus);
	if (fitsStatus != 0) {
		throw fitsStatus;
	}

	//
	passValue = minimumTelescopePhotonRadiusMM;
	keyword = "RTMTPRMM";
	comment = "Ray Trace Minimum Telescope Photon Radius MM";
	fits_write_key(fitsFilePtr, TDOUBLE, keyword, &passValue,
				   comment, &fitsStatus);
	if (fitsStatus != 0) {
		throw fitsStatus;
	}

	//
	passValue = maximumTelescopePhotonRadiusMM;
	keyword = "RTXTPRMM";
	comment = "Ray Trace maXimum Telescope Photon Radius MM";
	fits_write_key(fitsFilePtr, TDOUBLE, keyword, &passValue,
				   comment, &fitsStatus);
	if (fitsStatus != 0) {
		throw fitsStatus;
	}

	//
	ipassValue = (int) structure.getNumberOfLayers();
	keyword = "RTNLT";
	comment = "Ray Trace Number of Layers in Telescope";
	fits_write_key(fitsFilePtr, TINT, keyword, &ipassValue,
				   comment, &fitsStatus);
	if (fitsStatus != 0) {
		throw fitsStatus;
	}

	//
	passValue = structure.getFocalLengthMM();
	keyword = "RTATFLMM";
	comment = "Ray Trace Assumed Telescope Focal Length MM";
	fits_write_key(fitsFilePtr, TDOUBLE, keyword, &passValue,
				   comment, &fitsStatus);
	if (fitsStatus != 0) {
		throw fitsStatus;
	}

	//
	passValue = structure.getOuterHousingRadius();
	keyword = "RTAOHRMM";
	comment = "Ray Trace Assumed telescope Outer Housing Radius MM";
	fits_write_key(fitsFilePtr, TDOUBLE, keyword, &passValue,
				   comment, &fitsStatus);
	if (fitsStatus != 0) {
		throw fitsStatus;
	}

	//
	passValue = structure.getInnerHousingRadius();
	keyword = "RTAIHRMM";
	comment = "Ray Trace Assumed telescope Inner Housing Radius MM";
	fits_write_key(fitsFilePtr, TDOUBLE, keyword, &passValue,
				   comment, &fitsStatus);
	if (fitsStatus != 0) {
		throw fitsStatus;
	}

	//
	if (dynamicReflectionTable) {
		fitsStatus = 0;
		char* charArray[1];
		char *commentArray[1];

		charArray[0] = (char *)frontSurfaceTableName.c_str();
		comment = "Raytrace Front Reflection Table Name";
		commentArray[0] = comment;
		fitsReturn = fits_write_keys_str(fitsFilePtr, "FRTN",1, 1,
										 charArray, commentArray, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}

		keyword = "RFRTAF";
		comment = "Raytrace Front Reflection Table Atomic Formula";
		fitsReturn = fits_write_key(fitsFilePtr, TSTRING, keyword,
			(char *)frontSurface->getSymbolFormula().c_str(),
			comment, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}

		passValue = frontSurfaceCGSDensity;
		keyword = "RFRTCGSD";
		comment = "Raytrace Front Reflection Table CGS Density";
		fits_write_key(fitsFilePtr, TDOUBLE, keyword,
					   &passValue, comment, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}

		passValue = frontSurfaceRoughness;
		keyword = "RFRTR";
		comment = "Raytrace Front Reflection Table Roughness";
		fits_write_key(fitsFilePtr, TDOUBLE, keyword,
					   &passValue, comment, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}

		charArray[0] = (char *)backSurfaceTableName.c_str();
		comment = "Raytrace Back Reflection Table Name";
		commentArray[0] = comment;
		fitsReturn = fits_write_keys_str(fitsFilePtr, "BRTN",1, 1,
										 charArray, commentArray, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}

		keyword = "RBRTAF";
		comment = "Raytrace Back Reflection Table Atomic Formula";
		fitsReturn = fits_write_key(fitsFilePtr, TSTRING, keyword,
			(char *)backSurface->getSymbolFormula().c_str(),
			comment, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}

		passValue = backSurfaceCGSDensity;
		keyword = "RBRTCGSD";
		comment = "Raytrace Back Reflection Table CGS Density";
		fits_write_key(fitsFilePtr, TDOUBLE, keyword,
					   &passValue, comment, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}

		passValue = backSurfaceRoughness;
		keyword = "RBRTR";
		comment = "Raytrace Back Reflection Table Roughness";
		fits_write_key(fitsFilePtr, TDOUBLE, keyword,
					   &passValue, comment, &fitsStatus);
		if (fitsStatus != 0) {
			throw fitsStatus;
		}
	}

}

XrrtTable*
XrrtRaytrace::getFrontTableEntry( const string& reflectTableString)
{
//
// Return a pointer to the reflection table whose name is supplied. If
// the table does not exist, one is created.
//
	vector<string>::iterator found;

	// Search the table to see if this string has been stored yet
	found = find(frontReflectTables.begin(), frontReflectTables.end(),
				 reflectTableString.c_str());
	if (found != frontReflectTables.end()) {
		// It already exists; return the reflection table number
		return defaultReflection.tableEntry(reflectTableString);
	}

	// The table name has not yet been stored
	// Store it and create a table slot for it
	frontReflectTables.push_back(reflectTableString);
	return defaultReflection.createReflectTable(reflectTableString);
}

XrrtTable*
XrrtRaytrace::getBackTableEntry( const string& reflectTableString)
{
//
// Return a pointer to the reflection table whose name is supplied. If
// the table does not exist, one is created.
	vector<string>::iterator found;

	// Search the table to see if this string has been stored yet
	found = find(backReflectTables.begin(), backReflectTables.end(),
				 reflectTableString.c_str());;
	if (found != backReflectTables.end()) {
		// It already exists; return the reflection table number
		return defaultReflection.tableEntry(reflectTableString);
	}
	// The table name has not yet been stored
	// Store it and create a table slot for it
	backReflectTables.push_back(reflectTableString);
	return defaultReflection.createReflectTable(reflectTableString);
}

XrrtTable*
XrrtRaytrace::getPcolTableEntry( const string& reflectTableString)
{
//
// Return a pointer to the reflection table whose name is supplied. If
// the table does not exist, one is created.
//
	vector<string>::iterator found;

	// Search the table to see if this string has been stored yet
	found = find(pcolReflectTables.begin(), pcolReflectTables.end(),
				 reflectTableString.c_str());
	if (found != pcolReflectTables.end()) {
		// It already exists; return the reflection table number
		return defaultReflection.tableEntry(reflectTableString);
	}
	// The table name has not yet been stored
	// Store it and create a table slot for it
	pcolReflectTables.push_back(reflectTableString);
	return defaultReflection.createReflectTable(reflectTableString);
}

PhotonStatusCodes
XrrtRaytrace::rayTrace()
{
//
// Ray trace the photon thru the telescope
//

// Hidden assumptions:
// There is a hidden assumption in this code that photons always have a
// z-axis (telescope axis) negative direction vector. This means that
// photons are always moving TOWARDS the focal plane. Internally, if a
// photon undergoes a direction transition to z direction vector of
// zero, it dies on the spot.

	//
	// Start a ray trace by verifing that we have what we need:
	// 1) A telescope description
	// 2) A photon
	//
	if (!telescopeDefined) {
		throw noTelescopeDefined;
	}
	if (!photonDefined) {
		throw noPhotonDefined;
	}

	//
	// Before ray-tracing, we must know the current telescope for which
	// we now execute the simulation, in order to set the quadrant
	// coordinate
	// (added by Hideyuki MORI : 2006/01/31)
	//
	quadrant.setCurrentTelescope(astroeXrtTelescopeName);

	//
	// Raytracing is a process of moving the photon from its present
	// position to the focal plane while interacting it with the telescope
	// parts
	//
	Count numberOfLayers = structure.getNumberOfLayers();

	// OKADA (20061106)
	//	fprintf(stdout,"numberOfLayers = %d\n",numberOfLayers);
	//


	// Step the photon thru the telescope layers
	//
	// For each layer
	//
	for (Count layer=0; layer < numberOfLayers; layer++) {
		//
		// Move the photon to the top of the current layer
		//
		//    1st find out where the top is
		FPDistanceInMM layerDistance = structure.getLayerDistance(layer);

		//    2nd move the virtual photon there
		defaultPhoton.setVirtualToCurrent();
		defaultPhoton.projectVirtualPhoton(layerDistance);

		//
		// Since there is nothing between layers, the above move should
		// be in free space so the only possible event is impact on a housing
		// structure. Check whether the photon would have impacted the outer
		// housing or tried to cross the inner housing.
		//
		RadiusInMM radiusOfPhoton = defaultPhoton.getVirtualRadius();
		if (radiusOfPhoton >= structure.getOuterHousingRadius()) {
			// The photon hit the outer limit of travel
			defaultPhoton.setPhotonStatus(PHOTON_HITS_OUTER_HOUSING);

			// OKADA (20061107)
			//			fprintf(stdout,"Hit on outerHousing\n");
			// 1000000 photon 振って１つもひっかからなかった

			return defaultPhoton.photonStatus();
		}
		if (radiusOfPhoton <= structure.getInnerHousingRadius()) {
			// The photon hit the inner limit of travel
			defaultPhoton.setPhotonStatus(PHOTON_HITS_INNER_HOUSING);

			// OKADA (20061107)
			//			fprintf(stdout,"Hit on innerHousing\n");
			// 1000000 photon 振って１つもひっかからなかった

			return defaultPhoton.photonStatus();
		}

		defaultPhoton.setCurrentToVirtual();

		//
		// After we determine the current quadrant into which the photon goes, we
		// convert the direction vector of the photon in the telesope coordinate
		// into that in the quadrant coordinate.  We archieve the tilt of the
		// quadrant axis by the relative offset of the photon's direction vector.
		// (added by Hideyuki MORI : date 2005/12/17)
		//
		// Temporal hard coding
		// fprintf(stdout, "QT setting start\n");
		// quadrant.clearQuadrant();
		//
		AngleInRadians rotationAngle;
		rotationAngle = defaultPhoton.getRotationAngle();
		TelescopeQuadrant photonQuadrant = structure.getTelescopeQuadrant(rotationAngle);

		quadrant.setCurrentQuadrant(photonQuadrant);
		quadrant.setCurrentLayer(layer);
		quadrant.setCurrentOffset();
		quadrant.setCurrentAngleOffset();

		defaultPhoton.setTelescopeToQuadrantCoordinate(quadrant);

		//
		// Layer traverse depends on the type of layer this is
		//
		LayerType layerType = structure.getLayerType(layer);

		switch (layerType) {
		case MIRROR:
			// printf("MIRROR: layer=%d, layerType=%d\n", layer, layerType);
			mirrorLayerTrace(layer);
			break;
		case OBSTRUCTION:
			// printf("OBSTRUCTION: layer=%d, layerType=%d\n", layer, layerType);
			obstructionLayerTrace(layer);
			break;
			// Add one more ray-tracing routine for Astro-E2 pre-collimator
			// (added by Hideyuki MORI)
		case COLLIMATOR:
			// printf("COLLIMATOR: layer=%d, layerType=%d\n", layer, layerType);
			collimatorLayerTrace(layer);
			break;
		default:
			throw badStructureLayerType;
			break;
		}

		//
		// Back the direction vector of the photon in the quadrant coordinate
		// to that in the telescope coordinate
		// (added by Hideyuki MORI : date 2005/12/17)
		//
		defaultPhoton.setQuadrantToTelescopeCoordinate(quadrant);

		//
		// Check photon status at the end of the layer
		//
		if (defaultPhoton.photonStatus() != PHOTON_CONTINUES) {
			// The photon terminated somehow
			break;
		}

	}

	//
	// Check photon status at the end of the last layer
	//
	if (defaultPhoton.photonStatus() == PHOTON_CONTINUES) {

		// The photon left the last telescope layer so project it to the
		// focal plane
		defaultPhoton.projectPhoton(assumedFocalPlaneZ);
		defaultPhoton.setPhotonStatus(PHOTON_HIT_FOCAL_PLANE);
		defaultPhoton.classifyPhotonByReflections();
	}

	// OKADA (20061103)
	//	fprintf(stdout,"--------------\n");

	return defaultPhoton.photonStatus();
}

void
XrrtRaytrace::obstructionLayerTrace(const Count& layer)
{
	const char fname[] = "XrrtRaytrace::obstructionLayerTrace():";
//
// Trace the photon thru a layer made up of obstructions
//

// This code improves speed for obstruction testing by assuming that
// many obstructions will occur at the same Z distance.

	// We must check whether the photon impacts any of the
	// obstructions in this layer.
	VertexInMM x = 0;
	VertexInMM y = 0;
	defaultPhoton.getXY(&x, &y);
	TelescopeQuadrant photonQuadrant = structure.getTelescopeQuadrant(x,y);

	// OKADA (20061104)
	// photonQuadrant は XrrtPhoton で定義される(本当はもっと階層は
	// 深い)メンバ関数でphoton のx,y座標を取ってきて、それを
	// XrrtStructure で定義されるgetTelescopeQuadrant メンバ関数で
	// enum で定義される TelescopeQuadrant へ入れている
	//	fprintf(stdout,"layer = %d\n",layer);
	//	fprintf(stdout,"Quadrant Coordinate = %lf %lf, QT = %d\n",
	//                                  x,y,photonQuadrant);

	//
	// Get the 1st obstruction; there must be one since a layer exists
	//
	XrrtObstruction* obstruction = 0;
	XrrtObstruction* lastObstruction=0;
	obstruction = structure.getObstructionByLayer(layer, photonQuadrant);
	if ( NULL == obstruction ) {
		fprintf(stderr, "\
%s: WARNING: null obstruction\n", fname);
		defaultPhoton.setPhotonStatus(PHOTON_CONTINUES);
		return;
	}
	lastObstruction=obstruction;
	//
	// Keep track of the Z distance of the 1st obsruction handled
	//
	double lastObstructionZ = 0;
	lastObstructionZ = obstruction->getFPDistance();

	// OKADA (20061104)
	//	fprintf(stdout,"lastObstructionZ = %lf mm \n",lastObstructionZ);

	//
	// Project the photon to the 1st obstruction
	//
	defaultPhoton.projectPhoton(lastObstructionZ);
	defaultPhoton.getXY(&x, &y);

	// OKADA (20061104)
	//	fprintf(stdout,"Quadrant Coordinate = %lf %lf, QT = %d\n",x,y,photonQuadrant);


	double currentObstructionZ;
	//
	// Loop until there are no more obstructions
	//
	while (obstruction != 0) {
		//
		// Move the photon to the current obstruction
		//
		currentObstructionZ = obstruction->getFPDistance();

		// OKADA (20061103)
		//		fprintf(stdout,"lastObstructionZ = %lf mm, currentObstructionZ = %lf mm\n",lastObstructionZ,currentObstructionZ);

		//
		// Have we moved in Z?
		//
		if (lastObstructionZ != currentObstructionZ) {
			//
			// The obstruction layer has multiple Z distances
			// Move the photon to the new Z
			//
			defaultPhoton.projectPhoton(currentObstructionZ);
			defaultPhoton.getXY(&x, &y);
			lastObstructionZ = currentObstructionZ;
		}
		//
		// Photon is at the required Z distance
		//
		// Check for impact of photon on obstruction
		//
		if (obstruction->xyInObstruction(x,y)) {
			//
			// Photon hit obstruction
			//
			// Mark the photon with the obstruction pointer and set the status
			defaultPhoton.setImpactObstruction(obstruction);
			defaultPhoton.setPhotonStatus(PHOTON_HITS_OBSTRUCTION);
			return;
		}
		//
		// Get the next obstruction in the layer
		//
		obstruction = obstruction->getNextInLayer();
		lastObstruction=obstruction;
	}
	//
	// We successfully bypassed all obstructions in the layer
	//
	defaultPhoton.setPhotonStatus(PHOTON_CONTINUES);
	return;
}

void
XrrtRaytrace::mirrorLayerTrace(const Count& layer)
{
//
// Trace the photon thru a mirror layer
//
	const double xrrtTwoPI = 6.283185307e0;

	XrrtMirror* outerMirror = 0;
	XrrtMirror* newOuterMirror = 0;
	RadiusInMM radiusOfPhoton = 0;
	AngleInRadians angleOfPhoton = 0;
	// AngleInRadians photonAngleAdjustment= 0.0e0;

	// When entering a mirror set we 1st determine if-any/which
	// mirror is involved
	//
	// Find the mirror with a outer top radius greater than
	// the photon position and with an overlaping range in rotation
	// angle
	radiusOfPhoton = defaultPhoton.getRadius();
	angleOfPhoton = defaultPhoton.getRotationAngle();
	outerMirror = structure.getOuterMirror(layer, radiusOfPhoton,
	                                       angleOfPhoton);
	//
	// See whether a mirror was returned
	//
	if (outerMirror == 0) {
		// There is no outer mirror, the photon is outside the
		// telescope and may have hit the outer housing
		//
		if (radiusOfPhoton >= structure.getOuterHousingRadius()) {
			// The photon hit the outer limit of travel
			defaultPhoton.setPhotonStatus(PHOTON_HITS_OUTER_HOUSING);
			return;
		} else {
			//
			// It is bad design to allow a gap between the outer most mirror
			// and the outer housing. This code has no idea what to do with
			// such a situation or why a designer would want such a thing.
			// So we abort the photon and warn the user.
			//c_fcerr("Error:No outer mirror for photon");
			defaultPhoton.setPhotonStatus(ERROR);
			return;
		}
	}

	//
	// Check that this code can handle this mirror type
	//
	if (outerMirror->getSurfaceFunction() != PLANE_MIRROR) {
		// This routine can only handle plane mirrors
		throw unsupportedMirrorType;
	}

	//
	// Project the virtual photon position to the top of the
	// mirror so we can see if it stays with this mirror.
	// Even though the photon was already projected to the top of the layer,
	// there is no requirement that all mirrors extend across the whole
	// layer distance.
	//
	defaultPhoton.setVirtualToCurrent();
	defaultPhoton.projectVirtualPhoton(outerMirror->getTopDistance());
	//
	// See if the photon stays with this mirror
	//
	radiusOfPhoton = defaultPhoton.getVirtualRadius();
	angleOfPhoton = defaultPhoton.getVirtualRotationAngle();
	newOuterMirror = structure.getOuterMirror(layer, radiusOfPhoton,
	                                          angleOfPhoton);
	if (newOuterMirror != outerMirror) {
		// The photon is abnormal and changing location rapidly
		// or there is too much of a step between the layer
		// definition mirror distance and the nominal outer mirror.
		// This mode of operation is untested so we will abort the photon.
		c_fcerr("\
Error: newOuterMirror != outerMirror");
		fprintf(stdout, "\
Radius of real photon : %f\n", defaultPhoton.getRadius());
		fprintf(stdout, "\
Radius of virtual photon : %f\n", defaultPhoton.getVirtualRadius());
		fprintf(stdout, "\
Rotation angle of real photon : %f\n", defaultPhoton.getRotationAngle());
		fprintf(stdout, "\
Rotation angle of virtual photon : %f\n", defaultPhoton.getVirtualRotationAngle());
		fprintf(stdout, "\
Radius of Outer Mirror : %f\n", outerMirror->getTopInnerRadius());
		fprintf(stdout, "\
Radius of new Outer Mirror : %f\n", newOuterMirror->getTopInnerRadius());

		defaultPhoton.setPhotonStatus(ERROR);
		return;
	}
	defaultPhoton.setCurrentToVirtual();

	//
	// The photon may impact on the top of the mirror cross-section so
	// check for that.
	//
	if ( outerMirror->getTopOuterRadius() >= defaultPhoton.getRadius() &&
	     defaultPhoton.getRadius() >= outerMirror->getTopInnerRadius() ) {
		// The photon impacts on the mirror cross section
/*		printf("\
currentQuadrant=%d, currentLayer=%d, \
getRadius()=%f, getTopOuterRadius()=%f, getTopInnerRadius()=%f\n",
			   theQuadrant().getCurrentQuadrant(), theQuadrant().getCurrentLayer(),
			   defaultPhoton.getRadius(), outerMirror->getTopOuterRadius(), outerMirror->getTopInnerRadius());*/
/*		printf("\
currentQuadrant=%d, currentLayer=%d, \
offsetX=%f, offsetY=%f, offsetZ=%f, angOffsX=%f, angOffsY=%f, angOffsZ=%f\n",
			   theQuadrant().getCurrentQuadrant(), theQuadrant().getCurrentLayer(),
			   theQuadrant().getCurrentOffsetX(), theQuadrant().getCurrentOffsetY(), theQuadrant().getCurrentOffsetZ(),
			   theQuadrant().getCurrentAngleOffsetThetaX(), theQuadrant().getCurrentAngleOffsetThetaY(), theQuadrant().getCurrentAngleOffsetThetaZ()
			   );*/
		defaultPhoton.setAbsorptionMirror(outerMirror);
		defaultPhoton.setAbsorptionMirrorFace(MIRROR_TOP);
		defaultPhoton.setPhotonStatus(PHOTON_HITS_TOP_OF_MIRROR);
		return;
	}

	//
	// Where does the photon go?
	// The photon has several options:
	// 1) It can hit the outer mirror
	// 2) It can hit the inner mirror
	// 3) It can change mirrors before it strikes anything (i.e. move out of
	//    the rotational range of the current mirror)
	// 4) It can miss both inner and outer mirror and escape thru the layer
	// 5) It can do a combination of the above
	//
	// Which of the above it does is a function of its position relative
	// to its surroundings and its relative rate of motion in the 3 directions.

	//
	// Loop on the photon until it either escapes the mirror layer or terminates
	//
	defaultPhoton.setPhotonStatus(PHOTON_CONTINUES);
	XrrtMirror* innerMirror = 0;
	bool photonHitMirror = false;
	do {
		photonHitMirror = false;
		//
		// What direction is the photon currently moving?
		//
		UnitVectorMag  photonRadialDirection;
		AngleInRadians photonPhiAngle;
		UnitVectorMag  photonZDirection;
		defaultPhoton.getPhotonDirection(photonRadialDirection,
										 photonPhiAngle,
										 photonZDirection);

		//
		// From the current photon location, will it strike a inner mirror?
		//
		// Note: I cast away the const'ness of outerMirror because I KNOW
		//       getInnerMirror will not modify it!
		innerMirror = (XrrtMirror*) outerMirror->getInnerMirror();
		if (innerMirror == 0) {
			// There is no inner mirror here
			// See whether the photon can make it to the end of the layer or
			// whether it will crash into the inner housing limit
			double currentPhotonAngle = defaultPhoton.getRotationAngle();
			double mirrorBottomDistance = outerMirror->getBottomDistance();
			defaultPhoton.setVirtualToCurrent();
			defaultPhoton.projectVirtualPhoton(mirrorBottomDistance);
			double futurePhotonAngle = defaultPhoton.getVirtualRotationAngle();
			double futurePhotonRadius = defaultPhoton.getVirtualRadius();
			if (futurePhotonRadius < theStructure().getInnerHousingRadius()) {
				defaultPhoton.setPhotonStatus(PHOTON_HITS_INNER_HOUSING);
				break;
			}
			if (fabs(futurePhotonAngle - currentPhotonAngle) > 1.57) {
				// this is weak but should assume impact on housing here as well
				defaultPhoton.setPhotonStatus(PHOTON_HITS_INNER_HOUSING);
				break;
			}
		} else {
			//
			// Check for Inner mirror impact
			// For speed, I have removed the ability of the code to deal
			// with multiple inner mirrors for a given outer mirror but
			// an echo of the code structure remains
			//
			do {
				//
				// Get the inner mirror direction vectors relative to where the
				// photon is now
				//
				UnitVectorMag mirrorRadialDirection;
				AngleInRadians mirrorPhiAngle;
				UnitVectorMag mirrorZDirection;
				XrrtVector mirrorVector = innerMirror->getPlaneMirrorBackVector(
					defaultPhoton.getRotationAngle());
				mirrorVector.getVectorDirection(mirrorRadialDirection,
												mirrorPhiAngle,
												mirrorZDirection);
				//
				// Switch to the mirror frame of reference
				// From the point of view of the mirror, the photon is either
				// moving towards it or away; if away, it can not be hit
				//
				// Compute the magnitude of the direction vector in the mirror
				// reference system; since the photon must catch up to the mirror
				// in order to hit it, the pojection of the photon vector onto the
				// mirror vector must be greater than the mirror vector; thus
				// the radialInMirrorFrame must be -ve.
				//
				UnitVectorMag radialInMirrorFrame = mirrorRadialDirection -
				photonRadialDirection * cos(mirrorPhiAngle - photonPhiAngle);
				if ( radialInMirrorFrame >= 0.0 ) {
					// The mirror can not be hit
					continue;
	            }
				//
				// The "t" it takes for the photon to move radially to the outer
				// mirror implies the z distance it will require. If that z
				// distance
				// is greater than the distance to the bottom of the mirror, the
				// photon misses the mirror.
				//
				FPDistanceInMM photonZ = defaultPhoton.getPhotonDistance();
				RadiusInMM     mirrorRadiusAtZ = innerMirror->getOuterRadiusAtZ(photonZ);
				RadiusInMM     photonRadius = defaultPhoton.getRadius();
				//
				// We want t to end up positive for a -ve radialInMirrorFrame
				// and the photon radius is > the mirror radius
				double t = (mirrorRadiusAtZ - photonRadius)/radialInMirrorFrame;
				if ( t  <=0.0 ) {
					// This occurs when a previously reflected photon appeared
					// (due to pecision loss in the radius calc) to actually
					// be inside the mirror and trying to come out and hit the
					// surface again. So we ignore this event.
					break;
	            }
				//
				// Compute where the photon will be after "t" units.
				// If this is beyond the top or bottom of the mirror, it
				// can not hit the mirror
				//
				FPDistanceInMM mirrorBotDistance = innerMirror->getBottomDistance();
				FPDistanceInMM mirrorTopDistance = innerMirror->getTopDistance();
				FPDistanceInMM impactZ = photonZ + (photonZDirection*t);
				if (impactZ > mirrorTopDistance || impactZ < mirrorBotDistance) {
					// The inner mirror is not hit
					continue;
	            }
				//
				// Just because the photon will hit the mirror doesn't mean the
				// mirror will be there to be hit.
				// Check that the photon will not move too far in rotation angle
				// phi. Since mirrors can have limited phi range, the mirror
				// may not be there to be hit.
				//
				AngleInRadians photonAngle  = defaultPhoton.getRotationAngle();
				AngleInRadians mirrorStartAngle  = innerMirror->getStartAngle();
				AngleInRadians mirrorEndAngle    = innerMirror->getEndAngle();
				if (photonAngle < mirrorStartAngle) {
					photonAngle = photonAngle + xrrtTwoPI;
	            }
				// mirrorPhiAngle - photonPhiAngle
				// or
				// photonPhiAngle - mirrorPhiAngle?
				AngleInRadians photonPhiChange = photonRadialDirection *
				sin(mirrorPhiAngle - photonPhiAngle)/defaultPhoton.getRadius();
				photonAngle = photonAngle + photonPhiChange*t;
				if ( photonAngle <= mirrorStartAngle ||
					 photonAngle >= mirrorEndAngle ) {
					// The photon moves off the mirror and does not hit it
					continue;
	            }
				//
				// Move the current photon location to the impact point.
				// Unfortunately there is a precision loss even with doubles
				// that results in many/most of the photons appearing to be inside
				// the mirror when they reflect. This is only a tiny distance,
				// usually on the order of 0.001 mm which is probably much less
				// than how well the actual surface location is known to.
				// But it is undesirable and can cause internal confusion when
				// photons are scattered to travel very nearly parallel to the
				// mirror surface. This needs to be solved somehow.
				//
				defaultPhoton.projectPhoton(impactZ);
				//           mirrorRadiusAtZ = innerMirror->getOuterRadiusAtZ(impactZ);
				defaultPhoton.setVirtualToCurrent();
				//
				// The photon hit the innerMirror at its current position
				//
				photonHitMirror = true;
				defaultPhoton.addReflection(outerMirror->getLayer());
				//
				// See if the photon reflects or is absorbed
				// if it reflects, the virtual photon will have the reflection path
				//
				if (defaultReflection.photonAbsorbedOnInnerMirror(innerMirror,
																  defaultPhoton)) {
					// The photon was absorbed
					defaultPhoton.setPhotonStatus(PHOTON_ABSORBED_ON_INNER_MIRROR);
					defaultPhoton.setAbsorptionMirror(innerMirror);
					defaultPhoton.setAbsorptionMirrorFace(INNER_MIRROR_FACE);
					break;
	            } else {
					// The photon reflected and the virtual photon shows the path
					defaultScatterInfo.backScatter(innerMirror,defaultPhoton);
					//
					// add the information which surface photon is reflected into reflection surface array
					// add the selection of subroutine whether Pre-Collimator is mounted or not
					// (modified by H. Mori : date 2003/01/28)
					ReflectionSurfaceSide SurfaceSide = INNER;
					if ( mountPreCollimator ) {
						defaultPhoton.addReflectionSurfaceWithPreCollimator(outerMirror->getLayer(), SurfaceSide);
					} else {
						defaultPhoton.addReflectionSurfaceWithoutPreCollimator(outerMirror->getLayer(), SurfaceSide);
					}
					break;
	            }
			} while(false);
			//
			// Check whether anything happened to the photon while it was
			// interacting with the inner mirror.
			//
			if (defaultPhoton.photonStatus() != PHOTON_CONTINUES) {
				break;
			}
		}

		//
		// From the current photon location, will it strike the outer mirror?
		//
		//
		// Get the photon direction vectors
		defaultPhoton.getPhotonDirection(photonRadialDirection,
										 photonPhiAngle,
										 photonZDirection);
		// Get the mirror direction vectors
		UnitVectorMag mirrorRadialDirection;
		UnitVectorMag mirrorPhiAngle;
		UnitVectorMag mirrorZDirection;
		XrrtVector mirrorVector =
		outerMirror->getPlaneMirrorFrontVector(defaultPhoton.getRotationAngle());
		mirrorVector.getVectorDirection(mirrorRadialDirection,
										mirrorPhiAngle,
										mirrorZDirection);
		//
		// Switch to the mirror frame of reference
		// From the point of view of the mirror, the photon is either
		// moving towards it or away; if away, it can not be hit
		//
		// Compute the magnitude of the direction vector in the mirror
		// reference system; for an outer mirror, if the projection of the
		// photon vector onto the mirror vector is smaller than the mirror vector
		// then the mirror will catch the photon; thus we want the
		// radialInMirrorFrame to be +ve.
		//
		UnitVectorMag radialInMirrorFrame = mirrorRadialDirection -
			photonRadialDirection * cos(mirrorPhiAngle - photonPhiAngle);
		if ( radialInMirrorFrame <= 0.0 ) {
			// The mirror can not be hit
			break;
		}
		//
		// The "t" it takes for the photon to move radially to the outer
		// mirror implies the z distance it will require. If that z distance is
		// greater than the distance to the bottom of the mirror, the photon
		// misses the mirror.
		//
		FPDistanceInMM photonZ = defaultPhoton.getPhotonDistance();
		RadiusInMM     mirrorRadiusAtZ = outerMirror->getInnerRadiusAtZ(photonZ);
		RadiusInMM     photonRadius = defaultPhoton.getRadius();
		// We want t to end up positive for a +ve radialInMirrorFrame
		double t = (mirrorRadiusAtZ - photonRadius) / radialInMirrorFrame;
		if ( t <= 0.0 ) {
			// This occurs when the precision of the photon radius calc
			// fails and the program believes that a previously reflected photon
			// is actually inside the mirror and trying to come out so we ignore
			// it
			break;
		}
		//
		// Compute the impact Z and see if the mirror will be there
		//
		FPDistanceInMM mirrorBotDistance = outerMirror->getBottomDistance();
		FPDistanceInMM mirrorTopDistance = outerMirror->getTopDistance();
		FPDistanceInMM impactZ = photonZ + (photonZDirection*t);
		if (impactZ > mirrorTopDistance || impactZ < mirrorBotDistance) {
			// The outer mirror is not hit
			break;
		}
		//
		// Just because the photon will hit the mirror doesn't mean the mirror
		// will be there to be hit.
		//
		// Check that the photon will not move too far in rotation angle phi.
		//
		AngleInRadians photonAngle  = defaultPhoton.getRotationAngle();
		AngleInRadians mirrorStartAngle  = outerMirror->getStartAngle();
		AngleInRadians mirrorEndAngle    = outerMirror->getEndAngle();
		if (photonAngle < mirrorStartAngle) {
			photonAngle = photonAngle + xrrtTwoPI;
		}
		// mirrorPhiAngle - photonPhiAngle
		// or
		// photonPhiAngle - mirrorPhiAngle?
		AngleInRadians photonPhiChange = photonRadialDirection *
			sin(mirrorPhiAngle - photonPhiAngle)/defaultPhoton.getRadius();

		double newPhotonAngle = photonAngle + photonPhiChange*t;
		if ( newPhotonAngle <= mirrorStartAngle ||
			 newPhotonAngle >= mirrorEndAngle ) {
			// The photon moves off the mirror
			// We may need a new mirror
			//
			if ( photonPhiChange < 0 ) {
				// The photon is moving clockwise and will leave via the mirror
				// start angle
				t = (mirrorStartAngle - photonAngle)/photonPhiChange;
				if ( outerMirror->canPhotonCrossStartAngle() ) {
					// The photon can cross the boundary
					// Get the new outer mirror
					outerMirror = outerMirror->getStartAngleNeighbor();
	            } else {
					//Photon dies at start of mirror
					defaultPhoton.setPhotonStatus(PHOTON_CAN_NOT_LEAVE_MIRROR);
	            }
			} else {
				// The photon is moving counter-clockwise and will leave via the
				// mirror end angle
				t = (mirrorEndAngle - photonAngle) / photonPhiChange;
				if (outerMirror->canPhotonCrossEndAngle()) {
					// The photon can cross the boundary
					outerMirror = outerMirror->getEndAngleNeighbor();
	            } else {
					// Photon dies at end of mirror
					defaultPhoton.setPhotonStatus(PHOTON_CAN_NOT_LEAVE_MIRROR);
	            }
			}
			//
			// Fix a low probbility bug that triggers a closed loop
			if ( t < 0.01 ) {
				t = 0.01;
			}
			//
			FPDistanceInMM z = photonZ + photonZDirection*t;
			//
			// Move the current photon location to the mirror leave point
			defaultPhoton.projectPhoton(z);
			defaultPhoton.setVirtualToCurrent();
			continue;
		}

		// Move the current photon location to the impact point
		// Unfortunately there is a precision loss even with doubles
		// that results in many/most of the photons appearing to be inside
		// the mirror when they reflect. This effect is very small
		// (approx 0.001 mm) but needs to be fixed sometime.
		//
		defaultPhoton.projectPhoton(impactZ);
		//     mirrorRadiusAtZ = outerMirror->getInnerRadiusAtZ(impactZ);
		defaultPhoton.setVirtualToCurrent();
		//
		// The photon hits the outer mirror at its current position
		//
		photonHitMirror = true;
		defaultPhoton.addReflection(outerMirror->getLayer());
		//
		// See if the photon reflects or is absorbed
		// if it reflects, the virtual photon will have the reflection path
		//
		if ( defaultReflection.photonAbsorbedOnOuterMirror(outerMirror,
														  defaultPhoton) ) {
			// The photon was absorbed
			defaultPhoton.setPhotonStatus(PHOTON_ABSORBED_ON_OUTER_MIRROR);
			defaultPhoton.setAbsorptionMirror(outerMirror);
			defaultPhoton.setAbsorptionMirrorFace(OUTER_MIRROR_FACE);
			break;
		} else {
			// The photon reflected and the virtual photon shows the path
			defaultScatterInfo.frontScatter(outerMirror,defaultPhoton);
			//
			// add the information which surface photon is reflected into stray pass array
			// add the selection of subroutine whether Pre-Collimator is mounted or not
			// (modified by H. Mori : date 2003/01/28)
			ReflectionSurfaceSide SurfaceSide = OUTER;
			if ( mountPreCollimator ) {
				defaultPhoton.addReflectionSurfaceWithPreCollimator(outerMirror->getLayer(), SurfaceSide);
			} else {
				defaultPhoton.addReflectionSurfaceWithoutPreCollimator(outerMirror->getLayer(), SurfaceSide);
			}
			continue;
		}

		// If the photon didn't hit a mirror and didn't escape rotationally,
		// it must have passed out of the layer
		if ( !photonHitMirror ) {
			// Move the current photon position to the bottom of the current
			// outer mirror and leave the loop
			defaultPhoton.projectPhoton(outerMirror->getBottomDistance());
			defaultPhoton.setVirtualToCurrent();
			break;
		}
	} while ( defaultPhoton.photonStatus() == PHOTON_CONTINUES );
	//
	// If the photon escaped the layer, make sure it is at the bottom.
	//
	if ( defaultPhoton.photonStatus() == PHOTON_CONTINUES ) {
		// Move the current photon position to the bottom of the current
		// outer mirror
		defaultPhoton.projectPhoton(outerMirror->getBottomDistance());
		defaultPhoton.setVirtualToCurrent();
	}

	return;
}

// Add ray-tracing routine which the photon goes through collimator layer
// This code is written based on the previous routine : mirrorLayerTrace();
// (added by H. Mori)
void
XrrtRaytrace::collimatorLayerTrace(const Count& layer)
{
//
// Trace the photon thru a collimator layer
//
	const double xrrtTwoPI = 6.283185307e0;

	XrrtCollimator* outerCollimator = 0;
	XrrtCollimator* newOuterCollimator = 0;
	ColRadiusInMM radiusOfPhoton = 0;
	ColAngleInRadians angleOfPhoton = 0;
	// ColAngleInRadians photonAngleAdjustment= 0.0e0;

	// When entering a collimator set we 1st determine if-any/which
	// collimator is involved
	//
	// Find the collimator with a outer top radius greater than
	// the photon position and with an overlaping range in rotation
	// angle
	radiusOfPhoton = defaultPhoton.getRadius();
	angleOfPhoton = defaultPhoton.getRotationAngle();
	outerCollimator = structure.getOuterCollimator(layer, radiusOfPhoton,
												   angleOfPhoton);
	//
	// See whether a collimator was returned
	//
	if ( outerCollimator == 0 ) {
		// There is no outer collimator, the photon is outside the
		// telescope and may have hit the outer housing
		//
		if ( radiusOfPhoton >= structure.getOuterHousingRadius() ) {
			// The photon hit the outer limit of travel
			defaultPhoton.setPhotonStatus(PHOTON_HITS_OUTER_HOUSING);
			return;
		} else {
			//
			// It is bad design to allow a gap between the outer most collimator
			// and the outer housing. This code has no idea what to do with
			// such a situation or why a designer would want such a thing.
			// So we abort the photon and warn the user.
			//c_fcerr("Error:No outer mirror for photon");
			defaultPhoton.setPhotonStatus(ERROR);
			return;
		}
	}

	//
	// Check that this code can handle this collimator type
	//
	if ( outerCollimator->getColSurfaceFunction() != PLANE_COLLIMATOR ) {
		// This routine can only handle plane mirrors
		throw unsupportedCollimatorType;
	}

	//
	// Project the virtual photon position to the top of the
	// collimator so we can see if it stays with this collimator.
	// Even though the photon was already projected to the top of the layer,
	// there is no requirement that all collimators extend across the whole
	// layer distance.
	//
	defaultPhoton.setVirtualToCurrent();
	defaultPhoton.projectVirtualPhoton(outerCollimator->getColTopDistance());
	//
	// See if the photon stays with this mirror
	//
	radiusOfPhoton = defaultPhoton.getVirtualRadius();
	angleOfPhoton = defaultPhoton.getVirtualRotationAngle();
	newOuterCollimator = structure.getOuterCollimator(layer, radiusOfPhoton,
													  angleOfPhoton);
	if ( newOuterCollimator != outerCollimator ) {
		// The photon is abnormal and changing location rapidly
		// or there is too much of a step between the layer
		// definition collimator distance and the nominal outer collimator.
		// This mode of operation is untested so we will abort the photon.
		c_fcerr("Error: newOuterCollimator != outerCollimator");
		// fprintf(stdout, "%lf %lf is not %lf %lf\n in collimatorLayerTrace()\n",
		//                                                  defaultPhoton.getRadius(),
		//                                                  defaultPhoton.getRotationAngle(),
		//                                                  defaultPhoton.getVirtualRadius(),
		//                                                  defaultPhoton.getVirtualRotationAngle());
		// fprintf(stdout, "Photon distance is %lf\n", defaultPhoton.getPhotonDistance());
		// fprintf(stdout, "Top distance of the pre-collimator is %lf\n",
		//         outerCollimator->getColTopDistance());
		defaultPhoton.setPhotonStatus(ERROR);
		return;
	}
	defaultPhoton.setCurrentToVirtual();

	//
	// The photon may impact on the top of the collimator cross-section so
	// check for that.
	//
	if ( outerCollimator->getColTopOuterRadius() >= defaultPhoton.getRadius()  &&
	     defaultPhoton.getRadius() >= outerCollimator->getColTopInnerRadius() ) {
		// The photon impacts on the mirror cross section
		defaultPhoton.setAbsorptionCollimator(outerCollimator);
		defaultPhoton.setAbsorptionCollimatorFace(COLLIMATOR_TOP);
		defaultPhoton.setPhotonStatus(PHOTON_HITS_TOP_OF_COLLIMATOR);
		return;
	}

	//
	// Where does the photon go?
	// The photon has several options:
	// 1) It can hit the outer collimator
	// 2) It can hit the inner collimator
	// 3) It can change collimators before it strikes anything (i.e. move out of
	//    the rotational range of the current collimator)
	// 4) It can miss both inner and outer collimator and escape thru the layer
	// 5) It can do a combination of the above
	//
	// Which of the above it does is a function of its position relative
	// to its surroundings and its relative rate of motion in the 3 directions.

	//
	// Loop on the photon until it either escapes the collimator layer or terminates
	//
	defaultPhoton.setPhotonStatus(PHOTON_CONTINUES);
	XrrtCollimator* innerCollimator = 0;
	bool photonHitCollimator = false;
	do {
		photonHitCollimator = false;
		//
		// What direction is the photon currently moving?
		//
		UnitVectorMag  photonRadialDirection;
		AngleInRadians photonPhiAngle;
		UnitVectorMag  photonZDirection;
		defaultPhoton.getPhotonDirection(photonRadialDirection,
										 photonPhiAngle,
										 photonZDirection);

		//
		// From the current photon location, will it strike a inner collimator?
		//
		// Note: I cast away the const'ness of outerCollimator because I KNOW
		//       getInnerCollimator will not modify it!
		innerCollimator = (XrrtCollimator*) outerCollimator->getInnerCollimator();
		if (innerCollimator == 0) {
			// There is no inner collimator here
			// See whether the photon can make it to the end of the layer or
			// whether it will crash into the inner housing limit
			double currentPhotonAngle = defaultPhoton.getRotationAngle();
			double collimatorBottomDistance = outerCollimator->getColBottomDistance();
			defaultPhoton.setVirtualToCurrent();
			defaultPhoton.projectVirtualPhoton(collimatorBottomDistance);
			double futurePhotonAngle = defaultPhoton.getVirtualRotationAngle();
			double futurePhotonRadius = defaultPhoton.getVirtualRadius();
			if (futurePhotonRadius < theStructure().getInnerHousingRadius()) {
				defaultPhoton.setPhotonStatus(PHOTON_HITS_INNER_HOUSING);
				break;
			}
			if (fabs(futurePhotonAngle - currentPhotonAngle) > 1.57) {
				// this is weak but should assume impact on housing here as well
				defaultPhoton.setPhotonStatus(PHOTON_HITS_INNER_HOUSING);
				break;
			}
		} else {
			//
			// Check for Inner collimator impact
			// For speed, I have removed the ability of the code to deal
			// with multiple inner collimators for a given outer collimator but
			// an echo of the code structure remains
			//
			do {
				//
				// Get the inner collimator direction vectors relative to where the
				// photon is now
				//
				UnitVectorMag  collimatorRadialDirection;
				AngleInRadians collimatorPhiAngle;
				UnitVectorMag  collimatorZDirection;
				XrrtVector collimatorVector = innerCollimator->getPlaneCollimatorBackVector(
					defaultPhoton.getRotationAngle());
				collimatorVector.getVectorDirection(collimatorRadialDirection,
													collimatorPhiAngle,
													collimatorZDirection);
				//
				// Switch to the collimator frame of reference
				// From the point of view of the collimator, the photon is either
				// moving towards it or away; if away, it can not be hit
				//
				// Compute the magnitude of the direction vector in the collimator
				// reference system; since the photon must catch up to the collimator
				// in order to hit it, the pojection of the photon vector onto the
				// collimator vector must be greater than the collimator vector; thus
				// the radialInCollimatorFrame must be -ve.
				//
				UnitVectorMag radialInCollimatorFrame = collimatorRadialDirection -
				photonRadialDirection * cos(collimatorPhiAngle - photonPhiAngle);
				if ( radialInCollimatorFrame >= 0.0 ) {
					// The collimator can not be hit
					continue;
	            }
				//
				// The "t" it takes for the photon to move radially to the outer
				// collimator implies the z distance it will require. If that z
				// distance
				// is greater than the distance to the bottom of the collimator, the
				// photon misses the collimator.
				//
				FPDistanceInMM photonZ = defaultPhoton.getPhotonDistance();
				RadiusInMM     collimatorRadiusAtZ = innerCollimator->getColOuterRadiusAtZ(photonZ);
				RadiusInMM     photonRadius = defaultPhoton.getRadius();
				//
				// We want t to end up positive for a -ve radialInCollimatorFrame
				// and the photon radius is > the collimator radius
				double t = (collimatorRadiusAtZ - photonRadius)/radialInCollimatorFrame;
				if ( t <= 0.0 ) {
					// This occurs when a previously reflected photon appeared
					// (due to pecision loss in the radius calc) to actually
					// be inside the collimator and trying to come out and hit the
					// surface again. So we ignore this event.
					break;
				}
				//
				// Compute where the photon will be after "t" units.
				// If this is beyond the top or bottom of the collimator, it
				// can not hit the collimator
				//
				FPDistanceInMM collimatorBotDistance = innerCollimator->getColBottomDistance();
				FPDistanceInMM collimatorTopDistance = innerCollimator->getColTopDistance();
				FPDistanceInMM impactZ = photonZ + (photonZDirection*t);
				if (impactZ > collimatorTopDistance || impactZ < collimatorBotDistance) {
					// The inner collimator is not hit
					continue;
	            }
				//
				// Just because the photon will hit the collimator doesn't mean the
				// collimator will be there to be hit.
				// Check that the photon will not move too far in rotation angle
				// phi. Since collimators can have limited phi range, the collimator
				// may not be there to be hit.
				//
				AngleInRadians photonAngle  = defaultPhoton.getRotationAngle();
				AngleInRadians collimatorStartAngle  = innerCollimator->getColStartAngle();
				AngleInRadians collimatorEndAngle    = innerCollimator->getColEndAngle();
				if (photonAngle < collimatorStartAngle) {
					photonAngle = photonAngle + xrrtTwoPI;
	            }
				// collimatorPhiAngle - photonPhiAngle
				// or
				// photonPhiAngle - collimatorPhiAngle?
				AngleInRadians photonPhiChange = photonRadialDirection *
					sin(collimatorPhiAngle - photonPhiAngle)/defaultPhoton.getRadius();
				photonAngle = photonAngle + photonPhiChange*t;
				if ( photonAngle <= collimatorStartAngle ||
					 photonAngle >= collimatorEndAngle ) {
					// The photon moves off the collimator and does not hit it
					continue;
	            }
				//
				// Move the current photon location to the impact point.
				// Unfortunately there is a precision loss even with doubles
				// that results in many/most of the photons appearing to be inside
				// the collimator when they reflect. This is only a tiny distance,
				// usually on the order of 0.001 mm which is probably much less
				// than how well the actual surface location is known to.
				// But it is undesirable and can cause internal confusion when
				// photons are scattered to travel very nearly parallel to the
				// collimator surface. This needs to be solved somehow.
				//
				defaultPhoton.projectPhoton(impactZ);
				//           collimatorRadiusAtZ = innerCollimator->getColOuterRadiusAtZ(impactZ);
				defaultPhoton.setVirtualToCurrent();
				//
				// The photon hit the innerCollimator at its current position
				//
				photonHitCollimator = true;
				defaultPhoton.addReflection(outerCollimator->getColLayer());
				//
				// See if the photon reflects or is absorbed
				// if it reflects, the virtual photon will have the reflection path
				//
				if ( defaultReflection.photonAbsorbedOnInnerCollimator(innerCollimator,
																	   defaultPhoton) ) {
					// The photon was absorbed
					defaultPhoton.setPhotonStatus(PHOTON_ABSORBED_ON_INNER_COLLIMATOR);
					defaultPhoton.setAbsorptionCollimator(innerCollimator);
					defaultPhoton.setAbsorptionCollimatorFace(INNER_COLLIMATOR_FACE);
					break;
	            } else {
					// The photon reflected and the virtual photon shows the path
					// (modified by H. Mori : date 2003/01/14)
					defaultScatterInfo.pcolScatter(innerCollimator,defaultPhoton);
					//
					// add the information which surface photon is reflected into reflection surface array
					// add the selection of subroutine whether Pre-Collimator is mounted or not
					// (modified by H. Mori : date 2003/01/28)
					ReflectionSurfaceSide SurfaceSide = INNER;
					if(mountPreCollimator){
						defaultPhoton.addReflectionSurfaceWithPreCollimator(outerCollimator->getColLayer(), SurfaceSide);
					} else {
						defaultPhoton.addReflectionSurfaceWithoutPreCollimator(outerCollimator->getColLayer(), SurfaceSide);
					}
					break;
	            }
			} while ( false );
			//
			// Check whether anything happened to the photon while it was
			// interacting with the inner collimator.
			//
			if ( defaultPhoton.photonStatus() != PHOTON_CONTINUES ) {
				break;
			}
		}

		//
		// From the current photon location, will it strike the outer collimator?
		//
		//
		// Get the photon direction vectors
		defaultPhoton.getPhotonDirection(photonRadialDirection,
										 photonPhiAngle,
										 photonZDirection);
		// Get the collimator direction vectors
		UnitVectorMag collimatorRadialDirection;
		UnitVectorMag collimatorPhiAngle;
		UnitVectorMag collimatorZDirection;
		XrrtVector collimatorVector = outerCollimator->getPlaneCollimatorFrontVector(defaultPhoton.getRotationAngle());
		collimatorVector.getVectorDirection(collimatorRadialDirection,
											collimatorPhiAngle,
											collimatorZDirection);
		//
		// Switch to the collimator frame of reference
		// From the point of view of the collimator, the photon is either
		// moving towards it or away; if away, it can not be hit
		//
		// Compute the magnitude of the direction vector in the collimator
		// reference system; for an outer collimator, if the projection of the
		// photon vector onto the collimator vector is smaller than the collimator vector
		// then the collimator will catch the photon; thus we want the
		// radialInCollimatorFrame to be +ve.
		//
		UnitVectorMag radialInCollimatorFrame = collimatorRadialDirection -
			photonRadialDirection * cos(collimatorPhiAngle - photonPhiAngle);
		if ( radialInCollimatorFrame <= 0.0 ) {
			// The collimator can not be hit
			break;
		}
		//
		// The "t" it takes for the photon to move radially to the outer
		// collimator implies the z distance it will require. If that z distance is
		// greater than the distance to the bottom of the collimator, the photon
		// misses the collimator.
		//
		FPDistanceInMM photonZ = defaultPhoton.getPhotonDistance();
		RadiusInMM     collimatorRadiusAtZ = outerCollimator->getColInnerRadiusAtZ(photonZ);
		RadiusInMM     photonRadius = defaultPhoton.getRadius();
		// We want t to end up positive for a +ve radialInCollimatorFrame
		double t = (collimatorRadiusAtZ - photonRadius) / radialInCollimatorFrame;
		if ( t <= 0.0 ) {
			// This occurs when the precision of the photon radius calc
			// fails and the program believes that a previously reflected photon
			// is actually inside the collimator and trying to come out so we ignore
			// it
			break;
		}
		//
		// Compute the impact Z and see if the collimator will be there
		//
		FPDistanceInMM collimatorBotDistance = outerCollimator->getColBottomDistance();
		FPDistanceInMM collimatorTopDistance = outerCollimator->getColTopDistance();
		FPDistanceInMM impactZ = photonZ + (photonZDirection*t);
		if ( impactZ > collimatorTopDistance || impactZ < collimatorBotDistance ) {
			// The outer collimator is not hit
			break;
		}
		//
		// Just because the photon will hit the collimator doesn't mean the collimator
		// will be there to be hit.
		//
		// Check that the photon will not move too far in rotation angle phi.
		//
		AngleInRadians photonAngle  = defaultPhoton.getRotationAngle();
		AngleInRadians collimatorStartAngle  = outerCollimator->getColStartAngle();
		AngleInRadians collimatorEndAngle    = outerCollimator->getColEndAngle();
		if ( photonAngle < collimatorStartAngle ) {
			photonAngle = photonAngle + xrrtTwoPI;
		}
		// collimatorPhiAngle - photonPhiAngle
		// or
		// photonPhiAngle - collimatorPhiAngle?
		AngleInRadians photonPhiChange = photonRadialDirection *
			sin(collimatorPhiAngle - photonPhiAngle)/defaultPhoton.getRadius();

		double newPhotonAngle = photonAngle + photonPhiChange*t;
		if ( newPhotonAngle <= collimatorStartAngle ||
			 newPhotonAngle >= collimatorEndAngle ) {
			// The photon moves off the collimator
			// We may need a new collimator
			//
			if ( photonPhiChange < 0 ) {
				// The photon is moving clockwise and will leave via the collimator
				// start angle
				t = (collimatorStartAngle - photonAngle)/photonPhiChange;
				if ( outerCollimator->canPhotonCrossColStartAngle() ) {
					// The photon can cross the boundary
					// Get the new outer collimator
					outerCollimator = outerCollimator->getColStartAngleNeighbor();
	            } else {
					//Photon dies at start of collimator
					defaultPhoton.setPhotonStatus(PHOTON_CAN_NOT_LEAVE_COLLIMATOR);
	            }
			} else {
				// The photon is moving counter-clockwise and will leave via the
				// collimator end angle
				t = (collimatorEndAngle-photonAngle)/photonPhiChange;
				if ( outerCollimator->canPhotonCrossColEndAngle() ) {
					// The photon can cross the boundary
					outerCollimator = outerCollimator->getColEndAngleNeighbor();
	            } else {
					// Photon dies at end of mirror
					defaultPhoton.setPhotonStatus(PHOTON_CAN_NOT_LEAVE_COLLIMATOR);
	            }
			}
			//
			// Fix a low probbility bug that triggers a closed loop
			if ( t < 0.01 ) {
				t = 0.01;
			}
			//
			FPDistanceInMM z = photonZ + photonZDirection*t;
			//
			// Move the current photon location to the collimator leave point
			defaultPhoton.projectPhoton(z);
			defaultPhoton.setVirtualToCurrent();
			continue;
		}

		// Move the current photon location to the impact point
		// Unfortunately there is a precision loss even with doubles
		// that results in many/most of the photons appearing to be inside
		// the collimator when they reflect. This effect is very small
		// (approx 0.001 mm) but needs to be fixed sometime.
		//
		defaultPhoton.projectPhoton(impactZ);
		//     collimatorRadiusAtZ = outerCollimator->getColInnerRadiusAtZ(impactZ);
		defaultPhoton.setVirtualToCurrent();
		//
		// The photon hits the outer collimator at its current position
		//
		photonHitCollimator = true;
		defaultPhoton.addReflection(outerCollimator->getColLayer());
		//
		// See if the photon reflects or is absorbed
		// if it reflects, the virtual photon will have the reflection path
		//
		if ( defaultReflection.photonAbsorbedOnOuterCollimator(outerCollimator,
															  defaultPhoton) ) {
			// The photon was absorbed
			defaultPhoton.setPhotonStatus(PHOTON_ABSORBED_ON_OUTER_COLLIMATOR);
			defaultPhoton.setAbsorptionCollimator(outerCollimator);
			defaultPhoton.setAbsorptionCollimatorFace(OUTER_COLLIMATOR_FACE);
			break;
		} else {
			// The photon reflected and the virtual photon shows the path
			// (modified by H. Mori)
			defaultScatterInfo.pcolScatter(outerCollimator,defaultPhoton);
			//
			// add the information which surface photon is reflected into stray pass array
			// add the selection of subroutine whether Pre-Collimator is mounted or not
			// (modified by H. Mori : date 2003/01/28)
			ReflectionSurfaceSide SurfaceSide = OUTER;
			if ( mountPreCollimator ) {
				defaultPhoton.addReflectionSurfaceWithPreCollimator(outerCollimator->getColLayer(), SurfaceSide);
			} else {
				defaultPhoton.addReflectionSurfaceWithoutPreCollimator(outerCollimator->getColLayer(), SurfaceSide);
			}
			continue;
		}

		// If the photon didn't hit a collimator and didn't escape rotationally,
		// it must have passed out of the layer
		if ( !photonHitCollimator ) {
			// Move the current photon position to the bottom of the current
			// outer collimator and leave the loop
			defaultPhoton.projectPhoton(outerCollimator->getColBottomDistance());
			defaultPhoton.setVirtualToCurrent();
			break;
		}
	} while ( defaultPhoton.photonStatus() == PHOTON_CONTINUES );
	//
	// If the photon escaped the layer, make sure it is at the bottom.
	//
	if ( defaultPhoton.photonStatus() == PHOTON_CONTINUES ) {
		// Move the current photon position to the bottom of the current
		// outer collimator
		defaultPhoton.projectPhoton(outerCollimator->getColBottomDistance());
		defaultPhoton.setVirtualToCurrent();
		double radius, theta;
		radius = defaultPhoton.getRadius();
		theta = defaultPhoton.getRotationAngle();
	}

	return;
}

void
XrrtRaytrace::loadReflect(fitsfile* fitsReflectFile, XrrtTable* reflectTable)
{
	int      fitsStatus;        // Standard error code return from FITSIO
	long     fitsRowLimit;
	long     fitsRow;
	int      fitsAnyNull;
	int      formatVersion;
	char     formatVersionString[256];
	char     extname[FLEN_VALUE];

	char     key[16];
	int      crpx;
	double   crvl, cdlt;
	int      typecode;
	long     repeat, width;
	int      binAngleNum;
	double   binAngleZero, binAngleDelta;
	ReflectProb*  refProbArray;
	ScatProb*  scatProbArray;

	// FITSIO column references
	int      fitsColEnergy;
	int      fitsColBangle;
	int      fitsColRefprob;
	int      fitsColScatprob;

	char * const FITS_COLNAME_ENERGY    = "ENERGY";
	char * const FITS_COLNAME_BANGLE    = "BANGLE";
	char * const FITS_COLNAME_REFPROB   = "REFPROB";
	char * const FITS_COLNAME_SCATPROB  = "SCATPROB";
#define FITS_READ_COL_ENERGY   fits_read_col_dbl
#define FITS_READ_COL_BANGLE   fits_read_col_dbl
#define FITS_READ_COL_REFPROB  fits_read_col_dbl
#define FITS_READ_COL_SCATPROB fits_read_col_flt

	TableEnergy        energy;
	BinAngle           bAngle;
	ReflectProb        refProb;

	fitsStatus = 0;

	// Find EXTNAME keyword
	fits_read_key_str(fitsReflectFile, "EXTNAME", extname, NULL, &fitsStatus);
	if ( NO_ERROR != fitsStatus ) {
		throw fitsStatus;
	}

	// Find
	// CBD20001= 'FORMAT_VERSION(2)'  / Format version of XRT reflectivity file
	fits_read_key_str(fitsReflectFile, "CBD20001",
					  formatVersionString, NULL, &fitsStatus);
	if ( NO_ERROR != fitsStatus ) {
		fitsStatus = 0;
		formatVersion = 1;
	} else if ( 0 != strncmp("FORMAT_VERSION(", formatVersionString, 15) ) {
		throw invalidFormatVersionString;
	} else {
		formatVersion = atoi(formatVersionString + 15);
	}

	if ( 1 == formatVersion ) {
		// Find the FITS file matching columns for the required column names
		fits_get_colnum(fitsReflectFile, FALSE, FITS_COLNAME_ENERGY,
						&fitsColEnergy, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsEnergyCol;
		}

		fits_get_colnum(fitsReflectFile, FALSE, FITS_COLNAME_BANGLE,
						&fitsColBangle, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsBangleCol;
		}

		fits_get_colnum(fitsReflectFile, FALSE, FITS_COLNAME_REFPROB,
						&fitsColRefprob, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsRefprobCol;
		}

		// Find out how many table rows we will have
		fits_read_key_lng(fitsReflectFile, "NAXIS2", &fitsRowLimit,
						  NULL, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		// Begin loading the rows to the appropriate table
		for (fitsRow=1; fitsRow <= fitsRowLimit; fitsRow++) {

			// Read the next vertex
			FITS_READ_COL_ENERGY(fitsReflectFile, fitsColEnergy,
								 fitsRow, 1L, 1L, 0, &energy,
								 &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}

			FITS_READ_COL_BANGLE(fitsReflectFile, fitsColBangle,
								 fitsRow, 1L, 1L, 0, &bAngle,
								 &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}

			FITS_READ_COL_REFPROB(fitsReflectFile, fitsColRefprob,
								  fitsRow, 1L, 1L, 0, &refProb,
								  &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			defaultReflection.setReflectTable(reflectTable,
											  energy,
											  bAngle,
											  refProb);
		}

	} else if ( 2 == formatVersion || 3 == formatVersion ) {
		// Find the FITS file matching columns for the required column names
		fits_get_colnum(fitsReflectFile, FALSE, FITS_COLNAME_ENERGY,
						&fitsColEnergy, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsEnergyCol;
		}

		fits_get_colnum(fitsReflectFile, FALSE, FITS_COLNAME_REFPROB,
						&fitsColRefprob, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsRefprobCol;
		}

		fits_get_colnum(fitsReflectFile, FALSE, FITS_COLNAME_SCATPROB,
						&fitsColScatprob, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			fitsStatus = 0;		// ignore error
			fitsColScatprob = -1;
		} else {
			fflush(NULL);
			printf("\
%s: '%s' column found in the reflect file, reading it.\n",
				xrrtgetpname(), FITS_COLNAME_SCATPROB);
			fflush(NULL);
		}

		// Find out how many table rows we will have
		fits_read_key_lng(fitsReflectFile, "NAXIS2", &fitsRowLimit,
						  NULL, &fitsStatus);
		sprintf(key, "1CRPX%d", fitsColRefprob);
		fits_read_key(fitsReflectFile, TINT, key, &crpx, NULL, &fitsStatus);
		sprintf(key, "1CRVL%d", fitsColRefprob);
		fits_read_key_dbl(fitsReflectFile, key, &crvl, NULL, &fitsStatus);
		sprintf(key, "1CDLT%d", fitsColRefprob);
		fits_read_key_dbl(fitsReflectFile, key, &cdlt, NULL, &fitsStatus);
		fits_get_coltype(fitsReflectFile, fitsColRefprob,
						 &typecode, &repeat, &width, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		binAngleNum = repeat;
		binAngleZero = crvl;
		if ( 1 != crpx ) {
			binAngleZero -= (crpx - 1)*cdlt;
		}
		binAngleDelta = cdlt;

		fflush(NULL); printf("\
NAXIS2=%ld, binAngleNum=%d, binAngleZero=%f, binAngleDelta=%e\n",
			fitsRowLimit, binAngleNum, binAngleZero, binAngleDelta);
		fflush(NULL);

		// Begin loading the rows to the appropriate table
		for (fitsRow=1; fitsRow <= fitsRowLimit; fitsRow++) {

			refProbArray = new ReflectProb[binAngleNum];
			if ( NULL == refProbArray ) {
				throw outOfMemory;
			}

			scatProbArray = NULL;
			if ( 0 < fitsColScatprob || 0 == strcasecmp("AEFront", extname) ) {
				scatProbArray = new ScatProb[binAngleNum];
				if ( NULL == scatProbArray ) {
					throw outOfMemory;
				}
			}

			// Read the next vertex
			FITS_READ_COL_ENERGY(fitsReflectFile, fitsColEnergy,
								 fitsRow, 1L, 1L, 0, &energy,
								 &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}

			FITS_READ_COL_REFPROB(fitsReflectFile, fitsColRefprob,
								  fitsRow, 1L, repeat, 0, refProbArray,
								  &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}

			if ( NULL != scatProbArray ) {
				if ( 0 < fitsColScatprob ) {
					FITS_READ_COL_SCATPROB(fitsReflectFile, fitsColScatprob,
										   fitsRow, 1L, repeat, 0,
										   scatProbArray,
										   &fitsAnyNull, &fitsStatus);
					if (fitsStatus != NO_ERROR) {
						throw fitsStatus;
					}
				} else {
					for (int i = 0; i < binAngleNum; i++) {
						scatProbArray[i] = -1.0;
					}
				}
			}

			defaultReflection.setReflectTable2(reflectTable,
											   energy,
											   binAngleNum,
											   binAngleZero,
											   binAngleDelta,
											   refProbArray,
											   scatProbArray);
		}

	} else {
		throw unknownFormatVersion;
	}

	reflectTable->setTableIndex();
}

//
// Add a function to load the pre-collimator description file overloaded
// the normal loadMirrorDesc() for the Astro-E2 pre-collimator
// (added by Hideyuki MORI)
//
// Add the extension name which indicates the part of the quadrant
// information
// (added by Hideyuki MORI : date 2006/02/28)
void
XrrtRaytrace::loadMirrorDesc(string mirrorFileName,
							 string mirrorExtension,
							 string obstructExtension,
							 string quadrantExtension,
							 string pcolExtension,
							 string reflectFileName,
	                         string backprofFileName)
{

//
// Load the telescope description file and the mirror associated
// reflection tables.
//
// FITS FILE RELATED DEFAULTS
	char * const FITS_EXTENSION_NAME   = "EXTNAME";
	char * const FITS_COLNAME_LAYER    = "layer";
	char * const FITS_COLNAME_ASSEMBLY = "assembly";
	char * const FITS_COLNAME_NUMBER   = "number";
	char * const FITS_COLNAME_FRAGMENT = "fragment";
	char * const FITS_COLNAME_FUNCTION = "function";
	char * const FITS_COLNAME_SCATTER  = "scatter";
	char * const FITS_COLNAME_FREFLECT = "freflect";
	char * const FITS_COLNAME_BREFLECT = "breflect";
	char * const FITS_COLNAME_FSTART   = "fstart";
	char * const FITS_COLNAME_SCROSS   = "scross";
	char * const FITS_COLNAME_FEND     = "fend";
	char * const FITS_COLNAME_ECROSS   = "ecross";
	char * const FITS_COLNAME_TOPINR   = "topinr";
	char * const FITS_COLNAME_TOPOUTR  = "topoutr";
	char * const FITS_COLNAME_BOTINR   = "botinr";
	char * const FITS_COLNAME_BOTOUTR  = "botoutr";
	char * const FITS_COLNAME_TOPD     = "topd";
	char * const FITS_COLNAME_BOTD     = "botd";
#define FITS_READ_COL_LAYER    fits_read_col_int
#define FITS_READ_COL_ASSEMBLY fits_read_col_int
#define FITS_READ_COL_NUMBER   fits_read_col_int
#define FITS_READ_COL_FRAGMENT fits_read_col_int
#define FITS_READ_COL_FUNCTION fits_read_col_int
#define FITS_READ_COL_SCATTER  fits_read_col_int
#define FITS_READ_COL_FREFLECT fits_read_col_str
#define FITS_READ_COL_BREFLECT fits_read_col_str
#define FITS_READ_COL_FSTART   fits_read_col_dbl
#define FITS_READ_COL_SCROSS   fits_read_col_log
#define FITS_READ_COL_FEND     fits_read_col_dbl
#define FITS_READ_COL_ECROSS   fits_read_col_log
#define FITS_READ_COL_TOPINR   fits_read_col_dbl
#define FITS_READ_COL_TOPOUTR  fits_read_col_dbl
#define FITS_READ_COL_BOTINR   fits_read_col_dbl
#define FITS_READ_COL_BOTOUTR  fits_read_col_dbl
#define FITS_READ_COL_TOPD     fits_read_col_dbl
#define FITS_READ_COL_BOTD     fits_read_col_dbl

	char * const FITS_COLNAME_PLAYER     = "layer";
	char * const FITS_COLNAME_POLYNUM    = "polynum";
	char * const FITS_COLNAME_DISTANCE   = "distance";
	char * const FITS_COLNAME_XVERTEX    = "xvertex";
	char * const FITS_COLNAME_YVERTEX    = "yvertex";
#define FITS_READ_COL_PLAYER    fits_read_col_int
#define FITS_READ_COL_POLYNUM   fits_read_col_int
#define FITS_READ_COL_DISTANCE  fits_read_col_dbl
#define FITS_READ_COL_XVERTEX   fits_read_col_dbl
#define FITS_READ_COL_YVERTEX   fits_read_col_dbl

//
// Load the information of the quadrant coordinates
// especially for Astro-E2 XRTs
// (added by Hideyuki MORI : date 2006/02/28)
	char * const FITS_COLNAME_QUADRANT  = "quadrant";
	char * const FITS_COLNAME_QLAYER    = "layer";
	char * const FITS_COLNAME_QDELTAX   = "deltax";
	char * const FITS_COLNAME_QDELTAY   = "deltay";
	char * const FITS_COLNAME_QDELTAZ   = "deltaz";
	char * const FITS_COLNAME_QDELTATX  = "deltatx";
	char * const FITS_COLNAME_QDELTATY  = "deltaty";
	char * const FITS_COLNAME_QDELTATZ  = "deltatz";
#define FITS_READ_COL_QUADRANT  fits_read_col_int
#define FITS_READ_COL_QLAYER    fits_read_col_int
#define FITS_READ_COL_QDELTAX   fits_read_col_dbl
#define FITS_READ_COL_QDELTAY   fits_read_col_dbl
#define FITS_READ_COL_QDELTAZ   fits_read_col_dbl
#define FITS_READ_COL_QDELTATX   fits_read_col_dbl
#define FITS_READ_COL_QDELTATY   fits_read_col_dbl
#define FITS_READ_COL_QDELTATZ   fits_read_col_dbl

//
// Load the collimator description file
// (added by Hideyuki MORI)
	char * const FITS_COLNAME_PCOL_LAYER    = "layer";
	char * const FITS_COLNAME_PCOL_ASSEMBLY = "assembly";
	char * const FITS_COLNAME_PCOL_NUMBER   = "number";
	char * const FITS_COLNAME_PCOL_FRAGMENT = "fragment";
	char * const FITS_COLNAME_PCOL_FUNCTION = "function";
	char * const FITS_COLNAME_PCOL_SCATTER  = "scatter";
	char * const FITS_COLNAME_PCOL_REFLECT  = "freflect";
	char * const FITS_COLNAME_PCOL_FSTART   = "fstart";
	char * const FITS_COLNAME_PCOL_SCROSS   = "scross";
	char * const FITS_COLNAME_PCOL_FEND     = "fend";
	char * const FITS_COLNAME_PCOL_ECROSS   = "ecross";
	char * const FITS_COLNAME_PCOL_TOPINR   = "topinr";
	char * const FITS_COLNAME_PCOL_TOPOUTR  = "topoutr";
	char * const FITS_COLNAME_PCOL_BOTINR   = "botinr";
	char * const FITS_COLNAME_PCOL_BOTOUTR  = "botoutr";
	char * const FITS_COLNAME_PCOL_TOPD     = "topd";
	char * const FITS_COLNAME_PCOL_BOTD     = "botd";
#define FITS_READ_COL_PCOL_LAYER    fits_read_col_int
#define FITS_READ_COL_PCOL_ASSEMBLY fits_read_col_int
#define FITS_READ_COL_PCOL_NUMBER   fits_read_col_int
#define FITS_READ_COL_PCOL_FRAGMENT fits_read_col_int
#define FITS_READ_COL_PCOL_FUNCTION fits_read_col_int
#define FITS_READ_COL_PCOL_SCATTER  fits_read_col_int
#define FITS_READ_COL_PCOL_REFLECT  fits_read_col_str
#define FITS_READ_COL_PCOL_FSTART   fits_read_col_dbl
#define FITS_READ_COL_PCOL_SCROSS   fits_read_col_log
#define FITS_READ_COL_PCOL_FEND     fits_read_col_dbl
#define FITS_READ_COL_PCOL_ECROSS   fits_read_col_log
#define FITS_READ_COL_PCOL_TOPINR   fits_read_col_dbl
#define FITS_READ_COL_PCOL_TOPOUTR  fits_read_col_dbl
#define FITS_READ_COL_PCOL_BOTINR   fits_read_col_dbl
#define FITS_READ_COL_PCOL_BOTOUTR  fits_read_col_dbl
#define FITS_READ_COL_PCOL_TOPD     fits_read_col_dbl
#define FITS_READ_COL_PCOL_BOTD     fits_read_col_dbl

// Local variables
	XrrtObstruction* obstruction = NULL;
	string reflectTableString;
	XrrtTable* reflectTable;
	vector<string>::iterator found;

// Standard FITSIO calling sequence
	int      fitsStatus;        // Standard error code return from FITSIO
	int      fitsHduType;       // Std FITS HDU type return
	char     fitsChar[256];     // Dummy catch for FITS return string
	char     fitsComment[256];  // Dummy catch for FITS Comment return
	long     fitsRowLimit;
	long     fitsRow;
	int      fitsAnyNull;

// FITSIO column references
	int      fitsColLayer;
	int      fitsColAssembly;
	int      fitsColNumber;
	int      fitsColFragment;
	int      fitsColFunction;
	int      fitsColScatter;
	int      fitsColFreflect;
	int      fitsColBreflect;
	int      fitsColFstart;
	int      fitsColScross;
	int      fitsColFend;
	int      fitsColEcross;
	int      fitsColTopinr;
	int      fitsColTopoutr;
	int      fitsColBotinr;
	int      fitsColBotoutr;
	int      fitsColTopd;
	int      fitsColBotd;

	int      fitsColPlayer;
	int      fitsColPolynum;
	int      fitsColDistance;
	int      fitsColXvertex;
	int      fitsColYvertex;

// FITSIO column reference for the quadrant information
// (added by Hideyuki MORI : date 2006/02/28)
	int      fitsColQuadrant;
	int      fitsColQlayer;
	int      fitsColDeltax;
	int      fitsColDeltay;
	int      fitsColDeltaz;
	int      fitsColDeltaTx;
	int      fitsColDeltaTy;
	int      fitsColDeltaTz;

// FITSIO column reference for the Astro-E2 pre-collimator
// (added by Hideyuki MORI)
	int      fitsColPcolLayer;
	int      fitsColPcolAssembly;
	int      fitsColPcolNumber;
	int      fitsColPcolFragment;
	int      fitsColPcolFunction;
	int      fitsColPcolScatter;
	int      fitsColPcolReflect;
	int      fitsColPcolFstart;
	int      fitsColPcolScross;
	int      fitsColPcolFend;
	int      fitsColPcolEcross;
	int      fitsColPcolTopinr;
	int      fitsColPcolTopoutr;
	int      fitsColPcolBotinr;
	int      fitsColPcolBotoutr;
	int      fitsColPcolTopd;
	int      fitsColPcolBotd;

// FITS column return values
	int                fitsLayer;
	int                fitsAssembly;
	int                fitsNumber;
	int                fitsFragment;
	SurfaceFunction    surfaceFunction;
	SurfaceScatterMode scatterMode;
	AngleInRadians     startAngle;
	char               sCross[2];
	AngleInRadians     endAngle;
	char               eCross[2];
	RadiusInMM         topInnerRadius;
	RadiusInMM         topOuterRadius;
	RadiusInMM         bottomInnerRadius;
	RadiusInMM         bottomOuterRadius;
	FPDistanceInMM     topDistance;
	FPDistanceInMM     bottomDistance;
	char               frontReflectTable[256];
	char               backReflectTable[256];
	int                fitsPLayer;
	int                polynum;
	FPDistanceInMM     fpDistance;
	VertexInMM         xvertex;
	VertexInMM         yvertex;

// FITS column return values for the quadrant information
// (added by Hideyuki MORI : date 2006/02/28)
	int                fitsQuadrant;
	int                quadrantLayer;
	double             deltaX;
	double             deltaY;
	double             deltaZ;
	double             deltaTx;
	double             deltaTy;
	double             deltaTz;

// FITS column return values for the Astro-E2 pre-collimator
// (added by Hideyuki MORI)
	int                fitsPcolLayer;
	int                fitsPcolAssembly;
	int                fitsPcolNumber;
	int                fitsPcolFragment;
	SurfaceFunction    pcolSurfaceFunction;
	SurfaceScatterMode pcolScatterMode;
	AngleInRadians     pcolStartAngle;
	char               pcolSCross[2];
	AngleInRadians     pcolEndAngle;
	char               pcolECross[2];
	RadiusInMM         pcolTopInnerRadius;
	RadiusInMM         pcolTopOuterRadius;
	RadiusInMM         pcolBottomInnerRadius;
	RadiusInMM         pcolBottomOuterRadius;
	FPDistanceInMM     pcolTopDistance;
	FPDistanceInMM     pcolBottomDistance;
	char               pcolReflectTable[256];

// FITS header keyword values (added by Y.ISHISAKI)

	double mirrorThickness;
	double mirrorMissAlignment;
	char mirrorMissAlignmentMode[FLEN_VALUE];

	double pcolThickness;
	double pcolMissAlignment;
	char pcolMissAlignmentMode[FLEN_VALUE];

	long scatmode = DEFAULT_SCATTER_MODE;

	long   consct = DEFAULT_ASCA_CONSCT;
	double sigma1 = DEFAULT_ASCA_SIGMA1;
	double sigma2 = DEFAULT_ASCA_SIGMA2;
	double lalin  = DEFAULT_ASCA_LALIN;
	double calss3 = DEFAULT_ASCA_CALSS3;
	double calsc3 = DEFAULT_ASCA_CALSC3;

	long   nvfsw    = DEFAULT_SUZAKU_NVFSW;
	double gausigma = DEFAULT_SUZAKU_GAUSIGMA;
	double ngau     = DEFAULT_SUZAKU_NGAU;
	double expsigma = DEFAULT_SUZAKU_EXPSIGMA;
	double nexp     = DEFAULT_SUZAKU_NEXP;
	double lorgamma = DEFAULT_SUZAKU_LORGAMMA;
	double nlor     = DEFAULT_SUZAKU_NLOR;
	long   scatsw   = DEFAULT_SUZAKU_SCATSW;
	double w_norm   = DEFAULT_SUZAKU_W_NORM;
	double w_power  = DEFAULT_SUZAKU_W_POWER;
	double g_norm   = DEFAULT_SUZAKU_G_NORM;

	double gc_a     = DEFAULT_SUZAKU_PCOL_GC_A;
	double gc_b     = DEFAULT_SUZAKU_PCOL_GC_B;
	double gw_a     = DEFAULT_SUZAKU_PCOL_GW_A;
	double gw_b     = DEFAULT_SUZAKU_PCOL_GW_B;

	fitsfile* fitsMirrorFile;    // FITSIO FILE* pointer
	fitsfile* fitsReflectFile;   // FITSIO FILE* pointer

	const char *charMirrorFileName = mirrorFileName.c_str();

	fflush(NULL);
	printf("\
%s\n\
Reading '%s' ...\n", xrrtgetcredit(), charMirrorFileName);
	fflush(NULL);

	// Open the mirror file
	fitsStatus = 0;
	fits_open_file(&fitsMirrorFile, charMirrorFileName, READONLY, &fitsStatus);
	if ( fitsStatus != NO_ERROR ) {
		throw fitsStatus;
	}

// Find the mirror extension in the Mirror file
// Scan all possible extentions looking for the Mirror extension
// 10000 is an arbitary large number
// 2 is the 1st extension of a FITS file
	for (int i=2; i<10000; i++) {
		fits_movabs_hdu(fitsMirrorFile, i, &fitsHduType, &fitsStatus);
		if (fitsStatus == END_OF_FILE) {
			throw noMirrorExtension;
		}
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		fits_read_key_str(fitsMirrorFile, FITS_EXTENSION_NAME,
						  fitsChar, fitsComment, &fitsStatus);
		if ( 0 == strcasecmp(fitsChar, mirrorExtension.c_str()) ) {
			// Mirror extension found
			break;
		}
	}
	//
	// We now have the mirror extension as the file position
	//
	// Find the FITS file matching columns for the required column names
	//
	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_LAYER,
					&fitsColLayer, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsLayerCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_ASSEMBLY,
					&fitsColAssembly, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsAssemblyCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_NUMBER,
					&fitsColNumber, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsNumberCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_FRAGMENT,
					&fitsColFragment, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsFragmentCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_FUNCTION,
					&fitsColFunction, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsFunctionCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_SCATTER,
					&fitsColScatter, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsScatterCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_FREFLECT,
					&fitsColFreflect, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsFrelectCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_BREFLECT,
					&fitsColBreflect, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsBreflectCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_FSTART,
					&fitsColFstart, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsFstartCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_SCROSS,
					&fitsColScross, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsScrossCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_FEND,
					&fitsColFend, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsFendCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_ECROSS,
					&fitsColEcross, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsEcrossCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_TOPINR,
					&fitsColTopinr, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsTopinrCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_TOPOUTR,
					&fitsColTopoutr, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsTopoutrCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_BOTINR,
					&fitsColBotinr, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsBotinrCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_BOTOUTR,
					&fitsColBotoutr, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsBotoutrCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_TOPD,
					&fitsColTopd, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsTopdCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_BOTD,
					&fitsColBotd, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsBotdCol;
	}

	//
	// Find the nominal focal length in MM
	//
	double focalLengthMM;
	fits_read_key_dbl(fitsMirrorFile, "FOCALLEN", &focalLengthMM,
					  fitsComment, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw fitsStatus;
	}
	show_header_card(fitsMirrorFile, "FOCALLEN");

	//
	// Tell the structure what the focal length is
	//
	structure.setFocalLengthMM(focalLengthMM);
	//
	// Find the minimum telescope photon radius
	//
	double minRadius;
	fits_read_key_dbl(fitsMirrorFile, "PMINRAD", &minRadius,
					  fitsComment, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw fitsStatus;
	}
	minimumTelescopePhotonRadiusMM = minRadius;
	show_header_card(fitsMirrorFile, "PMINRAD");

	//
	// Find the maximum telescope photon radius
	//
	double maxRadius;
	fits_read_key_dbl(fitsMirrorFile, "PMAXRAD", &maxRadius,
					  fitsComment, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw fitsStatus;
	}
	maximumTelescopePhotonRadiusMM = maxRadius;
	show_header_card(fitsMirrorFile, "PMAXRAD");

	// 2008-03-02 Y.ISHISAKI	version 6.5.0
	// Find out mirror thickness, missalignment
	//
	fits_read_key_dbl(fitsMirrorFile, "THICKNES", &mirrorThickness,
					  fitsComment, &fitsStatus);
	fits_read_key_str(fitsMirrorFile, "MALIGN_M", mirrorMissAlignmentMode,
					  fitsComment, &fitsStatus);
	fits_read_key_dbl(fitsMirrorFile, "MALIGN_N", &mirrorMissAlignment,
					  fitsComment, &fitsStatus);
	if ( fitsStatus ) {
		fitsStatus = 0;		// ignore error & load default values
		mirrorThickness = DEFAULT_SUZAKU_THICKNES;
		strcpy(mirrorMissAlignmentMode,DEFAULT_SUZAKU_MALIGN_M);
		mirrorMissAlignment = DEFAULT_SUZAKU_MALIGN_N;
		printf("\
foilthickness=%.4f, missalignmode='%s', missalignment=%.4f\n",
			mirrorThickness, mirrorMissAlignmentMode, mirrorMissAlignment);
	} else {
		show_header_card(fitsMirrorFile, "THICKNES");
		show_header_card(fitsMirrorFile, "MALIGN_M");
		show_header_card(fitsMirrorFile, "MALIGN_N");
	}

	//
	// Find out how many Mirror fragments we will have
	//
	fits_read_key_lng(fitsMirrorFile, "NAXIS2", &fitsRowLimit,
					  fitsComment, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw fitsStatus;
	}

	//
	// Begin loading the Mirror fragments into the Mirror class
	//
	// For each mirror fragment
	for (fitsRow=1; fitsRow <= fitsRowLimit; fitsRow++) {
		//
		// Obtain a mirror object
		//
		XrrtMirror* mirror = structure.createMirror();
		//
		// Setup mirror thickness and miss alignment (modified by H. Mori)
		//
		mirror->setMirrorThickness(mirrorThickness);
		mirror->setMirrorMissAlignmentMode(mirrorMissAlignmentMode);
		mirror->setMirrorMissAlignment(mirrorMissAlignment);
		//
		// Read the next fragment
		//
		FITS_READ_COL_LAYER(fitsMirrorFile, fitsColLayer,
							fitsRow, 1L, 1L, 0, &fitsLayer,
							&fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setFitsLayer(fitsLayer);

		FITS_READ_COL_ASSEMBLY(fitsMirrorFile, fitsColAssembly,
							   fitsRow, 1L, 1L, 0, &fitsAssembly,
							   &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setFitsAssembly(fitsAssembly);

		FITS_READ_COL_NUMBER(fitsMirrorFile, fitsColNumber,
							 fitsRow, 1L, 1L, 0, &fitsNumber,
							 &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setFitsMirrorNumber(fitsNumber);

		FITS_READ_COL_FRAGMENT(fitsMirrorFile, fitsColFragment,
							   fitsRow, 1L, 1L, 0, &fitsFragment,
							   &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setFitsFragmentNumber(fitsFragment);

		FITS_READ_COL_FUNCTION(fitsMirrorFile, fitsColFunction,
							   fitsRow, 1L, 1L, 0, &surfaceFunction,
							   &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setSurfaceFunction(surfaceFunction);

		FITS_READ_COL_SCATTER(fitsMirrorFile, fitsColScatter,
							  fitsRow, 1L, 1L, 0, &scatterMode,
							  &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setScatterMode(scatterMode);
		//
		// keep track of the various scatter modes required.
		//
		if (scatterMode == 0) {
			scatterNoneFound = true;
		} else if (scatterMode == 1) {
			scatterAscaFound = true;
		} else if (scatterMode == 2) {
			scatterAstroeFound = true;
		}

		char* charArray[2];
		charArray[0] = frontReflectTable;
		charArray[1] = 0;
		strcpy(frontReflectTable,"    ");
		FITS_READ_COL_FREFLECT(fitsMirrorFile, fitsColFreflect,
							   fitsRow, 1, 1, "", charArray,
							   &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		// Convert the char to a string and then see if it has been remapped to
		//     a different name
		// 2008-03-03 Y.ISHISAKI	version 6.5.0
		// Upcase string in the column
		reflectTableString = upcase_string(frontReflectTable);
		reflectTableString = frontRemap.getRemapName(reflectTableString);
		//   ofstream fout("test");
		//   fout << reflectTableString;
		//   fout << reflectTableString << "\n";
		//
		// If we are processing a reflection table file we need a table index
		// If we are dynamically calculating, the table pointer is zero
		if (dynamicReflectionTable) {
			mirror->setFrontReflectTable(0);
		} else {
			// Get a table index for the table name
			reflectTable = getFrontTableEntry(reflectTableString);
			mirror->setFrontReflectTable(reflectTable);
		}

		charArray[0] = backReflectTable;
		FITS_READ_COL_BREFLECT(fitsMirrorFile, fitsColBreflect,
							   fitsRow, 1L, 1L, "", charArray,
							   &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		// Convert the char to a string and then see if it has been remapped to
		//     a different name
		// 2008-03-03 Y.ISHISAKI	version 6.5.0
		// Upcase string in the column
		reflectTableString = upcase_string(backReflectTable);
		reflectTableString = backRemap.getRemapName(reflectTableString);
		//   fout << reflectTableString;
		//   fout << reflectTableString << "\n";
		//   fout.close();
		//
		// If we are processing a reflection table file we need a table index
		// If we are dynamically calculating, the table pointer is zero
		if (dynamicReflectionTable) {
			mirror->setBackReflectTable(0);
		} else {
			// Get a table index for the table name
			reflectTable = getBackTableEntry( reflectTableString);
			mirror->setBackReflectTable(reflectTable);
		}

		FITS_READ_COL_FSTART(fitsMirrorFile, fitsColFstart,
							 fitsRow, 1L, 1L, 0,
							 &startAngle, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setStartAngle(startAngle);

		FITS_READ_COL_SCROSS(fitsMirrorFile, fitsColScross,
							 fitsRow, 1L, 1L, 0,
							 sCross, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		if (sCross[0] == 0x01) {
			mirror->setStartAngleCross(true);
		} else if (sCross[0] == 0x00) {
			mirror->setStartAngleCross(false);
		} else {
			throw invalidScross;
		}

		FITS_READ_COL_FEND(fitsMirrorFile, fitsColFend,
						   fitsRow, 1L, 1L, 0,
						   &endAngle, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setEndAngle(endAngle);

		FITS_READ_COL_ECROSS(fitsMirrorFile, fitsColEcross,
							 fitsRow, 1L, 1L, 0,
							 eCross, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		if (eCross[0] == 0x01) {
			mirror->setEndAngleCross(true);
		} else if (eCross[0] == 0x00) {
			mirror->setEndAngleCross(false);
		} else {
			throw invalidEcross;
		}

		FITS_READ_COL_TOPINR(fitsMirrorFile, fitsColTopinr,
							 fitsRow, 1L, 1L, 0,
							 &topInnerRadius, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setTopInnerRadius(topInnerRadius);

		FITS_READ_COL_TOPOUTR(fitsMirrorFile, fitsColTopoutr,
							  fitsRow, 1L, 1L, 0,
							  &topOuterRadius, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setTopOuterRadius(topOuterRadius);

		FITS_READ_COL_BOTINR(fitsMirrorFile, fitsColBotinr,
							 fitsRow, 1L, 1L, 0,
							 &bottomInnerRadius, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setBottomInnerRadius(bottomInnerRadius);

		FITS_READ_COL_BOTOUTR(fitsMirrorFile, fitsColBotoutr,
							  fitsRow, 1L, 1L, 0,
							  &bottomOuterRadius, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setBottomOuterRadius(bottomOuterRadius);

		FITS_READ_COL_TOPD(fitsMirrorFile, fitsColTopd,
						   fitsRow, 1L, 1L, 0,
						   &topDistance, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setTopDistance(topDistance);

		FITS_READ_COL_BOTD(fitsMirrorFile, fitsColBotd,
						   fitsRow, 1L, 1L, 0,
						   &bottomDistance, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		mirror->setBottomDistance(bottomDistance);

		if (surfaceFunction == PLANE_MIRROR) {
			//
			// Compute all the derived values based on the fixed values just
			// provided
			//
			mirror->computePlaneMirrorData();
		}
	}

//
// Find and load the Obstruction tables
//
// Find the Obstruction extension in the Mirror file
// Scan all possible extentions looking for the Obstruction extension
// 10000 is an arbitary large number
// 2 is the 1st extension of a FITS file
	for (int i=2; i<10000; i++) {
		fits_movabs_hdu(fitsMirrorFile, i, &fitsHduType, &fitsStatus);
		if (fitsStatus == END_OF_FILE) {
			throw noObstructionExtension;
		}
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		fits_read_key_str(fitsMirrorFile, FITS_EXTENSION_NAME,
						  fitsChar, fitsComment, &fitsStatus);
		if ( 0 == strcasecmp(fitsChar, obstructExtension.c_str()) ) {
			// Obstruction extension found
			break;
		}
	}
	//
	// We now have the Obstruction extension as the file position
	//
	// Find the FITS file matching columns for the required column names
	//
	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PLAYER,
					&fitsColPlayer, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsPlayerCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_POLYNUM,
					&fitsColPolynum, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsPolynumCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_DISTANCE,
					&fitsColDistance, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsDistanceCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_XVERTEX,
					&fitsColXvertex, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsXvertexCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_YVERTEX,
					&fitsColYvertex, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsYvertexCol;
	}

	//
	// Find the radius of the inner housing
	//
	double innerHousingRadius;
	fits_read_key_dbl(fitsMirrorFile, "MINRAD", &innerHousingRadius,
					  fitsComment, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw fitsStatus;
	}
	//
	// Tell the structure where the inner housing begins
	//
	structure.setInnerHousingRadius(innerHousingRadius);
	//
	// Find the radius of the outer housing
	//
	double outerHousingRadius;
	fits_read_key_dbl(fitsMirrorFile, "MAXRAD", &outerHousingRadius,
					  fitsComment, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw fitsStatus;
	}
	//
	// Tell the structure where the outer housing begins
	//
	structure.setOuterHousingRadius(outerHousingRadius);

	//
	// Find out how many Obstruction vertexes we will have
	//
	fits_read_key_lng(fitsMirrorFile, "NAXIS2", &fitsRowLimit,
					  fitsComment, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw fitsStatus;
	}

	//
	// Begin loading the vertex points into the Obstruction class
	//
	int lastPolynum = -1;
	int lastFitsPLayer = -1;
	for (fitsRow=1; fitsRow <= fitsRowLimit; fitsRow++) {

		// Read the next vertex
		FITS_READ_COL_PLAYER(fitsMirrorFile, fitsColPlayer,
							 fitsRow, 1L, 1L, 0, &fitsPLayer,
							 &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		FITS_READ_COL_POLYNUM(fitsMirrorFile, fitsColPolynum,
							  fitsRow, 1L, 1L, 0, &polynum,
							  &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		if (fitsPLayer != lastFitsPLayer || polynum != lastPolynum) {
			// Obtain an Obstruction object
			obstruction = structure.createObstruction();
		}
		lastPolynum = polynum;
		lastFitsPLayer  = fitsPLayer;
		obstruction->setFitsLayer(fitsPLayer);
		obstruction->setFitsPolynum(polynum);

		FITS_READ_COL_DISTANCE(fitsMirrorFile, fitsColDistance,
							   fitsRow, 1L, 1L, 0,
							   &fpDistance, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		obstruction->setFPDistance(fpDistance);

		FITS_READ_COL_XVERTEX(fitsMirrorFile, fitsColXvertex,
							  fitsRow, 1L, 1L, 0,
							  &xvertex, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		FITS_READ_COL_YVERTEX(fitsMirrorFile, fitsColYvertex,
							  fitsRow, 1L, 1L, 0,
							  &yvertex, &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		obstruction->setVertex(xvertex, yvertex);
		obstruction->setQuadrant();
	}

//
// Find and load the Quadrant tables
//
// Find the Quadrant extension in the Mirror file
// Scan all possible extentions looking for the Quadrant extension
// 10000 is an arbitary large number
// 2 is the 1st extension of a FITS file
// (added by Hideyuki MORI : date 2006/02/28)
	for (int i=2; i<10000; i++) {
		fits_movabs_hdu(fitsMirrorFile, i, &fitsHduType, &fitsStatus);
		if (fitsStatus == END_OF_FILE) {
			throw noQuadrantExtension;
		}
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		fits_read_key_str(fitsMirrorFile, FITS_EXTENSION_NAME,
						  fitsChar, fitsComment, &fitsStatus);
		if ( 0 == strcasecmp(fitsChar, quadrantExtension.c_str()) ) {
			// Quadrant extension found
			break;
		}
	}

//
// We now have the Quadrant extension as the file position
//
// Find the FITS file matching columns for the required column names
//
	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_QUADRANT,
					&fitsColQuadrant, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsQuadrantCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_QLAYER,
					&fitsColQlayer, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsQlayerCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_QDELTAX,
					&fitsColDeltax, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsDeltaxCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_QDELTAY,
					&fitsColDeltay, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsDeltayCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_QDELTAZ,
					&fitsColDeltaz, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsDeltazCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_QDELTATX,
					&fitsColDeltaTx, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsDeltaTxCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_QDELTATY,
					&fitsColDeltaTy, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsDeltaTyCol;
	}

	fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_QDELTATZ,
					&fitsColDeltaTz, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw noFitsDeltaTzCol;
	}

	//
	// Find out how many Quadrant informations we will have
	//
	fits_read_key_lng(fitsMirrorFile, "NAXIS2", &fitsRowLimit,
					  fitsComment, &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw fitsStatus;
	}

	//
	// Begin loading the quadrant informations, (dx, dy, dx) and
	// (dTx, dTy, dTz), into the Quadrant class
	//
	for (fitsRow=1; fitsRow <= fitsRowLimit; fitsRow++) {

		// Read the next vertex
		FITS_READ_COL_QUADRANT(fitsMirrorFile, fitsColQuadrant,
							   fitsRow, 1L, 1L, 0, &fitsQuadrant,
							   &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		FITS_READ_COL_QLAYER(fitsMirrorFile, fitsColQlayer,
							 fitsRow, 1L, 1L, 0, &quadrantLayer,
							 &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		quadrant.setFitsQuadrant(fitsQuadrant);
		quadrant.setFitsQuadrantLayer(quadrantLayer);

		FITS_READ_COL_QDELTAX(fitsMirrorFile, fitsColDeltax,
							  fitsRow, 1L, 1L, 0, &deltaX,
							  &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		FITS_READ_COL_QDELTAY(fitsMirrorFile, fitsColDeltay,
							  fitsRow, 1L, 1L, 0, &deltaY,
							  &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		FITS_READ_COL_QDELTAZ(fitsMirrorFile, fitsColDeltaz,
							  fitsRow, 1L, 1L, 0, &deltaZ,
							  &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		quadrant.setDisplacement(deltaX, deltaY, deltaZ);

		FITS_READ_COL_QDELTATX(fitsMirrorFile, fitsColDeltaTx,
							   fitsRow, 1L, 1L, 0, &deltaTx,
							   &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		FITS_READ_COL_QDELTATY(fitsMirrorFile, fitsColDeltaTy,
							   fitsRow, 1L, 1L, 0, &deltaTy,
							   &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		FITS_READ_COL_QDELTATZ(fitsMirrorFile, fitsColDeltaTz,
							   fitsRow, 1L, 1L, 0, &deltaTz,
							   &fitsAnyNull, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		quadrant.setRotationAngle(deltaTx, deltaTy, deltaTz);

		// Check the initialization of the quadrant informations
		// (added by Hideyuki MORI : date 2006/03/04)
		// quadrant.checkInitialization();
	}

	//
	// EXECUTE ONE OF THE FOLLOWING BLOCKS OF CODE
	//
	if ( dynamicReflectionTable ) {

		//
		// Load the Atomic Scatter Factors and Atomic Data files from the CALDB
		//

		fflush(NULL);
		printf("\
Reading '%s' ...\n", atomicDataFileName.c_str());
		fflush(NULL);

		//
		// obsolete, done in XrrtRaytrace::setAtomicDataFileNames()
		//
		//if (!overrideCALDBAtomicFiles) {
		//	atomicScatterFactorsFileName = findAtomScatFactorFileName();
		//	atomicDataFileName = findAtomDataFileName();
		//}

		// Load the atomic data table
		try {
			string extension = "ATOMDATA";
			theAtomicDataTable().loadTableFromFits(atomicDataFileName, extension);
		} catch (int fitsErrors) {
			fits_report_error(stderr, fitsErrors);
			c_fcerr("from theAtomicDataTable().loadTableFromFits()");
			c_fcerr("from XrrtRaytrace::loadMirrorDesc()");
			throw errorLoadingReflectionTables;
		}

		//
		// Load atomic Scatter Factors Table from FITS

		fflush(NULL);
		printf("\
Reading '%s' ...\n", atomicScatterFactorsFileName.c_str());
		fflush(NULL);

		try {
			string extension = "henke97";
			theAtomicScatterFactorTable().loadTableFromFITS(
															atomicScatterFactorsFileName, extension);
		} catch (int fitsErrors) {
			fits_report_error(stderr, fitsErrors);
			c_fcerr("from theAtomicScatterFactorTable().loadTableFromFits()");
			c_fcerr("from XrrtRaytrace::loadMirrorDesc()");
			throw errorLoadingReflectionTables;
		}
		// END LOAD ATOMIC SCATTER FACTORS AND ATOMIC DATA FILE

	} else {	// if (!dynamicReflectionTable)

		fflush(NULL);
		printf("\
Reading '%s' ...\n", reflectFileName.c_str());
		fflush(NULL);

		//
		// READ REFLECTION TABLES FROM A REFLECTION TABLE FILE
		// Open the reflection table file
		fits_open_file(&fitsReflectFile, reflectFileName.c_str(),
					   READONLY, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		// 2008-03-02 Y.ISHISAKI	version 6.5.0
		// Find out scattering parameters

		// Move to 1st extension
		fits_movabs_hdu(fitsReflectFile, 2, &fitsHduType, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		if ( defaultScatterInfo.defaultScatterModeSet ) {
			scatmode = defaultScatterInfo.defaultScatterMode;
		} else {
			fits_read_key_lng(fitsReflectFile, "SCATMODE", &scatmode,
							  fitsComment, &fitsStatus);
			if ( fitsStatus ) {
				fitsStatus = 0;		// ignore error & load default values
				scatmode = DEFAULT_SCATTER_MODE;
			}
		}

		if ( 0 == scatmode ) {			// NONE
			;
		} else if ( 1 == scatmode ) {	// ASCA scatter

			fits_read_key_lng(fitsReflectFile, "CONSCT", &consct,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "SIGMA1", &sigma1,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "SIGMA2", &sigma2,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "LALIN", &lalin,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "CALSS3", &calss3,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "CALSC3", &calsc3,
							  fitsComment, &fitsStatus);

			if ( fitsStatus ) {
				fitsStatus = 0;		// ignore error & load default values
				consct = DEFAULT_ASCA_CONSCT;
				sigma1 = DEFAULT_ASCA_SIGMA1;
				sigma2 = DEFAULT_ASCA_SIGMA2;
				lalin  = DEFAULT_ASCA_LALIN;
				calss3 = DEFAULT_ASCA_CALSS3;
				calsc3 = DEFAULT_ASCA_CALSC3;
				printf("\
scatmode=%ld, consct=%ld\n\
sigma1=%.3f, sigma2=%.3f, lalin=%.3f, calss3=%.3f, calsc3=%.3f\n",
					scatmode, consct, sigma1, sigma2, lalin, calss3, calsc3);
			} else {
				show_header_card(fitsReflectFile, "SCATMODE");
				show_header_card(fitsReflectFile, "CONSCT");
				show_header_card(fitsReflectFile, "SIGMA1");
				show_header_card(fitsReflectFile, "SIGMA2");
				show_header_card(fitsReflectFile, "LALIN");
				show_header_card(fitsReflectFile, "CALSS3");
				show_header_card(fitsReflectFile, "CALSC3");
			}

		} else if ( 2 == scatmode ) {	// SUZAKU scatter

			fits_read_key_lng(fitsReflectFile, "NVFSW", &nvfsw,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "GAUSIGMA", &gausigma,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "NGAU", &ngau,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "EXPSIGMA", &expsigma,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "NEXP", &nexp,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "LORGAMMA", &lorgamma,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "NLOR", &nlor,
							  fitsComment, &fitsStatus);
			fits_read_key_lng(fitsReflectFile, "SCATSW", &scatsw,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "W_NORM", &w_norm,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "W_POWER", &w_power,
							  fitsComment, &fitsStatus);
			fits_read_key_dbl(fitsReflectFile, "G_NORM", &g_norm,
							  fitsComment, &fitsStatus);

			if ( fitsStatus ) {
				fitsStatus = 0;		// ignore error & load default values
				nvfsw    = DEFAULT_SUZAKU_NVFSW;
				gausigma = DEFAULT_SUZAKU_GAUSIGMA;
				ngau     = DEFAULT_SUZAKU_NGAU;
				expsigma = DEFAULT_SUZAKU_EXPSIGMA;
				nexp     = DEFAULT_SUZAKU_NEXP;
				lorgamma = DEFAULT_SUZAKU_LORGAMMA;
				nlor     = DEFAULT_SUZAKU_NLOR;
				scatsw   = DEFAULT_SUZAKU_SCATSW;
				w_norm   = DEFAULT_SUZAKU_W_NORM;
				w_power  = DEFAULT_SUZAKU_W_POWER;
				g_norm   = DEFAULT_SUZAKU_G_NORM;
				printf("\
scatmode=%ld, nvfsw=%ld, gausigma=%.4f, ngau=%.4f\n\
expsigma=%.4f, nexp=%.4f, lorgamma=%.4f, nlor=%.4f\n\
scatsw=%ld, w_norm=%.3e, w_power=%.2e, g_norm=%.2e\n",
					   scatmode, nvfsw, gausigma, ngau, expsigma, nexp,
					   lorgamma, nlor, scatsw, w_norm, w_power, g_norm);
			} else {
				show_header_card(fitsReflectFile, "SCATMODE");
				show_header_card(fitsReflectFile, "NVFSW");
				show_header_card(fitsReflectFile, "GAUSIGMA");
				show_header_card(fitsReflectFile, "NGAU");
				show_header_card(fitsReflectFile, "EXPSIGMA");
				show_header_card(fitsReflectFile, "NEXP");
				show_header_card(fitsReflectFile, "LORGAMMA");
				show_header_card(fitsReflectFile, "NLOR");
				show_header_card(fitsReflectFile, "SCATSW");
				show_header_card(fitsReflectFile, "W_NORM");
				show_header_card(fitsReflectFile, "W_POWER");
				show_header_card(fitsReflectFile, "G_NORM");
			}

		}

		// For each reflection table we will need, search for the table
		// and load it
		// Since we don't know the order of the tables on the FITS file, the
		// fastest way is to count the number of tables we need, check each
		// table in order to see if we need it, and stop when all have been
		// found.

		Count tables = frontReflectTables.size() + backReflectTables.size();

		// Find Reflection Table extensions in the Reflection file
		// Scan all possible extentions looking for the required extension
		// 10000 is an arbitary large number
		// 2 is the 1st extension of a FITS file
		for (int i=2; i<10000; i++) {
			// Are we still looking for any more tables?
			if (tables <= 0) {
				break;
			}

			// Move to each extension in turn
			fits_movabs_hdu(fitsReflectFile, i, &fitsHduType, &fitsStatus);
			if (fitsStatus == END_OF_FILE) {
				throw missingReflectionTables;
			}
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			// Read the name of the extension
			fits_read_key_str(fitsReflectFile, FITS_EXTENSION_NAME,
							  fitsChar, fitsComment, &fitsStatus);
			string extensionName = upcase_string(fitsChar);
			// Do we need this table for the front reflection?
			// gcc 2.7.2.2 is screwed here for passing a string to the find
			// so we send the char *
			found = find(frontReflectTables.begin(), frontReflectTables.end(),
						 fitsChar);
			if (found != frontReflectTables.end()) {
				// We need this table
				tables--;
				reflectTable = getFrontTableEntry( *found);
				// Find the FITS file matching columns
				// for the required column names

				loadReflect(fitsReflectFile, reflectTable);
			}

			// Do we need this table for the back reflection?
			// gcc 2.7.2.2 is screwed here for passing a string to the find
			// so we send the char *
			found = find(backReflectTables.begin(), backReflectTables.end(),
						 fitsChar);
			if (found != backReflectTables.end()) {
				// We need this table
				tables--;
				reflectTable = getBackTableEntry( *found);
				loadReflect(fitsReflectFile, reflectTable);
			}

		}

		// Close the reflection file
		fits_close_file(fitsReflectFile,  &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		// END LOAD OF REFLECTIONS TABLES FROM REFLECTION FILE
	}

	// 2005-12-24 Y.ISHISAKI	version 6.2.4
	if ( 0 == strcasecmp("NONE", pcolExtension.c_str()) ) {
		mountPreCollimator = false;
	} else {
		mountPreCollimator = true;
	}

	// Determine ... (added by H. Mori)
	if ( mountPreCollimator ) {
//		fflush(NULL);
//		printf("Reading '%s' ...\n", precollimatorFileName.c_str());
//		fflush(NULL);

		// Find the collimator extension in the Collimator description file
		// Scan all possible extentions looking for the Collimator extension
		// 10000 is an arbitary large number
		// 2 is the 1st extension of a FITS file (added by H. Mori)
		for (int i=2; i<10000; i++) {
			fits_movabs_hdu(fitsMirrorFile, i, &fitsHduType, &fitsStatus);
			if (fitsStatus == END_OF_FILE) {
				throw noPreCollimatorExtension;
			}
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			fits_read_key_str(fitsMirrorFile, FITS_EXTENSION_NAME,
							  fitsChar, fitsComment, &fitsStatus);
			if ( 0 == strcasecmp(fitsChar, pcolExtension.c_str()) ) {
				// Collimator extension found
				break;
			}
		}
		//
		// We now have the collimator extension as the file position
		//
		// Find the FITS file matching columns for the required column names
		// (added by H. Mori)
		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_LAYER,
						&fitsColPcolLayer, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColLayerCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_ASSEMBLY,
						&fitsColPcolAssembly, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColAssemblyCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_NUMBER,
						&fitsColPcolNumber, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColNumberCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_FRAGMENT,
						&fitsColPcolFragment, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColFragmentCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_FUNCTION,
						&fitsColPcolFunction, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColFunctionCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_SCATTER,
						&fitsColPcolScatter, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColScatterCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_REFLECT,
						&fitsColPcolReflect, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColFrelectCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_FSTART,
						&fitsColPcolFstart, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColFstartCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_SCROSS,
						&fitsColPcolScross, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColScrossCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_FEND,
						&fitsColPcolFend, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColFendCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_ECROSS,
						&fitsColPcolEcross, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColEcrossCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_TOPINR,
						&fitsColPcolTopinr, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColTopinrCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_TOPOUTR,
						&fitsColPcolTopoutr, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColTopoutrCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_BOTINR,
						&fitsColPcolBotinr, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColBotinrCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_BOTOUTR,
						&fitsColPcolBotoutr, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColBotoutrCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_TOPD,
						&fitsColPcolTopd, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColTopdCol;
		}

		fits_get_colnum(fitsMirrorFile, FALSE, FITS_COLNAME_PCOL_BOTD,
						&fitsColPcolBotd, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw noFitsColBotdCol;
		}

		// 2008-03-02 Y.ISHISAKI	version 6.5.0
		// Find out mirror thickness, missalignment

		fits_read_key_dbl(fitsMirrorFile, "THICKNES", &pcolThickness,
						  fitsComment, &fitsStatus);
		fits_read_key_str(fitsMirrorFile, "MALIGN_M", pcolMissAlignmentMode,
						  fitsComment, &fitsStatus);
		fits_read_key_dbl(fitsMirrorFile, "MALIGN_N", &pcolMissAlignment,
						  fitsComment, &fitsStatus);
		if ( fitsStatus ) {
			fitsStatus = 0;		// ignore error & load default values
			pcolThickness = DEFAULT_SUZAKU_PCOL_THICKNES;
			strcpy(pcolMissAlignmentMode,DEFAULT_SUZAKU_PCOL_MALIGN_M);
			pcolMissAlignment = DEFAULT_SUZAKU_PCOL_MALIGN_N;
			printf("\
pcolthickness=%.4f, pcolmissalignmode='%s', pcolmissalignment=%.4f\n",
			pcolThickness, pcolMissAlignmentMode, pcolMissAlignment);
		} else {
			show_header_card(fitsMirrorFile, "THICKNES");
			show_header_card(fitsMirrorFile, "MALIGN_M");
			show_header_card(fitsMirrorFile, "MALIGN_N");
		}

		//
		// Find out how many Collimator fragments we will have
		// (added by H. Mori)
		fits_read_key_lng(fitsMirrorFile, "NAXIS2", &fitsRowLimit,
						  fitsComment, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		//
		// Begin loading the Collimator fragments into the Collimator class
		//
		// For each collimator fragment (added by H. Mori)
		for (fitsRow=1; fitsRow <= fitsRowLimit; fitsRow++) {
			//
			// Obtain a collimator object
			// (added by H. Mori)
			XrrtCollimator* collimator = structure.createCollimator();
			//
			// Setup Pre-Collimator blade thickness and miss alignment
			// (modified by H. Mori : date 2002/08/16)
			collimator->setCollimatorThickness(pcolThickness);
			collimator->setCollimatorMissAlignmentMode(pcolMissAlignmentMode);
			collimator->setCollimatorMissAlignment(pcolMissAlignment);
			//
			// Read the next fragment
			// (added by H. Mori)
			FITS_READ_COL_PCOL_LAYER(fitsMirrorFile, fitsColPcolLayer,
									 fitsRow, 1L, 1L, 0, &fitsPcolLayer,
									 &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setFitsColLayer(fitsPcolLayer);

			FITS_READ_COL_PCOL_ASSEMBLY(fitsMirrorFile, fitsColPcolAssembly,
										fitsRow, 1L, 1L, 0, &fitsPcolAssembly,
										&fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setFitsColAssembly(fitsPcolAssembly);

			FITS_READ_COL_PCOL_NUMBER(fitsMirrorFile, fitsColPcolNumber,
									  fitsRow, 1L, 1L, 0, &fitsPcolNumber,
									  &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setFitsColNumber(fitsPcolNumber);

			FITS_READ_COL_PCOL_FRAGMENT(fitsMirrorFile, fitsColPcolFragment,
										fitsRow, 1L, 1L, 0, &fitsPcolFragment,
										&fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setFitsColFragmentNumber(fitsPcolFragment);

			FITS_READ_COL_PCOL_FUNCTION(fitsMirrorFile, fitsColPcolFunction,
										fitsRow, 1L, 1L, 0,
										&pcolSurfaceFunction,
										&fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setColSurfaceFunction(pcolSurfaceFunction);

			FITS_READ_COL_PCOL_SCATTER(fitsMirrorFile, fitsColPcolScatter,
									   fitsRow, 1L, 1L, 0, &pcolScatterMode,
									   &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setColScatterMode(pcolScatterMode);
			//
			// keep track of the various scatter modes required.
			//
			// comment out for setting scatter mode because collimator
			// is equipped only by ASTRO-E2
			// (modified by H. Mori)
			//   if (scatterMode == 0)
			//      {
			//      scatterNoneFound = true;
			//      }
			//   if (scatterMode == 1)
			//      {
			//      scatterAscaFound = true;
			//      }
			//   if (scatterMode == 2)
			//      {
			//      scatterAstroeFound = true;
			//      }

			// This section is reflect table code for Pre-Collimator
			// (modified by H. Mori : date 2003/01/14)
			char* charColArray[2];
			charColArray[0] = pcolReflectTable;
			charColArray[1] = NULL;
			strcpy(pcolReflectTable, "    ");
			FITS_READ_COL_PCOL_REFLECT(fitsMirrorFile, fitsColPcolReflect,
									   fitsRow, 1, 1, "", charColArray,
									   &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			// This code is needed to modify for collimator,
			// but I have not read subroutine for XrrtRemap class yet.
			// So I shall return to change for collimator
			// (modified by H. Mori)
			// Convert the char to a string and then see if it has been
			// remapped to a different name.
			//	 ofstream fout2("test2");
			// 2008-03-03 Y.ISHISAKI	version 6.5.0
			// Upcase string in the column
			reflectTableString = upcase_string(pcolReflectTable);
			//      fout2 << reflectTableString;
			reflectTableString = pcolRemap.getRemapName(reflectTableString);
			//      fout2 << reflectTableString << "\n";
			//
			// If we are processing a reflection table file,
			// we need a table index. Get a table index for the table name.
			// (modified by H. Mori : date 2003/01/14)
			reflectTable = getPcolTableEntry(reflectTableString);
			collimator->setPreCollimatorReflectTable(reflectTable);

			FITS_READ_COL_PCOL_FSTART(fitsMirrorFile, fitsColPcolFstart,
									  fitsRow, 1L, 1L, 0, &pcolStartAngle,
									  &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setColStartAngle(pcolStartAngle);

			FITS_READ_COL_PCOL_SCROSS(fitsMirrorFile, fitsColPcolScross,
									  fitsRow, 1L, 1L, 0, pcolSCross,
									  &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}

			if (pcolSCross[0] == 0x01) {
				collimator->setColStartAngleCross(true);
			} else if (pcolSCross[0] == 0x00) {
				collimator->setColStartAngleCross(false);
			} else {
				throw invalidCollimatorScross;
			}

			FITS_READ_COL_PCOL_FEND(fitsMirrorFile, fitsColPcolFend,
									fitsRow, 1L, 1L, 0, &pcolEndAngle,
									&fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setColEndAngle(pcolEndAngle);

			FITS_READ_COL_PCOL_ECROSS(fitsMirrorFile, fitsColPcolEcross,
									  fitsRow, 1L, 1L, 0, pcolECross,
									  &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}

			if (pcolECross[0] == 0x01) {
				collimator->setColEndAngleCross(true);
			} else if (pcolECross[0] == 0x00) {
				collimator->setColEndAngleCross(false);
			} else {
				throw invalidCollimatorEcross;
			}

			FITS_READ_COL_PCOL_TOPINR(fitsMirrorFile, fitsColPcolTopinr,
									  fitsRow, 1L, 1L, 0, &pcolTopInnerRadius,
									  &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setColTopInnerRadius(pcolTopInnerRadius);

			FITS_READ_COL_PCOL_TOPOUTR(fitsMirrorFile, fitsColPcolTopoutr,
									   fitsRow, 1L, 1L, 0, &pcolTopOuterRadius,
									   &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setColTopOuterRadius(pcolTopOuterRadius);

			FITS_READ_COL_PCOL_BOTINR(fitsMirrorFile, fitsColPcolBotinr,
									  fitsRow, 1L, 1L, 0,
									  &pcolBottomInnerRadius,
									  &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setColBottomInnerRadius(pcolBottomInnerRadius);

			FITS_READ_COL_PCOL_BOTOUTR(fitsMirrorFile, fitsColPcolBotoutr,
									   fitsRow, 1L, 1L, 0,
									   &pcolBottomOuterRadius,
									   &fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setColBottomOuterRadius(pcolBottomOuterRadius);

			FITS_READ_COL_PCOL_TOPD(fitsMirrorFile, fitsColPcolTopd,
									fitsRow, 1L, 1L, 0, &pcolTopDistance,
									&fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setColTopDistance(pcolTopDistance);

			FITS_READ_COL_PCOL_BOTD(fitsMirrorFile, fitsColPcolBotd,
									fitsRow, 1L, 1L, 0, &pcolBottomDistance,
									&fitsAnyNull, &fitsStatus);
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			collimator->setColBottomDistance(pcolBottomDistance);

			if (pcolSurfaceFunction == PLANE_COLLIMATOR) {
				//
				// Compute all the derived values based on the fixed values just
				// provided
				// (added by H. Mori)
				collimator->computePlaneCollimatorData();
			}
		}

		// BEGIN LOADING PRE-COLLIMATOR BLADE REFLECTION TABLE PART

//		fflush(NULL);
//		printf("Reading '%s' ...\n", charPreCollimatorReflectFileName);
//		fflush(NULL);

		// READ REFLECTION TABLES FROM A PRE-COLLIMATOR REFLECTION TABLE FILE
		// Open the Pre-Collimator reflection table file
		fits_open_file(&fitsReflectFile, reflectFileName.c_str(),
					   READONLY, &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}

		// For each reflection table we will need, search for the table
		// and load it
		// Since we don't know the order of the tables on the FITS file, the
		// fastest way is to count the number of tables we need, check each
		// table in order to see if we need it, and stop when all have been
		// found.

		Count tables = pcolReflectTables.size();
		// Find Reflection Table extensions in the Reflection file
		// Scan all possible extentions looking for the required extension
		// 10000 is an arbitary large number
		// 2 is the 1st extension of a FITS file
		for (int i=2; i<10000; i++) {
			// Are we still looking for any more tables?
			if (tables <= 0) {
				break;
			}

			// Move to each extension in turn
			fits_movabs_hdu(fitsReflectFile, i, &fitsHduType, &fitsStatus);
			if (fitsStatus == END_OF_FILE) {
				throw missingPreCollimatorReflectionTables;
			}
			if (fitsStatus != NO_ERROR) {
				throw fitsStatus;
			}
			// Read the name of the extension
			fits_read_key_str(fitsReflectFile, FITS_EXTENSION_NAME,
							  fitsChar, fitsComment, &fitsStatus);
			string extensionName = upcase_string(fitsChar);
			// Do we need this table for the pre-collimator reflection?
			// gcc 2.7.2.2 is screwed here for passing a string to the find
			// so we send the char *
			found = find(pcolReflectTables.begin(), pcolReflectTables.end(),
						 fitsChar);
			if (found != pcolReflectTables.end()) {
				// We need this table
				tables--;
				reflectTable = getPcolTableEntry( *found);
				loadReflect(fitsReflectFile, reflectTable);

				// 2008-03-02 Y.ISHISAKI	version 6.5.0
				// Find out scattering parameters for PCOL

				if ( 2 == scatmode ) {		// SUZAKU scatter

					fits_read_key_dbl(fitsReflectFile, "GC_A", &gc_a,
									  fitsComment, &fitsStatus);
					fits_read_key_dbl(fitsReflectFile, "GC_B", &gc_b,
									  fitsComment, &fitsStatus);
					fits_read_key_dbl(fitsReflectFile, "GW_A", &gw_a,
									  fitsComment, &fitsStatus);
					fits_read_key_dbl(fitsReflectFile, "GW_B", &gw_b,
									  fitsComment, &fitsStatus);

					if ( fitsStatus ) {
						fitsStatus = 0;	// ignore error & load default values
						gc_a = DEFAULT_SUZAKU_PCOL_GC_A;
						gc_b = DEFAULT_SUZAKU_PCOL_GC_B;
						gw_a = DEFAULT_SUZAKU_PCOL_GW_A;
						gw_b = DEFAULT_SUZAKU_PCOL_GW_B;
						printf("\
gc_a=%.7f, gc_b=%.7f, gw_a=%.7f, gw_b=%.7f\n", gc_a, gc_b, gw_a, gw_b);
					} else {
						show_header_card(fitsReflectFile, "GC_A");
						show_header_card(fitsReflectFile, "GC_B");
						show_header_card(fitsReflectFile, "GW_A");
						show_header_card(fitsReflectFile, "GW_B");
					}
				}

			}
		}

		// Close the Pre-Collimator reflection file
		fits_close_file(fitsReflectFile,  &fitsStatus);
		if (fitsStatus != NO_ERROR) {
			throw fitsStatus;
		}
		// END LOAD OF REFLECTIONS TABLES FROM PRE-COLLIMATOR REFLECTION FILE
		// END LOAD OF PRE-COLLIMATOR PARAMETER
	}

	// Close the mirror file
	fits_close_file(fitsMirrorFile,  &fitsStatus);
	if (fitsStatus != NO_ERROR) {
		throw fitsStatus;
	}

	// 2008-03-02 Y.ISHISAKI	version 6.5.0
	// Set scatter mode parameters

	// setOverrideScatterMode(scatmode);

	defaultScatterInfo.setAscaParams(consct, sigma1, sigma2,
									 lalin, calss3, calsc3);

	defaultScatterInfo.setAstroeParams(nvfsw, gausigma, ngau,
									   expsigma, nexp, lorgamma, nlor,
									   scatsw, w_norm, w_power, g_norm,
									   gc_a, gc_b, gw_a, gw_b);

	// 2008-03-02 Y.ISHISAKI	version 6.5.0
	// Read the backprof file
	defaultScatterInfo.setAstroeBackParams(backprofFileName);

	//  printf("Done.\n");

	// Build the telescope structure from the mirrors,
	// obstructions and collimators
	// (modified by H. Mori)
	structure.organizeStructure();

	// Leave a marker that we have built the telescope description
	telescopeDefined = true;
}

void
XrrtRaytrace::initPhoton(const double& energy,
	                     const double& radius,
	                     const double& rotationAngle,
	                     const double& zDistance,
	                     const double& photonUnitRadial,
	                     const double& photonPhiAngle,
	                     const double& photonUnitZ)
{
	//
	// Set the photon to a default top of telescope status
	//
	defaultPhoton.clearPhoton();
	//
	// Set the photon to the requested status
	//
	defaultPhoton.setEnergy(energy);
	defaultPhoton.setRadius(radius);
	defaultPhoton.setRotationAngle(rotationAngle);
	defaultPhoton.setPhotonDistance(zDistance);
	defaultPhoton.setPhotonDirection(photonUnitRadial,
									 photonPhiAngle,
									 photonUnitZ);
	//

	// zero out reflections counts by layer so we can determine the
	// reflection class of the photon after it is traced thru the telescope
	//
	defaultPhoton.zeroReflectionsByLayer(structure.getNumberOfLayers());
	//
	// Save the top of telescope state of the photon so it can be accessed
	// later
	defaultPhoton.setInitialToCurrent();
	//
	// Note that the photon is ready to be traced
	//
	photonDefined = true;
}

void
XrrtRaytrace::setOverrideScatterMode( const SurfaceScatterMode& mode)
{
	defaultScatterInfo.setDefaultScatterMode(mode);
}

bool
XrrtRaytrace::getAscaParamStatus()
{
	return defaultScatterInfo.getAscaParamStatus();
}

bool
XrrtRaytrace::getAstroeParamStatus()
{
     return defaultScatterInfo.getAstroeParamStatus();
}

bool
XrrtRaytrace::scatterNoneExists() const
{
	return scatterNoneFound;
}

bool
XrrtRaytrace::scatterAscaExists() const
{
	return scatterAscaFound;
}

bool
XrrtRaytrace::scatterAstroeExists() const
{
	return scatterAstroeFound;
}

void
XrrtRaytrace::setFocalPlanePosition(const double& focalPlaneZ)
{
	assumedFocalPlaneZ = focalPlaneZ;
}

double
XrrtRaytrace::getMinimumTelescopePhotonRadiusMM() const
{
	return minimumTelescopePhotonRadiusMM;
}

double
XrrtRaytrace::getMaximumTelescopePhotonRadiusMM() const
{
	return maximumTelescopePhotonRadiusMM;
}

void
XrrtRaytrace::setAtomicDataFileNames(string& atomicScatterFileName,
	                                 string& atomicInfoFileName)
{
	overrideCALDBAtomicFiles = true;
	atomicScatterFactorsFileName = atomicScatterFileName;
	atomicDataFileName = atomicInfoFileName;
}

//
// Add the accessors to access the telescope name of the Astro-E2 XRTs
// (added by Hideyuki MORI : date 2006/01/31)
//
void
XrrtRaytrace::setTelescopeName(string &telescopeName)
{
	astroeXrtTelescopeName = telescopeName;
}

string
XrrtRaytrace::getTelescopeName() const
{
	return astroeXrtTelescopeName;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C++ ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
