/// \file xrtraytrace_lib.cxx
/// \brief Calculate ray tracing
/// \author Kristin Rutkowski, Tahir Yaqoob
/// \date $Date: 2017/01/18 15:43:50 $


#define AHLABEL tool_raytrace_xrtraytracelib
#define AHCVSID "$Id: "
#define TOOLNAME "raytrace"

#include "xrtraytrace_lib.h"

// Parameter file access.
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "ape/ape_util.h"   // for writeParametersToLog()

// heautils utilities
#include "headas_utils.h"
#include "hdcal.h"          // HDgtcalf

#include <strings.h>        // strcasecmp, strncasecmp

// +++ remove when headas_startup is ready
//#include <cstring>            // std::strlen
//#include <cstdlib>            // std::strtol, getenv
//#include <iostream>           // stat
//#include "sys/time.h"         // seeding random numbers



/**********************************************
 * ********************************************
 * 		initialize(): supporting functions
 * ********************************************
**********************************************/


// FUNCTION NAME: xrtSetup
//
// CALLING SEQUENCE:
//   xrtSetup(param, scat, misalignment_struct, genTelescope, housings, 
//            XRTObjects, numXRTObjects, firstMirrorIndex, lastMirrorIndex, 
//            zminsortxrtobjectindex, zmaxsortxrtobjectindex, 
//            zmaxsortxrtobjectindexAll, frontColumnNames, xrtzlevels, 
//            numxrtzlevels, obstructionIntervalLowIndex, 
//            obstructionIntervalHighIndex, xrtRegionToObjectID, has3DObjects, 
//            numGroups, transforms, topExtObjects, bottomExtObjects, 
//            sectorsShells); 
//
// PURPOSE:
//   Set up the data structures relating the telescope
//   (1)  gather information from the TDF MIRROR extension
//   (2)  gather information from the TDF COLLIMATOR extension
//   (3)  sum all the housing shifts found
//   (4)  gather information from the TDF OBSTRUCT extension
//   (5)  gather information from the TDF SEGMENT extension
//   (6)  Calculate sector IDs of front and back of mirror and pre-collimator foils.
//   (7)  Calculate slopes of front and back of mirror and pre-collimator foils.
//   (8)  Find scattering column for each object
//   (9)  Organize the telescope objects (obstructions, mirrors, then pre-collimators)
//        and set up the associated pointers, coordinate shifts, and rotations.
//  (10)  Sort the XRT bounding boxes by their shortest distance from the results 
//        plane (focal plane by default)
//  (11)  Divide the telescope into several levels in the z-direction, which group 
//        XRT objects that occupy similar z-coordinate intervals
//  (12)  Create a look-up table that returns an XRT object ID given a shell 
//        number and a sector ID: sectorshell2objectid
//  (13)  Set up a lower and upper radial bound for each XRT object of type foil 
//        corresponding to the adjacent shell smaller and larger in radius 
//        respectively than the shell for the XRT object in question.
//  (14)  Set up arrays that will hold coordinate transformations for each XRT object
//
// INPUTS:
//   param, scat
//
// OUTPUTS:
//   XRTObjects, numXRTObjects, zminsortxrtobjectindex, 
//   zmaxsortxrtobjectindex, zmaxsortxrtobjectindexAll, 
//   frontColumnNames, xrtzlevels, numxrtzlevels, xrtRegionToObjectID, 
//   has3DObjects, telescop, instrume, numGroups, transforms
//
// CALLED BY:
//   initialize()
//
// SUBROUTINES:
//   setupTDFMirror(), setupTDFPrecol(), setupTDFObstruct(), setupTDFSegment(), 
//   makeObstructBBox(), makeFoilBBox(), setupXRTTransform(), transformBBoxes()
//
void xrtSetup(Param & param, 
              const Scattering & scat, 
              Misalignment & misalignment_struct,
              GenTelescope & genTelescope, 
              std::vector<HousingGeometry> & housings,
              std::vector<XRTObject> & XRTObjects, 
              long & numXRTObjects, 
              long & firstMirrorIndex, 
              long & lastMirrorIndex,
              std::vector<long> & zminsortxrtobjectindex,
              std::vector<long> & zmaxsortxrtobjectindex,
              std::vector<long> & zmaxsortxrtobjectindexAll,
              std::vector<std::string> & uniqueFrontNames,
              std::vector<double> & xrtzlevels, 
              long & numxrtzlevels,
              std::vector<int> & obstructionIntervalLowIndex, 
              std::vector<int> & obstructionIntervalHighIndex, 
              vector4Dint & xrtRegionToObjectID,
              bool & has3DObjects, 
              long & numGroups, 
              Transforms & transforms, 
              ExternalObjectsStructure & topExtObjects,
              ExternalObjectsStructure & bottomExtObjects, 
              SectorsShells & sectorsShells) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  const int numDim = 3;       // three dimensions: x,y,z
  const int maxNumSets = 3;   // number of sets: pcol, primary, secondary
  
  // structs to hold data from TDF extensions
  TDF_Foil mirror;         // struct to hold data from TDF MIRROR extension
  TDF_Foil pcol;           // struct to hold data from TDF COLLIMATOR extension
  TDF_Obstruct obstruct;   // struct to hold data from TDF OBSTRUCT extension
  TDF_Segment seg;         // struct to hold data from TDF SEGMENT extension
  TDF_External tdfExtObjects;// struct to hold data from TDF AZIMUTHALSTRUCT extension
  
  //std::vector<std::string> uniqueFrontNames;
  int numUniqueFrontNames = 0;
  std::vector<int> frontReflindex(mirror.m_numRows);        // +++ why size to numRows.  mirror is empty right now
  std::vector<std::string> uniqueRoughNames;
  int numUniqueRoughNames = 0;
  std::vector<int> backMirrorReflIndex(mirror.m_numRows);   // +++ why size to numRows.  mirror is empty right now
  std::vector<int> pcolFrontReflIndex;
  std::vector<int> pcolBackReflIndex;
  
  // vector of all the obstructions in the telescope
  std::vector<Obstruction> obstructions;

  // Do the primary and secondary mirror sector ids and calculate the front and back foil slopes. 
  std::vector<int> mirrorSectorID;
  vector2Ddbl mirrorSectorStartAngle;
  vector2Ddbl mirrorSectorEndAngle;
  vector1Ddbl frontMirrorSlope;
  vector1Ddbl frontMirrorSlopeSq; // square of front-mirror slopes
  vector1Ddbl backMirrorSlope;
  vector1Ddbl backMirrorSlopeSq;  // square of back-mirror slopes
  double mirrorHeight = 0.0;      // temp var, just to calc slopes
  double rDeltaFront = 0.0;       // temp var, just to calc slopes
  double rDeltaBack = 0.0;        // temp var, just to calc slopes
  
  // An arbitrarily large value for the mirror slopes in the case that a 
  // mirror has a cylindrical geometry instead of conical: this will indicate 
  // to downstream routines not to use the slope and to use cylindrical 
  // geometry. 
  double cylSlope = -1.0e30;
  
  int numMirrorSectors = 0;
  int numPrimarySectors = 0;
  int numSecondarySectors = 0;
  int numSecondaryRows = 0;
  
  bool rowLogged = false;
  // Do the pre-collimator sector ids and calculate the front and back pre-collimator foil slopes. 
  // The previously obtained number of rows in the PCOL extension of the TDF is pcol.m_numRows.
  std::vector<int> pcolsectorid;
  std::vector<double> pcolsectorstartangle;
  std::vector<double> pcolsectorendangle;
  std::vector<double> frontpcolslope;
  std::vector<double> frontpcolslopesq;
  std::vector<double> backpcolslope;
  std::vector<double> backpcolslopesq;
  double pcolFoilHeight = 0.0;        // temp var, just to calc slopes
  double pcolrDeltaFront = 0.0;       // temp var, just to calc slopes
  double pcolrDeltaBack = 0.0;        // temp var, just to calc slopes
    
  long objectIdx = 0;                 // object counter, will count ALL XRT objects
  
  double dummy = -9999.0;             // used for obstruction sidewalls
  ObjectBoundingBox obstructBBox;     // Bounding box for this obstruction
  
  // inputs for makeFoilBBox
  //  calculate the bounding box for this mirror object
  std::vector<double> foilbboxcoords(6);
  ObjectBoundingBox foilBBox;
  
  // Output bounding box parameters
  int numVertices = 8;
  std::vector< std::vector<double> > foilvertices(numDim);
  for (unsigned int i = 0 ; i < foilvertices.size() ; ++i) {
    foilvertices[i].resize(numVertices);
  }
  // Output vertices of the foil (1st index=x,y,z, 2nd index=vertex label)
  std::vector<int> foilcoordquadrant(3);
  
  // maps used to create the zminsortxrtobjectindex arrays
  std::map<double, long> zminsortxrtobjectindexMap;
  std::map<double, long> zmaxsortxrtobjectindexMap;
  // we need a map with ALL zlevels, including duplicates
  std::multimap<double, long> zminsortxrtobjectindexMultiMap;
  std::multimap<double, long> zmaxsortxrtobjectindexMultiMap;
  
  // for getting zlevels
  long zLevelIdx = 0;
  double zLevelDelta = 0.0;
  double prevZLevel = 0.0;
  double nextZLevel = 0.0;
  
  // to check if any twists, etc, to determine if we need to do transforms
  double sumHousingShifts = 0.0;
  double totalTransformValue = 0.0;
  
  // A distance which defines the z-coordinate of the plane parallel to the 
  // x-y plane that contains the axes about which the segment misalignment 
  // rotation angles (xrtsegmentrotx and xrtsegmentroty) are defined. [mm]
  // Not controllable by the user and assigned to its default value of the 
  // focallength of the telescope
  double segmentRotationZOffset = 0.0;
  
  // used to assign m_scatterIndexFront and Back to an XRTObject, 
  // after reading the scatter column from the mirror file
  int numScatColumns = 0;
  std::string frontmirrorscattername;
  std::string backmirrorscattername;
  bool frontmatch = false;
  bool backmatch = false;
  bool pcolscattermatch = false;
  std::vector<int> frontmirrorscatterindex;
  std::vector<int> backmirrorscatterindex;
  //+++ resize to nummirrorrows.  then can use [iRow] = -1; etc instead of push_back.  or at least .reserve()
  std::vector<int> pcolscatterindex;
  
  // these are used for determining which external objects to use
  // top-most z-coordinate of the principal telescope components (i.e. tops of 
  // pre-collimator foils, or tops of primaries if there is no pre-collimator);
  double telescopetopz = 0.0;
  // bottom-most z-coordinate of the principal telescope components (i.e. 
  // bottoms of secondary foils)
  double telescopebottomz = 0.0;
  
  // -------------------------------------
  
  AH_OUT << "Setting up telescope structure" << std::endl;
  
  //+++ add comments about what exactly is being input and output
  // input to setupTDFMirror:
  // output to setupTDFMirror:
  setupTDFMirror(param, mirror, housings, misalignment_struct, frontReflindex, uniqueFrontNames, numUniqueFrontNames, uniqueRoughNames, numUniqueRoughNames, backMirrorReflIndex, numGroups, genTelescope);
  
  setupTDFPrecol(param, pcol, housings, misalignment_struct, pcolFrontReflIndex, pcolBackReflIndex, uniqueRoughNames, numUniqueRoughNames);
  
  AH_DEBUG << "pcol tilt pivot = " << misalignment_struct.m_tiltPivot[PCOL] << std::endl;
  AH_DEBUG << "prim tilt pivot = " << misalignment_struct.m_tiltPivot[PRIMARY] << std::endl;
  AH_DEBUG << "secon tilt pivot = " << misalignment_struct.m_tiltPivot[SECONDARY] << std::endl;
  
  // after finding housing geometry parameters in setupTDFMirror(), use those to set up some general telescope geometry parameters
  // inner and outer radius of the topmost of the three housing units 
  // (pre-collimator unit), appropriate for defining the telescope aperture
  // +++ what if there is no precollimator?
  genTelescope.m_innerhousingradius = housings[PCOL].m_rInner;
  genTelescope.m_outerhousingradius = housings[PCOL].m_rOuter;
  
  AH_DEBUG << "innerhousingradius = " << genTelescope.m_innerhousingradius << std::endl;
  AH_DEBUG << "outerhousingradius = " << genTelescope.m_outerhousingradius << std::endl;
  
  // Sum of all housinggeometry shifts for later use as part of the determination of whether to switch off all coordinate transformations or not
  // +++ what if there's no pcol?
  for (int iHousing = 0 ; iHousing < s_numHousings ; ++iHousing) {
    sumHousingShifts += (housings[iHousing].m_xShift + housings[iHousing].m_yShift);
  }
  
  AH_DEBUG << "uniqueRoughNames.size() = " << uniqueRoughNames.size() << std::endl;
  for (uint i = 0 ; i < uniqueRoughNames.size() ; i++ ) {
    AH_DEBUG << "uniqueRoughNames["<<i<<"] = " << uniqueRoughNames[i] << std::endl;
  }
  //+++ not sure if this should be repeating REFPROBPCOL
  
  // get the information about obstructions, in the OBSTRUCT extension of TDF
  setupTDFObstruct(param, obstruct, obstructions, has3DObjects);
  
  // get the information about segments, rotations, etc, in the SEGMENT 
  // extension of TDF
  setupTDFSegment(param, seg);
  
  // now that we know how many segments in this telescope, store it
  sectorsShells.m_numSegments = seg.m_maxSegment;
  sectorsShells.m_sectorsPerSegment = mirror.m_sectorsPerSegment;
  
  // set up the external objects if applicable, in the AZIMUTHALSTRUCT
  // extension of TDF
  if (param.m_doExternalObjects) {
    setupTDFExternalObjects(param, tdfExtObjects);
  }
  
  // resize vector4Dint xrtRegionToObjectID
  xrtRegionToObjectID.resize(maxNumSets);
  for (int i = 0 ; i < maxNumSets ; ++i) {
    xrtRegionToObjectID[i].resize(mirror.m_maxSegment);
    for (int j = 0 ; j < mirror.m_maxSegment ; ++j) {
       xrtRegionToObjectID[i][j].resize(mirror.m_sectorsPerSegment);
      for (int k = 0 ; k < mirror.m_sectorsPerSegment ; ++k) {
        xrtRegionToObjectID[i][j][k].resize(mirror.m_numberofshells);
      }
    }
  }
  
  // Calculate sector IDs and slopes of front and back of mirror and 
  // pre-collimator foils.
  
  // The slope refers to the slope of a line in a 2-dimensional cross-section 
  // of a cone-shaped foil that lies on the curved surface (it is the slope 
  // z/r where z is the perpendicular height above the circular base of the 
  // cone and r is a radial line on the base of the cone). The slopes will be 
  // needed later to evaluate expressions relating to the intercept of a ray 
  // with the foil surfaces, as well as to calculate normal vectors to the 
  // foil surfaces for the treatment of reflection.
  
  // First loop over mirror and pre-collimator foils to identify unique sector 
  // id label numbers. We can calculate the sector ID from the start and end 
  // angle columns/arrays. Sector IDs will run from 1 through to the primaries 
  // and then continue on to the secondaries. Going in, we don't know how many 
  // sectors there will be but there cannot be more than the number of rows in 
  // the mirror extension (mirror.m_numRows).
  // The routine recognizes primary and secondary mirror sets: 
  // 0 for the second index refers to primary mirror quantities
  // 1 refers to secondary mirror quantities
  
  mirrorSectorID.resize(mirror.m_numRows);
  mirrorSectorStartAngle.resize(mirror.m_numRows);  //+++ should this be so large?
  mirrorSectorEndAngle.resize(mirror.m_numRows);
  for (int i = 0 ; i < mirror.m_numRows ; ++i) {
    mirrorSectorStartAngle[i].resize(2);
    mirrorSectorEndAngle[i].resize(2);
  }
  frontMirrorSlope.resize(mirror.m_numRows);
  frontMirrorSlopeSq.resize(mirror.m_numRows);
  backMirrorSlope.resize(mirror.m_numRows);
  backMirrorSlopeSq.resize(mirror.m_numRows);

  
  // initialize variables that will be assigned to the extremal top and 
  // bottom z-coordinates of the primary and secondary mirror foils.
  double primariestopz = 0.0;
  double primariesbotz = 1.e30;
  double secondariestopz = 0.0;
  double secondariesbotz = 1.e30;
  
  int unqSectorCtr = 0;         // unique sector counter
  
  // for each row in the file see if the start and end angles for the row have 
  // already been logged in the mirrorSectorStartAngle & mirrorSectorEndAngle.
  // also establish extremal z-coordinates of tops and bottoms of mirror foils
  for (int iRow = 0 ; iRow < mirror.m_numRows ; ++iRow) {
    
    // +++ for efficiency, this could be combined with if-statements below
    // first get the z-coords of top and bottom of foils
    if ( (mirror.m_set.at(iRow) == 1) && 
         (mirror.m_topZ.at(iRow) > primariestopz)) {
      primariestopz = mirror.m_topZ.at(iRow);
    }
    if ( (mirror.m_set.at(iRow) == 1) && 
         (mirror.m_botZ.at(iRow) < primariesbotz) ) {
      primariesbotz = mirror.m_botZ.at(iRow);
    }
    if ( (mirror.m_set.at(iRow) == 2) && 
         (mirror.m_topZ.at(iRow) > secondariestopz) ) {
      secondariestopz = mirror.m_topZ.at(iRow);
    }
    if ( (mirror.m_set.at(iRow) == 2) && 
         (mirror.m_botZ.at(iRow) < secondariesbotz) ) {
      secondariesbotz = mirror.m_botZ.at(iRow);
    }
    
    // now get the sector angles
    rowLogged = false;
    if (mirror.m_set[iRow] == 1) { 
      // primary mirror sectors
      
      for (int iSect = 0 ; iSect < numPrimarySectors ; ++iSect) {
        if ( (mirror.m_startAngle[iRow] == mirrorSectorStartAngle[iSect][0]) && 
             (mirror.m_endAngle[iRow] == mirrorSectorEndAngle[iSect][0]) ) {
          rowLogged = true;
          mirrorSectorID[iRow] = iSect;
        }
      }

      if (!rowLogged) {
        mirrorSectorStartAngle[numPrimarySectors][0] = mirror.m_startAngle[iRow];
        mirrorSectorEndAngle[numPrimarySectors][0] = mirror.m_endAngle[iRow];
        numPrimarySectors++;
        mirrorSectorID[iRow] = unqSectorCtr;
        unqSectorCtr++;
      }
      
    } else if (mirror.m_set[iRow] == 2) {
      // Secondary sectors

      numSecondaryRows++;

      // Reset the sector counter when starting on the secondaries
      if (numSecondaryRows == 1) {
        //+++ I think this may be a logic error?
        unqSectorCtr = 0;
      }

      for (int iSect = 0 ; iSect < numSecondarySectors ; ++iSect) {
        if ( (mirror.m_startAngle[iRow] == mirrorSectorStartAngle[iSect][1]) && 
            (mirror.m_endAngle[iRow] == mirrorSectorEndAngle[iSect][1]) ) {
          rowLogged = true;
          mirrorSectorID[iRow] = iSect;
        }
      }
      if (!rowLogged) {
        mirrorSectorStartAngle[numSecondarySectors][1] = mirror.m_startAngle[iRow];
        mirrorSectorEndAngle[numSecondarySectors][1] = mirror.m_endAngle[iRow];
        numSecondarySectors++;
        mirrorSectorID[iRow] = unqSectorCtr;
        unqSectorCtr++;
      }
      
    } // end if-block selecting primary or secondary mirrors
    
    // calculate the slopes of the front and back of mirror foils
    mirrorHeight = mirror.m_topZ[iRow] - mirror.m_botZ[iRow];
    rDeltaFront = mirror.m_topInnerRadius[iRow] - mirror.m_botInnerRadius[iRow];
    if (rDeltaFront != 0.0) {
      frontMirrorSlope[iRow] = mirrorHeight / rDeltaFront;
      frontMirrorSlopeSq[iRow] = frontMirrorSlope[iRow] * frontMirrorSlope[iRow];
    } else {
      frontMirrorSlope[iRow] = cylSlope;
      frontMirrorSlopeSq[iRow] = cylSlope;
    }
    rDeltaBack = mirror.m_topOuterRadius[iRow] - mirror.m_botOuterRadius[iRow];
    if (rDeltaBack != 0.0) {
      backMirrorSlope[iRow] = mirrorHeight / rDeltaBack;
      backMirrorSlopeSq[iRow] = backMirrorSlope[iRow] * backMirrorSlope[iRow];
    } else {
      backMirrorSlope[iRow] = cylSlope;
      backMirrorSlopeSq[iRow] = cylSlope;
    }
    
    // Examine the string SCATTER column in the current row of the TDF, which should correspond to a column name in the correct SEGMENT extension of the scattering file for front-side mirror scattering. Find a match for the column name in the scattering file and index it by column number. For the back-side mirror scattering it is currently assumed that all mirror foils in a SEGMENT will have one and only one scattering profile, and therefore only one column in the scattering file. It is further assumed that the column for the back-side scattering has the string "BACK" (or "back") somewhere in the name and this is how it is recognized. In the future, if there is more complexity in the back-side scattering, we will have to add a new column in the TDF for the back-side scattering index
    // Find a match for the front and back scattering column in the scattering 
    // file and index the column for later use; if no match is found store the 
    // value -1 for the index
    if (!isEqualCaseInsens(param.m_scattermode, "none")) {
      
      frontmirrorscattername = mirror.m_scatter[iRow];
      backmirrorscattername = "BACK";
      frontmatch = false;
      backmatch  = false;
      numScatColumns = scat.m_scatcolnames[mirror.m_segment[iRow]-1].size();
      
      for (int k = 0 ; k < numScatColumns ; ++k) {
        if ( !frontmatch && isEqualCaseInsens(scat.m_scatcolnames[mirror.m_segment[iRow]-1][k], frontmirrorscattername) ) {
          frontmirrorscatterindex.push_back(k);
          frontmatch = true;
        }
        if ( !backmatch && foundCaseInsens(scat.m_scatcolnames[mirror.m_segment[iRow]-1][k], backmirrorscattername) ) {
          backmirrorscatterindex.push_back(k);
          backmatch= true;
        }
        // if we've found both the front and back scatter column for this foil 
        // in this segment, break out of the scatter column loop
        if (frontmatch && backmatch) {
          break;
        }
      }
      
      if (!frontmatch) {
        frontmirrorscatterindex.push_back(-1);
      }
      if (!backmatch) {
        backmirrorscatterindex.push_back(-1);
      }
      
    }
    
  } // end for-loop over mirror file rows
  
  numMirrorSectors = numPrimarySectors + numSecondarySectors;
  
  // output some screen messages:
  AH_DEBUG << "Number of unique sectors = " << numMirrorSectors << std::endl;
  AH_DEBUG << "Number of primary mirror sectors = " << numPrimarySectors << std::endl;
  AH_DEBUG << "Number of secondary mirror sectors = " << numSecondarySectors << std::endl;
  
  // loop over all rows again to make secondary sectors IDs run continuously 
  // from primary IDs by adding the number of primaries to the ID. 
  for (int i = 0 ; i < mirror.m_numRows ; ++i) {
    if (mirror.m_set[i] == 2) {
      mirrorSectorID[i] += numPrimarySectors;
    }
  }
  
  // The end result should be that for every row of the MIRROR extension, 
  // mirrorSectorID[i] holds the integer number 1 through (total # of mirror 
  // sectors). For indexing, id has to have 1 subtracted from it.
  
  if (param.m_pcolExists) {
    
    // resize the pcol arrays after reading the pcol file
    pcolsectorid.resize(pcol.m_numRows);
    pcolsectorstartangle.resize(pcol.m_numRows);
    pcolsectorendangle.resize(pcol.m_numRows);
    frontpcolslope.resize(pcol.m_numRows);
    frontpcolslopesq.resize(pcol.m_numRows);
    backpcolslope.resize(pcol.m_numRows);
    backpcolslopesq.resize(pcol.m_numRows);
    
    int numPcolSectors = 0;             // unique sector counter
    for (int iRow = 0 ; iRow < pcol.m_numRows ; ++iRow) {
      // for each row in the file see if the start and end angles for the row 
      // have already been logged in pcolsectorstartangle & pcolsectorendangle
      rowLogged = false;
      if (pcol.m_set[iRow] == 1) {
        if (numPcolSectors > 0) {
          for (int k = 0 ; k < numPcolSectors ; ++k) {
            if ( (pcol.m_startAngle[iRow] == pcolsectorstartangle[k]) && 
                 (pcol.m_endAngle[iRow] == (pcolsectorendangle[k])) ) {
              rowLogged = true;
              pcolsectorid[iRow] = k;
            }
          }
        }
        if (!rowLogged) {
          pcolsectorstartangle[numPcolSectors] = pcol.m_startAngle[iRow];
          pcolsectorendangle[numPcolSectors] = pcol.m_endAngle[iRow];
          pcolsectorid[iRow] = numPcolSectors;
          numPcolSectors++;
        }
      } else {
        AH_THROW_RUNTIME("ERROR: code cannot handle more than one precollimator layer");
      }
              
      // calculate the slopes of the front and back of pre-collimator foils
      pcolFoilHeight = pcol.m_topZ[iRow] - pcol.m_botZ[iRow];
      pcolrDeltaFront = pcol.m_topInnerRadius[iRow] - pcol.m_botInnerRadius[iRow];
      if (pcolrDeltaFront != 0.0) {
        frontpcolslope[iRow] = pcolFoilHeight / pcolrDeltaFront;
        frontpcolslopesq[iRow]=frontpcolslope[iRow] * frontpcolslope[iRow];
      } else {
        frontpcolslope[iRow] = cylSlope;
        frontpcolslopesq[iRow] = cylSlope;
      }
        
      pcolrDeltaBack = pcol.m_topOuterRadius[iRow] - pcol.m_botOuterRadius[iRow];
      if (pcolrDeltaBack != 0.0) {
        backpcolslope[iRow] = pcolFoilHeight / pcolrDeltaBack;
        backpcolslopesq[iRow]=backpcolslope[iRow] * backpcolslope[iRow];
      } else {
        backpcolslope[iRow] = cylSlope;
        backpcolslopesq[iRow] = cylSlope;
      }

      // Find a match for the pre-collimator scattering column in the 
      // scattering file and index the column for later use; if no match is 
      // found store the value -1 for the index
      if (!isEqualCaseInsens(param.m_scattermode, "none")) {
        pcolscattermatch = false;
        numScatColumns = scat.m_scatcolnames[pcol.m_segment[iRow]-1].size();
        for (int k = 0 ; k < numScatColumns ; ++k) {
          if ( isEqualCaseInsens(scat.m_scatcolnames[pcol.m_segment[iRow]-1][k], pcol.m_scatter[iRow]) ) {
            pcolscatterindex.push_back(k);
            pcolscattermatch = true;
            break;
          }
        }
        if (!pcolscattermatch) {
          pcolscatterindex.push_back(-1);
        }
      }
      
    } // end for-loop of pre-collimator extension rows
            
    AH_DEBUG << "Number of unique pre-collimator sectors = " << numPcolSectors << std::endl;
    
    // shift up the pcolsectorid so that it carries on sequentially from the 
    // last mirrorsectorid - this will enable correct indexing later on 
    //+++ is this correct? (trf just said pcolsectorid += numMirrorSectors
    for (int iRow = 0 ; iRow < pcol.m_numRows ; ++iRow) {
      pcolsectorid[iRow] += numMirrorSectors;
    }
    
    // The end result should be that for every row of the PCOL extension, 
    // pcolsectorid() holds the integer number 1+(number of mirror sectors) 
    // through (total num of sectors). For indexing, id has to have 1 
    // subtracted from it.
  
  } // end if-block that checks if pcolExists
  
  
  // -------------------------------------
  // Organize the telescope objects 
  // (obstructions, mirrors, and pre-collimators) and set up 
  // the associated pointers, coordinate shifts, and rotations.
  // The xrtobjects and their attributes and some pointers will be placed 
  // together in single arrays, starting with the obstructions (support 
  // structures), followed by the mirror fragments, followed by the 
  // pre-collimator fragments. 
  // -------------------------------------
   
  // total number of xrt objects
  numXRTObjects = obstruct.m_numObst + mirror.m_numRows + pcol.m_numRows;
  
  AH_DEBUG << "Number of XRT Objects: " << numXRTObjects << std::endl;
  AH_DEBUG << "Number of obstructions: " << obstruct.m_numObst << std::endl;
  AH_DEBUG << "Number of mirror objects: " << mirror.m_numRows << std::endl;
  AH_DEBUG << "Number of precollimator objects: " << pcol.m_numRows << std::endl;
  
  // set the indices for the start and end of mirror foils
  // initialize() needs these to set the taupermm indices
  firstMirrorIndex = obstruct.m_numObst;
  lastMirrorIndex = firstMirrorIndex + mirror.m_numRows - 1;
  
  AH_DEBUG << "firstMirrorIndex = " << firstMirrorIndex << std::endl;
  AH_DEBUG << "lastMirrorIndex = " << lastMirrorIndex << std::endl;
  
  
  AH_DEBUG << "primariestopz = " << primariestopz << std::endl;
  AH_DEBUG << "primariesbotz = " << primariesbotz << std::endl;
  AH_DEBUG << "secondariesbotz = " << secondariesbotz << std::endl;
  AH_DEBUG << "secondariestopz = " << secondariestopz << std::endl;
  
  
  // initialize the array of XRTObjects
  XRTObjects.resize(numXRTObjects);
  
  // used when looping through all XRT objects
  Obstruction & currObstruct = obstructions[0];
  
  // loop over XRT objects, setting up some of the xrtobject variables 
  // First do obstructions, then mirror foils, then pre-collimator foils
  
  // First loop over the obstructions
  for (int iObs = 0 ; iObs < obstruct.m_numObst ; ++iObs, ++objectIdx) {
    
    XRTObject & currObject = XRTObjects[objectIdx];
    currObject.m_objectIdx = objectIdx;
    currObstruct = obstructions[iObs];
    
    currObject.m_type = OBSTRUCTION;
    currObject.m_set = currObstruct.m_set;
    currObject.m_geometry = OBSTRUCT;
    
    // No reflection on obstructions; leave xrtobjectreflindex with zero values.
    
    // No xrtobjectsectorid for obstructions; leave with zero values
    
    // The only misalignments (shifts/rotations) we apply to the obstructions 
    // are the ones associated with bulk shifts/rotations of the primary or 
    // secondary mirror units and housing. 
    // currObject.m_set = 1,2 for primary and 3,4 for secondary units.
    // +++ NOTE: We need to verify the above, and it may change if we have 
    // misunderstood how exactly the support structures are attached to the 
    // telescope.
    if ( (currObject.m_set == 1) || (currObject.m_set == 2) ) { 
      currObject.m_xHousingShift = housings[PRIMARY].m_xShift;
      currObject.m_yHousingShift = housings[PRIMARY].m_yShift;
    } else if ( (currObject.m_set == 3) || (currObject.m_set == 4) ) { 
      currObject.m_xHousingShift = housings[SECONDARY].m_xShift;
      currObject.m_yHousingShift = housings[SECONDARY].m_xShift;
    }
    
    // calculate the bounding box for this obstruction (in the default 
    // position, without any shifts or rotations)
    ObjectBoundingBox obstructBBoxCoords;
    std::vector<CartesianCoord> obstructBBoxVertices;
    // Output bounding box for one obstruction
    makeObstructBBox(currObstruct.m_vertices, obstructBBoxCoords, obstructBBoxVertices);
    // Transfer result to the master array xrtobjectboundingbox()
    currObject.m_BBox = obstructBBoxCoords;
    // Also transfer the vertices of the obstructions to the master vertices array.
    //+++ ? this I think was incorrect: currObject.m_vertices = obstructBBoxVertices;
    currObject.m_vertices = currObstruct.m_vertices;
    currObject.m_numVertices = currObstruct.m_numVertices;
    
    // test that I stored the correct vertices for each obstruction XRTObject
//    AH_DEBUG << "XRTObject id " << objectIdx << " has " << currObject.m_numVertices << " vertices" << std::endl;
//    for (int i = 0 ; i < currObject.m_numVertices ; ++i) {
//      AH_DEBUG << "currObject.m_vertices["<<i<<"] = " << currObject.m_vertices[i] << std::endl;
//    }
    
    // For obstructions, put dummy negative values in the sidewalls arrays 
    // (start angle and end angle of radial sidewalls), which will tell the 
    // ray-tracing code that the obstruction XRT objects do not have sidewalls
    currObject.m_sideWalls[0] = dummy;
    currObject.m_sideWalls[1] = dummy;
    
    // Set "do transmission" and "do scattering" flags to false 
    // (this could be different in the future)
    currObject.m_doTransmission = false;
    currObject.m_doScattering = false;
    
    // Stuff required for fastmode (but it is done even if fastmode is not 
    // selected because it may be used to improve the speed of regular mode)
    // Here we establish which sets of obstructions are above the primary 
    // mirrors, which set is inbetween the primary and secondary mirrors, and 
    // which set is below the secondary mirrors. 
    // The first of these groups of obstructions are the ones first encountered 
    // by a photon upon entering the telescope aperture.
    // Note that this relies on the rows in the TDF describing the obstructions 
    // (or the corresponding array of z-coordinates) is sorted in order of 
    // descending z.
    // +++ with that assumption, we should do error checking
    
    double currZCoord = obstructions.at(iObs).m_zCoord;
    
    if (currZCoord >= primariestopz) {
      // Above the primaries? first obstruction interval
      
      currObject.m_interval = ABOVE;
      
      // if we haven't set the low index yet (if it's still negative), set it
      // to this first object id we come to in this group
      // +++ why not just set this to 0, since we wrote the code that has obstructions first in the xrtobjects list?
      //     obstructionIntervalLowIndex.at(0) = 0;
      if (obstructionIntervalLowIndex.at(ABOVE) < 0) {
        obstructionIntervalLowIndex.at(ABOVE) = iObs;
      }
      // since the XRTObjects (obstructions) are in decending z order, see if 
      // this object id is greater than what is currently in high index
      // +++ why not just assign this = iObs-1 when we reach the secondaries?
      if (iObs > obstructionIntervalHighIndex.at(ABOVE)) {
        obstructionIntervalHighIndex.at(ABOVE) = iObs;
      }
      
    } else if ( (currZCoord <= primariesbotz) && 
                (currZCoord >= secondariestopz) ) {
      // between primaries and secondaries? second obstruction interval
      
      currObject.m_interval = BETWEEN;
      
      if (obstructionIntervalLowIndex.at(BETWEEN) < 0) {
        obstructionIntervalLowIndex.at(BETWEEN) = iObs;
      }
      if (iObs > obstructionIntervalHighIndex.at(BETWEEN)) {
        obstructionIntervalHighIndex.at(BETWEEN) = iObs;
      }
      
    } else if (currZCoord <= secondariesbotz) { 
      // Below the secondaries? third obstruction interval
      
      currObject.m_interval = BELOW;
      
      if (obstructionIntervalLowIndex.at(BELOW) < 0) {
        obstructionIntervalLowIndex.at(BELOW) = iObs;
      }
      if (iObs > obstructionIntervalHighIndex.at(BELOW)) {
        obstructionIntervalHighIndex.at(BELOW) = iObs;
      }
    }
    
//    AH_DEBUG << "objectIdx " << objectIdx << ": " << currObstruct.m_zCoord << std::endl;
    
  } // end for-loop for support structure portions of the xrtobject*** arrays
  
  
  /* testing that I stored correct obstruction interval indices */
  #ifdef DEBUG
  #endif
  AH_DEBUG << "*** stored correct obstruction interval indices ***" << std::endl;
  AH_DEBUG << "INTERVAL \t LOW INDEX \t HIGH INDEX" << std::endl;
  for (int iInterval = 0 ; iInterval < s_numObstrIntervals ; ++iInterval) {
    AH_DEBUG <<  iInterval << " " << '\t' <<  '\t' << " " << 
                 obstructionIntervalLowIndex.at(iInterval) << '\t' <<  '\t' << " " << 
                 obstructionIntervalHighIndex.at(iInterval) << std::endl;;
  }
  
  // Now loop over the mirror fragments
  for (int iMir = 0 ; iMir < mirror.m_numRows ; ++iMir, ++objectIdx) {
    
    XRTObject & currObject = XRTObjects[objectIdx];
    currObject.m_objectIdx = objectIdx;
    
    currObject.m_type = FOIL;
    currObject.m_set = mirror.m_set[iMir];
    
    switch (mirror.m_function[iMir]) {
      case 0:
        currObject.m_geometry = OBSTRUCT;
        break;
      case 1:
        currObject.m_geometry = CYLINDER;
        break;
      case 2:
        currObject.m_geometry = CONE;
        break;
      default:
        AH_THROW_RUNTIME("Only values of 1 and 2 are supported for the 'function' column in the TDF");
      
    }
    
    // Do reflection indices: frontReflindex() and backMirrorReflIndex() were 
    // calculated earlier - see notes there.
    currObject.m_reflIndexBack = backMirrorReflIndex[iMir];
    currObject.m_reflIndexFront = frontReflindex[iMir];
    
    // store the start and end angles of this XRTObject
    currObject.m_startAngle = mirror.m_startAngle[iMir];
    currObject.m_endAngle   = mirror.m_endAngle[iMir];
    
    // Do sidewall angles: use the information in the TDF to flag whether the 
    // boundary angles should be negative (indicating there is no sidewall for 
    // this object)
    currObject.m_sideWalls.resize(currObject.m_numSideWalls);
    if (!mirror.m_startCross[iMir]) {
      currObject.m_sideWalls[0] = mirror.m_startAngle[iMir];
    } else {
      currObject.m_sideWalls[0] = -1.0 * mirror.m_startAngle[iMir];
    }
    if (!mirror.m_endCross[iMir]) {
      currObject.m_sideWalls[1] = mirror.m_endAngle[iMir];
    } else {
      currObject.m_sideWalls[1] = -1.0 * mirror.m_endAngle[iMir];
    }
    currObject.m_sideWalls[2] = mirror.m_tanStartAngle[iMir];
    currObject.m_sideWalls[3] = mirror.m_tanEndAngle[iMir];
    currObject.m_sideWalls[4] = std::fmod(mirror.m_startAngle[iMir], s_twopi);
    currObject.m_sideWalls[5] = std::fmod(mirror.m_endAngle[iMir], s_twopi);
    
    // back and front mirror slopes and their squares
    currObject.m_slopes.m_back = backMirrorSlope[iMir];
    currObject.m_slopes.m_backSq = backMirrorSlopeSq[iMir];
    currObject.m_slopes.m_front = frontMirrorSlope[iMir];
    currObject.m_slopes.m_frontSq = frontMirrorSlopeSq[iMir];
    
    // geometrical quantities for the mirror foils
    currObject.m_geoParams[0]  = mirror.m_topZ[iMir];
    currObject.m_geoParams[1]  = mirror.m_botZ[iMir];
    currObject.m_geoParams[2]  = mirror.m_botZ[iMir] * mirror.m_botZ[iMir];
    currObject.m_geoParams[3]  = mirror.m_botInnerRadius[iMir];
    currObject.m_geoParams[4]  = mirror.m_botOuterRadius[iMir];
    currObject.m_geoParams[5]  = mirror.m_botInnerRadius[iMir] * mirror.m_botInnerRadius[iMir];
    currObject.m_geoParams[6]  = mirror.m_botOuterRadius[iMir] * mirror.m_botOuterRadius[iMir];
    currObject.m_geoParams[7]  = mirror.m_botZ[iMir] * mirror.m_botInnerRadius[iMir];
    currObject.m_geoParams[8]  = mirror.m_botZ[iMir] * mirror.m_botOuterRadius[iMir];
    currObject.m_geoParams[9]  = mirror.m_topInnerRadius[iMir];
    currObject.m_geoParams[10] = mirror.m_topOuterRadius[iMir];
    currObject.m_geoParams[11] = mirror.m_topInnerRadius[iMir] * mirror.m_topInnerRadius[iMir];
    currObject.m_geoParams[12] = mirror.m_topOuterRadius[iMir] * mirror.m_topOuterRadius[iMir];
    currObject.m_geoParams[13] = backMirrorSlope[iMir] * mirror.m_botOuterRadius[iMir];
    currObject.m_geoParams[14] = frontMirrorSlope[iMir] * mirror.m_botInnerRadius[iMir];
    currObject.m_geoParams[15] = mirror.m_botZ[iMir] - currObject.m_geoParams[13];
    currObject.m_geoParams[16] = mirror.m_botZ[iMir] - currObject.m_geoParams[14];
    currObject.m_geoParams[17] = mirror.m_topZ[iMir] - currObject.m_geoParams[15];
    currObject.m_geoParams[18] = mirror.m_topZ[iMir] - currObject.m_geoParams[16];
    
    // assign the sector IDs
    currObject.m_sectorID = mirrorSectorID[iMir] + 1;
    // see if we have more sectors to add to total number
    if (currObject.m_sectorID > sectorsShells.m_numSectors) {
      sectorsShells.m_numSectors = currObject.m_sectorID;
    }
    
    // assign the shell number
    currObject.m_shellNumber = mirror.m_number[iMir];
    // see if we have more shells to add to total number
    if (currObject.m_shellNumber > sectorsShells.m_numShells) {
      sectorsShells.m_numShells = currObject.m_shellNumber;
    }
    
    // assign the segment IDs and sector numbers
    currObject.m_segmentID = mirror.m_segment[iMir];
    currObject.m_sectorNumber = mirror.m_sectorNumber[iMir];
  
    // assign the object id to the array xrtRegionToObjectID
    xrtRegionToObjectID[currObject.m_set-1][currObject.m_segmentID-1][mirror.m_sectorNumber[iMir]-1][mirror.m_number[iMir]-1] = objectIdx;
    
    // assign the sector shift to each mirror sector fragment using the sectorshifts array
    // NOTE: This will likely change since the sector shifts and how to implement them are currently under discussion
    currObject.m_rSectorShift = mirror.m_sectorShift[iMir];
    // Do bulk housing shifts & rotations to the primary and secondary units
    currObject.m_xHousingShift = housings[mirror.m_set[iMir]].m_xShift;
    currObject.m_yHousingShift = housings[mirror.m_set[iMir]].m_yShift;
    
    // Now do the segment transformation assignments. The following makes use 
    // of the new proposed format of the TDF SEGMENT extension. Specifically, 
    // we are assuming, for each of the x,y,z shifts, segmentlayer=1 refers to 
    // primary mirrors and segmentlayer=2 refers to secondary mirrors. If this 
    // is not the case, the index will have to be redefined.
    //+++ check -1
    currObject.m_xSegmentShift = seg.m_xShift[currObject.m_set][currObject.m_segmentID-1];
    currObject.m_ySegmentShift = seg.m_yShift[currObject.m_set][currObject.m_segmentID-1];
    currObject.m_zSegmentShift = seg.m_zShift[currObject.m_set][currObject.m_segmentID-1];
    currObject.m_xSegmentRot = seg.m_xRotation[currObject.m_set][currObject.m_segmentID-1];
    currObject.m_ySegmentRot = seg.m_yRotation[currObject.m_set][currObject.m_segmentID-1];
    currObject.m_zSegmentRot = seg.m_zRotation[currObject.m_set][currObject.m_segmentID-1];

    // 140923: The calculation of randomized tilt and twist misalignment angles is now removed. Only the angles in the SYSTILT and SYSTWIST columns in the TDF are now used (but there are now three values for the tilt axes fractional height).
    if (transforms.m_doTransforms) {
      currObject.m_tiltAngle = mirror.m_sysTilt[iMir];
      currObject.m_twistAngle = mirror.m_sysTwist[iMir];
    } else {
      currObject.m_tiltAngle = 0.0;
      currObject.m_twistAngle = 0.0;
    } // end if-block doTransforms
    
    // The coordinate axes quadrants occupied by the foil
    // (index 0=start quadrant, 1=end quadrant, 2=number of quadrants)
    //+++ redo this to use CartesianCoord structs
    makeFoilBBox(mirror.m_botInnerRadius[iMir], mirror.m_botOuterRadius[iMir], 
                 mirror.m_topInnerRadius[iMir], mirror.m_topOuterRadius[iMir], 
                 mirror.m_startAngle[iMir], mirror.m_endAngle[iMir], 
                 mirror.m_botZ[iMir], mirror.m_topZ[iMir], 
                 foilbboxcoords, foilBBox, foilvertices, foilcoordquadrant);
    
    // Transfer the bounding box parameters and other outputs to the respective 
    // master XRT object arrays
    currObject.m_BBox = foilBBox;
    currObject.m_coordQuads = foilcoordquadrant;
    CartesianCoord currVertex;
    //+++ check that this is correct, or change makeFoilBBox to output CartesianCoord vector
    for (int i = 0 ; i < numVertices ; ++i) {
      currVertex.m_x = foilvertices[0][i];
      currVertex.m_y = foilvertices[1][i];
      currVertex.m_z = foilvertices[2][i];
      currObject.m_vertices.push_back(currVertex);
    }
    currObject.m_numVertices = numVertices;
    
    // Set "do transmission" flag individually for each XRT object, 
    // according to the input parameter file.
    if ( isEqualCaseInsens(param.m_transmode, "all") || 
         isEqualCaseInsens(param.m_transmode, "mirror") ) {
      currObject.m_doTransmission = true;
    } else {
      currObject.m_doTransmission = false;
    }
    
    // Set "do scattering" flag individually for each XRT object, 
    // according to the input parameter file.
    if ( (isEqualCaseInsens(param.m_scattermode, "all") || 
          isEqualCaseInsens(param.m_scattermode, "mirror") ) &&
         ((backmirrorscatterindex[iMir] >= 0) && (frontmirrorscatterindex[iMir] >= 0)) ) { 
      currObject.m_doScattering = true; 
      // pointers to scattering probability arrays
      currObject.m_scatterIndexBack = backmirrorscatterindex[iMir];
      currObject.m_scatterIndexFront = frontmirrorscatterindex[iMir];
    } else {
      currObject.m_doScattering = false;
    }
    
//    AH_DEBUG << "objectIdx " << objectIdx << " iMir " << iMir << 
//                ": m_reflIndexFront = " << currObject.m_reflIndexFront << 
//            " sectornumber = " << currObject.m_sectorNumber << 
//            " shell number = " << currObject.m_shellNumber << 
//            std::endl;
    
  } // end for-loop over mirrors
  
  AH_DEBUG << "uniqueFrontNames.size() = " << uniqueFrontNames.size() << std::endl;
  for (uint iName = 0 ; iName < uniqueFrontNames.size() ; ++iName) {
    AH_DEBUG << "uniqueFrontNames["<<iName<<"] = " << uniqueFrontNames[iName] << std::endl;
  }
  
  
  // Next, loop over pre-collimator foils (if a pre-collimator exists)
  if (param.m_pcolExists) {
    for (int iFoil = 0 ; iFoil < pcol.m_numRows ; ++iFoil, ++objectIdx) {
      
      XRTObject & currObject = XRTObjects[objectIdx];
      currObject.m_objectIdx = objectIdx;
      
      currObject.m_type = FOIL;
      currObject.m_set = 0; //+++ make this a const variable somewhere
      
      switch (pcol.m_function[iFoil]) {
        case 0:
          currObject.m_geometry = OBSTRUCT;
          break;
        case 1:
          currObject.m_geometry = CYLINDER;
          break;
        case 2:
          currObject.m_geometry = CONE;
          break;
        default:
          AH_THROW_RUNTIME("Only values of 1 and 2 are supported for the 'function' column in the TDF.");
      }
      // +++ print these out, make sure they're cylinders
      
      // Do reflection indices: pcolbackreflindex() and pcolfrontReflindex() 
      // were calculated earlier - see notes there
      currObject.m_reflIndexBack = pcolBackReflIndex[iFoil];
      currObject.m_reflIndexFront = pcolFrontReflIndex[iFoil];
    
      // store the start and end angles of this XRTObject
      currObject.m_startAngle = pcol.m_startAngle[iFoil];
      currObject.m_endAngle   = pcol.m_endAngle[iFoil];
    
      // Do sidewall angles: use the information in the TDF to flag whether 
      // the boundary angles should be negative (indicating there is no 
      // sidewall for this object)
      if (!pcol.m_startCross[iFoil]) {
        currObject.m_sideWalls[0] = pcol.m_startAngle[iFoil];
      } else {
        currObject.m_sideWalls[0] = -1.0 * pcol.m_startAngle[iFoil];
      } 
      if (!pcol.m_endCross[iFoil]) {
        currObject.m_sideWalls[1] = pcol.m_endAngle[iFoil];
      } else {
        currObject.m_sideWalls[1] = -1.0 * pcol.m_endAngle[iFoil];
      }
      currObject.m_sideWalls[2] = pcol.m_tanStartAngle[iFoil];
      currObject.m_sideWalls[3] = pcol.m_tanEndAngle[iFoil];
      currObject.m_sideWalls[4] = std::fmod(pcol.m_startAngle[iFoil], s_twopi);
      currObject.m_sideWalls[5] = std::fmod(pcol.m_endAngle[iFoil], s_twopi);
      
      // back and front pre-collimator foil slopes and their squares
      currObject.m_slopes.m_back    = backpcolslope[iFoil];
      currObject.m_slopes.m_front   = frontpcolslope[iFoil];
      currObject.m_slopes.m_backSq  = backpcolslopesq[iFoil];
      currObject.m_slopes.m_frontSq = frontpcolslopesq[iFoil];
      
      // geometrical quantities for the pre-collimator foils
      // NOTE: In general different pre-collimator geometries will not be 
      // characterized by the same parameters so to retain generality, the 
      // structure xrtobjectgeoparams should not have name segments that are 
      // specific to these particular parameters.
      currObject.m_geoParams[0] = pcol.m_topZ[iFoil];
      currObject.m_geoParams[1] = pcol.m_botZ[iFoil];
      currObject.m_geoParams[2] = pcol.m_botZ[iFoil] * pcol.m_botZ[iFoil];
      currObject.m_geoParams[3] = pcol.m_botInnerRadius[iFoil];
      currObject.m_geoParams[4] = pcol.m_botOuterRadius[iFoil];
      currObject.m_geoParams[5] = pcol.m_botInnerRadius[iFoil] * pcol.m_botInnerRadius[iFoil];
      currObject.m_geoParams[6] = pcol.m_botOuterRadius[iFoil] * pcol.m_botOuterRadius[iFoil];
      // following is the product of the z-coordinate of the bottom of a foil and the inner bottom radius
      currObject.m_geoParams[7] = pcol.m_botZ[iFoil] * pcol.m_botInnerRadius[iFoil];
      // following is the product of the z-coordinate of the bottom of a foil and the outer bottom radius
      currObject.m_geoParams[8] = pcol.m_botZ[iFoil] * pcol.m_botOuterRadius[iFoil];
      // top radii and their squares
      currObject.m_geoParams[9] = pcol.m_topInnerRadius[iFoil];
      currObject.m_geoParams[10] = pcol.m_topOuterRadius[iFoil];
      currObject.m_geoParams[11] = pcol.m_topInnerRadius[iFoil] * pcol.m_topInnerRadius[iFoil];
      currObject.m_geoParams[12] = pcol.m_topOuterRadius[iFoil] * pcol.m_topOuterRadius[iFoil];
      currObject.m_geoParams[13] = backpcolslope[iFoil] * pcol.m_botOuterRadius[iFoil];
      currObject.m_geoParams[14] = frontpcolslope[iFoil] * pcol.m_botInnerRadius[iFoil];
      currObject.m_geoParams[15] = pcol.m_botZ[iFoil] - currObject.m_geoParams[13];
      currObject.m_geoParams[16] = pcol.m_botZ[iFoil] - currObject.m_geoParams[14];
      currObject.m_geoParams[17] = pcol.m_topZ[iFoil] - currObject.m_geoParams[15];
      currObject.m_geoParams[18] = pcol.m_topZ[iFoil] - currObject.m_geoParams[16];
      
      // assign the sector IDs
      currObject.m_sectorID = pcolsectorid[iFoil]+1;
      // see if we have more sectors to add to total number
      if (currObject.m_sectorID > sectorsShells.m_numSectors) {
        sectorsShells.m_numSectors = currObject.m_sectorID;
      }
      
      // assign the shell number
      currObject.m_shellNumber = pcol.m_number[iFoil];
      // see if we have more shells to add to total number
      if (currObject.m_shellNumber > sectorsShells.m_numShells) {
        sectorsShells.m_numShells = currObject.m_shellNumber;
      }
      
      // assign the sector shift to each pre-collimator fragment using the 
      // sectorshifts array
      // NOTE: This will likely change since the sector shifts and how to 
      // implement them are currently under discussion
      currObject.m_rSectorShift = pcol.m_sectorShift[iFoil];
      
      // assign the segment IDs and sector numbers 
      currObject.m_segmentID = pcol.m_segment[iFoil];
      currObject.m_sectorNumber = pcol.m_sectorNumber[iFoil];
    
      // assign the object id to the array xrtRegionToObjectID
      xrtRegionToObjectID[pcol.m_set[iFoil]+1][currObject.m_segmentID-1][pcol.m_sectorNumber[iFoil]-1][pcol.m_number[iFoil]-1] = objectIdx;

      // Do the bulk housing shifts & rotations for the pre-collimator foils 
      currObject.m_xHousingShift = housings[PCOL].m_xShift;
      currObject.m_yHousingShift = housings[PCOL].m_yShift;
      
      // Now do the segment transformation assignments. The following makes 
      // use of the new proposed format of the TDF SEGMENT extension. 
      // Specifically, we are assuming, for each of the x,y,z shifts, 
      // segmentlayer=0 refers to pre-collimators. If this is not the case, 
      // the index will have to modified.
      currObject.m_xSegmentShift = seg.m_xShift[currObject.m_set][currObject.m_segmentID-1];
      currObject.m_ySegmentShift = seg.m_yShift[currObject.m_set][currObject.m_segmentID-1];
      currObject.m_zSegmentShift = seg.m_zShift[currObject.m_set][currObject.m_segmentID-1]; 
      currObject.m_xSegmentRot = seg.m_xRotation[currObject.m_set][currObject.m_segmentID-1];
      currObject.m_ySegmentRot = seg.m_yRotation[currObject.m_set][currObject.m_segmentID-1]; 
      currObject.m_zSegmentRot = seg.m_zRotation[currObject.m_set][currObject.m_segmentID-1];

      
      // 140923: The calculation of randomized tilt and twist misalignment angles is now removed. Only the angles in the SYSTILT and SYSTWIST columns in the TDF are now used (but there are now three values for the tilt axes fractional height).
      if (transforms.m_doTransforms) {
        currObject.m_tiltAngle = pcol.m_sysTilt[iFoil];
        currObject.m_twistAngle = pcol.m_sysTwist[iFoil];
      } else {
        currObject.m_tiltAngle = 0.0;
        currObject.m_twistAngle = 0.0;
      } // end if-block doTransforms
      
      // Call routine to calculate the bounding box for this pre-collimator 
      // foil object
      // The outputs foilbboxcoords,foilvertices,foilcoordquad are defined in 
      // the same way for the pre-collimator foils as they are for the mirror 
      // foils (see mirror loop above)
      makeFoilBBox(pcol.m_botInnerRadius[iFoil], pcol.m_botOuterRadius[iFoil], 
                   pcol.m_topInnerRadius[iFoil], pcol.m_topOuterRadius[iFoil], 
                   pcol.m_startAngle[iFoil], pcol.m_endAngle[iFoil], 
                   pcol.m_botZ[iFoil], pcol.m_topZ[iFoil], 
                   foilbboxcoords, foilBBox, foilvertices, foilcoordquadrant);
      
      //+++ make sure I'm clearing these output arrays inside makeFoilBBox
      // Transfer the outputs of makefoilbbox to the respective master arrays
      currObject.m_BBox = foilBBox; 
      currObject.m_coordQuads = foilcoordquadrant;
      currObject.m_numVertices = numVertices;
      
      // Set "do transmission" flag individually for each XRT object, 
      // according to the input parameter file
      if ( isEqualCaseInsens(param.m_transmode, "all") || 
           isEqualCaseInsens(param.m_transmode, "pcol") ) {
        currObject.m_doTransmission = true;
      } else {
        currObject.m_doTransmission = false;
      }
      
      // Set "do scattering" flag individually for each XRT object, 
      // according to the input parameter file
      if ( (isEqualCaseInsens(param.m_scattermode, "all") || 
            isEqualCaseInsens(param.m_scattermode, "pcol")) &&
              (pcolscatterindex[iFoil] > 0) ) { 
        currObject.m_doScattering = true;
        // pointers to scattering probability arrays 
        currObject.m_scatterIndexBack = pcolscatterindex[iFoil];
        currObject.m_scatterIndexFront = pcolscatterindex[iFoil];
      } else {
        currObject.m_doScattering = false;
      }
      
    } // end for-loop populating xrtobject*** arrays, pre-collimator
  } // end if-block, pre-collimator exists
  
  
  // -------------------------------------
  // Sort the XRT bounding boxes by their shortest distance from the results 
  // plane (focal plane by default)
  // -------------------------------------
  
  // Create an array of indices that point to elements in the bounding box array xrtobjectboundingbox (and related arrays) that correspond to sorting on zmin (zmin is the lowest z-coordinate of an xrt object). The sort order is descending, starting from the highest zmin. It is ok to sort on zmin using the bounding boxes before the coordinate transformations are applied because we only need crude z-coordinate divisions in the telescope structure to implement a scheme that improves run time by restricting the number of candidate photon/xrt object interactions. We also do not need to distinguish between XRT object types because the routine that finds candidate XRT objects for a photon to interact with operates on all object types.
  for (int iObj = 0 ; iObj < numXRTObjects ; ++iObj) {
    zminsortxrtobjectindexMap.insert(std::make_pair(XRTObjects[iObj].m_BBox.m_zMin, iObj));
    zmaxsortxrtobjectindexMap.insert(std::make_pair(XRTObjects[iObj].m_BBox.m_zMax, iObj));
    zminsortxrtobjectindexMultiMap.insert(std::make_pair(XRTObjects[iObj].m_BBox.m_zMin, iObj));
    zmaxsortxrtobjectindexMultiMap.insert(std::make_pair(XRTObjects[iObj].m_BBox.m_zMax, iObj));
  }
    
  // keep a vector holding the indices, instead of a map (so that in interceptXRTObjects() we can reference elements by index)
  // because the map is in sorted order, we need a reverse iterator here to get
  // the vector sorted how we want
  for (std::map<double, long>::reverse_iterator iter = zminsortxrtobjectindexMap.rbegin() ; 
       iter != zminsortxrtobjectindexMap.rend() ; ++iter) {
    zminsortxrtobjectindex.push_back(iter->second);
  }
  for (std::map<double, long>::reverse_iterator iter = zmaxsortxrtobjectindexMap.rbegin() ; 
       iter != zmaxsortxrtobjectindexMap.rend() ; ++iter) {
    zmaxsortxrtobjectindex.push_back(iter->second);
  }
  // we need an array with ALL zmax values
  for (std::multimap<double, long>::reverse_iterator iter = zmaxsortxrtobjectindexMultiMap.rbegin() ; 
       iter != zmaxsortxrtobjectindexMultiMap.rend() ; ++iter) {
    zmaxsortxrtobjectindexAll.push_back(iter->second);
  }
    
  // Divide the telescope into several levels in the z-direction, which group 
  // XRT objects that occupy similar z-coordinate intervals
  // Go through the bounding box array to pick out values of zmin that lie 
  // either side of gaps in zmin. This procedure identifies several "natural" 
  // boundaries (or levels) in the z-direction of the telescope and will be 
  // used to define intervals in which to find photon/XRT-object interaction 
  // candidates (we could choose arbitrary z values for this but it is 
  // convenient to use the boundaries that already exist). The first z-level 
  // will always be the highest z that any XRT object has, and the last z-level 
  // will always be the lowest z that any xrt object has.
  // The z-levels array (xrtzlevels) will be much smaller than numXRTObjects, 
  // but we don’t necessarily know the size in advance so set it up with size 
  // numXRTObjects since it will never be larger than numXRTObjects. The number 
  // of levels will be assigned to the variable numxrtzlevels.
  
  // +++ clear all the input/output structs up at top of function?
  xrtzlevels.clear();
  xrtzlevels.reserve(numXRTObjects);
  
  // set the first level to the highest z-coordinate, the highest ZMAX
  zLevelIdx = zmaxsortxrtobjectindex[0];
  xrtzlevels.push_back(XRTObjects[zLevelIdx].m_BBox.m_zMax);
  
  // set the next level to the highest ZMIN, if it is different to the above
  zLevelIdx = zminsortxrtobjectindex[0];
  nextZLevel = XRTObjects[zLevelIdx].m_BBox.m_zMin;
  if ( nextZLevel != xrtzlevels[0] ) {
    xrtzlevels.push_back(nextZLevel);
  }
  
  // Loop over xrtobjectboundingbox zmin values, starting from the second one 
  // (since the first was treated above), testing if the difference between 
  // this zmin value and the last one is greater than 0.
  for (uint iObj = 1 ; iObj < zminsortxrtobjectindex.size() ; ++iObj) {
    nextZLevel = XRTObjects[zminsortxrtobjectindex[iObj]].m_BBox.m_zMin;
    prevZLevel = XRTObjects[zminsortxrtobjectindex[iObj-1]].m_BBox.m_zMin;
    zLevelDelta = std::abs(nextZLevel - prevZLevel);
    if ( zLevelDelta > 0.0 ) {
      xrtzlevels.push_back(nextZLevel);
    }
  }
  numxrtzlevels = xrtzlevels.size();
  
  for (int izLevel = 0 ; izLevel < numxrtzlevels ; ++izLevel) {
    AH_DEBUG << "zlevel["<<izLevel<<"] = " << xrtzlevels.at(izLevel) << std::endl;
  }
  
  // +++ --------------------- new stuff ------------------------ v
  
  // Any external objects that are not above or below the telescope are 
  // ignored and not included

  // store the telescope top and bottom z-coords. 
  telescopetopz = xrtzlevels.at(0);
  telescopebottomz = xrtzlevels.at(numxrtzlevels-1);

  // The following will be the z-coordinate of the top of the thick housing 
  // wall in the hole, and the inner radius of that wall, respectiviely. 
  // Initialize to default values, which will be over-written if a valid 
  // candidate is found in the TDF
  
  genTelescope.m_holewalltopz = telescopetopz;
  genTelescope.m_holesideinnerradius = genTelescope.m_innerhousingradius;

  AH_DEBUG << "telescopetopz = " << telescopetopz << std::endl;
  AH_DEBUG << "telescopebottomz = " << telescopebottomz << std::endl;

  AH_DEBUG << "do external objects? " << ( param.m_doExternalObjects ? "yes" : "no" ) << std::endl;
  
  // Seperate external objects into three groups, above and below the telescope
  if (param.m_doExternalObjects) {
    
    // store z-coord of external objects (thermal shield) closest to telescope 
    topExtObjects.m_zCoord = tdfExtObjects.m_zMax.at(0);
    bottomExtObjects.m_zCoord = tdfExtObjects.m_zMax.at(0);

    // store the largest value of mabsindex, to double check agains the 
    // reflectivity file later that there are indeed the required number of 
    // columns for mass absorption coefficients
    topExtObjects.m_maxMabsIndex = getMaxLong(tdfExtObjects.m_mabsIndex);
    bottomExtObjects.m_maxMabsIndex = topExtObjects.m_maxMabsIndex;

    for (int iExtObj = 0 ; iExtObj < tdfExtObjects.m_numExtObjectParts ; ++iExtObj) {
      
      AH_DEBUG << "iExtObj = " << iExtObj << std::endl;
      
      if ( param.m_doTopExternalObjects && 
           (tdfExtObjects.m_zMin.at(iExtObj) >= telescopetopz) ) {
        
        topExtObjects.m_object.push_back(tdfExtObjects.m_objectNumber.at(iExtObj));
        topExtObjects.m_subObject.push_back(tdfExtObjects.m_partNumber.at(iExtObj));
        topExtObjects.m_rMin.push_back(tdfExtObjects.m_radMin.at(iExtObj));
        topExtObjects.m_rMax.push_back(tdfExtObjects.m_radMax.at(iExtObj));
        topExtObjects.m_startAngleDeg.push_back(tdfExtObjects.m_startAngle.at(iExtObj));
        topExtObjects.m_endAngleDeg.push_back(tdfExtObjects.m_endAngle.at(iExtObj));
        topExtObjects.m_startAngleRad.push_back(tdfExtObjects.m_startAngle.at(iExtObj) * s_degreesToRadian);
        topExtObjects.m_endAngleRad.push_back(tdfExtObjects.m_endAngle.at(iExtObj) * s_degreesToRadian);
        topExtObjects.m_zMin.push_back(tdfExtObjects.m_zMin.at(iExtObj));
        topExtObjects.m_zMax.push_back(tdfExtObjects.m_zMax.at(iExtObj));
        topExtObjects.m_thickness.push_back(tdfExtObjects.m_zMax.at(iExtObj) - tdfExtObjects.m_zMin.at(iExtObj));
        topExtObjects.m_density.push_back(tdfExtObjects.m_density.at(iExtObj));
        topExtObjects.m_mabsIndex.push_back(tdfExtObjects.m_mabsIndex.at(iExtObj));
        
        if (tdfExtObjects.m_zMin.at(iExtObj) <= topExtObjects.m_zCoord) {
          topExtObjects.m_zCoord = tdfExtObjects.m_zMin.at(iExtObj);
        }
        
        topExtObjects.m_numExtObjectParts++;
        
      } else if ( param.m_doBottomExternalObjects && 
                  (tdfExtObjects.m_zMax.at(iExtObj) <= telescopebottomz) ) {
        
        bottomExtObjects.m_object.push_back(tdfExtObjects.m_objectNumber.at(iExtObj));
        bottomExtObjects.m_subObject.push_back(tdfExtObjects.m_partNumber.at(iExtObj));
        bottomExtObjects.m_rMin.push_back(tdfExtObjects.m_radMin.at(iExtObj));
        bottomExtObjects.m_rMax.push_back(tdfExtObjects.m_radMax.at(iExtObj));
        bottomExtObjects.m_startAngleDeg.push_back(tdfExtObjects.m_startAngle.at(iExtObj));
        bottomExtObjects.m_endAngleDeg.push_back(tdfExtObjects.m_endAngle.at(iExtObj));
        bottomExtObjects.m_startAngleRad.push_back(tdfExtObjects.m_startAngle.at(iExtObj) * s_degreesToRadian);
        bottomExtObjects.m_endAngleRad.push_back(tdfExtObjects.m_endAngle.at(iExtObj) * s_degreesToRadian);
        bottomExtObjects.m_zMin.push_back(tdfExtObjects.m_zMin.at(iExtObj));
        bottomExtObjects.m_zMax.push_back(tdfExtObjects.m_zMax.at(iExtObj));
        bottomExtObjects.m_thickness.push_back( tdfExtObjects.m_zMax.at(iExtObj) - tdfExtObjects.m_zMin.at(iExtObj));
        bottomExtObjects.m_density.push_back(tdfExtObjects.m_density.at(iExtObj));
        bottomExtObjects.m_mabsIndex.push_back(tdfExtObjects.m_mabsIndex.at(iExtObj));
        
        if (tdfExtObjects.m_zMax.at(iExtObj) <= bottomExtObjects.m_zCoord) {
          bottomExtObjects.m_zCoord = tdfExtObjects.m_zMax.at(iExtObj);
        }
        
        bottomExtObjects.m_numExtObjectParts++;
        
      } else if ( (tdfExtObjects.m_zMax.at(iExtObj)   <  telescopetopz) && 
                  (tdfExtObjects.m_radMax.at(iExtObj) <= genTelescope.m_innerhousingradius) && 
                  (tdfExtObjects.m_radMin.at(iExtObj) <  genTelescope.m_innerhousingradius) &&
                  (tdfExtObjects.m_zMax.at(iExtObj)   >= housings[PCOL].m_zLower) && 
                  (genTelescope.m_numcentralextobjectparts < 1) ) {
        
        AH_DEBUG << "add to central" << std::endl;
        
        // This part of the if-block finds the central wall structure inside 
        // the hole if it exists in the TDF extension
        // Note that the condition houssinggeometry(0,2)=keyword PCOLZMIN, or 
        // the z-coordinate of the bottom of the pre-collimator, so the 
        // condition extobjects.zmax>=housinggeometry(0,2) means that to 
        // qualify, the central object must have a height that at least reaches 
        // the pre-collimator bottom; there should not be a situation when this 
        // is not true because then X-ray entering the hole could directly 
        // illuminate primary or secondary mirror foils which would be 
        // undesirable for a telescope design. However, having the condition 
        // here ensures that the code does not crash if there is an erroneous 
        // entry in the TDF.
        
        genTelescope.m_holewalltopz = tdfExtObjects.m_zMax.at(iExtObj);
        
        // holesideinnerradius is otherwise = innerhousingradius by default if 
        // we don’t reach this if-block
        genTelescope.m_holesideinnerradius = tdfExtObjects.m_radMin.at(iExtObj);
        
        // No gap will be allowed between extobjects.rmax(i) and 
        // innerhousingradius; so effectively, "holewallouterradius" would be 
        // equal to innerhousingradius, so we will just use the latter
        
        genTelescope.m_numcentralextobjectparts++;
      
      } // end-if testing for top, bottom, central objects
      
    } // end for-loop through external structures
    
    
    AH_DEBUG << "tdfExtObjects.m_zMax.at(5) = " << tdfExtObjects.m_zMax.at(5)  << std::endl;
    AH_DEBUG << "telescopetopz = " << telescopetopz  << std::endl;
    AH_DEBUG << "tdfExtObjects.m_zMin.at(5) = " << tdfExtObjects.m_zMin.at(5) << std::endl;
    AH_DEBUG << "telescopebottomz = " << telescopebottomz  << std::endl;
    AH_DEBUG << "tdfExtObjects.m_radMax.at(5) = " << tdfExtObjects.m_radMax.at(5)  << std::endl;
    AH_DEBUG << "genTelescope.m_innerhousingradius = " << genTelescope.m_innerhousingradius  << std::endl;
    AH_DEBUG << "tdfExtObjects.m_zMax.at(5) = " << tdfExtObjects.m_zMax.at(5)  << std::endl;
    AH_DEBUG << "housings[PCOL].m_zLower = " << housings[PCOL].m_zLower  << std::endl;
    
    AH_DEBUG << std::endl;

    AH_DEBUG << "topExtObjects.m_numExtObjectParts = " << topExtObjects.m_numExtObjectParts << std::endl;
    AH_DEBUG << "bottomExtObjects.m_numExtObjectParts = " << bottomExtObjects.m_numExtObjectParts << std::endl;
    AH_DEBUG << "genTelescope.m_numcentralextobjectparts = " << genTelescope.m_numcentralextobjectparts << std::endl;
    AH_DEBUG << "extobjectstopzcoord = " << topExtObjects.m_zCoord  << std::endl;
    AH_DEBUG << "extobjectsbottomzcoord = " << bottomExtObjects.m_zCoord << std::endl;
  } // end-if m_doExternalObjects
  
  // If number of objects is zero, turn off doexternalobjects
  if (topExtObjects.m_numExtObjectParts == 0) {
    param.m_doTopExternalObjects = false;
    topExtObjects.m_doExternalObjects = false;
  } else {
    param.m_doTopExternalObjects = true;
    topExtObjects.m_doExternalObjects = true;
  }
  if (bottomExtObjects.m_numExtObjectParts == 0) {
    param.m_doBottomExternalObjects = false;
    bottomExtObjects.m_doExternalObjects = false;
  } else {
    param.m_doBottomExternalObjects = true;
    bottomExtObjects.m_doExternalObjects = true;
  }
  if ( (topExtObjects.m_numExtObjectParts == 0) && 
       (bottomExtObjects.m_numExtObjectParts == 0) ) {
    param.m_doExternalObjects = false;
  }
  
  // Create a look-up table that returns an XRT object ID given a shell 
  // number and a sector ID (shell number and sector ID are not defined for 
  // obstructions so this is only relevant for the foils). The array 
  // sectorshell2objectid(#sectors,#shells) will give the XRT object ID for a 
  // given sector and shell number (which corresponds to a unique mirror or 
  // pre-collimator foil). This will be used to locate adjacent foils to a 
  // given foil.
  // (Both of the above assume consecutive numbering of shells and sectors, starting from 1)
  std::vector< std::vector<long> > sectorshell2objectid(sectorsShells.m_numSectors);
  for (int iSector = 0 ; iSector < sectorsShells.m_numSectors ; ++iSector) {
    sectorshell2objectid[iSector].resize(sectorsShells.m_numShells);
  }
  
  // For fastmode we need a different way of generating an XRT object id from shell number and sector: we need a sector number that is unique for each foil set (p-col, primary, secondary), not unique per segment. So we need to generate an object id given the set#, sector# per set, and shell#
  sectorsShells.m_numApertureSectors = sectorsShells.m_sectorsPerSegment * sectorsShells.m_numSegments;
  sectorsShells.m_setsectorshell2xrtobjectid.resize(s_numHousings);
  for (int iSet = 0 ; iSet < s_numHousings ; ++iSet) {
    sectorsShells.m_setsectorshell2xrtobjectid.at(iSet).resize(sectorsShells.m_numApertureSectors);
    for (int iSector = 0 ; iSector < sectorsShells.m_numApertureSectors ; ++iSector) {
      sectorsShells.m_setsectorshell2xrtobjectid.at(iSet).at(iSector).resize(sectorsShells.m_numShells);
    }
  }
  
  AH_DEBUG << "sectorsShells.m_numApertureSectors = " << sectorsShells.m_numApertureSectors << std::endl;
  AH_DEBUG << "sectorsShells.m_sectorsPerSegment = " << sectorsShells.m_sectorsPerSegment << std::endl;
  AH_DEBUG << "sectorsShells.m_numSegments = " << sectorsShells.m_numSegments << std::endl;
  AH_DEBUG << "sectorsShells.m_numSectors = " << sectorsShells.m_numSectors << std::endl;
  AH_DEBUG << "sectorsShells.m_numShells = " << sectorsShells.m_numShells << std::endl;

  // +++ maybe iSector isn't a good var name here
  long iSector = 0;
  // Loop over all XRT objects. In general we can’t assume we know where the 
  // obstructions are so use xrtobjectType to query what the object is. 
  // Currently foils are the only other type of object but if there is another 
  // type of object in a future telescope this will have to be modified.
  for (long iObj = 0 ; iObj < numXRTObjects ; ++iObj) {
    if (XRTObjects[iObj].m_type == FOIL) {
//      AH_DEBUG << "iObj = " << iObj << std::endl;
//      AH_DEBUG << "XRTObjects[iObj].m_sectorID = " << XRTObjects[iObj].m_sectorID << std::endl;
//      AH_DEBUG << "XRTObjects[iObj].m_sectorNumber = " << XRTObjects[iObj].m_sectorNumber << std::endl;
//      AH_DEBUG << "XRTObjects[iObj].m_shellNumber = " << XRTObjects[iObj].m_shellNumber << std::endl;
//      AH_DEBUG << "XRTObjects[iObj].m_set = " << XRTObjects[iObj].m_set << std::endl;
//      AH_DEBUG << "XRTObjects[iObj].m_segmentID = " << XRTObjects[iObj].m_segmentID << std::endl;
      
      sectorshell2objectid[XRTObjects[iObj].m_sectorID-1][XRTObjects[iObj].m_shellNumber-1] = iObj;
      iSector = ((XRTObjects[iObj].m_segmentID - 1) * sectorsShells.m_sectorsPerSegment) + XRTObjects[iObj].m_sectorNumber - 1;
//      AH_DEBUG << "iSector = " << iSector << std::endl;
      sectorsShells.m_setsectorshell2xrtobjectid.at(XRTObjects[iObj].m_set).at(iSector).at(XRTObjects[iObj].m_shellNumber-1) = iObj;
    }
  }
  
  // Set up a lower and upper radial bound for each XRT object of type foil 
  // corresponding to the adjacent shell smaller and larger in radius 
  // respectively than the shell for the XRT object in question. The array 
  // xrtobjectradialbounds(numXRTObjects,0) will hold the outer radius of the 
  // adjacent foil closest to the telescope axis, and 
  // xrtobjectradialbounds(numXRTObjects,1) will hold the inner radius of the 
  // adjacent foil furthest from the telescope axis. These radial bounds are 
  // useful because any shifts and rotational misalignments of foils cannot 
  // take any part of the foil beyond adjacent shells of foils. The bounds 
  // will be used in conjunction with the Cartesian bounding boxes to quickly 
  // eliminate foils that cannot be intercepted by a given photon.
  
  // Loop over sectorshell2objectid to fill in the xrtobjectradialbounds(*,2);
  for (int iSector = 0 ; iSector < sectorsShells.m_numSectors ; ++iSector) {
    for (int iShell = 0 ; iShell < sectorsShells.m_numShells ; ++iShell) {
      if (iShell == 0) {
        // For 1st shell, lower bound = housing inner radius, upper bound = top outer radius of next shell
        XRTObjects[sectorshell2objectid[iSector][iShell]].m_lowerRadialBound = housings[PCOL].m_rInner;
        XRTObjects[sectorshell2objectid[iSector][iShell]].m_upperRadialBound = XRTObjects[sectorshell2objectid[iSector][iShell+1]].m_geoParams[10];
      } else if ( (iShell == (sectorsShells.m_numShells-1)) && (iShell > 0) ) {
        // For last shell, lower bound = bottom inner radius of previous shell, upper bound = housing outer radius
        XRTObjects[sectorshell2objectid[iSector][iShell]].m_lowerRadialBound = XRTObjects[sectorshell2objectid[iSector][iShell-1]].m_geoParams[3];
        XRTObjects[sectorshell2objectid[iSector][iShell]].m_upperRadialBound = housings[PCOL].m_rOuter;
      } else {
        // In general, lower bound = bottom inner radius of previous shell, upper bound = top outer radius of next shell
        XRTObjects[sectorshell2objectid[iSector][iShell]].m_lowerRadialBound = XRTObjects[sectorshell2objectid[iSector][iShell-1]].m_geoParams[3];
        XRTObjects[sectorshell2objectid[iSector][iShell]].m_upperRadialBound = XRTObjects[sectorshell2objectid[iSector][iShell+1]].m_geoParams[10];
      }
    }
  }
  
  // This section sets up angular and radial information needed for fast mode.
  // Given a photon's position (x,y) on the aperture we need to deduce which sector it is in and which “gap” between shells it is in (or if it is not in a gap).
  // A problem with deducing which sector it is in occurs if a sector encompasses the positive x- axis (i.e. angle=0). If we allow angles to > 360 degrees there is still a discontinuity in the angles for the next sector. The solution is to determine whether any sector does cross the x- axis, and if it does we split the sector artificially into two sectors, the boundary between the two being the x-axis. Then we determine which of the artificial sectors the point (x,y) is in and in turn we deduce the real sector the point is in.
  // Find the sector that has a start angle that is zero or the sector that encompasses 0 degrees between the start and end angle.
  // From the mirrorsectorangleboundaries array we can get the sector number for a given angle by 1+ [(angle lo bin) mod sectorsShells.m_numApertureSectors]
  sectorsShells.m_zerosectorid = -1;
  double losectorangle = 0.0;
  double hisectorangle = 0.0;
  double losectorangletwopidev = 0.0;
  const double tolerance = 1.e-6;
  int angleboundoffset = 0;     // This controls the number of "fake" sector boundaries 
  int angleboundindex = 0;
  for (int iSector = 0 ; iSector < sectorsShells.m_numApertureSectors ; ++iSector) {
    // in case there is no pre- collimator (but the boundaries should be the same in any case)
    losectorangle = std::fmod(XRTObjects[sectorsShells.m_setsectorshell2xrtobjectid[1][iSector][0]].m_startAngle, s_twopi);
    hisectorangle = std::fmod(XRTObjects[sectorsShells.m_setsectorshell2xrtobjectid[1][iSector][0]].m_endAngle, s_twopi);
    losectorangletwopidev = std::abs(1.0 - (losectorangle/s_twopi));
    
    // There are only two possibilities: either a sector boundary is exactly coincident with the x-axis, or one of the sectors crosses the x-axis. In the former case we allow a precision error of 1.e-6
    if ( (losectorangle == 0.0) || (losectorangletwopidev < tolerance) ) {
      sectorsShells.m_zerosectorid = iSector;
      angleboundoffset = 0;
    } else if (losectorangle > hisectorangle) {
      sectorsShells.m_zerosectorid = iSector;
      angleboundoffset = 1;
    }

  }
  
  if (sectorsShells.m_zerosectorid < 0) {
    // This should never happen: code should stop if it ever does
    AH_THROW_RUNTIME("*** ERROR *** No sector found that brackets 0 degrees ***");
  }
  
  sectorsShells.m_nummirrorsectorangleboundaries = sectorsShells.m_numApertureSectors + 1 + angleboundoffset;
  sectorsShells.m_mirrorsectorangleboundaries.resize(sectorsShells.m_nummirrorsectorangleboundaries);
  sectorsShells.m_mirrorsectorangleboundaries.at(0) = 0.0;
  sectorsShells.m_mirrorsectorangleboundaries.at(sectorsShells.m_nummirrorsectorangleboundaries-1) = s_twopi;
  for (int iBoundary = 1 ; iBoundary <= sectorsShells.m_nummirrorsectorangleboundaries-2 ; ++iBoundary) {
    angleboundindex = (iBoundary + sectorsShells.m_zerosectorid) % sectorsShells.m_numApertureSectors;
    sectorsShells.m_mirrorsectorangleboundaries.at(iBoundary+angleboundoffset) = std::fmod(XRTObjects[sectorsShells.m_setsectorshell2xrtobjectid[1][angleboundindex][0]].m_startAngle, s_twopi);
  }
  
  // Now set up some radial boundaries needed for fastmode: these are in different format to the previously calculated radial bounds. In fastmode we will be dealing with "gap number" (space between shells) as well as shell number. Accounting for the inner and outer housing, the number of gaps is equal to the number of shells + 1, and the total number radial boundaries is equal to the number of shells + 2.
  
  // Set up radial boundaries that correspond to the the top inner radii of mirror and pre-collimator foils, with an additional boundary at each end corresponding to the inner and outer housing radii. This array will be used later to determine which pair of shells that the initial photon position is surrounded by.
  sectorsShells.m_numgapradialboundaries = sectorsShells.m_numShells + 2;
  sectorsShells.m_gapradialboundaries.resize(sectorsShells.m_numgapradialboundaries);
  sectorsShells.m_gapradialboundaries.at(0) = genTelescope.m_innerhousingradius;
  sectorsShells.m_gapradialboundaries.at(sectorsShells.m_numgapradialboundaries-1) = genTelescope.m_outerhousingradius;
  sectorsShells.m_numGaps = sectorsShells.m_numgapradialboundaries - 1;
  
  // Note that if the initial photon position is inside the inner housing radius (i.e. in the "hole"), it will be treated separately in fastmode because there are no XRT objects in the hole to interact with.
  // The radii of the pre-collimator shells are different to the primary foil shells so it makes a difference to the radial boundary values that are used, whether or not there is a pre- collimator.
  int gapset = 0;
  gapset = (param.m_pcolExists ? 0 : 1);
  // The radial boundary values are calculated for the 1st sector, for primary mirrors, but it should not matter which sector is used.
  for (int iBoundary = 1 ; iBoundary <= sectorsShells.m_numgapradialboundaries-2 ; ++iBoundary) {
    sectorsShells.m_gapradialboundaries.at(iBoundary) = XRTObjects[sectorsShells.m_setsectorshell2xrtobjectid[gapset][0][iBoundary-1]].m_geoParams[9];
  }
  
  
  /* testing for correct gap boundaries 
  #ifdef DEBUG
  #endif
  AH_DEBUG << "*** testing for correct gap boundaries ***" << std::endl;
  AH_DEBUG << "numgapradialboundaries = " << sectorsShells.m_numgapradialboundaries << std::endl;
  for (int i = 0 ; i < sectorsShells.m_numgapradialboundaries ; ++i) { 
    AH_DEBUG << "gapradialboundary "<<i<<" = " << sectorsShells.m_gapradialboundaries.at(i) << std::endl;
  }*/
  
  
  // +++ this should be inside setupXRTTransforms - no, he said it may be needed even if no transforms
  // Set up arrays that will hold coordinate transformations for each XRT object that are appropriate for a number of situations as described below. In each case, the transformations consist of 12 numbers that describe a net translational shift and a net rotation.
  // In the following, "XRT frame" refers to the frame of reference in which the results plane (by default, the focal plane) contains the x and y Cartesian coordinate axes, and the z-axis coincides with central axis of the cylindrical housing units and foil arrangements, when no translational or rotational offsets are applied. The "object" frame refers to a frame of reference in which a given XRT object is not shifted or rotated. Therefore, each XRT object is associated with a unique transformation that transforms the position and direction of a photon from the XRT frame to the object frame. A further transformation can be applied such that the object will appear to be in a "standard position" from the point of view of the photon (for example the cone that gives rise to a conical foil surface could be placed with its apex at the origin of coordinates, and its axis coinciding with the z-axis).
  // The intercept points for a ray interecting an XRT object are found by transforming the photon into the object frame first. The interaction (e.g. reflection, transmission, and scattering) is treated in the object frame, and the resulting photon is transformed back into the XRT frame. These transformations considerably reduce the burden on run-time of the main ray-tracing loop because they significantly simplify the math, so the transformations are critical for keeping the run-time low.
  //  xrt frame to object frame in TDF (default) position
  transforms.m_xrt2objectframe.resize(numXRTObjects);
  //  object frame in TDF (default) position to xrt frame
  transforms.m_object2xrtframe.resize(numXRTObjects);
  //  xrt frame to sector sidewall frame
  transforms.m_xrt2sectorframe.resize(numXRTObjects);
  //  sector sidewall frame to xrt frame
  transforms.m_sector2xrtframe.resize(numXRTObjects);
  for (int iObj = 0 ; iObj < numXRTObjects ; ++iObj) {
    transforms.m_xrt2objectframe[iObj].resize(transforms.m_maxTransformXYZ);
    transforms.m_object2xrtframe[iObj].resize(transforms.m_maxTransformXYZ);
    transforms.m_xrt2sectorframe[iObj].resize(transforms.m_maxTransformXYZ);
    transforms.m_sector2xrtframe[iObj].resize(transforms.m_maxTransformXYZ);
  }
  
  // Call the subroutine that generates the above transformation arrays.
  // But first check if ALL the transforms are zero (no rotations, no shifts), 
  // in which case set doTransforms to false even if it was true before
  totalTransformValue = seg.m_sumofsegmentoffsets + sumHousingShifts;
  
  // pcol mean tilt, mirror mean twist, etc are used to generate the random 
  // components of these offsets but now we need to add in all the systematic 
  // tilts and twists.
  for (int iRow = 0 ; iRow < mirror.m_numRows ; ++iRow) {
    totalTransformValue += mirror.m_sysTilt[iRow] + 
                           mirror.m_sysTwist[iRow] + 
                           mirror.m_sectorShift[iRow];
  }
  if (param.m_pcolExists) {
    for (int iRow = 0 ; iRow < pcol.m_numRows ; ++iRow) {
      totalTransformValue += pcol.m_sysTilt[iRow] + 
                            pcol.m_sysTwist[iRow] + 
                            pcol.m_sectorShift[iRow];
    }
  }
  if (totalTransformValue == 0.0) {
    transforms.m_doTransforms = false;
  }
  
  if (transforms.m_doTransforms) {
    
    AH_OUT << "Setting up misalignment transformations" << std::endl;
    
    segmentRotationZOffset = genTelescope.m_focalLength;
    
    // Call the subroutine that generates the above transformation arrays, held 
    // in the transforms struct
    setupXRTTransform(XRTObjects, numXRTObjects, segmentRotationZOffset, misalignment_struct, transforms);
    
    // convert the bounding boxes so that they enclose the XRT objects after 
    // all shifts and rotations
    transformBBoxes(numXRTObjects, transforms.m_maxTransformXYZ, transforms.m_xrt2objectframe, XRTObjects);
    
  } else {
    
    // leave all transformation arrays (e.g. xrt2objectframe, etc.) with their 
    // default zero values for all elements
    
    // Assign the output bounding box array to be equal to the one that was 
    // calculated without transformations:
    for (long iObj = 0 ; iObj < numXRTObjects ; ++iObj) {
      XRTObjects[iObj].m_BBoxTrnsfrmd = XRTObjects[iObj].m_BBox;
    }
    
  } // end if-block doTransforms

} // end xrtSetup()


/******************************************************************************/


void setupTDFMirror(Param & param, 
                    TDF_Foil & mirror, 
                    std::vector<HousingGeometry> & housings, 
                    Misalignment & misalignment_struct,
                    std::vector<int> & frontReflIndex, 
                    std::vector<std::string> & uniqueFrontNames, 
                    int numUniqueFrontNames, 
                    std::vector<std::string> & uniqueRoughNames, 
                    int numUniqueRoughNames, 
                    std::vector<int> & backMirrorReflIndex, 
                    long & numGroups, 
                    GenTelescope & genTelescope) {
  
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * tdf_mirror_fp;             // telescope description file - pointer
  int status = 0;                       // for cfitsio function calls
  
  // for getting string keywords TELESCOP, INSTRUME, etc
  char * telescop_c;
  telescop_c    = (char *) malloc(80*sizeof(char));
  char * instrume_c;
  instrume_c    = (char *) malloc(80*sizeof(char));
  char * detnam_c;
  detnam_c      = (char *) malloc(80*sizeof(char));
  char * focalLength_c;
  focalLength_c = (char *) malloc(80*sizeof(char));
  
  std::string focalLengthUnit;           // units of focal length should be mm
  
  // for storing the TSTRING types from the input fits file 
  //+++ get the value 20 from the fits file
  char * input_scatter;      // 20 characters are allowed
  input_scatter = (char *) malloc(20*sizeof(char));
  char * input_freflect;      // 20 characters are allowed
  input_freflect = (char *) malloc(20*sizeof(char));
  char * input_breflect;        // 20 characters are allowed
  input_breflect = (char *) malloc(20*sizeof(char));
  
  // for storing the TLOGICAL types from the input fits file 
  char input_scross;
  char input_ecross;
  
  std::map<std::string, int> columns;   // associate column names and numbers
  int columnNum = 0;                    // temp storage of column number
  
  int firstIndex = 0;                 // for call to indexUniqueNames()
  
  // -------------------------------------

  // open the file to the specified extension
  fits_open_file(&tdf_mirror_fp, (param.m_mirrorfile).c_str(), READONLY, &status);
  checkForFITSError(status, "opening", param.m_mirrorfile);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_mirrorfile << std::endl;

  // Read some keywords that define the housing geometry and offsets from the 
  // header and put them in the array.
  // This is a three element vector of HousingGeometry structs.  
  //      housings[PCOL] = precollimator 
  //      housings[PRIMARY] = primary mirrors 
  //      housings[SECONDARY] = secondary mirrors
  // even if there is no precollimator, we still require a three element vector
  housings.resize(s_numHousings);
  
  // get the telescope and instrument name, so we can write to the output files
  fits_read_key_str(tdf_mirror_fp, "TELESCOP", telescop_c, NULL, &status);
  checkForFITSError(status, "getting TELESCOP keyword in", param.m_mirrorfile);
  fits_read_key_str(tdf_mirror_fp, "INSTRUME", instrume_c, NULL, &status);
  checkForFITSError(status, "getting INSTRUME keyword in", param.m_mirrorfile);
  fits_read_key_str(tdf_mirror_fp, "DETNAM", detnam_c, NULL, &status);
  checkForFITSError(status, "getting DETNAM keyword in", param.m_mirrorfile);
  
  fits_read_key_dbl(tdf_mirror_fp, "FOCALLEN", &(genTelescope.m_focalLength), NULL, &status);
  checkForFITSError(status, "reading keyword FOCALLEN in", param.m_mirrorfile);
  
  fits_read_key_dbl(tdf_mirror_fp, "TELFPROT", &(genTelescope.m_telfprot), NULL, &status);

  // Check if TELFPROT exists if not set telfprot to 0
  if (status) {
    if (status == KEY_NO_EXIST) {
      AH_OUT << "***NOTE: TELFPROT keyword not found in MIRROR extension of TDF (assuming TELFPROT = 0).***" << std::endl;
      AH_OUT << "***Coordinate system may be rotated with respect to detector coordinate system.***" << std::endl;
      genTelescope.m_telfprot = 0;
      status = 0;
    } else {  // Only triggered if keyword is found but there was a error reading it
      checkForFITSError(status, "reading keyword TELFPROT in", param.m_mirrorfile);
    }
  }

  // Convert telfprot into radians, store in separate variable
  // precompute sin and cosine of telfprot
  genTelescope.m_telfprotRadians = s_degreesToRadian * genTelescope.m_telfprot;
  genTelescope.m_costelfprot = cos(genTelescope.m_telfprotRadians);
  genTelescope.m_sintelfprot = sin(genTelescope.m_telfprotRadians);

  AH_DEBUG << "telfprotRadians = " << genTelescope.m_telfprotRadians << std::endl;
  AH_DEBUG << "costelfprot = " << genTelescope.m_costelfprot << std::endl;
  AH_DEBUG << "sintelfprot = " << genTelescope.m_sintelfprot << std::endl;
 

  // assumes units are in square brackets at beginning of keyword comment field
  //  fits_read_key_unit(tdf_mirror_fp, "FOCALLEN", focalLength_c, &status);
  
  genTelescope.m_telescop = (std::string)telescop_c;
  genTelescope.m_instrume = (std::string)instrume_c;
  genTelescope.m_detnam   = (std::string)detnam_c;
  focalLengthUnit         = (std::string)focalLength_c;
  
  // free the char * we used to get the string keywords from cfitsio
  free(telescop_c);     telescop_c=0;
  free(instrume_c);     instrume_c=0;
  free(detnam_c);       detnam_c=0;
  free(focalLength_c);  focalLength_c=0;
  
  // error checking that TELESCOP and INSTRUME (user input params) match this 
  // file, in case the user supplied the file instead of CALDB
  if (!isEqualCaseInsens(param.m_telescop, genTelescope.m_telescop)) {
    AH_THROW_RUNTIME("TELESCOP keyword in mirror file (" + 
                     genTelescope.m_telescop+ ", in file " + param.m_mirrorfile + 
                     ") and input parameter 'telescop' (" + param.m_telescop + ") must match");
  }
  if (!isEqualCaseInsens(param.m_instrume, genTelescope.m_instrume)) {
    std::string errMsg = "INSTRUME keyword in mirror file (" + 
                         genTelescope.m_instrume + ", in file " + param.m_mirrorfile + 
                         ") and input parameter 'instrume' (" + param.m_instrume + ") do not match";
    AH_INFO(ahlog::HIGH) << errMsg << std::endl;
    // +++ 20150909 KLR warning, not error, so that tests with old files will work.
//    AH_THROW_RUNTIME("INSTRUME keyword in mirror file (" + 
//                     genTelescope.m_instrume + ", in file " + param.m_mirrorfile + 
//                     ") and input parameter 'instrume' (" + param.m_instrume + ") must match");
  }
  
  
  // +++ 20150401 KLR per conversation w TY, don't check for units here becuase 
  //                  it will cause failures when using old TDFs, where the 
  //                  units are not formatted this way
//  // make sure the focal length is in mm
//  if (!isEqualCaseInsens(focalLengthUnit,"mm")) {
//    AH_THROW_RUNTIME("Focal Length (keyword FOCALLEN) must be in mm.  The unit should be enclosed in square brackets in the beginning of the keyword comment field.");
//  }
  
  // precollimator keywords
  // +++ what if there is no precol?  need to fill with zeros?
  fits_read_key_dbl(tdf_mirror_fp, "PMINRAD", &(housings[PCOL].m_rInner), NULL, &status);
  checkForFITSError(status, "reading keyword PMINRAD in", param.m_mirrorfile);
  fits_read_key_dbl(tdf_mirror_fp, "PMAXRAD", &(housings[PCOL].m_rOuter), NULL, &status);
  checkForFITSError(status, "reading keyword PMAXRAD in", param.m_mirrorfile);
  
  // primary mirror
  housings[PRIMARY].m_rInner = housings[PCOL].m_rInner;
  housings[PRIMARY].m_rOuter = housings[PCOL].m_rOuter;
  fits_read_key_dbl(tdf_mirror_fp, "PRIMZMIN", &(housings[PRIMARY].m_zLower), NULL, &status);
  checkForFITSError(status, "reading keyword PRIMZMIN in", param.m_mirrorfile);
  fits_read_key_dbl(tdf_mirror_fp, "PRIMZMAX", &(housings[PRIMARY].m_zUpper), NULL, &status);
  checkForFITSError(status, "reading keyword PRIMZMAX in", param.m_mirrorfile);
  fits_read_key_dbl(tdf_mirror_fp, "HXPRSHFT", &(housings[PRIMARY].m_xShift), NULL, &status);
  checkForFITSError(status, "reading keyword HXPRSHFT in", param.m_mirrorfile);
  fits_read_key_dbl(tdf_mirror_fp, "HYPRSHFT", &(housings[PRIMARY].m_yShift), NULL, &status);
  checkForFITSError(status, "reading keyword HYPRSHFT in", param.m_mirrorfile);
  
  // secondary mirror
  housings[SECONDARY].m_rInner = housings[PCOL].m_rInner;
  housings[SECONDARY].m_rOuter = housings[PCOL].m_rOuter;
  fits_read_key_dbl(tdf_mirror_fp, "SECDZMIN", &(housings[SECONDARY].m_zLower), NULL, &status);
  checkForFITSError(status, "reading keyword SECDZMIN in", param.m_mirrorfile);
  fits_read_key_dbl(tdf_mirror_fp, "SECDZMAX", &(housings[SECONDARY].m_zUpper), NULL, &status);
  checkForFITSError(status, "reading keyword SECDZMAX in", param.m_mirrorfile);
  fits_read_key_dbl(tdf_mirror_fp, "HXSRSHFT", &(housings[SECONDARY].m_xShift), NULL, &status);
  checkForFITSError(status, "reading keyword HXSRSHFT in", param.m_mirrorfile);
  fits_read_key_dbl(tdf_mirror_fp, "HYSRSHFT", &(housings[SECONDARY].m_yShift), NULL, &status);
  checkForFITSError(status, "reading keyword HYSRSHFT in", param.m_mirrorfile);
  
  
  // There is too much freedom with 3x2 housing shifts because one pair is 
  // always arbitrary and creates unnecessary complexity and run-time burden on 
  // the code. Redefine the shifts so that the 1st layer/set shifts are always 
  // zero and the others are relative to this.
  if (param.m_pcolExists) {
    
    // Subtract pcol shifts from primary and secondary shifts
    // set pcol shifts to 0.0 
    
    // HXPRSHFT = HXPRSHFT-HXPCSHFT
    // HYPRSHFT = HYPRSHFT-HYPCSHFT
    housings[PRIMARY].m_xShift = housings[PRIMARY].m_xShift - housings[PCOL].m_xShift;
    housings[PRIMARY].m_yShift = housings[PRIMARY].m_yShift - housings[PCOL].m_yShift;
    
    // HXSRSHFT = HXSRSHFT-HXPCSHFT
    // HYSRSHFT = HYSRSHFT-HYPCSHFT
    housings[SECONDARY].m_xShift = housings[SECONDARY].m_xShift - housings[PCOL].m_xShift;
    housings[SECONDARY].m_yShift = housings[SECONDARY].m_yShift - housings[PCOL].m_yShift;
    
    // HXPCSHFT=0.0, HYPCSHFT=0.0
    housings[PCOL].m_xShift = 0.0;
    housings[PCOL].m_yShift = 0.0;
    
  } else {
    
    // Subtract primary shifts from secondary shifts
    // set primary shifts to 0.0
    
    // HXSRSHFT = HXSRSHFT-HXPCSHFT
    // HYSRSHFT = HYSRSHFT-HYPCSHFT
    housings[SECONDARY].m_xShift = housings[SECONDARY].m_xShift - housings[PRIMARY].m_xShift;
    housings[SECONDARY].m_yShift = housings[SECONDARY].m_yShift - housings[PRIMARY].m_yShift;
    
    // HXPRSHFT=0.0, HYPRSHFT=0.0
    housings[PRIMARY].m_xShift = 0.0;
    housings[PRIMARY].m_yShift = 0.0;
    
  }
  
  // Read the value of the keywords for the TILT and TWIST pivot axes for the 
  // primary and secondary mirror foils. The indices will be directly related 
  // to the values of xrtobjectset() so should not be altered. (Note: the 
  // pre-collimator will be done when reading the COLLIMATOR extension and put 
  // in systiltpivot(0)).
  fits_read_key_dbl(tdf_mirror_fp, "TLTPVPRI", &(misalignment_struct.m_tiltPivot[PRIMARY]), NULL, &status);
  checkForFITSError(status, "reading keyword TLTPVPRI in", param.m_mirrorfile);
  fits_read_key_dbl(tdf_mirror_fp, "TLTPVSEC", &(misalignment_struct.m_tiltPivot[SECONDARY]), NULL, &status);
  checkForFITSError(status, "reading keyword TLTPVSEC in", param.m_mirrorfile);
  
  fits_get_num_rows(tdf_mirror_fp, &(mirror.m_numRows), &status);
  checkForFITSError(status, "getting number of rows in", param.m_mirrorfile);
  
  // resize all vectors to store data
  mirror.m_set.resize(mirror.m_numRows);
  mirror.m_segment.resize(mirror.m_numRows);
  mirror.m_number.resize(mirror.m_numRows);
  mirror.m_sectorNumber.resize(mirror.m_numRows);
  mirror.m_sectorShift.resize(mirror.m_numRows);
  mirror.m_sysTilt.resize(mirror.m_numRows);
  mirror.m_sysTwist.resize(mirror.m_numRows);
  mirror.m_function.resize(mirror.m_numRows);
  mirror.m_scatter.resize(mirror.m_numRows);
  mirror.m_fRelfect.resize(mirror.m_numRows);
  mirror.m_bRelfect.resize(mirror.m_numRows);
  mirror.m_startAngle.resize(mirror.m_numRows);
  mirror.m_endAngle.resize(mirror.m_numRows);
  mirror.m_topInnerRadius.resize(mirror.m_numRows);
  mirror.m_topOuterRadius.resize(mirror.m_numRows);
  mirror.m_botInnerRadius.resize(mirror.m_numRows);
  mirror.m_botOuterRadius.resize(mirror.m_numRows);
  mirror.m_topZ.resize(mirror.m_numRows);
  mirror.m_botZ.resize(mirror.m_numRows);
  mirror.m_startCross.resize(mirror.m_numRows);
  mirror.m_endCross.resize(mirror.m_numRows);
  
  // get column numbers
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("LAYER"), &columnNum, &status);
  columns["LAYER"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("ASSEMBLY"), &columnNum, &status);
  columns["ASSEMBLY"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("NUMBER"), &columnNum, &status);
  columns["NUMBER"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("SECTORNUMBER"), &columnNum, &status);
  columns["SECTORNUMBER"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("SECTORSHIFT"), &columnNum, &status);
  columns["SECTORSHIFT"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("SYSTILT"), &columnNum, &status);
  columns["SYSTILT"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("SYSTWIST"), &columnNum, &status);
  columns["SYSTWIST"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("FUNCTION"), &columnNum, &status);
  columns["FUNCTION"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("SCATTER"), &columnNum, &status);
  columns["SCATTER"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("FREFLECT"), &columnNum, &status);
  columns["FREFLECT"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("BREFLECT"), &columnNum, &status);
  columns["BREFLECT"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("FSTART"), &columnNum, &status);
  columns["FSTART"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("FEND"), &columnNum, &status);
  columns["FEND"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("TOPINR"), &columnNum, &status);
  columns["TOPINR"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("TOPOUTR"), &columnNum, &status);
  columns["TOPOUTR"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("BOTINR"), &columnNum, &status);
  columns["BOTINR"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("BOTOUTR"), &columnNum, &status);
  columns["BOTOUTR"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("TOPD"), &columnNum, &status);
  columns["TOPD"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("BOTD"), &columnNum, &status);
  columns["BOTD"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("SCROSS"), &columnNum, &status);
  columns["SCROSS"] = columnNum;
  fits_get_colnum(tdf_mirror_fp, CASEINSEN, const_cast<char *>("ECROSS"), &columnNum, &status);
  columns["ECROSS"] = columnNum;
  checkForFITSError(status, "getting column numbers in", param.m_mirrorfile);
  
  // get data from file, store it in vectors
  for (long i = 0 ; i < mirror.m_numRows ; ++i) {
    fits_read_col(tdf_mirror_fp, TLONG,   columns["LAYER"], i+1, 1, 1, 0, &(mirror.m_set[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TLONG,   columns["ASSEMBLY"], i+1, 1, 1, 0, &(mirror.m_segment[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TLONG,   columns["NUMBER"], i+1, 1, 1, 0, &(mirror.m_number[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TLONG,   columns["SECTORNUMBER"], i+1, 1, 1, 0, &(mirror.m_sectorNumber[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["SECTORSHIFT"], i+1, 1, 1, 0, &(mirror.m_sectorShift[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["SYSTILT"], i+1, 1, 1, 0, &(mirror.m_sysTilt[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["SYSTWIST"], i+1, 1, 1, 0, &(mirror.m_sysTwist[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TLONG,   columns["FUNCTION"], i+1, 1, 1, 0, &(mirror.m_function[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TSTRING, columns["SCATTER"], i+1, 1, 1, 0, &input_scatter, 0, &status);
    fits_read_col(tdf_mirror_fp, TSTRING, columns["FREFLECT"], i+1, 1, 1, 0, &input_freflect, 0, &status);
    fits_read_col(tdf_mirror_fp, TSTRING, columns["BREFLECT"], i+1, 1, 1, 0, &input_breflect, 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["FSTART"], i+1, 1, 1, 0, &(mirror.m_startAngle[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["FEND"], i+1, 1, 1, 0, &(mirror.m_endAngle[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["TOPINR"], i+1, 1, 1, 0, &(mirror.m_topInnerRadius[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["TOPOUTR"], i+1, 1, 1, 0, &(mirror.m_topOuterRadius[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["BOTINR"], i+1, 1, 1, 0, &(mirror.m_botInnerRadius[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["BOTOUTR"], i+1, 1, 1, 0, &(mirror.m_botOuterRadius[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["TOPD"], i+1, 1, 1, 0, &(mirror.m_topZ[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TDOUBLE, columns["BOTD"], i+1, 1, 1, 0, &(mirror.m_botZ[i]), 0, &status);
    fits_read_col(tdf_mirror_fp, TLOGICAL, columns["SCROSS"], i+1, 1, 1, 0, &input_scross, 0, &status);
    fits_read_col(tdf_mirror_fp, TLOGICAL, columns["ECROSS"], i+1, 1, 1, 0, &input_ecross, 0, &status);
    
    // convert the char * from cfitsio into C++ strings, store in mirror struct
    mirror.m_scatter[i]  = (std::string)input_scatter;
    mirror.m_fRelfect[i] = (std::string)input_freflect;
    mirror.m_bRelfect[i] = (std::string)input_breflect;
    mirror.m_startCross[i] = ( input_scross == 0 ? false : true );
    mirror.m_endCross[i]   = ( input_ecross == 0 ? false : true );
  }
  
  // free the char * we used to get the TSTRING data from cfitsio
  free(input_scatter);  input_scatter=0;
  free(input_freflect); input_freflect=0;
  free(input_breflect); input_breflect=0;
  
  // check fits error after we free the char *
  checkForFITSError(status, "getting data from", param.m_mirrorfile);
  
  // now briefly go to the SURFACE extension, just to read the NGROUPS keyword
  fits_movnam_hdu(tdf_mirror_fp, ANY_HDU, const_cast<char *>(s_tdfSurfExtName.c_str()), 0, &status);
  checkForFITSError(status, "moving to SURFACE extension in", param.m_mirrorfile);
  fits_read_key_lng(tdf_mirror_fp, const_cast<char *>("NGROUPS"), &numGroups, NULL, &status);
  checkForFITSError(status, "reading NGROUPS keyword in", param.m_mirrorfile);
  
  // close tdf file (mirror ext)
  fits_close_file(tdf_mirror_fp, &status);
  checkForFITSError(status, "closing", param.m_mirrorfile);
  
  // max segment id number of any segment 
  // (e.g. will be 4 for Hitomi SXT and 3 for Hitomi HXT)
  mirror.m_maxSegment = getMaxLong(mirror.m_segment);
  // used for resizing xrtRegionToObjectID
  mirror.m_sectorsPerSegment = getMaxLong(mirror.m_sectorNumber);
  mirror.m_numberofshells = getMaxLong(mirror.m_number);
  
  // pre-calc trig quantities in the interest of run-time speed for ray-tracing
  mirror.m_tanStartAngle.resize(mirror.m_numRows);
  mirror.m_tanEndAngle.resize(mirror.m_numRows);
  for (long i = 0 ; i < mirror.m_numRows ; ++i) {
    mirror.m_tanStartAngle[i] = std::tan(mirror.m_startAngle[i]);
    mirror.m_tanEndAngle[i] = std::tan(mirror.m_endAngle[i]);
  }
  
  
  // Create reflectivity index for front-side (coated surface) of mirror foils
  // The elements of the array mirrorfreflect correspond to the column names 
  // for reflection probability in the reflectivity file for the front-side 
  // surfaces of mirror foils (the columns correspond to different groups of 
  // mirror foils). Organize the names into a smaller array of unique names 
  // and create an index that maps elements of mirrorfreflect onto the smaller 
  // array of uniquenames. Only the index is required for output, simply 
  // relying on the fact that the order of the columns in the reflectivity 
  // file is in the order of the first appearance of each column in the array 
  // mirrorfreflect.
  firstIndex = 0;
  indexUniqueNames(mirror.m_fRelfect, firstIndex, numUniqueFrontNames, uniqueFrontNames, frontReflIndex);
  
  
  // Create reflectivity index for rough surface XRT components
  // All other surfaces (back of mirrors, pcol) are referred to as rough 
  // surfaces and the columns in the TDF FREFLECT and BREFLECT for rough 
  // surfaces hold names of extensions in the reflectivity file where the 
  // reflection probability data can be found. We will index all the rough 
  // surfaces in a unified way so that we end up with a mapping of each mirror 
  // or pre-collimator surface to a set of integers that correspond to the 
  // correct extension in the reflectivity file (aside from an offset). This 
  // relies on the fact that the order of the extensions in the reflectivity 
  // file is always: back of mirrors, front of pre-collimators, back of 
  // pre-collimators (if the latter is missing back=front).
  firstIndex = 0;
  indexUniqueNames(mirror.m_bRelfect, firstIndex, numUniqueRoughNames, uniqueRoughNames, backMirrorReflIndex);
  
  
} // end setupTDFMirror()


/******************************************************************************/


void setupTDFPrecol(Param & param, 
                    TDF_Foil & pcol, 
                    std::vector<HousingGeometry> & housings, 
                    Misalignment & misalignment_struct, 
                    std::vector<int> & pcolFrontReflIndex, 
                    std::vector<int> & pcolBackReflIndex, 
                    std::vector<std::string> & uniqueRoughNames, 
                    int numUniqueRoughNames) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
    
  fitsfile * tdf_pcol_fp;           // telescope description file - pointer
  int status = 0;                   // for cfitsio function calls
  char tdfFilename[FLEN_FILENAME];               // get the name of the TDF
  std::string pcolExtName = "[" + s_tdfPrecolExtName + "]";
  std::string pcolFilename;         // TDF filename plus pcol extension
  
  // for storing the TSTRING types from the input fits file 
  //+++ get the value 20 from the fits file
  char * input_scatter;      // 20 characters are allowed
  input_scatter = (char *) malloc(20*sizeof(char));
  char * input_freflect;      // 20 characters are allowed
  input_freflect = (char *) malloc(20*sizeof(char));
  char * input_breflect;        // 20 characters are allowed
  input_breflect = (char *) malloc(20*sizeof(char));
  
  // for storing the TLOGICAL types from the input fits file 
  char input_scross;
  char input_ecross;
  
  std::map<std::string, int> columns;   // associate column names and numbers
  int columnNum = 0;                    // temp storage of column number
  
  int firstIndex = 0;                // for call to indexUniqueNames()
  
  // -------------------------------------
  
  // extract the filename of the TDF
  fits_parse_input_filename(const_cast<char *>(param.m_mirrorfile.c_str()), 0, tdfFilename, 0, 0, 0, 0, 0, 0, &status);
  checkForFITSError(status, "getting filename of", (std::string)tdfFilename);
  pcolFilename = ((std::string)tdfFilename).append(pcolExtName);
  
  // open the file to the first MIRROR extension
  fits_open_file(&tdf_pcol_fp, param.m_mirrorfile.c_str(), READONLY, &status);
  checkForFITSError(status, "opening MIRROR file to find COLLIMATOR extension in", (std::string)tdfFilename);
  
  // now try to move to the COLLIMATOR extension
  fits_movnam_hdu(tdf_pcol_fp, BINARY_TBL, const_cast<char *>(s_tdfPrecolExtName.c_str()), 0, &status);
  if (status == BAD_HDU_NUM) {
    //+++ is this correct with fits_open_file?
    param.m_pcolExists = false;
    pcol.m_numRows = 0;     // +++ 20140430 why bother with this?
    return;
  }
  // see if there were any other errors
  checkForFITSError(status, "opening COLLIMATOR extension in", (std::string)tdfFilename);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << pcolFilename << std::endl;
  
  // if we got here, the COLLIMATOR extension exists
  param.m_pcolExists = true;
  
  // precollimator keywords
  // +++ what if there is no precol?  need to fill with zeros?
  // housings[PCOL].m_rInner and rOuter are from PMINRAD in MIRROR extension, in setupTDFMirror()
  fits_read_key_dbl(tdf_pcol_fp, "PCOLZMIN", &(housings[PCOL].m_zLower), NULL, &status);
  checkForFITSError(status, "reading keyword PCOLZMIN in", pcolFilename);
  fits_read_key_dbl(tdf_pcol_fp, "PCOLZMAX", &(housings[PCOL].m_zUpper), NULL, &status);
  checkForFITSError(status, "reading keyword PCOLZMAX in", pcolFilename);
  fits_read_key_dbl(tdf_pcol_fp, "HXPCSHFT", &(housings[PCOL].m_xShift), NULL, &status);
  checkForFITSError(status, "reading keyword HXPCSHFT in", pcolFilename);
  fits_read_key_dbl(tdf_pcol_fp, "HYPCSHFT", &(housings[PCOL].m_yShift), NULL, &status);
  checkForFITSError(status, "reading keyword HYPCSHFT in", pcolFilename);
  
  fits_read_key_dbl(tdf_pcol_fp, "TLTPVPCL", &(misalignment_struct.m_tiltPivot[PCOL]), NULL, &status);
  checkForFITSError(status, "reading keyword TLTPVPCL in", pcolFilename);
  
  fits_get_num_rows(tdf_pcol_fp, &(pcol.m_numRows), &status);
  checkForFITSError(status, "getting number of rows in", pcolFilename);
  
  // resize all vectors to store data
  pcol.m_set.resize(pcol.m_numRows);
  pcol.m_segment.resize(pcol.m_numRows);
  pcol.m_number.resize(pcol.m_numRows);
  pcol.m_sectorNumber.resize(pcol.m_numRows);
  pcol.m_sectorShift.resize(pcol.m_numRows);
  pcol.m_sysTilt.resize(pcol.m_numRows);
  pcol.m_sysTwist.resize(pcol.m_numRows);
  pcol.m_function.resize(pcol.m_numRows);
  pcol.m_scatter.resize(pcol.m_numRows);
  pcol.m_fRelfect.resize(pcol.m_numRows);
  pcol.m_bRelfect.resize(pcol.m_numRows);
  pcol.m_startAngle.resize(pcol.m_numRows);
  pcol.m_endAngle.resize(pcol.m_numRows);
  pcol.m_topInnerRadius.resize(pcol.m_numRows);
  pcol.m_topOuterRadius.resize(pcol.m_numRows);
  pcol.m_botInnerRadius.resize(pcol.m_numRows);
  pcol.m_botOuterRadius.resize(pcol.m_numRows);
  pcol.m_topZ.resize(pcol.m_numRows);
  pcol.m_botZ.resize(pcol.m_numRows);
  pcol.m_startCross.resize(pcol.m_numRows);
  pcol.m_endCross.resize(pcol.m_numRows);
  
  // get column numbers
  //+++ this is a duplicate of setupTDFMirror.  have a common function?
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("LAYER"), &columnNum, &status);
  columns["LAYER"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("ASSEMBLY"), &columnNum, &status);
  columns["ASSEMBLY"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("NUMBER"), &columnNum, &status);
  columns["NUMBER"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("SECTORNUMBER"), &columnNum, &status);
  columns["SECTORNUMBER"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("SECTORSHIFT"), &columnNum, &status);
  columns["SECTORSHIFT"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("SYSTILT"), &columnNum, &status);
  columns["SYSTILT"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("SYSTWIST"), &columnNum, &status);
  columns["SYSTWIST"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("FUNCTION"), &columnNum, &status);
  columns["FUNCTION"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("SCATTER"), &columnNum, &status);
  columns["SCATTER"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("FREFLECT"), &columnNum, &status);
  columns["FREFLECT"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("BREFLECT"), &columnNum, &status);
  columns["BREFLECT"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("FSTART"), &columnNum, &status);
  columns["FSTART"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("FEND"), &columnNum, &status);
  columns["FEND"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("TOPINR"), &columnNum, &status);
  columns["TOPINR"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("TOPOUTR"), &columnNum, &status);
  columns["TOPOUTR"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("BOTINR"), &columnNum, &status);
  columns["BOTINR"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("BOTOUTR"), &columnNum, &status);
  columns["BOTOUTR"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("TOPD"), &columnNum, &status);
  columns["TOPD"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("BOTD"), &columnNum, &status);
  columns["BOTD"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("SCROSS"), &columnNum, &status);
  columns["SCROSS"] = columnNum;
  fits_get_colnum(tdf_pcol_fp, CASEINSEN, const_cast<char *>("ECROSS"), &columnNum, &status);
  columns["ECROSS"] = columnNum;
  checkForFITSError(status, "getting column numbers in", pcolFilename);
  
  // get data from file, store it in vectors
  for (long i = 0 ; i < pcol.m_numRows ; ++i) {
    //+++ they're not all doubles
    fits_read_col(tdf_pcol_fp, TLONG, columns["LAYER"], i+1, 1, 1, 0, &(pcol.m_set[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TLONG, columns["ASSEMBLY"], i+1, 1, 1, 0, &(pcol.m_segment[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TLONG, columns["NUMBER"], i+1, 1, 1, 0, &(pcol.m_number[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TLONG, columns["FUNCTION"], i+1, 1, 1, 0, &(pcol.m_function[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TSTRING, columns["SCATTER"], i+1, 1, 1, 0, &input_scatter, 0, &status);
    fits_read_col(tdf_pcol_fp, TSTRING, columns["FREFLECT"], i+1, 1, 1, 0, &input_freflect, 0, &status);
    fits_read_col(tdf_pcol_fp, TSTRING, columns["BREFLECT"], i+1, 1, 1, 0, &input_breflect, 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["FSTART"], i+1, 1, 1, 0, &(pcol.m_startAngle[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["FEND"], i+1, 1, 1, 0, &(pcol.m_endAngle[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["TOPINR"], i+1, 1, 1, 0, &(pcol.m_topInnerRadius[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["TOPOUTR"], i+1, 1, 1, 0, &(pcol.m_topOuterRadius[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["BOTINR"], i+1, 1, 1, 0, &(pcol.m_botInnerRadius[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["BOTOUTR"], i+1, 1, 1, 0, &(pcol.m_botOuterRadius[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["TOPD"], i+1, 1, 1, 0, &(pcol.m_topZ[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["BOTD"], i+1, 1, 1, 0, &(pcol.m_botZ[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TLONG, columns["SECTORNUMBER"], i+1, 1, 1, 0, &(pcol.m_sectorNumber[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["SECTORSHIFT"], i+1, 1, 1, 0, &(pcol.m_sectorShift[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["SYSTILT"], i+1, 1, 1, 0, &(pcol.m_sysTilt[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TDOUBLE, columns["SYSTWIST"], i+1, 1, 1, 0, &(pcol.m_sysTwist[i]), 0, &status);
    fits_read_col(tdf_pcol_fp, TLOGICAL, columns["SCROSS"], i+1, 1, 1, 0, &input_scross, 0, &status);
    fits_read_col(tdf_pcol_fp, TLOGICAL, columns["ECROSS"], i+1, 1, 1, 0, &input_ecross, 0, &status);
    
    // convert the char * from cfitsio into C++ strings, store in pcol struct
    pcol.m_scatter[i]  = (std::string)input_scatter;
    pcol.m_fRelfect[i] = (std::string)input_freflect;
    pcol.m_bRelfect[i] = (std::string)input_breflect;
    pcol.m_startCross[i] = ( input_scross == 0 ? false : true );
    pcol.m_endCross[i]   = ( input_ecross == 0 ? false : true );
  }
  
  // free the char * we used to get the TSTRING and TLOGICAL data from cfitsio
  free(input_scatter);  input_scatter=0;
  free(input_freflect); input_freflect=0;
  free(input_breflect); input_breflect=0;
  
  // check fits error after we free the char *
  checkForFITSError(status, "getting data from", pcolFilename);

  // close tdf file (pcol ext)
  fits_close_file(tdf_pcol_fp, &status);
  checkForFITSError(status, "closing", pcolFilename);
  
  // max segment id number of any segment 
  // (e.g. will be 4 for Hitomi SXT and 3 for Hitomi HXT)
  pcol.m_maxSegment = getMaxLong(pcol.m_segment);
  
  // pre-calc trig quantities in the interest of run-time speed for ray-tracing
  pcol.m_tanStartAngle.resize(pcol.m_numRows);
  pcol.m_tanEndAngle.resize(pcol.m_numRows);
  for (long i = 0 ; i < pcol.m_numRows ; ++i) {
    pcol.m_tanStartAngle[i] = std::tan(pcol.m_startAngle[i]);
    pcol.m_tanEndAngle[i] = std::tan(pcol.m_endAngle[i]);
  }
  
  // Create index arrays for mapping each pre-collimator foil to the correct 
  // reflectivity arrays from the reflectivity file.  see notes in 
  // setupTDFMirror() for doing this for the mirror foils. The array 
  // uniqueroughnames is a unified one covering the back of mirror foils,
  // the front and back of the pre-collimator foils, and future rough-surface 
  // components. 
  // Although the front and back of pre-collimators will end up with the same 
  // index, we do it in a general way that sets up how to extend this in future
  
  // first do the front
  firstIndex = 0;
  pcolFrontReflIndex.resize(pcol.m_numRows);
  indexUniqueNames(pcol.m_fRelfect, firstIndex, numUniqueRoughNames, uniqueRoughNames, pcolFrontReflIndex);
  
  // now do the back
  pcolBackReflIndex.resize(pcol.m_numRows);
  indexUniqueNames(pcol.m_bRelfect, firstIndex, numUniqueRoughNames, uniqueRoughNames, pcolBackReflIndex);

  
  
} // end setupTDFPrecol()


/******************************************************************************/


void setupTDFObstruct(Param & param, TDF_Obstruct & tdfobstruct, 
                      std::vector<Obstruction> & obstructions, 
                      bool & has3DObjects) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * tdf_obstruct_fp;           // telescope description file - pointer
  int status = 0;                       // for cfitsio function calls
  
  // for getting string keywords
  char * telescop_c;
  telescop_c    = (char *) malloc(80*sizeof(char));
  char * instrume_c;
  instrume_c    = (char *) malloc(80*sizeof(char));
  char * detnam_c;
  detnam_c      = (char *) malloc(80*sizeof(char));
  
  std::map<std::string, int> columns;   // associate column names and numbers
  int columnNum = 0;                    // temp storage of column number
  
  
  std::string errorMsg;
  
  // -------------------------------------

  // open the file to the specified extension
  fits_open_file(&tdf_obstruct_fp, (param.m_obstructfile).c_str(), READONLY, &status);
  checkForFITSError(status, "opening", param.m_obstructfile);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_obstructfile << std::endl;
  
  // error checking that TELESCOP and INSTRUME (user input params) match this 
  // file, in case the user supplied the file instead of CALDB
  fits_read_key_str(tdf_obstruct_fp, "TELESCOP", telescop_c, NULL, &status);
  fits_read_key_str(tdf_obstruct_fp, "INSTRUME", instrume_c, NULL, &status);
  if (!isEqualCaseInsens(param.m_telescop, (std::string)telescop_c)) {
    AH_THROW_RUNTIME("TELESCOP keyword in obstruction file (" + 
                     (std::string)telescop_c + ", in file " + param.m_obstructfile + 
                     ") and input parameter 'telescop' (" + param.m_telescop + ") must match");
  }
  if (!isEqualCaseInsens(param.m_instrume, (std::string)instrume_c)) {
    std::string errMsg = "INSTRUME keyword in obstruction file (" + 
                     (std::string)instrume_c + ", in file " + param.m_obstructfile + 
                     ") and input parameter 'instrume' (" + param.m_instrume + ") do not match";
    AH_INFO(ahlog::HIGH) << errMsg << std::endl;
    // +++ 20150909 KLR warning, not error, so that tests with old files will work.
//    AH_THROW_RUNTIME("INSTRUME keyword in obstruction file (" + 
//                     (std::string)instrume_c + ", in file " + param.m_obstructfile + 
//                     ") and input parameter 'instrume' (" + param.m_instrume + ") must match");
  }
  // free the char * we used to get the string keywords from cfitsio
  free(telescop_c);     telescop_c=0;
  free(instrume_c);     instrume_c=0;
  
  fits_get_num_rows(tdf_obstruct_fp, &(tdfobstruct.m_numRows), &status);
  checkForFITSError(status, "getting number of rows in", param.m_obstructfile);
  
  // resize all vectors to store data
  tdfobstruct.m_set.resize(tdfobstruct.m_numRows);
  tdfobstruct.m_idNumber.resize(tdfobstruct.m_numRows);
  tdfobstruct.m_zCoord.resize(tdfobstruct.m_numRows);
  tdfobstruct.m_xVertex.resize(tdfobstruct.m_numRows);
  tdfobstruct.m_yVertex.resize(tdfobstruct.m_numRows);
  
  // get column numbers
  fits_get_colnum(tdf_obstruct_fp, CASEINSEN, const_cast<char *>("LAYER"), &columnNum, &status);
  columns["LAYER"] = columnNum;
  fits_get_colnum(tdf_obstruct_fp, CASEINSEN, const_cast<char *>("POLYNUM"), &columnNum, &status);
  columns["POLYNUM"] = columnNum;
  fits_get_colnum(tdf_obstruct_fp, CASEINSEN, const_cast<char *>("DISTANCE"), &columnNum, &status);
  columns["DISTANCE"] = columnNum;
  fits_get_colnum(tdf_obstruct_fp, CASEINSEN, const_cast<char *>("XVERTEX"), &columnNum, &status);
  columns["XVERTEX"] = columnNum;
  fits_get_colnum(tdf_obstruct_fp, CASEINSEN, const_cast<char *>("YVERTEX"), &columnNum, &status);
  columns["YVERTEX"] = columnNum;
  checkForFITSError(status, "getting column numbers in", param.m_obstructfile);
  
  // get data from file, store it in vectors
  for (long i = 0 ; i < tdfobstruct.m_numRows ; ++i) {
    fits_read_col(tdf_obstruct_fp, TLONG,   columns["LAYER"],   i+1, 1, 1, 0, &(tdfobstruct.m_set[i]), 0, &status);
    fits_read_col(tdf_obstruct_fp, TLONG,   columns["POLYNUM"], i+1, 1, 1, 0, &(tdfobstruct.m_idNumber[i]), 0, &status);
    fits_read_col(tdf_obstruct_fp, TDOUBLE, columns["DISTANCE"],i+1, 1, 1, 0, &(tdfobstruct.m_zCoord[i]), 0, &status);
    fits_read_col(tdf_obstruct_fp, TDOUBLE, columns["XVERTEX"], i+1, 1, 1, 0, &(tdfobstruct.m_xVertex[i]), 0, &status);
    fits_read_col(tdf_obstruct_fp, TDOUBLE, columns["YVERTEX"], i+1, 1, 1, 0, &(tdfobstruct.m_yVertex[i]), 0, &status);
  }
  checkForFITSError(status, "getting data from", param.m_obstructfile);
  
  //+++ should I be doing anything with keywords?
  
  // close tdf file (obstruct ext)
  fits_close_file(tdf_obstruct_fp, &status);
  checkForFITSError(status, "closing", param.m_obstructfile);
  
  // index tdfobstruct.m_idNumber and tdfobstruct.m_set into ascending order
  tdfobstruct.m_idSortedIdx.resize(tdfobstruct.m_numRows);
  tdfobstruct.m_grpSortedIdx.resize(tdfobstruct.m_numRows);
  //+++ see if alternate way is acceptable, instead of these indices
  
  tdfobstruct.m_idSortedIdx = tdfobstruct.m_idNumber;
  tdfobstruct.m_grpSortedIdx = tdfobstruct.m_set;
  std::sort(tdfobstruct.m_idSortedIdx.begin(), tdfobstruct.m_idSortedIdx.end());
  std::sort(tdfobstruct.m_grpSortedIdx.begin(), tdfobstruct.m_grpSortedIdx.end());
  
  // count obstructions and obstruction groups(sets) (obstCtr used as index)
  int obstCtr = 0;
  
  // store the first obstruction
  Obstruction newObstruction;
  obstructions.push_back(newObstruction);
  obstructions[obstCtr].m_set = tdfobstruct.m_set[0];
  obstructions[obstCtr].m_idNumber = tdfobstruct.m_idNumber[0];
  obstructions[obstCtr].m_zCoord = tdfobstruct.m_zCoord[0];
  obstructions[obstCtr].m_zCoord = tdfobstruct.m_zCoord[0];
  CartesianCoord vertex(tdfobstruct.m_xVertex[0], 
                        tdfobstruct.m_yVertex[0], 
                        tdfobstruct.m_zCoord[0]);
  obstructions[obstCtr].m_vertices.push_back(vertex);   // +++ or resize vertices first?
  obstructions[obstCtr].m_numVertices = 1;
  tdfobstruct.m_numObstGrps = 1;
  
  // Loop over all vertices (file rows) starting from the 2nd one, and count 
  // how many times the id changes
  for (int i = 1 ; i < tdfobstruct.m_numRows ; ++i) {
      
    if ( tdfobstruct.m_idSortedIdx[i] != 
         tdfobstruct.m_idSortedIdx[i-1] ) {
      
      // we're at a new obstruction
      //+++ make sure a default one was constructed
      Obstruction newObstruction;
      newObstruction.m_set = tdfobstruct.m_set[i];
      newObstruction.m_idNumber = tdfobstruct.m_idNumber[i];
      newObstruction.m_zCoord = tdfobstruct.m_zCoord[i];
      CartesianCoord vertex(tdfobstruct.m_xVertex[i], 
                            tdfobstruct.m_yVertex[i], 
                            tdfobstruct.m_zCoord[i]);
      newObstruction.m_vertices.push_back(vertex);
      newObstruction.m_numVertices = 1;
      obstructions.push_back(newObstruction);
      obstCtr++;
    } else {
      // still on same obstruction: add current vertex to array
      CartesianCoord vertex(tdfobstruct.m_xVertex[i], 
                            tdfobstruct.m_yVertex[i], 
                            tdfobstruct.m_zCoord[i]);
      obstructions[obstCtr].m_vertices.push_back(vertex);
      obstructions[obstCtr].m_numVertices++;
    }
    if ( tdfobstruct.m_grpSortedIdx[i] != 
         tdfobstruct.m_grpSortedIdx[i-1] ) { 
      // we're at a new group
      tdfobstruct.m_numObstGrps++;
    }
  }
  // add one more, since we didn't count the first obstruction we added
  // (because obstCt is used as an index, we can't start at 1)
  obstCtr++;
  tdfobstruct.m_numObst = obstCtr;
  
  //+++ show Tahir this loop for getting 3D
  double currZ = 0.0;
  for (int iObst = 0 ; iObst < tdfobstruct.m_numObst ; ++iObst) {
    Obstruction currObstruction = obstructions[iObst];
    currZ = currObstruction.m_zCoord;
      
    for (std::vector<CartesianCoord>::iterator it = currObstruction.m_vertices.begin() ; 
            it != currObstruction.m_vertices.end(); ++it) {
      if ( (*it).m_z != currZ ) {
        tdfobstruct.m_obstructions3d++;
      }
    }
  }
  
  // 3-D obstructions are not yet supported
  if (tdfobstruct.m_obstructions3d > 0) {
    has3DObjects = true;
    errorMsg = "At least one 3-D obstruction representation found. ";
    errorMsg.append("3-D support structures are not yet supported.");
    AH_THROW_RUNTIME(errorMsg);   //+++ we're aborting here, right?
  } else {
    has3DObjects = false;
  }
  
} // end setupTDFObstruct()


/******************************************************************************/


void setupTDFSegment(Param & param, TDF_Segment & seg) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * tdf_seg_fp;            // telescope description file - pointer
  int status = 0;                   // for cfitsio function calls
  char tdfFilename[FLEN_FILENAME];               // get the name of the TDF
  std::string segExtName = "[" + s_tdfSegExtName + "]";
  std::string segFilename;         // TDF filename plus pcol extension
  
  std::map<std::string, int> columns;   // associate column names and numbers
  int columnNum = 0;                    // temp storage of column number
  
  // -------------------------------------

  // extract the filename of the TDF
  fits_parse_input_filename(const_cast<char *>(param.m_mirrorfile.c_str()), 0, tdfFilename, 0, 0, 0, 0, 0, 0, &status);
  checkForFITSError(status, "getting filename of", (std::string)tdfFilename);
  segFilename = ((std::string)tdfFilename).append(segExtName);
  
  // open the file
  fits_open_file(&tdf_seg_fp, segFilename.c_str(), READONLY, &status);
  checkForFITSError(status, "opening", segFilename);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << segFilename << std::endl;
  
  fits_get_num_rows(tdf_seg_fp, &(seg.m_numRows), &status);
  checkForFITSError(status, "getting number of rows in", segFilename);
  
  // resize all vectors to store data
  seg.m_segment.resize(seg.m_numRows);
  seg.m_startAngle.resize(seg.m_numRows);
  seg.m_endAngle.resize(seg.m_numRows);
  seg.m_layer.resize(seg.m_numRows);
  seg.m_deltax.resize(seg.m_numRows);
  seg.m_deltay.resize(seg.m_numRows);
  seg.m_deltaz.resize(seg.m_numRows);
  seg.m_deltatxArcmin.resize(seg.m_numRows);
  seg.m_deltatyArcmin.resize(seg.m_numRows);
  seg.m_deltatzArcmin.resize(seg.m_numRows);
  seg.m_deltatxRad.resize(seg.m_numRows);
  seg.m_deltatyRad.resize(seg.m_numRows);
  seg.m_deltatzRad.resize(seg.m_numRows);
  
  // get column numbers
  fits_get_colnum(tdf_seg_fp, CASEINSEN, const_cast<char *>("SEGMENT"), &columnNum, &status);
  columns["SEGMENT"] = columnNum;
  fits_get_colnum(tdf_seg_fp, CASEINSEN, const_cast<char *>("STARTANGLE"), &columnNum, &status);
  columns["STARTANGLE"] = columnNum;
  fits_get_colnum(tdf_seg_fp, CASEINSEN, const_cast<char *>("ENDANGLE"), &columnNum, &status);
  columns["ENDANGLE"] = columnNum;
  fits_get_colnum(tdf_seg_fp, CASEINSEN, const_cast<char *>("COMPONENT"), &columnNum, &status);
  columns["COMPONENT"] = columnNum;
  fits_get_colnum(tdf_seg_fp, CASEINSEN, const_cast<char *>("DELTAX"), &columnNum, &status);
  columns["DELTAX"] = columnNum;
  fits_get_colnum(tdf_seg_fp, CASEINSEN, const_cast<char *>("DELTAY"), &columnNum, &status);
  columns["DELTAY"] = columnNum;
  fits_get_colnum(tdf_seg_fp, CASEINSEN, const_cast<char *>("DELTAZ"), &columnNum, &status);
  columns["DELTAZ"] = columnNum;
  fits_get_colnum(tdf_seg_fp, CASEINSEN, const_cast<char *>("DELTATX"), &columnNum, &status);
  columns["DELTATX"] = columnNum;
  fits_get_colnum(tdf_seg_fp, CASEINSEN, const_cast<char *>("DELTATY"), &columnNum, &status);
  columns["DELTATY"] = columnNum;
  fits_get_colnum(tdf_seg_fp, CASEINSEN, const_cast<char *>("DELTATZ"), &columnNum, &status);
  columns["DELTATZ"] = columnNum;
  checkForFITSError(status, "getting column numbers in", segFilename);
  
  // get data from file, store it in vectors
  for (long i = 0 ; i < seg.m_numRows ; ++i) {
    //+++ they're not all doubles
    fits_read_col(tdf_seg_fp, TLONG,   columns["SEGMENT"], i+1, 1, 1, 0, &(seg.m_segment[i]), 0, &status);
    fits_read_col(tdf_seg_fp, TDOUBLE, columns["STARTANGLE"], i+1, 1, 1, 0, &(seg.m_startAngle[i]), 0, &status);
    fits_read_col(tdf_seg_fp, TDOUBLE, columns["ENDANGLE"], i+1, 1, 1, 0, &(seg.m_endAngle[i]), 0, &status);
    fits_read_col(tdf_seg_fp, TLONG,   columns["COMPONENT"], i+1, 1, 1, 0, &(seg.m_layer[i]), 0, &status);
    fits_read_col(tdf_seg_fp, TDOUBLE, columns["DELTAX"], i+1, 1, 1, 0, &(seg.m_deltax[i]), 0, &status);
    fits_read_col(tdf_seg_fp, TDOUBLE, columns["DELTAY"], i+1, 1, 1, 0, &(seg.m_deltay[i]), 0, &status);
    fits_read_col(tdf_seg_fp, TDOUBLE, columns["DELTAZ"], i+1, 1, 1, 0, &(seg.m_deltaz[i]), 0, &status);
    fits_read_col(tdf_seg_fp, TDOUBLE, columns["DELTATX"], i+1, 1, 1, 0, &(seg.m_deltatxArcmin[i]), 0, &status);
    fits_read_col(tdf_seg_fp, TDOUBLE, columns["DELTATY"], i+1, 1, 1, 0, &(seg.m_deltatyArcmin[i]), 0, &status);
    fits_read_col(tdf_seg_fp, TDOUBLE, columns["DELTATZ"], i+1, 1, 1, 0, &(seg.m_deltatzArcmin[i]), 0, &status);
    
    seg.m_deltatxRad[i] = seg.m_deltatxArcmin[i] * s_arcminToRadian;
    seg.m_deltatyRad[i] = seg.m_deltatyArcmin[i] * s_arcminToRadian;
    seg.m_deltatzRad[i] = seg.m_deltatzArcmin[i] * s_arcminToRadian;
  }
  checkForFITSError(status, "getting data from", segFilename);
  
  // close tdf file (segment ext)
  fits_close_file(tdf_seg_fp, &status);
  checkForFITSError(status, "closing", segFilename);
  
  // so that 0=pcol, 1=primary, 2=secondary
  std::transform(seg.m_layer.begin(), seg.m_layer.end(), seg.m_layer.begin(),
                 std::bind2nd(std::minus<int>(), 1));
  
  // get min and max segment and layer 
  seg.m_minSegment = *std::min_element(seg.m_segment.begin(), seg.m_segment.end());
  seg.m_maxSegment = *std::max_element(seg.m_segment.begin(), seg.m_segment.end());
  seg.m_minLayer = ( *std::max_element(seg.m_layer.begin(), seg.m_layer.end()) ) + 1;
  seg.m_maxLayer = ( *std::max_element(seg.m_layer.begin(), seg.m_layer.end()) ) + 1;
  
  // reorganize shifts and rotations into 2-D arrays
  seg.m_xShift.resize(seg.m_maxLayer);
  seg.m_yShift.resize(seg.m_maxLayer);
  seg.m_zShift.resize(seg.m_maxLayer);
  seg.m_xRotation.resize(seg.m_maxLayer);
  seg.m_yRotation.resize(seg.m_maxLayer);
  seg.m_zRotation.resize(seg.m_maxLayer);
  for (int i = 0 ; i < seg.m_maxLayer ; ++i) {
    seg.m_xShift[i].resize(seg.m_maxSegment);
    seg.m_yShift[i].resize(seg.m_maxSegment);
    seg.m_zShift[i].resize(seg.m_maxSegment);
    seg.m_xRotation[i].resize(seg.m_maxSegment);
    seg.m_yRotation[i].resize(seg.m_maxSegment);
    seg.m_zRotation[i].resize(seg.m_maxSegment);
  }
  
  // This assumes that all possible layers and segments are present up to the 
  // maximum of each
  for (int i=0 ; i < seg.m_numRows ; ++i) {
    seg.m_xShift[seg.m_layer[i]][seg.m_segment[i]-1] = seg.m_deltax[i];
    seg.m_yShift[seg.m_layer[i]][seg.m_segment[i]-1] = seg.m_deltay[i];
    seg.m_zShift[seg.m_layer[i]][seg.m_segment[i]-1] = seg.m_deltaz[i];
    seg.m_xRotation[seg.m_layer[i]][seg.m_segment[i]-1] = seg.m_deltatxRad[i];
    seg.m_yRotation[seg.m_layer[i]][seg.m_segment[i]-1] = seg.m_deltatyRad[i];
    seg.m_zRotation[seg.m_layer[i]][seg.m_segment[i]-1] = seg.m_deltatzRad[i];
    seg.m_sumofsegmentoffsets += (seg.m_deltax[i] + seg.m_deltay[i] + seg.m_deltaz[i] + 
                                 seg.m_deltatxRad[i] + seg.m_deltatyRad[i] + seg.m_deltatzRad[i]);
  }
  
} // end setupTDFSegment() 


/******************************************************************************/


void setupTDFSurface(Param & param, 
                     int numMaterials, 
                     long numGroups, 
                     int pcolMaterialIndex, 
                     const vector1Ddbl & materialDensity, 
                     const std::vector<std::string> & materialNames,
                     const vector2Ddbl & massAbs,
                     TDF_Surface & surf, 
                     ReflectTransGrids & reflectTransGrids,
                     std::vector<int> & xrtShellToGroupNumber) {
  
  // +++ do I need surf outside of this function? 
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * tdf_surf_fp;             // telescope description file - pointer
  int status = 0;                     // for cfitsio function calls
  char tdfFilename[FLEN_FILENAME];    // get the name of the TDF
  std::string surfExtName = "[" + s_tdfSurfExtName + "]";
  std::string surfFilename;           // TDF filename plus extension
  
  std::map<std::string, int> columns;   // associate column names and numbers
  int columnNum = 0;                    // temp storage of column number
  
  // for storing the TSTRING types from the input atomic data fits file 
  char * material;      // 2 character string
  long numChars = 0;    // how many characters the MATERIAL column holds
  
  int currGroup = 0;    // to reference inside for-loops
  
  // to identify correct mass-absorption coefficient by matching material names
  // size: surf.m_numRows
  std::vector<int> mabssurfaceindex;
  
  // initialize output:
  
  // initialize the tau per mm arrays 
  reflectTransGrids.m_frontTauPermm.resize(reflectTransGrids.m_numFrontEnergies);
  reflectTransGrids.m_roughTauPermm.resize(reflectTransGrids.m_numFrontEnergies);
  for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
    reflectTransGrids.m_frontTauPermm[iEnergy].resize(numGroups+1);
    reflectTransGrids.m_roughTauPermm[iEnergy].resize(numGroups+1);
  }
  
  surf.m_totalThickness.resize(numGroups);
  
  // -------------------------------------
  
  // extract the filename of the TDF
  fits_parse_input_filename(const_cast<char *>(param.m_mirrorfile.c_str()), 0, tdfFilename, 0, 0, 0, 0, 0, 0, &status);
  checkForFITSError(status, "getting filename of", (std::string)tdfFilename);
  surfFilename = ((std::string)tdfFilename).append(surfExtName);
  
  // open the file
  fits_open_file(&tdf_surf_fp, surfFilename.c_str(), READONLY, &status);
  checkForFITSError(status, "opening", surfFilename);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << surfFilename << std::endl;
  
  fits_get_num_rows(tdf_surf_fp, &(surf.m_numRows), &status);
  checkForFITSError(status, "getting number of rows in", surfFilename);
  
  // resize all vectors to store data
  surf.m_layerGroupNumber.resize(surf.m_numRows);
  surf.m_layerFirstShell.resize(surf.m_numRows);
  surf.m_layerLastShell.resize(surf.m_numRows);
  surf.m_layerNumber.resize(surf.m_numRows);
  surf.m_layerMaterial.resize(surf.m_numRows);
  surf.m_layerSurfaceDensity.resize(surf.m_numRows);
  surf.m_layerSurfaceThickness.resize(surf.m_numRows);
  
  // get column numbers
  fits_get_colnum(tdf_surf_fp, CASEINSEN, const_cast<char *>("GROUP"), &columnNum, &status);
  columns["GROUP"] = columnNum;
  fits_get_colnum(tdf_surf_fp, CASEINSEN, const_cast<char *>("FIRSTSHELL"), &columnNum, &status);
  columns["FIRSTSHELL"] = columnNum;
  fits_get_colnum(tdf_surf_fp, CASEINSEN, const_cast<char *>("LASTSHELL"), &columnNum, &status);
  columns["LASTSHELL"] = columnNum;
  fits_get_colnum(tdf_surf_fp, CASEINSEN, const_cast<char *>("LAYER"), &columnNum, &status);
  columns["LAYER"] = columnNum;
  fits_get_colnum(tdf_surf_fp, CASEINSEN, const_cast<char *>("MATERIAL"), &columnNum, &status);
  columns["MATERIAL"] = columnNum;
  fits_get_colnum(tdf_surf_fp, CASEINSEN, const_cast<char *>("DENSITY"), &columnNum, &status);
  columns["DENSITY"] = columnNum;
  fits_get_colnum(tdf_surf_fp, CASEINSEN, const_cast<char *>("THICKNESS"), &columnNum, &status);
  columns["THICKNESS"] = columnNum;
  
  // see how many characters the MATERIAL column holds
  fits_get_coltype(tdf_surf_fp, columns["MATERIAL"], 0, 0, &numChars, &status);
  checkForFITSError(status, "getting width of MATERIAL column", surfFilename);
  material = (char *) malloc(numChars * sizeof(char));
  
  // get data from file, store it in vectors
  for (long i = 0 ; i < surf.m_numRows ; ++i) {
    fits_read_col_lng(tdf_surf_fp, columns["GROUP"],      i+1, 1, 1, 0, &(surf.m_layerGroupNumber[i]), 0, &status);
    fits_read_col_lng(tdf_surf_fp, columns["FIRSTSHELL"], i+1, 1, 1, 0, &(surf.m_layerFirstShell[i]), 0, &status);
    fits_read_col_lng(tdf_surf_fp, columns["LASTSHELL"],  i+1, 1, 1, 0, &(surf.m_layerLastShell[i]), 0, &status);
    fits_read_col_lng(tdf_surf_fp, columns["LAYER"],      i+1, 1, 1, 0, &(surf.m_layerNumber[i]), 0, &status);
    fits_read_col_str(tdf_surf_fp, columns["MATERIAL"],   i+1, 1, 1, 0, &material, 0, &status);
    fits_read_col_dbl(tdf_surf_fp, columns["DENSITY"],    i+1, 1, 1, 0, &(surf.m_layerSurfaceDensity[i]), 0, &status);
    fits_read_col_dbl(tdf_surf_fp, columns["THICKNESS"],  i+1, 1, 1, 0, &(surf.m_layerSurfaceThickness[i]), 0, &status);
    
    // convert the char * from cfitsio into C++ strings, store in the Param
    surf.m_layerMaterial[i] = (std::string)material;
    
    // +++ error check input?  
    // make sure GROUP doesn't go above numGroups, etc (or else vectors sized to numGroups will fail)
    // make sure densities match materialDensity, which is from refl file, which should be from this file
  }
  
  // free the char * we used to get the TSTRING data from cfitsio
  free(material); material=0;
  
  // check fits error after we free the char *
  checkForFITSError(status, "getting data from", surfFilename);
  
  // close tdf file (SURFACE ext)
  fits_close_file(tdf_surf_fp, &status);
  checkForFITSError(status, "closing", surfFilename);
  
  // number of mirror shells: value of the last element of "layerlastshell"
  surf.m_numMirrorShells = surf.m_layerLastShell.at(surf.m_numRows-1);
  
  AH_DEBUG << "numMaterials = " << reflectTransGrids.m_numMaterials << std::endl;
  AH_DEBUG << "numGroups = " << numGroups << std::endl;
  
  /* testing that I got surface grid data
  #ifdef DEBUG
  #endif
  AH_DEBUG << "*** testing that I got surface data ***" << std::endl;
  AH_DEBUG << "m_numRows = " << surf.m_numRows << std::endl;
  AH_DEBUG << "  GROUP \t FIRSTSHELL \t LASTSHELL \t LAYER \t MATERIAL \t DENSITY \t THICKNESS     " << std::endl;
  for (int i = 0 ; i < surf.m_numRows ; ++i) { 
    AH_DEBUG <<  i << " " << surf.m_layerGroupNumber[i] << '\t' << '\t' << 
                             surf.m_layerFirstShell[i] << '\t' << '\t' << 
                             surf.m_layerLastShell[i] << '\t' << '\t' << 
                             surf.m_layerNumber[i] << '\t' << 
                             surf.m_layerMaterial[i] << '\t' << '\t' << 
                             surf.m_layerSurfaceDensity[i] << '\t' << '\t' << 
                             surf.m_layerSurfaceThickness[i] << '\t' << 
                             std::endl;
  }
  AH_DEBUG << "m_numMirrorShells = " << surf.m_numMirrorShells << std::endl;
   */
  
  // initialize to how many shells we have.  each shell will match to a group number
  xrtShellToGroupNumber.resize(surf.m_numMirrorShells);
  
  mabssurfaceindex.resize(surf.m_numRows);
  
  // Fill in xrtShellToGroupNumber for the first group of shells
  currGroup = surf.m_layerGroupNumber.at(0);
  for (int iShell = surf.m_layerFirstShell.at(0) ; iShell < surf.m_layerLastShell.at(0) ; ++iShell) {
    xrtShellToGroupNumber.at(iShell-1) = currGroup;
  }
  
  // Loop over rows in the surface extension
  for (int iRow = 0 ; iRow < surf.m_numRows ; ++iRow) {
    
    currGroup = surf.m_layerGroupNumber.at(iRow);
    
    // At each group number change, fill in groupnumbers for each range of shells
    if ( (iRow > 0) && (currGroup != surf.m_layerGroupNumber.at(iRow-1) ) ) {
      for (int iShell = surf.m_layerFirstShell.at(iRow) ; iShell <= surf.m_layerLastShell.at(iRow) ; ++iShell) { 
        xrtShellToGroupNumber.at(iShell-1) = currGroup;
      }
    }
    
    // identify the correct mass-absorption coefficient by matching the material names
    for (int iMat = 0 ; iMat < reflectTransGrids.m_numMaterials ; ++iMat) {
      //if ( surf.m_layerMaterial.at(iRow).compare(materialNames.at(iMat)) == 0) {
      if ( surf.m_layerMaterial.at(iRow) == materialNames.at(iMat) ) {
        mabssurfaceindex.at(iRow) = iMat;
      }
    }
    
    // thin film (positive layer number)
    if (surf.m_layerNumber.at(iRow) >= 0) {
      
      // accumulate the optical depth for the thin film (positive layer number) by group
      for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
        reflectTransGrids.m_frontTauPermm.at(iEnergy).at(currGroup) += 
                (surf.m_layerSurfaceDensity.at(iRow) * 
                 surf.m_layerSurfaceThickness.at(iRow) * 
                 s_mabsToTau * massAbs.at(iEnergy).at(mabssurfaceindex.at(iRow)));
      }
      
    } else {
      // thick layers (negative layer number)
      // layer(i) < 0
      
      // accumulate the optical depth for the thick layers (negative layer number) by group 
      for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
        reflectTransGrids.m_roughTauPermm.at(iEnergy).at(currGroup) += 
                ( surf.m_layerSurfaceDensity.at(iRow) * 
                  surf.m_layerSurfaceThickness.at(iRow) * 
                  s_mabsToTau * massAbs.at(iEnergy).at(mabssurfaceindex.at(iRow)) );
      }
      
    }
    
    // accumulate the total thickness by group number
    surf.m_totalThickness.at(surf.m_layerGroupNumber.at(iRow)-1) += (s_angstromTomm * surf.m_layerSurfaceThickness.at(iRow));
    
  } // end-for over SURFACE extension rows
  
  AH_DEBUG << "materialDensity.size() = " << materialDensity.size() << std::endl;
  AH_DEBUG << "surf.m_totalThickness.size() = " << surf.m_totalThickness.size() << std::endl;
  AH_DEBUG << "reflectTransGrids.m_roughTauPermm.size() = " << reflectTransGrids.m_roughTauPermm.size() << std::endl;
  AH_DEBUG << "pcolMaterialIndex = " << pcolMaterialIndex << std::endl;
  AH_DEBUG << "reflectTransGrids.m_frontTauPermm[0].size = " << reflectTransGrids.m_frontTauPermm[0].size() << std::endl;
  
  /* testing that I calculated material and group data correctly */
  #ifdef DEBUG
  AH_DEBUG << "*** testing that I calculated material and group data correctly ***" << std::endl;
  for (uint i = 0 ; i < materialDensity.size() ; ++i) {
    AH_DEBUG << "material[" << i << "] name = '" << materialNames[i] << "' Density = " << materialDensity.at(i) << std::endl;
  }
  for (int i = 0 ; i < numGroups ; ++i) {
    AH_DEBUG << "group " << i << " total thickness = " << surf.m_totalThickness[i] << std::endl;
  }
  #endif
  
  
  // Now do the pre-collimator, which is effectively group zero
  if (param.m_pcolExists) {
  // regarding pcolMaterialIndex: 'index' here really means column number.  The 
  // reflectivity files from the tool xtreftable write the energy column first, 
  // then the mass absorption coefficient columns.  To get the actual index of 
  // materialDensity, we need to subtract 2.
    double pcolOpticalDepthFactor = materialDensity.at(pcolMaterialIndex-2) * 0.1;
    // The optical depth grids are on the same energy grid as the front-side reflectivity
    for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
      reflectTransGrids.m_roughTauPermm.at(iEnergy).at(0) += (pcolOpticalDepthFactor * massAbs.at(iEnergy).at(pcolMaterialIndex-2));
    }
  }
  
  // Loop over each group and all energies again to normalize the optical 
  // depths so that they are optical depth per mm
  for (int iGroup = 0 ; iGroup < numGroups ; ++iGroup) {
    // recall the the size of TauPermm arrays is ngroups+1 in the 2nd index 
    for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
      reflectTransGrids.m_frontTauPermm.at(iEnergy).at(iGroup+1) /= surf.m_totalThickness.at(iGroup);
      reflectTransGrids.m_roughTauPermm.at(iEnergy).at(iGroup+1) /= surf.m_totalThickness.at(iGroup);
    }
  }
  
//  for (int iGroup = 0 ; iGroup <= numGroups ; ++iGroup) {
//    for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
//      AH_DEBUG << "frontTauPermm["<<iEnergy<<"]["<<iGroup<<"] = " << reflectTransGrids.m_frontTauPermm.at(iEnergy).at(iGroup) << std::endl; 
//      AH_DEBUG << "roughTauPermm["<<iEnergy<<"]["<<iGroup<<"] = " << reflectTransGrids.m_roughTauPermm.at(iEnergy).at(iGroup) << std::endl; 
//    }
//  }
  
} // end setupTDFSurface()


/******************************************************************************/


void setupTDFExternalObjects(Param & param, 
                             TDF_External & tdfExtObjects) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * tdf_ext_fp;                // telescope description file - pointer
  int status = 0;                       // for cfitsio function calls
  char tdfFilename[FLEN_FILENAME];      // get the name of the TDF
  std::string externalExtName = "[" + s_tdfExternalExtName + "]";
  std::string externalFilename;         // TDF filename plus extension
  
  std::map<std::string, int> columns;   // associate column names and numbers
  int columnNum = 0;                    // temp storage of column number
  
  // -------------------------------------

  // extract the filename of the TDF
  fits_parse_input_filename(const_cast<char *>(param.m_mirrorfile.c_str()), 0, tdfFilename, 0, 0, 0, 0, 0, 0, &status);
  checkForFITSError(status, "getting filename of", (std::string)tdfFilename);
  externalFilename = ((std::string)tdfFilename).append(externalExtName);
  
  // open the file
  fits_open_file(&tdf_ext_fp, externalFilename.c_str(), READONLY, &status);
  checkForFITSError(status, "opening", externalExtName);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << externalFilename << std::endl;
  
  fits_get_num_rows(tdf_ext_fp, &(tdfExtObjects.m_numExtObjectParts), &status);
  checkForFITSError(status, "getting number of rows in", externalFilename);
  
  // resize all vectors to store data
  tdfExtObjects.m_objectNumber.resize(tdfExtObjects.m_numExtObjectParts);
  tdfExtObjects.m_partNumber.resize(tdfExtObjects.m_numExtObjectParts);
  tdfExtObjects.m_radMin.resize(tdfExtObjects.m_numExtObjectParts);
  tdfExtObjects.m_radMax.resize(tdfExtObjects.m_numExtObjectParts);
  tdfExtObjects.m_startAngle.resize(tdfExtObjects.m_numExtObjectParts);
  tdfExtObjects.m_endAngle.resize(tdfExtObjects.m_numExtObjectParts);
  tdfExtObjects.m_zMin.resize(tdfExtObjects.m_numExtObjectParts);
  tdfExtObjects.m_zMax.resize(tdfExtObjects.m_numExtObjectParts);
  tdfExtObjects.m_density.resize(tdfExtObjects.m_numExtObjectParts);
  tdfExtObjects.m_mabsIndex.resize(tdfExtObjects.m_numExtObjectParts);

  
  // get column numbers
  fits_get_colnum(tdf_ext_fp, CASEINSEN, const_cast<char *>("OBJECT"), &columnNum, &status);
  columns["OBJECT"] = columnNum;
  fits_get_colnum(tdf_ext_fp, CASEINSEN, const_cast<char *>("SUBOBJECT"), &columnNum, &status);
  columns["SUBOBJECT"] = columnNum;
  fits_get_colnum(tdf_ext_fp, CASEINSEN, const_cast<char *>("RMIN"), &columnNum, &status);
  columns["RMIN"] = columnNum;
  fits_get_colnum(tdf_ext_fp, CASEINSEN, const_cast<char *>("RMAX"), &columnNum, &status);
  columns["RMAX"] = columnNum;
  fits_get_colnum(tdf_ext_fp, CASEINSEN, const_cast<char *>("STARTANGLE"), &columnNum, &status);
  columns["STARTANGLE"] = columnNum;
  fits_get_colnum(tdf_ext_fp, CASEINSEN, const_cast<char *>("ENDANGLE"), &columnNum, &status);
  columns["ENDANGLE"] = columnNum;
  fits_get_colnum(tdf_ext_fp, CASEINSEN, const_cast<char *>("ZMIN"), &columnNum, &status);
  columns["ZMIN"] = columnNum;
  fits_get_colnum(tdf_ext_fp, CASEINSEN, const_cast<char *>("ZMAX"), &columnNum, &status);
  columns["ZMAX"] = columnNum;
  fits_get_colnum(tdf_ext_fp, CASEINSEN, const_cast<char *>("EFFDENSITY"), &columnNum, &status);
  columns["EFFDENSITY"] = columnNum;
  fits_get_colnum(tdf_ext_fp, CASEINSEN, const_cast<char *>("MABSINDEX"), &columnNum, &status);
  columns["MABSINDEX"] = columnNum;
  checkForFITSError(status, "getting column numbers in", externalFilename);
  
  // get data from file, store it in vectors
  for (long iRow = 0 ; iRow < tdfExtObjects.m_numExtObjectParts ; ++iRow) {
    //+++ they're not all doubles
    fits_read_col(tdf_ext_fp, TLONG,   columns["OBJECT"], iRow+1, 1, 1, 0, &(tdfExtObjects.m_objectNumber[iRow]), 0, &status);
    fits_read_col(tdf_ext_fp, TLONG,   columns["SUBOBJECT"], iRow+1, 1, 1, 0, &(tdfExtObjects.m_partNumber[iRow]), 0, &status);
    fits_read_col(tdf_ext_fp, TDOUBLE, columns["RMIN"], iRow+1, 1, 1, 0, &(tdfExtObjects.m_radMin[iRow]), 0, &status);
    fits_read_col(tdf_ext_fp, TDOUBLE, columns["RMAX"], iRow+1, 1, 1, 0, &(tdfExtObjects.m_radMax[iRow]), 0, &status);
    fits_read_col(tdf_ext_fp, TDOUBLE, columns["STARTANGLE"], iRow+1, 1, 1, 0, &(tdfExtObjects.m_startAngle[iRow]), 0, &status);
    fits_read_col(tdf_ext_fp, TDOUBLE, columns["ENDANGLE"], iRow+1, 1, 1, 0, &(tdfExtObjects.m_endAngle[iRow]), 0, &status);
    fits_read_col(tdf_ext_fp, TDOUBLE, columns["ZMIN"], iRow+1, 1, 1, 0, &(tdfExtObjects.m_zMin[iRow]), 0, &status);
    fits_read_col(tdf_ext_fp, TDOUBLE, columns["ZMAX"], iRow+1, 1, 1, 0, &(tdfExtObjects.m_zMax[iRow]), 0, &status);
    fits_read_col(tdf_ext_fp, TDOUBLE, columns["EFFDENSITY"], iRow+1, 1, 1, 0, &(tdfExtObjects.m_density[iRow]), 0, &status);
    fits_read_col(tdf_ext_fp, TLONG,   columns["MABSINDEX"], iRow+1, 1, 1, 0, &(tdfExtObjects.m_mabsIndex[iRow]), 0, &status);
  }
  checkForFITSError(status, "getting data from", externalFilename);
  
  // close tdf file (segment ext)
  fits_close_file(tdf_ext_fp, &status);
  checkForFITSError(status, "closing", externalFilename);
  
  
  // The smallest positive value of mabsindex will be used by the routine that reads the mass-absorption data from the FITS file to separate the data for mirror foil bodies and the data for external objects.
  for (int iExtObj = 0 ; iExtObj < tdfExtObjects.m_numExtObjectParts ; ++iExtObj) {
    
    // first, look for the first index above zero
    if ( (tdfExtObjects.m_minMabsIndex == 0) && 
         (tdfExtObjects.m_mabsIndex[iExtObj] > 0) ) {
      tdfExtObjects.m_minMabsIndex = tdfExtObjects.m_mabsIndex[iExtObj];
      
    } else if ( (tdfExtObjects.m_minMabsIndex != 0) && 
                (tdfExtObjects.m_mabsIndex[iExtObj] < tdfExtObjects.m_minMabsIndex) ) {
      // then, look for any positive indices that are smaller
      tdfExtObjects.m_minMabsIndex = tdfExtObjects.m_mabsIndex[iExtObj];
      
    }
  }
  
  AH_DEBUG << "tdfExtObjects.m_minMabsIndex = " << tdfExtObjects.m_minMabsIndex << std::endl;
  
} // end setupTDFExternalObjects()


/******************************************************************************/


void readScatteringFile(const std::string & scatterfilename, 
                        const std::string & telescop,
                        const std::string & instrume,
                        Scattering & scat) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * scat_fp;                   // scattering probability data file
  int status = 0;                       // for cfitsio function calls
  std::string errorMsg;                 // error message to print
  errorMsg = "The scattering file <";
  errorMsg.append(scatterfilename);
  errorMsg.append("> must be formatted correctly. ");
  
  // for getting string keywords
  char * telescop_c;
  telescop_c    = (char *) malloc(80*sizeof(char));
  char * instrume_c;
  instrume_c    = (char *) malloc(80*sizeof(char));
  std::string telescop_str;
  std::string instrume_str;
  
  // columns are 1based, and we don't care about first, energy, column, so add
  // an offset as we loop through them later
  const int colOffset = 2;
  int currColNum = 0;
  int currRowNum = 0;
  char colNameChar[FLEN_VALUE] = "";    // for getting the names of a column
  int energyColNum = 0;       
  
  // for searching for back and pcol columns
  bool backColumnExists = false;
  bool pcolColumnExists = false;
 
  int naxis = 0;                  // for reading TDIMn
  int maxdim = 2;                 // TDIMn keyword is for a 2-dimensional array
  std::vector<long> numAxes(2);   // contains TDIMn axis numbers
  
  // quick variables to hold num scat and inc angles
  int numScat = 0;
  int numInc = 0;
  
  double scatStart = 0.0;          // initial value
  double scatDelta = 0.0;          // delta
  double scatHalfDelta = 0.0;      // .5 * delta
  double incStart = 0.0;           // initial value
  double incDelta = 0.0;           // delta
  
  // for getting the keywords later.  I will need to append column number
  std::string ONECRVL = "1CRVL";
  std::string ONECDLT = "1CDLT";
  std::string TWOCRVL = "2CRVL";
  std::string TWOCDLT = "2CDLT";
  std::string currColNumStr;
  
  // to use as a reference later, as we're checking that all front columns
  // have same dimensions and value for angle grids
  std::vector<int> refFrontColIdx;
  
  // Count the number of zero-valued probabilities in a given scattering array slice
  int zeroValuesCounter = 0;
  
  // size of the probability array in the fits
  long probArraySize = 0;
  // temp holder of scattering probability for entry in a given row and column,
  // used by cfitsio to read data
  vector1Ddbl tempScatProb;
  // tempScatProb is a 1D array, but we access it inside two loops
  int tempScatProbIdx = 0;
  // for summing all the previous probabilities to create a cumulative array
  double tempCumScatProb = 0.0;   
  
  vector2Dlng allnumscatteringangles;
  vector2Ddbl allscatteringAngleStartValues;
  vector2Ddbl allscatteringAngleDeltaValues;
  vector2Dlng allnumincidentangles;
  vector2Ddbl allincidentAngleStartValues;
  vector2Ddbl allincidentAngleDeltaValues;
  std::vector<bool> allBackColExists;
  std::vector<bool> allPcolColExists;
      
  // -------------------------------------
  
  // open the file to the first extension
  fits_open_file(&scat_fp, scatterfilename.c_str(), READONLY, &status); // +++ valgrind
  checkForFITSError(status, "opening", scatterfilename);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << scatterfilename << std::endl;
  
  // how many extensions (each ext is for one segment)
  fits_get_num_hdus(scat_fp, &scat.m_numSegments, &status);
  checkForFITSError(status, "counting extensions in", scatterfilename);
  // subtract the primary extension
  scat.m_numSegments -= 1;
  
  AH_DEBUG << "num scattering extensions (not primary) = " << scat.m_numSegments << std::endl;
  
  // resize arrays where we need first dimen = number of extensions
  scat.m_numColumns.resize(scat.m_numSegments);
  scat.m_scatcolnames.resize(scat.m_numSegments);
  scat.m_backColumnScatID.resize(scat.m_numSegments);
  scat.m_pcolColumnScatID.resize(scat.m_numSegments);
  scat.m_cumScatDist.resize(scat.m_numSegments);
  scat.m_firstNonZeroScatProb.resize(scat.m_numSegments);
  scat.m_lastNonZeroScatProb.resize(scat.m_numSegments);
  
  allnumscatteringangles.resize(scat.m_numSegments);
  allscatteringAngleStartValues.resize(scat.m_numSegments);
  allscatteringAngleDeltaValues.resize(scat.m_numSegments);
  allnumincidentangles.resize(scat.m_numSegments);
  allincidentAngleStartValues.resize(scat.m_numSegments);
  allincidentAngleDeltaValues.resize(scat.m_numSegments);
  allBackColExists.resize(scat.m_numSegments, false);
  allPcolColExists.resize(scat.m_numSegments, false);
  
  refFrontColIdx.resize(scat.m_numSegments);
  
  // move to the first relevant extension
  fits_movrel_hdu(scat_fp, 1, 0, &status);
  checkForFITSError(status, "moving to first extension in", scatterfilename);
  
  // error checking that TELESCOP and INSTRUME (user input params) match this 
  // file, in case the user supplied the file instead of CALDB
  fits_read_key_str(scat_fp, "TELESCOP", telescop_c, NULL, &status);
  fits_read_key_str(scat_fp, "INSTRUME", instrume_c, NULL, &status);
  if (!isEqualCaseInsens(telescop, (std::string)telescop_c)) {
    AH_THROW_RUNTIME("TELESCOP keyword in scattering file (" + 
                     (std::string)telescop_c + ", in file " + scatterfilename + 
                     ") and input parameter 'telescop' (" + telescop + ") must match");
  }
  if (!isEqualCaseInsens(instrume, (std::string)instrume_c)) {
    std::string errMsg = "INSTRUME keyword in scattering file (" + 
                     (std::string)instrume_c + ", in file " + scatterfilename + 
                     ") and input parameter 'instrume' (" + instrume + ") do not match";
    AH_INFO(ahlog::HIGH) << errMsg << std::endl;
    // +++ 20150909 KLR warning, not error, so that tests with old files will work.
//    AH_THROW_RUNTIME("INSTRUME keyword in scattering file (" + 
//                     (std::string)instrume_c + ", in file " + scatterfilename + 
//                     ") and input parameter 'instrume' (" + instrume + ") must match");
  }
  // free the char * we used to get the string keywords from cfitsio
  free(telescop_c);     telescop_c=0;
  free(instrume_c);     instrume_c=0;
  
  // get the energy grid (all extensions should be same, just grab it from first extension)
  fits_get_colnum(scat_fp, CASEINSEN, const_cast<char *>("ENERGY"), &energyColNum, &status);
  checkForFITSError(status, "getting energy column number in", scatterfilename);
  fits_get_num_rows(scat_fp, &scat.m_numscatenergies, &status);
  checkForFITSError(status, "counting rows in", scatterfilename);
  scat.m_scatEnergies.resize(scat.m_numscatenergies);
  fits_read_col_dbl(scat_fp, energyColNum, 1, 1, scat.m_numscatenergies, 0, &(scat.m_scatEnergies[0]), 0, &status);
  checkForFITSError(status, "reading energy data in", scatterfilename);
  
  // Loop over extensions (segments)
  for (int iSeg = 0 ; iSeg < scat.m_numSegments ; ++iSeg) {
    
    // reset this for each extension.  If a back and/or pcol column are found 
    // in one extension, they should be in every extension, so these will be 
    // true at the end of the for-loop through extensions
//    backColumnExists = false;
//    pcolColumnExists = false;
    
    // go to this extension (first extension, number 1, is primary.  go to 2)
    fits_movabs_hdu(scat_fp, iSeg+2, 0, &status);
    checkForFITSError(status, "moving to extension in", scatterfilename);

    // how many columns in this extension
    fits_get_num_cols(scat_fp, &(scat.m_numColumns[iSeg]), &status);
    checkForFITSError(status, "getting number of columns in", scatterfilename);
    // subtract the energy column count
    scat.m_numColumns[iSeg] -= 1;
    
    // resize, how many columns in this extension
    scat.m_scatcolnames[iSeg].resize(scat.m_numColumns[iSeg]);
    scat.m_cumScatDist[iSeg].resize(scat.m_numColumns[iSeg]);
    scat.m_firstNonZeroScatProb[iSeg].resize(scat.m_numColumns[iSeg]);
    scat.m_lastNonZeroScatProb[iSeg].resize(scat.m_numColumns[iSeg]);
    
    allnumscatteringangles[iSeg].resize(scat.m_numColumns[iSeg]);
    allscatteringAngleStartValues[iSeg].resize(scat.m_numColumns[iSeg]);
    allscatteringAngleDeltaValues[iSeg].resize(scat.m_numColumns[iSeg]);
    allnumincidentangles[iSeg].resize(scat.m_numColumns[iSeg]);
    allincidentAngleStartValues[iSeg].resize(scat.m_numColumns[iSeg]);
    allincidentAngleDeltaValues[iSeg].resize(scat.m_numColumns[iSeg]);
    
    // There may be zero or one column corresponding to back-side mirror 
    // scattering and there may be zero or one column corresponding to 
    // pre-collimator scattering. All of the front-side scattering columns 
    // should have the same dimensions but the back-side and pre-collimator 
    // columns could have different dimensions to the front-side scattering but 
    // the back-side and pre-collimator scattering arrays should have the same 
    // dimensions as each other. To accommodate these uncertainties, we loop 
    // over all the columns and establish the incident angle and scattering 
    // angle dimensions. The probability array will be created using the 
    // largest dimensions found. The scattering index array established in 
    // xrtsetup() and used in raytraceonephoton() relies on the probabilities 
    // being in a single array so the probability array should not be split.
    // Note that it is assumed that the back-scattering and pre-collimator 
    // columns in different segments are identical in their structure.
    
    // the code assumes that energy is the first column.  verify that here.
    fits_get_colnum(scat_fp, CASEINSEN, const_cast<char *>("ENERGY"), &currColNum, &status);
    checkForFITSError(status, "getting energy column number in", scatterfilename);
    if (currColNum != 1) {
      AH_THROW_RUNTIME("'Energy' must be first column in scattering file");
    }
    
    // +++ TODO 20150121 KLR this should be checked for user-error
    //     verify: back and pcol columns have same dim as each other, and in all extensions
    //             1CTYP2 = scattering, 2CTYP2 = incident?
    
    // loop through all the columns in this extension
    for (int iCol = 0 ; iCol < scat.m_numColumns[iSeg] ; ++iCol) {
      
      // columns are 1-based.  we don't care about the first column, energy.
      currColNum = iCol + colOffset;
      currColNumStr = intToString(currColNum);
      
      // get and store the current column name
      fits_get_colname(scat_fp, CASEINSEN, const_cast<char*>(currColNumStr.c_str()), colNameChar, &currColNum, &status);
      checkForFITSError(status, "getting column name in", scatterfilename);
      scat.m_scatcolnames[iSeg][iCol] = std::string(colNameChar);

      // get the number of angles from TDIM
      // Generate the scattering and incident angle grids for column 2
      fits_read_tdim(scat_fp, currColNum, maxdim, &naxis, &(numAxes[0]), &status);
      checkForFITSError(status, "reading TDIMn keyword in", scatterfilename);
      allnumscatteringangles[iSeg][iCol] = numAxes[0];
      allnumincidentangles[iSeg][iCol] = numAxes[1];
      
      // get 1CRVLnn, etc to generate the angle grids for column nn
      ONECRVL = "1CRVL";
      ONECDLT = "1CDLT";
      TWOCRVL = "2CRVL";
      TWOCDLT = "2CDLT";
      ONECRVL.append(currColNumStr);
      ONECDLT.append(currColNumStr);
      TWOCRVL.append(currColNumStr);
      TWOCDLT.append(currColNumStr);
      // +++ verify this, that 1=scat and 2=inc
      fits_read_key(scat_fp, TDOUBLE, const_cast<char *>(ONECRVL.c_str()), &scatStart, 0, &status);
      fits_read_key(scat_fp, TDOUBLE, const_cast<char *>(ONECDLT.c_str()), &scatDelta, 0, &status);
      fits_read_key(scat_fp, TDOUBLE, const_cast<char *>(TWOCRVL.c_str()), &incStart, 0, &status);
      fits_read_key(scat_fp, TDOUBLE, const_cast<char *>(TWOCDLT.c_str()), &incDelta, 0, &status);
      checkForFITSError(status, "reading CRVL, etc keywords in", scatterfilename);

      allscatteringAngleStartValues[iSeg][iCol] = scatStart;
      allscatteringAngleDeltaValues[iSeg][iCol] = scatDelta;
      allincidentAngleStartValues[iSeg][iCol] = incStart;
      allincidentAngleDeltaValues[iSeg][iCol] = incDelta;
      
      // Check if the column has the string 'BACK' or 'PCOL'; if so, record 
      // the column number and don’t do this again for the same segment.
      if ( foundCaseInsens(scat.m_scatcolnames[iSeg][iCol], "BACK") && !allBackColExists[iSeg] ) {
        scat.m_backColumnScatID[iSeg] = iCol; // this is index, not col num
        backColumnExists = true;
        allBackColExists[iSeg] = true;
      } else if ( foundCaseInsens(scat.m_scatcolnames[iSeg][iCol], "PCOL") && !allPcolColExists[iSeg] ) {
        scat.m_pcolColumnScatID[iSeg] = iCol; // this is index, not col num
        pcolColumnExists = true;
        allPcolColExists[iSeg] = true;
      } else {
        // if it's not a back or pcol column, it's front column.  store this
        // index as a reference front column.  It will end up being the last
        // front column, which is fine.  All front columns should have same 
        // dimensions, which we check later
        refFrontColIdx[iSeg] = iCol;
      }
      
      // finish resizing the arrays
      scat.m_cumScatDist[iSeg][iCol].resize(scat.m_numscatenergies);
      scat.m_firstNonZeroScatProb[iSeg][iCol].resize(scat.m_numscatenergies);
      scat.m_lastNonZeroScatProb[iSeg][iCol].resize(scat.m_numscatenergies);
      for (int iEnergy = 0 ; iEnergy < scat.m_numscatenergies ; ++iEnergy) {
        scat.m_cumScatDist[iSeg][iCol][iEnergy].resize(allnumscatteringangles[iSeg][iCol]);
        for (int iScat = 0 ; iScat < allnumscatteringangles[iSeg][iCol] ; ++iScat) {
          scat.m_cumScatDist[iSeg][iCol][iEnergy][iScat].resize(allnumincidentangles[iSeg][iCol], 0.0);
        }
        scat.m_firstNonZeroScatProb[iSeg][iCol][iEnergy].resize(allnumincidentangles[iSeg][iCol], -1);
        scat.m_lastNonZeroScatProb[iSeg][iCol][iEnergy].resize(allnumincidentangles[iSeg][iCol], -1);
      }
      
    } // end for-loop over columns
    
    // verify that, if they exist, there is only one back and one pcol col, 
    // at the end of ext, in that order
//    if (pcolColumnExists) {
//      if (scat.m_pcolColumnScatID[iSeg] != scat.m_numColumns[iSeg]-1) {
//        errorMsg.append("The precollimator column must be the last column.");
//        AH_THROW_RUNTIME(errorMsg);
//      }
//      // if there's a pcol and back column, the back must be before pcol
//      if (backColumnExists) {
//        if (scat.m_backColumnScatID[iSeg] != scat.m_numColumns[iSeg]-2){
//          errorMsg.append("The back column must be at the end of the extension, just before the precollimator column.");
//          AH_THROW_RUNTIME(errorMsg);
//        }
//      }
//    } else if (backColumnExists) {
//      // if there's no pcol, but is a back, the back must be last
//      if (scat.m_backColumnScatID[iSeg] != scat.m_numColumns[iSeg]-1){
//        errorMsg.append("The back column must be be the last column.");
//        AH_THROW_RUNTIME(errorMsg);
//      }
//    }
    
  } // end for-loop over segments (extensions)
  
  // If at least one back and/or pcol column was found, make sure each extension
  // had a back and/or pcol column
  if (backColumnExists) {
    for (int iSeg = 0 ; iSeg < scat.m_numSegments ; ++iSeg) {
      if (!allBackColExists[iSeg]) {
        errorMsg.append("A column for the back of the foils must be in each extension if it is in any extension.");
        AH_THROW_RUNTIME(errorMsg);
      }
    }
  }
  if (pcolColumnExists) {
    for (int iSeg = 0 ; iSeg < scat.m_numSegments ; ++iSeg) {
      if (!allPcolColExists[iSeg]) {
        errorMsg.append("A column for the precollimator must be in each extension if it is in any extension.");
        AH_THROW_RUNTIME(errorMsg);
      }
    }
  }
  
  // get the column id (not column number) for the first rough column.  This is 
  // for getting the scattering and incident arrays.  Each extension should be 
  // the same, so just use first extension (index 0).
  if (backColumnExists) {
    scat.m_roughScatID = scat.m_backColumnScatID[0];
  } else if (pcolColumnExists) {
    scat.m_roughScatID = scat.m_pcolColumnScatID[0];
  } else {
    scat.m_roughScatID = -1;
  }
  
  int currBackColID = 0;
  int currPcolColID = 0;
  int currFrontColID = 0;
  int firstBackColID = 0;
  int firstPcolColID = 0;
  
  if (backColumnExists) {
    firstBackColID = scat.m_backColumnScatID[0];
  }
  if (pcolColumnExists) {
    firstPcolColID = scat.m_pcolColumnScatID[0];
  }
  
  // verify that all of the front scatter columns have same dimensions,
  // start, and delta across segments and columns
  for (int iSeg = 0 ; iSeg < scat.m_numSegments ; ++iSeg) {
    
    // make sure all back angles, if they are provided, are the same
    // across all extensions (just compare to the first extension)
    if (backColumnExists) {
      currBackColID = scat.m_backColumnScatID[iSeg];
      if (allnumscatteringangles[iSeg][currBackColID] != allnumscatteringangles[0][firstBackColID]) {
        errorMsg.append("All back-side scattering angle dimensions must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allnumincidentangles[iSeg][currBackColID] != allnumincidentangles[0][firstBackColID]) {
        errorMsg.append("All back-side incident angle dimensions must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allscatteringAngleStartValues[iSeg][currBackColID] != allscatteringAngleStartValues[0][firstBackColID]) {
        errorMsg.append("All back-side scattering angle start values, in keyword 1CRVLnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allscatteringAngleDeltaValues[iSeg][currBackColID] != allscatteringAngleDeltaValues[0][firstBackColID]) {
        errorMsg.append("All back-side scattering angle delta values, in keyword 1CDLTnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allincidentAngleStartValues[iSeg][currBackColID] != allincidentAngleStartValues[0][firstBackColID]) {
        errorMsg.append("All back-side incident angle start values, in keyword 2CRVLnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allincidentAngleDeltaValues[iSeg][currBackColID] != allincidentAngleDeltaValues[0][firstBackColID]) {
        errorMsg.append("All back-side incident angle delta values, in keyword 2CDLTnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
    }
    // now make sure all pcol angles, if they are provided, are the same
    // across all extensions (just compare to the first extension)
    if (pcolColumnExists) {
      currPcolColID = scat.m_pcolColumnScatID[iSeg];
      if (allnumscatteringangles[iSeg][currPcolColID] != allnumscatteringangles[0][firstPcolColID]) {
        errorMsg.append("All precollimator scattering angle dimensions must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allnumincidentangles[iSeg][currPcolColID] != allnumincidentangles[0][firstPcolColID]) {
        errorMsg.append("All precollimator incident angle dimensions must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allscatteringAngleStartValues[iSeg][currPcolColID] != allscatteringAngleStartValues[0][firstPcolColID]) {
        errorMsg.append("All precollimator scattering angle start values, in keyword 1CRVLnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allscatteringAngleDeltaValues[iSeg][currPcolColID] != allscatteringAngleDeltaValues[0][firstPcolColID]) {
        errorMsg.append("All precollimator scattering angle delta values, in keyword 1CDLTnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allincidentAngleStartValues[iSeg][currPcolColID] != allincidentAngleStartValues[0][firstPcolColID]) {
        errorMsg.append("All precollimator incident angle start values, in keyword 2CRVLnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allincidentAngleDeltaValues[iSeg][currPcolColID] != allincidentAngleDeltaValues[0][firstPcolColID]) {
        errorMsg.append("All precollimator incident angle delta values, in keyword 2CDLTnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
    }
    
    // grab a front column id from this extension to compare against for all
    // front columns
    currFrontColID = refFrontColIdx[iSeg];
    
    AH_DEBUG << "currFrontColID = " << currFrontColID << std::endl;
    AH_DEBUG << "currBackColID = " << currBackColID << std::endl;
    AH_DEBUG << "currPcolColID = " << currPcolColID << std::endl;
    
    for (int iCol = 0 ; iCol < scat.m_numColumns[iSeg] ; ++iCol) {
      
//      AH_DEBUG << "iSeg = "<< iSeg << "   iCol = " << iCol << std::endl;
      
      // if there is a back and/or pcol, we don't check those columns against
      // front columns
      if ( backColumnExists && (currBackColID == iCol) ) { continue; }
      if ( pcolColumnExists && (currPcolColID == iCol) ) { continue; }
        
      // make sure all front columns have the same scattering dimensions
      if (allnumscatteringangles[iSeg][iCol] != allnumscatteringangles[iSeg][currFrontColID]) {
        errorMsg.append("All front-side scattering angle dimensions must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      // make sure all front columns have the same incident dimensions
      if (allnumincidentangles[iSeg][iCol] != allnumincidentangles[iSeg][currFrontColID]) {
        errorMsg.append("All front-side incident angle dimensions must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allscatteringAngleStartValues[iSeg][iCol] != allscatteringAngleStartValues[iSeg][currFrontColID]) {
        errorMsg.append("All front-side scattering angle start values, in keyword 1CRVLnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allscatteringAngleDeltaValues[iSeg][iCol] != allscatteringAngleDeltaValues[iSeg][currFrontColID]) {
        errorMsg.append("All front-side scattering angle delta values, in keyword 1CDLTnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allincidentAngleStartValues[iSeg][iCol] != allincidentAngleStartValues[iSeg][currFrontColID]) {
        errorMsg.append("All front-side incident angle start values, in keyword 2CRVLnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
      if (allincidentAngleDeltaValues[iSeg][iCol] != allincidentAngleDeltaValues[iSeg][currFrontColID]) {
        errorMsg.append("All front-side incident angle delta values, in keyword 2CDLTnn, must match.");
        AH_THROW_RUNTIME(errorMsg);
      }
        
    }
  }
  
  AH_DEBUG << "+++ debugging" << std::endl;
//  AH_THROW_RUNTIME("+++ debugging");
  
  // now store some front mirror and rough surface data in the struct
  // All of the front scattering and incident columns should be the same across 
  // columns and extensions, so just grab one from the first extension
  currFrontColID = refFrontColIdx[0];
  scat.m_firstIncidentAngle = allincidentAngleStartValues[0][currFrontColID];
  scat.m_deltaIncidentAngle = allincidentAngleDeltaValues[0][currFrontColID];
  scat.m_numIncidentAngles = allnumincidentangles[0][currFrontColID];
  scat.m_firstScatteringAngle = allscatteringAngleStartValues[0][currFrontColID];
  scat.m_deltaScatteringAngle = allscatteringAngleDeltaValues[0][currFrontColID];
  scat.m_numScatteringAngles = allnumscatteringangles[0][currFrontColID];
  
  if (scat.m_roughScatID > 0) {
    // All of the rough scattering and incident columns should be the same 
    // across extensions, so just grab the first one
    // m_roughScatID is from the first extension
    scat.m_firstRoughIncidentAngle = allincidentAngleStartValues[0][scat.m_roughScatID];
    scat.m_deltaRoughIncidentAngle = allincidentAngleDeltaValues[0][scat.m_roughScatID];
    scat.m_numRoughIncidentAngles = allnumincidentangles[0][scat.m_roughScatID];
    scat.m_firstRoughScatteringAngle = allscatteringAngleStartValues[0][scat.m_roughScatID];
    scat.m_deltaRoughScatteringAngle = allscatteringAngleDeltaValues[0][scat.m_roughScatID];
    scat.m_numRoughScatteringAngles = allnumscatteringangles[0][scat.m_roughScatID];
  }
  
  // Now set up two sets of incident and scattering angle arrays. The first set 
  // is for the front-side scattering and the second set is for the back and 
  // pre-collimator scattering. 
  // For the front-side, we only use the results from segment 1, column 2 
  // (which is index 0 in the column arrays because we didn't include the 
  // energy array); the others should be the same. 
  // Likewise, for the back and pre-collimator we only use the 1st value in 
  // the 1st segment.
  
  // SCATTERING ANGLES
  scatStart = scat.m_firstScatteringAngle;
  scatDelta = scat.m_deltaScatteringAngle;
  scatHalfDelta = 0.5 * scatDelta;
  scat.m_scatteringAngles.resize(scat.m_numScatteringAngles);
  for (int iScat = 0 ; iScat < scat.m_numScatteringAngles ; iScat++ ) {
    // The number of valid angle boundaries for the cumulative array will be 
    // one more than the number of angle bins and the boundaries are shifted 
    // relative to the bin centers for the cumulative arrays. The additional 
    // angle bin (for cumulative probability=0.0) depends on where the valid 
    // cumulative probabilities start so will be generated later.
    scat.m_scatteringAngles[iScat] = scatStart + (iScat * scatDelta) + scatHalfDelta;
  }

  /* testing that I created front scattering angle grid correctly */
  #ifdef DEBUG
  #endif  
  AH_DEBUG << "*** testing that I created front scattering angle grid correctly ***" << std::endl;
  AH_DEBUG << "scat.m_firstScatteringAngle = " << scat.m_firstScatteringAngle << std::endl;
  AH_DEBUG << "scat.m_deltaScatteringAngle = " << scat.m_deltaScatteringAngle << std::endl;
  AH_DEBUG << "scat.m_numScatteringAngles = " << scat.m_numScatteringAngles << std::endl;
//  for (int iScat = 0 ; iScat < scat.m_numScatteringAngles ; iScat++ ) {
//    AH_DEBUG << "scat.m_scatteringAngles[" << iScat << "] = " << scat.m_scatteringAngles[iScat] << std::endl;
//  }
  
  // INCIDENT ANGLES
  incStart = scat.m_firstIncidentAngle;
  incDelta = scat.m_deltaIncidentAngle;
  scat.m_incidentAngles.resize(scat.m_numIncidentAngles);
  for (int iInc = 0 ; iInc < scat.m_numIncidentAngles ; ++iInc) {
    scat.m_incidentAngles[iInc] = incStart + (iInc * incDelta);
  }

  /* testing that I created front incident angle grid correctly */
  #ifdef DEBUG
  AH_DEBUG << "*** testing that I created front incident angle grid correctly ***" << std::endl;
  AH_DEBUG << "scat.m_firstIncidentAngle = " << scat.m_firstIncidentAngle << std::endl;
  AH_DEBUG << "scat.m_deltaIncidentAngle = " << scat.m_deltaIncidentAngle << std::endl;
  AH_DEBUG << "scat.m_numIncidentAngles = " << scat.m_numIncidentAngles << std::endl;
  for (int iInc = 0 ; iInc < scat.m_numIncidentAngles ; ++iInc) {
    AH_DEBUG << "scat.m_incidentAngles[" << iInc << "] = " << scat.m_incidentAngles[iInc] << std::endl;
  }
  #endif  
  
//  AH_THROW_RUNTIME("+++ debugging");
  
  // now do scattering and incident arrays for rough surfaces, if they exist
  if (backColumnExists || pcolColumnExists) {
    
    // SCATTERING ANGLES
    scatStart = scat.m_firstRoughScatteringAngle;
    scatDelta = scat.m_deltaRoughScatteringAngle;
    scatHalfDelta = 0.5 * scatDelta;
    scat.m_roughScatteringAngles.resize(scat.m_numRoughScatteringAngles);
    for (int iScat = 0 ; iScat < scat.m_numRoughScatteringAngles ; ++iScat) {
      // The number of valid angle boundaries for the cumulative array will be 
      // one more than the number of angle bins and the boundaries are shifted 
      // relative to the bin centers for the cumulative arrays. The additional 
      // angle bin (for cumulative probability=0.0) depends on where the valid 
      // cumulative probabilities start so will be generated later.
      scat.m_roughScatteringAngles[iScat] = scatStart + (iScat * scatDelta) + scatHalfDelta;
    }

    /* testing that I created rough scattering angle grid correctly */
    #ifdef DEBUG
    #endif  
    AH_DEBUG << "*** testing that I created rough scattering angle grid correctly ***" << std::endl;
    AH_DEBUG << "scat.m_firstRoughScatteringAngle = " << scat.m_firstRoughScatteringAngle << std::endl;
    AH_DEBUG << "scat.m_deltaRoughScatteringAngle = " << scat.m_deltaRoughScatteringAngle << std::endl;
    AH_DEBUG << "scat.m_numRoughScatteringAngles = " << scat.m_numRoughScatteringAngles << std::endl;
    for (int iScat = 0 ; iScat < scat.m_numRoughScatteringAngles ; iScat++ ) {
      AH_DEBUG << "scat.m_roughScatteringAngles[" << iScat << "] = " << scat.m_roughScatteringAngles[iScat] << std::endl;
    }
    
    // INCIDENT ANGLES
    incStart = scat.m_firstRoughIncidentAngle;
    incDelta = scat.m_deltaRoughIncidentAngle;
    scat.m_roughIncidentAngles.resize(scat.m_numRoughIncidentAngles);
    for (int iInc = 0 ; iInc < scat.m_numRoughIncidentAngles ; ++iInc) {
      scat.m_roughIncidentAngles[iInc] = incStart + (iInc * incDelta);
    }
    
    /* testing that I created rough incident angle grid correctly */
    #ifdef DEBUG
    AH_DEBUG << "*** testing that I created rough incident angle grid correctly ***" << std::endl;
    AH_DEBUG << "scat.m_firstRoughIncidentAngle = " << scat.m_firstRoughIncidentAngle << std::endl;
    AH_DEBUG << "scat.m_deltaRoughIncidentAngle = " << scat.m_deltaRoughIncidentAngle << std::endl;
    AH_DEBUG << "scat.m_numRoughIncidentAngles = " << scat.m_numRoughIncidentAngles << std::endl;
    for (int iInc = 0 ; iInc < scat.m_numRoughIncidentAngles ; ++iInc) {
      AH_DEBUG << "scat.m_roughIncidentAngles[" << iInc << "] = " << scat.m_roughIncidentAngles[iInc] << std::endl;
    }
    #endif 
    
  }
  
  // Now fill in the probability 
  // Loop over extensions (should be 1 per segment) and columns to fill in 
  // cumulative probability array
  for (int iSeg = 0 ; iSeg < scat.m_numSegments ; ++iSeg) {
    
    // go to this extension (first extension, number 1, is primary.  go to 2)
    fits_movabs_hdu(scat_fp, iSeg+2, 0, &status);// +++ valgrind
    checkForFITSError(status, "moving to extension in", scatterfilename);

    // loop through all the columns in this extension
    for (int iCol = 0 ; iCol < scat.m_numColumns[iSeg] ; ++iCol) {
      currColNum = iCol + colOffset;
      
      numScat = allnumscatteringangles[iSeg][iCol];
      numInc = allnumincidentangles[iSeg][iCol];
      
      // calculate appropriate probability array size for this column
      probArraySize = numScat * numInc;
      tempScatProb.clear();
      tempScatProb.resize(probArraySize);
      
      // loop over rows (energy)
      for (int iEnergy = 0 ; iEnergy < scat.m_numscatenergies ; ++iEnergy) {
        // rows are 1-based
        currRowNum = iEnergy + 1;
        
        // Read the scattering probability array into 1dim tempScatProb
        // the units of the scattering probability in the file are probability 
        // per radian so the values in the file should be multiplied by bin width (later)
        fits_read_col_dbl(scat_fp, currColNum, currRowNum, 1, probArraySize, 0, &(tempScatProb[0]), 0, &status);
        checkForFITSError(status, "reading scattering probability data in", scatterfilename);
        
        // now that we've read the data, fill in the cumulative 5dim array
        for (int iInc = 0 ; iInc < numInc ; ++iInc) {
          
          tempCumScatProb = 0.0;
          
          // pointers to the 1st and last non-zero scattering probabilities
          // +++ isn't this unnecessary, since these are initialized to -1?
          scat.m_firstNonZeroScatProb[iSeg][iCol][iEnergy][iInc] = -1;
          scat.m_lastNonZeroScatProb[iSeg][iCol][iEnergy][iInc] = -1;
          
          // num zero-valued probabilities in given scattering array slice
          zeroValuesCounter = 0;
          
          for (int iScat = 0 ; iScat < numScat ; ++iScat) {
            
            // we want tempScatProb(iScat,iInc): calculate index in 1D
            tempScatProbIdx = numScat*iInc + iScat;
            
            // once we reach an actual value, store which index this is
            if (tempScatProb[tempScatProbIdx] > 0.0) {
              
              // this will keep updating until the end of the loop, so the 
              // last value stored will be the last entry
              scat.m_lastNonZeroScatProb[iSeg][iCol][iEnergy][iInc] = iScat;
              
              if (scat.m_firstNonZeroScatProb[iSeg][iCol][iEnergy][iInc] < 0) {
                
                // before assigning this index to firstNonZeroScatProb, 
                // make sure it's a valid index.  We don't want a file without
                // any buffer '-1' values between angle bins, so make sure this
                // isn't the first value in this angle bin
                if (iScat == 0) {
                  errorMsg = "  Extension ";
                  errorMsg.append(intToString(iSeg+2));
                  errorMsg.append(", column ");
                  errorMsg.append(intToString(currColNum));
                  errorMsg.append(" does not have any '-1' values to pad before the angle bins for incident angle bin ");
                  errorMsg.append(intToString(iInc+1));
                  AH_THROW_RUNTIME(errorMsg);
                }
                scat.m_firstNonZeroScatProb[iSeg][iCol][iEnergy][iInc] = iScat;
              }
              
              // Only count non-zero and positive values in the scattering arrays: the format should be such that a consecutive sequence of positive values (probabilities) is enclosed by a zero-valued bin below the lowest valid scattering angle, and above the highest valid scattering angle. All other arrays values should be negative (indicating a scattering probability that is not defined and/or unphysical).
              // Note that although each slice of scattering probability should be normalized to 1.0, this is not checked.
              // Make sure cumscatprob is properly zeroed because the angle value for the first value for each scattering array slice where the cumulative probability should be zero is only calculated later in getscattereddirection().
              tempCumScatProb += tempScatProb[tempScatProbIdx] * allscatteringAngleDeltaValues[iSeg][iCol];
              scat.m_cumScatDist[iSeg][iCol][iEnergy][iScat][iInc] = tempCumScatProb; 
              
            } else if (tempScatProb[tempScatProbIdx] == 0.0) {
              zeroValuesCounter++;
            }
            
          } // end scattering angle loop
          
          // Reject file and abort if >2 zero-valued probabilities are found
          if (zeroValuesCounter > 2) {
            AH_THROW_RUNTIME("Incorrect scattering file format (too many zeros)");
          }
          
        } // end incident angle loop
        
      } // end energy (row) loop
      
    } // end column loop
    
  } // end segment (extension) loop
  
  
  // make sure that the lastNonZeroScatProb index isn't the last one.  
  // ie: make sure there is a 0 then a padding of '-1's after the values
  for (int iSeg = 0 ; iSeg < scat.m_numSegments ; ++iSeg) {
    for (int iCol = 0 ; iCol < scat.m_numColumns[iSeg] ; ++iCol) {
      numScat = allnumscatteringangles[iSeg][iCol];
      for (int iEnergy = 0 ; iEnergy < scat.m_numscatenergies ; ++iEnergy) {
        for (int iInc = 0 ; iInc < allnumincidentangles[iSeg][iCol] ; ++iInc) {
          if (scat.m_lastNonZeroScatProb[iSeg][iCol][iEnergy][iInc] == numScat) {
            errorMsg = "The scattering file is formatted incorrectly.  Extension ";
            errorMsg.append(intToString(iSeg+2));
            errorMsg.append(", column ");
            errorMsg.append(intToString(iCol+colOffset));
            errorMsg.append(" does not have any '0' then '-1' values to pad after the angle bins for incident angle bin ");
            errorMsg.append(intToString(iInc+1));
            AH_THROW_RUNTIME(errorMsg);
          }
        }
      }
    }
  }
  
  /*
  for (int iSeg = 0 ; iSeg < scat.m_numSegments ; ++iSeg) {
    for (int iCol = 0 ; iCol < scat.m_numColumns[iSeg] ; ++iCol) {
      for (int iEnergy = 0 ; iEnergy < scat.m_numscatenergies ; ++iEnergy) {
        for (int iInc = 0 ; iInc < allnumincidentangles[iSeg][iCol] ; ++iInc) {
          AH_DEBUG << "scat.m_firstNonZeroScatProb["<<iSeg<<"]["<<iCol<<"]["<<iEnergy<<"]["<<iInc<<"] = " << 
                      scat.m_firstNonZeroScatProb[iSeg][iCol][iEnergy][iInc] << std::endl;
          AH_DEBUG << "scat.m_lastNonZeroScatProb ["<<iSeg<<"]["<<iCol<<"]["<<iEnergy<<"]["<<iInc<<"] = " << 
                      scat.m_lastNonZeroScatProb[iSeg][iCol][iEnergy][iInc] << std::endl;
        }
      }
    }
  }
  */
  
} // end readScatteringFile()


/******************************************************************************/


void readReflectTrans(Param & param, 
                      const Photons & photons,
                      long numGroups, 
                      const std::vector<std::string> & frontRefColNames,
                      ReflectTransGrids & reflectTransGrids, 
                      std::vector<int> & xrtShellToGroupNumber,
                      bool & doTransmission, 
                      bool & isMultiLayerSurface) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * refl_fp;                   // reflectivity data file
  int status = 0;                       // for cfitsio function calls
  
  int angleColNum = 0;
  
  energyUnits_e energyUnit;             // what units energy is in file
  int energyColNum = 0;                 // column number for Energy column
  long firstNeededRow = 0;              // first row to grab, inside enrg range
  long lastNeededRow = 0;               // last row to grab, inside enrgy range
  int currRow = 0;                      // for looping through the rows
  
  int numFrontRefCols = frontRefColNames.size();
  std::string currTranColumnName;
  std::vector<std::string> m_frontTranColNames;
  std::string currColName;
  int currColNum = 0;
  std::string currKeywordName;          // getting SUBSTR, SUBTHK keywords
  
  char value[FLEN_VALUE] = "";          // getting TFORMn keyword
  char keyname[FLEN_KEYWORD] = "";      // getting TFORMn keyword
  
  long keywordLong;                     // to check the keywords
  int isMultiLayerSurfaceInt = 0;       // to get the MULTLAYR keyword
  
  // for getting string keywords
  char * telescop_c;
  telescop_c    = (char *) malloc(80*sizeof(char));
  char * instrume_c;
  instrume_c    = (char *) malloc(80*sizeof(char));
  
  // MASS_ABSORTION information
  // number of materials that have a column for mass-absorption coefficients 
  long numMaterials = 0;
  long numSubstrates = 0;
  // The index in substrateIndices refers to the column number in the mass 
  // absorption extension. Index 2 refers to the second column, which is
  // mabscoef1 for substrate 1, etc.
  std::vector<int> substrateIndices;
  std::vector<double> substrateThickness;
  double totalSubstrateThickness = 0.0;
  
  // PRECOLLIMATOR variables
  long pcolMaterialIndex = 0;
  
  std::string errorMsg;
  
  // -------------------------------------
  
  // open the file to the specified extension
  fits_open_file(&refl_fp, (param.m_frontreffile).c_str(), READONLY, &status);
  checkForFITSError(status, "opening", param.m_frontreffile);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_frontreffile << std::endl;

  // -------------------------------------
  //    reflectivity extension
  // -------------------------------------
  
  // error checking that TELESCOP and INSTRUME (user input params) match this 
  // file, in case the user supplied the file instead of CALDB
  fits_read_key_str(refl_fp, "TELESCOP", telescop_c, NULL, &status);
  fits_read_key_str(refl_fp, "INSTRUME", instrume_c, NULL, &status);
  if (!isEqualCaseInsens(param.m_telescop, (std::string)telescop_c)) {
    AH_THROW_RUNTIME("TELESCOP keyword in front reflectivity file (" + 
                     (std::string)telescop_c + ", in file " + param.m_frontreffile + 
                     ") and input parameter 'telescop' (" + param.m_telescop + ") must match");
  }
  if (!isEqualCaseInsens(param.m_instrume, (std::string)instrume_c)) {
    std::string errMsg = "INSTRUME keyword in front reflectivity file (" + 
                     (std::string)instrume_c + ", in file " + param.m_frontreffile + 
                     ") and input parameter 'instrume' (" + param.m_instrume + ") do not match";
    AH_INFO(ahlog::HIGH) << errMsg << std::endl;
    // +++ 20150909 KLR warning, not error, so that tests with old files will work.
//    AH_THROW_RUNTIME("INSTRUME keyword in front reflectivity file (" + 
//                     (std::string)instrume_c + ", in file " + param.m_frontreffile + 
//                     ") and input parameter 'instrume' (" + param.m_instrume + ") must match");
  }
  // free the char * we used to get the string keywords from cfitsio
  free(telescop_c);     telescop_c=0;
  free(instrume_c);     instrume_c=0;
   
  // make sure NGROUPS keyword is same as what we found in the surface file,
  // and same as size of the front column names vector
  fits_read_key_lng(refl_fp, const_cast<char *>("NGROUPS"), &keywordLong, NULL, &status);
  checkForFITSError(status, "reading keyword NGROUPS in", param.m_frontreffile);
  if ( (numFrontRefCols != numGroups) || (keywordLong != numGroups) ) {
    errorMsg = "NGROUPS keywords in the reflectivity file ";
    errorMsg.append(param.m_frontreffile);
    errorMsg.append(" and in the telescope description file "); 
    errorMsg.append(param.m_mirrorfile);
    errorMsg.append(" do not match.");
    errorMsg.append(" ABORTING.");
    AH_DEBUG << errorMsg << std::endl;
    AH_THROW_RUNTIME(errorMsg);
  }
  
  fits_read_key_log(refl_fp, const_cast<char *>("MULTLAYR"), &isMultiLayerSurfaceInt, NULL, &status);
  checkForFITSError(status, "reading keyword MULTLAYR in", param.m_frontreffile);
  isMultiLayerSurface = ( (isMultiLayerSurfaceInt==1) ? true : false);
  
  fits_get_colnum(refl_fp, CASEINSEN, const_cast<char *>("ENERGY"), &energyColNum, &status);
  checkForFITSError(status, "getting energy column number", param.m_frontreffile);
  
  energyUnit = getEnergyUnit(refl_fp, param.m_frontreffile, energyColNum);
  getRowsInEnergyRange(refl_fp, param.m_frontreffile, photons, energyUnit, 
                       energyColNum, firstNeededRow, lastNeededRow);
  
  AH_DEBUG << "firstNeededRow = " << firstNeededRow << std::endl;
  AH_DEBUG << "lastNeededRow = " << lastNeededRow << std::endl;
  
  reflectTransGrids.m_numFrontEnergies = lastNeededRow - firstNeededRow + 1;
  reflectTransGrids.m_frontReflEnergies.resize(reflectTransGrids.m_numFrontEnergies);
  
  // get angle column number
  fits_get_colnum(refl_fp, CASEINSEN, const_cast<char *>("ANGLE"), &angleColNum, &status);
  checkForFITSError(status, "getting angle column number", param.m_frontreffile);
  
  // see how many angles in the angle grid
  sprintf(keyname, "TFORM%d", angleColNum);
  fits_read_keyword(refl_fp, keyname, value, 0, &status);
  checkForFITSError(status, "getting keyword information", param.m_frontreffile);
  // if value is of bad form or empty i.e., 1PE, 1PE(, 1PE(A, 0 is returned
  std::istringstream iss(value);
  iss.ignore(FLEN_KEYWORD,'\'');
  iss >> value;
  reflectTransGrids.m_numFrontAngles = std::strtol(value,0,10);
  reflectTransGrids.m_frontAngles.resize(reflectTransGrids.m_numFrontAngles);
  
  // store first row of Angle column into this array 
  // (every row has same vector in it)
  fits_read_col(refl_fp, TDOUBLE, angleColNum, 1, 1, reflectTransGrids.m_numFrontAngles, 0, &(reflectTransGrids.m_frontAngles[0]), 0, &status);
  checkForFITSError(status, "reading angle column", param.m_frontreffile);
  
  /* testing that I got angle grid data */
  #ifdef DEBUG
  AH_DEBUG << "*** testing that I got angle grid data ***" << std::endl;
  AH_DEBUG << "m_numFrontAngles = " << reflectTransGrids.m_numFrontAngles << std::endl;
  for (int i = 0 ; i < reflectTransGrids.m_numFrontAngles ; ++i) { 
    AH_DEBUG << "angle[" << i << "] = " << reflectTransGrids.m_frontAngles[i] << std::endl;
  }
  #endif

  // frontRefColNames should have numGroups strings corresponding to column names 
  // in this extension for the front-side reflectivity. Create another array, 
  // m_frontTranColNames for the column names for the front-side transmission 
  // data by replacing the first 3 characters in the name by "Tran" so the 
  // column names will be in the form TranProbnn.
  for (std::vector<std::string>::const_iterator iter = frontRefColNames.begin() ; 
       iter != frontRefColNames.end(); ++iter) {
    currTranColumnName = *iter;
    currTranColumnName.replace(0, 3, "Tran");
    m_frontTranColNames.push_back(currTranColumnName);
  }
  
  /* testing that I created TranProbnn columns correctly */
  #ifdef DEBUG
  AH_DEBUG << "*** testing that I created TranProbnn columns correctly ***" << std::endl;
  for (std::vector<std::string>::iterator iter = m_frontTranColNames.begin() ; 
       iter != m_frontTranColNames.end(); ++iter) {
    AH_DEBUG << *iter << std::endl;
  }
  #endif
  
  reflectTransGrids.m_frontRefl.resize(reflectTransGrids.m_numFrontEnergies);
  reflectTransGrids.m_frontTran.resize(reflectTransGrids.m_numFrontEnergies);
  
  // reindex the refl and trans probabilities into new arrays
  for (int iRow = 0 ; iRow < reflectTransGrids.m_numFrontEnergies ; ++iRow) {
    currRow = iRow + firstNeededRow;
    
    // add energy value from Energy column to m_frontReflEnergies array
    fits_read_col(refl_fp, TDOUBLE, energyColNum, currRow, 1, 1, 0, &(reflectTransGrids.m_frontReflEnergies[iRow]), 0, &status);
    checkForFITSError(status, "reading energy column", param.m_frontreffile);
    
    reflectTransGrids.m_frontRefl[iRow].resize(reflectTransGrids.m_numFrontAngles);
    reflectTransGrids.m_frontTran[iRow].resize(reflectTransGrids.m_numFrontAngles);
    
    // now get the reflectivity and transmission
    for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numFrontAngles ; ++iAngle) {
      
      reflectTransGrids.m_frontRefl[iRow][iAngle].resize(numGroups);
      reflectTransGrids.m_frontTran[iRow][iAngle].resize(numGroups);
      
      for (int iGroup = 0 ; iGroup < numGroups ; ++iGroup) {
        
        currColName = frontRefColNames[iGroup];
        fits_get_colnum(refl_fp, CASEINSEN, const_cast<char *>(currColName.c_str()), &currColNum, &status);
        checkForFITSError(status, "getting reflectivity column number", param.m_frontreffile);
        fits_read_col(refl_fp, TDOUBLE, currColNum, currRow, iAngle+1, 1, 0, &(reflectTransGrids.m_frontRefl[iRow][iAngle][iGroup]), 0, &status);
        checkForFITSError(status, "reading reflectivity column", param.m_frontreffile);
         
        currColName = m_frontTranColNames[iGroup];
        fits_get_colnum(refl_fp, CASEINSEN, const_cast<char *>(currColName.c_str()), &currColNum, &status);
        checkForFITSError(status, "getting transmission column number", param.m_frontreffile);
        fits_read_col(refl_fp, TDOUBLE, currColNum, currRow, iAngle+1, 1, 0, &(reflectTransGrids.m_frontTran[iRow][iAngle][iGroup]), 0, &status);
        checkForFITSError(status, "reading transmission column", param.m_frontreffile);
        
      }
    }
  }
  storeEnergyInkeV(energyUnit, reflectTransGrids.m_frontReflEnergies);
  
  
  /* testing that I created m_frontReflEnergies, m_frontRefl, and m_frontTran correctly */
  #ifdef DEBUG
  #endif 
  AH_DEBUG << "*** testing that I created m_frontReflEnergies, m_frontRefl, and m_frontTran correctly ***" << std::endl;
  AH_DEBUG << "reflectTransGrids.m_numFrontEnergies = " << reflectTransGrids.m_numFrontEnergies << std::endl;
  AH_DEBUG << "reflectTransGrids.m_frontReflEnergies.size() = " << reflectTransGrids.m_frontReflEnergies.size() << std::endl;
  for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
    AH_DEBUG << "m_frontReflEnergies[" << iEnergy << "] = " << reflectTransGrids.m_frontReflEnergies[iEnergy] << std::endl;
  }
//  for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
//    for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numFrontAngles ; ++iAngle) {
//      for (int iGroup = 0 ; iGroup < numGroups ; ++iGroup) {
//        AH_DEBUG << "m_frontRefl[" << iEnergy << "][" << iAngle << "][" << iGroup << "] = " << reflectTransGrids.m_frontRefl[iEnergy][iAngle][iGroup] << std::endl;
//      }
//    }
//  }
//  for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
//    for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numFrontAngles ; ++iAngle) {
//      for (int iGroup = 0 ; iGroup < numGroups ; ++iGroup) {
//        AH_DEBUG << "m_frontTran[" << iEnergy << "][" << iAngle << "][" << iGroup << "] = " << reflectTransGrids.m_frontTran[iEnergy][iAngle][iGroup] << std::endl;
//      }
//    }
//  }

  // -------------------------------------
  //    mass absorption data
  // -------------------------------------
  
  // see if the user requested transmission
  if (isEqualCaseInsens(param.m_transmode, "none")) {
    doTransmission = false;
  } else {
    doTransmission = true;
  }
  
  
  if (doTransmission || param.m_doExternalObjects) {
    // While we're still in the refl extension, read the keyword NMATERIA for 
    // the number of materials that have a column for their mass-absorption 
    // coefficients.
    fits_read_key(refl_fp, TLONG, "NMATERIA", &reflectTransGrids.m_numMaterials, 0, &status);
    // if the keyword wasn't found, don't do the mass absorption
    if (status == VALUE_UNDEFINED) {
      doTransmission = false;
      status = 0;
    }
  }
  
  if (doTransmission || param.m_doExternalObjects) {
        
    // Also read the number of substrate layers, NSUBSTRA (thick materials)
    // in the mirror foils below the surface coating, and an index that
    // points to the correct mass-absorption coefficient array for each
    // substrate layer material
    fits_read_key(refl_fp, TLONG, "NSUBSTRA", &numSubstrates, 0, &status);
    // if the keyword wasn't found, don't do the mass absorption
    if (status == VALUE_UNDEFINED) {
      doTransmission = false;
      status = 0;  
    } else {
      substrateIndices.resize(numSubstrates);
      substrateThickness.resize(numSubstrates);
      
      for (int iSubstrate = 0 ; iSubstrate < numSubstrates ; ++iSubstrate) {
        currKeywordName = "SUBSTR";
        currKeywordName.append(intToString(iSubstrate+1));
        fits_read_key(refl_fp, TINT, const_cast<char *>(currKeywordName.c_str()), &(substrateIndices[iSubstrate]), 0, &status);
        checkForFITSError(status, "getting SUBSTRn keywords", param.m_frontreffile);
        
        currKeywordName = "SUBTHK";
        currKeywordName.append(intToString(iSubstrate+1));
        fits_read_key(refl_fp, TDOUBLE, const_cast<char *>(currKeywordName.c_str()), &(substrateThickness[iSubstrate]), 0, &status);
        checkForFITSError(status, "getting SUBTHKn keywords", param.m_frontreffile);
        
        totalSubstrateThickness += substrateThickness[iSubstrate];
      }
      
    }
  }
  
  // If a pre-collimator exists then read the index in the header which 
  // points to which mass-absorption coefficient array to use for it
  // 'index' here really means column number.  The reflectivity files from 
  // the tool xtreftable write the energy column first, then the mass 
  // absorption coefficient columns.
  if (param.m_pcolExists) {
    fits_read_key(refl_fp, TLONG, "PCOLMTRL", &pcolMaterialIndex, 0, &status);
    checkForFITSError(status, "reading PCOLMTRL keyword", param.m_frontreffile);
  }
        
  if (doTransmission || param.m_doExternalObjects) {
    // if an extension called MASS_ABSORPTION exists in the file, go there
    fits_movnam_hdu(refl_fp, ANY_HDU, const_cast<char *>(s_massAbsExtName.c_str()), 0, &status);
    // if it doesn't exist, skip next block of code
    if (status == BAD_HDU_NUM) {
      doTransmission = false;
      param.m_doExternalObjects = false;
      status = 0;
    }
  }
  
  // Only do the mass-absorption section if the user wants transmission, 
  // and we found necessary keywords and extension
  // We need to do the mass-absorption coeffients if either dotransmission=true 
  // OR if doexternalobjects=True because we need the transmission for the 
  // thermal shield
  if (doTransmission || param.m_doExternalObjects) {
    getMassAbsorptionData(refl_fp, param, numMaterials, numGroups,
                          numSubstrates, substrateIndices, substrateThickness,
                          totalSubstrateThickness, pcolMaterialIndex,
                          firstNeededRow, reflectTransGrids, xrtShellToGroupNumber);
  } // end-if doTransmission
  
  // close file
  fits_close_file(refl_fp, &status);
  checkForFITSError(status, "closing", param.m_frontreffile);
  
  // back-side mirror data
  getMirrorBackData(param, photons, reflectTransGrids);
  
  // pre-collimator data
  if (param.m_pcolExists) {
    getMirrorPcolData(param, photons, reflectTransGrids);
  }
  
}   // end readReflectTrans()


/******************************************************************************/


void getMassAbsorptionData(fitsfile * refl_fp, 
                           Param & param, 
                           int numMaterials, 
                           long numGroups, 
                           int numSubstrates, 
                           const std::vector<int> & substrateIndices, 
                           const std::vector<double> & substrateThickness,
                           double totalSubstrateThickness, 
                           int pcolMaterialIndex, 
                           long firstNeededRow, 
                           ReflectTransGrids & reflectTransGrids, 
                           std::vector<int> & xrtShellToGroupNumber) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;                       // for cfitsio function calls
  
  // reference to the mass absorption coefficients vector [cm^2/g]
  // dimen: (numFrontEnergies,numMaterials)
  vector2Ddbl & massAbs = reflectTransGrids.m_massAbsCoeff;
  
  std::stringstream currKeywordNumStrStr;
  std::string currKeywordNumStr;
  std::string currKeywordName;
  std::string currColName;
  
  int currRow = 0;                          // iterating through fits rows
  int currColNum = 0;                       // iterating through fits columns
  std::vector<double> materialDensity(reflectTransGrids.m_numMaterials);       // density of each material
  std::vector<std::string> materialNames(reflectTransGrids.m_numMaterials);    // name of each material
  std::vector<double> opticalDepthFactor(numSubstrates);
  
  // storing data from TDF SURFACE extension
  TDF_Surface surf;
  
  // -------------------------------------
  
  // we moved to MASS_ABSORPTION extension before coming into this function.
  // if the extension didn't exist, this routine would not be called.
  
  // Calculate the optical depths per mm for rough/thick material transmission
  // For the moment we are going to assume that the energy grid for the mass-
  // absorption coefficients is identical to that for the front-side mirror 
  // reflectivity
  
  // The optical depth per mm for transmission from the back-side of the -
  // mirror foils to the bottom of the thin-film coating is obtained by 
  // summing the optical depth for each substrate. This sum is then 
  // normalized to the total thickness of all layers so that raytrace can 
  // then just multiply the optical depth per unit length by the photon 
  // path length to get the actual optical depth. However, here we simply
  // calculate the energy-independent part for each substrate layer.
  
  massAbs.resize(reflectTransGrids.m_numFrontEnergies);
  for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
    massAbs[iEnergy].resize(reflectTransGrids.m_numMaterials);
  }

  
  AH_DEBUG << "numMaterials = " << reflectTransGrids.m_numMaterials << std::endl;
  
  // Read mass-absorption coefficient data and the density for each material.
  // For all mirror foil materials (not the pre-collimator), we will now no 
  // longer use the density from the keywords but instead get it from the 
  // density column in the SURFACE and AZIMUTHALSTRUCT extensions of the TDF.
  // However, we will still need the density from the header keyword for the 
  // pre-collimator so read in all the density keywords anyway.
  for (int iMaterial = 0 ; iMaterial < reflectTransGrids.m_numMaterials ; ++iMaterial) {

    AH_DEBUG << "iMaterial = " << iMaterial << std::endl;
  
    // get the zero-padded keyword index for this material iteration
    currKeywordNumStrStr.str("");
    currKeywordNumStrStr.clear();
    // vector is 0-based, but fits file wants 1-based, so use iMaterial+1
    currKeywordNumStrStr << std::setw(2) << std::setfill('0') << iMaterial+1;
    currKeywordNumStr = currKeywordNumStrStr.str();
    
    // get the Density keywords
    currKeywordName = "DENSIT";
    currKeywordName.append(currKeywordNumStr);
    fits_read_key(refl_fp, TDOUBLE, const_cast<char *>(currKeywordName.c_str()), &(materialDensity[iMaterial]), 0, &status);
    checkForFITSError(status, "reading density keywords", param.m_frontreffile);
    
    
    // for getting string keywords MATERInn
    char * materi_c;
    materi_c = (char *) malloc(80*sizeof(char));
    // now get the material name keywords
    currKeywordName = "MATERI";
    currKeywordName.append(currKeywordNumStr);
    fits_read_key_str(refl_fp, const_cast<char *>(currKeywordName.c_str()), materi_c, NULL, &status);
    checkForFITSError(status, "reading MATERInn keywords", param.m_frontreffile);
    // store the name
    materialNames[iMaterial] = (std::string)materi_c;
    // free the char * we used to get the string keywords from cfitsio
    free(materi_c);  materi_c=0;
    
    
    // get the column number for the actual data
    currColName = "mabscoef";
    currColName.append(currKeywordNumStr);
    fits_get_colnum(refl_fp, CASEINSEN, const_cast<char *>(currColName.c_str()), &currColNum, &status);
    checkForFITSError(status, "getting mass absorption column number", param.m_frontreffile);
      
    // get the mass absorption coefficients
    for (int iRow = 0 ; iRow < reflectTransGrids.m_numFrontEnergies ; ++iRow) {
      currRow = iRow + firstNeededRow;
      fits_read_col(refl_fp, TDOUBLE, currColNum, currRow, 1, 1, 0, &(massAbs[iRow][iMaterial]), 0, &status);
      checkForFITSError(status, "reading mass absorption column", param.m_frontreffile);
    }
    
  }
  
  /* testing that I created mass absorption grid correctly */
  #ifdef DEBUG
  AH_DEBUG << "*** testing that I created mass absorption grid correctly ***" << std::endl;
  for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
    for (int iMaterial = 0 ; iMaterial < reflectTransGrids.m_numMaterials ; ++iMaterial) {
      AH_DEBUG << "massAbs[" << iEnergy << "]["<< iMaterial <<"] = " << massAbs[iEnergy][iMaterial] << std::endl;
    }
  }
  #endif
  
  // Next calculate the optical depth, noting:
  // (1) Different mirror groups have different thicknesses
  // (2) The optical depth of the mirror thin film surface going from back to 
  //     front now uses values calculated from information in the TDF SURFACE 
  //     extension
  // (3) Calculate optical depths for the external objects such as the thermal 
  //     shields, using information in the TDF AZIMUTHALSTRUCT extension
  
  // get information from the TDF SURFACE extension
  setupTDFSurface(param, numMaterials, numGroups, pcolMaterialIndex, materialDensity, materialNames, massAbs, surf, reflectTransGrids, xrtShellToGroupNumber);
  
  
  
  
} // end getMassAbsorptionData()


/******************************************************************************/


void getMirrorBackData(Param & param, 
                       const Photons & photons, 
                       ReflectTransGrids & reflectTransGrids) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * refl_fp;                   // reflectivity data file
  int status = 0;                       // for cfitsio function calls
  
  energyUnits_e energyUnit;               // what units energy is in file
  int energyColNum = 0;                 // column number for Energy column
  long firstNeededRow = 0;              // first row to grab, inside enrg range
  long lastNeededRow = 0;               // last row to grab, inside enrgy range
  int currRow = 0;                      // for looping through the rows
  
  char value[FLEN_VALUE] = "";          // getting TFORMn keyword
  char keyname[FLEN_KEYWORD] = "";      // getting TFORMn keyword
  
  // for getting string keywords
  char * telescop_c;
  telescop_c    = (char *) malloc(80*sizeof(char));
  char * instrume_c;
  instrume_c    = (char *) malloc(80*sizeof(char));
  
  // -------------------------------------
  
  // open the file
  fits_open_file(&refl_fp, (param.m_backreffile).c_str(), READONLY, &status);
  checkForFITSError(status, "opening", param.m_backreffile);
  //+++ there's no check that this extension (given in .par file) matches breflect column in TDF
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_backreffile << std::endl;
  
  // error checking that TELESCOP and INSTRUME (user input params) match this 
  // file, in case the user supplied the file instead of CALDB
  fits_read_key_str(refl_fp, "TELESCOP", telescop_c, NULL, &status);
  fits_read_key_str(refl_fp, "INSTRUME", instrume_c, NULL, &status);
  if (!isEqualCaseInsens(param.m_telescop, (std::string)telescop_c)) {
    AH_THROW_RUNTIME("TELESCOP keyword in back reflectivity file (" + 
                     (std::string)telescop_c + ", in file " + param.m_backreffile + 
                     ") and input parameter 'telescop' (" + param.m_telescop + ") must match");
  }
  if (!isEqualCaseInsens(param.m_instrume, (std::string)instrume_c)) {
    std::string errMsg = "INSTRUME keyword in back reflectivity file (" + 
                     (std::string)instrume_c + ", in file " + param.m_backreffile + 
                     ") and input parameter 'instrume' (" + param.m_instrume + ") do not match";
    AH_INFO(ahlog::HIGH) << errMsg << std::endl;
    // +++ 20150909 KLR warning, not error, so that tests with old files will work.
//    AH_THROW_RUNTIME("INSTRUME keyword in back reflectivity file (" + 
//                     (std::string)instrume_c + ", in file " + param.m_backreffile + 
//                     ") and input parameter 'instrume' (" + param.m_instrume + ") must match");
  }
  // free the char * we used to get the string keywords from cfitsio
  free(telescop_c);     telescop_c=0;
  free(instrume_c);     instrume_c=0;
   
  fits_get_colnum(refl_fp, CASEINSEN, const_cast<char *>("ENERGY"), &energyColNum, &status);
  checkForFITSError(status, "getting energy column number in", param.m_backreffile);
  
  energyUnit = getEnergyUnit(refl_fp, param.m_backreffile, energyColNum);
  getRowsInEnergyRange(refl_fp, param.m_backreffile, photons, energyUnit, 
                       energyColNum, firstNeededRow, lastNeededRow);
  
  reflectTransGrids.m_numBackEnergies = lastNeededRow - firstNeededRow + 1;
  reflectTransGrids.m_backEnergies.resize(reflectTransGrids.m_numBackEnergies);
  
  // get Angle data
  // In the back-side mirror extension there is no angle array, and no
  // keyword for number of angles. Each row of the file (for 1 energy) is an
  // array whose size is the number of angles. The delta and the first angle 
  // are in keywords.
  
  fits_read_key(refl_fp, TDOUBLE, "1CRVL2", &(reflectTransGrids.m_backAngleStart), 0, &status);
  checkForFITSError(status, "reading 1CRVL2 keyword in", param.m_backreffile);
  fits_read_key(refl_fp, TDOUBLE, "1CDLT2", &(reflectTransGrids.m_backAngleDelta), 0, &status);
  checkForFITSError(status, "reading 1CDLT2 keyword in", param.m_backreffile);
  sprintf(keyname, "TFORM%d", 2);
  fits_read_keyword(refl_fp, keyname, value, 0, &status);
  checkForFITSError(status, "getting keyword information", param.m_backreffile);
  // if value is of bad form or empty i.e., 1PE, 1PE(, 1PE(A, 0 is returned
  std::istringstream iss(value);
  iss.ignore(FLEN_KEYWORD,'\'');
  iss >> value;
  reflectTransGrids.m_numBackAngles = std::strtol(value,0,10);
  reflectTransGrids.m_backAngles.resize(reflectTransGrids.m_numBackAngles);
  for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numBackAngles ; ++iAngle) {
    reflectTransGrids.m_backAngles[iAngle] = reflectTransGrids.m_backAngleStart + 
                                      ( (double)iAngle * 
                                        reflectTransGrids.m_backAngleDelta );
  }
  
  /* testing that I created angle grid for mirror back correctly */
  #ifdef DEBUG
  AH_DEBUG << "*** testing that I created angle grid for mirror back correctly ***" << std::endl;
  for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numBackAngles ; ++iAngle) {
    AH_DEBUG << "m_backAngles[" << iAngle << "] = " << reflectTransGrids.m_backAngles[iAngle] << std::endl;
  }
  #endif
  
  // get Reflectivity data
  reflectTransGrids.m_backRefl.resize(reflectTransGrids.m_numBackEnergies);
  for (int iRow = 0 ; iRow < reflectTransGrids.m_numBackEnergies ; ++iRow) {
    currRow = iRow + firstNeededRow;
    
    // read currRow of fits file
    // add energy value from Energy column to BackEnergies array
    fits_read_col(refl_fp, TDOUBLE, energyColNum, currRow, 1, 1, 0, &(reflectTransGrids.m_backEnergies[iRow]), 0, &status);
    checkForFITSError(status, "reading energy column in", param.m_backreffile);
    
    reflectTransGrids.m_backRefl[iRow].resize(reflectTransGrids.m_numBackAngles);
    for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numBackAngles ; ++iAngle) {
      fits_read_col(refl_fp, TDOUBLE, 2, currRow, iAngle+1, 1, 0, &(reflectTransGrids.m_backRefl[iRow][iAngle]), 0, &status);
      checkForFITSError(status, "reading back reflectivity column in", param.m_backreffile);
    }
    
  }
  
  storeEnergyInkeV(energyUnit, reflectTransGrids.m_backEnergies);
  
  /* testing that I read energy and angle grid for mirror back correctly */
  AH_DEBUG << "*** testing that I created energy grid for mirror back correctly ***" << std::endl;
  AH_DEBUG << "reflectTransGrids.m_numBackEnergies = " << reflectTransGrids.m_numBackEnergies << std::endl;
  for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numBackEnergies ; ++iEnergy) {
    AH_DEBUG << "m_backEnergies[" << iEnergy << "] = " << reflectTransGrids.m_backEnergies[iEnergy] << std::endl;
  }
  #ifdef DEBUG
  AH_DEBUG << "*** testing that I created angle grid for mirror back correctly ***" << std::endl;
  for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numBackEnergies ; ++iEnergy) {
    for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numBackAngles ; ++iAngle) {
      AH_DEBUG << "m_backRefl[" << iEnergy << "][" << iAngle << "] = " << reflectTransGrids.m_backRefl[iEnergy][iAngle] << std::endl;
    }
  }
  #endif
  
  // close file
  fits_close_file(refl_fp, &status);
  checkForFITSError(status, "closing", param.m_backreffile);
  
} // end getMirrorBackData()


/******************************************************************************/


void getMirrorPcolData(Param & param, 
                       const Photons & photons,
                       ReflectTransGrids & reflectTransGrids) {
    
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * refl_fp;                   // reflectivity data file
  int status = 0;                       // for cfitsio function calls
  
  energyUnits_e energyUnit;             // what units energy is in file
  int energyColNum = 0;                 // column number for Energy column
  long firstNeededRow = 0;              // first row to grab, inside enrg range
  long lastNeededRow = 0;               // last row to grab, inside enrgy range
  int currRow = 0;                      // for looping through the rows
  
  char value[FLEN_VALUE] = "";          // getting TFORMn keyword
  char keyname[FLEN_KEYWORD] = "";      // getting TFORMn keyword
  
  // for getting string keywords
  char * telescop_c;
  telescop_c    = (char *) malloc(80*sizeof(char));
  char * instrume_c;
  instrume_c    = (char *) malloc(80*sizeof(char));
  
  // -------------------------------------
  
  // open the file
  fits_open_file(&refl_fp, (param.m_pcolreffile).c_str(), READONLY, &status);
  checkForFITSError(status, "opening", param.m_pcolreffile);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_pcolreffile << std::endl;
  
  // error checking that TELESCOP and INSTRUME (user input params) match this 
  // file, in case the user supplied the file instead of CALDB
  fits_read_key_str(refl_fp, "TELESCOP", telescop_c, NULL, &status);
  fits_read_key_str(refl_fp, "INSTRUME", instrume_c, NULL, &status);
  if (!isEqualCaseInsens(param.m_telescop, (std::string)telescop_c)) {
    AH_THROW_RUNTIME("TELESCOP keyword in pre-collimator reflectivity file (" + 
                     (std::string)telescop_c + ", in file " + param.m_pcolreffile + 
                     ") and input parameter 'telescop' (" + param.m_telescop + ") must match");
  }
  if (!isEqualCaseInsens(param.m_instrume, (std::string)instrume_c)) {
    std::string errMsg = "INSTRUME keyword in pre-collimator reflectivity file (" + 
                     (std::string)instrume_c + ", in file " + param.m_pcolreffile + 
                     ") and input parameter 'instrume' (" + param.m_instrume + ") do not match";
    AH_INFO(ahlog::HIGH) << errMsg << std::endl;
    // +++ 20150909 KLR warning, not error, so that tests with old files will work.
//    AH_THROW_RUNTIME("INSTRUME keyword in pre-collimator reflectivity file (" + 
//                     (std::string)instrume_c + ", in file " + param.m_pcolreffile + 
//                     ") and input parameter 'instrume' (" + param.m_instrume + ") must match");
  }
  // free the char * we used to get the string keywords from cfitsio
  free(telescop_c);     telescop_c=0;
  free(instrume_c);     instrume_c=0;
  
  fits_get_colnum(refl_fp, CASEINSEN, const_cast<char *>("ENERGY"), &energyColNum, &status);
  checkForFITSError(status, "getting energy column number in", param.m_backreffile);
  
  energyUnit = getEnergyUnit(refl_fp, param.m_backreffile, energyColNum);
  getRowsInEnergyRange(refl_fp, param.m_backreffile, photons, energyUnit, 
                       energyColNum, firstNeededRow, lastNeededRow);
  
  reflectTransGrids.m_numPcolEnergies = lastNeededRow - firstNeededRow + 1;
  reflectTransGrids.m_pcolEnergies.resize(reflectTransGrids.m_numPcolEnergies);
  
  // get Angle data
  // In the pre-collimator extension there is no angle array, and no
  // keyword for number of angles. Each row of the file (for 1 energy) is an
  // array whose size is the number of angles. The delta and the first angle 
  // are in keywords.
  
  fits_read_key(refl_fp, TDOUBLE, "1CRVL2", &(reflectTransGrids.m_pcolAngleStart), 0, &status);
  checkForFITSError(status, "reading 1CRVL2 keyword in", param.m_pcolreffile);
  fits_read_key(refl_fp, TDOUBLE, "1CDLT2", &(reflectTransGrids.m_pcolAngleDelta), 0, &status);
  checkForFITSError(status, "reading 1CDLT2 keyword in", param.m_pcolreffile);
  sprintf(keyname, "TFORM%d", 2);
  fits_read_keyword(refl_fp, keyname, value, 0, &status);
  checkForFITSError(status, "getting keyword information in", param.m_pcolreffile);
  // if value is of bad form or empty i.e., 1PE, 1PE(, 1PE(A, 0 is returned
  std::istringstream iss(value);
  iss.ignore(FLEN_KEYWORD,'\'');
  iss >> value;
  reflectTransGrids.m_numPcolAngles = std::strtol(value,0,10);
  
  // If the back-side mirror and pre-collimator angle grids aren't identical,
  // we cannot continue (with the current form of the code)
  if ( (reflectTransGrids.m_backAngleStart != reflectTransGrids.m_pcolAngleStart) || 
       (reflectTransGrids.m_backAngleDelta != reflectTransGrids.m_pcolAngleDelta) || 
       (reflectTransGrids.m_numBackAngles  != reflectTransGrids.m_numPcolAngles) ) {
    AH_THROW_RUNTIME("Angle grids for the back-sides of mirror foils and for pre-collimator foils must be identical");
  }
  
  reflectTransGrids.m_pcolAngles.resize(reflectTransGrids.m_numPcolAngles);
  for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numPcolAngles ; ++iAngle) {
    reflectTransGrids.m_pcolAngles[iAngle] = reflectTransGrids.m_pcolAngleStart + 
                                      ( (double)iAngle * 
                                        reflectTransGrids.m_pcolAngleDelta );
  }
  
  /* testing that I created angle grid for pre-collimator correctly */
  #ifdef DEBUG
  AH_DEBUG << "*** testing that I created angle grid for pre-collimatorcorrectly ***" << std::endl;
  for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numPcolAngles ; ++iAngle) {
    AH_DEBUG << "m_pcolAngles[" << iAngle << "] = " << reflectTransGrids.m_pcolAngles[iAngle] << std::endl;
  }
  #endif
  
  // get pre-collimator reflectivity data
  reflectTransGrids.m_pcolRefl.resize(reflectTransGrids.m_numPcolEnergies);
  for (int iRow = 0 ; iRow < reflectTransGrids.m_numPcolEnergies ; ++iRow) {
    currRow = iRow + firstNeededRow;
    
    // read currRow of fits file
    // add energy value from Energy column to BackEnergies array
    fits_read_col(refl_fp, TDOUBLE, energyColNum, currRow, 1, 1, 0, &(reflectTransGrids.m_pcolEnergies[iRow]), 0, &status);
    checkForFITSError(status, "reading pre-collimator energy column in", param.m_pcolreffile);
    
    reflectTransGrids.m_pcolRefl[iRow].resize(reflectTransGrids.m_numPcolAngles);
    for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numPcolAngles ; ++iAngle) {
      fits_read_col(refl_fp, TDOUBLE, 2, currRow, iAngle+1, 1, 0, &(reflectTransGrids.m_pcolRefl[iRow][iAngle]), 0, &status);
      checkForFITSError(status, "reading pre-collimator reflectivity column in", param.m_pcolreffile);
    }
    
  }
  
  storeEnergyInkeV(energyUnit, reflectTransGrids.m_pcolEnergies);
  
  /* testing that I read energy grid for pre-collimator correctly */
  #ifdef DEBUG
  AH_DEBUG << "*** testing that I created energy grid for pre-collimator correctly ***" << std::endl;
  for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numPcolEnergies ; ++iEnergy) {
    AH_DEBUG << "m_pcolEnergies[" << iEnergy << "] = " << reflectTransGrids.m_pcolEnergies[iEnergy] << std::endl;
  }
  // testing that I read reflectivity grid for pre-collimator correctly 
  AH_DEBUG << "*** testing that I created angle grid for pre-collimator correctly ***" << std::endl;
  for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numPcolEnergies ; ++iEnergy) {
    for (int iAngle = 0 ; iAngle < reflectTransGrids.m_numPcolAngles ; ++iAngle) {
      AH_DEBUG << "m_pcolRefl[" << iEnergy << "][" << iAngle << "] = " << reflectTransGrids.m_pcolRefl[iEnergy][iAngle] << std::endl;
    }
  }
  #endif
  
  // close file
  fits_close_file(refl_fp, &status);
  checkForFITSError(status, "closing", param.m_pcolreffile);

} // end getMirrorPcolData()


/******************************************************************************/


void remapReflectTrans(const ReflectTransGrids & reflGrids, 
                       long numOutEnergies, 
                       const std::vector<double> & outEnergies, 
                       long numGroups, 
                       bool pcolExists, 
                       bool doTransmission, 
                       RemappedReflectTransGrids & remappedGrids) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
    
  // +++ put in .h file that back is index 0 and pcol is index 1
  int backIdx = 0;
  int pcolIdx = 1;
   
  // input yGrids for bisectionInterp
  std::vector<double> frontReflSlice;
  std::vector<double> frontTranSlice;
  std::vector<double> frontTauPermmSlice;
  std::vector<double> backReflSlice;
  std::vector<double> pcolReflSlice;
  std::vector<double> roughTauPermmSlice;
  std::vector<double> topExtObjectsTransSlice;
  std::vector<double> bottomExtObjectsTransSlice;
  
  double interpolatedValue;             // output for calls to bisectionInterp
    
  // +++
  int numRoughSurfaces = 2;

  // +++ make local vars for reflGrids.m_numFrontEnergies, etc?

  // -------------------------------------
  
//  AH_DEBUG << " = " <<  << std::endl;
  AH_DEBUG << "numOutEnergies = " << numOutEnergies << std::endl;
  for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
    AH_DEBUG << "outEnergies["<<iEnergy<<"] = " << outEnergies[iEnergy] << std::endl;
  }
  
  AH_DEBUG << "reflGrids.m_frontReflEnergies.size() = " << reflGrids.m_frontReflEnergies.size() << std::endl;
  for (uint iEnergy = 0 ; iEnergy < reflGrids.m_frontReflEnergies.size() ; ++iEnergy) {
    AH_DEBUG << "reflGrids.m_frontReflEnergies["<<iEnergy<<"] = " << reflGrids.m_frontReflEnergies[iEnergy] << std::endl;
  }
  
  // size all the remapped grids
  // +++ should this be done here?
  remappedGrids.m_frontRefl.resize(numOutEnergies);
  remappedGrids.m_frontTran.resize(numOutEnergies);
  remappedGrids.m_frontTauPermm.resize(numOutEnergies);
  remappedGrids.m_roughRefl.resize(numOutEnergies);
  remappedGrids.m_roughTauPermm.resize(numOutEnergies);
  remappedGrids.m_topExtObjectTransProb.resize(numOutEnergies);
  remappedGrids.m_bottomExtObjectTransProb.resize(numOutEnergies);
  for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
    remappedGrids.m_frontRefl[iEnergy].resize(reflGrids.m_numFrontAngles);
    remappedGrids.m_frontTran[iEnergy].resize(reflGrids.m_numFrontAngles);
    remappedGrids.m_frontTauPermm[iEnergy].resize(numGroups+1);
    remappedGrids.m_roughRefl[iEnergy].resize(reflGrids.m_numRoughAngles);
    remappedGrids.m_roughTauPermm[iEnergy].resize(numGroups+1);
    remappedGrids.m_topExtObjectTransProb[iEnergy].resize(reflGrids.m_numTopExtObjectParts);
    remappedGrids.m_bottomExtObjectTransProb[iEnergy].resize(reflGrids.m_numBottomExtObjectParts);
    for (int iAngle = 0 ; iAngle < reflGrids.m_numFrontAngles ; ++iAngle) {
      remappedGrids.m_frontRefl[iEnergy][iAngle].resize(numGroups);
      remappedGrids.m_frontTran[iEnergy][iAngle].resize(numGroups);
    }
    for (int iAngle = 0 ; iAngle < reflGrids.m_numRoughAngles ; ++iAngle) {
      remappedGrids.m_roughRefl[iEnergy][iAngle].resize(numRoughSurfaces);
    }
  }
  
  if (pcolExists) {
    remappedGrids.m_pcolRefl.resize(numOutEnergies);
    for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
      remappedGrids.m_pcolRefl[iEnergy].resize(reflGrids.m_numRoughAngles);
    }
  }
  
  AH_DEBUG << std::endl;
  
  // First do the front-side mirror quantities, for each group and each angle, 
  // loop over the energy grid on to which the arrays are to be remapped.
  // Front-side reflectivity and surface transmission
  frontReflSlice.resize(reflGrids.m_numFrontEnergies);
  for (int iGroup = 0 ; iGroup < numGroups ; ++iGroup) {
    for (int iAngle = 0 ; iAngle < reflGrids.m_numFrontAngles ; ++iAngle) {
      // Extract slices of reflectivity for 1 group and 1 angle
      // +++ this may not be most efficient way to store/retrieve data?
      for (int iEnergy = 0 ; iEnergy < reflGrids.m_numFrontEnergies ; ++iEnergy) {
        frontReflSlice[iEnergy] = reflGrids.m_frontRefl[iEnergy][iAngle][iGroup];
      }
      for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
        bisectionInterp(reflGrids.m_numFrontEnergies, reflGrids.m_frontReflEnergies, frontReflSlice, outEnergies[iEnergy], interpolatedValue);
        remappedGrids.m_frontRefl[iEnergy][iAngle][iGroup] = interpolatedValue;
      } 
    }
  }
  
  AH_DEBUG << std::endl;
  
  /* testing that I created remapped front refl grid correctly */
  #ifdef DEBUG
  AH_DEBUG << "*** testing that I created remapped front refl grid correctly ***" << std::endl;
  for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
    for (int iAngle = 0 ; iAngle < reflGrids.m_numFrontAngles ; ++iAngle) {
      for (int iGroup = 0 ; iGroup < numGroups ; ++iGroup) {
        AH_DEBUG << "m_frontRefl[" << iEnergy << "][" << iAngle << "][" << iGroup <<  "] = " << remappedGrids.m_frontRefl[iEnergy][iAngle][iGroup] << std::endl;
      }
    }
  }
  #endif 
  
  // +++ in TRF, this was inside above loop.  compare time for both methods
  if (doTransmission) {
    frontTranSlice.resize(reflGrids.m_numFrontEnergies);
    for (int iGroup = 0 ; iGroup < numGroups ; ++iGroup) {
      for (int iAngle = 0 ; iAngle < reflGrids.m_numFrontAngles ; ++iAngle) {
        // Extract slices of transmission for 1 group and 1 angle
        // +++ this may not be most efficient way to store/retrieve data?
        for (int iEnergy = 0 ; iEnergy < reflGrids.m_numFrontEnergies ; ++iEnergy) {
          frontTranSlice[iEnergy] = reflGrids.m_frontTran[iEnergy][iAngle][iGroup];
        }
        for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
          bisectionInterp(reflGrids.m_numFrontEnergies, reflGrids.m_frontReflEnergies, frontTranSlice, outEnergies[iEnergy], interpolatedValue);
          remappedGrids.m_frontTran[iEnergy][iAngle][iGroup] = interpolatedValue;
        }
      }
    }
  }
  
  AH_DEBUG << std::endl;
  
  // Extract tau per mm for the bottom of the front-side mirror coating to 
  // the top (optical depth per mm) for each group of the front-side mirrors 
  // (it does not depend on angle)
  // The arrays fronttaupermm have a size ngroups+1 in the 2nd index; the 0th 
  // element is the pre-collimator so the value will always be 0 for this and 
  // we do not interpolate for that 
  if (doTransmission) {
    frontTauPermmSlice.resize(reflGrids.m_numFrontEnergies);
    for (int iGroup = 1 ; iGroup <= numGroups ; ++iGroup) {
      // Extract slices of transmission for 1 group and 1 angle
      // +++ this may not be most efficient way to store/retrieve data?
      for (int iEnergy = 0 ; iEnergy < reflGrids.m_numFrontEnergies ; ++iEnergy) {
        frontTauPermmSlice[iEnergy] = reflGrids.m_frontTauPermm[iEnergy][iGroup];
      }
      for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
        // Interpolate the tau per mm array for each group
        bisectionInterp(reflGrids.m_numFrontEnergies, reflGrids.m_frontReflEnergies, frontTauPermmSlice, outEnergies[iEnergy], interpolatedValue);
        remappedGrids.m_frontTauPermm[iEnergy][iGroup]= interpolatedValue;
      } 
    }
  }
  
  AH_DEBUG << std::endl;
  
  // Do the reflectivity of the back-side of mirror foils
  backReflSlice.resize(reflGrids.m_numBackEnergies) ;
  for (int iAngle = 0 ; iAngle < reflGrids.m_numBackAngles ; ++iAngle) {
    // Extract slices of reflectivity for 1 angle
    for (int iEnergy = 0 ; iEnergy < reflGrids.m_numBackEnergies ; ++iEnergy) {
      backReflSlice[iEnergy] = reflGrids.m_backRefl[iEnergy][iAngle];
    }
    for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
      // Interpolate for a single output energy and single angle
      bisectionInterp(reflGrids.m_numBackEnergies, reflGrids.m_backEnergies, backReflSlice, outEnergies[iEnergy], interpolatedValue);
      remappedGrids.m_roughRefl[iEnergy][iAngle][backIdx] = interpolatedValue;
    }
  }
  
  AH_DEBUG << std::endl;
  
  // Do the reflectivity of the pre-collimator foils, if pre-collimator was specified in the .par file
  // +++ include this in above loop? for runtime efficiency?
  if (pcolExists) {
    pcolReflSlice.resize(reflGrids.m_numPcolEnergies);
    for (int iAngle = 0 ; iAngle < reflGrids.m_numPcolAngles ; ++iAngle) {
      // Extract slices of reflectivity for 1 angle
      for (int iEnergy = 0 ; iEnergy < reflGrids.m_numPcolEnergies ; ++iEnergy) {
        pcolReflSlice[iEnergy] = reflGrids.m_pcolRefl[iEnergy][iAngle];
      }
      for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
        // Interpolate for a single output energy and single angle
        bisectionInterp(reflGrids.m_numPcolEnergies, reflGrids.m_pcolEnergies, pcolReflSlice, outEnergies[iEnergy], interpolatedValue);
        remappedGrids.m_roughRefl[iEnergy][iAngle][pcolIdx] = interpolatedValue;
      }
    }
  }
  
  AH_DEBUG << std::endl;
  
  // Now do the optical depth per mm of the rough/thick materials 
  // (e.g. back-side of mirrors, pre-collimator)
  if (doTransmission) {
    roughTauPermmSlice.resize(reflGrids.m_numFrontEnergies);
    // Remember that the size of the *taupermm arrays is ngroups+1 (0 for p-col)
    for (int iGroup = 0 ; iGroup <= numGroups ; ++iGroup) {
      
      if (iGroup == 0 and !pcolExists) {
        // skip the whole loop (i.e. don’t do pre-collimator)
        continue; // +++ continue, not break, right?
      }
      
      // Extract tau per mm for each rough surface group/material (it does not depend on angle)
      for (int iEnergy = 0 ; iEnergy < reflGrids.m_numFrontEnergies ; ++iEnergy) {
        roughTauPermmSlice[iEnergy] = reflGrids.m_roughTauPermm[iEnergy][iGroup];
      }
      AH_DEBUG << std::endl;
      for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
        // Interpolate the tau per mm array for each material
        bisectionInterp(reflGrids.m_numFrontEnergies, reflGrids.m_frontReflEnergies, roughTauPermmSlice, outEnergies[iEnergy], interpolatedValue);
        remappedGrids.m_roughTauPermm[iEnergy][iGroup] = interpolatedValue;
      }
    } 
  }
  
  AH_DEBUG << std::endl;
  
  // Now do top external objects
  topExtObjectsTransSlice.resize(reflGrids.m_numFrontEnergies);   // +++ why not resize all these up top?
  for (int iObj = 0 ; iObj < reflGrids.m_numTopExtObjectParts ; ++iObj) {
    for (int iEnergy = 0 ; iEnergy < reflGrids.m_numFrontEnergies ; ++iEnergy) {
      topExtObjectsTransSlice[iEnergy] = reflGrids.m_topExtObjectsTransProb[iEnergy][iObj];
    }
    for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
      bisectionInterp(reflGrids.m_numFrontEnergies, reflGrids.m_frontReflEnergies, topExtObjectsTransSlice, outEnergies[iEnergy], interpolatedValue);
      remappedGrids.m_topExtObjectTransProb[iEnergy][iObj] = interpolatedValue;
    }
  }

  AH_DEBUG << std::endl;
  
  // Now do bottom external objects
  bottomExtObjectsTransSlice.resize(reflGrids.m_numFrontEnergies);
  for (int iObj = 0 ; iObj < reflGrids.m_numBottomExtObjectParts ; ++iObj) {
    for (int iEnergy = 0 ; iEnergy < reflGrids.m_numFrontEnergies ; ++iEnergy) {
      bottomExtObjectsTransSlice[iEnergy] = reflGrids.m_bottomExtObjectsTransProb[iEnergy][iObj];
    }
    for (int iEnergy = 0 ; iEnergy < numOutEnergies ; ++iEnergy) {
      bisectionInterp(reflGrids.m_numFrontEnergies, reflGrids.m_frontReflEnergies, bottomExtObjectsTransSlice, outEnergies[iEnergy], interpolatedValue);
      remappedGrids.m_bottomExtObjectTransProb[iEnergy][iObj] = interpolatedValue;
    }
  }
  
  AH_DEBUG << "end remapReflectTrans" << std::endl;
  
  // +++ for testing
  //createRemappedFITSFile(remappedGrids, numGroups, reflGrids, numOutEnergies, outEnergies);
  
} // end remapReflectTrans()


/******************************************************************************/


energyUnits_e getEnergyUnit(fitsfile * fits_fp, const std::string & filename,
                          int energyColNum) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  //+++ can't i get filename from fits_fp?
  
  int status = 0;                       // for cfitsio function calls
  
  char value[FLEN_VALUE] = "";          // getting TUNITn keyword
  char keyname[FLEN_KEYWORD] = "";      // getting TUNITn keyword
  std::string energyUnit;
  std::string errorMsg;
  
  // -------------------------------------
  
  sprintf(keyname, "TUNIT%d", energyColNum);
  fits_read_keyword(fits_fp, keyname, value, 0, &status);
  checkForFITSError(status, "checking units of energy in", filename);
  energyUnit = (std::string)value;
  energyUnit.erase(std::remove_if(energyUnit.begin(), energyUnit.end(), isspace), energyUnit.end());
  energyUnit.erase(std::remove(energyUnit.begin(), energyUnit.end(), '\''), energyUnit.end());
  
  if (isEqualCaseInsens(energyUnit,"keV")) {
    return e_keV;
  } else if (isEqualCaseInsens(energyUnit,"eV")) {
    return e_eV;
  } else {
    errorMsg = "Energy column must have TUNIT of eV or keV, in file ";
    errorMsg.append(filename);
    AH_THROW_RUNTIME(errorMsg);
  }
  
}


/******************************************************************************/


void getRowsInEnergyRange(fitsfile * fits_fp, 
                          const std::string & filename,
                          const Photons & photons, 
                          energyUnits_e energyUnit, 
                          int energyColNum, 
                          long & firstNeededRow, 
                          long & lastNeededRow) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;                       // for cfitsio function calls
  
  double tempEnergy = 0.0;
  bool foundFirstNeededRow = false;
  bool foundLastNeededRow = false;
  long numRowsFile = 0;
  vector1Ddbl tempEnergies;
  
  std::string errorMsg;
  
  // -------------------------------------
  
  // +++ does the speed matter than I should pass in filename instead of use getfilename?
  
  // assuming that this is in order by energy, look for the range of rows
  // within our desired energy bounds
  
  fits_get_num_rows(fits_fp, &numRowsFile, &status);
  checkForFITSError(status, "getting number of rows in", filename);
  
  // store all the energies from the file, in keV
  tempEnergies.resize(numRowsFile);
  fits_read_col_dbl(fits_fp, energyColNum, 1, 1, numRowsFile, 0, &(tempEnergies[0]), 0, &status);
  storeEnergyInkeV(energyUnit, tempEnergies);
  
  AH_DEBUG << "numRowsFile = " << numRowsFile << " in " << filename << std::endl;
  AH_DEBUG << "energies (after converting to keV)" << std::endl;
//  for (long long i = 0 ; i < numRowsFile ; ++i) {
//    AH_DEBUG << "tempEnergies["<<i<<"] = " << tempEnergies[i] << std::endl;
//  }
  
  // make sure that given min and max energies are within range of file
  if (photons.m_minPhotonEnergy < getMinDouble(tempEnergies)) {
    errorMsg = "The provided minimum energy of ";
    errorMsg.append(doubleToString(photons.m_minPhotonEnergy));
    errorMsg.append(" is below the valid energy range in ");
    errorMsg.append(filename);
    AH_THROW_RUNTIME(errorMsg);
  }
  if (photons.m_maxPhotonEnergy > getMaxDouble(tempEnergies)) {
    errorMsg = "The provided maximum energy of ";
    errorMsg.append(doubleToString(photons.m_maxPhotonEnergy));
    errorMsg.append(" is above the valid energy range in ");
    errorMsg.append(filename);
    AH_THROW_RUNTIME(errorMsg);
  }
  
  for (long long i = 1 ; i <= numRowsFile ; ++i) {
    tempEnergy = tempEnergies[i-1];
    if ( !foundFirstNeededRow && (tempEnergy == photons.m_minPhotonEnergy) ) {
      firstNeededRow = i;
      foundFirstNeededRow = true;
    }
    if ( !foundFirstNeededRow && (tempEnergy >  photons.m_minPhotonEnergy) ) {
      firstNeededRow = ( i==1 ? i : i-1 );
      foundFirstNeededRow = true;
    }
    if (tempEnergy >= photons.m_maxPhotonEnergy) {
      lastNeededRow = i;
      foundLastNeededRow = true;
      break;
    }
  }
  
  // in case file ended before finding maxPhotonEnergy
  if (foundFirstNeededRow && !foundLastNeededRow) {
    lastNeededRow = numRowsFile;
  }
  
  // if we didn't find either bounds, the file is invalid
  if ( (firstNeededRow == 0) && (firstNeededRow == 0) ) {
    errorMsg = "There are no rows in the file ";
    errorMsg.append(filename);
    errorMsg.append(" which are in the valid energy range.");
    AH_THROW_RUNTIME(errorMsg);
  }
  
} // end getRowsInEnergyRange()


/******************************************************************************/


void storeEnergyInkeV(energyUnits_e energyUnit, vector1Ddbl & energyGrid) {
  
  if (energyUnit == e_keV) {
    // do nothing if energy is already in keV
  } else if (energyUnit == e_eV) {
    try {
      std::transform(energyGrid.begin(), energyGrid.end(), energyGrid.begin(),
                     std::bind2nd(std::multiplies<double>(), s_eVTokeV));
    } catch(const std::exception & e) {
      AH_THROW_RUNTIME("Error converting energy to keV.");
    }
  } else {
    AH_THROW_RUNTIME("Energy must be entered in eV or keV.");
  }
  
}


/******************************************************************************/


void bisectionInterp(long numGridPts, 
                     const std::vector<double> & xGrid, 
                     const std::vector<double> & yGrid, 
                     double xIn, double & yOut) {
  
  // add a comment about how there are no bounds checking in this function,
  // because it's called often from the raytraceonephoton() function and we 
  // want it to go as fast as possible.  The bounds checks should be done 
  // (angles, and it's called for scattering which should be normalized between 
  // 0 and 1, etc)
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  double slope = 0.0;          // for the linear interpolation
  
  // These will change as the routine progresses
  int lowBin = 0;
  int highBin = numGridPts-1;
  int deltaBins = highBin - lowBin + 1;
  int midBin = 0;
  
  std::string errorMsg;
  
  // -------------------------------------

  // xIn = lowest grid value
  if (xIn == xGrid[0]) {
    yOut = yGrid[0];
    return;
  }
  
  // xIn = highest grid value
  if (xIn == xGrid[numGridPts-1]) {
    yOut = yGrid[numGridPts-1];
    return;
  }
  
  // else, xIn is somewhere in the middle
  while (deltaBins > 2) {
  
    midBin = lowBin + (deltaBins/2);
    // +++ shouldn't this be (xIn < xGrid[highbin]) etc?
    if ( (xIn > xGrid[midBin]) && (xIn < xGrid[numGridPts-1]) ) {
      // xIn is in current upper half
      lowBin = midBin;
    } else if ( (xIn < xGrid[midBin]) && (xIn > xGrid[0]) ) {
      // xIn is in current lower half
      highBin = midBin;
    } else if (xIn == xGrid[midBin]) {
      // xIn = mid value
      yOut = yGrid[midBin];
      return;
    } else {
      //+++ this will lead to an infinite loop without this else
  AH_DEBUG << "numGridPts = " << numGridPts << std::endl;
  AH_DEBUG << "xIn = " << xIn << std::endl;
  AH_DEBUG << "xGrid[0] = " << xGrid[0] << std::endl;
  AH_DEBUG << "xGrid[midBin] = " << xGrid[midBin] << std::endl;
  AH_DEBUG << "xGrid[numGridPts-1] = " << xGrid[numGridPts-1] << std::endl;
      errorMsg = "error while interpolating value ";
      errorMsg.append(doubleToString(xIn));
      errorMsg.append(" between ");
      errorMsg.append(doubleToString(xGrid[0]));
      errorMsg.append(" and ");
      errorMsg.append(doubleToString(xGrid[numGridPts-1]));
      AH_DEBUG << errorMsg << std::endl;
      AH_THROW_RUNTIME(errorMsg);
    }
    // lowBin and highBin moved; update deltaBins
    deltaBins = highBin - lowBin + 1;
    
  }
  
  // now do the linear interpolation
  if (xIn == xGrid[lowBin]) {
    yOut = yGrid[lowBin];
  } else if (xIn == xGrid[highBin]) {
    yOut = yGrid[highBin];
  } else {
    slope = (yGrid[highBin]-yGrid[lowBin]) / (xGrid[highBin]-xGrid[lowBin]);
    yOut = yGrid[lowBin] + (slope * (xIn-xGrid[lowBin]));
  }
  
} // end bisectionInterp()


/******************************************************************************/


void bisectionLocate(const std::vector<double> & xGrid, 
                     long numGridPts, 
                     double xin, 
                     int & index1, 
                     int & index2) {

  // +++ use .at() or []?
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // initial values of pointers, they will change as the routine progresses
  long lobin = 0;
  long hibin = numGridPts - 1;
  long deltabins = hibin - lobin + 1;
  long midbin = 0;
  
  double firstX = xGrid.at(lobin);
  double lastX  = xGrid.at(hibin);
  double midX   = 0.0;
  
  // -------------------------------------
  
  // case of XIN = highest grid value
  if (xin == lastX ) {
    index1 = numGridPts - 1;
    index2 = index1;
    return;
  }
  
  // case of XIN = lowest grid value
  if (xin == firstX) {
    index1 = 0;
    index2 = index1;
    return;
  }
  
  // make sure xin is valid
  if ( (xin < firstX) || (xin > lastX) ) {
    AH_THROW_RUNTIME("input x=" 
                      + doubleToString(xin) 
                      + " is not in range [" 
                      + doubleToString(firstX) 
                      + "," 
                      + doubleToString(lastX) 
                      + "]");
  }
  
  while (deltabins > 2) {
    
    midbin = lobin + (deltabins/2);
    midX = xGrid.at(midbin);
    if ( (xin > midX) && (xin < lastX) ) {
      lobin = midbin;
    } else if ( (xin < midX) && (xin > firstX) ) {
      hibin = midbin;
    // Case of XIN exactly equal to the mid value
    } else if (xin == midX) {
      index1 = midbin - 1;
      index2 = midbin;
      return;
    }
    // lobin and hibin move - following is the latest deltabins
    deltabins = hibin - lobin + 1;
    
  } // end-while
  
  // lobin and hibin should now point to the index1 and index2 respectively 
  index1 = lobin;
  index2 = hibin;
  
} // end bisectionLocate()


/******************************************************************************/


void makeObstructBBox(const std::vector<CartesianCoord> & vertices, 
                      ObjectBoundingBox & obstructBBoxCoords, 
                      std::vector<CartesianCoord> & obstructBBoxVertices) {

  int numVerticesIn;
  vector1Ddbl xVertices;
  vector1Ddbl yVertices;
  vector1Ddbl zVertices;
  
  numVerticesIn = vertices.size();
  xVertices.resize(numVerticesIn);
  yVertices.resize(numVerticesIn);
  zVertices.resize(numVerticesIn);
  
  for (int i = 0 ; i < numVerticesIn ; ++i) {
    xVertices[i] = vertices[i].m_x;
    yVertices[i] = vertices[i].m_y;
    zVertices[i] = vertices[i].m_z;
  }
  
  double xmin = getMinDouble(xVertices);
  double xmax = getMaxDouble(xVertices);
  double ymin = getMinDouble(yVertices);
  double ymax = getMaxDouble(yVertices);
  double zmin = getMinDouble(zVertices);
  double zmax = getMaxDouble(zVertices);
  
  obstructBBoxCoords.m_xMin = xmin;
  obstructBBoxCoords.m_xMax = xmax;
  obstructBBoxCoords.m_yMin = ymin;
  obstructBBoxCoords.m_yMax = ymax;
  obstructBBoxCoords.m_zMin = zmin;
  obstructBBoxCoords.m_zMax = zmax;
  
  obstructBBoxVertices.resize(8);
  CartesianCoord vertex0(xmin, ymin, zmin);
  CartesianCoord vertex1(xmax, ymin, zmin);
  CartesianCoord vertex2(xmin, ymax, zmin);
  CartesianCoord vertex3(xmax, ymax, zmin);
  CartesianCoord vertex4(xmin, ymin, zmax);
  CartesianCoord vertex5(xmax, ymin, zmax);
  CartesianCoord vertex6(xmin, ymax, zmax);
  CartesianCoord vertex7(xmax, ymax, zmax);
  obstructBBoxVertices[0] = vertex0;
  obstructBBoxVertices[1] = vertex1;
  obstructBBoxVertices[2] = vertex2;
  obstructBBoxVertices[3] = vertex3;
  obstructBBoxVertices[4] = vertex4;
  obstructBBoxVertices[5] = vertex5;
  obstructBBoxVertices[6] = vertex6;
  obstructBBoxVertices[7] = vertex7;

} // end makeObstructBBox()


/******************************************************************************/


void makeFoilBBox(double rBottomInner, 
                  double rBottomOuter, 
                  double rTopInner, 
                  double rTopOuter, 
                  double angleStartIn, 
                  double angleEndIn, 
                  double zBottom, 
                  double zTop,
                  std::vector<double> & foilBBoxCoords,
                  ObjectBoundingBox & foilBBox, 
                  std::vector< std::vector<double> > & foilVertices,
                  std::vector<int> & foilCoordQuad) {

  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // get the mod, so we're between 0 and 2pi
  double angleStart = std::fmod(angleStartIn, s_twopi);
  double angleEnd = std::fmod(angleEndIn, s_twopi);
  
  double xmax;
  double xmin;
  double ymax;
  double ymin;
  double zmax;
  double zmin;
  
  // -------------------------------------
  
  // record how many quadrants this box spans
  foilCoordQuad[0] = (1 + (int)(angleStart/s_piovertwo)) % 4;
  foilCoordQuad[1] = (1 + (int)(angleEnd/s_piovertwo)) % 4;
  foilCoordQuad[2] = foilCoordQuad[1] - foilCoordQuad[0] + 1;
  // +++ 20140402 KLR per trf: test to make sure this is correct.  conversion to int can vary.  
  
  // There are 8 vertices. Convert the polar coords of each to Cartesian
  double cosStartAngle = std::cos(angleStart);
  double sinStartAngle = std::sin(angleStart);
  double cosEndAngle = std::cos(angleEnd);
  double sinEndAngle = std::sin(angleEnd);
  foilVertices.clear();
  foilVertices.resize(3);
  foilVertices[0].resize(8);
  foilVertices[1].resize(8);
  foilVertices[2].resize(8);
  foilVertices[0][0] = rBottomInner * cosStartAngle;
  foilVertices[0][1] = rBottomOuter * cosStartAngle;
  foilVertices[0][2] = rBottomInner * cosEndAngle;
  foilVertices[0][3] = rBottomOuter * cosEndAngle;
  foilVertices[0][4] = rTopInner * cosStartAngle;
  foilVertices[0][5] = rTopOuter * cosStartAngle;
  foilVertices[0][6] = rTopInner * cosEndAngle;
  foilVertices[0][7] = rTopOuter * cosEndAngle;
  foilVertices[1][0] = rBottomInner * sinStartAngle;
  foilVertices[1][1] = rBottomOuter * sinStartAngle;
  foilVertices[1][2] = rBottomInner * sinEndAngle;
  foilVertices[1][3] = rBottomOuter * sinEndAngle;
  foilVertices[1][4] = rTopInner * sinStartAngle;
  foilVertices[1][5] = rTopOuter * sinStartAngle;
  foilVertices[1][6] = rTopInner * sinEndAngle;
  foilVertices[1][7] = rTopOuter * sinEndAngle;
  for (int i = 0 ; i < 4 ; ++i)
    foilVertices[2][i] = zBottom;
  for (int i = 4 ; i < 8 ; ++i)
    foilVertices[2][i] = zTop;
  
  // Find the extrema in x,y,z. The z-direction is straight forward but for 
  // x and y we have to account for the fact that the extrema of the curved 
  // foil are not always going to be at the vertices.
  // if the start and end angle of foil encloses 0 or pi, the max x value
  // is at zero or pi respectively (on curved part of top outer foil)
  if ((angleStart > s_pi) && (angleEnd < s_pi)) {
    xmax = std::max(rTopOuter,rBottomOuter);
    xmin = *std::min_element(foilVertices[0].begin(), foilVertices[0].end());
  } else if ((angleStart <= s_pi) && (angleEnd > s_pi) && 
             (angleEnd < s_twopi)) {
    xmin = std::min(-1.0 * rTopOuter, -1.0 * rBottomOuter);
    xmax = *std::max_element(foilVertices[0].begin(), foilVertices[0].end());
  } else {
    // if 0 or pi is not enclosed, the extrema are at the vertices.
    xmax = *std::max_element(foilVertices[0].begin(), foilVertices[0].end());
    xmin = *std::min_element(foilVertices[0].begin(), foilVertices[0].end());
  }

  if ((angleStart <= s_piovertwo) && 
          (angleEnd > s_piovertwo) && (angleEnd < s_pi)) {
    ymax = std::max(rTopOuter,rBottomOuter);
    ymin = *std::min_element(foilVertices[1].begin(), foilVertices[1].end());
  } else if ((angleStart > s_piovertwo) && (angleStart <= s_threepiovertwo) && 
             (angleEnd > s_threepiovertwo) && (angleEnd < s_twopi)) {
    ymin = std::min(-1.0 * rTopOuter, -1.0 * rBottomOuter);
    //+++ is this correct?
    ymax = *std::max_element(foilVertices[1].begin(), foilVertices[1].end());
  } else {
    ymax = *std::max_element(foilVertices[1].begin(), foilVertices[1].end());
    ymin = *std::min_element(foilVertices[1].begin(), foilVertices[1].end());
  }
  zmin = zBottom;
  zmax = zTop;
  
  foilBBoxCoords[0] = xmin;
  foilBBoxCoords[1] = xmax;
  foilBBoxCoords[2] = ymin;
  foilBBoxCoords[3] = ymax;
  foilBBoxCoords[4] = zmin;
  foilBBoxCoords[5] = zmax;
  
  
  foilBBox.m_xMin = xmin;
  foilBBox.m_xMax = xmax;
  foilBBox.m_yMin = ymin;
  foilBBox.m_yMax = ymax;
  foilBBox.m_zMin = zmin;
  foilBBox.m_zMax = zmax;

} // end makeFoilBBox()


/******************************************************************************/


void makePhotonBBox(const CartesianCoord & photonPos, 
                    const DirectionVector & photonDir, 
                    double zEnd, 
                    double housingHardLimit, 
                    PhotonBoundingBox & photonBBox, 
                    int & photonBBoxError) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  double timez = 0.0;
  double xEnd = 0.0;
  double yEnd = 0.0;
  
  // set dummy variables as pos or neg, depending on sign of vector
  double dummyx = housingHardLimit * ( (photonDir.m_xDir < 0.) ? -1.0 : 1.0 );
  double dummyy = housingHardLimit * ( (photonDir.m_yDir < 0.) ? -1.0 : 1.0 );
  double dummyz = housingHardLimit * ( (photonDir.m_zDir < 0.) ? -1.0 : 1.0 );
  
  double r1photonBBox = 0.0;
  double r2photonBBox = 0.0;
  
  // -------------------------------------
  
  
  // Since speed is the primary concern (XRT objects with potential photon hits 
  // have to be selected for every ray-trace photon), only the crudest photon 
  // bounding box is sufficient, otherwise the computation time in calculating 
  // a more sophisticated box does not result in an overall increase in speed. 
  // This means that we do not even precisely confine the photon box to the 
  // telescope housing. (The full geometrical information for the housing 
  // components is used later if/when we encounter an interaction between 
  // photon and housing.) If photonDir.m_zDir=0, the bounding box will be 
  // two dimensional and semi-infinite in extent. If both photonDir.m_xDir and 
  // photonDir.m_yDir are zero (photon traveling parallel z- axis) then again,  
  // the bounding box is two-dimensional. 
  
  // note: |photondir*| must be <1!
  
  if ( (photonDir.m_xDir == 0.0) && 
       (photonDir.m_yDir == 0.0) && 
       (photonDir.m_zDir == 0.0) ) {
    // all three = 0
         
    // return an error condition to alert the ray-tracing program (the error 
    // is not fatal and the condition vx=vy=vz=0 may still be useful in 
    // finding a nearest neighbor).
    photonBBox.m_xMin = photonPos.m_x;
    photonBBox.m_xMax = photonPos.m_x;
    photonBBox.m_yMin = photonPos.m_y;
    photonBBox.m_yMax = photonPos.m_y;
    photonBBox.m_zMin = photonPos.m_z;
    photonBBox.m_zMax = photonPos.m_z;
    photonBBoxError = 1;

  } else  if ( (photonDir.m_zDir == 0.0) &&
               (photonDir.m_xDir != 0.0) && 
               (photonDir.m_yDir != 0.0) ) {
    // only z = 0
    
    photonBBox.m_xMin = std::min(photonPos.m_x, dummyx);
    photonBBox.m_xMax = std::max(photonPos.m_x, dummyx);
    photonBBox.m_yMin = std::min(photonPos.m_y, dummyy);
    photonBBox.m_yMax = std::max(photonPos.m_y, dummyy);
    photonBBox.m_zMin = photonPos.m_z;
    photonBBox.m_zMax = photonPos.m_z;
    
  } else if ( (photonDir.m_xDir == 0.0) && 
              (photonDir.m_yDir == 0.0) ) {
    // x = y = 0.  z is not 0 
    // (or else first block would have executed)
    
    timez = (zEnd - photonPos.m_z) / photonDir.m_zDir;
    photonBBox.m_xMin = photonPos.m_x;
    photonBBox.m_xMax = photonPos.m_x;
    photonBBox.m_yMin = photonPos.m_y;
    photonBBox.m_yMax = photonPos.m_y;
    if (timez < 0.0) {
      photonBBox.m_zMin = std::min(photonPos.m_z, dummyz);
      photonBBox.m_zMax = std::max(photonPos.m_z, dummyz);
    } else if (timez > 0.0) {
      photonBBox.m_zMin = std::min(photonPos.m_z, zEnd);
      photonBBox.m_zMax = std::max(photonPos.m_z, zEnd);
    } else {
      photonBBox.m_zMin = photonPos.m_z;
      photonBBox.m_zMax = photonPos.m_z;
      photonBBoxError = 2;
    }
    
  } else if (photonDir.m_zDir == 0.0) {
    // either (x and z == 0) or (y and z == 0)
    // (if all three were zero, first if-block would have been executed)
    
    photonBBox.m_xMin = ( (photonDir.m_xDir == 0.0) ? photonPos.m_x : std::min(photonPos.m_x, dummyx) );
    photonBBox.m_xMax = ( (photonDir.m_xDir == 0.0) ? photonPos.m_x : std::max(photonPos.m_x, dummyx) );
    photonBBox.m_yMin = ( (photonDir.m_yDir == 0.0) ? photonPos.m_y : std::min(photonPos.m_y, dummyy) );
    photonBBox.m_yMax = ( (photonDir.m_yDir == 0.0) ? photonPos.m_y : std::max(photonPos.m_y, dummyy) );
    photonBBox.m_zMin = photonPos.m_z;
    photonBBox.m_zMax = photonPos.m_z;
    
  } else {
    // none = 0
    
    timez = (zEnd - photonPos.m_z) / photonDir.m_zDir;
    if (timez < 0.0) {
      // photon can't reach specified endpoint (zEnd) so bounding box 
      // has to be semi-infinite.
      photonBBox.m_xMin = std::min(photonPos.m_x, dummyx);
      photonBBox.m_xMax = std::max(photonPos.m_x, dummyx);
      photonBBox.m_yMin = std::min(photonPos.m_y, dummyy);
      photonBBox.m_yMax = std::max(photonPos.m_y, dummyy);
      photonBBox.m_zMin = std::min(photonPos.m_z, dummyz);
      photonBBox.m_zMax = std::max(photonPos.m_z, dummyz);
    } else {
      xEnd = photonPos.m_x + (photonDir.m_xDir * timez);
      yEnd = photonPos.m_y + (photonDir.m_yDir * timez);
      if (photonPos.m_x <= xEnd) {
        photonBBox.m_xMin = photonPos.m_x;
        photonBBox.m_xMax = xEnd;
      } else {
        photonBBox.m_xMin = xEnd;
        photonBBox.m_xMax = photonPos.m_x;
      }
      if (photonPos.m_y <= yEnd) {
        photonBBox.m_yMin = photonPos.m_y;
        photonBBox.m_yMax = yEnd;
      } else {
        photonBBox.m_yMin = yEnd;
        photonBBox.m_yMax = photonPos.m_y;
      }
      if (photonPos.m_z <= zEnd) {
        photonBBox.m_zMin = photonPos.m_z;
        photonBBox.m_zMax = zEnd;
      } else {
        photonBBox.m_zMin = zEnd;
        photonBBox.m_zMax = photonPos.m_z;
      }
    }
    
  }
  
  // Compute the radial bounds
  r1photonBBox = std::sqrt( (photonBBox.m_xMin * photonBBox.m_xMin) + 
                       (photonBBox.m_yMin * photonBBox.m_yMin) );
  r2photonBBox = std::sqrt( (photonBBox.m_xMax * photonBBox.m_xMax) + 
                       (photonBBox.m_yMax * photonBBox.m_yMax) );
  photonBBox.m_rMin = std::min(r1photonBBox, r2photonBBox);
  photonBBox.m_rMax = std::max(r2photonBBox, r1photonBBox);
  
} // end makePhotonBBox()


/******************************************************************************/


void transformBBoxes(long numXRTObjects, 
                     int maxTransformXYZ, 
                     const vector2Ddbl & xrttransform, 
                     std::vector<XRTObject> & XRTObjects) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  const int numVertices = 8;
  const int numDim = 3;
  int transIdx = 0;     // for getting index of xrttransform element
  
  // vertices of the input and output bounding boxes respectively
  // outbox is 2D array to represent x,y,z because of loop below
  std::vector<CartesianCoord> inboxcoordinates(numVertices);
  std::vector< std::vector<double> > outboxcoordinates(numVertices);
  for (int iVertex = 0 ; iVertex < numVertices ; ++iVertex) {
    outboxcoordinates[iVertex].resize(numDim);
  }
  
  // combine all x's into one vector, etc
  std::vector<double> allXs(numVertices);
  std::vector<double> allYs(numVertices);
  std::vector<double> allZs(numVertices);
  
  // -------------------------------------
  
  for (int iObj = 0 ; iObj < numXRTObjects ; ++iObj) {
    
    ObjectBoundingBox & currInBBox = XRTObjects[iObj].m_BBox;
    ObjectBoundingBox & currOutBBox = XRTObjects[iObj].m_BBoxTrnsfrmd;
    
    // set the coordinates of the 8 (numVertices) vertices using the input
    // bounding box
    inboxcoordinates[0].m_x = currInBBox.m_xMin;
    inboxcoordinates[0].m_y = currInBBox.m_yMin;
    inboxcoordinates[0].m_z = currInBBox.m_zMin;
    inboxcoordinates[1].m_x = currInBBox.m_xMax;
    inboxcoordinates[1].m_y = currInBBox.m_yMin;
    inboxcoordinates[1].m_z = currInBBox.m_zMin;
    inboxcoordinates[2].m_x = currInBBox.m_xMin;
    inboxcoordinates[2].m_y = currInBBox.m_yMax;
    inboxcoordinates[2].m_z = currInBBox.m_zMin;
    inboxcoordinates[3].m_x = currInBBox.m_xMax;
    inboxcoordinates[3].m_y = currInBBox.m_yMax;
    inboxcoordinates[3].m_z = currInBBox.m_zMin;
    inboxcoordinates[4].m_x = currInBBox.m_xMax;
    inboxcoordinates[4].m_y = currInBBox.m_yMin;
    inboxcoordinates[4].m_z = currInBBox.m_zMax;
    inboxcoordinates[5].m_x = currInBBox.m_xMax;
    inboxcoordinates[5].m_y = currInBBox.m_yMin;
    inboxcoordinates[5].m_z = currInBBox.m_zMax;
    inboxcoordinates[6].m_x = currInBBox.m_xMax;
    inboxcoordinates[6].m_y = currInBBox.m_yMax;
    inboxcoordinates[6].m_z = currInBBox.m_zMax;
    inboxcoordinates[7].m_x = currInBBox.m_xMax;
    inboxcoordinates[7].m_y = currInBBox.m_yMax;
    inboxcoordinates[7].m_z = currInBBox.m_zMax;
    
    // clear the outboxcoordinates for this new XRTObject
    for (int iVertex = 0 ; iVertex < numVertices ; ++iVertex) {
      std::fill(outboxcoordinates[iVertex].begin(), outboxcoordinates[iVertex].end(), 0.0);
    }
    
    // Loop over each vertex of the input bounding box
    for (int iVertex = 0 ; iVertex < numVertices ; ++iVertex) {
      // Find the transformed x, y, z in turn by doing the matrix 
      // multiplication for rotations and simple addition for the shifts
      for (int iDim = 0 ; iDim < numDim ; ++iDim) {
        transIdx = numDim * iDim;
    
        outboxcoordinates[iVertex][iDim] += ( ( inboxcoordinates[iVertex].m_x * xrttransform[iObj][transIdx]   ) +
                                              ( inboxcoordinates[iVertex].m_y * xrttransform[iObj][transIdx+1] ) + 
                                              ( inboxcoordinates[iVertex].m_z * xrttransform[iObj][transIdx+2] ) );
        outboxcoordinates[iVertex][iDim] += xrttransform[iObj][iDim+9];
      }
    }
    
    // add all the x's, etc, to their own array, to get the min and max later
    for (int iVertex = 0 ; iVertex < numVertices ; ++iVertex) {
      allXs[iVertex] = outboxcoordinates[iVertex][0];
      allYs[iVertex] = outboxcoordinates[iVertex][1];
      allZs[iVertex] = outboxcoordinates[iVertex][2];
    }
    
    // Now get the minimum and maximum x y z values of the transformed bounding box vertices: these are the 6 bounding box parameters
    currOutBBox.m_xMin = getMinDouble(allXs);
    currOutBBox.m_xMax = getMaxDouble(allXs);
    currOutBBox.m_yMin = getMinDouble(allYs);
    currOutBBox.m_yMax = getMaxDouble(allYs);
    currOutBBox.m_zMin = getMinDouble(allZs);
    currOutBBox.m_zMax = getMaxDouble(allZs);
        
  } // end looping over XRTObjects
  
} // end transformBBoxes()


/******************************************************************************/


/**********************************************
 * ********************************************
 * 		doWork(): supporting functions
 * ********************************************
**********************************************/



// FUNCTION NAME: raytraceOnePhoton
//
// CALLING SEQUENCE:
//   raytraceOnePhoton(has3DObjects, initialphotonpos, initialphotondir, reflectTransGrids.m_numFrontAngles, reflectTransGrids.m_frontAngles, frefeslice, ftranseslice, fronttaupermmeslice, reflectTransGrids.m_numBackAngles, reflectTransGrids.m_backAngles, roughrefeslice, roughtaupermmeslice, scatenergyindex, scat, housings, numxrtzlevels, xrtzlevels, param.m_resultsplanez, maxnumpathcoords, transforms, zmaxsortxrtobjectindexAll, numXRTObjects, XRTObjects, numInteractions, pathCoords, pathDirs, pathCode, pathXRTObjectID, finalPhotonPos, finalPhotonDir, resultsPlaneImpact, pathErrorCode); 
//
// PURPOSE:
//   Follow a photon through an X-ray telescope until it is absorbed by an XRT 
//   object or until it intercepts the results plane
//   (1)  using housing information, Set the bound to the size of the photon bounding box 
//   (2)  loop through code, recording interactions between the photon and the telescope.  
//        The loop ends if the photon is absorbed, the photon goes over available zlevels,
//        (this happens when it hits the focal plane), 
//        or if the number of interactions for this photon exceeds a preset amount. 
//   (3) if the photon was absorbed, record the appropriate coordinates and codes
//
// INPUTS:
//   has3DObjects, initialPhotonCoord, initialPhotonDir, numIncidentAngles, 
//   incidentAngles, frontRefl, frontTran, frontTauPermm, numRoughAngles, 
//   roughAngles, roughRefl, roughtaupermm, scatenergyindex, scat, housings, 
//   numxrtzlevels, xrtzlevels, resultsplanez, maxNumPathCoords, transforms, 
//   zmaxsortxrtobjectindexAll, numXRTObjects, XRTObjects
//
// OUTPUTS:
//   numInteractions, pathCoords, pathDirs, pathCode, pathXRTObjectID, 
//   finalPhotonPos, finalPhotonDir, resultsPlaneWasImpacted, pathErrorCode
//
// CALLED BY:
//   doWork()
//
// SUBROUTINES:
//   makePhotonBBox(), housingImpactPositions(), interceptXRTObjects(), 
//   applyXRTTransform(), getPhotonObjectImpactCoords(), getXYForNewZ(), 
//   sidewallImpact(), getSurfaceNormal(), bisectionInterp(), getRandom(), 
//   getScatteredDirection(), getReflectionDirection()

//
void raytraceOnePhoton(bool has3DObjects, 
                       bool pcolExists,
                       bool doFastMode,
                       const CartesianCoord & initialPhotonCoord,
                       const DirectionVector & initialPhotonDir, 
                       double initialPhotonRadius, 
                       double initialPhotonPhi,
                       int numIncidentAngles,
                       const vector1Ddbl & incidentAngles, 
                       const vector2Ddbl & frontRefl, 
                       const vector2Ddbl & frontTran, 
                       const vector1Ddbl & frontTauPermm, 
                       int numRoughAngles, 
                       const vector1Ddbl & roughAngles,
                       const vector2Ddbl & roughRefl,
                       const vector1Ddbl & roughtaupermm, 
                       const ExternalObjectsStructure & topExtObjects, 
                       const ExternalObjectsStructure & bottomExtObjects, 
                       const vector1Ddbl & topTransProb,   // slice of reflTranGrid.m_topExtObjectsTransProb, for a single energy
                       const vector1Ddbl & bottomTransProb,   // slice of reflTranGrid.m_bottomExtObjectsTransProb, for a single energy
                       int scatenergyindex,
                       const Scattering & scat, 
                       const SectorsShells & sectorsShells, 
                       const std::vector<int> & loopSectorNumbers, 
                       int lowShell, 
                       int highShell,
                       const GenTelescope & genTelescope,
                       const std::vector<HousingGeometry> & housings, 
                       double housingHardLimit,
                       long numxrtzlevels, 
                       const vector1Ddbl & xrtzlevels,
                       double resultsplanez, 
                       int maxNumPathCoords, 
                       const Transforms & transforms, 
                       const std::vector<long> & zmaxsortxrtobjectindexAll, 
                       int numXRTObjects, 
                       const std::vector<XRTObject> & XRTObjects, 
                       const std::vector<int> & obstructionIntervalLowIndex, 
                       const std::vector<int> & obstructionIntervalHighIndex, 
                       int & numInteractions, 
                       std::vector<CartesianCoord> & pathCoords, 
                       std::vector<DirectionVector> & pathDirs,
                       vector2Dint & pathCode, 
                       std::vector<int> & pathXRTObjectID,
                       CartesianCoord & finalPhotonPos,
                       DirectionVector & finalPhotonDir, 
                       bool & resultsPlaneWasImpacted, 
                       std::vector<int> & pathErrorCode,
                        
                       std::vector<double> & pathIncidentAngle,
                       std::vector<double> & pathScatteringAngle) {
  // +++ 20140918 pathIncidentAngle is for testing
  
  AH_DEBUG << "initialPhotonCoord = " << initialPhotonCoord << std::endl;
  AH_DEBUG << "initialPhotonDir = " << initialPhotonDir << std::endl;
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // set a large negative number to use when writing coords for photons that 
  // don't make it to the results plane
  const double largeDummy = -1.0e30;
  
  // output from makePhotonBBox
  PhotonBoundingBox photonBBox;
  int photonBBoxError;
  
  double zendbox = 0.0;
  
  // The variable zLevelCtr will be incremented sequentially in the main 
  // while-loop below (while photon is not absorbed) to point to the 
  // z-coordinates in xrtzlevels(), which mark key z- divisions in the 
  // telescope. The values in xrtzlevels will be used in making photon 
  // bounding boxes that are not larger than they need to be (reducing the 
  // number of potential impact candidates). The zeroth value is the telescope 
  // aperture, so start with the next z-level.
  int zLevelCtr = 1;
  
  // output from interceptXRTObjects() and getImpactCandidates() (fastmode)
  // initially size this to numXRTObjects.  It will stay this size inside 
  // getImpactCandidates(), where it will just be reset to zero each time in 
  // function.  In interceptXRTObjects, it is cleared and resized to a different
  // size each call.
  std::vector<int> candidateObjectList(numXRTObjects);
  
  int newIdxLow = 0;
   
  // flag for photon absorption; if/when this flag is >0, an absorption event 
  // has occurred and the photon path will be terminated.
  int photonAbsorbed = 0;
  //  0   means not absorbed
  //  1   means absorbed on results plane
  //  2   means +++ missing from trf
  //  3   means absorbed on a side wall
  //  4   means absorbed on a housing wall
  //  5   means absorbed on a support structure
  //  6   means absorbed in a foil body coming in from an "edge"-face, 
  //          terminating in the body
  //  7   means absorbed in a foil after entering the front or back face surface
  //  8   means Effectively terminated due to photon incident angle falling 
  //          outside the reflectivity/transmission angle grid.
  //  9   means absorbed by an external object above the telescope
  //  10  means absorbed by an external object below the telescope
  //  11  means photon misses telescope
  //  999 means absorbed in a mirror/pre-collimator foil after "throwing the 
  //            dice" for reflection and transmission.
  
  // reassign variables for the current photon position and direction to 
  // initial values
  CartesianCoord currPhotonPos = initialPhotonCoord;
  DirectionVector currPhotonDir = initialPhotonDir;
      
  CartesianCoord localCoord;
  
  // inputs for applying transforms
  transformType_e transformType = BOTH; // transform both position and direction

  // this empty coordinate is used to fill all the previously used static
  // vectors with default value, ready for current loop through this function
  CartesianCoord emptyCoord;
  DirectionVector emptyDir;
  
  // The following arrays beginning with "path" will hold various quantities 
  // associated with each interaction point along the photon path. The first 
  // index labels the interactions consecutively.
  //+++ what do we do with pathobjectframecoords
  // The Cartesian coordinates at the interaction (XRT object frame)
  std::vector<CartesianCoord> pathobjectframecoords(maxNumPathCoords);
  // The straight-line path length (in mm) for the photon to exit the XRT 
  // object after entering at the interaction point (used to calculate optical 
  // depth for transmission)
  std::vector<double> pathtransdistance(maxNumPathCoords);
  // The distance (in mm) between the current position and the next interaction 
  // position
  std::vector<double> pathImpactDistance(maxNumPathCoords);
  // Which face of the XRT obects was impacted 
  std::vector<long> pathFaceHit(maxNumPathCoords);
  //    0 = back
  //    1 = front
  //    2 = top edge
  //    3 = bottom edge
  //    4 = sides
  //    5 = unclassified
  //+++ add all these to faces_e, and getSurfaceNormal?
  // pathtermination=True on the interaction that the photon path terminates
  std::vector<bool> pathtermination(maxNumPathCoords);
  
  // NOTE: the following are declared static in order to help runtime.  Rather 
  // than declaring and resizing new vectors, size numXRTObjects, each time
  // this function is called, just call them static.  Later, only the entries 
  // actually used will be zeroed, rather than clearing all numXRTObjects.
  // nimpacts is declared static so that we can remember how many elements were 
  // actually used, and only reset those ones to zero.  nimpacts is static so 
  // that we can do it here in the beginning, instead of searching later in the 
  // code
  
  // Set impact counter for this group of candidates to zero. The variable 
  // nimpacts will count the potential number of XRT objects intercepted using 
  // exact calculations (the larger candidateObjectList was made using bounding 
  // boxes and approximate criteria)
  static long nimpacts = 0;

  // Define some temporary arrays to hold potential impact information. 
  // (See analogous path* arrays for explanations.)
  static std::vector<long> potentialimpactids(numXRTObjects);
  static std::vector<long> potentialimpactface(numXRTObjects);
  static std::vector<double> potentialimpactDistances(numXRTObjects);
  static std::vector<CartesianCoord> potentialimpactcoords(numXRTObjects);
  static std::vector<DirectionVector> potentialLocalDirs(numXRTObjects);
  static std::vector<CartesianCoord> potentialobjframeimpactcoords(numXRTObjects);
  static std::vector<double> potentialtransdistance(numXRTObjects);
  static std::vector<long> potentialtermination(numXRTObjects);
  static std::vector<long> potentialerrorCode(numXRTObjects);
  
  // fill the used static vectors with 0s
  // first time in function, nimpacts=0, everything is default zero anyway.
  // if this isn't first time in here, then nimpacts has been updated, and 
  // vectors populated up to nimpacts elements.  reset them all to zero.
  std::fill(potentialimpactids.begin(), potentialimpactids.begin()+nimpacts, 0);
  std::fill(potentialimpactface.begin(), potentialimpactface.begin()+nimpacts, 0);
  std::fill(potentialimpactDistances.begin(), potentialimpactDistances.begin()+nimpacts, 0.0);
  std::fill(potentialimpactcoords.begin(), potentialimpactcoords.begin()+nimpacts, emptyCoord);
  std::fill(potentialLocalDirs.begin(), potentialLocalDirs.begin()+nimpacts, emptyDir);
  std::fill(potentialobjframeimpactcoords.begin(), potentialobjframeimpactcoords.begin()+nimpacts, emptyCoord);
  std::fill(potentialtransdistance.begin(), potentialtransdistance.begin()+nimpacts, 0.0);
  std::fill(potentialtermination.begin(), potentialtermination.begin()+nimpacts, 0);
  std::fill(potentialerrorCode.begin(), potentialerrorCode.begin()+nimpacts, 0);
  
  // input/output for getPhotonObjectImpactCoords
  // declare inputs to getPhotonObjectImpactCoords:
  // these are specific to the particular candidate XRT object that will be  
  // considered, one at a time
  long currObjIdx = 0;
  surfaceGeometryTypes_e surfacegeometry;
  objectTypes_e objectType;
  std::vector<double> geoparams;
  double startAngle = 0.0;
  double endAngle = 0.0;
  double startAngleMod = 0.0;
  double endAngleMod = 0.0;
  std::vector<CartesianCoord> vertices;
  int numvertices = 0;
  Slopes slopes;
  ObjectBoundingBox boundingbox;
  // declare outputs from getPhotonObjectImpactCoords:
  int faceHit = 0;
  int exitface = 0;
  CartesianCoord impactcoord;       // Impact coordinates in the XRT frame 
  CartesianCoord stdcoord;          // Impact coordinates in the object frame (object in "standard position")
  double impactDistance = 0.0;      // Distance between current position and impact point (=time) 
  double pathLength = 0.0;          // Straight line distance between entry and exit points
  int errorCode = 0;                // Error condition (0 = no error)
  bool killThePhoton = true;        // true if the photon is tagged to be terminated

  // used to find distances of impacts later, and which object was impacted
  double minImpactDistance = 0.0;
  long minImpactDistanceIndex = 0;
  
  // for keeping track of the objectid that is impacted
  // defaut to -1 so we can check before getImpactCandidates whether an actual 
  // object has been impacted in an interaction.  This will enable the thermal
  // shield to be interacted with, but still allow a dynamic seach for shells
  int impactedXRTObjectID = -1;
  
  // input/output for sidewallImpact
  // tanthetawall1 corresponds to side wall 1 associated with startangle
  // tanthetawall2 corresponds to side wall 2 associated with endangle
  // potentialimpactcoords (xobj, yobj, zobj) are the potential impact coordinates of the XRT object
  // impactCoords (xwallimpact,ywallimpact, zwallimpact) are the impact coordinates on the nearest side wall.
  double tanthetawall1 = 0.0;
  double tanthetawall2 = 0.0;
  CartesianCoord objectCoords;
  CartesianCoord impactCoords;
  bool objimpacted = false;
  int wallid = 0;
  double sidewallImpactDistance = 0.0;    // +++ this is used below - what is it???

  // flag that a sidewall was potentially hit   // +++ is this a correct description?
  bool sidewallpotentialhit = false;
  
  // this will be input to sidewallimpact.  set default to false.  it will be 
  // set to true later if an impact is possible.
  bool checkfoilimpact = false;

  // indicate whether a particular XRT object is to be excluded for 
  // consideration for finding a ray-object intercept, usually the object that 
  // the photon is currently interacting with. A negative value means do not 
  // exclude any objects, which is appropriate for the initial photon.
  int excludedID = -1;

  // variables used if photon impacts a foil
  double thickTransmissionProb = 0.0;
  double thicktau = 0.0;
  bool angleOutOfRange = false;
  // inputs to getSurfaceNormal:
  surfaceGeometryTypes_e currSurfaceGeometry;
  objectTypes_e currobjectType;
  Slopes currSlopes;
  std::vector<double> currGeoParams;
  // outputs to getSurfaceNormal:
  CartesianCoord normalPos;
  int normalError = 0;
  // other variables:
  int currObjectSet = 0;
  std::vector<double> reflectionGrid;
  long numReflGridPts = 0;
  
  // initial pointer to first XRTObject, to test for impact in z-sorted 
  // xrtobject arrays
  int sortedIdxLow = 0;
  
  // will be a random number, if foil surface front or back is impacted
  double chooseoutcome = 0.0;         
  
  int sidewalloutcome = 0;            // output from sidewallImpact()
  double twallimpact = 0.0;           // output from sidewallImpact()
  
  // doRadialRejection could be changed to "False" if doing so would increase 
  // the speed (trade-off between applying a radial rejection criterion that 
  // would decrease the number of impact candidates, versus larger number of 
  // candidates but without the overhead of testing for radial bounds):
  const bool doRadialRejection = true;  // input for interceptXRTObjects()
  
  int numCandidates = 0;                // output from interceptXRTObjects()
  long reflindex = 0;
  std::vector<double> transmissionGrid; // input to bisectionInterp()
  std::vector<double> angleGrid;
  double angleGridMin = 0.0;
  double angleGridMax = 0.0;
  double thinTransmissionProb = 0.0;
  double absorptionprob = 0.0;
  double reflectionProb = 0.0;
  double nettransmissionprob = 0.0;
  double abslobound = 0.0;
  double abshibound = 0.0;
  double refllobound = 0.0;
  double reflhibound = 0.0;
  double transmissionlobound = 0.0;
  double transmissionhibound = 0.0;
  double grazingIncidentAngle = 0.0;
  double singrazingIncidentAngle = 0.0;
          
  // inputs to getScatteredDirection()
  bool isRoughScattering = false;
  int scatsegmentid = 0;
  int scatcolumnindex = 0;
  // output to getScatteredDirection()
  double scatteredangle = 0.0;

  // inputs and outputs to getreflectiondirection()
  // input:
  bool isSpecular = false;        // flag to do specular reflection
  DirectionVector localDir;       // input to getreflectiondirection(), etc
  DirectionVector normalDir;      // input to getreflectiondirection(), output to getSurfaceNormal(), etc
  double sinReflected = 0.0;      // input to getreflectiondirection()
  // output:
  DirectionVector newLocalDir;    // output to getreflectiondirection()
  int reflErrorCode = 0;          // output from getreflectiondirection().  +++ not used?
  
  // after the candidate loop, we populate this with the chosen localDir
  DirectionVector selectedLocalDir;
  
  // outputs from getXYForNewZ()
  CartesianCoord resultPlaneCoord;
  double tresultsplane = 0.0;
  
  // error values for pathErrorCode
  const int tooManyInteractionsCode = 998;
  const int angleOutOfRangeCode = 999;
  
  // ----------------------------
  // housing variables:
  // ----------------------------
  
  // output from housingImpactPositions
  CartesianCoord housingImpact;       // coords of possible impact w/ housing
  double housingImpactDistance = 0.0; // Impact distance (=impact time)
  // flag for a potential impact because at this stage we don't know if 
  // photon will impact another XRT object before hitting the potential 
  // housing impact point
  int housingPotentiallyHit = 0;
  // -1 no impact possible
  //  0 potential pre-collimator housing hit 
  //  1 potential primary mirror housing hit
  //  2 potential secondary housing hit
  int housinginnerouter = 0;         
  //  0 = 
  //  1 = 
  // +++ use housingSides_e housingSide; // INNER or OUTER ??  need to change code below if so
    
  
  // ----------------------------
  // initialize output variables:
  // ----------------------------
  
  // Set the number of interactions counter to 1 because the zeroth interaction 
  // is the initial telescope entry event. As an index, it will always point to 
  // quantities relating to the current ray-object interaction
  // +++ make sure all comments about this are clear
  // numInteractions starts as 1 (initial position is the first interaction),
  // but it's to be used as an index for arrays later, which are 0-based. 
  numInteractions = 1;
  
  
  std::fill(pathCoords.begin(), pathCoords.end(), emptyCoord);
  std::fill(pathDirs.begin(), pathDirs.end(), emptyDir);
  for (int iInteraction = 0 ; iInteraction < maxNumPathCoords ; ++iInteraction) {
    std::fill(pathCode[iInteraction].begin(), pathCode[iInteraction].end(), 0);
  }
  std::fill(pathXRTObjectID.begin(), pathXRTObjectID.end(), 0);
  std::fill(pathErrorCode.begin(), pathErrorCode.end(), 0);
  std::fill(pathIncidentAngle.begin(), pathIncidentAngle.end(), 0.0);
  std::fill(pathScatteringAngle.begin(), pathScatteringAngle.end(), 0.0);
  
  // set the final position to be a large negative number.  If photon doesn't 
  // reach the focal plane, this will remain the final position, to alert user
  // that this coord is not valid (in case user naively tries to plot all final
  // positions without filtering on resultsplane column in history file)
  finalPhotonPos.m_x = largeDummy;
  finalPhotonPos.m_y = largeDummy;
  finalPhotonPos.m_z = largeDummy;
  
  // set the final photon direction to be all 0 (not physically possible). 
  // it will be set to a valid number only if photon reaches results plane
  finalPhotonDir.m_xDir = 0.0;
  finalPhotonDir.m_yDir = 0.0;
  finalPhotonDir.m_zDir = 0.0;
  
  // initialize this to false.  We'll only set it to true later only if we hit
  // the results plane (because we 'return' out of function if photon is 
  // terminated)
  resultsPlaneWasImpacted = false;
  
  
  // finished initializing variables
  // -------------------------------------
  
  
  // set the first entries for the path and photon direction history to the 
  // initial (input) values. This will be zeroth interaction.
  pathCoords[0] = initialPhotonCoord;
  pathDirs[0] = initialPhotonDir;
    
  // +++ vvv same as raytracePhotonFast vvv
  // +++++++ new stuff   v
  
  // +++ if I make a function checkPhotonInTop()
  // inputs
    // const ExternalObjectsStructure & topExtObjects;
    // const ExternalObjectsStructure & bottomExtObjects;
    // const vector1Ddbl & topTransProb;   // slice of reflTranGrid.m_topExtObjectsTransProb, for a single energy
    // const vector1Ddbl & bottomTransProb;   // slice of reflTranGrid.m_bottomExtObjectsTransProb, for a single energy
    // initialPhotonRadius
    // initialPhotonPhi
  // outputs 
    // photonAbsorbed
    // pathcoordinates
    // pathphotondir
    // pathCode
    // numInteractions

  
  // Before entering the actual telescope, first check if the photon survives 
  // passage through external objects above the telescope, if there are any:

  // Loop over each object part to check if the object intercepts the part: 
  // we only need one match - it is assumed that no object part overlaps with 
  // any other so there is no ambiguity about the transmission probability.
  // There may be gaps, so the photon may not necessarily interact with a top
  // object.
  for (int iObject = 0 ; iObject < topExtObjects.m_numExtObjectParts; ++iObject) {

    AH_DEBUG << "iObject = " << iObject << std::endl;

    if ( (initialPhotonRadius >= topExtObjects.m_rMin[iObject]) && 
          (initialPhotonRadius <= topExtObjects.m_rMax[iObject]) && 
          (initialPhotonPhi >= topExtObjects.m_startAngleRad[iObject]) && 
          (initialPhotonPhi <= topExtObjects.m_endAngleRad[iObject]) ) {

      AH_DEBUG << "topTransProb[iObject] = " << topTransProb[iObject] << std::endl;

      if (topTransProb[iObject] == 0.0) {
        // object is to be treated as an obstruction and photon is terminated 
        // (topTransProb(i) was set 0.0 earlier if the mass-absorption index 
        //  was negative)
        photonAbsorbed = 9; 
      } else {
        // Generate a random number to see if the photon will be transmitted
        double topXRTObjectRandomTrans = getRandom();
        if (topXRTObjectRandomTrans > topTransProb[iObject]) { 
          // Photon is absorbed if the random number is > trans. prob.
          photonAbsorbed = 9;
        }
      }

      // Whatever the outcome, save the photon coordinates and direction (which 
      // should actually just be the same as the initial values since the top 
      // xrt objects are at the telescope aperture)
      pathCoords[numInteractions] = currPhotonPos;
      pathDirs[numInteractions] = currPhotonDir;

      // Note that even if the photon is transmitted, we leave the photon in 
      // the current position because the main telescope components may be 
      // misaligned: if the photon misses the telescope because of the finite 
      // distance between the external object and the telescope entrance, the 
      // correct outcome will be automatically determined by the algorithm that 
      // determines impacts between the photon and telescope components because 
      // there is no requirement that the initial photon position has to be at 
      // the telescope entrance. If the photon is transmitted, it is as if the 
      // external object is not present once the outcome of successful 
      // transmission is determined.

      // Set the pathCode depending on the outcome; 
      // if the photon was absorbed return, 
      // if not continue after incrementing counter for number of interactions 
      //   ready for the next interaction.
      if (photonAbsorbed > 0) {
        pathCode[numInteractions][A]  = 1;
        pathCode[numInteractions][BC] = 9;
        pathCode[numInteractions][D]  = 1;
        AH_DEBUG << "about to return" << std::endl;
        return;
      } else {
        pathCode[numInteractions][A]  = 4;
        pathCode[numInteractions][BC] = 9;
        pathCode[numInteractions][D]  = 1;
        numInteractions++;
        AH_DEBUG << "topXRTObjectIntercepted = true" << std::endl;
        // there was in interaction with a top object, we can stop searching
        break;
      }

    } // end-if photon position is inside external object i (i.e. if an 
      // intercept occurred)

  } // end-loop checking for photon-object intercept, over top xrt objects
  
  
  // +++ if I make a function checkPhotonInTop()
  // we checked the passage through any external objects above the telescope.  
  // If the photon was absorped there, we already set the appropriate pathCode,
  // now return out of the raytrace function.
//  if (absorbedInTopObject) {
//    return;
//  }
  // +++ ^^^ same as raytracePhotonFast ^^^
  
//  // 141122 Previously there was no special treatment in raytraceonephoton() if the initial photon position is inside the central hole because the algorithm automatically took account of possible interaction with the inner housing walls. However, the inner housing wall may not have a simple structure (described in the AZIMUTHALSTRUCT extension of the TDF) so we have to to account of passage through the hole explicitly. This part of the code is identical to that in raytracephotonfast() up to the next “============”.
  
  // +++ vvv same as raytracePhotonFast vvv
  
  // If the photon makes it to this point we have to first check if the photon falls through the central hole instead of the main telescope containing the mirror foils and support structures.
  double tothousingshifts = housings[PRIMARY].m_xShift + 
                            housings[PRIMARY].m_yShift + 
                            housings[SECONDARY].m_xShift + 
                            housings[SECONDARY].m_yShift;
  int hstart = 0;
  
  AH_DEBUG << "tothousingshifts = " << tothousingshifts << std::endl;
  AH_DEBUG << "initialPhotonRadius = " << initialPhotonRadius << std::endl;
  AH_DEBUG << "genTelescope.m_innerhousingradius = " << genTelescope.m_innerhousingradius << std::endl;
  
  // this will later hold the top coordinates as we move photon through
  // the housings and the central hole
  CartesianCoord topCoord;  // +++ trf: xtop, ytop, ztop

  // projected coord
  CartesianCoord projCoord; // +++ trf: xproj,yproj,zproj
  // for output for getXYForNewZ for coord projCoord
  double tproj = 0.0; 
  // after calling getXYForNewZ for coord projCoord, call cartesianToPolar
  // for radius and phi of the projected coord
  double rproj = 0.0;
  double phiproj = 0.0;

  // whether or not the photon escapes the hole
  bool photonEntersTelescope = false;
    
  if (initialPhotonRadius < genTelescope.m_innerhousingradius) {
    // Photon goes through hole at aperture but we need to check:
    // If the AZIMUTHALSTRUCT extension in the TDF supplied information about 
    // the finite width of the inner housing structure 
    // (i.e. numcentralextobjectparts>0);
    // if so, and the height of the inner housing does not reach the top of the 
    // pre-collimator, whether the photon leaves the hole and enters the main 
    // telescope or whether the hits an inner housing wall on the way down to 
    // the results plane.
    // This must be done in steps because of possible misalignments between 
    // housing units. Project photon to top of "thick" housing wall, or bottom 
    // of pre-collimator housing if no "thick" housing information is supplied; 
    // then to the bottom of the primary housing, and then the secondary 
    // housing. Compare the radial coordinate at the impact point to the radial 
    // coordinates of the relevant telescope parts to determine the outcome. If 
    // a housing wall is impacted, terminate the photon.
    
    if ( (tothousingshifts == 0.0) && 
          (genTelescope.m_numcentralextobjectparts == 0) ) {
      // go straight to results plane if there are no housing shifts and there 
      // is no housing structure inside innerhousingradius
      hstart = 3; 
    } else {
      if (pcolExists) {
        hstart = 0;
      } else {
        hstart = 1;
      }
    } // end-if extra structure in the hole and if there are no misalignments

    int iHousing = hstart; // +++ this is the housing number PCOL, PRIMARY, SECONDARY
    
    // for the first housing unit there are no x/y shifts by definition so don’t need to correct xtop,ytop,ztop
    topCoord = currPhotonPos;
    
    AH_DEBUG << "iHousing = " << iHousing << std::endl;
    
    // Note that we do not update the photon position while we are testing for 
    // housing wall impacts: if there is an impact the photon is terminated 
    // anyway, and if there is no impact, the current photon position and 
    // direction can still be used to determine the impact coordinates of the 
    // next interaction, whatever that may be. This is true even when the 
    // photon escapes through a gap in the inner housing wall through to the 
    // main telescope.
    
    while ( (photonAbsorbed == 0) && (iHousing <= s_numHousings) ) {
      
      AH_DEBUG << std::endl;
      
      if (iHousing > hstart) {
        // correct photon position for housing shifts 
        topCoord.m_x = currPhotonPos.m_x + housings[iHousing].m_xShift;
        topCoord.m_y = currPhotonPos.m_z + housings[iHousing].m_yShift;
      }
      
      // If we are doing the pre-collimator, and if there is a "thick" wall 
      // inside the hole, we have to first project to the top of this structure 
      // to see if it can pass through into the main telescope area; if not it 
      // could hit the top of this structure, or pass into the hole.
    
      if (iHousing == 0) {
        if (genTelescope.m_numcentralextobjectparts > 0) {
          
          projCoord.m_z = genTelescope.m_holewalltopz;
          
          getXYForNewZ(topCoord, currPhotonDir, projCoord, tproj);
          cartesianToPolar(projCoord, rproj, phiproj);
          
          if ( (rproj >= genTelescope.m_holesideinnerradius) && 
               (rproj <= genTelescope.m_innerhousingradius) ) { 
            photonAbsorbed = 4; // photon hits top face of short housing wall
          } else if (rproj > genTelescope.m_innerhousingradius) {
            // photon escapes over out of the hole into the main telescope
            photonEntersTelescope = true;
          }
        
        } // end-if inner housing wall has structure
      } // end-if i=0

      // If the photon did not escape through the gap at the top of the pre-col 
      // inner housing, project it down to the bottom of the pre-collimator 
      // level, the primary, the secondary (i=0..2) or then finally to the 
      // focal plane (i=3).
      if (!photonEntersTelescope) {
        
        if ( (iHousing < 3) && (!photonEntersTelescope) ) {
          // housing.zLower is from PCOLZMIN, PRIMZMIN, or SECDZMIN
          projCoord.m_z = housings[iHousing].m_zLower;
        } else if (iHousing == 3) {
          projCoord.m_z = resultsplanez;
        }
        
        getXYForNewZ(topCoord, currPhotonDir, projCoord, tproj);
        cartesianToPolar(projCoord, rproj, phiproj);
        
        if (rproj >= genTelescope.m_holesideinnerradius) {
          if (iHousing < 3) {
            // photon is absorbed by inner housing wall
            photonAbsorbed = 4;
          } else if (iHousing == 3) {
            photonAbsorbed = 1;
          }
        } else {
          // the photon goes to the focal plane unimpeded.  
          // if other objects are added to inside of telescope, may need to modify this
          photonAbsorbed = 1;
        }
        iHousing++;
        
    AH_DEBUG << "photonAbsorbed = " << photonAbsorbed << std::endl;
    
      } // end-if photon did not escape through gap at the top of the pre-collimator housing unit
      
    } // end while
    
  } else if (initialPhotonRadius == genTelescope.m_innerhousingradius) {
    
    photonAbsorbed = 4;
    
  } // end-if initial photon position is inside hole or on the top edge

  if (photonAbsorbed == 4) { 
    pathCode[numInteractions][A]  = 1;
    pathCode[numInteractions][BC] = 2;
    pathCode[numInteractions][D]  = 2;
    return;
  } else if (photonAbsorbed == 1) { 
    pathCode[numInteractions][A]  = 1;
    pathCode[numInteractions][BC] = 1;
    pathCode[numInteractions][D]  = 1;
    pathCoords[numInteractions].m_x = projCoord.m_x;
    pathCoords[numInteractions].m_y = projCoord.m_y;
    pathCoords[numInteractions].m_z = resultsplanez;
    finalPhotonPos = pathCoords[numInteractions];
    finalPhotonDir = currPhotonDir;
    resultsPlaneWasImpacted = true;
    return;
  }
  
  // If we reach this point, the photon has passed through external objects but either did not enter the hole, or entered but "escaped" into the main telescope through a gap in the inner housing wall at the pre-collimator level. Since the external objects are in general some height above the main telescope components, it is possible that the photon will miss the telescope at the outer perimeter. To test this we project the photon to the z-coordinate of the top of the main telescope and see if its radial coordinate is larger than the telescope outer housing radius. We don’t need to do this for photons that entered the hole and escaped into the main telescope.
  // we only need to check for this if there were top external objects // +++ added 20141215.  pass in param, to use doTopExternalObjects?  or add bool to topExtObjects
  
  if ( (topExtObjects.m_numExtObjectParts > 0) && 
       (initialPhotonRadius > genTelescope.m_innerhousingradius) ) {
    topCoord = currPhotonPos;
    projCoord.m_z = xrtzlevels[0]; // z-coordinate of top of main telescope
    getXYForNewZ(topCoord, currPhotonDir, projCoord, tproj);
    cartesianToPolar(projCoord, rproj, phiproj);
    
    if (rproj >= genTelescope.m_outerhousingradius) {
      // photon misses the outer housing wall
      photonAbsorbed = 11;
      AH_DEBUG << "pathcode is 9989 !!" << std::endl;
      pathCode[numInteractions][A]  = 9;
      pathCode[numInteractions][BC] = 98;
      pathCode[numInteractions][D]  = 9;
      return;
    }
  } // end-if photon did not come through central hole
  
  // +++ ^^^ same as raytracePhotonFast ^^^
  
  // Initialize the pointer used to track which set of foils have been impacted so far: 0=no primaries or secondaries; 1=at least 1 primary, 2=at least 1 secondary. This will be used by the impact candidate selection routine getimpactcandidates()
  int zGroupPointer = 0;

  AH_DEBUG << "BEFORE MAIN WHILE-LOOP" << std::endl;
  
  // begin the main loop that follows the photon to the lowest z-coordinate of 
  // any XRT object in the telescope. The photon is tested for interactions 
  // with groups of XRT objects starting with groups with the highest 
  // z-coordinates, and then if necessary incrementally going lower in the 
  // z-coordinate (closer to focal plane).  The loop ends when the photon is 
  // either absorbed or survives through to the lowest z-coordinate of XRT 
  // objects in telescope (after which it is propagated to the results plane)
  while ( (photonAbsorbed == 0) && (zLevelCtr < numxrtzlevels) ) {
    
    AH_DEBUG << std::endl;
    AH_DEBUG << "start of while loop" << std::endl;
    AH_DEBUG << "zLevelCtr = " << zLevelCtr << std::endl;
    AH_DEBUG << "numInteractions = " << numInteractions << std::endl;
    
    // because numInteractions is used as an array index, and the arrays are all 
    // resized to maxNumPathCoords, we can't allow numInteractions to reach
    // maxNumPathCoords.  This shouldn't happen, but just in case it does,
    // set an error code, then stop tracing this photon
    if (numInteractions >= maxNumPathCoords-1) {
      // +++ add this code to description in .h file
      pathCode[numInteractions][0] = 9;
      pathCode[numInteractions][1] = 9;
      pathCode[numInteractions][2] = 9;
      pathErrorCode[numInteractions] = tooManyInteractionsCode;
      AH_DEBUG << "Number of interactions for current photon reached the maximum allowed (" <<
                  maxNumPathCoords << ")" << std::endl;
      // +++ instead of returning, use this error code as a check after this loop
      return;
    }
    
    // Save the current photon direction here in the path history because it is 
    // the direction just before impact in the *next* interaction that we want 
    // to save and it will not change before the impact. If there will not be a 
    // next interaction, we will not reach this point in the do-while loop.
    pathDirs[numInteractions] = currPhotonDir;

    // zendbox sets the z-coordinate of the bottom of the photon bounding box. 
    // start with zendbox=2nd z position and if no XRT object candidates are 
    // intercepted by the photon, increment the value of zendbox, make another 
    // photon bounding box, and search for ray-object intercepts. This is 
    // repeated until a ray-object intercept is found. The values of zendbox 
    // are incremented by sequentially using the values in the input array 
    // zxrtlevels. Note that the 1st z-level is the telescope aperture 
    // z-coordinate.
    
    // However, first check that the current photon position has not gone below the current z-level. If it has, drop down in z-levels until the current photon position is above the z-level
    while (currPhotonPos.m_z <= xrtzlevels[zLevelCtr]) {
      zLevelCtr++;
      if (zLevelCtr >= numxrtzlevels) {
        AH_DEBUG << "zLevelCtr >= numxrtzlevels!!!!" << std::endl;
        // +++ this means that in the last loop iteration, currPhotonPos.m_z was set to 0.  need to find how and fix it
        break;
      }
    }
    AH_DEBUG << "zLevelCtr = " << zLevelCtr << " (after incrementing while (curr z <= xrtzlevels[zLevelCtr]))" << std::endl;
    
    zendbox = xrtzlevels[zLevelCtr];
     
    // fastmode experiment 4, comment this line
    // Make the photon bounding box using the current value of zendbox 
    makePhotonBBox(currPhotonPos, currPhotonDir, zendbox, housingHardLimit, photonBBox, photonBBoxError);
    
    // Test for possible inner or outer housing impact by by the photon (there are several housing units and they may not be co-aligned). The routine returns parameters of only one possible impact. A flag is set only for a potential impact because at this stage we do not yet know if the photon will impact another XRT object before hitting the potential housing impact point:
    
    // NOTE: Later, in the interest of speed, the routine should only be called under stricter conditions, e.g. if the initial photon position was between a housing wall and foil. However, during testing leave it as it is because we don’t know yet what to expect for stray light and X- ray background photons.
    housingImpactPositions(housings, currPhotonPos, currPhotonDir, housingPotentiallyHit, housinginnerouter, housingImpactDistance, housingImpact);
    
    // Now call a subroutine that loops over a relevant range of XRT object bounding boxes, and using the photon bounding box comes up with a candidate shortlist of XRT objects that can potentially be impacted by the photon. If the routine is being called on a second or subsequent pass, it will not search XRT objects that were already rejected on a previous pass (because the output "newIdxLow" is updated on every pass so that on the next pass, sortedIdxLow is set equal to newIdxLow from the previous pass. The xrtobjects searched on each pass are those that have indices lying between sortedIdxLow and sortedIdxHigh with respect to the zmin- sorted index array zminsortedobjectindex.
    
    // If this is the first time in this loop, use a new array
    if (numInteractions == 1) {
      newIdxLow = 0; 
    }
    
    // +++ 20150112 adding new section to dynamically choose which shells to 
    // search through m_shellNumber
    // only find new shells if we've already had an interaction.  The thermal 
    // shield doesn't count, so also check impactedXRTObjectID.
    if ( (numInteractions > 1) && (impactedXRTObjectID >= 0) ) {
      int lastInteractionObj = 0;
      int currShellNum = 0;
      lastInteractionObj = pathXRTObjectID[numInteractions-1];
      currShellNum = XRTObjects[lastInteractionObj].m_shellNumber;
      lowShell  = std::max(currShellNum - sectorsShells.s_deltaShells, 1);
      highShell = std::min(currShellNum + sectorsShells.s_deltaShells, sectorsShells.m_numShells);
      
      AH_DEBUG << "pathXRTObjectID[numInteractions] = " << pathXRTObjectID[numInteractions] << std::endl;
      AH_DEBUG << "pathXRTObjectID[numInteractions-1] = " << lastInteractionObj << 
              " shell=" << currShellNum << 
              " lowShell=" << lowShell << 
              " highShell=" << highShell << std::endl;
    }
    
    // interceptXRTObjects is more exact; getImpactCandidates is faster
    if (doFastMode) {
      AH_DEBUG << "fast mode" << std::endl;
      getImpactCandidates(photonBBox, numXRTObjects, XRTObjects, excludedID, obstructionIntervalLowIndex, obstructionIntervalHighIndex, zGroupPointer, sectorsShells.m_numLoopSectors, loopSectorNumbers, lowShell, highShell, sectorsShells, numCandidates, candidateObjectList);
    } else {
      AH_DEBUG << "not fast mode" << std::endl;
      interceptXRTObjects(photonBBox, sortedIdxLow, numXRTObjects, zmaxsortxrtobjectindexAll, XRTObjects, excludedID, doRadialRejection, newIdxLow, numCandidates, candidateObjectList);
    }
    
    // Set impact counter for this group of candidates to zero. The variable nimpacts will count the potential number of XRT objects intercepted using exact calculations (the larger candidateObjectList was made using bounding boxes and approximate criteria)
    nimpacts = 0;
    
    AH_DEBUG << "before candidate loop. numCandidates = " << numCandidates << std::endl;
      
    // Loop over candidates and determine if an impact is possible; if so, find 
    // the distance between current position and impact point in each case
    for (int iCand = 0 ; iCand < numCandidates ; ++iCand) {
      
      AH_DEBUG << "candidateObjectList["<<iCand<<"] = " << candidateObjectList[iCand] << std::endl;
//      AH_DEBUG << "m_xSegmentRot = " << XRTObjects[candidateObjectList[iCand]].m_xSegmentRot << std::endl;
//      AH_DEBUG << "m_xSegmentShift = " << XRTObjects[candidateObjectList[iCand]].m_xSegmentShift << std::endl;
//      AH_DEBUG << "m_ySegmentRot = " << XRTObjects[candidateObjectList[iCand]].m_ySegmentRot << std::endl;
//      AH_DEBUG << "m_ySegmentShift = " << XRTObjects[candidateObjectList[iCand]].m_ySegmentShift << std::endl;
//      AH_DEBUG << "m_zSegmentRot = " << XRTObjects[candidateObjectList[iCand]].m_zSegmentRot << std::endl;
//      AH_DEBUG << "m_zSegmentShift = " << XRTObjects[candidateObjectList[iCand]].m_zSegmentShift << std::endl;
      
      // ===============================================================================
      //  vv ALL REFLECTION, TRANSMISSION, & SCATTERING TAKES PLACE IN THIS BLOCK vv
            
      // transform the photon position and direction into the XRT object frame
      // In the object frame the position and direction vector components will have names beginning with local. The bool doTransforms is first checked in case transformations were turned off or happened to be all zero.
      if (transforms.m_doTransforms) {
        // +++ should we have a flag to see if this particular object has a doTransforms?  (also when calling xrttransformsetup)
        
        // Since we are transforming the photon and not the object, the "reverse" transformation is the correct one: i.e. the photon is initially in the object frame in which the object is in default position (untransformed) and we transform the photon out to the XRT frame in which the object would appear to be transformed.
        transformType = BOTH; // transform both position and direction
        applyXRTTransform(transformType, currPhotonPos, currPhotonDir, transforms.m_object2xrtframe[candidateObjectList[iCand]], localCoord, localDir, candidateObjectList[iCand]);
        
      } else {
        localCoord = currPhotonPos;
        localDir = currPhotonDir;
      }
      
      // Call routine to determine if an impact is possible (which returns the impact distance and impact coordinates if it is).
      // The routine will set a variable "faceHit" as follows:
      //  -1: No impact
      //   0: back surface (outside of convex shape)
      //   1: front surface (inside of convex shape)
      // 2-5: edges of mirrors and foils (see routine for details)
      // exitface is set with analgous values (corresponding to the straight-line exit point of a ray). Other output variables are also set (see routine for details)
      
      
      // Set some input parameters for the routine that are specific to the particular candidate XRT object that is being treated
      currObjIdx = candidateObjectList[iCand];
      // currObject goes out of scope after this candidate for-loop
      const XRTObject & currObject = XRTObjects[currObjIdx];
      // +++ it would be cleaner and easier if I just passed in a reference to the whole object, instead of assigning all these variables.
      surfacegeometry = currObject.m_geometry;
      objectType = currObject.m_type;
      geoparams = currObject.m_geoParams;
      startAngle = std::abs(currObject.m_sideWalls[0]);
      endAngle = std::abs(currObject.m_sideWalls[1]);
      startAngleMod = currObject.m_sideWalls[4];
      endAngleMod = currObject.m_sideWalls[5];
      vertices = currObject.m_vertices;
      numvertices = currObject.m_numVertices;  // +++ according to above line in TRF, this is 8.  why store?
      slopes = currObject.m_slopes;
      boundingbox = currObject.m_BBox;    // +++ so here is a case where we don't need radial bounds
      
      getPhotonObjectImpactCoords(currObjIdx, objectType, surfacegeometry, has3DObjects, slopes, geoparams, numvertices, vertices, boundingbox, startAngle, endAngle, startAngleMod, endAngleMod, currPhotonPos, currPhotonDir, localCoord, localDir, faceHit, exitface, impactcoord, stdcoord, impactDistance, pathLength, errorCode, killThePhoton);
      
      AH_DEBUG << "impactcoord = " << impactcoord << std::endl;
      AH_DEBUG << "stdcoord    = " << stdcoord << std::endl;
      
      // initialize this before it's set later
      sidewallpotentialhit = false;
      
      // If an impact is possible and an impact distance exists, find the actual 
      // XRT object that is hit (if any)
      if (faceHit >= 0) {
        
        // Store the candidate id in a temporary array potentialimpactids and the 
        // corresponding distances in potentialimpactDistances, and also store 
        // the impact coordinates in analagous arrays
        potentialimpactids[nimpacts] = candidateObjectList[iCand];
        potentialimpactface[nimpacts] = faceHit;
        potentialimpactDistances[nimpacts] = impactDistance;
        potentialimpactcoords[nimpacts] = impactcoord;
        potentialLocalDirs[nimpacts] = localDir;
        potentialobjframeimpactcoords[nimpacts] = stdcoord;
        potentialtransdistance[nimpacts] = pathLength;
        potentialerrorCode[nimpacts] = errorCode;
        potentialtermination[nimpacts] = killThePhoton;
        
        // this will be input to sidewallimpact
        checkfoilimpact = true;
        
        // update impact counter
        nimpacts++;
        
      } // end if-block checking if there was an actual impact (faceHit>=0)
    
    } // end for-loop through candidate objects
      
    AH_DEBUG << "nimpacts = " << nimpacts << " (after looping through candidates)" << std::endl;
    
    // If there were no impacts so far we must go back and make another photon 
    // box that reaches to a lower z down the telescope using the next z-level 
    // in xrtzlevels and come up with a new set of candidates.
    if (nimpacts == 0) {
      
      // The following is to be added to improve the candidate search efficiency by moving the current photon position down to the current z-level if no impact was found. This is still over-cautious in that it will still repeat calculations if the top of the photon box lies exactly on the bottom plane of the already-rejected candidates. At the moment it seems unavoidable.
      // +++ 20151018 The following block is deleted for the moment (photon now not
      //     moved to next z-level if no impact was found) because there is still a
      //     problem under some circumstances when an object spans more than one z-level
//      CartesianCoord coordAtNewz(0.0, 0.0, xrtzlevels[zLevelCtr]);
//      double tempTime = 0.0;
//      getXYForNewZ(currPhotonPos, currPhotonDir, coordAtNewz, tempTime);
//      currPhotonPos = coordAtNewz;
      
      // if zLevelCtr exceeds numxrtzlevels, we will come out of the main
      // while-loop and go on to see where the photon impacts the focal plane.
      // It has survived going through all of the telescope body without being 
      // absorbed.
      zLevelCtr++;
      AH_DEBUG << "zLevelCtr = " << zLevelCtr << " (after incrementing again because nimpacts==0)" << std::endl;
      
      // We must ensure that XRT objects that have already been checked for 
      // impact are not checked again
      sortedIdxLow = newIdxLow;
      
      // +++ I can just 'continue' to go to next loop iteration.  the next else goes all the way to the end of the loop
      
    } else {
      // if nimpacts>0, choose the impact that has the shortest path
      
      minImpactDistance = 1.e30;
      // minImpactDistanceIndex is pointer to xrtobjects* array elements for the XRT object that is potentially hit in this interaction (even if we establish that it is hit, it will remain the xrtobject "id" because it is not stored permanantly until later).
      minImpactDistanceIndex = 0;
      for (int iImpact = 0 ; iImpact < nimpacts ; ++iImpact) {
        if (potentialimpactDistances[iImpact] < minImpactDistance) {
          minImpactDistance = potentialimpactDistances[iImpact];
          minImpactDistanceIndex = iImpact;
        }
      }
      
      impactedXRTObjectID = potentialimpactids[minImpactDistanceIndex];
      AH_DEBUG << "impacted object id = " << impactedXRTObjectID << std::endl;
      
      // Earlier, a flag was set if there was a possibility of impacting the housing structure. Now determine if the photon has to intercept a side wall before it can reach the target XRT object if it is not an obstruction (if so it will be intercepted by the side wall and the XRT object cannot be hit).
      // Note that for XRT objects for which sidewalls are not relevant, either because the object is an obstruction, or because the "wall" is virtual (so the photon can cross it), the start and/or end angle are negative.
      if ( (XRTObjects[impactedXRTObjectID].m_sideWalls[0] < 0.0) && 
           (XRTObjects[impactedXRTObjectID].m_sideWalls[1] < 0.0) ) {
        // Sidewalls are both virtual so cannot be hit
        sidewallpotentialhit = false;
        
      } else {
        
        // input/output for sidewallImpact
        // tanthetawall1 corresponds to side wall 1 associated with startangle
        // tanthetawall2 corresponds to side wall 2 associated with endangle
        // potentialimpactcoords (xobj, yobj, zobj) are the potential impact coordinates of the XRT object
        // impactCoords (xwallimpact,ywallimpact, zwallimpact) are the impact coordinates on the nearest side wall.
        tanthetawall1 = XRTObjects[impactedXRTObjectID].m_sideWalls[2];
        tanthetawall2 = XRTObjects[impactedXRTObjectID].m_sideWalls[3];
        objectCoords = potentialimpactcoords[minImpactDistanceIndex];
        
        
        // output from applyXRTTransform, input to sidewallImpact
        CartesianCoord sectorPhotonCoord;
        DirectionVector sectorPhotonDir;
        
        // Transform to the sector frame before considering side wall impact
        if (transforms.m_doTransforms) {
          transformType = DIRECTION;
          applyXRTTransform(transformType, currPhotonPos, currPhotonDir, transforms.m_sector2xrtframe[impactedXRTObjectID], sectorPhotonCoord, sectorPhotonDir, impactedXRTObjectID);
        } else {
          sectorPhotonDir = currPhotonDir;
        }
        
        sidewallImpact(sectorPhotonCoord, sectorPhotonDir, objectCoords, checkfoilimpact, tanthetawall1, tanthetawall2, objimpacted, sidewalloutcome, twallimpact, impactCoords, wallid);
        
        // wallid=1 is wall 1; wallid=2 is wall 2
        // objimpacted=true  means the XRT object was impacted, not the sidewall
        // objimpacted=false means the sidewall is impacted, not the XRT object
        // sidewalloutcome has various values, and all negative values correspond to anomalies, which should be flagged with a photonerror (TBD)
        
        sidewallImpactDistance = twallimpact;
        
        if (sidewalloutcome >= 0) {
          if (!objimpacted) {
            if ( (wallid == 1) && (XRTObjects[impactedXRTObjectID].m_sideWalls[0] >= 0.0) ) {
              sidewallpotentialhit = true;
            }
            if ( (wallid == 2) && (XRTObjects[impactedXRTObjectID].m_sideWalls[1] >= 0.0) ) {
              sidewallpotentialhit = true;
            }
          } // +++ no else?
        } else {
          // If sidewalloutcome is anomalous set no potential hit (+++set an error flag here - TBD)
          sidewallpotentialhit = false;
        } // end if-block that checks if sidewalloutcome>0
        
      } // end if-block that checks if sidewalls are virtual or not relevant
      
      // Out of the sidewall, housing wall, or XRT object, determine which of the three is actually impacted by the photon.
      if (housingPotentiallyHit > 0) {
        if (housingImpactDistance < minImpactDistance) {
          // XRT object is not hit; check if sidewall was hit instead of housing
          // Note that we do not need the impact distance to the side wall.
          // Also remember that for obstructions sidewallpotentialhit is automatically 0
          if (sidewallpotentialhit) {
            // side wall was hit
            photonAbsorbed = 3;
          } else {
            // The only possibility remaining is that a housing wall was hit
            photonAbsorbed = 4;
          }
        }
      } else {
        // if housing could not be hit (if housingPotentiallyHit=0)
        //+++ combine these in an &&  ?
        if (sidewallpotentialhit) {
          if (sidewallImpactDistance < minImpactDistance) {
            // XRT object was not hit, instead a sidewall was hit.
            photonAbsorbed = 3;
          }
        }
      } // end if-block that checked if housingPotentiallyHit>0 or not
      
      // Record the impact coordinates if sidewall or housing was impacted
      if (photonAbsorbed == 3) {
        pathCoords[numInteractions] = impactCoords;
        pathCode[numInteractions][0] = 1;
        pathCode[numInteractions][1] = 4;
        pathCode[numInteractions][2] = 6;
        pathXRTObjectID[numInteractions] = -1; // Dummy value since sidewall is not obstruction or foil
      }
      if (photonAbsorbed == 4) {
        pathCoords[numInteractions] = housingImpact;
        pathCode[numInteractions][0] = 1;
        pathCode[numInteractions][1] = 2+housinginnerouter; //+++  make sure housinginnerouter is as expected
        pathCode[numInteractions][2] = 6;
        pathXRTObjectID[numInteractions] = -1; // Dummy value since housing is not obstruction or foil
      }
    
      // If we have reached this point, if the photon was absorbed (photonAbsorbed>0) the do-while loop will end and the photon will be terminated.
      // Otherwise, if the photon has survived it must have impacted an XRT object (since we are inside an if-block that already took care of the case that the photon did not interact with anything at all.
      
      // Following is the main if-block that will handle what happens to a photon after it impacts an XRT object, including reflection, transmission, and absorption.
      // If the photon actually impacted an XRT object...
      if (photonAbsorbed == 0) {

        // The result is that now minImpactDistanceIndex points to the element of xrtobjectType(), identifiying which xrt object was impacted. The properties of that object are then readily accessible via the other xrtobject* arrays, using the same index. Transfer some quantities for this interaction from the "potential" to "actual" arrays that will hold details of the path of the photon.
        pathXRTObjectID[numInteractions] = impactedXRTObjectID;
        pathFaceHit[numInteractions] = potentialimpactface[minImpactDistanceIndex];
        excludedID = impactedXRTObjectID;
        pathImpactDistance[numInteractions] = potentialimpactDistances[minImpactDistanceIndex]; 
        // Save the impact coordinates in the XRT frame because they are already calculated by getphotonimpactcoordinates() [which also calculates the object-frame coordinates].
        pathCoords[numInteractions] = potentialimpactcoords[minImpactDistanceIndex];
//        pathDirs[numInteractions] = potentialLocalDirs[minImpactDistanceIndex];
        pathobjectframecoords[numInteractions] = potentialobjframeimpactcoords[minImpactDistanceIndex];
        pathtransdistance[numInteractions] = potentialtransdistance[minImpactDistanceIndex];
        pathErrorCode[numInteractions] = potentialerrorCode[minImpactDistanceIndex];
        pathtermination[numInteractions] = potentialtermination[minImpactDistanceIndex];
        
        AH_DEBUG << "impact coords = " << pathCoords[numInteractions] << std::endl;
        AH_DEBUG << "pathobjectframecoords = " << pathobjectframecoords[numInteractions] << std::endl;

        // Following is the main if-block that will handle what happens to a photon after it impacts an XRT object, including reflection, transmission, and absorption.

        // First check whether the impacted XRT object is an obstruction (support structure). If so, the photon is absorbed and will terminate. Save the impact coordinates in the object frame (i.e. don't convert back to XRT frame in the interest of speed). Since we don't transform the coordinates back we do not update the current photon position (so don't use it again for this photon) 
        
        // get the object that was impacted
        const XRTObject & currObject = XRTObjects[impactedXRTObjectID];
        
        if (currObject.m_type == OBSTRUCTION) {
          
          AH_DEBUG << "impacted an obstruction" << std::endl;
          
          photonAbsorbed = 5;
          // Codify the result
          pathCode[numInteractions][0] = 1;
          pathCode[numInteractions][1] = 8;
          pathCode[numInteractions][2] = 1;
          
        } else if (currObject.m_type == FOIL) {
          // if the XRT object impacted is of type "foil" (mirror or pre-collimator), set the appropriate result codes
          
          AH_DEBUG << "impacted a mirror or precollimator" << std::endl;
          
          // Update the pointer used to track which set of foils have been 
          // impacted so far: 0=no primaries or secondaries; 
          // 1=at least 1 primary, 2=at least 1 secondary
          zGroupPointer = currObject.m_set;
          
          pathCode[numInteractions][1] = currObject.m_set + 5;
          pathCode[numInteractions][2] = pathFaceHit[numInteractions]+1;
          
          // Now check if the photon was flagged for termination in 
          // getphotonimpactcoordinates() and set the appropriate absorption 
          // codes and break out of this function.
          if (pathtermination[numInteractions]) {
            photonAbsorbed = 6; // photon ended in foil body
            pathCode[numInteractions][0] = 1;
            AH_DEBUG << "photon flagged for termination.  set photonAbsorbed = 6 and return out of raytraceonephoton" << std::endl;
            return;
          }
          
          // If pathtransdistance is non-negative, it means we need to get the transmission optical depths for the rough materials. In the case of a front-side mirror hit we will get the thin-film transmission later because we need the incident angle for that. In the case of a pre-colllimator, both front and back are rough materials. However, this if-block pertains to transmission only for the bodies of the foils, and optical depth per mm is the same regardless of which face the ray hit first. Technically the the correct optical depth is accessed using the pathFaceHit variable in xrtobjectroughtaupermmindex but due to the symmetry, this can be also be accessed simply by specifiying the index for "back side" (or 0) in every case. The array xrtobjectroughtaupermmindex points to the correct values in roughtaupermm according to the XRT object type so we don't need to explicitly know what type of object since this has aleady been set up so we only need to specify the object id.
          thickTransmissionProb = 0.0;
        
          AH_DEBUG << "pathtransdistance[numInteractions] = " << pathtransdistance[numInteractions] << std::endl;
          AH_DEBUG << "currObject.m_doTransmission = " << (currObject.m_doTransmission ? "is true" : "is false") << std::endl;
          
          
          if ( (pathtransdistance[numInteractions] > 0.0) &&
               (currObject.m_doTransmission) ) {
            thicktau = pathtransdistance[numInteractions] * 
                       roughtaupermm[currObject.m_tauPermmIndexBack];
            thickTransmissionProb = exp(-1.0 * thicktau);
            AH_DEBUG << "tentative thick transmission in raytraceOnePhoton()" << std::endl;
            AH_DEBUG << "currObject.m_tauPermmIndexBack = " << currObject.m_tauPermmIndexBack << std::endl;
            AH_DEBUG << "roughtaupermm[currObject.m_tauPermmIndexBack] = " << roughtaupermm[currObject.m_tauPermmIndexBack] << std::endl;
            AH_DEBUG << "thickTransmissionProb = " << thickTransmissionProb << std::endl;
            
          } else {
            // If pathtransdistance is negative it means that the transmission is effectively considered to be zero. For example, an entry point on the front/back and exit on a side would be such a case. We do not terminate the photon because it could still reflect.
            thickTransmissionProb = 0.0;
            //+++ remove this else and just leave the default value?
          }
          
          // Now we need to calculate a reflection probability. For the case that the ray entry was on one of the 4 edges/sides, there is no reflection, only transmission or absorption. For the reflection probability we do need to get the incident angle but we don't get the reflection direction until/if the photon is definitely going to be reflected.
          // The action now depends on which face of the foil was impacted.
          
          if (pathFaceHit[numInteractions] > 1) {
      
            // If one of the four thin (edge) faces was hit, the photon is either transmitted or absorbed so we set the reflection probability to zero. The thick material transmission probability will already have been calculated earlier.
            reflectionProb = 0.0;
            thinTransmissionProb = 1.0;
            nettransmissionprob = thickTransmissionProb;
          } else if ((pathFaceHit[numInteractions] == 0) || (pathFaceHit[numInteractions] == 1)) {
      
            // For the case that either the front or back foil surface is impacted, we have to first calculate the incident angle before we can calculate the reflection probability. then, for this XRT object, xrtobjectreflindex has pointers to the front and back reflectivity arrays (as a function of angle). Use faceHit to select the correct xrtobjectreflindex. Extract the correct array as a function of angle and interpolate on this for the derived incident angle to return a single value for the reflection probability.
            // First get the normal vector to the foil surface in the XRT object frame
            currSurfaceGeometry = currObject.m_geometry;        // {OBSTRUCT, CYLINDER, CONE}
            currobjectType = currObject.m_type;                 // {OBSTRUCTION, FOIL}
            currSlopes = currObject.m_slopes;
            currGeoParams = currObject.m_geoParams;
            normalPos = pathobjectframecoords[numInteractions];
            getSurfaceNormal(currSurfaceGeometry, currGeoParams, currobjectType, currSlopes, pathFaceHit[numInteractions], normalPos, normalDir, normalError);
            
            // +++ used below...
            currObjectSet = currObject.m_set;
            
            // +++ this could maybe be reset above somewhere?  Or just here, where we use it.
            // +++ reset localDir to the value of the object
            selectedLocalDir = potentialLocalDirs[minImpactDistanceIndex];
            
            
            AH_DEBUG << "selectedLocalDir = " << selectedLocalDir << std::endl;
            AH_DEBUG << "normalDir = " << normalDir << std::endl;
            
            // Get the dot product of the normal with the photon direction in the XRT object frame; the incident angle is the arcsin of the dot product because it is the grazing angle that we want, not the angle relative to the normal.
            singrazingIncidentAngle = ( (normalDir.m_xDir * selectedLocalDir.m_xDir) + 
                                        (normalDir.m_yDir * selectedLocalDir.m_yDir) + 
                                        (normalDir.m_zDir * selectedLocalDir.m_zDir) ) * -1.0;
            grazingIncidentAngle = std::abs(std::asin(singrazingIncidentAngle));
            
            AH_DEBUG << "singrazingIncidentAngle = " << singrazingIncidentAngle << std::endl;
              
            if (pathFaceHit[numInteractions] == 0) {
              // back surface reflection
              AH_DEBUG << "back surface reflection" << std::endl;
              
              reflindex = currObject.m_reflIndexBack;
      
              numReflGridPts = numRoughAngles;
              reflectionGrid.clear();
              reflectionGrid.reserve(numRoughAngles);
      
              for (int i = 0 ; i < numRoughAngles ; ++i) {
                reflectionGrid.push_back(roughRefl[i][reflindex]); //+++ this is correct?  reserve then pushback?
              }
              
              angleGrid = roughAngles;
              angleGridMin = roughAngles[0];
              angleGridMax = roughAngles[numRoughAngles-1];
      
              // Set a flag inidicating whether the incident angle is out of range (i.e. not covered by the reflectivity and transmission arrays). If it is out of range, mark the photon for absorption
              if ( (grazingIncidentAngle < angleGridMin) || 
                  (grazingIncidentAngle > angleGridMax) ) {
                angleOutOfRange = true;
              } else {
                angleOutOfRange = false;
                
//                // For the moment set the thin-film transmission from the back to the front to 1.0 (i.e. zero thickness). For the pre-collimator this will always be true. For the backside of a mirror foil, strictly speaking thinTransmissionProb should be set to fronttaupermm * pathLength
//                thinTransmissionProb = 1.0;
                
                if ( (pathtransdistance[numInteractions] > 0.0) && 
                     (currObject.m_doTransmission) ) {
                  double thinTau = pathtransdistance[numInteractions] * 
                                   frontTauPermm[currObject.m_tauPermmIndexBack];
                  thinTransmissionProb = exp(-1.0 * thinTau);
                } else {
                  // If pathtransdistance is negative it means that the transmission is effectively considered to be zero. For example, an entry point on the front/back and exit on a side would be such a case. We do not terminate the photon because it could still reflect.
                  thinTransmissionProb = 0.0;
                }
                
              }

            } else if (pathFaceHit[numInteractions] == 1) {
              // +++ use an enum
              
              // Front surface reflection
              AH_DEBUG << "front surface reflection" << std::endl;
              
              reflindex = currObject.m_reflIndexFront;
              
              // for mirror foils the index points to one of the groups in frontRefl; for precollimator foils the index points to the rough surface reflectivity. The integer xrtobjectset indicates whether we have a mirror foil or pre-collimator. If in the future there are other objects aside from pre-collimator or mirrors then the conditionals will have to change. Maybe always reserve currObjectSet=0 for rough front surface reflectivity.
              if (currObjectSet == 0) {
                // set=0 means pcol
                numReflGridPts = numRoughAngles;
                reflectionGrid.clear();
                reflectionGrid.reserve(numRoughAngles);
                for (int i = 0 ; i < numRoughAngles ; ++i) {
                  reflectionGrid.push_back(roughRefl[i][reflindex]);
                }
                angleGrid = roughAngles;
                angleGridMin = roughAngles[0];
                angleGridMax = roughAngles[numRoughAngles-1];
                thinTransmissionProb = 1.0;
              } else {
                // set!=0 means primary or secondary
                numReflGridPts = numIncidentAngles;
                reflectionGrid.clear();
                reflectionGrid.reserve(numIncidentAngles);
                for (int i = 0 ; i < numIncidentAngles ; ++i) {
                  reflectionGrid.push_back(frontRefl[i][reflindex]);
                }
                angleGrid = incidentAngles;
                angleGridMin = incidentAngles[0];
                angleGridMax = incidentAngles[numIncidentAngles-1];
              } // end if-block determining whether the XRT object is a mirror or pre-collimator foil

              // Set a flag inidicating whether the incident angle is out of range (i.e. not covered by the reflectivity and transmission arrays).
              if ( (grazingIncidentAngle < angleGridMin) || 
                  (grazingIncidentAngle > angleGridMax) ) {
                angleOutOfRange = true;
              } else { 
                angleOutOfRange = false;
                // Interpolate thin-film transmission probability for the calculated incident angle
                // this is only meant for thin film transmission, not precollimators (which are thick)
                if ( currObject.m_doTransmission && (currObjectSet > 0) )   {
                  transmissionGrid.clear();
                  transmissionGrid.reserve(numIncidentAngles);
                  for (int i = 0 ; i < numIncidentAngles ; ++i) {
                    transmissionGrid.push_back(frontTran[i][reflindex]);
                  }
                  // +++ I forgot why I thought this should be passed angleGrid vs incidentAngles.  If I get an error again, record it
                  // +++ 20140912 saw error again.  email from Tahir.
                  bisectionInterp(numReflGridPts, angleGrid, transmissionGrid, grazingIncidentAngle, thinTransmissionProb);
                } // end if-block determining whether to interpolate transmission or not

              } // end if-block that checks if incidentangle values are out of range

            } // end if-block determining whether front-side or back-side of a foil was impacted (faceHit=0 or 1)
      
            // Interpolate reflection probability for the calculated incident angle if the incident angle is not out of range. If it is out of range, terminate the photon and leave.
            if (!angleOutOfRange) {
              AH_DEBUG << "!angleOutOfRange, about to call bisectionInterp" << std::endl;
              bisectionInterp(numReflGridPts, angleGrid, reflectionGrid, grazingIncidentAngle, reflectionProb);
              
              // Calculate the net transmission probability due to any thin-film surface and the thick body of the XRT object. For surfaces other than the front-side mirror, there is also a 1-(reflection probability) factor.
              if ( (currObject.m_doTransmission) &&
                   (pathFaceHit[numInteractions] == 1) ) {
                nettransmissionprob = thinTransmissionProb*thickTransmissionProb;
              } else if (currObject.m_doTransmission) {
                nettransmissionprob = thinTransmissionProb*thickTransmissionProb*(1-reflectionProb);
              } else if (!currObject.m_doTransmission) {
                nettransmissionprob = 0.0;
              }
              
              AH_DEBUG << "grazingIncidentAngle = " << grazingIncidentAngle << std::endl;
              AH_DEBUG << "final transmission probabilities" << std::endl;
              AH_DEBUG << "thinTransmissionProb = " << thinTransmissionProb << std::endl;
              AH_DEBUG << "thickTransmissionProb = " << thickTransmissionProb << std::endl;
              AH_DEBUG << "nettransmissionprob = " << nettransmissionprob << std::endl;
              
            } else {
              photonAbsorbed = 8;
              pathErrorCode[numInteractions] = angleOutOfRangeCode;
              pathCode[numInteractions][0] = 1;
              pathCode[numInteractions][1] = currObjectSet+5;
              pathCode[numInteractions][2] = pathFaceHit[numInteractions]+1;
              return;
            }
            
            //  ^^ ALL THE REFLECTION, TRANSMISSION, & SCATTERING TOOK PLACE IN THIS BLOCK. ^^
            // ===============================================================================

          } // end if-block that determines whether either the back-side or front-side were impacted (as opposed to the sides)

          // Update the pathCode for which face was hit
          pathCode[numInteractions][2] = pathFaceHit[numInteractions] + 1;
          // Now at this stage we have a reflection probability and a transmission probability. For reflection we have to use the local photon direction (in the object frame) to generate the reflected ray direction and convert this back to the XRT frame.
          // Calculate absorption probability and other probability boundaries
          absorptionprob = 1.0 - reflectionProb - nettransmissionprob;
          abslobound = 0.0;
          abshibound = absorptionprob;
          refllobound = absorptionprob;
          reflhibound = absorptionprob + reflectionProb;
          transmissionlobound = reflhibound;
          transmissionhibound = 1.0;
          
          chooseoutcome = getRandom();   // random number between 0 and 1
          if ( (chooseoutcome <= abshibound) && (absorptionprob > 0.0) ) { 
            // We have absorption
            AH_DEBUG << "We have absorption" << std::endl;
            
            photonAbsorbed = 999; // +++ make this a dummy value?
            pathCode[numInteractions][0] = 1;
            
          } else if ( ( (chooseoutcome > transmissionlobound) && (nettransmissionprob > 0.0) ) || 
                      (nettransmissionprob == 1.0) ) {
            // We have transmission
            AH_DEBUG << "We have transmission" << std::endl;
            
            // photon direction remains the same, we have already recorded the impact coordinates, just update the current position. (Don't put the photon at the exit point because that's not what happens physically and that would involve more transformations). Since we definitely have transmission in this part of the if-block, the foil is now effectively transparent.
            currPhotonPos = pathCoords[numInteractions];
            
            pathCode[numInteractions][0] = 4;
            
          } else if ( ( (chooseoutcome > refllobound) && (chooseoutcome <= reflhibound) && (reflectionProb > 0.0) ) || 
                       (reflectionProb == 1.0) ) {
            // We have reflection
            AH_DEBUG << "We have reflection" << std::endl;
            
            // The procedure is more complicated: first get the direction of the reflected photon in the XRT object frame, modify the direction of the reflected photon using the scattering distribution, convert photon direction back to XRT frame (the impact coordinates in the XRT frame were already recored earlier).
             
            // Get the direction of the reflected photon; the grazing incident angle was calculated earlier in order to calculate the reflection probability so the routine should not calculate it again.
            // The reflected vector will include any modification to the specular direction due to scattering. The probability for scattering and the scattering angle probability distribution are read from tables in the scattering file.
            
            // set output variables to 0 before entering
            scatteredangle = 0.0;
            sinReflected = 0.0;
            reflErrorCode = 0;
            
            // If reflection occurred (whether or not the reflected vector was affected by scattering away from the specular direction), calculate the new direction of the photon using getreflectiondirection().
            
            if (currObject.m_doScattering) {
              // do scattering 
              AH_DEBUG << "We have scattering." << std::endl;
              
              // If scattering is treated, calculate the modification to the specular direction and calculate the "effective" reflected angle, and the sine of that angle, which will then be an input to the routine getreflectiondirection().
            
              isSpecular = false;
              pathCode[numInteractions][0] = 3;  // specular reflection+scattering
              
              scatsegmentid = currObject.m_segmentID;
              if (pathFaceHit[numInteractions] == 0) {
                scatcolumnindex = currObject.m_scatterIndexBack;
              } else {
                scatcolumnindex = currObject.m_scatterIndexFront;
              }
              
              // indicate to getscattereddirection whether to use the angle grids for front-side scattering or for rough scattering
              // +++ use an enum
              if ( (pathFaceHit[numInteractions] == 0) || 
                   ( (currObject.m_type == FOIL) && 
                     (currObject.m_set == 0) ) ) {
                isRoughScattering = true;
              } else {
                isRoughScattering = false;
              }
              
              getScatteredDirection(grazingIncidentAngle, scatsegmentid, scatcolumnindex, scatenergyindex, scat, isRoughScattering, scatteredangle);
              sinReflected = std::sin(grazingIncidentAngle + scatteredangle);
              
              // +++ 20141219 for now, add this cludgy catch in case 
              //     sinReflected is zero.  currently, a zero value will fail 
              //     inside getReflectionDirection() because it divides by 
              //     sinReflected.  And we don't have time right before this 
              //     delivery to fully fix and test.
//              sinReflected = std::max(sinReflected, 1.0e-10);
              
//              if (sinReflected == 0.0) {
//                AH_DEBUG << "sinReflected == 0.0" << std::endl;
//              }
              
              // +++ 20141219 also have a version where we just get out of the 
              //     while loop.  We're not sure if this will break something 
              //     else, though.
              if ( sinReflected == 0.0 ) {
//              if ( grazingIncidentAngle == (-1.0 * scatteredangle) ) {
                photonAbsorbed = 999; // +++ make this a dummy value?
                pathCode[numInteractions][A]  = 9;
                pathCode[numInteractions][BC] = 99;
                pathCode[numInteractions][D]  = 9;
                AH_DEBUG << "photon flagged for termination because sinReflected=0." << std::endl;
                break;  // break out of this loop, so we don't call getReflectionDirection()
              }
              
              AH_DEBUG << "isRoughScattering = " << (isRoughScattering ? "it's true" : "it's false") << std::endl;
              AH_DEBUG << "grazingIncidentAngle = " << grazingIncidentAngle << std::endl;
              AH_DEBUG << "scatteredangle = " << scatteredangle << std::endl;
              AH_DEBUG << "sinReflected = " << sinReflected << std::endl;
              
            } else {
              // don't do scattering
              AH_DEBUG << "We don't have scattering." << std::endl;
              
              // Set the flag for the routine to do specular reflection if appropriate, because it is much quicker than applying the full set of equations with a scattering angle of zero
              isSpecular = true;
              pathCode[numInteractions][0] = 2;    // specular reflection only
              
            }   // end-if doscattering
            
            AH_DEBUG << "selectedLocalDir = " << selectedLocalDir << std::endl;
            getReflectionDirection(selectedLocalDir, normalDir, isSpecular, singrazingIncidentAngle, sinReflected, newLocalDir, reflErrorCode);
            AH_DEBUG << "newLocalDir = " << newLocalDir << std::endl;

            // Change current photon direction back to the XRT frame. For the moment we do not have the transformation routines in place so only doTransforms=False works. Note that the impact and photon position coordinates already have XRT frame values since they had to be calculated along the way.
            if (transforms.m_doTransforms) {
              transformType = DIRECTION;
              // +++ declare these up top?
              CartesianCoord dummyCoordOrig;
              CartesianCoord dummyCoordNew;
              applyXRTTransform(transformType, dummyCoordOrig, newLocalDir, transforms.m_xrt2objectframe[pathXRTObjectID[numInteractions]], dummyCoordNew, currPhotonDir, pathXRTObjectID[numInteractions]);
              
              AH_DEBUG << "right after calling applyXRTTransform() third time" << std::endl;
              AH_DEBUG << " converting photon direction back to xrt frame " << std::endl;
              AH_DEBUG << " newLocalDir = " << newLocalDir << std::endl;
              AH_DEBUG << " currPhotonDir      = " << currPhotonDir << std::endl;
              
            } else {
              currPhotonDir = newLocalDir;
            }

            // Update the current photon position
            currPhotonPos = pathCoords[numInteractions];
                  
          } // end if-block that selects between absorption, reflection, and transmission (the last part of the if-block treated reflection)

        // end if-block portion that treats the case that the impacted XRT 
        // object is of type "foil" (mirror or pre-collimator)
        } else {
          
          // At present there are no other XRT object types aside from "obstruction" or "foil" but there may be in the future.
          AH_THROW_RUNTIME("No other XRT object types aside from OBSTRUCTION and FOIL are currently supported");
          // +++ Although the code should never get here, put a photon error code (TBD) in case of future modifications of other parts of the code and/or mirror definition file.

        } // end-if determining what type of XRT object was impacted in this interaction

      } // end-if photon was not absorbed (photonAbsorbed == 0)
      // there's no else for this.  if the photon is absorbed, no further action needs to be taken
    
      //+++ should this comment be before or after the next endif?
      // We are still inside the if-block and do-while loop following a particular interaction of the photon with a telescope component. The interaction has now played out. Whatever the outcome was, the impact coordinates are in potentialimpactcoords(minImpactDistanceIndex,0:2) and pathCoords(numInteractions,0:2), and other attributes of this interaction are in accompanying arrays.
    
      // +++ 20140918 the following are for testing
      pathIncidentAngle[numInteractions] = grazingIncidentAngle;
      pathScatteringAngle[numInteractions] = scatteredangle;
    
      // Finally, update the counter for the number of interactions between 
      // photon and XRT components
      numInteractions++;
      
      AH_DEBUG << "numInteractions so far (end of main while loop, after increment) = " << numInteractions << std::endl;
  
    } // end if-block that checks number of actual impacts (nimpacts>0)
  
    
    AH_DEBUG << "at end of current while-loop iteration: " << std::endl;
    AH_DEBUG << "  currPhotonPos = " << currPhotonPos << std::endl;
    AH_DEBUG << "  currPhotonDir = " << currPhotonDir << std::endl;
    AH_DEBUG << "  photonAbsorbed = " << photonAbsorbed << std::endl;
    AH_DEBUG << "  zLevelCtr = " << zLevelCtr << std::endl;
    
  } // end main while-loop, while photon is not absorbed

  AH_DEBUG << "after main while-loop" << std::endl;
  AH_DEBUG << "numInteractions = " << numInteractions << std::endl;
  
  
  
  // +++ vvv same as raytracePhotonFast vvv
  
  // If this point has been reached with photonAbsorbed=0, it means that the 
  // photon has survived passing through the telescope (i.e. to the bottoms of 
  // the secondary mirrors), and now we need to check if any external objects 
  // (e.g., thermal shield, bottom) are intercepted on the way to the focal 
  // plane. The photon may or may not survive.
  
  if ( (photonAbsorbed == 0) && (bottomExtObjects.m_numExtObjectParts > 0) ) {
    
    AH_DEBUG << "search through bottom external objects" << std::endl;
    
    // unused output for getXYForNewZ(), projecting phtoon to bottom ext objs
    double timetobottom = 0.0;
    // projected coordinates of photon as it moves through bottom objects
    CartesianCoord bottomExtObjCoord(0.0, 0.0, bottomExtObjects.m_zCoord);
    double extobjectbottomrad = 0.0;
    double extobjectbottomphi = 0.0;
    
    // Project the photon to the z-coordinate of the plane containing the 
    // external objects below the telescope and get new x, y coordinates there
    getXYForNewZ(currPhotonPos, currPhotonDir, bottomExtObjCoord, timetobottom);
    cartesianToPolar(bottomExtObjCoord, extobjectbottomrad, extobjectbottomphi);
    
    // Loop over each of the external objects; we can stop if/as soon as there 
    // is an impact because it is assumed that external objects do not overlap
    for (int iExtObj = 0 ; iExtObj < bottomExtObjects.m_numExtObjectParts ; ++iExtObj) {
      
      if ( (extobjectbottomrad >= bottomExtObjects.m_rMin[iExtObj]) &&
           (extobjectbottomrad <= bottomExtObjects.m_rMax[iExtObj]) && 
           (extobjectbottomphi >= bottomExtObjects.m_startAngleRad[iExtObj]) && 
           (extobjectbottomphi <= bottomExtObjects.m_endAngleRad[iExtObj]) ) {
        
        // photon-object intercept; now check if photon is transmitted
        if (bottomTransProb[iExtObj] == 0.0) {
          // The object is to be treated as an obstruction and the photon is 
          // terminated (bottomtransprob(i) was set 0.0 earlier if the 
          // mass-absorption index was negative)
          photonAbsorbed = 10;
        } else {
          // Generate a random number to see if the photon will be transmitted
          double bottomxrtobjectrandomtrans = getRandom();
          if (bottomxrtobjectrandomtrans > bottomTransProb[iExtObj]) {
            // Photon is absorbed if the random number is > trans. prob.
            photonAbsorbed = 10;
          }
        }
        
        // Whatever the outcome, save the photon impact position. We do not 
        // update the current photon position because the main telescope 
        // components may be misaligned: a more accurate position for impact on 
        // the focal plane (if there is one) will be obtained because if the 
        // photon is transmitted, it is as if the external object is not 
        // present once the outcome of successful transmission is determined. 
        // Note that impact with the external object does not change the photon 
        // direction so we do not need to update that.
        pathCoords[numInteractions] = bottomExtObjCoord;
        pathDirs[numInteractions] = currPhotonDir;
        
        // Set the pathcode depending on the outcome; if the photon was 
        // absorbed return, if not continue after incrementing counter for 
        // number of interactions ready for the next interaction.
        if (photonAbsorbed > 0) {
          pathCode[numInteractions][A]  = 1;
          pathCode[numInteractions][BC] = 10;
          pathCode[numInteractions][D]  = 1;
          return;
        } else {
          pathCode[numInteractions][A]  = 4;
          pathCode[numInteractions][BC] = 10;
          pathCode[numInteractions][D]  = 1;
          numInteractions++;
        }
        
        // photon has been intercepted. break out of this loop through 
        // bottom external objects
        break;
        
      } // end-if checking for photon-object intercept
      
    } // end-while loop over bottom object parts, checking for an intercept
    
  } // end-if any external objects below the main telescope
  // +++ ^^^ same as raytracePhotonFast ^^^
  
  // +++ vvv same as raytracePhotonFast vvv
  
  // +++ put this inside the next if?  to be clearer.
  // if we made it to the results plane, set the z of the photon
  resultPlaneCoord.m_z = resultsplanez;
  
  // if the photon was not absorbed in the XRT structure, find the coordinates 
  // of the impact on the results plane.
  if (photonAbsorbed == 0) {
      
    resultsPlaneWasImpacted = true;
    
    CartesianCoord coordIn = pathCoords[numInteractions-1];
    getXYForNewZ(coordIn, currPhotonDir, resultPlaneCoord, tresultsplane);
    // Save the coordinates in the pathCoords array
    pathCoords[numInteractions] = resultPlaneCoord;
      
    // Set the result code for this final interaction
    pathCode[numInteractions][0] = 1;
    pathCode[numInteractions][1] = 1;
    pathCode[numInteractions][2] = 1;
      
    // Copy the final photon attributes into convenience arrays (we don’t know 
    // in advance how many interactions a given photon will have)
    finalPhotonPos = resultPlaneCoord;
    finalPhotonDir = currPhotonDir;
    // The object ID is set to -1 for a results/focal plane hit, otherwise it 
    // would be 0, being confused with an XRT object with that ID
    pathXRTObjectID[numInteractions] = -1;
      
  } else {
    
    AH_DEBUG << "no results plane impact.  decrement numInteractions" << std::endl;
    resultsPlaneWasImpacted = false;
    // The numInteractions variable has to be decremented if the photon did not 
    // make it to the focal plane because that counter is always 1 ahead of the 
    // actual number of interactions that have actually taken place.
    numInteractions--;
  }
  // +++ ^^^ same as raytracePhotonFast ^^^
  
  AH_DEBUG << "end of raytraceOnePhoton." << std::endl;
  AH_DEBUG << " numInteractions = " << numInteractions << std::endl;
  
} // end raytraceOnePhoton()


/******************************************************************************/


/// \brief 
/// \param[in] 
/// \param[out] 
/// 
/// Follows a photon through an X-ray telescope until it is absorbed by an 
/// XRT object or until it is otherwise terminated. This routine is a fast 
/// version of raytraceOnePhoton() that is designed to quickly calculate the 
/// effective area at the expense of not fully calculating paths that do not 
/// contribute to the effective area (or the contribution is negligible).
void raytracePhotonFast(bool has3DObjects,  
                        bool pcolExists,
                        const CartesianCoord & initialPhotonCoord,
                        const DirectionVector & initialPhotonDir, 
                        double initialPhotonRadius, 
                        double initialPhotonPhi,
                        int numIncidentAngles,
                        const vector1Ddbl & incidentAngles, 
                        const vector2Ddbl & frontRefl, 
                        const vector2Ddbl & frontTran, 
                        const vector1Ddbl & frontTauPermm,
                        int numRoughAngles, 
                        const vector1Ddbl & roughAngles, 
                        const vector2Ddbl & roughRefl,
                        const vector1Ddbl & roughtaupermm, 
                        const ExternalObjectsStructure & topExtObjects, 
                        const ExternalObjectsStructure & bottomExtObjects, 
                        const vector1Ddbl & topTransProb,   // slice of reflTranGrid.m_topExtObjectsTransProb, for a single energy
                        const vector1Ddbl & bottomTransProb,   // slice of reflTranGrid.m_bottomExtObjectsTransProb, for a single energy
                        const GenTelescope & genTelescope,
                        const std::vector<HousingGeometry> & housings, 
                        const vector1Ddbl & xrtzlevels,
                        double resultsplanez, 
                        int maxNumPathCoords, 
                        const std::vector<XRTObject> & XRTObjects,
                        int scatenergyindex,
                        const Scattering & scat, 
                        const SectorsShells & sectorsShells, 
                        const std::vector<int> & obstructionIntervalLowIndex, 
                        const std::vector<int> & obstructionIntervalHighIndex, 
                        const Transforms & transforms, 
         
                        int & numInteractions, 
                        std::vector<CartesianCoord> & pathCoords, 
                        std::vector<DirectionVector> & pathDirs,
                        vector2Dint & pathCode, 
                        std::vector<int> & pathXRTObjectID,
                        CartesianCoord & finalPhotonPos,
                        DirectionVector & finalPhotonDir, 
                        bool & resultsPlaneWasImpacted, 
                        std::vector<int> & pathErrorCode) {
  
  // +++ think of a better way to have this and raytraceOnePhoton, rather than two functions that are VERY similar?
  
    AH_DEBUG << std::endl;
    
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // reassign variables for the current photon position and direction to 
  // initial values
  CartesianCoord currPhotonPos = initialPhotonCoord;
  DirectionVector currPhotonDir = initialPhotonDir;
  
  
  // flag for photon absorption; if/when this flag is >0, an absorption event 
  // has occurred and the photon path will be terminated.
  // note that this is only used internally by the routine; the more concise 
  // information that is output by the routine appears in the pathCode() array
  int photonAbsorbed = 0;
  //  0   means not absorbed
  //  1   means absorbed on results plane
  //  2   means +++ missing from trf
  //  3   means absorbed on a side wall
  //  4   means absorbed on a housing wall
  //  5   means absorbed on a support structure
  //  6   means absorbed in a foil body coming in from an "edge"-face, 
  //            terminating in the body
  //  7   means absorbed in a foil after entering the front or back face surface
  //  8   means Effectively terminated due to photon incident angle falling 
  //            outside the reflectivity/transmission angle grid.
  //  9   means absorbed by an external object above the telescope
  //  10  means absorbed by an external object below the telescope
  //  11  means photon misses telescope
  //  999 means absorbed in a mirror/pre-collimator foil after "throwing the 
  //            dice" for reflection and transmission.
  
  
  // Flag that will indicate termination of the photon path
  bool photonTerminated = false;
  
  // Path length default (negative if not calculated or impact is not possible)
  double pathLength = -1.0;
  
  
  // XRTObject id of the previous interaction so that the same object does not 
  // appear in the candidate list for the next interaction. The initial value 
  // is set equal to a value that does not correspond to any xrt object id
  int lastImpactedObjectID = -1;
  
  int lowersector = 0;
  int uppersector = 0;
  
  // Default value of faceHit (no hit); 
  // will be over-written when there is a ray-object interaction
  int faceHit = -1;
  
  // inputs for applying transforms
  // default to transform both position and direction
  transformType_e transformType = BOTH;

  
  // outputs from getXYForNewZ()
  CartesianCoord resultPlaneCoord;
  double tresultsplane = 0.0;
  
    AH_DEBUG << std::endl;
    
  // inputs and outputs to getreflectiondirection()
  // input:
  bool isSpecular = false;        // flag to do specular reflection
  DirectionVector localDir;       // input to getreflectiondirection(), etc
  DirectionVector normalDir;      // input to getreflectiondirection(), output to getSurfaceNormal(), etc
  double sinReflected = 0.0;      // input to getreflectiondirection()
  // output:
  DirectionVector newLocalDir;    // output to getreflectiondirection()
  int reflErrorCode = 0;          // output from getreflectiondirection().  +++ not used?
  
  
  //+++ what do we do with pathobjectframecoords
  // The Cartesian coordinates at the interaction (XRT object frame)
  std::vector<CartesianCoord> pathobjectframecoords(maxNumPathCoords);
  
  // The straight-line path length (in mm) for the photon to exit the XRT 
  // object after entering at the interaction point (used to calculate optical 
  // depth for transmission)
  std::vector<double> pathtransdistance(maxNumPathCoords);
  
  // The distance (in mm) between the current position and the next interaction 
  // position
  std::vector<double> pathImpactDistance(maxNumPathCoords);
  
  // Which face of the XRT obects was impacted 
  std::vector<long> pathFaceHit(maxNumPathCoords);
  //    0 = back
  //    1 = front
  //    2 = top edge
  //    3 = bottom edge
  //    4 = sides
  //    5 = unclassified
  
    AH_DEBUG << std::endl;
    
  // +++ just in raytracefast
  std::vector<double> pathreflprob(maxNumPathCoords);
  std::vector<double> paththintransprob(maxNumPathCoords);
  std::vector<double> paththicktransprob(maxNumPathCoords);
      
  long reflindex = 0;
  std::vector<double> transmissionGrid; // input to bisectionInterp()
  std::vector<double> angleGrid;
  double angleGridMin = 0.0;
  double angleGridMax = 0.0;
  double thinTransmissionProb = 0.0;
  double absorptionprob = 0.0;
  double reflectionProb = 0.0;
  double nettransmissionprob = 0.0;
  double abslobound = 0.0;
  double abshibound = 0.0;
  double refllobound = 0.0;
  double reflhibound = 0.0;
  double transmissionlobound = 0.0;
  double transmissionhibound = 0.0;
  double grazingIncidentAngle = 0.0;
  double singrazingIncidentAngle = 0.0;
  
  // inputs to getScatteredDirection()
  bool isRoughScattering = false;
  int scatsegmentid = 0;
  int scatcolumnindex = 0;
  // output to getScatteredDirection()
  double scatteredangle = 0.0;
  
    AH_DEBUG << std::endl;
    
  double thickTransmissionProb = 0.0;
  double thicktau = 0.0;
  
  
  const int angleOutOfRangeCode = 999;
  
  bool angleOutOfRange = false;
  
  // inputs to getSurfaceNormal:
  surfaceGeometryTypes_e currSurfaceGeometry;
  objectTypes_e currobjectType;
  Slopes currSlopes;
  std::vector<double> currGeoParams;
  // outputs to getSurfaceNormal:
  CartesianCoord normalPos;
  int normalError = 0;
  // other variables:
  int currObjectSet = 0;
  std::vector<double> reflectionGrid;
  long numReflGridPts = 0;
  
  int gapIncrement = 0;
  
  // will be a random number, if foil surface front or back is impacted
  double chooseoutcome = 0.0;         
  
  
  
  AH_DEBUG << std::endl;
    
  // ----------------------------
  // initialize output variables:
  // ----------------------------
  
  resultsPlaneWasImpacted = false;
  
  // +++ maybe fill with 0s instead of clear and resize?
  
  // resize to hold the full path coords and direction vectors
  pathCoords.clear();
//  std::vector<CartesianCoord>(pathCoords).swap(pathCoords); +++ I don't need this, really, since I resize to same
  pathCoords.resize(maxNumPathCoords);
  
  pathDirs.clear();
  pathDirs.resize(maxNumPathCoords);
  
//  const int numPathCodeInts = 3;    // A, BC, D
  
  AH_DEBUG << std::endl;
  
  pathCode.clear();
  pathCode.resize(maxNumPathCoords);
  for (int iInteraction = 0 ; iInteraction < maxNumPathCoords ; ++iInteraction) {
    pathCode[iInteraction].resize(s_numPathCodeInts);
  }
  
  AH_DEBUG << std::endl;
  
  // Set the default code for prematurely-terminated photons in fastmode to 9999
  // Photon paths that are not terminated prematurely will over-write this 
  // value with the correct path code
  for (int iPath = 1 ; iPath < maxNumPathCoords ; ++iPath) {
    // +++ create enums for 9999
    // +++ I don't like maxNumPathCoords
    pathCode[iPath][A] = 9;
    pathCode[iPath][BC] = 99;
    pathCode[iPath][D] = 9;
  }
  
  AH_DEBUG << std::endl;
  
  // +++ this is not my preferred way to do this.  I don't like using maxNumPathCoords
  pathXRTObjectID.clear();
  pathXRTObjectID.resize(maxNumPathCoords);
  
  pathErrorCode.clear();
  pathErrorCode.resize(maxNumPathCoords);
  
  // Since the 0th elements of the path arrays correspond to the initial photon 
  // attributes, set a dummy value for the object id.
  pathXRTObjectID[0] = -1;
  
  // set the first entries for the path and photon direction history to the 
  // initial (input) values. This will be zeroth interaction.
  pathCoords[0] = initialPhotonCoord;
  pathDirs[0] = initialPhotonDir;
  
  // Set the number of interactions counter to 1 because the zeroth interaction 
  // is the initial telescope entry event. This variable will always point to 
  // quantities relating to the current ray- object interaction
  numInteractions = 1;
  
  // finished initializing variables
  // -------------------------------------
    
  AH_DEBUG << std::endl;
  
  // +++++++ new stuff   v
  
  // +++ if I make a function checkPhotonInTop()
  // inputs
    // const ExternalObjectsStructure & topExtObjects;
    // const ExternalObjectsStructure & bottomExtObjects;
    // const vector1Ddbl & topTransProb;   // slice of reflTranGrid.m_topExtObjectsTransProb, for a single energy
    // const vector1Ddbl & bottomTransProb;   // slice of reflTranGrid.m_bottomExtObjectsTransProb, for a single energy
    // initialPhotonRadius
    // initialPhotonPhi
  // outputs 
    // photonAbsorbed
    // pathcoordinates
    // pathphotondir
    // pathCode
    // numInteractions

  
  // Before entering the actual telescope, first check if the photon survives 
  // passage through external objects above the telescope, if there are any:
  if (topExtObjects.m_doExternalObjects) {

    // Loop over each object part to check if the object intercepts the part: 
    // we only need one match - it is assumed that no object part overlaps with 
    // any other so there is no ambiguity about the transmission probability.
    bool topXRTObjectIntercepted = false;
    int iObject = 0; // Counter for the object parts being looped over 
    
    // +++ use a for loop instead?
    while (!topXRTObjectIntercepted) {
      
      if ( (initialPhotonRadius >= topExtObjects.m_rMin[iObject]) && 
           (initialPhotonRadius <= topExtObjects.m_rMax[iObject]) && 
           (initialPhotonPhi >= topExtObjects.m_startAngleRad[iObject]) && 
           (initialPhotonPhi <= topExtObjects.m_endAngleRad[iObject]) ) {
        
        if (topTransProb[iObject] == 0.0) {
          // The object is to be treated as an obstruction and the photon is terminated 
          // (topTransProb(i) was set 0.0 earlier if the mass-absorption index was negative)
          photonAbsorbed = 9; 
        } else {
          // Generate a random number to see if the photon will be transmitted
          double topXRTObjectRandomTrans = getRandom();
          if (topXRTObjectRandomTrans > topTransProb[iObject]) { 
            // Photon is absorbed if the random number is > trans. prob.
            photonAbsorbed = 9;
          }
        }

        // Whatever the outcome, save the photon coordinates and direction (which should actually just be the same as the initial values since the top xrt objects are at the telescope aperture)
        pathCoords[numInteractions] = currPhotonPos;
        pathDirs[numInteractions] = currPhotonDir;

        // Note that even if the photon is transmitted, we leave the photon in the current position because the main telescope components may be misaligned: if the photon misses the telescope because of the finite distance between the external object and the telescope entrance, the correct outcome will be automatically determined by the algorithm that determines impacts between the photon and telescope components because there is no requirement that the initial photon position has to be at the telescope entrance. If the photon is transmitted, it is as if the external object is not present once the outcome of successful transmission is determined.

        // Set the pathCode depending on the outcome; 
        // if the photon was absorbed return, 
        // if not continue after incrementing counter for number of interactions 
        //   ready for the next interaction.
        if (photonAbsorbed > 0) {
          pathCode[numInteractions][A]  = 1;
          pathCode[numInteractions][BC] = 9;
          pathCode[numInteractions][D]  = 1;
          return;
        } else {
          topXRTObjectIntercepted = true;
          pathCode[numInteractions][A]  = 4;
          pathCode[numInteractions][BC] = 9;
          pathCode[numInteractions][D]  = 1;
          numInteractions++;
        }
        
        topXRTObjectIntercepted = true;
        
      } // end-if photon position is inside external object i (i.e. if an intercept occurred)
      
      //  Move to next external object if there was no intercept (i.e. if the do-while loop did not end)
      iObject++;

    } // end-while checking for photon-object intercept, over top xrt objects
    
  } // end-if objects above the telescope
    
  AH_DEBUG << std::endl;
  
  
  // +++ if I make a function checkPhotonInTop()
  // we checked the passage through any external objects above the telescope.  
  // If the photon was absorped there, we already set the appropriate pathCode,
  // now return out of the raytrace function.
//  if (absorbedInTopObject) {
//    return;
//  }
  
  // If the photon makes it to this point we have to first check if the photon falls through the central hole instead of the main telescope containing the mirror foils and support structures.
  double tothousingshifts = housings[PRIMARY].m_xShift + 
                            housings[PRIMARY].m_yShift + 
                            housings[SECONDARY].m_xShift + 
                            housings[SECONDARY].m_yShift;
  int hstart = 0;
    
  // this will later hold the top coordinates as we move photon through
  // the housings and the central hole
  CartesianCoord topCoord;  // +++ trf: xtop, ytop, ztop

  // projected coord
  CartesianCoord projCoord; // +++ trf: xproj,yproj,zproj
  // for output for getXYForNewZ for coord projCoord
  double tproj = 0.0; 
  // after calling getXYForNewZ for coord projCoord, call cartesianToPolar
  // for radius and phi of the projected coord
  double rproj = 0.0;
  double phiproj = 0.0;

  // whether or not the photon escapes the hole
  bool photonEntersTelescope = false;
  
  AH_DEBUG << std::endl;
  
  if (initialPhotonRadius < genTelescope.m_innerhousingradius) {
    // Photon goes through hole at aperture but we need to check:
    // If the AZIMUTHALSTRUCT extension in the TDF supplied information about 
    // the finite width of the inner housing structure 
    // (i.e. numcentralextobjectparts>0);
    // if so, and the height of the inner housing does not reach the top of the 
    // pre-collimator, whether the photon leaves the hole and enters the main 
    // telescope or whether the hits an inner housing wall on the way down to 
    // the results plane.
    // This must be done in steps because of possible misalignments between 
    // housing units. Project photon to top of "thick" housing wall, or bottom 
    // of pre-collimator housing if no "thick" housing information is supplied; 
    // then to the bottom of the primary housing, and then the secondary 
    // housing. Compare the radial coordinate at the impact point to the radial 
    // coordinates of the relevant telescope parts to determine the outcome. If 
    // a housing wall is impacted, terminate the photon.
    
      
    AH_DEBUG << std::endl;
  
    if ( (tothousingshifts == 0.0) && 
          (genTelescope.m_numcentralextobjectparts == 0) ) {
      // go straight to results plane if there are no housing shifts and there 
      // is no housing structure inside innerhousingradius
      hstart = 3; 
    } else {
      if (pcolExists) {
        hstart = 0;
      } else {
        hstart = 1;
      }
    } // end-if extra structure in the hole and if there are no misalignments
    
    AH_DEBUG << std::endl;
    
    int iHousing = hstart; // +++ this is the housing number PCOL, PRIMARY, SECONDARY
    
    // for the first housing unit there are no x/y shifts by definition so don’t need to correct xtop,ytop,ztop
    topCoord = currPhotonPos;
    
    // Note that we do not update the photon position while we are testing for 
    // housing wall impacts: if there is an impact the photon is terminated 
    // anyway, and if there is no impact, the current photon position and 
    // direction can still be used to determine the impact coordinates of the 
    // next interaction, whatever that may be. This is true even when the 
    // photon escapes through a gap in the inner housing wall through to the 
    // main telescope.
    
    AH_DEBUG << std::endl;
    
    while ( (photonAbsorbed == 0) && (iHousing < 4) ) {
      
      AH_DEBUG << std::endl;
    
      if (iHousing > hstart) {
        // correct photon position for housing shifts 
        topCoord.m_x = currPhotonPos.m_x + housings[iHousing].m_xShift;
        topCoord.m_y = currPhotonPos.m_z + housings[iHousing].m_yShift;
      }
      
      // If we are doing the pre-collimator, and if there is a "thick" wall inside the hole, we have to first project to the top of this structure to see if it can pass through into the main telescope area; if not it could hit the top of this structure, or pass into the hole.
    
      if (iHousing == 0) {
        if (genTelescope.m_numcentralextobjectparts > 0) {
          
          projCoord.m_z = genTelescope.m_holewalltopz;
          
          getXYForNewZ(topCoord, currPhotonDir, projCoord, tproj);
          cartesianToPolar(projCoord, rproj, phiproj);
          
          if ( (rproj >= genTelescope.m_holesideinnerradius) && 
               (rproj <= genTelescope.m_innerhousingradius) ) { 
            photonAbsorbed = 4; // photon hits top face of short housing wall
          } else if (rproj > genTelescope.m_innerhousingradius) {
            // photon escapes over out of the hole into the main telescope
            photonEntersTelescope = true;
          }
        
        } // end-if inner housing wall has structure
      } // end-if i=0

      // If the photon did not escape through the gap at the top of the pre-col inner housing, project it down to the bottom of the pre-collimator level, the primary, the secondary (i=0..2) or then finally to the focal plane (i=3).
      if (!photonEntersTelescope) {
        
        if ( (iHousing < 3) && (!photonEntersTelescope) ) {
          // housing.zLower is from PCOLZMIN, PRIMZMIN, or SECDZMIN
          projCoord.m_z = housings[iHousing].m_zLower;
        } else if (iHousing == 3) {
          projCoord.m_z = resultsplanez;
        }
        
        getXYForNewZ(topCoord, currPhotonDir, projCoord, tproj);
        cartesianToPolar(projCoord, rproj, phiproj);
        if (rproj >= genTelescope.m_holesideinnerradius) {
          if (iHousing < 3) {
            // photon is absorbed by inner housing wall
            photonAbsorbed = 4;
          } else if (iHousing == 3) {
            photonAbsorbed = 1;
          }
        }
        iHousing++;
        
      } // end-if photon did not escape through gap at the top of the pre-collimator housing unit
      
    } // end while
    
  } else if (initialPhotonRadius == genTelescope.m_innerhousingradius) {
    
    photonAbsorbed = 4;
    
  } // end-if initial photon position is inside hole or on the top edge

  if (photonAbsorbed == 4) { 
    pathCode[numInteractions][A]  = 1;
    pathCode[numInteractions][BC] = 2;
    pathCode[numInteractions][D]  = 2;
    return;
  } else if (photonAbsorbed == 1) { 
    pathCode[numInteractions][A]  = 1;
    pathCode[numInteractions][BC] = 1;
    pathCode[numInteractions][D]  = 1;
    pathCoords[numInteractions].m_x = projCoord.m_x;
    pathCoords[numInteractions].m_y = projCoord.m_y;
    pathCoords[numInteractions].m_z = resultsplanez;
    finalPhotonPos = pathCoords[numInteractions];
    finalPhotonDir = currPhotonDir;
    resultsPlaneWasImpacted = true;
    return;
  }
  
  // If we reach this point, the photon has passed through external objects but either did not enter the hole, or entered but "escaped" into the main telescope through a gap in the inner housing wall at the pre-collimator level. Since the external objects are in general some height above the main telescope components, it is possible that the photon will miss the telescope at the outer perimeter. To test this we project the photon to the z-coordinate of the top of the main telescope and see if its radial coordinate is larger than the telescope outer housing radius. We don’t need to do this for photons that entered the hole and escaped into the main telescope.
  
  if (initialPhotonRadius > genTelescope.m_innerhousingradius) {
    topCoord.m_x = currPhotonPos.m_x;
    topCoord.m_y = currPhotonPos.m_y;
    projCoord.m_z = xrtzlevels[0]; // z-coordinate of top of main telescope
    getXYForNewZ(topCoord, currPhotonDir, projCoord, tproj);
    cartesianToPolar(projCoord, rproj, phiproj);
    if (rproj >= genTelescope.m_outerhousingradius) {
      // photon misses the outer housing wall
      photonAbsorbed = 11;
      pathCode[numInteractions][A]  = 9;
      pathCode[numInteractions][BC] = 98;
      pathCode[numInteractions][D]  = 9;
      return;
    }
  } // end-if photon did not come through central hole
  
  
  // +++ p237 of trf
  
  // Now we test for impact with the first group of obstructions (support structures). This group consists of the obstructions immediately above the primary mirror tops, and those immediately above the pre-collimator if there is one. Note that we will go back to test for impact with the pre-collimator foil faces after checking for impact on support structures below the pre-collimator. This is a fastmode approximation and should not compromise accuracy because:
  // (1) photons that impact a pre-collimator foil top edge would be terminated anyway, and 
  // (2) photons that impact a pre-collimator face first would also be terminated in fastmode
  
  // index 0 ABOVE = p-col & primary top group of obstructions 
  int obstructionStart = obstructionIntervalLowIndex[ABOVE];
  int obstructionEnd = obstructionIntervalHighIndex[ABOVE];
  int currObstructionIdx = 0;
  int numVertices = 0;
  bool pointIsInsidePolygon = false; 
  CartesianCoord localPos;
  double tobsimpact = 0.0;
  CartesianCoord newPolyCoord;
  
  
  currObstructionIdx = obstructionStart;
  
  while ( (photonAbsorbed == 0) && (currObstructionIdx <= obstructionEnd) ) {
    
    const XRTObject & currObject = XRTObjects[currObstructionIdx];
    numVertices = currObject.m_numVertices;
    const std::vector<CartesianCoord> & vertices = currObject.m_vertices;
    // +++ why take the first vertex z?
    newPolyCoord.m_z = vertices[0].m_z;
    
    if (transforms.m_doTransforms) {
      transformType = BOTH; //transform both position and direction
      applyXRTTransform(transformType, currPhotonPos, currPhotonDir, transforms.m_object2xrtframe[currObstructionIdx], localPos, localDir, currObstructionIdx);
    } else {
      localPos = currPhotonPos;
      localDir = currPhotonDir;
    }
    
    getXYForNewZ(localPos, localDir, newPolyCoord, tobsimpact);
    pointIsInsidePolygon = isPointInsidePolygon(newPolyCoord, numVertices, vertices);
    if (pointIsInsidePolygon) {
      photonAbsorbed = 5;
      // +++ In case we need this for diagnostic purposes, uncomment:
      // impactedobstructionid=currObstructionIdx
      pathCode[numInteractions][A]  = 1;
      pathCode[numInteractions][BC] = 8;
      pathCode[numInteractions][D]  = 1;
      pathCoords[numInteractions].m_x = currPhotonPos.m_x + (tobsimpact * currPhotonDir.m_xDir);
      pathCoords[numInteractions].m_y = currPhotonPos.m_y + (tobsimpact * currPhotonDir.m_yDir);
      pathCoords[numInteractions].m_z = currPhotonPos.m_z + (tobsimpact * currPhotonDir.m_zDir);
      return;
    }
    
    currObstructionIdx++;
    
  } // end-while: impact with first group of obstructions routine
  
  // If we have reached this point it means that the photon did not miss the main telescope aperture, and successfully passed through any external objects, (e.g. thermal shield, top), and avoided the first group of obstructions (immediately above primaries, and above pre- collimator if there is one).
  // Now we begin interaction with the main telescope
  // Determine the gap number, and shell numbers that enclose the photon initial position, and also the sector number corresponding to that position.
  // A gap is defined as the interval between the front-side faces of two adjacent foils (or between the inner housing wall and the front-side of the first foil; or between the front-side of the last foil and the outer housing radius).
  // Thus, a gap is not completely empty but includes the thickness of the innermost foil of a pair (except for the first gap).
  // The gapnumber goes from 1 for the gap between the inner housing radius and the 1st shell, to numgapradialboundaries-1, or (number of foil shells)+1.
  // The routine will utilize gapradialboundaries which will already have taken into account whether the first objects encountered are pre-collimator or mirror foils. (The shell positions for the pre-collimator are not exactly the same as the primary mirror positions of the top edges of the foils).
  // Use a bisection method to locate the gap number and radii corresponding photon initial radius. The variables numgapradialboundaries and gapradialboundaries were calculated in xrtsetup().
  // Note that the gap & sector that are located do not account for misalignments so could be wrong at this stage. However, fastmode overcomes this by selecting a gap and sector range that covers likely misalignment ranges, and then subsequent impact testing routines will identify the correct photon-object impacts. The purpose of this initial gap and sector location is simply to serve as an anchor around which a range of sub-foils will be selected for testing. 
  int gapindex1;
  int gapindex2;
  bisectionLocate(sectorsShells.m_gapradialboundaries, sectorsShells.m_numgapradialboundaries, initialPhotonRadius, gapindex1, gapindex2);
  int currentgapnumber = gapindex1 + 1;
  int currentloshell = gapindex1;
  int currenthishell = currentgapnumber;
  // Determine the sector number that the initial photon position lies in. The sector number goes from 1 to // of segments X // sectors per segment (note that the "sectornumber" in the TDF resets to 1 at the start of each segment so is not what is required here). We call it the “aperture sector number” and an interpolation grid for this was calculated in xrtsetup(). The interpolation grid goes from 0 to 360 degrees and contains an artificial sector if one of the original sectors crossed the +ve x-axis (0 degree line).
  // The units of the angle boundaries are radians so the initial photon phi also has to be in radians
  int sectorgapindex1;
  int sectorgapindex2;
  bisectionLocate(sectorsShells.m_mirrorsectorangleboundaries, sectorsShells.m_nummirrorsectorangleboundaries, initialPhotonPhi, sectorgapindex1, sectorgapindex2);
  int currentsectornumber = sectorgapindex1 + sectorsShells.m_zerosectorid + 1;
  if (currentsectornumber > sectorsShells.m_numApertureSectors) {
    currentsectornumber -= sectorsShells.m_numApertureSectors;
  }
  // Initialize some counters for reflections
//  int numPrimaryImpacts = 0;    // +++ these aren't used
  int numPrimaryReflections = 0;
//  int numSecondaryImpacts = 0;
  int numSecondaryReflections = 0;
  int numDoubleReflections = 0;
//  int numPcolImpacts = 0;
//  int numPcolReflections = 0;
  int numTotalReflections = 0;
  
  // maximum number of impact candidates: 
  int maxnumcandidates = maxNumPathCoords;
  
  // A list of XRT object IDs that are potential impact candidates
  std::vector<int> candidateobjectlist(maxnumcandidates);
  
  // The following is an integer array whose elements can be 0, positive, or negative and is equal to the change in the gap number that would occur if the current photon interacted with the XRT object referred to by a particular element. When added to the current gap number it will give the new gap number. The reason for using this variable is entirely because of misalignments: it indicates whether the initial 1st interaction of the photon is not in the same gap as the initial photon position and its value is used to modify the current gap number.
  
  // Note that a change in gap number can also occur if the photon is transmitted through a foil. This change in gap number is taken care of later, in a different way.
  std::vector<int> candidateobjectchangegap(maxnumcandidates);
  
  // The following is just for diagnostics and not used
  std::vector<int> candidateobjectsector(maxnumcandidates);
  
  // objectset=0,1,2 corresponds to the pre-collimator, primary and secondary sets & the value of currentobjectset signifies which set is being treated. The variable numcurrentobjectsetimpacts counts the number of impacts that have taken place in the current objectset. If a photon transmits through to a different shell this impact counter is reset to zero.
  
  // Set the following to zero each time currentobjectset is incremented
  int numcurrentobjectsetimpacts = 0;
  
  // Counter for number of impacts in the current gap
  // Set the following to zero each time the currentgapnumber is incremented. 
  int numcurrentgapimpacts = 0;
  
  // We will loop over the groups of pre-collimators, primaries, and secondaries in that order, indicated by the value of the variable currentobjectset being 0, 1, or 2 respectively. First we need to set the group first encountered by photons entering the aperture, allowing for the possibility that some telescopes will not have a pre-collimator.
  int currentobjectset = (pcolExists ? 0 : 1);
  
  int candidateobjectid = 0;
  
  //  *********************The following is the start of the main loop *************************

  // Start a do-while loop (while photon not terminated and the flag for leaving the telescope is false: this flag will be set if there has been a double reflection and no more back-side secondaries can be impacted.)
  // In fastmode, if there are no impacts, and if currentobjectset=primary or secondary, the photon is terminated because there can be no double-reflection event. If there *is* an impact in the primary or secondary we come back here with currentobjectset or currentgapnumber changed.
  // In either case we make a new candidate list but omit the last impacted object.
  
  // The following is a flag to indicate that the photon will survive going through the telescope all the way to the bottom of the secondaries, and therefore it will be able to impact the results plane (=focal plane by default)
  bool photonEscapesToFP = false;
  // to flag whether a current gap number is the first or last
  bool firstgap = true;
  bool lastgap = true;
  
  while (!photonTerminated && !photonEscapesToFP) {
    // Note that the do-while loop should terminate if, after all the impact testing in the current gap, numcurrentgapimpacts is still 0: this is contained implicitly in "photonTerminated" because the the photonTerminated flag will be set to True if there are no impacts in the currentgap.
    
    // We have to flag whether the current gap number is the first or last 
    // (because it will be bounded by inner or outer housing respectively).
    if (currentgapnumber == 1) {
      firstgap = true;
    } else {
      firstgap = false;
    }
    if (currentgapnumber == sectorsShells.m_numGaps) {
      lastgap = true;
    } else {
      lastgap = false;
    }
    
    // Save the current photon direction vector
    pathDirs[numInteractions] = currPhotonDir;
    
    // Set up the impact candidate list
    // candidateobjectlist() will contain the xrt object id of the candidates.
    
    // Note that the gap number can only change due to misalignments upon the 1st impact (whether pre-collimator or primary): after that only double reflections in the same gap are vaild, unless the path is punctuated with transmission
    
    // Find candidate objects for photon impact in the currentobjectset. The number of candidates is set to 0 each time the photon appears in a new currentobjectset or interacts and survives. The list is ordered so that the least likely candidates are done last.
    int ncandidates = 0;
    
    // For all (pcol, pri, sec), the front-side of the foil in the outer shell in a gap is always a candidate, but we need to account for the fact that if we are in the last shell gap, there is no such candidate (the outer housing wall is there instead).
    if (!lastgap) {
      // Front side of the outer shell in this gap, in the current sector
      candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][currentsectornumber-1][currenthishell-1];
      if (candidateobjectid != lastImpactedObjectID) {
        candidateobjectlist[ncandidates] = candidateobjectid;
        candidateobjectchangegap[ncandidates] = 0;
        candidateobjectsector[ncandidates] = currentsectornumber;
        ncandidates++;
      }
    }
    
    // Back side of the inner shell in the current sector for pre-collimator foils.
    if ( !firstgap && (currentobjectset == 0) ) {
      candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][currentsectornumber-1][currentloshell-1];
      if (candidateobjectid != lastImpactedObjectID) {
        candidateobjectlist[ncandidates] = candidateobjectid;
        candidateobjectchangegap[ncandidates] = 0;
        candidateobjectsector[ncandidates] = currentsectornumber;
        ncandidates++;
      }
    }
    
    // Back side of the inner shell in the current sector for secondaries *only if* there has already been one primary and one secondary reflection.
    if ( !firstgap && (currentobjectset == 2) && (numDoubleReflections == 1) ) {
      candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][currentsectornumber-1][currentloshell-1];
      if (candidateobjectid != lastImpactedObjectID) {
        candidateobjectlist[ncandidates] = candidateobjectid;
        candidateobjectchangegap[ncandidates] = 0;
        candidateobjectsector[ncandidates] = currentsectornumber;
        ncandidates++;
      }
    }
    
    // Set the adjacent lower and upper sectors, taking into account that the sector number starts at 1 again after the highest sector number
    if (currentsectornumber > 1) {
      lowersector = currentsectornumber - 1;
    // Make sure lower sector is not <1
    } else { 
      lowersector = sectorsShells.m_numApertureSectors;
    }
    if (currentsectornumber < sectorsShells.m_numApertureSectors) {
      uppersector = currentsectornumber + 1; // Make sure upper sector is < number of sectors
    } else {
      uppersector = 1;
    }
    
    // Now add candidates in adjacent sectors if:
    // (1) it is the 1st interaction and any misalignments are active OR
    // (2) if is not 1st interaction *and* if sector side walls are "virtual"
    
    // Do the lower sectors first
    if (!lastgap) {
      // Front side of the outer shell in the lower sector but not if there has already been a double reflection
      candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][lowersector-1][currenthishell-1];
      if ( ( (numInteractions == 1) && transforms.m_doTransforms) || 
           ( (numInteractions > 1) && (XRTObjects[candidateobjectid].m_sideWalls[1] < 0.0) ) ) {
        if ( (candidateobjectid != lastImpactedObjectID) && 
             (numDoubleReflections == 0) ) {
          candidateobjectlist[ncandidates] = candidateobjectid;
          candidateobjectsector[ncandidates] = lowersector;
          candidateobjectchangegap[ncandidates] = 0;
          ncandidates++;
        }
      }
    }
    
    // Back side of the inner shell in the lower sector for pre-collimator foils
    if (!firstgap && (currentobjectset == 0)) {
      candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][lowersector-1][currentloshell-1];
      if ( ( (numInteractions == 1) && transforms.m_doTransforms) || 
           ( (numInteractions > 1) && (XRTObjects[candidateobjectid].m_sideWalls[1] < 0.0) ) ) {
        if (candidateobjectid != lastImpactedObjectID) {
          candidateobjectlist[ncandidates] = candidateobjectid;
          candidateobjectchangegap[ncandidates] = 0;
          candidateobjectsector[ncandidates] = lowersector;
          ncandidates++;
        }
      }
    }
    
    // Back side of the inner shell in the lower sector for secondaries *only if* there has already been one primary and one secondary reflection.
    
    if ( !firstgap && (currentobjectset == 2) && (numDoubleReflections == 1)) {
      candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][lowersector-1][currentloshell-1];
      if ( ( (numInteractions == 1) && transforms.m_doTransforms) || 
           ( (numInteractions > 1) && (XRTObjects[candidateobjectid].m_sideWalls[1] < 0.0) ) ) {
        if (candidateobjectid != lastImpactedObjectID) {
          candidateobjectlist[ncandidates] = candidateobjectid;
          candidateobjectchangegap[ncandidates] = 0;
          candidateobjectsector[ncandidates] = lowersector;
          ncandidates++;
        }
      }
    }
    
    // Now do the upper sectors
    
    if (!lastgap) {
      // Front side of the upper shell in the upper sector but not if there has already been a double reflection
      candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][uppersector-1][currenthishell-1];
      if ( ( (numInteractions == 1) && transforms.m_doTransforms) || 
           ( (numInteractions > 1) && (XRTObjects[candidateobjectid].m_sideWalls[1] < 0.0) ) ) {
        if ( (candidateobjectid != lastImpactedObjectID) && 
             (numDoubleReflections == 0) ) {
          candidateobjectlist[ncandidates] = candidateobjectid;
          candidateobjectchangegap[ncandidates] = 0;
          candidateobjectsector[ncandidates] = uppersector;
          ncandidates++;
        }
      }
    }
    
    // Back side of the outer shell in the current sector but for pre-collimator foils.
    if ( !firstgap && (currentobjectset == 0)) {
      candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][uppersector-1][currentloshell-1];
      if ( ( (numInteractions == 1) && transforms.m_doTransforms) || 
           ( (numInteractions > 1) && (XRTObjects[candidateobjectid].m_sideWalls[1] < 0.0) ) ) {
        if (candidateobjectid != lastImpactedObjectID) {
          candidateobjectlist[ncandidates] = candidateobjectid;
          candidateobjectchangegap[ncandidates] = 0;
          candidateobjectsector[ncandidates] = uppersector;
          ncandidates++;
        }
      }
    }
    
    // Back side of the outer shell in the current sector for secondaries *only if* there has already been one primary and one secondary reflection.
    if ((!firstgap) && (currentobjectset == 2) && (numDoubleReflections == 1)) {
      candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][uppersector-1][currentloshell-1];
      if ( ( (numInteractions == 1) && transforms.m_doTransforms) || 
           ( (numInteractions > 1) && (XRTObjects[candidateobjectid].m_sideWalls[1] < 0.0) ) ) {
        if (candidateobjectid != lastImpactedObjectID) {
          candidateobjectlist[ncandidates] = candidateobjectid;
          candidateobjectchangegap[ncandidates] = 0;
          candidateobjectsector[ncandidates] = uppersector;
          ncandidates++;
        }
      }
    }
    
    // If misalignments are active we add objects in the current sector that are in the gap below and above the current gap. We only do this for the first interaction, after that interactions must take place in the same gap. If there is a double reflection that crosses a gap boundary due to misalignments, the assumption is that it will make a negligible contribution to the focused image (and therefore to the effective area), since such an event is likely to be very rare. However, this should be tested by rigorous comparisons with the regular code. On the other hand, double reflections that cross gap boundaries due to transmission can make a significant contribution to the effective area at high energies, but transmission is correctly handled by moving the photon to the next gap when transmission occurs. Here we are simply selecting impact candidates in the current gap; movement across gaps will be handled later when the impacts are processed.
    
    if ( transforms.m_doTransforms && (numInteractions == 1) ) {
    
      // Obviously we don’t add candidates in the next higher gap if the current gap is the last one
      // +++ is numgaps - 1 the last gap?
      if (currentgapnumber < (sectorsShells.m_numGaps - 1) ) {
        // Front side of the outer shell in the next higher gap, in the current sector; interaction with this candidate will increase currentgapnumber by +1.
        candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][currentsectornumber-1][currenthishell];
        if (candidateobjectid != lastImpactedObjectID) {
          candidateobjectlist[ncandidates] = candidateobjectid;
          candidateobjectchangegap[ncandidates] = 1;
          candidateobjectsector[ncandidates] = currentsectornumber;
          ncandidates++;
        }
        
        // +++ should this comment be grayed out? p245 of trf
        // Back side of the inner shell in the next higher gap, in the current sector for pre-collimator foils but this object would already have been included a front-side of object in current gap
      }
      
      // Back side of the inner shell in the next lower gap, in the current sector for pre-collimator foils but not if the current shell is adjacent to the innermost shell (i.e. not if this is the 1st or 2nd gap)
      if (currentgapnumber > 2) {
        // 141122 added front side of next lower shell -this would already have been included as the back-side of an object in the current gap if it was a pre-collimator but not if it was a primary
        candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][currentsectornumber-1][currenthishell-2];
        if (candidateobjectid != lastImpactedObjectID) {
          candidateobjectlist[ncandidates] = candidateobjectid;
          candidateobjectchangegap[ncandidates] = -1;
          candidateobjectsector[ncandidates] = currentsectornumber;
          ncandidates++;
        }
        if (currentobjectset == 0) {
          candidateobjectid = sectorsShells.m_setsectorshell2xrtobjectid[currentobjectset][currentsectornumber-1][currentloshell-2];
          if (candidateobjectid != lastImpactedObjectID) {
            candidateobjectlist[ncandidates] = candidateobjectid;
            candidateobjectchangegap[ncandidates] = -1;
            candidateobjectsector[ncandidates] = currentsectornumber;
            ncandidates++;
          }
        }
      }
     
    } // end-if dotransforms==True or numInteractions==1
    
    // Done setting up impact candidate list
    
    // It is possible that no candidates are found. If so, unless currentobjectset=0 (pre-collimator) we must terminate the photon because we cannot then get a double-reflection event. For the pre-collimator set, increment currentobjectset to move to the primaries.
    if (ncandidates == 0) {
      if (currentobjectset == 0) {
        currentobjectset = 1;
      } else {
        photonTerminated = true;
        // The photon path is ended prematurely by fastmode and the pathcode default is already set
        return;
      }
    }
    
    // Start a do-while loop over candidate object ids (while no object impacted or candidate list is not exhausted).
    // +++ why start at 1, and then subtract 1?
    int candidateidnumber = 1;
    bool objectWasImpacted = false;
    
    while ( !objectWasImpacted && (candidateidnumber <= ncandidates) ) {
      
      // Change photon frame due to misalignments
      if (transforms.m_doTransforms) {
        // +++ transformType is never not BOTH in this function.  Save time by not assigning it so often?  Or is the chance that we could change it later bigger?
        transformType = BOTH; // transform both position and direction
        applyXRTTransform(transformType, currPhotonPos, currPhotonDir, transforms.m_object2xrtframe[candidateidnumber-1], localPos, localDir, currObstructionIdx);
      } else {
        localPos = currPhotonPos;
        localDir = currPhotonDir;
      }
      
      objectTypes_e objectType;
      surfaceGeometryTypes_e surfaceGeometry;  
      Slopes slopes;
      std::vector<double> geoParams;
      int numVertices = 0;
      std::vector<CartesianCoord> vertices;
      ObjectBoundingBox objectBBox;
      double startAngle = 0.0;
      double endAngle = 0.0;
      double startAngleMod = 0.0;
      double endAngleMod = 0.0;
      int faceHit = 0;
      int exitFace = 0;
      CartesianCoord impactCoordOrig;
      CartesianCoord impactCoordStd;
      double impactdistance = 0.0;
      int errorCode = 0;
      bool killThePhoton = false;
      
      // Set geoparams etc., and test for impact
      // Set some input parameters for the routine that are specific to the particular candidate XRT object that is being treated
      int currObjIdx = candidateobjectlist[candidateidnumber-1];
      // currObject goes out of scope after this candidate for-loop
      const XRTObject & currObject = XRTObjects[currObjIdx];
      // +++ it would be cleaner and easier if I just passed in a reference to the whole object, instead of assigning all these variables.
      surfaceGeometry = currObject.m_geometry;
      objectType = currObject.m_type;
      geoParams = currObject.m_geoParams;
      startAngle = std::abs(currObject.m_sideWalls[0]);
      endAngle = std::abs(currObject.m_sideWalls[1]);
      startAngleMod = currObject.m_sideWalls[4];
      endAngleMod = currObject.m_sideWalls[5];
      vertices = currObject.m_vertices;
      numVertices = currObject.m_numVertices;  // +++ according to above line in TRF, this is 8.  why store?
      slopes = currObject.m_slopes;
      objectBBox = currObject.m_BBox;    // +++ so here is a case where we don't need radial bounds
      
      getPhotonObjectImpactCoords(candidateobjectid, objectType, surfaceGeometry, has3DObjects, slopes, geoParams, numVertices, vertices, objectBBox, startAngle, endAngle, startAngleMod, endAngleMod, currPhotonPos, currPhotonDir, localPos, localDir, faceHit, exitFace, impactCoordOrig, impactCoordStd, impactdistance, pathLength, errorCode, killThePhoton);
      
      
      // Terminate the photon if the above routine flagged the photon path as anomalous (it is rare, and such events would never contribute to the effective area)
      if (killThePhoton) {
        photonTerminated = true;
        return;
      }
      
      if (faceHit >= 0) {
        objectWasImpacted = true;
        // There was an impact; if this is the 1st impact check if the currentgapnumber changes (due to foils being misaligned)
        if (numInteractions == 1 ) {
          currentgapnumber = currentgapnumber + candidateobjectchangegap[candidateidnumber-1];
        }
        
        // Update the number of impacts in the object set and this gap
        numcurrentobjectsetimpacts = numcurrentobjectsetimpacts+1;
        numcurrentgapimpacts = numcurrentgapimpacts+1;
        
        // Note that numInteractions will already have been incremented at the end of the previous loop iteration
        
        // For this interaction save the coordinates and various other attributes (the coordinates are coordinates in the XRT frame)
        pathXRTObjectID[numInteractions] = candidateobjectlist[candidateidnumber-1];
        pathCoords[numInteractions] = impactCoordOrig;
        pathobjectframecoords[numInteractions] = impactCoordStd;
        pathImpactDistance[numInteractions] = impactdistance;
        pathtransdistance[numInteractions] = pathLength;
        pathFaceHit[numInteractions] = faceHit;
        
        // We already know part of the pathcode:
        pathCode[numInteractions][BC] = currentobjectset + 5;
        pathCode[numInteractions][D]  = pathFaceHit[numInteractions] + 1;
        
        // Note that pathphotondir (photon direction vector) is saved before the interaction because it is defined as the direction before impact
        
        // Save the impactedobject to avoid interaction with the same object in the next iteration
        lastImpactedObjectID = pathXRTObjectID[numInteractions];
      
      } else {
        
        // If there was no impact and we already had a double reflection and currentobjectset=2 then flag the photon to escape to the results plane (=focal plane by default)
        if ((numDoubleReflections == 1) && (currentobjectset == 2)) {
          photonEscapesToFP = true;
        }
      } // end-if there was an impact
      
      // In fast mode various conditions terminate the photon. Any one of the following can be commented out to change the behaviour of fast mode.
      
      // Bottom or side impacts terminate the photon
      if (faceHit > 2) {
        photonTerminated = true;
        return;
      }
      
      // If the path length is negative it means we do not treat transmission (e.g. entry on top edge of foil, exit on bottom edge), and it is an event that cannot be involved in a double-reflection path
      if ((faceHit >= 0) && (pathLength < 0.0e0)) {
        photonTerminated = true;
        return;
      }
      
      // Back-side primary impacts cannot result in double-reflection focal plane impact but are permitted if there has already been a primary reflection because the photon could be transmitted and reflect on a secondary in the new gap
      if ((currentobjectset == 1 ) && (faceHit == 0) && (numPrimaryReflections == 0)) {
        photonTerminated = true;
        return;
      }
      
      // Front-side secondary impacts are only accepted if there has already been a primary reflection
      if ((currentobjectset == 2) && (faceHit == 1 ) && (numPrimaryReflections == 0)) {
        photonTerminated = true;
        return;
      }
      
      // Back-side secondary impacts are only accepted if there has already been a primary & secondary double-reflection
      if ((currentobjectset == 2) && (faceHit == 0) && (numDoubleReflections == 0)) {
        photonTerminated = true;
        return;
      }
      candidateidnumber++;
      
      
    } // end-while loop over impact candidates
    
    // If there were no impacts in this shell gap for this currentobjectset:
    //        if currentobjectset=pcol:increment currentobjectset [this forces leaving pcol]
    //        if currentobjectset=pri: no double reflection possible so terminate the photon
    //        if currentobjectset=sec: if double reflection=false terminate the photon;
    //                else if doublereflection=true and there are no secondary impacts
    //                in this shell photon escapes to results plane
    // (else we will check for secondary back-side transmissions)

    
    if (numcurrentobjectsetimpacts == 0) {
      
      AH_DEBUG << "currentobjectset = " << currentobjectset << std::endl;
      
      if (currentobjectset == 0) {
        currentobjectset = 1; 
        // (if no p-col impacts move to primaries) 
        // Reset impact counter for primaries
        numcurrentobjectsetimpacts = 0;
      } else if (currentobjectset == 1 ) {
        photonTerminated = true;
        // (if no primary impacts terminate)
        return;
      } else if (currentobjectset == 2) {
        if ( (numDoubleReflections == 0) or (numcurrentgapimpacts == 0) ) {
          photonTerminated = true;
          // (if there have been no double reflections or no impacts so far, terminate – but maybe numcurrentgapimpacts==0 is redundant?)
        } else if (numDoubleReflections == 1) {
          photonEscapesToFP = true;
          // (if back-side secondary is impacted after a double reflection, flag it for possible results-plane impact, pending successful transmission)
        }
      }
      
      
      AH_DEBUG << "currentobjectset = " << currentobjectset << std::endl;
      AH_THROW_RUNTIME("");
      
      // else if there was an impact in the current object set and the photon has not already terminated, the interaction needs to be processed according to the type of object and interaction.
    } else if ( (numcurrentobjectsetimpacts > 0) && objectWasImpacted && !photonTerminated ) { // D
    
      // +++ p252 trf
      
      int impactedXRTObjectID = pathXRTObjectID[numInteractions];
      // get the object that was impacted
      const XRTObject & currObject = XRTObjects[impactedXRTObjectID];
      
      // Test for object type and face; pcol all faces, pri & sec front and top only (back hits killed)
      
      // +++ From this point the routine is very similar to raytraceonephoton()
      
      // If pathLength is non-negative, it means we need to get the transmission optical depths for the rough materials. In the case of a front-side mirror hit we will get the thin-film transmission later because we need the incident angle for that. In the case of a pre-colllimator, both front and back are rough materials. However, this if-block pertains to transmission only for the bodies of the foils, and optical depth per mm for this can be accessed simply by specifiying the array xrtobjectroughtaupermmindex which points to the correct values in roughtaupermm according to the XRT object type so we don't need to explicitly know what type of object since this has aleady been set up so we only need to specify the object id. (The index points to group number, where group 0 corresponds to all the pre-collimator foils)
      
      thickTransmissionProb = 0.0;

      AH_DEBUG << "pathtransdistance[numInteractions] = " << pathtransdistance[numInteractions] << std::endl;
      AH_DEBUG << "currObject.m_doTransmission = " << (currObject.m_doTransmission ? "is true" : "is false") << std::endl;


      if ( (pathtransdistance[numInteractions] > 0.0) &&
           (currObject.m_doTransmission) ) {
        thicktau = pathtransdistance[numInteractions] * 
                    roughtaupermm[currObject.m_tauPermmIndexBack];
        thickTransmissionProb = exp(-1.0 * thicktau);
        AH_DEBUG << "tentative thick transmission in raytraceOnePhoton()" << std::endl;
        AH_DEBUG << "currObject.m_tauPermmIndexBack = " << currObject.m_tauPermmIndexBack << std::endl;
        AH_DEBUG << "roughtaupermm[currObject.m_tauPermmIndexBack] = " << roughtaupermm[currObject.m_tauPermmIndexBack] << std::endl;
        AH_DEBUG << "thickTransmissionProb = " << thickTransmissionProb << std::endl;

      } else {
        // If pathtransdistance is negative it means that the transmission is effectively considered to be zero. For example, an entry point on the front/back and exit on a side would be such a case. We do not terminate the photon because it could still reflect.
        thickTransmissionProb = 0.0;
        //+++ remove this else and just leave the default value?
      }

      // Now we need to calculate a reflection probability. For the case that the ray entry was on one of the 4 edges/sides, there is no reflection, only transmission or absorption. For the reflection probability we do need to get the incident angle but we don't get the reflection direction until/if the photon is definitely going to be reflected.
      // The action now depends on which face of the foil was impacted.

      if (pathFaceHit[numInteractions] > 1) {

        // If one of the four thin (edge) faces was hit, the photon is either transmitted or absorbed so we set the reflection probability to zero. The thick material transmission probability will already have been calculated earlier.
        
        // (Note that in fastmode we are only considering possible transmission from top edge through a face: side hits should already have terminated the photon)
        
        reflectionProb = 0.0;
        thinTransmissionProb = 1.0;
        nettransmissionprob = thickTransmissionProb;
        
      } else if ((pathFaceHit[numInteractions] == 0) || (pathFaceHit[numInteractions] == 1)) {
      
        // For the case that either the front or back foil surface is impacted, we have to first calculate the incident angle before we can calculate the reflection probability. {, for this XRT object, xrtobjectreflindex has pointers to the front and back reflectivity arrays (as a function of angle). Use faceHit to select the correct xrtobjectreflindex. Extract the correct array as a function of angle and interpolate on this for the derived incident angle to return a single value for the reflection probability.
        // First get the normal vector to the foil surface in the XRT object frame
        currSurfaceGeometry = currObject.m_geometry;        // {OBSTRUCT, CYLINDER, CONE}
        currobjectType = currObject.m_type;                 // {OBSTRUCTION, FOIL}
        currSlopes = currObject.m_slopes;
        currGeoParams = currObject.m_geoParams;
        normalPos = pathobjectframecoords[numInteractions];
        getSurfaceNormal(currSurfaceGeometry, currGeoParams, currobjectType, currSlopes, pathFaceHit[numInteractions], normalPos, normalDir, normalError);

        // +++ used below...
        currObjectSet = currObject.m_set;

        // Get the dot product of the normal with the photon direction in the XRT object frame; the incident angle is the arcsin of the dot product because it is the grazing angle that we want, not the angle relative to the normal.
        singrazingIncidentAngle = ( (normalDir.m_xDir * localDir.m_xDir) + 
                                    (normalDir.m_yDir * localDir.m_yDir) + 
                                    (normalDir.m_zDir * localDir.m_zDir) ) * -1.0;
        grazingIncidentAngle = std::abs(std::asin(singrazingIncidentAngle));

        AH_DEBUG << "singrazingIncidentAngle = " << singrazingIncidentAngle << std::endl;
            
        if (pathFaceHit[numInteractions] == 0) {
          // back surface reflection
          AH_DEBUG << "back surface reflection" << std::endl;

          reflindex = currObject.m_reflIndexBack;

          gapIncrement = -1;

          numReflGridPts = numRoughAngles;
          reflectionGrid.clear();
          reflectionGrid.reserve(numRoughAngles);

          for (int i = 0 ; i < numRoughAngles ; ++i) {
            reflectionGrid.push_back(roughRefl[i][reflindex]); //+++ this is correct?  reserve then pushback?
          }

          angleGrid = roughAngles;
          angleGridMin = roughAngles[0];
          angleGridMax = roughAngles[numRoughAngles-1];
          
          // Set a flag inidicating whether the incident angle is out of range (i.e. not covered by the reflectivity and transmission arrays). If it is out of range, mark the photon for absorption
          if ( (grazingIncidentAngle < angleGridMin) || 
              (grazingIncidentAngle > angleGridMax) ) {
            angleOutOfRange = true;
          } else {
            angleOutOfRange = false;
            if ( (pathtransdistance[numInteractions] > 0.0) && 
                 (currObject.m_doTransmission) ) {
              double thinTau = pathtransdistance[numInteractions] * 
                               frontTauPermm[currObject.m_tauPermmIndexBack];
            thinTransmissionProb = exp(-1.0 * thinTau);
            } else {
              // If pathtransdistance is negative it means that the transmission is effectively considered to be zero. For example, an entry point on the front/back and exit on a side would be such a case. We do not terminate the photon because it could still reflect.
              thinTransmissionProb = 0.0;
            }
          }
        
        } else if (pathFaceHit[numInteractions] == 1) {
              // +++ use an enum
          
          gapIncrement =1;
          
          // Front surface reflection
          AH_DEBUG << "front surface reflection" << std::endl;

          reflindex = currObject.m_reflIndexFront;

          // for mirror foils the index points to one of the groups in frontRefl; for precollimator foils the index points to the rough surface reflectivity. The integer xrtobjectset indicates whether we have a mirror foil or pre-collimator. If in the future there are other objects aside from pre-collimator or mirrors then the conditionals will have to change. Maybe always reserve currObjectSet=0 for rough front surface reflectivity.
          if (currObjectSet == 0) {
            // set=0 means pcol
            numReflGridPts = numRoughAngles;
            reflectionGrid.clear();
            reflectionGrid.reserve(numRoughAngles);
            for (int i = 0 ; i < numRoughAngles ; ++i) {
              reflectionGrid.push_back(roughRefl[i][reflindex]);
            }
            angleGrid = roughAngles;
            angleGridMin = roughAngles[0];
            angleGridMax = roughAngles[numRoughAngles-1];
            thinTransmissionProb = 1.0;
          } else {
            // set!=0 means primary or secondary
            numReflGridPts = numIncidentAngles;
            reflectionGrid.clear();
            reflectionGrid.reserve(numIncidentAngles);
            for (int i = 0 ; i < numIncidentAngles ; ++i) {
              reflectionGrid.push_back(frontRefl[i][reflindex]);
            }
            angleGrid = incidentAngles;
            angleGridMin = incidentAngles[0];
            angleGridMax = incidentAngles[numIncidentAngles-1];
          } // end if-block determining whether the XRT object is a mirror or pre-collimator foil

          // Set a flag inidicating whether the incident angle is out of range (i.e. not covered by the reflectivity and transmission arrays).
          if ( (grazingIncidentAngle < angleGridMin) || 
              (grazingIncidentAngle > angleGridMax) ) {
            angleOutOfRange = true;
          } else { 
            angleOutOfRange = false;
            // Interpolate thin-film transmission probability for the calculated incident angle
            // this is only meant for thin film transmission, not precollimators (which are thick)
            if ( currObject.m_doTransmission && (currObjectSet > 0) )   {
              transmissionGrid.clear();
              transmissionGrid.reserve(numIncidentAngles);
              for (int i = 0 ; i < numIncidentAngles ; ++i) {
                transmissionGrid.push_back(frontTran[i][reflindex]);
              }
              // +++ I forgot why I thought this should be passed angleGrid vs incidentAngles.  If I get an error again, record it
              // +++ 20140912 saw error again.  email from Tahir.
              bisectionInterp(numReflGridPts, angleGrid, transmissionGrid, grazingIncidentAngle, thinTransmissionProb);
            } // end if-block determining whether to interpolate transmission or not

          } // end if-block that checks if incidentangle values are out of range

        } // end if-block determining whether front-side or back-side of a foil was impacted (faceHit=0 or 1)
            
        
        // Interpolate reflection probability for the calculated incident angle if the incident angle is not out of range. If it is out of range, terminate the photon and leave.
        if (!angleOutOfRange) {
          AH_DEBUG << "!angleOutOfRange, about to call bisectionInterp" << std::endl;
          bisectionInterp(numReflGridPts, angleGrid, reflectionGrid, grazingIncidentAngle, reflectionProb);

          // Calculate the net transmission probability due to any thin-film surface and the thick body of the XRT object. For surfaces other than the front-side mirror, there is also a 1-(reflection probability) factor.
          if ( (currObject.m_doTransmission) &&
                (pathFaceHit[numInteractions] == 1) ) {
            nettransmissionprob = thinTransmissionProb*thickTransmissionProb;
          } else if (currObject.m_doTransmission) {
            nettransmissionprob = thinTransmissionProb*thickTransmissionProb*(1-reflectionProb);
          } else if (!currObject.m_doTransmission) {
            nettransmissionprob = 0.0;
          }

          AH_DEBUG << "grazingIncidentAngle = " << grazingIncidentAngle << std::endl;
          AH_DEBUG << "final transmission probabilities" << std::endl;
          AH_DEBUG << "thinTransmissionProb = " << thinTransmissionProb << std::endl;
          AH_DEBUG << "thickTransmissionProb = " << thickTransmissionProb << std::endl;
          AH_DEBUG << "nettransmissionprob = " << nettransmissionprob << std::endl;

        } else {
          photonAbsorbed = 8;
          pathErrorCode[numInteractions] = angleOutOfRangeCode;
          pathCode[numInteractions][0] = 1;
          pathCode[numInteractions][1] = currObjectSet+5;
          pathCode[numInteractions][2] = pathFaceHit[numInteractions]+1;
          return;
        }

        //  ^^ ALL THE REFLECTION, TRANSMISSION, & SCATTERING TOOK PLACE IN THIS BLOCK. ^^
        // ===============================================================================

      } // end if-block that determines whether either the back-side or front-side were impacted (as opposed to the sides)
      
      // Update the pathCode for which face was hit
      pathCode[numInteractions][2] = pathFaceHit[numInteractions] + 1;
      // Now at this stage we have a reflection probability and a transmission probability. For reflection we have to use the local photon direction (in the object frame) to generate the reflected ray direction and convert this back to the XRT frame.
      // Calculate absorption probability and other probability boundaries
      absorptionprob = 1.0 - reflectionProb - nettransmissionprob;
      abslobound = 0.0;
      abshibound = absorptionprob;
      refllobound = absorptionprob;
      reflhibound = absorptionprob + reflectionProb;
      transmissionlobound = reflhibound;
      transmissionhibound = 1.0;
      
      // +++ just in raytracefast
      pathreflprob[numInteractions] = reflectionProb;
      paththintransprob[numInteractions] = thinTransmissionProb;
      paththicktransprob[numInteractions] = thickTransmissionProb;
      
      chooseoutcome = getRandom();   // random number between 0 and 1
      if ( (chooseoutcome <= abshibound) && (absorptionprob > 0.0) ) { 
        // We have absorption
        AH_DEBUG << "We have absorption" << std::endl;

        photonAbsorbed = 999; // +++ make this a dummy value?
        pathCode[numInteractions][0] = 1;
        
        // +++ just in raytracefast
        photonTerminated = true;
        return;
        
        } else if ( ( (chooseoutcome > transmissionlobound) && (nettransmissionprob > 0.0) ) || 
                      (nettransmissionprob == 1.0) ) {
          // We have transmission
          AH_DEBUG << "We have transmission" << std::endl;

          // photon direction remains the same, we have already recorded the impact coordinates, just update the current position. (Don't put the photon at the exit point because that's not what happens physically and that would involve more transformations). Since we definitely have transmission in this part of the if-block, the foil is now effectively transparent.
          currPhotonPos = pathCoords[numInteractions];

          pathCode[numInteractions][0] = 4;
          
          // +++ just in raytracefast
          // Move to next gap
          currentgapnumber = currentgapnumber + gapIncrement;
          currenthishell = currentgapnumber;
          currentloshell = currentgapnumber-1;
          // The photon passes into a new gap so we have to reset the number of impacts in the current gap and the number of impacts in the currentonjectset.
          numcurrentobjectsetimpacts = 0;
          numcurrentgapimpacts = 0;
          
        } else if ( ( (chooseoutcome > refllobound) && (chooseoutcome <= reflhibound) && (reflectionProb > 0.0) ) || 
                      (reflectionProb == 1.0) ) {
          // We have reflection
          AH_DEBUG << "We have reflection" << std::endl;

          // The procedure is more complicated: first get the direction of the reflected photon in the XRT object frame, modify the direction of the reflected photon using the scattering distribution, convert photon direction back to XRT frame (the impact coordinates in the XRT frame were already recored earlier).

          // Get the direction of the reflected photon; the grazing incident angle was calculated earlier in order to calculate the reflection probability so the routine should not calculate it again.
          // The reflected vector will include any modification to the specular direction due to scattering. The probability for scattering and the scattering angle probability distribution are read from tables in the scattering file.

          // +++ just in raytracefast
          // First, update counters to keep track of number of double reflections
          if (faceHit == 1) {
            if (currentobjectset == 1) {
              numPrimaryReflections=numPrimaryReflections+1;
            } else if (currentobjectset == 2) {
              numSecondaryReflections = numSecondaryReflections+1;
            }
          }
          numTotalReflections=numPrimaryReflections+numSecondaryReflections;
          if ((numPrimaryReflections == 1l) && (numSecondaryReflections == 1)) {
            numDoubleReflections = numDoubleReflections+1;
          }
          
          // set output variables to 0 before entering
          scatteredangle = 0.0;
          sinReflected = 0.0;
          reflErrorCode = 0;

          // If reflection occurred (whether or not the reflected vector was affected by scattering away from the specular direction), calculate the new direction of the photon using getreflectiondirection().
          
          if (currObject.m_doScattering) {
            // do scattering 
            AH_DEBUG << "We have scattering." << std::endl;

            // If scattering is treated, calculate the modification to the specular direction and calculate the "effective" reflected angle, and the sine of that angle, which will then be an input to the routine getreflectiondirection().
            
            isSpecular = false;
            pathCode[numInteractions][0] = 3;  // specular reflection+scattering

            scatsegmentid = currObject.m_segmentID;
            if (pathFaceHit[numInteractions] == 0) {
              scatcolumnindex = currObject.m_scatterIndexBack;
            } else {
              scatcolumnindex = currObject.m_scatterIndexFront;
            }

            // indicate to getscattereddirection whether to use the angle grids for front-side scattering or for rough scattering
            // +++ use an enum
            if ( (pathFaceHit[numInteractions] == 0) || 
                  ( (currObject.m_type == FOIL) && 
                    (currObject.m_set == 0) ) ) {
              isRoughScattering = true;
            } else {
              isRoughScattering = false;
            }

            getScatteredDirection(grazingIncidentAngle, scatsegmentid, scatcolumnindex, scatenergyindex, scat, isRoughScattering, scatteredangle);
            sinReflected = std::sin(grazingIncidentAngle + scatteredangle);

            AH_DEBUG << "isRoughScattering = " << (isRoughScattering ? "it's true" : "it's false") << std::endl;
            AH_DEBUG << "grazingIncidentAngle = " << grazingIncidentAngle << std::endl;
            AH_DEBUG << "scatteredangle = " << scatteredangle << std::endl;
            AH_DEBUG << "sinReflected = " << sinReflected << std::endl;

          } else {
            // don't do scattering
            AH_DEBUG << "We don't have scattering." << std::endl;

            // Set the flag for the routine to do specular reflection if appropriate, because it is much quicker than applying the full set of equations with a scattering angle of zero
            isSpecular = true;
            pathCode[numInteractions][0] = 2;    // specular reflection only

          }   // end-if doscattering

          AH_DEBUG << "localDir = " << localDir << std::endl;
          getReflectionDirection(localDir, normalDir, isSpecular, singrazingIncidentAngle, sinReflected, newLocalDir, reflErrorCode);
          AH_DEBUG << "newLocalDir = " << newLocalDir << std::endl;
          
          // Change current photon direction back to the XRT frame. For the moment we do not have the transformation routines in place so only doTransforms=False works. Note that the impact and photon position coordinates already have XRT frame values since they had to be calculated along the way.
          if (transforms.m_doTransforms) {
            AH_DEBUG << "doTransforms" << std::endl;
            transformType = DIRECTION;
            CartesianCoord dummyCoordOrig;
            CartesianCoord dummyCoordNew;
            applyXRTTransform(transformType, dummyCoordOrig, newLocalDir, transforms.m_xrt2objectframe[pathXRTObjectID[numInteractions]], dummyCoordNew, currPhotonDir, pathXRTObjectID[numInteractions]);

            AH_DEBUG << "right after calling applyXRTTransform() third time" << std::endl;
            AH_DEBUG << " currPhotonPos = " << currPhotonPos << std::endl;
            AH_DEBUG << " newLocalDir = " << newLocalDir << std::endl;
            AH_DEBUG << " currPhotonDir      = " << currPhotonDir << std::endl;

          } else {
            AH_DEBUG << "don't doTransforms" << std::endl;
            currPhotonDir = newLocalDir;
          }

          // Update the current photon position
          currPhotonPos = pathCoords[numInteractions];

        } // end if-block that selects between absorption, reflection, and transmission (the last part of the if-block treated reflection)
        
        
      // We are still inside the if-block and do-while loop following a particular interaction of the photon with a telescope component. The interaction has now played out. Whatever the outcome was, the impact coordinates are in potentialimpactcoords(minimpactdistindex,0:2) and pathcoordinates(numInteractions,0:2), and other attributes of this interaction are in accompanying arrays.
      // Finally, update the counter for the number of interactions between photon and XRT components.
      // If there has already been one primary reflection, move to the secondaries
      numInteractions++;
      if ((currentobjectset == 1) && (numPrimaryReflections == 1)) {
        currentobjectset = 2;
        numcurrentobjectsetimpacts = 0;
      }
      AH_DEBUG << std::endl;
      // +++ p259 trf
      
    } // end-if zero or a non-zero number of actual impacts
    AH_DEBUG << std::endl;
  } // end main while loop that keeps going until the photon is absorbed or escapes the telescope structure to hit the results plane.
  AH_DEBUG << std::endl;
  // +++ vvv same as raytracePhotonFast vvv
  
  // If this point has been reached with photonAbsorbed=0, it means that the 
  // photon has survived passing through the telescope (i.e. to the bottoms of 
  // the secondary mirrors), and now we need to check if any external objects 
  // (e.g., thermal shield, bottom) are intercepted on the way to the focal 
  // plane. The photon may or may not survive.
  
  if (bottomExtObjects.m_numExtObjectParts > 0) {
    
    AH_DEBUG << "search through bottom external objects" << std::endl;
    
    // unused output for getXYForNewZ(), projecting phtoon to bottom ext objs
    double timetobottom = 0.0;
    // projected coordinates of photon as it moves through bottom objects
    CartesianCoord bottomExtObjCoord(0.0, 0.0, bottomExtObjects.m_zCoord);
    double extobjectbottomrad = 0.0;
    double extobjectbottomphi = 0.0;
    
    // Project the photon to the z-coordinate of the plane containing the 
    // external objects below the telescope and get new x, y coordinates there
    getXYForNewZ(currPhotonPos, currPhotonDir, bottomExtObjCoord, timetobottom);
    cartesianToPolar(bottomExtObjCoord, extobjectbottomrad, extobjectbottomphi);
    
    // Loop over each of the external objects; we can stop if/as soon as there 
    // is an impact because it is assumed that external objects do not overlap
    for (int iExtObj = 0 ; iExtObj < bottomExtObjects.m_numExtObjectParts ; ++iExtObj) {
      
      if ( (extobjectbottomrad >= bottomExtObjects.m_rMin[iExtObj]) &&
           (extobjectbottomrad <= bottomExtObjects.m_rMax[iExtObj]) && 
           (extobjectbottomphi >= bottomExtObjects.m_startAngleRad[iExtObj]) && 
           (extobjectbottomphi <= bottomExtObjects.m_endAngleRad[iExtObj]) ) {
        
        // photon-object intercept; now check if photon is transmitted
        if (bottomTransProb[iExtObj] == 0.0) {
          // The object is to be treated as an obstruction and the photon is 
          // terminated (bottomtransprob(i) was set 0.0 earlier if the 
          // mass-absorption index was negative)
          photonAbsorbed = 10;
        } else {
          // Generate a random number to see if the photon will be transmitted
          double bottomxrtobjectrandomtrans = getRandom();
          if (bottomxrtobjectrandomtrans > bottomTransProb[iExtObj]) {
            // Photon is absorbed if the random number is > trans. prob.
            photonAbsorbed = 10;
          }
        }
        
        // Whatever the outcome, save the photon impact position. We do not 
        // update the current photon position because the main telescope 
        // components may be misaligned: a more accurate position for impact on 
        // the focal plane (if there is one) will be obtained because if the 
        // photon is transmitted, it is as if the external object is not 
        // present once the outcome of successful transmission is determined. 
        // Note that impact with the external object does not change the photon 
        // direction so we do not need to update that.
        pathCoords[numInteractions] = bottomExtObjCoord;
        pathDirs[numInteractions] = currPhotonDir;
        
        // Set the pathcode depending on the outcome; if the photon was 
        // absorbed return, if not continue after incrementing counter for 
        // number of interactions ready for the next interaction.
        if (photonAbsorbed > 0) {
          pathCode[numInteractions][A]  = 1;
          pathCode[numInteractions][BC] = 10;
          pathCode[numInteractions][D]  = 1;
          return;
        } else {
          pathCode[numInteractions][A]  = 4;
          pathCode[numInteractions][BC] = 10;
          pathCode[numInteractions][D]  = 1;
          numInteractions++;
        }
        
        // photon has been intercepted. break out of this loop through 
        // bottom external objects
        break;
        
      } // end-if checking for photon-object intercept
      
    } // end-while loop over bottom object parts, checking for an intercept
    
  } // end-if any external objects below the main telescope
  // +++ ^^^ same as raytracePhotonFast ^^^
  
  // +++ vvv same as raytracePhotonFast vvv
  
  // +++ put this inside the next if?  to be clearer.
  // if we made it to the results plane, set the z of the photon
  resultPlaneCoord.m_z = resultsplanez;
  
  // if the photon was not absorbed in the XRT structure, find the coordinates 
  // of the impact on the results plane.
  if (photonAbsorbed == 0) {
      
    resultsPlaneWasImpacted = true;
    
    CartesianCoord coordIn = pathCoords[numInteractions-1];
    getXYForNewZ(coordIn, currPhotonDir, resultPlaneCoord, tresultsplane);
    // Save the coordinates in the pathCoords array
    pathCoords[numInteractions] = resultPlaneCoord;
      
    // Set the result code for this final interaction
    pathCode[numInteractions][0] = 1;
    pathCode[numInteractions][1] = 1;
    pathCode[numInteractions][2] = 1;
      
    // Copy the final photon attributes into convenience arrays (we don’t know 
    // in advance how many interactions a given photon will have)
    finalPhotonPos = resultPlaneCoord;
    finalPhotonDir = currPhotonDir;
    // The object ID is set to -1 for a results/focal plane hit, otherwise it 
    // would be 0, being confused with an XRT object with that ID
    pathXRTObjectID[numInteractions] = -1;
      
  } else {
    
    AH_DEBUG << "no results plane impact.  decrement numInteractions" << std::endl;
    resultsPlaneWasImpacted = false;
    // The numInteractions variable has to be decremented if the photon did not 
    // make it to the focal plane because that counter is always 1 ahead of the 
    // actual number of interactions that have actually taken place.
    numInteractions--;
  }
  // +++ ^^^ same as raytracePhotonFast ^^^
  
  AH_DEBUG << "end of raytracePhotonFast." << std::endl;
  
} // end raytracePhotonFast()


/******************************************************************************/


void createCombinedPathCodeString(int numInteractions, 
                                  const std::vector< std::vector<int> > & pathCode, 
                                  std::string & combinedPathCodeString) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  const int numInteractionToRecord = 8;   
  // +++ this is 7 elsewhere.  I need to make this a static const.

  // for creating pathCodeStr
  std::string A;
  //std::stringstream BC;
  std::string D;
  std::string ABCD;

  // For each ray-object interaction convert the 3-integer result  in 
  // pathCode(i,0:2) into a single 4-character string ABCD, where:
  //     A  = pathCode[i][0]
  //     BC = pathCode[i][1] (put a leading ‘0’ if <10)
  //     D  = pathCode[i][2]
  // the first interaction, pathCode[0] is empty - it is simply the entry.  
  // we need pathCodeStr to store the first, plus the next numInteractions
  std::vector<std::string> pathCodeStr(numInteractions+1);

  // reset the output string
  combinedPathCodeString = "";
  
  // -------------------------------------
  
  
  // start at 1, because the 0th interaction is simply entry into the 
  // telescope. go all the way up to numInteractions, because that last
  // interaction is the last one we need
  for (int i = 1 ; i <= numInteractions ; ++i) {
    // clear the strings for each interaction
    A = "";
    std::stringstream BC;     // +++ how to clear this before each loop iteration
    D = "";
    ABCD = "";

    // Convert A, BC, D into strings and join together.
    A = intToString(pathCode[i][0]);
    BC << std::setw(2) << std::setfill('0') << pathCode[i][1];
    D = intToString(pathCode[i][2]);
    ABCD.append(A);
    ABCD.append(BC.str());
    ABCD.append(D);
    pathCodeStr[i] = ABCD;

  }

  // now combine all the path code strings into one string
  // again, we're starting at index 1
  for (int iInteraction = 1 ; iInteraction < numInteractionToRecord ; ++iInteraction) {
    // append pathCodestring to end of combinedpathCodestring, but only up to 
    // how many actual interactions there were, or up to numInteractionToRecord
    if (iInteraction <= numInteractions) {
      combinedPathCodeString.append(pathCodeStr[iInteraction]);
    }
  }
  
} // end createCombinedPathCodeString()


/******************************************************************************/


void interceptXRTObjects(const PhotonBoundingBox & photonBBox, 
                         int idxLow, 
                         int numXRTObjects, 
                         std::vector<long> zmaxsortxrtobjectindex,
                         const std::vector<XRTObject> & XRTObjects,
                         long excludedID,
                         bool doRadialRejection,
                         
                         int & newIdxLow, 
                         int & numCandidates,
                         std::vector<int> & candidateList) {
  
  // The intention of doRadialRejection is that we may need to experiment with 
  // changing radialrejection to "False" to see which option gives a faster 
  // run-time since there is a trade-off meaning it is not obvious which is 
  // better. So eventually when we have established which is faster we could 
  // get rid of the variable all together.  
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int currObjIdx = 0;                       // for looping through objects
  
  int candidateCtr = 0;                     // Candidate counter
  
  // grab these doubles so photonBBox isn't accessed inside loop
  double xMinPhoton = photonBBox.m_xMin;
  double xMaxPhoton = photonBBox.m_xMax;
  double yMinPhoton = photonBBox.m_yMin;
  double yMaxPhoton = photonBBox.m_yMax;
  double zMinPhoton = photonBBox.m_zMin;
  double zMaxPhoton = photonBBox.m_zMax;
  double rMinPhoton = photonBBox.m_rMin;
  double rMaxPhoton = photonBBox.m_rMax;
  
  
  // initialize output variables:
  int numMaxCandidates = numXRTObjects - idxLow;
  candidateList.clear();
  candidateList.resize(numMaxCandidates);
  
  // -------------------------------------
  
  AH_DEBUG << "idxLow = " << idxLow << std::endl;
  
  // Loop of xrt objects in the order of the array sorted on the maximum 
  // z-coordinate of an object (descending) and stop the first time that the 
  // lowest z-coordinate of the photon bounding box cannot reach the highest 
  // z-coordinate of the xrt object.
  for (int iObj = idxLow ; iObj < numXRTObjects ; iObj++) {
    
    currObjIdx = zmaxsortxrtobjectindex[iObj];
    const XRTObject & currObject = XRTObjects[currObjIdx];
        
    if (currObjIdx == 0 || currObjIdx == 1 || currObjIdx == 500) {
      AH_DEBUG << "currObjIdx = " << currObjIdx << std::endl;
      AH_DEBUG << "BBoxTrnsfrmd = low rad" << 
                  currObject.m_lowerRadialBound << " up rad " << 
                  currObject.m_upperRadialBound <<  " xmin " << 
                  currObject.m_BBoxTrnsfrmd.m_xMin <<  " xmax " << 
                  currObject.m_BBoxTrnsfrmd.m_xMax <<  " ymin " << 
                  currObject.m_BBoxTrnsfrmd.m_yMin <<  " ymax " << 
                  currObject.m_BBoxTrnsfrmd.m_yMax << std::endl;
      AH_DEBUG << "BBox = low rad" << 
                  currObject.m_lowerRadialBound << " up rad " << 
                  currObject.m_upperRadialBound <<  " xmin " << 
                  currObject.m_BBox.m_xMin <<  " xmax " << 
                  currObject.m_BBox.m_xMax <<  " ymin " << 
                  currObject.m_BBox.m_yMin <<  " ymax " << 
                  currObject.m_BBox.m_yMax << std::endl;
    }
    
    // If we go below the z-coord of the next object, we're at end of eligible 
    // candidates and can break out of loop.
    // We need >= not just > to ensure that objects with ZMAX exactly equal to 
    // the ZMIN of the photon bounding box are not done in this z-interval but 
    // will be done in the next one (otherwise they could be missed)
    // +++ Update 20151018: For the moment we are omitting the following block of     
    //     code completely because there is still a problem with objects that
    //     span more than one z-level interval when there are misalignments,
    //     under certain circumstances. This change will slow down the code
    //     significantly in non-fast mode but the accuracy of finding 
    //	   ray-object intercepts is not compromised. There may be a more 
    //     efficient solution that does not result in such a large change in
    // 	   code speed but this is deferred to a future date. 
    // +++ Update 20151207: The if-block and break are found to be critical,
    //     but the newIdxLow assignment is still not needed
    if (zMinPhoton >= currObject.m_BBoxTrnsfrmd.m_zMax) {
//      newIdxLow = iObj;
      AH_DEBUG << std::endl;
      break;
    }

    // Reject objects with id equal to "excludedID", 
    //   or objects that are switched off with negative values of xrtobjectset, 
    //   or objects whose lowest z-coordinate is higher than the highest 
    //      z-coordinate of the photon bounding box
    //   or if the object is of type foil only accept objects that lie inside 
    //      the radial bounds of the photon box
    //   or objects with x,y,z out of bounds
    
    if ( (excludedID != currObjIdx) && 
         (currObject.m_set >= 0) && 
         (zMaxPhoton >= currObject.m_BBoxTrnsfrmd.m_zMin) ) {
      
      // now decide if it's a foil or obstruction.  foils may need radial 
      // bounds checking
      if ( (currObject.m_type == FOIL) ) {
        
        if ( doRadialRejection && 
             (rMaxPhoton >= currObject.m_lowerRadialBound) && 
             (rMinPhoton <= currObject.m_upperRadialBound) && 
             (xMaxPhoton >= currObject.m_BBoxTrnsfrmd.m_xMin) && 
             (xMinPhoton <= currObject.m_BBoxTrnsfrmd.m_xMax) && 
             (yMaxPhoton >= currObject.m_BBoxTrnsfrmd.m_yMin) && 
             (yMinPhoton <= currObject.m_BBoxTrnsfrmd.m_yMax) ) {
          
          // Now the XRT object is accepted as a candidate for a potential 
          // interaction with the photon.
          candidateList[candidateCtr] = currObjIdx;
          candidateCtr++;
          
        } else if ( !doRadialRejection && 
             (xMaxPhoton >= currObject.m_BBoxTrnsfrmd.m_xMin) && 
             (xMinPhoton <= currObject.m_BBoxTrnsfrmd.m_xMax) && 
             (yMaxPhoton >= currObject.m_BBoxTrnsfrmd.m_yMin) && 
             (yMinPhoton <= currObject.m_BBoxTrnsfrmd.m_yMax) ) {
          
          // Now the XRT object is accepted as a candidate for a potential 
          // interaction with the photon.
          candidateList[candidateCtr] = currObjIdx;
          candidateCtr++;
        
        }
        
      } else {
        // obstuctions: don't check radial bounds
        
        if ( (xMaxPhoton >= currObject.m_BBoxTrnsfrmd.m_xMin) && 
             (xMinPhoton <= currObject.m_BBoxTrnsfrmd.m_xMax) && 
             (yMaxPhoton >= currObject.m_BBoxTrnsfrmd.m_yMin) && 
             (yMinPhoton <= currObject.m_BBoxTrnsfrmd.m_yMax) ) {
          
          // Now the XRT object is accepted as a candidate for a potential 
          // interaction with the photon.
          candidateList[candidateCtr] = currObjIdx;
          candidateCtr++;
          
        }
        
      } // end-if FOIL or OBSTRUCTION
      
    }
    
  } // end for-loop through sorted object list
    
  numCandidates = candidateCtr;

} // end interceptXRTObjects()


/******************************************************************************/


void getImpactCandidates(const PhotonBoundingBox & photonBBox, 
                         int numXRTObjects, 
                         const std::vector<XRTObject> & XRTObjects,
                         long excludedID,
                         const std::vector<int> & obstructionIntervalLowIndex, 
                         const std::vector<int> & obstructionIntervalHighIndex, 
                         long zGroupPointer, 
                         int numLoopSectors,
                         const std::vector<int> & loopSectorNumbers, 
                         int lowShell, 
                         int highShell, 
                         const SectorsShells & sectorsShells, 
                         int & numCandidates,
                         std::vector<int> & candidateList) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
//  candidateList.clear();
//  candidateList.resize(numXRTObjects);
  // +++ is it better to allocate 19000 ints every time this function is called, or to use push_back later?
  //     or use a temp vector, and then just copy .size() elements into candidateList?
  
  // +++ 20150109 try just filling with zeros.  I initialized size in raytraceonephoton to be numXRTObjects
  std::fill(candidateList.begin(), candidateList.end(), 0);
  
  
  int candidateCtr = 0;
  
  // create local vars for each photonBBox member, for speed inside the loops
  double photonXMin = photonBBox.m_xMin;
  double photonXMax = photonBBox.m_xMax;
  double photonYMin = photonBBox.m_yMin;
  double photonYMax = photonBBox.m_yMax;
  double photonZMin = photonBBox.m_zMin;
  double photonZMax = photonBBox.m_zMax;
  
  int obsIndexLow  = obstructionIntervalLowIndex[zGroupPointer];
  int obsIndexHigh = obstructionIntervalHighIndex[zGroupPointer];
  
  // -------------------------------------
  
  AH_DEBUG << std::endl;
  
  // First loop over relevant obstructions, using zGroupPointer to exclude the group of obstructions that cannot be impacted:
  // zGroupPointer = 0 means all obstructions can be impacted
  // zGroupPointer = 1 means a primary mirror has been impacted so 1st group of obstructions cannot be impacted;
  // zGroupPointer = 2 means a secondary mirror has been impacted so 1st and 2nd group of obstructions cannot be impacted
  
  AH_DEBUG << "zGroupPointer = " << zGroupPointer << std::endl;
  AH_DEBUG << "obsIndexLow = " << obsIndexLow << std::endl;
  AH_DEBUG << "obsIndexHigh = " << obsIndexHigh << std::endl;
  
  // only check for an obstruction interval if there actually are obstructions 
  // in that interval.  If there are, then we updated 
  // obstructionIntervalLowIndex from it's default value of -1
  if (obsIndexLow >= 0) {
    for (int iObject = obsIndexLow ; iObject <= obsIndexHigh ; ++iObject) {
      // +++ this gave a segfault on Tahir's mac when obsIndexLow = -1.  why didn't mine fail????
//      AH_DEBUG << "iObject = " << iObject << std::endl;
//      AH_DEBUG << "XRTObjects[iObject].m_set = " << XRTObjects[iObject].m_set << std::endl;
      const XRTObject & currObject = XRTObjects[iObject];
      // +++ make a reference for the currObject.m_BBoxTrnsfrmd?

      // Reject objects with id equal to "excludedid", 
      //   or objects that are switched off with negative values of xrtobjectset, 
      //   or objects whose lowest z-coordinate is higher than the highest z-coordinate of the photon bounding box
      // xmax(photon)>=xmin(xrtobject) & xmin(photon)<=xmax(xrtobject)
      // ymax(photon)>=ymin(xrtobject) & ymin(photon)<=ymax(xrtobject)
      if ( (excludedID != iObject) && 
          (currObject.m_set >= 0) && 
          (photonZMax >= currObject.m_BBoxTrnsfrmd.m_zMin) &&
          (photonZMin <= currObject.m_BBoxTrnsfrmd.m_zMax) && 
          (photonXMax >= currObject.m_BBoxTrnsfrmd.m_xMin) && 
          (photonXMin <= currObject.m_BBoxTrnsfrmd.m_xMax) && 
          (photonYMax >= currObject.m_BBoxTrnsfrmd.m_yMin) && 
          (photonYMin <= currObject.m_BBoxTrnsfrmd.m_yMax) ) {

        // Now the XRT object is accepted as a candidate for a potential 
        // interaction with the photon.
        candidateList[candidateCtr] = iObject;
        candidateCtr++;

      }

    } // end-for through obstructions
  } // end-if that there are obstructions in this interval
  
  AH_DEBUG << std::endl;
  
  // Now deal with the foils
  // Before calling getimpactcandidates(), the sector and shell range to loop 
  // over (surrounding the photon initial position) was pre-calculated.

  for (int iHousing = zGroupPointer ; iHousing < s_numHousings ; ++iHousing) {
    for (int iSector = 0 ; iSector < numLoopSectors ; ++iSector) {
      int currSector = loopSectorNumbers[iSector];
      for (int iShell = lowShell ; iShell <= highShell ; ++iShell) {
        
        int currObjectID = sectorsShells.m_setsectorshell2xrtobjectid[iHousing][currSector-1][iShell-1];
        const XRTObject & currObject = XRTObjects[currObjectID];
        
        // xmax(photon)>=xmin(xrtobject) & xmin(photon)<=xmax(xrtobject)
        // ymax(photon)>=ymin(xrtobject) & ymin(photon)<=ymax(xrtobject)
        if ( (excludedID != currObjectID) && 
             (currObject.m_set >= 0) && 
             (photonZMax >= currObject.m_BBoxTrnsfrmd.m_zMin) && 
             (photonZMin <= currObject.m_BBoxTrnsfrmd.m_zMax) &&
             (photonXMax >= currObject.m_BBoxTrnsfrmd.m_xMin) && 
             (photonXMin <= currObject.m_BBoxTrnsfrmd.m_xMax) && 
             (photonYMax >= currObject.m_BBoxTrnsfrmd.m_yMin) && 
             (photonYMin <= currObject.m_BBoxTrnsfrmd.m_yMax) ) {
        
          // Now the XRT object is accepted as a candidate for a potential 
          // interaction with the photon.
          candidateList[candidateCtr] = currObjectID;
          candidateCtr++;
          
        }
      }
    }
  } // end-for dealing with foils
  
  AH_DEBUG << std::endl;
  
  numCandidates = candidateCtr;
  
} // end getImpactCandidates()


/******************************************************************************/


void rayConeIntercept(const CartesianCoord & coordOrig, 
                      const DirectionVector & dirVecOrig,
                      const CartesianCoord & coordTran,
                      const DirectionVector & dirVecTran,
                      double kc, 
                      double kcSq, 
                      int & phInitPos, 
                      int & rayConeResult, 
                      double & tImpact, 
                      CartesianCoord & coordImpactOrig, 
                      CartesianCoord & coordImpactTran) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // The critical value of ac/b^2 in the quadratic equation used to determine 
  // whether to switch to the linear approximation solution in order to avoid 
  // a divergence when the ray is parallel to the surface.
  double criticalRatio = 1.e-8;
  
  // transformed derived quantities
  double vxtsq = dirVecTran.m_xDir * dirVecTran.m_xDir;
  double vytsq = dirVecTran.m_yDir * dirVecTran.m_yDir;
  double vztsq = dirVecTran.m_zDir * dirVecTran.m_zDir;
  double xptsq = coordTran.m_x * coordTran.m_x;
  double yptsq = coordTran.m_y * coordTran.m_y;
  double zptsq = coordTran.m_z * coordTran.m_z;
  double rpointsq = xptsq + yptsq;
  double fac1 = (kcSq * dirVecTran.m_xDir * coordTran.m_x) + 
                (kcSq * dirVecTran.m_yDir * coordTran.m_y) - 
                       (dirVecTran.m_zDir * coordTran.m_z);
  double fac2 = kcSq * (vxtsq + vytsq) - vztsq;
  double fac3 = (kcSq * rpointsq) - zptsq;
  double fac23 = fac2 * fac3;
  double fac1sq = fac1 * fac1;
  double discri = fac1sq - fac23;
  double sqrtDiscri = 0.0;
    
  // to calculate solutions for the time lapse for impact
  //+++ 20140428 KLR I can rename these variables to be cleaner
  double a = fac2;
  double b = fac1;
  double c = fac3;
  double ac = fac23;
  double aa = a;
  
  double t1 = 0.0;
  double t2 = 0.0;
  double zstd1 = 0.0;
  double zstd2 = 0.0;
  
  // +++ what are phInitPos and rayConeResults?
  
  // -------------------------------------
  
  // return immediately if the discriminator is negative
  if (discri < 0.0) {
    rayConeResult = -1;
    return;
  }
  
  // The quantity fac3 is effectively (k^2 r^2 - z^2)
  if (fac3 > 0.0) {
    // photon initial position is outside cone
    rayConeResult = 1;
    phInitPos = 1;
  } else if (fac3 < 0.0) {
    // photon initial position is inside cone
    rayConeResult = 0;
    phInitPos = 0;
  } else {
    // fac3 == 0.0 means photon initial position is on cone surface
    // NOTE: this situation should not normally occur and if it does 
    // self-interaction is disallowed later on anyway
    rayConeResult = 4;
    phInitPos = 2;
    tImpact = 0.0;
    coordImpactOrig.m_x = coordOrig.m_x;
    coordImpactOrig.m_y = coordOrig.m_y;
    coordImpactOrig.m_z = coordOrig.m_z;
    coordImpactTran.m_x = coordTran.m_x;
    coordImpactTran.m_y = coordTran.m_y;
    coordImpactTran.m_z = coordTran.m_z;
    return;
  }
  
  // Determine if we will need to switch to linear approximation for solution 
  // if denominator in quadratic solution equation is approaching zero
  if (std::abs(ac) < (criticalRatio*fac1sq)) {
    
    t1 = -0.50 * c / b;
    // (second solution is infinite)
    if (t1 < 0.0) {
      rayConeResult=-4;
      // ray is parallel or nearly parallel: no solution in direction of motion
      return;
    }
    
    // If time is positive we have to reject interception of the ghost cone, so 
    // evaluate the z-coordinate of the impact
    zstd1 = coordTran.m_z + (dirVecTran.m_zDir * t1);
    if (zstd1 < 0.0) {
      // rRay is parallel or nearly parallel to the cone surface but the only 
      // solution is interception of the ghost cone.
      rayConeResult = -5;
      return;
    }
    
    // We have arrived at a valid solution for the time to impact, in the 
    // linear approximation mode
    rayConeResult++;
    tImpact = t1;
    coordImpactTran.m_z = zstd1;
    
  } else {
    // else use regular quadratic solutions
    
    // No need to test whether the denominator is 0. this was already checked 
    // with the previous criteria
    sqrtDiscri = std::sqrt(discri);
    t1 = (sqrtDiscri - b) / aa;
    t2 = (sqrtDiscri + b) / (-1.0 * aa);
    // The following will be needed later to reject intersections with the ghost cone
    // They are the z impact coordinates in standard position
    zstd1 = coordTran.m_z + (dirVecTran.m_zDir * t1);
    zstd2 = coordTran.m_z + (dirVecTran.m_zDir * t2);
    // Impact in the direction of motion is possible only for positive t. The smallest value is not necessarily the solution we want because the cone equation actually describes two cones sharing an apex. For the purpose of modeling telescope foils we want the upright cone which has, in standard position, only positive z values.
    
    if ((t1 < 0.0) && (t2 < 0.0)) {
      // no impact possible in the direction of motion
      rayConeResult=-2;
      return;
    }
      
    else if ((t1 >= 0.0) && (t2 >= 0.0)) {

      if ((zstd1 >= 0.0) && (zstd2 >= 0.0)) {
        if (t1 <= t2) {
          tImpact = t1;
          coordImpactTran.m_z = zstd1;
        } else if (t2 < t1) {
          tImpact = t2;
          coordImpactTran.m_z = zstd2;
        }
      } else if ((zstd1 > 0.0) && (zstd2 < 0.0)) {
        tImpact = t1;
        coordImpactTran.m_z = zstd1;
      } else if ((zstd2 > 0.0) && (zstd1 < 0.0)) {
        tImpact = t2;
        coordImpactTran.m_z = zstd2;
      } else if ((zstd1 < 0.0) && (zstd2 < 0.0)) {
        // Both solutions are for the "ghost" cone: effectively there are no solutions.
        tImpact = std::min(t1, t2);
        coordImpactTran.m_z = coordTran.m_z + (dirVecTran.m_zDir * tImpact);
        rayConeResult = -3;
      }
    
    } else if ( (t1 < 0.0) && (t2 > 0.0) ) {
      tImpact = t2;
      coordImpactTran.m_z = zstd2;
    } else if ( (t1 > 0.0) && (t2 < 0.0) ) {
      tImpact = t1;
      coordImpactTran.m_z = zstd1;
    }
  
  } // end if-block to determine whether to use quadratic or linear approx.
  
  // if there was a valid solution for tImpact:
  if (rayConeResult >= 0) {
    
    // Calculate the impact coordinates by using the fact that tImpact is 
    // frame-independent, applying it to the untransformed photon quantities. 
    // impact coordinates in the XRT frame:
    coordImpactOrig.m_x = coordOrig.m_x + (dirVecOrig.m_xDir * tImpact);
    coordImpactOrig.m_y = coordOrig.m_y + (dirVecOrig.m_yDir * tImpact);
    coordImpactOrig.m_z = coordOrig.m_z + (dirVecOrig.m_zDir * tImpact);
    
    // Also calculate the impact coordinates in the transformed frame in which 
    // the cone is in the standard position. In order to calculate the 
    // coordinates with the cone in the exact position it is in the mirror 
    // file, add the z-shift offset zb-(kc*r0). OR simply pre-calculate the 
    // bottoms and tops of the mirrors for the cone in standard position.
    coordImpactTran.m_x = coordTran.m_x + (dirVecTran.m_xDir * tImpact);
    coordImpactTran.m_y = coordTran.m_y + (dirVecTran.m_yDir * tImpact);
    // coordImpactTran.m_z already calculated
    
  }
  
} // end rayConeIntercept()


/******************************************************************************/


void rayCylinderIntercept(const CartesianCoord & coordOrig, 
                          const DirectionVector & dirVecOrig,
                          const CartesianCoord & coordTran,
                          const DirectionVector & dirVecTran,
                          double r0Sq, 
                          int & phInitPos, 
                          int & rayCylResult, 
                          double & tImpact, 
                          CartesianCoord & coordImpactOrig, 
                          CartesianCoord & coordImpactStd) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // transformed derived quantities
  double vxtsq = dirVecTran.m_xDir * dirVecTran.m_xDir;
  double vytsq = dirVecTran.m_yDir * dirVecTran.m_yDir;
  double xptsq = coordTran.m_x * coordTran.m_x;
  double yptsq = coordTran.m_y * coordTran.m_y;
  double vxxpt = dirVecTran.m_xDir * coordTran.m_x;
  double vyypt = dirVecTran.m_yDir * coordTran.m_y;
  double vxtsqplusvytsq = vxtsq + vytsq;
  double vxxptplusvyypt = vxxpt + vyypt;
  double rpointsq = xptsq + yptsq;
  double discri = (r0Sq * vxtsqplusvytsq) + (2.0 * vxxpt * vyypt) - 
                  (vytsq * xptsq) - (vxtsq * yptsq);
  double sqrtDiscri = 0.0;
  
  double t1 = 0.0;
  double t2 = 0.0;
  double zstd1 = 0.0;
  double zstd2 = 0.0;

  // -------------------------------------
  
  AH_DEBUG << "** top of rayCylinderIntercept" << std::endl;
  
  AH_DEBUG << "coordTran = " << coordTran << std::endl;
  
  if (rpointsq > r0Sq) { 
    rayCylResult = 1;
    phInitPos = 1;
  } else if (rpointsq < r0Sq) { 
    rayCylResult = 0;
    phInitPos = 0;
  } else {
    // (rpointsq == r0Sq) 
    // Note: the following should never happen - if it is does an error 
    // condition should be inserted here
    rayCylResult = 4;
    phInitPos = 2;
    tImpact = 0.0;
    coordImpactOrig.m_x = coordOrig.m_x;
    coordImpactOrig.m_y = coordOrig.m_y;
    coordImpactOrig.m_z = coordOrig.m_z;
    coordImpactStd.m_x = coordTran.m_x;
    coordImpactStd.m_y = coordTran.m_y;
    coordImpactStd.m_z = coordTran.m_z;
    return;
  }
  
  // calculate solutions for the time lapse for impact; return immediately if 
  // the discriminator is negative (no solutions)
  if (discri < 0.0) {
    rayCylResult = -1;
    return;
  }
  
  AH_DEBUG << "disc is >= 0" << std::endl;
  
  // Impact is not impossible
  if (vxtsqplusvytsq <= 0.0) { 
    // Ray is parallel to surface and no solution in direction of motion.
    rayCylResult = -4;
    return;
  }
  
  AH_DEBUG << "impact is possible" << std::endl;

  sqrtDiscri = std::sqrt(discri);
  t1 = -1.0 * (vxxptplusvyypt + sqrtDiscri) / vxtsqplusvytsq;
  t2 = -1.0 * (vxxptplusvyypt - sqrtDiscri) / vxtsqplusvytsq;
  // The following will be needed later to reject intersections with the ghost 
  // cylinder (the portion below z=0)
  zstd1 = coordTran.m_z + (dirVecTran.m_zDir * t1);
  zstd2 = coordTran.m_z + (dirVecTran.m_zDir * t2);
  
  AH_DEBUG << "t1 = " << t1 << std::endl;
  AH_DEBUG << "t2 = " << t2 << std::endl;
  AH_DEBUG << "zstd1 = " << zstd1 << std::endl;
  AH_DEBUG << "zstd2 = " << zstd2 << std::endl;
  
  // Impact in the direction of motion is possible only for positive t.
  // The smallest value is not necessarily the solution we want.
  // For the purpose of modeling telescope foils we want only positive z values.
  if ( (t1 < 0.0) && (t2 < 0.0) ) {
    // impact not possible in the direction of motion
    rayCylResult = -2;
    return;
    
  } else if ( (t1 >= 0.0) && (t2 >= 0.0) ) {
          
    if ( (zstd1 >= 0.0) && (zstd2 >= 0.0) ) {
      tImpact = std::min(t1, t2);
      coordImpactStd.m_z = ( (t1 <= t2) ? zstd1 : zstd2 );
    } else if ( (zstd1 > 0.0) && (zstd2 < 0.0) ) {
      tImpact = t1;
      coordImpactStd.m_z = zstd1;
    } else if ( (zstd1 < 0.0) && (zstd2 > 0.0) ) {
      tImpact = t2;
      coordImpactStd.m_z = zstd2;
    } else if ( (zstd1 < 0.0) && (zstd2 < 0.0) ) {
      // both solutions are for the ghost cylinder so effectively there are 
      // no solutions
      tImpact = std::min(t1, t2);
      coordImpactStd.m_z = coordTran.m_z + (dirVecTran.m_zDir * tImpact);
      rayCylResult = -3;
    }
          
  // If there is only one solution choose the one with positive impact time
  } else if ( (t1 < 0.0) && (t2 > 0.0) ) {
    tImpact = t2;
    coordImpactStd.m_z = zstd2;
          
  } else if ( (t1 > 0.0) && (t2 < 0.0) ) {
    tImpact = t1;
    coordImpactStd.m_z = zstd1;
  }
  
  AH_DEBUG << "tImpact = " << tImpact << std::endl;
  AH_DEBUG << "coordImpactStd.m_z = " << coordImpactStd.m_z << std::endl;
  AH_DEBUG << "rayCylResult = " << rayCylResult << std::endl;
  
  if (rayCylResult >= 0) {
    
    // This point is reached only if there was a valid solution for tImpact.
    // Calculate the impact coordinates by using the fact that tImpact is 
    // frame-independent, applying it to the untransformed photon quantities. 
    
    // impact coordinates in the XRT frame:
    coordImpactOrig.m_x = coordOrig.m_x + (dirVecOrig.m_xDir * tImpact);
    coordImpactOrig.m_y = coordOrig.m_y + (dirVecOrig.m_yDir * tImpact);
    coordImpactOrig.m_z = coordOrig.m_z + (dirVecOrig.m_zDir * tImpact);
    
    // impact coordinates in the transformed frame:
    coordImpactStd.m_x = coordTran.m_x + (dirVecTran.m_xDir * tImpact);
    coordImpactStd.m_y = coordTran.m_y + (dirVecTran.m_yDir * tImpact);
    // coordImpactStd.m_z already calculated
    
  }
  
} // end rayCylinderIntercept()


/******************************************************************************/


bool isPointInsidePolygon(const CartesianCoord & point, 
                          int numVertices,
                          const std::vector<CartesianCoord> & vertices) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int edgeCrossings = 0;
  double nudgeFactor = 0.0;
  double critFactor = 0;
  double xIntercept = 0.0;
  double yProduct = 0.0;
  std::vector<CartesianCoord> newVertices(numVertices);
  
  // -------------------------------------

  
  // Transform the vertices so that the input point is at the origin
  // Check if the test line from point will intercept one of the vertex 
  // y values and also test if the point lies on origin
  for (int i = 0 ; i < numVertices ; ++i) {
    
    newVertices[i].m_x = vertices[i].m_x - point.m_x;
    newVertices[i].m_y = vertices[i].m_y - point.m_y;
    
    //if the vertex lies on the origin the point is inside the polygon
    if ((newVertices[i].m_x == 0.0) && (newVertices[i].m_y == 0.0)) {
      // Point lies on a vertex
      return true;
    }
    
    /* Method: We check if a line drawn from the point crosses edges and how
    many. For the transformed polygon that means effectively the x-axis. If this line
    passes through one or more vertices there is a problem because two edges
    converge on to a single point and are then counted as one giving the wrong
    result. The method to overcome this is to nudge the vertex away from the line
    by a small amount because it will then give the right result. */
    if ((newVertices[i].m_y == 0.0) && (nudgeFactor == 0.0)) {
      if (point.m_y == 0.0) {   // +++ is this an example where == doesn't work for doubles?
        nudgeFactor = 0.01;
      } else {
        nudgeFactor = 0.01 * point.m_y;
      }
    }
  }
  
  // Transform the y-coordinates of the vertices, applying the nudge factor
  if (nudgeFactor > 0.0) {
    for (int i = 0 ; i < numVertices ; ++i) {
      newVertices[i].m_y += nudgeFactor;
    }
  }
  
  for (int i = 1 ; i < numVertices ; ++i) {
    // Does this vertex lie to the right of the y-axis and does the edge 
    // formed with the previous vertex cross y=0? The following basically 
    // demands that there is an intercept on the x-axis between two points 
    // and that the intercept should have x>0
    critFactor = newVertices[i-1].m_y * 
                  (newVertices[i].m_x - newVertices[i-1].m_x) / 
                  (newVertices[i].m_y - newVertices[i-1].m_y);
    xIntercept = newVertices[i-1].m_x - critFactor;
    yProduct = newVertices[i].m_y * newVertices[i-1].m_y;
    if ((newVertices[i-1].m_x > critFactor) && (yProduct <= 0.0)) {
      edgeCrossings++;
    }
  }
  
  // The loop did not do the last edge (last vertex joining 1st vertex)/ 
  // so do it now
  critFactor = newVertices[0].m_y * 
               (newVertices[numVertices-1].m_x - newVertices[0].m_x) / 
               (newVertices[numVertices-1].m_y - newVertices[0].m_y);
  xIntercept = newVertices[0].m_y - critFactor;
  yProduct = newVertices[numVertices-1].m_y * newVertices[0].m_y;
  if ((newVertices[0].m_x > critFactor) && (yProduct <= 0.0)) {
    edgeCrossings++;
  }
  
  if ( (edgeCrossings % 2) == 0) {
    // even number of edge crossings, so it's not inside
    return false;
  } else {
    return true;
  }

} // end isPointInsidePolygon()


/******************************************************************************/


void housingImpactPositions(const std::vector<HousingGeometry> & housings,
                            const CartesianCoord & coordInit,
                            const DirectionVector & dirVecInit,
                            int & impactResult, int & innerOuter,
                            double & tImpact, CartesianCoord & coordImpact) {
  //+++ rename, similar to getPhotonObjectImpactCoords?
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  HousingGeometry currHousing;    // for looping through housings
  CartesianCoord coordTran;
  DirectionVector dirVecTran;
  CartesianCoord coordCross;
  double tCross = 0.0;            // input to getXYForNewZ, not used anywhere
  
  // inputs to rayCylinderIntercept
  double r0Sq = 0.0;
  int phInitPos = 0;
  int rayCylinderResult = 0;
  CartesianCoord coordImpactTran; // input, but not used anywhere
  
  double radialDistance = 0.0;
  
  // how many housings (precol, primary, secondary, etc)
  int numHousingUnits = housings.size();
  
  // initialize output variables
  impactResult = -1;
  innerOuter = -1;
  
  // -------------------------------------
  
  
  // if the ray is parallel to the z-axis, there cannot be any impact with any 
  // of the housing units
  if ((dirVecInit.m_xDir == 0.0) && (dirVecInit.m_yDir == 0.0)) {
    return;
  }
  
  // loop through layers (pre-collimator, primary mirror, etc) of the telescope 
  // structure (each layer containing an inner and outer housing unit)
  for (int i = 0 ; i < numHousingUnits ; ++i) {
    //+++ why do we need three housing elements, even if no precol? (I wrote that down in a note)
    //+++ Tahir is looking into that
    
    currHousing = housings[i];
    
    // set up a new coordinate for this housing
    coordCross.m_x = 0.0;
    coordCross.m_y = 0.0;
    coordCross.m_z = currHousing.m_zLower;
    
    // if the lowest z-coordinate of the housing unit is higher than the photon 
    // initial z-coordinate, skip this housing unit
    if (coordCross.m_z > coordInit.m_z) {
      continue;
    }
    
    // transform the photon position so that the origin of the coordinate axes 
    // is at the center of the housing unit.
    coordTran.m_x = coordInit.m_x - currHousing.m_xShift;
    coordTran.m_y = coordInit.m_y - currHousing.m_yShift;
    coordTran.m_z = coordInit.m_z;
    dirVecTran = dirVecInit;      // even though we aren't transforming the 
                                  // direction vector, still copy it into a new
                                  // var that says "Tran" so that code is clear
                                  // when we call rayCylinderIntercept() later
    
    // project the ray to the z-plane that corresponds to the bottom of the 
    // housing unit and find the x,y coordinates of interception on that plane
    getXYForNewZ(coordTran, dirVecInit, coordCross, tCross);
    
    // calculate the radial distance of the intercept point from the origin and 
    // check whether it is >= the inner housing unit radius
    radialDistance = std::sqrt( (coordCross.m_x * coordCross.m_x) + 
                                (coordCross.m_y * coordCross.m_y) );
    if (radialDistance <= currHousing.m_rInner) {
      // inner housing impact can occur
      // Set r0 equal to the inner housing radius if the projected ray lies 
      // inside the inner radius on the z=zbottom plane
      r0Sq = currHousing.m_rInnerSq;
      innerOuter = 0;
    } else if (radialDistance >= currHousing.m_rOuter) {
      // Set r0 equal to the outer housing radius if the projected ray lies 
      // outside the outer radius on the z=zbottom plane
      r0Sq = currHousing.m_rOuterSq;
      innerOuter = 1;
    }
    
    if (innerOuter >= 0) { 
      // find the actual impact point
      rayCylinderIntercept(coordInit, dirVecInit, coordTran, dirVecTran, 
                           r0Sq, phInitPos, rayCylinderResult, 
                           tImpact, coordImpact, coordImpactTran);
      
      // if an impact, check that zimpact lies within housing dimensions
      if ( (rayCylinderResult >= 0) && 
           (coordImpact.m_z <= currHousing.m_zUpper) && 
           (coordImpact.m_z >= currHousing.m_zLower) ) {
        impactResult = i;
        break;  // we can get out of the for-loop if there was an impact
      }
    }
      
  } // end for-loop through housings
  
  // if no impact, impactResult will remain at -1 and innerOuter = -1

} // end housingImpactPositions()


// FUNCTION NAME: getPhotonObjectImpactCoords
//
// CALLING SEQUENCE:
//   getPhotonObjectImpactCoords(currObjIdx, objectType, surfacegeometry, 
//        has3DObjects, slopes, geoparams, numvertices, vertices, boundingbox, 
//        startAngle, endAngle, startAngleMod, endAngleMod, currPhotonPos, 
//        currPhotonDir, localCoord, localDir, faceHit, exitface, impactcoord, 
//        stdcoord, impactDistance, pathLength, errorCode, killThePhoton); 
//
// PURPOSE:
//   
//
// INPUTS:
//   
//
// OUTPUTS:
//   
//
// CALLED BY:
//   raytraceOnePhoton()
//
// SUBROUTINES:
//   getXYForNewZ(), isPointInsidePolygon(), rayCylinderIntercept(), 
//   rayConeIntercept(), cartesianToPolar()
//
void getPhotonObjectImpactCoords(long objIndex, 
                                 objectTypes_e objectType, 
                                 surfaceGeometryTypes_e surfaceGeometry,
                                 bool treatAs3D, 
                                 const Slopes & slopes,
                                 const std::vector<double> & geoParams, 
                                 int numVertices, 
                                 const std::vector<CartesianCoord> & vertices,
                                 const ObjectBoundingBox & objectBBox, 
                                 double startAngle, 
                                 double endAngle,
                                 double startAngleMod, 
                                 double endAngleMod, 
                                 const CartesianCoord & origCoord,
                                 const DirectionVector & origDirVec,
                                 const CartesianCoord & transformedCoord,
                                 const DirectionVector & transformedDirVec,
                                 int & faceHit, 
                                 int & exitFace, 
                                 CartesianCoord & impactCoordOrig,
                                 CartesianCoord & impactCoordStd,
                                 double & distance, 
                                 double & pathLength,
                                 int & errorCode, 
                                 bool & killThePhoton) {
  
  // +++ in object struct: objectType, surfaceGeometry, and slopes, and geoParams, etc
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // true if the point lies inside the obstruction (i.e. if the support 
  // structure intercepts the photon ray)
  bool pointIsInsidePolygon = false;
  
  CartesianCoord newPolyCoord;
  double timeObsImpact = 0.0;
  
  double topImpactRadiusSq = 0.0;
  double bottomImpactRadiusSq = 0.0;

  CartesianCoord newCoordTop;           // +++ add comment
  double topImpactTime = 0.0;           // +++ add comment
  CartesianCoord newCoordBottom;        // +++ add comment
  double bottomImpactTime = 0.0;        // +++ add comment
    
  CartesianCoord coordTransfStd;        // +++ add comment
  double kc = 0.0;                      // +++ add comment
  double kcSq = 0.0;                    // +++ add comment
  double timeImpactFront = 0.0;         // +++ add comment
  double timeImpactBack = 0.0;          // +++ add comment
  CartesianCoord coordFrontImpactOrig;  // +++ add comment
  CartesianCoord coordFrontImpactStd;   // +++ add comment
  CartesianCoord coordBackImpactOrig;   // +++ add comment
  CartesianCoord coordBackImpactStd;    // +++ add comment
  int photonOutsideFront = 0;
  int photonOutsideBack = 0;
  int rayConeInterceptResultFront = 0;
  int rayConeInterceptResultBack = 0;
  
  // bools to flag whether a certain face was hit or not
  bool topFaceHit = false;
  bool bottomFaceHit = false;
  bool innerFaceHit = false;
  bool outerFaceHit = false;
      
  double foiltopzOuter = 0.0;
  double foiltopzInner = 0.0;
  double foilbotzOuter = 0.0;
  double foilbotzInner = 0.0;
  
  // for finding boundaries of foil
  double radius = 0.0;
  double phi = 0.0;

  std::string errorMsg;
  
  // initialize output params
  errorCode = 0;
  pathLength = -1.0;
  killThePhoton = false;
  
  // -------------------------------------
  
  if (objectType == OBSTRUCTION) {
    
    // +++ this is passed in from raytraceonephoton, which only stores one bool.
    //     but there should be a bool for each object.
    if (treatAs3D) {
      faceHit = -1;
      errorMsg = "Object is a 3-D support structure. ";
      errorMsg.append("Impact with this object is not yet implemented.");
      AH_THROW_RUNTIME(errorMsg);
    }
    
    // Object is a 2-D support structure
    
    // In the 2-D case the z-coords should all be the same. Use the first 
    // one to project the photon ray onto a plane with that z-value and find 
    // the x,y values on that plane. Then check if that point lies inside or 
    // outside the polygon defining the obstruction.
    newPolyCoord.m_z = vertices[0].m_z;
    getXYForNewZ(transformedCoord, transformedDirVec, newPolyCoord, timeObsImpact);
    
    pointIsInsidePolygon = isPointInsidePolygon(newPolyCoord, numVertices, vertices);
    
    if (pointIsInsidePolygon) {
      killThePhoton = true;
      
      if (transformedDirVec.m_zDir > 0.0) {
        // Polygon impacted from the top. 
        // x,y impact coordinates are newPolyCoord
        faceHit = 0;
        exitFace = 1;
      } else if (transformedDirVec.m_zDir <= 0.0) {
        faceHit = 1;
        exitFace = 0;
      }
      
      distance = timeObsImpact;
      impactCoordOrig.m_x = newPolyCoord.m_x;
      impactCoordOrig.m_y = newPolyCoord.m_y;
      impactCoordOrig.m_z = newPolyCoord.m_z;
      impactCoordStd.m_x = impactCoordOrig.m_x;
      impactCoordStd.m_y = impactCoordOrig.m_y;
      impactCoordStd.m_z = impactCoordOrig.m_z;
      
    } else {
      // impact point is not inside obstruction polygon
      faceHit = -1;
      exitFace = -1;
    }
  
  } else if (objectType == FOIL) {
    
    //+++ maybe print this? AH_DEBUG << "Object is foil type" << std::endl;
    
    if ( (surfaceGeometry == CYLINDER) || (surfaceGeometry == CONE)) {
      
      // Check whether there is a potential impact on the top edge of the foil 
      // by projecting the ray to the z-coord of the top of the foil, getting 
      // the new x,y coords, and checking whether they are within the radial 
      // bounds of the foil. We already know that the photon path overlaps the 
      // bounding box of the foil so we don't need to check the angular 
      // position of the potential impact point.
      newCoordTop.m_z = geoParams[0];
      double r_i_tsq = geoParams[11];
      double r_o_tsq = geoParams[12];
      getXYForNewZ(transformedCoord, transformedDirVec, newCoordTop, topImpactTime);
      topImpactRadiusSq = (newCoordTop.m_x * newCoordTop.m_x) + 
                          (newCoordTop.m_y * newCoordTop.m_y);
      
      if ( (topImpactRadiusSq >= r_i_tsq) && 
           (topImpactRadiusSq <= r_o_tsq) && 
           (topImpactTime >= 0.0) ) {
        
        // Ray path intersects top face/edge of foil (potential hit)
        cartesianToPolar(newCoordTop, radius, phi);
        if (endAngle <= s_twopi) {
          if ((phi >= startAngle) && (phi < endAngle)) {
            topFaceHit = true;
          }
        } else {
          // the end angle is > 2pi
          // two cases: (a) startangle is <=2pi and (b) startangle is >2pi
          if (startAngle <= s_twopi) {
            if ( ( (phi >= 0.0) && (phi < endAngleMod) ) ||
                 ( (phi >= startAngle) && (phi < s_twopi) ) ) {
              topFaceHit = true;
            }
          } else {
            // startAngle > 2pi
            if ( (phi >= startAngleMod) and (phi < endAngleMod)) {
              topFaceHit = true;
            }
          }
        }
        
      }
      // Ray path intersects top face/edge of foil if topfaceHit = true
      
      // choose cone or cylinder surface
      if (surfaceGeometry == CYLINDER) {
        
        // geoParams[0] = z coord of top of foil
        foiltopzOuter = geoParams[0];
        foiltopzInner = geoParams[0];
        // geoParams[1] = z coord of bottom of foil
        foilbotzOuter = geoParams[1];
        foilbotzInner = geoParams[1];
        
        // Check whether the ray intercepts the inner and outer cylinders that 
        // define the front and back surfaces of the foils respectively. 
        // rayCylinderIntercept simply finds the intercept of the ray with 
        // entire cylinders (boundaries of foils are imposed afterwards)
        
        coordTransfStd.m_x = transformedCoord.m_x;
        coordTransfStd.m_y = transformedCoord.m_y;
        coordTransfStd.m_z = transformedCoord.m_z;
        
        // First do the cylinder representing the front side of the foil
        rayCylinderIntercept(origCoord, origDirVec, coordTransfStd, 
                             transformedDirVec, r_i_tsq, 
                             photonOutsideFront, rayConeInterceptResultFront, 
                             timeImpactFront, coordFrontImpactOrig, 
                             coordFrontImpactStd);
        
        AH_DEBUG << "front side for cyclinder: rayConeInterceptResultFront = " << rayConeInterceptResultFront << std::endl;
        
        // Then do the back sides
        rayCylinderIntercept(origCoord, origDirVec, coordTransfStd,  
                             transformedDirVec, r_o_tsq, 
                             photonOutsideBack, rayConeInterceptResultBack, 
                             timeImpactBack, coordBackImpactOrig, 
                             coordBackImpactStd);
        AH_DEBUG << "back side for cyclinder: rayConeInterceptResultBack = " << rayConeInterceptResultBack << std::endl;

      } else if (surfaceGeometry == CONE) {
        
        // geoParams[13] = bottom z of outer cone
        foilbotzOuter = geoParams[13];
        // geoParams[14] = bottom z of inner cone
        foilbotzInner = geoParams[14];
        // geoParams[17] = top z of outer cone
        foiltopzOuter = geoParams[17];
        // geoParams[18] = top z of outer cone
        foiltopzInner = geoParams[18];
        
        // Check whether the ray intercepts the inner and outer cones that 
        // define the front and back surfaces of the foils respectively. 
        // rayConeIntercept() simply finds the intercept of the ray with entire 
        // cone (the boundaries of the foils are imposed afterwards). For each 
        // cone we move the photon z position so that effectively each cone is 
        // in "standard position."
        
        coordTransfStd.m_x = transformedCoord.m_x;
        coordTransfStd.m_y = transformedCoord.m_y;
        
        // First do the cone representing the front side of the foil
        coordTransfStd.m_z = transformedCoord.m_z - geoParams[16];
        kc = slopes.m_front;
        kcSq = slopes.m_frontSq;
        rayConeIntercept(origCoord, origDirVec, coordTransfStd,  
                         transformedDirVec, kc, kcSq, photonOutsideFront, 
                         rayConeInterceptResultFront, timeImpactFront, 
                         coordFrontImpactOrig, coordFrontImpactStd);
        
        // Then do the back sides
        coordTransfStd.m_z = transformedCoord.m_z - geoParams[15];
        kc = slopes.m_back;
        kcSq = slopes.m_backSq;
        rayConeIntercept(origCoord, origDirVec, coordTransfStd, 
                         transformedDirVec, kc, kcSq, photonOutsideBack, 
                         rayConeInterceptResultBack, timeImpactBack, 
                         coordBackImpactOrig, coordBackImpactStd);
        
      } // end if-block, CONE or CYLINDER
      
      if ( (rayConeInterceptResultFront < 0) && (rayConeInterceptResultBack < 0) ) {
        if (topFaceHit) {
          // If top face of the foil is hit but the inner and outer cones 
          // cannot be intercepted then the photon must exit from the bottom 
          // of the face or the sides (but the sides may not be real for a 
          // sub-foil). Either way, the photon will be set up to terminate 
          // (the implicit assumption being that the transmission probability 
          // is negligible).
          faceHit = 2;
          exitFace = 5;
          impactCoordStd.m_x = newCoordTop.m_x;
          impactCoordStd.m_y = newCoordTop.m_y;
          impactCoordStd.m_z = newCoordTop.m_z;
          distance = topImpactTime;
          impactCoordOrig.m_x = origCoord.m_x + (origDirVec.m_xDir * topImpactTime);
          impactCoordOrig.m_y = origCoord.m_y + (origDirVec.m_yDir * topImpactTime);
          impactCoordOrig.m_z = origCoord.m_z + (origDirVec.m_zDir * topImpactTime);
          killThePhoton = true;
          // No inner or outer cone intercept possible, top face impacted
          return;
        } else {
          // If neither the top face nor either of the inner or outer cones 
          // can be intercepted, the foil cannot be impacted so return with an 
          // appropriate code
          faceHit = -1;
          exitFace = -1;
          // top face of foil and inner and outer foil cones cannot be impacted
          return;
        }
      }
      
      // If either the inner or outer cone (or both) can be intercepted by the ray, we have to determine whether the front, back, or both faces of the foil can be impacted, taking account of the boundaries of the foil. The action then depends on how many and which faces were hit and in what order.
      
      // We already have a flag that tells us whether the top face can be intercepted; to check if the front or back surface of the foil was impacted we need to checkthe angular and bounds of the foil compared to the impact point. We do not need to explicitly check the angular bounds, only the bounding box boundaries. If only one of the impacts lies inside the boundaries, then the impact was on one of the edges. Note that we check the impact coordinates in the standard position frame: we already know the impact point lies on the surface of the cone, we just need to establish that it lies inside the untransformed bounding box as well. This will avoid costly evaluation of trig functions.
      
      // The variables innerFaceHit and outerFaceHit are set to 1 only if the cone was intercepted and the impact point was within the foil boundaries, so a value of 0 could mean that the photon did not hit the cone at all.
      
      // Note that we need to compare the boundaries of the foils with the impact position in a frame that corresponds to the original mirror file. However we only have the impact coordinates in the XRT frame or in the standard position. The only difference between the mirror file frame and the standard position is the location of the apex. Therefore in geoParams(15)-geoParams(18) we stored the bottoms and tops of the back and front faces of foils, in that order (the standard position is different for the back and front surfaces).
      
      // check for front side impact innerFaceHit
      if ( (rayConeInterceptResultFront >= 0) && 
           (coordFrontImpactStd.m_z <= foiltopzInner) && 
           (coordFrontImpactStd.m_z >= foilbotzInner) && 
           (coordFrontImpactStd.m_x >= objectBBox.m_xMin) && 
           (coordFrontImpactStd.m_x <= objectBBox.m_xMax) && 
           (coordFrontImpactStd.m_y >= objectBBox.m_yMin) && 
           (coordFrontImpactStd.m_y <= objectBBox.m_yMax) ) {
        
        cartesianToPolar(coordFrontImpactStd, radius, phi);
        
        AH_DEBUG << "ray inside foil boundary, front" << std::endl;
        AH_DEBUG << "coordFrontImpactStd = " << coordFrontImpactStd << std::endl;
        AH_DEBUG << "radius = " << radius << std::endl;
        AH_DEBUG << "phi = " << phi << std::endl;
        AH_DEBUG << "endAngle = " << endAngle << " endAngleMod =" << endAngleMod << std::endl;
        AH_DEBUG << "startAngle = " << startAngle << " startAngleMod =" << startAngleMod << std::endl;
        
        if (endAngle <= s_twopi) {
          if ((phi >= startAngle) && (phi < endAngle)) { 
            innerFaceHit = true;
          }
        } else {
          // endAngle is > 2pi
          // Two cases, depending on whether the start angle is <= or > twopi
          if (startAngle <= s_twopi) {
            if ( ( (phi >= 0.0) && (phi < endAngleMod) ) || 
                 ( (phi >= startAngle) && (phi < s_twopi) ) ) {
              innerFaceHit = true;
            }
          } else {
            if ( (phi >= startAngleMod) && (phi < endAngleMod) ) {
              innerFaceHit = true;
            }
          }
        }
      }
      
      // check for back side impact outerFaceHit
      if ( (rayConeInterceptResultBack >= 0) &&
           (coordBackImpactStd.m_z <= foiltopzOuter) && 
           (coordBackImpactStd.m_z >= foilbotzOuter) && 
           (coordBackImpactStd.m_x >= objectBBox.m_xMin) && 
           (coordBackImpactStd.m_x <= objectBBox.m_xMax) && 
           (coordBackImpactStd.m_y >= objectBBox.m_yMin) && 
           (coordBackImpactStd.m_y <= objectBBox.m_yMax) ) {
        
        cartesianToPolar(coordBackImpactStd, radius, phi);
        
        AH_DEBUG << "ray inside foil boundary, back" << std::endl;
        AH_DEBUG << "coordBackImpactStd = " << coordBackImpactStd << std::endl;
        AH_DEBUG << "radius = " << radius << std::endl;
        AH_DEBUG << "phi = " << phi << std::endl;
        AH_DEBUG << "endAngle = " << endAngle << " endAngleMod =" << endAngleMod << std::endl;
        AH_DEBUG << "startAngle = " << startAngle << " startAngleMod =" << startAngleMod << std::endl;
        
        if (endAngle <= s_twopi) {
          if ((phi >= startAngle) && (phi < endAngle)) { 
            outerFaceHit = true;
          }
        } else {
          // endangle >2pi
          // Two cases, depending on whether the start angle is <= 2pi or not
          if (startAngle <= s_twopi) {
            if ( ( (phi >= 0.0) && (phi < endAngleMod) ) || 
                 ( (phi >= startAngle) && (phi < s_twopi) ) ) {
              outerFaceHit = true;
            }
          } else {
            if ( (phi >= startAngleMod) && (phi < endAngleMod) ) {
              outerFaceHit = true;
            }
          }
        }
      }

      // Check whether both front and back foil surfaces were impacted
      if (innerFaceHit && outerFaceHit) {
      
        // If initial position was inside one cone but outside of the other, the photon must have been inside the mirror, which is fatal.
        if (photonOutsideFront != photonOutsideBack) {
          AH_OUT << "Impact position is invalid because the photon started inside the mirror! ** ABORTING this photon **" << std::endl;
          faceHit = -2;
          errorCode = 1;
          // +++ can I return out of this function at this point?
        }
        
        // Note that photon velocity vector is required to be normalized to 1 so impact distance and time to impact are numerically equivalent. In the following we use the time between initial position and impact to determine which face of the foil is hit. Also, the path length for transmission is simply the difference of the impact distances.
        pathLength = std::abs(timeImpactBack-timeImpactFront);
        
        // Both faces intercepted
        if (timeImpactBack < timeImpactFront) {
          // Outer face or back-side is impacted first
          faceHit = 0;
          // Back-side of foil impacted first
          exitFace = 1;
        } else if (timeImpactFront < timeImpactBack) {
          // Inner face or front-side is impacted first
          faceHit = 1;
          // Front-side of foil impacted first
          exitFace = 0;
        } else {
          // (timeImpactFront == timeImpactBack) This should never happen
          // Impact distance to front and back is equal (error!)
          faceHit = -1;
        }

      // If neither front or back or only front or back were impacted, check whether the top or bottom faces were impacted everything in the following } else-block has NOT (innerFaceHit AND outerFaceHit =1)
      } else {
        
        if (topFaceHit) {
          // If the other face was the top..
          if (innerFaceHit) {
            // If the inner cone was hit, it was the front-side, but which hit first?
            if (std::abs(topImpactTime) < std::abs(timeImpactFront)) {
              // top face hit first
              faceHit = 2;
              exitFace = 1;
              // Top face hit first followed by front face
              distance = topImpactTime;
            } else {
              faceHit = 1;
              exitFace = 2;
              AH_OUT << "Front face hit first followed by top face" << std::endl;
              distance = timeImpactFront;
            }
            pathLength = std::abs(std::abs(timeImpactFront) - std::abs(topImpactTime));
            
          } else if (outerFaceHit) {
            // If the outer cone was hit, it was the back-side, but which hit first?
            if (std::abs(topImpactTime) < std::abs(timeImpactBack)) {
              // Top face hit first followed by back face
              faceHit = 2;
              exitFace = 0;
              distance = topImpactTime;
            } else {
              // Back face hit first followed by top face
              faceHit = 0;
              exitFace = 2;
              distance = timeImpactBack;
            }
            pathLength = std::abs(std::abs(timeImpactBack) - std::abs(topImpactTime));
            
          } else {
            // (innerFaceHit=false outerFaceHit=false) 
            // Neither face was impacted but top was impacted
            // +++ NOTE this section of the if-block may be redundant because this scenario has already been considered earlier
            faceHit = 2;
            exitFace = 5;
            impactCoordStd.m_x = newCoordTop.m_x;
            impactCoordStd.m_y = newCoordTop.m_y;
            impactCoordStd.m_z = newCoordTop.m_z;
            distance = topImpactTime;
            impactCoordOrig.m_x = origCoord.m_x + (origDirVec.m_xDir * topImpactTime);
            impactCoordOrig.m_y = origCoord.m_y + (origDirVec.m_yDir * topImpactTime);
            impactCoordOrig.m_z = origCoord.m_z + (origDirVec.m_zDir * topImpactTime);
            killThePhoton = true;
            return;
          }
          
        } else {
          
          // If the topface could not be impacted, 
          // then see if the bottom face can be intercepted
          newCoordBottom.m_z = geoParams[1];
          getXYForNewZ(transformedCoord, transformedDirVec, newCoordBottom, bottomImpactTime);
          bottomImpactRadiusSq = (newCoordBottom.m_x * newCoordBottom.m_x) + 
                                 (newCoordBottom.m_y * newCoordBottom.m_y);
          
          if ((bottomImpactRadiusSq >= geoParams[5]) && 
              (bottomImpactRadiusSq <= geoParams[6]) && 
              (bottomImpactTime > 0.0)) {
            // Ray path intersects bottom face/edge of foil (potential hit)
            cartesianToPolar(newCoordBottom, radius, phi);
            if (endAngle <= s_twopi) {
              if ( (phi >= startAngle) && (phi < endAngle) ) {
                bottomFaceHit = true;
              }
            } else {
              // Two cases depending on whether startangle is <=2pi or not
              if (startAngle <= s_twopi) {
                if ( ( (phi >= 0.0) && (phi < endAngleMod) ) ||
                     ( (phi >= startAngle) && (phi < s_twopi) ) ) {
                  bottomFaceHit = true;
                }
              } else {
                // startangle>2pi
                if ( (phi >= startAngleMod) && (phi < endAngleMod) ) {
                  bottomFaceHit = true;
                }
              }
            }
          }
          
          // If the bottom face was hit
          if (bottomFaceHit) {
            
            // what was the other face that was hit?
            if (innerFaceHit) {
              
              // If inner cone was hit, it was front-side, but which hit first?
              if (std::abs(bottomImpactTime) < std::abs(timeImpactFront)) {
                // Bottom face hit first followed by front face
                faceHit = 3;
                exitFace = 1;
                distance = bottomImpactTime;
              } else {
                // Front face hit first followed by bottom face
                faceHit = 1;
                exitFace = 3;
                distance = timeImpactFront;
              }
              pathLength = std::abs(std::abs(timeImpactFront) - std::abs(bottomImpactTime));
              
            } else if (outerFaceHit) {
              
              // If outer cone was hit, it was back-side, but which hit first?
              if (std::abs(bottomImpactTime) < std::abs(timeImpactBack)) {
                // Bottom face hit first followed by back face
                faceHit = 3;
                exitFace = 0;
                distance = bottomImpactTime;
              } else {
                // Back face hit first followed by bottom face
                faceHit = 0;
                exitFace = 3;
                distance = timeImpactBack;
              }
              pathLength = std::abs(std::abs(timeImpactBack) - std::abs(bottomImpactTime));
              
            } else {
              // neither inner nor outer face was impacted
              faceHit = 3;
              exitFace = 5;
              impactCoordStd.m_x = newCoordBottom.m_x;
              impactCoordStd.m_y = newCoordBottom.m_y;
              impactCoordStd.m_z = newCoordBottom.m_z;
              impactCoordOrig.m_x = origCoord.m_x + (origDirVec.m_xDir * bottomImpactTime);
              impactCoordOrig.m_y = origCoord.m_y + (origDirVec.m_yDir * bottomImpactTime);
              impactCoordOrig.m_z = origCoord.m_z + (origDirVec.m_zDir * bottomImpactTime);
              distance = bottomImpactTime;
              killThePhoton = true;
              return;
            }
            
          } else {
            // ELSE if bottom was not hit: the top was not hit either and only 
            // 0 or 1 front/back faces were hit; if 0 then return with no 
            // impact with anything; if 1 then it means the other entry/exit 
            // point must have been the sides: terminate those photons
            if (!innerFaceHit && !outerFaceHit) {
              faceHit = -1;
              exitFace = -1;
              // None of the 6 faces of the foil were impacted
              return;
            } else {
              if (outerFaceHit) {
                faceHit = 0;
                distance = timeImpactBack;
                impactCoordOrig = coordBackImpactOrig;
                impactCoordStd = coordBackImpactStd;
                
              }
              if (innerFaceHit) {
                faceHit = 1;
                distance = timeImpactFront;
                impactCoordOrig = coordFrontImpactOrig;
                impactCoordStd = coordFrontImpactStd;
              }
              exitFace = 4;
              
              // since the side edges of most of the foils are virtual we only want to terminate the photon for transmission but reflection is still possible. The transmission is already flagged by default "pathLength" being initialized to a negative value and only re-assigned for valid transmission.  so don't flag killThePhoton here.
              
              // One impact on front or back and the other on the sides- photon will be terminated.
              return;
            }
         
          } // end if-block testing if the bottom face of the foil was impacted
          
          // Otherwise the other face must be one of the sides. Whether this was an entry or exit point we terminate the photon path in this case. We do not evaluate the impact time so we do not know which face was hit first.
       
        } // end if-block testing topfaceHit
       
      } // end if-block testing if both front and back faces were hit
      
    } // end if-block specifying geometry of the object (surfaceGeometry)
    
    // Recall that (objectType == FOIL) here
    //+++ check that's correct
    // Set the impact coordinates according to which face of an XRT object of foil type was hit first
    if (faceHit == 0) {
      impactCoordOrig.m_x = coordBackImpactOrig.m_x;
      impactCoordOrig.m_y = coordBackImpactOrig.m_y;
      impactCoordOrig.m_z = coordBackImpactOrig.m_z;
      distance = timeImpactBack;
      impactCoordStd.m_x = coordBackImpactStd.m_x;
      impactCoordStd.m_y = coordBackImpactStd.m_y;
      impactCoordStd.m_z = coordBackImpactStd.m_z;
    } else if (faceHit == 1) {
      impactCoordOrig.m_x = coordFrontImpactOrig.m_x;
      impactCoordOrig.m_y = coordFrontImpactOrig.m_y;
      impactCoordOrig.m_z = coordFrontImpactOrig.m_z;
      distance = timeImpactFront;
      impactCoordStd.m_x = coordFrontImpactStd.m_x;
      impactCoordStd.m_y = coordFrontImpactStd.m_y;
      impactCoordStd.m_z = coordFrontImpactStd.m_z;
    } else if (faceHit == 2) {
      impactCoordOrig.m_x = origCoord.m_x + (origDirVec.m_xDir * topImpactTime);
      impactCoordOrig.m_y = origCoord.m_y + (origDirVec.m_yDir * topImpactTime);
      impactCoordOrig.m_z = origCoord.m_z + (origDirVec.m_zDir * topImpactTime);
      impactCoordStd.m_x = newCoordTop.m_x;
      impactCoordStd.m_y = newCoordTop.m_y;
      impactCoordStd.m_z = newCoordTop.m_z;
    } else if (faceHit == 3) {
      impactCoordOrig.m_x = origCoord.m_x + (origDirVec.m_xDir * bottomImpactTime);
      impactCoordOrig.m_y = origCoord.m_y + (origDirVec.m_yDir * bottomImpactTime);
      impactCoordOrig.m_z = origCoord.m_z + (origDirVec.m_zDir * bottomImpactTime);
      impactCoordStd.m_x = newCoordBottom.m_x;
      impactCoordStd.m_y = newCoordBottom.m_y;
      impactCoordStd.m_z = newCoordBottom.m_z;
    }
    
  } // end if-block specifying object type
  
} // end getPhotonObjectImpactCoords()


/******************************************************************************/


void sidewallImpact(const CartesianCoord & coordIn, 
                    const DirectionVector & dirVec, 
                    const CartesianCoord & coordFoil, 
                    bool checkFoilImpact,
                    double tanThetaWall1, 
                    double tanThetaWall2, 
                    bool & foilImpactedFirst, 
                    int & sidewallOutcome,
                    double & tWallImpact, 
                    CartesianCoord & coordImpact,
                    int & wallID) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  double t1 = 0.0;
  double t2 = 0.0;
  double t1denominator = 0.0;
  double t2denominator = 0.0;
  
  // -------------------------------------

  // We don't know yet which of the two side walls is facing the direction of
  // motion, so compute the time for impacting each of them. The correct one is 
  // the one with a positive time. Need to catch the case of a side wall at 
  // 90 or 270 degrees (indicated by a very large (artificial) value of 
  // tanThetaWall1 or tanThetaWall2)
  
  //+++ 1e29????
  if (std::abs(tanThetaWall1) > 1.e29) {
    // Side wall #1 is parallel to the y-axis
    t1 = -1. * coordIn.m_x / dirVec.m_xDir;
  } else {
    t1denominator = (dirVec.m_xDir * tanThetaWall1) - dirVec.m_yDir;
    if (t1denominator == 0.0) {
      // Photon is traveling parallel to sidewall #1 so no impact is possible
      sidewallOutcome = -1;
    } else {
      t1 = (coordIn.m_y - (coordIn.m_x * tanThetaWall1)) / t1denominator;
    }
  }
  
  if (std::abs(tanThetaWall2) > 1.e29) {
    // Side wall #2 is parallel to the y-axis
    t2 = -1. * coordIn.m_x / dirVec.m_xDir;
  } else {
    t2denominator = (dirVec.m_xDir * tanThetaWall2) - dirVec.m_yDir;
    if (t2denominator == 0.0) {
      // Photon is traveling parallel to sidewall #2 so no impact is possible
      sidewallOutcome = -2;
    } else {
      t2 = (coordIn.m_y- (coordIn.m_x * tanThetaWall2)) / t2denominator;
    }
  }
  
  if ((t1 > 0.0) && (t2 > 0.0)) {
    // t1 and t2 are both positive - what to do?!
    tWallImpact = std::min(t1, t2);
    sidewallOutcome = 1;
    if (t1 < t2) {
      wallID = 1;
    } else if (t1 > t2) {
      wallID = 2;
    } else {
      // 3rd case is if the two times are equal but this should not happen so a
      // negative value for wallID is used indicate trouble.
      wallID = -1;
    }
  } else if ((t1 < 0.0) && (t2 < 0.0)) {
    // t1 and t2 are both negative - no impact is possible.
    sidewallOutcome = -3;
  } else if ((t1 == 0.0) && (t2 == 0.0)) {
    // Both t1 and t2 are zero: your telescope is messed up!
    // +++ AH_INFO?
    sidewallOutcome = -4;
  } else {
    // t1 and t2 have opposite signs
    sidewallOutcome = 0;
    tWallImpact = std::max(t1, t2);
    if (t1 > t2) {
      wallID = 1;
    } else if (t1 < t2) {
      wallID = 2;
    } else {
      // 3rd case is if the two times are equal but this should not happen so a
      // negative value for wallID is used indicate trouble.
    // +++ AH_INFO?
      wallID = -1;
    }
  }
  
  if (sidewallOutcome >= 0) {
    
    // Calculate sidewall impact coordinates
    coordImpact.m_x = coordIn.m_x + (dirVec.m_xDir * tWallImpact);
    coordImpact.m_y = coordIn.m_y + (dirVec.m_yDir * tWallImpact);
    coordImpact.m_z = coordIn.m_z + (dirVec.m_zDir * tWallImpact);
    
    // default if we know the foil was not hit but the sidewall was hit
    foilImpactedFirst = false; 
    
    // if mirror or pre-collimator foil impact coordinates were specified in 
    // input parameters, check whether foil will be impacted first, instead of 
    // side wall
    if (checkFoilImpact) {
      if ( (coordFoil.m_x >= coordImpact.m_x) && 
           (coordFoil.m_x <= coordIn.m_x) && 
           (coordFoil.m_y >= coordImpact.m_y) &&
           (coordFoil.m_y <= coordIn.m_y) ) {
        // Foil is impacted before sidewall
        foilImpactedFirst = true;
      } else {
        // Sidewall is impacted before foil
        foilImpactedFirst = false;
      }
    }
    
  }
  
} // end sidewallImpact()


/******************************************************************************/


void getFlatCirclePhoton(long randSeed, double flatRadius, 
                         double & flatCircleTheta, double & flatCirclePhi) {

  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  double ctheta = 0.0;
  double cosf = 0.0;
  
  // -------------------------------------
  
  // We need equal numbers of photons in equal solid angles,
  // so generate a random cosine of the polar angle
  cosf = std::cos(flatRadius * s_arcminToRadian);
  ctheta = cosf + (getRandom() * (1-cosf));
  flatCircleTheta = std::acos(ctheta);

  // Generate a random azimuthal angle (radians) in the source
  flatCirclePhi = s_twopi * getRandom();

} // end getFlatCirclePhoton()


/******************************************************************************/


void getBetaModelPhoton(long randSeed, double coreRadius, double beta, 
                        double maxRadius, 
                        double & modelTheta, double & modelPhi) {

  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  double minRadius = 0.0;   // this could be an input parameter in the future
  double rat_min = 0.0;
  double rat_max = 0.0;
  double bslope = 0.0;
  double oneoverbslope = 0.0;
  double rminfac = 0.0;
  double rmaxfac = 0.0;
  double t = 0.0;
  
  // -------------------------------------
  
  rat_min = minRadius/coreRadius;
  rat_max = maxRadius/ coreRadius;
  bslope = 1.5 - 3.0 * beta;
  oneoverbslope = 1./bslope;
  // (1+(rmin/core_radius^2)**(1.5-3.0*beta):
  rminfac = std::pow(1.0 + rat_min * rat_min, bslope);
  // (1+(rmax/core_radius)^2)**(1.5-3.0*beta):
  rmaxfac = std::pow(1.0 + rat_max * rat_max, bslope);

  t = std::pow(getRandom() * (rmaxfac - rminfac) + rminfac, oneoverbslope);
  modelTheta = (s_arcminToRadian*coreRadius) * std::sqrt(t - 1.0);

  // Generate a random azimuthal angle (radians) in the source
  modelPhi = s_twopi * getRandom();
  
} // end getBetaModelPhoton()


/******************************************************************************/


void getSurfaceNormal(surfaceGeometryTypes_e surfaceGeometry, 
                      const std::vector<double> & geoParams, 
                      objectTypes_e objectType, 
                      const Slopes & slopes, 
                      int face, 
                      const CartesianCoord & coordIn, 
                      DirectionVector & normal,
                      int normalError) {
  //+++ make face into faces_e
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // direction of the normal vector (pos=inward, neg=out)
  double normalFactor = 0.0;
  // choose which geoParam element we want, based on what face this is
  double geoFactor = ( (face == 0) ? geoParams[3] : geoParams[4] );
  //  choose which slope element we want, based on what face this is
  double slopeFactor = ( (face == 0) ? slopes.m_backSq : slopes.m_frontSq );
  // used with slopeFactor later 
  double factor = 0.0;
  
  //+++ ask if declaring these here will affect speed appreciably (rather than
  //    declaring inside if-block where they're needed
  
  // initialize output variable
  normalError = 0;
  
  // -------------------------------------
  
  // Note: The magnitude of the normal vector should be unity but it is not 
  // explicitly checked here in the interest of speed. It should automatically 
  // be unity because the formulas were derived from imposition of the 
  // condition for unit magnitude but numerical errors may creep in so we need 
  // to keep an eye on this. 
  
  // The following will determine the direction of the normal vector 
  // (inward or outward)
  if (face == 0) {
    normalFactor = 1.0;
  } else if (face == 1) {
    normalFactor = -1.0;
  } else {
    normalError = 1;
    return;
  }
  
  if (objectType == FOIL) {
  
    if (surfaceGeometry == CYLINDER) {
      normal.m_xDir = normalFactor * coordIn.m_x / geoFactor;
      normal.m_yDir = normalFactor * coordIn.m_y / geoFactor;
      normal.m_zDir = 0.0;
    } else if (surfaceGeometry == CONE) {
      if (coordIn.m_z == 0.0) {
        // point it as apex of cone.  Error: no normal exists here 
        normal.m_xDir = 0.0;
        normal.m_yDir = 0.0;
        normal.m_zDir = 0.0;
        normalError = 2;
      } else {
        factor = normalFactor / std::sqrt(1.0 + slopeFactor);
        normal.m_xDir = factor * coordIn.m_x * slopeFactor / coordIn.m_z;
        normal.m_yDir = factor * coordIn.m_y * slopeFactor / coordIn.m_z;
        normal.m_zDir = -1.0 * factor;
      }
    } else {
      AH_THROW_RUNTIME("Only Cylinders and Cones are supported");
    }  // end if-block surfaceGeometry
  
  } else {
      AH_THROW_RUNTIME("Only Foils are supported");
  } // end if-block objectType
  
} // end getSurfaceNormal()


/******************************************************************************/


void getReflectionDirection(const DirectionVector & initialDir, 
                            const DirectionVector & normal, 
                            bool isSpecular, 
                            double & sinIncident, 
                            double & sinReflected,
                            DirectionVector & reflectedDir, 
                            int & reflError) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  double twovindotn = 0.0;
  double sinIncidentSq = 0.0;
  double cosReflected = 0.0;
  double sinReflectedSq = 0.0;
  double ratio = 0.0;
  double ratioSq = 0.0;
  double factor = 0.0;
  double ncoef = 0.0;
  double vcoef = 0.0;
  //+++ ask if declaring these here will affect speed appreciably (rather than
  //    declaring inside if-block where they're needed
  
  // initialize output variable
  reflError = 0;
  
  // -------------------------------------
  
  // if sin(incident angle) is not supplied, calculate it
  if (sinIncident < -1.0) {
    sinIncident = ( (initialDir.m_xDir * normal.m_xDir) + 
                    (initialDir.m_yDir * normal.m_yDir) + 
                    (initialDir.m_zDir * normal.m_zDir) ) * -1.0;
  }
  
  if (isSpecular) {
    
    if (sinIncident == 0.0) {
      sinReflected = 0.0;
      reflectedDir.m_xDir = initialDir.m_xDir;
      reflectedDir.m_yDir = initialDir.m_yDir;
      reflectedDir.m_zDir = initialDir.m_zDir;
    } else {
      // If incident angle is non-zero and we have specular reflection
      sinReflected = sinIncident;
      twovindotn = -2.0 * sinIncident;
      reflectedDir.m_xDir = initialDir.m_xDir - (twovindotn * normal.m_xDir);
      reflectedDir.m_yDir = initialDir.m_yDir - (twovindotn * normal.m_yDir);
      reflectedDir.m_zDir = initialDir.m_zDir - (twovindotn * normal.m_zDir);
    }
    
  } else {
    // If the specular direction is modified (e.g. by scattering)
    
//    if (sinReflected == 0.0) {
//      AH_OUT << "getReflectionDirection(): sinReflected == 0.0" << std::endl;
//    }
    
    
    sinIncidentSq = sinIncident * sinIncident;
    sinReflectedSq = sinReflected * sinReflected;
    if (sinIncident == 0.0) {
      cosReflected = std::sqrt(1.0 - sinReflectedSq);
      reflectedDir.m_xDir = (cosReflected * initialDir.m_xDir) + (sinReflected * normal.m_xDir);
      reflectedDir.m_yDir = (cosReflected * initialDir.m_yDir) + (sinReflected * normal.m_yDir);
      reflectedDir.m_zDir = (cosReflected * initialDir.m_zDir) + (sinReflected * normal.m_zDir);
    } else if (sinIncidentSq < 1.0) {
      // If incident angle is non-zero and we have non-specular reflection 
      // (but the incident angle must be less than 90 degrees)
      ratio = sinIncident / sinReflected;
      ratioSq = ratio * ratio;
      factor = std::sqrt(1.0 - ((ratioSq - 1.0)/(sinIncidentSq - 1.0)));
      ncoef = sinReflected * (1.0 + factor);
      vcoef = factor / ratio;
      reflectedDir.m_xDir = (vcoef * initialDir.m_xDir) + (ncoef * normal.m_xDir);
      reflectedDir.m_yDir = (vcoef * initialDir.m_yDir) + (ncoef * normal.m_yDir);
      reflectedDir.m_zDir = (vcoef * initialDir.m_zDir) + (ncoef * normal.m_zDir);
//      if (sinReflected == 0.0) {
//        AH_OUT << "sinIncident = " << sinIncident << std::endl;
//        AH_OUT << "sinReflected = " << sinReflected << std::endl;
//        AH_OUT << "ratio = " << ratio << std::endl;
//        AH_OUT << "ratioSq = " << ratioSq << std::endl;
//        AH_OUT << "sinIncidentSq = " << sinIncidentSq << std::endl;
//        AH_OUT << "1.0 + ((ratioSq - 1.0)/(sinIncidentSq - 1.0)) = " << 1.0 + ((ratioSq - 1.0)/(sinIncidentSq - 1.0)) << std::endl;
//        AH_OUT << "factor = " << factor << std::endl;
//        AH_OUT << "vcoef = " << vcoef << std::endl;
//        AH_OUT << "initialDir = " << initialDir << std::endl;
//        AH_OUT << "ncoef = " << ncoef << std::endl;
//        AH_OUT << "normal = " << normal << std::endl;
//        AH_OUT << "vcoef^2 + ncoef^2 = " << vcoef*vcoef + ncoef*ncoef << std::endl;
//        AH_OUT << "reflectedDir = " << reflectedDir << std::endl;   
//      }
      AH_DEBUG << "sinIncident = " << sinIncident << std::endl;
      AH_DEBUG << "sinReflected = " << sinReflected << std::endl;
      AH_DEBUG << "ratio = " << ratio << std::endl;
      AH_DEBUG << "ratioSq = " << ratioSq << std::endl;
      AH_DEBUG << "sinIncidentSq = " << sinIncidentSq << std::endl;
      AH_DEBUG << "1.0 + ((ratioSq - 1.0)/(sinIncidentSq - 1.0)) = " << 1.0 + ((ratioSq - 1.0)/(sinIncidentSq - 1.0)) << std::endl;
      AH_DEBUG << "factor = " << factor << std::endl;
      
      AH_DEBUG << "vcoef = " << vcoef << std::endl;
      AH_DEBUG << "initialDir = " << initialDir << std::endl;
      
      AH_DEBUG << "ncoef = " << ncoef << std::endl;
      AH_DEBUG << "normal = " << normal << std::endl;
      
      AH_DEBUG << "vcoef^2 + ncoef^2 = " << vcoef*vcoef + ncoef*ncoef << std::endl;
      
      AH_DEBUG << "reflectedDir = " << reflectedDir << std::endl;
    } else {
      // ERROR: incident grazing angle must be less than 90 degrees
      reflError = 1;
    }
  } // end if-block isSpecular
  
} // end getReflectionDirection()


/******************************************************************************/


void getScatteredDirection(double incidentAngle, 
                           int segmentID, 
                           int scatColIdx, 
                           int energyIdx,
                           const Scattering & scat, 
                           bool isRoughScattering,
                           double & scatteredAngle) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int incidentAngleBin = 0;
  
  // for creating a slice of the angle bin
  int firstsubanglebin = 0;
  int lastsubanglebin = 0;
  int numsubscatterangles = 0;
  vector1Ddbl subscatteringangles;
  int scatIdx = 0;
  
  // a slice of the cumulative probability array
  vector1Ddbl cumscatprobslice;
  
  // Maximum cumulative probability
  double maxcumulativeprob = 0.0;
  
  // Random number between 0.0 and maxcumulativeprob
  double sprobval = 0.0;
  
  // -------------------------------------
  
  AH_DEBUG << "incidentAngle = " << incidentAngle << std::endl;
  AH_DEBUG << "segmentID = " << segmentID << std::endl;
  AH_DEBUG << "scatColIdx = " << scatColIdx << " is " << scat.m_scatcolnames[segmentID-1][scatColIdx] << std::endl;
  AH_DEBUG << "energyIdx = " << energyIdx << std::endl;
  AH_DEBUG << "isRoughScattering = " << (isRoughScattering ? "true" : "false") << std::endl;
//  AH_DEBUG << "input scatteredAngle = " << scatteredAngle << std::endl;
  
  // Return with specular reflection assumption if no scattering data exists
  if (scatColIdx < 0) {
    scatteredAngle = incidentAngle;
    return;
  }
  
  // The portion of the scattering probability array that is required is obtained by the extracting the sections of the array referenced by energyindex for the energy slice, segmentid for the slice relevant to the XRT segment, and scatcolumnindex for the region of the XRT that was referred to by xrtobjectscatterindex() (corresponding to one of the columns in the original FITS scattering profile file). For the incident angle slice, we obtain the index number of the element in the incident angle grid by using the start value and the grid angle interval
  
  // If the surface is a rough surface then use the rough surface angle grid, 
  // otherwise use the front-side mirror angle grid
  if (isRoughScattering) {
    
    // If the input incident angle is out of range, just use the lowest or 
    // highest value in the grid.
    if (incidentAngle < scat.m_roughIncidentAngles[0]) {
      incidentAngleBin=0;
    } else if (incidentAngle > scat.m_roughIncidentAngles[scat.m_numRoughIncidentAngles-1]) {
      incidentAngleBin = scat.m_numRoughIncidentAngles-1;
    } else {
      incidentAngleBin = static_cast<int>((incidentAngle - scat.m_firstRoughIncidentAngle) / scat.m_deltaRoughIncidentAngle); 
    }
    
  } else {
    
    // If the input incident angle is out of range, just use the lowest or highest value in the grid. 
    // Later, we should flag this so that the photon acquires an error code indicating that this happened.
    // If the input incident angle is out of range, just use the lowest or 
    // highest value in the grid.
    if (incidentAngle < scat.m_incidentAngles[0]) {
      incidentAngleBin=0;
    } else if (incidentAngle > scat.m_incidentAngles[scat.m_numIncidentAngles-1]) {
      incidentAngleBin = scat.m_numIncidentAngles-1;
    } else {
      incidentAngleBin = static_cast<int>((incidentAngle - scat.m_firstIncidentAngle) / scat.m_deltaIncidentAngle);
    }
    
  }
  
  // Now extract a sub-array of just cumulative probability versus scattering angle
  // In the file the scattering probabilities were given at angle bin centers but for the cumulative probability array we have values at the upper angle bin bounds and we need to add an additional value of zero cumulative probability at the lower bound of the first angle bin that was give in the file. In readscatteringfile we have: 
  // firstnzscatprob(segment,column,energy,incident angle)= index of 1st non-zero probability 
  // lastnzscatprob (segment,column,energy,incident angle = index of last non-zero probability 
  // so number of angle values in the subarray is (lastnzscatprob-firstnzscatprob+2)
  firstsubanglebin = scat.m_firstNonZeroScatProb[segmentID-1][scatColIdx][energyIdx][incidentAngleBin] - 1;
  lastsubanglebin = scat.m_lastNonZeroScatProb[segmentID-1][scatColIdx][energyIdx][incidentAngleBin];
  numsubscatterangles = lastsubanglebin - firstsubanglebin + 1;
  subscatteringangles.resize(numsubscatterangles);
  for (int iScat = 0 ; iScat < numsubscatterangles ; ++iScat) {
    scatIdx = iScat + firstsubanglebin;
    if (isRoughScattering) {
      subscatteringangles[iScat] = scat.m_roughScatteringAngles[scatIdx];
//      AH_DEBUG << "scat.m_roughScatteringAngles["<<scatIdx<<"] = " << scat.m_roughScatteringAngles[scatIdx] << std::endl;
      AH_DEBUG << "subscatteringangles["<<iScat<<"] = " << subscatteringangles[iScat] << std::endl;
    } else {
      subscatteringangles[iScat] = scat.m_scatteringAngles[scatIdx];
    }
    
  }
  
  AH_DEBUG << "first time subscatteringangles[0] = " << subscatteringangles[0] << std::endl;
  
  // If a1 is the original "first" angle bin above and w is the interval, we 
  // need a1-w for the very first angle bin
  // i.e. a1 -> a1-w = a1 - (a2-a1) = 2(a1) - a2
  subscatteringangles[0] = (2 * subscatteringangles[0]) - scat.m_scatteringAngles[firstsubanglebin];
  
//  for (uint ii = 0 ; ii < scat.m_firstNonZeroScatProb[0][0][0].size() ; ++ii) {
//    AH_DEBUG << "scat.m_firstNonZeroScatProb[0][0][0]["<<ii<<"] = " << scat.m_firstNonZeroScatProb[0][0][0][ii] << std::endl;
//  }
  
  AH_DEBUG << "incidentAngleBin = " << incidentAngleBin << std::endl;
  AH_DEBUG << "scat.m_firstNonZeroScatProb[segmentID-1][scatColIdx][energyIdx][incidentAngleBin] = " << 
              "scat.m_firstNonZeroScatProb["<<segmentID-1<<"]["<<scatColIdx<<"]["<<energyIdx<<"]["<<incidentAngleBin<<"] = " << 
              scat.m_firstNonZeroScatProb[segmentID-1][scatColIdx][energyIdx][incidentAngleBin] << std::endl;
  AH_DEBUG << "firstsubanglebin = " << firstsubanglebin << std::endl;
  AH_DEBUG << "lastsubanglebin = " << lastsubanglebin << std::endl;
  AH_DEBUG << "numsubscatterangles = " << numsubscatterangles << std::endl;
  
  AH_DEBUG << "again" << std::endl;
  for (int iScat = 0 ; iScat < numsubscatterangles ; ++iScat) {
    AH_DEBUG << "subscatteringangles["<<iScat<<"] = " << subscatteringangles[iScat] << std::endl;
  }
  
  // create the cumulative probability slice for these scattering angles
  cumscatprobslice.resize(numsubscatterangles);
  for (int iScat = 0 ; iScat < numsubscatterangles ; ++iScat) {
    scatIdx = iScat + firstsubanglebin;
    cumscatprobslice[iScat] = scat.m_cumScatDist[segmentID-1][scatColIdx][energyIdx][scatIdx][incidentAngleBin];
  }
  
  AH_DEBUG << "again" << std::endl;
  for (int iScat = 0 ; iScat < numsubscatterangles ; ++iScat) {
    AH_DEBUG << "subscatteringangles["<<iScat<<"] = " << subscatteringangles[iScat] << std::endl;
  }
  
  
  AH_DEBUG << "iScat \t subscatteringangles \t cumscatprobslice" << std::endl;
  for (int iScat = 0 ; iScat < numsubscatterangles ; ++iScat) {
    AH_DEBUG << iScat << "\t" << subscatteringangles[iScat] << "\t" << cumscatprobslice[iScat] << std::endl;
  }
  
  
  // Maximum cumulative probability: if everything is normalized correctly this should be 1.0 – however if choose a random number between 0 and the maximum probability, the routine will still work. This avoids running into precision problems and/or having to decide on a tolerance level. The interpolation routine will not work if a scattering probability is requested that is outside the range, so the exact maximum value must be used here.
  maxcumulativeprob = getMaxDouble(cumscatprobslice);
  AH_DEBUG << "maxcumulativeprob = " << maxcumulativeprob << std::endl;
  
  // Generate a scattering probability using the random number generator and random number seed to generate a random number between 0.0 and maxcumulativeprob
  sprobval = getRandom() * maxcumulativeprob;
  AH_DEBUG << "sprobval = " << sprobval << std::endl;
  
  AH_DEBUG << "before bisection interp" << std::endl;
  
  // Interpolate on the tabulated probability to infer the scattering angle that this corresponds to (note that the relation between scattering angle and probability has to be "inverted"):
  bisectionInterp(numsubscatterangles, cumscatprobslice, subscatteringangles, sprobval, scatteredAngle);
  
  AH_DEBUG << "after bisection interp" << std::endl;
  
  // The output scatteredAngle can be negative or positive and adds to the incident angle to give the net (grazing) reflection angle. Obviously, this angle cannot be <0 because the photon would be entering the material surface. The scattering profile should already be constrained correctly so that this does not happen, but just in case:
  if ( (scatteredAngle + incidentAngle) < 0 ) { 
    scatteredAngle = -1.0 * incidentAngle;
  }

} // end getScatteredDirection()


/******************************************************************************/


void resetEventCounters(StatResults & stats) {
  stats.numresultsplaneimpacts = 0;
  stats.numrpiPerEnergy = 0;
  stats.numphwitherror = 0;
  stats.numrpiphwitherror = 0;
  stats.numrpiPrimaryRefl = 0;
  stats.numrpiSecondaryRefl = 0;
  stats.numrpiDoubleRefl = 0;
  stats.numrpiDoubleReflPerEnergy = 0;
  stats.numrpiPriSecRefl = 0;
  stats.numrpiMoreThanTwoRefl = 0;
  stats.numrpiNoFrontRefl = 0;
  stats.numrpiPreColReflNoMirror = 0;
  stats.numrpiPreColReflOneMirror = 0;
  stats.numrpiPreColReflTwoMirror = 0;
  stats.numrpiPreColReflMoreThanTwoMirror = 0;
  stats.numrpiBackSideOneRefl = 0;
  stats.numrpiBackSideMoreThanOne = 0;
  stats.numrpiScatter = 0;
  stats.numrpiFrntMScatter = 0;
  stats.numTransmission = 0;
  stats.numPreColTransmission = 0;
  stats.numMirrorTransmission = 0;
  stats.numrpiTransmission = 0;
  stats.numrpiPreColTransmission = 0;
  stats.numrpiMirrorTransmission = 0;
  stats.numTotalAbsorbed = 0;
  stats.numPreColEdgeImpact = 0;
  stats.numPreColEdgeImpactAbsorbed = 0;
  stats.numMirrorEdgeImpact = 0;
  stats.numEdgeMirrorAbsorbed = 0;
  stats.numPreColFaceAbsorbed = 0;
  stats.numFrontMirrorAbsorbed = 0;
  stats.numBackMirrorAbsorbed = 0;
  stats.numObstructionAbsorbed = 0;
  stats.numSectorWallAbsorbed = 0;
  stats.numInnerHousingAbsorbed = 0;
  stats.numOuterHousingAbsorbed = 0;
  stats.numAllResultsPlaneImpacts = 0;
  stats.numrpiNoScatter = 0;
  
} // end resetEventCounters()


/******************************************************************************/


void updateEventCounters(long numInteractions, 
                         const std::vector< std::vector<int> > & pathCode, 
                         bool resultsPlaneWasImpacted, 
                         StatResults & stats, 
                         std::vector<int> & pathErrorCode, 
                         int & sumpatherrorcode) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // counters
  long numtransmissionevents = 0;
  long numpcoltransmissionevents = 0;
  long nummirrortransmissionevents = 0;
  long numprimaryrefl = 0;
  long numprimarybackrefl = 0;
  long numprimaryfrontrefl = 0;
  long numsecondaryrefl = 0;
  long numsecondarybackrefl = 0;
  long numsecondaryfrontrefl = 0;
  long numpcolrefl = 0;
  long numreflectionswithscattering = 0;
  long numfrntmirrorreflwithscattering = 0;
  long totalnumberofreflections = 0;
  long totalmirrorreflections = 0;
  long totalfrontmirrorreflections = 0;
  long totalbackmirrorreflections = 0;
  
  // initialize output
  sumpatherrorcode = 0;
  
  // -------------------------------------
  
  // First update the counters that apply to all photons, not just those that 
  // impact the results plane ( which is focal plane by default)
  
  // Number of photons that resulted in an error condition (non-zero error), 
  // out of all photons input.
  for (long iInteraction = 1 ; iInteraction <= numInteractions ; ++iInteraction) {
    
    sumpatherrorcode += pathErrorCode[iInteraction];
    
    // +++ use enums?
    if ( (pathCode[iInteraction][0] == 1) && (pathCode[iInteraction][1] != 1) ) {
      
      // photon absorbed before reaching focal plane
      stats.numTotalAbsorbed++;
      
      // more details about where the absorption took place
      // +++ put each if case into (), or refactor into switch?
      // +++ use enum or DEFINE instead of using numbers
      if ( (pathCode[iInteraction][1] == 5) && (pathCode[iInteraction][2] <= 2) ) {
        stats.numPreColFaceAbsorbed++;
        
      } else if ( (pathCode[iInteraction][1] == 5) && (pathCode[iInteraction][2] > 2) ) {
        // +++ what if it == 2?
        stats.numPreColEdgeImpactAbsorbed++;
        
      } else if ( (pathCode[iInteraction][1] == 6) || (pathCode[iInteraction][1] == 7) ) {
        
        if (pathCode[iInteraction][2] == 1) {
          stats.numBackMirrorAbsorbed++; 
        } else if (pathCode[iInteraction][2] == 2) {
          stats.numFrontMirrorAbsorbed++;
        } else if (pathCode[iInteraction][2] > 2) {
          stats.numEdgeMirrorAbsorbed++;
        }
        
      } else if (pathCode[iInteraction][1] == 4) {
        stats.numSectorWallAbsorbed++;
      } else if (pathCode[iInteraction][1] == 2) {
        stats.numInnerHousingAbsorbed++;
      } else if (pathCode[iInteraction][1] == 3) {
        stats.numOuterHousingAbsorbed++;
      } else if (pathCode[iInteraction][1] == 8) {
        stats.numObstructionAbsorbed++;
      }
      
    // transmission, regardless of whether results plane was impacted
    } else if (pathCode[iInteraction][0] == 4) {
      numtransmissionevents++;
      
      // If the transmission was through a pre-collimator foil:
      if (pathCode[iInteraction][1] == 5) {
        numpcoltransmissionevents++;
      
        // If the transmission was through a primary or secondary mirror foil:
      } else if ( (pathCode[iInteraction][1] == 6) || (pathCode[iInteraction][1] == 7) ) {
        nummirrortransmissionevents++;
      }
      
    // Number of pre-collimator edge impacts regardless of interaction type
    } else if ( (pathCode[iInteraction][1] == 5) && (pathCode[iInteraction][2] > 2) ) {
      stats.numPreColEdgeImpact++;
    // Number of primary mirror edge impacts regardless of interaction type
    } else if ( (pathCode[iInteraction][1] == 6) && (pathCode[iInteraction][2] > 2) ) {
      stats.numMirrorEdgeImpact++;
    // Number of secondary mirror edge impacts regardless of interaction type
    } else if ( (pathCode[iInteraction][1] == 7) && (pathCode[iInteraction][2] > 2) ) {
      stats.numMirrorEdgeImpact++;
    }
    
  } // end for-loop through interactions
  
  // Update number of photons that resulted in an error condition (non-zero error), out of all photons input.
  if (sumpatherrorcode > 0) {
    stats.numphwitherror++;
  }
  
  // Update counters for transmission of photons regardless of whether the results plane was impacted or not
  if (numtransmissionevents > 0) {
    stats.numTransmission++;
  }
  if (numpcoltransmissionevents > 0) {
    stats.numPreColTransmission++;
  }
  if (nummirrortransmissionevents > 0) {
    stats.numMirrorTransmission++;
  }
  
  // Counters inside the following loop track only photons that impact the results plane (=focal plane by default)
  if (resultsPlaneWasImpacted) {
    
    // Update the number of photons impacting results plane regardless of error condition
    stats.numAllResultsPlaneImpacts++;
    
    if (sumpatherrorcode == 0) {
      
      // Update the number of photons impacting the focal plane for paths that were error-free
      stats.numresultsplaneimpacts++;
      
      // Update the number of results-plane photons for the current unique energy (for EA calculation)
      stats.numrpiPerEnergy++;
      
      // Tally various reflection paths
      
      // reset counters for each interaction
      numprimaryrefl = 0;
      numprimarybackrefl = 0;
      numprimaryfrontrefl = 0;
      numsecondaryrefl = 0;
      numsecondarybackrefl = 0;
      numsecondaryfrontrefl = 0;
      numpcolrefl = 0;
      numreflectionswithscattering = 0;
      numfrntmirrorreflwithscattering = 0;

      // loop over each interaction and examine path attributes
      for (long iInteraction = 0 ; iInteraction < numInteractions ; ++iInteraction) {
        
        // If reflection on any object occurred, with or without scattering:
        if ( (pathCode[iInteraction][0]==2) || (pathCode[iInteraction][0]==3) ) {
          if (pathCode[iInteraction][1]==5) {
            // If pre-collimator
            numpcolrefl++;
          } else if (pathCode[iInteraction][1]==6) {
            // If primary mirror
            numprimaryrefl++;

            if (pathCode[iInteraction][2]==1) {
              // backside
              numprimarybackrefl++;
            } else if (pathCode[iInteraction][2]==2) {
              // frontside
              numprimaryfrontrefl++;
            }
          } else if (pathCode[iInteraction][1]==7) {
            // If secondary mirror
            numsecondaryrefl++;
            
            if (pathCode[iInteraction][2]==1) {
              numsecondarybackrefl++;
            } else if (pathCode[iInteraction][2]==2) {
            numsecondaryfrontrefl++;
            }

          }  // end if-block checking object type    //+++ how does this check object type?
          
          // end if-block checking for reflection or reflection+scattering, 
          // now count reflection+scattering only
        } else if (pathCode[iInteraction][0] == 3) {
          numreflectionswithscattering++;
          if ( (pathCode[iInteraction][1]>=6) && (pathCode[iInteraction][1]<=7) && (pathCode[iInteraction][2]==1) ) {
            // Scattering after a front-side mirror reflection
            numfrntmirrorreflwithscattering++;
          }
        
        } // end if-block checking for reflection (regardless of scattering)
        
      } // end for-loop through interactions
      
      // Now tally up various scenarios
      totalnumberofreflections = numpcolrefl + numprimaryrefl + numsecondaryrefl;
      totalmirrorreflections = numprimaryrefl + numsecondaryrefl;
      totalfrontmirrorreflections = numprimaryfrontrefl + numsecondaryfrontrefl;
      totalbackmirrorreflections = numprimarybackrefl + numsecondarybackrefl;
      if (numreflectionswithscattering == 0) {
        stats.numrpiNoScatter++;
      } else {
        stats.numrpiScatter++;
        if (numfrntmirrorreflwithscattering > 0) {
        stats.numrpiFrntMScatter++;
        }
      }
      
      // Number of photons that underwent no front-side mirror reflections
      if (numprimaryfrontrefl == 0) {
        stats.numrpiNoFrontRefl++;
      }
      // More than two front-side mirror reflections, no pre-col reflection
      if (numprimaryfrontrefl > 2) {
        stats.numrpiMoreThanTwoRefl++;
      }
      // One front-side primary mirror reflection, no other reflection
      if ( (numprimaryfrontrefl == 1) && (totalnumberofreflections == 1) ) {
        stats.numrpiPrimaryRefl++;
      }
      // One front-side secondary mirror reflection, no other reflection
      if ( (numsecondaryfrontrefl == 1) && (totalnumberofreflections == 1) ) {
        stats.numrpiSecondaryRefl++;
      }
      // Exactly one front-side mirror primary reflection, one secondary reflection, and no other reflection
      if ( (numprimaryfrontrefl == 1) && (numsecondaryfrontrefl == 1) && (numpcolrefl == 0) ) {
        stats.numrpiPriSecRefl++;
      }
      // Number of results-plane photons that underwent exactly one reflection on the back-side of a mirror foil (regardless of other interactions):
      if (totalbackmirrorreflections == 1) {
        stats.numrpiBackSideOneRefl++;
      }
      // Number of results-plane photons that underwent more than one reflection on the back-side of a mirror foil (regardless of other interactions):
      if (totalbackmirrorreflections > 1) {
        stats.numrpiBackSideMoreThanOne++;
      }
      // At least one pcol reflection, no mirror reflection
      if ((numpcolrefl != 0) && (totalmirrorreflections == 0)) {
        stats.numrpiPreColReflNoMirror++;
      }
      // At least one pcol reflection, one front-side mirror reflection
      if ((numpcolrefl != 0) && (totalfrontmirrorreflections == 1)) {
        stats.numrpiPreColReflOneMirror++;
      }
      // At least one pcol reflection, two front-side mirror reflections
      if ((numpcolrefl != 0) && (totalfrontmirrorreflections == 2)) {
        stats.numrpiPreColReflTwoMirror++;
      }
      // At least one pcol reflection, more than two front-side mirror reflections
      if ( (numpcolrefl != 0) && (totalfrontmirrorreflections > 2) ) {
        stats.numrpiPreColReflMoreThanTwoMirror++;
      }
      
    } // end if-block (sumpatherrorcode == 0), photon path was error-free
    
    // Number of front-side double reflections (exactly one primary and one 
    // secondary), regardless of scattering or pre-collimator reflection, will 
    // be used for effective area calculation:
    if ( (numprimaryfrontrefl == 1) && (numsecondaryfrontrefl == 1) ) { 
      stats.numrpiDoubleRefl++;
      stats.numrpiDoubleReflPerEnergy++;
    }
    
  } // end if-block resultsPlaneWasImpacted
  
} // end updateEventCounters()


/******************************************************************************/


void setupXRTTransform(const std::vector<XRTObject> & XRTObjects, 
                       long numXRTObjects, 
                       double segmentRotationZOffset,
                       const Misalignment & misalignment_struct, 
                       Transforms & transforms) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  const int maxTransformNumSteps = 20;  // max num individual transform steps
  const int numDim = 3;               // the rest of the dimensions are 3
  int imx = 0;                        // used at end: transfer results to output
  int nSteps = 0;                     // num steps to combine into a net matrix
  const int numTrSteps = 6;           // num steps for transferring (at end)
  
  // variables to hold misalignment information, reset for each XRTObject
  double tiltPivot = 0.0;
  bool doTilt = false;
  bool doTwist = false;
  
    // +++ why didn't this segfault with runs m001a?  it did with m001b
      
  // set up lots of multidimensional arrays
  vector3Ddbl xrt2objectrotation(maxTransformNumSteps);
  vector2Ddbl xrt2objectshift(maxTransformNumSteps);
  vector3Ddbl object2xrtrotation(maxTransformNumSteps);
  vector2Ddbl object2xrtshift(maxTransformNumSteps);
  vector2Ddbl netxrt2objectrotation(numDim);
  vector1Ddbl netxrt2objectshift(numDim);
  vector2Ddbl netobject2xrtrotation(numDim);
  vector1Ddbl netobject2xrtshift(numDim);
  vector3Ddbl xrt2sidewallrotation(maxTransformNumSteps);
  vector2Ddbl xrt2sidewallshift(maxTransformNumSteps);
  vector3Ddbl sidewall2xrtrotation(maxTransformNumSteps);
  vector2Ddbl sidewall2xrtshift(maxTransformNumSteps);
  vector2Ddbl netxrt2sidewallrotation(numDim);
  vector1Ddbl netxrt2sidewallshift(numDim);
  vector2Ddbl netsidewall2xrtrotation(numDim);
  vector1Ddbl netsidewall2xrtshift(numDim);
  vector2Ddbl unitRotationMatrix(numDim);
  for (int i = 0 ; i < numDim ; ++i) {
    netxrt2objectrotation[i].resize(numDim);
    netobject2xrtrotation[i].resize(numDim);
    netxrt2sidewallrotation[i].resize(numDim);
    netsidewall2xrtrotation[i].resize(numDim);
    unitRotationMatrix[i].resize(numDim);
  }
  for (int i = 0 ; i < maxTransformNumSteps ; ++i) {
    xrt2objectrotation[i].resize(numDim);
    xrt2objectshift[i].resize(numDim);
    object2xrtrotation[i].resize(numDim);
    object2xrtshift[i].resize(numDim);
    xrt2sidewallrotation[i].resize(numDim);
    xrt2sidewallshift[i].resize(numDim);
    sidewall2xrtrotation[i].resize(numDim);
    sidewall2xrtshift[i].resize(numDim);
    for (int j = 0 ; j < numDim ; ++j) {
    xrt2objectrotation[i][j].resize(numDim);
    object2xrtrotation[i][j].resize(numDim);
    xrt2sidewallrotation[i][j].resize(numDim);
    sidewall2xrtrotation[i][j].resize(numDim);
    }
  }
  
  // set the 1s on the diagonal for the unit rotation matrix
  unitRotationMatrix[0][0] = 1.0;
  unitRotationMatrix[1][1] = 1.0;
  unitRotationMatrix[2][2] = 1.0;

  double radiusatpivot = 0.0;
  double ztop = 0.0;
  double zbottom = 0.0;
  double pivotaxiszheight = 0.0;
  double midwayphiangle = 0.0;
  double sinmidwayphiangle = 0.0;
  double cosmidwayphiangle = 0.0;
  double tiltinradians = 0.0;
  double costiltangle = 0.0;
  double sintiltangle = 0.0;
  double twistinradians = 0.0;
  double costwistangle = 0.0;
  double sintwistangle = 0.0;
  double sinsegmentx = 0.0;
  double cossegmentx = 0.0;
  double sinsegmenty = 0.0;
  double cossegmenty = 0.0;
  double sinsegmentz = 0.0;
  double cossegmentz = 0.0;
  
  // initialize outputs:
  // the four transform vectors (m_xrt2objectframe, etc) were sized before 
  // entering this function
  
  // -------------------------------------
  
  for (int iObj = 0 ; iObj < numXRTObjects ; ++iObj) {
    const XRTObject & currObject = XRTObjects[iObj];
    
    // Assign correct pivot axis, tilt and twist switches for this XRT object
    tiltPivot = misalignment_struct.m_tiltPivot[currObject.m_set];
    doTilt = misalignment_struct.m_tiltSwitch[currObject.m_set];
    doTwist = misalignment_struct.m_twistSwitch[currObject.m_set];
    
    // reset every element in the matrices to 0
    std::fill(netxrt2objectshift.begin(), netxrt2objectshift.end(), 0.0);
    std::fill(netobject2xrtshift.begin(), netobject2xrtshift.end(), 0.0);
    std::fill(netxrt2sidewallshift.begin(), netxrt2sidewallshift.end(), 0.0);
    std::fill(netsidewall2xrtshift.begin(), netsidewall2xrtshift.end(), 0.0);
    for (int ii = 0 ; ii < maxTransformNumSteps ; ++ii) {
      std::fill(xrt2objectshift[ii].begin(), xrt2objectshift[ii].end(), 0.0);
      std::fill(object2xrtshift[ii].begin(), object2xrtshift[ii].end(), 0.0);
      std::fill(xrt2sidewallshift[ii].begin(), xrt2sidewallshift[ii].end(), 0.0);
      std::fill(sidewall2xrtshift[ii].begin(), sidewall2xrtshift[ii].end(), 0.0);
      for (int jj = 0 ; jj < numDim ; ++jj) {
        std::fill(xrt2objectrotation[ii][jj].begin(), xrt2objectrotation[ii][jj].end(), 0.0);
        std::fill(object2xrtrotation[ii][jj].begin(), object2xrtrotation[ii][jj].end(), 0.0);
        std::fill(xrt2sidewallrotation[ii][jj].begin(), xrt2sidewallrotation[ii][jj].end(), 0.0);
        std::fill(sidewall2xrtrotation[ii][jj].begin(), sidewall2xrtrotation[ii][jj].end(), 0.0);
        
      }
    }
    
    if (currObject.m_type == OBSTRUCTION) {
      // For obstructions, only apply the housing shift (and set rotation to unit matrix)
      transforms.m_xrt2objectframe[iObj][0]  = 1.0;
      transforms.m_xrt2objectframe[iObj][4]  = 1.0;
      transforms.m_xrt2objectframe[iObj][8]  = 1.0;
      transforms.m_xrt2objectframe[iObj][9]  = currObject.m_xHousingShift;
      transforms.m_xrt2objectframe[iObj][10] = currObject.m_yHousingShift;
      transforms.m_xrt2objectframe[iObj][11] = 0.0;
      
      // also set the reverse reverse transform
      transforms.m_object2xrtframe[iObj][0]  = 1.0;
      transforms.m_object2xrtframe[iObj][4]  = 1.0;
      transforms.m_object2xrtframe[iObj][8]  = 1.0;
      transforms.m_object2xrtframe[iObj][9]  = -1.0 * currObject.m_xHousingShift;
      transforms.m_object2xrtframe[iObj][10] = -1.0 * currObject.m_yHousingShift;
      transforms.m_object2xrtframe[iObj][11] = 0.0;

    } else if (currObject.m_type == FOIL) {
      // For mirror or pre-collimator foils, more complex procedure
      
      // first extract some geometrical parameters that will be needed and 
      // calculate the height of the tilt pivot axis and its radial distance 
      // from the optical axis:
      ztop = currObject.m_geoParams[0];
      zbottom = currObject.m_geoParams[1];
      pivotaxiszheight = zbottom + (tiltPivot * (ztop - zbottom));
      if (currObject.m_geometry == CYLINDER) {
        // for cylindrical foils
        radiusatpivot = 0.50 * (currObject.m_geoParams[3] + currObject.m_geoParams[4]);
      } else {
        // For cone-shaped foils
        // Note that the following will crash if the foil z/r slope is zero 
        // (it should never be)
        // +++ set up error checking here
        radiusatpivot = 0.50 * (currObject.m_geoParams[3] + 
                                currObject.m_geoParams[4]) + 
                                ( (tiltPivot / currObject.m_slopes.m_front) * 
                                  (ztop - zbottom) );
      }
      
      // Calculate the half-way azimuthal angle of the foil to locate the twist axis
      midwayphiangle = 0.50 * ( std::abs(currObject.m_sideWalls[0]) + std::abs(currObject.m_sideWalls[1]) );
      sinmidwayphiangle = std::sin(midwayphiangle);
      cosmidwayphiangle = std::cos(midwayphiangle);
      
      
      if (iObj == 4 || iObj == 7 || iObj == 1) {
        AH_DEBUG << "iObj " << iObj << ": midwayphiangle deg = " << midwayphiangle * s_radianToDegrees << std::endl;
      } 
      
      // ----- FORWARD TRANSFORMATIONS -----
      
      // Set up 14 steps for foils (XRT to object frame, or, for photons, 
      // object to XRT frame):

      // 1. Auxillary shift down to z=0 so that the pivot axis for tilt lies in the z=0 plane; no rotation for this step.
      xrt2objectrotation[0] = unitRotationMatrix;
      xrt2objectshift[0][2] = -1.0 * pivotaxiszheight;
      
      // 2. Auxillary clockwise rotation around z axis to align center of foil on x-axis. Shift is zero for this step.
//      xrt2objectrotation[1][0][0] = cosmidwayphiangle;
//      xrt2objectrotation[1][0][1] = -1.0 * sinmidwayphiangle;
//      xrt2objectrotation[1][1][0] = sinmidwayphiangle;
//      xrt2objectrotation[1][1][1] = cosmidwayphiangle;
//      xrt2objectrotation[1][2][2] = 1.0;
      
      xrt2objectrotation[1][0][0] = cosmidwayphiangle;
      xrt2objectrotation[1][0][1] = sinmidwayphiangle;
      xrt2objectrotation[1][1][0] = -1.0 * sinmidwayphiangle;
      xrt2objectrotation[1][1][1] = cosmidwayphiangle;
      xrt2objectrotation[1][2][2] = 1.0;
      
      // 3. Auxillary shift in the -x direction to align the intersection of the pivot axis and the foil "vertical" axis with the origin.
      xrt2objectrotation[2] = unitRotationMatrix;
      xrt2objectshift[2][0] = -1.0 * radiusatpivot;
      
      // 4. Rotation by the tilt angle around y-axis; No shift in this step.
      if (doTilt) {
        tiltinradians = s_arcsecToRadian * currObject.m_tiltAngle;
        costiltangle = std::cos(tiltinradians);
        sintiltangle = std::sin(tiltinradians);
        xrt2objectrotation[3][0][0] = costiltangle;
        xrt2objectrotation[3][0][2] = -1.0 * sintiltangle;
        xrt2objectrotation[3][2][0] = sintiltangle;
        xrt2objectrotation[3][2][2] = costiltangle;
        xrt2objectrotation[3][1][1] = 1.0;
      } else {
        // simply put a diagonal matrix if tilt was switched off
        xrt2objectrotation[3] = unitRotationMatrix;
      }
      
      // 5. Rotation by the twist angle around z-axis; No shift in this step.
      if (doTwist) {
        twistinradians = s_arcsecToRadian * currObject.m_twistAngle;
        costwistangle = std::cos(twistinradians);
        sintwistangle = std::sin(twistinradians);
        xrt2objectrotation[4][0][0] = costwistangle;
        xrt2objectrotation[4][0][1] = sintwistangle;
        xrt2objectrotation[4][1][0] = -1.0 * sintwistangle;
        xrt2objectrotation[4][1][1] = costwistangle;
        xrt2objectrotation[4][2][2] = 1.0;
      } else {
        // simply put a diagonal matrix if twist was switched off
        xrt2objectrotation[4] = unitRotationMatrix;
      }
      
      // 6. Auxillary shift back along +x axis (no rotation).
      xrt2objectrotation[5] = unitRotationMatrix;
      xrt2objectshift[5][0] = radiusatpivot;
      
      // 7. Auxillary rotation back around z-axis counter clockwise. No shift.
//      xrt2objectrotation[6][0][0] = cosmidwayphiangle;
//      xrt2objectrotation[6][0][1] = sinmidwayphiangle;
//      xrt2objectrotation[6][1][0] = -1.0 * sinmidwayphiangle;
//      xrt2objectrotation[6][1][1] = cosmidwayphiangle;
//      xrt2objectrotation[6][2][2] = 1.0;
      
      xrt2objectrotation[6][0][0] = cosmidwayphiangle;
      xrt2objectrotation[6][0][1] = -1.0 * sinmidwayphiangle;
      xrt2objectrotation[6][1][0] = sinmidwayphiangle;
      xrt2objectrotation[6][1][1] = cosmidwayphiangle;
      xrt2objectrotation[6][2][2] = 1.0;
      
      // 8. Auxillary shift back up +z axis (no rotation).
      xrt2objectrotation[7] = unitRotationMatrix;
      xrt2objectshift[7][2] = pivotaxiszheight;
      
      // 9. Next do the segment shifts, sector radial shift, and housing shifts
      xrt2objectshift[8][0] = currObject.m_xSegmentShift + (currObject.m_rSectorShift * cosmidwayphiangle) + currObject.m_xHousingShift;
      xrt2objectshift[8][1] = currObject.m_ySegmentShift + (currObject.m_rSectorShift * sinmidwayphiangle) + currObject.m_yHousingShift;
      xrt2objectshift[8][2] = currObject.m_zSegmentShift;
      xrt2objectrotation[8] = unitRotationMatrix;
      
      // Segment rotations around x,y,z axes
      // The segment rotation transformations are currently defined for 
      // left-handed rotation: convert the segment rotations to be right-handed
      sinsegmentx = -1.0 * std::sin(currObject.m_xSegmentRot);
      cossegmentx = std::cos(currObject.m_xSegmentRot);
      sinsegmenty = -1.0 * std::sin(currObject.m_ySegmentRot);
      cossegmenty = std::cos(currObject.m_ySegmentRot);
      sinsegmentz = -1.0 * std::sin(currObject.m_zSegmentRot);
      cossegmentz = std::cos(currObject.m_zSegmentRot);
      
      if (iObj == 7) {
        AH_DEBUG << "cossegmentx = " << cossegmentx << std::endl;
        AH_DEBUG << "sinsegmentx = " << sinsegmentx << std::endl;
      }
      // 10. Auxillary shift along -z direction down to z=0 in order to do the segement rotations; no rotation in this step
      xrt2objectrotation[9] = unitRotationMatrix;
      xrt2objectshift[9][2] = -1.0 * segmentRotationZOffset;
      
      // 11. first, rotation around x-axis
      xrt2objectrotation[10][0][0] = 1.0;
      xrt2objectrotation[10][1][1] = cossegmentx;
      xrt2objectrotation[10][1][2] = sinsegmentx;
      xrt2objectrotation[10][2][1] = -1.0 * sinsegmentx;
      xrt2objectrotation[10][2][2] = cossegmentx;

      
//      if (iObj == 7) {
//        AH_DEBUG << "xrt2objectrotation[10][1][1] = " << xrt2objectrotation[10][1][1] << std::endl;
//      }
      
      // 12. next, rotation around y-axis
      xrt2objectrotation[11][1][1] = 1.0;
      xrt2objectrotation[11][0][0] = cossegmenty;
      xrt2objectrotation[11][0][2] = -1.0 * sinsegmenty;
      xrt2objectrotation[11][2][0] = sinsegmenty;
      xrt2objectrotation[11][2][2] = cossegmenty;

      // 13. Then rotation around z-axis
      xrt2objectrotation[12][2][2] = 1.0;
      xrt2objectrotation[12][0][0] = cossegmentz;
      xrt2objectrotation[12][0][1] = sinsegmentz;
      xrt2objectrotation[12][1][0] = -1.0 * sinsegmentz;
      xrt2objectrotation[12][1][1] = cossegmentz;
      
      // 14. Auxillary shift back up +z direction from z=0, no rotation in this step
      xrt2objectrotation[13] = unitRotationMatrix;
      xrt2objectshift[13][2] = 1.0 * segmentRotationZOffset;
      

      // Now get the combined net rotation matrix and net shift vector for XRT object i
      if ( iObj == 4 ) {
        AH_DEBUG << "Calculate net transforms" << std::endl;
      }
      nSteps = transforms.m_maxTransformXYZ;
      calcNetTransformation(nSteps, xrt2objectrotation, xrt2objectshift, netxrt2objectrotation, netxrt2objectshift, iObj);

      // ----- REVERSE TRANSFORMATIONS -----
      
      // Now do the reverse transformations: start from the last step, proceed 
      // in reverse order, changing the sign of each transformation, except for 
      // auxillary shifts and rotations

      // 1. Auxillary shift in –z direction for segment rotations
      object2xrtrotation[0] = unitRotationMatrix;
      object2xrtshift[0][2] = -1.0 * segmentRotationZOffset;
      
      // 2. z-axis segment rotation
      object2xrtrotation[1][2][2] = 1.0;
      object2xrtrotation[1][0][0] = cossegmentz;
      object2xrtrotation[1][0][1] = -1.0 * sinsegmentz;
      object2xrtrotation[1][1][0] = sinsegmentz;
      object2xrtrotation[1][1][1] = cossegmentz;

      // 3. y segment rotation
      object2xrtrotation[2][1][1] = 1.0;
      object2xrtrotation[2][0][0] = cossegmenty;
      object2xrtrotation[2][0][2] = sinsegmenty;
      object2xrtrotation[2][2][0] = -1.0 * sinsegmenty;
      object2xrtrotation[2][2][2] = cossegmenty;

      // 4. x segment rotation
      object2xrtrotation[3][0][0] = 1.0;
      object2xrtrotation[3][1][1] = cossegmentx;
      object2xrtrotation[3][1][2] = -1.0 * sinsegmentx;
      object2xrtrotation[3][2][1] = sinsegmentx;
      object2xrtrotation[3][2][2] = cossegmentx;

      // 5. Auxillary shift back up +z direction from z=0, no rotation in this step
      object2xrtrotation[4] = unitRotationMatrix;
      object2xrtshift[4][2] = 1.0 * segmentRotationZOffset;
              
      // 6. Segment shifts, sector radial shift, and housing shifts
      object2xrtrotation[5] = unitRotationMatrix;
      object2xrtshift[5][0] = -1.0 * xrt2objectshift[8][0];
      object2xrtshift[5][1] = -1.0 * xrt2objectshift[8][1];
      object2xrtshift[5][2] = -1.0 * xrt2objectshift[8][2];

      // 7. Auxillary shift along -z axis (no rotation)
      object2xrtrotation[6] = unitRotationMatrix;
      object2xrtshift[6][2] = -1.0 * pivotaxiszheight;

      // 8. Auxillary rotation back around z-axis clockwise (no shift)
//      object2xrtrotation[7][0][0] = cosmidwayphiangle;
//      object2xrtrotation[7][0][1] = -1.0 * sinmidwayphiangle;
//      object2xrtrotation[7][1][0] = sinmidwayphiangle;
//      object2xrtrotation[7][1][1] = cosmidwayphiangle;
//      object2xrtrotation[7][2][2] = 1.0;
      
      object2xrtrotation[7][0][0] = cosmidwayphiangle;
      object2xrtrotation[7][0][1] = sinmidwayphiangle;
      object2xrtrotation[7][1][0] = -1.0 * sinmidwayphiangle;
      object2xrtrotation[7][1][1] = cosmidwayphiangle;
      object2xrtrotation[7][2][2] = 1.0;

      // 9. Auxillary shift back along +x axis
      object2xrtrotation[8] = unitRotationMatrix;
      object2xrtshift[8][0] = -1.0 * radiusatpivot;

      // 10. Rotation by the tilt angle around z-axis (no shift)
      if (doTwist) {
        object2xrtrotation[9][0][0] = costwistangle;
        object2xrtrotation[9][0][1] = -1.0 * sintwistangle;
        object2xrtrotation[9][1][0] = sintwistangle;
        object2xrtrotation[9][1][1] = costwistangle;
        object2xrtrotation[9][2][2] = 1.0;
      } else {
        object2xrtrotation[9] = unitRotationMatrix;
      }
      
      // 11. Rotation by the tilt angle around y-axis (no shift)
      if (doTilt) {
        object2xrtrotation[10][0][0] = costiltangle;
        object2xrtrotation[10][0][2] = sintiltangle;
        object2xrtrotation[10][2][0] = -1.0 * sintiltangle;
        object2xrtrotation[10][2][2] = costiltangle;
        object2xrtrotation[10][1][1] = 1.0;
      } else {
        object2xrtrotation[10] = unitRotationMatrix;
      }
      
      // 12. Auxillary shift in the -x direction (no rotation)
      object2xrtrotation[11] = unitRotationMatrix;
      object2xrtshift[11][0] = radiusatpivot;

      // 13. Auxillary counter clockwise rotation around z axis (no shift)
//      object2xrtrotation[12][0][0] = cosmidwayphiangle;
//      object2xrtrotation[12][0][1] = sinmidwayphiangle;
//      object2xrtrotation[12][1][0] = -1.0 * sinmidwayphiangle;
//      object2xrtrotation[12][1][1] = cosmidwayphiangle;
//      object2xrtrotation[12][2][2] = 1.0;
      
      object2xrtrotation[12][0][0] = cosmidwayphiangle;
      object2xrtrotation[12][0][1] = -1.0 * sinmidwayphiangle;
      object2xrtrotation[12][1][0] = sinmidwayphiangle;
      object2xrtrotation[12][1][1] = cosmidwayphiangle;
      object2xrtrotation[12][2][2] = 1.0;

      // 14. Auxillary shift up +z axis (no rotation)
      object2xrtrotation[13] = unitRotationMatrix;
      object2xrtshift[13][2] = pivotaxiszheight;

      // Calculate net inverse transforms
      if ( iObj == 4 ) {
        AH_DEBUG << "Calculate net inverse transforms" << std::endl;
      }
      calcNetTransformation(nSteps, object2xrtrotation, object2xrtshift, netobject2xrtrotation, netobject2xrtshift, iObj);
      
      // Use steps 9-14 for sidewall arrays and calculate net transformations
      // Note that it is this subset of transformations that is to be used for 
      // obstructions if a more sophisticated approach is taken for them (as 
      // opposed to only applying housing shifts)
      for (int iStep = 0 ; iStep < numTrSteps ; ++iStep) { // Transferring total of 6 steps
        xrt2sidewallrotation[iStep] = xrt2objectrotation[iStep+8];
        xrt2sidewallshift[iStep]    = xrt2objectshift[iStep+8];
        sidewall2xrtrotation[iStep] = object2xrtrotation[iStep];
        sidewall2xrtshift[iStep]    = object2xrtshift[iStep];
      }

      // Combine the 6 steps into 1 (both directions, in turn)
      nSteps = numTrSteps;      
      calcNetTransformation(nSteps, xrt2sidewallrotation, xrt2sidewallshift, netxrt2sidewallrotation, netxrt2sidewallshift, iObj);
      calcNetTransformation(nSteps, sidewall2xrtrotation, sidewall2xrtshift, netsidewall2xrtrotation, netsidewall2xrtshift, iObj);
      
      // Transfer the results to xrt2objectframe, xrt2sectorframe, object2xrtframe, and sector2xrtframe
      for (int m = 0 ; m < numDim ; ++m) { 
        for (int n = 0 ; n < numDim ; ++n) {
          imx = (numDim * m) + n;
          transforms.m_xrt2objectframe[iObj][imx] = netxrt2objectrotation[m][n];
          transforms.m_xrt2sectorframe[iObj][imx] = netxrt2sidewallrotation[m][n];
          transforms.m_object2xrtframe[iObj][imx] = netobject2xrtrotation[m][n];
          transforms.m_sector2xrtframe[iObj][imx] = netsidewall2xrtrotation[m][n];
//          if (iObj == 7) {
//            AH_DEBUG << "netxrt2objectrotation["<<m<<"]["<<n<<"] = " << netxrt2objectrotation[m][n] << std::endl;
////            AH_DEBUG << "netobject2xrtrotation["<<m<<"]["<<n<<"] = " << netobject2xrtrotation[m][n] << std::endl;
//          }
        }
      }
            
//      if (iObj == 7) {
//        for (int m = 0 ; m < numDim ; ++m) { 
//          for (int n = 0 ; n < numDim ; ++n) {
////            AH_DEBUG << "netxrt2objectrotation["<<m<<"]["<<n<<"] = " << netxrt2objectrotation[m][n] << std::endl;
//            AH_DEBUG << "netobject2xrtrotation["<<m<<"]["<<n<<"] = " << netobject2xrtrotation[m][n] << std::endl;
//          }
//        }
//      }
      
      // copy net shifts into indices [9] through [11] in output
      for (int i = 0 ; i < numDim ; ++i) {  // +++ use a const instead of 9
        transforms.m_xrt2objectframe[iObj][i+9] = netxrt2objectshift[i];
        transforms.m_xrt2sectorframe[iObj][i+9] = netxrt2sidewallshift[i];
        transforms.m_object2xrtframe[iObj][i+9] = netobject2xrtshift[i];
        transforms.m_sector2xrtframe[iObj][i+9] = netsidewall2xrtshift[i];
      }
      
    } // end if-block OBSTRUCTION or FOIL
    
//    if (iObj == 7) {
//      for (int i = 0 ; i < 12 ; ++i) {
//        AH_DEBUG << "transforms.m_xrt2objectframe["<<iObj<<"]["<<i<<"] = " << transforms.m_xrt2objectframe[iObj][i] << std::endl;
//      }
//      for (int i = 0 ; i < 12 ; ++i) {
//        AH_DEBUG << "transforms.m_object2xrtframe["<<iObj<<"]["<<i<<"] = " << transforms.m_object2xrtframe[iObj][i] << std::endl;
//      }
//    }
    
  } // end for-loop through objects
  
  AH_DEBUG << "outside loop" << std::endl;
  for (uint i = 0 ; i < transforms.m_object2xrtframe[1].size() ; ++i) {
    AH_DEBUG << " object2xrtframe[1][i] = " << transforms.m_object2xrtframe[1][i] << std::endl;
  }
  AH_DEBUG << "outside loop" << std::endl;
  for (uint i = 0 ; i < transforms.m_object2xrtframe[4].size() ; ++i) {
    AH_DEBUG << " object2xrtframe[4][i] = " << transforms.m_object2xrtframe[4][i] << std::endl;
  }
  AH_DEBUG << "outside loop" << std::endl;
  for (uint i = 0 ; i < transforms.m_object2xrtframe[7].size() ; ++i) {
    AH_DEBUG << " object2xrtframe[7][i] = " << transforms.m_object2xrtframe[7][i] << std::endl;
  }
    
  
} // end setupXRTTransform()


/******************************************************************************/


void applyXRTTransform(transformType_e transformType, 
                       const CartesianCoord & coordIn,
                       const DirectionVector & dirVecIn,
                       const vector1Ddbl & xrtObjectTransform,
                       CartesianCoord & transformedCoord,
                       DirectionVector & transformedDirVec,
        long objIndex) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  const int numDim = 3;     // each rotation matrix is 3x3
  int index = 0;            // used in looping through matrices below
  
  // temporary arrays for transformed position and direction
  std::vector<double> rotatedPosition(numDim);
  std::vector<double> rotatedDirection(numDim);
  
  if (objIndex == 1) {
    for (uint i = 0 ; i < xrtObjectTransform.size() ; ++i) {
      AH_DEBUG << " object2xrtframe[1][i] = " << xrtObjectTransform[i] << std::endl;
    }
  }
  if (objIndex == 4) {
    for (uint i = 0 ; i < xrtObjectTransform.size() ; ++i) {
      AH_DEBUG << " object2xrtframe[4][i] = " << xrtObjectTransform[i] << std::endl;
    }
  }
  if (objIndex == 7) {
    for (uint i = 0 ; i < xrtObjectTransform.size() ; ++i) {
      AH_DEBUG << " object2xrtframe[7][i] = " << xrtObjectTransform[i] << std::endl;
    }
  }
  
  // -------------------------------------
  
  // transform position regardless of direction
  if ( (transformType == BOTH) || (transformType == POSITION) ) { 

    // apply rotation to the position
    for (int iRow = 0 ; iRow < numDim ; ++iRow) {
      index = numDim * iRow;
      //+++ have a check here to make sure xrtObjectTransform always has enough elements: no array out of bounds exceptions
      rotatedPosition[iRow] = ( (coordIn.m_x * xrtObjectTransform[index]) + 
                                 (coordIn.m_y * xrtObjectTransform[index+1]) + 
                                 (coordIn.m_z * xrtObjectTransform[index+2]) );
    }

    // apply shift to the position
    transformedCoord.m_x = rotatedPosition[0] + xrtObjectTransform[9];
    transformedCoord.m_y = rotatedPosition[1] + xrtObjectTransform[10];
    transformedCoord.m_z = rotatedPosition[2] + xrtObjectTransform[11];
    
  }

  // Apply rotation to the direction vector
  if ( (transformType == BOTH) || (transformType == DIRECTION) ) {
    
    for (int iRow = 0 ; iRow < numDim ; ++iRow) {
      index = numDim * iRow;
      rotatedDirection[iRow] = ( (dirVecIn.m_xDir * xrtObjectTransform[index]) + 
                                  (dirVecIn.m_yDir * xrtObjectTransform[index+1]) + 
                                  (dirVecIn.m_zDir * xrtObjectTransform[index+2]) );
    }
    
    transformedDirVec.m_xDir = rotatedDirection[0];
    transformedDirVec.m_yDir = rotatedDirection[1];
    transformedDirVec.m_zDir = rotatedDirection[2];
    
  }
  
} // end applyXRTTransform()


/******************************************************************************/


void calcNetTransformation(long numSteps, 
                           const vector3Ddbl & inputRotations,
                           const vector2Ddbl & inputShifts,
                           vector2Ddbl & netRotation, 
                           vector1Ddbl & netShift,
        
                           long objIndex) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int numDim = 3;     // each rotation matrix is 3x3
  
  vector3Ddbl cumulativeRotMatrix(numSteps);   // dimensions numSteps,3,3
  vector2Ddbl stepShifts(numSteps);            // dimensions numSteps,3
  for (long i = 0 ; i < numSteps ; ++i) {
    cumulativeRotMatrix[i].resize(numDim);
    stepShifts[i].resize(numDim);
    for (int j = 0 ; j < numDim ; ++j) {
      cumulativeRotMatrix[i][j].resize(numDim);
    }
  }
  
  vector2Ddbl mj(numDim);
  vector2Ddbl matrixproduct(numDim);
  vector2Ddbl cumulativematrixproduct(numDim);
  for (int i = 0 ; i < numDim ; ++i) {
    mj[i].resize(numDim);
    matrixproduct[i].resize(numDim);
    cumulativematrixproduct[i].resize(numDim);
  }
  
  // +++ resize, etc the output?
  
  // -------------------------------------
  
  // For NSTEPS steps we need the following cumulative matrix products:
  // Mnsteps Mnsteps-1...M2 M1,
  // Mnsteps Mnsteps-1...M3 etc., to
  // Mnsteps Mnsteps-1
  // Following will hold the cumulative matrix products; the first index value [0] will hold
  // Mnsteps Mnsteps-1...M2 M1 and the last will hold just Mnsteps
  
  // +++ vvvvvv old vvvvvv
//  for (long iStep = numSteps-1 ; iStep >= 0 ; iStep--) {
//    
//    if (iStep == (numSteps-1)) { 
//      //+++ is speed is critical here, we can take this part out of the 
//      //    for-loop, and just shorten the for-loop
//      for (int i = 0 ; i < numDim ; ++i) {
//        for (int j = 0 ; j < numDim ; ++j) {
//          cumulativeRotMatrix[iStep][i][j] = inputRotations[iStep][i][j];
//        }
//      }
//    } else {
//      // multiply cumulative matrix product, cumulativeRotMatrix, so far by the matrix in the next higher step
//      for (int i = 0 ; i < numDim ; ++i) {
//        for (int j = 0 ; j < numDim ; ++j) {
//          for (int k = 0 ; k < numDim ; ++k) { 
//            cumulativeRotMatrix[iStep][i][j] += 
//                                    (cumulativeRotMatrix[iStep+1][i][k] * 
//                                     inputRotations[iStep][k][j]);
//          }
//        }
//      }
//      
//    }
//    
//      if ( numSteps == 14 && objIndex == 4 ) {
//        AH_DEBUG << "iStep = " << iStep << std::endl;
//        for (int i = 0 ; i < numDim ; ++i) {
//              AH_DEBUG << cumulativeRotMatrix[iStep][i][0] << "\t " << cumulativeRotMatrix[iStep][i][1] << "\t " << cumulativeRotMatrix[iStep][i][2] << std::endl;
//          //    AH_DEBUG << " = " <<  << std::endl;
//        }
//      }
          
    // +++ ^^^^^ old ^^^^^
  
  
  
  // +++ vvvvvv new vvvvvv
  
  // +++ think of maybe a faster way to do this matrix multiplication
  
  for (int iStep = 0 ; iStep < numSteps ; ++iStep) {
  
    for (int j = iStep ; j < numSteps ; ++j) {
      mj = inputRotations[j];
      
      if (j == iStep) {
        cumulativematrixproduct = mj;
        
      } else {
        // multiply cumulative matrix product, cumulativematrixproduct, so far by the matrix in the next higher step
        
        // clear the array 
        for (int i = 0 ; i < numDim ; ++i) {
          for (int jj = 0 ; jj < numDim ; ++jj) {
            matrixproduct[i][jj] = 0.0;
//          std::fill(matrixproduct[i].begin(), matrixproduct[i].end(), 0.0);
          }
        }
        
        
//        if ( numSteps == 14 && objIndex == 4 ) {
//          AH_DEBUG << "iStep " << iStep << " j " << j << " printing mj" << std::endl;
//          for (int i = 0 ; i < numDim ; ++i) {
//                  AH_DEBUG << mj[i][0] << "\t " << 
//                              mj[i][1] << "\t " << 
//                              mj[i][2] 
//                              << std::endl;
//          }
//          AH_DEBUG << "printing cumulativematrixproduct" << std::endl;
//          for (int i = 0 ; i < numDim ; ++i) {
//                  AH_DEBUG << cumulativematrixproduct[i][0] << "\t " << 
//                              cumulativematrixproduct[i][1] << "\t " << 
//                              cumulativematrixproduct[i][2] 
//                              << std::endl;
//          }
//        }
        
        for (int m = 0 ; m < numDim ; ++m) {
          for (int i = 0 ; i < numDim ; ++i) {
            for (int n = 0 ; n < numDim ; ++n) {
              
//              if ( numSteps == 14 && objIndex == 4 ) {
//                AH_OUT << "before cumulativematrixproduct = matrixproduct" << 
//                            " matrixproduct["<<m<<"]["<<i<<"] = " << matrixproduct[m][i] << 
//                            " + ( mj["<<m<<"]["<<n<<"] = " << mj[m][n] << 
//                            " * cumulativematrixproduct["<<n<<"]["<<i<<"] = " << cumulativematrixproduct[n][i] << " )" << std::endl;
//              }
              
              matrixproduct[m][i] += ( mj[m][n] * cumulativematrixproduct[n][i] );
              
//              if ( numSteps == 14 && objIndex == 4 ) {
//                AH_OUT << "after cumulativematrixproduct = matrixproduct" << 
//                          " cumulativematrixproduct["<<n<<"]["<<i<<"] = " << cumulativematrixproduct[n][i] << std::endl;
//              }
              
              
            }
//            if ( numSteps == 14 && objIndex == 4 ) {
//              AH_DEBUG <<  << std::endl;
//            }
            
            
          }
        }
        
            
              cumulativematrixproduct = matrixproduct;
            
        
        
      }
      
    } // end-if
    cumulativeRotMatrix[iStep] = cumulativematrixproduct;
      
//    if ( numSteps == 14 && objIndex == 4 ) {
//      AH_DEBUG << "iStep " << iStep << std::endl;
//      for (int i = 0 ; i < numDim ; ++i) {
//        AH_DEBUG << cumulativeRotMatrix[iStep][i][0] << 
//                    "\t " << 
//                    cumulativeRotMatrix[iStep][i][1] << 
//                    "\t " << 
//                    cumulativeRotMatrix[iStep][i][2] 
//                    << std::endl;
//      }
//    }
    
    
    // +++ ^^^^^ new ^^^^^
  
  
    // Now we have cumulative matrix for step iStep: M_nsteps ...M_iStep+1 M_iStep. The 
    // contribution to the net shift is the product of this cumulative matrix 
    // and the vector shift for step iStep
    for (int i = 0 ; i < numDim ; ++i) {
      for (int j = 0 ; j < numDim ; ++j) {
        stepShifts[iStep][i] += (cumulativeRotMatrix[iStep][i][j] * inputShifts[iStep][j]);
      }
      netShift[i] += stepShifts[iStep][i];
    }

  }

  // The net rotation is simply the product of all of the individual rotation matrices
  for (int i = 0 ; i < numDim ; ++i) {
    for (int j = 0 ; j < numDim ; ++j) {
      netRotation[i][j] = cumulativeRotMatrix[0][i][j];
    }
      
//    if ( numSteps == 14 && objIndex == 4 ) {
//      AH_DEBUG << netRotation[i][0] << "\t " << netRotation[i][1] << "\t " << netRotation[i][2] << std::endl;
//    }
    
  }
//  if ( numSteps == 14 && objIndex == 4 ) {
//      AH_DEBUG << "testing" << std::endl;
//  }
  //    AH_DEBUG << " = " <<  << std::endl;
  
} // end calcNetTransformation()


/******************************************************************************/


void indexUniqueNames(const std::vector<std::string> namesIn, 
                      int firstIndex, int & numUniqueNames, 
                      std::vector<std::string> & uniqueNames, 
                      std::vector<int> & nameIndex) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  bool rowLogged = false;
  int numInput = namesIn.size();
  
  // -------------------------------------
  
  // if numUniqueNames is 0, create an array with 1 element, whose value is 
  // equal to the first value of the input array
  //+++ 20140429 KLR when would numUniqueNames = 0?
  if (numUniqueNames == 0) {
    // +++ 20140429 KLR make sure namesIn[0] is defined
    uniqueNames.push_back(namesIn[0]);
    numUniqueNames = 1;
  }
  
  nameIndex.resize(numInput);
  //+++ 20140429 KLR make sure this doesn't mess it up, if it's cumulative
  for (int i = 0 ; i < numInput ; ++i) {
    rowLogged = false;
    // check each existing element of uniqueNames to see if the ith element 
    // of namesIn already exists there. If it does, create the index and set a 
    // flag indicating that this has been done.
    for (int j = 0 ; j < numUniqueNames ; ++j) {
      if ( isEqualCaseInsens(namesIn[i], uniqueNames[j]) ) {
        nameIndex[i] = firstIndex + j;
        rowLogged = true;
      }
    }
    // If namesIn[i] did not already exist in the uniquenames array then add a 
    // new entry to uniqueNames and create the index.
    if (!rowLogged) {
      uniqueNames.push_back(namesIn[i]); 
      nameIndex[i] = firstIndex + numUniqueNames;
      numUniqueNames++;
    } 
  }

  
} // end indexUniqueNames()


/******************************************************************************/


void sourceLocalToAbsolutePos(double centerCosTheta, double centerSinTheta, 
                              double centerCosPhi, double centerSinPhi, 
                              double offsetTheta, double offsetPhi,
                              CartesianCoord & coordFinal) {
                                
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  double cosoffsettheta = std::cos(offsetTheta);
  double sinoffsettheta = std::sin(offsetTheta);
  double cosoffsetphi = std::cos(offsetPhi);
  double sinoffsetphi = std::sin(offsetPhi);
  
  double fac1  = sinoffsettheta* cosoffsetphi;  // +++ check this
  double fac11 = fac1*centerCosTheta;
  double fac2  = cosoffsettheta*centerSinTheta;
  double fac3  = sinoffsettheta*sinoffsetphi;

  // -------------------------------------
  
  // +++ 20140402 KLR just pass in theta and phi, instead of cos and sin of each?

  coordFinal.m_x = (fac11*centerCosPhi) + (fac2*centerCosPhi) - (fac3*centerSinPhi);
  coordFinal.m_y = (fac11*centerSinPhi) + (fac2*centerSinPhi) + (fac3*centerCosPhi);
  coordFinal.m_z = (cosoffsettheta*centerCosTheta) - (fac1*centerSinTheta);
  
} // end sourceLocalToAbsolutePos()


/******************************************************************************/


void getXYForNewZ(const CartesianCoord & coordIn, const DirectionVector & dirVec, 
                  CartesianCoord & coordOut, double & tImpact) {
  
  // if current position is maintained
  if ( (coordIn.m_z == coordOut.m_z) || (dirVec.m_zDir == 0.0) ) {
    coordOut.m_x = coordIn.m_x;
    coordOut.m_y = coordIn.m_y;
    tImpact = 0;
    return;
  }
  
  //+++ have a check in case I'm sending in newz=0 (if I forgot to initialize)
  
  // "time" to reach new z
  tImpact = (coordOut.m_z - coordIn.m_z) / dirVec.m_zDir;
  
  // new x and y obtained using vx and vy respectively for this "time"
  coordOut.m_x = coordIn.m_x + (dirVec.m_xDir * tImpact);
  coordOut.m_y = coordIn.m_y + (dirVec.m_yDir * tImpact);

} // end getXYForNewZ()


/******************************************************************************/


void sphericalToCartesianDir(double theta, double phi, 
                             DirectionVector & dirVec) {
                               
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------

  double sintheta = std::sin(theta);
  double costheta = std::cos(theta);
  double sinphi = std::sin(phi);
  double cosphi = std::cos(phi);

  // -------------------------------------
  
  dirVec.m_xDir = sintheta * cosphi;
  dirVec.m_yDir = sintheta * sinphi;
  dirVec.m_zDir = costheta;

} // end sphericalToCartesianDir()


/******************************************************************************/


void cartesianToPolar(const CartesianCoord & coordIn, 
                      double & radius, double & phi) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  double xoverr = 0.0;
  
  // initialize output variables
  radius = 0.0;
  phi = 0.0;
  
  // -------------------------------------
  
  // +++ is this correct???  it didn't like std::hypot
  radius = hypot(coordIn.m_x, coordIn.m_y);
  
  if (radius != 0.0) {
    xoverr = coordIn.m_x / radius;
    if (coordIn.m_y > 0.0) {
      phi = std::acos(xoverr);
    } else {
      phi = s_twopi - std::acos(xoverr);
    }
  } else {
    phi = 0.0;
  }
  
  // An angle of exactly 2pi is converted to 0 to avoid confusion in code
  phi = std::fmod(phi, s_twopi) ;
  
  // std::pair<double, double> => radius, phi
  // +++ return {std::hypot(x, y), std::atan2(y,x)};
  
} // end cartesianToPolar()




/**********************************************
 * ********************************************
 * 		writing output functions
 * ********************************************
**********************************************/


// FUNCTION NAME: writeEFFAreaExt
//
// CALLING SEQUENCE:
//   writeEFFAreaExt(param, eaExtNum, genTelescope, energyidstr, 
//          inputOffAxisThetaArcmin, currAzimuth, photons.m_photonEnergies, 
//          effareaslice); 
//
// PURPOSE:
//   Write an extension in the Effective area file (input param outeafile) 
//   for the effective area for this particular offaxis and azimuth.
//
// INPUTS:
//   param, eaExtNum, genTelescope, energyidstr, offaxis, azimuthangle, 
//   energies, effarea
//
// OUTPUTS:
//   none
//
// CALLED BY:
//   doWork()
//
// SUBROUTINES:
//   checkForFITSError(), writeCommonKeywords()
//
void writeEFFAreaExt(Param & param, 
                     int eaExtNum,
                     const GenTelescope & genTelescope, 
                     const std::string & energyidstr,
                     double offaxis, 
                     double azimuthangle, 
                     const vector1Ddbl & energies, 
                     const vector1Ddbl & effarea) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;             // for cfitsio calls
  
  const std::string effareafile = param.m_outeafile;
  
  // Create the name of the extension using the id numbers for the energy, offaxis angle and azimuthal angle
  std::string extName = "EFFAREA";
  extName.append(intToString(eaExtNum));
  
  // for creating the extension
  int tfields = 0;        // how many columns in the extension
  // the following define column names, formats, units; are keywords in header
  std::vector<std::string> ttype;       // column name in fits file
  std::vector<std::string> tform;       // data format for column in fits file
  std::vector<std::string> tunit;       // physical unit for column in fits file
  char ** c_ttype;        // convert ttype to c string for fits call
  char ** c_tform;        // convert tform to c string for fits call
  char ** c_tunit;        // convert tunit to c string for fits call
  
  // make non-const copies of vectors to write to file
  vector1Ddbl finalEnergies = energies;
  vector1Ddbl finalEFFArea = effarea;
  int numRows = finalEnergies.size();
  
  // -------------------------------------
  
  // create the column structures
  ttype.push_back("Energy");
  tform.push_back("1D");
  tunit.push_back("keV");
  ttype.push_back("EFFAREA");
  tform.push_back("1D");
  tunit.push_back("cm**2");
  
  // store how many columns we now have
  tfields = ttype.size();
  
  // copy the C++ strings into C char *s, to pass to cfitsio
  c_ttype = new char * [tfields];
  c_tform = new char * [tfields];
  c_tunit = new char * [tfields];
  for (int i = 0 ; i < tfields ; ++i) {
    c_ttype[i] = const_cast<char*>(ttype[i].c_str());
    c_tform[i] = const_cast<char*>(tform[i].c_str());
    c_tunit[i] = const_cast<char*>(tunit[i].c_str());
  }
  
  // create the extension
  fits_create_tbl(param.m_ea_fp, BINARY_TBL, 0, tfields, c_ttype, c_tform, c_tunit, extName.c_str(), &status);
  checkForFITSError(status, "creating EFFAREA extension in", effareafile);
  
  // move to the extension
  fits_movnam_hdu(param.m_ea_fp, ANY_HDU, const_cast<char *>(extName.c_str()), 0, &status);
  checkForFITSError(status, "moving to EFFAREA extension in", effareafile);
  
  // update the TTYPEn comments (for column names)
  fits_update_key_str(param.m_ea_fp, "TTYPE1", "Energy", "Energy", &status);
  fits_update_key_str(param.m_ea_fp, "TTYPE2", "EFFAREA", "Effective Area per energy", &status);
  
  // write the keywords for the extension
  fits_update_key_longstr(param.m_ea_fp, "TELESCOP", const_cast<char *>(genTelescope.m_telescop.c_str()), "Mission name", &status);
  fits_update_key_longstr(param.m_ea_fp, "INSTRUME", const_cast<char *>(genTelescope.m_instrume.c_str()), "Instrument name", &status);
  fits_update_key_longstr(param.m_ea_fp, "DETNAM",   const_cast<char *>(genTelescope.m_detnam.c_str()),   "Telescope name", &status);
  fits_update_key_str(param.m_ea_fp, "CVSD0001", const_cast<char *>(param.m_validdate.c_str()), const_cast<char *>("UTC date when file should be first used"), &status);
  fits_update_key_str(param.m_ea_fp, "CVST0001", const_cast<char *>(param.m_validtime.c_str()), const_cast<char *>("UTC time when file should be first used"), &status);
  fits_update_key_dbl(param.m_ea_fp, "OFFAXIS", offaxis, 6, "[arcmin] Source off-axis angle", &status);
  fits_update_key_dbl(param.m_ea_fp, "AZIMUTH", azimuthangle, 6, "[deg] Source rotational (azimuthal) angle", &status);
  fits_update_key_lng(param.m_ea_fp, "NUMPHOT", param.m_numphoton, "Number of photons per energy", &status);
  fits_update_key_str(param.m_ea_fp, "Energy", const_cast<char *>(energyidstr.c_str()), "[keV] Energy or energy range of image", &status); // +++ correct comment?
  checkForFITSError(status, "writing keywords in EFFAREA extension in", effareafile);
  
  // Write the energy and effective area columns
  fits_write_col(param.m_ea_fp, TDOUBLE, 1, 1, 1, numRows, &finalEnergies[0], &status);
  fits_write_col(param.m_ea_fp, TDOUBLE, 2, 1, 1, numRows, &finalEFFArea[0], &status);
  checkForFITSError(status, "writing data to EA extension in", effareafile);
  
  // reset the vectors and arrays
  delete [] c_ttype;    // +++ =0?
  delete [] c_tform;
  delete [] c_tunit;
  
  // stamp the input parameters
  writeCommonKeywords(param, &param.m_ea_fp);
  
} // end writeEFFAreaExt()


/******************************************************************************/


// FUNCTION NAME: writePSFType1Ext
//
// CALLING SEQUENCE:
//   writePSFType1Ext(param, psfExtNum, photons, genTelescope, 
//        plateScaleArcmin, energyidstr, inputOffAxisThetaArcmin, currAzimuth, 
//        totalpsfphotons, psf.m_psfx[j][k], psf.m_psfy[j][k], 
//        psf.m_psfLowerXCorner, psf.m_psfLowerYCorner, psf.m_delta_radeef, 
//        psf.m_psfImage); 
//
// PURPOSE:
//   Write an image extension in the PSF file (input param outpsffile) 
//   for this particular offaxis and azimuth.
//
// INPUTS:
//   param, psfExtNum, photons, genTelescope, plateScale, energyidstr, offaxis, azimuthangle, 
//   totalpsfphotons, psfxcen, psfycen, psfLowerXCorner, psfLowerYCorner, deltacoord, psfImage
//
// OUTPUTS:
//   none
//
// CALLED BY:
//   doWork()
//
// SUBROUTINES:
//   checkForFITSError(), writeCommonKeywords()
//
void writePSFType1Ext(Param & param, 
                      int psfExtNum,
                      const Photons & photons,
                      const GenTelescope & genTelescope, 
                      double plateScale, 
                      const std::string & energyidstr, 
                      double offaxis, 
                      double azimuthangle, 
                      long totalpsfphotons, 
                      double psfxcen, 
                      double psfycen, 
                      double psfLowerXCorner, 
                      double psfLowerYCorner, 
                      double deltacoord, 
                      const vector2Ddbl & psfImage) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;             // for cfitsio calls
  
  // Create the name of the extension
  std::string extName = "PSFIMG";
  extName.append(intToString(psfExtNum));
  
  const int numDec = 6;       // number of decimals to write doubles
  const int numDim = 2;       // will be a 2dim image
  
  // Note that currently numpsfyvals = numpsfxvals
  // Using a single value of psfImageCenterPixel for both X and Y requires this
  long numPSFxVals = psfImage.size();
  long numPSFyVals = psfImage[0].size();
  long numElem = numPSFxVals * numPSFyVals;
  
  long naxes[2] = {0};        // 2-element array with NAXIS1 and NAXIS2 values
  naxes[0] = numPSFxVals;     // NAXIS1
  naxes[1] = numPSFyVals;     // NAXIS2
  
  // we'll divide by a normalization factor
  long dividebytotalpsfphotons = 0;
  
  // final form of image that CFITSIO needs
  vector1Ddbl finalImg(numElem);
  
  double psfxcenToWrite = ( psfxcen == 0 ? psfxcen : psfxcen*(-1.0) );
  double psfycenToWrite = ( psfycen == 0 ? psfycen : psfycen*(-1.0) );
  
  // -------------------------------------
  
  // create new extension (fits moves us there automatically)
  fits_create_img(param.m_psf_fp, DOUBLE_IMG, numDim, naxes, &status);
  checkForFITSError(status, "creating image extension in", param.m_outpsffile);
  
  // +++ is this only way to name this extension, by fits_update_key_str?
  fits_update_key_str(param.m_psf_fp, "EXTNAME", const_cast<char *>(extName.c_str()), "Extension name", &status);
  fits_update_key_longstr(param.m_psf_fp, "TELESCOP", const_cast<char *>(genTelescope.m_telescop.c_str()), "Mission name", &status);
  fits_update_key_longstr(param.m_psf_fp, "INSTRUME", const_cast<char *>(genTelescope.m_instrume.c_str()), "Instrument name", &status);
  fits_update_key_longstr(param.m_psf_fp, "DETNAM",   const_cast<char *>(genTelescope.m_detnam.c_str()),   "Telescope name", &status);
  fits_update_key_str(param.m_psf_fp, "CVSD0001", const_cast<char *>(param.m_validdate.c_str()), const_cast<char *>("UTC date when file should be first used"), &status);
  fits_update_key_str(param.m_psf_fp, "CVST0001", const_cast<char *>(param.m_validtime.c_str()), const_cast<char *>("UTC time when file should be first used"), &status);
  fits_update_key_dbl(param.m_psf_fp, "OFFAXIS", offaxis, numDec, "[arcmin] Source off-axis angle", &status);
  fits_update_key_dbl(param.m_psf_fp, "AZIMUTH", azimuthangle, numDec, "[deg] Source rotational (azimuthal) angle", &status);
  fits_update_key_str(param.m_psf_fp, "Energy", const_cast<char *>(energyidstr.c_str()), "[keV] Energy or energy range of image", &status);
  fits_update_key_lng(param.m_psf_fp, "TOTCTS", totalpsfphotons, "Total pixel counts in image", &status);
  fits_update_key_dbl(param.m_psf_fp, "FPMM2AM", 1/plateScale, numDec, "Focal Plane (mm) to arcminutes scale factor", &status);
  fits_update_key_dbl(param.m_psf_fp, "XCENTER", psfxcenToWrite, numDec, "Source center X-coordinate", &status);
  fits_update_key_dbl(param.m_psf_fp, "YCENTER", psfycenToWrite, numDec, "Source center Y-coordinate", &status);
  fits_update_key_str(param.m_psf_fp, "CTYPE1", "X", "Source of X-axis", &status);
  fits_update_key_lng(param.m_psf_fp, "CRPIX1", 1, "X-axis reference pixel", &status);
  fits_update_key_dbl(param.m_psf_fp, "CRVAL1", psfLowerXCorner, numDec, "X-coordinate at the reference pixel", &status);
  fits_update_key_dbl(param.m_psf_fp, "CDELT1", deltacoord, numDec, "X-axis increment", &status);
  fits_update_key_str(param.m_psf_fp, "CUNIT1", "arcsec", "X-axis unit", &status);
  fits_update_key_str(param.m_psf_fp, "CTYPE2", "Y", "Source of Y-axis", &status);
  fits_update_key_lng(param.m_psf_fp, "CRPIX2", 1, "Y-axis reference pixel", &status);
  fits_update_key_dbl(param.m_psf_fp, "CRVAL2", psfLowerYCorner, numDec, "Y-coordinate at the reference pixel", &status);
  fits_update_key_dbl(param.m_psf_fp, "CDELT2", deltacoord, numDec, "Y-axis increment", &status);
  fits_update_key_str(param.m_psf_fp, "CUNIT2", "arcsec", "Y-axis unit", &status);
  checkForFITSError(status, "writing keywords in PSF extension in", param.m_outpsffile);
  
  // put the psf image into a 1D array, normalized by totalpsfphotons
  // only divide by totalpsfphotons if it's not 0
  dividebytotalpsfphotons = ( (totalpsfphotons>0) ? totalpsfphotons : 1);
  long kk = 0;
  for (long ii = 0 ; ii < numPSFxVals ; ++ii) {
    for (long jj = 0 ; jj < numPSFyVals ; ++jj) {
      kk = ii + jj*numPSFyVals;
      finalImg[kk] = psfImage[ii][jj] / dividebytotalpsfphotons;
    }
  }
  
  // Write the image
  fits_write_img(param.m_psf_fp, TDOUBLE, 1, numElem, &finalImg[0], &status);
  checkForFITSError(status, "creating PSF image extension in", param.m_outpsffile);
  
  // if this is the primary extension, write a few extra keywords
  if (psfExtNum == 1) {
    writePrimaryExtKeywords(&param.m_psf_fp, param.m_outpsffile, photons);
  }
  
  // stamp the input parameters
  writeCommonKeywords(param, &param.m_psf_fp);
  
} // end writePSFType1Ext()


/******************************************************************************/


// FUNCTION NAME: writePSFType2Ext
//
// CALLING SEQUENCE:
//   writePSFType2Ext(param, psfExtNum, genTelescope, plateScaleArcmin, 
//        energyidstr, inputOffAxisThetaArcmin, currAzimuth, totalpsfphotons, 
//        psf.m_psfx[j][k], psf.m_psfy[j][k], psf.m_radeef, psf.m_delta_radeef, 
//        totaleefenergy, eefSlice); 
//
// PURPOSE:
//   Write a bintable extension (EEF) in the PSF file (input param outpsffile) 
//   for this particular offaxis and azimuth.
//
// INPUTS:
//   param, psfExtNum, genTelescope, plateScale, energyidstr, offaxis, 
//   azimuthangle, totalpsfphotons, psfxcen, psfycen, radeef, delta_radeef, 
//   totaleefenergy, eef
//
// OUTPUTS:
//   none
//
// CALLED BY:
//   doWork()
//
// SUBROUTINES:
//   checkForFITSError(), writeCommonKeywords()
//
void writePSFType2Ext(Param & param, 
                      int psfExtNum,
                      const GenTelescope & genTelescope, 
                      double plateScale, 
                      const std::string & energyidstr, 
                      double offaxis, 
                      double azimuthangle, 
                      long totalpsfphotons, 
                      double psfxcen, 
                      double psfycen, 
                      vector1Ddbl radeef, 
                      double delta_radeef, 
                      double totaleefenergy, 
                      const vector1Ddbl & eef) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;             // for cfitsio calls
  
  const int numDec = 6;       // number of decimals to write doubles
  
  // Create the name of the extension using the id numbers for the energy, 
  // offaxis angle and azimuthal angle (but make them 1-based)
  std::string extName = "EEF";
  extName.append(intToString(psfExtNum));
  
  // for creating the extension
  int tfields = 0;        // how many columns in the extension
  // the following define column names, formats, units; are keywords in header
  std::vector<std::string> ttype;       // column name in fits file
  std::vector<std::string> tform;       // data format for column in fits file
  std::vector<std::string> tunit;       // physical unit for column in fits file
  char ** c_ttype;        // convert ttype to c string for fits call
  char ** c_tform;        // convert tform to c string for fits call
  char ** c_tunit;        // convert tunit to c string for fits call
  
  
  // normalize the EEF by totaleefenergy
  int numRows = eef.size();
  vector1Ddbl finalEEF(numRows);
  
  for (int i = 0 ; i < numRows ; ++i) {
    finalEEF[i] = eef[i] / totaleefenergy;
  }
  
  double psfxcenToWrite = ( psfxcen == 0 ? psfxcen : psfxcen*(-1.0) );
  double psfycenToWrite = ( psfycen == 0 ? psfycen : psfycen*(-1.0) );
  
  // +++ this should work with bind2nd
//  std::transform(finalEEF.begin(), finalEEF.end(), finalEEF.begin(),
//                 std::bind2nd(std::divides<double>(), totaleefenergy));
  
  // -------------------------------------
  
  // create the column structures
  ttype.push_back("Radius");
  tform.push_back("1D");
  tunit.push_back("arcsec");
  ttype.push_back("EEF");
  tform.push_back("1D");
  
  // store how many columns we now have
  tfields = ttype.size();
  // make the tunit vector the same size
  tunit.resize(tfields);
  
  // copy the C++ strings into C char *s, to pass to cfitsio
  c_ttype = new char * [tfields];
  c_tform = new char * [tfields];
  c_tunit = new char * [tfields];
  for (int i = 0 ; i < tfields ; ++i) {
    c_ttype[i] = const_cast<char*>(ttype[i].c_str());
    c_tform[i] = const_cast<char*>(tform[i].c_str());
    c_tunit[i] = const_cast<char*>(tunit[i].c_str());
  }
  
  // create the extension
  fits_create_tbl(param.m_psf_fp, BINARY_TBL, 0, tfields, c_ttype, c_tform, c_tunit, extName.c_str(), &status);
  checkForFITSError(status, "creating EEF extension in", param.m_outpsffile);
  
  // move to the extension
  fits_movnam_hdu(param.m_psf_fp, ANY_HDU, const_cast<char *>(extName.c_str()), 0, &status);
  checkForFITSError(status, "moving to EEF extension in", param.m_outpsffile);
  
  // +++ write a subfunction for these, since it's done in psftype1 also?
  
  // update the TTYPEn comments (for column names)
  fits_update_key_str(param.m_psf_fp, "TTYPE1", "Radius", "Enclosed Radius", &status);
  fits_update_key_str(param.m_psf_fp, "TTYPE2", "EEF", "Enclosed Energy Function", &status);
  
  // write the keywords for the extension
  fits_update_key_longstr(param.m_psf_fp, "TELESCOP", const_cast<char *>(genTelescope.m_telescop.c_str()), "Mission name", &status);
  fits_update_key_longstr(param.m_psf_fp, "INSTRUME", const_cast<char *>(genTelescope.m_instrume.c_str()), "Instrument name", &status);
  fits_update_key_longstr(param.m_psf_fp, "DETNAM",   const_cast<char *>(genTelescope.m_detnam.c_str()),   "Telescope name", &status);
  fits_update_key_str(param.m_psf_fp, "CVSD0001", const_cast<char *>(param.m_validdate.c_str()), const_cast<char *>("UTC date when file should be first used"), &status);
  fits_update_key_str(param.m_psf_fp, "CVST0001", const_cast<char *>(param.m_validtime.c_str()), const_cast<char *>("UTC time when file should be first used"), &status);
  fits_update_key_dbl(param.m_psf_fp, "OFFAXIS", offaxis, numDec, "[arcmin] Source off-axis angle", &status);
  fits_update_key_dbl(param.m_psf_fp, "AZIMUTH", azimuthangle, numDec, "[deg] Source rotational (azimuthal) angle", &status);
  fits_update_key_str(param.m_psf_fp, "Energy", const_cast<char *>(energyidstr.c_str()), "[keV] Energy or energy range of image", &status);
  fits_update_key_lng(param.m_psf_fp, "TOTCTS", totalpsfphotons, "Total number of photons in EEF", &status);
  fits_update_key_dbl(param.m_psf_fp, "FPMM2AM", 1/plateScale, numDec, "Focal Plane (mm) to arcminutes scale factor", &status);
  fits_update_key_dbl(param.m_psf_fp, "XCENTER", psfxcenToWrite, numDec, "Source center X-coordinate", &status);
  fits_update_key_dbl(param.m_psf_fp, "YCENTER", psfycenToWrite, numDec, "Source center Y-coordinate", &status);
  fits_update_key_str(param.m_psf_fp, "1CTYP1", "Radial distance", "Radial distance from peak", &status);
  fits_update_key_str(param.m_psf_fp, "1CUNI1", "arcsec", "Units of radial distance from peak", &status);
  fits_update_key_lng(param.m_psf_fp, "1CRPX1", 1, "reference pixel corresponding to start radius", &status);
  fits_update_key_dbl(param.m_psf_fp, "1CRVL1", 0.0, 6, "Radial distance at the reference pixel", &status);
  fits_update_key_dbl(param.m_psf_fp, "1CDLT1", delta_radeef, numDec, "Radial distance increment per pixel", &status);
  checkForFITSError(status, "writing keywords in EEF extension in", param.m_outpsffile);
  
  // Write the radial distance column and the (normalized) EEF column.
  fits_write_col(param.m_psf_fp, TDOUBLE, 1, 1, 1, numRows, &radeef[0], &status);
  fits_write_col(param.m_psf_fp, TDOUBLE, 2, 1, 1, numRows, &finalEEF[0], &status);
  checkForFITSError(status, "writing data to EEF extension in", param.m_outpsffile);
  
  // reset the vectors and arrays
  delete [] c_ttype;    // +++ =0?
  delete [] c_tform;
  delete [] c_tunit;
  
  // stamp the input parameters
  writeCommonKeywords(param, &param.m_psf_fp);
  
} // end writePSFType2Ext()


/******************************************************************************/


// FUNCTION NAME: writeHistoryFileHeader
//
// CALLING SEQUENCE:
//   writeHistoryFileHeader(param, stats, photons, genTelescope, 
//        genTelescope.m_focalLength, plateScaleArcmin, 
//        aperture.m_fullGeometricAreaSqmm, aperture.m_geometricAreaSqcm, 
//        aperture.m_ztelescopeaperture, aperture.m_isAnnulus, 
//        genTelescope.m_innerhousingradius, genTelescope.m_outerhousingradius, 
//        transforms.m_doTransforms, photons.m_numPhotonEnergies, photoncount);
//
// PURPOSE:
//   Write the header keywords for the main extension (PHOTONHISTORY) of the 
//   history file.
//
// INPUTS:
//   param, stats, photons, genTelescope, focalLength, plateScale, 
//   fullGeometricArea, apertureGeometricArea, ztelescopeaperture, 
//   apertureIsAnnulus, innerHousingRadius, outerHousingRadius, doTransforms, 
//   numPhotonEnergies, photonCountIn
//
// OUTPUTS:
//   none
//
// CALLED BY:
//   doWork()
//
// SUBROUTINES:
//   checkForFITSError(), writeCommonKeywords()
//
void writeHistoryFileHeader(Param & param, 
                            const StatResults & stats, 
                            const Photons & photons, 
                            const GenTelescope & genTelescope, 
                            double focalLength, 
                            double plateScale, 
                            double fullGeometricArea, 
                            double apertureGeometricArea, 
                            double ztelescopeaperture, 
                            bool apertureIsAnnulus, 
                            double innerHousingRadius, 
                            double outerHousingRadius, 
                            bool doTransforms, 
                            long numPhotonEnergies, 
                            long photonCountIn) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;             // for cfitsio calls
  
  const int numDec = 6;       // number of decimals to write doubles
  
  // Convert the total number of input photons (photonCountIn) to type double
  // so that fractional keywords below are written as doubles
  double photonCount = photonCountIn;
  
  long numPhotonsToWrite = 0;
  // +++ comment
  if (photons.m_photonsFromFile) {
    numPhotonsToWrite = photons.m_nphotons;
  } else {
    numPhotonsToWrite = param.m_numphoton;
  }
  
  std::string dotrnsfm = (doTransforms ? "YES" : "NO");
  
  // get file names (without paths) of input files
  std::string tdfn      = getFilename(param.m_mirrorfile);
  std::string rtfn      = getFilename(param.m_frontreffile);
  std::string scfn      = getFilename(param.m_scatterfile);
  std::string phfn      = getFilename(param.m_outphistfile);
  std::string eafile    = getFilename(param.m_outeafile);
  std::string psffile   = getFilename(param.m_outpsffile);
  std::string psrcfile  = getFilename(param.m_psrcfile);
  
  // -------------------------------------
  
  // write the keywords for the extension
  fits_update_key_longstr(param.m_history_fp, "TELESCOP", const_cast<char *>(genTelescope.m_telescop.c_str()), "Mission name", &status);
  fits_update_key_longstr(param.m_history_fp, "INSTRUME", const_cast<char *>(genTelescope.m_instrume.c_str()), "Instrument name", &status);
  fits_update_key_longstr(param.m_history_fp, "DETNAM",   const_cast<char *>(genTelescope.m_detnam.c_str()),   "Telescope name", &status);
  
  fits_update_key_str(param.m_history_fp, "CVSD0001", const_cast<char *>(param.m_validdate.c_str()), const_cast<char *>("UTC date when file should be first used"), &status);
  fits_update_key_str(param.m_history_fp, "CVST0001", const_cast<char *>(param.m_validtime.c_str()), const_cast<char *>("UTC time when file should be first used"), &status);
  
  // these coordinates are currently not selectable by the user
  fits_update_key_dbl(param.m_history_fp, "SFPXC", 0.0, numDec, "[mm] Focal Plane X Center", &status);
  fits_update_key_dbl(param.m_history_fp, "SFPYC", 0.0, numDec, "[mm] Focal Plane Y Center", &status);
  
  fits_update_key_dbl(param.m_history_fp, "DFLMM", focalLength, numDec, "[mm] Design Focal length", &status);
  fits_update_key_dbl(param.m_history_fp, "FPMM2AM", 1/plateScale, numDec, "Focal Plane (mm) to arcminutes scale factor", &status);

  fits_update_key_dbl(param.m_history_fp, "FGEOAREA", fullGeometricArea/100.0, numDec, "[cm**2] Full telescope geometric area", &status);
  fits_update_key_dbl(param.m_history_fp, "GEOMAREA", apertureGeometricArea, numDec, "[cm**2] Telescope aperture geometric area for this run", &status);
  fits_update_key_dbl(param.m_history_fp, "ZAPRTURE", ztelescopeaperture, numDec, "z-coordinate of telescope aperture", &status);

  fits_update_key_longstr(param.m_history_fp, "TDFN", const_cast<char *>(tdfn.c_str()), 0, &status);
  fits_write_comment(param.m_history_fp, "TDFN: Telescope Description filename", &status);
  fits_update_key_longstr(param.m_history_fp, "RTFN", const_cast<char *>(rtfn.c_str()), 0, &status);
  fits_write_comment(param.m_history_fp, "RTFN: Reflectivity and transmission table filename", &status);
  fits_update_key_longstr(param.m_history_fp, "SCFN", const_cast<char *>(scfn.c_str()), 0, &status);
  fits_write_comment(param.m_history_fp, "SCFN: Scattering profiles filename", &status);
  
  fits_update_key_longstr(param.m_history_fp, "ENERGIES", const_cast<char *>((param.m_energy).c_str()), "Energy grid input filename or energy list", &status);
  fits_update_key_lng(param.m_history_fp, "NUMPHOT", numPhotonsToWrite, "Input photons for each E, off-axis, roll", &status);
  fits_update_key_lng(param.m_history_fp, "TOTNPHOT", photonCountIn, "Total number of input photons", &status);
  
  AH_DEBUG << "NUMPHOT = " << numPhotonsToWrite << std::endl;
  AH_DEBUG << "TOTNPHOT = " << photonCountIn << std::endl;
  
  fits_update_key_lng(param.m_history_fp, "RANSEED", param.m_seed, "Random number seed", &status);
  
  fits_update_key_str(param.m_history_fp, "TRANMODE", const_cast<char *>((param.m_transmode).c_str()), "XRT transmission treatment option", &status);
  fits_update_key_str(param.m_history_fp, "SCATMODE", const_cast<char *>((param.m_scattermode).c_str()), "XRT scattering treatment option", &status);
  fits_update_key_str(param.m_history_fp, "DOTRNSFM", const_cast<char *>(dotrnsfm.c_str()), "Misalignment offsets & rotations on?", &status);
  fits_update_key_str(param.m_history_fp, "SRCMDL", const_cast<char *>((param.m_source).c_str()), "Photon source model selected for this run", &status);
  
  if ( isEqualCaseInsens(param.m_source, "betamodel") ) {
    fits_update_key_str(param.m_history_fp, "BETAPARS", const_cast<char *>((param.m_betapars).c_str()), "core radius (arcmin), beta index, max radius (arcmin)", &status);
  } else if ( isEqualCaseInsens(param.m_source, "flatcircle") ) {
    fits_update_key_dbl(param.m_history_fp, "FLATRAD", param.m_flatradius, numDec, "[arcmin] Radius of source in FLATCIRCLE model", &status);
  } else if ( isEqualCaseInsens(param.m_source, "photonlist") || isEqualCaseInsens(param.m_source, "groundmodel") ) {
    fits_update_key_str(param.m_history_fp, "SRCFILE", const_cast<char *>(psrcfile.c_str()), "File containing input photon attributes", &status);
  } else if ( isEqualCaseInsens(param.m_source, "diagnosticmode") ) {
    fits_update_key_str(param.m_history_fp, "DIAGPARS", const_cast<char *>((param.m_diagpars).c_str()), "Input parameters for diagnostic mode", &status);
  }
  
  if (!photons.m_photonsFromFile) {
    if (photons.m_nthetain == 1) {
      fits_update_key_dbl(param.m_history_fp, "OFFAXIS", param.m_offaxisVec[0], numDec, "[arcmin] Photon source center off-axis angle", &status);
    }
    if (photons.m_nrollin == 1) {
      fits_update_key_dbl(param.m_history_fp, "AZIMUTH", param.m_rollVec[0], numDec, "[deg] Photon source center rotation angle", &status);
    }
  }
    
  if (apertureIsAnnulus) {
    fits_update_key_str(param.m_history_fp, "APERTURE", "Annulus", "Aperture shape (Annulus/Rectangle)", &status);
    fits_update_key_str(param.m_history_fp, "ANNUPARS", const_cast<char *>((param.m_annulus).c_str()), "Annular aperture parameters", &status);
  } else {
    fits_update_key_str(param.m_history_fp, "APERTURE", "Rectangle", "Aperture shape (Annulus/Rectangle)", &status);
    fits_update_key_str(param.m_history_fp, "RECTPARS", const_cast<char *>((param.m_rectangle).c_str()), "Rectangular aperture parameters", &status);
  }
  
  fits_update_key_str(param.m_history_fp, "PHFN", const_cast<char *>(phfn.c_str()), 0, &status);
  fits_write_comment(param.m_history_fp, "PHFN: Photon History output file name from xrtraytrace", &status);
  fits_update_key_str(param.m_history_fp, "EAFILE", const_cast<char *>(eafile.c_str()), 0, &status);
  fits_write_comment(param.m_history_fp, "EAFILE: Effective area output file name from xrtraytrace", &status);
  fits_update_key_str(param.m_history_fp, "PSFFILE", const_cast<char *>(psffile.c_str()), 0, &status);
  fits_write_comment(param.m_history_fp, "PSFFILE: PSF output file name from xrtraytrace", &status);
  fits_update_key_str(param.m_history_fp, "PSFPARS", const_cast<char *>((param.m_psfpars).c_str()), "PSF parmaters", &status);
  
  fits_update_key_dbl(param.m_history_fp, "RESLTSPZ", param.m_resultsplanez, numDec, "Results plane z-coordinate (0.0=focal plane)", &status);
  
  fits_update_key_str(param.m_history_fp, "RSLPONLY", (param.m_resplaneonly ? "Yes" : "No"), "Only results-plane events or not", &status);
  fits_update_key_str(param.m_history_fp, "PHSTTYPE", const_cast<char *>((param.m_phisttype).c_str()), "Type of history file (full/basic/brief)", &status);
  fits_update_key_str(param.m_history_fp, "EXTNOBJS", const_cast<char *>((param.m_externobjects).c_str()), "Which external XRT objects included", &status);
  fits_update_key_str(param.m_history_fp, "FASTMODE", (param.m_doFastMode ? "Yes" : "No"), "Raytracing run in fastmode or not", &status);
  
  fits_update_key_dbl(param.m_history_fp, "PMINRAD", innerHousingRadius, numDec, "[mm] Inner housing unit radius", &status);
  fits_update_key_dbl(param.m_history_fp, "PMAXRAD", outerHousingRadius, numDec, "[mm] Outer housing unit radius", &status);
  
  fits_write_comment(param.m_history_fp, "Following keywords that begin with N are photon numbers", &status);
  fits_write_comment(param.m_history_fp, "Following kewords that begin with F are photon numbers as a fraction of", &status);
  fits_write_comment(param.m_history_fp, "the total number of input photons TOTNPHOT", &status);
  fits_write_comment(param.m_history_fp, "RP == Results Plane (focal plane by default, at z=0)", &status);

  fits_update_key_lng(param.m_history_fp, "NRESPLN", stats.numAllResultsPlaneImpacts, "Total impacts on results plane (RP)", &status);
  fits_update_key_dbl(param.m_history_fp, "FRESPLN", stats.numAllResultsPlaneImpacts/photonCount, numDec, "Fractional number of impacts on RP", &status);
  fits_update_key_lng(param.m_history_fp, "NNZRSPLN", stats.numresultsplaneimpacts, "RP impacts with no error (zero error flag)", &status);
  fits_update_key_dbl(param.m_history_fp, "MNEFAREA", apertureGeometricArea * stats.numrpiDoubleRefl / (photonCount) , numDec, "Mean eff area cm^2 dbl refl. for all energies", &status);
  fits_update_key_lng(param.m_history_fp, "NPERROR", stats.numrpiphwitherror, "RP impacts with non-zero error flag", &status);
  
  // From here we only write keywords if not in fastmode
  // +++ if superfastmode
  if (!param.m_doSuperFastMode) {
  
    // No front-side reflections on any mirror
    fits_update_key_lng(param.m_history_fp, "NZFMREFL", stats.numrpiNoFrontRefl, "RP impacts with no front-side reflection", &status);
    fits_update_key_dbl(param.m_history_fp, "FZFMREFL", stats.numrpiNoFrontRefl/photonCount, numDec, "Fractional RP impacts with no front-side refl.", &status);

    // One primary and one secondary reflection (including other reflections and transmissions in the path).  Use this for effective area
    fits_update_key_lng(param.m_history_fp, "NDBLREFL", stats.numrpiDoubleRefl, "RP imp. front refl. 1 pri, 1 sec", &status);
    fits_update_key_dbl(param.m_history_fp, "FDBLREFL", stats.numrpiDoubleRefl/photonCount, numDec, "Frac. RP imp. front refl. 1 pri, 1 sec", &status);
    
    // One primary and one secondary reflection (and no other reflection)
    fits_update_key_lng(param.m_history_fp, "N1P1SFRL", stats.numrpiPriSecRefl, "RP imp. front refl. 1 pri, 1 sec, 0 pcol", &status);
    fits_update_key_dbl(param.m_history_fp, "F1P1SFRL", stats.numrpiPriSecRefl/photonCount, numDec, "Frac. RP front-side, 2 refl. 1 pri + 1 sec", &status);

    // Primary reflection and no secondary
    fits_update_key_lng(param.m_history_fp, "N1PFONLY", stats.numrpiPrimaryRefl, "RP impacts with 1 refl. (front-side primary)", &status);
    fits_update_key_dbl(param.m_history_fp, "F1PFONLY", stats.numrpiPrimaryRefl/photonCount, numDec, "Frac. RP impacts with 1 refl. (front-side pri)", &status);

    // Secondary reflection and no primary
    fits_update_key_lng(param.m_history_fp, "N1SFONLY", stats.numrpiSecondaryRefl, "RP impacts with 1 refl. (front-side secondary)", &status);
    fits_update_key_dbl(param.m_history_fp, "F1SFONLY", stats.numrpiSecondaryRefl/photonCount, numDec, "Frac. RP impacts, 1 refl. (front-side sec)", &status);

    // Only one mirror impact (primary or secondary)
    fits_update_key_lng(param.m_history_fp, "N1MFONLY", stats.numrpiPrimaryRefl + stats.numrpiSecondaryRefl, "RP impacts 1 front-side refl. only, pri or sec", &status);
    fits_update_key_dbl(param.m_history_fp, "F1MFONLY", (stats.numrpiPrimaryRefl + stats.numrpiSecondaryRefl) / photonCount, numDec, "Frac RP, 1 front-side refl. only, pri or sec", &status);

    // More than two front-side mirror reflections
    fits_update_key_lng(param.m_history_fp, "NMFGT2", stats.numrpiMoreThanTwoRefl, "RP imp. with > 2 front-side refl., pri/sec", &status);
    fits_update_key_dbl(param.m_history_fp, "FMFGT2", stats.numrpiMoreThanTwoRefl/photonCount, numDec, "Frac. RP, with > 2 front-side refl. pri/sec", &status);

    // Back-side Mirror Reflections
    fits_update_key_lng(param.m_history_fp, "N1MBREFL", stats.numrpiBackSideOneRefl, "1 reflection on back-sides of mirrors ", &status);
    fits_update_key_dbl(param.m_history_fp, "F1MBREFL", stats.numrpiBackSideOneRefl/photonCount, numDec, "Frac. with 1 refl. on back-sides of mirrors", &status);
    fits_update_key_lng(param.m_history_fp, "NMGT2BRF", stats.numrpiBackSideMoreThanOne, "Number with >1 reflection on mirror back-sides ", &status);
    fits_update_key_dbl(param.m_history_fp, "FMGT2BRF", stats.numrpiBackSideMoreThanOne/photonCount, numDec, "Frac. with >1 reflection on mirror back-sides", &status);

    // Impacts on mirror edges (tops and sides)
    fits_update_key_lng(param.m_history_fp, "NMEDGE", stats.numMirrorEdgeImpact, "Number with impacts on a mirror edge", &status);
    fits_update_key_dbl(param.m_history_fp, "FMEDGE", stats.numMirrorEdgeImpact/photonCount, numDec, "Fraction with impacts on a mirror edge", &status);

    // Pre-collimator reflections
    fits_update_key_lng(param.m_history_fp, "NCREFLZM", stats.numrpiPreColReflNoMirror, "RP impacts & refl. on p-col, no mirror refl.", &status);
    fits_update_key_dbl(param.m_history_fp, "FCREFLZM", stats.numrpiPreColReflNoMirror/photonCount, numDec, "Frac. RP impacts & refl. on p-col, not mirror", &status);
    fits_update_key_lng(param.m_history_fp, "NCREFL1M", stats.numrpiPreColReflOneMirror, "RP imp. & refl. on p-col, 1 front mirror refl.", &status);
    fits_update_key_dbl(param.m_history_fp, "FCREFL1M", stats.numrpiPreColReflOneMirror/photonCount, numDec, "Frac. RP with p-col & 1 front mirror refl.", &status);
    fits_update_key_lng(param.m_history_fp, "NCREFL2M", stats.numrpiPreColReflTwoMirror, "RP imp. & refl. on p-col, 2 front mirror refl.", &status);
    fits_update_key_dbl(param.m_history_fp, "FCREFL2M", stats.numrpiPreColReflTwoMirror/photonCount, numDec, "Frac. RP with p-col & 2 front mirror refl.", &status);
    fits_update_key_lng(param.m_history_fp, "NCRFGT2M", stats.numrpiPreColReflMoreThanTwoMirror, "RP imp. & p-col refl. & > 2 front mirror refl.", &status);
    fits_update_key_dbl(param.m_history_fp, "FCRFGT2M", stats.numrpiPreColReflMoreThanTwoMirror/photonCount, numDec, "Frac. RP & p-col refl., > 2 front mirror refl.", &status);

    // Impacts on pre-collimator edges (tops or sides)
    fits_update_key_lng(param.m_history_fp, "NCEDGE", stats.numPreColEdgeImpact, "Impacts on a pre-collimator edge", &status);
    fits_update_key_dbl(param.m_history_fp, "FCEDGE", stats.numPreColEdgeImpact/photonCount, numDec, "Fraction with impacts on a pre-collimator edge", &status);

    // Scattering after reflection
    fits_update_key_lng(param.m_history_fp, "NSCATTER", stats.numrpiScatter, "RP impacts with scattering", &status);
    fits_update_key_dbl(param.m_history_fp, "FSCATTER", stats.numrpiScatter/photonCount, numDec, "Fraction with RP impacts with scattering", &status);
    fits_update_key_lng(param.m_history_fp, "NFRNTSCT", stats.numrpiFrntMScatter, "RP imp. with scattering & front mirror refl.", &status);
    fits_update_key_dbl(param.m_history_fp, "FFRNTSCT", stats.numrpiFrntMScatter/photonCount, numDec, "Frac. RP with scattering & front mirror refl.", &status);

    // Transmission
    fits_update_key_lng(param.m_history_fp, "NGE1TRAN", stats.numTransmission, "Number with at least 1 transmission event", &status);
    fits_update_key_dbl(param.m_history_fp, "FGE1TRAN", stats.numTransmission/photonCount, numDec, "Frac. with at least 1 transmission event", &status);
    fits_update_key_lng(param.m_history_fp, "NRPGE1TR", stats.numrpiTransmission, "RP impacts at least 1 trans. event", &status);
    fits_update_key_dbl(param.m_history_fp, "FRPGE1TR", stats.numrpiTransmission/photonCount, numDec, "Fraction RP impacts at least 1 trans. event", &status);
    fits_update_key_lng(param.m_history_fp, "NMIRTRAN", stats.numMirrorTransmission, "At least 1 mirror transmission event", &status);
    fits_update_key_dbl(param.m_history_fp, "FMIRTRAN", stats.numMirrorTransmission/photonCount, numDec, "Frac. with at least 1 mirror transmission event", &status);
    fits_update_key_lng(param.m_history_fp, "NMRPTRAN", stats.numrpiMirrorTransmission, "RP impacts with at least 1 trans. event", &status);
    fits_update_key_dbl(param.m_history_fp, "FMRPTRAN", stats.numrpiMirrorTransmission/photonCount, numDec, "Fraction RP least 1 mirror transmission event", &status);
    fits_update_key_lng(param.m_history_fp, "NCOLTRAN", stats.numPreColTransmission, "Number with >=1 p-col transmission event", &status);
    fits_update_key_dbl(param.m_history_fp, "FCOLTRAN", stats.numPreColTransmission/photonCount, numDec, "Fraction with >= 1 p-col transmission event", &status);
    fits_update_key_lng(param.m_history_fp, "NCRPTRAN", stats.numrpiPreColTransmission, "RP impacts with >=1 p-col transmission event", &status);
    fits_update_key_dbl(param.m_history_fp, "FCRPTRAN", stats.numrpiPreColTransmission/photonCount, numDec, "Frac. RP with >= 1 p-col trans. event", &status);

    // Total Absorption
    fits_update_key_lng(param.m_history_fp, "NTOTABSD", stats.numTotalAbsorbed, "Total number of photons absorbed", &status);
    fits_update_key_dbl(param.m_history_fp, "FTOTABSD", stats.numTotalAbsorbed/photonCount, numDec, "Fraction photons absorbed", &status);

    // Impacts on the telescope support structure
    fits_update_key_lng(param.m_history_fp, "NABOBSTR", stats.numObstructionAbsorbed, "Absorbed in a support structure", &status);
    fits_update_key_dbl(param.m_history_fp, "FABOBSTR", stats.numObstructionAbsorbed/photonCount, numDec, "Fraction absorbed in a support structure", &status);

    // Impacts on sector side walls
    fits_update_key_lng(param.m_history_fp, "NABSDWLL", stats.numSectorWallAbsorbed, "Number that impacted a sector side wall", &status);
    fits_update_key_dbl(param.m_history_fp, "FABSDWLL", stats.numSectorWallAbsorbed/photonCount, numDec, "Fraction that impacted a sector side wall", &status);

    // Impacts on the telescope housing
    fits_update_key_lng(param.m_history_fp, "NINRHSNG", stats.numInnerHousingAbsorbed, "Number of photon impacts on the inner housing", &status);
    fits_update_key_dbl(param.m_history_fp, "FINRHSNG", stats.numInnerHousingAbsorbed/photonCount, numDec, "Fraction with impacts on the inner housing", &status);
    fits_update_key_lng(param.m_history_fp, "NOUTHSNG", stats.numOuterHousingAbsorbed, "Number of photon impacts on the outer housing", &status);
    fits_update_key_dbl(param.m_history_fp, "FOUTHSNG", stats.numOuterHousingAbsorbed/photonCount, numDec, "Fraction with impacts on the outer housing", &status);

    // Absorption after impact on the front side of a mirror
    fits_update_key_lng(param.m_history_fp, "NABFRNTM", stats.numFrontMirrorAbsorbed, "Absorbed on the front-side of mirrors", &status);
    fits_update_key_dbl(param.m_history_fp, "FABFRNTM", stats.numFrontMirrorAbsorbed/photonCount, numDec, "Fraction absorbed on the front-side of mirrors", &status);

    // Absorption after impact on the back side of a mirror
    fits_update_key_lng(param.m_history_fp, "NABBACKM", stats.numBackMirrorAbsorbed, "Absorbed on the back-side of mirrors", &status);
    fits_update_key_dbl(param.m_history_fp, "FABBACKM", stats.numBackMirrorAbsorbed/photonCount, numDec, "Fraction absorbed on the back-side of mirrors", &status);

    // Absorption after impact on a mirror-foil edge (top or sides)
    fits_update_key_lng(param.m_history_fp, "NABEDGEM", stats.numEdgeMirrorAbsorbed, "Absorbed after impacting mirror edge", &status);
    fits_update_key_dbl(param.m_history_fp, "FABEDGEM", stats.numEdgeMirrorAbsorbed/photonCount, numDec, "Fraction absorbed after impacting mirror edge", &status);

    // Absorption after impact on a pre-collimator blade front or back
    fits_update_key_lng(param.m_history_fp, "NABPCOLL", stats.numPreColFaceAbsorbed, "Absorbed on back or front of p-col", &status);
    fits_update_key_dbl(param.m_history_fp, "FABPCOLL", stats.numPreColFaceAbsorbed/photonCount, numDec, "Fraction absorbed on back or front of p-col", &status);

    // Absorption after impact on a pre-collimator blade edge (top or sides)
    fits_update_key_lng(param.m_history_fp, "NABEDGEC", stats.numPreColEdgeImpactAbsorbed, "Absorbed on pre-collimator blade edge", &status);
    fits_update_key_dbl(param.m_history_fp, "FABEDGEC", stats.numPreColEdgeImpactAbsorbed/photonCount, numDec, "Fraction absorbed on pre-collimator blade edge", &status);

  }
  
  checkForFITSError(status, "writing keywords to header in", "history file");
  
  // stamp the input parameters
  writeCommonKeywords(param, &param.m_history_fp);
  
  AH_DEBUG << "end of writeHistoryFileHeader()" << std::endl;
  
} // end writeHistoryFileHeader()


/******************************************************************************/


void writeHistoryFile2ndExtension(Param & param, 
                                  Photons & photons,
                                  const GenTelescope & genTelescope) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;             // for cfitsio calls
  
  const std::string filename = getFilename(param.m_outphistfile);   
  
  long numPhotonsLong = param.m_numphoton;
  
  // for creating the extension
  int tfields = 0;            // how many columns in the extension
  // the following define column names, formats, units; are keywords in header
  std::vector<std::string> ttype;       // column name in fits file
  std::vector<std::string> tform;       // data format for column in fits file
  std::vector<std::string> tunit;       // physical unit for column in fits file
  char ** c_ttype;            // convert ttype to c string for fits call
  char ** c_tform;            // convert tform to c string for fits call
  char ** c_tunit;            // convert tunit to c string for fits call
  
  long iRow = 0;              // for looping through extension rows
  
  // -------------------------------------
  
  // create the column structures
  ttype.push_back("INITIALTHETA");
  tform.push_back("1D");
  tunit.push_back("arcmin");
  ttype.push_back("INITIALAZIMDIR");
  tform.push_back("1D");
  tunit.push_back("deg");
  ttype.push_back("ENERGY");
  tform.push_back("1D");
  tunit.push_back("keV");
  ttype.push_back("NUMPHOTONS");
  tform.push_back("1J");
  
  // store how many columns we now have
  tfields = ttype.size();
  // make the tunit vector the same size
  tunit.resize(tfields);
  
  // copy the C++ strings into C char *s, to pass to cfitsio
  c_ttype = new char * [tfields];
  c_tform = new char * [tfields];
  c_tunit = new char * [tfields];
  for (int i = 0 ; i < tfields ; ++i) {
    c_ttype[i] = const_cast<char*>(ttype[i].c_str());
    c_tform[i] = const_cast<char*>(tform[i].c_str());
    c_tunit[i] = const_cast<char*>(tunit[i].c_str());
  }
  
  // create the extension
  fits_create_tbl(param.m_history_fp, BINARY_TBL, 0, tfields, c_ttype, c_tform, c_tunit, s_historyExtName2.c_str(), &status);
  checkForFITSError(status, "creating 2nd extension in", filename);
  
  // move to the extension
  fits_movnam_hdu(param.m_history_fp, ANY_HDU, const_cast<char *>(s_historyExtName2.c_str()), 0, &status);
  checkForFITSError(status, "moving to 2nd extension in", filename);
  
  // write the header keywords:
  fits_update_key_longstr(param.m_history_fp, "TELESCOP", const_cast<char *>(genTelescope.m_telescop.c_str()), "Mission name", &status);
  fits_update_key_longstr(param.m_history_fp, "INSTRUME", const_cast<char *>(genTelescope.m_instrume.c_str()), "Instrument name", &status);
  fits_update_key_longstr(param.m_history_fp, "DETNAM",   const_cast<char *>(genTelescope.m_detnam.c_str()),   "Telescope name", &status);
  fits_update_key_lng(param.m_history_fp, "NOFFAXIS", photons.m_nthetain, "Number photon source center off-axis angles", &status);
  fits_update_key_lng(param.m_history_fp, "NAZIMUTH", photons.m_nrollin, "Number photon source center rotation angles", &status);
  fits_update_key_lng(param.m_history_fp, "NUMENRG",  photons.m_numPhotonEnergies, "Number unique photon energies", &status);
  checkForFITSError(status, "writing keywords to header in 2nd extension of", filename);
  
  // update comments for TTYPEn keywords (column name keywords)
  fits_update_key_str(param.m_history_fp, "TTYPE1", "INITIALTHETA", "Intial photon direction off-axis angle", &status);
  fits_update_key_str(param.m_history_fp, "TTYPE2", "INITIALAZIMDIR", "Intial photon direction azimuthal angle", &status);
  fits_update_key_str(param.m_history_fp, "TTYPE3", "ENERGY", "Photon energy", &status);
  fits_update_key_str(param.m_history_fp, "TTYPE4", "NUMPHOTONS", "Number of photons traced for this row", &status);
  
  AH_DEBUG << "m_numPhotonEnergies = " << photons.m_numPhotonEnergies << std::endl;
    
  // loop over off-axis (theta) angle
  for (int iTheta = 0 ; iTheta < photons.m_nthetain ; ++iTheta) {
    // loop over azimuthal (or roll) angle
    for (int iRoll = 0 ; iRoll < photons.m_nrollin ; ++iRoll) {
      
      // If we are doing only diagonal theta,phi values, skip this iRoll loop 
      // if it does not form a diagonal pair
      if ((param.m_doThetaPhiPairs) && (iTheta != iRoll)) { 
        continue;
      }
      
      // loop over energies
      for (int iEnergy = 0 ; iEnergy < photons.m_numPhotonEnergies ; ++iEnergy) {
        iRow++;
        // Write 4 columns
        fits_write_col_dbl(param.m_history_fp, 1, iRow, 1, 1, &(photons.m_offaxisThetaArcmin[iTheta]), &status);
        fits_write_col_dbl(param.m_history_fp, 2, iRow, 1, 1, &(photons.m_rollAnglesDeg[iRoll]), &status);
        fits_write_col_dbl(param.m_history_fp, 3, iRow, 1, 1, &(photons.m_photonEnergies[iEnergy]), &status);
        fits_write_col_lng(param.m_history_fp, 4, iRow, 1, 1, &numPhotonsLong, &status);
        checkForFITSError(status, "writing data to 2nd extension in", filename);
      }
    }
  }
  
  // reset the vectors and arrays
  delete [] c_ttype;    // +++ =0?
  delete [] c_tform;
  delete [] c_tunit;
  
  // stamp the input parameters
  writeCommonKeywords(param, &param.m_history_fp);
  
} // end writeHistoryFile2ndExtension()


/******************************************************************************/


void writeCommonKeywords(Param & param, fitsfile ** fitsfp) {
  
  int status = 0;             // for cfitsio calls
  int history = 0;
  
  //+++ fits_file_name(fitsfile *fptr, > char *filename, int *status);
  
  // add a version number to the output, so we know which version of the code produced this file
  fits_update_key_str(*fitsfp, "VERSION", "v1.10.2", "Version number of xrtraytrace code", &status);
  checkForFITSError(status, "writing VERSION keyword in", "output file"); 
  
  // add the date this HDU was created
  fits_write_date(*fitsfp, &status);
  checkForFITSError(status, "writing DATE keyword in", "output file");
  
  // if history parameter was set, write all the input parameters to header
  get_history(&history);
  if (history) {
    // note: 2nd argument = 0 means to stamp the active HDU
    HDpar_stamp(*fitsfp, 0, &status);
    checkForFITSError(status, "writing param history keywords to", "output file");
  }
  
  // write the DATASUM and CHECKSUM keywords to file
  fits_write_chksum(*fitsfp, &status);
  checkForFITSError(status, "writing DATASUM and CHECKSUM keywords in", "output file");
  
} // end writeCommonKeywords()


/******************************************************************************/


/// \brief Write keywords to the primary extension header.
/// \param[in] fitsfp Pointer to a FITS file pointer.
/// \param[in] photons
/// 
/// Write some keywords to the primary extension header.  No data will be 
/// written to the primary extension. The header and data for each extension 
/// was written prior to this function.  This is for the PSF and EA files.
void writePrimaryExtKeywords(fitsfile ** fitsfp,
                          const std::string & filename,
                          const Photons & photons) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;                       // for cfitsio function calls
  
  const int numDec = 6;                 // number of decimals to write doubles
  const double normRad = 1.0e30;        // normalization radius: keyword NORMRAD
  
  // -------------------------------------
  
//  fits_file_name(*fitsfp, > char *filename, &status);
  
  // Write some keywords to the primary extension header.  No data will be 
  // written to the primary extension. The header and data for each extension 
  // was written prior to this function. (except psf type 1 which is an image)
  fits_movabs_hdu(*fitsfp, 1, 0, &status);
  checkForFITSError(status, "moving to primary extension in", filename);
  
  fits_update_key_lng(*fitsfp, "NOFFAXIS", photons.m_nthetain, "Number off-axis angles", &status);
  fits_update_key_lng(*fitsfp, "NAZIMUTH", photons.m_nrollin, "Number azimuthal angles", &status);
  fits_update_key_dbl(*fitsfp, "NORMRAD",  normRad, numDec, "Normalization radius", &status);
  // NOTE: this normalization radius effectively indicates that it is infinite 
  // but downstream software can create a similar file with a meaningful value.
  checkForFITSError(status, "writing keywords to primary extension in", filename); 
  
} // end writePrimaryExtKeywords()



/**********************************************
 * ********************************************
 * 		getPar*() functions
 * ********************************************
**********************************************/


std::string reportGetParErr(const std::string & parname, int status) {
  std::stringstream msg;
  msg << "problem getting parameter: " << parname << " (APE status: " 
      << status << ")";
  return msg.str();
}


std::string getParString(const std::string & parname) {
  char* t_par;
  if (int status = ape_trad_query_string(parname.c_str(),&t_par) != eOK )
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  std::string out = (std::string)t_par;
  free(t_par); t_par=0;
  return out;
}


bool getParBool(const std::string & parname) {
  char t_par;
  if (int status = ape_trad_query_bool(parname.c_str(),&t_par) != eOK)
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  if (t_par != 0) return true;
  return false;
}


double getParDouble(const std::string & parname) {
  double out;
  if (int status = ape_trad_query_double(parname.c_str(),&out) != eOK)
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  return out;
}


int getParInt(const std::string & parname) {
  int out;
  if (int status = ape_trad_query_int(parname.c_str(),&out) != eOK)
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  return out;
}


std::string convertCharToString(char * input) {
  std::string out = (std::string)input;
  free(input); input=0;
  return out;
}


bool isInputFileGiven(const std::string & infile) {
  bool fileIsGiven = true;
  if (isEqualCaseInsens(infile, "none") || infile.empty())
    fileIsGiven = false;
  return fileIsGiven;
}


void listStringsToDoubles(const std::string & stringList, vector1Ddbl & doubleList) {
  
  double tempDouble;
  std::string errorMsg;
  std::string::size_type begin = stringList.find_first_not_of(" \t\n");
  std::string::size_type end;
  
  // empty out the input array
  doubleList.clear();
  
  // loop through the input string which should be a list
  while (std::string::npos != begin) {
    
    // get the position of to the first space, comma, tab, or newline
    end = stringList.find_first_of(" ,\t\n", begin);

    std::istringstream ss(stringList.substr(begin, end - begin));
    if ( (ss >> tempDouble) && (ss >> std::ws).eof() ) {
      doubleList.push_back(tempDouble);
    } else {
      errorMsg = "There was an error converting input parameter '";
      errorMsg.append(stringList);
      errorMsg.append("' to a list of real numbers. If this is supposed to be a filename, verify that the file exists.");
      AH_THROW_RUNTIME(errorMsg);
    }
    
    // now get the starting position of the next non-space character
    begin = stringList.find_first_not_of(" ,\t\n", end);
    
  }

} // end listStringsToDoubles()



std::string resolve(const std::string & filename,
                    const std::string & filetype,
                    const std::string & instrume,
                    const std::string & detnam,
                    const std::string & codename,
                    const std::string & datetime,
                    const std::string & expression,
                    const std::string & telescop) {
  
  // initialize the return variable to the input filename. If it's not CALDB,
  // this is what will be returned.  
  std::string filenameFull = filename;
  
  // these will only be updated if the user passed in valid a datetime
  std::string querydate = "-";
  std::string querytime = "-";
  
  if (isEqualCaseInsens(filename, "CALDB")) {
    
    AH_INFO(ahlog::HIGH) << "Searching CALDB for "<<filetype<<" file" << std::endl;
    
    // check if relevant environment variables are set
    if (getenv("CALDB") == NULL) {
      AH_THROW_RUNTIME("$CALDB environment variable not defined");
    }
    if (getenv("CALDBCONFIG") == NULL) {
      AH_THROW_RUNTIME("$CALDBCONFIG environment variable not defined");
    }
    if (getenv("CALDBALIAS") == NULL) {
      AH_THROW_RUNTIME("$CALDBALIAS environment variable not defined");
    } 
    
    // Log location of user's CALDB 
    AH_INFO(ahlog::LOW) << "$CALDB="<<getenv("CALDB") << std::endl;
    AH_INFO(ahlog::LOW) << "$CALDBCONFIG="<<getenv("CALDBCONFIG") << std::endl;
    AH_INFO(ahlog::LOW) << "$CALDBALIAS="<<getenv("CALDBALIAS") << std::endl;

    if (datetime.length() > 1) {
      //get date from datatime string
      querydate = datetime.substr(0,10);
      //get time in hh:mm:ss format from 
      querytime = datetime.substr(11,8);
    }
    
    const char* tele      = telescop.c_str();
    const char* instr     = instrume.c_str();
    const char* det       = detnam.c_str();
    const char* filt      = "-";
    const char* codenam   = codename.c_str();
    const char* strtdate  = querydate.c_str();
    const char* strttime  = querytime.c_str();
    const char* stpdate   = "-";
    const char* stptime   = "-";
    const char* expr      = expression.c_str();

    const int fnamesize   = 256;    // max size of filename
    const int onlinesize  = 10;     // size of online 
    int maxret            = 1;
    char filename[fnamesize];
    char* fnptr           = filename;
    long extno            = 0;
    char online[onlinesize];
    char* onptr           = online;
    int nret              = 0;
    int nfound            = 0;
    int status            = 0;
    
    AH_INFO(ahlog::HIGH) << "Querying CALDB with these parameters: " << std::endl; 
    AH_INFO(ahlog::HIGH) << "   TELESCOPE:  "  << tele << std::endl;
    AH_INFO(ahlog::HIGH) << "   INSTRUMENT: "  << instr << std::endl;
    AH_INFO(ahlog::HIGH) << "   DETNAM:     "  << det << std::endl;
    AH_INFO(ahlog::HIGH) << "   FILTER:     "  << filt << std::endl;
    AH_INFO(ahlog::HIGH) << "   CODENAME:   "  << codenam << std::endl;
    AH_INFO(ahlog::HIGH) << "   START DATE: "  << strtdate << std::endl;
    AH_INFO(ahlog::HIGH) << "   START TIME: "  << strttime << std::endl;
    AH_INFO(ahlog::HIGH) << "   STOP DATE:  "  << stpdate << std::endl;
    AH_INFO(ahlog::HIGH) << "   STOP TIME:  "  << stptime << std::endl;
    AH_INFO(ahlog::HIGH) << "   EXPRESSION: "  << expr << std::endl;

    HDgtcalf(tele,instr,det,filt,codenam,strtdate,strttime,
             stpdate,stptime,expr,maxret,fnamesize,&fnptr,&extno,
             &onptr,&nret,&nfound,&status);

    if (status != 0) {
      std::stringstream serr;
      serr << "Could not get "<<filetype<<" file from CALDB; "
           << "HDgtcalf status: " << status;
      AH_THROW_RUNTIME(serr.str());
    }

    if (nfound == 0) {
      AH_THROW_RUNTIME("No "+filetype+" file found in CALDB");
    }
    
    // include extended syntax, extension number in string to be returned
    std::stringstream tmp;
    tmp << filename << "[" << extno << "]";
    filenameFull = tmp.str();

    AH_INFO(ahlog::HIGH) << "CALDB " << filetype << " file: " << filenameFull << std::endl;
    
  } // end-if CALDB
   
  return filenameFull;

} // end resolve()





/**********************************************
 * ********************************************
* 		random number generator functions
 * ********************************************
**********************************************/

// +++ 20140402 KLR make sure these work as expected (seeding, with time or a
//                  number; freeing; btwn 0 and 1; gaussian; etc)

HDmt_state * & getRandSeedState() {
  static HDmt_state *state=0;
  return state;
}


void seedRandom(unsigned long int seedIn) {
  unsigned long int seed = seedIn;
  // if the user passed in 0, then use the system time
  if (0 == seedIn) {
    // copied from DR's code for simulator
    struct timeval tt;      // time structure
    unsigned int time_s;    // time in seconds
    unsigned int time_us;   // time in micro seconds

    gettimeofday(&tt, NULL);   // get the system time
    time_s = tt.tv_sec;        // time in seconds
    time_us = tt.tv_usec;      // time in micro seconds
    seed = time_s + time_us;   // combine into powerfully random seed
  }
  getRandSeedState() = HDmt_srand(seed);
}


double getRandom(void) {
  return HDmt_drand(getRandSeedState());
}


double getRandomGaussian(void) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  double mean   = 0.0;
  double stddev = 1.0;
  double x      = 0.0;
  double y      = 0.0;
  double rsq    = 0.0;
  double d      = 0.0;
  double n1     = 0.0;
  
  // -------------------------------------
  
  do {
    x = 2.0 * getRandom() - 1.0;
    y = 2.0 * getRandom() - 1.0;
    rsq = x*x + y*y;
  } while (rsq == 0.0 || rsq > 1.0);
  d = std::sqrt(-2.0 * log(rsq) / rsq);
  n1 = x * d;
  return n1 * stddev + mean;
  
}


void freeRandom(void) {
  HDmt_destroy_state(getRandSeedState());
}


/**********************************************
 * ********************************************
* 		misc functions
 * ********************************************
**********************************************/

// +++ for all of these, I should have error checking that vector has elements

double getMinDouble(std::vector<double> & vec) {
  std::vector<double>::iterator iter;
  iter = std::min_element(vec.begin(), vec.end());
  return *iter;
}
double getMaxDouble(std::vector<double> & vec) {
  std::vector<double>::iterator iter;
  iter = std::max_element(vec.begin(), vec.end());
  return *iter;
}
int getMinInt(std::vector<int> & vec) {
  std::vector<int>::iterator iter;
  iter = std::min_element(vec.begin(), vec.end());
  return *iter;
}
int getMaxInt(std::vector<int> & vec) {
  std::vector<int>::iterator iter;
  iter = std::max_element(vec.begin(), vec.end());
  return *iter;
}
long getMinLong(std::vector<long> & vec) {
  std::vector<long>::iterator iter;
  iter = std::min_element(vec.begin(), vec.end());
  return *iter;
}
long getMaxLong(std::vector<long> & vec) {
  std::vector<long>::iterator iter;
  iter = std::max_element(vec.begin(), vec.end());
  return *iter;
}


bool doublesAreEqual(double inA, double inB, double maxRelDiff) {
  
  double A = std::abs(inA);
  double B = std::abs(inB);
  double largest = (B > A) ? B : A;
  double diff = std::abs(A - B);
  
  if (diff <= largest * maxRelDiff) {
    return true;
  }
  return false; 
  
}


std::string doubleToString(double value) {
	std::ostringstream os;
	os << value;
	return os.str();
}

std::string intToString(int value) {
	std::ostringstream os;
	os << value;
	return os.str();
}

std::string longToString(long value) {
	std::ostringstream os;
	os << value;
	return os.str();
}


bool isEqualCaseInsens(const std::string & str1, const std::string & str2) {
   // check that they're the same length
   if ( str1.length() != str2.length() ) {
     return false;
   }
   // now check for equality
   for (unsigned int i = 0 ; i < str1.length() ; ++i) {
    if ( toupper(str1.at(i)) != toupper(str2.at(i)) )
      return false;
  }
  // if we made it out of the loop, they must be equal
  return true;
}

bool foundCaseInsens(const std::string & mainStr, const std::string & subStr) {
  
  // first, convert both strings to uppercase
  // the types of strings in this program are column names, keywords, etc:
  // all relatively short strings.  So converting to uppercase isn't a big deal
  std::string mainUpper;
  std::string subUpper;
  transform(mainStr.begin(), mainStr.end(), std::back_inserter(mainUpper), toupper);
  transform(subStr.begin(), subStr.end(), std::back_inserter(subUpper), toupper);
  
  return (mainUpper.find(subUpper) != std::string::npos);
  
} // end foundCaseInsens()


std::string getFilename(const std::string & str) {
  // look for the last delimeter of a file path
  int found = str.find_last_of("/\\");
  // return only the filename, at the end of the file
  return str.substr(found+1);
}


void checkForFITSError(const int status, const std::string & doing, const std::string & filename) {
  
  // the stream to hold the message
  std::stringstream msg;
  
  /*  
  // if it was a success, and we're in debug mode, print a message to log
  if (status == 0 && ahlog::get_debug()) {
    std::cout << TOOLNAME << " debug: FITSIO success while " << doing 
              << " file " << filename << std::endl;
  }*/
 
  // if returned status from the fits call isn't zero, abort with a message
  if(status != 0) {
    msg << TOOLNAME << " error: FITSIO error while " << doing << " file " << filename;
    fits_report_error(stdout,status);
    AH_THROW_RUNTIME(msg.str());
  }
  
} // end checkForFITSError()


st_stream::OStream & operator<<(st_stream::OStream & os, const CartesianCoord & coord) {
  std::stringstream msg;
  msg.str("");
  msg << std::setprecision(16) << "x: " << coord.m_x << " y: " << coord.m_y << " z: " << coord.m_z;
  os << msg.str();
  return os;
}
st_stream::OStream & operator<<(st_stream::OStream & os, const DirectionVector & vec) {
  std::stringstream msg;
  msg.str("");
  msg << std::setprecision(16) << "xDir: " << vec.m_xDir << " yDir: " << vec.m_yDir << " zDir: " << vec.m_zDir;
  os << msg.str();
  return os;
}

void writeParametersToLog() {
  char ** par_names = 0;
  int ii = 0;
  std::stringstream out;

  // start output line with name of tool
  char toolname[128];          // 128 is the size used in headas_toolname.c
  get_toolname(toolname);
  out << toolname;

  // write parameters to output line
  ape_trad_get_par_names(&par_names);
  while(par_names[ii] != NULL){
    char* value=0;
    ape_trad_get_string(par_names[ii],&value);
    out << " '" << par_names[ii] << "=" << value << "'";
    free(value);
    ii++;
  }

  // Write parameter list to log file
  AH_INFO(ahlog::HIGH) << "START PARAMETER LIST:" << std::endl;
  AH_INFO(ahlog::HIGH) << out.str() << std::endl;
  AH_INFO(ahlog::HIGH) << "END PARAMETER LIST" << std::endl << std::endl;

  // Clean up parameter list array
  ape_util_free_string_array(par_names);

} // end of writeParametersToLog


/* Revision Log
 $Log: xrtraytrace_lib.cxx,v $
 Revision 1.35  2017/01/18 15:43:50  mdutka
 Correcting sign error with final y position

 Revision 1.34  2017/01/13 13:45:19  mdutka
 Adding bug correction for final x and final y position

 Revision 1.33  2016/11/10 20:32:33  mdutka
 Xrtraytrace will now account for rotations in the telescop definition file

 Revision 1.32  2016/04/07 21:03:09  rshill
 Parameter dump to log changed to command line format.

 Revision 1.31  2016/03/25 18:00:00  klrutkow
 added fits_write_chksum

 Revision 1.30  2015/12/07 21:14:51  klrutkow
 interceptXRTObjects(): add back in the if-block and break when looking at zmin ; getImpactCandidates(): add photonZMin to conditionals ; udpated version number

 Revision 1.29  2015/11/02 17:34:46  klrutkow
 xrtSetup(): fix typo to use pcol scattering column instead of mirror scattering ; raytraceOnePhoton(): commented out block moving photon to next z-level ; interceptXRTObjects(): commented out block to break out of loop using zlevel ; getSurfaceNormal(): take out sign error when calculating normal vector ; change version to 1.09.1

 Revision 1.28  2015/10/01 17:22:56  klrutkow
 updated version from 1.08 to 1.09

 Revision 1.27  2015/09/29 21:14:16  klrutkow
 interceptXRTObjects: update radial bound check btwn photon and object

 Revision 1.26  2015/09/23 13:11:24  klrutkow
 added log of user's CALDB location

 Revision 1.25  2015/09/16 21:06:11  klrutkow
 added string.h for strncasecmp in writeParametersToLog

 Revision 1.24  2015/09/15 17:19:42  klrutkow
 added writeParametersToLog

 Revision 1.23  2015/09/10 02:35:04  klrutkow
 add check that telescop and instrume params match keywords in all infiles ; add telescop and instrume params to readScatteringFile() ; log statements as each file is opened ;

 Revision 1.22  2015/08/13 03:42:42  klrutkow
 updated to version 1.08 ; write DATE keyword to output files

 Revision 1.21  2015/08/13 03:39:12  klrutkow
 edit TTYPEn (column name) keyword comments ; renamed writeParamHistoryKeywords to writeCommonKeywords ; write TELESCOP, INSTRUME, DETNAM to history file second extension ; added resolve() function to query CALDB

 Revision 1.20  2015/06/29 15:28:17  klrutkow
 added longToString function

 Revision 1.19  2015/06/18 17:54:57  klrutkow
 comment out and add some debug statements ; writePSFType2Ext(): 1CDLT1 etc keywords should have 1 at end not 2 ; interceptXRTObjects(): changed comparison of photon zmin and object zmax to >= not = ;

 Revision 1.18  2015/05/14 20:46:01  klrutkow
 fixed bug in housing shift calculation

 Revision 1.17  2015/04/30 23:40:10  klrutkow
 removed fits_read_key_unit for FOCALLEN ; MNEFAREA keyword is no longer divided by nthetain and nrollin ; adding more comments, documentation ; fixed bug in remapReflectTrans(), to size roughtaupermmslice using numFrontEnergies, not numOutEnergies (so it matches TRF, to fix segfault when energy='0.5, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9') ; writeParamHistoryKeywords(): changed version number to 1.07.1 ; changed fits_get_col(TDOUBLE) to fits_get_col_dbl

 Revision 1.16  2015/03/23 14:36:20  klrutkow
 moved telescop, instrume, detnam function params to struct genTelescope

 Revision 1.15  2015/02/11 20:04:28  klrutkow
 corrected bug when reading energies from reflectivity file, so it can read in eV or keV ; improved if-block for storing energies in keV ; added units to GEOMAREA keyword comment

 Revision 1.14  2015/02/09 02:18:48  klrutkow
 updated VERSION keyword to v1.07 from v1.06 ; raytraceonephoton: fix loop through top external objects to stop after searching all top objects, fix if-block checking for central external objects ; corrected the value written to FGEOAREA keyword in history file ; made allnumscatteringangles, allnumincidentangles longs instead of doubles, and static_cast incidentAngleBin to int to avoid warning on linux 32bit ;

 Revision 1.13  2015/01/29 03:17:42  klrutkow
 updated with new params, per issue 472

 Revision 1.12  2015/01/29 03:04:35  klrutkow
 in remap, fixed size of topExtObjectsTransSlice and bottomExtObjectsTransSlice to usenumFrontEnergies instead of numOutEnergies (to fix error with energy grid '1.9 2 2.9') ; getRowsInEnergyRange(): add check that minPhotonEnergy is no smaller than min energy in file, same for max energy, so that input energies are within range of reflectivity file ; added error checking for scattering file ; getScatteredDirection(): using roughscatterangles for rough surfaces instead of frontscatterangles

 Revision 1.11  2015/01/21 17:20:56  klrutkow
 use variables validdt and validtm instead of validdate and validtime, until I redo all param names

 Revision 1.10  2015/01/16 16:07:43  klrutkow
 updated VERSION keyword to v1.06 from v1.05; added new variable stats.numrpiDoubleReflPerEnergy for effective area calculation; size pathCoords, etc arrays in doWork instead of in raytraceOnePhoton(); dynamically change shell seach using new deltashells=1; fixed FDBLREFL to write as a double, not a long; in raytraceOnePhoton(): assigned default impactedXRTObjectID = -1, check this value for finding new search shell low and hi; write validdate and validtime params to CVSD0001 and CVST0001 keywords in output files; using std::fill to reset arrays in raytraceOnePhoton instead of clear and resize

 Revision 1.9  2014/12/29 17:20:08  klrutkow
 new params writeonlyresultsplane, phisttype, externalobjects, fastmode; fixed error with TDF, extension SEGMENT, columns DELTATX Y and Z: store in rad not arcmin; made transformType an enum; raytraceOnePhoton(): fixed typo for pathCode[][2] if impacted an obstruction; changed m_maxTransformXYZ from 12 to 14; updated setupXRTTransform(); new function getImpactCandidates(); new structs to hold telescope information: sectorsShells, ExternalObjects, GenTelescope; added variables to existing structs (topextobjectstrans, etc); added phi mod 2pi to cartesianToPolar(); added keywords NOFFAXIS, NAZIMUTH, NORMRAD to PSF and EEF primary extensions; added history file keyword: NDBLREFL, FDBLREFL; edited comment for history file keyword N1P1SFRL; history file keyword MNEFAREA now uses numrpiDoubleRefl instead of numrpiPriSecRefl; fixed bug in transformBBoxes, to reset output array for each loop; added second extension, INPUTPHOTONS, to history file; used break if sinReflected=0, before entering getReflectionDirection(); applyXRTTransform(): didn't add cumulative; raytraceOnePhoton(): inside candidate loop, store each localDir in potentialLocalDir, use that variable later; setupXRTTransform(): changed comment from cw to ccw in reverse step 13, changing signs of sines in forward steps 2 and 7

 Revision 1.8  2014/11/17 18:01:31  klrutkow
 in psf, ea files, moved EXTNAME keyword above TELESCOP; fixed PSF file so psfxcen is not -0.0; added APERTURE keyword to history file if rectangle; removed zminsortxrtobjectindexAll (since we now use zmaxsortxrtobjectindexAll); made some function input params const (getReflTrans, etc)

 Revision 1.7  2014/11/07 15:25:31  klrutkow
 updated sign in calculating 'factor' in getReflectionDirection(); initialized all bools; change comments in output headers to have units in sq brackets right after slash, to match cfitsio convention (http://heasarc.gsfc.nasa.gov/fitsio/c/c_user/node116.html); made large vectors in raytraceonephoton static to save time; new, faster algorithm for interceptXRTObjects; updated comments for header keywords for output files; in raytraceOnePhoton(): updated thinTransmissionProb=1.0e0 inside 'if (pathFaceHit[numInteractions] > 1)'; in raytraceOnePhoton() 'Front surface reflection' set=0: added 'thinTransmissionProb = 1.0'; in raytraceOnePhoton() 'Interpolate thin-film transmission': changed '(objectset!=0)' to '(objectset>0)'; in raytraceOnePhoton() 'Interpolate thin-film transmission': removed 'thinTransmissionProb = 0.0' from end of if-else block; moved housingHardLimit calculation from raytraceOnePhoton() to initialize(); updated comment for keyword SFPYC; implement heaapp for startup() and shutdown()

 Revision 1.6  2014/10/06 22:47:33  klrutkow
 updated code for xrtraytrace_suite.v1.03: add scattering implementation; changed misalignment implementation to remove random misalignment; allow user to go below inner housing radius; fix bugs in setupXRTTransform; fix keywords in output files for offaxis, psfcenter, fix offset for psf image, added 4 columns to end of history file; debugged betamodel, flatcircle, and photon list modes; if annulus and rectangle are turned off, aperture defaults to annulus

 Revision 1.5  2014/10/03 16:48:56  klrutkow
 code as of v1.02 Sept 11 2014: ensure NGROUPS matches between reflectivity files and TDF; update getPhotonObjectImpactCoords() to correctly hit top of foils, removed last 'killPhoton' flag, better calculate front and back boundaries, better calculate top and bottom boundaries; updated probability calculcations in raytraceonephoton() for absorption, transmission, or reflection; added ability in diagnosticmode to input initial x and y; small edit to updateEventCounters(); abort if user enters malign=gauss but wrong number of doubles for ftilt etc; added .clear() to refl and transm grids before bisectionInterp() in raytraceonephoton(); if numInteractions goes above maxnumpathcoords, stop that photon with an error code instead of aborting program

 Revision 1.4  2014/09/04 16:24:06  klrutkow
 updates for v1.01 delivery

 Revision 1.3  2014/08/12 18:19:31  klrutkow
 fixed error when converting from double to int

 Revision 1.2  2014/08/12 15:12:48  klrutkow
 added headas_utils.h to get headas params

*/
