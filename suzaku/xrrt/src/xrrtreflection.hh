// xrrtreflection.hh
//
// Class definition for X-ray mirror reflection.
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/22 Upgraded documentation. R. Fink

#ifndef XRRTREFLECT_HH
#define XRRTREFLECT_HH

// 
// System interfaces used
//
#include <exception>
#include <string>
// Modified by H. Mori (2005/09/14)
// #include <stl.h>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"
#include "xrrttable.hh"
#include "xrrtmirror.hh"
#include "xrrtcollimator.hh"
#include "xrrtphoton.hh"
#include "xrrtoptical.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// Local enums
//
enum XrrtReflectionErrorCode
        {
        XrrtReflectionErrors,
        noSuchTable,              //"This reflection table was not defined"
        noSuchTableBin,           //"This bin was not allocated"
        unknownMirrorSurfaceFunction, // A mirror surface function value was
                                  // found that code did not exist to handle
        unknownCollimatorSurfaceFunction, // A collimator surface function value was
                                  // found that code did not exist to handle
        noTableForEnergy,         // An energy was requested that did not exist
                                  // in a precomputed reflection table
        XrrtReflectionErrorsEnd
        };

//
// XrrtReflection handles reflections of photons off surfaces
//
class XrrtReflection
{
    // 
    // theReflectionInfo() is a friend so that it can access the private
    // constructor and create the static reflection class
    //
    friend XrrtReflection& theReflectionInfo();

    public:
          //
          // Return a reflection table pointer for the requested name
          //
          XrrtTable* tableEntry(const string& reflectTableName);

          //
          // Create a new reflection table
          //
          XrrtTable* createReflectTable(const string& tableName);

          //
          // Add a reflection table entry to an existing reflection table
          //
          void setReflectTable( XrrtTable* reflectTable,
                                const TableEnergy& energy,
                                const BinAngle& bAngle,
                                const ReflectProb& refProb);

          void setReflectTable2( XrrtTable* reflectTable,
                                 TableEnergy energy,
				 int binAngleNum,
				 BinAngle binAngleZero,
				 BinAngle binAngleDelta,
				 ReflectProb* refProbArray,
				 ScatProb* scatProbArray);

          //
          // Add a reflection table entry to an existing reflection table
          // for ASTRO-E2 Pre-Collimator
          // (modified by H. Mori : date 2003/01/14)
          void setPreCollimatorReflectTable( XrrtTable* reflectTable,
                                             const TableEnergy& energy,
                                             const BinAngle& bAngle,
                                             const ReflectProb& refProb);

          //
          // Add the case of collimator absorption (added by HIDEYUKI MORI)
          // Return true if the photon is absorbed on a mirror
          // and reflect the virtual photon direction if not
          //
          bool photonAbsorbedOnOuterMirror(const XrrtMirror* outerMirror, 
                                                 XrrtPhoton& photon);
          bool photonAbsorbedOnInnerMirror(const XrrtMirror* innerMirror, 
                                                 XrrtPhoton& photon);

          //
          // Return true if the photon is absorbed on a collimator
          // and reflect the virtual photon direction if not
          //
          bool photonAbsorbedOnOuterCollimator(const XrrtCollimator* outerCollimator, 
					       XrrtPhoton& photon);
          bool photonAbsorbedOnInnerCollimator(const XrrtCollimator* innerCollimator,
					       XrrtPhoton& photon);
          
          //
          // Define data for internal reflection tables
          void defineInternalReflectionTables(string& frontSurfaceTableName,
                                              XrrtMolecule* frontSurface,
                                              double& frontSurfaceCGSDensity,
                                              double& frontSurfaceRoughness,
                                              string& backSurfaceTableName,
                                              XrrtMolecule* backSurface,
                                              double& backSurfaceCGSDensity,
                                              double& backSurfaceRoughness);
          //
          // Convert error codes to error messages
          //
          string errorMessage(XrrtReflectionErrorCode errorCode);

    private:
          // Constructor
          XrrtReflection( );
          // Copy Constructor
          XrrtReflection( const XrrtReflection& reflection);

          //
          // A list of the reflection tables that have been stored in memory
          vector<XrrtTable*> tableList;

          // Data for the special case where we compute reflection data
          // dynamically
          bool dynamicReflectionTable;
          string frontSurfaceTableName;
          XrrtMolecule* frontSurface;
          double frontSurfaceCGSDensity;
          double frontSurfaceRoughness;
          string backSurfaceTableName;
          XrrtMolecule* backSurface;
          double  backSurfaceCGSDensity;
          double backSurfaceRoughness;
          XrrtOpticalConstants optical;

};

XrrtReflection& theReflectionInfo();

#endif
