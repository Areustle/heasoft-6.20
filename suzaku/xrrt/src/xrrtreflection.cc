// xrrtreflection.cc
//
// Member functions for Reflection class
//
// Richard L Fink GSFC/631
// 1997/05/30

// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 1.2  2001/03/19 06:37:54  mori
// Incorporate the frontside and backside collimator reflection
// in Xrrtreflection class.
//
// Revision 1.1  2000/10/19 12:23:41  mori
// Initial revision
//

//#include <iostream.h>
#include "xrrtreflection.hh"

XrrtReflection&
theReflectionInfo()
{
//
// Return the static XrrtReflection info
//
	static XrrtReflection reflectionInfo;

	return reflectionInfo;
}

XrrtReflection::XrrtReflection():
tableList()
{
// A simple constructor
}

string
XrrtReflection::errorMessage(XrrtReflectionErrorCode errorCode)
{
	//
	// Convert error codes to error messages
	//
	string errorMessage;

    switch (errorCode) {
	case noSuchTable:
		errorMessage = "\
A non-existent reflection table was requested";
		break;
	case noSuchTableBin:
		errorMessage = "\
A reflection table bin was requested that does not exist";
		break;
	case unknownMirrorSurfaceFunction:
		errorMessage = "\
A mirror surface function type was requested that has no code to support it";
		break;
	case noTableForEnergy:
		errorMessage = "\
An energy was requested that does not exist in the reflection table";
		break;
	default: 
		{
			char charNumber[1024];
			sprintf(charNumber, "%d",errorCode);
			errorMessage = "\
XrrtReflection::errorMessage Unknown error code: ";
			errorMessage.append(charNumber);
		}
		break;
	}
	return errorMessage;
}

void
XrrtReflection::defineInternalReflectionTables(string& frontTableName,
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
}

XrrtTable*
XrrtReflection::tableEntry(const string& reflectTableName)
{
//
// Search the list of tables for one that matches the supplied name
//
	int limit;

	limit = tableList.end() - tableList.begin();

	for (int i=0; i<limit; i++) {
		if (tableList[i]->getTableName() == reflectTableName) {
			return tableList[i];
		}
	}
	return 0;
}

bool
XrrtReflection::photonAbsorbedOnInnerMirror(const XrrtMirror* innerMirror, 
                                                  XrrtPhoton& photon)
{
	//
	// Return true if the photon is absorbed.
	// Return false if the photon is not absorbed and reflect it in the
	// virtual photon.
	//

    //
    // Obtain a random number for the reflection probability of THIS photon
    //
    double random = xrrtrandom();
    //
    // Compute the incident reflection angle between the mirror and the
    // photon
    //
    if (innerMirror->getSurfaceFunction() == PLANE_MIRROR) {
		//
		// Get the unit vector for the back of the mirror
		//
		XrrtVector mirrorVector = 
		innerMirror->getPlaneMirrorBackVector(photon.getRotationAngle());
		//
		// Get the unit vector for the photon direction
		//
		XrrtVector photonVector = photon.getCurrentVector();
		//
		// Compute the angle between them
		//
		double incidentAngle = mirrorVector.angleBetween(photonVector);
		//if (incidentAngle < 0.0e0) {
		//	cout <<"IncidentAngle ="<<incidentAngle<<"\n";
		//}
		//
		// Get the photon energy
		//
		double energy = photon.getEnergy();
		//
		// Get the reflection probability for a photon with this energy
		// and this incident angle for the mirror surface.
		//
		double probability;
		if (dynamicReflectionTable) {
			probability = optical.computeSingleLayerReflect(*backSurface,
														backSurfaceCGSDensity,
														energy,
														backSurfaceRoughness,
														incidentAngle);
		} else {
			//
			// Get the reflection table for the back of the mirror
			//
			XrrtTable* table = innerMirror->getBackReflectTable();
			probability = table->getReflectivity(energy, incidentAngle);
		}
		//
		// If the random number is greater than the reflection probability
		// then absorption occurs;
		// for reflect prob = 1 then no absorption (random always < 1)
		// for reflect prob = 0.5 then 1/2 of random number yield absorption
		// for reflect prob = 0.1 then 9/10 of random numbers yield absorption
		if (random > probability) {
			// Absorption
			return true;
		}
		// 
		// The photon was NOT absorbed (so it reflected)
		// Now we need to reflect the virtual photon
		//
		// Get the photon direction
		//
		double photonX;
		double photonY;
		double photonZ;
		photon.getPhotonDirectionXYZ(photonX, photonY, photonZ);
		//
		// Get the normal vector to the mirror surface
		//
		XrrtVector backNormal = 
		innerMirror->getPlaneMirrorBackNormal(photon.getRotationAngle());
		//
		// Get the normal direction
		//
		double normalX;
		double normalY;
		double normalZ;
		backNormal.getVectorDirectionXYZ(normalX, normalY, normalZ);
		//
		// A (dot) B = |A| |B| cos(angle between)
		//
		double cosNormalToPhoton =
		photonX*normalX + photonY*normalY + photonZ*normalZ;
		//
		// Calculate the reflection direction vectors
		//
		double newPhotonX = photonX - 2.0e0*cosNormalToPhoton*normalX;
		double newPhotonY = photonY - 2.0e0*cosNormalToPhoton*normalY;
		double newPhotonZ = photonZ - 2.0e0*cosNormalToPhoton*normalZ;
		//
		// Update the virtual direction photon
		//
		photon.setVirtualDirectionXYZ(newPhotonX, newPhotonY, newPhotonZ);
		// The photon was not absorbed
		return false;
	} else {
		// this code has no idea how to compute the reflection
		throw unknownMirrorSurfaceFunction;
	}
}

bool
XrrtReflection::photonAbsorbedOnOuterMirror(const XrrtMirror* outerMirror, 
                                                  XrrtPhoton& photon)
{
	//
	// Return true if the photon is absorbed.
	// Return false if the photon is not absorbed and reflect it in the
	// virtual photon.
	//

    //
    // Obtain a random number for the reflection probability of THIS photon
    //
    double random = xrrtrandom();
    //
    // Compute the incident reflection angle between the mirror and the
    // photon
    //
    if (outerMirror->getSurfaceFunction() == PLANE_MIRROR) {
		//
		// Get the unit vector for the back of the mirror
		//
		XrrtVector mirrorVector = 
		outerMirror->getPlaneMirrorFrontVector(photon.getRotationAngle());
		//
		// Get the unit vector for the photon direction
		//
		XrrtVector photonVector = photon.getCurrentVector();
		//
		// Compute the angle between them
		//
		double incidentAngle = mirrorVector.angleBetween(photonVector);
		//if (incidentAngle < 0.0e0) {
		//	cout <<"IncidentAngle ="<<incidentAngle<<"\n";
		//}
		//
		// Get the photon energy
		//
		double energy = photon.getEnergy();
		double probability;
		if (dynamicReflectionTable) {
			probability = optical.computeSingleLayerReflect(*frontSurface,
													frontSurfaceCGSDensity,
													energy,
													frontSurfaceRoughness,
													incidentAngle);
		} else {
			//
			// Get the reflection table for the front of the mirror
			//
			XrrtTable* table = outerMirror->getFrontReflectTable();
			probability = table->getReflectivity(energy, incidentAngle);
		}
		//
		// If the random number is greater than the reflection probability
		// then absorption occurs;
		// for reflect prob = 1 then no absorption (random always < 1)
		// for reflect prob = 0.5 then 1/2 of random number yield absorption
		// for reflect prob = 0.1 then 9/10 of random numbers yield absorption
		if (random > probability) {
			// Absorption
			return true;
		}
		// The photon was NOT absorbed (so it reflected)
		// Now we need to reflect the virtual photon
		//
		// Get the photon direction
		//
		double photonX;
		double photonY;
		double photonZ;
		photon.getPhotonDirectionXYZ(photonX, photonY, photonZ);
		//
		// Get the normal vector to the mirror surface
		//
		XrrtVector mirrorNormalVector = 
		outerMirror->getPlaneMirrorFrontNormal(photon.getRotationAngle());
		//
		// Get the normal direction
		//
		double normalX;
		double normalY;
		double normalZ;
		mirrorNormalVector.getVectorDirectionXYZ(normalX, normalY, normalZ);
		//
		// A (dot) B = |A| |B| cos(angle between)
		//
		double cosNormalToPhoton =
		photonX*normalX + photonY*normalY + photonZ*normalZ;
		//
		// Calculate the reflection direction vectors
		//
		double newPhotonX = photonX - 2.0e0*cosNormalToPhoton*normalX;
		double newPhotonY = photonY - 2.0e0*cosNormalToPhoton*normalY;
		double newPhotonZ = photonZ - 2.0e0*cosNormalToPhoton*normalZ;

		//
		// Update the virtual direction photon
		//
		photon.setVirtualDirectionXYZ(newPhotonX,newPhotonY,newPhotonZ);
		// Get the unit vector for the photon direction
		photonVector = photon.getVirtualVector();
		// Compute the angle between them
		double reflectAngle = mirrorVector.angleBetween(photonVector);
		if (fabs(reflectAngle - incidentAngle) > 0.0001) {
			//cout <<"#RA "<<reflectAngle<<" IA "<<incidentAngle<<"\n";
		}
		// The photon was not absorbed
		return false;
	} else {
		// this code has no idea how to compute the reflection
		throw unknownMirrorSurfaceFunction;
	}
}

// Add the case of absorption on inner collimator (added by HIDEYUKI MORI)
bool
XrrtReflection::photonAbsorbedOnInnerCollimator(
		const XrrtCollimator* innerCollimator,
		XrrtPhoton& photon)
{
	//
	// Return true if the photon is absorbed.
	// Return false if the photon is not absorbed and reflect it in the
	// virtual photon.
	//

    //
    // Obtain a random number for the reflection probability of THIS photon
    //
    double random = xrrtrandom();
    //
    // Compute the incident reflection angle between the collimator and the
    // photon
    //
    if (innerCollimator->getColSurfaceFunction() == PLANE_COLLIMATOR) {
		//
		// Get the unit vector for the back of the collimator
		//
		XrrtVector collimatorVector = 
		innerCollimator->getPlaneCollimatorBackVector(photon.getRotationAngle());
		//
		// Get the unit vector for the photon direction
		//
		XrrtVector photonVector = photon.getCurrentVector();
		//
		// Compute the angle between them
		//
		double incidentAngle = collimatorVector.angleBetween(photonVector);
		//if (incidentAngle < 0.0e0) {
		//	cout <<"IncidentAngle ="<<incidentAngle<<"\n";
		//}
		//
		// Get the photon energy
		//
		double energy = photon.getEnergy();
		//
		// Get the reflection probability for a photon with this energy
		// and this incident angle for the collimator surface.
		//
		double probability;
		//
		// Get the reflection table for the back of the collimator
		// Remove the case of dynamically reflectivity calculation because
		// Pre-Collimator blade reflection cannot be interpreted by the
		// theoretical X-ray reflection 
		// (modified by H. Mori : date 2003/01/14)
		XrrtTable* table = innerCollimator->getPreCollimatorReflectTable();
		probability = table->getReflectivity(energy, incidentAngle);

		//
		// If the random number is greater than the reflection probability
		// then absorption occurs;
		// for reflect prob = 1 then no absorption (random always < 1)
		// for reflect prob = 0.5 then 1/2 of random number yield absorption
		// for reflect prob = 0.1 then 9/10 of random numbers yield absorption
		if (random > probability) {
			// Absorption
			return true;
		}
		// 
		// The photon was NOT absorbed (so it reflected)
		// Now we need to reflect the virtual photon
		//
		// Get the photon direction
		//
		double photonX;
		double photonY;
		double photonZ;
		photon.getPhotonDirectionXYZ(photonX, photonY, photonZ);
		//
		// Get the normal vector to the collimator surface
		//
		XrrtVector backNormal = 
		innerCollimator->getPlaneCollimatorBackNormal(photon.getRotationAngle());
		//
		// Get the normal direction
		//
		double normalX;
		double normalY;
		double normalZ;
		backNormal.getVectorDirectionXYZ(normalX, normalY, normalZ);
		//
		// A (dot) B = |A| |B| cos(angle between)
		//
		double cosNormalToPhoton =
		photonX*normalX + photonY*normalY + photonZ*normalZ;
		//
		// Calculate the reflection direction vectors
		//
		double newPhotonX = photonX - 2.0e0*cosNormalToPhoton*normalX;
		double newPhotonY = photonY - 2.0e0*cosNormalToPhoton*normalY;
		double newPhotonZ = photonZ - 2.0e0*cosNormalToPhoton*normalZ;
		//
		// Update the virtual direction photon
		//
		photon.setVirtualDirectionXYZ(newPhotonX, newPhotonY, newPhotonZ);
		// The photon was not absorbed
		return false;
	} else {
		// this code has no idea how to compute the reflection
		throw unknownCollimatorSurfaceFunction;
	}
}

bool
XrrtReflection::photonAbsorbedOnOuterCollimator(
		const XrrtCollimator* outerCollimator,
		XrrtPhoton& photon)
{
	//
	// Return true if the photon is absorbed.
	// Return false if the photon is not absorbed and reflect it in the
	// virtual photon.
	//

    //
    // Obtain a random number for the reflection probability of THIS photon
    //
    double random = xrrtrandom();
    //
    // Compute the incident reflection angle between the collimator and the
    // photon
    //
    if (outerCollimator->getColSurfaceFunction() == PLANE_COLLIMATOR) {
		//
		// Get the unit vector for the back of the collimator
		//
		XrrtVector collimatorVector = 
		outerCollimator->getPlaneCollimatorFrontVector(photon.getRotationAngle());
		//
		// Get the unit vector for the photon direction
		//
		XrrtVector photonVector = photon.getCurrentVector();
		//
		// Compute the angle between them
		//
		double incidentAngle = collimatorVector.angleBetween(photonVector);
		//if (incidentAngle < 0.0e0) {
		//	cout <<"IncidentAngle ="<<incidentAngle<<"\n";
		//}
		//
		// Get the photon energy
		//
		double energy = photon.getEnergy();
		double probability;
		//
		// Get the reflection table for the front of the Pre-Collimator
		// Remove the case of dynamically reflectivity calculation because
		// Pre-Collimator blade reflection cannot be interpreted by the
		// theoretical X-ray reflection 
		// (modified by H. Mori : date 2003/01/14)
		XrrtTable* table = outerCollimator->getPreCollimatorReflectTable();
		probability = table->getReflectivity(energy, incidentAngle);

		//
		// If the random number is greater than the reflection probability
		// then absorption occurs;
		// for reflect prob = 1 then no absorption (random always < 1)
		// for reflect prob = 0.5 then 1/2 of random number yield absorption
		// for reflect prob = 0.1 then 9/10 of random numbers yield absorption
		if (random > probability) {
			// Absorption
			return true;
		}
		// The photon was NOT absorbed (so it reflected)
		// Now we need to reflect the virtual photon
		//
		// Get the photon direction
		//
		double photonX;
		double photonY;
		double photonZ;
		photon.getPhotonDirectionXYZ(photonX, photonY, photonZ);
		//
		// Get the normal vector to the collimator surface
		//
		XrrtVector collimatorNormalVector = 
		outerCollimator->getPlaneCollimatorFrontNormal(photon.getRotationAngle());
		//
		// Get the normal direction
		//
		double normalX;
		double normalY;
		double normalZ;
		collimatorNormalVector.getVectorDirectionXYZ(normalX, normalY, normalZ);
		//
		// A (dot) B = |A| |B| cos(angle between)
		//
		double cosNormalToPhoton =
		photonX*normalX + photonY*normalY + photonZ*normalZ;
		//
		// Calculate the reflection direction vectors
		//
		double newPhotonX = photonX - 2.0e0*cosNormalToPhoton*normalX;
		double newPhotonY = photonY - 2.0e0*cosNormalToPhoton*normalY;
		double newPhotonZ = photonZ - 2.0e0*cosNormalToPhoton*normalZ;

		//
		// Update the virtual direction photon
		//
		photon.setVirtualDirectionXYZ(newPhotonX,newPhotonY,newPhotonZ);
		// Get the unit vector for the photon direction
		photonVector = photon.getVirtualVector();
		// Compute the angle between them
		double reflectAngle = collimatorVector.angleBetween(photonVector);
		if (fabs(reflectAngle - incidentAngle) > 0.0001) {
			//cout <<"#RA "<<reflectAngle<<" IA "<<incidentAngle<<"\n";
		}
		// The photon was not absorbed
		return false;
	} else {
		// this code has no idea how to compute the reflection
		throw unknownCollimatorSurfaceFunction;
	}
}

void 
XrrtReflection::setReflectTable(XrrtTable* reflectTable,
                                const TableEnergy& energy,
                                const BinAngle& bAngle,
                                const ReflectProb& refProb)
{
	reflectTable->setTableEntry(energy, bAngle, refProb);

}

void 
XrrtReflection::setReflectTable2(XrrtTable* reflectTable,
								 TableEnergy energy,
								 int binAngleNum,
								 BinAngle binAngleZero,
								 BinAngle binAngleDelta,
								 ReflectProb* refProbArray,
								 ScatProb* scatProbArray)
{
	reflectTable->setTableRow(energy,
							  binAngleNum,
							  binAngleZero,
							  binAngleDelta,
							  refProbArray,
							  scatProbArray);
}

void 
XrrtReflection::setPreCollimatorReflectTable(XrrtTable* reflectTable,
                                             const TableEnergy& energy,
                                             const BinAngle& bAngle,
                                             const ReflectProb& refProb)
{
	reflectTable->setTableEntry(energy, bAngle, refProb);
}

XrrtTable*
XrrtReflection::createReflectTable(const string& tableName)
{
	XrrtTable* tableEntry;

	tableEntry = new XrrtTable;
	tableList.push_back(tableEntry);
	tableEntry->setTableName(tableName);
	return tableEntry;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C++ ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
