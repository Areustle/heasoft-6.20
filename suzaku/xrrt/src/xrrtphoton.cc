// xrrtphoton.cc
//
// Member functions for Photon class
//
// Richard L Fink GSFC/631
// 1997/05/19
// 1997/09/24 Upgrade documentation. R. Fink
// 1998/10/10 Modified XrrtPhoton::classifyPhotonByReflections() to track
//            which layer a single reflection came from.

// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 2.1  2000/11/29 04:28:17  mori
// Add the case of existing pre-collimator for ASTRO-E-II
// Revision 1.1  2000/10/19 06:41:43  mori
// Initial revision

#include "xrrtphoton.hh"


XrrtPhoton&
thePhoton()
{
//
// Return the static photon used by the program
//
static XrrtPhoton photon;

return photon;
}

XrrtPhoton::XrrtPhoton():
    photonCounter(0),
    energy(0),
    initialLocation(),
    location(),
    virtualLocation(),
    impactObstruction(0),
    absorptionMirror(0),
    absorptionFace(NO_ABSORPTION),
    // Add case which photon is absorbed in collimator (added by HIDEYUKI MORI)
    absorptionCollimator(0),
    absorptionColFace(NO_COLLIMATOR_ABSORPTION),
    reflectionsByLayer(),
    classFromReflections(NO_REFLECTION_CLASS)
{
// Simple constructor
}

string
XrrtPhoton::errorMessage(XrrtPhotonErrorCode errorCode)
{
//
// Convert error codes to error messages
//
string errorMessage;

    switch (errorCode)
        {
        case invalidPhotonClass:
            errorMessage = 
            "An invalid photon class was detected";
            break;
        default:
                {
                char charNumber[1024];
                sprintf(charNumber, "%d",errorCode);
                errorMessage = 
                           "XrrtPhoton::errorMessage Unknown error code: ";
                errorMessage.append(charNumber);
                }
             break;
        }
     return errorMessage;
}

string
XrrtPhoton::errorMessage(XrrtVectorErrorCode errorCode)
{
//
// A pass thru for error codes from XrrtVector
//
string errorMessage;

     errorMessage = location.errorMessage(errorCode);
     return errorMessage;
}

void
XrrtPhoton::classifyPhotonByReflections()
{
//
// Classify the path the photon took thru the mirrors from the reflections
// it made on mirrors and obstructions
// Note that this code does not differenciate between reflection off
// inner mirror surfaces and outer mirror surfaces.
// To do is a trivial addition.
//

    // default the photon class to NO_REFLECTION_CLASS
    classFromReflections = NO_REFLECTION_CLASS;

    // If the photon did not reach the focal plane, we have nothing to do
    if (currentPhotonStatus != PHOTON_HIT_FOCAL_PLANE)
       {
       return;
       }
    
    // Walk thru the reflections in order to set the correct code
    // We use an exclusion algorithm here; the photon is abnormal until
    // it proves it is something else; then we test for that
    
    // Add for the case of collimator reflection
    // (added by HIDEYUKI MORI)

    classFromReflections = ABNORMAL_PATH;
    int reflectionTotal = 0;
    int numberOfMirrors = 0;
    int numberOfCollimators = 0;
    LayerType layerType;
    int layerCount;
    PhotonReflectionClass saveLayerInfo = NO_REFLECTION_CLASS;
    //
    // For each layer in the telescope
    //
    for (int layer = 0;
         layer < theStructure().getNumberOfLayers(); 
         layer++)
        {
        //
        // What kind of layer is this?
        layerType = theStructure().getLayerType(layer);
        //
        // How many reflections occured in this layer?
        layerCount = reflectionsByLayer[layer];
        //
        if (layerType == MIRROR)
           {
           numberOfMirrors++;
           }
	
	if (layerType == COLLIMATOR)
	  {
	  numberOfCollimators++;
	  }

        if (layerType == OBSTRUCTION && layerCount != 0)
           {
           // We don't reflect off obstructions
           // BUT this could be a warning of internal memory problems as 
           // happened in testing
           cout << "Warning: Invalid reflection count for photon "<<photonCounter<<"\n";
           continue;
           }

        //
        // Keep track to the total number of reflections
        reflectionTotal = reflectionTotal + layerCount;

	// Add the case of collimator reflection, set ABNORMAL_PATH for convenience
	// (added by H. Mori)
	if (layerType == COLLIMATOR && layerCount != 0)
	  {
	    return;
	  }

        //
        if (layerType == MIRROR && layerCount > 1)
           {
           // Any time we reflect off a mirror more than once we are abnormal
           return;
           }
        //
        // Leave a bread-crumb trail for the single reflection case so
        // we can later tell which layer the reflection came from.
        if (layerType == MIRROR && layerCount == 1)
           {
	   // What is the first layer type? Collimator or Obstruction?
           // Then we select primary and secondary only reflection components
           // by the first layer.
           // (modified by H. Mori : date 2002/07/23)
	     if(theStructure().getLayerType(0) == COLLIMATOR){  
	       if (layer == 2)
		 {
		   saveLayerInfo = FIRST_LAYER_ONLY;
		 }
	       else if (layer != 2)
		 {
		   saveLayerInfo = SECOND_LAYER_ONLY;
		 }
	       else
		 {
		   saveLayerInfo = ONE_LAYER_ONLY;
		 }
	     }
	     else if(theStructure().getLayerType(0) == OBSTRUCTION){
	       if (layer == 1)
		 {
		   saveLayerInfo = FIRST_LAYER_ONLY;
		 }
	       else if (layer != 1)
		 {
		   saveLayerInfo = SECOND_LAYER_ONLY;
		 }
	       else
		 {
		   saveLayerInfo = ONE_LAYER_ONLY;
		 }
	     }
	   }
    }	
    //
    // If we got thru the above loop, then we are either a normal photon
    // or a single reflection photon
    if (reflectionTotal == numberOfMirrors)
       {
       // The above code proves that 1 reflection per 1 mirror
       classFromReflections = ONE_PER_LAYER;
       return;
       }
    if (reflectionTotal == 1)
       {
       // Obviously
       classFromReflections = saveLayerInfo;
       return;
       }
    if (reflectionTotal == 0)
       {
       classFromReflections = NO_REFLECTIONS;
       return;
       }
    if (reflectionTotal < numberOfMirrors)
       {
       // We skipped a mirror therefore we are abnormal
       classFromReflections = ABNORMAL_PATH;
       return;
       }
    // Arriving here is an error since we have accounted for all photon
    // classes that can occur
    throw invalidPhotonClass;
}



void
XrrtPhoton::clearPhoton()
{
    // Set the photon to a nominal "clean" state
    currentPhotonStatus = PHOTON_CONTINUES;
    energy = 0.0e0;
    initialLocation.zeroVector();
    location.zeroVector();
    virtualLocation.zeroVector();
    impactObstruction = (XrrtObstruction*) 0;
    absorptionMirror  = (XrrtMirror*) 0;
    absorptionFace    = NO_ABSORPTION;
    // Add the case of collimator absorption (added by HIDEYUKI MORI)
    absorptionCollimator = (XrrtCollimator*) 0;
    absorptionColFace = NO_COLLIMATOR_ABSORPTION;
    classFromReflections = NO_REFLECTION_CLASS;
    fill_n(reflectionsByLayer.begin(),reflectionsByLayer.size(),0);
    // Set the photon reflection pass to clean state (modified by HIDEYUKI MORI)
    ReflectionPath.clear();
    photonCounter++;
}

void 
XrrtPhoton::setPhotonStatus(const PhotonStatusCodes code)
{
    currentPhotonStatus = code;
}


void
XrrtPhoton::setRadius(const RadiusInMM& parameter)
{
      location.setRadius (parameter);
}



void
XrrtPhoton::setRotationAngle(const AngleInRadians& parameter)
{
      location.setRotationAngle(parameter);
}


AngleInRadians
XrrtPhoton::getVirtualRotationAngle() const
{
      return virtualLocation.getRotationAngle();
}

void 
XrrtPhoton::setEnergy (const EnergyInKev& parameter)
{
     energy = parameter;
}

double 
XrrtPhoton::getEnergy() const
{
    return energy;
}


void
XrrtPhoton::setPhotonDirection(const UnitVectorMag& radial,
                               const UnitVectorMag& theta,
                               const UnitVectorMag& z)
{
      location.setVectorDirection(radial,theta,z);
}

void
XrrtPhoton::setPhotonDirectionXYZ(const UnitVectorMag& xDirection,
                                  const UnitVectorMag& yDirection,
                                  const UnitVectorMag& zDirection)
{
    location.setVectorDirectionXYZ(xDirection,yDirection,zDirection);
}


void
XrrtPhoton::setVirtualDirection(const UnitVectorMag& radial,
                                const UnitVectorMag& theta,
                                const UnitVectorMag& z)
{
      virtualLocation.setVectorDirection(radial,theta,z);
}

void
XrrtPhoton::setVirtualDirectionXYZ(const UnitVectorMag& xDirection,
                                   const UnitVectorMag& yDirection,
                                   const UnitVectorMag& zDirection)
{
      virtualLocation.setVectorDirectionXYZ(xDirection,yDirection,zDirection);
}

void 
XrrtPhoton::getVirtualDirection(UnitVectorMag& radial,
                                UnitVectorMag& theta,
                                UnitVectorMag& z)
{
    virtualLocation.getVectorDirection(radial,theta,z);
}

void 
XrrtPhoton::getVirtualDirectionXYZ(UnitVectorMag& xDirection,
                                   UnitVectorMag& yDirection,
                                   UnitVectorMag& zDirection)
{
    virtualLocation.getVectorDirectionXYZ(xDirection,yDirection,zDirection);
}


void
XrrtPhoton::getPhotonDirection(UnitVectorMag& radial,
                               UnitVectorMag& theta,
                               UnitVectorMag& z)
{
      location.getVectorDirection(radial,theta,z);
}

void 
XrrtPhoton::getPhotonDirectionXYZ(UnitVectorMag& xDirection,
                                  UnitVectorMag& yDirection,
                                  UnitVectorMag& zDirection)
{
    location.getVectorDirectionXYZ(xDirection,yDirection,zDirection);
}

void
XrrtPhoton::projectPhoton(const FPDistanceInMM& distance)
{
      location.projectVector(distance);
}

void
XrrtPhoton::projectVirtualPhoton(const FPDistanceInMM& distance)
{
      virtualLocation.projectVector(distance);
}

void
XrrtPhoton::getVirtualXY(VertexInMM* x, VertexInMM* y)
{
      virtualLocation.getXY(x,y);
}

void 
XrrtPhoton::setInitialToCurrent()
{
    initialLocation = location;
}

void 
XrrtPhoton::setPhotonDistance( const FPDistanceInMM& parameter)
{
    location.setFPDistance(parameter);
}

FPDistanceInMM 
XrrtPhoton::getPhotonDistance() const
{
     return location.getFPDistance();
}

XrrtVector 
XrrtPhoton::getCurrentVector() const
{
    return location;
}

XrrtVector 
XrrtPhoton::getVirtualVector() const
{
    return virtualLocation;
}

XrrtVector 
XrrtPhoton::getInitialVector() const
{
     return initialLocation;
}

void 
XrrtPhoton::setImpactObstruction(const XrrtObstruction* obstruction)
{
    impactObstruction =(XrrtObstruction*)  obstruction;
}

void 
XrrtPhoton::addReflection(const int& layer)
{
    reflectionsByLayer[layer]++;
}

// (added by H. Mori : date 2003/01/28)
void
XrrtPhoton::addReflectionSurfaceWithPreCollimator(const int& layer, ReflectionSurfaceSide SurfaceSide){
  if(SurfaceSide == INNER){
    switch(layer){
    case 0 : setReflectionSurface(COLLIMATOR_INNER);
      break;
    case 2 : setReflectionSurface(PRIMARY_INNER);
      break;
    case 4 : setReflectionSurface(SECONDARY_INNER);
      break;
    default : setReflectionSurface(REFLECTION_PATH_ERROR);
      break;
    }
  }
  if(SurfaceSide == OUTER){
    switch(layer){
    case 0 : setReflectionSurface(COLLIMATOR_OUTER);
      break;
    case 2 : setReflectionSurface(PRIMARY_OUTER);
      break;
    case 4 : setReflectionSurface(SECONDARY_OUTER);
      break;
    default : setReflectionSurface(REFLECTION_PATH_ERROR);
      break;
    }
  }
  else return;
}

// (added by H. Mori : date 2003/01/28)
void
XrrtPhoton::addReflectionSurfaceWithoutPreCollimator(const int& layer, ReflectionSurfaceSide SurfaceSide){
  if(SurfaceSide == INNER){
    switch(layer){
    case 1 : setReflectionSurface(PRIMARY_INNER);
      break;
    case 3 : setReflectionSurface(SECONDARY_INNER);
      break;
    default : setReflectionSurface(REFLECTION_PATH_ERROR);
      break;
    }
  }
  if(SurfaceSide == OUTER){
    switch(layer){
    case 1 : setReflectionSurface(PRIMARY_OUTER);
      break;
    case 3 : setReflectionSurface(SECONDARY_OUTER);
      break;
    default : setReflectionSurface(REFLECTION_PATH_ERROR);
      break;
    }
  }
  else return;
}

// modified by H. Mori
void
XrrtPhoton::setReflectionSurface(const PhotonReflectionSurface code){
  ReflectionPath.push_back(code);
}

// modified by H. Mori
long
XrrtPhoton::getReflectionPath() const
{
  int PathArrayNum, i, j;
  long reflectionPathNumber = 0;
  long powerDecimal = 1;
  
  PathArrayNum = ReflectionPath.size();
  for(i = 0; i < PathArrayNum; i++){
    for(j = PathArrayNum - i - 1; j > 0; j--) powerDecimal = 10 * powerDecimal;
    reflectionPathNumber = reflectionPathNumber + powerDecimal * ReflectionPath[i];
    powerDecimal = 1;
  }
  
  return reflectionPathNumber;
}

void 
XrrtPhoton::zeroReflectionsByLayer(const int& layerCount)
{
    if (reflectionsByLayer.empty())
       {
       for (int i=0; i< layerCount; i++)
          {
          reflectionsByLayer.push_back(0);
          }
       }
    else
       {
       fill_n(reflectionsByLayer.begin(), layerCount, 0);
       }
}

int 
XrrtPhoton::getTotalReflections() const
{
    return accumulate(reflectionsByLayer.begin(), 
                      reflectionsByLayer.end(),
                      0);
}

void 
XrrtPhoton::setAbsorptionMirror(const XrrtMirror* mirror)
{
    // Discard const since we intend to only read the mirror
    // at the time this is written
    absorptionMirror = (XrrtMirror*) mirror;
}

// Add the case which photon is absorbed in collimator (added by HIDEYUKI MORI)
void 
XrrtPhoton::setAbsorptionCollimator(const XrrtCollimator* collimator)
{
  // Discard const since we intend to only read the collimator
  // at the time this is written
  absorptionCollimator = (XrrtCollimator*) collimator;
}

void 
XrrtPhoton::setAbsorptionMirrorFace(const AbsorptionMirrorFace face)
{
    absorptionFace = face;
}

// Add the case which photon is absorbed in collimator (added by HIDEYUKI MORI)
void 
XrrtPhoton::setAbsorptionCollimatorFace(const AbsorptionCollimatorFace face)
{
  absorptionColFace = face;
}

PhotonReflectionClass 
XrrtPhoton::getPhotonReflectionClass() const
{
    return classFromReflections;
}

//
// Convert the direction vector of a current photon in the
// telescope (base) coordinate to that in the quadrant coordinate
// (added by Hideyuki MORI : date 2005/12/15)
// Add the argument of xrrtquadrant class to get the information 
// about the quadrant's offset
// (modified by Hideyuki MORI : date 2006/01/27)
//
void
XrrtPhoton::setTelescopeToQuadrantCoordinate(const XrrtQuadrant &quadrant)
{
    VertexInMM telescopeX;
    VertexInMM telescopeY;
    FPDistanceInMM telescopeZ;
    VertexInMM quadrantX;
    VertexInMM quadrantY;
    FPDistanceInMM quadrantZ;

    UnitVectorMag telescopeXDirection;
    UnitVectorMag telescopeYDirection;
    UnitVectorMag telescopeZDirection;

    UnitVectorMag quadrantXDirection;
    UnitVectorMag quadrantYDirection;
    UnitVectorMag quadrantZDirection;

    VertexInMM offsetX;
    VertexInMM offsetY;
    FPDistanceInMM offsetZ;

    AngleInRadians angleOffsetThetaX;
    AngleInRadians angleOffsetThetaY;
    AngleInRadians angleOffsetThetaZ;

    UnitVectorMag oldDirectionX;
    UnitVectorMag oldDirectionY;
    UnitVectorMag oldDirectionZ;

    UnitVectorMag newDirectionX;
    UnitVectorMag newDirectionY;
    UnitVectorMag newDirectionZ;

    double cosThetaX = 0.0e0;
    double sinThetaX = 0.0e0;
    double cosThetaY = 0.0e0;
    double sinThetaY = 0.0e0;
    double cosThetaZ = 0.0e0;
    double sinThetaZ = 0.0e0;

    location.getXY(&telescopeX, &telescopeY);
    telescopeZ = location.getFPDistance();

    /*
    // OKADA 20061108
    // まだ同じ値でした。
    VertexInMM arereTelescopeX=0e0;
    VertexInMM arereTelescopeY=0e0;
    FPDistanceInMM arereTelescopeZ=0e0;
    virtualLocation.getXY( &arereTelescopeX, &arereTelescopeY);
    arereTelescopeZ = virtualLocation.getFPDistance();
    fprintf(stdout,"location(%lf, %lf) , virtualLocation(%lf, %lf)\n"
	    , telescopeX, telescopeY
	    , arereTelescopeX, arereTelescopeY);
    fprintf(stdout,"locationZ = %lf, virtualLocationZ = %lf\n"
	    , telescopeZ, arereTelescopeZ);
    */
    
    offsetX = quadrant.getCurrentOffsetX();
    offsetY = quadrant.getCurrentOffsetY();
    offsetZ = quadrant.getCurrentOffsetZ();
/*	printf("currentQuadrant=%d, currentLayer=%d, offsetX=%f, offsetY=%f, offsetZ=%f\n",
		   quadrant.getCurrentQuadrant(), quadrant.getCurrentLayer(), offsetX, offsetY, offsetZ);*/

    quadrantX = telescopeX - offsetX;
    quadrantY = telescopeY - offsetY;
    quadrantZ = telescopeZ - offsetZ;

    /*
    // OKADA 20061108
    fprintf(stdout,"(telescopeX, quadrantX)=( %lf, %lf)\n",
	    telescopeX, quadrantX);
    fprintf(stdout,"(telescopeY, quadrantY)=( %lf, %lf)\n",
	    telescopeY, quadrantY);
    fprintf(stdout,"(telescopeZ, quadrantZ)=( %lf, %lf)\n",
	    telescopeZ, quadrantZ);
    fprintf(stdout,"OffsetXYZ = %lf %lf %lf\n"
	    , offsetX
	    , offsetY
	    , offsetZ);
    */

    location.getVectorDirectionXYZ(telescopeXDirection,
				   telescopeYDirection,
				   telescopeZDirection);

    /*
    // OKADA 20061108
    // そもそも xrrtraytrace でこの関数を呼び出す時、すでに
    // Current を Virtual にセットしているため、 location.***
    // で関数を呼び出してきて良い
    fprintf(stdout,"location:        %lf %lf %lf in start of raytrace\n"
	    , telescopeXDirection
	    , telescopeYDirection
	    , telescopeZDirection);

    virtualLocation.getVectorDirectionXYZ(telescopeXDirection,
					  telescopeYDirection,
					  telescopeZDirection);
    fprintf(stdout,"virtualLocation: %lf %lf %lf in start of raytrace\n"
	    , telescopeXDirection
	    , telescopeYDirection
	    , telescopeZDirection);
    */

    angleOffsetThetaX = quadrant.getCurrentAngleOffsetThetaX();
    angleOffsetThetaY = quadrant.getCurrentAngleOffsetThetaY();
    angleOffsetThetaZ = quadrant.getCurrentAngleOffsetThetaZ();

    cosThetaX = cos(angleOffsetThetaX);
    sinThetaX = sin(angleOffsetThetaX);
    cosThetaY = cos(angleOffsetThetaY);
    sinThetaY = sin(angleOffsetThetaY);
    cosThetaZ = cos(angleOffsetThetaZ);
    sinThetaZ = sin(angleOffsetThetaZ);

	oldDirectionX = telescopeXDirection;
	oldDirectionY = telescopeYDirection;
	oldDirectionZ = telescopeZDirection;
	
    /*----------------------------------------------------------
      OKADA 20061108
      そもそもこれは photon のベクトルの座標変換なので、
      かける行列のθが座標変換のものと−倍ずれることに注意
    ------------------------------------------------------------*/
    /*
    // rotation around X-axis
    newDirectionX = telescopeXDirection;
    newDirectionY = cosThetaX * telescopeYDirection + sinThetaX * telescopeZDirection;
    newDirectionZ = cosThetaX * telescopeZDirection - sinThetaX * telescopeYDirection;
    */

    // OKADA 20061108
    // modified by Shunsaku Okada 2006/11/08
    newDirectionX = oldDirectionX;
    newDirectionY = cosThetaX * oldDirectionY - sinThetaX * oldDirectionZ;
    newDirectionZ = cosThetaX * oldDirectionZ + sinThetaX * oldDirectionY;

    oldDirectionX = newDirectionX;
    oldDirectionY = newDirectionY;
    oldDirectionZ = newDirectionZ;

    /*
    // rotation around Y-axis
    newDirectionX = cosThetaY * oldDirectionX - sinThetaY * oldDirectionZ;
    newDirectionY = oldDirectionY;
    newDirectionZ = cosThetaY * oldDirectionZ + sinThetaY * oldDirectionX;
    */

    // OKADA 20061108
    // modified by Shunsaku Okada 2006/11/08
    newDirectionX = cosThetaY * oldDirectionX + sinThetaY * oldDirectionZ;
    newDirectionY = oldDirectionY;
    newDirectionZ = cosThetaY * oldDirectionZ - sinThetaY * oldDirectionX;

    oldDirectionX = newDirectionX;
    oldDirectionY = newDirectionY;
    oldDirectionZ = newDirectionZ;

    // rotation around Z-axis
    newDirectionX = cosThetaZ * oldDirectionX - sinThetaZ * oldDirectionY;
    newDirectionY = cosThetaZ * oldDirectionY + sinThetaZ * oldDirectionX;
    newDirectionZ = oldDirectionZ;

    quadrantXDirection = newDirectionX;
    quadrantYDirection = newDirectionY;
    quadrantZDirection = newDirectionZ;

    /*
    // OKADA 20061108
    // modified by Shunsaku Okada 2006/11/08
    fprintf(stdout,"%lf %lf %lf\n", quadrantXDirection, quadrantYDirection
       , quadrantZDirection);
    UnitVectorMag arereXDirection;
    UnitVectorMag arereYDirection;
    UnitVectorMag arereZDirection;
    arereXDirection = quadrantXDirection;
    arereYDirection = quadrantYDirection;
    arereZDirection = quadrantZDirection;
    quadrantXDirection = cosThetaZ * oldDirectionX + sinThetaZ * oldDirectionY;
    quadrantYDirection = cosThetaZ * oldDirectionY - sinThetaZ * oldDirectionX;
    quadrantZDirection = oldDirectionZ;
    fprintf(stdout,"%lf %lf\n", quadrantXDirection, quadrantYDirection
	  , quadrantZDirection);
    fprintf(stdout,"%lf %lf %lf\n"
	    , quadrantXDirection - arereXDirection
	    , quadrantYDirection - arereYDirection
	    , quadrantZDirection - arereZDirection);
    */

    // Set photon parameters in the quadrant coordinate
    location.setXY(quadrantX, quadrantY);
    location.setFPDistance(quadrantZ);
    location.setVectorDirectionXYZ(quadrantXDirection,
				   quadrantYDirection,
				   quadrantZDirection);
}

//
// Add the argument of xrrtquadrant class to get the information 
// about the quadrant's offset
// (modified by Hideyuki MORI : date 2006/01/27)
//
void
XrrtPhoton::setQuadrantToTelescopeCoordinate(const XrrtQuadrant &quadrant)
{
    VertexInMM quadrantX;
    VertexInMM quadrantY;
    FPDistanceInMM quadrantZ;
    VertexInMM telescopeX;
    VertexInMM telescopeY;
    FPDistanceInMM telescopeZ;

    UnitVectorMag quadrantXDirection;
    UnitVectorMag quadrantYDirection;
    UnitVectorMag quadrantZDirection;

    UnitVectorMag telescopeXDirection;
    UnitVectorMag telescopeYDirection;
    UnitVectorMag telescopeZDirection;

    VertexInMM offsetX;
    VertexInMM offsetY;
    FPDistanceInMM offsetZ;

    AngleInRadians angleOffsetThetaX;
    AngleInRadians angleOffsetThetaY;
    AngleInRadians angleOffsetThetaZ;

    UnitVectorMag oldDirectionX;
    UnitVectorMag oldDirectionY;
    UnitVectorMag oldDirectionZ;

    UnitVectorMag newDirectionX;
    UnitVectorMag newDirectionY;
    UnitVectorMag newDirectionZ;

    double cosThetaX = 0.0e0;
    double sinThetaX = 0.0e0;
    double cosThetaY = 0.0e0;
    double sinThetaY = 0.0e0;
    double cosThetaZ = 0.0e0;
    double sinThetaZ = 0.0e0;

    location.getXY(&quadrantX, &quadrantY);
    quadrantZ = location.getFPDistance();

    offsetX = quadrant.getCurrentOffsetX();
    offsetY = quadrant.getCurrentOffsetY();
    offsetZ = quadrant.getCurrentOffsetZ();
/*	printf("currentQuadrant=%d, currentLayer=%d, offsetX=%f, offsetY=%f, offsetZ=%f\n",
		   quadrant.getCurrentQuadrant(), quadrant.getCurrentLayer(), offsetX, offsetY, offsetZ);*/

    telescopeX = quadrantX + offsetX;
    telescopeY = quadrantY + offsetY;
    telescopeZ = quadrantZ + offsetZ;

    location.getVectorDirectionXYZ(quadrantXDirection,
				   quadrantYDirection,
				   quadrantZDirection);

    angleOffsetThetaX = quadrant.getCurrentAngleOffsetThetaX();
    angleOffsetThetaY = quadrant.getCurrentAngleOffsetThetaY();
    angleOffsetThetaZ = quadrant.getCurrentAngleOffsetThetaZ();
    
    cosThetaX = cos(angleOffsetThetaX);
    sinThetaX = sin(angleOffsetThetaX);
    cosThetaY = cos(angleOffsetThetaY);
    sinThetaY = sin(angleOffsetThetaY);
    cosThetaZ = cos(angleOffsetThetaZ);
    sinThetaZ = sin(angleOffsetThetaZ);


    /*----------------------------------------------------------
      OKADA 20061108
      そもそもこれは photon のベクトルの座標変換なので、
      かける行列のθが座標変換のものと−倍ずれることに注意
    ------------------------------------------------------------*/

	oldDirectionX = quadrantXDirection;
	oldDirectionY = quadrantYDirection;
	oldDirectionZ = quadrantZDirection;

    // rotation around Z-axis
    newDirectionX = cosThetaZ * oldDirectionX + sinThetaZ * oldDirectionY;
    newDirectionY = cosThetaZ * oldDirectionY - sinThetaZ * oldDirectionX;
    newDirectionZ = oldDirectionZ;

    oldDirectionX = newDirectionX;
    oldDirectionY = newDirectionY;
    oldDirectionZ = newDirectionZ;

    // rotation around Y-axis
    newDirectionX = cosThetaY * oldDirectionX - sinThetaY * oldDirectionZ;
    newDirectionY = oldDirectionY;
    newDirectionZ = cosThetaY * oldDirectionZ + sinThetaY * oldDirectionX;

    oldDirectionX = newDirectionX;
    oldDirectionY = newDirectionY;
    oldDirectionZ = newDirectionZ;

    // rotation around X-axis
    newDirectionX = oldDirectionX;
    newDirectionY = cosThetaX * oldDirectionY + sinThetaX * oldDirectionZ;
    newDirectionZ = cosThetaX * oldDirectionZ - sinThetaX * oldDirectionY;

    telescopeXDirection = newDirectionX;
    telescopeYDirection = newDirectionY;
    telescopeZDirection = newDirectionZ;

    // Set photon parameters in the telescope (base) coordinate
    location.setXY(telescopeX, telescopeY);
    location.setFPDistance(telescopeZ);
    location.setVectorDirectionXYZ(telescopeXDirection,
				   telescopeYDirection,
				   telescopeZDirection);
}
