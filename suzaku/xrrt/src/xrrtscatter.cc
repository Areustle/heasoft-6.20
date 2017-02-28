// xrrtscatter.cc
//
// Member functions for XrrtScatter class
//
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/25 Upgrade documentation. R. Fink
// 1998/12/08 Modify setAscaParams to allow gamma to be zero.
// 1999/02/16 Modify astroe scattering to account for the Astro-E quadrants
//			being rotated by 45 degrees when mounted in the spacecraft.
//			R Fink
// 2005/12/15 Y.ISHISAKI	version 6.2.2
//	rewrite setAstroeBackParams(), setAstroeBxrrtback()
// 2006/06/19 Y.ISHISAKI	version 6.3.9
//	remove #include "xrrtscatter.hh", which is included in "xrrtraytrace.hh"
//
// 2007/04/05 Y.ISHISAKI	version 6.4.5
//	add XrrtScatter::gauRandom(), expRandom(), lorRandom()
//	rewrite XrrtScatter::astroeScat()
//	change XrrtScatter::astroeFdiffract() -> astroeFdiffract() [global func]
//
// 2008/01/24 K. SOMEYA     version 6.4.6
//	change w_norm, w_power, g_norm
//
// 2008/03/02 Y.ISHISAKI	version 6.5.0
//	back astroeFdiffract() [global func] -> XrrtScatter::astroeFdiffract()
//	set defaultScatterModeSet = false, if mode == -1
//	check backprofFileName == "NONE" in XrrtScatter::setAstroeBackParams()
//	include "xrrtdefaults.hh"

#include <cmath>
#include "xrrtraytrace.hh"
#include "xrrtdefaults.hh"

bool
XrrtScatter::getAstroeParamStatus()
{
	return astroeParamsSet;
}

inline double
XrrtScatter::astroeFfluct(double rx)
{
	const double PI = M_PI;		// 3.14159265358979323846;

	double x = rx / PI * 180.0 * 60.0;

	double gau = astroeNgau*exp(-x*x*astroeGaussSigma2InvHalf);
	double absexp = astroeNexp*exp(-fabs(x)*astroeExpSigmaInv);
	double tmp = 2.0*x*astroeLorentzGammaInv;
	double lor = astroeNlor/(1+(tmp*tmp));
	return (gau+absexp+lor);
}

inline void
XrrtScatter::astroeStprnd(double& rx, double& y)
{
	double rs;
	double absRs;
	rs = xrrtrandom() - 0.5;
	absRs = fabs(rs);

	double s0PlusS1 = astroeStps0+astroeStps1;
	if (absRs <= astroeStps0) {
		rx = absRs/astroeStpy0;
		y  = astroeStpy0*astroeStpc;
	} else if ((absRs > astroeStps0) && (absRs <= s0PlusS1)) {
		rx=(absRs-astroeStps0)/astroeStpy1+astroeStpx1;
		y=astroeStpy1*astroeStpc;
	} else if ((absRs > s0PlusS1) && (absRs <= 5.0e-1)) {
		rx=(absRs - s0PlusS1)/astroeStpy2+astroeStpx2;
		y=astroeStpy2*astroeStpc;
	} else {
		throw rxOutOfRegion;
	}
	if (rs <= 0.0) {
		rx = -1.0 * rx;
	}
	return;
}

// gaussian random routine
//		2007/04/05 Y.ISHISAKI	adopted from aste_drndtsg() in astetool-1.84
inline double
XrrtScatter::gauRandom(void)
{
	static int ncall = 0;
	static double save;

	double fact, theta, ran0, ran1;

	ncall++;
	if ( ncall & 1 ) {
		ran0 = xrrtrandom();
		ran1 = xrrtrandom();
		while ( 0.0 == ran0 ) {
			ran0 = xrrtrandom();
		}
		fact = sqrt(-2*log(ran0));
		theta = 2 * M_PI * ran1;
		save = fact * sin(theta);
		return fact * cos(theta);
	}
	return save;
}

// exponential random routine
//		2007/04/05 Y.ISHISAKI
inline double
XrrtScatter::expRandom(void)
{
	int isign;
	double ran;

	ran = xrrtrandom();
	while ( 0.0 == ran ) {
		ran = xrrtrandom();
	}

	isign = 1;
	ran *= 2;					/* 0 < ran < 2.0 */
	if ( 1.0 <= ran ) {
		isign = -1;
		ran -= 1.0;				/* 0 <= ran < 1.0 */
	}

	ran = isign * log(1 - ran);

	return ran;
}

// lorentzian random routine
//		2007/04/05 Y.ISHISAKI
inline double
XrrtScatter::lorRandom(void)
{
	double ran;

	ran = xrrtrandom();
	while ( 0.0 == ran ) {
		ran = xrrtrandom();
	}
	ran = 0.5 * M_PI * (ran - 0.5);	/* -PI/2 < ran < PI/2 */
	ran = tan(ran);

	return ran;
}

inline double
XrrtScatter::astroeNvfluct(void)
{
	const double ARCMIN2RADIAN = M_PI / (180.0 * 60.0);
	double ran;

	ran = xrrtrandom();
	if ( ran < astroeBRgau ) {
		ran = astroeGaussSigma * gauRandom();
	} else if ( ran < astroeBRgauPlusBRexp ) {
		ran = astroeExpSigma * expRandom();
	} else {
		ran = astroeLorentzGamma * lorRandom();
	}
	ran *= ARCMIN2RADIAN;

	return ran;
}

inline double
XrrtScatter::astroeBfluct(double rx, double IncidentAngleInRadians, double energyInKeV)
{
	const double PI = M_PI;		// 3.14159265358979323846;
	double x, y;
	// double gau, lor;

	x = rx / PI * 180.0 * 60.0;   // arcmin
	
	// gau = astroeBackNgau*exp(-x*x/2.0/astroeBackSigma/astroeBackSigma);
	// lor = astroeBackNlor/(1+2.0*2.0*x*x/astroeBacklorSigma/astroeBacklorSigma);
	
	// printf("%f %f %f\n", x, IncidentAngleInRadians*180.0/PI, energyInKeV);

	setAstroeBxrrtback(IncidentAngleInRadians, energyInKeV);
	y = astroeBxrrtback(rx);

	// return (gau+lor);
	return y;
}

inline void
XrrtScatter::astroeBackStprnd(double& rx, double& y)
{
	int i = 0;
	double integ = 0.0;
	double rs = xrrtrandom() / backPitch;

	for (i = 0; i < numBackProf; i++) {
		double integ2 = integ + astroeBackStpy[i];
		if ( rs < integ2 ) {
			rx = astroeBackStpx[i] + backPitch*(rs-integ)/astroeBackStpy[i];
			y = astroeBackStpy[i] * backNorm;
//			printf("XrrtScatter::astroeBackStprnd(): rx=%f ry=%f\n", rx, y);
			return;
		}
		integ = integ2;
	}

	rx = astroeBackStpx[i-1];
	y = astroeBackStpy[i-1] * backNorm;
}

inline double
XrrtScatter::astroeBacksideNvfluct(double IncidentAngleInRadians, double energyInKeV)
{
	double rx;
	double y;
	double ry;
	double fx;

	setAstroeBxrrtback(IncidentAngleInRadians, energyInKeV);
	do {
		astroeBackStprnd(rx, y);
		ry = xrrtrandom()*y;
		fx = astroeBfluct(rx, IncidentAngleInRadians, energyInKeV);
	} while (ry > fx);

	return rx;
}

// Add for ASTRO-E2 Pre-Collimator Normal Vector Fluctuation
// (modified by H. Mori : date 2003/01/22)
inline double
XrrtScatter::astroePcolNvfluct(double IncidentAngleInRadians)
{
	const double PI = M_PI;		// 3.14159265358979323846;
	double rx;
	double ry;
	double gc;
	double gw;
	//	double gn;
	
	ry = IncidentAngleInRadians / PI * 180.0;

//	gc = -0.6429944 * ry + 0.2735732;
//	gw = 0.2891246 * ry + 0.1206682;

	gc = astroePcolGcA * ry + astroePcolGcB;
	gw = astroePcolGwA * ry + astroePcolGwB;

//	rx = gw * xrrtgaussrandom() + gc;
	rx = gw * gauRandom() + gc;

	return rx;
}

XrrtScatter::XrrtScatter():
defaultScatterModeSet(false),
defaultScatterMode(DEFAULT_SCATTER_MODE),
ascaParamsSet(false),
ascaConsct(DEFAULT_ASCA_CONSCT),
ascaSigma1(DEFAULT_ASCA_SIGMA1),
ascaSigma2(DEFAULT_ASCA_SIGMA2),
ascaAlin(DEFAULT_ASCA_LALIN),
ascaS3(DEFAULT_ASCA_CALSS3),
ascaC3(DEFAULT_ASCA_CALSC3),
ascaArd(0),
ascaBrd(0),
ascaCrd(0),
ascaXmaxrd(0),
ascaStpx1(0),
ascaStpx2(0),
ascaStpy0(0),
ascaStpy1(0),
ascaStpy2(0),
ascaStps0(0),
ascaStps1(0),
ascaStps2(0),
ascaStpc(0),
astroeParamsSet(false),
astroeNvfsw(DEFAULT_SUZAKU_NVFSW),
astroeGaussSigma(DEFAULT_SUZAKU_GAUSIGMA),
astroeNgau(DEFAULT_SUZAKU_NGAU),
astroeExpSigma(DEFAULT_SUZAKU_EXPSIGMA),
astroeNexp(DEFAULT_SUZAKU_NEXP),
astroeLorentzGamma(DEFAULT_SUZAKU_LORGAMMA),
astroeNlor(DEFAULT_SUZAKU_NLOR),
astroeScatsw(DEFAULT_SUZAKU_SCATSW),
astroeWnorm(DEFAULT_SUZAKU_W_NORM),
astroeWpower(DEFAULT_SUZAKU_W_POWER),
astroeGnorm(DEFAULT_SUZAKU_G_NORM),
astroeStpx1(0),
astroeStpx2(0),
astroeStpy0(0),
astroeStpy1(0),
astroeStpy2(0),
astroeStps0(0),
astroeStps1(0),
astroeStps2(0),
astroeStpc(0),
astroePcolGcA(DEFAULT_SUZAKU_PCOL_GC_A),
astroePcolGcB(DEFAULT_SUZAKU_PCOL_GC_B),
astroePcolGwA(DEFAULT_SUZAKU_PCOL_GW_A),
astroePcolGwB(DEFAULT_SUZAKU_PCOL_GW_B)
{
	// A simple constructor
}

XrrtScatter&
theScatterInfo()
{
	//
	// Return the static scatter object for the program.
	//
	static XrrtScatter scatterInfo;

	return scatterInfo;
}

string
XrrtScatter::errorMessage(XrrtScatterErrorCode errorCode)
{
	//
	// Convert error codes to error messages.
	//
	string errorMessage;

	switch (errorCode) {
	case noSuchScatterMode:
		errorMessage = 
		"A scatter mode was requested that has no code to support it";
		break;
	case rxOutOfRegion:
		errorMessage = 
		"XrrtScatter::ascaStprnd went out of valid range";
		break;
	default:
		char charNumber[1024];
		sprintf(charNumber, "%d",errorCode);
		errorMessage =
		"XrrtScatter::errorMessage Unknown error code: ";
		errorMessage.append(charNumber);
		break;
	}

	return errorMessage;
}

void
XrrtScatter::setAscaParams(const int& consct,
						   const double& sigma1,
						   const double& sigma2,
						   const double& alin,
						   const double& s3,
						   const double& c3)
{
	//
	// This code was distilled from the ASCA scatter code of Nagoya's
	// ray2areav6_0cal program.
	//
	const double xrrtPI = M_PI;		// 3.1415926535897932385e0;
	const double x1 = 5.0e-1;
	const double x2 = 5.0;
	const double xmax = 1.2e2;

	ascaConsct = consct;
	ascaSigma1 = sigma1;
	ascaSigma2 = sigma2;
	ascaAlin   = alin;
	ascaS3	 = s3;
	ascaC3	 = c3;
	ascaParamsSet = true;

	// Compute additional ASCA params
	double gamma = ascaSigma1*60.0;
	if ( gamma > 0.0 ) {
		ascaArd = ascaAlin*6.e1*1.8e2/xrrtPI;
		double b = gamma/2.0;
		ascaBrd = b/6.e1/1.8e2*xrrtPI;
		ascaXmaxrd = xmax/6.e1/1.8e2*xrrtPI;
		ascaCrd = 1.0 /
		(ascaArd * log(xmax*xmax/b/b+1.0) + 2.0 / ascaBrd * atan(xmax/b));
		
		// Compute additional ASCA params
		ascaStpx1=x1/6.e1/1.8e2*xrrtPI;
		ascaStpx2=x2/6.e1/1.8e2*xrrtPI;
		double y0=1.1e0*ascaFfluct(0.0);
		double s0=y0*ascaStpx1;
		double y1=1.1e0*ascaFfluct(ascaStpx1);
		double s1=y1*(ascaStpx2-ascaStpx1);
		double y2=1.1e0*ascaFfluct(ascaStpx2);
		double s2=y2*(ascaXmaxrd-ascaStpx2);
		ascaStpc=2.0*(s0+s1+s2);
		
		ascaStpy0=y0/ascaStpc;
		ascaStpy1=y1/ascaStpc;
		ascaStpy2=y2/ascaStpc;
		ascaStps0=s0/ascaStpc;
		ascaStps1=s1/ascaStpc;
		ascaStps2=s2/ascaStpc;
	}
}

void
XrrtScatter::frontScatter(const XrrtMirror* mirror, XrrtPhoton& photon)
{
	//
	// Handle photon scatter off front mirror surfaces.
	//
	SurfaceScatterMode scatterMode;

	if (defaultScatterModeSet) {
		scatterMode = defaultScatterMode;
	} else {
		scatterMode = mirror->getScatterMode();
	}

	if (scatterMode == NO_SCATTER) {
		// The photon is at the scatter point on the mirror.
		// The virtual photon contains the direction of the photon due to
		// pure reflection. So all we have to do is promote the virtual
		// photon to be the current photon.
		photon.setCurrentToVirtual();
		return;
	} else if (scatterMode == ASCA_SCATTER) {
		// Apply ASCA scattering to the virtual photon
		ascaScatter(mirror, photon);
		photon.setCurrentToVirtual();
	} else if (scatterMode == ASTROE_SCATTER) {
		astroeFrontScatter(mirror, photon);
		photon.setCurrentToVirtual();
	} else {
		throw noSuchScatterMode;
	}
}

void
XrrtScatter::backScatter(const XrrtMirror* mirror, XrrtPhoton& photon)
{
	//
	// Handle photon scatter off mirror back surfaces.
	//
	SurfaceScatterMode scatterMode;
	
	if (defaultScatterModeSet) {
		scatterMode = defaultScatterMode;
	} else {
		scatterMode = mirror->getScatterMode();
	}

	if (scatterMode == NO_SCATTER) {	 // no read by iizuka
		// The photon is at the scatter point on the mirror.
		// The virtual photon contains the direction of the photon due to
		// pure reflection. So all we have to do is promote the virtual
		// photon to be the current photon.
		photon.setCurrentToVirtual();
		return;
	} else if (scatterMode == ASCA_SCATTER) {
		// ASCA provides no special scatter behavior for back surfaces
		// so we use the no scatter case

		// The photon is at the scatter point on the mirror.
		// The virtual photon contains the direction of the photon due to
		// pure reflection. So all we have to do is promote the virtual
		// photon to be the current photon.
		photon.setCurrentToVirtual();
		return;
	} else if (scatterMode == ASTROE_SCATTER) {	 //  <- read !!!!!! by iizuka
		astroeBackScatter(mirror, photon);
		photon.setCurrentToVirtual();
	} else {
		throw noSuchScatterMode;
	}

}

// Add the case of Pre-Collimator scatter for ASTRO-E2
// This member function refers to above function for XrrtMirror
// (modified by H. Mori : date 2003/01/14)
void
XrrtScatter::pcolScatter(const XrrtCollimator* collimator, XrrtPhoton& photon)
{
	//
	// Handle photon scatter off Pre-Collimator surfaces.
	//
	ColSurfaceScatterMode scatterMode;

	if (defaultScatterModeSet) {
		scatterMode = defaultScatterMode;
	} else {
		scatterMode = collimator->getColScatterMode();
	}

	if (scatterMode == NO_SCATTER) {
		// The photon is at the scatter point on the Pre-Collimator.
		// The virtual photon contains the direction of the photon due to
		// pure reflection. So all we have to do is promote the virtual
		// photon to be the current photon.
		photon.setCurrentToVirtual();
		return;
	} else if (scatterMode == ASCA_SCATTER) {
		// ASCA is out of consideration to use Pre-Collimator.
		// so we use no scatter case.
		photon.setCurrentToVirtual();
	} else if (scatterMode == ASTROE_SCATTER) {
		astroePcolScatter(collimator, photon);
		photon.setCurrentToVirtual();
	} else {
		throw noSuchScatterMode;
	}

}

void
XrrtScatter::ascaScatter(const XrrtMirror* mirror, XrrtPhoton& photon)
{
	// The photon is at the scatter point on the mirror.
	// The virtual photon contains the direction of the photon due to
	// pure reflection.

	// Replicate the Nagoya ASCA scatting code as closely as possible
	// The code from RAY2AREAV6_0CAL is found in RAYCHA and called
	// routines
	//
	const double DEG2RAD = 0.017453292519943295769;
	const double SCT2EDGE1 = 6.428e0 * DEG2RAD;
	const double SCT2EDGE2 = 12.857e0 * DEG2RAD;
	const double SCT13EDGE1 = 90.0*DEG2RAD - SCT2EDGE2;
	const double SCT13EDGE2 = 90.0*DEG2RAD - SCT2EDGE1;
	const double xrrtPI = M_PI;		// 3.14159265358979323846;
	const double PI_OVER_2 = xrrtPI/2.0e0;

	double broadenAngle = 0;
	double reflectionAngle;
	
	double mirrorXDirection;
	double mirrorYDirection;
	double mirrorZDirection;
	XrrtVector mirrorFrontVector = 
	mirror->getPlaneMirrorFrontVector(photon.getRotationAngle());
	mirrorFrontVector.getVectorDirectionXYZ(mirrorXDirection, 
											mirrorYDirection, 
											mirrorZDirection);
	double photonXDirection;
	double photonYDirection;
	double photonZDirection;
	photon.getVirtualDirectionXYZ(photonXDirection, 
								  photonYDirection, 
								  photonZDirection);
	reflectionAngle = acos(mirrorXDirection*photonXDirection +
						   mirrorYDirection*photonYDirection +
						   mirrorZDirection*photonZDirection);
	
	double rotationAngle = photon.getRotationAngle();
	double ascaSectorAngle = fmod(rotationAngle, PI_OVER_2);
	double energyInKeV = photon.getEnergy();
	if ( ascaConsct >= 1 || ascaSigma1 > 0.0 ) {
	broadenLoop:
		if ( ascaConsct >= 1 ) {
			broadenAngle = ascaScat(reflectionAngle,energyInKeV);
		}
		if ( ascaSigma1 > 0.0 ) {
			if ( ascaSigma2 > 0.0 ) {
				if ((ascaSectorAngle>SCT2EDGE1 && ascaSectorAngle<SCT2EDGE2) ||
					(ascaSectorAngle>SCT13EDGE1 && ascaSectorAngle<SCT13EDGE2)
					 ) {
					broadenAngle = broadenAngle + ascaFlor(ascaSigma2);
				} else {
					broadenAngle = broadenAngle + ascaNvfluct();
				}
			} else {
				broadenAngle = broadenAngle + ascaNvfluct();
			}
		}
		if ( reflectionAngle + broadenAngle <= 0.0 ) {
			broadenAngle = 0.0;
			goto broadenLoop;
		}
		// Apply the broadening to the reflection photon direction vector
		double sphericalCoordTheta = acos(-photonZDirection);
		double cosb = sin(sphericalCoordTheta+broadenAngle) /
		sin(sphericalCoordTheta);
		photonXDirection = photonXDirection*cosb;
		photonYDirection = photonYDirection*cosb;
		photonZDirection = -cos(sphericalCoordTheta+broadenAngle);
		photon.setVirtualDirectionXYZ(photonXDirection,
									  photonYDirection,
									  photonZDirection);
		return;
	}
}

void
XrrtScatter::astroeFrontScatter(const XrrtMirror* mirror, XrrtPhoton& photon)
{
	// The photon is at the scatter point on the mirror.
	// The virtual photon contains the direction of the photon due to
	// pure reflection.

	const double DEG2RAD = 0.017453292519943295769;
	//	  const double RAD2MIN = 1.0 / DEG2RAD * 60.0;
	//	  const double SCT2EDGE1 = 6.428e0 * DEG2RAD;
	//	  const double SCT2EDGE2 = 12.857e0 * DEG2RAD;
	//	  const double SCT13EDGE1 = 90.0*DEG2RAD - SCT2EDGE2;
	//	  const double SCT13EDGE2 = 90.0*DEG2RAD - SCT2EDGE1;
	const double xrrtPI = M_PI;		// 3.14159265358979323846;
	//	  const double PI_OVER_2 = xrrtPI/2.0e0;

	double reflectionAngle;
	
	double mirrorXDirection;
	double mirrorYDirection;
	double mirrorZDirection;
	XrrtVector mirrorFrontVector = 
	mirror->getPlaneMirrorFrontVector(photon.getRotationAngle());

	mirrorFrontVector.getVectorDirectionXYZ(mirrorXDirection, 
											mirrorYDirection, 
											mirrorZDirection);
	double photonXDirection;
	double photonYDirection;
	double photonZDirection;
	photon.getVirtualDirectionXYZ(photonXDirection, 
								  photonYDirection, 
								  photonZDirection);
	reflectionAngle = acos(mirrorXDirection*photonXDirection +
						   mirrorYDirection*photonYDirection +
						   mirrorZDirection*photonZDirection);
	
	double rotationAngle = photon.getRotationAngle();
	//
	// Astro-E quadrants are rotated by +45.0 degrees around the z-axis.
	// so the angle in the quadrant is the photon angle MINUS 45 degrees.
	// Photon angle = 90 degrees = quadrant 1 angle of 45 degrees.
	// Quadrant 1 spans from 45 -> 135 degrees.
	double astroeQuadrantAngle = rotationAngle - (45.0*DEG2RAD);

	if ( astroeQuadrantAngle < 0.0 ) {
		astroeQuadrantAngle = astroeQuadrantAngle + xrrtPI*2.0e0;
	}
	//	  double astroeSectorAngle = fmod(astroeQuadrantAngle, PI_OVER_2);
	double energyInKeV = photon.getEnergy();

 broadenLoop:
	double broadenAngle = 0;
	if ( astroeNvfsw ) {
		broadenAngle = astroeNvfluct();
	}
	if ( astroeScatsw ) {
		//	cout << "call astroeScat on frontside\n";
		// added code for the investigation of scattering angle
		// (by Hideyuki Mori, 2002/12/17)
		double tmp = astroeScat(mirror,reflectionAngle,energyInKeV);
//		if ( tmp != 0.0 ) {
//			printf("%lf %lf %lf ", reflectionAngle*180.0/xrrtPI*60.0, broadenAngle*180.0/xrrtPI*60.0, tmp*180.0/xrrtPI*60.0);
//		}
		broadenAngle = broadenAngle + tmp;
	}

	if ( reflectionAngle + broadenAngle <= 0.0 ) {
		broadenAngle = 0.0;
		goto broadenLoop;
	}
	double sphericalCoordTheta = acos(-photonZDirection);
	double cosb = sin(sphericalCoordTheta+broadenAngle) / sin(sphericalCoordTheta);
	photonXDirection = photonXDirection*cosb;
	photonYDirection = photonYDirection*cosb;
	photonZDirection = -cos(sphericalCoordTheta+broadenAngle);
	photon.setVirtualDirectionXYZ(photonXDirection,
								  photonYDirection,
								  photonZDirection);
	return;
}

inline double
XrrtScatter::astroeBacksideScat(const XrrtMirror* mirror, double reflectAngleInRadians, double energyInKeV)
{
	return 0.0;
}

void
XrrtScatter::astroeBackScatter(const XrrtMirror* mirror, XrrtPhoton& photon)
{
	// The photon is at the scatter point on the mirror.
	// The virtual photon contains the direction of the photon due to
	// pure reflection.

	const double DEG2RAD = 0.017453292519943295769;
	//	  const double SCT2EDGE1 = 6.428e0 * DEG2RAD;
	//	  const double SCT2EDGE2 = 12.857e0 * DEG2RAD;
	//	  const double SCT13EDGE1 = 90.0*DEG2RAD - SCT2EDGE2;
	//	  const double SCT13EDGE2 = 90.0*DEG2RAD - SCT2EDGE1;
	const double xrrtPI = M_PI;		// 3.14159265358979323846;
	//	  const double PI_OVER_2 = xrrtPI/2.0e0;

	double reflectionAngle;
	
	double mirrorXDirection;
	double mirrorYDirection;
	double mirrorZDirection;
	XrrtVector mirrorBackVector = 
	mirror->getPlaneMirrorBackVector(photon.getRotationAngle());

	mirrorBackVector.getVectorDirectionXYZ(mirrorXDirection, 
										   mirrorYDirection, 
										   mirrorZDirection);
	double photonXDirection;
	double photonYDirection;
	double photonZDirection;
	photon.getVirtualDirectionXYZ(photonXDirection, 
								  photonYDirection, 
								  photonZDirection);
	reflectionAngle = acos(mirrorXDirection*photonXDirection +
						   mirrorYDirection*photonYDirection +
						   mirrorZDirection*photonZDirection);
	
	double rotationAngle = photon.getRotationAngle();
	//
	// Astro-E quadrants are rotated by +45.0 degrees around the z-axis.
	// so the angle in the quadrant is the photon angle MINUS 45 degrees.
	// Photon angle = 90 degrees = quadrant 1 angle of 45 degrees.
	// Quadrant 1 spans from 45 -> 135 degrees.
	double astroeQuadrantAngle = rotationAngle - (45.0*DEG2RAD);
	if ( astroeQuadrantAngle < 0.0 ) {
		astroeQuadrantAngle = astroeQuadrantAngle + xrrtPI*2.0e0;
	}
	//	  double astroeSectorAngle = fmod(astroeQuadrantAngle, PI_OVER_2);
	double energyInKeV = photon.getEnergy();

 broadenLoop:
	double broadenAngle = 0;
	if ( astroeNvfsw ) {
		broadenAngle = astroeBacksideNvfluct(reflectionAngle, energyInKeV);			  /* read by iizuka !!*/
	}
	if ( astroeScatsw ) {
		broadenAngle = broadenAngle + astroeBacksideScat(mirror,reflectionAngle,energyInKeV);	/* unread by iizuka !!*/
	}
	if ( reflectionAngle + broadenAngle <= 0.0 ) {
		broadenAngle = 0.0;
		goto broadenLoop;
	}
	//	  double reflectionAngleDegree = reflectionAngle * 1.8e2 * 6.0e1 / xrrtPI;
	//	  if(reflectionAngleDegree > 1.4e1 && reflectionAngleDegree < 1.6e1){
	//	fprintf(stdout, "%lf\n", (broadenAngle + reflectionAngle) * 1.8e2 * 6.0e1 / xrrtPI);
	//	  }
	double foilAngle = acos(-mirrorZDirection);  // radian
	if ( reflectionAngle < foilAngle) {
		broadenAngle = -broadenAngle;
	}
	double sphericalCoordTheta = acos(-photonZDirection);
	double cosb = sin(sphericalCoordTheta+broadenAngle) / sin(sphericalCoordTheta);

	// printf("%f %f %f %f %f %f %f %f %f %f\n", reflectionAngle*180.0/xrrtPI, broadenAngle*180.0/xrrtPI, photonXDirection, photonYDirection, photonZDirection, mirrorXDirection, mirrorYDirection, mirrorZDirection, sphericalCoordTheta*180/xrrtPI, foilAngle*180.0/xrrtPI);

	photonXDirection = photonXDirection*cosb;
	photonYDirection = photonYDirection*cosb;
	photonZDirection = -cos(sphericalCoordTheta+broadenAngle);
	photon.setVirtualDirectionXYZ(photonXDirection,
								  photonYDirection,
								  photonZDirection);
	return;
}

inline double
XrrtScatter::astroePcolScat(const XrrtCollimator* collimator, double reflectAngleInRadians, double energyInKeV)
{
	return 0.0;
}

// Add the case of ASTRO-E2 Pre-Collimator scatter
// (added by H. Mori : date 2003/01/14)
void
XrrtScatter::astroePcolScatter(const XrrtCollimator* collimator, XrrtPhoton& photon)
{
	// The photon is at the scatter point on the Pre-Collimator.
	// The virtual photon contains the direction of the photon due to
	// pure reflection.

	const double DEG2RAD = 0.017453292519943295769;
	//	  const double SCT2EDGE1 = 6.428e0 * DEG2RAD;
	//	  const double SCT2EDGE2 = 12.857e0 * DEG2RAD;
	//	  const double SCT13EDGE1 = 90.0*DEG2RAD - SCT2EDGE2;
	//	  const double SCT13EDGE2 = 90.0*DEG2RAD - SCT2EDGE1;
	const double xrrtPI = M_PI;		// 3.14159265358979323846;
	//	  const double PI_OVER_2 = xrrtPI/2.0e0;

	double reflectionAngle;
	
	double collimatorXDirection;
	double collimatorYDirection;
	double collimatorZDirection;
	XrrtVector collimatorFrontVector = 
	collimator->getPlaneCollimatorFrontVector(photon.getRotationAngle());

	collimatorFrontVector.getVectorDirectionXYZ(collimatorXDirection, 
												collimatorYDirection, 
												collimatorZDirection);
	double photonXDirection;
	double photonYDirection;
	double photonZDirection;
	photon.getVirtualDirectionXYZ(photonXDirection, 
								  photonYDirection, 
								  photonZDirection);
	reflectionAngle = acos(collimatorXDirection*photonXDirection +
						   collimatorYDirection*photonYDirection +
						   collimatorZDirection*photonZDirection);
	
	double rotationAngle = photon.getRotationAngle();
	//
	// Astro-E quadrants are rotated by +45.0 degrees around the z-axis.
	// so the angle in the quadrant is the photon angle MINUS 45 degrees.
	// Photon angle = 90 degrees = quadrant 1 angle of 45 degrees.
	// Quadrant 1 spans from 45 -> 135 degrees.
	double astroeQuadrantAngle = rotationAngle - (45.0*DEG2RAD);
	if ( astroeQuadrantAngle < 0.0 ) {
		astroeQuadrantAngle = astroeQuadrantAngle + xrrtPI*2.0e0;
	}
	//	  double astroeSectorAngle = fmod(astroeQuadrantAngle, PI_OVER_2);
	double energyInKeV = photon.getEnergy();

 broadenLoop:
	double broadenAngle = 0;
	if ( astroeNvfsw ) {
		broadenAngle = astroePcolNvfluct(reflectionAngle);
	}
	if ( astroeScatsw ) {
		broadenAngle = broadenAngle + astroePcolScat(collimator,reflectionAngle, energyInKeV);
	}

	if ( reflectionAngle + broadenAngle <= 0.0 ) {
		broadenAngle = 0.0;
		goto broadenLoop;
	}

	double sphericalCoordTheta = acos(-photonZDirection);
	double cosb = sin(sphericalCoordTheta+broadenAngle) / sin(sphericalCoordTheta);
	photonXDirection = photonXDirection*cosb;
	photonYDirection = photonYDirection*cosb;
	photonZDirection = -cos(sphericalCoordTheta+broadenAngle);
	photon.setVirtualDirectionXYZ(photonXDirection,
								  photonYDirection,
								  photonZDirection);

	return;
}

double
XrrtScatter::ascaScat(double reflectAngleInRadians, double energyInKeV)
{
	const double DEG2RAD = 0.017453292519943295769;
	const double MIN2RAD = DEG2RAD/60.0;

	// correct for calling code out of order
	if ( reflectAngleInRadians < 0.0 ) {
		reflectAngleInRadians = -reflectAngleInRadians;
	}
	double f;
	double fc;
	double core = 0.8840e0;
	double r1;
	double r2;
	double absR1;
	double rthr;
	double pwave;
	//	double absPwave;
	double fscat;
	// following were created in hope of faster inner loop code
	double fscatHoist1 = 68.8797 * (1.0+ascaS3) * 
			energyInKeV*energyInKeV*energyInKeV *
			sin(reflectAngleInRadians);
	double fscatHoist2 = -9329.0*(1.0+ascaC3);
	double fscatTemp = 0.0;
	double pwaveHoist = 0.50671 * energyInKeV;

	// Generation of random numbers
 randomNumbers:
	r1 = (xrrtrandom() - 0.5e0)*100.0;
	r2 = xrrtrandom();
	absR1 = fabs(r1);
	rthr = r1*MIN2RAD;
	// Calculation of core distribution
	if ((-rthr) > reflectAngleInRadians) {
		// Unphysical scatter angle
		goto randomNumbers;
	}
	// This accelerates the calculation of the scatter distribution by
	// bypassing large parts of the r1Xr2 random plane where physical
	// solutions so not exist. It does cause the function output to be
	// warped for large angles.
	if (absR1 >= 2.0) {
		if (absR1 < 10.0) {
			if (r2 > 0.2e0) {
				goto randomNumbers;
			}
		} else {
			if (r2 > 0.015e0) {
				goto randomNumbers;
			}
		}
		f = log10(absR1);
		double temp = -0.5456e0-1.71887e0*f-0.70664e0*f*f;
		fc = pow(10.0, temp);
	} else {
		f = r1/core;
		fc = exp(-f*f/2.0);
	}
	// Calculation of scattering component
	//   pwave = 0.50671e0 * energyInKeV * 
	//				  sin(reflectAngleInRadians+rthr/2.0) * sin(rthr);
	//   absPwave = fabs(pwave);
	//   fscat = 68.8797e0 * (1.0+ascaS3) * pow(energyInKeV,3) *
	//		   sin(reflectAngleInRadians) *
	//		   sin(reflectAngleInRadians+rthr) * sin(reflectAngleInRadians+rthr)*
	//		   exp(-9329.0*(1.0+ascaC3)*absPwave);
	pwave = pwaveHoist * sin(reflectAngleInRadians+rthr/2.0) * sin(rthr);
	fscatTemp = sin(reflectAngleInRadians+rthr);
	fscat = fscatHoist1 * fscatTemp * fscatTemp *
	exp(fscatHoist2*fabs(pwave));
	if ( 2 == ascaConsct ) {
		fscat = 0.0;
	} else if ( 3 <= ascaConsct ) {
		fc = 0.0;
	}
	f = (fc+fscat)*0.8e0;
	// Are we inside the distribution?
	if (r2 > f) {
		// try another pair of random numbers
		goto randomNumbers;
	}
	return rthr;
}

double
XrrtScatter::astroeScat(const XrrtMirror* mirror, double reflectAngleInRadians, double energyInKeV)
{
	// correct for calling code out of order
	if ( reflectAngleInRadians < 0.0 ) {
		reflectAngleInRadians = -reflectAngleInRadians;
	}

	double prob, ran;

	XrrtTable* table = mirror->getFrontReflectTable();
	prob = table->getScatProb(energyInKeV, reflectAngleInRadians);
	ran = xrrtrandom();
	if ( prob < ran ) {
		return 0.0;		// no scatter
	}

	const double MIN2RAD = M_PI/180.0/60.0;
	const int ndiv = 200;
	const double xmin = -50.0;		// [arcmin]
	const double xmax = +50.0;		// [arcmin]
	const double pitch = (xmax - xmin) / ndiv;	// 0.5 arcmin

	double diffractAngleInRadians;
	double ri, rs, xi, ss;

	ri = table->getReflectivity(energyInKeV, reflectAngleInRadians);
	
	ran = prob * xrrtrandom() / pitch;
	for (int i = 0; i < ndiv/2; i++) {
		xi = pitch * (i + 0.5) * MIN2RAD;
		rs = table->getReflectivity(energyInKeV, reflectAngleInRadians + xi);
		ss = astroeFdiffract(rs/ri, reflectAngleInRadians, energyInKeV, xi);
		if ( ran < ss ) {
			diffractAngleInRadians = pitch * (i + ran/ss) * MIN2RAD;
			return diffractAngleInRadians;
		}
		ran -= ss;

		xi *= -1;
		rs = table->getReflectivity(energyInKeV, reflectAngleInRadians + xi);
		ss = astroeFdiffract(rs/ri, reflectAngleInRadians, energyInKeV, xi);
		if ( ran < ss ) {
			diffractAngleInRadians = - pitch * (i + ran/ss) * MIN2RAD;
			return diffractAngleInRadians;
		}
		ran -= ss;
	}

//	printf("ran=%f\n", ran);

	return 0.0;
}

double
XrrtScatter::astroeFdiffract(double rsi, double reflectAngleInRadians, double energyInKeV, double diffractAngleInRadians)
{
	double probability;
	double outAngleInRadians = reflectAngleInRadians + diffractAngleInRadians;

	if ( diffractAngleInRadians == 0.0 || outAngleInRadians <= 0.0 ) {
//		if ( 0.0 == diffractAngleInRadians ) {
//			printf("diffractAngleInRadians=%f\n", diffractAngleInRadians);
//		}
		probability = 0.0;
		return probability;
	}

	// const double DEG2RAD = 0.017453292519943295769;
	// const double MIN2RAD = DEG2RAD/60.0;
	const double xrrtPI = M_PI;		// 3.14159265358979323846;
	const double KEV2MICRON = 1.0e-4 * 12.3985;
	const double f_probability_norm = 16.0*xrrtPI*xrrtPI*xrrtPI/180.0/60.0;

// front side scatter parameter was updated. 20080124 by Someya, K

//	const double w_norm = 5.2e-9;// 5.2e-9;//4.65e-8;  //   5.2e-9;
//	const double w_power = -1.25;   //  -1.25;
//	const double g_norm = 2.14e5;   //   2.14e5;
//	double reflectivity_i;
//	double reflectivity_s;

	double wavelength = KEV2MICRON / energyInKeV;
	double frequency;

//	reflectivity_i = table->getReflectivity(energyInKeV,reflectAngleInRadians);
//	reflectivity_s = table->getReflectivity(energyInKeV,outAngleInRadians);
	frequency = fabs(cos(reflectAngleInRadians) - cos(outAngleInRadians));
	frequency = frequency / wavelength;
	
	// modified by Iizuka Ryo.
	if ( frequency == 0.0 ) {
		probability = 0.0;
		return probability;
	}

	double sinOutAngleInRadians = sin(outAngleInRadians);
	probability = f_probability_norm / (wavelength * wavelength * wavelength) *
		sin(reflectAngleInRadians) * 
		sinOutAngleInRadians * sinOutAngleInRadians *
		sqrt(rsi) * astroeWnorm * pow(frequency, astroeWpower) *
		exp(-1.0/frequency/astroeGnorm/wavelength);

	// printf("%lf %lf %lf %lf %lf %lf %lf %lf %lf\n",probability, f_probability_norm, pow(wavelength,3.0), sin(reflectAngleInRadians), sin(outAngleInRadians), sqrt(reflectivity_s/reflectivity_i), w_norm, pow(frequency,w_power),  exp(-1.0/frequency/g_norm/wavelength));
	
	return probability;
}

void
XrrtScatter::setDefaultScatterMode(const SurfaceScatterMode& mode)
{
	if ( -1 == mode ) {		// use scatter mode in the mirror file
		defaultScatterModeSet = false;
		defaultScatterMode = 0;
	} else {
		defaultScatterModeSet = true;
		defaultScatterMode = mode;
	}
}

bool
XrrtScatter::getAscaParamStatus()
{
	return ascaParamsSet;
}

double
XrrtScatter::ascaFfluct(double rx)
{
	return (ascaCrd*(ascaArd*fabs(rx)+1.0)/(rx*rx+ascaBrd*ascaBrd));
}

void
XrrtScatter::ascaStprnd(double& rx, double& y)
{
	double rs;
	double absRs;
	rs = xrrtrandom() - 5.0e-1;
	absRs = fabs(rs);

	double s0PlusS1 = ascaStps0+ascaStps1;
	if ( absRs <= ascaStps0 ) {
		rx = absRs/ascaStpy0;
		y  = ascaStpy0*ascaStpc;
	} else if ((absRs > ascaStps0) && (absRs <= s0PlusS1)) {
		rx=(absRs-ascaStps0)/ascaStpy1+ascaStpx1;
		y=ascaStpy1*ascaStpc;
	} else if ((absRs > s0PlusS1) && (absRs <= 5.0e-1)) {
		rx=(absRs - s0PlusS1)/ascaStpy2+ascaStpx2;
		y=ascaStpy2*ascaStpc;
	} else {
		throw rxOutOfRegion;
	}
	if ( rs <= 0.0 ) {
		rx = -1.0 * rx;
	}
	return;
}

double
XrrtScatter::ascaNvfluct()
{
	double rx;
	double y;
	double ry;
	double fx;
	do {
		ascaStprnd(rx,y);
		ry = xrrtrandom()*y;
		fx = ascaFfluct(rx);
	} while (ry > fx);
	return rx;
}

double
XrrtScatter::ascaFlor(double gamma)
{
	const double xrrtPI = M_PI;		// 3.14159265358979323846;

	double a	  = -2.0/gamma*60.0;
	double b	  =  2.0/gamma*60.0;

	double flor  = tan((atan(b) - atan(a))*xrrtrandom() + atan(a));
	double newFlor = flor * gamma / 2.0 * xrrtPI / 180.0;

	return newFlor;
}

void
XrrtScatter::setAstroeParams(const int& nvfsw,
							 const double& gausigma,
							 const double& ngau,
							 const double& expsigma,
							 const double& nexp,
							 const double& lorgamma,
							 const double& nlor,
							 const int& scatsw,
							 const double& w_norm,
							 const double& w_power,
							 const double& g_norm,
							 const double& gc_a,
							 const double& gc_b,
							 const double& gw_a,
							 const double& gw_b)
{
	//
	// This code was test version for ASTROE scatter tuning added by Furuzawa.
	// 
	//
	const double xrrtPI = M_PI;		// 3.1415926535897932385e0;
	const double x1 = 1.0;
	const double x2 = 5.0;
	const double xmax = 1.2e2;

	astroeNvfsw = nvfsw;
	astroeGaussSigma = gausigma;
	astroeNgau = ngau;
	astroeExpSigma = expsigma;
	astroeNexp = nexp;
	astroeLorentzGamma = lorgamma;
	astroeNlor = nlor;
	astroeScatsw = scatsw;
	astroeWnorm	 = w_norm;
	astroeWpower = w_power;
	astroeGnorm  = g_norm;
	astroePcolGcA = gc_a;
	astroePcolGcB = gc_b;
	astroePcolGwA = gw_a;
	astroePcolGwB = gw_b;
	astroeParamsSet = true;

	// Compute additional ASTROE params

	astroeGaussSigma2InvHalf = 0.5/(gausigma*gausigma);
	astroeExpSigmaInv = 1.0/expsigma;
	astroeLorentzGammaInv = 1.0/lorgamma;

	double bgau = sqrt(2*xrrtPI) * gausigma * ngau;
	double bexp = 2 * expsigma * nexp;
	double blor = 2 * xrrtPI * lorgamma * nlor;
	double ball = bgau + bexp + blor;
	if ( 0.0 == ball ) {
		astroeNvfsw = 0;
	} else {
		astroeBRgau = bgau / ball;
		astroeBRgauPlusBRexp = (bgau + bexp) / ball;
	}

	astroeXmaxrd = xmax/60.0/180.0*xrrtPI;
	astroeStpx1  = x1/60.0/180.0*xrrtPI;
	astroeStpx2  = x2/60.0/180.0*xrrtPI;
	double y0 = 1.1*astroeFfluct(0.0);
	double s0 = y0*astroeStpx1;
	double y1 = 1.1*astroeFfluct(astroeStpx1);
	double s1 = y1*(astroeStpx2-astroeStpx1);
	double y2 = 1.1*astroeFfluct(astroeStpx2);
	double s2 = y2*(astroeXmaxrd-astroeStpx2);
	astroeStpc= 2.0*(s0+s1+s2);

	astroeStpy0 = y0/astroeStpc;
	astroeStpy1 = y1/astroeStpc;
	astroeStpy2 = y2/astroeStpc;
	astroeStps0 = s0/astroeStpc;
	astroeStps1 = s1/astroeStpc;
	astroeStps2 = s2/astroeStpc;
}

int
XrrtScatter::readBackProfVer1(fitsfile *fp)
{
	const char fname[] = "XrrtScatter::readBackProfVer1():";
	const double xrrtPI = M_PI;		// 3.1415926535897932385;
	const double arcmin2radian = xrrtPI / (180.0 * 60.0);

	int i, j, ienergy, iangle, anul, hdunum, hdutype, istat;
	long naxis2;
	char extnum[256+1];
	char* extnumPtr[1] = { extnum };

	istat = 0;
	if ( fits_read_key_lng(fp, "NAXIS2", &naxis2, NULL, &istat) ) {
		fprintf(stderr, "\
%s: fits_read_key_lng('NAXIS2') failed\n", fname);
		fits_report_error(stderr, istat);
		return istat;
	}

	int numBackIndex = naxis2;
	vector<double> bIndexE(numBackIndex);
	vector<double> bIndexA(numBackIndex);
	vector<int>	bIndexN(numBackIndex);

	numBackE = 0;
	for (i = 0; i < numBackIndex; i++) {
		if (
fits_read_col_dbl(fp, 1, i+1, 1, 1, 0.0, &bIndexE[i], &anul, &istat) ||
fits_read_col_dbl(fp, 2, i+1, 1, 1, 0.0, &bIndexA[i], &anul, &istat) ||
fits_read_col_str(fp, 3, i+1, 1, 1, "", extnumPtr, &anul, &istat) ||
			0 ) {
			fprintf(stderr, "\
%s: fits_read_col(colnum=%d) failed\n", fname, i+1);
			fits_report_error(stderr, istat);
			return istat;
		}
		bIndexN[i] = atoi(extnum);
		if ( 0 == i || bIndexE[i] != bIndexE[i-1] ) {
			numBackE++;
		}
	}

	struct AstroeBackE *bE;
	struct AstroeBackA *bA;

	backE = new struct AstroeBackE[numBackE];
	ienergy = iangle = 0;
	for (i = 0; i < numBackIndex; i++) {
		iangle++;
		if ( i == numBackIndex - 1 || bIndexE[i] != bIndexE[i+1] ) {
			bE = &backE[ienergy];
			bE->energy = bIndexE[i];
			bE->numBackA = iangle;
			bE->backA = bA = new struct AstroeBackA[iangle];

			while ( iangle ) {
				iangle--;
				hdunum = bIndexN[i-iangle];
				fits_movabs_hdu(fp, hdunum, &hdutype, &istat);
				if ( istat ) {
					fprintf(stderr, "\
%s: fits_movabs_hdu(hdunum=%d) failed\n", fname, hdunum);
					fits_report_error(stderr, istat);
					return istat;
				}
				fits_read_key_lng(fp, "NAXIS2", &naxis2, NULL, &istat);
				if ( istat ) {
					fprintf(stderr, "\
%s: fits_read_key_lng('NAXIS2') failed\n", fname);
					fits_report_error(stderr, istat);
					return istat;
				}

				int numBackP = naxis2;
				double *backStpx = new double[numBackP];
				double *backStpy = new double[numBackP];

				if (
fits_read_col_dbl(fp, 1, 1, 1, numBackP, 0.0, backStpx, &anul, &istat) ||
fits_read_col_dbl(fp, 2, 1, 1, numBackP, 0.0, backStpy, &anul, &istat) ||
					0 ) {
					fprintf(stderr, "\
%s: fits_read_col_dbl() failed\n", fname);
					fits_report_error(stderr, istat);
					return istat;
				}

				double backNorm = 0.0;
				double pitch = (backStpx[1] - backStpx[0]) * arcmin2radian;
				for (j = 0; j < numBackP; j++) {
					backStpx[j] *= arcmin2radian;
					backNorm += backStpy[j] * pitch;
				}

				if ( backNorm <= 0.0 ) {
					fprintf(stderr, "\
%s: Backside reflection profile is zero.\n", fname);
					return -1;
				}

				for (j = 0; j < numBackP; j++) {
					backStpy[j] /= backNorm;
				}

				bA->bangle   = bIndexA[i-iangle];
				bA->numBackP = numBackP;
				bA->norm     = backNorm;
				bA->pitch    = pitch;
				bA->stpx     = backStpx;
				bA->stpy     = backStpy;

				bA++;
			}
			ienergy++;
		}
	}

	return 0;
}

int
XrrtScatter::readBackProfVer2(fitsfile *fp)
{
	const char fname[] = "XrrtScatter::readBackProfVer2():";
	const double xrrtPI = M_PI;		// 3.1415926535897932385;
	const double arcmin2radian = xrrtPI / (180.0 * 60.0);

	int naxis2, tfields, numE, numA, typecode, col_energy;
	long repeat, width;
	char key[16], val[80];
	int i, j, ienergy, iangle, anul, istat;

	istat = 0;
	if ( fits_read_key(fp, TINT, "NAXIS2", &naxis2, NULL, &istat) ) {
		fprintf(stderr, "\
%s: fits_read_key('NAXIS2') failed\n", fname);
		fits_report_error(stderr, istat);
		return istat;
	}
	numE = naxis2;

	if ( fits_get_colnum(fp, CASEINSEN, "ENERGY", &col_energy, &istat) ) {
		fprintf(stderr, "\
%s: fits_get_colnum('ENERGY') failed\n", fname);
		fits_report_error(stderr, istat);
		return istat;
	}

	if ( fits_read_key(fp, TINT, "TFIELDS", &tfields, NULL, &istat) ) {
		fprintf(stderr, "\
%s: fits_read_key('TFIELDS') failed\n", fname);
		fits_report_error(stderr, istat);
		return istat;
	}
	numA = tfields - 1;
	vector<int> colnum(numA);
	vector<int> colrep(numA);
	vector<double> bangle(numA);
	int crpx;
	double crvl;
	double cdlt;
	vector<double> pitch(numA);
	vector<double*> stpx(numA);

	numA = 0;
	for (i = 1; i <= tfields; i++) {
		if ( col_energy == i ) {
			continue;
		}
		colnum[numA] = i;
		if ( fits_get_coltype(fp, i, &typecode, &repeat, &width, &istat) ) {
			fprintf(stderr, "\
%s: fits_get_coltype(colnum=%d) failed\n", fname, i);
			fits_report_error(stderr, istat);
			return istat;
		}
		colrep[numA] = repeat;

		sprintf(key, "TTYPE%d", i);
		if ( fits_read_key_str(fp, key, val, NULL, &istat) ) {
			fprintf(stderr, "\
%s: fits_read_key('%s') failed\n", fname, key);
			fits_report_error(stderr, istat);
			return istat;
		}
		if ( 0 != strncmp("Intensity", val, 9) ) {
			fprintf(stderr, "\
%s: invalid column name '%s' ignored\n", fname, val);
			continue;
		}
		if ( '_' == val[10] ) {
			val[10] = '.';		// replace '0_8deg' -> '0.8deg'
		}
		bangle[numA] = atof(val + 9);

		if (
0 == sprintf(key, "1CRPX%d", i) ||
fits_read_key(fp, TINT, key, &crpx, NULL, &istat) ||
0 == sprintf(key, "1CRVL%d", i) ||
fits_read_key_dbl(fp, key, &crvl, NULL, &istat) ||
0 == sprintf(key, "1CDLT%d", i) ||
fits_read_key_dbl(fp, key, &cdlt, NULL, &istat) ||
			 0 ) {
			fprintf(stderr, "\
%s: fits_read_key('%s') failed\n", fname, key);
			fits_report_error(stderr, istat);
			return istat;
		}

		pitch[numA] = cdlt * arcmin2radian;
		stpx[numA] = new double[repeat];
		for (j = 0; j < repeat; j++) {
			stpx[numA][j] = (crvl + (j + 1 - crpx) * cdlt) * arcmin2radian;
		}

		numA++;
	}

/*	printf("numE=%d, numA=%d\n", numE, numA);
	for (iangle = 0; iangle < numA; iangle++) {
		printf("\
%3d: bangle=%.1f (deg) pitch=%.1f (arcmin) numP=%d stpx[0]=%.1f (arcmin)\n",
			iangle, bangle[iangle], pitch[iangle]/arcmin2radian,
			colrep[iangle], stpx[iangle][0]/arcmin2radian);
	}*/

	struct AstroeBackE *bE;
	struct AstroeBackA *bA;

	numBackE = numE;
	backE = new struct AstroeBackE[numE];
	for (ienergy = 0; ienergy < numE; ienergy++) {
		bE = &backE[ienergy];
		bE->numBackA = numA;
		bE->backA = new struct AstroeBackA[numA];
		fits_read_col_dbl(fp, col_energy, ienergy+1, 1, 1,
			0.0, &bE->energy, &anul, &istat);
		for (iangle = 0; iangle < numA; iangle++) {
			bA = &bE->backA[iangle];
			bA->bangle   = bangle[iangle];
			bA->pitch    = pitch[iangle];
			bA->numBackP = colrep[iangle];
			bA->stpx     = stpx[iangle];
			bA->stpy     = new double[bA->numBackP];
			fits_read_col_dbl(fp, colnum[iangle], ienergy+1, 1, bA->numBackP,
				0.0, bA->stpy, &anul, &istat);
			bA->norm = 0.0;
			for (j = 0; j < bA->numBackP; j++) {
				bA->norm += bA->stpy[j] * bA->pitch;
			}
			for (j = 0; j < bA->numBackP; j++) {
				bA->stpy[j] /= bA->norm;
			}
		}
	}

	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_col() failed\n", fname);
		fits_report_error(stderr, istat);
		return istat;
	}

	return 0;
}

int
XrrtScatter::setAstroeBackParams(string backprofFileName)
{
	const char fname[] = "XrrtScatter::setAstroeBackParams():";

	//
	// This code was test version for ASTROE scatter tuning added by Mori.
	// 
	//
	//astroeBackNlor = 0.5e0;
	//astroeBackNgau = 0.5e0;
	//astroeBackSigma = 20.0;	 		// arcmin (defalut 40.0)
	//astroeBacklorSigma = 25.0e0;	// arcmin (defalut 50.0)
	// astroeBackSigma = 40.0 * xrrtPI / (180.0 * 60.0);
	// astroeBacklorSigma = 50.0 * xrrtPI / (180.0 * 60.0);

	fitsfile *fp;
	int hdutype;
    char formatVersionString[256];
    int formatVersion;

	int istat = 0;
	const char *charBackprofFileName = backprofFileName.c_str();

	// 2008-03-02 Y.ISHISAKI	version 6.5.0
	if ( 0 == strcasecmp("NONE", charBackprofFileName) ) {
		return 0;
	}

	fflush(NULL);
	printf("Reading '%s' ...\n", charBackprofFileName);
	fflush(NULL);

	if ( fits_open_file(&fp, charBackprofFileName, READONLY, &istat) ) {
		fprintf(stderr, "\
%s: fits_open_file('%s') failed\n", fname, charBackprofFileName);
		fits_report_error(stderr, istat);
		return istat;
	}

	if ( fits_movabs_hdu(fp, 2, &hdutype, &istat) ) {
		fits_report_error(stderr, istat);
		return istat;
	} 

    // Find
    // CBD10001= 'FORMAT_VERSION(2)'  / Format version of XRT reflectivity file
	fits_read_key_str(fp, "CBD10001", formatVersionString, NULL, &istat);
    if ( istat ) {
		istat = 0;
		formatVersion = 1;
    } else if ( 0 != strncmp("FORMAT_VERSION(", formatVersionString, 15) ) {
		throw invalidFormatVersionString;
    } else {
		formatVersion = atoi(formatVersionString + 15);
    }

    if ( 1 == formatVersion ) {
		istat = readBackProfVer1(fp);
	} else if ( 2 == formatVersion || 3 == formatVersion ) {
		istat = readBackProfVer2(fp);
	} else {
		throw unknownFormatVersion;
	}
	if ( istat ) {
		return istat;
	}

	fits_close_file(fp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_close_file('%s') failed\n", fname, charBackprofFileName);	  
		fits_report_error(stderr, istat);
		return istat;
	} 

	return istat;
}

///////////////////////////////////////////////////////

void
XrrtScatter::setAstroeBxrrtback(double IncidentAngleInRadians, double energyInKeV)
{
	if ( lastIncidentAngle == IncidentAngleInRadians &&
		 lastEnergy == energyInKeV ) {
		return;
	}

	lastIncidentAngle = IncidentAngleInRadians;

	//
	// Set Xrrtback profile
	// fits reading added (modified by M. Ebara : date 2005/05/31)
	//

	int i;
	double delta_ene, sa_ene;
	struct AstroeBackE *bE, *bEi;

	if ( lastEnergy == energyInKeV ) {
		bE = lastBackE;
	} else {
		/*一番近いenergyをとってくるようにする */	
		bE = bEi = backE;
		sa_ene = fabs(bEi->energy - energyInKeV);
		for (i = 1, bEi += 1; i < numBackE; i++, bEi++){
			delta_ene = fabs(bEi->energy - energyInKeV);
			if ( delta_ene < sa_ene ) {
				bE = bEi;
				sa_ene = delta_ene;
			}
		}
		lastEnergy = energyInKeV;
		lastBackE = bE;
	}

	const double xrrtPI = M_PI;		// 3.1415926535897932385;
	const double radian2deg = 180.0 / xrrtPI;
	double incidentAngleInDeg = IncidentAngleInRadians * radian2deg;
	double delta_inc, sa_inc;
	int numBackA = bE->numBackA;
	struct AstroeBackA *bA, *bAi;

	/*一番近いincident_angleをとってくるようにする */
	bA = bAi = bE->backA;
	sa_inc = fabs(bAi->bangle - incidentAngleInDeg);
	for (i = 1, bAi += 1; i < numBackA; i++, bAi++){
		delta_inc = fabs(bAi->bangle - incidentAngleInDeg);
		if ( delta_inc < sa_inc ) {
			bA = bAi;
			sa_inc = delta_inc;
		}
	}

	//  printf("energyInKeV=%f, IncidentAngleInRadians=%f\n", energyInKeV, IncidentAngleInRadians);
	//  printf("match_ene=%f, match_inc=%f, match_ext=%d\n", match_ene, match_inc, match_ext);

	/* profile セット */
	numBackProf    = bA->numBackP;
	backNorm       = bA->norm;
	backPitch      = bA->pitch;
	astroeBackStpx = bA->stpx;
	astroeBackStpy = bA->stpy;
}

/////////////////////////////////////////////////////////////////////

double
XrrtScatter::astroeBxrrtback(double rx)
{
	double y, dy;
	int i;

	i = (int)( (rx - astroeBackStpx[0]) / backPitch );

	if ( i < 0 ) {
		i = 0;
	} else if ( numBackProf <= i ) {
		i = numBackProf - 1;
	}

	if ( rx == astroeBackStpx[i] ){
		y = astroeBackStpy[i];
	} else if ( numBackProf - 1 == i ) {
		dy = astroeBackStpy[i] - astroeBackStpy[i-1];
		y = astroeBackStpy[i] + (dy/backPitch) * (rx - astroeBackStpx[i-1]);
	} else {
		dy = astroeBackStpy[i+1] - astroeBackStpy[i];
		y = astroeBackStpy[i] + (dy/backPitch) * (rx - astroeBackStpx[i]);
	}
	
	return y;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C++ ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
