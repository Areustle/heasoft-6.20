// xrrtoptical.cc
//
// Member definition for Optical constants
// Richard L Fink GSFC/631
// 1997/06/19
// 1997/09/24 More documentaion. R. Fink

//#include <iostream.h>
#include "xrrtoptical.hh"


XrrtOpticalConstants::XrrtOpticalConstants()
{
// A very simple constructor
}


void 
XrrtOpticalConstants::computeOpticalConstants(const double& energyInKeV,
                                     const XrrtMolecule& compound,
                                     const double& cgsDensity,
                                           double& opticalDelta,
                                           double& opticalBeta,
					      // Modified by H. Mori (2005/09/14)
					      // double_complex& refractiveIndexN)
					      complex<double>& refractiveIndexN)
{
//
// Compute the optical delta, beta, and refractive index for a
// surface
//
const double radiusOfElectron = 2.817939e-13; // in cm
const double avagadroNumber = 6.022169e23;
const double PI = 3.1415926535897932385e0;
const double eVtoCM = 12396.3e-8;

// Based on "Soft X-Ray Optics" by Eberhard Spiller
// SPIE Optical Engineering Press 1994
// ISBN 0-8194-1654-1 (soft cover)

// Delta based on page 13 eq 2.18a
// opticalDelta = radiusOfElectron * waveLength**2 * density * F1 /
//                2PI / atomicWeightOfOneAtom
// F1 is the real part of the atomic scattering factor (see page 11 cf 2.2)

// Beta based on page 13 eq 2.18b
// opticalBeta =  radiusOfElectron * waveLength**2 * density * F2 /
//                2PI / atomicWeightOfOneAtom
// F2 is the imaginary part of the atomic scattering factor (see page 11 cf 2.2)

// refractiveIndex based on page 13 eq 2.17
// refractiveIndex = (1 - opticalDelta) + (opticalBeta)i


// We add up the contributions of the elements that make up the compound
// to compute the Delta and Beta. This ignores likely fine structure
// at the absorption edges which can only be measured directly.

   int numberOfElements = compound.getNumberOfElements();
   double energyIneV = energyInKeV * 1000.0e0;

   double atomicWeightSum = 0;
   opticalDelta = 0;
   opticalBeta  = 0;
   for (int elementCount = 0;
        elementCount < numberOfElements;
        elementCount++)
       {
       int atomicNumber;
       int numberOfTimesOccurs;
       compound.getElement(elementCount, atomicNumber, numberOfTimesOccurs);
       atomicWeightSum = atomicWeightSum +
                         theAtomicDataTable().getAtomicWeight(atomicNumber) *
                         (double) numberOfTimesOccurs;
       opticalDelta = opticalDelta + 
            theAtomicScatterFactorTable().getRealF1(atomicNumber,energyIneV) *
            (double) numberOfTimesOccurs;
       opticalBeta  = opticalBeta +
            theAtomicScatterFactorTable().getImgF2(atomicNumber,energyIneV) *
            (double) numberOfTimesOccurs;
       }
   double wavelength = eVtoCM / energyIneV;
   double scaleFactor = (radiusOfElectron * wavelength * wavelength *
                        avagadroNumber * cgsDensity) / 
                        (2.0e0 * PI * atomicWeightSum);
   opticalDelta = scaleFactor * opticalDelta;
   opticalBeta  = scaleFactor * opticalBeta;
   // Modified by H. Mori (2005/09/14)
   // double_complex temp(1.0e0-opticalDelta, opticalBeta);
   complex<double> temp(1.0e0-opticalDelta, opticalBeta);
   refractiveIndexN = temp;
}

double
XrrtOpticalConstants::computeSingleLayerReflect(const XrrtMolecule& molecule,
                                       const double& cgsDensity,
                                       const double& energyInKeV,
                                       const double& roughness,
                                       const double& grazingAngleInRadians)
{
//
// Return the reflection probabilty for a single reflection between VACUMN
// and the given surface.
//
// ONLY INCLUDES S-POLARIZATION
//
const double PI = 3.1415926535897932385e0;

// Based on "Soft X-Ray Optics" by Eberhard Spiller
// SPIE Optical Engineering Press 1994
// ISBN 0-8194-1654-1 (soft cover)

// Assumes vacuum against a thick layer of the given molecule

    // derive needed quantities

    // See Spiller (above) page 24 THESE ARE ALL COMPLEX
    // r12 = (n1*cos(phi1) - n2*cos(phi2))/(n1*cos(phi1) + n2*cos(phi2))
    // n1 = 1 so ignore it
    // n2 can be calculated
    // phi1 is the incident angle to the normal or PI/2 - grazingAngleInRadians
    // phi2 is by Spiller 
    //      cos(phi2) = sqrt(1-(n1/n2)**2(sin(phi1))**2)
    // so we restate r12 above by substitution as
    // r12 = (sin(grazingAngleInRadians) - 
    //            sqrt(n2**2-cos(grazingAngleInRadians)**2)) /
    //       (sin(grazingAngleInRadians) + 
    //            sqrt(n2**2-cos(grazingAngleInRadians)**2))

    // Compute the complex refractive index of the layer
    double opticalDelta = 0;
    double opticalBeta = 0;
    // Modified by H. Mori (2005/09/14)
    // double_complex n2;
    complex<double> n2;

    computeOpticalConstants(energyInKeV, molecule, cgsDensity, 
                            opticalDelta, opticalBeta, n2);

    // Compile for GCC 3.2.2
    // Modified by H. Mori (2005/09/14)
    //
    // Now the complex incident angle
    // double_complex incidentAngle(grazingAngleInRadians,0.0);
    //
    // Help the compiler
    // double_complex sinIncidentAngle = sin(incidentAngle);
    // double_complex cosIncidentAngle = cos(incidentAngle);
    // double_complex temp = sqrt(n2*n2 - cosIncidentAngle*cosIncidentAngle); // n2*sin(theta_2) (snell's law)
    //
    // compute the reflected amplitude
    // double_complex reflectedAmplitude = 
    //     (sinIncidentAngle - temp)/(sinIncidentAngle + temp);
    
    // Now the complex incident angle
    complex<double> incidentAngle(grazingAngleInRadians,0.0);

    // Help the compiler
    complex<double> sinIncidentAngle = sin(incidentAngle);
    complex<double> cosIncidentAngle = cos(incidentAngle);
    complex<double> temp = sqrt(n2*n2 - cosIncidentAngle*cosIncidentAngle); // n2*sin(theta_2) (snell's law)
    
    // compute the reflected amplitude
    complex<double> reflectedAmplitude = 
        (sinIncidentAngle - temp)/(sinIncidentAngle + temp);

    // compute real reflectivity |r12|**2
    double reflectivity =  reflectedAmplitude.real()*reflectedAmplitude.real() +
                           reflectedAmplitude.imag()*reflectedAmplitude.imag();

    // apply a roughness correction
    // For example see Spiller page 132
    // This form taken unchecked from Keith Gendreau's code

    // DW -> NC model Spiller page 112
    // Modified by H. Mori (2005/09/14)
    // double_complex NC ; // q1*q2
    complex<double> NC ; // q1*q2
    NC = sin(incidentAngle)*temp ;
   
    // printf("%f %f %f\n", grazingAngleInRadians*180/PI, NC.real(), NC.imag());
    if (roughness != 0.0e0)
       {
	 reflectivity = reflectivity * 
	   exp(- NC.real()*
	       pow((4.0e0*PI* roughness*energyInKeV/12.4e0),2));
	 /*
	   exp(-pow((4.0e0*PI*sin(grazingAngleInRadians)*
	   roughness*energyInKeV/12.4e0),2));
	 */
       }
   
    // Return reflectivity
    return reflectivity;
}


