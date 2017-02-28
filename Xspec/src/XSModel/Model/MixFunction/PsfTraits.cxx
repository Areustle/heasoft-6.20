//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Error/Error.h>
#include <CCfits/CCfits>

// Psf
#include <Psf.h>
// PsfRegion
#include <PsfRegion.h>
// PsfTraits
#include <PsfTraits.h>

const size_t XMM::NE = 10;
const Real XMM::psfEnergy[] = {.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0, 6.0};

const size_t Suzaku::NE = 2;
const Real Suzaku::psfEnergy[] = {0.1, 15.0};


// Class XMM 
const string XMM::keywordRoot = string("XMMPSF");

void XMM::pointSpreadFunction (Psf<XMM>& psf, size_t iObs, size_t iReg, int ixb, int iyb, size_t iEng)
{
  // Routine to calc the PSF (in m_psfArray) for a photon at a particular
  // pixel position and energy.

  // PSF data is from EPIC-MCT-TN-011 by Simona Ghizzardi (Oct 8, 2001)
  // The PSF is assumed to be a King profile
  //
  //     PSF = A / (1 + (r/r_c)^2)^(-alpha)
  //
  // where A = (alpha-1)/r_c^2/PI to ensure the PSF integrates to 1 and
  //
  // r_c   = r1(I) + r2(I)*E + r3(I)*Theta + r4(I)*E*Theta
  // alpha = a1(I) + a2(I)*E + a3(I)*Theta + a4(I)*E*Theta
  //
  // E is the energy (keV), Theta the off-axis angle (arcmin) and I is the
  // instrument.
  const Real PI = 4.*std::atan(1.);
  const int NINST = 3;
  const Real r1[NINST] = {5.074, 4.759, 6.636};
  const Real r2[NINST] = {-0.236, -0.203, -0.305};
  const Real r3[NINST] = {0.002, 0.014, -0.175};
  const Real r4[NINST] = {-0.0180, -0.0229, -0.0067};
  const Real a1[NINST] = {1.472, 1.411, 1.525};
  const Real a2[NINST] = {-0.010, -0.005, -0.015};
  const Real a3[NINST] = {-0.001, -0.001, -0.012};
  const Real a4[NINST] = {-0.0016, -0.0002, -0.001};

  const PsfRegion<XMM>* region = psf.regions()[iObs][iReg];
  const Real wmrbn = region->wmrbn();
  const Real xrmin = region->lowerLeft().first;
  const Real yrmin = region->lowerLeft().second;
  const int inst = region->instrument();
  const Real energy = psf.psfEnergy()[iEng];
  if (inst >= NINST || inst < 0)
  {
     throw RedAlert("Instrument unrecongnized in XMM point spread function");
  }

  // Calculate the off-axis angle of the source. Currently assume the optical
  // axis is at detector coordinates (0,0)
  Real xdet = ixb*wmrbn + xrmin;
  Real ydet = iyb*wmrbn + yrmin;
  Real binSz = psf.binSizes()[iObs];
  Real theta = sqrt((xdet*xdet + ydet*ydet))*(binSz/wmrbn);

  // Calculate core radius^2 and alpha for this energy and theta
  Real rcore2 = r1[inst] + r2[inst]*energy + r3[inst]*theta + 
                r4[inst]*energy*theta;
  rcore2 *= rcore2;
  Real alpha = a1[inst] + a2[inst]*energy + a3[inst]*theta +
                a4[inst]*energy*theta;

  // Convert the core radius from arcsec to wmap bins
  rcore2 /= 60.0*binSz*60.0*binSz;

  // Calculate the norm. Include the area of the wmap bin (which = 1).
  Real psfNorm = (alpha-1.0)/(rcore2*PI);

  // Loop round the output array
  std::vector<PsfRegion<XMM>*>& regions = psf.regions()[iObs];
  size_t nReg = regions.size();
  for (size_t jReg=0; jReg<nReg; ++jReg)
  {
     PsfRegion<XMM>* region_j = regions[jReg];

     // For each bin in region_j, calculate its distance from bin
     // (ixb,iyb) in the current region (region given by iReg).
     // Then calculate the PSF contribution.
     const int nxj = region_j->nx();
     const int nyj = region_j->ny();
     RealArray& psfArray = region_j->psfArray();
     psfArray.resize(static_cast<size_t>(nxj*nyj), .0);

     const Real xOffset = (xrmin - region_j->lowerLeft().first)/wmrbn;
     const Real yOffset = (yrmin - region_j->lowerLeft().second)/wmrbn;
     // wmapPos contains the positions of all non-neg pixels.
     const IntegerArray& pixPos = region_j->wmapPos();
     int nPix = pixPos.size();
     for (int indjPix=0; indjPix<nPix; ++indjPix)
     {
        int jPixIdx = pixPos[indjPix];
        int ixbj = jPixIdx % nxj;
        int iybj = jPixIdx / nxj;
        Real dist2 = (iyb - iybj + yOffset)*(iyb - iybj + yOffset) +
                     (ixb - ixbj + xOffset)*(ixb - ixbj + xOffset);
        psfArray[jPixIdx] = psfNorm*pow(1.0+dist2/rcore2,-alpha);
     }
  }
}

void XMM::readSpecificMapKeys (PsfRegion<XMM>& region)
{
  size_t flag = 0;
  const int NINST = 3;
  const CCfits::PHDU& hdu = region.file()->pHDU(); 
  static const Real det2sky[NINST][6] = 
        {{2.416711943880075e4, 2.529931630805351e4, 7.15e1, -2.425e2, -1.0, 1.0},
         {2.414449223345453e4, 2.527086327870846e4, 6.95e1, -1.26e2, -1.0, 1.0},
         {2.547981387888674e4, 2.3138433606285634, -2.20153, -1.1013, -1.0, 1.0}};
  bool isExt = false;
  try
  {
     float tmpFloat = .0;
     hdu.keyWord("PIXSIZE").value(tmpFloat);
     region.pixSize(static_cast<Real>(tmpFloat));
     ++flag;
     int intVal = 0;
     hdu.keyWord("SKYBIN").value(intVal);
     region.skybin(intVal);
     ++flag;
     string instrument;
     hdu.keyWord("INSTRUME").value(instrument);
     if (!instrument.compare(0,5,"EMOS1"))
     {
        region.instrument(0);
     }
     else if (!instrument.compare(0,5,"EMOS2"))
     {
        region.instrument(1);
     }
     else if (!instrument.compare(0,3,"EPN"))
     {
        region.instrument(2);
     }
     else
     {
        string msg = "Unknown instrument in ";
        msg += hdu.parent()->name();
        throw PsfRegion<XMM>::PsfRegionError(msg);
     }
     ++flag; 

     // Get the reference WCS keywords which describe the sky coordinates
     Real realVal = 0.0;
     hdu.keyWord("REFXCRVL").value(realVal);
     region.refxcrval(realVal);
     hdu.keyWord("REFYCRVL").value(realVal);
     region.refycrval(realVal);
     hdu.keyWord("REFXCRPX").value(realVal);
     region.refxcrpix(realVal);
     hdu.keyWord("REFYCRPX").value(realVal);
     region.refycrpix(realVal);
     hdu.keyWord("REFXCDLT").value(realVal);
     region.refxcdlt(realVal);
     hdu.keyWord("REFYCDLT").value(realVal);
     region.refycdlt(realVal);
     ++flag;

     // Try to read the EXT* keywords. If not load the values from 
     // the data statement and read the PA_PNT to get the roll angle
     try 
     {
        hdu.keyWord("EXTXF0").value(realVal);
        region.wfxf0(realVal);
        isExt = true;
     }
     catch (...)
     {
     }
     if (isExt)
     {
        hdu.keyWord("EXTYF0").value(realVal);
        region.wfyf0(realVal);
        hdu.keyWord("EXTXH0").value(realVal);
        region.wfxh0(realVal);
        hdu.keyWord("EXTYH0").value(realVal);
        region.wfyh0(realVal);
        hdu.keyWord("EXTXSIGN").value(realVal);
        region.xsign(realVal);
        hdu.keyWord("EXTYSIGN").value(realVal);
        region.ysign(realVal);
        hdu.keyWord("EXTTHETA").value(realVal);
        region.wftheta(realVal);
     }
     else
     {
        hdu.keyWord("PA_PNT").value(realVal);
        region.wftheta(realVal);
        const Real PI = 4.*std::atan(1.0);
        const int instrument = region.instrument();
        if (instrument == 1)
        {
           region.wftheta(region.wftheta() + 90.0);
        }
        else if (instrument == 2)
        {
           region.wftheta(region.wftheta() - 90.0);
        }
        region.wftheta(region.wftheta() * (-1.0*PI/180.0));
        region.wfxf0(det2sky[instrument][0]);
        region.wfyf0(det2sky[instrument][1]);
        region.wfxh0(det2sky[instrument][2]);
        region.wfyh0(det2sky[instrument][3]);
        region.xsign(det2sky[instrument][4]);
        region.ysign(det2sky[instrument][5]);
     }

  }
  catch  (CCfits::FitsException&)
  {
     string specMsg;
     switch (flag)
     {
        case 0:
           specMsg = "PIXSIZE";
           break;
        case 1:
           specMsg = "SKYBIN";
           break;
        case 2:
           specMsg = "INSTRUME";
           break;
        case 3:
           specMsg = "reference pointing";
           break;
        case 4:
           specMsg = isExt ? string("EXT*") : string("PA_PNT");
           break;
        default:
           break;
     }
     string msg = "Error reading ";
     msg += specMsg;
     msg += " keyword(s) in ";
     msg += hdu.parent()->name();
     throw PsfRegion<XMM>::PsfRegionError(msg);
  }
}

// Additional Declarations

// Class Suzaku 
const string Suzaku::keywordRoot = string("SUZPSF");

void Suzaku::pointSpreadFunction (Psf<Suzaku>& psf, size_t iObs, size_t iReg, int ixb, int iyb, size_t iEng)
{
  // Routine to calc the PSF (in m_psfArray) for a photon at a particular
  // pixel position and energy.

  // Empirical analytic model for PS from MCG-6-30-15 data taken in very 
  // early phase of Suzaku mission.
  // The PSF is assumed to be the sum of two exponentials and a gaussian
  //
  //     PSF = EN1*EXP(-r/EW1) + GN1*EXP(-(r/GW1)^2/2.) + EN2*EXP(-r/EW2)
  //
  // The PSF is currently assumed to be position and energy independent

  const Real PI = 4.*std::atan(1.);
  const int NINST = 4;

  const Real EW1[NINST] = {0.221, 0.176, 0.172, 0.218};
  const Real EN1[NINST] = {1.101, 1.006, 1.082, 0.957};
  const Real GW1[NINST] = {0.970, 1.047, 0.927, 1.014};
  const Real GN1[NINST] = {0.0919, 0.0929, 0.1175, 0.0891};
  const Real EW2[NINST] = {1.937, 1.937, 1.937, 1.937};
  const Real EN2[NINST] = {0.00539, 0.00539, 0.00539, 0.00539};

  const PsfRegion<Suzaku>* region = psf.regions()[iObs][iReg];
  const Real wmrbn = region->wmrbn();
  const Real xrmin = region->lowerLeft().first;
  const Real yrmin = region->lowerLeft().second;
  const int inst = region->instrument();
  //  const Real energy = psf.psfEnergy()[iEng];
  if (inst >= NINST || inst < 0)
  {
     throw RedAlert("Instrument unrecognized in Suzaku point spread function");
  }

  // Calculate the off-axis angle of the source. Currently assume the optical
  // axis is at detector coordinates (0,0)
  // Real xdet = ixb*wmrbn + xrmin;
  // Real ydet = iyb*wmrbn + yrmin;
  Real binSz = psf.binSizes()[iObs];
  // Real theta = sqrt((xdet*xdet + ydet*ydet))*(binSz/wmrbn);

  // Calculate the exponential and gaussian widths in wmap bin units
  Real expwidth1 = EW1[inst] / binSz;
  Real expwidth2 = EW2[inst] / binSz;
  Real gauwidth  = GW1[inst] / binSz;

  // Calculate the psf norm. Include the area of the wmap bin (which = 1).
  // = 2pi(EN1*EW^2 + GN1*GW1^2 + EN2*EW2^2)
  Real psfNorm = 1.0/(2*PI*( EN1[inst]*expwidth1*expwidth1 
                      + GN1[inst]*gauwidth*gauwidth 
		      + EN2[inst]*expwidth2*expwidth2));

  // Loop round the output array
  std::vector<PsfRegion<Suzaku>*>& regions = psf.regions()[iObs];
  size_t nReg = regions.size();
  for (size_t jReg=0; jReg<nReg; ++jReg)
  {
     PsfRegion<Suzaku>* region_j = regions[jReg];

     // For each bin in region_j, calculate its distance from bin
     // (ixb,iyb) in the current region (region given by iReg).
     // Then calculate the PSF contribution.
     const int nxj = region_j->nx();
     const int nyj = region_j->ny();
     RealArray& psfArray = region_j->psfArray();
     psfArray.resize(static_cast<size_t>(nxj*nyj), .0);

     const Real xOffset = (xrmin - region_j->lowerLeft().first)/wmrbn;
     const Real yOffset = (yrmin - region_j->lowerLeft().second)/wmrbn;
     // wmapPos contains the positions of all non-neg pixels.
     const IntegerArray& pixPos = region_j->wmapPos();
     int nPix = pixPos.size();
     for (int indjPix=0; indjPix<nPix; ++indjPix)
     {
        int jPixIdx = pixPos[indjPix];
        int ixbj = jPixIdx % nxj;
        int iybj = jPixIdx / nxj;
        Real dist2 = (iyb - iybj + yOffset)*(iyb - iybj + yOffset) +
                     (ixb - ixbj + xOffset)*(ixb - ixbj + xOffset);
	Real dist  = sqrt(dist2);
        psfArray[jPixIdx] = psfNorm*( EN1[inst]*exp(-dist/expwidth1)
			            + GN1[inst]*exp(-dist2/gauwidth/gauwidth/2)
				    + EN2[inst]*exp(-dist/expwidth2) );
     }

  }
}

void Suzaku::readSpecificMapKeys (PsfRegion<Suzaku>& region)
{
  size_t flag = 0;
  const CCfits::PHDU& hdu = region.file()->pHDU(); 
  try
  {
     float tmpFloat = .0;
     hdu.keyWord("PIXSIZE").value(tmpFloat);
     region.pixSize(static_cast<Real>(tmpFloat));
     ++flag;
     int intVal = 0;
     hdu.keyWord("SKYBIN").value(intVal);
     region.skybin(intVal);
     ++flag;
     string instrument;
     hdu.keyWord("INSTRUME").value(instrument);
     if (!instrument.compare(0,4,"XIS0"))
     {
        region.instrument(0);
     }
     else if (!instrument.compare(0,4,"XIS1"))
     {
        region.instrument(1);
     }
     else if (!instrument.compare(0,4,"XIS2"))
     {
        region.instrument(2);
     }
     else if (!instrument.compare(0,4,"XIS3"))
     {
        region.instrument(3);
     }
     else
     {
        string msg = "Unknown instrument in ";
        msg += hdu.parent()->name();
        throw PsfRegion<Suzaku>::PsfRegionError(msg);
     }
     ++flag; 

     // Get the reference WCS keywords which describe the sky coordinates

     Real realVal = 0.0;
     hdu.keyWord("REFXCRVL").value(realVal);
     region.refxcrval(realVal);
     hdu.keyWord("REFYCRVL").value(realVal);
     region.refycrval(realVal);
     hdu.keyWord("REFXCRPX").value(realVal);
     region.refxcrpix(realVal);
     hdu.keyWord("REFYCRPX").value(realVal);
     region.refycrpix(realVal);
     hdu.keyWord("REFXCDLT").value(realVal);
     region.refxcdlt(realVal);
     hdu.keyWord("REFYCDLT").value(realVal);
     region.refycdlt(realVal);
     ++flag;

     // Read the wf* members to provide the sky to detector map

     hdu.keyWord("EXTXF0").value(realVal);
     region.wfxf0(realVal);
     hdu.keyWord("EXTYF0").value(realVal);
     region.wfyf0(realVal);
     hdu.keyWord("EXTXH0").value(realVal);
     region.wfxh0(realVal);
     hdu.keyWord("EXTYH0").value(realVal);
     region.wfyh0(realVal);
     hdu.keyWord("EXTXSIGN").value(realVal);
     region.xsign(realVal);
     hdu.keyWord("EXTYSIGN").value(realVal);
     region.ysign(realVal);
     hdu.keyWord("EXTTHETA").value(realVal);
     region.wftheta(realVal);

  }
  catch  (CCfits::FitsException&)
  {
     string specMsg;
     switch (flag)
     {
        case 0:
           specMsg = "PIXSIZE";
           break;
        case 1:
           specMsg = "SKYBIN";
           break;
        case 2:
           specMsg = "INSTRUME";
           break;
        case 3:
           specMsg = "reference pointing";
           break;
        case 4:
           specMsg = "EXT*";
           break;
        default:
           break;
     }
     string msg = "Error reading ";
     msg += specMsg;
     msg += " keyword(s) in ";
     msg += hdu.parent()->name();
     throw PsfRegion<Suzaku>::PsfRegionError(msg);
  }
}

// Additional Declarations
