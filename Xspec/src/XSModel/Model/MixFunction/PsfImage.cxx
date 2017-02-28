//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <CCfits/CCfits>
#include "XSstreams.h"
#include <sstream>
#include <cstring>

// PsfImage
#include <PsfImage.h>
const Real PsfImage::s_energy[] = {1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5};
const size_t PsfImage::s_NE = 10;
const Real PsfImage::s_thetas[] = {1.8, 3.8, 6.0, 8.1, 10.3, 13.0, 17.0};
const size_t PsfImage::s_nThetas = 7;
const size_t PsfImage::s_nPhisMax = 3;
const size_t PsfImage::s_nPhis[] = {1, 1, 2, 1, 2, 3, 3};
const Real PsfImage::s_phis[] = {5., 0., 0., 10., 0., 0., 9., 19., 0.,
                                34., 0., 0., 5., 23., 0., 0., 22., 40., 0., 18., 39.};
const size_t PsfImage::s_nPoint[] = {1, 0, 0, 2, 0, 0, 3, 4, 0, 5, 0, 0, 
                                6, 7, 0, 8, 9, 10, 11, 12, 13};
const int PsfImage::s_iDetCent[] = {30, 32, 30, 34, 28, 33, 28, 28, 27, 28, 23, 33,
                                    23, 29, 21, 34, 21, 29, 20, 22, 16, 29, 16, 28,
                                    20, 20};


// Class PsfImage::PsfImageError 

PsfImage::PsfImageError::PsfImageError (const string& msg)
  : YellowAlert("Psf Image Error: ")
{
   std::cerr << msg <<std::endl;
}


// Class PsfImage 
const size_t PsfImage::s_NDIM = 63;
const size_t PsfImage::s_NP = 13;

PsfImage::PsfImage (const string& fileName)
  : m_fileName(),
    m_pGood(0),
    m_pAllPsf(0)
{

  // This code is a translation of the FORTRAN subroutine MAKE_PSF
  // by Maxim Markevitch, distributed with XSPEC prior to version 12.  
  // These comments are from the original file:

  /*
c Input: real E - energy, keV; real theta - off-axis distance, arcmin;
c real phi - position angle, deg (in the det coords, counterclockwise
c from the det x axis);logical qsmooth - if true then use smoothed PSF.
c Output: real psf(NDIM,NDIM) - PSF image with the
c peak brightness at the center of the central pixel, with the pixel
c size pix arcmin (now NDIM=63 and pix=1'), normalized to 1 within the 6'
c radius; real bad(NDIM,NDIM) contains the values of "badness" of the
c psf in each pixel, in the range 0 (ok) to 1 (bad), which are the
c weights in the output image of the zones outside the detector
c calibrated area (r<irdetmax=20'). notok=1 if theta or phi was out of
c range or the data were not read, otherwise notok=0.
c 
c The program uses fitsio library, and uses the file unit ifi=66. 
c
c This function reads a set of preprocessed normalized Asca GIS Cyg X-1
c images each of NDIM x NDIM size, corresp. to NE energy bands and NP
c positions (the filename is set in the code), and interpolates between
c them to model the PSF image at a given position in the focal plane
c and energy. Array thetas contains the values of theta sampled by the
c data, nphis has the number of phi values sampled for each of those
c thetas, phis has those values of phi (its undefined values are set to
c 0), and npoint has the sequential number of the data image (in the
c range 1:NP) corresp. to that theta,phi in the data array. 
c
c The algorythm is to interpolate between 4 bracketing positions. We
c first find the values from the array thetas which bracket the theta we
c need, make images for each of those thetas corresponding to the phi we
c need, and then interpolate between those 2 images to get the needed
c theta. For each of the bracketing theta, if the needed phi is between
c the sampled phis, then interpolate between them, otherwise,
c interpolate between the real image and its reflection in either the x
c axis or the 45 deg diagonal. Here "interpolation" means just the
c linear interpolation between fluxes in the corresponding pixels of
c the data images.
c 
c Please look at the accompanying README file for the description of
c limitations and uncertainties.
c
c Maxim Markevitch <maxim@astro.isas.ac.jp>, 14 Dec 1994

  */
  m_fileName = FunctionUtility::modelDataPath() + fileName;
  readImage();
  initGoodArray();
}


PsfImage::~PsfImage()
{
}


void PsfImage::calcPsf (Real E, Real theta, Real phi, RealArray& psf)
{
  const Real* allPsf = m_pAllPsf.get();
  const bool* good = m_pGood.get();
  const size_t NDIM2 = s_NDIM*s_NDIM;

  if (theta < .0 || theta > s_thetas[s_nThetas-1])
  {
     std::ostringstream msg;
     msg << "theta = " << theta << " is out of allowed range, 0 - "
         << s_thetas[s_nThetas-1] << std::endl;
     throw PsfImageError(msg.str());
  }

  // Convert phi to 0-45 deg interval.
  int irot = static_cast<int>(phi/90.0);
  if (irot < 0 || irot > 3)
  {
     std::ostringstream msg;
     msg << "phi = " << phi << " is out of allowed range, 0 - 360" << std::endl;
     throw PsfImageError(msg.str());
  }

  int isym = static_cast<int>(phi/45.0);
  // Check if isym is odd. If it is, so is symmetry with respect
  // to the diagonal.
  bool asym = (2*(isym/2) < isym);
  Real phi0 = asym ? 90.0-(phi-90.0*irot) : phi-90.0*irot;

  size_t ie0, ie1;
  Real fe, fe1;
  getEnergyBracket(E, ie0, ie1, fe);
  const size_t ie0Offset = ie0*NDIM2;
  const size_t ie1Offset = ie1*NDIM2;

  fe1 = 1.0 - fe;

  // Using C-arrays for speed.
  Real* bad = new Real[NDIM2];
  Real* work = new Real[NDIM2]; 
  Real* work1 = new Real[NDIM2];
  Real* cPsf = new Real[NDIM2];
  memset(bad, 0, NDIM2*sizeof(Real));
  memset(work, 0, NDIM2*sizeof(Real));
  memset(work1, 0, NDIM2*sizeof(Real));
  memset(cPsf, 0, NDIM2*sizeof(Real));

  // Interpolate between positions.
  size_t ith0, ith1;
  Real fth, fth1;
  if (theta <= s_thetas[0])
  {
     // Interpolate between the first theta and its centrally
     // symmetric image.
     ith0 = 0;
     ith1 = 0;
     fth = (theta + s_thetas[0])/(2*s_thetas[0]);
     fth1 = 1.0 - fth;     
  }
  else if (theta >= s_thetas[s_nThetas-1])
  {
     // ???
  }
  else 
  {
     size_t i=0;
     while (i < s_nThetas-1)
     {
        if (theta >= s_thetas[i] && theta < s_thetas[i+1])  break;
        ++i;
     }
     ith0 = i;
     ith1 = i+1;
     fth = (theta - s_thetas[ith0])/(s_thetas[ith1] - s_thetas[ith0]);
     fth1 = 1.0 - fth;
  }

  // interpolate between phis for each of the 2 bracketing thetas (between
  // points ip0 and ip1). for the lower theta thetas(ith0), the result will
  // be stored in the array work, and for the upper theta thetas(ith1),
  // the array psf:

  size_t ip0, ip1;
  Real fp, fp1;
  size_t l_ith0 = s_nPhisMax*ith0;
  size_t h_ith0 = s_nPhis[ith0]-1 + s_nPhisMax*ith0;
  // lower theta:
  if (phi0 < s_phis[l_ith0])
  {
   // phi0 is between 0 and the first of phis. interp between the image
   // corresp. to the first of phis and the same image reflected with resp.
   // to x-axis:
     ip0 = s_nPoint[l_ith0] - 1;
     ip1 = ip0;
     const size_t ip0Offset = ip0*NDIM2*s_NE;
     const size_t ip1Offset = ip0Offset;

     fp = (phi0+s_phis[l_ith0])/(2*s_phis[l_ith0]);
     fp1 = 1.0 - fp;

     for (size_t i=0; i<s_NDIM; ++i)
     {
        // Following the FORTRAN notation work(i,j), i varies first.
        for (size_t j=0; j<s_NDIM; ++j)
        {
           size_t ii, jj;
           rotSym(i, j, irot, asym, ii, jj);
           const size_t jj0Offset = (s_NDIM-1-jj)*s_NDIM;
           const size_t jj1Offset = jj*s_NDIM;
           // In FORTRAN notation, allPsf(ii,jj,ie,ip)
           work[i+s_NDIM*j] = fe1*(fp1*allPsf[ii+jj0Offset+ie0Offset+ip0Offset] +
                        fp*allPsf[ii+jj1Offset+ie0Offset+ip1Offset]) +
                        fe*(fp1*allPsf[ii+jj0Offset+ie1Offset+ip0Offset] +
                        fp*allPsf[ii+jj1Offset+ie1Offset+ip1Offset]);

           // det center distance check:
           if (!good[ii+jj0Offset+NDIM2*ip0])
           {
              work1[i+s_NDIM*j] += fp1;
           }
           if (!good[ii+jj1Offset+NDIM2*ip1])
           {
              work1[i+s_NDIM*j] += fp;
           }
        }
     }
  } // end if phi0 below low range
  else if (phi0 >= s_phis[h_ith0])
  {
     // phi0 is between the largest of phis and 45 deg. interp between the
     // image corresp. to the largest phis and the same image reflected with
     // resp. to the diagonal:
     ip0 = s_nPoint[h_ith0] - 1;
     ip1 = ip0;
     const size_t ip0Offset = ip0*NDIM2*s_NE;
     const size_t ip1Offset = ip0Offset;

     fp = (phi0-s_phis[h_ith0])/(2*(45.-s_phis[h_ith0]));
     fp1 = 1.0 -fp;

     for (size_t i=0; i<s_NDIM; ++i)
     {
        for (size_t j=0; j<s_NDIM; ++j)
        {
           size_t ii, jj;
           rotSym(i, j, irot, asym, ii, jj);
           const size_t iiOffset = ii*s_NDIM;
           const size_t jjOffset = jj*s_NDIM;

           work[i+s_NDIM*j] = 
                 fe1*(fp1*allPsf[ii+jjOffset+ie0Offset+ip0Offset]
                 +fp*allPsf[jj+iiOffset+ie0Offset+ip1Offset])
                 +fe*(fp1*allPsf[ii+jjOffset+ie1Offset+ip0Offset]
                 +fp*allPsf[jj+iiOffset+ie1Offset+ip1Offset]);

           // det center distance check:
           if (!good[ii+jjOffset+NDIM2*ip0])
           {
              work1[i+s_NDIM*j] += fp1;
           }
           if (!good[jj+iiOffset+NDIM2*ip1])
           {
              work1[i+s_NDIM*j] += fp;
           }
        }
     }
  } // end if phi0 above high range
  else
  {
    // between 2 real points:
     // already checked for out of bounds cases
     size_t i=0;     
     while (!(phi0 >= s_phis[i+s_nPhisMax*ith0] && 
                phi0 < s_phis[i+1+s_nPhisMax*ith0]))
     {
        ++i;
     }
     const size_t iElem = i + s_nPhisMax*ith0;
     ip0 = s_nPoint[iElem] - 1;
     ip1 = s_nPoint[iElem+1] - 1;
     const size_t ip0Offset = ip0*NDIM2*s_NE;
     const size_t ip1Offset = ip1*NDIM2*s_NE;

     fp = (phi0-s_phis[iElem])/(s_phis[iElem+1]-s_phis[iElem]);
     fp1 = 1.0 - fp;
     for (i=0; i<s_NDIM; ++i)
     {
        for (size_t j=0; j<s_NDIM; ++j)
        {
           size_t ii, jj;
           rotSym(i, j, irot, asym, ii, jj);
           const size_t jjOffset = jj*s_NDIM;
           work[i+s_NDIM*j] =
              fe1*(fp1*allPsf[ii+jjOffset+ie0Offset+ip0Offset]
              +fp*allPsf[ii+jjOffset+ie0Offset+ip1Offset])
              +fe*(fp1*allPsf[ii+jjOffset+ie1Offset+ip0Offset]
              +fp*allPsf[ii+jjOffset+ie1Offset+ip1Offset]);

           if (!good[ii+jjOffset+NDIM2*ip0])
           {
              work1[i+s_NDIM*j] += fp1;
           }
           if (!good[ii+jjOffset+NDIM2*ip1])
           {
              work1[i+s_NDIM*j] += fp;
           }
        }
     }
  } // end if between 2 real points

  // the same for upper theta (except the special case of ith1=1,
  // where we interpolate between theta and -theta):
  size_t l_ith1 = s_nPhisMax*ith1;
  size_t h_ith1 = s_nPhis[ith1]-1 + s_nPhisMax*ith1;
  if (ith1 > 0)
  {
     if (phi0 < s_phis[l_ith1])
     {
      // phi0 is between 0 and the first of phis. interp between the image
      // corresp. to the first of phis and the same image reflected with resp.
      // to x-axis:
        ip0 = s_nPoint[l_ith1] - 1;
        ip1 = ip0;
        const size_t ip0Offset = ip0*NDIM2*s_NE;
        const size_t ip1Offset = ip0Offset;

        fp = (phi0+s_phis[l_ith1])/(2*s_phis[l_ith1]);
        fp1 = 1.0 - fp;

        for (size_t i=0; i<s_NDIM; ++i)
        {
           // Following the FORTRAN notation work(i,j), i varies first.
           for (size_t j=0; j<s_NDIM; ++j)
           {
              size_t ii, jj;
              rotSym(i, j, irot, asym, ii, jj);
              const size_t jj0Offset = (s_NDIM-1-jj)*s_NDIM;
              const size_t jj1Offset = jj*s_NDIM;
              // In FORTRAN notation, allPsf(ii,jj,ie,ip)
              cPsf[i+s_NDIM*j] = fe1*(fp1*allPsf[ii+jj0Offset+ie0Offset+ip0Offset] +
                           fp*allPsf[ii+jj1Offset+ie0Offset+ip1Offset]) +
                           fe*(fp1*allPsf[ii+jj0Offset+ie1Offset+ip0Offset] +
                           fp*allPsf[ii+jj1Offset+ie1Offset+ip1Offset]);

              // det center distance check:
              if (!good[ii+jj0Offset+NDIM2*ip0])
              {
                 bad[i+s_NDIM*j] += fp1;
              }
              if (!good[ii+jj1Offset+NDIM2*ip1])
              {
                 bad[i+s_NDIM*j] += fp;
              }
           }
        }
     } // end if phi0 below low range
     else if (phi0 >= s_phis[h_ith1])
     {
        // phi0 is between the largest of phis and 45 deg. interp between the
        // image corresp. to the largest phis and the same image reflected with
        // resp. to the diagonal:
        ip0 = s_nPoint[h_ith1] - 1;
        ip1 = ip0;
        const size_t ip0Offset = ip0*NDIM2*s_NE;
        const size_t ip1Offset = ip0Offset;

        fp = (phi0-s_phis[h_ith1])/(2*(45.-s_phis[h_ith1]));
        fp1 = 1.0 -fp;

        for (size_t i=0; i<s_NDIM; ++i)
        {
           for (size_t j=0; j<s_NDIM; ++j)
           {
              size_t ii, jj;
              rotSym(i, j, irot, asym, ii, jj);
              const size_t iiOffset = ii*s_NDIM;
              const size_t jjOffset = jj*s_NDIM;
              cPsf[i+s_NDIM*j] = 
                    fe1*(fp1*allPsf[ii+jjOffset+ie0Offset+ip0Offset]
                    +fp*allPsf[jj+iiOffset+ie0Offset+ip1Offset])
                    +fe*(fp1*allPsf[ii+jjOffset+ie1Offset+ip0Offset]
                    +fp*allPsf[jj+iiOffset+ie1Offset+ip1Offset]);

              // det center distance check:
              if (!good[ii+jjOffset+NDIM2*ip0])
              {
                 bad[i+s_NDIM*j] += fp1;
              }
              if (!good[jj+iiOffset+NDIM2*ip1])
              {
                 bad[i+s_NDIM*j] += fp;
              }
           }
        }
     } // end if phi0 above high range
     else
     {
       // between 2 real points:
        // already checked for out of bounds cases
        size_t i=0;     
        while (!(phi0 >= s_phis[i+s_nPhisMax*ith1] && 
                   phi0 < s_phis[i+1+s_nPhisMax*ith1]))
        {
           ++i;
        }
        const size_t iElem = i + s_nPhisMax*ith1;
        ip0 = s_nPoint[iElem] - 1;
        ip1 = s_nPoint[iElem+1] - 1;
        const size_t ip0Offset = ip0*NDIM2*s_NE;
        const size_t ip1Offset = ip1*NDIM2*s_NE;

        fp = (phi0-s_phis[iElem])/(s_phis[iElem+1]-s_phis[iElem]);
        fp1 = 1.0 - fp;
        for (i=0; i<s_NDIM; ++i)
        {
           for (size_t j=0; j<s_NDIM; ++j)
           {
              size_t ii, jj;
              rotSym(i, j, irot, asym, ii, jj);
              const size_t jjOffset = jj*s_NDIM;
              cPsf[i+s_NDIM*j] = 
                 fe1*(fp1*allPsf[ii+jjOffset+ie0Offset+ip0Offset]
                 +fp*allPsf[ii+jjOffset+ie0Offset+ip1Offset])
                 +fe*(fp1*allPsf[ii+jjOffset+ie1Offset+ip0Offset]
                 +fp*allPsf[ii+jjOffset+ie1Offset+ip1Offset]);

              if (!good[ii+jjOffset+NDIM2*ip0])
              {
                 bad[i+s_NDIM*j] += fp1;
              }
              if (!good[ii+jjOffset+NDIM2*ip1])
              {
                 bad[i+s_NDIM*j] += fp;
              }
           }
        }
     } // end if between 2 real points
  } // end if ith1 > 0

  // Now interpolate between thetas:
  if (ith1 > 0)
  {
     for (size_t i=0; i<s_NDIM; ++i)
     {
        for (size_t j=0; j<s_NDIM; ++j)
        {
           size_t iElem = i+s_NDIM*j;
           cPsf[iElem] = fth1*work[iElem] + fth*cPsf[iElem];
           bad[iElem] = fth1*work1[iElem] + fth*bad[iElem];
        }
     }
  }
  else
  {
     // central symmetry:
     for (size_t i=0; i<s_NDIM; ++i)
     {
        for (size_t j=0; j<s_NDIM; ++j)
        {
           size_t iElem = i+s_NDIM*j;
           size_t iElem1 = s_NDIM-1-i + s_NDIM*(s_NDIM-1-j);
           cPsf[iElem] = fth1*work[iElem1] + fth*work[iElem];
           bad[iElem] = fth1*work1[iElem1] + fth*work1[iElem];
        }
     }
  }

  psf.resize(NDIM2);
  for (size_t i=0; i<NDIM2; ++i)
  {
     psf[i] = cPsf[i];
  }
  delete [] bad;
  delete [] work;
  delete [] work1; 
  delete [] cPsf; 
}

void PsfImage::readImage ()
{
  using namespace CCfits;
  try
  {
     std::auto_ptr<FITS> p(new FITS(m_fileName));
     PHDU& primary = p->pHDU();
     RealArray vAllPsf;
     long nElem = s_NDIM*s_NDIM*s_NP*s_NE;
     primary.read(vAllPsf, 1, nElem);
     size_t sz = vAllPsf.size();
     // gnu compiler on linux doesn't allow this, not sure why:
     //m_pAllPsf = XSutility::auto_array_ptr<Real>(new Real[sz]);
     XSutility::auto_array_ptr<Real> tmpAllPsf(new Real[sz]);
     m_pAllPsf = tmpAllPsf;
     Real* allPsf = m_pAllPsf.get();
     for (size_t i=0; i<sz; ++i)
     {
        allPsf[i] = vAllPsf[i];
     }
  }
  catch (CCfits::FITS::CantOpen)
  {
     string msg = "Cannot open image file: ";
     msg += m_fileName;
     throw PsfImageError(msg);
  }
  catch (CCfits::FitsException&)
  {
     string msg = "Error attempting to read image from file: ";
     msg += m_fileName;
     throw PsfImageError(msg);
  }
}

void PsfImage::initGoodArray ()
{
  const int iRDetMax = 20*20;
  const size_t NDIM2 = s_NDIM*s_NDIM;
  // See comments in readImage regarding auto_array_ptr init.
  XSutility::auto_array_ptr<bool> tmpGood(new bool[s_NP*NDIM2]);
  m_pGood = tmpGood;
  bool* good = m_pGood.get();
  for (size_t ip=0; ip<s_NP; ++ip)
  {
     int i_detCent = s_iDetCent[2*ip];
     int j_detCent = s_iDetCent[2*ip+1];
     size_t ipOffset = ip*NDIM2;
     for (int i=0; i<(int)s_NDIM; ++i)
     {
        int iDist = (i+1-i_detCent)*(i+1-i_detCent);
        for (int j=0; j<(int)s_NDIM; ++j)
        {
           int jDist = (j+1-j_detCent)*(j+1-j_detCent);
           good[i+s_NDIM*j+ipOffset] = ((iDist + jDist) <= iRDetMax); 
        }
     }
  }
}

void PsfImage::rotSym (size_t i, size_t j, int irot, bool asym, size_t& ii, size_t& jj)
{
  switch (irot)
  {
     case 0:
        ii = i;
        jj = j;
        break;
     case 1:
        ii = j;
        jj = s_NDIM - 1 - i;
        break;
     case 2:
        ii = s_NDIM - 1 - i;
        jj = s_NDIM - 1 - j;
        break;
     default:
        ii = s_NDIM - 1 -j;
        jj = i;
        break;                  
  }
  if (asym)
  {
     size_t tmp = ii;
     ii = jj;
     jj = tmp;
  }
}

void PsfImage::getEnergyBracket (Real E, size_t& ie0, size_t& ie1, Real& fe)
{
  static bool toldFlag1 = false;
  static bool toldFlag2 = false;
  if (E <= s_energy[0])
  {
     ie0 = 0;
     ie1 = 0;
     fe = .0;
     if (!toldFlag1)
     {
        tcout << "\nPsfImage: E = " << E << " below " << s_energy[0] 
              << " keV, use " << s_energy[0] << " keV" << std::endl;
        toldFlag1 = true;
     }
  }
  else if (E >= s_energy[s_NE-1])
  {
     ie0 = s_NE-1;
     ie1 = s_NE-1;
     fe = .0;
     if (!toldFlag2)
     {
        tcout << "\nPsfImage: E = " << E << " above " << s_energy[s_NE-1] 
              << " keV, use " << s_energy[s_NE-1] << " keV" << std::endl;
        toldFlag2 = true;      
     }
  }
  else
  {
     size_t i=0;
     while (i<s_NE-1)
     {
        if (E >= s_energy[i] && E < s_energy[i+1])  break;
        ++i;
     }
     ie0 = i;
     ie1 = i+1;
     fe = (E - s_energy[ie0])/(s_energy[ie1] - s_energy[ie0]);
  }
}

size_t PsfImage::NDIM ()
{
  return s_NDIM;
}

// Additional Declarations
