/// \file sxsrmflib.cxx
/// \brief functions for sxsrmf
/// \author Mike Dutka
/// \date $Date: 2015/12/16 20:31:58 $
 
#define AHLABEL tool_sxsrmf_sxsrmflib
#define AHCVSID "$Id: sxsrmflib.cxx,v 1.10 2015/12/16 20:31:58 mdutka Exp $"

//local definitions
#define RESOLUTIONS (3) //how many different energy resolutions are availible  
const int NPIXELS = 36;

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "sxsrmflib.h"

#include <stdlib.h>
#include <iostream>
#include <sstream>
#include <cctype>
#include <vector>

namespace sxsrmf_sigma {

// ---------------------------------------------------------------------------

void load(const std::string & filename, DataType & dat, long nfrac,
          std::vector<int> pixInfile, std::vector<int> resolInfile) {

  
  // declare ahfits pointer and variables to store column values
  ahfits::FilePtr fptr=0;
  int ii = 0;
  double energy = 0;
  double fwhm[NPIXELS][RESOLUTIONS];
  const double eVtokeV = 0.001;   

  //declare variables to determine which pixel column we are reading from
  std::stringstream ss;
  std::string pixelname;
  int ipix = 0;
  int ires = 0;
  

  // open file
  ahfits::open(filename,"LINESIGMA",&fptr);
  AH_INFO(ahlog::HIGH) << "Opened " << filename << " extension LINESIGMA" << std::endl;
  if (!ahfits::readOK(fptr)) {
    std::stringstream msg;
    msg << "Sigma CALDB file " << filename << " has no data";
    AH_THROW_RUNTIME(msg.str());
  }

  //read range keyword
  dat.m_de_gaus[0] =eVtokeV * ahfits::getKeyValDbl(fptr, "RANGEHIG");
  dat.m_de_gaus[1] =eVtokeV * ahfits::getKeyValDbl(fptr, "RANGEMID");
  dat.m_de_gaus[2] =eVtokeV * ahfits::getKeyValDbl(fptr, "RANGELOW");
  
  //read naxis2 (number of rows) keyword
  dat.m_naxis2 = ahfits::getKeyValLLong(fptr, "NAXIS2");

  AH_INFO(ahlog::LOW) << "Number of linesigma energies: " << dat.m_naxis2 << std::endl;
  AH_INFO(ahlog::LOW) << "Range of the Gaussian component: " << dat.m_de_gaus << std::endl; 

  // assign memory to dynamic arrays, size = naxis2 and
  // initialize all values to 0
  int size = dat.m_naxis2;
  
  dat.m_egrid_tab = new double[size];
  for (int jj = 0; jj < size; ++jj) dat.m_egrid_tab[jj] = 0.0;
  
  dat.m_fwhm = new double *[nfrac];
  for (int jj = 0; jj < nfrac; ++jj) dat.m_fwhm[jj] = new double[size];

  // setup router
  ahfits::Router router(fptr);

  // make connections to local variables
  router.connectScalar(ahfits::e_READONLY,"Energy",energy);

  //connteting 2-D array fwhm to all 36 pixels and all three grades for each pixel
  for (int pix=0; pix<NPIXELS; ++pix) {
    ss << pix;
    pixelname = "PIXEL" + ss.str();
    ss.str("");
    router.connectFixedLengthArray(ahfits::e_READONLY,pixelname,fwhm[pix]);
  }

  // read table and store column values to the dat struct under the sxs_sigma namespace
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    dat.m_egrid_tab[ii]=eVtokeV * energy;
    for (int ifrac=0; ifrac<nfrac; ifrac++) {
      ipix = pixInfile[ifrac];
      ires = resolInfile[ifrac];
      dat.m_fwhm[ifrac][ii] = eVtokeV * fwhm[ipix][ires];
    }     
    ++ii;
  }

  // close FITS file
  ahfits::close(fptr);
}

void cleanup(DataType & dat,long nfrac) {
 
  delete [] dat.m_egrid_tab; 
  for (int ii=0; ii<nfrac; ++ii) {
    delete [] dat.m_fwhm[ii];
  }
  delete [] dat.m_fwhm;
}
  
    



}  // namespace sxsrmf_sigma


namespace sxsrmf_tau {
  
// ---------------------------------------------------------------------------

void load(const std::string & filename, DataType & dat, std::string whichrmf) {
   
 
  // declare ahfits pointer and variables to store column values and header keywords
  ahfits::FilePtr fptr=0;
  
  double energy = 0;
  int row = 0;
  double tau = 0;
  double frac_exp = 0;
  double frac_cont = 0;
  std::stringstream ss;
  std::string epeak_offset;
  std::string fpeak_colname;
  std::vector<std::string> epeak_column_names;
  std::vector<double> frac_peak;
  const double eVtokeV = 0.001;
  
  // open file
  ahfits::open(filename,"LINETAU",&fptr);

  AH_INFO(ahlog::HIGH) << "Opened " << filename << " extension LINETAU" << std::endl;

  if (!ahfits::readOK(fptr)) {
    std::stringstream msg;
    msg << "Tau CALDB file " << filename << " has no data";
    AH_THROW_RUNTIME(msg.str());
  }

  //read header keywords
  dat.m_npeak = ahfits::getKeyValLLong(fptr, "NPEAK");
  dat.m_de_exp = eVtokeV * ahfits::getKeyValDbl(fptr, "RANGETAU");
  dat.m_naxis2 = ahfits::getKeyValLLong(fptr, "NAXIS2");

  AH_INFO(ahlog::LOW) << "Number of linetau energies: " << dat.m_naxis2 << std::endl;
  AH_INFO(ahlog::LOW) << "Range of exponential: " << dat.m_de_exp << std::endl;
  AH_INFO(ahlog::LOW) << "Number of escape peaks: " << dat.m_npeak << std::endl;

  // assign memory to dynamic arrays, size = naxis2 and initialize all values
  int size = dat.m_naxis2;
  int npeak = dat.m_npeak;     

  dat.m_egrid_tab = new double[size];
  for (int jj = 0; jj < size; ++jj) dat.m_egrid_tab[jj] = 0.0;    
   
  dat.m_tau = new double[size];
  for (int jj = 0; jj < size; ++jj) dat.m_tau[jj] = 0.0;

  dat.m_frac_exp = new double[size];
  for (int jj = 0; jj < size; ++jj) dat.m_frac_exp[jj] = 0.0;

  dat.m_frac_cont = new double[size];
  for (int jj = 0; jj < size; ++jj) dat.m_frac_cont[jj] = 0.0; 

  dat.m_frac_peak = new double*[npeak];
  for (int jj = 0; jj < npeak; ++jj){
    dat.m_frac_peak[jj] = new double[size];
    for (int kk = 0; kk < size; ++kk) dat.m_frac_peak[jj][kk] = 0.0;
  }

  dat.m_de_peak = new double[npeak];
  
  for (int jj = 0; jj < npeak; ++jj) {
    dat.m_de_peak[jj] = 0.0;
  }
  
  AH_INFO(ahlog::LOW) << "Escape peak energies: ";

  //The header and column name of the escape peaks are stored in the vector de_peak 
  for (int ii = 1; ii <= dat.m_npeak; ++ii){
    ss << ii;
    epeak_offset = "EP" + ss.str();
    fpeak_colname = "F_EP" + ss.str();
    epeak_column_names.push_back(fpeak_colname);
    dat.m_de_peak[ii-1] = eVtokeV * ahfits::getKeyValDbl(fptr, epeak_offset);
    AH_INFO(ahlog::LOW) << dat.m_de_peak[ii-1] << std::endl;
    ss.str(std::string());
  }
  AH_INFO(ahlog::LOW) << std::endl;
  
  //give vector frac_peak length NPEAK in order to store fractional energy values from the escape peaks
  for (double ii = 0; ii != dat.m_npeak; ++ii){
    frac_peak.push_back(0);
  }

  // setup router
  ahfits::Router router(fptr);
  
  // make connections to local variables
  router.connectScalar(ahfits::e_READONLY,"Energy",energy);
  router.connectScalar(ahfits::e_READONLY,"TAU",tau);
  router.connectScalar(ahfits::e_READONLY,"F_TAU",frac_exp);
  router.connectScalar(ahfits::e_READONLY,"F_ELC",frac_cont);
  for (int ii = 0; ii != dat.m_npeak; ++ii){
    router.connectScalar(ahfits::e_READONLY,epeak_column_names[ii],frac_peak[ii]);
  }

  // read table and store the data into the dat struct under the sxsrmf_tau namespace 
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    dat.m_egrid_tab[row]=eVtokeV * energy;
    dat.m_tau[row]=eVtokeV * tau;
    dat.m_frac_exp[row]=frac_exp;
    dat.m_frac_cont[row]=frac_cont;
    for (int column = 0; column != dat.m_npeak; ++column)
      dat.m_frac_peak[column][row] = frac_peak[column];
    ++row;
  }

  // close FITS file
  ahfits::close(fptr);

 
}


void cleanup(DataType & dat) {
 
  delete [] dat.m_egrid_tab;
  delete [] dat.m_tau;
  delete [] dat.m_frac_exp;
  delete [] dat.m_frac_cont;
  for (int ii=0; ii<dat.m_npeak; ++ii) {
    delete [] dat.m_frac_peak[ii];
  }
  delete [] dat.m_frac_peak;
  delete [] dat.m_de_peak;
}

}  // namespace sxsrmf_tau




/* Revision Log
 $Log: sxsrmflib.cxx,v $
 Revision 1.10  2015/12/16 20:31:58  mdutka
 correcting memory leaks

 Revision 1.9  2015/12/08 18:43:55  mdutka
 updating sxsrmf to handle multiple pixels and grades

 Revision 1.8  2015/07/31 18:51:55  mwitthoe
 sxsrmf: fix typos in log statements

 Revision 1.7  2015/07/31 18:17:54  mwitthoe
 sxsrmf: fix bug where only the small RMF was being computed; updated tool prologue to standard form; general clean-up

 Revision 1.6  2015/07/27 14:01:19  mdutka
 Updateing sxsrmf to handel multiple grids

 Revision 1.5  2014/09/12 17:52:26  mdutka
 Added support for the new response writing tools (rmflib)

 Revision 1.4  2014/08/04 17:19:36  mdutka
 Fixed converting double to integer warnings

 Revision 1.3  2014/07/10 18:47:19  mdutka
 removed debugging message

 Revision 1.2  2014/07/10 18:21:11  mdutka
 all dynamically sized arrays are now initialized to zero, added dynamic sizing to containers which store data from the CALDB information

 Revision 1.1  2014/06/30 19:02:36  mdutka
 Added sxsrmf tool

 Revision 1.3  2014/01/22 20:40:21  mwitthoe
 sxsflagpix: update accordng to code review; issue 331

 Revision 1.2  2014/01/15 22:26:33  peachey
 Add Peer Review comments regarding variable declarations,

 Revision 1.1  2013/11/12 17:45:03  mwitthoe
 sxsflagpix: moved getPar_warn() functions to ahapp; renamed sxspixel map to sxsflagpixlib; worked on standards compliance for source (mainly moving variable declarations to top of scope)



 BELOW IS THE REVISION LOG FOR sxspixelmap.cxx BEFORE RENAMING TO sxsflagpixlib.cxx

 Revision 1.3  2013/10/07 21:20:11  mwitthoe
 sxsflagpix: switch over to new ahfits connection functions (issue 270)

 Revision 1.2  2013/04/16 17:19:26  mwitthoe
 sxsflagpix: change source to match latest TRF (major change to cross-talk algorithm, add MXS algorithm); modify form ahcaldb library, sxspixelmap, to follow new standards

 Revision 1.1  2013/01/02 19:09:52  mwitthoe
 add new ahcaldb library for the SXS pixel map


*/
