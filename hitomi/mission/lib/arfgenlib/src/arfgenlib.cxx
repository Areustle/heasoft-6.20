/// \file arfgenlib.cxx
/// \brief Shared library for arf generator tools
/// \author Mike Dutka
/// \date $Date: 2016/07/29 17:47:25 $

#define AHLABEL arfgenlib_arfgenlib
#define AHCVSID "$Id: arfgenlib.cxx,v 1.32 2016/07/29 17:47:25 mdutka Exp $"

#include "ahfits/ahfits.h"
#include "ahmission/ahmission.h"
#include "ahfits/ahfits.h"
#include "fitsio.h"
#include "arfgenlib/arfgenlib.h"
#include "ahgen/ahgen.h"
#include "ahapp/ahapp.h"
#include "ahlog/cahlog.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <string.h>
#include <algorithm>
#include <iostream>
#include <vector>
#include <math.h>
#include <sstream>

//Global variable definitions
const int MAX_NGRP = 10;      //Upper limit to the number of groups 
                              //in a response element for Astro-H
const double eVtokeV = 0.001; //conversion factor from eV to keV

namespace arfgenlib {

// -----------------------------------------------------------------------------

void rmfproc(std::string & filename, bool & rmfflag, s_rmfegrid & rmfegrid, 
             s_rmfmatrix & rmfmatrix, bool & kevunits)
{
  ahfits::FilePtr ahffp;            //file pointer to rmf file
  std::string eunit;                //units used for energy must be eV or keV
  long detchans = 0;                //DETCHANS keyword from rmf file/maximum
                                    //length of matrix column element

  // Local variables to store rmf column data
  double l_energ_lo = 0.;            //energ_lo column data
  double l_energ_hi = 0.;            //energ_hi column data
  int l_ngrp = 0;                    //number of groups
  int * l_nchan = 0;                 //n_chan column data
  ahfits::IndexType nnchan = 0;      //number of elements in row of nchan column
  std::vector<int> nchan_row;        //single row of nchan column
  int * l_fchan = 0;                 //f_chan column data
  ahfits::IndexType nfchan = 0;      //number of element in ro of fchan column
  std::vector<int> fchan_row;        //single row of fchna column
  double * l_matrix = 0;             //matrix column data
  ahfits::IndexType nmatrix = 0;     //number of elements in matrix column
  std::vector<double> matrix_row;    //single row of matrix column
  double l_emin = 0.;                //emin column data
  double l_emax = 0.;                //emax column data

  ahfits::open(filename, "MATRIX", &ahffp);
  
  rmfegrid.m_nebin = ahfits::getKeyValLLong(ahffp, "NAXIS2");
  detchans = ahfits::getKeyValLLong(ahffp, "DETCHANS");
  
  // Determine which energy unit is used, must be either eV or keV
  eunit = ahfits::getKeyValStr(ahffp, "TUNIT1");
  if (eunit == "keV") {
    kevunits = true;
  } else if (eunit == "eV") {
    kevunits = false;
  } else {
    AH_THROW_RUNTIME("Energy units in rmf file must be eV or keV -ABORTING");
  }
  
  // Setup router
  ahfits::Router router(ahffp);
 
  // Intialize variables which will hold varible length column data
  l_nchan = new int[MAX_NGRP];
  l_fchan = new int[MAX_NGRP];
  l_matrix = new double[detchans];
  
  // Make connections to local varibles Matrix extension
  router.connectScalar(ahfits::e_READONLY,"ENERG_LO",l_energ_lo);
  router.connectScalar(ahfits::e_READONLY,"ENERG_HI",l_energ_hi);
  router.connectScalar(ahfits::e_READONLY,"N_GRP",l_ngrp);


  // Read and store energy grid from MATRIX extension
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp);
    rmfegrid.m_energ_lo.push_back(l_energ_lo);
    rmfegrid.m_energ_hi.push_back(l_energ_hi);
    rmfegrid.m_ecenter.push_back(0.5 * (l_energ_lo + l_energ_hi));
  }
  
  // If rmfflag is false close file and return energy grid only
  if (!rmfflag) {
   ahfits::close(ahffp);
   return;
  }

  router.clearConnections();

  router.connectVariableLengthArray(ahfits::e_READONLY,"F_CHAN",l_fchan,nfchan);
  router.connectVariableLengthArray(ahfits::e_READONLY,"N_CHAN",l_nchan,nnchan);
  router.connectVariableLengthArray(ahfits::e_READONLY,"MATRIX",l_matrix,nmatrix);

  // Read and full RMF data from MATRIX extension
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp);
    rmfmatrix.m_ngrp.push_back(l_ngrp);
    for (int ii = 0; ii < nnchan; ++ii) {
      nchan_row.push_back(l_nchan[ii]);
    }
    rmfmatrix.m_nchan.push_back(nchan_row);
    for (int ii = 0; ii < nfchan; ++ii) {
      fchan_row.push_back(l_fchan[ii]);
    }
    rmfmatrix.m_fchan.push_back(fchan_row);
    for (int ii = 0; ii < nmatrix; ++ii) {
      matrix_row.push_back(l_matrix[ii]);
    }
    rmfmatrix.m_matrix.push_back(matrix_row);
  }
  router.clearConnections();

  // Move to ebounds extension and store E_MIN and E_MAX columns
  ahfits::move(ahffp,"EBOUNDS");
  router.connectScalar(ahfits::e_READONLY,"E_MIN",l_emin);
  router.connectScalar(ahfits::e_READONLY,"E_MAX",l_emax);
  
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
   ahfits::readRow(ahffp);
   rmfegrid.m_ebounds_lo.push_back(l_emin);
   rmfegrid.m_ebounds_hi.push_back(l_emax);
  }
  ahfits::close(ahffp); 
}

// -----------------------------------------------------------------------------

void getarfdata(std::string & arffile, long & numarfens, 
                std::vector<double> & arf_energ_lo, 
                std::vector<double> & arf_energ_hi, 
                std::vector<double> & arf_ecenter, 
                std::vector<double> & arfeffarea) {
  
  ahfits::FilePtr ahffp;   // Ahfits file pointer to arf data 
  std::string eunit;       // Energy units (keV or eV)
  double l_energ_lo = 0.;  // Local container for ENERG_HI column
  double l_energ_hi = 0.;  // Local container for ENERG_LO column
  double l_arfeffarea = 0.; // Local container for SPECRESP column

  // Open arf file
  ahfits::open(arffile,"",&ahffp);
  
  //moving to extension 1
  ahfits::move(ahffp,2);

  //Number of energy & effective area points = number of rows
  numarfens = ahfits::getKeyValLLong(ahffp,"NAXIS2");
  eunit = ahfits::getKeyValStr(ahffp,"TUNIT1");
  
  //setup router and connect local variables to columns 
  ahfits::Router router(ahffp);
  router.connectScalar(ahfits::e_READONLY,"ENERG_LO",l_energ_lo);
  router.connectScalar(ahfits::e_READONLY,"ENERG_HI",l_energ_hi);
  router.connectScalar(ahfits::e_READONLY,"SPECRESP",l_arfeffarea);
 
  // Read and store columns ENERG_LO and ENERG_HI; Regardless of energy units in
  // Output file, the energy units returned by this routine are keV because the 
  // arfgen routines work internally with keV units only.  The center energy is
  // also determined
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp); 
    if (eunit == "eV") {
      arf_energ_lo.push_back(l_energ_lo*eVtokeV);
      arf_energ_hi.push_back(l_energ_hi*eVtokeV);
      arf_ecenter.push_back(0.5 * (l_energ_lo + l_energ_hi));
      arfeffarea.push_back(l_arfeffarea);
    } else if (eunit == "keV") { 
      arf_energ_lo.push_back(l_energ_lo);
      arf_energ_hi.push_back(l_energ_hi);
      arf_ecenter.push_back(0.5 * (l_energ_lo + l_energ_hi));
      arfeffarea.push_back(l_arfeffarea);    
    } else { 
      AH_THROW_RUNTIME("energy unit in arf file is not eV or keV - Aborting");
    }  
  }
  
  // Close the input arf file
  ahfits::close(ahffp);

}

// -----------------------------------------------------------------------------

void geteadata(std::string & eafile, long & numeaens, 
               std::vector<double> & ea_ecenter, 
               std::vector<double> & effarea) { 

  ahfits::FilePtr ahffp = 0;    // Ahfits file pointer to effective area file  
  std::string eunit;            // Energy unit (keV or eV)
  double l_ea_ecenter = 0.;     // local container for energy column
  double l_effarea = 0.;        // local container for EFFAREA column
  
  // Open effective area file
  ahfits::open(eafile,"",&ahffp);
  if (!ahfits::firstHDU(ahffp, ahfits::e_BINARY_TBL)) {
    AH_THROW_RUNTIME("No binary table found in effective area file");
  }
 
  ahfits::close(ahffp);
  ahfits::open(eafile,"",&ahffp);

  // Number of energy & effective area points = number of rows
  numeaens = ahfits::getKeyValLLong(ahffp,"NAXIS2");
  eunit = ahfits::getKeyValStr(ahffp,"TUNIT1");
 
  // Setup router and connect local variables to columns 
  ahfits::Router router(ahffp);
 
  router.connectScalar(ahfits::e_READONLY,"ENERGY",l_ea_ecenter);
  router.connectScalar(ahfits::e_READONLY,"EFFAREA",l_effarea);

  // Read and store the ENERGY column as keV (convert if unit is not already 
  // keV) and the EFFAREA column
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp);
    if (eunit == "eV") {
      ea_ecenter.push_back(l_ea_ecenter * eVtokeV);
      effarea.push_back(l_effarea * eVtokeV);
    } else if (eunit == "keV") {
      ea_ecenter.push_back(l_ea_ecenter);
      effarea.push_back(l_effarea);
    } else { 
      AH_THROW_RUNTIME("energy unit in effective area file is not eV or keV -Aborting");
    }  
  }

  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void writearffile(std::string & outfile, bool & kevunits, 
                  arfkeywords & arfkeywords, long & numeoutbins, 
                  std::vector<double> & eoutlo, std::vector<double> & eouthi,
                  std::vector<double> & outputarfarray) {

  ahfits::FilePtr ahffp = 0;     // Ahfits file pointer to output arf file 
  double l_eoutlo = 0.;          // Local container for ENERG_LO column
  double l_eouthi = 0.;          // Local container for ENERG_HI column
  double l_outputarfarray = 0.;  // Local container for SPECRESP column
  
  // Create outputfile
  ahfits::create(outfile,"",&ahffp);
  
  // Create HDU 1
  ahfits::addEmptyTbl(ahffp,"SPECRESP");

  // Create output columns
  ahfits::insertColAfter(ahffp, "ENERG_LO","E", "");
  ahfits::insertColAfter(ahffp, "ENERG_HI","E", "");
  ahfits::insertColAfter(ahffp, "SPECRESP","E", "");
  
  // Set the units of each each output column 
  ahfits::setTUnit(ahffp,"SPECRESP","cm^2");
  if (kevunits) {
    ahfits::setTUnit(ahffp,"ENERG_LO","keV");
    ahfits::setTUnit(ahffp,"ENERG_HI","keV");
  } else {
    ahfits::setTUnit(ahffp,"ENERG_LO","eV");
    ahfits::setTUnit(ahffp,"ENERG_HI","eV");
  }

  // Write header keywords
  ahfits::writeKeyValLLong(ahffp,"NAXIS2",numeoutbins,"");
  ahfits::writeKeyValStr(ahffp,"TELESCOP",arfkeywords.m_telescop,"");
  ahfits::writeKeyValStr(ahffp,"INSTRUME",arfkeywords.m_instrume,"");
  ahfits::writeKeyValStr(ahffp,"DETNAM",arfkeywords.m_detnam,"");
  ahfits::writeKeyValDbl(ahffp,"GEOMAREA",arfkeywords.m_geomarea,"");
  ahfits::writeKeyValDbl(ahffp,"OBSTIME",arfkeywords.m_obstime,"");
  ahfits::writeKeyValStr(ahffp,"EXMAPNAM",arfkeywords.m_emapfile,"");
  ahfits::writeKeyValStr(ahffp,"RTEVFILE",arfkeywords.m_xrtevtfile,"");
  ahfits::writeKeyValStr(ahffp,"ARFVERSN","1992a","");
  ahfits::writeKeyValStr(ahffp,"HDUCLASS","OGIP","");
  ahfits::writeKeyValStr(ahffp,"HDUCLAS1","RESPONSE","");
  ahfits::writeKeyValStr(ahffp,"HDUVERS1","1.0.0","");
  ahfits::writeKeyValStr(ahffp,"HDUCLAS2","SPECRESP","");
  ahfits::writeKeyValStr(ahffp,"HDUVERS2","1.1.0","");

  // Setup router and connect local variables 
  ahfits::Router router(ahffp);
  router.connectScalar(ahfits::e_WRITEONLY,"ENERG_LO",l_eoutlo);
  router.connectScalar(ahfits::e_WRITEONLY,"ENERG_HI",l_eouthi);
  router.connectScalar(ahfits::e_WRITEONLY,"SPECRESP",l_outputarfarray);

  // Write data to columns
  for (long ii=0; ii<numeoutbins; ++ii) { 
    l_eoutlo = eoutlo[ii];
    l_eouthi = eouthi[ii];
    l_outputarfarray = outputarfarray[ii];
    ahfits::lastRow(ahffp);
    ahfits::nextRow(ahffp);
    ahfits::writeRow(ahffp);
  } 
  
  // Close output arf file
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void setupregionstruct(long & numregions, 
                       std::vector<std::string> & regionfilenames, 
                       std::vector<SAORegion*> & xrtregions) {
  int status = 0;     // Status code for fits_read_ascii_region routine
  SAORegion* singleregion = 0;  //  Single SAORegion struct
  WCSdata* WCS = 0;  // World Coordinate system data for region



  // Read each xrtregion and append to vector xrtregions
  for (int ii=0; ii<numregions; ++ii) {
    if (!ahgen::fileExists(regionfilenames[ii])) {
      std::string regfile_err = "The region file " + regionfilenames[ii] + " does not exist.";
      AH_THROW_RUNTIME(regfile_err);
    }
    status =  fits_read_ascii_region(regionfilenames[ii].c_str(), WCS, 
                                     &singleregion, &status);
    if (status != 0) {
      delete [] singleregion;
      arfgenlib::cleanupRegionStruct(xrtregions);
      AH_OUT << "There is problem with your region file check that regmode is appropriate for DET or SKY file: " 
             << regionfilenames[ii] << std::endl;
      AH_DEBUG << "status = " << status << std::endl;     
      AH_THROW_RUNTIME("Aborting...");
    }
    xrtregions.push_back(singleregion);
  }
}

// -----------------------------------------------------------------------------

void cleanupRegionStruct(std::vector<SAORegion*> & xrtregions) {
  
  // Convert size of xrtregion into regular int
  int length = xrtregions.size();
  for (int ii=0; ii<length; ++ii) {
    delete [] xrtregions[ii]; 
  }
}

// -----------------------------------------------------------------------------

int InRegion(const char* file, double detx, double dety) {
  
  int status = 0;     
  SAORegion* region = 0; 
  WCSdata* WCS = 0;  
  
  fits_read_ascii_region(file, WCS, &region, &status);
  
  if (status != 0) {
    std::cout << "status = " <<  status << std::endl;
    delete [] region;
    AH_THROW_RUNTIME("Failed reading region file Aborting");
  }

  return fits_in_region(detx,dety,region);
}

// -------------------------------------------------------------------------------

bool ChkDblRefl(std::string & pathcode) {

  

  const int indivPathCodeSize = 4;   // The size of each individual path code 
  std::string PrimRefl = "2062";     // Front-side primary reflection
  std::string PrimReflScat = "3062"; // Front-side primary reflection 
                                     // followed by scattering
  std::string SecRefl = "2072";      // Front-side secondary reflection
  std::string SecReflScat = "3072";  // Front-side secondary reflection 
                                     // followed by scattering
  std::string FPcode = "1011";       // Code for focal-plane impact  
  std::vector<std::string> currPathCodes;  // Vector containing each 4 character
                                           // pathcode
  int numPrimRefl = 0;               // Number of primary reflection events
  int numSecRefl = 0;                // Number of secondary reflection events
  bool FocalPlaneHit = false;        // Boolean for focal plane hit
  int numPathCodeChars = 0;          // The total number of pathcode characters
  int numInteractions = 0;           // The number of 4 character pathcodes

  // Break up the pathcode string into individual pieces of 4
  
  int pathlength = pathcode.length();
  for (int ii=0; ii+indivPathCodeSize<=pathlength; ii=ii+indivPathCodeSize) {
  
    currPathCodes.push_back(pathcode.substr(ii,indivPathCodeSize));
  }
  

  // Determine the number of the 4 character pathcodes in the pathcode string
  numPathCodeChars = (int)pathcode.size();
  numInteractions = numPathCodeChars / indivPathCodeSize;
  

  // Loop through the pathcodes, counting primary and secondary reflections
  
  for (int iCode = 0 ; iCode < numInteractions ; ++iCode ) {
    // Search for front reflections (can have scattering)
    if ( (currPathCodes[iCode] == PrimRefl) ||
       (currPathCodes[iCode] == PrimReflScat) ) {
      numPrimRefl++;
    }
    // Seach for reflections on a secondary mirror
    if ( (currPathCodes[iCode] == SecRefl) ||
         (currPathCodes[iCode] == SecReflScat)  ) {
      numSecRefl++;
    }
    if (currPathCodes[iCode] == FPcode) {
      FocalPlaneHit = true;
    }
  } // End-loop through pathcode

  // If there was exactly one primary reflection and one secondary reflection 
  // then there was a double-reflection in the path.
  if ( (numPrimRefl == 1) && (numSecRefl == 1) && (FocalPlaneHit == true)) {
    return true;
  } else {
    return false;
  }

  
  
}

// -----------------------------------------------------------------------------

void readtrans(std::string & transmissionfile, int extnumber, 
               long & numenergies, std::vector<double> & energies, 
               std::vector<double> & transmission) {

  ahfits::FilePtr ahffp = 0;   // Ahfit file pointer to transmission file
  std::string eunit;           // Energy unit (keV/eV)
  std::string col2_name;       // Name of the second column in 
                               // transmissionfile[extnumber]
  double l_energies = 0.;      // Local container for energy column
  double l_transmission = 0.;  // Local container for transmission column
  
  // Open transmmission file
  ahfits::open(transmissionfile,"",&ahffp);

  // Move to extnumber
  ahfits::move(ahffp, extnumber);

  // Read the header keywords
  numenergies = ahfits::getKeyValLLong(ahffp,"NAXIS2");
  eunit = ahfits::getKeyValStr(ahffp,"TUNIT1");
  col2_name = ahfits::getKeyValStr(ahffp,"TTYPE2");

  // Setup router and connect to columns ENERGY and col2_name
  ahfits::Router router(ahffp);
  router.connectScalar(ahfits::e_READONLY,"ENERGY",l_energies);
  router.connectScalar(ahfits::e_READONLY,col2_name,l_transmission);

  // Read and store the transmission and energy column (convert to keV if neccesary)
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp);
    transmission.push_back(l_transmission);
    if (eunit == "eV") {
      energies.push_back(l_energies*eVtokeV);
    } else if (eunit == "keV") {
      energies.push_back(l_energies);
    } else {
      AH_THROW_RUNTIME("energy units in transmission file are not eV or keV -Aborting"); 
    }
  }

  // Close transmission file
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void readtype2trans(std::string & transmissionfile,long& nx1, long& ny1, 
                    std::vector<double> & gx, std::vector<double> & gy,
                    double & deltgx, double & deltgy,
                    std::vector< std::vector<double> > & geoimage, long & nx2,
                    long & ny2, std::vector< std::vector<double> > & fracimage
                    , long & ne1, std::vector<double> & ens1, 
                    std::vector<double> & trans1, long & ne2,
                    std::vector<double> & ens2, std::vector<double> & trans2) {
 
  ahfits::FilePtr ahffp = 0;    // Ahfits file poitner to transmission file 
  std::string eunit1;           // Energy unit (keV/eV) extension 1
  std::string eunit2;           // Energy unit (keV/eV) extension 2
  double crpix1 = 0.;           // Keyword CRPIX1 x axis reference pixel
  double crval1 = 0.;           // Keyword CRVAL1 x coordinate reference pixel 
  double cdelt1 = 0.;           // Keyword CDELT1 x-axis increment
  double crpix2 = 0.;           // Keyword CRPIX2 y axis reference pixel
  double crval2 = 0.;           // Keyword CRVAL2 y coordinate reference pixel
  double cdelt2 = 0.;           // Keyword CDELT2 y-axis increment

  double l_ens1 = 0.;           // Local container for energy extension 1
  double l_trans1 = 0.;         // Local container for transmission extension 1
  double l_ens2 = 0.;           // Local container for energy extension 2
  double l_trans2 = 0.;         // Local container for transmission extension 2
  
  long numrows = 0;             // Number of rows extension 3
  std::vector<int> pxnumber;    // Vector of pixel numbers
  int l_pxnumber = 0;           // Local container for column PIXELID
  std::vector<int> pixelx;      // Vector of pixel x coordinates
  int l_pixelx = 0;             // Local container for column PXIELX
  std::vector<int> pixely;      // Vector of pixel y coordinates
  int l_pixely = 0;             // Local conatiner for column PIXELY
  std::vector<double> coltransmission; // Vector of transmission factor
  double l_coltransmission;     // Local container for column transmission
  std::vector< std::vector<long> > pixelindex;  // Index number for pixel 
                                                // coordinate combination 
                                                // (i.e. one pixel)
  double totfracimage = 0.;    // Total image tranmission fraction 
  int idx = 0;                 // individual index for pixelindex
 
  // Open transmission file
  ahfits::open(transmissionfile,"",&ahffp);

  // Move to primary extension
  ahfits::move(ahffp,1);
 
  // Read header keywords
  nx1 = ahfits::getKeyValLLong(ahffp,"NAXIS1");
  ny1 = ahfits::getKeyValLLong(ahffp,"NAXIS2");
  crpix1 = ahfits::getKeyValDbl(ahffp,"CRPIX1");
  crval1 = ahfits::getKeyValDbl(ahffp,"CRVAL1");
  cdelt1 = ahfits::getKeyValDbl(ahffp,"CDELT1");
  crpix2 = ahfits::getKeyValDbl(ahffp,"CRPIX2");
  crval2 = ahfits::getKeyValDbl(ahffp,"CRVAL2");  
  cdelt2 = ahfits::getKeyValDbl(ahffp,"CDELT2");

  // Read Image into geoimage
  ahfits::readImage(ahffp,geoimage,nx1,ny1);
  
  // Set delta gx and gy
  deltgx = cdelt1;
  deltgy = cdelt2;

  // Set up geoimage array
  for (int ii=0; ii<nx1; ++ii) { 
    gx.push_back(crval1 + ((double)ii-crpix1)*cdelt1);
  }
  for (int ii=0; ii<ny1; ++ii) {
    gy.push_back(crval2 + ((double)ii-crpix2)*cdelt2);
  }

  // Move to extension one
  ahfits::move(ahffp,"TRANSMISSION");
  
  // Read header keywords
  ne1 = ahfits::getKeyValLLong(ahffp,"NAXIS2");
  eunit1 = ahfits::getKeyValStr(ahffp,"TUNIT1");
  
  // Setup router and connect Local veribles to columns extension one
  ahfits::Router router(ahffp);
  router.connectScalar(ahfits::e_READONLY,"ENERGY",l_ens1);
  router.connectScalar(ahfits::e_READONLY,"TRANSMISSION",l_trans1);

  // Read energy and transmission columns in extension one converting to keV if 
  // nessecary
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp))
  {
    ahfits::readRow(ahffp);
    if (eunit1 == "eV") {
      ens1.push_back(l_ens1*eVtokeV);
    } else {
      ens1.push_back(l_ens1);
    } 
    trans1.push_back(l_trans1);
  }

  // Clear connections to router
  router.clearConnections();

  // Move to extension 2
  ahfits::move(ahffp,"SURFACETRANSMISSION");
  ne2 = ahfits::getKeyValLLong(ahffp,"NAXIS2");
  eunit2 = ahfits::getKeyValStr(ahffp,"TUNIT1");
  router.connectScalar(ahfits::e_READONLY,"ENERGY",l_ens2);
  router.connectScalar(ahfits::e_READONLY,"TRANSMISSION",l_trans2);

  // Read energy and transmission columns in extension two converting to keV if 
  // nessecary
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp);
    if (eunit2 == "eV") {
      ens2.push_back(l_ens2*eVtokeV);
    } else {
      ens2.push_back(l_ens2);
    }
    trans2.push_back(l_trans2);
  }

  // Clear connections to router
  router.clearConnections();
  
  // Move to extension 3
  ahfits::move(ahffp,"PIXELRATIOMAP");
 
  // Obtain the number of rows
  numrows = ahfits::getKeyValLLong(ahffp,"NAXIS2");
 
  // Initialize 2-D vectors
  std::vector<long> row_pixelindex;
  std::vector<double> row_fracimage;
  row_pixelindex.resize(ny2,0);
  row_fracimage.resize(ny2,0.0);
  for (int jj=0; jj<nx2; ++jj){
    pixelindex.push_back(row_pixelindex);
    fracimage.push_back(row_fracimage);
  }

  // Connect local variables to columns in extension #3
  router.connectScalar(ahfits::e_READONLY,"PIXELID",l_pxnumber);
  router.connectScalar(ahfits::e_READONLY,"PIXELX",l_pixelx);
  router.connectScalar(ahfits::e_READONLY,"PIXELY",l_pixely);
  router.connectScalar(ahfits::e_READONLY,"PIXELRATIOS",l_coltransmission);
  
  // Store columns PIXEL, PIXELX, PIXELY and PIXELRATIOS into vectors pxnumber,
  // pixelx, pixely and coltransmission
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp);
    pxnumber.push_back(l_pxnumber);
    pixelx.push_back(l_pixelx);
    pixely.push_back(l_pixely);
    coltransmission.push_back(l_coltransmission);
    pixelindex[pixelx[idx]][pixely[idx]] = idx;
    fracimage[pixelx[idx]][pixely[idx]] = coltransmission[idx];
    totfracimage=totfracimage+coltransmission[idx];
    ++idx;
  } // End loop over rows
  
  // Normalize pixel fractions so that we account for the fact that the 
  // transmission array in extension 3 is averaged over all pixels
  for (int ii=0;ii<numrows;++ii) { 
    fracimage[pixelx[ii]][pixely[ii]] = fracimage[pixelx[ii]][pixely[ii]] * 
                                        (double)numrows / totfracimage;
  } // End loop over rows

  // Close the transmission file
  ahfits::close(ahffp);
 

} // End readtranstype2

//------------------------------------------------------------------------------


void sxiefficiencies(double obstime, std::string qefile, 
                     std::string contamifile, long numdetx, long numdety, 
                     std::vector<double> detxlo, std::vector<double> detxhi, 
                     std::vector<double> detylo, std::vector<double> detyhi, 
                     long numebinsfine, std::vector<double> efinecen, 
                     long & numqefunc, std::vector< std::vector<long> > & 
                     qetransindex, std::vector< std::vector<double> > & 
                     qecbftrans, long & numcontamfunc, 
                     std::vector< std::vector<double> > & qetrans,
                     std::vector< std::vector<long> > & contamtransindex,
                     std::vector< std::vector<double> > & contamtrans,
                     int & trantype, std::vector<double> & abund,
                     std::vector<double> & cols, std::vector<double> & covfac) {

  ahfits::FilePtr ahffp=0;                // Ahfits file pointer to 
                                          // Blocking filter file
  long numqeenergies = 0;                 // Number energies in QE file
  std::vector<double> qe_energies;        // Vector of energy points inf QE file
  std::vector< std::vector<double> > qefiletrans;  // QE transimission fraction
  std::vector<double> qefiletrans_row;    // Individual row of qefiletrans
  std::vector<double> qecbftrans_row;     // Individual row of qecbftrans 
  double outqe = 0.0;                     // Interpolated qe transmission
  double outcbf = 0.0;                    // Iterpolated cbf transmission
  std::vector<double> cbftrans;           // Vector cbf transmission
  std::vector<double> cbf_energies;       // Vector of cbf energies
  std::vector<double> cbffiletrans;       // Vector of contamination blocking 
                                          // filter transmission
  long numcontamenergies;                 // The number of contamination 
                                          // energies 
  std::vector<double> contam_energies;    // Vector of contamination energies
  std::vector< std::vector<double> > contamfiletrans; // Contamination file 
                                                      // transmission
  std::vector<double> contamtrans_row;     // Individual row of contamtrans
  std::vector<double> contamfiletrans_row; // Individual row of 
                                           // contamfiletrans
  double outcontamtrans = 0.0;             // Output contamination transmission
  std::vector< std::vector<double> > qefunctions;  // Net transmission due to 
                                                   // contamination, as a 
                                                   // function of energy and 
                                                   // unique function 
  std::vector< std::vector<long> > qefunctionsindex;  // Pointer to unique 
                                                      // transmission value for
                                                      // each xy pixel 
                                                      // coordinate  
  long numcbfenergies = 0;                      // number of cbf energies

  

  // Check if no QE file was specified, in which case create dummy areas 
  // with unit transmission
  if (ahgen::strtoupper(qefile) == "NONE") {
    numqefunc = 1;
    qecbftrans_row.resize(numebinsfine,1.0);
    for (int kk=0; kk<numqefunc; ++kk) { 
      qecbftrans.push_back(qecbftrans_row);
    }
  } else {  //Process QE file if there was one
    
    // Interpolate each unqiue QE curve onto the common energy grid, collecting 
    // all the unique QE vs. energy curves into a single 2D array.  Populate 
    // the associated 2D image index array that will map a pair of (detx, dety)
    // values to an index pointing to the correct QE vs. E curve.  Multiply the
    // fine-grid QE by the fine-grid (position-independent) CBF transmission. 

    // Call routine to read QE data organized into unique functions of 
    // transmission versus energy
    arfgenlib::getsxiqefunctions(qefile,detxlo,detxhi,detylo,detyhi, 
                                 numdetx,numdety,numqeenergies,qe_energies, 
                                 numqefunc,qetransindex,qefiletrans, 
                                 numcbfenergies,cbf_energies,cbffiletrans);
   
    // Intializing 2-D vector qecbftrans
    std::vector<double> qecbftrans_row;
    qecbftrans_row.resize(numqefunc,0.0);
    for (int ii = 0; ii<numebinsfine; ++ii) {
      qecbftrans.push_back(qecbftrans_row);
    }
  
    // Initializing 2-D vector qetrans
    std::vector<double> qetrans_row;
    qetrans_row.resize(numqefunc,0.0);
    for (int ii=0; ii<numebinsfine; ++ii) {
      qetrans.push_back(qetrans_row);
    }
        
    // Interpolate the “native” QE value onto the fine-bin common energy grid 
    // for each unique function
    for (int jj=0; jj<numqefunc; ++jj) {

      //storing each element of qefiletrans into qefiletrans_row
      qefiletrans_row = qefiletrans[jj];

      for (int ii=0;ii<numebinsfine;++ii) {
        if ((efinecen[ii] >= qe_energies[0]) &&
            (efinecen[ii] <= qe_energies[numqeenergies-1])) {
          bisectionInterp(numqeenergies,qe_energies,qefiletrans_row,
                          efinecen[ii],outqe);  
          qetrans[ii][jj] = outqe;
        } // end check to see if efinecen is within qe_energies  
      }// End of ii for-loop over QE fine-energy grid points
    }// End of jj for-loop over unique QE function index 
     
    // The CBF transmission is also to be put on the same fine energy grid and 
    // combined with the OBL transmission, but it has a different original 
    // energy grid so the interpolation has to be done separately before 
    // combining.

    // The following will contain the interpolated CBF transmission
    cbftrans.resize(numebinsfine,0.0);
    for (int ii=0; ii<numebinsfine; ++ii) {
      if ((efinecen[ii] >= cbf_energies[0]) &&
          (efinecen[ii] <= cbf_energies[numcbfenergies-1])) {      
        bisectionInterp(numcbfenergies,cbf_energies,cbffiletrans,
                        efinecen[ii],outcbf);
        cbftrans[ii]=outcbf;
        for (int jj=0; jj<numqefunc; ++jj) {
          qecbftrans[ii][jj] = qetrans[ii][jj]*cbftrans[ii];
        }
      } // end if efinecen is within cbf_energies grid
    }
    ahfits::close(ahffp);
  }// End of if-block checking if no QE file was specified for input
  
  // Now do the SXI contamination treatment
  if (ahgen::strtoupper(contamifile) == "NONE") {
    // If no contamination file was specified, set up dummy array 
    // for unit transmission factor
    // initialize contamtransindex
    std::vector<long> contamtransindex_row;
    contamtransindex_row.resize(numdety,0.0);
    for (int ii=0; ii<numdetx; ++ii) {
      contamtransindex.push_back(contamtransindex_row);
    }   

    numcontamfunc = 1; 

    // Intialize contamtrans to 1
    contamtrans_row.resize(numcontamfunc,1.0);
    for (int kk=0;kk<numebinsfine;++kk) {
      contamtrans.push_back(contamtrans_row);
    }

  } else { // Process the contamination file if one was specified

    arfgenlib::getsxcontamifunctions(contamifile,detxlo,detxhi,detylo,detyhi, 
                                     numdetx,numdety,obstime,numcontamenergies, 
                                     contam_energies,numcontamfunc,
                                     contamtransindex,contamfiletrans,abund,
                                     cols,covfac);
  
    // Intialize contamtrans to 0
    contamtrans_row.resize(numcontamfunc,0.0);
    for (int kk=0;kk<numebinsfine;++kk) {
      contamtrans.push_back(contamtrans_row);
    }

    // Initialize contamfiletrans row
    contamfiletrans_row.resize(numcontamenergies, 0.0); 

    // Interpolate the "native" contamination value onto the fine-bin 
    // Common energy grid
    for (int jj=0; jj<numcontamfunc; ++jj) {
      // Obtain individual row of contamfiletrans
      for (int kk=0; kk<numcontamenergies; ++kk) {
        contamfiletrans_row[kk] = contamfiletrans[kk][jj];
      }
      for (int ii=0; ii<numebinsfine; ++ii) {
        if ((efinecen[ii] >= contam_energies[0]) &&
            (efinecen[ii] <= contam_energies[numcontamenergies-1])) {
          bisectionInterp(numcontamenergies,contam_energies,contamfiletrans_row,
                          efinecen[ii],outcontamtrans); 
          contamtrans[ii][jj] = outcontamtrans;
        } // End if efinecen is within contam_energies
      }// End of ii for-loop over contamination transmission fine-energy grid
    }// End of jj for-loop over unique contamination transmission function index
  } // End of if-block checking if a contamination file was specified
    
}

//------------------------------------------------------------------------------

void sxsefficiencies(double obstime, std::string qefile, std::string obfile,
                     std::string fwfile, std::string contamifile, 
                     bool dogatevalve, std::string gatevalvefile, 
                     std::string filterwheel, long numdetx, long numdety,  
                     std::vector<double> detxlo, std::vector<double> detxhi, 
                     std::vector<double> detylo, std::vector<double> detyhi, 
                     long numebinsfine, std::vector<double> efinecen, 
                     long & numqefunc, long & numcontamfunc,
                     std::vector< std::vector<double> > & qetrans, 
                     std::vector< std::vector<long> > & qetransindex,
                     std::vector< std::vector<double> > & contamtrans,
                     std::vector< std::vector<long> > & contamtransindex, 
                     double & filterheight, double & filterradius, 
                     std::vector< std::vector<double> > & gvfracimage, 
                     std::vector< std::vector<double> > & fwfracimage, 
                     long & fwnx, long & fwny, std::vector<double>
                     & fwx, std::vector<double> & fwy, std::vector<double> & 
                     fwxbnds, std::vector<double> & fwybnds,
                     std::vector< std::vector<double> > & fwgeoimage, 
                     int & trantype, std::vector<double> & abund,
                     std::vector<double> & cols, std::vector<double> & covfac){
 
  // Local variables
  double outtrans = 0.;
   
  long numqeenergies = 0;             // Number of energy bins for QE vs. E
  std::vector<double> qe_energies;    // Put the QE energies from QE file here
  std::vector<double>  qefiletrans;   // Read the QE data into here, with its 
                                      // "native" energy grid
  long numbfenergies = 0;             // Number of energy bins for transmission
                                      // vs. E 
  std::vector<double> bf_energies;    // Put the transmission energies from 
                                      // QE file here
  std::vector<double> bffiletrans;    // Read the transmission data into here, 
                                      // with its “native” energy grid
  long numgvbenergies = 0;            // Number energy bins Be film trans vs. E
  std::vector<double> gvb_energies;   // Vector containing gate valve energies
  std::vector<double> gatevfiletrans; // Gate valve file transmission
  long gvimgnx = 0;                   // Gate valve image x dimension
  long gvimgny = 0;                   // Gate valve image y dimension
  std::vector<double> gvimgx;         // Vector of x coordinates Gatevalve file
  std::vector<double> gvimgy;         // Vector of y coordinate Gatevalve file
  double gvimgdeltx;                  // Gate valve image delta x
  double gvimgdelty;                  // Gate vavle image delta y
  long numgatevenergies;              // number of energy bins E vs. Trans
  std::vector<double> gvbfiletrans;   // Transmission data from gatevalve file
  std::vector< std::vector<double> > gvgeomimage; // Gate valve geometric area 
                                                  // image
  std::vector<double> gatev_energies; // Be film energies
 
  
  double fwdeltx = 0.;                // Pixel widths for geometrical structure 
                                      // image in type 2 filter
  double fwdelty = 0.;                // Pixel widths for geometrical structure 
                                      // image in type 2 filter
  long numcontamenergies;             // Number of energy bins for 
                                      // contaminations v E funtions
  std::vector<double> contam_energies; // Energies from contamination file 
  std::vector< std::vector<double> > contamfiletrans;  // contamination 
                                                       // transmission curve 
                                                       // (with its "native" 
                                                       // energy grid) 
  double outcontamtrans;               // individual point for final 
                                       // interpolated contamination 
                                       // transmission 
  long numfwenergies = 0.;             // Number of energy bins for 
                                       // transmission vs. E
  std::vector<double> fw_energies;     // Transmission energies from fw file
  std::vector<double> fwfiletrans;     // Transmission data with its native 
                                       // energy grid
  long numfwgeomenergies = 0.;         // Number of energy bins for 2nd 
                                       // transmission vs. E
  std::vector<double> fwgeom_energies; // 2nd transmission energies from 
                                       // fw file
  std::vector<double> fwgeomfiletrans; // 2nd transmission data
    

  // Set the number of quantum efficiency functions to one 
  numqefunc = 1;
  
  // Put the QE data here, interpolated onto the common, fine-energy grid. 
  // This will also contain the product of all of the filter transmissions 
  // (i.e. it will be the net transmission). 
  std::vector<double> qetrans_row;
  qetrans_row.resize(numqefunc,0.0);
  for (int ii=0; ii<numebinsfine; ++ii) {
    qetrans.push_back(qetrans_row);
  }

  // Index for net transmission, currently a dummy placeholder because there 
  // is no position dependence. Make sure array is initialized to zero.
  std::vector<long> qetransindex_row;
  qetransindex_row.resize(numdety,0);
  for (int ii=0; ii<numdetx; ++ii) {
    qetransindex.push_back(qetransindex_row);
  }
 
  if (ahgen::strtoupper(qefile) == "NONE") {
    // If no QE file was specified then set up dummy transmission factor of 
    // unity. Note that if the gatevalve is “ON” then the QE file MUST be 
    // specified since it contains the transmission for the Be film.
    for (int kk=0;kk<numebinsfine;++kk) {
      qetrans[kk][0] = 1.0;
    }
  } else { //Read QE file
    arfgenlib::readtrans(qefile, 2, numqeenergies, qe_energies, qefiletrans);
    
    double outtrans = 0.; //temporary stroage variable
    
    // Interpolate the QE array onto the fine energy-bin grid
    for (int ii=0; ii<numebinsfine; ++ii) {
      if ((efinecen[ii] >= qe_energies[0]) && 
          (efinecen[ii] <= qe_energies[numqeenergies-1])) {
        bisectionInterp(numqeenergies,qe_energies,qefiletrans, 
                        efinecen[ii], outtrans);
        qetrans[ii][0] = outtrans;
      }
    }
  }// End of if-block checking if a QE file was specified for input
  
  // BLOCKING FILTER transmission 
  if (ahgen::strtoupper(obfile) != "NONE") { 
    // Read optical blocking filter transmission
    arfgenlib::readtrans(obfile,2,numbfenergies,bf_energies,bffiletrans);
    // Interpolate the transmission array onto the fine energy-bin grid 
    for (int ii=0; ii<numebinsfine; ++ii) {
      if ((efinecen[ii] >= bf_energies[0]) && 
          (efinecen[ii] <= bf_energies[numbfenergies-1])) {
        bisectionInterp(numbfenergies,bf_energies,bffiletrans,efinecen[ii]
                        ,outtrans);
        qetrans[ii][0] = qetrans[ii][0]*outtrans;
      }
    }
  }// End of if-block checking if obffile!=NONE
  
  // FILTERWHEEL transmission (if OPEN  or fwfile=NONE do nothing)
  if ((ahgen::strtoupper(filterwheel.substr(0,4)) != "OPEN") && 
      (ahgen::strtoupper(fwfile) != "NONE")) {
        
    // Read filter wheel file; there are two types of file: simple transmission
    // (TRANTYPE=1) which has 1 extension, and complex transmission 
    // (TRANTYPE=2) which has 3 extensions and a primary image, and 2 of the 
    // extensions contain a transmission function (these will be combined).
    
    // (Note that the FW file specified in the input .par file should already 
    // have been checked against the keyword “FILTER” in the exposure map file 
    // to ensure that the correct file is being used for the appropriate filter
    // setting.)

    // Read keywords from filter file for filter type, file type, height and 
    // radius of the filter
    ahfits::FilePtr ahffp_fw;
    ahfits::open(fwfile,"",&ahffp_fw);
  
    // Move to extension 1   
    ahfits::move(ahffp_fw,2);
    
    // Note: check that this is the same value for the same keyword in the 
    // exposure map file, stop if it is not.

    int trantype = ahfits::getKeyValLLong(ahffp_fw,"TRANTYPE");
    filterheight = ahfits::getKeyValDbl(ahffp_fw,"FLHEIGHT");
    filterradius = ahfits::getKeyValDbl(ahffp_fw,"FLRADIUS");
    ahfits::close(ahffp_fw);
    
    if (trantype == 1) { // If filter file type is simple
      arfgenlib::readtrans(fwfile,2,numfwenergies,fw_energies,fwfiletrans);
    } else if (trantype == 2) { // If filter file type is not simple
      arfgenlib::readtype2trans(fwfile,fwnx,fwny,fwx,fwy,fwdeltx,fwdelty,
                                fwgeoimage,numdetx,numdety,fwfracimage,
                                numfwgeomenergies,fwgeom_energies,
                                fwgeomfiletrans,numfwenergies,fw_energies,
                                fwfiletrans); 

      // Array of boundary x and y values to facilitate locating a pixel 
      // Corresponding to x,y
      std::vector<double> fwxbnds;
      std::vector<double> fwybnds;
      fwxbnds.resize(fwnx+1);
      fwybnds.resize(fwny+1);

      for (int kk=0; kk<fwnx; ++kk) {
        fwxbnds[kk]=fwx[kk];
      }

      fwxbnds[fwnx]=fwx[fwnx];
      
      for (int kk=0; kk<fwny; kk++) {
        fwybnds[kk]=fwy[kk];
      }

      fwybnds[fwny]=fwy[fwny];
    } // End of if-block checking if filter file type is simple or not
  
    // Interpolate the 1st transmission array onto the fine energy-bin grid and
    // merge with the cumulative transmission function   
    for (int ii=0; ii<numebinsfine; ++ii) { 
      if ((efinecen[ii] >= fw_energies[0]) && 
          (efinecen[ii] <= fw_energies[numfwenergies-1])) {
        bisectionInterp(numfwenergies, fw_energies,fwfiletrans, efinecen[ii],
                        outtrans);
        qetrans[ii][0] = qetrans[ii][0] * outtrans;
      }
    }

    // If TRANTYPE=2 then interpolate the 2nd  transmission array onto the fine 
    // energy-bin grid and merge with the cumulative transmission function
    if (trantype == 2) {
      for (int ii=0; ii<numebinsfine; ++ii) {
        if ((efinecen[ii] >= fwgeom_energies[0]) && 
            (efinecen[ii] <= fwgeom_energies[numfwenergies-1])) {
          bisectionInterp(numfwgeomenergies,fwgeom_energies,fwgeomfiletrans,
                          efinecen[ii],outtrans); 
          qetrans[ii][0] = qetrans[ii][0] * outtrans;
        }
      }
    } // End of if-block checking if TRANTYPE=2
  } else { 
    // TRANTYPE is an output of sxsefficiencies and a value of 0 will 
    // indicate that either there was no filter file specified, OR that 
    // the filter wheel setting was OPEN1 or OPEN2
    trantype = 0;
  } // End of if-block checking if filter wheel setting is not "OPEN" and 
    // fwfile is not NONE
  
  // If gate valve is closed read the gate valve Be film +mesh transmission, 
  // the gate valve support structure transmission, and the gate valve pixel 
  // fractions.
  if (dogatevalve == true) {
     
    // Variables in the following 3 lines are read from the 1st transmission 
    // array in the gate valve file and correspond to the  Be film transmission
    // and energy columns. We are assuming the MESH fraction is included in the
    // transmission column (should be 0.71 of the amount with no mesh).
    arfgenlib::readtype2trans(gatevalvefile,gvimgnx,gvimgny,gvimgx,gvimgy,
                              gvimgdeltx,gvimgdelty,gvgeomimage,numdetx,numdety,
                              gvfracimage,numgatevenergies,gatev_energies,
                              gatevfiletrans,numgvbenergies,gvb_energies,
                              gvbfiletrans);

    // Interpolate 1st transmission onto fine energy grid and merge with 
    // cumulatve efficiency array
    for (int ii=0;ii<numebinsfine;++ii) {
      if ((efinecen[ii] >= gvb_energies[0]) && 
          (efinecen[ii] <= gvb_energies[numgvbenergies-1])) {
        bisectionInterp(numgvbenergies,gvb_energies,gvbfiletrans,efinecen[ii],
                        outtrans);
        qetrans[ii][0] = qetrans[ii][0]*outtrans;
      }
    }
  
    // Interpolate 2nd transmission onto fine energy grid and merge with 
    // cumulative efficiency array
    for (int ii=0;ii<numebinsfine;++ii) { 
      if ((efinecen[ii] >= gatev_energies[0]) && 
          (efinecen[ii] <= gatev_energies[numgatevenergies-1])) {
        bisectionInterp(numgatevenergies,gatev_energies,gatevfiletrans,
                        efinecen[ii],outtrans);
        qetrans[ii][0] = qetrans[ii][0]*outtrans;
      }
    }
  } // End of if-block checking dogatevalve==True
 
  // Initialize contamtransindex to 0
  std::vector<long> row_contamtransindex;
  row_contamtransindex.resize(numdety,0.0);
  for (int ii=0; ii<=numdetx; ++ii){
    contamtransindex.push_back(row_contamtransindex);
  }

  if (ahgen::strtoupper(contamifile) == "NONE") {
    // If no contamination file was specified for input, set up a dummy array 
    // with unit transmission factor
    numcontamfunc = 1;
 
    // Initialize 2-D vector contamtrans to 1
    std::vector<double> contamtrans_row;
    contamtrans_row.resize(numcontamfunc,1.0);
    for (int ii=0; ii<numebinsfine; ++ii) {
      contamtrans.push_back(contamtrans_row);
    }

  } else {  // Process the contamination file

    arfgenlib::getsxcontamifunctions(contamifile,detxlo,detxhi,detylo,detyhi, 
                                     numdetx,numdety,obstime,numcontamenergies,
                                     contam_energies,numcontamfunc,
                                     contamtransindex,contamfiletrans,abund,
                                     cols,covfac);

    // Intialize contamtrans to 0 
    std::vector<double> contamtrans_row;
    contamtrans_row.resize(numcontamfunc,0.0);
    for (int kk=0;kk<numebinsfine;++kk) {
      contamtrans.push_back(contamtrans_row);
    }  
    
    // Interpolate the contamination transmission curve onto the 
    // common energy grid
    std::vector<double> contamfiletrans_row;
    contamfiletrans_row.resize(numcontamenergies, 0.0);
    for (int jj=0; jj<numcontamfunc; ++jj) {
      for (int kk=0; kk<numcontamenergies; ++kk) {
        contamfiletrans_row[kk] = contamfiletrans[kk][jj];
      }
      for (int ii=0; ii<numebinsfine; ++ii) {
        // Interpolate the "native" contamination value onto the fine-bin 
        // common energy grid
        bisectionInterp(numcontamenergies, contam_energies,
                        contamfiletrans_row, efinecen[ii], outcontamtrans);
        contamtrans[ii][jj] = outcontamtrans;
      } // End of ii for-loop over contamination transmission fine-energy grid
    } // End of loop over the number of contamination regions
  } // End of if-block checking if a contamination file was specified for input
}


//------------------------------------------------------------------------------

void getsxiqefunctions(std::string qefile, std::vector<double> detxlo, 
                       std::vector<double> detxhi, std::vector<double> detylo, 
                       std::vector<double> detyhi,  long numdetx, long numdety,
                       long & numenergies, std::vector<double> & energies, 
                       long & numregions, 
                       std::vector< std::vector<long> > & qefunctionsindex,
                       std::vector< std::vector<double> > & qefunctions,
                       long & numcbfenergies, 
                       std::vector<double> & cbf_energies, 
                       std::vector<double> & cbffiletrans) {
 
  // Ahfits file pointers
  ahfits::FilePtr ahffp_qe = 0;

  const int numvertices = 4;             // Number of verticies in qe image  
  std::vector< CartesianCoord > vertices; // Vector containing the coordinates 
                                          // of each vertex of qe image polygon
  CartesianCoord currVertex;              // Individual vertex
  std::vector< std::vector<double> > xcornercoords;  // Vector stores 
                                                     // CORNER_DETX column
  std::vector< std::vector<double> > ycornercoords;  // Vector stores 
                                                     // CORNER_DETY column
  std::vector<double> xrow;                    // Local container for 
                                               // each row of CORNER_DETX 
  std::vector<double> yrow;                    // Local container for 
                                               // each row of CORNER_DETY
  std::vector<double> detxcen;                 // Center of detxlo/hi
  std::vector<double> detycen;                 // Center of detylo/hi
  CartesianCoord pointcoords;                  // Indivdual point 
                                               // detxcen,detycen,0  
  bool pointIsInsidePolygon = false;           // Bool for point being inside 
                                               // polygon
  std::vector<double> vertsX;                  // List of x coordinates
  std::vector<double> vertsY;                  // List of y coordinates
  std::string eunits;                          // Energy unit
  double l_cbf_energies = 0.;                  // Local container for ENERGY 
                                               // columns values ext. 3
  double l_cbffiletrans = 0.;                  // Local container for 
                                               // TRANSMISSION column
  long xminregion = 0;                         // Min pixel coordinate x
  long xmaxregion = 0;                         // Max pixel coordinate x 
  long yminregion = 0;                         // Min pixel coordinate y
  long ymaxregion = 0;                         // Max pixel coordinate y
  long idx = 0;                                // Index qe file

  // Resize xrow and yrow for each row of qefile
  xrow.resize(4,0.0);
  yrow.resize(4,0.0);

  // Open qe file
  ahfits::open(qefile,"",&ahffp_qe);

  // Move to extension 1
  ahfits::move(ahffp_qe, 2);
  
  numregions = ahfits::getKeyValLLong(ahffp_qe,"NAXIS2");
  
  // Establish how many energies there are 
  numenergies = ahfits::columnRepeat(ahffp_qe,"ENERGY");
  
  // Initialize vector containers
  energies.resize(numenergies,0.0);
 
  std::vector<double> qefunctions_row;
  qefunctions_row.resize(numenergies,0.0);
  

  // Setup router and connect to file
  ahfits::Router router(ahffp_qe);
  router.connectFixedLengthArray(ahfits::e_READONLY,"ENERGY",&energies[0]);

  // Read energy from the first row of the qe file 
  ahfits::firstRow(ahffp_qe);
  ahfits::readRow(ahffp_qe);
 
  // Clear connections to router
  router.clearConnections();

  // Connect xrow and yrow to CORNER_DETX/Y
  router.connectFixedLengthArray(ahfits::e_READONLY,"QE",&qefunctions_row[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"CORNER_DETX",&xrow[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"CORNER_DETY",&yrow[0]);  

  for (ahfits::firstRow(ahffp_qe); ahfits::readOK(ahffp_qe); 
       ahfits::nextRow(ahffp_qe)) {
    ++idx;
    // Read in CORNER_DETX->xrow and CORNER_DETY->yrow
    ahfits::readRow(ahffp_qe);
    // Store all values in x/ycornercoords
    qefunctions.push_back(qefunctions_row);
    xcornercoords.push_back(xrow);
    ycornercoords.push_back(yrow);
  } // End of loop over rows in extension 1
  
  // Convert energies to keV if necessary
  eunits = ahfits::getKeyValStr(ahffp_qe,"TUNIT1");
  if (eunits == "eV") {
    for (int jj=0;jj<numenergies;++jj) {
      energies[jj] = energies[jj]/1000.0;
    }
  }
  
  // Initialize detxcen and detycen
  detxcen.resize(numdetx,0.0);
  detycen.resize(numdety,0.0);

  // Now calculate the center coordingate of x and y pixels
  for (int ii=0; ii<numdetx; ++ii) {
    detxcen[ii] = 0.5 * (detxlo[ii] + detxhi[ii]);
  } 
 for (int ii=0; ii<numdety; ++ii) {
    detycen[ii] = 0.5 * (detylo[ii] + detyhi[ii]);
  } 

  // Clear connections to router
  router.clearConnections();

  // qefunctionsindex will hold the region id for a given x,y pixel
  // initialize the qefunctionsindex with the value -1 because any 
  // pixel that is not covered by the QE region data will be left 
  // with an index of -1 and this will indicate to downstream software 
  // that the pixel is not a valid one for data collection.
  std::vector<long> qefunctionsindex_row;
  qefunctionsindex_row.resize(numdety,-1.0); 
  for (int ii=0; ii<numdetx; ++ii) { 
    qefunctionsindex.push_back(qefunctionsindex_row);
  }
  
  // Calculate the x and y pixel ranges for each region and fill the 
  // qefunctionsindex array accordingly

  int cnt = 0;
  
  for (int ii=0; ii<numregions; ++ii) {
    // Empty verticies matrix should only hole one shape
    vertices.clear();
    for (int jj=0; jj<numvertices; ++jj) {
      currVertex.m_x = xcornercoords[ii][jj];
      currVertex.m_y = ycornercoords[ii][jj];
      currVertex.m_z = 0.0;
      vertices.push_back(currVertex);
    }
    // Establish which x,y pixels are in the current region and fill 
    // those pixels values with the region number index, which will 
    // serve as a pointer.
    vertsX.clear();
    vertsY.clear();
    for (int gg=0; gg<numvertices; ++gg) {
      vertsX.push_back(vertices[gg].m_x);
      vertsY.push_back(vertices[gg].m_y);
    }

    // Determine the range of points to use 
    xminregion = (long) *std::min_element(vertsX.begin(),vertsX.end());
    xmaxregion = (long) *std::max_element(vertsX.begin(),vertsX.end());
    yminregion = (long) *std::min_element(vertsY.begin(),vertsY.end());
    ymaxregion = (long) *std::max_element(vertsY.begin(),vertsY.end());
    if (xminregion < 0) {
      xminregion=0;
    }
    if (yminregion < 0) {
      yminregion=0;
    }
    if (xmaxregion > (numdetx-1)) {
      xmaxregion=numdetx-1;
    }
    if (ymaxregion > (numdety-1)) {
      ymaxregion=numdety-1;
    }

    // Determine if each of these points are inside or outside the detector area
    for (int jj=xminregion; jj<=xmaxregion; ++jj) { 
      for (int kk=yminregion; kk<=ymaxregion; ++kk) { 
        pointcoords.m_x = detxcen[jj];
        pointcoords.m_y = detycen[kk];
        pointcoords.m_z = 0.0;
        pointIsInsidePolygon = arfgenlib::isPointInsidePolygon(pointcoords,
                                                               numvertices, 
                                                               vertices);
        // If point is inside the detector area fill in qefunctions index
        if (pointIsInsidePolygon == true) { 
          qefunctionsindex[jj][kk] = ii;
          ++cnt;
        }
      }// end loop over y-pixels (k) 
    }// end loop over x-pixels (j)  
  } //end of for loop over rows (i)

  // Now obtain the CBF transmission
  
  // Move to second extension
  ahfits::move(ahffp_qe,3);
  
  // Get the number of Contamination blocking filter energies
  numcbfenergies = ahfits::getKeyValLLong(ahffp_qe,"NAXIS2");
 
  // Connect the columns to local containers
  router.connectScalar(ahfits::e_READONLY,"ENERGY",l_cbf_energies);
  router.connectScalar(ahfits::e_READONLY,"TRANSMISSION",l_cbffiletrans);

  // Read and store ENERGY and TRANSMISSION columns 
  for (ahfits::firstRow(ahffp_qe); ahfits::readOK(ahffp_qe); ahfits::nextRow(ahffp_qe)) {
    ahfits::readRow(ahffp_qe); 
    cbf_energies.push_back(l_cbf_energies);
    cbffiletrans.push_back(l_cbffiletrans);
  }
  
  // Convert to keV if neccesary
  eunits = ahfits::getKeyValStr(ahffp_qe,"TUNIT1"); 
  if (eunits == "eV") {
    for (int jj=0;jj<numcbfenergies;++jj) {
      cbf_energies[jj] = cbf_energies[jj] * eVtokeV;
    }
  }
  
  // Close QE file
  ahfits::close(ahffp_qe);

}


//------------------------------------------------------------------------------

void getsxcontamifunctions(std::string contamifile, std::vector<double> detxlo,
                           std::vector<double> detxhi, 
                           std::vector<double> detylo, 
                           std::vector<double> detyhi, long numdetx, 
                           long numdety, double & obstime, long & numenergies, 
                           std::vector<double> & energies, long & numregions, 
                           std::vector< std::vector<long> > & contamifunctionsindex, 
                           std::vector < std::vector<double> > & contamifunctions, 
                           std::vector<double> & abund, 
                           std::vector<double> & cols,
                           std::vector<double> & covfac){

  ahfits::FilePtr ahffp = 0;     // Ahfits file pointer to contamination 
                                 // function
  long totnumrows = 0;           // total number of rows ext. 1 
  int nmaterials = 0;            // total number of contaminationg materials
  std::vector <double> epochtime;// Vector containing TIME column
  std::vector <long> region_id;  // Vector containing REGION_ID column
  std::vector < std::vector<double> > coldens;  // Vector containing COLUMN 
                                                // column 
  std::vector < std::vector<double> > coveringfactor; // Vector containing 
                                                      //FACTOR column
  std::vector < std::vector<double> > transfactor;    // Vector containing  
                                                      // TRANSMISSION column
  long tloindex=0;               // Lower time limit of a time epoch
  long thiindex=0;               // Upper time limit of a time epoch
  long transwidth;               // Width of the TRANSMISSION column
  std::string eunits;            // Eenrgy Unit (keV/eV)
  double l_time = 0.;            // Local container TIME column extension 1
  long l_region_id = 0;          // Local container REGION_ID column extension 1
  std::vector<double> l_column;  // Local container COLUMN column extension 1
  std::vector<double> l_factor;  // Local container FACTOR column extension 1
  double l_energy = 0.;          // Local container ENERGY column extension 2
  std::vector<double> l_transmission; // Local container TRANSMISSION column 
                                      // ext. 2

  const int numvertices = 4;     // Number of verticies in detector area
  std::vector<CartesianCoord> vertices;  // Vector containing all verticies
  CartesianCoord pointcoords;    // Coordinates of input point
 
  long xminregion = 0;                         // Min pixel coordinate x
  long xmaxregion = 0;                         // Max pixel coordinate x 
  long yminregion = 0;                         // Min pixel coordinate y
  long ymaxregion = 0;                         // Max pixel coordinate y

  std::vector<long> regionnumber;              // Region index
  std::vector< std::vector<double> > xcornercoords; // X pixel coordinate
  std::vector< std::vector<double> > ycornercoords; // Y pixel coordinate
  std::vector<double> xcornercoords_row;            // Tempory used to 
                                                    // initialize xcornercoords
  std::vector<double> ycornercoords_row;            // Temporary used to 
                                                    // initialize ycornercoords
  std::vector<double> vertsX;                       // List of X coordinates for
                                                    // verticies
  std::vector<double> vertsY;                       // List of Y coordinates for
                                                    // verticies
  bool pointIsInsidePolygon = false;                // Point insdie detector 
                                                    // area?(y/n) 
   
  std::vector<double> detxcen;                      // Center x detector 
                                                    // coordinates
  std::vector<double> detycen;                      // Center y detector 
                                                    // coordinates
  double tottrans = 0.;                             // Total transmission
  double nh = 0.;                                   // Column density
  double pfac = 0.;                                 // Covering factor
  

  // Open contamination file and move to first extension
  ahfits::open(contamifile,"",&ahffp);
  ahfits::move(ahffp,2);
  totnumrows = ahfits::getKeyValLLong(ahffp,"NAXIS2"); 
  
  // Establish how many materials there are number of array element in with 
  // col 3 or 4
  nmaterials = ahfits::columnRepeat(ahffp,"COLUMN");
  
  // Initialize l_column and l_factor
  l_column.resize(nmaterials,0.0);
  l_factor.resize(nmaterials,0.0);
  
  // Setup router and connect local varibles to colums in the first extension
  ahfits::Router router(ahffp); 
  router.connectScalar(ahfits::e_READONLY,"TIME",l_time);
  router.connectScalar(ahfits::e_READONLY,"REGION_ID",l_region_id);
  router.connectFixedLengthArray(ahfits::e_READONLY,"COLUMN",&l_column[0]); 
  router.connectFixedLengthArray(ahfits::e_READONLY,"FACTOR",&l_factor[0]);
  
  // Store data from columns TIME, REGION_ID, COLUMN and FACTRO into 
  // vectors epochtime region_id colden and coveringfactor.
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp);
    epochtime.push_back(l_time);
    region_id.push_back(l_region_id);
    coldens.push_back(l_column);
    coveringfactor.push_back(l_factor);
  }
 
  // Establish how many rows per time epoch and which epoch should be used for 
  // the given observation time (obstime)  
  tloindex=0;
  thiindex=totnumrows-1; 
  for (int ii=1; ii<totnumrows; ++ii) {
    if (obstime > epochtime[ii]) {
      break;
    }
    if (epochtime[ii] > epochtime[ii-1]) {
      tloindex = ii;
    }
    thiindex=ii;
  }// End of loop over rows in time column
  
  // Total number of rows to use
  numregions = thiindex-tloindex+1;

  // Remove connection from fits columns to local varibles
  router.clearConnections();

  // Move to extension 2
  ahfits::move(ahffp,3);
  
  // Read header keywords
  numenergies = ahfits::getKeyValLLong(ahffp,"NAXIS2");
  transwidth = ahfits::columnRepeat(ahffp,"TRANSMISSION");
  
  // Connect columns to local variables
  l_transmission.resize(transwidth,0.0);
  router.connectScalar(ahfits::e_READONLY,"ENERGY",l_energy);
  router.connectFixedLengthArray(ahfits::e_READONLY,"TRANSMISSION",
                                 &l_transmission[0]);
  
  // Store ENEREGY and TRANSMISSION
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp);
    energies.push_back(l_energy);
    transfactor.push_back(l_transmission);
  }

  // Convert energies to keV if neccesary
  eunits = ahfits::getKeyValStr(ahffp,"TUNIT1"); 
  if (eunits == "eV") {
    for (int jj=0;jj<numenergies;++jj) {
      energies[jj] = energies[jj]/1000.0;
    }
  }

  // Go to extension 3
  router.clearConnections(); 
  ahfits::move(ahffp,4);
  
  // Allocate memory to containers x/ycornercoords
  xcornercoords_row.resize(4,0.0);
  ycornercoords_row.resize(4,0.0);
  
  // Connect local variables to columns
  router.connectScalar(ahfits::e_READONLY,"REGION_ID",l_region_id);
  router.connectFixedLengthArray(ahfits::e_READONLY,"CORNER_DETX",
                                 &xcornercoords_row[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"CORNER_DETY",
                                 &ycornercoords_row[0]);
    
  // Store CORNER_DETX and CORNER_DETY in xcornercoords and ycornercoords
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp);
    regionnumber.push_back(l_region_id);
    xcornercoords.push_back(xcornercoords_row);
    ycornercoords.push_back(ycornercoords_row);
  }
   
  detxcen.resize(numdetx,0.0);
  detycen.resize(numdety,0.0);
  
  //Now calulate the center coordinates of x and y pixels
  for (int ii=0; ii<numdetx; ++ii) {
    detxcen[ii] = 0.5 * (detxlo[ii] + detxhi[ii]);
  } //end of loop over x pixels
  
  for (int ii=0; ii<numdety; ++ii) {
    detycen[ii] = 0.5 * (detylo[ii] + detyhi[ii]);
  } //end of loop over y pixels 


  //contamifunctionsindex will hold the region id for a given x,y pixel
  //contamifunctions index 
  std::vector<long> contamifunctionsindex_row;
  contamifunctionsindex_row.resize(numdety,0.0);
  for (int ii=0; ii<numdetx; ++ii) {
    contamifunctionsindex.push_back(contamifunctionsindex_row);
  }
   
  CartesianCoord currVertex;
   
  //For each row in the selected time epoch, calculate the x and y pixel 
  //ranges for each region and fill the contamifunctionsindex array accordingly
  for (int ii=tloindex; ii<thiindex; ++ii) { 
    vertices.clear();
    for (int jj=0; jj<numvertices; ++jj) {
      currVertex.m_x = xcornercoords[ii][jj];
      currVertex.m_y = ycornercoords[ii][jj];
      currVertex.m_z = 0.0;
      vertices.push_back(currVertex);
    }
    // Establish which x,y pixels are in the current region and fill 
    // those pixels values with the region number index, which will 
    // serve as a pointer.
    vertsX.clear();
    vertsY.clear();
    for (int gg=0; gg<numvertices; ++gg) {
      vertsX.push_back(vertices[gg].m_x);
      vertsY.push_back(vertices[gg].m_y);
    } 
    xminregion = (long) *std::min_element(vertsX.begin(),vertsX.end());
    xmaxregion = (long) *std::max_element(vertsX.begin(),vertsX.end());
    yminregion = (long) *std::min_element(vertsY.begin(),vertsY.end());
    ymaxregion = (long) *std::max_element(vertsY.begin(),vertsY.end());
    for (int jj=xminregion; jj<xmaxregion; ++jj) { 
      for (int kk=yminregion; kk<ymaxregion; ++kk) { 
        pointcoords.m_x = detxcen[jj];
        pointcoords.m_y = detycen[kk];
        pointcoords.m_z = 0.0;
        pointIsInsidePolygon = arfgenlib::isPointInsidePolygon(pointcoords, 
                                                               numvertices,
                                                               vertices);
        if (pointIsInsidePolygon == true) { 
          contamifunctionsindex[jj][kk] = ii;
        }
      }
    }
  } // End of for loop over rows in the selected time epoch
    
  //Following will hold the net transmission versus energy for 
  //each unique region
  std::vector<double> contamifunctions_row;
  contamifunctions_row.resize(numregions,0.0);
  for (int ii=0; ii<numenergies; ++ii) {
    contamifunctions.push_back(contamifunctions_row);
  }
 
  if (abund.size() != cols.size() || abund.size() != covfac.size()) {
    AH_THROW_RUNTIME("abund, cols, and covfac must have same number of elements");
  }
  if (abund.size() != 1 && abund.size() != static_cast<size_t>(nmaterials)) {
    AH_THROW_RUNTIME(std::string("number of values in abund, cols, and covfac arrays ") 
     + std::string("must equal the number of materials, or else be 1"));
  }
  for (int ii=tloindex; ii<=thiindex; ++ii) {
    for (int jj=0; jj<numenergies; ++jj) {
      tottrans=1.0;
      for (int kk=0, mm=0; kk<nmaterials; ++kk) {
        if (abund.size() == 1) {
          mm = 0;
        } else {
          mm = kk;
        }
        nh = abund[mm] * (coldens[regionnumber[ii]][kk] + cols[mm]);
        pfac = coveringfactor[regionnumber[ii]][kk]*covfac[mm];
        if (pfac > 1.0) {
           AH_INFO(ahlog::HIGH) << "Warning: Contaminant covering factor = " << 
             pfac << ", which is greater than 1" << std::endl;
        }
        tottrans = tottrans*((1.0-pfac)+ ((pow(transfactor[jj][kk],nh)) * pfac));
      } //End of for loop over materials
	contamifunctions[jj][regionnumber[ii]] = tottrans;
    } //End of for loop over energy
  } //End of for loop over each region index

  // Close file
  ahfits::close(ahffp);

}

//------------------------------------------------------------------------------

void readhxiqedata(std::string & qefile, long & numlayers,long & numrawx, 
                   long & numrawy, long & numebinscoarse, 
                   std::vector<double> &ecoarsecen, 
                   std::vector < std::vector < std::vector < std::vector<double> > > >  & qemap
                   ) {
  // NOTE: the dimensions of qe map arrays are given as inputs to this routine 
  // (obtained from the exposure map); these should be consistent with keywords etc.
  // for those same dimensions in the QE file – we could put some checks here for 
  // that.
  // Current format of the QE file is:
  // -Energy array in 2nd extension
  // -QE data in 1st extension: each column corresponds to 1 layer; each row 
  //  corresponds to 1 energy and contains an array of a spatial QE map with 
  //  dimensions numrawx, numrawy

  ahfits::FilePtr ahffp_qe;       // Ahfits file pointer to hxi qe file
  long numqeenergies=0;           // number of qe energies given by NAXIS2 keyword in 
                                  // extension 2 of qe file
  std::vector<double> qeenergies; // Vector container for column ENERGY ext. 2 
  double l_energy = 0.;           // Local container data in column ENERGY ext. 2
  long numqefilerows = 0;         // number of qe file rows NAXIS2 in ext. 1 
  int irawx = 0.;                 // value of column RAWX in row i
  int irawy = 0.;                 // value of column RAWY in row i
  std::stringstream ss;
  std::string qecolname;

  double ** qe;                             // row of vectors read from file ext. 1
                                             // connects to columns QE_LAYER1, 
                                             // QE_LAYER2, ...
  std::vector<double> qefileslice;           // slice of QE vs energy (file grid) for
                                             // a particular pixel and layer
  double outqe=0;                            // single interpolated qe point
 

  // Open QE file to extension 2
  ahfits::open(qefile,"",&ahffp_qe);
  ahfits::move(ahffp_qe,3); 
 
  // Read keyword
  numqeenergies = ahfits::getKeyValLLong(ahffp_qe,"NAXIS2");
  
  // Setup router connect to columns
  ahfits::Router router(ahffp_qe);
  router.connectScalar(ahfits::e_READONLY,"ENERGY",l_energy);

  // Read column ENERGY and store in vector qeenergies
  for (ahfits::firstRow(ahffp_qe); ahfits::readOK(ahffp_qe); 
       ahfits::nextRow(ahffp_qe)) {
    ahfits::readRow(ahffp_qe);
    qeenergies.push_back(l_energy);
  }

  // Clear local variable to column connections from router
  router.clearConnections();
  
  // Move to extension 1
  ahfits::move(ahffp_qe,2);  

  // Read in the number of rows in extension 1
  numqefilerows = ahfits::getKeyValLLong(ahffp_qe,"NAXIS2");
  
  // Initialize the QE array that will be the result of interpolation onto the 
  // coarse energy grid used for the raytracing.
   
  qemap.resize(numebinscoarse);
  for (int aa=0; aa<numebinscoarse; ++aa) {
    qemap[aa].resize(numrawx);
    for (int bb=0; bb<numrawx; ++bb) {
      qemap[aa][bb].resize(numrawy);
      for (int cc=0; cc<numrawy; ++cc) {
        qemap[aa][bb][cc].resize(numlayers);
      }
    }
  }

  for (int aa=0; aa<numebinscoarse; ++aa) {
    for (int bb=0; bb<numrawx; ++bb) {
      for (int cc=0; cc<numrawy; ++cc) {
        for (int dd=0; dd<numlayers; ++dd) {
          qemap[aa][bb][cc][dd] = 0.0;
        } 
      }
    }
  }

  //connect local variables to router      
  router.connectScalar(ahfits::e_READONLY,"RAWX",irawx);
  router.connectScalar(ahfits::e_READONLY,"RAWY",irawy);
 
  // Allocate memory to qe
  qe = new double *[numlayers];
  for (int kk=0; kk<numlayers; ++kk) {
    qe[kk] = new double[numqeenergies];
  }

  // Connect each element of qefile array to columns QE_LAYER1, QE_LAYER2, ...
  ss.str("");
  for (int ii=0; ii<numlayers; ++ii) {
    ss << ii;
    qecolname = "QE_LAYER" + ss.str();
    ss.str("");
    router.connectFixedLengthArray(ahfits::e_READONLY,qecolname,qe[ii]);
  }
 
  // Values read from the QE file are interpolated onto the raytracing energy grid;
  // the result of that is stored in qemap

  qefileslice.resize(numqeenergies,0.0); 
  // Move to first row
  ahfits::firstRow(ahffp_qe);
  for (int ii=0; ii<numqefilerows; ++ii) {
    //Read row and then continue to the next one
    // AH_INFO(ahlog::LOW) << "Reading row " << ahfits::currentRow(ahffp_qe) << " of QE map file" << std::endl;
    ahfits::readRow(ahffp_qe);
    for (int mm=0; mm<numlayers; ++mm) {
      for (int ll=0; ll<numqeenergies; ++ll) {
        qefileslice[ll] = qe[mm][ll];
      }
      for (int ll=0; ll<numebinscoarse; ++ll) {
        if ((ecoarsecen[ll] >= qeenergies[0]) && (ecoarsecen[ll] <= qeenergies[numqeenergies-1])) {
          bisectionInterp(numqeenergies,qeenergies,qefileslice,ecoarsecen[ll],outqe);
          qemap[ll][irawx-1][irawy-1][mm] = outqe;
        } else {
          qemap[ll][irawx-1][irawy-1][mm] = 0.0;
        }
      }
    }
    ahfits::nextRow(ahffp_qe);
  }

  // Close HXI qe file
  ahfits::close(ahffp_qe);

  // Clean up 
  for (int ii=0;ii<numlayers;++ii) {
    delete [] qe[ii];
  }
  delete [] qe;

}


//------------------------------------------------------------------------------


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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

std::string doubleToString(double value) {
	std::ostringstream os;
	os << value;
	return os.str();
}

//------------------------------------------------------------------------------

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
  // y values and also test if the point lies on origi 
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

//------------------------------------------------------------------------------

void listStringsToDoubles(const std::string & stringList, 
                          std::vector<double> & doubleList) {
  
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

//------------------------------------------------------------------------------


void readahvignetfile(const std::string & vignetfile, int & numcoeff, 
                      double *** vignetcoeff) { 

  ahfits::FilePtr ahffp=0;          // ahfits file point to vignetting function
  long numinstr = 0;                // The total number of instruments
  long numcolumns = 0;              // the total number of columns
  std::vector<double> l_columndata; // Store data for each row of PAR0, PAR1 ect.
  std::vector< std::vector<double> > columndata; //Store all the data in columns PAR0, PAR1 ect.
  std::string l_inst;               // local contain for instrument column
  std::vector<std::string> inst;    /// store all instrument names


  // Open file
  ahfits::open(vignetfile,"", &ahffp);
  
  // Move to extension one
  ahfits::move(ahffp,2);
  
  // Get header key values
  numinstr = ahfits::getKeyValLLong(ahffp, "NAXIS2");
  numcolumns = ahfits::getKeyValLLong(ahffp, "TFIELDS");

  // Check an assumption
  if (numinstr != 4) {
    AH_THROW_RUNTIME("Should be 4 instruments in vignetting file");
  }

  // Determine the number of coefficients
  numcoeff = numcolumns - 1;
  
  // Assign memeory and intialize
  *vignetcoeff = new double*[numinstr];
  for (int ii=0;ii<numinstr;++ii) {
    (*vignetcoeff)[ii] = new double[numcoeff];
    for (int kk=0; kk<numcoeff; ++kk) (*vignetcoeff)[ii][kk] = 0.0;
  }

  // Assign memory to l_columndata  
  l_columndata.resize(numcoeff,0.0);

  // Setup router
  ahfits::Router router(ahffp);
  
  // Connect local varibles, l_columndata is a vector
  // connected to PAR0, PAR1, PAR2, ... PAR(numcoeff)
  std::stringstream ss;
  std::string colname;
  router.connectScalar(ahfits::e_READONLY,"INSTRUMENT",l_inst);
  for (int ii=0; ii<numcoeff; ++ii) {
    ss << ii;
    colname = "PAR" + ss.str();
    ss.str("");
    router.connectScalar(ahfits::e_READONLY,colname,l_columndata[ii]);
  }  

  // Store all column data into inst (column 1) and columndata (rest of the columns)
  for (ahfits::firstRow(ahffp); ahfits::readOK(ahffp); ahfits::nextRow(ahffp)) {
    ahfits::readRow(ahffp);
    inst.push_back(l_inst);
    columndata.push_back(l_columndata);
  }

  // Assign each value of vignetcoeff
  for (int ii=0; ii<numinstr; ++ii) {
    for (int jj=0; jj<numcoeff; ++jj) {
      // AH_INFO(ahlog::LOW) << "inst[ii]=" << inst[ii] << " ii=" << ii << " jj=" << jj 
      //   << " columndata[ii][jj]=" << columndata[ii][jj] << std::endl;
      if (inst[ii] == "SXS") {
        (*vignetcoeff)[0][jj] = columndata[ii][jj];
      } else if (inst[ii] == "SXI") {             
        (*vignetcoeff)[1][jj] = columndata[ii][jj];
      } else if (inst[ii] == "HXI1") {            
        (*vignetcoeff)[2][jj] = columndata[ii][jj];
      } else if (inst[ii] == "HXI2") {            
        (*vignetcoeff)[3][jj] = columndata[ii][jj];
      } else {
        AH_THROW_RUNTIME("Bad instrument string in vignetting file");
      }
    }
  }
   
  // Close File 
  ahfits::close(ahffp);  

}

//-----------------------------------------------------------------------------

/* Compute SXT vignetting.

   energy - energy in keV
   offaxis - offaxis angle in arcmin
   param - array of 5 parameters
*/
double sxtvign(double energy, double offaxis, double* param) {

  /*
    vign(th,E) = 1/(1+(2*th/LW)**2)
    LW(E) = p0/(exp(p1*(E-p2))+1) + p3*E + p4

    where E = energy, th = offaxis, and p0 = param[0], etc
  */

  double lw=param[0]/(exp(param[1]*(energy-param[2]))+1)+param[3]*energy+param[4];
  double val=2.*offaxis/lw;
  double vig=1./(1.+val*val);

  return vig;
}

//-----------------------------------------------------------------------------

/* Compute HXT vignetting.

   energy - energy in keV
   offaxis - offaxis angle in arcmin
   param - array of 5 parameters
*/
double hxtvign(double energy, double offaxis, double* param) {

  /*
    vign(th,E) = [1/(1+(2*th/LW)**2)-1]*LN + 1
    LW(E) = p0*exp(E/p1) + p2
    LN(E) = p3*E + p4

    where E = energy, th = offaxis, and p0 = param[0], etc
  */

  double lw=param[0]*exp(energy/param[1])+param[2];
  double ln=param[3]*energy+param[4];
  double val=2.*offaxis/lw;
  double vig=(1./(1.+val*val)-1.)*ln+1.;

  return vig;
}

//-----------------------------------------------------------------------------

int ahvignetfactor(char* instrume, double & energy, double & offaxisangle,
               int & numcoeff, double ** vignetcoeff, 
               double & vignetfactor) {

  int i = -1;            // Instrument  0=SXS,1=SXI,2=HXI1,3=HXI2
  int error_status = 0;  // Error status

  // Determine instrument
  if (0 == strcasecmp(instrume, "SXS")) {
    i = 0; 
  } else if (0 == strcasecmp(instrume, "SXI")) {
    i = 1;
  } else if (0 == strcasecmp(instrume, "HXI1")) { 
    i = 2;
  } else if (0 == strcasecmp(instrume,"HXI2")) {
    i = 3;
  }

  // Compute vignetting factor based on the instrument
  if ((0 == strcasecmp(instrume, "SXS")) || (0 == strcasecmp(instrume, "SXI"))) {
    vignetfactor = sxtvign(energy, offaxisangle, vignetcoeff[i]);
  } else if ((0 == strcasecmp(instrume, "HXI1")) ||(0 == strcasecmp(instrume,"HXI2"))) {
    vignetfactor = hxtvign(energy, offaxisangle, vignetcoeff[i]);
  } else {
    ahlog_err(__PRETTY_FUNCTION__,"INSTRUME keyword is not regonized \n");
    error_status = 1;
    return error_status;
  }
  return error_status;
}


void cleanup_vignet(double ** vignetcoeff) {

  if (0 != vignetcoeff) {
    for (int ii=0; ii<4; ++ii) {
      if (0 != vignetcoeff[ii]) {
        delete [] vignetcoeff[ii];
      }
    }
    delete [] vignetcoeff;
  }

}


} //end namespace arfgenlib


/** Revision Log
 $Log: arfgenlib.cxx,v $
 Revision 1.32  2016/07/29 17:47:25  mdutka
 Adding check on each individual file in the list of region files, also fixing segfault if program fails in dowork or initialize

 Revision 1.31  2016/04/13 14:00:09  mdutka
 Adding Telescop keyword to output arf

 Revision 1.30  2016/03/18 18:43:10  mdutka
 minor correct to output logging (missing space)

 Revision 1.29  2016/03/03 23:05:53  mdutka
 minor change to logging after setupregion struct fails to read and regionfile

 Revision 1.28  2016/02/26 17:07:57  mdutka
 Removing extraneous std::cout statements

 Revision 1.27  2016/02/23 21:51:01  rshill
 Change from TY:  protect QE interpolation from energy out of range;
 return 0 for the QE in that case.

 Revision 1.26  2016/01/27 23:01:59  rshill
 In readhxiqedata, process QE map file one line at a time.

 Revision 1.25  2016/01/23 18:06:38  mdutka
 corrected bug in geteadata, fine grid was being read into the coarse grid

 Revision 1.24  2016/01/11 23:42:54  rshill
 Robust processing of abund, cols, covfac parameters to tweak contamination.

 Revision 1.22  2016/01/08 20:56:22  rshill
 Added move to first bintable for EA file; corrected move
 to first row of QE file so it happens for each layer, not just the first.

 Revision 1.21  2016/01/07 14:37:05  mdutka
 Corrected reading of SPECRSP column in getarfdata subroutine

 Revision 1.20  2016/01/07 00:52:53  rshill
 Fixed non-robust vignet_cleanup routine.

 Revision 1.19  2016/01/05 23:42:35  rshill
 Changed vignetting to use plain C-style interior functions for clarity.
 Corrected order of parameters for consistency with CALDB file.

 Revision 1.18  2015/12/31 14:59:42  mdutka
 Commiting vigneting functions for reading the vigneting caldb file and determining the vigneting factor

 Revision 1.17  2015/12/21 17:58:32  mdutka
 Adding more code cleanup and documentation

 Revision 1.16  2015/12/18 17:20:26  mdutka
 Checking in tested version of the readhxiqe function


*/
