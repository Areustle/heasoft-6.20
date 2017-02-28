/// \file sxipilib.h
/// \brief CALDB access routines for sxipi
/// \author Andy Sargent 
/// \date $Date: 2017/01/13 17:44:08 $
///
/// This header file declares the task-specific enumerations and functions needed for use in the 
/// sxipi task.  The functions are defined in sxipilib.cxx.

#define AHLABEL tool_sxipi_sxipilib
#define AHCVSID "$Id: sxipilib.cxx,v 1.59 2017/01/13 17:44:08 mdutka Exp $"

#include "sxipilib.h"
#include "ahmission/caldb.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahversion.h"
#include "ahgen/ahgen.h"
#include "hdcal.h"               // Caldb query utilities
#include "fitsio.h"
#include "ahmath/ahmath.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "ape/ape_trad.h"
#include "headas_utils.h"        // expand_item_list()

#include <sstream>
#include <cmath>                 // pow()
#include <cstring>               // strlen()
#include <cstdlib>               // used to convert string to long: strtol()
#include <time.h>                // temporary used to mesure performance

namespace sxipilib {

void loadHK(const std::string & hkfile, HKData & hkdat, const std::string& extname, 
            const std::string& hkcolstem, double tstart, double tstop, 
            const std::vector<char>& hkvideoid) {

  std::string inst;

  // Local variables for router connection
  double l_timeHK = 0.;                    // HK time from HK file
  char l_timeHK_nullflg = 0;               // local null flag for time column
  double l_vt0 = 0.;                       // video temperature of ASIC PQ on video board A (CCD1)
  double l_vt1 = 0.;                       // video temperature of ASIC RS on video board A (CCD2)
  double l_vt2 = 0.;                       // video temperature of ASIC PQ on video board B (CCD3)
  double l_vt3 = 0.;                       // video temperature of ASIC RS on video board B (CCD4)

  double tstarthk = 0.0;
  double tstophk = 0.0;

  std::string mio1vta = hkcolstem+"MIO1_VT_"+hkvideoid[0]+"_CAL";
  std::string mio1vtb = hkcolstem+"MIO1_VT_"+hkvideoid[1]+"_CAL";
  std::string mio2vta = hkcolstem+"MIO2_VT_"+hkvideoid[2]+"_CAL";
  std::string mio2vtb = hkcolstem+"MIO2_VT_"+hkvideoid[3]+"_CAL";

  int numrow = 0;

  // Open even charge trail and verify okay
  ahfits::FilePtr fptr; // File pointer for passed FITS file

  AH_INFO(ahlog::HIGH) << "Opening HK file " << hkfile << std::endl;
  ahfits::open(hkfile, extname, &fptr);
  if(0==ahfits::numRows(fptr)) {
    AH_THROW_RUNTIME("No rows in input HK file");
  }
  
  // Check for correct instrument
  inst = ahfits::getKeyValStr(fptr, "INSTRUME");
  
  if("SXI" != inst) {
    AH_THROW_RUNTIME("In file " + hkfile + " INSTRUME keyword should be SXI, not " + inst);
  }

  ahfits::haveColumn(fptr,mio1vta);
  ahfits::haveColumn(fptr,mio1vtb);
  ahfits::haveColumn(fptr,mio2vta);
  ahfits::haveColumn(fptr,mio2vtb);
  AH_INFO(ahlog::LOW) << "Using columns, " << mio1vta << ", " << mio1vtb << ", " << mio2vta << ", " <<  mio2vtb << "from HK file" << std::endl;

  // Read TSTART, TSTOP from HK file
  tstarthk = ahfits::getKeyValDbl(fptr, "TSTART");
  tstophk = ahfits::getKeyValDbl(fptr, "TSTOP");
  if(tstarthk > tstop || tstophk < tstart) {
    AH_THROW_RUNTIME("HK and event do not overlap");
  } else if (tstarthk <= tstart && tstophk >= tstop) {
    AH_INFO(ahlog::HIGH) << "HK and event overlap" << std::endl;
  } else {
    AH_INFO(ahlog::HIGH) << "HK and event files overlap, but some events in certain preiods may lack corresponding HK." << std::endl;
  }
  
  // start irowHK0 at row 2
  hkdat.m_irowHK0 = 1;

  // setup router
  ahfits::Router router1(fptr); // Router creation for FITS file
  ahfits::Router router2(fptr); // Router creation for FITS file
  ahfits::Router router3(fptr); // Router creation for FITS file
  ahfits::Router router4(fptr); // Router creation for FITS file

  router1.connectScalar(ahfits::e_READONLY, "TIME", l_timeHK, &l_timeHK_nullflg);
  router1.connectScalar(ahfits::e_READONLY, mio1vta, l_vt0);
  router2.connectScalar(ahfits::e_READONLY, mio1vtb, l_vt1);
  router3.connectScalar(ahfits::e_READONLY, mio2vta, l_vt2);
  router4.connectScalar(ahfits::e_READONLY, mio2vtb, l_vt3);

  // read table
  ahfits::firstRow(fptr);
  ahfits::readRow(fptr);
  if(ahfits::readOK(fptr) == false) AH_THROW_RUNTIME("No valid data for file " + hkfile);

  for(ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    hkdat.m_timeHK.push_back(l_timeHK);
    hkdat.m_timeHK_nullflgs.push_back(l_timeHK_nullflg);
    hkdat.m_vt0.push_back(l_vt0);
    hkdat.m_vt1.push_back(l_vt1);
    hkdat.m_vt2.push_back(l_vt2);
    hkdat.m_vt3.push_back(l_vt3);
    numrow++;
  }

  hkdat.m_numrows = numrow;
  
  // close FITS file
  ahfits::close(fptr);

}

// ****************************************************************************

void loadEvenOdd(std::string & eofile, EvenOddData & eodat, double tstart, 
                 double tstop, const std::string & telescop, 
                 const std::string & instrume) {

  std::string inst;      // Instrument variable string
  std::string ccnm0001;  // code name keyword
  double chunkTime = 0;
  int startRow = 0; 

  // Local variables for router connection
  double l_time = 0.;
  char l_ccdid=0;
  char l_segment=0;
  char l_readnode=0;
  int l_adcAve=0;
  int l_evenOdd=0;
  double l_vtGainNorm=0.;
  double l_vtGainIndex=0.;
  double l_vtGainOffset=0.;
  double l_gainOffset=0.;
  double l_gainLinr=0.;
  double l_gainQuad=0.;

  ahfits::FilePtr fptr=0;    // File pointer for passed FITS file

  int row = 0;          // Current row value for event loop

  // Empty all data
  clearEvenOdd(eodat);

  // Look for the even odd parameter file
  if (eofile == "CALDB") {
    AH_INFO(ahlog::HIGH) << "Using even/odd param file from CALDB." << std::endl;
    eofile = ahmission::caldb::resolve(eofile, "even odd", instrume, "-", "VT_EVEN_ODD", "-", "-", telescop);
    ape_trad_set_string("vtevnoddfile",eofile.c_str());   // to record actual file path in history
    ahfits::open(eofile, "", &fptr);
  } else {
    AH_INFO(ahlog::HIGH) << "Using even/odd param file " << eofile << std::endl;
    //Open specified file (including optional extended syntax)
    ahfits::open(eofile, "", &fptr);
    //If extended syntax including the HDU number is not provided move to the first binary extension
    if (ahfits::isPrimary(fptr)) {
      AH_INFO(ahlog::HIGH) << "Using first binary table extension of even/odd param file." << std::endl;
      ahfits::firstHDU(fptr,ahfits::e_BINARY_TBL);
    } 
    // Check for correct instrument
    inst = ahfits::getKeyValStr(fptr, "INSTRUME");
    if("SXI" != inst) {
      AH_THROW_RUNTIME("INSTRUME keyword in even/odd param file should be SXI, not " + inst);
    }
  }

  // Verify even odd file is OK
  if(0==ahfits::numRows(fptr)) {
    AH_THROW_RUNTIME("No rows in input Even Odd file");
  }

  // Get keywords from FITS file
  eodat.m_keyVtMin = ahfits::getKeyValDbl(fptr, "VT_MIN"); // VT_MIN()
  eodat.m_keyVtMax = ahfits::getKeyValDbl(fptr, "VT_MAX"); // VT_MAX()

  AH_DEBUG << "eodat.m_keyVtMin in load even odd = " << eodat.m_keyVtMin << std::endl;
  AH_DEBUG << "eodat.m_keyVtMax in load even odd = " << eodat.m_keyVtMax << std::endl; 

  eodat.m_keyVtRef = ahfits::getKeyValDbl(fptr, "VT_REF");
  eodat.m_keyPHRefEvenOdd = ahfits::getKeyValDbl(fptr, "PH_REF");

  // Allocate memory for each data
  eodat.m_ccdid.resize(96);
  eodat.m_segment.resize(96);
  eodat.m_readnode.resize(96);
  eodat.m_adcAve.resize(96);
  eodat.m_evenOdd.resize(96);
  eodat.m_vtGainNorm.resize(96);
  eodat.m_vtGainIndex.resize(96);
  eodat.m_vtGainOffset.resize(96);
  eodat.m_gainOffset.resize(96);
  eodat.m_gainLinr.resize(96);
  eodat.m_gainQuad.resize(96);

  // setup router
  ahfits::Router router(fptr); // Router creation for FITS file

  // Initialize connections to columns
  router.connectScalar(ahfits::e_READONLY,"TIME",l_time);
  router.connectScalar(ahfits::e_READONLY,"CCD_ID",l_ccdid);
  router.connectScalar(ahfits::e_READONLY,"SEGMENT",l_segment);
  router.connectScalar(ahfits::e_READONLY,"READNODE",l_readnode);
  router.connectScalar(ahfits::e_READONLY,"ADCAVE",l_adcAve);
  router.connectScalar(ahfits::e_READONLY,"EVEN_ODD",l_evenOdd);
  router.connectScalar(ahfits::e_READONLY,"VT_GAIN_NORM",l_vtGainNorm);
  router.connectScalar(ahfits::e_READONLY,"VT_GAIN_INDEX",l_vtGainIndex);
  router.connectScalar(ahfits::e_READONLY,"VT_GAIN_OFFSET",l_vtGainOffset);
  router.connectScalar(ahfits::e_READONLY,"GAIN_OFFSET",l_gainOffset);
  router.connectScalar(ahfits::e_READONLY,"GAIN_LINR",l_gainLinr);
  router.connectScalar(ahfits::e_READONLY,"GAIN_QUAD",l_gainQuad);

  // read table
  ahfits::firstRow(fptr);
  ahfits::readRow(fptr);

  if(tstart < l_time) {
    AH_THROW_RUNTIME("First time in " + eofile + " should not be greater than TSTART in event file");
  }

  chunkTime = l_time;
  startRow = ahfits::currentRow(fptr);

  while(tstart > l_time && ahfits::readOK(fptr)) {
    if (chunkTime != l_time) {
      chunkTime = l_time;
      startRow = ahfits::currentRow(fptr);
    }
    ahfits::nextRow(fptr);
    if (!ahfits::readOK(fptr)) break;
    ahfits::readRow(fptr);
  }

  row = 0;
  AH_DEBUG << "Evenodd CALDB file: Start loading data at row " << startRow << std::endl;
  ahfits::gotoRow(fptr, startRow);
  AH_DEBUG << "Evenodd CALDB file: Reading row " << row+startRow << std::endl;
  ahfits::readRow(fptr);

  while(chunkTime == l_time && readOK(fptr)){  
    if(row > 95) AH_THROW_RUNTIME("More than 96 rows of even odd data for one timestamp.");
    eodat.m_ccdid[row] = l_ccdid;
    eodat.m_segment[row] = l_segment;
    eodat.m_readnode[row] = l_readnode;
    eodat.m_adcAve[row] = l_adcAve;
    eodat.m_evenOdd[row] = l_evenOdd;
    eodat.m_vtGainNorm[row] = l_vtGainNorm;
    eodat.m_vtGainIndex[row] = l_vtGainIndex;
    eodat.m_vtGainOffset[row] = l_vtGainOffset;
    eodat.m_gainOffset[row] = l_gainOffset;
    eodat.m_gainLinr[row] = l_gainLinr;
    eodat.m_gainQuad[row] = l_gainQuad;
    row++;
    ahfits::nextRow(fptr);
    if (!ahfits::readOK(fptr)) break;
    AH_DEBUG << "Evenodd CALDB file: Reading row " << row+startRow << std::endl;
    ahfits::readRow(fptr);
  }

  AH_DEBUG << row << " rows were read from the even odd file " << std::endl;

  for (int ii=0; ii < 96; ++ii) { 
    AH_DEBUG << "eodat.m_ccdid["<<ii<<"] = " << eodat.m_ccdid[ii] << std::endl;
    AH_DEBUG << "eodat.m_segment["<<ii<<"] = " << eodat.m_segment[ii]<< std::endl;
    AH_DEBUG << "eodat.m_readnode["<<ii<<"] = " << eodat.m_readnode[ii] << std::endl;
    AH_DEBUG << "eodat.m_adcAve["<<ii<<"] = " << eodat.m_adcAve[ii] << std::endl;
    AH_DEBUG << "eodat.m_evenOdd["<<ii<<"] = " << eodat.m_evenOdd[ii] << std::endl;
    AH_DEBUG << "eodat.m_vtGainNorm["<<ii<<"] = " << eodat.m_vtGainNorm[ii] << std::endl;
    AH_DEBUG << "eodat.m_vtGainIndex["<<ii<<"] = " << eodat.m_vtGainIndex[ii]  << std::endl;
    AH_DEBUG << "eodat.m_vtGainOffset["<<ii<<"] = " << eodat.m_vtGainOffset[ii] << std::endl;
    AH_DEBUG << "eodat.m_gainOffset["<<ii<<"] = " << eodat.m_gainOffset[ii] << std::endl;
    AH_DEBUG << "eodat.m_gainLinr["<<ii<<"] = " << eodat.m_gainLinr[ii] << std::endl;
    AH_DEBUG << "eodat.m_gainQuad["<<ii<<"] = " << eodat.m_gainQuad[ii] << std::endl;
  }
  // Verify that correct number of data were read
  if(row < 1) AH_THROW_RUNTIME("No even odd data read.");

  // close FITS file
  ahfits::close(fptr);

}

// ****************************************************************************

void loadChargeTrail(std::string & chtrfile, ChargeTrailData & chtrdat, 
                     double tstart, double tstop, sxipilib::Keywords &  keydat, 
                     const std::string & telescop, const std::string & instrume, 
                     const std::string & expr) {

  std::string inst;     //instrume keyword
  std::string ccnm0001; // code name keyword 
  double chunkTime = 0;
  int startRow = 0;

  // Local variables for router connection
  double l_time=0.;
  char l_ccdid=0;
  char l_segment=0;
  char l_readnode=0;
  double l_normH=0.;
  double l_normV=0.;
  double l_indexH=0.;
  double l_indexV=0.;
  double l_offsetH=0.;
  double l_offsetV=0.;
  double l_ph_cut=0.;
  double l_eslope=0.;
  double l_eoffset=0.;

  ahfits::FilePtr fptr=0;    // File pointer for passed FITS file

  int row = 0;          // Current row value for event loop

  // Empty all data
  clearChargeTrail(chtrdat);

  // Look for the charge trail parameter file
  if (chtrfile == "CALDB") {
    AH_INFO(ahlog::HIGH) << "Using charge trail param file from CALDB." << std::endl;
    chtrfile = ahmission::caldb::resolve(chtrfile, "charge trail", instrume, "-", "CHARGE_TRAIL", "-", expr, telescop);
    ape_trad_set_string("chtrailfile",chtrfile.c_str());   // to record actual file path in history
    ahfits::open(chtrfile, "", &fptr);
  } else {
       
    AH_INFO(ahlog::HIGH) << "Using charge trail param file " << chtrfile << std::endl;
    //Open specified file (including optional extended syntax)
    ahfits::open(chtrfile, "", &fptr);
    
    // Move to the correct extension depending on the data mode
    std::string windowmode = expr.substr(12,7); 
    if (ahgen::strtoupper(windowmode) == "WINDOW1") {
      AH_INFO(ahlog::HIGH) << "Datamode is Window1 moving to first extension" << std::endl;
      ahfits::move(fptr,2);
    } else if (ahgen::strtoupper(windowmode) == "WINDOW2") {
      AH_INFO(ahlog::HIGH) << "Datamode is Window2 moving to second extension" << std::endl;
      ahfits::move(fptr,3);
    } else {
      AH_THROW_RUNTIME("Datamode keyword is not recognized, must be either Window1 or Window2");
    }
 
    // Check for correct instrument
    inst = ahfits::getKeyValStr(fptr, "INSTRUME");
    if("SXI" != inst) {
      AH_THROW_RUNTIME("INSTRUME keyword in charge trail param file should be SXI, not " + inst);
    }
  }

  // Verify charge trail file is OK
  if(0 == ahfits::numRows(fptr)) {
    AH_THROW_RUNTIME("No rows in SXI charge trail CALDB file");
  }

  // Get keywords from FITS file
  chtrdat.m_keyPHRefChTrail = ahfits::getKeyValDbl(fptr, "PH_REF");

  // Allocate memory for each data
  chtrdat.m_ccdid.resize(16);
  chtrdat.m_segment.resize(16);
  chtrdat.m_readnode.resize(16);
  chtrdat.m_normH.resize(16);
  chtrdat.m_normV.resize(16);
  chtrdat.m_indexH.resize(16);
  chtrdat.m_indexV.resize(16);
  chtrdat.m_offsetH.resize(16);
  chtrdat.m_offsetV.resize(16);
  chtrdat.m_ph_cut.resize(16);
  chtrdat.m_eslope.resize(16);
  chtrdat.m_eoffset.resize(16);

  // setup router
  ahfits::Router router(fptr); // Router creation for FITS file

  router.connectScalar(ahfits::e_READONLY,"TIME",l_time);
  router.connectScalar(ahfits::e_READONLY,"CCD_ID",l_ccdid);
  router.connectScalar(ahfits::e_READONLY,"SEGMENT",l_segment);
  router.connectScalar(ahfits::e_READONLY,"READNODE",l_readnode);
  router.connectScalar(ahfits::e_READONLY,"NORM_H",l_normH);
  router.connectScalar(ahfits::e_READONLY,"NORM_V",l_normV);
  router.connectScalar(ahfits::e_READONLY,"INDEX_H",l_indexH);
  router.connectScalar(ahfits::e_READONLY,"INDEX_V",l_indexV);
  router.connectScalar(ahfits::e_READONLY,"OFFSET_H",l_offsetH);
  router.connectScalar(ahfits::e_READONLY,"OFFSET_V",l_offsetV);
  router.connectScalar(ahfits::e_READONLY,"PH_CUT",l_ph_cut);
  router.connectScalar(ahfits::e_READONLY,"ESLOPE",l_eslope);
  router.connectScalar(ahfits::e_READONLY,"EOFFSET",l_eoffset);

  // read table
  ahfits::firstRow(fptr);
  ahfits::readRow(fptr);

  if(tstart < l_time) {
    AH_THROW_RUNTIME("First time in " + chtrfile + " should not be greater than TSTART in event file");
  }

  chunkTime = l_time;
  startRow = ahfits::currentRow(fptr);  

  while(tstart > l_time && ahfits::readOK(fptr)) {
    if (chunkTime != l_time) {
      chunkTime = l_time;
      startRow = ahfits::currentRow(fptr);
    }
    ahfits::nextRow(fptr);
    if (!ahfits::readOK(fptr)) break;
    ahfits::readRow(fptr);
  }

  row = 0;
  AH_DEBUG << "Charge trail CALDB file: Start loading data at row " << startRow << std::endl;
  ahfits::gotoRow(fptr, startRow);
  AH_DEBUG << "Charge trail CALDB file: Reading row " << row+startRow << std::endl;
  ahfits::readRow(fptr);

  while(chunkTime == l_time && readOK(fptr)){  
    if(row > 15) AH_THROW_RUNTIME("More than 16 rows of charge trail data for one timestamp.");
    chtrdat.m_ccdid[row] = l_ccdid;
    chtrdat.m_segment[row] = l_segment;
    chtrdat.m_readnode[row] = l_readnode;
    chtrdat.m_normH[row] = l_normH;
    chtrdat.m_normV[row] = l_normV;
    chtrdat.m_indexH[row] = l_indexH;
    chtrdat.m_indexV[row] = l_indexV;
    chtrdat.m_offsetH[row] = l_offsetH;
    chtrdat.m_offsetV[row] = l_offsetV;
    chtrdat.m_ph_cut[row] = l_ph_cut;
    chtrdat.m_eslope[row] = l_eslope;
    chtrdat.m_eoffset[row] = l_eoffset;
    row++;
    ahfits::nextRow(fptr);
    if (!ahfits::readOK(fptr)) break;
    AH_DEBUG << "Charge trail CALDB file: Reading row " << row+startRow << std::endl;
    ahfits::readRow(fptr);
  }
  AH_DEBUG << row << " rows were read for charge trail file" << std::endl;  

  for (int ii=0; ii < 16; ++ii) { 
    AH_DEBUG << "chtrdat.m_ccdid["<<ii<<"] = " << chtrdat.m_ccdid[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_segment["<<ii<<"] = " << chtrdat.m_segment[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_readnode["<<ii<<"] = " << chtrdat.m_readnode[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_normH["<<ii<<"] = " << chtrdat.m_normH[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_normV["<<ii<<"] = " << chtrdat.m_normV[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_indexH["<<ii<<"] = " << chtrdat.m_indexH[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_indexV["<<ii<<"] = " << chtrdat.m_indexV[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_offsetH["<<ii<<"] = " << chtrdat.m_offsetH[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_offsetV["<<ii<<"] = " << chtrdat.m_offsetV[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_ph_cut["<<ii<<"] = " << chtrdat.m_ph_cut[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_eslope["<<ii<<"] = " << chtrdat.m_eslope[ii] << std::endl;
    AH_DEBUG << "chtrdat.m_eoffset["<<ii<<"] = " << chtrdat.m_eoffset[ii] << std::endl;
  }
  // Verify that correct number of data were read
  if(row < 1) AH_THROW_RUNTIME("No charge trail data read.");

  // close FITS file
  ahfits::close(fptr);

}

// ****************************************************************************

void loadCTI(std::string & ctifile, CTIData & ctidat, AnomRegData & AnomRegData,
             double tstart, double tstop, const sxipilib::Keywords & keydat, 
             const std::string & telescop, const std::string & instrume, 
             const std::string & expr) {

  std::string inst;
  std::string ccnm0001;
  double chunkTime = 0;
  int startRow = 0;

  // Local variables for router connection
  double l_time=0.;
  char l_ccdid=0;
  char l_segment=0;
  char l_readnode=0;
  vecDbl l_paraFNorm;
  vecDbl l_paraFProb;
  vecDbl l_paraFScale;
  vecDbl l_paraFIndex;
  vecDbl l_paraSNorm;
  vecDbl l_paraSProb;
  vecDbl l_paraSScale;
  vecDbl l_paraSIndex;
  vecDbl l_paraANorm;
  vecDbl l_paraAProb;
  vecDbl l_paraAScale;
  vecDbl l_paraAIndex;
  vecDbl l_gfactor;       
  vecDbl l_goffset_al0;   
  vecDbl l_goffset_al1;   
  vecDbl l_goffset_al2;   
  vecDbl l_goffset_al3;   
  vecDbl l_goffset_am0;   
  vecDbl l_goffset_am1;   
  vecDbl l_goffset_am2;   
  vecDbl l_goffset_am3;   
  vecDbl l_goffset_ah0;   
  double l_goffset_ebl;   
  double l_goffset_ebh;   
  double l_ph_cut = 0;    

  //anomaly region local variables
  int l_ccdid_anomaly;
  int l_actx;
  int l_anomaly_min;
  int l_anomaly_max;

  int row = 0;          // Current row value for event loop

  ahfits::FilePtr fptr; // File pointer for passed FITS file

  // Empty all data
  clearCTI(ctidat);

  // Look for the CTI parameter file
  if (ctifile == "CALDB") {
    AH_INFO(ahlog::HIGH) << "Using CTI param file from CALDB." << std::endl;
    ctifile = ahmission::caldb::resolve(ctifile, "CTI", instrume, "-", "CTI", "-", expr, telescop);
    ape_trad_set_string("ctifile",ctifile.c_str());   // to record actual file path in history
    ahfits::open(ctifile, "", &fptr);
  } else {
    AH_INFO(ahlog::HIGH) << "Using CTI param file " << ctifile << std::endl;
    //Open specified file (including optional extended syntax)
    ahfits::open(ctifile, "", &fptr);
   
    // Move to the correct extension depending on the data mode
    std::string windowmode = expr.substr(12,7); 
    if (ahgen::strtoupper(windowmode) == "WINDOW1") {
      AH_INFO(ahlog::HIGH) << "Datamode is Window1 moving to first extension" << std::endl;
      ahfits::move(fptr,2);
    } else if (ahgen::strtoupper(windowmode) == "WINDOW2") {
      AH_INFO(ahlog::HIGH) << "Datamode is Window2 moving to second extension" << std::endl;
      ahfits::move(fptr,3);
    } else {
      AH_THROW_RUNTIME("Datamode keyword is not recognized, must be either Window1 or Window2");
    }

    // Check for correct instrument
    inst = ahfits::getKeyValStr(fptr, "INSTRUME");
    if("SXI" != inst) {
      AH_THROW_RUNTIME("INSTRUME keyword in CTI param file should be SXI, not " + inst);
    }
  }

  // Verify CTI file is OK
  if(0==ahfits::numRows(fptr)) {
    AH_THROW_RUNTIME("No rows in input CTI file");
  }

  ctidat.m_keyPHRefCTI = ahfits::getKeyValDbl(fptr, "PH_REF");

  // Allocate memory for each data container
  ctidat.m_ccdid.resize(16);
  ctidat.m_segment.resize(16);
  ctidat.m_readnode.resize(16);
  l_paraFNorm.resize(320);
  l_paraFProb.resize(320);
  l_paraFScale.resize(320);
  l_paraFIndex.resize(320);
  l_paraSNorm.resize(320);
  l_paraSProb.resize(320);
  l_paraSScale.resize(320);
  l_paraSIndex.resize(320);
  l_paraANorm.resize(320);
  l_paraAProb.resize(320);
  l_paraAScale.resize(320);
  l_paraAIndex.resize(320);  
  l_gfactor.resize(4);       
  l_goffset_al0.resize(4);   
  l_goffset_al1.resize(4);   
  l_goffset_al2.resize(4);   
  l_goffset_al3.resize(4);   
  l_goffset_am0.resize(4);   
  l_goffset_am1.resize(4);   
  l_goffset_am2.resize(4);   
  l_goffset_am3.resize(4);   
  l_goffset_ah0.resize(4);   

  // setup router
  ahfits::Router router(fptr); // Router creation for FITS file

  router.connectScalar(ahfits::e_READONLY,"TIME",l_time);
  router.connectScalar(ahfits::e_READONLY,"CCD_ID",l_ccdid);
  router.connectScalar(ahfits::e_READONLY,"SEGMENT",l_segment);
  router.connectScalar(ahfits::e_READONLY,"READNODE",l_readnode);

  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_F_NORM",&l_paraFNorm[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_F_PROB",&l_paraFProb[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_F_SCALE",&l_paraFScale[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_F_INDEX",&l_paraFIndex[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_S_NORM",&l_paraSNorm[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_S_PROB",&l_paraSProb[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_S_SCALE",&l_paraSScale[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_S_INDEX",&l_paraSIndex[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_A_NORM",&l_paraANorm[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_A_PROB",&l_paraAProb[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_A_SCALE",&l_paraAScale[0]);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PARA_A_INDEX",&l_paraAIndex[0]);  
  router.connectFixedLengthArray(ahfits::e_READONLY,"GFACTOR",&l_gfactor[0]);  
  router.connectFixedLengthArray(ahfits::e_READONLY,"GOFFSET_AL0",&l_goffset_al0[0]);  
  router.connectFixedLengthArray(ahfits::e_READONLY,"GOFFSET_AL1",&l_goffset_al1[0]);  
  router.connectFixedLengthArray(ahfits::e_READONLY,"GOFFSET_AL2",&l_goffset_al2[0]);  
  router.connectFixedLengthArray(ahfits::e_READONLY,"GOFFSET_AL3",&l_goffset_al3[0]);  
  router.connectFixedLengthArray(ahfits::e_READONLY,"GOFFSET_AM0",&l_goffset_am0[0]);  
  router.connectFixedLengthArray(ahfits::e_READONLY,"GOFFSET_AM1",&l_goffset_am1[0]);  
  router.connectFixedLengthArray(ahfits::e_READONLY,"GOFFSET_AM2",&l_goffset_am2[0]);  
  router.connectFixedLengthArray(ahfits::e_READONLY,"GOFFSET_AM3",&l_goffset_am3[0]);  
  router.connectFixedLengthArray(ahfits::e_READONLY,"GOFFSET_AH0",&l_goffset_ah0[0]);  
  router.connectScalar(ahfits::e_READONLY,"GOFFSET_EBL",l_goffset_ebl);  
  router.connectScalar(ahfits::e_READONLY,"GOFFSET_EBH",l_goffset_ebh);  
  router.connectScalar(ahfits::e_READONLY,"PH_CUT",l_ph_cut);  

  ahfits::firstRow(fptr);
  ahfits::readRow(fptr);

  // read table
  if(tstart < l_time) {
    AH_THROW_RUNTIME("First time in " + ctifile + " should not be greater than TSTART in event file");
  }

  chunkTime = l_time;
  startRow = ahfits::currentRow(fptr);

  while(tstart > l_time && ahfits::readOK(fptr)) {
    if (chunkTime != l_time) {
      chunkTime = l_time;
      startRow = ahfits::currentRow(fptr);
    }
    ahfits::nextRow(fptr);
    if (!ahfits::readOK(fptr)) break;
    ahfits::readRow(fptr);
  }

  row = 0;
  AH_DEBUG << "CTI CALDB file: Start loading data at row " << startRow << std::endl;
  ahfits::gotoRow(fptr, startRow);
  AH_DEBUG << "CTI CALDB file: Reading row " << row+startRow << std::endl;
  ahfits::readRow(fptr);

  while(chunkTime == l_time && readOK(fptr)){  
    if(row > 15) AH_THROW_RUNTIME("More than 16 rows of CTI data for one timestamp.");
    ctidat.m_ccdid[row] = l_ccdid;
    ctidat.m_segment[row] = l_segment;
    ctidat.m_readnode[row] = l_readnode;
    ctidat.m_paraFNorm.push_back(l_paraFNorm);
    ctidat.m_paraFProb.push_back(l_paraFProb);
    ctidat.m_paraFScale.push_back(l_paraFScale);
    ctidat.m_paraFIndex.push_back(l_paraFIndex);
    ctidat.m_paraSNorm.push_back(l_paraSNorm);
    ctidat.m_paraSProb.push_back(l_paraSProb);
    ctidat.m_paraSScale.push_back(l_paraSScale);
    ctidat.m_paraSIndex.push_back(l_paraSIndex);
    ctidat.m_paraANorm.push_back(l_paraANorm);
    ctidat.m_paraAProb.push_back(l_paraAProb);
    ctidat.m_paraAScale.push_back(l_paraAScale);
    ctidat.m_paraAIndex.push_back(l_paraAIndex);
    ctidat.m_gfactor.push_back(l_gfactor);
    ctidat.m_goffset_al0.push_back(l_goffset_al0);
    ctidat.m_goffset_al1.push_back(l_goffset_al1);
    ctidat.m_goffset_al2.push_back(l_goffset_al2);
    ctidat.m_goffset_al3.push_back(l_goffset_al3);
    ctidat.m_goffset_am0.push_back(l_goffset_am0);
    ctidat.m_goffset_am1.push_back(l_goffset_am1);
    ctidat.m_goffset_am2.push_back(l_goffset_am2);
    ctidat.m_goffset_am3.push_back(l_goffset_am3); 
    ctidat.m_goffset_ah0.push_back(l_goffset_ah0);
    ctidat.m_goffset_ebl.push_back(l_goffset_ebl); 
    ctidat.m_goffset_ebh.push_back(l_goffset_ebh); 
    ctidat.m_ph_cut.push_back(l_ph_cut);
    row++;
    ahfits::nextRow(fptr);
    if (!ahfits::readOK(fptr)) break;
    AH_DEBUG << "CTI CALDB file: Reading row " << row+startRow << std::endl;
    ahfits::readRow(fptr);
  }

  AH_DEBUG << row << " rows were read from cti file" << std::endl;

  for (int ii=0; ii < row; ++ii) { 
    AH_DEBUG << "ctidat.m_ccdid["<<ii<<"] = " << ctidat.m_ccdid[ii] << std::endl;
    AH_DEBUG << "ctidat.m_segment["<<ii<<"] = " << ctidat.m_segment[ii] << std::endl;
    AH_DEBUG << "ctidat.m_readnode["<<ii<<"] = " << ctidat.m_readnode[ii] << std::endl;
    AH_DEBUG << "ctidat.m_paraFNorm["<<ii<<"][0] = " << ctidat.m_paraFNorm[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraFProb["<<ii<<"][0] = " << ctidat.m_paraFProb[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraFScale["<<ii<<"][0] = " << ctidat.m_paraFScale[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraFIndex["<<ii<<"][0] = " << ctidat.m_paraFIndex[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraSNorm["<<ii<<"][0] = " << ctidat.m_paraSNorm[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraSProb["<<ii<<"][0] = " << ctidat.m_paraSProb[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraSScale["<<ii<<"][0] = " << ctidat.m_paraSScale[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraSIndex["<<ii<<"][0] = " << ctidat.m_paraSIndex[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraANorm["<<ii<<"][0] = " << ctidat.m_paraANorm[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraAProb["<<ii<<"][0] = " << ctidat.m_paraAProb[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraAScale["<<ii<<"][0] = " << ctidat.m_paraAScale[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_paraAIndex["<<ii<<"][0] = " << ctidat.m_paraAIndex[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_gfactor["<<ii<<"][0] = " << ctidat.m_gfactor[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_al0["<<ii<<"][0] = " << ctidat.m_goffset_al0[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_al1["<<ii<<"][0] = " << ctidat.m_goffset_al1[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_al2["<<ii<<"][0] = " << ctidat.m_goffset_al2[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_al3["<<ii<<"][0] = " << ctidat.m_goffset_al3[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_am0["<<ii<<"][0] = " << ctidat.m_goffset_am0[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_am1["<<ii<<"][0] = " << ctidat.m_goffset_am1[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_am2["<<ii<<"][0] = " << ctidat.m_goffset_am2[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_am3["<<ii<<"][0] = " << ctidat.m_goffset_am3[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_ah0["<<ii<<"][0] = " << ctidat.m_goffset_ah0[ii][0] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_ebl["<<ii<<"] = " << ctidat.m_goffset_ebl[ii] << std::endl;
    AH_DEBUG << "ctidat.m_goffset_ebh["<<ii<<"] = " << ctidat.m_goffset_ebh[ii] << std::endl;
    AH_DEBUG << "ctidat.m_ph_cut["<<ii<<"] = " << ctidat.m_ph_cut[ii] << std::endl;
  }
  // Verify that correct number of data were read
  if(row < 1) AH_THROW_RUNTIME("No CTI data read.");

  ahfits::move(fptr,"ANOMALY_REGION");
  router.clearConnections();
  
  router.connectScalar(ahfits::e_READONLY,"CCD_ID",l_ccdid_anomaly);
  router.connectScalar(ahfits::e_READONLY,"ACTX",l_actx);
  router.connectScalar(ahfits::e_READONLY,"ANOMALY_MIN",l_anomaly_min);
  router.connectScalar(ahfits::e_READONLY,"ANOMALY_MAX",l_anomaly_max);
  
  //allocate memory to 2-D anomaly region data containers
  std::vector<int> actx_row;
  actx_row.resize(640,0);
  for (int kk=0; kk<4; ++kk) {
    AnomRegData.m_actx.push_back(actx_row);
  }
  std::vector<int> anomaly_min_row;
  anomaly_min_row.resize(640,0);
  for (int kk=0; kk<4; ++kk) {
    AnomRegData.m_anomaly_min.push_back(anomaly_min_row);
  }
  std::vector<int> anomaly_max_row;
  anomaly_max_row.resize(640,0);
  for (int kk=0; kk<4; ++kk) {
    AnomRegData.m_anomaly_max.push_back(anomaly_max_row);
  }

  int idx0 = 0;
  int idx1 = 0; 
  int idx2 = 0;
  int idx3 = 0;
  //Read anolmaly region data in from CALDB file 
  //+++ MSD WARNING current method assumes exactly 640 elements per ccdid 
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    AnomRegData.m_ccdid.push_back(l_ccdid_anomaly);
    if (l_ccdid_anomaly == 0) { 
      AnomRegData.m_actx[l_ccdid_anomaly][idx0] = l_actx;
      AnomRegData.m_anomaly_min[l_ccdid_anomaly][idx0] = l_anomaly_min;
      AnomRegData.m_anomaly_max[l_ccdid_anomaly][idx0] = l_anomaly_max;
      ++idx0;
    } else if (l_ccdid_anomaly == 1) { 
      AnomRegData.m_actx[l_ccdid_anomaly][idx1] = l_actx;
      AnomRegData.m_anomaly_min[l_ccdid_anomaly][idx1] = l_anomaly_min;
      AnomRegData.m_anomaly_max[l_ccdid_anomaly][idx1] = l_anomaly_max;
      ++idx1;
    } else if (l_ccdid_anomaly == 2) { 
      AnomRegData.m_actx[l_ccdid_anomaly][idx2] = l_actx;
      AnomRegData.m_anomaly_min[l_ccdid_anomaly][idx2] = l_anomaly_min;
      AnomRegData.m_anomaly_max[l_ccdid_anomaly][idx2] = l_anomaly_max;
      ++idx2;
    } else if (l_ccdid_anomaly == 3) { 
      AnomRegData.m_actx[l_ccdid_anomaly][idx3] = l_actx;
      AnomRegData.m_anomaly_min[l_ccdid_anomaly][idx3] = l_anomaly_min;
      AnomRegData.m_anomaly_max[l_ccdid_anomaly][idx3] = l_anomaly_max;
      ++idx3;
    } else {
      //std::cout << "l_ccdid_anomaly = " << l_ccdid_anomaly << std::endl;
      AH_THROW_RUNTIME("CCDID in ANOMALY_REGION extension of CTI CALDB file is not recognized");
    }
  }
  
  //std::cout << "idx0 = " << idx0 << std::endl;
  //std::cout << "idx1 = " << idx1 << std::endl;
  //std::cout << "idx2 = " << idx2 << std::endl;
  //std::cout << "idx3 = " << idx3 << std::endl;
  // close FITS file
  ahfits::close(fptr);

}

// ****************************************************************************

void loadSpTh(std::string & spthfile, SpThData & spthdat, double tstart, 
              const std::string & telescop, const std::string & instrume) {

  std::string inst;
  std::string ccnm0001;
  double chunkTime = 0;
  int startRow = 0;

  // Local variables for router connection
  double l_time=0.;
  char l_ccdid=0;
  char l_segment=0;
  char l_readnode=0;
  double l_spthOffset=0.;
  double l_spthLinr=0.;

  int row = 0;          // Current row value for event loop

  ahfits::FilePtr fptr; // File pointer for passed FITS file

  // Empty all data
  clearSpTh(spthdat);

  // Look for the split threshold parameter file
  if (spthfile == "CALDB") {
    AH_INFO(ahlog::HIGH) << "Using split threshold param file from CALDB." << std::endl;
    spthfile = ahmission::caldb::resolve(spthfile, "split threshold", instrume, "-", "SPLIT_TH", "-", "-", telescop);
    ape_trad_set_string("spthfile",spthfile.c_str());   // to record actual file path in history
    ahfits::open(spthfile, "", &fptr);
  } else {
    AH_INFO(ahlog::HIGH) << "Using split threshold param file " << spthfile << std::endl;
    //Open specified file (including optional extended syntax)
    ahfits::open(spthfile, "", &fptr);
    //If extended syntax including the HDU number is not provided move to the first binary extension
    if (ahfits::isPrimary(fptr)) {
      AH_INFO(ahlog::HIGH) << "Using first binary table extension of split threshold param file." << std::endl;
      ahfits::firstHDU(fptr,ahfits::e_BINARY_TBL);
    } 
    // Check for correct instrument
    inst = ahfits::getKeyValStr(fptr, "INSTRUME");
    if("SXI" != inst) {
      AH_THROW_RUNTIME("INSTRUME keyword in split threshold param file should be SXI, not " + inst);
    }
  }

  // Verify split threshold file is OK
  if(0 == ahfits::numRows(fptr)) {
    AH_THROW_RUNTIME("No rows in SXI split threshold CALDB file");
  }

  // Allocate memory for each data
  spthdat.m_ccdid.resize(16);
  spthdat.m_segment.resize(16);
  spthdat.m_readnode.resize(16);
  spthdat.m_spthOffset.resize(16);
  spthdat.m_spthLinr.resize(16);

  // setup router
  ahfits::Router router(fptr); // Router creation for FITS file

  router.connectScalar(ahfits::e_READONLY,"TIME",l_time);
  router.connectScalar(ahfits::e_READONLY,"CCD_ID",l_ccdid);
  router.connectScalar(ahfits::e_READONLY,"SEGMENT",l_segment);
  router.connectScalar(ahfits::e_READONLY,"READNODE",l_readnode);
  router.connectScalar(ahfits::e_READONLY,"SPTH_OFFSET",l_spthOffset);
  router.connectScalar(ahfits::e_READONLY,"SPTH_LINR",l_spthLinr);

  ahfits::firstRow(fptr);
  ahfits::readRow(fptr);

  if(tstart < l_time) {
    AH_THROW_RUNTIME("First time in " + spthfile + " should not be greater than TSTART in event file");
  }

  chunkTime = l_time;
  startRow = ahfits::currentRow(fptr);

  while(tstart > l_time && ahfits::readOK(fptr)) {
    if (chunkTime != l_time) {
      chunkTime = l_time;
      startRow = ahfits::currentRow(fptr);
    }
    ahfits::nextRow(fptr);
    if (!ahfits::readOK(fptr)) break;
    ahfits::readRow(fptr);
  }

  row = 0;
  AH_DEBUG << "SpTh CALDB file: Start loading data at row " << startRow << std::endl;
  ahfits::gotoRow(fptr, startRow);
  AH_DEBUG << "SpTh CALDB file: Reading row " << row+startRow << std::endl;
  ahfits::readRow(fptr);

  while(chunkTime == l_time && readOK(fptr)){  
    if(row > 15) AH_THROW_RUNTIME("More than 16 rows of split threshold data for one timestamp.");
    ahfits::readRow(fptr);
    spthdat.m_ccdid[row] = l_ccdid;
    spthdat.m_segment[row] = l_segment;
    spthdat.m_readnode[row] = l_readnode;
    spthdat.m_spthOffset[row] = l_spthOffset;
    spthdat.m_spthLinr[row] = l_spthLinr;
    row++;
    ahfits::nextRow(fptr);
    if (!ahfits::readOK(fptr)) break;
    AH_DEBUG << "SpTh CALDB file: Reading row " << row+startRow << std::endl;
    ahfits::readRow(fptr);
  }

  AH_DEBUG << row << " rows were read from the split threshold file" << std::endl;

  for (int ii=0; ii < 16; ++ii) { 
    AH_DEBUG << "spthdat.m_ccdid["<<ii<<"] = " << spthdat.m_ccdid[ii] << std::endl;
    AH_DEBUG << "spthdat.m_segment["<<ii<<"] = " << spthdat.m_segment[ii] << std::endl;
    AH_DEBUG << "spthdat.m_readnode["<<ii<<"] = " << spthdat.m_readnode[ii] << std::endl;
    AH_DEBUG << "spthdat.m_spthOffset["<<ii<<"] = " << spthdat.m_spthOffset[ii] << std::endl;
    AH_DEBUG << "spthdat.m_spthLinr["<<ii<<"] = " << spthdat.m_spthLinr[ii] << std::endl;
  }
  // Verify that correct number of data were read
  if(row < 1) AH_THROW_RUNTIME("No split threshold data read.");

  // close FITS file
  ahfits::close(fptr);

}

// ****************************************************************************

void loadGain(std::string & gainfile, GainData & gaindat, 
              double tstart, const sxipilib::Keywords & keydat, 
              const std::string & telescop, const std::string & instrume, 
              const std::string & expr) {

  std::string inst;
  std::string ccnm0001;
  double chunkTime = 0.;
  int startRow = 0;

  // Local variables for router connection
  double l_time=0.;
  char l_ccdid=0;
  char l_segment=0;
  char l_readnode=0;
  double l_offsetLow=0.;
  double l_linrLow=0.;
  double l_quadLow=0.;
  double l_offsetMed=0.;
  double l_linrMed=0.;
  double l_quadMed=0.;
  double l_offsetHi=0.;
  double l_linrHi=0.;
  double l_quadHi=0.;
  double l_boundLM=0.;
  double l_boundMH=0.;

  int row = 0;          // Current row value for event loop

  ahfits::FilePtr fptr; // File pointer for passed FITS file

  // Empty all data
  clearGain(gaindat);

  // Look for the gain parameter file
  if (gainfile == "CALDB") {
    AH_INFO(ahlog::HIGH) << "Using gain param file from CALDB." << std::endl;
    gainfile = ahmission::caldb::resolve(gainfile, "gain", instrume, "-", "GAIN", "now", expr, telescop);
    ape_trad_set_string("gainfile",gainfile.c_str());   // to record actual file path in history
    ahfits::open(gainfile, "", &fptr);
  } else {
    AH_INFO(ahlog::HIGH) << "Using gain param file " << gainfile << std::endl;
    //Open specified file (including optional extended syntax)
    ahfits::open(gainfile, "", &fptr);
   
    // Move to the correct extension depending on the data mode
    std::string windowmode = expr.substr(12,7); 
    if (ahgen::strtoupper(windowmode) == "WINDOW1") {
      AH_INFO(ahlog::HIGH) << "Datamode is Window1 moving to first extension" << std::endl;
      ahfits::move(fptr,2);
    } else if (ahgen::strtoupper(windowmode) == "WINDOW2") {
      AH_INFO(ahlog::HIGH) << "Datamode is Window2 moving to second extension" << std::endl;
      ahfits::move(fptr,3);
    } else {
      AH_THROW_RUNTIME("Datamode keyword is not recognized, must be either Window1 or Window2");
    }

    // Check for correct instrument
    inst = ahfits::getKeyValStr(fptr, "INSTRUME");
    if("SXI" != inst) {
      AH_THROW_RUNTIME("INSTRUME keyword in gain param file should be SXI, not " + inst);
    }
  }

  // Verify gain file
  if(0 == ahfits::numRows(fptr)) {
    AH_THROW_RUNTIME("No rows in SXI GAIN CALDB file");
  }

  // Allocate memory for each data
  gaindat.m_ccdid.resize(16);
  gaindat.m_segment.resize(16);
  gaindat.m_readnode.resize(16);
  gaindat.m_offsetLow.resize(16);
  gaindat.m_linrLow.resize(16);
  gaindat.m_quadLow.resize(16);
  gaindat.m_offsetMed.resize(16);
  gaindat.m_linrMed.resize(16);
  gaindat.m_quadMed.resize(16);
  gaindat.m_offsetHi.resize(16);
  gaindat.m_linrHi.resize(16);
  gaindat.m_quadHi.resize(16);
  gaindat.m_boundLM.resize(16);
  gaindat.m_boundMH.resize(16);

  // setup router
  ahfits::Router router(fptr); // Router creation for FITS file

  router.connectScalar(ahfits::e_READONLY,"TIME",l_time);
  router.connectScalar(ahfits::e_READONLY,"CCD_ID",l_ccdid);
  router.connectScalar(ahfits::e_READONLY,"SEGMENT",l_segment);
  router.connectScalar(ahfits::e_READONLY,"READNODE",l_readnode);
  router.connectScalar(ahfits::e_READONLY,"OFFSET_LOW",l_offsetLow);
  router.connectScalar(ahfits::e_READONLY,"LINR_LOW",l_linrLow);
  router.connectScalar(ahfits::e_READONLY,"QUAD_LOW",l_quadLow);
  router.connectScalar(ahfits::e_READONLY,"OFFSET_MED",l_offsetMed);
  router.connectScalar(ahfits::e_READONLY,"LINR_MED",l_linrMed);
  router.connectScalar(ahfits::e_READONLY,"QUAD_MED",l_quadMed);
  router.connectScalar(ahfits::e_READONLY,"OFFSET_HI",l_offsetHi);
  router.connectScalar(ahfits::e_READONLY,"LINR_HI",l_linrHi);
  router.connectScalar(ahfits::e_READONLY,"QUAD_HI",l_quadHi);
  router.connectScalar(ahfits::e_READONLY,"BOUND_LM",l_boundLM);
  router.connectScalar(ahfits::e_READONLY,"BOUND_MH",l_boundMH);

  ahfits::firstRow(fptr);
  ahfits::readRow(fptr);

  if(tstart < l_time) {
    AH_THROW_RUNTIME("First time in " + gainfile + " should not be greater than TSTART in event file");
  }

  chunkTime = l_time;
  startRow = ahfits::currentRow(fptr);

  while(tstart > l_time && ahfits::readOK(fptr)) {
    if (chunkTime != l_time) {
      chunkTime = l_time;
      startRow = ahfits::currentRow(fptr);
    }
    ahfits::nextRow(fptr);
    if (!ahfits::readOK(fptr)) break;
    ahfits::readRow(fptr);
  }

  row = 0;
  AH_DEBUG << "Gain CALDB file: Start loading data at row " << startRow << std::endl;
  ahfits::gotoRow(fptr, startRow);
  AH_DEBUG << "Gain CALDB file: Reading row " << row+startRow << std::endl;
  ahfits::readRow(fptr);

  while(chunkTime == l_time && readOK(fptr)){  
    if(row > 15) AH_THROW_RUNTIME("More than 16 rows of gain data for one timestamp.");
    gaindat.m_ccdid[row] = l_ccdid;
    gaindat.m_segment[row] = l_segment;
    gaindat.m_readnode[row] = l_readnode;
    gaindat.m_offsetLow[row] = l_offsetLow;
    gaindat.m_linrLow[row] = l_linrLow;
    gaindat.m_quadLow[row] = l_quadLow;
    gaindat.m_offsetMed[row] = l_offsetMed;
    gaindat.m_linrMed[row] = l_linrMed;
    gaindat.m_quadMed[row] = l_quadMed;
    gaindat.m_offsetHi[row] = l_offsetHi;
    gaindat.m_linrHi[row] = l_linrHi;
    gaindat.m_quadHi[row] = l_quadHi;
    gaindat.m_boundLM[row] = l_boundLM;
    gaindat.m_boundMH[row] = l_boundMH;
    row++;
    ahfits::nextRow(fptr);
    if (!ahfits::readOK(fptr)) break;
    AH_DEBUG << "Gain CALDB file: Reading row " << row+startRow << std::endl;
    ahfits::readRow(fptr);
  }

  AH_DEBUG << row << " rows were read from the gain file" << std::endl;

  for (int ii=0; ii < 16; ++ii) { 
    AH_DEBUG << "gaindat.m_ccdid["<<ii<<"] = " << gaindat.m_ccdid[ii] << std::endl;
    AH_DEBUG << "gaindat.m_segment["<<ii<<"] = " << gaindat.m_segment[ii] << std::endl;
    AH_DEBUG << "gaindat.m_readnode["<<ii<<"] = " << gaindat.m_readnode[ii] << std::endl;
    AH_DEBUG << "gaindat.m_offsetLow["<<ii<<"] = " << gaindat.m_offsetLow[ii] << std::endl;
    AH_DEBUG << "gaindat.m_linrLow["<<ii<<"] = " << gaindat.m_linrLow[ii] << std::endl;
    AH_DEBUG << "gaindat.m_quadLow["<<ii<<"] = " << gaindat.m_quadLow[ii] << std::endl;
    AH_DEBUG << "gaindat.m_offsetMed["<<ii<<"] = " << gaindat.m_offsetMed[ii] << std::endl;
    AH_DEBUG << "gaindat.m_linrMed["<<ii<<"] = " << gaindat.m_linrMed[ii] << std::endl;
    AH_DEBUG << "gaindat.m_quadMed["<<ii<<"] = " << gaindat.m_quadMed[ii] << std::endl;
    AH_DEBUG << "gaindat.m_offsetHi["<<ii<<"] = " << gaindat.m_offsetHi[ii] << std::endl;
    AH_DEBUG << "gaindat.m_linrHi["<<ii<<"] = " << gaindat.m_linrHi[ii] << std::endl;
    AH_DEBUG << "gaindat.m_quadHi["<<ii<<"] = " << gaindat.m_quadHi[ii] << std::endl;
    AH_DEBUG << "gaindat.m_boundLM["<<ii<<"] = " << gaindat.m_boundLM[ii] << std::endl;
    AH_DEBUG << "gaindat.m_boundMH["<<ii<<"] = " << gaindat.m_boundMH[ii] << std::endl;
  }
  // Verify that correct number of data were read
  if(row < 1) AH_THROW_RUNTIME("No gain data read.");

  // close FITS file
  ahfits::close(fptr);

}

// ****************************************************************************

void loadGrade(std::string & gradefile, GradeData & gradedat,
               const std::string & telescop, const std::string & instrume) {

  std::string inst;
  std::string ccnm0001;

  // Local variables for router connection
  int l_grade;
  int l_pattern[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };

  int row = 0;          // Current row value for event loop
  int numRows = 0;      // Number of data elements

  ahfits::FilePtr fptr; // File pointer for passed FITS file

  // Empty all data
  clearGrade(gradedat);

  // Look for the grade hit pattern file
  if (gradefile == "CALDB") {
    AH_INFO(ahlog::HIGH) << "Using grade hit pattern file from CALDB." << std::endl;
    gradefile = ahmission::caldb::resolve(gradefile, "grade hit pattern", instrume, "-", "HIT_PATTERN", "now", "-", telescop);
    ape_trad_set_string("patternfile",gradefile.c_str());   // to record actual file path in history
    ahfits::open(gradefile, "", &fptr);
  } else {
    AH_INFO(ahlog::HIGH) << "Using grade hit pattern file " << gradefile << std::endl;
    //Open specified file (including optional extended syntax)
    ahfits::open(gradefile, "", &fptr);
    //If extended syntax including the HDU number is not provided move to the first binary extension
    if (ahfits::isPrimary(fptr)) {
      AH_INFO(ahlog::HIGH) << "Using first binary table extension of grade hit pattern file." << std::endl;
      ahfits::firstHDU(fptr,ahfits::e_BINARY_TBL);
    } 
    // Check for correct instrument
    inst = ahfits::getKeyValStr(fptr, "INSTRUME");
    if("SXI" != inst) {
      AH_THROW_RUNTIME("INSTRUME keyword in grade hit pattern file should be SXI, not " + inst);
    }
  }

  // Verify hit pattern file
  if(0 == ahfits::numRows(fptr)) {
    AH_THROW_RUNTIME("No rows in SXI hit pattern CALDB file");
  }

  // Get number of elements
  numRows = ahfits::numRows(fptr);

  // Allocate memory for each data
  gradedat.m_grade.resize(numRows);
  gradedat.m_pattern.resize(numRows);

  // setup router
  ahfits::Router router(fptr); // Router creation for FITS file

  router.connectScalar(ahfits::e_READONLY,"GRADE",l_grade);
  router.connectFixedLengthArray(ahfits::e_READONLY,"PATTERN",l_pattern);

  // read table
  for(ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    if(ahfits::readOK(fptr) == false) AH_THROW_RUNTIME("Invalid data for file " + gradefile);

    ahfits::readRow(fptr);
    gradedat.m_pattern[row].resize(9);

    gradedat.m_grade[row] = l_grade;
    for(int ii = 0; ii < 9; ++ii) gradedat.m_pattern[row][ii] = l_pattern[ii];
    row++;
  }

  // close FITS file
  ahfits::close(fptr);

}

// ****************************************************************************

void clearEvenOdd(EvenOddData & eodat){

  // Clear keyword values
  eodat.m_keyVtMin = 0;
  eodat.m_keyVtMax = 0;
  eodat.m_keyVtRef = 0;
  eodat.m_keyPHRefEvenOdd = 0; 

  // Clear column values
  eodat.m_ccdid.clear();
  eodat.m_segment.clear();
  eodat.m_readnode.clear();
  eodat.m_adcAve.clear();
  eodat.m_evenOdd.clear();
  eodat.m_vtGainNorm.clear();
  eodat.m_vtGainIndex.clear();
  eodat.m_vtGainOffset.clear();
  eodat.m_gainOffset.clear();
  eodat.m_gainLinr.clear();
  eodat.m_gainQuad.clear();
}

// ****************************************************************************

void clearChargeTrail(ChargeTrailData & chtrdat){

  // Clear keyword values
  chtrdat.m_keyPHRefChTrail = 0;

  // Clear column values
  chtrdat.m_ccdid.clear();
  chtrdat.m_segment.clear();
  chtrdat.m_readnode.clear();
  chtrdat.m_normH.clear();
  chtrdat.m_normV.clear();
  chtrdat.m_indexH.clear();
  chtrdat.m_indexV.clear();
  chtrdat.m_offsetH.clear();
  chtrdat.m_offsetV.clear();
}

// ****************************************************************************

void clearCTI(CTIData & ctidat){

  // Clear keyword values
  ctidat.m_keyCIStatus = 0;
  ctidat.m_keyPHRefCTI = 0;

  // Clear column values
  ctidat.m_ccdid.clear();
  ctidat.m_segment.clear();
  ctidat.m_readnode.clear();
  ctidat.m_paraFNorm.clear();
  ctidat.m_paraFProb.clear();
  ctidat.m_paraFScale.clear();
  ctidat.m_paraFIndex.clear();
  ctidat.m_paraSNorm.clear();
  ctidat.m_paraSProb.clear();
  ctidat.m_paraSScale.clear();
  ctidat.m_paraSIndex.clear();
  ctidat.m_paraANorm.clear();
  ctidat.m_paraAProb.clear();
  ctidat.m_paraAScale.clear();
  ctidat.m_paraAIndex.clear();
  ctidat.m_gfactor.clear();
  ctidat.m_goffset_al0.clear();
  ctidat.m_goffset_al1.clear();
  ctidat.m_goffset_al2.clear();
  ctidat.m_goffset_al3.clear();
  ctidat.m_goffset_am0.clear();
  ctidat.m_goffset_am1.clear();
  ctidat.m_goffset_am2.clear();
  ctidat.m_goffset_am3.clear(); 
  ctidat.m_goffset_ebl.clear(); 
  ctidat.m_goffset_ebh.clear();
  ctidat.m_ph_cut.clear();

}

// ****************************************************************************

void clearSpTh(SpThData & spthdat){

  // Clear column values
  spthdat.m_ccdid.clear();
  spthdat.m_segment.clear();
  spthdat.m_readnode.clear();
  spthdat.m_spthOffset.clear();
  spthdat.m_spthLinr.clear();
}

// ****************************************************************************

void clearGain(GainData & gaindat){

  // Clear column values
  gaindat.m_ccdid.clear();
  gaindat.m_segment.clear();
  gaindat.m_readnode.clear();
  gaindat.m_offsetLow.clear();
  gaindat.m_linrLow.clear();
  gaindat.m_quadLow.clear();
  gaindat.m_offsetMed.clear();
  gaindat.m_linrMed.clear();
  gaindat.m_quadMed.clear();
  gaindat.m_offsetHi.clear();
  gaindat.m_linrHi.clear();
  gaindat.m_quadHi.clear();
  gaindat.m_boundLM.clear();
  gaindat.m_boundMH.clear();
}

// ****************************************************************************

void clearGrade(GradeData & gradedat){

  // Clear column values
  gradedat.m_grade.clear();
  gradedat.m_pattern.clear();
}

// ****************************************************************************

void searchEvenOdd(const sxipilib::EvenOddData& eodat, char ccdid, char segment,
                   char readnode, int adcAve, int evenOdd, double & vtGainNorm,
                   double & vtGainIndex, double & vtGainOffset, double & gainOffset,
                   double & gainLinr, double & gainQuad) {

  int nsize = eodat.m_ccdid.size();

  // Search for first match. CCD ID, Segment, readout node, ADC Avg and even odd must match
  for(int ii = 0; ii < nsize; ++ii) {
    
    if(ccdid != eodat.m_ccdid[ii]) continue;
    if(segment != eodat.m_segment[ii]) continue;
    if(readnode != eodat.m_readnode[ii]) continue;
    if(adcAve != eodat.m_adcAve[ii]) continue;
    if(evenOdd != eodat.m_evenOdd[ii]) continue;

    vtGainNorm = eodat.m_vtGainNorm[ii];
    vtGainIndex = eodat.m_vtGainIndex[ii];
    vtGainOffset = eodat.m_vtGainOffset[ii];

    gainOffset = eodat.m_gainOffset[ii];
    gainLinr = eodat.m_gainLinr[ii];
    gainQuad = eodat.m_gainQuad[ii];
    
    break;
  }

} // end searchEvenOdd

// ****************************************************************************

void searchChargeTrail(const sxipilib::ChargeTrailData& chtrdat, char ccdid, char segment, 
                       char readnode, double & normH, double & normV, double & indexH, 
                       double & indexV, double & offsetH, double & offsetV, double & ph_cut,
                       double & eslope, double & eoffset) {

  int nsize = chtrdat.m_ccdid.size();

  // Search for first match. CCD ID, segment and readout node must match
  for(int ii = 0; ii < nsize; ++ii) {
    
    if(ccdid != chtrdat.m_ccdid[ii]) continue;
    if(segment != chtrdat.m_segment[ii]) continue;
    if(readnode != chtrdat.m_readnode[ii]) continue;

    normH = chtrdat.m_normH[ii];
    normV = chtrdat.m_normV[ii];
    indexH = chtrdat.m_indexH[ii];
    indexV = chtrdat.m_indexV[ii];
    offsetH = chtrdat.m_offsetH[ii];
    offsetV = chtrdat.m_offsetV[ii];
    ph_cut = chtrdat.m_ph_cut[ii];
    eslope = chtrdat.m_eslope[ii];
    eoffset = chtrdat.m_eoffset[ii];

    break;
  }

} // end searchChargeTrail

// ****************************************************************************

void searchCTI(const sxipilib::CTIData& ctidat, char ccdid, char segment, 
               char readnode, vecDbl & paraFNorm, vecDbl & paraFProb, 
               vecDbl & paraFScale, vecDbl & paraFIndex, vecDbl & paraSNorm, 
               vecDbl & paraSProb, vecDbl & paraSScale, vecDbl & paraSIndex,
               vecDbl & paraANorm, vecDbl & paraAProb, vecDbl & paraAScale, 
               vecDbl & paraAIndex, vecDbl & gfactor, vecDbl & goffset_al0, 
               vecDbl & goffset_al1, vecDbl & goffset_al2, vecDbl & goffset_al3,
               vecDbl & goffset_am0, vecDbl & goffset_am1, vecDbl & goffset_am2,
               vecDbl & goffset_am3, vecDbl & goffset_ah0, double & goffset_ebl,
               double & goffset_ebh, double & ph_cut) {
  AH_DEBUG << "Start seachCTI" << std::endl;
  int nsize = ctidat.m_ccdid.size();

  AH_DEBUG << "nsize = " << nsize << std::endl; 

  // Search for first match. CCD ID, segment and readout node must match
  for(int ii = 0; ii < nsize; ++ii) {
    AH_DEBUG << "accessing element " << ii << std::endl;

    if(ccdid != ctidat.m_ccdid[ii]) continue;
    if(segment != ctidat.m_segment[ii]) continue;
    if(readnode != ctidat.m_readnode[ii]) continue;
   
    paraFNorm = ctidat.m_paraFNorm[ii];
    paraFProb = ctidat.m_paraFProb[ii];
    paraFScale = ctidat.m_paraFScale[ii];
    paraFIndex = ctidat.m_paraFIndex[ii];

    paraSNorm = ctidat.m_paraSNorm[ii];
    paraSProb = ctidat.m_paraSProb[ii];
    paraSScale = ctidat.m_paraSScale[ii];
    paraSIndex = ctidat.m_paraSIndex[ii];

    paraANorm = ctidat.m_paraANorm[ii];
    paraAProb = ctidat.m_paraAProb[ii];
    paraAScale = ctidat.m_paraAScale[ii];
    paraAIndex = ctidat.m_paraAIndex[ii];  

    gfactor = ctidat.m_gfactor[ii];
    goffset_al0 = ctidat.m_goffset_al0[ii];
    goffset_al1 = ctidat.m_goffset_al1[ii];
    goffset_al2 = ctidat.m_goffset_al2[ii];
    goffset_al3 = ctidat.m_goffset_al3[ii];
    goffset_am0 = ctidat.m_goffset_am0[ii];
    goffset_am1 = ctidat.m_goffset_am1[ii];
    goffset_am2 = ctidat.m_goffset_am2[ii];
    goffset_am3 = ctidat.m_goffset_am3[ii];
    goffset_ah0 = ctidat.m_goffset_ah0[ii];
    goffset_ebl = ctidat.m_goffset_ebl[ii];
    goffset_ebh = ctidat.m_goffset_ebh[ii];

    ph_cut = ctidat.m_ph_cut[ii];

    break;
  }
  AH_DEBUG << "end seachCTI" << std::endl;
} // end searchCTI

// ****************************************************************************

void searchAnomReg(AnomRegData anomregdat, char & ccd_id, int & actx, 
                   vecInt & anomaly_min, vecInt & anomaly_max) {
  // Returns the lower and upper ACTY boundaries of the CTI anomaly region for
  // the three columns around the input (event) ACTX.
  // Loop through the three columns.
  
  AH_DEBUG << "start searchAnomReg" << std::endl;
  
  int tmp_actx = 0; 
  int idx = 0; 


  for (int ii=0; ii<=2; ii++) {
    tmp_actx = actx + ii - 1; 
    // if the event is on the CCD boundary, then set the min/max to zero
    if (tmp_actx < 1 || tmp_actx > 640) {
      anomaly_min[ii] = 0;
      anomaly_max[ii] = 0;
    } else {
      // Find the index for tmp_actx; it’s probably tmp_actx-1, but can’t assume CALDB
      // is in order.
      for (int kk=0; kk<640; ++kk) { 
        if (anomregdat.m_actx[ccd_id][kk] == tmp_actx) { 
          //std::cout << "tmp_actx = " << tmp_actx << std::endl;
          //std::cout << "anomregdat.m_actx[ccd_id][kk] = " << anomregdat.m_actx[ccd_id][kk] << std::endl;
          idx = kk;
          break;
        }
        if (kk >= 640) { 
          AH_THROW_RUNTIME("tmp_actx not found in actx array");
        }
      }
      //std::cout << "ii = " << ii << std::endl;
      //std::cout << "anomaly min size = " << anomaly_min.size() << std::endl;
      anomaly_min[ii] = anomregdat.m_anomaly_min[ccd_id][idx];
      anomaly_max[ii] = anomregdat.m_anomaly_max[ccd_id][idx];
    }
  }



  AH_DEBUG << "end searchAnomReg" << std::endl;
} // end searchAnomReg
 
// ****************************************************************************

void searchSPTH(const sxipilib::SpThData& spthdat, char ccdid, char segment,
                char readnode, double & spthOffset, double & spthLinr) {
  
  // Search for first match. CCD ID, segment and readout node must match
  int nsize = spthdat.m_ccdid.size();

  for(int ii = 0; ii < nsize; ++ii) {
    
    if(ccdid != spthdat.m_ccdid[ii]) continue;
    if(segment != spthdat.m_segment[ii]) continue;
    if(readnode != spthdat.m_readnode[ii]) continue;

    spthOffset = spthdat.m_spthOffset[ii];
    spthLinr = spthdat.m_spthLinr[ii];

    break;
  }

} // end searchSPTH

// ****************************************************************************

void searchGain(const GainData& gaindat, char ccdid, char segment, char readnode,
                double & offsetLow, double & linrLow, double & quadLow,
                double & offsetMed, double & linrMed, double & quadMed,
                double & offsetHi, double & linrHi, double & quadHi,
                double & boundLM, double & boundMH) {
  
  int nsize = gaindat.m_ccdid.size();

  // Search for first match. CCD ID, segment and readout node must match
  for(int ii = 0; ii < nsize; ++ii) {
    
    if(ccdid != gaindat.m_ccdid[ii]) continue;
    if(segment != gaindat.m_segment[ii]) continue;
    if(readnode != gaindat.m_readnode[ii]) continue;

    offsetLow = gaindat.m_offsetLow[ii];
    linrLow = gaindat.m_linrLow[ii];
    quadLow = gaindat.m_quadLow[ii];
    
    offsetMed = gaindat.m_offsetMed[ii];
    linrMed = gaindat.m_linrMed[ii];
    quadMed = gaindat.m_quadMed[ii];
    
    offsetHi = gaindat.m_offsetHi[ii];
    linrHi = gaindat.m_linrHi[ii];
    quadHi = gaindat.m_quadHi[ii];

    boundLM = gaindat.m_boundLM[ii];
    boundMH = gaindat.m_boundMH[ii];

    break;
  }

} // end searchGain

// ****************************************************************************

void searchGradeData(const sxipilib::GradeData& gradedat, int * pattern3x3Evt,
                     int & grade3x3) {
  
  int nsize = gradedat.m_grade.size();
  bool skipRow = false;

  // Search for first hit pattern match. Assign grade.
  for(int ii = 0; ii < nsize; ++ii) {
  skipRow = false; // reset boolean
    for(int jj = 0; jj < 9; ++jj) { 
      // Loop through array, search for non-matching indexes.
      // If there is a mis-match, move to next hit pattern
      if(pattern3x3Evt[jj] != gradedat.m_pattern[ii][jj]) { 
        skipRow = true; 
        break;
      }    
    }
    if(skipRow == true) continue;

    grade3x3 = gradedat.m_grade[ii];
    break;
  }
    
} // end searchGradeData

// ****************************************************************************

void parseCALDBKeywordDbl(const std::string& keystr, double & keydbl) {

  keydbl = 0;
  std::istringstream iss(keystr);
  iss.ignore(FLEN_KEYWORD,'(');
  iss >> keydbl;

}

// ****************************************************************************

void searchVT(double timeEvt, char ccdidEvt, double deltatime,
              double & vt, int & vtStatus, double vtmin, double vtmax, 
              sxipilib::HKData & hkdat) {

  // Get temperature at the closest HK time (time hk) before timeevt.
  // HK values are recorded every 4 seconds, but not coordinated to the
  // event readout time.
  // Out of range temperature is rare, so we are not concerned with finding
  // nearest valid temperature to the event.
  // Temperatures of ASICs connected to CCD 1, 2, 3 and 4 are written in
  // MIO1_VT_A, MIO1_VT_B, MIO2_VT_A and MIO2_VT_B respectively.
  // If closest TIME has valid temperature, vt_status = 0
  // if no value within deltatime, vtStatus = -1

  // Temporary holding variables
  double timeHKNext = 0., timeHKPrev = 0.;   // HK time from HK file
  double vt0Next = 0., vt0Prev = 0;          // video temperature of ASIC PQ on video board A (CCD1)
  double vt1Next = 0., vt1Prev = 0;          // video temperature of ASIC RS on video board A (CCD2)
  double vt2Next = 0., vt2Prev = 0;          // video temperature of ASIC PQ on video board B (CCD3)
  double vt3Next = 0., vt3Prev = 0;          // video temperature of ASIC RS on video board B (CCD4)

  double timeHKInterval = 0.;                // Time between HK events
  double weightPrev = 0., weightNext = 0.;   // Weight current/previous row has on VT
  long irowHK0 = 0;
  long irowHKPrev = 0;
  long firstHKrow = 0;
  long lastHKrow =  hkdat.m_numrows - 1;

  while (hkdat.m_timeHK_nullflgs[firstHKrow] == 1) {
    ++firstHKrow;
    if (firstHKrow > hkdat.m_numrows - 1) {
      vt = 0.;
      vtStatus = -1;
      return;
    }
  }

  while (hkdat.m_timeHK_nullflgs[lastHKrow] == 1) {
    --lastHKrow;
    if (lastHKrow < 0) {
      vt = 0.;
      vtStatus = -1;
      return;
    }
  }   

  // If event time is outside range of hk file set 
  // video temperature to closest time in hkfile
  // status bit 27 will also be set  
  if(timeEvt < hkdat.m_timeHK[firstHKrow]){ 
    if(ccdidEvt == 0) vt = hkdat.m_vt0[firstHKrow];
    if(ccdidEvt == 1) vt = hkdat.m_vt1[firstHKrow];
    if(ccdidEvt == 2) vt = hkdat.m_vt2[firstHKrow];
    if(ccdidEvt == 3) vt = hkdat.m_vt3[firstHKrow];
    vtStatus = -1;
    AH_DEBUG << "Event time = " << timeEvt << std::endl;
    AH_DEBUG << "first time in loaded HK data = " << hkdat.m_timeHK[0] << std::endl;
    AH_DEBUG << "last time in loaded HK data = " << hkdat.m_timeHK[hkdat.m_numrows] << std::endl;
    AH_DEBUG << "event time less than first time loaded from HK file" << std::endl;
    return;
  }
  if (timeEvt > hkdat.m_timeHK[lastHKrow]) {
    if(ccdidEvt == 0) vt = hkdat.m_vt0[lastHKrow];
    if(ccdidEvt == 1) vt = hkdat.m_vt1[lastHKrow];
    if(ccdidEvt == 2) vt = hkdat.m_vt2[lastHKrow];
    if(ccdidEvt == 3) vt = hkdat.m_vt3[lastHKrow];
    vtStatus = -1;
    AH_DEBUG << "Event time = " << timeEvt << std::endl;
    AH_DEBUG << "first time in loaded HK data = " << hkdat.m_timeHK[0] << std::endl;
    AH_DEBUG << "last time in loaded HK data = " << hkdat.m_timeHK[hkdat.m_numrows] << std::endl;
    AH_DEBUG << "numrows hk file = " << hkdat.m_numrows << std::endl;
    AH_DEBUG << "last element hk data struct = " << hkdat.m_timeHK.back();
    AH_DEBUG << "evet time greater than last time loaded from HK file" << std::endl;
    return;
  }

  // Get first HK row within deltatime (or after) event time.
  // If no more rows in HK file, break.
  // vt_status will be set to -1 in subsequent rows.
  // This assumes that HK file is time-ordered.
  irowHK0 = hkdat.m_irowHK0;

  //insure time is not null
  //while time != null increment irowHK0
  while (hkdat.m_timeHK_nullflgs[irowHK0] == 1) {
    ++irowHK0;
    if (irowHK0 > hkdat.m_numrows) {
      vt = 0.;
      vtStatus=-1;
      return;
    }
  } 
  while(1) {
    // break if on last row
    if(irowHK0 == hkdat.m_numrows) {
      vt = 0.;
      vtStatus=-1;
      return;
    }

    // Read TIME, MIO1_VT_A, MIO1_VT_B, MIO2_VT_A, MIO2_VT_B at 
    // row = irowHK0 from HK1 file into timehk, vt0, vt1, vt2, vt3 respectively

    timeHKNext = hkdat.m_timeHK[irowHK0];
    vt0Next = hkdat.m_vt0[irowHK0];
    vt1Next = hkdat.m_vt1[irowHK0];
    vt2Next = hkdat.m_vt2[irowHK0];
    vt3Next = hkdat.m_vt3[irowHK0];

    // Move through rows until positive time difference is found
    if(timeHKNext - timeEvt > 0) {

      //initialize index of previous time
      irowHKPrev = irowHK0-1;

      //find index of previous non-null video temperature
      while (hkdat.m_timeHK_nullflgs[irowHKPrev] == 1) {
        --irowHKPrev;
        if (irowHKPrev < 0) {
          vt = 0.;
          vtStatus=-1;
          return;
        }
      }     

      // set previous time and video temperature
      timeHKPrev = hkdat.m_timeHK[irowHKPrev];
      vt0Prev = hkdat.m_vt0[irowHKPrev];
      vt1Prev = hkdat.m_vt1[irowHKPrev];
      vt2Prev = hkdat.m_vt2[irowHKPrev];
      vt3Prev = hkdat.m_vt3[irowHKPrev];

      // Get interval. HK file must be time ordered
      timeHKInterval = timeHKNext - timeHKPrev;
      if(timeHKInterval <= 0) {
        AH_DEBUG << "HK file not in time order" << std::endl;
        vtStatus = 2;
        return;
      }

      // Check if HK time gap is longer than delta time
      if(timeHKInterval <= deltatime) {
        vtStatus = 0;
        weightPrev = (timeHKNext-timeEvt)/timeHKInterval;
        weightNext = (timeEvt-timeHKPrev)/timeHKInterval;
        if(ccdidEvt == 0) vt = weightPrev*vt0Prev+weightNext*vt0Next;
        if(ccdidEvt == 1) vt = weightPrev*vt1Prev+weightNext*vt1Next;
        if(ccdidEvt == 2) vt = weightPrev*vt2Prev+weightNext*vt2Next;
        if(ccdidEvt == 3) vt = weightPrev*vt3Prev+weightNext*vt3Next;
        if(vt < vtmin || vt > vtmax) {
          AH_DEBUG << "vt = " << vt << std::endl;
          AH_DEBUG << "vtmin = " << vtmin << std::endl;
          AH_DEBUG << "vtmax = " << vtmax << std::endl;
          AH_DEBUG << "vt < VTMIN or vt > vtmax" << std::endl;
          vtStatus = 1;
        }
      } else {
        // Use previous HK row 
        vtStatus = -1;
        if(ccdidEvt == 0) vt = vt0Prev;
        if(ccdidEvt == 1) vt = vt1Prev;
        if(ccdidEvt == 2) vt = vt2Prev;
        if(ccdidEvt == 3) vt = vt3Prev;
      }
      break;
    }
    irowHK0++;
    //insure that next time is not null increment irowHK until time != null
    while (hkdat.m_timeHK_nullflgs[irowHK0] == 1) {
      ++irowHK0;
      if (irowHK0 > hkdat.m_numrows) {
        vt = 0.;
        vtStatus=-1;
        return;
      }
    }
  }
  hkdat.m_irowHK0 = irowHK0;

} // end searchVT

// ****************************************************************************

void correctEvenOdd(double * phasTmp, double timeEvt, int rawxEvt, int actxEvt, char ccdidEvt, 
                    char segmentEvt, char readnodeEvt, int adcAveEvt, const int * actvnode,
                     int evenOddCor, double deltatime, const sxipilib::EvenOddData& eodat,
                    double * phasEvenOdd, int & errorStatus, sxipilib::HKData & hkdat,
                    const sxipilib::Params& param, char * statusEvt) {

  double vt = 0.;                  // Video temperature
  int vtStatus = 0;                // Status of video temperature (0: good)

  // CALDB variables
  int evenOddEvt = 0;              // Even (0) or odd (1)
  double vtGainNorm = 0.;          // Normalization of video temperature gain function
  double vtGainIndex = 0.;         // Power-law index for video temperature gain function
  double vtGainOffset = 0.;        // Offset parameter value for video temperature gain function
  double gainOffset = 0.;          // Offset parameter value for gain function
  double gainLinr = 0.;            // Linear coefficient for the gain function
  double gainQuad = 0.;            // Quadratic coefficient for the gain function
  double gfact = 0;                // Factor correction for gain

  // CALDB variables for sides
  int evenOddSide = 0;            // Even (0) or odd (1) for side
  double vtGainNormSide = 0.;      // Normalization of video temperature gain function for side
  double vtGainIndexSide = 0.;     // Power-law index for video temperature gain function for side
  double vtGainOffsetSide = 0.;    // Offset parameter value for video temperature gain function for side
  double gainOffsetSide = 0.;      // Offset parameter value for gain function for side
  double gainLinrSide = 0.;        // Linear coefficient for the gain function for side
  double gainQuadSide = 0.;        // Quadratic coefficient for the gain function for side
  double gfactSide = 0;            // Factor correction for gain side

  // CALDB varibles for segment boundary events
  int segmentOther = 0;
  int indxOtherSegment = 0;
  int readnodeOther = 0;
  int evenOddOther = 0;             // Even (0) or odd (1) for boundary
  double vtGainNormOther = 0.;      // Normalization of video temperature gain function for boundary
  double vtGainIndexOther = 0.;     // Power-law index for video temperature gain function for boundary
  double vtGainOffsetOther = 0.;    // Offset parameter value for video temperature gain function for boundary
  double gainOffsetOther = 0.;      // Offset parameter value for gain function for boundary
  double gainLinrOther = 0.;        // Linear coefficient for the gain function for boundary
  double gainQuadOther = 0.;        // Quadratic coefficient for the gain function for boundary
  double gfactOther = 0.;           // factor correction for gain boundary

  errorStatus = 0;                // Error status. 0: OK; 1: Continue, next row; 2: Stop program

  for(int ii = 0; ii < LENPHAS; ++ii) phasEvenOdd[ii] = phasTmp[ii];

  if(evenOddCor == false) return;

  AH_DEBUG << "eodat.m_keyVtMin in even odd Correction = " << eodat.m_keyVtMin << std::endl;
  AH_DEBUG << "eodat.m_keyVtMax in even odd Correction = " << eodat.m_keyVtMax << std::endl;

  // Read HK to get vt and vtStatus
  if (param.m_hkfile != "NONE") {
    searchVT(timeEvt, ccdidEvt, deltatime, vt, vtStatus, eodat.m_keyVtMin, eodat.m_keyVtMax, hkdat);
  } else { 
    vt = 0;
    vtStatus = -1;
  }

  if (vtStatus == 2) { // HK file is invalid
    errorStatus = 2;
    return;
  }

  // Determine if even or odd
  evenOddEvt = rawxEvt%2; // Define RAWX = 2n -> even, 2n+1 -> odd
  evenOddSide = (rawxEvt+1)%2; // for side columns

  // Get CALDB values
  searchEvenOdd(eodat, ccdidEvt, segmentEvt, readnodeEvt, adcAveEvt, evenOddEvt,
                vtGainNorm, vtGainIndex, vtGainOffset, gainOffset, gainLinr,
                gainQuad);

  // Get CALDB values for side columns
  searchEvenOdd(eodat, ccdidEvt, segmentEvt, readnodeEvt, adcAveEvt, evenOddSide,
                vtGainNormSide, vtGainIndexSide, vtGainOffsetSide, gainOffsetSide,
                gainLinrSide, gainQuadSide);

  // Only for segment boundary events: Get CALDB values for a column in the other segment
  if (actxEvt == 320) {	// event center is in Segment AB
    segmentOther = 1;
    indxOtherSegment = 2 * ccdidEvt + 1;
    readnodeOther = actvnode[indxOtherSegment];
    // note that indx_other segment should be 0, 1, …, or 7. 
    evenOddOther = readnodeOther;
    // If readnode D [or C] is used, actx=321 is ‘even’ [or ‘odd’]. 
    searchEvenOdd(eodat, ccdidEvt, segmentOther, readnodeOther, adcAveEvt,
                  evenOddOther, vtGainNormOther, vtGainIndexOther,
                  vtGainOffsetOther, gainOffsetOther, 
                  gainLinrOther, gainQuadOther);
   } 
   else if (actxEvt == 321)  { // event center is in Segment CD
      segmentOther = 0;
      indxOtherSegment = 2 * ccdidEvt;
      readnodeOther = actvnode[indxOtherSegment];
      // note that indx_other segment should be 0, 1, …, or 7. 
      evenOddOther = readnodeOther;
      // If readnode A [or B] is used, actx=320 is ‘even’ [or ‘odd’]. 
      searchEvenOdd(eodat, ccdidEvt, segmentOther, readnodeOther,adcAveEvt, 
                    evenOddOther, vtGainNormOther, vtGainIndexOther, 
                    vtGainOffsetOther, gainOffsetOther, 
                    gainLinrOther, gainQuadOther);
   } 


  // Main correction routine:

  // Calculate gfact for gain correction depending on the video temperature
  if(vtStatus == 0) { // vt is valid
    gfact = 1 + (vtGainNorm*pow(vt-vtGainOffset, vtGainIndex)/eodat.m_keyPHRefEvenOdd) - (vtGainNorm*pow(eodat.m_keyVtRef-vtGainOffset, vtGainIndex)/eodat.m_keyPHRefEvenOdd);
    gfactSide = 1 + vtGainNormSide*pow(vt-vtGainOffsetSide, vtGainIndexSide)/eodat.m_keyPHRefEvenOdd - vtGainNormSide*pow(eodat.m_keyVtRef-vtGainOffsetSide, vtGainIndexSide)/eodat.m_keyPHRefEvenOdd;

    if (actxEvt == 320 || actxEvt == 321) { //for segment boundary events
      gfactOther = 1 + (vtGainNormOther*pow(vt-vtGainOffsetOther,vtGainIndexOther)/eodat.m_keyPHRefEvenOdd) -(vtGainNormOther*pow(eodat.m_keyVtRef-vtGainOffsetOther,vtGainIndexOther)/eodat.m_keyPHRefEvenOdd);
    }

    AH_DEBUG << "vtGainNorm = " << vtGainNorm << std::endl;
    AH_DEBUG << "vt = " << vt << std::endl; 
    AH_DEBUG << "vtGainOffset = " << vtGainOffset << std::endl; 
    AH_DEBUG << "vtGainIndex = " << vtGainIndex << std::endl;
    AH_DEBUG << "eodat.m_keyPHRefEvenOdd = " << eodat.m_keyPHRefEvenOdd << std::endl;
    AH_DEBUG << "vtGainNormSide = " << vtGainNormSide << std::endl;
    AH_DEBUG << "eodat.m_keyVtRef = " << eodat.m_keyVtRef << std::endl; 
    AH_DEBUG << "vtGainOffsetSide = " << vtGainOffsetSide << std::endl;
    AH_DEBUG << "vtGainIndexSide = " << vtGainIndexSide << std::endl;
  }
  if(gfact < 0 || gfact > 1) gfact = 1;
  if(gfactSide < 0 || gfactSide > 1) gfactSide = 1;

  if(vtStatus == 1) { // vt is invalid
    gfact = 1;
    gfactSide = 1;
    statusEvt[26] = 1;
  }
  if(vtStatus == -1) { // no HK time is found within deltatime from timeEvt
    gfact = 1;
    gfactSide = 1;
    statusEvt[27] = 1;
  }

  AH_DEBUG << "gfact = " << gfact << std::endl;
  AH_DEBUG << "gainOffset = "<< gainOffset << std::endl;
  AH_DEBUG << "gainLinr = " << gainLinr << std::endl;
  AH_DEBUG << "gainQuad = " << gainQuad << std::endl;
  AH_DEBUG << "gfactSide = " << gfactSide << std::endl;
  AH_DEBUG << "gainOffsetSide =" << gainOffsetSide << std::endl;
  AH_DEBUG << "gainLinrSide = " << gainLinrSide << std::endl;
  AH_DEBUG << "gainQuadSide = " << gainQuadSide << std::endl;

  // Gain correction (temperature dependence), dividing PHAS by gfact
  for(int ii = 0; ii <= 8; ++ii) {
    if(phasEvenOdd[ii]>0) {
      if(ii == 0 || ii == 2 || ii == 7) {
        phasEvenOdd[ii] = phasEvenOdd[ii]/gfact;
        phasEvenOdd[ii] = gainOffset+gainLinr*phasEvenOdd[ii]+gainQuad*pow(phasEvenOdd[ii],2);
      } else if (actxEvt == 320 || actxEvt == 321) { // For an event on the segment boundary….
        if ((readnodeEvt == 0 && (ii==1 || ii==4 || ii==6)) || 
            (readnodeEvt == 1 && (ii==3 || ii==5 || ii==8)) ) { // Pixels in same segment as center
          phasEvenOdd[ii] = phasEvenOdd[ii] / gfactSide;
          phasEvenOdd[ii] = gainOffsetSide + gainLinrSide * phasEvenOdd[ii] 
                            + gainQuadSide * pow(phasEvenOdd[ii],2);
        } else { // Pixels fall in the other segment from center
          phasEvenOdd[ii] = phasEvenOdd[ii] / gfactOther;
          phasEvenOdd[ii] = gainOffsetOther+ gainLinrOther * phasEvenOdd[ii] 
                            + gainQuadOther * pow(phasEvenOdd[ii],2);
        }

      } else {
        phasEvenOdd[ii] = phasEvenOdd[ii]/gfactSide;
        phasEvenOdd[ii] = gainOffsetSide+gainLinrSide*phasEvenOdd[ii]+gainQuadSide*pow(phasEvenOdd[ii],2);
      }
    }
  } // end for loop

} // end correctEvenOdd

// ****************************************************************************

void correctChargeTrail(double * phasTmp, int rawxEvt, int actxEvt, int actyEvt, 
                        char ccdidEvt, char segmentEvt, char readnodeEvt, 
                        char * statusEvt, int huclegth, int vucheght, int imgheght,
                        int ciStatus, int ciPeriod, int ciOffset, int ciFirst,
                        int chTrailCor, const sxipilib::ChargeTrailData& chtrdat,
                        double * phasChTrail, int & errorStatus) {
  AH_DEBUG << "Start correctChargeTrail " << std::endl;

  double normH = 0;         // Power-law normalization for horizontal trail correction
  double normV = 0;         // Power-law normalization for vertical trail correction
  double indexH = 0;        // Power-law index for horizontal trail correction
  double indexV = 0;        // Power-law index for vertical trail  correction
  double offsetH = 0;       // Offset parameter value for horizontal trail correction
  double offsetV = 0;       // Offset parameter value for vertical trail correction

  double trailRateH = 0.;   // Rate of horizontal trailed charge
  double trailFracH = 0.;   // Fraction of horizontal trailed PH to untrailed (original PH)
  double trailRateV = 0.;   // Rate of vertical trailed charge
  double trailFracV = 0.;   // Fraction of vertical trailed PH to untrailed (original PH)

  double ph_cut = 0.0;      // pulse height cutoff
  double eslope = 0.0;      // energy slope
  double eoffset = 0.0;     // energy offset
 
  double phTrail = 0.;      //photon trail

  double phTrailH = 0.;     // Amount of horizontal trailed charge
  double phTrailV = 0.;     // Amount of vertical trailed charge

  errorStatus = 0;          // Error status. 0: OK; 1: Continue, next row; 2: Stop program
  bool cirow = false;       // if cirow is true then we return to doWork 

  for(int ii = 0; ii < LENPHAS; ++ii) phasChTrail[ii] = phasTmp[ii];

  if(chTrailCor == false) return;

  // if event is on a CI row, skip this subroutine
  if (ciStatus == 1) {
    if (actyEvt >= (ciFirst + ciOffset) && (actyEvt-ciFirst-ciOffset) % ciPeriod == 0) {
      cirow = true;
      AH_DEBUG << "Event is on a CI row, skipping charge trail." << std::endl;
    }
  }
  if (cirow) return;

  // Get CALDB values
  searchChargeTrail(chtrdat, ccdidEvt, segmentEvt, readnodeEvt, normH, normV,
                    indexH, indexV, offsetH, offsetV, ph_cut, eslope, eoffset);

  AH_DEBUG << "normH = " << normH << std::endl;
  AH_DEBUG << "normV = " << normV << std::endl;
  AH_DEBUG << "indexH = " << indexH << std::endl;
  AH_DEBUG << "indexV = " << indexV << std::endl;
  AH_DEBUG << "offsetH = " << offsetH << std::endl;
  AH_DEBUG << "offsetV = " << offsetV << std::endl;
  AH_DEBUG << "ph_cut = " << ph_cut << std::endl;

  // Main correction routine
  // Correction for CT from the preceding pixels to the center pixel
  // Calculate the amount of trailed charge
  // Horizontal component, trail from pixel 4 to pixel 0
  // The horizontal trail correction is calculated even if the PHAS is negative EDM 20160712
  if (phasChTrail[4] >= ph_cut) {
    phTrail = phasChTrail[4];
  } else {
    phTrail = ph_cut;
  }
  trailRateH = normH*pow(phTrail/chtrdat.m_keyPHRefChTrail, indexH);
  // Calculate th fraction of trailed PH to untrailed (original) PH
  trailFracH = trailRateH*(rawxEvt+huclegth+offsetH);
  AH_DEBUG << " phasChTrail[4] is greater than zero, calculating 0->4 trail = " << std::endl;
  AH_DEBUG << " phTrail = " << phTrail << std::endl;
  AH_DEBUG << " keyPHRef = " << chtrdat.m_keyPHRefChTrail << std::endl;
  AH_DEBUG << " rawxEvt = " << rawxEvt << std::endl;
  AH_DEBUG << " huclegth = " << huclegth << std::endl;
  AH_DEBUG << " trailRateH = " << trailRateH << std::endl;

  // Amount of trailed charge
  phTrailH = phasChTrail[4]*trailFracH;

  // If the event is on the segment boundary and pixel 4 is in the different segment from 
  // pixel 0, then the trail correction shouldn’t be performed; 
  // this is true if the the event is on segAB and readnode is B (SEGMENT=0, READNODE=1),
  // or if the event is on segCD and readnode is C (SEGMENT=1, READNODE=1). 
  // Here actxevt==320 means it’s on segAB on the boundary, and actxevt==321 means
  // it’s on segBC on the boundary.
  if ((actxEvt == 320 || actxEvt == 321) && readnodeEvt == 1) { 
    AH_DEBUG << "Event on segment boundary, so pix4->pix0 chtrail not performed." << std::endl;
    phTrailH = 0;
  }

  AH_DEBUG << "trailFracH = " << trailFracH << std::endl;
  AH_DEBUG << "phTrailH = " << phTrailH << std::endl;

  // Vertical component, trail from pixel 2 into pixel 0.
  // The vertical trail correction is calculated even if the PHAS is negative.	EDM 20160712
  if (phasChTrail[2] >= ph_cut) {
    phTrail = phasChTrail[2];
  } else {
    phTrail = ph_cut;
  }
  trailRateV = normV*pow(phTrail/chtrdat.m_keyPHRefChTrail, indexV);
  trailFracV = trailRateV*(actyEvt-1.0+imgheght+offsetV);
  AH_DEBUG << " phasChTrail[2] is greater than zero, calculating 2->0 trail = " << std::endl;
  AH_DEBUG << " phTrail = " << phTrail << std::endl;
  AH_DEBUG << " keyPHRef = " << chtrdat.m_keyPHRefChTrail << std::endl;
  AH_DEBUG << " actyEvt = " << actyEvt << std::endl;
  AH_DEBUG << " imgheght = " << imgheght << std::endl;
  AH_DEBUG << " trailRateV = " << trailRateH << std::endl;
  
  phTrailV = phasChTrail[2]*trailFracV;

  // If trailed PH is negative, set status and set PH trail to zero
  if(phTrailH < 0.0) {
    // Small negative PHAS is expected as normal, error status should remain zero.
    phTrailH = 0;
    statusEvt[28] = 1;
  }
  if(phTrailV < 0.0) {
    // Small negative PHAS is expected as normal, error status should remain zero.
    phTrailV = 0;
    statusEvt[28] = 1;
  }

  AH_DEBUG << "trailFracV = " << trailFracV << std::endl;
  AH_DEBUG << "phTrailV = " << phTrailV << std::endl;

  // Correct charge trail
  // Correction is applied to preceding pixels 2 and 4 even if they have negative PH
  // EDM 20160712
  phasChTrail[4] = phasChTrail[4] + phTrailH;
  phasChTrail[2] = phasChTrail[2] + phTrailV;
  if(phasChTrail[0] > 0) phasChTrail[0] = phasChTrail[0] - phTrailH - phTrailV;

  // Correction for CT from the center pixel to the following pixel
  if(phasChTrail[0] > 0.0) {
    if (phasChTrail[0] >= ph_cut) { 
      phTrail = phasChTrail[0];
    } else {
      phTrail = ph_cut;
    }
    trailRateH = normH*pow(phTrail/chtrdat.m_keyPHRefChTrail, indexH);
    trailFracH = trailRateH*(rawxEvt+1.0+huclegth+offsetH);
    trailRateV = normV*pow(phTrail/chtrdat.m_keyPHRefChTrail, indexV);
    trailFracV = trailRateV*(actyEvt+imgheght+offsetV);
  } else {
    trailFracH = 0.0;
    trailFracV = 0.0;
  }
  phTrailH = phasChTrail[0]*trailFracH;
  phTrailV = phasChTrail[0]*trailFracV;
  AH_DEBUG << "(center) phTrailH = " << phTrailH << std::endl;
  AH_DEBUG << "(center) phTrailV = " << phTrailV << std::endl;

  // If trailed PH is negative, set status and set PH trail to zero
  if(phTrailH < 0.0) {
    // Small negative PHAS is expected as normal, error status should remain zero.
    phTrailH = 0;
    statusEvt[28] = 1;
  }
  if(phTrailV < 0.0) {
    // Small negative PHAS is expected as normal, error status should remain zero.
    phTrailV = 0;
    statusEvt[28] = 1;
  }

  // Correct charge trail; pixel 0 is always corrected here regardless of segment boundary
  if(phasChTrail[0] > 0) phasChTrail[0] = phasChTrail[0] + phTrailH + phTrailV;
 
  // If the event is on the segment boundary and pixel 5 is in the different segment from 
  // pixel 0, then the trail correction on pixel 5shouldn’t be performed; this is true 
  // if the the event is on segAB and readnode is A (SEGMENT=0, READNODE=0),
  // or if the event is on segCD and readnode is D (SEGMENT=1, READNODE=0). 
  // Here actxevt==320 means it’s on segAB on the boundary, and actxevt==321 means
  // it’s on segBC on the boundary.
  if ((actxEvt == 320 || actxEvt == 321) && readnodeEvt == 0) {
    AH_DEBUG << "Event on segment boundary, so pix0->pix5 chtrail not performed." << std::endl;
    phTrailH = 0;
  } else if (phasTmp[0] > 0) { 
    phasChTrail[5] = phasChTrail[5] + (eslope * phasTmp[0]) + eoffset;
  }

  // Succeeding (trailing) pixels 5 and 7 are corrected even if they have negative PH
  // EDM 20160712
  phasChTrail[5] = phasChTrail[5] - phTrailH;
  phasChTrail[7] = phasChTrail[7] - phTrailV;

  AH_DEBUG << "End correctChargeTrail " << std::endl;

} // end correctChargeTrail

// ****************************************************************************

void correctCTI(double * phasTmp, int gradeEvt, double phaEvt, int rawxEvt, int actxEvt, 
                int actyEvt, const sxipilib::Params& param, 
                char ccdidEvt, char segmentEvt, char readnodeEvt, const int * actvnode, int huclegth,
                int vucheght, int imgheght, int winSt, int winSize, 
                int ciStatus, int ciPeriod, int ciOffset, int ciFirst, 
                int ctiCor, const sxipilib::CTIData& ctidat, 
                const sxipilib::AnomRegData & AnomRegData, 
                double * phasCTI, int & errorStatus) {
  AH_DEBUG << "Start correctCTI." << std::endl;


  // Calculate loss of charge per transfer 
  // The fast transfer is the charge that is transferred from imaging area to
  // the frame store area
  // The slow transfer is the charge that is transferred from the frame store
  // to the serial readout register
  // The serial transfer is the charge that is transferred from the serial
  // register to the readout node


  vecDbl paraFNorm;       // Normalization for fast parallel CTI
  vecDbl paraFProb;       // Probability for fast parallel CTI
  vecDbl paraFScale;      // Scale factor for fast parallel CTI
  vecDbl paraFIndex;      // Power-law index for fast parallel CTI
  vecDbl paraSNorm;       // Normalization for slow parallel CTI
  vecDbl paraSProb;       // Probability for slow parallel CTI
  vecDbl paraSScale;      // Scale factor for slow parallel CTI
  vecDbl paraSIndex;      // Power-law index for slow parallel CTI
  vecDbl paraANorm;       // Normalization for anomaly parallel CTI
  vecDbl paraAProb;       // Probability for anomaly parallel CTI
  vecDbl paraAScale;      // Scale factor for anomaly parallel CTI
  vecDbl paraAIndex;      // Power-law index for anomaly parallel CTI
  vecDbl gfactor;
  vecDbl goffset_al0; 
  vecDbl goffset_al1;
  vecDbl goffset_al2;
  vecDbl goffset_al3;
  vecDbl goffset_am0;
  vecDbl goffset_am1; 
  vecDbl goffset_am2; 
  vecDbl goffset_am3; 
  vecDbl goffset_ah0; 
  double goffset_ebl = 0;
  double goffset_ebh = 0;
  double ph_cut = 0.;

  vecDbl paraFNormOther; 
  vecDbl paraFProbOther; 
  vecDbl paraFScaleOther;
  vecDbl paraFIndexOther;
  vecDbl paraSNormOther; 
  vecDbl paraSProbOther; 
  vecDbl paraSScaleOther;
  vecDbl paraSIndexOther;
  vecDbl paraANormOther; 
  vecDbl paraAProbOther; 
  vecDbl paraAScaleOther; 
  vecDbl paraAIndexOther; 
  vecDbl gfactorOther;
  vecDbl goffset_al0_other; 
  vecDbl goffset_al1_other;
  vecDbl goffset_al2_other;
  vecDbl goffset_al3_other;
  vecDbl goffset_am0_other;
  vecDbl goffset_am1_other; 
  vecDbl goffset_am2_other; 
  vecDbl goffset_am3_other; 
  vecDbl goffset_ah0_other; 
  double goffset_ebl_other = 0.;
  double goffset_ebh_other = 0.;
  double ph_cut_other = 0.;
  
  vecInt anomaly_min;
  vecInt anomaly_max;
  anomaly_min.resize(3,0);
  anomaly_max.resize(3,0);
  int anomaly_min_pix = 0; 
  int anomaly_max_pix = 0; 

  double para_f_norm_pix = 0.;
  double para_f_prob_pix = 0.;
  double para_f_scale_pix = 0.;
  double para_f_index_pix = 0.;
  double para_s_norm_pix = 0.;
  double para_s_prob_pix = 0.;
  double para_s_scale_pix = 0.;
  double para_s_index_pix = 0.;
  double para_a_norm_pix = 0.;
  double para_a_prob_pix = 0.;
  double para_a_scale_pix = 0.;
  double para_a_index_pix = 0.;

  
  double gfactor_evt = 0.;
  double goffset_evt = 0.; 
  int g_indx = 0;

  double cti_fp0 = 0.;         //CTI due to fast transfer p0 mean probabilty set to zero
  double cti_f = 0.;           //CTI due to fast transfer
  double cti_sp0 = 0.;         //CTI due to slow transfer p0 mean probabilty set to zero
  double cti_s = 0.;           //CTI due to slow transfer
  double cti_ap0 = 0.;         //CTI due to anomaly region transfer p0 mean probabilty set to zero
  double cti_a = 0.;           //CTI due to anomaly region transfer
 
  double cti_fact;             //CTI correction factor

  double ci_acty_in_first = 0.;
  double ci_acty_in_last = 0.;

  double delta_y_anom_max = 0.; 
  int size_anom_acty = 0;

  int n_ci = 0;   

  int ci_acty_pre = 0;
  int ci_acty_post = 0;

  double ph_cti = 0.; 


  //Exponents in each CTI term
  double Y0 = 0.;
  double Y1 = 0.;        
  double Y2 = 0.; 
  double Y3 = 0.; 
  double Y4 = 0.; 

 
  int rawx = 0;
  int acty = 0;
  int actx = 0;          
  int deltaY = 0;             // Distance between event row and nearest CI row

  int segmentOther = 0;
  int indxOtherSegment = 0;
  int readnodeOther = 0;
  int rawxOther = 0;
 

  // Define RAWX/RAWY offset of each pixel position in 3x3 PHAS array
  int offsetRawX[LENPHAS] = { 0, -1, 0, 1, -1, 1, -1, 0, 1 };
  int offsetActY[LENPHAS] = { 0, -1, -1, -1, 0, 0, 1, 1, 1 };
  int offsetActX[LENPHAS];
  if (readnodeEvt == segmentEvt) {
    //READNODE==0 (A) for Segment AB, READNODE==1 (C) for Segment CD
    //ACTX is in same sense as RAWX so offsets are the same
    offsetActX[0] = 0;
    offsetActX[1] = -1;
    offsetActX[2] = 0;
    offsetActX[3] = 1;
    offsetActX[4] = -1;
    offsetActX[5] = 1;
    offsetActX[6] = -1;
    offsetActX[7] = 0;
    offsetActX[8] = 1;
  } else {
    // READNODE==1 (B) for Segment AB, READNODE==0 (D) for Segment CD
    // ACTX is in flipped sense from RAWX so offsets are flipped
    offsetActX[0] = 0;
    offsetActX[1] = 1;
    offsetActX[2] = 0;
    offsetActX[3] = -1;
    offsetActX[4] = 1;
    offsetActX[5] = -1;
    offsetActX[6] = 1;
    offsetActX[7] = 0;
    offsetActX[8] = -1;    
  }
 

  errorStatus = 0;            // Error status. 0: OK; 1: Continue, next row; 2: Stop program

  for(int ii = 0; ii < LENPHAS; ++ii) phasCTI[ii] = phasTmp[ii];

  // Check if CTI correction is on
  if(ctiCor == false) return;

  // Get CTI CALDB values
  searchCTI(ctidat,ccdidEvt,segmentEvt,readnodeEvt,
            paraFNorm,paraFProb,paraFScale,paraFIndex,
            paraSNorm,paraSProb,paraSScale,paraSIndex,
            paraANorm,paraAProb,paraAScale,paraAIndex,
            gfactor,
            goffset_al0,goffset_al1,goffset_al2,goffset_al3,
            goffset_am0,goffset_am1,goffset_am2,goffset_am3,
            goffset_ah0,goffset_ebl,goffset_ebh,ph_cut);

  // If the event is on a segment boundary, we need the CALDB parameters from the other 
  // segment to use on the pixel in that segment.   We only need the “para_*_*” params,
  // not the gfactor, goffset, ph_cut, but they come along for the ride.		EDM 20151026
  if (actxEvt==320 || actxEvt==321) {
    if (actxEvt == 320) {			// event center is in Segment AB
      segmentOther = 1;
      indxOtherSegment = 2 * ccdidEvt + 1;
    } else {					// event center is in Segment CD
      segmentOther = 0;
      indxOtherSegment = 2 * ccdidEvt;
    }
    readnodeOther = actvnode[indxOtherSegment];  // indx_other_segment = 0, 1, …, 7. 
    //std::cout << "calling searchCTI operation on *****Other" << std::endl;
    searchCTI(ctidat,ccdidEvt,segmentOther,readnodeOther,
              paraFNormOther,paraFProbOther,paraFScaleOther,paraFIndexOther,
              paraSNormOther,paraSProbOther,paraSScaleOther,paraSIndexOther,
              paraANormOther,paraAProbOther,paraAScaleOther,paraAIndexOther,
              gfactorOther,
              goffset_al0_other,goffset_al1_other,goffset_al2_other, goffset_al3_other,
              goffset_am0_other,goffset_am1_other, goffset_am2_other,goffset_am3_other,
              goffset_ah0_other,goffset_ebl_other,goffset_ebh_other,ph_cut_other);
  }

  //Get Anomaly region CALDB values
  searchAnomReg(AnomRegData,ccdidEvt,actxEvt,anomaly_min,anomaly_max);

  
  AH_DEBUG << "CCD_ID " << int(ccdidEvt) << std::endl; 
  AH_DEBUG << "SEGMENT " << int(segmentEvt) << std::endl; 
  AH_DEBUG << "READNODE " << int(readnodeEvt) << std::endl; 
  AH_DEBUG << "SEGMENT (other) " << int(segmentOther) << std::endl; 
  AH_DEBUG << "READNODE (other) " << int(readnodeOther) << std::endl; 


  //Set ph_cut to the parameter value if the parameter phcut is not CALDB
  if (ahgen::strtoupper(param.m_phcut) != "CALDB") {
    ph_cut = atof(param.m_phcut.c_str());
  }
  
  //Set GFACTOR=1 and GOFFSET=0 values by default, will be used if ctigrade="no".
  gfactor_evt = 1.;
  goffset_evt = 0.; 

  // If ctigrade="yes" set them according to the GRADE and PHA values, and CALDB 
  // information which provides the energy-dependent polynomial coefficients.
  if (param.m_ctigrade) {
    if (gradeEvt <= 1) { 
      g_indx = 0;
    } else if (gradeEvt == 2) { 
      g_indx = 1; 
    } else if (gradeEvt>=3 && gradeEvt<=5) {
      g_indx = 2;
    } else {
      g_indx = 3;
    }

    // GFACTOR is just function of grade
    //std::cout << "Determine gfactor goffset" << std::endl;
    gfactor_evt = gfactor[g_indx];
  
    // GOFFSET Case 1: PHA is less than lower bound
    if (phaEvt <= goffset_ebl) {
      AH_DEBUG << "GOFFSET Case 1 found" << std::endl; 
      AH_DEBUG << "phaEVT = " << phaEvt << std::endl; 
      AH_DEBUG << "goffset_al0 = " << goffset_al0[g_indx] << std::endl; 
      AH_DEBUG << "goffset_al1 = " << goffset_al1[g_indx] << std::endl; 
      AH_DEBUG << "goffset_al2 = " << goffset_al2[g_indx] << std::endl; 
      AH_DEBUG << "goffset_al3 = " << goffset_al3[g_indx] << std::endl; 
      goffset_evt = goffset_al0[g_indx]
                    + (goffset_al1[g_indx] * phaEvt)
                    + (goffset_al2[g_indx] * pow(phaEvt,2.0))
                    + (goffset_al3[g_indx] * pow(phaEvt,3.0));
    //GOFFSET Case 2: PHA is between lower and upper bound
    } else if (goffset_ebl < phaEvt && phaEvt <= goffset_ebh) {
      AH_DEBUG << "GOFFSET Case 2 found" << std::endl; 
      AH_DEBUG << "phaEVT = " << phaEvt << std::endl; 
      AH_DEBUG << "goffset_am0 = " << goffset_am0[g_indx] << std::endl; 
      AH_DEBUG << "goffset_am1 = " << goffset_am1[g_indx] << std::endl; 
      AH_DEBUG << "goffset_am2 = " << goffset_am2[g_indx] << std::endl; 
      AH_DEBUG << "goffset_am3 = " << goffset_am3[g_indx] << std::endl; 
      goffset_evt = goffset_am0[g_indx]
                    + (goffset_am1[g_indx] * phaEvt)
                    + (goffset_am2[g_indx] * pow(phaEvt,2))
                    + (goffset_am3[g_indx] * pow(phaEvt,3));
    // GOFFSET Case 3: PHA is above upper bound
    } else if (goffset_ebh < phaEvt) {
      AH_DEBUG << "GOFFSET Case 3 found" << std::endl; 
      AH_DEBUG << "goffset_ah0 = " << goffset_ah0[g_indx] << std::endl; 
      goffset_evt =   goffset_ah0[g_indx];
    }
  }

  AH_DEBUG << "gfactor_evt = " << gfactor_evt << std::endl; 
  AH_DEBUG << "goffset_evt = " << goffset_evt << std::endl; 
  AH_DEBUG << "rawxEvt = " << rawxEvt << std::endl; 
  AH_DEBUG << "actxEvt = " << actxEvt << std::endl; 
  AH_DEBUG << "actyEvt = " << actyEvt << std::endl; 

  // Main pixel-by-pixel correction routine
  //std::cout << "Beging main correction routine" << std::endl;
  for(int ii = 0; ii <= 8; ++ii) { // loop through the 3x3 pixels
    //std::cout << "ii = " << ii << std::endl;
    if (phasCTI[ii] > 0.0) { // only apply correction to positive PH values

      // Get the RAWX, ACTY, and delta y of the pixel
      //std::cout << "Get the RAWX, ACTY, and delta y of the pixel" << std::endl;
      rawx = rawxEvt + offsetRawX[ii];
      acty = actyEvt + offsetActY[ii];
      actx = actxEvt + offsetActX[ii];

      AH_DEBUG << "Calculating CTI parameters for pixel " << ii << std::endl; 
      AH_DEBUG << "  rawx = " << rawx << std::endl; 
      AH_DEBUG << "  actx = " << actx << std::endl; 
      AH_DEBUG << "  acty = " << acty << std::endl; 

      //if (readnodeEvt == segmentEvt) {  
      //  //READNODE ==0 (A) for Segment AB, READNODE==1 (C) for Segment CD 
      //  actx = actxEvt + offsetRawX[ii];
      //} else {
      //   //READNODE==1 (B) for Segment AB READNODE==0 (D) for segemnt CD
      //   actx = actxEvt - offsetRawX[ii];
      //}

      if (ciStatus == 0) { 
        deltaY = 0; 
      } else if (ciStatus == 1) { 
        // Calculate delta_y, distance in ACTY between pixel and previous CI row.
        // CIFIRST is the first injected row; CIOFFSET is the first repeating
        // injected row; CIPERIOD is the number of rows between successive CI rows.
        // So if ACTY of the pixel is less than CIOFFSET, use the ACT distance to
        // the CIFIRST.  delta_y is calculated later for each pixel because this
        // depends on ACTY offset from the center pixel.
        if (acty < ciOffset) {
          deltaY = acty - ciFirst;
        } else {
          deltaY = (acty - ciOffset) % ciPeriod;
        }
      } else {
        errorStatus = 1;
      } // end if CiStatus

      AH_DEBUG << "  deltaY = " << deltaY << std::endl; 

      // Extract the correct CALDB parameters for this pixel's RAWX column
      // from the arrays.
      //std::cout << "Extract the correct CALDB parameters for this pixel's RAWX column from the arrays." << std::endl;
      if (actx < 1 || actx > 640) {
        // First, if the pixel is off of the CCD, then set the CTI correction to zero, otherwise
        // the CALDB arrays will be out of range.
        para_f_norm_pix = 0.;
        para_f_prob_pix = 0.;
        para_f_scale_pix = 1.;
        para_f_index_pix = 0.;
        para_s_norm_pix = 0.;
        para_s_prob_pix = 0.;
        para_s_scale_pix = 1.;
        para_s_index_pix = 0.;
        para_a_norm_pix = 0.;
        para_a_prob_pix = 0.;
        para_a_scale_pix = 1.;
        para_a_index_pix = 0.;
        anomaly_min_pix = 0;
        anomaly_max_pix = 0;
        AH_DEBUG << "  Pixel is outside the allowed actx, using dummy CTI params" << std::endl; 
      } else if ((actxEvt == 320 && actx == 321) ||
                 (actxEvt == 321 && actx == 320)) {
        // If the pixel is on the CCD but in a different segment from central pixel, use the 
        // CALDB values from the other segment.  This is the case for an event on the 
        // segment boundary.
        //
        // Figure out the rawx for the pixel, which we need to get from actx and the
        // segment, readnode of the other segment
        if (actx==321) {
             if (segmentOther == readnodeOther) {
                  rawxOther = 0;
             } else {
                  rawxOther = 319;
             }
        } else if (actx==320) {
             if (segmentOther == readnodeOther) {
                  rawxOther = 319;
             } else {
                  rawxOther = 0;
             }
        }
        para_f_norm_pix = paraFNormOther[rawxOther];
        para_f_prob_pix = paraFProbOther[rawxOther];
        para_f_scale_pix = paraFScaleOther[rawxOther];
        para_f_index_pix = paraFIndexOther[rawxOther];
        para_s_norm_pix = paraSNormOther[rawxOther];
        para_s_prob_pix = paraSProbOther[rawxOther];
        para_s_scale_pix = paraSScaleOther[rawxOther];
        para_s_index_pix = paraSIndexOther[rawxOther];
        para_a_norm_pix = paraANormOther[rawxOther];
        para_a_prob_pix = paraAProbOther[rawxOther];
        para_a_scale_pix = paraAScaleOther[rawxOther];
        para_a_index_pix = paraAIndexOther[rawxOther];
        //std::cout << "anomalymin/max" << std::endl;
        anomaly_min_pix = anomaly_min[offsetActX[ii]+1];
        anomaly_max_pix = anomaly_max[offsetActX[ii]+1];
        //std::cout << "done" << std::endl;
        AH_DEBUG << "  Pixel is on the other segment from event center, using other CTI params" << std::endl; 
        AH_DEBUG << "    rawxOther = " << rawxOther << std::endl;
      } else if (rawx >= 0 && rawx <= 319) { 
        //std::cout << "normal case" << std::endl;
        para_f_norm_pix = paraFNorm[rawx];
        para_f_prob_pix = paraFProb[rawx];
        para_f_scale_pix = paraFScale[rawx];
        para_f_index_pix = paraFIndex[rawx];
        para_s_norm_pix = paraSNorm[rawx];
        para_s_prob_pix = paraSProb[rawx];
        para_s_scale_pix = paraSScale[rawx];
        para_s_index_pix = paraSIndex[rawx];
        para_a_norm_pix = paraANorm[rawx];
        para_a_prob_pix = paraAProb[rawx];
        para_a_scale_pix = paraAScale[rawx];
        para_a_index_pix = paraAIndex[rawx];
        anomaly_min_pix = anomaly_min[offsetActX[ii]+1];
        anomaly_max_pix = anomaly_max[offsetActX[ii]+1];     
        AH_DEBUG << "  Pixel is on the same segment as event center, using same CTI params" << std::endl; 
      } else {
        AH_ERR << "Pixel RAWX value out of range in CTI correction" << std::endl;
        errorStatus = 2;  
      }
      // Calculate CTI terms, where CTI is defined as the fractional loss of
      // charge per transfer.  If PHAS of the pixel is above PH_CUT, then an
      // energy-dependent (actually PH-dependent) CTI term is calculated.  
      // Otherwise it is energy-independent and PH_CUT is used.
      //std::cout << " Calculate CTI terms" << std::endl;
      if (phasCTI[ii] >= ph_cut) {
        ph_cti = phasCTI[ii];
        AH_DEBUG << "  Pixel PH is " << phasCTI[ii] << ", greater than ph_cut = " << ph_cut << ". Using ph_cti = " << ph_cti << std::endl; 
      } else {
        ph_cti = ph_cut;
        AH_DEBUG << "  Pixel PH is " << phasCTI[ii] << ", less than ph_cut = " << ph_cut << ". Using ph_cti = " << ph_cti << std::endl; 
      }
      // CTI due to fast transfer (when charge is transfered from imaging
      // area to frame store area); "p0" means the probability is set to zero
      cti_fp0 = gfactor_evt * para_f_norm_pix 
                * pow(ph_cti / ctidat.m_keyPHRefCTI, para_f_index_pix);
      cti_f = cti_fp0 * (1.0 - para_f_prob_pix * exp((-1 * deltaY)/para_f_scale_pix));
      // CTI due to slow transfer (when charge is transfered from frame store
      // area to readout node); "p0" means the probability is set to zero
      cti_sp0 = gfactor_evt * para_s_norm_pix 
                * pow(ph_cti / ctidat.m_keyPHRefCTI, para_s_index_pix);
      cti_s = cti_sp0 * (1.0 - para_s_prob_pix * exp((-1 * deltaY)/para_s_scale_pix));
      // CTI due to anomaly transfer (when charge is transfered within 
      // anomalous CTI regions); "p0" means the probability is set to zero
      cti_ap0 = gfactor_evt * para_a_norm_pix 
                * pow(ph_cti / ctidat.m_keyPHRefCTI, para_a_index_pix);
      cti_a = cti_ap0 * (1.0 - para_a_prob_pix * exp((-1 * deltaY)/para_a_scale_pix));

      AH_DEBUG << "  cti_f = " << cti_f << ",  cti_fp0 = " << cti_fp0 << std::endl; 
      AH_DEBUG << "    para_f_norm_pix = " << para_f_norm_pix << std::endl;
      AH_DEBUG << "    para_f_prob_pix = " << para_f_prob_pix << std::endl;
      AH_DEBUG << "    para_f_scale_pix = " << para_f_scale_pix << std::endl;
      AH_DEBUG << "    para_f_index_pix = " << para_f_index_pix << std::endl;
      AH_DEBUG << "  cti_s = " << cti_s << ",  cti_sp0 = " << cti_sp0 << std::endl; 
      AH_DEBUG << "    para_s_norm_pix = " << para_s_norm_pix << std::endl;
      AH_DEBUG << "    para_s_prob_pix = " << para_s_prob_pix << std::endl;
      AH_DEBUG << "    para_s_scale_pix = " << para_s_scale_pix << std::endl;
      AH_DEBUG << "    para_s_index_pix = " << para_s_index_pix << std::endl;
      AH_DEBUG << "  cti_a = " << cti_a << ",  cti_ap0 = " << cti_ap0 << std::endl; 
      AH_DEBUG << "    para_a_norm_pix = " << para_a_norm_pix << std::endl;
      AH_DEBUG << "    para_a_prob_pix = " << para_a_prob_pix << std::endl;
      AH_DEBUG << "    para_a_scale_pix = " << para_a_scale_pix << std::endl;
      AH_DEBUG << "    para_a_index_pix = " << para_a_index_pix << std::endl;

      // Calculate the Y0,Y1,Y2,Y3,Y4 which are the exponents in each CTI term.
      // Y1,Y2,Y3 depend on the anomaly region and distance to CI rows.

      // CASE A:  no CTI anomaly region in this column.
      if (anomaly_min_pix == 0 && anomaly_max_pix == 0) {
        AH_DEBUG << "  CASE A: No CTI anomaly in column." << std::endl; 
        Y1 = deltaY;
        Y2 = 0.; 
        Y3 = 0.; 
      } else {
        // For other cases, we need to know if there is a CI row in the anomaly
        // region.  If the delta_acty between ANOMALY_MAX and the nearest
        // previous CI row is greater than delta_acty between ANOMALY_MAX and
        // ANOMALY_MIN, there is no CI row in the anomaly. 
        AH_DEBUG << "  There is an anomaly region in this column." << std::endl;
        AH_DEBUG << "  anomaly_min_pix = " << anomaly_min_pix << ", anomaly_max_pix = " << anomaly_max_pix << std::endl; 
        if (ciStatus == 1) {
          delta_y_anom_max = (anomaly_max_pix - ciOffset) % ciPeriod; 
          size_anom_acty = (anomaly_max_pix-anomaly_min_pix);
           
          //CASE B:  no CI row between ANOMALY_MIN nd ANOMALY_MAX for CI on
          if (delta_y_anom_max >= size_anom_acty) {
            AH_DEBUG << "  CASE B: CTI anomaly in column, but no CI row in anomaly." << std::endl; 
            //Find ACTY of the nearest CI rows before and after the anomaly
            ci_acty_pre = anomaly_max_pix - delta_y_anom_max;
            ci_acty_post = ci_acty_pre + ciPeriod;
            // CASE B.1:  pixel is below/before the anomaly region
            if (acty <= anomaly_min_pix) {
              Y1 = deltaY;
              Y2 = 0.;
              Y3 = 0.;
            // CASE B.2:  pixel is in the anomaly region
            } else if ((anomaly_min_pix < acty) && (acty <= anomaly_max_pix)) {
              Y1 = anomaly_min_pix - ci_acty_pre;
              Y2 = 0.;
              Y3 = acty - anomaly_min_pix;
            // CASE B.3:  pixel is above/after the anomaly region, but before next
            // CI row
            } else if ((anomaly_max_pix < acty) && (acty <= ci_acty_post)) {
              Y1 = (acty - anomaly_max_pix ) + (anomaly_min_pix - ci_acty_pre);
              Y2 = 0.;
              Y3 = anomaly_max_pix - anomaly_min_pix;
            // CASE B.4:  pixel is above/after the anomaly region and the next CI row
            } else if (ci_acty_post < acty) {
              Y1 = deltaY;
              Y2 = anomaly_max_pix - anomaly_min_pix;
              Y3 = 0.;
            } 
            // end CASE B (column has anomaly region, but no CI row in anomaly)
          } else {									
           // CASE C:  there is a CI row(s) between ANOMALY_MIN and _MAX 
           // Determine the number of CI rows in the anomaly.
            n_ci = 1;
            while (1) {
              if (size_anom_acty < delta_y_anom_max + n_ci * ciPeriod)  break;
              n_ci++;
              if (n_ci > 641) {
                AH_THROW_RUNTIME("number of charge injection rows is greater than the number of CCD rows");
              }
            }
            AH_DEBUG << "  CASE C: CTI anomaly in column with " << n_ci << " CI row(s) in anomaly." << std::endl; 
  
            // Find ACTY of the CI row(s) in the anomaly, and the nearest CI rows before
            // and after the anomaly
            ci_acty_in_first = anomaly_max_pix - delta_y_anom_max - (n_ci - 1) * ciPeriod;
            // first (lowest ACTY) CI row in anomaly
            ci_acty_in_last = anomaly_max_pix - delta_y_anom_max; 
            // last (highest ACTY) CI row in anomaly

            // if there is only one CI row, ci_acty_in_first and last are identical.
            ci_acty_pre = ci_acty_in_first - ciPeriod;              // CI row before anomaly
            ci_acty_post = ci_acty_in_last + ciPeriod;              // CI row after anomaly
            // CASE C.1:  pixel is below/before the anomaly region
            if (acty <= anomaly_min_pix) {
              Y1 = deltaY;
              Y2 = 0.;
              Y3 = 0.;
            // CASE C.2:  pixel is in the anomaly region, but below/before the first CI row
            } else if ((anomaly_min_pix < acty) && (acty <= ci_acty_in_first)) {
              Y1 = anomaly_min_pix - ci_acty_pre;
              Y2 = 0.;
              Y3 = acty - anomaly_min_pix;
            // CASE C.3:  pixel is in the anomaly region, but above/after the first CI row
            } else if ((ci_acty_in_first < acty) && (acty <= anomaly_max_pix) ) {
              Y1 = 0.;
              Y2 = acty - deltaY - anomaly_min_pix;
              Y3 = deltaY;
            // CASE C.4:  pixel is above/after the anomaly region, but before next
            // CI row
            } else if ((anomaly_max_pix < acty) && (acty <= ci_acty_post)) {
              Y1 = acty - anomaly_max_pix;
              Y2 = ci_acty_in_last - anomaly_min_pix;
              Y3 = anomaly_max_pix - ci_acty_in_last;
            // CASE C.5:  pixel is above/after the anomaly region and the next CI row
            } else if (ci_acty_post < acty) {
              Y1 = deltaY;
              Y2 = anomaly_max_pix - anomaly_min_pix;
              Y3 = 0.;
            } 
          }// end CASE C (column has anomaly region with CI row(s) in anomaly)
        } else {
          //std::cout << "CASE D" << std::endl;
          //CASE D: CI off data with anomaly region
          //CASE D.1:  pixel is below/before the anomaly region
          AH_DEBUG << "  CASE D: CTI anomaly in column, but CI is off " << std::endl; 
          if (acty <= anomaly_min_pix) {
            Y1 = deltaY;
            Y2 = 0.;
            Y3 = 0.;
          // CASE D.2:  pixel is in the anomaly region
          } else if ((anomaly_min_pix < acty) && (acty <= anomaly_max_pix)) {
            Y1 = anomaly_min_pix; 
            Y2 = 0.;
            Y3 = acty - anomaly_min_pix;
          // CASE D.3:  pixel is above/after the anomaly region
          } else {
            Y1 = acty - anomaly_max_pix + anomaly_min_pix;
            Y2 = 0.;
            Y3 = anomaly_max_pix - anomaly_min_pix;
          } 
        } //end of Case D/ end of if branch
      } // end if block to calculate Y1,Y2,Y3

      // the coefficients Y0 and Y4 are independent of the anomaly region
      // but depend on Y1,Y2,Y3
      Y0 = imgheght + winSt - (Y1 + Y2 + Y3); 
      Y4 = acty + vucheght - 1; 

      // calculate CTI correction factor from the above coefficients
      // each factor is a value slightly less than 1, so cti_fact is also a value less than 1   
      cti_fact   =  pow(1.0 - cti_f,Y0) 
                    * pow(1.0 - cti_fp0,Y1) 
                    * pow(1.0 - cti_a,Y2)
                    * pow(1.0 - cti_ap0,Y3)
                    * pow(1.0 - cti_s,Y4);

      AH_DEBUG << "  Y0 = " << Y0 << std::endl; 
      AH_DEBUG << "  Y1 = " << Y1 << std::endl; 
      AH_DEBUG << "  Y2 = " << Y2 << std::endl; 
      AH_DEBUG << "  Y3 = " << Y3 << std::endl; 
      AH_DEBUG << "  Y4 = " << Y4 << std::endl; 
      AH_DEBUG << "  cti_fact = " << cti_fact << std::endl; 

      //calculate CTI-corrected PH value for the pixel
      //std::cout << "calculate CTI-corrected PH value for the pixel" << std::endl;
      phasCTI[ii] = phasCTI[ii] / cti_fact; 
   
      //add grade-dependent offset to central pixel PHAS only after all other CTI corrections
      //std::cout << "add grade-dependent offset to central pixel PHAS only after all other CTI corrections" << std::endl;
      if (ii == 0) { 
        phasCTI[ii] = phasCTI[ii] + goffset_evt;
        AH_DEBUG << "  Central pixel, adding goffset_evt = " << goffset_evt << std::endl;
      }

      AH_DEBUG << "Final phasCTI = " << phasCTI[ii] << std::endl;

    } //end if – for zero or negative phas value, just skip the correction
  } // end for loop through event pixels


  AH_DEBUG << "end Correct CTI" << std::endl;
 
} // end correctCTI

// ****************************************************************************

void gradePHA(double * phasTmp, int * phasMask, int badpixopt, short pOuterMostEvt, 
              char ccdidEvt, char segmentEvt, char readnodeEvt, bool spthiter, 
              bool spthcaldb, int spthoff, int spthslope,
              const sxipilib::GradeData& gradedat, const sxipilib::SpThData& spthdat,
              double & phaEvt, int & gradeEvt, double & phaSpTh, int & gradeSpTh,
              int & errorStatus) {
  AH_DEBUG << "Start gradePHA." << std::endl;

  double spthOffset = 0;    // Offset for the split threshold function
  double spthLinr = 0;      // Linear coefficient for the split threshold function
  double spth = 0;          // Split threshold calculation

  errorStatus = 0;          // Error status. 0: OK; 1: Continue, next row; 2: Stop program

  // If one of the 3x3 pixels is bad (PHAS_MASK[i]==1), then check the ‘badpixopt’
  // parameter to see what we should do:
  // badpixopt=1: Ignore the bad status and use the pixel as in the Suzaku case;
  // badpixopt=2: Set the pixel PH value to NULL (use only pixels with PHAS_MASK[i]!=1)
  // badpixopt=3: Set GRADE and PHA to NULL if any of the pixels have PHAS_MASK [i]==1
  // The third option is performed by returning errorstatus=1.

  if (badpixopt == 2) {
    for (int ii=0; ii<=8; ii++) { 
      if (phasMask[ii] == 1) {
        AH_DEBUG << "Pixel " << ii << " has phasMask==1, badpixopt==2, set to -32768" << std::endl;
        phasTmp[ii] = -32768;
      }
    }
  } else if (badpixopt == 3) {
    for (int ii=0; ii<=8; ii++) {
      if (phasMask[ii] == 1) { 
        AH_DEBUG << "Pixel " << ii << " has phasMask==1, badpixopt==3, set all to NULL" << std::endl;
        errorStatus = 1;
        return; 
      }
    }
  }

  // Make sure the central pixel has positive pulse height, otherwise NULL PHA and GRADE
  if (phasTmp[0] < 0) {
     AH_DEBUG << "PHAS[0] < 0, set all to NULL" << std::endl;
     errorStatus = 1;
     return; 
  }

  // Make sure each pixel is not NaN, otherwise NULL PHA and GRADE
  for (int ii=0; ii<=8; ii++) {
    if (std::isnan(phasTmp[ii])) { 
      AH_DEBUG << "Pixel " << ii << " has NaN PHAS_CTICORR" << std::endl;
      errorStatus = 1;
      return; 
    }
  }

  // Define the initial split threshold and the offset and slope to use
  // spthOffset and spthLinr are defined in searchSPTH and only overwritten
  // with parameter file values here if needed
  if(!spthiter) { // Use a constant split threshold (first iteration only)
    if(!spthcaldb) spthOffset = spthoff; // CALDB
    // Constant and linear split threshold read from caldb
    else searchSPTH(spthdat, ccdidEvt, segmentEvt, readnodeEvt, spthOffset, spthLinr);
    AH_DEBUG << "Using contant spth: spthOffset = " << spthOffset << ", spthLinr = " << spthLinr << std::endl;
  } else {                  // use energy-dependent split thereshold (both iterations)
    if(!spthcaldb) {
      spthOffset = spthoff; // Constant split threshold from par file
      spthLinr = spthslope; // Constant split threshold slope from par file
    } else {  
      // Constant and linear split threshold read from caldb
      searchSPTH(spthdat, ccdidEvt, segmentEvt, readnodeEvt, spthOffset, spthLinr);
    }
    AH_DEBUG << "Using energ-dep spth: spthOffset = " << spthOffset << ", spthLinr = " << spthLinr << std::endl;
  }

  // First iteration, use the CALDB split threshold offset as the split threshold
  gradeIter(phasTmp, pOuterMostEvt, gradedat, spthOffset, phaEvt, gradeEvt);
  phaSpTh = phaEvt;
  gradeSpTh = gradeEvt;
  AH_DEBUG << "After Iter 1: phaEvt = " << phaEvt << ", gradeEvt = " << gradeEvt << std::endl;

  // Use constant split threshold if no correction
  if(!spthiter) return;

  // Second iteration, use energy-dependent split threshold
  spth = spthLinr*phaEvt + spthOffset;
  gradeIter(phasTmp, pOuterMostEvt, gradedat, spth, phaEvt, gradeEvt);
  AH_DEBUG << "After Iter 2: phaEvt = " << phaEvt << ", gradeEvt = " << gradeEvt << std::endl;

  AH_DEBUG << "End gradePHA." << std::endl;

} // end gradePHA

// ****************************************************************************

void gradeIter(double * phas, short pOuterMostEvt, const sxipilib::GradeData& gradedat,
               double spth, double & phaEvt, int & gradeEvt) {

  int pattern3x3Evt[LENPHAS];         // 3x3 hit pattern array
  int pattern5x5Evt[16];              // Array of outer pixels for hit pattern
  int grade3x3 = 0;                   // Grade of PHAS
  int bitMask = 0;                    // Bitwise comparison for outer pixels
  int cornerCheck = 0;                // Index value of corner pixel
  int outerCheck = 0;                 // Index value of adjacent corner pixels

  for (int iii=0; iii < sxipilib::LENPHAS; iii++) pattern3x3Evt[iii]=0;
  for (int iii=0; iii < 16; iii++) pattern5x5Evt[iii]=0;

  // Do the 3x3 grading with CALDB info
  // Make a hit pattern (a 9-element string) out of PHAS array
  // For pattern3x3Evt - 1: pixel PH >= split threshold
  //                     2: pixel PH < split threshold
  for(int ii = 0; ii <= 8; ++ii) {
    if(phas[ii] >= spth) {
      pattern3x3Evt[ii] = 1;
    } else {
      pattern3x3Evt[ii] = 2;
    }
  }

  // Get CALDB hit pattern
  searchGradeData(gradedat, pattern3x3Evt, grade3x3);

  // Convert pOuterMostEvt (a 16-bit bitmask in integer form) to pattern5x5Evt
  // (a 16-element string) to make it similar to pattern3x3Evt and the CALDB
  // hit pattern.
  // For pOuterMostEvt - 0: pixel PH < split threshold
  //                     1: pixel PH >= split threshold
  // for pattern5x5Evt - 1: pixel PH >= split threshold
  //                     2: pixel PH < split threshold
  // pOuterMostEvt is a signed short integer, which should work properly
  // with the bitwise & and bit shift operators.
  // EDM 20160809 -- fixed order of P_OUTER_MOST
  // P_OUTER_MOST is a 16-bit integer encoding which outer pixels are
  // above split threshold, with pixel 9 encoded in the least significant
  // bit and pixel 24 in the most.  Since we loop through only the outer
  // 16 pixels, we left shift 1 by the pixel index ii and compare that
  // to P_OUTER_MOST.
  // pattern5x5Evt[16] elements 0=9, 1=10, 2=11, 3=12, 4=13, etc.
  for(int ii = 0; ii < 16; ++ii) {
    bitMask = (pOuterMostEvt & (1 << ii));
    if(bitMask != 0) {
      pattern5x5Evt[ii] = 1;
    } else {
      pattern5x5Evt[ii] = 2;
    }
  }

  // Refined grading with algorithm includes outer 5x5 info and grade 6 corner
  // PH refinement.
  // Final grade set to something illegal for testing
  gradeEvt = -1;
  phaEvt = -1;

  // First 6 is changed to 11 if surrounded corner PH is greater than both
  // neighbor PH for 6 or 11.
  // PHA is sum of central two abutting pixels and surrounded corner pixel
  if(grade3x3 == 6) {

    cornerCheck = 0;

    // If pixel 1 is the surrounded corner, then check pixels 2, 4
    if(checkCorner(pattern3x3Evt, 1, 2, 4, phas) == 1) cornerCheck = 1;
    // If pixel 3 is the surrounded corner, then check pixels 2, 5 
    if(checkCorner(pattern3x3Evt, 3, 2, 5, phas) == 1) cornerCheck = 1;
    // If pixel 6 is the surrounded corner, then check pixels 4, 7
    if(checkCorner(pattern3x3Evt, 6, 4, 7, phas) == 1) cornerCheck = 1;
    // If pixel 8 is the surrounded corner, then check pixels 5, 7 
    if(checkCorner(pattern3x3Evt, 8, 5, 7, phas) == 1) cornerCheck = 1;

    // Set final grade to 11 if any of the above conditions are met
    // Otherwise, don't set the final grade yet
    if(cornerCheck == 1) gradeEvt = 11;
  }

  // 0, 1 are never changed once set, PHA is only the central pixel
  // 2,3,4,5,6,8 are changed to 10 if any of the 3x3 split pixels
  // borders an outer 5x5 split pixel.
  // 7 is kept as 7 if any of the 3x3 split pixels borders an
  // outer 5x5 split pixel. Otherwise, 7 is changed to 9.
  //
  // Must be a left, right, up or down border, not a diagonal
  if(grade3x3 == 0 || grade3x3 == 1) {
    gradeEvt = grade3x3;
    phaEvt = phas[0];
  } else if(grade3x3 == 2 || grade3x3 == 3 || grade3x3 == 4 ||
            grade3x3 == 5 || grade3x3 == 6 || grade3x3 == 8) {

    // Some 6s were changed to 11. These need to be skipped.
    if(gradeEvt != 11) {
      outerCheck = checkOuter(pattern3x3Evt, pattern5x5Evt);
      if(outerCheck == 1) {
        gradeEvt = 10;
      } else {
        gradeEvt = grade3x3;
      }
    }

    // The PHA for 6, 8, 11 and the 10s they make is sum of central two abutting
    // pixels, and surrounded corner pixels.
    if(phas[2] >= spth && phas[4] >= spth) {
      phaEvt = phas[0] + phas[2] + phas[4] + phas[1];
    } else if(phas[2] >= spth && phas[5] >= spth) {
      phaEvt = phas[0] + phas[2] + phas[5] + phas[3];
    } else if(phas[4] >= spth && phas[7] >= spth) {
      phaEvt = phas[0] + phas[4] + phas[7] + phas[6];
    } else if(phas[5] >= spth && phas[7] >= spth) {
      phaEvt = phas[0] + phas[5] + phas[7] + phas[8];
    // The PHA for 2, 3, 4, 5, and the 10s they make is sum of central pixels
    // plus the single abutting split pixel
    } else if(phas[2] >= spth) {
      phaEvt = phas[0] + phas[2];
    } else if(phas[4] >= spth) {
      phaEvt = phas[0] + phas[4];
    } else if(phas[5] >= spth) {
      phaEvt = phas[0] + phas[5];
    } else if(phas[7] >= spth) {
      phaEvt = phas[0] + phas[7];
    }

  } else if(grade3x3 == 7) {
    outerCheck = checkOuter(pattern3x3Evt, pattern5x5Evt);
    if(outerCheck == 1) {
      gradeEvt = 7;
    } else {
      gradeEvt = 9;
    }

    // PHA for 7, 9 is sum of all PHAS[0-8] >= split threshold
    phaEvt = phas[0];
    for(int ii = 1; ii <= 8; ++ii) {
      if(phas[ii] >= spth) {
        phaEvt += phas[ii];
      }
    }


  } // end if grade3x3

  // Any remaining 8 is change to 6 
  if (gradeEvt == 8) {
    gradeEvt = 6; 
  }

  //phaEvt = int(phaEvt);     EDM 20151215 removed per Hiroya email
  if(phaEvt > sxipilib::PHA_TLMAX) phaEvt = sxipilib::PHA_TLMAX;
  if(phaEvt < sxipilib::PHA_TLMIN) phaEvt = sxipilib::PHA_TLMIN;

} // end gradeIter

// ****************************************************************************

int checkCorner(int * pattern3x3Evt, int cornerPixelIndex, int checkPixInd1, 
                int checkPixInd2, double * phas) {

  // Function to check the two abutting neighbors of a corner pixel.
  // Returns 1 if the corner PH is greater than both neighboring
  // PH values. Returns 0 otherwise.

  if(pattern3x3Evt[cornerPixelIndex] == 1) {
    if(pattern3x3Evt[checkPixInd1] == 1 && 
       pattern3x3Evt[checkPixInd2] == 1) {
      if(phas[cornerPixelIndex] > phas[checkPixInd1] &&
         phas[cornerPixelIndex] > phas[checkPixInd2]) {
        return 1;
      }
    }
  }

  return 0;

} // end checkCorner

// ****************************************************************************

int checkOuter(int * pattern3x3Evt, int * pattern5x5Evt) {

  // Function to check the outer 5x5 hit pattern against the 3x3
  // hit pattern. 
  // Check if there are abutting pixels above threshold.
  // Diagonal pixels do not count.
  // Returns 1 if there is any bordering outer pixel, 0 otherwise
  //
  // 20 21 22 23 24  <-- Readout    ^
  // 18  6  7  8 19            |    |
  // 16  4  0  5 17            V    RAWY
  // 14  1  2  3 15                 RAWX -->
  //  9 10 11 12 13
  //
  //  Inner pixels are numbered 1-8 (excluding center)
  //  Outer pixels are numbred 9-24.

  bool pixelDone = false; // Verify if readout is complete
  int pixNum = 0;         // Current pixel in pixel loop
  int innerFlag = 0;      // 1: pixel PH >= split threshold, 2: pixel PH < split threshold
  int pixelIndex = 0;     // Pixel index to check
  int innerToCheck = 0;   // Inner pixel index to check
  int outerToCheck = 0;   // Outer pixel index to check

  // Array of inner pixel indices, excluding center
  int pixels3x3[8] = { 1, 2, 3, 4, 5, 6, 7, 8 }; 

  // For each inner pixel, check their neighbors in the inner pixels
  int toCheck3x3[8][2] = { { 4, 2 }, { -1, -1 }, { 2, 5 }, { -1, -1 }, 
                          { -1, -1 }, { 4, 7 }, { -1, -1 }, { 7, 5 } };

  // For each inner pixel, check their neighbors in the outer pixels
  int toCheck5x5[8][2] = { { 10, 14 }, { 11, -1 }, { 12, 15 }, { 16, -1 }, 
                          { 17, -1 }, { 18, 21 }, { 22, -1 }, { 19, 23 } };

  // Loop through the inner pixels, excluding center
  // Pixel check
  for(int ii = 0; ii <= 7; ++ii) {
    pixelDone = false;
    pixNum = pixels3x3[ii]; // current pixel

    // Inner flag is the flag of the current inner pixel:
    // 1: pixel PH >= split threshold
    // 2: pixel PH < split threshold
    innerFlag = pattern3x3Evt[pixNum];

    // Skip to next pixel if this one is below split threshold
    if(innerFlag != 1) continue;

    // Skip if this is a corner pixel that is not surrounded
    // (Must have directly abutting neighbors on both sides).
    // First check if corner
    if( pixNum == 1 || pixNum == 3 || pixNum == 6 || pixNum == 8 ) {
      // Now check if it is surrounded. tocheck3x3 tells which 3x3
      // neighbors to check.
      // Inner check
      for(int jj = 0; jj < 2; ++jj) {
        pixelIndex = toCheck3x3[ii][jj];
        if(pixelIndex < 0) continue;

        // innerToCheck is the flag of the current inner pixel to be checked
        // 1: pixel PH >= split threshold
        // 2: pixel PH < split threshold
        innerToCheck = pattern3x3Evt[pixelIndex];

        // If the neighbor is below threshold, move on to the next pixel
        if(innerToCheck != 1) {
          // next pixel
          pixelDone = true;
          break;
        }
      } 

      if(pixelDone) continue; // go to next pixel

    } // end loop on inner pixels

    // Check outer
    for(int jj = 0; jj < 2; ++jj) {

      pixelIndex = toCheck5x5[ii][jj];
      if(pixelIndex < 0) continue;

      // outerToCheck is the flag of the current inner pixel to be checked
      // 1: pixel PH >= split threshold
      // 2: pixel PH < split threshold
      outerToCheck = pattern5x5Evt[pixelIndex-9];
      if(outerToCheck == 1) return 1;

    } // end loop on outer pixels

  } // end loop on all pixels

  return 0;

} // end checkOuter

// ****************************************************************************

void correctGain(double phaEvt, char ccdidEvt, char segmentEvt, char readnodeEvt, 
                 const sxipilib::GainData& gaindat, double & piEvt, int & errorStatus) {

  // Final gain correction
  double offsetLow = 0;       // Offset of the gain function in the low energy band
  double linrLow = 0;         // Linear coefficient of the gain in the low energy band
  double quadLow = 0;         // Quadratic coefficient of the gain in the low energy band
  double offsetMed = 0;       // Offset of the gain function in the medium energy band
  double linrMed = 0;         // Linear coefficient of the gain in the medium energy band
  double quadMed = 0;         // Quadratic coefficient of the gain in the medium energy band
  double offsetHi = 0;        // Offset of the gain function in the high energy band
  double linrHi = 0;          // Linear coefficient of the gain in the high energy band
  double quadHi = 0;          // Quadratic coefficient of the gain in the high energy band
  double boundLM = 0;         // Boundary between the low and medium energy bands
  double boundMH = 0;         // Boundary between the medium and high energy bands

  errorStatus = 0;            // Error status. 0: OK; 1: Continue, next row; 2: Stop program

  // Get CALDB values
  searchGain(gaindat, ccdidEvt, segmentEvt, readnodeEvt, offsetLow, linrLow,
             quadLow, offsetMed, linrMed, quadMed, offsetHi, linrHi, quadHi,
             boundLM, boundMH);

  // Main correction routine, calculate PI
  if(phaEvt <= boundLM) {
    piEvt = offsetLow + linrLow*phaEvt + quadLow*pow(phaEvt,2);
  } else if(phaEvt > boundLM && phaEvt <= boundMH) {
    piEvt = offsetMed + linrMed*phaEvt + quadMed*pow(phaEvt,2);
  } else {
    piEvt = offsetHi + linrHi*phaEvt + quadHi*pow(phaEvt,2);
  }

  piEvt = int(piEvt);
  if(piEvt > sxipilib::PI_TLMAX) piEvt = sxipilib::PI_TLMAX;
  if(piEvt < sxipilib::PI_TLMIN) piEvt = sxipilib::PI_TLMIN;

} // end correctGain

// ****************************************************************************

void parseStringInt(std::string str, int length, int * outarr) {

  // This function takes in strings of format "(1,2,3,4,5)" and returns an array of ints, separated
  // by commas and ignoring excess beginning/ending whitespace or parentheses.

  std::stringstream msg;  // For message output

  char delimiter = ',';  // INPUT may have comma-delimited values 
  char **pieces = NULL;  // Pointers to comma-delimited parts of INPUT 
  int n_pieces = 0;      // Number of comma-delimited parts of INPUT
  int trim_blanks = 1;   // Flag to trim blanks preceding and following comma-delimited strings
  int skip_empty = 1;    // Flag to skip empty slots between commas
  int guard_paren = 1;   // Flag to allow quoting by parens or quote marks
  int parse_status = 0;  // Flag for valid outcome of breaking up INPUT at commas
  char *ptr = NULL;      // Used by strtod and strtol

  char liststr[FLEN_VALUE+1]; // Intermediate buffer for expand_item_list call
 
  bool trim = true;         // Check whether to continue trimming whitespace/parentheses

  // trim beginning and ending excess:
  while(trim) {
    if((str[0] == '(') || (str[0] == ' ')) { 
      str.erase(0,1);
    } else if((str[str.length()-1] == ')') || (str[str.length()-1] == ' ') || (str[str.length()-1] == '\n')) {
      str.erase(str.length()-1,1);
    } else { trim = false; }
  }
  
  // initialize the C-style string used in call to
  // expand_item_list
  for (int i=0; i<=FLEN_VALUE; i++) {
    liststr[FLEN_VALUE] = '\0';
  }

  // copy trimmed value into C-style string
  // (must be length of the value field of a FITS keyword,
  // or less)
  std::strncpy(liststr, str.c_str(), FLEN_VALUE);

  // expand_item_list permits @-files, but this program
  // does not
  if ('@' == liststr[0]) {
    AH_THROW_RUNTIME("Indirect file (@-file) not permitted");
  }

  // parse string with comma as a delimiter
  pieces = expand_item_list(liststr,
    &n_pieces, delimiter, trim_blanks,
    skip_empty, guard_paren, &parse_status);

  if (length != n_pieces) {
    msg.str("");
    msg << "string \"" << str << "\" must specify " << length << " item(s); gives " 
      << n_pieces << " item(s) instead";
    AH_THROW_RUNTIME(msg.str());
  }

  for(int i = 0; i < length; ++i) {
    if (ahgen::strtoupper(pieces[i]) != "NA") {
      outarr[i] = std::strtol(pieces[i], &ptr, 10);
      if (ptr == pieces[i] || ptr != pieces[i] + std::strlen(pieces[i])) {
        msg.str("");
        msg << "non-numeric value found: " << std::string(pieces[i]);
        AH_THROW_RUNTIME(msg.str());
      }
    }
  }

}

// ****************************************************************************

std::string concatenate(std::string const & keyroot, int colnum) {
  std::stringstream s;
  s << keyroot << colnum;
  return s.str();
}

// ****************************************************************************

void setDebugNull(double* phasEvenOdd, double* phasChTrail, double* phasCTI,
                  int len, double& phaSpTh, int& gradeSpTh) {
  for(int ii = 0; ii < len; ++ii) {
    phasEvenOdd[ii] = NAN;
    phasChTrail[ii] = NAN;
    phasCTI[ii] = NAN;
  }
  phaSpTh = sxipilib::PHA_TNULL;
  gradeSpTh = sxipilib::GRADE_TNULL;
}

// ****************************************************************************

void setOutputNull(double& phaEvt, int& gradeEvt, double& piEvt) {
  phaEvt = sxipilib::PHA_TNULL;
  gradeEvt = sxipilib::GRADE_TNULL;
  piEvt = sxipilib::PI_TNULL;
}

// ****************************************************************************

} // namespace sxipilib

/* Revision Log
 $Log: sxipilib.cxx,v $
 Revision 1.59  2017/01/13 17:44:08  mdutka
 Correcting segfault error with debug statements when reading CTI CALDB file

 Revision 1.58  2016/09/29 19:37:49  rshill
 Corrected bugs in CALDB file reading routines loadEvenOdd, loadChargeTrail, loadCTI, loadSpTh, loadGain.
 Reading blocks of lines with same timestamp was failing if more than one such block in CALDB file.
 Fix mainly due to E. Miller.

 Revision 1.57  2016/08/09 16:45:15  rshill
 Incorporated E. Miller's edit reversing bit order assumed for P_OUTER_MOST.

 Revision 1.56  2016/07/29 23:17:08  rshill
 Enable NA value when parsing integer vectors represented as strings.
 Causes the particular value to to skipped.

 Revision 1.55  2016/07/27 22:50:11  rshill
 Changed wording of an error message.

 Revision 1.54  2016/07/27 22:39:11  rshill
 Corrected parsing of EVTTHRE to detect error conditions:
 (1) non-numeric string; (2) wrong number of values specified.

 Revision 1.53  2016/07/26 18:45:06  rshill
 Corrected accidental skipping of record in HK file
 due to extra nextRow() at bottom of for loop.

 Revision 1.52  2016/07/19 13:40:00  mdutka
 Updating behavior of charge trail correction now applied to negative values

 Revision 1.51  2016/02/09 19:42:51  mdutka
 Correctiong behavior based on DATAMODE keywords

 Revision 1.50  2016/01/15 14:50:22  mdutka
 removing tnull from array columns, this causes ftverify to fail.  Also using multiple routers to connect to video temperature file, fixes bug with hkvideoid parameter

 Revision 1.49  2015/12/29 13:44:28  mdutka
 Changing handling of of CALDB files when the CALDB query is not performed

 Revision 1.47  2015/12/01 17:52:20  mdutka
 minor bug fix

 Revision 1.46  2015/11/16 17:06:55  mdutka
 Adding bug fix from eric caught 11-13-15

 Revision 1.45  2015/11/12 17:51:14  mdutka
 Updating sxipi to account for inverse echo effect

 Revision 1.44  2015/11/09 13:47:13  mdutka
 commiting updated version of sxipi from erci miller

 Revision 1.43  2015/10/29 21:57:49  mdutka
 Fixed bug in CTI correction routine

 Revision 1.42  2015/10/29 16:46:43  mdutka
 adding debugging output

 Revision 1.41  2015/10/28 20:47:43  mdutka
 Commiting sxipi after bug fix

 Revision 1.40  2015/10/21 19:36:18  mdutka
 Checking in sxipi after adding updated CTI correction routine

 Revision 1.39  2015/10/01 16:47:12  klrutkow
 updated extension numbers to move to for CI, CTI, and gain files

 Revision 1.38  2015/09/16 15:00:00  klrutkow
 removed CALDB calls (this is now done in sxipi.cxx using the ahmission calbd resolve query) ; fixed ccnm0001 check for even odd file ; check numRows after moving to correct extension instead of before

 Revision 1.37  2015/08/19 15:52:46  mwitthoe
 sxipi: change TNULL from 4095 to 4096; create function to set NULL to reduce duplication; create constants for TLMIN, TLMAX, and TNULL for PHA, PI, and GRADE

 Revision 1.36  2015/08/19 13:54:56  asargent
 Fixed bug when indexing the hkdata array. Bug showed up on 32-bit Mac and caused GCC to segfault due to using a long long as an index

 Revision 1.35  2015/08/17 23:53:10  asargent
 Updated FITS file checking just after opening

 Revision 1.34  2015/08/10 15:40:40  mwitthoe
 sxipi: add standard prologue, stamp parameters to log file; move non-standard functions from sxipi.cxx to sxipilib.h/cxx; general clean-up

 Revision 1.33  2015/07/31 21:37:06  mdutka
 Adding logging to sxipi

 Revision 1.32  2015/06/24 16:33:23  mdutka
 debugging CALDB query

 Revision 1.31  2015/06/08 20:11:37  mdutka
 adding handling time = null in event and hk file

 Revision 1.30  2015/06/05 20:17:47  mdutka
 adding hkvideoid

 Revision 1.29  2015/06/04 20:13:13  mdutka
 debug

 Revision 1.28  2015/06/04 20:09:14  mdutka
 debug

 Revision 1.27  2015/06/04 20:04:39  mdutka
 debug

 Revision 1.26  2015/06/04 18:56:51  mdutka
 adding debug info

 Revision 1.25  2015/06/03 20:44:24  mdutka
 checking in bug fix with caldb loading

 Revision 1.24  2015/06/03 20:33:20  mdutka
 adding debug info

 Revision 1.23  2015/06/03 19:57:22  mdutka
 adding tnull support for PHAS column

 Revision 1.22  2015/06/03 18:53:28  mdutka
 fix row bug

 Revision 1.21  2015/06/03 18:43:40  mdutka
 fix reading of first row sxipilib

 Revision 1.20  2015/06/03 17:14:42  mdutka
 fixed bug with CALDB lodaing

 Revision 1.19  2015/06/02 21:16:21  mdutka
 updating row number checks

 Revision 1.18  2015/06/02 19:55:36  mdutka
 fixed bug in CALDB file loading

 Revision 1.17  2015/06/02 19:12:09  mdutka
 correcting loading of CALDB files

 Revision 1.16  2015/05/28 22:04:41  mdutka
 updating CALDB query for chargetrail file

 Revision 1.15  2015/05/27 20:21:45  mdutka
 fixed bug when reading hkfile

 Revision 1.14  2015/05/22 17:03:58  mdutka
 change keyword code looks for in CTI caldb file

 Revision 1.13  2015/05/22 13:13:34  mdutka
 Chaging flag bit order and implementing CALDB

 Revision 1.12  2015/04/13 16:27:42  mdutka
 fixing memory bug for status bit column and issues Eric identified in first report on sxipi Apr9_2015

 Revision 1.11  2015/01/22 14:56:19  asargent
 Fixed CALDB time check in file loading

 Revision 1.10  2014/12/11 21:27:08  asargent
 New data structure to hold HK information. Fixed CALDB time verification to correct check.

 Revision 1.9  2014/10/29 21:41:20  asargent
 Added boundary checking for each event and TNULL,TLMIN,TLMAX keyword writing.

 Revision 1.8  2014/09/18 20:07:20  asargent
 Changed event file keyword from CISTART to CIFIRST. Made extension explicit in HK file.

 Revision 1.7  2014/07/17 14:21:46  asargent
 Changed instances of float to double for better precision between different architectures.

 Revision 1.6  2014/06/23 13:59:50  asargent
 Updated sxipi to include more debugging options for starting PHAS column. Also changed local variables ccdid, readnode and segment to type char in accordance with changes to FITS file from J-type to B-type.

 Revision 1.5  2014/04/01 15:04:54  asargent
 Updates to include observation time validation in load functions. New function to parse keyword for CALDB files.

 Revision 1.4  2014/02/20 19:17:46  asargent
 Minor comment changes, first working version of sxipi.

 Revision 1.3  2014/02/12 14:23:24  asargent
 Added in memory allocation for data structure elements

 Revision 1.2  2014/02/11 16:00:55  asargent
 Verified column names and rearranged includes.

 Revision 1.1  2014/02/10 21:09:02  asargent
 First version of sxipilib source file.

*/
