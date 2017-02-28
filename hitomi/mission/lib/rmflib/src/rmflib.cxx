/// \file rmflib.cxx
/// \brief Shared RMF functions for astroh tools
/// \author Andy Sargent
/// \date $Date: 2016/10/04 16:27:12 $
 
#define AHLABEL rmflib_rmflib
#define AHCVSID "$Id: rmflib.cxx,v 1.39 2016/10/04 16:27:12 rshill Exp $"

#include "rmflib/rmflib.h"
#include "ahlog/ahlog.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"

#include "ahmath/ahmath.h"

namespace rmflib {

void openRMFFile(RMFData * rmfdat, const std::string filename, const std::string matrix_extname)
{ 
  std::stringstream ss;         // For formatting strings

  double l_energlo = 0.0;     // Connected variables for energies
  double l_energhi = 0.0;
  double l_emin = 0.0;
  double l_emax = 0.0;

  long irow = 0;   // Row number in binary table
  long nrow = 0;   // Number of rows in binary table

  if (rmfdat == 0) {
    AH_THROW_RUNTIME("openRMFFile called with unallocated RMFData structure");
  }

  if (rmfdat->m_ahffp != 0 || rmfdat->m_matrixrow.m_router != 0) {
    AH_THROW_RUNTIME("openRMFFile called with RMFData structure already in use");
  }

  // set read/write flag
  rmfdat->m_write = false;

  // open the file
  ahfits::open(filename, "", &rmfdat->m_ahffp);

  // open EBOUNDS extension
  ahfits::move(rmfdat->m_ahffp, "EBOUNDS");

  // setup router
  rmfdat->m_matrixrow.m_router = new ahfits::Router(rmfdat->m_ahffp);

  // read required header keywords
  rmfdat->m_ebkey.m_telescop = ahfits::getKeyValStr(rmfdat->m_ahffp,"TELESCOP");
  rmfdat->m_ebkey.m_instrume = ahfits::getKeyValStr(rmfdat->m_ahffp,"INSTRUME");
  if (ahfits::keywordExists(rmfdat->m_ahffp, "FILTER")) {
    rmfdat->m_ebkey.m_filter    = ahfits::getKeyValStr(rmfdat->m_ahffp,"FILTER");
  } else {
    rmfdat->m_ebkey.m_filter    = "";
    AH_INFO(ahlog::LOW) << "FILTER keyword missing from EBOUNDS extension; proceeding anyway" << std::endl;
  }
  rmfdat->m_ebkey.m_chantype = ahfits::getKeyValStr(rmfdat->m_ahffp,"CHANTYPE");
  rmfdat->m_ebkey.m_detchans  = ahfits::getKeyValLLong(rmfdat->m_ahffp,"DETCHANS");
  rmfdat->m_ebkey.m_hduclass = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUCLASS");
  rmfdat->m_ebkey.m_hduclas1 = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUCLAS1");
  rmfdat->m_ebkey.m_hduclas2 == ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUCLAS2");
  rmfdat->m_ebkey.m_hduvers = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUVERS");
  rmfdat->m_ebkey.m_hduvers1 = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUVERS1");
  rmfdat->m_ebkey.m_hduvers2 = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUVERS2");
  if (ahfits::keywordExists(rmfdat->m_ahffp, "RMFVERSN")) {
    rmfdat->m_ebkey.m_rmfversn  = ahfits::getKeyValStr(rmfdat->m_ahffp,"RMFVERSN");
  } else {
    rmfdat->m_ebkey.m_rmfversn  = "";
    AH_INFO(ahlog::LOW) << "RMFVERSN keyword missing from EBOUNDS extension; proceeding anyway" << std::endl;
  }
  rmfdat->m_ebkey.m_ccls0001 = ahfits::getKeyValStr(rmfdat->m_ahffp,"CCLS0001");
  rmfdat->m_ebkey.m_ccnm0001 = ahfits::getKeyValStr(rmfdat->m_ahffp,"CCNM0001");
  rmfdat->m_ebkey.m_cdtp0001 = ahfits::getKeyValStr(rmfdat->m_ahffp,"CDTP0001");
  rmfdat->m_ebkey.m_cvsd0001 = ahfits::getKeyValStr(rmfdat->m_ahffp,"CVSD0001");
  rmfdat->m_ebkey.m_cvst0001 = ahfits::getKeyValStr(rmfdat->m_ahffp,"CVST0001");
  rmfdat->m_ebkey.m_cdes0001 = ahfits::getKeyValStr(rmfdat->m_ahffp,"CDES0001");

  // Store TLMIN/TLMAX of CHANNEL column
  ahfits::columnRange(rmfdat->m_ahffp,"CHANNEL",rmfdat->m_tlmin_channel,rmfdat->m_tlmax_channel);
 
  // Store units of E_Min column
  rmfdat->m_tunit_emin = ahfits::columnUnits(rmfdat->m_ahffp,"E_MIN");

  // read EBOUNDS extension into structure

  nrow = ahfits::getKeyValLLong(rmfdat->m_ahffp, "NAXIS2");
  rmfdat->m_echanstart = new double[nrow];
  rmfdat->m_echanstop = new double[nrow];
  rmfdat->m_echancen = new double[nrow];
  
  // connect varibles to input columns
  rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READONLY, "E_MIN", l_emin);
  rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READONLY, "E_MAX", l_emax);

  irow = 0;
  for (ahfits::firstRow(rmfdat->m_ahffp); ahfits::readOK(rmfdat->m_ahffp);
    ahfits::nextRow(rmfdat->m_ahffp)) {
    ahfits::readRow(rmfdat->m_ahffp);
    rmfdat->m_echanstart[irow] = l_emin;
    rmfdat->m_echanstop[irow] = l_emax;
    rmfdat->m_echancen[irow] = 0.5*(l_emin + l_emax);
    irow++;
  }

  if (irow != nrow) {
    ss.str("");
    ss << "Bad number of rows in EBOUNDS: counted " << irow << ", NAXIS2 = " << nrow;
    AH_THROW_RUNTIME(ss.str());
  }

  rmfdat->m_nchan = nrow;

  rmfdat->m_matrixrow.m_router->clearConnections();

  // go to MATRIX extension
  ahfits::move(rmfdat->m_ahffp, matrix_extname);

  // read required header keywords 
  rmfdat->m_matkey.m_telescop  = ahfits::getKeyValStr(rmfdat->m_ahffp,"TELESCOP");
  rmfdat->m_matkey.m_instrume  = ahfits::getKeyValStr(rmfdat->m_ahffp,"INSTRUME");
  if (ahfits::keywordExists(rmfdat->m_ahffp, "DETNAM")) {
    rmfdat->m_matkey.m_detnam    = ahfits::getKeyValStr(rmfdat->m_ahffp,"DETNAM");
  } else {
    rmfdat->m_matkey.m_detnam    = "";
    AH_INFO(ahlog::LOW) << "DETNAM keyword missing from " << matrix_extname 
      << " extension; proceeding anyway" << std::endl;
  }
  if (ahfits::keywordExists(rmfdat->m_ahffp, "FILTER")) {
    rmfdat->m_matkey.m_filter    = ahfits::getKeyValStr(rmfdat->m_ahffp,"FILTER");
  } else {
    rmfdat->m_matkey.m_filter    = "";
    AH_INFO(ahlog::LOW) << "FILTER keyword missing from " << matrix_extname 
      << " extension; proceeding anyway" << std::endl;
  }
  rmfdat->m_matkey.m_chantype  = ahfits::getKeyValStr(rmfdat->m_ahffp,"CHANTYPE");
  rmfdat->m_matkey.m_hduclass  = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUCLASS");
  rmfdat->m_matkey.m_hduclas1  = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUCLAS1");
  rmfdat->m_matkey.m_hduclas2  = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUCLAS2");
  rmfdat->m_matkey.m_hduclas2  = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUCLAS3");
  rmfdat->m_matkey.m_hduvers   = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUVERS");
  rmfdat->m_matkey.m_hduvers1  = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUVERS1");
  rmfdat->m_matkey.m_hduvers2  = ahfits::getKeyValStr(rmfdat->m_ahffp,"HDUVERS2");
  if (ahfits::keywordExists(rmfdat->m_ahffp, "RMFVERSN")) {
    rmfdat->m_matkey.m_rmfversn  = ahfits::getKeyValStr(rmfdat->m_ahffp,"RMFVERSN");
  } else {
    rmfdat->m_matkey.m_rmfversn  = "";
    AH_INFO(ahlog::LOW) << "RMFVERSN keyword missing from " << matrix_extname 
      << " extension; proceeding anyway" << std::endl;
  }
  rmfdat->m_matkey.m_lo_thresh = ahfits::getKeyValDbl(rmfdat->m_ahffp,"LO_THRES");
  rmfdat->m_matkey.m_ccls0001  = ahfits::getKeyValStr(rmfdat->m_ahffp,"CCLS0001");
  rmfdat->m_matkey.m_ccnm0001  = ahfits::getKeyValStr(rmfdat->m_ahffp,"CCNM0001");
  rmfdat->m_matkey.m_cdtp0001  = ahfits::getKeyValStr(rmfdat->m_ahffp,"CDTP0001");
  rmfdat->m_matkey.m_cvsd0001  = ahfits::getKeyValStr(rmfdat->m_ahffp,"CVSD0001");
  rmfdat->m_matkey.m_cvst0001  = ahfits::getKeyValStr(rmfdat->m_ahffp,"CVST0001");
  rmfdat->m_matkey.m_cdes0001  = ahfits::getKeyValStr(rmfdat->m_ahffp,"CDES0001");
  rmfdat->m_matkey.m_detchans  = ahfits::getKeyValLLong(rmfdat->m_ahffp,"DETCHANS");
  if (ahfits::keywordExists(rmfdat->m_ahffp, "NUMGRP")) {
    rmfdat->m_matkey.m_numgrp    = ahfits::getKeyValLLong(rmfdat->m_ahffp,"NUMGRP");
  } else {
    rmfdat->m_matkey.m_numgrp    = 0;
    AH_INFO(ahlog::LOW) << "NUMGRP keyword missing from " << matrix_extname 
      << " extension; proceeding anyway" << std::endl;
  }
  if (ahfits::keywordExists(rmfdat->m_ahffp, "NUMELT")) {
    rmfdat->m_matkey.m_numelt    = ahfits::getKeyValLLong(rmfdat->m_ahffp,"NUMELT");
  } else {
    rmfdat->m_matkey.m_numelt    = 0;
    AH_INFO(ahlog::LOW) << "NUMELT keyword missing from " << matrix_extname 
      << " extension; proceeding anyway" << std::endl;
  }

  // Store TLMIN/TLMAX of F_CHAN column
  ahfits::columnRange(rmfdat->m_ahffp,"F_CHAN",rmfdat->m_tlmin_fchan,rmfdat->m_tlmax_fchan);
 
  // Store units of ENERG_LO column
  rmfdat->m_tunit_energlo = ahfits::columnUnits(rmfdat->m_ahffp,"ENERG_LO");
  rmfdat->m_kevunits = ahgen::strtoupper(rmfdat->m_tunit_energlo) == "KEV";
 
  // get size and allocate

  nrow = ahfits::getKeyValLLong(rmfdat->m_ahffp, "NAXIS2");
  rmfdat->m_ematstart = new double[nrow];
  rmfdat->m_ematstop = new double[nrow];
  rmfdat->m_ematcen = new double[nrow];

  // connect local variables

  rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READONLY, "ENERG_LO", l_energlo);
  rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READONLY, "ENERG_HI", l_energhi);

  // read into structure members

  irow = 0;
  for (ahfits::firstRow(rmfdat->m_ahffp); ahfits::readOK(rmfdat->m_ahffp);
    ahfits::nextRow(rmfdat->m_ahffp)) {
    ahfits::readRow(rmfdat->m_ahffp);
    rmfdat->m_ematstart[irow] = l_energlo;
    rmfdat->m_ematstop[irow] = l_energhi;
    rmfdat->m_ematcen[irow] = 0.5*(l_energlo + l_energhi);
    irow++;
  }

  if (irow != nrow) {
    ss.str("");
    ss << "Bad number of rows in MATRIX: counted " << irow << ", NAXIS2 = " << nrow;
    AH_THROW_RUNTIME(ss.str());
  }

  rmfdat->m_nmat = nrow;

  rmfdat->m_matrixrow.m_router->clearConnections();

  // Initialize members used in writing.  This is in case they need
  // to be copied to another instance of RMFData.  

  rmfdat->m_telescop = rmfdat->m_matkey.m_telescop;
  rmfdat->m_instrume = rmfdat->m_matkey.m_instrume;
  rmfdat->m_filter = rmfdat->m_matkey.m_filter;
  rmfdat->m_detnam = rmfdat->m_matkey.m_detnam;
  rmfdat->m_lo_thresh = rmfdat->m_matkey.m_lo_thresh;
  rmfdat->m_chantype = rmfdat->m_matkey.m_chantype;
  rmfdat->m_date = rmfdat->m_matkey.m_cvsd0001;
  rmfdat->m_time = rmfdat->m_matkey.m_cvst0001;
  rmfdat->m_description_matrix = rmfdat->m_matkey.m_cdes0001;
  rmfdat->m_description_ebounds = rmfdat->m_ebkey.m_cdes0001;
  rmfdat->m_numgrp = rmfdat->m_matkey.m_numgrp;
  rmfdat->m_numelt = rmfdat->m_matkey.m_numelt;

  // Allocate storage for variable-length arrays.
  rmfdat->m_matrixrow.m_f_chan = new int[MAX_NGRP];
  rmfdat->m_matrixrow.m_n_chan = new int[MAX_NGRP];
  rmfdat->m_matrixrow.m_matrix = new double[rmfdat->m_nchan];

  // connect structure members in preparation for row-by-row reading

  rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READONLY,"ENERG_LO",
            rmfdat->m_matrixrow.m_energ_lo);
  rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READONLY,"ENERG_HI",
              rmfdat->m_matrixrow.m_energ_hi);
  rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READONLY,"N_GRP", 
            rmfdat->m_matrixrow.m_ngrp);
  // permit N_CHAN, F_CHAN columns to be fixed length or scalar
  if (ahfits::columnType(rmfdat->m_ahffp, "F_CHAN") < 0) {
    rmfdat->m_matrixrow.m_router->connectVariableLengthArray(ahfits::e_READONLY,"F_CHAN", 
                     rmfdat->m_matrixrow.m_f_chan, 
                     rmfdat->m_matrixrow.m_ngrp);
  } else {
    if (ahfits::columnRepeat(rmfdat->m_ahffp, "F_CHAN") == 1) {
      rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READONLY,"F_CHAN", 
                       rmfdat->m_matrixrow.m_f_chan[0]);
      AH_INFO(ahlog::LOW) << "F_CHAN column is scalar; proceeding anyway" << std::endl;
    } else {
      rmfdat->m_matrixrow.m_router->connectFixedLengthArray(ahfits::e_READONLY,"F_CHAN", 
                       rmfdat->m_matrixrow.m_f_chan);
      AH_INFO(ahlog::LOW) << "F_CHAN column is fixed-length array; proceeding anyway" << std::endl;
    }
  }
  if (ahfits::columnType(rmfdat->m_ahffp, "N_CHAN") < 0) {
    rmfdat->m_matrixrow.m_router->connectVariableLengthArray(ahfits::e_READONLY,"N_CHAN", 
                     rmfdat->m_matrixrow.m_n_chan, 
                     rmfdat->m_matrixrow.m_ngrp);
  } else {
    if (ahfits::columnRepeat(rmfdat->m_ahffp, "N_CHAN") == 1) {
      rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READONLY,"N_CHAN", 
                       rmfdat->m_matrixrow.m_n_chan[0]);
      AH_INFO(ahlog::LOW) << "N_CHAN column is scalar; proceeding anyway" << std::endl;
    } else {
      rmfdat->m_matrixrow.m_router->connectFixedLengthArray(ahfits::e_READONLY,"N_CHAN", 
                       rmfdat->m_matrixrow.m_n_chan);
      AH_INFO(ahlog::LOW) << "N_CHAN column is fixed-length array; proceeding anyway" << std::endl;
    }
  }
  rmfdat->m_matrixrow.m_router->connectVariableLengthArray(ahfits::e_READONLY,"MATRIX", 
                   rmfdat->m_matrixrow.m_matrix, 
                   rmfdat->m_matrixrow.m_nmatrix);

}

void copyRMFData(RMFData * rmfdatout, RMFData * rmfdatin) {

  rmfdatout->m_ahffp = rmfdatin->m_ahffp;
  rmfdatout->m_write = rmfdatin->m_write;

  // input energy bins (deep copy; allocates storage)
  rmfdatout->m_nmat = rmfdatin->m_nmat;
  delete [] rmfdatout->m_ematstart;
  delete [] rmfdatout->m_ematstop;
  delete [] rmfdatout->m_ematcen;
  rmfdatout->m_ematstart = new double[rmfdatout->m_nmat];
  rmfdatout->m_ematstop = new double[rmfdatout->m_nmat];
  rmfdatout->m_ematcen = new double[rmfdatout->m_nmat];
  for (long i=0; i<rmfdatout->m_nmat; i++) {
    rmfdatout->m_ematstart[i] = rmfdatin->m_ematstart[i];
    rmfdatout->m_ematstop[i]  = rmfdatin->m_ematstop[i];
    rmfdatout->m_ematcen[i]   = rmfdatin->m_ematcen[i];
  }

  // channel energy bins (deep copy; allocates storage)
  rmfdatout->m_nchan = rmfdatin->m_nchan;
  delete [] rmfdatout->m_echanstart;
  delete [] rmfdatout->m_echanstop;
  delete [] rmfdatout->m_echancen;
  rmfdatout->m_echanstart = new double[rmfdatout->m_nchan];
  rmfdatout->m_echanstop = new double[rmfdatout->m_nchan];
  rmfdatout->m_echancen = new double[rmfdatout->m_nchan];
  for (long i=0; i<rmfdatout->m_nchan; i++) {
    rmfdatout->m_echanstart[i] = rmfdatin->m_echanstart[i];
    rmfdatout->m_echanstop[i]  = rmfdatin->m_echanstop[i];
    rmfdatout->m_echancen[i]   = rmfdatin->m_echancen[i];
  }

  // total number of groups and elements in RMF
  rmfdatout->m_numgrp = rmfdatin->m_numgrp;
  rmfdatout->m_numelt = rmfdatin->m_numelt;

  // keyword values read on input
  // These structures have only scalar members with no
  // pointers, so they can be assigned directly.
  rmfdatout->m_matkey = rmfdatin->m_matkey;
  rmfdatout->m_ebkey  = rmfdatin->m_ebkey;

  // keyword values needed for output
  rmfdatout->m_telescop = rmfdatin->m_telescop;
  rmfdatout->m_instrume = rmfdatin->m_instrume;
  rmfdatout->m_filter = rmfdatin->m_filter;
  rmfdatout->m_detnam = rmfdatin->m_detnam;
  rmfdatout->m_lo_thresh = rmfdatin->m_lo_thresh;
  rmfdatout->m_chantype = rmfdatin->m_chantype;
  rmfdatout->m_date = rmfdatin->m_date;
  rmfdatout->m_time = rmfdatin->m_time;
  rmfdatout->m_description_matrix = rmfdatin->m_description_matrix;
  rmfdatout->m_description_ebounds = rmfdatin->m_description_ebounds;
 
  // TLMIN and TLMAX for CHANNEL (ebounds ext.) and FCHAN (matrix ext.) columns
  rmfdatout->m_tlmin_channel = rmfdatin->m_tlmin_channel;
  rmfdatout->m_tlmax_channel = rmfdatin->m_tlmax_channel;
  rmfdatout->m_tlmin_fchan = rmfdatin->m_tlmin_fchan;
  rmfdatout->m_tlmax_fchan = rmfdatin->m_tlmax_fchan;

  // Units information
  rmfdatout->m_tunit_emin = rmfdatin->m_tunit_emin;
  rmfdatout->m_tunit_energlo = rmfdatin->m_tunit_energlo;
  rmfdatout->m_kevunits = rmfdatin->m_kevunits;

  // data for a single input energy (i.e. row)
  copyRMFMatrixRowVars(&rmfdatout->m_matrixrow, &rmfdatin->m_matrixrow);
}

void copyRMFMatrixRowVars(RMFMatrixRowVars * matrixrowout, RMFMatrixRowVars * matrixrowin) {
  matrixrowout->m_energ_lo = matrixrowin->m_energ_lo;
  matrixrowout->m_energ_hi = matrixrowin->m_energ_hi;
  matrixrowout->m_ngrp = matrixrowin->m_ngrp;
  matrixrowout->m_nmatrix = matrixrowin->m_nmatrix;
  delete [] matrixrowout->m_f_chan;
  matrixrowout->m_f_chan = new int[matrixrowout->m_ngrp];
  delete [] matrixrowout->m_n_chan;
  matrixrowout->m_n_chan = new int[matrixrowout->m_ngrp];
  matrixrowout->m_matrix = new double[matrixrowout->m_nmatrix];
  for (ahfits::IndexType i=0; i<matrixrowout->m_ngrp; i++) {
    matrixrowout->m_f_chan[i] = matrixrowin->m_f_chan[i];
    matrixrowout->m_n_chan[i] = matrixrowin->m_n_chan[i];
  }
  for (ahfits::IndexType i=0; i<matrixrowout->m_nmatrix; i++) {
    matrixrowout->m_matrix[i] = matrixrowin->m_matrix[i];
  }
  matrixrowout->m_router = matrixrowin->m_router;
}

void createRMFFile(RMFData * rmfdat, const std::string filename)
{ 

  if (rmfdat == 0) {
    AH_THROW_RUNTIME("creatermffile called with unallocated RMFData structure");
  }

  if (rmfdat->m_ahffp != 0) {
    AH_THROW_RUNTIME("createRMFFile called with RMFData structure that is already in use");
  }

  // set read/write flag
  rmfdat->m_write = true;

  ahfits::create(filename, "", &rmfdat->m_ahffp);
  
  // create MATRIX extension
  ahfits::addEmptyTbl(rmfdat->m_ahffp, "MATRIX");
 
  // create EBOUNDS extension 
  ahfits::addEmptyTbl(rmfdat->m_ahffp, "EBOUNDS");
 
}

void setInputEnergies(RMFData * rmfdat, double * ematstart, double * ematstop, 
                      double * ematcen, int nmat)
{

  if (rmfdat == 0) {
    AH_THROW_RUNTIME("setInputEnergies called with unallocated RMFData structure");
  }

  if (!rmfdat->m_write) {
    AH_THROW_RUNTIME("setInputEnergies called for RMF file that is open for reading only");
  }

  if (nmat < 1){ 
    AH_THROW_RUNTIME("The number of input energy bins is less than one");
  }

  rmfdat->m_ematstart = new double[nmat];
  for (int ii = 0; ii < nmat; ++ii) rmfdat->m_ematstart[ii] = 0.0;
  rmfdat->m_ematstop = new double[nmat];
  for (int ii = 0; ii < nmat; ++ii) rmfdat->m_ematstop[ii] = 0.0;
  rmfdat->m_ematcen = new double[nmat];
  for (int ii = 0; ii < nmat; ++ii) rmfdat->m_ematcen[ii] = 0.0;

  for (int ii = 0; ii < nmat; ++ii) {
    rmfdat->m_ematstart[ii] = ematstart[ii];
    rmfdat->m_ematstop[ii] = ematstop[ii];
    rmfdat->m_ematcen[ii] = ematcen[ii];
  }
  rmfdat->m_nmat = nmat;
}

void setChannelEnergies(RMFData * rmfdat, double * echanstart, double * echanstop,
                        double * echancen, int nchan)
{

  if (rmfdat == 0) {
    AH_THROW_RUNTIME("setChannelEnergies called with unallocated RMFData structure");
  }

  if (!rmfdat->m_write) {
    AH_THROW_RUNTIME("setChannelEnergies called for RMF file that is open for reading only");
  }

  if (nchan < 1){ 
    AH_THROW_RUNTIME("The number of output energy channels is less than one");
  }

  rmfdat->m_echanstart = new double[nchan];
  for (int ii = 0; ii < nchan; ++ii) rmfdat->m_echanstart[ii] = 0.0;
  rmfdat->m_echanstop = new double[nchan];
  for (int ii = 0; ii < nchan; ++ii) rmfdat->m_echanstop[ii] = 0.0;
  rmfdat->m_echancen = new double[nchan];
  for (int ii = 0; ii < nchan; ++ii) rmfdat->m_echancen[ii] = 0.0;
    
  for (int ii = 0; ii < nchan; ++ii) {
    rmfdat->m_echanstart[ii] = echanstart[ii];
    rmfdat->m_echanstop[ii] = echanstop[ii];
    rmfdat->m_echancen[ii] = echancen[ii];
  }

  rmfdat->m_nchan = nchan;
}


void writeEboundsHDU(RMFData * rmfdat) {
        
  int channel = 0;   // channel number for ebounds extension
  double e_min = 0.0; // E_MIN column value written to fits file
  double e_max = 0.0; // E_MAX column value written to fits file

  if (rmfdat == 0) {
    AH_THROW_RUNTIME("writeEboundsHDU called with unallocated RMFData structure");
  }

  if (!rmfdat->m_write) {
    AH_THROW_RUNTIME("writeEboundsHDU called for RMF file that is open for reading only");
  }

  // Move to Ebounds extension
  // std::cout << "Move to Ebounds extension" << std::endl;
  ahfits::move(rmfdat->m_ahffp, "EBOUNDS");
   
  // add columns to Ebounds binary extension
  // std::cout << "add columns to Ebounds binary extension" << std::endl;
  ahfits::insertColAfter(rmfdat->m_ahffp, "CHANNEL","J", "");
  ahfits::setTLmin(rmfdat->m_ahffp,"CHANNEL",rmfdat->m_tlmin_channel);
  ahfits::setTLmax(rmfdat->m_ahffp,"CHANNEL",rmfdat->m_tlmax_channel);
  ahfits::setColumnDescription(rmfdat->m_ahffp,"CHANNEL","Detector channel number");
  ahfits::insertColAfter(rmfdat->m_ahffp, "E_MIN","E", "");
  ahfits::setTUnit(rmfdat->m_ahffp,"E_MIN","keV");
  ahfits::setColumnDescription(rmfdat->m_ahffp,"E_MIN","Low energy bound");
  ahfits::insertColAfter(rmfdat->m_ahffp, "E_MAX","E", "");
  ahfits::setTUnit(rmfdat->m_ahffp,"E_MAX","keV");
  ahfits::setColumnDescription(rmfdat->m_ahffp,"E_MAX","High energy bound");

  // setup router
  // std::cout << "setup router" << std::endl;
  ahfits::Router router(rmfdat->m_ahffp);
  
  // connect varibles to output columns
  // std::cout << "connect variables to output columns" << std::endl;
  router.connectScalar(ahfits::e_WRITEONLY,"CHANNEL",channel);
  router.connectScalar(ahfits::e_WRITEONLY,"E_MIN",e_min);
  router.connectScalar(ahfits::e_WRITEONLY,"E_MAX",e_max);
  
  // std::cout << "fill in columns for ebounds extension" << std::endl;
  ahfits::firstRow(rmfdat->m_ahffp);
  for (channel = 0; channel < rmfdat->m_nchan; ++channel) {
    e_min = rmfdat->m_echanstart[channel];
    e_max = rmfdat->m_echanstop[channel];
    ahfits::writeRow(rmfdat->m_ahffp); 
    ahfits::nextRow(rmfdat->m_ahffp);
  }

  ahfits::writeKeyValStr(rmfdat->m_ahffp,"TELESCOP",rmfdat->m_telescop,"Telescope mission name");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"INSTRUME",rmfdat->m_instrume,"Instrument name");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"DETNAM",rmfdat->m_detnam,"Detector subsystem");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"FILTER",rmfdat->m_filter,"Filter name used");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CHANTYPE",rmfdat->m_chantype,"Type of channels (PHA, PI etc)");  
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUCLASS","OGIP","Format conforms to OGIP standards");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUCLAS1","RESPONSE","Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUCLAS2","EBOUNDS","Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUVERS","1.2.0","Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUVERS1","1.1.0","Obsolete");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUVERS2","1.2.0","Obsolete");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"RMFVERSN","1992a","Obsolete");   
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CCLS0001","CPF","Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CCNM0001","EBOUNDS","Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CDTP0001","DATA","Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CVSD0001",rmfdat->m_date,"Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CVST0001",rmfdat->m_time,"Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CDES0001",rmfdat->m_description_ebounds,"Keyword information for Caltools Software");
  
}

void createMatrixHDU(RMFData * rmfdat) {

  if (rmfdat == 0) {
    AH_THROW_RUNTIME("createMatrixHDU called with unallocated RMFData structure");
  }

  if (!rmfdat->m_write) {
    AH_THROW_RUNTIME("createMatrixHDU called for RMF file that is open for reading only");
  }

  // move to Matrix binary extension 
  ahfits::move(rmfdat->m_ahffp, "MATRIX");

  // initialize total number of rmf groups and elements

  rmfdat->m_numgrp = 0;
  rmfdat->m_numelt = 0;
   
  // add columns to Matrix binary extension
  ahfits::insertColAfter(rmfdat->m_ahffp,"ENERG_LO","E", "");
  ahfits::setTUnit(rmfdat->m_ahffp,"ENERG_LO","keV");
  ahfits::setColumnDescription(rmfdat->m_ahffp,"ENERG_LO","Low energy bound");
  ahfits::insertColAfter(rmfdat->m_ahffp,"ENERG_HI","E", "");
  ahfits::setTUnit(rmfdat->m_ahffp,"ENERG_HI","keV"); 
  ahfits::setColumnDescription(rmfdat->m_ahffp,"ENERG_HI","High energy bound");
  ahfits::insertColAfter(rmfdat->m_ahffp,"N_GRP","I", "");
  ahfits::setColumnDescription(rmfdat->m_ahffp,"N_GRP","Number of channel subsets");
  ahfits::insertColAfter(rmfdat->m_ahffp,"F_CHAN","PJ", "");
  ahfits::setTLmin(rmfdat->m_ahffp,"F_CHAN",rmfdat->m_tlmin_fchan);
  ahfits::setTLmax(rmfdat->m_ahffp,"F_CHAN",rmfdat->m_tlmax_fchan);
  ahfits::setColumnDescription(rmfdat->m_ahffp,"F_CHAN","First channel in each subset");
  ahfits::insertColAfter(rmfdat->m_ahffp,"N_CHAN","PJ", ""); 
  ahfits::setColumnDescription(rmfdat->m_ahffp,"N_CHAN","Number chans in each subset");
  ahfits::insertColAfter(rmfdat->m_ahffp,"MATRIX","PE", "");
  ahfits::setColumnDescription(rmfdat->m_ahffp,"MATRIX","(non-zero) Matrix elements");

  // write header keywords 
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"TELESCOP", rmfdat->m_telescop, "Telescope mission name");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"INSTRUME", rmfdat->m_instrume, "Instrument name");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"DETNAM",rmfdat->m_detnam,"Detector subsystem");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"FILTER",rmfdat->m_filter,"Filter name used");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CHANTYPE", rmfdat->m_chantype, "Type of channels (PHA, PI etc)");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUCLASS", "OGIP", "Format conforms to OGIP standards");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUCLAS1", "RESPONSE", "Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUCLAS2", "RSP_MATRIX", "Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUCLAS3", "REDIST", "Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUVERS", "1.3.0", "Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUVERS1", "1.1.0", "Obsolete");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"HDUVERS2", "1.2.0", "Obsolete");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"RMFVERSN", "1992a", "Obsolete"); 
  ahfits::writeKeyValDbl(rmfdat->m_ahffp,"LO_THRES", rmfdat->m_lo_thresh, "Lower probability density threshold for matrix");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CCLS0001","CPF","Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CCNM0001","MATRIX","Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CDTP0001","DATA","Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CVSD0001",rmfdat->m_date,"Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CVST0001",rmfdat->m_time,"Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat->m_ahffp,"CDES0001",rmfdat->m_description_matrix,"Keyword information for Caltools Software");
  
  // Assign memory and initialize f_chan, n_chan, m_matrix

  rmfdat->m_matrixrow.m_f_chan = new int[MAX_NGRP];
  for (int ii = 0; ii < MAX_NGRP; ++ii) rmfdat->m_matrixrow.m_f_chan[ii] = 0;
  rmfdat->m_matrixrow.m_n_chan = new int[MAX_NGRP];
  for (int ii = 0; ii < MAX_NGRP; ++ii) rmfdat->m_matrixrow.m_n_chan[ii] = 0;
  rmfdat->m_matrixrow.m_matrix = new double[rmfdat->m_nchan];
  for (int ii = 0; ii < rmfdat->m_nchan; ++ii) rmfdat->m_matrixrow.m_matrix[ii] = 0.0;

  // setup router
  rmfdat->m_matrixrow.m_router = new ahfits::Router(rmfdat->m_ahffp);

  // connect local variables

  rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READWRITE,"ENERG_LO",
            rmfdat->m_matrixrow.m_energ_lo);
  rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READWRITE,"ENERG_HI",
              rmfdat->m_matrixrow.m_energ_hi);
  rmfdat->m_matrixrow.m_router->connectScalar(ahfits::e_READWRITE,"N_GRP", 
            rmfdat->m_matrixrow.m_ngrp);
  rmfdat->m_matrixrow.m_router->connectVariableLengthArray(ahfits::e_READWRITE,"F_CHAN", 
                   rmfdat->m_matrixrow.m_f_chan, 
                   rmfdat->m_matrixrow.m_ngrp);
  rmfdat->m_matrixrow.m_router->connectVariableLengthArray(ahfits::e_READWRITE,"N_CHAN", 
                   rmfdat->m_matrixrow.m_n_chan, 
                   rmfdat->m_matrixrow.m_ngrp);
  rmfdat->m_matrixrow.m_router->connectVariableLengthArray(ahfits::e_READWRITE,"MATRIX", 
                   rmfdat->m_matrixrow.m_matrix, 
                   rmfdat->m_matrixrow.m_nmatrix);


}

void readMatrixRow(RMFData * rmfdat, double * totmatrix, int nchan, int rowidx) {

  int chanstart = -1;     // Starting channel number of a group
  int groupsize = -1;     // Number of channels in a group
  int mpos = 0;           // Position in matrix to get a channel element

  std::stringstream ss;   // for constructing error messages

  if (rmfdat == 0) {
    AH_THROW_RUNTIME("readMatrixRow called with unallocated RMFData structure");
  }

  if (nchan != rmfdat->m_nchan) {
    AH_THROW_RUNTIME("Number of elements for output energy mesh does not match expected value");
  }  

  // The rowidx variable is zero-based.  So the first row will have rowidx=0.
  if (rowidx < 0 || rowidx >= rmfdat->m_nmat) {
    ss.str("");
    ss << "Row index " << rowidx << " is outside range 0 - " << (rmfdat->m_nmat-1);
    AH_THROW_RUNTIME(ss.str());
  }

  // initialize the expanded row
  for (int i=0; i<nchan; ++i) {
    totmatrix[i] = 0.0;
  }

  // router already set up in openRMFFile
  ahfits::gotoRow(rmfdat->m_ahffp, rowidx+1);
  ahfits::readRow(rmfdat->m_ahffp);

  if (rmfdat->m_matrixrow.m_energ_lo != rmfdat->m_ematstart[rowidx]
    || rmfdat->m_matrixrow.m_energ_hi != rmfdat->m_ematstop[rowidx]) {
    AH_THROW_RUNTIME("ENERG_LO and ENERG_HI in MATRIX row out of sync with RMFData structure");
  }
 
  // Begin loop over all groups to read matrix row
  mpos = 0;
  for (int ii=0; ii<rmfdat->m_matrixrow.m_ngrp; ++ii) {

    chanstart = rmfdat->m_matrixrow.m_f_chan[ii];
    groupsize = rmfdat->m_matrixrow.m_n_chan[ii];

    for (int jj=0; jj<groupsize; ++jj) {
      totmatrix[chanstart + jj] = rmfdat->m_matrixrow.m_matrix[mpos];
      mpos++;
    }

  } // End loop over groups

} 


void writeMatrixRow(RMFData * rmfdat, double * totmatrix, int nchan, int rowidx, double effarea) {

  int chanstart = -1;     // Flag to indicate if a channel has been started.
  double sumall = 0.0;    // The sum of all counts in matrix
  double sumrow = 0.0;    // The sum of all counts in a particular group
  int mpos = 0;           // Position in matrix to write a channel element

  if (rmfdat == 0) {
    AH_THROW_RUNTIME("writeMatrixRow called with unallocated RMFData structure");
  }

  if (!rmfdat->m_write) {
    AH_THROW_RUNTIME("writeMatrixRow called for RMF file that is open for reading only");
  }

  if (nchan != rmfdat->m_nchan) {
    AH_THROW_RUNTIME("Number of elements for output energy mesh does not match expected value");
  }  

  // Intialize counts of total rmf groups and elements
 
  rmfdat->m_matrixrow.m_ngrp = 0;  
  rmfdat->m_matrixrow.m_nmatrix = 0;
 
  // Begin loop over all channels to write matrix row
 
  for(int jj = 0; jj < nchan; ++jj) {
    sumall += totmatrix[jj];

    // Start new group
    if(totmatrix[jj] > rmfdat->m_lo_thresh && chanstart < 0) { 
      chanstart = jj;
    }

    if((totmatrix[jj] < rmfdat->m_lo_thresh || jj == nchan-1) && chanstart >= 0) { // done with group
      // If we are closing the group because we are on the last channel,
      // then we need to increase the size of the group by one (since 
      // the current point needs to be included in the group as opposed
      // to the case where the current point is below the threshold)
      int addone = 0;
      if(jj == nchan-1 && totmatrix[jj] >= rmfdat->m_lo_thresh) addone=1;
      int numchaningroup = jj-chanstart+addone;
      if(numchaningroup>nchan) numchaningroup=nchan;      // just for safety

      // fill arrays for writing rmf
      rmfdat->m_matrixrow.m_f_chan[rmfdat->m_matrixrow.m_ngrp] = chanstart;
      rmfdat->m_matrixrow.m_n_chan[rmfdat->m_matrixrow.m_ngrp] = numchaningroup;  
      for(int kk = 0; kk < numchaningroup; ++kk) {
        if ((kk+chanstart) < nchan) rmfdat->m_matrixrow.m_matrix[mpos]=totmatrix[kk+chanstart];
        sumrow += rmfdat->m_matrixrow.m_matrix[mpos];
        mpos++;
      }
      // Reset starting position
      chanstart = -1;
      // Increment number of groups
      rmfdat->m_matrixrow.m_ngrp++;
    } 
  } // End loop over channels

  if (rmfdat->m_matrixrow.m_ngrp > MAX_NGRP) { 
    AH_THROW_RUNTIME("The number of escape peaks + 1 exceeds the MAX_NGRP variable in rmflib.h, please increase the value of this variable"); 
  }

  // Verify that sumrow is greater than one part in 10^6
  if((sumall/sumrow) > 1+1e-6) {
    AH_INFO(ahlog::LOW) << "With this threshold, matrix is not normalized to one part in 10^6 for index " 
                        << rowidx << " (row " << (rowidx+1) << ")" << std::endl;
  }

  // determine the size of the matrix column
  rmfdat->m_matrixrow.m_nmatrix = 0;
  for (int ii = 0; ii < rmfdat->m_matrixrow.m_ngrp; ++ii) {
    rmfdat->m_matrixrow.m_nmatrix += rmfdat->m_matrixrow.m_n_chan[ii];
  }

  // multiply by effective area 
  for (long ii=0; ii<rmfdat->m_matrixrow.m_nmatrix; ++ii) {
    rmfdat->m_matrixrow.m_matrix[ii] *= effarea;
  }

  // set energy_lo and energy_hi columns  
  rmfdat->m_matrixrow.m_energ_lo = rmfdat->m_ematstart[rowidx];
  rmfdat->m_matrixrow.m_energ_hi = rmfdat->m_ematstop[rowidx];

  // Update total number of rmf groups and elements
  rmfdat->m_numgrp += rmfdat->m_matrixrow.m_ngrp;
  rmfdat->m_numelt += rmfdat->m_matrixrow.m_nmatrix;

  // write rmf to fits file
  ahfits::lastRow(rmfdat->m_ahffp);
  ahfits::nextRow(rmfdat->m_ahffp);
  ahfits::writeRow(rmfdat->m_ahffp);

} 

void closeRMFFile(RMFData * rmfdat) {
  
  if (rmfdat == 0) {
    AH_THROW_RUNTIME("closeRMFFile called with unallocated RMFData structure");
  }

  if (rmfdat->m_matrixrow.m_router != 0) {
    delete rmfdat->m_matrixrow.m_router;
    rmfdat->m_matrixrow.m_router = 0;
  }

  if (rmfdat->m_ahffp != 0) {
    if (rmfdat->m_write) {
      // write DETCHANS, NUMGRP and NUMELT keywords 
      ahfits::move(rmfdat->m_ahffp, "EBOUNDS");  
      ahfits::writeKeyValLLong(rmfdat->m_ahffp, "DETCHANS", rmfdat->m_nchan, "Total number of detector PHA channels");
      ahfits::move(rmfdat->m_ahffp, "MATRIX");
      ahfits::writeKeyValLLong(rmfdat->m_ahffp, "DETCHANS", rmfdat->m_nchan, "Total number of detector PHA channels");
      ahfits::writeKeyValLLong(rmfdat->m_ahffp, "NUMGRP", rmfdat->m_numgrp, "Total number of groups");
      ahfits::writeKeyValLLong(rmfdat->m_ahffp, "NUMELT", rmfdat->m_numelt, "Total number of non-zero matrix elements");
    }
    ahfits::close(rmfdat->m_ahffp);
  }

  // deallocate memory from dynamic arrays
  if (rmfdat->m_ematstart != 0) delete [] rmfdat->m_ematstart; 
  if (rmfdat->m_ematstop != 0) delete [] rmfdat->m_ematstop;
  if (rmfdat->m_ematcen != 0) delete [] rmfdat->m_ematcen;
  if (rmfdat->m_echanstart != 0) delete [] rmfdat->m_echanstart;
  if (rmfdat->m_echanstop != 0) delete [] rmfdat->m_echanstop;
  if (rmfdat->m_echancen != 0) delete [] rmfdat->m_echancen;
  if (rmfdat->m_matrixrow.m_f_chan != 0) delete [] rmfdat->m_matrixrow.m_f_chan;
  if (rmfdat->m_matrixrow.m_n_chan != 0) delete [] rmfdat->m_matrixrow.m_n_chan;
  if (rmfdat->m_matrixrow.m_matrix != 0) delete [] rmfdat->m_matrixrow.m_matrix;
  rmfdat->m_ematstart = 0; 
  rmfdat->m_ematstop = 0;
  rmfdat->m_ematcen = 0;
  rmfdat->m_echanstart = 0;
  rmfdat->m_echanstop = 0;
  rmfdat->m_echancen = 0;
  rmfdat->m_matrixrow.m_f_chan = 0;
  rmfdat->m_matrixrow.m_n_chan = 0;
  rmfdat->m_matrixrow.m_matrix = 0;
}


void energy2Chan(double energy, double emin_mat, double de_mat, int nchan, int & channel) {

  // Calculate channel at specified energy

  channel = (int) (std::floor((energy-emin_mat)/de_mat));

  // Check the boundaries of the channels
  if(channel < 0) channel = 0;
  if(channel > nchan-1) channel = nchan-1;

}

void gaussianPeak(double Eo, int chanstart, int chanstop, int nchan, 
                  double fwhm, double * ecen, double * matrix, double & renorm) {
  
  double sigma = ahmath::convertFWHM2sigma(fwhm);
  
  // Reset matrix
  for(int ii = 0; ii < nchan; ++ii) matrix[ii] = 0.0;

  // Loop through channels to calculate RMF for gaussian 
  for(int ii = chanstart; ii < chanstop+1; ++ii) {
    // Calculate Gaussian Point ( e^(-0.5 * (E^2/sigma^2)/((2pi)^0.5)/sigma))
    matrix[ii] = exp((-0.5 * (ecen[ii]-Eo)*(ecen[ii]-Eo))/sigma/sigma)/std::sqrt(2.*M_PI)/sigma;
    renorm = renorm + matrix[ii];
  }

}

// ****************************************************************************

void exponentialShoulder(double Eo, int chanstart, int chanstop, int nchan, 
                         double tau, double * ecen, double * ematrix,
                         double & renorm) {

  // Calculate exponential to model the shape at low energy due to energy loss -
  // medium, large rmf

  // Reset matrix
  for(int ii = 0; ii < nchan; ++ii) ematrix[ii] = 0.0;

  // Loop through channels to calculate RMF for shoulder 
  for(int ii = chanstart; ii < chanstop+1; ++ii) {
    // Calculate exponential point (e^(E/tau)/tau))
    ematrix[ii] = exp((ecen[ii]-Eo)/tau)/tau;
  }

}

// ****************************************************************************

void continuumPeak(double Eo, int chanstart, int chanstop, int nchan, 
                   double * ecen, double * cmatrix, double & renorm) {

  // Calculate continuum component from the core to 0 eV, produced when a photon
  // interacts close to the insensitive layer and some charge is lost

  // Reset matrix
  for(int ii = 0; ii < nchan; ++ii) cmatrix[ii] = 0.0;

  // Loop through channels to calculate RMF for continuum
  for(int ii = chanstart; ii < chanstop+1; ++ii) {
    cmatrix[ii] = 1;
    // Set normalization factor
    renorm = renorm + cmatrix[ii];
  }

}

// ****************************************************************************



} // end namespace rmflib

// ****************************************************************************

/** Revision Log
 $Log: rmflib.cxx,v $
 Revision 1.39  2016/10/04 16:27:12  rshill
 Changed CHANNEL column in EBOUNDS extension from I type to J.

 Revision 1.38  2016/04/19 23:49:13  rshill
 Deallocate pointers before allocating.

 Revision 1.37  2016/04/14 15:05:53  rshill
 Write DETNAM keyword to output MATRIX and EBOUNDS extensions.

 Revision 1.36  2016/01/13 00:36:00  rshill
 Permissive file format on input - fixed or scalar
 F_CHAN, N_CHAN, some missing keywords.

 Revision 1.35  2016/01/12 01:30:02  rshill
 Added optional matrix EXTNAME parameter to openRMFFile().

 Revision 1.34  2016/01/08 22:08:44  mwitthoe
 rmflib: improve error message

 Revision 1.33  2016/01/08 19:04:39  mwitthoe
 rmflib: use local router in writeEboundsHDU()

 Revision 1.32  2016/01/08 18:21:05  mwitthoe
 rmflib: read/writeMatrixRow routines to have the rowidx variable be zero-based instead of one-based

 Revision 1.31  2016/01/08 15:34:09  rshill
 Now cleaning up router in writeEboundsHDU.

 Revision 1.30  2016/01/08 02:46:18  rshill
 Simplified EBOUNDS writing loop.  Corrected setting
 energ_lo and energ_hi in writeMatrixRow().

 Revision 1.29  2016/01/08 00:10:36  rshill
 Added functions to copy an RMFData structure; added multiplication
 by a scalar effective area to writeMatrixRow.

 Revision 1.28  2016/01/07 03:02:28  rshill
 Added allocation of storage for variable-length vectors.

 Revision 1.27  2016/01/07 00:59:03  rshill
 Corrected CSV* keyword names (date and time) to CVS*.

 Revision 1.26  2016/01/04 14:47:27  mdutka
 Updating log message for matrix not normalized

 Revision 1.25  2015/12/28 04:10:20  rshill
 Added detection of units (keV or not).

 Revision 1.24  2015/12/24 14:39:49  mwitthoe
 rmflib: 1) fix bug in readMatrixRow(), 2) allow to call readMatrixRow when in write-mode

 Revision 1.23  2015/12/24 01:43:02  rshill
 Added reading capability.  Made all calling sequences
 uniformly pass RMFData by pointer rather than by value.  Added error checks.  Cleaned up.

 Revision 1.22  2015/12/08 19:13:13  mdutka
 updating the max number of groups for output response matrix

 Revision 1.21  2015/11/17 22:39:54  mwitthoe
 rmflib: add column/keyword comments to output RMF file

 Revision 1.20  2015/10/19 18:45:13  mwitthoe
 rmflib: remove getPeaks() function which is no used by sxsrmf (or any other tool)

 Revision 1.19  2015/09/10 20:14:03  mdutka
 adding units to relavent column in ouput rmf file

 Revision 1.18  2015/08/10 16:13:09  mwitthoe
 rmflib: fix dimension bug in writeMatrixRow identified by Mike L

 Revision 1.17  2015/08/06 14:11:36  mwitthoe
 rmflib: fix a couple index problems allowing below-threshold RMF values to appear in the output and artifically chopping off the last element of an RMF vector with all values above threshold

 Revision 1.16  2015/07/31 17:21:37  mwitthoe
 rmflib: comment out std::cout debug statements; add some safety measures in the closeRMF function

 Revision 1.15  2015/07/27 14:52:53  mdutka
 updating rmflib after code cleanup july 2015

 Revision 1.14  2014/12/11 21:31:14  mdutka
 reversed the order of Matrix and Ebounds extensions

 Revision 1.13  2014/12/10 15:02:03  mdutka
 sumall/sumrow inequality reversed see issue #407

 Revision 1.12  2014/09/12 15:35:20  mdutka
 fixed the output energy grid writing, was actually writing channels

 Revision 1.10  2014/08/11 17:00:02  mdutka
 Added new functions which will use less system memory when writing the response matrix see issue #407

 Revision 1.9  2014/08/04 18:50:29  mdutka
 added type casting to channel variable

 Revision 1.8  2014/07/03 20:40:40  mdutka
 Changed continuum to be a flat line

 Revision 1.7  2014/06/27 20:23:55  mdutka
 removed renormalization update from exponentialShoulder.  renorm is now set in sxsrmf.cxx after convolution with gaussian function.

 Revision 1.6  2014/06/26 18:31:51  mdutka
 Changed the gaussian function and the exponetial function to be consistent with ahmath

 Revision 1.5  2014/06/24 18:14:32  asargent
 Added in upper bound for numchaningroup

 Revision 1.4  2014/06/24 17:52:18  asargent
 Simplified gaussianPeak, exponentialShoulder and continuumPeak functions with addition of energy2Chan function. Removed escapePeaks function. Fixed indexing error with calcMatrix.

 Revision 1.3  2014/06/03 14:57:16  asargent
 Updated calcMatrix function to include cutoff when groups reach a maximum.

 Revision 1.2  2014/05/27 15:16:05  asargent
 Fixed typo in AHLABEL definition

 Revision 1.1  2014/05/23 18:47:58  asargent
 New RMF tools library

*/
