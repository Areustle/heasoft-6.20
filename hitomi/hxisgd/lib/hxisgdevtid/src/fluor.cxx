/// \file fluor.cxx
/// \brief functions to act on the CALDB fluorescence file for HXI/SGD
/// \author Mike Witthoeft
/// \date $Date: 2016/12/05 19:52:39 $

#define AHLABEL hxisgdevtid_fluor
#define AHCVSID "$Id: fluor.cxx,v 1.7 2016/12/05 19:52:39 rshill Exp $"

#include "hxisgdevtid/fluor.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"

#include "headas.h"    // expand_item_list

namespace hxisgdevtid {

namespace fluor {

// ---------------------------------------------------------------------------

void load(const std::string& filename, const std::string& instrume, 
          DataType & fluor_table) {

  // variables for FITS columns
  std::string l_material;
  std::string l_fluorescence;
  double l_energy=0.;
  double l_linemin=0.;
  double l_linemax=0.;

  // check if table already loaded
  if (fluor_table.m_data != 0)
    AH_THROW_LOGIC("fluorescence CALDB data already loaded!");

  // open file
  ahfits::FilePtr fptr;
  ahfits::open(filename,"FLUORESCENCE",&fptr);
  if (!ahfits::readOK(fptr)) {
    AH_THROW_RUNTIME("failed to open fluorescence FITS file: "+filename);
  }

  // check that INSTRUME keyword has expected value
  std::string chk_instrume=ahfits::getKeyValStr(fptr,"INSTRUME");
  if (chk_instrume != instrume) {
    AH_THROW_RUNTIME("expecting FITS file with INSTRUME="+instrume+
                     "; given file ("+filename+") has INSTRUME="+chk_instrume);
  }

  // read resolution keyword
  if (instrume == "SGD") {
    // variables used in expand_item_list
    char** items=0;
    int nitems=0;
    int trim=1;      // trim spaces
    int skip=1;      // exclude empty items
    int guard=0;     // do not protect against commas in parentheses
    int status=0;

    // get energy uncertainity coefficients for Si layers
    std::string keyvalstr=ahfits::getKeyValStr(fptr,"ERRSI");
    items=expand_item_list((char*)keyvalstr.c_str(),&nitems,',',trim,skip,guard,&status);
    if (status != 0) AH_THROW_RUNTIME("invalid value for ERRSI parameter in fluorescence file");
    if (nitems != 3) AH_THROW_RUNTIME("expecting exactly three items in ERRSI keyword value");
    fluor_table.m_sgd_errsi_a_sqr=atof(items[0])*atof(items[0]);
    fluor_table.m_sgd_errsi_b_sqr=atof(items[1])*atof(items[1]);
    fluor_table.m_sgd_errsi_c_sqr=atof(items[2])*atof(items[2]);

    // get energy uncertainity coefficients for CdTe bottom layers
    keyvalstr=ahfits::getKeyValStr(fptr,"ERRCDBTM");
    items=expand_item_list((char*)keyvalstr.c_str(),&nitems,',',trim,skip,guard,&status);
    if (status != 0) AH_THROW_RUNTIME("invalid value for ERRCDBTM parameter in fluorescence file");
    if (nitems != 3) AH_THROW_RUNTIME("expecting exactly three items in ERRCDBTM keyword value");
    fluor_table.m_sgd_errcdbtm_a_sqr=atof(items[0])*atof(items[0]);
    fluor_table.m_sgd_errcdbtm_b_sqr=atof(items[1])*atof(items[1]);
    fluor_table.m_sgd_errcdbtm_c_sqr=atof(items[2])*atof(items[2]);

    // get energy uncertainity coefficients for CdTe side layers
    keyvalstr=ahfits::getKeyValStr(fptr,"ERRCDSID");
    items=expand_item_list((char*)keyvalstr.c_str(),&nitems,',',trim,skip,guard,&status);
    if (status != 0) AH_THROW_RUNTIME("invalid value for ERRCDSIDE parameter in fluorescence file");
    if (nitems != 3) AH_THROW_RUNTIME("expecting exactly three items in ERRCDSIDE keyword value");
    fluor_table.m_sgd_errcdsid_a_sqr=atof(items[0])*atof(items[0]);
    fluor_table.m_sgd_errcdsid_b_sqr=atof(items[1])*atof(items[1]);
    fluor_table.m_sgd_errcdsid_c_sqr=atof(items[2])*atof(items[2]);
  } else {
    std::string keyname_resol="HXIRESOL";
    fluor_table.m_resol=ahfits::getKeyValDbl(fptr,keyname_resol);
  }

  // get number of rows in input file and allocated space in struct
  int nrows=ahfits::numRows(fptr);
  fluor_table.m_data=new DataRow[nrows];
  fluor_table.m_size=nrows;

  // make connections between local variables and FITS columns
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_READONLY,"MaterialName",l_material);
  router.connectScalar(ahfits::e_READONLY,"Fluorescence",l_fluorescence);
  router.connectScalar(ahfits::e_READONLY,"Energy",l_energy);
  router.connectScalar(ahfits::e_READONLY,"Linemin",l_linemin);
  router.connectScalar(ahfits::e_READONLY,"Linemax",l_linemax);

  // read table
  int irow=0;
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    fluor_table.m_data[irow].m_material=l_material;
    fluor_table.m_data[irow].m_fluorescence=l_fluorescence;
    fluor_table.m_data[irow].m_energy=l_energy;
    fluor_table.m_data[irow].m_linemin=l_linemin;
    fluor_table.m_data[irow].m_linemax=l_linemax;
    irow++;
  }

  // close FITS file
  ahfits::close(fptr);
}

// ---------------------------------------------------------------------------

bool hasMatch(std::string material, double energy, const DataType& fluor_table, 
              std::string& fluorescence) {

  bool found_material=false;
  fluorescence="";
  for (int irow=0; irow < fluor_table.m_size; irow++) {
    if (material != fluor_table.m_data[irow].m_material) continue;
    found_material=true;
    if (energy < fluor_table.m_data[irow].m_linemin || 
        energy > fluor_table.m_data[irow].m_linemax) continue;
    fluorescence=fluor_table.m_data[irow].m_fluorescence;
    return true;
  }
  if (!found_material) {
    AH_THROW_RUNTIME("Material "+material+" does not exist in fluorescence table");
  }
  return false;
}

// ---------------------------------------------------------------------------

}  // namespace fluor

}  // namespace hxisgdevtid

/* Revision Log
 $Log: fluor.cxx,v $
 Revision 1.7  2016/12/05 19:52:39  rshill
 In hasMatch(), throw error if material used as key for the query
 is not in the fluorescence table at all, regardless of energy.

 Revision 1.6  2015/07/15 19:14:01  klrutkow
 updated error message

 Revision 1.5  2015/03/03 18:24:45  mwitthoe
 hxisgdevtid library: add common structures/functions for reconstruction and expand tools; add new keywords to fluorescence CALDB file for SGD energy test

 Revision 1.4  2014/05/09 19:51:36  mwitthoe
 hxisgdevtidlib: update common CALDB access routines to use new versions of CALDB files

 Revision 1.3  2014/01/31 15:00:48  rshill
 Added SGD resolutions for Si and CdTe.

 Revision 1.2  2014/01/16 18:15:54  mwitthoe
 hxisgdevtid library: change outer namespace from hxisgd to hxisgdevtid

 Revision 1.1  2014/01/16 18:00:02  mwitthoe
 add hxisgdevtid library with CALDB access libraries used by the hxievtid and sgdevtid tools


*/
