/// \file hxisgdremap.cxx
/// \brief functions to retrieve layer and coordinates of pixel in HXI and
///  SGD detectors
/// \author Mike Witthoeft
/// \date $Date: 2015/07/15 19:21:38 $

#define AHLABEL hxisgdevtid_hxisgdremap
#define AHCVSID "$Id: remap.cxx,v 1.11 2015/07/15 19:21:38 klrutkow Exp $"

#include "hxisgdevtid/remap.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"

#include "hdcal.h"

#include <iostream>
#include <fstream>
#include <string.h>
#include <sstream>
#include <cmath>

namespace hxisgdevtid {

namespace remap {
  

// ---------------------------------------------------------------------------

void load(const std::string & filename, const std::string& instrume,
          DataType & map) {

  // local variables to connect with FITS columns
  int l_readout_id_rmap=0;
  int l_layer=0;
  int l_sensor_id=0;
  int l_rawx=0;
  int l_rawy=0;
  double l_camerax=0.0;
  double l_cameray=0.0;
  double l_cameraz=0.0;
  int l_asic_id=0;
  int l_readout_id=0;

  ahfits::FilePtr fptr;        // ahfits file accessor for FITS file
  std::string chk_instrume;    // store INSTRUME keyword of FITS file

  // must clear() before loading again
  map.clear();

  // open file
  ahfits::open(filename,"REMAPPING",&fptr);  // opens into REMAPPING extension
  if (!ahfits::readOK(fptr)) {
    AH_THROW_RUNTIME("failed to open component remapping FITS file: "+filename);
  }

  // check that INSTRUME keyword has expected value
  chk_instrume=ahfits::getKeyValStr(fptr,"INSTRUME");
  if (chk_instrume != instrume) {
    AH_THROW_RUNTIME("expecting FITS file with INSTRUME="+instrume+
                     "; given file ("+filename+") has INSTRUME="+chk_instrume);
  }

  // setup router
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_READONLY,"READOUT_ID_RMAP",l_readout_id_rmap);
  router.connectScalar(ahfits::e_READONLY,"LAYER",l_layer);
  if (ahfits::haveColumn(fptr,"SENSOR_ID"))
    router.connectScalar(ahfits::e_READONLY,"SENSOR_ID",l_sensor_id);
  if (ahfits::haveColumn(fptr,"RAWX"))
    router.connectScalar(ahfits::e_READONLY,"RAWX",l_rawx);
  if (ahfits::haveColumn(fptr,"RAWY"))
    router.connectScalar(ahfits::e_READONLY,"RAWY",l_rawy);
  if (ahfits::haveColumn(fptr,"CAMERAX"))
    router.connectScalar(ahfits::e_READONLY,"CAMERAX",l_camerax);
  if (ahfits::haveColumn(fptr,"CAMERAY"))
    router.connectScalar(ahfits::e_READONLY,"CAMERAY",l_cameray);
  if (ahfits::haveColumn(fptr,"CAMERAZ"))
    router.connectScalar(ahfits::e_READONLY,"CAMERAZ",l_cameraz);
  if (ahfits::haveColumn(fptr,"ASIC_ID"))
    router.connectScalar(ahfits::e_READONLY,"ASIC_ID",l_asic_id);
  if (ahfits::haveColumn(fptr,"READOUT_ID"))
    router.connectScalar(ahfits::e_READONLY,"READOUT_ID",l_readout_id);

  // read in table, row by row
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    map.insert(std::make_pair(l_readout_id_rmap, 
      DataSubType(l_layer, l_sensor_id, l_rawx,l_rawy, l_camerax, l_cameray, 
                  l_cameraz, l_asic_id, l_readout_id)));
  }

  // close FITS file
  ahfits::close(fptr);
}


void loadGeomSGD(const std::string & filename, const std::string& instrume,
                 GeomKeywords & geom) {

  // local scratch variables for CdTe side identification
  int digit0=0;
  int digit1=0;

  ahfits::FilePtr fptr;        // ahfits file accessor for FITS file
  std::string chk_instrume;    // store INSTRUME keyword of FITS file

  // open file
  ahfits::open(filename,"REMAPPING",&fptr);  // opens into REMAPPING extension
  if (!ahfits::readOK(fptr)) {
    AH_THROW_RUNTIME("failed to open component remapping FITS file: "+filename);
  }

  // check that INSTRUME keyword has expected value
  chk_instrume=ahfits::getKeyValStr(fptr,"INSTRUME");
  if (chk_instrume != instrume) {
    AH_THROW_RUNTIME("expecting FITS file with INSTRUME="+instrume+
                     "; given file ("+filename+") has INSTRUME="+chk_instrume);
  }

  // get header keywords
  geom.m_surside1 = ahfits::getKeyValLLong(fptr, "SURSIDE1");
  geom.m_surside2 = ahfits::getKeyValLLong(fptr, "SURSIDE2");
  geom.m_surside3 = ahfits::getKeyValLLong(fptr, "SURSIDE3");
  geom.m_surside4 = ahfits::getKeyValLLong(fptr, "SURSIDE4");
  geom.m_surbtm   = ahfits::getKeyValLLong(fptr, "SURBTM");
  geom.m_distan01 = ahfits::getKeyValDbl(fptr, "DISTAN01");
  geom.m_distan02 = ahfits::getKeyValDbl(fptr, "DISTAN02");
  geom.m_distan03 = ahfits::getKeyValDbl(fptr, "DISTAN03");
  geom.m_distan04 = ahfits::getKeyValDbl(fptr, "DISTAN04");
  geom.m_distan05 = ahfits::getKeyValDbl(fptr, "DISTAN05");
  geom.m_delzsi   = ahfits::getKeyValDbl(fptr, "DELZSI");
  geom.m_delzcdte = ahfits::getKeyValDbl(fptr, "DELZCDTE");
  geom.m_delxyany = ahfits::getKeyValDbl(fptr, "DELXYANY");

  // derived geometric parameters (CdTe side layer identification)
  digit0 = geom.m_surside1/100;
  digit1 = geom.m_surside1 - 100*digit0;
  geom.m_east_0 = 100*digit0 + 10*digit1;
  digit0 = geom.m_surside2/100;
  digit1 = geom.m_surside2 - 100*digit0;
  geom.m_north_0 = 100*digit0 + 10*digit1;
  digit0 = geom.m_surside3/100;
  digit1 = geom.m_surside3 - 100*digit0;
  geom.m_west_0 = 100*digit0 + 10*digit1;
  digit0 = geom.m_surside4/100;
  digit1 = geom.m_surside4 - 100*digit0;
  geom.m_south_0 = 100*digit0 + 10*digit1;
}

int getLAYER(DataType& map, int readout_id_rmap) {
  return map[readout_id_rmap].m_layer;
}

int getSENSOR_ID(DataType& map, int readout_id_rmap) {
  return map[readout_id_rmap].m_sensor_id;
}

int getRAWX(DataType& map, int readout_id_rmap) {
  return map[readout_id_rmap].m_rawx;
}

int getRAWY(DataType& map, int readout_id_rmap) {
  return map[readout_id_rmap].m_rawy;
}

double getCAMERAX(DataType& map, int readout_id_rmap) {
  return map[readout_id_rmap].m_camerax;
}

double getCAMERAY(DataType& map, int readout_id_rmap) {
  return map[readout_id_rmap].m_cameray;
}

double getCAMERAZ(DataType& map, int readout_id_rmap) {
  return map[readout_id_rmap].m_cameraz;
}

int getASIC_ID(DataType& map, int readout_id_rmap) {
  return map[readout_id_rmap].m_asic_id;
}

int getREADOUT_ID(DataType& map, int readout_id_rmap) {
  return map[readout_id_rmap].m_readout_id;
}

// ---------------------------------------------------------------------------

}  // namespace remap

}  // namespace hxisgdevtid

/* Revision Log
 $Log: remap.cxx,v $
 Revision 1.11  2015/07/15 19:21:38  klrutkow
 removed local resolve function in favor of ahmission caldb resolve

 Revision 1.10  2015/07/13 00:16:04  klrutkow
 added resolve() function to get CALDB filename

 Revision 1.9  2014/06/24 17:49:52  rshill
 Split loading SGD geom keywords into a separate call.

 Revision 1.8  2014/06/13 00:41:37  rshill
 Geometry of layers in remap file header.

 Revision 1.7  2014/05/09 19:51:36  mwitthoe
 hxisgdevtidlib: update common CALDB access routines to use new versions of CALDB files

 Revision 1.6  2014/03/01 17:20:25  mwitthoe
 hxisgdevtid library: read ASIC_ID and READOUT_ID from the remapping file

 Revision 1.5  2014/01/30 21:31:08  rshill
 Removed duplicate SENSOR_ID connect and typo in CAMERAZ connect.

 Revision 1.4  2014/01/24 20:07:15  rshill
 Changed CAMERAX, CAMERAY, and CAMERAZ to double.

 Revision 1.3  2014/01/17 20:19:10  rshill
 Added SENSOR_ID to interface.

 Revision 1.2  2014/01/16 18:15:54  mwitthoe
 hxisgdevtid library: change outer namespace from hxisgd to hxisgdevtid

 Revision 1.1  2014/01/16 18:00:02  mwitthoe
 add hxisgdevtid library with CALDB access libraries used by the hxievtid and sgdevtid tools


*/
