/// \file remap.h
/// \brief functions to retrieve layer and coordinates of pixel in HXI and
///  SGD detectors
/// \author Mike Witthoeft
/// \date $Date: 2015/07/15 19:21:38 $

/// \addtogroup mod_hxisgdevtid
/// \section hxisgdevtid_remap HXI/SGD pixel mapping - remap
///
/// Constructs a mapping between the READOUT_ID_RMAP of a pixel and its
/// RAWX, RAWY, and LAYER_INDEX values.
///

#ifndef HXISGDEVTID_REMAP_H
#define HXISGDEVTID_REMAP_H

#include "ahgen/ahversion.h"
AHVERSION(HXISGDEVTID_REMAP,"$Id: remap.h,v 1.12 2015/07/15 19:21:38 klrutkow Exp $")

#include <sstream>
#include <string>
#include <vector>
#include <map>

/// \ingroup mod_hxisgdevtid

namespace hxisgdevtid {

namespace remap {

/** \addtogroup mod_hxisgdevtid
 *  @{
 */

/// \brief struct containing RAWX, RAWY, and LAYER_INDEX for single pixel
struct DataSubType {

  DataSubType(): m_layer(0), m_sensor_id(0), m_rawx(0), m_rawy(0),
                 m_camerax(0.0), m_cameray(0.0), m_cameraz(0.0),
                 m_asic_id(0), m_readout_id(0) {}

  DataSubType(int layer, int sensor_id, int rawx, int rawy,
              double camerax, double cameray, double cameraz,
              int asic_id, int readout_id): 
    m_layer(layer), m_sensor_id(sensor_id), m_rawx(rawx), m_rawy(rawy), 
    m_camerax(camerax), m_cameray(cameray), m_cameraz(cameraz),
    m_asic_id(asic_id), m_readout_id(readout_id) {}

  int m_layer;
  int m_sensor_id;
  int m_rawx;
  int m_rawy;
  double m_camerax;
  double m_cameray;
  double m_cameraz;
  int m_asic_id;
  int m_readout_id;
};

/// \brief Structure to hold the keywords describing SGD
///   pixel geometry in the remap CALDB file header.
struct GeomKeywords {
  int m_surside1;
  int m_surside2;
  int m_surside3;
  int m_surside4;
  int m_surbtm;
  double m_distan01;
  double m_distan02;
  double m_distan03;
  double m_distan04;
  double m_distan05;
  double m_delzsi;
  double m_delzcdte;
  double m_delxyany;
  int m_east_0;
  int m_north_0;
  int m_west_0;
  int m_south_0;
};

/// \brief map connecting READOUT_ID_RMAP to coordinates for all pixels
typedef std::map<int, DataSubType> DataType;


/// \brief read remapping file; load data into global data map and
///    geometric keywords into a structure
/// \param[in] filename name of remapping file
/// \param[in] instrume required value for the INSTRUME keyword in the given
///  file.
/// \param[out] remap_table map structure
void load(const std::string & filename, const std::string& instrume,
          DataType & map);

/// \brief read remapping file; load data into global data map and
///    geometric keywords into a structure
/// \param[in] filename name of remapping file
/// \param[in] instrume required value for the INSTRUME keyword in the given
///  file.
/// \param[out] geom detector geometry keywords from header
void loadGeomSGD(const std::string & filename, const std::string& instrume,
          GeomKeywords & geom);

/// \brief return LAYER for given READOUT_ID_RMAP value
int getLAYER(DataType& map, int readout_id_rmap);

/// \brief return LAYER_INDEX for given READOUT_ID_RMAP value
int getSENSOR_ID(DataType& map, int readout_id_rmap);

/// \brief return RAWX for given READOUT_ID_RMAP value
int getRAWX(DataType& map, int readout_id_rmap);

/// \brief return RAWY for given READOUT_ID_RMAP value
int getRAWY(DataType& map, int readout_id_rmap);

/// \brief return CAMERAX for given READOUT_ID_RMAP value
double getCAMERAX(DataType& map, int readout_id_rmap);

/// \brief return CAMERAY for given READOUT_ID_RMAP value
double getCAMERAY(DataType& map, int readout_id_rmap);

/// \brief return CAMERAZ for given READOUT_ID_RMAP value
double getCAMERAZ(DataType& map, int readout_id_rmap);

/// \brief return ASIC_ID for given READOUT_ID_RMAP value
int getASIC_ID(DataType& map, int readout_id_rmap);

/// \brief return READOUT_ID for given READOUT_ID_RMAP value
int getREADOUT_ID(DataType& map, int readout_id_rmap);


/** @} */

}  // namespace hxisgdevtid

}  // namespace remap

#endif /* HXISGDEVTID_REMAP_H */

/* Revision Log

 $Log: remap.h,v $
 Revision 1.12  2015/07/15 19:21:38  klrutkow
 removed local resolve function in favor of ahmission caldb resolve

 Revision 1.11  2015/07/13 00:15:44  klrutkow
 added resolve() function to get CALDB filename

 Revision 1.10  2014/12/15 21:27:44  rshill
 Corrected typo in getSensorID prototype.

 Revision 1.9  2014/06/24 17:49:26  rshill
 Split loading SGD geom keywords into a separate call.

 Revision 1.8  2014/06/13 00:41:17  rshill
 Geometry of layers in remap file header.

 Revision 1.7  2014/05/09 19:51:36  mwitthoe
 hxisgdevtidlib: update common CALDB access routines to use new versions of CALDB files

 Revision 1.6  2014/03/01 17:20:25  mwitthoe
 hxisgdevtid library: read ASIC_ID and READOUT_ID from the remapping file

 Revision 1.5  2014/01/30 21:29:30  rshill
 Changed camera coord inits to 0.0.

 Revision 1.4  2014/01/24 20:07:31  rshill
 Changed CAMERAX, CAMERAY, and CAMERAZ to double.

 Revision 1.3  2014/01/17 20:18:46  rshill
 Added SENSOR_ID to interface.

 Revision 1.2  2014/01/16 18:15:54  mwitthoe
 hxisgdevtid library: change outer namespace from hxisgd to hxisgdevtid

 Revision 1.1  2014/01/16 18:00:01  mwitthoe
 add hxisgdevtid library with CALDB access libraries used by the hxievtid and sgdevtid tools


*/
