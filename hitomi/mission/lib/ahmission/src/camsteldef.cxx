/// \brief camsteldef.cxx: Defines functions for the CAMSTelDef structure.
/// \author Timothy Reichard
/// \date $Date: 2016/02/19 01:56:55 $
///
/// This C++ source code file defines constructors and output functions
/// for the CAMSTelDef structure.

#define AHLABEL AHMISSION_CAMSTELDEF
#define AHCVSID "$Id: camsteldef.cxx,v 1.4 2016/02/19 01:56:55 rshill Exp $"

#include "ahmission/camsteldef.h"
#include "ahmission/ahmission.h"

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include <strings.h>

namespace ahmission {

namespace teldef {

// ================================================================================

CAMSTelDef::CAMSTelDef()
{
  m_filename = "";
  m_unit_name = "";
  m_unit_number = 0;

  m_x_location = 0.;
  m_y_location = 0.;
  m_z_location = 0.;
  m_rot_angle = 0.;
  m_x_flip = 1;
  m_y_flip = 1;
  m_scale = 1.;
  m_x_offset = 0.;
  m_y_offset = 0.;
}

// ----------------------------------------------------------------------

CAMSTelDef::CAMSTelDef(const std::string& cams_teldef_file, const int cams_unit_number)
{
  ahfits::FilePtr p_cams_teldef;
  std::string mission = "";
  std::string std_mission = "";
  std::string instrument = "";
  
  m_filename = cams_teldef_file;

  m_unit_number = cams_unit_number;
  if(cams_unit_number == 1)
    m_unit_name = "CAMS1";
  else if(cams_unit_number == 2)
    m_unit_name = "CAMS2";
  else
    {
      m_unit_name = "";
      AH_THROW_RUNTIME("Only CAMS1 and CAMS2 are valid CAMS instruments.");
    }

  // Open the CAMS TelDef file and move to the primary extension.

  ahfits::open(cams_teldef_file, "", &p_cams_teldef);

  // Check that this TelDef file is for the correct mission and CAMS unit.

  mission = getKeyValStr(p_cams_teldef, "TELESCOP");
  std_mission = ahmission::getTELESCOPString();
  if(!!strcasecmp(mission.c_str(), std_mission.c_str()))
    AH_THROW_RUNTIME("TelDef file " + cams_teldef_file + " is intended for mission " + mission + " rather than " + ahmission::getTELESCOPString() + ".");

  instrument = getKeyValStr(p_cams_teldef, "INSTRUME");
  if(!!strcasecmp(m_unit_name.c_str(), instrument.c_str()))
    AH_THROW_RUNTIME("TelDef file " + cams_teldef_file + " is intended for instrument " + std::string(instrument) + " rather than " + m_unit_name + ".");
  
  // Read the TelDef Keywords into the structure.
  
  m_x_location = getKeyValDbl(p_cams_teldef, "CAM_XLOC");
  m_y_location = getKeyValDbl(p_cams_teldef, "CAM_YLOC");
  m_z_location = getKeyValDbl(p_cams_teldef, "CAM_ZLOC");
  m_rot_angle = getKeyValDbl(p_cams_teldef, "CAM_ROTD");
  m_x_flip = getKeyValLLong(p_cams_teldef, "CAMXFLIP");
  m_y_flip = getKeyValLLong(p_cams_teldef, "CAMYFLIP");
  m_scale = getKeyValDbl(p_cams_teldef, "CAM_SCAL");
  m_x_offset = getKeyValDbl(p_cams_teldef, "CAM_XOFF");
  m_y_offset = getKeyValDbl(p_cams_teldef, "CAM_YOFF");

  // Close the file.

  ahfits::close(p_cams_teldef);
}

// ----------------------------------------------------------------------

st_stream::OStream& operator<< (st_stream::OStream& str, const CAMSTelDef& c)
{
  str << "  m_filename: " << c.m_filename << std::endl;
  str << "  m_unit_name: " << c.m_unit_name << std::endl;
  str << "  m_unit_number: " << c.m_unit_number << std::endl;
  str << "  m_x_location: " << c.m_x_location << std::endl;
  str << "  m_y_location: " << c.m_y_location << std::endl;
  str << "  m_z_location: " << c.m_z_location << std::endl;
  str << "  m_rot_angle: " << c.m_rot_angle << std::endl;
  str << "  m_x_flip: " << c.m_x_flip << std::endl;
  str << "  m_y_flip: " << c.m_y_flip << std::endl;
  str << "  m_scale: " << c.m_scale << std::endl;
  str << "  m_x_offset: " << c.m_x_offset << std::endl;
  str << "  m_y_offset: " << c.m_y_offset << std::endl;
  
  return str;
}

} //end namespace teldef

} //end namespace ahmission

// ================================================================================

/* Revision Log
   $Log: camsteldef.cxx,v $
   Revision 1.4  2016/02/19 01:56:55  rshill
   Recoded the string comparision of mission name to use std::string::c_str().

   Revision 1.3  2016/02/19 00:26:39  klrutkow
   use ahmission::getTELESCOPString() to set TELESCOP variable

   Revision 1.2  2015/03/25 21:53:29  asargent
   Added new keyword access x location

   Revision 1.1  2014/01/07 19:38:30  treichar
   Moved TelDef and CAMS unit data file I/O data structures and routines from cams2det task to ahmission library.

   Revision 1.6  2013/12/18 22:10:28  treichar
   Completed CFITSIO->ahfits conversion.

*/
