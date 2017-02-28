/// \file hxiteldef.cxx
/// \brief Defines the data structure and file-reading routings for non-standardHXI TelDef.
/// \author Timothy Reichard
/// \date $Date: 2016/02/19 01:56:54 $
///
/// This C++ source code file defines constructors and output functions
/// for the HXITelDef structure.

#define AHLABEL AHMISSION_HXITELDEF
#define AHCVSID "$Id: hxiteldef.cxx,v 1.4 2016/02/19 01:56:54 rshill Exp $"

#include "ahmission/hxiteldef.h"
#include "ahmission/ahmission.h"

#include "ahlog/ahlog.h"
#include "ahfits/ahfits.h"
#include <strings.h>

namespace ahmission {

namespace teldef {

// ================================================================================

HXITelDef::HXITelDef()
{
  m_filename = "";
  m_unit_name = "";
  m_unit_number = 0;

  m_x_location = 0.;
  m_y_location = 0.;
  m_z_location = 0.;
  m_rot_angle = 0.;
  m_x_phys_size = 0.;
  m_y_phys_size = 0.;
}

// ----------------------------------------------------------------------

HXITelDef::HXITelDef(std::string hxi_teldef_file, int hxi_unit_number)
{
  ahfits::FilePtr p_hxi_teldef;
  std::string mission = "";
  std::string std_mission = "";
  std::string instrument = "";
 
  m_filename = hxi_teldef_file;

  m_unit_number = hxi_unit_number;
  if(hxi_unit_number == 1)
    m_unit_name = "HXI1";
  else if(hxi_unit_number == 2)
    m_unit_name = "HXI2";
  else
    {
      m_unit_name = "";
      AH_THROW_RUNTIME("Only HXI1 and HXI2 are valid HXI instruments.");
    }

  // Open the HXI TelDef file and move to the primary extension.

  ahfits::open(hxi_teldef_file, "", &p_hxi_teldef);

  // Check that this TelDef file is for the correct mission and HXI unit.

  mission = getKeyValStr(p_hxi_teldef, "TELESCOP");
  std_mission = ahmission::getTELESCOPString();
  if(!!strcasecmp(mission.c_str(), std_mission.c_str()))
    AH_THROW_RUNTIME("TelDef file " + hxi_teldef_file + " is intended for mission " + mission + " rather than " + ahmission::getTELESCOPString() + ".");

  instrument = getKeyValStr(p_hxi_teldef, "INSTRUME");
  if(!!strcasecmp(m_unit_name.c_str(), instrument.c_str()))
    AH_THROW_RUNTIME("TelDef file " + hxi_teldef_file + " is intended for instrument " + std::string(instrument) + " rather than " + m_unit_name + ".");
  
  // Read the TelDef Keywords into the structure.
  
  m_x_location = getKeyValDbl(p_hxi_teldef, "HXI_XLOC");
  m_y_location = getKeyValDbl(p_hxi_teldef, "HXI_YLOC");
  m_z_location = getKeyValDbl(p_hxi_teldef, "HXI_ZLOC");
  m_rot_angle = getKeyValDbl(p_hxi_teldef, "HXI_ROTD");
  m_x_phys_size = getKeyValDbl(p_hxi_teldef, "HXI_XPHY");
  m_y_phys_size = getKeyValDbl(p_hxi_teldef, "HXI_YPHY");
  
  // Close the file.

  ahfits::close(p_hxi_teldef);
}

// ----------------------------------------------------------------------

st_stream::OStream& operator<< (st_stream::OStream& str, const HXITelDef& c)
{
  str << "  m_filename: " << c.m_filename << std::endl;
  str << "  m_unit_name: " << c.m_unit_name << std::endl;
  str << "  m_unit_number: " << c.m_unit_number << std::endl;
  str << "  m_x_location: " << c.m_x_location << std::endl;
  str << "  m_y_location: " << c.m_y_location << std::endl;
  str << "  m_z_location: " << c.m_z_location << std::endl;
  str << "  m_rot_angle: " << c.m_rot_angle << std::endl;
  str << "  m_x_phys_size: " << c.m_x_phys_size << std::endl;
  str << "  m_y_phys_size: " << c.m_y_phys_size << std::endl;
  
  return str;
}

} //end namespace teldef

} //end namespace ahmission

// ================================================================================

/* Revision Log
 $Log: hxiteldef.cxx,v $
 Revision 1.4  2016/02/19 01:56:54  rshill
 Recoded the string comparision of mission name to use std::string::c_str().

 Revision 1.3  2016/02/19 00:26:39  klrutkow
 use ahmission::getTELESCOPString() to set TELESCOP variable

 Revision 1.2  2015/03/25 21:53:29  asargent
 Added new keyword access x location

 Revision 1.1  2014/01/07 19:38:30  treichar
 Moved TelDef and CAMS unit data file I/O data structures and routines from cams2det task to ahmission library.

 Revision 1.6  2013/12/18 22:10:28  treichar
 Completed CFITSIO->ahfits conversion.

 Revision 1.5  2013/07/25 19:18:39  treichar
 Removed cleanup parameter, which is no longer needed due to switch from using ftcreate to ahfits for writing the output file.  Updated doxygen mark-up and comments
 throughout code.

*/
