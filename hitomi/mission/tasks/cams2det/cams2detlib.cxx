/// \file cams2detlib.cxx
/// \brief Defines the task-specific functions for the cams2det task.
/// \author Timothy Reichard
/// \date $Date: 2016/02/19 00:44:04 $
///
/// This source code file declares the task-specific enumerations and
/// functions needed for use in the cams2det task.  The functions are
/// declared in functions.h

#define AHLABEL TASKS_CAMS2DET_CAMS2DETLIB
#define AHCVSID "$Id: cams2detlib.cxx,v 1.18 2016/02/19 00:44:04 klrutkow Exp $"

#include "cams2detlib.h"
#include "ahmission/ahmission.h"
#include <string.h>
#include <iomanip>
#include <sstream>

// ======================================================================

CAMSHXIGeometry::CAMSHXIGeometry(const ahmission::teldef::CAMSTelDef& cams1,    
                                 const ahmission::teldef::CAMSTelDef& cams2,    
                                 const ahmission::teldef::HXITelDef& hxi_extra, 
                                 TELDEF2* hxi_std, const Params& param         
                                 )
{
  // Declare indices for the standard TelDef structure.

  int raw_sys = 0;
  int raw_min_seg = -1;
  int det_sys = 0;
  int det_min_seg = -1;
  int foc_sys = 0;
  int foc_min_seg = -1;

  // Declare intermediate variables for values obtained from TelDef.

  double rot_angle_foc = 0.0;
  double x_offset_foc = 0.0;
  double y_offset_foc = 0.0;
  double x_scale_det = 0.0;
  double y_scale_det = 0.0;
  
  // Get the coordinate system number of the RAW system in the 
  // HXI standard teldef structure.
  
  raw_sys = getCoordSystemNumberFromName(hxi_std, "RAW");
  if(raw_sys < 0)
    AH_THROW_RUNTIME("Cannot find RAW coordinate system in HXI TelDef file " + param.m_hxi_teldef_file + ".");
  raw_min_seg = hxi_std->min_segment[raw_sys];
  
  // Get the coordinate system number of the RAW system in the 
  // HXI standard teldef structure.
  
  det_sys = getCoordSystemNumberFromName(hxi_std, "DET");
  if(det_sys < 0)
    AH_THROW_RUNTIME("Cannot find DET coordinate system in HXI TelDef file " + param.m_hxi_teldef_file + ".");
  det_min_seg = hxi_std->min_segment[det_sys];
  
  // Get the coordinate system number of the FOC system in the 
  // HXI standard teldef structure.
  
  foc_sys = getCoordSystemNumberFromName(hxi_std, "FOC");
  if(foc_sys < 0)
    AH_THROW_RUNTIME("Cannot find FOC coordinate system in HXI TelDef file " + param.m_hxi_teldef_file + ".");
  foc_min_seg = hxi_std->min_segment[foc_sys];

  // Calculate x and y distances between CAMS units

  m_r12x = cams2.m_x_location - cams1.m_x_location;
  m_r12y = cams2.m_y_location - cams1.m_y_location;

  // Calculate cosines and sine of CAMS and HXI rotation angles.

  m_cos_cams1 = cos(cams1.m_rot_angle * M_PI/180.);
  m_sin_cams1 = sin(cams1.m_rot_angle * M_PI/180.);
  m_cos_cams2 = cos(cams2.m_rot_angle * M_PI/180.);
  m_sin_cams2 = sin(cams2.m_rot_angle * M_PI/180.);
  m_cos_hxi = cos(hxi_extra.m_rot_angle * M_PI/180.);
  m_sin_hxi = sin(hxi_extra.m_rot_angle * M_PI/180.);
  rot_angle_foc = hxi_std->basicparam[foc_sys-1]->rotangle;
  m_cos_foc = cos(rot_angle_foc * M_PI/180.);
  m_sin_foc = sin(rot_angle_foc * M_PI/180.);

  // Calculate the offsets from the HXI center to the center of the RAW
  // coordinate system.

  m_raw_xcen = (hxi_std->coordsys[raw_sys][raw_min_seg]->npixels_x - 1)/2.0 + hxi_std->coordsys[raw_sys][raw_min_seg]->first_pixel_x;
  m_raw_ycen = (hxi_std->coordsys[raw_sys][raw_min_seg]->npixels_y - 1)/2.0 + hxi_std->coordsys[raw_sys][raw_min_seg]->first_pixel_y;
  m_raw_xhxi = 0.5*hxi_std->coordsys[raw_sys][raw_min_seg]->first_pixel_x + 0.5*hxi_extra.m_x_phys_size/hxi_std->coordsys[raw_sys][raw_min_seg]->scale_x;
  m_raw_yhxi = 0.5*hxi_std->coordsys[raw_sys][raw_min_seg]->first_pixel_y + 0.5*hxi_extra.m_y_phys_size/hxi_std->coordsys[raw_sys][raw_min_seg]->scale_y;

  m_x_offset_px = m_raw_xcen - m_raw_xhxi;
  m_y_offset_px = m_raw_ycen - m_raw_yhxi;
  m_x_offset_mm_raw = m_x_offset_px * hxi_std->coordsys[raw_sys][raw_min_seg]->scale_x;
  m_y_offset_mm_raw = m_y_offset_px * hxi_std->coordsys[raw_sys][raw_min_seg]->scale_y;

  // Convert offsets to SAT system.

  if (param.m_starting_sys == "RAW") {
    m_x_offset_mm = m_cos_hxi*m_x_offset_mm_raw - m_sin_hxi*m_y_offset_mm_raw;
    m_y_offset_mm = m_sin_hxi*m_x_offset_mm_raw + m_cos_hxi*m_y_offset_mm_raw;
  } else if (param.m_starting_sys == "FOC") {
    x_offset_foc = hxi_std->basicparam[foc_sys-1]->xoffset;
    y_offset_foc = hxi_std->basicparam[foc_sys-1]->yoffset;
    x_scale_det = hxi_std->coordsys[det_sys][det_min_seg]->scale_x;
    y_scale_det = hxi_std->coordsys[det_sys][det_min_seg]->scale_y;
    m_x_offset_mm = (m_cos_foc*x_offset_foc - m_sin_foc*y_offset_foc)*x_scale_det;
    m_y_offset_mm = (m_sin_foc*x_offset_foc + m_cos_foc*y_offset_foc)*y_scale_det;
  } else {
    AH_THROW_RUNTIME("Starting coordinate system " + param.m_starting_sys + " is not supported.");
  }

  AH_DEBUG << "rot_angle_foc=" << rot_angle_foc <<
              " x_offset_foc=" << x_offset_foc <<
              " y_offset_foc=" << y_offset_foc <<
              " x_scale_det=" << x_scale_det <<
              " y_scale_det=" << y_scale_det << std::endl;
  
  // Calculate the distance from the center of the two CAMS units 
  // to the center of the HXI RAW system.

  m_x_cam_hxi = 0.5*(cams1.m_x_location + cams2.m_x_location) - (hxi_extra.m_x_location + m_x_offset_mm);
  m_y_cam_hxi = 0.5*(cams1.m_y_location + cams2.m_y_location) - (hxi_extra.m_y_location + m_y_offset_mm);

  // Print these calculated values in debug mode.

  AH_DEBUG << this << std::endl;
}

// ----------------------------------------------------------------------

st_stream::OStream& operator<<(st_stream::OStream& str, const CAMSHXIGeometry& g)
{
  // Display the values of the structure members.
  
  str << "\n\nDerived CAMS & HXI geometric properties:\n"
      << "  r12x: " << g.m_r12x << "\n"
      << "  r12y: " << g.m_r12y << "\n"
      << "  raw_xcen: " << g.m_raw_xcen << "\n"
      << "  raw_ycen: " << g.m_raw_ycen << "\n"
      << "  raw_xhxi: " << g.m_raw_xhxi << "\n"
      << "  raw_yhxi: " << g.m_raw_yhxi << "\n"
      << "  x_offset_px: " << g.m_x_offset_px << "\n"
      << "  y_offset_px: " << g.m_y_offset_px << "\n"
      << "  x_offset_mm_raw: " << g.m_x_offset_mm_raw << "\n"
      << "  y_offset_mm_raw: " << g.m_y_offset_mm_raw << "\n"
      << "  x_offset_mm: " << g.m_x_offset_mm << "\n"
      << "  y_offset_mm: " << g.m_y_offset_mm << "\n"
      << "  x_cam_hxi: " << g.m_x_cam_hxi << "\n"
      << "  y_cam_hxi: " << g.m_y_cam_hxi << std::endl;
  
  return str;
}

// ----------------------------------------------------------------------

ahfits::Router* CAMSOffsetsDataRow::connectColumns(ahfits::FilePtr& fp,
                                                   const ahfits::RWModeEnum mode)
{
  // Connect all the columns to a router and return a pointer to the router.

  ahfits::Router* p_rt = new ahfits::Router(fp); 
  p_rt->connectScalar(mode, "TIME", m_t);
  p_rt->connectScalar(mode, "DELTARAWX", m_dx);
  p_rt->connectScalar(mode, "DELTARAWY", m_dy);
  p_rt->connectScalar(mode, "COSANGLE", m_cos_twist);
  p_rt->connectScalar(mode, "SINANGLE", m_sin_twist);
  p_rt->connectScalar(mode, "X1", m_x1);
  p_rt->connectScalar(mode, "Y1", m_y1);
  p_rt->connectScalar(mode, "X2", m_x2);
  p_rt->connectScalar(mode, "Y2", m_y2);
  p_rt->connectScalar(mode, "JUMPX1", m_jump_x1);
  p_rt->connectScalar(mode, "JUMPY1", m_jump_y1);
  p_rt->connectScalar(mode, "JUMPX2", m_jump_x2);
  p_rt->connectScalar(mode, "JUMPY2", m_jump_y2);
  p_rt->connectScalar(mode, "QUALITY1", m_q1);
  p_rt->connectScalar(mode, "QUALITY2", m_q2);
  p_rt->connectScalar(mode, "XDISTANCE", m_x_dist);
  p_rt->connectScalar(mode, "YDISTANCE", m_y_dist);
  p_rt->connectScalar(mode, "DELTASATX", m_dx_sat);
  p_rt->connectScalar(mode, "DELTASATY", m_dy_sat);
  p_rt->connectScalar(mode, "BAD_UNITS", m_bad_units);
  p_rt->connectScalar(mode, "CALC_QUALITY", m_calc_quality);

  return p_rt;
}

// ----------------------------------------------------------------------

void CAMSOffsetsDataRow::setRowToNull()
{
  // Initialize the structure members to NULL or default values.

  m_t = DOUBLENULL;
  m_dx = DOUBLENULL;
  m_dy = DOUBLENULL;
  m_cos_twist = DOUBLENULL;
  m_sin_twist = DOUBLENULL;
  m_x1 = INTNULL;
  m_y1 = INTNULL;
  m_x2 = INTNULL;
  m_y2 = INTNULL;
  m_jump_x1 = INTNULL;
  m_jump_y1 = INTNULL;
  m_jump_x2 = INTNULL;
  m_jump_y2 = INTNULL;
  m_q1 = QNULL;
  m_q2 = QNULL;
  m_x_dist = DOUBLENULL;
  m_y_dist = DOUBLENULL;
  m_dx_sat = DOUBLENULL;
  m_dy_sat = DOUBLENULL;
  m_bad_units = e_CAMS12_OK;
  m_calc_quality = 0;
}

// ----------------------------------------------------------------------

void CAMSOffsetsDataRow::fillWithCAMSUnitData(const ahmission::camsio::UnitDataRow& data, const int unit)
{
  // Copy the x and y positions and quality flag from the CAMS unit
  // data row to the offsets file row.  If either of x and y are null,
  // set both to null.
  
  if(unit == 1)
    {
      if(data.m_x_raw == INTNULL || data.m_y_raw == INTNULL)
        {
          m_x1 = INTNULL;
          m_y1 = INTNULL;
        }
      else
        {
          m_x1 = data.m_x;
          m_y1 = data.m_y;
        }
      m_q1 = data.m_q;
    }
  else if(unit == 2)
    {
      if(data.m_x_raw == INTNULL || data.m_y_raw == INTNULL)
        {
          m_x2 = INTNULL;
          m_y2 = INTNULL;
        }
      else
        {
          m_x2 = data.m_x;
          m_y2 = data.m_y;
        }
      m_q2 = data.m_q;
    }
}

// ----------------------------------------------------------------------

int calcPositionJump(const int value, const int last_good_value, 
                     const int null_value)
{
  // Set the jump to the difference between the current value and the
  // last good value if both are non-null. Set the jump to null if
  // either the current or last good value is null.

  if(value == null_value)
    return null_value;
  else if(last_good_value == null_value)
    return null_value;
  else 
    return value - last_good_value;
}

// ----------------------------------------------------------------------

void calcCAMSDisplacements(CAMSOffsetsDataRow& off, double& last_good_cos_twist, 
                           double& last_good_sin_twist, const CAMSHXIGeometry geom,  
                           const ahmission::teldef::CAMSTelDef& cams1, 
                           const ahmission::teldef::CAMSTelDef& cams2,     
                           const ahmission::teldef::HXITelDef& hxi_extra, TELDEF2* hxi_std,            
                           const bool start_from_raw,
                           const int raw_sys, const int raw_min_seg,
                           const int foc_sys, const int foc_min_seg
                           )
{
  // Declare input position values.

  double x1_px = 0;
  double y1_px = 0;
  double x2_px = 0;
  double y2_px = 0;

  // Declare position values in mm.

  double x1_mm = 0;
  double y1_mm = 0;
  double x2_mm = 0;
  double y2_mm = 0;

  // Declare flipped position values.

  double x1_flip = 0;
  double y1_flip = 0;
  double x2_flip = 0;
  double y2_flip = 0;

  // Declare positions after rotating and after shifting.

  double x1 = 0;
  double y1 = 0;
  double x2 = 0;
  double y2 = 0;

  double x1_shift = 0.;
  double y1_shift = 0.;
  double x2_shift = 0.;
  double y2_shift = 0.;

  // Declare vector arithmetic variables.

  double r12_dot_r12s = 0.;
  double r12_r12s_mag = 0.;
  double r12_cross_r12s = 0.;
  double r12_r12s_mag_cross = 0.;

  // Declare distances from center of CAMS unit to center of HXI RAW system.

  double cams_hxi_x_shift = 0.;
  double cams_hxi_y_shift = 0.;
  
  // Declare offsets in the SAT coordinate system.

  double delta_x_mm = 0.;
  double delta_y_mm = 0.;

  // Declare offsets in the HXI RAW coordinate system.

  double dx = 0.;
  double dy = 0.;


  // Declare displacement correction factor for differing heights
  // of CAMS and HXI.

  double height_factor = 0.;

  // Declare the twist angle and last calculated non-null twist angle
  // in the SAT coordinate system.

  double cos_angle = 1.;
  double sin_angle = 0.;

  double last_good_cos_angle = 1.;
  double last_good_sin_angle = 0.;
  
  // Declare a message stream.

  std::stringstream message;

  // Invert the twist angle's previous good value to match the angle
  // used in the computation, which has the opposite sign.  At the end
  // of the computation, the sign will be flipped back. Null values
  // shouldn't be negated.
  
  if(isDoubleNull(last_good_cos_twist))
    last_good_cos_angle = DOUBLENULL;
  else
    last_good_cos_angle = last_good_cos_twist;

  if(isDoubleNull(last_good_sin_twist))
    last_good_sin_angle = DOUBLENULL;
  else
    last_good_sin_angle = -last_good_sin_twist;

  // Transform the CAMS quantities from CAMS system (bits) to SAT
  // system (mm).  (The input values (bits) of the input files were
  // changed to units of mm after the initial algorithm was
  // written. The original transformation was nontrivial.)

  x1_px = off.m_x1;
  y1_px = off.m_y1;
  x2_px = off.m_x2;
  y2_px = off.m_y2;

  // Subtract offset and convert to mm using scale from TelDef file.

  x1_mm = (x1_px - cams1.m_x_offset) * cams1.m_scale;
  y1_mm = (y1_px - cams1.m_y_offset) * cams1.m_scale;
  x2_mm = (x2_px - cams2.m_x_offset) * cams2.m_scale;
  y2_mm = (y2_px - cams2.m_y_offset) * cams2.m_scale;

  // Flip axes.

  x1_flip = x1_mm * cams1.m_x_flip;
  y1_flip = y1_mm * cams1.m_y_flip;
  x2_flip = x2_mm * cams2.m_x_flip;
  y2_flip = y2_mm * cams2.m_y_flip;

  // Rotate about x- or y-axis.

  x1 = geom.m_cos_cams1 * x1_flip - geom.m_sin_cams1 * y1_flip;
  y1 = geom.m_sin_cams1 * x1_flip + geom.m_cos_cams1 * y1_flip;
  x2 = geom.m_cos_cams2 * x2_flip - geom.m_sin_cams2 * y2_flip;
  y2 = geom.m_sin_cams2 * x2_flip + geom.m_cos_cams2 * y2_flip;

  // The algorithm's next steps vary according to whether one or both
  // CAMS units have valid data.  Validity is determined by comparing
  // x and y values to null values.

  off.m_bad_units = e_CAMS12_OK;
  off.m_calc_quality = e_QF_OK;

  if(off.m_x1 == INTNULL || off.m_y1 == INTNULL)
    off.m_bad_units |= e_CAMS1_BAD;
  if(off.m_x2 == INTNULL || off.m_y2 == INTNULL)
    off.m_bad_units |= e_CAMS2_BAD;

  if(off.m_bad_units == e_CAMS12_OK)
    {
      // Case: Both CAMS units have valid data, so calculate displacements in both position and angle.
      // Derive the shifted CAMS positions and shifted distances between CAMS units.

      x1_shift = cams1.m_x_location + x1;
      y1_shift = cams1.m_y_location + y1;
      x2_shift = cams2.m_x_location + x2;
      y2_shift = cams2.m_y_location + y2;

      off.m_x_dist = x2_shift - x1_shift;
      off.m_y_dist = y2_shift - y1_shift;

      // Do some needed vector arithmetic.

      r12_dot_r12s = geom.m_r12x * off.m_x_dist + geom.m_r12y * off.m_y_dist; // dot product
      r12_r12s_mag = sqrt(geom.m_r12x*geom.m_r12x + geom.m_r12y*geom.m_r12y) * 
        sqrt(off.m_x_dist*off.m_x_dist + off.m_y_dist*off.m_y_dist); // product of magnitudes.
      r12_cross_r12s = geom.m_r12x * off.m_y_dist - geom.m_r12y * off.m_x_dist; // co-planar cross product
      r12_r12s_mag_cross = sqrt(r12_cross_r12s * r12_cross_r12s); // mag. of cross product

      // Calculate the cosine and sine of the displacement angle.

      cos_angle = r12_dot_r12s/r12_r12s_mag;
      if(r12_r12s_mag_cross != 0 && 1. - cos_angle*cos_angle >= 0.)
        sin_angle = r12_cross_r12s/r12_r12s_mag_cross * sqrt(1.0 - cos_angle*cos_angle);
      else
        {
          // If the magnitude of the cross product is zero, so is the
          // cross product.  cos_angle is unity in this case, and so
          // sin_angle is 0.

          sin_angle = 0.;
          off.m_calc_quality |= e_QF_FIXED_SINE;
          AH_DEBUG << "sin_angle has been set to 0. e_QF_FIXED_SINE flagged." << std::endl;
        }

      // Fix angle if abs(cos_angle) > 1 due to numerical precision errors.

      if(cos_angle > 1.)
        {
          cos_angle = 1.0;
          sin_angle = 0.0;
          off.m_calc_quality |= e_QF_FIXED_POS_COSINE;
          AH_DEBUG << "cos_angle has been set to 1. e_QF_FIXED_POS_COSINE flagged." << std::endl;
        }
      else if(cos_angle < -1.)
        {
          cos_angle = -1.0;
          sin_angle = 0.0;
          off.m_calc_quality |= e_QF_FIXED_NEG_COSINE;
          AH_DEBUG << "cos_angle has been set to -1. e_QF_FIXED_NEG_COSINE flagged." << std::endl;
        }

      // This angle is now the last good one.

      last_good_cos_angle = cos_angle;
      last_good_sin_angle = sin_angle;

      // Calculate the distance from the center of the two shifted CAMS to the center of
      // the HXI RAW system.

      cams_hxi_x_shift = (x1_shift + x2_shift)/2.0 - (hxi_extra.m_x_location + geom.m_x_offset_mm);
      cams_hxi_y_shift = (y1_shift + y2_shift)/2.0 - (hxi_extra.m_y_location + geom.m_y_offset_mm);

      // Calculate the offsets in the SAT system.

      off.m_dx_sat = cams_hxi_x_shift - cos_angle*geom.m_x_cam_hxi + sin_angle*geom.m_y_cam_hxi;
      off.m_dy_sat = cams_hxi_y_shift - sin_angle*geom.m_x_cam_hxi - cos_angle*geom.m_y_cam_hxi;

      // Apply the correction to the offsets based on the relative heights of the HXI
      // and CAMS units above the EOB.

      height_factor = hxi_extra.m_z_location/((cams1.m_z_location + cams2.m_z_location)/2.0);  // Use mean of CAMS heights.
      off.m_dx_sat *= height_factor;
      off.m_dy_sat *= height_factor;


    } // end case: CAMS1 and CAMS2 both have good data.
  else if(off.m_bad_units != e_CAMS12_BAD) // One but not two CAMS units has good data.
    {
      // Set displacement angle to the last good angle and flag this as NO_ANGLE.

      cos_angle = last_good_cos_angle;
      sin_angle = last_good_sin_angle;
      off.m_calc_quality |= e_QF_NO_ANGLE;

      // Set the displacement position based on one CAMS unit.

      if((off.m_bad_units & e_CAMS1_BAD) == 0)
        {
          // CAMS1 data are good.  CAMS2 data are bad.

          off.m_dx_sat = x1;
          off.m_dy_sat = y1;
          height_factor = hxi_extra.m_z_location/cams1.m_z_location;
          off.m_dx_sat *= height_factor;
          off.m_dy_sat *= height_factor;
          
          x1_shift = cams1.m_x_location + x1;
          y1_shift = cams1.m_y_location + y1;
          x2_shift = cams2.m_x_location;
          y2_shift = cams2.m_y_location;
        }
      else
        {
          // CAMS2 data are good.  CAMS1 data are bad.

          off.m_dx_sat = x2;
          off.m_dy_sat = y2;
          height_factor = hxi_extra.m_z_location/cams2.m_z_location;
          off.m_dx_sat *= height_factor;
          off.m_dy_sat *= height_factor;

          x1_shift = cams1.m_x_location;
          y1_shift = cams1.m_y_location;
          x2_shift = cams2.m_x_location + x2;
          y2_shift = cams2.m_y_location + y2;
        }
      
      // Cannot calculate distance, so set to null.

      off.m_x_dist = DOUBLENULL;
      off.m_y_dist = DOUBLENULL;
    } // end case: one but not two CAMS units has good data.
  else
    {
      // Neither CAMS unit has good data.

      cos_angle = DOUBLENULL;
      sin_angle = DOUBLENULL;
      off.m_dx_sat = DOUBLENULL;
      off.m_dy_sat = DOUBLENULL;
    }

  // Convert delta_x|y_sc to HXI system by rotating to HXI axes, and then rescale to HXI pixels.  
  // Null values remain null.  
  
  if(off.m_bad_units == e_CAMS12_BAD)
    {
      dx = DOUBLENULL;
      dy = DOUBLENULL;
      off.m_dx = DOUBLENULL;
      off.m_dy = DOUBLENULL;
    }
  else
    {
      if (start_from_raw)
        {
          delta_x_mm = geom.m_cos_hxi * off.m_dx_sat - geom.m_sin_hxi * off.m_dy_sat;
          delta_y_mm = geom.m_sin_hxi * off.m_dx_sat + geom.m_cos_hxi * off.m_dy_sat;
          dx = delta_x_mm/hxi_std->coordsys[raw_sys][raw_min_seg]->scale_x;
          dy = delta_y_mm/hxi_std->coordsys[raw_sys][raw_min_seg]->scale_y;
        }
      else
        {
          delta_x_mm = off.m_dx_sat;
          delta_y_mm = -off.m_dy_sat;
          dx = delta_x_mm/hxi_std->coordsys[foc_sys][foc_min_seg]->scale_x;
          dy = delta_y_mm/hxi_std->coordsys[foc_sys][foc_min_seg]->scale_y;
        }
      

      AH_INFO(ahlog::LOW) << "x1=" << x1 
               << " y1=" << y1 
               << " x2=" << x2 
               << " y2=" << y2 
               << " delta_x_sc=" << off.m_dx_sat 
               << " delta_y_sc=" << off.m_dy_sat 
               << " dx=" << dx 
               << " dy=" << dy 
               << " raw scale_x=" << hxi_std->coordsys[raw_sys][raw_min_seg]->scale_x 
               << " raw scale_y=" << hxi_std->coordsys[raw_sys][raw_min_seg]->scale_y 
               << " foc scale_x=" << hxi_std->coordsys[foc_sys][foc_min_seg]->scale_x 
               << " foc scale_y=" << hxi_std->coordsys[foc_sys][foc_min_seg]->scale_y 
               << " dx_mm=" << delta_x_mm 
               << " dy_mm=" << delta_y_mm 
               << std::endl;

      // Convert values from ACT->RAW to RAW->ACT values. This
      // involves a rotation of the offsets to put them in RAW pixel
      // units and a negation of the twist angle. Null angles should
      // remain null.

      if(isDoubleNull(cos_angle))
        off.m_cos_twist = DOUBLENULL;
      else
        off.m_cos_twist = cos_angle;

      if(isDoubleNull(sin_angle))
        off.m_sin_twist = DOUBLENULL;
      else
        if (start_from_raw)  // Do not reverse sign for FOC transformation
          {
            off.m_sin_twist = -sin_angle;
          }
      
      if(isDoubleNull(last_good_cos_angle))
        last_good_cos_twist = DOUBLENULL;
      else
        last_good_cos_twist = last_good_cos_angle;

      if(isDoubleNull(last_good_sin_angle))
        last_good_sin_twist = DOUBLENULL;
      else
        last_good_sin_twist = -last_good_sin_angle;
      
      off.m_dx = -off.m_cos_twist*dx + off.m_sin_twist*dy;
      off.m_dy = -off.m_sin_twist*dx - off.m_cos_twist*dy;
    }
}

// ----------------------------------------------------------------------

void openCAMSDataExtension(ahfits::FilePtr& fp, const std::string& filename,
                           const std::string& ext_name, const std::string& unit_name, long& n_rows)
{
  // Declare strings to store the TELESCOP and INSTRUME keywords.

  std::string mission = "";
  std::string instrument = "";

  // Move to data extension.

  ahfits::move(fp, ext_name);
  
  // Check the mission and instrument keywords for compatibility with HITOMI CAMS units.
  
  mission = getKeyValStr(fp, "TELESCOP");
  if(mission != ahmission::getTELESCOPString())
    AH_INFO(ahlog::HIGH) << "TELESCOP = '" + mission + "' in extension " + ext_name 
      + " of file " + filename + ", but only " + ahmission::getTELESCOPString() + " files may be used." << std::endl;
  
  instrument = getKeyValStr(fp, "INSTRUME");
  if(0 != strcasecmp(instrument.c_str(), unit_name.c_str()))
    AH_INFO(ahlog::HIGH) << "INSTRUME = '" + std::string(instrument) + "' in extension " + ext_name 
      + " of file " + filename + ", but this file was expected to have data from " + unit_name + "." << std::endl;
     
  // Check for the expected table columns.

  if(!haveColumn(fp, "TIME"))
    AH_THROW_RUNTIME("Cannot find column 'TIME' in " + ext_name + " extension of file " + filename + ".");

  if(!haveColumn(fp, "X_RAW"))
    AH_THROW_RUNTIME("Cannot find column 'X_RAW' in " + ext_name + " extension of file " + filename + ".");

  if(!haveColumn(fp, "Y_RAW"))
    AH_THROW_RUNTIME("Cannot find column 'Y_RAW' in " + ext_name + " extension of file " + filename + ".");

  if(!haveColumn(fp, "X"))
    AH_THROW_RUNTIME("Cannot find column 'X' in " + ext_name + " extension of file " + filename + ".");

  if(!haveColumn(fp, "Y"))
    AH_THROW_RUNTIME("Cannot find column 'Y' in " + ext_name + " extension of file " + filename + ".");

  if(!haveColumn(fp, "QUALITY"))
    AH_THROW_RUNTIME("Cannot find column 'QUALITY' in " + ext_name + " extension of file " + filename + ".");

  if(!haveColumn(fp, "PROC_STATUS"))
    AH_THROW_RUNTIME("Cannot find column 'PROC_STATUS' in " + ext_name + " extension of file " + filename + ".");
  
  // Check for a positive number of table rows.

  n_rows = getKeyValLLong(fp, "NAXIS2");
  if(n_rows <= 0)
    AH_THROW_RUNTIME("No rows found in " + std::string(ext_name) + " extension of file " + filename + ".");

  AH_INFO(ahlog::LOW) << "Opened " << ext_name << " extension of CAMS data file " << filename << "." << std::endl;
  AH_INFO(ahlog::LOW) << "All required input columns have been located in that extension." << std::endl;
}

// ----------------------------------------------------------------------

void addCAMSOffsetsFileColumns(ahfits::FilePtr& fp)
{
  
  // Add columns to the table extension.
  
  ahfits::insertColBefore(fp, "TIME", "1D", "");
  ahfits::setTUnit(fp, "TIME", "s");
  ahfits::setColumnDescription(fp, "TIME", "Seconds from 01 Jan 2014 00:00:00");

  ahfits::insertColAfter(fp, "DELTARAWX", "1D", "");
  ahfits::setTUnit(fp, "DELTARAWX", "px");
  ahfits::setColumnDescription(fp, "DELTARAWX", "X Offset to correct HXI RAW data");

  ahfits::insertColAfter(fp, "DELTARAWY", "1D", "");
  ahfits::setTUnit(fp, "DELTARAWY", "px");
  ahfits::setColumnDescription(fp, "DELTARAWY", "Y Offset to correct HXI RAW data");

  ahfits::insertColAfter(fp, "COSANGLE", "1D", "");
  ahfits::setColumnDescription(fp, "COSANGLE", "Cosine rot ang to correct HXi RAW data");
  ahfits::insertColAfter(fp, "SINANGLE", "1D", "");
  ahfits::setColumnDescription(fp, "SINANGLE", "Sine rot angle to correct HXi RAW data");
  
  ahfits::insertColAfter(fp, "X1", "1J", "");
  ahfits::setTNull(fp, "X1", INTNULL);
  ahfits::setColumnDescription(fp, "X1", "X direction motion in CAMS1 system");

  ahfits::insertColAfter(fp, "Y1", "1J", "");
  ahfits::setTNull(fp, "Y1", INTNULL);
  ahfits::setColumnDescription(fp, "Y1", "Y direction motion in CAMS1 system");

  ahfits::insertColAfter(fp, "X2", "1J", "");
  ahfits::setTNull(fp, "X2", INTNULL);
  ahfits::setColumnDescription(fp, "X2", "X direction motion in CAMS2 system");

  ahfits::insertColAfter(fp, "Y2", "1J", "");
  ahfits::setTNull(fp, "Y2", INTNULL);
  ahfits::setColumnDescription(fp, "Y2", "Y direction motion in CAMS2 system");

  ahfits::insertColAfter(fp, "JUMPX1", "1J", "");
  ahfits::setTNull(fp, "JUMPX1", INTNULL);
  ahfits::setColumnDescription(fp, "JUMPX1", "Diff between successive values of X1");
  ahfits::insertColAfter(fp, "JUMPY1", "1J", "");
  ahfits::setTNull(fp, "JUMPY1", INTNULL);
  ahfits::setColumnDescription(fp, "JUMPY1", "Diff between successive values of Y1");
  ahfits::insertColAfter(fp, "JUMPX2", "1J", "");
  ahfits::setTNull(fp, "JUMPX2", INTNULL);
  ahfits::setColumnDescription(fp, "JUMPX2", "Diff between successive values of X2");
  ahfits::insertColAfter(fp, "JUMPY2", "1J", "");
  ahfits::setTNull(fp, "JUMPY2", INTNULL);
  ahfits::setColumnDescription(fp, "JUMPY2", "Diff between successive values of Y2");

  ahfits::insertColAfter(fp, "QUALITY1", "1J", "");
  ahfits::setTNull(fp, "QUALITY1", QNULL);
  ahfits::setColumnDescription(fp, "QUALITY1", "Quality from telemetry for CAMS1");
  ahfits::insertColAfter(fp, "QUALITY2", "1J", "");
  ahfits::setTNull(fp, "QUALITY2", QNULL);
  ahfits::setColumnDescription(fp, "QUALITY2", "Quality from telemetry for CAMS2");

  ahfits::insertColAfter(fp, "XDISTANCE", "1D", "");
  ahfits::setTUnit(fp, "XDISTANCE", "mm");
  ahfits::setColumnDescription(fp, "XDISTANCE", "X distance between CAMS units (X2-X1)");

  ahfits::insertColAfter(fp, "YDISTANCE", "1D", "");
  ahfits::setTUnit(fp, "YDISTANCE", "mm");
  ahfits::setColumnDescription(fp, "YDISTANCE", "Y distance between CAMS units (Y2-Y1)");
  
  ahfits::insertColAfter(fp, "DELTASATX", "1D", "");
  ahfits::setTUnit(fp, "DELTASATX", "mm");
  ahfits::setColumnDescription(fp, "DELTASATX", "X Offset in SAT coordinate system");

  ahfits::insertColAfter(fp, "DELTASATY", "1D", "");
  ahfits::setTUnit(fp, "DELTASATY", "mm");
  ahfits::setColumnDescription(fp, "DELTASATY", "Y Offset in SAT coordinate system");

  ahfits::insertColAfter(fp, "BAD_UNITS", "1J", "");
  ahfits::setTNull(fp, "BAD_UNITS", QNULL);
  ahfits::setColumnDescription(fp, "BAD_UNITS", "Which CAMS unit has bad data for row");
  ahfits::insertColAfter(fp, "CALC_QUALITY", "1J", "");
  ahfits::setTNull(fp, "CALC_QUALITY", QNULL);
  ahfits::setColumnDescription(fp, "CALC_QUALITY", "Calculation quality bit flags");
}

// ----------------------------------------------------------------------

bool isDoubleNull(const double t)
{
  // A double value is null if it is NaN.

  return std::isnan(t);
}


// ======================================================================

/* Revision Log
 $Log: cams2detlib.cxx,v $
 Revision 1.18  2016/02/19 00:44:04  klrutkow
 use ahmission::getTELESCOPString() to set TELESCOP

 Revision 1.17  2015/08/07 19:08:19  rshill
 Check for PROC_STATUS column before processing data.

 Revision 1.16  2015/07/31 20:50:25  rshill
 Include temperature-corrected CAMS data columns.

 Revision 1.15  2015/07/31 00:00:14  rshill
 Code cleanup; added proc_status processing; added more counters; added descriptions and comments in output FITS header.

 Revision 1.14  2015/07/30 00:50:15  rshill
 Incorporated keyword copying.

 Revision 1.13  2015/07/29 21:37:37  rshill
 Changed all detnam to instrume.

 Revision 1.12  2015/07/23 20:23:21  rshill
 Tabs->spaces; finished the job this time.

 Revision 1.11  2015/07/23 20:19:49  rshill
 Tabs->spaces.

 Revision 1.10  2015/07/21 21:45:49  rshill
 Added +++ comments to be addressed.

 Revision 1.9  2015/04/07 17:50:55  rshill
 Added calls to getTELESCOPString in the right places.

 Revision 1.8  2015/04/01 16:14:22  rshill
 Fixed bug in CAMSHXIGeometry constructor.

 Revision 1.7  2015/04/01 00:04:16  rshill
 Put in accounting for height difference between CAMS and HXI.  Allow starting coord system to be either RAW or FOC.

 Revision 1.6  2014/01/27 16:28:29  treichar
 Added more comments

 Revision 1.5  2014/01/16 20:23:28  treichar
 Added more comments.

 Revision 1.4  2014/01/15 21:27:30  treichar
 Fixed previous change.

 Revision 1.3  2014/01/15 18:28:08  treichar
 Avoids calculating the sine of the twist angle from a sqrt of a negative number.

 Revision 1.2  2014/01/10 20:41:35  treichar
 Coupled the null-ness of X and Y values and corresponding jumps according to the prescribed algorithm change.

 Revision 1.1  2014/01/07 19:34:11  treichar
 Moved hxiteldef.{h,cxx} and camsteldef.{h,cxx} to the ahmission library.  Moved the functions for reading a CAMS unit data file out of functions.{h,cxx} to the ahmission
 library.  Reorganized the remaining functions in functions.{h,cxx} and params.h into cams2detlib.{h,cxx}.  Updated the standard main function.

 Revision 1.20  2014/01/02 21:39:14  treichar
 Added missing std:: to isnan() function.

 Revision 1.19  2014/01/02 19:17:39  treichar
 Changed the time-matching algorithm to a more robust one that does not rely on matching file times to a nominal time determined by the regularly spaced time offsets in CALDB. The
 new algorithm scans the times in each of the two input files, combines the input data rows as a single output row when the two times match within a tolerance, and keeps the
 input data rows in separate output rows when the two times are too widely spaced.

 Revision 1.18  2013/12/27 20:23:05  treichar
 Grouped various quantities into structures for simpler function calls and better organization.

 Revision 1.17  2013/12/18 22:10:28  treichar
 Completed CFITSIO->ahfits conversion.

 Revision 1.16  2013/08/02 20:04:55  treichar
 Updated the position offsets and twist angle calculation according to the corrections devised by Hans Krimm and Casey Lambert.  Changed default value of flipoutputsign to no, and this parameter may be removed in
 the future.

 Revision 1.15  2013/07/29 14:35:27  treichar
 Changed screen/log output stream to debug stream to avoid unit test failures from floating point numbers in log text.

 Revision 1.14  2013/07/26 21:10:05  treichar
 Reduced precision of high-chatter output so that unit tests can pass on various platforms.

 Revision 1.13  2013/07/25 19:18:39  treichar
 Removed cleanup parameter, which is no longer needed due to switch from using ftcreate to ahfits for writing the output file.  Updated doxygen mark-up and comments
 throughout code.

*/
