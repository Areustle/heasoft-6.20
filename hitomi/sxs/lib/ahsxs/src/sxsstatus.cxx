/// \file sxsstatus.cxx
/// \brief Functions to read/write the SXS STATUS column
/// \author Mike Witthoeft
/// \date $Date: 2015/12/03 19:33:57 $

#define AHLABEL ahsxs_sxsstatus
#define AHCVSID "$Id: sxsstatus.cxx,v 1.3 2015/12/03 19:33:57 mwitthoe Exp $"

#include "ahsxs/sxsstatus.h"

namespace ahsxs {


// ---------------------------------------------------------------------------

void clear_status(type_status* status) {
  for (int i=0; i < LEN_SXSSTATUS; i++) status[i]=0;
}

// ---------------------------------------------------------------------------

void set_status_general_gti(bool stat, type_status* status) {
  if (stat)
    status[e_STATUS_GTI_GENERAL]=1;
  else
    status[e_STATUS_GTI_GENERAL]=0;
}

// ---------------------------------------------------------------------------

void set_status_pixel_gti(bool stat, type_status* status) {
  if (stat)
    status[e_STATUS_GTI_PIXEL]=1;
  else
    status[e_STATUS_GTI_PIXEL]=0;
}

// ---------------------------------------------------------------------------

void set_status_antico(bool stat, type_status* status) {
  if (stat)
    status[e_STATUS_ANTICO]=1;
  else
    status[e_STATUS_ANTICO]=0;
}

// ---------------------------------------------------------------------------

void set_status_prox(bool intime, type_status* status) {
  if (intime)
    status[e_STATUS_PROXIMITY]=1;
  else
    status[e_STATUS_PROXIMITY]=0;
}

// ---------------------------------------------------------------------------

void set_status_recoil(bool is12, bool eOK, type_status* status) {

  // reset recoil bits
  status[e_STATUS_NEAR_CALPIX]=0;
  status[e_STATUS_RECOIL]=0;

  // pixel 12?
  if (!is12) return;
  status[e_STATUS_NEAR_CALPIX]=1;

  // energy test satisfied?
  if (!eOK) return;
  status[e_STATUS_RECOIL]=1;
}

// ---------------------------------------------------------------------------

void set_status_electrical(bool intime, bool prime, type_status* status) {
  // reset electrical bits
  status[e_STATUS_ELECTRICAL]=0;
  status[e_STATUS_ELEC_MAX]=0;

  // time test satisfied?
  if (!intime) return;
  status[e_STATUS_ELECTRICAL]=1;

  // largest PHA?
  if (!prime) return;
  status[e_STATUS_ELEC_MAX]=1;
}

// ---------------------------------------------------------------------------

void set_status_mxs(bool isOn, bool glow, bool isdirect, type_status* status) {

  if (isdirect) {
    if (isOn) {
      status[e_STATUS_MXS_DIRECT]=1;
      status[e_STATUS_MXS_DIRECT_GLOW]=0;
    } else if (glow) {
      status[e_STATUS_MXS_DIRECT]=0;
      status[e_STATUS_MXS_DIRECT_GLOW]=1;
    } else {
      status[e_STATUS_MXS_DIRECT]=0;
      status[e_STATUS_MXS_DIRECT_GLOW]=0;
    }
  } else {    // indirect
    if (isOn) {
      status[e_STATUS_MXS_INDIRECT]=1;
      status[e_STATUS_MXS_INDIRECT_GLOW]=0;
    } else if (glow) {
      status[e_STATUS_MXS_INDIRECT]=0;
      status[e_STATUS_MXS_INDIRECT_GLOW]=1;
    } else {
      status[e_STATUS_MXS_INDIRECT]=0;
      status[e_STATUS_MXS_INDIRECT_GLOW]=0;
    }
  }

}

// ---------------------------------------------------------------------------

void set_status_electrical2(bool intime, bool prime, type_status* status) {
  // reset electrical bits
  status[e_STATUS_ELECTRICAL2]=0;
  status[e_STATUS_ELEC_MAX2]=0;

  // time test satisfied?
  if (!intime) return;
  status[e_STATUS_ELECTRICAL2]=1;

  // largest PHA?
  if (!prime) return;
  status[e_STATUS_ELEC_MAX2]=1;
}

// ---------------------------------------------------------------------------

bool has_status_general_gti(type_status* status) {
  if (status[e_STATUS_GTI_GENERAL] == 1) return true;
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_pixel_gti(type_status* status) {
  if (status[e_STATUS_GTI_PIXEL] == 1) return true;
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_antico(type_status* status) {
  if (status[e_STATUS_ANTICO] == 1) return true;
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_prox(type_status* status) {
  if (status[e_STATUS_PROXIMITY] == 1) return true;
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_gti(type_status* status) {
  if (status[e_STATUS_GTI_GENERAL] == 1) return true;
  if (status[e_STATUS_GTI_PIXEL] == 1) return true;
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_crosstalk(type_status* status) {
  if (status[e_STATUS_ELECTRICAL] == 1) return true;
  if (status[e_STATUS_RECOIL] == 1) return true;
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_recoil_crosstalk(type_status* status) {
  if (status[e_STATUS_RECOIL] == 1) return true;
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_electrical_crosstalk_source(type_status* status) {
  if (status[e_STATUS_ELECTRICAL] == 0) return false;
  if (status[e_STATUS_ELEC_MAX] == 1) return true;
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_electrical_crosstalk_residual(type_status* status) {
  if (status[e_STATUS_ELECTRICAL] == 0) return false;
  if (status[e_STATUS_ELEC_MAX] == 0) return true;
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_mxs(type_status* status, bool afterglow) {
  if (status[e_STATUS_MXS_DIRECT] == 1) return true;
  if (status[e_STATUS_MXS_INDIRECT] == 1) return true;
  if (afterglow) {
    if (status[e_STATUS_MXS_DIRECT_GLOW] == 1) return true;
    if (status[e_STATUS_MXS_INDIRECT_GLOW] == 1) return true;
  }
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_electrical_crosstalk_source2(type_status* status) {
  if (status[e_STATUS_ELECTRICAL2] == 0) return false;
  if (status[e_STATUS_ELEC_MAX2] == 1) return true;
  return false;
}

// ---------------------------------------------------------------------------

bool has_status_electrical_crosstalk_residual2(type_status* status) {
  if (status[e_STATUS_ELECTRICAL2] == 0) return false;
  if (status[e_STATUS_ELEC_MAX2] == 0) return true;
  return false;
}

// ---------------------------------------------------------------------------

}  // namespace ahsxs

/* Revision Log
 $Log: sxsstatus.cxx,v $
 Revision 1.3  2015/12/03 19:33:57  mwitthoe
 ahsxs library: support CTEL2DT column in delta-times CALDB file; assign 2 bits in STATUS for a 2nd electrical cross-talk flagging

 Revision 1.2  2015/09/17 19:55:26  mwitthoe
 ahsxs: add functions to check SXS STATUS columns for recoil and electrical cross-talk separately; this is needed for sxssecid (see issue 554)

 Revision 1.1  2014/11/18 22:14:44  mwitthoe
 ahsxs: add library to set/check SXS STATUS column bits; these functions will be used by the sxsflagpix and sxsdrift tasks; see issue 445


*/
