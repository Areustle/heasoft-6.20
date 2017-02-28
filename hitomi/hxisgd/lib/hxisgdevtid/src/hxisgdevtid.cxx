/// \file hxisgdevtid.cxx
/// \brief structures and functions common to HXI/SGD reconstruction tasks.
/// \author Mike Witthoeft
/// \date $Date: 2016/02/17 20:35:35 $

#define AHLABEL hxisgdevtid
#define AHCVSID "$Id: hxisgdevtid.cxx,v 1.2 2016/02/17 20:35:35 asargent Exp $"

#include "hxisgdevtid/hxisgdevtid.h"

#include <sstream>

namespace hxisgdevtid {

// ---------------------------------------------------------------------------

int getOccurrenceMode(const std::string& inst, hxisgdevtid::SFFRowData& inrow,
                      int alive_asic, bool* isAm241) {

  // which bits to check in FLAG_TRIGPAT depends on the instrument
  int ntrigpat=0;
  int iam241=0;
  int itrigpat1=0;
  int itrigpat2=0;
  int itrigpat3=0;
  if (inst == "HXI") {
    ntrigpat=5;
    iam241=1;
    itrigpat1=5;
    itrigpat2=6;
    itrigpat3=7;
  } else if (inst == "SGD") {
    ntrigpat=27;
    itrigpat1=28;
    itrigpat2=29;
    itrigpat3=30;
  } else {
    AH_THROW_LOGIC("invalid inst value: "+inst+"; should be HXI or SGD");
  }

  bool tisAm241=false;               // occurrence is flagged as Am241 (HXI only)
  bool tpother0=true;                // true if all other FLAG_TRIGPAT bits are zero
  int mode=hxisgdevtid::e_NOTSURE;
  for (int itp=0; itp < ntrigpat; itp++) {
    if (inrow.m_flag_trigpat[itp] != 0) tpother0=false;
  }
  if (inst == "HXI" && inrow.m_flag_hitpat[iam241] == 1) tisAm241=true;
  if (inrow.m_flag_trigpat[itrigpat1] == 0 && inrow.m_flag_trigpat[itrigpat2] == 0 && inrow.m_flag_trigpat[itrigpat3] == 0) {
    if (tisAm241)
      mode=hxisgdevtid::e_AM241;
    else
      mode=hxisgdevtid::e_CAMERA;       // also CC for SGD
  } else if (tpother0) {   // all TRIGPAT bits other than 6,7,8 are zero
    if (inrow.m_num_asic == 0 && inrow.m_flag_trigpat[itrigpat2] == 1 &&
        inrow.m_flag_trigpat[itrigpat1] == 0 && inrow.m_flag_trigpat[itrigpat3] == 0) {
      mode=hxisgdevtid::e_PSEUDO;
    } else if (inrow.m_flag_trigpat[itrigpat3] == 1 && inrow.m_flag_trigpat[itrigpat1] == 0 && 
               inrow.m_flag_trigpat[itrigpat2] == 0) {
      mode=hxisgdevtid::e_CALMODE;
    } else if (inrow.m_num_asic == alive_asic && inrow.m_flag_trigpat[itrigpat3] == 0 &&
               (inrow.m_flag_trigpat[itrigpat1] == 1 || inrow.m_flag_trigpat[itrigpat2] == 1)) {
      mode=hxisgdevtid::e_READALL;
    }
  }

  if (isAm241 != 0) *isAm241=tisAm241;   // set output isAm241 only if variable passed to function
  return mode;
}

// ---------------------------------------------------------------------------

void convertEPIToPI(const std::string& inst, double epi, int& pi, char& pi_null) {
  // this function will never produce a NULL PI value
  pi_null=0;

  if (inst == "HXI") {
    const double offset=0.1;     // keV
    const double factor=0.1;     // keV per PI channel
    pi=(int)std::floor((epi-offset)/factor + 1.0);
    if (pi < 0) {
      AH_DEBUG << "PI value out of range, setting to min value; EPI, PI = " << epi << ", " << pi << std::endl;
      pi=0;
    }
    if (pi > 2047) {
      AH_DEBUG << "PI value out of range, setting to max value; EPI, PI = " << epi << ", " << pi << std::endl;
      pi=2047;
    }
  } else if (inst == "SGD") {
    const double offset=0.1;
    const double factor=0.75;
    pi = (int)std::floor((epi-offset)/factor + 1.0);
    if (pi < 0) {
      AH_DEBUG << "PI value out of range, setting to min value; EPI, PI = " << epi << ", " << pi << std::endl;
      pi=0;
    }
    if (pi > 2047) {
      AH_DEBUG << "PI value out of range, setting to max value; EPI, PI = " << epi << ", " << pi << std::endl;
      pi=2047;
    }
  } else {
    AH_THROW_LOGIC("invalid inst value: "+inst+"; should be HXI or SGD");
  }
}

// ---------------------------------------------------------------------------

int SGDLayerType(int layer_num) {
  if (layer_num <= SGD_SI_STACK_MAX) {
    return SGD_SI_STACK;
  } else if (layer_num <= SGD_CDTE_BOTTOM_MAX) {
  return SGD_CDTE_BOTTOM;
  } else {
  return SGD_CDTE_SIDE;
  }
}

// ---------------------------------------------------------------------------

bool SGDLayerIsCdTe(int layer_num) {
  int lt = SGDLayerType(layer_num);
  return lt == SGD_CDTE_BOTTOM || lt == SGD_CDTE_SIDE;
}

// ---------------------------------------------------------------------------

bool SGDLayerIsSi(int layer_num) {
  int lt = SGDLayerType(layer_num);
  return lt == SGD_SI_STACK;
}

// ---------------------------------------------------------------------------

std::string getColumnFormat(int length, const std::string& type) {
  std::stringstream out;
  out << length << type;
  return out.str();
}

// ---------------------------------------------------------------------------

}  // namespace hxisgdevtid

/* Revision Log
 $Log: hxisgdevtid.cxx,v $
 Revision 1.2  2016/02/17 20:35:35  asargent
 Added range checks to convertEPIToPI() to check for valid PI range.

 Revision 1.1  2015/03/03 18:24:45  mwitthoe
 hxisgdevtid library: add common structures/functions for reconstruction and expand tools; add new keywords to fluorescence CALDB file for SGD energy test


*/
