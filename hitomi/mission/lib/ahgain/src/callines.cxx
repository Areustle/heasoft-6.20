/// \file callines.cxx
/// \brief Load calibration line information from CALDB file.
/// \author Mike Witthoeft
/// \date $Date: 2016/03/18 16:03:35 $
 
#define AHLABEL ahgain_callines
#define AHCVSID "$Id: callines.cxx,v 1.2 2016/03/18 16:03:35 asargent Exp $"

#include "ahgain/callines.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"

#include <sstream>

namespace callines {

// ---------------------------------------------------------------------------

void load(const std::string & filename, const std::string & extname, 
          CalLines & dat) {

  // declare variables
  ahfits::FilePtr fptr=0;           // ahfits file pointer
  double l_energy=0.;               // column: ENERGY
  double l_width=0.;                // column: WIDTH
  double l_amplitude=0.;            // column: AMPLITUDE

  // open file
  ahfits::open(filename,extname,&fptr);
  if (!ahfits::readOK(fptr)) {
    std::stringstream msg;
    msg << "CALDB file with SXS calibration line energies, " << filename << "[" << extname << "], has no data";
    AH_THROW_RUNTIME(msg.str());
  }

  // read number of components and average energy from header
  dat.m_label=extname;
  dat.m_nlines=ahfits::getKeyValLLong(fptr,"ENERGREF");
  dat.m_avenergy=ahfits::getKeyValDbl(fptr,"ENERGAVE");

  // setup router
  ahfits::Router router(fptr);

  // make connections to local variables
  router.connectScalar(ahfits::e_READONLY,"ENERGY",l_energy);
  router.connectScalar(ahfits::e_READONLY,"WIDTH",l_width);
  router.connectScalar(ahfits::e_READONLY,"AMPLITUDE",l_amplitude);

  // read table
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {

    ahfits::readRow(fptr);
    dat.m_energy.push_back(l_energy);
    dat.m_width.push_back(l_width);
    dat.m_amplitude.push_back(l_amplitude);
  }

  // close FITS file
  ahfits::close(fptr);

  // check that number of components read is consistent with header keyword
  if (dat.m_energy.size() != (unsigned int)dat.m_nlines) {
    AH_INFO(ahlog::LOW) << " *** Number of components read for calibration source, "
                        << dat.m_label << ", is inconsistent with the number "
                        << "expected from the ENERGREF keyword: "
                        << dat.m_energy.size() << " vs " << dat.m_nlines
                        << std::endl;
  }

}

// ---------------------------------------------------------------------------

double getMinEnergy(CalLines& dat) {
  double out=dat.m_energy[0];
  for (int i=1; i < dat.m_nlines; i++) {
    if (dat.m_energy[i] < out) out=dat.m_energy[i];
  }
  return out;
}

// ---------------------------------------------------------------------------

double getMaxEnergy(CalLines& dat) {
  double out=dat.m_energy[0];
  for (int i=1; i < dat.m_nlines; i++) {
    if (dat.m_energy[i] > out) out=dat.m_energy[i];
  }
  return out;
}

// ---------------------------------------------------------------------------

}  // namespace callines

/* Revision Log
 $Log: callines.cxx,v $
 Revision 1.2  2016/03/18 16:03:35  asargent
 Changed AH_WARN to AH_INFO

 Revision 1.1  2014/07/17 19:47:20  mwitthoe
 add ahgain library which contains routines for fitting event data with a calibration feature


*/
