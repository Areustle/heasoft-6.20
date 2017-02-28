/// \file camstoffset.cxx
/// \brief functions to act on the CALDB file containing CAMS offset parameters
/// \author Mike Witthoeft
/// \date $Date: 2015/10/02 20:07:57 $
 
#define AHLABEL ahmission_camstoffset
#define AHCVSID "$Id: camstoffset.cxx,v 1.6 2015/10/02 20:07:57 mwitthoe Exp $"

#include "ahmath/ahmath.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahmission/camstoffset.h"

#include <fstream>
#include <sstream>

// ===========================================================================

namespace ahmission {

namespace camstoffset {

// ---------------------------------------------------------------------------

/// \callgraph
void load(const std::string & filename, DataType & dat) {

  // open file
  ahfits::FilePtr fptr;
  ahfits::open(filename,"CAMS_OFFSET",&fptr);
  if (!ahfits::readOK(fptr)) {
    AH_THROW_RUNTIME("failed to open CAMS offset FITS file");
  }

  // initialize data structure
  ahmission::camstoffset::clear(dat);

  // setup router
  ahfits::Router router(fptr);

  // get period and number of measurements per period 
  dat.m_cams_period=ahfits::getKeyValDbl(fptr,"TPERIOD");
  dat.m_cams_freq=ahfits::getKeyValLLong(fptr,"TFREQ");

  // make connections to local variables
  double l_offset;
  router.connectScalar(ahfits::e_READONLY,"offset",l_offset);
  int l_tc;
  router.connectScalar(ahfits::e_READONLY,"TIME_CODE",l_tc);

  // read table
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr);
       ahfits::nextRow(fptr)) {
    
    ahfits::readRow(fptr);
    dat.m_cams_offsets[l_tc]=l_offset;
  }

  // close FITS file
  ahfits::close(fptr);
}

// ---------------------------------------------------------------------------

void clear(DataType & dat) {
  dat.m_cams_offsets.clear();
  dat.m_cams_period=0.0;
  dat.m_cams_freq=0;
}

// ---------------------------------------------------------------------------

double get_period(const DataType & dat) {
  return dat.m_cams_period;
}

// ---------------------------------------------------------------------------

double get_frequency(const DataType & dat) {
  return dat.m_cams_freq;
}

// ---------------------------------------------------------------------------

double get_offset(DataType & dat, int timecode) {
  if (0 == dat.m_cams_offsets.count(timecode))
    AH_THROW_RUNTIME("invalid time code for CAMS offset");
  return dat.m_cams_offsets[timecode];
}

// ---------------------------------------------------------------------------

int num_offset(const DataType & dat) {
  return dat.m_cams_offsets.size();
}

// ---------------------------------------------------------------------------

/// \callgraph
void write_cams_template(const std::string & tplfile, double period,
                         int freq) {

  std::ofstream out;
  out.open(tplfile.c_str());
  out << "# Template file for CAMS offset FITS file" << std::endl;
  out << "#" <<std::endl;
  out << "# there is a single extension with 2 columns giving: " << std::endl;
  out << "#   2) offset in seconds" << std::endl;
  out << "#   3) offset as integer" << std::endl;
  out << "# " << std::endl;
  out << "# also, in the header, is the CAMS period: TPERIOD" << std::endl;
  out << "# see Table 24 in SCT 021 (Sep 2012)" << std::endl;
  out << "" << std::endl;

  out << "" << std::endl;
  out << "xtension = bintable" << std::endl;
  out << "extname = 'CAMS_OFFSET'" << std::endl;
  out << "naxis2 = 5" << std::endl;
  out << "TPERIOD = " << period << std::endl;
  out << "TFREQ = " << freq << std::endl;
  out << "" << std::endl;
  out << "ttype# = offset" << std::endl;
  out << "tform# = D" << std::endl;
  out << "tunit# = 's'" <<std::endl;
  out << "" << std::endl;
  out << "ttype# = TIME_CODE" << std::endl;
  out << "tform# = J" << std::endl;
  out << "" << std::endl;

  out.close();
}

// ---------------------------------------------------------------------------

/// \callgraph
void create_cams_fits(const std::string & filename, 
                      const std::string & tplfile) {

  // make FITS file from template
  std::string tfile='!'+filename;
  ahfits::FilePtr fptr;
  ahfits::create(tfile,tplfile,&fptr);
  ahfits::close(fptr);

}

// ---------------------------------------------------------------------------

void write_cams_data(const std::string & filename, 
                     const std::vector<double> offsets,
                     const std::vector<int> tcs) {

  // check if arrays are of equal size
  if (tcs.size() != offsets.size()) {
    AH_THROW_RUNTIME("input vectors have different lengths");
  }

  // open file
  ahfits::FilePtr fptr;
  ahfits::open(filename,"CAMS_OFFSET",&fptr);
  if (!ahfits::readOK(fptr)) {
    AH_THROW_RUNTIME("failed to open CAMS offset FITS file");
  }

  // make connections with local variable
  double offset;
  int tc;
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_WRITEONLY,"offset",offset);
  router.connectScalar(ahfits::e_WRITEONLY,"TIME_CODE",tc);

  // loop through vectors, local=vector[i], write row
  // be sure to check if at last row
  ahfits::firstRow(fptr);
  if (!ahfits::readOK(fptr)) {
    ahfits::close(fptr);
    std::stringstream msg;
    msg << "no rows in FITS file? " << filename << " : CAMS";
    AH_THROW_RUNTIME(msg.str());
  }
  int nrow=tcs.size();
  for (int irow=0; irow < nrow; irow++) {
    offset=offsets[irow];
    tc=tcs[irow];
    ahfits::writeRow(fptr);
    ahfits::nextRow(fptr);
  }

  // close FITS file
  ahfits::close(fptr);
}

// ---------------------------------------------------------------------------

}  // namespace camstoffset

}  // namespace ahmission

/* Revision Log
 $Log: camstoffset.cxx,v $
 Revision 1.6  2015/10/02 20:07:57  mwitthoe
 ahmission library: remove redundant CALDB resolve functions; see issue 535

 Revision 1.5  2013/10/08 14:02:42  mwitthoe
 ahmission library: switch over to new ahfits connect functions (see issue 270)

 Revision 1.4  2013/04/11 18:47:06  mwitthoe
 update former ahcaldb libraries now residing in ahmission: camstoffset, delay, and leapsec; changed namespace, add read/write flags to ahfits connections, removed static instance of DataType holding CALDB data in memory

 Revision 1.3  2012/12/10 18:13:32  mwitthoe
 in ahcaldb, revert to using old versions of open/create which return void instead of FilePtr

 Revision 1.2  2012/11/16 18:52:27  mwitthoe
 add TFREQ keyword to CALDB file with CAMS offsets which gives the number of measurements per CAMS period

 Revision 1.1  2012/11/15 02:24:23  mwitthoe
 add ahcaldb folder under mission; moved from ahtime: leapsec, delay, timecoldef, camstoffset, freqtemp (these names have changed)



 ==== Messages below refer to old location: mission/lib/ahtime ====

 Revision 1.4  2012/11/13 18:23:25  mwitthoe
 ahtime library: use new open/create/clone functions from ahfits; change time delay CALDB file and implementation to match latest Oct 31 TRF; change column definitions CALDB file and implementation to match Oct 31 TRF; read and use SXS lookup tables in time assignment; use new instrument names for HXI1/2 and SGD1/2

 Revision 1.3  2012/11/05 21:02:25  mwitthoe
 modify CAMS FITS file to match definition in SCT 021 (Sep 7, 2012)

 Revision 1.2  2012/11/01 20:03:01  mwitthoe
 remove clobber arguments from ahfits create/clone calls from ahtime library and unit tests

 Revision 1.1  2012/10/18 17:35:40  mwitthoe
 ahtime library: created single header file, ahtime.h, to include entire ahtime library (old ahtime -> ahtime_base); separated CAMS data from instrument delay CALDB file into new file: associated libraries created


*/
