/// \file ahcalcl32ti.cxx
/// \brief Compute L32TI from S_TIME
/// \author Mike Witthoeft
/// \date $Date: 2016/12/02 19:15:24 $
/// \version 1.0

/**

\defgroup tool_ahcalcl32ti Compute L32TI from S_TIME (ahcalcl32ti)
@ingroup mod_mission_tasks

Compute and fill the L32TI column using the S_TIME column and the TIME_PACKETS
extension of a TIM file.

Source files:

  ahcalcl32ti.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahgen
  heacore/ahlog
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath

Modification history:

  Ver   Date        Author  Description
  1.0   2016-06-02   MCW    Create task

*/ 

#define AHLABEL tool_ahcalcl32ti
#define AHCVSID "$Id: ahcalcl32ti.cxx,v 1.3 2016/12/02 19:15:24 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahmath/ahmath.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <vector>           // std::vector
#include <cstdio>           // std::remove
#include <sstream>          // std::stringstream


/** \addtogroup tool_ahcalcl32ti
 *  @{
 */

struct Par {
  Par(): m_interp(0) {}

  std::string m_infile;             ///< input FITS file
  std::string m_outfile;            ///< output FITS file
  std::string m_timfile;            ///< input TIM file
  std::string m_l32ticol;           ///< name of output L32TI column
  int m_interp;                     ///< enumeration for interpolation type
};

/// \brief This structure will store the S_TIME vs L32TI table in the
///  TIME_PACKETS extension of the input TIM file.  These data are used
///  to anchor Suzaku-mode points when GPS is unsynchronized for long
///  periods of time.
struct TimePackets {
  TimePackets(): m_nrows(0), m_tstart(0.), m_l32tistart(0.), m_tstop(0.), 
                 m_l32tistop(0.) {}

  std::vector<double> m_stime;
  std::vector<double> m_l32ti;
  int m_nrows;             // number of rows in HDU (size of m_stime/m_l32ti)
  double m_tstart;         // TSTART keyword value
  double m_l32tistart;     // LTISTART keyword value
  double m_tstop;          // TSTOP keyword value
  double m_l32tistop;      // LTISTOP keyword value
};

/// \brief Get parameter values
/// \param[out] par structure with parameter values
void getPar(Par& par);

/// \brief Clone input file; add output column
/// \param[in] par structure with parameter values
/// \param[out] fp ahfits pointer for output FITS file
/// \param[out] timdat structure containing TIME_PACKETS data from TIM file
void initialize(Par& par, ahfits::FilePtr & fp, TimePackets& timepackets);

/// \brief Compute and fill the L32TI column
/// \param[in] par structure with parameter values
/// \param[in] fp ahfits pointer for output FITS file
/// \param[in] timdat structure containing TIME_PACKETS data from TIM file
/// \param[out] clean if yes, delete output files
void doWork(const Par& par, ahfits::FilePtr fp, TimePackets& timepackets,
            bool& clean);

/// \brief close open FITS files
/// \param[in] fp ahfits pointer for output FITS file
/// \param[in] clean if yes, delete output files
void finalize(ahfits::FilePtr fp, bool clean);

// ****************************************************************************

/// \brief ahcalcl32ti tool
int main(int argc, char** argv) {

  Par par;                        // structure with parameter values
  ahfits::FilePtr fp=0;           // FITS file pointer to input HK file: GPS HDU
  TimePackets timepackets;        // store TIME_PACKETS data
  bool clean=true;                // if true, delete output files (b/c error)

  int status=ahapp::startUp(argc,argv,TOOLTAG);
  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog(); 
      initialize(par,fp,timepackets);
      doWork(par,fp,timepackets,clean);
      finalize(fp,clean);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,fp,timepackets);
        doWork(par,fp,timepackets,clean);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fp,clean);
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unanble to start up tool." << std::endl;
  }

  return status;
}

// ****************************************************************************

void getPar(Par& par) {

  par.m_infile=ahapp::getParString("infile");
  par.m_outfile=ahapp::getParString("outfile");
  par.m_timfile=ahapp::getParString("timfile");
  par.m_l32ticol=ahapp::getParString("l32ticol");

  // Read interpolation parameter and convert it into the ahmath
  // enumerated value.
  std::string tinterp=ahgen::strtoupper(ahapp::getParString("interp"));
  par.m_interp=ahmath::interpolation_type(tinterp);
}

// ****************************************************************************

void initialize(Par& par, ahfits::FilePtr & fp, TimePackets& timepackets) {

  // Clone input file to output
  ahfits::clone(par.m_infile,par.m_outfile,&fp,true);
  if (ahfits::isPrimary(fp)) ahfits::firstHDU(fp,ahfits::e_BINARY_TBL);   // move to first table if no extended synatx

  // If output column is present, stop tool
  if (ahfits::haveColumn(fp,par.m_l32ticol)) {
    std::stringstream msg;
    msg << "Output column, " << par.m_l32ticol << ", already exists in file. "
        << "Either rename this column in the input file or use the l32ticol "
        << "parameter to specify another output column name.";
    AH_THROW_RUNTIME(msg.str());
  }

  // Insert output column (threw an error above if already present)
  ahfits::insertColAfter(fp,par.m_l32ticol,"J","");
  ahfits::setTZero(fp,par.m_l32ticol,2147483648L);

  // Open TIM file and move to correct extension
  ahfits::FilePtr fptim;
  ahfits::open(par.m_timfile,"",&fptim);
  if (ahfits::isPrimary(fptim)) ahfits::move(fptim,"TIME_PACKETS");  // move to TIME_PACKETS extension if no extended syntax

  // Read TIME_PACKETS extension
  double l_stime=0.;
  double l_l32ti=0.;
  ahfits::Router rout(fptim);
  rout.connectScalar(ahfits::e_READONLY,"S_TIME",l_stime);
  rout.connectScalar(ahfits::e_READONLY,"L32TI",l_l32ti);
  for (ahfits::firstRow(fptim); ahfits::readOK(fptim); ahfits::nextRow(fptim)) {
    ahfits::readRow(fptim);
    timepackets.m_stime.push_back(l_stime);
    timepackets.m_l32ti.push_back(l_l32ti);
  }
  timepackets.m_nrows=timepackets.m_stime.size();
  ahfits::close(fptim);

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

}

// ****************************************************************************

void doWork(const Par& par, ahfits::FilePtr fp, TimePackets& timepackets,
            bool& clean) {

  ahfits::IndexType itim=0;      // search index in TIME_PACKETS table
  bool extrap=false;             // true if input S_TIME outside range of table

  // Local variables for output file
  long long l_l32ti=0;           // L32TI
  double l_stime=0.;             // S_TIME

  // max value of L32TI
  double l32timax=(double)0xFFFFFFFF;    // 2^32-1

  // Set up connections to input columns
  ahfits::Router rout(fp);
  rout.connectScalar(ahfits::e_WRITEONLY,par.m_l32ticol,l_l32ti);
  rout.connectScalar(ahfits::e_READONLY,"S_TIME",l_stime);

  // Loop over input rows and compute L32TI
  clean=false;
  for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp)) {
    ahfits::readRow(fp);

    // Search TIM data for input S_TIME
    // Note: &timepackets.m_stime[0] -> allows std::vector to be accepted as
    //  a double* argument
    itim=ahmath::search_sawX2Y(l_stime,&timepackets.m_stime[0],&timepackets.m_l32ti[0],
                               timepackets.m_nrows,extrap,par.m_interp,itim);

    // Interpolate on S_TIME to get L32TI
    // Note: &timepackets.m_stime[0] -> allows std::vector to be accepted as
    //  a double* argument
    double tl32ti=0.;
    tl32ti=ahmath::interpolate_sawX2Y(l_stime,l32timax,&timepackets.m_stime[0],
                                      &timepackets.m_l32ti[0],timepackets.m_nrows,
                                      par.m_interp,itim);

    // convert L32TI from double to long long by rounding
    l_l32ti=(tl32ti+0.5);

    ahfits::writeRow(fp);
  }

}

// ****************************************************************************

void finalize(ahfits::FilePtr fp, bool clean) {

  // get file name to delete if there is an error
  std::string filename;
  if (0 != fp) filename=fp->m_filename;

  // close all open files
  ahfits::close(fp);

  // delete output file if there was an early error
  if (clean && filename != "" && ahgen::fileExists(filename)) std::remove(filename.c_str());

}

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: ahcalcl32ti.cxx,v $
 Revision 1.3  2016/12/02 19:15:24  mwitthoe
 ahcalcl32ti: write the TZERO keyword for the output L32TI column to make the column unsigned

 Revision 1.2  2016/06/09 15:37:37  mwitthoe
 ahcalcl32ti: tool will stop with an error if the output L32TI column is already present in the input file

 Revision 1.1  2016/06/02 19:33:45  mwitthoe
 add new task ahcalcl32ti which computes L32TI from S_TIME and the TIME_PACKETS extension of the TIM file


*/

