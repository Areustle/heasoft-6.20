/// \file sxsseccor.cxx
/// \brief Correct PHA for secondary events
/// \author Mike Witthoeft
/// \date $Date: 2016/05/24 19:04:04 $
/// \version 1.0

/** 

\defgroup tool_sxsseccor Correct PHA for secondary SXS events (sxsseccor)
@ingroup mod_sxs_tasks

Correct PHA values for mid-resolution secondary events using the pulse
amplitude file in CALDB.

Source files:

  sxsseccor.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/mission/lib/ahmission
  astroh/sxs/lib/ahsxs

Modification history:

  Ver   Date        Author  Description
  1.0   2015-0806   MCW    Clean-up code

*/
 
#define AHLABEL tool_sxsseccor
#define AHCVSID "$Id: sxsseccor.cxx,v 1.3 2016/05/24 19:04:04 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
#include "ahsxs/ahsxs.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"
#include "ahlog/ahlog.h"
#include "ahsxs/ahsxs.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <stdlib.h>      // atof
#include <cmath>         // std::pow
#include <cstdio>        // std::remove()


/** \addtogroup tool_sxsseccor
 *  @{
 */

/// \brief convert nanoseconds into seconds
const double NANO2SEC=1.e-9;

/// \brief convert milliseconds into seconds
const double MILLI2SEC=1.e-3;

/// \brief number of pulse amplitude bins
const int NPULSEBINS=1024;

/// \brief size of TIME bins of pulse amplitude array in seconds
const double TIMEBINSIZE=80.e-6;

/// \brief structure to store input parameters
struct Par {
  std::string m_infile;      ///< input file name
  std::string m_outfile;     ///< output file name
  std::string m_pulsefile;   ///< input file with pulse amplitudes (or CALDB)
  std::string m_itypecol;    ///< ITYPE column to use
  std::string m_phaout;      ///< name of output PHA column
};

/// \brief structure to hold data from a single row of the input file
struct RowData {
  RowData(): m_rowidx(0),m_time(0.),m_timenull(0),m_pha(0),m_phanull(0),
             m_phaout(0),m_phaoutnull(0),m_pixel(0),m_itype(0),
             m_groups(0),m_groupsnull(0),m_num_proc_status(32) {
               for (int ip=0; ip < 32; ip++) m_proc_status[ip]=0;
             }

  long long m_rowidx;                   ///< current row index; not a column
  double m_time;                        ///< input TIME column; assigned time in seconds
  char m_timenull;                      ///< =1 if TIME == NULL
  int m_pha;                            ///< input PHA column
  char m_phanull;                       ///< =1 if PHA == NULL
  int m_phaout;                         ///< output PHA column
  char m_phaoutnull;                    ///< =1 if output PHA == NULL
  int m_pixel;                          ///< input PIXEL column; pixel index from 0 to 35
  int m_itype;                          ///< input ITYPE column; Hp (=0), Mp, Ms, Lp, Ls, BL, LO, or Rj (=7)
  long m_groups;                        ///< output GROUPS column
  char m_groupsnull;                    ///< =1 if GROUPS==NULL
  char m_proc_status[32];               ///< input PROC_STATUS column
  ahfits::IndexType m_num_proc_status;  ///< number of PROC_STATUS bits
};

/// \brief vector of row data
typedef std::vector<RowData> RowDataVec;

/// \brief iterator for RowDataVec
typedef std::vector<RowData>::iterator RowDataVecIter;

/// \brief structure to hold pulse amplitudes from single pixel/energy group
struct PulseInfo {
  PulseInfo(): m_phamin(0), m_phamax(0), m_offset(0) {
    for (int i=0; i < NPULSEBINS; i++) m_pulse[i]=0;
  }
  int m_phamin;                        ///< min(PHA) of group
  int m_phamax;                        ///< max(PHA) of group
  int m_offset;                        ///< offset of group
  int m_pulse[NPULSEBINS];                   ///< pulse amplitudes in 80 microsecond bins
};

/// \brief pulse amplitudes for all energy groups for a single pixel
typedef std::vector<PulseInfo> PulseInfoVec;

/// \brief pulse amplitudes for all energy groups for all pixels
struct AllPulseInfo {
  PulseInfoVec m_data[36];
};

/// \brief Get parameter values
/// \param[out] par         structure with parameter values
void getPar(Par& par);

/// \brief Copy contents of infile to outfile; resolve default values of columns
/// \param[in] par          structure with parameter values
/// \param[out] fp          ahfits object of output file (clone of input)
/// \param[out] pulsedat    structure containing secondary pulse data
void initialize(const Par& par, ahfits::FilePtr& fp, AllPulseInfo& pulsedat);

/// \brief Fill in 2nd extension of output TIM file
/// \param[in] par          structure with parameter values
/// \param[in] fp           ahfits object of output file (clone of input)
/// \param[in] pulsedat     structure containing secondary pulse data
/// \param[out] clean       if yes, delete output files
void doWork(const Par& par, ahfits::FilePtr fp, const AllPulseInfo& pulsedat,
            bool& clean);

/// \brief close open FITS files
/// \param[in] fp          ahfits object of output file (clone of input)
/// \param[in] clean       if yes, delete output files
void finalize(ahfits::FilePtr fp, bool clean);

/// \brief load secondary pulse CALDB file
/// \param[in] filename    name of input secondary pulse CALDB file
/// \param[out] pulsedat   structure to hold pulse data per pixel
void loadSecPulseFile(const std::string& filename, AllPulseInfo& pulsedat);


// ****************************************************************************

/// \brief sxsseccor tool
int main(int argc, char** argv) {

  Par par;                      // structure containing parameter values
  ahfits::FilePtr fp=0;         // FITS file pointer to output file
  AllPulseInfo pulsedat;        // structure containing secondary pulse data
  bool clean=true;              // delete output files (upon error)

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      initialize(par,fp,pulsedat);
      doWork(par,fp,pulsedat,clean);
      finalize(fp,clean);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,fp,pulsedat);
        doWork(par,fp,pulsedat,clean);
      } catch (const std::exception &x) {
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
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;
}

// ****************************************************************************

void getPar(Par& par) {

  par.m_infile=ahapp::getParString("infile");
  par.m_outfile=ahapp::getParString("outfile");
  par.m_pulsefile=ahapp::getParString("pulsefile");
  par.m_itypecol=ahapp::getParString("itypecol");
  par.m_phaout=ahapp::getParString("phaout");

  if (par.m_phaout == "PHA") AH_THROW_RUNTIME("phaout parameter cannot be PHA");
}

// ****************************************************************************

void initialize(const Par& par, ahfits::FilePtr& fp, AllPulseInfo& pulsedat) {

  // Declare variables.
  std::string extname="";    // extension of input file to use
  std::string devpthre;      // DEVPTHRE keyword needed for CALDB query

  // variables for CALDB query
  std::string pulsefile;
  std::string filetype_pulse="SXS Secondary Pulse Amplitudes";
  std::string instrume;
  std::string detnam="-";
  std::string codename_pulse="SECPULSE";
  std::string datetime;
  std::string expr;

  // PHA column information
  long long tlmin=0;
  long long tlmax=0;
  long long tnull=0;

  // Copy contents of infile to outfile and return opened output file (fp).
  // If editing input file in-place (allowed with last argument = true), 
  // fp will point to the opened input file.
  extname="EVENTS";
  ahfits::clone(par.m_infile,par.m_outfile,&fp,true);
  if (ahfits::isPrimary(fp)) ahfits::move(fp,extname);   // move to EVENTS extension if extended syntax not used
  ahmission::checkEmptyTable(fp,par.m_infile);

  // check that is SXS event of SXS-specific HK file
  instrume = ahfits::getKeyValStr(fp,"INSTRUME");
  datetime = ahfits::getKeyValStr(fp,"DATE-OBS");
  if ("SXS" != ahgen::strtoupper(instrume))
    AH_THROW_RUNTIME("input file must have INSTRUME=SXS");
  if ("PIXEL" != ahgen::strtoupper(ahfits::getKeyValStr(fp,"DETNAM")))
    AH_THROW_RUNTIME("input file must have DETNAM=PIXEL");

  // Check that GROUPS column exists
  if (!ahfits::haveColumn(fp,"GROUPS")) {
    AH_THROW_RUNTIME("GROUPS column not found; run sxssecid");
  }

  // Get PHA column information
  if (!ahfits::columnNull(fp,"PHA",tnull)) {
    AH_THROW_RUNTIME("PHA column needs to have TNULL defined");
  }
  if (!ahfits::columnRange(fp,"PHA",tlmin,tlmax)) {
    AH_THROW_RUNTIME("PHA column needs to have TLMIN/TLMAX defined");
  }

  // Read keyword needed for CALDB query
  if (par.m_pulsefile == "CALDB") {
    devpthre=ahfits::getKeyValStr(fp,"DEVPTHRE");
    expr="DEVPTHRE.EQ."+devpthre;
  }

  // Add output PHA column, if not present
  if (!ahfits::haveColumn(fp,par.m_phaout) ) {
    ahfits::insertColAfter(fp,par.m_phaout,"J","");    // insert at end
    ahfits::setColumnDescription(fp,par.m_phaout,"sxsseccor");
    ahfits::setTNull(fp,par.m_phaout,tnull);
    ahfits::setTLmin(fp,par.m_phaout,tlmin);
    ahfits::setTLmax(fp,par.m_phaout,tlmax);
  }

  // Query CALDB for pulse file
  pulsefile = ahmission::caldb::resolve(par.m_pulsefile,filetype_pulse,instrume,
                                        detnam,codename_pulse,datetime,expr); 
  ape_trad_set_string("pulsefile",pulsefile.c_str());   // to record actual file path in history

  // Load secondary pulse data into structure
  loadSecPulseFile(pulsefile,pulsedat);

  // Write list of parameters to log file.
  ahapp::writeParametersToLog(); 
}

// ****************************************************************************

void doWork(const Par& par, ahfits::FilePtr fp, const AllPulseInfo& pulsedat,
            bool& clean) {

  // Declare variables.
  RowDataVec groupdat[36];          // store group data for each pixel
  RowData l_rowdat;                 // store current row data
  ahfits::Router rout(fp);          // ahfits router for input/output file

  // Counters
  long long nevents=0;              // number of events read
  long long nskip_procstatus=0;     // number with bad PROC_STATUS
  long long nskip_timenull=0;       // number with TIME=NULL
  long long nskip_itype=0;          // number with wrong ITYPE
  long long nskip_phanull=0;        // number skipped b/c PHA=NULL
  long long nprimary=0;             // number of primary events
  long long nsecondary=0;           // number of secondaries
  long long nmidsec=0;              // number of mid-res secondaries
  long long nlowsec=0;              // number of low-res secondaries
  long long norphan=0;              // number of orphaned secondaries receiving no correction
  long long nmsorphan=0;            // number of orphaned mid-res secondaries receiving no correction
  long long nlsorphan=0;            // number of orphaned low-res secondaries receiving no correction
  long long ncorrections=0;         // number of secondary corrections made
  long long nphaoutnull=0;          // number of output PHA values set to NULL b/c input or corrected PHA out-of-range
  long long ncorskip=0;             // number of intermediate corrections skipped b/c delta-time out-of-range

  // Make connections between input file and local variables.
  rout.connectScalar(ahfits::e_READONLY,"PIXEL",l_rowdat.m_pixel);
  rout.connectScalar(ahfits::e_READONLY,par.m_itypecol,l_rowdat.m_itype);
  rout.connectScalar(ahfits::e_READONLY,"TIME",l_rowdat.m_time,&l_rowdat.m_timenull);
  rout.connectScalar(ahfits::e_READONLY,"PHA",l_rowdat.m_pha,&l_rowdat.m_phanull);
  rout.connectScalar(ahfits::e_WRITEONLY,par.m_phaout,l_rowdat.m_phaout,&l_rowdat.m_phaoutnull);
  rout.connectScalar(ahfits::e_READONLY,"GROUPS",l_rowdat.m_groups,&l_rowdat.m_groupsnull);
  rout.connectBit(ahfits::e_READONLY,"PROC_STATUS",l_rowdat.m_proc_status,l_rowdat.m_num_proc_status);

  // Main EVENTS row loop
  clean=false;      // at this point the output files are not deleted if there is an error
  for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp) ) {

    // Declare variables.
    int pixel=0;      // pixel number; just for convenience
    int itype=0;      // itype index; just for convenience

    // Read row and fill convenience variables.
    ahfits::readRow(fp);
    l_rowdat.m_rowidx=ahfits::currentRow(fp);
    pixel=l_rowdat.m_pixel;
    itype=l_rowdat.m_itype;
    nevents++;

    // Most events do not need a correction, so set default output to PHA
    l_rowdat.m_phaout=l_rowdat.m_pha;
    l_rowdat.m_phaoutnull=l_rowdat.m_phanull;

    // If bad PROC_STATUS, PHA is unchanged
    if (!procstatus::processRow(l_rowdat.m_proc_status)) {
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping event with bad PROC_STATUS" << std::endl;
      nskip_procstatus++;
      continue;
    }

    // If TIME is NULL, PHA is unchanged
    if (l_rowdat.m_timenull != 0) {
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping event with TIME=NULL" << std::endl;
      nskip_timenull++;
      continue;
    }

    // If baseline, lost, or rejected; PHA is unchanged
    if (itype >= 5) {
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping baseline/lost/rejected event" << std::endl;
      nskip_itype++;
      continue;
    }

    // If input PHA is NULL, PHA is unchanged
    if (l_rowdat.m_phanull == 1) {
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping event with PHA=NULL" << std::endl;
      nskip_phanull++;
      continue;
    }

    // If primary event: Hp, Mp, Lp; start new group
    if (itype == 0 || itype == 1 || itype == 3) {
      nprimary++;

      // Temporary copy of current row
      RowData t_rowdat=l_rowdat;
      long long trow=ahfits::currentRow(fp);

      // Perform secondary correction for completed group
      if (groupdat[pixel].size() > 0) {

        for (int k=0; k < (int)groupdat[pixel].size(); k++) {

          // perform correction for Ms events only
          if (groupdat[pixel][k].m_itype == 2) {
            AH_DEBUG << "make correction for pixel, k = " << pixel << ", " << k << std::endl;
            ncorrections++;
            for (int j=0; j < k; j++) {

              // if any previous event in sequence has output PHA = NULL, then
              // have to set PHA for this event to NULL also; a negative PHA
              // value will be turned into PHA=NULL after the end of the j loop
              if (groupdat[pixel][j].m_phaoutnull == 1) {
                groupdat[pixel][k].m_phaout=-1;
                break;
              }

              // find energy group to use
              int ipulse=0;
              while (ipulse < (int)pulsedat.m_data[pixel].size()) {
                if (groupdat[pixel][j].m_phaout >= pulsedat.m_data[pixel][ipulse].m_phamin &&
                    groupdat[pixel][j].m_phaout <= pulsedat.m_data[pixel][ipulse].m_phamax) {
                  break;
                }
                ipulse++;
              }
              if (ipulse >= (int)pulsedat.m_data[pixel].size()) {
                AH_INFO(ahlog::HIGH) << "Encountered PHA value not defined in energy range in SECPULSE CALDB file" << std::endl;
                AH_INFO(ahlog::HIGH) << "row, pixel, PHA: " << groupdat[pixel][j].m_rowidx << ", " << pixel << ", " << groupdat[pixel][j].m_phaout << std::endl;
                AH_INFO(ahlog::HIGH) << "Setting corrected PHA to NULL" << std::endl;
                AH_INFO(ahlog::HIGH) << "Note: the PHA value above may have been corrected and not match the original value" << std::endl;
                groupdat[pixel][k].m_phaout=-1;
                break;
//                AH_THROW_RUNTIME("Encountered PHA value not defined in SECPULSE CALDB file (see log for more info)");
              }
              AH_DEBUG << "  ipulse: " << ipulse << std::endl;

              double dt=groupdat[pixel][k].m_time-groupdat[pixel][j].m_time;
              int idt=dt/TIMEBINSIZE+pulsedat.m_data[pixel][ipulse].m_offset;       // convert delta-time to index in 80 microsecond bins
              AH_DEBUG << "  j, dt, idt: " << j << ", " << dt << ", " << idt << std::endl;
              if (idt >= NPULSEBINS) {
                ncorskip++;
                AH_INFO(ahlog::LOW) << "Event in row " << groupdat[pixel][k].m_rowidx
                                    << " not being corrected by event in row " << groupdat[pixel][j].m_rowidx
                                    << " since they are too far apart in time; dt = "
                                    << dt << " seconds; time bin = " << idt
                                    << " (greater than 1023)" << std::endl;
                continue;
              }


              // make correction
              int corr=pulsedat.m_data[pixel][ipulse].m_pulse[idt];
              AH_DEBUG << " correction: " << (corr*groupdat[pixel][j].m_phaout/32767) << std::endl;
              groupdat[pixel][k].m_phaout-=corr*groupdat[pixel][j].m_phaout/32767;
              AH_DEBUG << "  new PHA: " << groupdat[pixel][k].m_phaout << std::endl;
            }   // end j loop

            // if new PHA is out-of-range, set output PHA to NULL
            if (groupdat[pixel][k].m_phaout < 0) groupdat[pixel][k].m_phaoutnull=1;
            if (groupdat[pixel][k].m_phaout > 32767) groupdat[pixel][k].m_phaoutnull=1;
            if (1 == groupdat[pixel][k].m_phaoutnull) nphaoutnull++;

          }   // end if Ms

          // write to output
          l_rowdat=groupdat[pixel][k];
          ahfits::gotoRow(fp,l_rowdat.m_rowidx);
          ahfits::writeRow(fp);

        }   // end k loop

        groupdat[pixel].clear();
      }   // end size(group) > 0

      // Restore current row info.
      l_rowdat=t_rowdat;
      ahfits::gotoRow(fp,trow);

      // If not mid or low, write immediately, else store in groupdat.
      if (itype == 0) {
        ahfits::writeRow(fp);
      } else {
        groupdat[pixel].push_back(l_rowdat);
      }

    } else {     // secondary event
      nsecondary++;
      if (2 == itype) nmidsec++;
      if (4 == itype) nlowsec++;

      // only add secondary to group if not an orphan
      if (groupdat[pixel].size() > 0 && l_rowdat.m_groups > 0) {
        groupdat[pixel].push_back(l_rowdat);
      } else {
        norphan++;
        if (2 == itype) nmsorphan++;
        if (4 == itype) nlsorphan++;
        ahfits::writeRow(fp);
      }

    }    // end if secondary

  }    // end loop over rows

  // Done reading FITS file; assign group index for remaining events in groupdat.
  for (int pixel=0; pixel < 36; pixel++) {

    if (groupdat[pixel].size() > 0) {

      for (int k=0; k < (int)groupdat[pixel].size(); k++) {

        // perform correction for Ms events only
        if (groupdat[pixel][k].m_itype == 2) {
          AH_DEBUG << "make correction for pixel, k = " << pixel << ", " << k << std::endl;
          ncorrections++;
          for (int j=0; j < k; j++) {

            // if any previous event in sequence has output PHA = NULL, then
            // have to set PHA for this event to NULL also; a negative PHA
            // value will be turned into PHA=NULL after the end of the j loop
            if (groupdat[pixel][j].m_phaoutnull == 1) {
              groupdat[pixel][k].m_phaout=-1;
              break;
            }

            // find energy group to use
            int ipulse=0;
            while (ipulse < (int)pulsedat.m_data[pixel].size()) {
              if (groupdat[pixel][j].m_phaout >= pulsedat.m_data[pixel][ipulse].m_phamin &&
                  groupdat[pixel][j].m_phaout <= pulsedat.m_data[pixel][ipulse].m_phamax) {
                break;
              }
              ipulse++;
            }
            if (ipulse >= (int)pulsedat.m_data[pixel].size()) {
              AH_INFO(ahlog::HIGH) << "Encountered PHA value not defined in energy range in SECPULSE CALDB file" << std::endl;
              AH_INFO(ahlog::HIGH) << "row, pixel, PHA: " << groupdat[pixel][j].m_rowidx << ", " << pixel << ", " << groupdat[pixel][j].m_phaout << std::endl;
              AH_INFO(ahlog::HIGH) << "Setting corrected PHA to NULL" << std::endl;
              AH_INFO(ahlog::HIGH) << "Note: the PHA value above may have been corrected and not match the original value" << std::endl;
              groupdat[pixel][k].m_phaout=-1;
              break;
//            AH_THROW_RUNTIME("Encountered PHA value not defined in SECPULSE CALDB file (see log for more info)");
            }
            AH_DEBUG << "  ipulse: " << ipulse << std::endl;

            double dt=groupdat[pixel][k].m_time-groupdat[pixel][j].m_time;
            int idt=dt/TIMEBINSIZE+pulsedat.m_data[pixel][ipulse].m_offset;;       // convert delta-time to index in 80 microsecond bins
            AH_DEBUG << "  j, dt, idt: " << j << ", " << dt << ", " << idt << std::endl;
            if (idt >= NPULSEBINS) {
              ncorskip++;
              AH_INFO(ahlog::LOW) << "Event in row " << groupdat[pixel][k].m_rowidx
                                  << " not being corrected by event in row " << groupdat[pixel][j].m_rowidx
                                  << " since they are too far apart in time; dt = "
                                  << dt << " seconds; time bin = " << idt
                                  << " (greater than 1023)" << std::endl;
              continue;
            }

            // make correction
            int corr=pulsedat.m_data[pixel][ipulse].m_pulse[idt];
            AH_DEBUG << " correction: " << (corr*groupdat[pixel][j].m_phaout/32767) << std::endl;
            groupdat[pixel][k].m_phaout-=corr*groupdat[pixel][j].m_phaout/32767;
            AH_DEBUG << "  new PHA: " << groupdat[pixel][k].m_phaout << std::endl;
          }   // end j loop

          // if new PHA is out-of-range, set output PHA to NULL
          if (groupdat[pixel][k].m_phaout < 0) groupdat[pixel][k].m_phaoutnull=1;
          if (groupdat[pixel][k].m_phaout > 32767) groupdat[pixel][k].m_phaoutnull=1;
          if (1 == groupdat[pixel][k].m_phaoutnull) nphaoutnull++;

        }   // end if Ms

        // write to output
        l_rowdat=groupdat[pixel][k];
        ahfits::gotoRow(fp,l_rowdat.m_rowidx);
        ahfits::writeRow(fp);

      }   // end k loop
    }   // end if size(group) > 0
  }   // end loop over pixels

  // Report counts.
  AH_INFO(ahlog::HIGH) << "Results of secondary assignment: " << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of events read:                       " << nevents << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c bad PROC_STATUS:          " << nskip_procstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c TIME=NULL:                " << nskip_timenull << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c baseline/lost/rejected:   " << nskip_itype << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c PHA=NULL:                 " << nskip_phanull << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of primaries:                         " << nprimary << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of secondaries:                       " << nsecondary << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of mid-res secondaries (ITYPE=2):     " << nmidsec << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of low-res secondaries (ITYPE=4):     " << nlowsec << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of orphans (no correction):           " << norphan << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of mid-res orphans:                   " << nmsorphan << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of low-res orphans:                   " << nlsorphan << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of corrections made:                  " << ncorrections << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of output PHA=NULL b/c out-of-range:  " << nphaoutnull << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c delta-time out-of-range:  " << ncorskip << std::endl;

}

// ****************************************************************************

void finalize(ahfits::FilePtr fp, bool clean) {

  std::string filename;
  if (0 != fp) filename=fp->m_filename;
  ahfits::close(fp);
  if (clean && "" != filename && ahgen::fileExists(filename)) std::remove(filename.c_str());

}

// ****************************************************************************

void loadSecPulseFile(const std::string& filename, AllPulseInfo& pulsedat) {

  // variables for reading row data
  int l_pixel=0;
  int l_enerange=0;
  int l_phamin=0;
  int l_phamax=0;
  int l_offset=0;
  int l_pulse[NPULSEBINS];
  for (int i=0; i < NPULSEBINS; i++) l_pulse[i]=0;

  // open CALDB file
  ahfits::FilePtr fp;
  ahfits::open(filename,"",&fp);
  if (ahfits::isPrimary(fp)) ahfits::move(fp,"SECPULSE");

  // set up connections for reading row data
  ahfits::Router rout(fp);
  rout.connectScalar(ahfits::e_READONLY,"PIXEL",l_pixel);
  rout.connectScalar(ahfits::e_READONLY,"ENERANGE",l_enerange);
  rout.connectScalar(ahfits::e_READONLY,"PHAMIN",l_phamin);
  rout.connectScalar(ahfits::e_READONLY,"PHAMAX",l_phamax);
  rout.connectScalar(ahfits::e_READONLY,"OFFSET",l_offset);
  rout.connectFixedLengthArray(ahfits::e_READONLY,"PULSE",l_pulse);

  // read table
  for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp)) {
    ahfits::readRow(fp);

    PulseInfo row;
    row.m_phamin=l_phamin;
    row.m_phamax=l_phamax;
    row.m_offset=l_offset;
    for (int i=0; i < NPULSEBINS; i++) row.m_pulse[i]=l_pulse[i];

    pulsedat.m_data[l_pixel].push_back(row);
  }

  // close CALDB file
  ahfits::close(fp);
}

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: sxsseccor.cxx,v $
 Revision 1.3  2016/05/24 19:04:04  mwitthoe
 sxsseccor: add expression to CALDB query to match DEVPTHRE keyword from event file; change default value of phaout parameter from PHANEW to PHA2

 Revision 1.2  2016/05/13 18:47:36  mwitthoe
 sxsseccor: fix equation used to correct PHA for Ms events

 Revision 1.1  2016/04/21 15:35:34  mwitthoe
 add new task, sxssecor, which corrects the PHA for SXS secondary events


*/

