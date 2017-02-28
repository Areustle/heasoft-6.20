/// \file sxssecid.cxx
/// \brief Identify the secondary event groups and associated primary.
/// \author Mike Witthoeft
/// \date $Date: 2016/08/09 14:41:14 $
/// \version 1.0

/** 

\defgroup tool_sxssecid Identify secondary SXS events (sxssecid)
@ingroup mod_sxs_tasks

Associate secondary events with the nearest, preceding primary event.  Also,
optionally, regrade events based on TIME.

Source files:

  sxssecid.cxx

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
 
#define AHLABEL tool_sxssecid
#define AHCVSID "$Id: sxssecid.cxx,v 1.44 2016/08/09 14:41:14 mwitthoe Exp $"

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


/** \addtogroup tool_sxssecid
 *  @{
 */

/// \brief convert nanoseconds into seconds
const double NANO2SEC=1.e-9;

/// \brief convert milliseconds into seconds
const double MILLI2SEC=1.e-3;

/// \brief structure to store input parameters
struct Par {
  Par(): m_tol(0.), m_pxpithr(0), m_ckctrec(false), m_ckctel(false),
         m_ckant(false), m_ckrisetime(false), m_regrade(false) {}

  std::string m_infile;      ///< input file name
  std::string m_outfile;     ///< output file name
  std::string m_itypecol;    ///< name of ITYPE column to use
  std::string m_dtprimary;   ///< time interval (or CALDB) used to determine primary/secondary (in ms)
  std::string m_dtlowmid;    ///< time interval (or CALDB) used to determine low grade (in ms)
  std::string m_dtmidhigh;   ///< time interval (or CALDB) used to determine mid grade (in ms)
  double m_tol;              ///< amount to extend time intervals (in ms)
  long m_pxpithr;            ///< Events with PI below this value are labeled as orphans
  std::string m_usepxpithr;  ///< Choose to use pxpithr (ALL/NONE)
  bool m_ckctrec;            ///< if true, exclude events with recoil cross-talk
  bool m_ckctel;             ///< if true, exclude events with electrical cross-talk
  bool m_ckant;              ///< if true, exclude events with antico coincidence
  bool m_ckrisetime;         ///< if true, exclude events with RISE_TIME > 127
  bool m_regrade;            ///< if true, regrade events and fill column given by itypecol parameter
};

/// \brief structure to hold data from a single row of the input file
struct RowData {
  RowData(): m_rowidx(0),m_time(0.),m_timenull(0),m_pixel(0),m_itype(0),
             m_rise_time(0), m_pi(0),m_pinull(0),m_index(0),m_groups(0),
             m_num_status(10),m_num_proc_status(32) {
               for (int ip=0; ip < 32; ip++) m_proc_status[ip]=0;
               for (int ip=0; ip < 10; ip++) m_status[ip]=0;
             }

  long long m_rowidx;         ///< current row index; not a column
  double m_time;              ///< input TIME column; assigned time in seconds
  char m_timenull;            ///< =1 if TIME == NULL
  int m_pixel;                ///< input PIXEL column; pixel index from 0 to 35
  int m_itype;                ///< input ITYPE column; Hp (=0), Mp, Ms, Lp, Ls, BL, LO, or Rj (=7)
  int m_rise_time;            ///< input RISE_TIME column
  long m_pi;                  ///< input PI column
  char m_pinull;              ///< NULL flag for PI column
  long m_index;               ///< output INDEX column
  long m_groups;              ///< output GROUPS column
  long m_seq;                 ///< output SEQ column
  char m_status[10];          ///< input STATUS column
  char m_proc_status[32];     ///< input PROC_STATUS column
  ahfits::IndexType m_num_status;       ///< number of STATUS bits
  ahfits::IndexType m_num_proc_status;  ///< number of PROC_STATUS bits
};

/// \brief vector of row data
typedef std::vector<RowData> RowDataVec;

/// \brief iterator for RowDataVec
typedef std::vector<RowData>::iterator RowDataVecIter;


/// \brief Get parameter values
/// \param[out] par         structure with parameter values
void getPar(Par& par);

/// \brief Copy contents of infile to outfile; resolve default values of columns
/// \param[in] par          structure with parameter values
/// \param[out] fp          ahfits object of output file (clone of input)
/// \param[out] dtprimary   time interval used to define primary/secondary (seconds)
/// \param[out] dtlowmid    time interval used to define low grade (seconds)
/// \param[out] dtmidhigh   time interval used to define mid grade (seconds)
void initialize(const Par& par, ahfits::FilePtr& fp, double& dtprimary,
                double& dtmid, double& dtmidhigh);

/// \brief Fill in 2nd extension of output TIM file
/// \param[in] par          structure with parameter values
/// \param[in] fp           ahfits object of output file (clone of input)
/// \param[out] dtprimary   time interval used to define primary/secondary (seconds)
/// \param[out] dtlowmid    time interval used to define low grade (seconds)
/// \param[out] dtmidhigh   time interval used to define mid grade (seconds)
/// \param[out] clean       if yes, delete output files
void doWork(const Par& par, ahfits::FilePtr fp, double& dtprimary,
            double& dtlowmid, double& dtmidhigh, bool& clean);

/// \brief close open FITS files
/// \param[in] fp          ahfits object of output file (clone of input)
/// \param[in] clean       if yes, delete output files
void finalize(ahfits::FilePtr fp, bool clean);

/// \brief Regrade events and fill output ITYPE column
/// \param[in] par          structure with parameter values
/// \param[in] fp           ahfits object of output file (clone of input)
/// \param[out] dtprimary   time interval used to define primary/secondary (seconds)
/// \param[out] dtlowmid    time interval used to define low grade (seconds)
/// \param[out] dtmidhigh   time interval used to define mid grade (seconds)
/// \param[out] clean       if yes, delete output files
void regrade(const Par& par, ahfits::FilePtr fp, double& dtprimary,
             double& dtlowmid, double& dtmidhigh, bool& clean);

// ****************************************************************************

/// \brief sxssecid tool
int main(int argc, char** argv) {

  Par par;                      // structure containing parameter values
  ahfits::FilePtr fp=0;         // FITS file pointer to output file
  double dtprimary=0.;          // time interval used to define primary/secondary (seconds)
  double dtlowmid=0.;           // time interval used to define low grade (seconds)
  double dtmidhigh=0.;          // time interval used to define mid grade (seconds)
  bool clean=true;              // delete output files (upon error)

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      initialize(par,fp,dtprimary,dtlowmid,dtmidhigh);
      doWork(par,fp,dtprimary,dtlowmid,dtmidhigh,clean);
      finalize(fp,clean);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,fp,dtprimary,dtlowmid,dtmidhigh);
        doWork(par,fp,dtprimary,dtlowmid,dtmidhigh,clean);
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
  par.m_itypecol=ahapp::getParString("itypecol");
  par.m_dtprimary=ahapp::getParString("dtprimary");
  par.m_dtlowmid=ahapp::getParString("dtlowmid");
  par.m_dtmidhigh=ahapp::getParString("dtmidhigh");
  par.m_tol=NANO2SEC*ahapp::getParDouble("tol");   // convert from ns to sec
  par.m_pxpithr=ahapp::getParInt("pxpithr");
  par.m_usepxpithr=ahgen::strtoupper(ahapp::getParString("usepxpithr"));
  par.m_ckctrec=ahapp::getParBool("ckctrec");
  par.m_ckctel=ahapp::getParBool("ckctel");
  par.m_ckant=ahapp::getParBool("ckant");
  par.m_ckrisetime=ahapp::getParBool("ckrisetime");
  par.m_regrade=ahapp::getParBool("regrade");

  // Check for legal values of usepxpithr
  if (par.m_usepxpithr != "ALL" && par.m_usepxpithr != "NONE") {
    AH_THROW_RUNTIME("invalid value for usepxpithr parameter; must be ALL or NONE");
  }
}

// ****************************************************************************

void initialize(const Par& par, ahfits::FilePtr& fp, double& dtprimary,
                double& dtlowmid, double& dtmidhigh) {

  // Declare variables.
  std::string extname="";    // extension of input file to use
  std::string inst="";       // store INSTRUME keyword value
  std::string det="";        // store DETNAM keyword value
  std::string dtfile="";     // name of time interval CALDB file
  double tstart=0.;          // store TSTART keyword value
  std::string datetime;      //DATE-OBS keyword value
  
  // Copy contents of infile to outfile and return opened output file (fp).
  // If editing input file in-place (allowed with last argument = true), 
  // fp will point to the opened input file.
  extname="EVENTS";
  ahfits::clone(par.m_infile,par.m_outfile,&fp,true);
  if (ahfits::isPrimary(fp)) ahfits::move(fp,extname);   // move to EVENTS extension if extended syntax not used
  ahmission::checkEmptyTable(fp,par.m_infile);

  inst=ahfits::getKeyValStr(fp,"INSTRUME");
  det=ahfits::getKeyValStr(fp,"DETNAM");
  tstart=ahfits::getKeyValDbl(fp,"TSTART");
  datetime = ahfits::getKeyValStr(fp, "DATE-OBS");
  if ("SXS" != inst)
    AH_THROW_RUNTIME("INSTRUME keyword in input file should be SXS, not "+inst);
  if ("PIXEL" != det)
    AH_THROW_RUNTIME("DETNAM keyword in input file should be PIXEL, not "+det);

  // Add INDEX, GROUPS, and SEQ columns, if not present.
  if (!ahfits::haveColumn(fp,"INDEX") ) {
    ahfits::insertColAfter(fp,"INDEX","J","");    // insert at end
    ahfits::setColumnDescription(fp,"INDEX","sxssecid");
  }
  if (!ahfits::haveColumn(fp,"GROUPS") ) {
    ahfits::insertColAfter(fp,"GROUPS","J","");    // insert at end
    ahfits::setTNull(fp,"GROUPS",-9999);
    ahfits::setColumnDescription(fp,"GROUPS","sxssecid");
  }
  if (!ahfits::haveColumn(fp,"SEQ") ) {
    ahfits::insertColAfter(fp,"SEQ","J","");    // insert at end
    ahfits::setTNull(fp,"SEQ",-9999);
    ahfits::setColumnDescription(fp,"SEQ","sxssecid");
  }

  // Initial values for interval times; negative implies value comes from CALDB.
  dtprimary=-1.;
  dtlowmid=-1.;
  dtmidhigh=-1.;

  // Try to convert each time interval parameter to a double; if it fails
  // then assume the parameter represents a file name.
  if (ahgen::isNumber(par.m_dtmidhigh)) {
    dtmidhigh=MILLI2SEC*atof(par.m_dtmidhigh.c_str());       // convert from ms to seconds
    if (dtmidhigh <= 0.) AH_THROW_RUNTIME("dtmidhigh parameter must be greater than zero");
  } else {
    dtfile=par.m_dtmidhigh;
  }
  if (par.m_regrade) {
    if (ahgen::isNumber(par.m_dtprimary)) {
      dtprimary=MILLI2SEC*atof(par.m_dtprimary.c_str());     // convert from ms to seconds
      if (dtprimary <= 0.) AH_THROW_RUNTIME("dtprimary parameter must be greater than zero");
    } else {
      if (dtfile != "" && par.m_dtprimary != dtfile)
        AH_THROW_RUNTIME("time interval CALDB file names must be consistent");
      else
        dtfile=par.m_dtprimary;
    }
    if (ahgen::isNumber(par.m_dtlowmid)) {
      dtlowmid=MILLI2SEC*atof(par.m_dtlowmid.c_str());       // convert from ms to seconds
      if (dtlowmid <= 0.) AH_THROW_RUNTIME("dtlowmid parameter must be greater than zero");
    } else {
      if (dtfile != "" && par.m_dtlowmid != dtfile)
        AH_THROW_RUNTIME("time interval CALDB file names must be consistent");
      else
        dtfile=par.m_dtlowmid;
    }
  }    // end if (par.m_regrade)

  // Get time intervals from CALDB file, if necessary.
  // Note: times in CALDB file are in seconds.
  if (dtfile != "") {
    dtfile=ahmission::caldb::resolve(dtfile,"delta-t","SXS","-","DELTIMES",datetime);
    ahsxs::sxsdt::SXSTimeIntervals dts;    // store CALDB information
    ahsxs::sxsdt::loadSXSTimeIntervals(dtfile,tstart,dts);
    if (dtmidhigh < 0.) dtmidhigh=dts.m_dtmidhigh;
    if (par.m_regrade) {
      if (dtprimary < 0.) dtprimary=dts.m_dtprimary;
      if (dtlowmid < 0.) dtlowmid=dts.m_dtlowmid;
    }
  }

  AH_INFO(ahlog::HIGH) << "Delta time values used: " << std::endl;
  if (par.m_regrade) {
    AH_INFO(ahlog::HIGH) << "  dtprimary: " << dtprimary << std::endl;
    AH_INFO(ahlog::HIGH) << "  dtlowmid:  " << dtlowmid << std::endl;
    AH_INFO(ahlog::HIGH) << "  dtmidhigh: " << dtmidhigh << std::endl;
  } else {
    AH_INFO(ahlog::HIGH) << "  dtprimary: not used" << std::endl;
    AH_INFO(ahlog::HIGH) << "  dtlowmid:  not used" << std::endl;
    AH_INFO(ahlog::HIGH) << "  dtmidhigh: " << dtmidhigh << std::endl;
  }

  // Check that dtlowmid < dtmidhigh.
  if (dtlowmid >= dtmidhigh) 
    AH_THROW_RUNTIME("dtlowmid must be smaller than dtmidhigh");

  // Write list of parameters to log file.
  ahapp::writeParametersToLog(); 
}

// ****************************************************************************

void doWork(const Par& par, ahfits::FilePtr fp, double& dtprimary,
            double& dtlowmid, double& dtmidhigh, bool& clean) {

  // Declare variables.
  RowDataVec groupdat[36];          // store group data for each pixel
  RowDataVec grouporphan[36];       // store orphans for each pixel
  RowData l_rowdat;                 // store current row data
  char groups_null=0;               // =1 to have GROUP column set to NULL
  char seq_null=0;                  // =1 to have the SEQ column set to NULL
  ahfits::Router rout(fp);          // ahfits router for input/output file
  long evcounter=0;                 // event counter
  long lastgroup[36]={0};           // index of last group index written

  // Counters
  long long nevents=0;              // number of events read
  long long nskip_procstatus=0;     // number with bad PROC_STATUS
  long long nskip_timenull=0;       // number with TIME=NULL
  long long nskip_risetime=0;       // number with RISE_TIME > 127
  long long nskip_ctrec=0;          // number skipped due to recoil cross-talk
  long long nskip_ctel=0;           // number skipped due to electrical cross-talk
  long long nskip_ant=0;            // number skipped due to antico coincidence
  long long nskip_itype=0;          // number with wrong ITYPE
  long long nskip_pithr=0;          // number of events with PI < pxpithr        
  long long nprimary=0;             // number of primary events
  long long nsecondary=0;           // number of secondaries
  long long nsecassign=0;           // number of secondaries assigned
  long long nsecorphan=0;           // number of secondary orphans 

  // Regrade events, if necessary.
  if (par.m_regrade) regrade(par,fp,dtprimary,dtlowmid,dtmidhigh,clean);

  // Make connections between input file and local variables.
  rout.connectScalar(ahfits::e_READONLY,"PIXEL",l_rowdat.m_pixel);
  rout.connectScalar(ahfits::e_READONLY,par.m_itypecol,l_rowdat.m_itype);
  rout.connectScalar(ahfits::e_READONLY,"TIME",l_rowdat.m_time,&l_rowdat.m_timenull);
  rout.connectScalar(ahfits::e_READONLY,"RISE_TIME",l_rowdat.m_rise_time);
  rout.connectScalar(ahfits::e_READONLY,"PI",l_rowdat.m_pi,&l_rowdat.m_pinull);
  rout.connectScalar(ahfits::e_WRITEONLY,"INDEX",l_rowdat.m_index);
  rout.connectScalar(ahfits::e_WRITEONLY,"GROUPS",l_rowdat.m_groups,&groups_null);
  rout.connectScalar(ahfits::e_WRITEONLY,"SEQ",l_rowdat.m_seq,&seq_null);
  rout.connectBit(ahfits::e_READONLY,"PROC_STATUS",l_rowdat.m_proc_status,l_rowdat.m_num_proc_status);

  // Only need to connet to STATUS column if chkstatus parameter is set.
  // If chkstatus=no, then l_rowdat.m_status will always be zero.
  if (par.m_ckctrec || par.m_ckctel || par.m_ckant) {
    rout.connectBit(ahfits::e_READONLY,"STATUS",l_rowdat.m_status,l_rowdat.m_num_status);
  }

  AH_INFO(ahlog::HIGH) << "Assigning secondaries" << std::endl;

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

    // Reset GROUPS and SEQ null flags.
    groups_null=0;
    seq_null=0;

    // Assign next event index.
    evcounter++;
    l_rowdat.m_index=evcounter;
    l_rowdat.m_groups=l_rowdat.m_index;   // will be updated later, if necessary
    l_rowdat.m_seq=l_rowdat.m_itype+1;    // will be updated later, if necessary

    // If bad PROC_STATUS, set GROUPS to NULL and continue to next row.
    if (!procstatus::processRow(l_rowdat.m_proc_status)) {
      groups_null=1;
      seq_null=1;
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping event with bad PROC_STATUS" << std::endl;
      nskip_procstatus++;
      continue;
    }

    // If TIME is NULL, set GROUPS to NULL and continue to next row.
    if (l_rowdat.m_timenull != 0) {
      groups_null=1;
      seq_null=1;
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping event with TIME=NULL" << std::endl;
      nskip_timenull++;
      continue;
    }

    // If baseline, lost, or rejected; set GROUPS to NULL.
    if (itype >= 5) {
      groups_null=1;
      seq_null=1;
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping baseline/lost/rejected event" << std::endl;
      nskip_itype++;
      continue;
    }

    // If RISE_TIME > 127, set GROUPS to NULL
    if (par.m_ckrisetime && l_rowdat.m_rise_time > 127) {
      groups_null=1;
      seq_null=1;
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping event with RISE_TIME > 127" << std::endl;
      nskip_risetime++;
      continue;
    }

    // Skip recoil cross-talk events
    if (par.m_ckctrec && ahsxs::has_status_recoil_crosstalk(l_rowdat.m_status)) {
      groups_null=1;
      seq_null=1;
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping event flagged as recoil cross-talk" << std::endl;
      nskip_ctrec++;
      continue;
    }

    // Skip residual electrical cross-talk events (do not skip source event)
    if (par.m_ckctel && ahsxs::has_status_electrical_crosstalk_residual(l_rowdat.m_status)) {
      groups_null=1;
      seq_null=1;
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping event flagged as residual electrical cross-talk" << std::endl;
      nskip_ctel++;
      continue;
    }

    // Skip antico events
    if (par.m_ckant && ahsxs::has_status_antico(l_rowdat.m_status)) {
      groups_null=1;
      seq_null=1;
      ahfits::writeRow(fp);
      AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": skipping event flagged as antico" << std::endl;
      nskip_ant++;
      continue;
    }

    // Set as orphan relative to self if PI below threshold
    if (par.m_usepxpithr == "ALL") {
      if (l_rowdat.m_pinull == 1 || l_rowdat.m_pi < par.m_pxpithr) {
        l_rowdat.m_groups=-l_rowdat.m_index;
        ahfits::writeRow(fp);
        AH_DEBUG << "Row " << l_rowdat.m_rowidx << ": event has NULL PI or PI < pxpithr; setting as orphan" << std::endl;
        nskip_pithr++;
        continue;
      }
    }

    // If primary event: Hp, Mp, Lp
    if (itype == 0 || itype == 1 || itype == 3) {
      nprimary++;

      // Temporary copy of current row
      RowData t_rowdat=l_rowdat;
      long long trow=ahfits::currentRow(fp);

      // Associate any secondary events with previous group.
      if (groupdat[pixel].size() > 0) {
      
        // Check if current primary too close to previous, groupdat event.
        double dt=l_rowdat.m_time-groupdat[pixel].back().m_time;
        if (dt < dtprimary+par.m_tol) {
          AH_INFO(ahlog::HIGH) << " *** Primary event within dtprimary of previous event; row: " << fp->m_currow << std::endl;
        }

        // Form sequence integer.
        long seq=0;
        int n=groupdat[pixel].size();
        if (n <= 9) {
          int i=0;
          for(RowDataVecIter it=groupdat[pixel].begin(); it != groupdat[pixel].end(); it++) {
            seq+=(it->m_itype+1)*(int)std::pow(10.,n-i-1);
            i++;
          }
        } else {
          seq=999;      // impossible sequence value if group size too large
        }

        lastgroup[pixel]=groupdat[pixel][0].m_index;
        for(RowDataVecIter it=groupdat[pixel].begin(); it != groupdat[pixel].end(); it++) {
          it->m_groups=lastgroup[pixel];
          it->m_seq=seq;
          l_rowdat=*it;
          ahfits::gotoRow(fp,l_rowdat.m_rowidx);
          ahfits::writeRow(fp);
        }
        groupdat[pixel].clear();
      }

      // Associate orphan events with a group.
      if (grouporphan[pixel].size() > 0) {
        for(RowDataVecIter it=grouporphan[pixel].begin(); it != grouporphan[pixel].end(); it++) {
          if (lastgroup[pixel] == 0) lastgroup[pixel]=it->m_rowidx;    // if orphan appears in file before primary for a given pixel
          it->m_groups=-lastgroup[pixel];
          seq_null=1;
          l_rowdat=*it;
          ahfits::gotoRow(fp,l_rowdat.m_rowidx);
          ahfits::writeRow(fp);
        }
        grouporphan[pixel].clear();
      }

      // Restore current row info.
      l_rowdat=t_rowdat;
      ahfits::gotoRow(fp,trow);

      // If not mid or low, write immediately, else store in groupdat.
      if (itype == 0 || itype >= 5) {
        lastgroup[pixel]=l_rowdat.m_groups;
        ahfits::writeRow(fp);
      } else {
        groupdat[pixel].push_back(l_rowdat);
      }
      continue;
    }    // end if itype == Hp, Mp, Lp

    // === secondary events ===
    // 3-part if-statement:
    //   1. if groupdat is empty
    //      - add event to groupdat
    //   2. if secondary is close enough to last event in groupdat
    //      - add event to groupdat
    //      - assign group index to all orphans
    //   3. if secondary is too far from last event in groupdat
    //      - add event to orphans
    //      - assign group index to all events in groupdat
    nsecondary++;
    if (groupdat[pixel].size() == 0) {
      nsecorphan++;
      grouporphan[pixel].push_back(l_rowdat);
    } else if (l_rowdat.m_time-groupdat[pixel].back().m_time < dtmidhigh+par.m_tol) {
      groupdat[pixel].push_back(l_rowdat);

      // Assign group index for orphans.
      nsecassign++;
      if (grouporphan[pixel].size() > 0) {
        long long trow=ahfits::currentRow(fp);
        for(RowDataVecIter it=grouporphan[pixel].begin(); it != grouporphan[pixel].end(); it++) {
          it->m_groups=-lastgroup[pixel];
          seq_null=1;
          l_rowdat=*it;
          ahfits::gotoRow(fp,l_rowdat.m_rowidx);
          ahfits::writeRow(fp);
        }
        grouporphan[pixel].clear();
        ahfits::gotoRow(fp,trow);
      }
    } else {                  // secondary further than dtmidhigh from previous non-orphan
      grouporphan[pixel].push_back(l_rowdat);
      nsecorphan++;

      // Form sequence integer.
      long seq=0;
      int n=groupdat[pixel].size();
      if (n <= 9) {
        int i=0;
        for(RowDataVecIter it=groupdat[pixel].begin(); it != groupdat[pixel].end(); it++) {
          seq+=(it->m_itype+1)*(int)std::pow(10.,n-i-1);
          i++;
        }
      } else {
        seq=999;      // impossible sequence value if group size too large
      }

      // Assign group index for groupdat.
      if (groupdat[pixel].size() > 0) {
        long long trow=ahfits::currentRow(fp);
        lastgroup[pixel]=groupdat[pixel][0].m_groups;
        for(RowDataVecIter it=groupdat[pixel].begin(); it != groupdat[pixel].end(); it++) {
          it->m_groups=lastgroup[pixel];
          it->m_seq=seq;
          l_rowdat=*it;
          ahfits::gotoRow(fp,l_rowdat.m_rowidx);
          ahfits::writeRow(fp);
        }
        groupdat[pixel].clear();
        ahfits::gotoRow(fp,trow);
      }
    }    // end 3-part if-statement

  }    // end loop over rows

  // Done reading FITS file; assign group index for remaining events in groupdat.
  for (int pixel=0; pixel < 36; pixel++) {

    if (groupdat[pixel].size() > 0) {

      // Form sequence integer.
      long seq=0;
      int n=groupdat[pixel].size();
      if (n <= 9) {
        int i=0;
        for(RowDataVecIter it=groupdat[pixel].begin(); it != groupdat[pixel].end(); it++) {
          seq+=(it->m_itype+1)*(int)std::pow(10.,n-i-1);
          i++;
        }
      } else {
        seq=999;      // impossible sequence value if group size too large
      }

      lastgroup[pixel]=groupdat[pixel][0].m_groups;
      for(RowDataVecIter it=groupdat[pixel].begin(); it != groupdat[pixel].end(); it++) {
        groups_null=0;
        seq_null=0;
        it->m_groups=lastgroup[pixel];
        it->m_seq=seq;
        l_rowdat=*it;
        ahfits::gotoRow(fp,l_rowdat.m_rowidx);
        ahfits::writeRow(fp);
      }
    }

    // Assign group index for remaining events in grouporphan.
    if (grouporphan[pixel].size() > 0) {
      for(RowDataVecIter it=grouporphan[pixel].begin(); it != grouporphan[pixel].end(); it++) {
        it->m_groups=-lastgroup[pixel];
        seq_null=1;
        l_rowdat=*it;
        ahfits::gotoRow(fp,l_rowdat.m_rowidx);
        ahfits::writeRow(fp);
      }
    }
  }    // end loop over pixels

  // Report counts.
  AH_INFO(ahlog::HIGH) << "Results of secondary assignment: " << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of events read:                       " << nevents << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with bad PROC_STATUS:                 " << nskip_procstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with TIME=NULL:                       " << nskip_timenull << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of baseline/lost/rejected:            " << nskip_itype << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of RISE_TIME > 127:                   " << nskip_risetime << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with recoil cross-talk:               " << nskip_ctrec << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with residual electrical cross-talk:  " << nskip_ctel << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with antico coincidence:              " << nskip_ant << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with PI < pxpithr:                    " << nskip_pithr << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of primaries:                         " << nprimary << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of secondaries:                       " << nsecondary << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of assigned secondaries:              " << nsecassign << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of orphan secondaries:                " << nsecorphan << std::endl;

}

// ****************************************************************************

void finalize(ahfits::FilePtr fp, bool clean) {

  std::string filename;
  if (0 != fp) filename=fp->m_filename;
  ahfits::close(fp);
  if (clean && "" != filename && ahgen::fileExists(filename)) std::remove(filename.c_str());

}

// ****************************************************************************

void regrade(const Par& par, ahfits::FilePtr fp, double& dtprimary,
             double& dtlowmid, double& dtmidhigh, bool& clean) {

  // The grade (ITYPE) of a single event can be determined by knowing the TIME
  // of the previous and next events from the same PIXEL.  Therefore, we will
  // always hold three events in memory for each pixel and determine the grade
  // of the middle of the three events.  The grades of the first and last events
  // of each pixel will not be changed.
  //
  // The regrading scheme requires two passes through the file.  The first pass
  // will assign the grade for each event and the second pass will write the 
  // grades to the column given by the itype parameter.

  // Declare variables.
  int* itypeout=0;        // array to hold output ITYPE for each column
  long long row1[36];     // array to hold row number for first point per pixel
  double time1[36];       // array to hold TIME for first point per pixel
  long long row2[36];     // array to hold row number for second point per pixel
  double time2[36];       // array to hold TIME for second point per pixel
  int itype2[36];         // array to hold ITYPE for second point per pixel
  RowData l_rowdat;       // store current row data

  // Counters
  long long nevents=0;              // number of events read
  long long nskip_itype=0;          // number skipped due to wrong ITYPE
  long long nskip_timenull=0;       // number skipped due to TIME=NULL
  long long nskip_procstatus=0;     // number skipped due to bad PROC_STATUS
  long long nskip_risetime=0;       // number skipped due to RISE_TIME > 127
  long long nskip_ctrec=0;          // number skipped due to recoil cross-talk
  long long nskip_ctel=0;           // number skipped due to residual electrical cross-talk
  long long nskip_ant=0;            // number skipped due to antico coincidence
  long long nskip_pithr=0;          // number skipped b/c event has PI < pxpithr
  long long ngrade=0;               // number of events regraded
  long long nitype[5]={0,0,0,0,0};  // number of events per grade (0=Hp, etc)
  long long nchange=0;              // number of events that change grades

  if (!par.m_regrade) return;    // nothing to do

  AH_INFO(ahlog::HIGH) << "Regrading events - output column: " << par.m_itypecol << std::endl;

  // Do not allow the ITYPE column to be overwritten.
  if (par.m_itypecol == "ITYPE") {
    AH_THROW_RUNTIME("Not allowed to overwrite the ITYPE column when regrading, set itypecol to another value");
  }

  // Add new ITYPE column, if necessary.
  if (!ahfits::haveColumn(fp,par.m_itypecol)) {
    ahfits::insertColAfter(fp,par.m_itypecol,"B");
    ahfits::setColumnDescription(fp,par.m_itypecol,"sxssecid");
    AH_INFO(ahlog::HIGH) << "Adding column, " << par.m_itypecol << ", for storing new grades" << std::endl;
  }

  // Allocate output ITYPE array to number of rows in file.
  long long nrows=fp->m_numrow;
  itypeout=new int[nrows];
  for (int ii=0; ii < nrows; ii++) {
    itypeout[ii]=0;
  }

  // Initialize arrays to store first point per pixel.
  for (int ip=0; ip < 36; ip++) {
    row1[ip]=-1;
    time1[ip]=-1.;
  }

  // Initialize arrays to store second point per pixel.
  for (int ip=0; ip < 36; ip++) {
    row2[ip]=-1;
    time2[ip]=-1.;
    itype2[ip]=-1;
  }

  // Make connections between local variables and FITS columns.
  ahfits::Router rout(fp);
  rout.connectScalar(ahfits::e_READONLY,"TIME",l_rowdat.m_time,&l_rowdat.m_timenull);
  rout.connectScalar(ahfits::e_READONLY,"PIXEL",l_rowdat.m_pixel);
  rout.connectScalar(ahfits::e_READONLY,"ITYPE",l_rowdat.m_itype);
  rout.connectScalar(ahfits::e_READONLY,"PI",l_rowdat.m_pi,&l_rowdat.m_pinull);
  rout.connectBit(ahfits::e_READONLY,"PROC_STATUS",l_rowdat.m_proc_status,l_rowdat.m_num_proc_status);
  rout.connectScalar(ahfits::e_READONLY,"RISE_TIME",l_rowdat.m_rise_time);

  // Only need to connet to STATUS column if chkstatus parameter is set.
  // If chkstatus=no, then l_rowdat.m_status will always be zero.
  if (par.m_ckctrec || par.m_ckctel || par.m_ckant) {
    rout.connectBit(ahfits::e_READONLY,"STATUS",l_rowdat.m_status,l_rowdat.m_num_status);
  }

  // Perform first iteration through file to assign grade.
  clean=false;      // at this point the output files are not deleted if there is an error
  long long irow=0;
  for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp)) {
    irow++;
    ahfits::readRow(fp);
    nevents++;

    // Initialize output ITYPE to the original ITYPE so that, if the event
    // is skipped, the ITYPE value will be unchanged.
    itypeout[irow-1]=l_rowdat.m_itype;    // -1 since first index is zero

    // Skip events with a bad PROC_STATUS => itypeout=ITYPE.
    if (!procstatus::processRow(l_rowdat.m_proc_status)) {
      nskip_procstatus++;
      AH_INFO(ahlog::LOW) << "Row " << irow << ": skipping event with bad PROC_STATUS" << std::endl;
      continue;
    }

    // Skip events where TIME is NULL => itypeout=ITYPE.
    if (l_rowdat.m_timenull != 0) {
      nskip_timenull++;
      AH_INFO(ahlog::LOW) << "Row " << irow << ": skipping event with TIME=NULL" << std::endl;
      continue;
    }

    // Skip baseline, lost, and rejected events => itypeout=ITYPE.
    if (l_rowdat.m_itype >= 5) {
      nskip_itype++;
      AH_INFO(ahlog::LOW) << "Row " << irow << ": skipping baseline, lost, or rejected event" << std::endl;
      continue;
    }

    // Skip events with RISE_TIME > 127 => itypeout=ITYPE.
    if (par.m_ckrisetime && l_rowdat.m_rise_time > 127) {
      nskip_risetime++;
      AH_INFO(ahlog::HIGH) << "Row " << irow << ": skipping event with RISE_TIME > 127" << std::endl;
      continue;
    }

    // Skip recoil cross talk events
    if (par.m_ckctrec && ahsxs::has_status_recoil_crosstalk(l_rowdat.m_status)) {
      nskip_ctrec++;
      AH_INFO(ahlog::LOW) << "Row " << irow << ": skipping event flagged as recoil cross-talk" << std::endl;
      continue;
    }

    // Skip residual electrical cross talk events
    if (par.m_ckctel && ahsxs::has_status_electrical_crosstalk_residual(l_rowdat.m_status)) {
      nskip_ctel++;
      AH_INFO(ahlog::LOW) << "Row " << irow << ": skipping event flagged as residual electrical cross-talk" << std::endl;
      continue;
    }

    // Skip antico events
    if (par.m_ckant && ahsxs::has_status_antico(l_rowdat.m_status)) {
      nskip_ant++;
      AH_INFO(ahlog::LOW) << "Row " << irow << ": skipping event flagged with antico coincidence" << std::endl;
      continue;
    }

    // Skip events below PI threshold
    if (par.m_usepxpithr == "ALL") {
      if (l_rowdat.m_pinull == 1 || l_rowdat.m_pi < par.m_pxpithr) {
        nskip_pithr++;
        AH_INFO(ahlog::LOW) << "Row " << irow << ": skipping event with NULL PI or PI < pxpithr" << std::endl;
        continue;
      }
    }

    // If point 1 is undefined; set to current event and continue loop.
    if (row1[l_rowdat.m_pixel] < 0) {
      row1[l_rowdat.m_pixel]=irow;
      time1[l_rowdat.m_pixel]=l_rowdat.m_time;
      continue;
    }

    // If point 2 is undefined; set to current event and continue loop.
    if (row2[l_rowdat.m_pixel] < 0) {
      row2[l_rowdat.m_pixel]=irow;
      time2[l_rowdat.m_pixel]=l_rowdat.m_time;
      itype2[l_rowdat.m_pixel]=l_rowdat.m_itype;
      continue;
    }

    // Now, we have three points and we can regrade the middle point.
    ngrade++;
    itypeout[row2[l_rowdat.m_pixel]-1]=ahsxs::calcGrade(time1[l_rowdat.m_pixel],   // prior event
                                                        time2[l_rowdat.m_pixel],   // current event
                                                        l_rowdat.m_time,           // next event
                                                        dtprimary,dtlowmid,dtmidhigh);
    nitype[itypeout[row2[l_rowdat.m_pixel]-1]]++;     // populate histogram of new grades
    if (itypeout[row2[l_rowdat.m_pixel]-1] != itype2[l_rowdat.m_pixel]) {
      nchange++;
      AH_INFO(ahlog::LOW) << "Row " << row2[l_rowdat.m_pixel] 
                          << ": ITYPE change; original, new = " 
                          << itype2[l_rowdat.m_pixel] << ", " 
                          << itypeout[row2[l_rowdat.m_pixel]-1] << std::endl;
    }

    // Shift stored rows.
    row1[l_rowdat.m_pixel]=row2[l_rowdat.m_pixel];
    time1[l_rowdat.m_pixel]=time2[l_rowdat.m_pixel];
    row2[l_rowdat.m_pixel]=irow;
    time2[l_rowdat.m_pixel]=l_rowdat.m_time;
    itype2[l_rowdat.m_pixel]=l_rowdat.m_itype;

  }   // end row loop


  // Reset router and connect to output ITYPE column.
  rout.connectScalar(ahfits::e_WRITEONLY,par.m_itypecol,l_rowdat.m_itype);

  // Perform second iteration through file to write new grade.
  irow=0;
  for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp)) {
    irow++;
    l_rowdat.m_itype=itypeout[irow-1];
    ahfits::writeRow(fp);
  }

  // Report counts.
  AH_INFO(ahlog::HIGH) << "Results of regrading: " << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of events read:                       " << nevents << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with bad PROC_STATUS:                 " << nskip_procstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with TIME=NULL:                       " << nskip_timenull << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of baseline/lost/rejected:            " << nskip_itype << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with RISE_TIME > 127:                 " << nskip_risetime << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with recoil cross-talk:               " << nskip_ctrec << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with residual electrical cross-talk:  " << nskip_ctel << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with antico coincidence:              " << nskip_ant << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with PI NULL or below pxpithr:        " << nskip_pithr << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number being regraded:                       " << ngrade << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of Hp events (ITYPE = 0):             " << nitype[0] << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of Mp events (ITYPE = 1):             " << nitype[1] << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of Ms events (ITYPE = 2):             " << nitype[2] << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of Lp events (ITYPE = 3):             " << nitype[3] << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of Ls events (ITYPE = 4):             " << nitype[4] << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of events where grade changed:        " << nchange << std::endl;

  // Clean up memory.
  delete [] itypeout;
}

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: sxssecid.cxx,v $
 Revision 1.44  2016/08/09 14:41:14  mwitthoe
 sxssecid: add parameter, usepxpithr, which decides whether or not to test event PI values against the pxpithr parameter

 Revision 1.43  2016/07/25 18:39:24  mwitthoe
 sxssecid: add parameter, pxpithr; events below this PI threshold are skipped

 Revision 1.42  2016/04/07 17:25:41  mwitthoe
 sxssecid: fix bug where TIME=NULL was not properly being checked

 Revision 1.41  2016/04/05 20:18:26  mwitthoe
 sxssecid: change some AH_INFO statments to AH_DEBUG to cut down on log output

 Revision 1.40  2016/03/24 19:35:59  asargent
 Changed an event loop info statement to a debug statement

 Revision 1.39  2016/03/18 15:14:07  asargent
 Replaced AH_WARN with AH_INFO

 Revision 1.38  2015/12/29 16:42:47  mwitthoe
 sxssecid: throw error if no rows in input file

 Revision 1.37  2015/10/14 14:52:08  mwitthoe
 sxssecid: bug-fix - ckrisetime parameter was not be used in the regrading routine; now RISE_TIME>127 events are skipped in regrading if ckrisetime is yes

 Revision 1.36  2015/10/07 15:38:56  mwitthoe
 sxssecid: add parameter, ckrisetime, to allow skipping of events with RISE_TIME>127

 Revision 1.35  2015/10/02 18:44:47  mwitthoe
 sxssecid: switch CALDB query over to general function

 Revision 1.34  2015/09/18 20:13:49  mwitthoe
 sxssecid & sxspha2pi: be more careful in removing files in finalize upon an error; now the file is checked for existence before trying to delete it

 Revision 1.33  2015/09/17 20:39:18  mwitthoe
 sxssecid: 1) change default value of dt* parameters to CALDB; 2) split chkstatus parameter in ckctrec, ckctel, and ckant parameters to check recoil/electrical and antico flagging separately; 3) delete output file if there is an early error; see issue 554

 Revision 1.32  2015/08/06 20:33:08  mwitthoe
 sxssecid; perform tool clean-up; see issues 532, 534, and 543

 Revision 1.31  2015/07/15 20:58:45  mdutka
 Adding CALDB query

 Revision 1.30  2015/06/05 18:50:14  mwitthoe
 sxssecid: change type of output ITYPE column from I to B

 Revision 1.29  2015/04/03 18:47:49  mwitthoe
 sxssecid: update boolean parameters; covert dtfile to uppercase before checking for CALDB

 Revision 1.28  2015/03/18 20:45:55  mwitthoe
 sxssecid: change DETNAME to DETNAM

 Revision 1.27  2015/01/09 13:34:43  mwitthoe
 sxssecid: fix bug where null flags were not being reset when writing remaining groups after reaching the end of the event file; reduce max group size which generates a SEQ value so that it always fits into the J range

 Revision 1.26  2014/12/29 20:53:23  driethmi
 Updated parameter list.

 Revision 1.25  2014/12/04 18:42:33  mwitthoe
 sxssecid: add include statement for ahgen

 Revision 1.24  2014/11/19 16:26:42  mwitthoe
 sxssecid: start using general function for computing the event grade from ahsxs

 Revision 1.23  2014/10/10 20:26:21  mwitthoe
 sxssecid: fix indeterminate type issue in std::pow() call -- take 2

 Revision 1.22  2014/10/10 17:32:29  mwitthoe
 sxssecid: fix indeterminate type issue in std::pow() call

 Revision 1.21  2014/10/09 21:25:32  mwitthoe
 sxssecid: add output SEQ column which gives the sequence of ITYPES of the primary/secondary group

 Revision 1.20  2014/10/01 23:40:44  mwitthoe
 sxssecid: require that the dtlowmid parameter is smaller than dtmidhigh

 Revision 1.19  2014/10/01 01:27:13  mwitthoe
 sxssecid: column names for time interval CALDB file were changes; algorithm for regrading was corrected

 Revision 1.18  2014/09/30 23:59:50  mwitthoe
 sxssecid: implement changes resulting from Japan trip in Sep 2014: create CALDB file for time intervals, add regrading option, add option to check STATUS column; see issue 445

 Revision 1.17  2014/09/15 21:22:09  mwitthoe
 sxssecid: add support for extended syntax; issue 179

 Revision 1.16  2014/09/12 21:32:15  mwitthoe
 sxssecid: remove instrument argument in procstatus::processRow() -- there is no instrument dependency

 Revision 1.15  2014/01/22 21:52:21  mwitthoe
 sxssecid: update according to code review; issue 331

 Revision 1.14  2014/01/22 19:42:08  peachey
 Add Peer Review comments regarding variable declarations,

 Revision 1.13  2014/01/13 22:06:05  mwitthoe
 sxssecid: switch to new PROC_STATUS function; see issue 314

 Revision 1.12  2014/01/09 19:55:27  rshill
 Code review comment

 Revision 1.11  2014/01/03 22:16:12  mwitthoe
 sxssecid: update standard main, see issue 327

 Revision 1.10  2013/12/02 22:59:43  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.9  2013/10/23 20:18:52  mwitthoe
 sxssecid: update description to explain meaning of negative values in the output GROUPS column

 Revision 1.8  2013/10/07 21:40:02  mwitthoe
 sxssecid: switch to using new ahfits connect functions (issue e 270)

 Revision 1.7  2013/08/29 18:57:34  mwitthoe
 sxssecid: if, for a given pixel, a secondary event occurs before any primary events, set GROUPS=-INDEX instead of GROUPS=0

 Revision 1.6  2013/08/02 18:07:34  mwitthoe
 sxssecid: fixed bug discussed in issue 274

 Revision 1.5  2013/07/31 20:24:17  mwitthoe
 sxssecid: remove unnecessary include for ahgen

 Revision 1.4  2013/07/19 18:11:44  mwitthoe
 sxssecid: add TNULL for output GROUPS column

 Revision 1.3  2013/07/09 14:01:07  mwitthoe
 sxssecid: add PROC_STATUS check

 Revision 1.2  2013/05/28 22:01:44  mwitthoe
 sxssecid: add comments to explain source code; move variable declarations to top of scope

 Revision 1.1  2013/05/28 21:30:43  mwitthoe
 add new tool, sxssecid, which identifies secondary events in SXS EVENTS files


*/

