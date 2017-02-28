/// \file mxstime.cxx
/// \brief Assign time to interval start and stop times in MXS files
/// \author Mike Witthoeft
/// \date $Date: 2017/01/13 18:52:15 $
/// \version 1.0

/** 

\defgroup tool_mxstime MXS Time Assignment (mxstime)
@ingroup mod_sxs_tasks

This tool operates on the SXS HK file extension HK_SXS_FWE to assign start 
and stop time for each of four LEDs (8 columns total)  and/or to calculate  
the GTI (course and fine) for when the LEDs are on. 

Source files:

  mxstime.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath
  astroh/mission/lib/ahmission
  astroh/mission/lib/ahtime

Modification history:

  Ver   Date        Author  Description
  1.0   2015-07-29   MCW    Clean-up code

*/
 
#define AHLABEL tool_mxstime
#define AHCVSID "$Id: mxstime.cxx,v 1.66 2017/01/13 18:52:15 mwitthoe Exp $"

#define TOOLTAG "$Name: Hitomi_dev $"

#include "ahapp/ahapp.h"
#include "ahmath/ahmath.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahtime/ahtime.h"
#include "ahgen/ahgen.h"
#include "ahsxs/ahsxs.h"
#include "ahsxs/sxsdt.h"
#include "ahmission/ahmission.h"
#include "ahmission/timfile.h"
#include "ahmission/delay.h"
#include "ahmission/caldb.h"
#include "ahmission/keyword.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <sstream>
#include <algorithm>     // std::count
#include <cstdio>        // std::remove()

/** \addtogroup tool_mxstime
 *  @{
 */

/// \brief type used to store 40-bit TI column values
typedef long long type_ti;

/// \brief type used to store output TIME values
typedef double type_time;

/// \brief type used to store pulse dimensions
typedef int type_pulse;


/// \brief number of LEDs
const int NUMLED=4;

/// \brief conversion factor for the pulse spacing into seconds 
const double UNITPLSSPC = 0.015625;

/// \brief conversion factor for the pulse length into seconds
const double UNITPLSLEN = 0.000125;


/// \brief structure to hold parameter values
struct Par {

  Par(): m_calctime(false), m_calcgti(false), m_dtdecay(0.), m_mxsoffset(0.),
         m_interp(0) {};

  std::string m_infile;          ///< name of input MXS file
  std::string m_outfile;         ///< name of output MXS file with time assignments
  std::string m_outgti;          ///< template of output GTI files
  std::string m_timfile;         ///< name of TIM file
  std::string m_delayfile;       ///< name instrument delay CALDB file
  std::string m_leapsecfile;     ///< leap second FITS file location (or CALDB)
  std::string m_stimecol;        ///< name of S_TIME column
  std::string m_tioncol;         ///< name of columns with LED-on TIs (requires single # character)
  std::string m_tioffcol;        ///< name of columns with LED-off TIs (requires single # character)
  std::string m_plslencol;       ///< name of columns with LED pulse lengths (requires single # character)
  std::string m_plsspccol;       ///< name of columns with LED pulse spacings (requires single # character)
  std::string m_timeoncol;       ///< name of columns with LED-on TIMEs (requires single # character)
  std::string m_timeoffcol;      ///< name of columns with LED-off TIMEs (requires single # character)
  bool m_calctime;               ///< true to assign interval time columns
  bool m_calcgti;                ///< true to create GTI file
  bool m_afterglow;              ///< true if adding afterglow time to fine GTI STOP values
  double m_dtdecay;              ///< afterglow time in seconds
  double m_mxsoffset;            ///< Shift applied to MXS GTI times (in seconds)
  int m_interp;                  ///< interp interpolation type

  std::string m_dtdecaypar;      ///< dtdecay parameter value which will be converted to a double
};

/// \brief store column data for a single MXS row
struct RowInfo {

  RowInfo(): m_stime(0.), m_num_proc_status(32) {
    for (int iled1=0; iled1 < NUMLED; iled1++) {
      m_tion[iled1]=0LL;
      m_tioff[iled1]=0LL;
      m_plslen[iled1]=0;
      m_plsspc[iled1]=0;
      m_timeon[iled1]=0.;
      m_timeoff[iled1]=0.;
      m_timeon_null[iled1]=0;
      m_timeoff_null[iled1]=0;
    }
    for (int iii=0; iii < 32; iii++) {
      m_proc_status[iii]=0;
    }
  };
  
  type_time m_stime;                      ///< S_TIME
  type_ti m_tion[NUMLED];                 ///< TI for LED ON for each LED
  type_ti m_tioff[NUMLED];                ///< TI for LED OFF for each LED
  type_pulse m_plslen[NUMLED];            ///< MXS pulse length for each LED
  type_pulse m_plsspc[NUMLED];            ///< MXS pulse spacing for each LED
  type_time m_timeon[NUMLED];             ///< TIME for LED ON for each LED
  type_time m_timeoff[NUMLED];            ///< TIME for LED OFF for each LED
  char m_timeon_null[NUMLED];             ///< NULL flag for TIME on
  char m_timeoff_null[NUMLED];            ///< NULL flag for TIME off
  char m_proc_status[32];                 ///< PROC_STATUS
  ahfits::IndexType m_num_proc_status;    ///< size of PROC_STATUS column
};


/// \brief Get parameter values
/// \param[out] par structure with parameter values
void getPar(Par& par);

/// \brief Copy contents of input file to output file.
/// \param[out] par structure with parameter values
/// \param[out] fptr FITS file pointer to output (cloned) file
/// \param[out] gpc array of FITS file pointers for coarse GTI
/// \param[out] gpf array of FITS file pointer for fine GTI
/// \param[out] gpc_extnames names of extensions used in coarse GTI file
/// \param[out] gpf_extnames names of extensions used in fine GTI file
/// \param[out] tioncols array of TI-on column names
/// \param[out] tioffcols array of TI-off column names
/// \param[out] plslencols array of pulse length column names
/// \param[out] plsspccols array of pulse spacing column names
/// \param[out] timeoncols array of TIME-on column names
/// \param[out] timeoffcols array of TIME-off column names
/// \param[out] timdat look up tables between L32TI and TIME from TIM file
/// \param[out] delaytime     instrument delay
/// \param[out] leapsecdat    leap second data
void initialize(Par& par, ahfits::FilePtr & fptr, ahfits::FilePtr gpc[], 
                ahfits::FilePtr gpf[], std::string gpc_extnames[],
                std::string gpf_extnames[], std::string tioncols[], 
                std::string tioffcols[], std::string plslencols[], 
                std::string plsspccols[], std::string timeoncols[], 
                std::string timeoffcols[], ahmission::timfile::TimFileData& timdat,
                double& delaytime, ahtime::leapsec::LeapSecTable& leapsecdat);

/// \brief Calculate TIME column in input file and output results
/// \param[out] par structure with parameter values
/// \param[in] fptr FITS file pointer to input file
/// \param[in] gpc array of FITS file pointers for coarse GTI
/// \param[in] gpf array of FITS file pointer for fine GTI
/// \param[in] gpc_extnames names of extensions used in coarse GTI file
/// \param[in] gpf_extnames names of extensions used in fine GTI file
/// \param[in] tioncols array of TI-on column names
/// \param[in] tioffcols array of TI-off column names
/// \param[in] plslencols array of pulse length column names
/// \param[in] plsspccols array of pulse spacing column names
/// \param[in] timeoncols array of TIME-on column names
/// \param[in] timeoffcols array of TIME-off column names
/// \param[out] timdat look up tables between L32TI and TIME from TIM file
/// \param[in] delaytime instrument delay
/// \param[in] leapsecdat leap second data
/// \param[out] clean if yes, delete output files
void doWork(Par& par, ahfits::FilePtr & fptr, ahfits::FilePtr gpc[],
            ahfits::FilePtr gpf[], std::string gpc_extnames[],
            std::string gpf_extnames[],
            std::string tioncols[], std::string tioffcols[],
            std::string plslencols[], std::string plsspccols[],
            std::string timeoncols[], std::string timeoffcols[], 
            ahmission::timfile::TimFileData& timdat, double delaytime,
            ahtime::leapsec::LeapSecTable& leapsecdat,bool& clean);

/// \brief close open FITS files
/// \param[out] par structure with parameter values
/// \param[in] fptr FITS file pointer to input file
/// \param[in] gpc FITS file pointer to coarse GTI output
/// \param[in] gpf FITS file pointer to fine GTI output
/// \param[in] clean if yes, delete output files
void finalize(Par& par, ahfits::FilePtr fptr, ahfits::FilePtr gpc[],
              ahfits::FilePtr gpf[], bool clean);

/// \brief Check that column name template has correct format
///  (single # character present)
/// \param[in] colname column name template
/// \return true if column name template okay
bool checkColName(const std::string & colname);

/// \brief Form column name from template and LED number
/// \param[in] colname column name template
/// \param[in] idx LED index (1-4)
/// \return column name
std::string formColName(const std::string & colname,int idx);

/// \brief create empty GTI file with four extensions
/// \param[in] filename name of new file
/// \param[in,out] gp ahfits FilePtr for new file
/// \param[in] array of extension names
/// \param[in] fp ahfits FilePtr with keywords to copy into new file
/// \param[in] type type of file: COARSE or FINE
void create_gti_file(const std::string& filename, ahfits::FilePtr* gp,
                     const std::string* extnames, ahfits::FilePtr& fp,
                     const std::string& type);

/// \brief create single HDU in GTI file
/// \param[in,out] gp ahfits FilePtr for new file
/// \param[in] name of extension to add
/// \param[in] fp ahfits FilePtr with keywords to copy into new HDU
/// \param[in] type type of file: COARSE or FINE
void create_gti_hdu(ahfits::FilePtr& gp, const std::string& extname,
                    ahfits::FilePtr& fp, const std::string& type);


// ****************************************************************************

/// \brief ahtime tool
///
/// Long description
int main(int argc, char** argv) {

  Par par;                         // structure with parameter values

  std::string tioncols[NUMLED];    // column names for TI-on column
  std::string tioffcols[NUMLED];   // column names for TI-off column
  std::string plslencols[NUMLED];  // column names for pulse length column
  std::string plsspccols[NUMLED];  // column names for pulse spacing column
  std::string timeoncols[NUMLED];  // column names for TIME-on column
  std::string timeoffcols[NUMLED]; // column names for TIME-off column
  bool clean=true;                 // delete output files (upon error)

  ahfits::FilePtr fptr=0;          // FITS file pointer to output file

  ahmission::timfile::TimFileData timdat;      // look up tables from TIM file

  // FITS file pointers for GTI files
  ahfits::FilePtr gpc[NUMLED]={0};   // coarse GTI
  ahfits::FilePtr gpf[NUMLED]={0};   // fine GTI
  std::string gpc_extnames[NUMLED];  // extension names in coarse GTI file
  std::string gpf_extnames[NUMLED];  // extension names in fine GTI file

  // leap second data from CALDB file
  ahtime::leapsec::LeapSecTable leapsecdat;

  // instrument delay
  double delaytime=0.;

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      // Write list of parameters to log file
      ahapp::writeParametersToLog();
      initialize(par, fptr, gpc, gpf, gpc_extnames, gpf_extnames,
                 tioncols, tioffcols, plslencols, plsspccols, timeoncols,
                 timeoffcols, timdat, delaytime, leapsecdat);

      doWork(par, fptr, gpc, gpf, gpc_extnames, gpf_extnames, tioncols, 
             tioffcols, plslencols, plsspccols, timeoncols, timeoffcols, 
             timdat, delaytime,leapsecdat,clean);
      finalize(par,fptr,gpc,gpf,clean);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par, fptr, gpc, gpf, gpc_extnames, gpf_extnames,
                   tioncols, tioffcols, plslencols, plsspccols, timeoncols,
                   timeoffcols, timdat, delaytime, leapsecdat);
        doWork(par, fptr, gpc, gpf, gpc_extnames, gpf_extnames, tioncols, 
               tioffcols, plslencols, plsspccols, timeoncols, timeoffcols, 
               timdat, delaytime,leapsecdat,clean);
      } catch (const std::exception &x) {
        // Write list of parameters to log file  
        ahapp::writeParametersToLog();
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(par,fptr,gpc,gpf,clean);
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
  par.m_outgti=ahapp::getParString("outgti");

  par.m_calctime=ahapp::getParBool("calctime");
  par.m_calcgti=ahapp::getParBool("calcgti");

  if (par.m_calctime) par.m_timfile=ahapp::getParString("timfile");
  if (par.m_calctime) par.m_delayfile=ahapp::getParString("delayfile");
  par.m_leapsecfile=ahapp::getParString("leapsecfile");

  par.m_stimecol=ahapp::getParString("stimecol");
  par.m_tioncol=ahapp::getParString("tioncol");
  par.m_tioffcol=ahapp::getParString("tioffcol");
  par.m_plslencol=ahapp::getParString("plslencol");
  par.m_plsspccol=ahapp::getParString("plsspccol");
  par.m_timeoncol=ahapp::getParString("timeoncol");
  par.m_timeoffcol=ahapp::getParString("timeoffcol");

  par.m_afterglow=ahapp::getParBool("afterglow");
  par.m_dtdecaypar=ahapp::getParString("dtdecay");
  par.m_mxsoffset=ahapp::getParDouble("mxsoffset");

  std::string tinterp=ahapp::getParString("interp");
  par.m_interp=ahmath::interpolation_type(tinterp);

  // some column names require a single '#' character to be present
  if (par.m_calctime && !checkColName(par.m_outgti))
    AH_THROW_RUNTIME("parameter, outgti, requires a single '#' character");
  if (par.m_calctime && !checkColName(par.m_tioncol))
    AH_THROW_RUNTIME("parameter, tioncol, requires a single '#' character");
  if (par.m_calctime && !checkColName(par.m_tioffcol))
    AH_THROW_RUNTIME("parameter, tioffcol, requires a single '#' character");
  if (par.m_calctime && !checkColName(par.m_plslencol))
    AH_THROW_RUNTIME("parameter, plslencol, requires a single '#' character");
  if (par.m_calctime && !checkColName(par.m_plsspccol))
    AH_THROW_RUNTIME("parameter, plsspccol, requires a single '#' character");
  if (!checkColName(par.m_timeoncol))
    AH_THROW_RUNTIME("parameter, timeoncol, requires a single '#' character");
  if (!checkColName(par.m_timeoffcol))
    AH_THROW_RUNTIME("parameter, timeoffcol, requires a single '#' character");

  // calctime and calcgti cannot both be false
  if (!par.m_calctime && !par.m_calcgti)
    AH_THROW_RUNTIME("calctime and calcgti both false; nothing to do!");

}

// ****************************************************************************

void initialize(Par& par, ahfits::FilePtr & fptr, ahfits::FilePtr gpc[], 
                ahfits::FilePtr gpf[], std::string gpc_extnames[],
                std::string gpf_extnames[], std::string tioncols[], 
                std::string tioffcols[], std::string plslencols[], 
                std::string plsspccols[], std::string timeoncols[], 
                std::string timeoffcols[], ahmission::timfile::TimFileData& timdat,
                double& delaytime, ahtime::leapsec::LeapSecTable& leapsecdat) {

  // names of output GTI files
  std::string outgticrs;
  std::string outgtifine;

  // TSTART and TSTOP values in event file
  double tstart_in=0.;
  double tstop_in=0.;

  //Time intervals data structure
  ahsxs::sxsdt::SXSTimeIntervals dts;

  // read TIM data: TIME, L32TI, and STATUS
  // construct two lookup tables:
  //  1. between L32TI and TIME including illegal STATUS rows
  //  2. between L32TI and TIME excluding illegal STATUS rows
  // construct mapping of indices between first and second lookup tables
  if (par.m_calctime) ahmission::timfile::loadTimFile(par.m_timfile,timdat);

  // copy contents of input to output and return opened output file (fptr);
  // if editing input file in-place (allowed with last argument = true), 
  // fptr will point to the opened input file.
  if (par.m_calctime) {
    ahfits::clone(par.m_infile,par.m_outfile,&fptr,true);
  } else {
    ahfits::open(par.m_infile,"",&fptr);
  }
  if (ahfits::isPrimary(fptr)) ahfits::move(fptr,"HK_SXS_FWE");
  ahmission::checkEmptyTable(fptr,par.m_infile);
  AH_INFO(ahlog::HIGH) << "Input file, extension = " << par.m_infile << ", " << ahfits::getKeyValStr(fptr,"EXTNAME") << std::endl;
  tstart_in=ahfits::getKeyValDbl(fptr,"TSTART");
  tstop_in=ahfits::getKeyValDbl(fptr,"TSTOP");

  // check if S_TIME present in file
  if (!ahfits::haveColumn(fptr,par.m_stimecol))
    AH_THROW_RUNTIME("missing S_TIME column in infile");

  // form input/output column names and check if present in file
  for (int iled=1; iled <= NUMLED; iled++) {
    static std::string timecolformat="D";
    int iled1=iled-1;  // array index for LED [0 - NUMLED-1]

    tioncols[iled-1]=formColName(par.m_tioncol,iled);
    tioffcols[iled1]=formColName(par.m_tioffcol,iled);
    plslencols[iled1]=formColName(par.m_plslencol,iled);
    plsspccols[iled1]=formColName(par.m_plsspccol,iled);
    timeoncols[iled1]=formColName(par.m_timeoncol,iled);     // does no harm if calctime=false
    timeoffcols[iled1]=formColName(par.m_timeoffcol,iled);

    // check in input columns present
    if (!ahfits::haveColumn(fptr,tioncols[iled1]))
      AH_THROW_RUNTIME("input column, "+tioncols[iled1]+", missing from infile");
    if (!ahfits::haveColumn(fptr,tioffcols[iled1]))
      AH_THROW_RUNTIME("input column, "+tioffcols[iled1]+", missing from infile");
    if (!ahfits::haveColumn(fptr,plslencols[iled1]))
      AH_THROW_RUNTIME("input column, "+plslencols[iled1]+", missing from infile");
    if (!ahfits::haveColumn(fptr,plsspccols[iled1]))
      AH_THROW_RUNTIME("input column, "+plsspccols[iled1]+", missing from infile");

    // add output columns, if not present (and calctime=true)
    // TIME-ON/TIME-OFF pair will follow TI-ON/TI-OFF pair
    if (!ahfits::haveColumn(fptr,timeoncols[iled1])) {
      if (par.m_calctime) {
        // form column description
        std::stringstream desc;
        desc << "LED " << iled << " on time";

        ahfits::insertColAfter(fptr,timeoncols[iled1],timecolformat,tioffcols[iled1]);
        ahfits::setColumnDescription(fptr,timeoncols[iled1],desc.str());
        AH_INFO(ahlog::HIGH) << "Creating column: " << timeoncols[iled1] << std::endl;
      } else if (par.m_calcgti) {   // AND NOT calctime
        AH_THROW_RUNTIME("gti operation requires time assignment");
      }
    }
    if (!ahfits::haveColumn(fptr,timeoffcols[iled1])) {
      if (par.m_calctime) {
        // form column description
        std::stringstream desc;
        desc << "LED " << iled << " off time";

        ahfits::insertColAfter(fptr,timeoffcols[iled1],timecolformat,timeoncols[iled1]);
        ahfits::setColumnDescription(fptr,timeoffcols[iled1],desc.str());
        AH_INFO(ahlog::HIGH) << "Creating column: " << timeoffcols[iled1] << std::endl;
      } else if (par.m_calcgti) {   // AND NOT calctime
        AH_THROW_RUNTIME("gti operation requires time assignment");
      }
    }
  }

  // check that TIM time range covers infile TIME range
  if (par.m_calctime) {
    if (timdat.m_tstop < tstart_in || timdat.m_tstart > tstop_in) {
      AH_THROW_RUNTIME("TIM file does not overlap input file in time");
    }
  }

  // open/load delay file and get delay time
  // note: using new scope to reduce collision risk of temporary variables
  if (par.m_calctime) {
    std::string datetime=ahfits::getKeyValStr(fptr,"DATE-OBS");
    std::string delayname="FW";   // +++ 20150705 KLR FYI: I made this a string, to pass into resolve and loadAndGet.
    std::string tmp=ahmission::caldb::resolve(par.m_delayfile, "delay", "GEN", "-", "DELAY_FW", datetime);
    ape_trad_set_string("delayfile",tmp.c_str());   // to record actual file path in history
    
    double delaytime2=0.;       // dummy variable; do not need 2nd delay time
    ahmission::delay::loadAndGet(tmp,delayname,tstart_in,delaytime,delaytime2);
  }

  // load leap second data
  // note: using new scope to reduce collision risk of temporary variables
  {
    std::string tmp=ahmission::caldb::resolve(par.m_leapsecfile, "leap second", "INS", "-", "LEAPSECS", "-", "-", "GEN");
    ape_trad_set_string("leapsecfile",tmp.c_str());   // to record actual file path in history
    ahtime::leapsec::load(tmp,leapsecdat);
  }

  // get afterglow time from parameter or CALDB file
  if (par.m_calcgti && par.m_afterglow) {
    if (ahgen::isNumber(par.m_dtdecaypar)) {
      par.m_dtdecay=atof(par.m_dtdecaypar.c_str());
    } else {
      AH_INFO(ahlog::HIGH) << "Loading delta-time FITS file" << std::endl;
  
      //resolve dtfile name and query CALDB if neccessary
      std::string datetime=ahfits::getKeyValStr(fptr,"DATE-OBS");
      std::string dtfile = ahmission::caldb::resolve(par.m_dtdecaypar, "delta-t", "SXS", "-", "DELTIMES", datetime);
        
      //load time intervals file
      ahsxs::sxsdt::loadSXSTimeIntervals(dtfile,tstart_in,dts);
      
      par.m_dtdecay=dts.m_mxsdt;
      
      // Set paramter value in current .par file
      std::stringstream ss;
      ss << par.m_dtdecay;
      std::string tmp = ss.str(); 
      ape_trad_set_string("dtdecay", tmp.c_str()); 
    }
  }

  if (par.m_calcgti) {

    // form GTI file names from template
    size_t start_pos=par.m_outgti.find('#');
    outgticrs=par.m_outgti;
    outgticrs.replace(start_pos,1,"cs");
    outgtifine=par.m_outgti;
    outgtifine.replace(start_pos,1,"fn");

    // create and open gti files with outgticrs and outgtifine parameters
    gpc_extnames[0]="GTIMXSCSON1";
    gpc_extnames[1]="GTIMXSCSON2";
    gpc_extnames[2]="GTIMXSCSON3";
    gpc_extnames[3]="GTIMXSCSON4";
    gpf_extnames[0]="GTIMXSFNON1";
    gpf_extnames[1]="GTIMXSFNON2";
    gpf_extnames[2]="GTIMXSFNON3";
    gpf_extnames[3]="GTIMXSFNON4";
    create_gti_file(outgticrs,gpc,gpc_extnames,fptr,"COARSE");
    create_gti_file(outgtifine,gpf,gpf_extnames,fptr,"FINE");

  }

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 
}

// ****************************************************************************

void doWork(Par& par, ahfits::FilePtr & fptr, ahfits::FilePtr gpc[],
            ahfits::FilePtr gpf[], std::string gpc_extnames[],
            std::string gpf_extnames[],
            std::string tioncols[], std::string tioffcols[],
            std::string plslencols[], std::string plsspccols[],
            std::string timeoncols[], std::string timeoffcols[], 
            ahmission::timfile::TimFileData& timdat, double delaytime,
            ahtime::leapsec::LeapSecTable& leapsecdat, bool& clean) {

  // get epoch from input file (needed to calculate DATE-OBS/END for GTI files)
  int mjdrefi=(int)ahfits::getKeyValLLong(fptr,"MJDREFI");
  double mjdreff=ahfits::getKeyValDbl(fptr,"MJDREFF");
  ahtime::AhMJDTime ttepoch_mjd(mjdrefi,mjdreff);       // epoch modified Julian Day
  ahtime::AhDateTime ttepoch;                           // epoch as terrestrial time
  ahtime::reformatMJDAsDateTime(ttepoch_mjd,ttepoch);
  ahtime::AhDateTime epoch;                             // epoch as UTC time
  ahtime::convertTTToUTC(ttepoch,leapsecdat,epoch);

  // get TSTART from input file which is used as the initial TIME if a GTI
  // begins before the start of the file
  double tstart=ahfits::getKeyValDbl(fptr,"TSTART");
  double tstop=ahfits::getKeyValDbl(fptr,"TSTOP");

  // largest value of L32TI calculated from 38-bit TI value
  type_ti l32timask=0xFFFFFFFF;
  type_ti l32timax=l32timask;

  // keep track of number of LEDs which are turned on, 0=none
  // ideally, only 1 LED is active at a single time
  // note: this is checked in the GTI calculation (calcgti = yes)
  int ledon=0;

  // index of TIM lookup table (original TIM data set including invalid rows)
  unsigned long timidx=0;

  // Need to store data for current and previous rows.
  RowInfo rowdat;
  RowInfo rowdat_prev;

  // GTI variables for coarse GTI
  double start_gpc[NUMLED]={0.,0.,0.,0.};    // START column value for each LED
  double stop_gpc[NUMLED]={0.,0.,0.,0.};     // STOP column value for each LED
  double plslen_gpc[NUMLED]={0.,0.,0.,0.};   // PLSLEN value for each LED 
  double plsspc_gpc[NUMLED]={0.,0.,0.,0.};   // PLSSPC value for each LED
  double plslen_key[NUMLED]={0.,0.,0.,0.};   // PLSLEN keyword value for each LED
  double plsspc_key[NUMLED]={0.,0.,0.,0.};   // PLSSPC keyword value for each LED
  ahfits::Router* rout_gpc[NUMLED];          // one router per extension
  for (int iled1=0; iled1 < NUMLED; iled1++) rout_gpc[iled1]=0;

  // GTI variables for fine GTI
  double start_gpf[NUMLED]={0.,0.,0.,0.};  // START column value for each LED
  double stop_gpf[NUMLED]={0.,0.,0.,0.};   // STOP column value for each LED
  ahfits::Router* rout_gpf[NUMLED];        // one router per extension
  for (int iled1=0; iled1 < NUMLED; iled1++) rout_gpf[iled1]=0;

  // variables to keep track of LED/GTI state
  bool first_gti[NUMLED];            // true means no GTI detected yet for LED
  type_time gti_start[NUMLED];       // < 0 means no start detected
  type_time tstart_gti[NUMLED];      // < 0 means TSTART not determined yet
  type_time tstop_gti[NUMLED];       // < 0 means TSTOP not determined yet
  type_pulse gti_plslen[NUMLED];     // GTI pulse length associated with GTI start time
  type_pulse gti_plsspc[NUMLED];     // GTI pulse spacing associated with GTI start time
  for (int iled1=0; iled1 < NUMLED; iled1++) {
    first_gti[iled1]=true;
    gti_start[iled1]=-1;
    tstart_gti[iled1]=-1.;
    tstop_gti[iled1]=-1.;
    gti_plslen[iled1]=rowdat.m_plslen[iled1];
    gti_plsspc[iled1]=rowdat.m_plsspc[iled1];
  }

  // make connections from input file to local variables
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_READONLY,par.m_stimecol,rowdat.m_stime);
  router.connectBit(ahfits::e_READONLY,"PROC_STATUS",rowdat.m_proc_status,rowdat.m_num_proc_status);
  for (int iled1=0; iled1 < NUMLED; iled1++) {
    router.connectScalar(ahfits::e_READONLY,tioncols[iled1],rowdat.m_tion[iled1]);
    router.connectScalar(ahfits::e_READONLY,tioffcols[iled1],rowdat.m_tioff[iled1]);
    router.connectScalar(ahfits::e_READONLY,plslencols[iled1],rowdat.m_plslen[iled1]);
    router.connectScalar(ahfits::e_READONLY,plsspccols[iled1],rowdat.m_plsspc[iled1]);
    if (par.m_calctime) {
      router.connectScalar(ahfits::e_READWRITE,timeoncols[iled1],rowdat.m_timeon[iled1],&rowdat.m_timeon_null[iled1]);
      router.connectScalar(ahfits::e_READWRITE,timeoffcols[iled1],rowdat.m_timeoff[iled1],&rowdat.m_timeoff_null[iled1]);
    } else {
      router.connectScalar(ahfits::e_READONLY,timeoncols[iled1],rowdat.m_timeon[iled1],&rowdat.m_timeon_null[iled1]);
      router.connectScalar(ahfits::e_READONLY,timeoffcols[iled1],rowdat.m_timeoff[iled1],&rowdat.m_timeoff_null[iled1]);
    }
  }

  // read first row from input/output file to initialize "unchanged" column 
  // values; set TIME = TSTART for first row; store 1st row into PREV.  Note:
  // we do not need to add delaytime to TSTART since it is already accounted 
  // for by ahtime.
  ahfits::readRow(fptr);
  for (int iled1=0; iled1 < NUMLED; iled1++) {
    rowdat.m_timeon[iled1]=tstart;
    rowdat.m_timeoff[iled1]=tstart;

    rowdat_prev.m_tion[iled1]=rowdat.m_tion[iled1];
    rowdat_prev.m_tioff[iled1]=rowdat.m_tioff[iled1];
    rowdat_prev.m_timeon[iled1]=rowdat.m_timeon[iled1];
    rowdat_prev.m_timeoff[iled1]=rowdat.m_timeoff[iled1];
  }

  // if calcgti and not calctime, check that first TIME column is 
  //  non-zero implying that time assignment has already been
  //  calculated (assuming that TI != 0 or 2014-01-01 00:00:00)
  if (par.m_calcgti && !par.m_calctime) {
    if (rowdat.m_timeon[0] == 0.) 
      AH_THROW_RUNTIME("gti operation requires time assignment");
  }

  // make connections between coarse GTI file and local variables
  if (par.m_calcgti) {
    for (int iled1=0; iled1 < NUMLED; iled1++) {
      rout_gpc[iled1]=new ahfits::Router(gpc[iled1]);
      rout_gpc[iled1]->connectScalar(ahfits::e_WRITEONLY,"START",start_gpc[iled1]);
      rout_gpc[iled1]->connectScalar(ahfits::e_WRITEONLY,"STOP",stop_gpc[iled1]);
      rout_gpc[iled1]->connectScalar(ahfits::e_WRITEONLY,"PLSLEN",plslen_gpc[iled1]);
      rout_gpc[iled1]->connectScalar(ahfits::e_WRITEONLY,"PLSSPC",plsspc_gpc[iled1]);
    }
  }

  // make connections between fine GTI file and local variables
  if (par.m_calcgti) {
    for (int iled1=0; iled1 < NUMLED; iled1++) {
      rout_gpf[iled1]=new ahfits::Router(gpf[iled1]);
      rout_gpf[iled1]->connectScalar(ahfits::e_WRITEONLY,"START",start_gpf[iled1]);
      rout_gpf[iled1]->connectScalar(ahfits::e_WRITEONLY,"STOP",stop_gpf[iled1]);
    }
  }

  // set up counters
  long long num_ledstart[NUMLED];     // number of times LED start is triggered
  long long num_ledend[NUMLED];       // number of times LED stop is triggered
  long long num_startdouble[NUMLED];  // number of times two LED starts occur before a stop
  long long num_stopdouble[NUMLED];   // number of times two LED stops occur before a start
  bool ledonoff[NUMLED];              // set to True if LED is active at any time
  long long num_gticoarse[NUMLED];    // number of coarse GTIs written
  long long num_gtifine[NUMLED];      // number of fine GTIs written
  long long num_rowbad=0;             // number of rows with any bad time assignment
  for (int iled1=0; iled1 < NUMLED; iled1++) {
    num_ledstart[iled1]=0;
    num_ledend[iled1]=0;
    num_startdouble[iled1]=0;
    num_stopdouble[iled1]=0;
    ledonoff[iled1]=false;
    num_gticoarse[iled1]=0;
    num_gtifine[iled1]=0;
  }

  // loop over rows (note first row is read again, but it is okay)
  clean=false;
  long long irow=0;
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    irow++;

    // reset NULL flags
    for (int iled1=0; iled1 < NUMLED; iled1++) {
      rowdat.m_timeon_null[iled1]=0;
      rowdat.m_timeoff_null[iled1]=0;
    }

    // If the last row in the file has a bad PROC_STATUS or a NULL TIME, then
    // we need to use the last, good row in the file for resolving any open
    // MXS intervals in the GTI processing.  


    // Flag that last row in file has a NULL TIME
    bool lastrownull=false;

    // Rows with a bad PROC_STATUS are assigned a NULL time and not considered
    // for GTIs.  If the last row has a bad PROC_STATUS, then we need to 
    // continue to the GTI section to see if there is an open MXS interval
    // so that the correct GTIs can be written.
    if (!procstatus::processRow(rowdat.m_proc_status)) {
      if (par.m_calctime) {
        for (int iled1=0; iled1 < NUMLED; iled1++) {
          rowdat.m_timeon_null[iled1]=1;
          rowdat.m_timeoff_null[iled1]=1;
        }
        num_rowbad++;
        ahfits::writeRow(fptr);
      }
      if (!ahfits::atLastRow(fptr)) continue;
      lastrownull=true;
    }

    // A pre-launch data set (thermal vac) had a junk row which caused
    // problems in the GTI calculations.  The row could be identified by
    // having a TI value.  This is a mistake in the data set and should not
    // show up after launch, but we are checking for it for safety.
    bool skipme=false;
    for (int iled1=0; iled1 < NUMLED; iled1++) {
      if (rowdat.m_tion[iled1] < 0) skipme=true;
      if (rowdat.m_tioff[iled1] < 0) skipme=true;
    }
    if (skipme) {
      AH_INFO(ahlog::HIGH) << "Row " << irow << ": skipping row with negative TI on/off" << std::endl;
      if (par.m_calctime) {
        for (int iled1=0; iled1 < NUMLED; iled1++) {
          rowdat.m_timeon_null[iled1]=1;
          rowdat.m_timeoff_null[iled1]=1;
        }
        num_rowbad++;
        ahfits::writeRow(fptr);
      }
      if (!ahfits::atLastRow(fptr)) continue;
      lastrownull=true;
    }

    if (par.m_calctime && !lastrownull) {

      bool timegood=true;   // this will be set to false, if any time assignment fails 
      for (int iled1=0; iled1 < NUMLED; iled1++) {

        // if TI_LED_ON is unchanged, use PREV TIME, else calculate new TIME
        if (rowdat.m_tion[iled1] == rowdat_prev.m_tion[iled1]) {
          rowdat.m_timeon[iled1]=rowdat_prev.m_timeon[iled1];
          rowdat.m_timeon_null[iled1]=rowdat_prev.m_timeon_null[iled1];
        } else {
          // get L32TI from 38-bit TI
          long long l32tion = (rowdat.m_tion[iled1] & l32timask);

          // assign TIME with TIM file
          bool extrap=false;     // used to check if routine will extrapolate
          timidx=ahmath::search_sawY2X(rowdat.m_stime,l32tion,l32timax,timdat.m_time1,
                 timdat.m_l32ti1,timdat.m_size1,0.5,extrap,timidx);
          if (extrap) {
            long irow=ahfits::currentRow(fptr);
            AH_INFO(ahlog::HIGH) << " *** Extrapolating to get TI_LED" << (iled1+1) 
                                 << "_ON for row " << irow << std::endl;
          }

          if (ahmission::timfile::isTimStatusIllegal(timdat.m_status1[timidx]) || 
              ahmission::timfile::isTimStatusIllegal(timdat.m_status1[timidx+1])) {
            rowdat.m_timeon_null[iled1]=1;    // flag TIME as NULL
            timegood=false;
            AH_INFO(ahlog::LOW) << " row " << irow << ", led " << (iled1+1) << ": adjacent to bad TIM row; setting START TIME=NULL" << std::endl;
          } else {

            // get index in valid-only TIM lookup table and interpolate
            long timidxp=timdat.m_map[timidx];
            rowdat.m_timeon[iled1]=ahmath::interpolate_sawY2X(l32tion,timdat.m_time2,
                                   timdat.m_l32ti2,timdat.m_size2,par.m_interp,timidxp);

            // instrument delay
            rowdat.m_timeon[iled1]+=delaytime;
          }
        }

        // if TI_LED_OFF is unchanged, use PREV TIME, else calculate new TIME
        if (rowdat.m_tioff[iled1] == rowdat_prev.m_tioff[iled1]) {
          rowdat.m_timeoff[iled1]=rowdat_prev.m_timeoff[iled1];
          rowdat.m_timeoff_null[iled1]=rowdat_prev.m_timeoff_null[iled1];
        } else {
          // get L32TI from 38-bit TI
          long long l32tioff = (rowdat.m_tioff[iled1] & l32timask);

          // assign TIME with TIM file
          bool extrap=false;     // used to check if routine will extrapolate
          timidx=ahmath::search_sawY2X(rowdat.m_stime,l32tioff,l32timax,timdat.m_time1,
                 timdat.m_l32ti1,timdat.m_size1,0.5,extrap,timidx);
          if (extrap) {
            long irow=ahfits::currentRow(fptr);
            AH_INFO(ahlog::HIGH) << " *** Extrapolating to get TI_LED" << (iled1+1) 
                                 << "_OFF for row " << irow << std::endl;
          }

          if (ahmission::timfile::isTimStatusIllegal(timdat.m_status1[timidx]) || 
              ahmission::timfile::isTimStatusIllegal(timdat.m_status1[timidx+1])) {
            rowdat.m_timeoff_null[iled1]=1;    // flag TIME as NULL
            timegood=false;
            AH_INFO(ahlog::LOW) << " row " << irow << ", led " << (iled1+1) << ": adjacent to bad TIM row; setting STOP TIME=NULL" << std::endl;
          } else {

            // get index in valid-only TIM lookup table and interpolate
            long timidxp=timdat.m_map[timidx];
            rowdat.m_timeoff[iled1]=ahmath::interpolate_sawY2X(l32tioff,timdat.m_time2,
                                    timdat.m_l32ti2,timdat.m_size2,par.m_interp,timidxp);

            // instrument delay
            rowdat.m_timeoff[iled1]+=delaytime;
          }
        }

      }    // end for loop over LED
      ahfits::writeRow(fptr);

      if (!timegood) num_rowbad++;
    }  // end if calctime

    // GTI processing
    if (par.m_calcgti) {

      // check if any times have changed
      clean=false;
      for (int iled1=0; iled1 < NUMLED; iled1++) {

        // skip row if TIMEON or TIMEOFF is NULL, but not at the last row
        if (!ahfits::atLastRow(fptr)) {
          if (rowdat.m_timeon_null[iled1] == 1 || rowdat.m_timeoff_null[iled1] == 1) {
            continue;
          }
        }

        // check if GTI interval has ended and write coarse and fine GTI
        // or at last row of input file
        if (rowdat.m_tioff[iled1] != rowdat_prev.m_tioff[iled1] || ahfits::atLastRow(fptr)) {
  
          if (gti_start[iled1] > 0.) {       // GTI start is defined
            if (!ahfits::atLastRow(fptr)) num_ledend[iled1]++;
  
            // if at last row, then set TIMEOFF = TSTOP (just for GTI 
            // calculation, the correct TIME is written to the output file)
            if (ahfits::atLastRow(fptr)) rowdat.m_timeoff[iled1]=tstop;

            // write coarse interval and update TSTOP
            start_gpc[iled1]=gti_start[iled1]+par.m_mxsoffset;
            stop_gpc[iled1]=rowdat.m_timeoff[iled1]+par.m_mxsoffset;
            plslen_gpc[iled1]=UNITPLSLEN*gti_plslen[iled1];
            plsspc_gpc[iled1]=UNITPLSSPC*gti_plsspc[iled1];
            ahfits::writeRow(gpc[iled1]);
            ahfits::nextRow(gpc[iled1]);

            // update PLSLEN/PLSSPC keyword values
            if (0. == plslen_key[iled1]) {
              plslen_key[iled1]=plslen_gpc[iled1];
            } else if (plslen_key[iled1] != plslen_gpc[iled1]) {
              if (plslen_key[iled1] > 0.) plslen_key[iled1]=-plslen_key[iled1];
            }
            if (0. == plsspc_key[iled1]) {
              plsspc_key[iled1]=plsspc_gpc[iled1];
            } else if (plsspc_key[iled1] != plsspc_gpc[iled1]) {
              if (plsspc_key[iled1] > 0.) plsspc_key[iled1]=-plsspc_key[iled1];
            }

            // update TSTART/TSTOP
            if (tstart_gti[iled1] < 0. || (start_gpc[iled1] < tstart_gti[iled1])) {
              tstart_gti[iled1]=start_gpc[iled1];
            }
            if (stop_gpc[iled1] > tstop_gti[iled1]) {
              tstop_gti[iled1]=stop_gpc[iled1];
            }

            // compute/write all fine GTI in coarse GTI interval; if the
            // fine GTI overlap due to the afterglow, then use the coarse
            // GTI instead (with the afterglow added to the STOP time)
            start_gpf[iled1]=start_gpc[iled1];
            long long counter=0;
            if (par.m_afterglow && (par.m_dtdecay > UNITPLSSPC * gti_plsspc[iled1])) {
              // afterglow larger than pulse spacing, so write a single GTI
              AH_INFO(ahlog::LOW) << "fine GTI overlap; writing single GTI instead" << std::endl;
              stop_gpf[iled1]=stop_gpc[iled1]+par.m_dtdecay;
              counter++;
              ahfits::writeRow(gpf[iled1]);
              ahfits::nextRow(gpf[iled1]);
            } else {
              while (start_gpf[iled1] < rowdat.m_timeoff[iled1]) {
                counter++;
                stop_gpf[iled1]=start_gpf[iled1]+ (UNITPLSLEN * gti_plslen[iled1]);
                if (par.m_afterglow) stop_gpf[iled1]+=par.m_dtdecay;
                ahfits::writeRow(gpf[iled1]);
                ahfits::nextRow(gpf[iled1]);
                start_gpf[iled1]+=(UNITPLSSPC * gti_plsspc[iled1]);
              }
            }

            // reset GTI state
            first_gti[iled1]=false;
            gti_start[iled1]=-1.;
            ledon--;    // reduce active LED count
  
            // update counters
            ledonoff[iled1]=true;
            num_gticoarse[iled1]++;
            num_gtifine[iled1]+=counter;
  
            AH_INFO(ahlog::HIGH) << "MXS interval written for LED " << (iled1+1) << std::endl;
            AH_INFO(ahlog::HIGH) << "  START, STOP = " << start_gpc[iled1] << ", " << stop_gpc[iled1] << std::endl;
            AH_INFO(ahlog::HIGH) << "  Pulse length, spacing (s) = " << (UNITPLSLEN * gti_plslen[iled1]) << ", " << (UNITPLSSPC * gti_plsspc[iled1]) << std::endl;
            AH_INFO(ahlog::HIGH) << "  Number of fine GTI = " << counter << std::endl;
  
          } else if (first_gti[iled1] == true && !ahfits::atLastRow(fptr)) {   // first GTI starts before TSTART of input file
            num_ledend[iled1]++;

            // set coarse interval where starting time is a dummy value
            start_gpc[iled1]=rowdat.m_timeon[iled1]+par.m_mxsoffset;    // dummy value, will be replaced with the first fine GTI start
            stop_gpc[iled1]=rowdat.m_timeoff[iled1]+par.m_mxsoffset;    // this value will be the reference point for the fine GTI

            // Estimate number of fine GTI and prepare array to store fine GTI
            // in memory before writing to file (since these GTI are computed
            // in reverse time order).
            long ngti=100+(rowdat.m_timeoff[iled1]-rowdat.m_timeon[iled1])/rowdat.m_plslen[iled1];
            std::vector<double> startvec;
            std::vector<double> stopvec;
            startvec.reserve(ngti);
            stopvec.reserve(ngti);

            // compute fine GTI intervals backwards from TIME_LEDi_OFF
            // Note: we are using rowdat.m_plslen/spc instead of gti_plslen/spc
            // since the former will not have been set yet due to the fact that
            // the no LED-on TIME exists for this LED.
            start_gpf[iled1]=stop_gpc[iled1]-(UNITPLSSPC * rowdat.m_plsspc[iled1]);
            stop_gpf[iled1]=start_gpf[iled1]+(UNITPLSLEN * rowdat.m_plslen[iled1]);
            if (par.m_afterglow) stop_gpf[iled1]+=par.m_dtdecay;
            startvec.push_back(start_gpf[iled1]);
            stopvec.push_back(stop_gpf[iled1]);
            long long counter=0;
            while (start_gpf[iled1] >= rowdat.m_timeon[iled1]) {
              counter++;
              start_gpf[iled1]-=(UNITPLSSPC * rowdat.m_plsspc[iled1]);
              stop_gpf[iled1]-=(UNITPLSSPC * rowdat.m_plsspc[iled1]);
              startvec.push_back(start_gpf[iled1]);
              stopvec.push_back(stop_gpf[iled1]);
            }

            // Write fine GTI to file (have to iterate vectors in reverse so
            // that the times are written in the correct order.  If the fine
            // GTI overlap due to a large afterglow time, then write a single
            // fine GTI covering the whole time span.
            ngti=startvec.size();
            if (par.m_afterglow && (par.m_dtdecay > UNITPLSSPC * rowdat.m_plsspc[iled1])) {
              AH_INFO(ahlog::LOW) << "fine GTI overlap; writing single GTI instead" << std::endl;
              counter=1;                 // have to reset number of fine GTI 
              start_gpf[iled1]=startvec[ngti-1];
              stop_gpf[iled1]=stopvec[0];
              ahfits::writeRow(gpf[iled1]);
              ahfits::nextRow(gpf[iled1]);
            } else {
              for (long igti=ngti-1; igti >= 0; igti--) {
                start_gpf[iled1]=startvec[igti];
                stop_gpf[iled1]=stopvec[igti];
                ahfits::writeRow(gpf[iled1]);
                ahfits::nextRow(gpf[iled1]);
              }
            }

            // Set coarse GTI start to first fine GTI start (which is the
            // last element in the start_gpf array).  Then write coarse GTI
            // interval to file.
            start_gpc[iled1]=startvec[ngti-1];
            plslen_gpc[iled1]=UNITPLSLEN*rowdat.m_plslen[iled1];
            plsspc_gpc[iled1]=UNITPLSSPC*rowdat.m_plsspc[iled1];
            ahfits::writeRow(gpc[iled1]);
            ahfits::nextRow(gpc[iled1]);

            // update PLSLEN/PLSSPC keyword values
            if (0. == plslen_key[iled1]) {
              plslen_key[iled1]=plslen_gpc[iled1];
            } else if (plslen_key[iled1] != plslen_gpc[iled1]) {
              if (plslen_key[iled1] > 0.) plslen_key[iled1]=-plslen_key[iled1];
            }
            if (0. == plsspc_key[iled1]) {
              plsspc_key[iled1]=plsspc_gpc[iled1];
            } else if (plsspc_key[iled1] != plsspc_gpc[iled1]) {
              if (plsspc_key[iled1] > 0.) plsspc_key[iled1]=-plsspc_key[iled1];
            }

            // update TSTART/TSTOP
            if (tstart_gti[iled1] < 0. || (start_gpc[iled1] < tstart_gti[iled1])) {
              tstart_gti[iled1]=start_gpc[iled1];
            }
            if (stop_gpc[iled1] > tstop_gti[iled1]) {
              tstop_gti[iled1]=stop_gpc[iled1];
            }

            // reset GTI state
            first_gti[iled1]=false;
            gti_start[iled1]=-1.;
            ledon--;    // reduce active LED count
  
            // update counters
            ledonoff[iled1]=true;
            num_gticoarse[iled1]++;
            num_gtifine[iled1]+=counter;
  
            AH_INFO(ahlog::HIGH) << "MXS interval written for LED " << (iled1+1) << std::endl;
            AH_INFO(ahlog::HIGH) << "  interval starts before beginning of file" << std::endl;
            AH_INFO(ahlog::HIGH) << "  STOP = " << stop_gpc[iled1] << std::endl;
            AH_INFO(ahlog::HIGH) << "  Pulse length, spacing (s) = " << (UNITPLSLEN * rowdat.m_plslen[iled1]) << ", " << (UNITPLSSPC * rowdat.m_plsspc[iled1]) << std::endl;
            AH_INFO(ahlog::HIGH) << "  Number of fine GTI = " << counter << std::endl;
  
  
          } else {       // have two STOP times in a row... ignore
            if (!ahfits::atLastRow(fptr)) {
              num_stopdouble[iled1]++;
              AH_INFO(ahlog::HIGH) << "*** Two stop times in a row for LED " << (iled1+1)
                                   << "; ignoring 2nd STOP TIME occurring on row "
                                   << ahfits::currentRow(fptr) << std::endl;
            }
          }
        }   // end if tioff != previous tioff


        // check if GTI interval has started and store TIME
        if (rowdat.m_tion[iled1] != rowdat_prev.m_tion[iled1]) {
          num_ledstart[iled1]++;
  
          // check for two starts in a row
          if (gti_start[iled1] > 0.) {
            num_startdouble[iled1]++;
            AH_INFO(ahlog::HIGH) << "*** Two start times in a row for LED " << (iled1+1)
                                 << "; ignoring 2nd START TIME occurring on row "
                                 << ahfits::currentRow(fptr) << std::endl;
          }
  
          // store start time and pulse parameters
          gti_start[iled1]=rowdat.m_timeon[iled1];
          gti_plslen[iled1]=rowdat.m_plslen[iled1];
          gti_plsspc[iled1]=rowdat.m_plsspc[iled1];
          ledon++;    // increase active LED count
        }

      }   // end loop over LED

    }    // end if (calcgti)

    // store current values as prev
    for (int iled1=0; iled1 < NUMLED; iled1++) {
      rowdat_prev.m_tion[iled1]=rowdat.m_tion[iled1];
      rowdat_prev.m_tioff[iled1]=rowdat.m_tioff[iled1];
      rowdat_prev.m_timeon[iled1]=rowdat.m_timeon[iled1];
      rowdat_prev.m_timeon_null[iled1]=rowdat.m_timeon_null[iled1];
      rowdat_prev.m_timeoff[iled1]=rowdat.m_timeoff[iled1];
      rowdat_prev.m_timeoff_null[iled1]=rowdat.m_timeoff_null[iled1];
    }

  }   // end loop over rows

  // write counters
  AH_INFO(ahlog::HIGH) << "Counters: " << std::endl;
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "Total number of rows:          " << irow << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of rows with TIME=NULL: " << num_rowbad << std::endl;
  AH_INFO(ahlog::HIGH) << std::endl;
  if (par.m_calcgti) {
    for (int iled1=0; iled1 < NUMLED; iled1++) {
      AH_INFO(ahlog::HIGH) << "LED " << (iled1+1) << std::endl;
      if (!ledonoff[iled1]) {
        AH_INFO(ahlog::HIGH) << "  LED never active" << std::endl;
      } else {
        AH_INFO(ahlog::HIGH) << "  Number of LED starts:    " << num_ledstart[iled1] << std::endl;
        AH_INFO(ahlog::HIGH) << "  Number of LED stops:     " << num_ledend[iled1] << std::endl;
        AH_INFO(ahlog::HIGH) << "  Number of start doubles: " << num_startdouble[iled1] << std::endl;
        AH_INFO(ahlog::HIGH) << "  Number of stop doubles:  " << num_stopdouble[iled1] << std::endl;
        AH_INFO(ahlog::HIGH) << "  Number of coarse GTI:    " << num_gticoarse[iled1] << std::endl;
        AH_INFO(ahlog::HIGH) << "  Number of fine GTI:      " << num_gtifine[iled1] << std::endl;
      }
    }
  }

  // write keywords to GTI extension for each LED
  if (par.m_calcgti) {
    for (int iled1=0; iled1 < NUMLED; iled1++) {

      // get LED number in string form
      std::stringstream ssledstr;
      ssledstr << iled1+1;
      std::string ledstr=ssledstr.str();

      // tstart_gti and tstop_gti will be set to -1 if the LED# TI column never
      // changes.  However, we cannot use calcDateObs with a negative time or
      // an error will be thrown, so we will set tstart/tstop = 0 in this case.
      if (tstart_gti[iled1] < 0) tstart_gti[iled1]=0.;
      if (tstop_gti[iled1] < 0) tstop_gti[iled1]=0.;

      // compute DATE-OBS/END from TSTART/TSTOP
      std::string dateobs=ahtime::calcDateObs(tstart_gti[iled1],epoch,leapsecdat);
      std::string dateend=ahtime::calcDateObs(tstop_gti[iled1],epoch,leapsecdat);

      // coarse GTI
      ahfits::writeKeyValDbl(gpc[iled1],"TSTART",tstart_gti[iled1],"Start time");
      ahfits::writeKeyValDbl(gpc[iled1],"TSTOP",tstop_gti[iled1],"Stop time");
      ahfits::writeKeyValStr(gpc[iled1],"DATE-OBS",dateobs,"Start Date");
      ahfits::writeKeyValStr(gpc[iled1],"DATE-END",dateend,"Stop Date");
      if (ledonoff[iled1]) {
        ahfits::writeKeyValStr(gpc[iled1],"DATAMODE","MXSON"+ledstr,"Data acquisition mode");
        ahfits::writeKeyValStr(gpc[iled1],"MXSONOFF","ON","If MXS is ON OFF or UNKNOWN");
        ahfits::writeKeyValStr(gpc[iled1],"MXSTYPE",ledstr,"Which MXS is or else NONE");
      } else {
        ahfits::writeKeyValStr(gpc[iled1],"DATAMODE","MXSOFF","Data acquisition mode");
        ahfits::writeKeyValStr(gpc[iled1],"MXSONOFF","OFF","If MXS is ON OFF or UNKNOWN");
        ahfits::writeKeyValStr(gpc[iled1],"MXSTYPE","NONE","Which MXS is or else NONE");
      }
      ahfits::writeKeyValDbl(gpc[iled1],"LED"+ledstr+"LEN",plslen_key[iled1],"Pulse length [s]");
      ahfits::writeKeyValDbl(gpc[iled1],"LED"+ledstr+"SPC",plsspc_key[iled1],"Pulse spacing [s]");

      // fine GTI
      ahfits::writeKeyValDbl(gpf[iled1],"TSTART",tstart_gti[iled1],"Start time");
      ahfits::writeKeyValDbl(gpf[iled1],"TSTOP",tstop_gti[iled1],"Stop time");
      ahfits::writeKeyValStr(gpf[iled1],"DATE-OBS",dateobs,"Start Date");
      ahfits::writeKeyValStr(gpf[iled1],"DATE-END",dateend,"Stop Date");
      if (ledonoff[iled1]) {
        ahfits::writeKeyValStr(gpf[iled1],"DATAMODE","MXSON"+ledstr,"Data acquisition mode");
        ahfits::writeKeyValStr(gpf[iled1],"MXSONOFF","ON","If MXS is ON OFF or UNKNOWN");
        ahfits::writeKeyValStr(gpf[iled1],"MXSTYPE",ledstr,"Which MXS is or else NONE");
      } else {
        ahfits::writeKeyValStr(gpf[iled1],"DATAMODE","MXSOFF","Data acquisition mode");
        ahfits::writeKeyValStr(gpf[iled1],"MXSONOFF","OFF","If MXS is ON OFF or UNKNOWN");
        ahfits::writeKeyValStr(gpf[iled1],"MXSTYPE","NONE","Which MXS is or else NONE");
      }
      if (par.m_afterglow) {
        ahfits::writeKeyValDbl(gpf[iled1],"DTDECAY",par.m_dtdecay,"MXS afterglow time");
      }
    }
  }

  // free allocated Routers
  if (par.m_calcgti) {
    for (int iled1=0; iled1 < NUMLED; iled1++) {
      if (rout_gpc[iled1] != 0) delete rout_gpc[iled1], rout_gpc[iled1]=0;
      if (rout_gpf[iled1] != 0) delete rout_gpf[iled1], rout_gpf[iled1]=0;
    }
  }

}

// ****************************************************************************

void finalize(Par& par, ahfits::FilePtr fptr, ahfits::FilePtr gpc[],
              ahfits::FilePtr gpf[], bool clean) {

  // file names for deleting files upon error
  std::string infile, gpcfile, gpffile;
  if (0 != fptr) infile=fptr->m_filename;
  if (0 != gpc[0]) gpcfile=gpc[0]->m_filename;
  if (0 != gpf[0]) gpffile=gpf[0]->m_filename;

  // close all open files/extensions
  ahfits::close(fptr);
  if (par.m_calcgti) {
    for (int iled1=0; iled1 < NUMLED; iled1++) {
      ahfits::close(gpc[iled1]);
      ahfits::close(gpf[iled1]);
    }
  }

  // delete files if there was an early error
  if (clean) {
    if (par.m_calctime && "" != infile && ahgen::fileExists(infile)) std::remove(infile.c_str());
    if ("" != gpcfile && ahgen::fileExists(gpcfile)) std::remove(gpcfile.c_str());
    if ("" != gpffile && ahgen::fileExists(gpffile)) std::remove(gpffile.c_str());
  }

}

// ****************************************************************************

bool checkColName(const std::string & colname) {

  size_t nhash=std::count(colname.begin(), colname.end(), '#');
  if (nhash != 1) return false;
  return true;
}

// ****************************************************************************

std::string formColName(const std::string & colname,int idx) {

  // copy input column name and get location of replacement character
  std::string out=colname;
  size_t loc=out.find('#');

  // turn valid integer index into replacement character
  std::string ch;
  if (idx == 1)
    ch="1";
  else if (idx == 2)
    ch="2";
  else if (idx == 3)
    ch="3";
  else if (idx == 4)
    ch="4";
  else
    AH_THROW_LOGIC("invalid index given; must be 1-4");

  // copy column name to output and do replacement
  out.replace(loc,1,ch);
  return out;
}

// ****************************************************************************

void create_gti_file(const std::string& filename, ahfits::FilePtr* gp,
                     const std::string* extnames, ahfits::FilePtr& fp,
                     const std::string& type) {
  // create file with 4 empty extensions
  ahfits::FilePtr tgp=0;
  ahfits::create(filename,"",&tgp);
  for (int iled1=0; iled1 < NUMLED; iled1++) create_gti_hdu(tgp,extnames[iled1],fp,type);
  ahfits::close(tgp);

  // re-open file 4 times, 1 per extension
  for (int iled1=0; iled1 < NUMLED; iled1++)
    ahfits::open(filename,extnames[iled1],&gp[iled1]);

  AH_INFO(ahlog::HIGH) << "Created output GTI file: " << filename << std::endl;
}

// ****************************************************************************

void create_gti_hdu(ahfits::FilePtr& gp, const std::string& extname,
                    ahfits::FilePtr& fp, const std::string& type) {

  ahfits::addEmptyTbl(gp,extname);
  ahfits::writeKeyValStr(gp,"TELESCOP",ahmission::getTELESCOPString(),"Telescope mission name");
  ahfits::insertColAfter(gp,"START","1D","");
  ahfits::setTUnit(gp,"START","s");
  ahfits::setColumnDescription(gp,"START","Start interval time");
  ahfits::insertColAfter(gp,"STOP","1D","");
  ahfits::setTUnit(gp,"STOP","s");
  ahfits::setColumnDescription(gp,"STOP","Stop interval time");
  if (type == "COARSE") {
    ahfits::insertColAfter(gp,"PLSLEN","1D","");
    ahfits::setTUnit(gp,"PLSLEN","s");
    ahfits::setColumnDescription(gp,"PLSLEN","MXS pulse length");
    ahfits::insertColAfter(gp,"PLSSPC","1D","");
    ahfits::setTUnit(gp,"PLSSPC","s");
    ahfits::setColumnDescription(gp,"PLSSPC","MXS pulse spacing");
  }
  ahmission::keyword::copyAllKeywords(fp,gp,ahmission::keyword::e_EVENT);
  ahfits::writeKeyValStr(gp,"DETNAM","PIXEL","Detector subsystem");
  ahfits::writeKeyValStr(gp,"HDUCLAS1","GTI","Extension contains Good Time Intervals");

}

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: mxstime.cxx,v $
 Revision 1.66  2017/01/13 18:52:15  mwitthoe
 mxstime: fig bug occuring when the MXS start time is before the start of the HK file where the check for overlapping fine GTI was using the incorrect pulse spacing

 Revision 1.65  2016/04/14 16:23:32  mwitthoe
 mxstime: set HDUCLAS1=GTI in output GTI files

 Revision 1.64  2016/03/22 18:20:27  mdutka
 adding parameter logging to standard main and ape_trad_set is now applied to dtdecay

 Revision 1.63  2016/03/18 15:13:16  asargent
 Replaced AH_WARN with AH_INFO

 Revision 1.62  2016/03/09 21:48:14  mwitthoe
 mxstime: fix bug where fine GTI were incorrect for the case where there is an MXS stop but no MXS start AND the afterglow time is larger than fine pulse separation time

 Revision 1.61  2016/02/19 00:14:13  klrutkow
 use ahmission::getTELESCOPString() to write TELESCOP keyword in create_gti_hdu()

 Revision 1.60  2016/01/29 19:09:40  mwitthoe
 mxstime: merge fine GTI into a single GTI in the case where the decay (afterglow) time causes the fine GTI to overlap

 Revision 1.59  2016/01/06 18:32:14  mwitthoe
 mxstime: 1) add PLSLEN/PLSSPC columns to output, coarse GTI file; 2) add LEDnLEN/SPC keywords to output, coarse GTI file

 Revision 1.58  2015/12/29 16:02:41  mwitthoe
 mxstime: throw error if no rows in input FITS table

 Revision 1.57  2015/12/15 02:06:01  mwitthoe
 mxstime: change output file names to use 'fn' and 'cs' instead of 'fine' and 'coarse' in the hash replacement

 Revision 1.56  2015/12/03 21:42:39  mwitthoe
 mxstime: set DETNAM=PIXEL in output GTI extensions to conform to standard

 Revision 1.55  2015/11/25 16:04:58  mwitthoe
 mxstime: add parameter, mxsoffset, which will be applied to the start/stop times of the MXS GTI; set the coarse GTI START value equal to the first fine GTI START for the case where we compute the fine GTI backwards from the coarse STOP value (this allows coarse STOP-START to be an even number of pulse spacings)

 Revision 1.54  2015/11/17 20:40:26  mwitthoe
 mxstime: add column/keyword comments

 Revision 1.53  2015/11/03 19:06:30  mwitthoe
 mxstime: correct fine GTI for case where there is an LED-off time, but not LED-on; in this situation, the fine GTI are computed backwards from the LED-off time

 Revision 1.52  2015/10/16 20:05:47  mwitthoe
 mxstime: fix bug where a non-empty MXS extension without an LED turning on or off may lead to an infinite loop

 Revision 1.51  2015/10/09 19:14:01  mwitthoe
 mxstime: change output GTI extension names

 Revision 1.50  2015/10/09 19:02:50  mwitthoe
 mxstime: remove underscores from output GTI extension names

 Revision 1.49  2015/10/08 21:45:08  mwitthoe
 mxstime: skip rows with a negative TI on/off value for any LED; this occurred by mistake in one test data set, but really screwed the GTI algorithm

 Revision 1.48  2015/10/07 21:11:46  mwitthoe
 mxstime: fix bugs - 1) TSTART in GTI files was not correct when  an LED was on before the start of the input file; 2) the log file incorrectly said a GTI stop was found when the file ended while an LED was on

 Revision 1.47  2015/09/18 20:09:21  mwitthoe
 mxstime: 1) only try to delete files that are present on disk; 2) open input file instead of cloning when not calculating time (calctime=no); 3) only open TIM and delay files if calculating time

 Revision 1.46  2015/09/18 19:29:13  mwitthoe
 mxstime: 1) new parameters, afterglow & dtdecay, to have tool add an afterglow time to the STOP times of the fine GTIs; 2) if afterglow is used, write DTDECAY keyword with the afterglow time in seconds; 3) correctly set the DATAMODE, MXSONOFF, and MXSTYPE keywords; 4) delete output files if there is an early error

 Revision 1.45  2015/09/10 21:50:31  mwitthoe
 mxstime: fix bug where fine GTI were being written in reverse order when the GTI started before the start of the file

 Revision 1.44  2015/08/24 03:42:04  mwitthoe
 mxstime: 1) update default paramter values for columns names; 2) fix algorithm problems when calctime or calcgti is false; 3) first row time assignment was incorrectly added the delay time to TSTART, but delay is already added to TSTART by ahtime; 4) use TSTART as the last TIME when an MXS interval stops after the end of a file; 5) fix TSTART/TSTOP values for GTI extensions when no MXS interval occurs; 6) fix bug where GTI intervals were not written when file ends before stop of an MXS interval (also allow such intervals to be written correctly even when the file ends with a TIME=NULL row

 Revision 1.43  2015/07/30 16:30:13  mwitthoe
 mxstime: add parameter stamping to log file

 Revision 1.42  2015/07/29 18:31:04  mwitthoe
 mxstime: clean tool; see issues 532, 533, and 534

 Revision 1.41  2015/07/22 13:58:37  klrutkow
 changed local CALDB queries for delay and leapsec to use ahmission caldb resolve query

 Revision 1.40  2015/07/09 15:42:17  mwitthoe
 mxstime: comment out debug statements in order to get unit test to pass; tool will be properly cleaned up later

 Revision 1.39  2015/07/06 15:47:55  klrutkow
 changed DATE_OBS to DATE-OBS and DATE_END to DATE-END

 Revision 1.38  2015/07/06 04:16:11  klrutkow
 added call to new delay::resolve function to get CALDB filename

 Revision 1.37  2015/06/03 23:00:39  mwitthoe
 mxstime: many change from funcd test; this is still a working version, but I needed it to get into the next tarball

 Revision 1.36  2015/04/28 18:32:05  mwitthoe
 mxstime: instead of moving to MXS extension, move to 1st binary table by default; it is expected that extended syntax is used to provide the extension name; see issue 515

 Revision 1.35  2015/04/03 18:27:45  mwitthoe
 mxstime: update boolean parameters

 Revision 1.34  2015/03/18 20:40:07  mwitthoe
 mxstime: change DETNAME to DETNAM

 Revision 1.33  2014/12/29 20:19:22  mwitthoe
 mxstime: parameter changes; see issue 472

 Revision 1.32  2014/11/05 21:52:44  mwitthoe
 mxstime: update tool to use new version of ahmath::search_sawY2X() (see issue 458; fix bug where delay time was not being set correctly

 Revision 1.31  2014/09/15 21:02:30  mwitthoe
 mxstime: add support for extended syntax; issue 179

 Revision 1.30  2014/09/10 03:55:39  mwitthoe
 mxstime: fix bug related to switchover to new TIM file library

 Revision 1.29  2014/09/10 02:52:03  mwitthoe
 mxstime tool: update tool to reflect new locations of timfile and leapsec CALDB libraries

 Revision 1.28  2014/08/19 20:35:46  mwitthoe
 mxstime: replace two outgti parameters with single parameter which serves as a template for the two output GTI file names; use TSTART instead of S_TIME as the initial TIME for GTIs that begin before the start of the FITS file

 Revision 1.27  2014/07/30 20:56:24  mdutka
 removed decimal points from plslen and plespc initialization

 Revision 1.26  2014/07/30 16:02:19  mdutka
 Change pulse length and pulse spacing to integers, and added constants in order to convert those values to seconds. ; see issue #374

 Revision 1.25  2014/03/26 18:15:01  mwitthoe
 mxstime: change connect() to connectScalar() (ahfits)

 Revision 1.24  2014/01/22 21:05:47  mwitthoe
 mxstime: update according to code review; issue 331

 Revision 1.23  2014/01/15 22:26:32  peachey
 Add Peer Review comments regarding variable declarations,

 Revision 1.22  2014/01/03 21:56:31  mwitthoe
 mxstime: update standard main, see issue 327

 Revision 1.21  2013/12/02 23:02:27  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.20  2013/11/21 16:51:13  mwitthoe
 mxstime: free memory for allocated GTI routers

 Revision 1.19  2013/11/20 23:19:09  mwitthoe
 mxstime: switch to using new functions in the ahtime library for reading the TIM file; this change allows mxstime to work with TIM files which has a bit-type STATUS column instead of the old, integer-type version; see issue 315

 Revision 1.18  2013/10/17 18:11:35  mwitthoe
 mxstime: as described in issue 298, switched to using 4 ahfits file pointers per GTI file instead of one file pointer per file.  Also moved some variable declarations in doWork to the top of the routine to be more in line with the coding standards

 Revision 1.17  2013/10/07 21:15:25  mwitthoe
 mxstime: switch over to new ahfits connect functions (issue sue 270)

 Revision 1.16  2013/09/17 19:32:06  mwitthoe
 mxstime tool: switch over to using new ahtime library functions (see issue 290)

 Revision 1.15  2013/09/11 20:03:32  mwitthoe
 mxstime: switch how HDUs are accessed to use new defintion of empty string to represent the primary HDU instead of the first, non-primary HDU

 Revision 1.14  2013/08/30 20:19:52  mwitthoe
 mxstime: switch over to new version of ahmission/delay (issue 236)

 Revision 1.13  2013/07/30 19:06:54  mwitthoe
 mxstime: remove unnecessary include for ahgen

 Revision 1.12  2013/07/22 20:28:31  mwitthoe
 mxstime: fix scope issue (indicated by +++ comment) involving routers that were only needed when a parameter had a certain value

 Revision 1.11  2013/07/12 22:22:34  mwitthoe
 mxstime: add MJDREFI/MJDREFF keywords to test TIM file; add code to create GTI files

 Revision 1.10  2013/07/02 17:46:16  mwitthoe
 mxtime tool: fix name of extension accessed in instrument delay CALDB file (from MXS to FW)

 Revision 1.9  2013/05/13 20:02:04  mwitthoe
 remove obsolete ahgti include in mxstime tool

 Revision 1.8  2013/04/18 22:05:41  mwitthoe
 mxstime tool: add statements for writing GTI files (rows and keywords); a new parameter, leapsec, is required to convert TSTART/TSTOP to DATE_OBS/END keywords; if calcgti=false, the router initialization calls will throw an error unless the ahfits Router is allowed to be initialized with a NULL FilePtr

 Revision 1.7  2013/04/16 19:10:13  mwitthoe
 mxstime tool: include instrument delay (from CALDB file)

 Revision 1.6  2013/04/12 22:47:11  mwitthoe
 fix bug in mxstime where warning was incorrectly given upon reaching last row of input file; change make_test_mxtime.cxx to use read/write state in ahfits connect calls

 Revision 1.5  2013/04/10 21:28:07  mwitthoe
 update mxstime to latest TRF (trf_timing_2013-04-02.doc); code compiles, but update is not complete (see +++ comments)

 Revision 1.4  2013/03/26 15:12:40  mwitthoe
 update standard main for mxstime tool (issue 230)

 Revision 1.3  2013/02/19 21:58:32  mwitthoe
 add testdata for mxstime tool; bugfixes in mxstime regarding LED index

 Revision 1.2  2013/01/16 21:22:49  mwitthoe
 major overhaul of the mxstool to bring it into agreement with the latest TRF; only the basic flow of the algorithm is untouched

 Revision 1.1  2013/01/09 22:07:48  mwitthoe
 add mxstime tool


*/

