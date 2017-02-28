/// \file ahmkehk.cxx
/// \brief Tool to generate an event filtering file from attitude and orbit data
/// \author Robert S. Hill, A.J. Sargent
/// \date $Date: 2016/07/21 17:45:26 $

/**

\defgroup tool_ahmkehk Generate event filtering file from attitude and orbit (ahmkehk)
@ingroup mod_mission_tasks

This task uses a number of input files to generate or calculate parameters 
equi-space in time that are stored in a *ehk file. These parameters are 
either used as part of mkf or in the screening tasks related to Astro-H. 

Source files:

  ahmkehk.cxx

Library dependencies:

  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath
  astroh/mission/lib/ahmission/caldb
  astroh/mission/lib/ahmission/keyword
  astroh/mission/lib/ahtime
  attitude/lib/atfunctions
  attitude/lib/coordfits
  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
  heacore/ape
  heacore/heainit/headas
  heagen/lib/geomag

Modification history:

  Ver   Date        Author  Description
  1.0   2015-07-27  AJS     initial version, after cleaning code

*/


#define AHLABEL tool_ahmkehk
#define AHCVSID "$Id: ahmkehk.cxx,v 1.56 2016/07/21 17:45:26 rshill Exp $"
#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahapp/ahapp.h"
#include "ahgen/ahgen.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include "ahmath/ahmath.h"    
#include "ahtime/ahtime.h"

#include "ahmission/caldb.h"  // ahmission::caldb::resolve()
#include "ahmission/keyword.h"// ahmission::keyword::copyAllKeywords()

#include "headas.h"           // expand_item_list()
#include "cfortran.h"         // xyzmag()

#include "hd_math_compat.h"   // isfinite() 

#include <algorithm>          // std::max_element

                                      PROTOCCALLSFSUB7(XYZMAG,xyzmag,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT)
#define XYZMAG(A1,A2,A3,A4,A5,A6,A7)       CCALLSFSUB7(XYZMAG,xyzmag,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,A1,A2,A3,A4,A5,A6,A7)

#ifdef __cplusplus
extern "C" {
#endif

#include "coordfits2.h"
#include "atFunctions.h"

#ifdef __cplusplus
}
#endif

#include <math.h>
#include <iomanip>
#include <sstream>
#include <cstring>
#include <string>
#include <vector>

/** \addtogroup tool_ahmkehk
 *   @{
 *   */

struct Param {
  // User-supplied parameters
  std::string attfile;     // input attitude file name
  std::string orbfile;     // input orbit file name
  std::string outfile;     // output EHK file name
  double tstart;           // start of output time range, or 0 to get from attfile
  double tstop;            // end of output time range, or 0 to get from attfile
  double bintime;          // output time step
  double textend;          // time margin for widening time range
  std::string reference;   // input reference file name
  std::string timecol;     // name of mission time column in reference file
  std::string teldeffile;  // input teldef file name
  std::string optaxis;     // FOC optical axes of instruments
  std::string cor2file;    // input rigidity  Suzaku file name
  std::string cor3file;    // input rigidity  IGRF 2016 file name
  std::string saafile;     // input  saa vertices for each instrument 
  std::string leapsecfile; // input leap second file name
  std::string attext;      // input attitude extension name
  std::string attcol;      // input attitude column name
  std::string attform;     // input attitude format name
  std::string orbext;      // input orbit extension name
  std::string orbcol;      // input orbit column name
  std::string orbform;     // input orbit format name

  // Derived parameters
  double hx1_optx;
  double hx1_opty;
  double hx2_optx;
  double hx2_opty;
  double sxs_optx;
  double sxs_opty;
  double sxi_optx;
  double sxi_opty;
  double tstart_actual;    // actual tstart used
  double tstop_actual;     // actual tstop  used

  Param() : attfile(""), orbfile(""), outfile(""), tstart(0.0), tstop(0.0),
    bintime(0.0), textend(0.0), reference(""), timecol(""), teldeffile(""),
    cor2file(""), cor3file(""), saafile(""),leapsecfile(""), attext(""), attcol(""), attform(""),
    orbext(""), orbcol(""), orbform(""), 
    hx1_optx(0.0), hx1_opty(0.0),
    hx2_optx(0.0), hx2_opty(0.0),
    sxs_optx(0.0), sxs_opty(0.0),
    sxi_optx(0.0), sxi_opty(0.0),
    tstart_actual(0.0), tstop_actual(0.0) {};
};


/// \brief Structure containing several data for SAA
struct SAAData {

  SAAData() : m_useAtFunctions(0), m_instrument(""),
              m_saa(0), m_t_saa(0), m_tn_saa(0),
              m_last_saa(0), m_last_t_saa(0), m_next_t_saa(0) {};

  bool m_useAtFunctions;        /// Boolean to use Attitude functions for instrument
  std::string m_instrument;     /// String containing instrument name

  ahmath::Polygon m_saaPolygon; /// Structure containing polygon data

  int m_saa;                    /// In SAA? (0 = no)
  double m_t_saa;               /// Time since last SAA crossing (0 = inside SAA)
  double m_tn_saa;              /// Time until next SAA crossing (0 = inside SAA)

  int m_last_saa;               /// SAA from previous row
  double m_last_t_saa;          /// t_saa from previous row
  double m_next_t_saa;          /// tn_saa from previous row

};

/// \brief Structure containing several data for multiple polygon SAA
struct SAA2Data {

  SAA2Data() : m_instrument(""),
              m_saa(0), m_t_saa(0), m_tn_saa(0) {} ;

  std::vector<SAAData> m_saa_data;  /// Individual SAA pieces

  std::string m_instrument;     /// String containing instrument name

  //  Composite quantities
  int m_saa;                    /// In SAA? (0 = no)
  double m_t_saa;               /// Time since last SAA crossing (0 = inside SAA)
  double m_tn_saa;              /// Time until next SAA crossing (0 = inside SAA)

};

/// \brief Vector of SAA Data (one element per instrument)
typedef std::vector<SAAData> SAADataVec;

/// \brief Vector of SAA Data (one element per instrument)
typedef std::vector<SAA2Data> SAA2DataVec;

/// \brief Get parameter values
/// \param[out] par parameter structure
void getPar(Param & par);
  
/// \brief Open files and fill information structures
/// \param[in] par structure containing parameter values
/// \param[out] fp_ref pointer to reference file
/// \param[out] fp_out pointer to outpur file
/// \param[out] leapsec_data leap second table
/// \param[out] att_descrip_ptr pointer to attitude file descriptor
/// \param[out] mjdrefi mission epoch, integer part
/// \param[out] mjdreff mission epoch, fractional part
/// \param[out] orb_descrip_ptr pointer to orbit file descriptor
/// \param[out] rigidity2_data_ptr pointer to rigidity data table for COR2
/// \param[out] rigidity3_data_ptr pointer to rigidity data table for COR3
/// \param[out] saa_data Structure containing SAA data for each instrument
/// \param[out] saa2_data Structure containing multiple-polygon SAA data for each instrument
/// \param[out] teldef_data_ptr pointer to telescope (coordinates) definition
/// \param[out] mean_ra mean R. A., dec, and roll (deg)
/// \param[out] align rotation and roll angle alignment convention
void initialize(Param & par,
                ahfits::FilePtr & fp_ref, ahfits::FilePtr & fp_out,
                ahtime::leapsec::LeapSecTable & leapsec_data,
                GENATTFILE ** att_descrip_ptr,
                int & mjdrefi, double & mjdreff,
                GENORBFILE ** orb_descrip_ptr,
                AtRigData2 ** rigidity2_data_ptr,
                AtRigData2 ** rigidity3_data_ptr,
                SAADataVec & saa_data,
                SAA2DataVec & saa2_data,
                TELDEF2 ** teldef_data_ptr, 
                POINTING ** mean_p, ALIGN ** align);

/// \brief Process the orbit and attitude files
/// \param[in] par structure containing parameter values
/// \param[in] att_descrip attitude file descriptor
/// \param[in] mjdrefi mission epoch, integer part
/// \param[in] mjdreff mission epoch, fractional part
/// \param[in] orb_descrip pointer to orbit file descriptor
/// \param[in] fp_ref pointer to reference file
/// \param[in] fp_out pointer to outpur file
/// \param[in] leapsec_data leap second table
/// \param[in] rigidity2_data rigidity data table for COR2
/// \param[in] rigidity3_data rigidity data table for COR3
/// \param[in] saa_data Structure containing all SAA data for each instrument
/// \param[in] teldef_data telescope (coordinates) definition
/// \param[in] mean_p mean R. A., Dec and roll (deg)
/// \param[in] align rotation and roll angle alignment convention
void doWork(const Param & par,
            GENATTFILE * att_descrip,
            int mjdrefi, double mjdreff,
            GENORBFILE * orb_descrip,
            ahfits::FilePtr fp_ref, ahfits::FilePtr fp_out,
            ahtime::leapsec::LeapSecTable & leapsec_data,
            AtRigData2 * rigidity2_data,
            AtRigData2 * rigidity3_data,
            SAADataVec & saa_data,
            SAA2DataVec & saa2_data,
            TELDEF2 * teldef_data,
            POINTING * mean_p, ALIGN * align);

/// \brief Free data structures and close files
/// \param[in] leapsec_data leap second table
/// \param[in] rigidity2_data rigidity data table for COR2
/// \param[in] rigidity3_data rigidity data table for COR3
/// \param[in] teldef_data telescope (coordinates) definition
/// \param[in] att_descrip attitude file descriptor
/// \param[in] fp_ref pointer to reference file
/// \param[in] fp_out pointer to outpur file
void finalize(ahtime::leapsec::LeapSecTable & leapsec_data,
              AtRigData2 * rigidity2_data,
              AtRigData2 * rigidity3_data,
              POINTING * mean_p,
              TELDEF2 * teldef_data,
              GENORBFILE * orb_descrip,
              GENATTFILE * att_descrip,
              ahfits::FilePtr & fp_ref, ahfits::FilePtr & fp_out);

/// \brief Add a (lat,lon) point to a multiple polygon SAA 
/// \param[in] index polygon number, 0-based
/// \param[in] latlon (lat,lon) pari
/// \param[in] inststr instrument name
/// \param[inout] saa2 multiple polygon SAA structure
void addPointToSAA2Data(int & index, double latlon[2], const std::string inststr, 
  SAA2Data & saa2);

/// \brief Load SAA data and vertex information into SAA structure
/// \param[in] saafile  Input SAA file 
/// \param[in] saa_data Structure containing all SAA data for each instrument
void load_saa_data(std::string & saafile, SAADataVec & saa_data);

/// \brief Load multiple-polygon SAA data and vertex information into SAA structure
/// \param[in] saafile  Input SAA file 
/// \param[in] saa2_data Structure containing all SAA data for each instrument
void load_saa2_data(std::string & saafile, SAA2DataVec & saa2_data);

/// \brief Use AtFunctions to calculate elevation from DAY/NIGHT boundary
/// \param[in] t0 input mission time to find orbit vector
/// \param[in] epoch_utc DateTime UTC epoch used a reference for mission time
/// \param[in] leapsec_data leap second table
/// \param[in] orb_descrip pointer to orbit file descriptor
/// \param[out] day_night output earth occultation code (0: sky 1: dark 2: bright earth)
/// \param[out] alt0  Output calculated altitude
void computeDayNightAltitude(double t0, const ahtime::AhDateTime& epoch_utc, 
                             ahtime::leapsec::LeapSecTable & leapsec_data,
                             GENORBFILE * orb_descrip,
                             int & day_night, double & alt0);

/// \brief Calculate SAA and time boundary from SAA vertex file and atFunctions
///        Using time incrments (Coarse: +60s; Fine: -1s)
/// \param[in] mission_time input mission time to find orbit vector
/// \param[in] backwards Search for SAA in reverse increments (Coarse: -60s; Fine: +1s)
/// \param[in] skipCoarse Skip the coarse SAA search (must input in_saa0)
/// \param[in] saa Structure containing all SAA data for a single instrument
/// \param[in] epoch_utc DateTime UTC epoch used a reference for mission time
/// \param[in] leapsec_data leap second table
/// \param[in] orb_descrip pointer to orbit file descriptor
/// \param[in] in_saa0 input SAA value to start fine search from (if skipCoarse)
/// \param[out] out_saa_time Output time boundary of the SAA found
void computeSAATimeBoundary(double mission_time, bool backwards, bool skipCoarse, 
                            const SAAData & saa, const ahtime::AhDateTime & epoch_utc, 
                            ahtime::leapsec::LeapSecTable & leapsec_data, 
                            GENORBFILE * orb_descrip, int in_saa0, double & out_saa_time);

/// \brief Translate between ORBPOSITION and atFunctions 3-vector.
/// \param[out] atv 3-vector of atVect type
/// \param[in] orbpos orbital position in coordfits representation
void copy_atvect_from_orbpos(AtVect atv, ORBPOSITION* orbpos);

/// \brief Angular difference, making sure absolute value < 180
/// \param[in] phi1 ang.e (deg)
/// \param[in] phi0 angle (deg)
/// \return difference phi1 - phi0
double delta_phi(double phi1, double phi0);

/// \brief use ahfits to create new FITS file for output
/// \param[in] outfile output file name
/// \param[out] fp_out output file pointer
/// \param[in] actual_leapsec resolved leap second file name
/// \param[in] attfile attitude file name
/// \param[in] orbfile orbit file name
/// \param[in] actual_teldef resolved TELDEF file name
/// \param[in] actual_rigfile2 resolved rigidity file name for COR2
/// \param[in] actual_rigfile3 resolved rigidity file name for COR3
/// \param[in] actual_saafile resolved SAA file name for SAA columns
/// \param[in] bintime Binning factor
void create_output_file(const std::string& outfile, ahfits::FilePtr& fp_out,
                        const std::string& actual_leapsec, const std::string& attfile, const std::string& attext,
                        const std::string& orbfile, const std::string& actual_teldef,
                        const std::string& actual_rigfile2, const std::string& actual_rigfile3, 
                        const std::string& actual_saafile, const double& bintime);

/// \brief Compute the UTC version of MJD, which is needed by
///   the atFunction library
/// \param[in] mission_time mission time expressed as seconds wrt to epoch
/// \param[in] epoch DateTime UTC epoch used a reference for mission time
/// \param[in] leapsec_data leap second table
/// \param[out] time_utc mission time converted to UTC
/// \param[out] mjd_utc MJD(UTC) of mission time, suitable for atFunctions
void compute_mjd_utc (double mission_time, const ahtime::AhDateTime& epoch,
                      ahtime::leapsec::LeapSecTable & leapsec_data,
                      ahtime::AhDateTime& time_utc, double & mjd_utc);

// ****************************************************************************

/// \brief ahmkehk tool
int main(int argc, char** argv) {

  Param par;
  double mjdreff=0.0;
  int mjdrefi=0;

  ahtime::leapsec::LeapSecTable leapsec_data;
  AtRigData2 * rigidity2_data = 0;
  AtRigData2 * rigidity3_data = 0;
  TELDEF2 * teldef_data = 0;
  GENATTFILE * att_descrip = 0;  // Points to a struct with state of attitude file
  GENORBFILE * orb_descrip = 0;  // Points to a struct with state of orbit file
  ALIGN * align=0;               // Alignment structure containing roll angle convention
  POINTING * mean_p;             // Structure containg mean pointing
  SAADataVec saa_data(8);
  SAA2DataVec saa2_data(6);

  ahfits::FilePtr fp_ref=0, fp_out=0;

  int status = ahapp::startUp(argc,argv,TOOLTAG);
  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog();
      initialize(par, fp_ref, fp_out,
                 leapsec_data, &att_descrip, mjdrefi, mjdreff,
                 &orb_descrip, &rigidity2_data, &rigidity3_data, 
                 saa_data, saa2_data, &teldef_data, &mean_p, &align);
      doWork(par, att_descrip, mjdrefi, mjdreff, orb_descrip, 
             fp_ref, fp_out,
             leapsec_data, rigidity2_data, rigidity3_data, 
             saa_data, saa2_data, teldef_data, mean_p, align);
      finalize(leapsec_data, rigidity2_data, rigidity3_data,
               mean_p, teldef_data, orb_descrip,
               att_descrip, fp_ref, fp_out);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par, fp_ref, fp_out,
                   leapsec_data, &att_descrip, mjdrefi, mjdreff,
                   &orb_descrip, &rigidity2_data, &rigidity3_data, 
                   saa_data, saa2_data, &teldef_data, &mean_p, &align);
        doWork(par, att_descrip, mjdrefi, mjdreff, orb_descrip, 
               fp_ref, fp_out,
               leapsec_data, rigidity2_data, rigidity3_data, 
               saa_data, saa2_data, teldef_data, mean_p, align);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog();
        status = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(leapsec_data, rigidity2_data, rigidity3_data,
                 mean_p, teldef_data, orb_descrip,
                 att_descrip, fp_ref, fp_out);
      } catch (const std::exception &x) {
        status = 1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
        status = 1;
        AH_ERR << "During shutDown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }
  return status;
}

// ****************************************************************************

void getPar(Param & par) {

  // Get input attitude file name.
  par.attfile = ahapp::getParString("attfile");

  // Get input orbit file name.
  par.orbfile = ahapp::getParString("orbfile");

  // Get output EHK file name.
  par.outfile = ahapp::getParString("outfile");

  // Get start of output time range.
  par.tstart = ahapp::getParDouble("tstart");

  // Get end of output time range.
  par.tstop = ahapp::getParDouble("tstop");

  // Get output time step.
  par.bintime = ahapp::getParDouble("bintime");

  // Get time margin for widening time range.
  par.textend = ahapp::getParDouble("textend");

  // Get input reference file name.
  par.reference = ahapp::getParString("reference");

  // Get name of mission time column in reference file
  par.timecol = ahapp::getParString("timecol");

  // Get input teldef file name.
  par.teldeffile = ahapp::getParString("teldeffile");

  // Get input teldef file name.
  par.optaxis = ahapp::getParString("optaxis");

  // Get input rigidity file name.
  par.cor2file = ahapp::getParString("cor2file");

  // Get input rigidity file name.
  par.cor3file = ahapp::getParString("cor3file");

  // Get input rigidity file name.
  par.saafile = ahapp::getParString("saafile");

  // Get input leap second file name.
  par.leapsecfile = ahapp::getParString("leapsecfile");

  // Get input attitude extension name.
  par.attext = ahapp::getParString("attext");

  // Get input attitude column name.
  par.attcol = ahapp::getParString("attcol");

  // Get input attitude format name.
  par.attform = ahapp::getParString("attform");

  // Get input orbit extension name.
  par.orbext = ahapp::getParString("orbext");

  // Get input orbit column name.
  par.orbcol = ahapp::getParString("orbcol");

  // Get input orbit format name.
  par.orbform = ahapp::getParString("orbform");

}

// ****************************************************************************

/// \brief Process the orbit and attitude files
void initialize(Param & par,
                ahfits::FilePtr & fp_ref, ahfits::FilePtr & fp_out,
                ahtime::leapsec::LeapSecTable & leapsec_data,
                GENATTFILE ** att_descrip_ptr,
                int & mjdrefi, double & mjdreff,
                GENORBFILE ** orb_descrip_ptr,
                AtRigData2 ** rigidity2_data_ptr,
                AtRigData2 ** rigidity3_data_ptr,
                SAADataVec & saa_data,
                SAA2DataVec & saa2_data,
                TELDEF2 ** teldef_data_ptr,
                POINTING ** mean_p, ALIGN ** align) {

  int cfitsio_status=0;

  double mean_ra=0.0, mean_dec=0.0, mean_roll=0.0;  // Mean pointing
  double half_time=0., ref_time=0.;

  std::stringstream msg;
  std::string actual_leapsec="";     // name of leap second file after CALDB query
  std::string actual_rigfile2="";    // name of rigidity file after CALDB query
  std::string actual_rigfile3="";    // name of rigidity file after CALDB query
  std::string actual_saafile="";     // name of SAA boundary file after CALDB query
  std::string actual_teldef="";      // name of teldef file after CALDB query
  std::string uc_ref="";             // reference parameter converted to uppercase
  std::string dateobs="";
  ahfits::FilePtr fpatt = 0;

  // Allocate basic structure elements in alignment and mean pointing
  *align = allocateDefaultAlign();
  *mean_p = allocatePointing();

  // Variables related to attitude file access.  Any of these could
  // be made into task parameters if useful in the future.
  std::string att_ext_name="", att_col_name="";
  int att_format=AF_QUAT, att_interp_method=ATTFILE_LINEAR, use_interp_margin=0;
  double extrap_margin=0.0, interp_margin=0.0, limit_nearby=0.0;

  // Variables used in orbit file access.

  const char sep[] = ",";           // Separator for column names
  char** orb_col_name=0;            // Array of parsed column names
  char orbcolbuffer[FLEN_VALUE];    // Working storage for parsing column names
  char* token=0;                    // Utility pointer for parsing column names
  ORBFORMAT orb_format=ORBF_UNKNOWN;  // Enum for orbit file format
  ORBINTERP orb_interp_method=ORBI_UNKNOWN;  // Enum for orbit interp method
  int num_orb_cols=0;               // Number of orbit file columns extracted
  int icol=0;                       // Index of loop over columns

  // Variables used for optical axis parameter fetching
  
  int status=0;
  int trim=1;         // trim spaces
  int skip=1;         // exclude empty items
  int guard=0;        // do not protect against commas in parentheses 
  int nitems=0;
  char ** optaxis_arr = 0;

  //  Resolve file specs for leap second, rigidity, and teldef files, 
  //  and read them in
  //  leapsecond file can be in refarea or CALDB
  //  resolve function will return location of file
  actual_leapsec = ahmission::caldb::resolve(par.leapsecfile, "leap second", "INS", "-", "LEAPSECS", "-", "-", "GEN");
  ahtime::leapsec::load(actual_leapsec, leapsec_data);
  AH_INFO(ahlog::LOW) << "Leap second file loaded" << std::endl;
  // Update leapsecfile parameter for logging
  ape_trad_set_string("leapsecfile",actual_leapsec.c_str());

  // Query CALDB COR file for COR2 column (2006-04-21)
  // cor2file is typically rigidity_20060421.fits
  // rigidity files can be in refarea or CALDB
  // resolve function will return location of file
  actual_rigfile2 = ahmission::caldb::resolve(par.cor2file, "COR2 rigidity", "INS", "-", "RIGIDITY", "2005-01-01","-","GEN");
  atRigSet2(rigidity2_data_ptr, const_cast<char *>(actual_rigfile2.c_str()));
  if(0 == *rigidity2_data_ptr) AH_THROW_RUNTIME ("Could not open COR2 file");
  AH_INFO(ahlog::LOW) << "Rigidity file opened" << std::endl;
  // Update cor2file parameter for logging
  ape_trad_set_string("cor2file",actual_rigfile2.c_str());

  // Query CALDB COR file for COR3 column (2016-01-01)
  // cor3file is typically rigidity_20160101.fits
  actual_rigfile3 = ahmission::caldb::resolve(par.cor3file, "COR3 rigidity", "INS", "-", "RIGIDITY", "2016-01-01","-","GEN");
  atRigSet2(rigidity3_data_ptr, const_cast<char *>(actual_rigfile3.c_str()));
  if(0 == *rigidity3_data_ptr) AH_THROW_RUNTIME ("Could not open COR3 file");
  AH_INFO(ahlog::LOW) << "Rigidity file opened" << std::endl;
  // Update cor3file parameter for logging
  ape_trad_set_string("cor3file",actual_rigfile3.c_str());


  // Open the attitude file and read the DATE-OBS keyword for SAA and TelDef CALDB files
  ahfits::open(par.attfile,"ATTITUDE",&fpatt);
  dateobs = ahfits::getKeyValStr(fpatt,"DATE-OBS");
  ahfits::close(fpatt);

  // Query SAA CALDB file 
  actual_saafile = ahmission::caldb::resolve(par.saafile, "saa data", "GEN", "-", "SAA", dateobs);
  // Load SAA data
  // Data is sorted as follows:
  //   saaData[0] = SUZAKU
  //   saaData[1] = HXD
  //   saaData[2] = HXI1
  //   saaData[3] = HXI2
  //   saaData[4] = SGD1
  //   saaData[5] = SGD2
  //   saaData[6] = SXI
  //   saaData[7] = SXS
  load_saa_data(actual_saafile,saa_data);
  load_saa2_data(actual_saafile,saa2_data);
  AH_INFO(ahlog::HIGH) << "SAA data loaded" << std::endl;
  // Update saafile parameter for logging
  ape_trad_set_string("saafile",actual_saafile.c_str());
  
  // Query for the SXI TelDef file
  actual_teldef = ahmission::caldb::resolve(par.teldeffile, "sxi teldef", "SXI", "-", "TELDEF", dateobs);
  AH_INFO(ahlog::HIGH) << "TELDEF file is " << actual_teldef << std::endl;

  readTelDef2(actual_teldef.c_str(), teldef_data_ptr);
  if(0 == *teldef_data_ptr) AH_THROW_RUNTIME ("Could not open TelDef file");
  AH_INFO(ahlog::LOW) << "TELDEF file opened" << std::endl;
  // Update teldeffile parameter for logging
  ape_trad_set_string("teldeffile",actual_teldef.c_str());

  // Open the attitude, orbit, reference, and output files
  att_ext_name = par.attext;
  att_col_name = par.attcol;
  if (ahgen::strtoupper(par.attform) == "QUAT") {
    att_format = AF_QUAT;
  } else if (ahgen::strtoupper(par.attform) == "EULER") {
    att_format = AF_EULER;
  } else {
    AH_THROW_RUNTIME ("Invalid attitude format specified");
  }

  // The following settings affect the values returned by the called 
  //   function findQuatInGenAttFile.
    
  // Interpolate quaternions linearly with respect to time.
  att_interp_method = ATTFILE_LINEAR;   

  // Set attitude extrapolation limits to be before
  // the first attitude row by extrap_margin seconds and after the 
  // last attitude row by extrap_margin seconds.
  // It is the responsibility of the calling program (this program)
  // to check the current time against these limits by calling the function 
  // isInExtrapolatedAttFile.
  // Extrapolation is done by nearest neighbor.
  extrap_margin = par.textend + par.bintime;

  // Permit linear interpolation regardless of the difference between the
  // current time and the nearest bracketing times in the attitude file.
  // (The alternative would be for the function to return a null quaternion
  // if interpolating between bracketing times that are too far apart, and to
  // take the quaternion from the nearer bracketing time if it is close enough
  // and the farther bracketing time is too far away.)
  interp_margin = 0.0;
  use_interp_margin = 0;
  *att_descrip_ptr = openGenAttFile(
                       const_cast<char *>(par.attfile.c_str()),
                       const_cast<char *>(att_ext_name.c_str()),
                       const_cast<char *>(att_col_name.c_str()),
                       att_format,
                       att_interp_method,
                       extrap_margin,
                       interp_margin,
                       use_interp_margin);
  if(0 == *att_descrip_ptr) AH_THROW_RUNTIME ("Could not open attitude file");

  // The following setting affects the values returned by the called 
  //   function findQuatInGenAttFile, if and only if limit_nearby is
  //   non-negative.
  // If the attitude file has one and only one row:
  //   The current time must be within limit_nearby seconds of the row time, or else false
  //   is returned as the function value; the quaternion from the row is returned,
  //   regardless.
  // If the attitude file has multiple rows:
  //   The current time must be within limit_nearby seconds of both the preceding and following
  //     rows in the attitude file, or else false is returned as the function value;
  //     however, the linearly interpolated quaternion is returned, regardless.
  // (Note that if att_interp_method == ATTFILE_LINEAR_NEARBY, which is not the case 
  //   here, another consequence follows.  Unless the current time is within limit_nearby seconds 
  //   of both the preceding and following rows, then linear interpolation is 
  //   skipped and the quaternion from the nearer row in time is returned.  In this case
  //   also, false is returned as the function value.)
  limit_nearby = -1.0;
  requireTimeNearAttFileRecord((*att_descrip_ptr)->af, limit_nearby);
  AH_INFO(ahlog::HIGH) << "Attitude file opened" << std::endl;

  if (ahgen::strtoupper(par.orbform) == "KEPLERIAN") {
    orb_format = ORBF_KEPLERIAN; 
    num_orb_cols=6;
  } else if (ahgen::strtoupper(par.orbform) == "COMPONENTS") {
    orb_format = ORBF_COMPONENTS_POS; 
    num_orb_cols=3;
  } else if (ahgen::strtoupper(par.orbform) == "VECTOR") {
    orb_format = ORBF_VECTOR_POS; 
    num_orb_cols=1;
  } else {
    AH_THROW_RUNTIME ("Invalid orbit format specified");
  }

  orb_interp_method = ORBI_WEIGHTED;  /* Always do linear orbit interpolation */
  orb_col_name = (char**) malloc(num_orb_cols*sizeof(char*));
  if (ORBF_VECTOR == orb_format) {
    orb_col_name[0] = (char *) malloc(FLEN_VALUE*sizeof(char));
    strcpy(orb_col_name[0], const_cast<char*>((par.orbcol).c_str()));
  } else {
    strcpy(orbcolbuffer, const_cast<char*>((par.orbcol).c_str()));
    for (icol=0; icol<num_orb_cols; icol++) {
      orb_col_name[icol] = (char *) malloc(FLEN_VALUE*sizeof(char));
      if (icol == 0) {
        token = std::strtok(orbcolbuffer, sep);
      } else {
        token = std::strtok(NULL, sep);
      }
      strcpy(orb_col_name[icol], token);
      AH_DEBUG << "Orbit column name " << icol << " parsed:  " << orb_col_name[icol] << std::endl;
    }
  }

  *orb_descrip_ptr = openGenOrbFile(
                       const_cast<char *>((par.orbfile).c_str()),
                       const_cast<char *>((par.orbext).c_str()),
                       orb_col_name,
                       orb_format, orb_interp_method);
  if(0 == *orb_descrip_ptr) AH_THROW_RUNTIME ("Could not open orbit file");
  AH_INFO(ahlog::LOW) << "Orbit file opened" << std::endl;

  // Get start and stop parameters from attitude file header if supplied as zero
  par.tstart_actual = par.tstart;
  par.tstop_actual = par.tstop;

  if (par.tstart_actual <= 0.0 || par.tstop_actual <= 0.0) {
    par.tstart_actual = (*att_descrip_ptr)->af->tstart;
    par.tstop_actual = (*att_descrip_ptr)->af->tstop;
  }
  par.tstart_actual = floor(par.tstart_actual - par.textend);
  par.tstop_actual = ceil(par.tstop_actual + par.textend);
  ape_trad_set_double("tstart",par.tstart_actual);
  ape_trad_set_double("tstop",par.tstop_actual);
  
  // Open reference file
  uc_ref = par.reference;
  for (unsigned int i=0; i<(par.reference).length(); ++i) {
    uc_ref[i] = toupper(par.reference[i]);
  }
  if (uc_ref != "" && uc_ref != "NONE") {
    AH_INFO(ahlog::HIGH) << "Reference file is " << par.reference << std::endl;
    ahfits::open(par.reference, "", &fp_ref);
    if (ahfits::isPrimary(fp_ref)) ahfits::firstHDU(fp_ref,ahfits::e_BINARY_TBL);  // go to first binary table if HDU not specified by extended syntax
    AH_INFO(ahlog::LOW) << "   reference file opened" << std::endl;
    ahfits::Router rt_ref_tmp(fp_ref);
    rt_ref_tmp.connectScalar(ahfits::e_READONLY, par.timecol, ref_time);
    ahfits::firstRow(fp_ref);
    ahfits::readRow(fp_ref);
    msg.str("");
    msg << std::setprecision(15) << "   first row time = " << ref_time;
    AH_INFO(ahlog::LOW) << msg.str() << std::endl;
    par.tstart_actual = ref_time;
    ahfits::lastRow(fp_ref);
    ahfits::readRow(fp_ref);
    msg.str("");
    msg << std::setprecision(15) << "   last row time = " << ref_time;
    AH_INFO(ahlog::LOW) << msg.str() << std::endl;
    par.tstop_actual = ref_time;
    ahfits::firstRow(fp_ref);
  }

  // reading alignment structure from teldef
  for (int isys=0; isys<(**teldef_data_ptr).n_coordsys; ++isys) {
    if ((**teldef_data_ptr).skyattparam[isys] != 0) {
      *align = (**teldef_data_ptr).skyattparam[isys]->alignment;
      break;
    }
  }

  // read RA_NOM, DEC_NOM, ROLL_NOM from attitude file
  cfitsio_status = 0;
  fits_read_key((*att_descrip_ptr)->af->fp,TDOUBLE,"RA_NOM",&mean_ra,0,&cfitsio_status);
  fits_read_key((*att_descrip_ptr)->af->fp,TDOUBLE,"DEC_NOM",&mean_dec,0,&cfitsio_status);
  fits_read_key((*att_descrip_ptr)->af->fp,TDOUBLE,"PA_NOM",&mean_roll,0,&cfitsio_status);
  if (cfitsio_status != 0 || mean_ra == -999.0 || mean_dec == -999.0 || mean_roll == -999.0) {
    QUAT q;
    AH_INFO(ahlog::LOW) << "*** Valid RA_NOM, DEC_NOM, and ROLL_NOM not found in attitude file header" << std::endl;
    AH_INFO(ahlog::LOW) << "Using attitude at 0.5*(TSTART+TSTOP)" << std::endl;
    half_time = 0.5*(par.tstart_actual + par.tstop_actual);
    if ( 0 == findQuatInGenAttFile(*att_descrip_ptr, &q, half_time) ) {
      printGenAttFile(*att_descrip_ptr, stdout);
      msg.str("");
      msg << "Time " << std::setprecision(15) << half_time << " not found in attitude file";
      AH_THROW_RUNTIME(msg.str());
    }
    convertQuatToPointing(*mean_p,&q,*align);
  } else {
    AH_INFO(ahlog::LOW) << "*** RA_NOM, DEC_NOM, and PA_NOM found in attitude file header" << std::endl;
    (**mean_p).ra = mean_ra;
    (**mean_p).dec = mean_dec;
    (**mean_p).roll = mean_roll;
  }
  AH_INFO(ahlog::HIGH) << "Mean pointing:" << std::endl;
  msg.str("");
  msg << std::setprecision(15) << (**mean_p).ra;
  AH_INFO(ahlog::HIGH) << "   alpha = " <<  msg.str() << std::endl;
  msg.str("");
  msg << std::setprecision(15) << (**mean_p).dec;
  AH_INFO(ahlog::HIGH) << "   delta = " << msg.str() << std::endl;
  msg.str("");
  msg << std::setprecision(15) << (**mean_p).roll;
  AH_INFO(ahlog::HIGH) << "   roll  = " << msg.str() << std::endl;

  AH_INFO(ahlog::HIGH) << "Generating output file: " << std::endl;
  AH_INFO(ahlog::HIGH) << "   " << par.outfile << std::endl;

  // Read the optical axis coordinates from list in optaxis parameter
  // USAGE NOTE: The optical axis coordinates in TELDEF files are in
  // DET. These need to be in FOC. The conversion can be done using the
  // coordpnt task

  optaxis_arr=expand_item_list((char*)par.optaxis.c_str(),&nitems,',',trim,skip,guard,&status);
  if (status != 0) {
    free(optaxis_arr);
    std::stringstream msg;
    msg << "failure in expand_item_list call: status = " << status;
    AH_THROW_RUNTIME(msg.str());
  }
  if(nitems != 8) {
    free(optaxis_arr);
    std::stringstream msg;
    msg << "Invalid number of optical axis values for optaxis parameter." << std::endl;
    msg << "optaxis = " << par.optaxis.c_str() <<  " (nitems = " << nitems << ")" << std::endl;
    AH_THROW_RUNTIME(msg.str());
  }
  par.hx1_optx = atof(optaxis_arr[0]);
  par.hx1_opty = atof(optaxis_arr[1]);
  par.hx2_optx = atof(optaxis_arr[2]);
  par.hx2_opty = atof(optaxis_arr[3]);
  par.sxi_optx = atof(optaxis_arr[4]);
  par.sxi_opty = atof(optaxis_arr[5]);
  par.sxs_optx = atof(optaxis_arr[6]);
  par.sxs_opty = atof(optaxis_arr[7]);
  free(optaxis_arr);
  
  AH_INFO(ahlog::HIGH) << "Optical axis coordinates: " << std::endl;
  AH_INFO(ahlog::HIGH) << "   optaxis = " << par.optaxis.c_str() <<  " (nitems = " << nitems << ")" << std::endl;
  AH_INFO(ahlog::HIGH) << "   HXI1 : (FOCX,FOCY) = (" << par.hx1_optx << "," << par.hx1_opty << ")" << std::endl;
  AH_INFO(ahlog::HIGH) << "   HXI2 : (FOCX,FOCY) = (" << par.hx2_optx << "," << par.hx2_opty << ")" << std::endl;
  AH_INFO(ahlog::HIGH) << "   SXI  : (FOCX,FOCY) = (" << par.sxi_optx << "," << par.sxi_opty << ")" << std::endl;
  AH_INFO(ahlog::HIGH) << "   SXS  : (FOCX,FOCY) = (" << par.sxs_optx << "," << par.sxs_opty << ")" << std::endl;

  // Open output file with column definitions and standard header keywords
  create_output_file(par.outfile, fp_out,
    actual_leapsec, par.attfile, par.attext, par.orbfile, 
    actual_teldef, actual_rigfile2, actual_rigfile3, actual_saafile, par.bintime); 

  mjdrefi = getKeyValLLong(fp_out,"MJDREFI");
  mjdreff = getKeyValDbl(fp_out,"MJDREFF");

  writeKeyValDbl(fp_out,"RA_NOM",(**mean_p).ra,"[deg] Nominal aspect point R.A.");
  writeKeyValDbl(fp_out,"DEC_NOM",(**mean_p).dec,"[deg] Nominal aspect point Dec");
  writeKeyValDbl(fp_out,"PA_NOM",(**mean_p).roll,"Nominal aspect point Roll");

  ahapp::writeParametersToLog();

}

// ****************************************************************************

void doWork(const Param & par,
            GENATTFILE * att_descrip,
            int mjdrefi, double mjdreff,
            GENORBFILE * orb_descrip,
            ahfits::FilePtr fp_ref, ahfits::FilePtr fp_out,
            ahtime::leapsec::LeapSecTable & leapsec_data,
            AtRigData2 * rigidity2_data,
            AtRigData2 * rigidity3_data,
            SAADataVec & saa_data,
            SAA2DataVec & saa2_data,
            TELDEF2 * teldef_data,
            POINTING * mean_p, ALIGN * align) {

  std::stringstream msg; 

  double ref_time=0., mission_time=0., t0=0., t1=0., last_mission_time=0.;
  double mission_time_0=0., mission_time_1=0.;
  double dlt_ra=0., dlt_dec=0., dlt_roll=0., ang_dist_rad=0., ang_dist=0.;
  long irow=0, nrow=0;
  long yyyymmdd=0, hhmmss=0;
  bool time_reset_flag=false, use_reference_file=false;
  int status=0;
  
  // Datatypes used by coordfits library
  QUAT q;
  EULER ea;
  ROTMATRIX rm;
  ORBPOSITION* orbpos = allocateOrbPosition();
  POINTING p;
 
  // Datatypes used by atFunctions library
  AtVect unit_vec, unit_vec_mz, mean_vec;
  AtVect vSun, nvSun;
  AtVect vSat, vSatG;
  AtVect vMag, vect, vEarth;
  AtPolarVect pvSatG, pvEarth, geomag;

  // Declarations for the SAA & geomagnetic computations
  double center_x=0.,center_y=0.,incr_x=0.,incr_y=0.;
  double hx1_optx_sky=0., hx1_opty_sky=0.;
  double hx2_optx_sky=0., hx2_opty_sky=0.;
  double sxs_optx_sky=0., sxs_opty_sky=0.;
  double sxi_optx_sky=0., sxi_opty_sky=0.;
  double hx1_ra_pnt=0., hx1_dec_pnt=0.;
  double hx2_ra_pnt=0., hx2_dec_pnt=0.;
  double sxs_ra_pnt=0., sxs_dec_pnt=0.;
  double sxi_ra_pnt=0., sxi_dec_pnt=0.;
  double zeroes[3] = { 0. };
  char coordtype[] = "-TAN";
  double sat_alt=0., sat_lon=0., sat_lat=0., lon=0., lat=0.;
  double sun_alt=0., alt0, alt1=0.;
  double last_sun_alt=0., t_dy_nt=0., tn_dy_nt=0., last_dy_nt_time=0., next_dy_nt_time=0.;
  double mjd_utc=0.;
  double mjd_utc_0=0.;
  double cor=0., cor2=0., cor3=0., zgmag_ang=0., zgmag_phi=0., ze_ang=0., ze_phi=0.;
  float cortime=0.;
  double elev[3]={0.,0.,0.};
  double elev_mz[3]={0.,0.,0.};
  double elv=0., dye_elv=0., nte_elv=0.;
  double mzelv=0., mzdye_elv=0., mznte_elv=0.;
  int occult=0;
  int day_night=0;

  // Variables for output table data, all scalar
  double euler1=0., euler2=0., euler3=0.;
  double quat1=0., quat2=0., quat3=0., quat4=0.;

  // XYZMAG rigidity calculation variables
  // Not used: b0mag, macl, lat0, earthvec
  float tjd;
  float b0mag;
  float macl;
  float lat0;
  float earthvec[4];   /* longitude, latitude, altitude, local time */
  float orbpos_flt[3]; // copy of the orbpos0 pointing as floats

  // Set up connections between local variable and FITS output columns
  ahfits::Router rt(fp_out);
  rt.connectScalar(ahfits::e_WRITEONLY, "TIME", mission_time);
  rt.connectScalar(ahfits::e_WRITEONLY, "YYYYMMDD", yyyymmdd);
  rt.connectScalar(ahfits::e_WRITEONLY, "HHMMSS", hhmmss);
  rt.connectScalar(ahfits::e_WRITEONLY, "EULER1", euler1);
  rt.connectScalar(ahfits::e_WRITEONLY, "EULER2", euler2);
  rt.connectScalar(ahfits::e_WRITEONLY, "EULER3", euler3);
  rt.connectScalar(ahfits::e_WRITEONLY, "QUAT1", quat1);
  rt.connectScalar(ahfits::e_WRITEONLY, "QUAT2", quat2);
  rt.connectScalar(ahfits::e_WRITEONLY, "QUAT3", quat3);
  rt.connectScalar(ahfits::e_WRITEONLY, "QUAT4", quat4);
  rt.connectScalar(ahfits::e_WRITEONLY, "RA", p.ra);
  rt.connectScalar(ahfits::e_WRITEONLY, "DEC", p.dec);
  rt.connectScalar(ahfits::e_WRITEONLY, "ROLL", p.roll);
  rt.connectScalar(ahfits::e_WRITEONLY, "HX1_RA_PNT", hx1_ra_pnt);
  rt.connectScalar(ahfits::e_WRITEONLY, "HX1_DEC_PNT", hx1_dec_pnt);
  rt.connectScalar(ahfits::e_WRITEONLY, "HX2_RA_PNT", hx2_ra_pnt);
  rt.connectScalar(ahfits::e_WRITEONLY, "HX2_DEC_PNT", hx2_dec_pnt);
  rt.connectScalar(ahfits::e_WRITEONLY, "SXS_RA_PNT", sxs_ra_pnt);
  rt.connectScalar(ahfits::e_WRITEONLY, "SXS_DEC_PNT", sxs_dec_pnt);
  rt.connectScalar(ahfits::e_WRITEONLY, "SXI_RA_PNT", sxi_ra_pnt);
  rt.connectScalar(ahfits::e_WRITEONLY, "SXI_DEC_PNT", sxi_dec_pnt);
  rt.connectScalar(ahfits::e_WRITEONLY, "DLT_RA", dlt_ra);
  rt.connectScalar(ahfits::e_WRITEONLY, "DLT_DEC", dlt_dec);
  rt.connectScalar(ahfits::e_WRITEONLY, "DLT_ROLL", dlt_roll);
  rt.connectScalar(ahfits::e_WRITEONLY, "ANG_DIST", ang_dist);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAT_ALT", sat_alt);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAT_LON", sat_lon);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAT_LAT", sat_lat);
  rt.connectScalar(ahfits::e_WRITEONLY, "ELV", elv);
  rt.connectScalar(ahfits::e_WRITEONLY, "DYE_ELV", dye_elv);
  rt.connectScalar(ahfits::e_WRITEONLY, "NTE_ELV", nte_elv);
  rt.connectScalar(ahfits::e_WRITEONLY, "SUN_ALT", sun_alt);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_DY_NT", t_dy_nt);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_DY_NT", tn_dy_nt);
  rt.connectScalar(ahfits::e_WRITEONLY, "COR", cor);
  rt.connectScalar(ahfits::e_WRITEONLY, "COR2", cor2);
  rt.connectScalar(ahfits::e_WRITEONLY, "COR3", cor3);
  rt.connectScalar(ahfits::e_WRITEONLY, "CORTIME", cortime);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA", saa_data[0].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA", saa_data[0].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA", saa_data[0].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA_HXD", saa_data[1].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA_HXD", saa_data[1].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA_HXD", saa_data[1].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA_HXI1", saa_data[2].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA_HXI1", saa_data[2].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA_HXI1", saa_data[2].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA_HXI2", saa_data[3].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA_HXI2", saa_data[3].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA_HXI2", saa_data[3].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA_SGD1", saa_data[4].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA_SGD1", saa_data[4].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA_SGD1", saa_data[4].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA_SGD2", saa_data[5].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA_SGD2", saa_data[5].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA_SGD2", saa_data[5].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA_SXI", saa_data[6].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA_SXI", saa_data[6].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA_SXI", saa_data[6].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA_SXS", saa_data[7].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA_SXS", saa_data[7].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA_SXS", saa_data[7].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA2_HXI1", saa2_data[0].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA2_HXI1", saa2_data[0].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA2_HXI1", saa2_data[0].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA2_HXI2", saa2_data[1].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA2_HXI2", saa2_data[1].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA2_HXI2", saa2_data[1].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA2_SGD1", saa2_data[2].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA2_SGD1", saa2_data[2].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA2_SGD1", saa2_data[2].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA2_SGD2", saa2_data[3].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA2_SGD2", saa2_data[3].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA2_SGD2", saa2_data[3].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA2_SXI", saa2_data[4].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA2_SXI", saa2_data[4].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA2_SXI", saa2_data[4].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "SAA2_SXS", saa2_data[5].m_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "T_SAA2_SXS", saa2_data[5].m_t_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "TN_SAA2_SXS", saa2_data[5].m_tn_saa);
  rt.connectScalar(ahfits::e_WRITEONLY, "ZGMAG_ANG", zgmag_ang);
  rt.connectScalar(ahfits::e_WRITEONLY, "ZGMAG_PHI", zgmag_phi);
  rt.connectScalar(ahfits::e_WRITEONLY, "ZE_ANG", ze_ang);
  rt.connectScalar(ahfits::e_WRITEONLY, "ZE_PHI", ze_phi);
  rt.connectScalar(ahfits::e_WRITEONLY, "MZELV", mzelv);
  rt.connectScalar(ahfits::e_WRITEONLY, "MZDYE_ELV", mzdye_elv);
  rt.connectScalar(ahfits::e_WRITEONLY, "MZNTE_ELV", mznte_elv);

  // Time setup
  ahtime::AhMJDTime epoch_mjd(mjdrefi);
  ahtime::AhDateTime epoch_utc;
  ahtime::reformatMJDAsDateTime(epoch_mjd,epoch_utc);
  AH_INFO(ahlog::LOW) << "Mission epoch (UTC):" << std::endl;
  AH_INFO(ahlog::LOW) << "   year        = " << epoch_utc.year() << std::endl;
  AH_INFO(ahlog::LOW) << "   month       = " << epoch_utc.month() << std::endl;
  AH_INFO(ahlog::LOW) << "   day         = " << epoch_utc.day() << std::endl;
  AH_INFO(ahlog::LOW) << "   hour        = " << epoch_utc.hour() << std::endl;
  AH_INFO(ahlog::LOW) << "   minute      = " << epoch_utc.minute() << std::endl;
  AH_INFO(ahlog::LOW) << "   second      = " << epoch_utc.second() << std::endl;
  AH_INFO(ahlog::LOW) << "   microsecond = " << epoch_utc.getSubsecondAsInt(6) << std::endl;

  // Mission times in DateTime format
  ahtime::AhDateTime time_utc;
  ahtime::AhDateTime t0_utc;
  
  // Set up reference file, if required.  This is a file that supplies
  // the time vector for the outout.
  use_reference_file = ( fp_ref != 0 );
  ref_time = 0.0;
  ahfits::Router * rt_ref=0;
  if (use_reference_file) {
    AH_INFO(ahlog::HIGH) << "Using reference file time column" << std::endl;
    rt_ref = new ahfits::Router(fp_ref);
    rt_ref->connectScalar(ahfits::e_READONLY, par.timecol, ref_time);
    nrow = ahfits::getKeyValLLong(fp_ref, "NAXIS2");
  } else {
    AH_INFO(ahlog::HIGH) << "Using attitude file to calculate time column" << std::endl;
    nrow = (long)(ceil((par.tstop_actual - par.tstart_actual)/par.bintime) + 1.0);
  }

  AH_INFO(ahlog::HIGH) << "Actual time range: " << std::endl;
  msg.str("");
  msg << std::setprecision(15) << "   tstart_actual   = " << par.tstart_actual << std::endl;
  AH_INFO(ahlog::HIGH) << msg.str();
  msg.str("");
  msg << std::setprecision(15) << "   tstop_actual    = " << par.tstop_actual << std::endl;
  AH_INFO(ahlog::HIGH) << msg.str();
  AH_INFO(ahlog::HIGH) << "Number of rows in output EHK file = " << nrow << std::endl;

  if ((isInExtrapolatedAttFile(att_descrip->af, par.tstart_actual) == 0) ||
      (isInExtrapolatedAttFile(att_descrip->af, par.tstop_actual ) == 0)) {
    msg.str("");
    msg << "effective starting time = " << std::setprecision(15) << par.tstart_actual;
    AH_ERR << msg.str() << std::endl;
    msg.str("");
    msg << "effective ending time = " << std::setprecision(15) << par.tstop_actual;
    AH_ERR << msg.str() << std::endl;
    msg.str("");
    msg << "extrapolated attitude file time range = " << std::setprecision(15) <<
      att_descrip->af->min_extrapolated << " - " << att_descrip->af->max_extrapolated;
    AH_ERR << msg.str() << std::endl;
    AH_THROW_RUNTIME("Specified time range outside bounds of attitude file");
  }

  // SKY plate scale, pix/deg
  if(ahlog::get_debug()) printTelDef2(teldef_data,stdout);
  center_x = teldef_data->coordsys[teldef_data->n_coordsys-1][0]->center_x;
  center_y = teldef_data->coordsys[teldef_data->n_coordsys-1][0]->center_y;
  incr_x = -1.0*teldef_data->coordsys[teldef_data->n_coordsys-1][0]->scale_x;   // RA goes right to left
  incr_y = teldef_data->coordsys[teldef_data->n_coordsys-1][0]->scale_y;
  AH_INFO(ahlog::HIGH) << "   SKY (center_x,center_y) = " << center_x << "," << center_y << std::endl;
  AH_INFO(ahlog::HIGH) << "   SKY increment (pix/deg) (incr_x,incr_y)  = " << incr_x << "," << incr_y << std::endl;

  // Set up SKY coordinate system with nominal center.
  setSkyCoordCenterInTelDef2(teldef_data, mean_p->ra, mean_p->dec, 0.0);


  // convert mean pointing to unit vector.  this is used later to calculate
  // angular distance (ang_dist)
  atPolDegToVect(1.0, mean_p->ra, mean_p->dec, mean_vec);

  if (use_reference_file) {
    ahfits::firstRow(fp_ref);
  }
  ahfits::firstRow(fp_out);

  for (irow=0; irow<nrow; ++irow) {

    if (use_reference_file) {
      ahfits::readRow(fp_ref);
      mission_time = ref_time;
    } else {
      mission_time = irow*par.bintime + par.tstart_actual;
    }

    AH_DEBUG << "main loop irow=" << irow << " mission_time=" << mission_time << std::endl;
    AH_DEBUG << "mission_time-tstart=" << mission_time-par.tstart_actual
             << " tstop-mission_time=" << par.tstop_actual-mission_time << std::endl;

    if (irow == 0) {
      mission_time_0 = mission_time;
    }
    mission_time_1 = mission_time;

    // Convert mission_time to yyyymmdd and hhmmss
    compute_mjd_utc(mission_time,epoch_utc, leapsec_data, time_utc, mjd_utc);
    yyyymmdd=time_utc.getDateAsInt();
    hhmmss=time_utc.getTimeAsInt();

    //  For many of the remaining computations that use atFunctions,
    //  MJD(UTC) is required.  

    //  time_reset_flag is true if a backward search in the orbit file is
    //  required.  This is true 
    //    (1) for the first row of the attitude file;
    //    (2) if the time vector of the attitude file takes a backward step;
    //    (3) if the time between rows in the attitude file jumps by
    //        10 minutes or more.
    //  This is a robustness strategy.  In general, each crossing of a 
    //  boundary by the satellite (e.g., the day/night boundary or the edge
    //  of the SAA region) is saved to be used subsequently as the "previous
    //  crossing."  But conditions (2) and (3) are regarded as invalidating 
    //  the saved crossing data, so that the "previous crossing" has to be
    //  found again by a backward search.
    time_reset_flag = false;
    if (irow == 0) {
      t0 = mission_time;
      time_reset_flag = true;
    } else if ( mission_time < last_mission_time ) {      // time inversion
      AH_OUT << "Time " << mission_time << " at row " << irow << " is earlier than time in previous row (" << last_mission_time << ")" << std::endl;
      time_reset_flag = true;
    } else if ( 600.0 < mission_time - last_mission_time ) {  // time jump of 10 min
      time_reset_flag = true;
    }

    t1 = mission_time;

    if ( 0 == findQuatInGenAttFile(att_descrip, &q, mission_time) ) {
      printGenAttFile(att_descrip, stdout);
      msg.str("");
      msg << "Time " << std::setprecision(15) << mission_time << " not found in attitude file";
      AH_THROW_RUNTIME(msg.str());
    }

    //  Copy quaternion to the variables that are connected to the output FITS file.
    quat1 = q.p[0];
    quat2 = q.p[1];
    quat3 = q.p[2];
    quat4 = q.p[3];
    
    //  Convert quaternion to other forms of the attitude for the output FITS file.
    convertQuatToEuler(&ea, &q);
    euler1 = ea.phi*180.0/M_PI;
    euler2 = ea.theta*180.0/M_PI;
    euler3 = ea.psi*180.0/M_PI;
    if (euler1 < 0) euler1 += 360.0;
    if (euler2 < 0) euler2 += 360.0;
    if (euler3 < 0) euler3 += 360.0;

    convertQuatToPointing(&p,&q,align);
    
    // Compute delta pointing columns DLT_RA, DLT_DEC, and DLT_ROLL, 
    // using p and mean pointing from keywords
    // DLT_RA and _DEC are in arcmin
    // delta_phi() is a short utility routine that finds a difference between 
    // angles in degress and forces it into the range -180 to +180 
    dlt_ra = 60.0*delta_phi(p.ra, mean_p->ra);
    dlt_dec = 60.0*(p.dec - mean_p->dec);      
    dlt_roll = delta_phi(p.roll, mean_p->roll);

    // Compute ANG_DIST using mean pointing and the FOC current pointing 
    atPolDegToVect(1.0, p.ra, p.dec, unit_vec);
    atAngDistance(unit_vec, mean_vec, &ang_dist_rad);
    ang_dist = 60.0*ang_dist_rad*180.0/M_PI;

    // Compute optical axis coordinates for each instrument

    // Compute unit vector of minus Z direction
    for (int ii=0;ii<3;ii++) {
      unit_vec_mz[ii] = -1. * unit_vec[ii];
    }

    AH_DEBUG << "unit_vec " << unit_vec[0] << " " << unit_vec[1] << " " << unit_vec[2] << std::endl;
    AH_DEBUG << "unit_vec_mz " << unit_vec_mz[0] << " " << unit_vec_mz[1] << " " << unit_vec_mz[2] << std::endl;

    // NOTE ON ATTITUDE: No aberration correction is done here.
    // Last two args to convertToHigherCoordSkyAtt are velocities
    // and are set to zero.

    // HX1-1
    convertToHigherCoordSkyAtt(teldef_data,par.hx1_optx,par.hx1_opty,
        &hx1_optx_sky,&hx1_opty_sky,teldef_data->n_coordsys-2,&q,0.0,zeroes);

    fits_pix_to_world(hx1_optx_sky,hx1_opty_sky,mean_p->ra,mean_p->dec,
        center_x,center_y,incr_x,incr_y,0.0,coordtype,&hx1_ra_pnt,&hx1_dec_pnt,&status);

    // HX1-2
    convertToHigherCoordSkyAtt(teldef_data,par.hx2_optx,par.hx2_opty,
        &hx2_optx_sky,&hx2_opty_sky,teldef_data->n_coordsys-2,&q,0.0,zeroes);

    fits_pix_to_world(hx2_optx_sky,hx2_opty_sky,mean_p->ra,mean_p->dec,
        center_x,center_y,incr_x,incr_y,0.0,coordtype,&hx2_ra_pnt,&hx2_dec_pnt,&status);

    // SXS
    convertToHigherCoordSkyAtt(teldef_data,par.sxs_optx,par.sxs_opty,
        &sxs_optx_sky,&sxs_opty_sky,teldef_data->n_coordsys-2,&q,0.0,zeroes);

    fits_pix_to_world(sxs_optx_sky,sxs_opty_sky,mean_p->ra,mean_p->dec,
        center_x,center_y,incr_x,incr_y,0.0,coordtype,&sxs_ra_pnt,&sxs_dec_pnt,&status);

    // SXI
    convertToHigherCoordSkyAtt(teldef_data,par.sxi_optx,par.sxi_opty,
        &sxi_optx_sky,&sxi_opty_sky,teldef_data->n_coordsys-2,&q,0.0,zeroes);

    fits_pix_to_world(sxi_optx_sky,sxi_opty_sky,mean_p->ra,mean_p->dec,
        center_x,center_y,incr_x,incr_y,0.0,coordtype,&sxi_ra_pnt,&sxi_dec_pnt,&status);

    if(status != 0) {
      AH_THROW_RUNTIME("Error calculating optical axis coordinates");
    }
 
    // Compute geographic coordinates (coordinates on the ellipsoid) of the
    // satellite track -- SAT_ALT, SAT_LON, SAT_LAT:

    // Get orbital position from the orbit file, interpolating in
    // time if necessary.
    findOrbPositionInGenOrbFile(orb_descrip, orbpos, mission_time);
    copy_atvect_from_orbpos(vSat, orbpos);
    atGeodcr(mjd_utc, vSat, &sat_alt, &lon, &lat);
    sat_lon = lon*180.0/M_PI;
    sat_lat = lat*180.0/M_PI;

    // Compute elevation = angular distance between target and Earth limb
    // as seen from satellite:
    atSun(mjd_utc, vSun);
    atEarthElev(vSat, unit_vec, vSun, &occult, elev);
    elv = elev[0] * 180.0/M_PI;

    // Compute dayside/nightside elevation = angular distance between target 
    // and bright/dark part of the Earth, as seen from satellite.  If the target
    // is on the day side of the Earth, then dye_elv = elv, and nte_elv is
    // the angular distrance from the target to the terminator, and vice
    // versa if the target is on the night side of the Earth:
    dye_elv = elev[1] * 180.0/M_PI;
    nte_elv = elev[2] * 180.0/M_PI;
    atNormVect(vSun, nvSun);
    atEarthOccult(vSat, nvSun, vSun, &day_night, &sun_alt);
    sun_alt = sun_alt * 180.0/M_PI;

    // Compute elevation and dayside/nightside elevation for the minus Z direction, 
    // in the same way as above.
    atEarthElev(vSat, unit_vec_mz, vSun, &occult, elev_mz);
    mzelv = elev_mz[0] * 180.0/M_PI;
    mzdye_elv = elev_mz[1] * 180.0/M_PI;
    mznte_elv = elev_mz[2] * 180.0/M_PI;

    // Compute time in orbit previous day/night boundary and time until next day/night
    // boundary, from the sun position and the orbital position.
    if ( time_reset_flag ) {

      // Last day/night boundary.
      t0 = mission_time;
      mjd_utc_0 = mjd_utc;
      alt0 = sun_alt;
      do {
        t1 = t0;
        t0 = t1 - 60.0;
        alt1 = alt0;
        computeDayNightAltitude(t0,epoch_utc,leapsec_data,orb_descrip,day_night,alt0);
      } while ( 0 < alt0 * alt1 );
      last_dy_nt_time = (alt1*t0 - alt0*t1) / (alt1 - alt0);

      // Next day/night boundary.
      t0 = mission_time;
      alt0 = sun_alt;
      do {
        t1 = t0;
        alt1 = alt0;
        t0 = t1 + 60.0;
        computeDayNightAltitude(t0,epoch_utc,leapsec_data,orb_descrip,day_night,alt0);
      } while ( 0 < alt0 * alt1 );
      next_dy_nt_time = (alt1*t0 - alt0*t1) / (alt1 - alt0);

    } else if ( last_sun_alt * sun_alt <= 0.0 ) {

      // Last day/night boundary.
      t1 = mission_time;
      alt1 = sun_alt;
      t0 = last_mission_time;
      alt0 = last_sun_alt;
      last_dy_nt_time = (alt1*t0 - alt0*t1) / (alt1 - alt0);

      // Next day/night boundary.
      t0 = mission_time;
      alt0 = sun_alt;
      do {
        t1 = t0;
        alt1 = alt0;
        t0 = t1 + 60.0;
        computeDayNightAltitude(t0,epoch_utc,leapsec_data,orb_descrip,day_night,alt0);
      } while ( 0 < alt0 * alt1 );
      next_dy_nt_time = (alt1*t0 - alt0*t1) / (alt1 - alt0);
    }

    // these are the output columns: SUN_ALT, T_DY_NT, TN_DY_NT
    last_sun_alt = sun_alt;
    t_dy_nt = mission_time - last_dy_nt_time;
    tn_dy_nt = next_dy_nt_time - mission_time;

    // Compute cutoff rigidity using oldest model, which is a function of geodetic
    // lat, lon, and altitude (COR):
    atGeodetic(mjd_utc, vSat, vSatG);
    atVectToPol(vSatG, &pvSatG);
    atRigidityD(&pvSatG, &cor);

    // Compute cutoff rigidity using a newer model (20060421), which is also a function of geodetic
    // lat, lon, and altitude (COR2):
    atRigidity2(rigidity2_data, &pvSatG, &cor2);

    // Compute cutoff rigidity using a newer model (20160101), which is also a function of geodetic
    // lat, lon, and altitude (COR3):
    atRigidity2(rigidity3_data, &pvSatG, &cor3);

    // Compute cutoff rigidity using the fortran xyzmag function (similar to prefilter)
    tjd = mjd_utc-40000;
    for (int i=0; i<3; i++) { orbpos_flt[i] = orbpos->p[i]; } // need to convert orbpos from double to float
    XYZMAG(tjd, orbpos_flt[0], b0mag, macl, cortime, lat0, earthvec[0]);

    AH_DEBUG << "  COR     :" << cor << std::endl;
    AH_DEBUG << "  COR2    :" << cor2 << std::endl;
    AH_DEBUG << "  COR3    :" << cor3 << std::endl;
    AH_DEBUG << "  CORTIME :" << cortime << std::endl;

    // Compute SAA flag.  atBrazil tests for the satellite being inside a simplified
    // outline based purely on lat and lon.  atSISBrazil tests a simple outline
    // for "deep SAA".  atSTTBrazil tests a different "deep SAA" outline.

    // Loop through all of the instruments in the SAAData variable
    for(int ii = 0; ii < 8; ++ii) {
      int saa=0, saa0=0, sis_saa=0, stt_saa=0;
      int last_saa = saa_data[ii].m_last_saa;
      double last_saa_time = saa_data[ii].m_last_t_saa;
      double next_saa_time = saa_data[ii].m_next_t_saa;
      if(ii == 0) {
        // Suzaku
        atBrazil(pvSatG.lon, pvSatG.lat, &saa);
        if ( saa ) saa = 1;
        atSISBrazil(pvSatG.lon, pvSatG.lat, &sis_saa);
        if ( sis_saa ) saa = 2;
        atSTTBrazil(pvSatG.lon, pvSatG.lat, &stt_saa);
        if ( stt_saa ) saa = 3;
      } else if( ii == 1 ) {
        // HXD 
        atHXDBrazil(pvSatG.lon, pvSatG.lat, &saa);
        if ( saa ) saa = 1;
      } else {
        // The longitude values in the SAA CALDB file are in the range 
        // [-180 : +180].  However the pvSatG longitude values are in 
        // the range [0 : 360].  So, we need to covert the pvSatG longitude
        // to be consistent with the SAA data.
        // double lon_shift=pvSatG.lon;
        // if (lon_shift > M_PI) lon_shift-=2.*M_PI;

        // Hitomi
        // atPolarVect arrays are output in radians, need to convert them into degrees.
        // In the atFunctions, conversion is done within the function
        if(ahmath::isPointInsidePolygon(RAD2DEG*pvSatG.lat, RAD2DEG*pvSatG.lon, saa_data[ii].m_saaPolygon)) {
          saa = 1;
        } else if(ahmath::isPointInsidePolygon(RAD2DEG*pvSatG.lat, RAD2DEG*pvSatG.lon-360.0, saa_data[ii].m_saaPolygon)) {
          saa = 1;
        }
      }
      saa_data[ii].m_saa = saa;

      // Compute T_SAA=time since last SAA and TN_SAA = time until next SAA. This is
      // a brute force coarse search along the orbit path with 60 second resolution,
      // followed by a fine search with 1 second resolution:
      if ( time_reset_flag ) {

        // search backward
        computeSAATimeBoundary(mission_time,true,false,saa_data[ii],epoch_utc,
                           leapsec_data,orb_descrip,0,last_saa_time);

        // search forward
        computeSAATimeBoundary(mission_time,false,false,saa_data[ii],epoch_utc,
                           leapsec_data,orb_descrip,0,next_saa_time);

        // save the next and last SAA boundary times for the next row iteration

      } else if( 0 == saa && last_saa ) {

        // search backward and skip coarse time steps
        saa0 = last_saa;
        computeSAATimeBoundary(last_mission_time,true,true,saa_data[ii],epoch_utc,
                           leapsec_data,orb_descrip,saa0,last_saa_time);

        // search forward
        computeSAATimeBoundary(mission_time,false,false,saa_data[ii],epoch_utc,
                           leapsec_data,orb_descrip,0,next_saa_time);

        // save the next and last SAA boundary times for the next row iteration

      }

      saa_data[ii].m_last_saa = saa_data[ii].m_saa;
      saa_data[ii].m_last_t_saa = last_saa_time;
      saa_data[ii].m_next_t_saa = next_saa_time;
      if ( saa_data[ii].m_saa ) {
        saa_data[ii].m_t_saa = 0.0;
        saa_data[ii].m_tn_saa = 0.0;
      } else {
        saa_data[ii].m_t_saa = mission_time - last_saa_time;
        saa_data[ii].m_tn_saa = next_saa_time - mission_time;
      }
      
    } // end loop over SAA data


    // Loop through all of the instruments in the SAA2Data variable
    for(int ii = 0; ii < 6; ++ii) {

      int num_pieces = saa2_data[ii].m_saa_data.size();

      for (int jj=0; jj<num_pieces; ++jj) {

        int saa=0, saa0=0;
        int last_saa = saa2_data[ii].m_saa_data[jj].m_last_saa;
        double last_saa_time = saa2_data[ii].m_saa_data[jj].m_last_t_saa;
        double next_saa_time = saa2_data[ii].m_saa_data[jj].m_next_t_saa;

        // The longitude values in the SAA CALDB file are in the range 
        // [-180 : +180].  However the pvSatG longitude values are in 
        // the range [0 : 360].  So, we need to convert the pvSatG longitude
        // to be consistent with the SAA data.
        // double lon_shift=pvSatG.lon;
        // if (lon_shift > M_PI) lon_shift-=2.*M_PI;

        // Hitomi
        // atPolarVect arrays are output in radians, need to convert them into degrees.
        // In the atFunctions, conversion is done within the function
        if(ahmath::isPointInsidePolygon(RAD2DEG*pvSatG.lat, RAD2DEG*pvSatG.lon, 
          saa2_data[ii].m_saa_data[jj].m_saaPolygon)) {
          saa = 1;
        } else if(ahmath::isPointInsidePolygon(RAD2DEG*pvSatG.lat, RAD2DEG*pvSatG.lon-360.0, 
          saa2_data[ii].m_saa_data[jj].m_saaPolygon)) {
          saa = 1;
        }

        saa2_data[ii].m_saa_data[jj].m_saa = saa;

        // Compute T_SAA=time since last SAA and TN_SAA = time until next SAA. This is
        // a brute force coarse search along the orbit path with 60 second resolution,
        // followed by a fine search with 1 second resolution:
        if ( time_reset_flag ) {

          // search backward
          computeSAATimeBoundary(mission_time,true,false,saa2_data[ii].m_saa_data[jj],epoch_utc,
                             leapsec_data,orb_descrip,0,last_saa_time);

          // search forward
          computeSAATimeBoundary(mission_time,false,false,saa2_data[ii].m_saa_data[jj],epoch_utc,
                             leapsec_data,orb_descrip,0,next_saa_time);

          // save the next and last SAA boundary times for the next row iteration

        } else if( 0 == saa && last_saa ) {

          // search backward and skip coarse time steps
          saa0 = last_saa;
          computeSAATimeBoundary(last_mission_time,true,true,saa2_data[ii].m_saa_data[jj],epoch_utc,
                             leapsec_data,orb_descrip,saa0,last_saa_time);

          // search forward
          computeSAATimeBoundary(mission_time,false,false,saa2_data[ii].m_saa_data[jj],epoch_utc,
                             leapsec_data,orb_descrip,0,next_saa_time);

          // save the next and last SAA boundary times for the next row iteration

        }

        saa2_data[ii].m_saa_data[jj].m_last_saa = saa2_data[ii].m_saa_data[jj].m_saa;
        saa2_data[ii].m_saa_data[jj].m_last_t_saa = last_saa_time;
        saa2_data[ii].m_saa_data[jj].m_next_t_saa = next_saa_time;
        if ( saa2_data[ii].m_saa_data[jj].m_saa ) {
          saa2_data[ii].m_saa_data[jj].m_t_saa = 0.0;
          saa2_data[ii].m_saa_data[jj].m_tn_saa = 0.0;
        } else {
          saa2_data[ii].m_saa_data[jj].m_t_saa = mission_time - last_saa_time;
          saa2_data[ii].m_saa_data[jj].m_tn_saa = next_saa_time - mission_time;
        }
      }

      // Compute composite quantities
      saa2_data[ii].m_saa = saa2_data[ii].m_saa_data[0].m_saa;
      saa2_data[ii].m_t_saa = saa2_data[ii].m_saa_data[0].m_t_saa;
      saa2_data[ii].m_tn_saa = saa2_data[ii].m_saa_data[0].m_tn_saa;
      for (int jj=1; jj<num_pieces; ++jj) {

        // This is a logical OR -- if the pointing is in any of the SAA
        // pieces, it's in the SAA
        if (0 == saa2_data[ii].m_saa) {
          saa2_data[ii].m_saa = saa2_data[ii].m_saa_data[jj].m_saa;
        }

        // Each interval (time since last SAA or time before next SAA)
        // is the minimum over the SAA pieces
        if (saa2_data[ii].m_saa_data[jj].m_t_saa < saa2_data[ii].m_t_saa) {
          saa2_data[ii].m_t_saa = saa2_data[ii].m_saa_data[jj].m_t_saa;
        }
        if (saa2_data[ii].m_saa_data[jj].m_tn_saa < saa2_data[ii].m_tn_saa) {
          saa2_data[ii].m_tn_saa = saa2_data[ii].m_saa_data[jj].m_tn_saa;
        }

      }
      
    } // end loop over SAA2 data

    // Compute geomagnetic field using a spherical harmonic decomposition, coefficients
    // IGRF 2005 (ZGMAG_ANG, ZGMAG_PHI):

    atGeomagSet(mjd_utc, 8);
    // nmax=8 recommended for atFunctions <= v2.7, nmax ignored >= v2.8
    atGeomag(&pvSatG, vSat, vMag);
    //atEulerToRM(&ea, rm);
    convertEulerToRotMatrix(&rm, &ea);
    //atRotVect(rm, vMag, vect);
    applyRotMatrixToVector(&rm, vect, vMag);
    atVectToPol(vect, &geomag);
    zgmag_ang = geomag.lat * 180.0/M_PI + 90.0;
    zgmag_phi = geomag.lon * 180.0/M_PI;

    // Compute angle between target and center of the Earth (ZE_ANG) and longitude of
    // Earth in a frame with the target at the +Z axis (ZE_PHI):

    atInvVect(vSat, vEarth);
    applyRotMatrixToVector(&rm, vect, vEarth);
    atVectToPol(vect, &pvEarth);
    ze_ang = 90.0 - pvEarth.lat * 180.0/M_PI;  // 0.0 if seeing Earth center
    ze_phi = pvEarth.lon * 180.0/M_PI;

    ahfits::writeRow(fp_out);

    if (irow < (nrow-1)) {
      ahfits::nextRow(fp_out);
      if (use_reference_file) {
        ahfits::nextRow(fp_ref);
      }
    }

    last_mission_time = mission_time;
   
  }
  AH_OUT << "Wrote " << nrow << " rows" << std::endl;

  ahfits::writeKeyValDbl(fp_out, "TSTART", mission_time_0, "Start time");
  ahfits::writeKeyValDbl(fp_out, "TSTOP" , mission_time_1, "Stop time");

  if (use_reference_file) {
    delete rt_ref;
  }

  return;
}

// ****************************************************************************

void finalize(ahtime::leapsec::LeapSecTable & leapsec_data,
              AtRigData2 * rigidity2_data,
              AtRigData2 * rigidity3_data,
              POINTING * mean_p,
              TELDEF2 * teldef_data,
              GENORBFILE * orb_descrip,
              GENATTFILE * att_descrip,
              ahfits::FilePtr & fp_ref, ahfits::FilePtr & fp_out) {

  ahfits::close(fp_out);
  AH_INFO(ahlog::LOW) << "Output file closed" << std::endl;
  if(fp_ref != 0) {
    ahfits::close(fp_ref);
    AH_INFO(ahlog::LOW) << "Reference file closed" << std::endl;
  }

  ahtime::leapsec::clear(leapsec_data);
  AH_INFO(ahlog::LOW) << "Leap second data cleared" << std::endl;
  atRigFree2(rigidity2_data);
  AH_INFO(ahlog::LOW) << "Rigidity2 data cleared" << std::endl;
  atRigFree2(rigidity3_data);
  AH_INFO(ahlog::LOW) << "Rigidity3 data cleared" << std::endl;
  // +++ 2015-05-04 AS: quaternion data is never set, so a segfault occurs. No need to destroy alignment struct
  //destroyAlign(&align);
  if(mean_p != NULL) {
    free(mean_p);
    AH_INFO(ahlog::LOW) << "Mean pointing data cleared" << std::endl;
  }
  destroyTelDef2(teldef_data);
  AH_INFO(ahlog::LOW) << "TELDEF data cleared" << std::endl;
  closeGenOrbFile(orb_descrip);
  AH_INFO(ahlog::LOW) << "Orbit file closed" << std::endl;
  closeGenAttFile(att_descrip);
  AH_INFO(ahlog::LOW) << "Attitude file closed" << std::endl;

}

// ****************************************************************************

void load_saa_data(std::string & saafile, SAADataVec & saa_data) {

  // Initialize local variables
  double l_hx1_saa[2] = { -1000.0 };
  double l_hx2_saa[2] = { -1000.0 };
  double l_sg1_saa[2] = { -1000.0 };
  double l_sg2_saa[2] = { -1000.0 };
  double l_sxi_saa[2] = { -1000.0 };
  double l_sxs_saa[2] = { -1000.0 };

  int hx1_row = 0;
  int hx2_row = 0;
  int sg1_row = 0;
  int sg2_row = 0;
  int sxi_row = 0;
  int sxs_row = 0;

  std::stringstream msg;

  ahfits::FilePtr fpsaa; // File pointer for passed FITS file

  // Hard code instruments to vector
  saa_data[0].m_instrument = "SUZAKU";
  saa_data[1].m_instrument = "HXD";
  saa_data[2].m_instrument = "HX1";
  saa_data[3].m_instrument = "HX2";
  saa_data[4].m_instrument = "SG1";
  saa_data[5].m_instrument = "SG2";
  saa_data[6].m_instrument = "SXI";
  saa_data[7].m_instrument = "SXS";

  // Set atFunctions variable for Suzaku and HXD
  // Only the useAtFunctions member variable is used for Suzaku and HXD
  saa_data[0].m_useAtFunctions = true;
  saa_data[1].m_useAtFunctions = true;

  // Open SAA file
  AH_INFO(ahlog::HIGH) << "Opening SAA data file" << std::endl;
  ahfits::open(saafile,"SAA_VERTICES",&fpsaa);
  if(!ahfits::readOK(fpsaa)) {
    AH_THROW_RUNTIME("failed to open SAA file");
  }

  // setup router
  ahfits::Router router(fpsaa); // Router creation for FITS file
  router.connectFixedLengthArray(ahfits::e_READONLY, "HXI1_SAA", l_hx1_saa);
  router.connectFixedLengthArray(ahfits::e_READONLY, "HXI2_SAA", l_hx2_saa);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SGD1_SAA", l_sg1_saa);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SGD2_SAA", l_sg2_saa);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SXI_SAA", l_sxi_saa);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SXS_SAA", l_sxs_saa);

  // Loop over SAA file row
  for(ahfits::firstRow(fpsaa); ahfits::readOK(fpsaa); ahfits::nextRow(fpsaa)) {

    ahfits::readRow(fpsaa);
    // if latitude or longitude are -1000, set to minimum lat or lon

    // Add point to HXI1 SAA data
    if(l_hx1_saa[0] != -1000.0 || l_hx1_saa[1] != -1000.0) { 
      if(l_hx1_saa[0] == -1000.0) l_hx1_saa[0] = -90;
      if(l_hx1_saa[1] == -1000.0) l_hx1_saa[1] = -180;
      if(l_hx1_saa[0] < -90 || l_hx1_saa[0] > 90 || l_hx1_saa[1] < -180 || l_hx1_saa[1] > 180) {
        msg.str("");
        msg << "HX1 SAA coordinates out of range (lat,lon): (" << l_hx1_saa[0] << "," << l_hx1_saa[1] << ")";
        msg <<  " (" << saafile << ": row " << ahfits::currentRow(fpsaa) << ")";
        AH_THROW_RUNTIME(msg.str());
      }
      ahmath::addPointToPolygon(l_hx1_saa[0], l_hx1_saa[1], saa_data[2].m_saaPolygon);
      hx1_row++;
    }

    // Add point to HXI2 SAA data
    if(l_hx2_saa[0] != -1000.0 || l_hx2_saa[1] != -1000.0) {
      if(l_hx2_saa[0] == -1000.0) l_hx2_saa[0] = -90;
      if(l_hx2_saa[1] == -1000.0) l_hx2_saa[1] = -180;
      if(l_hx2_saa[0] < -90 || l_hx2_saa[0] > 90 || l_hx2_saa[1] < -180 || l_hx2_saa[1] > 180) {
        msg.str("");
        msg << "HX2 SAA coordinates out of range (lat,lon): (" << l_hx2_saa[0] << "," << l_hx2_saa[1] << ")";
        msg <<  " (" << saafile << ": row " << ahfits::currentRow(fpsaa) << ")";
        AH_THROW_RUNTIME(msg.str());
      }
      ahmath::addPointToPolygon(l_hx2_saa[0], l_hx2_saa[1], saa_data[3].m_saaPolygon);
      hx2_row++;
    }

    // Add point to SGD1 SAA data
    if(l_sg1_saa[0] != -1000.0 || l_sg1_saa[1] != -1000.0) {
      if(l_sg1_saa[0] == -1000.0) l_sg1_saa[0] = -90;
      if(l_sg1_saa[1] == -1000.0) l_sg1_saa[1] = -180;
      if(l_sg1_saa[0] < -90 || l_sg1_saa[0] > 90 || l_sg1_saa[1] < -180 || l_sg1_saa[1] > 180) {
        msg.str("");
        msg << "SG1 SAA coordinates out of range (lat,lon): (" << l_sg1_saa[0] << "," << l_sg1_saa[1] << ")";
        msg <<  " (" << saafile << ": row " << ahfits::currentRow(fpsaa) << ")";
        AH_THROW_RUNTIME(msg.str());
      }
      ahmath::addPointToPolygon(l_sg1_saa[0], l_sg1_saa[1], saa_data[4].m_saaPolygon);
      sg1_row++;
    }

    // Add point to SGD2 SAA data
    if(l_sg2_saa[0] != -1000.0 || l_sg2_saa[1] != -1000.0) {
      if(l_sg2_saa[0] == -1000.0) l_sg2_saa[0] = -90;
      if(l_sg2_saa[1] == -1000.0) l_sg2_saa[1] = -180;
      if(l_sg2_saa[0] < -90 || l_sg2_saa[0] > 90 || l_sg2_saa[1] < -180 || l_sg2_saa[1] > 180) {
        msg.str("");
        msg << "SG2 SAA coordinates out of range (lat,lon): (" << l_sg2_saa[0] << "," << l_sg2_saa[1] << ")";
        msg <<  " (" << saafile << ": row " << ahfits::currentRow(fpsaa) << ")";
        AH_THROW_RUNTIME(msg.str());
      }
      ahmath::addPointToPolygon(l_sg2_saa[0], l_sg2_saa[1], saa_data[5].m_saaPolygon);
      sg2_row++;
    }

    // Add point to SXI SAA data
    if(l_sxi_saa[0] != -1000.0 || l_sxi_saa[1] != -1000.0) {
      if(l_sxi_saa[0] == -1000.0) l_sxi_saa[0] = -90;
      if(l_sxi_saa[1] == -1000.0) l_sxi_saa[1] = -180;
      if(l_sxi_saa[0] < -90 || l_sxi_saa[0] > 90 || l_sxi_saa[1] < -180 || l_sxi_saa[1] > 180) {
        msg.str("");
        msg << "SXI SAA coordinates out of range (lat,lon): (" << l_sxi_saa[0] << "," << l_sxi_saa[1] << ")";
        msg <<  " (" << saafile << ": row " << ahfits::currentRow(fpsaa) << ")";
        AH_THROW_RUNTIME(msg.str());
      }
      ahmath::addPointToPolygon(l_sxi_saa[0], l_sxi_saa[1], saa_data[6].m_saaPolygon);
      sxi_row++;
    }

    // Add point to SXS SAA data
    if(l_sxs_saa[0] != -1000.0 || l_sxs_saa[1] != -1000.0) {
      if(l_sxs_saa[0] == -1000.0) l_sxs_saa[0] = -90;
      if(l_sxs_saa[1] == -1000.0) l_sxs_saa[1] = -180;
      if(l_sxs_saa[0] < -90 || l_sxs_saa[0] > 90 || l_sxs_saa[1] < -180 || l_sxs_saa[1] > 180) {
        msg.str("");
        msg << "SXS SAA coordinates out of range  (lat,lon): (" << l_sxs_saa[0] << "," << l_sxs_saa[1] << ")";
        msg <<  " (" << saafile << ": row " << ahfits::currentRow(fpsaa) << ")";
        AH_THROW_RUNTIME(msg.str());
      }
      ahmath::addPointToPolygon(l_sxs_saa[0], l_sxs_saa[1], saa_data[7].m_saaPolygon);
      sxs_row++;
    }

  } // end loop over SAA file rows
  
  // loop over instruments 
  for(int ii = 2; ii < 8; ++ii) {

    int numPoints = 0;
    std::string nvert = saa_data[ii].m_instrument+"NVERT";
    numPoints = ahfits::getKeyValLLong(fpsaa, nvert);

    if(numPoints != saa_data[ii].m_saaPolygon.m_num_points) {
      AH_THROW_RUNTIME("Invalid number of points for instrument for instrument " + saa_data[ii].m_instrument);
    }

    saa_data[ii].m_useAtFunctions = false;

    AH_DEBUG << "Instrument: " << saa_data[ii].m_instrument << std::endl;
    for(int jj = 0; jj < numPoints; ++jj) { 
      AH_DEBUG << "  lat: " << saa_data[ii].m_saaPolygon.m_x[jj] << "  lon: " << saa_data[ii].m_saaPolygon.m_y[jj] << std::endl;
    }

    ahmath::checkPolygon(saa_data[ii].m_saaPolygon);

  } // end loop over instrument

  ahfits::close(fpsaa);

} // end load_saa_data

// ****************************************************************************

void addPointToSAA2Data(int & index, double latlon[2], const std::string inststr, 
  SAA2Data & saa2) {

  SAAData new_saa_piece;

  std::stringstream msg;

  // NaN, Nan is end marker
  if (isfinite(latlon[0]) && isfinite(latlon[1])) {
    if(std::abs(latlon[0]) != 1000.0 || std::abs(latlon[1]) != 1000.0) { 
      if(latlon[0] == -1000.0) latlon[0] = -90;
      if(latlon[0] == +1000.0) latlon[0] = +90;

      // +++ 2016-07-21 RSH This doesn't seem applicable to longitude
      //if(latlon[1] == -1000.0) latlon[1] = -180;
      //if(latlon[1] == +1000.0) latlon[1] = +180;
      //if(latlon[0] < -90 || latlon[0] > 90 || latlon[1] < -180 || latlon[1] > 180) 
      if(latlon[0] < -90 || latlon[0] > 90 || latlon[1] < -360 || latlon[1] > +360) {
        msg.str("");
        msg << inststr << " SAA2 coordinates out of range (lat,lon): (" << latlon[0] << "," << latlon[1] << ")";
        AH_THROW_RUNTIME(msg.str());
      }
      if ((size_t)index >= saa2.m_saa_data.size()) {
        saa2.m_saa_data.push_back(new_saa_piece);
        saa2.m_saa_data[index].m_instrument = inststr;
        saa2.m_saa_data[index].m_useAtFunctions = false;
      }
      AH_DEBUG << "Adding point " << latlon[0] << " " << latlon[1] << std::endl;
      ahmath::addPointToPolygon(latlon[0], latlon[1], saa2.m_saa_data[index].m_saaPolygon);
    }
  } else {
    AH_DEBUG << "End of polygon" << std::endl;
    index++;
  }
}

// ****************************************************************************

void load_saa2_data(std::string & saafile, SAA2DataVec & saa2_data) {

  // Initialize local variables
  double l_hx1_saa2[2] = { -1000.0 };
  double l_hx2_saa2[2] = { -1000.0 };
  double l_sg1_saa2[2] = { -1000.0 };
  double l_sg2_saa2[2] = { -1000.0 };
  double l_sxi_saa2[2] = { -1000.0 };
  double l_sxs_saa2[2] = { -1000.0 };

  int ihx1 = 0;  // Current SAA numbers in 2D array
  int ihx2 = 0;
  int isg1 = 0;
  int isg2 = 0;
  int isxi = 0;
  int isxs = 0;

  std::stringstream msg;

  ahfits::FilePtr fpsaa; // File pointer for passed FITS file

  saa2_data[0].m_instrument = "HX1";
  saa2_data[1].m_instrument = "HX2";
  saa2_data[2].m_instrument = "SG1";
  saa2_data[3].m_instrument = "SG2";
  saa2_data[4].m_instrument = "SXI";
  saa2_data[5].m_instrument = "SXS";

  // Open SAA file
  AH_INFO(ahlog::HIGH) << "Opening SAA data file" << std::endl;
  ahfits::open(saafile,"SAA2_VERTICES",&fpsaa);
  if(!ahfits::readOK(fpsaa)) {
    AH_THROW_RUNTIME("failed to open SAA file");
  }

  // setup router
  ahfits::Router router(fpsaa); // Router creation for FITS file
  router.connectFixedLengthArray(ahfits::e_READONLY, "HXI1_SAA2", l_hx1_saa2);
  router.connectFixedLengthArray(ahfits::e_READONLY, "HXI2_SAA2", l_hx2_saa2);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SGD1_SAA2", l_sg1_saa2);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SGD2_SAA2", l_sg2_saa2);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SXI_SAA2", l_sxi_saa2);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SXS_SAA2", l_sxs_saa2);

  // Loop over SAA file row
  for(ahfits::firstRow(fpsaa); ahfits::readOK(fpsaa); ahfits::nextRow(fpsaa)) {

    ahfits::readRow(fpsaa);

    // If latitude or longitude are -1000, set to minimum lat or lon;
    // do similarly for +1000 and maximum lat or lon.

    // Add points to SAA data for each instrument

    addPointToSAA2Data(ihx1, l_hx1_saa2, "HX1", saa2_data[0]);
    addPointToSAA2Data(ihx2, l_hx2_saa2, "HX2", saa2_data[1]);
    addPointToSAA2Data(isg1, l_sg1_saa2, "SG1", saa2_data[2]);
    addPointToSAA2Data(isg2, l_sg1_saa2, "SG2", saa2_data[3]);
    addPointToSAA2Data(isxi, l_sxi_saa2, "SXI", saa2_data[4]);
    addPointToSAA2Data(isxs, l_sxs_saa2, "SXS", saa2_data[5]);

  } // end loop over SAA file rows
  
  // loop over instruments 
  for(int ii = 0, numPieces = 0; ii < 6; ++ii) {

    numPieces = saa2_data[ii].m_saa_data.size();
    AH_DEBUG << "Instrument index: " << ii << "; number of SAA pieces: " 
             << numPieces << std::endl;

    for (int jj = 0, numPoints = 0; jj < numPieces; ++jj) {

      numPoints = saa2_data[ii].m_saa_data[jj].m_saaPolygon.m_num_points;
      AH_DEBUG << "Instrument: " << saa2_data[ii].m_saa_data[jj].m_instrument << std::endl;
      AH_DEBUG << "Piece index " << jj << "; number of polygon points: " << numPoints << std::endl;

      for(int kk = 0; kk < numPoints; ++kk) { 
        AH_DEBUG << "  lat: " << saa2_data[ii].m_saa_data[jj].m_saaPolygon.m_x[kk] 
                 << "  lon: " << saa2_data[ii].m_saa_data[jj].m_saaPolygon.m_y[kk] << std::endl;
      }

      ahmath::checkPolygon(saa2_data[ii].m_saa_data[jj].m_saaPolygon);

    }

  } // end loop over instrument

  ahfits::close(fpsaa);

} // end load_saa2_data

// ****************************************************************************

void computeDayNightAltitude(double t0, const ahtime::AhDateTime& epoch_utc, 
                             ahtime::leapsec::LeapSecTable & leapsec_data,
                             GENORBFILE * orb_descrip,
                             int & day_night, double & alt0) {

  double mjd_utc_0;                  // local MJD from t0
  AtVect vSat0, vSun, vSun0, nvSun;  // Satellite and Sun vectors calculated from orbpos0 
  ahtime::AhDateTime t0_utc;         //  local time in utc (unused)
  ORBPOSITION* orbpos0 = allocateOrbPosition(); // Vector to hold local orbital position

  compute_mjd_utc(t0, epoch_utc, leapsec_data, t0_utc, mjd_utc_0);
  findOrbPositionInGenOrbFile(orb_descrip, orbpos0, t0);
  copy_atvect_from_orbpos(vSat0, orbpos0);
  atSun(mjd_utc_0, vSun0);
  atNormVect(vSun0, nvSun);
  atEarthOccult(vSat0, nvSun, vSun, &day_night, &alt0);

}

// ****************************************************************************

void computeSAATimeBoundary(double mission_time, bool backwards, bool skipCoarse, 
                            const SAAData & saa, const ahtime::AhDateTime & epoch_utc, 
                            ahtime::leapsec::LeapSecTable & leapsec_data, 
                            GENORBFILE * orb_descrip, int in_saa0, double & out_saa_time) {

  // Compute SAA boundary time using a brute force coarse search along the orbit path 
  // 60 second resolution, followed by a fine search with 1 second resolution

  int saa0 = 0;               // local SAA value
  double t0 = mission_time;   // local time in search
  double mjd_utc_0 = 0.0;     // local MJD from t0
  double incr_coarse = 0.;    // time increment for coarse search
  double incr_fine = 0.;      // time increment for fine search
  double lat = 0.;
  double lon = 0.;

  ahtime::AhDateTime t0_utc;  // local time in utc (unused)
  ORBPOSITION* orbpos0 = allocateOrbPosition(); // Vector to hold local orbital position

  AtVect vSat0;               // local orbital position from orbpos0
  AtVect vSatG0;              // Geodetic orbital position
  AtPolarVect pvSatG0;        // Polar orbital position

  // Check if we're skipping the coarse search. If so, use the 
  // input saa value to start the fine search from
  if(!skipCoarse) {
    saa0 = saa.m_saa;
  } else {
    saa0 = in_saa0;
 }

  // Set up the increment values which depend on the direction
  // being searched
  if(backwards) {
    incr_coarse = -60;
    incr_fine = 1;
  } else {
    incr_coarse = 60;
    incr_fine = -1;
  }

  // Compute if we are in the SAA for each coarse time increment
  // Once we are outside of the SAA, exit the coarse search
  // and start a fine search in the reverse direction to find
  // the time at the SAA boundary
  if(!skipCoarse) {
    // Coarse search
    while(!saa0) {
      // Update the time of the SAA search, and get the satellite coordinates
      // at that time
      t0 = t0 + incr_coarse;
      compute_mjd_utc(t0, epoch_utc, leapsec_data, t0_utc, mjd_utc_0);
      findOrbPositionInGenOrbFile(orb_descrip, orbpos0, t0);
      copy_atvect_from_orbpos(vSat0, orbpos0);
      atGeodetic(mjd_utc_0, vSat0, vSatG0);
      atVectToPol(vSatG0, &pvSatG0);
      lat = RAD2DEG*pvSatG0.lat;
      lon = RAD2DEG*pvSatG0.lon;

      // Determine if we are in the SAA
      if(saa.m_useAtFunctions) {
        if(ahgen::strtoupper(saa.m_instrument) == "SUZAKU") {
          atBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
        } else if(ahgen::strtoupper(saa.m_instrument) == "HXD")  {
          atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
        }
      } else {
        // atPolarVect arrays are output in radians, need to convert them into degrees.
        // In the atFunctions, conversion is done within the function
        // saaPolygon data is read where: latitude is the first element (x-axis)
        //                                longitude is the second element (y-axis)
        // if (lon >= 180.) lon -= 360.;
        if(ahmath::isPointInsidePolygon(lat, lon, saa.m_saaPolygon)){
          saa0 = 1;
        } else if(ahmath::isPointInsidePolygon(lat, lon-360.0, saa.m_saaPolygon)){
          saa0 = 1;
        } else {
          // if not in the polygon set the saa value to 0
          saa0 = 0;
        }
      }
    }
  }

  // Fine search
  while(saa0) {
    // Update the time of the SAA search, and get the satellite coordinates
    // at that time
    t0 = t0 + incr_fine;
    compute_mjd_utc(t0, epoch_utc, leapsec_data, t0_utc, mjd_utc_0);
    findOrbPositionInGenOrbFile(orb_descrip, orbpos0, t0);
    copy_atvect_from_orbpos(vSat0, orbpos0);
    atGeodetic(mjd_utc_0, vSat0, vSatG0);
    atVectToPol(vSatG0, &pvSatG0);
    lat = RAD2DEG*pvSatG0.lat;
    lon = RAD2DEG*pvSatG0.lon;

    // Determine if we are in the SAA
    if(saa.m_useAtFunctions) {
      if(ahgen::strtoupper(saa.m_instrument) == "SUZAKU") {
        atBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
        if(0 == saa0) {
          atSISBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
          if(0 == saa0) {
            atSTTBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
          }
        }
      } else if(ahgen::strtoupper(saa.m_instrument) == "HXD")  {
        // Use the Suzaku atFunction to calculate if in the SAA
        atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
      }
    } else {
      // atPolarVect arrays are output in radians, need to convert them into degrees.
      // In the atFunctions, conversion is done within the function
      // saaPolygon data is read where: latitude is the first element (x-axis)
      //                                longitude is the second element (y-axis)
      //if (lon >= 180.) lon -= 360.;
      if(ahmath::isPointInsidePolygon(lat, lon, saa.m_saaPolygon)) {
        saa0 = 1;
      } else if(ahmath::isPointInsidePolygon(lat, lon-360.0, saa.m_saaPolygon)) {
        saa0 = 1;
      } else {
        // if not in the polygon set the saa value to 0
        saa0 = 0;
      }
    }
  }

  // return the final t0 as the time of the boundary
  out_saa_time = t0;

}

// ****************************************************************************

void copy_atvect_from_orbpos(AtVect atv, ORBPOSITION* orbpos) {
  for (int i=0; i<3; i++) {
    atv[i] = orbpos->p[i];
  }
}

// ****************************************************************************

double delta_phi(double phi1, double phi0) {
  double dphi = phi1 - phi0;
  while ( 180.0 < dphi ) {
    dphi -= 180.0;
  }
  while ( dphi <= -180.0 ) {
    dphi += 180.0;
  }
  return dphi;
}

// ****************************************************************************

void compute_mjd_utc (double mission_time, const ahtime::AhDateTime& epoch,
                      ahtime::leapsec::LeapSecTable & leapsec_data,
                      ahtime::AhDateTime& time_utc, double & mjd_utc) {

  // get UTC as DateTime
  ahtime::convertMissionTimeToUTC(mission_time,epoch,leapsec_data,time_utc);

  // get UTC as MJD
  ahtime::AhMJDTime tmp;
  ahtime::reformatDateTimeAsMJD(time_utc,tmp);
  mjd_utc=tmp.mjd();
}

// ****************************************************************************

void create_output_file(const std::string& outfile, ahfits::FilePtr& fp_out,
  const std::string& actual_leapsec, const std::string& attfile, const std::string& attext,
  const std::string& orbfile, const std::string& actual_teldef,
  const std::string& actual_rigfile2, const std::string& actual_rigfile3, 
  const std::string& actual_saafile, const double& bintime) {

  ahfits::create(outfile,"",&fp_out);

  ahfits::addEmptyTbl(fp_out,"EHK");
  
  // Copy some keywords from the attitude file
  ahfits::FilePtr fp_att = 0;
  ahfits::open(attfile,attext,&fp_att);
  ahmission::keyword::copyAllKeywords(fp_att,fp_out,ahmission::keyword::e_HK);
  ahfits::close(fp_att);

  // Set standard keywords
  ahfits::writeKeyValStr(fp_out, "CREATOR", "ahmkehk", "");
  ahfits::writeKeyValStr(fp_out, "LEAPFILE", actual_leapsec, "Name of the leapsecond file");
  ahfits::writeKeyValStr(fp_out, "TELDEF", actual_teldef, "Name of the TelDef file");
  ahfits::writeKeyValStr(fp_out, "RIG2FILE", actual_rigfile2, "Name of the rigidity file for COR2");
  ahfits::writeKeyValStr(fp_out, "RIG3FILE", actual_rigfile3, "Name of the rigidity file for COR3");
  ahfits::writeKeyValStr(fp_out, "SAAFILE", actual_saafile, "Name of the SAA vertices file");
  ahfits::writeKeyValDbl(fp_out, "TIMEDEL", bintime, "Binning factor");

  // Set up columns
  ahfits::insertColAfter(fp_out,"TIME","1D","");
  ahfits::setColumnDescription(fp_out,"TIME","mission time");
  ahfits::setTUnit(fp_out, "TIME","s");
  ahfits::setTDisp(fp_out, "TIME", "F16.6");
  ahfits::insertColAfter(fp_out,"YYYYMMDD","1J","");
  ahfits::setColumnDescription(fp_out,"YYYYMMDD","year*10000+month*100+day");
  ahfits::setTDisp(fp_out, "YYYYMMDD", "I8.8");
  ahfits::insertColAfter(fp_out,"HHMMSS","1J","");
  ahfits::setColumnDescription(fp_out,"HHMMSS","hour*10000+minute*100+second");
  ahfits::setTDisp(fp_out, "HHMMSS", "I6.6");
  ahfits::insertColAfter(fp_out,"EULER1","1E","");
  ahfits::setColumnDescription(fp_out,"EULER1","satellite Euler angle phi");
  ahfits::setTUnit(fp_out, "EULER1", "deg");
  ahfits::setTDisp(fp_out, "EULER1", "F11.6");
  ahfits::insertColAfter(fp_out,"EULER2","1E","");
  ahfits::setColumnDescription(fp_out,"EULER2","satellite Euler angle theta");
  ahfits::setTUnit(fp_out, "EULER2", "deg");
  ahfits::setTDisp(fp_out, "EULER2", "F11.6");
  ahfits::insertColAfter(fp_out,"EULER3","1E","");
  ahfits::setColumnDescription(fp_out,"EULER3","satellite Euler angle psi");
  ahfits::setTUnit(fp_out, "EULER3", "deg");
  ahfits::setTDisp(fp_out, "EULER3", "F11.6");
  ahfits::insertColAfter(fp_out,"QUAT1","1E","");
  ahfits::setColumnDescription(fp_out,"QUAT1","Quaternion Element 1");
  ahfits::setTUnit(fp_out, "QUAT1", "deg");
  ahfits::setTDisp(fp_out, "QUAT1", "F11.6");
  ahfits::insertColAfter(fp_out,"QUAT2","1E","");
  ahfits::setColumnDescription(fp_out,"QUAT2","Quaternion Element 2");
  ahfits::setTUnit(fp_out, "QUAT2", "deg");
  ahfits::setTDisp(fp_out, "QUAT2", "F11.6");
  ahfits::insertColAfter(fp_out,"QUAT3","1E","");
  ahfits::setColumnDescription(fp_out,"QUAT3","Quaternion Element 3");
  ahfits::setTUnit(fp_out, "QUAT3", "deg");
  ahfits::setTDisp(fp_out, "QUAT3", "F11.6");
  ahfits::insertColAfter(fp_out,"QUAT4","1E","");
  ahfits::setColumnDescription(fp_out,"QUAT4","Quaternion Element 4");
  ahfits::setTUnit(fp_out, "QUAT4", "deg");
  ahfits::setTDisp(fp_out, "QUAT4", "F11.6");
  ahfits::insertColAfter(fp_out,"RA","1D","");
  ahfits::setColumnDescription(fp_out,"RA","R.A.(J2000) of FOC center pos");
  ahfits::setTUnit(fp_out, "RA", "deg");
  ahfits::setTDisp(fp_out, "RA", "F11.6");
  ahfits::insertColAfter(fp_out,"DEC","1D","");
  ahfits::setColumnDescription(fp_out,"DEC","DEC.(J2000) of FOC center pos");
  ahfits::setTUnit(fp_out, "DEC", "deg");
  ahfits::setTDisp(fp_out, "DEC", "F11.6");
  ahfits::insertColAfter(fp_out,"ROLL","1D","");
  ahfits::setColumnDescription(fp_out,"ROLL","roll angle of FOC coordinates");
  ahfits::setTUnit(fp_out, "ROLL", "deg");
  ahfits::setTDisp(fp_out, "ROLL", "F11.6");
  ahfits::insertColAfter(fp_out,"HX1_RA_PNT","1D","");
  ahfits::setColumnDescription(fp_out,"HX1_RA_PNT","R.A. (J2000) of HXI1 optical axis");
  ahfits::setTUnit(fp_out, "HX1_RA_PNT", "deg");
  ahfits::setTDisp(fp_out, "HX1_RA_PNT", "F11.6");
  ahfits::insertColAfter(fp_out,"HX1_DEC_PNT","1D","");
  ahfits::setColumnDescription(fp_out,"HX1_DEC_PNT","DEC. (J2000) of HXI1 optical axis");
  ahfits::setTUnit(fp_out, "HX1_DEC_PNT", "deg");
  ahfits::setTDisp(fp_out, "HX1_DEC_PNT", "F11.6");
  ahfits::insertColAfter(fp_out,"HX2_RA_PNT","1D","");
  ahfits::setColumnDescription(fp_out,"HX2_RA_PNT","R.A. (J2000) of HXI2 optical axis");
  ahfits::setTUnit(fp_out, "HX2_RA_PNT", "deg");
  ahfits::setTDisp(fp_out, "HX2_RA_PNT", "F11.6");
  ahfits::insertColAfter(fp_out,"HX2_DEC_PNT","1D","");
  ahfits::setColumnDescription(fp_out,"HX2_DEC_PNT","DEC. (J2000) of HXI2 optical axis");
  ahfits::setTUnit(fp_out, "HX2_DEC_PNT", "deg");
  ahfits::setTDisp(fp_out, "HX2_DEC_PNT", "F11.6");
  ahfits::insertColAfter(fp_out,"SXS_RA_PNT","1D","");
  ahfits::setColumnDescription(fp_out,"SXS_RA_PNT","R.A. (J2000) of SXS optical axis");
  ahfits::setTUnit(fp_out, "SXS_RA_PNT", "deg");
  ahfits::setTDisp(fp_out, "SXS_RA_PNT", "F11.6");
  ahfits::insertColAfter(fp_out,"SXS_DEC_PNT","1D","");
  ahfits::setColumnDescription(fp_out,"SXS_DEC_PNT","DEC. (J2000) of SXS optical axis");
  ahfits::setTUnit(fp_out, "SXS_DEC_PNT", "deg");
  ahfits::setTDisp(fp_out, "SXS_DEC_PNT", "F11.6");
  ahfits::insertColAfter(fp_out,"SXI_RA_PNT","1D","");
  ahfits::setColumnDescription(fp_out,"SXI_RA_PNT","R.A. (J2000) of SXI optical axis");
  ahfits::setTUnit(fp_out, "SXI_RA_PNT", "deg");
  ahfits::setTDisp(fp_out, "SXI_RA_PNT", "F11.6");
  ahfits::insertColAfter(fp_out,"SXI_DEC_PNT","1D","");
  ahfits::setColumnDescription(fp_out,"SXI_DEC_PNT","DEC. (J2000) of SXI optical axis");
  ahfits::setTUnit(fp_out, "SXI_DEC_PNT", "deg");
  ahfits::setTDisp(fp_out, "SXI_DEC_PNT", "F11.6");
  ahfits::insertColAfter(fp_out,"DLT_RA","1E","");
  ahfits::setColumnDescription(fp_out,"DLT_RA","difference from mean R.A.");
  ahfits::setTUnit(fp_out, "DLT_RA", "arcmin");
  ahfits::setTDisp(fp_out, "DLT_RA", "F8.3");
  ahfits::insertColAfter(fp_out,"DLT_DEC","1E","");
  ahfits::setColumnDescription(fp_out,"DLT_DEC","difference from mean DEC.");
  ahfits::setTUnit(fp_out, "DLT_DEC", "arcmin");
  ahfits::setTDisp(fp_out, "DLT_DEC", "F8.3");
  ahfits::insertColAfter(fp_out,"DLT_ROLL","1E","");
  ahfits::setColumnDescription(fp_out,"DLT_ROLL","difference from mean roll angle");
  ahfits::setTUnit(fp_out, "DLT_ROLL", "deg");
  ahfits::setTDisp(fp_out, "DLT_ROLL", "F8.3");
  ahfits::insertColAfter(fp_out,"ANG_DIST","1E","");
  ahfits::setColumnDescription(fp_out,"ANG_DIST","distance from mean pointing pos");
  ahfits::setTUnit(fp_out, "ANG_DIST", "arcmin");
  ahfits::setTDisp(fp_out, "ANG_DIST", "F8.3");
  ahfits::insertColAfter(fp_out,"SAT_ALT","1E","");
  ahfits::setColumnDescription(fp_out,"SAT_ALT","altitude of sat orbit from earth surface");
  ahfits::setTUnit(fp_out, "SAT_ALT", "km");
  ahfits::setTDisp(fp_out, "SAT_ALT", "F11.6");
  ahfits::insertColAfter(fp_out,"SAT_LON","1E","");
  ahfits::setColumnDescription(fp_out,"SAT_LON","longitude of satellite orbit");
  ahfits::setTUnit(fp_out, "SAT_LON", "deg");
  ahfits::setTDisp(fp_out, "SAT_LON", "F11.6");
  ahfits::insertColAfter(fp_out,"SAT_LAT","1E","");
  ahfits::setColumnDescription(fp_out,"SAT_LAT","lattitude of satellite orbit");
  ahfits::setTUnit(fp_out, "SAT_LAT", "deg");
  ahfits::setTDisp(fp_out, "SAT_LAT", "F11.6");
  ahfits::insertColAfter(fp_out,"ELV","1E","");
  ahfits::setColumnDescription(fp_out,"ELV","earth elevation of FOC center pos");
  ahfits::setTUnit(fp_out, "ELV", "deg");
  ahfits::setTDisp(fp_out, "ELV", "F8.3");
  ahfits::insertColAfter(fp_out,"DYE_ELV","1E","");
  ahfits::setColumnDescription(fp_out,"DYE_ELV","day earth elev. of FOC center pos");
  ahfits::setTUnit(fp_out, "DYE_ELV", "deg");
  ahfits::setTDisp(fp_out, "DYE_ELV", "F8.3");
  ahfits::insertColAfter(fp_out,"NTE_ELV","1E","");
  ahfits::setColumnDescription(fp_out,"NTE_ELV","night earth elev. of FOC center pos");
  ahfits::setTUnit(fp_out, "NTE_ELV", "deg");
  ahfits::setTDisp(fp_out, "NTE_ELV", "F8.3");
  ahfits::insertColAfter(fp_out,"SUN_ALT","1E","");
  ahfits::setColumnDescription(fp_out,"SUN_ALT","altitude of the sun from the earth rim");
  ahfits::setTUnit(fp_out, "SUN_ALT", "deg");
  ahfits::setTDisp(fp_out, "SUN_ALT", "F8.3");
  ahfits::insertColAfter(fp_out,"T_DY_NT","1E","");
  ahfits::setColumnDescription(fp_out,"T_DY_NT","time after day <-> night transition");
  ahfits::setTUnit(fp_out, "T_DY_NT", "s");
  ahfits::setTDisp(fp_out, "T_DY_NT", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_DY_NT","1E","");
  ahfits::setColumnDescription(fp_out,"TN_DY_NT","time to next day <-> night transition");
  ahfits::setTUnit(fp_out, "TN_DY_NT", "s");
  ahfits::setTDisp(fp_out, "TN_DY_NT", "F9.3");
  ahfits::insertColAfter(fp_out,"COR","1E","");
  ahfits::setColumnDescription(fp_out,"COR","cut off rigidity (GV) with old table");
  ahfits::setTUnit(fp_out, "COR", "GV");
  ahfits::setTDisp(fp_out, "COR", "F8.3");
  ahfits::insertColAfter(fp_out,"COR2","1E","");
  ahfits::setColumnDescription(fp_out,"COR2","cut off rigidity (GV) with 20060421 table");
  ahfits::setTUnit(fp_out, "COR2", "GV");
  ahfits::setTDisp(fp_out, "COR2", "F8.3");
  ahfits::insertColAfter(fp_out,"COR3","1E","");
  ahfits::setColumnDescription(fp_out,"COR3","cut off rigidity (GV) with 20160101 table");
  ahfits::setTUnit(fp_out, "COR3", "GV");
  ahfits::setTDisp(fp_out, "COR3", "F8.3");
  ahfits::insertColAfter(fp_out,"CORTIME","1E","");
  ahfits::setColumnDescription(fp_out,"CORTIME","cut off rigidity (GV) with computed table");
  ahfits::setTUnit(fp_out, "CORTIME", "GV");
  ahfits::setTDisp(fp_out, "CORTIME", "F8.3");

  // SAA by Suzaku criteria
  ahfits::insertColAfter(fp_out,"SAA","1B","");
  ahfits::setColumnDescription(fp_out,"SAA","passage of South Atlantic Anomaly (0->3:deep)");
  ahfits::insertColAfter(fp_out,"T_SAA","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA","time after SAA passage");
  ahfits::setTUnit(fp_out, "T_SAA", "s");
  ahfits::setTDisp(fp_out, "T_SAA", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA","time to next SAA passage");
  ahfits::setTUnit(fp_out, "TN_SAA", "s");
  ahfits::setTDisp(fp_out, "TN_SAA", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA_HXD","1B","");
  ahfits::setColumnDescription(fp_out,"SAA_HXD","passage of South Atlantic Anomaly for HXD");
  ahfits::insertColAfter(fp_out,"T_SAA_HXD","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA_HXD","time after SAA passage for HXD");
  ahfits::setTUnit(fp_out, "T_SAA_HXD", "s");
  ahfits::setTDisp(fp_out, "T_SAA_HXD", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA_HXD","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA_HXD","time to next SAA passage for HXD");
  ahfits::setTUnit(fp_out, "TN_SAA_HXD", "s");
  ahfits::setTDisp(fp_out, "TN_SAA_HXD", "F9.3");

  // SAA for Hitomi instruments, by polygons on sky
  ahfits::insertColAfter(fp_out,"SAA_HXI1","1B","");
  ahfits::setColumnDescription(fp_out,"SAA_HXI1","passage of South Atlantic Anomaly for HXI1");
  ahfits::insertColAfter(fp_out,"T_SAA_HXI1","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA_HXI1","time after SAA passage for HXI1");
  ahfits::setTUnit(fp_out, "T_SAA_HXI1", "s");
  ahfits::setTDisp(fp_out, "T_SAA_HXI1", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA_HXI1","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA_HXI1","time to next SAA passage for HXI1");
  ahfits::setTUnit(fp_out, "TN_SAA_HXI1", "s");
  ahfits::setTDisp(fp_out, "TN_SAA_HXI1", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA_HXI2","1B","");
  ahfits::setColumnDescription(fp_out,"SAA_HXI2","passage of South Atlantic Anomaly for HXI2");
  ahfits::insertColAfter(fp_out,"T_SAA_HXI2","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA_HXI2","time after SAA passage for HXI2");
  ahfits::setTUnit(fp_out, "T_SAA_HXI2", "s");
  ahfits::setTDisp(fp_out, "T_SAA_HXI2", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA_HXI2","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA_HXI2","time to next SAA passage for HXI2");
  ahfits::setTUnit(fp_out, "TN_SAA_HXI2", "s");
  ahfits::setTDisp(fp_out, "TN_SAA_HXI2", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA_SGD1","1B","");
  ahfits::setColumnDescription(fp_out,"SAA_SGD1","passage of South Atlantic Anomaly for SGD1");
  ahfits::insertColAfter(fp_out,"T_SAA_SGD1","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA_SGD1","time after SAA passage for SGD1");
  ahfits::setTUnit(fp_out, "T_SAA_SGD1", "s");
  ahfits::setTDisp(fp_out, "T_SAA_SGD1", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA_SGD1","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA_SGD1","time to next SAA passage for SGD1");
  ahfits::setTUnit(fp_out, "TN_SAA_SGD1", "s");
  ahfits::setTDisp(fp_out, "TN_SAA_SGD1", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA_SGD2","1B","");
  ahfits::setColumnDescription(fp_out,"SAA_SGD2","passage of South Atlantic Anomaly for SGD2");
  ahfits::insertColAfter(fp_out,"T_SAA_SGD2","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA_SGD2","time after SAA passage for SGD2");
  ahfits::setTUnit(fp_out, "T_SAA_SGD2", "s");
  ahfits::setTDisp(fp_out, "T_SAA_SGD2", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA_SGD2","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA_SGD2","time to next SAA passage for SGD2");
  ahfits::setTUnit(fp_out, "TN_SAA_SGD2", "s");
  ahfits::setTDisp(fp_out, "TN_SAA_SGD2", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA_SXI","1B","");
  ahfits::setColumnDescription(fp_out,"SAA_SXI","passage of South Atlantic Anomaly for SXI");
  ahfits::insertColAfter(fp_out,"T_SAA_SXI","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA_SXI","time after SAA passage for SXI");
  ahfits::setTUnit(fp_out, "T_SAA_SXI", "s");
  ahfits::setTDisp(fp_out, "T_SAA_SXI", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA_SXI","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA_SXI","time to next SAA passage for SXI");
  ahfits::setTUnit(fp_out, "TN_SAA_SXI", "s");
  ahfits::setTDisp(fp_out, "TN_SAA_SXI", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA_SXS","1B","");
  ahfits::setColumnDescription(fp_out,"SAA_SXS","passage of South Atlantic Anomaly for SXS");
  ahfits::insertColAfter(fp_out,"T_SAA_SXS","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA_SXS","time after SAA passage for SXS");
  ahfits::setTUnit(fp_out, "T_SAA_SXS", "s");
  ahfits::setTDisp(fp_out, "T_SAA_SXS", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA_SXS","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA_SXS","time to next SAA passage for SXS");
  ahfits::setTUnit(fp_out, "TN_SAA_SXS", "s");
  ahfits::setTDisp(fp_out, "TN_SAA_SXS", "F9.3");

  // High-flux SAA regions for Hitomi instruments, defined by multiple polygons
  ahfits::insertColAfter(fp_out,"SAA2_HXI1","1B","");
  ahfits::setColumnDescription(fp_out,"SAA2_HXI1","passage of South Atlantic Anomaly for HXI1");
  ahfits::insertColAfter(fp_out,"T_SAA2_HXI1","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA2_HXI1","time after SAA passage for HXI1");
  ahfits::setTUnit(fp_out, "T_SAA2_HXI1", "s");
  ahfits::setTDisp(fp_out, "T_SAA2_HXI1", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA2_HXI1","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA2_HXI1","time to next SAA passage for HXI1");
  ahfits::setTUnit(fp_out, "TN_SAA2_HXI1", "s");
  ahfits::setTDisp(fp_out, "TN_SAA2_HXI1", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA2_HXI2","1B","");
  ahfits::setColumnDescription(fp_out,"SAA2_HXI2","passage of South Atlantic Anomaly for HXI2");
  ahfits::insertColAfter(fp_out,"T_SAA2_HXI2","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA2_HXI2","time after SAA passage for HXI2");
  ahfits::setTUnit(fp_out, "T_SAA2_HXI2", "s");
  ahfits::setTDisp(fp_out, "T_SAA2_HXI2", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA2_HXI2","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA2_HXI2","time to next SAA passage for HXI2");
  ahfits::setTUnit(fp_out, "TN_SAA2_HXI2", "s");
  ahfits::setTDisp(fp_out, "TN_SAA2_HXI2", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA2_SGD1","1B","");
  ahfits::setColumnDescription(fp_out,"SAA2_SGD1","passage of South Atlantic Anomaly for SGD1");
  ahfits::insertColAfter(fp_out,"T_SAA2_SGD1","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA2_SGD1","time after SAA passage for SGD1");
  ahfits::setTUnit(fp_out, "T_SAA2_SGD1", "s");
  ahfits::setTDisp(fp_out, "T_SAA2_SGD1", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA2_SGD1","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA2_SGD1","time to next SAA passage for SGD1");
  ahfits::setTUnit(fp_out, "TN_SAA2_SGD1", "s");
  ahfits::setTDisp(fp_out, "TN_SAA2_SGD1", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA2_SGD2","1B","");
  ahfits::setColumnDescription(fp_out,"SAA2_SGD2","passage of South Atlantic Anomaly for SGD2");
  ahfits::insertColAfter(fp_out,"T_SAA2_SGD2","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA2_SGD2","time after SAA passage for SGD2");
  ahfits::setTUnit(fp_out, "T_SAA2_SGD2", "s");
  ahfits::setTDisp(fp_out, "T_SAA2_SGD2", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA2_SGD2","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA2_SGD2","time to next SAA passage for SGD2");
  ahfits::setTUnit(fp_out, "TN_SAA2_SGD2", "s");
  ahfits::setTDisp(fp_out, "TN_SAA2_SGD2", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA2_SXI","1B","");
  ahfits::setColumnDescription(fp_out,"SAA2_SXI","passage of South Atlantic Anomaly for SXI");
  ahfits::insertColAfter(fp_out,"T_SAA2_SXI","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA2_SXI","time after SAA passage for SXI");
  ahfits::setTUnit(fp_out, "T_SAA2_SXI", "s");
  ahfits::setTDisp(fp_out, "T_SAA2_SXI", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA2_SXI","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA2_SXI","time to next SAA passage for SXI");
  ahfits::setTUnit(fp_out, "TN_SAA2_SXI", "s");
  ahfits::setTDisp(fp_out, "TN_SAA2_SXI", "F9.3");
  ahfits::insertColAfter(fp_out,"SAA2_SXS","1B","");
  ahfits::setColumnDescription(fp_out,"SAA2_SXS","passage of South Atlantic Anomaly for SXS");
  ahfits::insertColAfter(fp_out,"T_SAA2_SXS","1E","");
  ahfits::setColumnDescription(fp_out,"T_SAA2_SXS","time after SAA passage for SXS");
  ahfits::setTUnit(fp_out, "T_SAA2_SXS", "s");
  ahfits::setTDisp(fp_out, "T_SAA2_SXS", "F9.3");
  ahfits::insertColAfter(fp_out,"TN_SAA2_SXS","1E","");
  ahfits::setColumnDescription(fp_out,"TN_SAA2_SXS","time to next SAA passage for SXS");
  ahfits::setTUnit(fp_out, "TN_SAA2_SXS", "s");
  ahfits::setTDisp(fp_out, "TN_SAA2_SXS", "F9.3");

  ahfits::insertColAfter(fp_out,"ZGMAG_ANG","1E","");
  ahfits::setColumnDescription(fp_out,"ZGMAG_ANG","z-axis angle of geomagnetic field");
  ahfits::setTUnit(fp_out, "ZGMAG_ANG", "deg");
  ahfits::setTDisp(fp_out, "ZGMAG_ANG", "F8.3");
  ahfits::insertColAfter(fp_out,"ZGMAG_PHI","1E","");
  ahfits::setColumnDescription(fp_out,"ZGMAG_PHI","z-axis roll of geomagnetic field");
  ahfits::setTUnit(fp_out, "ZGMAG_PHI", "deg");
  ahfits::setTDisp(fp_out, "ZGMAG_PHI", "F8.3");
  ahfits::insertColAfter(fp_out,"ZE_ANG","1E","");
  ahfits::setColumnDescription(fp_out,"ZE_ANG","z-axis angle to center of the Earth");
  ahfits::setTUnit(fp_out, "ZE_ANG", "deg");
  ahfits::setTDisp(fp_out, "ZE_ANG", "F8.3");
  ahfits::insertColAfter(fp_out,"ZE_PHI","1E","");
  ahfits::setColumnDescription(fp_out,"ZE_PHI","z-axis roll to Earth center direction");
  ahfits::setTUnit(fp_out, "ZE_PHI", "deg");
  ahfits::setTDisp(fp_out, "ZE_PHI", "F8.3");
  ahfits::insertColAfter(fp_out,"MZELV","1E","");
  ahfits::setColumnDescription(fp_out,"MZELV","earth elevation of minus Z direction");
  ahfits::setTUnit(fp_out, "MZELV", "deg");
  ahfits::setTDisp(fp_out, "MZELV", "F8.3");
  ahfits::insertColAfter(fp_out,"MZDYE_ELV","1E","");
  ahfits::setColumnDescription(fp_out,"MZDYE_ELV","day earth elev. of minus Z direction");
  ahfits::setTUnit(fp_out, "MZDYE_ELV", "deg");
  ahfits::setTDisp(fp_out, "MZDYE_ELV", "F8.3");
  ahfits::insertColAfter(fp_out,"MZNTE_ELV","1E","");
  ahfits::setColumnDescription(fp_out,"MZNTE_ELV","night earth elev. of minus Z direction");
  ahfits::setTUnit(fp_out, "MZNTE_ELV", "deg");
  ahfits::setTDisp(fp_out, "MZNTE_ELV", "F8.3");

}

// ****************************************************************************

/** @} */

/* Revision Log

 $Log: ahmkehk.cxx,v $
 Revision 1.56  2016/07/21 17:45:26  rshill
 Cleaned up; added comments.

 Revision 1.55  2016/07/21 01:46:00  rshill
 Added multiple polygon SAA capability using 2nd extension of SAA file.

 Revision 1.54  2016/05/24 23:22:19  rshill
 Added columns MZELV, MZDYE_ELV, MZNTE_ELV (minus-Z elevation from
 earth, dayside earth, and nightside earth, respectively).

 Revision 1.52  2016/05/13 19:07:36  asargent
 Addition of TIMEDEL(=bintime) keyword to EHK file.

 Revision 1.51  2016/03/28 09:16:03  mwitthoe
 ahmkehk: fix bug where telescope longitude is computed in the range [0:360] degrees, but the SAA vertices have longitude values in the range [-180:+180]; the telescope longitude is converted to the SAA range just for Hitomi immediately before checking if the point is contained in the polygon

 Revision 1.50  2016/03/24 15:42:58  rshill
 Checks for -999 in RA_NOM, DEC_NOM, or PA_NOM.

 Revision 1.49  2016/03/22 15:37:16  rshill
 Made keyword string comparisons cases-insensitive (issue #610).

 Revision 1.48  2016/02/22 15:19:52  asargent
 Switched the order of the SXI/SXS optical axis values, they were wrong in code

 Revision 1.47  2016/01/29 16:45:41  asargent
 Added keyword comments for TSTART,TSTOP and nominal pointing keywords.

 Revision 1.46  2016/01/09 16:32:25  asargent
 Fixed bug during SAA calculation where astro-h longitudes were not being offset by -360 degrees when greater than 180 degrees.

 Revision 1.45  2015/10/23 15:11:16  rshill
 Change inst_RA_PNT and inst_DEC_PNT columns to double.

 Revision 1.44  2015/10/09 15:47:20  asargent
 Updated RA, DEC, ROLL columns in output file to double precision.

 Revision 1.43  2015/09/23 15:32:04  asargent
 Moved SKY coordinate centering to outside of row loop

 Revision 1.42  2015/09/23 15:21:51  asargent
 Fixed instrments in *_RA_PNT and *_DEC_PNT column descriptions

 Revision 1.41  2015/09/23 12:34:45  rshill
 Add setSkyCoordCenterInTelDef2; negate scale_x.  (Bugfixes for optaxis tracking.)

 Revision 1.40  2015/09/22 20:37:40  asargent
 Implemented optical axis coordinate calculations (ongoing testing)

 Revision 1.39  2015/08/04 16:42:44  asargent
 Updated reading of keywords MEAN_EAn to POINTING keywords. Changed column names for saa file.

 Revision 1.38  2015/07/30 18:07:52  asargent
 Use of attitude file DATE-OBS keyword for CALDB SAA and TelDef file retrieval. Use of new copy keywords function in output file creation

 Revision 1.37  2015/07/28 14:24:42  asargent
 Updated ahmkehk

 Revision 1.36  2015/07/27 19:38:30  klrutkow
 per issue 532, cleanup of tools

 Revision 1.35  2015/07/24 20:22:56  asargent
 Several new updates to ahmkehk including new SAA CALDB file, new COR3 and CORTIME columns, removed old caldb resolve functions and replaced with ahmission::caldb::resolve

 Revision 1.34  2015/06/19 23:40:45  rshill
 Tweaked for new version of genorbfile; no functional difference.

 Revision 1.33  2015/06/11 19:19:01  asargent
 Added SAA columns for HXI, SGD, SXI and SXS

 Revision 1.32  2015/05/18 18:05:48  asargent
 Added column descriptions and copy several keywords from attitude file to output file

 Revision 1.31  2015/05/05 17:12:40  asargent
 Throw an error if attitude or orbit files do not open correctly

 Revision 1.30  2015/05/04 21:40:34  asargent
 Removed use of euler2radecroll and replaced with coordfits function convertEulerToPointing to use alignment and pointing structures

 Revision 1.29  2015/04/14 20:12:52  rshill
 Clarified the attitude file initialization.

 Revision 1.28  2015/04/06 16:12:49  rshill
 Change the COMPONENTS and VECTOR options to position only.

 Revision 1.27  2015/03/18 22:54:45  rshill
 Use upgraded genorbfile for orbit file access.

 Revision 1.26  2015/03/18 17:39:10  asargent
 Updated detname to detnam

 Revision 1.25  2015/02/12 19:23:08  rshill
 Cleaned up +++ comments.

 Revision 1.24  2015/01/07 04:57:36  mwitthoe
 ahmkehk: update parameters; see issue 472

 Revision 1.23  2014/09/12 18:09:13  mwitthoe
 ahmkehk: add support for extended syntax when using the reference file

 Revision 1.22  2014/09/10 02:49:05  mwitthoe
 ahmkehk tool: update tool to reflect new locations of timfile and leapsec CALDB libraries; there was a rounding error when converting the TT epoch to UTC, so use the integer part of the epoch as the UTC value

 Revision 1.21  2014/08/11 20:56:40  mwitthoe
 ahmkehk: fix doxygen title

 Revision 1.20  2014/08/05 12:42:39  mwitthoe
 ahmkehk: make an implicit type conversions explicit to avoid compiler error

 Revision 1.19  2014/01/28 17:56:55  mwitthoe
 ahmkehk code review changes: add constructor to DataType struct in ahmkehklib; add const to std::strings arguments; make parameter variables consistent with parameter names; fix some Doxygen statements; initialize all variables and add some descriptions (note: initializing pointers solved a seg fault bug); remove extra startUp() call in main()

 Revision 1.18  2014/01/09 20:55:10  klrutkow
 klrutkow: code review: comments

 Revision 1.17  2014/01/09 20:29:07  mwitthoe
 ahmkehk: code review: variable declarations

 Revision 1.16  2014/01/09 20:03:12  asargent
 Added comments pertaining to std main() CR

 Revision 1.15  2014/01/03 20:31:37  rshill
 Added quaternion capability.  Converted to new standard main.
 Converted to ahmkehklib.cxx/h rather than orbit.cxx/h.

 Revision 1.14  2013/12/02 22:56:27  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.13  2013/10/16 17:24:16  rshill
 Converted connects to new calls.

 Revision 1.12  2013/10/15 17:47:02  mwitthoe
 ahmkehk: update tool to use new version of ahtime library classes

 Revision 1.11  2013/09/12 15:18:23  mwitthoe
 ahmkehk: explicitly move to first binary table in reference FITS fi file to prepare to redefinition of an empty string to represent the primary HDU in ahfits (see issue #270)

 Revision 1.10  2013/08/05 20:20:21  rshill
 Upgraded doxygen comments.

 Revision 1.9  2013/08/01 19:58:20  klrutkow
 added comments, added the column and keyword names in the doxygen at the beginning of the file

 Revision 1.8  2013/07/26 15:57:19  rshill
 Corrections for unit tests.

 Revision 1.7  2013/07/24 14:57:11  rshill
 First full implementation.

 Revision 1.6  2013/07/22 22:46:30  rshill
 Complete implementation, not fully tested.

 Revision 1.5  2013/07/19 23:10:36  rshill
 Needs getPar guts and some keyword processing.

 Revision 1.4  2013/07/18 23:45:56  rshill
 Small revisions for conformity with TRF.

 Revision 1.3  2013/07/18 18:49:40  rshill
 Working for one hard-coded example; reference file not implemented.

 Revision 1.2  2013/07/16 00:34:06  rshill
 Check-in for backup.  Not done.

 Revision 1.1  2013/07/03 18:53:30  rshill
 First version that builds.  Very rudimentary; not ready for use/delivery.

*/
