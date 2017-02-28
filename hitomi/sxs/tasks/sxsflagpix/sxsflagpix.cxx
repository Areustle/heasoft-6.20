/// \file sxsflagpix.cxx
/// \brief Flag SXS event data as antico or cross-talk
/// \author Mike Witthoeft
/// \date $Date: 2016/10/07 15:33:24 $
/// \version 1.0

/**

\defgroup tool_sxsflagpix Flag SXS events (sxsflagpix)
@ingroup mod_sxs_tasks

The sxsflagpix tool will flag SXS pixel events with antico coincidence, 
electrical or recoil cross-talk, or are in MXS GTI. The flags are written to 
the STATUS column.

STATUS bits set:

 Bit          Description
  0            Event is contained within a general GTI
  1            Event is contained within a pixel-specific GTI
  2            Event is coincident with an antico event
  3            Event occurs near in time to another event
  4            Event occurs near in time to another pixel 12 event (recoil cross-talk)
  5            Bit 4 is set and the recoil energy test is satisfied
  6            Event occurs near in time to an event in an electrically- adjacent pixel
  7            Within group of electrical cross-talk events, this event has the largest PHA
  8            Event is contained within the direct MXS GTI
  9            Event is contained in the afterglow region of a direct MXS GTI
 10            Event is contained within the indirect MXS GTI
 11            Event is contained in the afterglow region of an indirect MXS GTI
 12            Same as bit 6, but using 2nd electrical cross-talk a-time
 13            Same as bit 7, but using 2nd electrical cross-talk a-time
 14            Undefined
 15            Undefined

Source files:

  sxsflagpix.cxx
  sxsflagpixlib.h
  sxsflagpixlib.cxx

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
  1.0   2015-07-30   MCW    Clean-up code

*/

#define AHLABEL tool_sxsflagpix
#define AHCVSID "$Id: sxsflagpix.cxx,v 1.68 2016/10/07 15:33:24 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "sxsflagpixlib.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahsxs/ahsxs.h"
#include "ahsxs/sxsdt.h"
#include "ahsxs/sxsstatus.h"
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <iterator>
#include <cmath>
#include <cstdio>           // std::remove()

/** \addtogroup tool_sxsflagpix
 *  @{
 */

/// \brief Get parameter values.
/// \param[out] par          structure to store input parameter
void getPar(Par& par);

/// \brief Copy contents of infile to outfile; resolve default values of columns.
/// \param[in]  par          structure to store input parameter
/// \param[out] antdtpre     delta time in sec preceding an antico event
/// \param[out] antdtfol     delta time in sec following an antico event
/// \param[out] proxdt       delta time in sec for checking event proximity
/// \param[out] ctrecdt      delta time in sec for event cross-talk due to recoil electrons
/// \param[out] cteldt       delta time in sec for electrical cross-talk between pixels
/// \param[out] cteldt2      delta time in sec for electrical cross-talk 2 between pixels
/// \param[out] mxsdt        afterglow time
/// \param[out] antshift     Shift to apply to the TIME of an antico event (s) or CALDB
/// \param[out] dtfile       name of FITS file with delta-time values
/// \param[out] fp           ahfits object of output file (clone of input)
/// \param[out] fpa          ahfits object of antico file (if needed)
/// \param[out] gp           ahfits object of MXS GTI file (if needed)
/// \param[out] extnumarray  number of rows for each MXS GTI extension
/// \param[out] pixmapdat   data from SXS pixel map CALDB file
void initialize(Par& par, double& antdtpre,double& antdtfol, double& proxdt,
                double& ctrecdt, double& cteldt, double& cteldt2, double& mxsdt,
                double& antshift, std::string& dtfile, ahfits::FilePtr& fp,
                ahfits::FilePtr& fpa, ahfits::FilePtr& gp, long* extnumarray,
                sxspixelmap::DataType& pixmapdat);

/// \brief Fill in 2nd extension of output TIM file.
/// \param[in] fp          ahfits object of output file (clone of input)
/// \param[in] fpa         ahfits object of antico file (if needed)
/// \param[in] gp          ahfits object of MXS GTI file (if needed)
/// \param[in] par         structure to store input parameter
/// \param[in] antshift    Shift to apply to the TIME of an antico event (s) or CALDB
/// \param[in] antdtpre    delta time in sec preceding an antico event
/// \param[in] antdtfol    delta time in sec following an antico event
/// \param[in] proxdt      delta time in sec for checking event proximity
/// \param[in] ctrecdt     delta time in sec for event cross-talk due to recoil electrons
/// \param[in] cteldt      delta time in sec for electrical cross-talk between pixels
/// \param[in] cteldt2     delta time in sec for electrical cross-talk 2 between pixels
/// \param[in] mxsdt       afterglow time
/// \param[in] extnumarray number of rows for each MXS GTI extension
/// \param[in] pixmapdat   data from SXS pixel map CALDB file
/// \param[out] clean if yes, delete output files
void doWork(ahfits::FilePtr fp, ahfits::FilePtr fpa,  ahfits::FilePtr gp, Par par,
            double antshift, double antdtpre, double antdtfol, double proxdt, 
            double ctrecdt, double cteldt, double cteldt2, double mxsdt,
            long* extnumarray, sxspixelmap::DataType& pixmapdat, bool& clean);

/// \brief Close open FITS files.
/// \param[in] fp          ahfits object of output file (clone of input)
/// \param[in] fpa         ahfits object of antico file (if needed)
/// \param[in] gp          ahfits object of MXS GTI file (if needed)
/// \param[in] clean if yes, delete output files
void finalize(ahfits::FilePtr fp, ahfits::FilePtr fpa, ahfits::FilePtr gp,
              bool clean);


/// \brief sxsflagpix tool
int main(int argc, char** argv) {

  Par par;                         // structure containg paramter values
  double antdtpre = -1.;           // delta-t before event to check for antico
  double antdtfol = -1.;           // delta-t after event to check for antico
  double proxdt = -1.;             // delta-t for checking event proximity
  double ctrecdt = -1.;            // delta-t for recoil cross-talk
  double cteldt = -1.;             // delta-t for electrical cross-talk
  double cteldt2 = -1.;            // delta-t for electrical cross-talk 2
  double mxsdt = -1.;              // delta-t for MXS GTI afterglow
  double antshift = -1.;           // shift to apply to the TIME of an antico event(s) or CALDB
  std::string dtfile = "";         // file containing multiple time intervals
  bool clean=true;                 // delete output files (upon error)

  long extnumarray[NUMLED];        // number of GTI rows for each LED
  sxspixelmap::DataType pixmapdat; // data from SXS pixel map CALDB file

  ahfits::FilePtr fp=0;            // FITS file pointer to output file
  ahfits::FilePtr fpa=0;           // FITS file pointer to Antico file
  ahfits::FilePtr gp=0;            // FITS file pointer to MXS GTI file

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog(); 
      initialize(par, antdtpre, antdtfol, proxdt, ctrecdt, cteldt, cteldt2, 
                 mxsdt, antshift, dtfile, fp, fpa, gp, extnumarray, pixmapdat);
      doWork(fp, fpa, gp, par, antshift, antdtpre, antdtfol, proxdt, ctrecdt, 
             cteldt, cteldt2, mxsdt, extnumarray, pixmapdat,clean);
      finalize(fp,fpa,gp,clean);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par, antdtpre, antdtfol, proxdt, ctrecdt, cteldt, cteldt2,
                   mxsdt, antshift, dtfile, fp, fpa, gp, extnumarray, pixmapdat);
        doWork(fp, fpa, gp, par, antshift, antdtpre, antdtfol, proxdt, ctrecdt, 
               cteldt, cteldt2, mxsdt, extnumarray, pixmapdat,clean);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fp,fpa,gp,clean);
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

void getPar(Par & par) {

  // The following flags indicate if valid parameters are provided for different
  // operation modes
  bool calcantokay=true;
  bool calcctrecokay=true;
  bool calcctelokay=true;
  bool calcctelokay2=true;
  bool calcmxsokay=true;
  bool calcproxokay=true;
  bool anybad=false;

  par.m_infile=ahapp::getParString("infile");
  par.m_inantfile=ahapp::getParString("inantfile");
  par.m_outfile=ahapp::getParString("outfile");
  par.m_dtflag=ahapp::getParBool("dtflag");
  par.m_ckrisetime=ahapp::getParBool("ckrisetime");
  par.m_resetflags=ahapp::getParString("resetflags");

  par.m_antpsp=ahgen::strtoupper(ahapp::getParString("antpsp"));
  if (par.m_antpsp == "PSPA" || par.m_antpsp == "PSP-A") par.m_antpsp="A";
  if (par.m_antpsp == "PSPB" || par.m_antpsp == "PSP-B") par.m_antpsp="B";
  if (par.m_antpsp != "A" && par.m_antpsp != "B") 
    AH_THROW_RUNTIME("invalid value for antpsp parameter; should be A or B");

  par.m_antshift=ahapp::getParString("antshift");
  par.m_gtifile=ahapp::getParString("gtifile");

  par.m_kalow=ahapp::getParDouble("kalow");
  par.m_kahigh=ahapp::getParDouble("kahigh");
  par.m_kbeta=ahapp::getParDouble("kbeta");

  par.m_calcant=ahapp::getParBool("calcant");
  if (par.m_calcant) {
    par.m_antdtpre=ahapp::getParString_warn("antdtpre",calcantokay);
    par.m_antdtfol=ahapp::getParString_warn("antdtfol",calcantokay);
    par.m_antswitch=ahapp::getParInt_warn("antswitch",calcantokay);
    par.m_antphathr=ahapp::getParInt_warn("antphathr",calcantokay);
    par.m_antdurthr=ahapp::getParInt_warn("antdurthr",calcantokay);
  }

  par.m_calcctrec=ahapp::getParBool("calcctrec");
  if (par.m_calcctrec) {
    par.m_ctrecdt=ahapp::getParString_warn("ctrecdt",calcctrecokay);
  }

  par.m_calcctel=ahapp::getParBool("calcctel");
  if (par.m_calcctel) {
    par.m_pixdeffile=ahapp::getParString_warn("pixdeffile",calcctelokay);
    par.m_cteldt=ahapp::getParString_warn("cteldt",calcctelokay);
    par.m_ctelnear=ahapp::getParInt_warn("ctelnear",calcctelokay);
  }

  par.m_calcctel2=ahapp::getParBool("calcctel2");
  if (par.m_calcctel2) {
    par.m_pixdeffile=ahapp::getParString_warn("pixdeffile",calcctelokay2);
    par.m_cteldt2=ahapp::getParString_warn("cteldt2",calcctelokay2);
    par.m_ctelnear2=ahapp::getParInt_warn("ctelnear2",calcctelokay2);
  }

  par.m_pxpithr=ahapp::getParInt("pxpithr");
  par.m_usepxpithr=ahapp::getParString("usepxpithr");

  par.m_calcmxs=ahapp::getParBool("calcmxs");
  if (par.m_calcmxs) {
    par.m_mxsdt=ahapp::getParString_warn("mxsdt",calcmxsokay);
    par.m_inmxsgti=ahapp::getParString_warn("inmxsgti",calcmxsokay);
  }

  if (par.m_calcant) {
    if (calcantokay)
      AH_INFO(ahlog::HIGH) << "flagging Antico: YES" << std::endl;
    else
      AH_INFO(ahlog::HIGH) << "flagging Antico: NO" << std::endl;
  }

  if (par.m_calcctrec) {
    if (calcctrecokay)
      AH_INFO(ahlog::HIGH) << "flagging Recoil CT: YES" << std::endl;
    else
      AH_INFO(ahlog::HIGH) << "flagging Recoil CT: NO" << std::endl;
  }

  par.m_calcprox = ahapp::getParBool("calcprox");
  if (par.m_calcprox) {
    par.m_proxdt=ahapp::getParString_warn("proxdt",calcproxokay);
  }

  if (par.m_calcctel) {
    if (calcctelokay)
      AH_INFO(ahlog::HIGH) << "flagging Electrical CT: YES" << std::endl;
    else
      AH_INFO(ahlog::HIGH) << "flagging Electrical CT: NO" << std::endl;
  }

  if (par.m_calcctel2) {
    if (calcctelokay2)
      AH_INFO(ahlog::HIGH) << "flagging Electrical CT 2: YES" << std::endl;
    else
      AH_INFO(ahlog::HIGH) << "flagging Electrical CT 2: NO" << std::endl;
  }

  if (par.m_calcmxs) {
    if (calcmxsokay)
      AH_INFO(ahlog::HIGH) << "flagging MXS: YES" << std::endl;
    else
      AH_INFO(ahlog::HIGH) << "flagging MXS: NO" << std::endl;
  }

  anybad=!calcantokay || !calcctrecokay || !calcctelokay || !calcctelokay2 || !calcmxsokay || !calcproxokay;
  if (anybad)
    AH_THROW_RUNTIME("bad parameter values for some operations");

  // convert usepxpithr parameter string into set of enumerated values
  if (par.m_calcprox || par.m_calcctel || par.m_calcctel2 || par.m_calcctrec) {
    parse_flagging_types("usepxpithr",par.m_usepxpithr,par.m_usepxpithr_values);
  }

  // convert resetflags parameter string into set of enumerated values
  parse_flagging_types("resetflags",par.m_resetflags,par.m_resetflags_values);

}

// ****************************************************************************

void initialize(Par& par, double& antdtpre,double& antdtfol, double& proxdt,
                double& ctrecdt, double& cteldt, double& cteldt2, double& mxsdt,
                double& antshift, std::string& dtfile, ahfits::FilePtr& fp,
                ahfits::FilePtr& fpa, ahfits::FilePtr& gp, long* extnumarray,
                sxspixelmap::DataType& pixmapdat) {

  // Extension names for input and antico file; strings to store INSTRUME
  // and DETNAM keyword values.
  std::string extname="EVENTS";
  std::string antiext="EVENTS";
  std::string inst, det, det_ant;

  // TSTART/TSTOP values for input, antico, and GTI files
  double tstart_in=0.;
  double tstop_in=0.;
  double tstart_gti=0.;
  double tstop_gti=0.;

  //parameters needed for CALDB query
  std::string filetype = "delta-t";
  std::string detnam = "-";
  std::string codename = "DELTIMES";
  std::string datetime;
  
  //Time intervals data structure
  ahsxs::sxsdt::SXSTimeIntervals dts;

  // Copy contents of infile to outfile and return opened output file (fp).
  // If editing input file in-place (allowed with last argument = true),
  // fp will point to the opened input file.
  ahfits::clone(par.m_infile,par.m_outfile,&fp,true);
  if (ahfits::isPrimary(fp)) ahfits::move(fp,extname);   // move to EVENTS extension if extended syntax not used
  ahmission::checkEmptyTable(fp,par.m_infile);
  AH_INFO(ahlog::HIGH) << "Input file, extension: " << par.m_infile << ", " << ahfits::getKeyValStr(fp,"EXTNAME") << std::endl;

  inst=ahfits::getKeyValStr(fp,"INSTRUME");
  det=ahfits::getKeyValStr(fp,"DETNAM");
  tstart_in=ahfits::getKeyValDbl(fp,"TSTART");
  tstop_in=ahfits::getKeyValDbl(fp,"TSTOP");
  datetime = ahfits::getKeyValStr(fp,"DATE-OBS");
  if ("SXS" != inst)
    AH_THROW_RUNTIME("INSTRUME keyword in input file should be SXS, not "+inst);
  if (("PIXEL" != det) && ("BASELINE" != det))
    AH_THROW_RUNTIME("DETNAM keyword in input file should be PIXEL or BASELINE, not "+det);

  // Interval times can come from calibration file if "CALDB" or a filename is 
  // specified as the parameter value.  However, the same calibration file must
  // be used for each value. Below we will check if each time interval parameter
  // is a number or filename/CALDB, then check if all of the file names are the
  // same.  If a calibration file is needed, then the it is queried (if CALDB)
  // and the time intervals loaded based on the value of TSTART from the input
  // file.
  antdtpre = -1.;
  antdtfol = -1.;
  proxdt = -1.;
  ctrecdt = -1.;
  cteldt = -1.;
  cteldt2 = -1.;
  mxsdt = -1.;
  antshift = -1.;
  dtfile = "";
  if (par.m_calcant) getdtpar(par.m_antdtpre, antdtpre, dtfile);
  if (par.m_calcant) getdtpar(par.m_antdtfol, antdtfol, dtfile);
  if (par.m_calcprox) getdtpar(par.m_proxdt, proxdt, dtfile);
  if (par.m_calcctrec) getdtpar(par.m_ctrecdt, ctrecdt, dtfile);
  if (par.m_calcctel) getdtpar(par.m_cteldt, cteldt, dtfile);
  if (par.m_calcctel2) getdtpar(par.m_cteldt2, cteldt2, dtfile);
  if (par.m_calcmxs) getdtpar(par.m_mxsdt, mxsdt, dtfile);
  if (par.m_calcant) getdtpar(par.m_antshift, antshift, dtfile,true);   // true -> allow negative values

  if (dtfile != ""){
    AH_INFO(ahlog::HIGH) << "Loading delta-time FITS file" << std::endl;

    //resolve dtfile name and query CALDB if neccessary
    dtfile = ahmission::caldb::resolve(dtfile, filetype, inst, detnam, codename, datetime);
    

    //load time intervals file
    ahsxs::sxsdt::loadSXSTimeIntervals(dtfile,tstart_in,dts);

    if (par.m_calcant && antdtpre < 0) {
      antdtpre=dts.m_antdtpre;
      // reset paramter value in current .par file
      std::stringstream ss;
      ss << antdtpre;
      std::string tmp = ss.str(); 
      ape_trad_set_string("antdtpre", tmp.c_str()); 
    }
    if (par.m_calcant && antdtfol < 0) {
      antdtfol=dts.m_antdtfol;
      std::stringstream ss;
      ss << antdtfol;
      std::string tmp = ss.str(); 
      ape_trad_set_string("antdtfol", tmp.c_str()); 
    }
    if (par.m_calcprox && proxdt < 0) { 
      proxdt=dts.m_proxdt;
      std::stringstream ss;
      ss << proxdt;
      std::string tmp = ss.str(); 
      ape_trad_set_string("proxdt", tmp.c_str());  
    }
    if (par.m_calcctrec && ctrecdt < 0) {
      ctrecdt=dts.m_ctrecdt;
      std::stringstream ss;
      ss << ctrecdt;
      std::string tmp = ss.str(); 
      ape_trad_set_string("ctrecdt", tmp.c_str());   
    }
    if (par.m_calcctel && cteldt < 0) {
      cteldt=dts.m_cteldt;
      std::stringstream ss;
      ss << cteldt;
      std::string tmp = ss.str(); 
      ape_trad_set_string("cteldt", tmp.c_str());   
    }
    if (par.m_calcctel2 && cteldt2 < 0) {
      cteldt2=dts.m_cteldt2;
      std::stringstream ss;
      ss << cteldt2;
      std::string tmp = ss.str(); 
      ape_trad_set_string("cteldt2", tmp.c_str());   
    }
    if (par.m_calcmxs && mxsdt < 0) {
      mxsdt=dts.m_mxsdt;
      std::stringstream ss;
      ss << mxsdt;
      std::string tmp = ss.str(); 
      ape_trad_set_string("mxsdt", tmp.c_str());       
    }
    if (par.m_calcant && antshift < 0) {
      if (par.m_antpsp == "A") {
        antshift=dts.m_antshift[0];
      } else { 
        antshift=dts.m_antshift[1];
      }
      std::stringstream ss;
      ss << antshift;
      std::string tmp = ss.str(); 
      ape_trad_set_string("antshift", tmp.c_str());  
    }

    // +++ 2015-12-03 MCW To protect against old versions of the CALDB file,
    // +++ 2015-12-03 MCW we check if cteldt2 is zero.  If so, it means the
    // +++ 2015-12-03 MCW CTEL2DT column is missing from the CALDB file.  
    // +++ 2015-12-03 MCW This check should be removed once the CALDB file
    if (par.m_calcctel2 && cteldt2 < 0) {
      AH_THROW_RUNTIME("flagging 2nd electrical cross-talk, but CTEL2DT column missing from CALDB file");
      cteldt2=dts.m_cteldt2;
    }

  }

  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "Delta-time constants: " << std::endl;
  AH_INFO(ahlog::HIGH) << "  antdtpre = " << antdtpre << std::endl;
  AH_INFO(ahlog::HIGH) << "  antdtfol = " << antdtfol << std::endl;
  AH_INFO(ahlog::HIGH) << "  proxdt =   " << proxdt << std::endl;
  AH_INFO(ahlog::HIGH) << "  ctrecdt =  " << ctrecdt << std::endl;
  AH_INFO(ahlog::HIGH) << "  cteldt =   " << cteldt << std::endl;
  AH_INFO(ahlog::HIGH) << "  cteldt2 =  " << cteldt2 << std::endl;
  AH_INFO(ahlog::HIGH) << "  mxsdt =    " << mxsdt << std::endl;
  AH_INFO(ahlog::HIGH) << "  antshift = " << antshift << std::endl;
  AH_INFO(ahlog::HIGH) << std::endl;

  // Create CTMULT & DTCTEL in output file, if necessary.
  if (par.m_calcctel) {
    if (!ahfits::haveColumn(fp,"CTMULT")) {
      ahfits::insertColAfter(fp,"CTMULT","I","");
      ahfits::setColumnDescription(fp,"CTMULT","Record multiplicity for electrical cross-talk");
      AH_INFO(ahlog::HIGH) << "Creating column: CTMULT" << std::endl;
    }
    if (par.m_dtflag && !ahfits::haveColumn(fp,"DTCTEL")) {
      ahfits::insertColAfter(fp,"DTCTEL","D","");
      ahfits::setColumnDescription(fp,"DTCTEL","Seconds between electrical cross-talk events");
      AH_INFO(ahlog::HIGH) << "Creating column: DTCTEL" << std::endl;
    }
  }

  // Create CTMULT2 & DTCTEL2 in output file, if necessary.
  if (par.m_calcctel2) {
    if (!ahfits::haveColumn(fp,"CTMULT2")) {
      ahfits::insertColAfter(fp,"CTMULT2","I","");
      ahfits::setColumnDescription(fp,"CTMULT2","Record multiplicity for 2nd electrical cross-talk");
      AH_INFO(ahlog::HIGH) << "Creating column: CTMULT2" << std::endl;
    }
    if (par.m_dtflag && !ahfits::haveColumn(fp,"DTCTEL2")) {
      ahfits::insertColAfter(fp,"DTCTEL2","D","");
      ahfits::setColumnDescription(fp,"DTCTEL2","Seconds between 2nd electrical cross-talk events");
      AH_INFO(ahlog::HIGH) << "Creating column: DTCTEL2" << std::endl;
    }
  }

  // Create DTCTREC in output file, if necessary.
  if (par.m_dtflag && par.m_calcctrec) {
    if (!ahfits::haveColumn(fp,"DTCTREC")) {
      ahfits::insertColAfter(fp,"DTCTREC","D","");
      ahfits::setColumnDescription(fp,"DTCTREC","Seconds between recoil cross-talk events");
      AH_INFO(ahlog::HIGH) << "Creating column: DTCTREC" << std::endl;
    }
  }

  // Create DTANT in output file, if necessary.
  if (par.m_dtflag && par.m_calcant) {
    if (!ahfits::haveColumn(fp,"DTANT")) {
      ahfits::insertColAfter(fp,"DTANT","D","");
      ahfits::setColumnDescription(fp,"DTANT","Seconds between pixel and antico events");
      AH_INFO(ahlog::HIGH) << "Creating column: DTANT" << std::endl;
    }
  }

  // Open Antico file, if needed.
  if (par.m_calcant) {
    ahfits::open(par.m_inantfile,"",&fpa);
    if (ahfits::isPrimary(fpa)) ahfits::move(fpa,antiext);   // move to EVENTS extension if extended syntax not used
    det_ant=ahfits::getKeyValStr(fpa,"DETNAM");
    if ("ANTICO" != det_ant) AH_THROW_RUNTIME("DETNAME keyword in Antico file should be ANTICO, not "+det_ant);
    AH_INFO(ahlog::HIGH) << "Open antico file: " << par.m_inantfile << "[" << ahfits::getKeyValStr(fpa,"EXTNAME") << "]" << std::endl;
  }

  // read pixel map file
  if (par.m_calcctel) {
    std::string pixdeffilename=ahmission::caldb::resolve(par.m_pixdeffile, "pixel map", "SXS", "-", "PIXMAP", "-");
    ape_trad_set_string("pixdeffile",pixdeffilename.c_str());
    AH_INFO(ahlog::LOW) << "Using pixel map file: " << pixdeffilename << std::endl;
    sxspixelmap::load(pixdeffilename,pixmapdat);
    
    // to record actual file path in par file, and in history keywords
    ape_trad_set_string("pixdeffile",pixdeffilename.c_str());
  }

  // Open GTI file and check if time range compatible with input file.
  extnumarray[0]=0;
  extnumarray[1]=0;
  extnumarray[2]=0;
  extnumarray[3]=0;
  if (par.m_calcmxs) {

    // smallest TSTART and largest TSTOP in GTI extensions
    double firsttstartgti=-1.;
    double lasttstopgti=-1.;

    AH_INFO(ahlog::HIGH) << "Opening MXS GTI file: " << par.m_inmxsgti << std::endl;
    ahfits::open(par.m_inmxsgti,"",&gp);
    ahfits::firstHDU(gp,ahfits::e_BINARY_TBL);

    bool haveled1=false;
    bool haveled2=false;
    bool haveled3=false;
    bool haveled4=false;
    do {           // loop over all HDUs of GTI file
      std::string extname=ahfits::getKeyValStr(gp,"EXTNAME");
      long nrows=ahfits::getKeyValLLong(gp,"NAXIS2");
      int n=0;     // extension number (LED index - 1)

      tstart_gti=ahfits::getKeyValDbl(gp,"TSTART");
      tstop_gti=ahfits::getKeyValDbl(gp,"TSTOP");

      if ("GTIMXSFNON1" == extname) {
        if (haveled1) {
          AH_INFO(ahlog::HIGH) << "Duplicate extension found for MXS LED1; using only first" << std::endl;
          continue;
        }
        haveled1=true;
        n=0;
      } else if ("GTIMXSFNON2" == extname) {
        if (haveled2) {
          AH_INFO(ahlog::HIGH) << "Duplicate extension found for MXS LED2; using only first" << std::endl;
          continue;
        }
        haveled2=true;
        n=1;
      } else if ("GTIMXSFNON3" == extname) {
        if (haveled3) {
          AH_INFO(ahlog::HIGH) << "Duplicate extension found for MXS LED3; using only first" << std::endl;
          continue;
        }
        haveled3=true;
        n=2;
      } else if ("GTIMXSFNON4" == extname) {
        if (haveled4) {
          AH_INFO(ahlog::HIGH) << "Duplicate extension found for MXS LED4; using only first" << std::endl;
          continue;
        }
        haveled4=true;
        n=3;
      } else {
        continue;
      }

      // store number of rows
      extnumarray[n]=nrows;

      if (nrows > 0) {     // do not consider TSTART/TSTOP if HDU is empty
        if (firsttstartgti <= 0. || lasttstopgti <= 0.) {
          firsttstartgti=tstart_gti;
          lasttstopgti=tstop_gti;
        } else {
          if (tstop_gti > lasttstopgti) lasttstopgti=tstop_gti;
          if (tstart_gti < firsttstartgti) firsttstartgti=tstart_gti;
        }
      }
    } while (ahfits::nextHDU(gp,ahfits::e_BINARY_TBL));
    if (!haveled1) AH_INFO(ahlog::HIGH) << "No extension found for MXS LED1" << std::endl;
    if (!haveled2) AH_INFO(ahlog::HIGH) << "No extension found for MXS LED2" << std::endl;
    if (!haveled3) AH_INFO(ahlog::HIGH) << "No extension found for MXS LED3" << std::endl;
    if (!haveled4) AH_INFO(ahlog::HIGH) << "No extension found for MXS LED4" << std::endl;

    // Check for overlap with input file.
    if (firsttstartgti >= 0. && lasttstopgti >= 0.) {    // if negative, then no non-empty MXS HDU found
      if (lasttstopgti < tstart_in || firsttstartgti > tstop_in) {
        AH_INFO(ahlog::HIGH) << "TSTART/TSTOP range for input and GTI files not compatible; skipping MXS flagging" << std::endl;
        par.m_calcmxs=false;
      }
    } else {     // all GTI are empty
      AH_INFO(ahlog::HIGH) << "All MXS GTI extensions are empty; skipping MXS flagging" << std::endl;
      par.m_calcmxs=false;
    }
  }

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 
}

// ****************************************************************************

void doWork(ahfits::FilePtr fp, ahfits::FilePtr fpa,  ahfits::FilePtr gp, Par par,
            double antshift, double antdtpre, double antdtfol, double proxdt, 
            double ctrecdt, double cteldt, double cteldt2, double mxsdt,
            long* extnumarray, sxspixelmap::DataType& pixmapdat, bool& clean) {

  // Reset STATUS bits based on resetflags parameter
  reset_status(fp,par.m_resetflags_values,clean);

  // GTI
  if (ahgen::strtoupper(par.m_gtifile) != "NONE")
    flag_gti(fp,par.m_gtifile,par.m_ckrisetime,clean);

  // ANTICO
  if (par.m_calcant)
    process_antico(fp,fpa,par.m_ckrisetime,par.m_antpsp,antshift,antdtpre,
                   antdtfol,par.m_antswitch,par.m_antphathr,par.m_antdurthr,
                   par.m_dtflag,clean);

  // PROXIMTY
  if (par.m_calcprox) {
    int ctelidx=1;   // process electrical cross-talk with 1st delta-time (not applicable to this check, but need a value)
    bool calcprox=true;
    bool calcctel=false;
    bool calcctrec=false;
    bool usepxpithr=false;
    if (par.m_usepxpithr_values.count(e_PROX) > 0) usepxpithr=true;
    process_crosstalk(fp,par.m_ckrisetime,ctrecdt,cteldt,par.m_ctelnear,proxdt,
                      calcctrec,calcctel,calcprox,par.m_dtflag,par.m_kalow,
                      par.m_kahigh,par.m_kbeta,ctelidx,usepxpithr,par.m_pxpithr,
                      pixmapdat,clean);
  }

  // ELECTRICAL CROSS TALK
  if (par.m_calcctel) {
    int ctelidx=1;   // process electrical cross-talk with 1st delta-time
    bool calcprox=false;
    bool calcctel=true;
    bool calcctrec=false;
    bool usepxpithr=false;
    if (par.m_usepxpithr_values.count(e_CTEL) > 0) usepxpithr=true;
    process_crosstalk(fp,par.m_ckrisetime,ctrecdt,cteldt,par.m_ctelnear,proxdt,
                      calcctrec,calcctel,calcprox,par.m_dtflag,par.m_kalow,
                      par.m_kahigh,par.m_kbeta,ctelidx,usepxpithr,par.m_pxpithr,
                      pixmapdat,clean);
  }

  // RECOIL CROSS TALK
  if (par.m_calcctrec) {
    int ctelidx=1;   // process electrical cross-talk with 1st delta-time (not applicable to this check, but need a value)
    bool calcprox=false;
    bool calcctel=false;
    bool calcctrec=true;
    bool usepxpithr=false;
    if (par.m_usepxpithr_values.count(e_CTREC) > 0) usepxpithr=true;
    process_crosstalk(fp,par.m_ckrisetime,ctrecdt,cteldt,par.m_ctelnear,proxdt,
                      calcctrec,calcctel,calcprox,par.m_dtflag,par.m_kalow,
                      par.m_kahigh,par.m_kbeta,ctelidx,usepxpithr,par.m_pxpithr,
                      pixmapdat,clean);
  }

  // 2ND ELECTRICAL CROSS TALK
  if (par.m_calcctel2) {
    int ctelidx=2;   // process electrical cross-talk with 1st delta-time
    bool calcprox=false;
    bool calcctel=true;
    bool calcctrec=false;
    bool usepxpithr=false;
    if (par.m_usepxpithr_values.count(e_CTEL2) > 0) usepxpithr=true;
    process_crosstalk(fp,par.m_ckrisetime,ctrecdt,cteldt2,par.m_ctelnear,proxdt,
                      calcctrec,calcctel,calcprox,par.m_dtflag,par.m_kalow,
                      par.m_kahigh,par.m_kbeta,ctelidx,usepxpithr,par.m_pxpithr,
                      pixmapdat,clean);
  }

  // flag MXS
  if (par.m_calcmxs)
    process_mxs(fp,gp,par.m_ckrisetime,mxsdt,extnumarray,clean);

}

// ****************************************************************************

void finalize(ahfits::FilePtr fp, ahfits::FilePtr fpa, ahfits::FilePtr gp,
              bool clean) {

  // get file name to delete if there is an error
  std::string filename;
  if (0 != fp) filename=fp->m_filename;

  // close all open files
  ahfits::close(fp);
  ahfits::close(fpa);
  ahfits::close(gp);

  // delete output file if there was an early error
  if (clean && filename != "" && ahgen::fileExists(filename)) std::remove(filename.c_str());

}

/** @} */


/* Revision Log
 $Log: sxsflagpix.cxx,v $
 Revision 1.68  2016/10/07 15:33:24  mwitthoe
 sxsflagpix: instead of throwing an error, now print log message and skip MXS flagging if MXS GTI times do not overlap with the input file

 Revision 1.67  2016/08/22 23:20:35  mwitthoe
 sxsflagpix: fix bug where negative values for antshift were not permitted when entered manually (CALDB was OK); now antshift can be positive or negative

 Revision 1.66  2016/08/11 20:07:01  mwitthoe
 sxsflagpix: 1) switch meaning on 0/1 for GTI STATUS bits - 0 means invalid event or not in GTI, 1 means valid event in GTI; 2) allow negative pxpithr values

 Revision 1.65  2016/08/10 19:42:29  mwitthoe
 sxsflagpix: fix bug where the cteldt2 value was not being retrieved from CALDB

 Revision 1.64  2016/08/10 16:53:11  mwitthoe
 sxsflagpix: add support for pixel-dependent GTI files in two formats: 1) one extension per pixel, or 2) one extension with a PIXEL column

 Revision 1.63  2016/07/25 18:46:47  mwitthoe
 sxsflagpix: 1) skip events with PHA/PI/EPI=NULL; 2) do not include pixel 12 events in proximity check; 3) add parameters pxpithr/usepxpithr for setting a PI threshold for including events in the proximity and cross-talk checks; 4) change meaning of resetflags parameter to be a list of flagging types to reset the STATUS bits for

 Revision 1.62  2016/05/24 19:40:12  mwitthoe
 sxsflagpix: add new parameter, resetflags, to set all STATUS bits to zero before flagging new bits

 Revision 1.61  2016/04/01 21:10:23  rshill
 Added status bit table to prolog.

 Revision 1.60  2016/03/24 12:57:13  mdutka
 making changes described in issue #610

 Revision 1.59  2015/12/23 20:18:04  mwitthoe
 sxsflagpix: throw error if input FITS table has no rows

 Revision 1.58  2015/12/03 19:36:14  mwitthoe
 sxsflagpix: perform a 2nd electrical cross-talk check using a different delta-time

 Revision 1.57  2015/11/17 20:45:44  mwitthoe
 sxsflagpix: add column comments

 Revision 1.56  2015/10/16 20:19:48  mwitthoe
 sxsflagpix: fix bug where TSTART/TSTOP from empty MXS GTI extensions were checked against the TSTART/TSTOP range of the input event file.  Now TSTART/TSTOP from empty extensions are ignored.  If all MXS GTI extensions are empty then no check against the event TSTART/TSTOP is performed

 Revision 1.55  2015/10/13 20:48:45  mwitthoe
 sxsflagpix: update names of MXS extensions (they were recently changed in mxstime)

 Revision 1.54  2015/10/07 16:53:21  mwitthoe
 sxsflagpix: add ckrisetime parameter to skip events with RISE_TIME>127

 Revision 1.53  2015/10/02 20:37:25  mwitthoe
 sxsflagpix: remove unnecessary pixmap include statement

 Revision 1.52  2015/10/02 18:37:42  mwitthoe
 sxsflagpix: switch CALDB query over to general function

 Revision 1.51  2015/09/25 20:02:10  mwitthoe
 sxsflagpix: add parameter, antdurthr, which is the DURATION threshold for accepting antico events; if an antico event has a DURATION less than antdurthr, then it is ignored

 Revision 1.50  2015/09/23 14:22:15  mwitthoe
 sxsflagpix: delete output file if there is an error before processing

 Revision 1.49  2015/09/23 14:11:24  mwitthoe
 sxsflagpix: 1) add dtflag parameter which switches on new delta-time output columns for cross-talk and antico (diagnostics); 2) throw error if discovered that TIME column is not sorted; 3) perform recoil cross-talk energy check with EPI instead of PHA; 4) fix parameter description for calcprox; 5) fix afterglow counter for MXS flagging and clarify log messages

 Revision 1.48  2015/08/03 20:39:19  mdutka
 now using general CALDB query routine

 Revision 1.47  2015/07/30 16:35:24  mwitthoe
 sxsflagpix: add parameter stamping to log file

 Revision 1.46  2015/07/30 16:07:14  mwitthoe
 sxsflagpix: clean up tool; see issues 532, 534, and 543

 Revision 1.45  2015/07/15 20:48:29  mdutka
 fixing bug with flagpix caldb query

 Revision 1.44  2015/07/13 15:11:36  mdutka
 Adding CALDB queries

 Revision 1.43  2015/04/28 16:32:03  mwitthoe
 sxsflagpix: change pspid parameter to antpsp where antico PSP_ID values are allowed to be 1-4 instead of 1 & 3 only; remove TSTART/TSTOP comparison between event and antico files; see issue 515

 Revision 1.42  2015/04/03 18:38:28  mwitthoe
 sxsflagpix: convert dtfile parameter to uppercase before checking for CALDB

 Revision 1.41  2015/04/02 20:18:32  mwitthoe
 sxsflagpix: fix typo in error message

 Revision 1.40  2015/04/01 19:52:06  mwitthoe
 sxsflagpix: fix bug where mxsdt parameter was being read as a double instead of a string

 Revision 1.39  2015/03/23 20:48:48  mwitthoe
 sxsflagpix: remove check on FLG_PARITY_ERR column; see issue 497

 Revision 1.38  2015/03/23 18:42:25  mwitthoe
 sxsflagpix: update tool after format change of the DT CALDB file (antshift now a 2D column; per PSP_ID)

 Revision 1.37  2015/03/20 17:09:29  mwitthoe
 sxsflagpix: fix bug where a variable was not being initialized leading to a different result on various machines

 Revision 1.36  2015/03/18 20:43:40  mwitthoe
 sxsflagpix: change DETNAME to DETNAM

 Revision 1.35  2015/03/12 14:29:20  mwitthoe
 sxsflagpix: skip antico rows with FLG_EVENT_LOST or FLG_PARITY_ERR columns set to 1; add parameters for kalow/kahigh/kbeta energies used in recoil cross-talk check

 Revision 1.34  2015/03/11 17:37:14  mwitthoe
 sxsflagpix: add support for proxdt to be taken from the sxsdt CALDB file (issue 475); move GTI flagging from initialize() to doWork(); remove debugging cout statements

 Revision 1.33  2015/01/12 21:17:58  asargent
 Fixed bug during getPar where final check for okay parameters was checking that ALL booleans were bad rather than ANY. Also added calcprox to parameter boolean check. See issue 478

 Revision 1.32  2015/01/09 22:45:53  mwitthoe
 sxsflagpix: add check for maximum group size and increase the MAXGROUPSIZE constant

 Revision 1.31  2015/01/09 02:53:39  mwitthoe
 sxsflagpix: allow CALDB for dt parameters; debug antico and cross-talk subroutines using SXS PSP test data

 Revision 1.30  2014/12/29 20:51:14  mwitthoe
 sxsflagpix: make parameter changes; see issue 472

 Revision 1.29  2014/11/19 19:02:45  mdutka
 Removed tab character

 Revision 1.28  2014/11/19 18:38:12  mdutka
 Line 1186 set_status_mxs(false,true,offset,l_status) was true,true ect.

 Revision 1.26  2014/11/12 15:40:38  mdutka
 Made changes to sxsflagpix detailed in issue #448

 Revision 1.25  2014/09/15 21:09:50  mwitthoe
 sxsflagpix: add support for extended syntax for the input and antico files; 
issue 179

 Revision 1.24  2014/07/01 19:07:59  mdutka
 1. CTMULT column only created if crosstalk is flagged 2. changed documentation
 and LENSTATUS variable such that status flags are now 10 bits 3. added BASELINE
 as a possible value for the DETNAME keyword.

 Revision 1.23  2014/01/22 20:40:21  mwitthoe
 sxsflagpix: update accordng to code review; issue 331

 Revision 1.22  2014/01/15 22:26:32  peachey
 Add Peer Review comments regarding variable declarations,

 Revision 1.21  2014/01/03 22:02:26  mwitthoe
 sxsflagpix: update standard main, see issue 327

 Revision 1.20  2013/12/02 23:03:16  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.19  2013/11/26 21:47:41  mwitthoe
 sxsflagpix: add CTMULT column to output if not present in input file

 Revision 1.18  2013/11/22 18:02:28  mwitthoe
 sxsflagpix: TSTART/TSTOP times were not being read from Antico input file, but were being compared to values from main input file; this is now fixed (thanks to Andy to discovering bug); my editor stripped all trailing spaces from file, thus the large number of other changes

 Revision 1.17  2013/11/13 21:55:37  mwitthoe
 sxsflagpix: change STATUS column from I type to 10X (bit-type)

 Revision 1.16  2013/11/12 17:45:02  mwitthoe
 sxsflagpix: moved getPar_warn() functions to ahapp; renamed sxspixel map to sxsflagpixlib; worked on standards compliance for source (mainly moving variable declarations to top of scope)

 Revision 1.15  2013/10/07 21:20:11  mwitthoe
 sxsflagpix: switch over to new ahfits connection functions (issue 270)

 Revision 1.14  2013/09/11 18:28:57  mwitthoe
 sxsflagpix: prepare for redefinition of empty string to represent primary extension in ahfits

 Revision 1.13  2013/08/12 19:24:21  mwitthoe
 sxsflagpix: process checklist submitted by Reggie on 2013-07-29

 Revision 1.12  2013/07/31 19:04:09  mwitthoe
 sxsflagpix: remove unnecessary include for ahmission

 Revision 1.11  2013/07/31 19:00:42  mwitthoe
 sxsflagpix: remove unnecessary include for ahgen

 Revision 1.10  2013/07/31 18:59:12  mwitthoe
 sxsflagpix: bugfixes: routines writing STATUS column had not been updated to new definition; extraneous value being stored in the PHA group vector; PHA group for sorting was not created in the right place, causing the PHA ordering to be off; number of events not calculated for cross-talk

 Revision 1.9  2013/07/22 20:17:21  mwitthoe
 sxsflagpix: resolve +++ comment concerning extension name of Antico file; it is EVENTS

 Revision 1.8  2013/04/16 17:19:26  mwitthoe
 sxsflagpix: change source to match latest TRF (major change to cross-talk algorithm, add MXS algorithm); modify form ahcaldb library, sxspixelmap, to follow new standards

 Revision 1.7  2013/03/26 15:15:10  mwitthoe
 update standard main for sxsflagpix (issue 230)

 Revision 1.6  2013/02/19 21:29:42  mwitthoe
 sxsflagpix: use calcctrec and calcctel flags in cross-talk algorithm

 Revision 1.5  2013/01/23 19:19:58  mwitthoe
 make correction to comment in sxsflagpix

 Revision 1.4  2013/01/16 02:06:32  mwitthoe
 add short description to sxsflagpix

 Revision 1.3  2013/01/16 01:05:19  mwitthoe
 sxsflagpix: changes to tool based on 2012-01-04 checklist: make variables to hold column names (set in initialize); insert CTMULT column if not present in FITS file

 Revision 1.2  2013/01/07 20:20:35  mwitthoe
 sxsflagpix: fix range of ctelnear parameter, fix name of multiplicity column, add energy condition to recoil cross-talk check

 Revision 1.1  2013/01/02 19:11:30  mwitthoe
 add new SXS tool: sxsflagpix

*/

