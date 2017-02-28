/// \file sxsflagpixlib.cxx
/// \brief functions for sxsflagpix
/// \author Mike Witthoeft
/// \date $Date: 2016/08/23 15:56:35 $
 
#define AHLABEL tool_sxsflagpix_sxsflagpixlib
#define AHCVSID "$Id: sxsflagpixlib.cxx,v 1.29 2016/08/23 15:56:35 mwitthoe Exp $"

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahmission/ahmission.h"
#include "ahsxs/sxsstatus.h"
#include "ahsxs/pixgti.h"
#include "sxsflagpixlib.h"

#include "headas.h"           // expand_item_list

#include <sstream>
#include <cmath>
#include <vector>
#include <algorithm>     // std::sort


// ---------------------------------------------------------------------------

namespace sxspixelmap {

// ---------------------------------------------------------------------------

/// \callgraph
void load(const std::string & filename, DataType & dat) {

  // Declare ahfits pointer and variables to store column values.
  ahfits::FilePtr fptr=0;
  int l_pixel=0;
  int l_xmside=0;
  int l_xmpixel=0;
  int l_spcquad=0;
  int l_spcpixel=0;

  // Must clear() before loading again.
  clear(dat);

  // Open file.
  ahfits::open(filename,"PIXMAP",&fptr);
  if (!ahfits::readOK(fptr)) {
    std::stringstream msg;
    msg << "CALDB file with SXS pixel map, " << filename << "[PIXMAP], has no data";
    AH_THROW_RUNTIME(msg.str());
  }

  // Setup router.
  ahfits::Router router(fptr);

  // Make connections to local variables.
  router.connectScalar(ahfits::e_READONLY,"PIXEL",l_pixel);
  router.connectScalar(ahfits::e_READONLY,"XMSIDE",l_xmside);
  router.connectScalar(ahfits::e_READONLY,"XMPIXEL",l_xmpixel);
  router.connectScalar(ahfits::e_READONLY,"SPCQUAD",l_spcquad);
  router.connectScalar(ahfits::e_READONLY,"SPCPIXEL",l_spcpixel);

  // Read table.
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    dat.m_xmside[l_pixel]=l_xmside;
    dat.m_xmpixel[l_pixel]=l_xmpixel;
    dat.m_spcquad[l_pixel]=l_spcquad;
    dat.m_spcpixel[l_pixel]=l_spcpixel;
  }

  // Close FITS file.
  ahfits::close(fptr);
}

// ---------------------------------------------------------------------------

void clear(DataType & dat) {
  dat.m_xmside.clear();
  dat.m_xmpixel.clear();
  dat.m_spcquad.clear();
  dat.m_spcpixel.clear();
}

// ---------------------------------------------------------------------------

double get_xmside(DataType & dat, int pixel) {
  return dat.m_xmside[pixel];
}

// ---------------------------------------------------------------------------

double get_xmpixel(DataType & dat, int pixel) {
  return dat.m_xmpixel[pixel];
}

// ---------------------------------------------------------------------------

double get_spcquad(DataType & dat, int pixel) {
  return dat.m_spcquad[pixel];
}

// ---------------------------------------------------------------------------

double get_spcpixel(DataType & dat, int pixel) {
  return dat.m_spcpixel[pixel];
}

// ---------------------------------------------------------------------------

}  // namespace sxspixelmap


// ---------------------------------------------------------------------------

void getdtpar(const std::string& parval,double& value,std::string& dtfile,
              bool allowneg) {
  if (ahgen::isNumber(std::string(parval))) {
    value = atof(parval.c_str());
    if (!allowneg && value < 0) AH_THROW_RUNTIME("Parameter value cannot be negative"); 
  } else {
    if (dtfile != "" && parval != dtfile) {
      AH_THROW_RUNTIME("Parameter filenames must be the same");
    } else {
      dtfile=parval;
    }
  }
}  

// ---------------------------------------------------------------------------

void parse_flagging_types(const std::string& parname, const std::string& parval,
                          std::set<int>& types) {
  int status=0;
  int trim=1;        // trim spaces
  int skip=1;        // exclude empty items
  int guard=0;       // do not protect against commas in parentheses
  char* in=(char*)parval.c_str();
  char** items=0;
  int nitems=0;
  items=expand_item_list(in,&nitems,',',trim,skip,guard,&status);
  if (0 != status) {
    free(items);
    std::stringstream msg1;
    msg1 << "expand_item_list failed with status " << status;
    AH_DEBUG << msg1.str() << std::endl;
    
    std::stringstream msg2;
    msg2 << "Could not parse " << parname << " parameter; should be comma-delimited list of strings";
    AH_THROW_RUNTIME(msg2.str());
  }

  bool haveall=false;
  bool havenone=false;
  for (int i=0; i < nitems; i++) {
    if (ahgen::strtoupper(std::string(items[i])) == "ALL") {
      haveall=true;
      types.insert(e_ANT);
      types.insert(e_PROX);
      types.insert(e_CTEL);
      types.insert(e_CTEL2);
      types.insert(e_CTREC);
      types.insert(e_GTI);
      types.insert(e_MXS);
    } else if (ahgen::strtoupper((std::string)items[i]) == "NONE") {
      havenone=true;
    } else if (ahgen::strtoupper((std::string)items[i]) == "ANT") {
      types.insert(e_ANT);
    } else if (ahgen::strtoupper((std::string)items[i]) == "PROX") {
      types.insert(e_PROX);
    } else if (ahgen::strtoupper((std::string)items[i]) == "CTEL") {
      types.insert(e_CTEL);
    } else if (ahgen::strtoupper((std::string)items[i]) == "CTEL2") {
      types.insert(e_CTEL2);
    } else if (ahgen::strtoupper((std::string)items[i]) == "CTREC") {
      types.insert(e_CTREC);
    } else if (ahgen::strtoupper((std::string)items[i]) == "GTI") {
      types.insert(e_GTI);
    } else if (ahgen::strtoupper((std::string)items[i]) == "MXS") {
      types.insert(e_MXS);
    } else {
      std::stringstream msg;
      msg << parname << ": flagging type, " << items[i] << ", not supported; skipping";
      AH_INFO(ahlog::HIGH) << msg.str() << std::endl;
    }
  }
  if (haveall && havenone) {
    std::stringstream msg;
    msg << parname << ": cannot contain ALL and NONE";
    AH_THROW_RUNTIME(msg.str());
  } else if (havenone && types.size() > 0) {
    std::stringstream msg;
    msg << parname << ": cannot combine NONE with other types";
    AH_THROW_RUNTIME(msg.str());
  } else if (!havenone && types.size() == 0) {
    std::stringstream msg;
    msg << parname << ": no valid flagging types found";
    AH_THROW_RUNTIME(msg.str());
  }
}

// ---------------------------------------------------------------------------

void reset_status(ahfits::FilePtr fp, const std::set<int>& types, bool& clean) {

  ahfits::IndexType num_status=ahsxs::LEN_SXSSTATUS;
  type_status l_status[ahsxs::LEN_SXSSTATUS];
  for (int istat=0; istat < ahsxs::LEN_SXSSTATUS; istat++) {
    l_status[istat]=0;
  }

  ahfits::Router router(fp);
  router.connectBit(ahfits::e_READWRITE,"STATUS",l_status,num_status);

  clean=false;       // false => do not delete output file upon error
  for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp)) {
    ahfits::readRow(fp);

    if (types.count(e_GTI) > 0) {
      l_status[ahsxs::e_STATUS_GTI_GENERAL]=0;
      l_status[ahsxs::e_STATUS_GTI_PIXEL]=0;
    }
    if (types.count(e_ANT) > 0) {
      l_status[ahsxs::e_STATUS_ANTICO]=0;
    }
    if (types.count(e_PROX) > 0) {
      l_status[ahsxs::e_STATUS_PROXIMITY]=0;
    }
    if (types.count(e_CTREC) > 0) {
      l_status[ahsxs::e_STATUS_NEAR_CALPIX]=0;
      l_status[ahsxs::e_STATUS_RECOIL]=0;
    }
    if (types.count(e_CTEL) > 0) {
      l_status[ahsxs::e_STATUS_ELECTRICAL]=0;
      l_status[ahsxs::e_STATUS_ELEC_MAX]=0;
    }
    if (types.count(e_MXS) > 0) {
      l_status[ahsxs::e_STATUS_MXS_DIRECT]=0;
      l_status[ahsxs::e_STATUS_MXS_DIRECT_GLOW]=0;
      l_status[ahsxs::e_STATUS_MXS_INDIRECT]=0;
      l_status[ahsxs::e_STATUS_MXS_INDIRECT_GLOW]=0;
    }
    if (types.count(e_CTEL2) > 0) {
      l_status[ahsxs::e_STATUS_ELECTRICAL2]=0;
      l_status[ahsxs::e_STATUS_ELEC_MAX2]=0;
    }
    l_status[ahsxs::e_STATUS_RESERVED_1]=0;
    l_status[ahsxs::e_STATUS_RESERVED_2]=0;

    ahfits::writeRow(fp);
  }

}

// ---------------------------------------------------------------------------

void process_crosstalk(ahfits::FilePtr fp, bool ckrisetime, double ctrecdt,
                       double cteldt, int ctelnear, double proxdt, bool calcctrec,
                       bool calcctel, bool calcprox, bool dtflag, double kalow,
                       double kahigh, double kbeta, int ctelidx, bool usepxpithr,
                       int pxpithr, sxspixelmap::DataType& pixmapdat,bool& clean) {

  AH_INFO(ahlog::HIGH) << "Processing cross-talk" << std::endl;

  // If ctelidx == 2, then prox and ctrec should not be processed
  if (ctelidx == 2) {
    if (calcctrec || calcprox) AH_THROW_LOGIC("when processing electrical cross-talk 2; recoil cross-talk and proximity checks should be disabled");
  }

  // Track statistics.
  long noevent=0;
  long norecoil=0;
  long noelectrical=0;
  long noelectricalpr=0;
  long noprox=0;

  // Local variables to connect with FITS columns.
  CTRow l_row;

  // Local variables to store first point of next queue.
  CTRow b_row;

  double dtmax=0;               // largest delta-t determines grouping of points
  long currow=0;                // current row in file
  bool eof=false;               // end-of-file reached
  double timeprev=0.;           // TIME of previous event
  double time_ctelmax=0.;       // TIME of electrical cross-talk source event (used in DTCTEL calculation)
  type_pha pha_ctelmax=0.;      // PHA  of electrical cross-talk source event (used in DTCTEL calculation)

  // Initialize queues to hold point data.
  std::vector<CTRow> q_row;

  // Largest delta-t determines grouping of points.
  dtmax = std::max(cteldt, ctrecdt); //group by largest delta-t
  dtmax = std::max(dtmax, proxdt);

  // Set up local variables and router for reading/writing file.
  ahfits::Router router(fp,MAXGROUPSIZE);    // use overlap of MAXGROUPSIZE rows in buffer
  router.connectScalar(ahfits::e_READONLY,"TIME",l_row.m_time, &l_row.m_time_null);
  router.connectScalar(ahfits::e_READONLY,"PIXEL",l_row.m_pixel,&l_row.m_pixel_null);
  router.connectScalar(ahfits::e_READONLY,"PHA",l_row.m_pha,&l_row.m_pha_null);
  if (usepxpithr) router.connectScalar(ahfits::e_READONLY,"PI",l_row.m_pi,&l_row.m_pi_null);
  if (calcctrec) router.connectScalar(ahfits::e_READONLY,"EPI",l_row.m_epi,&l_row.m_epi_null);
  router.connectScalar(ahfits::e_READONLY,"RISE_TIME",l_row.m_rise_time);
  router.connectBit(ahfits::e_READWRITE,"STATUS",l_row.m_status,l_row.num_status);
  if (calcctel) {
    if (ctelidx == 1)
      router.connectScalar(ahfits::e_WRITEONLY,"CTMULT",l_row.m_mult);
    else
      router.connectScalar(ahfits::e_WRITEONLY,"CTMULT2",l_row.m_mult);
  }
  router.connectScalar(ahfits::e_READONLY,"ITYPE",l_row.m_itype);
  router.connectBit(ahfits::e_READWRITE,"PROC_STATUS",l_row.m_proc_status,l_row.num_proc_status);
  if (dtflag && calcctel) {
    if (ctelidx == 1) 
      router.connectScalar(ahfits::e_WRITEONLY,"DTCTEL",l_row.m_dtctel, &l_row.m_dtctel_null);
    else
      router.connectScalar(ahfits::e_WRITEONLY,"DTCTEL2",l_row.m_dtctel, &l_row.m_dtctel_null);
  }
  if (dtflag && calcctrec)
    router.connectScalar(ahfits::e_WRITEONLY,"DTCTREC",l_row.m_dtctrec, &l_row.m_dtctrec_null);

  // Go to first row input.
  clean=false;       // false => do not delete output file upon error
  ahfits::firstRow(fp);

  // Populate queues with first valid row data.
  ahfits::readRow(fp);
  l_row.m_row=ahfits::currentRow(fp);
  while (l_row.m_itype >= 5 || !(procstatus::processRow(l_row.m_proc_status)) || 
        l_row.m_time_null == 1 || (ckrisetime && (l_row.m_rise_time > 127)) ||
        l_row.m_pixel_null == 1 || l_row.m_pha_null == 1 || l_row.m_epi_null == 1 ||
        (usepxpithr && (l_row.m_pi_null == 1 || l_row.m_pi < pxpithr)) ) {
    // skipping row, so write DTCTEL and DTCTREC columns as NULL
    l_row.m_dtctel_null=1;
    l_row.m_dtctrec_null=1;
    l_row.m_mult=0;

    // if PIXEL=NULL, also set the GSFC bit of PROC_STATUS to bad
    if (l_row.m_pixel_null == 1) {
      l_row.m_proc_status[16]=1;
    }

    ahfits::writeRow(fp);

    // read next row
    ahfits::nextRow(fp);
    if (!ahfits::readOK(fp)) break;
    ahfits::readRow(fp);
  }
  if (!ahfits::readOK(fp)) {   // no valid rows in file; leave function
    return;
  }
  currow=ahfits::currentRow(fp);
  timeprev=l_row.m_time;

  noevent++;
  q_row.push_back(l_row);

  // Populate and process single group of points.
  while (!eof) {

    // Vector of PHA values for easy sorting of electrical cross-talk signals.
    std::vector<type_pha> phas;

    // Done if queue is empty.
    if (q_row.size() == 0) break;

    // Read line from file and add point to group
    // (will continue to next while iteration until group is finished).
    if (!eof) {
      currow++;
      ahfits::gotoRow(fp,currow);   // reset location to next row to be read
      if (!ahfits::readOK(fp)) {
        eof=true;
      } else {
        ahfits::readRow(fp);
        l_row.m_row=ahfits::currentRow(fp);
        while (l_row.m_itype >= 5 || !(procstatus::processRow(l_row.m_proc_status)) || 
        l_row.m_time_null == 1 || (ckrisetime && (l_row.m_rise_time > 127)) ||
        l_row.m_pixel_null == 1 || l_row.m_pha_null == 1 || l_row.m_epi_null == 1 ||
        (usepxpithr && (l_row.m_pi_null == 1 || l_row.m_pi < pxpithr)) ) {
          // skipping row, so write DTCTEL and DTCTREC columns as NULL
          l_row.m_dtctel_null=1;
          l_row.m_dtctrec_null=1;
          l_row.m_mult=0;

          // if PIXEL=NULL, also set the GSFC bit of PROC_STATUS to bad
          if (l_row.m_pixel_null == 1) {
            l_row.m_proc_status[16]=1;
          }

          ahfits::writeRow(fp);
      
          // read next row
          ahfits::nextRow(fp);
          currow++;
          if (!ahfits::readOK(fp)) break;
          ahfits::readRow(fp);
          l_row.m_row=ahfits::currentRow(fp);
        }
        if (!ahfits::readOK(fp)) {
          eof=true;
        } else {
          if (l_row.m_time < timeprev) AH_THROW_RUNTIME("Event file is not sorted by TIME");
            timeprev=l_row.m_time;
        }
      }
      if (!eof) {
        noevent++;
        if ( (l_row.m_time - q_row.back().m_time) < dtmax) {    // add point to queue
          q_row.push_back(l_row);
          continue;    // next while iteration
        } else {                                  // first point of next queue
          b_row=l_row;
        }
      }
    }

    // Note: at this point we have a list of points to be processed
    //       and the starting point for the next iteration.

    // Need separate loop to check for electrical cross-talk since we also
    // need the list of all EPI values to get the relative order.
    time_ctelmax=0.;
    pha_ctelmax=0;
    for (long ip=0; ip < (long)q_row.size(); ip++) {

      for (long jp=ip+1; jp < (long)q_row.size(); jp++) {

        if (calcctel && std::abs(q_row[ip].m_time-q_row[jp].m_time) < cteldt ) {

          // same quadrant?
          if ( sxspixelmap::get_spcquad(pixmapdat,q_row[ip].m_pixel) ==
               sxspixelmap::get_spcquad(pixmapdat,q_row[jp].m_pixel) ) {

            // pixels near enough?
            if ( std::abs(sxspixelmap::get_spcpixel(pixmapdat,q_row[ip].m_pixel) -
                          sxspixelmap::get_spcpixel(pixmapdat,q_row[jp].m_pixel)) <= ctelnear) {
              q_row[ip].m_isel=true;
              q_row[jp].m_isel=true;    // this will ensure that the last pixel in the group is flagged properly
            }
          }
        }

      }
      // If ip event has any electrical cross-talk, count and store PHA
      if (q_row[ip].m_isel == true) {
        noelectrical++;
        phas.push_back(q_row[ip].m_pha);
        if (q_row[ip].m_pha > pha_ctelmax) {
          pha_ctelmax=q_row[ip].m_pha;
          time_ctelmax=q_row[ip].m_time;
        }
      }
    }
    if (phas.size() > 9)
      AH_DEBUG << "*** Discovered electrical cross talk group too many members: " << phas.size() << "; setting CTMULT=00" << std::endl;

    // Sort array of EPIs vector in descending order for filling CTMULT column
    // later.
    std::sort(phas.begin(),phas.end());
    std::reverse(phas.begin(),phas.end());

    // Goto location in file corresponding to first point in queue.
    ahfits::gotoRow(fp,q_row[0].m_row);

    // Loop over points in queue.
    for (long ip=0; ip < (long)q_row.size(); ip++) {

      type_mult mult=0;       // combination of multiplicity and order index of current pixel
                              // 2-digit number: 1st digit = multiplicity; 2nd digit = order
      type_status status[ahsxs::LEN_SXSSTATUS];
      for (int istat=0; istat < ahsxs::LEN_SXSSTATUS; istat++) {
        status[istat]=0;
      }

      // Initialize status flags.
      bool prox_intime=false;
      bool rec_is12=false;
      bool rec_eOK=false;
      bool el_intime=false;
      bool el_prime=false;

      l_row.m_dtctrec_null=1;
      for (long jp=0; jp < (long)q_row.size(); jp++) {
        // Point cannot cross-talk with itself.
        if (jp == ip) continue;
        
        // Check proximity.
        if (calcprox && q_row[ip].m_pixel !=12 && q_row[jp].m_pixel != 12 && 
            std::abs(q_row[ip].m_time-q_row[jp].m_time) < proxdt) {
          prox_intime=true;
        }

        // Check recoil cross-talk.
        if (calcctrec && std::abs(q_row[ip].m_time-q_row[jp].m_time) < ctrecdt) {
          if ( (q_row[ip].m_pixel == 12) != (q_row[jp].m_pixel == 12) ) {   // i XOR j is pixel 12
            rec_is12=true;
            if ( (q_row[ip].m_epi <= kalow) || (q_row[ip].m_epi > kahigh && q_row[ip].m_epi < kbeta)) {
              rec_eOK=true;
              if (q_row[ip].m_pixel == 12) {      // do not assign delta-time for pixel 12 event
                l_row.m_dtctrec_null=1;
              } else {
                l_row.m_dtctrec_null=0;
                l_row.m_dtctrec=q_row[ip].m_time-q_row[jp].m_time;
              }
            }
          }
        }

      }   // end for loop: jp

      // Proximity and recoil counts (counts must be outside jp loop to prevent
      // double counting).
      if (calcctrec && rec_is12) norecoil++;
      if (calcprox && prox_intime) noprox++;

      // Determine multiplicity and order index for current event.
      l_row.m_dtctel_null=1;
      if (calcctel && q_row[ip].m_isel && phas.size() > 1) {
        int tmult=phas.size();                                                                         // multiplicity
        int order=std::distance(phas.begin(),std::find(phas.begin(),phas.end(),q_row[ip].m_pha));      // order index
        mult=10*tmult+order;
        l_row.m_dtctel_null=0;
        l_row.m_dtctel=q_row[ip].m_time-time_ctelmax;
        if (order == 0) {
          el_prime=true;
          l_row.m_dtctel_null=1;       // no delta-time for source event
          noelectricalpr++;
        }
        if (tmult > 9) mult=0;   // if too many member is group; CTMULT cannot be set correctly
        el_intime=true;          // flag as electrical cross-talk
      }

      // Construct status.
      for (int is=0; is < ahsxs::LEN_SXSSTATUS; is++) status[is]=q_row[ip].m_status[is];
      if (calcprox) ahsxs::set_status_prox(prox_intime,status);
      if (calcctrec) ahsxs::set_status_recoil(rec_is12,rec_eOK,status);
      if (calcctel) {
        if (ctelidx == 1)
          ahsxs::set_status_electrical(el_intime,el_prime,status);
        else
          ahsxs::set_status_electrical2(el_intime,el_prime,status);
      }

      // Write row.
      for (int is=0; is < ahsxs::LEN_SXSSTATUS; is++) l_row.m_status[is]=status[is];
      l_row.m_mult=mult;
      ahfits::gotoRow(fp,q_row[ip].m_row);
      ahfits::writeRow(fp);

    }   // end for loop over queue

    // Initialize next queue.
    q_row.clear();
    q_row.push_back(b_row);

  } // end while loop

  // Output statistics.
  if (ctelidx == 1) {
    AH_INFO(ahlog::HIGH) << std::endl;
    AH_INFO(ahlog::HIGH) << "Flag cross-talk statistics" << std::endl;
    AH_INFO(ahlog::HIGH) << "Number of events checked:     " << noevent << std::endl;
    if (calcctrec) AH_INFO(ahlog::HIGH) << "Number of recoil flagged:     " << norecoil << std::endl;
    if (calcctel) {
      AH_INFO(ahlog::HIGH) << "Number of electrical flagged: " << noelectrical << std::endl;
      AH_INFO(ahlog::HIGH) << "Number of electrical prime:   " << noelectricalpr << std::endl;
    }
    if (calcprox) AH_INFO(ahlog::HIGH) << "Number of proximate events:   " << noprox << std::endl;
    AH_INFO(ahlog::HIGH) << std::endl;
  } else {
    AH_INFO(ahlog::HIGH) << "Number of events checked:      " << noevent << std::endl;
    AH_INFO(ahlog::HIGH) << "Number of electrical2 flagged: " << noelectrical << std::endl;
    AH_INFO(ahlog::HIGH) << "Number of electrical2 prime:   " << noelectricalpr << std::endl;
  }

}

// ---------------------------------------------------------------------------

void process_antico(ahfits::FilePtr fp, ahfits::FilePtr fpa, bool ckrisetime,
                    const std::string& antpsp, double antshift, double antdtpre,
                    double antdtfol, int antswitch, int antphathr, int antdurthr,
                    bool dtflag, bool& clean) {

  AH_INFO(ahlog::HIGH) << "Processing antico" << std::endl;

  // Store column values for a single event in the EVENT file.
  type_time l_time=0.;
  char l_time_null=0;
  type_pha l_pha=0;
  char l_pha_null=0;
  type_itype l_itype=0;
  int l_pixel=0;
  char l_pixel_null=0;
  int l_rise_time=0;
  ahfits::IndexType num_status=ahsxs::LEN_SXSSTATUS;
  type_status l_status[ahsxs::LEN_SXSSTATUS];
  for (int istat=0; istat < ahsxs::LEN_SXSSTATUS; istat++) {
    l_status[istat]=0;
  }
  type_proc_status l_proc_status[LENPROC_STATUS];
  for (int procstat=0; procstat < LENPROC_STATUS; procstat++) {
    l_proc_status[procstat]=0;
  }
  ahfits::IndexType l_num_proc_status=LENPROC_STATUS;
  type_time l_dtant=0.;
  char l_dtant_null=0;

  // Store column values for single row in antico file.
  type_time la_time=0.;
  char la_time_null=0;
  type_pha la_pha=0;
  char la_pha_null=0;
  type_flag la_flgbase[1]={0};
  ahfits::IndexType num_flgbase=1;
  type_flag la_flglost[1]={0};
  ahfits::IndexType num_flglost=1;
  type_duration la_duration=0.;
  type_pspid la_pspid=0;
  type_proc_status la_proc_status[LENPROC_STATUS];
  for (int procstat=0; procstat < LENPROC_STATUS; procstat++) {
    la_proc_status[procstat]=0;
  }
  ahfits::IndexType la_num_proc_status=LENPROC_STATUS;

  // Track flagging statistics.
  long noeventflag=0;         // number of events from fp flagged as antico
  long noevent=0;             // number of events from fp
  long noantico=0;            // number of antico events from fpa considered (+baseline) (some antico events are skipped)
  long noanticobaseline=0;    // if skipping baseline, number of antico events considered
  long noanticopha=0;         // number of antico events with PHA above threshold
  long noanticoskip=0;        // if skipping baseline, number of baseline antico events skipped
  long noanticophaskip=0;     // number of antico events with PHA below threshold
  long noanticodurskip=0;     // number of antico events with DURATION below threshold

  bool done=false;            // flag for ending while-loop over antico rows
  double timeprevevt=0.;      // time of previous event row
  double timeprevant=0.;      // time of previous antico row

  // Make connections to event file columns.
  ahfits::Router router(fp);
  router.connectScalar(ahfits::e_READONLY,"TIME",l_time, &l_time_null);
  router.connectScalar(ahfits::e_READONLY,"PHA",l_pha,&l_pha_null);
  router.connectScalar(ahfits::e_READONLY,"ITYPE",l_itype);
  router.connectScalar(ahfits::e_READONLY,"PIXEL",l_pixel,&l_pixel_null);
  router.connectScalar(ahfits::e_READONLY,"RISE_TIME",l_rise_time);
  router.connectBit(ahfits::e_READWRITE,"STATUS",l_status,num_status);
  router.connectBit(ahfits::e_READWRITE,"PROC_STATUS",l_proc_status, l_num_proc_status);
  if (dtflag) router.connectScalar(ahfits::e_WRITEONLY,"DTANT",l_dtant, &l_dtant_null);

  // Make connections to antico file columns.
  ahfits::Router routera(fpa);
  routera.connectScalar(ahfits::e_READONLY,"TIME",la_time, &la_time_null);
  routera.connectScalar(ahfits::e_READONLY,"PHA",la_pha,&la_pha_null);
  routera.connectBit(ahfits::e_READONLY,"FLG_BASELINE",la_flgbase, num_flgbase);
  routera.connectBit(ahfits::e_READONLY,"FLG_EVENT_LOST",la_flglost, num_flglost);
  routera.connectScalar(ahfits::e_READONLY,"DURATION",la_duration);
  routera.connectScalar(ahfits::e_READONLY,"PSP_ID",la_pspid);
  routera.connectBit(ahfits::e_READWRITE,"PROC_STATUS",la_proc_status,la_num_proc_status);

  // There are two allowed event PSP_ID values based on the pspid parameter.
  int ant_pspid1=0;
  int ant_pspid2=1;
  if (antpsp == "B") {
    ant_pspid1=2;
    ant_pspid2=3;
  }

  // Read first row from event file.
  clean=false;         // false => do not delete output file upon error
  ahfits::firstRow(fp);
  ahfits::readRow(fp);
  while (l_itype >= 5 || !(procstatus::processRow(l_proc_status)) || 
         l_time_null == 1 || (ckrisetime && (l_rise_time > 127)) ||
         l_pixel_null == 1 || l_pha_null == 1) {

    // row is skipped, so write NULL into DTANT
    l_dtant_null=1;

    // if PIXEL=NULL, also set the GSFC bit of PROC_STATUS to bad
    if (l_pixel_null == 1) {
      l_proc_status[16]=1;
    }

    ahfits::writeRow(fp);

    // read next row
    ahfits::nextRow(fp);
    if (!ahfits::readOK(fp)) break;
    ahfits::readRow(fp);
  }
  if (!ahfits::readOK(fp)) {    // nothing to do if no valid events found
    return;
  }
  timeprevevt=l_time;
  noevent++;

  // Loop over antico rows.
  timeprevant=-1.;
  for (ahfits::firstRow(fpa); ahfits::readOK(fpa); ahfits::nextRow(fpa)) {

    // Time interval for checking antico coincidence: [ t1 : t2 ].
    double t1=0.;
    double t2=0.;

    ahfits::readRow(fpa);

    if (la_pspid != ant_pspid1 && la_pspid != ant_pspid2) {   // antico from wrong PSP; ignore
      continue; 
    }
    if (!procstatus::processRow(la_proc_status)) {            // skip antico with bad proc_status
      continue;
    }
    if (la_time_null == 1) {                                  // skip antico with TIME unassigned
      continue;
    }
    if (la_pha_null == 1) {                                   // skip antico with PHA unassigned
      continue;
    }
    noantico++;

    // Check that TIMEs are sorted
    if (la_time < timeprevant) AH_THROW_RUNTIME("Antico file is not sorted by TIME");
    timeprevant=la_time;

    // Skip if FLG_BASELINE, FLG_LOST_EVENT, or FLG_PARITY_ERR are set.
    if (la_flgbase[0] == 1 || la_flglost[0] == 1) {
      noanticoskip++;
      continue;
    } else {
      noanticobaseline++;
    }

    // If PHA too small, skip.
    if (la_pha < antphathr) {
      noanticophaskip++;
      continue;
    }

    // If antico DURATION too small, skip.
    if (la_duration < antdurthr) {
      noanticodurskip++;
      continue;
    }

    // At this point, the antico event is being used
    noanticopha++;

    // Calculate time interval around antico event.
    t1=la_time-antdtpre + antshift;
    t2=la_time+antdtfol + antshift;
    if (!antswitch) t2=la_time+la_duration*ANTDURUNITS;   // converting DURATION to seconds

    // Check if antico outside range of event data.
    if (l_time > t2) {
      continue;
    }

    // Read rows in event file until one is inside antico interval.
    while (l_time < t1) {
      ahsxs::set_status_antico(false,l_status);
      l_dtant_null=1;         // no antico coincidence, set DTANT=NULL
      ahfits::writeRow(fp);
      ahfits::nextRow(fp);
      if (ahfits::readOK(fp)) {
        ahfits::readRow(fp);
        while (l_itype >= 5 || !(procstatus::processRow(l_proc_status)) || 
               l_time_null == 1 || (ckrisetime && (l_rise_time > 127)) ||
               l_pixel_null == 1 || l_pha_null == 1) {
          // event row is skipped, write DTANT=NULL
          l_dtant_null=1;

          // if PIXEL=NULL, also set the GSFC bit of PROC_STATUS to bad
          if (l_pixel_null == 1) {
            l_proc_status[16]=1;
          }
          ahfits::writeRow(fp);

          // read next row
          ahfits::nextRow(fp);
          if (!ahfits::readOK(fp)) break;
          ahfits::readRow(fp);
        }
      }
      if (!ahfits::readOK(fp)) {    // done if reached end of file
        done=true;
        break;
      } else {
        if (l_time < timeprevevt) AH_THROW_RUNTIME("Event file is not sorted by TIME");
        timeprevevt=l_time;
      }
      noevent++;
    }
    if (done) break;

    // Set flags for events in antico interval.
    while (l_time < t2) {
      ahsxs::set_status_antico(true,l_status);
      l_dtant_null=0;
      l_dtant=l_time-la_time;          // DTANT = TIME(event) - TIME(antico)
      ahfits::writeRow(fp);
      noeventflag++;
      ahfits::nextRow(fp);
      if (ahfits::readOK(fp)) {        // get next valid event if not at end of file
        ahfits::readRow(fp);
        while (l_itype >= 5 || !(procstatus::processRow(l_proc_status)) || 
               l_time_null == 1 || (ckrisetime && (l_rise_time > 127)) ||
               l_pixel_null == 1 || l_pha_null == 1) {
          // event row is skipped, write DTANT=NULL
          l_dtant_null=1;

          // if PIXEL=NULL, also set the GSFC bit of PROC_STATUS to bad
          if (l_pixel_null == 1) {
            l_proc_status[16]=1;
          }

          ahfits::writeRow(fp);

          // read next row
          ahfits::nextRow(fp);
          if (!ahfits::readOK(fp)) break;
          ahfits::readRow(fp);
        }
        if (!ahfits::readOK(fp)) {     // check if at end of file
          done=true;
          break;                       // exit while(l_time < t2) loop
        } else {
          if (l_time < timeprevevt) AH_THROW_RUNTIME("Event file is not sorted by TIME");
          timeprevevt=l_time;
        }
      }
      if (!ahfits::readOK(fp)) {    // done if reached end of file
        done=true;
        break;
      } else {
        if (l_time < timeprevevt) AH_THROW_RUNTIME("Event file is not sorted by TIME");
        timeprevevt=l_time;
      }
      noevent++;
    }
    if (done) break;
  }

  // Antico file is done, for rest of events, set status to zero.
  while (ahfits::readOK(fp)) {
    ahfits::readRow(fp);
    while (l_itype >= 5 || !(procstatus::processRow(l_proc_status)) || 
           l_time_null == 1 || (ckrisetime && (l_rise_time > 127)) ||
           l_pixel_null == 1 || l_pha_null == 1) {
      // event row is skipped, write DTANT=NULL
      l_dtant_null=1;

      // if PIXEL=NULL, also set the GSFC bit of PROC_STATUS to bad
      if (l_pixel_null == 1) {
        l_proc_status[16]=1;
      }

      ahfits::writeRow(fp);

      // read next row
      ahfits::nextRow(fp);
      if (!ahfits::readOK(fp)) break;
      ahfits::readRow(fp);
    }
    if (!ahfits::readOK(fp)) break;
    if (l_time < timeprevevt) AH_THROW_RUNTIME("Event file is not sorted by TIME");
    timeprevevt=l_time;
    noevent++;
    ahsxs::set_status_antico(false,l_status);
    l_dtant_null=1;                   // no antico coincidence, set DTANT=NULL
    ahfits::writeRow(fp);
    ahfits::nextRow(fp);
  }

  // Output statistics.
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "Flag Antico statistics" << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of events checked:                       " << noevent << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of events flagged:                       " << noeventflag << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of antico checked:                       " << noantico << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of antico w/o baseline:                  " << noanticoskip <<std::endl;
  AH_INFO(ahlog::HIGH) << "Number of antico w/ baseline:                   " << noanticobaseline << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of antico w/ PHA below antphathr:        " << noanticophaskip << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of antico w/ DURATION below antdurthr:   " << noanticodurskip << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of antico used:                          " << noanticopha << std::endl;
  AH_INFO(ahlog::HIGH) << std::endl;

}

// ---------------------------------------------------------------------------

void process_mxs(ahfits::FilePtr fp, ahfits::FilePtr gp, bool ckrisetime,
                 double mxsdt, long* extnumarray, bool& clean) {

  AH_INFO(ahlog::HIGH) << "Processing MXS" << std::endl;

  // keep track of LEDs processing; skip any duplicates
  bool haveled1=false;
  bool haveled2=false;
  bool haveled3=false;
  bool haveled4=false;

  // Store column values for a single for in the EVENT file.
  type_time l_time=0.;
  char l_time_null=0;
  int l_pixel=0;
  char l_pixel_null=0;
  int l_rise_time=0;
  ahfits::IndexType num_proc_status=LENPROC_STATUS;
  type_proc_status l_proc_status[LENPROC_STATUS];
  for (int procstat=0; procstat < LENPROC_STATUS; procstat++) {
    l_proc_status[procstat]=0;
  }
  
  ahfits::IndexType num_status=ahsxs::LEN_SXSSTATUS;
  type_status l_status[ahsxs::LEN_SXSSTATUS];
  for (int istat=0; istat < ahsxs::LEN_SXSSTATUS; istat++) {
    l_status[istat]=0;
  }

  // Store column values for a single for in the MXS file.
  type_time g_start=0.;
  type_time g_stop=0.;

  // Set up ahfits connections for event file.
  ahfits::Router rout_in(fp);
  rout_in.connectScalar(ahfits::e_READONLY,"TIME",l_time, &l_time_null);
  rout_in.connectScalar(ahfits::e_READONLY,"PIXEL",l_pixel,&l_pixel_null);
  rout_in.connectBit(ahfits::e_READWRITE,"STATUS",l_status,num_status);
  rout_in.connectScalar(ahfits::e_READWRITE,"RISE_TIME",l_rise_time);
  rout_in.connectBit(ahfits::e_READWRITE,"PROC_STATUS",l_proc_status, num_proc_status);
 

  // Set up ahfits connections MXS file.
  ahfits::Router rout_gti(gp);
  rout_gti.connectScalar(ahfits::e_READONLY,"START",g_start);
  rout_gti.connectScalar(ahfits::e_READONLY,"STOP",g_stop);

  // Print statistics header.
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "Flag MXS statistics" << std::endl;

  // Loop over MXS GTI extensions (1 HDU per LED).
  clean=false;        // false => do not delete output file upon error
  ahfits::firstHDU(gp,ahfits::e_BINARY_TBL);
  do {
    bool done=false;      // true if done with current GTI
    std::string extname;  // extension name of MXS file
    int n=0;              // extension number based on extname
    int offset=0;         // LED index determines which STATUS bits to modify;
                          // offset gives this location in STATUS

    // Gather flagging statistics for each GTI.
    long mxs_flag=0;
    long mxs_flagaf=0;
    long noevent=0;

    // Get LED index from extension name.
    extname=ahfits::getKeyValStr(gp,"EXTNAME");
    if ("GTIMXSFNON1" == extname) {
      if (haveled1) continue;     // log message about duplicate written in initialize()
      haveled1=true;
      n=0;
    } else if ("GTIMXSFNON2" == extname) {
      if (haveled2) continue;
      haveled2=true;
      n=1;
    } else if ("GTIMXSFNON3" == extname) {
      if (haveled3) continue;
      haveled3=true;
      n=2;
    } else if ("GTIMXSFNON4" == extname) {
      if (haveled4) continue;
      haveled4=true;
      n=3;
    } else {
      continue;
    }

    // Skip empty GTI extensions.
    if (extnumarray[n] == 0) continue;

    // STATUS bits to modify depend on LED index.
    if (n == 2 || n == 4) offset=2;

    // Read first GTI.
    ahfits::readRow(gp);

    // Loop over EVENTs in input file (fp).
    for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp)) {

      ahfits::readRow(fp);
      if (!procstatus::processRow(l_proc_status)) continue;
      if (l_time_null == 1) continue;
      if (ckrisetime && (l_rise_time > 127)) continue;

      // if PIXEL is NULL; set GSFC bit of PROC_STATUS to bad and go to next row
      if (l_pixel_null == 1) {
        l_proc_status[16]=1;
        ahfits::writeRow(fp);
        continue;
      }

      noevent++;

      // Read GTI rows until current EVENT time is greater than GTI STOP (+afterglow).
      while (l_time > g_stop+mxsdt) {
        ahfits::nextRow(gp);
        if (done || !ahfits::readOK(gp)) {
          done=true;
          break;
        } else {
          ahfits::readRow(gp);
        }
      }

      if (done) {                               // finished with GTI extension
        ahsxs::set_status_mxs(false,false,offset,l_status);
      } else if (l_time < g_start) {            // EVENT outside GTI intervals
        ahsxs::set_status_mxs(false,false,offset,l_status);
      } else if (l_time > g_stop && l_time < g_stop+mxsdt) {      // afterglow
        mxs_flag++;
        mxs_flagaf++;
        ahsxs::set_status_mxs(false,true,offset,l_status);
      } else {                                 // inside GTI, but no afterglow
        mxs_flag++;
        ahsxs::set_status_mxs(true,false,offset,l_status);
      }

      // Write STATUS.
      ahfits::writeRow(fp);

    }  // end loop over EVENT rows

    // Write statistics.
    AH_INFO(ahlog::HIGH) << "=== LED#" << (n+1) << " ===" << std::endl;
    AH_INFO(ahlog::HIGH) << "Number of events:       " << noevent << std::endl;
    AH_INFO(ahlog::HIGH) << "Total number flagged:   " << mxs_flag << std::endl;
    AH_INFO(ahlog::HIGH) << " in GTI:                " << (mxs_flag-mxs_flagaf) << std::endl;
    AH_INFO(ahlog::HIGH) << " in afterglow:          " << mxs_flagaf << std::endl;

  } while (ahfits::nextHDU(gp,ahfits::e_BINARY_TBL));

  // Statistics footer.
  AH_INFO(ahlog::HIGH) << std::endl;

}

// ---------------------------------------------------------------------------

void flag_gti(ahfits::FilePtr fp, const std::string & gtifile, bool ckrisetime,
              bool& clean) {

  AH_INFO(ahlog::HIGH) << "Processing GTI" << std::endl;

  // Local varible declarations.
  bool done = true;                                //Is true when all valid HDU's are opened
  int noevent=0;                                   //count number of events
  int nogengti=0;                                  //count number of general GTI bits set
  int nopixgti=0;                                  //count number of pixel GTI bits set
  type_proc_status e_proc_status[LENPROC_STATUS];  //local variable to store proc_status column
  for (int procstat=0; procstat < LENPROC_STATUS; procstat++) {
    e_proc_status[procstat]=0;
  }
  ahfits::IndexType num_proc_status=LENPROC_STATUS;//size of proc_status bit
  type_time e_time=0.;                             //local variable to store time column
  int e_rise_time=0;                               //local variable to store RISE_TIME column
  char e_time_null=0;                              //null flag for time column
  type_status e_status[ahsxs::LEN_SXSSTATUS];      //local variable to store status column
  for (int istat=0; istat < ahsxs::LEN_SXSSTATUS; istat++) {
    e_status[istat]=0;
  }
  ahfits::IndexType num_status=ahsxs::LEN_SXSSTATUS;          //size of status column
  type_pixel e_pixel=0;                                       //local variable to store pixel column
  char e_pixel_null=0;
  double timeprev=0.;                                         // TIME of previous event

  // Open GTI file and prepare for reading data from all relevant extensions.
  // The loadGTIFile function will open all extensions and read the first GTI
  // from each.
  ahsxs::GTIFile gti;
  ahsxs::loadGTIFile(gtifile,gti);
  AH_INFO(ahlog::LOW) << "Loaded GTI file: " << gtifile << std::endl;
  if (gti.m_havegen) AH_INFO(ahlog::LOW) << "  found general GTI extension" << std::endl;
  if (gti.m_haveanypix) AH_INFO(ahlog::LOW) << "  found pixel-dependent GTI extension(s)" << std::endl;

  // Setup router and make connections to event file columns.
  ahfits::Router router(fp);
  router.connectScalar(ahfits::e_READONLY,"TIME",e_time,&e_time_null);
  router.connectScalar(ahfits::e_READONLY,"PIXEL",e_pixel,&e_pixel_null);
  router.connectScalar(ahfits::e_READONLY,"RISE_TIME",e_rise_time);
  router.connectBit(ahfits::e_READWRITE,"STATUS",e_status,num_status);
  router.connectBit(ahfits::e_READWRITE,"PROC_STATUS",e_proc_status,num_proc_status);

  clean=false;   // false => do not delete output file upon error
  timeprev=-1.;
  for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp)) {

    ahfits::readRow(fp);
    if (!(procstatus::processRow(e_proc_status))) continue; 
    if (e_time_null == 1) continue;
    if (ckrisetime && (e_rise_time > 127)) continue;

    // if PIXEL is NULL; set GSFC bit of PROC_STATUS to bad and go to next row
    if (e_pixel_null == 1) {
      e_proc_status[16]=1;
      ahfits::writeRow(fp);
      continue;
    }

    noevent++;

    // Check if TIMEs are sorted
    if (e_time < timeprev) AH_THROW_RUNTIME("Event TIMEs are not sorted.");

    // Check if in GTI for current pixel.
    if (gti.m_havepix[e_pixel]) {
      done=false;

      while (e_time > gti.m_stoppix[e_pixel]) {
        done=!ahsxs::readNextPixelGTI(gti,e_pixel);
        if (done) break;
      }
  
      if (!done && e_time >= gti.m_startpix[e_pixel]) {  // in GTI
        ahsxs::set_status_pixel_gti(true, e_status);
      } else {                                           // not in GTI
        ahsxs::set_status_pixel_gti(false, e_status);  
        nopixgti++;
      }  
    } else if (gti.m_haveanypix) {
      // if there are pixel GTI, but none for the given pixel, then
      // the event should be flagged as not-in-GTI
      ahsxs::set_status_pixel_gti(false, e_status);  
      nopixgti++;
    }

    // Check if in general GTI.
    if (gti.m_havegen) {
      done = false; 

      while (e_time > gti.m_stopgen) {
        done=!ahsxs::readNextGeneralGTI(gti);
        if (done) break;
      }
  
      if (!done && e_time >= gti.m_startgen) {          // in GTI
        ahsxs::set_status_general_gti(true, e_status);
      } else {                                          // not in GTI
        ahsxs::set_status_general_gti(false, e_status);  
        nogengti++;
      }
    }

    // Write event STATUS.
    ahfits::writeRow(fp);

    timeprev=e_time;
  } //  end loop over rows in event file

  // Output statistics.
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "Flag gti statistics" << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of events:               " << noevent << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of events outside general good time intervals:       " << nogengti << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of event outside pixel good time intervals:          " << nopixgti << std::endl;
  AH_INFO(ahlog::HIGH) << std::endl; 

}

/* Revision Log
 $Log: sxsflagpixlib.cxx,v $
 Revision 1.29  2016/08/23 15:56:35  mwitthoe
 sxsflagpix: fix bug where proximity and electrical cross-talk checks were skipped when the EPI column is NULL

 Revision 1.28  2016/08/22 23:20:35  mwitthoe
 sxsflagpix: fix bug where negative values for antshift were not permitted when entered manually (CALDB was OK); now antshift can be positive or negative

 Revision 1.27  2016/08/16 18:12:20  mwitthoe
 sxsflagpix: remove constraint on group size when flagging cross-talk; previously the tool threw an error if there were more than 1000 in a group, now there can be any number

 Revision 1.26  2016/08/11 20:07:01  mwitthoe
 sxsflagpix: 1) switch meaning on 0/1 for GTI STATUS bits - 0 means invalid event or not in GTI, 1 means valid event in GTI; 2) allow negative pxpithr values

 Revision 1.25  2016/08/10 22:14:21  mwitthoe
 sxsflagpix: if given pixel-dependent GTI, but there are no GTI for a given pixel, then flag those events as not-in-GTI

 Revision 1.24  2016/08/10 17:54:56  mwitthoe
 sxsflagpix: 1) speed-up GTI flagging routine; 2) fix bug where not all STATUS bits were reset according to the resetflags parameter

 Revision 1.23  2016/08/10 16:53:11  mwitthoe
 sxsflagpix: add support for pixel-dependent GTI files in two formats: 1) one extension per pixel, or 2) one extension with a PIXEL column

 Revision 1.22  2016/07/25 18:46:47  mwitthoe
 sxsflagpix: 1) skip events with PHA/PI/EPI=NULL; 2) do not include pixel 12 events in proximity check; 3) add parameters pxpithr/usepxpithr for setting a PI threshold for including events in the proximity and cross-talk checks; 4) change meaning of resetflags parameter to be a list of flagging types to reset the STATUS bits for

 Revision 1.21  2016/06/24 19:34:09  mwitthoe
 sxsflagpix: fix bug - when last row in the input event file was coincident with antico the algorithm entered into an infinite loop

 Revision 1.20  2016/06/22 16:36:40  mwitthoe
 sxsflagpix: bug-fix - go to first row in table before starting antico flagging

 Revision 1.19  2016/06/03 15:31:14  mwitthoe
 sxsflagpix: correct log counter for number of antico events used

 Revision 1.18  2016/05/24 19:42:13  mwitthoe
 sxsflagpix: remove debug statement

 Revision 1.17  2016/05/24 19:40:12  mwitthoe
 sxsflagpix: add new parameter, resetflags, to set all STATUS bits to zero before flagging new bits

 Revision 1.16  2016/04/07 21:05:29  mwitthoe
 sxsflagpix: change INFO statements to DEBUG inside row loops

 Revision 1.15  2016/03/07 18:11:43  mwitthoe
 sxsflagpix: tool now checks for PIXEL==NULL and, if so, will set the GSFC bit of PROC_STATUS to BAD and go on to the next row

 Revision 1.14  2015/12/08 15:52:27  mwitthoe
 sxsflagpix: convert antico DURATION value to seconds when constructing the antico search interval

 Revision 1.13  2015/12/03 19:36:14  mwitthoe
 sxsflagpix: perform a 2nd electrical cross-talk check using a different delta-time

 Revision 1.12  2015/11/16 17:57:57  mwitthoe
 sxsflagpix: revert to using PHA column instead of EPI for determining order of PHA magnitude in a collection of associated electrical cross-talk events

 Revision 1.11  2015/11/13 19:23:53  mwitthoe
 sxsflagpix: fix bug in GTI flagging routine where multiple GTI extensions were not be read properly

 Revision 1.10  2015/10/13 20:48:45  mwitthoe
 sxsflagpix: update names of MXS extensions (they were recently changed in mxstime)

 Revision 1.9  2015/10/07 16:53:21  mwitthoe
 sxsflagpix: add ckrisetime parameter to skip events with RISE_TIME>127

 Revision 1.8  2015/09/25 20:02:10  mwitthoe
 sxsflagpix: add parameter, antdurthr, which is the DURATION threshold for accepting antico events; if an antico event has a DURATION less than antdurthr, then it is ignored

 Revision 1.7  2015/09/23 14:22:15  mwitthoe
 sxsflagpix: delete output file if there is an error before processing

 Revision 1.6  2015/09/23 14:11:25  mwitthoe
 sxsflagpix: 1) add dtflag parameter which switches on new delta-time output columns for cross-talk and antico (diagnostics); 2) throw error if discovered that TIME column is not sorted; 3) perform recoil cross-talk energy check with EPI instead of PHA; 4) fix parameter description for calcprox; 5) fix afterglow counter for MXS flagging and clarify log messages

 Revision 1.5  2015/09/10 21:59:44  mwitthoe
 sxsflagpix bug-fixes: 1) was writing to input GTI file instead of output file when flagging MXS; 2) log statements in MXS routine were being written for each event row instead of once after the event loop; 3) made the GTI search more efficient; 4) event loop was not ending correctly when the event time was larger than the last MXS GTI

 Revision 1.4  2015/07/30 16:07:14  mwitthoe
 sxsflagpix: clean up tool; see issues 532, 534, and 543

 Revision 1.3  2014/01/22 20:40:21  mwitthoe
 sxsflagpix: update accordng to code review; issue 331

 Revision 1.2  2014/01/15 22:26:33  peachey
 Add Peer Review comments regarding variable declarations,

 Revision 1.1  2013/11/12 17:45:03  mwitthoe
 sxsflagpix: moved getPar_warn() functions to ahapp; renamed sxspixel map to sxsflagpixlib; worked on standards compliance for source (mainly moving variable declarations to top of scope)



 BELOW IS THE REVISION LOG FOR sxspixelmap.cxx BEFORE RENAMING TO sxsflagpixlib.cxx

 Revision 1.3  2013/10/07 21:20:11  mwitthoe
 sxsflagpix: switch over to new ahfits connection functions (issue 270)

 Revision 1.2  2013/04/16 17:19:26  mwitthoe
 sxsflagpix: change source to match latest TRF (major change to cross-talk algorithm, add MXS algorithm); modify form ahcaldb library, sxspixelmap, to follow new standards

 Revision 1.1  2013/01/02 19:09:52  mwitthoe
 add new ahcaldb library for the SXS pixel map


*/
