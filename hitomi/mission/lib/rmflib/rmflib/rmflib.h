/// \file rmflib.cxx
/// \brief Shared RMF functions for astroh tools
/// \author Andy Sargent
/// \date $Date: 2016/01/12 01:30:01 $
 
/// \addtogroup mod_rmflib
/// \section rmflib_rmflib RMF constants and functions - rmflib
///

#ifndef RMFLIB_RMFLIB_H
#define RMFLIB_RMFLIB_H

#include "ahgen/ahversion.h"
#include "ahfits/ahfits.h"
AHVERSION(RMFLIB_RMFLIB,"$Id: rmflib.h,v 1.18 2016/01/12 01:30:01 rshill Exp $")

#include "rmf.h"
#include "ahfits/ahfits.h"

// Global constants
const int MAX_NGRP = 40;    /// < Maximum number of components for each row of 
                            ///   output response matrix. Must be greater than
                            ///   the number of escape peaks + 1 


/// \brief sxi- and sxs-specific rmf functions
/// \ingroup mod_rmflib
namespace rmflib {

/// \brief Keywords of MATRIX extension
struct RMFMatrixKeywords {
  
  RMFMatrixKeywords() : m_telescop(""), m_instrume(""), m_detnam(""),
    m_filter(""), m_chantype(""), 
    m_hduclass(""), m_hduclas1(""), m_hduclas2(""),
    m_hduclas3(""), m_hduvers(""), m_hduvers1(""),  m_hduvers2(""),
    m_rmfversn(""), m_lo_thresh(0.0), m_ccls0001(""), m_ccnm0001(""),
    m_cdtp0001(""), m_cvsd0001(""), m_cvst0001(""), m_cdes0001(""),
    m_detchans(0), m_numgrp(0), m_numelt(0) {};

  std::string m_telescop;
  std::string m_instrume;
  std::string m_detnam;
  std::string m_filter;
  std::string m_chantype;
  std::string m_hduclass;
  std::string m_hduclas1;
  std::string m_hduclas2;
  std::string m_hduclas3;
  std::string m_hduvers;
  std::string m_hduvers1;
  std::string m_hduvers2;
  std::string m_rmfversn;
  double m_lo_thresh;
  std::string m_ccls0001;
  std::string m_ccnm0001;
  std::string m_cdtp0001;
  std::string m_cvsd0001;
  std::string m_cvst0001;
  std::string m_cdes0001;
  long m_detchans;
  long m_numgrp;
  long m_numelt;
};

/// \brief Keywords of EBOUNDS extension
struct RMFEboundsKeywords {
  
  RMFEboundsKeywords() : m_telescop(""), m_instrume(""), m_filter(""),
    m_chantype(""), m_detchans(0), m_hduclass(""), m_hduclas1(""), m_hduclas2(""),
    m_hduvers(""), m_hduvers1(""),  m_hduvers2(""),
    m_rmfversn(""), m_ccls0001(""), m_ccnm0001(""),
    m_cdtp0001(""), m_cvsd0001(""), m_cvst0001(""), m_cdes0001("") {};

  std::string m_telescop;
  std::string m_instrume;
  std::string m_filter;
  std::string m_chantype;
  long m_detchans;
  std::string m_hduclass;
  std::string m_hduclas1;
  std::string m_hduclas2;
  std::string m_hduvers;
  std::string m_hduvers1;
  std::string m_hduvers2;
  std::string m_rmfversn;
  std::string m_ccls0001;
  std::string m_ccnm0001;
  std::string m_cdtp0001;
  std::string m_cvsd0001;
  std::string m_cvst0001;
  std::string m_cdes0001;
};


/// \brief This structure contains the rmf information for a single input energy
///(ie fits row}
///
///
///  Parameters:
///  m_energ_lo     0...nmat-1 input energy bin lower edge  
///  m_energ_hi     0...nmat-1 input energy bin high edge
///  m_ngrp         number of groups for single input energy (i.e. row)
///  m_n_matrix     number elements for a single input energy (i.e. row)
///  m_f_chan       first channel within each group for a single input 
///                 energy (i.e. row)
///  m_n_chan       number of channels within each group for a single input 
///                 energy (i.e. row)
///  m_matrix       response matrix elements for a single input 
///                 energy (i.e. row)
///  m_router       ahfits router used to connect local variables to columns
struct RMFMatrixRowVars {
  // constructor
  RMFMatrixRowVars():  m_energ_lo(0.), m_energ_hi(0.), m_ngrp(0), 
    m_nmatrix(0), m_f_chan(0), m_n_chan(0), m_matrix(0), m_router(0)  {}

  
  double m_energ_lo;
  double m_energ_hi;
 
  // define rmf groups
  ahfits::IndexType m_ngrp;
  ahfits::IndexType m_nmatrix;
  int * m_f_chan;  
  int * m_n_chan;
  double * m_matrix;  

  // ahfits router, connects the variables to fits file
  ahfits::Router * m_router;
  
};

/// \brief Struct contains information for the entire rmf file
///        
///  
///  Parameters:
///  ahffp            ahfits file pointer to output rmf file
///  m_nmat           Number energy bins 
///  m_ematstart      (0...nchan-1) input energy bin lower edge – in keV 
///  m_ematstop       (0...nchan-1) input energy bin lower edge – in keV 
///  m_ematcen        (0...nchan-1) input energy bin center – in keV  
///  m_nchan          number of output energy channels 
///  m_echanstart     (0...nchan-1) output energy channel lower edge – in keV
///  m_echanstop      (0...nchan-1) output energy channel upper edge – in keV
///  m_echancen       (0...nchan-1) output energy channel center in – in keV
///  m_numgrp         total number of groups in output rmf
///  m_numelt         total number of elements in output rmf
///  m_telescop       telescop keyword in output rmf
///  m_instrume       instrume keyword in output rmf
///  m_lo_thresh      lo_thresh keyword in output rmf
///  m_chantype       chantype keyword in output rmf
///  m_matrixrow      struct containing data for a single 
///                   input energy (i.e. row)
///  m_date           date in YYYY-MM-DD formate
///  m_time           time in hh:mm:ss
///  m_description    description CDES0001 keyword
struct RMFData {
  // constructor
RMFData(): m_ahffp(0), m_write(false), m_nmat(0), m_ematstart(0), m_ematstop(0),
           m_ematcen(0), m_echanstart(0), m_echanstop(0), m_echancen(0), 
           m_numgrp(0), m_numelt(0), m_lo_thresh(0.), m_tlmin_channel(0), 
           m_tlmax_channel(0), m_tlmin_fchan(0), m_tlmax_fchan(0),
           m_kevunits(false) {}  

  // ahfits fileptr to object
  ahfits::FilePtr m_ahffp;

  // flag whether open for writing
  bool m_write;
  
  // input energy bins
  long m_nmat;
  double * m_ematstart;
  double * m_ematstop;
  double * m_ematcen;

  // channel energy bins
  long m_nchan;
  double * m_echanstart;
  double * m_echanstop;
  double * m_echancen;

  // total number of groups and elements in RMF
  long m_numgrp;
  long m_numelt;

  // keyword values read on input
  RMFMatrixKeywords  m_matkey;
  RMFEboundsKeywords m_ebkey;

  // keyword values needed for output
  std::string m_telescop;
  std::string m_instrume;
  std::string m_filter;
  std::string m_detnam;
  double m_lo_thresh;
  std::string m_chantype;
  std::string m_date;
  std::string m_time;
  std::string m_description_matrix;
  std::string m_description_ebounds;
 
  // TLMIN and TLMAX for CHANNEL (ebounds ext.) and FCHAN (matrix ext.) columns
  long long m_tlmin_channel;
  long long m_tlmax_channel;
  long long m_tlmin_fchan;
  long long m_tlmax_fchan;

  // Units information
  std::string m_tunit_emin;
  std::string m_tunit_energlo;
  bool m_kevunits;

  // data for a single input energy (i.e. row)
  RMFMatrixRowVars m_matrixrow;
};

/// \brief Copy one RMFData structure to another.
/// \param[out] rmfdatout   output structure
/// \param[in]  rmfdatin    input structure
void copyRMFData(RMFData * rmfdatout, RMFData * rmfdatin);

/// \brief Copy one RMFMatrixRows structure to another.
/// \param[out] matrixrowout   output structure
/// \param[in]  matrixrowin    input structure
void copyRMFMatrixRowVars(RMFMatrixRowVars * matrixrowout, RMFMatrixRowVars * matrixrowin);

/// \brief Open an existing fits file to hold the rmf and set up router
/// \param[out] ahffp       pointer to input file
/// \param[out] rmfdat      struct contains information for the entire rmf file
/// \param[out] matrix_extname EXTNAME of matrix extension (for lsf files)
void openRMFFile(RMFData * rmfdat, const std::string filename,
                 const std::string matrix_extname="MATRIX");   

/// \brief Create an empty fits file to hold the rmf and set up router
/// \param[out] ahffp       pointer to outputfile
/// \param[in]  rmfdat      struct contains information for the entire rmf file
void createRMFFile(RMFData * rmfdat, const std::string filename);   

/// \brief constructs the input energy mesh
/// \param[in] rmfdat     struct contains information for the entire rmf file
/// \param[in] ematstart  0...nmat-1 input energy bin low edge – in keV
/// \param[in] ematstop   0...nmat-1 input energy bin high edge – in keV
/// \param[in] ematcen    0...nmat-1 input energy bin center – in keV
/// \param[in] nmat       total number of input energy bins
void setInputEnergies(RMFData * rmfdat, double * ematstart, double * ematstop, 
                      double * ematcen,int nmat); 

/// \brief constructs the output energy mesh
/// \param[in] rmfdat     struct contains information for the entire rmf file
/// \param[in] echanstart  0...nmat-1 output energy channel low edge – in keV
/// \param[in] echanstop   0...nmat-1 output energy channel high edge – in keV
/// \param[in] echancen    0...nmat-1 output energy channel center – in keV
/// \param[in] nchan       total number of input energy bins
void setChannelEnergies(RMFData * rmfdat, double * echanstart, 
                        double * echanstop, double * echancen,int nchan);

/// \brief Calculate channel boundary at specified energy
/// \param[in] energy       Center of energy for energy bin
/// \param[in] emin_eb      Edge of energy channel
/// \param[in] de_mat       Energy grid spacing of output energy channel
/// \param[in] nchan        Maximum number of output energy 
///                         channels in response matrix
/// \param[out] channel     Output channel boundary
void energy2Chan(double energy, double emin_mat, double de_mat, int nchan, int & channel);

/// \brief Calculate energy peak of gaussian 
/// \param[in] Eo           Center of energy for energy bin
/// \param[in] chanstart    Channel start boundary
/// \param[in] chanstop     Channel stop boundary
/// \param[in] nchan        Maximum number of output energy channels in response matrix
/// \param[in] fwhm         FWHM for gaussian core
/// \param[in] ecen         Center of energy channel
/// \param[out] matrix      Output response for energy bin
/// \param[out] renorm      Output normalization factor for energy bin
void gaussianPeak(double Eo, int chanstart, int chanstop, int nchan, 
                  double fwhm, double * ecen, double * matrix, double & renorm);

/// \brief Calculate width of exponential shoulder
/// \param[in] Eo           Center of energy (shifted) from primary peak for energy bin
/// \param[in] nchan        Maximum number of output energy channels in response matrix
/// \param[in] tau          Decay width for exponential tail
/// \param[in] ecen         Center of energy channel
/// \param[out] ematrix     Output response for exponential energy bin
/// \param[out] renorm      Output normalization factor for exponential energy bin
void exponentialShoulder(double Eo, int chanstart, int chanstop, int nchan, 
                         double tau, double * ecen, double * ematrix,
                         double & renorm);

/// \brief Calculate continuum response matrix
/// \param[in] emincont     Minimum energy to which continuum extends
/// \param[in] Eo           Center of energy (shifted) from primary peak for energy bin
/// \param[in] nchan        Maximum number of output energy channels in response matrix
/// \param[in] ecen         Center of energy channel
/// \param[out] cmatrix     Output continuum response for energy bin
/// \param[out] renorm      Output continuum normalization factor for energy bin
void continuumPeak(double emincont, int chanstart, int chanstop, int nchan, 
                   double * ecen, double * cmatrix, double & renorm);


/// \brief creates the Ebounds extension and fills in ecen array
/// \param[in] rmfdat     struct contains information for the entire rmf file
void writeEboundsHDU(RMFData * rmfdat);

/// \brief creates the Matrix extension and fills in energy_mid array
/// \param[in] rmfdat     struct contains information for the entire rmf file
void createMatrixHDU(RMFData * rmfdat);

/// \brief read an input row and unpack into array with one element per channel
/// \param[in]  rmfdat     struct contains information for the entire rmf file
/// \param[out] totmatrix  one row of expanded matrix
/// \param[in]  nchan      total number of energy bins in expanded matrix row
/// \param[in]  rowidx     current row index in input file (zero-based index)
void readMatrixRow(RMFData * rmfdat, double * totmatrix, int nchan, int rowidx);

/// \brief determine rmf groups and write each output row
/// \param[in] rmfdat     struct contains information for the entire rmf file
/// \param[in] totmatrix  one row of total response matrix
/// \param[in] nchan      total number of input energy bin
/// \param[in] rowidx     current row index in output file (zero-based index)
/// \param[in] effarea    ARF element by which row is multiplied after all other processing
void writeMatrixRow(RMFData * rmfdat, double * totmatrix, int nchan, int rowidx, double effarea=1.0);

/// \brief 1. deallocates memory for energ_lo energ_hi and matrixrow
///        2. writes numgrp and num elements keywords
///        3. closes the rmf fits file
/// \param[in] rmfdat     struct contains information for the entire rmf file
void closeRMFFile(RMFData * rmfdat);

}

#endif   /* SXISXSRMFLIB_SXISXSRMFLIB_H */

/** Revision Log
 $Log: rmflib.h,v $
 Revision 1.18  2016/01/12 01:30:01  rshill
 Added optional matrix EXTNAME parameter to openRMFFile().

 Revision 1.17  2016/01/08 18:21:05  mwitthoe
 rmflib: read/writeMatrixRow routines to have the rowidx variable be zero-based instead of one-based

 Revision 1.16  2016/01/08 00:10:35  rshill
 Added functions to copy an RMFData structure; added multiplication
 by a scalar effective area to writeMatrixRow.

 Revision 1.15  2016/01/07 00:54:10  rshill
 Corrected keyword names used as structure member names.

 Revision 1.14  2015/12/30 00:50:44  rshill
 Fixed readMatrixRow arg list.

 Revision 1.13  2015/12/28 04:10:19  rshill
 Added detection of units (keV or not).

 Revision 1.12  2015/12/24 14:39:49  mwitthoe
 rmflib: 1) fix bug in readMatrixRow(), 2) allow to call readMatrixRow when in write-mode

 Revision 1.11  2015/12/24 01:42:32  rshill
 Added reading capability.  Made all calling sequences
 uniformly pass RMFData by pointer rather than by value.

 Revision 1.10  2015/12/09 15:48:23  mdutka
 fixing bug

 Revision 1.9  2015/12/08 19:11:57  mdutka
 increase the maximum number of groups for output response matrix

 Revision 1.8  2015/10/19 18:45:13  mwitthoe
 rmflib: remove getPeaks() function which is no used by sxsrmf (or any other tool)

 Revision 1.7  2015/08/26 00:08:08  mwitthoe
 rmflib: fix bad library unit test - there was an array out-of-bounds error which showed up on some systems; also fix the description of the fpk_mid argument in the getPeaks function

 Revision 1.6  2015/07/27 17:23:25  mdutka
 updating rmflib after code review july 2015

 Revision 1.5  2014/09/10 14:56:17  mdutka
 Updated rmflib to handle writing large rmf files

 Revision 1.4  2014/08/11 17:02:10  mdutka
 Added new functions which will use less system memory when writing the response matrix see issue #407

 Revision 1.3  2014/06/24 17:52:06  asargent
 Simplified gaussianPeak, exponentialShoulder and continuumPeak functions with addition of energy2Chan function. Removed escapePeaks function. Fixed indexing error with calcMatrix.

 Revision 1.2  2014/05/23 19:43:31  asargent
 Removed vector include

 Revision 1.1  2014/05/23 18:49:14  asargent
 New RMF tools library

*/
