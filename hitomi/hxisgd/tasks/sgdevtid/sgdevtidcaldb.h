/// \file sgdevtidcaldb.h
/// \brief Functions to act on probability CALDB files for SGD.
/// \author Robert S. Hill
/// \date $Date: 2015/03/03 18:37:33 $

/// \addtogroup tool_sgdevtid
/// \section hxievtid_sgdevtidcaldb Sequence and event probability data for SGD - sgdevtidcaldb
///
/// SGD event reconstruction requires probabilities for hit sequences 
/// based on the configuration of detector layers and of specific
/// pixels where the hits take place.  It also requires a field-of-view
/// probability for the the reconstructed event, which is essentially
/// a likelihood that it comes from the observed target rather than
/// background.  These probabilities are obtained from CALDB files,
/// which are accessed using the following functions.

#ifndef SGDEVTID_SGDEVTIDCALDB_H
#define SGDEVTID_SGDEVTIDCALDB_H

#include "ahgen/ahversion.h"
AHVERSION(SGDEVTID_SGDEVTIDCALDB,"$Id: sgdevtidcaldb.h,v 1.9 2015/03/03 18:37:33 mwitthoe Exp $")

#include "ahfits/ahfits.h"

#include <string>
#include <vector>
#include <map>

/// \ingroup tool_sgdevtid

namespace sgdprobseq {

/** \addtogroup tool_sgdevtid
 *  @{
 */

const int ESCAPE=3;

struct DataRow {

  DataRow(): m_numhits(0), m_probhits(0.), m_mechanism(0), m_mechanism1("") {}

  int m_numhits;
  int m_sequence[5];
  double m_probhits;
  int m_mechanism;
  std::string m_mechanism1;
};

struct DataType {

  DataType(): m_size(0), m_data(0) {}

  ~DataType() {
    if (m_data != 0) {
      delete [] m_data;
      m_data=0;
    }
  }

  int m_size;                     ///< size of data array
  DataRow* m_data;                ///< array to store all row data
};

/// \brief read CALDB file
/// \param[in] filename name of remapping file
/// \param[out] probseq_table structure containing CALDB data
void load(const std::string& filename, DataType & probseq_table);

/// \brief (internal) Find row associated with given sequence
/// \param[in] mat a vectof of materials (0=Si, 1=CdTe bottom, 2=CdTe side)
/// \param[in] escape_flag True if escape energy was used in calculation
/// \param[out] probseq_table structure containing CALDB data
/// \return matching row index
///
/// This function is not intended to be used outside this library.
int find (const int* mat, const bool escape_flag, DataType& probseq_table);

/// \brief Look up probability for a sequence.
/// \param[in] mat a vectof of materials (0=Si, 1=CdTe bottom, 2=CdTe side)
/// \param[in] escape_flag True if escape energy was used in calculation
/// \param[out] probseq_table structure containing CALDB data
/// \return probability of sequence occurring
double lookupProbability (const int* mat, const bool escape_flag, DataType& probseq_table);

/// \brief Look up mechanism index for a sequence.
/// \param[in] mat a vectof of materials (0=Si, 1=CdTe bottom, 2=CdTe side)
/// \param[in] escape_flag True if escape energy was used in calculation
/// \param[out] probseq_table structure containing CALDB data
/// \return mechanism index assigned to sequence
int lookupMechanism (const int* mat, const bool escape_flag, DataType& probseq_table);

/** @} */

}  // namespace sgdprobseq

/// \ingroup tool_sgdevtid

namespace sgdprobfov {

/** \addtogroup tool_sgdevtid
 *  @{
 */

/// \brief number of parameter types
const int NPAR=8;

/// \brief type to map parameter name to internal index
typedef std::map<std::string, int> ParMap;

/// \brief type to map parameter name to value (double)
typedef std::map<std::string, double> ParValues;

/// \brief type to store parameter bin values
typedef std::vector<double> ParVec;

/// \brief type to store probabilities
typedef std::vector<float> ProbVec;

/// \brief type to store parameter indices for interpolation
typedef std::vector<int> InterpVec;

/// \brief Store all parameter bin arrays and set up access to probabilities.
///  If m_storeprob=true, then the entire PROB_FOV table is read into memory
///  (m_probvec).  If storeprob=false, then a router (m_router) to the PROB_FOV
///  table will be set up for reading the file directly from disk.  The local
///  variable for this router is m_prob.
struct DataType {

  DataType(): m_storeprob(false), m_ahffp(0), m_router(0), m_prob(0) {}

  ~DataType() {
    // delete router and close FITS file (if necessary)
    if (m_router != 0) {
      delete m_router;
      m_router=0;
    }
    if (m_ahffp != 0) {
      ahfits::close(m_ahffp);
      m_ahffp=0;
    }
  }

  bool m_storeprob;             ///< true if probabilities are stored in memory
  ahfits::FilePtr m_ahffp;      ///< ahfits FilePtr referencing open file
  ahfits::Router* m_router;     ///< ahfits router for reading probability
  float m_prob;                 ///< local variable for reading probability
  ProbVec m_probvec;            ///< vector for storing entire probability table
  ParMap m_parmap;              ///< map of parameter name to internal index
  ParVec m_parvecs[NPAR];       ///< array of parameter bin vectors
  InterpVec m_parinterp;        ///< vector of parameter indices to interpolate
};

/// \brief Set up structure for reading FOV probabilities.
/// \param[in] filename name of FOV probability file
/// \param[in] storeprob true to store probability table in memory
/// \param[out] fovdat structure for reading probabilities
void load(const std::string& filename, bool storeprob, DataType & fovdat);

/// \brief Flag given parameter for interpolation.
/// \param[in] parname name of parameter
/// \param[out] fovdat structure for reading probabilities
void enableInterpolation(const std::string& parname, DataType & fovdat);

/// \brief Look up probability for list of parameters.
/// \param[in] parvals array of parameter values
/// \param[in] fovdat structure for reading probabilities
/// \return FOV probability
double lookupFOVProbability(ParValues& parvals, DataType& fovdat);

/// \brief Look up probability for list of parameters (old method).
/// \param[in] parvals array of parameter values
/// \param[in] ninterp number of parameters to interpolate on
/// \param[in] fovdat structure for reading probabilities
/// \return FOV probability
double lookupFOVProbability_old(double* parvals, int ninterp, DataType& fovdat);

/// \brief (internal) Close FOV FITS file
/// \param[in,out] fovdat structure for reading probabilities
void cleanUp(DataType & fovdat);

/// \brief (internal) Read bins for single parameter.
/// \param[in] extname name of parameter extension to read
/// \param[in] ipar parameter index
/// \param[in] fovdat structure for reading probabilities
///
/// This function is not intended to be used outside this library.
void loadParameterBins(const std::string& extname, int ipar, DataType& fovdat);

/// \brief (internal) Search for index for given parameter value
/// \param[in] parval parameter value to search for
/// \param[in] ipar parameter index
/// \param[in] mode search mode: 0 = nearest neighbor; 1 = linear interpolation
/// \param[in] fovdat structure for reading probabilities
/// \return parameter index, or negative value if out-of-range
///
/// This function is not intended to be used outside this library.
int getParameterIndex(double parval, int ipar, int mode, DataType& fovdat);

/// \brief (internal) Form row number for array of indices.
/// \param[in] indices output search indices for each parameter
/// \param[in] fovdat structure for reading probabilities
/// \return row number in probability extension
long long formRowNumber(int* indices, DataType& fovdat);

/// \brief (internal) Retrieve single probability from file or memory.
/// \param[in] row row number to get
/// \param[in] fovdat structure for reading probabilities
/// \return probability at given row number
float getProbability(long long row, DataType& fovdat);

/// \brief (internal) Recursive function to perform interpolations.
/// \param[in] parvals array of parameter values to search for
/// \param[in] indices search indices for each parameter
/// \param[in] ninterp interpoltion order (counts down to zero to terminate
///            recursion loop)
/// \param[in] fovdat structure for reading probabilities
/// \return probability
double lookup(double* parvals, int* indices, int ninterp, DataType& fovdat);


/** @} */

}  // namespace sgdprobfov

#endif /* SGDEVTID_SGDEVTIDCALDB_H */

/* Revision Log

 $Log: sgdevtidcaldb.h,v $
 Revision 1.9  2015/03/03 18:37:33  mwitthoe
 sgdevtid: update after Feb 2015 Japan meeting: store delta-F in F test; remove randomization in fluorescence merging; remove expand mode; add energy-dependent energy uncertainties

 Revision 1.8  2015/01/22 21:55:24  mwitthoe
 sgdevit: implement FOV probability; see issue 482

 Revision 1.7  2014/12/24 18:31:33  rshill
 Updated for parameter standards (Redmine issue #472)

 Revision 1.6  2014/03/21 18:14:33  mwitthoe
 sgdevtid: in probm CALDB file 1) change type of probhit from int to double, 2) rename columns to match test file sent by Hiro on Mar 20; add best_k to hits structure; print hit sequence to log file (with AH_DEBUG) when occurrence_id is used; initialize test_f/g/prob to false instead of true

 Revision 1.5  2014/03/13 21:03:43  mwitthoe
 sgdevtid: add random selection for Step 1a-2; change FOM expression; include extension name in CALDB lookup function for the badpix file; if single F test passes with M>2, continue to G test; add lookup function for ProbM file to return the MECHANISM index which gets output in the SEQHITS column; print number of occurrences which have PI=NULL to end of log file; remove some old, commented-out code

 Revision 1.4  2014/01/31 00:15:54  rshill
 Added access to probm and probd files and deuggued via library unit tests.

 Revision 1.3  2014/01/30 20:42:13  rshill
 Added in actually using the probm CALDB file.

 Revision 1.2  2014/01/25 01:37:07  rshill
 Brought closer to TRF with differences called out.

 Revision 1.1  2014/01/24 19:45:34  rshill
 First commit of CALDB support for SGD event reconstruction.


*/
