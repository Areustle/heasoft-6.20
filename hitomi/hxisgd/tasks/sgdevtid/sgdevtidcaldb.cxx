/// \file sgdevtidcaldb.cxx
/// \brief functions to act on the sequence probability file for SGD
/// \author Robert S. Hill
/// \date $Date: 2015/08/18 00:12:30 $

#define AHLABEL tool_sgdevtid_sgdevtidcaldb
#define AHCVSID "$Id: sgdevtidcaldb.cxx,v 1.13 2015/08/18 00:12:30 mwitthoe Exp $"

#include "sgdevtidcaldb.h"
#include "ahmath/ahmath.h"
#include "ahlog/ahlog.h"

#include <sstream>         // std::stringstream
#include <algorithm>       // std::find

namespace sgdprobseq {

void load(const std::string& filename, DataType & probseq_table) {

  // variables for FITS columns
  int l_numhits;
  int l_sequence[5];
  double l_probhits;
  int l_mechanism;
  std::string l_mechanism1;

  // check if table already loaded
  if (probseq_table.m_data != 0)
    AH_THROW_LOGIC("sequence probability CALDB data already loaded!");

  // open file
  ahfits::FilePtr fptr;
  ahfits::open(filename,"PROB_SEQUENCE",&fptr);   // expecting HDU name == DETNAM
  if (0 == ahfits::numRows(fptr)) {
    AH_THROW_RUNTIME("Sequence probability FITS file contains no data: "+filename);
  }

  // get number of rows in input file and allocated space in struct
  int nrows=ahfits::numRows(fptr);
  probseq_table.m_data=new DataRow[nrows];
  probseq_table.m_size=nrows;

  // make connections between local variables and FITS columns
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_READONLY,"NUMHITS",l_numhits);
  router.connectFixedLengthArray(ahfits::e_READONLY,"SEQUENCE",l_sequence);
  router.connectScalar(ahfits::e_READONLY,"PROBHITS",l_probhits);
  router.connectScalar(ahfits::e_READONLY,"MECHANISM",l_mechanism);
  router.connectScalar(ahfits::e_READONLY,"MECHANISM1",l_mechanism1);

  // read table
  int irow=0;
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    probseq_table.m_data[irow].m_numhits=l_numhits;
    for (int j=0; j<5; ++j) {
      probseq_table.m_data[irow].m_sequence[j]=l_sequence[j];
    }
    probseq_table.m_data[irow].m_probhits=l_probhits;
    probseq_table.m_data[irow].m_mechanism=l_mechanism;
    probseq_table.m_data[irow].m_mechanism1=l_mechanism1;
    ++irow;
  }

  // close FITS file
  ahfits::close(fptr);
}

int find (const int* mat, const bool escape_flag, DataType& probseq_table) {
  int imatch = -1;

  // get number of hits in given sequence
  int m=0;
  for (m=0; m < 4; ++m) {    // 4 is the max number of hits
    if (mat[m] < 0) break;   // -1 indicates end of sequence
  }

  for (int i=0; i<probseq_table.m_size; ++i) {
    if (m == probseq_table.m_data[i].m_numhits) {
      bool all_match = true;
      for (int j=0; j<m; ++j) {
        if (mat[j] != probseq_table.m_data[i].m_sequence[j]) {
          all_match = false;
          break;
        }
        if (escape_flag && probseq_table.m_data[i].m_sequence[4] != ESCAPE) {
          all_match = false;
          break;
        }
      }
      if (all_match) {
        imatch = i;
        break;
      }
    }
  }
  return imatch;
}

double lookupProbability (const int* mat, const bool escape_flag, DataType& probseq_table) {
  int imatch=find(mat,escape_flag,probseq_table);
  if (imatch >= 0) {
    return probseq_table.m_data[imatch].m_probhits;
  } else {
    AH_THROW_RUNTIME("Probabability lookup failed in probseqfile");
  }
}

int lookupMechanism (const int* mat, const bool escape_flag, DataType& probseq_table) {
  int imatch=find(mat,escape_flag,probseq_table);
  if (imatch >= 0) {
    return probseq_table.m_data[imatch].m_mechanism;
  } else {
    AH_THROW_RUNTIME("Mechanism lookup failed in probseqfile");
  }
}

}  // namespace sgdprobseq

// *******************************************************************

namespace sgdprobfov {

// -----------------------------------------------------------------------------

void load(const std::string& filename, bool storeprob, DataType & fovdat) {

  // check if table already loaded
  if (fovdat.m_ahffp != 0) 
    AH_THROW_LOGIC("FOV probability file already loaded!");

  // clear parameter vectors (just in case)
  for (int i=0; i < NPAR; i++) fovdat.m_parvecs[i].clear();

  // open file
  ahfits::open(filename,"LIKELIHOOD",&fovdat.m_ahffp);
  if (0 == ahfits::numRows(fovdat.m_ahffp)) {
    AH_THROW_RUNTIME("FOV probability FITS file contains no data: "+filename);
  }

  // Get list (and order) of parameters.  The values read will be the
  // names of the extensions containing the parameter bin values.
  std::string parnames[NPAR];
  for (int i=0; i < NPAR; i++) {
    std::stringstream keyname;
    keyname << "PARAM" << i;
    parnames[i]=ahfits::getKeyValStr(fovdat.m_ahffp,keyname.str());
    fovdat.m_parmap[parnames[i]]=i;     // add name->index to parameter map
  }

  // Read parameter bins
  for (int i=0; i < NPAR; i++) loadParameterBins(parnames[i],i,fovdat);

  // Move back to PROB_FOV extension and set up router
  ahfits::move(fovdat.m_ahffp,"LIKELIHOOD");
  fovdat.m_router=new ahfits::Router(fovdat.m_ahffp);
  fovdat.m_router->connectScalar(ahfits::e_READONLY,"LIKELIHOOD",fovdat.m_prob);

  // If storing probability table in memory, read in table and close FITS file
  if (storeprob) {
    fovdat.m_storeprob=true;

    // preallocate vector in memory (instead of increasing vector per element
    fovdat.m_probvec.resize(ahfits::numRows(fovdat.m_ahffp));

    long long irow=0;
    for (ahfits::firstRow(fovdat.m_ahffp); ahfits::readOK(fovdat.m_ahffp); ahfits::nextRow(fovdat.m_ahffp)) {
      ahfits::readRow(fovdat.m_ahffp);
      fovdat.m_probvec[irow]=fovdat.m_prob;   // m_prob filled by readRow()
      irow++;
    }
    sgdprobfov::cleanUp(fovdat);   // close router and FITS file
  }

}

// -----------------------------------------------------------------------------

void enableInterpolation(const std::string& parname, DataType & fovdat) {

  // check if table loaded
  if (fovdat.m_parmap.size() == 0) AH_THROW_LOGIC("load FOV table before enabling parameter for interpolation: "+parname);

  // check if parname is valid
  if (fovdat.m_parmap.count(parname) == 0) AH_THROW_LOGIC("parameter not found: "+parname);

  // get index for parameter
  int idx=fovdat.m_parmap[parname];

  // if parameter only has a single value, interpolation cannot be performed
  if (fovdat.m_parvecs[idx].size() < 2) AH_THROW_LOGIC("cannot interpolate on parameter with a single value: "+parname);

  // if index already in list, nothing to be done
  for (unsigned int i=0; i < fovdat.m_parinterp.size(); i++) {
    if (idx == fovdat.m_parinterp[i]) return;
  }

  // add index to list
  fovdat.m_parinterp.push_back(idx);

}

// -----------------------------------------------------------------------------

double lookupFOVProbability(ParValues& parvals, DataType& fovdat) {

  // convert parameter value map into an array of values in the correct order
  double* pvalarr=new double[NPAR];
  ParMap::iterator it;    // it.first = parameter name; it.second = parameter index
  for (it = fovdat.m_parmap.begin(); it != fovdat.m_parmap.end(); it++) {
    if (parvals.count(it->first) == 0) AH_THROW_LOGIC("parameter not found in parvals map: "+(it->first));
    pvalarr[it->second]=parvals[it->first];
  }

  // search parameters and fill array of individual parameter indices
  int indices[NPAR];
  for (int i=0; i < NPAR; i++) {
    // search is a little different depending on whether interpolation
    // is used for a parameter
    int mode=ahmath::NEAREST;
    if (std::find(fovdat.m_parinterp.begin(), fovdat.m_parinterp.end(), i) != fovdat.m_parinterp.end())
      mode=ahmath::TWOPOINT;

    indices[i]=sgdprobfov::getParameterIndex(pvalarr[i],i,mode,fovdat);
    if (indices[i] < 0) return 0.;     // lookup failure if extrapolating
  }

  // get number of interpolated parameters
  int ninterp=fovdat.m_parinterp.size();

  // call recursive lookup function which will call itself for each 
  // parameter being interpolated
  return lookup(pvalarr,indices,ninterp,fovdat);
}

// -----------------------------------------------------------------------------

double lookupFOVProbability_old(double* parvals, int ninterp, DataType& fovdat) {

  // search parameters and fill array of individual parameter indices
  int indices1[NPAR];
  for (int i=0; i < NPAR; i++) {
    // search is a little different depending on whether interpolation
    // is used for a parameter
    int mode=ahmath::NEAREST;
    if (std::find(fovdat.m_parinterp.begin(), fovdat.m_parinterp.end(), i) != fovdat.m_parinterp.end())
      mode=ahmath::TWOPOINT;

    indices1[i]=sgdprobfov::getParameterIndex(parvals[i],i,mode,fovdat);
  }

  // a 2nd set of indices will be needed if doing interpolation
  int indices2[NPAR];
  for (int i=0; i < NPAR; i++) indices2[i]=indices1[i];

  // get probability row number from parameter indices
  long long row=sgdprobfov::formRowNumber(indices1,fovdat);

  // retrieve probability
  ahfits::gotoRow(fovdat.m_ahffp,row);
  ahfits::readRow(fovdat.m_ahffp);
  float prob=fovdat.m_prob;      // this is filled by the readRow operation

  // interpolation on at least one parameter
  if (ninterp > 0) {

    // cannot do interpolation if only one point for parameter
    if (fovdat.m_parvecs[0].size() == 1)
      AH_THROW_RUNTIME("cannot perform interpolation; only one point for 1st parameter in FOV probability file");

    // increase index of first parameter
    indices2[0]++;

    // get probability of 2nd point
    row=sgdprobfov::formRowNumber(indices2,fovdat);
    ahfits::gotoRow(fovdat.m_ahffp,row);
    ahfits::readRow(fovdat.m_ahffp);
    float prob2=fovdat.m_prob;   // this is filled by the readRow operation

    // apply linear interpolation
    int idx1=indices1[0];
    int idx2=indices2[0];
    prob=ahmath::interpolate_point_twopoint(parvals[0],                        // where to interpolate
                                            fovdat.m_parvecs[0][idx1],prob,    // 1st point
                                            fovdat.m_parvecs[0][idx2],prob2);  // 2nd point

  }

  // if interpolating two parameters, need to repeat everything after 
  // increasing the index of the 2nd parameter
  if (ninterp > 1) {

    // cannot do interpolation if only one point for 2nd parameter
    if (fovdat.m_parvecs[1].size() == 1)
      AH_THROW_RUNTIME("cannot perform interpolation; only one point for 2nd parameter in FOV probability file");

    // store original index & probability of 2nd parameter
    int idx0=indices1[1];
    float prob0=prob;

    // increase index of 2nd parameter in both sets of indices
    indices1[1]++;
    indices2[1]++;

    // get 1st probability
    row=sgdprobfov::formRowNumber(indices1,fovdat);
    float prob1=sgdprobfov::getProbability(row,fovdat);
    
    // get 2nd probability
    row=sgdprobfov::formRowNumber(indices2,fovdat);
    float prob2=sgdprobfov::getProbability(row,fovdat);

    // perform interpolation 2nd set of points
    int idx1=indices1[0];
    int idx2=indices2[0];
    prob=ahmath::interpolate_point_twopoint(parvals[0],                        // where to interpolate
                                            fovdat.m_parvecs[0][idx1],prob1,   // 1st point
                                            fovdat.m_parvecs[0][idx2],prob2);  // 2nd point

    // now interpolate between 1st and 2nd sets of points
    idx1=idx0;
    idx2=indices1[1];
    prob=ahmath::interpolate_point_twopoint(parvals[1],                        // where to interpolate
                                            fovdat.m_parvecs[1][idx1],prob0,   // 1st point
                                            fovdat.m_parvecs[1][idx2],prob);   // 2nd point
  }

  return prob;
}

// -----------------------------------------------------------------------------

void cleanUp(DataType & fovdat) {

  if (fovdat.m_router != 0) {
    delete fovdat.m_router;
    fovdat.m_router=0;
  }

  if (fovdat.m_ahffp != 0) {
    ahfits::close(fovdat.m_ahffp);
    fovdat.m_ahffp=0;
  }
}

// -----------------------------------------------------------------------------

void loadParameterBins(const std::string& extname, int ipar, DataType& fovdat) {

  if (ipar < 0 || ipar >= 8) AH_THROW_LOGIC("invalid parameter index");

  ahfits::move(fovdat.m_ahffp,extname);

  double l_binval=0.;
  ahfits::Router rout(fovdat.m_ahffp);
  rout.connectScalar(ahfits::e_READONLY,"VALUE",l_binval);

  for (ahfits::firstRow(fovdat.m_ahffp);ahfits::readOK(fovdat.m_ahffp);ahfits::nextRow(fovdat.m_ahffp)) {
    ahfits::readRow(fovdat.m_ahffp);
    fovdat.m_parvecs[ipar].push_back(l_binval);
  }

}

// -----------------------------------------------------------------------------

int getParameterIndex(double pval, int ipar, int mode, DataType& fovdat) {

  // number of parameter bins
  ahmath::IndexType nbins=fovdat.m_parvecs[ipar].size();

  // alias for pointer to C array inside std::vector container for parameter
  double* parr=&fovdat.m_parvecs[ipar][0];

  // if only one bin, just return index 0
  if (nbins == 1) return 0;

  // use ahmath to perform search
  // the index will refer to the bin value immediately below the search value
  ahmath::IndexType idx=0;
  bool extrap=false;
  idx=ahmath::search(pval,parr,nbins,extrap,idx);
  if (extrap) return -1;

  // If extrap == false and idx == nbins-1 (last index), then the input
  // value is equal to the last parameter value.  To allow interpolation
  // or nearest-neighbor method to work, need to decrement index.
  // Note: this code is superfluous since we are returning -1 if extrapolating,
  // however it will be needed if extrapolation is ever permitted.
  if (!extrap && idx == (nbins-1)) idx--;

  // When using linear interpolation, no more work is needed.  We will need
  // to use idx and idx+1 later.  For nearest-neighbor interpolation, however,
  // we want to return the index whose bin value is closer to the search value.
  // Note: if extrapolating and the search index was not zero, then it means
  // that the given parameter value is larger than the last bin.  In this case,
  // the index is for the second-to-last bin to allow linear extrapolation,
  // so increase it by one to point to the last bin.
  if (mode == ahmath::NEAREST) {
    if (extrap) {
      if (idx > 0) idx++;
    } else {
      double dbelow=pval-parr[idx];
      double dabove=parr[idx+1]-pval;
      if (dbelow > dabove) idx++;
    }
  }

  return (int)idx;
}

// -----------------------------------------------------------------------------

long long formRowNumber(int* indices, DataType& fovdat) {

  // This function implements the following expression:
  //
  //  row = 1 + sum_{j=1}^{NPAR} (i_j - 1) product_{k=j+1}^{NPAR} n_k
  //
  // where
  //   NPAR = 8 is the number of parameters
  //   i_j is the index of the j'th parameter resulting from the search
  //   n_k is the number of values for the k'th parameter

  long long row=1;
  long long period=1;
  for (int i=0; i < NPAR; i++) {
    if (indices[i] >= (int)fovdat.m_parvecs[i].size()) {
      std::stringstream msg;
      msg << "Trying to form row number with invalid index; par, index: " << i << ", " << indices[i] << std::endl;
      AH_THROW_LOGIC(msg.str());
    }
    row+=indices[i]*period;
    period*=fovdat.m_parvecs[i].size();
  }
  return row;
}

// -----------------------------------------------------------------------------

float getProbability(long long row, DataType& fovdat) {
  if (fovdat.m_storeprob) {
    return fovdat.m_probvec[row-1];   // index 0 == row 1
  } else {
    ahfits::gotoRow(fovdat.m_ahffp,row);
    ahfits::readRow(fovdat.m_ahffp);
    return fovdat.m_prob;
  }
}

// -----------------------------------------------------------------------------

double lookup(double* pvalarr, int* indices, int ninterp, DataType& fovdat) {

  // This is the termination of the recursive iteration.  Just return the
  // probability associated with the given indices.
  if (ninterp == 0) {
    long long row=sgdprobfov::formRowNumber(indices,fovdat);
    return sgdprobfov::getProbability(row,fovdat);
  }

  // parameter index for interpolation
  int paridx=fovdat.m_parinterp[ninterp-1];

  // get first probability - uses input indices
  int indices1[NPAR];
  for (int i=0; i < NPAR; i++) indices1[i]=indices[i];
  float prob1=sgdprobfov::lookup(pvalarr, indices1, ninterp-1, fovdat);

  // get second probability - increments index given by ninterp
  int indices2[NPAR];
  for (int i=0; i < NPAR; i++) indices2[i]=indices[i];
  indices2[paridx]++;
  float prob2=sgdprobfov::lookup(pvalarr, indices2, ninterp-1, fovdat);

  // perform interpolation
  int idx1=indices1[paridx];                   // search position for 1st point
  int idx2=indices2[paridx];                   // search position for 2nd point
  float par1=fovdat.m_parvecs[paridx][idx1];   // parameter value at 1st point
  float par2=fovdat.m_parvecs[paridx][idx2];   // parameter value at 2nd point
  float prob=ahmath::interpolate_point_twopoint(pvalarr[paridx],      // where to interpolate
                                                par1,prob1,           // 1st point
                                                par2,prob2);          // 2nd point

  return prob;

}

// -----------------------------------------------------------------------------

}  // namespace sgdprobfov

/* Revision Log
 $Log: sgdevtidcaldb.cxx,v $
 Revision 1.13  2015/08/18 00:12:30  mwitthoe
 sgdevtid: fix error message when CALDB files have no rows

 Revision 1.12  2015/03/18 19:58:21  asargent
 Changed DETNAME to DETNAM

 Revision 1.11  2015/03/03 18:37:33  mwitthoe
 sgdevtid: update after Feb 2015 Japan meeting: store delta-F in F test; remove randomization in fluorescence merging; remove expand mode; add energy-dependent energy uncertainties

 Revision 1.10  2015/01/29 14:10:37  mwitthoe
 sgdevtid: fix out-of-bounds error when searching for a parameter value which is equal to the last bin value; add error message when parameter index is out-of-bounds when forming probability row number

 Revision 1.9  2015/01/22 21:55:23  mwitthoe
 sgdevit: implement FOV probability; see issue 482

 Revision 1.8  2014/12/24 18:31:33  rshill
 Updated for parameter standards (Redmine issue #472)

 Revision 1.7  2014/07/24 17:40:14  mwitthoe
 sgdevtid: update probm CALDB file format; bug-fix: there were two expand variables being used; bug-fix: there was a mix-up between layer and sensor index when determining location of a signal; bug-fix: extrainfo was not being checked before trying to write the extra output file in sgdevtidlib

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
