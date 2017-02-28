/// \file spline.cxx
/// \brief Read CALDB file containing spline coefficients.
/// \author Robert S. Hill
/// \date $Date: 2015/08/19 17:11:08 $

#define AHLABEL ahmission_spline
#define AHCVSID "$Id: spline.cxx,v 1.4 2015/08/19 17:11:08 mwitthoe Exp $"

#include "ahmission/spline.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"

#include <sstream>

namespace ahmission {

namespace spline {

// ---------------------------------------------------------------------------

void load(const std::string& filename, const std::string& extname,
          const std::string& indexcol, const std::string& instrume,
          const std::string& detnam, SplineSetMap& spline_table) {

  long max_ninterval=0;                     // maximum number of intervals
  ahfits::FilePtr fptr;                     // ahfits file pointer
  std::pair<double, SplineInterval> p;       // single spline interval

  // reset spline data
  spline_table.clear();

  // There may be multiple extensions with the same extension name, but 
  // different INSTRUME and DETNAM values.  So, we iterate over all extensions
  // until we find a match.
  bool found=false;
  ahfits::open(filename,"",&fptr);
  ahfits::firstHDU(fptr,ahfits::e_BINARY_TBL);
  do {
    if (extname != ahfits::getKeyValStr(fptr,"EXTNAME")) continue;
    if (instrume != "" && instrume != ahfits::getKeyValStr(fptr,"INSTRUME")) continue;
    if (detnam != "" && detnam != ahfits::getKeyValStr(fptr,"DETNAM")) continue;
    found=true;
  
    // determine maximum number of intervals
    max_ninterval=ahfits::columnRepeat(fptr,"INTERVALX")-1;
    
    // declare variables for reading coefficients
    int l_index;
    int l_nintervals;
    double* l_intervalx=new double[max_ninterval+1];
    double* l_coeffx3=new double[max_ninterval];
    double* l_coeffx2=new double[max_ninterval];
    double* l_coeffx1=new double[max_ninterval];
    double* l_coeffx0=new double[max_ninterval];
  
    // set up ahfits to parse the file.
    ahfits::Router router(fptr);
    ahfits::IndexType node_count, coeffx3_count, coeffx2_count, coeffx1_count, coeffx0_count;
    bool okay=true;      // if error in connect(), still want to free memory and close file
    try {
      router.connectScalar(ahfits::e_READONLY,indexcol,l_index);
      router.connectScalar(ahfits::e_READONLY,"NINTERVAL",l_nintervals);
      router.connectVariableLengthArray(ahfits::e_READONLY,"INTERVALX",l_intervalx, node_count);
      router.connectVariableLengthArray(ahfits::e_READONLY,"COEFFX3", l_coeffx3, coeffx3_count);
      router.connectVariableLengthArray(ahfits::e_READONLY,"COEFFX2", l_coeffx2, coeffx2_count);
      router.connectVariableLengthArray(ahfits::e_READONLY,"COEFFX1", l_coeffx1, coeffx1_count);
      router.connectVariableLengthArray(ahfits::e_READONLY,"COEFFX0", l_coeffx0, coeffx0_count);
    } catch (...) {
      okay=false;
    }
  
    // read spline data
    if (okay) {
      for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
        ahfits::readRow(fptr);
    
        // set min/max x-values
        (spline_table[l_index]).m_x_domain_min = l_intervalx[0];
        (spline_table[l_index]).m_x_domain_max = l_intervalx[node_count-1];
    
        for (int j=0; j < l_nintervals; ++j) {
    
          // The variable p holds a single entry in the internal CALDB buffer; 
          // p is a C++ std::pair, which holds a key and a value.
    
          // Save the upper bound of the interval into the key field,
          // which is a simple scalar.
          p.first = l_intervalx[j+1];
    
          // Put the definition of the spline interval into the value field,
          // which is a struct holding the interval endpoints and the spline
          // coefficients.
          p.second.m_coeffx[0] = l_coeffx0[j];
          p.second.m_coeffx[1] = l_coeffx1[j];
          p.second.m_coeffx[2] = l_coeffx2[j];
          p.second.m_coeffx[3] = l_coeffx3[j];
    
          // Insert the entry into the internal CALDB buffer.
          (spline_table[l_index]).m_spline.insert(p);
        }
      }
    }
  
    // free memory
    delete [] l_intervalx; l_intervalx=0;
    delete [] l_coeffx3; l_coeffx3=0;
    delete [] l_coeffx2; l_coeffx2=0;
    delete [] l_coeffx1; l_coeffx1=0;
    delete [] l_coeffx0; l_coeffx0=0;

    if (!okay) {
      AH_INFO(ahlog::HIGH) << "Unable to load spline table (invalid index column name?)" << std::endl;
      AH_INFO(ahlog::HIGH) << "  File name: " << filename << std::endl;
      AH_INFO(ahlog::HIGH) << "  Extension: " << extname << std::endl;
      AH_INFO(ahlog::HIGH) << "  INSTRUME:  " << instrume << std::endl;
      AH_INFO(ahlog::HIGH) << "  DETNAM:    " << detnam << std::endl;
      AH_THROW_RUNTIME("Unable to load spline table.");
    }

  } while (ahfits::nextHDU(fptr,ahfits::e_BINARY_TBL));

  if (!found) {
    AH_INFO(ahlog::HIGH) << "Could not find extension matching search criteria" << std::endl;
    AH_INFO(ahlog::HIGH) << "  File name: " << filename << std::endl;
    AH_INFO(ahlog::HIGH) << "  Extension: " << extname << std::endl;
    AH_INFO(ahlog::HIGH) << "  INSTRUME:  " << instrume << std::endl;
    AH_INFO(ahlog::HIGH) << "  DETNAM:    " << detnam << std::endl;
    AH_THROW_RUNTIME("Failed to find spline table.");
  }
}

// ---------------------------------------------------------------------------

int get_coeff(SplineSetMap& spline_table, int index, double x, 
              bool& at_boundary, double coeff_set1[4], double coeff_set2[4]) {

  int i; // loop variable
  std::stringstream msg;  // message text

  // iterator for spline data
  std::map<double, SplineInterval>::iterator it;

  // alias for the current entry in the internal CALDB buffer
  SplineSet* spline_set=0;

  if (spline_table.empty()) AH_THROW_RUNTIME("Cannot get spline coefficients; spline data table is empty.");

  // check if spline set exists for given index
  if (spline_table.count(index) == 0) {
    msg << "No spline data present for index = " << index;
    AH_THROW_RUNTIME(msg.str());
  }

  // get spline set for given index
  spline_set=&spline_table[index];

  // return a bad code if x outside of the domain of the spline
  if (x < spline_set->m_x_domain_min || x > spline_set->m_x_domain_max) {
    return 1;
  }

  // Find the interval containing input argument x.
  //
  // The C++ lower_bound function finds the lowest key such that
  // x is a lower bound on key.   This key is then the upper bound of
  // the interval containing x.
  //
  // If x is greater or equal to than the highest key, this search
  // returns a fictitious "last-plus-one" table entry.  Because we
  // have already tested that x is in the domain of the spline (above),
  // this means that x is exactly equal to the upper limit.
  it=spline_set->m_spline.lower_bound(x);
  if (it == spline_set->m_spline.end()) {
    --it;          // x is high, so go back to the last interval.
  }

  // copy the coefficients to the argument
  for (i=0;i<4;++i) coeff_set1[i] = it->second.m_coeffx[i];

  // If x is exactly on a boundary, also get the next set of coefficients
  // (unless x is in the highest bin).
  if (x == it->first && x < spline_set->m_x_domain_max) {
    at_boundary = true;
    ++it;
    for (i=0;i<4;++i) coeff_set2[i] = it->second.m_coeffx[i];
  } else {
    at_boundary = false;
    for (i=0;i<4;++i) coeff_set2[i] = 0.0;
  }
  return 0;
}

// ---------------------------------------------------------------------------

int evaluate(SplineSetMap& spline_table, int index, double x, double& y) {

  // coefficients returned by spline lookup
  double coeff_set1[4]={0.,0.,0.,0.};
  double coeff_set2[4]={0.,0.,0.,0.};

  // get spline coefficients for interval containing x
  bool at_boundary=false;
  int status=get_coeff(spline_table,index,x,at_boundary,coeff_set1,coeff_set2);
  if (status != 0) return status;

  // evaluate spline for interval containing x.
  double out=coeff_set1[3]*x*x*x+coeff_set1[2]*x*x+coeff_set1[1]*x+coeff_set1[0];

  // If x is at an interval boundary, evaluate spline for the second 
  // interval containing x, and average the results of the two splines.
  if (at_boundary) {
    double out2=coeff_set2[3]*x*x*x+coeff_set2[2]*x*x+coeff_set2[1]*x+coeff_set2[0];
    out=0.5*(out+out2);
  }

  y=out;
  return 0;
}
// ---------------------------------------------------------------------------

}  // namespace spline

}  // namespace ahmission

/* Revision Log

 $Log: spline.cxx,v $
 Revision 1.4  2015/08/19 17:11:08  mwitthoe
 ahmission: update spline load function to iterate over all extensions with the same extension name to find the one that matches the given INSTRUME and DETNAME

 Revision 1.3  2015/03/18 17:34:56  asargent
 Changed DETNAME -> DETNAM

 Revision 1.2  2015/03/06 18:39:27  mwitthoe
 ahmission - spline: fix bug where an array was not allocated to the correct size

 Revision 1.1  2015/03/03 17:35:10  mwitthoe
 ahmission library: add general library for cubic spline FITS files


*/
