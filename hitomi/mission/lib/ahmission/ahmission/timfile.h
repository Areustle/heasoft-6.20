/// \file timfile.h
/// \brief read lookup tables from TIM file
/// \author Mike Witthoeft
/// \date $Date: 2014/09/10 02:30:35 $

/// \addtogroup mod_ahmission
/// \section lib_ahmission_timfile Read lookup tables from TIM file
///
/// This library will prepare two lookup tables between L32TI and TIME from
/// the given TIM file.  These lookup tables are used to assign TIME in the
/// ahtime and mxstime tools.  The first lookup table contains all rows from
/// the TIM file, including the rows with an illegal STATUS.  The second table
/// is the same as the first, but the illegal STATUS rows are excluded.  A
/// map array is also constructed which relates the array index of the first
/// table to the index in the second table.  A map value of -1 refers to a row
/// with an illegal STATUS and therefore omitted from the second lookup table.
///

#ifndef AHMISSION_TIMFILE_H
#define AHMISSION_TIMFILE_H

#include "ahgen/ahversion.h"
AHVERSION(AHMISSION_TIMFILE,"$Id: timfile.h,v 1.1 2014/09/10 02:30:35 mwitthoe Exp $")

#include <string>

/// \ingroup mod_ahmission
namespace ahmission {

namespace timfile {

/** \addtogroup mod_ahmission
 *  @{
 */

/// \brief number of bits in STATUS column (TIM file)
const int LENTIMSTATUS=10;

// The TIM file struct contains two lookup tables.  The first is the table 
// taken directly from the TIM file: TIME, L32TI, and STATUS.  The second
// table is the same as the first except the TIME=NULL rows have been removed.
// A map is present to get the second table's index from the first table's
// index; the map has a value of -1 for rows that have been removed.
struct TimFileData {

  // constructor
  TimFileData(): m_tstart(0.), m_tstop(0.), m_size1(0), m_time1(0), m_l32ti1(0),
                 m_status1(0), m_size2(0), m_time2(0), m_l32ti2(0), m_map(0) {}

  // destructor
  ~TimFileData() {
    if (m_time1 != 0) delete [] m_time1, m_time1=0;
    if (m_l32ti1 != 0) delete [] m_l32ti1, m_l32ti1=0;
    if (m_time2 != 0) delete [] m_time2, m_time2=0;
    if (m_l32ti2 != 0) delete [] m_l32ti2, m_l32ti2=0;
    if (m_map != 0) delete [] m_map, m_map=0;
    if (m_status1 != 0) {
      for (int ii=0; ii < m_size1; ii++) {
        if (m_status1[ii] != 0) delete [] m_status1[ii], m_status1[ii]=0;
      }
    }
  }

  // TSTART and TSTOP keywords
  double m_tstart;
  double m_tstop;

  // first lookup table (with TIME=NULL rows)
  long m_size1;
  double* m_time1;
  double* m_l32ti1;
  char** m_status1;

  // second lookup table (without TIME=NULL rows)
  long m_size2;
  double* m_time2;
  double* m_l32ti2;

  // mapping from table1 index to table2
  long* m_map;

};

/// \brief check if STATUS column from TIM file is marked as ILLEGAL
/// \param[in] status value from STATUS column of TIM file
/// \return true if column is ILLEGAL, false otherwise
bool isTimStatusIllegal(char* status);

/// \brief initialize TIM look up table 1
/// \param[in,out] timdat TimFileData instance
/// \param[in] size1 size of first lookup table (with TIME=NULL rows)
void initTimFileData1(TimFileData& timdat, long size1);

/// \brief initialize TIM look up table 2
/// \param[in,out] timdat TimFileData instance
/// \param[in] size2 size of second lookup table (without TIME=NULL rows)
void initTimFileData2(TimFileData& timdat, long size2);

/// \brief free all allocated memory in TimFileData instance
/// \param[in,out] timdat TimFileData instance
void cleanTimFileData(TimFileData& timdat);

/// \brief read lookup table from TIM file and populate both lookup tables
///  in TimFileData instance
/// \param[in] filename name of TIM file
/// \param[in,out] timdat TimFileData instance
void loadTimFile(const std::string& filename, TimFileData& timdat);

/** @} */

}  // namespace timfile

}  // namespace ahmission


#endif /* TOOL_AHMISSION_AHTIMFILE_H */

/* Revision Log
 $Log: timfile.h,v $
 Revision 1.1  2014/09/10 02:30:35  mwitthoe
 ahmission library: add timfile CALDB library (moved from ahtime library); remove leap second library (moved to ahtime library)

 Revision 1.3  2014/02/07 20:11:30  mwitthoe
 ahtime library: correct Doxygen module for timfile.h; update ahtime.dox

 Revision 1.2  2013/11/21 15:38:56  mwitthoe
 ahtime library: timfile: make a destructor for TIM data struct

 Revision 1.1  2013/11/20 23:03:49  mwitthoe
 ahtime library: add library for reading the TIM file needed for time assignment (ahtime and mxstime tools); remove obsolete testing code for old (ancient) TIM file library


*/
