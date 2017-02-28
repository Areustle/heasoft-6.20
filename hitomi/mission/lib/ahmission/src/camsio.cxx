/// \brief camsio.cxx: Defines functions for the UnitDataRow structure.
/// \author Timothy Reichard
/// \date $Date: 2015/07/31 20:52:18 $
///
/// Defines constructors and output functions
/// for the CAMSTelDef structure.

#define AHLABEL AHMISSION_CAMSIO
#define AHCVSID "$Id: camsio.cxx,v 1.3 2015/07/31 20:52:18 rshill Exp $"

#include "ahmission/camsio.h"

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include <strings.h>

namespace ahmission {

namespace camsio {

// ================================================================================

ahfits::Router* UnitDataRow::connectColumns(ahfits::FilePtr& fp,  const ahfits::RWModeEnum mode)
{
  ahfits::Router* p_rt = new ahfits::Router(fp); 
  p_rt->connectScalar(mode, "TIME", m_t);
  p_rt->connectScalar(mode, "X_RAW", m_x_raw);
  p_rt->connectScalar(mode, "Y_RAW", m_y_raw);
  p_rt->connectScalar(mode, "X", m_x);
  p_rt->connectScalar(mode, "Y", m_y);
  p_rt->connectScalar(mode, "QUALITY", m_q);
  p_rt->connectBit(mode, "PROC_STATUS", m_proc_status, m_num_proc_status);

  return p_rt;
}

} //end namespace camsio

} //end namespace ahmission

// ================================================================================

/* Revision Log
   $Log: camsio.cxx,v $
   Revision 1.3  2015/07/31 20:52:18  rshill
   Include temperature-corrected CAMS data columns.

   Revision 1.2  2015/07/31 18:36:53  rshill
   Add support for proc_status column.

   Revision 1.1  2014/01/07 19:38:30  treichar
   Moved TelDef and CAMS unit data file I/O data structures and routines from cams2det task to ahmission library.


*/
