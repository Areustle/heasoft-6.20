/// \file sxsstatus.h
/// \brief Functions to read/write the SXS STATUS column
/// \author Mike Witthoeft
/// \date $Date: 2015/12/03 19:33:57 $

/// \addtogroup mod_ahsxs
/// \section ahsxs_sxsstatus SXS STATUS functions - sxsstatus
///
/// This library contains functions to set and check the bits of the SXS
/// STATUS column.  The STATUS column contains 16 bits (see the constant:
/// LEN_SXSSTATUS) with the definition below.  The Description defines the
/// state when the particular bit is set to 1.  All bits are set by the
/// sxsflagpix task.
///
///  Bit          Description
///   0            Event is contained within a general GTI
///   1            Event is contained within a pixel-specific GTI
///   2            Event is coincident with an antico event 
///   3            Event occurs near in time to another event
///   4            Event occurs near in time to another pixel 12 event 
///                 (recoil cross-talk)
///   5            Bit 4 is set and the recoil energy test is satisfied
///   6            Event occurs near in time to an event in an electrically-
///                 adjacent pixel
///   7            Within group of electrical cross-talk events, this event
///                 has the largest PHA
///   8            Event is contained within the direct MXS GTI
///   9            Event is contained in the afterglow region of a direct
///                 MXS GTI
///  10            Event is contained within the indirect MXS GTI
///  11            Event is contained in the afterglow region of an indirect
///                 MXS GTI
///  12            Same as bit 6, but using 2nd electrical cross-talk delta-time
///  13            Same as bit 7, but using 2nd electrical cross-talk delta-time
///  14            Undefined
///  15            Undefined
///

#ifndef AHSXS_SXSSTATUS_H
#define AHSXS_SXSSTATUS_H

#include "ahgen/ahversion.h"
AHVERSION(AHSXS_SXSSTATUS,"$Id: sxsstatus.h,v 1.3 2015/12/03 19:33:57 mwitthoe Exp $")

/// \ingroup mod_ahsxs

namespace ahsxs {

/** \addtogroup mod_ahsxs
 *  @{
 */

/// \brief data type of element in SXS STATUS column
typedef char type_status;

/// \brief number of bits in SXS STATUS column
const int LEN_SXSSTATUS=16;

/// \brief enumeration defining meaning for each STATUS bit.
enum StatusID {
  e_STATUS_GTI_GENERAL=0,           // event is in general GTI
  e_STATUS_GTI_PIXEL=1,             // event is in GTI for specific pixel
  e_STATUS_ANTICO=2,                // event is coincident with antico event
  e_STATUS_PROXIMITY=3,             // event is near to another event
  e_STATUS_NEAR_CALPIX=4,           // event is occurs near to calpix event
  e_STATUS_RECOIL=5,                // event flagged as recoil cross-talk
  e_STATUS_ELECTRICAL=6,            // event flagged as electrical cross-talk
  e_STATUS_ELEC_MAX=7,              // event has largest PHA of electrical cross-talk group
  e_STATUS_MXS_DIRECT=8,            // event is coincident with direct MXS GTI (1 of 2)
  e_STATUS_MXS_DIRECT_GLOW=9,       // event is coincident with direct MXS GTI (2 of 2)
  e_STATUS_MXS_INDIRECT=10,         // event is coincident with indirect MXS GTI (1 of 2)
  e_STATUS_MXS_INDIRECT_GLOW=11,    // event is coincident with indirect MXS GTI (2 of 2)
  e_STATUS_ELECTRICAL2=12,          // event flagged as electrical cross-talk with 2nd delta-time
  e_STATUS_ELEC_MAX2=13,            // event has largest PHA of electrical cross-talk group with 2nd delta-time
  e_STATUS_RESERVED_1=14,           // reserved bit (1 of 2)
  e_STATUS_RESERVED_2=15            // reserved bit (1 of 2)
};

/// \brief enumeration defining masks for STATUS states
enum StatusMask {
  e_MASK_GTI_GENERAL=0x8000,        // event is in general GTI
  e_MASK_GTI_PIXEL=0x4000,          // event is in GTI for specific pixel
  e_MASK_ANTICO=0x2000,             // event is coincident with antico event
  e_MASK_PROXIMITY=0x1000,          // event is near to another event
  e_MASK_NEAR_CALPIX=0x0800,        // event is occurs near to calpix event
  e_MASK_RECOIL=0x0400,             // event flagged as recoil cross-talk
  e_MASK_ELECTRICAL=0x0200,         // event flagged as electrical cross-talk
  e_MASK_ELEC_MAX=0x0100,           // event has largest PHA of electrical cross-talk group
  e_MASK_MXS_DIRECT=0x0080,         // event is coincident with direct MXS GTI (1 of 2)
  e_MASK_MXS_DIRECT_GLOW=0x0040,    // event is coincident with direct MXS GTI (2 of 2)
  e_MASK_MXS_INDIRECT=0x0020,       // event is coincident with indirect MXS GTI (1 of 2)
  e_MASK_MXS_INDIRECT_GLOW=0x0010,  // event is coincident with indirect MXS GTI (2 of 2)
  e_MASK_ELECTRICAL2=0x0008,        // event flagged as electrical cross-talk with 2nd delta-time
  e_MASK_ELEC_MAX2=0x0004,          // event has largest PHA of electrical cross-talk group with 2nd delta-time
  e_MASK_RESERVED_1=0x0002,         // reserved bit (1 of 2)
  e_MASK_RESERVED_2=0x0001          // reserved bit (2 of 2)
};

/// \brief set all status bits to zero
/// \param[out] status   status column value to be updated
void clear_status(type_status* status);

// --- Set individual bits ---

/// \brief sets the general gti portion of the status column         
/// \param[in] stat      stat is true if outside the general good time interval 
/// \param[out] status   status column value to be updated
void set_status_general_gti(bool stat, type_status* status);

/// \brief sets the pixel gti portion of the status column
/// \param[in] stat     stat is true if outside pixel good time interval
/// \param[out] status  status column value to be updated
void set_status_pixel_gti(bool stat, type_status* status);

/// \brief set antico bit of status
/// \param[in] stat true if flagging antico
/// \param[in,out] status status column value to be updated
void set_status_antico(bool stat, type_status* status);

/// \brief set proximity portion of status
/// \param[in] intime true if delta-t satisfied
/// \param[in,out] status status column value to be updated
void set_status_prox(bool intime, type_status* status);

/// \brief set recoil cross talk portion of status
/// \param[in] is12 true if delta-t satisfied and other pixel has index 12
/// \param[in] eOK true if energy condition satisfied
/// \param[in,out] status status column value to be updated
void set_status_recoil(bool is12, bool eOK, type_status* status);

/// \brief set electrical cross talk portion of status
/// \param[in] intime true if delta-t satisfied
/// \param[in] prime true if pixel in group with largest PHA
/// \param[in,out] status status column value to be updated
void set_status_electrical(bool intime, bool prime, type_status* status);

/// \brief set direct or indirect MXS portion of status
/// \param[in] isOn true if MXS on
/// \param[in] glow true if afterglow (ignored if isOn=true)
/// \param[in] isdirect true=>direct; false=>indirect
/// \param[in,out] status status column value to be updated
void set_status_mxs(bool isOn, bool glow, bool isdirect, type_status* status);

/// \brief set electrical cross talk portion of status with 2nd delta-time
/// \param[in] intime true if delta-t satisfied
/// \param[in] prime true if pixel in group with largest PHA
/// \param[in,out] status status column value to be updated
void set_status_electrical2(bool intime, bool prime, type_status* status);



// --- Check individual bits ---

/// \brief return true if status is flagged for general GTI
/// \param[in] status status column value to check
bool has_status_general_gti(type_status* status);

/// \brief return true if status is flagged for pixel GTI
/// \param[in] status status column value to check
bool has_status_pixel_gti(type_status* status);

/// \brief return true if status is flagged for antico
/// \param[in] status status column value to check
bool has_status_antico(type_status* status);

/// \brief return true if status is flagged for proximity
/// \param[in] status status column value to check
bool has_status_prox(type_status* status);


// --- Check multiple bits ---

/// \brief return true if status is flagged for general or pixel GTI
/// \param[in] status status column value to check
bool has_status_gti(type_status* status);

/// \brief return true if status is flagged for any crosstalk
/// \param[in] status status column value to check
bool has_status_crosstalk(type_status* status);

/// \brief return true if status is flagged for recoil cross-talk
/// \param[in] status status column value to check
bool has_status_recoil_crosstalk(type_status* status);

/// \brief return true if status is flagged for electrical cross-talk where
///  the event is the source of the cross-talk (still good event)
/// \param[in] status status column value to check
bool has_status_electrical_crosstalk_source(type_status* status);

/// \brief return true if status is flagged for electrical cross-talk where
///  the event is the induced residual (not a real event)
/// \param[in] status status column value to check
bool has_status_electrical_crosstalk_residual(type_status* status);

/// \brief return true if status is flagged for direct or indirect MXS
/// \param[in] status status column value to check
/// \param[in] afterglow true to include afterglow region in check
bool has_status_mxs(type_status* status, bool afterglow);

/// \brief return true if status is flagged for electrical cross-talk with the
///  2nd delta-time where the event is the source of the cross-talk (still good
///  event)
/// \param[in] status status column value to check
bool has_status_electrical_crosstalk_source2(type_status* status);

/// \brief return true if status is flagged for electrical cross-talk with the
///  2nd delta-time where the event is the induced residual (not a real event)
/// \param[in] status status column value to check
bool has_status_electrical_crosstalk_residual2(type_status* status);



/** @} */

}  // namespace ahsxs

#endif /* AHSXS_SXSSTATUS */

/* Revision Log

 $Log: sxsstatus.h,v $
 Revision 1.3  2015/12/03 19:33:57  mwitthoe
 ahsxs library: support CTEL2DT column in delta-times CALDB file; assign 2 bits in STATUS for a 2nd electrical cross-talk flagging

 Revision 1.2  2015/09/17 19:55:26  mwitthoe
 ahsxs: add functions to check SXS STATUS columns for recoil and electrical cross-talk separately; this is needed for sxssecid (see issue 554)

 Revision 1.1  2014/11/18 22:14:44  mwitthoe
 ahsxs: add library to set/check SXS STATUS column bits; these functions will be used by the sxsflagpix and sxsdrift tasks; see issue 445


*/
