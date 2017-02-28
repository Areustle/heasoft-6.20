/// \file ahfits.h
/// \brief Wrapper to cfitsio
/// \author James Peachey
/// \date $Date: 2014/01/06 16:29:28 $

#ifndef AHFITS_AHFITS_H
#define AHFITS_AHFITS_H

#include "ahgen/ahversion.h"
AHVERSION(AHFITS_AHFITS,"$Id: ahfits.h,v 1.23 2014/01/06 16:29:28 asargent Exp $")

#include "ahfits/ahfits_base.h"
#include "ahfits/ahfits_header.h"
#include "ahfits/ahfits_colinfo.h"
#include "ahfits/ahfits_router.h"
#include "ahfits/ahfits_connect.h"
#include "ahfits/ahfits_buffer.h"
#include "ahfits/ahfits_row.h"
#include "ahfits/ahfits_file.h"
#include "ahfits/ahfits_file_list.h"
#include "ahfits/ahfits_image.h"

#endif   /* AHFITS_AHFITS_H */

/* Revision Log
   $Log: ahfits.h,v $
   Revision 1.23  2014/01/06 16:29:28  asargent
   Added new function HDUExists to check whether the HDU exists or not

   Revision 1.22  2013/07/18 15:54:35  mwitthoe
   ahfits: add ahfits_buffer.h to ahfits.h; the 2nd argument of the constructor had a default value which was messing up the building of tools (I do not know why), changed to an overloaded constructor for now

   Revision 1.21  2012/11/29 03:03:58  peachey
   Include file list facilities in main header.

   Revision 1.20  2012/10/16 22:56:52  mwitthoe
   removed old ahfits2.h and fixed define labels in ahfits.h

   Revision 1.19  2012/10/01 17:21:56  mwitthoe
   new version of ahfits with connections stored outside of FITS file object

   Revision 1.1  2012/09/27 18:13:17  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/
