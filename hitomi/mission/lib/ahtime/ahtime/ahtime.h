/// \file ahtime.h
/// \brief Astro-H time assignment library
/// \author Mike Witthoeft
/// \date $Date: 2015/10/02 20:35:54 $

#ifndef AHTIME_AHTIME_H
#define AHTIME_AHTIME_H

#include "ahgen/ahversion.h"
AHVERSION(AHTIME_AHTIME,"$Id: ahtime.h,v 1.15 2015/10/02 20:35:54 mwitthoe Exp $")

#include "ahtime/leapsec.h"        // read/store leap second table from CALDB file
        
#include "ahtime/ahtime_base.h"    // general time constants and functions
#include "ahtime/ahtimeconv.h"     // time conversion functions

#include "ahtime/AhDateTime.h"
#include "ahtime/AhMJDTime.h"

#endif   /* AHTIME_AHTIME_H */

/* Revision Log
   $Log: ahtime.h,v $
   Revision 1.15  2015/10/02 20:35:54  mwitthoe
   ahtime library: remove frqtemp include statement from header file

   Revision 1.14  2015/07/06 03:58:29  klrutkow
   added include for new file to resolve freq vs time CALDB filename

   Revision 1.13  2014/09/10 02:41:18  mwitthoe
   ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv

   Revision 1.12  2013/11/26 18:18:09  mwitthoe
   ahtime library: timfile.h was not being included properly in ahtiahtime.h (discovered by Andy: see issue 291); corrected

   Revision 1.11  2013/11/20 23:03:49  mwitthoe
   ahtime library: add library for reading the TIM file needed for time assignment (ahtime and mxstime tools); remove obsolete testing code for old (ancient) TIM file library

   Revision 1.10  2013/10/16 02:19:05  mwitthoe
   ahtime library: remove include statments for obsolete AhTimeUTC, AhTimeTT, and AhTimeTAI in ahtime.h

   Revision 1.9  2013/09/17 19:28:05  mwitthoe
   ahtime library: change how time conversion/reformatting is done between time systems and formats (see issue 290 for details); the old method is still intact, but can be removed once all tools/libraries have been switched over to the new method

   Revision 1.8  2013/04/08 21:24:25  mwitthoe
   ahtime library: remove ahlookup and ahtimeassign from repository

   Revision 1.7  2013/04/04 20:47:30  mwitthoe
   remove references to ahlookup and ahtimeassign in ahtime library; both of these are now implemented directly in the ahtime tool source

   Revision 1.6  2012/11/15 03:19:51  mwitthoe
   remove old CALDB libraries from ahtime.h in the ahtime library

   Revision 1.5  2012/11/15 02:54:05  mwitthoe
   change ahtime library to use new ahcaldb library

   Revision 1.4  2012/10/31 18:12:58  mwitthoe
   add ahlookup library to ahtime library

   Revision 1.3  2012/10/25 01:38:00  mwitthoe
   remove ahtimfile library from ahtime; its functions have been generalized and put into the ahlookup library in ahmath

   Revision 1.2  2012/10/24 18:57:52  mwitthoe
   add include for ahfreqtemp in ahtime.h

   Revision 1.1  2012/10/18 17:36:07  mwitthoe
   add new version of ahtime.h


*/
