/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Loader.h,v $
 * $Revision: 1.5 $
 * $Date: 2005/10/10 12:14:25 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Loader.h,v $
 * Revision 1.5  2005/10/10 12:14:25  rwiegand
 * Allow reference to observation matches.
 *
 * Revision 1.4  2005/08/27 12:51:17  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/03 13:07:35  drhunter
 * Tristarid as of 9:00 AM, 8/3/05.
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#ifndef LOADER_H
#define LOADER_H

#include <stdio.h>

#include "List.h"


enum
{
		LOADER_OBSERVATION = 0x01,
		LOADER_DEGREES = 0x02,
		LOADER_FLAGS = 0xf
};


int loadSources (List * s, char * file, int flags);


#endif

