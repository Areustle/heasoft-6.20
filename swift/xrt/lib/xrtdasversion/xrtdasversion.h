/*
 *	xrtdasversion.h: --- version of SWXRTDAS/FTOOLS/HEADAS packages 
 *                           running ---
 *
 *      HISTORY OF CHANGES:
 *              06/03/2002 FT first version
 *
 *	AUTHOR:
 *	 	ISAC Italian Swift Archive Center
 *
 */

#ifndef XRTDASVERSION_H
#define XRTDASVERSION_H

		/********************************/
                /*        header files          */
                /********************************/
#include <stdio.h>
#include <string.h>
#include "basic.h"

#include "headas_stdio.h"
#include "misc.h"
		/********************************/
		/*      defines / typedefs      */
		/********************************/

typedef char Version_t[30];     

		/********************************/
		/*           globals            */
		/********************************/

		/********************************/
		/*      function prototypes     */
		/********************************/

extern void GetSWXRTDASVersion	(Version_t info);


extern void GetFTOOLSVersion	(Version_t info);

extern void GetHEADASVersion	(Version_t info);


#endif



