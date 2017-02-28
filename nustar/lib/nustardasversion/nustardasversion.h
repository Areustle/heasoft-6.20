/*
 *	nustardasversion.h: --- version of NuSTARDAS/FTOOLS packages 
 *                              running ---
 *
 *      HISTORY OF CHANGES:
 *              
 *
 *	AUTHOR:
 *	 	ASDC - ASI Science Data Center
 *
 */

#ifndef NUSTARDASVERSION_H
#define NUSTARDASVERSION_H

		/********************************/
                /*        header files          */
                /********************************/

#include <stdio.h>
#include <string.h>

/* headas header */
#include "headas_stdio.h"

/* highfits header */
#include "nu_basic.h"

/* misc header */
#include "nu_misc.h"

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

extern void GetNuSTARDASVersion	(Version_t info);

extern void GetFTOOLSVersion	(Version_t info);


#endif



