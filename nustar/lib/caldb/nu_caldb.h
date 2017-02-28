/*
 *	nu_caldb.h:
 *
 *	DESCRIPTION:
 *
 *        This module provides a collection of definitions and macros
 *        useful for CALDB Files handle. 
 *
 *      CHANGE HISTORY:
 *	 0.1.0: - NS 25/10/2010 - First version
 *	 0.1.1: - NS 11/02/2011 - 'KWVL_GAIN_DSET' added
 *	 0.1.2: - NS 10/03/2011 - 'KWVL_PIXPOS_DSET' added
 *	 0.1.3: - NS 26/04/2011 - 'KWVL_BADPIX_DSET' added
 *	 0.1.4: - NS 07/07/2011 - 'KWVL_PHAPAR_DSET' added
 *	 0.1.5: - NS 30/09/2011 - 'KWVL_ALIGN_DSET' added
 *	 0.1.6: - NS 18/11/2011 - 'KWVL_METROLOGY_DSET' added
 *	 0.1.7: - NS 13/12/2011 - 'KWVL_SPECRESP_DSET' 'KWVL_TVIGNET_DSET' and 'KWVL_REEF_DSET' added
 *	 0.1.8: - NS 20/01/2012 - 'KWVL_TELDEF_DSET' and 'KWVL_PSF_DSET' added
 *	 0.1.9: - NS 14/02/2012 - 'KWVL_CLC_DSET' and 'KWVL_CLCFILTER_DSET' added
 *	 0.2.0: - NS 24/02/2012 - 'KWVL_INSTRMAP_DSET' 'KWVL_DEPTHCUT_DSET' and 'KWVL_GRPRMF_DSET' added
 *	 0.2.1: - NS 12/07/2012 - 'KWVL_EVTCUT_DSET' added
 *	 0.2.2: - NS 02/10/2012 - 'KWVL_SAAPAR_DSET' added
 *	 0.2.3: - NS 24/10/2012 - 'KWVL_CHUOFFSET_DSET' added
 *	 0.2.4: - NS 13/02/2013 - 'KWVL_APERTURE_DSET' added
 *	 0.2.5: - NS 06/03/2013 - 'KWVL_GHOSTRAYS_DSET' added
 *	 0.2.6: - NS 12/04/2013 - 'KWVL_DETABS_DSET' added
 *	 0.2.7: - NS 10/07/2013 - 'KWVL_INSTRPROBMAP_DSET' added
 *	 0.2.8: - NS 17/07/2013 - 'KWVL_GRPPSF_DSET' added;
 *	                           Modified 'KWVL_PSF_DSET' value
 *                              
 * 
 *	AUTHOR:
 *
 *        ASDC - ASI Science Data Center
 */


#ifndef NU_CALDB_H
#define NU_CALDB_H


	/********************************/
	/*        header files          */
	/********************************/


/* headas header */
#include <pil.h>
#include <hdcal.h>
#include <headas_stdio.h>

/* highfits header */
#include "nu_basic.h"
#include "nu_defs.h"

/* misc header */
#include "nu_termio.h"




	/********************************/
	/*      defines / typedefs      */
	/********************************/

#define DF_CALDB                "CALDB"	     /* d/f for access method
                                               for caldb-files	    	*/

#define DF_NOW                   "now"


/*
 * Default parameter values for HDgtcalf() routine 
 */

#define HD_MAXRET        1
#define HD_DETNAM       "-"                      
#define HD_FILT         "-"                     
#define HD_EXPR         "-"



        /***************************************/
	/*         DATASET CALDB  File         */
        /***************************************/


#define KWVL_GRADE_DSET          "GRADES"
#define KWVL_CAP_OFFSET_DSET     "CAP_OFFSET"
#define KWVL_GAIN_DSET           "GAIN"
#define KWVL_PIXPOS_DSET         "PIXPOS"
#define KWVL_BADPIX_DSET         "BADPIX"
#define KWVL_PHAPAR_DSET         "PHAPAR"
#define KWVL_ALIGN_DSET          "ALIGNMENT"
#define KWVL_METROLOGY_DSET      "METROLOGY"
#define KWVL_SPECRESP_DSET       "SPECRESP"
#define KWVL_TVIGNET_DSET        "TVIGNET"
#define KWVL_REEF_DSET           "REEF"
#define KWVL_TELDEF_DSET         "TELDEF"
#define KWVL_PSF_DSET            "2D_PSF_E"
#define KWVL_CLC_DSET            "CLC"
#define KWVL_CLCFILTER_DSET      "CLCFILTER"
#define KWVL_INSTRMAP_DSET       "INSTRMAP"
#define KWVL_INSTRPROBMAP_DSET   "INSTRPROBMAP"
#define KWVL_DEPTHCUT_DSET       "DEPTHCUT"
#define KWVL_EVTCUT_DSET         "EVTCUT"
#define KWVL_GRPRMF_DSET         "GRPRMF"
#define KWVL_SAAPAR_DSET         "SAAPAR"
#define KWVL_CHUOFFSET_DSET      "CHUOFFSET"
#define KWVL_APERTURE_DSET       "APERTURE"
#define KWVL_GHOSTRAYS_DSET      "GHOSTRAYS"
#define KWVL_DETABS_DSET         "DETABS"
#define KWVL_GRPPSF_DSET         "GRPPSF"


	/********************************/
	/*          prototypes          */
	/********************************/


int CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd, char *TimeEnd,const char *DataSet, char *CalFileName, char *expr, long *extno, const char *instrument, const char *detnam);



#endif
