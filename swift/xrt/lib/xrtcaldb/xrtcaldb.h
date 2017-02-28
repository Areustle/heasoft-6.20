/*
 *	xrtcaldb.h: definitions and declarations with global relevance
 *
 *      CHANGE HISTORY:
 *       - 08/05/03 - Updated CALDB Data Types following "Description
 *                    of the XRT Calibration Files" Revision 0.1 
 *       - 09/07/03 - Added 'GRADE' and 'BPTABLE' bad pixels dataset
 *       - 30/09/04 - Changed KWVL_BPTABLE_DSET from "BPTABLE" to "BADPIX"
 *                  - added KWVL_BPTABLE_TYPE and KWVL_BADPIX_TYPE
 *       - 06/07/07 - Added 'ThresholdCol_t' and 'ThresholdInfo_t' struct
 *                  - Added 'ReadThrFile' function declaration
 *                  - Added 'KWVL_XRTVSUB_DSET'
 *       - 30/04/09 - Added 'PHASCongfCol_t' struct
 *                  - Added 'GetPhasConf' function declaration
 *                  - Added 'KWVL_PHASCONF_DSET'
 *       - 09/02/12 - Modified 'KWVL_GAIN_DSET' value
 * 
 *	AUTHOR:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
#ifndef XRTCALDB_H
#define XRTCALDB_H

#include "basic.h"
#include "cfortran.h"
#include "highfits.h"
#include "misc.h"
#include "calculus.h"
#include "xrt_termio.h"
#include "xrtfits.h"
#include "xrtdefs.h"

#include <pil.h>
#include <hdcal.h>

/*
#include "caldb.h"
*/

/*
#include "ftools.h"
*/

/*
 * Default parameter values for HDgtcalf() routine 
 */

#define HD_MAXRET        1
#define HD_DETNAM       "-"                      
#define HD_FILT         "-"                     
#define HD_EXPR         "-"

	/********************************/
	/*      defines / typedefs      */
	/********************************/

#define DF_CALDB                "CALDB"	     /* d/f for access method
                                               for cal-files	    	*/

#define DF_NOW                   "now"


        /***************************************/
	/*         DATASET CALDB  File         */
        /***************************************/


#define KWVL_BADPIX_DSET        "BADPIX"
#define KWVL_GAIN_DSET          "GAIN2"
#define KWVL_BIAS_DSET          "BIAS"
#define KWVL_TELDEF_DSET        "TELDEF"
#define KWVL_GRADE_DSET         "GRADES"
#define KWVL_BPTABLE_DSET       "BADPIX"
#define KWVL_MATRIX_DSET        "MATRIX"
#define KWVL_FILTER_DSET        "FTRANS"
#define KWVL_MIRROR_DSET        "EFFAREA"
#define KWVL_VIGNET_DSET        "VIGNET"
#define KWVL_PSF_DSET           "PSF"
#define KWVL_REGION_DSET        "REGION"
#define KWVL_COL_DSET           "WTCOLOFFSET"
#define KWVL_XRTVSUB_DSET       "XRTVSUB"
#define KWVL_BADPIX_TYPE        "ONGROUND"
#define KWVL_BPTABLE_TYPE       "ONBOARD"
#define KWVL_PHASCONF_DSET      "PHASCONF"

#define DATATYPELEN 20


typedef struct {
  unsigned Amp;
  unsigned GRADEID;
  unsigned GRADE;
} GradeCol_t;

typedef struct {
  double bias;
  double time;
} BiasRow_t;

typedef struct {
  unsigned TIME;
  unsigned BIAS;
} BiasCol_t;

typedef struct {
  unsigned SHAPE;
  unsigned DETX;
  unsigned DETY;
  unsigned R;
} CalSrcCol_t;

typedef struct {
    int detx;
    int dety;
    int r;
} CalSrcInfo_t;

typedef struct {
  unsigned XRTVSUB;
  unsigned PCEVENT;
  unsigned PCSPLIT;
  unsigned WTEVENT;
  unsigned WTSPLIT;
  unsigned PDEVENT;
  unsigned PDSPLIT;
  unsigned PCEVTTHR;
  unsigned PCSPLITTHR;
  unsigned WTBIASTH;
  unsigned PDBIASTH;
} ThresholdCol_t;

typedef struct {
    int xrtvsub;
    int pcevent;
    int pcsplit;
    int wtevent;
    int wtsplit;
    int pdevent;
    int pdsplit;
    int pcevtthr;
    int pcsplitthr;
    int wtbiasth;
    int pdbiasth;
} ThresholdInfo_t;

typedef struct {
  int wave;
  int amp;
  int offset;
  int wm1stcol;
  int wmcolnum;
} WTOffsetT_t;

typedef struct {
  unsigned WAVE;
  unsigned AMP;
  unsigned WM1STCOL;
  unsigned WMCOLNUM;
  unsigned OFFSET;

} WTColT_t;

typedef struct {
  unsigned TIME;
  unsigned PHASCONF;
} PHASCongfCol_t;


/* prototypes */
extern int CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd, char *TimeEnd,const char *DataSet, char *CalFileName, char *expr, long *extno);
extern int CalGetExtNumber(char * CalFileName, char *ExtName, long *extno);
int ReadCalSourcesFile(char * filename, CalSrcInfo_t *calsrc, int extno);
int ReadColFile(char *filename, FitsFileUnit_t, int nrows, WTOffsetT_t *wtoffset);
int ReadThrFile(char * filename, long extno, int xrtvsub, ThresholdInfo_t * thrvalue );
int GetPhasConf(char* filename, double time, int phasconf[9], int *stdconf);

#endif
