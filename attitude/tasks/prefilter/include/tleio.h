/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/tleio.h,v $
 * $Revision: 1.2 $
 * $Date: 2002/11/26 20:13:20 $
 *
 * $Log: tleio.h,v $
 * Revision 1.2  2002/11/26 20:13:20  rwiegand
 * Corrected problem with duplicate final record in output.  Made TLE input
 * more robust.
 *
 * Revision 1.1  2002/02/20 14:39:17  miket
 * Merging Bob's original RCS files with HEAdas revisions
 *
 * Revision 1.2  2002/01/28 19:22:52  rwiegand
 * Updated TLE type names
 *
 * Revision 1.1  2001/11/06 16:30:56  rwiegand
 * Initial revision
 *
 */


#ifndef TLE_IO_H
#define TLE_IO_H


#define TLE_KEY_LENGTH 24
#define TLE_LINE_LENGTH 69

#define TLE_EXTNAME "TLE"


/* FITS table column indices */
enum
{
  TLE_COL_KEY = 1,
  TLE_COL_SATELLITE,
  TLE_COL_LINE_1,
  TLE_COL_LINE_2,

  TLE_COL_CLASSIFIED,
  TLE_COL_DESIGNATOR,
  TLE_COL_EPOCH,
  TLE_COL_MOTION2_DT,
  TLE_COL_MOTION6_D2T,
  TLE_COL_BSTAR,

  TLE_COL_INCLINATION,
  TLE_COL_RAAN,
  TLE_COL_ECCENTRICITY,
  TLE_COL_ARG_PERIGEE,
  TLE_COL_MEAN_ANOMALY,
  TLE_COL_MEAN_MOTION,
  TLE_COL_REVOLUTION,
  TLE_COL_DUMMY
};


typedef struct
{
  int ok;

  /* line 0 */
  char name0[TLE_KEY_LENGTH+1];
  char line1[TLE_LINE_LENGTH+1];
  char line2[TLE_LINE_LENGTH+1];

  /* line 1 */
  char number1;
  char satellite1[6];
  char classification;
  char designator[9];
  char epochyear[3];
  char epochdoy[4];
  char epochfrac[9];
  char motion2dt[11];
  char motion6d2t[9];
  char bstar[9];
  char type;
  char element[5];
  char checksum1;

  /* line 2 */
  char number2;
  char satellite2[6];
  char inclination[9];
  char raan[9];
  char eccentricity[8];
  char argperigee[9];
  char meananomaly[9];
  char meanmotion[12];
  char revolution[6];
  char checksum2;
} tleraw_t;


typedef struct
{
  int ok;

  /* line 0 */
  unsigned satellite;

  /* line 1 */
  int classified;
  double epoch;
  double motion2dt;
  double motion6d2t;
  double bstar;
  int type;
  int element;
  int checksum1;

  /* line 2 */
  double inclination;
  double raan;
  double eccentricity;
  double argperigee;
  double meananomaly;
  double meanmotion;
  unsigned revolution;
  int checksum2;
} tleconv_t;



int read_tle_raw(FILE *fp, tleraw_t *raw, int flags);
int tle_convert(const tleraw_t *raw, tleconv_t *conv, int flags);

int load_tle_from_filename(tlesgp_t *tle, const char *filename,
        const char *key, int flags);

#endif

