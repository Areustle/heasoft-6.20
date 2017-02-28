#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "fitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include "xpi.h"


#define  PSF_68(A)	((A == 0) ? 1 : .1021*pow(A/100, -0.534))

#define  SECDAY		86400			/* Seconds per day */

#define  CREATOR        "BARYTIME v3.0"


enum file_types {SMDB, SMDB_FITS, PULSAR_FITS};

typedef struct {
  long *mills;               /* Milliseconds since UTC midnight                 */
  short *mics;               /* Microseconds since last ms                      */
  short *jdtrun;             /* Truncated Julian Day of arrival                 */
  float **scposn;            /* Geocenter-to-spacecraft vector (km)             */
  long  *tags;               /* B and C plane tile tags                         */
  short *coincidence;        /* Coincidence dir/type mode                       */
  unsigned char *flags;      /* Packet error flags                              */
  unsigned char **tasc;      /* TASC #1 and #2 status flags                     */
  float *XZ_proj, *YZ_proj;  /* Gamma ray angle in X-Z and Y-Z planes           */
  float *zenith, *azimuth;   /* Gamma ray Earth coordinates (rad)               */
  float *grra, *grdec;       /* Gamma ray right ascension, declination (rad)    */
  float *grlat, *grlong;     /* Gamma ray galactic coordinates (rad)            */
  float *energy, *delta_e;   /* Gamma ray energy and uncertainty (MeV)          */
  double *frdet;             /* Day fraction that photon would reach SSBC       */
  double *time;              /* TIME=2440001 + TJD + FRDET                      */
  float *binphase, *phase;   /* Binary orbital phase, pulsar rotation phase     */
  float *pra, *pdec;         /* Pulsar right ascension, declination (rad)       */
  long  **bcvec;             /* Barycenter vector (light-microseconds)          */
  char  *good;               /* Indicates whether a row in the record is good   */
} Record;

typedef struct {
  int month, day, year, hour, minute;
  double seconds;
} Date;

int Arrtim(char *misc_dir, char *input_file, fitsfile *ofptr);
void Barytime();
void CalcObstime();
void CelGal(double in_ra, double in_dec, double *glong, double *glat);
int Delay(char *misc_dir, int jdutc, double frc, Record *record, int irec);
void ErrorHandler(char *msg);
long ExtractGoodRecords(long nrows, Record *record);
void GalCel();
void GetCoord();
void GetDates(Date date, double *yydd, double *seconds, char *ddmmyy, char *hhmmss);
void GetExcludeRange(double *t1, double *t2);
int GetInput(double *consiz, double *mincon, double *angmin, double *angmax,
	     double *emin, double *emax, double *zenoff, double *zenmax,
	     char *coord, char *rastring, char *decstring);
void GetLimit(double file_value, double *value, int type);
int GetParameters(char *input_file, char *printfile, fitsfile *ofptr, char *angsel, 
		  char *zensel, int file_type);
void GetPositions(double frc);
void InitGlobalVariables(char *misc_dir);
void InitVar();
double Julian(Date date);
int LoadEphemData(char *misc_dir, int jdutc);
int OpenInfile(char *misc_dir, char *output_dir, char *filename, char *output_file, 
	       char *printfile, char *angsel, char *zensel, int *file_type, 
	       int *clobber);
int OpenNewfile(char *output_dir, char *filename, fitsfile **ofptr, int clobber);
int PrintHeader(char *input_file, char *printfile);
int ReadInfile(Record *record, int irec);
void SaveRecord(Record *record, int irec);
int ScanHeader(char *input_file, fitsfile **fptr);
int SetDefaults(char *input_file, char *angsel, char *zensel);
int TimeKeywds(fitsfile *fptr, char *extname);
int WriteHeader(fitsfile *fptr, fitsfile *ofptr, int file_type);
int WriteRecords(fitsfile *ofptr, long nrows, Record *record);















