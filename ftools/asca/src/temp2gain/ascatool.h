#include "faio.h"
#include "jcoord.h"
#include "ascatime.h"

#ifdef MJD_J2000	/* check atFunctions */

double parseRightAscension(char *expression);
double parseDeclination(char *expression);
void atDegToRA(double deg, AtRightAscension *ra);
void atDegToDec(double deg, AtDeclination *dec);

int atEarthElev(AtVect vSat,AtVect nvTgt,AtVect vSun,int *occlt,double elv[3]);

void atAberration(double mjd, AtVect x0, AtVect x);
void atInvAberration(double mjd, AtVect x0, AtVect x);
int atSISBrazil(double lon, double lat, int *saa);
int atSTTBrazil(double lon, double lat, int *saa);

int ascaSatPos(double mjd, AtVect vSat);
AtRotMat* ascaGetXRTrev(void);
double ascaRigidity(double mjd);
int ascaEulerAng(double ascatime, AtEulerAng *ea);
int ascaGeodcr(double mjd, double *height, double *longi, double *latti);
int ascaEarthOccult(double ascatime, int *occult, double *elev);
int ascaEarthElev(double ascatime, int *occult, double elev[3]);
int ascaBrazil(double mjd, int *saa);
int ascaSISBrazil(double mjd, int *saa);
int ascaSTTBrazil(double mjd, int *saa);
void ascaDayNight(double mjd, int *day_night, double *elev);
int ascaFov(AtEulerAng *ea, double *alpha, double *delta, double *roll);
int ascaGeomag(double mjd, AtVect field);

#endif	/* atFunctions */

int opFilenameGet(char *key1, char *key2, char *key3, char *filnam);

struct ascatool_frf_files {
	double starttime, endtime;
	char *fn;
	unsigned long size;
};

char* ascatool_splitpath(char *p);

char* ascatool_expand_file_name(char *fn);

int ascatool_cmp_frf_files(const void *p1, const void *p2);
double ascatool_frf2ascatime(char *fn);
int ascatool_frf_file_name(char *p, char ft_or_fa);
struct ascatool_frf_files *ascatool_read_frf_dir(char *frfpath, char ft_or_fa);
char* ascatool_read_region_file(char *filename);
#ifdef MJD_J2000	/* check atFunctions */
char* ascatool_scanYYMMDDHHMMSS(char *p, AtTime *ji);
#endif
char* ascatool_scanTimeOffset(char *p, double *sec);

void stderr_MSG(char *format, ...);
void stdout_MSG(char *format, ...);

double ascatool_hostdouble(double v);

#ifdef stdin
char* ascatool_read_alloc_line(FILE *fp);

char* ascatool_read_alloc_file(FILE *fp);

float** ascatool_read_fl_ascii_table(FILE *fp, int *r_nx, int *r_ny);
short** ascatool_read_sh_ascii_table(FILE *fp, int *r_nx, int *r_ny);
#endif

float** ascatool_read_fl_fits_table(char *fn, int *r_nx, int *r_ny);
short** ascatool_read_sh_fits_table(char *fn, int *r_nx, int *r_ny);

void* ascatool_mallocxy(unsigned siz, unsigned xx, unsigned yy);
void ascatool_freexy(void*);
void* ascatool_mallocxyz(unsigned siz, unsigned xx, unsigned yy, unsigned zz);
void ascatool_freexyz(void*);

double ascatool_rnd1(void);

char* ascatool_search_path(char *path, char *fn);

int sfAllcBuff(unsigned char **headerPtr, unsigned char **sfPtr);
int sfOpen(char *fn, char *header);
int sfClose(void);
int sfGetMJD(unsigned char (*sfp)[64][128], double *mjd);
int frameGetMJD(unsigned char (*frp)[128], double *mjd);
int sfGetASCA(unsigned char (*sfp)[64][128], double *ascatime);
int frameGetASCA(unsigned char (*frp)[128], double *ascatime);
#ifdef MJD_J2000	/* check atFunctions */
int sfGetAtTime(unsigned char (*sfp)[64][128], AtTime *ji);
int frameGetAtTime(unsigned char (*frp)[128], AtTime *ji);
#endif
int sfCheck(unsigned char (*sfp)[64][128]);

void atJ2000toB1950(double lo2000,double la2000,double *lo1950,double *la1950);
void atB1950toJ2000(double lo1950,double la1950,double *lo2000,double *la2000);
void atJ2000toGal(double ra, double dec, double *gl, double *gb);
void atGaltoJ2000(double gl, double gb, double *ra, double *dec);
void atGaltoB1950(double gl, double gb, double *ra, double *dec);
void atB1950toGal(double ra, double dec, double *gl, double *gb);

void whichopt(char frfname[], char frfdir[]);

void geocen(double *ascatime, double *ra_deg, double *dec_deg, double *asca_geotime, char *infile, int *status);

#ifdef __CFORTRAN_LOADED

#define ASCATOOL_GETLUN(A1)	CCALLSFSUB1(ASCATOOL_GETLUN,ascatool_getlun,PINT,A1)
#define ASCATOOL_FREELUN(A1)	CCALLSFSUB1(ASCATOOL_FREELUN,ascatool_freelun,PINT,A1)

#endif
