#ifndef _YOSHI_H_
#define _YOSHI_H_

#ifndef NULL
#include <stdio.h>
#endif

#ifndef _INT_TYPE_DEF_
#define _INT_TYPE_DEF_
typedef char Byte;			/* integer*1 */
typedef unsigned char uByte;
typedef short Short;			/* integer*2 */
typedef unsigned short uShort;
typedef int Long;			/* integer*4 */
typedef unsigned int uLong;
#endif

#ifndef _ENUM_DEF_
#define	_ENUM_DEF_
#ifdef TRUE
#undef TRUE
#endif
#ifdef FALSE
#undef FALSE
#endif
typedef enum { FALSE=0, TRUE=(!0) } BOOLEAN;
typedef enum { OFF=0, ON=1 } SWITCH;
typedef enum { GET=0, SET=1 } FUNC;
typedef enum { NO=0, YES=1 } YESNO;
#endif

#ifndef SEEK_SET
#define SEEK_SET	0
#define SEEK_CUR	1
#define SEEK_END	2
#endif

#define BS		8
#define TAB		9
#define CR		13
#define LF		10
#define ESCP		0x1b
#define SPC		0x20
#define CRLF		0x0a0d
#define MAXPATH		256

#define DBL		double
#define FLT		float
#define UNS		unsigned
#define until(a)	while(!(a))
#define unless(a)	if(!(a))
#define elif		else if
#define putln()		puts("")
#define fputln(fp)	fputs("\n",fp)
#define putstr(s)	fputs(s,stdout)
#define fputstr(s,fp)	fputs(s,fp)
#define putsln(s)	puts(s)
#define fputsln(msg,fp)	fprintf(fp,"%s\n",msg)
#define strtail(s)	(s+strlen(s))
#define null(p)		((p)==NULL)
#define getsnl(s)	gets(s)
#define getsln(s,n)	fgets(s,n,stdin)
#define fgetsln(s,n,fp)	fgets(s,n,fp)
#define squ(a)		((a)*(a))
#define existfile(fn)	(access(fn,0)==0)
#define stricmp(s1,s2)	strcasecmp(s1,s2)
/*
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;
*/
typedef struct wlist {
	struct wlist *next;
	struct wlist *prev;
	void *p;
} WLIST;

struct at_type {
	int dummy;
};

void errexit(char *msg);
void errexit2(char *msg, ...);
void perrexit(char *msg);
void perrexit2(char *msg, ...);
void *emalloc(unsigned size);
void *erealloc(void *p, unsigned size);
char *splitpath(char *fn);
char *splitext(char *fn);
/*FILE *opener(char *fn, char *mod);*/
void rebin(int np, double *p, int nq, double *q, double a, double b);
void irebin(int np, int *p, int nq, double *q, double a, double b);
void k_init(void);
int sensekey(void);
int inputkey(void);
int inkey(void);
int flushkey(void);
void delay(unsigned msec);
int selrecv(int sock, void *buf, unsigned len, int flags);
int ndelayrecv(int sock, void *buf, unsigned len, int flags);
char* strupper(char *d, char *s);
char* strlower(char *d, char *s);
int subspl(double x[/*n*/],double y[/*n*/],int n,double sp[/*n*/],double w[/*3n*/]);
double splint(double **sp,double x[/*n*/],double y[/*n*/],int n,double XX);
double spldif(double **sp, double x[/*n*/], double y[/*n*/], int n, double xx);
double splitg(double **sp,double x[/*n*/],double y[/*n*/],int n,double xs,double xe);
void ginkei(double alpha, double delta, double *l, double *b);
int read3d(FILE *fp, int *nxp, int *nyp, double **datap);
void allocate(double **data, unsigned num);
void allocate_clear(double **data, unsigned num);
void allocate_byte(char **data, unsigned num);
void allocate_byte_clear(char **Data, unsigned num);
void* mallocxy(unsigned siz, unsigned xx, unsigned yy);
void freexy(void*);
void* mallocxyz(unsigned siz, unsigned xx, unsigned yy, unsigned zz);
void freexyz(void*);
int ilog2(unsigned n);
#define isqrt(n)	((int)(sqrt((unsigned)n)+.5))
#define lsqrt(n)	((unsigned)(sqrt((unsigned)n)+.5))
double weightmean(double *x, double *sgm, unsigned n, double *gosa);
void pha_line_read(char buf[/*n*/], int n, FILE *fp);
double* pha_data_read(FILE *fp, int *phaid, char *title, int *ndata, double *integtime);
unsigned long* pha_ul_data_read(FILE *fp, int *phaid, char *title, int *ndata, double *integtime);
int pha_data_write(FILE *fp, char *title, int ndata, double integtime, double *dp);
int pha_ul_data_write(FILE *fp, char *title, int ndata, double integtime, unsigned long *dp);
FILE *opener(char *fn, char *mod);
void ntohl_array(Long *buf, unsigned n);
void ntohs_array(short *buf, unsigned n);
void htonl_array(Long *buf, unsigned n);
void htons_array(short *buf, unsigned n);
char* read_alloc_line(FILE *fp);
char* read_alloc_file(FILE *fp);
FILE* filter_open(char *command, FILE *fp);
int filter_go(char *command, FILE *fp, FILE *outfp);
char* strins(char *dst, char *src);
char* stredup(char *src);
char* strheadcmp(char *head, char *s);
int strcmpl(char *s1, ...);
int strISint(char *p);
void ERROR(char *format, ...);
double hostdouble(double);
double hostdouble_array(double array[], int n);
double netdouble(double);
double netdouble_array(double array[], int n);
double rnd1(void);
char* search_path(char *path, char *fn);
char* config_file(char *arg0, char *fn);
double gnuplot_calc(char *siki);
struct at_type* gnuplot_define_func(char *func);
double gnuplot_func(struct at_type *at_ptr, ...);
double gnuplot_ifunc(struct at_type *at_ptr, ...);
int gnuplot_cond(struct at_type *at_ptr, ...);
int gnuplot_icond(struct at_type *at_ptr, ...);
void fourn(float *data, int nn[], int ndim, int isign);
int imsmooth(int nx, int ny, float **img, float **mdl, float **smo);
float* imsmooth1(int nx, int ny, float **mdl);
int imsmooth2(int nx, int ny, float *mdlz, float **img, float **smo);
char* expand_file_name(char *fn);
unsigned long fsize(FILE *fp);
float** read_fl_fits_table(char *fn, int *r_nx, int *r_ny);
short** read_sh_fits_table(char *fn, int *r_nx, int *r_ny);
double parseRightAscension(char *expression);
double parseDeclination(char *expression);
void J2000toB1950(double lo20, double la20, double *lo50, double *la50);
void B1950toJ2000(double lo50, double la50, double *lo20, double *la20);

#if 0
BOOLEAN cdecl checkwild(char *fn);
BOOLEAN cdecl existfile(char *fn);
int cdecl str2int(char *str);
long cdecl str2long(char *str);
#endif
#endif	/* _YOSHI_H_ */
