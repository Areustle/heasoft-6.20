/* General.h */


#if !defined(GENERAL_DEFS)
#define GENERAL_DEFS


#include "constdef.h"
#include "fitsio.h"

#define Min(a, b) (a < b ? a : b)
#define Max(a, b) (a > b ? a : b)
#define Abs(a) (a >= 0 ? a : -a);


void CheckFile(char *filename, int chatter, int clobber);

int CreateEboundsExt1(fitsfile *fptr, int chatter, int nk_history, 
		      char **history, int nk_comm, char **comment, 
		      char *rmfversn, char *telescop, char *instrume, 
		      char *detnam, char *filter, float areascal, 
		      char *chantype, int fchan, int iebound, float *e_min, 
		      float *e_max);

int CreateNewFits(fitsfile **infp1, char *filename, int simple, int bitpix, 
		  int naxis, long *naxes, long pcount, long gcount, int extend, 
                  int chatter, int clobber, int *status);

void DispMsg(int chatter, int wtchatter, char *message);

void Printerror(int status);

void PrintWarning(char *program, char *version, char *message, int chatter, 
		  int wtChatter, int status);

int ReadTypeIPha(fitsfile *fptr, int chatter, char *telescop, char *instrume, 
		 char *detnam, char *filter, char *phaversn, char *hduclas2, 
		 long *fchan, double *texpos, float *areascal, char *backfil, 
		 float *backscal, char *corrfil, float *corrscal, char *rmffil, 
		 char *arffil, int *nXflt, char **xflt, char *dmode, 
		 long *detchans, char *chantyp, short **channel, long **ipha, 
		 float **pha, int *dtype, int *qerror, float **error, int *qsys,
		 float **sysfrc, int *qqual, short **quality, int *qgroup, 
		 short **grping, int *pois, long *nchan);

int ReadTypeIIPha(fitsfile *fptr, int chatter, char *telescop, char *instrume, 
		  char *detnam, char *filter, char *phaversn, char *hduclass, 
		  char *hduclas1, char *hduclas2, char *hduclas3, 
		  char *hduclas4, char *hduvers1, long *fchan, double *texpos, 
		  float *areascal, int *nbackfil, char ***backfil, 
		  float **backscal, int *ncorrfil, char ***corrfil, 
		  float **corrscal, int *nrespfil, char ***respfil, 
		  int *nancrfil, char ***ancrfil, int *nXflt, char **xflt, 
		  char *dmode, long *detchans, char *chantyp, short **spec_num,
		  short **channel, long **ipha, float **pha, char ***rowid, 
		  int *dtype, int *qerror, float **serr, int *qsys, 
		  float **syserr, int *qqual, short **quality, int *qgroup, 
		  short **grping, int *pois, long *nspec);

int WriteArf1(fitsfile *fptr, int chatter, int nk_history, char **history, 
	      int nk_comm, char **comment, char *arfversn, char *phafil, 
	      char *telescop, char *instrume, char *detnam, char *filter, 
	      int ienerg, float *energ_lo, float *energ_hi, float *sprsp);

int WriteRmf1(fitsfile *fptr, int chatter, int nk_history, char **history, 
	      int nk_comm, char **comment, char *rmfversn, char *hduclas3, 
	      char *telescop, char *instrume, char *detnam, char *filter, 
	      float areascal, char *chantype, int flchan, int ichan, int ienerg,
	      float *energ_lo, float *energ_hi, int *ngrp, int **f_chan, 
	      int **n_chan, float **fmatrix, float lo_thresh);

int WriteTypeIPha(fitsfile *fptr, int chatter, int nk_history, char **history, 
		  int nk_comm, char **comment, char *telescop, char *instrume, 
		  char *detnam, char *filter, char *phaversn, char *hduclas2, 
		  long fchan, double texpos, float areascal, char *backfil, 
		  float backscal, char *corrfil, float corrscal, char *respfil, 
		  char *ancrfil, long detchans, char *chantyp, short *channel, 
		  float *counts, int dtype, int qerror, float *serr, int qsys, 
		  float *syserr, int qqual, short *quality, int qgroup, 
		  short *grping, long nchan);

int WriteTypeIIPha(fitsfile *fptr, int chatter, int nk_history, char **history, 
		   int nk_comm, char **comment, char *telescop, char *instrume, 
		   char *detnam, char *filter, char *phaversn, char *hduclas2, 
		   long fchan, double texpos, float areascal, int nbackfil, 
		   char **backfil, float *backscal, int ncorrfil,
		   char **corrfil, float *corrscal, int nrespfil, 
		   char **respfil, int nancrfil, char **ancrfil, int ntemplfil,
		   char **templfil, long detchans, char *chantyp, 
		   short **channel, float **counts, int dtype, int qerror, 
		   float **serr, int qsys, float **syserr, int qqual,
		   short **quality, int qgroup, short **grping, int nspec, 
		   long nchan);

void XTE_Fcecho(char *outstr);




#endif
