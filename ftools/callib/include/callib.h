/* 
   callib.h
       Head file for the C wrapped fortran routines in callib 
*/ 
#include "fitsio.h"

#ifndef CALLIBHEADER
#define CALLIBHEADER
void FindExtName(fitsfile *fptr, char *instr, int nsearch, 
                  int *nfound, int *next, char outhdu[][9][21], 
                char outver[9][21], char extname[][41],  int chatter,int* ierr); 
void FindHDUClass (fitsfile *fptr, int ninstr, char instr[][21], int 
               nsearch, int *nfound,  int *next, 
               char outhdu[][9][21], char outver[9][21], char extname[][41], 
               int chatter,  int* ierr); 
void GetKeys(fitsfile *fptr, int *nkeys, char keys[][9], 
             int chatter, int *ierr); 
void MoveExt(fitsfile *fptr, int extnum,  int ninstr, char instr[][21],
         char* extname, int nsearch, int* next,
        char outhdu[][9][21], char outver[9][21],
        char extnames[][41], int chatter, int * errflag);

/* GTI FILES */
void ReadGTI(fitsfile *fptr, int *n_gti, int max_gti, float *start,
    float *stop, int chatter,int *ierr); 

/* ARF FILES */
void ReadARF(fitsfile *fptr, char telescop[21], char instrume[21], 
    char detnam[21], char filter[21], int iebound, float *e_lo, 
    float *e_hi,  float *sprsp, char arfversn[6], int chatter, int *ierr);


/* EBD FILES */
void ReadEBD(fitsfile *fptr,  char telescop[21], 
             char instrume[21], char detnam[21], char filter[21],
             float *arescal, char chantype[21], int *flchan,
             int maxchan, int* iebound, int *channel, 
             float *e_min,  float *e_max, char arfversn[6], 
             int chatter, int *ierr);

void WriteEBD(fitsfile *fptr, int nk_hist, char hist[][FLEN_COMMENT],
    int nk_comm, char comment[][FLEN_COMMENT], char *rmfversn,
    char *telescop, char *instrume, char *detnam, char *filter,
    float areascal, char *chantype, int fchan, int iebound,
    float *e_min, float *e_max, int chatter, int *ierr);


/* RPF FILES */
void ReadRPRF(fitsfile *fptr,
            int *nrad, float* rad_lo, float* rad_hi,  char* radunit,
            int *ntheta, float* theta_lo, float* theta_hi, char* thetaunit,
            int *nenerg, float* energ_lo, float* energ_hi, char* energunit,
            float *rpsf, int *qerror, float *rpsf_err,
            char* rpsfunit, int *qarea, float *area_wgt,
            char* telescop, char* instrume,
            int maxrad, int maxtheta, char* hduclass,
            int chatter, int *ierr);

void WriteRPRF(fitsfile *fptr, char *extname,
             int nrad,float* rad_lo,float* rad_hi,
             char *radunit, int ntheta,float* theta_lo,
             float* theta_hi,char *thetaunit,
             int nenerg,float* energ_lo,float* energ_hi,
             char *energunit, float* rpsf,int* qerror,
             float* rpsf_err,char *rpsfunit,int *qarea,
             float* area_wgt, int nk_list, char hist[][81],
             int nk_comm, char comms[][81],
             char *telescop,char *instrume,char *hduclas3,
             int maxrad, int maxtheta, int chatter, int *ierr);

/* RMF FILES */
void ReadRMF(fitsfile *fptr, int qorder, int maxen, int maxgrp, 
             int maxelt, char *rmfversn, char* hduclas3,
             char *telescop, char *instrume, char *detnam, char *filter,
         float *areascal, char *chantype, int *flchan, int *numchn,
         int *nenerg, int *numgrp, int *numelt,
         float *energ_lo, float *energ_hi,
         int *ngrp, int *f_chan, int *n_chan, int *isorder, int* order,
         float *fmatrix, float *lo_thresh, int chatter, int *ierr); 


void WriteRMF(fitsfile *fptr, int nk_hist, char *hist,
         int nk_comm, char *comment,
         char *rmfversn, char *hduclas3, char *telescop,
         char *instrume, char *detnam, char *filter,
         float areascal, char *chantype, int flchan, int numelt,
         int nchan, int nenerg, int numgrp,
         float *energ_lo, float *energ_hi,
         int *ngrp, int *f_chan, int *n_chan, int qorder, int *order,
         float *fmatrix, float lo_thresh, int chatter, int *ierr);



void GetRMFGrids(fitsfile *fptr, int ienerg, float *en_lo, float *en_hi,  
                 int chatter, int *ierr); 


/* REEF FILES */  
void ReadREEF(fitsfile *fptr, int* nrad,float *rad_lo,float *rad_hi,
           char* radunit, int *ntheta,float *theta_lo,float *theta_hi,
           char* thetaunit, int *nenerg,float *energ_lo,float *energ_hi,
           char* energunit,float *reef, int *qerror,float *reef_err,
           char* reefunit, int *qarea,float *area_wgt,char* telescop,
           char* instrume, char* hduclas3, int maxrad, int maxtheta,
           int chatter,  int *ierr);


 void WriteREEF(fitsfile *fptr, char *extname, int nrad,float *rad_lo,
        float *rad_hi,
        char* radunit, int ntheta,float *theta_lo,float *theta_hi,
        char* thetaunit, int nenerg,float *energ_lo,float *energ_hi,
        char* energunit,float *reef, int *qerror,float *reef_err,
        char* reefunit, int *qarea,float *area_wgt,char* telescop,
        char* instrume, char* hduclas3, char *hduclas4,
        int nk_hist, char hist[][81], int nk_comm, char comm[][81],
        int maxrad, int maxtheta, int chatter,  int *ierr);

/* Observation and direction keywords */
void ReadOBSDateTime(fitsfile *fptr, char* date_obs,
     char* time_obs,
     char* date_end, char* time_end,
     double *mjdobs, int chatter,
     int *ierr);

void WriteObjRadDec(fitsfile *fptr, int qsys, char radcyss[71] , float equinox,
    float ra_obj, float dec_obj, float ra_obje, float dec_obje, 
    int chatter, int *ierr);

void WritePntDirRadDec(fitsfile *fptr, int qsys, char radecsys[71],
float equinox, float ra_pnt, float dec_pnt, float pa_pnt,
float ra_pnte, float dec_pnte,  float pa_pnte, int chatter, int *ierr);


/* CALDB access routines */
void GetCALDBFiles( char *tele_str, char *instr_str, char *detnam_str, 
 char *filt_str,  char *codenam_str,  char *strtdate,  char *strtime, 
 char *stpdate,  char *stptime, 
 char *expr_str, int maxret, char filenam[][FLEN_FILENAME],
 int *extno,  char online[][21], int *nret, int *nfound, 
 int chatter, int *status);  

/* Misc. */
void RemapOneDim(int nin, float *xin_lo, float *xin_hi, float *y_in,
                 int nout, float *xout_lo, float *xout_hi, float *y_out,
                 int mode, float acc, int chatter,int *ierr); 

#endif
