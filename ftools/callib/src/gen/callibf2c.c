#include "fitsio.h"
#include "callib_wrap.h"
#include "callib.h"

void FindExtName(fitsfile *fptr, char *instr, int                
               nsearch,  int* nfound, int *next, 
               char outhdu[][9][21], char outver[9][21], 
               char extnam[][41], int chatter, int * ierr)
{ 
    int iunit; 
    iunit = CFITS2Unit(fptr);
    cwcal_FindExtName(chatter, iunit, instr, nsearch, *nfound, 
        next, outhdu, outver,extnam,*ierr);
    return;
}



void FindHDUClass (fitsfile *fptr, int ninstr, char instr[][21], int            
               nsearch, int *nfound,  int *next,
               char outhdu[][9][21], char outver[9][21], 
               char extnam[][41], int chatter, int * ierr)
{
    int iunit;
    iunit = CFITS2Unit(fptr);
    cwcal_FindHduClass(chatter, iunit, ninstr, instr, nsearch, *nfound, 
        next, outhdu, outver,extnam,*ierr);
    return;
}

    

void GetKeys(fitsfile *fptr, int *nkeys, 
        char keys[][9], int chatter, int *ierr)
{ 
    int iunit;
    iunit = CFITS2Unit(fptr);
    cwcal_GetKeys(iunit, *nkeys, keys, chatter, *ierr);
    return;
} 

void MoveExt(fitsfile *fptr, int extnum,  int ninstr, char instr[][21], 
	 char* extname, int nsearch, int *next, 
        char outhdu[][9][21], char outver[9][21], 
        char extnames[][41], int chatter, int * errflg)
{ 
    int iunit;
    iunit = CFITS2Unit(fptr);
    cwcal_MoveExt(iunit, extnum, ninstr, instr, nsearch, 
    next, outhdu, extnames, outver, extname, *errflg, chatter );  
    return;
} 

void GetRMFGrids(fitsfile *fptr, int ienerg, float *en_lo, float *en_hi,  
		 int chatter, int *ierr)
{ 
    int iunit;
    iunit = CFITS2Unit(fptr);
    cwcal_GetRMFGrids(chatter,iunit,ienerg,en_lo,en_hi,*ierr);
    return;
} 

void ReadARF(fitsfile *fptr, char telescop[21], char
instrume[21], char detnam[21], char filter[21], int iebound, float *e_lo,
float *e_hi,  float *sprsp, char arfversn[6], int chatter, int *ierr)
{ 
    int iunit;
    iunit = CFITS2Unit(fptr);
    cwcal_ReadARF1(iunit,chatter,telescop,instrume,detnam,filter,
         iebound,e_lo,e_hi,sprsp, arfversn,*ierr);
    return;
} 

void ReadEBD(fitsfile *fptr, char telescop[21], 
	     char instrume[21], char detnam[21], char filter[21], 
	     float *areascal, char chantype[21], int *flchan, 
	     int maxchan, int* iebound, int *channel, float *e_min,  
	     float *e_max, char rmfversn[6], int chatter, int *ierr)
{ 
    int iunit;
    iunit = CFITS2Unit(fptr);
    cwcal_ReadEBD3( iunit,chatter,maxchan, telescop,instrume,detnam,
	  filter,*areascal, chantype, *flchan, *iebound,channel,e_min,e_max, 
	  rmfversn,*ierr) ;
    return;
}

void ReadGTI(fitsfile *fptr, int *n_gti, int max_gti, float *start,
    float *stop, int chatter,int *ierr)
{ 
    int iunit;
    iunit = CFITS2Unit(fptr);
    cwcal_ReadGTI1(iunit,*n_gti,max_gti,start,stop,chatter,*ierr);
    return;
}

void ReadRMF(fitsfile *fptr, int qorder, int maxen, int maxgrp, int maxelt, 
	    char *rmfversn, char* hduclas3,char *telescop, char *instrume, 
	 char *detnam, char *filter,
         float *areascal, char *chantype, int *flchan, int *numchn, 
	 int *nenerg, int *numgrp, int *numelt,  
         float *energ_lo, float *energ_hi, 
         int *ngrp, int *f_chan, int *n_chan, int *isorder, int* order,
         float *fmatrix, float *lo_thresh, int chatter, int *ierr)
{
    int iunit;
    char info[7][69];
    int num[5];

    iunit = CFITS2Unit(fptr); 
    cwcal_ReadRMF4(iunit, chatter, qorder,maxen,maxgrp,maxelt,
     *areascal, energ_lo, energ_hi, ngrp, f_chan, n_chan, *isorder,
      order, fmatrix, *lo_thresh, info, num, *ierr); 
    *flchan = num[0];
    *numchn = num[1];
    *nenerg = num[2];
    *numgrp = num[3];
    *numelt = num[4];
    strcpy(rmfversn,info[0]);
    strcpy(hduclas3,info[1]);
    strcpy(telescop,info[2]);
    strcpy(instrume,info[3]);
    strcpy(detnam,info[4]);
    strcpy(filter,info[5]); 
    strcpy(chantype,info[6]);
    return;
}

void WriteRMF(fitsfile *fptr, int nk_hist, char *hist, 
         int nk_comm, char *comment, 
	 char *rmfversn, char *hduclas3, char *telescop, 
         char *instrume, char *detnam, char *filter,
         float areascal, char *chantype, int flchan, int numelt,  
	 int nchan, int nenerg, int numgrp,
         float *energ_lo, float *energ_hi, 
         int *ngrp, int *f_chan, int *n_chan, int qorder, int *order,
         float *fmatrix, float lo_thresh, int chatter, int *ierr)
{ 
     int ounit;
     char info[7][69];
     int num[4];
   
     ounit = CFITS2Unit(fptr); 
     num[0] = chatter;
     num[1] = flchan;
     num[2] = nchan;
     
     strcpy(info[0],rmfversn);
     strcpy(info[1],hduclas3);
     strcpy(info[2],telescop);
     strcpy(info[3],instrume);
     strcpy(info[4],detnam);
     strcpy(info[5],filter);
     strcpy(info[6],chantype); 

     cwcal_WriteRMF4(ounit,nk_hist,hist,nk_comm,comment,
     areascal,numelt,  nenerg,numgrp,energ_lo,energ_hi, 
     ngrp,f_chan,n_chan,qorder,order,fmatrix,lo_thresh,info, num); 
     *ierr = num[3];
     return;
}

void ReadRPRF(fitsfile *fptr, 
	    int *nrad, float* rad_lo, float* rad_hi,  char* radunit, 
	    int *ntheta, float* theta_lo, float* theta_hi, char* thetaunit,
	    int *nenerg, float* energ_lo, float* energ_hi, char* energunit,
	    float *rpsf, int *qerror, float *rpsf_err, 
	    char* rpsfunit, int *qarea, float *area_wgt, 
	    char* telescop, char* instrume,
	    int maxrad, int maxtheta, char* hduclass,
	    int chatter, int *ierr) 
{
    int iunit;
    char info[7][69];
    iunit = CFITS2Unit(fptr); 
    cwcal_ReadRPF1(iunit,*nrad,rad_lo,rad_hi, *ntheta, theta_lo,
             theta_hi, *nenerg, energ_lo, energ_hi, rpsf, *qerror, 
             rpsf_err, *qarea, area_wgt,  maxrad, maxtheta, 
             info, chatter, *ierr); 
    strcpy(radunit,info[1]);
    strcpy(thetaunit,info[2]);
    strcpy(energunit,info[3]);
    strcpy(rpsfunit,info[4]);
    strcpy(telescop,info[5]);
    strcpy(instrume,info[6]);
    strcpy(hduclass,info[0]);
    return;
}

void WriteRPRF(fitsfile *fptr, char *extname,
             int nrad,float* rad_lo,float* rad_hi,
             char *radunit, int ntheta,float* theta_lo,
             float* theta_hi,char *thetaunit,
             int nenerg,float* energ_lo,float* energ_hi,
             char *energunit, float* rpsf,int* qerror,
             float* rpsf_err,char *rpsfunit,int *qarea,
             float* area_wgt, int nk_hist, char hist[][81],
             int nk_comm, char comms[][81],
             char *telescop,char *instrume,char *hduclas3,
             int maxrad, int maxtheta, int chatter, int *ierr) 
{ 
     int ounit;
     char info[8][69];
     int qlog[2];
     int num[5];
    
     ounit = CFITS2Unit(fptr); 
     strcpy(info[0],extname);
     strcpy(info[1],hduclas3);
     strcpy(info[2],radunit);
     strcpy(info[3],thetaunit);
     strcpy(info[4],energunit);
     strcpy(info[5],rpsfunit);
     strcpy(info[6],telescop);
     strcpy(info[7],instrume);
     num[0] = nrad;
     num[1] = ntheta;
     num[2] =  nenerg;
     num[4] = chatter; 
     cwcal_WriteRPF1(ounit,rad_lo,rad_hi, theta_lo,theta_hi, 
        energ_lo,energ_hi, rpsf,rpsf_err,area_wgt,hist,
        nk_hist,comms, nk_comm, maxrad, maxtheta,info, num,qlog ); 
     *qerror = qlog[0];
     *qarea = qlog[1];
     *ierr = num[3];
     return;
}
 


void ReadREEF(fitsfile *fptr, int* nrad,float *rad_lo,float *rad_hi,
	char* radunit, int *ntheta,float *theta_lo,float *theta_hi,
	char* thetaunit, int *nenerg,float *energ_lo,float *energ_hi,
	char* energunit,float *reef, int *qerror,float *reef_err,
	char* reefunit, int *qarea,float *area_wgt,char* telescop,
	char* instrume, char* hduclas3, int maxrad, int maxtheta, 
	int chatter,  int *ierr)
{
    int iunit;
    char info[7][69];
    int num[3];
    int qlog[2];

    iunit = CFITS2Unit(fptr); 
    cwcal_ReadEEF(iunit,rad_lo,rad_hi, theta_lo,theta_hi,  energ_lo,energ_hi, 
     		reef,reef_err,  area_wgt, maxrad,maxtheta,info, 
		num,qlog,*ierr,chatter); 
    *nrad   = num[0];
    *ntheta = num[1];
    *nenerg = num[2]; 

    *qerror = qlog[0];
    *qarea  = qlog[1];
    strcpy(radunit,info[1]);
    strcpy(thetaunit,info[2]);
    strcpy(energunit,info[3]);
    strcpy(reefunit,info[4]);
    strcpy(telescop,info[5]);
    strcpy(instrume,info[6]);
    strcpy(hduclas3,info[0]);
    return;
} 

 void WriteREEF(fitsfile *fptr, char *extname, int nrad,float *rad_lo,
        float *rad_hi,
        char* radunit, int ntheta,float *theta_lo,float *theta_hi,
        char* thetaunit, int nenerg,float *energ_lo,float *energ_hi,
        char* energunit,float *reef, int *qerror,float *reef_err,
        char* reefunit, int *qarea,float *area_wgt,char* telescop,
        char* instrume, char* hduclas3, char *hduclas4,
        int nk_hist, char hist[][81], int nk_comm, char comm[][81],
        int maxrad, int maxtheta, int chatter,  int *ierr)

{
    int ounit;
    char info[9][69];
    int num[4];
    int qlog[2];

    ounit = CFITS2Unit(fptr);
    num[0] = nrad;
    num[1] = ntheta;
    num[2] = nenerg;
    num[3] = chatter;
    strcpy(info[0],extname);
    strcpy(info[1],hduclas3);
    strcpy(info[2],hduclas4);
    strcpy(info[3],radunit);
    strcpy(info[4],thetaunit);
    strcpy(info[5],energunit);
    strcpy(info[6],reefunit);
    strcpy(info[7],telescop);
    strcpy(info[8],instrume);
    cwcal_WriteEEF(ounit,rad_lo,rad_hi, theta_lo,theta_hi, energ_lo,energ_hi, 
        reef,reef_err,  area_wgt, hist,nk_hist,comm,nk_comm, maxrad,
        maxtheta, info, num,qlog,*ierr); 
   *qerror = qlog[0];
   *qarea = qlog[1];
   return;
}

void ReadOBSDateTime(fitsfile *fptr, char *date_obs, 
     char *time_obs,  
     char *date_end, char *time_end, 
     double *mjdobs, int chatter, 
     int *ierr) 

{ 
    int iunit;
    char*  tmp[4]; 
    int i; 
    iunit = CFITS2Unit(fptr);
    for (i = 0; i < 4; i++) { 
        tmp[i] = (char *)calloc(FLEN_VALUE+1, sizeof(char));
        memset(tmp[i],FLEN_VALUE,' ');
        tmp[i][FLEN_VALUE]='\0';
    }
    cwcal_ReadOBSDateTime(chatter, iunit, tmp[0], tmp[1],  
    tmp[2], tmp[3], *mjdobs,*ierr); 
    strcpy(date_obs, tmp[0]);
    strcpy(time_obs, tmp[1]);
    strcpy(date_end, tmp[2]);
    strcpy(time_end, tmp[3]);
    for (i = 0; i < 4; i++) free(tmp[i]); 
    return;
}

void WriteEBD(fitsfile *fptr, int nk_hist, char hist[][FLEN_COMMENT], 
    int nk_comm, char comment[][FLEN_COMMENT], char *rmfversn,  
    char *telescop, char *instrume, char *detnam, char *filter,
    float areascal, char *chantype, int fchan, int iebound, 
    float *e_min, float *e_max, int chatter, int *ierr) 
{
    int ounit;
    ounit = CFITS2Unit(fptr);
    cwcal_WriteEBD3(ounit, chatter, nk_hist, hist, 
        nk_comm, comment,rmfversn,  telescop, instrume, detnam, filter, 
        areascal,  chantype, fchan, iebound, e_min, e_max, *ierr); 
    return;
}
    

void WriteObjRadDec(fitsfile *fptr, int qsys, char radecsys[71], float equinox,
    float ra_obj, float dec_obj, float ra_obje, float dec_obje, 
    int chatter, int *ierr)
{ 
    int ounit;
    ounit = CFITS2Unit(fptr);
    cwcal_WriteObjRadDec(chatter, ounit, qsys, radecsys, equinox,
	   ra_obj, dec_obj, ra_obje, dec_obje,  *ierr); 
    return;
} 

void WritePntDirRadDec(fitsfile *fptr, int qsys, char radecsys[71], 
float equinox, float ra_pnt, float dec_pnt, float pa_pnt, 
float ra_pnte, float dec_pnte,  float pa_pnte, int chatter, int *ierr)
{ 
    int ounit;
    ounit = CFITS2Unit(fptr);
    cwcal_WritePntDirRadDec(chatter, ounit, qsys, radecsys, equinox,
	  ra_pnt, dec_pnt, pa_pnt,ra_pnte, dec_pnte,  pa_pnte, *ierr);
    return;
} 

void GetCALDBFiles( char *tele_str, char *instr_str, char *detnam_str, 
 char *filt_str,  char *codenam_str,  char *strtdate,  char *strtime, 
 char *stpdate,  char *stptime, 
 char *expr_str, int maxret, char filenam[][FLEN_FILENAME],
 int *extno,  char online[][21], int *nret, int *nfound, 
 int chatter, int *status)  

{ 
    cwcal_GetCALDB(chatter, tele_str,instr_str,detnam_str,
         filt_str, codenam_str, strtdate, strtime, stpdate,
      stptime, expr_str, maxret,filenam,extno,online, *nret,*nfound, *status);
    return;
} 

void RemapOneDim(int nin, float *xin_lo, float *xin_hi, float *y_in,
                 int nout, float *xout_lo, float *xout_hi, float *y_out, 
                 int mode, float acc, int chatter,int *ierr) 
{ 
   cwcal_RemapOneDim(chatter, nin, xin_lo, xin_hi, y_in,
                   nout, xout_lo, xout_hi, y_out, mode, acc, *ierr);
   return;
}
