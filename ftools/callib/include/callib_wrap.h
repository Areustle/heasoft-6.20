/* 
    callib_wrap.h: 
         Head file of the Calib C-wrapped fortran routins.
*/ 

#define MFITSUNIT INT
#define MPFITSUNIT PINT
#include "cfortran.h"

/* General routines */ 
#define fndext_ELEMS_7 ZTRINGV_NUM(450)
#define fndext_ELEMLEN_7 ZTRINGV_NUM(20)
#define fndext_ELEMS_8 ZTRINGV_NUM(9)
#define fndext_ELEMLEN_8 ZTRINGV_NUM(20)
#define fndext_ELEMS_9 ZTRINGV_NUM(50)
#define fndext_ELEMLEN_9 ZTRINGV_NUM(40)
PROTOCCALLSFSUB10(FNDEXT,fndext, INT, MFITSUNIT, STRING, INT, PINT, INTV, PZTRINGV, PZTRINGV, PZTRINGV, PINT)
#define cwcal_FindExtName(chatter, iunit, instr, nsearch, nfound, next, outhdu, outver,extnam,ierr) \
CCALLSFSUB10 (FNDEXT,fndext, INT, MFITSUNIT, STRING, INT, PINT, INTV, PZTRINGV,PZTRINGV, PZTRINGV, PINT, \
chatter, iunit, instr, nsearch, nfound, next, outhdu, outver,extnam,ierr) 

#define fndhdu_ELEMLEN_4 ZTRINGV_NUM(20) 
#define fndhdu_ELEMS_4 ZTRINGV_ARGS(3)
#define fndhdu_ELEMLEN_8 ZTRINGV_NUM(20) 
#define fndhdu_ELEMS_8 ZTRINGV_NUM(450)
#define fndhdu_ELEMLEN_8 ZTRINGV_NUM(20) 
#define fndhdu_ELEMS_9 ZTRINGV_NUM(9)
#define fndhdu_ELEMLEN_9 ZTRINGV_NUM(20)
#define fndhdu_ELEMS_10 ZTRINGV_NUM(50)
#define fndhdu_ELEMLEN_10 ZTRINGV_NUM(40) 
PROTOCCALLSFSUB11(FNDHDU,fndhdu, INT, MFITSUNIT, INT, ZTRINGV, INT, PINT, INTV, PZTRINGV, PZTRINGV, PZTRINGV, PINT)
#define cwcal_FindHduClass(chatter, iunit, ninstr, instr, nserch, nfound, next, outhdu, outver,extnam,ierr) \
CCALLSFSUB11 (FNDHDU,fndhdu, INT, MFITSUNIT, INT, STRINGV, INT, PINT, INTV, PZTRINGV, PZTRINGV, PZTRINGV, PINT, \
chatter, iunit, ninstr, instr, nserch, nfound, next, outhdu, outver,extnam,ierr)

PROTOCCALLSFSUB5(GETKEYS,getkeys, MFITSUNIT, PINT, PZTRINGV, INT, PINT)
#define getkeys_ELEMS_3 ZTRINGV_NUM(500)
#define getkeys_ELEMLEN_3 ZTRINGV_NUM(8)
#define cwcal_GetKeys(iunit, nkeys, keys, chatter, ierr) \
CCALLSFSUB5(GETKEYS,getkeys, MFITSUNIT, PINT, PZTRINGV, INT, PINT,\
iunit, nkeys, keys, chatter,ierr) 

PROTOCCALLSFSUB12(MVER,mver, MFITSUNIT, INT, INT, ZTRINGV, INT, INTV, \
PZTRINGV, PZTRINGV, PZTRINGV, STRING, PINT, INT)
#define mver_ELEMLEN_4 ZTRINGV_NUM(20) 
#define mver_ELEMS_4 ZTRINGV_ARGS(3)
#define mver_ELEMS_7 ZTRINGV_NUM(450)
#define mver_ELEMLEN_7 ZTRINGV_NUM(20) 
#define mver_ELEMS_8 ZTRINGV_NUM(50)
#define mver_ELEMLEN_8 ZTRINGV_NUM(40) 
#define mver_ELEMS_9 ZTRINGV_NUM(9)
#define mver_ELEMLEN_9 ZTRINGV_NUM(20)
#define cwcal_MoveExt(iunit, extnum, ninstr, instr, nsearch, \
next, outhdu, extnames, outver, extname, errflg, chatter ) \
CCALLSFSUB12(MVER,mver, MFITSUNIT, INT, INT, ZTRINGV, INT, INTV, PZTRINGV, PZTRINGV, PZTRINGV, STRING, PINT, INT, \
iunit, extnum, ninstr, instr, nsearch, next, outhdu, extnames, outver, extname, errflg, chatter) 

/* special routines  */ 
PROTOCCALLSFSUB6(GT_RMF_GRIDS,gt_rmf_grids, INT, MFITSUNIT, INT, FLOATV, \
 FLOATV, PINT)
#define cwcal_GetRMFGrids(chatter,iunit,ienerg,en_lo,en_hi,ierr) \
CCALLSFSUB6(GT_RMF_GRIDS,gt_rmf_grids, INT, MFITSUNIT, INT, FLOATV, FLOATV, \
PINT, chatter,iunit,ienerg,en_lo,en_hi,ierr)

#define rdarf1_ELEMLEN_3 ZTRINGV_NUM(20) 
#define rdarf1_ELEMS_3 ZTRINGV_NUM(1)
#define rdarf1_ELEMLEN_4 ZTRINGV_NUM(20) 
#define rdarf1_ELEMS_4 ZTRINGV_NUM(1)
#define rdarf1_ELEMLEN_5 ZTRINGV_NUM(20) 
#define rdarf1_ELEMS_5 ZTRINGV_NUM(1)
#define rdarf1_ELEMLEN_6 ZTRINGV_NUM(20) 
#define rdarf1_ELEMS_6 ZTRINGV_NUM(1)
#define rdarf1_ELEMLEN_11 ZTRINGV_NUM(5) 
#define rdarf1_ELEMS_11 ZTRINGV_NUM(1)
PROTOCCALLSFSUB12(RDARF1,rdarf1, MFITSUNIT, INT, PZTRINGV, PZTRINGV, PZTRINGV, \
                 PZTRINGV, INT, FLOATV, FLOATV, FLOATV, PZTRINGV, PINT)
#define cwcal_ReadARF1(iunit,chatter,telescop,instrume,detnam,filter, \
               iebound,e_lo,e_hi,sprsp, arfversn,ierr) \
CCALLSFSUB12( RDARF1,rdarf1, MFITSUNIT, INT, PZTRINGV, PZTRINGV, PZTRINGV, \
PZTRINGV, INT, FLOATV, FLOATV, FLOATV, PZTRINGV, PINT,\
iunit,chatter,telescop,instrume,detnam,filter, \
iebound,e_lo,e_hi,sprsp, arfversn,ierr)

#define rdebd3_ELEMLEN_4 ZTRINGV_NUM(20) 
#define rdebd3_ELEMS_4 ZTRINGV_NUM(1)
#define rdebd3_ELEMLEN_5 ZTRINGV_NUM(20) 
#define rdebd3_ELEMS_5 ZTRINGV_NUM(1)
#define rdebd3_ELEMLEN_6 ZTRINGV_NUM(20) 
#define rdebd3_ELEMS_6 ZTRINGV_NUM(1)
#define rdebd3_ELEMLEN_7 ZTRINGV_NUM(20) 
#define rdebd3_ELEMS_7 ZTRINGV_NUM(1)
#define rdebd3_ELEMLEN_9 ZTRINGV_NUM(20) 
#define rdebd3_ELEMS_9 ZTRINGV_NUM(1)
#define rdebd3_ELEMLEN_15 ZTRINGV_NUM(5) 
#define rdebd3_ELEMS_15 ZTRINGV_NUM(1)
PROTOCCALLSFSUB16(RDEBD3,rdebd3, MFITSUNIT, INT, INT, PZTRINGV, PZTRINGV, \
    PZTRINGV, PZTRINGV, PFLOAT, PZTRINGV,PINT, PINT, INTV,FLOATV, FLOATV, \
    PZTRINGV, PINT)
#define cwcal_ReadEBD3(iunit,chatter,maxchan, telescop,instrume,detnam,\
      filter,areascal, chantype, flchan, iebound,channel,e_min,e_max, \
      rmfversn,ierr) \
CCALLSFSUB16(RDEBD3,rdebd3, MFITSUNIT, INT, INT, PZTRINGV, PZTRINGV, \
    PZTRINGV, PZTRINGV, PFLOAT, PZTRINGV,PINT, PINT, INTV,FLOATV, FLOATV,\
    PZTRINGV, PINT, iunit,chatter,maxchan, telescop,instrume,detnam,\
      filter,areascal, chantype, flchan, iebound,channel,e_min,e_max, \
     rmfversn,ierr) 

 
PROTOCCALLSFSUB7(RDGTI1,rdgti1, MFITSUNIT, PINT, INT, FLOATV, FLOATV, \
    INT, PINT)
#define cwcal_ReadGTI1(iunit,n_gti,max_gti,start,stop,chatter,ierr) \
CCALLSFSUB7(RDGTI1,rdgti1, MFITSUNIT, PINT, INT, FLOATV, FLOATV, \
    INT, PINT, iunit,n_gti,max_gti,start,stop,chatter,ierr)

PROTOCCALLSFSUB8(RDTOBS,rdtobs, INT, MFITSUNIT, PSTRING, PSTRING, \
    PSTRING, PSTRING, PDOUBLE, PINT)
#define  cwcal_ReadOBSDateTime(chatter, iunit, date_obs, time_obs,\
     date_end, time_end, mjdobs,ierr) \
CCALLSFSUB8( RDTOBS,rdtobs, INT, MFITSUNIT, PSTRING, PSTRING, \
    PSTRINGV, PSTRINGV, PDOUBLE, PINT, \
    chatter, iunit, date_obs, time_obs, \
     date_end, time_end, mjdobs,ierr) 

PROTOCCALLSFSUB20(RDRPF1W,rdrpf1w,MFITSUNIT, PINT, FLOATV, FLOATV, \
PINT, FLOATV, FLOATV, PINT, FLOATV, FLOATV, \
FLOATV, PLOGICAL,FLOATV, PLOGICAL, FLOATV, \
INT, INT, PZTRINGV, INT,PINT)
#define rdrpf1w_ELEMLEN_18 ZTRINGV_NUM(68)
#define rdrpf1w_ELEMS_18 ZTRINGV_NUM(7)
#define cwcal_ReadRPF1(iunit, \
nrad,rad_lo,rad_hi, ntheta, theta_lo, theta_hi, \
nenerg, energ_lo, energ_hi, rpsf, qerror, \
rpsf_err, qarea, area_wgt,  maxrad, maxtheta, \
info, chatter, ierr) \
CCALLSFSUB20(RDRPF1W,rdrpf1w,MFITSUNIT, PINT, FLOATV, FLOATV,PINT, \
FLOATV, FLOATV, PINT, FLOATV, FLOATV, FLOATV, \
PLOGICAL,FLOATV,  PLOGICAL, FLOATV, INT, INT,  \
PZTRINGV, INT,PINT, iunit,nrad,rad_lo,rad_hi, ntheta, theta_lo, \
theta_hi, nenerg, energ_lo, energ_hi, rpsf, qerror, \
rpsf_err, qarea, area_wgt,  maxrad, maxtheta, info, chatter, ierr)

PROTOCCALLSFSUB17(RDEEF1W,rdeef1w,MFITSUNIT, \
FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, \
FLOATV, FLOATV, INT, INT, PZTRINGV,INTV, INTV, PINT, INT)
#define rdeef1w_ELEMLEN_13 ZTRINGV_NUM(68)
#define rdeef1w_ELEMS_13 ZTRINGV_NUM(7)
#define cwcal_ReadEEF(iunit,rad_lo,rad_hi, \
theta_lo,theta_hi,  energ_lo,energ_hi, \
 reef,reef_err,  area_wgt, maxrad,maxtheta,info, \
num,qlog,ierr,chatter) \
CCALLSFSUB17(RDEEF1W,rdeef1w,MFITSUNIT, \
FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, \
FLOATV, FLOATV, INT, INT, PZTRINGV,INTV, INTV, PINT, INT, \
iunit,rad_lo,rad_hi,theta_lo,theta_hi,  energ_lo,energ_hi, \
reef,reef_err,  area_wgt, maxrad,maxtheta,info, num,qlog,ierr,chatter)


PROTOCCALLSFSUB19(RDRMF4W,rdrmf4w,MFITSUNIT, INT, LOGICAL, INT, INT, \
INT, PFLOAT, FLOATV, FLOATV, INTV, INTV, INTV, PLOGICAL,INTV,\
FLOATV, PFLOAT,PZTRINGV,INTV,PINT)
#define rdrmf4w_ELEMLEN_17 ZTRINGV_NUM(68)
#define rdrmf4w_ELEMS_17 ZTRINGV_NUM(7)
#define cwcal_ReadRMF4(unit, chatter, Qorder,Maxen,Maxgrp,Maxelt, \
 areascal, energ_lo, energ_hi, ngrp, F_chan, N_chan, Isorder,\
 Order, Fmatrix, lo_thresh, info, num, ierr) \
CCALLSFSUB19(RDRMF4W,rdrmf4w,MFITSUNIT, INT, LOGICAL, INT, INT, \
INT, PFLOAT, FLOATV, FLOATV, INTV, INTV, INTV, PLOGICAL,INTV,\
FLOATV, PFLOAT,PZTRINGV,INTV,PINT,\
unit, chatter, Qorder,Maxen,Maxgrp,Maxelt,\
areascal, energ_lo, energ_hi, ngrp, F_chan, N_chan, Isorder,\
Order, Fmatrix, lo_thresh, info, num, ierr)


PROTOCCALLSFSUB18(WTEBD3, wtebd3, MFITSUNIT, INT, INT, ZTRINGV, \
INT, ZTRINGV, STRING, STRING, STRING, STRING, STRING, FLOAT, \
STRING, INT, INT, FLOATV, FLOATV, PINT) 
#define wtebd3_ELEMLEN_4 ZTRINGV_ARGS(3) 
#define wtebd3_ELEMS_4 ZTRINGV_NUM(FLEN_COMMENT-1)
#define wtebd3_ELEMLEN_6 ZTRINGV_ARGS(5) 
#define wtebd3_ELEMS_6 ZTRINGV_NUM(FLEN_COMMENT-1)
#define cwcal_WriteEBD3(ounit, chatter, nk_hist, hist, \
nk_comm, comment,rmfversn,  telescop, instrume, detnam, filter, \
areascal,  chantype, fchan, iebound, e_min, e_max, ierr) \
CCALLSFSUB18(WTEBD3, wtebd3, MFITSUNIT, INT, INT, ZTRINGV, \
INT, ZTRINGV, STRING, STRING, STRING, STRING, STRING, FLOAT, \
STRING, INT, INT, FLOATV, FLOATV, PINT, \
ounit, chatter, nk_hist, hist, \
nk_comm, comment,rmfversn,  telescop, instrume, detnam, filter, \
areascal,  chantype, fchan, iebound, e_min, e_max, ierr) 


PROTOCCALLSFSUB10(WT_OBJRADEC,wt_objradec, INT, MFITSUNIT, LOGICAL, \
PSTRING,FLOAT, FLOAT, FLOAT, FLOAT,FLOAT, PINT) 
#define  cwcal_WriteObjRadDec(chatter, ounit, qsys, radecsys, equinox,\
       ra_obj, dec_obj, ra_obje, dec_obje,  ierr) \
CCALLSFSUB10( WT_OBJRADEC,wt_objradec, INT, MFITSUNIT, LOGICAL,\
PSTRING,FLOAT, FLOAT, FLOAT, FLOAT,FLOAT, PINT, \
chatter, ounit, qsys, radecsys, equinox,\
ra_obj, dec_obj, ra_obje, dec_obje,  ierr)

PROTOCCALLSFSUB12(WT_PNTRADEC,wt_pntradec, INT, MFITSUNIT, LOGICAL, \
PSTRING,FLOAT, FLOAT, FLOAT, FLOAT,FLOAT, FLOAT, FLOAT, PINT) 
#define  cwcal_WritePntDirRadDec(chatter, ounit, qsys, radecsys, equinox,\
       ra_pnt, dec_pnt, pa_pnt,ra_pnte, dec_pnte,  pa_pnte, ierr) \
CCALLSFSUB12( WT_PNTRADEC,wt_pntradec, INT, MFITSUNIT, LOGICAL, \
PSTRING,FLOAT, FLOAT, FLOAT, FLOAT,FLOAT, FLOAT, FLOAT, PINT, \
chatter, ounit, qsys, radecsys, equinox,\
ra_pnt, dec_pnt, pa_pnt,ra_pnte, dec_pnte,  pa_pnte, ierr)

PROTOCCALLSFSUB19(WTRPF1W,wtrpf1w,MFITSUNIT, FLOATV, FLOATV, \
FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, FLOATV,  FLOATV, \
PZTRINGV, INT, PSTRINGV, INT, INT, INT, PZTRINGV, INTV,LOGICALV)
#define wtrpf1w_ELEMLEN_11 ZTRINGV_NUM(80)
#define wtrpf1w_ELEMS_11 ZTRINGV_ARGS(12)
#define wtrpf1w_ELEMLEN_13 ZTRINGV_NUM(80)
#define wtrpf1w_ELEMS_13 ZTRINGV_ARGS(14)
#define wtrpf1w_ELEMLEN_17 ZTRINGV_NUM(68)
#define wtrpf1w_ELEMS_17 ZTRINGV_NUM(8)
#define cwcal_WriteRPF1(ounit,rad_lo,rad_hi, theta_lo,theta_hi, \
        energ_lo,energ_hi, rpsf,rpsf_err,area_wgt,hist,nk_hist,comms, \
         nk_comm, maxrad, maxtheta,info, num,qlog ) \
CCALLSFSUB19(WTRPF1W,wtrpf1w,MFITSUNIT, FLOATV, FLOATV, \
FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, FLOATV,  FLOATV, \
PZTRINGV, INT, PZTRINGV, INT, INT, INT, PZTRINGV, INTV, LOGICALV, \
ounit,rad_lo,rad_hi, theta_lo,theta_hi, \
energ_lo,energ_hi, rpsf,rpsf_err,area_wgt,hist,nk_hist,comms, \
 nk_comm, maxrad, maxtheta,info, num,qlog )

PROTOCCALLSFSUB20(WTRMF4W,wtrmf4w,MFITSUNIT,INT, \
ZTRINGV, INT, ZTRINGV, FLOAT, INT, INT, \
INT, FLOATV, FLOATV, INTV, INTV, INTV,LOGICAL,INTV,\
FLOATV, FLOAT,ZTRINGV,INTV)
#define wtrmf4w_ELEMLEN_3 ZTRINGV_NUM(FLEN_COMMENT-1)
#define wtrmf4w_ELEMS_3 ZTRINGV_ARGS(2)
#define wtrmf4w_ELEMLEN_5 ZTRINGV_NUM(FLEN_COMMENT-1)
#define wtrmf4w_ELEMS_5 ZTRINGV_ARGS(4)
#define wtrmf4w_ELEMLEN_19 ZTRINGV_NUM(68)
#define wtrmf4w_ELEMS_19 ZTRINGV_NUM(7)
#define cwcal_WriteRMF4(Ounit,Nk_hist,Hist,Nk_comm,Comment,\
Areascal,Numelt,  Nenerg,Numgrp,Energ_lo,Energ_hi, \
Ngrp,F_chan,N_chan,Qorder,Order,Fmatrix,Lo_thresh,info, num) \
CCALLSFSUB20(WTRMF4W,wtrmf4w,MFITSUNIT,INT, \
ZTRINGV, INT, ZTRINGV, FLOAT, INT, INT, \
INT, FLOATV, FLOATV, INTV, INTV, INTV,LOGICAL,INTV,\
FLOATV, FLOAT,ZTRINGV,INTV, \
Ounit,Nk_hist,Hist,Nk_comm,Comment,\
Areascal,Numelt,  Nenerg,Numgrp,Energ_lo,Energ_hi, \
Ngrp,F_chan,N_chan,Qorder,Order,Fmatrix,Lo_thresh,info, num)

PROTOCCALLSFSUB20(WTEEF1W,wteef1w,MFITSUNIT, \
FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, \
FLOATV, FLOATV,  ZTRINGV, INT, ZTRINGV, INT, INT, INT,\
PZTRINGV,INTV, INTV, PINT)
#define wteef1w_ELEMLEN_17 ZTRINGV_NUM(68)
#define wteef1w_ELEMS_17 ZTRINGV_NUM(7)
#define wteef1w_ELEMLEN_11 ZTRINGV_NUM(80)
#define wteef1w_ELEMS_11 ZTRINGV_ARGS(12)
#define wteef1w_ELEMLEN_13 ZTRINGV_NUM(80)
#define wteef1w_ELEMS_13 ZTRINGV_ARGS(14)
#define cwcal_WriteEEF(ounit,rad_lo,rad_hi, \
theta_lo, theta_hi, energ_lo,energ_hi, \
reef,reef_err,  area_wgt,  \
hist,nk_hist,comms,nk_comm, maxrad, maxtheta, info, num,qlog, ierr) \
CCALLSFSUB20( WTEEF1W,wteef1w,MFITSUNIT, \
FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, FLOATV, \
FLOATV, FLOATV,  ZTRINGV, INT, ZTRINGV, INT, INT, INT,\
PZTRINGV,INTV, INTV, PINT, ounit,rad_lo,rad_hi, \
theta_lo, theta_hi, energ_lo,energ_hi, \
reef,reef_err,  area_wgt, \
hist,nk_hist,comms,nk_comm, maxrad, maxtheta,info, num,qlog, ierr)



PROTOCCALLSFSUB18(GTCALF,gtcalf, INT, PSTRING, \
    PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,\
 PSTRING, PSTRING,INT, PZTRINGV, INTV, PZTRINGV, PINT, PINT, PINT)
#define gtcalf_ELEMLEN_13 ZTRINGV_NUM(FLEN_FILENAME-1) 
#define gtcalf_ELEMS_13 ZTRINGV_ARGS(12)
#define gtcalf_ELEMLEN_15 ZTRINGV_NUM(30) 
#define gtcalf_ELEMS_15 ZTRINGV_ARGS(12)
#define cwcal_GetCALDB(chatter,tele_str,instr_str,detnam_str,\
filt_str, codenam_str, strtdate, strtime, stpdate,\
stptime, expr_str, maxret,filenam,extno,online, nret,nfound, status) \
CCALLSFSUB18( GTCALF,gtcalf, INT, PSTRING,\
    PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,\
 PSTRING, PSTRING,INT, PZTRINGV, INTV, PZTRINGV, PINT, PINT, PINT,\
chatter,tele_str,instr_str,detnam_str, \
filt_str, codenam_str, strtdate, strtime, stpdate,\
stptime, expr_str, maxret,filenam,extno,online, nret,nfound, status) 


PROTOCCALLSFSUB12(RMAP1D,rmap1d, INT, INT, FLOATV, FLOATV, FLOATV, \
INT, FLOATV, FLOATV, FLOATV, INT, FLOAT, PINT)
#define cwcal_RemapOneDim(chatter, nin, xin_lo, xin_hi, y_in, \
 nout, xout_lo, xout_hi, y_out, mode, acc, ierr) \
CCALLSFSUB12(RMAP1D,rmap1d, INT, INT, FLOATV, FLOATV, FLOATV, \
INT, FLOATV, FLOATV, FLOATV, INT, FLOAT, PINT, \
chatter, nin, xin_lo, xin_hi, y_in, \
nout, xout_lo, xout_hi, y_out, mode, acc, ierr) 

