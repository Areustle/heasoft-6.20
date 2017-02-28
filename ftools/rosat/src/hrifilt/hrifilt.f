
*+HRIFILT
c      -----------------
       subroutine hrifit 
c      -----------------
c --- DESCRIPTION -----------------------------------------------------
c HRIFILT writes a ROSAT HRI makefilter file, that is a file containing
c HK info, binned in a usefull way, for user-defined data screening in 
c xselect. This task takes an RDF ancillary file as input. Later it may
c be enhanced to read older formats.  
c
c --- CALLED ROUTINES -------------------------------------------------
c
c HRIF_GP     : Gets parameters
c HRIF_RDAT   : Reads data from RDF ancillary file
c HRIF_SAMPLE : Samples data from each extension from *_anc file
c HRIF_WT     : Write null primary array, and mkf data to outfile
c 
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1994 Oct 20)1.0.0
c
c Banashree M Seifert (1997, Aug 28) 2.0.0
c      . COMMON statement removed -- COMMON/task/taskname
c      . dimensions declaration is cleaned in various subroutines so 
c        that it carries dimensions from calling routine. So, replaced 
c        by (*) within subroutines instead of specifically mentioning 
c        the dimension
c      . before calling MVER, extnum=-1 instead of extnum=0 
c      . while calling 
c   cc      call rhmvrd(iunit,n_evr,max_evr,time,livt_cor,
c                                         ^
c                   time variable should be ev_time instead of time
c Peter D Wilson (1998 June 30) 2.0.1
c      . Updated for new FCPARS behavior
c--------------------------------------------------------------------
      IMPLICIT NONE
      character(5) version
      parameter (version = '2.0.1')
      character(40) taskname

*-
c ---------------------------------------------------------------------
c
C --- DYNAMIC MEMORY ALLOCATION --- 
C  the following MEM common block definition is in the system iraf77.inc
C  file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c ---------------------------------------------------------------------
c VARIABLES ...

      character(80) infile, outfile,desc,subinfo
      character(16) telescope,instrume,detnam
      integer chatter,errflg
      integer status
      character(30) termdesc,context,errstr
      logical del_exist

c MAX ARRAY SIZES
      integer max_ep,max_att,max_hkp
      integer max_evr,max_mkf

c COUNTERS ...

      integer n_ep,n_att,n_hkp,n_evr,n_mkf 

c ... pointers to "arrays" to be dynamically allocated

c     for EPHEM data
      integer p_mjd_int,p_mjd_frac,p_sun_x,p_obi_num
      integer p_sun_y,p_sun_z
      integer p_moon_x,p_moon_y,p_moon_z
      integer p_lon_east,p_lat_nort,p_gha,p_alt_sat
      integer p_sat_x,p_sat_y,p_sat_z,p_ses_ang,p_est_ang
      real*8 mjdref
c     "arrays" to be dynamically allocated for EPHEM data
c     integer mjd_int(max_ep)
c     real*8 mjd_frac(max_ep),gha(max_ep)
c     real*8 sun_x(max_ep),sun_y(max_ep),sun_z(max_ep)
c     real*8 moon_x(max_ep),moon_y(max_ep),moon_y(max_ep)
c     real*8 lon_east(max_ep),lat_nort(max_ep),alt_sat(max_ep)
c     real ses_ang(max_ep),est_ang(max_ep)

c     for ASPECT data
      integer p_time,p_roan_sc,p_ra_sc,p_dec_sc
      integer p_asp_qual,p_stt_qual,p_asp_rows
c     "arrays" to be dynamically allocated for ASPECT data
c     real*8 time(max_att),ra_sc(max_att),dec_sc(max_att)
c     real*8 roan_sc(max_att)
c     integer asp_qual(max_att),stt_qual(max_att)

c     for HKP data
      integer p_hkp_time,p_temp1,p_temp2,p_temp3
c     "arrays" to be dynamically allocated for HKP data
c     real temp1(max_hkp),temp2(max_hkp),temp3(max_hkp) 

c     for EVENTS data
      integer p_ev_time,p_accepted,p_livt_cor
c     "arrays" to be dynamically allocated for EVENTS data
c     real*8 ev_time(max_evr)
c     real accepted(max_evr),livt_cor(max_evr)

c     for output data
      integer p_md
      integer p_oaccepted,p_olivt_cor
      integer p_hkp_rows
c     "arrays" to be dynamically allocated
c     real*8 md(max_mkf)
c     real oaccepted(max_mkf),olivt_cor(max_mkf)
c     integer hkp_rows(max_mkf)

c --- INITIALISATION ---

      taskname='HRIFILT '//version
      context = 'fatal error'
      termdesc =' HRIFILT Ver '//version//' terminated !'

c --- GET PARAMETERS ---

      errflg = 0
      call hrif_gp(infile,outfile,del_exist,errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 200 
      ENDIF

c
c --- USER INFO ---
c
      IF (chatter.GE.1) THEN
        desc = ' Main HRIFILT Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- DETERMINE ARRAY DIMENSIONS ---
c
      call hrif_dims(infile,max_ep,max_att,max_hkp,max_evr,
     &                     errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 200 
      ENDIF

      max_mkf = max_ep

c ALLOCATE DMA

      p_mjd_int = 0
      p_mjd_frac = 0
      p_sun_x = 0
      p_sun_y = 0
      p_sun_z = 0
      p_moon_x = 0
      p_moon_y = 0
      p_moon_z = 0
      p_lon_east = 0
      p_lat_nort = 0
      p_sat_x = 0
      p_sat_y = 0
      p_sat_z = 0
      p_gha = 0
      p_alt_sat = 0
      p_ses_ang = 0
      p_est_ang = 0
      p_obi_num = 0
      p_time = 0
      p_roan_sc = 0
      p_ra_sc = 0
      p_dec_sc = 0 
      p_asp_qual = 0
      p_stt_qual = 0
      p_asp_rows = 0
      p_hkp_time = 0
      p_temp1 = 0
      p_temp2 = 0
      p_temp3 = 0 
      p_ev_time = 0
      p_accepted = 0
      p_livt_cor = 0
      p_md = 0
      p_oaccepted = 0
      p_olivt_cor = 0
      p_hkp_rows = 0

      status = 0
      call udmget(max_ep,4,p_mjd_int,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      status = 0
      call udmget(max_ep,7,p_mjd_frac,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      status = 0
      call udmget(max_ep,7,p_sun_x,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_sun_y,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_sun_z,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_moon_x,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_moon_y,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_moon_z,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_lon_east,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_lat_nort,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,4,p_sat_x,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,4,p_sat_y,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,4,p_sat_z,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_gha,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_alt_sat,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF 
      call udmget(max_ep,6,p_ses_ang,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,6,p_est_ang,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,3,p_obi_num,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF

      call udmget(max_att,7,p_time,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_roan_sc,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_ra_sc,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_dec_sc,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,3,p_asp_qual,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,3,p_stt_qual,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,4,p_asp_rows,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_hkp,7,p_hkp_time,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_hkp,6,p_temp1,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_hkp,6,p_temp2,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_hkp,6,p_temp3,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,7,p_ev_time,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,6,p_accepted,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,6,p_livt_cor,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_md,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,6,p_oaccepted,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,6,p_olivt_cor,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_hkp_rows,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF

 50   IF (status.NE.0) THEN
        subinfo = errstr//' failed to allocate Dynamic memory'
        call fcecho(subinfo)
        errflg = 1
        goto 70
      ENDIF
c
c --- READ FILE ---
c
      call hrif_rdat(infile,telescope,instrume,detnam,
     &  max_ep,n_ep,MEMI(p_mjd_int),MEMD(p_mjd_frac),
     &  MEMD(p_sun_x),MEMD(p_sun_y),MEMD(p_sun_z),
     &  MEMD(p_moon_x),MEMD(p_moon_y),MEMD(p_moon_z),
     &  MEMD(p_lon_east),MEMD(p_lat_nort),
     &  MEMI(p_sat_x),MEMI(p_sat_y),MEMI(p_sat_z),MEMD(p_gha),
     &  MEMD(p_alt_sat),MEMR(p_ses_ang),MEMR(p_est_ang),MEMS(p_obi_num),
     &  mjdref,max_att,n_att,MEMD(p_time),
     &  MEMD(p_roan_sc),MEMD(p_ra_sc),
     &  MEMD(p_dec_sc),MEMS(p_asp_qual),MEMS(p_stt_qual),
     &  max_hkp,n_hkp,MEMD(p_hkp_time),MEMR(p_temp1),MEMR(p_temp2),
     &  MEMR(p_temp3),max_evr,n_evr,MEMD(p_ev_time),MEMR(p_accepted),
     &  MEMR(p_livt_cor),errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 70
      ENDIF
c
c --- SORT THROUGH DATA ---
c
      call hrif_sample(max_ep,n_ep,MEMI(p_mjd_int),MEMD(p_mjd_frac),
     & MEMD(p_sun_x),MEMD(p_sun_y),MEMD(p_sun_z),MEMD(p_moon_x),
     & MEMD(p_moon_y),MEMD(p_moon_z),MEMD(p_lon_east),
     & MEMD(p_lat_nort),MEMI(p_sat_x),MEMI(p_sat_y),MEMI(p_sat_z),
     & MEMD(p_gha),MEMD(p_alt_sat),MEMR(p_ses_ang),MEMR(p_est_ang),
     & max_att,n_att,
     & MEMD(p_time),MEMD(p_roan_sc),MEMD(p_ra_sc),MEMD(p_dec_sc),
     & MEMS(p_asp_qual),MEMS(p_stt_qual),
     & max_evr,n_evr,MEMD(p_ev_time),MEMR(p_accepted),MEMR(p_livt_cor),
     & max_hkp,MEMD(p_hkp_time),n_hkp,max_mkf,n_mkf,
     & MEMD(p_md),MEMR(p_oaccepted),
     & MEMR(p_olivt_cor),MEMI(p_hkp_rows),MEMI(p_asp_rows),
     & mjdref,errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 70
      ENDIF

c --- WRITE OUTFILE ---

      call hrif_wt(outfile,telescope,instrume,detnam,infile,
     & max_mkf,n_mkf,MEMD(p_md),
     & MEMD(p_sun_x),MEMD(p_sun_y),MEMD(p_sun_z),
     & MEMD(p_moon_x),MEMD(p_moon_y),MEMD(p_moon_z),
     & MEMD(p_lon_east),MEMD(p_lat_nort),
     & MEMI(p_sat_x),MEMI(p_sat_y),MEMI(p_sat_z),MEMD(p_gha),
     & MEMD(p_alt_sat),MEMR(p_ses_ang),MEMR(p_est_ang),
     & MEMS(p_obi_num),max_att,n_att,
     & MEMS(p_asp_qual),MEMS(p_stt_qual),MEMD(p_roan_sc),
     & MEMD(p_ra_sc),MEMD(p_dec_sc),MEMR(p_oaccepted),
     & MEMR(p_olivt_cor),max_hkp,MEMR(p_temp1),MEMR(p_temp2),
     & MEMR(p_temp3),MEMI(p_hkp_rows),MEMI(p_asp_rows),
     & mjdref,taskname,
     & del_exist,errflg,chatter)
      IF (errflg.NE.0) THEN
        call fcerr(context)
        goto 70 
      ENDIF

c FREE THE DYNAMIC MEMORY

 70   status = 0
      call udmfre(p_mjd_int,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_mjd_frac,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sun_x,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sun_y,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF 
      call udmfre(p_sun_z,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_moon_x,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_moon_y,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_moon_z,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_lon_east,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_lat_nort,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sat_x,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sat_y,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sat_z,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_gha,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_alt_sat,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_ses_ang,6,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_est_ang,6,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_obi_num,3,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_time,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_roan_sc,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_ra_sc,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_dec_sc,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_asp_qual,3,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_stt_qual,3,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_asp_rows,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_hkp_time,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_temp1,6,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_temp2,6,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_temp3,6,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_ev_time,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_accepted,6,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_livt_cor,6,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_md,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_oaccepted,6,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_olivt_cor,6,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
 100  IF (status.NE.0) THEN
       subinfo = errstr//'failed to de-allocate Dynamic Memory'
       call fcecho(subinfo)
       errflg = 99
      ENDIF
 200  IF (errflg.NE.0) THEN
        call fcecho(context)
        call fcecho(termdesc)
        return
      ENDIF
      IF (chatter.GE.1) THEN
        desc = ' HRIFILT ver '//version//' completed'
        call fcecho(desc)
      ENDIF
      return
      end
c ----------------------------------------------------------------------
c     END OF MAIN HRIFILT 
c ----------------------------------------------------------------------

    
*+HRIF_GP
c     -----------------------------------------------------------
      subroutine hrif_gp(infile,outfile,del_exist,errflg,chatter)
c     -----------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile
      character(80) filename
      character(70) ill_files(5)
      character(30) errstr,wrnstr
      integer errflg,chatter,status,n_ill
      character(70) desc
      integer fcstln,flen,extnum
      logical ext,valfil,del_exist
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c infile     char   : input file name
c outfile    char   : Output filename
c chatter    int    : Chattiness flag, >20 verbose
c errflg     int    : Error flag
c
c --- CALLED ROUTINES -------------------------------------------------
c
c UCLGST     : (HOST) Get string input
c UCLGSI     : (HOST) Get integer input
c FCECHO     : (FTOOLS) Screen write
c
c --- COMPILATION/LINKING ---------------------------------------------
c
c CALTOOLS, FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1994 May 11)
c Rehana Yusaf (1994 Sept 28) 1.0.1; add clobber parameter
c Peter Wilson (1998 June 30) 1.0.2; Drop INQUIRE call
      character(5) version 
      parameter (version = '1.0.2')
*-
c ---------------------------------------------------------------------
c
      errstr = ' ERROR : HRIF_GP Ver '//version//':'
      wrnstr = ' WARNING : HRIF_GP Ver '//version//':'

c GET INFILE

      status = 0
      call uclgst('infile',infile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting infile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(infile)
      IF (infile.EQ.'  ') THEN
        errflg = 1
        desc = ' Input Ancillary file not entered !'
        call fcecho(desc)
        return
      ENDIF
C PDW 6/30/98: Don't bother.  Let FTOPEN check for file's existence later
C      call fcpars(infile,filename,extnum,status)
C      ext = .true.
C      flen = fcstln(filename)
C      INQUIRE(FILE=filename(:flen),EXIST=ext)
C      IF (.NOT.ext) THEN
C        errflg = 1
C        desc = errstr//' File does not EXIST :'//filename
C        call fcecho(desc)
C        return
C      ENDIF

c GET OUTFILE 

      status = 0
      call uclgst('outfile',outfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting outfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c CLOBBER PARAMETER

      status = 0
      call uclgsb('clobber',del_exist,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting del_exist parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c OUTFILE VALIDATION 

      call crmvlbk(outfile)
      IF (outfile.EQ.'  ') THEN
        desc = errstr//' outfile must be entered !!'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      n_ill = 1
C PDW 6/30/98: Use ftrtnm to strip off any extension specifier
C      ill_files(1) = infile
      call ftrtnm( infile, ill_files(1), status )
      call ck_file(outfile,ill_files,n_ill,valfil,del_exist,chatter)
      IF (.NOT.valfil) THEN
        errflg = 2
        return
      ENDIF

c GET CHATTER 

      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting chatter parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF        
      return
      end
c ---------------------------------------------------------------------
c     END OF HRIF_GP
c ---------------------------------------------------------------------




c     --------------------------------------------------------- 
      subroutine hrif_rdat(infile,telescope,instrume,detnam,
     &                 max_ep,n_ep,mjd_int,mjd_frac,
     &                 sun_x,sun_y,sun_z,moon_x,moon_y,moon_z,
     &                 lon_east,lat_nort,sat_x,sat_y,sat_z,
     &                 gha,alt_sat,ses_ang,est_ang,obi_num,mjdref,
     &                 max_att,n_att,time,roan_sc,ra_sc,
     &                 dec_sc,asp_qual,stt_qual,
     &                 max_hkp,n_hkp,hkp_time,temp1,temp2,temp3,
     &                 max_evr,n_evr,ev_time,accepted,livt_cor,
     &                 ierr,chatter)
c     ----------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c This routine reads the required data from an RDF Ancillary file.
c ------------------------------------------------------------------
c --- VARIABLES ---
c 
      IMPLICIT NONE
      character*(*) infile,telescope,instrume,detnam
      integer ierr,chatter

c EPHEM data 

      integer max_ep,n_ep
      integer mjd_int(*)
      integer*2 obi_num(*)
      real*8 mjd_frac(*),gha(*),mjdref
      real*8 sun_x(*),sun_y(*),sun_z(*)
      real*8 moon_x(*),moon_y(*),moon_z(*)
      real*8 lon_east(*),lat_nort(*),alt_sat(*)
      integer sat_x(*),sat_y(*),sat_z(*)

      real ses_ang,est_ang

c ASPECT data

      integer max_att,n_att
      real*8 time(*),ra_sc(*)
      real*8 dec_sc(*),roan_sc(*)
      integer*2 asp_qual(*),stt_qual(*)

c HKP data 

      integer max_hkp,n_hkp
      real*8 hkp_time(*)
      real temp1,temp2,temp3

c EVRATE data

      integer max_evr,n_evr
      real*8 ev_time(*)
      real accepted(*),livt_cor(*)
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c ierr       int    : Error flag, 0 is okay
c chatter    int    : Chattiness flag >20 verbose, <5 quiet
c infile     char   : input filename
c max_ep     int    : Maximum array size for EPHEM ext data
c n_ep       int    : counter for EPHEM data
c max_att    int    : Maximum array size for ASPECT ext data
c n_att      int    : counter for ASPECT data
c max_hkp    int    : Maximum array size for HKP ext data
c n_hkp      int    : counter for hkp data
c max_evr    int    : Maximum array size for EVENTS ext data
c n_evr      int    : counter for events data
c max_sta    int    : Maximum array size for HKSTA ext data
c n_sta      int    : counter for HKSTA data
c
c --- CALLED ROUTINES ----------------------------------------------
c
c RDEPRD    : (CALLIB) Reads EPHEM extension in RDF Ancillary file
c RDATRD    : (CALLIB) Reads ASPECT extension in RDF Ancillary file
c RDHKRD    : (CALLIB) Reads HKP extension in RDF Ancillary file
c RDMVRD    : (CALLIB) Reads EVENTS ext in RDF Ancillary file
c RDSTRD    : (CALLIB) Reads HKSTA ext in RDF Ancillary file
c
c ---- LINKING/COMPILATION -----------------------------------------
c
c FITSIO,CALLIB,FTOOLS LIBRARY
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1994 May 17) 1.0.0;
c
c Banashree M Seifert (1997, Aug 28) 2.0.0
c      . dimensions declaration is cleaned so that it carries
c        dimensions from calling routine. So replaced by (*) 
c        instead of specifically mentioning the dimension
c      . before calling MVER, extnum=-1 instead of extnum=0
c      . while calling 
ccc      call rhmvrd(iunit,n_evr,max_evr,time,livt_cor,
c                                         ^
c                   time variable should be ev_time instead of time
c--------------------------------------------------------------------
      character(5) version
      parameter (version = '2.0.0')
*-
c ------------------------------------------------------------------
c --- LOCALS ---
c
      character(30) errstr,wrnstr,comm
      character(70) subinfo,message
      integer iunit,frow,felem,colnum
      integer inull,htype,extnum,errflg,mjdint
      logical anyflg
      real*8 enull,mjdreal,nom_roll,nom_ra,nom_dec

c VARIABLES FOR MVEXT

      integer nsearch,ninstr
      parameter (nsearch = 50)
      integer next(nsearch)
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9)
      character(8) extname 
c
c --- USER INFO ---
c
      errstr = ' ERROR: HRIF_RDAT Ver '//version//':'
      wrnstr = ' WARNING: HRIF_RDAT Ver '//version//':'
      IF (chatter.GE.10) THEN
        subinfo = ' using HRIF_RDAT Ver '//version
        call fcecho(subinfo) 
      ENDIF
c
c --- OPEN FILE ---
c

c LOCATE EPHEM EXTENSION ...

      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'EPHEM'
      extname = 'EPHEM'
      call mvext(0,infile,iunit,ninstr,instr,nsearch,next,outhdu,
     &           extnames,outver,extname,ierr,chatter)       
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF

c READ EPHEM DATA ...

      call rdeprd(iunit,n_ep,max_ep,mjd_int,mjd_frac,
     &                  sun_x,sun_y,sun_z,
     &                  moon_x,moon_y,moon_z,
     &                  lon_east,lat_nort,sat_x,sat_y,sat_z,
     &                  gha,alt_sat,chatter,ierr)
      IF (ierr.NE.0) THEN
        subinfo = errstr//' reading EPHEM data'
        call fcecho(subinfo)
        goto 10 
      ENDIF
      ierr= 0
      call ftgcno(iunit,.false.,'SES_ANG',colnum,ierr)
      IF (ierr.NE.0) THEN
         subinfo= errstr//' SES_ANG column not present in EPHEM!'
         call fcecho(subinfo)
         goto 10
      ENDIF
      frow=1
      felem=1
      enull=0
      ierr=0
      call ftgcve(iunit,colnum,frow,felem,n_ep,enull,ses_ang,
     &             anyflg,ierr)
      subinfo = errstr//' reading ses_ang column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF
      ierr= 0
      call ftgcno(iunit,.false.,'EST_ANG',colnum,ierr)
      IF (ierr.NE.0) THEN
         subinfo= errstr//' EST_ANG column not present in EPHEM!'
         call fcecho(subinfo)
         goto 10
      ENDIF
      frow=1
      felem=1
      enull=0
      ierr=0
      call ftgcve(iunit,colnum,frow,felem,n_ep,enull,est_ang,
     &             anyflg,ierr)
      subinfo = errstr//' reading est_ang column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF

      ierr= 0
      call ftgcno(iunit,.false.,'OBI_NUM',colnum,ierr)
      IF (ierr.NE.0) THEN
         subinfo= errstr//' OBI_NUM column not present in EPHEM!'
         call fcecho(subinfo)
         goto 10
      ENDIF
      frow=1
      felem=1
      inull=0
      ierr=0
      call ftgcvi(iunit,colnum,frow,felem,n_ep,inull,obi_num,
     &             anyflg,ierr)
      subinfo = errstr//' reading obi_num column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF
 
      ierr = 0
      call ftgkyj(iunit,'MJDREFI',mjdint,comm,ierr)
      IF (ierr.NE.0) THEN
        mjdint = 48043
        IF (chatter.GE.20) THEN
          subinfo =errstr//' reading MJDREFI keyword'
          call wt_ferrmsg(ierr,subinfo)
          subinfo = ' default value 48043 is used'
          call fcecho(subinfo)
        ENDIF
      ENDIF

      ierr = 0
      call ftgkyd(iunit,'MJDREFF',mjdreal,comm,ierr)
      IF (ierr.NE.0) THEN
        mjdreal = 8.7974537037007E-01
        IF (chatter.GE.20) THEN
          subinfo =errstr//' reading MJDREFI keyword'
          call wt_ferrmsg(ierr,subinfo)
          subinfo = ' default value 8.7974537037007E-01 is used'
          call fcecho(subinfo)
        ENDIF
      ENDIF
      mjdref = mjdint + mjdreal

c READ TELESCOPE,INSTRUME,DETNAM ...

c     TELESCOP ...

      ierr= 0
      call ftgkys(iunit,'TELESCOP',telescope,comm,ierr)
      IF (chatter.GE.20) THEN
        message = wrnstr//' reading TELESCOP '
        call wt_ferrmsg(ierr,message)
      ENDIF
      IF (ierr.EQ.202) THEN
        telescope = 'UNKNOWN'
      ENDIF

c     INSTRUME ...

      ierr= 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,ierr)
      IF (chatter.GE.20) THEN
        message = wrnstr//' reading INSTRUME '
        call wt_ferrmsg(ierr,message)
      ENDIF
      IF (ierr.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF

c     DETNAM ...

      ierr = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,ierr)
      IF (chatter.GE.30) THEN
        message = wrnstr//' reading DETNAM '
        call wt_ferrmsg(ierr,message)
      ENDIF
      IF (ierr.EQ.202) THEN
        detnam = 'NONE'
      ENDIF

c READ ASPECT EXTENSION ...

      ierr = 0
      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'ASPECT'
      extname = 'ASPECT'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN 
        goto 10 
      ENDIF

c READ ASPECT EXTENSION ...

      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &           outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF

      call rhatrd(iunit,n_att,max_att,time,ra_sc,
     &           dec_sc,roan_sc,nom_ra,nom_dec,
     &           nom_roll,chatter,ierr)
      IF (ierr.NE.0) THEN
        subinfo = errstr//' reading ASPECT data'
        call fcecho(subinfo)
        goto 10 
      ENDIF
      ierr= 0
      call ftgcno(iunit,.false.,'ASP_QUAL',colnum,ierr)
      IF (ierr.NE.0) THEN
       subinfo= errstr//' ASP_QUAL column not present in ASPECT!'
       call fcecho(subinfo)
       goto 10 
      ENDIF
      frow=1
      felem=1
      inull=0
      ierr=0
      call ftgcvi(iunit,colnum,frow,felem,n_att,inull,
     &             asp_qual,anyflg,ierr)
      subinfo = errstr//' reading asp_qual column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF
      ierr= 0
      call ftgcno(iunit,.false.,'STT_QUAL',colnum,ierr)
      IF (ierr.NE.0) THEN
       subinfo= errstr//' STT_QUAL column not present in ASPECT!'
       call fcecho(subinfo)
       goto 10 
      ENDIF
      frow=1
      felem=1
      inull=0
      ierr=0
      call ftgcvi(iunit,colnum,frow,felem,n_att,inull,
     &             stt_qual,anyflg,ierr)
      subinfo = errstr//' reading stt_qual column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF      
c LOCATE HKP EXTENSION ...

      ierr = 0
      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'HKP'
      extname = 'HKP'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF

c READ HKP EXTENSION ...

      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &          outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
       goto 10 
      ENDIF
      call rhhkrd(iunit,n_hkp,max_hkp,hkp_time,temp1,
     &                  temp2,temp3,chatter,ierr)
      IF (ierr.NE.0) THEN
        subinfo = errstr//' reading HKP extension'
        call fcecho(subinfo)
        goto 10 
      ENDIF

c LOCATE EVRATE EXTENSION ...

      ninstr = 2 
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'EVRATE'
      extname = 'EVRATE'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF

c READ EVRATE EXTENSION ...
       
      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &          outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF
      call rhmvrd(iunit,n_evr,max_evr,ev_time,livt_cor,
     &                  chatter,ierr)
      IF (ierr.NE.0) THEN
        subinfo = errstr//' reading EVRATE extension'
        call fcecho(subinfo)
       goto 10 
      ENDIF
      ierr = 0
      call ftgcno(iunit,.false.,'ACCEPTED',colnum,ierr)
      IF (ierr.NE.0) THEN
         subinfo= errstr//' ACCEPTED column not present in EVRATE!'
         call fcecho(subinfo)
         goto 10
      ENDIF
      call ftgcve(iunit,colnum,frow,felem,n_evr,enull,
     &             accepted,anyflg,ierr)
      subinfo = errstr//' ACCEPTED reading column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF
 10   errflg = 0
      call ftclos(iunit,errflg)
      IF ((chatter.GE.20).AND.(ierr.EQ.0)) THEN
        subinfo = ' all data has been succesfully read'
        call fcecho(subinfo)
      ENDIF
      return
      end
c -------------------------------------------------------------------
c     END OF HRIF_RDAT
c -------------------------------------------------------------------


*+HRIF_SAMPLE
c       ----------------------------------------------------------
        subroutine hrif_sample(max_ep,n_ep,mjd_int,mjd_frac,sun_x,
     &  sun_y,sun_z,moon_x,moon_y,moon_z,
     &  lon_east,lat_nort,sat_x,sat_y,sat_z,
     &  gha,alt_sat,ses_ang,est_ang,max_att,n_att,time,
     &  roan_sc,ra_sc,dec_sc,asp_qual,stt_qual,
     &  max_evr,n_evr,ev_time,accepted,livt_cor,
     &  max_hkp,hkp_time,n_hkp,
     &  max_mkf,n_mkf,md,oaccepted,
     &  olivt_cor,hkp_rows,asp_rows,mjdref,ierr,chatter)
c       ----------------------------------------------------------
c --- DESCRIPTION ----
c     This routine sorts through the EPHEM, ASPECT,HKP and EVRATE
c     extensions of and RDF ancillary file, and produces a time-ordered
c     list of usefull information that can be used for screening. This
c     is done by sampling the ASPECT (Attitude) and HKP (Housekeeping)
c     data. The EVRATE (Event rates) values are averaged.
c
c --- PASSED VARIABLES ---
c
      IMPLICIT NONE
      integer ierr,chatter
      integer max_ep,n_ep

      integer mjd_int(*)
      real*8 mjd_frac(*),gha(*)
      real*8 sun_x(*),sun_y(*),sun_z(*)
      real*8 moon_x(*),moon_y(*),moon_z(*)
      real*8 lon_east(*),lat_nort(*),alt_sat(*)
      integer sat_x(*),sat_y(*),sat_z(*)
      real ses_ang(*),est_ang(*)
      integer max_att,n_att
      integer asp_rows(*)
      real*8 time(*),ra_sc(*),dec_sc(*), roan_sc(*)
      integer*2 asp_qual(*),stt_qual(*)
      integer max_hkp,n_hkp
      real*8 hkp_time(*)
      integer hkp_rows(*)
      integer max_evr,n_evr
      real*8 ev_time(*)
      real accepted(*),livt_cor(*)

      real*8 mjdref

c --- OUTPUT VARIABLES ---

      integer max_mkf,n_mkf
      real*8 md(*)
      real oaccepted(*),olivt_cor(*)

c --- CALLED ROUTINES ---
c
c     WTHRIMKF : (ROSAT) Writes MKF FITS extension
c 
c --- AUTHORS/MODIFICATION HISTORY ---
c
c     Rehana Yusaf 1.0.0; Dec 94
c
c Banashree M Seifert (1997, Aug 28) 2.0.0
c      . dimensions declaration is cleaned so that it carries
c        dimensions from calling routine. So replaced by (*) 
c        instead of specifically mentioning the dimension
c--------------------------------------------------------------------
      character(5) version
      parameter (version = '2.0.0')
*-
c ---------------------------------------------------------------------
c LOCAL VARIABLES 

       INTEGER*4  IE, IH,  IO, IA,  
     +      IS, IT1,  
     +          STATUS, 
     +      TOT1, TOT2

        REAL ACC,AACC,LT,ALT
C
        REAL*8 DIFFN, DIFFO
C
        REAL*8 MDJUL, DJULAT, DJULEV, DJULHK,DSCS

        character(30) errstr
        character(70) subinfo

        errstr = ' ERROR : HRIF_SAMPLE '//version//':'
        IF (chatter.GE.10) THEN
          subinfo = ' ... using HRIF_SAMPLE '//version
          call fcecho(subinfo)
        ENDIF
        IA = 0
        IE = 0
        IH = 0
        IS = 0
        djulev = 0.d0
        djulhk = 0.d0
         

C
C  Start the loop over the data
C
C 
        IO = 0
        STATUS = 0
        DO WHILE ((STATUS .EQ. 0) .AND. (IO .LT. n_ep))
            IO = IO + 1

c RY, NOTE: MDJUL is used instead of DJUL for determining approriate
c times for the ASPECT,EVRATE,HKP, and HKSTA data
c  
             md(IO) = mjd_int(IO) + mjd_frac(IO)
             MDJUL = md(IO) 

C
C  Find the look direction.  Search though the ATTITUDE data for
C  a close match.
C  DJULAT = Julian date of observation step
C
            DIFFO = 10000.D0
            DIFFN = 1000.D0
            DO WHILE ((IA .LT. n_att) .AND. (DIFFO .GT. DIFFN))
                DIFFO = DIFFN
                IA = IA + 1
                DSCS = time(IA)
                DJULAT = mjdref + DSCS/86400.D0
                DIFFN = DABS(DJULAT-MDJUL)
            ENDDO
            asp_rows(IO) = IA
            IF (IA.GE.2) THEN 
              IA = IA - 1
            ENDIF
C
C  Sort through the event rate information.  Take the average over the
C  time interval +/- 30 seconds from the orbit entry
C
            DO WHILE ((IE .LT. n_evr) .AND. 
     +                  (DJULEV .LT. MDJUL-3.47222D-4))
                IE = IE + 1
                DSCS = ev_time(IE) 
                DJULEV = mjdref + DSCS/86400.D0
            ENDDO
            IF (IE.GE.2) THEN
              IE = IE - 1
            ENDIF
            AACC = 0
            ALT = 0
            TOT1 = 0
            TOT2 = 0

            DO WHILE ((IE .LT. n_evr) .AND.
     +                  (DJULEV .LT. MDJUL+3.47222D-4))
                IT1 = ev_time(IE)
                DSCS = IT1 
                DJULEV = mjdref + DSCS/86400.D0
                IF(DJULEV .LT. MDJUL+3.47222D-4) THEN
                    IT1 = ev_time(IE)
                    ACC = accepted(IE)
                    LT = livt_cor(IE)
                    ALT = ALT + LT
                    TOT1 = TOT1 + 1
C
                    IF(ACC .GT. 0) THEN
                        TOT2 = TOT2 + 1
                        AACC = AACC + ACC
                    ENDIF
                    IE = IE + 1
                ENDIF
            ENDDO
            IF(TOT1 .GT. 0.) THEN
                ALT = ALT/TOT1
            ELSE
                ALT = 0
            ENDIF
            IF(TOT2 .GT. 0.) THEN
                AACC = AACC/TOT2
            ELSE
                AACC = 0.
            ENDIF
C
C  Sort through the housekeeping information.  
C
            DO WHILE ((IH .LT. n_hkp) .AND. 
     +                  (DJULHK .LT. MDJUL))
                IH = IH + 1
                IT1 = hkp_time(IH)
                DSCS = IT1 
                DJULHK = mjdref + DSCS/86400.D0
            ENDDO
            hkp_rows(IO) = IH
            IF (IH.GE.2) THEN
              IH = IH - 1
            ENDIF

            olivt_cor(IO) = ALT
            oaccepted(IO) = AACC

        ENDDO
C
        n_mkf = IO
        IF (chatter.GE.15) THEN
          subinfo = ' ... Data has been sampled'
          call fcecho(subinfo)
        ENDIF
        return 
        END

c -------------------------------------------------------------------
c       END OF HRIF_SAMPLE
c -------------------------------------------------------------------


*+HRIF_WT
c     ------------------------------------------------------
      subroutine hrif_wt(outfile,telescope,instrume,detnam,
     &              infile,max_mkf,n_mkf,md,sun_x,sun_y,sun_z,
     &              moon_x,moon_y,moon_z,lon_east,lat_nort,
     &              sat_x,sat_y,sat_z,gha,alt_sat,ses_ang,
     &              est_ang,obi_num,max_att,n_att,
     &              asp_qual,stt_qual,roan_sc,
     &              ra_sc,dec_sc,accepted,livt_cor,max_hkp,
     &              temp1,temp2,temp3,hkp_rows,asp_rows,
     &              mjdref,taskname,del_exist,errflg,chatter)
c     ------------------------------------------------------
c
c This routine writes a ROSAT HRI MKF file by calling WTHRIMKF
c
c --- VARIABLES ---
c
      IMPLICIT NONE
      integer max_mkf,n_mkf,errflg,chatter
      character*(*) outfile,telescope,instrume,infile
      character*(*) detnam,taskname 
      real*8 md(*),mjdref
      real*8 sun_x(*),sun_y(*),sun_z(*)
      real*8 moon_x(*),moon_y(*),moon_z(*) 
      real*8 lon_east(*),lat_nort(*)
      integer sat_x(*),sat_y(*), sat_z(*)
      integer*2 obi_num(*)
      real*8 gha(*),alt_sat(*)
      real ses_ang(*),est_ang(*), accepted(*),livt_cor(*) 
      integer max_hkp,max_att,n_att
      real temp1(*),temp2(*),temp3(*)
      integer hkp_rows(*),asp_rows(*)
      integer*2 asp_qual(*),stt_qual(*)
      real*8 roan_sc(*),ra_sc(*),dec_sc(*)

      logical del_exist
c
c --- CALLED ROUTINES ---
c
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (Oct 1994) 1.0.0;
c
c Banashree M Seifert (1997, Aug 28) 2.0.0
c      . dimensions declaration is cleaned so that it carries
c        dimensions from calling routine. So replaced by (*) 
c        instead of specifically mentioning the dimension
c--------------------------------------------------------------------
      character(5) version
      parameter (version = '2.0.0')
*-
c ----------------------------------------------------------------------
c
c LOCALS
c
      character(70) subinfo,errinfo,comms(2),hist(2)
      character(16) extname,hduclas3
      character(30) errstr
      integer ounit,nk_hist,nk_comm,status ,i
c
c --- USER INFO ---
c
      errstr = ' ERROR : HRIF_WT Ver '//version
      IF (chatter.GE.10) THEN
         subinfo = ' ... using HRIF_WT Ver '//version
         call fcecho(subinfo)
      ENDIF
c
c --- OPEN FILE AND WRITE NULL PRIMARY ARRAY ---
c
      call cgetlun(ounit)
      call opnpa(outfile,chatter,ounit,del_exist,errflg)
      IF (errflg.NE.0) THEN
       errinfo = errstr//'opening and writing primary to outfile'
       call fcecho(errinfo)
       goto 100
      ENDIF
c 
c --- WRITE OUTPUT MKF DATA ---
c
      nk_hist = 0
      nk_comm = 0
      status = 0
      extname = ' '
      hduclas3 = ' '
c
c --- Convert from MJD to secs ---
c
      do i=1,n_mkf
        md(i) = (md(i) - mjdref) * 86400.D0
      enddo
      call wthrimkf(ounit,telescope,instrume,detnam,extname,
     &             md,mjdref,hduclas3,n_mkf,sun_x,sun_y,sun_z,
     &             moon_x,moon_y,moon_z,lon_east,lat_nort,
     &             sat_x,sat_y,sat_z,gha,alt_sat,ses_ang,est_ang,
     &             obi_num,asp_qual,stt_qual,roan_sc,ra_sc,dec_sc,
     &             accepted,livt_cor,max_hkp,temp1,temp2,temp3,
     &             hkp_rows,asp_rows,nk_hist,hist,nk_comm,comms,
     &             status,chatter)
      IF (status.NE.0) THEN
        errinfo = errstr//' encountered writing HRI MKF file'
        call fcecho(errinfo)
        errflg = status
      ENDIF
      status = 0
      call ftpdat(ounit,status)
      status= 0
      call FTPKYS(ounit,'CREATOR',
     &                  taskname,
     &             's/w task which wrote this dataset',
     &                  status)
      errinfo = errstr//' problem writing CREATOR keyword'
      IF (chatter.GE.15) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      status = 0
      call FTPKYS(ounit,'INFILE',infile,
     &             'input dataset',status)
      errinfo = errstr//' problem writing INFILE keyword'
      IF (chatter.GE.15) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
 100  status = 0
      call ftclos(ounit,status)
      return
      end
c -------------------------------------------------------------------
c     END OF HRIF_WT
c -------------------------------------------------------------------  
  
 
*+HRIF_DIMS
c     ------------------------------------------------------------
      subroutine hrif_dims(infile,max_ep,max_att,max_hkp,max_evr,
     &                     ierr,chatter)
c     ------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c     This routine determines the array dimensions
c ----------------------------------------------------------------
c --- PASSED VARIABLES ---
c
      IMPLICIT NONE
      character*(*) infile
      integer max_ep,max_att,max_hkp,max_evr,ierr,chatter
c
c --- AUTHORS/MODIFICATION ---------------------------------------
c     Rehana Yusaf 1.0.0; DEC 1994
c
c Banashree M Seifert (1997, Aug 28) 2.0.0
c      . before calling MVER, extnum=-1 instead of extnum=0
c ---------------------------------------------------------------
      character(5) version
      parameter (version = '2.0.0')
c ----------------------------------------------------------------
*-
c --- LOCAL VARIABLES
      character(70) subinfo,errinfo
      character(32) errstr,wrnstr,comm
      integer htype,extnum,status

c --- VARIABLES FOR MVEXT ---

      integer nsearch,ninstr
      parameter (nsearch = 50)
      integer next(nsearch),iunit
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9)
      character(8) extname
c
c --- USER INFO ---
c
      errstr = ' ERROR: HRIF_DIMS Ver '//version//':'
      wrnstr = ' WARNING: HRIF_DIMS Ver '//version//':'
      IF (chatter.GE.10) THEN
         subinfo = ' ... using HRIF_DIMS Ver '//version
         call fcecho(subinfo)
      ENDIF
c
c --- OPEN FILE ---
c

c LOCATE EPHEM EXTENSION ...

      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'EPHEM'
      extname = 'EPHEM'
      call mvext(0,infile,iunit,ninstr,instr,nsearch,next,outhdu,
     &           extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        subinfo = errstr//' problem locating EPHEM extension'
        call fcecho(subinfo)
        goto 10
      ENDIF
      status = 0
      call ftgkyj(iunit,'NAXIS2',max_ep,comm,status)
      errinfo = errstr//' reading NAXIS2 from EPHEM ext'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        goto 10 
      ENDIF

      

c LOCATE ASPECT EXTENSION ...

      ierr = 0
      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'ASPECT'
      extname = 'ASPECT'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        errinfo = errstr//' moving to begining of infile'
        call fcecho(errinfo)
        goto 10
      ENDIF

      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &           outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        errinfo = errstr//' locating ASPECT ext'
        call fcecho(errinfo)
        goto 10
      ENDIF
      status = 0
      call ftgkyj(iunit,'NAXIS2',max_att,comm,status)
      errinfo = errstr//' reading NAXIS2 from ASPECT ext'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        goto 10 
      ENDIF

c LOCATE HKP EXTENSION ...

      ierr = 0
      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'HKP'
      extname = 'HKP'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF

      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &          outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
       errinfo = errstr//' locating HKP ext'
       call fcecho(errinfo)
       goto 10
      ENDIF
      status = 0
      call ftgkyj(iunit,'NAXIS2',max_hkp,comm,status)
      errinfo = errstr//' reading NAXIS2 from HKP ext'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        goto 10 
      ENDIF

c LOCATE EVRATE EXTENSION ...

      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'EVRATE'
      extname = 'EVRATE'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF

      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &          outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        errinfo = errstr//' locating EVRATE ext'
        call fcecho(errinfo)
        goto 10
      ENDIF

      status = 0
      call ftgkyj(iunit,'NAXIS2',max_evr,comm,status)
      errinfo = errstr//' reading NAXIS2 from EVRATE ext'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        goto 10 
      ENDIF

 10   status = 0 
      call ftclos(iunit,status)
      return
      end
c     -------------------------------------------------------------------
c     END OF HRIF_DIMS
c     -------------------------------------------------------------------    
