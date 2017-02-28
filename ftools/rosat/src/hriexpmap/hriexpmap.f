
*+HRIEXPMAP
c     -----------------
      subroutine hriexp 
c     -----------------
c --- DESCRIPTION ---------------------------------------------------
c This task generates an exposure map for a given HRI observation.
c The calibration input(s) is a detector map. The task takes the map
c and rotates it as appropriate for a given observation dataset, and
c sums the results (with approriate weighting factors), thus a 'mean'
c exposure map is produced. The science data inputs are 
c - a GTI (Good Time Interval) file
c - an EVR/LTF (Event Rate) file
c - an Attitude file.
c -------------------------------------------------------------------
c --- CALLED ROUTINES -----------------------------------------------
c
c HEXP_GP     : Gets parameters
c HEXP_RDAT   : Reads data, by calling a seperate routine for 
c              each file
c HEXP_MAP    : This routine makes the calculations
c HEXP_WT     : This writes the exposure map in FITS format
c 
c --- COMPILATION/LINKING -------------------------------------------
c
c FTOOLS - FITSIO,CALLIB
c 
c --- AUTHORS/MODIFICATION ------------------------------------------
c 
c Rehana Yusaf (1994 Sept 29) 1.0.0; Original version, program
c                                   based on PCEXPMAP structure
c Rehana Yusaf (1995 May 31) 1.0.1; use DMA for GTI arrays
c Banashree M Seifert (1996 Sept) 1.1.0:
c     . default dmap file to be caldb
c     . corrected segmentation fault in hexp_map subroutine
c     . replaced fcecho etc by wtinfo etc
c Peter Wilson (1998 Jan 21) 1.1.1:
c     . Fixed WCS keyword problems in hexp_wt
c Peter Wilson (1998 Jun 30) 1.1.2:
c     . Updated for new FCPARS behavior
c Ning  Gan    (1998 Jun 30) 1.1.3
C     . Changed the length of the date_obs and time_obs to 68.
c toliver (1999 May 21) 1.1.4:
c     . Correction in hexp_wt
c ----------------------------------------------------------------------
 
      IMPLICIT NONE
      character(5) version
      parameter (version = '1.1.4')
      character(10) taskname
      parameter (taskname = 'hriexpmap') 
*-
c -------------------------------------------------------------------
c
c --- DYNAMIC MEMORY ALLOCATION --- 
c  the following MEM common block definition is in the system iraf77.inc
c  file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c ------------------------------------------------------------------

c
c --- VARIABLES -----------------------------------------------------
c
      character(80) subinfo,outfil
      integer errflg,chatter,status
      logical gti,clobber         
      character(3) gti_data,form
      real rolof

c VARIABLES RELATED TO DETECTOR MAP ...

      character(120) dmapfil
      integer map_max,chanmin,chanmax,p_tmap
      parameter (map_max=512)
c     DYNAMIC MEMORY ALLOCATED ARRAY - real tmap(map_max,map_max) 

c VARIABLES RELATED TO ATTITUDE DATA ...

      character(120) attfil
      integer max_att,n_att
      integer p_asp_time,p_ra_sc,p_dec_sc,p_roan_sc
      real*8 nom_ra,nom_dec

c VARIABLES RELATED TO EVENT RATE DATA ...

      character(120) evrfil
      integer max_evr,n_evr
      integer p_ev_time,p_livt_cor

c VARIABLES RELATED TO GTI DATA ...

      character(120) gtifil
      integer max_gti,n_gti,p_start,p_stop
c      real*8 start(max_gti),stop(max_gti)

c VARIABLES RELATED TO EXPOSURE MAP ...

      integer p_exparr
c DMA    real*4 exparr(map_max,map_max)
c
c LOCAL VARIABLES FOR ATTITUDE DATA ...
c
      integer max_curatt
      integer p_asp_time_us,p_xoff,p_yoff,p_roll
c
c LOCAL VARIABLES FOR EVENTS DATA ...
c
      integer max_curevr
      integer p_ltf_stime,p_lt_cor

c
c --- INITIALISATION ---
c
      errflg = 0
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c dmapfil      char  : detector map filename
c map_max      int   : array dimension for dmap arrays
c chanmin      int   : Minimum PI range for image
c chanmax      int   : Maximum PI range for image
c tmap         real  : detector map
c attfil       char  : Attitude filename
c max_att      int   : array dimension for attitude data
c n_att        int   : counter for attitude data
c evrfil       char  : Event rate filename
c max_evr      int   : array dimension for event data
c n_evr        int   : counter for event data
c gtifil       char  : gti filename
c max_gti      int   : Array size for gti data
c n_gti        int   : counter for gti data
c start        real*8: array of start times
c stop         real*8: array of stop times
c errflg       int   : Error flag
c chatter      int   : chattiness flag (>20 verbose)
c
 
c
c --- GET PARAMETERS ---
c
      call hexp_gp(attfil,evrfil,gtifil,outfil,
     &             dmapfil,gti,clobber,rolof,chatter,errflg)
      IF (errflg.NE.0) THEN
         goto 100
      ENDIF
c
c --- USER INFO ---
c
      call wtbegm(taskname,version,chatter)
c
c --- DETERMINE DATA FORMAT, US or RDF ---
c
      call hexp_form(attfil,evrfil,gtifil,gti,gti_data,
     &              form,max_att,max_curatt,max_evr,
     &              max_curevr,max_gti,errflg,chatter)
      IF (errflg.NE.0) THEN
        return
      ENDIF
      IF (max_gti.LT.50) THEN
        max_gti = 50
      ENDIF
c
c --- ALLOCATE DYNAMIC MEMORY ---------------------------------------
c
      p_start = 0
      p_stop = 0
      p_tmap = 0 
      p_asp_time = 0
      p_ra_sc = 0
      p_dec_sc = 0
      p_roan_sc = 0
      p_ev_time = 0
      p_livt_cor = 0
      p_exparr = 0
      p_asp_time_us = 0
      p_xoff = 0
      p_yoff = 0
      p_roll = 0
      p_ltf_stime = 0
      p_lt_cor = 0

      status = 0
      call udmget(max_gti,7,p_start,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      status = 0
      call udmget(max_gti,7,p_stop,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      status = 0
      call udmget(map_max*map_max,6,p_tmap,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_att,7,p_asp_time,status)
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
      call udmget(max_att,7,p_roan_sc,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_evr,7,p_ev_time,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_evr,6,p_livt_cor,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(map_max*map_max,6,p_exparr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curatt,7,p_asp_time_us,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curatt,6,p_xoff,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curatt,6,p_yoff,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curatt,6,p_roll,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curevr,7,p_ltf_stime,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curevr,6,p_lt_cor,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
 50   IF (status.NE.0) THEN
        errflg = -1
        subinfo = 'failed to allocate dynamic memory'
        call wterrm(taskname,version,subinfo)
        goto 100
      ENDIF
c
c --- READ DATA ---
c
      call hexp_rdat(dmapfil,map_max,chanmin,chanmax,MEMR(p_tmap),
     & attfil,max_att,n_att,MEMD(p_asp_time),
     & MEMD(p_ra_sc),MEMD(p_dec_sc),
     & MEMD(p_roan_sc),nom_ra,nom_dec,
     & evrfil,max_evr,n_evr,
     & MEMD(p_ev_time),MEMR(p_livt_cor),
     & gtifil,gti,max_gti,n_gti,MEMD(p_start),MEMD(p_stop),
     & max_curatt,MEMD(p_asp_time_us),MEMR(p_xoff),
     & MEMR(p_yoff),MEMR(p_roll),
     & max_curevr,MEMD(p_ltf_stime),
     & MEMR(p_lt_cor),gti_data,form,
     & chatter,errflg)
      IF (errflg.NE.0) THEN
         subinfo='returning from hexp_rdat'
         goto 100 
      ENDIF               
c
c --- EXPOSURE MAP CALCULATIONS ---
c
      call hexp_map(map_max,chanmin,chanmax,MEMR(p_tmap),
     &             max_att,n_att,MEMD(p_asp_time),
     &             MEMD(p_ra_sc),MEMD(p_dec_sc),
     &             MEMD(p_roan_sc),max_evr,n_evr,MEMD(p_ev_time),
     &             MEMR(p_livt_cor),gti,
     &             max_gti,n_gti,MEMD(p_start),MEMD(p_stop),
     &             MEMR(p_exparr),rolof,
     &             chatter,errflg) 
      IF (errflg.NE.0) THEN
         subinfo='returning from hexp_map'
         goto 100 
      ENDIF               
c
c --- WRITE EXPOSURE MAP FILE ---
c
      call hexp_wt(outfil,dmapfil,attfil,evrfil,gtifil,
     &            chanmin,chanmax,map_max,MEMR(p_exparr),
     &            nom_ra,nom_dec,
     &            taskname,clobber,chatter,errflg)
      IF (errflg.NE.0) THEN
         subinfo='returning from hexp_wt'
         goto 100 
      ENDIF               
      

  100 IF (errflg.NE.0) THEN
        call wterrm(taskname,version,subinfo)  
      ENDIF
c
c --- FREE THE DYNAMIC MEMORY ---
c
      status = 0
      call udmfre(p_tmap,6,status)
      status = 0
      call udmfre(p_asp_time,7,status)
      status = 0
      call udmfre(p_ra_sc,7,status)
      status = 0
      call udmfre(p_dec_sc,7,status)
      status = 0
      call udmfre(p_roan_sc,7,status)
      status = 0
      call udmfre(p_ev_time,7,status)
      status = 0
      call udmfre(p_livt_cor,6,status)
      status = 0
      call udmfre(p_exparr,6,status)
      status = 0
      call udmfre(p_asp_time_us,7,status)
      status = 0
      call udmfre(p_xoff,6,status)
      status = 0
      call udmfre(p_yoff,6,status)
      status = 0
      call udmfre(p_roll,6,status)
      status = 0
      call udmfre(p_ltf_stime,7,status)
      status = 0
      call udmfre(p_lt_cor,6,status)
      IF (status.NE.0) THEN
        subinfo = 'failed to de-allocate memory'
        call wterrm(taskname,version,subinfo)
        errflg = 99
      ENDIF      

      call wtendm(taskname,version,errflg,chatter)

      return
      end
c ----------------------------------------------------------------------
c     END OF HRIEXPMAP
c ----------------------------------------------------------------------   


*+HEXP_GP
c     ------------------------------------------------------
      subroutine hexp_gp(attfil,evrfil,gtifil,outfil,
     &                  dmapfil,gti,clobber,rolof,chatter,errflg)
c     ------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c
c This subroutine reads the user defined parameters.
c
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) attfil,evrfil,outfil,dmapfil,gtifil
      integer chatter,errflg
      logical gti,clobber
      real rolof
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c attfil    char    : i/p Attitude filename
c evrfil    char    : i/p Event rate file
c gtifil    char    : i/p Good Time Interval (GTI) filename
c outfil    char    : i/p Output (EXP MAP FILE) filename
c dmapfil   char    : i/p Detector Map file 
c chatter   int     : i/p Chattines flag (>20 verbose)
c errflg    int     : Routine error flag, 0 is ok
c
c --- CALLED ROUTINES --------------------------------------------------
c
c UCLGST     : (HOST) Get string input
c UCLGSI     : (HOST) Get integer input
c FCECHO     : (FTOOLS) Screen write
c FTUPCH     : (FTOOLS/FITSIO) converts string to upper case
c 
c --- AUTHORS/MODIFICATION --------------------------------------------- 
c
c Rehana Yusaf (1993 Sept 30)
c Rehana Yusaf (1994 21 Mar) 1.0.1; Allow for extensions at the end
c                                   of filenames.
c Rehana Yusaf (1994 29 Mar) 1.0.2; Allow GTIFIL 'NONE' or ' '
c
c Banashree M Seifert (1996 Sept) 1.1.0:
c         . default caldb for dmap file
c           introduced gtcalf
c         . wtinfo etc are introduced
c     Peter D Wilson  (1998 June) 1.1.1
c         . Strip out INQUIRE calls for FITS files
c ------------------------------------------------------------------

      character(5) version
      parameter (version = '1.1.1')
*-
c ----------------------------------------------------------------------
c
c INTERNALS ...
c
      character(80) subinfo
      character(180) filename,ill_files(6),temp
      character(132) fileinfo
      integer status,n_ill,extnum
      logical ext,valfil
      character(8) telescop,instrume
      character(20) comm
      integer iunit,blocksize
      character(68) obs_date, obs_time
      character(80) online
      integer extno,nfound,nret

      character(8) subname
      parameter (subname='hexp_gp')
 
c 
c --- USER INFO ---
c
      subinfo='using '//subname//' Ver '//version
      call wtinfo(9,10,2,subinfo)

c
c --- GET EVENT FILENAME ---
c
      status = 0
      call uclgst('ltffil',evrfil,status)
      IF (status.NE.0) THEN
        subinfo = 'getting evrfil parameter !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF
      call crmvlbk(evrfil)
C PDW 6/30/98: Don't use INQUIRE. Call FTRTNM to strip off extension
      call ftrtnm( evrfil, filename, status )
      ill_files(1) = filename
C      call fcpars(evrfil,filename,extnum,status)
C      ill_files(1) = filename
C      ext = .true.
C      INQUIRE(FILE=filename,EXIST=ext)
C      IF ((.NOT.ext).OR.(filename.EQ.'  ')) THEN
C        subinfo = 'Event file does not exist '
C        call wterrm(subname,version,subinfo)
C        errflg = 1
C        return
C      ENDIF                    

c
c --- GET ATTITUDE FILENAME ---
c
      status = 0
      call uclgst('attfil',attfil,status)
      IF (status.NE.0) THEN
        subinfo = 'getting attfil parameter !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF
      call crmvlbk(attfil)
      IF (attfil.EQ.'%') THEN
        attfil = filename 
      ELSE
C PDW 6/30/98: Don't use INQUIRE. Call FTRTNM to strip off extension
        call ftrtnm( attfil, filename, status )
        ill_files(2) = filename
C        call fcpars(attfil,filename,extnum,status)
C        ill_files(2) = filename
C        INQUIRE(FILE=filename,EXIST=ext)
C        IF ((.NOT.ext).OR.(filename.EQ.'  ')) THEN
C         subinfo = 'Attitude file does not exist '
C         call wterrm(subname,version,subinfo)
C         errflg = 1
C         return
C        ENDIF         
      ENDIF
c
c --- GET GTI FILENAME ---
c
      status = 0
      gti = .false.
      call uclgst('gtifil',gtifil,status)
      IF (status.NE.0) THEN
        subinfo = 'getting gtifil parameter !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF          
      call crmvlbk(gtifil)
      temp = gtifil
      call ftupch(temp)
      IF ((temp.NE.'NONE').AND.(temp.NE.'  ')) THEN
C PDW 6/30/98: Don't use INQUIRE. Call FTRTNM to strip off extension
        call ftrtnm( gtifil, filename, status )
        ill_files(3) = filename
C        call fcpars(gtifil,filename,extnum,status)
C        INQUIRE(FILE=filename,EXIST=ext)
C        ill_files(3) = filename
C        IF (.NOT.ext) THEN
C         subinfo = 'GTIFIL file does not exist '
C         call wterrm(subname,version,subinfo)
C         errflg = 1
C         return
C        ENDIF      
        gti = .true.
      ENDIF 
c
c --- GET DETECTOR MAP FILE, IF QUZCIF IS FALSE ---
c
      status = 0
      call uclgst('dmapfil',dmapfil,status)
      IF (status.NE.0) THEN
        subinfo = 'getting dmapfil parameter !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF           

      if((dmapfil(1:5) .eq. 'caldb') .or.
     >             (dmapfil(1:5) .eq. 'CALDB')) then
         call ftgiou(iunit,status)
         call ftopen(iunit,evrfil,0,blocksize,status)
         call ftgkys(iunit,'TELESCOP',telescop,comm,status)
         call ftgkys(iunit,'INSTRUME',instrume,comm,status)
         call ftgkys(iunit,'DATE-OBS',obs_date,comm,status)
         call ftgkys(iunit,'TIME-OBS',obs_time,comm,status)

         call gtcalf(30,telescop,instrume,'-','-',
     >           'BRIGHT_EARTH_MAP',obs_date,obs_time,obs_date,obs_time,
     >           '-',1,dmapfil,extno,online,nret,nfound,status)

         call ftclos(iunit,status)
         call ftfiou(iunit,status)
      endif

      call crmvlbk(dmapfil)
C PDW 6/30/98: Don't use INQUIRE. Call FTRTNM to strip off extension
      call ftrtnm( dmapfil, filename, status )
      ill_files(4) = filename
C      call fcpars(dmapfil,filename,extnum,status)
C      ill_files(4) = filename
C      INQUIRE(FILE=filename,EXIST=ext)
C      IF ((.NOT.ext).OR.(filename.EQ.'  ')) THEN
C        subinfo = 'DMAP file does not exist '
C        call wterrm(subname,version,subinfo)
C        errflg = 1
C        return
C      ENDIF        

c
c --- GET OUTFILE NAME ---
c
        call uclgst('outfil',outfil,status)
        IF (status.NE.0) THEN
          subinfo = 'Getting outfile parameter'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
        ENDIF
        call crmvlbk(outfil)
        IF (outfil.EQ.'  ') THEN
          subinfo = 'output filename must be entered !'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
        ENDIF

c read clobber

        call uclgsb('clobber',clobber,status)
        IF (status.NE.0) THEN
           subinfo = 'getting clobber parameter'
           call wterrm(subname,version,subinfo)
           errflg = 1
           return
        ENDIF

c check outfil validity

      valfil = .true.
      n_ill = 4
      call ck_file(outfil,ill_files,n_ill,valfil,clobber,chatter)
      IF (.NOT.valfil) THEN
        subinfo = 'outfil is not valid !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF 
c
c --- GET ROLOF ---
c
      status = 0
      call uclgsr('rolof',rolof,status)
      IF (status.NE.0) THEN
        subinfo = 'getting chatter parameter !'
        call wtwarm(subname,version,9,9,subinfo)
        rolof = 0
      ENDIF
c
c --- GET CHATTER PARAMETER ---
c
      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        subinfo = 'getting chatter parameter !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF               
      return
      end
c ----------------------------------------------------------------------
c     END OF HEXP_GP
c ----------------------------------------------------------------------      


*+HEXP_RDAT
c     -------------------------------------------------------------
      subroutine hexp_rdat(dmapfil,map_max,chanmin,chanmax,tmap,
     &              attfil,max_att,n_att,asp_time,ra_sc,
     &              dec_sc,roan_sc,nom_ra,nom_dec,
     &              evrfil,max_evr,n_evr,ev_time,livt_cor,
     &              gtifil,gti,max_gti,n_gti,start,stop,
     &              max_curatt,asp_time_us,xoff,yoff,
     &              roll,max_curevr,ltf_stime,
     &              lt_cor,gti_data,form,chatter,errflg)
c     -------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c This subroutine reads the data inputs for the HRIEXPMAP task. It reads
c the calibration data  - Detector Map file
c and science data      - a GTI (Good Time Interval) file
c                       - an EVR (Event Rate) file                        
c                       - an Attitude file
c
c ----------------------------------------------------------------------
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      integer errflg,chatter
      logical gti
 
c VARIABLES RELATED TO DETECTOR MAP ...

      character*(*) dmapfil
      integer map_max,chanmin,chanmax
      real*4 tmap(map_max,map_max)

c VARIABLES RELATED TO ATTITUDE DATA ...

      character*(*) attfil
      integer max_att,n_att
      real*8 asp_time(max_att),ra_sc(max_att)
      real*8 dec_sc(max_att), roan_sc(max_att)
      real*8 nom_ra,nom_dec

c VARIABLES RELATED TO EVENT RATE DATA ...

      character*(*) evrfil
      integer max_evr,n_evr
      real*8 ev_time(max_evr)
      real livt_cor(max_evr)

c VARIABLES RELATED TO GTI DATA ...

      character*(*) gtifil
      integer*4 max_gti,n_gti
      real*8 start(max_gti),stop(max_gti)       

c --- CALLED ROUTINES -----------------------------------------------
c
c RHATU0      : Reads US REV0 format Attitude file
c RHATRD      : Reads RDF format ATTITUDE file
c RHMVU0      : Reads US REV0 format EVR file
c RHMVRD      : Reads RDF format EVR file
c RDGTI1      : Reads GTI file
c RDETM1      : Reads DETECTOR MAP file
c FTOPEN      : (FITSIO) Opens FITS file
c FTCLOS      : (FITSIO) Closes FITS file
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf 	    1.0.0; Aug 30 1994 
c
c Banashree M Seifert (Sept 1996) 1.1.0:
c           . replaced fcecho etc with wterrm,wtimfo etc
c Peter Wilson (January 1998) 1.1.1:
c           . return nominal ra and dec values for use in map
c -------------------------------------------------------------------

      character(5) version
      parameter (version='1.1.1')
      character(10) subname 
      parameter (subname='hexp_rdat')
*-
c -------------------------------------------------------------------
c INTERNALS ...

      character(70) subinfo,errinfo
      character(120) filename
      character(16) dmapinstr,dmaptlscop
      character(3) form,gti_data
      integer block,status,iunit
      real*8 pi

c LOCAL VARIABLES FOR READING ATTITUDE DATA ...

      integer n_curatt,i,max_curatt
      real*8 asp_time_us(max_curatt)
      real*8 nom_roll,cos_dec,n_sin_roll
      real*8 n_cos_roll,ra_sc1,dec_sc1
      real yoff(max_curatt),roll(max_curatt),xoff(max_curatt)
      logical end_attdata,end_evr


c LOCAL VARIABLES FOR READING EVENT RATE DATA

      integer max_curevr,n_curevr
      real*8 ltf_stime(max_curevr)
      real lt_cor(max_curevr)

c VARIABLES FOR MVEXT  

      integer nsearch,extnum,ninstr
      parameter (nsearch = 50)
      integer next(nsearch)
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(9,nsearch) ,instr(9)
      character(8) extname
      
c
c --- USER INFO ---
c
        subinfo = 'using '//subname//' Ver '//version
        call wtinfo(chatter,10,2,subinfo)
     
c MJT 05July96 -- fixing incorrect value for PI!!!!!
c      pi = 3.14592654
      pi = 3.141592654
c
c --- READ DETECTOR MAP FILE ---
c
      call rdetm1(dmapfil,map_max,dmapinstr,dmaptlscop,
     &                  chanmin,chanmax,tmap,chatter,errflg)
      IF (errflg.NE.0) THEN
        errinfo = 'fatal error reading DMAP file'
        call wterrm(subname,version,errinfo)
        return
      ENDIF
c
c --- READ ATTITUDE FILE ---
c
      IF (form.EQ.'US') THEN
        call fcpars(attfil,filename,extnum,status)
        call cgetlun(iunit)
        status = 0
        call ftopen(iunit,filename,0,block,status)
        errinfo = 'opening Attitude file !'
        call wtferr(subname,version,status,errinfo)
        IF (status.NE.0) THEN
         errflg = 1
         return
        ENDIF
        n_att = 0
        end_attdata = .false.
        do WHILE(.NOT.end_attdata)
          call rhatu0(iunit,n_curatt,max_curatt,asp_time_us,xoff,
     &                yoff,roll,chatter,errflg)
          IF ((errflg.EQ.107).OR.(errflg.EQ.207)) THEN
            end_attdata = .true.
            errflg = 0
          ELSEIF (errflg.NE.0) THEN
            errinfo = 'fatal error reading Attitude file'
            call wterrm(subname,version,subinfo)
            return     
          ENDIF
c
c --- IMG - I made this change in drunken state on the night of Oct 07
c
          IF (.NOT.end_attdata) THEN
           do i=1,n_curatt
            asp_time(i+n_att)= asp_time_us(i)
            ra_sc(i+n_att)   = xoff(i)
            dec_sc(i+n_att)  = yoff(i)
            roan_sc(i+n_att) = roll(i) 
           enddo
           n_att = n_att + n_curatt   
          ENDIF
        enddo
      ELSEIF (form.EQ.'RDF') THEN
        ninstr = 2
        instr(1) = 'TEMPORALDATA'
        instr(2) = 'ASPECT'
        call mvext(0,attfil,iunit,ninstr,instr,nsearch,next,outhdu,
     &             extnames,outver,'ASPECT',errflg,chatter)
        IF (errflg.NE.0) THEN
          return
        ENDIF
        call rhatrd(iunit,n_att,max_att,asp_time,
     &             ra_sc,dec_sc,roan_sc,nom_ra,nom_dec,
     &             nom_roll,chatter,errflg) 
        IF (errflg.NE.0) THEN
            errinfo = 'fatal error reading Attitude file'
            call wterrm(subname,version,subinfo)
            return
        ENDIF                      

c FOR RDF DATA, CONVERT TO REV0 FORMAT THAT HEXP_MAP EXPECTS
c MJT 05July96 converting to radian (std) versions of dcos,dsin

c     cos_dec = DCOSD(nom_dec)
c     n_sin_roll = DSIND(-1*nom_roll)
c     n_cos_roll = DCOSD(-1*nom_roll)

      cos_dec = DCOS(nom_dec*(pi/180))
      n_sin_roll = DSIN(-1*nom_roll*(pi/180))
      n_cos_roll = DCOS(-1*nom_roll*(pi/180))

c Convert Roll angle to radians
c Convert RA and DEC to pixels
c Also rotate RA and DEC by negative the nom_roll angle

      do i=1,n_att
        roan_sc(i) = roan_sc(i) * (pi/180)
        ra_sc1 = ((nom_ra - ra_sc(i)) * cos_dec) * 7200
        dec_sc1 = (nom_dec - dec_sc(i)) * 7200
        ra_sc(i) = ra_sc1 * n_cos_roll - dec_sc1 * n_sin_roll
        dec_sc(i) = ra_sc1 * n_sin_roll + dec_sc1 * n_cos_roll
      enddo

      ENDIF

      status = 0  
      call ftclos(iunit,status)
      errinfo = 'closing Attitude file'
      call wtferr(subname,version,status,errinfo)
c
c --- READ EVENT RATE FILE ---
c
      IF (form.EQ.'US') THEN
       call fcpars(evrfil,filename,extnum,status)
       call cgetlun(iunit)
       status = 0
       call ftopen(iunit,filename,0,block,status)
       errinfo = 'opening Event Rate file !'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg = 1
         return
       ENDIF
       end_evr = .false.
       n_evr = 0
       do WHILE (.NOT.end_evr)             
        call rhltu0(iunit,n_curevr,max_curevr,ltf_stime,
     &                lt_cor,chatter,errflg)
        IF ((errflg.EQ.107).OR.(errflg.EQ.207)) THEN
          end_evr = .true.
          errflg = 0
        ELSEIF (errflg.NE.0) THEN
          errinfo = 'fatal error reading Event Rate file'
          call wterrm(subname,version,errinfo)
          return
        ENDIF
        IF (.NOT.end_evr) THEN
          do i=1,n_curevr
            ev_time(i+n_evr) = ltf_stime(i)
            livt_cor(i+n_evr) = lt_cor(i)
          enddo
          n_evr = n_evr + n_curevr
        ENDIF
       enddo
      ELSEIF (form.EQ.'RDF') THEN
       ninstr = 2
       instr(1) = 'TEMPORALDATA'
       instr(2) = 'EVRATE'
       call mvext(0,attfil,iunit,ninstr,instr,nsearch,next,outhdu,
     &            extnames,outver,'EVRATE',errflg,chatter)
       IF (errflg.NE.0) THEN
         return
       ENDIF
       call rhmvrd(iunit,n_evr,max_evr,ev_time,
     &             livt_cor,chatter,errflg)     
      ENDIF
      IF (errflg.NE.0) THEN
        return
      ENDIF
      status = 0                       
      call ftclos(iunit,status)
      errinfo = 'closing Event Rate file'
      call wtferr(subname,version,status,errinfo)       
c
c --- READ GTI FILE ---
c
      IF (gti) THEN
      IF (gti_data.EQ.'US') THEN
        extname = 'GTI'
        ninstr = 1
        instr(1) = '*'
      ELSEIF (gti_data.EQ.'RDF') THEN
        extname = 'STDGTI'
        ninstr = 2
        instr(1) = 'GTI' 
        instr(2) = 'STANDARD' 
      ENDIF
      call mvext(0,gtifil,iunit,ninstr,instr,nsearch,next,outhdu,
     &           extnames,outver,extname,errflg,chatter)
      IF (errflg.NE.0) THEN
        return
      ENDIF
      call rdgti1(iunit,n_gti,max_gti,start,stop,chatter,errflg)
      IF (errflg.NE.0) THEN
        errinfo = 'fatal error reading Event Rate file'
        call wterrm(subname,version,errinfo)
        return
      ENDIF
      status = 0
      call ftclos(iunit,status)
      errinfo = 'closing GTI file'
      call wtferr(subname,version,status,errinfo)
      ELSE
        n_gti = 1
        start(1) = ev_time(1)
        stop(1) = ev_time(n_evr) + 1
      ENDIF

      subinfo = 'all data has been read'
      call wtinfo(chatter,20,1,subinfo)

      return
      end
c ------------------------------------------------------------------
c     END OF HEXP_RDAT
c ------------------------------------------------------------------


*+HEXP_WT
c     -----------------------------------------------------------------
      subroutine hexp_wt(outfil,dmapfil,attfil,evrfil,gtifil,
     &                  chanmin,chanmax,map_max,exparr,
     &                  nom_ra,nom_dec,
     &                  taskname,clobber,chatter,errflg)
c     -----------------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c This subroutine writes an exposure map FITS file.
c
c ---------------------------------------------------------------------
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) outfil,dmapfil,attfil,evrfil,gtifil
      character*(*) taskname
      integer map_max,errflg,chatter
      real*4 exparr(map_max,map_max)
      real*8 nom_ra,nom_dec
      integer chanmin,chanmax
      logical clobber
c
c --- CALLED ROUTINES -------------------------------------------------
c
c WTEXM1   : (CALLIB) Writes Exposure Map data 
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1993 Oct 5) 1.0.0 ;
c Rehana Yusaf (Nov 12 1993) 1.0.1; Use WTEXM1.F to write exposure map
c                                   instead of WT_EXPM1993a
c                                   write chanmin and chanmax in this
c  			            subroutine as well as all the input
c				    files.
c
c Banashree M Seifert (Sept, 1996) 1.1.0:
c            . fcecho,wt_ferrmsg etc are replaced by wtinfo,wterrm etc.
c Peter Wilson (Januaray 1998) 1.1.1:
c            . fixed cdelt values (should be 5"/pixel)
c            . Use nominal RA and DEC values for crval WCS keywords
c toliver (May, 1999) 1.1.2:
c            . corrected cdelt1 (value should be negative as RA decreases
c              from left to right)
c ----------------------------------------------------------------------
      character(8) subname
      parameter (subname='hexp_wt')
      character(5) version
      parameter (version = '1.1.2')
*-
c ---------------------------------------------------------------------
c
c INTERNALS ...
c
      integer ounit,status,clenact,nk_hist,nk_comm
      character(70) hist(2),comments(2)
      character(8) telescop,detnam,filter,radecsys,instrum
      character(8) cunit1,cunit2,ctype1,ctype2,bunit
      real equinox,crval1,crval2,cdelt1,cdelt2,crpix1,crpix2
      logical deadapp,vignapp
      character(70) subinfo
c
c --- USER INFO
c
        subinfo = 'using '//subname//' Ver '//version
        call wtinfo(chatter,10,2,subinfo)
c
c --- OPEN NEW FILE ---
c
      status = 0
      call cgetlun(ounit)
      call opfits(ounit,outfil,clobber,chatter,status)
      subinfo = 'opening output file !'
      call wtferr(subname,version,status,subinfo)
      IF (status.NE.0) THEN
        errflg = 1
        return
      ENDIF
c
c --- SET VALUES THAT ARE FIXED FOR THIS TASK ---
c
      telescop = 'ROSAT'
      detnam = ' '
      filter = ' '
      deadapp = .true.
      vignapp = .true.
      radecsys = 'FK5'
      equinox = 2.00000e+3
      ctype1 = 'RA---TAN'
      ctype2 = 'DEC--TAN'
      crpix1 = 256
      crpix2 = 256
      cunit1 = 'deg'
      cunit2 = 'deg'
      bunit =  's'
      crval1 = nom_ra
      crval2 = nom_dec
      cdelt1 = -0.00138889
      cdelt2 =  0.00138889
      bunit = 's'
                
c
c --- CALL ROUTINE WHICH WRITES DATA AND HEADER ---
c
      nk_hist = 0
      nk_comm = 0
      instrum = 'HRI'
      call wtexm1(ounit,telescop,instrum,detnam,filter,
     &           deadapp,vignapp,radecsys,equinox,ctype1,
     &           ctype2,crpix1,crpix2,cunit1,cunit2,bunit,
     &           crval1,crval2,cdelt1,cdelt2,
     &           map_max,exparr,nk_hist,hist,nk_comm,comments,
     &           chatter,errflg) 
      
c Write additional keywords ...

      status = 0
      call ftpkyj(ounit,'CHANMIN',chanmin,
     &'Minumum PI channel for image',status)
      subinfo = 'writing CHANMIN keyword'
      call wtferr(subname,version,status,subinfo)      
 
      status = 0
      call ftpkyj(ounit,'CHANMAX',chanmax,
     &'Maximum PI channel for image',status)
      subinfo = 'writing CHANMAX keyword'
      call wtferr(subname,version,status,subinfo)   

      status = 0
      call ftpkys(ounit,'GTIFILE',gtifil(1:clenact(gtifil)),
     &'Accepted time intervals file',status)
      subinfo = 'writing GTIFILE keyword'
      call wtferr(subname,version,status,subinfo)

      status = 0
      call ftpkys(ounit,'EVRFILE',evrfil(1:clenact(evrfil)),
     &'Events rate file',status)
      subinfo = 'writing EVRFILE keyword'
      call wtferr(subname,version,status,subinfo)

      status = 0
      call ftpkys(ounit,'ASPFILE',attfil(1:clenact(attfil)),
     &'Aspect offsets file',status)
      subinfo = 'writing ASPFILE keyword'
      call wtferr(subname,version,status,subinfo)

      status = 0
      call ftpkys(ounit,'DETMFILE',dmapfil(1:clenact(dmapfil)),
     &'Detector map file',status)
      subinfo = 'writing DETMFILE keyword'
      call wtferr(subname,version,status,subinfo)

      status = 0
      call ftpdat(ounit,status)
      status = 0
      call FTPKYS(ounit,'CREATOR',
     &                  taskname,
     &             's/w task which wrote this dataset',
     &                  status)
      subinfo = 'problem writing CREATOR keyword'
      call wtinfo(chatter,10,1,subinfo)

      status = 0
      call ftclos(ounit,status)
      IF (status.NE.0) THEN
        subinfo = 'closing output file !'
        call wtinfo(chatter,10,1,subinfo)
        errflg  = 1
      ENDIF
      return
      end
c ----------------------------------------------------------------------
c     END OF HEXP_WT 
c ----------------------------------------------------------------------

*+HEXP_FORM
c     ______________________________________________________
      subroutine hexp_form(attfil,evrfil,gtifil,gti,gti_data,
     &              form,max_att,max_curatt,max_evr,
     &              max_curevr,max_gti,errflg,chatter)
c     ______________________________________________________
c ___ DESCRIPTION ______________________________________________
c This routine determines which file format is read, that is
c US REV0 or RDF for HRIEXPMAP
c ___ VARIABLES ________________________________________________ 
c
      IMPLICIT NONE
      character*(*) attfil,evrfil,gtifil,form,gti_data
      integer errflg,chatter,max_gti
      integer max_att,max_curatt,max_evr,max_curevr
      logical gti
c 
c ___ AUTHORS/MODIFICATION _____________________________________ 
c 
c Rehana Yusaf Aug 29 1994 1.0.0; based on exp_form 
c                                 from PCEXPMAP
c Rehana Yusaf (May 31 1995) 1.0.1; max_gti passed to this array
c
c Banashree M Seifert (Sept, 1996) 1.1.0:
c            . fcecho,wt_ferrmsg etc are replaced by wtinfo,wterrm etc.
c ----------------------------------------------------------------------
      character(10) subname
      parameter (subname='hexp_form')
      character(5) version
      parameter (version='1.1.0')
*-
c ______________________________________________________________ 
c
c --- LOCALS ---
c
      integer iunit,block,rwmode,htype,i
      character(30) comm
      character(80) subinfo,errinfo
      character(120) filename 
      character(3) att_data,evr_data
      character(20) extname(50),outver(9,50),outhdu(9,50)
      integer nsearch,nfound,next(50),extnum,rows
c     
c --- USER INFO ---
c
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)
c
c --- OPEN AND READ ATTITUDE ---
c
      block = 2880
      rwmode = 0
      call fcpars(attfil,filename,extnum,errflg)
      call cgetlun(iunit)
      call ftopen(iunit,filename,rwmode,block,errflg)
      errinfo = 'opening ATTITUDE FILE '
      call wtferr(subname,version,errflg,errinfo)
      IF (errflg.NE.0) THEN
        return
      ENDIF
c 
c --- SEARCH ATTITUDE FILE FOR US DATA EXTNAME ---
c
      nsearch = 50
      call fndext(chatter,iunit,'AO',nsearch,nfound,
     &            next,outhdu,outver,extname,errflg)
      IF (nfound.GT.0) THEN
        att_data = 'US'
      ELSE
c
c --- IF NOT FOUND SEARCH FOR RDF DATA FORMAT ---
c
        call fndext(chatter,iunit,'ASPECT',nsearch,nfound,
     &              next,outhdu,outver,extname,errflg)
        IF (nfound.GT.0) THEN
          att_data='RDF'
        ELSE
          errinfo = 'unsupported format for attitude file'
          call wterrm(subname,version,errinfo)
          errflg = 2
          return
        ENDIF
      ENDIF


c DETERMINE MAX ARRAY SIZES FOR AO DATA

      max_att = 0
      max_curatt = 1
      do i=1,nfound
        call ftmahd(iunit,next(i)+1,htype,errflg)
        call ftgkyj(iunit,'NAXIS2',rows,comm,errflg)
        errinfo = 'problem reading NAXIS2 value'
        call wtferr(subname,version,errflg,errinfo)
        IF (errflg.NE.0) THEN
          return
        ENDIF
        max_att = max_att + rows
        IF (rows.GT.max_curatt) THEN
          max_curatt = rows
        ENDIF
      enddo
      max_att = max_att + 2
      max_curatt = max_curatt + 2

      call ftclos(iunit,errflg)
      errinfo = 'closing ATTITUDE FILE'
      call wtferr(subname,version,errflg,errinfo)

       write(errinfo,'(a,i12)')'Max AO array dimension:'
     & ,max_att
       call wtinfo(chatter,30,3,errinfo)
       write(errinfo,'(a,i12)')'Max AO local array size:'
     & ,max_curatt
       call wtinfo(chatter,30,3,errinfo)
c
c --- OPEN AND READ EVENTS FILE ---
c
      call fcpars(evrfil,filename,extnum,errflg)
      call cgetlun(iunit)
      call ftopen(iunit,filename,rwmode,block,errflg)
      errinfo = 'opening EVENTS FILE '
      call wtferr(subname,version,errflg,errinfo)
      IF (errflg.NE.0) THEN
        return
      ENDIF               
c
c --- DETERMINE EVENTS FILE FORMAT ---
c
      nsearch = 50
      nfound = 0
      call fndext(chatter,iunit,'LTF',nsearch,nfound,
     &            next,outhdu,outver,extname,errflg)
      IF (nfound.GT.0) THEN
        evr_data = 'US'
      ELSE
c
c --- IF NOT FOUND SEARCH FOR RDF DATA FORMAT ---
c
        call fndext(chatter,iunit,'EVRATE',nsearch,nfound,
     &              next,outhdu,outver,extname,errflg)
        IF (nfound.GT.0) THEN
          evr_data='RDF'
        ELSE
          errinfo = 'unsupported format for events file'
          call wterrm(subname,version,errinfo)
          errflg = 2
          return
        ENDIF
      ENDIF

c DETERMINE MAX ARRAY SIZES FOR LTF/EVENTS DATA

      max_evr = 0
      max_curevr = 1
      do i=1,nfound
        call ftmahd(iunit,next(i)+1,htype,errflg)
        call ftgkyj(iunit,'NAXIS2',rows,comm,errflg)
        errinfo = 'problem reading NAXIS2 value'
        call wtferr(subname,version,errflg,errinfo)
        IF (errflg.NE.0) THEN
          return
        ENDIF
        max_evr = max_evr + rows
        IF (rows.GT.max_curevr) THEN
          max_curevr = rows
        ENDIF
      enddo
      max_evr = max_evr + 2
      max_curevr = max_curevr + 2

      call ftclos(iunit,errflg)
      errinfo = 'closing EVENTS FILE'
      call wtferr(subname,version,errflg,errinfo)        
       write(errinfo,'(a,i12)')'Max LTF/EVRATE array dimension:'
     & ,max_evr
       call wtinfo(chatter,30,3,errinfo)
       write(errinfo,'(a,i12)')'Max LTF/EVRATE local array size:'
     & ,max_curevr
       call wtinfo(chatter,30,3,errinfo)
c
c --- OPEN AND READ GTI FILE ---
c
      IF (gti) THEN
      call fcpars(gtifil,filename,extnum,errflg)
      call cgetlun(iunit)
      call ftopen(iunit,filename,rwmode,block,errflg)
      errinfo = 'opening GTI FILE '
      call wtferr(subname,version,errflg,errinfo)
      IF (errflg.NE.0) THEN
        return
      ENDIF
c
c --- DETERMINE GTI FILE FORMAT ---
c
      nsearch = 50
      nfound = 0
      call fndext(chatter,iunit,'GTI',nsearch,nfound,
     &            next,outhdu,outver,extname,errflg)
      IF (nfound.GT.0) THEN
        gti_data = 'US'
      ELSE
c
c --- IF NOT FOUND SEARCH FOR RDF DATA FORMAT ---
c
        call fndext(chatter,iunit,'STDGTI',nsearch,nfound,
     &              next,outhdu,outver,extname,errflg)
        IF (nfound.GT.0) THEN
          gti_data='RDF'
        ELSE
          errinfo = 'unsupported format for GTI file'
          call wterrm(subname,version,errinfo)
          errflg = 2
          return
        ENDIF
      ENDIF
      errflg = 0
      max_gti = 0
      call ftmahd(iunit,next(1)+1,htype,errflg)
      call ftgkyj(iunit,'NAXIS2',rows,comm,errflg)
      errinfo = 'problem reading NAXIS2 value'
      call wtferr(subname,version,errflg,errinfo)
      IF (errflg.NE.0) THEN
         return
      ENDIF
      max_gti = max_gti + rows
      call ftclos(iunit,errflg)
      errinfo = 'closing GTI FILE'
      call wtferr(subname,version,errflg,errinfo)                              
      ENDIF
c
c --- CHECK THAT ALL FILES HAVE THE SAME FORMAT ---
c
      form = att_data
      IF (evr_data.NE.form) THEN
        errinfo = 'Format mismatch !'
        call wtinfo(chatter,0,2,errinfo)
        errinfo = ' ATTITUDE DATA : '//att_data
        call wtinfo(chatter,0,2,errinfo)
        errinfo = ' EVENTS DATA : '//evr_data
        call wtinfo(chatter,0,2,errinfo)

        IF (gti) THEN
          errinfo = ' GTI DATA : '//gti_data
          call wtinfo(chatter,0,2,errinfo)
        ENDIF
        errflg = 3
      ENDIF
        IF (errflg.EQ.0) THEN
          subinfo = 'Files are in '//form//' format'
          call wtinfo(chatter,20,3,subinfo)
        ENDIF
      return
      end
c ---------------------------------------------------------------------   
c     END OF EXP_FORM
c ---------------------------------------------------------------------

      
*+HEXP_MAP
c    -----------------------------------------------------
       subroutine hexp_map(map_max,chanmin,chanmax,tmap,
     &             max_att,n_att,asp_time,
     &             ra_sc,dec_sc,
     &             roan_sc,max_evr,n_evr,ev_time,
     &             livt_cor,gti,
     &             max_gti,n_gti,start,stop,exparr,roloff,
     &             chatter,errflg)
c    -----------------------------------------------------
c --- VARIABLES ---
c
      IMPLICIT NONE
      integer errflg,chatter
      logical gti
      real roloff

c VARIABLES RELATED TO DETECTOR MAP

      integer map_max,chanmin,chanmax
      real tmap(map_max,map_max)

c VARIABLES RELATED TO ATTITUDE DATA ...

      integer max_att,n_att
      real*8 asp_time(max_att),ra_sc(max_att) 
      real*8 dec_sc(max_att),roan_sc(max_att)

c VARIABLES RELATED TO EVENTS DATA ...

      integer max_evr,n_evr
      real*8 ev_time(max_evr)
      real livt_cor(max_evr)

c VARIABLES RELATED TO GTI DATA ...

      integer max_gti,n_gti
      real*8 start(max_gti),stop(max_gti)

c VARIABLES RELATED TO EXPOSURE MAP ...

      real exparr(map_max,map_max)

c --- DESCRIPTION --------------------------------------------------
c 
c This subroutine is a modification of Steve Snowden's CAST_EXP_HRI
c code. NOTE : Only the i/o has been changed slightly, that is the
c the data is passed to this routine in arrays in the orginal it
c was read row by row. CAST_EXP_HRI description ...
c
C      PROGRAM CAST_EXP_HRI
C
C  Author: Steve Snowden
C  Date:   18 December 1992
C  Update: 31 October 1993
C
C  Program CAST_EXP_HRI is used to cast the exposure maps for pointed 
C  observations of the HRI.  Presently, it uses a detector map created 
C  from in-orbit data from bright-Earth pointings.  
C
C  This program has been updated to read the required data directly
C  from the observation data set fits files, ATTITUDE and LIVETIME,
C  with FITSIO, except for the accepted time intervals.  It requires 
C  an accepted time file ACTIME.DAT.  This file can be created from 
C  the accepted times given in the observation data set but I suggest 
C  verifying the times by hand.  The program has also been modified to
C  read both German and US data.
C
C  The input files are expected to be named *_ATTITUDE.FITS and
C  *_LIVETIME.FITS where the * is a character string up to eight
C  characters long and is the same for both, e.g., MBM12_ATTITUDE.FITS 
C  and MBM12_LIVETIME.FITS.  The ACTIME.DAT file is an ASCII list
C  of ordered pairs of start and stop times for the accepted intervals.
C
c --- AUTHORS/MODIFICATION HISTORY -------------------------------------
c
c 
C  Author: Steve Snowden
C  Date:   18 December 1992
C  Update: 31 October 1993
c
c Rehana Yusaf (Aug 30 1994) 1.0.1; FTOOLised version
c
c Banashree M Seifert(SEpt 1996) 1.1.0:
c      . introduced variable TEXPARR due to the fact that
c        EXPARR is real = some real*8 variable
c      . fcecho,wt_ferrmsg etc are replaced by wtinfo,wterrm etc.
c ---------------------------------------------------------------
       character(9) subname
       parameter (subname='hexp_map')
       character(5) version
       parameter (version='1.1.0')
*-
c Internals...

C
        integer j,k
        INTEGER*4  I, IA, IAA,  IACTBE, IACTEN, 
     +      IASP(3,100000),  IE,  II, III,  IOS, 
     +      IR,  ISCS, ISCSO,
     +      IX, IXX, IY, IYY,
     +      NB, NB1, NB2, NB3,  NG, 
     +      NROWSA, NROWSE, NUM,  
     +      kk
C
        REAL*8 A,   ANGLE, ASP(100000), 
     +      COSROL,  DELT, EXP, FLIVE, 
     +      R, RMAP(512,512), ROLL, 
     +      SCALE, SINROL, TEMP, 
     +      TEMPAR(512,512),
     +      TOTEXP, TOTIME, X, XX, Y, YY

       REAL*8 TEXPARR(512,512)

        logical qgti
        character(80) subinfo,INFILE

        REAL*8  DSCS, LIVEN
C INIT
      ie =0
      ia =0
      ng =0
      nb =0
      nb1=0
      nb2=0
      nb3=0
      iscso =0
      num = 0
      flive =0
      temp =0
      liven = 0
      do j=1,map_max
        do k=1,map_max
          exparr(j,k)=0.0
        enddo
      enddo


C
C --- omitted reading input file names ---
C

c added by RY ...

          subinfo = 'using '//subname//' Ver '//version
          call wtinfo(chatter,10,2,subinfo)


        NROWSA = n_att
        NROWSE = n_evr

C
C  Turn the instrument map real.  
C  
C  Test 1: The instrument map is from the BE data with 5" pixels.  The
C  central pixel is at 256.5,256.5 and that the deltas are
C  RMAP(I,II) = TMAP(I,II)
C
        DO I=1,512
            DO II=1,512
                RMAP(I,II) = tmap(I,II)
                R = (I-256.5)**2 + (II-256.5)**2
                IF(R .LT. 3600.) THEN
                    TEMP = TEMP + RMAP(I,II)
                    NUM = NUM + 1
                ENDIF
            ENDDO
        ENDDO
        SCALE = TEMP/NUM
        DO I=1,512
            DO II=1,512
                RMAP(I,II) = RMAP(I,II)/SCALE
            ENDDO
        ENDDO
C
C        WRITE(99) RMAP
C        CLOSE(99)
C
        NUM = 0
C
C  Open the attitude data file.
C
c        IF(INATT .EQ. '         ') THEN
c            PRINT *, 'Enter ATTITUDE table name'
c            READ 1000, INATT
c 1000       FORMAT(A80)
c        ENDIF
C
c        PRINT *, INATT
c        LUNA = 91
c        CALL FTOPEN(LUNA,INATT,0,BLOCKA,STATUS)
c
C  Skip the first HDU
C
c        CALL FTMRHD (LUNA,1,HDUTYPEA,STATUS)
C
C  Read the FITS file header information
C
c        IF(ISTYLE .EQ. 1) THEN
c            CALL FTGHTB (LUNA,10,ROWLENA,NROWSA,TFIELDSA,TTYPEA,
c     +              TBCOLA,TFORMA,TUNITA,EXTNAMEA,STATUS)
c        ELSE
c            CALL FTGHBN (LUNA,10,NROWSA,TFIELDSA,TTYPEA,
c     +              TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
c            PRINT *, 'Enter ROLL offset, in radians'
c            READ *, ROLOFF
c        ENDIF
C
C  Open the accepted time file
C
        INFILE = 'ACTIME.DAT'
c        OPEN(UNIT=93,STATUS='OLD',FORM='FORMATTED',READONLY,
c     +          FILE=INFILE,IOSTAT=IOS)
c        IF(IOS .NE. 0) THEN

cC
C  The ACTIME.DAT file doesn't exist so open the GTI file and write it out.  
C
c            IF(INGTI .EQ. '         ') THEN
c                PRINT *, 'Enter GTI table name'
c                READ 1000, INGTI
c                PRINT *, INGTI
c            ENDIF
c            PRINT *, INGTI
C
c            LUNE = 92
c            CALL FTOPEN (LUNE,INGTI,0,BLOCKE,STATUS)
c            CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
C
C  Read the FITS file header information
C
c            IF(ISTYLE .EQ. 1) THEN
c                CALL FTGHTB (LUNE,35,ROWLENE,NROWSE,TFIELDSE,TTYPEE,
c     +              TBCOLE,TFORME,TUNITE,EXTNAMEE,STATUS)
c            ELSE
c                OPEN(UNIT=70,STATUS='NEW',FORM='FORMATTED',
c     +              FILE='ACTIME.DAT')
c                CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
c     +              TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
c                DO I=1,NROWSE
c                    CALL FTGCFD(LUNE,1,I,1,1,DS,FLAGVALA,ANYFA,STATUS)
c                    CALL FTGCFD(LUNE,2,I,1,1,DE,FLAGVALA,ANYFA,STATUS)
c                    IX = DS
c                    IY = DE
c                    PRINT *, IX, IY
c                    WRITE(70,*) IX, IY
c                ENDDO
c                CLOSE(70)
c                CALL FTCLOS(LUNE,STATUS)
C
C  Now that an ACTIME.DAT file exists, open it up
C
c                OPEN(UNIT=93,STATUS='OLD',FORM='FORMATTED',READONLY,
c     +              FILE=INFILE,IOSTAT=IOS)
c            ENDIF
c        ENDIF
C
C  Open the livetime file.  
C
c        IF(INEVE .EQ. '         ') THEN
c            PRINT *, 'Enter LIVETIME table name'
c            READ 1000, INEVE
c            PRINT *, INEVE
c        ENDIF
c        PRINT *, INEVE
C
c        LUNE = 92
c        CALL FTOPEN (LUNE,INEVE,0,BLOCKE,STATUS)
c        CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
C
C  Read the FITS file header information
C
c        IF(ISTYLE .EQ. 1) THEN
c            CALL FTGHTB (LUNE,35,ROWLENE,NROWSE,TFIELDSE,TTYPEE,TBCOLE,
c     +              TFORME,TUNITE,EXTNAMEE,STATUS)
c        ELSE
c            CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
c     +              TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
c        ENDIF
C
C  Start the loop over the attitude file
C
c   10   CONTINUE
        IOS = 0
        DO IAA=1,NROWSA
C
C  Read in the necessary data from the attitude entry
C
C      ITEMP  line counter from the MIDAS output, ignore
C      DSCS   double precision space craft clock seconds
C      X     pointing offset from nominal position, X direction
C      Y     pointing offset from nominal position, Y direction
C      ROLL  detector roll angle
C
c            IF(ISTYLE .EQ. 1) THEN
c                CALL FTGCFD(LUNA,1,IAA,1,1,DSCS,FLAGVALA,ANYFA,STATUS)
c                CALL FTGCFJ(LUNA,2,IAA,1,1,IX,FLAGVALA,ANYFA,STATUS)
c                CALL FTGCFJ(LUNA,3,IAA,1,1,IY,FLAGVALA,ANYFA,STATUS)
c                CALL FTGCFJ(LUNA,4,IAA,1,1,IROLL,FLAGVALA,ANYFA,STATUS)
c            ELSE
c                CALL FTGCFD(LUNA,1,IAA,1,1,DSCS,FLAGVALA,ANYFA,STATUS)
c                CALL FTGCFE(LUNA,2,IAA,1,1,X,FLAGVALA,ANYFA,STATUS)
c                CALL FTGCFE(LUNA,3,IAA,1,1,Y,FLAGVALA,ANYFA,STATUS)
c                CALL FTGCFE(LUNA,4,IAA,1,1,ROLL,FLAGVALA,ANYFA,STATUS)
C                PRINT *, DSCS,X,Y,ROLL
c            ENDIF

c added by RY ...


             DSCS = asp_time(IAA)
             X = ra_sc(IAA)
             Y = dec_sc(IAA)
             ROLL = roan_sc(IAA)
        
C
C  Determine the delta time from the last entry
C
            ISCS = DSCS
            DELT = ISCS - ISCSO
            IF(DELT .GT. 5.) DELT = 1.
            ISCSO = ISCS
C
C  Flip the Y value for consistancy.  Increasing Y in SASS is downward
C  in declination.  The instrument map has already been flipped.
C
            Y = -Y
            IF(IOS .EQ. 0) THEN
C
C  Process the attitude step, first check the accepted time file
C  to see if the attitude step is in an accepted time period
C
c                DO WHILE ((ISCS .GT. IACTEN) .AND. (IOS .EQ. 0))
c                    READ(93,*,IOSTAT=IOS) IACTBE, IACTEN
c                ENDDO

c                goto 193

c added by RY ...


                qgti = .false.
                IF (gti) THEN
                do kk = 1, n_gti
                  IF (.NOT.qgti) THEN
                    IF (ISCS.GE.start(kk).AND.ISCS.LE.stop(kk)) THEN
                        qgti = .true.
                        IACTBE = INT(start(kk))
                        IACTEN = INT(stop(kk))
                    ENDIF
                  ENDIF
                enddo
                ELSE
                 qgti = .true.
                 n_gti = 1
                 start(1) = ev_time(1)
                 stop(1) = ev_time(n_evr) + 1
                 IACTBE = INT(start(1))
                 IACTEN = INT(stop(1))
                ENDIF
C
c                IF((ISCS .GE. IACTBE) .AND. (IOS .EQ. 0)) THEN
c                    NUM = NUM + 1
c                    IF(MOD(NUM,1000) .EQ. 0) PRINT *, NUM
C
C  Accepted time, now find the live time.  First sort through the event 
C  rate file to find a close time
C
c  193               qgti = .true.

                IF (qgti) THEN
                    NUM = NUM + 1
c                     IF(MOD(NUM,1000) .EQ. 0) PRINT *, NUM

                    DO WHILE ((ISCS .GT. LIVEN) .AND. 
     +                      (IE .LT. NROWSE))
C
C  Read in the necessary data from the LIVETIME entry
C
                        IE = IE + 1
c                        CALL FTGCFD(LUNE,1,IE,1,1,LIVEN,
c     +                          FLAGVALE,ANYFE,STATUS)
c                        CALL FTGCFE(LUNE,2,IE,1,1,FLIVE,
c     +                          FLAGVALE,ANYFE,STATUS)
                         LIVEN = ev_time(IE)
                         FLIVE = livt_cor(IE)
                    ENDDO
C
C  The attitude steps should be on 1-second intervals, calculate the 
C  exposure
C
                    EXP = DELT*FLIVE
                    TOTIME = TOTIME + DELT
                    TOTEXP = TOTEXP + EXP
C
C  Set the X, Y, and ROLL values for the aspect array.  X,Y Steps are 
C  in units of 8.0 arc seconds (historical reasons, this is the digitization
C  of the detector map).  ROLL steps are in units of 0.0064 radians.  This
C  works out to 8" at a radius of 20'.  The roll digitization is therefore
C  equal to one detector map pixel at the edge of the detector.  We want the 
C  nominal pointing direction to be at the corner intersection of pixels
C  256,256 and 257,257.  
C
C  Test 1: the optical axis is at pixel 257.0,257.0
C  (the center of the pixel 256,256 is defined to be at 256.5,256.5), 
C  therefore, the values of the RA and Dec offsets from 
C  X/10.=-0.5 to X/10.=0.5, Y/10.=-0.5 to Y/10.=0.5 should have zero 
C  shifts.  
c changed X from 1001.0 X/10 to 1010.5 + X/10 SS, 20 Sept 1994
c changed Y from 1000.0 + Y/10. to Y = 1003.0 + Y/10., 20 Sept 1994


                    X = 1010.5 + X/10.
                    Y = 1003.0 + Y/10.
                    IX = INT(X) - 1000
                    IY = INT(Y) - 1000
                    ROLL = 20000.5 + (ROLL+roloff)/0.0064
                    IR = INT(ROLL) - 20000
C
C  Add to the aspect list
C
                    IF(IA .GT. 0) THEN
                        NB1 = 1
                        DO I=1,IA
                            IF((IX .EQ. IASP(1,I)) .AND. 
     +                                  (IY .EQ. IASP(2,I)) .AND.
     +                                  (IR .EQ. IASP(3,I))) THEN
                                ASP(I) = ASP(I) + EXP
                                NB1 = 0
                            ENDIF
                        ENDDO
                        IF(NB1 .EQ. 1) THEN
                            IA = IA + 1
                            IASP(1,IA) = IX
                            IASP(2,IA) = IY
                            IASP(3,IA) = IR
                            ASP(IA) = EXP
                        ENDIF
                    ELSE
                        IA = 1
                        IASP(1,IA) = IX
                        IASP(2,IA) = IY
                        IASP(3,IA) = IR
                        ASP(IA) = EXP
                    ENDIF
                    NG = NG + 1
                ELSE
                    NB = NB + 1
                ENDIF
            ENDIF
        ENDDO
C
C  For US data, see if there is another OBI
C
c        IF(ISTYLE .EQ. 2) THEN
C
C  Skip the first HDU
C
c            CALL FTMRHD (LUNA,1,HDUTYPEA,STATUS)
c            IF (STATUS .EQ. 0) THEN
c                IE = 0
cC
C  Read the FITS file header information
C
c                CALL FTGHBN (LUNA,10,NROWSA,TFIELDSA,TTYPEA,
c     +                  TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
c                GO TO 10
c            ENDIF
c        ENDIF
C
C  Print out diagnostic information
C
          write(subinfo,'(a,i12)') ' ... LIST LENGTH = ', IA
          call wtinfo(chatter,9,2,subinfo)
          write(subinfo,'(a,i12)') ' ... GOOD STEPS  = ', NG
          call wtinfo(chatter,9,2,subinfo)
          write(subinfo,'(a,i12)') ' ... BAD STEPS   = ', NB
          call wtinfo(chatter,9,2,subinfo)
          write(subinfo,'(a,F9.2)') ' ... TOTALTIME   = ',TOTIME
          call wtinfo(chatter,9,2,subinfo)
          write(subinfo,'(a,F9.2)') ' ... TOTALEXP    = ',TOTEXP
          call wtinfo(chatter,9,2,subinfo)

c        PRINT *, 'LIST LENGTH =', IA
c        PRINT *, 'GOOD STEPS = ', NG
c        PRINT *, 'BAD STEPS =  ', NB
c        PRINT *, TOTIME, TOTEXP
C
C  Sort on roll angle
C
        DO I=IA-1,2,-1
            DO II=1,I
                IF(IASP(3,II) .GT. IASP(3,II+1)) THEN
                    NB1 = IASP(1,II)
                    NB2 = IASP(2,II)
                    NB3 = IASP(3,II)
                    A = ASP(II)
                    IASP(1,II) = IASP(1,II+1)
                    IASP(2,II) = IASP(2,II+1)
                    IASP(3,II) = IASP(3,II+1)
                    ASP(II) = ASP(II+1)
                    IASP(1,II+1) = NB1
                    IASP(2,II+1) = NB2
                    IASP(3,II+1) = NB3
                    ASP(II+1) = A
                ENDIF
            ENDDO
        ENDDO
C
C  Now cast the exposure
C
        NB = IA/10
        NG = 0
        NB1 = -100000
        DO I=1,IA
            call xclock(I,IA,10)

            IX = IASP(1,I)
            IY = IASP(2,I)
            IR = IASP(3,I)
            IF(IR .NE. NB1) THEN
C
C  First nonzero aspect point with this roll angle, make a rotated map
C
                ANGLE = IR*0.0064
                COSROL = COS(ANGLE)
                SINROL = SIN(ANGLE)
C
C  Zero the temp array
C
                DO II=1,512
                    DO III=1,512
                        TEMPAR(II,III) = 0.
                    ENDDO
                ENDDO
C
C  Calculate the rotated array only once for each roll angle
C  Remember, the optical axis is at 
C  Test 1: 257.0, 257.0, so pixel 257,257 (whose center is 257.5, 257.5) 
C  is 0.5,0.5 pixels offset from the center.
C
C                X = (II - 256.5)
C                Y = (III - 256.5)
C                IXX = INT(XX + 257.00)
C                IYY = INT(YY + 257.00)
C
c RY and Steve Snowden 20 Sept 1994
c change II and III loop from 36,478 to 30,485
c c and X = (II - 256.5) changed to X = (II - 263.0)
c Y = (III - 256.5) to Y = (III - 255.5)

                DO II=30,485
                    DO III=30,485
                        IF(RMAP(II,III) .NE. 0.) THEN
                            X = (II - 263.0)
                            Y = (III - 255.5)
                            XX = COSROL*X + SINROL*Y
                            YY = COSROL*Y - SINROL*X
                            IXX = INT(XX + 257.00)
                            IYY = INT(YY + 257.00)
                            IF((IXX .GE. 1) .AND. (IXX .LE. 512) 
     +                              .AND.(IYY .GE. 1) .AND. 
     +                              (IYY .LE. 512)) THEN
                                TEMPAR(IXX,IYY) = 
     +                              TEMPAR(IXX,IYY) + RMAP(II,III)
                            ENDIF
                        ENDIF
                    ENDDO
                ENDDO
            ENDIF
            NB1 = IR
C
C  Cast the exposure
C
            DO II=1,512
                IXX = II + IX
                DO III=1,512
                    IF(TEMPAR(II,III) .NE. 0.) THEN
                        IYY = III + IY
                        IF((IXX .GE. 1) .AND. (IXX .LE. 512) .AND.
     +                     (IYY .GE. 1) .AND. (IYY .LE. 512)) then
                            TEXPARR(IXX,IYY) = TEXPARR(IXX,IYY) +
     +                              (ASP(I)*TEMPAR(II,III))
                            EXPARR(IXX,IYY) = sngl(TEXPARR(IXX,IYY))
                        ENDIF
                    ENDIF
                ENDDO
            ENDDO
        ENDDO
C
C
c        CLOSE(93)
        return
        END
