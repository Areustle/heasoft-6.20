*+PCEXPMAP
c     -----------------
      subroutine pcexpp 
c     -----------------
c --- DESCRIPTION ---------------------------------------------------
c This task generates an exposure map for a given PSPC observation.
c The calibration input(s) is a detector map. The task takes the map
c and rotates it as appropriate for a given observation dataset, and
c sums the results (with approriate weighting factors), thus a 'mean'
c exposure map is produced. The science data inputs are 
c - a GTI (Good Time Interval) file
c - an EVR (Event Rate) file
c - an Attitude file.
c -------------------------------------------------------------------
c --- CALLED ROUTINES -----------------------------------------------
c
c EXP_GP     : Gets parameters
c EXP_RDAT   : Reads data, by calling a separate routine for 
c              each file
c EXP_MAP    : This routine makes the calculations
c EXP_WT     : This writes the exposure map in FITS format
c 
c --- COMPILATION/LINKING -------------------------------------------
c
c FTOOLS - FITSIO,CALLIB
c 
c --- AUTHORS/MODIFICATION ------------------------------------------
c 
c Rehana Yusaf (1993 Oct 1)
c Ian M George (1.0.1: 1993 Oct 07) took out it2_cas array (due to changes 
c  					made to exp_rdat routine)
c
c Rehana Yusaf (Nov 12 1993) 1.0.2; Pass all filenames to EXP_WT
c                                
c Rehana Yusaf (March 16 1994) 2.0.0; Add Dynamic Memory Allocation
c                                     Also added RDF readers
c                                     One more subroutine,exp_form
c                                     it checks the format of the
c                                     data, currently US (rev0) and
c                                     RDF (Rev 2) are supported. 
c                                     renamed from pspcexpm
c Rehana Yusaf (June 27 1994) 2.0.1;  increase max_evr array dimension
c                                     from 8000 to 13046
c                                     improve DMA allocation
c                                     DMA allocate *_cur arrays
c Rehana Yusaf (23 August 1994) 2.0.2;move exp_form from exp_rdat
c                                     to main. exp_form now determines
c                                     array dimensions.
c                                     add clobber, (killit) parameter
c Rehana Yusaf (13 Dec 1994) 2.0.3;   replace MEML with MEMI, as MEML
c                                     overwrites arrays
c Rehana Yusaf (14 Nov 1995) 2.0.4;   determine instrume in exp_form
c Rehana Yusaf (22 Dec 1995) 2.0.5;   Fix keyword values, crval,cdelt,and crpix
c                                     Add wtinfo and frinds 
c
c Banashree M Seifert (June 1996) 2.1.0:
c            . added option for using devignetted detector map
c              The program will read the event file and find out the
c              date of observation.  If it is before Oct14, 1991 the
c              it will look for high gain file and if it is after
c              then it will look for low gain FITS file.
c  This modification is seen in exp_gp subroutine where it makes 
c  decision regarding this.
c
c Banashree M Seifert (Aug 1996) 2.2.0:
c            . before calling gtcalf if the instrument is ambiguous 
c              i.e.,only PSPC, then it will look for date of observation
c              and then decide whether PSPCB or PSPCC. 
c              GTCALF doesn't work without this 
c
c Banashree M Seifert (Dec 1996) 2.3.0:
c            . in subroutine exp_map, the array exparr was not
c              initialised causing segmentation fault in OSF system
c              and is initialised now
c Peter D Wilson (1998 June 30) 2.3.1:
c            . Update for new FCPARS behavior
c Ning Gan (1998 June 30) 2.3.2:
c            . Update for the new Date keyword format. 
C MFC (2004 Feb 26) 2.3.3
C 		bugfix: when calculating ONTIME from aspect time, check to see
C		that both current time (ISCS) and previous time (ISCSO) 
C		fall within the good time interval boundaries
C MFC & BW (2005 JAN 25) 2.3.4
C		bugfix: check that IYY & IXX (line 1783) within bounds of 
C 		exparr array; also initialized totime
c --------------------------------------------------------------------------

      IMPLICIT NONE
      character(5) version
      parameter (version = '2.3.4')
      character(40) taskname
      parameter (taskname = 'pcexpmap')
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
      character(16) instrum
      logical gti,killit         
      character(3) gti_data,form

c VARIABLES RELATED TO DETECTOR MAP ...

      character(180) dmapfil
      integer map_max,chanmin,chanmax,p_tmap
      real crval1,crval2
      parameter (map_max=512)
c     DYNAMIC MEMORY ALLOCATED ARRAY - 
c      real*4 tmap(map_max,map_max) 

c VARIABLES RELATED TO ATTITUDE DATA ...

      character(180) attfil
      integer max_att,n_att
      integer p_it1_cas,p_iro_cas,p_ixn_cas,p_iyn_cas
      integer p_roan_cas,p_ra_cas,p_dec_cas,p_time
c      parameter (max_att=100000)
c DMA    real*8 it1_cas(max_att),iro_cas(max_att)
c DMA    real*8 ixn_cas(max_att),iyn_cas(max_att)
c DMA    real*8 roan_cas(max_att),ra_cas(max_att)
c DMA    real*8 dec_cas(max_att),time(max_att)

c VARIABLES RELATED TO EVENT RATE DATA ...

      character(180) evrfil,evtfil
      integer max_evr,n_evr
      integer p_iti_evr,p_iac_evr,p_iqe_evr
      integer p_ia1_evr,p_iax_evr
c      parameter (max_evr=13046)
c DMA    real*8 iti_evr(max_evr)
c DMA    integer*4 iac_evr(max_evr),iqe_evr(max_evr)
c DMA    integer*4 ia1_evr(max_evr),iax_evr(max_evr)

c VARIABLES RELATED TO GTI DATA ...

      character(180) gtifil
      integer max_gti,n_gti
      parameter (max_gti=120)
      real*8 start(max_gti),stop(max_gti)

c VARIABLES RELATED TO EXPOSURE MAP ...

      integer p_exparr
c DMA    real*4 exparr(map_max,map_max)
c
c LOCAL VARIABLES FOR ATTITUDE DATA ...
c
      integer max_curatt
c      parameter (max_curatt = 10000)
      integer p_it1_curcas,p_iro_curcas,p_ixn_curcas
      integer p_iyn_curcas,p_it2_curcas
c
c LOCAL VARIABLES FOR EVENTS DATA ...
c
      integer max_curevr
c      parameter (max_curevr = 10000)
      integer p_iti_curevr,p_iac_curevr,p_iqe_curevr
      integer p_ia1_curevr,p_iax_curevr

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
c it1_cas      int   : Array of Time of corrected aspect in full seconds
c iro_cas      int   : Array of corrected roll angle
c ixn_cas      int   : Array of x-translation in NS-system
c iyn_cas      int   : Array of y-translation in NS-system
c evrfil       char  : Event rate filename
c max_evr      int   : array dimension for event data
c n_evr        int   : counter for event data
c iti_evr      int   : Array of event rate
c iac_evr      int   : Array of MV anticoincident rate
c iqe_evr      int   : Transmitted X-ray rate
c ia1_evr      int   : Array of events in A1 above low level threshold
C iax_evr      int   : Ac X-ray ra
c gtifil       char  : gti filename
c max_gti      int   : Array size for gti data
c n_gti        int   : counter for gti data
c iyn_cas      int   : Array of y-translation in NS-system
c evrfil       char  : Event rate filename
c max_evr      int   : array dimension for event data
c n_evr        int   : counter for event data
c iti_evr      int   : Array of event rate
c iac_evr      int   : Array of MV anticoincident rate
c iqe_evr      int   : Transmitted X-ray rate
c ia1_evr      int   : Array of events in A1 above low level threshold
c iax_evr      int   : Accepted X-ray rate
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
      call exp_gp(attfil,evrfil,gtifil,evtfil,outfil,
     &             dmapfil,gti,chatter,killit,errflg)
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
      call exp_form(attfil,evrfil,gtifil,gti,gti_data,evtfil,
     &              form,max_att,max_curatt,max_evr,
     &              max_curevr,crval1,crval2,errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 100
      ENDIF

c
c --- ALLOCATE DYNAMIC MEMORY ---------------------------------------
c
      p_tmap = 0
      p_it1_cas = 0
      p_iro_cas = 0
      p_ixn_cas = 0
      p_iyn_cas = 0
      p_roan_cas = 0
      p_ra_cas = 0
      p_dec_cas = 0
      p_time = 0
      p_iti_evr = 0
      p_iac_evr = 0
      p_iqe_evr = 0
      p_ia1_evr = 0
      p_iax_evr = 0
      p_exparr = 0
      p_it1_curcas = 0
      p_iro_curcas = 0
      p_ixn_curcas = 0
      p_iyn_curcas = 0
      p_it2_curcas = 0
      p_iti_curevr = 0
      p_iac_curevr = 0
      p_iqe_curevr = 0
      p_ia1_curevr = 0
      p_iax_curevr = 0

      status = 0
      call udmget(map_max*map_max,6,p_tmap,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_att,7,p_it1_cas,status)
      IF (status.NE.0) THEN
        goto 50 
      ENDIF         
      call udmget(max_att,7,p_iro_cas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_att,7,p_ixn_cas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_att,7,p_iyn_cas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_att,7,p_roan_cas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_ra_cas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_dec_cas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_time,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,7,p_iti_evr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_evr,4,p_iac_evr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_evr,4,p_iqe_evr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_evr,4,p_ia1_evr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(max_evr,4,p_iax_evr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF         
      call udmget(map_max*map_max,6,p_exparr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curatt,4,p_it1_curcas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curatt,4,p_iro_curcas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curatt,4,p_ixn_curcas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curatt,6,p_iyn_curcas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curatt,4,p_it2_curcas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curevr,4,p_iti_curevr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curevr,4,p_iac_curevr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curevr,4,p_iqe_curevr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curevr,4,p_ia1_curevr,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_curevr,4,p_iax_curevr,status)
 50   IF (status.NE.0) THEN
        errflg = -1
        subinfo = ' failed to allocate dynamic memory'
        call wterrm(taskname,version,subinfo)
        goto 100
      ENDIF
c
c --- READ DATA ---
c
      call exp_rdat(dmapfil,map_max,chanmin,chanmax,MEMR(p_tmap),
     &           attfil,max_att,n_att,MEMD(p_it1_cas),
     &           MEMD(p_iro_cas),MEMD(p_ixn_cas),
     &           MEMD(p_iyn_cas),MEMD(p_roan_cas),MEMD(p_ra_cas),
     &           MEMD(p_dec_cas),MEMD(p_time),
     &           evrfil,max_evr,n_evr,
     &           MEMD(p_iti_evr),MEMI(p_iac_evr),
     &           MEMI(p_iqe_evr),MEMI(p_ia1_evr),MEMI(p_iax_evr),
     &           gtifil,gti,max_gti,n_gti,start,stop,
     &           max_curatt,MEMI(p_it1_curcas),MEMI(p_iro_curcas),
     &           MEMI(p_ixn_curcas),MEMR(p_iyn_curcas),
     &           MEMS(p_it2_curcas),max_curevr,MEMI(p_iti_curevr),
     &           MEMI(p_iac_curevr),MEMI(p_iqe_curevr),
     &           MEMI(p_ia1_curevr),MEMI(p_iax_curevr),gti_data,form,
     &           instrum,chatter,errflg)
      IF (errflg.NE.0) THEN
         goto 100 
      ENDIF               

c
c --- EXPOSURE MAP CALCULATIONS ---
c
      call exp_map(map_max,chanmin,chanmax,MEMR(p_tmap),
     &             max_att,n_att,MEMD(p_it1_cas),
     &             MEMD(p_iro_cas),MEMD(p_ixn_cas),
     &             MEMD(p_iyn_cas),max_evr,n_evr,MEMD(p_iti_evr),
     &             MEMI(p_iac_evr),MEMI(p_iqe_evr),
     &             MEMI(p_ia1_evr),MEMI(p_iax_evr),gti,                       
     &             max_gti,n_gti,start,stop,MEMR(p_exparr),
     &             chatter,errflg) 
      IF (errflg.NE.0) THEN
         goto 100 
      ENDIF               
c
c --- WRITE EXPOSURE MAP FILE ---
c
      call exp_wt(outfil,dmapfil,attfil,evrfil,gtifil,instrum,
     &            chanmin,chanmax,map_max,MEMR(p_exparr),
     &            crval1,crval2,
     &            taskname,killit,chatter,errflg)
      IF (errflg.NE.0) THEN
         goto 100 
      ENDIF               
c
c --- FREE THE DYNAMIC MEMORY ---
c
      status = 0
      call udmfre(p_tmap,6,status)
      status = 0
      call udmfre(p_it1_cas,7,status)
      status = 0
      call udmfre(p_iro_cas,7,status)
      status = 0
      call udmfre(p_ixn_cas,7,status)
      status = 0
      call udmfre(p_iyn_cas,7,status)
      status = 0
      call udmfre(p_roan_cas,7,status)
      status = 0
      call udmfre(p_ra_cas,7,status)
      status = 0
      call udmfre(p_dec_cas,7,status)
      status = 0
      call udmfre(p_time,7,status)
      status = 0
      call udmfre(p_iti_evr,7,status)
      status = 0
      call udmfre(p_iac_evr,4,status)
      status = 0
cc it is giving some error from here for 4 parameters
      call udmfre(p_iqe_evr,4,status)
      status = 0
      call udmfre(p_ia1_evr,4,status)
      status = 0
      call udmfre(p_iax_evr,4,status)
      status = 0
      call udmfre(p_exparr,6,status)
      status = 0
      call udmfre(p_it1_curcas,4,status)
      status = 0
      call udmfre(p_iro_curcas,4,status)
      status = 0
      call udmfre(p_ixn_curcas,4,status)
      status = 0
      call udmfre(p_iyn_curcas,6,status)
      status = 0
      call udmfre(p_it2_curcas,4,status)
      status = 0
      call udmfre(p_iti_curevr,4,status)
      status = 0 
      call udmfre(p_iac_curevr,4,status)
      status = 0
      call udmfre(p_iqe_curevr,4,status)
      status = 0
      call udmfre(p_ia1_curevr,4,status)
      status = 0
      call udmfre(p_iax_curevr,4,status)
      IF (status.NE.0) THEN
        subinfo = ' failed to de-allocate memory'
        call wterrm(taskname,version,subinfo)
        errflg = 99
      ENDIF      
  100 call wtendm(taskname,version,errflg,chatter)
      return
      end
c ----------------------------------------------------------------------
c     END OF PCEXPMAP
c ----------------------------------------------------------------------   


*+EXP_GP
c     ------------------------------------------------------
      subroutine exp_gp(attfil,evrfil,gtifil,evtfil,outfil,
     &                  dmapfil,gti,chatter,killit,errflg)
c     ------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c
c This subroutine reads the user defined parameters.
c
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) attfil,evrfil,outfil,dmapfil,gtifil,evtfil
      integer chatter,errflg
      logical gti,killit
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
c Rehana Yusaf (1994 August 24) 1.0.3; Remove defval and add killit
c Rehana Yusaf (1995 Nov 14) 1.0.4; instrume no longer read
c Rehana Yusaf (1996 Jan 2) 1.0.5; read evtfil and add wtinfo etc
c
c Banashree M Seifert (1996 June) 1.1.0:
c          . added option for using devignetted map. If so, then
c            it will open the evnet file and look for observation 
c            DATE-OBS and choose the high/low gain FITS file
c            accordingly
c            For this purpose few new variables are added as internal 
c            varuables
c
c Banashree M Seifert (1996 Aug) 2.0.0:
c          . introduced call to gtcalf to get the required 
c            devignetted file from CALDB
c
c Banashree M Seifert (1996 Sept) 2.1.0:
c          . Modification for SOLARIS 
c            (instead of call to MVER it calls fndext and ftmrhd)
c Peter D Wilson (1998 June 30) 2.1.1:
c          . Update for new FCPARS behavior
c ----------------------------------------------------------------------

      character(5) version
      parameter (version = '2.1.1')
*-
c ----------------------------------------------------------------------
c
c INTERNALS ...
c
c these four lines below are added by BMS to include devignetted 
c detmap file option
      logical qdetmap
      integer iunit,block
c      character(10) date_obs,date_end,time_obs,time_end
      character(68) date_obs,date_end,time_obs,time_end
      character(80) com
c end of addition

      character(80) subinfo
      character(180) filename,ill_files(6),temp
      character(192) fileinfo
      integer status,n_ill,extnum
      logical ext,valfil
      character(6) subname
      parameter (subname = 'exp_gp')

      character(10) extname
      integer ninstr,nsearch
      character(10) instr(50)
      integer next(50)
      character(10) outhdu(9,50),extnames(50),outver(9,50)
      integer maxret
      parameter (maxret=4)
      character(80) online(maxret)
      character(10) telescop,instrume
      integer extno(maxret),nfound,nret

      integer iday,imnth,iyr
      integer hdutyp
c
c --- GET CHATTER PARAMETER ---
c
      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        subinfo = ' getting chatter parameter !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF               
c
c --- GET EVENT FILENAME ---
c
      status = 0
      call uclgst('evrfil',evrfil,status)
      IF (status.NE.0) THEN
        subinfo = ' getting evrfil parameter !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF
      call crmvlbk(evrfil)
C PDW 6/30/98: Drop call to INQUIRE
      call ftrtnm( evrfil, filename, status )
      ill_files(1) = filename
C      call fcpars(evrfil,filename,extnum,status)
C      ill_files(1) = filename
C      ext = .true.
C      INQUIRE(FILE=filename,EXIST=ext)
C      IF ((.NOT.ext).OR.(filename.EQ.'  ')) THEN
C        subinfo = 'event file does not exist '
C        call wterrm(subname,version,subinfo)
C        fileinfo = ' filename : '//filename
C        call wtinfo(0,0,1,fileinfo)
C        errflg = 1
C        return
C      ENDIF                    

c
c --- GET ATTITUDE FILENAME ---
c
      status = 0
      call uclgst('attfil',attfil,status)
      IF (status.NE.0) THEN
        subinfo = ' getting attfil parameter !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF
      call crmvlbk(attfil)
      IF (attfil.EQ.'%') THEN
        attfil = filename 
      ELSE
C PDW 6/30/98: Drop call to INQUIRE
        call ftrtnm( attfil, filename, status )
        ill_files(2) = filename
C        call fcpars(attfil,filename,extnum,status)
C        ill_files(2) = filename
C        INQUIRE(FILE=filename,EXIST=ext)
C        IF ((.NOT.ext).OR.(filename.EQ.'  ')) THEN
C         subinfo = ' attitude file does not exist '
C         call wterrm(subname,version,subinfo)
C         fileinfo = ' filename : '//filename
C         call wtinfo(0,0,1,fileinfo)
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
        subinfo = ' getting gtifil parameter !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF          
      call crmvlbk(gtifil)
      temp = gtifil
      call ftupch(temp)
      IF ((temp.NE.'NONE').AND.(temp.NE.'  ')) THEN
C PDW 6/30/98: Drop call to INQUIRE
       call ftrtnm( gtifil, filename, status )
       ill_files(3) = filename 
       gti = .true.
      ENDIF 
c
c --- GET EVTFIL ---
c
      call uclgst('evtfil',evtfil,status)
      IF (status.NE.0) THEN
        subinfo = ' getting evtfil parameter !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF
      call crmvlbk(evtfil)
      IF (evtfil.EQ.'%') THEN
        evtfil = filename
      ELSE
        filename = evtfil
        call ftupch(evtfil)
        IF (evtfil.NE.'NONE') THEN
C PDW 6/30/98: Drop call to INQUIRE
         evtfil = filename
         call ftrtnm( evtfil, filename, status )
         ill_files(5) = filename
C         call fcpars(evtfil,filename,extnum,status)
C         ill_files(2) = filename
C         INQUIRE(FILE=filename,EXIST=ext)
C         IF ((.NOT.ext).OR.(filename.EQ.'  ')) THEN
C          subinfo = ' events file does not exist '
C          call wterrm(subname,version,subinfo)
C          fileinfo = ' filename : '//filename
C          call wtinfo(0,0,1,fileinfo)
C          errflg = 1
C          return
C         ENDIF
        ENDIF
      ENDIF     

c
c --- GET OUTFILE NAME ---
c
        call uclgst('outfil',outfil,status)
        IF (status.NE.0) THEN
          subinfo = ' Getting outfile parameter'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
        ENDIF
        call crmvlbk(outfil)
        IF (outfil.EQ.'  ') THEN
          subinfo = ' output filename must be entered !'
          call wterrm(subname,version,subinfo)
          errflg=1
          return
        ENDIF

c -- if want to use devignetted detector map file

      call uclgsb('qdetmap',qdetmap,status)
      if (status. ne. 0) then
          subinfo = 'getting qdetmap parameter !'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

c if qdetmap=.false. then ask for detector map file
c if qdetmap=.true.  then look for the date of observation in the 
c evtfil primary header to find out which type (high or low gain) 
c of detmap file is required.
c if DATE-OBS before Oct 11, 1991 then use 
c                      dmapfil=devig_l.fits -- for low gain
c if DATE-OBS after  Oct 11, 1991 then use 
c                      dmapfil=devig_h.fits -- for high gain
c -- these two files are in /caldb/data/rosat/pspc/cpf/detmaps/ 

      if(qdetmap) then

         call ftgiou(iunit,status)
         call ftopen(iunit,evtfil,0,block,status)
         call ftgkys(iunit,'DATE-OBS',date_obs,com,status)
         status=0
         call ftgkys(iunit,'TIME-OBS',time_obs,com,status)
         status=0
         call ftgkys(iunit,'DATE-END',date_end,com,status)
         if(status .ne. 0) date_end=date_obs
         status=0
         call ftgkys(iunit,'TIME-END',time_end,com,status)
         if(status .ne. 0) time_end=time_obs
         status=0
         call ftgkys(iunit,'TELESCOP',telescop,com,status)
         status=0
         call ftgkys(iunit,'INSTRUME',instrume,com,status)

         if(status .ne. 0) then
            extname='EVENTS'
            nsearch=50
            call fndext(chatter,iunit,extname,nsearch,nfound,
     >                  next,outhdu,outver,extnames,status)
            if(nfound .gt. 1) then
               subinfo='more than one EVENTS exten found'
               call wterrm(subname,version,subinfo)
               errflg=1
               return
            endif

            if(status .ne. 0) then
               subinfo='no extension with EVENTS found'
               call wterrm(subname,version,subinfo)
               errflg=1
               return
            endif

            call ftmahd(iunit,next(1)+1,hdutyp,status)

            call ftgkys(iunit,'DATE-OBS',date_obs,com,status)
            status=0
            call ftgkys(iunit,'TIME-OBS',time_obs,com,status)
            status=0
            call ftgkys(iunit,'DATE-END',date_end,com,status)
            if(status .ne. 0) date_end=date_obs
            status=0
            call ftgkys(iunit,'TIME-END',time_end,com,status)
            if(status .ne. 0) time_end=time_obs
            status=0
            call ftgkys(iunit,'TELESCOP',telescop,com,status)
            status=0
            call ftgkys(iunit,'INSTRUME',instrume,com,status)
         endif
           
c           if instrument name is ambiguous, that is, PSPC then determine
c           whether PSPCB or PSPCC --
c           before  Jan 25, 1991 it is PSPCC
c           & after Jan 25, 1991 it is PSPCB

            if((instrume(5:5) .ne. 'B') .or. 
     >                             (instrume(5:5) .ne. 'C')) then 
C               read(date_obs(1:2),'(i2)')iday
C               read(date_obs(4:5),'(i2)')imnth
C               read(date_obs(7:8),'(i2)')iyr   
	       status = 0
	       call fts2dt(date_obs,iyr,imnth,iday,status) 
	       status = 0

               if(iyr .eq. 1990) then
                  instrume='PSPCC'
               endif

               if(iyr .gt. 1991) then
                  instrume='PSPCB'
               endif

               if(iyr .eq. 1991) then
                  if(imnth .gt. 1) then
                     instrume='PSPCC'
                  else
                     if(iday .gt. 25) then
                        instrume='PSPCC' 
                     else
                        instrume='PSPCB'
                     endif
                  endif
                endif
            endif 

         call gtcalf(chatter,telescop,instrume,'-','-','DETMASK',
     >               date_obs,time_obs,date_obs,time_obs,'-',
     >               1,dmapfil,extno,online,nret,nfound,errflg)


         call ftclos(iunit,status)
         call ftfiou(iunit,status)

      else

c --- GET DETECTOR MAP FILE, IF QUZCIF IS FALSE ---

        status = 0
        call uclgst('dmapfil',dmapfil,status)
        IF (status.NE.0) THEN
          subinfo = ' getting dmapfil parameter !'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
        ENDIF           

      endif

      call crmvlbk(dmapfil)
C PDW 6/30/98: Drop call to INQUIRE
      call ftrtnm( dmapfil, filename, status )
      ill_files(4) = filename
C      call fcpars(dmapfil,filename,extnum,status)
C      ill_files(4) = filename
C      INQUIRE(FILE=filename,EXIST=ext)
C      IF ((.NOT.ext).OR.(filename.EQ.'  ')) THEN
C          subinfo = 'dmap file does not exist '
C          call wterrm(subname,version,subinfo)
C          fileinfo = ' filename : '//filename
C          call wtinfo(0,0,1,fileinfo)
C          errflg = 1
C          return
C      ENDIF        

c read in clobber
      
        status = 0
        call uclgsb('clobber',killit,status)
        IF (status.NE.0) THEN
          subinfo = ' Getting clobber parameter'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
        ENDIF 

c check outfil validity

      valfil = .true.
      n_ill = 5
      call ck_file(outfil,ill_files,n_ill,valfil,killit,chatter)
      IF (.NOT.valfil) THEN
        subinfo = ' outfil is not valid !'
        call wterrm(subname,version,subinfo)
        errflg = 1
        return
      ENDIF 

      return
      end
c ----------------------------------------------------------------------
c     END OF EXP_GP
c ----------------------------------------------------------------------      


*+EXP_RDAT
c     -------------------------------------------------------------
      subroutine exp_rdat(dmapfil,map_max,chanmin,chanmax,tmap,
     &              attfil,max_att,n_att,it1_cas,iro_cas,
     &              ixn_cas,iyn_cas,roan_cas,ra_cas,dec_cas,time,
     &              evrfil,max_evr,n_evr,iti_evr,iac_evr,iqe_evr,
     &              ia1_evr,iax_evr,
     &              gtifil,gti,max_gti,n_gti,start,stop,
     &              max_curatt,it1_curcas,iro_curcas,ixn_curcas,
     &              iyn_curcas,it2_curcas,max_curevr,iti_curevr,
     &              iac_curevr,iqe_curevr,ia1_curevr,iax_curevr,
     &              gti_data,form,instrume,chatter,errflg)
c     -------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c This subroutine reads the data inputs for the PCEXPMAP task. It reads
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
      character*(*) instrume
 
c VARIABLES RELATED TO DETECTOR MAP ...

      character*(*) dmapfil
      integer map_max,chanmin,chanmax,i,j
      real*4 tmap(map_max,map_max)

c VARIABLES RELATED TO ATTITUDE DATA ...

      character*(*) attfil
      integer max_att,n_att
      real*8 it1_cas(*),iro_cas(*)
      real*8 ixn_cas(*), iyn_cas(*)
      real*8 roan_cas(*),ra_cas(*)
      real*8 dec_cas(*),time(*)
      integer max_curatt
      integer*4 it1_curcas(*)
      integer*4 iro_curcas(*),ixn_curcas(*)
      integer*2 it2_curcas(*)
      real iyn_curcas(*)

c VARIABLES RELATED TO EVENT RATE DATA ...

      character*(*) evrfil
      integer max_evr,n_evr
      real*8 iti_evr(*)
      integer*4 iac_evr(*),iqe_evr(*)
      integer*4 ia1_evr(*),iax_evr(*)                  
      integer max_curevr
      integer*4 iti_curevr(*)
      integer*4 iac_curevr(*),iqe_curevr(*)
      integer*4 ia1_curevr(*),iax_curevr(*)  

c VARIABLES RELATED TO GTI DATA ...

      character*(*) gtifil
      integer*4 max_gti,n_gti
      real*8 start(*),stop(*)       

c --- CALLED ROUTINES -----------------------------------------------
c
c EXP_FORM    : Determines data format - US or RDF
c RDATU0      : Reads US REV0 format Attitude file
c RDATRD      : Reads RDF format ATTITUDE file
c RDMVU0      : Reads US REV0 format EVR file
c RDMVRD      : Reads RDF format EVR file
c RDGTI1      : Reads GTI file
c RDETM1      : Reads DETECTOR MAP file
c FTOPEN      : (FITSIO) Opens FITS file
c FTCLOS      : (FITSIO) Closes FITS file
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf 	    1.0.0; 4 Oct 1993
c Ian M George     (1.0.1: 93 Oct 07) reset n_curatt=n_att given changes to
c				     rd_attusrev0 subroutine PLUS deleted 
c				     it2_cas as a passed parameter (see below)
c Rehana Yusaf (1993 Nov 11 1993) 1.0.2; Rename subroutines to conform
c				     to 6 unique character restriction
c Rehana Yusaf (1994 March 21) 1.0.3; Add routine - exp_form, to determine
c                                     data format, only US REV0 and
c                                     RDF (REV2) supported. Add RDF readers
c Rehana Yusaf (1994 June 27) 1.0.4;  DMA allocate *_curr arrays in main
c                                     extra argument for mvext call
c Rehana Yusaf (1994 august 24) 1.0.5; Remove exp_form, git_data &
c                                     form passed as arguments
c Rehana Yusaf (1995 Nov 14) 1.0.6; determine instrument being used
c Rehana Yusaf (1996 Jan 4) 1.0.7;  add wtinfo and friends
      character(5) version
      parameter (version='1.0.7')
*-
c -------------------------------------------------------------------
c INTERNALS ...

      character(70) subinfo,errinfo
      character(180) filename
      character(16) dmapinstr,dmaptlscop,comm
      character(3) form,gti_data
      integer block,status,iunit,inlen,chg_time
      character(68) date

c LOCAL VARIABLES FOR READING ATTITUDE DATA ...

      integer n_curatt
      logical end_attdata,end_evr


c LOCAL VARIABLES FOR READING EVENT RATE DATA

      integer n_curevr

c VARIABLES FOR MVEXT  

      integer nsearch,extnum,ninstr
      parameter (nsearch = 50)
      integer next(nsearch)
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(9,nsearch) ,instr(9)
      character(8) extname,subname
      parameter (subname='exp_rdat')
      
c
c --- USER INFO ---
c
      subinfo = ' using '//subname//' Ver '//version
      call wtinfo(chatter,10,1,subinfo)

      do i=1,512
        do j=1,512
          tmap(i,j) = 0.0
        enddo
      enddo
c
c --- READ DETECTOR MAP FILE ---
c
      call rdetm1(dmapfil,map_max,dmapinstr,dmaptlscop,
     &                  chanmin,chanmax,tmap,chatter,errflg)
      IF (errflg.NE.0) THEN
        errinfo = 'reading detector map file'
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
        errinfo = ' opening attitude file !'
        call wtferr(subname,version,status,errinfo)
        IF (status.NE.0) THEN
         errflg = 1
         return
        ENDIF
        n_att = 0
        end_attdata = .false.
        do WHILE(.NOT.end_attdata)
          call rdatu0(iunit,n_curatt,max_curatt,it1_curcas,it2_curcas,
     &                iro_curcas,ixn_curcas,iyn_curcas,chatter,errflg)
          IF ((errflg.EQ.107).OR.(errflg.EQ.207)) THEN
            end_attdata = .true.
            errflg = 0
          ELSEIF (errflg.NE.0) THEN
            errinfo = ' fatal error reading attitude file'
            call wtferr(subname,version,status,errinfo)
            return     
          ENDIF
c
c --- IMG - I made this change in drunken state on the night of Oct 07
c
          IF (.NOT.end_attdata) THEN
           do i=1,n_curatt
            it1_cas(i+n_att) = DBLE(it1_curcas(i))+it2_curcas(i)/64.D0
            iro_cas(i+n_att) = DBLE(iro_curcas(i))
            ixn_cas(i+n_att) = DBLE(ixn_curcas(i))
            iyn_cas(i+n_att) = DBLE(iyn_curcas(i))
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
        call rdatrd(iunit,n_att,max_att,time,
     &             roan_cas,ra_cas,dec_cas,chatter,errflg) 
        IF (errflg.NE.0) THEN
            errinfo = ' fatal error reading attitude file'
            call wtferr(subname,version,status,errinfo)
            return
        ENDIF                      
        do i=1,n_att
          it1_cas(i) = time(i)
          iro_cas(i) = roan_cas(i)*DBLE(7200)
          ixn_cas(i) = ra_cas(i)*DBLE(7200)
          iyn_cas(i) = dec_cas(i)*DBLE(7200)
        enddo
      ENDIF
      status = 0  
      call ftclos(iunit,status)
      errinfo = ' closing attitude file'
      call wtferr(subname,version,status,errinfo)
c
c --- READ EVENT RATE FILE ---
c
      IF (form.EQ.'US') THEN
       call fcpars(evrfil,filename,extnum,status)
       call cgetlun(iunit)
       status = 0
       call ftopen(iunit,filename,0,block,status)
       errinfo = ' opening event rate file !'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg = 1
         return
       ENDIF
       end_evr = .false.
       n_evr = 0
       do WHILE (.NOT.end_evr)             
        call rdmvu0(iunit,n_curevr,max_curevr,iti_curevr,
     &                iac_curevr,iqe_curevr,ia1_curevr,iax_curevr,
     &                chatter,errflg)
        IF ((errflg.EQ.107).OR.(errflg.EQ.207)) THEN
          end_evr = .true.
          errflg = 0
        ELSEIF (errflg.NE.0) THEN
          errinfo = ' fatel error reading event Rate file'
          call wtferr(subname,version,status,errinfo)
          return
        ENDIF
        IF (.NOT.end_evr) THEN
          do i=1,n_curevr
            iti_evr(i+n_evr) = DBLE(iti_curevr(i))
            iac_evr(i+n_evr) = DBLE(iac_curevr(i))
            iqe_evr(i+n_evr) = DBLE(iqe_curevr(i))
            ia1_evr(i+n_evr) = DBLE(ia1_curevr(i))
            iax_evr(i+n_evr) = DBLE(iax_curevr(i))
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
       call rdmvrd(iunit,n_evr,max_evr,iti_evr,
     &                iac_evr,iqe_evr,ia1_evr,iax_evr,
     &                chatter,errflg)     
      ENDIF
      IF (errflg.NE.0) THEN
        return
      ENDIF

c DETERMINE INSTRUMENT USED - PSPCB or PSPCC

      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      status = 0
      call crmvlbk(instrume)
      call ftupch(instrume)
      call trinst(instrume,inlen,status)
      IF ((instrume(5:5).NE.'C').AND.(instrume(5:5).NE.'B')) THEN
        status = 0
        date = '21/01/91'
        call dt2sc(date,48043,chg_time,chatter,status)
        IF (status.EQ.0) THEN
         IF (iti_evr(1).LE.chg_time) THEN
          instrume = 'PSPCC'
         ELSE
          instrume = 'PSPCB'
         ENDIF
         errinfo =' instrument determined '
     &//'using time : '//instrume
         call wtinfo(chatter,20,2,errinfo)
        ELSE
         errinfo = ' problem determining instrument name'
         call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF
      ENDIF
         
      status = 0                       
      call ftclos(iunit,status)
      errinfo = ' closing event rate file'
      call wtferr(subname,version,status,errinfo)

c --- CHECK THAT MAP IS APPRORIATE

      call crmvlbk(dmapinstr)
      IF ((dmapinstr(5:5).NE.' ').AND.(instrume(5:5).NE.' ')) THEN
        IF (dmapinstr.NE.instrume) THEN
           errinfo = ' event file/detector map instrument mismatch'
           call wterrm(subname,version,errinfo)
           errinfo = ' event file instrument : '//instrume
           call wtinfo(chatter,0,1,errinfo)
           errinfo = ' detector map instrument : '//dmapinstr
           call wtinfo(chatter,0,1,errinfo)
           errflg = 1
           return
        ENDIF
      ENDIF
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
        errinfo = ' fatal error reading gti file'
        call wterrm(subname,version,errinfo)
        return
      ENDIF
      status = 0
      call ftclos(iunit,status)
      ELSE
        n_gti = 1
        start(1) = iti_evr(1)
        stop(1) = iti_evr(n_evr) + 1
      ENDIF
      subinfo = ' all data has been read'
      call wtinfo(chatter,20,2,subinfo)
      return
      end
c ------------------------------------------------------------------
c     END OF EXP_RDAT
c ------------------------------------------------------------------
                    


*+EXP_MAP
c     ------------------------------------------------------
      subroutine exp_map(map_max,chanmin,chanmax,TMAP,
     &             max_att,n_att,it1_cas,iro_cas,
     &             ixn_cas,iyn_cas, 
     &             max_evr,n_evr,iti_evr,iac_evr,iqe_evr,
     &             ia1_evr,iax_evr,gti,
     &             max_gti,n_gti,start,stop,exparr,
     &             chatter,errflg) 
c     ------------------------------------------------------
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      integer errflg,chatter
      logical gti

c VARIABLES RELATED TO DETECTOR MAP ...
      integer map_max,chanmin,chanmax
      real*4 TMAP(map_max,map_max)

c VARIABLES RELATED TO ATTITUDE DATA ...
      integer max_att,n_att
      real*8 it1_cas(*),iro_cas(*)
      real*8 ixn_cas(*)
      real*8 iyn_cas(*)

c VARIABLES RELATED TO EVENT RATE DATA ...
      integer max_evr,n_evr
      real*8 iti_evr(*)
      integer*4 iac_evr(*),iqe_evr(*)
      integer*4 ia1_evr(*),iax_evr(*)

c VARIABLES RELATED TO GTI DATA ...
      integer max_gti,n_gti
      real*8 start(*),stop(*)

c VARIABLES RELATED TO EXPOSURE MAP ...
      real*4 exparr(map_max,map_max)

c --- DESCRIPTION ------------------------------------------------------
c
c This subroutine is a modification on STEVE SNOWDEN's CAST_EXP
c program. NOTE: Only the i/o has been changed slightly, that is the
c data is passed to this routine in arrays, in the original it was read
c row by row. CAST_EXP description ...
c
c        PROGRAM CAST_EXP
C
C  Author: Steve Snowden
C  Date:   18 December 1992
C
C  Program CAST_EXP is used to cast the band-correct exposure map for
C  pointed observations.  It uses detector maps created from survey
C  data.
C
C  This program has been updated to read the required data directly
C  from the observation data set fits files, ATTITUDE and EVENTRATES,
C  with FITSIO, except for the accepted time intervals.  It requires 
C  an accepted time file ACTIME.DAT.  This file can be created from 
C  the accepted times given in the observation data set but I suggest 
C  verifying the times by hand.
C
C  The program follows the suggestions of Snowden et al. (1992, ApJ, 
C  393 819) and Plucinsky et al. (1993, ApJ, in press) to exclude
C  regions of the PSPC near the edges of the PSPC which are strongly
C  affected by the particle background, the "bright line" regions.
C  The program also assumes that a selection has been done on the
C  data to exclude all events which follow within 0.35 ms of a 
C  "precursor" event.  This excludes some of the low pulse-height
C  crud which affects data collected after May 1992.
C
c Authors/Modification History
c   Steve Snowden     (.....:93 Oct 01) original (US data) version
c   Rehana Yusaf      (1.0.0:93 Oct 07) FTOOLized version
c   Ian M George      (1.0.1:93 Oct 07) minor cosmetics
c   Rehana Yusaf      (1.0.2:96 Jan 4) add screen display routines - wtinfo etc
c
c Banashree M Seifert (1.1.0:96 Aug) 
c       . for modification to run on LINUX, pi is intorduced
c         so pi is defined
c
c Banashree M Seifert (1.2.0:96 Dec)
c       . exparr array was not initialised and so was causing error
c         for OSF machine
c --------------------------------------------------------------------------
      character(5) version
      parameter (version = '1.2.0')
*-
C The following variables were read directly from i/p files in the original 
c CAST_EXP code. In this task, the required files are read externally, and
c the data passed as arrays into this subroutine. In order to minimize the
c changes to the original code, the variables are set to the relevant array 
c entry at the appropriate point. The following is a list of comments from 
c CAST_EXP regarding the meaning of these variables:
C  From the attitude entry:
C      ITEMP  line counter from the MIDAS output, ignore
C      DSCS   double precision space craft clock seconds
C      IX     pointing offset from nominal position, X direction
C      IY     pointing offset from nominal position, Y direction
C      IROLL  detector roll angle
C  From the eventrates entry:
C      ITEMP   line counter from the MIDAS output, ignore
C      ILIVEN  interger space craft clock seconds
C      IA1LL   A1LL scaler count rate
C      IAEXE   AEXE scaler count rate
C      IAXE    AXE scaler count rate
C      MV      MV (master veto) count rate

c Internals (from CAST_EXP)
c      INTEGER MAXREC
c        PARAMETER MAXREC=3000
C
      INTEGER*4 KK,IA,IAA, IA1LL,IAEXE, IAXE, IACTBE, IACTEN 
      INTEGER*4 IASP(3,10000),I,IE,IERR,IG,II,III,ILIVEN
      INTEGER*4 IR,IROLL,ISCS,ISCSO,IX,IXX,IY 
      INTEGER*4 IYY,MV,NB, NB1, NB2 
      INTEGER*4 NB3,  NG, NROWSA, NROWSE
C
      REAL*4 A, A1LL, AEXE, ANGLE, ASP(10000), AXE
      REAL*4 COSROL, DEADTP, DELT, EXP, FLIVE
      REAL*4 FLIVE1, FLIVE2, NMV, RMAP(512,512),ROLL,RMV
      REAL*4  SINROL,TEMPAR(512,512) 
      REAL*4  TMV 
      REAL*4 TOTEXP, TOTIME, X, XX, Y, YY
C
      REAL*8 DSCS
      real pi
      integer j

c Internals (introduced for ROSAT FTOOLS version)
        character(70) subinfo
	logical qgti
        integer counter1,counter2
        character(7) subname
        parameter (subname = 'exp_map')

C  The following data statement is for the deadtime scale factor
C   AND MUST BE REMOVED WHEN DEADTIME SUBROUTINE IS MADE AN FTOOL 
        DATA DEADTP /234./
C ---- INIT -----
        ie =0
        ia =0
        iliven =0
        iscso =0
        nmv =0.0
        rmv =0.0
        ng = 0
        nb = 0
        totexp = 0.0
	totime = 0.0
        tmv = 0.0

c --- RY /USER INFO ---

      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,10,1,subinfo)

      pi=4.*atan(1.)

C  Read in the gain
C  THIS WILL NEED TO BE UPDATED/MOVED
       IG = 1

C
C  Center the instrument map, invert the Y-axis, and turn it real.
C  Also, exclude the "bright line" regions of enhanced particle 
C  background as suggested by Plucinsky et al. (1993).  The optical 
C  axis of the detector is at 4119, 3929 in detector coordinates.  The
C  optical axis of the detector is therefore at IX=4119/16=257.4, 
C  IY=3929/16=245.6, all right so far?  The X position are left the
C  same, but the Y positions need to be flipped because of the ROSAT
C  coordinate system has Y increasing going downward.  By the equation
C  514-II-12, pixel 245, which contains the position 245.6 is put in 
C  pixel 257.  The Y position of the center is now at 257.4.  The
C  detector center is (257.4, 257.4)
C
c  this is done in V1.2.0 --> Banashree

         do i=1,map_max
            do j=1,map_max
               exparr(i,j) = 0.0
            enddo
         enddo



        IF(IG .EQ. 1) THEN
            DO I=25,488
                DO II=35,456
                    IF(II .EQ. 35) THEN
                        RMAP(I,514-II-12) = 0.25*TMAP(I,II)
                    ELSEIF(II .EQ. 456) THEN
                        RMAP(I,514-II-12) = 0.44*TMAP(I,II)
                    ELSE
                        RMAP(I,514-II-12) = TMAP(I,II)
                    ENDIF
                ENDDO
            ENDDO
        ELSE
            DO I=25,488
                DO II=47,443
                    IF(II .EQ. 47) THEN
                        RMAP(I,514-II-12) = 0.50*TMAP(I,II)
                    ELSEIF(II .EQ. 443) THEN
                        RMAP(I,514-II-12) = 0.625*TMAP(I,II)
                    ELSE
                        RMAP(I,514-II-12) = TMAP(I,II)
                    ENDIF
                ENDDO
            ENDDO
        ENDIF

C
c **** RY ... set nrowsa and nrowse to passed var ***

        NROWSA = n_att
        NROWSE = n_evr

c Dump info for chatty users
        	write(subinfo,'(A,i12)') 
     &			' Number of att entries', n_att
        	call wtinfo(chatter,20,2,subinfo)
        	write(subinfo,'(A,i12)') 
     &			' Number of evr entries', n_evr
        	call wtinfo(chatter,20,2,subinfo)
        	write(subinfo,'(A,i12)') 
     &			'  Number of gtis ', n_gti
        	call wtinfo(chatter,20,2,subinfo)

c Initialize a few counters
        counter1 = 0
	counter2 = 0

c Start Main DO-LOOP
        DO IAA=1,NROWSA

c *** RY set data from attitude file to passed values ***

            DSCS = it1_cas(IAA) 
            IX = ixn_cas(IAA)
            IY = iyn_cas(IAA)
            IROLL = iro_cas(IAA)

C  Determine the delta time from the last entry

            ISCS = DSCS
            DELT = ISCS - ISCSO
c	    write(*,*) "iscs, iscso,delt"
c	    write(*,*) iscs,iscso,delt
	    
C
C  Flip the Y value for consistancy.  Increasing Y in SASS is downward
C  in declination.  The instrument map has already been flipped
C
            IY = -IY

C  Process the attitude step, first check the accepted time file
C  to see if the attitude step is in an accepted time period 
c  IMG - by looping through the start & stop times of the GTIs
C
C  MFC - added (ISCSO.GE.start(kk).AND.ISCSO.LE.stop(kk))) to accepted
C        time check below

	  	qgti = .false.
                IF (gti) THEN
	 	do kk = 1, n_gti
                  IF (.NOT.qgti) THEN
  		    IF (( ISCS.GE.start(kk).AND. ISCS.LE.stop(kk)).AND.
     &                  (ISCSO.GE.start(kk).AND.ISCSO.LE.stop(kk)))THEN
			qgti = .true.
                 	IACTBE = start(kk)
                  	IACTEN = stop(kk)
		    ENDIF
                  ENDIF
		enddo        
                ELSE
                 qgti = .true.
                 n_gti = 1
                 start(1) = iti_evr(1)
                 stop(1) = iti_evr(n_evr) + 1
                ENDIF       
                
                ISCSO = ISCS

 		IF (qgti) THEN

C  Accepted time, now find the live time.  First sort through the event 
C  rate file to find a close time
C
                    
                    DO WHILE ((ISCS .GT. ILIVEN) .AND. 
     &                      (IE .LT. NROWSE))
                        IE = IE + 1

c *** RY set data from eventrate file to passed values ***

                        ILIVEN = iti_evr(IE)
                        IA1LL = ia1_evr(IE)
                        IAEXE = iqe_evr(IE)
                        IAXE = iax_evr(IE)
                        MV = iac_evr(IE)                        

                        A1LL = IA1LL
                        AEXE = IAEXE
                        AXE = IAXE
                        RMV = MV
                    ENDDO

C
C  Check to see if the detector was actually on
C
                    IF(A1LL .GT. 10.) THEN 

C
C  Calculate the live time fraction.  This is a GRH routine
C  THIS WILL COME OUT SOMEDAY TO BECOME A CALTOOL IN IT OWN RIGHT
C
                        CALL LIVTIM(A1LL,DEADTP,AXE,AEXE,FLIVE1,
     &                      FLIVE2,FLIVE,IERR)
C
C  Calculate the live time fraction with the additional 0.0001 sec
C  deadtime per acepted event.  This is to compensate for the removal
C  of all events less than 0.35 ms after a previous event which removes
C  some of the the AP background.  The nominal deadtime correction is
C  0.25 ms per event.
C
                        FLIVE1 = 1. - 0.0001*AEXE
C
C  The attitude steps should be on 1-second intervals, calculate the 
C  exposure
C
                        EXP = DELT*FLIVE*FLIVE1
                        TOTIME = TOTIME + DELT
c			write(*,*) 'totime, delt, exp'
c			write(*,*) totime, delt, exp
                        TOTEXP = TOTEXP + EXP
C
C  Sum the MV count rate to find an average value
C
                        TMV = TMV + RMV
                        NMV = NMV + 1.
C
C  Set the X, Y, and ROLL values for the aspect array.  X,Y Steps are 
C  in units of 14.94733 arc seconds (historical reasons).  ROLL steps
C  are in units of 0.2076 degrees.
C  We will want the central sky pixel to be 257,257.  Since the optical
C  axis is at 257.4, 257.4, values of the RA and Dec offsets from 
C  FLOAT(IX)/29.894656=-0.1 to FLOAT(IX)/29.894656=0.9 should have zero 
C  shifts.
C
                        X = 1000.1 + FLOAT(IX)/29.894656
                        IX = X - 1000
                        Y = 1000.1 + FLOAT(IY)/29.894656
                        IY = Y - 1000
                        ROLL = 20000.5 + FLOAT(IROLL)/1494.733
                        IR = ROLL - 20000

C
C  Add to the aspect list
C
                        IF(IA .GT. 0) THEN
                            NB1 = 1
                            DO I=1,IA
                                IF((IX .EQ. IASP(1,I)) .AND. 
     &                                  (IY .EQ. IASP(2,I)) .AND.
     &                                  (IR .EQ. IASP(3,I))) THEN
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
                    counter2 = counter2 + 1
                ENDIF
        	counter1 = counter1 + 1
        ENDDO

C  Print out diagnostic information
c
        	write(subinfo,'(A,i12)') 
     &		   ' Total Number of entries considered ', counter1
        	call wtinfo(chatter,20,2,subinfo)
        	write(subinfo,'(A,i12)') 
     &		   ' Number of entries within a GTI     ', counter2
        	call wtinfo(chatter,20,2,subinfo)
C
	IF (chatter.GE.5) THEN
        	write(subinfo,'(A,i12)') 
     &			' Number of unique detector positions', IA
        	call wtinfo(chatter,5,1,subinfo)
        	write(subinfo,'(A,i12)') 
     &			' Number of entries when Detector ON ', NG
        	call wtinfo(chatter,5,1,subinfo)
        	write(subinfo,'(A,i12,A)') 
     &			' Number of entries when Detector OFF', NB,
ccc     &			' (A1LL c/rate<10)'
     &			' (ALL c/rate<10)'
        	call wtinfo(chatter,5,1,subinfo)
        	write(subinfo,'(A,f15.5,A)') 
     &			' Total ONTIME        ', totime, ' s'
        	call wtinfo(chatter,5,1,subinfo)
        	write(subinfo,'(A,f15.5,A)') 
     &			' Total LIVETIME      ', totexp, ' s'
        	call wtinfo(chatter,5,1,subinfo)
        	TMV = TMV/NMV
        	write(subinfo,'(A,f15.5,A)') 
     &			' Average MV c/rate   ', TMV, ' count/s'
        	call wtinfo(chatter,5,1,subinfo)
	ENDIF

c 
c --- Error checking adding by RY ---
c
      IF (IA.LE.0) THEN
       subinfo = 'cannot process map with 0 detector pos'
       call wterrm(subname,version,subinfo)
       errflg = 2
       return
      ENDIF
      IF (NG.LE.0) THEN
       subinfo = ' cannot process map with no entries'
       call wterrm(subname,version,subinfo)
       errflg = 2
       return
      ENDIF
      IF (totime.LE.0) THEN
       subinfo = ' cannot process map with no ONTIME '  
       call wterrm(subname,version,subinfo)
       errflg = 2
       return
      ENDIF
      IF (totexp.LE.0) THEN
       subinfo = ' cannot process map with no LIVETIME'  
       call wterrm(subname,version,subinfo)
       errflg = 2
       return
      ENDIF
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
                ANGLE = IR*0.2076017
c MJT 16July96 (g77/linux) cosd,sind non-portable!
c                COSROL = COSD(ANGLE)
c                SINROL = SIND(ANGLE)
                 COSROL = COS(ANGLE*(pi/180.0))
                 SINROL = SIN(ANGLE*(pi/180.0))
C
C  Zero the temp array
C
                DO II=25,488
                    DO III=25,488
                        TEMPAR(II,III) = 0.
                    ENDDO
                ENDDO
C
C  Calculate the rotated array only once for each roll angle
C  Remember, the optical axis is at 257.4, 257.4, so pixel 257,257
C  (whose center is 257.5, 257.5) is 0.1,0.1 pixels offset from the
C  center.
C
                DO II=25,488
                    DO III=25,488
                        IF(RMAP(II,III) .NE. 0.) THEN
                            X = (II - 256.9)
                            Y = (III - 256.9)
                            XX = COSROL*X + SINROL*Y
                            YY = COSROL*Y - SINROL*X
                            IXX = XX + 257.4
                            IYY = YY + 257.4
                            TEMPAR(IXX,IYY) = 
     &                              TEMPAR(IXX,IYY) + RMAP(II,III)
                        ENDIF
                    ENDDO
                ENDDO
            ENDIF
            NB1 = IR
C
C  Cast the exposure
C
            DO II=25,488
                IXX = II + IX
                DO III=25,488
                    IF(TEMPAR(II,III) .NE. 0.) THEN
                        IYY = III + IY
                        if ((IYY.lt.1 .or. IYY.gt.map_max) .or.
     &                    (IXX.lt.1 .or. IXX.gt.map_max)) THEN
                          write(*,*) 'Array pixel ',ixx,iyy,
     &		 		' beyond array size 0 to ',map_max
			ELSE
                          exparr(IXX,IYY) = exparr(IXX,IYY) +
     &                                  ASP(I)*TEMPAR(II,III)
			ENDIF
                    ENDIF
                ENDDO
            ENDDO
        ENDDO

        return
        END
c ------------------------------------------------------------------------
c       END OF EXP_MAP
c ------------------------------------------------------------------------ 


*+EXP_WT
c     -----------------------------------------------------------------
      subroutine exp_wt(outfil,dmapfil,attfil,evrfil,gtifil,instrum,
     &                  chanmin,chanmax,map_max,exparr,crval1,crval2,
     &                  taskname,killit,chatter,errflg)
c     -----------------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c This subroutine writes an exposure map FITS file.
c
c ---------------------------------------------------------------------
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) outfil,dmapfil,attfil,evrfil,gtifil,instrum
      character*(*) taskname
      integer map_max,errflg,chatter
      real*4 exparr(map_max,map_max)
      integer chanmin,chanmax
      logical killit
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
c Rehana Yusaf (Dec 22 1995) 1.0.2; add crval1 and crval2 as arguments
c                                   change cdelt values to 14.947/3600
c                                   add wtinfo etc
      character(5) version
      parameter (version = '1.0.2')
*-
c ---------------------------------------------------------------------
c
c INTERNALS ...
c
      integer ounit,block,status,clenact,nk_hist,nk_comm
      character(70) hist(2),comments(2)
      character(8) telescop,detnam,filter,radecsys
      character(8) cunit1,cunit2,ctype1,ctype2,bunit
      real equinox,crval1,crval2,cdelt1,cdelt2,crpix1,crpix2
      logical deadapp,vignapp
      character(70) subinfo
      character(6) subname
      parameter (subname = 'exp_wt')
c
c --- USER INFO
c
      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,10,1,subinfo)
c
c --- OPEN NEW FILE ---
c
      status = 0
      block = 2880
      call cgetlun(ounit)
      call opfits(ounit,outfil,killit,chatter,status)
      subinfo = ' opening output file !'
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
      crpix1 = 256.5
      crpix2 = 256.5
      cunit1 = 'deg'
      cunit2 = 'deg'
      bunit =  's'
      cdelt1 = - 4.1520300e-03
      cdelt2 = cdelt1 *(-1)
      bunit = 's'
                
c
c --- CALL ROUTINE WHICH WRITES DATA AND HEADER ---
c
      nk_hist = 0
      nk_comm = 0
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
      subinfo = ' writing CHANMIN keyword'
      call wtfwrn(subname,version,chatter,9,status,subinfo)
 
      status = 0
      call ftpkyj(ounit,'CHANMAX',chanmax,
     &'Maximum PI channel for image',status)
      subinfo = ' writing CHANMAX keyword'
      call wtfwrn(subname,version,chatter,9,status,subinfo)

      status = 0
      call ftpkys(ounit,'GTIFILE',gtifil(1:clenact(gtifil)),
     &'Accepted time intervals file',status)
      subinfo = ' writing GTIFILE keyword'
      call wtfwrn(subname,version,chatter,15,status,subinfo)

      status = 0
      call ftpkys(ounit,'EVRFILE',evrfil(1:clenact(evrfil)),
     &'Events rate file',status)
      subinfo = ' writing EVRFILE keyword'
      call wtfwrn(subname,version,chatter,15,status,subinfo)

      status = 0
      call ftpkys(ounit,'ASPFILE',attfil(1:clenact(attfil)),
     &'Aspect offsets file',status)
      subinfo = ' writing ASPFILE keyword'
      call wtfwrn(subname,version,chatter,15,status,subinfo)

      status = 0
      call ftpkys(ounit,'DETMFILE',dmapfil(1:clenact(dmapfil)),
     &'Detector map file',status)
      subinfo = ' writing DETMFILE keyword'
      call wtfwrn(subname,version,chatter,15,status,subinfo)

      status = 0
      call ftpdat(ounit,status)
      status = 0
      call FTPKYS(ounit,'CREATOR',
     &                  taskname,
     &             's/w task which wrote this dataset',
     &                  status)
      subinfo = ' problem writing CREATOR keyword'
      call wtfwrn(subname,version,chatter,15,status,subinfo)

      status = 0
      call ftclos(ounit,status)
      subinfo = ' closing output file !'
      call wtferr(subname,version,status,subinfo)
      IF (status.NE.0) THEN
        errflg  = 1
      ENDIF
      return
      end
c ----------------------------------------------------------------------
c     END OF EXP_WT 
c ----------------------------------------------------------------------

*+EXP_FORM
c     ______________________________________________________
      subroutine exp_form(attfil,evrfil,gtifil,gti,gti_data,
     &              evtfil,form,max_att,max_curatt,max_evr,
     &              max_curevr,crval1,crval2,errflg,chatter)
c     ______________________________________________________
c ___ DESCRIPTION ______________________________________________
c This routine determines which file format is read, that is
c US REV0 or RDF 
c ___ VARIABLES ________________________________________________ 
c
      IMPLICIT NONE
      character*(*) attfil,evrfil,gtifil,form,evtfil
      character*(*) gti_data
      integer errflg,chatter
      real crval1,crval2
      integer max_att,max_curatt,max_evr,max_curevr
      logical gti
c 
c ___ AUTHORS/MODIFICATION _____________________________________ 
c 
c Rehana Yusaf March 18 1994 1.0.0;
c
c Rehana Yusaf August 24 1994 1.0.1; add max_att,max_curatt
c                                    ,max_evr,max_curevr as
c                                    arguments. This routine
c                                    now determines the maximum
c                                    array dimensions
c Rehana Yusaf (Dec 22 1995) 1.0.2; add crval1 and crval2 as arguments
c                                   add evtfil as parameter and wtinfo etc

      character(5) version
      parameter (version='1.0.2')
*-
c ______________________________________________________________ 
c
c --- LOCALS ---
c
      integer iunit,block,rwmode,htype,i,status
      character(30) comm
      character(70) subinfo,errinfo
      character(180) filename 
      character(3) att_data,evr_data
      character(8) ext_name 
      character(20) extname(50),outver(9,50),outhdu(9,50)
      integer nsearch,nfound,next(50),extnum,rows,ninstr
      character(20) instr(9)
      logical rd_radec
      character(8) subname
      parameter (subname = 'exp_form')
c     
c --- USER INFO ---
c
      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,10,1,subinfo)
c
c --- OPEN AND READ ATTITUDE ---
c
      block = 2880
      rwmode = 0
      call fcpars(attfil,filename,extnum,errflg)
      call cgetlun(iunit)
      call ftopen(iunit,filename,rwmode,block,errflg)
      errinfo = ' opening attitude file'
      call wtferr(subname,version,errflg,errinfo)
      IF (errflg.NE.0) THEN
        return
      ENDIF
c 
c --- SEARCH ATTITUDE FILE FOR US DATA EXTNAME ---
c
      nsearch = 50
      call fndext(chatter,iunit,'CORAS',nsearch,nfound,
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
          errinfo = ' unsupported format for attitude file'
          call wterrm(subname,version,errinfo) 
          errflg = 2
          return
        ENDIF
      ENDIF


c DETERMINE MAX ARRAY SIZES FOR ASPECT DATA

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

      IF ( chatter.GE.30) THEN
       write(errinfo,'(a,i12)')'Max aspect array dimension:'
     & ,max_att
       call wtinfo(chatter,30,2,errinfo)
       write(errinfo,'(a,i12)')'Max aspect local array size:'
     & ,max_curatt
       call wtinfo(chatter,30,2,errinfo)
      ENDIF
c
c --- OPEN AND READ EVENT RATE FILE ---
c
      call fcpars(evrfil,filename,extnum,errflg)
      call cgetlun(iunit)
      call ftopen(iunit,filename,rwmode,block,errflg)
      errinfo = ' opening events file'
      call wtferr(subname,version,errflg,errinfo)
      IF (errflg.NE.0) THEN
        return
      ENDIF               
c
c --- DETERMINE EVENTS FILE FORMAT ---
c
      nsearch = 50
      nfound = 0
      call fndext(chatter,iunit,'EVRAT',nsearch,nfound,
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
          errinfo = ' unsupported format for events file'
          call wterrm(subname,version,errinfo)
          errflg = 2
          return
        ENDIF
      ENDIF

c DETERMINE MAX ARRAY SIZES FOR EVENTS DATA

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
      errinfo = ' closing events file'
      call wtferr(subname,version,errflg,errinfo)
      IF ( chatter.GE.30) THEN
       write(errinfo,'(a,i12)')'Max evrate array dimension:'
     & ,max_evr
       call wtinfo(chatter,30,2,errinfo)
       write(errinfo,'(a,i12)')'Max evrate local array size:'
     & ,max_curevr
       call wtinfo(chatter,30,2,errinfo)
      ENDIF
c
c --- OPEN AND READ GTI FILE ---
c
      IF (gti) THEN
      call fcpars(gtifil,filename,extnum,errflg)
      call cgetlun(iunit)
      call ftopen(iunit,filename,rwmode,block,errflg)
      errinfo = ' opening gti file'
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
          errinfo = ' unsupported format for GTI file'
          call wterrm(subname,version,errinfo)
          errflg = 2
          return
        ENDIF
      ENDIF
      call ftclos(iunit,errflg)
      errinfo = ' closing gti file'
      call wtferr(subname,version,errflg,errinfo)
      ENDIF
c
c --- CHECK THAT ALL FILES HAVE THE SAME FORMAT ---
c
      form = att_data
      IF (evr_data.NE.form) THEN
        errinfo = ' Format mismatch !'
        call wterrm(subname,version,errinfo)
        errinfo = ' attitude data : '//att_data
        call wtinfo(chatter,0,1,errinfo)
        errinfo = ' event rate : '//evr_data
        call wtinfo(chatter,0,1,errinfo)
        IF (gti) THEN
          errinfo = ' gti data : '//gti_data
          call wtinfo(chatter,0,1,errinfo)
        ENDIF
        errflg = 3
        return
      ENDIF

c
c --- READ NOMINAL POINTING DIRECTION FROM PRIMARY ARRAY OF ATT FILE for RDF
c
      rd_radec = .false.
      status = 0
      IF (form.EQ.'RDF') THEN
        call fcpars(attfil,filename,extnum,status)
        call cgetlun(iunit)
        status = 0
        call ftopen(iunit,filename,rwmode,block,status)
        errinfo = ' opening attitude file'
        call wtferr(subname,version,errflg,errinfo)
        IF (status.NE.0) THEN
          status = 0
          call ftclos(iunit,status)
          errflg = 2
          return
        ENDIF
        status = 0
        call ftgkye(iunit,'RA_NOM',crval1,comm,status)
        errinfo = ' reading RA_NOM value from primary array'
        call wtfwrn(subname,version,chatter,5,status,errinfo)
        IF (status.NE.0) THEN
          rd_radec = .false.
        ELSE
          rd_radec = .true.
        ENDIF
        call ftgkye(iunit,'DEC_NOM',crval2,comm,status)
        errinfo = ' reading DEC_NOM value from primary array'
        call wtfwrn(subname,version,chatter,5,status,errinfo)
        IF (status.NE.0) THEN
          rd_radec = .false.
        ELSE
          rd_radec = .true.
        ENDIF
      ELSE

c --- FOR US Rev 0 data move to EVENTS EXT and get crvals ---

        IF (evtfil.NE.'NONE') THEN
          ninstr = 2
          instr(1) = 'STDEVT'
          instr(2) = 'ACCEPTED'
          ext_name = 'EVENTS'
          status = 0
          call mvext(0,evtfil,iunit,ninstr,instr,nsearch,next,outhdu,
     &         extname,outver,ext_name,status,chatter)
          status = 0
          call ftgkye(iunit,'CRVAL1',crval1,comm,status)
          errinfo = ' reading RA_NOM value from events extension'
          call wtfwrn(subname,version,chatter,5,status,errinfo)
          IF (status.NE.0) THEN
            rd_radec = .false.
          ELSE
            rd_radec = .true.
          ENDIF
          call ftgkye(iunit,'CRVAL2',crval2,comm,status)
          IF (status.NE.0) THEN
            rd_radec = .false.
          ELSE
            rd_radec = .true.
          ENDIF
        ENDIF
      ENDIF
      status = 0
      call ftclos(iunit,status)
      IF (.NOT.rd_radec) THEN
        status = 0
        call uclgsr('ra_nom',crval1,status)
        IF (status.NE.0) THEN
          errflg = 2
          return
        ENDIF
        call uclgsr('dec_nom',crval2,status)  
        IF (status.NE.0) THEN 
          errflg = 2 
          return
        ENDIF
      ENDIF

      IF (chatter.GE.20) THEN
        subinfo = ' Files are in '//form//' format'
        call wtinfo(chatter,20,2,subinfo)
      ENDIF
      return
      end
c ---------------------------------------------------------------------   
c     END OF EXP_FORM
c ---------------------------------------------------------------------
