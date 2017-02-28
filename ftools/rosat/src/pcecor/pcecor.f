*+PCECOR
c      -----------------
       subroutine pcecor
c      -----------------
c --- DESCRIPTION -----------------------------------------------------
c PCECOR corrects PI channels in a US Rev0 or RDF Events file. 
c
c --- CALLED ROUTINES -------------------------------------------------
c
c PCT_GP     : Gets parameters
c PCT_MDAT   : Moves to desired extension, and determines format
c               US Rev0/RDF supported
c PCT_CWT    : Read each PI value correct it and write (overwrite)
c               the EVENTS file with the corrected file 
c                
c 
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1996 Feb 23) 1.0.0; based on pctcor
c
c Banashree M Seifert (1997, Sept 5) 1.1.0:
c         . some other small modifications for goto 50
c               for any non zero status/errflg, it was pointing to
c               label 50 whereby it says that fails to allocate memory
c               now label 60 is introduced so that label 50 is for only
c               DMA error and other errors for label 60
c         . in the subroutine pce_cwt
c               defining variable desc2 is modified from 80 to 120 char
c               otherwise it was overwriting some other variable and
c               causing problem
c
c Banashree M Seifert (1997, Sept 8) 1.2.0:
c         . introduced nonlin1 subroutine in roslib.
c           For this, a new parameter is added as calflag.
c           if calflag = 1, then use ground calibration data (NONLIN)
c              calflag = other than 1, then use Prieto et al calibration
c                        (NONLIN1)
c
c Peter D Wilson (1998 Jun 30 ) 1.2.1:
c         . Update for new FCPARS Behavior
c Ning Gan (1998, Jun 30) 1.2.2:
c         . Update the date keyword for the new yyyy-mm-dd format.
c toliver (1999 July 12) 1.2.3:
c         . Corrected error handler in PCE_CWT
c Peter D Wilson (1999 Aug 16) 1.2.4:
c         . Replace gtcal with gtcalf
c
c ---------------------------------------------------------------------
      IMPLICIT NONE
      character(5) version
      parameter (version = '1.2.4')
      character(40) taskname
*-
c ---------------------------------------------------------------------
c --- VARIABLES ---
c
      integer max_alkmap
      parameter (max_alkmap = 20)
      character(120) infile,outfile
      character(120) gtifile, desc, alkfile
      character(8) instrume
      logical del_exist
      integer errflg,chatter,lun
      integer p_start,p_stop,gtilun,ngti,status
      character(3) form
      
      integer calflag
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

c --- INITIALISATION ---

      taskname='pcecor'

c --- GET PARAMETERS ---

      errflg = 0
      call pce_gp(infile,outfile,gtifile,alkfile,
     &          del_exist,instrume,calflag,errflg,chatter)
      call wtbegm(taskname,version,chatter)
      IF (errflg.NE.0) THEN
        goto 60
      ENDIF

c --- READ GTI TIMES FROM GTIFILE ---

      call pce_gti(gtifile,gtilun,ngti,errflg,chatter)

      IF (errflg.NE.0) THEN
        goto 60
      ENDIF

      status = 0

      p_start = 0
      call udmget(ngti, 7,p_start,status)
      IF (status.NE.0) THEN
         goto 50
      ENDIF

      p_stop = 0
      call udmget(ngti, 7,p_stop,status)
      IF (status.NE.0) THEN
         goto 50
      ENDIF

  50  IF (status.NE.0) THEN
        desc = ' failed to allocate memory'
        call fcecho(desc)
        errflg=status
        goto 60
      ENDIF

      call pce_rdgti(gtilun,MEMD(p_start),MEMD(p_stop),
     &                ngti,errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 60
      ENDIF

c --- DETERMINE FILE FORMAT, OPEN FILE AND MOVE TO DESIRED EXT ---

      call pce_mdat(infile,outfile,del_exist,form,lun,
     &                 errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 60
      ENDIF

c --- CORRECT PI CHANNEL AND WRITE CORRECTED VALUES TO OUTFILE ---

      call pce_cwt(infile,gtifile,del_exist,lun,form,
     &             MEMD(p_start),MEMD(p_stop),
     &             ngti,alkfile,instrume,calflag,errflg,chatter)

  
      status = 0 
      call udmfre(p_start,7,status)
      IF (status.NE.0) THEN
         desc = ' failed to free memory for start time'
         call fcecho(desc)
      ENDIF 
      status=0
      call udmfre(p_stop,7,status)
      IF (status.NE.0) THEN 
         desc = ' failed to free memory for stop time'
         call fcecho(desc)
         errflg=status
      ENDIF

 60   call wtendm(taskname,version,errflg,chatter)
      return
      end

c ---------------------------------------------------------------------
c     END OF MAIN PCECOR 
c ---------------------------------------------------------------------
    
*+PCE_GP
c     -------------------------------------------------------------
      subroutine pce_gp(infile,outfile,gtifile,alkfile,
     &          del_exist,instrume,calflag,errflg,chatter)
c     -------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile
      character*(*) alkfile
      character*(*) gtifile
      character(120) filename,gtifilename
      character(120) ill_files(5)
      character*(*) instrume
      integer errflg,chatter,status,n_ill
      character(70) desc
      integer fcstln,flen,extnum
      logical ext,valfil,del_exist,pce

      integer calflag
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c infile     char   : input file name
c outfile    char   : Output filename
c chatter    int    : Chattiness flag, >20 verbose
c errflg     int    : Error flag
c calfla     int    : flag for calibration data to be used
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
c Rehana Yusaf (1996 Feb)
c
c Banashree M Seifert (1997, Sept 8) 1.1.0:
c       . new parameter calflag added so that user can ask for any of
c         the two calibration data
c         calflag = 1 --> ground calibration to be used
c                 = other than 1 --> Prieto et al data to be used
c Peter D Wilson (1998 Jun 30 ) 1.1.1:
c         . Update for new FCPARS Behavior
c Peter D Wilson (1999 Aug 16) 1.1.2:
c         . Replace gtcal with gtcalf
c
c -------------------------------------------------------------------
      character(5) version 
      parameter (version = '1.1.2')
      character(6) subname
      parameter (subname = 'pce_gp')
*-
c ---------------------------------------------------------------------
c

c --- VARIABLES FOR MVEXT ---

      integer nsearch,ninstr,nfound,htype,nret
      parameter (nsearch = 50)
      integer maxret
      parameter (maxret = 4)
      integer next(nsearch),mvunit,extno(maxret)
      integer st_sctime,end_sctime
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9),comm
      character(180) tfile,files(maxret)
C      character(8) date,time,extname,online(maxret),date_end
      character(68) date,time,date_end
      character(8) extname,online(maxret)

      integer yy,mm,dd,inlen
      logical cal


c GET INFILE

      status = 0
      call uclgst('infile',infile,status)
      IF (status.NE.0) THEN
        desc = ' getting infile parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(infile)
      IF (infile.EQ.'  ') THEN
        errflg = 1
        desc = ' input file not entered !'
        call wterrm(subname,version,desc)
        return
      ENDIF
C PDW 6/30/98: Don't bother! Let FTOPEN decide if file exists
C      call fcpars(infile,filename,extnum,status)
C      ext = .true.
C      flen = fcstln(filename)
C      INQUIRE(FILE=filename(:flen),EXIST=ext)
C      IF (.NOT.ext) THEN
C        errflg = 1
C        desc = ' file does not EXIST :'//filename
C        call wterrm(subname,version,desc)
C        return
C      ENDIF
      n_ill = 1
      call ftrtnm( infile, filename, status )
      ill_files(1) = filename 

c GTIFILE PARAMETER

      call uclgst('gtifile',gtifile,status)
      IF (status.NE.0) THEN
       desc = ' getting gtifile parameter !'
       call wterrm(subname,version,desc)
       errflg = 1
       return
      ENDIF
      call crmvblk(gtifile)
      call fcpars(gtifile,gtifilename,extnum,status)
      IF ((gtifilename(1:1).EQ.'%').OR.(gtifile.EQ.' ')) THEN  
       gtifile = filename
      ELSE
C PDW 6/30/98: Don't bother! Let FTOPEN decide if file exists
C       INQUIRE(FILE=gtifilename,EXIST=ext)
C       IF (.NOT.ext) THEN
C         desc = ' gtifile does not exist !'
C         call wterrm(subname,version,desc)
C         errflg = 1
C         return
C       ENDIF
       n_ill = n_ill + 1
       call ftrtnm( gtifile, gtifilename, status )
       ill_files(n_ill) = gtifilename
      ENDIF

c CLOBBER PARAMETER

      status = 0
      call uclgsb('clobber',del_exist,status)
      IF (status.NE.0) THEN
        desc = ' getting del_exist parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF

c GET CHATTER

      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        desc = ' getting chatter parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF

c GET INSTRUME AND DATE-OBS values from infile

      cal = .true.
      ninstr = 2
      instr(1) = 'EVENTS'
      instr(2) = 'ACCEPTED'
      extname = 'EVENTS'
      nfound = 0
      call mvext(0,infile,mvunit,ninstr,instr,nsearch,next,outhdu,
     &         extnames,outver,extname,errflg,chatter)
      IF (errflg.NE.0) THEN
        status = 0
        call ftmahd(mvunit,1,htype,status)
        desc = ' moving to extension 0'
        call wtferr(subname,version,status,desc)
        IF (status.NE.0) THEN
          errflg = 1
          return
        ENDIF
        errflg = 0
        ninstr = 2
        instr(1) = 'EVENTS'
        instr(2) = 'ACCEPTED'
        extname = 'STDEVT'
        call mver(mvunit,extnum,ninstr,instr,nsearch,next,
     &           outhdu,extnames,outver,extname,extname,chatter)
        IF (errflg.NE.0) THEN
          desc = ' moving to EVENTS extension in infile'
          call wterrm(subname,version,desc)
          errflg = 2
          return
        ENDIF
      ENDIF 

      status = 0
      call ftgkys(mvunit,'INSTRUME',instrume,comm,status)
      status = 0
      call ftgkys(mvunit,'DATE-OBS',date,comm,status)
      desc = ' reading DATE-OBS keyword'
      call wtferr(subname,version,status,desc)
      IF (status.NE.0) THEN
        errflg = 2
        return
      ENDIF
      call ftgkys(mvunit,'TIME-OBS',time,comm,status)
      desc = ' reading TIME-OBS keyword'
      call wtferr(subname,version,status,desc)
      status = 0
      call dt2sc(date,48043,st_sctime,chatter,status)
      status = 0
      call ftgkys(mvunit,'DATE-END',date_end,comm,status)
      IF (status.NE.0) THEN
        status = 0
        call ftgkys(mvunit,'DATE_END',date_end,comm,status)
        desc = ' reading DATE_END keyword'
        call wtferr(subname,version,status,desc)
      ENDIF
      call dt2sc(date_end,48043,end_sctime,chatter,status)
       
      status = 0
      pce = .false.
      call ftgkyl(mvunit,'PCECORF',pce,comm,status)
      IF (pce) THEN
        call fcecho('  ')
        desc = 
     &  ' PCECOR has already been run on'
     &//' this dataset.'
        call wtwarm(subname,version,chatter,0,desc)
        desc = ' Rerun may produce inaccurate results'
        call wtinfo(chatter,0,1,desc)
      ENDIF 
      status = 0
      call ftclos(mvunit,status)
      call crmvlbk(instrume)
      call ftupch(instrume)
      call trinst(instrume,inlen,status)
      IF ((instrume(5:5).NE.'C').AND.(instrume(5:5).NE.'B')) THEN
        status = 0
	call fts2dt(date, yy, mm, dd, status)
C        read(date(1:2),50,IOSTAT=status) dd
C       read(date(4:5),50,IOSTAT=status) mm
C       read(date(7:8),50,IOSTAT=status) yy
C 50    format(i2)
        IF (status.NE.0) THEN
          desc = ' parsing DATE-OBS value'
          call wterrm(subname,version,desc)
          desc = ' cannot use CALDB option to determine calfiles'
     &//' as instrument not known !'
          call wtwarm(subname,version,chatter,0,desc)
          cal = .false.
          errflg = 2
          return
       ELSE
C         IF ((yy.LE.91).AND.(mm.LE.1).AND.(dd.LE.21)) THEN
         IF ((yy.LE.1991).AND.(mm.LE.1).AND.(dd.LE.21)) THEN
           instrume = 'PSPCC'
         ELSE
           instrume = 'PSPCB'
         ENDIF
         IF (chatter.GE.20) THEN
           desc = ' determined instrument by parsing date :'
     &            //instrume
           call wtinfo(chatter,20,2,desc)
         ENDIF
       ENDIF
      ENDIF


c GET ALKFILE

      status = 0
      call uclgst('alkfile',alkfile,status)
      IF (status.NE.0) THEN
         desc = ' getting alkfile parameter !'
         call wterrm(subname,version,desc)
         errflg = 1
         return
      ENDIF
      call crmvlbk(alkfile)
      tfile = alkfile
      call ftupch(tfile)
      IF (tfile.EQ.'CALDB') THEN
       call gtcalf(chatter,'ROSAT',instrume,'-','-','GAIN_HIST',
     &             date,time,date,time,'-',1,files,extno,
     &             online, nret,nfound,status)
       IF (nfound.EQ.0) THEN
         errflg = 2
         return
       ENDIF
       alkfile = files(1)
       IF (chatter.GE.20) THEN
         desc = ' CALDB selection for alkfile:'
         call wtinfo(chatter,20,2,desc)
         call wtinfo(chatter,20,3,alkfile)
       ENDIF
      ELSE 
       IF (alkfile.EQ.' ') THEN
         desc = ' no alkfile entered !'
         call wterrm(subname,version,desc)
         errflg = 2
       ENDIF
       flen = fcstln(alkfile)
       INQUIRE(FILE=alkfile(:flen),EXIST=ext)
       IF (.NOT.ext) THEN
        errflg = 1
        desc = ' File does not EXIST :'//alkfile
        call wterrm(subname,version,desc)
        return
       ENDIF
      ENDIF       
      n_ill = n_ill + 1
      ill_files(n_ill) = alkfile

c GET OUTFILE 

      status = 0
      call uclgst('outfile',outfile,status)
      IF (status.NE.0) THEN
        desc = ' getting outfile parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF

c OUTFILE VALIDATION 

      call crmvlbk(outfile)
      IF (outfile.EQ.'  ') THEN
        desc = ' outfile must be entered !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF
      call ck_file(outfile,ill_files,n_ill,valfil,del_exist,chatter)
      IF (.NOT.valfil) THEN
        errflg = 2
        return
      ENDIF
      IF (outfile(1:1).EQ.'!') THEN
        outfile = outfile(2:)
      ENDIF

c GET calflag parameter

      status = 0
      call uclgsi('calflag',calflag,status)
      IF (status.NE.0) THEN
        desc = ' getting calflag parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF

      
      return
      end
c ---------------------------------------------------------------------
c     END OF PCE_GP
c ---------------------------------------------------------------------

*+PCE_GTI
c     -------------------------------------------------------     
      subroutine pce_gti(gtifile,gtilun,ngti,errflg,chatter)
c     -------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c     This subroutine opens a file and locates the FITS gti ext
c     and determines the max array size.
c --- VARIABLES -------------------------------------------------
c
      IMPLICIT NONE
      character*(*) gtifile
      integer gtilun,ngti,errflg,chatter
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------
c
c                              Feb 23 1996, v 1.0.0; Rehana Yusaf
      character(5) version
      parameter (version = '1.0.0')
      character(7) subname
      parameter (subname = 'pce_gti')
*-
c ---------------------------------------------------------------
c
      character(80) errinfo,desc,filename
      integer nsearch,nfound,extnum,block,htype
      parameter (nsearch = 50)
      integer next(nsearch),status
      character(20) outhdu(9,nsearch),comm
      character(20) outver(9,nsearch)
      character(8) extname


      desc = ' using '//subname//' '//version
      call wtinfo(chatter,10,1,desc)

c OPEN GTIFILE

      nfound = 0 
      call fcpars(gtifile,filename,extnum,errflg)
      call cgetlun(gtilun)
      call ftopen(gtilun,filename,0,block,errflg)
      errinfo = ' opening gtifile'
      call wtferr(subname,version,errflg,errinfo)
      IF (errflg.NE.0) THEN
        return
      ENDIF

      IF (extnum.GT.0) THEN
        call ftmahd(gtilun,extnum+1,htype,errflg)
        IF (errflg.NE.0) THEN
          errinfo = ' moving to GTI extension'
          call wtferr(subname,version,errflg,errinfo)
          goto 100
        ENDIF 
      ELSE
        call fndext(chatter,gtilun,'GTI',nsearch,nfound,
     &            next,outhdu,outver,extname,errflg)
        IF (nfound.EQ.0) THEN
          call fndext(chatter,gtilun,'STDGTI',nsearch,nfound,
     &            next,outhdu,outver,extname,errflg)
         IF (nfound.EQ.0) THEN
          errinfo = ' unsupported format for GTI file'
          call wterrm(subname,version,errinfo)
          goto 100
         ENDIF
        ENDIF
        call ftmahd(gtilun,next(1)+1,htype,errflg)
        IF (errflg.NE.0) THEN 
          errinfo = ' moving to GTI extension'
          call wterrm(subname,version,errinfo)
          goto 100
        ENDIF 
      ENDIF 

c DETERMINE MAX ARRAY VALUES

      call ftgkyj(gtilun,'NAXIS2',ngti,comm,errflg)
      IF (errflg.NE.0) THEN
        errinfo=' reading NAXIS2 value from GTI file'
        call wtferr(subname,version,status,errinfo)
      ENDIF

 100  IF (errflg.NE.0) THEN
        status = 0
        call ftclos(gtilun,status)
        errinfo = ' closing gti file'
        call wtferr(subname,version,status,errinfo)
      ENDIF
      return
      end
c -------------------------------------------------------------------
c     END OF PCE_GTI
c -------------------------------------------------------------------

*+PCE_RDGTI
c     -------------------------------------------------------- 
      subroutine pce_rdgti(gtilun,start,stop,ngti,
     &               errflg,chatter)
c     --------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c
c This routine reads a GTI extension
c ----------------------------------------------------------------
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      integer gtilun,errflg,chatter,ngti
      real*8 start(ngti),stop(ngti)
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c     Rehana Yusaf, Oct 13, ver 1.0.0;
c
      character(5) version
      parameter (version = '1.0.0')
      character(9) subname
      parameter (subname = 'pce_rdgti')
*-
c ----------------------------------------------------------------
c
      character(70) desc,errinfo
      integer status 


      desc = ' using '//subname//' '//version
      call wtinfo(chatter,10,1,desc)

      call rdgti1(gtilun,ngti,ngti,start,stop,chatter,errflg)
      status = 0
      call ftclos(gtilun,status)
      errinfo = ' closing gtifile'
      call wtferr(subname,version,status,errinfo)
      return
      end
c ----------------------------------------------------------------
c     END OF PCE_RDGTI
c ----------------------------------------------------------------
*+PCE_MDAT
c     --------------------------------------------------------- 
      subroutine pce_mdat(infile,outfile,del_exist,form,lun,
     &                       errflg,chatter)
c     ----------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c This routine moves to the desired extension in the input file.
c ------------------------------------------------------------------
c --- VARIABLES ---
c 
      IMPLICIT NONE
      character*(*) infile,outfile
      logical del_exist
      integer errflg,chatter,lun
      character(3) form
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c ierr       int    : Error flag, 0 is okay
c chatter    int    : Chattiness flag >20 verbose, <5 quiet
c infile     char   : input filename
c
c --- CALLED ROUTINES ----------------------------------------------
c
c MVEXT : Locate and move to desired extension
c
c ---- LINKING/COMPILATION -----------------------------------------
c
c FITSIO,CALLIB,FTOOLS LIBRARY
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1994 Nov)
c Peter Wilson (1998 Jun) 1.0.1 : Update for new FCPARS Behavior
c
      character(5) version
      parameter (version = '1.0.1')
      character(8) subname
      parameter (subname = 'pce_mdat')
*-
c ------------------------------------------------------------------
c --- LOCALS ---
c
      character(70) subinfo,errinfo
      character(80) filename
      integer nfound,imove,status,htype,i,extnum
      logical overwrite

c VARIABLES FOR FNDEXT

      integer nsearch
      parameter (nsearch = 50)
      integer next(nsearch)
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) 

      integer block,rwmode
c
      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,10,1,subinfo)
c
c --- OPEN FILE ---
c
          overwrite =.false.
      If (del_exist) THEN
        call ftrtnm( infile, filename, errflg )
        if (filename.eq.outfile) THEN
          overwrite = .true.
        else
          overwrite = .false.
        endif
      endif

      block = 2880
      IF (overwrite) THEN
        rwmode = 1
      ELSE
        rwmode = 0
      ENDIF

      call fcpars(infile,filename,extnum,errflg)
      call ftgiou(lun,errflg)
      errinfo = ' getting free lun'
      call wtferr(subname,version,errflg,errinfo)
      IF (errflg.NE.0) THEN
        return
      ENDIF 
      call ftopen(lun,filename,rwmode,block,errflg)
      errinfo = ' opening input file'
      call wtferr(subname,version,errflg,errinfo)
      IF (errflg.NE.0) THEN
        return
      ENDIF

c LOCATE EVENTS EXTENSION ...

      IF (extnum.LE.0) THEN
        call fndext(chatter,lun,'EVENTS',nsearch,nfound,
     &            next,outhdu,outver,extnames,errflg)
        IF (nfound.GT.0) THEN
          form = 'US'
        ELSE
          call fndext(chatter,lun,'STDEVT',nsearch,nfound,
     &            next,outhdu,outver,extnames,errflg)
          IF (nfound.GT.0) THEN
            form = 'RDF'
          ENDIF
        ENDIF
        IF (nfound.GT.1) THEN
          errinfo = ' Input file contains >1 EVENTS datasets'
          call fcecho(errinfo)
          write(errinfo,'(i12,a)')nfound,' extensions found'
          call wterrm(subname,version,errinfo)
          do i=1,nfound
            write(errinfo,'(a,i2,a)')' Ext ',next(i),':'
            call wtinfo(chatter,0,1,errinfo)
            write(errinfo,'(4X,a,a)')'extname = ',extnames(i)
            call wtinfo(chatter,0,1,errinfo)
          enddo
          errinfo =
     & '... Extension number must be specified via infile parameter'
          call fcecho(errinfo)
          errinfo = ' for example infile[1]'
          call wtinfo(chatter,0,1,errinfo)
          errflg = 2
          return
        ELSEIF (nfound.EQ.0) THEN
            errinfo = ' Unable to locate an EVENTS' 
     &//'extension'
            call wterrm(subname,version,errinfo)
          errflg = 3
          return
        ENDIF
      ENDIF

c --- IF NOT OVERWRITING INFILE THEN MAKE A COPY OF IT ---

      IF (.NOT.overwrite) THEN
        call ftclos(lun,errflg)
        errflg = 0
C PDW 6/30/98: Make sure there is no extension in filename
        call ftrtnm( infile, filename, errflg )
        call copy(filename,outfile,chatter,errflg)
        IF (errflg.NE.0) THEN
          errinfo = ' error in creating new file'
          call wterrm(subname,version,errinfo)
          return
        ENDIF
        rwmode = 1
        call ftopen(lun,outfile,rwmode,block,errflg)
        errinfo = ' opening input file'
        call wtferr(subname,version,errflg,errinfo)
        IF (errflg.NE.0) THEN
          return
        ENDIF
      ENDIF

c --- MOVE TO APPRORIATE PLACE IN FILE ---

      IF (extnum.LE.0) THEN
        IF (next(1).GT.0) THEN
           imove = next(1)
           status = 0
           call ftmrhd(lun,imove,htype,status)
           errinfo = ' Problem moving to EVENTS '
     &//'xtens'
        IF (status.NE.0) THEN
          errflg = 4
          call wtferr(subname,version,status,errinfo)
        ENDIF
        extnum = next(1)
        ENDIF
      ELSE
        status = 0
        call ftmahd(lun,extnum+1,htype,status)
        errinfo = ' Problem moving to EVENTS '
     &//'extension'
        IF (status.NE.0) THEN
          errflg = 4
          call wtferr(subname,version,status,errinfo)
        ENDIF
      ENDIF

      return
      end
c -------------------------------------------------------------------
c     END OF PCE_MDAT
c -------------------------------------------------------------------


*+PCE_CWT
c     --------------------------------------------------------------
      subroutine pce_cwt(infile,gtifile,del_exist,lun,
     &   form,
     &   start,stop,ngti,alkfile,instrume,calflag,errflg,chatter)
c     --------------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c ------------------------------------------------------------------
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,form,gtifile
      character*(*) instrume,alkfile
      logical del_exist
      integer errflg,chatter,lun,ngti
      real*8 start(ngti),stop(ngti)
      integer calflag

c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1996 Feb) 1.0.0
c
c Banashree M Seifert (1997, Sept 5) 1.1.0:
c       . defining variable desc2 is modified from 80 to 120 char
c         otherwise it was overwriting some other variable and
c         causing problem
c
c Banashree M Seifert (1997, Sept 8) 1.2.0:
c       . nonlin1 subroutine in roslib is introduced so that if
c         calflag=1 --> calls nonlin
c                =other than 1, calls nonlin1
c
c toliver (1999 June 30) 1.2.2:
c      . Corrected error handler at end of routine to remove non-existant
c        LUN (lun_spa)
c

c ---------------------------------------------------------------
      character(5) version
      parameter (version = '1.2.2')
      character(7) subname
      parameter (subname = 'pce_cwt')
*-
c ------------------------------------------------------------------
c --- LOCAL VARIABLES ---
c
      character(120) desc,errinfo,desc2
      character(80) history(10)
      character(26) comm
      integer nchan,i,status
      integer dx_col,dy_col,time_col,pi_col,inull
      logical anyflg,foundcol
      real ft_ran2,pi_real
      real*8 time,enull
      integer dx,dy,pi,pi_cor,pha_col,pha
      integer j,ISEED,n_alk,max_miss,iscc_col,alk_col
      logical calc_pha
      parameter (max_miss = 30)
      integer curgti,n_miss,misses(max_miss),n_wt
      logical prev_miss,ingti,wt_miss,match
      integer n_mval,maxvals
      parameter (maxvals = 50)
      real mvals(maxvals),pi_real_cor,mean_pha
      integer maxdisp
      parameter (maxdisp = 20)
      character(80) miss_disp(maxdisp)

c VARIABLES FOR MVEXT

      integer nsearch,ninstr,alklun
      parameter (nsearch = 50)
      integer next(nsearch)
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9)
      character(8) extname,alkinst


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
c --- USER INFO ---
c
      desc = ' using '//subname//' '//version
      call wtinfo(chatter,10,1,desc)

      ISEED = 31456789

c
c --- READ NCHAN VALUE ---
c     
      status = 0
      call ftgkyj(lun,'NAXIS2',nchan,comm,status)
      errinfo = ' reading NAXIS2'
      call wtferr(subname,version,status,errinfo)
      IF (status.NE.0) THEN
        errflg = 4
        return
      ENDIF
      IF (chatter.GE.20) THEN
        write(desc,'(a,i6)')
     &       ' Number of channels = ',nchan
        call wtinfo(chatter,20,2,desc)
      ENDIF

c
c --- LOCATE DX,DY,TIME,PHA and PI columns ---
c
      IF (form.EQ.'US') THEN
        foundcol=.true.
        status = 0
        call ftgcno(lun,.false.,'DX',dx_col,status)
        IF (status.NE.0) THEN
          foundcol=.false.
        ENDIF
        IF (.NOT.foundcol) THEN
          errinfo='DX column not present in EVENTS ext'
          call wtferr(subname,version,status,errinfo)
          errflg = 2
          return
        ENDIF
        foundcol=.true.
        status = 0
        call ftgcno(lun,.false.,'DY',dy_col,status)
        IF (status.NE.0) THEN
          foundcol=.false.
        ENDIF
        IF (.NOT.foundcol) THEN
          errinfo='DY column not present in EVENTS ext'
          call wtferr(subname,version,status,errinfo)
          errflg = 2
          return
        ENDIF
      ELSE
        foundcol=.true.
        status = 0
        call ftgcno(lun,.false.,'DETX',dx_col,status)
        IF (status.NE.0) THEN
          foundcol=.false.
        ENDIF
        IF (.NOT.foundcol) THEN
          errinfo='DETX column not present in EVENTS ext'
          call wtferr(subname,version,status,errinfo)
          errflg = 2
          return
        ENDIF
        foundcol=.true.
        status = 0
        call ftgcno(lun,.false.,'DETY',dy_col,status)
        IF (status.NE.0) THEN
          foundcol=.false.
        ENDIF
        IF (.NOT.foundcol) THEN
          errinfo='DETY column not present in EVENTS ext'
          call wtferr(subname,version,status,errinfo)
          errflg = 2
          return
        ENDIF
      ENDIF

      status = 0
      call ftgcno(lun,.false.,'TIME',time_col,status)
      IF (status.NE.0) THEN
        foundcol=.false.
      ENDIF
      IF (.NOT.foundcol) THEN
        errinfo='TIME column not present in EVENTS ext'
        call wtferr(subname,version,status,errinfo)
        errflg = 2
        return
      ENDIF
      foundcol=.true.
      status = 0
      call ftgcno(lun,.false.,'PI',pi_col,status)
      IF (status.NE.0) THEN
        foundcol=.false.
      ENDIF
      IF (.NOT.foundcol) THEN
        errinfo='PI column not present in EVENTS ext'
        call wtferr(subname,version,status,errinfo)
        errflg = 2
        return
      ENDIF
      foundcol=.true.
      status = 0
      call ftgcno(lun,.false.,'PHA',pha_col,status)
      IF (status.NE.0) THEN
        foundcol=.false.
      ENDIF
      IF (.NOT.foundcol) THEN
        errinfo='PHA column not present in EVENTS ext'
        call wtferr(subname,version,status,errinfo)
        errflg = 2
        return
      ENDIF

c
c --- OPEN ALKFILE AND MOVE TO ALK HIST EXTENSION ---
c
      ninstr = 2
      instr(1) = 'RESPONSE'
      instr(2) = 'DET_GAIN'
      extname = 'ALK_HIST'
      call mvext(0,alkfile,alklun,ninstr,instr,
     &                 nsearch,next,outhdu,extnames,outver,
     &                 extname,errflg,chatter)
      IF (errflg.NE.0) THEN
        desc = ' problem moving to alk hist extension'
        call wterrm(subname,version,desc)
        return
      ENDIF

      status = 0
      call ftgkyj(alklun,'NAXIS2',n_alk,comm,status)
      desc = ' reading NAXIS2 from alkfile'
      call wtferr(subname,version,status,errinfo)
      IF (status.NE.0) THEN
        errflg = 2
        return
      ENDIF
      status = 0
      call ftgkys(alklun,'INSTRUME',alkinst,comm,status)
      call ftupch(alkinst)
      IF (alkinst.NE.instrume) THEN
        desc = ' instrument mismatch between ALK_HIST and'
     &//' EVENTS file'
        call wterrm(subname,version,desc)
        desc = ' Events file instrument :'//instrume
     &//' Alk file instrument :'//alkinst
        call wtinfo(chatter,0,1,desc)
        errflg = 3
        return
      ENDIF
      status = 0
      call ftgcno(alklun,.false.,'ISCC',iscc_col,status)
      desc = ' problem locating ISCC column number in ALK HIST'
      call wtferr(subname,version,status,desc)
      IF (status.NE.0) THEN
        return
      ENDIF
      call ftgcno(alklun,.false.,'ALK_BIN',alk_col,status)
      errinfo = ' finding ALK_BIN column in ALK HIST'
      call wtferr(subname,version,status,errinfo)
c
c --- READ DX,DY,TIME,PHA and PI row by row then correct PI ---
c
      curgti = 1
      ingti = .false.
      prev_miss = .false.
      wt_miss = .false.
      n_wt = 0
      n_miss = 0
      ingti = .false.
      calc_pha = .false.
      n_mval = 0
       
      do i=1,nchan
       status=0
       call ftgcvj(lun,dx_col,i,1,1,inull,dx,
     &             anyflg,status)
       write(errinfo,'(a,i6)') 'reading DX value row ',i
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       call ftgcvj(lun,dy_col,i,1,1,inull,dy,
     &             anyflg,status)
       write(errinfo,'(a,i6)') 'reading DY value row ',i
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       call ftgcvj(lun,pi_col,i,1,1,inull,pi,
     &             anyflg,status)
       write(errinfo,'(a,i6)') 'reading PI value row ',i
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       enull = 0
       call ftgcvd(lun,time_col,i,1,1,enull,time,
     &             anyflg,status)
       write(errinfo,'(a,i6)') 'reading TIME value row ',i
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       inull = 0
       call ftgcvj(lun,pha_col,i,1,1,inull,pha,
     &             anyflg,status)
       write(errinfo,'(a,i6)') 'reading PHA value row ',i
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF

c --- CHECK WHICH GTI THIS EVENT FALLS IN ---
       ingti = .false.
       IF ((start(curgti).LE.time).AND.(stop(curgti).GE.time)) THEN
         ingti = .true.
         IF (.NOT.calc_pha) THEN
           call calc_mpha(alklun,start(curgti),stop(curgti),
     &          mean_pha,n_alk,iscc_col,alk_col,errflg,chatter)
          IF (errflg.NE.0) THEN
             errinfo = 'Error calculating mean_pha value'
             call wterrm(subname,version,errinfo)
             goto 100
           ENDIF
           calc_pha = .true.
         ENDIF
       ELSE
         j = 0
         do WHILE ((.NOT.ingti).AND.(j.LT.ngti))
           j = j + 1
           IF ((start(j).LE.time).AND. (stop(j).GE.time)) THEN
             ingti = .true.
             call calc_mpha(alklun,start(curgti),stop(curgti),
     &       mean_pha,n_alk,iscc_col,alk_col,errflg,chatter)
             IF (errflg.NE.0) THEN
                errinfo = 'Error calculating mean_pha value'
                call wterrm(subname,version,desc)
                goto 100
             ENDIF
             calc_pha = .true.
           ENDIF
         enddo
         curgti = j
       ENDIF

c --- CALCULATE CORRECTED PI CHANNEL 
       IF (ingti) THEN
         j = 0
         match = .false.
         do WHILE((.NOT.match).AND.(j.LT.n_mval))
           j = j + 1
           IF (mvals(j).EQ.mean_pha) THEN
             match = .true.
           ENDIF
         enddo
         IF (.NOT.match) THEN
           n_mval = n_mval + 1
           mvals(n_mval) = mean_pha
         ENDIF
 
         pi_real = pi + ft_ran2(ISEED)

         if(calflag .eq. 1) then
            call nonlin(mean_pha,pi_real,pi_real_cor)
         else
            call nonlin1(time,pi_real,pi_real_cor)
         endif

         pi_cor = pi_real_cor

         IF (prev_miss) THEN
           write(desc,'(a,i6,a,i6,a)')' WARNING : Channels ',
     &     misses(1),' - ', misses(n_miss),' are not within a GTI '
           call rmvexsp(desc,desc2)
           IF (n_wt.LT.maxdisp) THEN
             n_wt = n_wt + 1
             miss_disp(n_wt) = desc2
           ENDIF
           n_miss = 0
           prev_miss = .false.
         ENDIF
      ELSE
         n_miss = n_miss + 1
         misses(n_miss) = i
         prev_miss = .true.
         pi_cor = pi
      ENDIF
         
c --- Write corrected PI channel ---

       call ftpclj(lun,pi_col,i,1,1,pi_cor,status)
       write(errinfo,'(a,i6)') 
     & 'writing corrected PI value row ',i
       call wtferr(subname,version,status,errinfo)
            IF (status.NE.0) THEN
                errflg= 3
                goto 100
            ENDIF

      enddo

 100  status = 0
      history(1) = ' infile :'//infile//
     & ' input events filename'
      history(2) = ' gtifile :'//gtifile//
     & ' input gtifile filename'
      history(3) = ' alkfile:'//alkfile//
     & ' input alk history filename'
      do j=1,3
       call ftphis(lun,history(j),status)
      enddo
      status = 0
      call ftpkyl(lun,'PCECORF',.true.,
     & ' PI channel has been gain corrected',status)
      status = 0
      call timestamp(lun)
      status = 0
      call ftmkys(lun,'INSTRUME',instrume,
     &           'instrument (detector) name',status)
      status = 0
      call ftclos(lun,status)
      errinfo = ' closing outfile'
      call wtferr(subname,version,status,errinfo)
      status = 0
      call ftfiou(lun,status)
      errinfo = ' free lun'
      call wtferr(subname,version,status,errinfo)
      status = 0
 150  IF (status.NE.0) THEN
        errinfo = ' failed to allocate DMA'
        call wterrm(subname,version,errinfo)
      ENDIF
      return
      end
c --------------------------------------------------------------------
c     END OF PCECOR
c --------------------------------------------------------------------
