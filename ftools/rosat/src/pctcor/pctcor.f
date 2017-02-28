c
*+PCTCOR
c      -----------------
       subroutine pctcor
c      -----------------
c --- DESCRIPTION -----------------------------------------------------
c PCTCOR corrects PI channels in a US Rev0 or RDF Events file. 
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
c Rehana Yusaf (1995 Nov 1) 1.0.0; based on pcsasscor (formerly pcpicor)
c Rehana Yusaf (1996 Feb 13); add pct_times and screen display routines
c
c Banashree M Seifert (1997 July) 1.1.0:
c    . n_alk modified to 13 instead of 8 (originally)
c    . everywhere hard coded 8 was removed to make it general n_alk
c Peter D Wilson (1998 June 30) 1.1.1:
c    . Updated for new FCPARS behavior
c Ning Gan (1998 June 30) 1.1.2:
c    . Updated for new date keyword format.
c toliver (1999 June 30) 1.1.3:
c    . Corrected LUN allocation in pct_times, corrected error handler
c      in pct_cwt
c toliver (1999 July 12) 1.1.4:
c    . Updated PCT_GP to get all caldb files
c

      IMPLICIT NONE
      character(5) version
      parameter (version = '1.1.4')
      character(40) taskname
c      COMMON/task/taskname
*-
c ---------------------------------------------------------------------
c --- VARIABLES ---
c
      integer max_alkmap
      parameter (max_alkmap = 20)
      character(120) infile,outfile
      character(120) gtifile, desc, alkfiles(max_alkmap)
      character(8) instrume
      logical del_exist
      integer errflg,chatter,lun,n_alk
      integer st_time(max_alkmap),end_time(max_alkmap)
      integer interpolate(max_alkmap)
      integer st_int(max_alkmap),end_int(max_alkmap)
      integer p_start,p_stop,gtilun,ngti,status
      character(3) form
    
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

      taskname='pctcor'

c --- GET PARAMETERS ---

      errflg = 0
      call pct_gp(infile,outfile,gtifile,alkfiles,
     &          max_alkmap,n_alk,st_time,end_time,
     &          interpolate,st_int,end_int,
     &          del_exist,instrume,errflg,chatter)

      call wtbegm(taskname,version,chatter)
      IF (errflg.NE.0) THEN
        goto 100
      ENDIF

c --- READ GTI TIMES FROM GTIFILE ---

      call pct_gti(gtifile,gtilun,ngti,errflg,chatter)

      IF (errflg.NE.0) THEN
        goto 100
      ENDIF

      IF (ngti.LT.50) THEN
        ngti = 50
      ENDIF

      status = 0
      p_start = 0
      call udmget(ngti, 7,p_start,status)
      IF (status.NE.0) THEN
         goto 50
      ENDIF
      status = 0
      p_stop = 0
      call udmget(ngti, 7,p_stop,status)
      IF (status.NE.0) THEN
         goto 50
      ENDIF
      call pct_rdgti(gtilun,MEMD(p_start),MEMD(p_stop),
     &                ngti,errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 100
      ENDIF

c --- DETERMINE FILE FORMAT, OPEN FILE AND MOVE TO DESIRED EXT ---

      call pct_mdat(infile,outfile,del_exist,form,lun,
     &                 errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 100
      ENDIF

c --- CORRECT PI CHANNEL AND WRITE CORRECTED VALUES TO OUTFILE ---

      call pct_cwt(infile,gtifile,del_exist,lun,form,
     &          max_alkmap,n_alk,st_time,end_time,
     &          interpolate,st_int,end_int,
     & MEMD(p_start),MEMD(p_stop),
     & ngti,alkfiles,instrume,errflg,chatter)

  50  IF (status.NE.0) THEN
        desc = ' failed to allocate memory'
        call fcecho(desc)
        goto 100
      ENDIF
  
      status = 0 
      call udmfre(p_start,7,status)
      IF (status.NE.0) THEN
         desc = ' failed to free memory'
         call fcecho(desc)
         goto 100
      ENDIF 
      call udmfre(p_stop,7,status)
      IF (status.NE.0) THEN 
         desc = ' failed to free memory'
         call fcecho(desc)
      ENDIF

 100  call wtendm(taskname,version,errflg,chatter)
      return
      end

c ---------------------------------------------------------------------
c     END OF MAIN PCTCOR 
c ---------------------------------------------------------------------
    
*+PCT_GP
c     -------------------------------------------------------------
      subroutine pct_gp(infile,outfile,gtifile,alkfiles,
     &          max_alkmap,n_alk,st_time,end_time,
     &          interpolate,st_int,end_int,
     &          del_exist,instrume,errflg,chatter)
c     -------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile
      integer max_alkmap
      character*(*) alkfiles(max_alkmap)
      character*(*) gtifile
      character(120) filename,gtifilename
      character(120) ill_files(20)
      character*(*) instrume
      integer errflg,chatter,status,n_ill
      integer n_alk,st_time(max_alkmap),end_time(max_alkmap)
      integer interpolate(max_alkmap)
      integer st_int(max_alkmap),end_int(max_alkmap),i
      character(70) desc
      integer fcstln,flen,extnum,j
      logical ext,valfil,del_exist,pcpi
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
c Rehana Yusaf (1995 Nov) 1.0.0:
c
c Banashree M Seifert (July, 1997) 1.1.0:
c    . n_alk modified to 13 instead of 8 (originally)
c    . everywhere hard coded 8 was removed to make it general n_alk
c Peter D Wilson (1998 June 30) 1.1.1:
c    . Drop INQUIRE test.  Replace fcpars with ftrtnm
c Ning Gan (1998 June 30) 1.1.2:
c    . Updated for new date keyword format.
c toliver (1999 July 12) 1.1.3:
c    . Increased n_alk to 15, perpetuating the insanity
c

      character(5) version 
      parameter (version = '1.1.3')
      character(6) subname
      parameter (subname = 'pct_gp')
*-
c ---------------------------------------------------------------------
c

c --- VARIABLES FOR MVEXT ---

      integer nsearch,ninstr,nfound,htype,nret
      parameter (nsearch = 50)
      integer maxret,st_sctime,end_sctime
      parameter (maxret = 4)
      integer next(nsearch),mvunit,extno(maxret)
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9),comm
      character(120) tfile,file
c      character(8) date,time,extname,online(maxret),date_end
      character(8) extname,online(maxret)
      character(68) date,time,date_end

      integer yy,mm,dd,inlen,lenact
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
C PDW 6/30/98: Don't use INQUIRE. Replace fcpars with ftrtnm
      call ftrtnm( infile, filename, status )
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
C PDW 6/30/98: Don't use INQUIRE. Replace fcpars with ftrtnm
C      call fcpars(gtifile,gtifilename,extnum,status)
      call ftrtnm( gtifile, gtifilename, status )
      IF ((gtifilename(1:1).EQ.'%').OR.(gtifile.EQ.' ')) THEN  
       gtifile = filename
      ELSE
C       INQUIRE(FILE=gtifilename,EXIST=ext)
C       IF (.NOT.ext) THEN
C         desc = ' gtifile does not exist !'
C         call wterrm(subname,version,desc)
C         errflg = 1
C         return
C       ENDIF
       n_ill = n_ill + 1
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
      pcpi = .false.
      call ftgkyl(mvunit,'PCTCORF',pcpi,comm,status)
      IF (pcpi) THEN
        call fcecho('  ')
        desc = 
     &  ' PCTCORF has already been run on'
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
C        read(date(1:2),50,IOSTAT=status) dd
C        read(date(4:5),50,IOSTAT=status) mm
C        read(date(7:8),50,IOSTAT=status) yy
C  50    format(i2)
	call fts2dt(date, yy,mm,dd,status)
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

c 

c GET ALKFILES 

C     hard-wiring this is brain-dead!
      n_alk = 15
      do i=1,n_alk
        status = 0
        if(i .lt. 10) then
           write(file,'(a,i1)',IOSTAT=status) 'alkfile',i
        else
           write(file,'(a,i2)',IOSTAT=status) 'alkfile',i
        endif

        status = 0
        call uclgst(file,alkfiles(i),status)
        IF (status.NE.0) THEN
          desc = 'getting '//file //' parameter !'
          call wterrm(subname,version,desc)
          errflg = 1
          return
        ENDIF
        call crmvlbk(alkfiles(i))
        tfile = alkfiles(i)
        call ftupch(tfile)
        IF (tfile.EQ.'CALDB') THEN

c start of IMG additions/changes
C  ----- ALL THIS IS ACTUALLY ALSO DONE IN GTCALF TOO ----
c ... first check that the CALDB is defined for the user
c	 call caldb_info(chatter,'INST','ROSAT', instrume, status)
c	 if(status.ne.0) then
c          call wterrm(subname,version,
c     &		'Set-up CALDB or enter calfiles by hand')
c          errflg = 2
c          return
c	 endif	
c ... now go and try and get the files
c	(all files ever valid for this instrume up til today)
c	 expr = 'ODDEVEN.eq."true"'

         call gtcalf(chatter,'ROSAT',instrume,'-','-',
     &      'ALK_PICH_MAP','-','-','now','now',
     &		'-',n_alk,alkfiles,extno,
     &        	online,nret,nfound,status)
         IF (nfound.EQ.0) THEN
           errflg = 2
           return
         ENDIF
	 call wtinfo(chatter,20,1,'CALDB selection for alkfiles:')
	 write(desc,'(a,i2)') 'Number of files found = ', nfound
	 call wtinfo(chatter,20,2,desc)
         do j=1,nfound
	     desc = alkfiles(j)
	     call wtinfo(chatter,20,3,desc) 
         enddo
         n_alk = nfound

c end of IMG additions/changes

         goto 70
        ELSE
          IF (alkfiles(i).EQ.' ') THEN
           desc = file(:lenact(file))//' name required !'
           call wterrm(subname,version,desc)
           errflg = 1
           return
          ENDIF
          flen = fcstln(alkfiles(i))
          INQUIRE(FILE=alkfiles(i)(:flen),EXIST=ext)
          IF (.NOT.ext) THEN
            errflg = 1
            desc = ' file does not exist :'//alkfiles(i)
            call wterrm(subname,version,desc)
            return
          ENDIF
        ENDIF
      enddo

c SET ALKFILE ARRAYS WITH THEIR S/C clock time ranges

  70  continue

      call pct_times(chatter,alkfiles,n_alk,st_time,end_time,
     &               interpolate,st_int,end_int,status)
      do i=1,n_alk
        ill_files(n_ill+i) = alkfiles(i)
      enddo
      n_ill = n_ill + n_alk

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

      return
      end
c ---------------------------------------------------------------------
c     END OF PCT_GP
c ---------------------------------------------------------------------

*+PCT_GTI
c     -------------------------------------------------------     
      subroutine pct_gti(gtifile,gtilun,ngti,errflg,chatter)
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
c                                  Oct 12, v 1.0.0; Rehana Yusaf
      character(5) version
      parameter (version = '1.0.0')
      character(7) subname
      parameter (subname = 'pct_gti')
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
c     END OF PCT_GTI
c -------------------------------------------------------------------

*+PCT_RDGTI
c     -------------------------------------------------------- 
      subroutine pct_rdgti(gtilun,start,stop,ngti,
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
      parameter (subname = 'pct_rdgti')
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
c     END OF PCT_RDGTI
c ----------------------------------------------------------------
*+PCT_MDAT
c     --------------------------------------------------------- 
      subroutine pct_mdat(infile,outfile,del_exist,form,lun,
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
c Peter Wilson (1998 Jun) 1.0.1: Call ftrtnm to strip off extension
c                                before calling copy
c
      character(5) version
      parameter (version = '1.0.1')
      character(8) subname
      parameter (subname = 'pct_mdat')
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
      overwrite = .false.
      If (del_exist) THEN
        if (infile.eq.outfile) THEN
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
            write(errinfo,'(a,i12,a)')' Ext ',next(i),':'
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
C PDW 6/30/98: Call ftrtnm to strip off extension
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
c     END OF PCPI_MDAT
c -------------------------------------------------------------------


*+PCT_CWT
c     --------------------------------------------------------------
      subroutine pct_cwt(infile,gtifile,del_exist,lun,
     &   form,max_alkmap,n_alk,st_time,end_time,
     &   interpolate,st_int,end_int,
     &   start,stop,ngti,alkfiles,instrume,errflg,chatter)
c     --------------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c  The field and golden disk distortions are undone, then the PI 
c channel is corrected.
c
c ------------------------------------------------------------------
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,form,gtifile
      integer max_alkmap,n_alk,alk
      integer interpolate(max_alkmap)
      character*(*) instrume,alkfiles(max_alkmap)
      integer st_int(max_alkmap),end_int(max_alkmap)
      integer st_time(max_alkmap),end_time(max_alkmap)
      logical del_exist
      integer errflg,chatter,lun,ngti
      real*8 start(ngti),stop(ngti)
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c Rehana Yusaf (1995 Sep) 1.0.0
c
c toliver (1999 June 30) 1.1.3:
c    . Corrected error handler at end of routine to remove non-existant
c      LUN (lun_spa)
c
      character(5) version
      parameter (version = '1.0.0')
      character(7) subname
      parameter (subname = 'pct_cwt')
*-
c ------------------------------------------------------------------
c --- LOCAL VARIABLES ---
c
      character(120) desc,errinfo
      character(80) history(10)
      character(26) comm
      integer nchan,i,status,p_map1,p_map2,maps(2)
      integer curmap1,curmap2,p_allmap
      integer dx_col,dy_col,time_col,pi_col,inull
      logical anyflg,foundcol
      real M_e, scale,ft_ran2,pi_real
      real*8 time,enull
      character(16) telescop,inst
      integer dx,dy,pi,pi_cor,pha_col,pha
      integer curgti,j,ISEED
      logical ingti,found_map

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
c --- DYNAMIC MEMORY ALLOCATION ---
c
      p_map1 = 0
      status = 0
      call udmget(61*61, 6, p_map1,status)
      IF (status.NE.0) THEN
        goto 150
      ENDIF

      p_map2 = 0
      status = 0
      call udmget(61*61, 6, p_map2,status)
      IF (status.NE.0) THEN
        goto 150
      ENDIF

      p_allmap = 0
      status = 0
      call udmget(61*61, 6, p_allmap,status)
      IF (status.NE.0) THEN
        goto 150
      ENDIF

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
        write(desc,'(a,i12)')
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
c --- READ DX,DY,TIME,PHA and PI row by row then correct PI ---
c
      curgti = 1
      ingti = .false.
      
      alk = 1
      curmap1 = 0
      curmap2 = 0

      call rdalkmap1(alkfiles(alk),61,inst,telescop,
     &      MEMR(p_allmap),chatter,status) 
       
      do i=1,nchan
       status=0
       call ftgcvj(lun,dx_col,i,1,1,inull,dx,
     &             anyflg,status)
       write(errinfo,'(a,i12)') 'reading DX value row ',i
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       call ftgcvj(lun,dy_col,i,1,1,inull,dy,
     &             anyflg,status)
       write(errinfo,'(a,i12)') 'reading DY value row ',i
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       call ftgcvj(lun,pi_col,i,1,1,inull,pi,
     &             anyflg,status)
       write(errinfo,'(a,i12)') 'reading PI value row ',i
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       enull = 0
       call ftgcvd(lun,time_col,i,1,1,enull,time,
     &             anyflg,status)
       write(errinfo,'(a,i12)') 'reading TIME value row ',i
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       inull = 0
       call ftgcvj(lun,pha_col,i,1,1,inull,pha,
     &             anyflg,status)
       write(errinfo,'(a,i12)') 'reading PHA value row ',i
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF

c --- DETERMINE WHICH ALKMAPS ARE REQUIRED ---

      found_map = .false.
      do WHILE((.NOT.found_map).AND.(alk.LE.n_alk))
        IF ((st_time(alk).LE.time)
     &  .AND.(time.LE.end_time(alk))) THEN
          IF (curmap1.NE.alk) THEN
            call rdalkmap1(alkfiles(alk),61,inst,telescop,
     &      MEMR(p_map1),chatter,status)
            IF (status.NE.0) THEN
              errinfo = ' reading alkmap:'//alkfiles(alk)
              call wterrm(subname,version,errinfo)
              errflg = 3
              goto 100
            ENDIF
            curmap1 = alk
          ENDIF 
          found_map = .true.
          maps(1) = alk
          maps(2) = 0
        ELSE
          IF (interpolate(alk).NE.0) THEN
           IF ((st_int(alk).LE.time)
     &         .AND.(time.LE.end_int(alk))) THEN
             IF (curmap1.NE.alk) THEN
               call rdalkmap1(alkfiles(alk),61,inst,telescop,
     &         MEMR(p_map1),chatter,status)
               IF (status.NE.0) THEN
              errinfo = ' reading alkmap:'//alkfiles(alk) 
              call wtferr(subname,version,status,errinfo)
                errflg = 3
                goto 100
               ENDIF
               curmap1 = alk
             ENDIF
             IF (curmap2.NE.interpolate(alk)) THEN
               call rdalkmap1(alkfiles(interpolate(alk)),61,
     &         inst,telescop,
     &         MEMR(p_map2),chatter,status)
               IF (status.NE.0) THEN
              errinfo = ' reading alkmap:'
     &        //alkfiles(interpolate(alk)) 
              call wtferr(subname,version,status,errinfo)
                errflg = 3
                goto 100
               ENDIF
               curmap2 = interpolate(alk)
             ENDIF
             found_map = .true.
             maps(1) = alk
             maps(2) = interpolate(alk)
           ENDIF
          ENDIF 
        ENDIF
        IF (.NOT.found_map) THEN
          alk = alk + 1
        ENDIF
        IF ((alk.GT.n_alk).AND.(.NOT.found_map)) THEN
           curmap1 = n_alk
           p_map1 = p_allmap
           maps(1) = n_alk
           maps(2) = 0 
        ENDIF   
       enddo

c --- DETERMINE APPRORIATE MEAN_CHANNEL ---

       call pct_cor(i,maps,MEMR(p_map1),MEMR(p_map2),MEMR(p_allmap),
     &     dx,dy,
     &     st_int(alk),end_int(alk),time,M_e,errflg,chatter)
       IF (errflg.NE.0) THEN
         errinfo = ' problem getting mean channel'
         call wtferr(subname,version,status,errinfo)
         errflg = 4
         goto 100
       ENDIF
       IF (M_e.GT.0) THEN
         scale = 151.20/M_e
       ELSE
         scale = 1
       ENDIF
       pi_real = pi + ft_ran2(ISEED)
       pi_real = scale * pi_real
       pi_cor = pi_real
         
c --- Write corrected PI channel ---

       call ftpclj(lun,pi_col,i,1,1,pi_cor,status)
       write(errinfo,'(a,i12)') 
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
      do j=1,2
       call ftphis(lun,history(j),status)
      enddo
      status = 0
      call ftpkyl(lun,'PCTCORF',.true.,
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
 150  IF (status.NE.0) THEN
        errinfo = ' failed to allocate DMA'
        call wterrm(subname,version,errinfo)
      ENDIF
      return
      end
c --------------------------------------------------------------------
c     END OF PCTCOR
c --------------------------------------------------------------------


*+PCT_COR
c     ---------------------------------------------------
      subroutine pct_cor(row,maps,map1,map2,allmap,dx,dy,
     &            start,end,time,M_e,errflg,chatter)
c     ---------------------------------------------------
c --- DESCRIPTION ----------------------------------------------
c This routine determines the mean channel (M_e) for a given 
c detector position. It uses Al K alpha calibration maps.
c NOTE : If the specified time is not in a map, then two maps
c are used, and the M_e is determined by interpolating.
c -------------------------------------------------------------- 
c --- VARIABLES ---
c
      IMPLICIT NONE
      integer dx,dy,maps(*),row
      integer start,end,errflg,chatter
      real map1(61,61),map2(61,61),allmap(61,61)
      real M_e
      real*8 time
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------
c
c Rehana Yusaf (Nov 14 1995) 1.0.0;
c Rehana Yusaf (Jan 17 1996) 1.0.1; add check to ensure that
c                                   dx and dy positions are within
c                                   the maps
      character(5) version
      parameter (version = '1.0.1')
      character(7) subname
      parameter (subname = 'pct_cor') 
c --------------------------------------------------------------
*-
c INTERNALS VARIABLES 

      character(180) desc,odesc
      integer alx,aly,status
      real M_e_map1,M_e_map2

c --- USER INFO ---

      desc = ' using '//subname//' '//version
      call wtinfo(chatter,50,2,desc)

c --- DETERMINE M_e ---

      alx = (dx - 4119)/128.451 + 31.5
      aly = (dy - 3929)/128.451 + 31.5

      IF ((alx.LT.1).OR.(alx.GT.61)) THEN
        write(desc,'(a,a,i12,a)')
     &' detector x coordinate outside of 61*61 Alk map range ',
     &   'for row ',row,' in the events file'
        call rmvexsp(desc,odesc)
        call wtwarm(subname,version,chatter,0,odesc)
        return
      ENDIF
      IF ((aly.LT.1).OR.(aly.GT.61)) THEN
        write(desc,'(a,a,i12,a)')
     &' detector y coordinate outside of 61*61 Alk map range ',
     &   'for row ',row,' in the events file'
        call rmvexsp(desc,odesc)
        call wtwarm(subname,version,chatter,0,odesc)
        return
      ENDIF

      IF (maps(2).EQ.0) THEN

c SINGLE FILE CASE 

        M_e = map1(alx,aly)

c If event lies outside covered region then allmap is used

        IF (M_e.EQ.(0.0)) THEN
          M_e = allmap(alx,aly)
        ENDIF
      ELSE

c INTERPOLATING BETWEEN TWO FILES

        M_e_map1 = map1(alx,aly)

c If event lies outside covered region then allmap is used

        IF (M_e_map1.EQ.(0.0)) THEN
          M_e_map1  = allmap(alx,aly)
        ENDIF

        M_e_map2 = map2(alx,aly)

        IF (M_e_map2.EQ.(0.0)) THEN
          M_e_map2 = allmap(alx,aly)
        else
        ENDIF
        M_e = M_e_map1 + ((M_e_map1 - M_e_map2) *
     &        ((time - start)/(end - start)))
      ENDIF

      IF (chatter.GE.50) THEN
        write(desc,'(a,f12.6)',IOSTAT=status)
     &  ' Calculated M_e value :',M_e
        call wtinfo(chatter,50,2,desc)
      ENDIF
      return
      end
c --------------------------------------------------------------- 
c     END OF PCT_COR
c ---------------------------------------------------------------

*+PCT_TIMES
c     ------------------------------------------------------- 
      subroutine pct_times(chatter,alkfiles,n_alk,st_time,
     &                     end_time,interpolate,st_int,
     &                     end_int,status)
c     ------------------------------------------------------- 
c ---  DESCRIPTION -----------------------------------------------
c
c This routines sorts the alk map files in time order. It also
c determines if any interpolation is required between files.
c ----------------------------------------------------------------
c --- VARIABLES --- 
c
      IMPLICIT NONE
      character*(*) alkfiles(*)
      integer n_alk,st_time(*),end_time(*),interpolate(*)
      integer st_int(*),end_int(*),status,chatter
c
c --- VARIABLE DIRECTORY -----------------------------------------
c
c chatter      int    : Flag to indicate level of screen display
c alkfiles     char   : Alk map filenames
c n_alk        int    : counter for number of files
c st_time      int    : Array for file start times
c end_time     int    : Array for file end times
c interpolate  int    : Array to indicate if current file will be
c                       interpolated with next file, if 0 then
c                       there is no interpolation, otherwise file
c                       number of interpolation file is stored.
c st_int       int    : Array for interpolation start times
c end_int      int    : Array for interpolation end times
c status       int    : error status flag
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c Rehana Yusaf ; Feb 8 - original
c
c toliver (1999 June 30) 1.0.1:
c    . Corrected LUN allocation (call FTGIOU instead of FTFIOU)
c
      character(9) subname
      parameter (subname = 'pct_times')
      character(6) version
      parameter (version = '1.0.1')
*-
c ----------------------------------------------------------------
c
c INTERNAL VARIABLES 
c
      character(120) desc,tmp_filename
      character(30) comm
      integer i,cur_st,cur_end,j,iunit,block
      logical ext
c
c --- USER INFO ---
c
      desc = ' using '//subname//' '//version
      call wtinfo(chatter,10,1,desc)
c
c --- SORT ALKFILES IN TIME ORDER ---
c
      status = 0
      call ftgiou(iunit,status)
      IF (status.NE.0) THEN
        call wterrm(subname,version,' getting frelun')
        return
      ENDIF
      
      do i=1,n_alk
        call ftopen(iunit,alkfiles(i),0,block,status)
        desc = ' opening '//alkfiles(i)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,desc)
          return
        ENDIF
        call ftgkyj(iunit,'MAPSTART',cur_st,comm,status)
        desc = ' reading MAPSTART keyword'
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,desc)
          return
        ENDIF
        call ftgkyj(iunit,'MAPSTOP',cur_end,comm,status)
        desc = ' reading MAPSTOP keyword'
        IF (status.NE.0) THEN 
          call wtferr(subname,version,status,desc)
          return
        ENDIF 
        call ftclos(iunit,status)
        desc = 'closing alkmap'
        call wtferr(subname,version,status,desc)
        st_time(i) = cur_st
        end_time(i) = cur_end
      enddo

      do i=1,n_alk
        do j=i,2,-1
          IF (end_time(j).LE.st_time(j-1)) THEN
            tmp_filename = alkfiles(j)
            cur_st = st_time(j)
            cur_end = end_time(j)
            alkfiles(j) = alkfiles(j-1)
            alkfiles(j-1) = tmp_filename
            st_time(j) = st_time(j-1)
            end_time(j) = end_time(j-1)
            st_time(j-1) = cur_st
            end_time(j-1) = cur_end
          ENDIF
        enddo
      enddo
c
c --- DETERMINE WHICH FILES ARE INTERPOLATED ---
c
      do i=1,n_alk-1
        IF ((end_time(i).GE.0).AND.
     &       (end_time(i).LT.st_time(i+1))) THEN
          interpolate(i) = i + 1
          st_int(i) = end_time(i)
          end_int(i) = st_time(i+1)
        ELSE
          interpolate(i) = 0
        ENDIF
        call crmvlbk(alkfiles(i))
        INQUIRE(FILE=alkfiles(i),EXIST=ext)
        IF (.NOT.ext) THEN
          desc = alkfiles(i)//' does not exist'
          call wterrm(subname,version,desc)
          return
        ENDIF
      enddo
      IF (chatter.GE.20) THEN
       call wtinfo(chatter,20,2,' alk maps in time order :')
       do i=1,n_alk
         call wtinfo(chatter,20,2,alkfiles(i))
         IF (st_time(i).LT.0) THEN
           desc = ' all map - valid for all times'
         ELSE
           write(desc,'(a,i12,a,i12)') ' start time :',st_time(i),
     &     ' end time :',end_time(i)
         ENDIF
         call wtinfo(chatter,20,2,desc)
         IF (interpolate(i).GT.0) THEN
           write(desc,'(a,i12,a,i12)') 'int start :',st_int(i),
     &     ' int end :',end_int(i)
           call wtinfo(chatter,20,2,desc)
         ENDIF
       enddo
      ENDIF
      
      return
      end
c -------------------------------------------------------------------
c     END OF PCT-TIMES
c ------------------------------------------------------------------- 
                
