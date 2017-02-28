
*+PCSASSCOR
c      -----------------
       subroutine pcsasr
c      -----------------
c --- DESCRIPTION -----------------------------------------------------
c PCSASSCOR corrects PI channels in a US Rev0 or RDF Events file. 
c
c UNDO -
c The field and golden disk corrections that have already been applied
c are undone.
c
c CORRECTIONS - 
c ADC ; non-linearity corrections applied
c Gain saturation and temporal gain corrections are applied
c Spatial corrections are done
c
c --- CALLED ROUTINES -------------------------------------------------
c
c PCPI_GP     : Gets parameters
c PCPI_MDAT   : Moves to desired extension, and determines format
c               US Rev0/RDF supported
c PCPI_CWT    : Read each PI value correct it and write (overwrite)
c               the EVENTS file with the corrected file 
c                
c 
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1995 Sept 7) 1.0.0; Original, based on personal version
c                                   of PCPICOR
c Rehana Yusaf (1995 Oct 18) 1.0.1; bug-fix in rd_korgain
c Rehana Yusaf (1995 Oct 23) 1.0.2; bug-fix, dx,dy should be int not real
c Rehana Yusaf (1995 Oct 31) 1.0.3; improve comments
c Rehana Yusaf (1996 Feb 28) 1.0.4; strip out calc_mpha - in roslib now
c Banashree M Seifert (1997, August 15) 1.1.0:
c      . call to FTMAHD was 
c         call ftmahd(lun,2,2,errflg) replaced by
c         call ftmahd(lun,2,hdutyp,errflg)
c        These are done in subroutines
c           RD__ADCBINS, RD__ENDEP, RD__SPAGAIN, RD__KORGAIN
c Peter D Wilson (1998 Jun 30) 1.1.1:
c      . Updated for new FCPARS behavior
c Ning  Gan (1998 Jun 30) 1.1.2:
c      . Updated for new date keyword format. 
c Peter D Wilson (1999 Aug) 1.1.3:
c      . Replace gtcal with gtcalf
c Ning Gan (1999 Aug) 1.1.4:
c      . Added checks for PROC_SYS Keyword. 
c ----------------------------------------------------------------------
      IMPLICIT NONE
      character(5) version
      parameter (version = '1.1.4')
      character(40) taskname
      COMMON/task/taskname
*-
c ---------------------------------------------------------------------
c --- VARIABLES ---
c
      character(120) infile,outfile,desc,endepfile,ymapfile
      character(120) adcfile,spafile,korfile,gtifile,alkfile
      character(30) context,termdesc
      character(8) instrume
      logical del_exist
      integer errflg,chatter,lun
      real mean_pha
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

      taskname='PCSASSCOR '//version
      context = 'fatal error'
      termdesc =' PCSASSCOR Ver '//version//' terminated !'

c --- GET PARAMETERS ---

      errflg = 0
      call pcpi_gp(infile,outfile,endepfile,ymapfile,gtifile,
     &             adcfile,spafile,korfile,mean_pha,alkfile,
     &             del_exist,instrume,errflg,chatter)
      IF (errflg.NE.0) THEN
        if(errflg.ne.1001) call fcecho(termdesc)
        return
      ENDIF

c --- USER INFO ---

      IF (chatter.GE.1) THEN
        desc = ' MAIN PCSASSCOR Ver '//version
        call fcecho(desc)
      ENDIF 

c --- READ GTI TIMES FROM GTIFILE ---

      call pcpi_gti(gtifile,gtilun,ngti,errflg,chatter)

      IF (errflg.NE.0) THEN
        call fcecho(termdesc)
        goto 50
      ENDIF

      IF (ngti.LT.50) THEN
        ngti = 50
      ENDIF

      p_start = 0
      status = 0
      call udmget(ngti, 7,p_start,status)
      IF (status.NE.0) THEN
         goto 50
      ENDIF
      p_stop = 0
      status = 0
      call udmget(ngti, 7,p_stop,status)
      IF (status.NE.0) THEN
         goto 50
      ENDIF
      call pcpi_rdgti(gtilun,MEMD(p_start),MEMD(p_stop),
     &                ngti,errflg,chatter)
      IF (errflg.NE.0) THEN
        call fcecho(termdesc)
        goto 50
      ENDIF

c --- DETERMINE FILE FORMAT, OPEN FILE AND MOVE TO DESIRED EXT ---

      call pcpi_mdat(infile,outfile,del_exist,form,lun,
     &                 errflg,chatter)
      IF (errflg.NE.0) THEN
        call fcecho(termdesc)
        goto 50
      ENDIF

c --- CORRECT PI CHANNEL AND WRITE CORRECTED VALUES TO OUTFILE ---

      call pcpi_cwt(infile,gtifile,del_exist,lun,form,
     & endepfile,ymapfile,
     & adcfile,spafile,korfile,mean_pha,MEMD(p_start),MEMD(p_stop),
     & ngti,alkfile,instrume,errflg,chatter)

      IF (chatter.GE.1) THEN
        IF (errflg.EQ.0) THEN
          desc = ' PCSASSCOR Ver '//version//' completed'
          call fcecho(desc)
        ENDIF
      ENDIF

  50  IF (status.NE.0) THEN
        desc = ' failed to allocate memory'
        call fcecho(desc)
      ENDIF
  
      status = 0 
      call udmfre(p_start,7,status)
      IF (status.NE.0) THEN
         desc = ' failed to free memory'
         call fcecho(desc)
      ENDIF 
      call udmfre(p_stop,7,status)
      IF (status.NE.0) THEN 
         desc = ' failed to free memory'
         call fcecho(desc)
      ENDIF

      IF (errflg.NE.0) THEN
        desc = 'PCSASSCOR Ver '//version//' : incomplete execution'
        call fcecho(desc)
      ENDIF

      return
      end

c ---------------------------------------------------------------------
c     END OF MAIN PCSASSCOR 
c ---------------------------------------------------------------------
    
*+PCPI_GP
c     -------------------------------------------------------------
      subroutine pcpi_gp(infile,outfile,endepfile,ymapfile,gtifile,
     &                   adcfile,spafile,korfile,mean_pha,alkfile,
     &                   del_exist,instrume,errflg,chatter)
c     -------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile
      character*(*) alkfile,ymapfile,endepfile
      character*(*) adcfile,spafile,korfile,gtifile
      character(180) filename,gtifilename
      character(180) ill_files(5)
      character(30) errstr,wrnstr
      character*(*) instrume
      integer errflg,chatter,status,n_ill
      real mean_pha
      character(100) desc
      integer fcstln,flen,extnum
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
c Rehana Yusaf (1994 Sept)
c
c Banashree M Seifert (1996 Sept)1.1.0:
c      . not to check for ill_files (commented them)
c Peter D Wilson (1998 June) 1.1.1:
c      . Drop INQUIRE test. Replace fcpars with ftrtnm
c Peter D Wilson (1999 Aug) 1.1.3:
c      . Replace gtcal with gtcalf
c ----------------------------------------------------------------------
      character(5) version 
      parameter (version = '1.1.3')
*-
c ---------------------------------------------------------------------
c

c --- VARIABLES FOR MVEXT ---

      integer nsearch,ninstr,nfound,htype,nret
      parameter (nsearch = 50)
      integer maxret
      parameter (maxret = 1)
      integer next(nsearch),mvunit,extno(maxret)
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9),comm
      character(180) files(maxret),tfile
c      character(8) date,time,extname,online(maxret)
      character(8) extname,online(maxret)
      character(68) date,time
      character(68) proc_sys

      integer yy,mm,dd,inlen
      logical cal

      errstr = ' ERROR : PCPI_GP Ver '//version//':'
      wrnstr = ' WARNING : PCPI_GP Ver '//version//':'

c GET INFILE

      status = 0
      call uclgst('infile',infile,status)
      IF (status.NE.0) THEN
        desc = errstr//' ... getting infile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(infile)
      IF (infile.EQ.'  ') THEN
        errflg = 1
        desc = ' Input file not entered !'
        call fcecho(desc)
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
C        desc = errstr//' File does not EXIST :'//filename
C        call fcecho(desc)
C        return
C      ENDIF
      n_ill = 1
      ill_files(1) = filename 

c GTIFILE PARAMETER

      call uclgst('gtifile',gtifile,status)
      IF (status.NE.0) THEN
       desc = errstr//' .. getting gtifile parameter !'
       call fcecho(desc)
       errflg = 1
       return
      ENDIF
      call crmvblk(gtifile)
C PDW 6/30/98: Don't use INQUIRE. Replace fcpars with ftrtnm
      call ftrtnm( gtifile, gtifilename, status )
C      call fcpars(gtifile,gtifilename,extnum,status)
      IF ((gtifilename(1:1).EQ.'%').OR.(gtifile.EQ.' ')) THEN  
       gtifile = filename
      ELSE
C       INQUIRE(FILE=gtifilename,EXIST=ext)
C       IF (.NOT.ext) THEN
C         desc = errstr//' gtifile does not exist !'
C         call fcecho(desc)
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
        desc = errstr//' .. getting del_exist parameter !'
        call fcecho(desc)
        errflg = 1
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
        desc = errstr//' moving to extension 0'
        call wt_ferrmsg(status,desc)
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
          desc = errstr//'moving to EVENTS extension in infile'
          call fcecho(desc)
          errflg = 2
          return
        ENDIF

      ENDIF 

      status = 0
      call ftgkys(mvunit,'INSTRUME',instrume,comm,status)
      status = 0
      call ftgkys(mvunit,'DATE-OBS',date,comm,status)
      desc = errstr//' reading DATE-OBS keyword'
      call wt_ferrmsg(status,desc)
      call ftgkys(mvunit,'TIME-OBS',time,comm,status)
      desc = errstr//' reading TIME-OBS keyword'
      call wt_ferrmsg(status,desc)
      status = 0
      pcpi = .false.
      call ftgkyl(mvunit,'PCSASSCORF',pcpi,comm,status)
      IF (pcpi) THEN
        call fcecho('  ')
        desc = 
     &  ' *** WARNING *** PCSASSCOR has already been run on'
     &//' this dataset.'
        call fcecho(desc)
        desc = ' Rerun may produce inaccurate results'
        call fcecho(desc)
        call fcecho('  ')
      ENDIF 
      status = 0
      call ftmahd(mvunit,1,htype,status)
      call ftgkys(mvunit,'PROC_SYS',proc_sys,comm,status)
      if(status.ne.0) THEN
          desc = errstr//'Error reading PROC_SYS keyword.'
          call fcecho(desc)
          errflg = 2
          return
      endif

      IF ((proc_sys(5:5).eq.'0').OR.(proc_sys(5:5).eq.'1').OR.
     &    (proc_sys(5:5).eq.'2').OR.(proc_sys(5:5).eq.'3').OR.
     &    (proc_sys(5:5).eq.'4').OR.(proc_sys(5:5).eq.'5').OR.
     &    (proc_sys(5:5).eq.'6').OR.
     &    ((proc_sys(5:5).eq.'7').AND.(proc_sys(7:7).eq.'0')).OR.
     &    ((proc_sys(5:5).eq.'7').AND.(proc_sys(7:7).eq.'1')).OR.
     &    ((proc_sys(5:5).eq.'7').AND.(proc_sys(7:7).eq.'2')).OR.
     &    ((proc_sys(5:5).eq.'7').AND.(proc_sys(7:7).eq.'3')).OR.
     &    ((proc_sys(5:5).eq.'7').AND.(proc_sys(7:7).eq.'4')).OR.
     &    ((proc_sys(5:5).eq.'7').AND.(proc_sys(7:7).eq.'5')).OR.
     &    ((proc_sys(5:5).eq.'7').AND.(proc_sys(7:7).eq.'6')).OR.
     &    ((proc_sys(5:5).eq.'7').AND.(proc_sys(7:7).eq.'7'))) then 
      else 
          desc = 'PCSASSCOR: Data processed with SASS version '//
     &	  proc_sys(1:7)// '. No correction necessary.'
          call fcecho(desc)
          errflg = 1001
          status = 0
          call ftclos(mvunit,status)
          return
      endif	  
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
	status = 0
	call fts2dt(date, yy,mm,dd,status)
        IF (status.NE.0) THEN
          desc = errstr//' parsing DATE-OBS value'
          call fcecho(desc)
          desc = ' cannot use CALDB option to determine calfiles'
          call fcecho(desc)
          desc = ' as instrument not known !'
          call fcecho(desc)
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
           call fcecho(desc)
         ENDIF
       ENDIF
      ENDIF

c GET ENDEPFILE 

      status = 0
      call uclgst('endepfile',endepfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting endepfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(endepfile)
      tfile = endepfile
      call ftupch(tfile)
      IF (tfile.EQ.'CALDB') THEN
       call gtcalf(chatter,'ROSAT',instrume,'-','-','WC_E',
     &             date,time,date,time,'-',1,files,extno,
     &             online,nret,nfound,status)
       IF (nfound.EQ.0) THEN
         errflg = 2
         return
       ENDIF
       endepfile = files(1)
       IF (chatter.GE.20) THEN
         desc = ' CALDB selection for endepfile :'
         call fcecho(desc)
         call fcecho(endepfile)
       ENDIF
      ELSE
       IF (endepfile.EQ.' ') THEN
        desc = errstr//' ..endepfile name required !'
        call fcecho(desc)
        errflg = 1
        return
       ENDIF
       flen = fcstln(endepfile)
       INQUIRE(FILE=endepfile(:flen),EXIST=ext)
       IF (.NOT.ext) THEN
        errflg = 1
        desc = errstr//' File does not EXIST :'//endepfile
        call fcecho(desc)
        return
       ENDIF
      ENDIF
      n_ill = n_ill + 1
      ill_files(n_ill) = endepfile

c GET YMAPFILE

      status = 0
      call uclgst('ymapfile',ymapfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting ymapfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(ymapfile)
      tfile = ymapfile
      call ftupch(tfile)
      IF (tfile.EQ.'CALDB') THEN
       call gtcalf(chatter,'ROSAT',instrume,'-','-','WC_POS_Y',
     &             date,time,date,time,'-',1,files,extno,
     &             online,nret,nfound,status)
       IF (nfound.EQ.0) THEN
         errflg = 2
         return
       ENDIF
       ymapfile = files(1)
       IF (chatter.GE.20) THEN
         desc = ' CALDB selection for ymapfile:'
         call fcecho(desc)
         call fcecho(ymapfile)
       ENDIF
      ELSE
       IF (ymapfile.EQ.' ') THEN
        desc = errstr//' ..ymapfile name required !'
        call fcecho(desc)
        errflg = 1
        return
       ENDIF
       flen = fcstln(ymapfile)
       INQUIRE(FILE=ymapfile(:flen),EXIST=ext)
       IF (.NOT.ext) THEN
        errflg = 1
        desc = errstr//' File does not EXIST :'//ymapfile
        call fcecho(desc)
        return
       ENDIF
      ENDIF
      n_ill = n_ill + 1
      ill_files(n_ill) = ymapfile

c GET ADCFILE

      status = 0
      call uclgst('adcfile',adcfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting adcfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(adcfile)
      tfile = adcfile
      call ftupch(tfile)
      IF (tfile.EQ.'CALDB') THEN
       call gtcalf(chatter,'ROSAT',instrume,'-','-','ADC_NLC',
     &             date,time,date,time,'-',1,files,extno,
     &             online,nret,nfound,status)
       IF (nfound.EQ.0) THEN
         errflg = 2
         return
       ENDIF
       adcfile = files(1)
       IF (chatter.GE.20) THEN
         desc = ' CALDB selection for adcfile :'
         call fcecho(desc)
         call fcecho(adcfile)
       ENDIF
      ELSE
       IF (adcfile.EQ.' ') THEN
        desc = errstr//' ..adcfile required !'
        call fcecho(desc)
        errflg = 1
        return
       ENDIF
       flen = fcstln(adcfile)
       INQUIRE(FILE=adcfile(:flen),EXIST=ext)
       IF (.NOT.ext) THEN
        errflg = 1
        desc = errstr//' File does not EXIST :'//adcfile
        call fcecho(desc)
        return
       ENDIF
      ENDIF
ccc      n_ill = n_ill + 1
ccc      ill_files(n_ill) = adcfile

c GET GAIN_SPAT FILE

      status = 0
      call uclgst('spgfile',spafile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting spgfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(spafile)
      tfile = spafile
      call ftupch(tfile)
      IF (tfile.EQ.'CALDB') THEN
       call gtcalf(chatter,'ROSAT',instrume,'-','-','SGC_E',
     &             date,time,date,time,'-',1,files,extno,
     &             online,nret,nfound,status)
       IF (nfound.EQ.0) THEN
         errflg = 2
         return
       ENDIF
       spafile = files(1)
       IF (chatter.GE.20) THEN
         desc = ' CALDB selection for spafile:'
         call fcecho(desc)
         call fcecho(spafile)
       ENDIF
      ELSE
       IF (spafile.EQ.' ') THEN
        desc = errstr//' ..spgfile required !'
        call fcecho(desc)
        errflg = 1
        return
       ENDIF
       flen = fcstln(spafile)
       INQUIRE(FILE=spafile(:flen),EXIST=ext)
       IF (.NOT.ext) THEN
        errflg = 1
        desc = errstr//' File does not EXIST :'//spafile
        call fcecho(desc)
        return
       ENDIF
      ENDIF
      n_ill = n_ill + 1
      ill_files(n_ill) = spafile

c GET SPAT GAIN FILE

      status = 0
      call uclgst('spgkfile',korfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting korfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(korfile)
      tfile = korfile
      call ftupch(tfile)
      IF (tfile.EQ.'CALDB') THEN
       call gtcalf(chatter,'ROSAT',instrume,'-','-','SGC_POS',
     &             date,time,date,time,'-',1,files,extno,
     &             online,nret,nfound,status)
       IF (nfound.EQ.0) THEN
         errflg = 2
         return
       ENDIF
       korfile = files(1)
       IF (chatter.GE.20) THEN
         desc = ' CALDB selection for korfile:'
         call fcecho(desc)
         call fcecho(korfile)
       ENDIF
      ELSE
       IF (korfile.EQ.' ') THEN
        desc = errstr//' ..korfile required !'
        call fcecho(desc)
        errflg = 1
        return
       ENDIF
       flen = fcstln(korfile)
       INQUIRE(FILE=korfile(:flen),EXIST=ext)
       IF (.NOT.ext) THEN
        errflg = 1
        desc = errstr//' File does not EXIST :'//korfile
        call fcecho(desc)
        return
       ENDIF
      ENDIF
      n_ill = n_ill + 1
      ill_files(n_ill) = korfile

c GET MEAN_PHA

c      status = 0
c      call uclgsi('mean_pha',mean_pha,status)
c      IF (status.NE.0) THEN
c          desc = errstr//' .. getting mean_pha parameter !'
c          call fcecho(desc)
c          errflg = 1
c          return
c      ENDIF

c GET ALKFILE

      status = 0
      call uclgst('alkfile',alkfile,status)
      IF (status.NE.0) THEN
         desc = errstr//' .. getting alkfile parameter !'
         call fcecho(desc)
         errflg = 1
         return
      ENDIF
      call crmvlbk(alkfile)
      tfile = alkfile
      call ftupch(tfile)
      IF (tfile.EQ.'CALDB') THEN
       call gtcalf(chatter,'ROSAT',instrume,'-','-','GAIN_HIST',
     &             date,time,date,time,'-',1,files,extno,
     &             online,nret,nfound,status)
       IF (nfound.EQ.0) THEN
         errflg = 2
         return
       ENDIF
       alkfile = files(1)
       IF (chatter.GE.20) THEN
         desc = ' CALDB selection for alkfile:'
         call fcecho(desc)
         call fcecho(alkfile)
       ENDIF
      ELSE 
       IF (alkfile.EQ.' ') THEN
         desc = ' no alkfile entered !'
         call fcecho(desc)
         errflg = 2
       ENDIF
       flen = fcstln(alkfile)
       INQUIRE(FILE=alkfile(:flen),EXIST=ext)
       IF (.NOT.ext) THEN
        errflg = 1
        desc = errstr//' File does not EXIST :'//alkfile
        call fcecho(desc)
        return
       ENDIF
      ENDIF       

c GET OUTFILE 

      status = 0
      call uclgst('outfile',outfile,status)
      IF (status.NE.0) THEN
          desc = errstr//' .. getting outfile parameter !'
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
c     END OF PCPI_GP
c ---------------------------------------------------------------------



*+PCPI_GTI
c     -------------------------------------------------------     
      subroutine pcpi_gti(gtifile,gtilun,ngti,errflg,chatter)
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
*-
c ---------------------------------------------------------------
c
      character(30) errstr
      character(80) errinfo,desc,filename
      integer nsearch,nfound,extnum,block,htype
      parameter (nsearch = 50)
      integer next(nsearch),status
      character(20) outhdu(9,nsearch),comm
      character(20) outver(9,nsearch)
      character(8) extname

      errstr = ' ERROR: PCPI_GTI Ver '//version//':'

      IF (chatter.GE.15) THEN
        desc = ' using PCPI_GTI Ver '//version
        call fcecho(desc)
      ENDIF

c OPEN GTIFILE

      nfound = 0 
      call fcpars(gtifile,filename,extnum,errflg)
      call cgetlun(gtilun)
      call ftopen(gtilun,filename,0,block,errflg)
      errinfo = errstr//' opening GTIFILE'
      call wt_ferrmsg(errflg,errinfo)
      IF (errflg.NE.0) THEN
        return
      ENDIF

      IF (extnum.GT.0) THEN
        call ftmahd(gtilun,extnum+1,htype,errflg)
        IF (errflg.NE.0) THEN
          errinfo = errstr//' moving to GTI extension'
          call fcecho(errinfo)
          goto 100
        ENDIF 
      ELSE
        call fndext(chatter,gtilun,'GTI',nsearch,nfound,
     &            next,outhdu,outver,extname,errflg)
        IF (nfound.EQ.0) THEN
          call fndext(chatter,gtilun,'STDGTI',nsearch,nfound,
     &            next,outhdu,outver,extname,errflg)
         IF (nfound.EQ.0) THEN
          errinfo = errstr//' unsupported format for GTI file'
          call fcecho(errinfo)
          goto 100
         ENDIF
        ENDIF
        call ftmahd(gtilun,next(1)+1,htype,errflg)
        IF (errflg.NE.0) THEN 
          errinfo = errstr//' moving to GTI extension'
          call fcecho(errinfo)
          goto 100
        ENDIF 
      ENDIF 

c DETERMINE MAX ARRAY VALUES

      call ftgkyj(gtilun,'NAXIS2',ngti,comm,errflg)
      IF (errflg.NE.0) THEN
        errinfo=errstr//' reading NAXIS2 value from GTI file'
        call fcecho(errinfo)
      ENDIF

 100  IF (errflg.NE.0) THEN
        status = 0
        call ftclos(gtilun,status)
        errinfo = errstr//' closing gti file'
        call wt_ferrmsg(status,errinfo)
      ENDIF
      return
      end
c -------------------------------------------------------------------
c     END OF PCPI_GTI
c -------------------------------------------------------------------

*+PCPI_RDGTI
c     -------------------------------------------------------- 
      subroutine pcpi_rdgti(gtilun,start,stop,ngti,
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
*-
c ----------------------------------------------------------------
c
      character(70) desc,errinfo
      character(30) errstr
      integer status 

      errstr = ' ERROR: PCPI_RDGTI Ver '//version//':'

      IF (chatter.GE.15) THEN
        desc = ' using PCPI_RDGTI Ver '//version
        call fcecho(desc)
      ENDIF

      call rdgti1(gtilun,ngti,ngti,start,stop,chatter,errflg)
      status = 0
      call ftclos(gtilun,status)
      errinfo = errstr//' closing gtifile'
      call wt_ferrmsg(status,errinfo)
      return
      end
c ----------------------------------------------------------------
c     END OF PCPI_RDGTI
c ----------------------------------------------------------------
*+PCPI_MDAT
c     --------------------------------------------------------- 
      subroutine pcpi_mdat(infile,outfile,del_exist,form,lun,
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
c Peter Wilson (1998 Jun) 1.0.1: Add call to ftrtnm to strip off extension
c                                before calling copy
c
      character(5) version
      parameter (version = '1.0.1')
*-
c ------------------------------------------------------------------
c --- LOCALS ---
c
      character(30) errstr,wrnstr
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
      overwrite = .false.
c
      errstr = ' ERROR: PCPI_MDAT Ver '//version//':'
      wrnstr = ' WARNING: PCPI_MDAT Ver '//version//':'
      IF (chatter.GE.10) THEN
        subinfo = ' using PCPI_MDAT Ver '//version
        call fcecho(subinfo) 
      ENDIF
c
c --- OPEN FILE ---
c
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
      errinfo = errstr//' getting free lun'
      call wt_ferrmsg(errflg,errinfo)
      IF (errflg.NE.0) THEN
        return
      ENDIF 
      call ftopen(lun,filename,rwmode,block,errflg)
      errinfo = errstr//' opening INPUT FILE'
      call wt_ferrmsg(errflg,errinfo)
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
          errinfo = errstr//
     &            ' Input file contains >1 EVENTS datasets'
          call fcecho(errinfo)
          write(errinfo,'(a,i12,a)')'... ',nfound,' extensions found'
          call fcecho(errinfo)
          do i=1,nfound
            write(errinfo,'(a,i12,a)')'... Ext ',next(i),':'
            call fcecho(errinfo)
            write(errinfo,'(4X,a,a)')'EXTNAME = ',extnames(i)
            call fcecho(errinfo) 
          enddo
          errinfo =
     & '... Extension number must be specified via infile parameter'
          call fcecho(errinfo)
          errinfo = ' ... for example INFILE[1]'
          call fcecho(errinfo) 
          errflg = 2
          return
        ELSEIF (nfound.EQ.0) THEN
            errinfo = errstr//' Unable to locate an EVENTS' 
     &//'extension'
            call fcecho(errinfo)
          errflg = 3
          return
        ENDIF
      ENDIF

c --- IF NOT OVERWRITING INFILE THEN MAKE A COPY OF IT ---

      IF (.NOT.overwrite) THEN
        call ftclos(lun,errflg)
c        call ftfiou(lun,errflg)
c        errinfo = errstr//' free lun'
c        IF (chatter.GE.10) THEN
c          call wt_ferrmsg(errflg,errinfo)
c        ENDIF
        errflg = 0
C PDW 6/30/98: Call ftrtnm to remove extension number
        call ftrtnm( infile, filename, errflg )
        call copy(filename,outfile,chatter,errflg)
        IF (errflg.NE.0) THEN
          errinfo = errstr//' error in creating new file'
          call fcecho(errinfo)
          return
        ENDIF
        rwmode = 1
c        call ftgiou(lun,errflg)
c        errinfo = errstr//' getting free lun'
c        call wt_ferrmsg(errflg,errinfo)
c        IF (errflg.NE.0) THEN
c          return
c        ENDIF
        call ftopen(lun,outfile,rwmode,block,errflg)
        errinfo = errstr//' opening INPUT FILE'
        call wt_ferrmsg(errflg,errinfo)
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
           errinfo = wrnstr // ' Problem moving to EVENTS '
     &//'xtens'
        IF (status.NE.0) THEN
          errflg = 4
          call wt_ferrmsg(status,errinfo)
        ENDIF
        extnum = next(1)
        ENDIF
      ELSE
        status = 0
        call ftmahd(lun,extnum+1,htype,status)
        errinfo = errstr//' Problem moving to EVENTS '
     &//'extension'
        IF (status.NE.0) THEN
          errflg = 4
          call wt_ferrmsg(status,errinfo)
        ENDIF
      ENDIF

      return
      end
c -------------------------------------------------------------------
c     END OF PCPI_MDAT
c -------------------------------------------------------------------


*+PCPI_CWT
c     --------------------------------------------------------------
      subroutine pcpi_cwt(infile,gtifile,del_exist,lun,
     &   form,endepfile,
     &   ymapfile,adcfile,spafile,korfile,mean_pha,
     &   start,stop,ngti,alkfile,instrume,errflg,chatter)
c     --------------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c  The field and golden disk distortions are undone, then the PI 
c channel is  corrected.
c
c ------------------------------------------------------------------
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,form,endepfile,ymapfile,gtifile
      character*(*) adcfile,spafile
      character*(*) instrume,korfile,alkfile
      logical del_exist
      integer errflg,chatter,lun,ngti
      real mean_pha
      real*8 start(ngti),stop(ngti)
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c Rehana Yusaf (1995 Sep) 1.0.0
      character(5) version
      parameter (version = '1.0.0')
*-
c ------------------------------------------------------------------
c --- LOCAL VARIABLES ---
c
      integer maxdisp
      parameter (maxdisp = 20)
      character(100) desc,errinfo,miss_disp(maxdisp),desc2
      character(80) history(10)
      character(40) errstr,wrnstr,comm
      integer nchan,i,status,n_alk
      integer dx_col,dy_col,time_col,pi_col,inull
      logical anyflg,foundcol,calc_pha
      real*8 time,enull
      integer dx,dy,pi,pi_cor,lun_spa,pha_col,pha
      real  yprime,gy
      integer p_iymap,p_ga,p_chan,p_bin_low,p_bin_high,alk_col
      integer p_ha_chan,p_ha,p_k,p_lf,p_hf,max_miss,iscc_col
      parameter (max_miss = 30)
      integer curgti,n_miss,misses(max_miss),n_wt,j
      logical prev_miss,ingti,wt_miss,match

      integer n_mval,maxvals
      parameter (maxvals = 50)
      real mvals(maxvals)

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
      wrnstr = ' WARNING : PCPI_CWT Ver '//version//':'
      errstr = ' ERROR : PCPI_CWT Ver '//version//':'
      IF (chatter.GE.10) THEN
        desc = ' using PCPI_CWT Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- DYNAMIC MEMORY ALLOCATION ---
c
      p_iymap = 0
      p_ga = 0
      p_chan = 0
      p_bin_low = 0
      p_bin_high = 0
      p_ha_chan = 0
      p_ha = 0
      p_k = 0
      p_lf = 0
      p_hf = 0
      status = 0
      call udmget(512*512, 4, p_iymap,status)
      IF (status.NE.0) THEN
        goto 150
      ENDIF
      call udmget(256, 6, p_ga,status)
      IF (status.NE.0) THEN
        goto 150
      ENDIF
      call udmget(256, 4, p_chan,status)
      IF (status.NE.0) THEN
        goto 150
      ENDIF
      call udmget(256,6,p_bin_low,status)
      IF (status.NE.0) THEN
        goto 150
      ENDIF
      call udmget(256,6,p_bin_high,status) 
      IF (status.NE.0) THEN
        goto 150
      ENDIF
      call udmget(256,4,p_ha_chan,status) 
      IF (status.NE.0) THEN
        goto 150
      ENDIF
      call udmget(256,6,p_ha,status) 
      IF (status.NE.0) THEN
        goto 150
      ENDIF
      call udmget(800,6,p_k,status) 
      IF (status.NE.0) THEN
        goto 150
      ENDIF
      call udmget(800,6,p_lf,status) 
      IF (status.NE.0) THEN
        goto 150
      ENDIF
      call udmget(800,6,p_hf,status) 
      IF (status.NE.0) THEN
        goto 150
      ENDIF
c
c --- READ NCHAN VALUE ---
c     
      status = 0
      call ftgkyj(lun,'NAXIS2',nchan,comm,status)
      errinfo = errstr//' reading NAXIS2'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        errflg = 4
        return
      ENDIF
      IF (chatter.GE.20) THEN
        write(desc,'(A,i12)')
     &       '  ... Number of channels = ',nchan
        call fcecho(desc)
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
          errinfo=errstr//'DX column not present in EVENTS ext'
          call fcecho(errinfo)
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
          errinfo=errstr//'DY column not present in EVENTS ext'
          call fcecho(errinfo)
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
          errinfo=errstr//'DETX column not present in EVENTS ext'
          call fcecho(errinfo)
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
          errinfo=errstr//'DETY column not present in EVENTS ext'
          call fcecho(errinfo)
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
        errinfo=errstr//'TIME column not present in EVENTS ext'
        call fcecho(errinfo)
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
        errinfo=errstr//'PI column not present in EVENTS ext'
        call fcecho(errinfo)
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
        errinfo=errstr//'PHA column not present in EVENTS ext'
        call fcecho(errinfo)
        errflg = 2
        return
      ENDIF
c
c --- IF US Rev 0 data then read energy dependent correction file ---
c     and read golden disk correction 
c 
      call rd_endep(endepfile,MEMI(p_chan),MEMR(p_ga),
     &              errflg,chatter)
      IF (errflg.NE.0) THEN
        return
      ENDIF
      call rd_gdisk(ymapfile,MEMI(p_iymap),
     &              instrume,errflg,chatter)
      IF (errflg.NE.0) THEN
        return
      ENDIF

      call rd_adcbins(adcfile,MEMR(p_bin_low),MEMR(p_bin_high),
     &                errflg,chatter)
      IF (errflg.NE.0) THEN
        return
      ENDIF
      call rd_spagain(spafile,MEMI(p_ha_chan),MEMR(p_ha),
     &                errflg,chatter)
      IF (errflg.NE.0) THEN
        return
      ENDIF
      call rd_korgain(korfile,MEMR(p_k),MEMR(p_lf),MEMR(p_hf),
     &                instrume,errflg,chatter)
      IF (errflg.NE.0) THEN
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
        call fcecho(desc)
        return
      ENDIF

      status = 0
      call ftgkyj(alklun,'NAXIS2',n_alk,comm,status)
      desc = errstr//' reading NAXIS2 from alkfile'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        errflg = 2
        return
      ENDIF
      status = 0
      call ftgkys(alklun,'INSTRUME',alkinst,comm,status)
      call ftupch(alkinst)
      IF (alkinst.NE.instrume) THEN
        desc = errstr//' instrument mismatch between ALK_HIST'
        call fcecho(desc)
        desc = ' and EVENTS file'
        call fcecho(desc)
        desc = ' Events file instrument :'//instrume
        call fcecho(desc)
        desc = ' Alk file instrument :'//alkinst
        errflg = 3
        return
      ENDIF
      status = 0
      call ftgcno(alklun,.false.,'ISCC',iscc_col,status)
      desc = ' problem locating ISCC column number in ALK HIST'
      call wt_ferrmsg(status,desc)
      IF (status.NE.0) THEN
        return
      ENDIF
      call ftgcno(alklun,.false.,'ALK_BIN',alk_col,status)
      errinfo = errstr//' finding ALK_BIN column in ALK HIST'
      call wt_ferrmsg(status,errinfo)

c
c --- READ DX,DY,TIME,PHA and PI row by row then correct PI ---
c
      curgti = 1
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
       write(errinfo,'(a,i8)') errstr//'reading DX value row ',i
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       call ftgcvj(lun,dy_col,i,1,1,inull,dy,
     &             anyflg,status)
       write(errinfo,'(a,i8)') errstr//'reading DY value row ',i
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       call ftgcvj(lun,pi_col,i,1,1,inull,pi,
     &             anyflg,status)
       write(errinfo,'(a,i8)') errstr//'reading PI value row ',i
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       enull = 0
       call ftgcvd(lun,time_col,i,1,1,enull,time,
     &             anyflg,status)
       write(errinfo,'(a,i8)') errstr//'reading TIME value row ',i
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
       status=0
       inull = 0
       call ftgcvj(lun,pha_col,i,1,1,inull,pha,
     &             anyflg,status)
       write(errinfo,'(a,i8)') errstr//'reading PHA value row ',i
       call wt_ferrmsg(status,errinfo)
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
             errinfo = errstr//' calculating mean_pha value'
             call fcecho(errinfo)
             goto 100
           ENDIF 
           calc_pha = .true.
         ENDIF
       ELSE
         j = 0
         do WHILE ((.NOT.ingti).AND.(j.LT.ngti))
           j = j + 1
           IF ((start(j).LE.time).AND.
     &       (stop(j).GE.time)) THEN
             ingti = .true.
             call calc_mpha(alklun,start(curgti),stop(curgti),
     &       mean_pha,n_alk,iscc_col,alk_col,errflg,chatter)
             IF (errflg.NE.0) THEN
                errinfo = errstr//' calculating mean_pha value'
                goto 100
             ENDIF
             calc_pha = .true.
           ENDIF
         enddo
         curgti = j
       ENDIF 

c --- IF US Rev 0 data then undo field distortion and golden disk correction --

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
         call pcpi_undous(dx,dy,pi,pi_cor,pha,yprime,gy,MEMR(p_ga),
     &                    MEMI(p_iymap),MEMI(p_chan),
     &                    errflg,chatter)
         IF (errflg.NE.0) THEN
           return
         ENDIF

c --- APPLY NEW CORRECTION ---

         call pcpi_cor(dx,dy,pi_cor,pha,yprime,gy,MEMR(p_bin_low),
     &               MEMR(p_bin_high),MEMI(p_ha_chan),MEMR(p_ha),
     &               MEMR(p_k),MEMR(p_lf),MEMR(p_hf),mean_pha,
     &               errflg,chatter)
         IF (errflg.NE.0) THEN
           return
         ENDIF
         IF (prev_miss) THEN
           write(desc,'(a,i3,a,i3,a)')' WARNING : Channels ',
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
       write(errinfo,'(a,i8)')
     &       errstr//'writing corrected PI value row ',i
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         errflg= 3
         goto 100
       ENDIF
      enddo

      IF (chatter.GE.15) THEN
        desc = ' Calculated mean pha values used :'
        call fcecho(desc)
        status = 0
        do j=1,n_mval
          write(desc,18,IOSTAT=status) mvals(j)
          call fcecho(desc)
        enddo
      ENDIF
  18  format(' mean_pha :',g12.4)
      IF (wt_miss) THEN
         do j=1,n_wt
           call fcecho(miss_disp(j))
         enddo
      ENDIF

 100  status = 0
      history(1) = ' infile :'//infile//
     & ' input events filename'
      history(2) = ' gtifile :'//gtifile//
     & ' input gtifile filename'
      history(3) = ' endepfile :'//endepfile//
     & ' energy dependence calfile'
      history(4) = ' ymapfile:'//ymapfile//
     & ' Golden disk calfile'
      history(5) = ' spgfile:'//spafile//
     & ' pos-dep terms for spatial gain correction calfile'
      history(6) = ' spgkfile:'//korfile//
     & ' e-dep terms for spatial gain correction'
      history(7) = ' alkfile:'//alkfile//
     & ' ALK Gain history calibration file'
      do j=1,7
       call ftphis(lun,history(j),status)
      enddo
      status = 0
      call ftpkyl(lun,'PCSASSCORF',.true.,
     & ' PI channel has been corrected',status)
      status = 0
      call timestamp(lun)
      status = 0
      call ftmkys(lun,'INSTRUME',instrume,
     &           'instrument (detector) name',status)
      status = 0
      call ftclos(lun,status)
      errinfo = errstr//' closing outfile'
      call wt_ferrmsg(status,errinfo)
      status = 0
      call ftfiou(lun,status)
      errinfo = errstr//' free lun'
      call wt_ferrmsg(status,errinfo)
      status = 0
C      call ftfiou(lun_spa,status)
C      call wt_ferrmsg(status,errinfo)
C      status = 0
      call ftclos(alklun,status)
 150  IF (status.NE.0) THEN
        errinfo = errstr//' failed to allocate DMA'
        call fcecho(errinfo)
      ENDIF
      return
      end
c --------------------------------------------------------------------
c     END OF PCSASSCOR
c --------------------------------------------------------------------



*+RD__ADCBINS
c     ---------------------------------------------------------
      subroutine rd_adcbins(adcfile,bin_low,bin_high,
     &                      errflg,chatter)
c     ---------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c This routine reads the ADC file
c -----------------------------------------------------------------
c --- VARIABLES ---
c
      IMPLICIT NONE
      character*(*) adcfile
      integer errflg,chatter,ph_1(256)
      real bin_low(256),bin_high(256)
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1995 Sept 11) 1.0.0;
c
c Banashree M Seifert (1997, August 15) 1.1.0:
c      . call to FTMAHD was 
c         call ftmahd(lun,2,2,errflg) replaced by
c         call ftmahd(lun,2,hdutyp,errflg)
c        So, a variable hdutyp introduced
c ----------------------------------------------------------------------

      character(5) version
      parameter (version = '1.1.0')
c -----------------------------------------------------------------
*-
c --- LOCALS ---
c
      character(30) errstr
      character(80) errinfo,desc
      integer lun,len,clenact,block
      integer hdutyp
c
c --- USER INFO ---
c
      errstr = ' ERROR: RD__ADCBINS Ver '//version//':'
      len = clenact(errstr)
      IF (chatter.GE.10) THEN
        desc = ' ... using RD__ADCBINS Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- GET LOGICAL FREE UNIT NUMBER ---
c
      call ftgiou(lun,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr(1:len)//' getting lun'
        call fcecho(errinfo)
        return
      ENDIF
c
c --- READ file ---
c
      call ftopen(lun,adcfile,0,block,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr(1:len)//' opening adcfile'
        call fcecho(errinfo)
        goto 300
      ENDIF
      call ftmahd(lun,2,hdutyp,errflg)
      errinfo = errstr//' moving to ext 1 in adcfile'
      call wt_ferrmsg(errflg,errinfo)
      IF (errflg.NE.0) THEN
        goto 300
      ENDIF

      call rdadc1(lun,ph_1,bin_low,bin_high,256,
     &            256,errflg,chatter)
 300  call ftclos(lun,errflg)
      call ftfiou(lun,errflg)
      IF (chatter.GE.20) THEN
        desc = ' read adc bins file'
        call fcecho(desc)
      ENDIF
      return
      end
c     ---------------------------------------------------------
c     END OF RD__ADCBINS
c     ---------------------------------------------------------



*+RD_ENDEP
c     -----------------------------------------------------
      subroutine rd_endep(endepfile,chan,ga,errflg,
     &                    chatter)
c     -----------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c This routine reads an energy dependent correction file
c -----------------------------------------------------------------
c --- VARIABLES ---
c
      IMPLICIT NONE
      character*(*) endepfile
      integer chan(256),errflg,chatter,pi(256)
      real ga(256)
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1995 Sept 11) 1.0.0;
c
c Banashree M Seifert (1997, August 15) 1.1.0:
c      . call to FTMAHD was 
c         call ftmahd(lun,2,2,errflg) replaced by
c         call ftmahd(lun,2,hdutyp,errflg)
c        So, a variable hdutyp introduced
c ----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.0')
c -----------------------------------------------------------------
*-
c --- LOCALS ---
c
      character(30) errstr
      character(80) errinfo,desc
      integer lun,len,clenact,block
      integer hdutyp
c
c --- USER INFO ---
c
      errstr = ' ERROR: RD_ENDEP Ver '//version//':'
      len = clenact(errstr)
      IF (chatter.GE.10) THEN
        desc = ' ... using RD_ENDEP Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- GET LOGICAL FREE UNIT NUMBER ---
c
      call ftgiou(lun,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr(1:len)//' getting lun'
        call fcecho(errinfo)
        return
      ENDIF
c
c --- READ file ---
c
      call ftopen(lun,endepfile,0,block,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr(1:len)//' opening endepfile'
        call fcecho(errinfo)
        goto 300
      ENDIF

      call ftmahd(lun,2,hdutyp,errflg) 
      call rdscal1(lun,pi,ga,256,
     &            256,errflg,chatter)
 300  call ftclos(lun,errflg) 
      call ftfiou(lun,errflg)
      IF (chatter.GE.20) THEN
        desc = ' read energy dependent correction file'
        call fcecho(desc)
      ENDIF
      return
      end
c     ---------------------------------------------------------
c     END OF RD_ENDEP
c     ---------------------------------------------------------



*+RD__SPAGAIN
c     ---------------------------------------------------------
      subroutine rd_spagain(spgfile,ha_chan,ha,
     &                      errflg,chatter)
c     ---------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c This routine reads the SPAFILE
c -----------------------------------------------------------------
c --- VARIABLES ---
c
      IMPLICIT NONE
      character*(*) spgfile
      integer errflg,chatter
      integer ha_chan(256)
      real ha(256)
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1995 Sept 11) 1.0.0;
c
c Banashree M Seifert (1997, August 15) 1.1.0:
c      . call to FTMAHD was 
c         call ftmahd(lun,2,2,errflg) replaced by
c         call ftmahd(lun,2,hdutyp,errflg)
c        So, a variable hdutyp introduced
c ----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.0')
c -----------------------------------------------------------------
*-
c --- LOCALS ---
c
      character(30) errstr
      character(80) errinfo,desc
      integer lun,len,clenact,block
      integer hdutyp
c
c --- USER INFO ---
c
      errstr = ' ERROR: RD__SPAGAIN Ver '//version//':'
      len = clenact(errstr)
      IF (chatter.GE.10) THEN
        desc = ' ... using RD__SPAGAIN Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- GET LOGICAL FREE UNIT NUMBER ---
c
      call ftgiou(lun,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr(1:len)//' getting lun'
        call fcecho(errinfo)
        return
      ENDIF
c
c --- READ file ---
c
      call ftopen(lun,spgfile,0,block,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr(1:len)//' opening spgfile'
        call fcecho(errinfo)
        goto 300
      ENDIF
      call ftmahd(lun,2,hdutyp,errflg)
      call rdspa1(lun,ha_chan,ha,256,
     &            256,errflg,chatter)
 300  call ftclos(lun,errflg)
      call ftfiou(lun,errflg)
      IF (chatter.GE.20) THEN
        desc = ' read spagain file'
        call fcecho(desc)
      ENDIF
      return
      end
c     ---------------------------------------------------------
c     END OF RD__SPAGAIN
c     ---------------------------------------------------------



*+RD__KORGAIN
c     ---------------------------------------------------------
      subroutine rd_korgain(korfile,k,lf,hf,inst,
     &                      errflg,chatter)
c     ---------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c This routine reads gain correction file
c -----------------------------------------------------------------
c --- VARIABLES ---
c
      IMPLICIT NONE
      character*(*) korfile ,inst
      integer errflg,chatter
      real k(800),lf(800),hf(800)
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1995 Sept 11) 1.0.0;
c Rehana Yusaf (1995 Oct 18) 1.0.1; bug-fix, rdgkor1 should 800
c                                   passed as the dimensions not
c                                   256 !
c
c Banashree M Seifert (1997, August 15) 1.1.0:
c      . call to FTMAHD was 
c         call ftmahd(lun,2,2,errflg) replaced by
c         call ftmahd(lun,2,hdutyp,errflg)
c        So, a variable hdutyp introduced
c ----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.0')
c -----------------------------------------------------------------
*-
c --- LOCALS ---
c
      character(30) errstr
      character(8) instrume
      character(80) errinfo,desc
      integer lun,len,clenact,block
      integer hdutyp
c
c --- USER INFO ---
c
      errstr = ' ERROR: RD__KORGAIN Ver '//version//':'
      len = clenact(errstr)
      IF (chatter.GE.10) THEN
        desc = ' ... using RD__KORGAIN Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- GET LOGICAL FREE UNIT NUMBER ---
c
      call ftgiou(lun,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr(1:len)//' getting lun'
        call fcecho(errinfo)
        goto 300
      ENDIF
c
c --- READ file ---
c
      call ftopen(lun,korfile,0,block,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr(1:len)//' opening korfile'
        call fcecho(errinfo)
        goto 300
      ENDIF
      call ftmahd(lun,2,hdutyp,errflg)
      call rdgkor1(lun,k,lf,hf,800,800,
     &            instrume,errflg,chatter)
 300  call ftclos(lun,errflg)
      call ftfiou(lun,errflg)
      IF (instrume.NE.inst) THEN
       desc = ' instrument mismatch '
       call fcecho(desc)
       desc = ' Events file instrument :'//inst
       call fcecho(desc)
       desc = ' spatial gain file instrument :'//instrume
       call fcecho(desc)
       errflg = 2
      ENDIF
      IF (chatter.GE.20) THEN
        desc = ' read korfile '
        call fcecho(desc)
      ENDIF
      return
      end
c     ---------------------------------------------------------
c     END OF RD__KORGAIN
c     ---------------------------------------------------------

*+RD_GDISK
c     -----------------------------------------------------
      subroutine rd_gdisk(ymapfile,iymap,inst,
     &                    errflg,chatter)
c     -----------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c This routine reads the golden disk file
c -----------------------------------------------------------------
c --- VARIABLES ---
c
      IMPLICIT NONE
      character*(*) ymapfile,inst
      integer errflg,chatter,min,max
      integer iymap(512,512)
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1995 Sept 11) 1.0.0;
c
      character(5) version
      parameter (version = '1.0.0')
c -----------------------------------------------------------------
*-
c --- LOCALS ---
c
      character(30) errstr
      character(8) instrume
      character(80) errinfo,desc
      integer lun,len,clenact
c
c --- USER INFO ---
c
      errstr = ' ERROR: RD_GDISK Ver '//version//':'
      len = clenact(errstr)
      IF (chatter.GE.10) THEN
        desc = ' ... using RD_GDISK Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- GET LOGICAL FREE UNIT NUMBER ---
c
      call ftgiou(lun,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr(1:len)//' getting lun'
        call fcecho(errinfo)
      ENDIF
c
c --- READ file ---
c
      call rdtab1(ymapfile,instrume,iymap,512,min,max,
     &            errflg,chatter)
      call ftupch(instrume)
      IF (instrume.NE.inst) THEN
       desc = ' instrument mismatch'
       call fcecho(desc)
       desc = ' Events file instrument name :'//inst
       call fcecho(desc)
       desc = ' Golden disk file instrument name :'//instrume
       call fcecho(desc)
       errflg = 2
      ENDIF
      return
      end
c     ---------------------------------------------------------
c     END OF RD_GDISK
c     ---------------------------------------------------------


*+PCPI_UNDOUS
c     -----------------------------------------------------------
      subroutine pcpi_undous(dx,dy,pi,pi_cor,pha,yprime,
     &                       gy,ga,iymap,chan,errflg,chatter)
c     -----------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------------
c This subroutine is used to undo the field distortion, and the
c golden disk correction.
c -------------------------------------------------------------------
c --- VARIABLES ---
c 
      IMPLICIT NONE 
      integer dx,dy,pi,pi_cor,pha
      integer iymap(512,512),chan(*)
      real yprime,gy,ga(*)
      integer errflg,chatter
c
c --- CALLED ROUTINES -----------------------------------------------
c
c onax_decor : Subroutine to undo field distortion
c dewindow   : Subroutine to undo golden disk correction
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (Sept 12 1995) 1.0.0;
c
      character(5) version
      parameter (version = '1.0.0')     
c -------------------------------------------------------------------
*-
c LOCAL VARIABLES      
c
      character(30) errstr
      character(80) desc
      integer len,clenact
      real dx_inter,dy_inter
c
c --- USER INFO ---
c
      errstr = ' ERROR: PCPI_UNDOUS Ver '//version//':'
      len = clenact(errstr)
      IF (chatter.GE.40) THEN
        desc = ' ... using pcpi_undous Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- UNDO FIELD DISTORTION ---
c
      pi_cor = pi
      call onax_decorr(pha,pi_cor,dx,dy,dx_inter,dy_inter,
     &                 errflg,chatter)
      IF (errflg.NE.0) THEN
        desc = errstr(1:len)//' problem undoing field distortion'
        call fcecho(desc)
        return
      ENDIF
c
c --- UNDO GOLDEN DISK CORRECTION ---
c
      call dewindow(pha,pi_cor,dx,dy,dx_inter,dy_inter,
     &              yprime,ga,chan,iymap,gy,
     &                 errflg,chatter)
      return
      end
c -----------------------------------------------------------------
c     END OF PCPI_UNDOUS
c -----------------------------------------------------------------


*+PCPI_COR
c     -------------------------------------------------------
      subroutine pcpi_cor(dx,dy,pi_cor,pha,yprime,
     &                    gy,bin_low,bin_high,ha_chan,ha,
     &                    k,lf,hf,mean_pha,errflg,chatter)
c     -------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------------
c This subroutine is to correction the PI channel.
c -------------------------------------------------------------------
c --- VARIABLES ---
c 
      IMPLICIT NONE 
      integer dx,dy,pi_cor,pha
      real yprime,gy,bin_low(*),bin_high(*),ha(*),k(*)
      real lf(*),hf(*)
      integer errflg,chatter,ha_chan(*)
      real mean_pha
c
c --- CALLED ROUTINES -----------------------------------------------
c
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (Sept 19 1995) 1.0.0;
c
      character(5) version
      parameter (version = '1.0.0')     
c -------------------------------------------------------------------
*-
c LOCAL VARIABLES      
c
      character(30) errstr
      character(80) desc
      integer len,clenact
      real aprime1,aprime2,aprime3
c
c --- USER INFO ---
c
      errstr = ' ERROR: PCPI_COR Ver '//version//':'
      len = clenact(errstr)
      IF (chatter.GE.40) THEN
        desc = ' ... using pcpi_cor Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- APPLY ADC CORRECTION ---
c
      call adc(dx,dy,pha,pi_cor,yprime,gy,aprime1,bin_low,
     &         bin_high,errflg,chatter)
      IF (errflg.NE.0) THEN
        desc = errstr(1:len)
     &  //' problem applying adc correction routine'
        call fcecho(desc)
      ENDIF
c
c --- APPLY GAIN SATURATION CORRECTION ---
c
      call gain_sat(dx,dy,pha,pi_cor,yprime,aprime1,
     &         aprime2,aprime3,mean_pha,
     &         errflg,chatter)
c
c --- APPLY SPATIAL GAIN CORRECTION ---
c
      call spat_gain(dx,dy,pha,pi_cor,yprime,aprime1,
     &         aprime2,aprime3,ha_chan,ha,
     &         k,lf,hf,errflg,chatter)
      return
      end
c -----------------------------------------------------------------
c     END OF PCPI_COR
c -----------------------------------------------------------------
