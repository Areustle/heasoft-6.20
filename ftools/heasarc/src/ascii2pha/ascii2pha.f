*+ASCII2PHA
c      -----------------
       subroutine asciia
c      -----------------
c --- DESCRIPTION -----------------------------------------------------
c This task reads an ASCII data file and writes a FITS PHA file
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE
      character(80) infile, outfile
      character(80) context
      character(70) desc,termdesc
      integer phsize,errflg,chatter,nchan,fchan,tlmin,detchans
c      parameter (phsize = 4096)
c      integer channel(phsize)
c      real rate(phsize), serr(phsize),exposure,equinox
      real exposure,equinox
      logical qerror,killit
      logical pois,chanpres
      character(16) telescope,instrume,detnam,filter,hduclas2
      character(16) phaversn
      character(120) respfile,backfile,corrfile,ancrfile
      real areascal,backscal,corrscal
      character(68) date_obs,time_obs,date_end,time_end
      character(16) chantype
      character(16) ra_obj,dec_obj
      character(80) rows
      integer dtype
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c infile     char   : Input filename
c outfile    char   : Output filename
c chatter    int    : Chattiness flag (>20 verbose)
c task       char   : Task name : ASCI2FPHA
c phsize     int    : Array dimension, total rows in the input ascii file. 
c channel    int    : Array containing Channel numbers (dynamical array)
c rate       real   : Array containing count rate (dynamical array)
c serr       real   : statistical errors (dynamical array)
c qerror     logical: true if statistical errors
c --- CALLED ROUTINES -------------------------------------------------
c
c ASC_GP     : Gets parameters
c ASC_RDAT   : Reads data file
c ASC_WT     : Writes FITS PHA file, using general writer
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf July 6th 1994 1.0.0; original
c
c Banashree Mitra Seifert (1.1.0: July 1996)
c        . fixed bug in called routine asc_wt (was a mismatch)
c        . changed parameter file so that it asks for exposure time
c        . made equinox date =2000.0
C Ning Gan (1.1.1)6/19/1998 Modified the comments for date_obs, time_obs
C                           date_end and time_end for the four digit year.
C Ning Gan (1.1.2)7/1/1998 More updates for new DATE keyword format.
c Jeff Guerber (1.1.3, 1998-07-13) Write date & time keywords as DATE-OBS etc.
c        not DATE_OBS.  Fix 'i' formats missing widths.
C Ning Gan (1.1.4, 7/20/1998) Minor fixes for the comments of DATE-OBS and
C                             TIME-OBS keywords.
C Ning Gan (1.1.5, 7/23/1998) Added checks for the old two digit year date
c                             format when the date-obs and date-end
C			      keywords are written.
C Ning Gan (1.1.6, 8/09/2000) Added dynamical allocated arrays.
C 
c ---------------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.6')
      character(40) taskname,creator
      COMMON/task/taskname


c ---------------------------------------------------------------------
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB,MEMS,MEMI,MEML,MEMR,MEMD,MEMX)
      COMMON /MEM/ MEMD

C     datatype gives a symbolic code for the data type, e.g.,
C     4 is Integer*4
C     1 is boolean
C     3 is short integer
C     4 is integer
C     5 is long integer
C     6 is single precision real
C     7 is double precision real
C     8 is complex
C
C     pointers for dynamical arrays
C
      integer p_channel,p_rate,p_serr
      character(80) message
      integer status,sstatus

*-
c ---------------------------------------------------------------------
c
c --- GET PARAMETERS ---
c
      taskname ='ASCII2PHA '//version
      creator = taskname
      context = 'fatal error'
      termdesc =' ASCII2PHA Ver '//version//' terminated !'
      errflg = 0
      call asc_gp(infile,outfile,chanpres,dtype,rows,
     &        fchan,tlmin,detchans,pois,
     &        telescope,instrume,detnam,filter,phaversn,
     &        exposure,areascal,backscal,corrscal,corrfile,
     &        backfile,respfile,ancrfile,date_obs,time_obs,
     &        date_end,time_end,ra_obj,dec_obj,
     &        equinox,hduclas2,chantype,qerror,
     &        chatter,killit,errflg)
      IF (errflg.NE.0) THEN
        call fcerr(context)
        call fcecho(termdesc)
        return
      ENDIF


c
c --- USER INFO ---
c
      IF (chatter.GE.1) THEN
        desc = ' Main ASCII2PHA Ver '//version
        call fcecho(desc)
      ENDIF

c
c ---  GET THE MAX SIZE OF PHA FILE 
c
      call get_phsize(infile,rows,phsize,killit, chatter,errflg)
      IF (errflg.NE.0) THEN
        call fcerr(context)
        call fcecho(termdesc)
        return
      ENDIF

c
c ---  DYNAMICAL ALLOCATE MEMORY 
c
      status = 0
      p_channel = 0
      CALL udmget(phsize,4,p_channel,status)
      if( status. ne. 0) then
          message = 'Insufficient memory for the CHANNEL array'
          call wt_ferrmsg(status,message)
          goto 999
      endif

      p_rate = 0
      CALL udmget(phsize,6,p_rate,status)
      if( status. ne. 0) then
          message = 'Insufficient memory for the RATE array'
          call wt_ferrmsg(status,message)
          goto 999
      endif
     

      p_serr = 0
      CALL udmget(phsize,6,p_serr,status)
      if( status. ne. 0) then
          message = 'Insufficient memory for the SERR array'
          call wt_ferrmsg(status,message)
          goto 999
      endif
c
c --- READ ASCII DATAFILE ---
c
      call asc_rdat(infile,phsize,rows,chanpres,MEMI(p_channel),nchan,
     &             fchan,tlmin,detchans,
     &             MEMR(p_rate),dtype,MEMR(p_serr),qerror,pois,exposure,
     &             killit,errflg,chatter)
      IF (errflg.NE.0) THEN
        call fcerr(context)
        call fcecho(termdesc)
        return
      ENDIF
c
c --- WRITE OUTPUT FILE ---
c
      errflg=0
      call asc_wt(outfile,infile,nchan,MEMI(p_channel),MEMR(p_rate),
     &            tlmin,detchans,
     &            dtype,qerror,MEMR(p_serr),pois,telescope,instrume,
     &            detnam,filter,phaversn,exposure,areascal,
     &            backscal,corrscal,backfile,corrfile,
     &            respfile,ancrfile,date_obs,time_obs,
     &            date_end,time_end,ra_obj,dec_obj,equinox,
     &            hduclas2,chantype,
     &            creator,errflg,killit,chatter)

      IF (errflg.NE.0) THEN
        call fcerr(context)
        call fcecho(termdesc)
      ENDIF

999   if(p_channel.ne.0) call udmfre(p_channel,4,sstatus)
      sstatus = 0
      if(p_rate.ne.0)  call udmfre(p_rate,6,sstatus)
      sstatus = 0
      if(p_serr.ne.0)  call udmfre(p_serr,6,sstatus)
      IF (status.NE.0) THEN
        call fcerr(context)
        call fcecho(termdesc)
        return 
      ENDIF
 
      IF (chatter.GE.1) THEN
        desc = ' ASCI2FPHA Ver '//version//' completed'
        call fcecho(desc)
      ENDIF
      return
      end
c ----------------------------------------------------------------------
c     END OF MAIN ASCII2PHA
c ----------------------------------------------------------------------

*+ASC_GP
c     ----------------------------------------------------------
      subroutine asc_gp(infile,outfile,chanpres,dtype,rows,
     &        fchan,tlmin,detchans,pois,
     &        telescope,instrume,detnam,filter,phaversn,
     &        exposure,areascal,backscal,corrscal,corrfile,
     &        backfile,respfile,ancrfile,date_obs,time_obs,
     &        date_end,time_end,ra_obj,dec_obj,
     &        equinox,hduclas2,chantype,qerror,chatter,
     &        killit,errflg)
c     ----------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile
      character*(*) chantype,hduclas2
      integer errflg,chatter
      character(26) errstr, wrnstr
      integer status,len,fcstln,n_ill
      character(70) desc,ill_files(5)
      logical ext,killit,valfil
      character*(*) telescope,instrume,detnam,filter
      character*(*) date_obs,time_obs,date_end,time_end
      character*(*) ra_obj,dec_obj,rows
      integer dtype,fchan,tlmin,detchans
      logical chanpres,pois,qerror
      character*(*) phaversn
      real exposure,equinox
      real areascal,backscal,corrscal
      character*(*) backfile,corrfile,ancrfile,respfile
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
c Rehana Yusaf (1994 July)
      character(5) version
      parameter (version = '1.0.0')
*-
c ---------------------------------------------------------------------
c
      errstr = ' ERROR : ASCII2PHA Ver '//version//':'
      wrnstr = ' WARNING : ASCII2PHA Ver '//version//':'

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
        desc = ' Input PHA file not entered !'
        call fcecho(desc)
        return
      ENDIF
      ext = .true.
      INQUIRE(FILE=infile,EXIST=ext)
      IF (.NOT.ext) THEN
        errflg = 1
        desc = errstr//' File does not EXIST :'//infile
        call fcecho(desc)
        return
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

c GET CLOBBER

      status = 0
      call uclgsb('clobber',killit,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting clobber parameter !'
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
      ill_files(1) = infile

      call ck_file(outfile,ill_files,n_ill,valfil,
     &             killit,chatter)
      IF (.NOT.valfil) THEN
        desc = errstr//' invalid outfile !'
        errflg = 1
        call fcecho(desc)
        return
      ENDIF

c GET CHANPRES

      status = 0
      call uclgsb('chanpres',chanpres,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting chanpres parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF


c GET DTYPE

      status = 0
      call uclgsi('dtype',dtype,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting dtype parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET QERROR

      status = 0
      call uclgsb('qerror',qerror,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting qerror parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF


c GET ROWS

      status = 0
      call uclgst('rows',rows,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting rows parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET FCHAN

      IF (.NOT.chanpres) THEN
       status = 0
       call uclgsi('fchan',fchan,status)
       IF (status.NE.0) THEN
        desc = errstr//' .. getting fchan parameter !'
        call fcecho(desc)
        errflg = 1
        return
       ENDIF
      ENDIF

c GET TLMIN

      status = 0
      call uclgsi('tlmin',tlmin,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting tlmin parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET DETCHANS

      status = 0
      call uclgsi('detchans',detchans,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting detchans parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET POIS

      status = 0
      call uclgsb('pois',pois,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting pois parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF


c GET TELESCOPE

      status = 0
      call uclgst('telescope',telescope,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting telescope parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(telescope)
      IF (telescope.EQ.'  ') THEN
        telescope = 'UNKNOWN'
      ELSE
        len = fcstln(telescope)
        status = 0
        call ftupch(telescope)
        call trtele(telescope,len,status)
      ENDIF

c GET INSTRUME

      status = 0
      call uclgst('instrume',instrume,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting instrume parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(instrume)
      IF (instrume.EQ.' ') THEN
        instrume = 'UNKNOWN'
      ELSE
        len = fcstln(instrume)
        status = 0
        call ftupch(instrume)
        call trinst(instrume,len,status)
      ENDIF


c GET DETNAM

      status = 0
      call uclgst('detnam',detnam,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting detnam parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(detnam)

c GET FILTER

      status = 0
      call uclgst('filter',filter,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting filter parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(filter)
      IF (filter.EQ.'  ') THEN
        filter = 'NONE'
      ENDIF

c GET PHAVERSN

      status = 0
      call uclgst('phaversn',phaversn,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting phaversn parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET EXPOSURE

      status = 0
      call uclgsr('exposure',exposure,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting exposure parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET AREASCAL

      status = 0
      call uclgsr('areascal',areascal,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting areascal parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET BACKSCAL

      status = 0
      call uclgsr('backscal',backscal,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting backscal parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET CORRSCAL

      status = 0
      call uclgsr('corrscal',corrscal,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting corrscal parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET BACKFILE

      status = 0
      call uclgst('backfile',backfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting backfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(backfile)
      IF (backfile.EQ.' ') THEN
        backfile = 'NONE'
      ENDIF


c GET CORRFILE

      status = 0
      call uclgst('corrfile',corrfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting corrfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(corrfile)
      IF (corrfile.EQ.' ') THEN
        corrfile = 'NONE'
      ENDIF

c GET RESPFILE

      status = 0
      call uclgst('respfile',respfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting respfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(respfile)
      IF (respfile.EQ.' ') THEN
        respfile = 'NONE'
      ENDIF

c GET ANCRFILE

      status = 0
      call uclgst('ancrfile',ancrfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting ancrfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(ancrfile)
      IF (ancrfile.EQ.' ') THEN
        ancrfile = 'NONE'
      ENDIF


c GET DATE-OBS

      status = 0
      call uclgst('date_obs',date_obs,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting date_obs parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(date_obs)

c GET TIME-OBS

      status = 0
      call uclgst('time_obs',time_obs,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting time_obs parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(time_obs)

c GET DATE_END

      status = 0
      call uclgst('date_end',date_end,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting date_end parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(date_end)

c        write(*,*) 'after date_end:',infile

c      GOTO 100

c GET TIME_END

      status = 0
      call uclgst('time_end',time_end,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting time_end parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(time_end)

c GET RA_OBJ

      status = 0
      call uclgst('ra_obj',ra_obj,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting ra_obj parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(ra_obj)

c GET DEC_OBJ

      status = 0
      call uclgst('dec_obj',dec_obj,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting dec_obj parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(dec_obj)
c
c
c GET EQUINOX

      status = 0
      call uclgsr('equinox',equinox,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting equinox parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET HDUCLAS2

      status = 0
      call uclgst('hduclas2',hduclas2,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting hduclas2 parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(hduclas2)
      IF (hduclas2.EQ.' ') THEN
         hduclas2 = 'UNKNOWN'
      ENDIF

c GET CHANTYPE

      status = 0
      call uclgst('chantype',chantype,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting chantype parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(chantype)

c GET CHATTER

      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting chatter parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET CLOBBER

      status = 0
      call uclgsb('clobber',killit,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting clobber parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      return
      end
c ---------------------------------------------------------------------
c     END OF ASC_GP
c ---------------------------------------------------------------------



*+ASC_RDAT
c     ---------------------------------------------------------------
      subroutine asc_rdat(infile,phsize,rows,chanpres,channel,nchan,
     &                    fchan,tlmin,detchans,
     &                    rate,dtype,serr,qerror,pois,exposure,
     &                    killit,errflg,chatter)
c     ---------------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c This subroutine reads an ascii file containing channel and counts
c data
c ---------------------------------------------------------------------
c --- VARIABLES ---
c
      IMPLICIT NONE
      character*(*) infile
      integer channel(*),errflg,nchan
      integer phsize,chatter,dtype,fchan,tlmin,detchans
      real serr(*),rate(*),exposure
      character*(*) rows
      logical chanpres,qerror,endfile,pois,blank,killit
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (2 July 1994)
c
      character(5) version
      parameter (version ='1.0.0')
*-
c ---------------------------------------------------------------------
c
c --- LOCALS ---
c
      integer iunit,i,ierr,cnts,j,cnum,k,numblk,totblk
      character(26) errstr,wrnstr
      character(70) line
      character(120) desc,desc2
      real calcpois,AMOD

      integer nrows,numranges,rowrange1(15),rowrange2(15)

c
c --- USER INFO ---
c
      errstr = ' ERROR:ASC_RDAT Ver'//version//':'
      IF (chatter.GE.15) THEN
        desc = ' ... using ASC_RDAT Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- OPEN FILE AND READ DATA ---
c
      call cgetlun(iunit)
      call opasci(iunit,infile,1,70,killit,chatter,errflg)
      IF (errflg.NE.0) THEN
        desc=errstr//'opening infile'
        call fcecho(desc)
        return
      ENDIF
      endfile = .false.
      i = 0
      do WHILE(.NOT.endfile)
        ierr = 0
        i = i+ 1
        read(iunit,'( )',IOSTAT=ierr,end = 50)
      enddo
 50   continue
      nrows = i - 1
      IF (chatter.GE.20) THEN
       write(desc,'(a,i12)')
     &        ' Number of lines in ASCII file: ',nrows
       call rmvexsp(desc,desc2)
       call fcecho(desc2)
      ENDIF
      ierr = 0
      rewind(unit=iunit,IOSTAT=ierr)
      IF (ierr.NE.0) THEN
        IF (chatter.GE.30) THEN
           desc = wrnstr//' problem rewinding infile'
           call fcecho(desc)
        ENDIF
        close(unit=iunit)
        open(unit=iunit,file=infile,status='old')
      ENDIF

c DETERMINE WHICH ROWS USER WANTS

      call crmvlbk(rows)
      IF (rows.EQ.'-') THEN
        numranges = 1
        rowrange1(1) = 1
        rowrange2(1) = nrows
      ELSE
        call fcgrgs(rows,nrows,numranges,rowrange1,rowrange2)
      ENDIF

      IF (rowrange1(1).LE.0) THEN
        desc = errstr//' starting rowrange <= 0 not allowed'
        call fcecho(desc)
        errflg = 2
        close (unit=iunit)
        return
      ENDIF

      IF (chatter.GE.30) THEN
        do j=1,numranges
          write(desc,'(a,i12,a,i12,a,i12)') ' Range ', j, ' :',
     &          rowrange1(j), ' to ', rowrange2(j)
         call rmvexsp(desc,desc2)
         call fcecho(desc2)
        enddo
      ENDIF

      cnum = 0
      blank =.false.
      numblk = 0
      totblk = 0
      j = 1
      do k=1,rowrange2(numranges)
       IF ((k.GE.rowrange1(j)).AND.(k.LE.rowrange2(j))) THEN
        ierr = 0
        line = '  '
        read(iunit,200,IOSTAT=ierr,end=100) line
        call crmvlbk(line)
        IF (line(1:6).EQ.'      ') THEN
          blank=.true.
          numblk = numblk + 1
        ENDIF
        IF (blank) THEN
          IF (chatter.GE.5) THEN
            write(desc,'(a,i12,a)') wrnstr//'row ',k,' is blank'
            call fcecho(desc)
            desc = ' this row is ignored'
            call fcecho(desc)
          ENDIF
          blank = .false.
        ELSE
         backspace(iunit)
         cnum = cnum + 1
         IF (chanpres) THEN
           ierr = 0
           IF (qerror) THEN
            read(iunit,*,IOSTAT=ierr,end=100) channel(cnum),
     &      rate(cnum),serr(cnum)
           ELSE
            read(iunit,*,IOSTAT=ierr,end=100) channel(cnum),
     &      rate(cnum)
           ENDIF
          ELSE
           IF (qerror) THEN
            read(iunit,*,IOSTAT=ierr,end=100)
     &      rate(cnum),serr(cnum)
           ELSE
            read(iunit,*,IOSTAT=ierr,end=100) rate(cnum)
           ENDIF
          channel(cnum) = fchan + cnum - 1
         ENDIF
          IF (ierr.NE.0) THEN
          write(desc,'(a,i12)') errstr//' invalid number in file'
     &            //' on line ',j
          call fcecho(desc)
          ENDIF
        ENDIF
        IF (dtype.EQ.1) THEN
          IF (AMOD((rate(cnum)*10),10.0).NE.(0.0)) THEN
            write(desc,'(a,i12,a)') errstr//'line ',k,
     &            ' contains a non-integer counts value !'
            call rmvexsp(desc,desc2)
            call fcecho(desc2)
            IF (chanpres) THEN
               write(desc,'(a,i12,a)') ' (channel ',channel(cnum),
     &              ' contains a non-integer counts value )'
               call rmvexsp(desc,desc2)
               call fcecho(desc2)
            ENDIF
            errflg = 22
            return
          ENDIF
        ENDIF
        blank = .false.
        IF (k.EQ.rowrange2(j)) THEN
         IF (numblk.EQ.(rowrange2(j)-rowrange1(j)+1)) THEN
          IF (chatter.GE.5) THEN
           write(desc,'(a,i12,a,i12,a)') ' All rows in range ',
     &            rowrange1(j), 'to', rowrange2(j), 'are blank'
           call rmvexsp(desc,desc2)
           call fcecho(desc2)
          ENDIF
          totblk = totblk + 1
         ENDIF
         j = j+1
        ENDIF
        IF ((cnum.GE.2).AND.(chanpres)) THEN
         IF (channel(cnum).LE.channel(cnum-1)) THEN
          desc = errstr//' channel dataset in wrong order'
          call fcecho(desc)
          desc = ' with respect to channel numbering'
          call fcecho(desc)
          write(desc,'(a,i12,a,i12,a,i12,a,i12)') ' Channel(', cnum,
     &        ') is ', channel(cnum), ' and channel(', cnum-1,
     &        ') is ', channel(cnum-1)
          call rmvexsp(desc,desc2)
          call fcecho(desc2)
          errflg = 21
          return
         ENDIF
        ENDIF
       ELSE
         ierr = 0
         read(iunit,'( )',IOSTAT=ierr)
         IF (ierr.NE.0) THEN
          desc = errstr//' problem skipping out of range lines'
          call fcecho(desc)
         ENDIF
         IF (chatter.GE.30) THEN
           write(desc,'(a,i12)') ' Skipped line ',k
           call fcecho(desc)
         ENDIF
       ENDIF
      enddo
      IF (totblk.EQ.numranges) THEN
        desc = ' No data has been read as all specified'
     &        //' rows are blank'
        call fcecho(desc)
        errflg = 20
        return
      ENDIF
 100  nchan = cnum
      close(unit=iunit)
      IF (chatter.GE.20) THEN
        write(desc,'(a,i12)') ' Number of data rows :',nchan
        call rmvexsp(desc,desc2)
        call fcecho(desc2)
      ENDIF

c CALCULATE POISONIAN ERRORS IF REQUESTED

      IF (pois) THEN
        IF (qerror) THEN
         IF (chatter.GT.0) THEN
           desc = wrnstr//' Poissonian errors are not applied'
           call fcecho(desc)
           desc = ' as errors are already present'
           call fcecho(desc)
          ENDIF
          pois = .false.
        ELSE
         IF (dtype.EQ.2) THEN
          IF (exposure.LT.(0.0)) THEN
             IF (chatter.GT.0) THEN
               desc = wrnstr//' Poissonian errors cannot'
     &//' be applied as Exposure is < 0 '
               call fcecho(desc)
               desc = ' and data is in counts/sec'
               call fcecho(desc)
             ENDIF
             pois = .false.
          ENDIF
        ENDIF
       ENDIF
      ENDIF

      IF (pois) THEN
        do i=1,nchan
          IF (dtype.EQ.2) THEN
            cnts = NINT(rate(i) * exposure)
            serr(i) = calcpois(cnts)/exposure
          ELSE
            cnts = NINT(rate(i))
            serr(i) = calcpois(cnts)
          ENDIF
        enddo
      ENDIF
  200 FORMAT(A70)
      return
      end
c ---------------------------------------------------------------
c     END OF ASC_RDAT
c ---------------------------------------------------------------



*+ASC_WT
c     -----------------------------------------------------------
       subroutine asc_wt(outfile,infile,nchan,channel,rate,
     &            tlmin,detchans,
     &            dtype,qerror,serr,pois,telescope,instrume,
     &            detnam,filter,phaversn,exposure,areascal,
     &            backscal,corrscal,backfile,corrfile,
     &            respfile,ancrfile,date_obs,time_obs,
     &            date_end,time_end,ra_obj,dec_obj,equinox,
     &            hduclas2,chantype,
     &            taskname,errflg,killit,chatter)
c     ------------------------------------------------------------
c
c --- DESCRIPTION ---
c
c This routine writes FITS with a null primary array and an OGIP
c FITS PHA extension
c
c --- VARIABLES ---
c
      character*(*) outfile,infile
      character*(*) ra_obj,dec_obj
      character*(*) hduclas2,chantype
      character*(*) taskname
      character*(*) telescope,instrume,detnam,filter
      character*(*) phaversn
      character*(*) respfile,corrfile,ancrfile,backfile
      character*(*) date_obs,time_obs,date_end,time_end
      real exposure,backscal,areascal,corrscal,equinox
      logical qerror,pois,killit
      integer channel(*),nchan,dtype,errflg,chatter
      integer tlmin,detchans
      real serr(*),rate(*)
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf 1.0.0; August 3rd 1994
c
c Banashree Mitra Seifert 1.1.0: July 1996
c     . calling routine and called routine of asc_wt was not
c       matching.  Fixed this bug
c Ning Gan 1.1.1: July 1998
c     . Add check for the old two digit year date format and  write out the 
C       new four digit year format.
c ----------------------------------------------------------------
      character(5) version
      parameter (version ='1.1.0')
*-
c ----------------------------------------------------------------
c LOCALS
c
      character(30) errstr,wrnstr
      character(70) desc,hist(5),comm(5),errinfo
      integer nk_hist,nk_comm
      integer ounit,status
      logical qqual,qsys,qgroup
      integer grping(20),qualty(20),syserr(20)
      integer iy,im,id
c
c --- USER INFO ---
c
      errstr = ' ERROR: ASC_WT Ver '//version//':'
      wrnstr = ' WARNING: ASC_WT Ver '//version//':'
      IF (chatter.GE.15) THEN
         desc = ' ... using ASC_WT Ver '//version
         call fcecho(desc)
      ENDIF

c
c --- OPEN FITS FILE ---
c
      call cgetlun(ounit)
      call opnpa(outfile,chatter,ounit,killit,errflg)
      IF (errflg.NE.0) THEN
       errinfo=errstr//'opening and writing primary to outfile'
       call fcecho(errinfo)
       goto 100
      ENDIF
c
c --- WRITE PHA extension ---
c
      nk_hist = 1
      hist(1) = 'infile :'//infile
      nk_comm = 0
      qsys = .false.
      qqual = .false.
      qgroup = .false.
      call wtpha1(ounit,chatter,nk_hist,hist,nk_comm,
     &            comm,telescope,instrume,detnam,filter,
     &            phaversn,hduclas2,tlmin,exposure,areascal,
     &            backfile,backscal,corrfile,corrscal,respfile,
     &            ancrfile,detchans,chantype,channel,rate,dtype,
     &            qerror,serr,qsys,syserr,qqual,qualty,qgroup,
     &            grping,nchan,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr//' problem writing PHA ext'
        call fcecho(errinfo)
        goto 100
      ENDIF

c WRITE ADDITIONAL USEFULL keywords ---

      status = 0
      IF (date_obs.NE.' ') THEN
	  if(index(date_obs,'/').ne.0) then
	      status = 0
	      call fts2dt(date_obs,iy,im,id,status)
	      status = 0
	      call ftdt2s(iy,im,id,date_obs,status)
	      status = 0
          endif
       call ftpkys(ounit,'DATE-OBS',date_obs,
     &     'UTC Date of Observation start yyyy-mm-dd',status)
       IF (chatter.GE.5) THEN
         errinfo = wrnstr//' problem writing DATE-OBS'
         call wt_ferrmsg(status,errinfo)
       ENDIF
      ENDIF

      status = 0
      IF (time_obs.NE.' ') THEN
       call ftpkys(ounit,'TIME-OBS',time_obs,
     &     'UTC Time of Observation start (hh:mm:ss)',status)
       IF (chatter.GE.5) THEN
         errinfo = wrnstr//' problem writing TIME-OBS'
         call wt_ferrmsg(status,errinfo)
       ENDIF
      ENDIF

      status = 0
      IF (date_end.NE.' ') THEN
	  if(index(date_end,'/').ne.0) then
	      status = 0
	      call fts2dt(date_end,iy,im,id,status)
	      status = 0
	      call ftdt2s(iy,im,id,date_end,status)
	      status = 0
          endif
       call ftpkys(ounit,'DATE-END',date_end,
     &     'UTC Date of Observation end yyyy-mm-dd',status)
       IF (chatter.GE.5) THEN
         errinfo = wrnstr//' problem writing DATE_END'
         call wt_ferrmsg(status,errinfo)
       ENDIF
      ENDIF

      status = 0
      IF (time_end.NE.' ') THEN
       call ftpkys(ounit,'TIME-END',time_end,
     &     'UTC Time of Observation end (hh:mm:ss)',status)
       IF (chatter.GE.5) THEN
         errinfo = wrnstr//' problem writing TIME_END'
         call wt_ferrmsg(status,errinfo)
       ENDIF
      ENDIF

      status = 0
      IF (ra_obj.NE.' ') THEN
       call ftpkys(ounit,'RA_OBJ',ra_obj,
     &     'RA of object',status)
       IF (chatter.GE.5) THEN
         errinfo = wrnstr//' problem writing RA_OBJ'
         call wt_ferrmsg(status,errinfo)
       ENDIF
      ENDIF

      status = 0
      IF (dec_obj.NE.' ') THEN
       call ftpkys(ounit,'DEC_OBJ',dec_obj,
     &     'DEC of object',status)
       IF (chatter.GE.5) THEN
         errinfo = wrnstr//' problem writing DEC_OBJ'
         call wt_ferrmsg(status,errinfo)
       ENDIF
      ENDIF

      status = 0
      IF (equinox.NE.(0.0)) THEN
        call ftpkye(ounit,'EQUINOX',equinox,4,
     &      'Equinox of Celestial coord system',status)
        IF (chatter.GE.5) THEN
          errinfo = wrnstr//' problem writing EQUINOX'
          call wt_ferrmsg(status,errinfo)
        ENDIF
      ENDIF

      status = 0
      call ftpdat(ounit,status)
      status = 0
      call ftpkys(ounit,'CREATOR',taskname(1:17),
     &     's/w task which wrote this dataset',status)
      IF (chatter.GE.10) THEN
        errinfo = wrnstr//' writing CREATOR '
        call wt_ferrmsg(status,errinfo)
      ENDIF
 100  status = 0
      call ftclos(ounit,status)
      errflg=0
      return
      end
c -----------------------------------------------------------------
c     END OF ASC_WT
c -----------------------------------------------------------------


C
C     ---  Count the rows and return the maximum size of the dynamical
C          array
C
      Subroutine  get_phsize(infile,rows,phsize,killit,chatter,errflg)
      implicit none
      character*(*) infile
      character*(*) rows
      integer phsize
      logical killit
      integer chatter
      integer errflg

      character(80) message
      character(20) errstr 
      integer iunit
      integer i,ierr

c
c --- Count the rows  ---
c
      errstr = ' ERROR:GET_PHSIZE:'
      call cgetlun(iunit)
      call opasci(iunit,infile,1,70,killit,chatter,errflg)
      IF (errflg.NE.0) THEN
        message=errstr//'opening infile'
        call fcecho(message)
        errflg = 1
        return
      ENDIF
      i = 0
      do WHILE(.TRUE.)
        ierr = 0
        i = i+ 1
        read(iunit,'( )',IOSTAT=ierr,end = 50)
      enddo
 50   continue
      phsize = i - 1
      close(unit=iunit)
      return 
      end
