c
      subroutine earthn()

c Earth - Sun Corrections (without satellite orbit information)

c Author:  eal  August 1994, NASA/Goddard Space Flight Center

      IMPLICIT NONE
      character(40) taskname
      common /task/ taskname

c
      character(80) rastr,decstr,istring,colnam
      character(256) in_fil,ou_fil,eph_fil,log_fil
      INTEGER*4 tchat,lchat,ierr,LENACT,parse
      EXTERNAL LENACT
      DATA istring,parse /' ',0/


      call xrversion('earth2sun', taskname)
      ierr = 0

c Get parameters.

      CALL escinit(in_fil,colnam,ou_fil,rastr,decstr,eph_fil,ierr)

      IF(ierr.eq.0) THEN


c Execute program.

         CALL escmake(in_fil,colnam,ou_fil,rastr,decstr,eph_fil,ierr)

      ENDIF
      return
      END

      subroutine escinit(in_fil,colnam,ou_fil,rastr,decstr,
     &                  eph_fil,ierr)

c get parameter values for program ESC.

c  O  in_fil    (c)  Name of input file
c  O  colnam    (c)  name of column to do corrections on
c  O  ou_fil    (c)  name of output file
c  O  rastr     (c)  R.A. string
c  O  decstr    (c)  Declination string
c  O  eph_fil   (c)  Name of ephemeris file
c  O  ierr      (i)  error status

c subroutine calls:  uclgsi, uclpsi, uclgst, uclpst

c Author:  eal GSFC/HSTX HEASARC  August 1994

      IMPLICIT NONE


      include '../../include/io.inc'
      CHARACTER*(*) ou_fil,in_fil,rastr,decstr,eph_fil,colnam
      character(80) par_file
      character(160) logname
      real rbuf(1)
      integer lbuf, nbuf
      INTEGER*4 ierr,tchat,lchat,LENACT
      EXTERNAL LENACT
      parameter (subname = 'escinit:')
      if(ierr.ne.0) return
C get the terminal chattiness parameter
      call uclgsi ('tchat', tchat, ierr)
      if (ierr .ne. 0) then
         errm = ' Error getting TCHAT parameter'
         errm = subname//' '//errm
         call xaerror (errm, 5)
         goto 999
      endif

C get the logfile chattiness parameter
      call uclgsi ('lchat', lchat, ierr)
      if (ierr .ne. 0) then
         errm = ' Error getting LCHAT parameter'
         errm = subname//' '//errm
         call xaerror (errm, 5)
         goto 999
      endif

C set up chattiness of program
      call xchaty(tchat, lchat)


      if(lchat.ne.0) then
C Open log file
C get log file name
         call uclgst ('logname', logname, ierr)
         if (ierr .ne. 0) then
            errm = ' Error getting LOGNAME parameter'
            goto 999
         endif
         if ((logname .eq. 'INDEF').or.(logname .eq. ' ') .or. 
     $        (logname .eq. '-').or.(logname.eq.'default')) then
            logname='esc.log'
         endif
         logname='+'//logname
         lbuf=160
         rbuf(1) = 0.
         call logger(3,rbuf,nbuf,logname,lbuf)
         if(nbuf.lt.0) then
            errm='Error opening log file'
            goto 999
         endif
      endif

c Prompt user for input file name.

      CALL uclgst('infile',in_fil,ierr)
      CALL uclpst('infile',in_fil,ierr)
      errm = 'Error reading/writing input file name'
      IF(ierr.ne.0) GOTO 999

c Prompt user for column name.

      CALL uclgst('colnam',colnam,ierr)
      CALL uclpst('colnam',colnam,ierr)
      errm = 'Error reading/writing column name'
      IF(ierr.ne.0) GOTO 999

c Prompt user for output file.
 
      CALL uclgst('outfile',ou_fil,ierr)
      CALL uclpst('outfile',ou_fil,ierr)
      errm = 'Error reading/writing output file name'
      IF(ierr.ne.0) GOTO 999

c Prompt user for RA and dec.
 
      CALL uclgst('ra',rastr,ierr)
      CALL uclpst('ra',rastr,ierr)
      CALL uclgst('dec',decstr,ierr)
      CALL uclpst('dec',decstr,ierr)
      errm = 'Error reading/writing RA/DEC strings'
      IF(ierr.ne.0) GOTO 999


c Read hidden SCC file name

      CALL uclgst('ephfile',eph_fil,ierr)
      CALL uclpst('ephfile',eph_fil,ierr)
      errm = 'Error reading/writing SCC correction file name'
      IF(ierr.ne.0) GOTO 999


      GOTO 1000

999   CONTINUE
      errm = subname//' '//errm
      CALL xaerror(errm, 5)
1000  CONTINUE

      RETURN
      END


      subroutine escmake(in_fil,colnam,ou_fil,rastr,decstr,eph_fil,ierr)

c MAKE timing corrections for program ESC.

c This routine opens three FITS files: an input data file (lui)
c and an output file (luo), and an ephemeris file (lue).  The
c output file is a straight copy of the input file, with all TIME
c values -- EVENT data, GTI's and timing keywords -- corrected to the solar
c system barycenter (or actaully to whatever correction is given by the
c corretion table).  Several checks for compatiblity are also made in this
c routine:  the input file keyword TIMEREF should be 'LOCAL'; the RA and 
c DEC in the correction file should agree with values input by the user;
c Times in the data file should be contained within start and stop of
c the correction table.

c  I  in_fil    (c)  Name of input file
c  I  colnam    (c)  name of column to do corrections on
c  I  ou_fil    (c)  name of output file
c  I  rastr     (c)  R.A. string
c  I  decstr    (c)  Declination string
c  I  eph_fil   (c)  Name of ephemeris file
c  I  ierr      (i)  Error status

c Primary internal variables:

c     lui       (i)  Lu of input FITS file
c     luo       (i)  Lu of output FITS file
c     lue       (i)  Lu of SCC correction FITS file
c     iext     (i)  Array containing the number for the EVENTS 
c                      extension (1) and the GTI extension (2) in
c                      the input events file.

c Author:  eal GSFC/HSTX HEASARC  August 1994
c Adapted from subroutine abcmake.

      IMPLICIT NONE


      include '../../include/io.inc'
      integer kmax,cmax
      parameter (kmax = 40, cmax = 100)

      logical anynul
      CHARACTER*(*) in_fil,ou_fil,eph_fil,rastr,decstr,colnam
      character(16) timeunit,ttype(cmax),tunit(cmax),tform(cmax)
     &   ,extname,zerodate,zerotime
      character(20) keywords(kmax),cbuf,cdum
      character(80) comm
      INTEGER*4 ierr,lui,luo,lue,block,ftstat,kystat,pcount
     &   ,xtend,ibuf,LENACT,ichat,idum1,idum2,idum3,idum4
     &   ,equinox,n,refflag,jd,jdcor
     &   ,iext(2),bcext(cmax),ivect(10),htunit,ctunit
     &   ,nrows,nfield,itype,iopt(15)
      DOUBLE PRECISION alpha,delta,frc,frccor,dtoffset
     &   ,timuncor,timcor,tstart,tstop,dtzero,ddum,ddum1,ddum2
     &   ,stacor,stocor,dtsta,dtsto,startout,stopout,zeroout
     &   ,dtint,nulvd,dumvec(3),pi

      external LENACT
      data block /2880/
      DATA keywords /kmax*'                    '/
      DATA bcext /cmax*0/
      data equinox /2000/
      data nulvd /1.2d-34/
      data dumvec /3*0.d0/
      data iopt /15*0/
      data ivect /10*0/
      parameter (cdum = ' ')
      parameter (subname = 'escmake:')

      if(ierr.ne.0) return

      ftstat = 0
      errm = ' '
      iext(1)=0
      iext(2)=0
      startout = 0.d0
      stopout  = 0.d0
      zeroout  = 0.d0

c------------------------------------------------------------
c------------------------------------------------------------
c Open files, find extensions, revise certain keywords, etc.
c------------------------------------------------------------
c------------------------------------------------------------



c Open the input file: rwmode=0 means read only.

      lui = 0
      luo = 0
      lue = 0
      CALL xropnfil(in_fil,ou_fil,10,lui,luo,iext,itype,ierr)
      if ( ierr.ne.0 ) then
         errm = ' opening input/output files'
         goto 999
      endif

c Write infile information to screen and log.

      CALL xrftwext(lui,10,2,iext,ierr)

c Open the Ephemeris file

      
      CALL getlun(lue)
      CALL ftopen(lue,eph_fil,0,block,ftstat)
      CALL ftmahd(lue,2,xtend,ftstat)
      if(ftstat.ne.0) then
         ierr = ftstat
         errm = ' opening ephemeris file ' // eph_fil
         GOTO 999
      endif

c Decode RA and DEC strings.

      CALL getra(rastr,equinox,alpha,ierr)
      CALL getdec(decstr,equinox,delta,ierr)
      errm = 'Error decoding RA/DEC in getra/dec'
      IF(ierr.gt.0) GOTO 999

c Convert RA and DEC to radians.
      pi = 2.d0*dasin(1.d0)
      alpha = alpha * pi /180.d0
      delta = delta * pi /180.d0      

c Go to EVENTS extension in lui and luo for keyword modification.

      CALL ftmahd(lui,iext(1),xtend,ftstat)
      CALL ftmahd(luo,iext(1),xtend,ftstat)

c All calls with ftstat to here should return 0.  Below does not matter.

      IF(ftstat.ne.0) THEN
         CALL ftgerr(ftstat,errm)
         ierr = ftstat
         GOTO 999
      ENDIF

      CALL xrftgdes(lui,keywords)

c Warn against doing corrections if they have been done already.

      cbuf = keywords(27)(:16)
      CALL rmvlbk(cbuf)
      ibuf = lenact(cbuf)
      if((keywords(27).ne.'LOCAL').and.(ibuf.ne.0)) 
     &   CALL xwarn(' TIMREF keyword is not `LOCAL`',1)

c TIMEREF.

      CALL ftdkey(luo,'TIMEREF',ftstat)
      ftstat = 0
      comm = 'Times corrected to solar system barycenter'
      CALL ftpkys(luo,'TIMEREF','SOLARSYSTEM',comm,ftstat)

c CLOCKAPP.

      CALL ftdkey(luo,'CLOCKAPP',ftstat)
      ftstat = 0
      comm = 'Clock corrections have been applied'
      CALL ftpkyl(luo,'CLOCKAPP',.true.,comm,ftstat)

c TIMESYS.

      kystat = 0
      CALL ftgkys(luo,'TIMESYS',cbuf,comm,kystat)
      IF(kystat.eq.0) THEN
         CALL ftgkys(luo,'ZERODATE',zerodate,comm,kystat)
         CALL ftgkys(luo,'ZEROTIME',zerotime,comm,kystat)
         IF(kystat.eq.0) THEN
            CALL rmvlbk(zerodate)
            read(zerodate,'(i2,x,i2,x,i2)') idum1,idum2,idum3
            idum3 = idum3 + 1900
            write(zerodate,'(i4,x,i2.2,x,i2.2)') idum3,idum2,idum1
            cbuf = zerodate(:lenact(zerodate)) // ' ' //
     &             zerotime(:lenact(zerotime))
            CALL ftmkys(luo,'TIMESYS',cbuf,'&',ftstat)
         ELSE
            CALL ftdkey(luo,'TIMESYS',ftstat)
         ENDIF
      ENDIF

c TASSIGN can be deleted.

      kystat = 0
      CALL ftdkey(luo,'TASSIGN',kystat)

c XRONOS TIMING KEYWORDS will be replaced.

      kystat = 0
      CALL ftdkey(luo,'TSTART',kystat)
      kystat = 0
      CALL ftdkey(luo,'TSTARTI',kystat)
      kystat = 0
      CALL ftdkey(luo,'TSTARTF',kystat)
      kystat = 0
      CALL ftdkey(luo,'TSTOP',kystat)
      kystat = 0
      CALL ftdkey(luo,'TSTOPI',kystat)
      kystat = 0
      CALL ftdkey(luo,'TSTOPF',kystat)
      kystat = 0
      CALL ftdkey(luo,'TIMEZERI',kystat)
      kystat = 0
      CALL ftdkey(luo,'TIMEZERF',kystat)
      kystat = 0
      CALL ftdkey(luo,'TIMEZERO',kystat)
      kystat = 0
      CALL ftdkey(luo,'TIMEUNIT',kystat)

c-----------------------------------------------------------
c-----------------------------------------------------------
c Files are now ready.  Do the corrections.
c-----------------------------------------------------------
c-----------------------------------------------------------

c---------------------
c Initialization part.
c---------------------

c Move to RATE TABLE extension in input and output.

      CALL ftmahd(lui,iext(1),xtend,ftstat)
      CALL ftmahd(luo,iext(1),xtend,ftstat)

c Read essential keywords.

      CALL ftghbn(lui,cmax,nrows,nfield,ttype,tform,tunit
     &           ,extname,pcount,ftstat)

c Set vector columns using TTYPEnnn keywords.

      CALL xrftgcol(nfield,ttype,tunit,ivect)

c Make sure the time column was found.

      IF(ivect(1).le.0) THEN
         errm = 'TIME column not found in event list'
         GOTO 999
      ENDIF

c File type is EVENTS (equivalently -- i.e., it must have a TIME column
c                      and we don't care about any RATE columns).

      itype = 1

c Get timing information.

      dtoffset = 0.d0
      CALL xrftgtky(lui,iopt,nrows,itype,ivect,htunit,ctunit
     &             ,dtzero,dtoffset,dtint,dtsta,dtsto,idum1,ddum2
     &             ,refflag,ftstat)
      IF(refflag.lt.0) THEN
         ierr = 1
         errm = 'Unable to convert times to Julian Day.'
         goto 999
      ENDIF
      dtsta = dtsta + 2440000.5
      dtsto = dtsto + 2440000.5
      dtoffset = dtoffset + 2440000.5

c Fudge to ignore errors about time units.

      if((abs(ftstat).eq.1048).or.(abs(ftstat).eq.1052).or.
     &   (abs(ftstat).eq.1053)) ftstat = 0

c-----------------
c Rate Table part.
c-----------------

c Loop over events.

      n = 0
      DO WHILE ((n.lt.nrows).and.(ierr.eq.0))

         n = n + 1

c Read input time (timuncor) from the file.

         anynul = .false.
         CALL ftgcvd(lui,ivect(1),n,1,1,nulvd,timuncor,anynul,ftstat)

c Convert to JD.  Integer part is jd, fractional part is 0 <= frc < 1.

         CALL xrdectun(cdum,ctunit,2,timuncor,ierr)
         jd = int(timuncor) + int(dtzero) + int(dtoffset)
         frc = timuncor - dint(timuncor) + dtzero - dint(dtzero)
     &       + dtoffset - dint(dtoffset)
         jd = jd + int(frc)
         frc = frc - dint(frc)

c Do barycenter correction.

         CALL barycen(lue,.false.,alpha,delta,jd,frc,dumvec,jdcor
     &               ,frccor,ierr)

c Convert back into original time units.

         timcor = (dble(jdcor) - dtoffset - dtzero) + frccor
         CALL xrdectun(cdum,2,ctunit,timcor,ierr)
         IF(ierr.ne.0) RETURN

c Write to output file.

         CALL ftpcld(luo,ivect(1),n,1,1,timcor,ftstat)

      ENDDO

c Store tzero for header output

      zeroout  = dtzero
      CALL xrdectun(cdum,2,htunit,zeroout,ierr)


c----------
c GTI part.
c----------

c Skip this part if GTIs are not present.

      IF(iext(2).le.0) THEN

c start time.  Integer part is jd, fractional part is 0 <= frc < 1.

         jd = int(dtsta)
         frc = dtsta - dint(dtsta)
         jd = jd + int(frc)
         frc = frc - dint(frc)
 
c Do barycenter correction.
 
         CALL barycen(lue,.false.,alpha,delta,jd,frc,dumvec,jdcor
     &               ,frccor,ierr)

c Convert back into original time units.

         stacor = (dble(jdcor) - dtoffset) + frccor
         CALL xrdectun(cdum,2,htunit,stacor,ierr)
         IF(ierr.ne.0) RETURN

c stop time.  Integer part is jd, fractional part is 0 <= frc < 1.

         jd = int(dtsto)
         frc = dtsto - dint(dtsto)
         jd = jd + int(frc)
         frc = frc - dint(frc)

c Do barycenter correction.

         CALL barycen(lue,.false.,alpha,delta,jd,frc,dumvec,jdcor
     &               ,frccor,ierr)

c Convert back into original time units.

         stocor = (dble(jdcor) - dtoffset) + frccor
         CALL xrdectun(cdum,2,htunit,stocor,ierr)
         IF(ierr.ne.0) RETURN

c Store start and stop times to be added to the headers later.

         startout = stacor
         stopout  = stocor

         GOTO 500
      ENDIF

c Move to GTI extension in input and output.

      CALL ftmahd(lui,iext(2),xtend,ftstat)
      CALL ftmahd(luo,iext(2),xtend,ftstat)

c Read mandatory binary header keywords from extension.

      CALL ftghbn(lui,cmax,nrows,nfield,ttype,tform,tunit
     &           ,extname,pcount,ftstat)

c Get column numbers.

      CALL xrftgcol(nfield,ttype,tunit,ivect)

c Make sure the start and stop columns were found.
 
      IF((ivect(7).le.0).or.(ivect(8).le.0)) THEN
         errm = 'START and/or STOP column not found in GTI list'
         GOTO 999
      ENDIF

c File type is GTI.

      itype = 4

c Get timing keywords.

      CALL xrftgtky(lui,iopt,nrows,itype,ivect,idum1,ctunit
     &             ,dtzero,ddum,ddum1,dtsta,dtsto,idum2,ddum2
     &             ,idum4,ftstat)

c Loop over GTIs.

      n = 0
      DO WHILE ((n.lt.nrows).and.(ierr.eq.0))

         n = n + 1

c Read GTI start and stop (tstart,tstop) from the file.

         anynul = .false.
         tstart = 0.d0
         tstop = 0.d0
         CALL ftgcvd(lui,ivect(7),n,1,1,nulvd,tstart,anynul,ftstat)
         CALL ftgcvd(lui,ivect(8),n,1,1,nulvd,tstop ,anynul,ftstat)

c Convert tstart to JD.  Integer part is jd, fractional part is 0 <= frc < 1.

         CALL xrdectun(cdum,ctunit,2,tstart,ierr)
         jd = int(tstart) + int(dtzero) + int(dtoffset)
         frc = tstart - dint(tstart) + dtzero - dint(dtzero)
     &       + dtoffset - dint(dtoffset)
         jd = jd + int(frc)
         frc = frc - dint(frc)
 
c Do barycenter correction.
 
         CALL barycen(lue,.false.,alpha,delta,jd,frc,dumvec,jdcor
     &               ,frccor,ierr)

c Convert back into original time units.

         stacor = (dble(jdcor) - dtoffset - dtzero) + frccor
         CALL xrdectun(cdum,2,ctunit,stacor,ierr)
         IF(ierr.ne.0) RETURN

c Convert tstop to JD.  Integer part is jd, fractional part is 0 <= frc < 1.

         CALL xrdectun(cdum,ctunit,2,tstop,ierr)
         jd = int(tstop) + int(dtzero) + int(dtoffset)
         frc = tstop - dint(tstop) + dtzero - dint(dtzero)
     &       + dtoffset - dint(dtoffset)
         jd = jd + int(frc)
         frc = frc - dint(frc)

c Do barycenter correction.

         CALL barycen(lue,.false.,alpha,delta,jd,frc,dumvec,jdcor
     &               ,frccor,ierr)

c Convert back into original time units.

         stocor = (dble(jdcor) - dtoffset - dtzero) + frccor
         CALL xrdectun(cdum,2,ctunit,stocor,ierr)
         IF(ierr.ne.0) RETURN

c Write to output file.

         CALL ftpcld(luo,ivect(7),n,1,1,stacor,ftstat)
         CALL ftpcld(luo,ivect(8),n,1,1,stocor,ftstat)

c Store start and stop times to be added to the headers later.

         if(n.eq.1)     startout = stacor
         if(n.eq.nrows) stopout  = stocor

      ENDDO

c Adjust tzero for header output

      CALL xrdectun(cdum,2,htunit,dtzero,ierr)

c Done with GTIs.

c Write timing keywords and comments to the EVENT extension.

      comm = 'Start time for this extension'
      CALL ftukyd(luo,'TSTART',startout,16,comm,ftstat)
      comm = 'Stop time for this extension'
      CALL ftukyd(luo,'TSTOP',stopout,16,comm,ftstat)
      if ( dtzero.ne.0.d0 ) then
         comm = 'Time offset for this extension'
         CALL ftukyd(luo,'TIMEZERO',dtzero,16,comm,ftstat)
      endif
      comm = 'Units for header timing keywords'
      timeunit = 's'
      if ( ctunit.eq.2 ) timeunit = 'd'
      CALL ftukys(luo,'TIMEUNIT',timeunit,comm,ftstat)

500   CONTINUE

c Write timing keywords and comments to the RATE TABLE extension.

      CALL ftmahd(luo,iext(1),xtend,ftstat)
      comm = 'Start time for this extension'
      CALL ftpkyd(luo,'TSTART',startout,16,comm,ftstat)
      comm = 'Stop time for this extension'
      CALL ftpkyd(luo,'TSTOP',stopout,16,comm,ftstat)
      if ( zeroout.ne.0.d0 ) then
         comm = 'Time offset for this extension'
         CALL ftpkyd(luo,'TIMEZERO',zeroout,16,comm,ftstat)
      endif
      comm = 'Units for header timing keywords'
      timeunit = 's'
      if ( htunit.eq.2 ) timeunit = 'd'
      CALL ftpkys(luo,'TIMEUNIT',timeunit,comm,ftstat)

c -------------
c Closing part.
c -------------

c Trap errors.

      IF(ftstat.ne.0) THEN
         ierr = ftstat
         write(errm,*) 'escmake: Fitsio status ', ftstat
         GOTO 999
      ENDIF

      GOTO 1000
999   CONTINUE
      errm = subname//' '//errm
      CALL xaerror(errm,1)
1000  CONTINUE

c Close all files
 
      ftstat = 0
      CALL ftclos(lui,ftstat)
      CALL ftclos(luo,ftstat)
      CALL ftclos(lue,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         errm = subname//' '//'Closing fits files'
         CALL xaerror(errm,1)
      ENDIF

c Free logical units.

      CALL frelun(lui)
      CALL frelun(luo)
      CALL frelun(lue)
c#       CALL frelun(-1)

      return
      end
