      subroutine abcmake(in_fil,bc_fil,ou_fil,rastr,decstr,scc_fil,ierr)

c MAKE timing corrections for program ABC.

c This routine opens 4 FITS files: 
c    an input data file (lui);
c    a barycenter correction table (lub);
c    an output file (luo);
c    and a spacecraft clock correction file (luc)  
c The output file is a straight copy of the input file, with all TIME
c values -- EVENT data, GTI's and timing keywords -- corrected to the solar
c system barycenter (or actually to whatever correction is given by the
c corretion table).  

c Several preliminary checks are made in this routine:  

c    the input file should have the keyword TIMEREF = 'LOCAL'; 

c    the RA and  DEC in the correction file should agree with values 
c    input by the user;

c    Times in the data file should be contained within start and stop of
c    the correction table;

c    The correction table should be contained in extensions that have the
c    keyword EXTAME = 'TIMEREF'.

c    The routine looks for the MJDREF keyword based on US ROSAT old and
c    new-type formats (either the pair XS-MJDRD, XS-MJDRF or MJDREFI,
c    MJDREFF respectively).  If this is not found the program exits.
c    The MJD reference value is used to convert times internally to
c    compare with the correction table, which is listed in JD.  The
c    corrected times are written to the output file in the original time
c    system.

c Several keywords are modified in the output file:

c    TIMEREF = 'SOLARSYSTEM'
c    CLOCKAPP = T
c    TIMESYS is constructed by concatenating the ZEROTIME and ZERODATE 
c       keywords from input file.  If the ZEROTIME and ZERODATE
c       keywords are not found TIMESYS is deleted.
c    TASSIGN is deleted.
c    TSTART, TSTOP, TIMEZERO and TIMEUNIT are updated.  

c The routine then gathers information about the structure of event list
c and enters a loop for the correction.  It reads each time from the file,
c correct for spacecraft clock (subroutine scc2utc), subtracts off the
c MJD reference value and gets the corrected time from subroutine abcgcor.
c It keeps track of any photons that arrived in gaps of greater than
c 2 minutes in the barycenter correction table and records them in the
c extension header.  At the end of the loop the number of gap photons
c is recorded in the priamry header and a warning is issued if there were
c any.

c    The routine assumes the input times are listed in their spacecraft 
c    clock values (in seconds).  They
c    are written to the new file in seconds from the MJD reference value.

c The routine then does the same for the GTI extension, keeping track of
c the earliest corrected GTI start and the latest corrected GTI stop.
c These are then used as the TSTART and TSTOP keyword values in both
c extension headers.

c  I  in_fil    (c)  Name of input file
c  I  bc_fil    (c)  Name of barycenter correction file
c  I  ou_fil    (c)  name of output file
c  I  rastr     (c)  R.A. string
c  I  decstr    (c)  Declination string
c  I  scc_fil   (c)  Name of SCC correction file
c  I  ierr      (i)  Error status

c Primary internal variables:

c     lui       (i)  Lu of input FITS file
c     luo       (i)  Lu of output FITS file
c     lub       (i)  Lu of barycenter correction FITS file
c     luc       (i)  Lu of SCC correction FITS file
c     ichat     (i)  Chattiness parameter
c     iext      (i)  Array containing the number for the EVENTS 
c                      extension (1) and the GTI extension (2) in
c                      the input events file.
c     bcext     (i)  Array containing the numbers of the TIMEREF extensions
c                    in the barycenter correction table.

c Author:  eal GSFC/HSTX HEASARC  February 1994
c
c Modification History:
c     11/7/94 EAG add returns on error
c
c Modified by Banashree Mitra Seifert (April,1996) at position:
c       second call to xrftgtky subroutine
c Modified by Ning Gan (June,1998) 
c       changed length of zerodate and zerotime from 16 to 68.
c       used fts2dt to read the date. 
c Modified by MFC 2006-08-27: 
c    initialize uninitialized variables 
c     iext(1),iext(2)=0
c     iopt(1:10)=0
c     ifound=0
c    create a temporary file and only rename as output file
c        if program completes successfully
c    update checksum
c

      IMPLICIT NONE

c      INCLUDE 'abcdef.inc'

      integer kmax,cmax,hdunum
      parameter (kmax = 40, cmax = 100)

      logical outside,anynul
      CHARACTER*(*) in_fil,bc_fil,ou_fil,scc_fil,rastr,decstr
      character(16) timeunit,ttype(cmax),tunit(cmax),tform(cmax)
     &   ,extname
      character(10) ou_filtmp
      character(68) zerodate,zerotime
      character(20) keywords(kmax),cbuf
      character(80) comm,errm
      INTEGER*4 ierr,lui,luo,lub,luc,block,ftstat,kystat
     &   ,xtend,ibuf,LENACT,ifound,ichat,itime,idum1,idum2,idum3
     &   ,equinox,n,noutside,refflag,mjdrefi
     &   ,iext(2),bcext(cmax),ivect(10),htunit,ctunit
     &   ,nrows,nfield,pcount,itype,iopt(10)
      DOUBLE PRECISION alpha,delta,ra,dec
     &   ,timuncor,timcor,tstart,tstop,dtzero,ddum,ddum1,ddum2
     &   ,stacor,stocor,dioffset,dtsta,dtsto,startout,stopout
     &   ,dtint,nulvd,mjdreff,frc,dzout

      external LENACT
      data block /2880/
      DATA keywords /kmax*'                    '/
      DATA bcext /cmax*0/
      data equinox /2000/
      data nulvd /1.2d-34/

      if(ierr.ne.0) return

      ftstat = 0
      errm = ' '
c
c     INITIALIZATION
c
      iext(1)=0
      iext(2)=0
      do n=1,10
         iopt(n)=0
      enddo
      ou_filtmp='abcout.tmp'

c------------------------------------------------------------
c------------------------------------------------------------
c Open files, find extensions, revise certain keywords, etc.
c------------------------------------------------------------
c------------------------------------------------------------

c >>> Still need to define this right.<<<

      ichat = 5

      CALL xropnfil(in_fil,ou_filtmp,ichat,lui,luo,iext,itype,ierr)
      if (ierr .ne. 0) return

c Write infile information to screen and log.

      CALL xrftwext(lui,5,2,iext,ierr)

c Open the barycenter file.

      CALL getlun(lub)
      CALL ftopen(lub,bc_fil,0,block,ftstat)
      if(ftstat.ne.0) then
         errm = ' opening barycenter file ' // bc_fil
         ierr = ftstat
         GOTO 999
      endif

c Get labels on all TIMREF extensions.
      ifound=0
      CALL xrftgext(lub,5,'TIMEREF',cmax,bcext,idum1,ifound,ierr)
      if (ierr .ne. 0) then
              errm=' xrftgext Failure'
              write(*,*) 'xrftgext Failure'
              ierr = ftstat
              GOTO 999
      endif
c Get full MJD ref.  Look for ROSAT old-type keywords and OGIP standard.

      CALL ftmahd(lui,iext(1),xtend,ftstat)
      kystat = 0
      CALL ftgkyj(lui,'MJDREFI',mjdrefi,comm,kystat)
      IF(kystat.eq.0) THEN
         CALL ftgkyd(lui,'MJDREFF',mjdreff,comm,kystat)
      ELSE
         kystat = 0
         CALL ftgkyj(lui,'XS-MJDRD',mjdrefi,comm,kystat)
         CALL ftgkyd(lui,'XS-MJDRF',mjdreff,comm,kystat)
      ENDIF
      IF(kystat.ne.0) THEN
         CALL ftclos(lui,ftstat)
         ierr = -1
         errm = 'Unable to find MJD reference value.'
         GOTO 999
      ENDIF
c Get the SCC - UTC file name and open it.

c      CALL ptend(disk,directory,scc_fil)
      CALL getlun(luc)
      CALL ftopen(luc,scc_fil,0,block,ftstat)
      if(ftstat.ne.0) then
         ierr = ftstat
         errm = ' opening SCC correction file ' // scc_fil
         GOTO 999
      endif

c Decode RA and DEC strings.

      CALL parsera(rastr,equinox,alpha,ierr)
      CALL parsedec(decstr,equinox,delta,ierr)
      errm = 'Error decoding RA/DEC in parsera/dec'
      IF(ierr.gt.0) GOTO 999

c Go to the primary header in the correction table for RA and DEC.
      CALL ftmahd(lub,1,xtend,ftstat)
      CALL xrftgdes(lub,keywords)
      read(keywords( 9),*,err = 40) ra
      read(keywords(13),*,err = 40) dec
      GOTO 50
40    CALL xwarn('Unable to read RA and DEC from input file',1)
50    CONTINUE

c Compare user ra and dec to that in the table.

c >>> How much error tolerance for RA and DEC?  Here assuming 1 arc sec. <<<

      if((dabs(ra  - alpha).gt.1.d0/3600.d0) .or.
     &   (dabs(dec - delta).gt.1.d0/3600.d0)) 
     &   CALL xwarn('RA and DEC not compatible with barycenter table',1)

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
	     call fts2dt(zerodate,idum3,idum2,idum1,kystat)
	     kystat = 0 
c            read(zerodate,'(i2,x,i2,x,i2)') idum1,idum2,idum3
c            idum3 = idum3 + 1900
            write(zerodate,'(i4,x,i2.2,x,i2.2)') idum3,idum2,idum1
            cbuf = zerodate(:lenact(zerodate)) // ' ' //
     &             zerotime(:lenact(zerotime))
            CALL ftmkys(luo,'TIMESYS',cbuf,'&',ftstat)
         ELSE
            CALL ftdkey(luo,'TIMESYS',ftstat)
         ENDIF
      ENDIF

c#       CALL ftdkey(luo,'TIMESYS',ftstat)
c#       ftstat = 0
c#       comm = 'Times listed in Modified Julian Day'
c#       CALL ftpkys(luo,'TIMESYS','MJD',comm,ftstat)

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
      timeunit = 's'

c-----------------------------------------------------------
c-----------------------------------------------------------
c Files are now ready.  Do the corrections.
c-----------------------------------------------------------
c-----------------------------------------------------------

c The flag refflag provides
c information to the writers so they can reconstruct the time system
c in the original file.  If refflag is changed manually, the writers
c will write the times in the specified time sysytem:
c refflag = 0  =>  There is an MJDREF in the file used to reconstruct the times
c refflag = 1  =>  times will be written in MJD
c refflag = 2  =>  times will be written in JD
c refflag = 3  =>  times will be written in TJD
c refflag < 0  =>  times are taken from the input file verbatim
c                  In this case the times in the program are not in MJD
c                  because no MJDREF was found.

c The flag ctunit provides information to the writers for which units
c (seconds or days) to write the times to the columns.  The value
c is returned from the readers, so nothing need be done to write
c the times out the same way.  If you wish to specify the units in the
c output file set ctunit = 2 for days and ctunit = 1 for seconds
c (only cases supported).

c The flag htunit works the same way as ctunit, except that it applies to
c times written the extension header.

c Example: to convert timuncor to TJD,
c      CALL xrdectun(cbuf,ctunit,2,timuncor,ierr)
c      timuncor = timuncor + (dtzero + dtoffset)

c Example: to reconstruct the full time in the original column time units,
c      dtmp = dtzero
c      CALL xrdectun(cbuf,2,ctunit,dtmp,ierr)
c      timuncor = timuncor + dtzero

c Example: to replace start and stop times and TIMEZERO in the original units:
c      dtsta = dtsta - dtoffset
c      CALL xrdectun(cbuf,2,htunit,dtsta,ierr)
c      dtsto = dtsto - dtoffset
c      CALL xrdectun(cbuf,2,htunit,dtsto,ierr)
c      CALL xrdectun(cbuf,2,htunit,dtzero,ierr)
c      IF(htunit.eq.1) timeunit = 's'
c      IF(htunit.eq.2) timeunit = 'd'
c      CALL xrftptky(luo,tstart,tstop,timeunit,dtzero,ftstat)
c Note that the conversion is done in place.

c---------------------
c Initialization part.
c---------------------

c# c Make a first call to the correction routine and retrieve the internal 
c# c offset.  The offset strips off from all the times the first 
c# c JD integer value from the correction table to maximize accuracy.
c# 
c#       CALL abcgcor(lub,cmax,bcext,ddum,dioffset,outside,timcor,ierr)
      dioffset = dble(mjdrefi) + 2400000.5

c Output will be in MJD.
c To make the output in days:
c#       ctout = 2
c To make the writers think there is an MJDREF to go with the stripped times:
c#       refout = 0
c#       dtout = 2440000.5d0 - dioffset

c Set dtzero for output

c#       dzout = dble(int(dioffset - 2400000.5d0))
      dzout = 0.d0

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

      CALL xrftgtky(lui,0,iopt,nrows,itype,ivect,htunit,ctunit
     &             ,dtzero,ddum,dtint,dtsta,dtsto,idum1,ddum2
     &             ,refflag,ftstat)

c Fudge to ignore errors about time units.

      if((abs(ftstat).eq.1048).or.(abs(ftstat).eq.1052).or.
     &   (abs(ftstat).eq.1053)) ftstat = 0

c-----------------
c Rate Table part.
c-----------------

c Loop over events.

      n = 0
      noutside = 0
      DO WHILE ((n.lt.nrows).and.(ierr.eq.0))

         n = n + 1

c Read input time (timuncor) from the file.

         anynul = .false.
         CALL ftgcvd(lui,ivect(1),n,1,1,nulvd,timuncor,anynul,ftstat)

c Convert from Spacecraft Clock to JD - offset.

         CALL scc2utc(luc,ichat,timuncor,itime,frc,ierr)
         if (ierr .ne. 0) return
         timuncor = frc + (dble(itime) - dioffset)

c Do barycenter correction.

         CALL abcgcor(lub,cmax,bcext,timuncor,dioffset,outside,timcor
     &               ,ierr)

c Convert to seconds since MJDREF.

         timcor = (timcor - mjdreff) *86400.d0

c Keep tabs on the number of photons that fell in gaps in the correction table.

         if(outside) then
            noutside = noutside + 1
c#             write(errm,101) timcor + (dioffset - 2400000.5d0)
            write(errm,101) timcor
101         format('Time ',1pe24.16,' is in a bc table gap.')
            CALL ftpcom(luo,errm,ftstat)
         endif

c Write to output file.

         CALL ftpcld(luo,ivect(1),n,1,1,timcor,ftstat)

c#          CALL wrtim(luo,n,ivect(1),dtout,ctout,refout,timcor,dzout,ierr)

      ENDDO

      IF(noutside.gt.0) THEN
         write(errm,102) noutside
102      format(i11,' photons arrived in gaps in the bc correction'
     &             ,' table.')
         CALL ftmahd(luo,1,xtend,ftstat)
         CALL ftpcom(luo,errm,ftstat)
         CALL ftmahd(luo,iext(1),xtend,ftstat)
         CALL ftpcom(luo,errm,ftstat)
         CALL xwarn(errm,ichat)
      ENDIF

c----------
c GTI part.
c----------

c Skip this part if GTIs are not present.

      IF(iext(2).le.0) GOTO 500

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

c the next line is modified by Banashree Mitra Seifert (April,1996)
c       IDUM1 is replaced by HTUNIT

c      CALL xrftgtky(lui,0,iopt,nrows,itype,ivect,idum1,ctunit
      CALL xrftgtky(lui,0,iopt,nrows,itype,ivect,htunit,ctunit
     &             ,dtzero,ddum,ddum1,dtsta,dtsto,idum2,ddum2
     &             ,idum3,ftstat)

c Loop over GTIs.

      n = 0
      noutside = 0
      DO WHILE ((n.lt.nrows).and.(ierr.eq.0))

         n = n + 1

c Read GTI start and stop (tstart,tstop) from the file.

         anynul = .false.
         CALL ftgcvd(lui,ivect(7),n,1,1,nulvd,tstart,anynul,ftstat)
         CALL ftgcvd(lui,ivect(8),n,1,1,nulvd,tstop ,anynul,ftstat)

c Convert from Spacecraft Clock to JD - offset.

         CALL scc2utc(luc,ichat,tstart,itime,frc,ierr)
         tstart = frc + (dble(itime) - dioffset)
         CALL scc2utc(luc,ichat,tstop ,itime,frc,ierr)
         tstop  = frc + (dble(itime) - dioffset)

c Do barycenter correction (start column).

         CALL abcgcor(lub,cmax,bcext,tstart,dioffset,outside,stacor
     &               ,ierr)

c Convert to seconds since MJDREF.

         stacor = (stacor - mjdreff) *86400.d0

c Keep tabs on the number of gtis that fell in gaps in the correction table.

         if(outside) noutside = noutside + 1

c Do barycenter correction (stop column).

         CALL abcgcor(lub,cmax,bcext,tstop ,dioffset,outside,stocor
     &               ,ierr)

c Convert to seconds since MJDREF.

         stocor = (stocor - mjdreff) *86400.d0

c Keep tabs on the number of photons that fell in gaps in the correction table.

         if(outside) noutside = noutside + 1

c Write to output file.

c#          CALL wrgti(luo,n,dtout,ivect,ctout,refout
c#      &             ,stacor,stocor,dzout,ierr)
         CALL ftpcld(luo,ivect(7),n,1,1,stacor,ftstat)
         CALL ftpcld(luo,ivect(8),n,1,1,stocor,ftstat)

c Store start and stop times to be added to the headers later.

c#          if(n.eq.1)     startout = stacor + (dioffset - 2400000.5d0)
c#          if(n.eq.nrows) stopout  = stocor + (dioffset - 2400000.5d0)
         if(n.eq.1)     startout = stacor
         if(n.eq.nrows) stopout  = stocor

      ENDDO

c Done with GTIs.  Announce warnings if any values were outside the 
c correction table extensions.

      IF(noutside.gt.0) THEN
         write(errm,100) noutside
100      format(i11,' GTI times arrived in gaps in the bc correction'
     &             ,'  table.')
         CALL ftmahd(luo,1,xtend,ftstat)
         CALL ftpcom(luo,errm,ftstat)
         CALL ftmahd(luo,iext(2),xtend,ftstat)
         CALL ftpcom(luo,errm,ftstat)
         CALL xwarn(errm,ichat)
      ENDIF

c Write timing keywords and comments to the EVENT extension.

      comm = 'Start time for this extension'
      CALL ftpkyd(luo,'TSTART',startout,16,comm,ftstat)
      comm = 'Stop time for this extension'
      CALL ftpkyd(luo,'TSTOP',stopout,16,comm,ftstat)
      comm = 'Units for header timing keywords'
      CALL ftpkys(luo,'TIMEUNIT',timeunit,comm,ftstat)

500   CONTINUE

c Write timing keywords and comments to the RATE TABLE extension.

      ftstat=0
      CALL ftmahd(luo,iext(1),xtend,ftstat)
      ftstat=0
      comm = 'Start time for this extension'
      CALL ftpkyd(luo,'TSTART',startout,16,comm,ftstat)
      ftstat=0
      comm = 'Stop time for this extension'
      CALL ftpkyd(luo,'TSTOP',stopout,16,comm,ftstat)
      ftstat=0
      comm = 'Units for header timing keywords'
      CALL ftpkys(luo,'TIMEUNIT',timeunit,comm,ftstat)

c -------------
c Closing part.
c -------------

c Trap errors.
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         write(errm,*) 'abcmake: Fitsio status ', ftstat
         GOTO 999
      ENDIF

      GOTO 1000
999   CONTINUE
c  if we get here a fatal error occurred
      CALL xaerror(errm,1)
c     close files
      CALL ftclos(lui,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         CALL xaerror('Closing fits files',1)
      ENDIF
      CALL ftclos(luo,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         CALL xaerror('Closing fits files',1)
      ENDIF
      CALL ftclos(lub,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         CALL xaerror('Closing fits files',1)
      ENDIF
      CALL ftclos(luc,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         CALL xaerror('Closing fits files',1)
      ENDIF

c Free logical units.
      CALL frelun(lui)
      CALL frelun(luo)
      CALL frelun(lub)
      CALL frelun(luc)
      return
      
      
1000  CONTINUE
c  if we get here, no errors encountered

      
C update the CHECKSUM and DATASUM keywords
      call ftthdu(luo,hdunum,ftstat)
      if (ftstat .ne. 0) then
          errm='Cannot get total number of HDUs for '//ou_filtmp
          call fcerr(errm)
          endif
      do n=1,hdunum
        call ftmahd(luo,n,xtend,ftstat)
        if (ftstat .ne. 0) then
          errm='Cannot move to extension '
          call fcerr(errm)
          endif
        call ftpcks(luo,ftstat)
        if (ftstat .ne. 0) then
          errm='Cannot update the CHECKSUM and DATASUM: '//ou_filtmp
          call fcerr(errm)
          endif
        enddo

c Close all files
 
c     ftstat = 0
      CALL ftclos(lui,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         CALL xaerror('Closing fits files',1)
      ENDIF
      CALL ftclos(luo,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         CALL xaerror('Closing fits files',1)
      ENDIF
      CALL ftclos(lub,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         CALL xaerror('Closing fits files',1)
      ENDIF
      CALL ftclos(luc,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         CALL xaerror('Closing fits files',1)
      ENDIF

c Free logical units.
      CALL frelun(lui)
      CALL frelun(luo)
      CALL frelun(lub)
      CALL frelun(luc)

C   Rename the temporary output file with the specified output file name
      call mvfile(ou_filtmp,ou_fil)

      return
      end

