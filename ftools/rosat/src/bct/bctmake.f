C
      subroutine bctmake(in_fil,ou_fil,rastr,decstr,eph_fil,ierr)

C  MAKE a Barycenter Correction Table from orbit data.

c  FOR ROSAT ONLY

c This routine does the following:

c 1)  Opens an input file and tries to find out what the file format is.
c     Three formats can be recognized:
c     Old-type US -- checks for EXTNAME = 'SPORB'
c     New-type US -- checks for EXTNAME = 'EPHEM'
c     German      -- no EXTNAME keyword -- checks column names
c     Any number of extensions can be processed up to 1000 (currently).
c     The extensions are then listed on screen and log.

c 2)  Tries to decode the RA and DEC strings given by the user.  If
c     Unsuccessful it exits with an error.

c 3)  Opens the output file.

c 4)  Opens the ephemeris file.  The full ephemeris file name is reconstructed
c     from a system-independent pathname given in bctdef.inc.

c 5)  Writes RA and DEC keywords to the primary header in the output file.

c 6)  Loops though each row of each extension in the orbit file, writing
c     the corresponding barycenter time delay in the output file.  The
c     overall structure of the input orbit file is preserved in the
c     output file.  The columns are arranged in four column format:
c     1 - UNCORRECTED TIME - INTEGER PART
c     2 - UNCORRECTED TIME - FRACTIONAL PART 
c     3 - CORRECTED TIME - INTEGER PART
c     4 - CORRECTED TIME - FRACTIONAL PART

c     For each extension in the output file the routine writes TSTART,
c     TSTOP, RA and DEC keywords.

c  I  in_fil    (c)  Name of input file
c  I  ou_fil    (c)  name of output file
c  I  rastr     (c)  R.A. string
c  I  decstr    (c)  Declination string
c  I  eph_fil   (c)  name of JPL 2000 ephemeris file
c  O  ierr      (i)  error status

c Author:  eal   December 1993, NASA/Goddard Space Flight Center
c
c modification history:
c  11/18/94 eag merged in LA's changes to FTOOLified version
c

      IMPLICIT NONE

c      INCLUDE 'bctdef.inc'

      integer emax, imax
      parameter (emax = 1000, imax = 10)
      LOGICAL*4 simple,extend
      character(80) comm,errm
      character(160) filename
      character(16) cbuf
      CHARACTER*(*) ou_fil,in_fil,rastr,decstr,eph_fil
      INTEGER*4 jdcor,jd,iext(emax),lui,luo,m,nrows,icols(imax)
     &   ,jdstore,ftstat,frowin,frowout,ierr,block,ifound,i
     &   ,equinox,itype,lue,xtend,pcount,bitpix,gcount,naxes(9),naxis
      integer idummy
      DOUBLE PRECISION frcnew,frc,reae(3),alpha,delta,frcstore,pi
      data simple,bitpix,naxis,extend,gcount /.true., 8, 0, .true., 1/

      if(ierr.ne.0) return
      ftstat = 0
      pcount = 0
      gcount = 0

c-------------------------------------------------------------
c Set-up Part
c-------------------------------------------------------------

      pi = 2.d0*dasin(1.d0)

c Open the input file: rwmode=0 means read only.

      CALL getlun(lui)
      call fcpars (in_fil, filename, idummy, ftstat)
      CALL ftopen(lui,filename,0,block,ftstat)
      if(ftstat.ne.0) then
         errm = ' error opening input file ' // in_fil
         ierr = ftstat
         GOTO 999
      endif

c Do the automatic extension search.  (The itype returned by xrftgext
c will be ignored.)
c Extname = SPORB is for old-type rosat files    - itype = 11
c Extname = EPHEM is for reformatted rosat files - itype = 12
c Old-type German files have no EXTNAME keyword  - itype = 13

c Also set column numbers.

         icols(1) = 1
         icols(2) = 2
         ifound = 0
         itype = 0
         DO i=1,emax
           iext(i)=0
         ENDDO
      CALL xrftgext(lui,0,'SPORB',emax,iext,itype,ifound,ierr)

      IF(ifound.gt.0) THEN
c Old-type U.S. file.
         itype = 11
         icols(3) = 3
         icols(4) = 11
         icols(5) = 12
         icols(6) = 13
      ELSE
         DO i=1,emax
           iext(i)=0
         ENDDO
         CALL xrftgext(lui,0,'EPHEM',emax,iext,itype,ifound,ierr)
         if(ifound.le.0)ierr=0
         IF(ifound.gt.0) THEN
c New-type U.S. file.
            itype = 12
            icols(3) = 0
            icols(4) = 9
            icols(5) = 10
            icols(6) = 11
         ELSE
            CALL ftmahd(lui,2,xtend,ftstat)
            CALL ftgkys(lui,'EXTNAME',cbuf,comm,ftstat)
            IF(ftstat.eq.202) THEN
               ftstat = 0
               CALL ftgkys(lui,'ORIGIN',cbuf,comm,ftstat)
               CALL ftgcno(lui,.true.,'DATE'      ,icols(1),ftstat)
               CALL ftgcno(lui,.true.,'DAYSEC'    ,icols(2),ftstat)
               CALL ftgcno(lui,.true.,'XSATELLITE',icols(4),ftstat)
               CALL ftgcno(lui,.true.,'YSATELLITE',icols(5),ftstat)
               CALL ftgcno(lui,.true.,'ZSATELLITE',icols(6),ftstat)
               IF((cbuf.eq.'ESO-MIDAS').and.(icols(1).eq.1 ).and.
     &                 (icols(2).eq.2 ).and.(icols(4).eq.9 ).and.
     &                 (icols(5).eq.10).and.(icols(6).eq.11).and.
     &                 (ftstat.eq.0)) THEN
c German file.
                  iext(1) = 2
                  ifound = 1
                  icols(3) = 0
                  itype = 13
               ELSE
c Unknown file type - exit with error.
                  errm = 'No orbit extensions found.'
                  ierr = -1
                  GOTO 999
               ENDIF
            ENDIF
         ENDIF
      ENDIF

c List extensions on the screen.
      CALL xrftwext(lui,5,ifound,iext,ierr)

c# c List columns on the screen (for first orbit extension)
c# 
c#       CALL xrftwcol(lui,5,icols,5,iext(1))

c Decode RA and DEC strings.
      CALL parsera(rastr,equinox,alpha,ierr)
      CALL parsedec(decstr,equinox,delta,ierr)
      errm = 'Error decoding RA/DEC in parsera/dec'
      IF(ierr.gt.0) GOTO 999

c Open the output file.

      CALL getlun(luo)
      CALL xrftinit(luo,ou_fil,ierr)
      if(ierr.ne.0) then
         errm = ' error opening output file ' // ou_fil
         GOTO 999
      endif

c Null primary in outfile.

      CALL ftphpr(luo,simple,bitpix,naxis,naxes,pcount,gcount
     &           ,extend,ftstat)
      CALL ftpdef(luo,bitpix,naxis,naxes,pcount,gcount,ftstat)

c Construct full ephemeris filename.

c      CALL ptend(disk,directory,eph_fil)

c Open Ephemeris file.

      CALL getlun(lue)
      CALL ftopen(lue,eph_fil,0,block,ierr)
      CALL ftmahd(lue,2,xtend,ierr)
      IF(ierr.ne.0) THEN
         errm = 'Opening ephemeris file'
         GOTO 999
      ENDIF

c Write RA and DEC keywords to output primary header.

      comm = 'Source RA in degrees'
      CALL ftpkyd(luo,'RA_SRC' ,alpha,12,comm,ftstat)
      comm = 'Source DEC in degrees'
      CALL ftpkyd(luo,'DEC_SRC',delta,12,comm,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         write(errm,*) 'bctmake: fitsio error ',ftstat
         GOTO 999
      ENDIF

c Convert RA and DEC to radians.

      alpha = alpha * pi /180.d0
      delta = delta * pi /180.d0

c----------------------------------------------------------
c Part for making barycenter table
c----------------------------------------------------------

      jdstore = 0
      frcstore = 0.d0

c Loop over extensions.

      DO m = 1, ifound

         frowin = 0
         frowout = 0
         nrows = 1

c Loop over rows.  The true value of nrows will be returned from bcread.

         DO WHILE (frowout.lt.nrows)

            frowin = frowin + 1

c Raed in time and orbit position.

            CALL bcread(lui,itype,iext(m),icols,frowin,nrows,jd,frc
     &                 ,reae,ierr)

c Crunch the numbers using stripped MIDAS routines.

            CALL barycen(lue,.true.,alpha,delta,jd,frc,reae,jdcor
     &                  ,frcnew,ierr)

c Ensure time-orderedness in the file.

            IF( ((jd.eq.jdstore).and.(frc.gt.frcstore)).or.
     &           (jd.gt.jdstore)                           ) THEN
               frowout = frowout + 1
               jdstore = jd
               frcstore = frc

c Write out uncorrected and corrected time.

               CALL bcwrite(luo,frowout,nrows,jd,frc,jdcor,frcnew,ierr)
            
            ELSE
               nrows = nrows - 1

c If this point is reached on the last row of the extension, redefine
c the limits of the extension.

               IF(frowout.eq.nrows) THEN
                  comm = 'Stop time'
                  CALL ftpkyj(luo,'TSTOPI' ,jdstore ,   comm,ftstat)
                  CALL ftpkyd(luo,'TSTOPF' ,frcstore,16,comm,ftstat)
                  CALL ftmkyj(luo,'NAXIS2' ,nrows,'&',ftstat)
                  comm = 'Source RA in degrees'
                  CALL ftpkyd(luo,'RA_SRC' ,alpha,12,comm,ftstat)
                  comm = 'Source DEC in degrees'
                  CALL ftpkyd(luo,'DEC_SRC',delta,12,comm,ftstat)
                  IF(ftstat.ne.0) THEN
                     ierr = ftstat
                     write(errm,*) 'bcmake: fitsio error ',ftstat
                     GOTO 999
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDDO

      GOTO 1000
999   CONTINUE
      CALL xaerror(errm,1)
1000  CONTINUE
 
c Close all files
 
      ftstat = 0
      CALL ftclos(lui,ftstat)
      CALL ftclos(luo,ftstat)
      IF(ftstat.ne.0) THEN
         ierr = ftstat
         CALL xaerror('Closing fits files',1)
      ENDIF
 
c Free logical units.
 
      CALL frelun(lui)
      CALL frelun(luo)
 
      RETURN
      END

