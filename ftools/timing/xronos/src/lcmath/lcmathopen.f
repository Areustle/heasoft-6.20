      subroutine lcmathopen(cfile,lui,iext,nrows,ivect,iopt,dtp,dtint
     &            ,dtsta,dtsto,dtzero,dtoffset,ctunit,crunit,naxis
     &            ,naxes,eaxis,taxis,emax,ierr)

c OPEN and check input files for program LCMATH.

c This routine gets called in lcmathmake -- once for the input file and once
c for the background file.  Checks that can be made independently for
c each file are made here.  These include:

c   Checking to be sure the file contains binned rate or count data and that
c   all header and column timing information is complete.

c   Checking that the extension has at least one row of data.

c   Finding out which columns should be copied to output, dropping those
c   other than TIME, RATE or COUNT, and ERROR if the rate is given as
c   an energy-channel vector.

C   Columns may be given as options VX VY VS and VE in the cfile variable

c   Checking that user-specified maximum and minimum energy channels 
c   are consistent with the file.

c For more information see the following subroutines in ../lib.
c xropnfil, xrftgcol, xrftgycf, xrphaopt, and xrftgtky

c  I  cfile    (c)  Name of input file
c  O  lui      (i)  Lu of input file.
c  O  iext     (i)  Extension number for rate table (1); (2) irrel. here
c  O  nrows    (i)  Number of rows in each file
c  O  ivect    (i)  FITS column numbers for TIME, Y and SY in infile
c  O  iopt     (i)  xronos options array (used only for energy bounds here)
c  O  dtp      (d)  Delta-time in packets (seconds)
c  O  dtint    (d)  File integration times (seconds)
c  O  dtsta    (d)  Start time (days - TJD if possible)
c  O  dtsto    (d)  Stop time (days - TJD if possible)
c  O  dtzero   (d)  Zero-point offset for TIME columns (days)
c  O  dtoffset (d)  Internal time offsets (days)
c  O  ctunit   (d)  Units on TIME columns
c  O  emax     (i)  Number of energy channels
c  O  ierr     (i)  error status
c Author:  eal   February 1994, NASA/Goddard Space Flight Center

      IMPLICIT NONE

c      INCLUDE 'lcmathdef.inc'

      include '../../include/io.inc'
      integer kmax,cmax
      parameter (kmax=40,cmax=100)

      character*(*) cfile
      character(16) ttype(cmax),tform(cmax),tunit(cmax),extname
      character(20) keywords(kmax)
      character(80) cdum
      character(160) in_fil
      integer nrows,lui,ivect(*),iopt(*),xtend,ierr,block,ftstat,pcount
     &   ,iext(*),idum1,idum2,idum3,ctunit,crunit,naxis,naxes(*),eaxis
     &   ,taxis,emax,LENACT,itype,ichat,nfield,i
      DOUBLE PRECISION dtsta,dtsto,ddum1,dtint,dopt(15)
     &   ,dtp,dtzero,dtoffset
      external LENACT
      integer mopt(15)
      character(10) copt(15)
      data block,pcount /2880,0/
      DATA keywords /kmax*'                    '/
      parameter (subname = 'lcmathopen:')

      if(ierr.ne.0) return

      ftstat=0
c Open file, get extension number.

      call xrparseopt(cfile,in_fil,copt,iopt,mopt,dopt,ierr)
      if(ierr.ne.0) then
         errm = subname//' '//'Error parsing options from string: '
         call xaerror(errm, 5)
         errm = subname//' '//cfile
         call xaerror(errm, 5)
         goto 999
      endif


      cdum = ' '
      CALL xropnfil(in_fil,cdum,ichat,lui,0,iext,itype,ierr)
      IF(ierr.ne.0) RETURN

c Quit if data are not binned.

      IF(itype.le.1) then
         ierr = -1
         errm = ' Input file does not contain binned data'
         goto 999
      endif

c Move to RATE extension.
c      write(*,*)'iext(1)',iext(1)
      CALL ftmahd(lui,iext(1),xtend,ftstat)
c      write(*,*)'iext(1)',iext(1),xtend,ftstat

c Read essential keywords.

      CALL ftghbn(lui,cmax,nrows,nfield,ttype,tform,tunit
     &           ,extname,pcount,ftstat)

c      write(*,*)'ttype(*)', ttype(1),ttype(2),ttype(3),ttype(4)
c Trap for no data.

      IF(nrows.eq.0) THEN
         ierr = -1
         errm = 'No data in the selected input FITS extension'
         goto 999
      ENDIF

c Set vector columns using TTYPEnnn keywords.
 
      CALL xrftgcol(nfield,ttype,tunit,ivect)
      CALL xrcolopt(iopt,nfield,itype,ivect,ierr)
c      write(*,*)'ivect', ivect(1), ivect(2)

c Quit if there is no rate column.

      if(ivect(2).le.0) then
         ierr = -1
         errm = ' Cannot find a RATE or COUNT column in input'
         goto 999
      endif

c Determine rate column format.

      CALL xrftgycf(lui,ivect(2),crunit,dtp,naxis,naxes,eaxis,taxis
     &             ,emax,idum1,ierr)
      IF(ierr.ne.0) return

c Drop columns other than TIME RATE and ERROR if rates are vectors.

      IF(emax.gt.1) THEN
         DO i = 4, 10
            ivect(i) = 0
         ENDDO
      ENDIF

c Check channel options.

      CALL xrphaopt(iopt,2,emax,idum1,idum2,ivect,ierr)
      ierr = 0

c Get timing keywords.
 
      CALL xrftgtky(lui,iopt,nrows,itype,ivect,idum3,ctunit
     &             ,dtzero,dtoffset,dtint,dtsta,dtsto,idum1
     &             ,ddum1,idum2,ierr)
 
      IF(ierr.ne.0) THEN
         errm = ' reading header timing information in input.'
         goto 999
      ENDIF

      goto 1000
999   continue
      errm = subname//' '//errm
      CALL xaerror(errm,1)
1000  continue

      return
      end
