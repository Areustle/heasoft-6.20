c
      subroutine xrfrdhe( cfilin, iopt, mopt ,dopt, dtint, dtsta, 
     $     dtsto, nobins, ngtis, csuna, csuse, status)
      implicit none
c
c ReaD and type information from a XRonos Fits file HEader.
c
c   I  cfilin = xronos input file name
c   I  iopt = flag options
c   I  mopt = mathematical options
c   I  dopt = constants for math opt
c   O  dtint = bin integration time (if <0 indicates an arrival time file)
c   O  dtsta = start time of xronos qdp file (days)
c   O  dtsto = stop    "   "    "    "   "      "
C  I/O nobins = Running total of number of input bins/events
C  I/O ngtis = Running total of number of input gtis
C   O  csuna,csuse = summary info for plot file
C  I/O status = error flag
c
c File options relevant to this subr.
c      iopt(2) = n to start reading at row n
c      iopt(3) = n to stop reading at row n
c      iopt(5) = n to assign column n to X-axis (time)
c      iopt(6) = n to assign column n to Y-axis
c      iopt(8) = n to assign column n to dead time
c      iopt(9) = n to assign column n to Y-error
c      iopt(10) = n to use extension n as the rate table
c >>> Not displayed here but used by the FITS reader: <<<
c      iopt(1) = n to start loading y-vector at element n
c      iopt(4) = n to stop  loading y-vector at element n

C Local variables:
C     iext(1) -- the RATE/COUNT extension

c Subroutines called: ftopen, ftghpr, ftmahd, ftghbn, ftgkys, xrftsetv,
c                     xrapfopt, xrftgtm0, xrftgtim, xrftgdtm, ftgcvd,
c                     xrfwrshe, xrfwrlhe, ftclos, xrwrplust, xrwrplusl

c Author: Eric Lufkin, HEASARC/GSFC, August 1993
c Based on subroutines xrqpdrdhe.f and xrrbrdhe.f by L. Stella and L. Angelini.

c keywords(1) = MINCHAN
c keywords(5) = MAXCHAN
c keywords(9) = RA
c keywords(13) = DEC
c keywords(17) = EXTNAME
c keywords(18) = OBJECT
c keywords(19) = TELESCOP
c keywords(20) = INSTRUME
c keywords(21) = DETNAME
c keywords(22) = FILTER
c keywords(23) = CLOCKCOR
c keywords(24) = TIMESYS
c keywords(4) = TIMEUNIT
c keywords(16) = HDUCLAS3

      include '../include/io.inc'
      integer cmax,maxdim,status,nobins,ngtis
      parameter(cmax=100,maxdim=9)
      logical agreed
      character cfilin*160, csuna*40, csuse*15, filename*160
      character(16) ttype(cmax),tform(cmax),tunit(cmax),extname
      character(20) keywords(40)
      character(80) istring
      integer lut, lcd, lch, lul, lt, lui, ierx, iopt(*), itype
     &   ,mopt(*),ierr,ctunit,ivect(10),iext(3),tmax,emax
     &   ,npts(3),refflag,htunit,nrows,nfield,pcount,parse
     &   ,block,rwmode,hdutype,phalo,phahi,idum,lchat,ichat
     &   ,ftstat,naxis,naxes(maxdim),eaxis,taxis,crunit,i
      real edum1,edum2,edum3,rbuf(2)
      double precision dtint,dtsta,dtsto,dopt(*),ddum
     &   ,dtoffset,dtzero,gtizero,gtista,gtisto,dtp
      data block,rwmode /2880,0/
      DATA istring,parse /' ',0/
      parameter (subname = 'xrfrdhe:')

c Miscellaneous initializations.
      idum=0
      do i=1,3
         iext(i)=0
      enddo
      eaxis=0
      taxis=0
      do i=1,10
         ivect(i) = 0
      enddo
      dtoffset = 0.d0

c     Error flag.
      if (status.ne.0) return

c Read header information from xronos FITS file.

c     Open the file.
      filename = cfilin
C      CALL xropnfil(filename,cdum,ichat,lui,0,iext,itype,status)
       CALL getlun(lui)
       CALL ftopen(lui,filename,rwmode,block,status)
       if(status.ne.0) then
          errm='Couldn''t open file:'//filename
          errm = subname//' '//errm
          call xaerror(errm, 5)
          go to 900
       endif

       IF(iopt(10).gt.0) THEN
          iext(1) = iopt(10) + 1
       ELSE
          iext(1) = 0
       ENDIF
 
c Look for usable extensions in the FITS file.
       CALL xrftgext(lui,' ',2,iext,itype,idum,status)
 
c RT option will override [ext#] on filename.
c# 
c#       CALL xrextopt(lui,ichat,iopt(10),iext,status)
c# 
c#       IF(iext(1).le.0) THEN
c#          status = 1050
c#          errm = ' Failed to find a FITS rate table extension'
c#          goto 900
c#       ENDIF

c Move to the chosen Rate Table header.

      CALL ftmahd(lui,iext(1),hdutype,status)


c# c Get the file type.  Now done in xrftgext
c# 
c#       CALL xrftgtyp(lui,itype)

c Turn off irrelevant extensions.
 
      if(itype.ne.1.or.iopt(8).eq.-99) then         
         iext(2) = 0
         iext(3) = 0
      endif
c
c Set GTI extension with veN option
c
      if ( itype.eq.1 .and. iopt(8).gt.0 ) then
         iext(2) = iopt(8) + 1
      endif

c Read essential keywords.

      CALL ftghbn(lui,cmax,nrows,nfield,ttype,tform,tunit
     &           ,extname,pcount,status)


c Trap for no data.

      IF(nrows.eq.0) THEN
         status=102
         errm = 'No data in this FITS extension'
         errm = subname//' '//errm
         call xaerror(errm, 5)
         goto 900
      ENDIF

c Set vector columns using TTYPEnnn keywords.

      CALL xrftgcol(nfield,ttype,tunit,ivect)
      CALL xrcolopt(iopt,nfield,itype,ivect,status)

c Determine rate column format.

      CALL xrftgycf(lui,ivect(2),crunit,dtp,naxis,naxes,eaxis,taxis
     &             ,emax,tmax,status)
c#       CALL xrftgycf(lui,ivect(2),dtp,tmax,emax,status)

      nobins = nobins + nrows*tmax


c Get timing keywords.

      CALL xrftgtky(lui,iopt,nrows,itype,ivect,htunit,ctunit
     &             ,dtzero,dtoffset,dtint,dtsta,dtsto,npts(1),ddum
     &             ,refflag,status)

      if(dtp.le.0.d0) dtp = dtint

      IF(status.ne.0) THEN
         errm = ' Trouble reading header timing information.'
         errm = subname//' '//errm
         call xaerror(errm, 5)
         goto 900
      ENDIF

c Energy bounds.

      CALL xrphaopt(iopt,itype,emax,phalo,phahi,ivect,status)
c
      if(status.ne.0) then
         errm = 'Trouble reading energy bounds'
         errm = subname//' '//errm
         call xaerror(errm, 5)
         goto 900
      endif



c Get descriptive keywords.

      CALL xrftgdes(lui,keywords)

      if(status.ne.0) then
         errm = 'Trouble reading timing keywords'
         errm = subname//' '//errm
         call xaerror(errm, 5)
         goto 900
      endif


c Store summary in csuna and csuse.

      csuna = keywords(18)(1:16)
      csuna(21:30) = keywords(19)(1:10)
      csuna(31:40) = keywords(20)(1:10)
      csuse(1:4) = keywords(1)(1:4)
      csuse(5:8) = keywords(5)(1:4)
      csuse(9:15) = '      '

c Move to the new extension header.  GTI will be ignored if
c EXPOSURE is present.

      if(iext(3).gt.0) then
         CALL ftmahd(lui,iext(3),hdutype,status)
      elseif(iext(2).gt.0) then
         CALL ftmahd(lui,iext(2),hdutype,status)
      else
         go to 100
      endif

      if(status.ne.0) then
         errm = 'Couldn''t move to GTI extension'
         errm = subname//' '//errm
         call xaerror(errm, 5)
         goto 900
      endif


c Read essential keywords.

      CALL ftghbn(lui,cmax,nrows,nfield,ttype,tform,tunit
     &           ,extname,pcount,status)

      ngtis = ngtis+nrows

      if(status.ne.0) then
         errm = 'Couldn''t read essential keywords'
         errm = subname//' '//errm
         call xaerror(errm, 5)
         goto 900
      endif

 
c Set vector columns using TTYPEnnn keywords.

      CALL xrftgcol(nfield,ttype,tunit,ivect)

      if(status.ne.0) then
         status = 101
         errm = 'Couldn''t read column info'
         errm = subname//' '//errm
         call xaerror(errm, 5)
         goto 900
      endif
 
      if(iext(3).gt.0) then

c Part for EXPOSURE extension.

c >>> Need analogue to xrgetfin time checker here. <<<

      else

c Part for GTI extension.

c Get timing keywords.

         CALL xrftgtky(lui,iopt,nrows,4,ivect,htunit,ctunit
     &                ,gtizero,dtoffset,dtint,gtista,gtisto,npts(2)
     &                ,ddum,idum,status)

c Check agreement with rate table times.

cc         CALL xrfagree(lui,ichat,iopt,iext,4,gtista,gtisto,agreed,status)
cc         IF(.not.agreed) THEN
ccc            iext(2) = 0
cc         ELSE

cc         ENDIF
         if(gtista.gt.dtsta.and.iopt(2).eq.0) then
            write(errm,
     $           '('' Warning, start time: '',g17.10)')dtsta
            call xwrite(errm,15)
            write(errm,
     $           '('' Conflicts with GTI start time: '',g17.10)')gtista
            call xwrite(errm,15)
         endif
         if(gtisto.lt.dtsto.and.iopt(3).eq.0) then
            write(errm,
     $           '('' Warning, stop time: '',g17.10)')dtsto
            call xwrite(errm,15)
            write(errm,
     $           '('' Conflicts with GTI stop time: '',g17.10)')gtisto
            call xwrite(errm,15)
         endif
      endif


100   continue

c Apply shift options to start and stop times.
      edum1=1
      edum2=1
      edum3=1

      CALL xrapplopt(iopt,mopt,dopt,dtsta ,dtint,edum1,edum2,edum3)
      CALL xrapplopt(iopt,mopt,dopt,dtsto ,dtint,edum1,edum2,edum3)
      CALL xrapplopt(iopt,mopt,dopt,gtista,dtint,edum1,edum2,edum3)
      CALL xrapplopt(iopt,mopt,dopt,gtisto,dtint,edum1,edum2,edum3)

c Execution phase completed.

c Print information to the terminal and
c log file as per the chattiness parameters.

      CALL xrfwrshe(lui,itype,dtint,dtsta,dtsto,dtzero
     &     ,dtoffset,dtp,keywords,tmax,refflag,ivect,npts
     &     ,iext,gtizero,gtista,gtisto,status)
c
c the printing of the the header is in teh xrfwrshe now
c      CALL xrfwrlhe(lui)
c

c Return the packet delta-time for making newbins.
      IF(itype.eq.1.and.iext(2).le.0.and.iext(3).le.0) iopt(8)=-99
      IF(itype.eq.1.and.iopt(8).ne.-99) THEN
          IF(iopt(2).eq.0) dtsta = gtista
          IF(iopt(3).eq.0) dtsto = gtisto
      ENDIF
      dtint = dtp

      go to 1000

c Error trap.

900   continue

1000  continue

c Close the file.

      ftstat = 0
      CALL ftclos(lui,ftstat)
      CALL frelun(lui)

      return
      end

