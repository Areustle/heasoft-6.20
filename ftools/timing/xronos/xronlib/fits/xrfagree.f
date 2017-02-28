      subroutine xrfagree(lui,ichat,iopt,extns,intype,tstart,tstop
     &                   ,dtsta, agreed,ftstat)

c XRonos Fits routine to check whether rate table and gti start and stop
c times AGREE. 

c If not, a warning is issued.

c If the routine is called with FITSIO in the EVENTS extension,
c it goes into the GTI extension and gets all the necessary
c information.  This is redundant, but necessary because xrfrdpon (the
c point reader) calls this routine and does not have any GTI information
c in it.

c If the routine is called with FITSIO in the GTI extension it goes into
c the rate table.

c On return, fitsio will be in the same extension as it was when the
c call was made.

c If iopt(8) = -99 -- meaning that the VC-99 file option was used --
c the GTI's get turned off.
c
c c To fix the start time of the series to be either the first gti or the
c first photon, need the gti start. It is too complicated modify other
c part of code. so in the point reader the new return dtsta is used
c for that poupose. (this changes also xrfrdpon and xrfrhea)
c
c  I  lui      (i)  Lu of input FITS file
c  I  ichat    (i)  input chattiness
c  I  ipot     (i)  input file options
c  I  extns    (i)  fits extension numbers for rates table (1) and GTIs (2)
c  I  intype   (i)  type of input extension 
c                   ( = 1 for EVENTS, 2 for RATE, 3 for PACKET = 4 for GTIs)
c  I  tstart   (d)  start time for input extension
c  I  tstop    (d)  stop time for input extension
c  O  dtsta    (d)  start time of the other extension (local)
c  O  agreed   (l)  = .false. if times do not agree
c  O  ftstat   (i)  FITSIO error status

c Author: eal  HSTX/GSFC HEASARC  June, 1994

      INTEGER cmax
      PARAMETER (cmax=100)
      LOGICAL agreed
      character(16) ttype(cmax),tform(cmax),tunit(cmax),extname
      INTEGER lui,ichat,ftstat,ivect(10),itype,extns(*),intype
     &   ,iopt(*),nrows,nfield,pcount,idum,idum1,idum2,idum3,idum4
      DOUBLE PRECISION ddum,ddum1,ddum2,ddum3,dtsta,dtsto,tstart,tstop
      data ivect /10*0/

      IF(ftstat .ne. 0) RETURN

c Default:

      agreed = .true.

c Make the test only for event lists and gtis.

      IF(intype.eq.4) THEN
         itype = 1
         CALL ftmahd(lui,extns(1),idum,ftstat)
      ELSEIF(intype.eq.1) THEN
         itype = 4
         CALL ftmahd(lui,extns(2),idum,ftstat)
      ELSE
         RETURN
      ENDIF

c Read essential keywords.

      CALL ftghbn(lui,cmax,nrows,nfield,ttype,tform,tunit
     &           ,extname,pcount,ftstat)

c Set vector columns using TTYPEnnn keywords.

      CALL xrftgcol(nfield,ttype,tunit,ivect)
      CALL xrcolopt(iopt,nfield,itype,ivect,ftstat)

c Get timing keywords.
      CALL xrftgtky(lui,iopt,idum,itype,ivect,idum1,idum2
     &             ,ddum,ddum1,ddum2,dtsta,dtsto,idum3,ddum3
     &             ,idum4,ftstat)

c Not concerned with errors in this routine -- they should have been flagged
c previously.

      ftstat = 0

c Test for agreement and return to the original extension.
      IF(intype.eq.4) THEN
         CALL ftmahd(lui,extns(2),idum,ftstat)
         IF((dtsta.lt.tstart).or.(dtsto.gt.tstop)) goto 999
      ELSEIF(intype.eq.1) THEN
         CALL ftmahd(lui,extns(1),idum,ftstat)
         IF((dtsta.gt.tstart).or.(dtsto.lt.tstop)) goto 999
      ENDIF

      RETURN

999   continue
      agreed = .false.
      CALL xwarn('GTI and Rate Table header times do not agree',ichat)
      IF(iopt(8).eq.-99) CALL xwarn(' -- GTIs will not be used.',ichat)

      RETURN
      END
