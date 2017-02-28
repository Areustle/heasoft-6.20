
*+ROSCC2UTC
      subroutine rosccc

c -----------------------------------------------------------------------------
c this task calculates the utc in secs, yyyy-mm-dd. 
c -----------------------------------------------------------------------------

      implicit none
      character(100) subinfo
      character(180) sccfil
      character(2) day,month,hr,min
      double precision scctime, utcr, rosat_day
      double precision dmjd,hour,mint,sec
      real mjd
      integer utci
      integer iunit,status,errflg,chatter,block
      integer yy,mm,dd,ihr,imin


c --------------author/modifications -------------------------------
c Banashree Mitra seifert (Aug. 1996) 1.0.0:
c
c Banashree Mitra seifert (Oct 1996) 1.1.0:
c        . correction for calculation of MJD done
c           (it was JD -2400000, but should be 2400000.5)
c        . ROSAT day calculation is included 
c          Formula for ROSAT DAY was obtained from the subroutine
c          rosat/src/abc/rosday.f
c Peter D Wilson (1998 Jun 30) 1.1.1:
c        . Updated for new FCPARS behavior
c Ning Gan (1998 Jul 10) 1.1.2:
c        . Use the new date format for output.
c -----------------------------------------------------------------------

      character(10) taskname
      parameter (taskname='roscc2utc')
      character(5) version
      parameter (version='1.1.2')
*-
c ----------------------------------------------------------------------------
      errflg=0
      chatter=0
      call roscc2utc_gp(sccfil,scctime,chatter,errflg)

      if(errflg .ne. 0) then
         subinfo='returning from get parameters'
         call wterrm(taskname,version,subinfo)
         goto 100
      endif

      call wtbegm(taskname,version,chatter)

      status = 0
      call ftgiou(iunit,status)
      call ftopen(iunit,sccfil,0,block,status)
      if (status .ne. 0) then
          chatter=10
          subinfo='error opening file'//sccfil
          call wtinfo(chatter,10,1,subinfo)
          goto 100
      endif

      call scc2utc_conv(iunit,0,scctime,utci,utcr,errflg)
      if(errflg .ne. 0) then
         chatter=10
         subinfo='returning from scc2utc_conv'
         call wtinfo(chatter,10,1,subinfo)
         goto 100
      endif
      
      call ftclos(iunit,status)
      call ftfiou(iunit,status)

      mjd=real(utci)-2400000.5
      dmjd = mjd*1.d0+utcr
      rosat_day = (scctime +27882.4d0)/85119.5d0

      write(*,'(a,i8)')    ' Integer part of UTC =',utci
      write(*,'(a,f14.11)')' & Fractional part   =',utcr      
      write(*,'(a,f18.11)')' MJD                 =',dmjd
      write(*,'(a,f18.11)')' ROSAT day           =',rosat_day

c ---------------------------------------------------------------------
c if fractional part > 0.5 then it is the next calendar day
c so yy,mm.dd has to be calculated for the next JD, but hr,min,sec will
c be before noon on that day (this is just to calculate the day of the
c year correctly) 
c ---------------------------------------------------------------------
      if (utcr .gt. 0.5d0) then  
          utci=utci+1
          utcr=utcr-0.5d0 
      endif 

c ------------------------------------------------------------------------
c to calculate the calendar date according to JD
c input is the integer JD and output is the date when input JD started
c at noon on that date.
c ------------------------------------------------------------------------

      call jul_to_cal(utci,mm,dd,yy)

c ------------------------------------------------------------------------
c next step:
c month and day is being converted to character(2) for writing purpose
c e.g. if month=2 it will write 02 etc.
c same for day also
c ------------------------------------------------------------------------
      if (mm .lt. 10) then
          write(month,'(a1,i1)') '0',mm
      else
          write(month,'(i2)') mm
      endif

      if (dd .lt. 10) then
          write(day,'(a1,i1)') '0',dd
      else
          write(day,'(i2)') dd
      endif

      write(*,'(a,i4,4a)') ' yyyy-mm-dd = ',yy,'-',month,'-',day

c ------------------------------------------------------------------------
c now gor for hour,min,sec of the day
c ------------------------------------------------------------------------
      hour = utcr*24.0d0
      ihr   = int(sngl(hour))
      mint = (hour-ihr) * 60.d0
      imin  = int(sngl(mint))
      sec  = (mint-imin) * 60.d0

c ------------------------------------------------------------------------
c hour and minute is being converted to character(2) for writing purpose
c e.g. if hour =2 it will write as hr=02 etc.
c same for minute(min) also
c ------------------------------------------------------------------------
      if(ihr .lt. 10) then
         write(hr,'(a1,i1)') '0',ihr
      else
         write(hr,'(i2)') ihr
      endif
 
      if(imin .lt. 10) then
         write(min,'(a1,i1)') '0',imin
      else
         write(min,'(i2)') imin
      endif 

      write(*,'(5a,f14.11)') ' hh:mm:ss   = ',hr,':',min,':',sec
     
100   call wtendm(taskname,version,status,chatter)
      end

c ----------------------------------------------------------------------------
c               end of main (roscc2utc)
c ----------------------------------------------------------------------------

*+roscc2utc_gp

      subroutine roscc2utc_gp (sccfil,scctime,chatter,errflg)

c ------------------------------------------------------------------------------
c this routine gets parameter for the main
c -----------------------------------------------------------------------------
      implicit none
      character*(*) sccfil
      double precision scctime
      integer chatter,errflg

c ------------------ internals ---------------------------------------------

      character* 100 subinfo
      character(180) filename, ill_file(2)
      integer extnum,n_ill, status
      logical ext

      character(13) subname
      parameter (subname='rossc2utc_gp')
      character(5) version
      parameter (version='1.0.1')

c ------------------- authors/modifications ----------------------------------
c Banashree M Seifert (1996, August) 1.0.0:
c Peter D Wilson (1998 Jun 30) 1.0.1:
c       . Drop INQUIRE test. Replace fcpars with ftrtnm
c ----------------------------------------------------------------------------
c chatter parameter

      status = 0
      call uclgsi('chatter',chatter,status)
      if (status .ne. 0) then
          subinfo = 'getting chatter parameter'
          call wtwarm(subname,version,0,0,subinfo)
          subinfo='setting chatter=9'
          call wtwarm(subname,version,0,0,subinfo)
          chatter = 9
          status = 0
      endif

c scc file

      status = 0
      call uclgst('sccfil',sccfil,status)
      if (status .ne. 0) then
          subinfo = 'getting scc_to_utc.tfits !'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(sccfil)

      n_ill=0
C PDW 6/30/98: Drop INQUIRE. Replace fcpars with ftrtnm
C      call fcpars(sccfil, filename, extnum, status)
      call ftrtnm( sccfil, filename, status )
      call crmvlbk(filename)
      n_ill = n_ill + 1
      ill_file(n_ill) = filename
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename(1:2)  .eq.  '  ')) then
C          subinfo= 'sccfile does not exist!'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wterrm(subname,version,subinfo)
C          errflg = 1
C          return
C      endif

c scc time
      status = 0
      call uclgsd('scctime',scctime,status)
      if (status .ne. 0) then
          subinfo = 'getting scc time parameter' 
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif
  
      return
      end
c -------------------------------------------------------------------
c            end of roscc2utc_gp
c --------------------------------------------------------------------
 
*+SCC2UTC_CONV

      subroutine scc2utc_conv(luc,ichat,scc,utci,utcr,ierr)

c --------------------------------------------------------------------
c Copy of scc2utc from rosat/src/abc/scc2utc.f
c renamed diferrent for IRAF
c -------------------------------------------------------------------
      implicit none
      double precision scc, utcr
      integer luc, ichat, utci, ierr

c ------------------------------------------------------------------------
C @(#)sccut2.for	1.1 (MPE-Garching) 3/16/94 16:46:26
c Originally:
c#        SUBROUTINE SCCUT2(SCC,UTCI,UTCR,SCCTAB,IERR)
C
CC  ROSAT Conversion from Spacecraft Clock to UTC
C
c  Adapted to XANADU timing analysis by eal  NASA/Goddard, HSTX  April, 1994
c
c# Replaced or deleted parts of the original program are denoted by c#.
c
c# 
c  New parts added for XANADU and FITS are set between blank c# lines.
c# 
C************************ FFORM VERSION 3.0 ********** 31-JAN-90 09:20
C
CA  author : TMB              date: 20-FEB-1991 11:05 from SCCUTC
CA  update : TMB/FDM          date: 15-MAY-1991 11:40 bug corrected 
CA  update : TMB              date: 04-JUL-1991 17:12 TEST ONLY
CA  update : TMB              date: 16-JUL-1991 17:16 new calib. (HBO)
CA  update : TMB              date: 28-JAN-1992 10:09 table input (CfA)
CA  update : TMB              date: 17-MAR-1992 10:10 new reference times
CA  update : JPF              date: 28-JUL-1992 12:10 table from input
CA  update : TMB              date: 20-OCT-1992 12:10 new table (HBO)
C
CT  status: not tested
C
C   general description:
CG  This module, given in input a value of the spacecraft clock, computes
CG   the corresponding UTC, by using the interpolation made by HBO.
CG   The output is divided into integer part and fractional part
CG   to avoid loss of precision.
CG   The interpolation parameters are read in from a calibration table
C
C   call_var.          type I/O description
CP  SCC                 R8  I   input spacecraft clock
CP  UTCI                I4    O output UTC (integer part)
CP  UTCR                R8    O output UTC (fractional part)
CP  SCCTAB              C   I   name of SCC2JD parameter table
CP  IERR                I     O = 0 no error 
C
C   include_block_name          description
CI  RCOMMON:CGENL.CMN           general parameter common block
C
C   routines_called    type     description
CR
C
C   extensions/system calls     description
CX  
C
C***********************************************************************
C
C   variables   meaning
C
C     NCOMP         I*4      (parameter) maximum number of polynomials
C     PCOEFF        I*4      (parameter) maximum order of polynomial
C     UTCI          R*8       output UTC (integer part)
C     JD90          R*8      (parameter) Julian date of midnight
C                                        between Dec 31,89 - Jan 1,90
C     DAYSEC        R*8      (parameter) one day in second units
C     A(NCOMP,NCOEFF) 
C                   R*8       array with polynomial coefficients
C     UTCR          R*8       output UTC (fractional part)
C     SCC           R*8       input spacecraft clock
C     DREF          R*8       days since reference date
C     UTH           R*8       seconds since midnight Dec 31,89 -> Jan 1,90
C     SCCTAB        C*(*)     name of SCC2JD parameter table
C     FIRST         L*4       flag for first call
C

      integer pcount,cmax,hdutype,nulvj
      parameter (cmax = 100)
ccc      integer*4 tbcol(cmax), size
      integer tbcol(cmax), size
      character(16) ttype(cmax),tform(cmax),tunit(cmax),extname
      character(7) routine
      character(80) errm
      double precision nulvd
      logical anynul

      integer  ncomp, pcoeff
      parameter  (ncomp = 20 )
      parameter  (pcoeff = 10 )
      double precision jd90, daysec
      parameter    (jd90      = 2447892.5d+00 )
      parameter    (daysec    =   86400.0d+00 )

      double precision a(ncomp,pcoeff), startt(ncomp), endt(ncomp)
      double precision reftim(ncomp)
C----------------------------------------------------------------
C I AM ADDING AN ARRAY FOR THE TIME ADDED TO SCC WHICH WE WILL
C SUBTRACT OFF THE TIME TO PERFORM THE INTERPOLATION 7/28/92 JPF
C----------------------------------------------------------------
      double precision sccadd(ncomp)
C----------------------------------------------------------------
C VARIABLE TO HOLD THE TIME ADDED TO SCC 7/28/92 JPF
C----------------------------------------------------------------
      character(8)   rtname
      double precision timadd, dref, ref, uth
      integer  partyp(ncomp), ncoeff(ncomp)
      integer  ncol, n, i, j
      logical  first, done

      routine ='scc2utc'
      rtname = 'scc2utc'
      nulvj = 0
      nulvd =0.d0
      first=.true.
ccc      save

      ref =0.d0
      ierr=0
C-----------------------------------------------------------------
C     On first call, open calibration table and read in parameters
C-----------------------------------------------------------------
          if(first) then
             first=.false.
C------------------------------------------------------
C        Open the SCC--UTC calibration table
C-------------------------------------------
         call ftmahd(luc,2,hdutype,ierr)
 
C----------------------------------------------------
C        Get general infos about the correction table
C----------------------------------------------------
         if(hdutype.eq.2)then 
            call ftghbn(luc,cmax,n,ncol,ttype,tform,tunit,
     &                  extname,pcount,ierr)
         endif
         if(hdutype.eq.1) then 
            call ftghtb(luc,cmax,size,n,ncol,ttype,tbcol,tform,
     &                  tunit,extname,ierr)
         endif
 
C-------------------------------------------------------
C        Check if it's a valid scc--utc correction table
C-------------------------------------------------------
C           Read in coefficients
C-------------------------------
            do i=1,n
C--->          Start time of the Ith interval
               call ftgcvd(luc,1,i,1,1,nulvd,startt(i),anynul,ierr)

C--->          End  time of the Ith interval
               call ftgcvd(luc,2,i,1,1,nulvd,endt(i)  ,anynul,ierr)
 
C--->          Type of function of interpolation:
               call ftgcvj(luc,3,i,1,1,nulvj,partyp(i),anynul,ierr)
 
C--->          Number of coefficients for this interval
               call ftgcvj(luc,4,i,1,1,nulvj,ncoeff(i),anynul,ierr)
 
C--->          Actual coefficients
               do j=1,ncoeff(i)
                  call ftgcvd(luc,4+j,i,1,1,nulvd,a(i,j),anynul,ierr)
               enddo
C--->          Reference time
               call ftgcvd(luc,ncol-1,i,1,1,nulvd,reftim(i),anynul,ierr)
 
C THIS IS WHERE I'LL READ IN THE TIME ADDED TO SCC JPF
               call ftgcvd(luc,ncol  ,i,1,1,nulvd,sccadd(i),anynul,ierr)
 
            enddo
C----------------------------------
C        Close the correction table
C----------------------------------
         if(ierr.ne.0) then
            errm = 'scc2utc: reading from correction file'
            call xerror(errm,1)
            return
         endif
 
      endif
c Fatal error if SCC value is not covered in table.

      if(scc .gt. endt(n)) THEN
         errm = 'scc value too large for SCC-UTC correction table.'
         call xerror(errm,1)
         return
      endif
C-----------------------------------------------------------------
C     Compute time in seconds after midnight dec 31,89 -> jan 1,90
C-----------------------------------------------------------------
C-------------------------------------------------
C     Loop over the components to find current one
C-------------------------------------------------
      uth = 0.0d0
      done = .false.
      do i=1,n
         if((scc.ge.startt(i)) .and. (scc.le.endt(i)))  then
            done = .true.
C-----------------------------
C           Set reference time
C-----------------------------
            ref = reftim(i)
C-------------------
C ADDITIVE TIME HERE
C-------------------
            timadd = sccadd(i)
            do j=1,ncoeff(i)
C-----------------
C SUBTRACT IT HERE
C-----------------
               uth = uth + a(i,j)*((scc-timadd)**(j-1))
            enddo
         endif
      enddo
C-----------------------------------------------
C     If time is after last calibration point...
C       you're extrapolating...
C-----------------------------------------------
      if(.not.done) then
C--------------------------
C        Set reference time
C--------------------------
         ref = reftim(n)
C-------------------
C ADDITIVE TIME HERE
C-------------------
         timadd = sccadd(n)
         do j=1,ncoeff(n)
C-----------------
C SUBTRACT IT HERE
C-----------------
            uth = uth + a(n,j)*((scc-timadd)**(j-1))
         enddo
      endif
C-------------------------------------
C     Get days since the reference day
C-------------------------------------
      dref = uth / daysec
C---------------------
C     Get integer part
C---------------------
      utci =int(dref)
C------------------------
C     Get fractional part
C------------------------
      utcr = dref - utci
C----------------------------------------
C     Check if it's going to the next day
C----------------------------------------
      utcr = utcr + 0.5d0
      if(utcr.ge.1.0) then
         utcr = utcr - 1.0d0
         utci = utci + 1
      endif
C-----------------------------------------------------------
C     Add reference date  (JD)
C      WARNING! The 0.5 is lost while converting to integer,
C      but this is taken into account above (15-MAY-1991)
C-----------------------------------------------------------
      utci = ref + utci

      return
      end     

c -----------------------------------------------------------
c                    end of scc2utc_conv 
c ------------------------------------------------------------

*+JUL_TO_CAL

      subroutine jul_to_cal(julian,mm,dd,yy)
     
c ------------------------------------------------------------------------
c this subroutine calculates the calendar day,month year for a given JD
c JD in integer.  So it returns the day when the JD starts at noon
c It is taken from Numerical recipe page 16 
c ------------------------------------------------------------------------
      integer julian,mm,dd,yy

c --------------------- internals ----------------------------------
      integer igreg
      parameter (igreg=2299161) 
      integer ja,jalpha,jb,jc,jd,je

c ------------------- authors/modifications ------------------------
c Banashree Mitra Seifert (1.0.0, Aug 1996)
c
c ---------------------------------------------------------------------
      character(11) subname
      parameter (subname='jul_to_cal')
      character(5) version
      parameter (version='1.0.0')
*-
c ------------------------------------------------------------------
c cross-over to Gregorian calendar produces following correction or 
c else no correction
c -----------------------------------------------------------------
      if(julian .ge. igreg) then
         jalpha=int(((julian-1867216)-0.25)/36524.25)
         ja=julian+1+jalpha-int(0.25*jalpha)
      else
         ja=julian
      endif
       
      jb=ja+1524
      jc=int(6680.+((jb-2439870)-122.1)/365.25)
      jd=365*jc+int(0.25*jc)
      je=int(jb-jd)/30.6001 
      dd=jb-jd-int(30.6001*je)
      mm=je-1
      if(mm .gt. 12) mm=mm-12
      yy=jc-4715
      if (mm .gt. 2) yy=yy-1
      if (yy .le. 0) yy=yy-1
      
      return
      end
c --------------------------------------------------------------------------
c                     end of jul_to_cal
c --------------------------------------------------------------------------

