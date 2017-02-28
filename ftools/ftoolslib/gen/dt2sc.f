*+DT2SC
c     ----------------------------------------------------- 
      subroutine dt2sc(date,zerotime,sctime,chatter,errflg)
c     -----------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c     This routine converts a date string - dd/mm/yy to S/C clock
c time.
c -----------------------------------------------------------------
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      integer fcstln
      character*(*) date
      integer zerotime
      integer sctime,chatter,errflg
c
c --- VARIABLE DIRECTORY ------------------------------------------
c
c date      char   : date in dd/mm/yy form
c
c
c --- CALLED ROUTINES ---------------------------------------------
c 
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1995 Nov 7) 1.0.0;
c
      character(5) version
      parameter (version = '1.0.0')
      character(5) subname
      parameter (subname = 'dt2sc')
c ----------------------------------------------------------------- 
*-
c --- LOCAL VARIABLES ---
c
      character(70) subinfo
      integer ierr
      double precision mjd,diff

c --- USER INFO ---

      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,15,1,subinfo)

c --- PARSE DATE STRING AND CONVERT TO MJD ---

      call dt2mjd(date,.true.,mjd,errflg)
      IF (errflg.NE.0) THEN
        return
      ENDIF

c --- USE ZERO TIME TO CALCULATE S/C CLOCK TIME ---

      diff = mjd - zerotime
      sctime = diff * 86400
      IF (chatter.GE.30) THEN
         ierr = 0
         write(subinfo,'(a,a,a,i12)',IOSTAT=ierr)
     &   'Converted :',date(:MIN(fcstln(date),len(subinfo)-37)),
     &   'to s/c time :',sctime
         call wtinfo(chatter,30,2,subinfo)
      ENDIF 
      return
      end
c ---------------------------------------------------------------
c     END OF DT2SC
c ---------------------------------------------------------------  
