      SUBROUTINE PARSERA(String,Equinox,dbldeg,Ierr)
      implicit none
c
c  get the ra from the string and return it in decimal degrees
c 			nick 19.6.92
c
c  James Peachey, HEASARC/GSFC/NASA, 14 April 1999
c  Makeover of get/parse ra/dec. Do not save final value until
c  and unless the end of the routine is reached without an error.
c  Because of this, there is no need to call chra to convert
c  the current value. Return with ierr set, but no message, if
c  given a blank string. Corrected improper bounds checking
c  and added final check on computed value. Use lenact to
c  determine whether a string is empty. Rewrote without using
c  ABS to avoid round-off error which was being introduced.
c
      CHARACTER*(*) String
      character(80) zwrite
      INTEGER*4 Ierr , ihr , imin , Equinox
      REAL*8 dbldeg , tdbldeg
      REAL*4 rsec
      LOGICAL*4 ISITDG
      integer lenact
c
c check the input string; fail silently for a blank string.
c
      if(lenact(string) .eq. 0) then
         ierr = 100
         return
      endif

c
c simply read string if in decimal degrees
c
      if (isitdg(String)) THEN
         READ (String,*,IOSTAT=Ierr) tdbldeg
         zwrite = ' RA format error, use decimal degrees or hr mn sec'
      else
c
c must be dms....
c
            CALL XRDANG(String,ihr,imin,rsec)
c
         if(ihr.lt.0 .or. ihr.gt.23) then
            WRITE (zwrite,*)
     &         ' RA hr outside [0, 23]: invalid coordinates'
            ierr = 100
         elseif(imin.lt.0 .or. imin.gt.59) then
            WRITE (zwrite,*)
     &         ' RA min outside [0, 59]: invalid coordinates'
            ierr = 100
         elseif(rsec.lt.0. .or. rsec.ge.60.) then
            WRITE (zwrite,*)
     &         ' RA sec outside [0, 60.0): invalid coordinates'
            ierr = 100
         else
            CALL CHRA(tdbldeg,ihr,imin,rsec,0)
         ENDIF
      ENDIF

c
c if nothing else went wrong yet, perform a final check for
c a computed value out-of-range
c
      if(ierr.eq.0 .and. (tdbldeg.lt.0. .or. tdbldeg.ge.360.)) then
         write(zwrite,*) ' RA decimal value outside '//
     &      '[0.0, 360.0]: invalid coordinates'
         ierr = 100
      endif
c
c set output = temp value calculated unless an error occurred
c
      if (ierr .eq. 0) then
         dbldeg = tdbldeg
      else
         call xwrite(zwrite, 5)
         string = ' '
      endif
c
      RETURN
      END
