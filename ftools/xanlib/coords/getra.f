      SUBROUTINE GETRA(string,equinox,radeg,ierr)
      implicit none
c
c  get the ra from the string and return it as radeg in decimal degrees
c  radeg is the default value at input
c 			nick 19.6.92
c
c  James Peachey, HEASARC/GSFC/NASA, 14 April 1999
c  Makeover of get/parse ra/dec. Do not call chra to convert
c  the current value unless it is actually needed (inside the
c  prompt loop.) Use lenact to determine whether a string is empty.
c  Fixed a potential infinite loop which would occur if input
c  were truncated. Only call parsera if a non-blank string was
c  entered by the user.
c
      CHARACTER*(*) string
      character(80) prompt, zwrite
      INTEGER*4 ira, imn, ierr, equinox
      REAL*8 Radeg
      REAL*4 sec
      logical*4 isitdg
      integer lenact
c
c if string is blank then prompt for it
c
      if(lenact(string) .eq. 0) then
c
c first get the default hr mn sec value from radeg
c
         CALL CHRA(Radeg,ira,imn,sec,1)
 100     WRITE (prompt,99001) Equinox, ira, imn, sec, radeg
99001 FORMAT ('R.A. (',I4,' d/f=',I3.2,I3.2,F6.2,' or ',f7.3,'): ')
         CALL XCREAD(prompt,string,ierr)
         IF ( ierr.NE.0 ) RETURN
         call rmvlbk(string)
         if(string(1:1).eq.'?'.or.string(1:1).eq.'h'.or.
     &     string(1:1).eq.'H')then
          zwrite=' Enter RA in hr mn sec OR decimal degrees'
          call xwrite(zwrite,5)
          go to 100
         endif
       ENDIF

c
c parse the inputs
c
      if(lenact(string) .ne. 0) then
         call parsera(string, equinox, radeg, ierr)
      endif
c
c
      RETURN
      END
