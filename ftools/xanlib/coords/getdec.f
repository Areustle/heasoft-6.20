      SUBROUTINE GETDEC(String,Equinox,Decdeg,Ierr)
      implicit none
c
c  get the dec from the string and return it in decimal degrees
c 			nick 19.6.92
c  
c  James Peachey, HEASARC/GSFC/NASA, 14 April 1999
c  Makeover of get/parse ra/dec. Do not call chdec to convert
c  the current value unless it is actually needed (inside the
c  prompt loop.) Fixed to loop back to prompt correctly if/after
c  help message is printed. Use lenact to determine whether a
c  string is empty. Only call parsedec if a non-blank string was
c  entered by the user. Removed block of vestigial useless code
c  near end of file.
c
      CHARACTER*(*) String
      character(80) prompt , zwrite
      character(1) sign
      INTEGER*4 Ierr , iddt , idmt , Equinox
      REAL*8 Decdeg
      REAL*4 dsect
      integer lenact
c
c if string is blank then prompt for it
c
      if(lenact(string) .eq. 0) then
c
c get the default values
c
         CALL CHDEC(Decdeg,iddt,idmt,dsect,1)
         IF ( iddt.GE.0 .AND. idmt.GE.0 .AND. dsect.GE.0.0 ) THEN
            sign = ' '
         ELSE
            sign = '-'
            iddt = ABS(iddt)
            idmt = ABS(idmt)
            dsect = ABS(dsect)
         ENDIF
         IF ( sign.EQ.'-' ) THEN
            WRITE (prompt,99001) Equinox , sign , iddt , idmt , 
     &           dsect , decdeg
         ELSE
            WRITE (prompt,99002) Equinox , iddt , idmt , dsect , decdeg
         ENDIF

 100  continue
         CALL XCREAD(prompt,String,Ierr)
         IF ( Ierr.NE.0 ) RETURN
         CALL RMVLBK(String)
         IF ( String(1:1).EQ.'?' .OR. String(1:1).EQ.'h' .OR. 
     &        String(1:1).EQ.'H' ) THEN
            zwrite = ' Enter Dec in dg mn sec OR decimal degrees'
            CALL XWRITE(zwrite,5)
            GOTO 100
         ENDIF
      ENDIF

c
c parse the input, unless the input was blank (accept default)
c
      if(lenact(string) .ne. 0) then
         call parsedec(String,Equinox,Decdeg,Ierr)
      endif
c
      RETURN

99001 FORMAT ('Dec  (',I4,' d/f=',A,I2.2,I3.2,F6.2,' or ',f7.3,'): ')
99002 FORMAT ('Dec  (',I4,' d/f=',I3.2,I3,F6.2,' or ',f7.3,'): ')
      END
