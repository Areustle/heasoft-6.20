      SUBROUTINE xfndxt(unit, extens, extns2, context, fterr)

      IMPLICIT NONE

      INTEGER fterr, unit
      CHARACTER extens*(*), extns2*(*), context*(*)

c Subroutine to go to the extension specified by EXTENS and EXTNS2. Works 
c by looking for either the EXTNAME or HDUCLAS1 and HDUCLAS2 keywords.

      INTEGER iext, index, extlen, extln2
      CHARACTER(72) string, comment
      CHARACTER(255) wrtstr

      INTEGER lenact
      EXTERNAL lenact

      fterr = 0
      extlen = lenact(extens)
      extln2 = lenact(extns2)

      iext = 0

 100  CONTINUE

c Go to the next extension

         iext = iext + 1
         CALL ftmahd(unit, iext, index, fterr)
         IF (fterr .NE. 0) THEN
            context = 
     & 'XFNDXT: Failed to find extension - check EXTNAME and HDUCLAS*'
            RETURN
         ENDIF

c First check for the EXTNAME variable. In the original versions of
c FITS files this was used to specify the extension type.

         CALL ftgkys(unit, 'EXTNAME', string, comment, fterr)

         IF ( fterr .EQ. 0 ) THEN

            IF ( extens(1:extlen) .EQ. 'SPECTRUM' ) THEN

               IF ( string(1:extlen) .EQ. extens(1:extlen) ) THEN
                  WRITE(wrtstr,'(a,a,i3)') ' Found ', 
     &                                      extens(1:extlen), iext
                  CALL xwrite(wrtstr, 25)
                  RETURN
               ENDIF

            ELSEIF ( extens(1:extlen) .EQ. 'RESPONSE' ) THEN

               IF ( string(1:extln2) .EQ. extns2(1:extln2) ) THEN
                  WRITE(wrtstr,'(a,a,a,i3)') ' Found ', 
     &                                      extens(1:extlen),
     &                                      extns2(1:extln2), iext
                  CALL xwrite(wrtstr, 25)
                  RETURN
               ENDIF

               IF ( (extns2(1:extln2) .EQ. 'RSP_MATRIX') .AND.
     &              ( (string(1:6) .EQ. 'MATRIX') .OR.
     &                (string(1:15) .EQ. 'SPECRESP MATRIX') ) ) THEN
                  WRITE(wrtstr,'(a,a,a,i3)') ' Found ', 
     &                                      extens(1:extlen),
     &                                      extns2(1:extln2), iext
                  CALL xwrite(wrtstr, 25)
                  RETURN
               ENDIF

            ENDIF

         ENDIF

c Now we can check for the HDUCLAS1 and HDUCLAS2 variables as passed
c through as arguments

         fterr = 0
         CALL ftgkys(unit, 'HDUCLAS1', string, comment, fterr)

         IF ( (fterr .EQ. 0) .AND. 
     &        (string(1:extlen) .EQ. extens(1:extlen)) ) THEN

            IF ( extln2 .EQ. 0 ) THEN

               WRITE(wrtstr,'(a,a,i3)') ' Found ', 
     &                                   extens(1:extlen), iext
               CALL xwrite(wrtstr, 25)
               RETURN

            ELSE

               CALL ftgkys(unit, 'HDUCLAS2', string, comment, fterr)
               IF ( (fterr .EQ. 0) .AND.
     &              (string(1:extln2) .EQ. extns2(1:extln2)) ) THEN
                  WRITE(wrtstr,'(a,a,a,i3)') ' Found ', 
     &                                      extens(1:extlen),
     &                                      extns2(1:extln2), iext
                  CALL xwrite(wrtstr, 25)
                  RETURN
               ENDIF

            ENDIF

         ENDIF

         fterr = 0

      GOTO 100

      END
