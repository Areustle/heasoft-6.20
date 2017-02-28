      SUBROUTINE fndext(unit, extens, extns2, context, fterr)

      INTEGER fterr, unit
      CHARACTER extens*(*), extns2*(*), context*(*)

c Subroutine to go to the extension specified by EXTENS and EXTNS2. Works 
c by looking for either the EXTNAME or HDUCLAS1 and HDUCLAS2 keywords.

      INTEGER iext, index, extlen, extln2
      CHARACTER string*72, comment*72

      INTEGER clenact
      EXTERNAL clenact

      fterr = 0
      extlen = clenact(extens)
      extln2 = clenact(extns2)

      iext = 0

 100  CONTINUE

c Go to the next extension

         iext = iext + 1
         CALL ftmahd(unit, iext, index, fterr)
         IF (fterr .NE. 0) THEN
            context = 
     & 'FNDEXT: Failed to find extension - check EXTNAME and HDUCLAS*'
            RETURN
         ENDIF

c First check for the EXTNAME variable. In the original versions of
c FITS files this was used to specify the extension type.

         CALL ftgkys(unit, 'EXTNAME', string, comment, fterr)

         IF ( fterr .EQ. 0 ) THEN

            IF ( extens(1:extlen) .EQ. 'SPECTRUM' ) THEN

               IF ( string(1:extlen) .EQ. extens(1:extlen) ) THEN
                  RETURN
               ENDIF

            ELSEIF ( extens(1:extlen) .EQ. 'RESPONSE' ) THEN

               IF ( string(1:extln2) .EQ. extns2(1:extln2) ) THEN
                  RETURN
               ENDIF

               IF ( (extns2(1:extln2) .EQ. 'RSP_MATRIX') .AND.
     &              ( (string(1:6) .EQ. 'MATRIX') .OR.
     &                (string(1:15) .EQ. 'SPECRESP MATRIX') ) ) THEN
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

               RETURN

            ELSE

               CALL ftgkys(unit, 'HDUCLAS2', string, comment, fterr)
               IF ( (fterr .EQ. 0) .AND.
     &              (string(1:extln2) .EQ. extns2(1:extln2)) ) THEN
                  RETURN
               ENDIF

            ENDIF

         ENDIF

         fterr = 0

      GOTO 100

      END
