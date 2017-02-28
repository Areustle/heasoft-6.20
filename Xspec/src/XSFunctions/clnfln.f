
      FUNCTION clnfln(filenm)

      CHARACTER*(*) filenm, clnfln

c Function to chop any spectrum specification off the end of the
c filename so that the result can be used to open the file.

c  Arguments :
c     filenm    c*(*)        i: filename + spectrum specification
c     clnfln    c*(*)        r: filename

c  kaa  1/26/95.

      character(1) START, END
      PARAMETER (START='{', END='}')

      INTEGER istart, iend

      clnfln = ' '

      istart = INDEX(filenm, start)
      iend   = INDEX(filenm, end)

      IF ( istart .EQ. 0 ) THEN

         clnfln = filenm

      ELSE

         IF ( iend .EQ. len(filenm) ) THEN

            clnfln = filenm(:istart-1)

         ELSE

            clnfln = filenm(:istart-1)//filenm(iend+1:)

         ENDIF

      ENDIF

      RETURN
      END
