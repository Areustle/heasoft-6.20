C*GRLEN -- inquire plotted length of character string
C+
      SUBROUTINE GRLEN (STRING, D)
C
C GRPCKG: length of text string (absolute units)
C--
C (3-Mar-1983)
C 19-Jan-1988 - remove unused label [TJP].
C  9-Sep-1989 - standardize [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL UNUSED
      INTEGER XYGRID(300)
      INTEGER LIST(256)
      CHARACTER*(*) STRING
      REAL FACTOR, DX, D, RATIO, FNTFAC, GRPSLF
      INTEGER I, IFNTLV, LX, NLIST
      INTRINSIC ABS, LEN
      CHARACTER DEVTYP*14
      LOGICAL DEVINT, PSCRIPT
C
      D = 0.0
      IF (LEN(STRING).LE.0) RETURN
C-----------------------------------------------------------------------
C               Compute scaling and orientation
C-----------------------------------------------------------------------
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRPXPI(GRCIDE)/GRPYPI(GRCIDE)
      FNTFAC = 1.0
      IFNTLV = 0
C
C If the driver is PS then need to correct for the different length of
C of PS text output. This is slightly complicated because some PS characters
C scale differently from others
C
      CALL GRQTYP (DEVTYP, DEVINT)
      PSCRIPT = (DEVTYP.EQ.'PS').OR.(DEVTYP.EQ.'VPS').OR.
     1    (DEVTYP.EQ.'CPS').OR.(DEVTYP.EQ.'VCPS')
C
C               Convert string to symbol numbers:
C               \u and \d escape sequences are converted to -1,-2
C
      CALL GRSYDS(LIST,NLIST,STRING,GRCFNT(GRCIDE))
C
C               Plot the string of characters
C
      DO 380 I = 1,NLIST

          IF (LIST(I).LT.0) THEN
              IF (LIST(I).EQ.-1) THEN
                  IFNTLV = IFNTLV+1
                  FNTFAC = 0.6**ABS(IFNTLV)
              ELSE IF (LIST(I).EQ.-2) THEN
                  IFNTLV = IFNTLV-1
                  FNTFAC = 0.6**ABS(IFNTLV)
              END IF
              GOTO 380
          END IF
          CALL GRSYXD(LIST(I),XYGRID,UNUSED)
          LX = XYGRID(5)-XYGRID(4)
          DX = FACTOR*LX*RATIO

          IF ( PSCRIPT ) DX = DX * GRPSLF(LIST(I))

          D = D + DX*FNTFAC

  380 CONTINUE

      END
