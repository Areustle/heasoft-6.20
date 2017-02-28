      SUBROUTINE EDICOM(CBUF, LBUF)
      CHARACTER CBUF*(*)
      INTEGER   LBUF
C---
C This command scans the %ED% string and executes any sub-commands
C found.
C
C James Peachey, HEASARC/GSFC/NASA, Raytheon STX, 25 March, 1998
C     1) Initialize ISAVE to 0, in case someone calls edicom('res', 3)
C        before calling edicom('sav', 3).
C     2) Check whether edicom has been turned on before proceeding
C        with the "assignment" option.
C---
      INCLUDE 'edicmn.inc'
      INTEGER   ICOMM(9)
      EQUIVALENCE (IUP,ICOMM(1))
C
      CHARACTER(64) CTOK
      CHARACTER COMS(9)
      INTEGER   I, ITMP, J, KP, LTOK
      INTEGER   IFIRST, ISAVE
      SAVE      IFIRST, ISAVE
      DATA COMS/'U','D','L','R','B','E','W','X','Z'/
      DATA IFIRST/1/
C---
      IF(IFIRST.NE.0) THEN
         IFIRST=0
         ICEDIT=0
         IUP   =  2
         IDOWN = 14
         IRIGHT=  6
         ILEFT =  4
         IBEG  =  8
         IEND  =  5
         IWRITE= 18
         IERASE= 21
         IEOF  = 26
         ISAVE = 0
         CALL FORTYP(IFTYPE)
      END IF
      KP=0
C
  100 CALL ALF(CBUF, LBUF, KP, CTOK, LTOK)
      IF(LTOK.LE.0) GOTO 900
      CALL UPC(CTOK)
      IF(CTOK(1:2).EQ.'OF') THEN
C %ED% OFf
         IF(ICEDIT.NE.0) THEN
            CALL TTRSET()
         END IF
         ICEDIT=0
      ELSE IF(CTOK(1:2).EQ.'ON') THEN
C %ED% ON
         IF(IFTYPE.NE.0) THEN
            IF(IFTYPE.EQ.-2) WRITE(*,*)
            CALL TTINIT()
            ICEDIT=1
         ELSE
            WRITE(*,101) 
     &        'EDICOM--Line editing has not been implemented.'
  101       FORMAT(1X,A)
         END IF
      ELSE IF(CTOK(1:3).EQ.'RES') THEN
C %ED% RES
         IF(ISAVE.NE.0) THEN
            CALL TTINIT()
            ICEDIT=1
         END IF
      ELSE IF(CTOK(1:3).EQ.'SAV') THEN
C %ED% SAV
         ISAVE=ICEDIT
         IF(ICEDIT.NE.0) CALL TTRSET()
         ICEDIT=0
      ELSE IF(CTOK(1:3).EQ.'VER') THEN
C %ED% VER
         WRITE(*,111)  'EDICOM--Version: 1998-Mar-25'
  111    FORMAT(1X,A)
      ELSE IF(CTOK(1:1).EQ.'?') THEN
C %ED% ?
         IF(ICEDIT.EQ.0) THEN
            WRITE(*,121) 'OFF'
  121       FORMAT(1X,A)
         ELSE
            WRITE(*,151) (COMS(J),CHAR(ICOMM(J)+64),J=1,9)
  151       FORMAT(1X,'ON ',20(1X,A,A))
         END IF
      ELSE IF(LTOK.EQ.2 .AND. ICEDIT.NE.0) THEN
C assignment
         IF(CTOK(2:2).LT.'A' .OR. CTOK(2:2).GT.'Z') GOTO 100
         ITMP=ICHAR(CTOK(2:2))-ICHAR('A')+1
         DO 180 I=1,9
            IF(CTOK(1:1).EQ.COMS(I)) THEN
               DO 170 J=1,9
                  IF(J.EQ.I) GOTO 170
                  IF(ICOMM(J).NE.ITMP) GOTO 170
                  WRITE(*,161) 'EDICOM--Error, ^',CTOK(2:2),
     :               ' was used in ',COMS(J),CTOK(2:2),'.'
  161             FORMAT(1X,6A)
                  GOTO 100
  170          CONTINUE
               ICOMM(I)=ITMP
               GOTO 100
            END IF
  180    CONTINUE
      END IF
      GOTO 100
C---
  900 CONTINUE
      RETURN
      END
