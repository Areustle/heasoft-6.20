      SUBROUTINE PLTCCT(Ctable, Ltable)
      CHARACTER Ctable*(*)
      INTEGER   Ltable
C---
C Change Color Table.
C From PGPLOT manual I = 0.30*R + 0.59*G + 0.11*B
C---
C Ctable  I
C Ltable  I
C---
      INTEGER  MXCOL
      PARAMETER (MXCOL=256)
      REAL     FPNUM
      INTEGER  LENACT
C
      CHARACTER cbuf*256, cnam*256, ctmp*256
      REAL      bint(MXCOL), gint(MXCOL), rint(MXCOL), rnci(MXCOL)
      REAL      bint1(2), gint1(2), rint1(2), rnci1(2)
      REAL      bint2(4), gint2(4), rint2(4), rnci2(4)
      REAL      bint3(7), gint3(7), rint3(7), rnci3(7)
      REAL      tmp
      INTEGER   i, ier, invert, ios, is, itab
      INTEGER   kp, lbuf, ltmp, lun, ncol
C
C Gray scale
      DATA rint1/0., 1./
      DATA gint1/0., 1./
      DATA bint1/0., 1./
      DATA rnci1/0., 1./
C
C Original Cambridge LUT
      DATA rint2/0., 0., 1., 1./
      DATA gint2/0., 1., 0., 1./
      DATA bint2/1., 0., 0., 1./
      DATA rnci2/0., .3333333, .6666666, 1.0/
C
C Color spectrum LUT
      DATA rint3/0., .30, .20, .05, .70, .90, 1./
      DATA gint3/0., .00, .20, .60, .70, .13, 1./
      DATA bint3/0., .30, .95, .05, .05, .13, 1./
      DATA rnci3/0., .167, .333, .500, .667, .833, 1./
C
   11 FORMAT(A)
C
C Check to see if color table should be inverted.
      IF ( Ctable(1:1).EQ.'-' ) THEN
         is = 2
         invert = 1
      ELSE
         is = 1
         invert = 0
      END IF
C
      ncol = 0
      itab = NINT(FPNUM(Ctable(is:),Ltable-is+1,ier))
      IF ( ier.EQ.0 ) THEN
         IF ( itab.EQ.1 ) THEN
            ncol = 2
            DO i=1,ncol
               rnci(i) = rnci1(i)
               rint(i) = rint1(i)
               gint(i) = gint1(i)
               bint(i) = bint1(i)
            END DO
         ELSE IF ( itab.EQ.2 ) THEN
            ncol = 4
            DO i=1,ncol
               rnci(i) = rnci2(i)
               rint(i) = rint2(i)
               gint(i) = gint2(i)
               bint(i) = bint2(i)
            END DO
         ELSE IF ( itab.EQ.3 ) THEN
            ncol = 7
            DO i=1,ncol
               rnci(i) = rnci3(i)
               rint(i) = rint3(i)
               gint(i) = gint3(i)
               bint(i) = bint3(i)
            END DO
         END IF
      ELSE
         CALL GETLUN(lun)
         cnam = Ctable(is:)
         CALL XTEND(cnam, 'ct')
         CALL OPENWR(lun,cnam,'OLD',' ',' ',0,1,ier)
         IF ( ier.NE.0 ) THEN
C Search user defined directory (if it exists)
            CALL TRLOG('MY_XCOMS',8,ctmp,ltmp)
            IF ( ltmp.GT.0 ) THEN
               ctmp(ltmp+1:) = cnam
               CALL OPENWR(lun,ctmp,'OLD',' ',' ',0,1,ier)
            END IF
            IF ( ier.NE.0 ) THEN
C Search system directory
               ctmp = cnam
               CALL PTEND('$XANADU','xanlib/xcoms',ctmp)
               CALL OPENWR(lun,ctmp,'OLD',' ',' ',0,1,ier)
               IF ( ier.NE.0 ) THEN
                  ltmp = LENACT(ctmp)
                  WRITE(*,*) 'Unable to open file=',ctmp(:ltmp)
                  GOTO 340
               END IF
            END IF
         END IF
  310    CONTINUE
            READ(lun,11,IOSTAT=ios) cbuf
            IF ( ios.NE.0 ) GOTO 340
            lbuf = LENACT(cbuf)
C Ignore blank lines, or comment linew
            IF ( lbuf.LE.0 .OR. cbuf(1:1).EQ.'!' ) GOTO 310
            ncol = ncol + 1
            kp = 0
            CALL ALF(cbuf, lbuf, kp, ctmp, ltmp)
            rnci(ncol) = FPNUM(ctmp, ltmp, ier)
            CALL ALF(cbuf, lbuf, kp, ctmp, ltmp)
            rint(ncol) = FPNUM(ctmp, ltmp, ier)
            CALL ALF(cbuf, lbuf, kp, ctmp, ltmp)
            gint(ncol) = FPNUM(ctmp, ltmp, ier)
            CALL ALF(cbuf, lbuf, kp, ctmp, ltmp)
            bint(ncol) = FPNUM(ctmp, ltmp, ier)
         IF ( ncol.LT.MXCOL ) GOTO 310
  340    CONTINUE
C         WRITE(*,*) 'Read',ncol,' colors.'
         CLOSE(UNIT=lun)
         CALL FRELUN(lun)
      END IF
C
      IF ( ncol.NE.0 ) THEN
C Got a valid color table, invert it if needed.
         IF ( invert.NE.0 ) THEN
            DO i=1,ncol/2
               tmp = rnci(i)
               rnci(i) = 1.0 - rnci(ncol+1-i)
               rnci(ncol+1-i) = 1.0 - tmp
               tmp = rint(i)
               rint(i) = rint(ncol+1-i)
               rint(ncol+1-i) = tmp
               tmp = gint(i)
               gint(i) = gint(ncol+1-i)
               gint(ncol+1-i) = tmp
               tmp = bint(i)
               bint(i) = bint(ncol+1-i)
               bint(ncol+1-i) = tmp
            END DO
         END IF
C Load table.
         CALL PGCTAB(rnci,rint,gint,bint,ncol,1.0,0.5)
      ELSE
C User gave an invalid color table, give him some help.
         WRITE(*,*) 'Builtin color tables are:'
         WRITE(*,*) ' 1 - Grayscale'
         WRITE(*,*) ' 2 - Black, Blue, Green, Red, White'
         WRITE(*,*) ' 3 - Black, Magenta, Blue, Yellow, Green, '//
     &         'Orange, Red, White'
      END IF
      RETURN
      END
