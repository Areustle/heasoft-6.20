      SUBROUTINE PLTCUR(WINLOC, BOXVP, XYSCAL, MXWIN, LOGX, LOGY,
     :   IACTW, Iwadj, IWNUM, VX, VY, WX, WY, CHR)
      INTEGER   MXWIN, LOGX(*), LOGY(*), IACTW(*), Iwadj(*)
      INTEGER   IWNUM
      REAL      BOXVP(4,*), WINLOC(4,*), XYSCAL(4,*)
      REAL      VX, VY, WX, WY
      CHARACTER CHR
C---
C Wrapup for PGCURSE.
C---
C IWNUM     O  The window containing the cursor position.
C VX,VY     O  The cursor position in NDC.
C WX,WY     O  The cursor position in window coordinates for IWNUM.
C---
      INTEGER   I
C---
      CALL PLTSVW(BOXVP, WINLOC, XYSCAL, LOGX, LOGY, Iwadj, 0)
      CALL PGCURSE( VX, VY, CHR)
      DO 120 I=1,MXWIN
         IF(IACTW(I).GT.0) THEN
            IF(WINLOC(1,I).LE.VX .AND. VX.LE.WINLOC(3,I) .AND.
     :         WINLOC(2,I).LE.VY .AND. VY.LE.WINLOC(4,I)) THEN
               IWNUM=I
               GOTO 130
            END IF
         END IF
  120 CONTINUE
      WX=VX
      WY=VY
      RETURN
C---
C The window has been identified, so convert to window coordinates.
  130 CONTINUE
      CALL PLTSVW(BOXVP, WINLOC, XYSCAL, LOGX, LOGY, Iwadj, IWNUM)
      CALL PLTVTW(VX, VY, WX, WY)
      IF(LOGX(IWNUM).NE.0 .AND. WX.LT.32.) THEN
         WX=10.**WX
      END IF
      IF(LOGY(IWNUM).NE.0 .AND. WY.LT.32.) THEN
         WY=10.**WY
      END IF
      RETURN
      END
