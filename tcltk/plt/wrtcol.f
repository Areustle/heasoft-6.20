      SUBROUTINE WRTCOL(Lun, Icol, Ngroup)
      INTEGER   Lun, Icol(*), Ngroup
C---
C Generate the COLOR command strings for the PLT WHead command.
C---
C Lun     I    Output logical unit number
C Icol    I    Which color to use for each group
C Ngroup  I    Number of groups to check in Icol list.
C---
C 1993-Apr-15 - New routine [AFT]
C---
      CHARACTER ctmp*80
      INTEGER   ig, iend, istart, ltmp
C---
   11 FORMAT(A)
C---
      ctmp = 'COL  OFF '
      ltmp = 9
      istart = 0
      iend = 0
      DO 190 ig=1, Ngroup
         IF(Icol(ig).LE.0 ) THEN
C Current group is colored off, add to list.
            IF ( istart.LE.0 ) THEN
               istart = ig
            ELSE
               iend = ig
            END IF
         END IF
         IF(Icol(ig).GT.0 .OR. ig.EQ.Ngroup ) THEN
C This group is colored on, write out colored off list, if any.
            IF ( istart.GT.0 ) THEN
               CALL CRAMI(istart, ctmp, ltmp)
               IF ( iend.GT.0 ) THEN
                  ctmp(ltmp+1:ltmp+2) = '..'
                  ltmp = ltmp + 2
                  CALL CRAMI(iend, ctmp, ltmp)
               END IF
               IF ( ltmp.GE.72 ) THEN
                  WRITE(Lun,11) ctmp(:ltmp)
                  ctmp = 'COL  OFF '
                  ltmp = 9
               ELSE
                  ltmp = ltmp+1
               END IF
               istart = 0
               iend = 0
            END IF
         END IF
  190 CONTINUE
      IF(ltmp.GT.9) WRITE(Lun,11) ctmp(:ltmp-1)
C Now do COlor ON command.
      DO 290 ig=1, Ngroup
         IF(Icol(ig).GT.0 .AND. Icol(ig).NE.ig ) THEN
            ctmp = 'COL '
            ltmp = 4
            CALL CRAMI(Icol(ig), ctmp, ltmp)
            ctmp(ltmp+1:ltmp+4) =' ON '
            ltmp = ltmp + 4
            CALL CRAMI(ig, ctmp, ltmp)
            WRITE(Lun,11) ctmp(:ltmp)
         END IF
  290 CONTINUE
      RETURN
      END
