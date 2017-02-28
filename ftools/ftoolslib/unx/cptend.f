*+CPTEND
      SUBROUTINE CPTEND(CDISK, CDIR, CFILE)
      CHARACTER CDISK*(*), CDIR*(*), CFILE*(*)
C-----------------------------------------------------------------------
C Description: Prefix exTENsion.  Add 'prefix' to file name. Does 
C              automatic translation of environment variable as "disk".
C
C Arguments:   CDISK     (i)    The disk name
C              CDIR      (i)    The directory name
C              CFILE     (i/r)  The file name
C
C Origin:      Swiped form Xanadu Library for Claibration Library
C
C Authors/Modification History:
C              AFT (1988 Aug 2) Original Version
C              KAA (1990 Sep 23) added TRLOG check.
C              Ron Zellar (1993 Apr 7) modified for Calibration Database
C-----------------------------------------------------------------------
*- Version 1.0
      INTEGER   FCSTLN
      INTEGER   I, LDISK, LDIR, LFILE, LOUT, LTMP
      character(256) CTMP, CTMP2
C---
C- Check for an environment variable as the disk. If none found
C- then also tries lowercase and uppercase versions.
      CTMP2 = CDISK
      LDISK=FCSTLN(CDISK)-1
      CALL CTRLOG(CTMP2(2:LDISK+1), LDISK, CTMP, LTMP)
      IF (LTMP.EQ.0) THEN
         CALL FTUPCH(CTMP2)
         CALL CTRLOG(CTMP2(2:LDISK+1), LDISK, CTMP, LTMP)
      ENDIF
      IF (LTMP.EQ.0) THEN
         CALL CLOCASE(CTMP2)
         CALL CTRLOG(CTMP2(2:LDISK+1), LDISK, CTMP, LTMP)
      ENDIF
C---
C- Check that created filename will not overrun buffer.
      IF(LTMP.GT.0) THEN
         LDISK=LTMP+1
      ELSE
         LDISK=FCSTLN(CDISK)+2
      ENDIF
      LDIR =FCSTLN(CDIR)+1
      LFILE=FCSTLN(CFILE)
      LOUT=LDISK+LDIR+LFILE
      IF(LOUT.GT.LEN(CFILE)) THEN
         CALL FCERR('CPTEND error, not enough room to create filename.')
         STOP
      END IF
C---
C- Move characters in CFILE
      DO 170 I=0,LFILE-1
        CFILE(LOUT-I:LOUT-I)=CFILE(LFILE-I:LFILE-I)
  170 CONTINUE
C---
C- Finish the job
      IF(LTMP.GT.0) THEN
         CFILE(1:LDISK)=CTMP(:LTMP)//'/'
      ELSE
         CFILE(1:LDISK)='/'//CDISK(:LDISK-2)//'/'
      ENDIF
      CFILE(LDISK+1:LDISK+LDIR)=CDIR(:LDIR-1)//'/'
      CALL CCONC(CFILE)
      RETURN
      END
