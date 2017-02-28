      PROGRAM TEST
      REAL      FPNUM
C
      CHARACTER CLINE*64, CBUF*72, cpar*72
      REAL      rnum
      INTEGER   I, IER, LLINE, kp, lpar
C
C      CALL LDBUF('LOADED LINE1',1,IER)
C      CALL LDBUF('LOADED LINE2',1,IER)
      DO I=1,100
         CALL GTBUF('TEST>', IER)
         IF(IER.LT.0) GOTO 999
         CALL GTREST(CLINE, LLINE)
         WRITE(CBUF,'(1X,I3,2X,A)') I,CLINE(1:LLINE)
         CALL TTWRT(CBUF, -1)
         kp = 0
         CALL ALF(cline, lline, kp, cpar, lpar)
         rnum = FPNUM(cpar, lpar, ier)
         WRITE(*,*) 'FPNUM=', rnum, ier
      END DO
C
  999 CONTINUE
      WRITE(*,*) 'This line was written with a Fortran WRITE.'
      CALL TTWRT(' There should be a blank line between here,',-1)
      CALL TTWRT('0and here.',-1)
      CALL EDICOM('OFF',3)
      END
