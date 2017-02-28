c
      SUBROUTINE xrappltwi(twia, twio, ntwi, dtime, dtint, expos, y, sy,
     &                     intsta, iend)
c
c ls  19/7/88 to apply time windows
c
c     I   twia,twio = time window start, stop times (days)
c     I   ntwi = no. of time windows
c     I   dtime = time of bin center (days)
c     I   dtint = duration of bin (secs)
c    I/R  expos = exposure fraction in bin (0->1)
c    I/R  y = cts/s in bin
c    I/R  sy = error on cts/s in bin
c    I/R  intsta = counter for bins excluded by time windows
c    I/R  iend = 0 normally, set =1 if time is beyond stop of last window
c
c If bin is rejected, expos, y and sy are flagged to the value -1.22e34,
c indicating rejection by time window and intsta is increased by 1.
c
      INTEGER*4 ntwi, k, intsta, iend
      REAL*4 expos, y, sy
      REAL*8 twia(*), twio(*), dtime, dtint
c
c check on all time windows
      IF (dtime.LT.twia(1)) THEN
         GOTO 101
      ENDIF
      IF (dtime.GT.twio(ntwi)) THEN
         GOTO 100
      ENDIF
      DO k = 1, ntwi
         IF (dtime.GE.twia(k) .AND. dtime.LE.twio(k)) RETURN
      ENDDO
      GOTO 101
c
c end of good data
 100  iend = 1
c
c rejection values
 101  CONTINUE
      expos = -1.22E34
      y = -1.22E34
      sy = -1.22E34
      intsta = intsta + 1
      RETURN
      END
c
c
c
