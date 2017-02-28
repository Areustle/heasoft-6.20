c
      SUBROUTINE xrapplpwi(pwi, pwia, pwio, npwi, dtime, dtint, expos,
     &                     y, sy, intsta)
c
c ls  22/8/88 to apply phase windows
c
c     I   pwi = epoch and period for phase windows (days)
c     I   pwia,pwio = phase window start, stop
c     I   npwi = no. of phase windows
c     I   dtime = time of bin center (days)
c     I   dtint = duration of bin (days)
c    I/R  expos = exposure fraction in bin (0->1)
c    I/R  y = cts/s in bin
c    I/R  sy = error on cts/s in bin
c    I/R  intsta = counter for bins excluded by phase windows
c
c If bin is rejected, expos, y and sy are flagged to the value -1.24e34,
c indicating rejection by phase window and intsta is increased by 1.
c
      INTEGER*4 npwi, k, intsta
      REAL*4 expos, y, sy, pwia(*), pwio(*), phase
      REAL*8 pwi(*), dtime, dtint, dphase, dtim
c
c work out phase
      dtim = (dtime-pwi(1))
c remainder
      dphase = dmod(dtim, pwi(2))
c phase
      dphase = dphase/pwi(2)
c if dtime is earlier than epoch
      IF (dphase.LT.0.D0) dphase = dphase + 1.D0
c real*4 conversion
      phase = dphase
c
c check on all phase windows
      DO k = 1, npwi
         IF (phase.GE.pwia(k) .AND. phase.LE.pwio(k)) RETURN
      ENDDO
c
c rejection values
      expos = -1.24E34
      y = -1.24E34
      sy = -1.24E34
      intsta = intsta + 1
      RETURN
      END
c
c
c
