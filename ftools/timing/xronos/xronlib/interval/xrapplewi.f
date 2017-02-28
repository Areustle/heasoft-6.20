c
      SUBROUTINE xrapplewi(ewia, ewio, newi, ise, j, expos, y, sy,
     &                     intsta)
c
c ls  19/7/88 to apply exposure windows
c
c     I   ewia,ewio = exposure window start, stop (0->1)
c     I   newi = no. of exposure windows
c     I   ise = series being considered (1,2,3)
c     I   j = type being considered (1=bins, 2=new bins, 3=intvs)
c    I/R  expos = exposure fraction in whatever (0->1)
c    I/R  y = cts/s in whatever
c    I/R  sy = error on cts/s in whatever
c    I/R  intsta = counter for bins excluded by iexpos. windows
c
c If bin is rejected, expos, y and sy are flagged to the value -1.28e34,
c indicating rejection by time window and intsta is increased by 1.
c
      INTEGER*4 newi, intsta, iv, ise, j
      REAL*4 expos, y, sy, ewia(*), ewio(*)
c  if already excluded
      IF (y.LT.-1.1E34) RETURN
c
      iv = 3*(ise-1) + j
c
c check relevant exposure window
c
      IF (expos.GE.ewia(iv) .AND. expos.LE.ewio(iv)) RETURN
c
c rejection values
c
      expos = -1.28E34
      y = -1.28E34
      sy = -1.28E34
      intsta = intsta + 1
      RETURN
      END
c
c
c
