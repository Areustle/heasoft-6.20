c
      SUBROUTINE xrapplfwi(fwia, fwio, nfwi, ise, j, expos, y, sy,
     &                     intsta)
c
c ls  19/7/88 to apply intensity (flux) windows
c
c     I   fwia,fwio = intensity window start, stop (cts/s)
c     I   nfwi = no. of intensity windows
c     I   ise = series being considered (1,2,3)
c     I   j = type being considered (1=bins, 2=new bins, 3=intvs)
c    I/R  expos = exposure fraction in whatever (0->1)
c    I/R  y = cts/s in whatever
c    I/R  sy = error on cts/s in whatever
c    I/R  intsta = counter for bins excluded by ints. windows
c
c If bin is rejected, expos, y and sy are flagged to the value -1.26e34,
c indicating rejection by time window and intsta is increased by 1.
c
      INTEGER*4 nfwi, k, intsta, iv, ise, j
      REAL*4 expos, y, sy, fwia(12, *), fwio(12, *)
c if already excluded
      IF (y.LT.-1.1E34) RETURN
c
      iv = 3*(ise-1) + j
c
c check all relevant intensity windows
c
      IF (y.LT.fwia(iv,1)) THEN
         GOTO 100
      ENDIF
      IF (y.GT.fwio(iv,nfwi)) THEN
         GOTO 100
      ENDIF
c
      DO k = 1, nfwi
         IF (y.GE.fwia(iv,k) .AND. y.LE.fwio(iv,k)) RETURN
      ENDDO
c
c rejection values
c
 100  CONTINUE
      expos = -1.26E34
      y = -1.26E34
      sy = -1.26E34
      intsta = intsta + 1
      RETURN
      END
c
c
c
