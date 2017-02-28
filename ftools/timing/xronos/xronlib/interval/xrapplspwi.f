c
      SUBROUTINE xrapplspwi(ibf, iaf, nbint, expos, y, sy, intsta)
c
c ls  26/9/88 to apply special newbin windows
c             (if a newbin has been rejected because of intensity windows
c              then all newbins within ibf (iaf) bins before (after)
c              this newbin are also rejected)  (ibf and iaf should
C              be checked for array bounds and the actual arguments for
C              expos, y, and sy should point to the rejected intensity 
C              window
c
c     I   ibf,iaf = no. of bins before and after (ibf is .le. 0) 
c     I   nbint = no. of newbins/intv
c    I/R  expos = exposure fraction in whatever (0->1)
c    I/R  y = cts/s in whatever
c    I/R  sy = error on cts/s in whatever
c     R   intsta = no. of newbins rejected due to special windows
c
c If newbin is rejected, expos, y and sy are flagged to the value -1.27e34,
c indicating rejection by special window
c
      INTEGER*4 nbint, k, ibf, iaf, ir, i, intsta
      REAL*4 expos(0:*), y(0:*), sy(0:*)
c
c
c
c apply special windows
c
c
      IF ((y(0).LT.-1.255E34 .AND. y(0).GT.-1.265E34)) then
c     condition for exp. wind rejection and no. of newbins before > 0
c     
         DO ir = ibf, -1
            IF (y(ir).GT.-1.1E34) THEN
               expos(ir) = -1.27E34
               y(ir) = -1.27E34
               sy(ir) = -1.27E34
               intsta = intsta+1
            ENDIF
         ENDDO
c     condition for exp. wind rejection and no. of newbins after > 0
c     
         DO ir = 1, iaf
            IF (y(ir).GT.-1.1E34) THEN
               expos(ir) = -1.27E34
               y(ir) = -1.27E34
               sy(ir) = -1.27E34
               intsta = intsta+1
            ENDIF
         ENDDO
      ENDIF
c
c
      RETURN
      END
c
c
c
