c
      SUBROUTINE xrpowtwo(n, imod)
c      SUBROUTINE xrpowtwo(n, nmax, imod)
c
c  ls 25/8/88 to handle a power of 2 integer
c
c    I/O  n = no. to be analysed (left unchnaged if it is a power of 2)
c    I    imod = 0 to set n=0 if n is not a power of 2
c              = 1 to set n equal to the next larger power of two
c
      INTEGER*4 n, imod, npo
      npo = 1
c
 1    npo = npo*2
      IF (n.EQ.npo) RETURN
      IF (n.LT.npo) then
         if(imod.EQ.1) THEN
            n = npo
         else if (imod.EQ.0) then
            n = 0
         ENDIF
         RETURN
      endif

      GOTO 1
c
      END
c
c
c
