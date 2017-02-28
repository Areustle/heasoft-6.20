C
      FUNCTION xrporegr(BTIME, I, J, JTERMS)
c
c ls 17/9/88 Evaluate terms of polynomial function for xrregr routine
c
c
c I    btime=binning time (to reconstruct time)
C I    I=index of data points
C I    J=index of term in polynomial function
C I    JTERMS=array of powers
C
C adapted from Bevington;  LS        10-6-83
C
      REAL*4 time, btime, xrporegr
      INTEGER*4 i, j, jterms(*)
      TIME = BTIME*float(I)
      xrporegr = TIME**JTERMS(J)
      RETURN
      END
C
