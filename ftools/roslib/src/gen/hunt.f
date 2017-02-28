      SUBROUTINE HUNT(XX,N,X,JLO)
c From Numerical Recipes, converted to double precision, eal 5/94.

c For an ordered input vector xx, hunt finds the position of x.
c xx(jlo) is the element in the vector such that x lies between
c xx(jlo) and xx(jlo + 1).  Xx can be in either ascending or descending
c order.  The routine will search through N elements.

      LOGICAL ASCND
      integer n,jlo,jhi,inc,jm
      double precision XX(N),x
      ASCND=XX(N).GT.XX(1)
      IF(JLO.LE.0.OR.JLO.GT.N)THEN
        JLO=0
        JHI=N+1
        GO TO 3
      ENDIF
      INC=1
      IF(X.GE.XX(JLO).EQV.ASCND)THEN
1       JHI=JLO+INC
        IF(JHI.GT.N)THEN
          JHI=N+1
        ELSE IF(X.GE.XX(JHI).EQV.ASCND)THEN
          JLO=JHI
          INC=INC+INC
          GO TO 1
        ENDIF
      ELSE
        JHI=JLO
2       JLO=JHI-INC
        IF(JLO.LT.1)THEN
          JLO=0
        ELSE IF(X.LT.XX(JLO).EQV.ASCND)THEN
          JHI=JLO
          INC=INC+INC
          GO TO 2
        ENDIF
      ENDIF
3     IF(JHI-JLO.EQ.1)RETURN
      JM=(JHI+JLO)/2
      IF(X.GT.XX(JM).EQV.ASCND)THEN
        JLO=JM
      ELSE
        JHI=JM
      ENDIF
      GO TO 3
      END
