**==LOC0D.spg  processed by SPAG 3.09I  at 09:44 on 20 Aug 1992
      INTEGER FUNCTION LOC0D(X,N,Xx)
* Import :
      REAL*8 X
      INTEGER N
      REAL*8 Xx(*)
* Local variables :
      INTEGER l , m , u
      INTEGER i
*-
      IF ( N.EQ.0 ) THEN
         i = 0
      ELSEIF ( N.EQ.1 ) THEN
         i = 1
      ELSE
         l = 0
         u = N + 1
         DO WHILE ( u-l.GT.1 )
            m = (u+l)/2
            IF ( (Xx(N).GT.Xx(1)) .EQV. (X.GT.Xx(m)) ) THEN
               l = m
            ELSE
               u = m
            ENDIF
         ENDDO
         i = l
      ENDIF
 
      LOC0D = i
 
      END
