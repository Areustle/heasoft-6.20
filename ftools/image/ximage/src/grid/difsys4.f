      SUBROUTINE difsys4(f,eps,h,x,y)
      IMPLICIT NONE
C----------------------------------------------------------------------
C GREG	Internal routine
C       adapted from Prof. Dr. R. BULIRSCH, 1972-73
C	P. VALIRON				21-AUG-80
C	adapted for single precision		27-SEP-84
C	Integration of first order differential equations.
C	The equation is supposed to be of the form:
C             DY/DX = F(X,Y)
C Arguments :
C	F	Ext	User supplied subroutine. Computes
C			the right end of (1) for a given X and Y
C			F must contain the instructions:
C				SUBROUTINE F(X,Y,DYDX)
C				REAL*4 X,Y,DYDX
C	EPS	R*4	Accuracy, kept internally > 1.E-11
C	H	R*4	User estimate of the best step of
C			integration, updated.
C	X	R*4	initial abcissa.
C	Y	R*4	Initial value of Y.
C	After the calculation, X is replaced by X+H2 and Y by its
C	value for the abscissa X+H2. H2 is less or equal to H, its value
C	is chosen internally to satisfy the required accuracy EPS.
C	The subroutine returns H equal to the best value for the next
C	step. If the system seems to be ill conditioned, or if the initial
C	value for H is much too high, H is returned equal to 0.
C	( See BULIRSCH,R. and STOER,J.; NUM.MATH. /8/,1-13 (1966) )
C Subroutines :
C	F	External
C----------------------------------------------------------------------
      EXTERNAL f
      REAL*4 eps , h , x , y
*
      REAL*4 ya , yl , ym , dy , dz , dt(7) , d(7) , s , xn , g , b , 
     &       b1 , u , v , c , ta , w , ep(4)
      LOGICAL konv , bo , kl , gr
      INTEGER jti , m , jr , js , j , l , k
      REAL fy , eta , fs , fv , fa
      DATA ep/0.4E-1 , 0.16E-2 , 0.64E-4 , 0.256E-5/
*
      w = 0.
      jti = 0
      fy = 1.
      eta = abs(eps)
      IF ( eta.LT.1.E-11 ) eta = 1.E-11
      ya = y
      CALL f(x,y,dz)
 100  xn = x + h
      bo = .FALSE.
      s = 0.
      m = 1
      jr = 2
      js = 3
      DO 300 j = 1 , 10
         IF ( .NOT.bo ) THEN
            d(2) = 2.25
            d(4) = 9.
            d(6) = 36.
         ELSE
            d(2) = 1.777777777777778
            d(4) = 7.111111111111111
            d(6) = 28.44444444444444
         ENDIF
         IF ( j.LE.7 ) THEN
            l = j
            d(l) = m*m
         ELSE
            l = 7
            d(7) = 64.
         ENDIF
         konv = l.GT.3
         m = m + m
         g = h/float(m)
         b = g + g
         m = m - 1
         yl = ya
         ym = ya + g*dz
         DO 150 k = 1 , m
            CALL f(x+float(k)*g,ym,dy)
            u = yl + b*dy
            yl = ym
            ym = u
            u = abs(u)
            IF ( u.GT.s ) s = u
 150     CONTINUE
         CALL f(xn,ym,dy)
         kl = l.LT.2
         gr = l.GT.5
         fs = 0.
         v = dt(1)
         c = (ym+yl+g*dy)*0.5
         dt(1) = c
         ta = c
         IF ( .NOT.(kl) ) THEN
            DO 160 k = 2 , l
               b1 = d(k)*v
               b = b1 - c
               w = c - v
               u = v
               IF ( b.NE.0. ) THEN
                  b = w/b
                  u = c*b
                  c = b1*b
               ENDIF
               v = dt(k)
               dt(k) = u
               ta = u + ta
 160        CONTINUE
            IF ( konv ) THEN
               IF ( abs(y-ta).GT.s*eta ) konv = .FALSE.
            ENDIF
            IF ( .NOT.(gr .OR. s.EQ.0.) ) THEN
               fv = abs(w)/s
               IF ( fs.LT.fv ) fs = fv
            ENDIF
         ENDIF
         y = ta
         IF ( fs.EQ.0. ) GOTO 200
         fa = fy
         k = l - 1
         fy = (ep(k)/fs)**(1./float(l+k))
         IF ( l.NE.2 ) THEN
            IF ( fy.LT.0.7*fa ) GOTO 200
         ENDIF
         IF ( fy.LE.0.7 ) THEN
            h = h*fy
            jti = jti + 1
            IF ( jti.LE.5 ) GOTO 100
            GOTO 500
         ENDIF
 200     IF ( konv ) GOTO 400
         d(3) = 4.
         d(5) = 16.
         bo = .NOT.bo
         m = jr
         jr = js
         js = m + m
 300  CONTINUE
      h = h*0.5
      GOTO 100
 400  x = xn
      h = h*fy
      RETURN
 500  h = 0.
      y = ya
      RETURN
      END
