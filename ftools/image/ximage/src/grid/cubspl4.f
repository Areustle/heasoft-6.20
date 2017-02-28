      SUBROUTINE cubspl4(n,tau,c1,c2,c3,c4,ibcbeg,ibcend,*)
      IMPLICIT NONE
C----------------------------------------------------------------------
C GREG	Internal routine
C
C	Piecewise cubic spline interpolants computation; adapted from
C	'A practical guide to SPLINES' , CARL DE BOOR ,
C	Applied Mathematical Sciences, SPRINGER-VERLAG, VOL.27, P57-59 (1978).
C
C Arguments :
C     	N	I	Number of data points. assumed to be .ge. 2.	Input
C	TAU	R*4(N)	Abscissae of data points, strictly monotonous	Input
C	C1	R*4(N)	Ordinates of data points			Input
C     	IBCBEG	I							Input
C	IBCEND  I	Boundary condition indicators, and		Input
C	C1	R*4(N)	First polynomial coefficient of spline	Output
C	C2	R*4(N)	Second   --         --           --	Output/Input
C	C3	R*4(N)	Third    --         --           --	Output
C	C4	R*4(N)	Fourth   --         --           --	Output
C
C 	C2(1), C2(N)  are boundary condition information.
C	Specifically,
C	IBCBEG = 0  means no boundary condition at tau(1) is given.
C		In this case, the not-a-knot condition is used, i.e. the
C		jump in the third derivative across TAU(2) is forced to
C		zero, thus the first and the second cubic polynomial pieces
C		are made to coincide.
C        IBCBEG = 1 means that the slope at TAU(1) is made to equal
C		C2(1), supplied by input.
C        IBCBEG = 2 means that the second derivative at TAU(1) is
C		made to equal C2(1), supplied by input.
C        IBCEND = 0, 1, or 2 has analogous meaning concerning the
C		boundary condition at TAU(N), with the additional
C		information taken from C2(N).
C
C	CJ(I), J=1,...,4; I=1,...,L (= N-1) are the polynomial
C	coefficients of the cubic interpolating spline. Precisely,
C	in the interval    [ TAU(I), TAU(I+1) ]    the spline F
C	is given by
C		F(X) = C1(I)+H*(C2(I)+H*(C3(I)+H*C4(I)/3.)/2.)
C	where H = X - TAU(I).
C
C	In other words, for I=1,...,N, C2(I) and C3(I) are respectively
C	equal to the values of the first and second derivatives of
C	the interpolating spline, and C4(I) is equal to the third
C	derivative of the interpolating spline in the interval
C	[ TAU(I), TAU(I+1) ]. C4(N) is meaningless and is set to 0.
C
C----------------------------------------------------------------------
      INTEGER n
      REAL tau(n) , c1(n) , c2(n) , c3(n) , c4(n)
*
      REAL taum1 , g , dtau , divdf1 , divdf3
      INTEGER i , j , l , m , ibcbeg , ibcend

c  Initialize to avoid warning
      dtau = 0.
c  --
*
* A tridiagonal linear system for the unknown slopes S(I) of
* F at TAU(I), I=1,...,N, is generated and then solved by gauss
* elimination, with S(I) ending up in C2(I), all I.
* C3(.) and C4(.) are used initially for temporary storage.
*
* Check if N.GE.2
*
      IF ( n.LT.2 ) STOP 'F-CUBSPL4,  Less than two pivots'
*
* Check if TAU strictly monotonous
      taum1 = tau(2)
      IF ( tau(2).LT.tau(1) ) THEN
         IF ( n.NE.2 ) THEN
            DO 20 i = 3 , n
               IF ( (tau(i)-taum1).GE.0. ) GOTO 200
               taum1 = tau(i)
 20         CONTINUE
         ENDIF
      ELSEIF ( tau(2).EQ.tau(1) ) THEN
         GOTO 200
      ELSEIF ( n.NE.2 ) THEN
         DO 50 i = 3 , n
            IF ( (tau(i)-taum1).LE.0. ) GOTO 200
            taum1 = tau(i)
 50      CONTINUE
      ENDIF
*
      l = n - 1
*
* Compute first differences of TAU sequence and store in C3(.).
* Also, compute first divided difference of data and store in C4(.).
*
      DO 100 m = 2 , n
         c3(m) = tau(m) - tau(m-1)
         c4(m) = (c1(m)-c1(m-1))/c3(m)
 100  CONTINUE
*
* Construct first equation from the boundary condition, of the form
*             C4(1)*S(1) + C3(1)*S(2) = C2(1)
*
      IF ( ibcbeg.LT.1 ) THEN
         IF ( n.GT.2 ) THEN
*
* Not-a-knot condition at left end and N .GT. 2.
            c4(1) = c3(3)
            c3(1) = c3(2) + c3(3)
            c2(1) = ((c3(2)+2.*c3(1))*c4(2)*c3(3)+c3(2)**2*c4(3))/c3(1)
            GOTO 400
         ELSE
*
* No condition at left end and N = 2.
            c4(1) = 1.
            c3(1) = 1.
            c2(1) = 2.*c4(2)
            GOTO 800
         ENDIF
      ELSEIF ( ibcbeg.EQ.1 ) THEN
*
* Slope prescribed at left end.
         c4(1) = 1.
         c3(1) = 0.
         GOTO 300
      ELSE
*
* Second derivative prescribed at left end.
         c4(1) = 2.
         c3(1) = 1.
         c2(1) = 3.*c4(2) - c3(2)/2.*c2(1)
         GOTO 300
      ENDIF
 200  CALL xwrite('E-CUBSPL4,  Variable is not strictly monotonous', 10)
      RETURN 1
 300  IF ( n.EQ.2 ) GOTO 800
*
* If there are interior knots, generate the corresponding equations and
* carry out the forward pass of gauss elimination, after which the M-th
* equation reads    C4(M)*S(M) + C3(M)*S(M+1) = C2(M).
*
 400  DO 500 m = 2 , l
         g = -c3(m+1)/c4(m-1)
         c2(m) = g*c2(m-1) + 3.*(c3(m)*c4(m+1)+c3(m+1)*c4(m))
         c4(m) = g*c3(m-1) + 2.*(c3(m)+c3(m+1))
 500  CONTINUE
*
* Construct last equation from the second boundary condition, of the form
*           (-G*C4(N-1))*S(N-1) + C4(N)*S(N) = C2(N)
* If slope is prescribed at right end, one can go directly to back
* substitution, since C array happens to be set up just right for it
* at this point.
*
      IF ( ibcend.LT.1 ) THEN
         IF ( n.NE.3 .OR. ibcbeg.NE.0 ) THEN
*
* not-a-knot and N.GE.3, and either N.GT.3 or also not-a-knot at
* left endpoint.
*
            g = c3(l) + c3(n)
            c2(n) = ((c3(n)+2.*g)*c4(n)*c3(l)+c3(n)**2*(c1(l)-c1(n-2))
     &              /c3(l))/g
            g = -g/c4(l)
            c4(n) = c3(l)
            GOTO 900
         ENDIF
      ELSEIF ( ibcend.EQ.1 ) THEN
         GOTO 1000
      ELSE
         GOTO 700
      ENDIF
*
* Either (N=3 and not-a-knot also at left) or (N=2 and not not-a-knot
* at left endpoint).
*
 600  c2(n) = 2.*c4(n)
      c4(n) = 1.
      g = -1./c4(l)
      GOTO 900
*
* Second derivative prescribed at right endpoint.
 700  c2(n) = 3.*c4(n) + c3(n)/2.*c2(n)
      c4(n) = 2.
      g = -1./c4(l)
      GOTO 900
 800  IF ( ibcend.LT.1 ) THEN
         IF ( ibcbeg.GT.0 ) GOTO 600
*
* not-a-knot at right endpoint and at left endpoint and N=2.
         c2(n) = c4(n)
         GOTO 1000
      ELSEIF ( ibcend.EQ.1 ) THEN
         GOTO 1000
      ELSE
         GOTO 700
      ENDIF
*
* Complete forward pass of GAUSS elimination.
 900  c4(n) = g*c3(l) + c4(n)
      c2(n) = (g*c2(l)+c2(n))/c4(n)
*
* Carry out back substitution
 1000 DO 1100 j = l , 1 , -1
         c2(j) = (c2(j)-c3(j)*c2(j+1))/c4(j)
 1100 CONTINUE
*
* Generate cubic coefficients in each interval, i.e., the derivatives
* at its left endpoint, from value and slope at its endpoints.
*
      DO 1200 i = 2 , n
         dtau = c3(i)
         divdf1 = (c1(i)-c1(i-1))/dtau
         divdf3 = c2(i-1) + c2(i) - 2.*divdf1
         c3(i-1) = 2.*(divdf1-c2(i-1)-divdf3)/dtau
         c4(i-1) = (divdf3/dtau)*(6./dtau)
 1200 CONTINUE
*
* Compute in addition C3(N). set C4(N) to 0.
      c3(n) = c3(l) + c4(l)*dtau
      c4(n) = 0.
      END
