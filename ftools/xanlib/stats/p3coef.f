**==P3COEF.spg  processed by SPAG 3.09I  at 09:45 on 20 Aug 1992
*+ P3COEF - Calculates the coefficients of a parabola spec'd by 3 points
      SUBROUTINE P3COEF(X1,Y1,X2,Y2,X3,Y3,A0,A1,A2,Status)
*    Description :
*     The 3 pairs of points (X1,Y1), (X2,Y2), (X3,Y3) are used to determine
*     a parabola whose coefficients are returned in A0 (constant coeff.),
*     A1 (1st order coeff.), and A2 (2nd order coeff.).
*    Invocation :
*     CALL P3COEF(X1,Y1,X2,Y2,X3,Y3,A0,A1,A2,STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Andy Pollock (BHVAD::AMTP)
*    History :
*     10 Apr 84: original (BHVAD::AMTP)
*     28 October 1987 : ESTEC installation outside SSE
*     23 October 1991 : Adapted to allow equal pairs (kaa)
*    Global constants :
      INCLUDE 'status.codes'
*    Import :
      REAL X1 , Y1 , X2 , Y2 , X3 , Y3
c  points defining parabola
*    Import-Export :
*    Export :
      REAL A0 , A1 , A2
c  coefficients of parabola
*    Status :
      INTEGER Status
*-
 
* Status check
      IF ( Status.NE.OK__ ) RETURN
 
* Check ordinates are sensible
      IF ( (X1.EQ.X2 .AND. Y1.NE.Y2) .OR. (X1.EQ.X3 .AND. Y1.NE.Y3) .OR. 
     &     (X3.EQ.X2 .AND. Y3.NE.Y2) ) THEN
         WRITE (*,*) ' equal Xs and unequal Ys passed to P3COEF'
         Status = ERROR__
         RETURN
      ENDIF
 
*  Fit the parabola y=a2*x*x+a1*x+a0 to these 3 points
      A1 = 0
      A2 = 0
      IF ( X1.EQ.X2 .AND. X1.EQ.X3 ) THEN
         A0 = Y1
      ELSEIF ( X1.EQ.X2 ) THEN
         A1 = (Y1-Y3)/(X1-X3)
         A0 = Y1 - X1*A1
      ELSEIF ( X1.EQ.X3 .OR. X2.EQ.X3 ) THEN
         A1 = (Y1-Y2)/(X1-X2)
         A0 = Y1 - X1*A1
      ELSE
         A2 = ((Y1-Y2)/(X1-X2)-(Y1-Y3)/(X1-X3))/(X2-X3)
         A1 = (Y1-Y2)/(X1-X2) - (X1+X2)*A2
         A0 = Y1 - X1*X1*A2 - X1*A1
      ENDIF
 
      END
