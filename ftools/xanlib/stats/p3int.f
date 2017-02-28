**==P3INT.spg  processed by SPAG 3.09I  at 09:45 on 20 Aug 1992
*+ P3INT - # point parabolic interpolation
      SUBROUTINE P3INT(X1,Y1,X2,Y2,X3,Y3,Xp,Yp,Status)
*    Description :
*     The points (X1,Y1), (X2,Y2), and (X3,Y3) are used to determine a
*     parabola. This parabola is the used to establish the value of YP
*     for given XP.
*     STATUS is set if any of the x's are equal and the appropriate y's
*     unequal.
*    Invocation :
*     CALL P3INT(X1,Y1,X2,Y2,X3,Y3,XP,YP,STATUS)
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
c  3 points defining the parabola
      REAL Xp
c  ordinate of point to be found
*    Import-Export :
*    Export :
      REAL Yp
c  abscissa of point to be found
*    Status :
      INTEGER Status
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      REAL a0 , a1 , a2
c  coefficients of parabola
*    Internal References :
*    Local data :
*-
 
* Status check
      IF ( Status.NE.OK__ ) RETURN
 
* Check that x's are sensible
      IF ( (X1.EQ.X2 .AND. Y1.NE.Y2) .OR. (X1.EQ.X3 .AND. Y1.NE.Y3) .OR. 
     &     (X3.EQ.X2 .AND. Y3.NE.Y2) ) THEN
         WRITE (*,*) ' equal Xs and unequal Ys passed to UTIL_P3INT'
         Status = ERROR__
         RETURN
      ENDIF
 
*  work out the coefficients of the interpolating parabola
      CALL P3COEF(X1,Y1,X2,Y2,X3,Y3,a0,a1,a2,Status)
 
*  ... and do the interpolation
      Yp = a0 + a1*Xp + a2*Xp*Xp
 
      END
