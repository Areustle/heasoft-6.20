      SUBROUTINE ZFUN(FUNC,Xmin,Xmax,Stepxx,Ok,Result,Imsol,Arycon)
      implicit none
C     ZERO OF FUNCTIONS
C***** THIS SUBROUTINE COMPUTES THE SOLUTION(S) OF THE EQ. F(X)=0
C***** IN AN INTERVAL DEFINED BY "XMIN"-"XMAX".
C***** "OK" IS THE MAXIMUM DEVIATION FROM THE REAL SOLUTION
C***** IF IN THE SPECIFIED INTERVAL (XMIN-XMAX) MORE THEN ONE
C***** SOLUTION IS PRESENT , ONLY THE LAST ONE WILL BE RETURNED
C***** AND THE FLAG IMSOL WILL BE SET EQUAL TO THE ACTUAL NUMBER
C***** OF SOULTIONS FOUND
C***** IF NO SOLUTIONS ARE FOUND IN THE INTERVAL XMIN-XMAX
C***** RESULT WILL BE ZERO AND IMSOL EQUAL TO ZERO.
C ** FUNC IS THE FUNCTION F(X) AND MUST BE DECLARED EXTERNAL IN THE
C ** MAIN PROGRAM
C ** XMIN AND XMAX DEFINE THE RANGE WHERE SOLUTIONS ARE SERCHED
C ** STEPXx IS THE STARTING VALUE FOR THE INCREMENT OF THE ARGUMENT
C ** OF THE FUNCTION
C ** OK IS THE MAXIMUM DEVIATION OF THE RETURNED NUMBER FROM THE
C ** TRUE SOLUTION OF THE EQUATION
C ** RESULT IS THE RETURNED VALUE of the last solution found
C ** IMSOL IS THE NUMBER OF SOLUTIONS FOUND
c
      REAL*4 FUNC
      real*4 Xmin, Xmax, Stepxx, Ok, Result, Arycon(*)
      integer*4 Imsol

      REAL*4 stepx , step , x1 , x2
      REAL*4 fx1 , fx2 , x
      logical loop
c
c  Initialize to avoid warning
      x = 0.
c  --
      stepx = Stepxx
      Imsol = 0
      step = stepx
      x1 = Xmin
c
 100  x2 = x1 + stepx
      fx1 = FUNC(x1,Arycon)
      loop = .true.
      DO WHILE ( loop )
c
c
         IF ( x2.LE.Xmax ) THEN
            fx2 = FUNC(x2,Arycon)
c
            IF ( fx1*fx2.LE.0. ) THEN
               IF ( ABS(x1-x2).LT.Ok ) THEN
                  x = (x1+x2)/2.
                  x1 = x + 2.*Ok
                  stepx = step
                  Imsol = Imsol + 1
                  GOTO 100
               ENDIF
               stepx = stepx/2.
               x2 = x1 + stepx
            ELSE
               x1 = x1 + stepx
               x2 = x1 + stepx
               fx1 = fx2
            ENDIF
            GOTO 200
         ENDIF
         Result = x
         RETURN
 200  ENDDO
      END
