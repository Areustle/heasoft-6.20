      SUBROUTINE RAD2DET(XU,YU,XI,YI,NXY)
      implicit none
C----------------------------------------------------------------------
C GREG	Internal routine
C	Converts XU array in user coordinates (radians) into XI array 
C       in plot	coordinates (detector)
C Arguments :
C	XU	R*8	X user coordinates		Input
C	YU	R*8	Y user coordinates		Input
C	XI	R*4	X plot coordinates		Output
C	YI	R*4	Y plot coordinates		Output
C	NXY	I	Number of data points		Input
C No subroutine referenced
C----------------------------------------------------------------------
      REAL*4 XI(*),YI(*)
      REAL*8 XU(*),YU(*)
      INTEGER*4 NXY,I
      include 'greg.inc'
*
      DO I=1,NXY
         XI(I) = GX1 + GUX * (XU(I)-GUX1)
      ENDDO
*
      DO I=1,NXY
         YI(I) = GY1 + GUY * (YU(I)-GUY1)
      ENDDO
      END
