      SUBROUTINE DRWELP(X,Y,RX,RY,SINA,COSA)
      implicit none
c
c Draws an ellipse at SCREEN coords
c 
c I  x    r  x pixel center in display coordinates
c I  y    r  y pixel center in display coordinates
c I  rx   r  x radius in display coordinates
c I  ry   r  y radius in display coordinates
c I  sina r  sine of angle of rotation
c I  cosa r  cosine of angle of rotation
c 
      REAL*4 X , Y , RX, RY, SINA, COSA
      REAL*4 xpt
      REAL*4 ypt
      REAL*4 PI
      PARAMETER (PI=3.1415926)
      INTEGER*4 i
      real*4 xoff, yoff
 
      i = 1
      xoff = RX*COS(FLOAT(i-1)*2*PI/200.)
      yoff = RY*SIN(FLOAT(i-1)*2*PI/200.)
      xpt = X + xoff*COSA - yoff*SINA
      ypt = Y + xoff*SINA + yoff*COSA
      CALL PGMOVE(xpt,ypt)
      DO 100 i = 2 , 201
         xoff = RX*COS(FLOAT(i-1)*2*PI/200.)
         yoff = RY*SIN(FLOAT(i-1)*2*PI/200.)
         xpt = X + xoff*COSA - yoff*SINA
         ypt = Y + xoff*SINA + yoff*COSA
         CALL PGDRAW(xpt,ypt)
 100  CONTINUE
 
      RETURN
      END
