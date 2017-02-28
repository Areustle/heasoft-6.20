      SUBROUTINE DRWCIR(X,Y,R)
      implicit none
c
c Draws a circle at SCREEN coords x,y with radius r
c 
c I  x   r  x pixel in display coordinates
c I  y   r  y pixel in display coordinates
c I  r   r  radius in display coordinates
c 
      REAL*4 X , Y , R
      REAL*4 PI
      PARAMETER (PI=3.1415926)
      REAL*4 xpt, ypt
      REAL*4 x1, x2, y1, y2, wid
      INTEGER*4 i, npts
 
      call pgqwin(x1,x2,y1,y2)
      wid = MIN(x2-x1,y2-y1)
c     Number of points range from 5 to 200
      npts = MAX(5,MIN(INT(R/wid*2000.),200))

      i = 1
      xpt = X + R*COS(FLOAT(i-1)*2*PI/float(npts))
      ypt = Y + R*SIN(FLOAT(i-1)*2*PI/float(npts))
      CALL PGMOVE(xpt,ypt)
      do i = 2 , npts+1
         xpt = X + R*COS(FLOAT(i-1)*2*PI/float(npts))
         ypt = Y + R*SIN(FLOAT(i-1)*2*PI/float(npts))
         CALL PGDRAW(xpt,ypt)
      enddo
 
      RETURN
      END
