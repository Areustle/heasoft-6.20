      subroutine spectro_frame(Lotitle,xlabel,ylabel,noframe,xmin_frame,
     &                         xmax_frame,ymin_frame,ymax_frame)
      implicit none
c
c  Displays frame around spectrogram
c
c  I  Lotitle     (c)  Text for frame title
c  I  Xlabel      (c)  X-axis label
c  I  Ylabel      (c)  Y-axis label
c  I  Noframe     (l)  Whether only image, without frame
c  I  Xmin_frame  (r)  Lower bound of image frame in x
c  I  Xmax_frame  (r)  Upper bound of image frame in x
c  I  Ymin_frame  (r)  Lower bound of image frame in y
c  I  Ymax_frame  (r)  Upper bound of image frame in y
c
      real*4 xmin_frame,xmax_frame,ymin_frame,ymax_frame
      character*(*) Lotitle, xlabel, ylabel
      logical noframe 
c
c  Local variables
      real*4 disp,coord
c
c store viewport coords of image panel
      call line_pgstate(1,1,1)
      CALL pgwindow(xmin_frame,xmax_frame,ymin_frame,ymax_frame)
      If  ( .NOT.Noframe ) THEN
         CALL PGBOX('BCNT',0.,0,'BCNT',0.,0)
      ELSE
         CALL PGBOX('BC',0.0,0,'BC',0.0,0)
      ENDIF 
      call pglabel(xlabel,' ',Lotitle)      
      disp = 4.
      coord= 0.2
      call pgmtext('L',disp,coord,0.,ylabel)
      return
      END
