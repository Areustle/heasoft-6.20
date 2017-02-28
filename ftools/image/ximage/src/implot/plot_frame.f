      SUBROUTINE PLOT_FRAME(Uptitle,Lotitle,Xlabel,Ylabel,Noframe,
     &                      Nobox,Xmin_frame,Xmax_frame,Ymin_frame,
     &                      Ymax_frame)
      IMPLICIT NONE
c
c  Plots frame around image and labels
c
c  I  Uptitle     (c)  Text immediately above Lotitle
c  I  Lotitle     (c)  Text immediately above viewport
c  I  Xlabel      (c)  X-axis label
c  I  Ylabel      (c)  Y-axis label
c  I  Noframe     (l)  Whether only image, without frame (ticks/labels)
c  I  Nobox       (l)  Whether only image (only title and image, no box)
c  I  Xmin_frame  (r)  Lower bound of image frame in x
c  I  Xmax_frame  (r)  Upper bound of image frame in x
c  I  Ymin_frame  (r)  Lower bound of image frame in y
c  I  Ymax_frame  (r)  Upper bound of image frame in y
c
      REAL*4 Xmin_frame , Xmax_frame , Ymin_frame , Ymax_frame
      CHARACTER*(*) Uptitle , Lotitle , Xlabel , Ylabel
      LOGICAL Noframe, Nobox
c
c  Local variables
      INTEGER color, lwidth, lstyle
      LOGICAL overint
      REAL*8 dd
c
c Color=foreground, lwidth=1 pixel, lstyle=solid for frame and labels
      color = 1
      lwidth = 1
      lstyle = 1
      call PGSAVE
      call line_pgstate(color, lwidth, lstyle)
c
c  call pgplot routine for the min/max frame
c  and min/max map
      CALL PGSWIN(Xmin_frame,Xmax_frame,Ymin_frame,Ymax_frame)
c
c  Check for potential integer overflow to avoid pgplot bug
c
      overint = .FALSE.
      dd = 5.*Xmin_frame*10000./(Xmax_frame-Xmin_frame)
      if ( dd.gt.2.0e9 ) overint = .TRUE.
      dd = 5.*Ymin_frame*10000./(Ymax_frame-Ymin_frame)
      if ( dd.gt.2.0e9 ) overint = .TRUE.
      
      IF ( .NOT.overint .and. .NOT.Noframe ) THEN
         CALL PGBOX('BCNT',0.,0,'BCNT',0.,0)
      ELSEIF ( .not.Nobox ) THEN
         CALL PGBOX('BC',0.0,0,'BC',0.0,0)
      ENDIF
c
c if no frame labels set blank
      IF ( Noframe ) THEN
         Xlabel = ' '
         Ylabel = ' '
      ENDIF
      CALL PGLABEL(Xlabel,Ylabel,Lotitle(1:79))
      CALL PGMTXT('T',3.2,0.5,0.5,Uptitle(1:79))
C
C  Obligatory PGIDEN replaced with optional timestamp command
C     CALL PGIDEN
      call PGUNSA
      RETURN
      END
