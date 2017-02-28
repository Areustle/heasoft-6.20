* Last processed by NICE on 10-SEP-1991 16:34:14
* Customized for :  VAX         VMS          
*----------------------------------------------------------------------
*
      REAL*8
     $GUX1, GUX2,                  ! Image limits in X (radians cen=0)
     $GUY1, GUY2,                  ! Image limits in Y (radians cen=0)
     $GUX, GUY                     ! Ratio of pixels to radians
*
      REAL*4
     $GX1,GX2,GY1,GY2,             ! Image limits (detector coords)
c    $XP,YP,                       ! Pen position
     $ACCURD                       ! Plot accuracy (cm)
*
      LOGICAL
     $TICKS                        ! Logical for a grid only shown with ticks
*
      COMMON /GREG01/
     $GUX1, GUX2, GUY1, GUY2, GUX, GUY,
     $GX1,GX2,GY1,GY2, ACCURD, TICKS
*
      SAVE /GREG01/
*----------------------------------------------------------------------
