      SUBROUTINE PLOT_START(Mapid,Ixmin,Ixmax,Iymin,Iymax,Uptitle,
     &                      Lotitle,Xlabel,Ylabel,R_black,R_white,
     &                      Xmin_frame,Xmax_frame,Ymin_frame,Ymax_frame,
     &                      Status)
      implicit none
c
c  Starts the image display. Performs tasks common to the 
c    display and contour commands.
c
c  I  Mapid        (c)  Display map id 
c  I  Ixmin        (i)  Min x index to display in image
c  I  Ixmax        (i)  Max x index to display in image
c  I  Iymin        (i)  Min y index to display in image
c  I  Iymax        (i)  Max y index to display in image
c I/O Uptitle      (c)  Text immediately above Lotitle
c I/O Lotitle      (c)  Text immediately above viewport
c  O  Xlabel       (c)  X-axis label
c  O  Ylabel       (c)  Y-axis label
c  O  R_black      (r)  Minimum image value
c  O  R_white      (r)  Maximum image value
c  O  Xmin_frame   (r)  Lower bound of image frame in x
c  O  Xmax_frame   (r)  Upper bound of image frame in x
c  O  Ymin_frame   (r)  Lower bound of image frame in y
c  O  Ymax_frame   (r)  Upper bound of image frame in y
c  O  Status       (i)  Error flag (0=OK)
c  
      INCLUDE '../include/startup.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'

      REAL*4 R_black, R_white
      REAL*4 Xmin_frame , Xmax_frame , Ymin_frame , Ymax_frame
      CHARACTER*(*) Mapid, Uptitle, Lotitle, Xlabel, Ylabel
      INTEGER*4 Ixmin, Ixmax, Iymin, Iymax, Status
c
c  Local variables
      INTEGER*4 LENACT, iexp
      INTEGER*4 year, mon, day, szx, szy
      REAL*4 xmin, xmax, ymin, ymax, xpix, ypix
      REAL*8 exposure, bscale, bzero, dd
      character(10) XMONTH, monstr
      character(16) time, seconds
      character(100) telstr, ds
      LOGICAL ISRNULL
c
c start variable
      Status = 0
      Xlabel = 'X Pixels'
      Ylabel = 'Y Pixels'
c
c  Look up exposure, needed for lotitle and/or exposure corrected map
c
      call gheadd (Mapid, 'EXPOSURE', exposure, 0, status)
c
c if unknown telescope title is blank otherwise uses 
c telescope instrument date and exposure from the header
c
      call get_telstr(Mapid, telstr)
      IF ( telstr(1:7).eq.'UNKNOWN' ) then
         CALL XWRITE(' ** Warning  unknown telescope ',10)
      ELSE
         call gheads (Mapid, 'DATE-OBS', ds, 0, status)
         status = 0
         call fts2dt (ds, year, mon, day, status) 
         if ( status.eq.0 .and. mon.ne.0 ) then
            monstr = XMONTH(mon)
            WRITE (time,'(i4.4,1x,a,1x,i2)') year , monstr(1:3) , day
         else
            time = '           '
         endif
         IF ( Uptitle(80:80).NE.'*' ) THEN
            call gheads (Mapid, 'OBJECT', Uptitle, 0, status)
         ENDIF
         IF ( Lotitle(80:80).NE.'*' ) THEN
            Lotitle = telstr(:LENACT(telstr))//'   '//time(1:13)
            iexp = NINT(exposure)
            IF ( iexp.GT.0. AND. iexp.LE.2147483646) THEN
                 WRITE (seconds,'(I16)') iexp
                 call RMVLBK(seconds)
                 Lotitle = Lotitle(:LENACT(Lotitle))//'    '//
     &                 'Exposure: '//seconds(:LENACT(seconds))//' s'
            ENDIF
         ENDIF
      ENDIF
c
c read from the internal header the minimum photon/pixel
c to set the black
c
      call gheadd (Mapid, 'BSCALE', bscale, 0, status)
      call gheadd (Mapid, 'BZERO', bzero, 0, status)
      call gheadd (Mapid, 'DATAMIN', dd, 0, status)
      R_black = dd/bscale - bzero
c
c read from the internal header the minimum photon/pixel
c to set the white
c
      call gheadd (Mapid, 'DATAMAX', dd, 0, status)
      R_white = dd/bscale - bzero
c
c get size and decide the dimension to plot
      call gheadi (Mapid, 'SZX', szx, 0, status)
      call gheadi (Mapid, 'SZY', szy, 0, status)
c
      WRITE (ds,*) ' Min =' , R_black , ' Max = ' , R_white
      IF ( Status.NE.0 ) RETURN
      IF ( R_black.EQ.0. .AND. R_white.EQ.0. ) THEN
         CALL XWRITE(' The map is all zero',5)
      ELSE IF ( ISRNULL(R_black) .AND. ISRNULL(R_white) ) THEN
         CALL XWRITE(' The map is all null',5)
      ELSE
         CALL XWRITE(ds,10)
      ENDIF
c
c  calculate frame min and max. This is in display coordinates
c
      xmin = float(ixmin) - 0.5
      xmax = float(ixmax) + 0.5
      ymin = float(iymin) - 0.5
      ymax = float(iymax) + 0.5

      call imgpix(Mapid, xpix, ypix, xmin, ymin, 1, status)
      Xmin_frame = xpix
      Ymin_frame = ypix
      call imgpix(Mapid, xpix, ypix, xmax, ymax, 1, status)
      Xmax_frame = xpix
      Ymax_frame = ypix
      if ( status.ne.0 ) 
     &   call xwrite(' Error in determining plot frame from WCSID', 5)
         

      write(ZWRite,*) ' Detector frame: ', Xmin_frame, Xmax_frame, 
     &                                     Ymin_frame, Ymax_frame
      call xwrite(ZWRite, 20)

      RETURN
99001 FORMAT (' ',i4,' ',a,' ',i2)
      END
