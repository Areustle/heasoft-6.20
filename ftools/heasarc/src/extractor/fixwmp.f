
      SUBROUTINE fixwmp(Wmsums, Imszh1, Imszh2, Imageh, Status)

      INTEGER Imszh1, Imszh2

      DOUBLE PRECISION Wmsums(9)

      REAL Imageh(Imszh1, Imszh2)

      INTEGER Status

c Routine to fix the WMAP by setting bins outside the map to -1. If the image
c and WMAP coordinates differ the complication is that the region is not given 
c in the same coordinates as the WMAP. The Wmsums array is used to derive the 
c relation between the coordinate systems.
c Arguments :
c       Wmsums         d           i: Summation over events of xf, yf, xh, yh,
c                                     xf*xh, xf*yh, yf*xh, yf*yh, N, where 
c                                     (xf,yf) is the position in image
c                                     and (xh,yh) in detector coordinates.
c       Imszh1         i           i: X-axis size of the WMAP
c       Imszh2         i           i: Y-axis size of the WMAP
c       Imageh         r         i/r: WMAP array
c       Status         i           r: Status  0==OK

      INCLUDE 'extractor.inc'

      DOUBLE PRECISION PI
      PARAMETER (PI=3.14159265358979323846264338327950d0)

      DOUBLE PRECISION f1, f2, xh, yh, xf, yf
      DOUBLE PRECISION xoff, yoff, cwftheta, swftheta

      INTEGER i, j, nevents

      CHARACTER(255) contxt

      LOGICAL qwarn

      LOGICAL select_region
      EXTERNAL select_region

      Status = 0

c First do the easy case of sky and WMAP coordinates the same

      IF ( (Xcolf.EQ.Xcolh) .AND. (Ycolf.EQ.Ycolh) ) THEN

         DO j = 1 , Imszh2
            DO  i = 1 , Imszh1

               IF ( imageh(i,j) .LE. 0.0 ) THEN

c If the center of the bin falls outside the region then mark with
c Imageh = -1.

                  xh = (i-1)*Binh + hbbox(1) + 0.5*(Binh-1)
                  yh = (j-1)*Binh + hbbox(2) + 0.5*(Binh-1)

                  IF ( .NOT.select_region(xh, yh, 1) ) Imageh(i,j) = -1

               ENDIF

            ENDDO
         ENDDO

         RETURN

      ENDIF

c Coordinates differ so have to derive the mapping.

      nevents = NINT(wmsums(9))

c Set up the WMAP X or Y map axis inversions if necessary

      xsign = 1.0d0
      ysign = 1.0d0
      IF ( swmapx ) xsign = -1.0d0
      IF ( swmapy ) ysign = -1.0d0

c If there are no events accumulated then we can't do anything so
c return

      IF ( nevents .EQ. 0 ) RETURN

c We will need the centers of the image and WMAP arrays

      wfxf0 = (fbound(3)+fbound(1))/2.d0
      wfyf0 = (fbound(4)+fbound(2))/2.d0
      wfxh0 = (hbound(3)+hbound(1))/2.d0
      wfyh0 = (hbound(4)+hbound(2))/2.d0

c We assume that the image and WMAP coordinate systems are related by
c  xf - wfxf0 - xoff =  xsign*(xh-wfxh0)*cos(wftheta) + ysign*(yh-wfyh0)*sin(wftheta)
c  yf - wfyf0 - yoff = -xsign*(xh-wfxh0)*sin(wftheta) + ysign*(yh-wfyh0)*cos(wftheta)

      f1 = ysign*Wmsums(6) - xsign*Wmsums(7) 
     &         - Wmsums(1)*ysign*Wmsums(4)/nevents
     &         + Wmsums(2)*xsign*Wmsums(3)/nevents
      f2 = xsign*Wmsums(5) + ysign*Wmsums(8) 
     &         - Wmsums(1)*xsign*Wmsums(3)/nevents
     &         - Wmsums(2)*ysign*Wmsums(4)/nevents

      IF ( f1 .EQ. 0. .AND. f2 .EQ. 0. ) THEN
         wftheta = 0.
      ELSE
         wftheta = atan2(f1, f2)
      ENDIF
      cwftheta = cos(wftheta)
      swftheta = sin(wftheta)

      xoff = (Wmsums(1) - xsign*Wmsums(3)*cwftheta 
     &                  - ysign*Wmsums(4)*swftheta)/nevents
      yoff = (Wmsums(2) + xsign*Wmsums(3)*swftheta 
     &                  - ysign*Wmsums(4)*cwftheta)/nevents

c Now loop round WMAP bins. Calculate the pixel position for the columns 
c used for the WMAP and place in xh, yh

      wfxf0 = wfxf0 + xoff
      wfyf0 = wfyf0 + yoff

      qwarn = .FALSE.
      DO j = 1, Imszh2
         DO i = 1, Imszh1

c If the center of the WMAP bin falls outside the region then mark with
c Imageh = -1

            xh = (i-1)*Binh + hbbox(1) + 0.5*(Binh-1) - wfxh0
            yh = (j-1)*Binh + hbbox(2) + 0.5*(Binh-1) - wfyh0
            
c Convert to image coordinates

            xf = wfxf0 + xsign * xh * cwftheta + 
     &           ysign * yh * swftheta
            yf = wfyf0 - xsign * xh * swftheta + 
     &           ysign * yh * cwftheta

c If this bin is not within the selected region set to -1 and write a warning
c if necessary

            IF ( .NOT.select_region(xf, yf, 1) ) THEN
               IF ( Imageh(i,j) .NE. 0 .AND. .NOT.qwarn ) THEN
                  WRITE(contxt, '(a,i6,i6)') 
     &        'Warning: setting non-zero WMAP bin to -1 at ', i, j
                  CALL xwrite(contxt, 25)
                  qwarn = .TRUE.
               ENDIF
               Imageh(i,j) = -1
            ENDIF

         ENDDO
      ENDDO

      RETURN
      END

