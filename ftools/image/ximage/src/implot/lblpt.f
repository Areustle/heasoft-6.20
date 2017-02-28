      subroutine LBLPT(xpt, ypt, symcsize, spaces, angle, just, text)

      implicit none
c
c  Labels a plotted point with text
c
c  I  xpt,ypt  (r)  Location of point
c  I  symcsize (r)  Symbol character size
c  I  spaces   (r)  Spacing in characters between pt. and text
c  I  angle    (r)  Angle of text
c  I  just     (c)  Justification of text (L,C,R)
c  I  text     (c)  Label text
c
      real*4 xpt, ypt, symcsize, spaces, angle
      character*(*) just, text
c
c  Local variables
c
      REAL*8 PI
      PARAMETER(PI=3.141592654)
      character(1) justflag
      real*4 svsize, symxch, symych, xch, ych
      real*4 spacing, angspc, offset, xoff, yoff, rjust
      real*4 xlb, ylb

c  Initialize to avoid warning
      angspc = 0.
      xoff = 0.
      yoff = 0.
c  --
      rjust = 0.0

      justflag = just(1:1)
      call UPC(justflag)
      call PGQCH(svsize)
      if ( symcsize.gt.0 ) call PGSCH(symcsize)
      call PGQCS(4,symxch,symych)
      call PGSCH(svsize)
      call PGQCS(4,xch,ych)

      spacing = symxch/3. + xch/3.*spaces
       
      if ( justflag.eq.'L' ) then
         rjust = 0.0
         offset = xch/3.
         angspc = angle*PI/180.
         xoff = offset*SIN(angspc)
         yoff = - offset*COS(angspc)
      else if ( just.eq.'R' ) then
         rjust = 1.0
         offset = xch/3.
         angspc = (angle+180)*PI/180.
         xoff = - offset*SIN(angspc)
         yoff = offset*COS(angspc)
      else if ( just.eq.'C' ) then
         rjust = 0.5
         angspc = (angle+90)*PI/180.
         xoff = 0.
         yoff = 0.
      endif
      xlb = xpt + spacing*COS(angspc) + xoff
      ylb = ypt + spacing*SIN(angspc) + yoff

      CALL PGPTXT(xlb,ylb,angle,rjust,text)
      
      RETURN
      END
