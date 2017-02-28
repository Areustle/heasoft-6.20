      function ccminfun(vec,pdim,npts,xref,yref,xskew,yskew)
c
c  Coordinate correction
c  Function to give to ccamoeba for minimizing
c
      integer pdim, npts
      real xref(pdim), yref(pdim), xskew(pdim), yskew(pdim)
c
c   I  vec     (r) Variables: delx,dely,theta
c   I  pdim    (i) Dimension of x/yref, x/yskew
c   I  npts    (i) Number of points in x/yref, x/yskew
c   I  x/yref  (r) Point in reference coordinates
c   I  x/yskew (r) Point in skewed coordinates
c
      real ccminfun, vec(3)
      real*8 xres, yres, delx, dely, theta, x, y, tot

      delx = vec(1)
      dely = vec(2)
      theta = vec(3)

      tot = 0.d0

      do i = 1, npts
         x = (xskew(i)*cos(theta)) - (yskew(i)*sin(theta)) + delx
         y = (xskew(i)*sin(theta)) + (yskew(i)*cos(theta)) + dely

         xres = (xref(i)-x)
         yres = (yref(i)-y)

         tot = tot + xres**2 + yres**2
      enddo

      ccminfun = tot

      return
      end
