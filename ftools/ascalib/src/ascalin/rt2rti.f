CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     function rt2rti (x, y, rt)
C     
C     rt, x, y(in):     input rise_time, X-position and Y-position.
C     rt2rti(out):            corrected rise time value.
C     
C     This subroutine carries out ASCA GIS Rise_Time --> Rise_Time_Invariance
C     conversion.  ASCA GIS has a position dependency of the Rise_Time for
C     a given event.  This subroutine carryes out the position dependency
C     correction.  The correction coefficients should be in the array rt_map,
C     which are to be read from fits format rise_time correction map, and
C     stored in the common area rt_map.
C     
C     Original program was written by Y. Ishisaki (Univ. Tokyo) in C.
C
C     93/07/26 Ken Ebisawa (ASCA GOF, NASA/GSFC) 
C
C     93/11/01 Modified for ASCALIN. Eric Gotthelf (ASCA GOF, NASA/GSFC) 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      integer function rt2rti(x, y, rtime)

      implicit none
      
      include 'asca_defs.inc'
      
      real x, y
      integer rtime

      include 'asca_common.inc'
      
      integer x0, y0
      real xp, yp, rt_inv, s, t

      if (rtime .eq. 0) then

         rt2rti = 0

      else

         xp = x + 0.5
         yp = y + 0.5
         
         x0 = max(min(int(xp), int(det_x_size-1)), 1) 
         y0 = max(min(int(yp), int(det_y_size-1)), 1) 
         
         s = xp - real(x0)
         t = yp - real(y0)
         
C     calculate rt_inv (rt invariant) by weight-averaging rt_map
C     values of the four surrounding pixels.
      
         rt_inv = rti_off - rt_map(x0  , y0  ) * (1-s) * (1-t)
         rt_inv = rt_inv  - rt_map(x0+1, y0  ) * s     * (1-t)
         rt_inv = rt_inv  - rt_map(x0  , y0+1) * (1-s) * t
         rt_inv = rt_inv  - rt_map(x0+1, y0+1) * s     * t
         
         rt2rti = int(float(rtime) * rt_scale + rt_off + rt_inv)         

c        rt2rti = int(float(rtime) + rt_inv / rt_scale )
         
      end if

      return

      end

