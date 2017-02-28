C
C   Set the overflow and underflow elements with max and min values.
C
  	Subroutine ouflow(img, nullflag, nelem, bitpix)

        double precision img(nelem)
        logical nullflag(nelem)
        integer bitpix
        double precision bmin, bmax
        parameter (bmin = 0, bmax = 255)
        double precision smin, smax
        parameter (smin = -32767, smax = 32767)
        double precision lmin, lmax
        parameter (lmin = -2147483647, lmax = 2147483647) 
        double precision rmax, rmin
        integer i
        
        rmax = 0.
        rmin = 0.

C       For the float or double images, do nothing.
        if(bitpix.lt.0) return

C       Get the max and minimum for the integer image.
        if(bitpix.eq.8) then
            rmax = bmax
            rmin = bmin
        endif
        if(bitpix.eq.16) then
            rmax = smax
            rmin = smin
        endif
        if(bitpix.eq.32) then
            rmax = lmax
            rmin = lmin
        endif 

C       Set the underflow and overflow values.
        do 4000 i = 1, nelem 
           if(nullflag(i)) goto 4000
           if(img(i).gt.rmax) then 
               img(i) = rmax
               goto 4000
           endif
           if(img(i).lt.rmin) then 
               img(i) = rmin
               goto 4000
           endif
4000    continue
        return
        end
