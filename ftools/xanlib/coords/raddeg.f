*+raddeg
       subroutine raddeg(rar,decr,ra,dec)
c
c convert radians to degrees 
c
c
c ra and dec are double precision
c
c author NEW 15-oct-1992
c
*- version 1.0

       include 'dpi.inc'
       real*8 ra, dec, rar, decr
c
c
       ra=rar/qi
       dec=decr/qi

       return
       end
