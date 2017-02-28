*+degrad
       subroutine degrad(ra,dec,rar,decr)
c
c convert degrees to radians 
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
       rar=ra*qi
       decr=dec*qi

       return
       end
