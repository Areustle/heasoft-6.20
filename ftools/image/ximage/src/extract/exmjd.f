* convert input mjd to jan 1 that same year
      SUBROUTINE EXMJD(Mjd1)
      implicit none
 
      REAL*8 Mjd1
      INTEGER*4 iy , im , id , j
      REAL*8 fd
 
* call slalib
 
      CALL SLA_DJCL(Mjd1,iy,im,id,fd,j)
      im = 1
      id = 1
      CALL SLA_CLDJ(iy,im,id,Mjd1,j)
 
      RETURN
      END
