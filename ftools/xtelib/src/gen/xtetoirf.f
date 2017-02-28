C*******************************************************************************
C SUBROUTINE:
C     xtetoirf
C
C DESCRIPTION:
C     Uses the attitude matrix from xteamat to transform a unit vector
C      in inertial coordinates (RA and DEC) to XTE spacecraft coordinates,
C      or vice versa.
C     
C AUTHOR:
C     Tod Strohmayer
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USEAGE:
C     call xtetoirf(att, coorin, coorout, invert)
C     
C ARGUMENTS:
C     att(3,3)     - attitude matrix
C     coorin(3)    - input coordinates
C     coorout(3)   - output coordinates
C     invert       = 1, coorin treated as intertial coord & coorout as s/c
C                  = -1, coorin treated as s/c coord & coorout as inertial
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C ***************************************************************************

      subroutine xtetoirf (att,coorin,coorout,invert)
      integer invert
      real coorin(3),coorout(3),att(3,3)
      integer i,j
c
c  Do Transformation or inverse using att from xteamat
c
      if (invert.eq.1) then
        do 1 i=1,3
          coorout(i)=0.0
          do 2 j=1,3
            coorout(i)=coorout(i)+att(i,j)*coorin(j)
2         continue
1       continue
      end if
      if (invert.eq.-1) then
        do 3 i=1,3
          coorout(i)=0.0
          do 4 j=1,3
            coorout(i)=coorout(i)+att(j,i)*coorin(j)
4         continue
3       continue
      end if
c
      return
      end

