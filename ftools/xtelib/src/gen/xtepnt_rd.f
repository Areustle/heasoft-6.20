C*******************************************************************************
C SUBROUTINE:
C     xtepnt_rd
C
C DESCRIPTION:
C     computes from the attitude matrix the ra and dec (inertial)
C     coordinates of each of the three spacecraft axes.
C     
C AUTHOR:
C     Tod Strohmayer
C
C MODIFICATION HISTORY:
C	May 9, 1995	Original
C	June 15, 1995	Revision
C
C NOTES:
C     coor(1,*) = X axis, for 2nd index, 1 = ra, 2 = dec
C     coor(2,*) = Y axis      "
C     coor(3,*) = Z axis      "
C     output has units of degrees for both ra and dec
C
C USEAGE:
C     call xtepnt_rd(att, coor)
C     
C ARGUMENTS:
C     att(3,3)     - attitude matrix
C     coor(3,2)    - output ra & dec intertial coordinates of s/c axes
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C ***************************************************************************
      
      subroutine xtepnt_rd(att,coor)
      real att(3,3),coor(3,2)
      real pi, p2, crd, ay, ax, bx, by
      real phi
      integer i

c
      pi=3.141592653589793
      p2=2.0*pi
      crd=180.0/pi

      phi=0.0
c
c     do dec coordinate of s/c axes
c
      do 1 i=1,3
        coor(i,2)=asin(att(i,3))
        coor(i,2)=crd*coor(i,2)
1     continue
c
c    do ra coordinate of s/c axes
c
      do 2 i=1,3
       ay=att(i,2)
       ax=att(i,1)
       bx=abs(ax)           
       by=abs(ay)             
          if (ax.eq.0.0) then
            if (ay.gt.0.0) then 
               phi=pi/2.0
            end if
            if (ay.lt.0.0) then
               phi=(3.0*pi)/2.0
            end if
          end if
c         
          if ((ax.gt.0.0).and.(ay.ge.0.0)) then
            phi=atan(by/bx)                       
          end if
          if ((ax.lt.0.0).and.(ay.ge.0.0)) then
            phi=pi-atan(by/bx)                    
          end if
          if ((ax.lt.0.0).and.(ay.le.0.0)) then
            phi=pi+atan(by/bx)                   
          end if
          if ((ax.gt.0.0).and.(ay.le.0.0)) then
            phi=p2-atan(by/bx)                    
          end if
          phi=phi*crd
          coor(i,1)=phi
2     continue
      return
      end
