C*******************************************************************************
C SUBROUTINE:
C     xtepnt_xyz
C
C DESCRIPTION:
C     computes from the attitude matrix the cartesian (inertial)
C     coordinates of each of the three spacecraft axes.
C     
C AUTHOR:
C     Tod Strohmayer
C
C MODIFICATION HISTORY:
C
C NOTES:
C     coor contains the x,y,z three spacecraft axes. 
C     coor(1,*) = X axis, for 2nd index, 1 = x, 2 = y, 3 = z
C     coor(2,*) = Y axis      "
C     coor(3,*) = Z axis      "
C     (x,y,z) is a unit vector
C
C USEAGE:
C     call xtepnt_xyz(att, coor)
C     
C ARGUMENTS:
C     att(3,3)     - attitude matrix
C     coor(3,3)    - output x,y,z intertial coordinates of s/c axes
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C ***************************************************************************

      subroutine xtepnt_xyz(att,coor)
      real att(3,3),coor(3,3)
      integer i
c
      do 1 i=1,3
        coor(i,1)=att(i,1)
        coor(i,2)=att(i,2)
        coor(i,3)=att(i,3)
1     continue
      return
      end
      
