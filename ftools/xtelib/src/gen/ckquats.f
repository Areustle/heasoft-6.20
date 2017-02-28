C ********************************************************************
C SUBROUTINE:
C     ck_quats
C
C DESCRIPTION:
C     compute the jitter in degrees of an average quaternion and its std. dev
C
C AUTHOR:
C     James Lochner 10/95
C
C MODIFICATION HISTORY:
C  Feb 12, 1996 - Bug fix: convert ra & dec's into radians for computation
C                  of distance using radian arguments of cos & sin
C  Oct 17, 1996 - changed declaration of minra, mindec, maxra, maxdec
C                  and cdist from real*4 to double precision.
C NOTES:
C 
C USEAGE:
C     call ck_quats(avgquat,sdquat,jitter)
C
C ARGUMENTS:
C     avgquat    - array of the average quaternion values
C     sdquat     - array of the std dev of the quaternion values
C     jitter     - jitter, in degrees, of the pointing
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED SUBROUTINES:
C
C ******************************************************************

      SUBROUTINE ck_quats(avgquat, sdquat, jitter)

      double precision  sdquat(4)
      real avgquat(4), jitter

      double precision minra, mindec, maxra, maxdec
      double precision cdist
      real*4 att(3,3), coor(3,2), quat(4)
      real*4 pi, crd
      integer i

      pi = 3.141592653589793
      crd = 180.0/ pi 

C Compute the pointing using "maximum" position
      do i = 1,4
         quat(i) = avgquat(i) + sdquat(i)
      end do
      call xteamat(quat,att)
      call xtepnt_rd(att,coor)
      maxra = coor(1,1)
      maxdec = coor(1,2)
      
C Compute the pointing using the "minimum" position
      do i = 1,4
         quat(i) = avgquat(i) - sdquat(i)
      end do
      call xteamat(quat,att)
      call xtepnt_rd(att,coor)
      minra = coor(1,1)
      mindec = coor(1,2)

C Convert ra's and dec's to radians
      minra = minra / crd
      mindec = mindec / crd
      maxra = maxra / crd
      maxdec = maxdec / crd
      
C Compute the distance between the min and max positions
      cdist = cos(mindec) * cos(maxdec) * cos(minra - maxra)
     $     + sin(mindec) * sin(maxdec)
      jitter = acos(cdist) * crd

      return
      end








