C*******************************************************************************
C SUBROUTINE:
C     xtemat
C
C DESCRIPTION:
C     Constructs the 3x3 attitude matrix from a given quaternion. See Wertz
C     (Spacecraft Attitude Determination and Control) for more on quaternions
C     and attitude matrix.
C
C AUTHOR:
C     Tod Strohmayer
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USEAGE:
C     call xteamat(quat,att)
C
C ARGUMENTS:
C     quat(4)      - quaternions
C     att(3,3)     - attitude matrix
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C
C ****************************************************************************

      subroutine xteamat(quat,att)
      implicit real*4 (a-h,o-z) 
      real quat(4),att(3,3)
      real q11, q22, q33, q44
      real q12, q34, q13, q24, q23, q14
c     
c   Compute Matrix elements  
c
      q11=quat(1)*quat(1)
      q22=quat(2)*quat(2)
      q33=quat(3)*quat(3)
      q44=quat(4)*quat(4)
      q12=quat(1)*quat(2)
      q34=quat(3)*quat(4)
      q13=quat(1)*quat(3)
      q24=quat(2)*quat(4)
      q23=quat(2)*quat(3)
      q14=quat(1)*quat(4)
c
      att(1,1)=q11-q22-q33+q44
      att(1,2)=2.0*(q12+q34)
      att(1,3)=2.0*(q13-q24)
c
      att(2,1)=2.0*(q12-q34)
      att(2,2)=-q11+q22-q33+q44
      att(2,3)=2.0*(q23+q14) 
c
      att(3,1)=2.0*(q13+q24)
      att(3,2)=2.0*(q23-q14)
      att(3,3)=-q11-q22+q33+q44
      return
      end

