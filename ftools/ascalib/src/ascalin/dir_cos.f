
C******************************************************************************
C FUNCTION:
C      dir_cos_inv
C
C DESCRIPTION: 
C	converts a 'q' parameter to a directional cosine matrix
C
C AUTHOR:  
C      Emily A. Greene
C	Hughes STX
C	March, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	dir_cosines(q, z)
C
C ARGUMENTS:
C	q       - q parameter (input)
C	z       - directional cosine matrix (output)
C
C****************************************************************************** 
       subroutine dir_cos(q, z)
       double precision q(4), z(3,3)
       
       z(1,1) = q(1)*q(1) - q(2)*q(2) - q(3)*q(3) + q(4)*q(4)
       z(1,2) = 2.0D0 * (q(1)*q(2) + q(3)*q(4))
       z(1,3) = 2.0D0 * (q(1)*q(3) - q(2)*q(4))
       z(2,1) = 2.0D0 * (q(1)*q(2) - q(3)*q(4))
       z(2,2) = -q(1)*q(1) + q(2)*q(2) - q(3)*q(3) + q(4)*q(4)
       z(2,3) = 2.0D0 * (q(2)*q(3) + q(1)*q(4))
       z(3,1) = 2.0D0 * (q(1)*q(3) + q(2)*q(4))
       z(3,2) = 2.0D0 * (q(2)*q(3) - q(1)*q(4))
       z(3,3) = -q(1)*q(1) - q(2)*q(2) + q(3)*q(3) + q(4)*q(4)
       
       end
