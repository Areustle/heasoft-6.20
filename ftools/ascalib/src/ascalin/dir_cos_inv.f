
C******************************************************************************
C FUNCTION:
C      dir_cos_inv
C
C DESCRIPTION: 
C	converts a 'q' parameter to a inverse directional cosine matrix
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
C	dir_cosines_inv(q, z)
C
C ARGUMENTS:
C	q       - q parameter (input)
C      iz       - transposed directional cosine matrix (output)
C
C****************************************************************************** 
       subroutine dir_cos_inv(q, iz)
       double precision q(4), iz(3,3)
       
       iz(1,1) = q(1)*q(1) - q(2)*q(2) - q(3)*q(3) + q(4)*q(4)
       iz(2,1) = 2.0D0 * (q(1)*q(2) + q(3)*q(4))
       iz(3,1) = 2.0D0 * (q(1)*q(3) - q(2)*q(4))
       iz(1,2) = 2.0D0 * (q(1)*q(2) - q(3)*q(4))
       iz(2,2) = -q(1)*q(1) + q(2)*q(2) - q(3)*q(3) + q(4)*q(4)
       iz(3,2) = 2.0D0 * (q(2)*q(3) + q(1)*q(4))
       iz(1,3) = 2.0D0 * (q(1)*q(3) + q(2)*q(4))
       iz(2,3) = 2.0D0 * (q(2)*q(3) - q(1)*q(4))
       iz(3,3) = -q(1)*q(1) - q(2)*q(2) + q(3)*q(3) + q(4)*q(4)
       
       end
