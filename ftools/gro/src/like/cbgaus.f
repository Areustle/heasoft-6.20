C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     Gaussian Function:
C      REAL FUNCTION CBGAUS(A,T)
C
C
C  $Id: cbgaus.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C
C
C  $Log: cbgaus.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:28  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  18:52:27  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:46:57  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:07  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      REAL FUNCTION CBGAUS(A,T)

      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      character(80) id
      common /id/id


      LOC='CBGAUS'
      id = '$Id: cbgaus.f,v 1.2 2013/05/21 19:08:25 irby Exp $'

      if (jae_cbgaus) write(*,'("In routine ",a)') LOC

      B = (A*A)/(T*T)
      IF (B.GT.7.) THEN
         CBGAUS = 0.

C     This is the 0.1% level
      ELSE
         CBGAUS = EXP(-B)
      END IF

      RETURN
      END
c
