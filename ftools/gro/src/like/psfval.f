C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      FUNCTION PSFVAL(R)
C
C
C  $Id: psfval.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*   POINT SOURCE ANALYSIS SYSTEM (SOURCE)
C*   Spec:
C*     PSFVAL = PROC(PSFARR,RADIUS)
C*              RETURNS(PSFVALUE)
C*              EFFECT: PSF value at radius relative to source direction
C*                      is derived from PSF array containing the
C*                      normalized PSF representation per SR
C***********************************************************************
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c	real R radius relative to source direction 
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED: 28 May 1991    by  HRM
C=======================================================================
C  $Log: psfval.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:42  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:32  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:52  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      FUNCTION PSFVAL(R,kk)

C  Common blocks used:
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save
      
      character(80) id
      common /id/id
C
C  VERSON WITH INTERPOLATION:
      id = '$Id: psfval.f,v 1.2 2013/05/21 19:08:26 irby Exp $'


      IF (R.LT.PSFINC/2) THEN
         PSFVAL=PSFARR(1)
      ELSE
         I1=(R-PSFINC/2)/PSFINC+1
         IF ((I1+2).LE.100) THEN
            RI1=I1*PSFINC-PSFINC/2
            RI2=RI1+PSFINC
            PSFVAL=(PSFARR(I1)*(R-RI2)-PSFARR(I1+1)*(R-RI1))/(RI1-RI2)
         ELSE
            PSFVAL=0
         ENDIF
      ENDIF

      RETURN
      END
c
