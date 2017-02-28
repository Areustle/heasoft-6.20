c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE PSMCOR(I,J,L,B)
C
C
C  $Id: psmcor.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*  EGRET Point Source Analysis System (SOURCE)
C*   Spec:
C*     PSMcor = proc(int:I;J;PSFmatrix) returns(real:L;B)
C*              signals('O': point outside region of map;
C*                      'E': coordinate not possible)
C*            effect: the relative coordinates L and B in degrees
C*               corresponding to bin indices I and J are calculated
C***********************************************************************
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c       INTEGER I,J  pixel coords in PSF
c       REAL L,B  longitude and latitude, in degrees, from PSF AXIS.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED: 05 Jun 1991    by  HRM
C=======================================================================
C  $Log: psmcor.f,v $
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
c Revision 5.0  1996/02/13  21:55:53  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE PSMCOR(I,J,L,B)

C  Common blocks used:
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save
C
      character(80) id
      common /id/id
      INTEGER I,J
      REAL L,B

      id = '$Id: psmcor.f,v 1.2 2013/05/21 19:08:26 irby Exp $'


C     Argument Testing:
      IF ((I.GT.PSFSZ).OR.(J.GT.PSFSZ)
     &     .OR.(I.LE.0).OR.(J.LE.0)) THEN
         SIGNAL = 'O'
         RETURN
      ENDIF
C
      L = (I - PSFORG(1))* PSFSCL
      B = (J - PSFORG(2))* PSFSCL
C
      RETURN
      END
c
