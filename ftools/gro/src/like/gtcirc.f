C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      FUNCTION GTCIRC(L1,B1,L2,B2)
C
C
C  $Id: gtcirc.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C*    GTCIRC:   PROC(l1,b1,l2,b2)   RETURNS(gtcirc)
C*    Gives shortest great circle distance between two points
C*    (l1,b1) and (l2,b2) on the celestial sphere. The spheri-
C*    cal trigonometric cosine rule is used. Angles are in degrees.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C
C%   Changes:
c 11/12/92, JRM: Restructure
c 2/16/93, JRM:  Correctly restructure
c
c
C  $Log: gtcirc.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:32  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:48:04  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:22  jae
c Subroutine Module for like V5.00
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      FUNCTION GTCIRC(L1,B1,L2,B2)

C     Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'

      save

      character(80) id
      common /id/id
      REAL L1,B1,L2,B2,DL,DB


      id = '$Id: gtcirc.f,v 1.2 2013/05/21 19:08:25 irby Exp $'

      DB=B2-B1
      DL=L2-L1
      COSB1=COS(B1*PI180)
      CDL=DL*COSB1
      
      IF ((ABS(CDL).LT.1.).AND.(ABS(DB).LT.1.)) THEN
         GTCIRC=SQRT(CDL**2+DB**2)
      ELSE
         GTCIRC=(SIN(B1*PI180)*SIN(B2*PI180)+
     &        COSB1*COS(B2*PI180)*COS(DL*PI180))
         if(ABS(GTCIRC).gt.0.999999999)then
            GTCIRC=0
         else
            GTCIRC=ACOS(GTCIRC)/PI180
         endif
      ENDIF

      RETURN
      END
c
