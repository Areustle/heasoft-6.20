C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE GASBAD
C
C
C  $Id: gasbad.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C*     GASBAD = PROC(GAS,BMP) MODIFIES (GAS)
C*            EFFECT: COS-B instrumental background estimate is added
c		to the diffuse model
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED: 16 May 1991    by  JRM
C=======================================================================
C  $Log: gasbad.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:31  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:56  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:07  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE GASBAD

C     Common blocks used:
      INCLUDE '../COMMON/gasrep.copy'
      INCLUDE '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'

      save
C
      character(80) id
      common /id/id

      id = '$Id: gasbad.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='GASBAD'


C     Check that maps are of correct type:
      IF (GASTYP.NE.'GMAP') THEN
         SIGNAL = 'G'
         WRITE(SIGMSG,
     &        "(' GASBAD: Wrong gas map type ',A4)")GASTYP
         RETURN
      END IF

      IF (BMPTYP.NE.'BMAP') THEN
         SIGNAL = 'B'
         WRITE(SIGMSG,
     &        "(' GASBAD: Wrong Bmap type ',A4)")BMPTYP
         RETURN
      END IF
C
      CALL MAPSUM(GASMAP,BMAP,CTLMSZ1,CTLMSZ2)
      IF(SIGNAL.NE.' ') CALL ERROR(1,LOC)
C
      SIGNAL = ' '

      RETURN
      END
c
