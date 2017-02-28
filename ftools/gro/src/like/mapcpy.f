C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPCPY(MAP1,MAP2,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: mapcpy.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*     MAPCPY = PROC(MAP1,MAP2) MODIFIES (MAP1)
C*            EFFECT: MAP2 IS COPIED TO MAP1.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:     by  JRM
C=======================================================================
C  $Log: mapcpy.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:37  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:47  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:58  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE MAPCPY(MAP1,MAP2,IMAPWIDTH,IMAPHEIGHT)

C     Common blocks used:
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
C
      REAL MAP1(IMAPWIDTH,IMAPHEIGHT),MAP2(IMAPWIDTH,IMAPHEIGHT)
C
      id = '$Id: mapcpy.f,v 1.2 2013/05/21 19:08:26 irby Exp $'


C     Step through bins:
      DO 10 J = 1,CTLMSZ2
         DO 10 I = 1,CTLMSZ1
            MAP1(I,J) = MAP2(I,J)
 10      CONTINUE
C     
         SIGNAL = ' '

         RETURN
         END
c
