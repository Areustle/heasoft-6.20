C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPMLT(MAP1,MAP2,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: mapmlt.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++    mapmlt = proc(map1,map2) modifies (map1)
C++           effect: map1 is multiplied by map2, result stored in map1.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:     by  JRM
C=======================================================================
C  $Log: mapmlt.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:38  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:09:01  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:51:57  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:12  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPMLT(MAP1,MAP2,IMAPWIDTH,IMAPHEIGHT)
C  Common blocks used:
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/errrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
C
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
      REAL MAP1(IMAPWIDTH,IMAPHEIGHT),MAP2(IMAPWIDTH,IMAPHEIGHT)

      id = '$Id: mapmlt.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='MAPMLT'

      if (jae_mapmlt) write(*,'("In routine ",a)') LOC

C     Step through bins:
      DO 10 J = 1,CTLMSZ2
         DO 10 I = 1,CTLMSZ1
            MAP1(I,J) = MAP1(I,J) * MAP2(I,J)
 10      CONTINUE
         
         RETURN
         END
c
