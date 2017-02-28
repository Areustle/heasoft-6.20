C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPRST(MAP,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: maprst.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*     MAPRST = PROC(MAP) MODIFIES(MAP)
C*            EFFECT: ALL PIXELS OF THE MAP ARE SET TO ZERO.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:     by  JRM
C=======================================================================
C  $Log: maprst.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:39  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:10:34  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:52:02  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:19  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPRST(MAP,IMAPWIDTH,IMAPHEIGHT)
C  Common blocks used:
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
C
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)
C

      id = '$Id: maprst.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='MAPRST'


      if (jae_maprst)write(*,'("In routine ",a)') LOC

C     reset all map values to zero:
      DO 10 I=1,CTLMSZ1
         DO 10 J=1,CTLMSZ2
            MAP(I,J)=0.
 10      CONTINUE
C
         RETURN
         END
c
