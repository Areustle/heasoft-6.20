C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      REAL FUNCTION TOTMAP(MAPIN,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: totmap.f,v 1.2 2013/05/21 19:08:27 irby Exp $
C=======================================================================
C++    TOTMAP = proc(MAPIN) returns(SUM)
C++           effect: MAPIN total is returned.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:     by  JRM
C=======================================================================
C  $Log: totmap.f,v $
C  Revision 1.2  2013/05/21 19:08:27  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:45  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:54:13  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:58  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      REAL FUNCTION TOTMAP(MAPIN,IMAPWIDTH,IMAPHEIGHT)
C
C  Common blocks used:
C
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/errrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'

      save

      character(80) id
      REAL MAPIN(IMAPWIDTH,IMAPHEIGHT)

      common /id/id
C
      id = '$Id: totmap.f,v 1.2 2013/05/21 19:08:27 irby Exp $'
      SUM = 0.
C
C     Step through bins:
C
      DO 10 J = 1,CTLMSZ2
         DO 10 I = 1,CTLMSZ1
            SUM = SUM + MAPIN(I,J)
 10      CONTINUE

         SIGNAL = ' '
C     
C     Check the result:
C
         IF (SUM.EQ.0.OR.SUM.GE.1E25.OR.SUM.LE.-1E25) THEN
            WRITE(lu(1),'('' TOTMAP: MAP SUM IS SUSPECT: '',E8.2)') SUM
         END IF

         TOTMAP = SUM

         RETURN
         END
