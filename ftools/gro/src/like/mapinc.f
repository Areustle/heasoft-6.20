C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPINC(IL,END,ROIPRG,ROIPND)
C
C
C  $Id: mapinc.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*     MAPINC = PROC(IL;ROI) RETURNS (IL,END)
C*            EFFECT: IL IS INCREMENTED, IF THE ROI WRAPS AROUND, SO
C*                   WILL IL. END=1 SIGNALS THE COMPLETION OF ROI.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:     by  JRM
C=======================================================================
C%   Changes
c 11/12/92, JRM: put ROIPRG,ROIPND in call list
C  $Log: mapinc.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:38  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:55  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:07  jae
c Subroutine Module for like V5.00
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPINC(IL,END,ROIPRG,ROIPND)
C  Common blocks used:
ccc      INCLUDE  'COMMON/roirep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
C
C     Arguments
      INTEGER IL,
C     longitude of pixel
     &     END,
c     1  indicates end
     &     ROIPRG,ROIPND
c     ROI longitude edges
C

      id = '$Id: mapinc.f,v 1.2 2013/05/21 19:08:26 irby Exp $'

      IF (IL.GT.CTLMSZ1.OR.IL.LT.1) THEN
         WRITE(SIGMSG,
     &        '('' MAPINC: longitude out of range, il='',i8)')il
         SIGNAL = 'I'
         RETURN
      ENDIF

      END=0

      IL = IL + 1

      IF (ROIPRG.LE.ROIPND) THEN
C     ROI DOES NOT WRAP AROUND
         IF (IL.LE.ROIPND) GOTO 100
      ELSE
C     ROI DOES WRAP AROUND
         IF (IL.GT.ROIPND+1) THEN
C     MAP DISCONTINUITY HAS NOT YET BEEN CROSSED
            IF (IL.LE.CTLMSZ1) THEN
               GOTO 100
            ELSE
C     OK - TIME TO CROSS THE MAP DISCONTINUITY
               IL=1
               GOTO 100
            ENDIF
         ELSE
C     MAP DISCONTINUITY HAS BEEN CROSSED
            IF(IL.LE.ROIPND) GOTO 100
         ENDIF
      ENDIF

      END=1
 100  CONTINUE
      SIGNAL = ' '

      RETURN
      END
c
