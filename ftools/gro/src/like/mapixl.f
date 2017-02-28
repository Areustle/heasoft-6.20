C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPIXL(L,B,I,J)
C
C
C  $Id: mapixl.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*              effect: the pixel coordinates I & J
C			corresponding to position L & B
C*                      are calculated using CTLORG and CTLSCL
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:     by  JRM
C=======================================================================
C  $Log: mapixl.f,v $
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
c Revision 5.0  1996/02/13  21:55:09  jae
c Subroutine Module for like V5.00
c
C%   Changes:
c	1/31/94 JRM Call L_check instead of lnCOR.
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPIXL(L,B,I,J)
C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
      CHARACTER sigbuf*59
C
C     Arguments:
      REAL L,B
C     longitude and latitude, in degrees
      INTEGER I,J
C     pixel coords in MAP
C     
      id = '$Id: mapixl.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='MAPIXL'

      if (jae_mapixl) write(6,*)'In routine MAPIXL'

      xL=L
      CALL L_CHECK(xL)
      IF (SIGNAL.NE.' ') THEN
         sigbuf=SIGMSG(1:59)
         WRITE(SIGMSG,18)sigbuf
 18      FORMAT('MAPIXL finds:',a)
         SIGNAL='O'
         return
      endif
C
      I = 1  + INT(0.500+(xL-CTLORG(1))/CTLSCL)
      J = 1  + INT(0.500+(B-CTLORG(2))/CTLSCL)
C
C     Test for results onscale:
      IF ((I.GT.CTLMSZ1).OR.(J.GT.CTLMSZ2)
     &     .OR.(I.LE.0).OR.(J.LE.0)) THEN
         SIGNAL='O'
         WRITE(SIGMSG,19) L,B,I,J
 19      FORMAT('MAPIXL error:',2(1X,1PE10.3),2(1X,I5)
     1        ,      ' outside map.')
         if (jae_mapixl) then
            write(6,'(" ")')
            write(6,'(" CTLORG: ",2(1x,F7.2))')CTLORG(1),CTLORG(2)
            write(6,'(" CTLSIZ: ",2(1x,I4))')CTLMSZ1,CTLMSZ2
            write(6,'(" CTLSCL: ",E12.5)')CTLSCL
            write(6,'("position: ",2(1x,1PE10.3))')xl,B
            write(6,'(" ")')
         endif

         RETURN
      END IF
C
      SIGNAL = ' '

      RETURN
      END
c
