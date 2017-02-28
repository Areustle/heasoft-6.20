C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPCOR(IL,JB,GL,GB)
C
C
C  $Id: mapcor.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*     MAPCOR = PROC(PIXEL COORDINATES:IL,JB) RETURNS(GALACTIC COORDS
C*              GL,GB)
C*              SIGNALS('O': POINT OUTSIDE REGION OF MAP;
C*                      'E': COORDINATE NOT POSSIBLE)
C*            EFFECT: THE SKY COORDINATES CORRESPONDING TO IL AND JB
C*                    ARE CALCULATED USING CTLORG AND CTLSCL
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: mapcor.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:37  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/03/22  22:31:53  jae
c repaired ctlorg and ctlend to reflect ctlscl/2
c >> offset of bin center
c
c Revision 5.1  1996/02/29  20:51:47  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:57  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE MAPCOR(IL,JB,GL,GB)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
C
C     Arguments:
      REAL GL,GB
C     longitude and latitude, in degrees
      INTEGER IL,JB
C     pixel coords in MAP
C

      id = '$Id: mapcor.f,v 1.2 2013/05/21 19:08:26 irby Exp $'

      SIGNAL = ' '
      loc='MAPCOR'
      dt=CTLSCL/2.
C     
      GL = CTLORG(1) + (FLOAT(IL-1))*CTLSCL
      GB = CTLORG(2) + (FLOAT(JB-1))*CTLSCL
C
      IF (ABS(GB).GT.90) THEN
         SIGNAL = 'E'
         write(SIGMSG,'("MAPCOR:Latitude=",f5.1,
     &        " out of range")') GB
         call error(-1,loc)
      ENDIF
C
      IF ((GL.LT.CTLORG(1)-dt).OR.
     1     (GL.GT.CTLEND(1)+dt).OR.
     1     (GB.LT.CTLORG(2)-dt).OR.
     1     (GB.GT.CTLEND(2)+dt) ) THEN
         WRITE(SIGMSG,111) GL,GB
 111     FORMAT('MAPCOR: datapoint outside map: L = ',1PE12.3
     1        ,      ' B = ',1PE12.3)
         SIGNAL ='O'
      ENDIF
C
      RETURN
      END
c
