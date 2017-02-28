C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPMAX(MAP,ILMAX,IBMAX,MAXIMUM,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: mapmax.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++    MAPMAX = proc(MAP;ROI) returns(pixel coords:IL,IB)
C++           effect: IL,IB are the indeces of the MAP pixel within
C++                  the ROI which has the maximum value.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:     by  JRM
C=======================================================================
C  $Log: mapmax.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:38  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:56  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:10  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPMAX(MAP,ILMAX,IBMAX,MAXIMUM,IMAPWIDTH,IMAPHEIGHT)
C  Common blocks used:
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save
C
      character(80) id
      common /id/id
C     Arguments
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
      INTEGER ILMAX,IBMAX
C     longitude and latitude of result pixel
      REAL MAXIMUM
C     maximum value of map
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)
C     
C     Local Variables:
      INTEGER IL,IB
C     pixel coords of map
C

      id = '$Id: mapmax.f,v 1.2 2013/05/21 19:08:26 irby Exp $'

      ILMAX=0
      IBMAX=0
      MAXIMUM = -1e30
      IL = ROIPRG(1)

 200  CONTINUE
      DO 100 IB = ROIPRG(2),ROIPND(2)
         IF (MAP(IL,IB).GT.MAXIMUM) THEN
            ILMAX = IL
            IBMAX = IB
            MAXIMUM = MAP(IL,IB)
         ENDIF
 100  CONTINUE

      CALL MAPINC(IL,IEND,ROIPRG(1),ROIPND(1))
      CALL ERROR(1,LOC)

      IF (IEND.NE.1) GOTO 200
C     
      IF ((ILMAX.GT.0).AND.(IBMAX.GT.0)) THEN
         SIGNAL = ' '
      ELSE
         SIGNAL = 'E'
         WRITE(SIGMSG,'('' MAPMAX: No maximum exists'')')
      ENDIF
C
      RETURN
      END
c
