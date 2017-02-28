C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE ROISET(L1,B1,L2,B2,ROIORG,ROIEND,ROIPRG,ROIPND)
C
C
C  $Id: roiset.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++    effect: the Region of Interest (ROI) is adjusted by the user
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c      REAL L1,B1,L2,B2       	coords of corners of ROI
c      REAL ROIORG(2)         	coords of pixel at beginning of ROI
c      REAL ROIEND(2)		coords of pixel at end of ROI
c      INTEGER ROIPRG(2)		pixel indices of ROIORG in MAP
c      INTEGER ROIPND(2)		pixel indices of ROIEND in MAP
c
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
c
C=======================================================================
C%   Changes:
c	1/31/94 JRM Call L_check instead of lnCOR.
c
C  $Log: roiset.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:43  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/03/22  22:32:42  jae
c repaired ctlorg and ctlend to reflect ctlscl/2
c >> offset of bin center
c
c Revision 5.1  1996/02/29  20:53:17  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:13  jae
c Subroutine Module for like V5.00
c
c
c-----------------------------------------------------------------------
      SUBROUTINE ROISET(L1,B1,L2,B2,ROIORG,ROIEND,ROIPRG,ROIPND)
C
C  Common blocks used:
C
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/errrep.copy'
      INCLUDE '../COMMON/ctlrep.copy'

      save
 
      character(80)  id
      character(1)   LOCSIG*1
      REAL          ROIORG(2)   !coords of pixel at beginning of ROI
      REAL          ROIEND(2)   !coords of pixel at end of ROI
      REAL          L1,B1,L2,B2
      INTEGER       ROIPRG(2)   !pixel indeces of ROIORG in MAP
      INTEGER       ROIPND(2)   !pixel indeces of ROIEND in MAP

      common /id/id

C coordinates of corners of ROI

      id = '$Id: roiset.f,v 1.2 2013/05/21 19:08:26 irby Exp $'


      LOC='ROISET  '
      LOCSIG=' '
      dt = CTLSCL/2.

      CALL L_check(L1)
      IF (SIGNAL.NE.' ') THEN
         L1=CTLORG(1)
         SIGNAL=' '
         LOCSIG='O'
      endif

      CALL L_check(L2)
      IF (SIGNAL.NE.' ') THEN
         L2=CTLEND(1)
         SIGNAL=' '
         LOCSIG='O'
      endif

      IF (B1.LT.CTLORG(2)-dt) then
         B1=CTLORG(2)
         LOCSIG='O'
      endif

      IF (B2.GT.CTLEND(2)+dt) then
         B2=CTLEND(2)
         LOCSIG='O'
      endif

      if ( b1.gt.b2.or.
     &     (.not.fullmap.and.l1.gt.l2)) then
         write(6,*)'ROI ends before it begins. Setting to full map'
         B1=CTLORG(2)
         B2=CTLEND(2)
         L1=CTLORG(1)
         L2=CTLEND(1)
      endif
C
C     Find pixel centers:
C
      CALL MAPIXL(L1,B1,IL,IB)
      CALL ERROR(1,LOC)
      CALL MAPCOR (IL,IB,L1,B1)
      CALL ERROR(1,LOC)
      CALL MAPIXL(L2,B2,IL,IB)
      CALL ERROR(1,LOC)
      CALL MAPCOR (IL,IB,L2,B2)
      CALL ERROR(1,LOC)
c
c     write(lu(1),'("ROISET: Setting ROI to",4f7.2)') L1,B1,L2,B2
c
      ROIORG(1) = L1
      ROIORG(2) = B1
      ROIEND(1) = L2
      ROIEND(2) = B2
C
C     Record the pixel values at the extremes of the ROI
C
      CALL MAPIXL(ROIORG(1),ROIORG(2),ROIPRG(1),ROIPRG(2))
      CALL ERROR(1,LOC)
      CALL MAPIXL(ROIEND(1),ROIEND(2),ROIPND(1),ROIPND(2))
      CALL ERROR(1,LOC)
C
      SIGNAL = ' '
      if (LOCSIG.eq.'O') then
         SIGNAL='O'
	 WRITE(SIGMSG,*)
     &        'Requested ROI not entirely within the map. Border(s) ',
     *        'adjstd.'
      endif

      RETURN
      END
