c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPRITROI(MAP,MAPTYP,MAPDOC,mapscratch)
C
C
C  $Id: mapritro.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*     Effect: The MAP map is written to fits for only the ROI
c		mapscratch is used internally for storage
c
c-----------------------------------------------------------------------
c     	Subroutine Argument Desriptions
c	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^	
c       REAL   map data matrix to write
c       CHARACTER 	MAPTYP*4		MAPTYP
c 	character(70) 	MAPDOC(10)		MAPDOC array
c       REAL   mapscratch working (scratch) memory array
c
c-------------------------------------------------------------------------
C 	LIKE Version: 5.0 DELIVERED: October 1st 1994   JMATTOX
C+             UPDATED:    by  JRM
c
C=======================================================================
C  $Log: mapritro.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:39  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:01  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:18  jae
c Subroutine Module for like V5.00
c
C
c
c
C-----------------------------------------------------------------------
      SUBROUTINE MAPRITROI(MAP,MAPTYP,MAPDOC,mapscratch)

C  Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/roirep.copy'

      save
c
      character(80) id
      common /id/id
c
c-------------------------------------------------------------------------
      CHARACTER MAPTYP*4
      character(70) MAPDOC(10)
      REAL tmp_CTLORG(2),mapval,mapscratch(*),map(*)


      id = '$Id: mapritro.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      loc='MAPRITROI'


      if (ROIEND(1).gt.ROIORG(1)) then
         newMSZ1=(ROIEND(1)-ROIORG(1))/CTLSCL + 1
      else
         newMSZ1=(ROIEND(1)+360.-ROIORG(1))/CTLSCL + 1
      endif

      JL=ROIPRG(1)              ! begin longitude loop
      il=0
      
 100  CONTINUE
      il=il+1

      DO 80 JB=ROIPRG(2),ROIPND(2) ! do latitude row
         IMAP=il + newMSZ1*(JB-ROIPRG(2))
         mapscratch(IMAP)=MAPVAL(map,JL,JB,CTLMSZ1,CTLMSZ2)
 80   CONTINUE                  ! end latitude loop

      CALL MAPINC(JL,IEND,ROIPRG(1),ROIPND(1)) ! DO ALL COLUMNS IN THE ROI
      IF (SIGNAL.NE.' ') CALL ERROR(1,LOC)
      IF (IEND.NE.1) GOTO 100

C     Save CTL data
      tmp_CTLMSZ1=CTLMSZ1
      tmp_CTLMSZ2=CTLMSZ2
      tmp_CTLORG(1)=CTLORG(1)
      tmp_CTLORG(2)=CTLORG(2)

C     Reset CTL data for ROI     
      CTLMSZ1=newMSZ1
      CTLMSZ2=(ROIEND(2)-ROIORG(2))/CTLSCL + 1
      CTLORG(1)=ROIORG(1)
      CTLORG(2)=ROIORG(2)

      CALL MAPRIT(mapscratch,MAPTYP,MAPDOC,CTLMSZ1,CTLMSZ2)
      CALL ERROR(0,LOC)

C     Reset CTL data
      CTLMSZ1=tmp_CTLMSZ1
      CTLMSZ2=tmp_CTLMSZ2
      CTLORG(1)=tmp_CTLORG(1)
      CTLORG(2)=tmp_CTLORG(2)

      RETURN
      END
c
