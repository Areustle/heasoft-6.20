c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C	SUBROUTINE MAPDIVROI(AJMAP,BJMAP,NAJ1,NAJ2)
C
C
C  $Id: mapdivro.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C
C	Purpose: Divides map A by map B over region of interest(ROI) 
c
c	Effect: Divides map AJMAP by map BJMAP and places result in 
c		map AJMAP.  The number of bins which define theROI 
c		longitude(RA) and Latitude(Dec)	distance must be equal
c		to NAJ1 and NAJ2 respectively.  If Bmap is zero in a
c		particular bin then zero is returned in the respective
c		AJMAP bin.
c
c	Return: AJMAP is returned containing the ratio of the original
c		AJMAP by BJMAP.
C
c	Programmer: J.A.Espsoito Delivered 28 AUG 1995
C
C  $Log: mapdivro.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2005/08/26 19:36:35  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.1  2002/04/16 20:27:38  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:50  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:02  jae
c Subroutine Module for like V5.00
c
C
	SUBROUTINE MAPDIVROI(AJMAP,BJMAP,NAJ1,NAJ2)

	character(80) id
	common /id/id
	real AJMAP(NAJ1,NAJ2)
	include '../COMMON/ctlrep.copy'
        INCLUDE '../COMMON/errrep.copy'
	include '../COMMON/roirep.copy'
	include '../COMMON/cnfrep.copy'

        save
c
	logical flag1, flag2

	id = '$Id: mapdivro.f,v 1.3 2013/05/21 19:08:26 irby Exp $'

	flag1=.false.
	flag2=.false.
	LOC='MAPCPROI'
	flag1=.true.
c
	if (NAJ2.lt.(ROIPND(2)-ROIPRG(2)+1) .or.
     1      NAJ1.lt.(ROIPND(1)-ROIPRG(1)+1)) then
	   flag1=.false.
	endif

	if (.not.flag1) then
	   SIGNAL='A'
	   SIGMSG='MAP arrays are sized smaller than ROI'
	   call error(0,LOC)
	   SIGNAL='A'
	   return
	endif

	do J=ROIPRG(1),ROIPND(1)
	   do K=ROIPRG(2),ROIPND(2)
	      J1=J-ROIPRG(1)+1
	      K1=K-ROIPRG(2)+1
	      xval=MAPVAL(BJMAP,J,K,CTLMSZ1,CTLMSZ2)
	      if (xval.ne.0) then
		 AJMAP(J1,K1)=AJMAP(J1,K1)/xval
	      else
		 AJMAP(J1,K1)=0
	      endif
	   enddo
	enddo

	return
	end
c
