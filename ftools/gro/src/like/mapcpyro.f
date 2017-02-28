c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C	SUBROUTINE MAPCPYROI(AJMAP,BJMAP,NAJ1,NAJ2,NBJ1,NBJ2)
C
C
C  $Id: mapcpyro.f,v 1.4 2013/05/21 19:08:26 irby Exp $
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
c
C
C
C  $Log: mapcpyro.f,v $
C  Revision 1.4  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2005/08/26 19:36:35  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.2  2002/04/18 19:34:10  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:37  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:49  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:59  jae
c Subroutine Module for like V5.00
c
C

	SUBROUTINE MAPCPYROI(AJMAP,BJMAP,NAJ1,NAJ2,NBJ1,NBJ2)
	character(80) id
	common /id/id

	real AJMAP(NAJ1,NAJ2),BJMAP(NBJ1,NBJ2)
	include '../COMMON/ctlrep.copy'
        INCLUDE '../COMMON/errrep.copy'
	include '../COMMON/roirep.copy'
	include '../COMMON/cnfrep.copy'

        save

	character input,moreinput*50
	logical flag1,flag2
c
	id = '$Id: mapcpyro.f,v 1.4 2013/05/21 19:08:26 irby Exp $'

	input=' '
	moreinput=' '
	signal=' '
	flag1=.false.
	flag2=.false.
	LOC='MAPCPROI'
c
	if (NAJ2.ge.ROIPND(2).and.NAJ1.ge.ROIPND(1)) then
	   flag1=.true.
	elseif (NBJ2.ge.ROIPND(2).and.NBJ1.ge.ROIPND(1)) then
	   flag1=.true.
	   flag2=.true.
	endif

	if (.not.flag1) then
	   SIGNAL='M'
	   input=signal
	   SIGMSG='MAP arrays are defined outside the ROI'
	   moreinput=sigmsg
	endif
c
	if (flag2) then
	   if (NAJ1.lt.(ROIPND(1)-ROIPRG(1)+1).or.
     1	       NAJ2.lt.(ROIPND(2)-ROIPRG(2)+1)) then
	      SIGNAL='A'
	      input=signal
	      SIGMSG='AJMAP array is smaller than the ROI'
	      moreinput=sigmsg
	   endif
	else
	   if (NBJ1.lt.(ROIPND(1)-ROIPRG(1)+1).or.
     1	       NBJ2.lt.(ROIPND(2)-ROIPRG(2)+1)) then
	      SIGNAL='B'
	      input=signal
	      SIGMSG='BJMAP array is smaller than the ROI'
	      moreinput=sigmsg
	   endif
	endif
c       
c
	do J=ROIPRG(1),ROIPND(1)
	   do K=ROIPRG(2),ROIPND(2)
	      J1=J-ROIPRG(1)+1
	      K1=K-ROIPRG(2)+1
	      if (flag2) then
		 AJMAP(J1,K1)=BJMAP(J,K)
	      else
		 AJMAP(J,K)=BJMAP(J1,K1)
	      endif
	   enddo
	enddo

	return
	end
c
