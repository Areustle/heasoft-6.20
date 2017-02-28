C=======================================================================
C	SUBROUTINE PSFREPLACE(Npsf)
C
C
C  $Id: psfrepla.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++     effect: Exchange parameters between the active PSF and 
c	PSF number Npsf in the other PSF list.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C  $Log: psfrepla.f,v $
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
C  Revision 1.1  2002/04/16 20:27:42  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:31  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:50  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	SUBROUTINE PSFREPLACE(Npsf)

C	Common blocks used:
	INCLUDE  '../COMMON/likrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'
	INCLUDE  '../COMMON/ctlrep.copy'
	INCLUDE  '../COMMON/bmprep.copy'
        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/psmrep.copy'

        save

	character(80) id
	common /id/id
	CHARACTER Name_temp*18

	id = '$Id: psfrepla.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
        LOC='PSFREP'


	if (Npsf.le.0.or.Npsf.gt.NSOURCE) then
	   signal='N'
	   write(sigmsg,'("Sorry, source",I4," does not exist.")')Npsf
	   return
	endif

	if (abs(Counts).gt.1.e-3) then
c       Add current PSF
	   call PSFADD(Counts,bmap,CTLMSZ1,CTLMSZ2)
	endif
c	
	srcL_temp=srcL
	srcL=SRC_PARMS(Npsf,1)
	SRC_PARMS(Npsf,1)=srcL_temp

	srcB_temp=srcB
	srcB=SRC_PARMS(Npsf,2)
	SRC_PARMS(Npsf,2)=srcB_temp

	call pixel_select(.false.,' ')
	call error(1,loc)

	Counts_temp=Counts
	Counts=SRC_PARMS(Npsf,3)
	SRC_PARMS(Npsf,3)=Counts_temp
	
	gamma_temp=gamma
	gamma=SRC_PARMS(Npsf,4)
	SRC_PARMS(Npsf,4)=gamma_temp

	if (abs(gamma_temp-gamma).gt.0.001) then ! change spectral index
	   CALL PSMGET
           CALL ERROR(1,LOC)
	endif
	
	Name_temp=srcN
	srcN=SRC_NAMES(Npsf)
	SRC_NAMES(Npsf)=Name_temp

	if (abs(Counts).gt.1.e-3) then
c       Subtract previous PSF
	   call PSFADD(-Counts,bmap,CTLMSZ1,CTLMSZ2)
	endif

c	Rebuild every 1000 times because of numerical inprecision
	Npsfrep=Npsfrep+1
	if (Npsfrep.gt.1000) then
	   print *,
     1       'Rebuild other PSF map from list to avoid',
     2       ' avoid numerical inprecision.'
	   call psfbld
	   Npsfrep=0
	endif

	LikTotaled=.false.
	RETURN
	END
c
