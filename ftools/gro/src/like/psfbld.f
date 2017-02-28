c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C	SUBROUTINE PSFBLD
C
C
C  $Id: psfbld.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++  effect: Create other PSF map from SRC_PARMS in likrep common block.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C  $Log: psfbld.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:42  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:29  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:47  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	SUBROUTINE PSFBLD

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

	id = '$Id: psfbld.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
        LOC='PSFBLD'


	call MAPRST(bmap,CTLMSZ1,CTLMSZ2)

	if (NSOURCE.lt.1) RETURN
	if (verbose) write(lu(1),*)'Building other PSF array.'

	srcL_temp=srcL
	srcB_temp=srcB
	Counts_temp=Counts
	gamma_temp=gamma
	gamma_last=gamma

	do Npsf=1,NSOURCE
	   srcL=SRC_PARMS(Npsf,1)
	   srcB=SRC_PARMS(Npsf,2)
	   Counts=SRC_PARMS(Npsf,3)
	   gamma=SRC_PARMS(Npsf,4)
	   call pixel_select(.false.,' ')
	   call error(1,loc)
	   if (abs(gamma_last-gamma).gt.0.001) then ! change spectral index
              CALL PSMGET
	      CALL ERROR(1,LOC)
	      gamma_last=gamma
	   endif
	   call PSFADD(Counts,bmap,CTLMSZ1,CTLMSZ2)
	enddo

	srcL=srcL_temp
	srcB=srcB_temp
	Counts=Counts_temp
	gamma=gamma_temp
	call pixel_select(.false.,' ')
	call error(0,loc)

	if (abs(gamma_temp-gamma).gt.0.001) then ! change spectral index
	   CALL PSMGET
	   CALL ERROR(1,LOC)
	endif

	LikTotaled=.false.

	RETURN
	END
c
