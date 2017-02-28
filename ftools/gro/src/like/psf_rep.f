C       SUBROUTINE PSF_REP(char*2)
C
C
C  $Id: psf_rep.f,v 1.4 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++     effect: Resets the SCR_PARMS parameters to their settings
C++             prior to last LPO[N] command
C=======================================================================
C  LIKE Version: 5.0 DELIVERED: June 27, 1996, Programmer J.A. ESPOSITO
C+            Updated: by JAE
C=======================================================================
C  $Log: psf_rep.f,v $
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
C  Revision 1.2  2002/12/26 17:42:55  irby
C  Fix read/write statements (for f90 compatibility): add/remove commas and
C  fix lines over 72 chars.
C
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 1.4  1996/07/01  19:45:09  jae
c re-added include to cnfrep.copy (needed for
c logical unit numbers)
c
c Revision 1.3  1996/07/01  19:37:51  jae
c Removed all common blocks except likrep.copy
c to prevent error in compiling. NOTE: error occurs
c when psr_rep.f contains an include to common
c block file: cnfrep.copy
c
c Revision 1.2  1996/06/27  19:55:05  jae
c Fixed character which specifies spectral index reset
c to 'S' ( like command 'RS' )
c
c Revision 1.1  1996/06/27  19:49:20  jae
c Initial revision
c
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	SUBROUTINE PSF_REP(s2)

	INCLUDE  '../COMMON/cnfrep.copy'
	INCLUDE  '../COMMON/likrep.copy'

	save

	character(80) id
	common /id/id
	character s2*2,s1*10

	LOC='PSF_REP'


	call to_upper(s2(1:1))
	call to_upper(s2(2:2))

	goto 10
 9	write(*,*)' '
	write(*,*)'A read error of your input occured'
	write(*,*)'Please enter a number, the letter A or <cr> only'
	write(*,*)' '
 10	write(*,'("Enter PSF number or A for all (<cr> to abort):"$)')

	read(LU(12),'(a)') s1
	numcar = index(s1, ' ') - 1 
	if (numcar.eq.0) return

	CALL to_upper(s1(1:1))
	if (numcar.eq.1.and.s1(1:1).eq.'A') then
	   n1=1
	   n2=NSOURCE
	   goto 20

	elseif (numcar.ge.1) then
	   if (s1(1:1).eq.'A') then
	      n1=1
	      n2=NSOURCE
	      goto 20
	   endif

	   read(s1,*,err=9)n2
	   n1=n2
	   if (n1.lt.1.or.n1.gt.NSOURCE) then
	      write(*,'("Input number is out of Bounds: 1<N<",
     1                  i4)') NSOURCE
	      write(*,*)' '
	   endif
	endif
 20	continue

	if (n1.lt.1.or.n2.gt.NSOURCE) then
	   write(*,*)' '
	   write(*,*)'An input error has occured !'
	   write(*,'("Your input ->",a,"<-")') s1(1:numcar)
	   write(*,*)'results in an out of bounds condition'
	   write(*,*)' '
	   goto 9
	endif

	do j=n1,n2
	   if (s2.eq.'RA') then
	      SRC_PARMS(j,8)=SRC_PARMS(j,7)

	   elseif (s2.eq.'RP') then
	      SRC_PARMS(j,1)=SRC_PARMS(j,9)
	      SRC_PARMS(j,2)=SRC_PARMS(j,10)

	   elseif (s2.eq.'RC') then
	      SRC_PARMS(j,3)=SRC_PARMS(j,11)

	   elseif (s2.eq.'RS') then
	      SRC_PARMS(j,4)=SRC_PARMS(j,12)

	   elseif (s2.eq.'RF') then
	      SRC_PARMS(j,1)=SRC_PARMS(j,9)
	      SRC_PARMS(j,2)=SRC_PARMS(j,10)
	      SRC_PARMS(j,3)=SRC_PARMS(j,11)
	      SRC_PARMS(j,4)=SRC_PARMS(j,12)
	      SRC_PARMS(j,8)=SRC_PARMS(j,7)

	   else
	      write(*,'("Command ",a," not implemented")') s2
	      return
	   endif
	enddo

	return
	end


