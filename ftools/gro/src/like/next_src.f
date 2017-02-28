C	SUBROUTINE next_src
C
C
C  $Id: next_src.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++            effect: the significance of the current source 
c	as an additional source is examined
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C%   Changes:
c  12-14-95 JAE: increased size of oldSRC_PARMS and oldSRC_NAMES from 500
c		 to 600.  This routine now replaces the older JRM V4.15
c		 routine next_src
c
C  $Log: next_src.f,v $
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
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:16  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:32  jae
c Subroutine Module for like V5.00
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	SUBROUTINE next_src

	INCLUDE  '../COMMON/likrep.copy'
	INCLUDE  '../COMMON/locpos.copy'
	INCLUDE  '../COMMON/ctlrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'
	INCLUDE  '../COMMON/bmprep.copy'

        save

	character input*50
	REAL oldSRC_PARMS(600,4) 
	character(18) testsrcN,oldSRC_NAMES(600)
	logical second_call
	data second_call/.false./

        LOC='next_src'
	report=.true.


	if (.not.second_call) then
	   write(6,'("Will test the significance of a source as ",
     1               "an additional source.")')
	   call pixel_select(.true.,'add. src position:  ')
	   
	   if (signal.ne.' ') return
	   
	   call srctest(.false.)
	   
	   if (signal.ne.' ') return
	   if (TS.lt.2.) then
	      print *, 'PSFTEST: Test statistic less than 2.',
     1                 ' No source here to test.'
	      return
	   endif

	   NSOURCE_old=NSOURCE
	   test_lnL=lnL
	   testL=srcL
	   testB=srcB
	   testsrcN=srcN
	   testC=Counts
	   second_call=.true.
	   print *
	   print *,'Now, optimize positions and fluxes for a model',
     1             ' without this source. Put all sources in the other',
     2             ' PSF model. Issue LN command a second time.'
	   
c	save psf data
	   do n=1,NSOURCE
	      oldSRC_PARMS(n,1)=SRC_PARMS(n,1)
	      oldSRC_PARMS(n,2)=SRC_PARMS(n,2)
	      oldSRC_PARMS(n,3)=SRC_PARMS(n,3)
	      oldSRC_PARMS(n,4)=SRC_PARMS(n,4)
	      oldSRC_NAMES(n)=SRC_NAMES(n)
	   enddo
	   
	   write(6,'("Sources within 10 degrees will",
     1               " be marked active (cr for OK) ")')
	   READ(LU(12),'(A)')input

	   if (input.eq.' ') then
	      do n=1,NSOURCE
		 dist=gtcirc(testL,testB,SRC_PARMS(n,1),SRC_PARMS(n,2))
		 if (dist.le.10.) then
c       perhaps this source should be active
		    SRC_PARMS(n,8)=1.0
		 else
		    SRC_PARMS(n,8)=.0
		 endif
	      enddo
	   endif

	else
c	second call execution.
	   write(6,'("This is the second step of the LN command. ",
     1           "You should have just run it for the new source.")')
	   write(6,'("Now obtaining optimal Gmult & Gbias without ",
     1           "new source, and Counts=0.")')
	   report=.true.
	   
	   Counts=0.
	   srcL=testL
	   srcB=testB
	   srcN=testsrcN
	   call pixel_select(.false.,' ')
	   call error(1,loc)       
	   call gasbias(.false.)

	   if (signal.ne.' ') return

	   Counts=testC

	   twovs1=-2.*(test_lnL-lnL)
	   print *,'The additional source test statistic is',twovs1
	   
	   if (twovs1.gt.12.) then
	      print *,'The additional source is indicated with',
     1                ' at least 99% confidence.'
	      print *,'Should original other PSF values be restored',
     1                ' (cr for yes)'    
	      READ(LU(12),'(A)')input

	      if (input.eq.' ') then
		 NSOURCE=NSOURCE_old
		 do n=1,NSOURCE
		    SRC_PARMS(n,1)=oldSRC_PARMS(n,1)
		    SRC_PARMS(n,2)=oldSRC_PARMS(n,2)
		    SRC_PARMS(n,3)=oldSRC_PARMS(n,3)
		    SRC_PARMS(n,4)=oldSRC_PARMS(n,4)
		    SRC_NAMES(n)=oldSRC_NAMES(n)
		 enddo
		 call psfbld
	      endif
	   else
	      print *,'The additional source is not significantly',
     1                ' indicated'
	   endif
	   second_call=.false.
	endif

	RETURN
	END
