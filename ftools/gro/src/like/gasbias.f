C       subroutine gasbias(flag)         
C
C
C  $Id: gasbias.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++   Effect: 
C++	Solve for Gbias, Gmult simultaneously with fixed
C++	Counts. If flag is false,  no interaction with user
c
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     logical   flag    false => no interaction with user
c
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated:    by  JRM
c
c
C=======================================================================
C  $Log: gasbias.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:31  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:57  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:09  jae
c Subroutine Module for like V5.00
c
C%   Changes: 
c
c                                                          
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine gasbias(flag)         

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/maprep.copy'

      save
      character(80) id
      common /id/id
      CHARACTER input*50
      logical flag

      id = '$Id: gasbias.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='gasbias'

      if (flag) then             ! examine our assumptions
         write(lu(1),'("Input Counts (cr for",e11.3,"):",$)')Counts
         READ(LU(12),'(A)')input
         if (input.ne.' ') read(input,*,end=1111)Counts
         call pixel_select(.true.,'       test point:  ')
         if (signal.ne.' ') return
         calc_uncert=.true.
      endif

C     COMPUTE TOTALS OVER CIRCLE OF INTEREST for model and PSF
      call liktot(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      if (signal.ne.' ') return
      
      OptC=.false.
      dGmult=0
      dGbias=0
      OptB=.not.Restrict_Gbias
      OptG=.not.Restrict_Gmult
      if (OptG.or.OptB) then
	 call optimize
	 if (signal.ne.' ') return
      else
         CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      endif
      thelnL=-lnL

      if (calc_uncert) then
C     find second derivatives
         if (OptB.or.OptB)call derivative2
         if (signal.ne.' ') return
      endif 

      if (report) then
         WRITE(LU(1),'( "Gmult",f12.6," +/-",f12.6)')
     &        Gmult,dGmult
         WRITE(LU(1),'( "Gbias",f12.5," +/-",f12.5)')Gbias,dGbias
         if (flag) then 
	    WRITE(LU(1),'("Log likelihood:",f13.2)') thelnL
         endif

c     end if (report) code
      endif
      return

 1111 write(lu(1),*)'Invalid input, try again.'
      return
      end
