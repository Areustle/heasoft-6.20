C       subroutine gascnts(flag)
C
C
C  $Id: gascnts.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++   Effect: 
C++	Solve for Gmult, Counts simultaneously with fixed Gbias
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
C  $Log: gascnts.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:31  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:59  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:11  jae
c Subroutine Module for like V5.00
c
C%   Changes:  
c
c                                                         
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine gascnts(flag)         

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/maprep.copy'

      save

      character(80) id
      common /id/id
      logical flag,test
      CHARACTER input*50,break*80
      data break/
     &'--------------------------------------------------------------'/

      id = '$Id: gascnts.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='gascnts'

      if (flag) then             ! examine our assumptions
	 if (.not.Restrict_Gbias) then
            write(lu(1),'("Input Gbias (cr for",e11.3,"):",$)')Gbias
            READ(LU(12),'(A)')input
            if (input.ne.' ') read(input,*,end=1111) Gbias
	 endif
         call pixel_select(.true.,'       test point:  ')
         if (signal.ne.' ') return
         report=.true.
         calc_uncert=.true.
         write(lu(1),'("Do you want to do a reduced likelihood ",
     &        "source test (cr for no)?  ",$)')
         READ(LU(12),'(A)')input
         if (input.ne.' ') then
	    test=.true.
         else
	    test=.false.
         endif
      else
         test=.false.
      endif

C     COMPUTE TOTALS OVER CIRCLE OF INTEREST for model and PSF
      call liktot(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      if (signal.ne.' ') return
      
      if (test) then
         Counts=0.
         WRITE(6,*)break
         WRITE(6,*)'Estimate of Gmult with counts=0:'
         OptC=.false.
         OptB=.false.
         OptG=.not.Restrict_Gmult
         if (OptG) then
            call optimize
         else
            CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         endif

         if (signal.ne.' ') return
         WRITE(LU(1),'( "Gmult",f12.6," +/-",f12.6)')
     &        Gmult,dGmult
         WRITE(LU(1),'("Log likelihood:",f13.2)') -lnL
         the_lnL_noc=lnL
         WRITE(6,*)break
         WRITE(6,*)
     &        'Simultaneous estimate of Gmult and Counts:'
      endif

      OptC=.true.
      OptB=.false.
      OptG=.not.Restrict_Gmult
      call optimize
      if (signal.ne.' ') return
      thelnL=-lnL
c     
      dGmult=0
      if (calc_uncert) then
C     find second derivatives
         call derivative2
         if (signal.ne.' ') return
      endif 

      if (report) then
         WRITE(LU(1),'( "Gmult",f12.6," +/-",f12.6)')
     &        Gmult,dGmult
         WRITE(LU(1),'( "Counts",f12.2," +/-",f12.2
     &        )')Counts,dCounts
         if (flag) then 
	    WRITE(LU(1),'("Log likelihood:",f13.2)') thelnL
         endif
         if (test) then
            TS=2.*(the_lnL_noc-lnL)
            if (Counts.le.0.)TS=0.
            if (TS.lt.0.)TS=0.
            write(lu(1),'("The reduced point source test yields TS=",
     &           f7.1)') TS
         endif
c     end if (report) code
      endif
      return

 1111 write(lu(1),*)'Invalid input, try again.'
      return
      end
