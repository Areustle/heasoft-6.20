C      subroutine CNTSBIAS(flag)
C
C
C  $Id: cntsbias.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++   Effect: Obtain maximum likelihood estimate for Counts and Gbias
c
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     logical   flag   false   => no interaction with user
c
c
c=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated:    by  JRM
c
C=======================================================================
C  $Log: cntsbias.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:28  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:14  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:20  jae
c Subroutine Module for like V5.00
c
C%   Changes:                                  
c
c                         
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine CNTSBIAS(flag)

C     Common blocks used:
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/psmrep.copy'

      save

      character(80) id
      common /id/id
      logical flag,test
      REAL MAPVAL
      CHARACTER input*50,break*80
      data break/
     &'--------------------------------------------------------------'/
      data LOC/'CNTSBIAS'/


      id = '$Id: cntsbias.f,v 1.2 2013/05/21 19:08:25 irby Exp $'

      Gmult_est=Gmult

      if (flag) then             ! examine our assumptions
	 if (.not.Restrict_Gmult) then
            write(lu(1),'("Input Gmult (cr for",e11.3,"):",$)')Gmult
            READ(LU(12),'(A)')input
            if (input.ne.' ') read(input,*,end=1111,err=1111)Gmult
	 endif
         call pixel_select(.true.,'source test point:  ')
         if (signal.ne.' ')return
         report=.true.
         calc_uncert=.true.
         write(lu(1),'("Do you want to do a reduced likelihood ",
     &        "source test (cr for no)? ",$)')
         READ(LU(12),'(A)')input
         if (input.ne.' ') then
	    test=.true.
	    WRITE(6,*)
	    WRITE(6,*)'QUIET!, the computer is thinking.'
	    WRITE(6,*)
         else
	    test=.false.
         endif
      else
         test=.false.
      endif

C     COMPUTE TOTALS OVER CIRCLE OF INTEREST for model and PSF
      call liktot(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      if (signal.ne.' ')return

      if (test) then
         Counts=0.
         WRITE(6,*)break
         WRITE(6,*)'Estimate of Gbias with counts=0:'
         OptC=.false.
         OptB=.not.Restrict_Gbias
         dGbias=0.
         OptG=.false.
         if (OptB) then
            call optimize
         else
            CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         endif
         if (signal.ne.' ') return
         WRITE(LU(1),'( "Gbias",f12.6," +/-",f12.6)')
     &        Gbias,dGbias
         WRITE(LU(1),'("Log likelihood:",f13.2)') -lnL
         the_lnL_noc=lnL
         WRITE(6,*)break
         if (.not.Restrict_Gbias) then
	    WRITE(6,*)
     &           'Simultaneous estimate of Gbias and Counts:'
         else
	    WRITE(6,*)'Restricted Gbias, estimate Counts:'
         endif
      endif

      OptC=.true.
      OptB=.not.Restrict_Gbias
      OptG=.false.
      call optimize
      if (signal.ne.' ') then
         CALL ERROR(0,LOC)
         return
      endif
      thelnL=-lnL

      if (calc_uncert) then
C     find second derivatives
         call derivative2
         if (Restrict_Gbias)dGbias=0
         if (signal.ne.' ') return
      endif 

      if (report) then
         WRITE(LU(1),'( "Gbias",e13.5," +/-",e13.5,
     &        " at",2f8.2)')Gbias,dGbias,srcL,srcB
         WRITE(LU(1),'( "Counts",e14.6," +/-",e14.6)') 
     &        Counts,dCounts
         if (flag) then 
            expose=mapval(emap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) / (
     &           (sin(pi180*(srcB+ctlscl/2.))-
     &           sin(pi180*(srcB-ctlscl/2.)))
     &           *ctlscl*pi180)
            WRITE(LU(1),'("Corresponding flux",e13.5," +/-",e13.5,
     &           " cm^-2 s^-1; for",I5," < E <",I5)')
     &           Counts/expose,dCounts/expose,ctlemn,ctlemx
            
            WRITE(LU(1),'("Log likelihood:",f13.2)') thelnL
         endif
         if (test) then
            TS=2.*(the_lnL_noc-lnL)
            if (Counts.le.0.)TS=0.
            if (TS.lt.0.)TS=0.
            write(lu(1),'("The reduced point source test ",
     &           "yields TS=",f7.1)') TS
         endif
c     end if (report) code
      endif
      return

 1111 write(lu(1),*)'Invalid input, try again.'
      Gmult=Gmult_est

      return
      END
