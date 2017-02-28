c
C       subroutine GASTEST(flag)
c
c
C  $Id: gastest.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++   Effect: GASTEST finds the value of Gmult which 
c              optimizes the likelihood
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
C Installation of RCS lines: 23 AUG 1995 by JAE
c
c
C=======================================================================
C  $Log: gastest.f,v $
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
c Revision 5.0  1996/02/13  21:37:13  jae
c Subroutine Module for like V5.00
c
C%   Changes: 
c
c                                                          
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine GASTEST(flag)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/psmrep.copy'

      save

      character(80) id
      common /id/id
      logical flag
      CHARACTER input*50


      id = '$Id: gastest.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='GASTEST'

      if (flag) then             ! examine our assumptions
         write(lu(1),'("Input Counts (cr for",e11.3,"):",$)')Counts
         READ(LU(12),'(A)')input
         if (input.ne.' ') read(input,*,end=1111)Counts
C+++  
C  8/25/94 JAE
c     added if-then-else-endif when Gbias is set false to prevent
c     setting Gbias if it is fixed
         if (.not.Restrict_Gbias) then
            write(lu(1),'("Input Gbias (cr for",e11.3,"):",$)')Gbias
            READ(LU(12),'(A)')input
            if (input.ne.' ') read(input,*,end=1111)Gbias
         else
            Gbias=Gbias_nomj
         endif
         call pixel_select(.true.,'test point          ')
         if (signal.ne.' ')return
         calc_uncert=.true.
      endif

C     COMPUTE TOTALS OVER CIRCLE OF INTEREST for model and PSF
      call liktot(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      if (signal.ne.' ')return

      OptC=.false.
      OptB=.false.

      if (.not.Restrict_Gmult) then 
	 OptG=.true.
	 call optimize
	 if (signal.eq.' ') then
            if (.not.calc_uncert) return
C     find second derivatives
            call derivative2
            if (signal.ne.' ') return
            if (.not.report)return
            WRITE(LU(1),'( "Gmult",e13.5," +/-",
     &           e13.5," (1 SIGMA)")')Gmult,dGmult
	 endif
      else
	 OptG=.false.
         CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
	 if (signal.ne.' ') then
	    call error(0,LOC)
	 else
	    write(lu(1),1001)'Gmult',Gmult,' log likelihood ',-lnL
	 endif
      endif
      
      if (.not.flag) return
      if (signal.eq.' ') then
         WRITE(LU(1),'("Log likelihood:",f13.2)') -lnL
      else
         call error(0,LOC)
      endif

      Gmult_est=Gmult
 160  write(lu(1),'(
     &     "Input Gmult test values for likelihood test",
     &     " (or cr to end):",/,
     &     "This will NOT permanently change Gmult: "$)')
      READ(LU(12),'(A)')input
      if (input.ne.' ') then
         read(input,*,end=1111)Gmult
         CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         if (signal.ne.' ') then
	    call error(0,LOC)
         else
	    write(lu(1),1001)'Gmult',Gmult,' log likelihood ',-lnL
 1001       format(A,e12.5,A,f13.2)
         endif
         goto 160
      else
         Gmult=Gmult_est
         return
      endif

 1111 write(lu(1),*)'Invalid input, try again.'
      Gmult=Gmult_est

      return
      END
