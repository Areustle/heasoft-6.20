C      subroutine BIASTEST(flag)
C
C
C  $Id: biastest.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++   Effect: Obtain maximum likelihood estimate for Gbias
c
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     logical   flag    true  => query user 
c                       false=> no interaction w/ user
c
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated:    by  JRM
c
C=======================================================================
C  $Log: biastest.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:27  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  17:21:31  jae
c *** empty log message ***
c
c Revision 5.1  1996/02/29  20:46:48  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:03  jae
c Subroutine Module for like V5.00
c
C%   Changes: 
c                                                          
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine BIASTEST(flag)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/psmrep.copy'

      save

      character(80) id
      common /id/id
      logical flag
      CHARACTER input*50


      id = '$Id: biastest.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='BIASTST'

      if (jae_biastest)write(*,'("In routine ",a)') LOC
c
      if (flag) then             ! examine our assumptions
         write(lu(1),'("Input Counts (cr for",e11.3,"):",$)')Counts
         READ(LU(12),'(A)')input

         if (input.ne.' ') read(input,*,end=1111)Counts
         if (.not.Restrict_Gmult) then
            write(lu(1),'("Input Gmult (cr for",e11.3,"):",$)')Gmult
            READ(LU(12),'(A)')input
            if (input.ne.' ') read(input,*,end=1111)Gmult
         endif
         call pixel_select(.true.,
     &        'test point          ')
         if (signal.ne.' ') return
         calc_uncert=.true.
      endif

C     COMPUTE TOTALS OVER CIRCLE OF INTEREST for model and PSF
      call liktot(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      if (signal.ne.' ') return

      OptC=.false.
      OptG=.false.

      if (.not.Restrict_Gbias) then
         OptB=.true.
         call optimize

         if (signal.eq.' ') then
            if (.not.calc_uncert) return
C     find second derivatives
            call derivative2
            if (signal.ne.' ') return
            if (.not.report) return
            WRITE(LU(1),'( "Gbias",e13.5," +/-",e13.5,
     &           " at",2f8.2)')Gbias,dGbias,srcL,srcB
         endif

      else
         OptB=.false.
         CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
	 if (signal.ne.' ') then
	    call error(0,LOC)
	 else
	    write(lu(1),1001)'Gbias',Gbias,' log likelihood ',-lnL
	 endif
      endif
c     
      if (.not.flag) return
      if (signal.eq.' ') then
         WRITE(LU(1),'("Log likelihood:",f13.2)') -lnL
      else
         call error(0,LOC)
      endif

      Gbias_est=Gbias
 160  write(lu(1),'(
     &     "Input Gbias for likelihood test (or cr to end):",$)')

      READ(LU(12),'(A)')input
      if (input.ne.' ') then
         read(input,*,end=1111)Gbias
         CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         if (signal.ne.' ') then
	    call error(0,LOC)
         else
	    write(lu(1),1001)'Gbias',Gbias,' log likelihood ',-lnL
 1001       format(A,e14.5,A,f13.2)
         endif
         goto 160
      else
         Gbias=Gbias_est
         return
      endif

 1111 write(lu(1),*)'Invalid input, try again.'
      Gbias=Gbias_est

      return
      END
