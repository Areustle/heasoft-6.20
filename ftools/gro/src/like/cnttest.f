c        subroutine CNTTEST(flag)
c
c
C  $Id: cnttest.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++   Effect: Obtain maximum likelihood estimate for source counts
c
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
C  $Log: cnttest.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:28  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:15  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:23  jae
c Subroutine Module for like V5.00
c
C%   Changes:        
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine CNTTEST(flag)

C  Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
c     INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
      logical flag
      REAL MAPVAL
      CHARACTER input*50


      id = '$Id: cnttest.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='CNTTEST'

      if (flag) then             ! examine our assumptions
	 if (.not.Restrict_Gmult) then
            write(lu(1),'("Input Gmult (cr for",e11.3,"):",$)')Gmult
            READ(LU(12),'(A)')input
            if (input.ne.' ') read(input,*,end=1111)Gmult
	 endif
	 if (.not.Restrict_Gbias) then
            write(lu(1),'("Input Gbias (cr for",e11.3,"):",$)')Gbias
            READ(LU(12),'(A)')input
            if (input.ne.' ') read(input,*,end=1111)Gbias
	 endif
         call pixel_select(.true.,'counts test point:  ')
         if (signal.ne.' ') return
         calc_uncert=.true.
      endif

C     COMPUTE TOTALS OVER CIRCLE OF INTEREST for model and PSF
      call liktot(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      if (signal.ne.' ') return
c     cntssv=counts
c     counts=0
c     CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
c     pss_not_lnL = -lnL
c     counts=cntssv
      CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      call srcounts

      if (signal.eq.' ') then
         if (.not.calc_uncert) return
C     find second derivatives
         OptC=.true.
         OptB=.false.
         OptG=.false.
         call derivative2
         if (signal.ne.' ') return
         if (.not.report) return
         WRITE(LU(1),'( "Counts",f11.2," +/-",f11.2, " at",2f8.2)')
     &        Counts,dCounts,srcL,srcB
      endif

      if (.not.flag) return
      expose=mapval(emap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) / (
     &     (sin(pi180*(srcB+ctlscl/2.))-sin(pi180*(srcB-ctlscl/2.)))
     &     *ctlscl*pi180)
      if (signal.eq.' ') then
         WRITE(LU(1),'("Corresponding flux",e13.5," +/-",e13.5,
     &        " cm^-2 s^-1")')
     &        Counts/expose,dCounts/expose
         WRITE(LU(1),'("exposure",e13.5," cm^2 s")')expose
         WRITE(LU(1),'("Log likelihood:",f13.2)') -lnL
      else
         call error(0,LOC)
      endif

      Counts_est=Counts
 160  write(lu(1),'(
     &     "Input number of counts for likelihood test ",
     &     "(or cr to end):",$)')

      READ(LU(12),'(A)')input
      if (input.ne.' ') then
         read(input,*,end=1111)Counts
         CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         if (signal.ne.' ') then
	    call error(0,LOC)
         else
	    write(lu(1),1001)'Counts',Counts,'; Log of Likelihood'
     &           ,-lnL ,'; flux',Counts/expose
 1001       format(A,e15.6,A,f13.2,A,e15.6)
         endif
         goto 160
      else
         Counts=Counts_est
         return
      endif
      
 1111 write(lu(1),*)'Invalid input, try again.'

      return
      END
