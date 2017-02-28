C        subroutine ALLFIT(flag)
C
C
C  $Id: allfit.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++   Effect: ALLFIT estimates counts, Gmult and Gbias
C++           for a specific position. 
c
c
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     logical   flag   true  => query user
c                      false => no interaction with user
c
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated:    by  JRM
c
C=======================================================================
C  $Log: allfit.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:27  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.3  1996/07/31  17:04:41  jae
c Added use of jae_allfit flag
c
c Revision 5.2  1996/02/29  20:46:33  jae
c *** empty log message ***
c
c Revision 5.1  1996/02/29  20:44:52  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:35:40  jae
c Subroutine Module for like V5.00
c
C   Changes:   
c                                                        
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine ALLFIT(flag)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/psmrep.copy'

      character(80) id
      common /id/id
      logical flag
      REAL MAPVAL
      data exp_min/0.28/


      id = '$Id: allfit.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='ALLFIT'

      if (jae_allfit)write(*,*)'In routine ALLFIT'

      dGmult=0
      dGbias=0

      if (flag) then             ! examine our assumptions
         call pixel_select(.true.,'counts test point:  ')
         if (signal.ne.' ')return
         calc_uncert=.true.
      endif

      if (report) calc_uncert=.true.
      
C     COMPUTE TOTALS OVER CIRCLE OF INTEREST for model and PSF
      call liktot(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      CALL ERROR(1,LOC)

      OptC=.true.
      OptB=.not.Restrict_Gbias
      OptG=.not.Restrict_Gmult
      call optimize
      if (signal.ne.' ') return

      thelnL=-lnL

      if (calc_uncert) then
C     find second derivatives
         call derivative2
         if (signal.ne.' ') return
      endif 

      if (report)then
c     WRITE(LU(1),'( "Analysis point:",2f7.2)')srcL,srcB
         WRITE(LU(1),'( "Gmult",f12.6," +/-",f12.6)')
     &        Gmult,dGmult
         WRITE(LU(1),'( "Gbias",f12.6," +/-",f12.6)')
     &        Gbias,dGbias
         WRITE(LU(1),'( "Counts",f11.2," +/-",f11.2)')
     &        Counts,dCounts
      endif

      if (flag) then 
         expose=mapval(emap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) / (
     &        (sin(pi180*(srcB+ctlscl/2.))-sin(pi180*(srcB-ctlscl/2.)))
     &        *ctlscl*pi180)
         WRITE(LU(1),'("Corresponding flux",f12.6," +/-",f12.6,
     &        " 10^-8 cm^-2 s^-1")')
     &        1.e8*Counts/expose,1.e8*dCounts/expose
         WRITE(LU(1),'("Log likelihood:",f13.2)') thelnL
      endif

      return
      END
