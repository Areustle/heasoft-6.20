C        subroutine SRCTEST(interact)
C
C
C  $Id: srctest.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++   Effect: Obtain maximum likelihood estimate for counts, Gmult, and 
C++           Gbias. Find source significance using likelihood ratio test.
C
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     logical interact     true => interact with user
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated:    by  JRM
C
C=======================================================================
C  $Log: srctest.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:44  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:53:29  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:28  jae
c Subroutine Module for like V5.00
c
C%   Changes:
c	jrm 1/8/94 Copy spectroscopy output to an external file.
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine SRCTEST(interact)

C  Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
      logical flag,interact
      REAL MAPVAL
      CHARACTER break*80,coord_phr*10

      data break/
     &'--------------------------------------------------------------'/

      id = '$Id: srctest.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='SRCTEST'


      if (jae_srctest) print *,'Inside routine srctest'
      
      flag=interact
      if (flag) then             ! examine our position
         call pixel_select(.true.,'       test point:  ')
         if (signal.ne.' ') return
         calc_uncert=.true.
         report=.true.
      endif

      if (report) report2=.true.
      Counts=0.
      
      if (gmap_null) then
         Gmult=0.
         if (report) then
	    print *,'GASMAP NULL; reduced src test will be preformed.'
            WRITE(6,*)break
            WRITE(6,*)'Estimate of Gbias with counts=0:'
         endif
         call BIASTEST(.false.)

      else
         if (report) then
            WRITE(6,*)break
            WRITE(6,*)'Estimate of Gmult and Gbias with counts=0:'
         endif
         call GASBIAS(.false.)
      endif

      if (signal.ne.' ') return
      source_notlnL=-lnL

      if (report) then
         WRITE(LU(1),'("Log likelihood:",f13.2)') source_notlnL
      endif
      
      if (gmap_null) then
         if (report) then
            WRITE(6,*)break
            WRITE(6,*)
     &           'Simultaneous estimate of Gbias and Counts:'
         endif

         call CNTSBIAS(.false.)

      else
         if (report) then
            WRITE(6,*)break
            WRITE(6,*)
     &           'Simultaneous estimate of Gmult, Gbias and Counts:'
         endif

         call ALLFIT(.false.)
      endif

      if (signal.ne.' ') return
      sourcelnL=-lnL
      TS=(sourcelnL-source_notlnL)*2.
      if (Counts.le.0.) TS=0.
      if (TS.lt.0.) TS=0.

c%%%%%%%%%%%%%%%%%%%% Start JAE addition %%%%%%%%%%%%%%%%%%%%%%->
      pss_lnL = lnL
      pss_not_lnL = -source_notlnL
      pss_TSj = TS
c%%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%->

      if (.not.report2) return

      if( report) then
         WRITE(LU(1),'("Log likelihood:",f13.2)') sourcelnL
         WRITE(LU(1),'("Test statistic:",f9.2)') TS
      endif

      Gmult_est=Gmult
      Gbias_est=Gbias
      Counts_est=Counts

      if (flag) then
c     Calculate counts bounds
c     estimate counts without constraining to be positive
         call srcounts
         if (signal.ne.' ') then
	    signal=' '
	    flag=.false.        !need to redo this for upper limit
	    goto 190
         endif

         Counts_est_unconstr=Counts
         unconstr_lnL=lnL

         if (Counts_est_unconstr.lt.0.)
     &        WRITE(6,'("Unconstrained Counts estimate is:",f7.1,
     &        " with lnL=",f7.1)') Counts_est_unconstr, -unconstr_lnL
         target_lnL=lnL+0.5     ! (1 sigma)

c     Set bounds for upper limit search
         AX=Counts_est_unconstr
         CX=Counts_est_unconstr+2.*dCounts
         call limit(AX,CX,.true.)

         if (signal.ne.' ') then
	    signal=' '
	    goto 190
         endif

         dCounts_up=Counts_limit-Counts_est_unconstr

c     Set bounds for lower limit search
         AX=Counts_est_unconstr
         CX=Counts_est_unconstr-2.*dCounts
         call limit(AX,CX,.false.)

         if (signal.ne.' ') then
	    signal=' '
	    goto 190
         endif

         dCounts_dn=Counts_est_unconstr-Counts_limit

         if (abs(dCounts_dn).lt.0.1) then
c     Counts_est_unconstr must be at the border of negative model cnts
	    goto 190
         endif

         WRITE(6,'("Corresponding delta Counts:  -",f6.1,
     &        "   ;          +",f6.1)') dCounts_dn,dCounts_up
         WRITE(6,'("Assymetric 68% confidence Counts range:",2f7.1)')
     &        Counts_est_unconstr-dCounts_dn,
     &        Counts_est_unconstr+dCounts_up
      endif
 190  continue

      if (TS.le.TS_max) then
c     calculate an upper limit
         if (.not.flag) then
c     estimate counts without constraining to be positive
	    call srcounts
            if (signal.ne.' ') then
               signal=' '
               Counts_limit=-Inf
               print *,'Upper limit error - setting upper limit to -inf'
               goto 191
            endif

	    Counts_est_unconstr=Counts
	    unconstr_lnL=lnL
            if (report.and.(Counts_est_unconstr.lt.0.))
     &           WRITE(6,'("Unconstrained counts estimate is:",f7.1,
     &           " with lnL="f7.1)')
     &           Counts_est_unconstr, -unconstr_lnL
         endif

         if (report) then
	    write(6,*)break
         endif

         AX=Counts_est_unconstr
         CX=Counts_est_unconstr+2.*dCounts
         target_lnL=unconstr_lnL+delta_lnL
         call limit(AX,CX,.true.)

         if (signal.ne.' ') then
	    signal=' '
	    Counts_limit=-Inf
            print *,'Upper limit error - setting upper limit to -inf'
	    goto 191
         endif

         if (Counts_est_unconstr.lt.0.) then
            if (report) then
               WRITE(6,'("The statistical upper limit is",f8.2,
     &              " counts.")')Counts_limit
               WRITE(6,'("However, the unconstrained Counts estimate ",
     *              "< 0 - which is non-physical")')
               WRITE(6,'(
     &              "therefore the actual upper limit is assumed to be",
     &              " the difference between")')
               WRITE(6,'(
     &              "the statistical upper limit",
     &              " and the the unconstrained Counts estimate.")')
c               WRITE(6,'(
c     &              "However, the unconstrained Counts estimate is < 0 -",
c     &              " which is non-physical;",/,
c     &              "therefore the actual upper limit is assumed to be",
c     &              " the difference between",/,
c     &              "the statistical upper limit",
c     &              " and the the unconstrained Counts estimate.")')
            endif
            Counts_limit=Counts_limit-Counts_est_unconstr
         endif

         if (report) then
	    WRITE(6,'("A",f7.1,"% confidence upper limit is ",
     &           f11.2," Counts.")')
     &           conf_precent,Counts_limit
	    Counts=Counts_est_unconstr-0.1
            CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)

	    if (signal.ne.' ') then
               signal=' '
               write(6,'("WARNING - this upper limit is possibly",
     &              " too small - the unconstrained counts estimate ",
     &              "is at the ",
     &              "border of causing negative model counts.")')
	    endif
         endif
 191     continue
      endif

      Counts=Counts_est
      Gmult=Gmult_est
      Gbias=Gbias_est
      
      if (coord_sys.eq.'C') then
         coord_phr='RA     DEC'
         pL=SC_RA
         pB=SC_DEC
      else
         coord_phr=' L       B'
         pL=SC_LII
         pB=SC_BII
      endif

      if (report) then
         write(6,*)break
         WRITE(6,'("  Name               ",A,"    sqrt(TS)",
     &        "  Flux+/- 1 sigma    (U.L.)",
     &        "       Cnts   +/- 1 sigma  (U.L.)",
     &        "    Gmult    Gbias  Ranal  Asp. EXP        lnL")')
     &        coord_phr
      endif

      theLong=srcL
      if (theLong.lt.0.) theLong=theLong+360.

      if (MAPTYPE.eq.'SINGLE_POINTING') then
         aspect=gtcirc(srcL,srcB,pL,pB)
      else
         aspect=1.e20
      endif

      expose=mapval(emap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) / (
     &     (sin(pi180*(srcB+ctlscl/2.))-sin(pi180*(srcB-ctlscl/2.)))
     &     *ctlscl*pi180)

c%%%%%%%%%%%%%%%%%%%% Start JAE addition %%%%%%%%%%%%%%%%%%%%%%->
      pss_expose=expose
c%%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%->

      Flux=1.e8*Counts/expose

      if (TS.gt.TS_max) then
c     This is a strong detection.
         WRITE(6,'(A18,1x,2f7.2,f7.1,f11.3,f9.3,11x,
     &        2f11.2,10x,f9.3,f9.3,f6.1,f6.1,f6.2,f12.2)')
     &        srcN,theLong,srcB,sqrt(TS),
     &        Flux,1.e8*dCounts/expose,
     &        Counts,dCounts,Gmult,Gbias,Ranal
     &        ,aspect,expose/1.E7,sourcelnL

      elseif (TS.le.TS_max.and.TS.gt.TS_min) then
c     This is a weak detection.
         WRITE(6,'(A18,1x,2f7.2,f7.1,f11.3,f9.3,"  <",f8.3,
     &        2f11.2,"  <",f7.2,f9.3,f9.3,f6.1,f6.1,f6.2,f12.2)')
     &        srcN,theLong,srcB,sqrt(TS),
     &        Flux,1.e8*dCounts/expose,1.e8*Counts_limit/expose,
     &        Counts,dCounts,Counts_limit,Gmult,Gbias,Ranal
     &        ,aspect,expose/1.E7,sourcelnL

      else
c     This is not detected.
         WRITE(6,'(A18,1x,2f7.2,f7.1,20x,"  <",f8.3,
     &        22x,"  <",f7.2,f9.3,f9.3,f6.1,f6.1,f6.2,f12.2)')
     &        srcN,theLong,srcB,sqrt(TS),
     &        1.e8*Counts_limit/expose,
     &        Counts_limit,Gmult,Gbias,Ranal
     &        ,aspect,expose/1.E7,sourcelnL
      endif

      if (spectral) then

c     produce an entry for spectral DC analysis

         if (CTLEMN.eq.30.and.CTLEMX.eq.50) then

c     factor_kniffen=2.7
c     factor_uncertainty=0.7
            factor_kniffen=1.
            factor_uncertainty=0.
c     write(6,*)
c     &         'Input for spectral program (Kniffen factor applied):'

         elseif (CTLEMN.eq.50.and.CTLEMX.eq.70)then

c     factor_kniffen=1.4
c     factor_uncertainty=0.2
            factor_kniffen=1.
            factor_uncertainty=0.
c     write(6,*)
c     &         'Input for spectral program (Kniffen factor applied):'

         elseif(CTLEMN.eq.30.and.CTLEMX.eq.100)then

c     factor_kniffen=1.43
c     factor_uncertainty=0.22
            factor_kniffen=1.
            factor_uncertainty=0.
c     write(6,'(
c     &         "Input for spectral program (Nolan factor=1.43+/-0.22",
c     &         " (VALID FOR PHOTON INDEX 2 ONLY!!) applied):")')

         else
            factor_kniffen=1.
            factor_uncertainty=0.
c     write(6,*)
c     &         'Input for spectral program:'
         endif

         write(6,*)'Input for spectral program:'
         write(6,*)
     &        '.......emin........emax.......counts....',
     *        'counts_err....zenmax'
         
         if (TS.gt.TS_min) then
            write(6,'(4x,I5,6x,I5,3x,f12.2,5x,f12.2,5x,f7.3)')
     &           CTLEMN,CTLEMX,
     &           Counts*factor_kniffen,
     &           sqrt(dCounts**2*factor_kniffen**2+
     &           Counts**2*factor_uncertainty**2)
     &           ,zenith_cut
            write(99,'(4x,I5,6x,I5,3x,f12.2,5x,f12.2,5x,f7.3,
     &           " :",A15)')
     &           CTLEMN,CTLEMX,
     &           Counts*factor_kniffen,
     &           sqrt(dCounts**2*factor_kniffen**2+
     &           Counts**2*factor_uncertainty**2)
     &           ,zenith_cut,srcN

         else
c     This is not detected.
            write(6,'(4x,I5,6x,I5,3x,f12.2,5x,f12.2,5x,f7.3)')
     &           CTLEMN,CTLEMX,0,
     &           Counts_limit*(factor_kniffen+factor_uncertainty/2.),
     &           zenith_cut
            zero = 0.0
            write(99,'(4x,I5,6x,I5,3x,f12.2,5x,f12.2,5x,f7.3,
     &           " :",A15)')CTLEMN,CTLEMX,zero,
     &           Counts_limit*(factor_kniffen+factor_uncertainty/2.),
     &           zenith_cut,srcN
         endif
         write(6,*)
      endif

c
c     return if Restrict_notice
c
      if (Restrict_notice) return
c
      if (.not.Restrict_Gmult) then
         if (Gmult.lt.Gmult_min.or.Gmult.gt.Gmult_max) then
	    write(lu(1),'(
     &           "Oops! - another case of Gmult=",f5.2,
     &           " outside the expected range:",f5.2," < Gmult <",f5.2,
     &           " - you may wish to use the CB command with Gmult ",
     *           "fixed.")')
     &           Gmult,Gmult_min,Gmult_max
         endif
      endif

      if (.not.Restrict_Gbias) then
         if (Gbias.lt.Gbias_min.or.Gbias.gt.Gbias_max) then
	    write(lu(1),'(
     &           "Oops! - another case of Gbias=",f5.2,
     &           " outside the expected range:",f5.2," < Gbias <",f5.2,
     &           " - you may wish to use the GC command with Gbias ",
     *           "fixed.")')
     &           Gbias,Gbias_min,Gbias_max
         endif
      endif

      return
      END
c
