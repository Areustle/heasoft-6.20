c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C        SUBROUTINE JFNDPOS(jflag,jflag2)
C
C
C  $Id: jfndpos.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=============================================================================
C*      effect: Finds the optimum postion for a PSF source
C*                      
C=============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+             UPDATED:     by  JAE
C=============================================================================
C* Subroutine Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^^^
C* jflag	logical: When true enhances the screen output for diagnostics
C* jflag2	logical: If true user is prompted for trial position
C*============================================================================
C* Returned Values
C* ^^^^^^^^^^^^^^^
C*   none
c*============================================================================
c
C  $Log: jfndpos.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:09  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:34  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.3  1996/06/20  18:48:38  jae
c Added lines to call error(-2,LOC) for error
c output.
c
c Revision 5.2  1996/04/08  16:12:03  jae
c Repaired naming of sources (srcN)
c
c Revision 5.1  1996/02/29  20:48:18  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:15  jae
c Subroutine Module for like V5.00
c
c
c==========================================================================

      SUBROUTINE JFNDPOS(jflag,jflag2)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
      logical jflag,rptsvj,calc_uncj,jflag2
      logical determined
      character input*50,text*20
      DIMENSION svopos(2),svcpos(2),paramj(3),
     &     AMATJ(3,2)
c
c---> initialize parameter values
c
      id = '$Id: jfndpos.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      text ='       test point:  '
      sigsv=' '
      signal=' '
      SIGMSG=' '
      LOC='JFNDPOS'
      if(jae_jfndpos)print *,'Inside routine jfndpos'
      dt=CTLSCL/2.
c
c---> save old coordinates, check coordinate system and set (pL,pB) to 
c---> (Long,Lat) or (RA,Dec) of pointing direction 
c
      svopos(1) = srcL
      svopos(2) = srcB
      if(coord_sys.eq.'G')then
         pL = SC_LJJ
         pB = SC_BJJ
      else
         pL = SC_RAJ
         pB = SC_DECJ
      endif
      tmpL = srcL
      tmpB = srcB
c
c---> final initializations
c
      rptsvj = report
      report = JFALSE
      signal = ' '
      calc_uncj = calc_uncert
      calc_uncert = JFALSE
 8    signal=' '
      SIGMSG=' '
c
c---> if jflag2=T prompt user for input
c
      if(jflag2) then 
 3       continue
         theLong=srcL
         CALL L_CHECK(srcL)
         if(theLong.lt.0)theLong=theLong+360.
         if(coord_sys.eq.'G') then
            write(lu(1),2020)text,theLong,srcB
         elseif(coord_sys.eq.'C') then
            write(lu(1),2030)text,theLong,srcB
         else
            write(lu(1),2040)text,theLong,srcB
         endif
         READ(LU(12),'(A)')input
         if(input.eq.' ')goto 4
         if(input.eq.'a'.or.input.eq.'A')  then
c     abort operation
            SIGNAL=' '
            SIGMSG=' '
            return
         endif
         read(input,*,end=3)srcL,srcB
         CALL L_CHECK(srcL)
         svopos(1) = srcL
         svopos(2) = srcB
 35      continue
         if(.not.jnmflg)then
            write(*,'("Source Name:",a)') srcN
            write(*,'("Enter new source name",
     &           "(<cr> to accept current name or",/,
     &           "  enter period to Autoname:"$)')
            read(LU(12),'(a)') srcN
            numcar = index( srcN,' ' ) - 1
            if(numcar.eq.0)goto 4
            if(srcN(1:1).eq.'.'.or.srcN.eq.' ')jnmflg=.true.
         endif
c
      else
c
         CALL L_CHECK(srcL)
c
      endif
c
 4    continue
      aspect = gtcirc(srcL,srcB,pL,pB)
c     
c---> get zenith aspect angle to pointing direction and test against maximum
c---> permitted aspect cone
c
      if(aspect.gt.aspj)then
         signal = 'z'
         if(input.ne.' ')then
            SIGMSG =
     &           'ERROR: POSITION IS OUTSIDE OF ASPECT CONE- LOC:'//LOC
         else
            SIGMSG =
     &           'ERROR: DEFAULT POSITION IS OUTSIDE OF ASPECT ' //
     *           'CONE- LOC:'//LOC
            call ERROR(0,LOC)
            write(6,'(" ")')
            srcL=pL
            srcB=pB
            goto 8
         endif
         call ERROR(0,LOC)
         write(6,'(" ")')
         srcL=tmpL
         srcB=tmpB
         goto 8
      endif
c
c---> set srcL to be within the map boundaries and select pixel
c
      CALL L_CHECK(srcL)
      CALL PIXEL_SELECT(JFALSE,jblank)
 7    if(signal.ne.' ')then
         srcL=svopos(1)
         srcB=svopos(2)
         sigsv=signal
c     
         CALL PIXEL_SELECT(JFALSE,jblank)
         LikTotaled = JFALSE
         CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         signal=sigsv
         return
      endif
      LikTotaled = JFALSE
      CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      if(signal.ne.' ')goto 7
      if(jflag2)then
 9       write(*,20)'convergence criteria for AMOEBA',tolj
 20      format('Input ',A,/,' (A to abort, cr for ',
     &        f8.3,'):',$)
         read(LU(12),'(A)')input
         if(input.ne.' '.and.input.ne.' ')then
            if(input.eq.'A'.or.input.eq.'a')then
               srcL=svopos(1)
               srcB=svopos(2)
               sigsv=signal
c     
               CALL PIXEL_SELECT(JFALSE,jblank)
               LikTotaled = JFALSE
               CALL LIKTOT(map,bmap,gasmap,emap,
     &              CTLMSZ1,CTLMSZ2)
               report = rptsvj
               calc_uncert = calc_uncj
               signal=sigsv
               return
            endif
            read(input,*,end=9)tolj
         endif
      else
         if(tolj.lt.0.001)tolj = 0.001
      endif
c
c---> check if position is within map boundaries
c
      if(srcL.gt.CTLEND(1)+dt .or. srcL.lt.CTLORG(1)-dt.or.
     &     srcB.gt.CTLEND(2)+dt.or.srcB.lt.CTLORG(2)-dt)then
         print *,' '
         print *,' Selected position is not within CTL '
         print *,' Pick another position'
         print *,' CTL: ',ctlorg(1),' < ',srcL,' < ',ctlend(1)
         print *,' CTL: ',ctlorg(2),' < ',srcB,' < ',ctlend(2)
         signal='C'
         SIGMSG =
     &        ' ERROR -C- position is outside of CTL - LOC:'//LOC
         determined=JFALSE
         if(jflag)call intent(determined)
         if(.not.determined)then
            srcL = svopos(1)
            srcB = svopos(2)
c     
            CALL PIXEL_SELECT(JFALSE,jblank)
            LikTotaled = JFALSE
            CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,
     &           CTLMSZ2)
            report = rptsvj
            calc_uncert = calc_uncj
            return
         endif
         goto 8
      endif
c
      iter2 = 0
 33   iter1 = 0
      iter2 = iter2 + 1
      svopos(1) = srcL          ! lon or RA
      svopos(2) = srcB          ! lat or dec
c
c---> set up verticies (arrays) for the amoeba search
c
      svcpos(1) = srcL
      svcpos(2) = srcB
      CALL SET_AMAT(amatj,paramj,svcpos,jflag)
      if(signal.ne.' ')then
c     
         sigsv=signal
         if(jae_jfndpos)then
            print *,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
            if(signal.eq.'Z')then
               print *,' TS <= 1.e-06 in setup -- ',
     *              'check start position with'
               print *,' likelihood command L from main menu'
            else
               print *,'     -- Error in routine called from setup.--'
               print *,'-- check position with command L ',
     *              'from main menu --'
            endif
            print *,'----- Back to Main Menu------'
            print *,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
            if(jae_jfndpos)then
               in_dex1=index(SIGMSG,'   ')-1
               if(in_dex1.le.0)in_dex1=1
               print *,'SIGNAL:',SIGNAL
               print *,'SIGMSG:',SIGMSG(1:in_dex1)
            endif
            signal=' '
         endif
         srcL = svopos(1)
         srcB = svopos(2)
c
c
c
         CALL PIXEL_SELECT(JFALSE,jblank)
         LikTotaled = JFALSE
         CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         report = rptsvj
         calc_uncert = calc_uncj
         signal = sigsv
         return
      endif
c
c---> all is okay, position is good to try amoeba
c
      CALL AMOEBAJ(amatj,paramj,3,2,2,tolj,iter1,jflag)
c     
c---> check for error on return
c
      if(signal.eq.'R'.or.signal.eq.'Z'.or.signal.eq.'z')then
         if(signal.eq.'R')then
            SIGMSG = 'AMOEBA -- Position out of ROI bounds --'
         elseif(signal.eq.'Z')then
            SIGMSG ='AMOEBA -- Test Statistic is ZERO too often --'
         else
            SIGMSG ='AMOEBA -- Position out of aspect angle bounds --'
         endif
         sigsv=signal
         if(.not.jae_jfndpos)then
            in_dex1=index(SIGMSG,'   ')-1
            if(in_dex1.le.0)in_dex1=1
            print *,'SIGNAL:',SIGNAL
            print *,'SIGMSG:',SIGMSG(1:in_dex1)
            print *,' '
         else
            CALL ERROR(-1,LOC)
            SIGMSG=' '
         endif
c
c---> reset original position, select pixel and return
c
         srcL = svopos(1)
         srcB = svopos(2) 
c
         CALL PIXEL_SELECT(JFALSE,jblank)
         LikTotaled = JFALSE
         CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         CALL SRCTEST(JFALSE)
         if(jnmflg)call getsrcnm()
         report = rptsvj
         calc_uncert = calc_uncj
         signal=sigsv
         return
      endif
c
c---> signal equals M. This is not a problem.
c
      if(signal.eq.'M')then
         sigsv=signal
         SIGMSG = '-- AMOEBA: maximum iterations exceeded --'
         if(.not.jae_jfndpos)then
            CALL ERROR(0,LOC)
            SIGMSG=' '
            goto 16
         else
            CALL ERROR(-1,LOC)
            determined = JFALSE
            CALL INTENT(determined)
            if(determined)goto 16
            signal=' '
            SIGMSG=' '
         endif
         srcL = svopos(1)
         srcB = svopos(2)
c
c
         CALL PIXEL_SELECT(JFALSE,jblank)
         LikTotaled = JFALSE
         CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         CALL SRCTEST(JFALSE)
         if(jnmflg)call getsrcnm()
         report = rptsvj
         calc_uncert = calc_uncj
         signal = sigsv
         return
      endif
c
c---> find best TS from the returned verticies
c
 16   itab = 0
      gtab = 100000.
      do jj = 1,3
         if(paramj(jj).le.gtab)then
            gtab = paramj(jj)
            itab = jj
         endif
      enddo
      if(itab.eq.0)itab = 1
      srcL = amatj(itab,1)
      srcB = amatj(itab,2)
      TS = -gtab
      CALL L_CHECK(srcL)
      if(iter2.le.1)then
c
c---> if first amoeba loop then repeat the process
c
         CALL PIXEL_SELECT(JFALSE,jblank)
         LikTotaled = JFALSE
         CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         goto 33
      endif
c
c---> search and optimization is completed.  process new position with
c---> error checking along the way.  If all okay return to calling routine
c
      CALL PIXEL_SELECT(JFALSE,jblank)
      if(signal.ne.' ')then
         if(.not.jae_jfndpos)then
            SIGMSG = 'ERROR in pixel_select --'
            CALL ERROR(0,LOC)
            SIGMSG=' '
         else
            CALL ERROR(-1,LOC)
            SIGMSG=' '
         endif
         report = rptsvj
         calc_uncert = calc_uncj
         signal = 'P'
         SIGMSG = 'ERROR in pixel_select from '//LOC
         return
      endif
      if(jnmflg)call getsrcnm()
      LikTotaled = JFALSE
      CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      if(signal.ne.' ')then
         SIGMSG = 'ERROR in liktot --'
         if(.not.jae_jfndpos)then
            CALL ERROR(0,LOC)
            SIGMSG=' '
         else
            CALL ERROR(-1,LOC)
            SIGMSG=' '
         endif
         report = rptsvj
         calc_uncert = calc_uncj
         signal = 'L'
         SIGMSG = 'ERROR in liktot from '//LOC
         return
      endif
c
      report=JTRUE
      calc_uncert = JTRUE
      report2 = JTRUE
      CALL SRCTEST(JFALSE)
      report2=JFALSE
      if(signal.ne.' ')then
         determined=JFALSE
         if(jae_jfndpos)call error(-1,LOC)
         if(jae_jfndpos)call intent(determined)
         if(.not.determined)then
            report = rptsvj
            calc_uncert = calc_uncj
            signal = 't'
            SIGMSG = 'ERROR in srctest from '//LOC
            return
         endif
         signal = ' '
         SIGMSG=' '
      endif
      if(jadjflg.and.TS.ge.psfminj)then
         nsrc = NSOURCE + 1
         SRC_PARMS(nsrc,8)=1.   ! active
         CALL PSFADD(Counts,bmap,CTLMSZ1,CTLMSZ2)
         SRC_PARMS(nsrc,1)=srcL
         SRC_PARMS(nsrc,2)=srcB
         SRC_PARMS(nsrc,3)=Counts
         SRC_PARMS(nsrc,4)=gamma
         SRC_NAMES(nsrc)=srcN
         NSOURCE=NSOURCE+1
         LikTotaled=JFALSE
         CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      endif
      report = rptsvj
      calc_uncert = calc_uncj
      return
 2020 format('Input ',A20,' GLON and GLAT',
     &     /,' (A to abort, cr for ', 2f8.3,'):',$)
 2030 format('Input ',A20,' RA (J2000) and DEC',
     &     /,' (A to abort, cr for ', 2f8.3,'):',$)
 2040 format('Input ',A20,' Map longitude and latitude'
     &     ,/,' (A to abort, cr for ', 2f8.3,'):',$)
      end
c
