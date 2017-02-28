c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C        REAL function FUNKJ(PPJ,jflag)
C
C
C  $Id: funkj.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=============================================================================
C*      effect: Calculates -TS for position PPJ(1),PPJ(2)
C			                
C=============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+             UPDATED:     by  JAE
C=============================================================================
C* Function Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^
c* PPJ(2)   real long(RA) and LAT(DEC) at which to evaluate the Test Statistic
C* jflag    logical: if true enhanced(VERBOSE) screen output is produced
C*============================================================================
C* Returned Value
C* ^^^^^^^^^^^^^^
C* FUNKJ    real: the negative of the Test Statistic
c
c=============================================================================
c* Error Conditions
C* ^^^^^^^^^^^^^^^^ 
c
C* returns when signal has a value other than ''.  
C* Sets signal to 'Z' if TS == 0 more than 10 trials in a row.
c=============================================================================
C  $Log: funkj.f,v $
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
C  Revision 1.1  2002/04/16 20:27:31  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.4  1996/06/25  17:18:40  jae
c Created temporary variable to contain the test
c source longitude for testing if the source is in t
c The function L_CHECK is applied to variable tmpL and srcL
c in order to fix the "Source out of CTL" error code.
c
c Revision 5.3  1996/06/20  19:06:48  jae
c repaired typo on line 192
c
c Revision 5.2  1996/06/20  18:47:47  jae
c Added error analysis lines and return on error
c if position outside ROI.
c
c Revision 5.1  1996/02/29  20:47:55  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:05  jae
c Subroutine Module for like V5.00
c
c
c
c*****************************************************************************
c
      REAL function FUNKJ(PPJ,jflag)

C     Common Blocks Included:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
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
c
      character(80) id
      common /id/id
      logical jflag
      dimension PPJ(2)
c
      id = '$Id: funkj.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      LOC = 'FUNKJ'


      if(jae_funkj)print *,'Inside routine funckj'
c
c---> Set dt=(scale size)/2 and zenith aspect angle to pointing direction
c
      dt=CTLSCL/2.

      if(coord_sys.eq.'G')then
         pL = SC_LJJ
         pB = SC_BJJ
      else
         pL = SC_RAJ
         pB = SC_DECJ
      endif

      tmpL = ppJ(1)
      tmpB = ppJ(2)

      Call L_check(tmpL)

      aspect = gtcirc(tmpL,tmpB,pL,pB)
c
c---> Test if position is within Latitude ROI and maximum aspect cone.
c---> if either of these tests fail return with SIGNAL set.
c
      if(tmpB.lt.ROIorg(2)-dt.or.tmpB.gt.ROIend(2)+dt.or.
     &     aspect.gt.aspj)then
         if(aspect.gt.aspj)then
            SIGMSG='PSF ATTEMPTED OUTSIDE ASPECT CONE'
            signal='z'
            CALL ERROR(-2,LOC)
            FUNKJ=1
            return
         else
            signal='R'
            FUNKJ=1
            SIGMSG='PSF ATTEMPTED OUTSIDE ROI LIMITS'
            if(.not.fullmap)then
               if(tmpL.lt.CTLORG(1)-dt.or.tmpB.lt.
     &              CTLORG(2)-dt.or.tmpL.gt.CTLEND(1)+dt.or.tmpB.gt.
     &              CTLEND(2)+dt)then
                  SIGMSG='PSF ATTEMPTED OUTSIDE CTL LIMITS'
               endif
               CALL ERROR(-2,LOC)
               return
            endif
            if(tmpB.lt.CTLORG(2)-dt.or.tmpB.gt.CTLEND(2)+dt)then
               SIGMSG='PSF ATTEMPTED OUTSIDE CTL(2) LIMITS'
               FUNKJ=1
               CALL ERROR(-2,LOC)
               return
            endif
            CALL ERROR(-2,LOC)
            return
         endif
      endif
c
c---> Check Longitude ROI with extra condition for wrap around if fullmap ROI
c
      if(tmpL.lt.ROIorg(1)-dt.or.tmpL.gt.ROIend(1)+dt)then
         signal='R'
         FUNKJ=1
         SIGMSG='PSF ATTEMPTED OUTSIDE ROI LIMITS'
         if(tmpL.lt.CTLORG(1)-dt.or.tmpB.lt.
     &        CTLORG(2)-dt.or.tmpL.gt.CTLEND(1)+dt.or.tmpB.gt.
     &        CTLEND(2)+dt)then
            SIGMSG='PSF ATTEMPTED OUTSIDE CTL LIMITS'
            CALL ERROR(-2,LOC)
            return
         endif
         CALL ERROR(-2,LOC)
         return
      endif
c     
c
c
      srcL=ppJ(1)
      call l_check(srcL)
      srcB=ppJ(2)
      CALL MAPIXL(srcL,srcB,IL,JB)
      CALL MAPCOR(IL,JB,srcLpxcnt,srcBpxcnt)
      CALL PSMMAT(srcL-srcLpxcnt,srcB-srcBpxcnt)

      if (signal.ne.' ')then
         SIGMSG='ERROR in PSMMAT from '//LOC
         FUNKJ=2
         CALL ERROR(-2,LOC)
         return
      endif

      LikTotaled=JFALSE
c
      CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)

      if(signal.ne.' ')then
         SIGMSG='ERROR in LIKTOT from '//LOC
         FUNKJ=2
         CALL ERROR(-2,LOC)
         return
      endif
c
      Counts_previous=Counts
      Counts=0.
      CALL GASBIAS(JFALSE)

      if(signal.ne.' ')then
         CALL ERROR(-2,LOC)
         signal='f'
         SIGMSG='ERROR return from FUNKJ'
         FUNKJ=2
         return
      endif

      source_notlnL=-lnL
      Counts=Counts_previous
      CALL ALLFIT(JFALSE)
      TS = 2*(-lnL - source_notlnL)

      if(signal.ne.' ')then
         CALL ERROR(-2,LOC)
         signal='f'
         SIGMSG='ERROR return from FUNKJ'
         FUNKJ=2
         return
      endif

      FUNKJ = -TS
c     
      if(jae_funkj)then
         print *,'%%%%%%%%%%%%%%%%%%%%%%%'
         print *,' srcL: ',srcL
         print *,' srcB: ',srcB
         print *,'FUNKJ: ',FUNKJ
         print *,'%%%%%%%%%%%%%%%%%%%%%%%'
      endif
c
      return
      end
c
