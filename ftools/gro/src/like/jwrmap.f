c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C        SUBROUTINE JWRMAP(jflag1,jflag2,jflag3)
C
C
C  $Id: jwrmap.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C==============================================================================
C*    effect: The Lmap array, TMPMAP, is filled and, if requested, autoscaling
C*            calculated.
C			
C*                      
C==============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+             UPDATED:     by  JAE
C==============================================================================
C* Subroutine Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^^^
c* jflag1   logical, jflag1=T: Output to file with GRO J2000 style filename
c*                   passed to JMAPFINE as 1st parameter
C* jflag2   logical, passed to JMAPFINE as 2nd parameter and to
c		jfnderr1 as first parameter(value of restrict_counts)
C* jflag3   logical, jflag3=F single source, jflag3=T auto multi-source
C==============================================================================
C  $Log: jwrmap.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:10  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:35  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:29  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:33  jae
c Subroutine Module for like V5.00
c
c Changes:
c
c==============================================================================
      SUBROUTINE JWRMAP(jflag1,jflag2,jflag3)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
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
c
      logical jflag1,jflag2,jflag3,sgsnd,jjplot_sv,rptsv,calc_uncsv
      character srcN_sv*18,input*70
      logical jflag1_sv,jjscale_sv,jflag2_sv
      character LMAPFILESV*50
c     
      id = '$Id: jwrmap.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
      LOC='JWRMAP'

      if(jae_debug)print *,'Inside:',LOC

      jncnt = 0
      jncnt4 = 0
      jflag1_sv=jflag1
c     jflag2_sv=counts_fixed
      jflag2 = restrict_counts  ! changed 8/14/01  DLB
      rptsv = report
      calc_uncsv = calc_uncert
      LMAPFILESV=LMAPFILE
c
c---> If auto multi-source mode then branch to line 6001
c     
      if(jflag3)goto 6001
c
c---> set pixel, map totals, save initial values.
c
      srcN_sv = srcN
      srcL_sv = srcL
      srcB_sv = srcB
      if(jpos(502).ne.0)jncnt=jpos(502)
      srcN='GRO'
      if(.not.jbest)then
         best_x_tmp=srcL
         best_y_tmp=srcB
      endif
      do j=1,50
         LMAPFILE(j:j)=' '
      enddo
      cnts_sv=counts
      CALL PIXEL_SELECT(JFALSE,jblank)
      LikTotaled = JFALSE
      CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      jncnt=jpos(502)
      if(jncnt.eq.0)then
         CALL SRCTEST(JFALSE)
         jpos(502)=NSOURCE+1
         jncnt=NSOURCE+1
         svn_TS(jncnt)=TS
         sv_cnts(1,jncnt)=Counts
      endif
 1    continue
c
c--->  Get GRO J2000 style filename if output requested in jflag1
c--->  if a file of this name exists, erase it WITHOUT user interaction
c
      restrict_counts=jflag2
      if(jflag1)then
         srcN='GRO'
         CALL GETSRCNM()
         srcN(4:4)='_'
         LMAPFILE=srcN
         srcN = srcN_sv
      else
         LMAPFILE=' '
         srcN=srcN_sv
      endif
 602  continue
      if(jflag1)then
         in_dexj = index(LMAPFILE, ' ') - 1
         if(in_dexj.lt.1)in_dexj=1
         input=' to file: '//LMAPFILE(1:in_dexj)
      else
         input='                                        '
      endif
      srcN=srcN_sv
      if(jflag1)then
         write(*,'(" creating map of PSF ",i3,
     &        " source: ",A18,A28)')jncnt,srcN(1:18),input(1:28)
      else
         write(*,'(" Creating map of PSF ",i3," source: ",A18)')
     &        jncnt,srcN(1:18)
      endif
c
c----> If rescaling or map generation is needed then call JFNDERR1: jjplot=T
c----> instructs not to make the call to JFNDERR1.  note that jjplot passes
c----> through to JMAPFINE within common block locpos.copy
c
      if(.not.jjplot.and..not.jjscale)then
	 if(jae_jwrmap)print *,'jkpoints:',jkpoints
         call JFNDERR1(jflag2,JFALSE)
         print *,' Autoscaling completed'
	 if(jae_jwrmap)print *,'jkpoints:',jkpoints
         if(signal.ne.' ')then
            sv_sigsv(jncnt)=signal
            if(jae_jwrmap)then
               CALL ERROR(-1,LOC)
            else
               CALL ERROR(0,LOC)
            endif        
         endif
      endif
c     
      rad_map = pss_rad_map
      if(rad_map.lt.0.0001)then
c
         signal='R'
         SIGMSG=
     &        'ERROR:'//LOC//':rad_map returned as zero'
         if(jae_jwrmap)then
            CALL ERROR(-1,LOC)
         else
            CALL ERROR(0,LOC)
         endif
         print *,' setting side length to 3 degrees'
c     
         rad_map=1.5		
      endif
      print *,'Map edge size = ',(2*rad_map),' degrees'
      if(jae_jwrmap)print *,'jkpoints:',jkpoints
      signal=' '
      srcL=srcL_sv
      srcB=srcB_sv
      jjplot_sv=jjplot
c     
c---> Map generation is performed by JMAPFINE, output to file if jflag1=T
c
      jjscale_sv=jjscale
      jjscale=JTRUE
      call JFNDERR1(jflag2,JFALSE)
      if(jflag1)then
         jjplot=JTRUE
         CALL JMAPFINE(JTRUE,jflag2,rad_map)
         print *,' '
         jjplot=JFALSE
      endif
      jjscale=jjscale_sv
      if(.not.jflag1)jflag1=jflag1_sv
c
c----> Check for error condition on return of JFNDERR1
c
      if(signal.ne.' ')then
         sv_sigsv(jncnt)=signal
         if(jae_jwrmap)then
            CALL ERROR(-1,LOC)
         else
            CALL ERROR(0,LOC)
         endif        
      endif
c
      if(signal.ne.' ')then
         sgsnd=JTRUE
         sv_sigsv(jncnt)=signal
         if(jflag3)then
            print *,' ERROR TYPE ',signal,'GETTING ',srcN
            print *,' message: ',SIGMSG
            signal=' '
            SIGMSG=' '
            restrict_counts=jflag2_sv
            goto 6001
         endif
      endif
c
c---> Go to line 6001 if auto multi-source mode is in effect
c
      restrict_counts=jflag2_sv
      if(jflag3)goto 6001
c
c----> restore common block parameters, reselect pixel and total maps.
c----> this is done to prevent errors on return to accidentally set these
c----> values to unknown or improper values.
c
      srcN=srcN_sv
      srcL=srcL_sv
      srcB=srcB_sv
      counts=cnts_sv
      CALL PIXEL_SELECT(JFALSE,jblank)
      LikTotaled = JFALSE
      CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      LMAPFILE=LMAPFILESV
c
c---> return specialized signal if error have occured in any called routine.
c
      if(sgsnd.eqv.JTRUE)signal='e'
      SIGMSG=' '
      return
c     
c----> auto multi-source mode in effect.  Removes active sources one-by-one
c----> from the PSF map and then jumps to block above.  When a source is 
c----> removed from the PSF map, it is re-added to the map before the next
c----> source is checked.
c
c
c----> jncnt=0: first time through the loop.  Count active sources.
c----> if jncnt > 0 then line 6001 can only be reached if present source has
c----> has been removed from the map.
c
 6001 if(jncnt.eq.0)then
         if(NSOURCE.lt.1)goto 6101
         jncnt3 = 0
         do k = 1,NSOURCE
            if(SRC_PARMS(k,8).gt.0.5)jncnt3 = jncnt3 + 1
         enddo
         if(jncnt3.lt.1)goto 6102
         CALL MAPRST(XRRMAP,CTLMSZ1,CTLMSZ2)
         CALL MAPCPY(XRRMAP,bmap,CTLMSZ1,CTLMSZ2)
      endif
      in_dexk=0
      if(jncnt.gt.0)then    
         Counts = cnts_sv
         SRC_PARMS(jncnt,3)=cnts_sv
         CALL MAPCPY(bmap,XRRMAP,CTLMSZ1,CTLMSZ2)
         SRC_NAMES(jncnt) = srcN_sv
      endif
c
c----> if all active sources completed then return
c
      if(jncnt4.eq.jncnt3)return
c
c----> loop line 6002 until active source is found.
c
 6002 jncnt = jncnt + 1
      JJ_TMP_FLG=JFALSE
      if(jncnt.gt.NSOURCE)then
         if(sgsnd.eqv.JFALSE)then
            signal=' '
            SIGMSG=' '
         endif
         report = rptsv
         calc_uncert = calc_uncsv
         LMAPFILE=LMAPFILESV
         jj_tmp_flg=JFALSE
         return
      endif
      if(SRC_PARMS(jncnt,8).lt.0.5)then
         sv_sigsv(jncnt)='*'
         goto 6002
      endif
c
c----> This source is active.  Remove it from the PSF map then process through
c----> to line 1.
c
      jncnt4 = jncnt4 + 1
      counts=0.0
      srcN='GRO'
      srcN_sv = SRC_NAMES(jncnt)
      do j=1,50
         LMAPFILE(j:j)=' '
      enddo
      cnts_sv = SRC_PARMS(jncnt,3)
      srcL = SRC_PARMS(jncnt,1)
      srcB = SRC_PARMS(jncnt,2)
      srcL_sv = srcL
      srcB_sv = srcB
      CALL GETSRCNM()
c
c----> jbest=T: use best value for position;jbest=F: use max TS position
c
      if(.not.jbest)then
         best_x_tmp=srcL
         best_y_tmp=srcB
      else
         if(sv_dstsv(2,jncnt))then
            best_x_tmp = best_x(jncnt)
            best_y_tmp = best_y(jncnt)
         else
            best_x_tmp = srcL
            best_y_tmp = srcB
         endif
      endif
c
c----> Remove the source by replacing with zero count source.
c
      CALL PSFREPLACE(jncnt)
      jpos(502)=jncnt
      jjplot=JFALSE
      jj_tmp_flg=JTRUE
      CALL L_CHECK(srcL)
      CALL PIXEL_SELECT(JFALSE,jblank)
      LikTotaled = JFALSE
      CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      CALL SRCTEST(JFALSE)
      svn_TS(jncnt)=TS
      sv_cnts(1,jncnt)=Counts
      tst_name=sv_dstsv(6,jncnt)
      goto 1
c     
 6101 print *,' There are NO sources in the other PSF array !'
      signal = 'n'
      goto 6105
 6102 print *,' There are NO ACTIVE sources in the other PSF array !'
      signal = 'a'
 6105 print *,' I suggest you use the PA command to look at the',
     &     ' PSF array contents.'
      print *,' Returning to main menu'
      print *,' '
      report = rptsv
      calc_uncert = calc_uncsv
      jj_tmp_flg=JFALSE
      return
      end
c
