c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C        SUBROUTINE JFNDERR1(jflag1,jflag2)
C
C
C  $Id: jfnderr1.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=============================================================================
C*      effect: determines the scale size (rad_map) for autoscaling
C*                      
C=============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+             UPDATED:     by  JAE
C=============================================================================
C* Subroutine Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^^^
C*  jflag1   logical value of counts_fixed passed to jmapfine
C*  jflag2   logical NO LONGER USED
C
C*    Note:  At present subroutine is allways called with: jflag1=T,jflag2=F
c=============================================================================
C* Returned Values
C* ^^^^^^^^^^^^^^^
C*  none
C*    Note: all parameters are passed through the common blocks
c=============================================================================
C  $Log: jfnderr1.f,v $
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
C  Revision 1.1  2002/04/16 20:27:33  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:48:15  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:11  jae
c Subroutine Module for like V5.00
c
c Changes:
c
c=============================================================================
c
      SUBROUTINE JFNDERR1(jflag1,jflag2)

C  Common blocks used:
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
      logical jflag1,jflag2,rptsv,calc_uncsv
      logical jp_ret,local_verbose
c
      if (tttflg.and.jae_jfnderr1)local_verbose=tttflg

c
      id = '$Id: jfnderr1.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      LOC='JFNDERR1'

      if (jae_jfnderr1)print *,' Inside:',LOC
c
c---> set initial values
c
      sv_rad_map=-1
      jp_ret=JFALSE
      rptsv = report
      calc_uncsv = calc_uncert
      report=JFALSE
      calc_uncert=JFALSE
      jncnt = 0
      srcL_sv = srcL
      srcB_sv = srcB
      jncnt=jpos(502)
c
c---> Autoscaling is not in effect, calculate arrays for error analysis
c
      if (jjscale) then
         jp_ret=JTRUE
         rad_map=pss_rad_map
      endif
      if (.not.jp_ret) then
         write(*,
     &        '(" Calculating error scale size for PSF ",i3,
     *        " source: ",A18)')
     &        jncnt,srcN
      endif
      CALL PSMMAT(srcL-srcLpxcnt,srcB-srcBpxcnt)
      if (signal.ne.' ') then
         if (jae_jfnderr1) then
            CALL ERROR(-1,LOC)
            print *,jkpoints
         else
            CALL ERROR(0,LOC)
         endif
	  SIGMSG='ERROR:JFNDERR1:PSMMAT: MAP ABORTED'
	  SIGNAL='P'
          return
       endif
       if (.not.jp_ret)rad_map =  1.5
       if (jpos(502).eq.0) then
          CALL SRCTEST(JFALSE)
          sv_cnts(1,NSOURCE+1)=Counts
          svn_TS(NSOURCE+1)=TS
          jpos(502)=NSOURCE+1
       endif
       tmp_TS=svn_TS(jpos(502))
       jkpnts_sv=jkpoints
       if (.not.jp_ret)jkpoints=12
       jp=0
c
 2     continue
       do jo=1,jkpoints+1
          do ko=1,jkpoints+1
             JOMAP= jo + (jkpoints+1)*(ko-1)
             iuse_Jtmpmap(JOMAP)=0
          enddo
       enddo
       jp=jp+1
       width=2*rad_map/float(jkpoints)
       diameter = 2*rad_map
       cornerB=best_y_tmp-diameter/2.
c
c---> set up arrays without output to file
c
       if (jae_jfnderr1) then
          print *,'srcL:',srcL,' srcB:',srcB
          print *,'Best_x:',best_x_tmp,' Best_y:',best_y_tmp
          print *,'TS(srcL,srcB)=',tmp_TS
          print *,'jpos(502):',jpos(502)
          if (jpos(502).gt.0) then
             print *,'Gmult:',sv_params(1,jpos(502)),
     &            '  Gbias:',sv_params(2,jpos(502))
             print *,'svn_TS:',svn_TS(jpos(502))
          endif
       endif
       CALL JMAPFINE(JFALSE,jflag1,rad_map)
       if (signal.ne.' ') then
c     
          sv_sigsv(jpos(502))=signal
          if (jae_jfnderr1) then
             call ERROR(-1,LOC)
          else
             CALL ERROR(0,LOC)
          endif
          jkpoints=jkpnts_sv
          return
       endif
c
c---> jpos(600) contains the bin number for the array maximum
c---> iuse_Jtmpmap contains 1=map max, 6: 68% within contour, 9: within 95% 
c---> contour otherwise content is zero.  Array is set in JMAPFINE
       CALL MAPMAX(TMPMAP,ILMAX,IBMAX,TSMAX,JKPOINTS+1,JKPOINTS+1)
       JPOSTMP=ILMAX + (JKPOINTS+1)*(IBMAX-1)
c     iuse_Jtmpmap(jpos(600))=1
       iuse_Jtmpmap(jpos(600))=1
       if (jae_jfnderr1)write(*,'("jpos:",i8,"  new J:",i8)') 
     &      jpos(600),JPOSTMP
c
 205   continue
c
c---> set initial values for this loop
c
       jkcnt2 = 0
       jkcnt3 = 0
       sum1=0
       sum2=0
       jkcnt=0
       jkpoints2 = (jkpoints+1)*(jkpoints+1)
c
c---> get sum of map solid angle, sum2, and sum of solid angle within 68%
c---> contour, sum1.  These values are used later to determine next loop
c---> scale size.  The two nested loops continue until no more 
C---> non-disjointed are found winthin the 95% contour
c
       do j = 1,jkpoints+1
          do i = 1,jkpoints+1
             PB = cornerB + (J-1) * width
             JMAP= I + (jkpoints+1)*(J-1)
             dc1 = cos((PB + width/2 - 90.)*piej/180.)
             dc2 = cos((PB - width/2 - 90.)*piej/180.)
             domega = width*(dc1-dc2)*piej/180.
             if (domega.lt.0)domega=-domega
             sum2=sum2+domega
cc
c
             delta_TS = tmp_TS - TMPMAP(JMAP)
             if (delta_TS.lt.6.0.and.iuse_Jtmpmap(JMAP).lt.1) then
                do l = -1,1
                   do k = -1,1
c     if (.not.(l.eq.0.and.k.eq.0)) then
c     if (l.eq.0.and.k.eq.0) then
                      if (1.eq.1) then
c     goto 2101
c     else
                         KMAP = I + k + (jkpoints+1)*(J+l-1)
c     
c Changed if statement 12-6-95 JAE  old one is commented out
c
c     if (KMAP.gt.0.and.KMAP.le.jkpoints2.and.
c     & (I+k).gt.0.and.(J+l).gt.0) then
                         if ((I+k).le.jkpoints+1.and.(J+l).le.
     &                        jkpoints+1.and.(I+k).gt.0.and.
     *                        (J+l).gt.0) then
                            if (iuse_Jtmpmap(KMAP).gt.0) then
                               jkcnt=jkcnt+1
                               iuse_Jtmpmap(JMAP) = 9
c     
c     12-6-95 JAE
c     Changed if to include iuse_Jtmpmap(KMAP).lt.9 statement
c
                               if (delta_TS.le.2.3.and.
     &                              iuse_Jtmpmap(KMAP).lt.9)
     *                              iuse_Jtmpmap(JMAP)=6
c     
c     
                               if (iuse_Jtmpmap(KMAP).eq.6)goto 210
                            endif
                         endif
                      endif
 2101                 continue
                   enddo
                enddo
             endif
 210         continue
c
          enddo
       enddo
       if (jae_jfnderr1) then
          print *,'A jp: ',jp
          print *,'  jkcnt1,2,3: ',jkcnt1,jkcnt2,jkcnt3
       endif
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c
c     new lines added (this segment for speed) 12-6-95 JAE
c     
       do j = jkpoints+1,1,-1
          do i = jkpoints+1,1,-1
             PB = cornerB + (J-1) * width
             JMAP= I + (jkpoints+1)*(J-1)
c     if (local_verbose) then
c     print *,' '
c     print *,' Before:'
c     print *,' ---------'
c     do l = -1,1
c     print *,' '
c     do k = -1,1
c     KMAP = I + k + (jkpoints+1)*(J+l-1)		
c     write(*,'(i2$)')iuse_Jtmpmap(KMAP)
c     enddo
c     enddo
c     print *,' '
c     print *,' ---------'
c     print *,' '
c     endif
cc
c
             delta_TS = tmp_TS - TMPMAP(JMAP)
             if (delta_TS.lt.6.0.and.iuse_Jtmpmap(JMAP).lt.1) then
                do l = 1,-1,-1
                   do k = 1,-1,-1
c     if (.not.(l.eq.0.and.k.eq.0)) then
c     if (l.eq.0.and.k.eq.0) then
                      if (1.eq.1) then
c     goto 2111
c     else
                         KMAP = I + k + (jkpoints+1)*(J+l-1)
c     
c     Changed if statement 12-6-95 JAE  old one is commented out
c     
c     if (KMAP.gt.0.and.KMAP.le.jkpoints2.and.
c     & (I+k).gt.0.and.(J+l).gt.0) then
                         if ((I+k).le.jkpoints+1.and.(J+l).le.
     &                        jkpoints+1.and.(I+k).gt.0.and.
     *                        (J+l).gt.0) then
                            if (iuse_Jtmpmap(KMAP).gt.0) then
                               jkcnt=jkcnt+1
                               iuse_Jtmpmap(JMAP) = 9
                               if (delta_TS.le.2.3.and.
     &                              iuse_Jtmpmap(KMAP).lt.9)
     *                              iuse_Jtmpmap(JMAP)=6
                               if (iuse_Jtmpmap(KMAP).eq.6)goto 211
                            endif
                         endif
                      endif
 2111                 continue
                   enddo
                enddo
             endif
 211         continue
             if (iuse_Jtmpmap(JMAP).gt.0) then
                jkcnt2=jkcnt2+1
                if (iuse_Jtmpmap(JMAP).lt.9) then
                   sum1 = sum1 + domega
                   jkcnt3=jkcnt3+1
                endif
             endif
c     if (jae_jfnderr1.and.iuse_Jtmpmap(JMAP).lt.1) then
c     print *,' '
c     print *,' After:'
c     print *,' ---------'
c     do l = -1,1
c     print *,' '
c     do k = -1,1
c     KMAP = I + k + (jkpoints+1)*(J+l-1)		
c     write(*,'(i2$)')iuse_Jtmpmap(KMAP)
c     enddo
c     enddo
c     print *,' '
c     print *,' ---------'
c     print *,' '
c     endif
c
          enddo
       enddo
cc
c
       if (local_verbose) then
          jkcnt5=0
          print *,' '
          write(6,'(31("="))')
          print *,' '
          print *,' '
          do j=1,jkpoints+1
             write(6,'(/)')
             do i = 1,jkpoints+1
                PL = cornerL + (I-1) * width
                PB = cornerB + (J-1) * width
                JMAP= I + (jkpoints+1)*(J-1)
                iuse_1 = iuse_Jtmpmap(JMAP)
                if (iuse_1.gt.0) then
                   jkcnt5=jkcnt5+1
                   write(6,'(i1$)')iuse_1
                else
                   write(6,'("-"$)')
                endif
             enddo
          enddo
          print *,' '
          print *,' '
          write(6,'(31("="))')
          print *,
     &         'Total Number of Pixels within the 95% contour:',jkcnt5
          print *,' '
       endif
c
c     end of new addition 12-6-95 JAE
c
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
       if (jae_jfnderr1) then
          print *,'B jp: ',jp
          print *,'  jkcnt1,2,3: ',jkcnt1,jkcnt2,jkcnt3
          print *,' continue: ',jkcnt
       endif
c
c----> if new positions are found within the 68% contour repeat proceedure
c
       if (jkcnt.ge.1)goto 205 
       if (jjscale)goto 4
       if (jp.lt.10) then
          print *,' '
          print *,'auto scaling iteration done: ',jp,
     &         ' with map side length ',2*rad_map
       else
          print *,' '
          print *,'auto scaling iteration done: ',jp,
     &         ' with map side length ',2*rad_map
          if (rad_map.gt.sv_rad_map)sv_rad_map=rad_map
          print *,' '
          print *,'Taking too many scaling iterations: ',jp
          print *,'Will accept value at iteration 20' 
          if (jp.ge.20) then
             if (rad_map.gt.5.0)sv_rad_map=5.0
             if (rad_map.gt.5.0)rad_map=5.0
             if (.not.jjscale)pss_rad_map=sv_rad_map
             goto 3
          endif
       endif
c
c---> if rat=sum1/sum2 == 1 then 68% contour solid angle is equal to 1/6 the
c---> solid angle of the entire map.  Permit ratio to be in the range 1/12 to
c---> 1/4.  If not repeat proceedure with rad_map = new map 1/2 side scale 
c---> which comes from the previous value of rad_map and the value of rat.
c
       sum1 = sum1*180*180/piej/piej
       sum2 = sum2*180*180/piej/piej
       rat = sum1/sum2
c     sum_min = 0.5*piej/36.
c     sum_max = 1.5*piej/36.
       sum_min = 0.9*piej/36.
       sum_max = 1.1*piej/36.
       if (jp_ret)goto 4
       jjtt = 0.25*float(jkpoints2)
       if (local_verbose) then
          write(6,'(31("="))')
          print *,'min:',sum_min,'  rat:',rat,' max:',sum_max
          print *,'rad_map:',rad_map
          print *,' new R: ',rad_map*sqrt(36.*rat/piej)
          print *,'jkcnt3:',jkcnt3,' ?> ',jjtt
          print *,gtcirc(0.,90.,srcL,abs(srcB)),' >? ',1.5*Ranal
          print *,'TS = ',tmp_TS,' ?> 16'
          write(6,'(31("="))')
       endif
       if (jkcnt3.gt.jjtt.and.gtcirc(0.,90.,srcL,abs(srcB)).gt.
     &      (1.5*Ranal).and.tmp_TS.gt.16) then
          if (local_verbose)print *,'switch 1'
          rad_map = rad_map*sqrt(36/piej)/2.
c     rad_map = rad_map*sqrt(piej/36.)/2.
          rad_map = 0.01*INT(rad_map*100.+0.5)
          goto 2
       endif
       if (rat.lt.sum_min.or.rat.gt.sum_max) then
          if (local_verbose)print *,'switch 2'
          rad_map = rad_map*sqrt(36.*rat/piej)
c     rad_map = rad_map*sqrt(piej/(rat*36.))
          rad_map = 0.01*INT(rad_map*100.+0.5)
          if (abs(rad_map).gt.5.0) then
             if (.not.jjscale)pss_rad_map = 5.0
             goto 3
          endif
          goto 2
       endif
c     if (.not.jjscale)pss_rad_map = rad_map
 3     continue
 4     srcL=srcL_sv
       srcB=srcB_sv
       jkpoints = jkpnts_sv
       jkpoints2 = (jkpoints+1)*(jkpoints+1)
       if (abs(rad_map).gt.5.0)rad_map=5.0
       if (.not.jjscale)pss_rad_map = rad_map
       if (jae_jfnderr1) then
          print *,'Final map half width: ',rad_map
       endif
       CALL PIXEL_SELECT(JFALSE,jblank)
       LikTotaled = JFALSE
       CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
       report = rptsv
       jp_ret=JFALSE
       calc_uncert = calc_uncsv
       CALL MAPIXL(srcL,srcB,ILL,JBB)
       CALL MAPCOR(ILL,JBB,srcLpxcnt,srcBpxcnt)
       CALL PSMMAT(srcL-srcLpxcnt,srcB-srcBpxcnt)
       if (signal.ne.' ') then
	  if (jae_jmapfine) then
             CALL ERROR(-1,LOC)
             print *,jkpoints
	  else
             CALL ERROR(0,LOC)
	  endif
	  SIGMSG='ERROR:JMAPFINE:PSMMAT: MAP ABORTED'
	  SIGNAL='P'
          return
       endif
       return
       end
c
