c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C        SUBROUTINE JFNDERR2(jflag1,jflag2,jflag3)
C
C
C  $Id: jfnderr2.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C==============================================================================
C*   effect: Calculates an equivilant error circle for each active PSF
c          at 68% and 95% confidence.  Also fills arrays used by JREPORT
C==============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+             UPDATED:     by  JAE
C=============================================================================
C* Subroutine Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^^^
C* jflag1	logical:  when true use multi source mode
C* jflag2	logical:  when true enhance screen output
C* jflag3	logical:  flag passed to JWRMAP as second argument
C*============================================================================
C* Returned Values
C* ^^^^^^^^^^^^^^^
C*   none
c*============================================================================
C  $Log: jfnderr2.f,v $
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
c Revision 5.3  1997/03/08  19:46:21  jae
c NO changes made.  Checked out by mistake.
c
c Revision 5.2  1996/08/13  16:23:43  jae
c Setup call to AUTO_OUT if flag is true and
c performing on source list.  Cleaned up some
c commented out line which were no longer
c needed
c
c Revision 5.1  1996/02/29  20:48:17  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:13  jae
c Subroutine Module for like V5.00
c
C
c
c==========================================================================
      SUBROUTINE JFNDERR2(jflag1,jflag2,jflag3)

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
      logical jflag1,jflag2,jflag3,jrptsv,jcalcsv,sgsnd,jjscale_sv
      character srcN_sv*18,best_ch_tmp*2
      integer nscnt_68(4),nscnt_95(4)
c
      id = '$Id: jfnderr2.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      LOC='JFNDERR2'

      if(jae_debug)print *,'Inside:',LOC
c
c---> initialize values
c
      jrptsv=report
      jncnt=0
      jkpnts_sv=jkpoints
      jcalcsv=calc_uncert
      calc_uncert=JFALSE
      report=JFALSE
      signal=' '
      SIGMSG=' '
c
c---> switch for multi source mode (LPEA command)
c
      if(jflag1)goto 7001
c
c---> final initializations for single source mode
c
      jncnt=NSOURCE+1
      jpos(502)=jncnt
      srcL_sv = srcL
      srcB_sv = srcB
      CALL L_CHECK(srcL)
      CALL PIXEL_SELECT(JFALSE,jblank)
      LikTotaled = JFALSE
      CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
c     
c---> loop point for multi source mode
c
 1    continue
      print *,' '
      print *,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      print *,'%%%%%%%%%%%%%%%%--> PSF: ',jncnt
      print *,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      print *,' '
c
c---> pre loop initializations, set values passed from source test
c---> for use in JREPORT subroutine
c
      sv_sigsv(jpos(502))='*'
      sv_dstsv(4,jncnt)=JTRUE
      sv_dstsv(5,jncnt)=JTRUE
      jnmflg=JFALSE
      sv_sigsv(jncnt)='*'
      wgt_sum68=0
      wgt_sum95=0
      calc_uncert=JTRUE
      report = JFALSE
      report2 = JTRUE
      CALL SRCTEST(JFALSE)
      calc_uncert=JFALSE
      report2 = JFALSE
      sv_flx(1,jncnt)=1.e8*Counts/pss_expose
      sv_flx(2,jncnt)=1.e8*dCounts/pss_expose
      sv_cnts(1,jncnt)=Counts
      sv_cnts(2,jncnt)=dCounts
      sv_expose(jncnt)=pss_expose
      svn_TS(jncnt)=TS
      if(TS.ge.25)then
         sv_dstsv(3,jncnt)=JTRUE
      else
         sv_dstsv(3,jncnt)=JFALSE
      endif
      sv_params(1,jncnt)=Gmult
      sv_params(2,jncnt)=Gbias
      sv_upperlim(jncnt)=1.e8*Counts_limit/pss_expose
c
c---> initialize averages and sums to zero
c
      sum68 = 0
      sum95 = 0
      avex68=0
      avey68=0
      avex95=0
      avey95=0
      tmp_TS=TS
c
c---> check TS.  If too small jump to end with respective error flags set
c
      if(tmp_TS.lt.4)then
         avex68=srcL_sv
         avey68=srcB_sv
         avex95=srcL_sv
         avey95=srcB_sv
         best_x(jncnt)=srcL_sv
         best_y(jncnt)=srcB_sv
         sv_dstsv(4,jncnt)=JFALSE
         sv_dstsv(5,jncnt)=JFALSE
         sv_err68(jncnt)=-1
         sv_err95(jncnt)=-1
         sv_rad_68_min(jncnt)=0
         sv_rad_95_min(jncnt)=0
         sv_rad_68_max(jncnt)=0
         sv_rad_95_max(jncnt)=0
         sv_sigsv(jncnt)='F'
         if(tmp_TS.gt.1.)sv_sigsv(jncnt)='E'
         do i=1,jkpoints+1
            do j = 1,jkpoints+1
               JMAP=I+ (jkpoints+1)*(J-1)
               if(sv_sigsv(jncnt).eq.
     &              'f')TMPMAP(JMAP)=0
               if(sv_sigsv(jncnt).eq.
     &              'F')TMPMAP(JMAP)=1
               if(sv_sigsv(jncnt).eq.
     &              'E')TMPMAP(JMAP)=3
            enddo
         enddo
         goto 6990
      endif
c
c---> begin the calculation
c---> call JWRMAP to generate initial map and auto scale if appropriate
c
      best_x_tmp=srcL
      best_y_tmp=srcB
      print *,' '
      jbest=JFALSE
      print *,' PASS 1 No Files Written'
      print *,' '
      jkpoints=jkpnts_sv
      Counts=sv_cnts(1,jncnt)
      CALL JWRMAP(JFALSE,jflag3,JFALSE)
      if(jae_jfnderr2)print *,'SRCN:',SRC_NAMES(jncnt),'jkpoints:',
     &     jkpoints
c
c---> function error found on return of JWRMAP set false map so problem
c---> can't be missed by user (unless the user completely ignores the output).
c
      if(sv_sigsv(jpos(502)).eq.'f')then
         do i=1,jkpoints+1
            do j = 1,jkpoints+1
               JMAP=I+ (jkpoints+1)*(J-1)
               TMPMAP(JMAP)=0
            enddo
         enddo
         goto 6990
      endif
c
c---> On error screen dump a message to alert the user to a problem
c
      if(signal.ne.' '.and.signal.ne.' ')then
         signal = sv_sigsv(jpos(502))
         CALL ERROR(0,LOC)
         signal = sv_sigsv(jpos(502))
         if(signal.eq.'e'.or.signal.eq.'S'.or.signal.eq.'M')
     &        signal=' '
         if(signal.ne.' ')then
            print *,' WARNING:  Deactivating PSF # ',jncnt
            print *,' Skipping to next active PSF '
            SRC_PARMS(jncnt,8)=0.1
            goto 2
         endif
      endif
c
c---> set map side length and center of map for analysis
c
      diameter = 2.0*pss_rad_map
      width=diameter/float(jkpoints)
      srcL=srcL_sv
      srcB=srcB_sv
      CALL PIXEL_SELECT(JFALSE,jblank)
      LikTotaled = JFALSE
      CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      cornerL=srcL-diameter/2.
      cornerB=srcB-diameter/2.
      if(sv_sigsv(jncnt).eq.' '.or.sv_sigsv(jncnt).eq.
     &     ' ')sv_sigsv(jncnt)='*'
      if(tmp_TS.lt.9)sv_sigsv(jncnt)='e'
c
c---> Verbose output of map statistics
c
      if(jae_jfnderr2)then
         jkcnt2=0
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
               if(iuse_Jtmpmap(JMAP).gt.0)jkcnt2=jkcnt2+1
               iuse_1 = iuse_Jtmpmap(JMAP)
               if(iuse_1.gt.0)then
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
     &        'Total Number of Pixels within the 95% contour:',jkcnt2
         print *,' '
      endif
c
c--->  PROGRAM PAUSES HERE IF JAE_JFNDERR2 SET TRUE
c
C     if(jae_jfnderr2)PAUSE
c
c---> get solid angle sums for non-disjointed contours.  Disjointedness is
c---> check by value of pixel at iuse_Jtmpmap(JMAP)
c
      wgtjsum68=0
      wgtjsum95=0
      do i=1,jkpoints+1
         do j = 1,jkpoints+1
            PL = cornerL + (I-1) * width
            PB = cornerB + (J-1) * width
            JMAP= I + (jkpoints+1)*(J-1)
            iuse_1 = iuse_Jtmpmap(JMAP)
            delta_TS = tmp_TS - TMPMAP(JMAP)
            dc1 = cos((PB + width/2 - 90.)*piej/180.)
            dc2 = cos((PB - width/2 - 90.)*piej/180.)
            domega = width*(dc1-dc2)*piej/180.
            wgtj=1
c     if(wgt_flg)wgtj=MAPVAL(TMPMAP,I,J,
c     & jkpoints+1,jkpoints+1)
            if(wgt_flg)wgtj=exp(-delta_TS)
            if(domega.lt.0)domega=-domega
            if(iuse_1.lt.9.and.iuse_1.gt.0)then
               wgtjsum68=wgtjsum68+wgtj*domega
               sum68 = sum68 + domega
               avex68 = avex68 + PL*domega*wgtj
               avey68 = avey68 + PB*domega*wgtj
            endif
c     
            if(iuse_Jtmpmap(JMAP).gt.0)then
               wgtjsum95=wgtjsum95+wgtj*domega
               sum95 = sum95 + domega
               avex95 = avex95 + PL*domega*wgtj
               avey95 = avey95 + PB*domega*wgtj
            endif
         enddo
      enddo
c
c---> get unweighted mean (center position) of contours
c
      if(sum68.le.0)then
         sv_dstsv(5,jncnt)=JFALSE
         sum68=piej
         avex68=srcL_sv
         avey68=srcB_sv
      else
         avex68=avex68/wgtjsum68
         avey68=avey68/wgtjsum68
      endif
      if(sum95.le.0)then
         sv_dstsv(4,jncnt)=JFALSE
         sum95=piej
         avex95=srcL_sv
         avey95=srcB_sv
      else
         avex95=avex95/wgtjsum95
         avey95=avey95/wgtjsum95
      endif
      if(jae_jfnderr2)then
         write(*,*)' '
         write(*,'("ave(x,y) 68% : ",f8.2,",",f8.2)') 
     &        avex68,avey68
         write(*,'("ave(x,y) 95% : ",f8.2,",",f8.2)') 
     &        avex95,avey95
         write(*,*)' '
         write(*,*)' '
      endif
      sv_tmp_68_x(jncnt) = avex68
      sv_tmp_68_y(jncnt) = avey68
      sv_tmp_95_x(jncnt) = avex95
      sv_tmp_95_y(jncnt) = avey95
c
c---> initialize values
c
      sv_rad_68_min(jncnt)= 1000
      sv_rad_68_max(jncnt)= -1
      sv_rad_95_min(jncnt)= 1000
      sv_rad_95_max(jncnt)= -1
c
c---> Now get minimum and maximum radii of contours to respective best
c---> estimate position.  Also count the number of contour pixels along
c---> each map edge.  The figure of merit is calculated from the algorithm
c---> FOM=radmin*radmax/(rad*rad), where rad=sqrt(contour area/Pi).
c---> A contour is considered good (error circle is reasonable) if
c---> The fraction of the contour along any edge is less than 10% and if the
c---> FOM is in the range: 0.7 < FOM < 1.3.  For a 21 pixel/side map this
c---> means no more than two pixels on any side elements of the contour, ie:
c---> the map scaling was too small or the contour is NOT containable.
c---> NOTE: FOM is identically one for a perfect ellipse or a circle
c
c---> initialize counting and sum variables
c
      tmp_jsum68=0
      tmp_jsum95=0
      do k2 = 1,4
         nscnt_68(k2)=0
         nscnt_95(k2)=0
      enddo
c
c---> loop over the map array and when on a side sum solid angle and count
c---> pixels
c
      do i=1,jkpoints+1
         do j = 1,jkpoints+1
            JMAP= I + (jkpoints+1)*(J-1)
            PL = cornerL + (I-1) * width
            PB = cornerB + (J-1) * width
            delta_TS = tmp_TS - TMPMAP(JMAP)
            dc1 = cos((PB + width/2 - 90.)*piej/180.)
            dc2 = cos((PB - width/2 - 90.)*piej/180.)
            domega = width*(dc1-dc2)*piej/180
            iuse_1 = iuse_Jtmpmap(JMAP)
            if(iuse_1.gt.0)then
               if(j.eq.1.or.j.eq.
     &              jkpoints+1.or.i.eq.1.or.i.eq.jkpoints+1)then
                  if(i.eq.1)k1=1
                  if(i.eq.jkpoints+1)k1=2
                  if(j.eq.1)k1=3
                  if(j.eq.jkpoints+1)k1=4
                  nscnt_95(k1)=nscnt_95(k1)+1
                  if(iuse_1.le.6)nscnt_68(k1)=nscnt_68(k1)+1
                  if(delta_TS.le.6.0.and.sv_sigsv(jncnt).ne.'6')
     &                 sv_sigsv(jncnt)='9'
                  if(delta_TS.le.2.3)sv_sigsv(jncnt)='6'
                  if(sv_sigsv(jncnt).eq.'6'.or.sv_sigsv(jncnt).eq.'9')
     &                 sgsnd=JTRUE
                  tmp_jsum95 = tmp_jsum95+domega
                  if(iuse_1.le.6)tmp_jsum68 = tmp_jsum68+domega
               endif
            endif
            rad1 = gtcirc(PL,PB,avex68,avey68)
            rad2 = gtcirc(PL,PB,avex95,avey95)
            if(jae_jfnderr2.and.(rad1.lt.0.or.rad2.lt.0))then
               print *,'rad1(68):',rad1
               print *,'rad2(95):',rad2
            endif
            if(delta_TS.le.2.3.and.rad1.ge.sv_rad_68_max(jncnt).and.
     &           iuse_1.gt.0)then
               sv_rad_68_max(jncnt)=rad1
               PLsav68max=PL
               PBsav68max=PB
            endif
            if(delta_TS.ge.2.3.and.rad1.le.sv_rad_68_min(jncnt))then
                 sv_rad_68_min(jncnt)=rad1
                 PLsav68min=PL
                 PBsav68min=PB
              endif
              if(delta_TS.le.6.0.and.rad2.ge.sv_rad_95_max(jncnt).and.
     &             iuse_1.gt.0)then
                 sv_rad_95_max(jncnt)=rad2
                 PLsav95max=PL
                 PBsav95max=PB
              endif
              if(delta_TS.ge.6.0.and.rad2.le.sv_rad_95_min(jncnt))then
                   sv_rad_95_min(jncnt)=rad2
                   PLsav95min=PL
                   PBsav95min=PB
		endif
             enddo
          enddo
c
c---> now make pixel count tests and set flags accordingly
c
          sv_dstsv(9,jncnt)=JFALSE
          sv_dstsv(10,jncnt)=JFALSE
          if(sv_dstsv(4,jncnt))then
             sv_dstsv(9,jncnt)=JTRUE
             tmp_jsum95 = tmp_jsum95/sum95
             if(tmp_jsum95.gt.0.05)sv_dstsv(4,jncnt)=JFALSE
             xtmpa1 = float(nscnt_95(1))/float(jkpoints+1)
             xtmpa2 = float(nscnt_95(2))/float(jkpoints+1)
             xtmpa3 = float(nscnt_95(3))/float(jkpoints+1)
             xtmpa4 = float(nscnt_95(4))/float(jkpoints+1)
             if(xtmpa1.gt.0.10)then
                sv_dstsv(4,jncnt)=JFALSE
             elseif(xtmpa2.gt.0.10)then
                sv_dstsv(4,jncnt)=JFALSE
             elseif(xtmpa3.gt.0.10)then
                sv_dstsv(4,jncnt)=JFALSE
             elseif(xtmpa4.gt.0.10)then
                sv_dstsv(4,jncnt)=JFALSE
             endif
          endif
          if(sv_dstsv(5,jncnt))then
             sv_dstsv(10,jncnt)=JTRUE
             tmp_jsum68 = tmp_jsum68/sum68
             if(tmp_jsum68.gt.0.05)then
                sv_dstsv(5,jncnt)=JFALSE
                sv_dstsv(4,jncnt)=JFALSE
             endif
             xtmpa1 = float(nscnt_68(1))/float(jkpoints+1)
             xtmpa2 = float(nscnt_68(2))/float(jkpoints+1)
             xtmpa3 = float(nscnt_68(3))/float(jkpoints+1)
             xtmpa4 = float(nscnt_68(4))/float(jkpoints+1)
             if(xtmpa1.gt.0.10)then
                sv_dstsv(4,jncnt)=JFALSE
                sv_dstsv(5,jncnt)=JFALSE
             elseif(xtmpa2.gt.0.10)then
                sv_dstsv(4,jncnt)=JFALSE
                sv_dstsv(5,jncnt)=JFALSE
             elseif(xtmpa3.gt.0.10)then
                sv_dstsv(4,jncnt)=JFALSE
                sv_dstsv(5,jncnt)=JFALSE
             elseif(xtmpa4.gt.0.10)then
                sv_dstsv(4,jncnt)=JFALSE
                sv_dstsv(5,jncnt)=JFALSE
             endif
          endif
c     
 6990     continue
          sv_dstsv(8,jncnt)=JFALSE
c
c---> screen output on error
c
          if(sv_sigsv(jpos(502)).eq.'f')then
             sv_dstsv(8,jncnt)=JTRUE
             print *,' '
             print *,' %%%%% MAPPING ERROR CODE DETECTED: code - f %%'
             print *,' '
          endif
          if(tmp_TS.lt.9)then
             print *,' '
             if(tmp_TS.le.1)then
                print *,' %%%%% ERROR MAPPING WARNING CODE DETECTED:',
     &               ' code - F %%%%% '
             elseif(tmp_TS.gt.1.and.tmp_TS.le.4)then
                print *,' %%%%% ERROR MAPPING WARNING CODE DETECTED:',
     &               ' code - E %%%%% '
             elseif(tmp_TS.gt.4)then
                print *,' %%%%% ERROR MAPPING WARNING CODE DETECTED:',
     &               ' code - e %%%%% '
             endif
             print *,' '
          endif
c
c---> convert to arc minutes, calculate equivilant error circle 
c
          sv_rad_68_max(jncnt)=60*sv_rad_68_max(jncnt)
          sv_rad_68_min(jncnt)=60*sv_rad_68_min(jncnt)
          sv_rad_95_max(jncnt)=60*sv_rad_95_max(jncnt)
          sv_rad_95_min(jncnt)=60*sv_rad_95_min(jncnt)
          sv_dstsv(2,jncnt)=JTRUE
          print *,' '
          print *,' PSF #: ',jncnt,' Maximum TS value: ',tmp_TS
          if(sv_sigsv(jncnt).ne.'*')print *,' Output Code: ',
     &         sv_sigsv(jncnt)
          print *,' position of maximum TS value: ',srcL_sv,' ',srcB_sv
          print *,' '
          sv_rad_sum_68(jncnt)=sum68
          sv_err68(jncnt)=60*sqrt(sum68/piej)*180/piej
          sv_rad_sum_95(jncnt)=sum95
          sv_err95(jncnt)=60*sqrt(sum95/piej)*180/piej
          if(sv_err68(jncnt).gt.0)then
             if(sv_rad_68_min(jncnt).lt.1.e-4)
     &            sv_rad_68_min(jncnt)=sv_err68(jncnt)
             if(sv_rad_68_max(jncnt).lt.1.e-4)
     &            sv_rad_68_max(jncnt)=sv_err68(jncnt)
             sv_schar_68(jncnt)=sv_rad_68_min(jncnt)*
     &            sv_rad_68_max(jncnt)/(sv_err68(jncnt)*sv_err68(jncnt))
          else
             sv_schar_68(jncnt)=0
          endif
          if(sv_err95(jncnt).gt.0)then
             if(sv_rad_95_min(jncnt).lt.1.e-4)
     &            sv_rad_95_min(jncnt)=sv_err95(jncnt)
             if(sv_rad_95_max(jncnt).lt.1.e-4)
     &            sv_rad_95_max(jncnt)=sv_err95(jncnt)
             sv_schar_95(jncnt)=sv_rad_95_min(jncnt)*
     &            sv_rad_95_max(jncnt)/(sv_err95(jncnt)*sv_err95(jncnt))
          else
             sv_schar_95(jncnt)=0
          endif
c
c---> more screen IO and then test FOM for the 68% and 95% contours
c
          if(jae_jfnderr2)then
             print *,' '
             print *,'-----------------------------------'
             print *,'68%min:',sv_rad_68_min(jncnt),' Position:',
     &            PLsav68min, ', ',PBsav68min
             print *,'68%max:',sv_rad_68_max(jncnt),' Position:',
     &            PLsav68max, ', ',PBsav68max
             print *,'68%rad:',sv_err68(jncnt),' Position:',
     &            avex68, ', ',avey68
             print *,'95%min:',sv_rad_95_min(jncnt),' Position:',
     &            PLsav95min, ', ',PBsav95min
             print *,'95%max:',sv_rad_95_max(jncnt),' Position:',
     &            PLsav95max, ', ',PBsav95max
             print *,'95%rad:',sv_err95(jncnt),' Position:',
     &            avex95, ', ',avey95
             print *,'-----------------------------------'
             print *,' '
          endif
          print *,' 68% characteristic radius: ',sv_err68(jncnt),
     &         '  Figure of merit: ',sv_schar_68(jncnt)
          print *,' 95% characteristic radius: ',sv_err95(jncnt),
     &         '  Figure of merit: ',sv_schar_95(jncnt)
          print *,' '
          sv_dstsv(7,jncnt)=JFALSE
          if(sv_schar_95(jncnt).gt.1.3.or.sv_schar_95(jncnt).lt.0.7)then
             sv_dstsv(4,jncnt)=JFALSE
             sv_sigsv(jncnt)='9'
             sv_dstsv(7,jncnt)=JTRUE
          endif
          if(sv_schar_68(jncnt).gt.1.3.or.sv_schar_68(jncnt).lt.0.7)then
             sv_dstsv(5,jncnt)=JFALSE
             sv_sigsv(jncnt)='6'
             sv_dstsv(7,jncnt)=JTRUE
          endif
c
c---> choose best estimate position try 95%, if failure try 68% then
c---> use TS position if both 68% and 95% fail FOM and pixel count tests
c
          if(sv_dstsv(4,jncnt))then
             best_x(jncnt) = avex95
             best_y(jncnt) = avey95
             best_choice(jncnt)='95'
          elseif(.not.sv_dstsv(4,jncnt).and.sv_dstsv(5,jncnt))then
             best_x(jncnt) = avex68
             best_y(jncnt) = avey68
             best_choice(jncnt)='68'
          else
             best_x(jncnt) = srcL_sv
             best_y(jncnt) = srcB_sv
             best_choice(jncnt)='TS'
          endif
          best_x_tmp=best_x(jncnt)
          best_y_tmp=best_y(jncnt)
c     
c---> output results to screen
c
          print *,' best_x: ',best_x(jncnt)
          print *,' best_y: ',best_y(jncnt)
          print *,' '
          if(best_choice(jncnt).ne.'TS')then
             print *,' Best Choice Contour: ',best_choice(jncnt),'%'
          else
             print *,' Best Choice Contour: ',best_choice(jncnt)
          endif
c     print *,' '
          print *,'  flux: ',sv_flx(1,jncnt),sv_flx(2,jncnt)
          print *,'Counts: ',sv_cnts(1,jncnt),sv_cnts(2,jncnt)
          print *,'    UL: ',sv_upperlim(jncnt)
          print *,'expose: ',sv_expose(jncnt)/1.e7
          if(jae_jfnderr2)print *,'jkpoints:',jkpoints
c
c
          jj=jncnt
 2919     if(best_choice(jj).ne.'95')then
             print *,' '
             print *,' This PSF has a poor 95% contour.',
     &            '  (TS or ID) Sourcename: ',SRC_NAMES(jj)
             print *,' '
          endif
c
          print *,' Choose best position from:'
          print *,' '
          print *,' TS: ',SRC_PARMS(jj,1),' ',SRC_PARMS(jj,2)
          print *,' 68: ',sv_tmp_68_x(jj),' ',sv_tmp_68_y(jj)
          print *,' 95: ',sv_tmp_95_x(jj),' ',sv_tmp_95_y(jj)
          print *,' '
          best_ch_tmp='  '
c
c---> If auto decision mode jump past user input section
c
          if(full_auto_lp)goto 2969
          write(*,
     &         '(" Enter TS, 68 or 95 ",
     *         "(cr to keep present selection): "$)')
          read(LU(12),'(A2)')best_ch_tmp(1:2)
          if(best_ch_tmp(1:2).ne.'TS'.and.best_ch_tmp(1:2).ne.'ts'.and.
     &         best_ch_tmp(1:2).ne.'68'.and.
     *         best_ch_tmp(1:2).ne.'95'.and.
     &         best_ch_tmp(1:2).ne.' '.and.best_ch_tmp(1:2).ne.' ')then
             if(best_ch_tmp(1:1).eq.'A'.or.best_ch_tmp(1:1).eq.'a')then
		if(jae_debug)return
             endif
             print *,' '
             print *,' Choose appropriate selection from ",
     *            "the list above !'
             goto 2919
          endif
          if(best_ch_tmp(1:1).eq.'T'.or.best_ch_tmp(1:1).eq.'t')then
             best_x(jj) = SRC_PARMS(jj,1)
             best_y(jj) = SRC_PARMS(jj,2)
             best_choice(jj)='TS'
          elseif(best_ch_tmp(1:1).eq.'6')then
             best_x(jj) = sv_tmp_68_x(jj)
             best_y(jj) = sv_tmp_68_y(jj)
             best_choice(jj)='68'
          elseif(best_ch_tmp(1:1).eq.'9')then
             best_x(jj) = sv_tmp_95_x(jj)
             best_y(jj) = sv_tmp_95_y(jj)
             best_choice(jj)='95'
          endif
cendif
c
 2969     continue
c
c---> set final values, get GRO J2000 source name at best estimate position
c
          if(full_auto_lp)then
             write(*,*)' '
             write(*,'("Auto-select Best choice:",a)') 
     &            best_choice(jj)
             write(*,*)' '
          endif
          jjplot=JFALSE
          if(best_choice(jj).eq.'TS')jjplot=JTRUE
          print *,' '
          srcL=best_x(jj)
          srcB=best_y(jj)
          if(coord_sys.eq.'C')then
             call CELGALD('CG',srcL,srcB,sv_cel_long(jj),
     *            sv_cel_lat(jj),iret)
             sv_gal_long(jj)=srcL
             sv_gal_lat(jj)=srcB
          else
             call CELGALD('GC',sv_cel_long(jj),sv_cel_lat(jj),srcL,
     *            srcB,iret)
             sv_cel_long(jj)=srcL
             sv_cel_lat(jj)=srcB
          endif
          srcN=SRC_NAMES(jncnt)
          tst_name=sv_dstsv(6,jj)
          best_x_tmp=best_x(jncnt)
          best_y_tmp=best_y(jncnt)
          jj_tmp_flg=JTRUE
          jpos(502)=jncnt
c     
c---> If map output requested generate and write to file a new map
c---> centered on the best estimate position.  The existing map is written
c---> and NOT generated if the TS position is the best estimate since this
c---> is the current map in the TMPMAP array
c     
          jbest=JTRUE
          if(jflag2)then
             jjscale_sv=jjscale
             jjscale=JTRUE
             srcL=srcL_sv
             srcB=srcB_sv
             Counts=sv_cnts(1,jncnt)
             print *,' PASS 2: centered at: ',best_x(jj),' ',best_y(jj)
             CALL JWRMAP(JTRUE,jflag3,JFALSE)
             jjscale=jjscale_sv
          endif
	
c
c---> reset values for return or loop end and, if in multi source mode jump
c---> to line 7001 to pick up next active PSF source
c
 2        tst_name=JFALSE
          jjplot=JFALSE
          jj_tmp_flg=JFALSE
          print *,' '
c     jjscale=JFALSE
          if(jflag1)goto 7001
c
          srcL=srcL_sv
          srcB=srcB_sv
          CALL PIXEL_SELECT(JFALSE,jblank)
          LikTotaled = JFALSE
          CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
          if(autooutput)call auto_out(3)
c     
c
          return
c
c---> perform first pass counting if no active sources or just no sources
c---> in PSF then return.  Otherwise loop to find first active PSF source
c---> 
c
 7001     if(jncnt.eq.0)then
             if(NSOURCE.lt.1)goto 7101
             if(NSOURCE.gt.500)goto 7103
             jncnt3 = 0
             jncnt4 = 0
             CALL MAPRST(XRRMAP,CTLMSZ1,CTLMSZ2)
             CALL MAPCPY(XRRMAP,bmap,CTLMSZ1,CTLMSZ2)
             do k = 1,NSOURCE
                if(SRC_PARMS(k,8).gt.0.5)jncnt3 = jncnt3 + 1
             enddo
             if(jncnt3.lt.1)goto 7102
          endif
          if(jncnt.gt.0)then
             srcL=srcL_sv
             srcB=srcB_sv
             Counts = cnts_sv
             SRC_PARMS(jncnt,3)=cnts_sv
c     
             if(srcN_sv(1:3).ne.'GRO')srcN = srcN_sv
             if(srcN(1:3).eq.'GRO')SRC_NAMES(jncnt)=srcN
             CALL PSFREPLACE(jncnt)
             CALL MAPCPY(bmap,XRRMAP,CTLMSZ1,CTLMSZ2)
             if(autooutput)call auto_out(3)
          endif
 7002     jncnt = jncnt + 1
          if(jncnt.gt.NSOURCE.or.jncnt4.ge.jncnt3)then
             if(sgsnd.eqv.JTRUE)signal='e'
             SIGMSG=' '
             report = jrptsv
             calc_uncert = jcalcsv
             return
          endif
          if(SRC_PARMS(jncnt,8).lt.0.5)then
             sv_sigsv(jncnt)=' '
             if(sv_dstsv(2,jncnt).eqv.JFALSE)best_choice(jncnt)='  '
             goto 7002
          endif
c
c---> active source found. Remove it from the PSF map and jump to calculation
c---> section
c
          jncnt4 = jncnt4 + 1
          Counts = 0
          srcN_sv=SRC_NAMES(jncnt)
          srcN = srcN_sv
          cnts_sv = SRC_PARMS(jncnt,3)
          srcL = SRC_PARMS(jncnt,1)
          srcB = SRC_PARMS(jncnt,2)
          srcL_sv = srcL
          srcB_sv = srcB
          CALL PSFREPLACE(jncnt)
c     CALL PSFBLD
          srcL = srcL_sv
          srcB = srcB_sv
          Counts = cnts_sv
          CALL PIXEL_SELECT(JFALSE,jblank)
          LikTotaled = JFALSE
          CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
          jpos(502)=jncnt
          goto 1
c
c     Error codes and warnings, return on error
c
 7101     print *,' There are NO sources in the other PSF array !'
          signal = 'n'
          goto 7105
 7102     print *,' There are NO ACTIVE sources in the other ',
     *         'PSF array !'
          signal = 'a'
          goto 7105
 7103     print *,' '
          print *,' There are more than 500 sources ! N = ',NSOURCE
          print *,' THIS IS A SERIOUS ERROR !!!!'
          print *,' '
 7105     print *,' I suggest you use the PA command to look',
     &         ' at the PSF array contents.'
          print *,' Returning to the main menu'
          print *,' '
          report = jrptsv
          report2 = JFALSE
          calc_uncert = jcalcsv
          return
          end
c
