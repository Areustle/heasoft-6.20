C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C       SUBROUTINE JLOC_POS(input)
C
C
C  $Id: jloc_pos.f,v 1.3 2013/05/21 19:08:25 irby Exp $
c
C=============================================================================
C*      effect: Parses LPX[y[z]] commands to their respective subroutines
C*                      
C=============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+             UPDATED:     by  JAE
C=============================================================================
C* Subroutine Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^^^
C* input   character* 50
C*============================================================================
C* Returned Values
C* ^^^^^^^^^^^^^^^
C*   none
c*============================================================================
C  $Log: jloc_pos.f,v $
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
c Revision 5.9  1996/10/31  16:17:26  jae
c In order to avoid recursive calls a change
c was made such that input_id_pos('LPID#Sx')
c is now JSORT('Sx').  Note that the input
c variable to JSORT is char*10.
c
c Revision 5.8  1996/08/13  16:25:28  jae
c Setup call to AUTO_OUT independent of flag
c at return from error analysis routine.
c Also, cleaned up some commented out lines of
c code.
c Changed default number of pixels in mapping and
c error analysis routine to 21 pixels per side.
c
c Revision 5.7  1996/06/27  20:14:26  jae
c Revised values permitted in call to LPO[N]
c zero will now abort the routine.
c Also, set a placeholder comment for future
c addition of an output i/o subroutine call
c after the end of an LPO[N] command
c
c Revision 5.6  1996/06/19  16:23:39  jae
c No changes done.
c
c Revision 5.5  1996/04/08  16:12:36  jae
c Repaired naming of sources (srcN)
c
c Revision 5.4  1996/03/12  21:51:15  jae
c Set a testline output to check input() on
c call to input_id_pos()
c
c Revision 5.3  1996/02/29  23:05:31  jae
c repaired missing parenthesis on line 141
c
c Revision 5.2  1996/02/29  23:02:41  jae
c corrected error in call to function to_upper on lines 134
c 135 and 137
c
c Revision 5.1  1996/02/29  20:48:23  jae
c y
c
c Revision 5.0  1996/02/13  21:54:23  jae
c Subroutine Module for like V5.00
c
C
c
c==========================================================================

      SUBROUTINE JLOC_POS(input)

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
      character input*50,jtmpchr*10,jinp3,jinp4,tinput*50
      logical jflag1,jflag2,jflag3,jflag4
c
c
c--->       Set all constants and flags to initial call values
c
      id = '$Id: jloc_pos.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      jblank='                    '
      LOC='JLOC_POS'

      if(jae_jloc_pos)write(*,'(" In routine ",a)') LOC

      JVER=VER
      dt = CTLSCL/2.
      piej= 4.0*atan(1.)
      jbest=.true.
      if(psfminj.lt.0.1)psfminj = 9
      if(tolj.lt.0.001)tolj = 0.005
      if(actj_flg.lt.0.001)actj_flg=0.01
      report=.false.
      report2=.false.
      jadjflg=.false.
      jnmflg=.false.
c     jftflg=.false.
      jpsfflg1=.false.
      full_auto_lp=.false.
      jflag1=.false.
      jflag2=.false.
      jflag3=.false.
      jflag4=.false.
c     
c---> set map pointing direction by MAPTYPE
c
      if(MAPTYPE.eq.'SINGLE_POINTING')then
         jptype = .true.
         if(aspj.lt.0.001)aspj = 40.
         if(coord_sys.eq.'G'.and..not.JTRUE)then
            SC_LJJ = SC_LII
            SC_BJJ = SC_BII
            tmpL=SC_LII
            tmpB=SC_BII
            CALL CELGALD('GC',SC_RAJ,SC_DECJ,tmpL,tmpB,iiret)
         elseif(.not.JTRUE)then
            SC_RAJ = SC_RA
            SC_DECJ = SC_DEC
            tmpL = SC_RA
            tmpB = SC_DEC
            CALL CELGALD('CG',tmpL,tmpB,SC_LJJ,SC_BJJ,iiret)
         endif     
      else
c
c---> tmpL is used for ROIEND(1) in case of ROI wrap around in a fullsky map
c
         if(aspj.lt.0.001)aspj = 360.001
         jptype = .false.
         if(ROIORG(1).gt.ROIEND(1).and.fullmap)then
            tmpL=ROIEND(1)+360
         else
            tmpL=ROIEND(1)
         endif
         tmpL = (ROIORG(1) + tmpL)/2.
         tmpB = (ROIORG(2) + ROIEND(2))/2.
         if(coord_sys.eq.'G'.and..not.JTRUE)then
            SC_LJJ=tmpL
            SC_BJJ=tmpB
            CALL CELGALD('GC',SC_RAJ,SC_DECJ,tmpL,tmpB,iiret)
         elseif(.not.JTRUE)then
            SC_RAJ = tmpL
            SC_DECJ = tmpB
            CALL CELGALD('CG',tmpL,tmpB,SC_LJJ,SC_BJJ,iiret)
         endif
      endif
      jmapflg=.false.
      JTRUE=.true.
      JFALSE=.false.
      thelong=srcL
c
c--> Next lines set variables jinp3,jinp4 and makes them upper case
c--> jinp{n} = input(n:n) also set input to upper case
c
      jinp3=input(3:3)
      jinp4=input(4:4)
c
      CALL to_upper(jinp3)
      CALL to_upper(jinp4)
      do i=1,50
         CALL to_upper(input(i:i))
      enddo
c
c--> end of upper case block
c
c-->  The following block checks if the initial source position is
c-->  within the plot boundaries.  If outside the boundaries then
c-->  it will set the source position to plot center at user discretion.
c
      if(jinp3.ne.'O'.and.jinp3.ne.'R'.and.jinp3.ne.'I'.and.jinp3.ne.
     &     'F'.and.jinp4.ne.'A'.and.jinp4.ne.'S')then
         if(ROIORG(1)-dt.lt.0.or.thelong.lt.0)call L_check(thelong)
         if(thelong.lt.ROIORG(1)-dt.or.thelong.gt.ROIEND(1)+dt.or.
     &        srcB.lt.ROIORG(2)-dt.or.srcB.gt.ROIEND(2)+dt)then
            if(coord_sys.eq.'G')then
               pL = SC_LJJ
               pB = SC_BJJ
            else
               pL = SC_RAJ
               pB = SC_DECJ
            endif
            print *,' '
            print *,' MAP CENTER or POINTING IS: ',pL,',',pB
            print *,' Test position: ',srcL,',',srcB
            SIGMSG=
     &           'Warning: The position is outside of the map ROI'
            write(*,'("Enter <cr>",
     &           " to adjust source position to map center;")')
            write(*,'("Enter A to abort:"$)')
            read(LU(12),'(a)') jtmpchr
            numtmp = index( jtmpchr,' ' ) - 1
            if(numtmp.ne.0.and.(jtmpchr(1:1).eq.'A'.or.
     &           jtmpchr(1:1).eq.'a'))then
               signal = 'R'
               CALL ERROR(0,LOC)
               SIGMSG=' '
               return
            endif
            srcL = pL
            srcB = pB
            CALL PIXEL_SELECT(JFALSE,jblank)
            LikTotaled = JFALSE
            CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         endif
      else
         if(coord_sys.eq.'G')then
            pL = SC_LJJ
            pB = SC_BJJ
         else
            pL = SC_RAJ
            pB = SC_DECJ
         endif
         srcL = pL
         srcB = pB
         CALL PIXEL_SELECT(JFALSE,jblank)
         LikTotaled = JFALSE
         CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      endif
c
c--> end of ROI block
c
c
c--> The following lines set two flags: jadjflg and jnmflg
c--> from user request (& or N set true by input(4:4) )
c--> jadjflg=T add PSF to SRC_PARMS array
c--> jnmflg=T get GRO J2000 name for source if appropriate
c
c
      jtmpchr = ' '
      if(jinp4.eq.'&')then
         jadjflg=JTRUE
         tinput='LPID#T'
         CALL INPUT_ID_POS(tinput)
      endif
      if(jinp4.eq.'N'.or.jinp4.eq.
     &     '&'.or.input(3:4).eq.'ms'.or.input(3:4).eq.'MS')jnmflg=JTRUE
c
c------------------------------------------------------------------
c---> Following segment is a 'switch' for LPSx.  Call is to JFNDPOS
c
c
      if(jinp3.eq.'S')then
	 if(jnmflg)srcN='.        '
	 if(.not.jnmflg.and.(srcN.eq.' '.or.srcN.eq.'.'))jnmflg=.true.
         CALL JFNDPOS(verbose,JTRUE)
         if(signal.ne.' ')call ERROR(0,LOC)
         signal=' '
         return
c
c------------------------------------------------------------------
c--->  Following segment is a 'switch' for LPFx.  Call is to JFNDPOSA
c
c
      elseif(jinp3.eq.'F')then
         jadjflg=JTRUE
         if(jinp4.eq.'R')jpsfflg1=JTRUE
         CALL JFNDPOSA(verbose)
         if(jpsfflg1)call PSFBLD
         jpsfflg1=JFALSE
         jadjflg=JFALSE
         if(signal.ne.' ')call ERROR(0,LOC)
         signal=' '
         return
c
c------------------------------------------------------------------
c---> Following segment is a 'switch' for LPOx.  Call is to JOPT_POS
c
c
      elseif(jinp3.eq.'O')then
 18      jadjflg=JFALSE
         print *,' '
c
c----> User is prompted for maximum number of LPO loop iterations
c
         print *,'Enter maximum number of iterations ( >= 5)'
         write(*,'("<cr> for default=50; zero to abort:"$)')
         read(LU(12),'(a)') jtmpchr
         numcar = index( jtmpchr,' ' ) - 1
         if(numcar.eq.0)then
            mxxiter=50
         else
            read(jtmpchr(1:10),*,err=18)mxxiter
         endif
         if(mxxiter.lt.5.and.NSOURCE.gt.1)then
            if(mxxiter.le.0)return
            print *,'This value is too low. Enter a number >= 5'
            goto 18
         elseif(mxxiter.lt.1)then
            return
         endif
c
c----> User is prompted for autodecision return at mxxiter iterations
c
         print *,'Enter <cr> for automatic abort at ',
     &        mxxiter,' iterations'
         read(LU(12),'(a)') jtmpchr
         numcar = index( jtmpchr,' ' ) - 1
         if(numcar.eq.0)then
            full_auto_lp=JTRUE
         else
            full_auto_lp=JFALSE
         endif
c
c----> call optimization routine JOPT_POS, check for errors on return and reset
c----> parameters
c
         CALL JOPT_POS(verbose)
         full_auto_lp=JFALSE
         if(signal.ne.' ')call error(0,LOC)
         signal=' '
c
c Place future call for lpiw output subroutine here:
c
         return
c
c------------------------------------------------------------------
c---> Following segment is a 'switch' for LPEx.  Call is to JFNDERR2
c
c
      elseif(jinp3.eq.'E')then
c
c---> set initial default values
c
 19      jadjflg=JFALSE
         jnmflg =JFALSE
         jflag1=JTRUE
         jjscale=JFALSE
cxxx
c
c---->  Request user input before JFNDERR2 call
c---->  values shown as equal to variable below are the default on <cr>
c
c---->  jflag1=T  write fine maps
c---->  jflag2=T  calculate Gmult and Gbias once at map center
c---->  jkpoints=21  number of pixels per side, 5 <= jkpoints <= 40
c---->  jjscale=F autoscaling of map linear size
c---->    if jjscale is true then pss_rad_map is set by user
c---->  pss_rad_map linear size, in degrees, per map side
c---->  full_auto_lp=T user is NOT prompted for error analysis decisions
cxxx
c---> Request input for jflag1, default= true
c
         write(6,'(" Write fine map to file(= GRO J2000 sourcename)?",
     &        " (cr for yes)"$)')
         read(LU(12),'(A)')jtmpchr
         if(jtmpchr.ne.' '.and.jtmpchr.ne.' '.and.jtmpchr.ne.'y'.and.
     &        jtmpchr.ne.'Y')jflag1=JFALSE
         jtmpchr=' '
cxxx  
c----> Request input for jflag2, default = true
c
         write(6,'(" Calculate parameters once at map center?",
     &        " (cr for yes) "$)')
         read(LU(12),'(A)')jtmpchr
         jflag2=JTRUE
         if(jtmpchr.ne.' '.and.jtmpchr.ne.' '.and.jtmpchr.ne.'y'.and.
     &        jtmpchr.ne.'Y')jflag2=JFALSE
cxxx
c----> Request input for number of pixels, jkpoints, default= 21
c
 20      jtmpchr=' '
         jkpoints = 21
         write(6,'(" Number of pixels per side for analysis",
     &        " grid (cr = 21, A = abort): "$)')
         read(LU(12),'(A)')jtmpchr
cxxx
c----> value 'A' or 'a' aborts request and returns to main(like)
c----> error check is performed on input value
c
         if(jtmpchr(1:1).eq.'A'.or.jtmpchr(1:1).eq.'a')then
            jflag1=JFALSE
            jflag2=JFALSE
            jtmpchr=' '
            return
         elseif(jtmpchr.eq.' '.or.jtmpchr.eq.' '.or.jtmpchr.eq.'y'.or.
     &           jtmpchr.eq.'Y')then
            jkpoints = 21
         else
            read(jtmpchr,*,err = 21)jkpoints
            goto 22
 21         print *,' input error ! Please try again.'
            print *,' '
            goto 20
         endif
 22      if(jkpoints.lt.10.or.jkpoints.gt.40)then
            if(jkpoints.lt.3.or.jkpoints.gt.50)then
               print *,'The value you selected in not reasonable !!'
               print *,' '
               jtmpchr(1:1)='z'
            endif
            print *,'The number of pixels should be in the range '
            print *,'  10 <= N <= 40 (50 is absolute maximum)'
            write(6,'("You selected: ",i2)')jkpoints
            if(jtmpchr.eq.'z')then
               print *,' Please input a more reasonable value '
               print *,' '
               goto 20
            endif
            write(6,'(" Are you sure you want this value ?",
     &           " (cr for Yes, N for NO): "$)')
            read(LU(12),'(A)')jtmpchr
            if(jtmpchr.eq.' '.or.jtmpchr.eq.' ')jtmpchr = 'Y'
            if(jtmpchr(1:1).ne.'Y'.and.jtmpchr(1:1).ne.'y')goto 20
         endif
cxxx
c----> Request scale size, or autoscaling (jjscale=F), default:autoscaling
c----> Entering a scale in degrees (0.1 < S < 12) sets jjscale=T,no autoscaling
c
 32      write(*,'("Enter fine map edge size in degrees ")')
         write(*,'(
     &        "(cr for autoscaling, A to abort or value:",/,
     &        " 0.1 < x <= 6 degrees): "$)')
         read(LU(12),'(A)')jtmpchr
         if(jtmpchr.eq.'a'.or.jtmpchr.eq.'A')then
            return
         elseif(jtmpchr.eq.' '.or.jtmpchr.eq.' '.or.jtmpchr.eq.'y'.or.
     &           jtmpchr.eq.'Y')then
            jjscale=JFALSE
            goto 38
         else
            read(jtmpchr,*,err=33)pss_rad_map
            if(pss_rad_map.lt.0.1.or.pss_rad_map.gt.6)then
               print *,' '
               print *,' This value is out of the range given above !'
               if(pss_rad_map.lt.0.1.or.pss_rad_map.gt.6)then
                  print *,' '
                  goto 32
               endif
               write(*,'(
     &              "Do you really want this value ? (Y/n) ",
     *              "(A to abort): "$)')
               read(LU(12),'(A)')jtmpchr
               if(jtmpchr.eq.'a'.or.jtmpchr.eq.'A')return
               if(jtmpchr.ne.' '.and.jtmpchr.ne.' '.and.
     &              jtmpchr(1:1).ne.'y'.and.jtmpchr(1:1).ne.'Y')goto 32
            endif
         endif
         jjscale=JTRUE
         pss_rad_map=pss_rad_map/2.
         goto 38
 33      print *,' READ ERROR: please input value again.'
         print *,' '
         goto 32
 38      continue
cxxx
c----> Request for position averages weighted by TS, default=F
c
         wgt_flg=JFALSE
         write(*,'(" Position averages weighted by TS ?")')
         write(*,'(" enter Y(y) for Yes, default=F: "$)')
         jtmpchr=' '
         read(LU(12),'(A)')jtmpchr
         if(jtmpchr.eq.'Y'.or.jtmpchr.eq.'y')wgt_flg=JTRUE
         
cxxx
c----> Request for Auto-decisions without user intervention, default=T
c
         jtmpchr=' '
         full_auto_lp=JTRUE
         write(*,'(
     &        " Full auto or user input decisions ? ",/,
     &        " (N(n) for user input, cr for auto): "$)')
         read(LU(12),'(A)')jtmpchr
         if(jtmpchr.eq.'N'.or.jtmpchr.eq.'n')full_auto_lp=JFALSE
c     
c
cxxx
c---->  Output user choices to the screen and permit user to change them
c---->  if desired.  The error analysis takes a long time and this prevents
c---->  wasting time on an incorrect setting.
c
 39      print *,' '
         print *,'****************************************************'
         print *,' '
         print *,' You Have Chosen The Following LPEA Run Parameters:'
         print *,' '
         print *,' WRITE FINE MAP FILES:              ',jflag1
         print *,' CALCULATE PARAMETERS ONCE:         ',jflag2
         print *,' Number of PIXELS per side:         ',jkpoints
         if(jjscale)then
            print *,' Autoscalling is OFF: scale size: ',
     &           (2*pss_rad_map)
         else
            print *,' Autoscalling has been requested  '
         endif
         print *,' Weight position calculation by TS: ',wgt_flg
         print *,' Full Auto-decisions requested:     ',full_auto_lp
         print *,' '
         write(*,'(" (C)ontinue,(A)bort LPEA, (X) change input: "$)')
         read(LU(12),'(A)')jtmpchr
         if(jtmpchr.eq.'c'.or.jtmpchr.eq.'C'.or.jtmpchr.eq.
     & ' '.or.jtmpchr.eq.' ')goto 40
         if(jtmpchr.eq.'x'.or.jtmpchr.eq.'X')goto 19
         if(jtmpchr.eq.'a'.or.jtmpchr.eq.'A')return
         print *,' '
         print *,' Choose from: C or A or X only ! '
         goto 39
cxxx
c----> Now call the error analysis subroutine
c
c-----> For jinp4='A' automatic run of all active PSF's
c-----> call JFNDERR2(.true.,jflag1,jflag2)
c----->   else call JFNDERR2(.false.,jflag1,jflag2)
c
 40      jkpoints=jkpoints-1
         if(jinp4.eq.'A')then
            print *,' ACTIVE PSFs: This will take a while.'
            print *,' '
            CALL JFNDERR2(JTRUE,jflag1,jflag2)
            call auto_out(3)
            if(signal.eq.'e')then
               print *,' '
cxxx
c---> Screen dump of results for jinp4='A'
c
               print *,' Some PSFs had large radii (> 90 arcmin) or ',
     *              'errors for',
     &              ' the confidence contours'
               print *,' The best possible error was calculated for ',
     *              'these PSFs'
               print *,' They require further investigation with ',
     *              'fine maps (MF)'
               signal=' '
            endif
            if(signal.eq.' ')then
               print *,' '
cxxx  
               write(*,'("  #  NAME                  Position",
     *              "68%    95%      Figure of merit    code")')
               print *,' '
               do j = 1,NSOURCE
                  if(SRC_PARMS(j,8).gt.0.5)then
                     sv_dstsv(2,j)=JTRUE
cxxx  
                     tmp_best_x=best_x(j)
                     if(tmp_best_x.lt.0)tmp_best_x=tmp_best_x+360
                     write(*,'(1x,i3,1x,A18,2f8.3,5x,f5.0,3x,f5.0,
     *                    2x,2f9.3,
     &                    4x,A)')j,SRC_NAMES(j),
     &                    tmp_best_x,best_y(j),sv_err68(j),sv_err95(j),
     &                    sv_schar_68(j),sv_schar_95(j),sv_sigsv(j)
                  endif
               enddo
               print *,' '
               print *,' '
            endif
         else
c
c----> Call for single analysis
c
            jpos(502)=NSOURCE+1
            print *,' SINGLE SOURCE: This will take a while.'
            CALL JFNDERR2(JFALSE,jflag1,jflag2)
c     
            if(signal.eq.'e')then
               print *,' '
cxxx  
c----> screen dump of results for single source
c
               print *,' The PSF had large radii (> 90 arcmin) or ',
     *              'errors for 68% conidence contour'
               print *,' The best possible error was calculated for ',
     *              'this PSF'
               print *,' The PSF requires further investigation with ',
     *              'fine maps  (MF)'
               signal=' '
            endif
            if(signal.eq.' ')then
               print *,' '
               write(*,'("  #  NAME                  ",
     *              "Position             ",
     &              "68%    95%     Figure of Merit    code")')
               print *,' '
               j=1
               jp=jpos(502)
               tmp_best_x=best_x(jp)
               if(tmp_best_x.lt.0)tmp_best_x=tmp_best_x+360
               write(*,'(1x,i3,1x,A18,2f8.3,5x,f5.0,3x,f5.0,
     &              3x,2f9.3)')j,srcN,
     &              tmp_best_x,best_y(jp),sv_err68(jp),sv_err95(jp),
     &              sv_schar_68(jp),sv_schar_95(jp)
               print *,' '
               print *,' '
            endif
         endif
c     
         if(signal.ne.' ')call error(0,LOC)
         signal=' '
c
         jjscale=JFALSE
         full_auto_lp=JFALSE
         return
c
c------------------------------------------------------------------
c---> Folloing segment is a 'switch' for command LPMx.  Call is JWRMAP
c
c
      elseif(jinp3.eq.'M'.and.jinp4.ne.'S')then
         jadjflg=JFALSE
         jnmflg=JFALSE
         jtmpchr=' '
         jjscale=JFALSE
c
c---> variable setting are the same as above for LPEx command
c---> except for the flag values given below.  See LPEx switch
c---> above for details on individual parameter values.
c--->
c---> jflag1=T write maps to files, allways true
c---> jflag2=T calculate Gmult and Gbias once at map center
c---> jflag3=T map all active PSF's (SRC_PARMS(8,N) > 0.5)
c
c---> Request user input on parameter(gmult,gbias) evaluation, default=T
c
         write(6,'(
     &        " Calculate parameters once at map center? ",
     *        "(cr for yes) "$)')
         read(LU(12),'(A)')jtmpchr
         jflag2=JTRUE
         if(jtmpchr.ne.' '.and.jtmpchr.ne.' ')jflag2=JFALSE
         jtmpchr=' '
         jflag1=JTRUE
         jflag3=JFALSE
c     
 210     continue
 220     jtmpchr=' '
c
c---> request user input on plot size in pixels, default=21
c
         jkpoints = 21
         write(6,'(" Number of pixels per side for analysis",
     &        " grid (cr = 21, A = abort): "$)')
         read(LU(12),'(A)')jtmpchr
         if(jtmpchr(1:1).eq.'A'.or.jtmpchr(1:1).eq.'a')then
            jflag1=JFALSE
            jflag2=JFALSE
            jtmpchr=' '
            return
         elseif(jtmpchr.eq.' '.or.jtmpchr.eq.' ')then
            jkpoints = 21
         else
            read(jtmpchr,*,err = 210)jkpoints
            goto 222
         endif
c
 222     if(jkpoints.lt.10.or.jkpoints.gt.40)then
            if(jkpoints.lt.5.or.jkpoints.gt.50)then
               print *,'The value you selected in not reasonable !!'
               print *,' '
               jtmpchr(1:1)='z'
            endif
            print *,'The number of pixels should be in the range '
            print *,'  10 <= N <= 40 (N < 5 or N > 40 should use MS)'
            write(6,'("You selected: ",i2,/)')jkpoints
            if(jtmpchr.eq.'z')then
               print *,' Please input a more reasonable value '
               print *,' '
               goto 220
            endif
            write(6,'(" Are you sure you want this value ?",
     &           " (cr for Yes, N for NO): "$)')
            read(LU(12),'(A)')jtmpchr
            if(jtmpchr.eq.' '.or.jtmpchr.eq.' ')jtmpchr = 'Y'
            if(jtmpchr(1:1).ne.'Y'.and.jtmpchr(1:1).ne.'y')goto 220
         endif
c
c
c----> Request user input of plot scale or autoscaling,default=auto
c
 320     write(*,'("Enter fine map edge size in degrees ")')
         write(*,'("(cr for autoscaling, A to abort or value:",
     &        " 0.1 < x <= 6 degrees): "$)')
         read(LU(12),'(A)')jtmpchr
         if(jtmpchr.eq.'a'.or.jtmpchr.eq.'A')then
            return
         elseif(jtmpchr.eq.' '.or.jtmpchr.eq.' ')then
            jjscale=JFALSE
            goto 380
         else
            read(jtmpchr,*,err=330)pss_rad_map
            if(pss_rad_map.lt.0.1.or.pss_rad_map.gt.6)then
               print *,' '
               print *,' This value is out of the range given above !'
               if(pss_rad_map.lt.0.1.or.pss_rad_map.gt.6)then
                  print *,' '
                  goto 320
               endif
               write(*,'("Do you really want this value ? (Y/n)",
     &              " (A to abort): "$)')
               read(LU(12),'(A)')jtmpchr
               if(jtmpchr.eq.'a'.or.jtmpchr.eq.'A')return
               if(jtmpchr.ne.' '.and.jtmpchr.ne.' '.and.
     &              jtmpchr(1:1).ne.'y'.and.jtmpchr(1:1).ne.'Y')goto 320
            endif
         endif
         jjscale=JTRUE
         goto 380
 330     print *,'READ ERROR: please input value again.'
         print *,' '
         goto 320
 380     continue
c
         jkpoints=jkpoints-1
         if(jinp4.eq.'A')then
            jflag3=JTRUE
         else
            jflag3=JFALSE
            jpos(502)=NSOURCE+1
         endif
         pss_rad_map=pss_rad_map/2
         print *,' This will take a while to calculate '
         print *,' '
         CALL JWRMAP(jflag1,jflag2,jflag3)
         write(*,'("  #  NAME                  Position    code")')
         print *,' '

         if(jflag3)then
            do j = 1,NSOURCE
               write(*,'(1x,i3,1x,A18,2f8.3,5x,A1)')j,SRC_NAMES(j),
     &              SRC_PARMS(j,1),SRC_PARMS(j,2),sv_sigsv(j)
            enddo
         else
            write(*,'(" 1  ",1x,A18,2f8.3,5x,A1)')srcN,
     &           srcL,srcB,sv_sigsv(1)
         endif

         print *,' '
         print *,' '
         if(signal.ne.' ')call error(-1,LOC)
         return
c
c-------------------------------------------------------------------
c---> Following segment is a 'switch' for LPR command.  Call is JREPORT
c---> Prints a report to files Table.3(TS>=25),Table.4(TS<25) and PSF.rpt
c---> The file PSF.rpt is used to diagnose failure or errors in the report
c---> tables.  Table.5 is also created and is output with system values for
c---> each likelihood run at the time of the LPR command.
c
      elseif(jinp3.eq.'R')then
         jflag1 = verbose
         jflag2 = JFALSE
         jflag3=JFALSE
         jflag4=JFALSE
         if(jinp4.eq.'A')jflag3=JTRUE
         CALL JREPORT(jflag1,jflag2,jflag3,jflag4)
         return
c     
c-------------------------------------------------------------------
c---> Following segment is a 'switch' for LPI[D x] command.  Call is
c---> INPUT_ID_POS
c
c
      elseif(jinp3.eq.'I'.or.
     &        input(3:4).eq.'MS')then
         jnmflg=JTRUE
         jtmpchr=input
         if(jae_input_id_pos)then
            write(*,*)'Call to input_id_pos'
            write(*,'("input=",a)') input(1:10)
         endif
         if(jinp3.ne.'I')full_auto_lp=JTRUE
         CALL INPUT_ID_POS(input)
C
C---> sort PSF's by GRO J2000 name for auto mapping survey (MS or LPID#M)
C---> Note use of '#' symbol to cause return from subroutine
c
         if(jtmpchr(1:4).eq.'LPMS'.or.jtmpchr(1:6).eq.'LPID#M')then
            input='ST        '
            CALL JSORT(input(1:10))
         endif
         full_auto_lp=JFALSE
         return
c
c-------------------------------------------------------------------
c---> Following segment is the DEFAULT 'switch' for unimplemented
c---> commands.  New commands should be placed above this segment
c
c     
      else
         write(*,'(/,"LPx Command ",a3,
     &        " NOT IMPLEMENTED",/)')input(1:3)
         return
      endif
      end
c
