c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C        SUBROUTINE JOPT_POS(jflag1)
C
C
C  $Id: jopt_pos.f,v 1.3 2013/05/21 19:08:26 irby Exp $
c==============================================================================
c  Effect:  Optimizes the position of one or more active PSF's in the
c           'Other Map'
c
C==============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+             UPDATED:     by  JAE
C=============================================================================
C* Subroutine Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^^^
C* jflag1	logical: When true enhances the screen output for diagnostics
C*============================================================================
C* Returned Values
C* ^^^^^^^^^^^^^^^
C*   none
c*============================================================================
C%   Changes:
c
c	22/04/94  Programmer: Joseph A. Esposito (jae@egret.gsfc.nasa.gov)
c
c		Bug in LPI sub M allowed an infinite loop condition due
c		to setting counts to 0 if TS < 0.1 at new location.  This
c		is fixed below and in jloc_pos_adj.f by watching for the
c		signature of the infinite loop and repairing the problem.
c		If the looping continues for 2 iterations after repairs are 
c		made then the user is notified and can abort or continue at
c		the users discression ( at command LPI sub M level in
c		jloc_pos_adj.f)
c			JAE
c
C  $Log: jopt_pos.f,v $
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
C  Revision 1.1  2002/04/16 20:27:34  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.13  1996/10/31  16:19:55  jae
c Calls to input_id_pos('LPID#Sx') changed
c to direct calls to JSORT('Sx') in order to
c avoid recursive calling.
c
c Revision 5.12  1996/08/13  15:40:01  jae
c Minor cleanup of commented out lines
c
c Revision 5.11  1996/07/26  15:34:27  jae
c Moved call to auto_out out of do 3 loop to
c below 4 continue.  This causes an automatic
c output after each iteration and NOT after each
c source.  Also added a final auto_out call
c after 1201 continue without regard to the
c autooutput flag.  This causes an automatic output to
c be done after every use of LPO[N]
c
c Revision 5.10  1996/07/23  15:32:57  jae
c Added call to auto_out(2) to permit
c autooutput of PSF list to LPIW format file
c if enabled.
c
c Revision 5.9  1996/06/27  20:17:06  jae
c Added lines for saving the SRC_PARMS
c parameter values (1-4, and 8) for later
c use by the SRC_PARMS reset command if
c called.
c
c Revision 5.8  1996/05/17  17:45:38  jae
c Repaired goto 22 statement in lines after first SRCTST.
c New statement "if(SRC_PARMS(jj,8).gt.0.8)goto 22.
c This stops routine from exiting before doing the
c AMOEBA search.
c
c Revision 5.7  1996/05/17  16:09:07  jae
c Took out typo line.  This is a simple edit.
c the line conatins "123456789" (I was counting
c columns)
c
c Revision 5.6  1996/05/17  15:51:50  jae
c Added lines for activity range 0.7 < A < 0.755
c
c Revision 5.5  1996/05/17  15:32:40  jae
c Added lines for output when jae_jloc_pos==T
c Repaired error: program did not reset SRC_PARMS(n,(1or2))
c and SRC_NAMES(n) after returning from the AMOEBA search.
c The lines were added after "22 continue" if activity > 0.76
c An additional line setting SRC_NAMES(n) is added if the
c activity is in the range 0.74 < A < 0.755.
c
c Revision 5.4  1996/05/09  16:36:00  jae
c lowest alowwed activity is 0.75 or 0.78(<-1) if
c no error. On error activity is set to 0.465 or 0.48 for
c fixed and fully active sources respectively.
c
c Revision 5.3  1996/05/08  19:09:06  jae
c fixed minor typo's since last build
c
c Revision 5.2  1996/05/08  17:48:57  jae
c Unpdated code to go 5 iterations before removing
c sources from the loop.  Also, tightened act>0.8 srcs
c to act=0.78 on error and 0.48 on removal so that original
c activity is re-established for simple, non-important errors
c and small TS values.  0.78 and 0.48 -> 1 on exit
c 0.465 -> 0.75 on exit
c 0.13 -> error on fixed source (high level error)
c 0.15 -> error on full activity source (high level error)
c
c Revision 5.1  1996/02/29  20:48:26  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:27  jae
c Subroutine Module for like V5.00
c
c
c==========================================================================
      SUBROUTINE JOPT_POS(jflag1)

C     Common blocks used:
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

      character(80) id
      common /id/id
c
      logical jflag1,jflag2,determined,idoneit,jloss,jnerr1
      character text*150,jtmpchr*10,text2*50
c     POINTER (psvo_TS,svo_TS)
      real*4 svo_TS(500),new_ts_max
c     psvo_TS=malloc(4*NSOURCE)
c
      id = '$Id: jopt_pos.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
      LOC='JOPT_POS'
      if(jae_jopt_pos)write(*,*)'in Subroutine JOPT_POS'
c
c---> initialize values
c
      jloss_cnt=0
      jcnt1=0
      new_ts_max=-1.
      determined=.false.
      jflag2=JFALSE
      idoneit=JFALSE
      JNSOURCE_sv=NSOURCE
c
      if (actj_flg.lt.0.005) actj_flg = 0.01

      do jj = 1,500
         if (SRC_PARMS(jj,8).gt.0.5) then
            svo_TS(jj)=svn_TS(jj)
            svo_infx(jj) = -1
            svo_infy(jj) = -1
c     svn_TS(jj) = 0.0
            sv_sigsv(jj)=' '
            svo_infx(jj) = SRC_PARMS(jj,1)
            svo_infy(jj) = SRC_PARMS(jj,2)
         endif

         SRC_PARMS(jj,7)=SRC_PARMS(jj,8)
         SRC_PARMS(jj,9)=SRC_PARMS(jj,1)
         SRC_PARMS(jj,10)=SRC_PARMS(jj,2)
         SRC_PARMS(jj,11)=SRC_PARMS(jj,3)
         SRC_PARMS(jj,12)=SRC_PARMS(jj,4)
      enddo

      ncnt = 0
      ncnt2=0
      jncnt = 0
      jloss = .false.
c
c---> Return if no PSF sources 
c
      if (NSOURCE.lt.1) goto 897
      print *,' '
      print *,' '
c
c---> count activity types, 0.75 position fixed, 1.0 position optimized
c---> the activity is set to fractional value by the LPI subcommand 'A' 
c
      do jj = 1, NSOURCE
         if (SRC_PARMS(jj,8).gt.0.8) ncnt = ncnt + 1
         if (SRC_PARMS(jj,8).gt.0.7.and.SRC_PARMS(jj,8).lt.
     &        0.8) ncnt2=ncnt2+1
      enddo

      if (mxxiter.lt.1) mxxiter=5
 1    continue
      print *,' '
      print *,' '
c
c---> No active PSF sources then return
c
      if (ncnt.eq.0.and.ncnt2.eq.0) goto 898
c
      text ='  #   NAME                  POSITION        Angular s'
      text=text(1:53)//'hift      TS        Optimum shift   activity'
c
c---> screen output each loop to check optimization progress
c
 2    write(6,'(
     &     "The current other PSF map contains:")')
c
      if (jncnt.eq.0) then
         write(6,'("  #   NAME                  POSITION    ",
     &        "   CNTS   Sp. I.   Active")')
         do jsrc=1,NSOURCE
            if(.not.sv_dstsv(2,jsrc)) then
               sv_err95(jsrc)=0
               sv_err68(jsrc)=0
               best_x(jsrc)=SRC_PARMS(jsrc,1)
               best_y(jsrc)=SRC_PARMS(jsrc,2)
               best_choice(jsrc)='TS'
            endif

            theLong=SRC_PARMS(jsrc,1)

            if (theLong.lt.0.) theLong=theLong+360.

            write(6,'(
     &           i3,3x,A18,2f7.2,f10.1,f6.2,f9.2)')
     &           jsrc,SRC_NAMES(jsrc),theLong,
     &           (SRC_PARMS(jsrc,ip),ip=2,4),SRC_PARMS(jsrc,8)
         enddo
         write(6,'(" There are ",i3," fully active sources")')ncnt
         write(6,'(" There are ",i3," position fixed sources")')ncnt2
      else
         aspsum = 0
         write(6,*)text
         do jsrc=1,NSOURCE
            theLong=SRC_PARMS(jsrc,1)
            if (theLong.lt.0.) theLong=theLong+360.
            if (SRC_PARMS(jsrc,8).gt.0.7.or.(SRC_PARMS(jsrc,8).gt.
     &           0.05.and.idoneit)) then
               jtmpchr='          '

               if (SRC_PARMS(jsrc,8).lt.0.69)j tmpchr='<--'
               write(6,
     &              '(I3,3x,A18,2f8.3,1x,f11.4,f14.2,6x,f11.4,
     *              7x,f4.2,1x,A1)')
     &              jsrc,SRC_NAMES(jsrc),theLong,
     &              SRC_PARMS(jsrc,2),svo_inf(jsrc),
     &              svn_TS(jsrc),svon_zen(jsrc),SRC_PARMS(jsrc,8),
     *              sv_sigsv(jsrc)
               aspsum = aspsum + svo_inf(jsrc)

               if (svo_inf(jsrc).lt.actj_flg.and..not.idoneit.and.
     &              SRC_PARMS(jsrc,8).gt.0.8.and.jncnt.gt.5) then
                  SRC_PARMS(jsrc,8)=0.78
                  ncnt = ncnt - 1
               endif
            endif
         enddo
cxxx
         if (.not.idoneit.and.jncnt.gt.0)
     &        write(*,'(/," Sum of angular shifts: ",
     &        f7.3," Sources left - Positional: ",I3," Fixed: ",I3,
     &        "  Newest maximum dTS: ",f10.3,
     &        " Number set inactive due to errors:",I4)')
     &        aspsum,ncnt,ncnt2,new_ts_max,jloss_cnt
      endif

      write(6,*)
      jncnt = jncnt + 1
      if (jcnt1.ge.1.and.new_ts_max.lt.0.01) then
         if (jae_jopt_pos) then
            write(*,*)'goto 1200 on jcnt1 and new_ts_max'
         endif
         goto 1200
      endif

      jtst = mod(jncnt-1,mxxiter)
      if (ncnt.le.0.and.jncnt.gt.5) then
	 if (ncnt2.eq.0.or.new_ts_max.lt.0.01) then
            if (jae_jopt_pos) then
               write(*,*)'goto 1200 on ncnt2=ncnt=0 and new_ts_max'
            endif
            goto 1200
	 endif
      endif

      if (jtst.eq.0.and.jncnt.gt.1) then
         if (full_auto_lp) then
            idoneit=.true.
            if (jae_jopt_pos) then
               write(*,*)'goto 1200 on jtst=0 and jncnt>1'
            endif
            goto 1200
         endif
         
         CALL INTENT(determined)
         if (.not.determined) then
            idoneit=.true.
            goto 1200
         endif

         print *,' Increase convergence tolerance from ',actj_flg
         print *,' <cr> to increase present value'
         print *,' '
         determined = .false.
         read(LU(12),'(a)') jtmpchr
         numcar = index(jtmpchr, ' ')

         if (numcar.eq.0) then
            write(*,'(" Enter new tolerance: ",$)')
            read(LU(12),*)actj_flg
         endif
      endif
ccc   
c---> The main optimization loop
c
      kk = 0
      text2='ST        '
      CALL JSORT(text2(1:10))
      new_ts_max=-0.5
      do jj = 1,NSOURCE
         sv_sigsv(jj)=' '
         jpos(502)=jj
	 if(SRC_PARMS(jj,8).lt.0.5)goto 315
         if(SRC_PARMS(jj,8).gt.0.7)then
            print *,' '
            print *,' iteration: ',jncnt
            print *,' Active source:',kk+1,
     &           ' of ',ncnt+ncnt2,' Activity: ',SRC_PARMS(jj,8)
            print *,' PSF #: ',jj,' NAME: ',SRC_NAMES(jj)
c
c
c
            kk = kk + 1
            if(ncnt.le.1)jcnt1=jcnt1+1
            if(sv_dstsv(1,jj))then
               TSjsv =svn_TS(jj)
            else
               TSjsv = 0
            endif
            counts=0.0
            cnts_sv = SRC_PARMS(jj,3)
            srcN = SRC_NAMES(jj)
            srcL = SRC_PARMS(jj,1)
            srcB = SRC_PARMS(jj,2)
c     print *,' PSFREPLACE is next'
            CALL PSFREPLACE(jj)
c     print *,' Back frfom PSFREPLACE'
            TS=0.
            report=JTRUE
            report2=JTRUE
c     
c---  >get initial TS value
c
            signal=' '
            jnerr1=JFALSE
            if(jae_jopt_pos)print *,' Called SRCTEST for ',jj
            CALL SRCTEST(JFALSE)
            if(jae_jopt_pos)print *,'SRCTEST return for ',
     &           jj,' SIGNAL: ',signal
            if(signal.ne.' '.and.signal.ne.' ')jnerr1=JTRUE
            tststj=TS
            if(jnmflg.and.SRC_PARMS(jj,8).lt.0.755)then
               if(jnerr1.and.signal.ne.'Z'.and.signal.ne.'z'
     &              .and.signal.ne.'M'.and.signal.ne.'S'.and.
     *              signal.ne.'R')then
                  continue
               else
                  CALL GETSRCNM()
                  if(signal.ne.'S')SRC_NAMES(jj) = srcN
               endif
            endif
            if(SRC_PARMS(jj,8).gt.0.7)then
               if(jnerr1.and.signal.ne.'Z'.and.signal.ne.'z'
     &              .and.signal.ne.'M'.and.signal.ne.'S'.and.
     *              signal.ne.'R')then
                  sv_sigsv(jj)=signal
                  if(SRC_PARMS(jj,8).lt.0.77)SRC_PARMS(jj,8)=0.465
                  if(SRC_PARMS(jj,8).gt.0.776)SRC_PARMS(jj,8)=0.48
                  svn_TS(jj) = 0
                  SRC_PARMS(jj,3)=0.
                  TS=0
                  jloss=JTRUE
                  CALL ERROR(0,LOC)
                  signal=' '
                  goto 3
               endif	
               if(TS.ge.1)then
                  tst_dif_ts=abs(TS-svn_TS(jj))/TS
               elseif(TS.lt.1.and.TS.ge.0.25)then
                  tst_dif_ts=-0.5
               else
                  tst_dif_ts=-1
               endif
               if(((tst_dif_ts.lt.1.e-03.and.
     &              tst_dif_ts.ge.0).or.TS.lt.0.1).and.jncnt.gt.5)then
                  if(SRC_PARMS(jj,8).gt.0.77)SRC_PARMS(jj,8)=0.78
               endif
               if(tst_dif_ts.gt.new_ts_max)new_ts_max=tst_dif_ts
               if(SRC_PARMS(jj,8).lt.0.8)goto 22
            endif
            cntstj=counts
c     
c---> set PSF source inactive or fixed activity on error
c
            report=JFALSE
            report2=JFALSE
            if(jae_jopt_pos)then
               write(*,*)'****before 21 and before goto 22'
               print *,' Active source:',kk,
     &              ' of ',ncnt+ncnt2,' Activity: ',SRC_PARMS(jj,8)
               print *,' PSF #: ',jj,' NAME: ',SRC_NAMES(jj),
     &              '  signal:',SIGNAL
               write(*,*)' '
            endif
            if(SRC_PARMS(jj,8).lt.0.8)goto 22
 21         continue
            if(jae_jopt_pos)then
               write(*,*)'****after 21 and before signal check'
               print *,' Active source:',kk,
     &              ' of ',ncnt+ncnt2,' Activity: ',SRC_PARMS(jj,8)
               print *,' PSF #: ',jj,' NAME: ',SRC_NAMES(jj),
     &              '  signal:',SIGNAL
               write(*,*)' '
            endif

            if(signal.ne.' '.and.signal.ne.' '.and.signal.ne.
     &           'S'.and.signal.ne.'M')then
               if(signal.ne.'R'.and.signal.ne.'z'.and.
     &              signal.ne.'Z')then
                  if(SRC_PARMS(jj,8).gt.0.8)
     &                 SRC_PARMS(jj,8)=0.15
                  if(SRC_PARMS(jj,8).gt.0.74)
     &                 SRC_PARMS(jj,8)=0.13
                  jnerr1=JTRUE
                  jloss=JTRUE
               else
                  if(SRC_PARMS(jj,8).gt.0.8)
     &                 SRC_PARMS(jj,8)=0.78
               endif
               Counts=cnts_sv
               sv_dstsv(1,jj)=JFALSE
               SRC_PARMS(jj,5)=-2
               srcL=SRC_PARMS(jj,1)
               srcB=SRC_PARMS(jj,2)
               sv_sigsv(jj)=signal
               CALL ERROR(0,LOC)
               SIGMSG=' '
               if(.not.jnerr1)then
                  CALL PIXEL_SELECT(JFALSE,jblank)
                  CALL SRCTEST(JFALSE)
                  goto 22
               endif
               svn_TS(jj)=TSjsv
               CALL PSFREPLACE(jj)
               call PSFBLD
               print *,'OPT-> PSF: ',jj,
     &              ' SET INACTIVE DUE TO SRCTEST ERROR: ',signal
               jloss = JTRUE
               goto 3
            endif
            if(TS.lt.0.0005.and.SRC_PARMS(jj,8).gt.0.8)then
c     
               SRC_PARMS(jj,8)=0.78
               print *,'OPT-> PSF: ',jj,
     &              ' SET FIXED activity - TS: ',TS,' < 0.005'
               goto 22
            endif
            if(SRC_PARMS(jj,8).lt.0.8)goto 22
            if(SRC_PARMS(jj,8).gt.0.8)then
               if(jae_jopt_pos)print *,' Called JFNDPOS for ',jj
               CALL JFNDPOS(jflag1,jflag2)
c     print *,' JFNDPOS return for ',jj,' SIGNAL: ',signal
c
               if(jae_jopt_pos)then
                  write(*,*)'****after 21 and before signal check'
                  print *,' Active source:',kk,
     &                 ' of ',ncnt+ncnt2,' Activity: ',SRC_PARMS(jj,8)
                  print *,' PSF #: ',jj,' NAME: ',SRC_NAMES(jj),
     &                 '  signal:',SIGNAL
                  write(*,*)' '
               endif
               if(signal.eq.'Z'.or.signal.eq.'z'.or.signal.eq.
     &              'R')then
                  SRC_PARMS(jj,8)=0.78
                  goto 22
               endif
               if(signal.ne.' ' .and. signal.ne.'S'.and.
     &              signal.ne.'M')then
                  CALL ERROR(0,LOC)
                  SIGNAL='T'
                  SIGMSG='ERROR: JFNDPOS routine failed'
                  sv_sigsv(jj)=signal
                  if(jae_jopt_pos)then
                     write(*,*)'****after 21 and before signal check'
                     print *,' Active source:',kk,
     &                    ' of ',ncnt+ncnt2,' Activity: ',
     *                    SRC_PARMS(jj,8)
                     print *,' PSF #: ',jj,' NAME: ',SRC_NAMES(jj),
     &                    '  signal:',SIGNAL
                     write(*,*)' '
                  endif
                  goto 21
               endif
            endif
c
c---> save returned values to arrays
c
 22         continue
c     print *,' past label 22 for ',jj
            if(jloss)jloss_cnt = jloss_cnt+1
            if(SRC_PARMS(jj,8).gt.0.7)then
               SRC_PARMS(jj,5)=2
               sv_dstsv(1,jj)=JTRUE
               if(TS.gt.1)then
                  tst_dif_ts=abs(TS-svn_TS(jj))/TS
               else
                  tst_dif_ts=0
               endif
               if(tst_dif_ts.gt.new_ts_max)new_ts_max=tst_dif_ts
            endif
            write(*,'("DIF; ",f7.1," Max: ",f7.1)') 
     &           tst_dif_ts,new_ts_max   
            if(SRC_PARMS(jj,8).lt.0.492.and.SRC_PARMS(jj,8).gt.
     &           0.39)print *,'Source has been removed from ',
     *           'optimization'
            write(*,*)' '
            write(*,*)' '
            svo_inf(jj) = gtcirc(SRC_PARMS(jj,1),SRC_PARMS(jj,2),
     &           srcL,srcB)
            if(SRC_PARMS(jj,8).gt.0.76)then
               SRC_NAMES(jj) =  srcN
               SRC_PARMS(jj,1) = srcL
               SRC_PARMS(jj,2) = srcB
            endif
            if(jae_jopt_pos)then
               write(*,*)'****after 22'
               print *,' Active source:',kk,
     &              ' of ',ncnt+ncnt2,' Activity: ',SRC_PARMS(jj,8)
               print *,' PSF #: ',jj,' NAME: ',SRC_NAMES(jj),
     &              '  signal:',SIGNAL
               write(*,'("srcL:",f7.1," srcB:",f7.1)') 
     &              SRC_PARMS(jj,1), SRC_PARMS(jj,2)
               write(*,*)' '
            endif
            sv_flx(1,jj)=1.e8*Counts/pss_expose
            sv_flx(2,jj)=1.e8*dCounts/pss_expose
            sv_cnts(1,jj)=Counts
            sv_cnts(2,jj)=dCounts
            sv_expose(jj)=pss_expose
            svn_TS(jj) = TS
            sv_params(1,jj)=Gmult
            sv_params(2,jj)=Gbias
            sv_upperlim(jj)=1.e8*Counts_limit/pss_expose
            if(TS.ge.25.)then
               sv_dstsv(3,jj)=JTRUE
            else
               sv_dstsv(3,jj)=JFALSE
            endif
            sv_sigsv(jj) = signal
            svon_zen(jj) = gtcirc(svo_infx(jj),svo_infy(jj),
     &           SRC_PARMS(jj,1),SRC_PARMS(jj,2))
            sigsv = ' '
            signal=' '
            if(jnmflg)SRC_NAMES(jj)=srcN
            CALL PSFREPLACE(jj)
 3          continue
         endif
 315  enddo
 4    continue
      ncnt=0
      ncnt2=0
      do jj=1,NSOURCE
         tst_jj_8=SRC_PARMS(jj,8)
         if(tst_jj_8.gt.0.8)ncnt=ncnt+1
         if(tst_jj_8.gt.0.7.and.tst_jj_8.lt.0.8)ncnt2=ncnt2+1
      enddo
      jloss = JFALSE
      if(autooutput)call auto_out(2)
      goto 2
c
c
c
c
c
c
 897  print *,' '
      print *,' There are NO PSFs in the OTHER MAP '
      print *,' returning to main menu'
      return
 898  print *,' '
      print *,' There are NO ACTIVE PSFs in the OTHER MAP '
      print *,' returning to main menu'
      return
 1200 continue
c
c---> branch to final screen dump if finished and raise flag so that next time
c---> through the process returns to the calling subroutine.  Also, reset
c---> initial activity if it wasn't deactivated due to error.
c
      if(idoneit)then
         idoneit=JFALSE
         if(jae_jopt_pos)then
            write(*,*)'set idoneit=F exit'
         endif
         do ii=1,NSOURCE
            if(SRC_PARMS(ii,8).gt.0.35.and.
     &           SRC_PARMS(ii,8).lt.0.45)SRC_PARMS(ii,8)=1
            if(SRC_PARMS(ii,8).gt.0.77.and.
     &           SRC_PARMS(ii,8).lt.0.79)SRC_PARMS(ii,8)=1
            if(SRC_PARMS(ii,8).gt.0.47.and.
     &           SRC_PARMS(ii,8).lt.0.49)SRC_PARMS(ii,8)=1
            if(SRC_PARMS(ii,8).gt.0.46.and.
     &           SRC_PARMS(ii,8).lt.0.467)SRC_PARMS(ii,8)=0.75
         enddo
         call auto_out(2)
         goto 1201
      else
         idoneit=JTRUE
         if(jae_jopt_pos)then
            write(*,*)'set idoneit=T pre-exit'
         endif
         do ii=1,NSOURCE
            if(SRC_PARMS(ii,8).gt.0.35.and.
     &           SRC_PARMS(ii,8).lt.0.45)SRC_PARMS(ii,8)=1
            if(SRC_PARMS(ii,8).gt.0.77.and.
     &           SRC_PARMS(ii,8).lt.0.79)SRC_PARMS(ii,8)=1
            if(SRC_PARMS(ii,8).gt.0.47.and.
     &           SRC_PARMS(ii,8).lt.0.49)SRC_PARMS(ii,8)=0.75
            if(SRC_PARMS(ii,8).gt.0.46.and.
     &           SRC_PARMS(ii,8).lt.0.467)SRC_PARMS(ii,8)=0.75
         enddo
         goto 2
      endif
 1201 continue
      new_ts_max=-1
      do jj=1,NSOURCE
	 if(SRC_PARMS(jj,8).gt.0.74)then
            sv_dstsv(1,jj)=JTRUE
            if(svn_TS(jj).gt.1)then
               tst_TS=abs(svo_TS(jj)-svn_TS(jj))/svn_TS(jj)
            else
               tst_TS=0
            endif
            if(sv_dstsv(2,jj).eqv.JTRUE.and.(svon_zen(jj).gt.
     &           0.1*sv_err95(jj).or.tst_TS.gt.0.02*svn_TS(jj)))then
               best_x(jj)=SRC_PARMS(jj,1)
               best_y(jj)=SRC_PARMS(jj,2)
               best_choice(jj)='TS'
               sv_err95(jj)=0
               sv_err68(jj)=0
               sv_dstsv(2,jj)=JFALSE
               goto 1202
            endif
            xxx = gtcirc(SRC_PARMS(jj,1),SRC_PARMS(jj,2),
     &           svo_infx(jj),svo_infy(jj))
            dif_TS=abs(svo_TS(jj)-svn_TS(jj))
            if(rattst.gt.new_ts_max)new_ts_max=dif_TS
            rattst=-1
            if(svn_TS(jj).ge.4)rattst=dif_TS/svn_TS(jj)
            if(rattst.gt.0.05.or.xxx.gt.0.1.or.(rattst.gt.0.02.and.
     &           svn_TS(jj).lt.62.5))then
               sv_dstsv(2,jj)=JFALSE
               best_x(jj)=SRC_PARMS(jj,1)
               best_y(jj)=SRC_PARMS(jj,2)
               best_choice(jj)='TS'
               if(rattst.gt.0.02.and.rattst.gt.new_ts_max)
     &              new_ts_max=rattst
               goto 1202
            endif
	 endif
 1202    continue
      enddo
      CALL PSFBLD
      text2='ST        '
      CALL JSORT(text2(1:10))
      return	
      end
c
