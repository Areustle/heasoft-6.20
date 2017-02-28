c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C       SUBROUTINE JMAPFINE(jflag1,jflag2,rad_map)
C
C
C  $Id: jmapfine.f,v 1.3 2013/05/21 19:08:25 irby Exp $
c
c       For LIKE v5.00
c       programmer: Joseph A. Esposito (jae@egret.gsfc.nasa.gov)
c       final form reached: 11-NOV-93
c--------------------------------------------------------------------
c
C++            effect: the likelihood operator is applied with
C++                    a shifted PSF to obtain a fine map of
C++                    likelihood versus position which is
C++                    written to LMAPFILE.
c
c
c              input variable:  jflag1: T output to file, F no file output
c                               jflag2 is passed to local logical
c				       counts_fixed.
c
c Change 12-10-95 JAE
c
c	Changed fixed Gm, Gb values for counts = 0 when optimum
c	Gm, Gb values are fixed.  Recalculate Source_not_lnL at
c	each x,y with Gm and Gb (counts)=0 and Gm, Gb ,counts for
c	proper renormalization.  This only affects calculations
c	when Gm and Gb are fixed at the optimum values for position
c	x, y.
c
c
C  $Log: jmapfine.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
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
c Revision 5.2  1996/02/29  21:07:47  jae
c no changes done
c
c Revision 5.1  1996/02/29  20:48:25  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:25  jae
c Subroutine Module for like V5.00
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE JMAPFINE(jflag1,jflag2,rad_map)

C     Common blocks used:
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
      logical jflag1,jflag2
      real rad_map,tmp_TS0
      REAL tmp_CTLORG(2)
      character input*60,bstch*2,tst_tmp_doc*70
      logical counts_fixed,FEXIST
      integer time
C
      id = '$Id: jmapfine.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      counts_fixed = jflag2
      LOC='JMAPFINE'

      if(jae_debug)print *,'Inside:',LOC

      srcL_sv=srcL
      srcB_sv=srcB
      if(rad_map.lt.0.0001)then
         signal='R'
         SIGMSG='ERROR:'//LOC//': Mapping side length is zero'
         if(jae_jmapfine)then
            CALL ERROR(0,LOC)
            print *,'JPOS(502):',jpos(502)
            print *,'jkpoints:',jkpoints
            print *,'srcN:',srcN
            print *,'srcL:',srcL,'  srcB:',srcB
            print *,'sv_dstsv(1...2,N):',sv_dstsv(1,jpos(502)),
     &           ' ',sv_dstsv(2,jpos(502))
         else
            CALL ERROR(0,LOC)
         endif
         signal='R'
         return
      endif
C
      if(jae_jmapfine)print *,'jkpoints:',jkpoints
      if(signal.ne.' ')signal=' '
      if(jflag1)then
         INQUIRE (FILE=LMAPFILE,EXIST=FEXIST)
         IF(FEXIST) THEN
            input='rm -f '//LMAPFILE
            CALL SYSTEM(input)
         ENDIF
      endif
      psf_rad=Ranal
      if(psf_rad.gt.20.)psf_rad=20.
      CALL PSMMAT(srcL-srcLpxcnt,srcB-srcBpxcnt)
      if (signal.ne.' ')then
         if(jae_jmapfine)then
            CALL ERROR(-1,LOC)
            print *,jkpoints
         else
            CALL ERROR(0,LOC)
         endif
         SIGMSG='ERROR:JMAPFINE:PSMMAT: MAP ABORTED'
         SIGNAL='P'
         return
      endif
      if(jjplot)goto 10
c
      if(counts_fixed) then
         if(EMAPFILE.eq.'unity') then
c     no exposure map
            source_notlnL=0.
         else
            calc_uncert=JFALSE
            report=JFALSE
            report2=JFALSE
            Counts=0.
            CALL GASBIAS(JFALSE)
            source_notlnL=-lnL
            Gmult0=Gmult
            Gbias0=Gbias
            if(sv_dstsv(1,jpos(502)))then
               Counts_previous=sv_cnts(1,jpos(502))
               Counts=Counts_previous
               Gmult=sv_params(1,jpos(502))
               Gbias=sv_params(2,jpos(502))
               Gmult1=Gmult
               Gbias1=Gbias
               tmp_TS0=svn_TS(jpos(502))
               if(jae_jmapfine)then
                  print *,'1a: TS max: ',tmp_TS0
                  print *,'Gmult:',Gmult,' Gbias:',Gbias
                  print *,'Counts:',Counts
                  print *,'srcL:',srcL,' srcB:',srcB
                  print *,'jpos(502):',jpos(502)
               endif
            else
               CALL SRCTEST(JFALSE)
               Counts_previous=Counts
               tmp_TS0=TS
               sv_params(1,jpos(502))=Gmult
               sv_params(2,jpos(502))=Gbias
               Gmult1=Gmult
               Gbias1=Gbias
               if(jae_jmapfine)then
                  print *,'1b: TS max: ',tmp_TS0
                  print *,'Gmult:',Gmult,' Gbias:',Gbias
                  print *,'Counts:',Counts
                  print *,'srcL:',srcL,' srcB:',srcB
               endif
               goto 10
            endif 
         endif
c
c     end counts_fixed if clause
      else
         Counts=0.
         CALL GASBIAS(JFALSE)
         source_notlnL=-lnL
         if(sv_dstsv(1,jpos(502)))then
            Counts_previous=sv_cnts(1,jpos(502))
            Gmult=sv_params(1,jpos(502))
            Gbias=sv_params(2,jpos(502))
            tmp_TS0=svn_TS(jpos(502))
            if(jae_jmapfine)then
               print *,'2a: TS max: ',tmp_TS0
               print *,'Gmult:',Gmult,' Gbias:',Gbias
               print *,'Counts:',Counts
               print *,'srcL:',srcL,' srcB:',srcB
            endif
         else
            CALL SRCTEST(JFALSE)
            Counts_previous=Counts
            tmp_TS0=TS
            sv_params(1,jpos(502))=Gmult
            sv_params(2,jpos(502))=Gbias
            if(jae_jmapfine)then
               print *,'2b: TS max: ',tmp_TS0
               print *,'Gmult:',Gmult,' Gbias:',Gbias
               print *,'Counts:',Counts
               print *,'srcL:',srcL,' srcB:',srcB
            endif
         endif
c
         report=JFALSE
         report2=report
         calc_uncert=JFALSE
      endif
c
 10   diameter = 2.*rad_map
      width=diameter/float(jkpoints)
      cornerL=best_x_tmp-diameter/2.
      cornerB=best_y_tmp-diameter/2.
      if(jjplot)goto 50
      delta_TS_min = 1000000.
      IMAPSV=0
      aspmaxj = -1
      if(jae_jmapfine)then
	 print *,' creating the map with jkpoints:',jkpoints
	 print *,' map side length: ',diameter
	 print *,' '
      endif
      ns=time()
C     Do likelihood ratio analysis of these points:
      DO  I = 1,jkpoints+1
         PL = cornerL + (I-1) * width
         DO  J = 1,jkpoints+1
            PB = cornerB + (J-1) * width
            if(coord_sys.eq.'C') then
               xpL=SC_RAJ
               xpB=SC_DECJ
            else
               xpL=SC_LJJ
               xpB=SC_BJJ
            endif
            srcL=PL
            srcB=pB
            CALL PIXEL_SELECT(JFALSE,jblank)
            LikTotaled = JFALSE
            CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
            asptmpj = gtcirc(xpL,xpB,PL,PB)
            if(asptmpj.gt.aspmaxj)then
               aspmaxj=asptmpj
               xsrcL = PL
               xsrcB = PB
            endif
            CALL MAPIXL(PL,PB,ILL,JBB)
            CALL MAPCOR(ILL,JBB,srcLpxcnt,srcBpxcnt)
            CALL PSMMAT(PL-srcLpxcnt,PB-srcBpxcnt)
            if (signal.ne.' ')then
               if(jae_jmapfine)then
                  CALL ERROR(-1,LOC)
                  print *,jkpoints
               else
                  CALL ERROR(0,LOC)
               endif
               SIGMSG='ERROR:JMAPFINE:PSMMAT: MAP ABORTED'
               SIGNAL='P'
               srcL=srcL_sv
               srcB=srcB_sv
               CALL PIXEL_SELECT(JFALSE,jblank)
               LikTotaled = JFALSE
               CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
               return
            endif
c
            IMAP=I+ (jkpoints+1)*(J-1)
c
c     print *,' IMAP: ',IMAP,' PL: ',PL,' PB: ',PB
            if(counts_fixed) then
               Counts=0
               Gmult=Gmult0
               Gbias=Gbias0
               CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
               source_notlnL=-lnL
               Counts=sv_cnts(1,jpos(502))
               Gmult=Gmult1
               Gbias=Gbias1
               CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
               if(signal.ne.' ')then
		  if(jae_jmapfine)then
                     CALL ERROR(-1,LOC)
                     print *,jkpoints
		  else
                     CALL ERROR(0,LOC)
		  endif
		  SIGMSG='ERROR:JMAPFINE:HOOD: MAP ABORTED'
		  SIGNAL='H'
		  srcL=srcL_sv
		  srcB=srcB_sv
	          CALL PIXEL_SELECT(JFALSE,jblank)
        	  LikTotaled = JFALSE
        	  CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
                  return
               endif
               source_lnL = -lnL
               TS = 2.*(source_lnL-source_notlnL)
               TMPMAP(IMAP)=TS
            else
               Counts_previous=Counts
               Counts=0.
               CALL GASBIAS(JFALSE)
               if(signal.ne.' ')then
                  if(jae_jmapfine)then
                     CALL ERROR(-1,LOC)
                     print *,jkpoints
                  else
                     CALL ERROR(0,LOC)
                  endif
		  SIGMSG='ERROR:JMAPFINE:GASBIAS: MAP ABORTED'
		  SIGNAL='G'
                  return
               endif
               source_notlnL=-lnL
               Counts=Counts_previous
               CALL ALLFIT(JFALSE)
               if(signal.ne.' ')then
                  if(jae_jmapfine)then
                     CALL ERROR(-1,LOC)
                     print *,jkpoints
                  else
                     CALL ERROR(0,LOC)
                  endif
		  SIGMSG='ERROR:JMAPFINE:ALLFIT: MAP ABORTED'
		  SIGNAL='A'
		  srcL=srcL_sv
		  srcB=srcB_sv
	          CALL PIXEL_SELECT(JFALSE,jblank)
        	  LikTotaled = JFALSE
        	  CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
                  return
               endif
               TS = 2*(-lnL - source_notlnL)
               TMPMAP(IMAP) = TS
            endif
            if(signal.ne.' ') then
               if(jae_jmapfine)then
                  CALL ERROR(-1,LOC)
                  print *,jkpoints
               else
                  CALL ERROR(0,LOC)
               endif
               SIGMSG='ERROR:JMAPFINE: MAP ABORTED'
               SIGNAL='J'
               srcL=srcL_sv
               srcB=srcB_sv
               CALL PIXEL_SELECT(JFALSE,jblank)
               LikTotaled = JFALSE
               CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
               return
            endif
            delta_TS = tmp_TS0 - TS
            if(delta_TS.lt.delta_TS_min)then
               if(jae_jmapfine)then
		  print *,' '
		  print *,'TS:',TS,' delta:',delta_TS,'->',Imap
		  if(delta_TS.lt.1.e-04)then
                     print *,'** dTS <~ 0 **'
                     print *,'pL:',PL,' pB:',PB
                     print *,'cornerL:',cornerL,' cornerB:',cornerB
                     print *,' I,J:',I,',',J
		  endif
		  print *,' '
               endif
               delta_TS_min=delta_TS
               IMAPSV=IMAP
            endif
         ENDDO
      ENDDO

      ne=time()
      if(jae_debug)print *,
     &     'Time for making the map:',(ne-ns),' seconds'
      jkpoints2=(jkpoints+1)*(jkpoints+1)
      if(IMAPSV.gt.0.and.IMAPSV.le.jkpoints2)then
         jpos(600)=IMAPSV
      else
         IMAPSV = (jkpoints2+1)/2
         jpos(600)=IMAPSV
      endif
      if(jae_jmapfine)then
         print *,'Final IMAP:->',IMAPSV,'->',jpos(600)
      endif
c     
      if(aspmaxj.gt.aspj)then
         signal='B'
         write(SIGMSG,'("WARNING: pixel at ",2f7.2,
     &        " angle to zenith is ",f4.0," deg")')xsrcL,xsrcB,aspmaxj
c     CALL ERROR(0,LOC)
c     SIGMSG=' '
      endif
c     
c
 50   continue
      if(jae_jmapfine)print *,jkpoints
      if(.not.jflag1)then
         if(jae_jmapfine)print *,'jmapfine return'
         srcL=srcL_sv
         srcB=srcB_sv
         CALL PIXEL_SELECT(JFALSE,jblank)
         LikTotaled = JFALSE
         CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         return
      endif
      if(jae_jmapfine)print *,'File to output->',LMAPFILE
c     
      if(width.lt.0.00001)then
         signal='W'
         SIGMSG=
     &        'ERROR:'//LOC//' :FITS map output attempted with zero ' //,
     *        'sidelength'
         CALL ERROR(-1,LOC)
         signal='W'
         SIGMSG=
     &        'ERROR:'//LOC//' :FITS map output attempted with zero ' //
     *        'sidelength'
         srcL=srcL_sv
         srcB=srcB_sv
         CALL PIXEL_SELECT(JFALSE,jblank)
         LikTotaled = JFALSE
         CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         return
      endif
C     Save CTL data
      tmp_CTLSCL=CTLSCL
      tmp_CTLMSZ1=CTLMSZ1
      tmp_CTLMSZ2=CTLMSZ2
      tmp_CTLORG(1)=CTLORG(1)
      tmp_CTLORG(2)=CTLORG(2)
c     
C     Reset CTL data for finemap
      CTLSCL=width
      CTLMSZ1=jkpoints+1
      CTLMSZ2=jkpoints+1
      CTLORG(1)=cornerL
      CTLORG(2)=cornerB
c
C     Write LMAP
      do i=1,70
         tst_tmp_doc(i:i)=' '
      enddo
      do i=1,10
         TMPDOC(i)=tst_tmp_doc
      enddo
      if(srcN.ne.' ')then
         write(TMPDOC(1),'("LIKE - v ",a)') VER,
     &        ' fine map of TS for ',srcN
      else
         write(TMPDOC(1),'(a,"- v ",a," fine map of TS")') PRG,VER
      endif
      in_dex1 = index(cmapfile, ' ') - 1
      if(in_dex1.le.0)in_dex1=1
      write(TMPDOC(2),'("Counts map: ",a)') cmapfile(1:in_dex1)
      in_dex1 = index(gmapfile, ' ') - 1
      if(in_dex1.le.0)in_dex1=1
      write(TMPDOC(3),'("Diffuse map: ",a)') gmapfile(1:in_dex1)
      WRITE(TMPDOC(4),'
     &     ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &     gamma,Ranal
      if(counts_fixed) then
         WRITE(TMPDOC(5),'
     &        ("Gmult,Gbias,Counts fixed at:"
     &        ,f10.4,f10.4,f12.4)')
     &        Gmult,Gbias,Counts
      else
         WRITE(TMPDOC(5),'
     &        ("Gmult,Gbias,Counts optimized at each point.")')
      endif
c     
      if(jj_tmp_flg)then
         thelong=srcL_sv
         if(thelong.lt.0)thelong = thelong + 360
         write(tst_tmp_doc,
     &        '("Maximum TS position: ",f7.3,1x,f7.3," with TS: ",
     *        f6.2)') thelong,srcB_sv,tmp_TS0
         nndoc=index(tst_tmp_doc,'      ')
         TMPDOC(6)=tst_tmp_doc(1:nndoc)
         do i=1,70
            tst_tmp_doc(i:i)=' '
         enddo
         best_x_tmp1 = best_x_tmp
         if(best_x_tmp1.lt.0)best_x_tmp1=best_x_tmp1+360
         write(tst_tmp_doc,'("Position estimate: ",f7.3,1x,f7.3)')
     &        best_x_tmp1,best_y_tmp
         nndoc=index(tst_tmp_doc,'      ')
         if(tst_name)then
            TMPDOC(7) = tst_tmp_doc(1:nndoc)//' ID: '//
     &           SRC_NAMES(jpos(502))
            print *,' ID: ',SRC_NAMES(jpos(502))
         else
            TMPDOC(7) = tst_tmp_doc(1:nndoc)//' SRC: '//
     &           SRC_NAMES(JPOS(502))
            print *,' SRC: ',SRC_NAMES(jpos(502))
         endif
         if(sv_dstsv(2,jpos(502)))then
            write(TMPDOC(8),'("Position error estimate: 68% - ",
     &           f4.0," 95% - ",f4.0)')
     *           sv_err68(jpos(502)),sv_err95(jpos(502))
            if(best_choice(jpos(502)).ne.'TS')then
               bstch = '% '
            else
               bstch = ' '
            endif
            if(sv_sigsv(jpos(502)).eq.'E'.or.sv_sigsv(jpos(502)).eq.
     &           'f'.or.sv_sigsv(jpos(502)).eq.'F')then
               write(TMPDOC(9),'(" ERROR in making map : code ",
     &              a1)')sv_sigsv(jpos(502))
            else
               write(TMPDOC(9),'("Position estimate calculated from ",
     &              a2,a2," contour  code: ",a1)')
     *              best_choice(jpos(502)),
     &              bstch,sv_sigsv(jpos(502))
            endif
         endif
      endif
c     Set map type to indicate likelihood map:
      TMPTYP = 'LSTA'
      MAPFILE=LMAPFILE
      CALL MAPRIT(TMPMAP,TMPTYP,TMPDOC,CTLMSZ1,CTLMSZ2)
      if(signal.ne.' '.and.signal.ne.' ')call error(0,LOC)
c     
C     Reset CTL data
      CTLSCL=tmp_CTLSCL
      CTLMSZ1=tmp_CTLMSZ1
      CTLMSZ2=tmp_CTLMSZ2
      CTLORG(1)=tmp_CTLORG(1)
      CTLORG(2)=tmp_CTLORG(2)
      srcL=srcL_sv
      srcB=srcB_sv
      CALL PIXEL_SELECT(JFALSE,jblank)
      LikTotaled = JFALSE
      CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      return
      END
c
