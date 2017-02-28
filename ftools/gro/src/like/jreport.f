c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C        SUBROUTINE JREPORT(jflag1,jflag2,jflag3,jflag4)
C
C
C  $Id: jreport.f,v 1.5 2013/05/21 19:08:26 irby Exp $
c
c       For LIKE v5.00
c       programmer: Joseph A. Esposito (jae@egret.gsfc.nasa.gov)
c       final form reached: 11-NOV-93
c--------------------------------------------------------------------
c
c effect: output VP report to files: Table.3, Table 4 and debug file PSF.rpt
c
C  $Log: jreport.f,v $
C  Revision 1.5  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.4  2007/01/31 21:04:11  irby
C  Replace use of FITSERROR routine (fitserro.f) with calls to fcerr/fcerrm.
C  The reliance of FITSERROR on the Fortran 'access' function prevents it
C  from compiling with gfortran (in which 'access' is not [yet?] implemented).
C  Also, fix some lines that were too long (>72 chars).
C
C  Revision 1.3  2002/12/26 17:42:54  irby
C  Fix read/write statements (for f90 compatibility): add/remove commas and
C  fix lines over 72 chars.
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
c Revision 5.11  1996/08/26  16:18:04  jae
c Repaired Table.3 output (cosmetic) for rch.
c
c Revision 5.10  1996/07/30  18:29:21  jae
c Fixed new typo at end of line 777 (an extra comma)
c
c Revision 5.9  1996/07/30  18:27:41  jae
c Found the typo: sqrt had one too few closing
c parentheses
c
c Revision 5.8  1996/07/30  18:23:27  jae
c another attempt at fixing this compile error
c
c Revision 5.7  1996/07/30  18:14:34  jae
c Another atempt to fix compile error on line 163
c 'Error: unbalanced parentheses, statement skipped'
c I have re-written the code snippets around this line
c and changed the continue mark-code distances
c
c Revision 5.6  1996/07/30  18:08:54  jae
c fixed all line length typo's globally
c
c Revision 5.5  1996/07/30  17:54:27  jae
c Repaired line length typo's and added
c output of Gmult(Gbias) values if restricted
c
c Revision 5.4  1996/07/30  17:41:48  jae
c Added output to Table.5
c Added output of PSF spectral index to
c Tables 3 and 4
c Changed verbose to jae_jreport globally
c
c Revision 5.3  1996/04/23  19:39:52  jae
c Changed Tables 3 and 4 output format slightly to
c accomdidate fluxes > 1000 (Tabl3 3) and general
c appearance.
c
c Revision 5.2  1996/03/06  21:21:07  jae
c Updated code: FITS_DIR read with GETENV only
c once at program start in like.f
c
c Revision 5.1  1996/02/29  20:48:29  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:31  jae
c Subroutine Module for like V5.00
c
c=====================================================================

      SUBROUTINE JREPORT(jflag1,jflag2,jflag3,jflag4)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id, fits_dir, context
      common /id/id
c     
      character(24) fdate
c
      integer block,status
      logical jflag1,jflag2,jdoneit,FEXIST,jflag3,jflag4
      character tmp_outp_jfile*50,input*80,ex_dir*60,ex_dir2*70
      character tmp_outp_jfile2*80,tmp_outp_date*24,tbc*2
      character(80) tmp_outp_jfile1,tmp_outp_jfile3,tmp_outp_jfile4
      character outp_jfile*100,jstyle*6,jjname*6,tc_tmp1*1,srcN_sv*18
      character tmp_DATE_OBS*8,tmp_DATE_END*8,tmp_read_doc*4900,
     &     tmp_read_doc2*200
      character tmp_read_doc3*9,jphase_doc*50
      integer jkar(3),jrepcnt
      character kywd*8,COMMENT*45
      real Emin,Emax,fudge,sfudge
c     
      id = '$Id: jreport.f,v 1.5 2013/05/21 19:08:26 irby Exp $'
      jrepcnt=0

      fits_dir = data_dir

      call MAPCPY(TMPMAP,Bmap,CTLMSZ1,CTLMSZ2)
      ntab3=15
      ntab4=15
      nntab3=0
      nntab4=0
      LOC = 'JREPORT'
      if(jae_jreport)write(*,'("In routine ",a)') LOC
      jkar(1)=0
      jkar(2)=0
      jkar(3)=0
      srcL_sv=srcL
      srcB_sv=srcB
c     
      if(EMAPFILE.eq.'as cnts')then
         EMAPFILE=CMAPFILE
         EMAPFILE(1:6)='exposr'
      endif
      do jjkm=1,50
         if(CMAPFILE(jjkm:jjkm).eq.'/')jkar(3)=1
         if(CMAPFILE(jjkm:jjkm).eq.'.')then
            if(jkar(1).eq.0)then
               jkar(1)=jjkm
               jkar(3)=0
            elseif(jkar(3).eq.0)then
               jkar(2)=jjkm
               goto 1
            elseif(jkar(3).ne.0)then
               jkar(1)=0
               jkar(3)=0
            endif
         endif
      enddo
 1    continue
      if(jkar(1).ne.0.and.jkar(2).ne.0)then
         if(CMAPFILE(jkar(1)+1:jkar(1)+2).eq.'vp'.and.
     &        (jkar(2)-jkar(1)-1).eq.6)then
            jphase_doc = CMAPFILE(jkar(1)+3:jkar(1)+6)
            jkar(3) = 4
         else
            jphase_doc = CMAPFILE(jkar(1)+1:jkar(2)-1)
            jkar(3) = jkar(2)-jkar(1)-1
         endif
      else
         write(jphase_doc,'(50("*"))')
         jphase_doc=CMAPFILE
         do jjkm=1,50
            if(jphase_doc(jjkm:jjkm).eq.'*')then
               jkar(3)=jjkm-1
               do jjkn=jjkm,50
                  jphase_doc(jjkm:jjkm)=' '
               enddo
               goto 2
            endif
         enddo
      endif
c
c
 2    continue
      print *,' CMAPFILE: ',CMAPFILE
      print *,' EMAPFILE: ',EMAPFILE
      print *,' GMAPFILE: ',GMAPFILE
c
      if(.not.jptype)then
         TMP_DATE_OBS='SUMMED  '
         TMP_DATE_END=' MAPS   '
      else
         TMP_DATE_OBS='NA      '
         TMP_DATE_END='        '
      endif
c     
c
c
      do kk=1,200
         tmp_read_doc(kk:kk)=' '
      enddo
      do kk=1,70
         if(kk.le.60)ex_dir(kk:kk)=' '
         ex_dir2(kk:kk)=' '
      enddo
c     
c
c
 3    continue
 4    continue
c     print *,'  Emin: ',CTLEMN
c     print *,'  Emax: ',CTLEMX
      Emin=1.0*CTLEMN
      Emax=1.0*CTLEMX
      CALL fknif(Emin,Emax,fudge,sfudge)
      xj_etmp_mn = float(CTLEMN)/1000.
      xj_etmp_mx = float(CTLEMX)/1000.
      if(xj_etmp_mx.gt.30)xj_etmp_mx=30
      jjname=' '
      jstyle=' '
      if(CTLEMN.lt.100)then
         write(jjname,'(f4.2)')xj_etmp_mn
      elseif(CTLEMN.ge.100.and.CTLEMN.lt.1000)then
         write(jjname,'(f4.1)')xj_etmp_mn
      else
         write(jjname,'(" ",f3.0)')xj_etmp_mn
      endif
      if(CTLEMX.lt.100)then
         write(jstyle,'(f4.2)')xj_etmp_mx
      elseif(CTLEMX.ge.100.and.CTLEMX.lt.1000)then
         write(jstyle,'(f4.1)')xj_etmp_mx
      else
         write(jstyle,'(f3.0," ")')xj_etmp_mx
      endif
      tmp_read_doc3(1:9)=jjname(1:4)//'-'//jstyle(1:4)
      jstyle=' '
      jjname=' '
      if(NSOURCE.lt.1)goto 1995
      if(NSOURCE.ne.JNSOURCE_sv)goto 1996
c
c
c
 11   print *,' '
      write(*,'("Enter pre-title for LPR output files: "$)')
      read(LU(12),'(a)') tmp_outp_jfile
      jtmp = index(tmp_outp_jfile, ' ') - 1
      if(jtmp.eq.0)then
         tmp_outp_jfile1='PSF.rpt'
	 tmp_outp_jfile4='Table.5'
	 tmp_outp_jfile2='Table.3'
	 tmp_outp_jfile3='Table.4'
      else
         tmp_outp_jfile1=tmp_outp_jfile(1:jtmp)//'.PSF.rpt'
	 tmp_outp_jfile4=tmp_outp_jfile(1:jtmp)//'.Table.5'
	 tmp_outp_jfile2=tmp_outp_jfile(1:jtmp)//'.Table.3'
	 tmp_outp_jfile3=tmp_outp_jfile(1:jtmp)//'.Table.4'
      endif
      numcar1=3
      INQUIRE (FILE=tmp_outp_jfile1,EXIST=FEXIST)
      if(FEXIST)then
         input='rm -f '//outp_jfile
         CALL SYSTEM(input)
      endif
      outp_jfile=tmp_outp_jfile1
      open(73,file=tmp_outp_jfile1,err=1999)
      outp_jfile=tmp_outp_jfile4
      open(74,file=tmp_outp_jfile4,err=1999)
      jjlen1 = index(tmp_outp_jfile1, ' ') - 1
      jjlen2 = index(tmp_outp_jfile2, ' ') - 1
      jjlen3 = index(tmp_outp_jfile3, ' ') - 1
      jjlen4 = index(tmp_outp_jfile4, ' ') - 1
      jstyle='TAB'
      jjname(1:1)='S'
      write(*,'("Output filenames are:")')
      write(*,
     &     '(A,/,A,/,A,/,A)')
     &     tmp_outp_jfile1(1:jjlen1),tmp_outp_jfile2(1:jjlen2),
     &     tmp_outp_jfile3(1:jjlen3),tmp_outp_jfile4(1:jjlen4)
      print *,' '
      write(74,'("likelihood version: ",A8)')JVER
      write(74,'("Date of output: ",A24)')fdate()
      write(*,'("likelihood version: ",A8)')JVER
      write(*,'("Date of output: ",A24)')fdate()
      tmp_outp_date=jblank//'    '
c     print *,' ftopen 1'
      SIGMSG=jblank//jblank//'          '
      jjlen0 = index(CMAPFILE, ' ') - 1
      if(jjlen0.eq.0)jjlen0=1
      MAPFILE=CMAPFILE(1:jjlen0)
      INQUIRE (FILE=MAPFILE,EXIST=FEXIST)
      if(.not.FEXIST)then
         in_dex = index(FITS_DIR, ' ')
         ex_dir=FITS_DIR(1:in_dex)
         in_dex = index(ex_dir, ' ') - 1
c     
c
         if(ex_dir(in_dex:in_dex).ne.'/'.and.in_dex.gt.0)then
            ex_dir(in_dex+1:in_dex+1)='/'
            in_dex=in_dex+1
         endif
         MAPFILE=ex_dir(1:in_dex)//CMAPFILE(1:jjlen0)
         INQUIRE (FILE=MAPFILE,EXIST=FEXIST)
         if(.NOT.FEXIST)then
            MAPFILE=CMAPFILE
            INQUIRE(FILE=MAPFILE,EXIST=FEXIST)
         endif
         if(.not.FEXIST)then
            print *,' CMAPFILE: ',CMAPFILE(1:jjlen0),' is NA'
            tmp_outp_date='NA  '//jblank
            goto 1103
         endif
      endif
      CALL ftopen(LU(14),MAPFILE,0,BLOCK,STATUS)
c     print *,' ftopen 1 STATUS:',STATUS
      if(STATUS.ne.0)then
         context='Error opening MAPFILE'
         call fcerr(context)
         go to 1992
c     print *,' CALL FITSERROR '
C        tmp_outp_date='NA  '//jblank
C        CALL FITSERROR(0,STATUS,'FTOPEN  ')
c     print *,' RETURN FROM FITSERROR'
C        STATUS=0
C        goto 1103
      endif
      KYWD='DATE    '
      CALL ftgkys(LU(14),KYWD,tmp_outp_date,COMMENT,STATUS)
      if(STATUS.ne.0)then
         context='Error reading keywords'
         call fcerr(context)
         go to 1993
C        tmp_outp_date='NA  '//jblank
C        CALL FITSERROR(0,STATUS,'FTGKYS  ')
C        STATUS=0
      endif
      if(jptype)then
	 KYWD='DATE-OBS'
	 CALL ftgkys(LU(14),KYWD,TMP_DATE_OBS,COMMENT,STATUS)
	 if(STATUS.ne.0)then
            context='Error reading keywords'
            call fcerr(context)
            go to 1993
C           tmp_DATE_OBS='NA      '
C           CALL FITSERROR(0,STATUS,'FTGKYS  ')
C           STATUS=0
	 endif
	 KYWD='DATE-END'
	 CALL ftgkys(LU(14),KYWD,TMP_DATE_END,COMMENT,STATUS)
	 if(STATUS.ne.0)then
            context='Error reading keywords'
            call fcerr(context)
            go to 1993
C           TMP_DATE_END='NA      '
C           CALL FITSERROR(0,STATUS,'FTGKYS  ')
C           STATUS=0
	 endif
      endif
      CALL FTCLOS(LU(14),STATUS)
C     if(STATUS.ne.0)then
C        write(SIGMSG,'("FTGKYS ERROR: FILE: ",A,2x,A)')
C    &        CMAPFILE(1:jjlen0),TMP_DATE_END(1:8)
C        CALL FITSERROR(0,STATUS,'FTCLOS  ')
C        STATUS=0
C     endif
 1103 continue
      jjlen5 = index(tmp_outp_date, ' ') - 1
      if(jjlen5.lt.1)jjlen5=1
      SIGMSG=jblank//jblank//'          '
      write(74,'("Counts map file: ",A,A," Created: ",A)')
     &     ex_dir(1:in_dex),CMAPFILE(1:jjlen0),tmp_outp_date(1:jjlen5)
      write(*,'("Counts map file: ",A,A," Created: ",A)')
     &     ex_dir(1:in_dex),CMAPFILE(1:jjlen0),tmp_outp_date(1:jjlen5)
c     print *,' ftopen 2'
      SIGMSG=jblank//jblank//'          '
      jjlen0 = index(EMAPFILE, ' ') - 1
      if(jjlen0.eq.0)jjlen0=1
      MAPFILE=EMAPFILE(1:jjlen0)
      tmp_outp_date=jblank//'    '
      INQUIRE (FILE=MAPFILE,EXIST=FEXIST)
      if(.not.FEXIST)then
         MAPFILE=ex_dir(1:in_dex)//EMAPFILE(1:jjlen0)
         INQUIRE (FILE=MAPFILE,EXIST=FEXIST)
         if(.not.FEXIST)then
            print *,' EMAPFILE: ',EMAPFILE(1:jjlen0),' is NA'
            tmp_outp_date='NA  '//jblank
            goto 1105
         endif
      endif
      CALL ftopen(LU(14),MAPFILE,0,BLOCK,STATUS)
c     print *,' ftopen 2 STATUS:',STATUS
      if(STATUS.ne.0)then
         context='Error opening MAPFILE'
         call fcerr(context)
         go to 1992
C        tmp_outp_date='NA  '//jblank
C        CALL FITSERROR(0,STATUS,'FTOPEN  ')
C        STATUS=0
C        goto 1105
      endif
      KYWD='DATE    '
      CALL ftgkys(LU(14),KYWD,tmp_outp_date,COMMENT,STATUS)
      if(STATUS.ne.0)then
         context='Error reading keywords'
         call fcerr(context)
         go to 1993
C        tmp_outp_date='NA  '//jblank
C        CALL FITSERROR(0,STATUS,'FTGKYS  ')
C        STATUS=0
      endif
      CALL FTCLOS(LU(14),STATUS)
      if(STATUS.ne.0)then
         context='Error closing MAPFILE'
         call fcerr(context)
         go to 1992
C        CALL FITSERROR(0,STATUS,'FTCLOS  ')
C        STATUS=0
      endif
 1105 continue
      jjlen5 = index(tmp_outp_date, ' ') - 1
      if(jjlen5.lt.1)jjlen5=1
      SIGMSG=jblank//jblank//'          '
      write(74,'("Exposure map file: ",A,A," Created: ",A)')
     &     ex_dir(1:in_dex),EMAPFILE(1:jjlen0),tmp_outp_date(1:jjlen5)
      write(*,'("Exposure map file: ",A,A," Created: ",A)')
     &     ex_dir(1:in_dex),EMAPFILE(1:jjlen0),tmp_outp_date(1:jjlen5)
c     print *,' ftopen 3'
      SIGMSG=jblank//jblank//'          '
      jjlen0 = index(GMAPFILE, ' ') - 1
      if(jjlen0.eq.0)jjlen0=1
      MAPFILE=GMAPFILE
      tmp_outp_date=jblank//'    '
      INQUIRE (FILE=MAPFILE,EXIST=FEXIST)
      if(.not.FEXIST)then
         MAPFILE=ex_dir(1:in_dex)//GMAPFILE(1:jjlen0)
         INQUIRE (FILE=MAPFILE,EXIST=FEXIST)
         if(.not.FEXIST)then
            print *,' GMAPFILE: ',GMAPFILE(1:jjlen0),' is NA'
            tmp_outp_date='NA  '//jblank
            goto 1107
         endif
      endif
      CALL ftopen(LU(14),MAPFILE,0,BLOCK,STATUS)
c     print *,' ftopen 3 STATUS:',STATUS
      if(STATUS.ne.0)then
         context='Error opening MAPFILE'
         call fcerr(context)
         go to 1992
C        tmp_outp_date='NA  '//jblank
C        CALL FITSERROR(0,STATUS,'FTOPEN  ')
C        STATUS=0
C        goto 1107
      endif
      KYWD='FILE-VER'
      CALL ftgkys(LU(14),KYWD,tmp_outp_date,COMMENT,STATUS)
      if(STATUS.ne.0)then
         context='Error reading keywords'
         call fcerr(context)
         go to 1993
C        tmp_outp_date='NA  '//jblank
C        CALL FITSERROR(0,STATUS,'FTGKYS  ')
C        STATUS=0
      endif
      CALL FTCLOS(LU(14),STATUS)
      if(STATUS.ne.0)then
         context='Error closing MAPFILE'
         call fcerr(context)
         go to 1993
C        CALL FITSERROR(1,STATUS,'FTCLOS  ')
C        STATUS=0
      endif
 1107 continue
      jjlen5 = index(tmp_outp_date, ' ') - 1
      if(jjlen5.lt.1)jjlen5=1
      SIGMSG=jblank//jblank//'          '
      write(74,'("Diffuse model file: ",A," Galdif version: ",A)')
     &     GMAPFILE(1:jjlen0),tmp_outp_date(1:jjlen5)
      write(74,'("Last setting of Ranal: ",F6.2)')Ranal
      write(74,'("Total Number of sources in PSF map: ",I3)')NSOURCE
      write(*,'("Total Number of sources in PSF map: ",I3)')NSOURCE
      SIGMSG=jblank//jblank//'          '
      kkp=0
      if(jflag3)then
	 do jj=1,NSOURCE
            if(SRC_PARMS(jj,8).gt.0.05)kkp=kkp+1
	 enddo
      else
	 kkp=NSOURCE
      endif
      if(jflag3)write(74,'("Total Number of active sources:",I4)')kkp
      if(jflag3)write(*,'("Total Number of active sources: ",I3)')kkp
      write(74,'("Coord. Sys.: ",A," Map Center: ",2f8.2)')coord_sys,
     &     SC_LII,SC_BII
      write(74,'("Present Analysis Center: ",2f8.2)')SC_LJJ,SC_BJJ
      write(74,'("Maximum Angle from Analysis Center: ",f7.2)')aspj
      roi_tmpL1=ROIORG(1)
      roi_tmpL2=ROIEND(1)
      roi_tmpB1=ROIORG(2)
      roi_tmpB2=ROIEND(2)
      CALL L_CHECK(roi_tmpL1)
      CALL L_CHECK(roi_tmpL2)
      CALL L_CHECK(roi_tmpB1)
      CALL L_CHECK(roi_tmpB2)
      write(74,'("ROI L(RA): ",2f8.2," B(DEC): ",2f7.2)')roi_tmpL1,
     &     roi_tmpL2,roi_tmpB1,roi_tmpB2
      if(Restrict_Gmult)then
         write(74,'("Present Gmult Restrictions: ",l2,
     &        " Gmult = ",f5.1)') Restrict_Gmult,Gmult
      else
         write(74,'("Present Gmult Restrictions: ",l2)')
     &        Restrict_Gmult
      endif
c     
      if(Restrict_Gbias)then
         write(74,'("Present Gbias Restrictions: ",l2,
     &        " Gbias = ",f5.1)')  Restrict_Gbias,Gbias
      else
         write(74,'("Present Gbias Restrictions: ",l2)')
     &        Restrict_Gbias
      endif
c     
      close(unit=74)
c     
      do kk=1,NSOURCE
         jpos(kk)=kk
      enddo
      print *,' '
      print *,' Scoring and sorting source in PSF map'
      print *,' '
      do 20 jj=1,NSOURCE
         svo_infx(jj)=SRC_PARMS(jj,1)
         svo_infy(jj)=SRC_PARMS(jj,2)
         sigsv = sv_sigsv(jj)
         if(sv_dstsv(1,jj).eqv.JFALSE.and.(.not.jflag3.or.
     &        (jflag3.and.SRC_PARMS(jj,8).gt.0.05)))then
            print *,' '
            write(*,'(" WARNING: Source number ",i3," named ",
     &           A18)')jj,SRC_NAMES(jj)
            print *,'This source was not position optimized with ',
     *           'command',
     &           ' ommand LPO[N]'
         endif
         if(sv_dstsv(2,jj).eqv.JTRUE)then
            srcL = best_x(jj)
            srcB = best_y(jj)
         else
            srcL=SRC_PARMS(jj,1)
            srcB=SRC_PARMS(jj,2)
            if(.not.jflag3.or.(jflag3.and.
     &           SRC_PARMS(jj,8).gt.0.05))then
               print *,' '
               write(*,
     &              '(" WARNING: For source number ",i3," named ",A18,
     &              " Error bars were not calculated !")')
     *              jj,SRC_NAMES(jj)
            endif
         endif
         if(coord_sys.eq.'G')then
            jjret = 0
            tmpg1 = srcL
            tmpg2 = srcB
            CALL CELGALD('GC',tmpc1,tmpc2,tmpg1,tmpg2,jjret)
         else
            jjret = 0
            tmpc1 = srcL
            tmpc2 = srcB
            CALL CELGALD('CG',tmpc1,tmpc2,tmpg1,tmpg2,jjret)
            sv_gal_long(jj)=tmpg1
            sv_gal_lat(jj)=tmpg2
         endif
         sv_gal_long(jj)=tmpg1
         sv_gal_lat(jj)=tmpg2
         sv_cel_long(jj)=tmpc1
         sv_cel_lat(jj)=tmpc2
 20   continue
      kk=0
 21   continue
      jdoneit = JTRUE
      kk = kk + 1
      do 25 jjk = 2,NSOURCE+1-kk
         if(sv_cel_long(jpos(jjk)).lt.sv_cel_long(jpos(jjk-1)))then
            jpos_tmp = jpos(jjk)
            jpos(jjk) = jpos(jjk-1)
            jpos(jjk-1) = jpos_tmp
            jdoneit = JFALSE
         endif
 25   continue
      if(jdoneit.eqv.JFALSE.and.kk.lt.NSOURCE-1)goto 21
c     
c     
      write(73,
     &     '("name           ID                  best position",
     &     "      error"$)')
      write(73,
     &     '("       Min-Max 68%   Min-Max 95%  Choice  A Flags")')
      if(jstyle(1:3).eq.'TAB')then
         INQUIRE (FILE=tmp_outp_jfile2,EXIST=FEXIST)
         if(FEXIST)then
            tmp_read_doc2='rm -f '//tmp_outp_jfile2(1:jjlen2)
            CALL SYSTEM(tmp_read_doc2)
         endif
         open(43,file=tmp_outp_jfile2,err=1999)
         INQUIRE (FILE=tmp_outp_jfile3,EXIST=FEXIST)
         if(FEXIST)then
            tmp_read_doc2='rm -f '//tmp_outp_jfile3(1:jjlen3)
            CALL SYSTEM(tmp_read_doc2)
         endif
         open(44,file=tmp_outp_jfile3,err=1999)
c
         write(43,'(///45x,''Table 3'')')
         write(43,'(/20x,"EGRET SOURCE CATALOG   EGRET SOURCES  -- "$)')
         write(43,'("VIEWING PERIOD: "$)')
         write(43,'(A$)')jphase_doc(1:jkar(3))
         write(43,'(/)')
         write(44,'(///45x,''Table 4'')')
         write(44,'(/20x,"EGRET SOURCE CATALOG   EGRET SOURCES  -- "$)')
         write(44,'("VIEWING PERIOD "$)')
         write(44,'(A$)')jphase_doc(1:jkar(3))
         write(44,'(" --  Detections and Upper Limits",/)')
c     
c
c
         write(44,'("Observation Period: ",A8," thru ",A8,8x,
     &        "Output print date: ",A24,1x,"LIKE V",A8)')
     &        TMP_DATE_OBS(1:8),TMP_DATE_END(1:8),fdate(),JVER(1:8)
         write(43,'("Observation Period: ",A8," thru ",A8,
     &        8x,"Output print date: ",A24,1x,"LIKE V",A8)')
     &        TMP_DATE_OBS(1:8),TMP_DATE_END(1:8),fdate(),JVER(1:8)
c     
         write(44,'("Analysis Energy range: ",A9,"MeV",
     &        15x,"Output Filename: ",A,/)')
     &        tmp_read_doc3(1:9),tmp_outp_jfile3(1:jjlen3)
         write(43,'("Analysis Energy range: ",A9,"MeV",
     &        15x,"Output Filename: ",A,/)')
     &        tmp_read_doc3(1:9),tmp_outp_jfile2(1:jjlen2)
c     
c
         write(43,'(1x,"Name          Ident                ",
     &        "      Measured Position        Error",
     &        "(arcmin)      Flux       Spect   True ",
     &        "position     TS(sig)        Cnts   Gmult   Gbias ")')
         write(43,'(2x,"              and notes            ",
     &        " l      b     ra     dec        68%   95%",
     &        "       val   err   Index     l       b")')
         write(43,'(1x,156(''-'')/)')
c     
c
         write(44,'(1x,"Name          Ident               ",
     &        "      Measured Position        Error",
     &        "(arcmin)       Flux      Upper   True ",
     &        "position     TS(sig)   Cnts   Gmult   Gbias  Spect")')
         write(44,'(1x,"              and notes           ",
     &        "   l      b     ra     dec      68%    95%",
     &        "       val   err   Limit     l       b")')
         write(44,'(1x,150(''-'')/)')
      endif
c     
c
      jjp=0
c     
      do i=1,NSOURCE
c     print *,i,' In loop: ',jjp,' out of ',kkp
	 if(jjp.gt.kkp)goto 1205
         jj=jpos(i)
	 if(jflag3.and.SRC_PARMS(jj,8).lt.0.05)goto 120
	 jjp=jjp+1
c     print *,' In if(jflag3): ',jjp
c
c     Set values in arrays to compensate for problems with LM command
c     which does NOT set arrays.  first set xbest and ybest to SRC_PARMS values
c     if lpea has not been run. Then set srcL, srcB to xbest and ybest.
c     run srctest(.false.,jblank) and place the returned values in the proper
c     array locations
c     
	 if(sv_dstsv(2,jj).eqv.JFALSE)then
            best_choice(jj)='NA'
            sv_err68(jj)=0
            sv_err95(jj)=0
	 endif
c     
	 if(.not.sv_dstsv(1,jj))then
c     print *,' .NOT.sv_dst(1,jj)'
            print *,'--------------------------------------------------'
            print *,i,' PSF: ',jj,'  NAME: ',SRC_NAMES(jj)
            print *,' Source requires optimization-> TS saved: ',
     &           sv_dstsv(1,jj)
            print *,' '
            srcL=SRC_PARMS(jj,1)
            srcB=SRC_PARMS(jj,2)
            print *,'srcL,srcB: ',srcL,srcB
            CALL PIXEL_SELECT(JFALSE,jblank)
c     print *,' err0'
            if(signal.ne.' ')call error(0,LOC)
            Counts = 0
            call PSFREPLACE(jj)
c     print *,' err1'
            if(signal.ne.' ')call error(0,LOC)
            CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
c     print *,'err2'
            if(signal.ne.' ')call error(0,LOC)
            report=JTRUE
            report2=JTRUE
            CALL SRCTEST(JFALSE)
c     print *,' err3'
            if(signal.ne.' ')call error(0,LOC)
            cnts_sv=SRC_PARMS(jj,3)
            if((ABS(Counts-cnts_sv)/cnts_sv).gt.0.02)then
               print *,'**********************************'
               print *,' dC/C has changed > 0.02 !!!'
               print *,' Old value: ',cnts_sv
               print *,' New value: ',Counts
               print *,'***********************************'
               SRC_PARMS(jj,3)=Counts
               call PSFREPLACE(jj)
               jrepcnt=jrepcnt+1
               goto 255
            elseif(jrepcnt.eq.0)then
               Counts=cnts_sv
               CALL MAPCPY(Bmap,TMPMAP,CTLMSZ1,CTLMSZ2)
               goto 255
            else
               SRC_PARMS(jj,3)=cnts_sv
               Counts=cnts_sv
               SRC_PARMS(jj,3)=Counts
               call PSFREPLACE(jj)
            endif
 255        print *,'-------------------------------------------------'
c     
c     
            liktotaled=.false.
            CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
            report=JFALSE
            report2=JFALSE
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
            SRC_PARMS(jj,5)=3
	 elseif(SRC_PARMS(jj,5).lt.-0.5)then
            write(*,'("ERROR occurred during optimization by: "$)')
            if(SRC_PARMS(jj,5).lt.-1.5)then
               write(*,'("LPO[N] command")')
            else
               write(*,'("LM command")')
            endif
	 endif
c        
	 sv_knif_flx_err=sqrt(sv_flx(2,jj)**2.0+
     &        (sv_flx(1,jj)*sfudge/fudge)**2.0)
         if(coord_sys.eq.'C')then
            if(sv_cel_long(jj).lt.0)sv_cel_long(jj)=sv_cel_long(jj)+360.
            srcL=sv_cel_long(jj)
            srcB=sv_cel_lat(jj)
         else
            if(sv_gal_long(jj).lt.0)sv_gal_long(jj)=sv_gal_long(jj)+360.
            srcL=sv_gal_long(jj)
            srcB=sv_gal_lat(jj)
         endif
c     
         srcN=' '
         CALL GETSRCNM()
         write(tmp_read_doc2(1:70),'(70(" "))')
         write(tmp_read_doc2(21:35),
     &        '(3x,f4.0,3x,f4.0,1x)')sv_err68(jj),sv_err95(jj)
         if(.not.sv_dstsv(3,jj))
     &        write(tmp_read_doc2(41:45),'(f5.0)')sv_upperlim(jj)
         srcN_sv=SRC_NAMES(jj)
         if(sv_dstsv(6,jj))then
            srcN_sv=SRC_NAMES(jj)
            tmp_sv_true_x=sv_true_x(jj)
            if(tmp_sv_true_x.lt.0)tmp_sv_true_x=
     &           tmp_sv_true_x+360
            write(tmp_read_doc2(51:64),'(2f7.2)')
     &           tmp_sv_true_x,sv_true_y(jj)
         else
            if(srcN_sv.eq.srcN)then
               srcN_sv='                  '
            endif
         endif
         tmp_best_x=best_x(jj)
         if(tmp_best_x.lt.0)tmp_best_x=tmp_best_x+360
c     
         tc_tmp1=' '
         tbc=best_choice(jj)
         if(.not.(tbc.eq.'TS'.or.(tbc(1:1).ge.'0'.
     &        and.tbc(1:1).le.'9')))best_choice(jj)='NA'
         if(best_choice(jj).ne.'TS'.and.best_choice(jj).ne.'NA')
     &        tc_tmp1='%'
         write(73,'(a14,1x,a18,1x,2f7.2,$)')srcN,srcN_sv,
     &        tmp_best_x,best_y(jj)
         write(73,'(a15,2x,$)')tmp_read_doc2(21:35)
         write(73,'(4f7.2,$)')sv_rad_68_min(jj),
     &        sv_rad_68_max(jj),sv_rad_95_min(jj),sv_rad_95_max(jj)
         write(73,'(4x,a2,a1,2x,f3.0,1x,f4.1,2x$)')best_choice(jj),
     &        tc_tmp1,SRC_PARMS(jj,5),SRC_PARMS(jj,8)
         write(73,'(10l3,2x,2f7.1)') (sv_dstsv(k,jj), k=1,10),
     &        SRC_PARMS(jj,1),SRC_PARMS(jj,2)
c     
         if(jstyle(1:1).eq.'T')then
            if(svn_TS(jj).ge.25)then
               sv_dstsv(3,jj)=JTRUE
            else
               sv_dstsv(3,jj)=JFALSE
            endif
            if(sv_dstsv(3,jj).eqv.JTRUE)then
               ntab3 = ntab3+1
               nntab3=nntab3+1
               lltst = mod(ntab3,65)
               if(lltst.eq.0)then
                  ntab3=8
                  write(43,'(//45x,"Table 3 continued"/)')
c
c
                  write(43,'(1x,"Name          Ident                ",
     &                 "      Measured Position        Error",
     &                 "(arcmin)      Flux       Spect    True ",
     &                 "position     TS(sig)       Cnts   Gmult   ",
     *                 "Gbias")')
                  write(43,'(2x,"            and notes             ",
     &                 " l      b     ra     dec        68%   95%",
     &                 "      val   err   Index     l       b")')
                  write(43,'(1x,156(''-'')/)')
                  
               endif
c     
               if(jae_jreport)write(*,'(" SOURCE ",a," ID: ",a,
     &              " to Table.3")')  srcN, srcN_sv
               write(43,
     &             '(a14,1x,a18,1x,4f7.2,2x,a15,2x,f7.1,f6.1,2x,f6.3$)')
     &              srcN,srcN_sv,sv_gal_long(jj),sv_gal_lat(jj),
     *              sv_cel_long(jj),
     &              sv_cel_lat(jj),tmp_read_doc2(21:35),sv_flx(1,jj),
     &              sv_knif_flx_err,SRC_PARMS(jj,4)
               write(43,'(2x,A14,2x,f7.1$)')
     &              tmp_read_doc2(51:64),svn_TS(jj)
               write(43,'("(",f5.1$)')sqrt(svn_TS(jj))
               write(43,'(")",1x,f7.1,2f8.4)')sv_cnts(1,jj),
     &              sv_params(1,jj),sv_params(2,jj)
c     
            else
c     
               ntab4=ntab4+1
               nntab4=nntab4+1
               lltst=mod(ntab4,65)
               if(lltst.eq.0)then
                  ntab4=8
                  write(44,'(//45x,"Table 4 continued"/)')
                  write(44,'(1x,"Name          Ident               ",
     &                 "      Measured Position        Error",
     &                 "(arcmin)       Flux      Upper   True ",
     &                 "position     TS(sig)   Cnts   Gmult   Gbias  ",
     *                 "Spect")')
                  write(44,'(1x,"              and notes           ",
     &                 "   l      b     ra     dec      68%    95%",
     &                 "       val   err   Limit     l       b")')
                  write(44,'(1x,152(''-'')/)')
               endif
c
               if(jae_jreport)print *,' SOURCE ',srcN,' ID: ',
     &              srcN_sv,' to Table.4'
               write(44,'(a14,1x,a18,1x,4f7.2,1x,a15,4x,f5.1,2x,
     *              f5.1,1x$)')
     &              srcN,srcN_sv,sv_gal_long(jj),sv_gal_lat(jj),
     &              sv_cel_long(jj),sv_cel_lat(jj),tmp_read_doc2(21:35),
     &              sv_flx(1,jj),sv_knif_flx_err
               write(44,'(2x,a5,3x,a14,f7.1$)')
     &              tmp_read_doc2(41:45),tmp_read_doc2(51:64),svn_TS(jj)
               write(44,'("(",f3.1,")",f6.1,1x,2f8.4,f7.3)')
     &              sqrt(svn_TS(jj)),sv_cnts(1,jj),sv_params(1,jj),
     &              sv_params(2,jj),SRC_PARMS(jj,4)
            endif
c     
         elseif(jstyle(1:1).eq.'C')then
            print *,' '
            print *,' SOURCE ',srcN(1:12),' ID: ',
     *           srcN_sv(1:18),' to file: ',
     &           outp_jfile(1:numcar1+8)
c     
            write(43,'("Name         Ident               Period  "$)')
            write(43,'("Spectral   True"$)')
            write(43,'(" Position     Measured Position      ",
     *           "Error  "$)')
            write(43,'("alpha  beta  theta")')
            write(43,
     &           '("                                         Index"$)')
            write(43,'("       l     b"$)')
            write(43,'("        l      b    RA    Dec (arcmin)")')
            write(43,'(118("-"))')
c     
            write(43,'(a12,1x,a18,1x," PH 1  0.00+/-0.00")$')
     &           srcN(1:12),srcN_sv(1:18)
            write(43,'(2x,a14,2f7.2,$)')tmp_read_doc2(51:64),
     &           sv_gal_long(jj),sv_gal_lat(jj)
            write(43,'(2f7.2$)')sv_cel_long(jj),sv_cel_lat(jj)
            write(43,'(3f7.1$)')sv_err95(jj),sv_rad_95_max(jj),
     &           sv_rad_95_min(jj)
            xxx5 = gtcirc(best_x(jj),best_y(jj),sv_tmp_95_x(jj),
     &           sv_tmp_95_y(jj))
            xxx4=gtcirc(best_x(jj),best_y(jj),sv_tmp_95_x(jj),
     *           best_y(jj))
            xxx3=0
            if(xxx5.ne.0)xxx3=xxx4/xxx5
            write(43,'(f7.1)')xxx3
c     
            write(43,'(//,7x,"exposure  significance   spectral    Flux"
     &           ,"    Upper   Counts   exp.  Zenith    Gmult   Gbias  "
     *           ,"Notes")')
            write(43,'(7x,"                         ",
     *           "index          "$)')
            write(43,'("    Limit                  angle",/,
     *           7x,70("-"))')
            write(43,'("  PH 1  ",f5.1,"   0.00+/-0.00  "$)')
     &           sqrt(svn_TS(jj))
            write(43,'(f6.1,"+/-",f5.1,$)')sv_flx(1,jj),sv_knif_flx_err
            write(43,'(2x,a5,2(2x,f7.1),2x,$)')tmp_read_doc2(41:45),
     &           sv_cnts(1,jj),sv_expose(jj)/1.e7
            write(43,'(" -- ",2(2x,f6.3))')
     *           sv_params(1,jj),sv_params(2,jj)
         endif
 120     continue
      enddo
c     
 1205 continue
c
      do kk=1,70
         if(kk.le.60)ex_dir(kk:kk)=' '
         ex_dir2(kk:kk)=' '
      enddo
c     CALL GETENV('MISC_DIR',ex_dir)
      ex_dir = misc_dir
      in_dex = index(ex_dir, ' ') - 1
c     
c     
      if(ex_dir(in_dex:in_dex).ne.'/')then
         ex_dir(in_dex+1:in_dex+1)='/'
         in_dex=in_dex+1
      endif
c
c
      kk=0
      if(jstyle(1:1).eq.'T')then
         if(nntab3.eq.0)write(43,
     &        '(" No Sources Detected with 25 < TS",//)')
         if(jae_jreport)print *,'nntab3:',nntab3,
     &        '  in_dex;',in_dex
         if(in_dex.lt.2)goto 140
         ex_dir2=ex_dir(1:in_dex)//'Table.3.list'
         in_dex2 = index(ex_dir2, ' ') - 1
         if(in_dex2.lt.1)in_dex2=1
         INQUIRE (FILE=ex_dir2,EXIST=FEXIST)
         if(.not.FEXIST.and.jae_jreport)print *,'File: ',
     &        ex_dir2(1:in_dex2),' does not exist'
         if(FEXIST)then
            open(46,file=ex_dir2,err=140)
 130        read(46,'(a)',end=140) tmp_read_doc2
            numcar = index(tmp_read_doc2, ' ') - 1
            write(43,'(A)')tmp_read_doc2(1:numcar)
            kk=kk+1
            if(kk.gt.30)goto 140
            goto 130
         endif
 140     continue
         close(unit=43)
         close(unit=46)
         kk=0
         if(nntab4.eq.0)write(44,
     &        '(" No Sources Detected with 9 < TS < 25",//)')
         if(jae_jreport)print *,'nntab4:',nntab4,
     &        '  in_dex;',in_dex
         if(in_dex.lt.2)goto 150
         ex_dir2=ex_dir(1:in_dex)//'Table.4.list'
         in_dex2 = index(ex_dir2, ' ') - 1
         if(in_dex2.lt.1)in_dex2=1
         INQUIRE (FILE=ex_dir2,EXIST=FEXIST)
         if(.not.FEXIST.and.jae_jreport)print *,'File: ',
     &        ex_dir2(1:in_dex2),' does not exist'
         if(FEXIST)then
            open(47,file=ex_dir2,err=155)
         endif
         
 150     read(47,'(a)',end=155) tmp_read_doc2
         numcar = index(tmp_read_doc2, ' ') - 1
         write(44,'(a)')tmp_read_doc2(1:numcar)
         kk=kk+1
         if(kk.gt.30)goto 155
         goto 150
             
 155     continue
         close(unit=47)
         close(unit=44)
      endif
      close(unit=73)
      write(6,'(" THERE WERE ",I3,
     &     " ENTRIES WRITTEN TO FILE: ",A)')(nntab3+nntab4),
     &     tmp_outp_jfile1(1:jjlen1)
      write(6,'(" THERE WERE ",I3,
     &     " ENTRIES WRITTEN TO FILE: ",A)')nntab3,
     &     tmp_outp_jfile2(1:jjlen2)
      write(6,'(" THERE WERE ",I3,
     &     " ENTRIES WRITTEN TO FILE: ",A)')nntab4,
     &     tmp_outp_jfile3(1:jjlen3)
      write(6,'(" Like information was written to file: ",A)')
     &     tmp_outp_jfile4(1:jjlen4)
      if(jrepcnt.eq.0)then
         CALL MAPCPY(Bmap,TMPMAP,CTLMSZ1,CTLMSZ2)
      else
         CALL PSFBLD
      endif
      GOTO 1994
C     Print out the FITSIO error number and text string
 1992 CALL FCERRM(STATUS)
      GOTO 1994
C     Print out the FITSIO error number and text string & close file
 1993 CALL FCERRM(STATUS)
      CALL FTCLOS(LU(14),STATUS)
 1994 continue
      return
c
 1995 print *,' '
      print *,' There are NO sources in the other PSF map.',
     &     '  Returning to main menu.'
      print *,' '
      return
 1996 continue
      print *,' '
      print *,' The number of sources currently in the PSF map',
     &     ' does NOT equal the number when'
      print *,' the sources were last simultaneously optimized for',
     &     ' position !'
      print *,' '
      if(jflag1)then
         write(*,
     &        '(" To continue report enter C (cr to abort report)"$)')
         read(LU(12),'(A)')sigsv
         if(sigsv.eq.'C'.or.sigsv.eq.'c')then
            sigsv = ' '
            goto 11
         endif
         sigsv = ' '
         return
      else
         goto 11
      endif
 1999 print *,' '
      jjlen = index(outp_jfile, ' ') - 1
      print *,' Error opening file ',outp_jfile(1:jjlen)
      print *,' '
      print *,' returning to main menu'
      print *,' '
      close(unit=43)
      close(unit=44)
      close(unit=73)
      return
      end
c
c
