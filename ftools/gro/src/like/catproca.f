C       SUBROUTINE CATPROCA()
c
C
C  $Id: catproca.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++     effect: A position/flux catalog entry is generated.  This
C++		catalog is designed for the third EGRET catalog
C***********************************************************************
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c	NONE
c-----------------------------------------------------------------------
c	definitions of key parameters:
c	estL estB, ra dec, estimated position in gal. and cel. coordinates
c
c
c
C=======================================================================
C LIKE Version: 5.53 DELIVERED: March 7, 1997, Programmer J.A. Esposito
C+            Updated: by JAE
C Installation of RCS lines: 07 MAR 1997 by JAE
c
c
C=======================================================================
C  $Log: catproca.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2006/04/17 20:54:28  irby
C  Updates for compliance with g95 and gfortran:
C
C  - Replace call to lnblnk with equivalent call to len_trim.
C    lnblnk (a routine which used to live in libg2c) is not currently
C    available with g95 or gfortran.
C
C  - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
C
C  - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
C
C  - Fix non-integer loop variables.
C
C  Revision 1.1  2002/04/16 20:27:28  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 1.11  1997/09/17  14:38:09  jae
c Added check for error analysis being run
c if .NOT. error analysis use TS position
c 	else use BEST (error analysis) position
c
c Revision 1.10  1997/04/07  20:41:45  jae
c Updated output to unit=73 and unit=83 for
c catalog output.
c This is a test version for alignment (like554a).
c
c Revision 1.9  1997/04/07  03:34:10  jae
c Repaired error in call to celgald: Celestial
c and galactic coordinates were reversed.
c
c Revision 1.8  1997/03/08  20:55:53  jae
c AGAIN-Updated output to units 73 and 83.
c
c Revision 1.7  1997/03/08  20:39:05  jae
c Updated output to units 73 and 83.
c
c Revision 1.6  1997/03/08  20:13:19  jae
c changed bestch(3) to bestch*3 in declaration.
c
c Revision 1.5  1997/03/08  20:10:01  jae
c Removed variable best_choice and used local
c variable bestch(1:3)
c
c Revision 1.4  1997/03/08  20:06:28  jae
c repaired output format again.
c
c Revision 1.3  1997/03/08  17:39:06  jae
c Adjusted output for unit=83 (summary file)
c
c Revision 1.2  1997/03/07  22:02:08  jae
c fixed output alignment
c
c Revision 1.1  1997/03/06  20:56:24  jae
c Initial revision
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE CATPROCA()
C
C     Common blocks used:
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
      CHARACTER break*110,obs*20,obs2*10
      CHARACTER notes*20, bestch*3
      CHARACTER FILEOUT1*80,FILEOUT2*80
      CHARACTER vp*6
      LOGICAL catalog_sv
      character(70) mapdocsav


      id = '$Id: catproca.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      if (.not.publish)return
      do J=1,80
         FILEOUT1(j:j)=' '
         FILEOUT2(j:j)=' '
         if (j.le.20)then
            obs(j:j)=' '
            notes(j:j)=' '
            if (j.le.10)obs2(j:j)=' '
         endif
      enddo

c     do initialization

      LOC='CATPROCA'
      break=
     & '------------------------------------------------------'//
     & '------------------------------------------------------'
       mapdocsav=mapdoc(1)
       mapdoc(1)=CMAPFILE(1:70)
       report=.false.

c     set obs phrase

       i=index(mapdoc(1),'.g1_')
       if (i.gt.0)then
          obs='Phase 1'
          obs2='P1    '
          goto 10
       endif
       i=index(mapdoc(1),'.g2_')
       if (i.gt.0)then
          obs='Phase 2'
          obs2='P2    '
          goto 10
       endif
       i=index(mapdoc(1),'.g3_')
       if (i.gt.0)then
          obs='Phase 3'
          obs2='P3    '
          goto 10
       endif
       i=index(mapdoc(1),'.g4_')
       if (i.gt.0)then
          obs='Phase 4'
          obs2='P4    '
          goto 10
       endif
       i=index(mapdoc(1),'.g5_')
       if (i.gt.0)then
          obs='Phase 5'
          obs2='P5    '
          goto 10
       endif
       i=index(mapdoc(1),'.g6_')
       if (i.gt.0)then
          obs='Phase 6'
          obs2='P6    '
          goto 10
       endif
       i=index(mapdoc(1),'.g7_')
       if (i.gt.0)then
          obs='Phase 7'
          obs2='P7    '
          goto 10
       endif
       i=index(mapdoc(1),'.c1_')
       if (i.gt.0)then
          obs='Phase 1'
          obs2='P1    '
          goto 10
       endif
       i=index(mapdoc(1),'.c2_')
       if (i.gt.0)then
          obs='Phase 2'
          obs2='P2    '
          goto 10
       endif
       i=index(mapdoc(1),'.c3_')
       if (i.gt.0)then
          obs='Phase 3'
          obs2='P3    '
          goto 10
       endif
       i=index(mapdoc(1),'.c4_')
       if (i.gt.0)then
          obs='Phase 4'
          obs2='P4    '
          goto 10
       endif
       i=index(mapdoc(1),'.c5_')
       if (i.gt.0)then
          obs='Phase 5'
          obs2='P5    '
          goto 10
       endif
       i=index(mapdoc(1),'.c6_')
       if (i.gt.0)then
          obs='Phase 6'
          obs2='P6    '
          goto 10
       endif
       i=index(mapdoc(1),'.c7_')
       if (i.gt.0)then
          obs='Phase 7'
          obs2='P7    '
          goto 10
       endif
       i=index(mapdoc(1),'.p12_')
       if (i.gt.0)then
          obs='Phase 1&2'
          obs2='P12   '
          goto 10
       endif
       i=index(mapdoc(1),'.g12_')
       if (i.gt.0)then
          obs='Phase 1&2'
          obs2='P12   '
          goto 10
       endif
       i=index(mapdoc(1),'.c12_')
       if (i.gt.0)then
          obs='Phase 1&2'
          obs2='P12   '
          goto 10
       endif
       i=index(mapdoc(1),'.p123_')
       if (i.gt.0)then
          obs='Phase 1&2&3'
          obs2='P123  '
          goto 10
       endif
       i=index(mapdoc(1),'.g123_')
       if (i.gt.0)then
          obs='Phase 1&2&3'
          obs2='P123  '
          goto 10
       endif
       i=index(mapdoc(1),'.c123_')
       if (i.gt.0)then
          obs='Phase 1&2&3'
          obs2='P123  '
          goto 10
       endif
       i=index(mapdoc(1),'.p1234_')
       if (i.gt.0)then
          obs='Phase 1&2&3&4'
          obs2='P1234  '
          goto 10
       endif
       i=index(mapdoc(1),'.g1234_')
       if (i.gt.0)then
          obs='Phase 1&2&3&4'
          obs2='P1234 '
          goto 10
       endif
       i=index(mapdoc(1),'.c1234_')
       if (i.gt.0)then
          obs='Phase 1&2&3&4'
          obs2='P1234 '
          goto 10
       endif
       i=index(mapdoc(1),'.p34_')
       if (i.gt.0)then
          obs='Phase 3&4'
          obs2='P34   '
          goto 10
       endif
       i=index(mapdoc(1),'.g34_')
       if (i.gt.0)then
          obs='Phase 3&4'
          obs2='P34   '
          goto 10
       endif
       i=index(mapdoc(1),'.c34_')
       if (i.gt.0)then
          obs='Phase 3&4'
          obs2='P34   '
          goto 10
       endif
       i=index(mapdoc(1),'s.v')
       if (i.eq.0)then
          i=index(mapdoc(1),'s.V')
       endif
       if (i.gt.0)then
          read(mapdoc(1)(i+2:i+7),*,end=5,err=5)vp
          obs=vp(1:6)
          obs2=vp(1:6)
          goto 10
       endif
 5     obs=' NA   '
       obs2='NA    '
 10    continue

       ji = len_trim(CMAPFILE) + 1
       FILEOUT1='cat.'//CMAPFILE(1:ji-1)
       FILEOUT2='cat.'//CMAPFILE(1:ji-1)//'.summary'

       open(unit=73,file=FILEOUT1)
       open(unit=83,file=FILEOUT2)

 15    continue
       write(83,'("Number of test sources:",i4)') NSOURCE
       write(83,'("EGRET LIKELIHOOD POINT-SOURCE ANALYSIS, ",
     &      "Program version ",a)') VER
       write(83,'("Measured energy selection: ",f7.1," to ",
     &      f7.1)') CTLEMN,CTLEMX
       write(83,'("Analysis of : ",a)') obs
       write(83,'("Counts map: ",a)') cmapfile(1:50)
       write(83,'("Diffuse map: ",a)') gmapfile(1:50)
       write(83,*)
       WRITE(83,'
     &      ("Analysis parameters: Ranal=",f4.1)')Ranal
       WRITE(83,'(
     &      "The PSF background model contains all sources in this",
     &      " catalog except ")')
       write(83,'("for the source being analyzed.")')
       write(83,'(
     &      "The flux for sources with TS > 4 is presented ",
     &      "as a detection.",/,
     &      "The flux for sources with TS < 4 ",
     &      "is presented as an upper limit.")')
       write(83,'(
     &      "The upper limits are at",f7.1,"% confidence ",
     *      "corresponding to a",/,
     &      "drop of lnL of",f7.2," from the value at the counts ",
     *      "estimate",
     &      " (unless",/,"the unconstrained counts ",
     &      "estimate is < 0; then the upper limit is",/,"assumed to ",
     *      "be the ",
     &      "difference between the statistical limit",
     &      " and the",/,"counts estimate.")')
     &      conf_precent,delta_lnL
       write(83,'(
     &      "The fluxes are in units 10^-8 cm^-2 s^-1. ",
     &      "The approximate")')
       write(83,'(
     &      "significance of a detection in units of sigma is ",
     *      "sqrt(TS).")')
       write(83,'(
     &      "The exposure (EXP) is given in units of 10^7 cm^2s.")')
       write(83,'(
     &      "The measured and true positions are given in degrees.")')
       write(83,'(
     &      "The difference is given in arcminutes.")')
       write(83,'(
     &      "The 95% confidence error region is fit with an equal ",
     &      "circle"$)')
       write(83,'(
     &      " area circle")')
c
c
c
       write(73,'("Name             RA    Dec     l      b    ",
     &      " R     F     DF  Cnts sqrtTS  VP")')
       write(73,'("11111111111111:222.22:333.33:444.44:555.55:",
     &      "6.66:7777.7:88.8:99999:AAA.A:BBBBBB")')
       
       write(83,'("Name            Best Choice  Flags 1   2   4   ",
     &      "5   7   8   9   10")')
       write(83,*)' '
c
       DO J=1,NSOURCE
          srcN=SRC_NAMES(J)
          if ((srcN(1:3).eq.'GRO'.or.srcN(1:3).eq.'   '.or.
     &         srcN(1:3).eq.'.  ').and.sv_dstsv(2,J).eqv..true.)then
             srcL=best_x(j)
             srcB=best_y(j)
          else
             srcL=SRC_PARMS(J,1)
             srcB=SRC_PARMS(J,2)
          endif
c
          CALL GETSRCNM

	  if (coord_sys.eq.'C')then
             ra=srcL
             dec=srcB
             Call CELGALD('CG',ra,dec,estL,estB,iiret)
	  else
             estL=srcL
             estB=srcB
             Call CELGALD('GC',ra,dec,estL,estB,iiret)
	  endif

          if (ra.lt.0)ra=ra+360
          if (estL.lt.0)estL=estL+360

          gamma = SRC_PARMS(J,4)
          TS=svn_TS(J)

          if (TS.ge.4)then
             iicnt=SRC_PARMS(J,3)+0.5
             write(73,'(A14,":",f6.2,":",f6.2,":",f6.2,":",f6.2,
     &            ":",f4.2,":",f6.1,":",f4.1,":",I5,":",f5.1,":",A6)')
     &            srcN,ra,dec,estL,estB,sv_err95(J)/60.,sv_flx(1,J),
     &            sv_flx(2,J),iicnt,sqrt(TS),obs2(1:6)
          else
             cnts_ul=sv_upperlim(J)*sv_expose(J)/1.e8
             iicnt=cnts_ul+0.5
             write(73,'(A14,":",f6.2,":",f6.2,":",f6.2,":",f6.2,
     &            ":",4x,":",f6.1,":",4x,":",I5,":",f5.1,":",A6)')
     &            srcN,ra,dec,estL,estB,
     &            sv_upperlim(J),iicnt,sqrt(TS),obs2(1:6)
c     
          endif

          bestch(1:3)='TS '

          if (sv_dstsv(4,J))bestch(1:3)='95%'
          if (.not.sv_dstsv(4,J).and.sv_dstsv(5,J))bestch(1:3)='68%'

          write(83,'(a,6x,a,10x,8l2)') srcN(1:14),bestch(1:3),
     &         sv_dstsv(1,J),sv_dstsv(2,J),sv_dstsv(4,J),sv_dstsv(5,J), 
     &         sv_dstsv(7,J),sv_dstsv(8,J),sv_dstsv(9,J),sv_dstsv(10,J)
 29       continue
       enddo

 30    continue
       close(unit=73)
       close(unit=83)
       catalog=catalog_sv

       RETURN

 1111  write(lu(1),*)'Invalid input, try again.'
       catalog=catalog_sv

       return
       END
C
