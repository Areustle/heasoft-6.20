C       SUBROUTINE CATPROC(first_time,catalog1)
c
C
C  $Id: catproc.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++     effect: A position/flux catalog entry is generated.
C++	IF(first_time) then initialize only.
C++	IF(catalog) then do catalog instead of a single source.
C***********************************************************************
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     LOGICAL first_time  true => initialize only
c
c     logical catalog     true => do catalog instead of a single source
c
c-----------------------------------------------------------------------
c	definitions of key parameters:
c	catL catB, catalog position in galactic coordinates
c	estL estB, ra dec, estimated position in gal. and cel. coordinates
c
c
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C Installation of RCS lines: 23 AUG 1995 by JAE
c
c
C=======================================================================
C  $Log: catproc.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:08  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:28  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:46:53  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:05  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE CATPROC(first_time,catalog1)
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
      CHARACTER input*200,break*110,oldnm*18,obs*10,obs2*10,medium*24
      CHARACTER notes*10,loc_SIGMSG*72,loc_SIGNAL*1,analysis_mode*1
      CHARACTER flux_SIGMSG*72,flux_SIGNAL*1
      LOGICAL first_time,catalog1,pos_anal,disjoint,ident,
     &     catalog_sv
      REAL MAPVAL


      id = '$Id: catproc.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      catalog_sv=catalog
      catalog=catalog1
      if (first_time) then
c if 1
c     do initialization
         LOC='CATPROC'
         break=
     &'------------------------------------------------------'//
     &'------------------------------------------------------'
         report=.false.
c     set obs phrase
         i=index(mapdoc(1),'_p1')
         if (i.gt.10) then
	    obs='Phase 1'
	    obs2='  P1 '
	    goto 10
         endif
         i=index(mapdoc(1),'_p2')
         if (i.gt.10) then
	    obs='Phase 2'
	    obs2='  P2 '
	    goto 10
         endif
         i=index(mapdoc(1),'_p3')
         if (i.gt.10) then
	    obs='Phase 3'
	    obs2='  P3 '
	    goto 10
         endif
         i=index(mapdoc(1),'_p4')
         if (i.gt.10) then
	    obs='Phase 4'
	    obs2='  P4 '
	    goto 10
         endif
         i=index(mapdoc(1),'p12')
         if (i.gt.10) then
	    obs='Phase 1&2'
	    obs2='  P12'
	    goto 10
         endif
         i=index(mapdoc(1),'g12')
         if (i.gt.10) then
	    obs='Phase 1&2'
	    obs2='  P12'
	    goto 10
         endif
         i=index(mapdoc(1),'c12')
         if (i.gt.10) then
	    obs='Phase 1&2'
	    obs2='  P12'
	    goto 10
         endif
         i=index(mapdoc(1),'.vp')
         if (i.gt.10) then
	    read(mapdoc(1)(i+3:i+6),*,end=5,err=5)vp
	    write(obs,'("vp ",f5.1,2x)')vp/10.
	    write(obs2,'(f5.1)')vp/10.
	    goto 10
         endif
 5       write(6,'(
     &        "Which observation is this (A10)?",$)')
         READ(LU(12),'(A)') obs
	 jjlen0=index(obs,'1&2')
	 if (jjlen0.eq.0)jjlen0=index(obs,'1+2')
	 if (jjlen0.ne.0) then
            obs=' P12'
            goto 20
	 endif
	 jjlen0=index(obs,'VP')
	 if (jjlen0.eq.0)jjlen0=index(obs,'vp')
	 if (jjlen0.ne.0) then
            read(obs(jjlen0+2:10),*)vp
            write(obs2,'(f5.1)')vp/10.
            goto 20
         endif
         jjlen0=index(obs,'1')
         if (jjlen0.ne.0) then
            obs2='  P1 '
            goto 20
         endif
         jjlen0=index(obs,'2')
         if (jjlen0.ne.0) then
            obs2='  P2 '
            goto 20
         endif
         jjlen0=index(obs,'3')
         if (jjlen0.ne.0) then
            obs2='  P3 '
            goto 20
         endif
         jjlen0=index(obs,'4')
         if (jjlen0.ne.0) then
            obs2='  P4 '
            goto 20
         endif
         obs='  NA '
 20      continue
 10      continue

 15      analysis_mode='N'  
         write(6,'(
     &        "Select location analysis mode (N,I,F,V, or ?; cr for ",
     &        A1,"):",$)')analysis_mode
         read(LU(12),'(A)')input
c     if 2
         if (input.ne.' ') then
	    analysis_mode=input(1:1)
c     if 3
	    if (analysis_mode.eq.'N'.or.analysis_mode.eq.'n') then
               analysis_mode='N'
               pos_anal=.false.
            elseif (analysis_mode.eq.'F'.or.analysis_mode.eq.'f') then
               analysis_mode='F'
               pos_anal=.true.
               medium='F'
            elseif (analysis_mode.eq.'I'.or.analysis_mode.eq.'i') then
               pos_anal=.true.
               analysis_mode='I'
               medium='I'
            elseif (analysis_mode.eq.'V'.or.analysis_mode.eq.'v') then
               analysis_mode='V'
               pos_anal=.true.
               write(6,'("graphics device? (return for ",A10,"):",$)')
     &              g_device
               read(LU(12),'(A)')input
               if (input.ne.' ')g_device=input
               medium=g_device
            else
               write(6,'(
     &              "Supported analysis modes are",/
     &              "    N(o) position analysis,",
     &              " but analyze flux at given positions;",/,
     &              "    I(gnore) plot, but do position analysis;",/,
     &              "    F(ile) plot to disk as srcN.ps postscript ",
     &              "file;",/,
     &              "    V(iew) plot on screen")')
               goto 15
            endif
         endif

         if (.not.catalog) then
            iout=6
	 else
            iout=18
c     
c     write catalog header for jrm and djt output
c     
c     jrm header first
c
            write(iout,'("EGRET LIKELIHOOD POINT-SOURCE ANALYSIS, ",
     &           "Program version ",a)') VER
            write(iout,'("Measured energy selection: ",f7.1," to ",
     &           f7.1)') CTLEMN,CTLEMX
            write(iout,'("Analysis of : ",a)') obs
            write(iout,'("Counts map: ",a)') cmapfile(1:50)
            write(iout,'("Diffuse map: ",a)') gmapfile(1:50)
            WRITE(iout,'
     &           ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &           gamma,Ranal
            WRITE(iout,'(
     &           "The PSF background model contains all sources in ",
     *           "this catalog except for the source being analyzed.")')
            write(iout,'(
     &           "The flux for sources with TS>",f7.2," is presented ",
     &           "as a detection only.",/,
     &           "The flux for sources with",f7.2," <TS<",f7.2,
     &           " is presented",
     &           " both as a detection",/,
     &           "and as an upper limit. The flux for sources ",
     &           "with TS<",f7.2,/,
     &           "is presented as an upper limit only.")')
     &           TS_max,TS_min,TS_max,TS_min
            write(iout,'(
     &           "The upper limits are at",f7.1,"% confidence ",
     &           "corresponding to a",/,
     &           "drop of lnL of",f7.2," from the value at the ",
     &           "counts estimate",
     &           " (unless the unconstrained counts",/,
     &           "estimate is < 0; then the upper limit is assumed to ",
     &           "be the ",
     &           "difference between the statistical limit",
     &           " and the counts estimate.")')
     &           conf_precent,delta_lnL
            write(iout,'(
     &           "The fluxes are in units 10^-8 cm^-2 s^-1.",
     &           " The approximate")')
            write(iout,'(
     &           "significance of a detection in units of ",
     &           "sigma is sqrt(TS).")')
            write(iout,'(
     &           "The exposure (EXP) is given in units of "
     &           "10^7 cm^2s.")')

            if (pos_anal) then
               write(iout,'(
     &              "The measured and true positions are given ",
     &              "in degrees.")')
               write(iout,'(
     &              "The difference is given in arcminutes.")')
               write(iout,'(
     &              "The 95% confidence error region is fit with ",
     &              "an ellipse which")')
               write(iout,'(
     &              "is characterized by the half length of the ",
     &              "major and minor")')
               write(iout,'(
     &              "axes (a and b, given in arcminutes), and the ",
     &              "position angle")')
               write(iout,'(
     &              "(eastward from north on the meridian of the ",
     &              "major axis,")')
               write(iout,'(
     &              "given in degrees). RMS is the deviation in ",
     &              "arcminutes from")')
               write(iout,'(
     &              "the elliptical fit. Fit notes:")')
               write(iout,'(
     &              "(a) means that 0.1<RMS/sqrt(a*b)<0.3",  
     &              " - the fit is not a good representation of the ",
     &              "error region.")')
               write(iout,'(
     &              "(b) means that 0.3<RMS/sqrt(a*b)",  
     &              " - the fit is a horrible representation of the ",
     &              "error region.")')
               write(iout,'(
     &              "(c) means elliptical fit failed - ",
     *              "characteristic circle given.",/,
     &              "(d) error region is disjoint.",/,
     &              "(i) means ",
     &              "identification is questionable because of ",
     &              "measured position.")')
            endif
c	  
c     djt header below here
c
            if (publish) then
               write(73,'("EGRET LIKELIHOOD POINT-SOURCE ANALYSIS, ",
     &              "Program version ",a)') VER
               write(73,'("Measured energy selection: ",
     &              f7.1," to ",f7.1)')
     &              CTLEMN,CTLEMX
               write(73,'("Analysis of : ",a)') obs
               write(73,'("Counts map: ",a)') cmapfile(1:50)
               write(73,'("Diffuse map: ",a)') gmapfile(1:50)
               WRITE(73,'(
     &              "Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &              gamma,Ranal
               WRITE(73,'(
     &              "The PSF background model contains all sources ",
     &              "in this catalog",
     &              " except for the source being analyzed.")')
               write(73,'(
     &              "The flux for sources with TS > 9 is presented ",
     &              "as a detection only.",/,
     &              "The flux for sources with TS < 9 ",
     &              "is presented as an upper limit only.")')
               write(73,'(
     &              "The upper limits are at",f7.1,"% ",
     &              "confidence corresponding to a",/,
     &              "drop of lnL of",f7.2," from the value at the ",
     &              "counts estimate",
     &              " (unless the unconstrained counts",/,
     &              "estimate is < 0; then the upper limit is ",
     *              "assumed to be the ",
     &              "difference between the statistical limit",
     &              " and the counts estimate.")')
     &              conf_precent,delta_lnL
               write(73,'(
     &              "The fluxes are in units 10^-8 cm^-2 s^-1. ",
     &              "The approximate")')
               write(73,'(
     &              "significance of a detection in units of sigma ",
     &              "is sqrt(TS).")')
               write(73,'(
     &              "The exposure (EXP) is given in units of ",
     &              "10^7 cm^2s.")')
               if (pos_anal) then
                  write(73,'(
     &                 "The measured and true positions are ",
     &                 "given in degrees.")')
                  write(73,'(
     &                 "The difference is given in arcminutes.")')
                  write(73,'(
     &                 "The 95% confidence error region is fit with ",
     &                 "an ellipse which")')
                  write(73,'(
     &                 "is characterized by the half length of the ",
     &                 "major and minor")')
                  write(73,'(
     &                 "axes (a and b, given in arcminutes), and ",
     &                 "the position angle")')
                  write(73,'(
     &                 "(eastward from north on the meridian of ",
     &                 "the major axis,")')
                  write(73,'(
     &                 "given in degrees). RMS is the deviation in ",
     &                 "arcminutes from")')
                  write(73,'(
     &                 "the elliptical fit. Fit notes:")')
                  write(73,'(
     &                 "(a) means that 0.1<RMS/sqrt(a*b)<0.3",  
     &                 " - the fit is not a good representation of ",
     &                 "the error region.")')
                  write(73,'(
     &                 "(b) means that 0.3<RMS/sqrt(a*b)",  
     &                 " - the fit is a horrible representation of ",
     &                 "the error region.")')
                  write(73,'(
     &                 "(c) means elliptical fit failed - ",
     &                 "characteristic circle given.",/,
     &                 "(d) error region is disjoint.",/,
     &                 "(i) means ",
     &                 "identification is questionable because of ",
     &                 "measured position.")')
c
c     endif 1,2 3
c
               endif            !pos analysis fit header
            endif               ! publish test
         endif                  !catalog header

         if (pos_anal) then
            Iconf=95
            deltaTS=6.          !95% confidence - chisqd_2 distribution
            write(6,'("Enter percentage confidence for source location",
     &           " region",/,"(tabulated values: 50, 68, 95, 99; cr ",
     *           "for "
     &           ,I2,"%):",$)')Iconf
            read(LU(12),'(A)')input

            if (input.ne.' ') read(input,*,end=1111)Iconf
            if (Iconf.eq.50) then
               deltaTS=1.4
            elseif (Iconf.eq.68) then
               deltaTS=2.3
            elseif (Iconf.eq.95) then
               deltaTS=6.
            elseif (Iconf.eq.99) then
               deltaTS=9.1
            else
               write(6,'("You have entered a non-tabulated",
     &              " value -",/,"enter corresponding drop in TS ",
     &              " to this confidence:",$)')
               read(LU(12),'(A)')input
               read(input,*,end=1111)deltaTS
            endif
	 endif
c
c     return if first_time == .true.
c     
	 catalog=catalog_sv
	 return 
c
c     end initialization
c
      endif

      if (.not.catalog.and..not.publish) then
         call pixel_select(.true.,'Initial position    ')
         if (signal.ne.' ') then
            catalog=catalog_sv
            return
         endif
         call L_CHECK(srcL)
         if (signal.ne.' ') then
            catalog=catalog_sv
            return
         endif
      endif

      if (pos_anal) then
c     do position analysis
         if (catalog.or.publish) then
	    call ERRMAP(.false.,Iconf,deltaTS,
     &           a,b,pa,RMS,estL,estB,medium,ident,disjoint)
         else
	    call ERRMAP(.true.,Iconf,deltaTS,
     &           a,b,pa,RMS,estL,estB,medium,ident,disjoint)
         endif
c     check for position analysis error
         if (signal.ne.' '.or.disjoint) then
	    loc_SIGMSG=SIGMSG
	    loc_SIGNAL=SIGNAL
	    sv_sigsv(jpos(NSOURCE+1))=SIGNAL
	    call error(0,loc)
         else
	    loc_SIGNAL=' '
	    if (Iconf.eq.68) then
               sv_tmp_68_x(jpos(NSOURCE+1))=estL
               sv_tmp_68_y(jpos(NSOURCE+1))=estB
               sv_err68(jpos(NSOURCE+1))=sqrt(a*b)
               src_parms(jpos(NSOURCE+1),5)=
     &              src_parms(jpos(NSOURCE+1),5)+100
               best_choice(jpos(NSOURCE+1))=' ? '
               sv_dstsv(2,jpos(NSOURCE+1))=.true.
	    elseif (Iconf.eq.95) then
               sv_tmp_95_x(jpos(NSOURCE+1))=estL
               sv_tmp_95_y(jpos(NSOURCE+1))=estB
               sv_err95(jpos(NSOURCE+1))=sqrt(a*b)
               src_parms(jpos(NSOURCE+1),5)=
     &              src_parms(jpos(NSOURCE+1),5)+1000
               sv_dstsv(2,jpos(NSOURCE+1))=.true.
               best_choice(jpos(NSOURCE+1))=' ? '
	    endif
         endif
      else
c     no position analysis - set estL,estB,ra,dec
c     
         if (coord_sys.eq.'C') then
c     will need the catalog position in galactic coordinates
	    tempra=srcL*PI180
	    tempdec=srcB*PI180
	    estL=srcL
	    estB=srcB
	    call CELGAL('CG',tempra,tempdec,catL,catB,IRET)
	    catL=catL/PI180
	    catB=catB/PI180
c     will need the measured position in galactic coordinates
	    ra=estL*PI180
	    dec=estB*PI180
	    call CELGAL('CG',ra,dec,estL,estB,IRET)
	    estL=estL/PI180
	    estB=estB/PI180
	    ra=ra/PI180
	    dec=dec/PI180
         else                   !map in galactic coordinates
	    catL=srcL
	    catB=srcB
	    estL=srcL
	    estB=srcB
c     will need the measured position in celestial coordinates
	    templ=estL*PI180
	    tempb=estB*PI180
	    call CELGAL('GC',ra,dec,templ,tempb,IRET)
	    ra=ra/PI180
	    dec=dec/PI180
         endif
c
      endif

c     do flux analysis
      calc_uncert=.true.
      report=.false.
      report2=.true.

      call srctest(.false.)

c     check for flux analysis error
      if (signal.ne.' ') then
         flux_SIGMSG=SIGMSG
         flux_SIGNAL=SIGNAL
         call error(0,loc)
      else
         flux_SIGNAL=' '
      endif

c     present the results
      write(iout,*)break
      if (srcL.lt.0.)srcL=srcL+360.
      if (pos_anal) then
	 if (loc_SIGNAL.ne.' ') then
            if (coord_sys.eq.'C') then
               WRITE(iout,
     &              '(A18,11x," Location error! Assuming position:"
     &              ,14x,2f7.2," ",A72)')
     &              srcN,srcL,srcB,loc_SIGMSG
            else                !galactic coordinates
               WRITE(iout,
     &              '(A18,11x," Location error! Assuming position:",
     &              2f7.2," ",A72)')
     &              srcN,srcL,srcB,loc_SIGMSG
            endif
	 else
            IRET=0
            if (coord_sys.eq.'C') then
c     will need the catalog position in galactic coordinates
               tempra=srcL*PI180
               tempdec=srcB*PI180
               call CELGAL('CG',tempra,tempdec,catL,catB,IRET)
               catL=catL/PI180
               catB=catB/PI180
c     will need the measured position in galactic coordinates
               ra=estL*PI180
               dec=estB*PI180
               call CELGAL('CG',ra,dec,estL,estB,IRET)
               estL=estL/PI180
               estB=estB/PI180
               ra=ra/PI180
               dec=dec/PI180
            else                !map in galactic coordinates
               catL=srcL
               catB=srcB
c     will need the measured position in celestial coordinates
               templ=estL*PI180
               tempb=estB*PI180
               call CELGAL('GC',ra,dec,templ,tempb,IRET)
               ra=ra/PI180
               dec=dec/PI180
            endif
            if (IRET.ne.0) then
               sigmsg='CATPROC:CELGAL error'
               signal='C'
               catalog=catalog_sv
               RETURN
            endif
            if (estL.lt.0.) estL=estL+360.
            if (catL.lt.0.) catL=catL+360.
            if (ra.lt.0.) ra=ra+360.
            f=60./PI180
            q=RMS/sqrt(a*b)
            notes=' '
            ntpt=1
            if (q.gt.0.1) then
               notes ='a'
               if (q.gt.0.3) then
                  notes ='b'
               endif
               ntpt=ntpt+1
            endif
            if (pa.eq.0..and.a.eq.b) then
               notes(ntpt:ntpt)='c'
               ntpt=ntpt+1
            endif
            if (disjoint) then
               notes(ntpt:ntpt)='d'
               ntpt=ntpt+1
            endif
            if (ident) then
               notes(ntpt:ntpt)='i'
               ntpt=ntpt+1
            endif
            WRITE(iout,'(50x,"true position",7x,"measured position",
     &           14x,"elliptical fit to position region")')
            if (notes.eq.' ') then
               WRITE(iout,'(
     &              "Name              Identification     ",
     &              "Observation   L      B   ",
     &              "     L      B      RA     DEC      diff",
     &              "  a     b     phi    RMS")')
	    else
               WRITE(iout,'(
     &              "Name              Identification     ",
     &              "Observation   L      B   ",
     &              "     L      B      RA     DEC      diff",
     &              "  a     b     phi    RMS    notes")')
	    endif
	    if (srcN(1:3).eq.'GRO'.or.srcN(1:3).eq.'2CG') then
c     unidentified source
               WRITE(iout,'(A18,19x,A10,15x,2x,4f7.2,8x,2f6.1,f7.1,f7.2,
     &              3x,A10)')
     &              srcN,obs,estL,estB,ra,dec
     &              ,a*f,b*f,pa/PI180,RMS*f,notes    
            else
c     identified source
               oldnm=srcN
               if (coord_sys.eq.'C') then
                  call iauname(ra,dec)
               else
                  call iauname(estL,estB)
               endif
               diff=gtcirc(catL,catB,estL,estB)
               WRITE(iout,'(2A18,
     &              1x,A10,1x,2f7.2,2x,4f7.2,2x,f6.1,2f6.1,f7.1,f7.2,
     &              3x,A10)')
     &              srcN,oldnm,obs,catL,catB,estL,estB,ra,dec,diff*60.
     &              ,a*f,b*f,pa/PI180,RMS*f,notes    
               srcN=oldnm
            endif
         endif                  !(pos_anal OK)clause
      else                      !pos_anal not requested
         if (coord_sys.eq.'C') then
            WRITE(iout,
     &           '(A18,8x,"Assumed position for flux analysis is ",
     &           14x,2f7.2)')
     &           srcN,srcL,srcB
         else                   !galactic coordinates
            WRITE(iout,
     &           '(A18,8x,"Assumed position for flux analysis is ",
     &           2f7.2)')
     &           srcN,srcL,srcB
         endif
      endif                     !(pos_anal)clause

      if (flux_SIGNAL.ne.' ') then
         WRITE(iout,'("Flux error: ",A72," for ",A12)')
     &        flux_SIGMSG,srcN(1:12)
      else
         WRITE(iout,'("        Observation  spec.index  sqrt(TS)",
     &        "  Flux +/- 1 sigma  (U.L.)",
     &        "      Cnts   +/- 1 sigma  (U.L.)     EXP",
     &        "   Gmult  Gbias")')
         expose=mapval(emap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) / (
     &        (sin(PI180*(srcB+ctlscl/2.))-sin(PI180*(srcB-ctlscl/2.)))
     &        *ctlscl*PI180)
         
         if (TS.gt.TS_max) then
c     This is a strong detection.
            WRITE(iout,'(8x,A10,3x,"???+/-???",
     &           f7.1,f11.3,f9.3,11x
     &           ,2f11.2,10x,f8.2,2f7.3)')
     &           obs,sqrt(TS),Flux,1.e8*dCounts/expose,
     &           Counts,dCounts,expose/1.E7,Gmult,Gbias
         elseif (TS.le.TS_max.and.TS.gt.TS_min) then
c     This is a weak detection.
            WRITE(iout,'(8x,A10,3x," ??+/-?? ",
     &           f7.1,f11.3,f9.3,"  <",f8.3,
     &           2f11.2,"  <",f7.2,f8.2,2f7.3)')
     &           obs,sqrt(TS),Flux,1.e8*dCounts/expose,
     &           1.e8*Counts_limit/expose,
     &           Counts,dCounts,Counts_limit,expose/1.E7,Gmult,Gbias
         else
c     This is not detected.
            WRITE(iout,'(8x,A10,12x,
     &           f7.1,20x,"  <",f8.3,
     &           22x,"  <",f7.2,f8.2,2f7.3)')
     &           obs,sqrt(TS),
     &           1.e8*Counts_limit/expose,
     &           Counts_limit,expose/1.E7,Gmult,Gbias
         endif
c
c     2nd catalog output for djt
	 if (.not.publish)goto 30
c     
	 CALL GETSRCNM
	 if (pos_anal) then
            if (coord_sys.eq.'C') then
               ra=srcL
               dec=srcB
               Call CELGALD('CG',estL,estB,ra,dec,iiret)
            else
               estL=srcL
               estB=srcB
               Call CELGALD('GC',estL,estB,ra,dec,iiret)
            endif
	 endif
	 if (ra.lt.0)ra=ra+360
	 if (estL.lt.0)estL=estL+360
	 if (TS.ge.9) then
            iicnt=Counts+0.5
            write(73,'(A14,2x,f6.2,f7.2,f8.2,f7.2,20x,f8.1,f6.1,
     &           10x,I7,f7.1,2x,A5,40x)')srcN,ra,dec,estL,estB,
     &           Flux,1.e8*dCounts/expose,iicnt,sqrt(TS),obs2(1:5)
c     
	 else
c     
            iicnt=Counts_limit+0.5
            write(73,'(A14,2x,f6.2,f7.2,f8.2,f7.2,21x,"<",f6.1,6x,
     &           11x,"<",I5,f7.1,2x,A5,40x)')srcN,ra,dec,estL,estB,
     &           1.e8*Counts_limit/expose,iicnt,sqrt(TS),obs2(1:5)
c     
            srcN=oldnm
	 endif
 30      continue
      endif                     !(flux_anal OK)clause
      catalog=catalog_sv
      RETURN
 1111 write(lu(1),*)'Invalid input, try again.'
      catalog=catalog_sv
      return
      END
C
