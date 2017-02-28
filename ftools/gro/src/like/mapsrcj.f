C       SUBROUTINE MAPSRCJ(flag)
C
C
C  $Id: mapsrcj.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++            effect: the srctest function is applied to MAP
C++                    over the region of interest to estimate the
C++                    point-source likelihood and strength as a
C++                    function of position. A map of the TS and 
C++	               background parameters are written.
C
C=======================================================================
C  LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C
C=======================================================================
C  $Log: mapsrcj.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/12/26 17:33:18  irby
C  - Change negative exponents to e.g. -1*number instead of -number for f90
C    compatibility (and put in parens (-1) where necessary.
C
C  - Fix write statements (commas).
C
C  Revision 1.1  2002/04/16 20:27:40  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.7  1996/05/09  14:16:35  jae
c added lines to set pointing direction if NOT previously set
c in jloc_pos(.f).  This will repair the error (empty MS maps)
c reported by rch and muk.
c
c Revision 5.6  1996/05/08  22:42:07  jae
c fixed errors inrevision 5.5 .  I used revision 5.4 to start
c
c Revision 5.4  1996/05/08  20:43:06  jae
c Repaired minor typo on line 240
c
c Revision 5.3  1996/05/08  19:37:48  jae
c added line for output if jae_exp_anal=TRUE
c
c Revision 5.2  1996/05/08  18:20:26  jae
c No changes performed
c
c Revision 5.1  1996/02/29  20:52:09  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:22  jae
c Subroutine Module for like V5.00
c
C%   Changes:
c
c	JAE	09-18-94
c
c	Added 'speedmap' option to shorten the time required for
c	this routine (mainly for large ROIs).  The routine will
c	only calculate gmult and gbias every nth bin (in latitude)
c	where n is determined from a user input scale length (angle)
c	divided by the pixel width. (e.g.: 5 deg scale and 0.5 deg
c	pixel implies every 10th bin in latitude).  Gbias and Gmult
c	are held fixed between these bins and only counts is optimized.
c
c	A second method is also used if TS is very small ~0.  In this
c	case nearby bins are set to very small nonzero values.  Only
c	bins containing values < 2e-10 are tested (the normal contents
c	of a reset array is zero for all bins).
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPSRCJ(flag)

C  Common blocks used:
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/nmprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/locpos.copy'
      INCLUDE  '../COMMON/fitrep.copy'

      save

      character(80) id
      common /id/id
      REAL MAPVAL               ! map value function
      character input*100,moreinput*100,jblank1*20,termtype*20
      LOGICAL determined,FEXIST,mapcnts,speedmap,flag,scndpass
      real ggm(2,2),ggb(2,2)
      integer IRRL(2,2),IRRB(2,2)
      logical Res_Gm_sv,Res_Gb_sv,rpt_sv,rpt2_sv,calc_u_sv
      logical isxterm

      id = '$Id: mapsrcj.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
      LOC='MAPSRCJ '


      if (jae_mapsrcj) print *,' Inside:',LOC

      isxterm=.false.
      call getenv('TERM',termtype)

      if (termtype(1:5).eq.'xterm') isxterm=.true.
c     
      if (coord_sys.eq.'G'.and..not.JTRUE) then
         SC_LJJ = SC_LII
         SC_BJJ = SC_BII
         tmpL=SC_LII
         tmpB=SC_BII
         CALL CELGALD('GC',SC_RAJ,SC_DECJ,tmpL,tmpB,iiret)
      elseif (.not.JTRUE) then
         SC_RAJ = SC_RA
         SC_DECJ = SC_DEC
         tmpL = SC_RA
         tmpB = SC_DEC
         CALL CELGALD('CG',tmpL,tmpB,SC_LJJ,SC_BJJ,iiret)
      endif
c
      jblank1='                    '

ccc JAE addition output request for speedmap
c     SPEEDMAP variable = T then request scalesize (deg) for refitting
c     gmult and gbias.  The values of gmult and gbias will be fit each
c     time MOD(IsrcL - ROIPRG(1),INT(scalesize/ctlscl))=0
c
c     e.g.: for scalesize=2 deg and ctlscl = 0.5 then
c     if(MOD(IsrcL-ROIPRG(1),4).eq.0)then
c     call allfit
c     else
c     call HOOD
c     endif

      rpt_sv=report
      itpj=1
      Gmult_sv=Gmult
      Gbias_sv=Gbias
      rpt2_sv=report2
      calc_u_sv=calc_uncert
      report=.false.
      report2=.false.
      calc_uncert=.false.
      Res_Gm_sv=Restrict_Gmult
      Res_Gb_sv=Restrict_Gbias
      scndpass=.false.

      if (.not.flag) goto 21

      mapcnts=.false.
      speedmap=.false.
      ntmpc=1

      if (tttflg2) goto 7

      if (Restrict_Gmult.and.Restrict_Gbias) goto 7

      write(*,'("Fast Mapping ? (default=N): Y or N :"$)')
      read(LU(12),'(a)') input
      numcar = index(input, ' ') - 1

      if (numcar.gt.0.and.(input(1:1).eq.'y'.or.input(1:1).eq.
     &     'Y')) then
 4       speedmap=.true.
         tmpc=2
         itpj=2
         input='                    '
         write(*,'("Set scale size(deg) [default = 2]: "$)')
         read(LU(12),'(a)') input
         numcar = index(input, ' ') - 1

         if (numcar.eq.0) goto 6

         read(input,*,err=5,end=5)tmpc

         if (tmpc.gt.20.001.or.tmpc.lt.ctlscl) goto 5

         goto 6
 5       print *,'invalid input: scale size: ',tmpc

         if (tmpc.gt.20.001) print *,' scale size too big (<= 20)'
         if (tmpc.lt.ctlscl) then
	    print *,'Scale size too low. Must be >=',ctlscl
         endif

         print *,' '
         print *,'Try again or enter <cr> to accept default'
         goto 4
 6       ntmpc=tmpc/ctlscl
      endif
c
 7    continue

      if (tttflg.and.flag) goto 8

      print *,
     &     'TS will be written to LMAPFILE ',
     &     'Gmult to NEWMAPFILE, and Gbias to ',
     &     'GBIASFILE'

      INQUIRE (FILE=LMAPFILE,EXIST=FEXIST)
      IF (FEXIST) THEN
         WRITE(lu(1),'(
     &        "Output FITS file, LMAPFILE exists.",
     &        " Do you want to remove it?")')
         input='/bin/rm -i '//LMAPFILE
         call system(input)
      ENDIF

      INQUIRE (FILE=GBIASFILE,EXIST=FEXIST)
      IF (FEXIST) THEN
         WRITE(lu(1),'(
     &        "Output FITS file, GBIASFILE exists.",
     &        " Do you want to remove it?")')
         input='/bin/rm -i '//GBIASFILE
         call system(input)
      ENDIF

      INQUIRE (FILE=NEWMAPFILE,EXIST=FEXIST)
      IF (FEXIST) THEN
         WRITE(lu(1),'(
     &        "Output FITS file, NEWMAPFILE (for Gmult output) exists.",
     &        " Do you want to remove it?")')
         input='/bin/rm -i '//NEWMAPFILE
         call system(input)
      ENDIF
      
 8    report=.false.
      report2=.false.
      calc_uncert=.false.

      if (.not.tttflg2) then
         write(lu(1),'("Will map point source TS for each pixel in",
     &        " the ROI, which is:")')
      else
         write(lu(1),'("Will map Gmult and Gbias for the ROI",
     &        " which is:")')
      endif

      if (coord_sys.eq.'C') then
         print *,'RA.',ROIORG(1),' to',ROIEND(1),
     &	      '; Dec.',ROIORG(2),' to',ROIEND(2)
      else
         print *,'Long.',ROIORG(1),' to',ROIEND(1),
     &	      '; Lat.',ROIORG(2),' to',ROIEND(2)
      endif

      write(6,'("Counts map: ",a)') cmapfile(1:58)
      write(6,'("Diffuse map: ",a)') gmapfile(1:57)
      WRITE(6,'
     &     ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &     gamma,Ranal
      write(6,*)
     &     'These PSFs are in the OTHER PSF map:'

      if (NSOURCE.gt.0) then
         write(6,'("number   POSITION              CNTS       ",
     &        "Spec. Index")')
         do nsrc=1,NSOURCE
	    write(6,'(
     &           i4,2f8.2,f20.6,f6.2)')
     &           nsrc,(SRC_PARMS(nsrc,ip),ip=1,4)
         enddo
      else
         write(6,*)
     &        'None.'
      endif

      write(6,*)
     &     'Strong sources should be modeled.'

      if (.not.jmapflg) call intent(determined)
      if (.not.determined.and..not.jmapflg) then
         report=rpt_sv
         report2=rpt2_sv
         calc_uncert=calc_u_sv
         return
      endif

      if (Ranal.gt.20..and..not.jmapflg) then
         write(lu(1),'("The value of Ranal will cause this job",
     &        " to take a long time.")')
         print *,'Ranal: ',Ranal
         call intent(determined)

         if (.not.determined) then
	    report=rpt_sv
	    report2=rpt2_sv
	    calc_uncert=calc_u_sv
	    return
         endif
      endif
c
 1    write(6,'(
     &     "Enter minimum exposure (in units of 10^7 cm^2s) ")')
      write(6,'(
     &     "required to analyze a source (default=1)")')
      write(6,'(
     &     "or enter F for 5% of exposure map maximum: ",$)')
      READ(LU(12),'(A)') input

      if (input.eq.' ') then
         exp_min=1.
      elseif (input.eq.'F'.or.input.eq.'f') then
         CALL MAPMAX(EMAP,IsrcL,IsrcB,exp_min,CTLMSZ1,CTLMSZ2)
         exp_min=exp_min/ (
     &        (sin(pi180*(srcB+ctlscl/2.))-sin(pi180*(srcB-ctlscl/2.)))
     &        *ctlscl*pi180)
         exp_min=exp_min/1.0E07
         print *,'Exposure maximum:',exp_min,' 10^7 cm^2-s'
         exp_min=0.05*exp_min
         print *,'Exposure minimum cut:',exp_min,'10^7 cm^2-s'
      else
         read(input,*,end=1,err=8108)exp_min
      endif

      goto 8109
 8108 print *,' '

c     sb-02/26/02	in_dex = index(input, ' ')
c     sb-02/26/02	print *,'Input Error:',input(1:in_dex)

      print *,'Input Error:',input
      print *,'Enter a number, F or <cr> for default'
      print *,' '
      goto 1
 8109 continue 
      aspect_max=0

      if (abs(aspect_max-aspj).gt.0.001.and..not.jmapflg) then
 8111    if (.not.tttflg) write(6,'(
     &        "Specify maximum aspect angle (cr for ",f5.1,")",$)')aspj
         if (tttflg) write(6,'("Specify aspect angle",
     &        " for source removal (cr for ",f5.1,")",$)')aspj
         READ(LU(12),'(A)') input
         if (input.ne.' ') then
            read(input,*,end=8111,err=8111)aspect_max
	    aspj=aspect_max
         else
            aspect_max=aspj
         endif
      else
         aspect_max=aspj
      endif	
      
      if (coord_sys.eq.'C') then
         pL=SC_RAJ
         pB=SC_DECJ
      else
         pL=SC_LJJ
         pB=SC_BJJ
      endif

      if (speedmap.or.(tttflg.and.flag).or.jmapflg) then
         mapcnts=.false.
         goto 21
      endif

      write(6,'(
     &     "Do you want to save Gbias",
     &     " or Counts in the GBIASFILE (cr for Gbias)?",$)')

      READ(LU(12),'(A)') input
      if (input.eq.' ') then
         mapcnts=.false.
      else
         if (input(1:2).eq.'$$') return
         mapcnts=.true.
      endif

C     Clear out maps:
 21   continue
      if (tttflg.and.flag.and..not.jmapflg) return

      CALL MAPRST(XRRMAP,CTLMSZ1,CTLMSZ2)

      if (.not.tttflg2.or.jmapflg) CALL MAPRST(TMPMAP,CTLMSZ1,CTLMSZ2)

      CALL MAPRST(nmap,CTLMSZ1,CTLMSZ2)
      lshift=0.
      bshift=0.
      old_lat=0.

      if (speedmap) then
         itpj=2
      else
         itpj=1
      endif
c
      ntst=(ROIPND(1)-ROIPRG(1)+1)/2
 23   DO 80 IsrcB=ROIPRG(2),ROIPND(2) ! do latitude row
         if (jae_mapsrcj) then
            print *,'after 23'
         endif

         CALL MAPCOR (ROIPRG(1),IsrcB,srcL,srcB)
         CALL ERROR(0,LOC)
         the_lat=cos(srcB*PI180)**(-1)
         jpass=1

         if (scndpass) jpass=2
         if (abs(the_lat-old_lat).gt.0.01) then 

c     need to adjust PSF to CLAT
	    CLAT=srcB
ccc
	    CALL psmmat(lshift,bshift)
            CALL ERROR(0,LOC)
	    old_lat=the_lat
         endif

         if (jae_mapsrcj) then
            print *,'about to check writes'
         endif

         moreinput=jblank1//jblank1//jblank1//jblank1//jblank1
         input=jblank1//jblank1//jblank1//jblank1//jblank1
         IATST=MOD(IsrcB-ROIPRG(2),ntmpc)

         if (IATST.eq.0.or.IsrcB.eq.ROIPND(2).or.
     &        scndpass) then
            if (jae_mapsrcj) then
               print *,'first write to moreinput !'
            endif

            write(moreinput,'(" MAPSRC: latitude=",
     &           f7.2," for pass number ",i2," of ",i2)')srcB,jpass,itpj
            
csb-02/26/02		   in_dex = index(moreinput, ' ')
csb-02/26/02		   if(in_dex.eq.0)in_dex=1
csb-02/26/02		   if(in_dex.gt.100)in_dex=100

            if (jae_mapsrcj) then
               print *,'first write to screen'
            endif

            if (jae_mapsrcj) write(*,'(A)')moreinput
csb-02/26/02	      	 if(jae_mapsrcj)write(*,'(A)')moreinput(1:in_dex)
         endif

         if (MOD((IsrcB-ROIPRG(2)),5).eq.0) then
            if (jae_mapsrcj) then
               print *,'second write to moreinput !'
            endif

            write(moreinput,'(" MAPSRC: latitude=",
     &           f6.2," for pass number ",i2," of ",i2)')srcB,jpass,itpj
csb-02/26/02		   in_dex = index(moreinput, ' ')
csb-02/26/02		   if(in_dex.eq.0)in_dex=1
csb-02/26/02		   if(in_dex.gt.100)in_dex=100
            if (jae_mapsrcj) then
               print *,'second write to screen'
            endif

            write(*,'(A)')moreinput
csb-02/26/02	      	   write(*,'(A)')moreinput(1:in_dex)
         endif
         IsrcL=ROIPRG(1)        ! begin longitude loop
 100     CONTINUE

         if (jae_mapsrcj) then
            print *,'inside inner loop: ',IsrcL,IsrcB
         endif

         CALL MAPCOR (IsrcL,IsrcB,srcLpxcnt,srcBpxcnt)
         CALL ERROR(-1,LOC)
c
         srcL=srcLpxcnt
         srcB=srcBpxcnt
         LikTotaled=.false.

         if (MAPTYPE.eq.'SINGLE_POINTING') then
	    aspect=-1
	    if (.not.tttflg) aspect=gtcirc(srcL,srcB,pL,pB)
            if (aspect.gt.aspect_max) then
c     
c     aspect not OK
c
               if (jae_mapsrcj) then
                  print *,'Aspect not OK: ',
     &                 aspect,' < ',aspect_max,' at:',IsrcL,' ',IsrcB
               endif

               TS=-1.e-10
               if (.not.Restrict_Gmult) Gmult=1
               if (.not.Restrict_Gbias) Gbias=Gbias_nom/100.
               if (Restrict_Gmult) Gmult_sv=Gmult
               if (Restrict_Gbias) Gbias_sv=Gbias
               GOTO 99
	    endif
         endif

         expose=mapval(emap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) / (
     &        (sin(pi180*(srcB+ctlscl/2.))-sin(pi180*(srcB-ctlscl/2.)))
     &        *ctlscl*pi180)

         if (expose/1.E7.lt.exp_min) then
c     
c     there is not enough exposure here
c
	    if (jae_exp_anal) print *,'Low exposure:',expose/1.e7,
     &           ' < ',exp_min,' at:', srcL,' ',srcB
	    TS=-1.e-10

	    if (.not.Restrict_Gmult) Gmult=0
	    if (.not.Restrict_Gbias) Gbias=0
	    GOTO 99
         endif
ccc JAE FIX to Speed up this routine 
c  18-8-94 JAE
ccc

         if (.not.speedmap) then
            if (jae_mapsrcj) print *,'SpMap:',speedmap,
     &           ' IsrcL,IsrcB: ',IsrcL,IsrcB
            call SRCTEST(.false.)
         else
            ILTST=MOD(IsrcL-ROIPRG(1),ntmpc)
            IBTST=MOD(IsrcB-ROIPRG(2),ntmpc)

            if (.not.scndpass.and.
     &           ILTST.ne.0.and.IBTST.ne.0) then
               if (jae_mapsrcj) print *,speedmap,' IsrcL,IsrcB: ',
     &              IsrcL,IsrcB,' goto 101'
               goto 101

            elseif (scndpass.and.
     &              (ILTST.ne.0.or.IBTST.ne.0)) then
               if (jae_mapsrcj) print *,speedmap,' IsrcL,IsrcB: ',
     &              IsrcL,IsrcB,' goto11'
               go to 11

            else
               if (jae_mapsrcj) write(*,'("Pass 1 check"$)')
               if (scndpass) goto 101
               if (jae_mapsrcj) write(*,'(" PASS 2 "$)')
               if (IBTST.eq.0.and.ILTST.ne.0.and.
     &              IsrcL.ne.ROIPRG(1).and.IsrcL.ne.ROIPND(1)) goto 101
               if (ILTST.eq.0.and.IBTST.ne.0.and.
     &              IsrcB.ne.ROIPRG(2).and.IsrcB.ne.ROIPND(2)) goto 101
c     print *,speedmap,' IsrcL,IsrcB: ',IsrcL,IsrcB
               call SRCTEST(.false.)
               if(signal.ne.' ')goto 98
               goto 12
            endif
c
c     calculate linear fit for Gmult and Gbias
c
c     New routine to locate proper bounding integer values and places them
c     in IRRL and IRRB.  The routine will return signal != ' ' if any of 
c     the bounding locations have improper values.
c
 11         if (jae_mapsrcj) then
               print *,'Call Jfind_iloc'
            endif

            CALL JFIND_ILOC(TMPMAP,IsrcL,IsrcB,IRRL,IRRB,
     &           ntmpc,ROIPND(1)-ROIPRG(1)+1,ROIPND(2)-ROIPRG(2)+1)

            if (signal.ne.' ') then
               signal=' '
               call SRCTEST(.false.)
               if(signal.ne.' ')goto 98
               goto 12
            endif

            dIRRL = IRRL(2,2)-IRRL(1,1)
            dIRRB = IRRB(2,2)-IRRB(1,1)
            dxL = (1.0*(IsrcL-IRRL(1,1)))/dIRRL
            dxB = (1.0*(IsrcB-IRRB(1,1)))/dIRRB

            do jjk=1,2
               do jjl=1,2
                  if (.not.Restrict_Gmult) then
                     ggm(jjk,jjl)=MAPVAL(nmap,IRRL(jjk,jjl),
     &                    IRRB(jjk,jjl),CTLMSZ1,CTLMSZ2)
                  endif

                  if (.not.Restrict_Gbias) then
                     ggb(jjk,jjl)=MAPVAL(XRRMAP,IRRL(jjk,jjl),
     &                    IRRB(jjk,jjl),CTLMSZ1,CTLMSZ2)
                  endif
               enddo
            enddo

            if (.not.Restrict_Gmult) then
               Gmult_sv=(1-dxL)*((1-dxB)*ggm(1,1)+dxB*ggm(1,2)) +
     &              dxL*((1-dxB)*ggm(2,1) + dxB*ggm(2,2))
            endif

            if (.not.Restrict_Gbias) then
               Gbias_sv=(1-dxL)*((1-dxB)*ggb(1,1)+dxB*ggb(1,2)) +
     &              dxL*((1-dxB)*ggb(2,1) + dxB*ggb(2,2))
            endif

            if (jae_mapsrcj) then
               print *,' '
               print *,'I,J: ',IsrcL,IsrcB
               print *,'Gmult_sv: ',Gmult_sv
               print *,'Gbias_sv: ',Gbias_sv
               print *,' '
               print *,'*************************************'
            endif
c     
            counts=0
            CALL GASBIAS(.false.)
            source_notlnL=-lnL

            if (signal.ne.' ') then
               goto 98
            endif

            if (.not.Restrict_Gmult) Gmult=Gmult_sv
            if (.not.Restrict_Gbias) Gbias=Gbias_sv
            CALL  CNTTEST(.false.)

            if (signal.ne.' ') goto 98
            source_lnL = -lnL
            TS = 2.*(source_lnL-source_notlnL)

            if (jae_mapsrcj) then
               print *,' notlnL: ',source_notlnL
               print *,'    lnL: ',source_lnL
               print *,'TS: ',TS
            endif

 12         if (TS.lt.0) TS=0
c     
c     End of else when if(speedmap) is valid
c     
         endif
c     
c     End of JAE fixup 18-8-94
cccc  
 98      if (signal.ne.' ') then
	    print *,'Error condition detected at',srcL,srcB
            CALL ERROR(0,LOC)
	    if(jae_mapsrcj)write(6,*)
     &           'Will write -0 to TS and background maps at ',
     *           'this point.'
	    TS=-2.e-10
	    if(.not.Restrict_Gmult)Gmult=0
	    if(.not.Restrict_Gbias)Gbias=0
	    signal=' '
         endif

 99      if (jae_mapsrcj) print *,'Writting maps. TS:',TS
         if (.not.tttflg2)
     &        call VALMAP(TS,TMPMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 
         call VALMAP(Gmult,nmap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 
         call VALMAP(Gbias,XRRMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2)

c     !DO ALL COLUMNS IN THE ROI
 101     CALL MAPINC(IsrcL,IEND,ROIPRG(1),ROIPND(1)) 
         CALL ERROR(0,LOC)

         IF (IEND.NE.1) GOTO 100
 80   CONTINUE                  ! end latitude loop

      if (.not.scndpass.and.speedmap) then
         scndpass=.true.
         old_lat=2.0
         goto 23
      endif

      if (tttflg2) return

      CALL MAPMAX(TMPMAP,IsrcL,IsrcB,SIGNIF,CTLMSZ1,CTLMSZ2)
      CALL MAPCOR(IsrcL,IsrcB,srcLpxcnt,srcBpxcnt)
      CALL ERROR(1,LOC)
      srcL=srcLpxcnt
      srcB=srcBpxcnt
c     WRITE(LU(1),'("HIGHEST SIGNIFICANCE (TS=",f7.1,") IS AT: ",
c     2f6.1)') SIGNIF,srcL,srcB
c     WRITE(LU(1),*)'Use L option for more information'
      CLAT=srcB
      CALL psmmat(lshift,bshift)
      LikTotaled=.false.

      if (tttflg.and..not.flag) return

      write(TMPDOC(2),'("Counts map: ",a)') cmapfile(1:58)
      write(TMPDOC(3),'("Diffuse map: ",a)') gmapfile(1:57)
      WRITE(TMPDOC(4),'
     &     ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &     gamma,Ranal

      if (NSOURCE.eq.0) then
         TMPDOC(5)='There are no PSFs modeled.'
      else
         write(TMPDOC(5),'(
     &        "There are ",I3," PSFs modeled. Number 1: ")')NSOURCE
         write(TMPDOC(5)(39:70),'(
     &        "At",f7.2,",",f6.2," are",f5.0," cnts; ")')
     &        SRC_PARMS(1,1),SRC_PARMS(1,2),SRC_PARMS(1,3)
      endif

      do i=6,10
         TMPDOC(i)=' '
      enddo

      do i=2,10
         XRRDOC(i)=TMPDOC(i)
      enddo

      write(TMPDOC(1),'(a,"- v ",a," map of EGRET point-",
     &     "source test statistic")') PRG,VER
      TMPTYP = 'LSTA'
      MAPFILE=LMAPFILE
      CALL MAPRITroi(TMPMAP,TMPTYP,TMPDOC,BMAP)
      CALL ERROR(1,LOC)

      if (mapcnts) then
         write(XRRDOC(1),'(a,"- v ",a," map of model point ",
     &        "source counts")')PRG,VER
         XRRTYP = 'CNTS'
         MAPFILE=GBIASFILE
         CALL MAPRITroi(XRRMAP,XRRTYP,XRRDOC,BMAP)
         CALL ERROR(1,LOC)
      else
         write(XRRDOC(1),'(a," - v ",a," map of isotropic ",
     &        "diffuse component")') PRG,VER
         XRRTYP = 'GBIA'
         MAPFILE=GBIASFILE
         CALL MAPRITroi(XRRMAP,XRRTYP,XRRDOC,BMAP)
         CALL ERROR(1,LOC)
      endif

      write(XRRDOC(1),'(a," - v ",a," map of diffuse ",
     &     "model multiplier")')PRG,VER
      XRRTYP = 'GMUL'
      MAPFILE=NEWMAPFILE
      CALL MAPRITroi(nmap,XRRTYP,XRRDOC,BMAP)
      CALL ERROR(1,LOC)

c     restore BMAP
      CALL PSFBLD
      CALL ERROR(1,LOC)
      
      report=rpt_sv
      report2=rpt2_sv
      calc_uncert=calc_u_sv

      RETURN
      END
