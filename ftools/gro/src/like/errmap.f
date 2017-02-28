c      SUBROUTINE ERRMAP(interact,Iconf,deltaTS,
c     & a,b,phi,RMS,aveL,aveB,medium,ident,disjoint)
c
C
C  $Id: errmap.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++   Effect: a finemap is created, and the source position
C++	estimate and uncertainty is determined.  
C++	If (srcN(1:5).eq.'GRO J'.or.srcN(1:3).eq.'2CG') then
C++	srcL & srcB are updated with the position estimate.
C***********************************************************************
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c input Arguments:
c     logical  interact  false =>  no interaction with use
c     integer   Iconf    percentage confidence for source location
c     real      deltaTS   drop in TS corresponding to this confidence
c     character(24)   medium    specifies graphics mode
c returned Arguments:
c     real       a    semi-major axis (in radians)
c     real       b    semi-minor axis (in radians) 
c     real       phi  position angle of major axis
c     real   RMS    RMS deviation of elliptical fit from region boundary
c     real   aveL   position estimate
c     real   aveB   position estimate
c     logical   ident     true => identity of source in doubt
c     logical   disjoint  true => source region is disjoint
c
C***********************************************************************
c	definitions of key parameters:
c	ellipse fit parameters, a=major axis, b=minor axis, phi=position 
c	angle of major axis, RMS deviation of elliptical fit from region 
c	boundry (a,b,phi,RMS in radian).
c       org_srcL - original longitude (from catalog - normally positive)
c       SrcL - corresponding longitude within map (may be negative)
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
c
C=======================================================================
C  $Log: errmap.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/12/26 17:33:17  irby
C  - Change negative exponents to e.g. -1*number instead of -number for f90
C    compatibility (and put in parens (-1) where necessary.
C
C  - Fix write statements (commas).
C
C  Revision 1.1  2002/04/16 20:27:30  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/18  15:07:18  jae
c Repaired error on line 329 reported by cvm
c
c Revision 5.1  1996/02/29  20:47:27  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:46  jae
c Subroutine Module for like V5.00
c
C%    Changes:
c     11/11/93 jrm Base contours on likelihood at the position estimate
C Installation of RCS lines: 23 AUG 1995 by JAE
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE ERRMAP(interact,Iconf,deltaTS,
     &     a,b,phi,RMS,aveL,aveB,medium,ident,disjoint)

C     Common blocks used:
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/nmprep.copy'

      save

      character(80) id
      common /id/id
      REAL MAPVAL
      REAL tmp_CTLORG(2),tmp_CTLEND(2)
      character input*50,old_srcN*18,old_signal*1,medium*24
      character filename*24
      logical interact,wrong,initialize,giveup,determined
      logical ident,disjoint


      id = '$Id: errmap.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      LOC='ERRMAP'

      if (jae_errmap)write(*,*)'Inside routine ERRMAP'

      initialize=.true.
      giveup=.false.
      Rc=-1.
      phi=-1.
      nloop=0
      Nbound=0
      Npoints=10
      psf_rad=Ranal
      if (psf_rad.gt.20.) psf_rad=20.

c     more initialization
      if (.not.interact) then
         calc_uncert=.false.
      else
         report=.true.
         calc_uncert=.true.
         do n=1,NSOURCE
	    if (abs(SRC_PARMS(n,1)-srcL).le.0.1.and.
     &           abs(SRC_PARMS(n,2)-srcB).le.0.1.and.
     &           abs(SRC_PARMS(n,3)).ge.1.0) then
               write(6,'("This source is in the background model. "/
     &              "It is suggested that you use PA to remove ",
     *              "it first.")')
               signal='E'
               sigmsg='ERRMAP:Source in background model'
               return
	    endif
         enddo
         write(6,'("Enter number rows & columns in fine ",
     &        "map (cr for ",I2,"):",$)')Npoints+1
         read(LU(12),'(A)')input
         if (input.ne.' ') then
	    read(input,*,end=1111)Npoints
	    Npoints=Npoints-1   !really Ndivisions
         endif
      endif
      orgL=srcL
      orgB=srcB

 10   continue                  ! set up for finemap 

      if (interact)print *,'Obtain fine map centered on',srcL,srcB
      Counts_previous=Counts
      Counts=0.

      if (interact) write(6,*)
     &     'Solving for lnL with Counts=0 for entire fine map.'
      if (gmap_null) then
         Gmult=0.
         call BIASTEST(.false.)
      else
         call gasbias(.false.)
      endif

      if (signal.ne.' ') goto 1113

      source_notlnL=-lnL
      Counts=Counts_previous

      if (interact) write(6,'("Solving for Counts, Gmult, and ",
     &     "Gbias to be used for entire fine map.")')

      if (gmap_null) then
         call CNTSBIAS(.false.)
      else
         call allfit(.false.)
      endif

      if (signal.ne.' ') goto 1113
      if ((-lnL-source_notlnL)*2..lt.deltaTS) then
         signal='S'
         write(sigmsg,'(
     &        "TS=",f4.1," makes no sense to try to locate this.")')
     &        (-lnL-source_notlnL)*2.
         if (interact) then
            CALL ERROR(0,LOC)
            signal='S'
            call intent(determined)
            if (.not.determined) goto 1113
         else
            goto 1113
         endif
      endif

      if (initialize) then
         diameter_int=4./(Counts/100.)**0.7
         diameter=diameter_int
         if (diameter.gt.psf_rad*1.333) diameter=psf_rad*1.333
         if (interact) then
	    write(6,'(
     &           "Initial finemap full width ",
     *           "(cr for ",f7.2,"):",$)')diameter
            read(LU(12),'(A)')input
            if (input.ne.' ') read(input,*,end=1111)diameter
c     this is the angle between the centers of the outside pixels
         endif
         initialize=.false.
      elseif (orgL.ne.srcL.or.orgB.ne.srcB) then
         if (interact)print *,'New finemap center=',srcL,srcB
      endif

 20   continue                  !make finemap
      nloop=nloop+1
      if (nloop.gt.10) then
         signal='I'
         sigmsg='ERRMAP: exceeded maximum iterations.'
         goto 1113
      endif

      if (interact)write(6,'("Try finemap full width=",f7.2)')diameter

      width=diameter/float(Npoints)
      cornerL=srcL-diameter/2.
      cornerB=srcB-diameter/2.
      xlmax=-1.e12

C     Do likelihood ratio analysis of these points:
      DO  I = 1,Npoints+1
         PL = cornerL + (I-1) * width
         DO  J = 1,Npoints+1
            PB = cornerB + (J-1) * width
	    call psmmat(PL-srcLpxcnt,PB-srcBpxcnt)
	    if (signal.ne.' ') goto 1113
	    LikTotaled=.false.
  	    IMAP=I+ (Npoints+1)*(J-1)
	    call liktot(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
	    if (signal.ne.' ') goto 1113
            CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
	    if (signal.ne.' ') goto 1113
       	    TMPMAP(IMAP) = 2.*(-lnL-source_notlnL)
	    if (signal.ne.' ') then
               write(6,'("Mapping error condition detected for",a,
     &              "  Abort mapping.")') srcN
               goto 1113
	    endif
	    if (xlmax.lt.TMPMAP(IMAP)) then
               xlmax=TMPMAP(IMAP)
               xmaxb=PB
               xmaxl=PL
	    endif
         ENDDO
      ENDDO

      if (interact) write(lu(1),'("Maximum TS =",f7.1,
     &     "; found at L=",f6.1," B=",f6.1)') xlmax,
     &     xmaxl,xmaxb
      
c     is the finemap properly located?
      if (xmaxl.gt.cornerL+0.666*diameter.or.
     &     xmaxl.lt.cornerL+0.333*diameter.or.
     &     xmaxb.gt.cornerB+0.666*diameter.or.
     &     xmaxb.lt.cornerB+0.333*diameter) then
         if (interact) then
            write(6,'("Maximum not within central 1/3 of both",
     &           " dimensions of map - redo centered on max")')
         endif
         srcL=xmaxl
         srcB=xmaxb
         call pixel_select(.false.,' ')
         call error(-1,loc)       
         goto 10
      endif

c     set CTL for fine map
      valmin=xlmax-deltaTS      !minimum value for region
C     Save CTL data
      tmp_CTLSCL=CTLSCL
      tmp_CTLMSZ1=CTLMSZ1
      tmp_CTLMSZ2=CTLMSZ2

      do i=1,2
         tmp_CTLORG(i)=CTLORG(i)
         tmp_CTLEND(i)=CTLEND(i)
      enddo

C     Reset CTL data for finemap
      CTLSCL=width
      CTLMSZ1=Npoints+1
      CTLMSZ2=Npoints+1
      CTLORG(1)=cornerL
      CTLORG(2)=cornerB
      CTLEND(1)=cornerL+diameter
      CTLEND(2)=cornerB+diameter

c     are there map border pixels within error contour?
      Nborder=0
      il=1

      do ib=1,ctlmsz2
         if (mapval(tmpmap,il,ib,CTLMSZ1,CTLMSZ2).gt.valmin) 
     &        Nborder=Nborder+1
      enddo

      ib=1
      do il=1,ctlmsz1
         if (mapval(tmpmap,il,ib,CTLMSZ1,CTLMSZ2).gt.valmin)
     &        Nborder=Nborder+1
      enddo

      il=CTLMSZ1
      do ib=1,ctlmsz2
         if (mapval(tmpmap,il,ib,CTLMSZ1,CTLMSZ2).gt.valmin) 
     &        Nborder=Nborder+1
      enddo

      ib=CTLMSZ2
      do il=1,ctlmsz1
         if (mapval(tmpmap,il,ib,CTLMSZ1,CTLMSZ2).gt.valmin) 
     &        Nborder=Nborder+1
      enddo

      if (Nborder.eq.0) then
c     no border pixels are in error contour- proceed
         wrong=.false.
      else
         if (interact) then
	    print *,'Map is too small,',Nborder,
     &           ' border pixels are within contour.'
         endif
         if (diameter.ge.psf_rad*1.333) then
	    signal='b'
	    sigmsg='ERRMAP:Cannot make map big enough to contain region'
	    giveup=.true.
	    goto 30
         endif
         wrong=.true.
         diameter=diameter*(1.+3.*float(Nborder)/float(ctlmsz1*4))
         if (diameter.gt.psf_rad*1.333) diameter=psf_rad*1.333
         goto 30
      endif

c     is the finemap size acceptable?
      Nin=0
      aveL=0.
      aveB=0.

      do il=2,ctlmsz1-1
         do ib=2,ctlmsz2-1
            if (mapval(tmpmap,il,ib,CTLMSZ1,CTLMSZ2).gt.valmin) then
c     the source could be in this pixel
               Nin=Nin+1
               call mapcor(il,ib,xl,xb)
               call error(-1,loc)
               aveL=aveL+xl
               aveB=aveB+xb
            endif
            call valmap(0.,nmap,il,ib,CTLMSZ1,CTLMSZ2) !null map for disjoint test
         enddo
      enddo

      aveL=aveL/float(Nin)
      aveB=aveB/float(Nin)
      ratio_in=float(Nin)/float(ctlmsz1)**2

      if (ratio_in.lt.0.15.or.ratio_in.gt.0.30) then
         goto 25                ! map size is OK
      endif

      if (nloop.ge.3) then
         if (ratio_in.gt.0.05.and.ratio_in.lt.0.50) then
            goto 25             ! map size is OK
         endif
      endif

      if (nloop.ge.6) then
         if (ratio_in.gt.0.02.and.ratio_in.lt.0.90) then
            goto 25             ! map size is OK
         endif
      endif

      if (nloop.ge.9) then
         if (ratio_in.gt.0.01.and.ratio_in.lt.0.95) then
            goto 25             ! map size is OK
         endif
      endif

c     map size unacceptable
      if (interact)write(6,
     &     '("fraction of pixels in contour is",f7.2)')ratio_in
      if (ratio_in.lt.0.15) then
         if (interact) write(6,'("Map is too large; ",$)')
      else
         if (interact) write(6,'("Map is too small; ",$)')
         if (diameter.ge.psf_rad*1.333) then
            signal='b'
            sigmsg='ERRMAP:Cant make map big enough to contain region'
            giveup=.true.
            goto 30
         endif
      endif

      diameter=diameter*sqrt(ratio_in/0.22)
      wrong=.true.
      xmaxl=aveL                !center new map on ave position
      xmaxb=aveB
      goto 30

 25   continue
c     ok - we will use this map

c     examine for disjointedness

      call mapixl(xmaxl,xmaxb,il,ib)
      call error(-1,loc)

c     assume this is the correct region if there is disjointedness
      call valmap(1.,nmap,il,ib,CTLMSZ1,CTLMSZ2) 
      Njoint_last=0
      do j=1,50
         disjoint=.false.
         Njoint=0
         aveL=0.
         aveB=0.
         do il=2,ctlmsz1-1
            do ib=2,ctlmsz2-1
               if (mapval(tmpmap,il,ib,CTLMSZ1,CTLMSZ2).gt.valmin) then
c     the source could be in this pixel, examine neighbors
                  do jl=il-1,il+1
                     do jb=ib-1,ib+1
                        if (mapval(nmap,jl,jb,CTLMSZ1,CTLMSZ2).gt.
     *                       0.5) then
c     a neighbor is within contour - join it
                           call valmap(1.,nmap,il,ib,CTLMSZ1,CTLMSZ2)
                           call mapcor(il,ib,xl,xb)
                           call error(-1,loc)
                           aveL=aveL+xl
                           aveB=aveB+xb
                           Njoint=Njoint+1
                           goto 300
                        endif
                     enddo
                  enddo
                  disjoint=.true.
 300              continue
               endif
            enddo
         enddo
         if (Njoint.ne.Njoint_last) then
c     we have not yet converged
            Njoint_last=Njoint
	 else
            goto 310
         endif
      enddo
      signal='D'
      sigmsg='ERRMAP:Disjoint test exceeds loop limit'
      giveup=.true.
      goto 30
 310  continue

      aveL=aveL/float(Njoint)
      aveB=aveB/float(Njoint)
      
      if (cornerL.lt.0..and.cornerL+diameter+width/2..lt.0.) then
c     all longitude is negative
         CTLORG(1)=CTLORG(1)+360.
         CTLEND(1)=CTLEND(1)+360.
         cornerL=cornerL+360.
         aveL=aveL+360.
         xmaxl=xmaxl+360.
         srcL=srcL +360.
      endif

c     find the interpolated border points of the error contour
      do il=2,ctlmsz1-1
         do ib=2,ctlmsz2-1
            if (mapval(tmpmap,il,ib,CTLMSZ1,CTLMSZ2).gt.valmin) then
c     the source could be in this pixel
c     goto 320 if not in subregion
               if (mapval(nmap,il,ib,CTLMSZ1,CTLMSZ2).lt.0.5) goto 320
               TSpix=mapval(tmpmap,il,ib,CTLMSZ1,CTLMSZ2)
               call mapcor(il,ib,xl,xb)
               call error(-1,loc)
               if (mapval(tmpmap,il+1,ib,CTLMSZ1,CTLMSZ2).le.
     *              valmin) then
c     source is excluded from neighboring pixel - this is a boundry
                  Nbound=Nbound+1
                  if (Nbound.gt.509) goto 1112
                  TS_fine1(Nbound)=xl+ctlscl*(TSpix-valmin)/
     &                 (TSpix-mapval(tmpmap,il+1,ib,CTLMSZ1,CTLMSZ2))
                  TS_fine2(Nbound)=xb
               endif
               if (mapval(tmpmap,il-1,ib,CTLMSZ1,CTLMSZ2).le.
     *              valmin) then
c     source is excluded from neighboring pixel - this is a boundry
                  Nbound=Nbound+1
                  if (Nbound.gt.509) goto 1112
                  TS_fine1(Nbound)=xl-ctlscl*(TSpix-valmin)/
     &                 (TSpix-mapval(tmpmap,il-1,ib,CTLMSZ1,CTLMSZ2))
                  TS_fine2(Nbound)=xb
               endif
               if (mapval(tmpmap,il,ib+1,CTLMSZ1,CTLMSZ2).le.
     *              valmin) then
c     source is excluded from neighboring pixel - this is a boundry
                  Nbound=Nbound+1
                  if (Nbound.gt.509) goto 1112
                  TS_fine1(Nbound)=xl
                  TS_fine2(Nbound)=xb+ctlscl*(TSpix-valmin)
     &                 /(TSpix-mapval(tmpmap,il,ib+1,CTLMSZ1,CTLMSZ2))
               endif
               if (mapval(tmpmap,il,ib-1,CTLMSZ1,CTLMSZ2).le.
     *              valmin) then
c     source is excluded from neighboring pixel - this is a boundry
                  Nbound=Nbound+1 
                  if (Nbound.gt.509) goto 1112
                  TS_fine1(Nbound)=xl
                  TS_fine2(Nbound)=xb-ctlscl*(TSpix-valmin)
     &                 /(TSpix-mapval(tmpmap,il,ib-1,CTLMSZ1,CTLMSZ2))
               endif
            endif
 320        continue
         enddo
      enddo

c     restore original CTL data
 30   continue
C     Reset CTL data
      CTLSCL=tmp_CTLSCL
      CTLMSZ1=tmp_CTLMSZ1
      CTLMSZ2=tmp_CTLMSZ2

      do i=1,2
         CTLORG(i)=tmp_CTLORG(i)
         CTLEND(i)=tmp_CTLEND(i)
      enddo
      
      if (giveup) goto 1113
      if (wrong) then

c     opps - fine map not acceptable - do it again
         if (xmaxl.gt.srcL+0.1*diameter.or.
     &        xmaxl.lt.srcL-0.1*diameter.or.
     &        xmaxb.gt.srcB+0.1*diameter.or.
     &        xmaxb.lt.srcB-0.1*diameter) then
c     re-center map on max TS pixel
            srcL=xmaxl
            srcB=xmaxb
            call pixel_select(.false.,' ')
            call error(-1,loc)       
            goto 10
         else
c     map centering is OK
            goto 20
         endif
      endif

c     analyze the result
      solid_angle=float(Nin)*width**2*cos(pi180*srcB)
      Rc=sqrt(solid_angle/3.1416)

      if (abs(aveB)+Rc.gt.80.) then
         if (coord_sys.eq.'C') then
	    sigmsg='ERRMAP:source is near the celestial pole'
         else
	    sigmsg='ERRMAP:source is near the galactic pole'
         endif
         signal='P'
         call error(0,loc)
         print *,'Error region analysis may be in error.'       
      endif

      call FITERR(aveL,aveB,Nbound,a,b,phi,Rc)
      if (signal.ne.' ') then
         if (interact) then
	    call error(0,loc)
	    print *,'Failure to fit elipse - use circle'
         else
	    signal=' '
         endif
         a=Rc*PI180
         b=Rc*PI180
         phi=0.
      endif

c     find deviation
      RMS=0.
      a2=a**2
      b2=b**2

      do n=1,Nbound
         theta=TS_fine1(n)-phi
         Rfit=((cos(theta)**2)/a2+(sin(theta)**2)/b2)**(-1.*0.5)
         TS_fine2(n)=TS_fine2(n)**(-1.*0.5)
         RMS=RMS+(Rfit-TS_fine2(n))**2
      enddo

      RMS=sqrt(RMS/float(Nbound))

      if (srcN(1:5).eq.'GRO J'.or.srcN(1:3).eq.'2CG') then
c     this source has not been identified - use measured position
         ident=.false.
         srcL=aveL
         srcB=aveB
         call pixel_select(.false.,' ')
         if (signal.ne.' ') goto 1113
         if (srcN(1:5).eq.'GRO J') then
c     update GRO name?
	    old_srcN=srcN
	    call iauname(srcL,srcB)
	    if (interact.and.old_srcN.ne.srcN) then
               print *, 'updating name from ',old_srcN
               print *, 'to ',srcN
	    endif
         endif

      else
c     this source has been identified - restore catalog position
         srcB=orgB
         srcL=orgL
         if (srcL.lt.0.) srcL=srcL+360.
         tempL=srcL
         call L_check(tempL)
         call psmmat(tempL-srcLpxcnt,srcB-srcBpxcnt)
         if (signal.ne.' ') goto 1113
         LikTotaled=.false.
c     check identification
         call liktot(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         if (signal.ne.' ') goto 1113
         CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         if (signal.ne.' ') goto 1113
         TSident = 2.*(-lnL-source_notlnL)
         if (TSident.lt.xlmax-6.0) then
	    ident=.true.
         else
	    ident=.false.
         endif
      endif

      if (interact) then
         f=60./PI180
         print *, 'Mean position within error region',aveL,aveB    
	 if (srcN(1:5).eq.'GRO J'.or.srcN(1:3).eq.'2CG') then
c     this source has not been identified
	 else
            print *,'The identified source is',
     *           gtcirc(srcL,srcB,aveL,aveB)
     &           ,' degrees from the mean position.'
            print *,'At the identified position, the TS is down by',
     &           xlmax-TSident,' from max'
            if (ident)
     &           print *,'The identification is questionable.'
	 endif
         print *,'Characteristic radius is',Rc*60.
         print *,
     &        'ellipse fit (in arcmin): major axis a=',a*f,
     &        '; minor axis b=',b*f,
     &        '; position angle phi=',phi/PI180,
     &        '; yields RMS deviation',RMS*f,' arcmin'
      endif
      
      if (medium.eq.'H') then
c     print plot
         call MAPERR('Like_position.ps/ps     ',
     &        tmpmap,Npoints+1,cornerL,cornerB,width,xlmax,
     &        xmaxl,xmaxb,aveL,aveB,deltaTS,Iconf,a,b,phi,RMS)
         call system('lpr Like_position.ps')

      elseif (medium.eq.'I') then
c     do nothing

      elseif (medium.eq.'F') then
c     file to disk
 500     i=18 
         do while (srcN(i:i).eq.' ')
            i=i-1
            if (i.lt.1) then
               write(6,'("Problem deriving name for ",a,
     &              " what to call it (A18)? ",$)') srcN
               read(LU(12),'(A)')srcN
               goto 500
            endif
         enddo
         filename=srcN(1:i)
         do j=1,i
            if (filename(j:j).eq.' ')filename(j:j)='_'
         enddo
         
         filename=filename(1:i)//'.ps'
         if (interact) 
     &        write(6,'("Writing postscript file ",a)') filename(1:i+3)

         call MAPERR(
     &        filename,tmpmap,Npoints+1,cornerL,cornerB,width,xlmax,
     &        xmaxl,xmaxb,aveL,aveB,deltaTS,Iconf,a,b,phi,RMS)
      else
c     view plot
         call MAPERR(
     &        medium,tmpmap,Npoints+1,cornerL,cornerB,width,xlmax,
     &        xmaxl,xmaxb,aveL,aveB,deltaTS,Iconf,a,b,phi,RMS)
      endif

      call error(0,loc)
      return
      
 1111 write(lu(1),*)'Invalid input, try again.'
      return
 1112 sigmsg='ERRMAP:Boundry points exceeds storage size' !509=sqrt(360*720)
      signal='C'
 1113 continue 
c     exiting with error - restore orginal coordinates
      old_signal=signal
      signal=' '
      srcL=orgL
      srcB=orgB

      call pixel_select(.false.,' ')
      call error(-1,loc)       

      signal=old_signal

      return
      END
