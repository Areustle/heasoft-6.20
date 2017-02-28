C       SUBROUTINE LISTPROC
C
C
C  $Id: listproc.f,v 1.4 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++     effect: A catalog of candidate sources is analyzed.
C	A flux or upper limit is derived for each source.
C	The format of the list of coordinates may be specified.
C	A control position may be analyzed at the same latitude
C	with similar exposure for each catalog position analyzed.
C***********************************************************************
c	definitions of key parameters:
c       positive longitude (from catalog):  org_srcL
c       corresponding longitude within map (may be negative):  SrcL
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C  $Log: listproc.f,v $
C  Revision 1.4  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
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
C  Revision 1.1  2002/04/16 20:27:36  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.5  1997/08/01  21:48:06  jae
c Changed lines:
c 	Ilong2 = Ilat1 + 5
c to
c 	Ilong2 = Ilong1 + 5
c
c
c MISC_DIR is now read from like.par.  Deleted call to getenv to get this
c variable - Sandhia Bansal 12/01
c
c in the RA and Longitude if statement for src.list format files.
c This section of code was NEVER correct !!!  But suddenly went bad
c during runtime.
c
c Revision 5.4  1996/09/03  17:02:10  jae
c Made slight modification to input column
c setting for default.
c
c Revision 5.3  1996/06/06  18:51:12  jae
c Added more debug output
c
c Revision 5.2  1996/06/06  17:38:41  jae
c repaired error in position where pL and pB
c are set.  They are now set after exp_anal
c is called.
c
c Revision 5.1  1996/02/29  20:51:41  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:48  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE LISTPROC

C     Common blocks used:
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
      CHARACTER input*200,coord_phr*10,break*80
c     CHARACTER ex_dir*60
      LOGICAL FEXIST,correlate,determined
      REAL MAPVAL

      id = '$Id: listproc.f,v 1.4 2013/05/21 19:08:26 irby Exp $'
      data break/
     &'--------------------------------------------------------------'/
      data Off_set /0.05/
      LOC='LISTPROC'

      if(jae_listproc)write(*,'("In ",a)') LOC(1:8)

      calc_uncert=.true.
      report2=.true. 
      report=.true.
      initL=srcL
      initB=srcB
      write(6,*)
     &     'Will solve for point sources for a catalog.'
      write(6,'("Counts map: ",a)') cmapfile(1:50)
      write(6,'("Diffuse map: ",a)') gmapfile(1:50)
      WRITE(6,'
     &     ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &     gamma,Ranal
      write(6,*)
     &     'These PSFs are in the OTHER PSF map:'
      if(NSOURCE.lt.1) then
         write(6,*)'No sources in other PSF map.'
      else
         write(6,*)
     &        '  #   NAME                  POSITION    ',
     &        '   CNTS   Sp. I.'
         do nsrc=1,NSOURCE
	    theLong=SRC_PARMS(nsrc,1)
	    if(theLong.lt.0.)theLong=theLong+360.
            write(6,'(
     &           i3,3x,A18,2f7.2,f10.1,f6.2,f9.0)')
     &           nsrc,SRC_NAMES(nsrc),theLong,
     &           (SRC_PARMS(nsrc,ip),ip=2,4)
         enddo
      endif

      write(6,*)
     &     'Strong sources should be modeled.'
      call intent(determined)
      write(6,'(
     &     "Do you wish to have a control position analyzed at the",/
     &     "same latitude with similar exposure for each catalog",/
     &     "position analyzed? (cr for no)",$)')
      READ(LU(12),'(A)')input
      if(input.eq.' '.or.input.eq.'n'.or.input.eq.'N')then
         correlate=.false.
      else
         if(coord_sys.eq.'C') then
            write(6,*)
     &           'Correlation suppported only with galactic coord.maps.'
	    correlate=.false.
         else
	    correlate=.true.
         endif
      endif
      
      call exp_anal(aspect_max,exp_min)
      if(coord_sys.eq.'C') then
         coord_phr='RA     DEC'
         pL=SC_RAJ
         pB=SC_DECJ
      else
         coord_phr=' L       B'
         pL=SC_LJJ
         pB=SC_BJJ
      endif

      write(6,'(
     &     "Disk file of input catalog? (cr for src.list)",$)')
      READ(LU(12),'(A)') input
      
      if(input.eq.' ')then
c     call getenv('MISC_DIR',ex_dir)
c
c     misc_dir is now read from like.par  sb-12/01
c
c     ex_dir = misc_dir
         in_dex = index(misc_dir, ' ') - 1
         if(in_dex.lt.2)then
	    Print *,'MISC_DIR environmental variable must be set',
     &           ' to use default.'
            goto 1111
         else
            input=misc_dir(1:in_dex)//'/src.list'
         endif
      endif

      INQUIRE (FILE=input,EXIST=FEXIST)
      IF(FEXIST) THEN
         open(8,file=input)
      ELSE
         WRITE(lu(1),*)'File does not exist.'
         return
      ENDIF

      Inam1=1
      Inam2=18
      if(coord_sys.eq.'C') then
         Ilong1=85
         Ilong2=Ilong1+5
         Ilat1=94
         Ilat2=Ilat1+5
      else
         Ilong1=67
         Ilong2=Ilong1+5
         Ilat1=76
         Ilat2=Ilat1+5
      endif

      if(input(in_dex+2:in_dex+9).eq.'src.list') then
         input=' '
      else
         write(6,'("Is this in src.list format (cr for yes)?:)",$)')
         READ(LU(12),'(A)') input
      endif

      if(input.ne.' ')then
         write(6,'(
     &        "Character range for source name? ",
     *        "(default is",2I3,"):",$)')
     &        Inam1,Inam2
         READ(LU(12),'(A)') input
         if(input.ne.' ')read(input,*,end=1111)Inam1,Inam2
         
         if(coord_sys.eq.'C') then
            write(6,'(
     &           "Character range for RA? (default is",2I4,"):",$)')
     &           Ilong1,Ilong2
         else
            write(6,'(
     &           "Character range for longitude? ",
     *           "(default is",2I4,"):",$)')
     &           Ilong1,Ilong2
         endif
         READ(LU(12),'(A)') input
         if(input.ne.' ')read(input,*,end=1111)Ilong1,Ilong2
         
         if(coord_sys.eq.'C') then
            write(6,'(
     &           "Character range for declination? ",
     *           "(default is",2I4,"):",$)')
     &           Ilat1,Ilat2
         else
            write(6,'(
     &           "Character range for latitude? ",
     *           "(default is",2I4,"):",$)')
     &           Ilat1,Ilat2
         endif
         READ(LU(12),'(A)') input
         if(input.ne.' ')read(input,*,end=1111)Ilat1,Ilat2
      endif                     ! end cat parm adj exclude
      
      write(6,'(
     &     "Disk file of output catalog? (cr for LC-catalog-out):)",$)')
      READ(LU(12),'(A)') input
      if(input.eq.' ')input='LC-catalog-out'
      open(18,file=input)
      
      if(correlate) then
         write(6,'(
     &        "Disk file of control catalog? ",
     *        "(cr for control_out):)",$)')
         READ(LU(12),'(A)') input
         if(input.eq.' ')input='control_out'
         open(19,file=input)
         write(19,*)break
         WRITE(19,'("  Name       type    ",A,"    sqrt(TS)",
     &        "  Flux+/- 1 sigma    (U.L.)",
     &        "       Cnts   +/- 1 sigma  (U.L.)",
     &        "    Gmult    Gbias  Ranal  Asp. EXP")')
     &        coord_phr
      endif

      write(6,'("Enter known source offset (cr for",
     &     f7.2,"):",$)')Off_set
      READ(LU(12),'(A)') input
      if(input.ne.' ')read(input,*,end=1111)Off_set
      
      write(18,'("EGRET LIKELIHOOD POINT-SOURCE ANALYSIS, ",
     &     "Program version ",a)') VER
      write(18,*)
      write(18,*)
     &     'Measured energy selection:',CTLEMN,' to',CTLEMX
      write(18,'("Counts map: ",a)') cmapfile(1:50)
      write(18,'("Diffuse map: ",a)') gmapfile(1:50)
      WRITE(18,'
     &     ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &     gamma,Ranal
      WRITE(18,'(
     &     "The OTHER PSF map contains:")')
      if(NSOURCE.lt.1) then
         write(18,*)'No sources in other PSF map.'
      else
         write(18,*)
     &        '  #   NAME                  POSITION    ',
     &        '   CNTS   Sp. I.'
         do nsrc=1,NSOURCE
	    theLong=SRC_PARMS(nsrc,1)
	    if(theLong.lt.0.)theLong=theLong+360.
            write(18,'(
     &           i3,3x,A18,2f7.2,f10.1,f6.2,f9.0)')
     &           nsrc,SRC_NAMES(nsrc),theLong,
     &           (SRC_PARMS(nsrc,ip),ip=2,4)
         enddo
      endif
      write(18,'(
     &     "The flux for sources with TS>",f7.2," is presented ",
     &     "as a detection only.",/,
     &     "The flux for sources with",f7.2," <TS<",f7.2," is presented"
     &     ," both as a detection",/,
     &     "and as an upper limit. The flux for sources with TS<",f7.2/
     &     "is presented as an upper limit only.")')
     &     TS_max,TS_min,TS_max,TS_min
      write(18,'(
     &     "The upper limits are at",f7.1,"% confidence corresponding ",
     *     "to a",/,
     &     "drop of lnL of",f7.2," from the value at the counts ",
     *     "estimate (unless the unconstrained counts",/,
     &     "estimate is < 0; then the upper limit is assumed to be ",
     *     "the difference between the statistical limit",
     &     " and the counts estimate.")')
     &     conf_precent,delta_lnL
      if(MAPTYPE.eq.'SINGLE_POINTING')then
         write(18,'(
     &        "Sources are analyzed only if they are within (aspect)",
     *        f7.2," degrees of the axis")')aspect_max
      else
         write(18,'(
     &        "This is analysis of a summed map")')
         write(18,'(
     &        "Pointing direction: ",F7.2,",",F6.2 )')pL,pB
         write(18,'(
     &        "Sources are analyzed only if they are within (aspect)",
     *        f7.2,/
     &        "   degrees of the pointing direction")')aspect_max
      endif
      write(18,'(
     &     "The minimum exposure required is",f6.2," 10^7 cm^2 s")')
     *     exp_min
      write(18,'(
     &     "For catalog entries within",f7.2,
     &     " degrees of a source in the OTHER PSF map,")')Off_set
      write(18,*)
     &     'the source is temporarily removed for the analysis the ',
     *     'entry.'
      write(18,'(
     &     "The fluxes are in units 10^-8 cm^-2 s^-1. The ",
     *     "approximate")')
      write(18,'(
     &     "significance of a detection in units of sigma ",
     *     "is sqrt(TS).")')
      write(18,'(
     &     "The exposure is given in units of 10^7 cm^2s.")')
      write(18,*)break
      WRITE(18,'("  Name       type    ",A,"    sqrt(TS)",
     &     "  Flux+/- 1 sigma    (U.L.)",
     &     "       Cnts   +/- 1 sigma  (U.L.)",
     &     "    Gmult    Gbias  Ranal  Asp. EXP")')
     &     coord_phr
      
      npage=36
      do while('night'.ne.'day')
         read(8,'(A)',end=1)input
         read(input(Inam1:Inam2),'(A)',end=2)srcN
         read(input(Ilong1:Ilong2),*,end=2,err=2)srcL
         read(input(Ilat1:Ilat2),*,end=2,err=2)srcB
         goto 3

 2       continue
         write(6,*)
     &        'Cannot parse catalog entry:'
         write(6,*)input
         if(jae_listproc)then
            write(6,'("Ilong1:",i4," Ilong2:",i4," Ilat1:",i4,
     &           " Ilat2:",i4)') Ilong1,ilong2,ilat1,ilat2
            write(6,*)' '
         endif
         
         goto 4
          
 3       continue
         org_srcL=srcL
         call pixel_select(.false.,' ')
         if(signal.ne.' ')then
            if(jae_listproc)then
               write(6,'("Source ",a," at ",2f7.1," is not on ",
     &              "the map.")') srcN,org_srcL,srcB
               write(*,*)' '
            endif
            signal=' '
            goto 4
         endif
         if(jae_listproc)then
            write(6,'("Source ",a," at ",2f7.1," is not on ",
     &           "the map.")') srcN,org_srcL,srcB
         endif

         aspect=gtcirc(srcL,srcB,pL,pB)
         if(aspect.gt.aspect_max)then
            if(jae_listproc)then
               write(6,'("Source ",a," at ",2f7.1," is too far ",
     &              "off-axis.")') srcN,org_srcL,srcB
               write(6,'("Aspect:",f5.1)') aspect
               write(*,*)' '
            endif
            goto 4
         endif

         expose=mapval(emap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) / (
     &        (sin(pi180*(srcB+ctlscl/2.))-sin(pi180*(srcB-ctlscl/2.)))
     &        *ctlscl*pi180)
         if(expose/1.E7.lt.exp_min) then
            if(jae_listproc)then
               write(6,'("Source ",a," at ",2f7.1," has insufficient",
     &              "exposure.")') srcN,org_srcL,srcB
            endif
            goto 4
         endif

         write(6,'("Will try ",a," at ",2f7.1)') srcN,org_srcL,srcB
         call get_flux(18,Off_set,expose,org_srcL)
         
         if(correlate) then
c     find control point (same b, similar exp., >10 deg)
C     to test the correlation
            expose_org=expose
            Isrcl_org=Isrcl
            srcL_org=srcL
            srcB_org=srcB
            pixel_sol_ang=(
     &           (sin(pi180*(srcB+ctlscl/2.))-
     *           sin(pi180*(srcB-ctlscl/2.)))
     &           *ctlscl*pi180)
            Iskip=10./ctlscl
            expose_diff=0.03
            
 7          continue
            do Isrcl=Isrcl_org+Iskip,ctlmsz1
               expose=mapval(emap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) / 
     &              pixel_sol_ang
               if(abs(expose-expose_org)/expose.le.expose_diff)then
c     this point has equivalent exposure
                  call mapcor(Isrcl,Isrcb,srcL,srcB)
                  call error(1,loc)
                  if(gtcirc(srcL_org,srcB_org,srcL,srcB).gt.10.)then
c     this point is independent
                     goto 6
                  endif
               endif
            enddo
            do Isrcl=1,Isrcl_org-Iskip
               expose=mapval(emap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) / 
     &              pixel_sol_ang
               if(abs(expose-expose_org)/expose.le.expose_diff)then
c     this point has equivalent exposure
                  call mapcor(Isrcl,Isrcb,srcL,srcB)
                  call error(1,loc)
                  if(gtcirc(srcL_org,srcB_org,srcL,srcB).gt.10.)then
c     this point is independent
                     goto 6
                  endif
               endif
            enddo
            if(expose_diff.lt.0.50) then
               expose_diff=expose_diff+0.03
               goto 7
            endif
            WRITE(19,'(A16,
     &           "   No independent point with equivalent exposure ",
     *           "found.")')
     &           srcN
            goto 4
            

 6          continue
            srcL=srcL+LSHIFT    ! same pixel offset as test point
            srcB=srcB+BSHIFT
            call pixel_select(.false.,' ')
            call error(1,loc)
            
            write(6,'("Will try control for ",a," at ",
     &           2f7.1)') srcN,srcL,srcB 
            call get_flux(19,Off_set,expose,org_srcL)
c     end control point analysis
         endif
         
 4       continue
      enddo
      
 1    continue
      close(8)
      close(18)
      close(19)
      srcL=initL
      srcB=initB
      call pixel_select(.false.,' ')
      call error(0,loc)
      RETURN

 1111 write(6,*)'Invalid input, try again.'
      RETURN
      END
