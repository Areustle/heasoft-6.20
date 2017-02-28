C       SUBROUTINE MAPFINE
C
C
C  $Id: mapfine.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++            Effect: the likelihood operator is applied with 
C++                    a shifted PSF to obtain a fine map of
C++                    likelihood versis position which is
C++                    written to LMAPFILE.
C
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C
C=======================================================================
C  $Log: mapfine.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/12/26 17:42:55  irby
C  Fix read/write statements (for f90 compatibility): add/remove commas and
C  fix lines over 72 chars.
C
C  Revision 1.1  2002/04/16 20:27:38  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/02/29  21:08:15  jae
c no changes done
c
c Revision 5.1  1996/02/29  20:51:52  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:05  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPFINE
C  Common blocks used:
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/psfrep.copy'

      save

      character(80) id
      common /id/id
      REAL tmp_CTLORG(2)
      character input*50,break*80
      logical determined,counts_fixed,FEXIST
C
      id = '$Id: mapfine.f,v 1.3 2013/05/21 19:08:26 irby Exp $'

      LOC='MAPFINE '
      data break/
     &'--------------------------------------------------------------'/
C
      INQUIRE (FILE=LMAPFILE,EXIST=FEXIST)
      IF (FEXIST) THEN
         WRITE(lu(1),'(
     &        "Output FITS file, LMAPFILE exits.",
     &        " Do you want to remove it?")')
         input='/bin/rm -i '//LMAPFILE
         call system(input)
      ENDIF

      call pixel_select(.true.,'Finemap center      ')
      if (signal.ne.' ') return

      call L_check(srcL)
      write(6,'(
     &     "Do you want to calculate Counts, ",
     &     "Gmult, and Gbias once at fine map center, ")')
      write(6,'(
     &     "or at each point - which takes ~10 times longer but",/,
     &     " is more accurate at edges ",
     *     "(cr for once at map center)?",$)')
      READ(LU(12),'(A)')input
      if (input.eq.' ') then
         counts_fixed=.true.
      else
         counts_fixed=.false.
      endif

      if (counts_fixed) then
         if (EMAPFILE.eq.'unity') then
c     no exposure map
	    source_notlnL=0.
         else
            calc_uncert=.true.
	    report=.true.
	    Counts_previous=Counts
	    Counts=0.
	    WRITE(6,*)break
	    write(6,*)
     &           'Solving for lnL with Counts=0 for entire fine map.'

	    if (gmap_null) then
               Gmult=0.
               call BIASTEST(.false.)
            else
               call gasbias(.false.)
	    endif

	    if (signal.ne.' ') return

	    source_notlnL=-lnL
	    Counts=Counts_previous
	    WRITE(6,*)break
	    write(6,'("Solving for Counts, Gmult, and Gbias ",
     &           "to be used for entire fine map.")')

	    if (gmap_null) then
               call CNTSBIAS(.false.)
            else
               call allfit(.false.)
	    endif

	    if (signal.ne.' ') return

	    WRITE(6,*)break
         endif

         write(lu(1),'("Input Counts (cr for",e14.5,"):",$)')Counts
         READ(LU(12),'(A)')input

         if (input.ne.' ') read(input,*,end=1111)Counts

         write(lu(1),'("Input Gbias (cr for",e15.5,"):",$)')Gbias
         READ(LU(12),'(A)')input

         if (input.ne.' ') read(input,*,end=1111)Gbias

         write(lu(1),'("Input Gmult (cr for",e15.5,"):",$)')Gmult
         READ(LU(12),'(A)')input

         if (input.ne.' ') read(input,*,end=1111)Gmult

c     end counts_fixed if clause
      else
         report=.false.
         report2=.false.
         calc_uncert=.false.
      endif

 10   write(lu(1),'(
     &     "How wide to make fine map (degrees)?",$)')
      READ(LU(12),'(A)')input
      read(input,*,end=10)diameter

      if (diameter.gt.10.*CTLSCL) then
         write(6,*)
     &        'Requested fine map spans more than 10 pixels. ',
     *        'Use MS instead.'
         call intent(determined)
         if(.not.determined)return
      endif

      psf_rad=Ranal
      if (psf_rad.gt.20.) psf_rad=20.
      if (diameter.gt.psf_rad*1.333) then
         write(6,'("Fine map extends more than 66% of Ranal ",/,
     &        "radius. Position might be biased due to PSF leakage ",
     *        "out of",/,
     &        " Ranal region. Perhaps you should return to main menu ",
     *        "and",/,
     &        " increase Ranal or decrease fine map width.")')
         call intent(determined)
         if(.not.determined)return
      endif

 20   write(lu(1),'("
     &     How many divisions in each dimension?",$)')
      READ(LU(12),'(A)')input
      read(input,*,end=20)Npoints
      width=diameter/float(Npoints)
      cornerL=srcL-diameter/2.
      cornerB=srcB-diameter/2.
      write(lu(1),'("Mapping the region:",f7.2,
     &     " < L < ",f7.2,", ",f7.2," < B < ",f7.2,"; with ",
     &     i3," X ",i3," pixels.")')
     &     cornerL,cornerL+diameter,cornerB,cornerB+diameter,
     &     Npoints,Npoints
      
      xlmax=-1.e12
C     Do likelihood ratio analysis of these points:
      DO  I = 1,Npoints+1
         PL = cornerL + (I-1) * width
         DO  J = 1,Npoints+1
            PB = cornerB + (J-1) * width
	    call psmmat(PL-srcLpxcnt,PB-srcBpxcnt)
	    if (signal.ne.' ') return
	    LikTotaled=.false.
  	    IMAP=I+ (Npoints+1)*(J-1)

	    if (counts_fixed) then
               call liktot(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
               if(signal.ne.' ')return
               call HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
               if(signal.ne.' ')return
               TMPMAP(IMAP) = 2.*(-lnL-source_notlnL)
            else
               call SRCTEST(.false.)
               TMPMAP(IMAP) = TS
	    endif

	    if (signal.ne.' ') then
               write(6,*)'Error condition detected. Abort mapping.'
               CALL ERROR(0,LOC)
               return
	    endif

	    if (xlmax.lt.TMPMAP(IMAP)) then
               xlmax=TMPMAP(IMAP)
               xmaxb=PB
               xmaxl=PL
	    endif
         ENDDO
      ENDDO

      write(lu(1),'("Maximum TS =",f7.1,
     &     "; found at L=",f6.1,"; B=",f6.1)') xlmax,xmaxl,xmaxb
      
      srcL=xmaxl
      srcB=xmaxb
      call pixel_select(.false.,' ')
      call error(1,loc)       

C     Save CTL data
      tmp_CTLSCL=CTLSCL
      tmp_CTLMSZ1=CTLMSZ1
      tmp_CTLMSZ2=CTLMSZ2
      tmp_CTLORG(1)=CTLORG(1)
      tmp_CTLORG(2)=CTLORG(2)

C     Reset CTL data for finemap
      CTLSCL=width
      CTLMSZ1=Npoints+1
      CTLMSZ2=Npoints+1
      CTLORG(1)=cornerL
      CTLORG(2)=cornerB

C     Write LMAP
      if (srcN.ne.' ')then
         write(TMPDOC(1),'("LIKE - v ",a)') VER,
     &        ' fine map of TS for ',srcN
      else
         write(TMPDOC(1),'(a,"- v ",a," fine map of TS")') PRG,VER
      endif

      write(TMPDOC(2),'("Counts map: ",a)') cmapfile(1:58)
      write(TMPDOC(3),'("Diffuse map: ",a)') gmapfile(1:57)
      WRITE(TMPDOC(4),'
     &     ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &     gamma,Ranal

      if (counts_fixed) then
         WRITE(TMPDOC(5),'
     &        ("Gmult,Gbias,Counts fixed at:"
     &        ,f10.4,f10.4,f12.4)')
     &        Gmult,Gbias,Counts
      else
         WRITE(TMPDOC(5),'
     &        ("Gmult,Gbias,Counts optimized at each point.")')
      endif

      if (NSOURCE.eq.0)then
         TMPDOC(6)='There are no PSFs modeled.'
      else
         write(TMPDOC(6),'(
     &        "There are ",I3," PSFs modeled. First:    ")')NSOURCE
         write(TMPDOC(6)(39:70),'(
     &        "At",f7.2,",",f6.2," are",f5.0," cnts; ")')
     &        SRC_PARMS(1,1),SRC_PARMS(1,2),SRC_PARMS(1,3)
      endif
 
      do i=7,10
         TMPDOC(i)=' '
      enddo

C     Set map type to indicate likelihood map:
      TMPTYP = 'LSTA'
      MAPFILE=LMAPFILE
      CALL MAPRIT(TMPMAP,TMPTYP,TMPDOC,CTLMSZ1,CTLMSZ2)
      CALL ERROR(1,LOC)

C     Reset CTL data
      CTLSCL=tmp_CTLSCL
      CTLMSZ1=tmp_CTLMSZ1
      CTLMSZ2=tmp_CTLMSZ2
      CTLORG(1)=tmp_CTLORG(1)
      CTLORG(2)=tmp_CTLORG(2)

      SIGNAL = ' '

      return
 1111 write(lu(1),*)'Invalid input, try again.'

      return
      END
