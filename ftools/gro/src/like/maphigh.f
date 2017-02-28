C       SUBROUTINE MAPHIGH
C
C
C  $Id: maphigh.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++            effect: the version 3 source test is applied to MAP
C++                    over the region of interest.
C++                    A map of Gbias, Counts, and TS is written.
C=======================================================================
C LIKE Version: 4.1  DELIVERED: April 15 1993      JMATTOX
C+            Updated: by JRM
C=======================================================================
C  $Log: maphigh.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/12/26 17:28:04  irby
C  Change negative exponents to e.g. -1*number instead of -number for f90
C  compatibility (and put in parens (-1) where necessary).
C
C  Revision 1.1  2002/04/16 20:27:38  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:53  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:06  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPHIGH
C  Common blocks used:
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/nmprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/fitrep.copy'

      save

      character(80) id
      common /id/id
      logical determined
      character input*50

      id = '$Id: maphigh.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
      LOC='MAPHIGH'

      print *,
     &     'TS will be written to LMAPFILE ',
     &     'Counts to NEWMAPFILE ',
     &     'and Gbias to ',
     &     'GBIASFILE'
      report=.false.
      report2=.false.
      calc_uncert=.false.

      write(lu(1),'("Will find TS, counts, and Gbias",
     &     " for each pixel in",
     &     " the ROI, which is:")')

      if (coord_sys.eq.'C') then
         write(6,'("RA.",f8.3," to",f8.3,"; Dec.",f8.3," to",
     &        f8.3)') ROIORG(1),ROIEND(1),ROIORG(2),ROIEND(2)
      else
         write(6,'("Long.",f8.3," to",f8.3,"; Lat.",f8.3," to",
     &        f8.3)') ROIORG(1),ROIEND(1),ROIORG(2),ROIEND(2)
      endif

      call intent(determined)

      if (.not.determined) return

      if (Ranal.gt.20.) then
         write(lu(1),'("This value of Ranal will cause this job",
     &        " to take a long time.")')
         call intent(determined)
         if(.not.determined)return
      endif

      write(lu(1),'("Gmult will be fixed for this map")')
      write(lu(1),'("Input Gmult (cr for",e11.3,"):",$)')Gmult
      READ(LU(12),'(A)')input

      if (input.ne.' ') read(input,*,end=1111)Gmult

      call exp_anal(aspect_max,exp_min)

      if (coord_sys.eq.'C') then
         pL=SC_RA
         pB=SC_DEC
      else
         pL=SC_LII
         pB=SC_BII
      endif

C     Clear out maps:
      CALL MAPRST(TMPMAP,CTLMSZ1,CTLMSZ2)
      CALL MAPRST(XRRMAP,CTLMSZ1,CTLMSZ2)
      CALL MAPRST(NMAP,CTLMSZ1,CTLMSZ2)
      lshift=0.
      bshift=0.
      old_lat=0.

      DO 80 IsrcB=ROIPRG(2),ROIPND(2) ! do latitude row
         CALL MAPCOR (ROIPRG(1),IsrcB,srcL,srcB)
         CALL ERROR(1,LOC)
         the_lat=cos(srcB*PI180)**(-1)

         if (abs(the_lat-old_lat).gt.0.01) then 
c     need to adjust PSF to CLAT
	    CLAT=srcB
	    write(lu(1),'("MAPSRC: Rebinning PSF for latitude=",
     &           f7.1)') CLAT
	    CALL psmmat(lshift,bshift)
            CALL ERROR(1,LOC)
	    old_lat=the_lat
         endif

         IsrcL=ROIPRG(1)        ! begin longitude loop
 100     CONTINUE
         CALL MAPCOR (IsrcL,IsrcB,srcLpxcnt,srcBpxcnt)
         CALL ERROR(1,LOC)
         srcL=srcLpxcnt
         srcB=srcBpxcnt
         LikTotaled=.false.

         if (MAPTYPE.eq.'SINGLE_POINTING') then
	    aspect=gtcirc(srcL,srcB,pL,pB)
            if (aspect.gt.aspect_max) then
c     aspect not OK
               GOTO 101
	    endif
         endif

         counts_est=counts
         Counts=0.
         call biastest(.false.)

         if (signal.eq.' ') then
	    the_lnL_noc=lnL
	    counts=counts_est
	    call CNTSbias(.false.)
         endif

         if (signal.ne.' ') then
	    write(6,'("Error condition detected at",2f7.1)') 
     &           srcL,srcB
            CALL ERROR(0,LOC)
	    write(6,*)
     &           'Will write 0 to the maps at this point.'
         else
	    TS=2.*(the_lnL_noc-lnL)
	    if(Counts.le.0.)TS=0.
	    if(TS.lt.0.)TS=0.
            call VALMAP(TS,TMPMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 
            call VALMAP(Gbias,XRRMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 
            call VALMAP(Counts,NMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 
         endif

 101     CALL MAPINC(IsrcL,IEND,ROIPRG(1),ROIPND(1)) !DO ALL COLUMNS IN THE ROI
         CALL ERROR(1,LOC)
         IF(IEND.NE.1) GOTO 100
 80   CONTINUE                  ! end latitude loop
      
C     Set map types
      TMPTYP = 'GMUL'
      write(TMPDOC(1),*)
     &     'LIKE_V4 map of diffuse model multiplier.'
      
      write(TMPDOC(2),'("Counts map: ",a)') cmapfile(1:58)
      write(TMPDOC(3),'("Diffuse map: ",a)') gmapfile(1:57)
      write(TMPDOC(4),'("Map made with Gmult fixed at",f5.1)') Gmult
      WRITE(TMPDOC(5),'
     &     ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &     gamma,Ranal

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
      
      write(TMPDOC(1),'(a,"- v ",a," map of EGRET point-source ",
     &     "test statistic")') PRG,VER
      
      TMPTYP = 'LSTA'
      MAPFILE=LMAPFILE
      CALL MAPRITroi(TMPMAP,TMPTYP,TMPDOC,BMAP)
      CALL ERROR(1,LOC)

      write(TMPDOC(1),'(a,"- v ",a," map of isotropic diffuse ",
     &     "component")') PRG,VER
      TMPTYP = 'GBIA'
      MAPFILE=GBIASFILE
      CALL MAPRITroi(XRRMAP,TMPTYP,TMPDOC,BMAP)
      CALL ERROR(1,LOC)


      write(TMPDOC(1),'(a,"- v ",a," map of Counts")')PRG,VER
      TMPTYP = 'CNTS'
      MAPFILE=NEWMAPFILE
      CALL MAPRITroi(nmap,TMPTYP,TMPDOC,BMAP)
      CALL ERROR(1,LOC)

c     restore BMAP
      CALL PSFBLD
      CALL ERROR(1,LOC)
      
      RETURN
      
 1111 write(lu(1),*)'Invalid input, try again.'
      return
      END
