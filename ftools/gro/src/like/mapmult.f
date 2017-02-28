C       SUBROUTINE MAPMULT
C
C
C  $Id: mapmult.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++            effect: the gastest function is applied to MAP
C++                    over the region of interest to estimate Gmult.
C++                    A map of Gmult is written.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C  $Log: mapmult.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/12/26 17:28:05  irby
C  Change negative exponents to e.g. -1*number instead of -number for f90
C  compatibility (and put in parens (-1) where necessary).
C
C  Revision 1.1  2002/04/16 20:27:38  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:58  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:13  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPMULT
C  Common blocks used:
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/nmprep.copy'
      INCLUDE  '../COMMON/fitrep.copy'

      save

      character(80) id
      common /id/id
      logical determined
      character input*50

      id = '$Id: mapmult.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
      LOC='MAPMULT'

      print *,
     &     'Counts will be written to NEWMAPFILE, and Gmult to ',
     &     'GBIASFILE'


      report=.false.
      report2=.false.
      calc_uncert=.false.

      write(lu(1),'("Will map Gmult for each pixel in",
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

      write(lu(1),'("Input Gbias (cr for",e11.3,"):",$)')Gbias
      READ(LU(12),'(A)')input

      if (input.ne.' ') read(input,*,end=1111)Gbias

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
            if (aspect.gt.aspect_max)then
c     aspect not OK
               GOTO 101
	    endif
         endif

         call GASCNTS(.false.)

         if (signal.ne.' ') then
	    write(6,'("Error condition detected at",2f7.2)')
     &           srcL,srcB
            CALL ERROR(0,LOC)
	    write(6,*)
     &           'Will write 0 to the maps at this point.'
         else
            call VALMAP(Gmult,TMPMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 
            call VALMAP(Counts,NMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 
         endif

 101     CALL MAPINC(IsrcL,IEND,ROIPRG(1),ROIPND(1)) !DO ALL COLUMNS IN THE ROI
         CALL ERROR(1,LOC)
         IF(IEND.NE.1) GOTO 100
 80   CONTINUE                  ! end latitude loop

      write(TMPDOC(2),'("Counts map: ",a)') cmapfile(1:58)
      write(TMPDOC(3),'("Diffuse map: ",a)') gmapfile(1:57)
      write(TMPDOC(4),'("Map made with Gbias fixed at",
     &     f6.1)')Gbias
      WRITE(TMPDOC(5),'
     &     ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &     gamma,Ranal

      if (NSOURCE.eq.0) then
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

      write(TMPDOC(1),'(a,"- v ",a)') PRG,VER,
     &     ' map of Counts'
      TMPTYP = 'CNTS'
      MAPFILE=NEWMAPFILE
      CALL MAPRITroi(nmap,TMPTYP,TMPDOC,BMAP)
      CALL ERROR(1,LOC)
      
      write(TMPDOC(1),'(a,"- v ",a)') PRG,VER,
     &     ' map of diffuse model multiplier'
      TMPTYP = 'GMUL'
      MAPFILE=GBIASFILE
      CALL MAPRITroi(TMPMAP,TMPTYP,TMPDOC,BMAP)
      CALL ERROR(1,LOC)

      RETURN
 1111 write(lu(1),*)'Invalid input, try again.'

      return
      END

