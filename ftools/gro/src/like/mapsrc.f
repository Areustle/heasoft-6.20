C       SUBROUTINE MAPSRC
C
C
C  $Id: mapsrc.f,v 1.4 2013/05/21 19:08:26 irby Exp $
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
C  $Log: mapsrc.f,v $
C  Revision 1.4  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2002/12/26 17:33:18  irby
C  - Change negative exponents to e.g. -1*number instead of -number for f90
C    compatibility (and put in parens (-1) where necessary.
C
C  - Fix write statements (commas).
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
C  Revision 1.1  2002/04/16 20:27:40  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/05/08  18:27:34  jae
c added line for output when jae_exp_anal=TRUE
c
c Revision 5.1  1996/02/29  20:52:04  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:21  jae
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
      SUBROUTINE MAPSRC
C  Common blocks used:
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/nmprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/fitrep.copy'

      save

      character(80) id
      common /id/id
      REAL MAPVAL               ! map value function
      character input*50
      LOGICAL determined,FEXIST,mapcnts,speedmap

      id = '$Id: mapsrc.f,v 1.4 2013/05/21 19:08:26 irby Exp $'
      LOC='MAPSRC  '


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

      speedmap=.false.
      write(*,'("Fast Mapping ? (default=N): Y or N :"$)')
      read(LU(12),'(a)') input
      numcar = index(input, ' ') - 1

      if (numcar.gt.0.and.(input(1:1).eq.'y'.or.input(1:1).eq.
     &     'Y')) then
 4       speedmap=.true.
         tmpc=2
         input='                    '
         write(*,'("Set scale size(deg) [default = 2]: "$)')
         read(LU(12),'(a)') input
         numcar = index(input, ' ') - 1

         if (numcar.eq.0) goto 6

         read(input,*,err=5,end=5)tmpc

         if (tmpc.gt.20.001.or.tmpc.le.2*ctlscl) goto 5

         goto 6
 5       print *,'invalid input: scale size: ',tmpc

         if (tmpc.gt.20.001) print *,' scale size too big (<= 20)'
         if (tmpc.le.2*ctlscl) then
	    print *,'Scale size too low. Must be >=',2*ctlscl
         endif

         print *,' '
         print *,'Try again or enter A to abort'
         goto 4
 6       ntmpc=tmpc/ctlscl
      endif
c
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
      
      report=.false.
      report2=.false.
      calc_uncert=.false.

      write(lu(1),'("Will map point source TS for each pixel in",
     &     " the ROI, which is:")')
      if (coord_sys.eq.'C') then
         write(6,'("RA.",f8.3," to",f8.3,"; Dec.",f8.3," to",
     &        f8.3)') ROIORG(1),ROIEND(1),ROIORG(2),ROIEND(2)
      else
         write(6,*)'Long.',ROIORG(1),' to',ROIEND(1),
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
         write(6,*)
     &        'number   POSITION              CNTS       Spec. Index'
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
      call intent(determined)

      if (.not.determined) return

      if (Ranal.gt.20.) then
         write(lu(1),'("This value of Ranal will cause this job",
     &        " to take a long time.")')
         call intent(determined)
         if(.not.determined)return
      endif

      call exp_anal(aspect_max,exp_min)

      if (coord_sys.eq.'C') then
         pL=SC_RA
         pB=SC_DEC
      else
         pL=SC_LII
         pB=SC_BII
      endif

      write(6,'(
     &     "Do you want to save Gbias",
     &     " or Counts in the GBIASFILE (cr for Gbias)?",$)')

      READ(LU(12),'(A)') input
      if (input.eq.' ') then
         mapcnts=.false.
      else
         mapcnts=.true.
      endif

C     Clear out maps:
      CALL MAPRST(XRRMAP,CTLMSZ1,CTLMSZ2)
      CALL MAPRST(TMPMAP,CTLMSZ1,CTLMSZ2)
      CALL MAPRST(nmap,CTLMSZ1,CTLMSZ2)
      lshift=0.
      bshift=0.
      old_lat=0.
      Counts=100.
      Gmult=1.
      Gbias=1.

      DO 80 IsrcB=ROIPRG(2),ROIPND(2) ! do latitude row
         CALL MAPCOR (ROIPRG(1),IsrcB,srcL,srcB)
         CALL ERROR(1,LOC)
         the_lat=cos(srcB*PI180)**(-1)

         if (abs(the_lat-old_lat).gt.0.01) then 
c     need to adjust PSF to CLAT
	    CLAT=srcB
ccc   
c
c     JAE 8-19-94  DEC Esc codes in next two writes
c
ccc
c     write(*,'(A1,"[2J",A1"[00H")')char(27),char(27)
c     write(*,'(A1,"[5;1H")')CHAR(27)
	    write(lu(1),'("MAPSRC: Rebinning PSF for latitude=",
     &           f7.2)')CLAT
	    CALL psmmat(lshift,bshift)
            CALL ERROR(1,LOC)
	    old_lat=the_lat
         endif

         IsrcL=ROIPRG(1)        ! begin longitude loop
 100     CONTINUE
         CALL MAPCOR (IsrcL,IsrcB,srcLpxcnt,srcBpxcnt)
         CALL ERROR(1,LOC)
c
         srcL=srcLpxcnt
         srcB=srcBpxcnt
         LikTotaled=.false.

         if (MAPTYPE.eq.'SINGLE_POINTING') then
	    aspect=gtcirc(srcL,srcB,pL,pB)
            if(aspect.gt.aspect_max)then
c     aspect not OK
               GOTO 101
	    endif
         endif

         expose=mapval(emap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) / (
     &        (sin(pi180*(srcB+ctlscl/2.))-sin(pi180*(srcB-ctlscl/2.)))
     &        *ctlscl*pi180)

         if (expose/1.E7.lt.exp_min) then
	    if (jae_exp_anal)write(*,'("Low exposure:",f10.7)')
     *           expose/1.e7,
     &           ' < ',exp_min,' at:', srcL,' ',srcB
c     there is not enough exposure here
	    GOTO 101
         endif

ccc   JAE FIX to Speed up this routine 
c     18-8-94 JAE
ccc   

         if (.not.speedmap) then
            call SRCTEST(.false.)
         else
ccc   
c     
c     JAE 8-19-94  DEC Esc codes in next four writes
c     
ccc   
c     write(*,'(A1,"[2;1H"$)')CHAR(27)
c     write(*,'(A1,"[2K")')CHAR(27)
c     write(*,'(A1,"[2;10H"$)')CHAR(27)
c     write(*,'(i4,1x,i4," "$)')IsrcL,IsrcB

            if (mapval(tmpmap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2).gt.
     &           1.90e-10) goto 101

            if (MOD(IsrcL-ROIPRG(1),ntmpc).eq.0) then
               call SRCTEST(.false.)
               source_notlnL = -pss_not_lnL
            else
               CALL CNTTEST(.false.)

               if (signal.ne.' ')then
                  CALL ERROR(0,LOC)
                  TS=0
                  goto 12
               endif

               source_lnL = -lnL
               TS = 2.*(source_lnL-source_notlnL)

 12            if (TS.le.1.90e-10)TS=2.0e-10
            endif

            if (TS.lt.0.25) then
               JL1 = MAX(ROIPRG(1),IsrcL-1)
               JL2 = MIN(ROIPND(1),IsrcL+1)
               JB1 = IsrcB
               JB2 = MIN(ROIPND(2),IsrcB+1)

               do JB=JB1,JB2
	  	  do JL=JL1,JL2
c     if(JL.ge.IsrcL.or.JB.eq.IsrcB+1)then
                     if (JB.le.ROIPND(2).and.JL.le.ROIPND(1)) then
                        call VALMAP(TS,TMPMAP,JL,JB,CTLMSZ1,CTLMSZ2) 
                        call VALMAP(Gmult,nmap,JL,JB,CTLMSZ1,CTLMSZ2) 
                        if (mapcnts) then
                           call VALMAP(Counts,XRRMAP,JL,JB,CTLMSZ1,
     *                          CTLMSZ2) 
                        else
                           call VALMAP(Gbias,XRRMAP,JL,JB,CTLMSZ1,
     *                          CTLMSZ2) 
                        endif
                     endif
	  	  enddo
               enddo	
               goto 101
            endif
c     
c     End of else when if(speedmap) is valid
c     
         endif
c     
c     End of JAE fixup 18-8-94
cccc  
         if (signal.ne.' ') then
	    write(6,'("Error condition detected at",2f6.1)') 
     *           srcL,srcB
            CALL ERROR(0,LOC)
	    write(6,*)
     &           'Will write 0 to TS and background maps at this point.'
         else
            call VALMAP(TS,TMPMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 
            call VALMAP(Gmult,nmap,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 

	    if (mapcnts) then
               call VALMAP(Counts,XRRMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 
            else
               call VALMAP(Gbias,XRRMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2) 
	    endif
         endif
         
c     !DO ALL COLUMNS IN THE ROI
         
 101     CALL MAPINC(IsrcL,IEND,ROIPRG(1),ROIPND(1)) 
         CALL ERROR(1,LOC)
         IF(IEND.NE.1) GOTO 100
 80   CONTINUE                  ! end latitude loop
      
      CALL MAPMAX(TMPMAP,IsrcL,IsrcB,SIGNIF,CTLMSZ1,CTLMSZ2)
      CALL MAPCOR(IsrcL,IsrcB,srcLpxcnt,srcBpxcnt)
      CALL ERROR(1,LOC)
      srcL=srcLpxcnt
      srcB=srcBpxcnt
      WRITE(LU(1),'("HIGHEST SIGNIFICANCE (TS=",f7.1,
     &     ") IS AT: ",2f6.1)') SIGNIF,srcL,srcB
      WRITE(LU(1),*)'Use L option for more information'
      CLAT=srcB
      CALL psmmat(lshift,bshift)
      LikTotaled=.false.

      write(TMPDOC(2),'("Counts map: ",a)') cmapfile(1:58)
      write(TMPDOC(3),'("Diffuse map: ",a)') gmapfile(1:57)
      WRITE(TMPDOC(4),'
     &     ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &     gamma,Ranal

      if (NSOURCE.eq.0)then
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

      write(TMPDOC(1),'(a,"- v ",a," map of EGRET ",
     &     "point-source test statistic")')PRG,VER
      TMPTYP = 'LSTA'
      MAPFILE=LMAPFILE
      CALL MAPRITroi(TMPMAP,TMPTYP,TMPDOC,BMAP)
      CALL ERROR(1,LOC)

      if (mapcnts) then
         write(XRRDOC(1),'(a,"- v ",a," map of model point ",
     &        "source counts")') PRG,VER
         XRRTYP = 'CNTS'
         MAPFILE=GBIASFILE
         CALL MAPRITroi(XRRMAP,XRRTYP,XRRDOC,BMAP)
         CALL ERROR(1,LOC)
      else
         write(XRRDOC(1),'(a,"- v ",a," map of isotropic ",
     &        "diffuse component")')PRG,VER
         XRRTYP = 'GBIA'
         MAPFILE=GBIASFILE
         CALL MAPRITroi(XRRMAP,XRRTYP,XRRDOC,BMAP)
         CALL ERROR(1,LOC)
      endif

      write(XRRDOC(1),'(a,"- v ",a," map of diffuse model ",
     &     "multiplier")')PRG,VER
      XRRTYP = 'GMUL'
      MAPFILE=NEWMAPFILE
      CALL MAPRITroi(nmap,XRRTYP,XRRDOC,BMAP)
      CALL ERROR(1,LOC)

c     restore BMAP
      CALL PSFBLD
      CALL ERROR(1,LOC)
      
      RETURN
      
 1111 write(lu(1),*)'Invalid input, try again.'
      return
      END
