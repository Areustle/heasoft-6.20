C       SUBROUTINE RESIDUE(input,determined)
C
C
C  $Id: residue.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++     Effect: Write the counts minus the model to a residue map
C
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     character input   Command text typed by user on console
c     logical   flag    User input requested on flag=.true.
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C
C=======================================================================
C  $Log: residue.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
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
C  Revision 1.1  2002/04/16 20:27:43  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:53:11  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:01  jae
c Subroutine Module for like V5.00
c
C%   Changes:                                                           
c
c	Sept. 11, 1994 JAE
c		Added input variable as determined for user input.  
c		All routines which call residue have been updated with
c		the new call parameters (char* 50, logical).
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE RESIDUE(input,determined)
C
C  Common blocks used:
C
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/nmprep.copy'

      save

      REAL          MAPVAL
      character(80)  id
      character(50)  input, moreinput
      LOGICAL       determined
      
      common /id/id

      
      id = '$Id: residue.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
      LOC='RESIDUE'


      write(6,'("Will write residual map (EGRET data - model)",
     &     " for the ROI, which is:")')
      if (coord_sys.eq.'C') then
         write(6,'("RA.",f8.3," to",f8.3,"; Dec.",f8.3," to",
     &        f8.3)') ROIORG(1),ROIEND(1),ROIORG(2),ROIEND(2)
      else
         write(6,'("Long.",f8.3," to",f8.3,"; Lat.",f8.3," to",
     &        f8.3)') ROIORG(1),ROIEND(1),ROIORG(2),ROIEND(2)
      endif

      if (determined) then
         call intent(determined)
         if (.not.determined) return
      endif

      write(6,*) 'The model includes Gmult*diffuse map + '
      write(6,*) 'Gbias + other PSF matrix.'
      write(6,'("Gmult=",f12.6," and Gbias=",f12.6)')Gmult,Gbias
c
c     first gather together the model
c     add counts due to Gm*GMAP
c
      CALL MAPRST(tmpmap,CTLMSZ1,CTLMSZ2)
      CALL MAPADD(Gmult,tmpmap,CTLMSZ1,CTLMSZ2)
      CALL MAPMLT(tmpmap,GASMAP,CTLMSZ1,CTLMSZ2)
c
c     add counts due to Gb*EMAP
c     
      CALL MAPRST(xrrmap,CTLMSZ1,CTLMSZ2)
      CALL MAPADD(Gbias*1.e-5,xrrmap,CTLMSZ1,CTLMSZ2)
      CALL MAPMLT(xrrmap,EMAP,CTLMSZ1,CTLMSZ2)
      CALL MAPSUM(tmpmap,xrrmap,CTLMSZ1,CTLMSZ2)
c
c     add counts due to other PSF matrix
c     
      CALL MAPSUM(tmpmap,bmap,CTLMSZ1,CTLMSZ2)
c     
c     now get the gamma ray data
c
      CALL MAPRST(xrrmap,CTLMSZ1,CTLMSZ2)
      CALL MAPSUM(xrrmap,map,CTLMSZ1,CTLMSZ2)
c
c     now subtract the model
c
      CALL MAPSUB(xrrmap,tmpmap,CTLMSZ1,CTLMSZ2)
        
      if (input(4:4).eq.'c'.or.input(4:4).eq.'C') then
c
c     write residual without normalization
c
         goto 81
      endif
c
c     now do final calculations (only for the ROI)
c
      DO 80 JB=ROIPRG(2),ROIPND(2) ! do latitude row
         JL=ROIPRG(1)           ! begin longitude loop
 100     CONTINUE

         if (input(4:4).eq.'f'.or.input(4:4).eq.'F') then
c
c     divide by the exposure
c
            exposure_val=MAPVAL(emap,JL,JB,CTLMSZ1,CTLMSZ2)
            if (exposure_val.ne.0.) then
               err_norm = MAPVAL(xrrmap,JL,JB,CTLMSZ1,CTLMSZ2)
     &              /exposure_val
            else
               err_norm = 0.
            endif

            call VALMAP(err_norm,xrrmap,JL,JB,CTLMSZ1,CTLMSZ2)
         else
c
c     normalize residual with error
c
            cnts_error=MAPVAL(tmpmap,JL,JB,CTLMSZ1,CTLMSZ2)
            if (cnts_error.gt.1.) then
               cnts_error=sqrt(cnts_error)
            else
               cnts_error=1.
            endif

            err_norm = MAPVAL(xrrmap,JL,JB,CTLMSZ1,CTLMSZ2)
     &           /cnts_error
            call VALMAP(err_norm,xrrmap,JL,JB,CTLMSZ1,CTLMSZ2)
         endif

         CALL MAPINC(JL,IEND,ROIPRG(1),ROIPND(1)) ! DO ALL COLUMNS IN THE ROI
         IF (SIGNAL.NE.' ') CALL ERROR(1,LOC)
         IF (IEND.NE.1) GOTO 100
 80   CONTINUE                  ! end latitude loop
 81   CONTINUE

      XRRTYP='RCMP'
      
      do i=1,10
         XRRDOC(i)=' '
      enddo

      write(XRRDOC(1), '(
     &     "Fit residual:Gmult=",f6.3,"applied;Gbias=",
     &     f6.3,"*1e-5 added.")')Gmult,Gbias
	  
      write(XRRDOC(2),'("Counts map: ",a)')
     %     cmapfile(1:len_trim(cmapfile))
      write(XRRDOC(3),'("Diffuse map: ",a)') 
     *     gmapfile(1:len_trim(gmapfile))
      write(XRRDOC(4),'(i3," PSFs in model. ")')NSOURCE
      if (input(4:4).eq.'c'.or.input(4:4).eq.'C') then
         write(moreinput,*)
     &        'Residual written without normalization.'
         nchar=38
         call DOCADD(nchar,XRRDOC,XRRTYP,moreinput)
         write(6,*)moreinput
      else if (input(4:4).eq.'f'.or.input(4:4).eq.'F') then
         XRRTYP='RFMP'
         write(moreinput,*)'Residual flux written.  '
         nchar=29
         call DOCADD(nchar,XRRDOC,XRRTYP,moreinput)
         write(6,*)moreinput
      else
         XRRTYP='RMAP'
         write(moreinput,*)'Normalized residual written.'
         nchar=29
         call DOCADD(nchar,XRRDOC,XRRTYP,moreinput)
         write(6,*)moreinput
         write(6,*)'Normalized by dividing by sqrt(model counts),'
         write(6,*)'or 1 (which ever is larger.'
      endif

      if (nsource.gt.0) then
c
c     SHOULD call DOCADD for PSF values
c
      endif

      MAPFILE=NEWMAPFILE
      write(6,'("Writing ",a)') XRRTYP
      CALL MAPRITROI(XRRMAP,XRRTYP,XRRDOC,NMAP)
      CALL ERROR(0,LOC)

      RETURN
      END
