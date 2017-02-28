C       SUBROUTINE  exp_anal(Aused,Bused)
C
C
C  $Id: exp_anal.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++     effect: The maximum aspect angle and minimum exposure are specified
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
c	variables:
c	AUSED==aspect_max, Maximum aspect angle for analysis (NOW COMMON)
c	BUSED==exp_min, minimum exposure for analysis (NOW COMMON)
c
c		Both variables are in locpos.copy COMMON block file
C=======================================================================
C%   Changes:
C  $Log: exp_anal.f,v $
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
C  Revision 1.1  2002/04/16 20:27:30  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/05/08  18:28:26  jae
c added output code when jae_exp_anal = TRUE
c
c Revision 5.1  1996/02/29  20:47:29  jae
c ..
c
c Revision 5.0  1996/02/13  21:36:51  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE  exp_anal(Aused,Bused)

C     Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
      CHARACTER input*50
      CHARACTER XSTRING*50,YSTRING*50,TITLE*70
      REAL MAPVAL


      id = '$Id: exp_anal.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      LOC='EXP_ANAL'

      if (jae_exp_anal)write(6,*)' In routine exp_anal'

 1109 if (coord_sys.eq.'C') then
         write(6,'(" Coordinate system: Celestial")')
         write(6,'(" Present pointing direction: ",F7.2,",",F6.2)')
     &        SC_RAJ,SC_DECJ
      else
         write(6,'(" Coordinate system: Galactic")')
         write(6,'(" Present pointing direction: ",F7.2,",",F6.2)')
     &        SC_LJJ,SC_BJJ
      endif
      write(6,'(" Present aspect angle limit: ",F6.2)')aspj
      
      if (coord_sys.eq.'C') then
         pL=SC_RAJ
         pB=SC_DECJ
      else
         pL=SC_LJJ
         pB=SC_BJJ
      endif

 1111 write(6,*)' Enter Z to change the Zenith Aspect cone;'
      write(6,*)' Enter ZP to change the Pointing Direction;'
      write(6,'("Enter ZR to center the Pointing Direction in the",
     &     " ROI;")')
      write(6,*)' Enter ZC to reset the Pointing Direction to the'
           write(6,*)'          original pointing direction;'
           write(*,'(" Enter C to accept the values above: ",$)')
           READ(LU(12),'(a)') input
           numcar = index(input, ' ') - 1
           if (numcar.ge.0) then
              do i=1,numcar
                 call to_upper(input(i:i))
              enddo
              if (input(1:1).eq.'C')goto 1112
              if (input(1:1).ne.'Z') then
                 write(6,'(//,"Input ERROR:",A,//)')
     &                input(1:numcar)
                 goto 1109
              endif
              input='LPID#'//input(1:2)
              CALL JLOC_POS(input)
              goto 1109
           endif
 1112      continue
           Aused=aspj
 1         continue
           write(6,'(
     &          " Enter minimum exposure (in units of 10^7 cm^2-s) ")')
           write(6,'(
     &          "      required to analyze a source;")')
           write(6,'(
     &          " Enter C or <cr> for default=1 (10^7 cm^2-s);")')
           write(6,'(
     &          " Enter M for exposure min and max within ROI;")')
           write(6,'(
     &          " Enter G for graphical map presentation;")')
           write(6,'(
     &          "or enter F for 5% of exposure map maximum: ",$)')
           READ(LU(12),'(A)') input

           CALL TO_UPPER(input(1:1))

           if (input.eq.' ') then
              Bused=1.
              return
           elseif (input(1:1).eq.'G'.or.input(1:1).eq.'M') then
              goto 3
           elseif (input(1:1).eq.'C') then
              Bused=1.
              return
           elseif (input.eq.'F') then
              CALL MAPMAX(EMAP,IsrcL,IsrcB,Bused,CTLMSZ1,CTLMSZ2)
              Bused=1.0E-07*Bused/ (
     &             (sin(pi180*(srcB+ctlscl/2.))-
     *             sin(pi180*(srcB-ctlscl/2.)))
     &             *ctlscl*pi180)
              print *,'Exposure maximum:',Bused,' 10^7 cm^2-s'
              Bused=0.05*Bused
              print *,'Exposure minimum cut:',Bused,' 10^7 cm^2-s'
              return
           else
              read(input,*,end=1,err=8108)Bused
              return
           endif

 8108      print *,' '
           in_dex = len_trim(input) + 1
           print *,'Input Error:',input(1:in_dex)
           print *,'Enter a number, G, M, F or C for default'
           print *,' '
           goto 1
           
 3         continue
           continue
           npixel=0
           ex_min=1.e20
           ex_max=-1.

           do i=ROIPRG(1),ROIPND(1)
              do j=ROIPRG(2),ROIPND(2)
                 call mapcor(i,j,xL,xB)
                 call error(1,loc)
                 if ( gtcirc(xL,xB,pL,pB).lt.Aused) then
                    npixel=npixel+1
                    tmpmap(npixel)=mapval(emap,I,J,CTLMSZ1,CTLMSZ2) / (
     &                   (sin(pi180*(xB+ctlscl/2.))-
     *                   sin(pi180*(xB-ctlscl/2.)))
     &                   *ctlscl*pi180)
                    tmpmap(npixel)=tmpmap(npixel)/1.E7
                    if (tmpmap(npixel).lt.ex_min)ex_min=tmpmap(npixel)
                    if (tmpmap(npixel).gt.ex_max)ex_max=tmpmap(npixel)
                 endif
              enddo
           enddo

           if (input(1:1).eq.'M') then
              write(6,'(//," Exposure maximum in ROI: ",E12.5,
     &             " 10^7 cm^2-s")')ex_max
              write(6,'(" Exposure minimum in ROI: ",E12.5,
     &             " 10^7 cm^2-s",//)')ex_min
              goto 1
           endif

           write(6,'(//,"graphics device? (return for ",A10,"):",$)')
     &          g_device
           read(LU(12),'(A)')input

           if (input.ne.' ')g_device=input

           YSTRING='Number of pixels'
           write(XSTRING,'("Exposure (in units of 10**7 cm**2s)")')
           TITLE='Exposure Distribution'
           CALL PGBEGIN(0,g_device,1,1)
           CALL PGSCH(1.2)
           CALL PGSLW(3)
           nbins=50
           CALL PGHIST(npixel,tmpmap,ex_min,ex_max,nbins,0)
           CALL PGLABEL(XSTRING,YSTRING,TITLE)
           CALL PGEND
           goto 1

           END



