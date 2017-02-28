C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE CTLRED(iired)
C
C
C  $Id: ctlred.f,v 1.4 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C*     EFFECT: A set of records from the control file CTL
C*             is read and the values are stored in the commons
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated:    by  JRM
C=======================================================================
C
C%   Changes:
c	jrm 11/7/93 don't read ranal
c	jae 22-DEC-94 Added card line OUTPUT for file output purposes
c			JOUT_P_TYPE added to cnfrep.copy
c			JOUT_P_TYPE value	EFFECT
c			     1			Output PSF values to file
c						name in Lmap variable
c			     2			Same as 1 for EDP
c			     3			Same as 1 for SAR
c
c	jae 26-DEC-94 Added card line JAE_SR for file calculation of 
c	subtended solid angle rings in PSFSHAPE routine.  JRM used approx.:
c
c		jrm_SR=2*PI*dT*sin(T)
c
c	JAE uses jae_SR=2*PI*(COS(t1)-COS(t2)) (exact calc)
c
c	where  T == t1, t2= t1+dT, dT = 0.2 degrees
c
c	NOTE: at T=0  % dif == 0, at T=0.2 % dif= 33% decreasing to 5% at 5 
c	degrees where % dif = (jrm_SR - jae_SR)/jae_SR
c
c			JAE_SR = 0 -> jrm method
c			JAE_SR = 1 -> jae method
c
c	jae 11/MAY/95	added input parameter logical iired to
c			bypass CTL reading and ask for CMAPFILE name 
c
C  $Log: ctlred.f,v $
C  Revision 1.4  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2002/12/26 17:42:53  irby
C  Fix read/write statements (for f90 compatibility): add/remove commas and
C  fix lines over 72 chars.
C
C  Revision 1.2  2002/04/18 19:34:09  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:29  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.12  1996/11/11  17:38:00  jae
c Added a cosmetic output of mode value to follow
c the file ID so that the user can tell if narrow,
c regular fan or strip mode has been used to
c calculate the PSF.
c
c Revision 5.11  1996/07/29  21:19:20  jae
c Fixed line longer than char*72
c
c Revision 5.10  1996/07/29  20:13:34  jae
c Added strip-mode for ctlfil15(mode 81) and
c ctlfil20(mode 87) which are instrument x and y
c strip-mode respectively.
c Also cleaned up some minor coding typo's
c
c Revision 5.9  1996/07/29  17:48:11  jae
c Updated file to include input files g12345,
c c12345,g123456,c123456,g5,g6,c5,c6.
c Also confirmed format of FITS_DIR
c
c Revision 5.8  1996/04/30  14:04:26  jae
c repaired typo above 8150 continue where
c isa_cal -> ise_cal on three lines
c
c Revision 5.7  1996/03/06  21:18:47  jae
c Updated code: FITS_DIR read with GETENV only
c once at program start in like.f
c
c Revision 5.6  1996/03/06  17:29:03  jae
c slight bug fix: added 1 to in_dex1 for FITS_DIR
c to add the slash in the proper place
c
c Revision 5.5  1996/03/06  16:58:04  jae
c fixed compile error due to syntax error
c
c Revision 5.4  1996/03/06  16:51:43  jae
c Added lines for error checking to find bug
c in file opening code
c
c Revision 5.3  1996/03/05  20:16:14  jae
c fixed vp input
c
c Revision 5.2  1996/03/05  18:06:41  jae
c Rapair of loop possibly caused by non-zero values contained in ISTATUSn
c
c Revision 5.1  1996/02/29  20:47:19  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:30  jae
c Subroutine Module for like V5.00
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE CTLRED(iired)

C  Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/jnfrep.copy'

      save

C     Local variables

      character(80)  id, fits_dir, comnt, comnte, comnts, themapfile
      logical       exp_ok, cat_ok, iired

      common /id/id

      
      id = '$Id: ctlred.f,v 1.4 2013/05/21 19:08:25 irby Exp $'
      LOC='CTLRED'
c
c     Set defaults


      jae_sr_type = 1
      fits_dir = data_dir

      if(jae_ctlred)print *,LOC,' iired: ',iired
      if(iired)goto 8100

      n_id=0
      exp_ok=.false.
      cat_ok=.false.

      do j=1,16
         PHI(j)=0
      enddo

      JAE_2ND_CALIB   = 3
      cal_curve_INDEX = 1
      CAL_TASC_COINC  = 1
      PSFINC          = 0.2
      PSMSCL          = 0.0
      PSMSZ           = 81
      CLAT            = 0.0
      SHFTMX          = 0.0
      PGRID           = 1
      PSFTYP          = "EGRETCAL...."
      ITRMOD          = 74
      THETA(1)        = 1
      THETA(2)        = 2
      THETA(3)        = 3
      DO I = 4, 9
         THETA(I)     = 0
      ENDDO
      NEWMAPFILE      = 'newmap'

c     Calculate derived map parameters and check validity
      if (PSMSZ.GT.201) then    ! this will change if the psfrep changes
         signal='C'
         sigmsg='PSFSZ exceeds size set in psfrep common'
         return
      endif
      
      ipsmsz=PSMSZ/2

      if (2*ipsmsz.eq.PSMSZ) then 
         signal='C'
         sigmsg='PSFSZ is not odd.'
         return
      endif

      PSFSZ  = PSMSZ
        
 8100 continue
      ISTATUS2=0
      ISTATUS1=0
      ISTATUS=0
c
      MAPFILE=CMAPFILE

      write(*,'("MAPFILE:",a)') MAPFILE
      write(*,*)' '
c     MAPFILE_C=MAPFILE
      MAPFILE_C = CMAPFILE_F
c
c     clear improper signals
c
      SIGNAL=' '
      SIGMSG='                                     '
c
      MAPFILE=EMAPFILE

      TMPLOC='CTLRED  '
      ISTATUS1=0
      CALL ftopen(LU(14),mapfile,0,NBLOCK,ISTATUS1)
      IF (ISTATUS1.ne.0) then
         write(*,*) 'Error while opeining ', mapfile
         return
      endif
c     
      WRITE (LU(1),'("Reading header of file:",A)')MAPFILE

      SIGNAL=' '
      SIGMSG='                              '
      CALL FTGHSP(LU(14),keyexist,keyadd,istatus)
      if (istatus.ne.0) then
         write(sigmsg,'("ERROR: FTGHSP -ISTATUS:",i4)')istatus
         signal='E'
         CALL ERROR(0,LOC)
         CALL FTCLOS(LU(14),istatus)
         return
      endif

      isa_dex=0
      ise_dex=0
      if (keyexist.gt.0) then
         do j=1,keyexist-1
            comnt=' '
            CALL FTGREC(LU(14),j,comnt,istatus)
            if (isa_dex.eq.0) then
               isa_dex=index(comnt,'SARFIL')
               if (isa_dex.ne.0) comnts=comnt(1:80)
            endif
            
            if (ise_dex.eq.0) then
               ise_dex=index(comnt,'EDPFIL')
               if (ise_dex.ne.0) comnte=comnt(1:80)
            endif
            
            if (ise_dex.gt.0.and.isa_dex.gt.0) goto 8150
            if (istatus.ne.0) then
               if (isa_dex.gt.0.or.ise_dex.gt.0) goto 8150
               CALL FTCLOS(LU(14),istatus)
               SIGNAL='E'
               write(SIGMSG,'("EMAP FILEREAD ERROR: ",
     &              "FTGREC istatus:",I4)')istatus
               CALL ERROR(0,LOC)
               return
            endif
         enddo

         if (isa_dex.gt.0.or.ise_dex.gt.0) goto 8150
         CALL FTCLOS(LU(14),istatus)
         goto 9000
      else
         SIGMSG='No FITS header lines found in EMAP file !'
         SIGNAL='E'
         write(*,*)' '
         CALL ERROR(0,LOC)
         return
      endif

 8150 if (isa_dex.gt.0) then
         fileid=comnts(isa_dex+6:isa_dex+7)
         isa_cal=index(comnts,'CALTBL')
         if (isa_cal.gt.0) then
            read(comnts(isa_cal+6:isa_cal+7),*)n_id
            if (n_id.eq.5) ITRMOD=75
            if (n_id.eq.15) ITRMOD=81
            if (n_id.eq.20) ITRMOD=87
         endif

         exp_ok=.TRUE.
         goto 9500
      else
         fileid=comnte(ise_dex+6:ise_dex+7)
         ise_cal=index(comnte,'CALTBL')
         if (ise_cal.gt.0) then
            read(comnts(ise_cal+6:ise_cal+7),*)n_id
            if (n_id.eq.5) ITRMOD=75
            if (n_id.eq.15) ITRMOD=81
            if (n_id.eq.20) ITRMOD=87
         endif

         exp_ok=.TRUE.
         goto 9500
      endif
 9000 continue

      if (ITRMOD.ge.75) ITRMOD=74
      call readcat(0,cal_curve_INDEX,CAL_TASC_COINC,ITRMOD,n_id,fileid)
      
      if (signal.ne.' ') then
         themapfile(1:1)=signal
         call error(0,LOC)
         signal='E'
         SIGMSG='ERROR in READCAT; FILEID unavailable'
         CALL ERROR(0,LOC)
         return
      endif

      cat_ok=.TRUE.
 9500 print *,' '
c     if(cat_ok)print *,'fileid from catalog file:',fileid,' Mode:',ITRMOD
c     if(exp_ok)print *,'fileid from exposure file:',fileid,' Mode:',ITRMOD
      print *,' '

      if (n_id.ne.0) then
         JAE_2ND_CALIB=16
      else
         JAE_2ND_CALIB=3
      endif
      
      do j=1,JAE_2ND_CALIB
         PHI(J)= 1              ! DLB
      enddo

      SIGNAL=' '
      SIGMSG='                              '
      MAPFILE_E=MAPFILE
      
      return
      END
