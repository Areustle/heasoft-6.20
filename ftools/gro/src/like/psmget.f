C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE PSMGET
C
C
C  $Id: psmget.f,v 1.4 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*   EGRET POINT SOURCE ANALYSIS SYSTEM (SOURCE)
C*   Spec:
C*     PSFGEN = proc(CTL) returns (PSF)
C*             signals('E': error,'D': Energy range not supported)
C*             effect: PSF array is filled with values of the COS-B
C*                     or EGRET pontspreaad function
C*                     for an energy range (CTLEMN<CTLEMX),
C*                     centred at (0,0) degree
C*                     EACH PSM BIN CONTENT IS AN AVERAGE OVER A SET
C*                     OF POINTS WITHIN THE BIN. THE CTLGRI
C*                     parameter governs the number of points per bin.
C
C=======================================================================
C LIKE Version: 4.8 DELIVERED: November 8th 1993, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: psmget.f,v $
C  Revision 1.4  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2007/01/31 21:04:41  irby
C  Fix lines that were too long (>72 chars).
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
C  Revision 1.1  2002/04/16 20:27:42  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:33  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:55  jae
c Subroutine Module for like V5.00
c
C
C% 5 Nov  1992, JRM include ROSAT PSF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE PSMGET

C  Common blocks used:
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(70)  tmpfnm

      character(80) id
      common /id/id
C                     Axis definitions for Calibration files:
      REAL     ENERAX(20)
      DATA     ENERAX/15,20,30,35,50,60,70,100,150,200,300,500,700,1000,
     1     2000,3000,4000,6000,7000,10000/
C
      id = '$Id: psmget.f,v 1.4 2013/05/21 19:08:26 irby Exp $'
      LOC='PSMGET'


      if (jae_psmget) then
         write(*,'("In subroutine ",a)') LOC
         write(*,'(" EGRET PSF: "$)')
         write(*,*)egret_psf
         write(*,*)' '
      endif

      IF (PSFTYP.EQ.'EGRETCAL....') THEN
c     adjust energy range boundries
         I=1
         DO WHILE (I.LE.20)
            IF (CTLEMN.LT.ENERAX(I)+1) THEN
               IEL=I
               I=20
            ENDIF
            I=I+1
         ENDDO

         I=1
         DO WHILE (I.LE.20)
            IF (CTLEMX.LT.ENERAX(I)+1) THEN
               IEH=I
               I=20
            ENDIF
            I=I+1
         ENDDO

         IF (CTLEMX.GT.ENERAX(20)+1) IEH=20 ! allow full EGRET range 
         IF (CTLEMN.GE.CTLEMX) THEN
            WRITE(SIGMSG,*)'INACCEPTABLE ENERGY RANGES:',CTLEMN,CTLEMX
            SIGNAL='E'
            RETURN
         ENDIF

         if (abs(float(CTLEMN)-ENERAX(IEL)).gt.1.e-2.or.
     &        abs(float(CTLEMX)-ENERAX(IEH)).gt.1.e-2) then
            if (initial) then
c     write(lu(1),*)
c     &   'MPE software does not support PSF for this energy range;'
               write(lu(1),'(" PSF being generated for",f8.1,
     &              " < E <",f8.1)') ENERAX(IEL),ENERAX(IEH)
            endif
         endif

         CALL CNFCAL
         CALL ERROR(1,LOC)

         if (verbose) Write(lu(1),*)
     &      'Setting up EGRET PSF with spectral index',gamma
         if (egret_psf) CALL EGRPSF
         if (egret_psf) CALL ERROR(1,LOC)      

      ELSEIF (PSFTYP.EQ.'COSBVELA....') THEN
         if (egret_psf) CALL COSPSF
         if (egret_psf) CALL ERROR(1,LOC)

      ELSEIF (PSFTYP.EQ.'SAS2VELA....') THEN
         if (egret_psf) CALL SASPSF
         if (egret_psf) CALL ERROR(1,LOC)

      ELSEIF (PSFTYP.EQ.'ROSAT.......') THEN
         if (egret_psf) CALL rosatPSF
c     if(egret_psf)CALL ERROR(1,LOC)

      ELSE
         write(lu(1),'("Unsupported PSF:",a)') PSFTYP
         stop 3
      ENDIF

C     PSF array in PSFREP contains central bin centred PSF (odd # bins)
C     (PSM array in PSMREP is for bin corner centred PSF (even # bins)

      PSMSZ  = int(20./PSFSCL) *2+1

      if (PSMSZ.gt.201) then
c     exceeds hard coded limit
         PSMSZ  = 201
         write(6,'("PSF array radius limited to",f6.2,
     &     " by hard coded PSF array size")') float((PSMSZ-1)/2)*PSFSCL
      endif

      PSFSZ  = PSMSZ
      PSFORG(1)=(PSFSZ-1)/2+1
      PSFORG(2)=(PSFSZ-1)/2+1
C
      CALL PSMMAT(lshift,bshift)
      CALL ERROR(1,LOC)
      psftotal=0.

      DO j=1,PSFSZ
         DO I=1,PSFSZ
            psftotal=psftotal+PSF(j,I)
c     if(jae_psmget)then
c     WRITE(6,'(2i4,f6.2)j,i,PSF(j,I)
         ENDDO
      ENDDO

      if (jae_psmget)
     &     WRITE(LU(1),'("PSMGET: psftotal",f7.2)') psftotal

      close(LU(21))
      close(LU(22))
      close(LU(23))
      
c     write (*,*),'***** in psmget: psd: ', psdfile, ' edp: ',
c     *		edpfile, ' sar: ', sarfile

      tmpfnm = 'rm -f ' // psdfile(1:len_trim(psdfile))
      call system(tmpfnm)
      
      tmpfnm = 'rm -f ' // edpfile(1:len_trim(edpfile))
      call system(tmpfnm)

      tmpfnm = 'rm -f ' // sarfile(1:len_trim(sarfile))
      call system(tmpfnm)

c      write(99,*)PSF

      RETURN
      END
c
