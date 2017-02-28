c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE PSMMAT(DL,DB)
C
C
C  $Id: psmmat.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*   EGRET POINT SOURCE ANALYSIS SYSTEM (SOURCE)
C*   Spec:
C*     PSMMAT = proc(DL, DB, CLAT, PSF-data) returns(PSF-matrix)
C*             effect: PSF array is filled with values of the
C*                     Point-Spread Function provided on input (PSMREP)
C*                     centred at (DL,DB) degrees from center of central
C*                     bin of matrix, which is at latitude CLAT (PSMREP)
C*                     The PSQ bin size is that set for CTL.
C*                     Matrix must have an odd number of bins (49*49)
C*                     For (DL,DB) = (0.,0.) the PSF peak is at the
C*                     precise centre of the central bin.
C*                     Each PSF bin contents is an average over a set
C*                     of points within the bin. The CTLGRI internal
C*                     parameter governs the number of points per bin.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:  by JRM
C=======================================================================
C  $Log: psmmat.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
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
C  Revision 1.1  2002/04/16 20:27:42  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:53:02  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:56  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE PSMMAT(DL,DB)
C
C  Common blocks used:
C     
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save
C
	
      REAL          DL,DB
      REAL*4        PINTGL,PSSUM
      character(80)  id

      common /id/id
C
C     The required shift in the PSQ, in degrees.
C     for array summation
C     
C     Local Variables:
      REAL L,B,LP,BP,CL,CB
C     coordinates
      REAL LPMIN,BPMIN
C     coordinate ranges
      REAL GTCIRC
C     function to return distance between two coordinates
      REAL PSFVAL
C     FUNCTION TO RETURN PSF VALUE (PER STERAD) AT R DEGREES
      REAL DPXL
C     step size for integration within a pixel
C

      id = '$Id: psmmat.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
      LOC='PSMMAT'


      if (jae_psmmat) then
         write(*,*)' '
         write(*,'(" In subroutine ",a)') LOC
         write(*,'(" EGRET PSF: "$)')
         write(*,*)egret_psf
         write(*,*)' '
      endif
C
C     Test argument range
C     
      AMAXDG = (psfsz-1)/2.*ctlscl ! array radius
      
      IF ((ABS(DL).GT.AMAXDG) .OR. (ABS(DB).GT.AMAXDG) ) THEN
         SIGNAL = 'E'
         WRITE(SIGMSG,'(''PSMMAT shift: '',2e9.2
     &        ,'' is more than PSF matrix'')')
     1        DL,DB
         return
c     call error(-1,loc)
      ENDIF
C
C     INITIALZE ARRAYS AND STERADIAN(LAT) CORRECTION
C
      PSR=PSFSCL*PI180
      CL0=CLAT-PSFSCL*(PSFSZ+1)/2
 
      DO 4 J=1,PSFSZ
	 call PSMCOR(1,J,L,B)
         PSMSR(J)=PSR*(sin((clat+b+PSFSCL/2.)*PI180)-
     &        sin((clat+b-PSFSCL/2.)*PI180))
 4    CONTINUE
C
C     Set up step size for integration within pixels:
      DPXL = PSFSCL/FLOAT(PGRID)
C
c
      if (jae_psmmat) then
         write(*,*)' '
         write(*,*)LOC
         CALL PSMCOR(1,1,L,B)
         L = L + float(PSFSZ+1)*PSFSCL/2.0
         B = B + float(PSFSZ+1)*PSFSCL/2.0
         write(*,'(" L:",f8.3,"  B:",f7.3)')L,B
         write(*,'("DL:",F8.3," DB:",F7.3)')DL,DB
         write(*,'("PSFSZ:",I3," PSFSCL:",e11.4)')PSFSZ,PSFSCL
         write(*,'("PSR:",e11.4," CLAT:",e11.4," CL0:",e11.4)')
     &        PSR,CLAT,CL0
         write(*,*)' '
      endif
c
c
C     Loop through output pixels, storing values of PSF at shifted l,b:
C     Also form integral, for normalization

      PINTGL = 0

c     WRITE(LU(9),*) ' PSFSZ, PSFSCL, DL, DB, PGRID, CLAT: '
c     WRITE(LU(9),*)   PSFSZ,PSFSCL,DL,DB,PGRID,CLAT
c     WRITE(LU(9),'(" PSFTYP: ",a)') PSFTYP

      MIT=PSFSZ/2+1

      DO 10 J=1,PSFSZ
         DO 10 I=1,PSFSZ
C     Find relative coordinates at centre of pixel I,J:
            CALL PSMCOR(I,J,L,B)
C     LATITUDE OFFSET OF PSF MATRIX FOR CORRECT ANGLE CALCULATIONS
            B = B+CLAT
            PSSUM = 0
            LPMIN = (L - PSFSCL/2 + DPXL/2)
            BPMIN = (B - PSFSCL/2 + DPXL/2)
C     DEFINE COORDINATES OF PSF CENTER FOR ANGLE DISTANCE CALCUL.
            CL=DL
            CB=CLAT+DB
C     Integrate over integration points within pixel
            DO 8 IL = 1,PGRID
               LP = LPMIN + (IL-1) * DPXL
               DO 8 IB = 1,PGRID
                  BP = BPMIN + (IB-1) * DPXL
C
C     Compute the radius from the PSF centre at LP,BP:
                  R = GTCIRC(CL,CB,LP,BP)
                  IF (SIGNAL.NE.' ') THEN
                     WRITE (SIGMSG,'(''PSMMAT: MAPDIS error: '',A,
     1                    ''.  Params: '',2(1PE10.1))') LP,BP
                  ENDIF
C
C     EVALUATE THE PSF VALUE AT THIS RADIUS, INCREMENT PSSUM:
                  kk = 0
                  if (I .eq. 39 .and. J .eq. 41) then
                     kk = 1
                  endif
                  PSSUM = PSSUM + PSFVAL(R,kk)
 8             CONTINUE

               PSSUM = PSSUM*PSMSR(J)/PGRID**2
C
               PSF(I,J) = PSSUM
C
               PINTGL = PINTGL + PSSUM
C
 10         CONTINUE

c            DO I=1,11
c            ENDDO
C
C     Verify that PSF is normalized.
            if (jae_psmmat) write(lu(1),'("PSF total is ",e10.3)')PINTGL
            IF (ABS(PINTGL-1.).GT.03) THEN
               WRITE(SIGMSG,'(
     &              "PSMMAT:PSF total is:",e10.3)')PINTGL
               SIGNAL = 'N'
               CALL ERROR(1,LOC)
               RETURN
            ENDIF

            SIGNAL = ' '

            RETURN
            END
c
