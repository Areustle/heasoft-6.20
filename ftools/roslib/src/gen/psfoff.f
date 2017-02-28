
      FUNCTION PSFOFF(ENE,EPS,X,IERR)
C
CC  calculates PSPC off-axis PSF (normalized surface brightness)
C
C************************ FFORM VERSION 1.3 ************ DD-MMM-YY HH:MM
C
CA  author : GRH        date: 24-FEB-1992 09:21 
CU  update : AMS        date: 2-APR-1993 16:42    off-axis dependence
CU  update : GRH        date: 2-APR-1993 17:05    exponential fraction
CU  update : RY         date: 2-AUG-1993 11:50    FNOR3, and A3
C   update : RY         date: Sept 1993           FNOR and A3
CT  status: not tested
C
C   general description:
CG  The PSPC on-axis PSF is a combination of three, physically well 
CG  understood terms:
CG
CG  1. a Gaussian for the intrinsic PSPC resolution due to the inherent
CG     statistics of the primary electron generation. Theoretically
CG     the Gaussian Sigma is propotrional to 1/SQRT(Energy)
CG
CG  2. an exponential function due to the finite penetration depth of
CG     the X-rays in the counter gas combined with the 8.5 degree cone
CG     angle. The PSPC is focussed for 1 keV; the 'chromatic aberration'
CG     is largest for large energies
CG
CG  3. a Lorentzian function for the mirror scattering which breaks into
CG     a different power law slope at larger energies. Theoretically the
CG     scattering fraction should increase like the square of the 
CG     energy, if the grazing angle remains constant. Due to the 
CG     diffraction laws, the shape parameters should be proporional to
CG     1/Energy.
CG
CG  In principle these three components should be folded with each 
CG  other, however, their angular domains are reasonably well separated
CG  that a simple addition is accurate enough. The detailed PSF 
CG  parameters and their energy dependence have been determined using 
CG  the PANTER telescope calibration data of both PSPC-A and PSPC-C at 
CG  the monochromatic energies 0.28, 0.93, 1.49 and 1.70 keV. At lower
CG  pulseheights than channel 15 (0.15 keV) additional 'ghost images' 
CG  appear in the PSPC for which no analytical fit is possible. These
CG  events should be avoided as far as possible in PSF modelling.
CG
CG  The off-axis blur of the telescope, although highly structured and
CG  asymmetric, can be modeled by a simple Gaussian in its radially
CG  integrated profile. This Gaussian is added in quadrature to the
CG  gaussian of the detector. Since the PSF is not convolved, but a
CG  simple addition of terms, the contribution of the exponential term
CG  has to be diminished while the gaussian is "eating up" the 
CG  exponential. This is modelled as a gaussian decay of the exponential
CG  term as a function of off-axis angle.
CG
CG
CG
C   call_var.          type I/O description
CP  ENE                 R   I   Energy [keV]
CP  EPS			R   I	off-axis angle [arc min]
CP  X                   R   I   angle from target position [arcsec]
C
CP  IERR                I   O   = 0 no error 
CP                              = 1 energy outside bounds 0.07-3 keV
CP                              = 3 
C
C     OUTPUT:
C             PSFOFF      surface brightness of point spread function,
C                         normalized, so that  Integral 2*PI*r*dr*f
C                         from 0 to infinity = 1 [1/arcsec2]
C
C   include_block_name          description
CI  R$COMMON:CGENL.CMN          general parameter common block
C
C   routines_called    type     description
CR  HFLAG               R       output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description
CX  
C
C***********************************************************************
C
C   variables   meaning
C
C
       IMPLICIT NONE
C      INCLUDE 'R$COMMON:CGENL.CMN'
C
      REAL*4        X, ENE, EPS, A1, A2, A3, FNOR1, FNOR2, FNOR3,
     #              SIGMA, RC, PSFOFF, BREAK1, BREAK2, ALPHA2,
     #		    ARG1, ARG2, AUX, SDETSQ, STELSQ,B3
      INTEGER*4     IERR
      REAL PI
      PI = 3.14159
C
C      DATA          RTNAME /'PSFOFF'/
C
C     IF (HFLAG(RTNAME,'in: ENE, X').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) ENE, X
C 101   FORMAT (1X,2F7.2)
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR=0
      IF(ENE.GE.0.07.AND.ENE.LE.3.)THEN
C
C       get energy-dependent parameters
C
C       exponential fraction (old)
C -old- A2=10**(-1.618+0.507*ENE+0.148*ENE*ENE)
C
C       exponential fraction; expression with exponent describes 
C       diminishing of the exponential part for off-axis angles due to 
C       increase of the Gaussian part. The exponential fraction has 
C       been updated and care is taken for the artefact above 2 keV
C
        A2=10**(-1.635+0.639*ENE+0.052*ENE*ENE)*exp(-0.5*(EPS/12.)**2)
C
C       scattering fraction
C
c prevoius A3 ... ( changed by RY on 7/26/93 )
c        A3=0.059*ENE**1.43
c         A3=0.041*ENE**1.43 ... changed by RY SEPT
        A3 = 0.075*ENE**1.43
C
C       avoid "exponential artefact"       
C     
        A2=MIN(A2,1.0-A3)
C
C       Gaussian fraction (total integral under the PSF is 1)
C
        A1=1.0-A2-A3
C
C       Gaussian sigma Detector
C
        SDETSQ=108.7*ENE**(-0.888)+1.121*ENE**6
C
C       Gaussian sigma Mirror
C
	STELSQ=0.219*EPS**2.848
	SIGMA=sqrt(SDETSQ+STELSQ)
C
C       exponential e-folding angle
C
        RC=SQRT(50.61*ENE**(-1.472)+6.80*ENE**5.62)
C
C       scattering Lorentzian break angles
C
        BREAK1=39.95/ENE
        BREAK2=861.9/ENE
C
C       scattering Lorentzian slope
C
        ALPHA2=2.119+0.212*ENE
C
C       normalization by integrals 0-infinity
C
        FNOR1=A1/(2.*PI*SIGMA*SIGMA)
        FNOR2=A2/(2.*PI*RC*RC)
        AUX=1.+BREAK2*BREAK2/BREAK1/BREAK1
c prev' FNOR (before 1993 aug 2)
c        FNOR3=A3/(PI*(LOG(AUX)+2./(AUX*(ALPHA2-2.))))
        B3 = BREAK2*BREAK2/(BREAK1*BREAK1)
        FNOR3=A3/(PI*(LOG(AUX)+(2.*B3)/(AUX*(ALPHA2-2.))))
C
C       calculate function
C
        ARG1=0.5*(X/SIGMA)**2
        IF(ARG1.GE.75.)ARG1=75.
        ARG2=X/RC
        IF(ARG2.GE.75.)ARG2=75.
        IF(X.LE.BREAK2)THEN
          PSFOFF=FNOR1*EXP(-ARG1)+
     #           FNOR2*EXP(-ARG2)+
     #           FNOR3/(BREAK1*BREAK1+X*X)
        ELSE
          PSFOFF=FNOR1*EXP(-ARG1)+
     #           FNOR2*EXP(-ARG2)+
     #           FNOR3/(BREAK1*BREAK1+BREAK2*BREAK2)*
     #           (X/BREAK2)**(-ALPHA2)
        ENDIF
      ELSE
        IERR=1
        PSFOFF=0.
      ENDIF
C
C     IF (HFLAG(RTNAME,'out: PSFOFF ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) PSFOFF
C9901   FORMAT (1X,E15.4)
C       CALL WRFLAG(OUTPT)
C     ENDIF
      RETURN
      END     


