      FUNCTION PSFOFF(Ene,Eps,X,Ierr)
C
CC  calculates PSPC off-axis PSF (normalized surface brightness)
C
C************************ FFORM VERSION 1.2 ************ DD-MMM-YY HH:MM
C
CA  author : GRH        date: 24-FEB-1992 09:21
CU  update : AMS        date: 2-APR-1993 16:42    off-axis dependence
CU  update : GRH        date: 2-APR-1993 17:05    exponential fraction
C
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
C
      REAL*4 X , Ene , Eps , a1 , a2 , a3 , fnor1 , fnor2 , fnor3 , 
     &       sigma , rc , PSFOFF , break1 , break2 , alpha2 , arg1 , 
     &       arg2 , aux , sdetsq , stelsq
      REAL*4 br1 , br2 , b3
      INTEGER*4 Ierr
      REAL pi
      pi = 3.14159
C
C      DATA          RTNAME /'PSFOFF'/
C
c     IF (HFLAG(RTNAME,'in: ENE, X').NE.0) THEN
c       WRITE (OUTPT,101,IOSTAT=IOS) ENE, X
c 101   FORMAT (1X,2F7.2)
c       CALL WRFLAG(OUTPT)
c     ENDIF
C
      Ierr = 0
      IF ( Ene.GE.0.07 .AND. Ene.LE.3. ) THEN
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
         a2 = 10**(-1.635+0.639*Ene+0.052*Ene*Ene)
     &        *EXP(-0.5*(Eps/12.)**2)
C
C       scattering fraction
C
c prevoius A3 ... ( changed by RY on 7/26/93 )
c        A3=0.059*ENE**1.43
c         A3=0.041*ENE**1.43
         a3 = 0.075*Ene**1.43
C
C       avoid "exponential artefact"
C
         a2 = MIN(a2,1.0-a3)
C
C       Gaussian fraction (total integral under the PSF is 1)
C
         a1 = 1.0 - a2 - a3
C
C       Gaussian sigma Detector
C
         sdetsq = 108.7*Ene**(-0.888) + 1.121*Ene**6
C
C       Gaussian sigma Mirror
C
         stelsq = 0.219*Eps**2.848
         sigma = SQRT(sdetsq+stelsq)
C
C       exponential e-folding angle
C
         rc = SQRT(50.61*Ene**(-1.472)+6.80*Ene**5.62)
C
C       scattering Lorentzian break angles
C
         break1 = 39.95/Ene
         break2 = 861.9/Ene
         br2 = break2*break2
         br1 = break1*break1
C
C       scattering Lorentzian slope
C
         alpha2 = 2.119 + 0.212*Ene
C
C       normalization by integrals 0-infinity
C
         fnor1 = a1/(2.*pi*sigma*sigma)
         fnor2 = a2/(2.*pi*rc*rc)
c        AUX=1.+BREAK2*BREAK2/BREAK1/BREAK1
         aux = 1. + br2/br1
         b3 = br2/br1
         fnor3 = a3/(pi*(LOG(aux)+(2.0*b3)/(aux*(alpha2-2.))))
C
C       calculate function
C
         arg1 = 0.5*(X/sigma)**2
         IF ( arg1.GE.75. ) arg1 = 75.
         arg2 = X/rc
         IF ( arg2.GE.75. ) arg2 = 75.
         IF ( X.LE.break2 ) THEN
c     #           FNOR3/(BREAK1*BREAK1+X*X)
            PSFOFF = fnor1*EXP(-arg1) + fnor2*EXP(-arg2)
     &               + fnor3/(br1+X*X)
         ELSE
c     #           FNOR3/(BREAK1*BREAK1+BREAK2*BREAK2)*
            PSFOFF = fnor1*EXP(-arg1) + fnor2*EXP(-arg2)
     &               + fnor3/(br1+br2)*(X/break2)**(-alpha2)
         ENDIF
      ELSE
         Ierr = 1
         PSFOFF = 0.
      ENDIF
C
c     IF (HFLAG(RTNAME,'out: PSFOFF ').NE.0) THEN
c       WRITE (OUTPT,9901,IOSTAT=IOS) PSFOFF
c9901   FORMAT (1X,E15.4)
c       CALL WRFLAG(OUTPT)
c     ENDIF
      RETURN
      END
