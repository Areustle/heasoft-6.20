*- exosat_uv.for - expected cma uv count rate
      real*4 function exosat_uv(filter_code,
     &                          object_class,
     &                          v,
     &                          colour_excess)
* Description :
*  Calculates a rough estimate of the stellar UV count rate in different cma
*  filters using the calibration in Paerels' thesis with measured IUE spectra
*  from Wu et al.'s spectral atlas in NASA IUE newsletter #22.
*  The uv calibrations are stored in xuv(wavelength,index) thus :
*     index=1    3000 lexan
*     index=2    aluminium/parylene
*     index=3    4000 lexan
*     index=4    polypropyline
*  with the UV count rates from other filters set to zero.
*  For objects other than stars of type OBA the UV count rate is set to zero.
*  The stars and spectral types used were in the following order :
*      1 HD93250 O3V
*      2 HD46223 O4
*      3 HD164794(9 Sgr) O5
*      4 HD163758 O6.5III
*      5 HD47839(15 Mon) O7V
*      6 HD14633 O8V
*      7 HD214680(10 Lac) O9V
*      8 HD36512(ups Ori) B0V
*      9 HD31726 B1V
*     10 HD3360(zeta Cas) B2IV
*     11 HD79447 B3III
*     12 HD65904 B4V
*     13 HD34759(rho Aur) B5V
*     14 HD90994(beta Sex) B6V
*     15 HD23630(eta Tau) B7III
*     16 HD23850(27 Tau) B8III
*     17 HD38899(134 Tau) B9V
*     18 HD103287(gam UMa) A0V
*     19 HD166205(del UMi) A1V
*     20 HD80081(38 Lyn) A2V
*     21 HD216956(alp Psa) A3V
*     22 HD97603(del Leo) A4V
*     23 HD159561(alp Oph) A5III
*     24 HD28527 A6V
*     25 HD87696(21 LMi) A7V
*     26 HD27176(51 Tau) A8V
*     27 HD147547(gam Her) A9III
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  24 August 1988 : original
*  24 August 1988 : put in the v-magnitude extinction which I'd forgot !
*  8 September 1993 : portable version
 
* Import :
*  filter code - CMA filter code
      real*4 filter_code
*  object_class - dbase object class
      real*4 object_class
*  V - optical V magnitude
      real*4 V
*  colour_excess - E(B-V)
      real*4 colour_excess

* Local constants :
      integer*4 npts
      parameter (npts=8)
*  ntype - no of stars with spectra
      integer*4 ntype
      parameter (ntype=27)
*  R - ratio of extinction to colour excess A(V)/E(B-V)
      real*4 R
      parameter (R=3.1)

* Local variables :
      integer*4 filter
      integer*4 jf
      character(4) cc
      character(2) spectral_type
      integer*4 jtype
      real*4 c
      real*4 dv
      real*4 p,q
      character(80) o
      integer*4 i

* External reference :
*  seaton - UV extinction
      real*4 seaton

* Local data :
*  lambda - wavelengths in Angstroms
      real*4 lambda(npts)
*  xuv - l*dl*area/hc
      real*4 xuv(npts,4)
*  type - stars' spectral types
      character(2) type(ntype)
*  V0 - stars' apparent magnitude
      real*4 v0(ntype)
*  xs0 - stars' E(B-V)
      real*4 xs0(ntype)
*  f - UV flux in 1e-11 ergs/cm**2/s/A
      real*4 f(npts,ntype)

      DATA lambda/1300., 1400., 1500., 1600., 1700., 1800., 1900.,
     &     2000./

      DATA xuv/5.1E7, 9.3E7, 1.8E8, 2.1E8, 1.0E8, 1.9E7, 1.6E6, 1.7E4,
     &     3.1E5, 6.0E4, 1.6E4, 3.4E3, 1.2E3, 6.0E2, 1.1E2, 7.9E0,
     &     4.2E5, 3.0E6, 2.7E7, 3.5E7, 1.3E7, 8.4E5, 2.2E4, 6.7E1, 0.,
     &     0., 0., 2.6, 3.2E9, 5.3E9, 4.7E9, 5.0E8/

      DATA v0/7.37, 7.26, 5.97, 7.31, 4.66, 7.46, 4.88, 4.62, 6.15,
     &     3.66, 3.97, 5.99, 5.23, 5.09, 2.87, 3.63, 4.91, 2.44, 4.36,
     &     3.82, 1.16, 2.56, 2.08, 4.78, 4.48, 5.65, 3.75/

      DATA xs0/0.48, 0.54, 0.32, 0.35, 0.07, 0.10, 0.10, 0.04, 0.05,
     &     0.04, 0.02, 0.04, 0.01, 0.00, 0.03, 0.01, 0.00, 0.01, 0.00,
     &     0.01, 0.01, 0.00, 0.00, 0.00, 0.00, 0.01, 0.01/

      DATA type/'O3', 'O4', 'O5', 'O6', 'O7', 'O8', 'O9', 'B0', 'B1',
     &     'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'A0', 'A1',
     &     'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9'/

      DATA f/3.0, 3.0, 3.5, 3.0, 2.8, 2.4, 2.0, 1.4, 1.4, 1.5, 2.1, 1.9,
     &     1.8, 1.6, 1.2, 0.7, 32., 28., 28., 23., 18., 16., 12., 8.5,
     &     2.7, 2.3, 3.0, 2.3, 2.8, 2.9, 2.8, 2.0, 330., 230., 210.,
     &     170., 130., 120., 110., 98., 18., 14., 13., 10., 8.0, 7.5,
     &     7.0, 6.5, 220., 170., 150., 100., 100., 90., 80., 70., 420.,
     &     290., 240., 180., 170., 140., 120., 117., 65., 44., 35., 27.,
     &     24., 22., 20., 18., 390., 290., 250., 200., 190., 175., 120.,
     &     120., 180., 140., 130., 110., 100., 90., 70., 70., 25., 22.,
     &     19., 16., 14., 13., 12., 10., 50., 38., 35., 30., 25., 22.,
     &     20., 20., 40., 35., 32., 28., 24., 22., 20., 20., 100., 115.,
     &     115., 96., 90., 90., 80., 80., 40., 51., 55., 47., 42., 43.,
     &     37., 36., 11., 14., 15., 14., 12., 11., 11., 10., 15., 34.,
     &     49., 53., 50., 56., 52., 47., 0.5, 2.2, 4.0, 6.4, 6.5, 7.8,
     &     7.1, 7.4, 1.0, 2.5, 4.0, 8.4, 9.0, 10.5, 10.3, 10.3, 0.0,
     &     2.0, 4.0, 38., 60., 110., 100., 110., 0.0, 0.0, 0.2, 4.0,
     &     14., 22., 19., 20., 0.0, 0.0, 0.3, 4.0, 12., 26., 24., 30.,
     &     0.0, 0.0, 0.0, 0.3, 0.9, 2.0, 1.8, 2.1, 0.0, 0.0, 0.0, 0.2,
     &     1.1, 3.2, 3.0, 3.3, 0.0, 0.0, 0.0, 0.0, 0.2, 0.6, 0.5, 0.6,
     &     0.0, 0.0, 0.0, 0.1, 0.6, 2.0, 1.8, 2.5/
*-
 
      WRITE (cc,'(i4.4)') nint(object_class)
      IF ( cc(1:1).EQ.'2' ) THEN
        IF ( cc(2:2).EQ.'1' ) THEN
          spectral_type = 'O'//cc(3:3)
        ELSE IF ( cc(2:2).EQ.'2' ) THEN
          spectral_type = 'B'//cc(3:3)
        ELSE IF ( cc(2:2).EQ.'3' ) THEN
          spectral_type = 'A'//cc(3:3)
        ELSE
          c = 0.
          exosat_uv = c
          return
        END IF
        jtype = 1
        DO WHILE ((spectral_type.NE.type(jtype)) .AND. (jtype.LT.ntype))
          jtype = jtype + 1
        END DO
        IF ( spectral_type.EQ.type(jtype) ) THEN
          filter = nint(filter_code)
          IF ( filter.EQ.7 ) THEN
            jf = 1
          ELSE IF ( filter.EQ.6 ) THEN
            jf = 2
          ELSE IF ( filter.EQ.3 ) THEN
            jf = 3
          ELSE IF ( filter.EQ.2 ) THEN
            jf = 4
          ELSE
            c = 0.
            exosat_uv = c
            return
          END IF
          c = 0.
          dv = V0(jtype) - R*xs0(jtype) - V + R*colour_excess
          p = -11. + 0.4*dv
          DO i = 1, npts
            q = p + 0.4*(seaton(lambda(i),xs0(jtype))
     &          -seaton(lambda(i),colour_excess))
            c = c + xuv(i,jf)*f(i,jtype)*10.**q
          END DO
        ELSE
          o=' Unknown spectral type '//spectral_type//' in EXOSAT_UV'
          call xwrite(o,5)
          c = 0.
        END IF
      ELSE
        c = 0.
      END IF
 
      exosat_uv = c
 
      return

      end
