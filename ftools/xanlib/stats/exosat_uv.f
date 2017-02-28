*- exosat_uv - expected cma uv count rate
      REAL FUNCTION exosat_uv(filter_code,object_class,v,colour_excess)
* description :
*  calculates a rough estimate of the stellar uv count rate in different cma
*  filters using the calibration in paerels' thesis with measured iue spectra
*  from wu et al's spectral atlas in nasa iue newsletter #22.
*  the uv calibrations are stored in xuv(wavelength,index) thus :
*     index=1    3000 lexan
*     index=2    aluminium/parylene
*     index=3    4000 lexan
*     index=4    polypropyline
*  with the uv count rates from other filters set to zero.
*  for objects other than stars of type oba the uv count rate is set to zero.
* author :
*  andy pollock (exosat::andy)
* history :
*  24 august 1988 : original
*  24 august 1988 : put in the v-magnitude extinction which i'd forgot !
 
* import :
      REAL filter_code                         ! cma filter code
      REAL object_class                        ! dbase object class
      REAL v                                   ! optical v magnitude
      REAL colour_excess                       ! e(b-v)
* local constants :
      INTEGER npts
      PARAMETER (npts=8)
      INTEGER ntype                            ! no of stars with spectra
      PARAMETER (ntype=27)
      REAL r                                   ! a(v)/e(b-v)
      PARAMETER (r=3.1)
* local variables :
      INTEGER filter                           ! filter code
      INTEGER jf                               ! filter index
      character(4) cc                           ! class string
      character(2) spectral_type                ! ..upper case
      INTEGER jtype                            ! spectral type index
      REAL c                                   ! count rate
      REAL dv                                  ! v-magnitude difference
      REAL p, q                                ! powers of 10
      INTEGER i
* external reference :
      REAL seaton                                      ! uv extinction
* local data :
      REAL xuv(npts,4)                         ! l*dl*area/hc
                                                                 ! 3lx
                                                                 ! al/par
                                                                 ! 4lx
      character(2) type(ntype)
      REAL v0(ntype)                           ! stars' apparent magnitude
      REAL xs0(ntype)                          ! stars' e(b-v)
      REAL f(npts,ntype)                       ! uv flux in 1e-11 ergs/cm**2/s/a
      REAL lambda(npts)                        ! wavelength in angstroms
      DATA lambda/1300., 1400., 1500., 1600., 1700., 1800., 1900.,
     &     2000./
      DATA xuv/5.1E7, 9.3E7, 1.8E8, 2.1E8, 1.0E8, 1.9E7, 1.6E6, 1.7E4,
     &     3.1E5, 6.0E4, 1.6E4, 3.4E3, 1.2E3, 6.0E2, 1.1E2, 7.9E0,
     &     4.2E5, 3.0E6, 2.7E7, 3.5E7, 1.3E7, 8.4E5, 2.2E4, 6.7E1, 0.,
     &     0., 0., 2.6, 3.2E9, 5.3E9, 4.7E9, 5.0E8/              ! ppl
      DATA v0/7.37, 7.26, 5.97, 7.31, 4.66, 7.46, 4.88, 4.62, 6.15,
     &     3.66, 3.97, 5.99, 5.23, 5.09, 2.87, 3.63, 4.91, 2.44, 4.36,
     &     3.82, 1.16, 2.56, 2.08, 4.78, 4.48, 5.65, 3.75/
      DATA xs0/0.48, 0.54, 0.32, 0.35, 0.07, 0.10, 0.10, 0.04, 0.05,
     &     0.04, 0.02, 0.04, 0.01, 0.00, 0.03, 0.01, 0.00, 0.01, 0.00,
     &     0.01, 0.01, 0.00, 0.00, 0.00, 0.00, 0.01, 0.01/
      DATA type/'O3', 'O4', 'O5', 'O6', 'O7', 'O8', 'O9', 'B0', 'B1',
     &     'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'A0', 'A1',
     &     'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9'/
                                                       ! hd93250 o3v
                                                       ! hd46223 o4
                                                       ! hd164794(9 sgr) o5
                                                       ! hd163758 o6.5iii
                                                       ! hd47839(15 mon) o7v
                                                       ! hd14633 o8v
                                                       ! hd214680(10 lac) o9v
                                                       ! hd36512(ups ori) b0v
                                                       ! hd31726 b1v
                                                       ! hd3360(zeta cas) b2iv
                                                       ! hd79447 b3iii
                                                       ! hd65904 b4v
                                                       ! hd34759(rho aur) b5v
                                                       ! hd90994(beta sex) b6v
                                                       ! hd23630(eta tau) b7iii
                                                       ! hd23850(27 tau) b8iii
                                                       ! hd38899(134 tau) b9v
                                                       ! hd103287(gam uma) a0v
                                                       ! hd166205(del umi) a1v
                                                       ! hd80081(38 lyn) a2v
                                                       ! hd216956(alp psa) a3v
                                                       ! hd97603(del leo) a4v
                                                       ! hd159561(alp oph) a5iii
                                                       ! hd28527 a6v
                                                       ! hd87696(21 lmi) a7v
                                                       ! hd27176(51 tau) a8v
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
     &     0.0, 0.0, 0.0, 0.1, 0.6, 2.0, 1.8, 2.5/     ! hd147547(gam her) a9iii
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
          GO TO 99999
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
            GO TO 99999
          END IF
          c = 0.
          dv = v0(jtype) - r*xs0(jtype) - v + r*colour_excess
          p = -11. + 0.4*dv
          DO i = 1, npts
            q = p + 0.4*(seaton(lambda(i),xs0(jtype))
     &          -seaton(lambda(i),colour_excess))
            c = c + xuv(i,jf)*f(i,jtype)*10.**q
          END DO
        ELSE
          WRITE (*,'(a)') ' unknown spectral type '//spectral_type//
     &                    ' in exosat_uv'
          c = 0.
        END IF
      ELSE
        c = 0.
      END IF
 
 
      exosat_uv = c
 
99999 CONTINUE
      END
