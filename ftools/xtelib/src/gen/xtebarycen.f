      subroutine xtebarycen(lue,satflag,alpha,delta,jd,frc,reae,jdcor
     &                     ,frccor,ierr)

c calculate a general BARYCENter correction.

c This routine uses JPL2000 ephemeris table to calculate the time
c delay at the solar system barycenter relative a position vector
c reae centered on the earth.

c This routine is mission independent.

c This routine processes a single input time whose integer part is jd and
c and whose fractional part if frc.

c If satflag is set equal to .false. the vector reae will be ignored,
c but even it is .true., reae = (0,0,0) will produce the same result.

c This routine calls subroutine xtereadeph, which returns the earth - sun
c position vector for the given input TDT time.

c  I  lue     (i)  LU of JPL 2000 ephemeris file
c  I  satflag (l)  if = .false. means ignor orbit data in reae.
c#  I  alpha   (d)  Source RA in radians -- epoch 2000.
c#  I  delta   (d)  Source DEC in radians -- epoch 2000.
c  I  dir (3) (d)  Dir vector of source calculated from RA and DEC
c  I  jd      (i)  Integer part of uncorrected time, in Julian Day
c  I  frc     (d)  Fractional part of uncorrected time, in Julian Day
c  I  reae    (d)  Satellite position vector from orbit file
c  O  jdcor   (i)  Integer part of corrected time, in Julian Day
c  O  frccor  (d)  Fractional part of corrected time, in Julian Day
c  O  error   (i)  error status

c Author: TMB  (MIDAS) -- originally program BARO.
c Adapted: EAL Goddard/HSTX  1994
c Modified 5-Feb-96 for XTE for which the ephemeris is already in J2000.0 
C     equatorial coordinates. --- Mike Stark
c Modified 14-Fed-96 to accept direction of source as input rather than
c     to calculate it from the RA and DEC on each iteration.

C   routines_called    type     description
CR  AR_JDC              SR      conversion DD,MM,YY to JD
C
C   variables   meaning
C
C     TWOPI         R*8     (parameter) 2*pi
C     C             R*8     (parameter) light speed (in m/s)
C     FRC           R*8      current fraction of day
CP  PI                         constant
C
C   this is the geocentric vector with X-axis pointing to 0 long that
C   is written in the orbit data as X,Y,Z of satellite. It is defined
C   new here
C     REAE(3)       R*8      vector from geocenter to spacecraft (m)
C
C   we will keep REA(3) to mean the earth center to satellite vector in
C   the sidereal system
C     SWLONG	    R*8      west longitude of sat. from Grnwch(rad)
C     SWLAT         R*8      latitude of satelitte (rad)
C     RADIUS        R*8      radius of sat. from geocenter (light-s)
C     SITER         R*8      distance of sat. in the eq. plane (light-s)
C     PH            R*8      hour angle of the sat. (rad)
C     ST            R*8      mean sidereal time (rad)
C     TTT           R*8      time used for precessing
C     EEQ(3)        R*8      earth sat. vec in sid. coord (light-s)
C     REA(3)        R*8      earth sat. vec in 2000 coord (light-s)
C     RCA(3)        R*8      vector from SSBC to spacecraft (light-s)
C     ETUT          R*8      TDB-UTC in seconds
Cc     ALPHA         R*8      RA  of the source (in radians)
Cc     DELTA         R*8      DEC of the source (in radians)
C     DIR(3)        R*8      unit vector from sun to source
C     RCE(3)        R*8      vector from SSBC to geocenter (light-s)
C     RCS(3)        R*8      vector from SSBC to suncenter (light-s)
C     VCE(3)        R*8      time derivetive of RCE
C     BARY          R*8      sum of propagation delays (seconds)
C     NVALS         I*4      number of values for keyword reading
C     I_UNIT        I*4      unit for input table (orbit data)
C     O_UNIT        I*4      unit for output table
C     LOG_LU        I*4      logical unit for logfile
C     K             I*4      index of main loop
C     I             I*4      loop index (for X,Y,Z)
C     SUNDIS        R*8     distance from sun to site (light-s)
C     SUNSIZ        R*8     apparent radius of the sun (radians)
C     CTH           R*8     cosine of site-sun-source angle
C     DTGR          R*8     relativistic (Shapiro) delay (s)
C     SUN           L*4     flag for source occultation by the sun

      IMPLICIT NONE
c
C
      double precision twopi, c
      PARAMETER (TWOPI=6.28318530717958648D0
     &   ,C=2.99792458D+8)
      LOGICAL*4 lsun,satflag
      INTEGER*4 jdcor, jd, i,ierr,lue, jd_2
      DOUBLE PRECISION pi, frccor, SatBDt
     &   , frc, rea(3), reae(3), rca(3), rsa(3)
     &   , alpha, delta, dir(3), rce(3), rcs(3), vce(3), bary
     &   , etut, frc_2, cth, dtgr, bclt, sunsiz, sundis
      REAL*8 DAYSEC
      PARAMETER (DAYSEC=1.D0/86400.D0)
      REAL*8 AULTSC
      PARAMETER (AULTSC=499.004782D0)
      REAL*8 GAUSS
      PARAMETER (GAUSS=0.01720209895D0)
      REAL*8 RSCHW
      PARAMETER (RSCHW=(GAUSS**2)*(AULTSC**3)*(DAYSEC**2))
      REAL*8 SUNRAD
      PARAMETER (SUNRAD=2.315D0)

      if(ierr.ne.0) return
      pi = 2.d0*dasin(1.d0)

c---------------------------------------------
c Convert satellite position to light-seconds.
c---------------------------------------------

      DO i = 1 , 3
        rea(i) = reae(i) / C
      ENDDO

c-------------------------------------
c Get satellite-barycenter vector rca.
c-------------------------------------


         jd_2 = Jd
         frc_2 = Frc

c Read from ephemeris file

      CALL xtereadeph(lue,jd_2,frc_2,Rce,Rcs,Etut,Vce,ierr)
C     TYPE *, 'Earth: ', RCE(1), RCE(2), RCE(3)
C     TYPE *, 'Sun:   ', RCS(1), RCS(2), RCS(3)
C     TYPE *, 'Sat:   ', REA(1), REA(2), REA(3)
c      print*,'ETUT is ',Etut
c      print*,'Earth ', RCE(1), RCE(2), RCE(3)
c      print*,'Sun ', RCS(1), RCS(2), RCS(3)
c      print*,'Sat ', REA(1), REA(2), REA(3)

      IF(ierr.ne.0) RETURN

c Compute RCA and convert it to light-seconds

      DO i = 1 , 3
         Rca(i) = Rce(i) + Rea(i)
      ENDDO

c------------------------
c Barycenter vector done.
c------------------------

c-------------------------------------------------------------------
c Now get the time delay for a source at RA = alpha and DEC = delta.
c-------------------------------------------------------------------

C     TYPE *, 'Alpha = ', Alpha, ' Delta = ', Delta
c      print*,'Alpha is ',alpha,' and Delta is ',delta

      dir(1) = DCOS(delta)*DCOS(alpha)
      dir(2) = DCOS(delta)*DSIN(alpha)
      dir(3) = DSIN(delta)

c      Print*,'DIR is ',dir(1),dir(2),dir(3)
c Calculate light-travel time to barycenter in Euclidean space

      bclt = Dir(1)*rca(1) + Dir(2)*rca(2) + Dir(3)*rca(3)
C     TYPE *, 'Euclidean travel time: ', BCLT
c      print*,'Euclidean travel time: ', BCLT

c Now calculate the time delay due to the gravitational field of the
c Sun (I.I. Shapiro, Phys. Rev. Lett. 13, 789 (1964)).

      SatBDt = rea(1) * Vce(1) + rea(2) * Vce(2) + rea(3) * Vce(3)
      rsa(1) = rca(1) - Rcs(1)
      rsa(2) = rca(2) - Rcs(2)
      rsa(3) = rca(3) - Rcs(3)
      sundis = DSQRT(rsa(1)*rsa(1)+rsa(2)*rsa(2)+rsa(3)*rsa(3))
      sunsiz = SUNRAD/sundis
      cth = (Dir(1)*rsa(1)+Dir(2)*rsa(2)+Dir(3)*rsa(3))/sundis
      lsun = ((cth+1.D0).LT.(0.5D0*sunsiz*sunsiz))

c      print*,'SUN logical is ',lsun
      
      IF ( lsun ) THEN
         bary = bclt
      ELSE
         dtgr = -2D0*RSCHW*DLOG(1.D0+cth)
c        TYPE *, DtGr
c         print*,'DTGR is ',dtgr
c         print*,'Etut and SatBDt is ',Etut, SatBDt

         bary = bclt - dtgr + Etut + SatBDt
      ENDIF

c-----------------------------------------
c Finally, apply time delay to input time.
c-----------------------------------------

c      print*,' '
c      print*,' Barycent time is ',bary
c      print*,' Barycenter time in seconds is ', bary/86400.d0
c      print*,' Fractional part of uncorrect time is ',frc
c      print*,' Integer part of uncorrect time is ',jd

      frccor = frc + bary/86400.d0
      IF ( frccor.GE.1.0 ) THEN
        jdcor = jd + 1
        frccor = frccor - 1.0
      ELSE
        jdcor = jd
      ENDIF

c      print*,' Integer part of corrected time is ',jdcor
c      print*,' Final fractional part of corrected time is ',frccor
c      print*,' '

      return
      END



