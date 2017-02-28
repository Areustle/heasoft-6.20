C This set of subroutines performs barycentric corrections to the data.
C  It has been slashed from the original BARYCENTER routines from the 
C  Vela 5B program SVELA.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0  13 Feb 1992  Jim Lochner and Laura Whitlock; routine for SVELA
C        2.0   7 Dec 1994  Adaptations to FTOOLS

       subroutine barycenter(mjdtime, long_sat, lat_sat, elements)


C Common block variables

       common /SOURCE/ begintime, endtime, long_src, lat_src, 
     +        searchrad, minflux, maxflux, maxerr, stimbin, 
     +        backopt, binopt, collim, spincheck, pointcheck, weight
       common /NAME/ sourcename
       
       logical collim, spincheck, pointcheck, weight
       integer backopt, binopt
       real long_src, lat_src, searchrad, minflux, maxflux, maxerr
       real stimbin
       double precision begintime, endtime
       character(10) sourcename

       common /BARYCONST/ vlight, rsat, rcpme, delte, pi, deg2rad
       common /BARYCORD/ xa, ya, za, xc, yc, zc
       common /BARYSOLSYS/ gam, sinie, cosie, ee, ce2, orbp1, orbe,
     +        xma, xma1, sw, cw, swe, cwe, c2
       common /BARYLUNAR/  sinil, cosil, el, flun, cl00
       common /BARYTIME/ day, jdcent, jdcsq, jdccu, GRS, tsm

       double precision delte, rcpme, vlight, rsat, pi, deg2rad
       double precision xa, ya, za, xc, yc, zc
       double precision gam, sinie, cosie, ee, ce2
       double precision orbp1(8), orbe(8), xma(8), xma1(8), c2(8)
       double precision swe, cwe, sw(8), cw(8)
       double precision sinil, cosil, el, flun, cl00
       double precision day, jdcent, jdcsq, jdccu, GRS, tsm

C Local variables

       integer elements

       real long_sat(elements), lat_sat(elements)

       double precision alpha, delta, x0, y0, z0
       double precision mjdtime(elements)

C Define some constants for the entire barycentric calculations

       pi = 3.14159265359D0
       deg2rad = pi / 180.0D0
       delte = 32.1843817D0
       rcpme = 328900.53D0
       vlight = 299792.456211D0
       rsat = 18.47D0*6378.156D0

C Convert the galactic source coordinates into 1950 celestial coordinates
C  (stored as a (x,y,z) vector)

       alpha = DBLE(long_src) * deg2rad
       delta = DBLE(lat_src) * deg2rad
       x0 = DCOS(alpha) * DCOS(delta) 
       y0 = DSIN(alpha) * DCOS(delta)
       z0 = DSIN(delta)

       call gal2cel(x0, y0, z0, xa, ya, za)

       call bary2
       call bary3(mjdtime, long_sat, lat_sat, elements)

       return

       end

c-----------------------------------------------------------------
C This subroutine defines a number of constants which are used by
C  the barycenteric calculations.  The results are shared with 
C  other barycenteric routines through the common blocks
C  BARYCONST and BARYCORD.  Most parameters pertain the positions and 
C  motions of the Earth, Moon, and planets.

       subroutine bary2

C Common block declarations

       common /BARYCONST/ vlight, rsat, rcpme, delte, pi, deg2rad
       common /BARYCORD/ xa, ya, za, xc, yc, zc
       common /BARYSOLSYS/ gam, sinie, cosie, ee, ce2, orbp1, orbe,
     +        xma, xma1, sw, cw, swe, cwe, c2
       common /BARYLUNAR/  sinil, cosil, el, flun, cl00
       common /BARYTIME/ day, jdcent, jdcsq, jdccu, GRS, tsm

       double precision delte, rcpme, vlight, rsat, pi, deg2rad
       double precision xa, ya, za, xc, yc, zc
       double precision gam, sinie, cosie, ee, ce2
       double precision orbp1(8), orbe(8), xma(8), xma1(8), c2(8)
       double precision swe, cwe, sw(8), cw(8)
       double precision sinil, cosil, el, flun, cl00
       double precision day, jdcent, jdcsq, jdccu, GRS, tsm

C Local variables

       integer i

       double precision ASN(8), PHE(8), PHE1(8), XIN(8)
       double precision ORAD(8), ORAD1(8), FAC(8)
       double precision ORBP(8), RCPM(8), c1(8), yrout, epsi
       double precision xb, yb, zb, xd, yd, zd, xe, ye, ze, xf, yf
       double precision pe, ae, ce1, emmr, pl, al, XILUN, cl0

      DATA ASN/48.03452D0,76.45444D0,49.3644D0,100.1883D0,113.4627D0,
     + 73.8929D0,131.5901D0,109.9880D0/

      DATA PHE/77.06592D0,131.2188D0,335.59818D0,13.9459D0,91.7116D0,
     + 169.5939D0,36.8738D0,224.2645D0/

      DATA XIN/7.00427D0,3.39438D0,1.84983D0,1.30565D0,
     +2.48887D0,0.77292D0,1.77084D0,17.14382D0/

      DATA ORAD/0.387099D0,0.723332D0,1.523691D0,5.202513D0,
     +9.540912D0,19.14329,30.04906D0,39.30096D0/

      DATA xma/192.4424D0,159.7286D0,267.4801D0,340.3471D0,
     +11.6494D0,36.0951,213.1813,338.0240D0/

      DATA ORBP/88.02337D0,224.62395D0,6.8702058D2,4333.5987D0,
     +10829.431D0,3.0586843D4,6.0451236D4,9.0465744D4/

      DATA orbe/.20563D0,.006785D0,.093382D0,4.80289D-2,
     +0.0551034D0,0.0451020D0,0.0100395D0,0.2461528D0/

      DATA RCPM/6023600.D0,408523.5D0,3098710.D0,
     +1047.355D0,3498.5D0,22869.D0,19314.0D0,3.D06/

C Calculate the time in julian centuries from Jan 1, 1900 (JD 2415020.5) 
C  to 19 Dec 1974 (JD 2442400.5) [jdcent]

       jdcent = 27380.5D0 / 36525.0D0
       jdcsq = jdcent * jdcent
       jdccu = jdcent * jdcsq

C Precess the source's celestial coordinates (xa, ya, za) to 19 Dec 1974

       yrout = 1900.0D0 + (jdcent * 100.0D0)
       call precs(xa, ya, za, 1950.0D0, yrout, xb, yb, zb)

C Compute the source's eliptocentric coordinates for 19 Dec 1974

       epsi = deg2rad * ((5.03D-07*jdccu) - (1.64D-06*jdcsq) - 
     +        (1.30125D-2*jdcent) + 23.45229D0)
       xd = xb
       yd =  (yb * DCOS(epsi)) + (zb * DSIN(epsi))
       zd = -(yb * DSIN(epsi)) + (zb * DCOS(epsi))

C Now set up Earth constants
C   pe  = period of earth's orbit around the Sun (s)
C   ae  = mean orbital radius of the Earth (km)
C   ce1 = mean angular speed of the Earth (km/s)
C   ce2 = Light travel time to the Sun (s)
C   GRS = Gravitational redshift time correction (s)

       pe = 365.25636D0 * 86400.0D0
       ae = 1.4959787141056D+08
       ce1 = 2.0D0 * pi * (ae / pe)
       ce2 = ae / vlight
       grs = 4.0D0 * pi * (ae**2) / (pe * vlight**2)

C Now set up Moon constants
C  EMMR = ?
C  el   = orbital eccentricty of the Moon
C  fl   = square of 1-e^2
C  XILUN= orbital inclination of the Moon to the ecliptic plane (radians)
C  CL0  = ?mean angular speed of the moon? (km/s)
C  CL00 = ?

       emmr = 81.3007D0
       pl = 27.32166D0 * 86400.0D0
       al = 384400.0D0
       el = 0.5490489D-01
       flun = DSQRT(1.0D0 - el**2)
       xilun = 5.145396D0 * deg2rad
       cl0 = 2.0D0 * pi* al / (pl * (emmr+1.0D0))
       cl00 = (cl0 / vlight) * (pl / (2.D0 * pi))
       sinil = DSIN(xilun)
       cosil = DCOS(xilun)
 
C Now set up planetary constants 

       do i = 1, 8
          phe1(i) = phe(i) - asn(i)
          orad1(i) = orad(i) * ae
          xma1(i) = xma(i) * deg2rad
          orbp1(i) = orbp(i) * 86400.0D0
          fac(i) = DSQRT(1.0D0 - orbe(i)**2)
c MJT 09July96 g77/linux DCOSD,DSIND -> DCOS,DSIN
c          xe =  (xd * DCOSD(asn(i))) + (yd * DSIND(asn(i)))
c          ye = -(xd * DSIND(asn(i))) + (yd * DCOSD(asn(i)))
          xe =  (xd * DCOS(asn(i)*deg2rad)) + 
     $         (yd * DSIN(asn(i)*deg2rad))
          ye = -(xd * DSIN(asn(i)*deg2rad)) + 
     $         (yd * DCOS(asn(i)*deg2rad))
          ze = zd
          xf = xe
c          yf = (ye * DCOSD(xin(i))) + (ze * DSIND(xin(i)))
c          sw(i) = -(xf * DCOSD(phe1(i))) - (yf * DSIND(phe1(i)))
c          cw(i) = FAC(i) * ((xf*DSIND(phe1(i))) - (yf*DCOSD(phe1(i))))
          yf = (ye * DCOS(xin(i)*deg2rad)) + 
     $         (ze * DSIN(xin(i)*deg2rad))
          sw(i) = -(xf * DCOS(phe1(i)*deg2rad)) - 
     $         (yf * DSIN(phe1(i)*deg2rad))
          cw(i) = FAC(i) * ((xf*DSIN(phe1(i)*deg2rad)) - 
     $         (yf*DCOS(phe1(i)*deg2rad)))
          c1(i) = 2.D0 * pi* orad1(i) / (orbp1(i) * (rcpm(i)+1.0D0))
          c2(i) = (c1(i)/vlight) * (orbp1(i)/(2.0D0 * pi))
       end do 

       return

       end

C----------------------------------------------------
C NOTE: previous version expected jd - 2400000, mjd is jd - 2400000.5

       subroutine bary3(mjdtime, long_sat, lat_sat, elements)

C Common block declarations

       common /BARYCONST/ vlight, rsat, rcpme, delte, pi, deg2rad
       common /BARYCORD/ xa, ya, za, xc, yc, zc
       common /BARYSOLSYS/ gam, sinie, cosie, ee, ce2, orbp1, orbe,
     +        xma, xma1, sw, cw, swe, cwe, c2
       common /BARYLUNAR/  sinil, cosil, el, flun, cl00
       common /BARYTIME/ day, jdcent, jdcsq, jdccu, GRS, tsm

       double precision delte, rcpme, vlight, rsat, pi, deg2rad
       double precision xa, ya, za, xc, yc, zc
       double precision gam, sinie, cosie, ee, ce2
       double precision orbp1(8), orbe(8), xma(8), xma1(8), c2(8)
       double precision swe, cwe, sw(8), cw(8)
       double precision sinil, cosil, el, flun, cl00
       double precision day, jdcent, jdcsq, jdccu, GRS, tsm

C Local variables

       integer i, iloop, elements

       real long_sat(elements), lat_sat(elements)

       double precision mjdtime(elements)
       double precision hjohn
       double precision frac, dayf, jdc0, yrout
       double precision epsi, coeps, sieps, yma(8), tbs(8), ecca(8)
       double precision se, ce, factor, sing, cosg, coso, sino
       double precision cres, gamp, omlun, temel, wlun, sinth, costh
       double precision swl, cwl, xmal, eccl, sl, cl
       double precision xg, yg, zg, xe, ye, ze, xf, yf
       
       character(80) message

       do 200 iloop = 1, elements
          if ((elements .lt. 10000) .and. 
     +        (mod(iloop,1000) .eq. 0)) then
             write(message,'('' Calculating element '', i4, 
     +            '' of '', i4)') iloop, elements
             call fcecho(message)
          else if ((elements .lt. 100000) .and. 
     +        (mod(iloop,10000) .eq. 0)) then
             write(message,'('' Calculating element '', i5, 
     +            '' of '', i5)') iloop, elements
             call fcecho(message)
          else if (mod(iloop,10000) .eq. 0) then
             write(message,'('' Calculating element '', i6, 
     +            '' of '', i6)') iloop, elements
             call fcecho(message)
          endif
          hjohn = mjdtime(iloop) + 1
c MJT 09July96 g77/linux JIDINT -> IDINT ?
c          jdc0 = (JIDINT(hjohn) * 1.D0 - 15020.5D0) / 36525.0D0
          jdc0 = (IDINT(hjohn) * 1.D0 - 15020.5D0) / 36525.0D0
          frac = DMOD(hjohn, 1.D0)
          dayf = frac + (delte/86400.D0)
          jdcent = jdc0 + (dayf/36525.D0)
          jdcsq = jdcent * jdcent
          jdccu = jdcent * jdcsq
          day = (jdc0 * 36525.D0) + dayf
          yrout = 1900.0D0 + (100.0D0 * jdcent)

C Precess source's celestial coordinates to the time of observation

          call precs(xa, ya, za, 1950.0D0, yrout, xc, yc, zc)

C Convert sources's observation epoch from celestial to ecliptic coordinates 

          epsi = deg2rad * ((5.03D-07*jdccu) - (1.64D-06*jdcsq) - 
     +                      (1.30125D-2*jdcent) + 23.45229D0)
          coeps = DCOS(epsi)
          sieps = DSIN(epsi)
          xg = xc
          yg =  (yc * coeps) + (zc * sieps)
          zg = -(yc * sieps) + (zc * coeps)

C Compute the mean longitude of perihelion (gam)

          gam = deg2rad * ((3.0D-06*jdccu) + (4.53D-04*jdcsq) +
     +                     (4.70684D-05 * day) + 281.22083D0)

C Compute the eccentricity of the Earth's orbit (?)

          ee = (-1.26D-07 * jdcsq) - (4.18D-05*jdcent) + 1.675104D-02

C Compute the mean anomalies of the planets

          do i = 1, 8
             tbs(i) = 0.0D0
          end do
          do i = 1, 8
             yma(i) = (day-27380.5D0) * 86400.0D0 * 
     +                ((2.0D0 * pi) / orbp1(i)) + xma1(i)
             call newton(yma(i), orbe(i), ecca(i))
             se = DSIN(ecca(i))
             ce = DCOS(ecca(i))
             tbs(i) = c2(i) * (cw(i) * se + (ce - orbe(i)) * sw(I))
          end do

C...FOR JUPITER, INDEX = 4

          sinie = cw(4) / DSQRT(1.0D0 - orbe(4)**2)
          sinie = DATAN2(sw(4),sinie)
          factor = DSQRT((1.0D0 + orbe(4)) / (1.0D0 - orbe(4))) *
     +             DTAN(ecca(4) * 0.5D0)
          sinie = (2.0D0 * DATAN(factor)) + sinie

C Convert ecliptic (xg, yg, zg) to Earth-Sun elliptocentric coordinates

          sing = DSIN(gam)
          cosg = DCOS(gam)
          swe = -(xg * cosg) - (yg * sing)
          cwe =  (xg * sing) - (yg * cosg)
          sinie = DATAN2(-swe,-cwe) - sinie
          cwe = cwe * DSQRT(1.0D0 - ee**2)

C    EARTH CONSTANTS ARE NOW SET UP FOR POINTS DURING THE HOUR
C    NOW READY FOR EARTH-MOON MOTION.

          cres = (2.70434358D+02 + (1.31763965268D+01 * day) - 
     +            (1.133D-03 * jdcsq) + (1.9D-06 * jdccu)) / 360.0D0
          gamp = (3.34329653D+02 + (1.114040803D-01 * day) - 
     +            (1.0325D-02 * jdcsq) - (1.2D-05 * jdccu)) / 360.0D0
          omlun = (2.59183275D+02 - (5.29539222D-02 * day) + 
     +             (2.078D-03 * jdcsq) + (2.D-05 * jdccu)) / 360.0D0
          gamp = 2.0D0 * pi * DMOD(gamp,1.0D0)
          omlun = 2.0D0 * pi * DMOD(omlun,1.0D0)

C    CRES IS THE MEAN LONGITUDE OF THE MOON IN THE ECLIPTIC TO THE
C ASCENDING NODE, THEN ALONG THE ORBIT.  GAMP IS THE SIMILARLY
C CONSTRUCTED LONGITUDE OF PERIGEE.  OMLUN IS THE LONGITUDE OF THE
C ASCENDING NODE OF THE ORBIT IN THE ECLIPTIC.

          wlun = gamp - omlun
          xmal = 2.D0 * pi * DMOD(cres,1.0D0) - gamp

C    NOW WE MUST TRANSFORM TO ELIPTO-CENTRIC COORDINATES.  ROTATE
C ECLIPTIC COORDINATES FORWARD BY OMLUN ABOUT Z AXIS, THEN BY XILUN
C ABOUT NEW X AXIS, THEN ABOUT NEW Z AXIS BY WLUN.

          temel = 0.D0
          coso = DCOS(omlun)
          sino = DSIN(omlun)
          xe =  (xg * coso) + (yg * sino)
          ye = -(xg * sino) + (yg * coso)
          ze = zg
          xf = xe
          yf = (ye * cosil) + (ze * sinil)
          sinth = DSIN(wlun)
          costh = DCOS(wlun)
          swl = -(xf * costh) - (yf * sinth)
          cwl = flun * ((xf * sinth) - (yf * costh))
          call newton(xmal,el,eccl)
          sl = DSIN(eccl)
          cl = DCOS(eccl)
          temel = cl00 * ((cwl * sl) + ((cl - el) * swl))

C    NOW DONE WITH MOON MOTION, SUM UP WITH THE PLANETS

          tsm = 0.D0
          do 100 i = 1, 8
             tsm = tbs(i) + tsm
 100      continue
          tsm = tsm + temel

          call bary4(mjdtime(iloop), long_sat(iloop), lat_sat(iloop))

 200   continue
       
       message = ' Barycenteric corrections complete '
       call fcecho(message)

       return

       end

c----------------------------------------------------------

       subroutine BARY4(mjd_sat, l_sat, b_sat)

C Common block declarations

       common /BARYCONST/ vlight, rsat, rcpme, delte, pi, deg2rad
       common /BARYCORD/ xa, ya, za, xc, yc, zc
       common /BARYSOLSYS/ gam, sinie, cosie, ee, ce2, orbp1, orbe,
     +        xma, xma1, sw, cw, swe, cwe, c2
       common /BARYLUNAR/  sinil, cosil, el, flun, cl00
       common /BARYTIME/ day, jdcent, jdcsq, jdccu, GRS, tsm

       double precision delte, rcpme, vlight, rsat, pi, deg2rad
       double precision xa, ya, za, xc, yc, zc
       double precision gam, sinie, cosie, ee, ce2
       double precision orbp1(8), orbe(8), xma(8), xma1(8), c2(8)
       double precision swe, cwe, sw(8), cw(8)
       double precision sinil, cosil, el, flun, cl00
       double precision day, jdcent, jdcsq, jdccu, GRS, tsm

C Local variables

       real l_sat, b_sat

       double precision mjd_sat, thsun, xmasn, ecces, ce, se
       double precision tse, tbse, tibe
       double precision alpha, delta, xh, yh, zh, xj, yj, zj
       double precision xdobs, ydobs, zdobs, dobs, tcobs

C Calculate the longitude (thsun) and mean anomaly (xmasn) of the Sun

       thsun = (2.7969668D+02 + (0.9856473354D0 * day) + 
     +          (3.03D-04 * jdcsq)) / 360.0D0
       xmasn = 2.0 * pi * DMOD(thsun,1.0D0) - gam

       xmasn = xmasn - (4.3D-05 * DSIN(sinie+xmasn))
       call newton(xmasn,ee,ecces)
       ce = DCOS(ecces)
       se = DSIN(ecces)
       tse = ce2 * ((se * cwe) + ((ce-ee) * swe))

C    NOW THE MOTION OF THE BARYCENTER DUE TO THE EARTH

       tbse = -tse / (rcpme + 1.0D0)

C    NOW THE GRAVITATIONAL REDSHIFT TERMS

       tibe = GRS * ee * se

C    NOW TIME DELAY TERMS FROM VELA 5B TO EARTH CENTER

       alpha = deg2rad * DBLE(l_sat)
       delta = deg2rad * DBLE(b_sat)
       xh = DCOS(alpha) * DCOS(delta)
       yh = DSIN(alpha) * DCOS(delta)
       zh = DSIN(delta)

       call gal2cel(xh, yh, zh, xj, yj, zj)

       xdobs = rsat * xj
       ydobs = rsat * yj
       zdobs = rsat * zj
       dobs = (xc * xdobs) + (yc * ydobs) + (zc * zdobs)
       tcobs = dobs / vlight

C..TOTAL THE VARIOUS CORRECTION TERMS

       tiobs = tcobs + tibe + tbse + tsm + tse

       mjd_sat = mjd_sat + (tiobs / 86400.0D0)

       return

       end

C----------------------------------------------------------------------

       subroutine newton (a, ec, b)

       implicit none

       integer i, iter

       double precision a, ec, b
       double precision ecc, pi, criter, xold, xnew, dx
       double precision value, dvalue

       pi = 3.14159265359D0

       ecc = ec
       iter = 50
       criter = 1.0D-06
       xold = a
       do 10 i = 1, iter
          call func(xold, value, ecc)
          call deriv(xold, dvalue, ecc)
          if (dvalue .eq. 0.0D0) go to 5
          dx = (a - value) / dvalue
          xnew = xold + dx
          if (xnew .eq. 0.D0) then
             if (dx .eq. 0.D0) go to 2
          else if (DABS(dx/xnew) .lt. criter) then
             go to 2
          endif
          xold = xnew
 10    continue
 5     xnew = xold
 2     b = xnew

C  This line occurs in the original program, but due to a typo (?), the
C twopi arguments are zero and the result does not get changed.  Commented
C out to see if the values match with this "error" included.
c
c MJT 09July96 g77/linux JIDINT -> IDINT ?
c       b = b - (2.0D0 * pi * JIDINT(b /(2.0D0 * pi)))
       b = b - (2.0D0 * pi * IDINT(b /(2.0D0 * pi)))

       return

       end

C----------------------------------------------------------------->

       subroutine func(g, b, ecc)

       double precision g, b, ecc

       b = g - (ecc * DSIN(g))

       return
 
       end

C------------------------------------------------------------------>

       subroutine deriv(g, b, ecc)

       double precision g, b, ecc

       b = 1.0D0 - (ecc * DCOS(g))

       return

       end
