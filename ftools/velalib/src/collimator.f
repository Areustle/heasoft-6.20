C This set of subroutines calculates the parallel and perpendicular 
C  components of the collimator response.  It is based on a set of routines
C  by Bill Pence.  About 9 degrees are included in the parallel component
C  due to satellite motion & aperture.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 0.0(?)            Bill Pence
C        1.0  13 Feb 1992  Laura Whitlock and Jim Lochner, in SVELA program
C        2.0   5 Dec 1994  Modifications for FTOOLS
C        2.1  22 Dec 1994  Explicitly passed the experiment's pointing
C        2.2   2 Mar 1995  Made generic (no common block, calls colcalc instead
C                           of performing the computations itself)

      subroutine colresp(obstime, searchrad, resp, long_exp, lat_exp,
     +           long_src, lat_src, spinper)

      real searchrad, resp, long_exp, lat_exp, spinper
      real long_src, lat_src
      real long_pole, lat_pole, az_pole, dist_pole

      double precision obstime


      call pole(obstime, long_pole, lat_pole)
      call azim(long_exp,lat_exp,long_pole,lat_pole,az_pole,dist_pole)
      call colcalc(searchrad, .TRUE., resp, long_exp, lat_exp,   
     +     long_src, lat_src, az_pole, dist_pole, spinper)

      return

      end

C---------------------------------------------------------------->
C This set of subroutines calculates the parallel and perpendicular 
C  components of the collimator response.  It is based on a set of routines
C  by Bill Pence.  About 9 degrees are included in the parallel component
C  due to satellite motion & aperture.  This used to be done with the 
C  routine 'colresp', but has been redesigned to allow more efficient sharing
C  with the FVELGALLC FTOOL
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 0.0(?)            Bill Pence
C        1.0  13 Feb 1992  Laura Whitlock and Jim Lochner, in SVELA program
C        2.0   5 Dec 1994  Modifications for FTOOLS
C        2.1  22 Dec 1994  Explicitly passed the experiment's pointing
C        2.2   2 Mar 1995  Converted to colcalc

       subroutine colcalc(searchrad, checklimit, resp, long_exp,  
     +            lat_exp, long_src, lat_src, az_pole, dist_pole, 
     +            spinper)

       logical checklimit

       real resp, circle, collsize, searchrad
       real rad2deg, deg2rad, pi
       real long_exp, lat_exp, long_src, lat_src, az_pole, dist_pole
       real az_src, dist_src, angle, dist_par, dist_perp, dist_lim
       real resp_par, resp_perp, angle2, delta, spinper

       parameter (pi = 3.14159265359)
       parameter (rad2deg = 180.0 / pi)
       parameter (deg2rad = 1.0 / rad2deg)
       data collsize /6.1/
       data circle /360.0/


       resp = 0.0
       resp_par = 0.0
       resp_perp = 0.0
       call azim(long_exp,lat_exp,long_src,lat_src,az_src,dist_src)
       angle = MOD((az_src - az_pole),circle)
       angle = angle * deg2rad
       dist_par = ABS(dist_src*COS(angle))
       dist_perp = ABS(dist_src*SIN(angle))
       dist_lim = SQRT(dist_par**2 + dist_perp**2)
       if ((checklimit) .and. (dist_lim .gt. searchrad)) RETURN
       if (dist_perp .gt. collsize) return

       angle2 = (collsize - dist_perp) / collsize
       resp_perp = ABS(angle2)
       delta = 180.0 / spinper
       if (dist_par .GT. (collsize + delta)) return
       if (dist_par .GT. (collsize - delta)) then
          resp_par = (dist_par - (collsize + delta))**2 / 
     +             collsize / 4.0 / delta
       else if (dist_par .gt. delta) then
          resp_par = (collsize - dist_par) / collsize
       else
          resp_par = 1.0 - ((dist_par**2 + delta**2) / 
     +                     (2.0 * collsize * delta))
       endif
       resp = resp_perp * resp_par

       return

       end

C---------------------------------------------------------------->
c...calculates the orbital pole of the vela 5b satellite at any given
c  time. uses known positions and times with linear interpolations
c  between these points. data obtained from jim terrell.

       subroutine pole (time, long, lat)

       integer poledate(98)

       real longpk(98), latpk(98)
       real long, lat, long_add, lat_add
       real deltatime, deltadate, deltafrac

       double precision time

       DATA poledate/40368,40380,40410,40440,40470,40500,40531,
     +   40560,40590,40620,40650,40680,40710,40740,40770,40800,
     +   40840,40870,40900,40930,40960,40990,41020,41050,41080,
     +   41110,41140,41170,41202,41230,41260,41290,41320,41350,
     +   41380,41410,41440,41470,41500,41541,41576,41600,41630,
     +   41660,41690,41720,41750,41780,41810,41840,41870,41900,
     +   41930,41960,41990,42020,42050,42080,42110,42140,42170,
     +   42200,42230,42270,42301,42330,42362,42391,42420,42451,
     +   42480,42510,42540,42570,42600,42631,42660,42690,42721,
     +   42750,42782,42810,42840,42870,42901,42940,42974,43060,
     +   43151,43240,43330,43420,43514,43600,43700,43790,43880,
     +   43970/
       DATA longpk/329.492E0,329.404E0,329.249E0,329.197E0,
     +  329.132E0,328.918E0,328.518E0,328.137E0,327.926E0,
     +  327.885E0,327.897E0,327.651E0,327.156E0,326.769E0,
     +  326.601E0,326.579E0,326.377E0,326.111E0,325.764E0,
     +  325.358E0,325.132E0,325.125E0,324.998E0,324.636E0,
     +  324.249E0,323.906E0,323.696E0,323.656E0,323.447E0,
     +  323.117E0,322.633E0,322.288E0,322.293E0,322.222E0,
     +  321.998E0,321.610E0,321.063E0,320.723E0,320.584E0,
     +  320.439E0,320.094E0,319.764E0,319.270E0,318.952E0,
     +  318.832E0,318.709E0,318.424E0,317.985E0,317.531E0,
     +  317.219E0,317.009E0,316.840E0,316.595E0,316.209E0,
     +  315.748E0,315.349E0,315.118E0,314.962E0,314.692E0,
     +  314.278E0,313.772E0,313.344E0,313.123E0,312.899E0,
     +  312.540E0,312.098E0,311.559E0,311.183E0,311.007E0,
     +  310.807E0,310.464E0,309.962E0,309.444E0,309.038E0,
     +  308.827E0,308.661E0,308.297E0,307.743E0,307.202E0,
     +  306.827E0,306.627E0,306.420E0,305.980E0,305.430E0,
     +  304.873E0,304.398E0,304.160E0,302.883E0,301.743E0,
     +  300.326E0,299.185E0,297.831E0,296.500E0,295.165E0,
     +  293.749E0,292.252E0,290.924E0,289.397E0/
       DATA latpk/-6.560E0,-6.216E0,-5.735E0,-5.291E0,-4.971E0,
     +  -4.785E0,-4.620E0,-4.276E0,-3.818E0,-3.344E0,-3.018E0,
     +  -2.864E0,-2.685E0,-2.317E0,-1.848E0,-1.407E0,-1.026E0,
     +  -.859E0,-.747E0,-.364E0,.134E0,.544E0,.752E0,.851E0,1.107E0,
     +  1.515E0,2.077E0,2.454E0,2.575E0,2.683E0,2.845E0,3.253E0,
     +  3.774E0,4.132E0,4.316E0,4.418E0,4.606E0,5.009E0,5.408E0,
     +  5.868E0,6.007E0,6.076E0,6.317E0,6.745E0,7.206E0,7.484E0,
     +  7.592E0,7.736E0,8.008E0,8.373E0,8.758E0,9.051E0,9.209E0,
     +  9.325E0,9.514E0,9.843E0,10.257E0,10.593E0,10.749E0,10.812E0,
     +  10.989E0,11.335E0,11.739E0,12.060E0,12.193E0,12.262E0,
     +  12.441E0,12.857E0,13.234E0,13.484E0,13.529E0,13.562E0,
     +  13.779E0,14.161E0,14.541E0,14.778E0,14.796E0,14.772E0,
     +  14.995E0,15.462E0,15.820E0,15.989E0,15.978E0,15.987E0,
     +  16.260E0,16.676E0,17.011E0,17.087E0,18.072E0,18.101E0,
     +  18.934E0,19.005E0,19.803E0,19.753E0,20.454E0,20.416E0,
     +  21.024E0,20.869E0/

       do 100 i = 1,98
          if (INT(time) .lt. poledate(i)) go to 200
          if (INT(time) .eq. poledate(i)) then
             long = longpk(i)
             lat = latpk(i)
             return
          endif
 100   continue

 200   deltadate = REAL(poledate(i) - poledate(i-1))
       deltatime = REAL(INT(time) - poledate(i-1))
       deltafrac = deltatime / deltadate
       long_add = (longpk(I) - longpk(I-1)) * deltafrac
       long = longpk(i-1) + long_add
       lat_add = (latpk(i) - latpk(i-1)) * deltafrac
       lat = latpk(i-1) + lat_add

       return

       end

C---------------------------------------------------------->
C NOTE: angular inputs and outputs are in degrees

       subroutine azim(long1, lat1, long2, lat2, A, D)

       real long1, long2, lat1, lat2, A, D
       real deltalong, sinlat1, coslat1, sinlat2, coslat2
       real sindltlg, cosdltlg, sindca, sindsa, sind, cosd
       real rad2deg, deg2rad, pi

       parameter (pi = 3.14159265359)
       parameter (rad2deg = 180.0 / pi)
       parameter (deg2rad = 1.0 / rad2deg)


       deltalong = (long2 - long1) * deg2rad
       sinlat1 = SIN(lat1*deg2rad)
       coslat1 = COS(lat1*deg2rad)
       sinlat2 = SIN(lat2*deg2rad)
       coslat2 = COS(lat2*deg2rad)
       sindltlg = SIN(deltalong)
       cosdltlg = COS(deltalong)
       sindca = (sinlat2 * coslat1) - (coslat2 * sinlat1 * cosdltlg)
       sindsa = coslat2 * sindltlg
       sind = SQRT(sindca**2 + sindsa**2)
       cosd = (sinlat2 * sinlat1) + (coslat2 * coslat1 * cosdltlg)
       D = rad2deg * ATAN2(sind,cosd)
       if (sind .EQ. 0.0) then
          A = 0.0
       else
          A = rad2deg * ATAN2(sindsa,sindca)
       endif

       return

       end
