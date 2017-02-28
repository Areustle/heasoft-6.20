**==up5078.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998
C MTRANS.FOR
C
C These routines do coordinate transformations
C
      SUBROUTINE UP5078(Ra5,Dc5,Ra7,Dc7)

c Ra5  I    r*8      Source RA in degrees (1950 equinox)
c Dc5  I    r*8      Source Dec in degrees (1950 equinox)
c Ra7  O    r*8      Source RA in degrees (1978 equinox)
c Dc7  O    r*8      Source Dec in degrees (1978 equinox)

      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL*8 cos62 , dc , Dc5 , Dc7 , ra , Ra5 , Ra7 , rm1 , rn1 , rtd , 
     &       rtd36 , sin62
C*** End of declarations inserted by SPAG
      DATA rtd , sin62 , cos62/57.29577951D0 , .8878153851D0 , 
     &     .4601997848D0/
C
      ra = Ra5/rtd
      dc = Dc5/rtd
      rtd36 = 206264.8062
      rm1 = -1267.82/rtd36
      rn1 = -551.14/rtd36
      Ra7 = ra - rm1 - rn1*DSIN(ra)*DTAN(dc)
      Dc7 = dc - rn1*DCOS(ra)
      Ra7 = Ra7*rtd
      Dc7 = Dc7*rtd
      RETURN
      END
**==ratoec.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998


      SUBROUTINE RATOEC(Ra7,Dc7,L,B)

c Ra7  I    r*8      Source RA in degrees (1978 equinox)
c Dc7  I    r*8      Source Dec in degrees (1978 equinox)
c L    O    r*8      ecliptic longitude
c B    O    r*8      ecliptic latitude

      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL*8 B , cos62 , cosb , cosd , cose , cosl , dc , Dc7 , e , L , 
     &       ra , Ra7 , rtd , sin62 , sina , sinb , sind , sine , sinl
C*** End of declarations inserted by SPAG
      DATA rtd , sin62 , cos62/57.29577951D0 , .8878153851D0 , 
     &     .4601997848D0/
      e = 23.442209/rtd
      ra = Ra7/rtd
      dc = Dc7/rtd
      cosd = DCOS(dc)
      sind = DSIN(dc)
      cose = DCOS(e)
      sine = DSIN(e)
      sina = DSIN(ra)
      sinb = sind*cose - cosd*sine*sina
      cosb = DSQRT(1.-sinb*sinb)
      cosl = cosd*DCOS(ra)/cosb
      sinl = (sind*sine+cosd*cose*sina)/cosb
      B = DASIN(sinb)*rtd
      L = 0.
      IF ( (sinl.NE.0.) .AND. (cosl.NE.0.) ) L = DATAN2(sinl,cosl)*rtd
      IF ( L.LT.0. ) L = L + 360.
      RETURN
      END

**==ephem.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998

      SUBROUTINE EPHEM(Td,Hjaye)


      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL g , pi , pi2 , pihaf , vdg , vldeg , vlmna , vlsca , vmn , 
     &     w , y4d , yd , yr
      INTEGER i2pi , ideg , iy4 , num
C*** End of declarations inserted by SPAG
C THIS SR COMPUTES THE SUN ANGLES AT MID-TIME OF ORBIT
      DIMENSION Td(5)
      DOUBLE PRECISION tdd , tdf , t , vlts , vlt2s , vimn , vsc
      DOUBLE PRECISION Td , Hjaye
      DATA pi/3.141593/ , w/.0174533/ , pi2/6.283185/ , pihaf/1.570796/

c  Initialize to avoid warning
      vldeg = 0.
      vlmna = 0.
      vlsca = 0.
c  --
C      COMPUTE JULIAN DATE
      yr = Td(1) - 1900.
      iy4 = yr/4.
      y4d = yr - FLOAT(iy4)*4.
      yd = FLOAT(iy4)*1461. + y4d*365.
      tdf = Td(3)/24. + Td(4)/1440. + Td(5)/86400.
      tdd = yd + Td(2) - 0.5
      t = (tdd+tdf)/36525.
      vlts = 1.2960276813D8*t
      vlt2s = 1.089*t*t
      vsc = vlts + vlt2s + 48.04
      vdg = 279.0
      vmn = 41.0
      ASSIGN 100 TO num
      GOTO 500
C
 100  vldeg = vdg
      vlmna = vmn
      vlsca = vsc
      vsc = 1.3935*t*t + 27.54 + vlts
      vdg = 279.0
      vmn = 41.
      ASSIGN 200 TO num
      GOTO 500
C
C      THE FOLLOWING COMPUTES THE MEAN ANOMOLY OF THE EARTH
 200  vsc = 33. + .1295965791D9*t - .54*t**2 - .012*t**3
      vdg = 358.0
      vmn = 28.0
      ASSIGN 300 TO num
      GOTO 500
C
 300  g = (vdg+vmn/60.+vsc/3600.)*w
C      THE  FOLLOWING  COMPUTES  THE  SUN S  LONGITUDE  ( ECLLIPTIC )
      vdg = vldeg
      vmn = vlmna
      vsc = SIN(g)*(6910.057-17.240*t-0.052*t*t) + SIN(2.*g)
     &      *(72.338-0.361*t) + SIN(3.*g)*(1.054-0.001*t) + SIN(4.*g)
     &      *0.018 + vlsca
      ASSIGN 400 TO num
      GOTO 500
C
 400  Hjaye = vdg + vmn/60. + vsc/3600.
      RETURN
C
 500  vimn = AINT(vsc/60.)
      vmn = vmn + vimn
      vsc = vsc - vimn*60.
      ideg = vmn/60.
      vdg = vdg + FLOAT(ideg)
      vmn = vmn - FLOAT(ideg)*60.
      i2pi = vdg/360.
      vdg = vdg - FLOAT(i2pi)*360.
      IF ( vsc.LT.0 ) THEN
         vsc = vsc + 60.
         vmn = vmn - 1.
         IF ( vmn.LT.0 ) THEN
            vmn = vmn + 60.
            vdg = vdg - 1.
            IF ( vdg.LT.0 ) vdg = vdg + 360.
         ENDIF
      ENDIF
      GOTO num
      END
**==sunday.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998

C this routine computes the days when the sun is at a given longitude
      SUBROUTINE SUNDAY(Alon,Days)

c Alon    I   r*4      longitude of sun (radians)
c Days    O   r*4      day when sun has above longitude (day of 1977)

      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL Alon , alon1 , day1 , Days , dgdy , dl , pi , pi2 , sloff , 
     &     sndy
      INTEGER i
C*** End of declarations inserted by SPAG
C  COMPUTES DAYS WHEN SUN IS AT GIVEN LONGITUDE
C  SUNDAY IS DAY OF 1977. ALON IS IN RADIANS.
C  ACCURACY IS 0.01 DAYS.
      DIMENSION Days(3)
      REAL*8 td(5) , hjaye
      DATA td/1977. , 0. , 0. , 0. , 0./
      DATA pi , pi2/3.14159 , 6.28319/
      DATA dgdy , sloff/0.017202 , 1.40411/
      day1 = (Alon+sloff)/dgdy
C  THIS IS APPROX DAY, RANGES FROM 81 TO 446
      IF ( day1.GT.370. ) day1 = day1 - 365.25
      DO i = 1 , 3
         DO WHILE ( .TRUE. )
            sndy = day1
            td(2) = sndy
            CALL EPHEM(td,hjaye)
            alon1 = hjaye/57.296
            dl = Alon - alon1
            IF ( dl.LT.-pi ) dl = dl + pi2
            IF ( dl.GT.pi ) dl = dl - pi2
            day1 = sndy + dl/dgdy
            IF ( ABS(day1-sndy).LE..01 ) THEN
               Days(i) = day1
               day1 = day1 + 365.25
               GOTO 100
            ENDIF
         ENDDO
 100  ENDDO
      RETURN
      END
