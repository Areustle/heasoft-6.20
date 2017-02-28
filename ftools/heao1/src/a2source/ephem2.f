**==ephem.spg  processed by SPAG 4.50J  at 11:08 on 28 Oct 1998
 
 
      SUBROUTINE EPHEM2(Td,Hjaye)
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
C
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
