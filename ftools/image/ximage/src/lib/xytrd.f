      SUBROUTINE XYTRD(Ex,Ey,Del,Ra,Dec,Ra0,Dec0,Theta,Oldang,Newang)
      implicit none
c
c return ra and dec for a given x, y  pixel position
c I    EX,EY      (r)  LINEARIZED DETECTOR COORDS OF SOURCE
c I    DEL        (d)  SIZE OF 1 UNIT OF EX,EY (DEGREES)
c I    RA0,DEC0   (d)  RA,DEC (1950.0) OF EX=0,EY=0
c I    THETA      (d)  ORIENTATION ANGLE OF NORTH VECTOR (IN EX,EY FRAME)
c O    RA,DEC     (d)  RA,DEC (1950.0) OF SOURCE                          (O/P
c I/O  OLDANG     (r)  ANGLE OF MAJOR AXIS OF SOURCE TO EX AXIS
c I/O  NEWANG     (r)  POSITION ANGLE OF MAJOR AXIS TO LOCAL NORTH VECTOR (O/P
c
c oldang and newang    dummy varaibles
c ex ey and del are the pixel values of the reference pixel 
c in image coodinates and increment pixels size in degrees
c ra0 and dec0 are the value of the reference pixel 
c ra and dec is the output 
c theta is the roll 

      REAL*4 Ex , Ey , Oldang , Newang
      REAL*8 zx , zy , pi2 , atheta , x , sinr , cosr , cosd , fract
      REAL*8 cs , sn , csx , snx , y , r , Ra , Dec0 , sind , frc
      REAL*8 Dec , omega , t1 , Theta , Ra0 , cosa , sina , tana
      REAL*8 Del(2)
c
      IF ( Del(1).EQ.0 .OR. Del(2).EQ.0 ) THEN
         Ra = Ra0
         Dec = Dec0
         RETURN
      ENDIF


      atheta = Theta
      pi2 = DACOS(0.D0)/90.D0
      zx = Ex
      zy = Ey
      cs = DCOS(Dec0*pi2)
      sn = DSIN(Dec0*pi2)
      csx = DCOS(atheta*pi2)
      snx = DSIN(atheta*pi2)
      y = (zx*csx+zy*snx)*(-Del(1))*pi2
      x = (zy*csx-zx*snx)*Del(2)*pi2
      r = DSQRT(x*x+y*y)
      sinr = DSIN(r)
      IF ( r.EQ.0.D0 ) THEN
C.....COMMENT  RA,DEC OF SOURCE = RA,DEC OF ORIGIN
         Ra = Ra0
         Dec = Dec0
         Newang = Oldang - Theta
      ELSE
         fract = sinr/r
         cosr = DCOS(r)
         sind = cosr*sn + fract*cs*y
         cosd = DSQRT(1.D0-sind*sind)
         IF ( cosd.EQ.0.D0 ) THEN
C.....COMMENT  SOURCE AT 1 OF THE POLES
            Dec = DSIGN(90.D0,sind)
            Ra = Ra0
            cosa = 1.D0
            sina = 0.D0
         ELSE
            Dec = DATAN(sind/cosd)/pi2
            sina = fract*x/cosd
            cosa = (cosr*cs-fract*sn*y)/cosd
            Ra = Ra0 + DATAN2(sina,cosa)/pi2
         ENDIF
c
c mod to fix -ve declinations around zero RA..... Nick 1/31/95
c
         if(ra.lt.0.0)then
          ra = ra + 3.6D+02
         endif
c
         frc = (cosr-1.D0/fract)/r/sinr
         IF ( cosa.EQ.0.D0 ) THEN
            t1 = x*cs*frc
            IF ( t1.EQ.0.D0 ) THEN
               omega = 90.D0
            ELSE
               omega = DATAN(sn/t1)/pi2
            ENDIF
         ELSE
            tana = sina/cosa
            t1 = (1.D0-tana*x*cs*frc)
            IF ( t1.EQ.0.D0 ) THEN
               omega = 90.D0
            ELSE
               omega = DATAN(-tana*(sn-cs*y*frc)/t1)/pi2
            ENDIF
         ENDIF
         Newang = Oldang - Theta - omega
      ENDIF
C.....COMMENT  STANDARDISE NEWANG (MAKE SURE 0..LE.NEWANG.LT.180.)
C.....THIS PART REMOVED SINCE I WILL NOT USE IT
      RETURN
      END
