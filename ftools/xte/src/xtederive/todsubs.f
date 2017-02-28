c
c  Subroutine radecbore(quat,borexte,sun,ra,dec,boreinert,roll) Uses the attitude
c  matrix to transform the boresight vector in XTE (body) coordinates 
c  to inerital (celestial) coordinates (RA and DEC). 
c  quat(4) contains the quaternions, borexte(3) the components of the boresight in 
c  the spacecraft frame (stored in an auxiliary data file). On exit boreinert(3)
c  contains the inertial, x,y,z components of the boresight and ra and dec are the
c  right ascension and declination of the boresight. roll contains the roll bias angle
c  offset in degrees, positive roll from 0 to 180 is clockwise from XTE z-axis, 
c  negative roll from 0 to -180 is anticlockwise from XTE Z-axis. sun contains the
c  inertial solar position vector from telemetry. NOTE: sun is NOT a unit vector.
c  Author: Tod Strohmayer (USRA/GSFC)
c  Development:  6/20/95  add roll bias angle computation
c
      subroutine radecbore(quat,borexte,sun,ra,dec,boreinert,roll)
      implicit real*4 (a-h,o-z)
      dimension boreinert(3),att(3,3),quat(4),borexte(3),sun(3),
     1sunbody(3)
c
c   set up attitude matrix (direction cosine matrix)
c
      q11=quat(1)*quat(1)
      q22=quat(2)*quat(2)
      q33=quat(3)*quat(3)
      q44=quat(4)*quat(4)
      q12=quat(1)*quat(2)
      q34=quat(3)*quat(4)
      q13=quat(1)*quat(3)
      q24=quat(2)*quat(4)
      q23=quat(2)*quat(3)
      q14=quat(1)*quat(4)
c
      att(1,1)=q11-q22-q33+q44
      att(1,2)=2.0*(q12+q34)
      att(1,3)=2.0*(q13-q24)
c
      att(2,1)=2.0*(q12-q34)
      att(2,2)=-q11+q22-q33+q44
      att(2,3)=2.0*(q23+q14) 
c
      att(3,1)=2.0*(q13+q24)
      att(3,2)=2.0*(q23-q14)
      att(3,3)=-q11-q22+q33+q44
c
c  Do Transformation of boresight in body to inertial reference (J2000)
c
        do 3 i=1,3
          boreinert(i)=0.0
          do 4 j=1,3
            boreinert(i)=boreinert(i)+att(j,i)*borexte(j)
4         continue
3       continue
      pi=3.141592653589793
      p2=2.0*pi
      crd=180.0/pi
c
c     find dec coordinate of boresight
c
        dec=crd*asin(boreinert(3))
c
c     find ra coordinate of boresight
c
       ay=boreinert(2)
       ax=boreinert(1)
       bx=abs(boreinert(1))
       by=abs(boreinert(2))
          if (ax.eq.0.0) then
            if (ay.gt.0.0) then 
               ra=pi/2.0
            end if
            if (ay.lt.0.0) then
               ra=(3.0*pi)/2.0
            end if
          end if
c         
          if ((ax.gt.0.0).and.(ay.ge.0.0)) then
            ra=atan(by/bx)
          end if
          if ((ax.lt.0.0).and.(ay.ge.0.0)) then
            ra=pi-atan(by/bx)
          end if
          if ((ax.lt.0.0).and.(ay.le.0.0)) then
            ra=pi+atan(by/bx)
          end if
          if ((ax.gt.0.0).and.(ay.le.0.0)) then
            ra=p2-atan(by/bx)
          end if
          ra=ra*crd
c
c  compute roll angle 
c
c   transform solar vector to spacecraft frame
c    first scale by modulus
       xnorm=0.0
       do 7 i=1,3
         xnorm=xnorm+sun(i)*sun(i)
7      continue
      xnorm=sqrt(xnorm)
      do 8 i=1,3
        sun(i)=sun(i)/xnorm
8     continue
c
c   transform to spacecraft frame
c
        do 9 i=1,3
          sunbody(i)=0.0
          do 10 j=1,3
            sunbody(i)=sunbody(i)+att(i,j)*sun(j)
10         continue
9       continue
c
c  
       ay=sunbody(2)
       az=sunbody(3)
          if (ay.eq.0.0) then
            if (az.gt.0.0) then 
               roll=0.0
            end if
            if (az.lt.0.0) then
               roll=pi
            end if
          end if
c         
          if ((ay.gt.0.0).and.(az.ge.0.0)) then
            roll=atan(ay/az)
          end if
          if ((ay.lt.0.0).and.(az.ge.0.0)) then
            roll=atan(ay/az)
          end if
          if ((ay.lt.0.0).and.(az.le.0.0)) then
            roll=-(pi-atan(ay/az))
          end if
          if ((ay.gt.0.0).and.(az.le.0.0)) then
            roll=atan(ay/az)+pi
          end if
          roll=roll*crd
      return
      end
c
c  Subroutine bsearth(rxte,boreinert,bselimb) computes the angle between the 
c  inertial boresight vector and the negative of the spacecraft position vector.
c  The output angle ranges from -half angle subtended by earth to 180-half angle
c  subtended by earth, in degrees.  rxte contains the x,y,z components of the 
c  spacecraft position vector. boreinert contains the components of the boresight
c  in the inertial frame, obtained from the above routine (ie, call radecbore 
c  before you call this routine). bselimb is the output angle
c
c  Author: Tod Strohmayer (USRA/GSFC)
c
      subroutine bsearth(rxte,boreinert,bselimb)
      implicit real*4 (a-h,o-z)
      dimension rxte(3),boreinert(3)
      re=6378.16
      pi=3.141592653589793
      crd=180.0/pi
c
c   compute angle between boresight (boreinert) and -xte position vector
c
      xnorm=0.0
      dot=0.0
      do 2 i=1,3
        xnorm=xnorm+(rxte(i)*rxte(i))
2     continue
      xnorm=sqrt(xnorm)
c     
      do 1 i=1,3
        dot=dot+(-rxte(i)*boreinert(i))
1     continue
      dot=dot/xnorm
      cang=acos(dot)
c
c    compute half angle subtended by earth
c
      csub=asin(re/xnorm)
      cout=crd*csub
c      print*,cout
c
c    output angle
c
      bselimb=cang-csub
      bselimb=crd*bselimb
      return
      end
