**==DIR_COS.spg  processed by SPAG 3.09I  at 13:27 on 15 Jan 1992
 
 
      SUBROUTINE DIR_COS(Ra,Dec,Dircos)
c Modified by James Peachey, HEASARC/GSFC/NASA, 22 May, 2000
c   Replaced calls to dcosd and dsind with dcos and dsin.
c   Input angles are converted to degrees prior to performing the
c   trig. This is because dcosd and dsind are not standard f77,
c   and do not exist on all platforms.
c
c  calculates the 3 direction cosines for given ra and dec   nick 29.02.90
c  uses dcosd and dsind which work in degrees
c
      REAL*8 Ra , Dec , Dircos(3)
      REAL*8 Ra_rad, Dec_rad, Rad_per_deg
c Value for pi is from CRC Standard Mathematical Tables, 26th Ed., p. 5
c Retained 17 decimals for double precision
      parameter(Rad_per_deg = 3.14159 26535 89793 2 D0/ 180.0D0)
      Ra_rad = Ra * Rad_per_deg
      Dec_rad = Dec * Rad_per_deg
c
      Dircos(1) = DCOS(Ra_rad)*DCOS(Dec_rad)
c
      Dircos(2) = DSIN(Ra_rad)*DCOS(Dec_rad)
c
      Dircos(3) = DSIN(Dec_rad)
c
      RETURN
      END
