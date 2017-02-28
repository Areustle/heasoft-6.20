C This subroutine computes the collimator response function for each 
C   of the user-specified sources.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.0   2 Mar 1995  First draft
C          1.1  12 Sep 1995  Altered to allow 20 sources
C          1.11 11 Dec 1995  Removed begining and ending times from common

      subroutine exposure(r, obstime)

C Common block declarations

      common /SOURCE/ long_src, lat_src, longcntr, latcntr, longmin, 
     +  longmax, latmin, latmax, longavg, latavg, counts, backgnd, 
     +  frate, numpnts, nsrc, imax, jmax
      common /NAME/ sourcename

      integer nsrc, imax, jmax, numpnts(20,20)
      real long_src(20), lat_src(20)
      real longcntr(20,20), latcntr(20,20)
      real counts(20,20), backgnd(20,20)
      real longmin, longmax, latmin, latmax, longavg, latavg, frate
      character(16) sourcename(20)

C Local variables

      integer i, j, k, ifine, jfine

      real long_pole, lat_pole, longf, latf, az_pole, dist_pole
      real resp, spinper
      data spinper/63.6/

      double precision r(20,20,23), obstime


C Calculate exposures r(i,j,k) of each map bin to each source;
C i.e., collimator transmission at that bin for that source.
C First, calculate the position (long_pole, lat_pole) of the spacecraft
C orbit pole

      call pole(obstime, long_pole, lat_pole)

C Calculate the response of each grid point.  Response is averaged over
C 2*2 points within each bin.

      do 700 i = 1, imax
         do 600 j = 1, jmax
            do 100 k = 1, nsrc
               r(i,j,k) = 0.0D0
 100        continue
            do 400 ifine = -1, 1, 2
               do 300 jfine = -1, 1, 2
                  longf = longcntr(i,j) + (ifine * 0.5774D0)
                  latf = latcntr(i,j) + (jfine * 0.5774D0)
                  call azim(longf, latf, long_pole, lat_pole, az_pole,
     +                 dist_pole)
                  do 200 k = 1, nsrc 
                     call colcalc(10.0, .FALSE., resp, longf, latf, 
     +                    long_src(k), lat_src(k), az_pole, dist_pole, 
     +                    spinper)
                     r(i,j,k) = r(i,j,k) + DBLE(resp)
 200              continue
 300           continue
 400        continue
            do 500 k = 1, nsrc
               r(i,j,k) = r(i,j,k) / 4.0D0
 500        continue
 600     continue
 700  continue

      return

      end
