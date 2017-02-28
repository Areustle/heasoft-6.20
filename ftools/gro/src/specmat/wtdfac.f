*
* WTDFAC
*
* Interpolate the energy dependent area scaling ("Kumar") factors for
* nonstandard energy bands.
*
* This function is based on Dave Bertsch's subroutine WTDFACTR.
* The chief difference is that this function returns the factor for
* a single energy band, while Dave's subroutine calculates a vector
* of values.
*
* Calling sequence:
*   factor = wtdfac(elo, ehi, sf)
*
*   inputs:
*     elo: (real) Lower edge of nonstandard energy band
*     ehi: (real) Upper edge of nonstandard energy band
*     sf:  (real(10)) Array of scale factors of standard bands
*
*   returned value:  (real) interpolated scale factor for specified band
*
* Written by Patrick Nolan, Stanford University
*  for Spectral version 2.10
*  October 1995

C  Modifications
*  -------------
CH6  1.  D.L.Bertsch  11/09/00
CH6
CH6  Made a slight change in the weighting scheme that will be used 
CH6  for viewing periods 4280 and later (last full gas fill.)  This 
CH6  scheme correctly uses piecewise spectral coefficients.  The
CH6  same result will be obtained for data prior to VP4280.  The
CH6  difference between the old and new scheme is slight.
CH6
CH6  This change required adding vp to the call list and to 
CH6  modify the calling program, scalef.



C  @(#) wtdfac.f 1.1@(#)

      real function wtdfac(vp,elo,ehi,sf)
      implicit none
      real elo,ehi,sf(10)
      real e1,e2,sexp,wt,sum_wt,sum_fwt,ratio
      integer i,imin,imax
      real stdbnd(11), spindx(10), Fspec(10)
      logical first
      character(4) vp

      save

      data stdbnd/30.,50.,70.,100.,150.,300.,500.,1000.,2000.,4000.,
     >     10000./
      data spindx/5*1.7, 2*1.9, 3*2.4/, first/.true./


C==>   Generate the factors for the integral spectrum used in weighting.
*      The first factor is arbitarily 1.0 (used in a ratio) and the 
*      rest are set to make the spectrum piecewise continuous.
       

       if( first ) then
          Fspec(1) = 1.0
          do i = 1, 9
             Fspec(i+1) = Fspec(i)*(1.0-spindx(i))/(1.0-spindx(i+1))
     &			  *stdbnd(i+1)**(spindx(i+1)-spindx(i))
          end do
          first = .false.
       endif


      imin = 1
      imax = 1
      do i = 1,10
         if (stdbnd(i  ).le.elo) imin = i
         if (stdbnd(i+1).lt.ehi) imax = i+1
      end do
      imax = min(imax,10)

      sum_wt = 0.
      sum_fwt = 0.

      do i = imin,imax
         e2 = min(ehi,stdbnd(i+1))
         e1 = max(elo,stdbnd(i  ))
         sexp = 1.0 - spindx(i)

         if( vp.ge.'4280' ) then     ! after the last gas fill
            wt = Fspec(i) * (e1**sexp - e2**sexp)
            sum_fwt = sum_fwt + wt * sf(i)
         else
            wt = e1**sexp - e2**sexp
            sum_fwt = sum_fwt + wt/max( 0.010,sf(i) )
         endif

         sum_wt = sum_wt + wt

      end do

      if( vp.ge.'4280' ) then
          wtdfac =  sum_fwt/sum_wt
      else
          if (sum_fwt.ne.0.) then
              ratio = sum_wt / sum_fwt
              wtdfac = min(2.0,max(ratio,0.01))
          else
              wtdfac = 1.0
          endif
      endif

      return
      end
