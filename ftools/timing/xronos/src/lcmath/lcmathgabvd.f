      subroutine lcmathgabdv( lui, deadapp, vignapp, corap, coraper )
      implicit none
c
c Read a xronos fits file header for LCMATH and Get Applied Background, 
c Deadtime and Vignetting corrections.  
c
c This routine looks up VIGNET and DEADC. The values of these keywords
c are assumed to lie between 0 and 1, i.e., they should be efficiencies.
c If either value is > 1, the routine assumes that the reciprocal is
c the efficiency.
c
c If either correction has been applied, the routine stores the
c applied efficiencies in corap, and corapb
c
c Only constant corrections (those listed as keywords) are considered in 
c this routine.  Those that change with time (and should therefore be 
c listed as a column in the FITS binary table) are not.
 
c   I  lui     (i)  = lu of input fits file
c   I  deadapp (l)  = whether deadtime corrections have been applied
c   I  vignapp (l)  = whether collimator corrections have been applied
c   O  corap   (r)  = Applied correction for counts or rates
c   O  coraper (r)  = Applied correction for errors

c
c This is a modified version of xrfrgbvd.f by
c Eric Lufkin, HEASARC/GSFC, October, 1993
c
c This routine basically returns the opposite of xrfrgbvd - returns the
c applied correction, instead of the correction to apply. This is
c useful for retrieving the original counts from corrected data
c

      character(16) comm
      
      integer lui, kystat
      
      logical deadapp, vignapp
     
      real    deadc, vignet, corap, coraper

c Default values.

      deadc  = 1.
      vignet = 1.

      kystat = 0
      CALL ftgkye( lui, 'DEADC   ', deadc, comm, kystat )
      if ( kystat .ne. 0 ) then
          deadc = 1.
      endif
      kystat = 0
      CALL ftgkye( lui, 'VIGNET  ', vignet, comm, kystat )
      if ( kystat .ne. 0 ) then
          vignet = 1.
      endif
      kystat = 0

c Insist that vignet and deadc are efficiencies.

      if ( vignet .gt. 1. ) vignet = 1. / vignet
      if ( deadc  .gt. 1. ) deadc  = 1. / deadc 

c return only corrections already applied

      if ( deadapp .and. vignapp ) then
          corap = ( deadc * vignet )
      elseif ( deadapp ) then
          corap = deadc
      elseif ( vignapp ) then
          corap = vignet
      else
          corap = 1.
      endif

c Correction for errors.

      coraper = corap

      return
      end
