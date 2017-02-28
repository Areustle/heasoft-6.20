c
      subroutine xrftgbdv(lui,deadapp,deadc,vignapp,vignet,rdty,rdtsy
     &                   ,expos)
      implicit none
c
c Read a XRonos FiTs file header and Get Background, Deadtime
c and Vignetting corrections.  

c This routine looks up two types of keywords in the current FITS header.

c First it asks whether DEADAPP and VIGNAPP are present.  If they are not
c present, the routine considers their value to be .false..

c Next it looks up VIGNET and DEADC.  The values of these keywords are
c assumed to lie between 0 and 1, i.e., they should be efficiencies.
c If either value is > 1, the routine assumes that the reciprocal is
c the efficiency.

c If either correction has not been APPlied, the routine then stores the
c reciprocal of the product of the unapplied efficiencies in rdty, which can 
c then be used as a correction factor for count rates in the calling routine.

c Only constant corrections (those listed as keywords) are considered in 
c this routine.  Those that change with time (and should therefore be 
c listed as a column in the FITS binary table) are not.

c For missions with sampling errors an additional keyword DEADERR is included
c in the correction factor for the error rdtsy.
 
c 09/27/93 : Background part not in place.
c
c   I  lui     (i)  = lu of input fits file
c   O  deadapp (l)  = whether deadtime corrections have been applied
c   O  deadc   (r)  = Vignetting correction factor
c   O  vignapp (l)  = whether collimator corrections have been applied
c   O  vignet  (r)  = Deadtime correction factor
c   O  rdty    (r)  = Correction for counts or rates
c   O  rdtsy   (r)  = Correction for errors
c   O  expos   (r)  = Default exposure constant (ignored in calling
c                       routine if the DEADC column is present).

c Author: Eric Lufkin, HEASARC/GSFC, October, 1993

      character(16) comm
      integer lui, kystat
      logical deadapp, vignapp
      real deadc, vignet, rdty, rdtsy, expos, deaderr

c Default values.

      deadapp = .false. 
      vignapp = .false. 
      deadc = 1.
      vignet = 1.

      kystat = 0
      CALL ftgkye(lui,'DEADC   ',deadc   ,comm,kystat) 
      kystat = 0
      CALL ftgkye(lui,'VIGNET  ',vignet  ,comm,kystat)
      kystat = 0
      CALL ftgkyl(lui,'DEADAPP ',deadapp ,comm,kystat)
      kystat = 0
      CALL ftgkyl(lui,'VIGNAPP ',vignapp ,comm,kystat)

c Insist that vignet and deadc are efficiencies.

      if(vignet.gt.1.) vignet = 1./vignet
      if(deadc .gt.1.) deadc  = 1./deadc 

c Apply only those corrections that are not already applied.

      if(deadapp.and.vignapp) then
         rdty = 1.
      elseif(deadapp) then
         rdty = 1./       vignet
      elseif(vignapp) then
         rdty = 1./ deadc
      else
         rdty = 1./(deadc*vignet)
      endif

c Correction for errors.

      rdtsy = rdty

c Special value for missions with sampling corrections.

      kystat = 0
      CALL ftgkye(lui,'DEADERR ',deaderr ,comm,kystat)
      if(kystat.eq.0) rdtsy = rdtsy * deaderr

c Default exposure value.

      expos =       deadc*vignet

      return
      end
