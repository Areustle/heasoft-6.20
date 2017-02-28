c
c  Dust scattering model (multiplicative) from Fred Baganoff
c
c  M(E) = exp(-tau_scatt(E))
c
c  where
c
c  tau_scatt = N_dust*sigma_dust = 0.486(NH/10^22 cm^-2)(E/keV)^-2
c
c  is based on relations in Predehl & Schmitt (1995, A&A, 293, 889).
c
c  par1 = neutral hydrogen column density in units of 10^22 cm^-2
c
c  NB: par1 should be linked to the NH parameter in phabs.
c
      subroutine dustscat(ear,ne,param,ifl,photar,photer)
      integer ne,ifl
      real*4 ear(0:ne),param(*),photar(ne),photer(ne)
    
      integer ie
      real*4 e

      do 10 ie=1,ne
        e = (ear(ie-1) + ear(ie))/2.
        photar(ie)=exp(-0.486*param(1)/e**2)
10    continue

      return
      end

