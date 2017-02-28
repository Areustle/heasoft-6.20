
      SUBROUTINE swind1(ear, ne, param, ifl, photar, photer)

      implicit none

      INTEGER ne, ifl
      REAL ear(0:ne), param(*), photar(ne), photer(*)

      character(255) filenm,xspeclocal
      character(255) fgmodf

      integer MAXAR
      parameter (MAXAR = 10000)

      real eparam(3),sparam(2)
      real e(0:MAXAR), p(MAXAR), perr(MAXAR)
      real emin, emax, elo, ehi, z, de
      real e1, e2

      INTEGER i, ix, nex

      INTEGER lenact
      EXTERNAL lenact, fgmodf

c=============

c this model does not calculate errors
      DO i = 1, ne
         photer(i) = 0.0
      ENDDO


      xspeclocal=fgmodf()
      filenm=xspeclocal(:lenact(xspeclocal))//'swind1_mtable.fits'

c     -alpha
c      eparam(1) = param(1)

c     nh in units of 1e22 - param(1)      
      eparam(1) = param(1)*1.e22

c     logxi
      eparam(2) = param(2)

c     redshift
      eparam(3) = param(4)
      z=param(4)

c     Gaussian sigma smearing - renormalised to 1 KeV from 6
c     See gsmooth.
      sparam(1) = 6. * param(3)

c     Sigma Variation law with eneregy - fixed to produce constant deltaE/E
      sparam(2) = 1.


c     Rebinning Grid size - fix at 1000
      nex = 1000

c Extend energy array

      emin = log10(0.1 * ear(1))
      emax = log10(10.0 * ear(ne))
      de = (emax - emin) / real(nex)
      
      if(nex > MAXAR) then
         write(*,*) '### My arrays overflow. Call the plumber ###'
         return
      end if

      do i = 0, nex
         e(i) = 10.0**(emin + i * de)
      end do

      call xsmtbl(e, nex, eparam, filenm, ifl, p, perr)

      
c fill in below/above the energy limits of 0.1 and 18.0 keV
c this is zero emission for the emitter, or unity (zero absorption) for
c the absorber
c and scale column linearly from 6e22

      elo = 0.1 / (z + 1.)
      ehi = 18.0 / (z + 1.)


      DO i = 1, nex
            if (e(i).ge.ehi .or. e(i).le.elo) p(i) = 1.0
      ENDDO

c     convolve with gsco
c     initialise some arrays

      call xsgsmt(e, nex, sparam, ifl, p, perr)

c rebinning
    
      ix = 1
      do i = 1, ne
         photar(i) = 0.0
         emin = ear(i - 1)
         emax = ear(i)
         do while(e(ix) < emin)
           ix = ix + 1
         end do

         do while(e(ix - 1) < emax)
           e1 = max(e(ix - 1), emin)
           e2 = min(e(ix), emax)
           photar(i) = photar(i) + p(ix) * (e2 - e1)
           ix = ix + 1
         end do
         ix = ix - 1

         photar(i) = photar(i) / (emax - emin)

      end do

      RETURN
      END
 


