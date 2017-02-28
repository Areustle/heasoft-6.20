      function xmm_psf(mos, energy, offaxis, rad, status)
c
c  I  mos      (i) MOS# instrument (0=PN)
c  I  energy   (r) Energy in keV
c  I  offaxis  (r) Offaxis in arcmin
c  I  rad      (r) Radius from source center (arcsec)
c  O  status   (i) Error flag (0=OK)
c
c  O  XMM_PSF  (r) PSF at r [Function return value]
c
      real*4 offaxis, rad, xmm_psf
      integer*4 mos, status
c
c  Based on In Flight Calibration of the PSF for the MOS1 and MOS2
c  Cameras (EPIC-MCT-TN-011) by Simona Ghizzardi, October 8, 2001
c
c  Local variables
c
      real*8 a, b, c, d
      real*8 x, y, z, w
      real*8 rcor, alph

      status = 0
      xmm_psf = 0.
c
      if ( mos.eq.0 ) then
         a = 6.636
         b = -0.305
         c = -0.175
         d = -0.0067
         x = 1.525
         y = -0.015
         z = -0.012
         w = -0.0010
      else if ( mos.eq.1 ) then
         a = 5.074
         b = -0.236
         c = 0.002
         d = -0.0180
         x = 1.472
         y = -0.010
         z = -0.001
         w = -0.0016
      else if ( mos.eq.2 ) then
         a = 4.759
         b = -0.203
         c = 0.014
         d = -0.0229
         x = 1.411
         y = -0.005
         z = -0.001
         w = -0.0002
      else
         status = -1
         return
      endif
      
      rcor = a + b*Energy + c*Offaxis + d*Energy*Offaxis
      alph = x + y*Energy + z*Offaxis + w*Energy*Offaxis

c     Original psf formula (unused)
c     xmm_psf = 1./(1. + (rad/rcor)**2)**alph

c     Use integrated PSF
      xmm_psf = 1. - 1./(1. + (rad/rcor)**2)**(alph-1.)

      return
      end
