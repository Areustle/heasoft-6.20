c======================================
      Real Function xrt_eff_area(e_min, e_max, theta, phi)
c======================================
      Implicit None
c input:
      Real e_min, e_max
      Real theta, phi
c output:
c local:
      Integer n
      Real energy, step
c function:
      DOUBLE PRECISION xrtea95b
      EXTERNAL xrtea95b
c
      step = e_max - e_min
      If ( ( 2.0 .lt. e_min ) .and.
     &     ( e_max .lt. 4.0 ) .and.
     &     ( 0.01 .lt. step ) ) then
         Do while ( 0.01 .lt. step )
            step = 0.5 * step
         End do
         energy = e_min + 0.5*step
         n = 0
         xrt_eff_area = 0.0
         Do while ( energy .lt. e_max )
            xrt_eff_area = xrt_eff_area + SNGL(xrtea95b(DBLE(energy), 
     &                                    DBLE(theta), DBLE(phi)))
            energy = energy + step
            n = n + 1
         End do
         xrt_eff_area = xrt_eff_area / n
      Else
         energy = 0.5 * (e_min + e_max)
         xrt_eff_area = SNGL(xrtea95b(DBLE(energy), DBLE(theta), 
     &                       DBLE(phi)))
      End if
c
      Return
      End





