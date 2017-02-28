
      FUNCTION xrtefa(e_min, e_max, theta, phi)

      REAL xrtefa, e_min, e_max, theta, phi

c Function to return the XRT effective area in the e_min to e_max
c bin. Calls the XRT team function XRTEA95b but integrates over the messy
c region between 2 and 4 keV.

c Arguments :
c     e_min       r         i: Start energy of bin
c     e_max       r         i: End energy of bin
c     theta       r         i: Angular distance from optical axis
c     phi         r         i: Azimuthal angle

      INTEGER n
      REAL    energy, step

c function:

      DOUBLE PRECISION XRTEA95B
      EXTERNAL XRTEA95B

      step = e_max - e_min

c If the energy is in the messy range and the size of the bin exceeds 10 eV
c then integrate

      IF ( ( e_min .GT. 2.0 ) .AND.
     &     ( e_max .LT. 4.0 ) .AND.
     &     ( step .GT. 0.01 ) ) THEN

         DO WHILE ( 0.01 .LT. step )
            step = 0.5 * step
         ENDDO
         energy = e_min + 0.5*step

         n = 0
         xrtefa = 0.0
         DO WHILE ( energy .LT. e_max )
            xrtefa = xrtefa + SNGL(XRTEA95B(DBLE(energy), 
     &                                      DBLE(theta), DBLE(phi)))
            energy = energy + step
            n = n + 1
         ENDDO

         xrtefa = xrtefa / n

      ELSE

c otherwise just take the area at the middle of the bin

         energy = 0.5 * (e_min + e_max)
         xrtefa = SNGL(XRTEA95B(DBLE(energy), DBLE(theta), DBLE(phi)))

      ENDIF


      RETURN
      END




