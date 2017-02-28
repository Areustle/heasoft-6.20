 
      SUBROUTINE STBINS(N_channels, Chan_low, Chan_high, Chan_break,
     &                  Chan_number, Chan_bnumber, N_energies, Resp_low,
     &                  Resp_high, Resp_break, Resp_number, 
     &                  Resp_bnumber, Disperse, Resp_reln, N_e_energies,
     &                  Resp_data, Chan_reln, n_c_energies, Chan_data, 
     &                  Ch_bounds, Resp_energies)

      IMPLICIT NONE

      INTEGER N_channels, N_energies, N_e_energies, N_c_energies
      INTEGER Resp_number, Resp_bnumber, Chan_number, Chan_bnumber
 
      REAL Ch_bounds(N_channels,2), Resp_energies(0:N_energies)
      REAL Resp_data(2, N_e_energies), Chan_data(2, N_c_energies)
      REAL Chan_low, Chan_high, Chan_break
      REAL Resp_low, Resp_high, Resp_break

      CHARACTER Resp_reln*(*), Chan_reln*(*)

      LOGICAL disperse

c Routine to calculate the nominal channel boundaries and the response
c energy bins. Can be either either linear or logarithmic (with the
c option of a break to a different slope) or based on file input.

c Arguments :
c   N_channels       i         i: Number of channels
c   Chan_low         r         i: Start energy/wavelength of first channel
c   Chan_high        r         i: End energy/wavelength of last channel
c   Chan_break       r         i: Energy/wavelength for change of linear slope
c   Chan_number      i         i: Number of channels
c   Chan_bnumber     i         i: Number of channels above Chan_break
c   N_energies       i         i: Number of response energy bins
c   Resp_low         r         i: Start energy/wavelength of first response bin
c   Resp_high        r         i: End energy/wavelength of last response bins
c   Resp_break       r         i: Energy/wavelength for change of slope
c   Resp_number      i         i: Number of response bins
c   Resp_bnumber     i         i: Number of response bins above break
c   disperse         l         i: If true then input in A otherwise  keV
c   Resp_reln        c*(*)     i: Response energy relation
c   N_e_energies     i         i: Number of entries in response definition file
c   Resp_data        r         i: Response definition data
c   Chan_reln        c*(*)     i: Channel relation
c   N_c_energies     i         i: Number of entries in channel definition file
c   Chan_data        r         i: Channel definition data
c   Ch_bounds        r         r: Channel boundaries
c   Resp_energies    r         r: Response energy bins

      REAL keVtoA
      PARAMETER (keVtoA=12.39854)

      REAL delta, temp
      INTEGER i

c First set the response energies. This assumes that energy bins are contiguous
c so we only need to specify N_energies+1 numbers.

c Do the case of energy definition data read in from the file

      IF ( Resp_reln .EQ. 'FILE' ) THEN

c Note that if these are in wavelengths then convert and ensure that the 
c energies are in increasing order

         IF ( disperse ) THEN
            Resp_energies(0) = keVtoA/Resp_data(2,N_e_energies)
            DO i = 1, N_energies
               Resp_energies(i) = keVtoA/Resp_data(1,N_energies-i+1)
            ENDDO
         ELSE
            Resp_energies(0) = Resp_data(1,1)
            DO i = 1, N_energies
               Resp_energies(i) = Resp_data(2,i)
            ENDDO
         ENDIF

c Now linear energy bins

      ELSEIF ( Resp_reln .EQ. 'LINEAR' ) THEN

c Input parameters in wavelength...

         IF ( disperse ) THEN
            
            Resp_energies(0) = keVtoA/Resp_high
            IF ( Resp_bnumber .EQ. 0 .OR. 
     &           Resp_break .GE. Resp_high ) THEN
               delta = (Resp_high-Resp_low)/Resp_number
               DO i = 1, Resp_number
                  Resp_energies(i) = keVtoA/(Resp_high-i*delta)
               ENDDO
            ELSE
               delta = (Resp_break-Resp_low)/Resp_number
               DO i = 1, Resp_number
                  Resp_energies(i) = keVtoA/(Resp_break-i*delta)
               ENDDO
               delta = (Resp_high-Resp_break)/Resp_bnumber
               DO i = 1, Resp_bnumber
                  Resp_energies(Resp_number+i) = 
     &                    keVtoA/(Resp_high-i*delta)
               ENDDO
            ENDIF

c Input parameters in energy...

         ELSE

            Resp_energies(0) = Resp_low
            IF ( Resp_bnumber .EQ. 0 .OR. 
     &           Resp_break .GE. Resp_high ) THEN
               delta = (Resp_high-Resp_low)/Resp_number
               DO i = 1, Resp_number
                  Resp_energies(i) = Resp_low + i*delta
               ENDDO
            ELSE
               delta = (Resp_break-Resp_low)/Resp_number
               DO i = 1, Resp_number
                  Resp_energies(i) = Resp_low + i*delta
               ENDDO
               delta = (Resp_high-Resp_break)/Resp_bnumber
               DO i = 1, Resp_bnumber
                  Resp_energies(Resp_number+i) = Resp_break + i*delta
               ENDDO
            ENDIF

         ENDIF

c Now logarithmic energy bins

      ELSEIF ( Resp_reln .EQ. 'LOG' ) THEN

c Not implemented yet

      ENDIF

c Now repeat to define the channels. Do the case of channel definition data 
c read in from the file

      IF ( Chan_reln .EQ. 'FILE' ) THEN

         IF ( disperse ) THEN
            DO i = 1, N_channels
               Ch_bounds(i,1) = keVtoA/(Chan_data(2,i))
               Ch_bounds(i,2) = keVtoA/(Chan_data(1,i))
            ENDDO
         ELSE
            DO i = 1, N_channels
               Ch_bounds(i,1) = Chan_data(1,i)
               Ch_bounds(i,2) = Chan_data(2,i)
            ENDDO
         ENDIF

c Now linear channels

      ELSEIF ( Chan_reln .EQ. 'LINEAR' ) THEN

c Input parameters in wavelength...

         IF ( disperse ) THEN
            
            IF ( Chan_bnumber .EQ. 0 .OR. 
     &           Chan_break .GE. Chan_high ) THEN
               delta = (Chan_high-Chan_low)/Chan_number
               DO i = 1, Chan_number
                  Ch_bounds(i,1) = keVtoA/(Chan_low+(i-1)*delta)
                  Ch_bounds(i,2) = keVtoA/(Chan_low+i*delta)
               ENDDO
            ELSE
               delta = (Chan_break-Chan_low)/Chan_number
               DO i = 1, Chan_number
                  Ch_bounds(i,1) = keVtoA/(Chan_low+(i-1)*delta)
                  Ch_bounds(i,2) = keVtoA/(Chan_low+i*delta)
               ENDDO
               delta = (Chan_high-Chan_break)/Chan_bnumber
               DO i = 1, Chan_bnumber
                  Ch_bounds(Chan_number+i,1) = 
     &                    keVtoA/(Chan_break+(i-1)*delta)
                  Ch_bounds(Chan_number+i,2) = 
     &                    keVtoA/(Chan_break+i*delta)
               ENDDO
            ENDIF

c Input parameters in energy...

         ELSE

            IF ( Chan_bnumber .EQ. 0 .OR. 
     &           Chan_break .GE. Chan_high ) THEN
               delta = (Chan_high-Chan_low)/Chan_number
               DO i = 1, Chan_number
                  Ch_bounds(i,1) = Chan_low + (i-1)*delta
                  Ch_bounds(i,2) = Chan_low + i*delta
               ENDDO
            ELSE
               delta = (Chan_break-Chan_low)/Chan_number
               DO i = 1, Chan_number
                  Ch_bounds(i,1) = Chan_low + (i-1)*delta
                  Ch_bounds(i,2) = Chan_low + i*delta
               ENDDO
               delta = (Chan_high-Chan_break)/Chan_bnumber
               DO i = 1, Chan_bnumber
                  Ch_bounds(Chan_number+i,1) = Chan_break + (i-1)*delta
                  Ch_bounds(Chan_number+i,2) = Chan_break + i*delta
               ENDDO
            ENDIF

         ENDIF

c Now logarithmic channels

      ELSEIF ( Chan_reln .EQ. 'LOG' ) THEN

c Not implemented yet

      ENDIF

c Ensure that the channel bounds write ENERG_LO < ENERG_HI

      DO i = 1, Chan_number
         IF ( Ch_bounds(i,1) .GT. Ch_bounds(i,2) ) THEN
            temp = Ch_bounds(i,1)
            Ch_bounds(i,1) = Ch_bounds(i,2)
            Ch_bounds(i,2) = temp
         ENDIF
      ENDDO

      RETURN
      END




