
      SUBROUTINE getsig(energy, iline, disperse, resol_reln, fwhm, 
     &                  n_r_energies, n_lines, lineEn, lineDt, 
     &                  centroid, sigma, norm)

      IMPLICIT NONE

      INTEGER n_r_energies, n_lines
      REAL lineEn(n_r_energies), lineDt(3,n_lines,n_r_energies)
      REAL energy, fwhm, centroid, sigma, norm
      INTEGER iline
      CHARACTER*(*) resol_reln
      LOGICAL disperse

c Get the centroid energy, sigma, norm for this energy. On error return a 
c negative sigma.
c The options are
c    1       constant in wavelength
c    2       constant in energy
c    3       table in wavelegth
c    4       table in energy
c    5       CZT in energy
c    6       Square root in energy
c    7       linear in wavelength
c    8       linear in energy

      REAL fw2sig, keVtoA
      PARAMETER (fw2sig=0.424661, keVtoA=12.39854)


      REAL savsig, factor, wave, f1, f2, den

      INTEGER option, i, j

      CHARACTER(72) contxt

      LOGICAL first

      SAVE savsig, factor, option, first

      INTEGER lenact
      EXTERNAL lenact

      DATA first, factor, savsig, option /.TRUE., 1.0, 1.0, 0/

c If the first time through find out what algorithm is required

      IF ( first ) THEN

         first = .FALSE.

c If constant then calculate the sigma (in keV)

         IF ( resol_reln .EQ. 'CONSTANT' ) THEN

            IF ( disperse ) THEN
               option = 1
               savsig = fwhm * fw2sig / keVtoA
            ELSE 
               option = 2
               savsig = fwhm * fw2sig
            ENDIF

c Else if we are using the tabulated resolution then convert from
c fwhm to sigma (in keV)

         ELSEIF ( resol_reln .EQ. 'FILE' ) THEN

            IF ( disperse ) THEN
               DO i = 1, n_r_energies
                  DO j = 1, n_lines
                     lineDt(2,j,i) = lineDt(2,j,i) * fw2sig / keVtoA
                  ENDDO
               ENDDO
               option = 3
            ELSE
               DO i = 1, n_r_energies
                  DO j = 1, n_lines
                     lineDt(2,j,i) = lineDt(2,j,i) * fw2sig
                  ENDDO
               ENDDO
               option = 4
            ENDIF

c If the special case of CZT then set the factor

         ELSEIF ( resol_reln .EQ. 'CZT' ) THEN

            option = 5
            factor = 10. / fw2sig

c If sqroot then calculate the fiducial sigma (in keV)

         ELSEIF ( resol_reln .EQ. 'SQROOT' ) THEN

            option = 6
            savsig = fwhm * fw2sig

c If linear then calculate the fiducial sigma (in keV)

         ELSEIF ( resol_reln .EQ. 'LINEAR' ) THEN

            IF ( disperse ) THEN
               option = 7
               savsig = fwhm * fw2sig / keVtoA
            ELSE 
               option = 8
               savsig = fwhm * fw2sig
            ENDIF

         ELSE

            option = 0
            WRITE(contxt,'(a,a)') 
     &         'Error : unknown resolution relation - ',
     &         resol_reln(:lenact(resol_reln))
            CALL xwrite(contxt,10)
            sigma = -1.0

         ENDIF

      ENDIF

c centroid and norm will be input and energy in 1 unless we are using option
c 3 or 4

      centroid = energy
      norm = 1.0

c Now calculate the sigma: first for the constant case - first constant
c in wavelength then constant in energy

      IF ( option .EQ. 1 ) THEN

         sigma = energy * energy * savsig

      ELSEIF ( option .EQ. 2 ) THEN

         sigma = savsig

c then the tabulated cases - do a simple linear interpolation
c this is slow but can speed it up later

      ELSEIF ( option .EQ. 3 ) THEN

         wave = keVtoA / energy 
         IF ( wave .LE. lineEn(1) ) THEN
            centroid = wave - lineDt(1,iline,1)
            sigma = lineDt(2,iline,1)
            norm = lineDt(3,iline,1)
         ELSEIF ( wave .GE. lineEn(n_r_energies) ) THEN
            centroid = wave - lineDt(1,iline,n_r_energies)
            sigma = lineDt(2,iline,n_r_energies)
            norm = lineDt(3,iline,n_r_energies)
         ELSE
            i = 1
            DO WHILE ( wave .GT. lineEn(i) )
               i = i + 1
            ENDDO
            f1 = lineEn(i)-wave 
            f2 = wave-lineEn(i-1)
            den = lineEn(i)-lineEn(i-1)
            centroid = wave
     &         - (lineDt(1,iline,i-1)*f1 + lineDt(1,iline,i)*f2)/den
            sigma = (lineDt(2,iline,i-1)*f1 + lineDt(2,iline,i)*f2)/den
            norm = (lineDt(3,iline,i-1)*f1 + lineDt(3,iline,i)*f2)/den
         ENDIF
         sigma = sigma * energy * energy

      ELSEIF ( option .EQ. 4 ) THEN

         IF ( energy .LE. lineEn(1) ) THEN
            centroid = energy - lineDt(1,iline,1)
            sigma = lineDt(2,iline,1)
            norm = lineDt(3,iline,1)
         ELSEIF ( energy .GE. lineEn(n_r_energies) ) THEN
            centroid = energy - lineDt(1,iline,n_r_energies)
            sigma = lineDt(2,iline,n_r_energies)
            norm = lineDt(3,iline,n_r_energies)
         ELSE
            i = 1
            DO WHILE ( energy .GT. lineEn(i) )
               i = i + 1
            ENDDO
            f1 = lineEn(i) - energy
            f2 = energy - lineEn(i-1)
            den = lineEn(i)-lineEn(i-1)
            centroid = energy 
     &         - (lineDt(1,iline,i-1)*f1 + lineDt(1,iline,i)*f2)/den
            sigma = (lineDt(2,iline,i-1)*f1 + lineDt(2,iline,i)*f2)/den
            norm = (lineDt(3,iline,i-1)*f1 + lineDt(3,iline,i)*f2)/den
         ENDIF

c or the CZT special case

      ELSEIF ( option .EQ. 5 ) THEN

         sigma = energy/(factor*sqrt(20/energy))

c or the square root case (proportional counters)

      ELSEIF ( option .EQ. 6 ) THEN

         sigma = savsig * SQRT(energy)

c or the linear case - first linear in wavelength then linear in energy
c note that in the linear in wavelength case the fiducial sigma is at 1 Angstrom

      ELSEIF ( option .EQ. 7 ) THEN

         sigma = ( energy / (1.0/keVtoA) ) * savsig

      ELSEIF ( option .EQ. 8 ) THEN

         sigma = energy * savsig

      ENDIF

      RETURN
      END


