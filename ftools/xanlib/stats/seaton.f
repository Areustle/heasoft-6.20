 
*+ seaton - uv interstellar extinction in magnitudes
      REAL FUNCTION seaton(lambda,colour_excess)
* description :
*  see seaton,m.j.,mnras,187,73p,1979
* authors :
*  andy pollock (bhvad::amtp)
* history :
*  23 august 1988 : original for exosat adapted from b'ham original (26/1/86)
 
* import :
      REAL lambda                     ! wavelength in angstroms
      REAL colour_excess              ! e(b-v)
* local variables :
      REAL x                          ! 1/microns
      REAL s                          ! uv extinction for unit reddening
*-
      x = 10000./lambda
      IF ( (x.GE.2.70) .AND. (x.LE.3.65) ) THEN
        s = 1.56 + 1.048*x + 1.01/((x-4.60)*(x-4.60)+0.280)
      ELSE IF ( (x.GT.3.65) .AND. (x.LE.7.14) ) THEN
        s = 2.29 + 0.848*x + 1.01/((x-4.60)*(x-4.60)+0.280)
      ELSE IF ( (x.GT.7.14) .AND. (x.LE.10.) ) THEN
        s = 16.17 - 3.20*x + 0.2975*x*x
      ELSE
        s = 0.
      END IF
 
      seaton = s*colour_excess
 
      END
