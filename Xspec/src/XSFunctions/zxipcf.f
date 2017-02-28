
      SUBROUTINE zxipcf(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne),param(4),photar(ne),photer(ne),eparam(3)

C---
C XSPEC model subroutine for redshifted "partial covering 
C absorption". Assumes that part of the emitter is covered by 
C the given absorption and the rest of the emitter is unobscured. 
C---
C number of model parameters: 2
C      1      ANH      Hydrogen column density (in units of 10**22
C                      atoms per square centimeter
c      2      xi
C      3      FRACT    Covering fraction (0 implies no absorption,
C                      1 implies the emitter is all absorbed with
C                      the indicated column ANH.
C      4      REDSHIFT
C---
C Arguments :
C    ear      r         i: energy ranges
c    ne       i         i: number of energy ranges
c    param    r         i: model parameter values
c    ifl      i         i: the dataset
c    photar   r         r: the transmission fraction
C---

      REAL fract, fractc, zfac
      INTEGER ie
      CHARACTER(128) pname, pvalue
      character(255) filenm, contxt
      LOGICAL qexist

      INTEGER lenact
      CHARACTER(128) fgmstr, fgmodf
      EXTERNAL lenact, fgmstr, fgmodf

c     nh in units of 1e22 - param(1)      
      eparam(1) = param(1)*1.e22

c     logxi
      eparam(2) = param(2)

c     z=0
      eparam(3)=0.0

C shift energies to the emitter frame

      zfac = 1.0 + param(4)
      DO ie = 0, ne
         ear(ie) = ear(ie) * zfac
      ENDDO

c construct the path to the mtable file required

      pname = 'ZXIPCF_DIR'
      pvalue = fgmstr(pname)

      IF ( lenact(pvalue) .LE. 0 ) THEN
         pvalue = fgmodf()
      ENDIF

      filenm = pvalue(:lenact(pvalue))//'zxipcf_mtable.fits'

c check whether the file exists

      INQUIRE(file=filenm, exist=qexist)
      IF (.NOT.qexist) THEN
         contxt = '  zxipcf model ignored : cannot find '
     &            //filenm(:lenact(filenm))
         CALL xwrite(contxt, 5)
         contxt = '  use xset ZXIPCF_DIR directory to point to the file'
         CALL xwrite(contxt, 5)
         DO ie = 1, ne
            photar(ie) = 1.0
         ENDDO
         RETURN
      ENDIF

c interpolate on the mtable

      call xsmtbl(ear, ne, eparam, filenm, ifl, photar, photer)

C now modify for the partial covering

      fract = param(3)
      fractc = 1. - fract

      DO ie = 1, ne
         photar(ie) = fractc + fract * photar(ie)
      ENDDO

C shift energies back to the observed frame

      zfac = 1.0 + param(4)
      DO ie = 0, ne
         ear(ie) = ear(ie) / zfac
      ENDDO

      RETURN
      END
