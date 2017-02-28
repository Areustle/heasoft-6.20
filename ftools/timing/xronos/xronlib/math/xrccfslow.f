c
      SUBROUTINE xrccfslow(nbint, yi, syi, yp, syp)
c
c  ls 18/7/89 to calculate cross-correlation function with slow algorithm
c
c  I   nbint = no. of newbins/intv.
c  I   nmax = dimansion of input arrays
c  I   yi = newbin values (with avg. =0 ) ([nmax,2] elements)
c  I   syi = newbin errors                ([nmax,2] elements)
c  O   yp = cross-correlation values (unormalised) ([nmax*2] elements)
c  O   syp = cross-correlation propagated errors (unormalised)
c                                                  ([nmax*2] elements)
c
c  Note that normalization 1/N(good) is done by the calling program
c
c
      INTEGER*4 nbint, nmax, k, i, imin, imax
      REAL*4 rv, yi(nbint*2, 2), syi(nbint*2, 2)
      real yp(nbint*2), syp(nbint*2)
c
c Calculate cross-correlation
c
c
c time delay index
      DO k = -nbint + 1, nbint - 1
c
c set results =0
         yp(k+nbint) = 0.
c set errors =0
         syp(k+nbint) = 0.
c
c set summation min and max
         IF (k.GE.0) THEN
            imin = 1
            imax = nbint - k
         ELSE
            imin = 1 - k
            imax = nbint
         ENDIF
c
c       Calculate values
c
         rv = 0.
c newbin index
         DO i = imin, imax
            rv = rv + yi(i, 1)*yi(i+k, 2)
         ENDDO
c /float(nbint) (normal. done in main)
         yp(k+nbint) = rv
c
c       Calculate propagated errors
c
         rv = 0.
c newbin index
         DO i = imin, imax
            rv = rv + (yi(i,1)*syi(i+k,2))**2 + (yi(i+k,2)*syi(i,1))**2
         ENDDO
c /float(nbint) (normal. done in main)
         syp(k+nbint) = sqrt(abs(rv))
c
      ENDDO
c
      RETURN
      END
c
c
c
