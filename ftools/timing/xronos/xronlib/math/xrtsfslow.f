c
      SUBROUTINE xrtsfslow(nbint, yi, syi, yp, syp)
c
c  ls 20/7/89 to calculate time skewness function with slow algorithm
c
c  I   nbint = no. of newbins/intv.
c  I   nmax = dimansion of input arrays
c  I   yi = newbin values (with avg. =0 ) ([nmax] elements)
c  I   syi = newbin errors                ([nmax] elements)
c  O   yp = time skewness values (unormalised) ([nmax]elements)
c  O   syp =  "      "    propagated  errors (unormalised)
c                                                  ([nmax] elements)
c
c Note that normalization by 1/N(good) is carried out by calling program
c
      INTEGER*4 nbint, k, i
      REAL*4 rv, yi(nbint), syi(nbint), yp(nbint), syp(nbint)
c
c Calculate time-skewness
c
c
c time delay index
      DO k = 0, nbint - 1
c
c set results =0
         yp(k+1) = 0.
c set errors =0
         syp(k+1) = 0.
c
c       Calculate values
c
         rv = 0.
c newbin index
         DO i = 1, nbint - k
            rv = rv + yi(i)*yi(i+k)*(yi(i)-yi(i+k))
         ENDDO
c /float(nbint) normalised in main program
         yp(k+1) = rv
c
c       Calculate propagated errors
c
         rv = 0.
c newbin index
         DO i = 1, nbint - k
            rv = rv + ((2.*yi(i+k)-yi(i))*yi(i)*syi(i+k))
     &           **2 + ((2.*yi(i)-yi(i+k))*yi(i+k)*syi(i))**2
         ENDDO
         IF (k+1.LE.nbint-k) THEN
c newbin index
            DO i = k + 1, nbint - k
               rv = rv - 2.*(2.*yi(i)-yi(i+k))*yi(i+k)
     &              *(2.*yi(i)-yi(i-k))*yi(i-k)*syi(i)**2
            ENDDO
         ENDIF
         syp(k+1) = sqrt(abs(rv))
c
      ENDDO
c
      RETURN
      END
c
c
c
