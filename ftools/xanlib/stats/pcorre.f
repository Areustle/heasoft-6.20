C
      real*4 FUNCTION pcorre(r,npts)
C
C     correlation prob. for no correlation between 2 varaibles
C     from bevington page 124
C
      REAL*8 r2, term, sum, fi, fnum, denom
      REAL*8 free, gamma               
c! to use paolo's real*8 gamma
      real*4 r
      integer*4 npts, nfree, imax, neven, i

      nfree = npts - 2
      IF ( nfree.LE.0 ) THEN
        pcorre = 0.
      ELSE
        r2 = r**2
        IF ( 1.-r2.LE.0 ) THEN
          pcorre = 0.
        ELSE
          neven = 2*(nfree/2)
          IF ( nfree.LE.neven ) THEN
            imax = (nfree-2)/2
            free = nfree
            term = abs(r)
            sum = term
            IF ( imax.LT.0 ) THEN
            ELSE IF ( imax.EQ.0 ) THEN
              pcorre = 1. - term
            ELSE
              DO i = 1, imax
                fi = i
                fnum = imax - i + 1
                denom = 2.*i + 1
                term = -term*r2*fnum/fi
                sum = sum + term/denom
              END DO
              pcorre = 1.128379167*(gamma((free+1.)/2.)/gamma(free/2.))
              pcorre = 1. - pcorre*sum
            END IF
          ELSE
            imax = (nfree-3)/2
            term = abs(r)*dsqrt(1.-r2)
            sum = datan(r2/term)
            IF ( imax.LT.0 ) THEN
            ELSE IF ( imax.EQ.0 ) THEN
              sum = sum + term
            ELSE
              sum = sum + term
              DO i = 1, imax
                fnum = 2.*i
                denom = 2.*i + 1
                term = term*(1.-r2)*fnum/denom
                sum = sum + term
              END DO
            END IF
            pcorre = 1. - 0.6366197724*sum
          END IF
        END IF
      END IF
      RETURN
      END
