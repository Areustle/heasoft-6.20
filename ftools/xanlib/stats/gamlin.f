      real*4 FUNCTION gamlin(xx)
c* numerical recipes p157 - their comments too
c*  returns the value ln(gamma(xx)) for xx > 0. full accuracy is obtained
c*  directly for xx > 1. else a reflection formula is used
c renamed from gammln to gamlin 15 oct 92 to avoid name clash
c with similar routine in xanadu:[lib.press]
      REAL*8 cof(6), stp, half, one, fpf, x, tmp, ser, pi
      real*4 xx, xxx
      integer*4 j
      LOGICAL smallx
      DATA cof/76.18009173D0, -86.50532033D0, 24.01409822D0,
     &     -1.231739516D0, .120858003D-2, -.536382D-5/
      DATA stp/2.50662827465D0/
      DATA half, one, fpf/0.5D0, 1.0D0, 5.5D0/
      smallx = (xx.LT.one)
      IF ( smallx ) THEN
        xxx = 2. - xx
      ELSE
        xxx = xx
      END IF
      x = xxx - one
      tmp = x + fpf
      tmp = (x+half)*log(tmp) - tmp
      ser = one
      DO j = 1, 6
        x = x + one
        ser = ser + cof(j)/x
      END DO
      gamlin = tmp + log(stp*ser)
      IF ( smallx ) THEN
        pi = 4.0D0*atan(one)
        gamlin = log(pi*(1.-xx)/sin(pi*(1.-xx))) - gamlin
      END IF
      RETURN
      END
