c
      SUBROUTINE xryrebin(rebin, nanal, k, iflags, nmaxa, yr, syr, expr)
c
c LS 3/11/88 to rebin y data linearly and logarithmically
c
c Note: error bars are propagated in the rebinning. One call will rebin
c       all the y arrays (yr, syr, expr)
c
c   I   rebin = 0 no rebinning,>1 step for const rebinning
c            <-1 step for geom. series rebinning, -1000 no rebinning (octave)
c   I   nanal = no. of bin in arrays before rebinning
c   I/O k = index of first result bin to be rebinned/last rebinned orig value
c   I   iflags = for plot, file type and analysis
c               (iflags(8)= no. of result variable +expr)
c   I   nmaxa = iflags (15) for dimension
c   I/O yr,syr,expr = array containing values,errors and expos.
c                     before/after rebinning
c
c  Note: expr is always 1 column and refers to the last result
c        max no. of y and sy columns is 8
c
      INTEGER*4 nanal, j, ireb, i, k, iflags(20), ireg(8), m, nmaxa
c       real*4 yr(iflags(15),(iflags(8)-1)/2),
c     &        syr(iflags(15),(iflags(8)-1)/2),expr(iflags(15)),
      REAL*4 yr(nmaxa, *), syr(nmaxa, *), expr(nmaxa), y(8), sy(8), ex,
     &       rebin, rbe
c
c  initialise values
c
      rbe = 1.
      j = 0
      IF (abs(rebin).LE.1. .OR. rebin.LT.-999.) RETURN
c
      DO WHILE (k.LE.nanal)
c determine ireb= current rebinning factor
         IF (rebin.GT.1.) THEN
c constant rebinning
            ireb = int(rebin)
         ELSE
c geometr. series rebinning
            rbe = rbe*abs(rebin)
            ireb = int(rbe)
         ENDIF
c
c        set initial values for current point
         DO m = 1, (iflags(8)-1)/2
            y(m) = -1.2E34
            sy(m) = -1.2E34
            ireg(m) = 0
         ENDDO
         ex = -1.2E34
c
c        actual rebinning
c
         DO i = k, k + ireb - 1
            IF (i.LE.nanal) THEN
               DO m = 1, (iflags(8)-1)/2
                  IF (yr(i,m).GT.-1.1E34) THEN
                     IF (y(m).LT.-1.1E34) THEN
                        y(m) = 0.
                        sy(m) = 0.
                        ex = 0.
                     ENDIF
                     y(m) = y(m) + yr(i, m)
                     sy(m) = sy(m) + syr(i, m)*syr(i, m)
                     ex = ex + expr(i)
                     ireg(m) = ireg(m) + 1
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
c
c        renormalise values
c
         j = j + 1
c         write(*,*)' j,ireb,k,k+ireb-1,ireg ',j,ireb,k,k+ireb-1,ireg(1)
         DO m = 1, (iflags(8)-1)/2
            yr(j, m) = y(m)/float(ireg(m))
            syr(j, m) = sqrt(sy(m))/float(ireg(m))
         ENDDO
         expr(j) = ex
         if ( k.eq.nanal ) then
            k = k + 1
         else
            k = min(k+ireb, nanal)
         endif
c         write(*,*)' j,y,ex ',j,yr(j,1),expr(j)
      ENDDO
c
      RETURN
      END
c
c
