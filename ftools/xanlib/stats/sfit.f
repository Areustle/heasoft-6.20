      SUBROUTINE sfit(x,y,ndata,sig,mwt,a,b,siga,sigb,chi2,q)
c
c     modelled after numerical recipes
c
      integer*4 ndata, mwt, i
      real*4 a, b, siga, sigb, chi2, q, ss, sx, sy, wt, sxoss
      real*4 x(*), y(*), sig(*), st2, sigdat, t, gammq
      sx = 0.
      sy = 0.
      st2 = 0.
      b = 0.
      IF ( mwt.NE.0 ) THEN
        ss = 0.
        DO i = 1, ndata
          wt = 1./(sig(i)**2)
          ss = ss + wt
          sx = sx + x(i)*wt
          sy = sy + y(i)*wt
        END DO
      ELSE
        DO i = 1, ndata
          sx = sx + x(i)
          sy = sy + y(i)
        END DO
        ss = float(ndata)
      END IF
      sxoss = sx/ss
      IF ( mwt.NE.0 ) THEN
        DO i = 1, ndata
          t = (x(i)-sxoss)/sig(i)
          st2 = st2 + t*t
          b = b + t*y(i)/sig(i)
        END DO
      ELSE
        DO i = 1, ndata
          t = x(i) - sxoss
          st2 = st2 + t*t
          b = b + t*y(i)
        END DO
      END IF
      b = b/st2
      a = (sy-sx*b)/ss
      siga = sqrt((1.+sx*sx/(ss*st2))/ss)
      sigb = sqrt(1./st2)
      chi2 = 0.
      IF ( mwt.EQ.0 ) THEN
        DO i = 1, ndata
          chi2 = chi2 + (y(i)-a-b*x(i))**2
        END DO
        q = 1.
        sigdat = sqrt(chi2/(ndata-2))
        siga = siga*sigdat
        sigb = sigb*sigdat
      ELSE
        DO i = 1, ndata
          chi2 = chi2 + ((y(i)-a-b*x(i))/sig(i))**2
        END DO
        q = gammq(0.5*(ndata-2),0.5*chi2)
      END IF
      RETURN
      END
