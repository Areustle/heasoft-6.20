      SUBROUTINE spear(data1,data2,n,wksp1,wksp2,d,zd,probd,rs,probrs)
      real*4 data1(*), data2(*), wksp1(*), wksp2(*)
      integer*4 j, n
      real*4 d, zd, probd, rs, probrs, sf, sg, en, en3n, aved, vard
      real*4 t, fac, betai, df, erfcc
c

      DO j = 1, n
        wksp1(j) = data1(j)
        wksp2(j) = data2(j)
      END DO
      CALL sort2(n,wksp1,wksp2)
      CALL crank(n,wksp1,sf)
      CALL sort2(n,wksp2,wksp1)
      CALL crank(n,wksp2,sg)
      d = 0.
      DO j = 1, n
        d = d + (wksp1(j)-wksp2(j))**2
      END DO
      en = n
      en3n = en**3 - en
      aved = en3n/6. - (sf+sg)/12.
      fac = (1.-sf/en3n)*(1.-sg/en3n)
      vard = ((en-1.)*en**2*(en+1.)**2/36.)*fac
      zd = (d-aved)/sqrt(vard)
      probd = erfcc(abs(zd)/1.4142136)
      rs = (1.-(6./en3n)*(d+0.5*(sf+sg)))/fac
      t = rs*sqrt((en-2.)/((1.+rs)*(1.-rs)))
      df = en - 2.
      probrs = betai(0.5*df,0.5,df/(df+t**2))
      RETURN
      END
