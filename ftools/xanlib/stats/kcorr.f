c
c
c
      SUBROUTINE kcorr(z,model,gamma1,gamma2,e1,e2,breakp,corr)
c
c       calculates k-correction for different spectral shapes
c       inputs :
c       z         -> redshift at which k-correction is to be calculated
c       model = 1 -> simple power law with (energy) slope gamma1
c       model = 2 -> double power law with slopes gamma1 and gamma2
c                    before and after the break point breakp
c       e1,e2     -> estrema of energy bandpass of instrument used
c       output :
c       corr      -> k-correction at redshift z
c       k-correction is defined as the ratio l(e1(1+z)-e2(1+z))/l(e1-e2)
c
      integer*4 model
      real*8 corr, z, gamma1, emin, emax, alp1, alp2, gamma2
      real*8 breakp, e1, e2, den, anum
c
      IF ( model.EQ.1 ) THEN
        corr = (1.D0+z)**(1.D0-gamma1)
        RETURN
      ELSE IF ( model.EQ.2 ) THEN
        emin = e1*(1.D0+z)
        emax = e2*(1.D0+z)
        alp1 = 1.D0 - gamma1
        alp2 = 1.D0 - gamma2
        den = (breakp**alp1-e1**alp1)/alp1 + breakp**(gamma2-gamma1)
     &        *(e2**alp2-breakp**alp2)/alp2
        IF ( breakp.LE.emin ) THEN
          IF ( breakp.LE.e1 ) THEN
            corr = (1.D0+z)**(1.D0-gamma2)
            RETURN
          ELSE IF ( breakp.GT.e2 ) THEN
            corr = (emax**alp2-emin**alp2)/(e2**alp1-e1**alp1)*alp1/alp2
          ELSE
            corr = (emax**alp2-emin**alp2)/alp2/den
            RETURN
          END IF
        ELSE IF ( breakp.GE.e2 ) THEN
          IF ( breakp.GE.emax ) THEN
            corr = (1.D0+z)**(1.D0-gamma1)
            RETURN
          ELSE IF ( breakp.GE.emin ) THEN
            anum = (breakp**alp1-emin**alp1)
     &             /alp1 + breakp**(gamma2-gamma1)
     &             *(emax**alp2-breakp**alp2)/alp2
            corr = anum/(e2**alp1-e1**alp1)*alp1
            RETURN
          ELSE
            corr = (emax**alp2-emin**alp2)/(e2**alp1-e1**alp1)*alp1/alp2
            RETURN
          END IF
        ELSE
          anum = (breakp**alp1-emin**alp1)
     &           /alp1 + breakp**(gamma2-gamma1)
     &           *(emax**alp2-breakp**alp2)/alp2
          corr = anum/den
          RETURN
        END IF
        WRITE (*,*) ' spectral model not supported - sub kcorr '
        STOP
      END IF
      END
