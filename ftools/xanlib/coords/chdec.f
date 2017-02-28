      SUBROUTINE CHDEC(Dec,Idd,Idm,Dsec,Iflag)
c
c change decimal degrees to deg mn sec (iflag = 1)
c or change decimal degrees to deg mn sec (iflag .ne. 1)
c 
c
      character(1) sign
      REAL*8 Dec , rdm
      REAL*4 Dsec
      INTEGER*4 Iflag , Idd , iddd , Idm
c
      IF ( Iflag.EQ.1 ) THEN
         if(dec.lt.0.0)then
          sign = '-'
         else
          sign = '+'
         endif
c
         Idd = Dec
         iddd = ABS(Idd)
         rdm = ABS(Dec) - iddd
         rdm = rdm*60
         Idm = rdm
         Dsec = rdm - Idm
         Dsec = Dsec*60.
         IF ( Dsec.EQ.60.0 ) THEN
            Dsec = 0.0
            Idm = Idm + 1
         ENDIF
         IF ( Idm.EQ.60.0 ) THEN
            Idm = 0.0
            IF ( Idd.GT.0 ) THEN
               Idd = Idd + 1
            ELSE
               Idd = Idd - 1
            ENDIF
         ENDIF
c
         if(sign.eq.'-')then
          if(idd.gt.0)then
            idd = -idd
          elseif(idm.gt.0.and.idd.eq.0)then
            idm = -idm
          elseif(idm.eq.0.and.idd.eq.0)then
            dsec = -dsec
          endif
         endif
c
      ELSEIF ( Dsec.LT.0. ) THEN
         Dec = DBLE(Dsec)/3600.D0
      ELSEIF ( Idm.LT.0 ) THEN
         Dec = DFLOAT(Idm)/60.D0 - DBLE(Dsec)/3600.D0
      ELSEIF ( Idd.LT.0 ) THEN
         Dec = DFLOAT(Idd) - DFLOAT(Idm)/60.D0 - DBLE(Dsec)/3600.D0
      ELSE
         Dec = DFLOAT(Idd) + DFLOAT(Idm)/60.D0 + DBLE(Dsec)/3600.D0
      ENDIF
      RETURN
      END
