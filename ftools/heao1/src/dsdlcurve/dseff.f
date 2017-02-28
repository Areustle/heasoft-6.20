**==dseff.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998
c
c This routine calculates effective exposure for dsdisk data

      SUBROUTINE DSEFF(Idwt,Ra50,Ixplo,Ixphi,Aexp,A2exp,Sumeff,Sumexp,
     &                 Eff1)

c IDWt        I  i       Integer flag for daily efficiency weighting 
c                        (0=no ; 1=yes)
c RA50        I  r*4     Ra of source position in 1950
c Ixplo       I  i       Ixplo and Ixphi are the low and high bounderies 
c Ixphi       I  i       to determinate the exposure 
c Aexp        I  r*4     No. of exposures weighted by DWT (where DWT
c                        = average efficiency for that day)
c                        If IDWT = 0 ; DWT = 1
c                        If IDWT = 1 ; DWT = aveff (see routine dsdwt.f)
c                        Aexp is for the entire scan of 25 degrees centered
c                        on the source. (A scan completes 25 degrees in 100 
c                        bins)
c A2exp       I  r*4     No. of exposures weighted by DWT**2
c                        A2exp is only for scan angles +/- 3 bins 
c                        (+/- 3*0.25 degrees) from centre of FWHM of
c                        scan. 
c Sumexp      I  r*4     Summed exposure for IDWt = 0
c Sumeff      I  r*4     Sum of (No of exposures * the average efficiency)
c                        for IDWt = 0
c Eff1        O  r*4     effective exposure 
c atmp           r*4     Weighted exposure around the peak of the scan
c                        (+/- 3 bins) relative to that of the overall scan.
c
c
      INTEGER Idwt, Ixphi, Ixplo
      REAL A2exp, Aexp, Eff1, Sumeff, Sumexp 
      REAL*8 Ra50
c
      INTEGER i, i1
      REAL atmp, ecnt
      character(80) context
      DIMENSION atmp(6) , Aexp(1) , A2exp(1)
c
      Eff1 = 1.
      IF ( Ra50.GE.0. ) THEN
         IF ( Idwt.NE.0 ) THEN
            Eff1 = 0.
            ecnt = 0.
            i1 = 0
            DO i = Ixplo , Ixphi
               i1 = i1 + 1
               atmp(i1) = 0.
               IF ( Aexp(i).NE.0. ) THEN
                   ecnt = ecnt + 1.
                  atmp(i1) = A2exp(i)/Aexp(i)
                  Eff1 = Eff1 + atmp(i1)
               ENDIF
            ENDDO
 
            WRITE (context,
     &'(" Ratio wtd peak exp. (+/- 3 bins) ")')
           CALL XWRITE(context,30)
           WRITE (context,
     &'(" to wtd total scan exp. ",6F7.3)') 
     & atmp
            CALL XWRITE(context,30)
            IF ( ecnt.GT.0.5 ) Eff1 = Eff1/ecnt
         ELSE
            IF ( Sumexp.NE.0. ) Eff1 = Sumeff/Sumexp
         ENDIF
      ENDIF
      context=' '
      CALL XWRITE(context,5)
      WRITE (context,'(" Effective Exposure:",F7.3)') Eff1
      CALL XWRITE(context,5)
      RETURN
      END
