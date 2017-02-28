c
      SUBROUTINE xrsta(iend, intsta, rntsta, dntsta, nanal, dtnb, 
     &                 yi, syi, expi)
c
c This routine calculates additional statistical varible and write either
c on the screen or in the log. It is valid for 1 series only 
c
c
c I  iend    i  end of good data iend
c I  intsta  i  statistical variable: integer values
c I  rntsta  r  statistical variable: real varible
c I  dntsta  d  statistical variable: double precision
c I  nanal   i  no of points in the final array 
c I  dtnb    d  integration time 
c I  yi      r  input count array c/s
c I  syi     r  input error count array
c I  expi    r  input exposure array
c
c Input varible
       INTEGER*4 iend, intsta(*), nanal, iv, nodiff
       REAL*4 rntsta(*), yi(nanal), syi(nanal), expi(nanal)
       REAL*8 dntsta(*), dtnb
c Local variable
       INTEGER*4 k
       REAL*4 s(20), expto, photto, souto
       REAL*4 dist, probga, xrchithr, rv
       REAL*8 gammq, dv 
       character(80) context 

c  Initialize to avoid warning
       souto = 0.
c  --
       DATA probga/1.35E-3/
c
c
       IF (iend.NE.2) THEN
          call xwrite(' ',2)
          CALL XWRITE(  
     $         '             Results from Statistical Analysis', 2)
c initialise new stat variable 

          DO k = 1, 20
             s(k) = 0.
          ENDDO
          expto = 0.
          photto = 0.
c
c  Calculate additional statistical variables
c  (see Numerical Recipes Chapter 13)
          DO k = 1, nanal
             IF (yi(k).GT.-1.1E34) THEN
c for mean abs. deviation
                s(1) = s(1) + abs(yi(k)-rntsta(1))
c for skewness
                s(2) = s(2) + (yi(k)-rntsta(1))**3
c for kurtosis
                s(3) = s(3) + (yi(k)-rntsta(1))**4
c accumul. total exposure
                expto = expto + expi(k)*dtnb
c accum. tot. no. of phot.
                photto = photto + (syi(k)*expi(k)*dtnb)**2
c accum. tot. no.of source phot.
                souto = souto + (yi(k)*expi(k)*dtnb)
             ENDIF
          ENDDO
c
          IF(rntsta(3).ne.0.and.intsta(2).ne.0)THEN 
c
c mean abs. deviation
             s(1) = s(1)/float(intsta(2))
c skewness
             s(2) = s(2)/float(intsta(2))/rntsta(3)**1.5
c kurtosis
             s(3) = s(3)/float(intsta(2))/rntsta(3)**2 - 3.
c
          ENDIF
c
c protection in case a valid interval made of all zeros is not properly
c rejected  
          dist = 0.
          IF(souto.ne.0.and.photto.ne.0.and.expto.ne.0) THEN
           DO k = 1, nanal
              IF (yi(k).GT.-1.1E34) THEN
c integral dist. of source ph.
                s(4) = s(4) + yi(k)*expi(k)*dtnb/souto
c integral dist. all ph.
c              s(4)=s(4)+(syi(k)*expi(k)*dtnb)**2/photto
c exp.integ distr of source ph.
                s(5) = s(5) + expi(k)*dtnb/expto
c max dist. for KS
               IF (dist.LT.abs(s(5)-s(4))) dist = abs(s(5)-s(4))
              ENDIF
           ENDDO
c
c  Work out prob. for Chisq and KS
c       (accuracy of gammq tested with tables in Abr.Stegun: low probs
c        tested in Gaussian limit dof=10**6: everything looks OK)
c double precision call
           s(4) = gammq(dble(intsta(2)-1)/2., dble(rntsta(9)/2.))
c
c KS Prob.(numrecip)  a la' Avni
c
           s(5) = gammq(1.d0, dble(4.*photto*dist*dist/2.))
c  !(chisq. with 2 dof)
          ENDIF 

c  Write results out
c
          call xwrite(' ',2)
          WRITE (context, 1101) dtnb
 1101     FORMAT (13X, 'Newbin Integration Time (s)..', G12.5)
          CALL XWRITE (context, 2)
          context=' '
          iv=((dntsta(4)-dntsta(3))*86400.D0)/dtnb
          nodiff=intsta(2)-int(iv)
          If(nodiff.EQ.1)THEN 
             dv=(dntsta(4)-dntsta(3))*86400.D0+dtnb  
          ELSE
             dv=(dntsta(4)-dntsta(3))*86400.D0
          ENDIF 
c          WRITE (context, 1102) (dntsta(4)-dntsta(3))*86400.D0
          WRITE (context, 1102) dv
 1102     FORMAT (13X, 'Interval Duration (s)........', G12.5)
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1103) intsta(2)
 1103     FORMAT (13X, 'No. of Newbins ..............', I8)
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1104) rntsta(1), rntsta(11)
 1104     FORMAT(13X, 'Average (c/s) ...............', G12.5, 
     &                        '  +/-  ',G10.2)
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1105) sqrt(rntsta(3))
 1105     FORMAT(13X, 'Standard Deviation (c/s).....', G12.5)
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1106) rntsta(6)
 1106     FORMAT(13X, 'Minimum (c/s) ...............', G12.5) 
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1107) rntsta(7)
 1107     FORMAT(13X, 'Maximum (c/s)................', G12.5)
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1108) rntsta(3),rntsta(13)
 1108     FORMAT(13X, 'Variance ((c/s)**2)..........', G12.5, 
     &                       ' +/-  ', G10.2) 
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1109) rntsta(4),rntsta(14)
 1109     FORMAT(13X, 'Expected Variance ((c/s)**2).', G12.5, 
     &                       ' +/-  ', G10.2)
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1110) rntsta(5)
 1110     FORMAT(13X, 'Third Moment ((c/s)**3)......', G12.5)
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1111) s(1)
 1111     FORMAT(13X, 'Average Deviation (c/s)......', G12.5)
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1112) s(2), sqrt(6./intsta(2))
 1112     FORMAT(13X, 'Skewness.....................', G12.5,
     &                  '    +/-  ', G10.2) 
          CALL XWRITE (context, 2)
          context=' '
          WRITE (context, 1113) s(3), sqrt(24./intsta(2))
 1113     FORMAT(13X,'Kurtosis.....................', G12.5, 
     &                  '    +/-  ', G10.2)
          CALL XWRITE (context, 2)
          context=' '
c
c Work out significance of rms and upper limit if necessary (see xrtyint.for)
c rms is meaningful only for >0 count rates
c
          IF (rntsta(1).GT.0.) THEN
             WRITE (context,1202) rntsta(9), intsta(2) - 1
 1202        FORMAT(13X, 'Chi-Square...................', G12.5,
     &                                   '    dof', I8) 
             CALL XWRITE (context, 2)
             context=' '
             WRITE (context,1203) s(4)
 1203        FORMAT(13X, 'Chi-Square Prob of constancy.', G12.5,
     &                            ' (0 means < 1.e-38)') 
             CALL XWRITE (context, 2)
             context=' '
             WRITE (context,1204)  s(5)
 1204        FORMAT( 13X, 'Kolm.-Smir. Prob of constancy', G12.5,
     &                  ' (0 means < 1.e-38)')
             CALL XWRITE (context, 2)
             context=' '
c
c Work out prob of corresponinding Chisq. distrib. 
c and compare it with 3sigma one-sided Gaussian probability
c
             IF (gammq(dble(max(intsta(2)-1,1))/2.,
     &            dble(max(intsta(2)-1,1))*rntsta(3)/
     &                        rntsta(4)/2.).LT.dble(probga)) THEN
c
                WRITE (context,1201) rntsta(10), rntsta(20)
 1201           FORMAT (13X, 'RMS fractional variation.....', G12.5,
     &                                 '    +/-  ', G10.2) 
                CALL XWRITE (context, 2)
                context=' '
             ELSE
c
c 3 sigma upper limit based on relevant chisq distr.
                rv = xrchithr(max(intsta(2)-1,1),probga)
                if ( rv.eq.-1000. ) then
                   WRITE(context, 1300) 'Undef'
 1300              FORMAT (13X, 'RMS fractional variation....', a,
     &                                 '(3 sigma)') 
                else
                   rv = sqrt(abs(rntsta(4)*rv
     &                  /float(max(intsta(2)-1,1))-rntsta(3)))/rntsta(1)
                   WRITE (context,1301) rv
 1301              FORMAT (13X, 'RMS fractional variation....<', G12.5,
     &                                 '(3 sigma)') 
                endif
                CALL XWRITE (context, 2)
             ENDIF
          ENDIF
       ENDIF
       context=' '
       CALL XWRITE (context, 2)
       RETURN
       END
