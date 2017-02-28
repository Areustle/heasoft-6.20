       SUBROUTINE xrpsdf(ipf, iend, intsta, rntsta, dntsta, iflags, 
     &                 rflags, nanal, nintfm, nbint, rwn, dxsta,
     &                 yi, syi, expi, yp, syp,
     &                 yr, syr, expr,irtsta, rrtsta,drtsta, iframe)
      implicit none
c
c This routine does the FFT and then averages if necessary into a frame
c If iframe=0 return to get another interval.
c if iframe=1 the frame is complpeted and the plot and output can be done.,c
c
c
c I  ipf     i  integer parameter original xronos prameters file.
c I  iend    i  end of good data iend  
c I  intsta  i  statistical variable: integer values 
c I  rntsta  r  statistical variable: real varible 
c I  dntsta  d  statistical variable: double precision 
c I  iflags  i  integer flags
c I  rflags  r  real flags
c I  nanal   i  no of points in the final array (e.g. no of freq for fft)
c I  nintfm  i  No of interval to avarage in a frame
c I  nbint   i  No of points in a interval
c I  rwn     r  exp white noise in avg (check if need to be saved)
c I  dxtsta  d  lower frequency or frequency spacing 
c I  yi      r  input count array c/s
c I  syi     r  input error count array 
c I  expi    r  input exposure array
c O  yr      r  output psd array 
c O  syr     r  'error' on psd (Note if interval is 1 syr = yr)
c O  expr    r  number of fft in average per points 
c O  irtsta  i  statistical results integer array 
c O  rrtsta  r  statistical results real array
c O  drtsta  d  statistical results double precision 
c O  iframe  i  if frame is complete =1  
c 
c
c Input varible
       INTEGER*4 ipf(*), iend, intsta(20), iflags(20)
       INTEGER*4 nbint, nanal, nintfm
       REAL*4    rntsta(20), rflags(10),  rwn
       REAL*8    dntsta(20), dxsta
       REAL*4 yi(nanal), syi(nanal),
     $      expi(nanal)
! temporary
       LOGICAL fast
c Output varible
       INTEGER*4 irtsta(20), iframe
       REAL*4 yr(nanal), syr(nanal), expr(nanal)
       REAL*4 rrtsta(20)     
       REAL*8 drtsta(20)     
c local variable
       INTEGER*4 k, k1, k2
       REAL*4 xnorm, rnoise, rv
       REAL*4 yp(nanal), syp(nanal)
       character(80) context
c
       fast=.false.
       fast=(iflags(15).eq.1)
c
c subtract average from intv. and fill gaps with 0 (=avg)
c not last intv, or good last intv
c
         IF (iend.NE.2) THEN
c
c standard; i.e. not expos profile
            IF (ipf(3).EQ.0) THEN
               DO k = 1, nbint
                  IF (yi(k).GT.-1.1E34) THEN
c
c subtract avg.
                     yi(k) = yi(k) - rntsta(1)
                  ELSE
c
c gaps set to 0
                     yi(k) = 0.
                     syi(k) = 0.
                     expi(k) = 0.
                  ENDIF
               ENDDO
            ENDIF
c
c call fft routine and calculate power spectrum
c normalization by N(good) only
            xnorm = float(intsta(2))
c
c  standard normal. (expected variance)
c  noise=2
            IF (ipf(4).EQ.1 .OR. ipf(4).EQ.-1) 
     &         xnorm = xnorm*rntsta(4)/2.
c
c   normalis. for rms fractional variation from SUM (not integral)
c   of power after noise subtraction
c /(dxsta*float(nanal))
            IF (ipf(4).EQ.2 .OR. ipf(4).EQ.-2)
     &         xnorm = xnorm*float(nanal)*rntsta(1)**2*dxsta
c
c   define white noise level to be subtracted if any
c   in the case of norm =2 and rms    
            rnoise = 0.
c 
c /(dxsta*float(nanal)))
            IF (ipf(4).EQ.-2) 
     &         rnoise = rntsta(4)/(float(nanal)*rntsta(1)**2*dxsta)
            IF (ipf(4).EQ.-1) rnoise = 2.
c
c for avg white noise. Note this is accounted after average
            rwn = rwn + rntsta(4)/(float(nanal)*rntsta(1)**2*dxsta)
c
            IF (fast) then   
c call fft routine
               CALL xrfft8(yi, nbint)
c
c from xrfft8 the yi(1) is the dc level and yi(2) is the high frequency
c term. the yi(odd index >1) are the imaginary, the yi(even index > 2)
c are the imaginary.
c Save the Nyq frequency for norm.
               rv = yi(2)
c
c prepare power spectrum
c other freqs.
c 16 dec 2003 Note the array is nanal-1 in the fast because 
c the nyq is calculated out of the do loop
c 
               DO k = 1, nanal-1
                  k1 = k*2 + 1
                  k2 = k*2 + 2
c
c power
c sigma=power
                  yp(k) = (yi(k1)*yi(k1)+yi(k2)*yi(k2))/xnorm - rnoise
c
c sigma=power
                  syp(k) = sqrt((yi(k1)*yi(k1)+yi(k2)*yi(k2))/xnorm)
               ENDDO
c
c for Nyq freq
               yp(nanal) = rv*rv/xnorm - rnoise
c for Nyq freq
c Rev.1 unnormal. for Nyq freq
               syp(nanal) = sqrt(rv*rv/xnorm/2.)
            ELSE
c
c call slow ft routine
               CALL xrslwft(yi, nbint, yp, syp)
c
c               prepare power spectrum
c note that Nyq. freq., if any, is in the loop
c 16 dec 2003 the do loop was changed from nanal-1 to nanal 
c in the slow the last frequency is not set. 
c in xronos 4 was nanal but during the transition to 5 got 
c changed by mistake to nanal-1 
c  
               DO k = 1, nanal
c power
c sigma=power
                  yp(k) = (yp(k)*yp(k)+syp(k)*syp(k))/xnorm - rnoise
c
c sigma=power
                  syp(k) = sqrt(yp(k)+rnoise)
               ENDDO
            ENDIF
            CALL XWRITE ('Power spectrum ready !', 10)
         ENDIF
c
c average results in frame
         CALL xrgetfm(nanal, nintfm, ipf, iend, iflags, rflags, 
     &                intsta, rntsta, dntsta, yp, syp, irtsta, rrtsta,
     &                drtsta, yr, syr, expr, iframe)
c
c If iframe =1  finish off the norma and go out for plot
c otherwise go out and get another interval
c
         IF (iframe.EQ.1) THEN
c 
c         write expected noise level for non-standard normalizations
c
            rflags(4) = 2.
            rnoise = 0.
            IF (ipf(4).NE.1) THEN
               IF (ipf(4).EQ.2 .OR. ipf(4).EQ.-2) THEN
c
c for rms**2 normal.
                  rflags(4) = rwn/float(irtsta(4))
c
c reset
                  rwn = 0.
               ENDIF
               IF (ipf(4).EQ.0) rflags(4) = rrtsta(4)
c
c if wn subtr.
               IF (ipf(4).EQ.-1 .OR. ipf(4).EQ.-2) rnoise = rflags(4)
               WRITE(context, 1101) rflags(4)
 1101          FORMAT (13X, 'Expected white noise level is ', G13.4)
               CALL XWRITE(context,10)
            ENDIF
c
c renormalise errors (if propagated)
c
            IF (nintfm.LT.ipf(2)) THEN
c all freqs.
               DO k = 1, nanal
                  syr(k) = syr(k)*sqrt(yr(k)+abs(rnoise))
               ENDDO
            ENDIF
         ENDIF 
      RETURN 
      END
