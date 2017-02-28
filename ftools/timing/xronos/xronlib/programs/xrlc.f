c     
      SUBROUTINE xrlc (nanal, n,  xnorm, rvn, yi, syi, expi, 
     &     yp, syp, expr)
      implicit none
c     
c     This routine calcultes ratio and sum for the input array yi, syi, expi
c     The results are stored in yr, syr, expr
c     for lc program the output go into plottin and writing 
c     for ef program the output is going to be average 
c     before plotting and writing
c     
c     
c     I   nanal      i no of analysis bin (lc nbint=nanal, ef no of phases)
c     I   n          i no of time series
c     I   xnorm      r norm (set to 1 for lc)
c     I   rvn        r norm = no of series
c     I   yi  (m,n)  r input c/s array
c     I   syi (m,n)  r input err c/s array
c     I   expi(m,n)  r input exposure array
c     O   yp  (m,nn) r output results/partial array input + ratios +sum
c     O   syp (m,nn) r output error results/partial array input + ratios +sum
c     O   expr(m)    r output exposure not really used
c     
c     NOTE :  m = the m dimension in  yi, syi, expi, yr, syr, expr corresponds
c     to the max number of point allow in the task
c     in LC m the number of new bin in interval
c     in EF m is the number of phases per folded light curve.
c     
c     nn = original number of times series, n, + the results
c     if n = 1  number of index out_vector = input_vector => nn=1
c     yp  (m,nn) = yi(m,n)
c     syp (m,nn) = syi(m,n)
c     expr(m)    = expi(m,n)
c     
c     if n = 2  number of index out_vector = input_vector + 2 => nn=4
c     yp(m,1) = series 1     syp(m,1) = err series 1
c     yp(m,2) = series 2     syp(m,2) = err series 2
c     yp(m,3) = ratio 2/1    syp(m,3) = err ratio 2/1
c     yp(m,4) = sum 1+2      syp(m,4) = err sum 1+2
c     
c     if n = 3  number of index out_vector = input_vector + 3 => nn=6
c     yp(m,1) = series 1     syp(m,1) = err series 1
c     yp(m,2) = series 2     syp(m,2) = err series 2
c     yp(m,3) = series 3     syp(m,3) = err series 3
c     yp(m,4) = ratio 2/1    syp(m,4) = err ratio 2/1
c     yp(m,5) = ratio 3/2    syp(m,5) = err ratio 3/2
c     yp(m,6) = sum 1+2+3    syp(m,6) = err sum 1+2+3
c     
c     if n = 4 number of index out_vector = input_vector + 4 => nn=7
c     yp(m,1) = series 1     syp(m,1) = err series 1
c     yp(m,2) = series 2     syp(m,2) = err series 2
c     yp(m,3) = series 3     syp(m,3) = err series 3
c     yp(m,4) = series 4     syp(m,4) = err series 4
c     yp(m,5) = ratio 2/1    syp(m,5) = err ratio 2/1
c     yp(m,6) = ratio 4/3    syp(m,6) = err ratio 4/3
c     yp(m,7) = sum 1+2+3+4  syp(m,7) = err sum 1+2+3+4
c     
c     and
c     NOTE :   the array containing the input exposure is not used in the ratio
c     or the sum because it is assumed that the series are corrected
c     already. The results exposure can be eventually return correctly
c     for the case of 2, 3, 4. The output values now are:
c     expr(m) = 1       good data point
c     expr(m) =-1.2e-34 bad data point
c     Also note that 
c     input variable
      INTEGER*4 nanal, n
      REAL*4 xnorm(4), rvn
      REAL*4 yi(nanal, *), syi(nanal, *), expi(nanal,*)
c     
c     output variable
      REAL*4 yp(nanal, *), syp(nanal, *), expr(nanal)
c     
c     local variable
      INTEGER*4 nn, k, m, iv
      REAl rv
      character(80) context
      LOGICAL good
c     
c     Work out the final resuls array
      IF (n.eq.1) nn=1
      IF (n.eq.2) nn=4
      IF (n.eq.3) nn=6
      IF (n.eq.4) nn=7
c     
c     apply normalization and calculate temporay result variables before
c     average
      context= '  Data listing '
      CALL xwrite(context,25) 
      context= '  ser#   bin#    rate      error      exposure'
      CALL xwrite(context,25) 
      DO m = 1, n
         DO k = 1, nanal
            IF(yi(k,m).GT.-1.1E34) THEN
               yp(k, m) = yi(k, m)/xnorm(m)
               syp(k, m) = syi(k, m)/xnorm(m)
            ELSE
               yp(k, m) = yi(k, m)
               syp(k,m) = syi(k, m)
            ENDIF
c
c Initialize expr for n > 1 to avoid crashes on different platforms  
            IF (n.eq.1) THEN
                expr(k) =expi(k,m)
            ELSE 
                expr(k) =1.
            ENDIF 
            write(context, 1000) m, k, yp(k, m), syp(k, m), expr(k)
            CALL xwrite(context,25)
         ENDDO
      ENDDO
c     
c     From here on only time series higher than 1
c     calculates simultaneounessa, ratio and sum
c     simultaneousness can be done only here
c     
      IF(n.GT.1) THEN
c     
c     initialise array
         DO m = n+1,nn
            DO k = 1, nanal
               yp(k, m) = 0.0
               syp(k,m) = 0.0
            ENDDO
         ENDDO
c     start ratio and sum
         
         context= '  Sum and Ratio  '
         CALL xwrite(context,25) 
         context= '  ser#   bin#    rate      error      exposure'
         CALL xwrite(context,25) 
         DO k =1, nanal
c     
c     SUM. The sum value is stored in the last column of the yp array
c     variable nn . The k bin in valid only if in all the n series is
c     not a gap LT -1.1E34. In this case data and exp set as gap.
c     NOTE yp and syp are still temporary
c     expr is reassigned internally in xrgetfm
c     valid for iflags(10)= 2, 3, 4
c     
            expr(k) = -1.2E34
            good=.true.
            DO m=1,n
               IF(yp(k,m).GT.-1.1E34.AND.good) THEN
                  yp(k,nn)=yp(k,nn)+yp(k,m)
                  syp(k,nn)= syp(k,nn)+syp(k,m)**2
                  expr(k) = 1.
               ELSE
                  yp(k, nn) = -1.2E34
                  syp(k, nn) = -1.2E34
                  good=.false.
               ENDIF
               write(context, 1000)nn, k, yp(k, nn), syp(k, nn), expr(k)
               CALL xwrite(context,25)
            ENDDO
c     apply norm and square the error
            IF(syp(k,nn).GT.0)THEN
               yp(k, nn) = yp(k,nn)/rvn
               syp(k, nn) = sqrt(syp(k,nn))/rvn
            ENDIF
c     
c     ratio
c     
c     calculate soft hardness ratio (only if both >0)
c     case n=2 n=3 n=4 the series dimension is always 1 and 2
c     the results of the ratio goes in n+1
c     
            IF (yp(k,1).GT.0.AND. yp(k,2).GT.0.) THEN
               yp(k, n+1) =
     &              (yp(k, 2)*xnorm(2))/(yp(k, 1)*xnorm(1))
               syp(k,n+1) = yp(k, n+1)*sqrt((syp(k,1)/
     &              yp(k,1))**2+(syp(k,2)/yp(k,2))**2)
            ELSE
               yp(k, n+1) = -1.2E34
               syp(k,n+1) = -1.2E34
            ENDIF
            write(context, 1000)n+1, k, yp(k, n+1), syp(k, n+1), expr(k)
            CALL xwrite(context,25)
c     
c     
            IF (n.gt.2) THEN
c     
c     calculate hard hardness ratio (only if both >0)
c     To figure out if there 4 or 3 time series, takes the interger of
c     half number of time series. In the case of n=3 the time series
c     for the hardness are yr(k,2) and yr(k,3), in the case of n=4 the
c     time series for the hardness are yr(k,3) and yr(k,4)
c     
               rv=n/2.+0.1
               iv=int(rv)
               IF (yp(k,iv+1).GT.0 .AND. yp(k,iv+2).GT.0.) THEN
                  yp(k, n+2) =
     &                 (yp(k,iv+2)*xnorm(iv+2))/(yp(k,iv+1)*xnorm(iv+1))
                  syp(k,n+2) = yp(k, n+2)*sqrt((syp(k,iv+1)/
     &                 yp(k,iv+1))**2+(syp(k,iv+2)/yp(k,iv+2))**2)
               ELSE
                  yp(k, n+2) = -1.2E34
                  syp(k,n+2) = -1.2E34
               ENDIF
               write(context, 1000)n+2, k, yp(k, n+2), syp(k, n+2), 
     $              expr(k)
               CALL xwrite(context,25)
            ENDIF
         ENDDO
      ENDIF
1000  FORMAT (' ', I4, 2X, I4, 2X, G11.4, 3X, G12.5, 3X, G11.4)
      RETURN
      END
