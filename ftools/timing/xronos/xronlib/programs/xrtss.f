      subroutine xrtss(ipf, iend, intsta, rntsta, dntsta, iflags,
     $                 rflags, nanal, nintfm, nbint, 
     $                 yi, syi, expi, yp, syp, 
     $                 yr, syr, expr, irtsta, rrtsta, drtsta, 
     $                 iframe)
      implicit none
C
C     This routine does the time skewness caculation and then averages into
C     a frame if requested
c If iframe=0 return to get another interval.
c if iframe=1 the frame is completed.
c I  ipf     i  integer parameter original xronos prameters file.
c I  iend    i  end of good data iend
c I  intsta  i  statistical variable: integer values
c I  rntsta  r  statistical variable: real varible
c I  dntsta  d  statistical variable: double precision
c I  iflags  i  integer flags
c I  rflags  r  real flags
c I  nanal   i  no of points in the final array (note nbint=nanal)
c I  nintfm  i  No of interval to avarage in a frame
c I  nbint   i  No of points in a interval
c I  yi      r  input count array c/s
c I  syi     r  input error count array
c I  expi    r  input exposure array
C    yp      r  intermediate count array (passed in because allocated above)
C    syp     r  intermediate count array (passed in because allocated above)
c O  yr      r  output ts array
c O  syr     r  'error' on ts 
c O  expr    r  number of ts in average per points
c O  irtsta  i  statistical results integer array
c O  rrtsta  r  statistical results real array
c O  drtsta  d  statistical results double precision
c O  iframe  i  if frame is complete =1
c Input variables
       INTEGER ipf(*), iend, intsta(20), iflags(20)
       INTEGER nbint, nanal, nintfm
       REAL*4    rntsta(20), rflags(10), dxsta
       REAL*8    dntsta(20)
       REAL*4 yi(nanal), syi(nanal), expi(nanal)
c input pointers
       REAL*4 yp(nanal), syp(nanal)
c Output varible
       INTEGER irtsta(20), iframe
       REAL*4 yr(nanal), syr(nanal), expr(nanal)
       REAL*4 rrtsta(20)
       REAL*8 drtsta(20)
c local variables
       integer k
       real*4 xnorm
       character(80) context

c     
c     subtract average from intv. and fill gaps with 0 (=avg)
c     
c     not last intv, or good last intv
      IF (iend.NE.2) THEN
c     standard; i.e. not expos profile
c     if(ipf(3).eq.0.and.iend.ne.2) then
c     standard; i.e. not expos profile
         IF (ipf(3).EQ.0) THEN
            DO k = 1, nbint
               IF (yi(k).GT.-1.1E34) THEN
c     subtract avg.
                  yi(k) = yi(k) - rntsta(1)
               ELSE
c     gaps set to 0
                  yi(k) = 0.
                  syi(k) = 0.
                  expi(k) = 0.
               ENDIF
            ENDDO
         ENDIF
c     
c     call fft routine and calculate time skewness
c     
c     normalis. by N(good) only
c     
         xnorm = float(intsta(2))
c     standard normal. (observed 3rd Moment)
c     
         IF (ipf(4).EQ.1) xnorm = xnorm*rntsta(5)
c     
         CALL xrtsfslow(nbint, yi, syi, yp, syp)
c     
         DO k = 1, nanal
            yp(k) = yp(k)/xnorm
            syp(k) = syp(k)/xnorm
         ENDDO
         WRITE (context, 1100)
         call xwrite(context,10)
 1100    FORMAT (13X, 'Time Skewness ready !')
      ENDIF
c     
c     average results in frame
c     
      CALL xrgetfm(nanal, nintfm, ipf, iend, iflags, rflags, 
     &     intsta, rntsta, dntsta, yp, syp, irtsta, rrtsta,
     &     drtsta, yr, syr, expr, iframe)
      
      END
