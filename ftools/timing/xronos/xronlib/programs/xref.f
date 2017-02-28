c
       SUBROUTINE xref(ipf, iend, intsta, rntsta, dntsta, iflags, 
     &                 rflags, nanal, nintfm, nbint,
     &                 yi, syi, expi,yp,syp,
     &                 yr, syr, expr, dumexpr,
     &                 irtsta, rrtsta,drtsta, iframe)
       implicit none
c
c  This routine calcultes ratio and sum for the input array yi, syi, expi 
c  for the folding programs. The average per interval is also included.
c  If iframe=0 return to get another interval.
c  if iframe=1 the frame is completed and the plot and output can be done.
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
c I  yi      r  input count array c/s
c I  syi     r  input error count array
c I  expi    r  input exposure array
c O  yr      r  output psd array
c O  syr     r  'error' on psd (Note if interval is 1 syr = yr)
c O  expr    r  number of fft in average per points
c O  dumexpr r  dummy array fed to xrlc to avoid clobbering expr
c O  irtsta  i  statistical results integer array
c O  rrtsta  r  statistical results real array
c O  drtsta  d  statistical results double precision
c O  iframe  i  if frame is complete =1
c
c input variable
       INTEGER*4 ipf(*), iend, iflags(20), intsta(20,*)
       INTEGER*4 nbint, nanal, nintfm
       REAL*4 rntsta(20,*), rflags(10)
       REAL*8 dntsta(20,*)
       REAL*4 yi(nanal,*), syi(nanal,*), 
     $      expi(nanal,*)
c 
c output variable 
       INTEGER*4 irtsta(20,*), iframe, nr
       REAL*4 yr(nanal,*), syr(nanal,*), expr(nanal), dumexpr(nanal)
       REAL*4 rrtsta(20,*)
       REAL*8 drtsta(20,*)
c
c local variable  
       INTEGER*4 n, m
       REAL*4 rv, rvn , xnorm(4)
       REAL*4 yp(nanal,7), syp(nanal,7)
       character(80) context 
c
c  not last intv., or good last intv.
       IF (iend.NE.2) THEN

c n number of series 
          n=iflags(10)
c
c Set the normalization for the series if xnorm=1 no norm
c otherwise standard norm (rescale by average)
C Huh?
C I think this means if "normalization parameter == 1" rescale by
C average, otherwise, set xnorm=1 which means "no normalization"
C     LB
          DO m=1, n
             xnorm(m) = 1.
             IF (ipf(4).EQ.1) xnorm(m) = xnorm(m)*rntsta(1, m)
          rvn=1.
          ENDDO
          IF(ipf(4).EQ.1) rvn=n  
          CALL xrlc(nanal, n, xnorm, rvn, yi, syi, expi,
     &                yp, syp, dumexpr)
          CALL XWRITE('  Folded light curve ready', 10)   
       ENDIF 
c
c average results in frame
       CALL xrgetfm ( nanal, nintfm, ipf, iend, iflags, rflags, 
     &             intsta, rntsta, dntsta, yp, syp, irtsta, rrtsta,
     &             drtsta, yr, syr, expr, iframe)

c     RETURN
      END
