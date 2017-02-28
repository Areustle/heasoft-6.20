c     
      SUBROUTINE xres(progtype, ipf, iend, intsta, rntsta, dntsta, 
     $     iflags, rflags, nanal, nbint, nser, yi, syi, expi,
     &     yr, syr, expr, chival, chierr, dper, dpdot, kmax)
      implicit none
c
c  Move the array of chi*2 statistical values into chival and chierr.
c  Searches for the best period and move the best lightcurve into yr.
c  currently yr can not be either plot or write out. 
c  The return from this routine is always plotting and/or writing 
c  there is no iframe option.   
c
c I  progtypec  program type       
c I  ipf     i  integer parameter original xronos prameters file.
c I  iend    i  end of good data iend
c I  intsta  i  statistical variable: integer values
c I  rntsta  r  statistical variable: real varible
c I  dntsta  d  statistical variable: double precision
c I  iflags  i  integer flags
c I  rflags  r  real flags
c I  nanal   i  number of periods
c I  nbint   i  no of points in the final array (e.g. no of phases)
c I  nser    i  no of time series being analyzed (= iflags(10) for array dims)
c I  yi      r  input count array c/s
c I  syi     r  input error count array
c I  expi    r  input exposure array
c O  yr      r  output folded array
c O  syr     r  'error' on folded array
c O  expr    r  
c O  chival  r  array of chisquared values
c O  chierr  r  array of chisquared err values
c O  dper    d  array of periods
c O  dpdot   d  array of period derivatives
c O  kmax    i  pointer for dpera array for best period
c
c input variables
      character(10) progtype
      INTEGER*4 nanal, nbint, nser
      INTEGER*4 ipf(*), iend, iflags(20), intsta(20,nser,nanal)
      REAL*4 rflags(10), rntsta(20,nser,nanal)
      REAL*4 yi(nbint,nser,nanal), syi(nbint,nser,nanal)
      REAl*4 expi(nbint,nser,nanal)
      REAL*8 dntsta(20,nser,nanal) 
      real*8 dper(nanal), dpdot(nanal)
c     
c     output variable 
      INTEGER*4 kmax(4)
      REAL*4 yr(nbint,*), syr(nbint,*), expr(nbint,*)
      REAL*4 chival(nanal,*), chierr(nanal,*)
c     
c     local variable  
      INTEGER*4 i, m, j 
      REAL*4 chitest    
      double precision ddum
c     
c     not last intv., or good last intv.
      IF (iend.NE.2) THEN
c     
c     Move the chi*2 into rrtsta and found the best value
         DO m=1, iflags(10) 
            chitest=-999999.
            kmax(m)=-100
            DO i=1, nanal
               chival(i,m) = rntsta(9,m,i)
c               chierr(i,m) = rntsta(19,m,i)
c To make a chierr similar to error calculated for other 
c quantity.
               chierr(i,m) = (rntsta(19,m,i)*chival(i,m))/intsta(2,m,i)
               IF(chival(i,m).GT.chitest)THEN
                  chitest=chival(i,m) 
                  kmax(m)=i
               ENDIF
            ENDDO
         ENDDO
c     
c     here move the yi array into the result array
c     First index is the phase the second is the series  
         DO m=1, iflags(10)
            j=kmax(m)
            DO i=1,nbint
               yr(i,m) = yi(i,m,j)
               syr(i,m) = syi(i,m,j)
               expr(i,m)= expi(i,m,j)
            ENDDO
         ENDDO
         CALL XWRITE('  Chisq. vs. period ready', 10)   
      ENDIF 
      do m=1,iflags(10)
      call xrtyint(m,nbint,ddum,yi(1,m,kmax(m)),syi(1,m,kmax(m)),
     $        expi(1,m,kmax(m)),
     $        intsta(1,m,kmax(m)),rntsta(1,m,kmax(m)),
     $        dntsta(1,m,kmax(m)),
     $        dper(kmax(m)),dpdot(kmax(m)),progtype,0)
      enddo
      return
      END
