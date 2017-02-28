      SUBROUTINE xracsf(ipf, iend, intsta, rntsta, dntsta, iflags,
     &                 rflags, nanal, nintfm, nbint,  dxsta,
     &                 yi, syi, expi, yp, syp,
     &                 yr, syr, expr,irtsta, rrtsta,drtsta, 
     $                 iframe,status)
      implicit none
c
c This routine does the auto correlation and than average if necessary into a frame
c If iframe=0 return to get another interval.
c if iframe=1 the frame is completed and the plot and output can be done.,c
c
c
c I  ipf     i  integer parameter original xronos prameters file.
c I  iend    i  end of good data iend
c I  intsta  i  statistical variable: integer values
c I  rntsta  r  statistical variable: real varible
c I  dntsta  d  statistical variable: double precision
c I  iflags  i  integer flags
c I  rflags  r  real flags
c I  nanal   i  no of points in the final array (e.g. note nbint=nanal)
c I  nintfm  i  No of interval to avarage in a frame
c I  nbint   i  No of points in a interval
c I  dxtsta  d  lower frequency or frequency spacing
c I  yi      r  input count array c/s
c I  syi     r  input error count array
c I  expi    r  input exposure array
c O  yr      r  output psd array
c O  syr     r  'error' on acf 
c O  expr    r  number of ac in average per points
c O  irtsta  i  statistical results integer array
c O  rrtsta  r  statistical results real array
c O  drtsta  d  statistical results double precision
c O  iframe  i  if frame is complete =1
c
c
c Input varible
      include '../include/io.inc'
       INTEGER ipf(*), iend, intsta(20), iflags(20)
       INTEGER nbint, nanal, nintfm
       REAL*4    rntsta(20), rflags(10), dxsta
       REAL*8    dntsta(20)
       REAL*4 yi(nanal), syi(nanal), expi(nanal)
CNote that in the "fast" case yi has 2*nanal allocated in the
Ccalling routine. This is only used in the actual xracffast routine
Cthe actual number of analysis points *is* always nanal
c     
       LOGICAL fast
c Output varible
       INTEGER irtsta(20), iframe
       REAL*4 yr(nanal), syr(nanal), expr(nanal)
       REAL*4 rrtsta(20)
       REAL*8 drtsta(20)
c local variable
       INTEGER k,nv
       REAL*4 xnorm, rv
       REAL*4 yp(nanal), syp(nanal)
c       COMPLEX cdata(nmaxa)
       integer cdata,status

       include '../include/dmcommon.inc'
      parameter (subname = 'xracsf:')

       status=0

c  Initialize to avoid warning
      rv = 1.
c  --
c
       fast=.false.
       fast=(iflags(15).eq.1)

c not last intv, or good last intv
         IF (iend.NE.2) THEN
c standard; i.e. not expos profile
c          if(ipf(3).eq.0.and.iend.ne.2) then
c standard; i.e. not expos profile
            IF (ipf(3).EQ.0) THEN
               DO k = 1, nbint
                  IF (yi(k).GT.-1.1E34) THEN
c subtract avg.
                     yi(k) = yi(k) - rntsta(1)
                  ELSE
c gaps set to 0
                     yi(k) = 0.
                     syi(k) = 0.
                     expi(k) = 0.
                  ENDIF
               ENDDO
            ENDIF
c
c calculate auto correlation
c normalis. by N(good) only
            xnorm = float(intsta(2))
c
c standard normal. (observed variance)
            IF (ipf(4).EQ.1) xnorm = xnorm*rntsta(3)
c
c fancy normal. (excess variance if >0)
            IF (ipf(4).EQ.2 .AND. rntsta(8).GT.0.)
     &           xnorm = xnorm*rntsta(8)
c variance if <0
            IF (ipf(4).EQ.2 .AND. rntsta(8).LE.0.)
     &           xnorm = xnorm*rntsta(3)
c
            IF (fast) THEN
c allocate work memory
               nv=max(50,2*nbint)
               cdata = 0
               call udmget(nv,8,cdata,status)
               if(status.ne.0) then
                  errm = subname//' '//
     &                  'Trouble allocating work array in XRACSF'
                  call xaerror(errm, 5)
                  return
               endif
c
c call to fast acf note: no syi errors returned
c and syi are set to zero

               CALL xracffast(memx(cdata), yi, 2*nbint)
               call udmfre(cdata,8,status)
               if(status.ne.0) then
                errm = subname//' '//
     &                'Trouble deallocating work array in XRACSF'
                call xaerror(errm, 5)
                  return
               endif

               DO k = 1, nanal
                  yi(k) = yi(k)/xnorm
                  syi(k) = 0.
               ENDDO
c
c correction for 0 lag autocorrelation
               IF (ipf(4).EQ.2 .AND. rntsta(8).GT.0.) THEN
                   yi(1) = yi(1)*rntsta(8)/rntsta(3)
               ENDIF
            ELSE 
c
c call to slow 
              CALL xracfslow(nbint, yi, syi, yp, syp)
              DO k = 1, nanal
                 yi(k) = yp(k)/xnorm
                 syi(k) = syp(k)/xnorm
              ENDDO
c           correction for 0 lag autocorrelation
              IF (ipf(4).EQ.2 .AND. rntsta(8).GT.0.) THEN
                  yi(1) = yi(1)*rntsta(8)/rntsta(3)
                  syi(1) = syi(1)*rntsta(8)/rntsta(3)
              ENDIF
            ENDIF
            CALL XWRITE ( 'Auto correlation finished.', 10)
         ENDIF
c
c average results in frame
         CALL xrgetfm(nanal, nintfm, ipf, iend, iflags, rflags,
     &                intsta, rntsta, dntsta, yi, syi, irtsta, rrtsta,
     &                drtsta, yr, syr, expr, iframe)
c
c If iframe =1  finish off the norma and go out for plot
c otherwise go out and get another interval
c
         IF (iframe.EQ.1) THEN
c          
c apply normalization no 3
            IF (ipf(4).EQ.3) THEN
c (avg. excess variance in frame if >0)
               IF (rrtsta(8).GT.0.) rv = rrtsta(8)
c (avg. variance in frame if <0)
               IF (rrtsta(8).LE.0.) rv = rrtsta(3)
               DO k = 1, nanal
                  yr(k) = yr(k)/rv
                  syr(k) = syr(k)/rv
               ENDDO
c
c correction for 0 lag autocorrelation
               IF (rrtsta(8).GT.0.) THEN
                  yr(1) = yr(1)*rrtsta(8)/rrtsta(3)
                  syr(1) = syr(1)*rrtsta(8)/rrtsta(3)
               ENDIF
c
            ENDIF
         ENDIF
       RETURN
      END
c
