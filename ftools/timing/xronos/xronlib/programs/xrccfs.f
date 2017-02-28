       SUBROUTINE xrccfs(ipf, iend, intsta, rntsta, dntsta, iflags,
     &                 rflags, nanal, nintfm, nbint, dxsta,
     &                 yi, syi, expi, yp, syp,
     &                 yr, syr, expr,irtsta, rrtsta,drtsta, iframe,
     $                 status)
      implicit none
c
c This routine does the FFT and than average if necessary into a frame
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
c I  nanal   i  no of points in the final array (e.g. no of freq for fft)
c I  nintfm  i  No of interval to avarage in a frame
c I  nbint   i  No of points in a interval
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
      include '../include/io.inc'
       INTEGER*4 ipf(*), iend, iflags(*), intsta(20,*)
       INTEGER*4 nbint, nanal, nintfm, nmaxa
       REAL*4    rntsta(20,*), rflags(10), dxsta
       REAL*8    dntsta(20,*)
       REAL*4 yi(2*nbint,*), syi(2*nbint,*), 
     $      expi(2*nbint,*)
! temporary
       LOGICAL fast
c Output varible
       INTEGER*4 irtsta(20,*), iframe, status
       REAL*4 yr(2*nbint), syr(2*nbint), expr(2*nbint)
       REAL*4 rrtsta(20,*)
       REAL*8 drtsta(20,*)
c local variable
       INTEGER*4 k, m , nv
       REAL*4 xnorm, rv
       REAL*4 yp(nbint*2), syp(nbint*2)
c       COMPLEX cowa(nmaxa), cowb(nmaxa)
       integer cowa,cowb
c
       include '../include/dmcommon.inc'
      parameter (subname = 'xrccfs:')

       status=0
       
c
       fast=.false.
       fast=(iflags(15).eq.1)

       call xrccshuffle(nbint,yi,syi)
c
c subtract average from intv. and fill gaps with 0 (=avg)
c not last intv., or good last intv.
         IF (iend.NE.2) THEN
c
c standard; i.e. not expos. profile
            IF (ipf(3).EQ.0) THEN
               DO m = 1, 2
                  DO k = 1, nbint
                     IF (yi(k,m).GT.-1.1E34) THEN
c
c subtract average
                        yi(k, m) = yi(k, m) - rntsta(1, m)
                     ELSE
c
c gaps set =0
                        yi(k, m) = 0.
                        syi(k, m) = 0.
                        expi(k, m) = 0.
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
c
c normalization by N(good) only
            xnorm = sqrt(float(intsta(2,1))*float(intsta(2,2)))
c
c standard normal. (sqrt of product of observed variances)
            IF (ipf(4).EQ.1) xnorm = xnorm*sqrt(rntsta(3,1)*rntsta(3,2))
c
c fancy normal. no.2
            IF (ipf(4).EQ.2) THEN
c
c (sqrt of product of excess variances if >0)
               IF (rntsta(8,1).GT.0. .AND. rntsta(8,2).GT.0.) THEN
                  xnorm = xnorm*sqrt(rntsta(8,1)*rntsta(8,2))
c
c (sqrt of product of variances if <0)
               ELSE
                  xnorm = xnorm*sqrt(rntsta(3,1)*rntsta(3,2))
               ENDIF
            ENDIF
c
c call to fast routine
            IF (fast) THEN
c allocate work memory
               nv=max(50,2*nbint)
               cowa = 0
               cowb = 0
               call udmget(nv,8,cowa,status)
               call udmget(nv,8,cowb,status)
               if(status.ne.0) then
                  errm = subname//' '//
     &                  'Trouble allocating work arrays in XRCCFS'
                  call xaerror(errm, 5)
                  return
               endif
C     
c
c Note: no error
               CALL xrccffast(yi(1,1), yi(1,2), 2*nbint, memx(cowa),
     $              memx(cowb), yp)

               call udmfre(cowa,8,status)
               call udmfre(cowb,8,status)
               if(status.ne.0) then
                errm = subname//' '//
     &                'Trouble deallocating work arrays in XRCCFS'
                call xaerror(errm, 5)
                  return
               endif

c

c apply normalization
               DO k = 1, nanal
                  yp(k) = yp(k)/xnorm
c note : no theor. errors syp
                  syp(k) = 0.
               ENDDO
            ELSE
c
c call to slow routine
              CALL xrccfslow(nbint, yi, syi, yp, syp)
c
c apply normalization
              DO k = 1, nanal
                 yp(k) = yp(k)/xnorm
                 syp(k) = syp(k)/xnorm
              ENDDO
            ENDIF
            CALL XWRITE('Cross correlation ready !', 10)
         ENDIF
c
c average results in frame
c
         CALL xrgetfm(nanal, nintfm, ipf, iend, iflags, rflags,
     &                intsta, rntsta, dntsta, yp, syp, irtsta, rrtsta,
     &                drtsta, yr, syr, expr, iframe)
c
         IF (iframe.EQ.1) THEN
c fancy normal. no.3
c
            IF (ipf(4).EQ.3) THEN
c
c sqrt of product of avg. excess variances in frame if >0
               IF (rrtsta(8,1).GT.0. .AND. rrtsta(8,2).GT.0.) THEN
                  rv = sqrt(rrtsta(8,1)*rrtsta(8,2))
c
c sqrt of product of avg. variances in frame if <0
               ELSE
                  rv = sqrt(rrtsta(3,1)*rrtsta(3,2))
               ENDIF
c apply normalization
               DO k = 1, nanal
                  yr(k) = yr(k)/rv
                  syr(k) = syr(k)/rv
               ENDDO
            ENDIF
         ENDIF
      RETURN 
      END


      subroutine xrccshuffle(nbint,yi,syi)
C     reshuffles the elements of yi and syi from an nbint "fastest
C     varying axis" to a 2*nbint "fastest varying axis"
      implicit none
      integer nbint
      real yi(nbint*4),syi(nbint*4)

      integer iv

      do iv=nbint+1,2*nbint
         yi(iv+nbint)=yi(iv)
         yi(iv)=0.0
         yi(iv+2*nbint)=0.0
         syi(iv+nbint)=syi(iv)
         syi(iv)=0.0
         syi(iv+2*nbint)=0.0
      enddo
      return
      end
