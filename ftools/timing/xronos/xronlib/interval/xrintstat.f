C     Subroutine xrintstat
C
C     Description:
C     
C
C     Arguments:

      subroutine xrintstat(nbint,nkount,dntsta,dtnb,iflags,
     $     intsta,rntsta,expi,yi,syi,progtype)
      implicit none
      character(10) progtype
      integer iflags(*),intsta(20,*),nbint,nkount
      double precision dntsta(20,*),dtnb
      real rntsta(20,*),expi(nkount,*),
     $     syi(nkount,*),yi(nkount,*)
C     LOCAL
      double precision dv
      real rv
      real var1, var2, var3, var4, eval13
      integer iv,k,m

c  Initialize to avoid warning
      dv = 0.d0
      iv = 0
      var2 = 0.
c  --

c  Calculate interval statistics
c
      do m=1,iflags(10)
         if(progtype(1:4).eq.'TIME') then
            dv = dntsta(1, m) - dtnb/86400.D0
            iv = 0
         else
            dntsta(3,m) = dntsta(1,m)
         endif
         rv = 0.
c initialize for later use  (this shouldn't be here and I've no idea
C why it was here...) LB
c         intsta(4,m) = 0
         DO k = 1, nkount
c center time of k-th bin (days)
         if(progtype(1:4).eq.'TIME') 
     $           dv = dv + dtnb/86400.D0
            IF (yi(k,m).GT.-1.1E34) THEN
c no. of good newbins in intv.
               intsta(2, m) = intsta(2, m) + 1
c for avg. cts/s in intv.
               rntsta(1, m) = rntsta(1, m) + yi(k, m)
c for avg. newb exp (see chis)
               rv = rv + expi(k, m)
c for obs. variance in intv.
c               rntsta(3, m) = rntsta(3, m) + yi(k, m)*yi(k, m) !Rev.10
c for expec. var. in intv.
               rntsta(4, m) = rntsta(4, m) + syi(k, m)*syi(k, m)
c for third moment
c               rntsta(5, m) = rntsta(5, m) + yi(k, m)*yi(k, m)*yi(k, m)!Rev.10
c set new min
               IF (rntsta(6,m).GT.yi(k,m)) rntsta(6, m) = yi(k, m)
c set new max
               IF (rntsta(7,m).LT.yi(k,m)) rntsta(7, m) = yi(k, m)
               if(progtype(1:4).eq.'TIME') then
c     for time of barycenter of intv.
                  dntsta(2, m) = dntsta(2, m) + dv
                  IF (iv.EQ.0) THEN
                     iv = 1
c     set center time of first good newbin
                     dntsta(3, m) = dv
                  ENDIF
c     for center time of last  good newbin
                  dntsta(4, m) = dv
               endif
            ENDIF
         ENDDO
c
c         write(*,*)'intsta(2,m)',intsta(2,m)
         IF (intsta(2,m).GT.1) THEN
c avg. cts/s in intv.
            rntsta(1, m) = rntsta(1, m)/float(intsta(2,m))
c fract. expos. in intv.
            rntsta(2, m) = float(intsta(2,m))/float(nkount)
c  for avg. newb exp (see chis)
            rv = rv/float(intsta(2,m))

c
c            write(*,*)'rntsta(2,m)',rntsta(1,m),rntsta(2,m),rv
c
c     loop to calculate variance and third moment more accurately 
            DO k = 1, nkount
               IF (yi(k,m).GT.-1.1E34) THEN
               rntsta(3, m) = rntsta(3, m) + (yi(k, m)-rntsta(1,m))**2
               rntsta(5, m) = rntsta(5, m) + (yi(k, m)-rntsta(1,m))**3
               ENDIF
            ENDDO
c
c obs. variance in intv.
            rntsta(3, m) = rntsta(3, m)/float(intsta(2,m))
c expec.variance in intv.
            rntsta(4, m) = rntsta(4, m)/float(intsta(2,m))
c obs. 3rd mom.in intv.
            rntsta(5, m) = rntsta(5, m)/float(intsta(2,m))
c excess variance
            rntsta(8, m) = rntsta(3, m) - rntsta(4, m)
c do for chisquare
c
c            write(*,*)'do chisquare',rntsta(4,m),rntsta(5,m)
c            write(*,*)'do chisquare',rntsta(3,m),rntsta(8,m)
c
            DO k = 1, nkount
               IF (yi(k,m).GT.-1.1E34) THEN
c            use error of individual points in chisquare 
                 IF(rntsta(4,m).ne.0)
     &             rntsta(9, m) = rntsta(9, m)
     &                         + ((rntsta(1,m)-yi(k,m))**2/rntsta(4,m)
     &                         /(rv/expi(k,m))**2)
c
c
c This is will insert in later versions. Start the calculation of the variance of 
c the excess variance needs some more testing. Define a new excess variance 
c normalized by the average. The array to carry out this information is 
c too small. The error of the ecess variance can be place in rntsta(18,n)   
c not currently used. 
c var(exce)=1/N-1 sum_i{((Xi-Xm)**2 -sXi**2)-exce}**2
c where X_i = yi(k, m) sXi = syi(k, m) Xm=rntsta(1,m)
c N=insta(2,m)
c
                   var1=(yi(k, m)-rntsta(1,m))**2-syi(k, m)**2 
                   var2= var2 + (var1 - rntsta(8, m))**2
               ENDIF
            ENDDO
c
c            write(*,*)'done chisquare',rntsta(9,m)
c                                                        !rms variability
            IF(rntsta(1,m).ne.0)THEN 
               IF (rntsta(8,m).GE.0.) 
     &              rntsta(10, m) = sqrt(rntsta(8,m))/rntsta(1, m)
               IF (rntsta(8,m).LT.0.) 
     &              rntsta(10, m) = -sqrt(-rntsta(8,m))/rntsta(1, m)
            ENDIF
c part for theoretical errors (wherever possible)
c
c                                                        !sigma of avg.
            rntsta(11, m) = sqrt(abs(rntsta(4,m)/float(intsta(2,m)-1)))
c                                                        !sigma of obs. var.
            rntsta(13, m) = sqrt(abs((2.*rntsta(3,m)**2)/
     &                      float(intsta(2,m)-1)))
c                                                        !sigma of exp. var.
            rntsta(14, m) = sqrt(abs((2.*rntsta(4,m)**2)/
     &                      float(intsta(2,m)-1)))
c                                                         !sigma excess variance
            rntsta(18, m) = sqrt(abs(var2)/
     &                      float(intsta(2,m)*(intsta(2, m)-1)))
c          write(*,*)'var1, var2, rntsta(18,m) ', var1,var2,rntsta(18,m)
c
c                                                        !sigma of chisq.distr.
            rntsta(19, m) = sqrt(abs(2.*float(intsta(2,m)-1)))
c
c excess variance normalized and error. The variance does not include the mean 
c in the calculataion 
c
c           var3=rntsta(8,m)/rntsta(1,m)**2
c           var4=rntsta(18, m)/rntsta(1,m)**2
c            write(*,*)'var3, var4 ',var3, var4
c
c            write(*,*)'var exc',rntsta(3,m),rntsta(4,m)
c            write(*,*)'exc chi',rntsta(8,m),rntsta(9,m)
c            write(*,*)'ave,no',rntsta(1,m),intsta(2,m)
c            write(*,*)'err var exp ',rntsta(13,m),rntsta(14,m)
c            write(*,*)'rntsta(19, m) chi',rntsta(19, m)
            eval13=sqrt(rntsta(13,m)**2+rntsta(14, m)**2)
c            write(*,*)'Err exc',rntsta(13,m)+rntsta(14, m),eval13
c
            IF(rntsta(3,m).eq.0.or.rntsta(4,m).eq.0)THEN
               rntsta(20,m)=0.
            ELSE 
c
c This fixes the bug but the function is not good approximated at zero 
c The excess variance normalized is a better choice
c 
                 IF(rntsta(3,m).NE.rntsta(4,m))THEN 
                   rntsta(20,m)=abs(rntsta(10,m)*rntsta(3,m)/rntsta(4,m)
     &                      *sqrt(abs(2./float(intsta(2,m)-1)))
     &                      /(2.*(rntsta(3,m)/rntsta(4,m)-1.)))
                 ELSE
                   rntsta(20,m)=0.
                 ENDIF 
c              
c            write(*,*)'rms err',rntsta(10,m),rntsta(20,m)
            ENDIF
c
c time of baryc. of intv.
            if(progtype(1:4).eq.'FOLD') then
c                write(*,*)'dntsta(2, m) fold',dntsta(2, m),intsta(3,m)
                dntsta(2, m) = dntsta(2, m)/dble(intsta(3,m))
            else
                dntsta(2, m) = dntsta(2, m)/dble(intsta(2,m))
c                write(*,*)'dntsta(2, m) time',dntsta(2, m)
            endif
         ENDIF

c
      enddo
      return
      end
