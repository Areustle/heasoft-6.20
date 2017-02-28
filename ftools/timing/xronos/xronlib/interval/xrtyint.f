c
      SUBROUTINE xrtyint(nser, nbint, dtnb, yi, syi, expi, 
     &     intsta, rntsta, dntsta,dper,dpdot, progtype, chatheight)
      implicit none
c
c ls  23/8/88 to type infos on interval for analyses requiring 1 series
c     I   nser = No. of series for which infos are typed
c     I   nbint = no. of newbins per interval
c     I   dtnb = duration of a newbin in secs
c     I   yi = cts/s in each new bin of interval (-1.2e34 or less means gap)
c     I   syi = error on cts/s in each new bin of interval
c     I   expi = exposure fraction (0->1) in each newbin
c     I   intsta = statistics of this interval (integer*4) (20)
c     I   rntsta =     "      "   "      "     (real*4)    (10)
c     I   dntsta =     "      "   "      "     (real*8)    (5)
c
c
c   xrskline, xrdhms = subroutines used
c
      INTEGER*4  k, nbint, intsta(nbint), itim(5),
     &          jtim(5), lt, ll, j, nser, chatheight
      character(10) progtype
      REAL*4 rv, yi(nbint), syi(nbint), expi(nbint), rntsta(nbint), 
     &      xrchithr, probga
      REAL*8 dntsta(20), swi, dtnb, dtime, dphase, gammq,dper,dpdot
c      double precision dtimei(nbint)
      character(80) context
      external xrchithr
c
      DATA probga/1.35E-3/
c
c
      IF (intsta(9).EQ.1) CALL xwrite(' ',10+chatheight)
c
c     Only basic info
c
      if (nser.eq.1) then
         if(progtype(1:10).eq.'FOLDSEARCH') then
            write (context, 990) dper,dpdot
            call xwrite(context,10+chatheight)
         endif
         CALL xrdhms(dntsta(1), swi, itim, 1)
         WRITE (context, 1000) intsta(9), (itim(k), k=1, 4)
         call xwrite(context,10+chatheight)
      endif
      WRITE (context, 1100) nser, rntsta(1), rntsta(9), rntsta(3),
     &     intsta(2)
      call xwrite(context,10+chatheight)
 990  FORMAT (1X, 'Period : ', G11.4, 'dP/dt : ',G11.4)
 1000 FORMAT (1X, 'Intv', I5, '   Start ', I5, I3, ':', I2, ':', I2)
 1100 FORMAT (1X, '    Ser.', I1, 5X, 'Avg', G11.4, '    Chisq', G11.4,
     &        '   Var', G11.4, ' Newbs.', I7)
c
c     Standard info
c
      WRITE (context, 1001) rntsta(6), rntsta(7), rntsta(4), intsta(3)
      call xwrite(context,10+chatheight)
 1001 FORMAT (1X, 9X, 5X, 'Min', G11.4, '      Max', G11.4, 'expVar',
     &        G11.4, '  Bins', I7)

c
c     Additional Statistical info
c
cc         sigma of expected variance  (replaced with rntsta(14)
c      sexvar=sqrt(abs(2.*rntsta(4)**2/float(max(intsta(2)-1,1))))
cc         sigma of observed variance (replaced with rntsta(13)
c      svar=sqrt(abs(2.*rntsta(3)**2/float(max(intsta(2)-1,1))))
c
c rms is meaningful only for >0 count rates
      IF (rntsta(1).GT.0.) THEN
c
c Work out prob of corresponinding Chisq. distrib. and compare it with 3sigma
c one-sided Gaussian probability
c
         IF (gammq(dble(max(intsta(2)-1,1)/2.),dble(max(intsta(2)-1,1)
     &       *rntsta(3)/rntsta(4)/2.)).LT.dble(probga)) THEN
c           value and error of rms fractional variation if > 3 sigma
c        replaced with rntsta(10)
c + ve rms
c          if(rntsta(8).ge.0.) rms=sqrt(rntsta(8))/rntsta(1)
c - ve rms
c          if(rntsta(8).lt.0.) rms=-sqrt(-rntsta(8))/rntsta(1)
c           sigma of rms (see red notebook p. 141) (replaced by rntsta(20))
c          srms=abs(rntsta(10))*rntsta(3)/rntsta(4)/
c     &      (sqrt(abs(2.*float(max(intsta(2)-1,1))))*(rntsta(3)/rntsta(4)-1.))
cwrong       srms=abs(rms)*sqrt(abs(1./(float(intsta(2))*rntsta(4))+
cwrong             &     0.5/((rntsta(3)/rntsta(4)-1.)**2*float(intsta(2)-1))))
c
            WRITE (context, 1006) rntsta(10), rntsta(20), rntsta(13)
            call xwrite(context,15+chatheight)
 1006       FORMAT (1X, 9X, 5X, 'Rms', G11.4, '     DRms', G11.4,
     &              '  DVar', G11.4)
         ELSE
c           3 sigma upper limit on rms based on sigma of expected var.
c          rv=sqrt(abs(3.*rntsta(14)))/rntsta(1)
c
c  3 sigma upper limit based on relevant chisq distr.
            rv = xrchithr(max(intsta(2)-1,1),probga)
            if ( rv.eq.-1000. ) then
               WRITE (context, 1007) 'Undef', rntsta(13)
 1007          FORMAT (1X, 9X, 5X, 'Rms (3 sigma upper lim) ', a,
     &              '  DVar', G11.4)
            else
               rv = sqrt(abs(rntsta(4)*rv
     &              /float(max(intsta(2)-1,1))-rntsta(3)))/rntsta(1)
               WRITE (context, 1008) rv, rntsta(13)
 1008          FORMAT (1X, 9X, 5X, 'Rms (3 sigma upper lim)', G11.4,
     &              '  DVar', G11.4)
            endif
            call xwrite(context,15+chatheight)
         ENDIF
      ENDIF
c
      CALL xrdhms(dntsta(3), swi, itim, 1)
      CALL xrdhms(dntsta(4), swi, jtim, 1)
      WRITE (context, 1002) (itim(k), k=1, 4), rntsta(5)
      call xwrite(context,15+chatheight)
      WRITE (context, 2002) 
     &     (jtim(k), k=1, 4), rntsta(2)
      call xwrite(context,15+chatheight)
 1002 FORMAT (1X, 9X, '   First', I5, I3, ':', I2, ':', I2, '  M3rd',
     &        G11.4)
 2002 FORMAT( 1X, 9X, '    Last', I5, I3, ':', I2, ':', I2,
     &        ' Expos', G11.4)
c
c     Total Statistical infos
c
      CALL xrdhms(dntsta(2), swi, itim, 1)
      WRITE (context, 1003) (itim(k), k=1, 4)
      call xwrite(context,15+chatheight)
      write (context,1023) 
      call xwrite(context,15+chatheight)
      write (context, 2003)  intsta(10),intsta(20)
      call xwrite(context,15+chatheight)
      write (context, 3003)  intsta(11),intsta(12)
      call xwrite(context,15+chatheight)
      write (context,4003) intsta(13),intsta(16)
      call xwrite(context,15+chatheight)
      write (context,5003) intsta(19)
      call xwrite(context,15+chatheight)
      write (context,6003) 
      call xwrite(context,15+chatheight)
      write (context,7003) intsta(14), intsta(17)
      call xwrite(context,15+chatheight)
 1003 FORMAT (1X, 9X, '  Center', I5, I3, ':', I2, ':', I2)
 1023 FORMAT (T45,'   Bins rejected (total):')
 2003 FORMAT (T45, '   Overflow', I7,'  Data gaps',  I7)
 3003 FORMAT (T45, '   Time Win', I7, '  Phase Win', I7)
 4003 FORMAT (T45, '   Ints Win', I7, '  Expos Win', I7)
 5003 FORMAT (T45, '   Neg Indx', I7)
 6003 FORMAT (T45, '   Newbins rejected (total):')
 7003 FORMAT (T45, '   Ints Win', I7, '  Expos win', I7)
c
c     Type all values for individual Newbins
c
      WRITE (context, 1004) nser
      call xwrite(context,15+chatheight) 
      call xwrite(' ',15+chatheight)
      if ( progtype(1:4).eq.'FOLD' ) then
         WRITE (context, 3004) 
         call xwrite(context,15+chatheight)
         DO k = 1, nbint
            IF (yi(k).GT.-1.1E34) THEN
               dphase = dble(k-1)/dble(nbint)
               WRITE (context, 2005) k, dphase,
     &                yi(k), syi(k), expi(k)
               call xwrite(context,15+chatheight)
            ENDIF
         ENDDO
      else
         WRITE (context, 2004) 
         call xwrite(context,15+chatheight)
         DO k = 1, nbint
            IF (yi(k).GT.-1.1E34) THEN
               dtime = dntsta(1) + dble(k-1)*dtnb/86400.D0
               CALL xrdhms(dtime, swi, itim, 1)
               WRITE (context, 1005) k, dtime, (itim(j), j=2, 5)
     &              , yi(k), syi(k), expi(k)
               call xwrite(context,15+chatheight)
            ENDIF
         ENDDO
      endif
 1004 FORMAT (1X, 'Series ', I1)
 2004 FORMAT ( 1X,
     &        'NewBin         Day         h  m  s  ms ',
     &        '   Val(c/s)     Err(c/s)      Exps. ')
 3004 FORMAT (1X, 'NewBin  Phase    Val(c/s)    Err(c/s)      Exps. ')
 1005 FORMAT (1X, I5, 2X, F17.11, I4, I3, I3, I4, G12.4, G12.4, G12.4)
 2005 FORMAT (1X, I5, 2X, F9.6, G12.4, G12.4, G12.4)
c
      call xwrite(' ',15+chatheight)
c
      RETURN
      END
c
c
c
