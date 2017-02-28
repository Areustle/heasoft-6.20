c
      SUBROUTINE xrgetfm(nanal, nintfm, ipf, iend, iflags, rflags,
     &                   intsta, rntsta, dntsta, yi, syi, irtsta,
     &                   rrtsta, drtsta, yr, syr, expr, iframe)
      implicit none
c
c ls  12/9/88 to accumulate frame for n series analyses
c     IMPORTANT: since expr is a one-dimensional array it is used as a counter
c                of good result bins even in the case in which the results
c                array are multidimensional. This means that all result arrays
c This might not be OK
c                must have "gaps" in the same points
c                for some (yet unknown) type of analysis.
c ls 26/7/89 Rev. rntsta(20,m) and rrtsta(20,m) to include stat.variab errors
c
c     I   nanal = no. of points in the analysis
c     I   nintfm = no. of intvs/frame (maximum)
c     I   ipf = from xronos.pf (above ipf(2) sigma is from scatter)
c     I   iend = 0=more data to come, 1=last good intv, 2=last bad intv
c     I   iflags = int*4 flags for plots, file type, analysis type (i*4x20)
c     I   rflags = real*4 flags for plots, file type, analysis type (r*4x10)
c     I/o intsta = interval. stat. param. (I*4x20) (see xrgetint1) (o for (4))
c     I   rntsta = interval. stat. param. (R*4x20) (see xrgetint1)
c     I   dntsta = interval. stat. param. (R*8x5)  (see xrgetint1)
c     I   yi = newbin values (in intv.)
c     I   syi = newbin errors (   "  )
c     O   irtsta = frame stat. param. (I*4x20) (see xrgetint1)
c     O   rrtsta = frame stat. param. (R*4x20) (see xrgetint1)
c     O   drtsta = frame stat. param. (R*8x5)  (see xrgetint1)
c     O   yr = newbin values (in frame)
c     O   syr = newbin errors (   "  )
c     O   expr = newbin expos (  "  )
c     O   iframe = 1 if frme is ready
c
c
c        (total means summed on all good intvs. and frames so far)
c
c        irtsta(1)= tot. no. of good data intvs.
c        irtsta(2)= no. of good newbins in frame
c        irtsta(3)= no. of good    bins in frame
c        irtsta(4)= no. of good   intvs in frame
c        irtsta(5)= tot. no. of good bins
c        irtsta(6)= tot. no. of good newbins
c        irtsta(9)= current frame no.
c        irtsta(10)= tot. no. of overflowing bins
c        irtsta(11)= tot. no. of bins excluded by time windows
c        irtsta(12)= tot. no. of bins excluded by phase windows
c        irtsta(13)= tot. no. of bins excluded by ints. windows
c        irtsta(14)= tot. no. of newbins excluded by ints. windows
c        irtsta(15)= tot. no. of intvs.  excluded by ints. windows
c        irtsta(16)= tot. no. of bins excluded by exps. windows
c        irtsta(17)= tot. no. of newbins excluded by exps. windows
c        irtsta(18)= tot. no. of intvs.  excluded by exps. windows
c        irtsta(19)= tot. no. of bins excluded because of neg. newbin index
c        irtsta(20)= tot. no. of gap bins
c
c        rrtsta(1)= avg cts/s in frame
c        rrtsta(2)= avg frac. expos in frame
c        rrtsta(3)= avg observed variance in frame
c        rrtsta(4)= avg expected variance in frame
c        rrtsta(5)= avg observed third moment in frame
c        rrtsta(6)= min cts/s in frame
c        rrtsta(7)= max cts/s in frame
c        rrtsta(8)= avg excess variance in frame
c        rrtsta(9)= avg chisquare in frame
c        rrtsta(10)= rms variability
c        rrtsta(11)= error on avg.
c        rrtsta(13)= error on observed variance
c        rrtsta(14)= error on expected variance
c        rrtsta(15)= error on third moment
c        rrtsta(18)= error on excess variance
c        rrtsta(19)= error on chisquare
c        rrtsta(20)= error on rms
c
c        drtsta(1)= center time of 1st newbin in frame or epoch for folding
c        drtsta(2)= time of baryc. of frame
c        drtsta(3)= center time of 1st good newbin in frame
c        drtsta(4)= center time of last good newbin in frame
c        drtsta(5)= period for folding
c
c
      INTEGER*4 iflags(20), m, nmaxa, intsta(20, *), irtsta(20, *),
     &          nanal, nintfm, k, ipf(*), iframe, iend
c     &         ,intsta(20,iflags(10)),irtsta(20,iflags(10))
      REAL*4 rv, rflags(10), rw
c     &         ,rntsta(10,iflags(10)),rrtsta(10,iflags(10))
c     &         ,yi(iflags(15),(iflags(8)-1)/2)
c     &         ,syi(iflags(15),(iflags(8)-1)/2)
c     &         ,yr(iflags(15),(iflags(8)-1)/2)
c     &         ,syr(iflags(15),(iflags(8)-1)/2)
c     &         ,expr(iflags(15))
c     &         ,rntsta(10,*          ),rrtsta(10,*         )
c Rev.1
      REAL*4  rntsta(20, *         ), rrtsta(20, *         )
     &        , yi(nanal, *), syi(nanal, *)
     &       , yr(nanal, *), syr(nanal, *), expr(nanal)
c     &         ,syi(iflags(15),(iflags(8)-1)/2)
c     &         ,yr(iflags(15),(iflags(8)-1)/2)
c     &         ,syr(iflags(15),(iflags(8)-1)/2)
c     &         ,expr(iflags(15))
c      real*8 dntsta(5,iflags(10)),drtsta(5,iflags(10))
      REAL*8  dntsta(20, *         ), drtsta(20, *         )
c
c set initial values
c
c frame not ready
      iframe = 0
c last frame empty
      IF (intsta(4,1).EQ.0 .AND. iend.EQ.2) RETURN
c last intv empty
      IF (intsta(4,1).GT.0 .AND. iend.EQ.2) THEN
         GOTO 900
      ENDIF
c
c means 0 intvs in frame so far
      IF (intsta(4,1).EQ.0) THEN
         DO m = 1, iflags(10)
c no. of good newbins in frame
            irtsta(2, m) = 0
c no. of good    bins in frame
            irtsta(3, m) = 0
c no. of good   intvs in frame
            irtsta(4, m) = 0
c current frame no.
            irtsta(9, m) = irtsta(9, m) + 1
c          do k=1,9
c Rev.1
            DO k = 1, 20
               rrtsta(k, m) = 0.
            ENDDO
c min cts/s in frame
            rrtsta(6, m) = 1.2E34
c max cts/s in frame
            rrtsta(7, m) = -1.2E34
c center of 1st newbin in frame or epoch
            drtsta(1, m) = dntsta(1, m)
c time of baryc. of frame
            drtsta(2, m) = 0.D0
c center time of 1st good newb in frame
            drtsta(3, m) = dntsta(3, m)
c period for folding
            drtsta(5, m) = dntsta(5, m)
c epoch for folding
            drtsta(6, m) = dntsta(6, m)
         ENDDO
c
c         write(*,*)'iflgs(8)',iflags(8)
         DO m = 1, (iflags(8)-1)/2
            DO k = 1, nanal
               yr(k, m) = -1.2E34
               syr(k, m) = -1.2E34
               IF (m.EQ.1) expr(k) = -1.2E34
            ENDDO
         ENDDO
      ENDIF
c
c Accumulate relevant variables
c
c     "Total" variables (integers)
c
      DO m = 1, iflags(10)
c tot. no. of good data intvs.
         irtsta(1, m) = intsta(1, m)
c tot. no. of good bins
         irtsta(5, m) = intsta(5, m)
c tot. no. of good newbins
         irtsta(6, m) = intsta(6, m)
         DO k = 10, 20
c all other total integer variabl.
            irtsta(k, m) = intsta(k, m)
         ENDDO
c min frame c/s
         IF (rntsta(6,m).LT.rrtsta(6,m)) rrtsta(6, m) = rntsta(6, m)
c max frame c/s
         IF (rntsta(7,m).GT.rrtsta(7,m)) rrtsta(7, m) = rntsta(7, m)
c center time of last good newbin in frame
         drtsta(4, m) = dntsta(4, m)
c
c       Cumulative variables
c
c no. of good newbins in frame
         irtsta(2, m) = irtsta(2, m) + intsta(2, m)
c no. of good    bins in frame
         irtsta(3, m) = irtsta(3, m) + intsta(3, m)
c no. of good   intvs in frame
         irtsta(4, m) = irtsta(4, m) + 1
c       also for dummy intv. variab
c no. of good   intvs in frame
         intsta(4, m) = intsta(4, m) + 1
c
c       Variables to be averaged
c
         DO k = 1, 5
c for avg.,frac.expos,var,exp.var,3rd M
c
            rrtsta(k, m) = rrtsta(k, m) + rntsta(k, m)
         ENDDO
c excess var
         rrtsta(8, m) = rrtsta(8, m) + rntsta(8, m)
c chisquare
         rrtsta(9, m) = rrtsta(9, m) + rntsta(9, m)
c  Rev. 1 start
c Rms variab.
         rrtsta(10, m) = rrtsta(10, m) + rntsta(10, m)
c errors from propagation
         IF (nintfm.LT.ipf(2)) THEN
            rrtsta(11, m) = rrtsta(11, m) + rntsta(11, m)*rntsta(11, m)
            rrtsta(13, m) = rrtsta(13, m) + rntsta(13, m)*rntsta(13, m)
            rrtsta(14, m) = rrtsta(14, m) + rntsta(14, m)*rntsta(14, m)
            rrtsta(15, m) = rrtsta(15, m) + rntsta(15, m)*rntsta(15, m)
            rrtsta(18, m) = rrtsta(18, m) + rntsta(18, m)*rntsta(18, m)
            rrtsta(19, m) = rrtsta(19, m) + rntsta(19, m)*rntsta(19, m)
            rrtsta(20, m) = rrtsta(20, m) + rntsta(20, m)*rntsta(20, m)
c errors from scatter
         ELSE
            rrtsta(11, m) = rrtsta(11, m) + rntsta(1, m)*rntsta(1, m)
            rrtsta(13, m) = rrtsta(13, m) + rntsta(3, m)*rntsta(3, m)
            rrtsta(14, m) = rrtsta(14, m) + rntsta(4, m)*rntsta(4, m)
            rrtsta(15, m) = rrtsta(15, m) + rntsta(5, m)*rntsta(5, m)
            rrtsta(18, m) = rrtsta(18, m) + rntsta(8, m)*rntsta(8, m)
            rrtsta(19, m) = rrtsta(19, m) + rntsta(9, m)*rntsta(9, m)
            rrtsta(20, m) = rrtsta(20, m) + rntsta(10, m)*rntsta(10, m)
         ENDIF
c  Rev. 1 stop
c
c baryc. time of frame
         drtsta(2, m) = drtsta(2, m) + dntsta(2, m)
      ENDDO
c
      DO m = 1, (iflags(8)-1)/2
         DO k = 1, nanal
            IF (yi(k,m).GT.-1.1E34) THEN
c reset result frame arrays
               IF (yr(k,m).LT.-1.1E34) THEN
                  yr(k, m) = 0.
                  syr(k, m) = 0.
                  IF (m.EQ.1) expr(k) = 0.
               ENDIF
c values
               yr(k, m) = yr(k, m) + yi(k, m)
c errors
               IF (nintfm.LT.ipf(2)) THEN
c indiv. errors propagated
                  syr(k, m) = syr(k, m) + syi(k, m)*syi(k, m)
               ELSE
c error from scatter
                  syr(k, m) = syr(k, m) + yi(k, m)*yi(k, m)
               ENDIF
c expos.
               IF (m.EQ.1) expr(k) = expr(k) + 1.
c            write(*,*)' m,k,expr(k)',m,k,expr(k) ,yr(k,m)   !!!!!!!!
            ENDIF
         ENDDO
      ENDDO
c
c Average results etc.
c
 900  CONTINUE
      IF (irtsta(4,1).EQ.nintfm .OR. iend.GT.0) THEN
         DO m = 1, iflags(10)
c reset dummy intv. variab.
            intsta(4, m) = 0
            rv = float(irtsta(4,m))
            DO k = 1, 5
c for avg.,frac.expos,var,exp.var,3rd M
               rrtsta(k, m) = rrtsta(k, m)/rv
            ENDDO
c excess var
            rrtsta(8, m) = rrtsta(8, m)/rv
c chisquare
            rrtsta(9, m) = rrtsta(9, m)/rv
c  Rev.1 start
c Rms variab.
            rrtsta(10, m) = rrtsta(10, m)/rv
c errors from propagation
            IF (nintfm.LT.ipf(2)) THEN
               rrtsta(11, m) = sqrt(abs(rrtsta(11,m)))/rv
               rrtsta(13, m) = sqrt(abs(rrtsta(13,m)))/rv
               rrtsta(14, m) = sqrt(abs(rrtsta(14,m)))/rv
               rrtsta(15, m) = sqrt(abs(rrtsta(15,m)))/rv
               rrtsta(18, m) = sqrt(abs(rrtsta(18,m)))/rv
               rrtsta(19, m) = sqrt(abs(rrtsta(19,m)))/rv
               rrtsta(20, m) = sqrt(abs(rrtsta(20,m)))/rv
c errors from scatter
            ELSE
               rw = sqrt(max(rv-1.,1.))
               rrtsta(11, m) = sqrt(abs(rrtsta(11,m)/rv-rrtsta(1,m)**2))
     &                         /rw
               rrtsta(13, m) = sqrt(abs(rrtsta(13,m)/rv-rrtsta(3,m)**2))
     &                         /rw
               rrtsta(14, m) = sqrt(abs(rrtsta(14,m)/rv-rrtsta(4,m)**2))
     &                         /rw
               rrtsta(15, m) = sqrt(abs(rrtsta(15,m)/rv-rrtsta(5,m)**2))
     &                         /rw
               rrtsta(18, m) = sqrt(abs(rrtsta(18,m)/rv-rrtsta(8,m)**2))
     &                         /rw
               rrtsta(19, m) = sqrt(abs(rrtsta(19,m)/rv-rrtsta(9,m)**2))
     &                         /rw
               rrtsta(20, m) = sqrt(abs(rrtsta(20,m)/rv-rrtsta(10,m)**2)
     &                         )/rw
            ENDIF
c  Rev.1 stop
c
c baryc. time of frame
            drtsta(2, m) = drtsta(2, m)/rv
         ENDDO
c
         DO m = 1, (iflags(8)-1)/2
            DO k = 1, nanal
               IF (yr(k,m).GT.-1.1E34) THEN
                  yr(k, m) = yr(k, m)/expr(k)
               ENDIF
            ENDDO
            DO k = 1, nanal
               IF (yr(k,m).GT.-1.1E34) THEN
                  IF (nintfm.LT.ipf(2)) THEN
c error propag.
                     syr(k, m) = sqrt(abs(syr(k,m)))/expr(k)
                  ELSE
c scatter
                     syr(k, m) = sqrt(abs(syr(k,m)/
     &                            expr(k)-yr(k,m)*yr(k,m)))
c sigma of avg
                     syr(k, m) = syr(k, m)/sqrt(max(expr(k)-1.,1.))
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         iframe = 1
      ENDIF
c
c
      RETURN
      END
c
c
c
