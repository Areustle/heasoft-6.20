c
c  Interval Statistics Parameter Definition : (n index of series)
c     intsta(1,n) = no. of good data intervals (total)
c     intsta(2,n) = no. of good newbins in intv.
c     intsta(3,n) = no. of good bins in intv.
c     intsta(4,n) = no. of good intvs in frame     (accum. outside this subr)
c     intsta(5,n) = no. of good bins (total)
c     intsta(6,n) = no. of good newbins (total)
c     intsta(7,n) = no. of photons in interval (for exclusion if = 0)
c     intsta(8,n) = no. of photons excluded by good time intervals (total)
c     intsta(9,n) = no. of interval which is being accumulated
c     intsta(10,n) = no. of overflowing bins (total)
c     intsta(11,n) = no. of bins excluded by time windows (total)
c     intsta(12,n) = no. of bins excluded by phase windows (total)
c     intsta(13,n) = no. of bins excluded by intensity windows (total)
c     intsta(14,n) = no. of newbins excluded by intensity windows (total)
c     intsta(15,n) = no. of intvs excluded by intensity windows (total)
c     intsta(16,n) = no. of bins excluded by exposure windows (total)
c     intsta(17,n) = no. of newbins excluded by exposure windows (total)
c     intsta(18,n) = no. of intvs excluded by exposure windows (total)
c     intsta(19,n) = no. of bins excl. because of negative newbin index (total)
c     intsta(20,n) = no. of gap bins (total)
c
c
c     rntsta(1,n) = avg. cts/s in interval (not weighted)
c     rntsta(2,n) = fractional exposure in intv. (=intsta(2)/nbint)
c     rntsta(3,n) = observed variance (in newbins)
c     rntsta(4,n) = expected variance (in newbins based on errors)
c     rntsta(5,n) = observed third moment (in newbins based on errors)
c     rntsta(6,n) = min cts/s in interval
c     rntsta(7,n) = max cts/s in interval
c     rntsta(8,n) = excess variance (in newbins) (rntsta(3)-rntsta(4))
c     rntsta(9,n) = chisquare for (unweighted) constant (in newbins)
c     rntsta(10,n)= rms variability
c     rntsta(11,n)= theor. error on avg. cts/s in interval
c     rntsta(13,n)= theor. error on obs. variance in newbins
c     rntsta(14,n)= theor. error on exp. variance in newbins
c     rntsta(15,n)= theor. error on observed third moment in newbins (not used)
c     rntsta(18,n)= theor. error on excess variance (not used)
c     rntsta(19,n)= theor. error on chisquare
c     rntsta(20,n)= theor. error on rms variability
c
c
c     dntsta(1,n) = center time of first newbin in intv. 
c     dntsta(2,n) = time of barycenter of intv. (days)
c     dntsta(3,n) = center time of first good newbin in intv. (days)
c     dntsta(4,n) = center time of last  good newbin in intv. (days)
c     dntsta(5,n) = period used for folding (s)
c     dntsta(6,n) = epoch for folding (days)  	
c     dntsta(7,n) = start time of first interval (for tunit=4 in lcurve)
c
c


      INTEGER iflags(20), nfilma, nexpect
      integer iblank, isptr, ib
      character(160) cfile(nfilma, *)
      character(160) cfil(8)
      character(10) copt(15, 8)
      CHARACTER cext*15, cpro*1, cque*1
      character(10) progtype
      LOGICAL dnear, istart(8), last_blank, fold, new_loop, loop_done
      INTEGER iv, ierr, k, nbint, nkount,
     $     nfilt(*),irec(8), novfl(8), nogap(8), nfil(8), nper, nser,
     $     iopt(15, 8),mopt(15, 8), nwi(*), ipf(*), nobins(4),
     $     intsta(20,nser,nper),nbindex(8),
     &     m, iendm(8), iend, itim(5), ilda, lui(8), itre, itremo,
     &     iwei, iyrsta(8), mm, lup, ftstat, ichat
      REAL rv, pwia(*), pwio(*), fwia(12, *), fwio(12, *), ewia(*),
     &       ewio(*), yi(nkount,nser,nper), 
     $       syi(nkount,nser,nper), expi(nkount,nser,nper),
     &       xrrunmean, rpf(*), rntsta(20,nser,nper),
     $       y(8), sy(8), expos(8),
     &       rflags(10)
      double precision dv, dtnb, twia(*), twio(*), pwi(*), dpf(*),
     $     dntsta(20,nser,nper),dopt(15, 8), dtime(8), dtint(8), 
     $     dsec, depoch, dper(nper), dpdot(nper)
      EXTERNAL dnear










