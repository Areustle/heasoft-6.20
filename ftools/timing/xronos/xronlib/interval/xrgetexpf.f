c
      SUBROUTINE xrgetexpf(nfilt, cfile, dtnb, 
     &     nbint,nkount,  ipf, rpf, dpf, twia, twio, pwi,
     &                     pwia, pwio, fwia, fwio, ewia, ewio, nwi, 
     &                     iflags, rflags, nfilma, depoch, dper,dpdot,
     &                     yi, syi, expi, intsta, rntsta, dntsta,
     $                     nper,nser)
c
c la/ls 9/7/91 to read exposures for folding from exposure files when using 
c              PAT files (adapted from xrgetexp.for)
c Rev.1 5/8/91 final do only ion phase bins 
C Rev.2 18/11/91 to use xrrbrdpon.for instaed of xrrbrdpo4,5,6
c                and xrqdprdpon.for instaed of xrqdprdpo4,5,6;
c                also change in dimension (from 3 to 6) of iopt, mopt,
c                dopt, cfil, copt, irec, iyrsta, dtime, dtint, expos, y, sy
c                novfl, nogap, for use in xrrbrdpon and xrqdprdpon
c                and change in numbering of the corresponding indeces
c                to make 4-6 correspond to exposure files
c Rev.3 20/11/91 to avoid problems with exposure when the exposure file(s)
c                is not found or there are problems with it: 8 s duration 
c                exposure bins with expos=1 are used in this case 
c Rev.4 17/4/92  avoid resetting igoodexp=1 at the beginning of a new intv.
c                since this caused problems when more than 1 intv is
c                produced and the xpos file is not found (see Rev.3 of xrgetexp)
c el  14/10/93 Rev.5  Add call to FITS reader.
c                     Call new subroutine isitfits to check whether a
c                     file is a FITS file.  Call FITS reader regardless
c                     of filename extension.  Filenames are illegal only
c                     if the file is not FITS.
c                     Call routine to look up header extension names and
c                     pass corresponding flags to xrfrdpon.
c el  19/11/93 Rev.6  Moved default times and durations for exposure bins
c                     to if block following statement 999.
c la/el 2/12/93 Rev.7 Test for approximate equality in newbin and exposure 
c                     grid indicies.
c
c     I   lut,lul,lcd,lch= terminal, logfile LU, chattiness of C-D and Q/A
c     I   nfilt = total no. of files
c     I   cfile = array of infile replies (infile+options)
c     I   dtnb = duration of a "newbin" in secs
c     I   nbint = no. of newbins per interval
c     I   ipf,rfp,dpf = integer*4, real*4, real*8 parameter file options
c     I   twia,twio,pwi,pwia,pwio,fwia,fwio,ewia,ewio,nwi = windows
c     I   iflags = int*4 flags for plots, file type, analysis type
c     I   rflags = real*4 flags for plots, file type, analysis type
c     I   nfilma = max. no. of input files/series
c     I   depoch = epoch for folding (days) 
c     I   dper = period for folding (sec)
c     I   dpdot = period derivative(s) for folding (s)
c     I   yi = cts/s in each new bin of interval (-1.2e34 or less means gap)
c     I   syi = error on cts/s in each new bin of interval
c     I/O expi = exposure in secs in each newbin
c     I   intsta = statistics of this interval (integer*4) (20)
c     I   rntsta =     "      "   "      "     (real*4)    (10)
c     I   dntsta =     "      "   "      "     (real*8)    (5)
c     I   nper = number of periods
C     I   nser  = number of series (=iflags(10))
c
c  Note that the array of options copt*10(10) is decoded in
c  a) flag options iopt(10) (0= no= df, 1=yes)
c  b) matemathical options mopt(10) and constants dopt(10) to be executed
c     In the same order as the input string
c
c  Interval Statistics Parameter Definition : (n index of series)
c     intsta(1,n) = no. of good data intervals (total)
c     intsta(2,n) = no. of good newbins in intv.
c     intsta(3,n) = no. of good bins in intv.
c     intsta(4,n) = no. of good intvs in frame     (accum. outside this subr)
c     intsta(5,n) = no. of good bins (total)
c     intsta(6,n) = no. of good newbins (total)
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
c     dntsta(1,n) = center time of first newbin in intv. (days)
c     dntsta(2,n) = time of barycenter of intv. (days)
c     dntsta(3,n) = center time of first good newbin in intv. (days)
c     dntsta(4,n) = center time of last  good newbin in intv. (days)
c
      implicit none
      INTEGER*4 iflags(20), nfilma
c      character(37) cfile(nfilma,*)
c      character(37) cfile(nfilma, *)
      LOGICAL dnear,last_blank
      character(80) context
      character(160) cfile(nfilma, *)
      character(160) cfil(8)
      character(10) copt(15, 8)
      CHARACTER cext*5, cpro*1, cque*1
c      integer*4 iv,lut,lch,lul,ierx,ierr,k,nbint,nfilt(iflags(10)),
c     &          intsta(20,iflags(10)),nbindex(8),m,iendm(8),
      INTEGER*4 iv, lut, lcd, lch, lul, ierx, ierr, k, nbint, nfilt(*),
     &     irec(8), novfl(8), nogap(8), nfil(8), iopt(15, 8),nser,
     &     mopt(15, 8), nwi(*), ipf(*), intsta(20,*),
     $     nbindex(8), nkount, j, iblank, isptr, ib, istat,
     &     m, iendm(8), lui(8), iyrsta(8), nbindexmip, nbindexmap,
     &     nbindexma(8), nbindexmi(8), indbin(8), ivmi, ivma, kk,
     &     nphas, nphasmi, igoodexp(8), nch, lenact,ivmip,ivmap,
     &     extns(10,8), ftstat, nbad(8), extn, idum,nper, npi
      REAL*4 rv, pwia(*), pwio(*), fwia(9, *), fwio(9, *), ewia(*),
     &       ewio(*), yi(nkount,nser,nper), syi(nkount,nser,nper),
     $     expi(nkount,nser,nper),
     &       rpf(*), rntsta(20,*), y(8), sy(8), expos(8),
     &       rflags(10)
c     &       rpf(*),rntsta(10,*),y(8),sy(8),expos(8),rflags(10)
c     &       yi(iflags(15),iflags(10)),syi(iflags(15),iflags(10)),
c     &       expi(iflags(15),iflags(10)),
c     &       rpf(*),rntsta(20,iflags(10)),y(8),sy(8),expos(8),rflags(10)
c      real*8 dv,dtnb,twia(*),twio(*),pwi(*),dpf(*),dntsta(5,iflags(10)),
      REAL*8 dv, dtnb, twia(*), twio(*), pwi(*), dpf(*), 
     $     dntsta(20,*),
     &       dopt(15, 8), dtime(8), dtint(8), dvmi, dvma, dvleft,
     &       dvright, dpmi, dpma, dphasmi, dphasma, depoch, 
     $     dpdot(nper), dper(nper), dbtime(8)
      EXTERNAL dnear
      SAVE
c
c set start values
      DATA ierx, ierr, nfil, irec, nbindex /0, 0, 8*0, 8*0, 8*0/
      DATA iendm, nbindexma, nbindexmi/8*0, 8*0, 8*0/
c      DATA lui/54, 55, 56, 57/
c Rev.4 set d/f =1
      DATA igoodexp/8*1/
c
c Work no. of phasebins  (passed in)
c      nphas = nint (dper/dtnb)
      nphas = nkount
c
c Start new interval (start)
c
c     set arrays to default values (in intv.)
c
      DO m = 1, iflags(10)
         indbin(m)=0
c
         do j=1,nper
            DO k = 1, nphas
c     gap values
               expi(k, m, j) = -1.2E34
         ENDDO
      enddo
c
c Rev.3 start: if the beginning of one intv. set d/f bin duration in expos.
c              files to 8 s (as in EXOSAT bckgd files) and work out the 
c              d/f center time of the 1st bin before the 1st nebin of the intv. 
c Rev.6 start
c         dtint(m+4)=8.d0
c         dtime(m+4)=dntsta(1,m)-dtnb/2.d0/86400.d0-
c     &              dtint(m+4)/2.d0/86400.d0
c Rev.6 stop
c Rev.4  to avoid resetting at the beginning of each intv.
c         igoodexp(m+4)=1
c Rev.3 stop
      ENDDO
c
c Start new interval (end)
c
c
c Big loop for data point reading, checking etc. (start)
c
c start from first series
      m = 1
c
c
c
      GOTO 1000
c
c to prepare next series
 1402 CONTINUE
      m = m + 1
c
 1000 CONTINUE
cc      if(indbin(m).lt.10.or.indbin(m).gt.1250)
cc     &   write(*,*) yi(1,1), syi(1,1), expi(1,1),indbin(m),m  !!!!!!!!!!!!
c
c     condition to jump if bin was read and accepted previously
c
      IF (nbindexma(m).GT.nbint) THEN    
         GOTO 1002
      ENDIF
c
      IF (indbin(m).EQ.0) THEN
         IF (iendm(m).EQ.0) THEN
c         condition to read 1st point 
            GOTO 1003
         ELSE
c         condition if iendm=1 (i.e. one of the series is
c           already at the end upon calling the subr. again or point used to
c           decide start of intv. was the end of the file)
c prepare intv.
            IF (m.EQ.iflags(10)) THEN
               GOTO 1100
            ENDIF
            IF (m.LT.iflags(10)) THEN
c prepare next series
               GOTO 1402
            ENDIF
         ENDIF
      ENDIF
c
c     when an infile needs to be open etc.
C     Once again, we replace (for now) xrasks with assignments
c
1003  CONTINUE
      IF (irec(m+4).EQ.0) THEN
c       decode filename and options of infile
         nfil(m) = nfil(m) + 1
 
         IF (nfil(m).EQ.1) iyrsta(m+4) = 0
         cfil(m) = ' '           
         cfil(m+4) = ' '           
         iblank = index(cfile(nfil(m),m),' ')
         cfil(m+4)=cfile(nfil(m),m)(1:iblank-1)
         do k=1,15
            copt(k,m+4)='          '
         enddo
c     break cfile on white space into copt         
C     (there's probably a routine somewhere to do this....)
            k=1
            isptr=1
            last_blank=.true.
            do ib=iblank+1,160
               if(cfile(nfil(m),m)(ib:ib).ne.' ') then
                  if(k.gt.15) then
                     context = 
     $  'WARNING: You have specified too many options for file:'//
     $                    cfile(nfil(m),m)
                     call xwrite(context,5)
                     write(context,'(''Maximum options: '',I3)')10
                     call xwrite(context,5)
                     call xwrite('Extra options ignored.',5)
                     goto 100
                  endif
                  copt(k,m+4)(isptr:isptr)=cfile(nfil(m),m)(ib:ib)
                  last_blank=.false.
                  isptr=isptr+1
               else
                  if(.not.last_blank) then
                     k=k+1
                     isptr=1
                  endif
                  last_blank=.true.
               endif
            enddo
 100        continue
         CALL xrdecopt(copt(1,m+4), iopt(1,m+4),
     &                 mopt(1,m+4), dopt(1,m+4), istat)
         if(istat.ne.0) goto 999
      ENDIF

c Rev.3 : if exposure file for current series has problem go to 999
      IF (igoodexp(m+4).eq.0) GOTO 999
c
c read bin
c
c Rev.5 start
c     Part for FITS files
      CALL xrfrdpon(cfil, 2 , iopt, mopt, dopt, m+4, 
     &     irec, iyrsta, dtime, dtint, expos, y, sy, 
     &     nbad, nogap, lui, ierr)
      if(ierr.ne.0) go to 999
      GOTO 1001

999   CONTINUE
c          if problem is found in exposure file for the first time reset 
c           relevant flag, reset error, increase irec and issue warning message
         IF(igoodexp(m+4).EQ.1) THEN
c                  reset error variables
            ierx=0
            ierr=0
            irec(m+4)=1
            igoodexp(m+4)=0
            nch=lenact(cfil(m+4))
c Rev.6 start
            dtint(m+4)=8.d0
            dtime(m+4)=dntsta(1,m)-dtnb/2.d0/86400.d0-
     &                 dtint(m+4)/2.d0/86400.d0
c Rev.6 stop
            call xwrite(' **** Warning: problem with expos.file ', 5)
            call xwrite(cfil(m+4)(1:nch), 5)
            call xwrite(' will start using 8 s bins of', 5)
            write(context,'(''exposure =1 for series'',i2)') m
            call xwrite(context, 5)
         ENDIF
c
c              now increase dtime by 1 bin (dtint) and set d/f exposure
c              (note that if an expos file bin was successfully read then 
c              dtime and dtint refer to that file)
         dtime(m+4)=dtime(m+4)+dtint(m+4)/86400.d0
         expos(m+4)=1.
c                Rev.3 stop
c
c Process individual bins
c
 1001 CONTINUE
c
c     if last point in file
c
      IF (irec(m+4).EQ.-1) THEN
c reset to open another file      
         irec(m+4) = 0
         IF (nfil(m)+1.LE.nfilt(m)) THEN
c if not the last file open another file 
            GOTO 1000
         ELSE
c flag for end of good data
            iendm(m) = 1
c prepare intv.
            IF (m.EQ.iflags(10)) THEN
               GOTO 1100
            ENDIF
            IF (m.LT.iflags(10)) THEN
c to calculate dummy start of the intv. of the same series
cc               IF (indbin(m).EQ.0) THEN
cc                  GOTO 1000
cc               ENDIF
c next series (prepare)
cc               IF (indbin(m).GT.0) THEN
               GOTO 1402
cc               ENDIF
            ENDIF
         ENDIF
      ENDIF
c
c     apply SS and ST options to this point (other options not applied)
c     (note that to do this a fake dv=-1. is used for dtint to fake an 
c     arrival time file)
c
c      IF (mopt(1,m).NE.0) THEN   !Rev,2
c Rev.2 
      IF (mopt(1,m+4).NE.0) THEN 
         dv=-1.D0
c         CALL xrapplopt(iopt(1,m), mopt(1,m), dopt(1,m)  !Rev.2 
c Rev.2 
         CALL xrapplopt(iopt(1,m+4), mopt(1,m+4), dopt(1,m+4)
     &                 , dtime(m+4),dv,expos(m+4),y(m+4),sy(m+4))
      ENDIF 

c
c  Apply time windows: note that windows will exclude an entire exposure 
c  bin based on its center time. this is OK as long as the duration 
c  of the excluded window is long compared to the exposure bin duration 
c
      IF (nwi(1).GT.0) THEN
         CALL xrappltwi(twia,twio, nwi(1),dtime(m+4),dtint(m+4),
     &                        expos(m+4),y(m+4),sy(m+4),iv,iendm(m))
c         write(*,*)'twia, twio, expos, y, iendm',
c     &              twia, twio, expos(m+4), y(m+4), iendm(m)
c
c end of good data (due to time window)
         IF (iendm(m).EQ.1) THEN     
c close infile
C            CLOSE (lui(m))
c prepare intv.
            IF (m.EQ.iflags(10)) THEN
               GOTO 1100
            ENDIF
            IF (m.LT.iflags(10)) THEN
c if first bin decrease series no.
cc               IF (indbin(m).EQ.0) THEN
cc                  GOTO 1000
cc               ENDIF
c next series (prepare)
cc               IF (indbin(m).GT.0) THEN
               GOTO 1402
cc               ENDIF
            ENDIF
         ENDIF
c         read again 
         IF (y(m+4).LT.-1.1e34) THEN
            GOTO 1000
         ENDIF
      ENDIF
c
c  Apply phase windows: note that windows will exclude an entire exposure 
c  bin based on its center time. this is OK as long as the duration 
c  of the excluded window is long compared to the exposure bin duration 
c
      IF (nwi(2).gt.0) THEN
         CALL xrapplpwi(pwi,pwia,pwio,nwi(2),dtime(m+4),dtint(m+4),
     &                            expos(m+4),y(m+4),sy(m+4),iv)
c         read again 
         IF (y(m+4).LT.-1.1e34) THEN
            GOTO 1000
         ENDIF
      ENDIF
c
c     apply bin exposure windows (and increase intsta(16) if necessary)
c     (note that intensity windows in bins are meaningless in this case)
c
      IF (nwi(12+(m-1)*3).GT.0) THEN
         CALL xrapplewi(ewia, ewio, nwi(12+(m-1)*3),m,1,expos(m+4),
     &                  y(m+4),sy(m+4),intsta(16,m))
c read again
         IF (y(m+4).LT.-1.1E34) THEN
            GOTO 1000
         ENDIF
      ENDIF
c
c For qualified points
c
c statement for jump if bin was read previously
 1002 CONTINUE
      indbin(m) = indbin(m) + 1
c to determine start of intv.
c      IF (indbin(m).EQ.0) THEN
c         GOTO 1000
c      ENDIF
c
c  calculate newbin index (start and stop) to see which part od the exposure
c  bin was actually used in the folding: note that newbins might be 
c  offset with respect to phase bins and therefore the no. of 
c  phasebin affected by the exposure bin might be +/- 1 different from 
c  the no. of affected newbins. 
c
c      dv = (dtime(m+4)-dntsta(1,m))*86400.D0/dtnb
c      nbindex(m) = nint(dv) + 1
      dvmi = ((dtime(m+4)-dntsta(1,m))*86400.D0-dtint(m+4)/2.d0)/
     &       dtnb+1.D0
      nbindexmi(m) = nint(dvmi)
      dvma = ((dtime(m+4)-dntsta(1,m))*86400.D0+dtint(m+4)/2.d0)/
     &       dtnb+1.D0
      nbindexma(m) = nint(dvma) 
c condition for nbindexma underflow
      IF (nbindexma(m).LT.1) THEN
c         intsta(19, m) = intsta(19, m) + 1
c read again from same series
         GOTO 1000
      ENDIF
c  condition for nbindexmi overflow (meaning current intv. is full and no 
c  part of this bin is useful for current intv.)
c  (note that the value of nbindexma is saved to use current bin in next intv)
      IF (nbindexmi(m).GT.nbint) THEN
         indbin(m) = indbin(m) - 1
c prepare intv.
         IF (m.EQ.iflags(10)) THEN
            GOTO 1100
         ENDIF
c next series (prepare)
         IF (m.LT.iflags(10)) THEN
            GOTO 1402
         ENDIF
      ENDIF
c
c  process bin in relevant phasebin(s) 
c
c        min and max newbin index affected by this bin 
      ivmi=max(1,nbindexmi(m))
      ivma=min(nbint,nbindexma(m))
c
c        for beginning of bin: time from epoch, residual time, phase (0:1)  
c      dpmi = (dtime(m+4)-depoch)*86400.D0-dtint(m+4)/2.d0 + dtnb/2.d0
c     half phasebin added below
      dpmi = (dtime(m+4)-depoch)*86400.D0-dtint(m+4)/2.d0 

      do npi = 1,nper
c       write(*,*)'npi',npi,dper(npi),dpdot(npi)
c
c Add dpdot
         IF (dpdot(npi).eq.0.d0) THEN
            dphasmi=dmod(dpmi + dper(npi)/dble(nphas)/2.D0,dper(npi))
c         write(*,*)'dphasmi, dntsta,dpmi',dphasmi,dntsta,dpmi
            dphasmi=dphasmi/dper(npi)
c         write(*,*)'dphasmi,depoch,dtint',dphasmi,depoch,dtint
         ELSE
            dphasmi = dlog(1.d0+dpdot(npi)*(dpmi+
     $           dper(npi)/dble(nphas)/2.D0)/
     $           dper(npi))/dpdot(npi)
            dphasmi = dmod(dphasmi,1.D0)
         ENDIF
         
         if(dphasmi.lt.0.d0) dphasmi = dphasmi + 1.d0
c     
c     for end of bin: time from dpmi, then converted to phase and then 
c     added with the phase of beginning of bin (note that in this way the 
c     no. of cycles after dphasmi is preserved)
         dpma = dtint(m+4)
         dphasma=dpma/(dper(npi)+dpdot(npi)*(dpmi+dtint(m+4)))+dphasmi
c      write(*,*)' dvmi,dvma,dphasmi,dphasma,dpmi '       !!!!!!!
c     &                    ,dvmi,dvma,dphasmi,dphasma     !!!!!!!
c     
c     work out first affected phase bin 
c     nphasmi = nint(dphasmi*dble(nphas)-dtnb/2.d0/dper(npi)) + 1
C     the following appears to be a newer revision
         nphasmi = nint(dphasmi*dble(nphas)) + 1
c     
c     convert phases to phasebin units 
c     dphasmi = dphasmi*dble(nphas) + 1.d0
c     dphasma = dphasma*dble(nphas) + 1.d0
C     the following appears to be a newer revision
         dphasmi = dphasmi*dble(nphas)
         dphasma = dphasma*dble(nphas)
c      write(*,*)'dphasmi',dphasmi,dphasma         
         
c     
c     Work out here ivmip and ivmap that apply to current period
c     
         nbindexmip = nint(((dtime(m+4)-dntsta(1,m))*86400.D0
     &        -dtint(m+4)/2.d0)/(dper(npi)/dble(nphas)) +1.D0)
         nbindexmap = nint(((dtime(m+4)-dntsta(1,m))*86400.D0
     &        +dtint(m+4)/2.d0)/(dper(npi)/dble(nphas)) +1.D0)
         
         ivmip=max(1,nbindexmip)
c Modified 1995 feb 24. The bin counter is still set on time 
c non of phase therefore the min is obtained using the number of bin in 
c time not as the number of cycles
c        ivmap=min(int(dble(nbint)*dtnb/dper(npi))+1,nbindexmap)
         ivmap=min(nbint+1,nbindexmap)         
         
c      write(*,*)'ivmap,ivmip',ivmap,ivmip,nbindexmip,nbindexmap
c     
c     note that do is 1 cycle more just in case the no. of phasebins affected 
c     by the exposure bin is +1 larger than the no. of newbins affected. 
         DO kk =1,ivmap-ivmip+2
c     work out phasebin (note no recycling)
            iv=nphasmi-1+kk
c     work out start and stop of exposure for this phasebin
            dvleft=max(dphasmi,dble(iv)-1.0D0)
            dvright=min(dphasma,dble(iv))
c     work out phase bin between 1 and nphas 
            iv=mod(iv-1,nphas)+1 
c     condition for current phase bin not to be skipped 
            IF (dvleft.LT.dvright) THEN
c     reset value if necessary 
               IF (expi(iv,m,npi).LT.-1.1E34) THEN
                  expi (iv, m, npi) = 0.
               ENDIF
c     work out exposure in secs
c     Rev.7
               IF(.not.dnear(dvright,dvleft,1.d-9))
     &              expi(iv,m,npi)=expi(iv,m,npi)+expos(m+4)*
     $              (dvright-dvleft)*dtnb
            ENDIF 
c      write(*,*)'dvl,rig,iv,per',dvleft,dvright,iv,npi,expi(iv,m,npi) 
         ENDDO
      enddo
c     
c     Condition for nbindexma overflow (meaning current intv. is full and 
c     part of this bin is useful for next intv.)
c     (note that the value of nbindexma is saved to use current bin in next intv)
c     
c      write(*,*)'nbindexma(m),nbint',nbindexma(m),nbint
      IF (nbindexma(m).GT.nbint) THEN    
c prepare intv.
         IF (m.EQ.iflags(10)) THEN
            GOTO 1100
         ENDIF
c next series (prepare)
         IF (m.LT.iflags(10)) THEN
            GOTO 1402
         ENDIF
      ENDIF
c      write(*,*)'nphasmi,nbindexmia', nphasmi,nbindexmi(1),nbindexma(1) !!!!!!!
c  
c read again from same series
      GOTO 1000
c
c interval ready 
c
 1100 CONTINUE
c
c Now prepare newbins 
c
      do npi = 1, nper
         DO m=1,iflags(10)
c     DO k=1,nbint   !Rev.1
c     !Rev.1 : do only on phase index 
            DO k=1,nphas
c     prepare newbins (note gaps identified based on expi)
c     reset exposed empty newbins to 0 
c      write(*,*)'yi(k,m,npi) expi(k,m,npi)',yi(k,m,npi),expi(k,m,npi)
               IF (expi(k,m,npi).GT.-1.1E34) THEN
                  IF (yi(k,m,npi).LE.-1.1E34) THEN 
c     reset exposed empty newbins to 0 
                     yi(k,m,npi)=0.
                     syi(k,m,npi)=0.
                  ENDIF
               ELSE 
c     reset unexposed newbins to gaps  
                  yi(k,m,npi)=-1.2E34
                  syi(k,m,npi)=-1.2E34
               ENDIF        
            ENDDO
         ENDDO
      enddo
      RETURN
c     
c Prepare interval  (end)
c                              Rev.3 start (all lines below commented out)
cc Errors
cc
cc Set full exposure in all newbins of current intv. from series m 
cc if any problem is found with exp files
cc  (Note that no warning messages are produced) 
c 998  CONTINUE
c 999  CONTINUE
cc   reset error variables
c      ierx=0
c      ierr=0
cc
cc
cc      DO k = 1, nbint  !Rev.1 
cc  !Rev.1 : do only on phase index 
c      DO k = 1, nphas
c            expi(k, m) = dtnb
c      ENDDO
cc if last series return
c      IF (m.EQ.iflags(10)) THEN
c         GOTO 1100
c      ENDIF
cc if not last series increase series  
c      GOTO 1402
c
c                                     !Rev.3 stop 
      END
cc
cc
cc
