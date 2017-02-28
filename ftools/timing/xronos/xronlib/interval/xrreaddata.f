       subroutine xrreaddata(nbindex,nbint,nkount,iendm,dtint,m,iflags,
     $     irec,nfil,iyrsta,dtime,expos,y,sy,nogap,istart,
     $     cfile,cfil,copt,iopt,mopt,dopt,novfl,ipf,dpf,rpf,
     $     nwi,twia,twio,pwi,pwia,pwio,fwia,fwio,ewia,ewio,dtnb,
     $     intsta,dntsta,rntsta,yi,syi,expi,nfilt,nfilma,ichat,
     $     depoch,dper,dpdot,nser,nper,nobins,nexpect,ilda,progtype,
     $     new_loop,loop_done,event,ierr)
      implicit none
c
c Reads data point from input file(s)
c
c I/O nbindex   (i) newbin index
c  I  nbint     (i) newbins/interval
c  -  nkount    (i) no. of phase bins [unused here]
c I/O iendm     (i) array of series flags 
c                    =0 not yet at end
c                    =1 end of good data, 
c                    =2 end of good data and last intv. reject.
c I/O dtint     (d) duration of bin (sec)
c I/O m         (i) current series number
c  I  iflags    (i) int*4 flags for plots, file type, analysis type
c I/O irec      (i) 0 to open the file, n when reading line n, 
c                   -1 when last point was read and file was closed
c I/O nfil      (i) file number
c I/O iyrsta    (i) flag indicates if start time is set (1) or 
c                   not set (0) originally set to year of first infile
c I/O dtime     (d) center time (days) of bin (or event arrival time)
c I/O expos     (r) bin fractional exposure
c  O  y         (r) cts/s in bin (or =1 for arrival time files)
c  O  sy        (r) error on cts/s in bin (or =1 for arrival time files)
c I/O nogap     (i) no. of gap bins
c  O  istart    (l) true if processing start time and not first photon
c  I  cfile     (s) array of infile replies (infile+options)
c  O  cfil      (s) current infile without options
c  O  copt      (s) array of options from infile reply
c  O  iopt      (i) array of flag options (docs in xronos.inc)
c  O  mopt      (i) array of math options (docs in xronos.inc)
c  O  dopt      (d) array of constants for math options 
c  -  novfl     (i) number of overflowed bins [never assigned]
c  I  ipf       (i) integer*4 parameter file options
c  I  dpf       (i) real*8 parameter file options
c  I  rpf       (i) real*4 parameter file options
c  I  nwi       (i) array containing number of windows 
c                   nwi(1) = number of time windows
c                   nwi(2) = number of phase windows
c                   nwi(3*m) = number of intensity windows orig bins
c                   nwi(3*m+1) = number of intensity windows new bins
c                   nwi(3*m+2) = number of intensity windows intervals
c  I  twia      (d) time windows start
c  I  twio      (d) time windows stop
c  I  pwi       (d) epoch and period for phase windows (days)
c  I  pwia      (r) phase windows start
c  I  pwio      (r) phase windows stop
c  I  fwia      (r) intensity (flux) windows start
c  I  fwio      (r) intensity (flux) windows stop
c  I  ewia      (r) exposure windows start
c  I  ewio      (r) exposure windows stop
c  I  dtnb      (d) duration of a "newbin" in secs
c I/O intsta    (i) statistics of this interval (docs in xrinterval.inc)
c I/O rntsta    (r) statistics of this interval (docs in xrinterval.inc)
c I/O dntsta    (d) statistics of this interval (docs in xrinterval.inc)
c  -  yi        (r) cts/s in each new bin of interval          [unused]
c  -  syi       (r) error on cts/s in each new bin of interval [unused]
c  -  expi      (r) exposure fraction (0->1) in each newbin    [unused]
c  I  nfilt     (i) total number of files
c  -  nfilma    (i) max. no. of input files/series             [unused]
c  I  ichat     (i) maximum of terminal chattiness and log chattiness
c  -  depoch    (d) epoch for folding (days)                   [unused]
c  -  dper      (d) period for folding (sec)                   [unused]
c  -  dpdot     (d) period derivative for folding (sec)        [unused]
c  -  nser      (i) number of series to analyze = iflags(10)   [unused]
c  -  nper      (i) number of periods to search (1 if non-fold task) []
c  O  nobins    (i) running total of input # of bins/events 
c  I  nexpect   (i) number of expected bins/events
c  I  ilda      (i) 0 normally, 1 if called by xrlda (list all bins)
c                   [note: there is no xrlda anywhere in xronos code]
c  I  progtype  (s) TIME, FOLD, or FOLDSEARCH analysis type
c I/O new_loop  (l) Flag when true indicates start of new read loop
c I/O loop_done (l) Flag when true indicates completion of read loop
c  O  event     (d) buffer to save dtint - duration of bin (sec)
c  O  ierr      (i) error status (0 = OK)
C
C     GOTO LABELS:
C     100  - top of data reading loop (return here for next data point)
C     200  - end of data screening section (needed to bypass reading and
C            screening code for certain conditions where you've read
C            and screened a point for a test condition but haven't actually
C            counted it yet
C     999  - exit from subroutine (This Way to the Egress :)

      include '../include/io.inc'
      include 'xrinterval.inc'
C     LOCAL
      integer ibinchat,nv, npi, i, idum
      character(80) context
      double precision dtim, dphase, event(8)
      integer nread
c      save nread
      save 
      
      data nread /0/
      parameter (subname = 'xrreaddata:')

      if(new_loop) then
         do i=1,4
            nobins(i)=0
         enddo
         new_loop=.false.
      endif
 100  Continue
C     Top of DATA reading loop

c
c     condition to jump if bin was read and accepted previously 
      IF (nbindex(m).GT.nbint) THEN
C         GO TO END OF BIN SCREENING
         goto 200
      ENDIF
c
      IF (intsta(3,m,1).EQ.0) THEN
         IF (iendm(m).EQ.0) THEN
c         condition to analyse point that was used to determine start of intv.
            IF((y(m).ge.0.).or.(dtint(m).ge.0.d0)) THEN
C     jump to end of bin screening since we already did that for this point
               GOTO 200
            ENDIF
         ELSE
c         condition if iendm=1 (i.e. one of the series is
c           already at the end upon calling the subr. again or point used to
c           decide start of intv. was the end of the file)
c prepare intv.
            IF (m.EQ.iflags(10)) THEN
               loop_done=.true.
               GOTO 999
            ENDIF
            IF (m.LT.iflags(10)) THEN
c prepare next series
               m=m+1
               goto 100
            ENDIF
         ENDIF
      ENDIF
      IF (irec(m).EQ.0) THEN
C     if a new file needs to be opened, parse filename string
C     xrfrdpon will do the actual file opening
         nfil(m) = nfil(m) + 1
         IF (nfil(m).EQ.1) iyrsta(m) = 0
         IF (nfil(m).GT.nfilt(m)) THEN
              write(context,
     &             '('' No more files for Series: '', I1)')m
              call xwrite(context,5)
              do m=1,iflags(10)
                 iendm(m)=1
              enddo
              loop_done=.true.
              goto 999
         ENDIF   
         call xrparseopt(cfile(nfil(m),m),cfil(m),
     &        copt(1,m),iopt(1,m),mopt(1,m),dopt(1,m),ierr)
         if(ierr.ne.0) then
            errm = subname//' '//'Error parsing options from string: '
            call xaerror(errm, 5)
            errm = subname//' '//cfile(nfil(m),m)
            call xaerror(errm, 5)
            goto 999
         endif
      endif
c
c read bin   
c
******************************READ_BIN******************************
c
      CALL xrfrdpon(cfil, 1, iopt, mopt, dopt, m, irec,
     &     iyrsta, dtime, dtint, expos, y, sy, intsta(8,m,1),
     &     nogap, lui, ierr)

      nread=nread+1
c
c save value into array 
      event(m)=dtint(m)

C     call counting clock for impatient users
      if(ichat.ne.0.and.ilda.ne.1)call xclock(nread,nexpect,1)

      if(ierr.ne.0)  then
         write(context,
     $   '(''Error reading point '',I7,'' series '',I1)')  nread,m
         errm = subname//' '//context
         call xaerror(errm, 5)
         errm = subname//' '//'From file: '//cfil(m)
         call xaerror(errm, 5)
      endif

c
c Process individual bins
c

c
c     if really processing the start time and not the first photon.
c
      istart(m) = .false.
      if((y(m).lt.0.).and.(dtint(m).lt.0.d0)) istart(m) = .true.
c
c     if last point in file
c
      IF (irec(m).EQ.-1) THEN
         intsta(10, m, 1) = intsta(10, m, 1) + novfl(m)
         intsta(20, m, 1) = intsta(20, m, 1) + nogap(m)
         irec(m) = 0
         IF ((nfil(m)+1).LE.nfilt(m)) THEN
c read again
            GOTO 100
         ELSE
c flag for end of good data
            iendm(m) = 1
c if first bin of intv.
            IF (intsta(3,m,1).EQ.-1) intsta(3, m, 1) = 0
c prepare intv.
            IF (m.EQ.iflags(10)) THEN
               loop_done=.true.
               GOTO 999
            ENDIF
            IF (m.LT.iflags(10)) THEN
c to calculate dummy start of the intv. of the same series
               IF (intsta(3,m,1).EQ.0) THEN
                  call xrfindstart(iflags,iendm,ipf,nwi,twia,dtnb,
     $                 dtime,dtint,intsta,y,dntsta,m,progtype)
                  goto 100
               ENDIF
c next series (prepare)
               IF (intsta(3,m,1).GT.0) THEN
                  m=m+1
                  goto 100
               ENDIF
            ENDIF
         ENDIF
      ENDIF
c
c     apply file options to this point (only math options so far: see if ...)
c
      IF (mopt(1,m).NE.0) CALL xrapplopt(iopt(1,m), mopt(1,m), dopt(1,m)
     &                                   , dtime(m), dtint(m), expos(m),
     &                                   y(m), sy(m))
c
c     apply windows 
c
      call xrappwins(m,1,.true.,.true.,.true.,.true.,.false.,
     $     dtime(m),dtint(m),expos(m),y(m),sy(m),idum,idum,nbint,
     $     nwi,twia,twio,pwi,pwia,pwio,fwia,fwio,ewia,ewio,
     $     intsta(11,m,1),intsta(12,m,1),intsta(13,m,1),intsta(16,m,1),
     $     idum,iendm)
c end of good data (due to time window)
      IF (iendm(m).EQ.1) THEN
         call ftclos(lui(m),ierr)
c     if first bin of intv.
         IF (intsta(3,m,1).EQ.-1) intsta(3, m, 1) = 0
c     prepare intv.
         IF (m.EQ.iflags(10)) THEN
            loop_done=.true.
            GOTO 999
         ENDIF
         IF (m.LT.iflags(10)) THEN
c     if first bin decrease series no.
            IF (intsta(3,m,1).EQ.0) THEN
               call xrfindstart(iflags,iendm,ipf,nwi,twia,dtnb,
     &              dtime,dtint,intsta,y,dntsta,m,progtype)
               goto 100
            ENDIF
c     next series (prepare)
            IF (intsta(3,m,1).GT.0) THEN
               m=m+1
               goto 100
            ENDIF
         ENDIF
      ENDIF
c     read again
      IF (y(m).LT.-1.1E34) THEN
         goto 100
      ENDIF
c
c For qualified points
c
c statement to jump to if bin was read previously
******************************END OF BIN SCREENING********************
 200  continue
       intsta(3, m, 1) = intsta(3, m, 1) + 1
c
c To determine start of intv.
c NOTE This is to fix if there is a need for a new start from a new file 
c      specifically for events file. In this case the start is taken from GTI.
c      and y(m) is marked as -1 and the expos is set as a gap since is not
c      a real point. But there is nothing to stop to process a start as a 
c      good point this causes a crash when try to use -1 and the und value 
c      in operation. The fix is to reset nbindex value to zero when determinate
c      the interval start.
c
c
      IF (intsta(3,m,1).EQ.0) THEN
         nbindex(m)=0
         call xrfindstart(iflags,iendm,ipf,nwi,twia,dtnb,
     &        dtime,dtint,intsta,y,dntsta,m,progtype)
         goto 100
      ENDIF
c
c  calculate newbin index
c
      dv = (dtime(m)-dntsta(1,m,1))*86400.D0/dtnb
c Trap round-off error.
      IF(dnear(dv - dble(int(dv)+1),-0.5d0,1.d-7) ) 
     &         dv = dble(int(dv)+1) -0.5d0
      nbindex(m) = nint(dv) + 1
c  condition for nbindex underflow
      IF (nbindex(m).LT.1) THEN
         intsta(19, m, 1) = intsta(19, m, 1) + 1
c read again from same series
         GOTO 100
      ENDIF
c  condition for nbindex overflow (meaning current intv. is full)
c  (note that the value of nbindex is saved to use current bin in next intv)
      IF (nbindex(m).GT.nbint) THEN
         intsta(3, m, 1) = intsta(3, m, 1) - 1
c prepare intv.
         IF (m.EQ.iflags(10)) THEN
            loop_done=.true.
            GOTO 999
         ENDIF
c next series (prepare)
         IF (m.LT.iflags(10)) THEN
            m=m+1
            goto 100
         ENDIF
      ENDIF
c
c     type bin values (after options and windows) if necessary
c
      if (ilda.eq.1) then
         ibinchat=1
      else
         ibinchat=20
      endif
         
c     This ichat if statement is for optimization purposes,
C     normally, we would use xwrite by itself
      if(ichat.ge.ibinchat) then
         IF (intsta(3,m,1).EQ.1 .AND. intsta(9,m,1).EQ.1) THEN
            call xwrite(' ',ibinchat)
            IF (iflags(10).gt.1) THEN
c     for programs with > 1 series
               WRITE (context, 1012) m
               call xwrite(context,ibinchat)
 1012          FORMAT (' Series ', I1)
               call xwrite(' ',ibinchat)
               call xwrite(' ',ibinchat)
            ENDIF
            call xwrite(
     $           '    Bin        Day          h  m  s  ms '//
     &           '   Dt(s)    Value(c/s)  Err.(c/s)  Exps.',ibinchat)
               call xwrite(' ',ibinchat)
         ENDIF
         CALL xrdhms(dtime(m), dsec, itim, 1)
       WRITE (context, 1011) intsta(3, m, 1), dtime(m), 
     $        (itim(k), k=2, 5), dtint(m), y(m), sy(m), expos(m)
         call xwrite(context,ibinchat)
 1011    FORMAT (' ', I7, 1X, F17.11, I3, I3, I3, I4, G11.4, G12.5,
     &        G11.4, F7.3)
      endif

 999  continue

      return
      end
