      subroutine xrgetintv(nfilt, cfile, dtnb, nbint, nkount,
     &     ilda, ipf, rpf, dpf, twia, twio, pwi,
     &     pwia, pwio, fwia, fwio, ewia, ewio, nwi, 
     &     iflags, rflags, nfilma, depoch, dper, dpdot, nser, nper,
     &     ichat, progtype, yi, syi, expi, 
     &     intsta, rntsta, dntsta,  nobins, iend, ierr)
      implicit none
C     Subroutine xrgetintv
C
C     Description:
C     
C     Reads and processes the data to create a single XRONOS interval 
C     data object.  Returns a new interval with each call. (IEND .ne. 0 )
C     indicates no more intervals available (for whatever reason).
C
c     Notes:
c
c     Note that the array of options copt*10(10) is decoded in
c     a) flag options iopt(10) (0= no= df, 1=yes)
c     b) matemathical options mopt(10) and constants dopt(10) to be executed
c     In the same order as the input string
      
C     Arguments:
c
c     I   nfilt = total no. of files
c     I   cfile = array of infile replies (infile+options)
c     I   dtnb = duration of a "newbin" in secs
c     I   nbint = no. of newbins per interval
c     I   nkount = no. of phase bins (or = nbint if non-efolding task)
c     I   ilda = 0 normally, 1 if calling prog is xrlda (to list all bins)
c     I   ipf,rfp,dpf = integer*4, real*4, real*8 parameter file options
c     I   twia,twio,pwi,pwia,pwio,fwia,fwio,ewia,ewio,nwi = windows
c     I   iflags = int*4 flags for plots, file type, analysis type
c     I   rflags = real*4 flags for plots, file type, analysis type
c     I   nfilma = max. no. of input files/series
c     I   depoch = epoch for folding (d)
c     I   dper = period for folding (s)
c     I   dpdot = period derivative for folding (s)
C     I   nser = number of series to analyze = iflags(10)
C     I   nper = number of periods to search (1 if non-fold&search task)
C     I   ichat = the maximum of terminal chattiness and log chattiness
C         passed purely for optimization purposes
C     I   progtype = TIME, FOLD, or FOLDSEARCH analysis type
c     O   yi = cts/s in each new bin of interval (-1.2e34 or less means gap)
c     O   syi = error on cts/s in each new bin of interval
c     O   expi = exposure fraction (0->1) in each newbin
c     O   intsta = statistics of this interval (integer*4) (20)
c     O   rntsta =     "      "   "      "     (real*4)    (10)
c     O   dntsta =     "      "   "      "     (real*8)    (5)
c     O   iend = 1 end of good data, =2 end of good data and last intv. reject.
c

      include 'xrinterval.inc'
      save
C     LOCAL      

      logical iend_flag,repeat_flag
      integer i,idum,nread
      real rdum
      integer npi,nv,msav,ichatheight
      logical been_here_before, exposl
      character(80) context
      double precision ddum,dphase, event(8)

      data been_here_before /.false./

      DATA  nfil, irec, nbindex, iwei/  8*0, 8*0, 8*0, 0/
      DATA iendm/8*0/
      DATA istart /8*.false./



      if (ierr.ne.0) return

      if(.not.been_here_before) then
         nexpect=0
         do m=1,iflags(10)
            nexpect=nexpect+nobins(m)
            do npi = 1,nper
               do k = 1,20
                  intsta(k,m,npi)=0
                  rntsta(k,m,npi)=0.
               enddo
               do k=1,20
                  dntsta(k,m,npi)=0.
               enddo
            enddo
         enddo
         been_here_before = .true.
      endif
      

      iend = 0



C     Start reading new interval here
 100  continue
c
c     set arrays to default values (in intv.)
c
      msav=0
      DO npi = 1, nper
         DO m = 1, iflags(10)
            intsta(2, m, npi) = 0
            intsta(3, m, npi) = -1
            intsta(8, m, npi) = 0
c current intv. no.
            intsta(9, m, npi) = intsta(9, m, npi) + 1

            DO k = 1, 20
               rntsta(k, m, npi) = 0.
            ENDDO
c dummy value for min cts/s in newbin
            rntsta(6, m, npi) = 1.E34
c dummy value for max cts/s in newbin
            rntsta(7, m, npi) = -1.E34
c
c dummy value for start or epoch
            DO k = 1, 20
               dntsta(k, m, npi) = 0.D0
            ENDDO

            DO k = 1, nkount
c gap values
               yi(k, m, npi) = -1.2E34
               syi(k, m, npi) = -1.2E34
               expi(k, m, npi) = -1.2E34
            ENDDO
         ENDDO
      enddo
c
c Return condition if last
c
c if no good data in last possible intv.
      IF (iend.EQ.1) THEN
c means last possible intv. rejected
         iend = 2
         goto 999
      ENDIF

c start from first series
      m=1
C     find first series start time
C note that we're using only the upper nkount x nser section of
C intsta and dntsta within this routine
      call xrfindstart(iflags,0,ipf,nwi,twia,dtnb,
     $     dtime,dtint,intsta,y,dntsta,m,progtype)
      
      new_loop=.true.
      loop_done=.false.
C********************TOP INTERVAL DATA READ LOOP**********************
 200   CONTINUE
       
       if(m.gt.msav) then
          write(context,
     $       '(''Processing Interval: '',i5,'' Series: '',I1)') 
     $         intsta(9,m,1),m
          call xwrite(context,15)
          msav=m
       endif

C     read a data point 
      call xrreaddata(nbindex,nbint,nkount,iendm,dtint,m,iflags,
     $     irec,nfil,iyrsta,dtime,expos,y,sy,nogap,istart,
     $     cfile,cfil,copt,iopt,mopt,dopt,novfl,ipf,dpf,rpf,
     $     nwi,twia,twio,pwi,pwia,pwio,fwia,fwio,ewia,ewio,dtnb,
     $     intsta,dntsta,rntsta,yi,syi,expi,nfilt,nfilma,ichat,
     $     depoch,dper,dpdot,nser,nper,nobins,nexpect,ilda,progtype,
     $     new_loop,loop_done,event,ierr)
      write(context,'(''read a point from xrreaddata'')')
      call xwrite(context, 25)
      if(ierr.ne.0.) goto 999
      if(loop_done) goto 900

      if(progtype(1:4).eq.'FOLD') then
c     prepare values of dntsta based on times of qualified bins
c     (note difference with TIME tasks, where qualified newbins are considered)
c     
c     for baryc. time
         dntsta(2, m, 1) = dntsta(2, m, 1) + dtime(m)
c         write(*,*)'dntsta(2), dtime',dntsta(2, m, 1), dtime(m)
c         write(*,*)'intsta(3),intsta(2)',intsta(3,m,1),intsta(2,m,1)
c     for time of last good bin
         dntsta(4, m, 1) = dtime(m)
      endif


      do npi=1,nper
         if(progtype(1:4).eq.'FOLD') then
            call xrfold(nkount,dtime(m),depoch,dper(npi),dpdot(npi),
     $           dphase)
c     index for folded lc
            nv = int(dphase*dble(nkount)) + 1
c            write(*,*)'nkount, dphase, nv',nkount, dphase, nv
         elseif(progtype.eq.'TIMEOBIN') then
            nobins(m)=nobins(m)+1
            nv=nobins(m)
C     set time = original bin time
c     Not currently done for memory space reasons
c            dtimei(nv,m,npi)=dtime(m)
         else
            nv=nbindex(m)
         endif

c
c  process nbindex bin
c
c  sums are weighted, and expos of every bin considered exactly
c
c to reset empty bin
         IF (yi(nv,m,npi).LT.-1.1E34) THEN
            yi(nv, m, npi) = 0.
            syi(nv, m, npi) = 0.
            expi(nv, m, npi) = 0.
         ENDIF
c     to convert back to actual cts
         rv = dtint(m)*expos(m)
c     is already cts in arriv. time files
         IF (rv.LT.0.) rv = 1.
c     !note temporary conversion to cts (below)
         yi(nv, m, npi) = yi(nv, m, npi) + y(m)*rv
         syi(nv, m, npi) = syi(nv, m, npi) + sy(m)*sy(m)*rv*rv
c     
c     this is in secs (or no. of events for arr. time files)
         expi(nv, m, npi) = expi(nv, m, npi) + rv
c
c set the exposure to one when the flatexpo is set to yes
c
c         write(*,*)'calculate exposure event flags', event(m),iflags(19) 
         if(event(m).LT.0.D0.and.iflags(19).eq.1) expi(nv, m, npi) = 1
c     read again same series
      enddo
      GOTO 200
      
 900  continue


C     calculate time array
C     currently disabled for memory space reasons
c      if(progtype.ne.'TIMEOBIN') then
c         do npi=1,nper
c            do m=1,iflags(10)
c               do i=1,nkount
c                  dtimei(i,m,npi)=dntsta(1,m,1) +
c     $                 dble(i-1)*dtnb/86400.d0
c               enddo
c            enddo
c         enddo
c      endif
c
c     set iend flag =1 if data from all series are ended
c
      
      iend_flag=(iendm(1).eq.1)
      do m=2,iflags(10)
         iend_flag = (iend_flag.and.(iendm(m).eq.1))
      enddo
      if(iend_flag) iend=1
c
c if just listing data
      IF (ilda.GE.1) RETURN
c
c get exposure and  prepare newbins for arrival time infiles(?)
      exposl=.false.
c      write(*,*)'out iflags(19),exposl',iflags(19),exposl
      DO m=1,iflags(10)
         IF (event(m).LT.0.D0.and.iflags(19).eq.0) exposl=.true. 
c         write(*,*)'in iflags(19),exposl',iflags(19),exposl
      ENDDO   

      IF (exposl) THEN
         call xwrite(' ',15)
         call xwrite('Calculating Exposure',15)
         if(progtype(1:4).eq.'FOLD') then
         CALL xrgetexpf (nfilt,cfile,dtnb,nbint,nkount,
     &           ipf,rpf,dpf,twia,twio,pwi,pwia,pwio,fwia,fwio,
     &           ewia,ewio,nwi,iflags,rflags,nfilma,depoch,dper,
     &           dpdot,yi,syi,expi,
     &           intsta(1,1,1),rntsta(1,1,1),dntsta(1,1,1),nper,
     &           iflags(10))
         else
         CALL xrgetexp (nfilt,cfile,dtnb,nbint,
     &           ipf,rpf,dpf,twia,twio,pwi,
     &           pwia, pwio, fwia, fwio, ewia, ewio,nwi,
     &           iflags, rflags, nfilma, yi(1,1,1), syi(1,1,1),
     &           expi(1,1,1),intsta(1,1,1), rntsta(1,1,1),
     &           dntsta(1,1,1))
         endif
         call xwrite('Exposure Calculated.',15)
       ENDIF
c
      do npi=1,nper
C     copy stat values from first period to current period
C
         do m=1,iflags(10)
            do k=1,20
               intsta(k,m,npi)=intsta(k,m,1)
               rntsta(k,m,npi)=rntsta(k,m,1)
            enddo
            do k=1,20
               dntsta(k,m,npi)=dntsta(k,m,1)
            enddo
         enddo
      enddo


      do npi=1,nper

C     prepare interval
      call xwrite (' Prepare interval',15)
      call xrprepint(iflags,dtint,dtnb,nbint,nkount,yi(1,1,npi),
     $     expi(1,1,npi),syi(1,1,npi),iopt,mopt,dopt,fwia,fwio,
     $     ewia,ewio,nwi,intsta(1,1,npi),rpf)
      call xwrite (' Interval done',15)

C     force simultaneity if requested
      if(iflags(16).eq.1 .and. iflags(10).gt.1 
     $     .and. progtype(1:4).eq.'TIME') 
     $     call xrforcesim(nbint,iflags,yi(1,1,1),syi(1,1,1),
     $     expi(1,1,1))

C     trend removal if desired
      if(iflags(3).ne.0 .and. progtype(1:4).eq.'TIME')  then
         do m=1,iflags(10)
            call xrtrere(yi(1,m,1), syi(1,m,1), 1, nbint, iflags(3),
     &           iflags(4), iwei)
         enddo
      endif
C     calculate interval statistics 
      call xwrite('Calculate Statistical variables',15)
      call xrintstat(nbint,nkount,dntsta(1,1,npi),dtnb,iflags,
     $     intsta(1,1,npi),rntsta(1,1,npi),expi(1,1,npi),yi(1,1,npi),
     $     syi(1,1,npi),progtype)

      call xwrite(' Statistical done',15)

c     Reject the interval if all bins are zero in an event list.
c     To compensate for roundoff error, multiply the sum of all bins by
c     dtnb to get a lower bound on the total number of counts and 
c     reject if less than 1.
c
c Change the do loop because in the mix case was not properly 
c calculating the exposure 
c 
      do m=1,iflags(10)
c      write(*,*)'rntsta(1,m,npi),intsta(2,m,npi),dtnb,nbint,nkout',
c     &           rntsta(1,m,npi),intsta(2,m,npi),dtnb,nbint,nkount
         IF ((rntsta(1,m,npi)*real(intsta(2,m,npi))*dtnb*
c     &        real(nbint)/real(nkount).LT.0.99)) THEN
     &        real(nbint)/real(nkount).LT.0.99)
     &        .AND.(event(m).LT.0.d0)) THEN
              intsta(2, m, npi) = 0
           write(context,
     &         '(''Interval '',I1,'' rejected; all bins zero'')') 
     &           intsta(9,m,npi)
            call xwrite(context,10)
            call xwrite(' ',10)
            GOTO 100
         ELSEIF(rntsta(1,m,npi).EQ.0.0) THEN
           write(context,
     &         '(''Interval '',I1,'' rejected; all bins zero'')')
     &           intsta(9,m,npi)
            call xwrite(context,10)
            call xwrite(' ',10)
            GOTO 100
         ENDIF
      enddo

      if (progtype.eq.'FOLDSEARCH') then
         ichatheight=10
      else
         ichatheight=0
      endif
C     print out results
      do m=1,iflags(10)
         call xwrite(' Print the results ',15)
         call xrtyint(m,nkount,dtnb,yi(1,m,npi),syi(1,m,npi),
     $        expi(1,m,npi),
     $        intsta(1,m,npi),rntsta(1,m,npi),dntsta(1,m,npi),
     $        dper(npi),dpdot(npi),progtype,ichatheight)
      enddo
      
      if(progtype(1:4).eq.'FOLD') then
         do m=1,iflags(10)
c     store epoch in dntsta(6)
            dntsta(6, m, npi) = depoch
c     store period in dntsta(5)
            dntsta(5, m, npi) = dper(npi)
         enddo
      endif



c  Apply intensity windows in intv (and increase intsta(15) if necessary)
c
      DO m = 1, iflags(10)

         call xrappwins(m,3,.false.,.false.,.true.,.true.,.false.,
     $        ddum,dtint(m),rntsta(2,m,npi),rntsta(1,m,npi),rdum,idum,
     $        idum,nbint,
     $        nwi,twia,twio,pwi,pwia,pwio,fwia,fwio,ewia,ewio,
     $        idum,idum,intsta(15,m,1),intsta(18,m,1),idum,iendm)
         
         
c     if rejected
         IF (rntsta(1,m,npi).LT.-1.1E34) THEN
c     reset no. of good newbins to 0
            intsta(2, m,npi) = 0
            write(context,1300) m
            call xwrite(context,5)
 1300       FORMAT (1X, 9X,
     &           '  Interval rejected because of window(s)',
     &           '  in series ', I1, ' !')
         ENDIF
      ENDDO
c

      repeat_flag=.false.
      do m=1,iflags(10)
         repeat_flag = repeat_flag.or.(intsta(2,m,npi).eq.0)
      enddo
C     if any of the series are completely vacant, try again
      if(repeat_flag) goto 100

     

      if (iflags(17).gt.0 .and. progtype(1:4).eq.'TIME') 
     $     call xrrnmean(iflags,nbint,yi(1,1,1),syi(1,1,1))
      if (ipf(3).eq.1) call xrexpprof(iflags,nkount,yi(1,1,1),
     $     syi(1,1,1))

      do m=1,iflags(10)
         intsta(1, m, npi) = intsta(1, m, npi) + 1
         intsta(5, m, npi) = intsta(5, m, npi) + intsta(3, m, npi)
         intsta(6, m, npi) = intsta(6, m, npi) + intsta(2, m, npi)
      enddo
      enddo

 999  continue
      return
      end
