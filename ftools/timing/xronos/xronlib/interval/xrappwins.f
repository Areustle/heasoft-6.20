      subroutine xrappwins(m,bintype,ltime,lphase,lflux,lexp,lspec,
     $     dtime,dtint,expos,y,sy,ibf,iaf,nbint,
     $     nwi,twia,twio,pwi,pwia,pwio,fwia,fwio,ewia,ewio,
     $     ntexc,npexc,nfexc,neexc,nsexc,iendm)

C     DESCRIPTION:
C     Applies windows to data.  Increments counters and puts null vals 
C     (-1.22e34) in newbin arrays if data is rejected.
C     
C     
C     ARGUMENTS:
C     m         I     series number
C     bintype   I     type of windows being applied (1-orig. bins, 
C                     2-new bins, 3-intervals)
C     lspec,ltime,lphase,lflux,lexp
C               I     do apply the specified type of window?
C     dtime     I     time of bin center (days)
C     dtint     I     duration of bin (sec)
c     expos     I/0   exposure fraction in bin (0->1) flagged -1.22e34 if rej.
C     y         I/O   cts/s in bin  flagged -1.22e34 if rej.
C     sy        I/O   error on cts/s in bin flagged -1.22e34 if rej.
C     ibf,iaf   I     (sp. win. par.) no. of bins before/after (bounds checked)
C                     (ibf is .le. 0)
C     nbint     I     newbins/interval
C     nwi       I     number of windows of each type array
C     twia,twio I     time window start,stop
c     pwi       I     epoch and period for phase windows (days)
c     pwia,pwio I     phase window start, stop
C     fwia,fwio I     intensity window start, stop
C     ewia,ewio I     exposure window start stop
C     ntexc,npexc,nfexc,neexc,nsexc
C               I     counters for each window type.  Incr. if window applied
C     iend      I     set = 1 if time is beyond stop of last window
      implicit none
      integer maxbintype,maxseries
      parameter( maxbintype = 3, maxseries = 4)
      integer m,nser,bintype,nbint,nwi(*),ntexc,npexc,nfexc,neexc,
     $     nsexc,iendm(*),ibf,iaf
      logical lspec,ltime,lphase,lflux,lexp
      double precision dtime,dtint,dtnb,twia(*),twio(*),pwi(*)
      real expos,y,sy,pwia(*),pwio(*),
     $     fwia(maxbintype*maxseries,*), fwio(maxbintype*maxseries,*),
     $     ewia(*),ewio(*)
c     LOCAL variables
      integer iv

      if(ltime.and.nwi(1).gt.0) then
         call xrappltwi(twia,twio,nwi(1),dtime,dtint,expos,
     $        y,sy, ntexc, iendm(m))
      endif
      if(lphase.and.nwi(2).gt.0) then
         CALL xrapplpwi(pwi, pwia, pwio, nwi(2), dtime, dtint,
     &        expos, y, sy, npexc)
      endif
      iv= 2+bintype+(m-1)*maxbintype
c   (do not apply bin intensity windows to arr. time files) 
      IF (lflux.and.
     $     nwi(iv).GT.0.and.dtint.gt.0.D0) THEN
         CALL xrapplfwi(fwia, fwio, nwi(iv), m, bintype, expos,
     &                  y, sy, nfexc)
      endif
c       Apply "special windows" around gaps produced by intensity windows
c       (i.e. exclude newbins less than rpf(1) (rpf(2)) secs before (after) a
c        newbin excluded by intensity windows) (useful for bursts etc.)
      if (lspec.and.
     $     nwi(iv).gt.0.and.
     $     (ibf.lT.0. .OR. iaf.GT.0.)) then
         CALL xrapplspwi(ibf, iaf, nbint, expos, y, sy, nsexc)
      endif

      iv = 2+maxbintype*maxseries+bintype+(m-1)*maxbintype
c   (do not apply bin expos. windows to arr. time files: this is done
c           in xrgetexp for the corresponding exposure file) 
      IF (lexp.and.
     $     nwi(iv).GT.0.and.
     $     (bintype.ne.1.or.dtint.gt.0.D0)) THEN
         CALL xrapplewi(ewia, ewio, nwi(iv), m, bintype, 
     $        expos, y, sy, neexc)
      endif
      return
      end
      





