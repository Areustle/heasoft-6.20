      subroutine efseah()
      implicit none

      include '../../include/io.inc'
      parameter (subname = 'efseah:')
      include '../../include/xronos.inc'
      integer i,j,iend,idum,idum2
c
c     the following are "pointers"
      integer yi,syi,expi,dpera,dpdota,intsta,rntsta,dntsta
      integer yr,syr,expr,chival,chierr,xr,sxr,yp,syp,iwork
      integer get_intsta
      external get_intsta

      real xnorm(4)
c
c
      include '../../include/dmcommon.inc'
      include '../../include/xronos_init.inc'
      include '../../include/xrinit.inc'

C*************************************************
C Initialize dynamic memory pointers for UDMGET()
C*************************************************
      rntsta = 0
      dntsta = 0
      intsta = 0
      dpera  = 0
      dpdota = 0
      chival = 0
      chierr = 0
      
C          task common
c      
      call xrversion('efsearch', taskname)
      iend=0
      status = 0
      progname = 'es'
      progtype = 'FOLDSEARCH'
c      
c Get the paramaters
c This is the interactive part
c Note this is common to all tasks in this form.
      CALL xrgetparams(cpf, ipf, rpf, dpf, iflags, rflags,
     &                 progname,progtype, ipow2, nintfm, ilda, 
     &                 nfil, cfile, dtint,dtsta,dtsto, twia, twio,
     &                 pwi, pwia, pwio, fwia, fwio, ewia, ewio, nwi,
     &                 nbdf, dtnb, nbint, nobins, ngtis, depoch, dper,
     &                 dpdot, nbin, dres, nper, cfilo, oftype, cqdp, 
     &                 iqdp, iuser,buser,duser,ruser,suser, csumm,
     &                 ichat,status)
c
      IF (status.NE.0) THEN
         errm = subname//' '//'Error reading parameters'
         call xaerror(errm, 5)
         GOTO 999
      ENDIF
c
c efsearch specific variable settings
c The search produces an arary of chi*2 values one for each time series
c this is stored into a statistical variable rrtsta(9,m,k)
c where the second index is the series number and the last index is 
c the period number on search.   
c 
c 
c no. of result columns/frame
      iflags(8) = ncolpfm(iflags(10))
c no. of indep. variable columns
      iflags(9) = 2
c type of plot desired (chooses default plotting file)
      iqdp(3) = iflags(11)
      DO i=1,maxseries
c start time of first newbin 
         dxsta(i) = dper-dble(nper/2)*dres
c x-axis step is newbin integrat. (s)
         dxstep(i) = dres
      ENDDO
c no. of "analysed" points
c In the search this corresponds to the total number of period search per 
c time series I 
      nanal = nper 
c
c report on the screen no. of analysis points and set iflags(13)
c
      rebin = 0.
      CALL xrtyana(nanal, rebin, iflags)
c
c Set value for memory allocation
c progtype FOLDSEARCH  folding search over a period range
c          FOLD        folding over one period
c          TIMEOBIN    time task but no binning  
c          TIME        time task with binning 
c
      IF(progtype(1:10).NE.'FOLDSEARCH') nper=1
c
      IF (progtype(1:4).EQ.'FOLD') THEN
c     dper/dtnb --> nphas
         nkount=nint(dper/dtnb)
      ELSEIF(progtype.EQ.'TIMEOBIN') THEN
         nkount=nobins(1)
         DO i=2,iflags(10)
            nkount=max(nkount,nobins(i))
         ENDDO
      ELSE
         nkount=nbint
      ENDIF
c      
c

      call xrstdalloc(iflags(10),nper,
     $     rntsta,dntsta,intsta,dpera,dpdota,chival,chierr,1,
     $     status)         


      IF (status.NE.0) THEN
         errm = subname//' '//'Error allocating memory.'
         call xaerror(errm, 5)
         GOTO 999
      ENDIF
c
c Fill the array for periods and periods derivative based on the 
c initial period, dper, # of periods, nper, resolution, dres  
      CALL xrpper(nper,dper,dpdot,dres,progtype,
     &            memd(dpera),memd(dpdota))

C     Currently, we just copy the light curve for the "best" 
C     for each series into the output array.  We don't actually output it yet

C     Allocate results arrays
      call xrtmpalloc(nkount*iflags(10),6,3,1,yr,syr,expr,status)
      if(status.ne.0) goto 999

      
c
c Read the data, correct for exposure, apply all windows, apply all options
c divide the data in interval, fold the data if the case, calculate 
c statistical variables.
c
      DO WHILE(iend.EQ.0) 

C     Allocate accumulation arrays
      call xrtmpalloc(nkount*iflags(10)*nper,6,3,1,yi,syi,expi,status)
      if(status.ne.0) goto 999

c     
c     
         CALL xrgetintv(nfil,cfile,dtnb,nbint,nkount,ilda,ipf,
     &        rpf,dpf,twia,twio,pwi,pwia,pwio,fwia,
     &        fwio,ewia,ewio, nwi,iflags,rflags,
     &        nfilemax,depoch,memd(dpera),memd(dpdota),iflags(10),
     &        nper,ichat,progtype,memr(yi),memr(syi),
     &        memr(expi), memi(intsta),memr(rntsta),
     &        memd(dntsta),nobins,iend,
     &        status)
c     
c     
         IF  (status.NE.0) THEN
            WRITE(context,'(''Error processing interval '',i3)')
     &           get_intsta(memi(intsta),iflags(10),nper,9,1,1)
            errm = subname//' '//context
            CALL xaerror(errm, 5)
            call ftgerr(status, errm)
            errm = subname//' '//errm
            CALL xaerror(errm, 5)
            GOTO 999
         ENDIF
c     
c     
         DO i=1,iflags(10)
            IF (progtype.NE.'timeobin') nobins(i)=nkount
         ENDDO
c     
c     note nanal = nper and nkount is instead the number of bins
C     in the folded lightcurve used in the chi**2 test
c     
         CALL xres(progtype,ipf, iend, memi(intsta), memr(rntsta),
     &        memd(dntsta),iflags,rflags,nanal, nkount, iflags(10),
     &        memr(yi),memr(syi),memr(expi),
     &        memr(yr),memr(syr),memr(expr),
     &        memr(chival), memr(chierr),memd(dpera),memd(dpdota),kmax)
c
C     Free arrays
      call xrtmpalloc(idum2,6,3,2,yi,syi,expi,status)
      if(status.ne.0) goto 999


C     Allocate dummy results x-axis to avoid potential argument problems
      call xrtmpalloc(nkount,7,2,1,xr,sxr,idum,status)
      if(status.ne.0) goto 999
c     
         CALL xrwroutplot(progname, cpf, iqdp, cqdp, cfilo, 
     &        oftype, iflags, rflags, memi(intsta),
     &        memr(rntsta), memd(dntsta), nper, csumm, 
     &        nanal, dtnb, nkount, nbint, nintfm, dxsta, 
     &        dxstep, memr(yr), memr(syr), memr(expr), 
     &        memd(xr), memd(sxr), memr(chival),memr(chierr), 
     &        memd(dpera), kmax, status)

C     Free dummy arrays
      call xrtmpalloc(idum2,7,2,2,xr,sxr,idum,status)
      if(status.ne.0) goto 999

      ENDDO


C     Free arrays
      call xrtmpalloc(idum2,6,3,2,yr,syr,expr,status)
      if(status.ne.0) goto 999
      call xrstdalloc(iflags(10),nper,
     $     rntsta,dntsta,intsta,dpera,dpdota,chival,chierr,2,
     $     status)         
      if(status.ne.0) goto 999

c     
 999  continue
      
      end
