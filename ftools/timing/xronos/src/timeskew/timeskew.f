      subroutine timesw()
      implicit none

      include '../../include/io.inc'
      parameter (subname = 'timesw:')
      include '../../include/xronos.inc'
      integer i,j,iend,nk,idum,idum2
C     the following are "pointers"
      integer yi,syi,expi,dpera,dpdota,intsta,rntsta,dntsta
      integer yr,syr,expr,chival,chierr,xr,sxr,yp,syp,iwork
      integer get_intsta
      external get_intsta

      real xnorm(4)

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
      
      call xrversion('timeskew', taskname)
      iend=0
      status = 0
      progname = 'ts'
      progtype = 'TIME'
      
      
      call xrgetparams(cpf, ipf, rpf, dpf, iflags, rflags,
     $     progname,progtype, ipow2, nintfm, ilda, nfil, cfile,
     $     dtint,dtsta,dtsto, twia, twio, pwi, pwia, pwio, fwia, 
     $     fwio, ewia, ewio, nwi, nbdf, dtnb, nbint, nobins, ngtis,
     $     depoch, dper, dpdot, nbin, dres, nper, cfilo, oftype, 
     $     cqdp, iqdp, iuser,buser,duser,ruser,suser,
     $     csumm,ichat,status)

      if (status.ne.0) then
         errm = subname//' '//'Error reading parameters'
         call xaerror(errm, 5)
         goto 999
      endif

C     ts specific variable settings

c no. of result columns/frame
      iflags(8) = 3
c no. of indep. variable columns
      iflags(9) = 2
c type of plot desired (chooses default plotting file)
      iqdp(3) = iflags(11)
      do i=1,maxseries
c start time of first newbin 
         dxsta(i) = 0.d0
c x-axis step is newbin integrat. (s)
         dxstep(i) = dtnb
      enddo
c no. of "analysed" points
      nanal = nbint
c
      rebin=rflags(1)
c
c type No. of analysis points and set iflags(13)
c
      CALL xrtyana(nanal, rebin, iflags)
c
      
      if(progtype(1:10).ne.'FOLDSEARCH') nper=1

      if (progtype(1:4).eq.'FOLD') then
C     dper/dtnb --> nphas
         nkount=nint(dper/dtnb)
      elseif(progtype.eq.'TIMEOBIN') then
         nkount=nobins(1)
         do i=2,iflags(10)
            nkount=max(nkount,nobins(i))
         enddo
      else
         nkount=nbint
      endif
      
C     work arrays yp and syp are nanal for this tool
      iwork=nanal



      call xrstdalloc(iflags(10),nper,
     $     rntsta,dntsta,intsta,dpera,dpdota,chival,chierr,1,
     $     status)         

      if (status.ne.0) then
         errm = subname//' '//'Error allocating memory.'
         call xaerror(errm, 5)
         goto 999
      endif



      call xrpper(nper,dper,dpdot,dres,progtype,
     $     memd(dpera),memd(dpdota))
      


C     Allocate results arrays
      call xrtmpalloc(nanal,6,3,1,yr,syr,expr,status)
      if(status.ne.0) goto 999

      
      do while(iend.eq.0) 

C     Allocate accumulation arrays
      call xrtmpalloc(nkount,6,3,1,yi,syi,expi,status)
      if(status.ne.0) goto 999
C     Allocate intermediate arrays
C     work arrays yp and syp are nanal for this tool
      call xrtmpalloc(nanal,6,2,1,yp,syp,idum,status)
      if(status.ne.0) goto 999

      nser=iflags(10)

      call xrgetintv(nfil,cfile,dtnb,nbint,nkount,ilda,ipf,
     $        rpf,dpf,twia,twio,pwi,pwia,pwio,fwia,fwio,ewia,ewio,
     $        nwi,iflags,rflags,nfilemax,depoch,memd(dpera),
     $        memd(dpdota),nser,nper,ichat,progtype,memr(yi),
     $        memr(syi),memr(expi), memi(intsta),memr(rntsta),
     $        memd(dntsta),nobins,iend,status)
      
      if (status.ne.0) then
         write(context,'(''Error processing interval '',i3)')
     $        get_intsta(memi(intsta),iflags(10),nper,9,1,1)
         errm = subname//' '//context
         call xaerror(errm, 5)
         call ftgerr(status, errm)
         errm = subname//' '//errm
         call xaerror(errm, 5)
         goto 999
      endif

      do i=1,iflags(10)
         if (progtype.ne.'TIMEOBIN') nobins(i)=nkount
      enddo

      call xrtss(ipf, iend, memi(intsta), memr(rntsta), memd(dntsta),
     $     iflags,rflags,nanal,nintfm,nbint,
     $     memr(yi),memr(syi),memr(expi),memr(yp),memr(syp),
     $     memr(yr),memr(syr),memr(expr),irtsta,rrtsta,drtsta,
     $     iframe)

C     Free arrays
      call xrtmpalloc(nkount,6,3,2,yi,syi,expi,status)
      if(status.ne.0) goto 999
      call xrtmpalloc(nanal,6,2,2,yp,syp,idum,status)
      if(status.ne.0) goto 999

      if(iframe.eq.1) then
C     Allocate results x-axis
         call xrtmpalloc(nanal,7,2,1,xr,sxr,idum,status)
         if(status.ne.0) goto 999
         call xrwroutplot(progname, cpf, iqdp, cqdp, cfilo, oftype, 
     $        iflags, rflags, irtsta, rrtsta, drtsta, 
     $        nper, csumm, nanal,  dtnb, nkount, nbint, nintfm, dxsta, 
     $        dxstep, memr(yr), memr(syr), memr(expr), memd(xr), 
     $        memd(sxr), memr(chival), memr(chierr),memd(dpera),
     $        kmax, status)
C     Free arrays
         call xrtmpalloc(nanal,7,2,2,xr,sxr,idum,status)
         if(status.ne.0) goto 999
      endif
      enddo
      
C     Free arrays
      call xrtmpalloc(nanal,6,3,2,yr,syr,expr,status)
      if(status.ne.0) goto 999

      call xrstdalloc(iflags(10),nper,
     $     rntsta,dntsta,intsta,dpera,dpdota,chival,chierr,2,
     $     status)         
      if(status.ne.0) goto 999

 999  continue

      end




      










