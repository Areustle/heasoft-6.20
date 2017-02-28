      subroutine listda()
      implicit none

      include '../../include/io.inc'
      parameter (subname = 'listda:')
      include '../../include/xronos.inc'
      integer i,j,iend,idum,idum2
C     the following are "pointers"
      integer yi,syi,expi,dpera,dpdota,intsta,rntsta,dntsta
      integer yr,syr,expr,chival,chierr,xr,sxr,yp,syp
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
      
      call xrversion('listdata', taskname)
      iend=0
      status = 0
      progname = 'ld'
      progtype = 'TIME'
      do i=1,4
         xnorm(i) = 1
      enddo
      
      
      call xrgetparams(cpf, ipf, rpf, dpf, iflags, rflags,
     $     progname,progtype, ipow2, nintfm, ilda, nfil, cfile,
     $     dtint,dtsta,dtsto, twia, twio, pwi, pwia, pwio, fwia, 
     $     fwio, ewia, ewio, nwi, nbdf, dtnb, nbint, nobins, ngtis,
     $     depoch, dper, dpdot, nbin, dres, nper, cfilo, oftype, 
     $     cqdp, iqdp, iuser,buser,duser,ruser,suser,
     $     csumm,ichat,status)
C     Set number of "bins" to number of expected input bins
      nbint = nobins(1)
c dummy dtnb to prevent overfloating. stop+0.02 is to prevent 
c overfloating for options frN and lrN with N same value.
      dtnb = (dtsto+0.02-dtsta)*86400.D0/dble(nbint)*1.1D0

      if (status.ne.0) then
         errm = subname//' '//'Error reading parameters'
         call xaerror(errm, 5)
         goto 999
      endif

C     lda specific variable settings


      do i=1,maxseries
c start time of first newbin 
         dxsta(i) = 0
c x-axis step is newbin integrat. (s)
         dxstep(i) = dtnb
      enddo
c no. of "analysed" points
      nanal = nobins(1)
c
c type No. of analysis points and set iflags(13)
c
      rebin = 0.
c     CALL xrtyana(nanal, rebin, iflags)
      iflags(13)=nanal
c

c

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
      
c no. of good intvs. in frame
c      intsta(4) = 1 
C     with dynamic memory, et al., this is :
      call lc_pintsta4(memi(intsta),iflags(10),1)

C     Allocate accumulation arrays
      call xrtmpalloc(nkount*iflags(10),6,3,1,yi,syi,expi,status)
      if(status.ne.0) goto 999
      
      do while(iend.eq.0) 

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
      enddo

C     Free arrays
      call xrtmpalloc(idum2,6,3,2,yi,syi,expi,status)
      if(status.ne.0) goto 999

 999  continue

      end





      










