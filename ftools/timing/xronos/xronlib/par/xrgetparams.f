C**********************************************************************
C
C subroutine xrgetparams.f
C            Central XRONOS parameter fetching, rationalization,
C            and file checkout routine
C
C written by:
C     Lawrence E. Brown
C     Hughes STX for 
C     HEASARC/GSFC/NASA
C     6/23/95
C
C modification history:
C     see CVS logs
C
C calling sequence:
c     call xrgetparams(cpf, ipf, rpf, dpf, iflags, rflags,
c    $     progname,progtype, ipow2, nintfm, ilda, nfil, cfile,
c    $     dtint,dtsta,dtsto, twia, twio, pwi, pwia, pwio, fwia, 
c    $     fwio, ewia, ewio, nwi, nbdf, dtnb, nbint, nobins, ngtis,
c    $     depoch, dper, dpdot, nbin, dres, nper, cfilo, cqdp, iqdp,
c    $     iuser,buser,duser,ruser,suser,ichat,status)
C
C variables:
C
C     all are currently output variables; see xronos5.inc for descriptions
C
C**********************************************************************

      subroutine xrgetparams(cpf, ipf, rpf, dpf, iflags, rflags,
     $     progname,progtype, ipow2, nintfm, ilda, nfil, cfile,
     $     dtint,dtsta,dtsto, twia, twio, pwi, pwia, pwio, fwia, 
     $     fwio, ewia, ewio, nwi, nbdf, dtnb, nbint, nobins, ngtis,
     $     depoch, dper, dpdot, nbin, dres, nper, cfilo, oftype, cqdp, 
     $     iqdp,iuser,buser,duser,ruser,suser,csumm,ichat,status)


      implicit none

      include '../include/io.inc'
      include '../include/xronos.inc'



      integer perfo
      integer k, k1, k2,idum
C normalization value
      real xnorm
      real rv
      double precision dintv
      include '../include/xronos_init.inc'
      parameter (subname = 'xrgetparams:')

      if(status.ne.0) return

C     initialize some stuff

      do idum = 1,maxtimewin
         twia(idum) = 0.0
         twio(idum) = 0.0
      enddo
      do idum = 1,maxwintype
         nwi(idum) = 0
      enddo

C moved initialization to xronos.inc include file
C undoubtedly, some will have to migrate back
      
      cpf(5) = progname

C get parameters from parameter file corresponding to old .pf global ones
C this routine also call xchaty to set up terminal and log chattiness
      call xrrdpf (cpf, ipf, rpf, dpf, iflags, rflags, ichat,
     $     progname, status)
      if(status.ne.0) then
         context = 
     $   'Error getting global parameters or initializing chattiness'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

      
C     This gets number of time series involved, and a few other
C     task specific things

      call xrgetname(iflags(10),ipow2,nintfm,nbdf,status)
      if(progname.eq.'ld') then 
         ilda=1
      else
         ilda=0
      endif

      if(status.ne.0) then
         context = 
     $   'Error getting program name and characteristics.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

C     "fast" modes require a power of 2 number of bins:
C     so if ipow2==1 and iflags(15)==1 (meaning "fast") 
C     set ipow2=1
      if(ipow2.eq.1) then
         ipow2=0
         if(iflags(15).eq.1) ipow2=1
      endif


C ask for input filenames and options
    
      CALL xrgetfiles(iflags(10), nfil, cfile,status)
      if(status.ne.0) then
         context = 
     $   'Error getting input filenames.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif



c  get start and stop times for all series
c     and list header content
      call xrcheckfiles(iflags(10),nfil,cfile,dtint,dtsta,dtsto,
     $     nobins, ngtis, csumm, status)
      if(status.ne.0) then
         context = 
     $   'Error while checking out input files.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

c
c get windows
c
      call xrgetwin (iflags(10), cpf, twia, twio, pwi, pwia, pwio,
     &     fwia, fwio, ewia, ewio, nwi, iflags(5), 
     $     dtsta,dtsto,status)
      if(status.ne.0) then
         context = 
     $   'Error getting window file.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

      if(ilda.eq.1) goto 999


c get newbin duration, no. of newbins/intv. and no. of intv./frame
c  (and modify dtsta and dtsto according to time windows and options if nec.)
c

      
      if(progtype(1:4).eq.'TIME') then

C     set code to enforce minimum newbin sizes  (or not for 'lc' tasks)
         if(progname(1:2).eq.'lc'.or.progname(1:2).eq.'ls') then
            isev=0
         else
            isev=1
         endif

         



         call xrgettime(dtint, dtsta, dtsto, 
     &     nbdf, ipow2, isev, dtnb, nbint, 
     &     status)
         if(status.ne.0) then
            context = 
     $           'Error getting TIME task parameters.'
            errm = subname//' '//context
            call xaerror(errm, 5)
            goto 999
         endif
      else if(progtype(1:4).eq.'FOLD') then
         call xrgetfold(dtint, dtsta, dtsto, nbdf, dtnb,
     $      depoch, dper, perfo, dpdot, nbin, nbint, status)
         if(status.ne.0) then
            context = 
     $           'Error getting FOLD task parameters.'
            errm = subname//' '//context
            call xaerror(errm, 5)
            goto 999
         endif

      endif

         
C     Note that xrgetnbin has one IF block that depends on
C     progtype
      CALL xrgetnbin( dtint, dtsta, dtsto, twia, twio, 
     $     nwi, ipf, ipow2,  dtnb, nbint, nintfm, progtype,
     $     iflags, rflags, status)
      if(status.ne.0) then
         context = 
     $        'Error getting new bin parameters.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

 
      dres = 0.d0
      nper = 1
      if(progtype(5:10).eq.'SEARCH') 
     $     call xrgetres(dtnb, dper, nbint, nbin, perfo,
     $     dres, nper, status)
      if(status.ne.0) then
         context = 
     $        'Error getting SEARCH task parameters.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

      if(progtype(1:4).eq.'TIME') then
C     get trend removal parameters
         call xrgettrend(iflags(3),iflags(4),status)

         if(status.ne.0) then
            context = 
     $           'Error getting trend removal task parameters.'
            errm = subname//' '//context
            call xaerror(errm, 5)
            goto 999
         endif
      endif



      if(progname(1:2).eq.'lc') 
     $     call xrgetlc(dtnb,nbint,iflags(2),status)
      if(status.ne.0) then
         context = 
     $   'Error getting lc task parameters.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

C     calculate expected number of intvs.
      dintv = (dtsto-dtsta)*86400.D0/dtnb/dble(nbint)
      nintv = int(dintv)
      if(nintv.lt.dintv) nintv=nintv+1
      
      if(progname(1:2).eq.'ls') goto 999
c
c ask for output filename
c
      CALL xrgetfout(cfile, cpf, progname, nintv, 
     $     cfilo, oftype, status)
      if(status.ne.0) then
         context = 
     $   'Error getting output filename.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

c
c ask for plot options
c

      CALL xrgetplto(progname,iflags,cqdp,iqdp,status)
      if(status.ne.0) then
         context = 
     $   'Error getting plotting options.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

C IRAF barfs on the way we're doing this, so this is commented out for 
C now, since we're not yet using it
C      call xrgetuser(iuser,buser,duser,ruser,suser,status)
C      if(status.ne.0) then
C         context = 
C     $   'Error getting user defined parameters.'
C         call xaerror(context, 5)
C         goto 999
C      endif


c
c end of interactive part
 999  continue
      return
      end









