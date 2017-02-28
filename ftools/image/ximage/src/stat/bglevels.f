      subroutine bglevels(Cmdid,Map,Szx,Szy,Mapid,
     &                    Numload,Ldlevs,Status)

      implicit none

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include 'backgd.inc'
c
c  Calculate levels based on background
c  
c  I  cmdid        (i) Command id
c  I  map          (r) Image map
c  I  szx/y        (i) Size of image map
c  I  mapid        (s) Map id string
c I/O numload      (i) Number of levels loaded
c I/O ldlevs       (r) Loaded levels
c  O  status       (i) Error flag (0=OK)
c
      integer Cmdid, Szx, Szy, Numload, Status
      real*4 Map(Szx,Szy), Ldlevs(*)
      character*(*) Mapid
c
c  Local variables
c
      INTEGER argc, i, sigfig, ilen, olen
      real*8 dd
      parameter(sigfig = 8)
      character(40) istr, ostr

      real*4 RNULL, POISS_UPLIM, background, bgimg
      LOGICAL optimize, ISRNULL, ISLOADED
      integer numsig, nummult, ibbace
      real*4 siglevs(MAX_NUMLEVS), multlevs(MAX_NUMLEVS)

      logical readonly, global

      background = RNULL()
      numsig = 0
      nummult = 0

      Status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARR(Cmdid,'BACKGROUND',background,Status)
      CALL GPARLR(Cmdid,'SIGMA_LEVELS',siglevs,numsig,MAX_NUMLEVS,
     &            Status)
      CALL GPARLR(Cmdid,'MULT_LEVELS',multlevs,nummult,MAX_NUMLEVS,
     &            Status)
      if ( Status.ne.0 ) return

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 10)
         Status = -1
         return
      endif
c
c  Set background
c
      ibbace = 0
      if ( .not.ISRNULL(background) ) then
         bgimg = background
         BNEw = bgimg
         call prback(mapid, status)
         NBOxes = 0
      elseif ( NBOxes.gt.0 ) then
         call xwrite(' Using existing background calculation', 10)
         call prback(mapid, status)
         bgimg = BNEw
      else
         call getbgmap(Mapid, BGMap, BGSz, status)
         if ( Status.ne.0 ) return
         optimize = .FALSE.
         SIGmult = DEFsigm
         BARylim = DEFbarl
         BXHprob = DEFhprob
         call do_back(Map, Szx, Szy, Mapid, optimize, ibbace, status)
         if ( status.eq.0 ) then
            bgimg = BNEw
         else
            call xwrite(' Background calc failed, using 0.', 10)
            bgimg = 0.
         endif
         NBOxes = 0
      endif
      status = 0
c
c  Default
c
      if ( numsig.eq.0 .and. nummult.eq.0 ) then
         numsig = 9
         siglevs(1) = 2.
         siglevs(2) = 3.
         siglevs(3) = 4.
         siglevs(4) = 6.
         siglevs(5) = 9.
         siglevs(6) = 12.
         siglevs(7) = 18.
         siglevs(8) = 35.
         siglevs(9) = 100.
      endif

      if ( numsig.gt.0 ) then

         Numload = numsig
         if ( bgimg.ge.20. ) then
            do i = 1, numsig
               Ldlevs(i) = siglevs(i)*SQRT(bgimg)+bgimg
            enddo
         else
            call xwrite(' Using upper limit approximation..', 10)
            do i = 1, numsig
               Ldlevs(i) = POISS_UPLIM(siglevs(i), bgimg)
            enddo
         endif

      elseif ( nummult.gt.0 ) then

         Numload = nummult
         do i = 1, nummult
            Ldlevs(i) = multlevs(i)*bgimg
         enddo

      endif
c
c  Print calculated levels
c
      call xwrite(' ', 10)
      if ( numsig.gt.0 ) then
         call xwrite(' #    sigma        contour lvl', 10)
         call xwrite('---   ---------    -----------', 10)
         do i = 1, Numload
            dd = Ldlevs(i)
            call xdstr(dd, sigfig, ostr, olen)
            dd = siglevs(i)
            call xdstr(dd, sigfig, istr, ilen)
            write(ZWRite,'(i3,3x,a,4x,a)') i, istr(:ilen), ostr(:olen)
            call xwrite(ZWRite, 10)
         enddo
      elseif ( nummult.gt.0 ) then
         call xwrite(' #    mult         contour lvl', 10)
         call xwrite('---   ---------    -----------', 10)
         do i = 1, Numload
            dd = Ldlevs(i)
            call xdstr(dd, sigfig, ostr, olen)
            dd = multlevs(i)
            call xdstr(dd, sigfig, istr, ilen)
            write(ZWRite,'(i3,3x,a,4x,a)') i, istr(:ilen), ostr(:olen)
            call xwrite(ZWRite, 10)
         enddo
      endif
      call xwrite(' ', 10)
c
c  Export levels as list to Tcl
c
      readonly = .TRUE.
      global = .TRUE.
      call tclvarlr('levels(list)', Ldlevs, Numload, readonly, global,
     &              status)

      status = 0
      return
      end
