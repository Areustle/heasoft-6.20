      subroutine calccor(Cmdid,Status)
      implicit none
c
c  Take set of x/y for skewed frame and corresponding x/y for
c  for reference frame and calculate offset and rotation
c
c  I  cmdid     (i)  Command id
c  O  status    (i)  Error flag (0=OK)
c
      integer cmdid, status

      integer MAXPTS, npts, nxs, nys, nxr, nyr, nvc, argc
      parameter (MAXPTS=100)
      real xref(MAXPTS), yref(MAXPTS), xskew(MAXPTS), yskew(MAXPTS)
      logical norot

      integer i, j, mp, np, ndim, iter
      parameter(mp=4,np=3,ndim=3)
      real p(mp,np), y(mp), ftol, vec(3), initvec(3), scale(ndim)
      real delx, dely, theta
      logical readonly, global

      real ccminfun
      external ccminfun

      include '../include/maxvals.inc'
      include '../include/pi.inc'
      include '../include/io.inc'

      nxs = 0
      nys = 0
      nxr = 0
      nyr = 0
      nvc = 0
      norot = .FALSE.

      Status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARLR(Cmdid,'XSKEW',xskew,nxs,MAXPTS,Status)
      CALL GPARLR(Cmdid,'YSKEW',yskew,nys,MAXPTS,Status)
      CALL GPARLR(Cmdid,'XREF',xref,nxr,MAXPTS,Status)
      CALL GPARLR(Cmdid,'YREF',yref,nyr,MAXPTS,Status)
      CALL GPARLR(Cmdid,'INITVEC',initvec,nvc,3,Status)
      CALL GPARL(Cmdid,'NOROT',norot,Status)
      if ( Status.ne.0 ) return

      npts = nxs
      if ( npts.eq.0 ) then
         call xwrite(' No points given for calculation', 5)
         status = -1
         return
      endif
      if ( npts.ne.nys .or. npts.ne.nxr .or. npts.ne.nyr ) then
         call xwrite(' X/YREF and X/YSKEW must be of equal number', 5)
         status = -1
         return
      endif
      if ( nvc.ne.0 .and. nvc.ne.3 ) then
         call xwrite(' Initial vector must have exactly 3 elements', 5)
         status = -1
         return
      endif

      ftol = 1.0e-5
      if ( nvc.eq.0 ) then
         p(1,1) = 0.
         p(1,2) = 0.
         p(1,3) = 0.
      else
         p(1,1) = initvec(1)
         p(1,2) = initvec(2)
         p(1,3) = initvec(3)
      endif
      do i = 1, ndim+1
         do j = 1, ndim
            p(i,j) = p(1,j)
         enddo
      enddo
c
c  Set resolution of simplex
c
      scale(1) = 0.5
      scale(2) = 0.5
      if ( norot ) then
         scale(3) = 0.
      else
         scale(3) = 2.*PI/360.
      endif
      do i = 1, ndim
         p(i+1,i) = p(i+1,i) + scale(i)
      enddo
c
c  Initialize y by calculating the minimization function for
c  each point in the simplex
c
      do i = 1, ndim+1
         vec(1) = p(i,1)
         vec(2) = p(i,2)
         vec(3) = p(i,3)
         y(i) = ccminfun(vec,MAXPTS,npts,xref,yref,xskew,yskew)
      enddo
      call ccamoeba(p,y,mp,np,ndim,ftol,ccminfun,MAXPTS,npts,xref,yref,
     &              xskew,yskew,iter)

      delx = p(1,1)
      dely = p(1,2)
      theta = p(1,3)

      write(ZWRite, *) ' Number of iterations : ', iter
      call xwrite(ZWRite, 15)
      write(ZWRite, *) ' DelX : ', delx
      call xwrite(ZWRite, 15)
      write(ZWRite, *) ' DelY : ', dely
      call xwrite(ZWRite, 15)
      write(ZWRite, *) ' Theta: ', theta, ' (rad) ', theta*180.d0/PI,
     &                 ' (deg)'
      call xwrite(ZWRite, 15)

      readonly = .FALSE.
      global = .FALSE.
      call tclvari('calccor(iter)', iter, readonly, global, status)
      call tclvarr('calccor(delx)', delx, readonly, global, status)
      call tclvarr('calccor(dely)', dely, readonly, global, status)
      call tclvarr('calccor(theta)', theta, readonly, global, status)

      end
