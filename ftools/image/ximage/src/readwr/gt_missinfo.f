      subroutine gt_missinfo(Idet,Xcol,Ycol,Ecol,Emin,Emax,
     &                       Szx,Szy,Usrzm,Gtiext,Status)
      implicit none 
c
c  Set default energy column, image size, gti,
c  and zoom based on mission unless already set
c
c I/O Xcol      (c) X column
c I/O Ycol      (c) Y column
c I/O Ecol      (c) Energy column
c I/O Emin      (i) Minimum energy value
c I/O Emax      (i) Maximum energy value
c I/O Szx       (i) Image size in x
c I/O Szy       (i) Image size in y
c I/O Usrzm(2)  (i) Rebin requested by user
c I/O Gtiext    (c) GTI extension expression
c  O  Status    (i) Error flag (0=OK)
c
      integer*4 Idet, Emin, Emax
      character*(*) Xcol, Ycol, Ecol, Gtiext
      integer*4 Szx, Szy, Status
      real*8 Usrzm(2)
c
c  Local variables
c
      integer*4 di
c
c X and Y columns
c
      if ( Xcol.eq.' ' ) then
         call gmdbs(Idet, 'XCOL', Xcol, 0, Status)
      endif
      if ( Ycol.eq.' ' ) then
         call gmdbs(Idet, 'YCOL', Ycol, 0, Status)
      endif
c
c Emin and Emax
c
      if ( Ecol.eq.' ' ) call gmdbs(Idet, 'ECOL', Ecol, 0, Status)
      if ( Emin.le.0 ) then
         call gmdbi(Idet, 'EMIN', Emin, 0, Status)
      else if ( Ecol.eq.' ' ) then
         call xwrite(' No ECOL, EMIN ignored', 10)
         Emin = 0
      endif
      if ( Emax.le.0 ) then
         call gmdbi(Idet, 'EMAX', Emax, 0, Status)
      else if ( Ecol.eq.' ' ) then
         call xwrite(' No ECOL, EMAX ignored', 10)
         Emax = 0
      endif
c
c Optimal image size
c
      if ( Szx.eq.0 .and. Szy.eq.0 ) then
         call gmdbi(Idet, 'DEFSZX', Szx, 0, Status)
         call gmdbi(Idet, 'DEFSZY', Szy, 0, Status)
         if ( Szx.ne.0 .and. Szy.eq.0 ) Szy = Szx
         if ( Szx.eq.0 .and. Szy.ne.0 ) Szx = Szy
      endif
         
      if ( Szx.eq.0 .and. Szy.eq.0 ) then
         Szx = 256
         Szy = 256
      endif
c
c Optimal zoom
c
      if ( Usrzm(1).le.0.d0 .or. Usrzm(2).le.0.d0 ) then
         call gmdbi(Idet, 'DEFZOOM', di, 0, Status)
         Usrzm(1) = di
         Usrzm(2) = di
      endif
c
c GTI expression
c
      if ( Gtiext.eq.' ' ) then
         call gmdbs(Idet, 'GTIEXT', Gtiext, 0, Status)
      endif

      return
      end
