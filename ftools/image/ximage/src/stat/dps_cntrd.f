      subroutine dps_cntrd(Map,Szx,Szy,W,Dd,Deriv,Bufsz,Bxcen,Bycen,
     &                     Boxrad,Xout,Yout,Status)
      implicit none
c
c  Use derivative of the partial sums wrt Bxcen, Bycen to
c   calculate centroid. Adapted from IDLastro command, CNTRD
c
c  PURPOSE: (from original CNTRD.pro)
c       Compute the centroid coordinates of a stellar object 
c       using the algorithm in the DAOPHOT FIND subroutine.
c
c  PROCEDURE: (from original CNTRD.pro)
c       Maximum pixel within distance from input pixel X, Y  determined 
c       from FHWM is found and used as the center of a square, within 
c       which the centroid is computed as the value (XOUT,YOUT) at which 
c       the derivatives of the partial sums of the input image over (y,x)
c       with respect to (x,y) = 0.
c
c  I  map    (r)  Image map
c  I  szx/y  (i)  Dimensions of image
c  I  w      (r)  Buffer for weighting factor
c  I  dd     (r)  Buffer for distance from center
c  I  deriv  (r)  Buffer for derivative
c  I  xcen   (r)  Approximate source center in x
c  I  ycen   (r)  Approximate source center in y
c  I  boxrad (r)  Half of box size
c  O  xout   (r)  Computed x centroid position
c  O  yout   (r)  Computed y centroid position
c  O  status (i)  Error flag (0=OK)
c
c ---CUT---
c  I  fwhm  (r)  Centroid is computed using a box of half
c                  width equal to 1.5 sigma = 0.637* FWHM
c ---CUT---
c
      integer*4 Szx, Szy, Bufsz, Status
      real*4 Map(Szx,Szy)
      real*4 W(Bufsz), Dd(Bufsz), Deriv(Bufsz)
      real*4 Boxrad, Bxcen, Bycen, Xout, Yout

      include '../include/maxvals.inc'
      include '../include/io.inc'
c
c  Local variables
c
      integer i, j, ii
      integer nhalf, nbox, nhalfbig, nbig
      integer ix, iy
      integer xmax, ymax
      real*4 sumc, sumd, sumxd, sumxsq
      real*4 dx, dy, curval, nxtval
      logical isrnull

      Status = 0

      nhalf = MAX(int(Boxrad), 2)
c     nhalf = MAX(int(0.637*fwhm), 2)
      nbox = 2*nhalf + 1
      nhalfbig = nhalf + 3
      nbig = nbox + 6

      xout = Bxcen
      yout = Bycen

      ix = NINT(Bxcen)
      iy = NINT(Bycen)

      if ( (ix.lt.nhalfbig) .or. (ix + nhalfbig.gt.Szx) .or.
     &     (iy.lt.nhalfbig) .or. (iy + nhalfbig.gt.Szy) ) then

         call XERROR (' Box is outside image', 5)
         return
      endif
c
c  Find maximum pixel value in box
c
c     maxval = 0
c     do i = ix-nhalfbig, ix+nhalfbig
c        do j = iy-nhalfbig, iy+nhalfbig
c           if ( map(i,j).gt.maxval ) then
c              maxval = map(i,j)
c              xmax = i
c              ymax = j
c           endif
c        enddo
c     enddo
c
c  Assume Bxcen,Bycen is already max pixel
c
      xmax = ix
      ymax = iy
c
c  Weighting factor w, unity in center, 0.5 at end, linear in between
c
      sumc = 0.
      do i = 1, nbox-1
         Dd(i) = float(i-1) + 0.5 - nhalf
         W(i) = 1 - 0.5*(abs(Dd(i))-0.5)/(nhalf-0.5)
         sumc = sumc + W(i)
         write(ZWRite,*) ' i/Dd/W ',i,Dd(i),W(i)
         call xwrite(ZWRite, 25)
      enddo
c
c Compute X centroid
c
c Subtract offset values in X to get derivative and sum over Y
c
      do i = xmax-nhalf, xmax+nhalf-1
         ii = i - (xmax-nhalf) + 1
         Deriv(ii) = 0.
         do j = ymax-nhalf+1, ymax+nhalf
            nxtval = Map(i+1,j)
            curval = Map(i,j)
            if ( isrnull(nxtval) ) nxtval = 0.
            if ( isrnull(curval) ) curval = 0.
            Deriv(ii) = Deriv(ii) + nxtval - curval
         enddo
      enddo

      call xwrite(' X derivative:', 25)
      do i = 1, nbox-1
         write (ZWRite,*) i, Deriv(i)
         call xwrite(ZWRite, 25)
      enddo

      sumd = 0.
      sumxd = 0.
      sumxsq = 0.
      do i = 1, nbox-1
         sumd = sumd + W(i)*Deriv(i)
         sumxd = sumxd + W(i)*Dd(i)*Deriv(i)
         sumxsq = sumxsq + W(i)*Dd(i)*Dd(i)
      enddo
      write(ZWRite,*) ' Sumc/d/xd/xsq for x: ', sumc,sumd,sumxd,sumxsq
      call xwrite(ZWRite, 25)
c
c  Reject if X derivative not decreasing
c
      if ( sumxd.gt.0 ) then
         call XWARN (' Unable to compute X centroid', 10)
         Status = -1
         return
      endif

      dx = sumxsq*sumd/(sumc*sumxd)
c
c  Reject if centroid outside box
c
      if ( abs(dx).gt.nhalf ) then
         call XWARN (' Computed centroid out of range', 10)
         Status = -1
         return
      endif

      xout = xmax - dx
c
c Compute Y centroid
c
c Subtract offset values in Y to get derivative and sum over X
c
      do j = ymax-nhalf, ymax+nhalf-1
         ii = j - (ymax-nhalf) + 1
         Deriv(ii) = 0.
         do i = xmax-nhalf+1, xmax+nhalf
            nxtval = Map(i,j+1)
            curval = Map(i,j)
            if ( isrnull(nxtval) ) nxtval = 0.
            if ( isrnull(curval) ) curval = 0.
            Deriv(ii) = Deriv(ii) + nxtval - curval
         enddo
      enddo

      call xwrite(' Y derivative:', 25)
      do i = 1, nbox-1
         write (ZWRite,*) i, Deriv(i)
         call xwrite(ZWRite, 25)
      enddo
  
      sumd = 0.
      sumxd = 0.
      sumxsq = 0.
      do i = 1, nbox-1
         sumd = sumd + W(i)*Deriv(i)
         sumxd = sumxd + W(i)*Dd(i)*Deriv(i)
         sumxsq = sumxsq + W(i)*Dd(i)*Dd(i)
      enddo
      write(ZWRite,*) ' Sumc/d/xd/xsq for y: ', sumc,sumd,sumxd,sumxsq
      call xwrite(ZWRite, 25)
c
c  Reject if Y derivative not decreasing
c
      if ( sumxd.gt.0 ) then
         call XWARN (' Unable to compute Y centroid', 10)
         Status = -1
         return
      endif

      dy = sumxsq*sumd/(sumc*sumxd)
c
c  Reject if centroid outside box
c
      if ( abs(dy).gt.nhalf ) then
         call XWARN (' Computed centroid out of range', 10)
         Status = -1
         return
      endif

      yout = ymax - dy

      return
      end
