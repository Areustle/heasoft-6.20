      subroutine rebinmax (Map, Szx, Szy, Bxbuf, Ibox, Bxcen, Bycen,
     &                     Xout, Yout, Status)
      implicit none
c
c  Find maximum in box by rebinning until location of maximum
c  stabilizes.
c
c  I  map     (r) Image map
c  I  szx/y   (i) Image size
c  I  bxbuf   (i) Work buffer for rebin calculation
c  I  ibox    (i) Size of work buffer
c  I  bxcen   (r) Center of box in image x coordinate
c  I  bycen   (r) Center of box in image y coordinate
c  O  xout    (r) Computed maximum pixel in image x coordinates
c  O  yout    (r) Computed maximum pixel in image y coordinates
c  O  status  (i) Error flag (0=OK)
c
      integer*4 Szx, Szy, Ibox, Status
      real*4 Map(Szx,Szy), Bxbuf(Ibox,Ibox)
      real*4 Bxcen, Bycen, Xout, Yout
c
c  Local variables
c
      include '../include/maxvals.inc'
      include '../include/io.inc'

      integer i, j, k, l, n, mi, mj
      integer rebin, iter, totzoom
      integer ixcen, iycen, boxsize
      real*4 maxval, xmax, ymax, last_xmax, last_ymax, sum, rnull
      logical stable, isrnull

      status = 0

      rebin = 2
      iter = 10
c
      boxsize = Ibox
      ixcen = int(Bxcen)
      iycen = int(Bycen)
      write(ZWRite,*) ' Considering box Map(', 1-Ibox/2+ixcen, ' ,',
     &                                         1-Ibox/2+iycen, ' )'
      call xwrite(ZWRite, 25)
      write(ZWRite,*) '              to Map(', Ibox-Ibox/2+ixcen, ' ,',
     &                                         Ibox-Ibox/2+iycen, ' )'
      call xwrite(ZWRite, 25)
c
c  Copy values in box into work area
c
      do i = 1, Ibox
         do j = 1, Ibox
            mi = i-Ibox/2+ixcen
            mj = j-Ibox/2+iycen
            if ( mi.ge.1 .and. mi.le.Szx .and.
     &           mj.ge.1 .and. mj.le.Szy ) then
               Bxbuf(i,j) = Map(mi,mj)
            else
               Status = -1
               call xwrite(' Box contains pixels outside image', 5)
               return
            endif
         enddo
      enddo
c
c  Keep Rebinning by 2 until either 10 iterations, the region has been
c  reduced to a single bin or an iteration yields a max bin
c  which contains the max bin from a previous iteration
c
      totzoom = 1
      n = 1
      last_xmax = 0
      last_ymax = 0
      stable = .FALSE.
      do while ( n.le.iter .and. .not.stable .and. boxsize.gt.1 )
c
c  Find maximum pixel in box
c
         maxval = rnull()
         do i = 1, boxsize
            do j = 1, boxsize
               if ( .not.isrnull(Bxbuf(i,j)) ) then
                  if ( isrnull(maxval) .or. Bxbuf(i,j).gt.maxval ) then
                     maxval = Bxbuf(i,j)
                     xmax = i
                     ymax = j
                  endif
               endif
            enddo
         enddo
         xmax = (xmax-0.5)*totzoom + 0.5
         ymax = (ymax-0.5)*totzoom + 0.5
         write(ZWRite,*) ' Rebin, Boxsize, Maxval/x/y', totzoom,boxsize,
     &                    maxval,xmax,ymax
         call XWRITE (ZWRite,25)
         if ( abs(last_xmax - xmax).le.totzoom/2 .and. 
     &        abs(last_ymax - ymax).le.totzoom/2) then
            stable = .TRUE.
c          If last run <= half pixel difference, use value for last run
c          Assumes lower rebin runs are more accurate
            xmax = last_xmax
            ymax = last_ymax
         endif
         last_xmax = xmax
         last_ymax = ymax
c
c  Rebin work area
c
         if ( .not.stable ) then
            do i = 1, boxsize
               do j = 1, boxsize
                  sum = rnull()
                  do k = (i-1)*rebin + 1, i*rebin
                     do l = (j-1)*rebin + 1, j*rebin
                        if ( k.gt.0 .and. k.le.boxsize .and.
     &                       l.gt.0 .and. l.le.boxsize ) then
                           if ( .not.isrnull(Bxbuf(k,l)) ) then
                              if ( isrnull(sum) ) then
                                 sum = Bxbuf(k,l)
                              else
                                 sum = sum + Bxbuf(k,l)
                              endif
                           endif
                        endif
                     enddo
                  enddo
                  Bxbuf(i,j) = sum
               enddo
            enddo

            totzoom = totzoom*rebin
            boxsize = boxsize/rebin
         endif
         n = n + 1

      enddo
c
c  Convert found maximum into image coordinates
c
      Xout = xmax - (float(Ibox)/2. + 0.5) + bxcen
      Yout = ymax - (float(Ibox)/2. + 0.5) + bycen

      return
      end
