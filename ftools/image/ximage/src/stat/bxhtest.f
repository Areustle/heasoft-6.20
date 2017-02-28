      subroutine bxhtest(Map, Szx, Szy, Imin1, Imax1, Imin2, Imax2, 
     &                   Probx, Proby, Status)
      implicit none
c
c  Background box half test.  Splits box in half vertically, compares
c  sides with chi square test, then splits horizontally and does the
c  same.
c
c  Note on NULLs: As long as special NULL value is such that
c                 NINT(rnull()) = 0 no change is needed
c                 to assume all nulls as zero
c
c  I  map   (r)  Image map
c  I  szx/y (i)  Size of image map
c  I  imin1 (i)  Minimum index of bg box in x
c  I  imax1 (i)  Maximum index of bg box in x
c  I  imin2 (i)  Minimum index of bg box in y
c  I  imax2 (i)  Maximum index of bg box in y
c  O  probx (r)  Probability that x sides match
c  O  proby (r)  Probability that y sides match
c
      integer Szx, Szy, Imin1, Imax1, Imin2, Imax2, Status
      real*4 Map(Szx,Szy)
      real*4 Probx, Proby

      include '../include/dynmem.inc'
c
c  Local variables
c
      integer npix
      integer p_alist, p_blist, p_rtmp
      integer bxsz, bxszh, i, j, k
      integer frstat

      if ( Imax1-Imin1.ne.Imax2-Imin2 ) then
         call XWRITE(' Non-square box in bghalft', 5)
         status = -1
         return
      endif

      bxsz = Imax1-Imin1+1
      bxszh = bxsz/2
      npix = bxsz*bxszh

      call workalloc(1, npix, 1, p_alist, status)
      if ( status.ne.0 ) goto 500
      call workalloc(1, npix, 1, p_blist, status)
      if ( status.ne.0 ) goto 500
      call ralloc(1, npix, 1, p_rtmp, status)
      if ( status.ne.0 ) goto 500
c
c  First do x direction (split vertically)
c
c    Record and sort values from left half
c
      k = 1
      do i = Imin1, Imin1+bxszh-1
         do j = Imin2, Imax2
            memr(p_rtmp+k-1) = nint(Map(i,j))
            k = k + 1
         enddo
      enddo
      call stdqsort(memr(p_rtmp), npix, npix, status)
      if ( status.ne.0 ) goto 500
      do i = 1, npix
         memi(p_alist+i-1) = memr(p_rtmp+i-1)
      enddo
c
c    Record and sort values from right half
c
      k = 1
      do i = Imax1-bxszh+1, Imax1
         do j = Imin2, Imax2
            memr(p_rtmp+k-1) = nint(Map(i,j))
            k = k + 1
         enddo
      enddo
      call stdqsort(memr(p_rtmp), npix, npix, status)
      if ( status.ne.0 ) goto 500
      do i = 1, npix
         memi(p_blist+i-1) = memr(p_rtmp+i-1)
      enddo

      call bxhwork(memi(p_alist), memi(p_blist), npix, probx)
c
c  Next do y direction (split horizontally)
c
c    Record and sort values from bottom half
c
      k = 1
      do i = Imin1, Imax1
         do j = Imin2, Imin2+bxszh-1
            memr(p_rtmp+k-1) = nint(Map(i,j))
            k = k + 1
         enddo
      enddo
      call stdqsort(memr(p_rtmp), npix, npix, status)
      if ( status.ne.0 ) goto 500
      do i = 1, npix
         memi(p_alist+i-1) = memr(p_rtmp+i-1)
      enddo
c
c    Record and sort values from top half
c
      k = 1
      do i = Imin1, Imax1
         do j = Imax2-bxszh+1, Imax2
            memr(p_rtmp+k-1) = nint(Map(i,j))
            k = k + 1
         enddo
      enddo
      call stdqsort(memr(p_rtmp), npix, npix, status)
      if ( status.ne.0 ) goto 500
      do i = 1, npix
         memi(p_blist+i-1) = memr(p_rtmp+i-1)
      enddo

      call bxhwork(memi(p_alist), memi(p_blist), npix, proby)

 500  call workalloc(0, npix, 1, p_alist, frstat)
      call workalloc(0, npix, 1, p_blist, frstat)
      call ralloc(0, npix, 1, p_rtmp, status)

      return
      end
