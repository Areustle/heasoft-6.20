      subroutine calchist(A, idim, jdim, i1, i2, j1, j2, minlev, maxlev, 
     &                    numlevs, levels, status)
      implicit none
c
c  Calculate levels for histogram imaging
c  Base initial histogram on sorted list of real values
c
c  I  A       (r)  2D Array
c  I  idim    (i)  Size in x
c  I  jdim    (i)  Size in y
c  I  i1      (i)  Minimum x
c  I  i2      (i)  Maximum x
c  I  j1      (i)  Minimum y
c  I  j2      (i)  Maximum y
c  I  minlev  (r)  Minimum histogram bin
c  I  maxlev  (r)  Maximum histogram bin
c  I  numlevs (i)  Number of levels(bins) to be calculated
c  O  levels  (r)  Histogram levels
c  O  status  (i)  Error flag  (0=OK)
c
      integer idim, jdim, i1, i2, j1, j2
      real*4 A(idim,jdim)
      real minlev, maxlev
      integer numlevs, status
      real*4 levels(*)
      logical isrnull

      include '../include/dynmem.inc'
c
c  Local variables
c
      integer*4 i, j, p_hist, ndim, nhist, ihist
      real*4 lastval
      integer*4 ntot, thr, sum

      status = 0

      ndim = (i2-i1+1)*(j2-j1+1)

      call ralloc(1, ndim, 1, p_hist, status)
      if ( status.ne.0 ) then
         call XWRITE ('Memory allocation failed for histogram '//
     &                'calculation', 10)
         status = -1
         return
      endif

      ihist = 1
      do i = i1, i2
         do j = j1, j2
            if ( .not.isrnull(A(i,j)) ) then
               memr(p_hist+ihist-1) = A(i,j)
               ihist = ihist + 1
            endif
         enddo
      enddo
      nhist = ihist-1

      call stdqsort(memr(p_hist),ndim,nhist,status)
      if ( status.ne.0 ) return
c 
c   Filter out values under minlev and over maxlev 
c 
      i = 1
      ihist = nhist
      do while ( memr(p_hist+ihist-1).gt.maxlev .and. ihist.gt.0 ) 
         ihist = ihist - 1
      enddo
      do while ( memr(p_hist+i-1).lt.minlev .and. i.le.ihist )
         i = i + 1
      enddo
c 
c   Determine levels
c    i  - index of bin
c    j  - index of levels
c   thr - threshold
c   
      j = 1
      lastval = minlev
      nhist = ihist
      ntot = i
      do while ( j.le.numlevs .and. i.le.nhist )
         sum = 0
         thr = (nhist - ntot + 1)/(numlevs - j + 1)
         do while ( sum.lt.thr .or. (i.lt.nhist .and. 
     &                               lastval.eq.memr(p_hist+i-1)) )
            lastval = memr(p_hist+i-1)
            i = i + 1
            sum = sum + 1
         enddo
         levels(j) = lastval

         ntot = ntot + sum
         j = j + 1
      enddo

c     If you run out of pixels, fill with last level */

      do while ( j.lt.numlevs ) 
         levels(j) = lastval
         j = j + 1
      enddo

c 
c   Free space
c  
      call ralloc(0, ndim, 1, p_hist, status)

      return
      end
