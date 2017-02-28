      subroutine mkclvl(map,szx,szy,hist,szhist,lvl,mxinten,status)
      implicit none
c
c  Calculate level values
c
c  I  map     (i)  image map
c  I  szx/szy (i)  size of image
c  I  hist    (i)  histogram buffer
c  O  szhist  (i)  size of histogram buffer
c  I  lvl     (r)  level values
c  I  mxinten (i)  maximum intensity to scale to
c  I  status  (i)  error flag (0=OK)
c
      integer szx, szy, szhist, mxinten, status
      integer map(szx,szy), hist(szhist)
      real lvl(mxinten+1)
c
c  Local variables
c
      integer i, j, nonemp, sum
      real part
c
c  Initialize
c
      do i = 1, szhist
         hist(i) = 0
      enddo
      nonemp = 0
c
c  Calculate histogram
c
      do i = 1, szx
         do j = 1, szy
            if ( map(i,j).gt.0 ) then
               hist(map(i,j)) = hist(map(i,j)) + 1
               nonemp = nonemp + 1
            endif
         enddo
      enddo
c
c  Find levels
c
      lvl(1) = 0.
      lvl(mxinten+1) = float(szhist)
      part = float(nonemp)/float(mxinten)
      j = 1
      do i = 2, mxinten
         sum = 0
         do while (j.le.szhist .and. sum.lt.part )
            sum = sum + hist(j)
            j = j + 1
         enddo
         lvl(i) = float(j-1)
      enddo

      return
      end
