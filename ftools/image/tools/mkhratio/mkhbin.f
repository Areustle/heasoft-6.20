      subroutine mkhbin(bins, nbins, status)
      implicit none
c
c  Assign bin values
c
c  O  bins   (r)  bin array
c  I  nbins  (i)  number of bins
c  O  status (i)  error flag (0=OK)
c  
      integer nbins, status
      real bins(nbins)
c
c  Local variables
c
      integer i
      real step
      character(80) zwrite
c

c     bins(1) = -10
      call xwrite(' bin #     value', 20)
c     write(zwrite,*) 1, bins(1)
c     call xwrite(zwrite, 20)
      step = 2./float(nbins-1)
      do i = 1, nbins-1
         bins(i) = -1. + step*float(i)
         write(zwrite,*) i, bins(i)
         call xwrite(zwrite, 20)
      enddo

      return
      end
