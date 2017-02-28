      subroutine mkcread(filename, map, szx, szy, datamin,
     &                   datamax, status)
      implicit none
c
c  Read in image map
c
c  I  filename  (s)  image file
c  O  map       (i)  image array
c  I  szx/y     (i)  size of image
c  O  datamin   (d)  minimum data value
c  O  datamax   (d)  maximum data value
c  O  status    (i)  error flag (0=OK)
c
      character*(*) filename
      integer szx, szy, status
      integer map(szx,szy)
      real*8 datamin, datamax
c
c  Local variables
c
      integer unit, readwrite, blocksize
      integer group, nullval
      integer i, j  
      logical anyf
c
c read a fits image and return min/max
c
      status=0
c
c     Get an unused Logical Unit Number to use to open the FITS file
      call ftgiou(unit,status)
c
      readwrite=0
      call ftopen(unit,filename,readwrite,blocksize,status)
c
c     initialize variables
      group=1
      nullval = 0
      datamin=1.0E30
      datamax=-1.0E30

      call ftg2dj(unit,group,nullval,szx,szx,szy,map,anyf,status)
c
c Determine image min/max
c
      do i = 1, szx
         do j = 1, szy
            datamin=min(datamin,dfloat(map(i,j)))
            datamax=max(datamax,dfloat(map(i,j)))
         enddo
      enddo
c     close the file and free the unit number
      call ftclos(unit, status)
      call ftfiou(unit, status)
      return 
      end
