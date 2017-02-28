      subroutine cphead(frommap, tomap)

      implicit none

      include '../include/maxvals.inc'
      include 'header.inc'
c
c  Copies one map's header to another
c  
c  I  frommap  (c) Header to be copied
c  I  tomap    (c) Header copy
c
      character*(*) frommap, tomap
c
c  Local variables
c
      integer*4 LENACT, status, i
      character(80) msg
      integer*4 ifrom, ito


      if ( frommap.eq.' ' ) then
         write(msg,'(3a)') ' Initializing ', tomap(:LENACT(tomap)), 
     &                     ' header'
         call xwrite(msg, 25)
      else
         write(msg,'(5a)') ' Copying ', frommap(:LENACT(frommap)),
     &                     ' header to ', tomap(:LENACT(tomap)), 
     &                     ' header'
         call xwrite(msg, 25)
         call mapidx(frommap, ifrom, status)
         if ( status.ne.0 ) return
      endif

      call mapidx(tomap, ito, status)

      if ( frommap.eq.' ' ) then
         do i = 1, dbl_num
            dbl_hdr(i,ito) = 0.
         enddo
         do i = 1, chr_num
            chr_hdr(i,ito) = ' '
         enddo
         do i = 1, int_num
            int_hdr(i,ito) = 0
         enddo
      else
         if ( ito.eq.ifrom ) return
         do i = 1, dbl_num
            dbl_hdr(i,ito) = dbl_hdr(i,ifrom)
         enddo
         do i = 1, chr_num
            chr_hdr(i,ito) = chr_hdr(i,ifrom)
         enddo
         do i = 1, int_num
            int_hdr(i,ito) = int_hdr(i,ifrom)
         enddo
      endif

      return
      end
