      subroutine initmdb
      implicit none
c
c  Initialize contents of mission database
c
      include 'mdb.inc'
c
c  Local variables
c
      integer i, j

      ZEXpnum = 0
      do i = 1, ZEXpmax
         ztelescop(i) = ' '
         zinstrume(i) = ' '
         zdetnam(i) = ' '
         do j = 1, mdbi_num
            int_mdb(j,i) = mdbi_defs(j)
         enddo
         do j = 1, mdbd_num
            dbl_mdb(j,i) = mdbd_defs(j)
         enddo
         do j = 1, mdbs_num
            str_mdb(j,i) = mdbs_defs(j)
         enddo
      enddo

      return
      end
