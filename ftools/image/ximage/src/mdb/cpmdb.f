      subroutine cpmdb(fromtel, frominst, fromdet, totel, toinst, 
     &                 todet, status)
      implicit none
c
c  Copy mdb entry from one mission to another
c
c  I  fromtel   (s)  Telescope copying from
c  I  frominst  (s)  Instrument copying from
c  I  fromdet   (s)  Detector copying from
c  I  totel     (s)  Telescope copying to
c  I  toinst    (s)  Instrument copying to
c  I  todet     (s)  Detector copying to
c  O  status    (i)  Error flag (0=OK)
c
      character*(*) fromtel, frominst, fromdet
      character*(*) totel, toinst, todet
      integer*4 status

      include '../include/io.inc'
      include 'mdb.inc'
c
c  Local variables
c
      integer*4 ifrom, ito, EDETIDX, DETIDX, i

      status = 0

      ifrom = DETIDX(fromtel, frominst, fromdet)
      if ( ifrom.le.0 ) then
         call xwrite(' Mission copying from not found', 10)
         status = -1
         return
      endif

      ito = EDETIDX(totel, toinst, todet)
      if ( ito.le.0 ) then
         call XWRITE(' Copying to new mission...', 10)
         call addmdb(totel, toinst, todet, ito, status)
         if ( status.ne.0 ) return
      endif

      do i = 1, mdbi_num
         int_mdb(i,ito) = int_mdb(i,ifrom)
      enddo
      do i = 1, mdbd_num
         dbl_mdb(i,ito) = dbl_mdb(i,ifrom)
      enddo
      do i = 1, mdbs_num
         str_mdb(i,ito) = str_mdb(i,ifrom)
      enddo

      return
      end
