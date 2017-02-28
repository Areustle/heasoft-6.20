      subroutine flatback(Mapid,Flatval,Ibbace,Status)
      implicit none
c
c  Fill background common with flat value
c
c  I  mapid      (s)  Map id string
c  I  flatval    (r)  Value for flat background
c I/O ibbace     (i)  Box size in image pixels
c  O  status     (i)  Error flag  (0 = OK)
c
      character*(*) mapid
      real*4 Flatval
      integer*4 Ibbace, Status

      INCLUDE '../include/io.inc'
      INCLUDE 'backgd.inc'
c
c  Local variables
c
      integer*4 i, szx, szy
      real*8 zmx, zmy
c
      Status = 0

      call gheadi(mapid, 'SZX', szx, 0, status)
      call gheadi(mapid, 'SZY', szy, 0, status)
      call gheadd(mapid, 'ZMX', zmx, 0, status)
      call gheadd(mapid, 'ZMY', zmy, 0, status)

      if ( ibbace.eq.0 ) ibbace = MIN(szx,szy)/4.
      NBX = float(szx)/float(ibbace)
      NBY = float(szy)/float(ibbace)
      NBOxes = NBX*NBY
      if ( NBOxes.gt.MAXbox ) then
         call XWRITE(' Too many background boxes', 10)
         NBOxes = 0
         status = -1
         return
      endif

      BACk = Flatval*(zmx*zmy)
      BNEw = BACk

      call xwrite(' Generating flat background boxes', 10)
      write(ZWRite,*) ' ', NBX, ' x ', NBY, ' boxes'
      call rmvxbk(ZWRite(2:))
      call xwrite(ZWRite, 15)
      write(ZWRite,*) ' Background (cnts/image pix)', BACk
      call xwrite(ZWRite, 15)

      do i = 1, NBOxes
         BB(i) = BACk
         BB_sig(i) = 0.
         BB_flags(i) = 0
         BB_npix(i) = ibbace*ibbace
         WW(i) = 1.
      enddo

      return
      end
