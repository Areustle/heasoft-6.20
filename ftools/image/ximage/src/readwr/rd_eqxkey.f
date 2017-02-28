      subroutine rd_eqxkey(Lun,Equinox,Equimg,Status)

      implicit none
c
c  Reads equinox keyword. If does not exist, try epoch.
c    If not found, set to zero
c
c  I  Lun     (i)  Logical unit of open FITS file
c  I  Equinox (i)  XIMAGE Equinox
c  O  Equimg  (i)  Equinox read from FITS file
c  O  Status  (i)  Error flag (0=OK)
c
      integer Lun, Equinox, Equimg, Status
c
c  Local variables
c
      character(80) comment

      Status = 0
      Equimg = 0

      call ftgkyj(Lun,'EQUINOX',Equimg,comment,Status)
      if ( Equimg.lt.1900 .or. Equimg.gt.2050 ) then
         Status = 0
         call ftgkyj(Lun,'EPOCH',Equimg,comment,Status)
         if ( Equimg.lt.1900 .or. Equimg.gt.2050 ) then
            call XWARN(' Image equinox not found',10)
         else
            call XWARN(' EQUINOX keyword not found, using EPOCH',15)
         endif
      else
         call XWRITE(' Using EQUINOX keyword', 20)
      endif

      return
      end
