      subroutine addmdb(tel, inst, det, itel, status)
      implicit none
c
c  Adds a telescop/instrume/detnam to end of mission db
c
c  I  tel    (s)  Telescope
c  I  inst   (s)  Instrument
c  I  det    (s)  Detector name
c  O  itel   (i)  Index for new entry
c  O  status (i)  Error flag (0=OK)
c
      character*(*) tel, inst, det
      integer*4 itel, status

      include '../include/startup.inc'
c
c  Local variables
c
      character(80) uptel, upinst, updet

      status = 0

      if ( ZEXpnum.ge.ZEXpmax ) then
         call XWRITE(' Maximum number of missions exceeded', 10)
         status = -1
         return
      endif
      ZEXpnum = ZEXpnum + 1
      itel = ZEXpnum
      uptel = tel
      call upc(uptel)
      ZTElescop(itel) = uptel
      upinst = inst
      call upc(upinst)
      ZINstrume(itel) = upinst
      updet = det
      call upc(updet)
      ZDEtnam(itel) = updet

      return
      end
