      subroutine get_telstr(Mapid, Telstr)
      implicit none
c
c  Return telescope id string, TELESCOP INSTRUME DETNAM
c
c   I  mapid   (c)  Map id
c   O  telstr  (c)  Telescope/Instrume/Detnam string
c
      character*(*) Mapid, Telstr

      include '../include/io.inc'
c
c  Local variables
c
      character(80) telescop, instrume, detnam
      integer ilen, status, LENACT

      telescop = ' '
      instrume = ' '
      detnam = ' '

      call gheads(Mapid, 'TELESCOP', telescop, 0, status)
      call gheads(Mapid, 'INSTRUME', instrume, 0, status)
      call gheads(Mapid, 'DETNAM', detnam, 0, status)

      if ( detnam.ne.' ' ) then
         write(ZWRite,'(a,1x,a,1x,a)') telescop(:LENACT(telescop)),
     &         instrume(:LENACT(instrume)), detnam(:LENACT(detnam))
      else
         write(ZWRite,'(a,1x,a)') telescop(:LENACT(telescop)),
     &                             instrume(:LENACT(instrume))
      endif

      ilen = LENACT(ZWRite)
      if (LEN(Telstr).lt.ilen) then
         Telstr = ZWRite(:LEN(Telstr))
      else
         Telstr = ZWRite(:ilen)
      endif

      return 
      end
