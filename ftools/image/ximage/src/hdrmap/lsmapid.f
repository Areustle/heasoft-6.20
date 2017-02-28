      subroutine lsmapid
      implicit none
c
c  List available values for MAPID
c
      include '../include/io.inc'
      include '../include/maxvals.inc'
      include 'header.inc'
c
c  Local variables
c
      integer di
      character(10) maxidx

      call xistr(MAX_SLOTS, maxidx, di)
      
      write(ZWRite,'(5a)') ' Available values for MAPID: MAP1 - MAP',
     &                     maxidx(1:di), ', EXMAP1 - EXMAP',
     &                     maxidx(1:di)
      call XWRITE(ZWRite, 10)
      write(ZWRite,'(29x,a)') 'CUR, EXCUR, DIS'
      call XWRITE(ZWRite, 10)
      return
      end

