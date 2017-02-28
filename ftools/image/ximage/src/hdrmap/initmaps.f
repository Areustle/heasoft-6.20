      subroutine initmaps
      implicit none
c
c   Initialize map definitions
c
      include '../include/maxvals.inc'
      include '../include/mapdef.inc'
c
c  Local variables
c
      integer i, slen, status
      character*(MAX_IDSTR) tmpstr
      logical readonly, global


c   Assign names to slots
      do i = 1, MAX_SLOTS
         call xistr(i, tmpstr, slen)
         write(Mapids(i*2-1),'(2a)') 'MAP', tmpstr(1:slen)
         write(Mapids(i*2),'(2a)') 'EXMAP', tmpstr(1:slen)
      enddo
      Mapids(MAX_SLOTS*2+1) = 'BGMAP'
      do i = 1, MAX_TMPMAPS
         call xistr(i, tmpstr, slen)
         write(Mapids(MAX_SLOTS*2+1+i),'(2a)') 'TMPMAP', tmpstr(1:slen)
      enddo

c   Init map locking mechanism.  Only tracks TMPMAPs in use (for now?)
      do i = 1, MAX_MAPS
         MAPlock(i) = .FALSE.
      enddo

c   Set current map to MAP1
      call setcurmap('MAP1', status)

c   Set display map to none
      call setdismap(' ', status)

c   Set Tcl variable maxmaps to MAX_SLOTS
      readonly = .TRUE.
      global = .TRUE.
      call tclvari('maxmaps', MAX_SLOTS, readonly, global, status)

c   Initialize pointers
      do i = 1, MAX_MAPS
         P_Map(i) = -1
         MSZx(i) = 0
         MSZy(i) = 0
      enddo

      return
      end
