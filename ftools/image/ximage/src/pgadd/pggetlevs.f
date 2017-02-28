C   Subroutine PGGETLEVS
C   21/5/90    R. Harris
C   Description:  
C   Returns the level table values
C   status = 1 if table has been defined and data is ok otherwise, 0
C
C   12/12/97 - MSJ - Levels now stored in common, rather than device
C
   
      Subroutine pggetlevs(outnumlevs,levout,status)
      implicit none
      real levout(*)
      integer i, outnumlevs, status

      include 'level.inc'

      outnumlevs = NUMlevs
      if ( NUMlevs.gt.0 ) then
         do i = 1, NUMlevs
            levout(i) = levels(i)
         enddo
         status = 0
      else
         call xwrite(" No levels to retrieve", 5)
         status = 1
      endif

      return
      end
       
