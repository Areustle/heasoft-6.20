      subroutine read_levels(filename,maxlevs,numlevs,levels,status)

      implicit none
c
c  Read levels from a file. The file format is ascii containing one column 
c  of numbers in increasing order each representing a level value. 
c 
c  I  filename     (c) Levels file
c  I  maxlevs      (i) Maximum number of levels
c  O  numlevs      (i) Number of levels read
c  O  levels       (r) Read-in levels
c  O  status       (i) Error flag (0=OK)
c
      integer maxlevs, numlevs
      real*4 levels(*)
      character*(*) filename
      integer status
c
c  Local variables
c
      integer lun, LENACT
      logical foundit
      character(255) ds
c
      status = 0
      numlevs = 0
      call getlun(lun)
      inquire (FILE=filename,EXIST=foundit)
      if ( filename.ne.' ' .and. foundit) then

          call txinit(status)
          call txrdcol(filename, 1, maxlevs, levels, numlevs, status)
          if (status .ne. 0) then
              call xwrite (' Failed to read levels',10)
              return
          end if

          write(ds,'(a,i2)') ' Levels read from file: ',numlevs
          CALL XWRITE(ds,10)

      else
         write(ds,'(1x,2a)') filename(:LENACT(filename)), 
     &                    ' does not exist.'
         call XWRITE(ds,10)
         status = -1
      endif
      return
      end
