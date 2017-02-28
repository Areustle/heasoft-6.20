      subroutine rstringfile(filename,inum,strings,abort)
c        implicit none

      integer isiz
      parameter (isiz=999)
      character*(*) filename
      character(80) strings(isiz)
      integer inum,lup,status,i
      logical abort
        
      status=0
      inum=0
      i=0
        
c     Assign a unit file number used in inputting file.
      call ftgiou(lup,status)
      if(status.ne.0)then
        call fcecho('Error getting input unit number')
        call fcecho('Setting to logical unit 10')
        status=0
        lup=10
      endif

      OPEN(lup,file=filename,status='UNKNOWN',err=20)
      
c      read header line
10    continue
      i=i+1
      read (lup,80,err=20,end=20) strings(i)
      inum=i
      goto 10
20    continue

      i=i-1

      if(inum.le.0)abort=.TRUE.

      if(abort)then
        call fcecho('Could not extract bitmask information.')
        call fcecho('Check the file that you input:')
        call fcecho(filename)
      endif

      close(lup)

      call ftfiou(lup,status)
      if(status.ne.0)then
        call fcecho('Error freeing input unit number')
        status=0
      endif
      
      return
80    format(A80)

      end
      
          
