      subroutine deletefile(filename,status)
      implicit none 
C
C      This is taken verbatim from the fitsio cookbook.
C    See:
C   http://heasarc.gsfc.nasa.gov/docs/software/fitsio/cookbook/cookbook.html
C
C
C      A simple little routine to delete a FITS file

       integer status,unit,blocksize
       character*(*) filename

C      simply return if status is greater than zero
       if (status .gt. 0)return

C      Get an unused Logical Unit Number to use to open the FITS file
       call ftgiou(unit,status)

C      try to open the file, to see if it exists
       call ftopen(unit,filename,1,blocksize,status)

       if (status .eq. 0)then
C         file was opened;  so now delete it 
          call ftdelt(unit,status)
       else if (status .eq. 103)then
C         file doesn't exist, so just reset status to zero and clear errors
          status=0
          call ftcmsg
       else
C         there was some other error opening the file; delete the file anyway
          status=0
          call ftcmsg
          call ftdelt(unit,status)
       end if

C      free the unit number for later reuse
       call ftfiou(unit, status)
       end
