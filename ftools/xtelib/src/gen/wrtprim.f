








C ***************************************************************************
C SUBROUTINE:
C      wrtprim
C
C DESCRIPTION:      
C       write the primary extension for the output FITS file
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USEAGE:
C      call wrtprim(iunit, ounit, inopen, outopen, outfile, author)
C      
C ARGUMENTS:
C      iunit 	 - logical unit of input FITS file
C      ounit 	 - logical unit of output FITS file
C      inopen    - logical flag for open status of input FITS file
C      outopen   - logical flag for open status of output FITS file
C      outfile	 - name of output FITS file
C      author    - name of program creating the output file
C      
C PRIMARY LOCAL VARIABLES:
C      mkywd     -
C      context   - string for local error messages
C      ftstatus  - fitsio status flag
C
C SUBROUTINES CALLED:
C      subroutine ftinit - open and initialize a new FITS file
C      subroutine ftmahd - absolute move to extension
C      subroutine ftcopy - copy extension from input file to output file
C      subroutine ftpdat - put the DATE keyword into a header
c      subroutine ftmkys - modify header keyword value
C      subroutine ftclos - close a FITS file opened with ftinit or ftopen
C      subroutine fcerr  - prints message to stderror device
C      
C **************************************************************************
      
      subroutine wrtprim(iunit, ounit, inopen, outopen, outfile, author)

C      Variable declarations
      character*(*) outfile, author
      integer iunit, ounit
      logical inopen, outopen, ex
      
      character(8) keywrd
      character(80) context, cvalue
      integer htype, ftstatus
      integer mkywd, block

      ftstatus=0
      
c      Open the output FITS file
      inquire(FILE=outfile, EXIST=ex)
      if (ex) then
         open(UNIT=ounit,FILE=outfile,STATUS='unknown')
         close(UNIT=ounit,STATUS='delete')
      endif
      block = 1
      call ftinit(ounit,outfile,block,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to open outfile'
         call fcerr(context)
         go to 999
      endif
      outopen = .true.

c      Copy over the primary extension from the input FITS file
      call ftmahd(iunit,1,htype,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to move to new HDU'
         call fcerr(context)
         go to 999
      endif

      mkywd = 10
      call ftcopy(iunit,ounit,mkywd,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to copy primary extension'
         call fcerr(context)
         go to 999
      endif

c      Modify the AUTHOR/CREATOR keyword value
      keywrd = 'AUTHOR'
      cvalue = author//'/XTE/GOF'
      call ftmkys(ounit,keywrd,cvalue,'&',ftstatus)
      if (ftstatus .eq. 202) then
         ftstatus = 0
         keywrd = 'CREATOR'
         call ftmkys(ounit,keywrd,cvalue,'&',ftstatus)
         if (ftstatus .eq. 202) ftstatus = 0
      endif
      
c      Modify the DATE keyword (creation date of this file)
      callftpdat(ounit,ftstatus)

c      Error handling
999   continue
      if (ftstatus .ne. 0) then         
         call fcerrm(ftstatus)
         ftstatus = 0
         if (inopen) then
            call ftclos(iunit,ftstatus)
            call ftfiou(iunit,ftstatus)
         endif
         if (outopen) then
            call ftclos(ounit,ftstatus)
            call ftfiou(iunit,ftstatus)
         endif
      endif
      
      return
      end

