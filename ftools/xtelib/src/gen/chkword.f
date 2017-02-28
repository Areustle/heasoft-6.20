
***************************************************************************** 
C      chkword
C
C DESCRIPTION: 
C      Checks to see if supplied keywords are same or different in 
C      supplied files. Assumes that we are scanning a BINTABLE file.
C
C AUTHOR:  
C      Brian K. Elza 10/94 
C
C MODIFICATION HISTORY:
C	None
C
C
C ARGUMENTS:
c
C     files    - input array containing the file names must < 100
C     keyword  - input this is the keyword for which the FITS file is searched
C     no       - input number of files in files
C     status    - output status of results, did everything work in fitsio
c     abort     - output logical telling if nothing useful was found. If
c                 no files could be found than we abort. 

C     PRIMARY LOCAL VARIABLES:   
c     file1    - first file in list
c     filen    - 2nd, 3rd,... or nth file in list
c     xtend    - data type of unit number checked
C     word1    - returned word1 value for KEYWORD of the first file
c     wordn    - returned wordn value for KEYWORD of the nth file
c     contxt   - error message
c     cval     - conversion of integer to *20 sting to print
c     filenum  - character(3) version of cval
c     
c SUBROUTINE CALLS
c     ftgiou - get a free logical unit number from a file
c     fcpars - parse a file name into file name and extension
c     ftopen - open a FITS file
c     ftclos - close a FITS file
c     ftmahd - skip to a particular FITS extension
c     ftgkys - read a particular KEYWORD string value
c     ftfiou - free up a logical unit number for reuse
c     fcgcls - parse input to get actual input file names
c     fcecho - echo out the string to STDOUT
c     fcerr  - echo out error message to STDERR
c     fcerrm - write out the error message associated with a status
c 
*****************************************************************************

      subroutine chkword(files,keyword,no,status,abort)

      integer no,extnum,xtend
      character*(*) keyword
      character(160) files(no),file1,filen
      character(80) word1, wordn
      character(80) contxt
      character(20) cval
      character(3) filenum

      integer status,iunit1,iunitn,block,i
      logical ok,abort

      ok = .FALSE.
      status = 0

c      Assign a unit file number used in inputting file.
      call ftgiou(iunit1,status)
      if(status.ne.0)then
        call fcecho('Error getting input unit number')
        call fcecho('Setting to logical unit 10')
        status=0
        iunit1=10
      endif

c      Assign a unit file number used in inputting file.
      call ftgiou(iunitn,status)
      if(status.ne.0)then
        contxt='Error getting input unit number'
        call fcecho(contxt)
        contxt='Setting to logical unitn 11'
        call fcecho(contxt)
        status=0
        iunitn=11
      endif

c      Parse the filename read in and get the extension number
      call fcpars(files(1),file1,extnum,status)

c      If the extension number isn't set then set it to 2
      if (extnum .eq. -99) extnum = 1
        
c      Open the first input file 
      call ftopen(iunit1,file1,0,block,status)

      if (status .ne. 0) then
        contxt = 'unable to open first file in infile'
        call fcerrm(status)
        call fcerr(contxt)
        call ftclos(iunit1,status)
        return
      endif

c      Since we are interested in searching the extension
c      BINTABLE we have to move to the proper data unit in the
c      file. This is done by use of ftmahd(unit,data#,type,status)
c      where data# is the data unit number to go to (here the second)
c      and where type 0= primary HDU, 1= ASCII table, 2=Binary table.

c     Skip down to a particular extension number within a fits file. 
      call ftmahd(iunit1,extnum+1,xtend,status)

c      Read in the value that goes with the assigned keyword
      call ftgkys(iunit1,keyword,word1,contxt,status)
      if(status.ne.0)then
        contxt='Could not find keyword'
        call fcecho(contxt)
        call fcecho(keyword)
        status=0
      endif

c      Close up all files that were opened to obtain this
c      initial information
      call ftclos(iunit1,status)
      if(status.ne.0)then
        contxt='Error closing other input files'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      if(no.eq.1)then

        if(keyword.eq.'EXTNAME')then
          if(word1.ne.'XTE_SA')then
            call fcerr('ERROR - MUST be Scientific Array Data')
            call fcerr('Check input file ! ABORTING!!!')
            abort=.TRUE.
            return
          endif
        endif

        if(keyword.eq.'INSTRUME')then
          if((word1.ne.'PCA').and.
     &       (word1.ne.'HEXTE'))then
            call fcerr('ERROR - MUST be PCA or HEXTE data')
            call fcerr('Check input file ! ABORTING!!!')
            abort=.TRUE.
            return
          endif
        endif
        
      endif
        
      do 10 i = 2, no
           
        call fcpars(files(i),filen,extnum,status)
        if(status.ne.0)then
          contxt='Could not parse the first file name'
          call fcerr(contxt)
          call fcerrm(status)
          status=0
        endif

c     If the extnum is not included then set it to the second
        if (extnum .eq. -99) extnum = 1

c     Open the file
        call ftopen(iunitn,filen,0,block,status)
        if (status .ne. 0) then
          call fcerrm(status)
          write (contxt, *) 'Unable to open file number', i
          call fcerr(contxt)
          call ftclos(iunitn,status)
          if(status.ne.0)then
            contxt='Error closing input number'
            call fcecho(contxt)
            status=0
          endif
      
          call ftfiou(iunitn,status)
          if(status.ne.0)then
            contxt='Error freeing input unit number'
            call fcecho(contxt)
            status=0
          endif
          return
        endif

c      Get the other strings assigned to specific keyword
        call ftmahd(iunitn,extnum+1,xtend,status)
        if(status.ne.0)then
          contxt='Error moving to extnum'
          call fcerr(contxt)
          call fcerrm(status)
          status=0
        endif

        call ftgkys(iunitn,keyword,wordn,contxt,status)           
        if(status.ne.0)then
          contxt='Could not find keyword'
          call fcecho(contxt)
          call fcecho(keyword)
          status=0
        endif


c      Close up all files that were opened to obtain this
c      initial information
        call ftclos(iunitn,status)
        if(status.ne.0)then
          contxt='Error closing other input files'
          call fcecho(contxt)
          call fcerrm(status)
          status=0
        endif

        if(keyword.eq.'EXTNAME')then
          if(word1.ne.'XTE_SA'.or.wordn.ne.'XTE_SA')then
            call fcerr('ERROR - MUST be Scientific Array Data')
            call fcerr('Check input files ! ABORTING!!!')
            abort=.TRUE.
            return
          endif
        endif
        
        if(keyword.eq.'INSTRUME')then
          if((word1.ne.'PCA'.or.wordn.ne.'PCA').and.
     &       (word1.ne.'HEXTE'.or.wordn.ne.'HEXTE'))then
            call fcerr('ERROR - MUST be PCA or HEXTE data')
            call fcerr('Check input files ! ABORTING!!!')
            abort=.TRUE.
            return
          endif
        endif

c     Do the comparison and see if the keywords from each file matches.
        ok =(word1 .eq. wordn)

c     If they do not match then issue warnings. 
        if (.not. ok) then
          call fcecho('WARNING KEYWORDs of files do not match')
          call fcecho('The KEYWORD searched for was')
          call fcecho(keyword)
          call fcecho('The values read were ')
          call fcecho(word1)
          call fcecho('for the first file read in and')
          call fcecho(wordn)
          call fcecho('for file number')
          call fti2c(i,cval,0)
          filenum=cval(18:20)
          call fcecho(filenum)
          
        endif
           
10    continue

c     Free up the logical unit number assigned to this file. 
      call ftfiou(iunit1,status)
      if(status.ne.0)then
        contxt='Error freeing output unit number'
        call fcecho(contxt)
        status=0
      endif

c     Free up the logical unit number assigned to all files.
      if(no.gt.1)then
        call ftfiou(iunitn,status)
        if(status.ne.0)then
          contxt='Error freeing output unit number'
          call fcecho(contxt)
          status=0
        endif
      endif
              
      return
      end
