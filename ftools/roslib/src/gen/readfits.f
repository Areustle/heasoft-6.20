C******************************************************************************
C SUBROUTINE:
C      		readfits
C
C DESCRIPTION:
C      read a column of data from a FITS file
C
C AUTHOR/DATE:
C      Srilal Weera  09/96
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C
C USAGE:
C      call readfits(infile,col1name,col1array,nelem1,
C		col2name,col2array,nelem2, status)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      col1(2)name - name of the column to be read 
C      col1(2)array - retrieved data array corresponding to col1name 
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C      fname   - input fits file name
C      context - comment string 
C      extname - extension name
C
C CALLED ROUTINES:
C      subroutine fcpars - parse filename and extension from infile
C      subroutine ft____ - FITSIO routines
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine readfits(infile,col1name,col1array,nelem1,col2name,
     &		col2array,nelem2, status)

      character*(*) infile,col1name,col2name
      integer col1num, col2num
      integer  nelem1(*), nelem2(*)
      integer frow, felem
      real col1array(*), col2array(*)
      character(160) fname
      character(80) context
      logical anyflg, inopen
      integer extnum,status, ftstat,iunit1,block,htype, nullval 

C   initialize variables
      status = 0
      ftstat = 0
      block = 0
      frow = 1
      felem = 1
      nullval = 0
      anyflg = .false.
      inopen = .false.
      context = ' '

c      print*,'*** infile', infile

C   get the filename and extension
      call fcpars(infile,fname,extnum,ftstat)

      if (extnum .eq. -99) extnum = 1

C   if the extension is less than 1 then give an error and exit
      if (extnum .lt. 1) then
         context = 'Primary extension not supported'
         call fcerr(context)
         goto 999
      endif

C   open the input FITS file
	call cgetlun(iunit1)
      call ftopen(iunit1,fname,0,block,ftstat)
      if ( ftstat .ne. 0 ) then
         context = 'readfits: Unable to open infile:'
         call fcerr(context)
         goto 999
      endif
      inopen = .true.

C   move to the extension in the input file
      call ftmrhd(iunit1,extnum,htype,ftstat)
      if ( ftstat .ne. 0 ) then
         context = 'Error moving to extension number '
         write(context,1000) context, extnum
 1000    format(A34,I3)
         call fcerr(context)
         goto 999
      endif

c Reading data from COL1NAME column
  	status = 0
      	call ftgcno(iunit1,.false.,col1name,col1num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column name'
           call fcerr(context)
	   goto 999
      	ENDIF
          

      	nullval = 0
      	call ftgcve(iunit1,col1num,frow,felem, nelem1,
     &	nullval, col1array,anyflg,status)


      	IF (status.NE.0) THEN
          context = ' Error reading column data'
          call fcerr(context)
	  goto 999
 	ENDIF

c Reading data from COL2NAME column
  	status = 0
      	call ftgcno(iunit1,.false.,col2name,col2num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column name'
           call fcerr(context)
	   goto 999
      	ENDIF
          
      	nullval = 0
      	call ftgcve(iunit1,col2num,frow,felem, nelem2,
     &	nullval, col2array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column data'
          call fcerr(context)
	  goto 999
 	ENDIF

 999  continue
      if ( ftstat .ne. 0 ) then
         call fcerrm(ftstat)
         ftstat = 0
      endif
      if ( inopen ) then
         call ftclos(iunit1,ftstat)
         ftstat = 0
      endif
      return
      end

C----------End of readfits subroutine-----------------------------------
