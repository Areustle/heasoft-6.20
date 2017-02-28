C******************************************************************************
C SUBROUTINE:
C      		read_sgdat
C
C DESCRIPTION:
C      read columns of data from caldb FITS file for SG.DAT
C
C AUTHOR/DATE:
C      Srilal Weera  10/96
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

      subroutine read_sgdat(infile,col1name,col1array,nelem1,col2name,
     & col2array,nelem2,col3name,col3array,nelem3,col4name,col4array,
     & nelem4, col5name,col5array,nelem5, col6name,col6array,nelem6,
     & col7name,col7array,nelem7, col8name,col8array,nelem8,
     & col9name,col9array,nelem9, col10name,col10array,nelem10,
     & col11name,col11array,nelem11,col12name,col12array,nelem12,status)


      character*(*) infile
      character*(*) col1name,col2name,col3name,col4name,col5name
      character*(*) col6name,col7name,col8name,col9name,col10name
      character*(*) col11name,col12name
      integer col1num, col2num, col3num, col4num, col5num, col6num
      integer col7num, col8num, col9num, col10num, col11num, col12num
      integer  nelem1(*), nelem2(*), nelem3(*), nelem4(*), nelem5(*)
      integer  nelem6(*), nelem7(*), nelem8(*), nelem9(*), nelem10(*)
      integer  nelem11(*), nelem12(*)
      integer frow, felem
      integer col1array(*)
      real*4  col2array(*), col3array(*), col4array(*), col5array(*)
      real*4  col6array(*), col7array(*), col8array(*)
      real*4 col9array(*),col10array(*),col11array(*),col12array(*)
      character(160) fname
      character(80) context
      logical anyflg, inopen
      integer extnum,status, ftstat,iunit,block,htype, nullval 

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

C      print*,'readsgdat:*** infile', infile

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
      call cgetlun(iunit)
      call ftopen(iunit,fname,0,block,ftstat)
      if ( ftstat .ne. 0 ) then
         context = 'readsgdat: Unable to open infile '
         call fcerr(context)
         goto 999
      endif
      inopen = .true.

C   move to the extension in the input file
      call ftmrhd(iunit,extnum,htype,ftstat)
      if ( ftstat .ne. 0 ) then
         context = 'Error moving to extension number '
         write(context,1000) context, extnum
 1000    format(A34,I3)
         call fcerr(context)
         goto 999
      endif

c Reading data from COL1NAME (DATE) column
  	status = 0
      	call ftgcno(iunit,.false.,col1name,col1num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 1 name'
           call fcerr(context)
	   goto 999
      	ENDIF
          
      	nullval = 0
      	call ftgcvj(iunit,col1num,frow,felem, nelem1,
     &	nullval, col1array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 1 data'
          call fcecho(context)
	  goto 999
 	ENDIF


c Reading data from COL2NAME (DATE_JULIAN) column
  	status = 0
      	call ftgcno(iunit,.false.,col2name,col2num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 2 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col2num,frow,felem, nelem2,
     &	nullval, col2array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 2 data'
           call fcecho(context)
	  goto 999
 	ENDIF


c Reading data from COL3NAME (SOLAR_FLUX) column

  	status = 0
      	call ftgcno(iunit,.false.,col3name,col3num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 3 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col3num,frow,felem, nelem3,
     &	nullval, col3array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 3 data'
           call fcecho(context)
	  goto 999
 	ENDIF


c Reading data from COL4NAME (AVERAGE_SOLAR_FLUX) column
  	status = 0
      	call ftgcno(iunit,.false.,col4name,col4num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 4 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col4num,frow,felem, nelem4,
     &	nullval, col4array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 4 data'
           call fcecho(context)
	  goto 999
 	ENDIF


c Reading data from COL5NAME (KP1) column
  	status = 0
      	call ftgcno(iunit,.false.,col5name,col5num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 5 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col5num,frow,felem, nelem5,
     &	nullval, col5array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 5 data'
           call fcecho(context)
	  goto 999
 	ENDIF

c Reading data from COL6NAME (KP2) column
  	status = 0
      	call ftgcno(iunit,.false.,col6name,col6num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 6 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col6num,frow,felem, nelem6,
     &	nullval, col6array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 6 data'
           call fcecho(context)
	  goto 999
 	ENDIF


c Reading data from COL7NAME (KP3) column
  	status = 0
      	call ftgcno(iunit,.false.,col7name,col7num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 7 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col7num,frow,felem, nelem7,
     &	nullval, col7array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 7 data'
           call fcecho(context)
	  goto 999
 	ENDIF


c Reading data from COL8NAME (KP4) column
  	status = 0
      	call ftgcno(iunit,.false.,col8name,col8num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 8 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col8num,frow,felem, nelem8,
     &	nullval, col8array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 8 data'
           call fcecho(context)
	  goto 999
 	ENDIF


c Reading data from COL9NAME (KP5) column
  	status = 0
      	call ftgcno(iunit,.false.,col9name,col9num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 9 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col9num,frow,felem, nelem9,
     &	nullval, col9array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 9 data'
           call fcecho(context)
	  goto 999
 	ENDIF


c Reading data from COL10NAME (KP6) column
  	status = 0
      	call ftgcno(iunit,.false.,col10name,col10num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 10 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col10num,frow,felem, nelem10,
     &	nullval, col10array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 10 data'
           call fcecho(context)
	  goto 999
 	ENDIF


c Reading data from COL11NAME (KP7) column
  	status = 0
      	call ftgcno(iunit,.false.,col11name,col11num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 11 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col11num,frow,felem, nelem11,
     &	nullval, col11array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 11 data'
           call fcecho(context)
	  goto 999
 	ENDIF


c Reading data from COL12NAME (KP8) column
  	status = 0
      	call ftgcno(iunit,.false.,col12name,col12num,status)
      	If (status.NE.0) THEN
           context = ' Error reading column 12 name'
           call fcecho(context)
	   goto 999
      	ENDIF

      	nullval = 0
      	call ftgcve(iunit,col12num,frow,felem, nelem12,
     &	nullval, col12array,anyflg,status)
      	IF (status.NE.0) THEN
          context = ' Error reading column 12 data'
           call fcecho(context)
	  goto 999
 	ENDIF

 999  continue
      if ( ftstat .ne. 0 ) then
         call fcerrm(ftstat)
         ftstat = 0
      endif
      if ( inopen ) then
         call ftclos(iunit,ftstat)
         ftstat = 0
      endif
      return
      end

C----------End of read_sgdat subroutine-----------------------------------
