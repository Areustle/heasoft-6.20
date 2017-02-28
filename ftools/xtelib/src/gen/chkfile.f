
***************************************************************************** 
C
C      chkfile
C
C
C DESCRIPTION: 
C     Checks to see if input files are more than one and perform several
c     checks to make sure that the files are present and that they are
c     all compatible with each other and can be processed. Adapted from
c      subroutines in the pre-existing FTOOL fmrgmsk.f
C
C AUTHOR:  
C      Brian K. Elza 10/94
C
C MODIFICATION HISTORY:
C	None
C
C ARGUMENTS:
C     infile 	- input FITS file and extension number or file containing
c                 a list of files with an @ sign prepended.
C     files     - output array containing the file names from 
c     chkit     - input logical to determine if ALL XTE specific checks
c                 are done
c     no        - output number of files that are in the input file
C     status    - output status of results, did everything work in fitsio
c     abort     - output logical telling if nothing useful was found. If
c                 no files could be found than we abort. 
C
C PRIMARY LOCAL VARIABLES:
c      negflag   - were we able to find additional files?
c
c SUBROUTINE CALLS
c     fcgcls - parse input to get actual input file names
c     fcecho - echo out the string to STDOUT
c     fcerr  - echo out error message to STDERR
c     chkword- checks to see if supplied keywords are identical in
c              all of the files that is supplied to it. This can be
c              turned off via CHKIT for XTE specific checks. 
C
***************************************************************************** 

        subroutine chkfile(infile,files,chkit,no,status,abort)
c        implicit none
        character*(*) infile
        parameter (isiz=999)
        character(160) files(isiz)

        integer      no
        integer      status 
	logical abort,chkit,negflag

C --- Check to see if the input file is a single file or
C --- if it is a file which contains filenames.
C --- "@" at the first character position  denotes that the value
C --- contained in INFILE denotes a filename containing filenames.

        status = 0

C find the file(s) or list of files in the first input string 
	if (infile .ne. ' ') then
		call fcgcls (infile, files, no, negflag)
	else
		no = 0
	endif

c      print*,'no is ', no
	if (no .gt. ISIZ) then
		call fcecho ('Too many files requested, using 999')
		no = ISIZ
	else if (no .le. 0) then
		call fcerr ('No input files found. Cannot continue')
		status = 1
		return
	endif

c      If we are to performs checks then do them and print warnings if
c      necessary. 
        if(chkit)then
           if (no.gt.1) then
c      
c      Check multiple input files to be sure that they are compatible.
c      The fields checked  are given below:
c
              call chkword(files,'TELESCOP',no,status,abort)
              call chkword(files,'APPID',no,status,abort)
              call chkword(files,'OBJECT',no,status,abort)

           endif
c
c      The following two calls check to see that the type of
c      data is scientific array data AND the instrument used
c      to collect the information was either PCA or HEXTE.
c        
           call chkword(files,'EXTNAME',no,status,abort)
           call chkword(files,'INSTRUME',no,status,abort)
        endif
        return
        end

