C**********************************************************************
C SELECTOR TASK:
C      mkgtif
C
C FILE:
C      mkgtif.f
C
C DESCRIPTION:
C      Reads a FITS file and creates a goodtime extension data file for it
C
C AUTHOR/DATE:
C      Richard Bentley  13Aug96
C
C MODIFICATION HISTORY:
C      05Jan98: MJT: changed ftgtbh/ftgbnh to ftghtb/ftghbn (obsolete)
C      10Jun98: MJT: changed nullval in ftgcvd() to 0.0d0 [v1.0b]
C
C NOTES:
C      mkgtif supported in IRAF and HOST environments
C
C USAGE:
C      HOST: call mkgtif
C      IRAF: task mkgtif
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      outfile - output FITS file
C
C CALLED ROUTINES:
C      subroutine gmkgti - gets parameters from parameter file
C      subroutine fimgti - select out rows 
C
C***********************************************************************

      subroutine mkgtif
      implicit none
      character(160)  infile, outfile
      integer        nrow
      character(40)   taskname
      common /task/ taskname
C----------------------------------------------------------------------
C Clear the message stack
      call ftcmsg

      taskname = 'mkgtif1.0b'
C  get parameters from parameter file
      call gmkgti(infile,outfile,nrow)

C  Read data from the input FITS file and create the goodtime extension
C  data file:
      call fimgti(infile,outfile,nrow)

      return
      end


C***********************************************************************
C SUBROUTINE:
C      gmkgti
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Richard Bentley    13Aug96
C
C MODIFICATION HISTORY:
C
C NOTES:
C       gmkgti uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gmkgti(infile, outfile, nrow)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output ASCII file
C      nrow    - number of rows to buffer from input file
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine fcerrm - echo error message to terminal
C
C***********************************************************************

      subroutine gmkgti(infile, outfile, nrow)
      implicit none
      character*(*) infile, outfile
      integer       nrow

      character(80) context
      integer status
C-----------------------------------------------------------------------
C  initialize variables
      status = 0

C  get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
         context = 'could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the output FITS file
      call uclgst('outfile',outfile,status)
      if (status .ne. 0) then
         context = 'could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

C  get the value of the nrow variable

      call uclgsi('nrows',nrow,status)
      if ( status .ne. 0 ) then
         context = 'Could not get nrows parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue
      if (status .ne. 0) then
         call fcerrm(status)
         stop
      endif

      return
      end


C**********************************************************************
C SUBROUTINE:
C      fimgti
C
C DESCRIPTION:
C     Create the goodtime extension data file 
C
C AUTHOR/DATE:
C      Richard Bentley, 13Aug96
C
C MODIFICATION HISTORY:
C
C NOTES:
C       
C
C USAGE:
C      call fimgti(infile,outfile,nrow)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output FITS file 
C      nrow    - number of rows to buffer from input file
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C      fname   - input fits file name
C      errstr  - concatenated error message
C      comment - comment string found in FITS file
C      maxcl   - maximum number of columns supported by software
C      inopen  - input file open flag
C      outopen - output file open flag
C      extnum  - extension number in input FITS file
C      ftstat  - fitsio library call return status
C      iunit   - input file unit number
C      ounit   - output file unit number
C      block   - fits file block size
C      htype   - fits file header type
C      rowlen  - length of FITS table in bytes
C      nrows   - number of records in FITS table
C      tfields - total number of columns in FITS table
C      varidat - size in bytes of variable data area 
C      ttype   - array of column names
C      tform   - array of column formats
C      tunit   - array of column units
C      tbcol   - column number of first char in each field
C      extname - extension name
C
C CALLED ROUTINES:
C      subroutine fcpars - parse filename and extension from infile
C      subroutine ftopen - open a FITS file
C      subroutine ftgtbh - get ascii table header keywords
C      subroutine ftgbnh - get binary table header keywords
C      subroutine ftclos - close a FITS file
C      subroutine fcstln - length of string once padded spaces removed
C
C**********************************************************************
      subroutine fimgti(infile, outfile, nrow)

      implicit none
C ARGUMENTS:
      character*(*) infile,outfile
      integer       nrow
C PRIMARY LOCAL VARIABLES:
      integer       maxcl
      parameter ( maxcl = 999 )
      character(160) fname
      character(80) context,errstr,comment
      logical inopen,outopen
      integer extnum,ftstat,iunit,ounit,block,htype
      integer rowlen,nrows
      integer tfields,varidat
      integer flen
      character(70) ttype(maxcl),tform(maxcl)
      character(70) tunit(maxcl)
      character(70) extname
      integer fcstln,tbcol(maxcl)
      common /hninfo/ iunit,htype,nrows
      common /hcinfo/ ttype,tform
      character(40)   taskname
      common /task/ taskname
C vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      integer TimeCol
      PARAMETER (TimeCol=1)
      integer frow
      integer felem
      integer nelements
      real*8 Time
      logical anyf
      integer naxis2
      character(20) snaxis2
      integer IDF, lastIDF, start, stop
C ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C   initialize variables
      ftstat = 0
      iunit = 15
      ounit = 16
      block = 0
      inopen = .false.
      outopen = .false.
      context = ' '
      errstr = ' '

C   get the filename and extension
      call fcpars(infile,fname,extnum,ftstat)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

      flen = fcstln(fname)

C   if the extension is 0 then give an error and exit
      if (extnum .lt. 1) then
         context = 'Primary extension not supported'
         call fcerr(context)
         goto 999
      endif

C   open the input FITS file
      call ftopen(iunit,fname,0,block,ftstat)
      if ( ftstat .ne. 0 ) then
         context = 'Unable to open infile'
         call fcerr(context)
         goto 999
      endif 
      inopen = .true.

C   open the output goodtime extension data file
      open (unit=ounit,file=outfile,status='new',iostat=ftstat)
      if (ftstat .ne. 0) then
         context = 'unable to open outfile'
         call fcerr(context)
         goto 999
      endif
      outopen = .true.

C   move to the extension in the input file
      call ftmahd(iunit,extnum+1,htype,ftstat)
      if ( ftstat .ne. 0 ) then
         errstr = 'Error moving to extension number '
         write(context,1000) errstr, extnum
 1000    format(A34,I3)
         call fcerr(context)
         goto 999
      endif

C   get extension header's keywords depending on extension type
      if ( htype .eq. 1 ) then
c        call ftgtbh(iunit,rowlen,nrows,tfields,ttype,tbcol,
c    &        tform,tunit,extname,ftstat)
c
c   MJT 05Jan98: OBSOLETE routine: should call ftghtb instead
c
         call ftghtb(iunit,0,rowlen,nrows,tfields,ttype,
     &        tbcol,tform,tunit,extname,ftstat)
      else if ( htype .eq. 2 ) then
c        call ftgbnh(iunit,nrows,tfields,ttype,tform,tunit,
c    &        extname,varidat,ftstat)
c
c   MJT 05Jan98: OBSOLETE routine: should call ftghbn instead
c
         call ftghbn(iunit,-1,nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,ftstat)
      else 
         context = 'Extension type not supported'
         call fcerr(context)
         goto 999
      endif

C vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

      call ftgkey(iunit, 'NAXIS2', snaxis2, comment, ftstat)
      read (snaxis2, 222) naxis2
 222  format (i6)

      felem = 1
      nelements = 1
      lastIDF = 0
      do 400 frow = 1,naxis2

        call ftgcvd(iunit, TimeCol, frow, felem, nelements,
     &              0.0d0, Time, anyf, ftstat)
        if (ftstat .ne. 0) then
          context = ' Error reading Time'
          call fcerr (context)
          goto 999
        endif

C If IDF has changed, write a line to the GTI file:
        IDF = int(Time/16)
        if (lastIDF .ne. IDF) then
          lastIDF = IDF
          start = IDF * 16
          stop = (IDF + 1) * 16
          write (ounit, 270) start, stop
 270      format(i10,i10)
        endif

 400  continue

 999  continue
      if ( inopen ) then
         call ftclos(iunit,ftstat)
         ftstat = 0
      endif
      if ( outopen ) then
         call ftclos(ounit,ftstat)
         ftstat = 0
      endif
      return
      end

