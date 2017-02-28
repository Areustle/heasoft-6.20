C******************************************************************************
C SELECTOR TASK:
C      fhelp
C
C FILE:
C      fhelp.f
C
C DESCRIPTION:
C       display help pages in a reasonable way
C
C AUTHOR/DATE:
C       James Kent Blackburn 11/18/92
C
C MODIFICATION HISTORY:
C       4/2/93 (EAG) - More prompt to boolean, alway true to begin with
C       3/8/94 (EAG) 2.8a - Made buffer sizes consistent
C       8/24/94 (EAG) 3.0a - open output file with faopen, add clobber
C       10/3/94 (EAG) 3.1a - rename fprintemp to pgfout, q to quit paging
C
C NOTES:
C      NOT SUPPORTED IN IRAF
C
C USAGE:
C      HOST: hfhelp
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      task    - input FITS file and extension number
C      path    - directory path
C      nrows   - number of rows in a page
C      outfile - output FITS file and extension number
C
C CALLED ROUTINES:
C      subroutine ghelp - gets parameters from environment
C      subroutine fhpage - gets column names from FITS file
C
C******************************************************************************

      subroutine fhelp
      character(80) task
      character(160) path,outfile
      integer    nrow
      character(40) taskname
      common /task/ taskname

      taskname = 'fhelp3.1a'
C get parameters from par file
      call ghelp(task,path,nrow,outfile)

C read in and display column names
      call fhpage(task,path,nrow,outfile)
      return
      end


C******************************************************************************
C SUBROUTINE:
C      ghelp
C
C DESCRIPTION:
C      gets parameters from the parameter file 
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/18/92
C
C MODIFICATION HISTORY:
C       
C
C NOTES:
C       ghelp uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call ghelp(task,path,nrow,outfile)
C
C ARGUMENTS:
C      task  - input FITS file and extension number
C      path    - directory path
C      nrows   - number of rows in a page
C      outfile - output FITS file and extension number
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      irafsts - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine ghelp(task,path,nrow,outfile)

      character*(*) task,path,outfile
      integer      nrow
      character(80) context
      integer      irafsts

C ---   Initialize variables 
      irafsts = 0

C ---   Get the name of the input FITS file ---

      call uclgst('task',task,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get task parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the path to the help file ---

      call uclgst('path',path,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get PATH parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the value of the NROW variable

      call uclgsi('nrows',nrow,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get NROW parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the output file ---

      call uclgst('outfile',outfile,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue
      return
      end


C******************************************************************************
C SUBROUTINE:
C      fhpage
C
C DESCRIPTION:
C      This subroutine actually does the reading of rows  
C      from the help file and copys them to the OUTFILE 
C      based on the values of the parameters 
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/18/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C       
C
C USAGE:
C      call fhpage(task,path,nrow,outfile)
C
C ARGUMENTS:
C      task  - input FITS file and extension number
C      path    - directory path
C      nrows   - number of rows in a page
C      outfile - output FITS file and extension number
C
C PRIMARY LOCAL VARIABLES:
C      iunit    - logical unit number for help file
C      ounit    - logical unit number for output file
C      filename - path and filename
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fishcl - display or write column names
C
C******************************************************************************

      subroutine fhpage(task,path,nrow,outfile)

      character*(*) task,path,outfile
      integer       nline,nrow,iunit,ounit,lpath,lfile,fcstln, status
      character(80)  hlprec
      character(160) filename, context
      character(1) contlp

      status = 0
      lpath = fcstln(path)
      lfile = fcstln(task)
      filename = path(:lpath)//task(:lfile)//'.txt'
      iunit = 15
      call faopen (iunit, filename, 1, 0, status)
      if (status .ne. 0) then
 10      context = 'Could not open help file:' // filename
         call fcerr(context)
         goto 999
      endif

      if ( outfile .ne. 'STDOUT' ) then
         ounit = 16
         call faopen (ounit, outfile, 2, 0, status)
         if (status .ne. 0) then
 30         context = 'Could not open outfile; may exist? ' // outfile
            call fcerr(context)
            goto 999
         endif
      endif

      nline = 1
 50   read(iunit,1001,end=999) hlprec
      if (outfile .eq. 'STDOUT' ) then
         call fcecho(hlprec)
         if ( nline .lt. nrow ) then
            nline = nline + 1
         else
            nline = 1
 55         call uclgst ('more', contlp, status)
c           call fcecho('(*press p<return> to page; q<return> to quit*)')
c   55      read(5,1002)contlp
c           if ( contlp(1:1) .eq. 'q') goto 999
            call ftupch (contlp)
            if ((contlp .eq. 'Q') .or. (contlp .eq. 'N') .or.
     &           (contlp .eq. 'F')) goto 999
c           if ( contlp(1:1) .ne. 'p') goto 55
         endif
         goto 50
      else
         write(ounit,1002) hlprec
         goto 50
      endif

 999  continue

C reset the more parameter to page
      call uclpst ('more', 'yes', status)
      close(iunit)
      if ( outfile .ne. 'STDOUT' ) then
         close(ounit)
      endif
 1000 continue
 1001 format(a80)
 1002 format(a)
      return
      end

C******************************************************************************
