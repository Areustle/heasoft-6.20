C******************************************************************************
C FTOOLS TASK:
C      fcolpar
C
C FILE:
C      fcolpar.f 
C
C DESCRIPTION: 
C      Returns the column number corresponding to an input filename
C      and column name.
C
C AUTHOR:  
C      Srilal Weera   - 9/1/95
C
C MODIFICATION HISTORY:
C 5/21/98 Ning Gan 1.1- Replace ftmrhd(...,extnum,...) with ftmahd(..,extnum+1,..)
C 1/11/01 Ning Gan 1.2- If the column does not exist, return 0 instead
C                       and  don't print out the error message.
C                     - Implemented the exact parameter.
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      uclgst - parameter interface subroutine to read a text parameter
C      uclpst - parameter interface subroutine to write a text parameter
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine fcolpr

      integer funit, colnum
      parameter (funit=37)
      integer status,blksiz,extnum,hdutyp
      character(160) infile,filename
      character(80) context
      logical exact
      character(25) colname
      character(40) taskname
      common /task/ taskname

      taskname = 'fcolpar v1.2'
      status=0
      blksiz =0
      colnum = 0
      call ftcmsg

C       get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
         context = 'could not get infile parameter'
         call fcerr(context)
         goto 999
      endif


C       get the name of the column to read
        call uclgst('colname',colname,status)
        if (status .ne. 0) then
            context = 'could not get colname parameter'
            call fcerr(context)
            goto 999
        endif

C       get the exact parameter to read
        exact=.false.
        call uclgsb('exact',exact,status)
        if (status .ne. 0) then
            context = 'could not get exact parameter'
            call fcerr(context)
            goto 999
        endif

C       parse the output string to get the file name and extension number
      call fcpars(infile,filename,extnum,status)

C  default to 1st extension
      if (extnum .eq. -99) extnum = 1

      if (status .gt. 0 .or. extnum .lt. 0)then
         context='unable to parse the filename[extension]'
         call fcerr(context)
         go to 999
      end if

C       open the new FITS file with read access
      call ftopen(funit,filename,0,blksiz,status)
      if (status .gt. 0)then
         context='unable to open the FITS file'
         call fcerr(context)
         go to 998
      end if

C       move to the correct extension
      call ftmahd(funit,extnum+1,hdutyp,status)
      if (status .gt. 0)then
         context='unable to move to specified extension'
         call fcerr(context)
         go to 998
      end if

C       get the column number corresponding to the column name
      call ftgcno(funit,exact,colname,colnum,status)
      if (status .gt. 0 .or. colnum .eq. 0)then
c         context='unable to find specified column name'
c         call fcerr(context)
         status = 0
         colnum = 0
      end if

C       write the column number to the .par file
      call uclpsi('colnum',colnum,status)
      if (status .gt. 0)then
         context='unable to write column number'
         call fcerr(context)
         go to 999
      end if
      go to 999

 998  continue
C       print out the FITSIO error number and text string
      call fcerrm(status)

 999  continue
C       close the fits file 
      call ftclos(funit,status)
      end


