C******************************************************************************
C FTOOLS TASK:
C      fpartab
C
C FILE:
C      fpartab.f 
C
C DESCRIPTION: 
C      Write an IRAF parameter to a FITS table element  
C
C AUTHOR:  
C      William Pence  6/30/92
C
C MODIFICATION HISTORY:
C       EAG 1/22/93 - Added BN to format statments for VAX
C       EAG 11/26/93 - Added array specification to element value
C       EAG 3/2/94 2.8a - Set status to 0 before closing file
C                         Add test for string for SUN internal read problem
C       EAG 9/1/94 3.0a - Clear FITSIO error stack
C       NG  5/21/98 3.0b- Replace ftmrhd(...,extnum,...) with ftmahd(..,extnum+1,..) 
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine fpartb

      integer funit
      parameter (funit=37)
      integer status,blksiz,extnum,hdutyp,row,felem,colnum
      integer i,vend,nxrows
      character(160) fitfil,infil
      character(80) value,tval,context, elemnt
      character column*24,fmt*10
      double precision dvalue
      logical exact, lvalues(2)
      character(40) taskname
      common /task/ taskname

      taskname = 'fpartab3.0b'
      call ftcmsg
      status=0

C  get the value string
      call uclgst('value',value,status)
      if (status .ne. 0) then
         context = 'could not get value parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the input FITS file
      call uclgst('fitsfile',fitfil,status)
      if (status .ne. 0) then
         context = 'could not get fitsfile parameter'
         call fcerr(context)
         goto 999
      endif

C       get the name of the column
      call uclgst('column',column,status)
      if (status .ne. 0) then
         context = 'could not get column parameter'
         call fcerr(context)
         goto 999
      endif

C       get the row number
      call uclgsi('row',row,status)
      if (status .ne. 0) then
         context = 'could not get row parameter'
         call fcerr(context)
         goto 999
      endif

C       get the element number
      call uclgst('element',elemnt,status)
      if (status .ne. 0) then
         context = 'could not get element parameter'
         call fcerr(context)
         goto 999
      endif

C       parse the output string to get the file name and extension number
      call fcpars(fitfil,infil,extnum,status)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

      if (status .gt. 0)then
         context='unable to parse the filename[extension]'
         call fcerr(context)
         go to 999
      end if

C       open the FITS file with write access
      call ftopen(funit,infil,1,blksiz,status)
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

C       check for correct extension type
      if (hdutyp .lt. 1 .or. hdutyp .gt. 2)then
         context='invalid FITS extension type'
         call fcerr(context)
         go to 999
      end if

C       get the column number corresponding to the column name
      exact=.false.
      call ftgcno(funit,exact,column,colnum,status)
      if (status .gt. 0 .or. colnum .eq. 0)then
         context='unable to find specified column name'
         call fcerr(context)
         go to 998
      end if

C get the correct element value
      call elemparse (elemnt, funit, colnum, felem, status)
      if (status .ne. 0) goto 998

C       check that row number is within the legal range of the table
      call ftgkyj(funit,'naxis2',nxrows,context,status)
      if (row .gt. nxrows)then
         context='row number is out of bounds'
         call fcerr(context)
         go to 999
      end if

C       determine the type of parameter and write it to the table
      tval=value
      call ftupch(tval)
      if (tval .eq. 'Y' .or.
     &     tval .eq. 'YES' .or.
     &     tval .eq. 'T'  .or.
     &     tval .eq. 'TRUE')then
C               this is a 'TRUE' logical value
	 lvalues(1) = .true.
         call ftpcll(funit,colnum,row,felem,1,lvalues,status)
      else if (tval .eq. 'N' .or.
     &        tval .eq. 'NO' .or.
     &        tval .eq. 'F'  .or.
     &        tval .eq. 'FALSE')then
C               this is a 'FALSE' logical value
	 lvalues(1) = .false.
         call ftpcll(funit,colnum,row,felem,1,lvalues,status)
      else if (value .eq. 'INDEF')then
C               set the table element as undefined
         call ftpclu(funit,colnum,row,felem,1,status)
      else 
C           try reading the parameter as a number (double precision)
C           find the end of the value string
         vend=0
         do 10 i=80,1,-1
            if (value(i:i) .ne. ' ')then
               vend=i
               go to 20
            end if
 10      continue
 20      continue
         if (vend .gt. 30 .or. vend .eq. 0)then
C               string is too long to be a number; must be a character string
            go to 100
         end if

C           encode the format string
         if (vend .lt. 10)then
            write (fmt,1001)vend
 1001       format('(BN,D',i1,'.0)')
         else 
            write(fmt,1002)vend
 1002       format('(BN,D',i2,'.0)')
         end if
C           if the internal read fails, then treat value as a string
         read(value,fmt,err=100)dvalue

C for the SUN, check whether this really is a value.  Single character strings
C and a few other strings get read as NaN 
         if ((value(1:1) .ne. '+') .and. (value(1:1) .ne. '-') .and.
     &        (value(1:1) .ne. '.') .and. ((value(1:1) .lt. '0') .or.
     &        (value(1:1) .gt. '9'))) goto 100

C           write the numeric value to the table
         call ftpcld(funit,colnum,row,felem,1,dvalue,status)
         go to 998

 100     continue
C           jump here if the input value appears to be a string
C           write the string value to the table
         call ftpcls(funit,colnum,row,felem,1,value,status)
      end if

 998  continue
C       report any FITSIO errors
      if (status .gt. 0)then
         call fcerrm(status)
      end if

 999  continue
C       close the fits file
      status = 0
      call ftclos(funit,status)
      end
