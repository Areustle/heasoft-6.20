C******************************************************************************
C FTOOLS TASK:
C      fkeytab
C
C FILE:
C      fkeytab.f 
C
C DESCRIPTION: 
C      Write a FITS keyword value to a FITS table element  
C
C AUTHOR:  
C      William Pence  7/01/92
C
C MODIFICATION HISTORY:
C	10/9/92 (EAG) - added existence parameter
C	11/26/93 (EAG) - allow for array specification
C       8/25/94 (EAG) 3.0a - clear FITSIO error stack appropriately
C       5/21/98 NG 3.0b - Replace ftmrhd(...,extnum,...) with ftmahd(..,extnum+1,..) 
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
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine fkeytb

      integer funit
      parameter (funit=37)
      integer status,blksiz,extnum,hdutyp,row
      integer felem,colnum,ival,nxrows
      character(80) elemnt
      character(160) fitfil,tblfil,infil
      character(80) value,context,sval,coment
      character column*24,keywrd*8,dtype*1
      double precision dval
      logical exist
      logical lval,exact
      character(40) taskname
      common /task/ taskname

      taskname = 'fkeytab3.0b'
      status=0
      call ftcmsg

C       get the name of the input FITS file
      call uclgst('fitsfile',fitfil,status)
      if (status .ne. 0) then
         context = 'could not get fitsfile parameter'
         call fcerr(context)
         goto 999
      endif

C       get the keyword name
      call uclgst('keyword',keywrd,status)
      if (status .ne. 0) then
         context = 'could not get keyword parameter'
         call fcerr(context)
         goto 999
      endif

C       get the name of the output FITS file
      call uclgst('tblfile',tblfil,status)
      if (status .ne. 0) then
         context = 'could not get fitsfile parameter'
         call fcerr(context)
         goto 999
      endif

C       get the column name 
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

C EAG 8/25/93 default to first extension
      if (extnum .eq. -99) extnum = 1

      if (status .gt. 0 .or. extnum .lt. 0)then
         context='unable to parse the filename[extension]'
         call fcerr(context)
         go to 999
      end if

C       open the new FITS file with write access
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
      if (hdutyp .lt. 0 .or. hdutyp .gt. 2)then
         context='invalid FITS extension type'
         call fcerr(context)
         go to 999
      end if

C       read the keyword and comment strings
      call ftgkey(funit,keywrd,value,coment,status)
      if (status .gt. 0)then
         exist = .false.
         status = 0
         call ftcmsg
      else
         exist = .true.
      endif

      call uclpsb ('exist', exist, status)
      if (status .ne. 0) then
         context='unable to write existence parameter'
         call fcerr(context)
         go to 998
      end if

C       check that the keyword had a value (not COMMENT or HISTORY)
      if (value .eq. ' ')then
         context='this comment keyword has no value field'
         call fcerr(context)
         go to 999
      end if

C       if output file is not the same as input file, open the new file
      if (tblfil .ne. ' ')then
         call ftclos(funit,status)
C               parse the name to get the file name and extension number
         call fcpars(tblfil,infil,extnum,status)

C EAG 8/25/93 default to 1st extension
         if (extnum .eq. -99) extnum = 1

         if (status .gt. 0)then
            context='unable to parse output file name'
            call fcerr(context)
            go to 999
         end if

C               open the new FITS file with write access
         call ftopen(funit,infil,1,blksiz,status)
         if (status .gt. 0)then
            context='unable to open output FITS file'
            call fcerr(context)
            go to 998
         end if

C               move to the correct extension
         call ftmahd(funit,extnum+1,hdutyp,status)
         if (status .gt. 0)then
            context='unable to move to output extension'
            call fcerr(context)
            go to 998
         end if

C               check for correct extension type
         if (hdutyp .lt. 1 .or. hdutyp .gt. 2)then
            context='invalid FITS extension type'
            call fcerr(context)
            go to 999
         end if
      end if

C       get the column number corresponding to the column name
      exact=.false.
      call ftgcno(funit,exact,column,colnum,status)
      if (status .gt. 0 .or. colnum .eq. 0)then
         context='unable to find specified column name'
         call fcerr(context)
         go to 998
      end if

C get the correct element number
      call elemparse (elemnt, funit, colnum, felem, status)
      if (status .ne. 0) goto 998

C       check that row number is within the legal range of the table
      call ftgkyj(funit,'naxis2',nxrows,context,status)
      if (row .gt. nxrows)then
         context='row number is out of bounds'
         call fcerr(context)
         go to 999
      end if

C       determine the data type of keyword
      call ftc2x(value,dtype,ival,lval,sval,dval,status)
      if (status .gt. 0)then
         context='error reading keyword value string'
         call fcerr(context)
         go to 998
      end if

C       write the value
      if (dtype .eq. 'I')then
C               write integer value to column
         call ftpclj(funit,colnum,row,felem,1,ival,status)
      else if (dtype .eq. 'L')then
C               write logical value to column
         call ftpcll(funit,colnum,row,felem,1,lval,status)
      else if (dtype .eq. 'C')then
C               write string value to column
         call ftpcls(funit,colnum,row,felem,1,sval,status)
      else 
C               write numeric value (double precision) to column
         call ftpcld(funit,colnum,row,felem,1,dval,status)
      end if
      
 998  continue
C       report any FITSIO errors
      if (status .gt. 0)then
         call fcerrm(status)
      end if

 999  continue
C       close the fits file
      call ftclos(funit,status)
      end
