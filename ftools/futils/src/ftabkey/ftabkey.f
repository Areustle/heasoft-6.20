C*****************************************************************************
C FTOOLS TASK:
C      ftabkey
C
C FILE:
C      ftabkey.f 
C
C DESCRIPTION: 
C      Copy a value from a FITS table to a FITS header keyword   
C
C AUTHOR:  
C      William Pence  6/30/92
C
C MODIFICATION HISTORY:
C	11/26/93 EAG add array specifications for element
C       9/12/94 EAG 3.0a - clear FITSIO stack, etc.
C       5/21/98 NG 3.0b- Replace ftmrhd(...,extnum,...) with ftmahd(..,extnum+1,..)
C       5/21/98 NG 3.0c- Replace ftmrhd(...,extnum,...) with ftmahd(..,extnum+1,..)
C      03/18/99 toliver 3.1 interpret values from integer columns with
C                           floating point TSCAL keywords as real numbers
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
      subroutine ftabky

      integer funit
      parameter (funit=37)
      integer status,blksiz,extnum,hdutyp,hdtype
      integer colnum,row,felem,nval,nxrows, clenact
      integer dattyp,ivalue,vend,i
      character(160) tblfil,fitfil,infil
      character(80) context,svalue, elemnt
      character s20*20,s35*35
      character(80) tscaln, ccolnum, tscalval, tscalcmt 
      character(1) tscaltype 
      character stemp*79, keywrd*8,tmplat*100,card*80
      character(25) column,tform,oldval*80,oldcom*80
      real rvalue
      double precision dvalue
      logical exact,anyflg,lvalue,add
      character(40) taskname
      common /task/ taskname

      taskname = 'ftabkey3.1'
      status=0
      call ftcmsg

C       get the name of the input FITS table
      call uclgst('tblfile',tblfil,status)
      if (status .ne. 0) then
         context = 'could not get tblfile parameter'
         call fcerr(context)
         goto 999
      endif

C       get the name of the column to read
      call uclgst('column',column,status)
      if (status .ne. 0) then
         context = 'could not get column parameter'
         call fcerr(context)
         goto 999
      endif

C       get the row number to read
      call uclgsi('row',row,status)
      if (status .ne. 0) then
         context = 'could not get row parameter'
         call fcerr(context)
         goto 999
      endif

C       get the name of the output FITS header file
      call uclgst('fitsfile',fitfil,status)
      if (status .ne. 0) then
         context = 'could not get fitsfile parameter'
         call fcerr(context)
         goto 999
      endif

C       get the name of the keyword
      call uclgst('keyword',keywrd,status)
      if (status .ne. 0) then
         context = 'could not get keyword parameter'
         call fcerr(context)
         goto 999
      endif

C       get the element number to read
      call uclgst('element',elemnt,status)
      if (status .ne. 0) then
         context = 'could not get element parameter'
         call fcerr(context)
         goto 999
      endif

C       get the add record flag
      call uclgsb('add',add,status)
      if (status .ne. 0) then
         context = 'could not get add flag'
         call fcerr(context)
         goto 999
      endif

C       parse the output string to get the file name and extension number
      call fcpars(tblfil,infil,extnum,status)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

      if (status .gt. 0 .or. extnum .lt. 0)then
         context='unable to parse the tblname[extension]'
         call fcerr(context)
         go to 999
      end if

C       open the new FITS file with read/write access
      call ftopen(funit,infil,1,blksiz,status)
      if (status .gt. 0)then
         context='unable to open input FITS file'
         call fcerr(context)
         go to 998
      end if

C       move to the correct extension
      call ftmahd(funit,extnum+1,hdutyp,status)
      if (status .gt. 0)then
         context='unable to move to input FITS extension'
         call fcerr(context)
         go to 998
      end if

C       test that this is a legal type of extension
      if (hdutyp .lt. 1 .or. hdutyp .gt. 2)then
         context='input is not a table extension'
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

C get the correct element number for this request
      call elemparse (elemnt, funit, colnum, felem, status)
      if (status .ne. 0) goto 998

C       check that row number is within the legal range of the table
      call ftgkyj(funit,'naxis2',nxrows,context,status)
      if (row .gt. nxrows)then
         context='row number is out of bounds'
         call fcerr(context)
         go to 999
      end if

C       get the column data type
      call ftgkns(funit,'TFORM',colnum,1,tform,nval,status)
      if (status .gt. 0)then
         context='unable to read TFORM keyword'
         call fcerr(context)
         go to 998
      end if

C       parse the TFORM string to get the numerical type
      if (hdutyp .eq. 2)then
C               this is a binary table
         call ftbnfm(tform,dattyp,ivalue,ivalue,status)
         if (status .gt. 0)then
            context='unable to parse the TFORM keyword'
            call fcerr(context)
            go to 998
         end if
      else 
C               parse the ASCII table TFORM string
         if (tform(1:1) .eq. 'A')then
            dattyp = 16
         else if (tform(1:1) .eq. 'I')then
            dattyp = 41
         else if (tform(1:1) .eq. 'E')then
            dattyp =42
         else if (tform(1:1) .eq. 'F')then
            dattyp =42
         else if (tform(1:1) .eq. 'D')then
            dattyp =82
         else
            context='error in ASCII table TFORM keyword'
            call fcerr(context)
            go to 999
         end if
      end if

C       read the table value and convert it to a string, if necessary
      anyflg=.false.
      svalue=' '
      if (dattyp .eq. 16)then
C               read an ASCII value
         call ftgcfs(funit,colnum,row,felem,1,
     &        stemp,anyflg,anyflg,status)
C               enclose the value in quotes (if it isn't already
         svalue(2:80)=stemp
         if (svalue(2:2) .ne. '''')then
            svalue(1:1)=''''
C                       find the end of the string
            vend=3
            do 10 i=79,2,-1
               if (svalue(i:i) .ne. ' ')then
                  vend=i+1
                  go to 20
               end if
 10         continue
 20         svalue(vend:vend)=''''
         end if                  
      else if (dattyp .eq. 14)then
C               read a logical value
         call ftgcl(funit,colnum,row,felem,1,lvalue,status)
         if (lvalue)then
            svalue='T'
         else
            svalue='F'
         end if
      else if (dattyp .le. 41)then
C
C        Get the data type of the TSCALn keyword, if it exists
C
         WRITE (ccolnum, '(I12)') colnum
         CALL crmvblk (ccolnum)
         tscaln = 'TSCAL' // ccolnum(:clenact (ccolnum))
         CALL ftgkey (funit, tscaln, tscalval, tscalcmt, status)
         IF (status .EQ. 0) THEN
            CALL ftdtyp (tscalval, tscaltype, status)
         ENDIF
C
C        If the TSCAL keyword exists with a real value, read the
C           value as a real number
C
         IF ((status .EQ. 0) .AND. (tscaltype .EQ. 'F')) THEN
            rvalue = 0.0
            CALL ftgcfe (funit, colnum, row, felem, 1,
     &                   rvalue, anyflg, anyflg, status)
C
C           Convert the floating point value to a string
C
            IF (anyflg) THEN
               svalue = 'INDEF'
            ELSE
               CALL ftr2e (rvalue, 6, s20, status)      
               svalue = s20
            ENDIF
C
C        Otherwise, read the value as an integer
C
         ELSE
            status = 0
            ivalue = 0
            CALL ftgcfj (funit, colnum, row, felem, 1,
     &                   ivalue, anyflg, anyflg, status)
C
C           Convert the integer to a string
C
            IF (anyflg) THEN
               svalue = 'INDEF'
            ELSE
               CALL fti2c (ivalue, s20, status)
               svalue = s20
            ENDIF
         ENDIF
      else if (dattyp. eq. 42)then
C               read a single precision value
         rvalue=0.
         call ftgcfe(funit,colnum,row,felem,1,
     &        rvalue,anyflg,anyflg,status)
C               convert the floating point value to a string
         call ftr2e(rvalue,6,s20,status)      
         svalue=s20
      else if (dattyp. eq. 82)then
C               read a double precision value
         dvalue=0.
         call ftgcfd(funit,colnum,row,felem,1,
     &        dvalue,anyflg,anyflg,status)
C               convert the floating point value to a string
         call ftd2e(dvalue,15,s35,ivalue,status)         
         svalue=s35
      else
C               dattype is not supported
         context='column datatype is not supported'
         call fcerr(context)
         go to 999
      end if
      if (status .gt. 0)go to 998

C       can't write keyword if table value was undefined
      if (anyflg)then
         context='table element is undefined'
         call fcerr(context)
         go to 999
      end if

C       find the end of the string
      vend=3
      do 30 i=80,1,-1
         if (svalue(i:i) .ne. ' ')then
            vend=i+1
            go to 40
         end if
 30   continue
 40   continue

C       concatinate the strings together into the header template
      tmplat=keywrd//' '//svalue(1:vend)//'  written by ftabkey'

C       reformat the template into a legal FITS keyword record
      call ftgthd(tmplat,card,hdtype,status)
      if (status .gt. 0)then
         context='unable to parse keyword template'
         call fcerr(context)
         go to 998
      end if

C       if output file is not the same as input file, open the new file
      if (fitfil .ne. ' ')then
         call ftclos(funit,status)
C               parse the name to get the file name and extension number
         call fcpars(fitfil,infil,extnum,status)

C EAG 8/25/93 default to 1st extension
         if (extnum .eq. -99) extnum = 1

         if (status .gt. 0)then
            context='unable to parse output file name'
            call fcerr(context)
            go to 998
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
      end if
      
C       modify the keyword if it already exists; otherwise create new one
      call ftgkey(funit,card(1:8),oldval,oldcom,status)
      if (status .le. 0)then
C               keyword exists, so modify it
         call ftmcrd(funit,card(1:8),card,status)
C               restore the old comment string
         call ftmcom(funit,card(1:8),oldcom,status)
      else if (status .eq. 202)then
C               keyword not found; can we add a new one?
         status=0
         if (add)then
            call ftprec(funit,card,status)
         else
C                    no permission to write new keyword
            context='No permission to create new keyword'
            call fcerr(context)
            go to 999
         end if
      else
C               some other FITSIO error
         context='Error reading FITS header'
         call fcerr(context)
      end if
      if (status .le. 0)go to 999

 998  continue
C       print out the fitsio error message
      call fcerrm(status)
      status = 0

 999  continue
C       close the fits file 
      call ftclos(funit,status)
      end

