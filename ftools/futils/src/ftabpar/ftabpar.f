C******************************************************************************
C FTOOLS TASK:
C      ftabpar
C
C FILE:
C      ftabpar.f 
C
C DESCRIPTION: 
C      Copy a value from a FITS table to an IRAF parameter   
C
C AUTHOR:  
C      William Pence  6/30/92
C
C MODIFICATION HISTORY:
C      9/12/94 EAG 3.0a clear FITSIO stack
C      3/18/98 PDW 3.0b ftd2e called with wrong # of arguments
C      5/21/98 NG 3.0c- Replace ftmrhd(...,extnum,...) with ftmahd(..,extnum+1,..) 
C     03/18/99 toliver 3.1 interpret values from integer columns with
C                          floating point TSCAL keywords as real numbers
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
        subroutine ftabpr

        integer funit
        parameter (funit=37)
        integer status,blksiz,extnum,hdutyp,colnum,row,felem,nval
        integer dattyp,ivalue,nxrows, clenact
        character(160) fitfil,infil
        character(80) context,svalue, elemnt
        character(16) column,tform
        character s20*20,s35*35
        character(80) tscaln, ccolnum, tscalval, tscalcmt 
        character(1) tscaltype 
        real rvalue
        double precision dvalue
        logical exact,anyflg,lvalue
	character(40) taskname
	common /task/ taskname

	taskname = 'ftabpar3.1'
        status=0
        call ftcmsg

C       get the name of the input FITS file
        call uclgst('fitsfile',fitfil,status)
        if (status .ne. 0) then
            context = 'could not get fitsfile parameter'
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

C       get the element number to read
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

C       open the new FITS file with read access
        call ftopen(funit,infil,0,blksiz,status)
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

C       test that this is a legal type of extension
        if (hdutyp .lt. 1 .or. hdutyp .gt. 2)then
                context='unsupported extension type'
                call fcerr(context)
                go to 999
        end if

C       get the column number corresponding to the column name
        exact=.false.
        call ftgcno(funit,exact,column,colnum,status)
        if (status .gt. 0)then
                context='unable to find specified column name'
                call fcerr(context)
                go to 998
        end if

C get the element number for vector columns
	call elemparse (elemnt, funit, colnum, felem, status)
	if (status .ne. 0) then
	    context = ' Error parsing input element specification'
	    call fcerr (context)
	    goto 998
	endif

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
                        context='unable to parse TFORM keyword'
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
                end if
        end if

C       read the table value and convert it to a string, if necessary
        anyflg=.false.
        svalue=' '
        if (dattyp .eq. 16)then
C               read an ASCII value
                call ftgcfs(funit,colnum,row,felem,1,
     &                      svalue,anyflg,anyflg,status)
                if (anyflg)then
                        svalue='INDEF'
                end if
        else if (dattyp .eq. 14)then
C               read a logical value
                call ftgcfl(funit,colnum,row,felem,1,lvalue,
     &                      anyflg,anyflg,status)
                if (anyflg)then
                        svalue='INDEF'
                else if (lvalue)then
                        svalue='T'
                else
                        svalue='F'
                end if
        else if (dattyp .le. 41)then
C
C               Get the data type of the TSCALn keyword, if it exists
C
                WRITE (ccolnum, '(I12)') colnum
                CALL crmvblk (ccolnum)
                tscaln = 'TSCAL' // ccolnum(:clenact (ccolnum))
                CALL ftgkey (funit, tscaln, tscalval, tscalcmt, status)
                IF (status .EQ. 0) THEN
                   CALL ftdtyp (tscalval, tscaltype, status)
                ENDIF
C
C               If the TSCAL keyword exists with a real value, read the
C                  value as a real number
C
                IF ((status .EQ. 0) .AND.
     &              (tscaltype .EQ. 'F')) THEN
                   rvalue = 0.0
                   CALL ftgcfe (funit, colnum, row, felem, 1,
     &                          rvalue, anyflg, anyflg, status)
C
C                  Convert the floating point value to a string
C
                   IF (anyflg) THEN
                      svalue = 'INDEF'
                   ELSE
                      CALL ftr2e (rvalue, 6, s20, status)      
                      svalue = s20
                   ENDIF
C
C               Otherwise, read the value as an integer
C
                ELSE
                   status = 0
                   ivalue = 0

C               Read as a double, to support unsigned 4-byte integers
                   CALL ftgcfd (funit, colnum, row, felem, 1,
     &                          dvalue, anyflg, anyflg, status)

C                   CALL ftgcfj (funit, colnum, row, felem, 1,
C     &                          ivalue, anyflg, anyflg, status)
C
C                  Convert the integer/double to a string
C
                   IF (anyflg) THEN
                      svalue = 'INDEF'
                   ELSE
	              call ftd2f(dvalue,0,s35,status)
C                     CALL fti2c (ivalue, s20, status)
                      svalue = s35
                   ENDIF
                ENDIF
        else if (dattyp. eq. 42)then
C               read a single precision value
                rvalue=0.
                call ftgcfe(funit,colnum,row,felem,1,
     &                      rvalue,anyflg,anyflg,status)
C               convert the floating point value to a string
		if (anyflg)then
			svalue='INDEF'
		else
	                call ftr2e(rvalue,7,s20,status)      
                        svalue=s20
		end if
        else if (dattyp. eq. 82)then
C               read a double precision value
                dvalue=0.
                call ftgcfd(funit,colnum,row,felem,1,
     &                      dvalue,anyflg,anyflg,status)
C               convert the floating point value to a string
		if (anyflg)then
			svalue='INDEF'
		else
	                call ftd2e(dvalue,15,s35,ivalue,status)
	                svalue=s35
		end if
        else
C               dattype is not supported
                context='column datatype is not supported'
                call fcerr(context)
                go to 999
        end if

        if (status .gt. 0)go to 998

C       write the value string to the .par file
        call uclpst('value',svalue,status)
        if (status .gt. 0)then
                context='unable to write keyword value'
                call fcerr(context)
                go to 999
        end if

C       write the undefined value flag to the .par file
        call uclpsb('undef',anyflg,status)
        if (status .gt. 0)then
                context='unable to write undef value'
                call fcerr(context)
        end if
        go to 999

998     continue
C       print out the fitsio error message
        call fcerrm(status)
        status = 0

999     continue
C       close the fits file 
        call ftclos(funit,status)
        end

