C******************************************************************************
C FTOOLS TASK:
C      fcreate
C
C FILE:
C      fcreate.f
C
C DESCRIPTION:
C      Create a FITS extension, either ASCII or BINARY, from ASCII template
C      files.
C
C AUTHOR:
C      William Pence  6/2/92
C
C MODIFICATION HISTORY:
C       3/17/93 EAG - fixed formatting confusion in getcol and dodata
C       12/13/93 EAG - remove one ' if two '' are found
C       5/24/94 JKB - fixed # comment character in header files
C       8/22/94 3.0b EAG - add clobber, neaten
C       11/28/1995 JRG v.3.0c - increased TTYPE length from 16 to 40
C	02/25/96 Srilal - v3.0d: Timestamp added
C       10/17/97 PDW 3.0e - Fixed bad gcount value in ftphpr
C       11/18/97 PDW 3.1 - Added support for vector columns and complex
C                          numbers, cleaned up code for newer FITSIO, and
C                          sped up processing by 4-8 times
C        1/13/98 PDW 3.1a- Support piping in data from stdin (datafile='-')
C        3/17/98 PDW 3.1b- Wasn't handling blank lines in data file correctly
C        8/10/98 NG  3.1c- changed the initial row number 
C       11/18/99 PDW 3.1d- Add bit-column support
C       05/20/05 WDP 3.1e - ignore extra linefeed characters at end of line
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      subroutine figprm - gets parameters from environment
C      subroutine getcol - get column definitions
C      subroutine dohead - process the header template file
C      subroutine dodata - process the data template file
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine fcreae

      integer maxfld,funit,aunit
      parameter (maxfld=1000)
      parameter (funit=37)
      parameter (aunit=38)
      integer nskip,nrows,morehd,status
      character fitsnm*160,cdfile*160,dtfile*160,hdfile*160,tabtyp*8
      character(40) ttype(maxfld)
      character(25) tunit(maxfld)
      character(80) extnam,aundef
      character(80) context
      character(16) tform(maxfld)
      integer nfield,iundef,i
C      integer rowlen,space
      integer begcol(maxfld),twidth(maxfld)
      integer tbcol(maxfld),datacode(maxfld),repeat(maxfld)
      logical histry,freefm
      character(40) taskname
      common /task/ taskname

      taskname = 'fcreate3.2e'
      call ftcmsg
      status=0

C       get all the input parameters
      call gcreae(fitsnm,cdfile,dtfile,hdfile,tabtyp,nskip,
     &     nrows,histry,morehd,extnam,aundef,iundef,status)
      if (status .gt. 0)go to 998

C       get the column definitions
      call getcol(aunit,cdfile,tabtyp,maxfld,nfield,ttype,tform,tunit,
     &     datacode,repeat,freefm,begcol,twidth,status)
      if (status .gt. 0)go to 998

C       create the new FITS file: Call Ftools routine ffinit, not FITSIO
C            checks clobber parameter and acts accordingly
      call ffinit(funit,fitsnm,status)
      if (status .gt. 0)then
         context='unable to create the FITS file, may exist? ' // fitsnm
         call fcerr(context)
         go to 998
      end if

C       write default null primary array keywords
      call ftphpr(funit,.true.,16,0,tbcol,0,1,.true.,status)
      if (status .gt. 0)then
         context='unable to write primary header keywords'
         call fcerr(context)
         go to 998
      end if

C       write the required header keywords
      if (tabtyp .eq. 'ASCII')then
C               determine the row length and each column's starting position
C        space=1
C         call ftgabc(nfield,tform,space,rowlen,tbcol,status)
C         call ftitab(funit,rowlen,0,nfield,ttype,tbcol,
C     &        tform,tunit,extnam,status)

C  Modified by WDP on 7/12/01
         call ftcrtb(funit,1,0,nfield,ttype,
     &        tform,tunit,extnam,status)

C               write the TNULL keywords, if required
         if (aundef .ne. ' ')then
            do 10 i=1,nfield
               call ftpkns(funit,'TNULL',i,1,aundef,
     &              'string representing an undefined value',status)
 10         continue
         end if
      else
C         call ftibin(funit,0,nfield,ttype,tform,tunit,
C     &        extnam,0,status)

C  Modified by WDP on 7/12/01
         call ftcrtb(funit,2,0,nfield,ttype,
     &        tform,tunit,extnam,status)

C               write the TNULL keywords, if required
         if (iundef .ne. 0)then
            do 20 i=1,nfield
               if ( datacode(i).eq.11 .or. datacode(i).eq.21
     &              .or. datacode(i).eq.41 ) then
                  call ftpknj(funit,'TNULL',i,1,iundef,
     &                 'integer representing an undefined value',status)
               end if
 20         continue
         end if
      end if
      if (status .gt. 0)then
         context='unable to create table extension'
         call fcerr(context)
         go to 998
      end if

C       process the header template file if specified by the user
      call dohead(aunit,funit,hdfile,status)

C       write history record, if required
      if (histry)then
         call ftphis(funit,'This FITS file was created '//
     &        'by the FCREATE task.',status)
         call timestamp(funit)
         call ftpdat(funit, status)
      end if

C       reserve space for more keywords if required
      if (morehd .gt. 0)then
         call fthdef(funit,morehd,status)
      end if

      if (status .gt. 0)go to 998

C       process the data template file
      call dodata(aunit,funit,dtfile,nskip,nrows,freefm,nfield,
     &     begcol,twidth,datacode,repeat,status)
      if (status .le. 0)go to 999

 998  continue
C       come here on some sort of error; write error message to user
      call fcerrm(status)

 999  continue
C       close the file
      if (status .eq. 0) then
         call ftclos(funit,status)
      else
C delete output file since there was an error
         status = 0
         call ftdelt(funit, status)
      endif

      end

C******************************************************************************
C SUBROUTINE:
C      gcreae
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C      William Pence   6/2/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C      figprm uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gcreae(outfil,cdfile,dtfile,hdfile,tabtyp,nskip,nrows,histry,
C             morehd,extnam,aundef,iundef,status)
C
C ARGUMENTS:
C    Output:
C      outfil   - name of output FITS file
C      cdfile   - name of column descriptor file
C      dtfile   - name of ASCII data template file
C      hdfile   - name of ASCII header template file
C      tabtyp   - ASCII or BINARY type of FITS extension to be created
C      nskip    - number of lines in data templete file to skip over
C      nrows    - number of rows in the data template file to read
C      histry   - if true, append a history record to the FITS file
C      morehd   - reserve room in header for this many more keywords
C      extnam   - value for the FITS EXTNAME keyword
C      aundef   - ASCII value to use for undefined values in an ASCII table
C      iundef   - integer value to use for undefined values in a binary table
C      status   - returned error status (0 = OK)
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsb - get boolian parameter
C
C******************************************************************************
      subroutine gcreae(outfil,cdfile,dtfile,hdfile,tabtyp,nskip,
     &     nrows,histry,morehd,extnam,aundef,iundef,status)

      character*(*) outfil,cdfile,dtfile,hdfile,tabtyp,extnam,aundef
      integer nskip,nrows,morehd,iundef,status
      logical histry
      character(80) context

C  get the name of the input column descriptor file
      call uclgst('cdfile',cdfile,status)
      if (status .ne. 0) then
         context = 'could not get cdfile parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the input data template file
      call uclgst('datafile',dtfile,status)
      if (status .ne. 0) then
         context = 'could not get datafile parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the output FITS file
      call uclgst('outfile',outfil,status)
      if (status .ne. 0) then
         context = 'could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the optional input header template file
      call uclgst('headfile',hdfile,status)
      if (status .ne. 0) then
         context = 'could not get headfile parameter'
         call fcerr(context)
         goto 999
      endif

C  get FITS extension type
      call uclgst('tbltype',tabtyp,status)
      if (status .ne. 0) then
         context = 'could not get tabtype parameter'
         call fcerr(context)
         goto 999
      endif
      call ftupch(tabtyp)

C  get the number of lines to skip in the data template file
      call uclgsi('nskip',nskip,status)
      if (status .ne. 0) then
         context = 'could not get nskip parameter'
         call fcerr(context)
         goto 999
      endif

C  get the number of rows to read in the data template file
      call uclgsi('nrows',nrows,status)
      if (status .ne. 0) then
         context = 'could not get nrows parameter'
         call fcerr(context)
         goto 999
      endif

C  get the history record flag
      call uclgsb('history',histry,status)
      if (status .ne. 0) then
         context = 'could not get history flag'
         call fcerr(context)
         goto 999
      endif

C  get the number of keywords to reserve space for in the FITS header
      call uclgsi('morehdr',morehd,status)
      if (status .ne. 0) then
         context = 'could not get morehdr parameter'
         call fcerr(context)
         goto 999
      endif

C  get FITS extension name
      call uclgst('extname',extnam,status)
      if (status .ne. 0) then
         context = 'could not get extname parameter'
         call fcerr(context)
         goto 999
      endif

C  get string undefined value
      call uclgst('anull',aundef,status)
      if (status .ne. 0) then
         context = 'could not get anull parameter'
         call fcerr(context)
         goto 999
      endif

C  get intwger undefined value
      call uclgsi('inull',iundef,status)
      if (status .ne. 0) then
         context = 'could not get inull parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue

      return
      end

C******************************************************************************
C SUBROUTINE:
C      getcol
C
C DESCRIPTION:
C	get column descriptions
C
C AUTHOR:
C      William Pence   6/2/92
C
C MODIFICATION HISTORY:
C	3/17/93 (EAG) - the variables begcol and twidth were performing
C			double duty.  Use fmtcol and fmtwidth to indicate
C			format of the format file, and begcol and twidth
C			to indicate the format of the data file.
C      11/14/97 PDW   - Replaced method for finding the data type...
C                           now call FITSIO routine FTBNFM or FTASFM
C                           and allow a repeat value for vector columns
C
C NOTES:
C
C USAGE:
C	call getcol(aunit,cdfile,tabtyp,maxdim,nfield,ttype,tform,tunit,
C                   datacode,repeat,freefm,begcol,twidth,status)
C
C ARGUMENTS:
C
C   Input:
C	aunit  - fortran unit number to use to read the column descriptor file
C	cdfile - name of the column descriptor file
C       tabtyp - type of table being created
C       maxdim - maximum number of columns allowed
C   Output:
C	nfield - number of columns
C	ttype  - (array) name of each column
C	tform  - (array) FITS format of each column
C	tunit  - (array) physical units of the column
C	datacode - (array) datatype of columns
C       repeat - (array) repeat value of vector columns (1 if not vector)
C	freefm - set to true if the template file is in free format
C	begcol - (array) beginning position of each column (if not free format)
C	twidth - (array) width of the column (if not free format)
C	status - returned error status
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine getcol(aunit,cdfile,tabtyp,maxdim,nfield,
     &     ttype,tform,tunit,datacode,repeat,freefm,begcol,twidth,
     &     status)

      integer aunit,maxdim,nfield,status
      integer datacode(*),repeat(*),begcol(*),twidth(*)
      character*(*) cdfile,tabtyp,ttype(*),tform(*),tunit(*)
      logical freefm
      integer fields,j,fmtcol(5),fmtwidth(5)
      character line*80, context*80

      if (status .gt. 0)return

C	open the column descriptor file
      open(unit=aunit,file=cdfile,status='old',err=997)

      nfield=0
C	start by assuming the data template file is in fixed format
      freefm=.false.

C	read the next line of the file
 10   read(aunit,1000,err=998,end=999)line
 1000 format(a)

C ignore blank lines (EAG 6/21/93)
      if (line .eq. ' ') goto 10

      nfield=nfield+1

C	look for up to 5 tokens on the line
      call gtoken(5,line,fields,fmtcol,fmtwidth)
      if (fields .lt. 2)then
C		error parsing file; ttype and tform are required
         status=108
         context=
     &        'cdfile format error: column name and format required'
         call fcerr(context)
         go to 999
      else if (fields .lt. 5)then
         freefm=.true.
      end if

C	copy the ttype and tform fields to the output variables
      ttype(nfield)=line(fmtcol(1):fmtcol(1)+fmtwidth(1)-1)
      tform(nfield)=line(fmtcol(2):fmtcol(2)+fmtwidth(2)-1)

C	copy out the tunit field if it is present and not equal to 'NONE'
      if (fields .ge. 3 .and. fmtwidth(3) .gt. 0)then
         tunit(nfield)=line(fmtcol(3):fmtcol(3)+fmtwidth(3)-1)
         if (tunit(nfield) .eq. 'NONE'
     &        .or. tunit(nfield) .eq. 'none')tunit(nfield)=' '
      else
         tunit(nfield)=' '
      end if

C	convert the starting position and column width fields, if present
      if (.not. freefm)then
         call ftc2ii(line(fmtcol(4):fmtcol(4)+fmtwidth(4)-1),
     &        begcol(nfield),status)
         call ftc2ii(line(fmtcol(5):fmtcol(5)+fmtwidth(5)-1),
     &        twidth(nfield),status)
         if (status .gt. 0)then
            context='error parsing column starting position and width'
            call fcerr(context)
            go to 999
         end if
      end if

C	now determine the type of each column: 11/14/97 PDW: Use FITSIO
      if( tabtyp.eq.'ASCII' ) then
         call ftasfm(tform(nfield),datacode(nfield),repeat(nfield),
     &        j,status)
         repeat(nfield)=1
      else
         call ftbnfm(tform(nfield),datacode(nfield),repeat(nfield),
     &        j,status)
         if( datacode(nfield).eq.16 ) repeat(nfield)=repeat(nfield)/j
         if( datacode(nfield).ge.83 ) repeat(nfield)=repeat(nfield)*2
      end if
      if( status.gt.0 ) then
         write(context,'(A,I12)')
     &        'Error parsing column description field ', nfield
         call fcerr(context)
         go to 999
      end if

      if ( nfield.lt.maxdim ) go to 10
      go to 999

 997  continue
C       error opening template file
      context='error opening column descriptor file'
      call fcerr(context)
      status=104
      return

 998  continue
C       error reading template file
      context='error reading column descriptor file'
      call fcerr(context)
      status=108

 999  continue
      close(unit=aunit)

      end

C******************************************************************************
C SUBROUTINE:
C      dohead
C
C DESCRIPTION:
C	process the ASCII header template file
C
C AUTHOR:
C      William Pence   6/2/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call dohead(aunit,funit,hdfile,status)
C
C ARGUMENTS:
C	aunit  - fortran unit number to use to read the template file
C	funit  - fortran unit number to use to write to FITS file
C	hdfile - name of the header template file
C	status - returned error status
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine dohead(aunit,funit,hdfile,status)

      integer aunit,funit,status,hdtype
      character*(*) hdfile
      character tmplte*100,recstr*80,context*80

      if (hdfile .ne. ' ')then
         open(unit=aunit,file=hdfile,status='OLD',err=997)

C		read next line from template file and write it to FITS file
 10      read(aunit,1000,end=999,err=998)tmplte
 1000    format(a)

C ignore blank lines (EAG 6/21/93)
         if (tmplte .eq. ' ') goto 10

C ignore lines  beginning with # (JKB 5/24/94)
         if (tmplte(1:1) .eq. '#') goto 10

         call ftgthd(tmplte,recstr,hdtype,status)
         if (status .lt. 0)go to 999

         if (hdtype .eq. 0 .or. hdtype .eq. 1)then
C                       append the record to the FITS file
            call ftprec(funit,recstr,status)
            go to 10
         else if (hdtype .eq. -1)then
C			this was a keyword to be deleted; ignore it
            go to 10
         else
C			this was the END record, so quit
            go to 999
         end if

 997     continue
C      		error opening template file
         context='error opening header template file'
         call fcerr(context)
         status=104
         return

 998     continue
C       	error reading template file
         context='error reading header template file'
         call fcerr(context)
         status=108

 999     continue
         close(unit=aunit)
      end if
      end

C******************************************************************************
C SUBROUTINE:
C      dodata
C
C DESCRIPTION:
C      process the ASCII data template file
C
C AUTHOR:
C      William Pence   6/2/92
C
C MODIFICATION HISTORY:
C      11/18/99 PDW - Add bit column support
C       3/17/98 PDW - Fixed problem with handling blank lines
C       1/13/98 PDW - Support piping in data from stdin (unit=5)
C      11/14/97 PDW - Added vector column and complex number support
C                     Also read integer columns as integers, not doubles
C                        (try to speed up routine by reducing conversions)
C	4/29/96 James Peachey - notify user of errors prior to returning
C			control to calling routine.
C	3/17/93 EAG - remove leading blanks from string before calling
C			fitsio string->double formatter
C
C NOTES:
C
C USAGE:
C	call dodata(aunit,funit,dtfile,nskip,nrows,freefm,nfield,
C     &             begcol,twidth,datacode,repeat,status)
C
C ARGUMENTS:
C	aunit  - fortran unit number to use to read template file
C	funit  - fortran unit number to use to write to FITS file
C	dtfile - name of the template file
C	nskip  - number of rows to skip at the beginning of the template file
C	nrows  - number of rows to process in the template file (0 = all rows)
C	freefm - is .true. if the template file is in free format
C	nfield - number of fields in the table
C	begcol - (array) starting position of each column, if fixed format
C	twidth - (array) width, in characters, of each column, if fixed format
C	datacode - (array) data type of column
C       repeat - (array) repeat value for vector columns
C	status - returned error status (0=OK)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      subroutine gtoken - find starting position and width of each token
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine dodata(aunit,funit,dtfile,nskip,nrows,freefm,
     &     nfield,begcol,twidth,datacode,repeat,status)
C
      integer aunit,funit,nskip,nrows,nfield,fields,begcol(*)
      integer twidth(*),status,irow
      character*(*) dtfile
      integer i,j,r,s1,s2,datacode(*),repeat(*),naxis2,dtlnum
      character dtline*30000,svalue*4096,context*80
      double precision dvalue
      logical freefm,lvalue
      integer nelem,elem
      integer txtunit
      integer jvalue
      integer nvalue
      integer r1
      logical isbits

      txtunit = aunit
C       count the number of data elements in each row
      nelem=0
      do 5 i=1,nfield
         nelem=nelem+repeat(i)
 5    continue

C       fix begcol and twidth to take into account vector columns
      if(.not.freefm) then
         j=nelem
         do 7 i=nfield,1,-1
            do 6 r=repeat(i),1,-1
               twidth(j) = twidth(i)
               begcol(j) = begcol(i)+(r-1)*twidth(i)
               j=j-1
 6          continue
 7       continue
      end if

C	open the data template file or reset unit# for stdin
      if (dtfile.eq.'-') then
         txtunit=5
      else
         open(unit=txtunit,file=dtfile,status='old',err=997)
      endif

C	skip, but count lines at beginning of data template file
      do 10 i = 1,nskip
         read(txtunit,1000,end=998,err=998)dtline
 1000    format(a)
 10   continue	
      dtlnum = nskip

C	set loop counter=0 set it to an impossibly high number
      if (nrows .le. 0)nrows=1000000000

C	loop through each row of the input table
      naxis2=0
      irow = 0
      fields=0
      elem=0
      do 100 i=1,nrows
         irow = irow + 1

C        do each column/element in turn
         do 30 j=1,nfield
            isbits =.false.
            do 25 r=1,repeat(j)
               if(.not. isbits) then
               elem = elem+1

               if (elem .gt. fields)then
20               read(txtunit,1000,end=999,err=998)dtline
                 elem=1
C		 skip, but count comment lines:
                 dtlnum = dtlnum + 1
                 if (dtline(1:1) .eq. '#' .or. dtline .eq. ' ') go to 20
C		   if template file is in free format, parse the line to find
C		   the starting position and width of each token:
                 if (freefm)then
                   call gtoken(1000,dtline,fields,begcol,twidth)
                   if (fields .eq. 1000)then
C                      too many elements in the row
                       write(context, '(A, I5)')
     &                 'Error: more than 999 values in row #',
     &                 dtlnum
                       call fcerr(context)
                       status = 111
                       call fcerrm(status)
                       go to 999
                   end if

C                  test for <CR> or <LF> character at end of line
                   s2 = begcol(fields) + twidth(fields) - 1
	           if (dtline(s2:s2) .eq. char(10) .or. 
     &                dtline(s2:s2) .eq. char(13) )then
		      twidth(fields) = twidth(fields) - 1
		      if (twidth(fields) .eq. 0)fields = fields -1
                   end if

                 else
                   fields = nelem
                 end if
               end if

               s1 = begcol(elem)
               s2 = s1 + twidth(elem) - 1
	       

	       
               svalue=dtline(s1:s2)
               if (svalue .eq. 'INDEF')then
                  call ftpclu(funit,j,irow,r,1,status)
               else if (datacode(j) .eq. 16)then
C		      write the ASCII string value to the FITS column
                  call ftpcls(funit,j,irow,r,1,
     &                 svalue(1:twidth(elem)),status)
               else if (datacode(j) .eq. 14)then
C		      write the logical value to the FITS column
                  lvalue=.true.
                  if (svalue .eq.  'F')lvalue=.false.
                  call ftpcll(funit,j,irow,r,1,lvalue,status)
               else if (datacode(j) .eq. 1) then
                  if( svalue(1:1).eq.' ' )
     &                 call frmblk(svalue(1:twidth(elem)))
                  call ftc2dd(svalue(1:twidth(elem)),dvalue,status)

                  nvalue =1
                  do r1 =1, repeat(j)
                  nvalue =nvalue*2
                  enddo

                  do r1 = 1,repeat(j)
                  nvalue =nvalue/2
                  jvalue =int(dvalue/nvalue)
                  jvalue = mod(jvalue,2)
                  lvalue=.true.
                  if (jvalue .eq. 0) lvalue=.false.
                  call ftpclx(funit,j,irow,r1,1,lvalue,status)
                  enddo 
                  isbits =.true.
               else
C		      write the numeric value to the FITS column
C ftc2dd can't handle leading blanks
                  if( svalue(1:1).eq.' ' ) 
     &                 call frmblk(svalue(1:twidth(elem)))
                  call ftc2dd(svalue(1:twidth(elem)),dvalue,status)
                  call ftpcld(funit,j,irow,r,1,dvalue,status)
               end if
C Check for and report errors, in particular, overflow on data conversion.
               if (status .eq. 0) then
                  go to 25
               else if (status .eq. 412) then
                  write(context, '(A, I5, A, I3, A, I4)')
     &                 'data value out of bounds in line #',
     &                 dtlnum, ', column #', j, ', element #',r
                  call fcerr(context)
                  write(context, '(A, D12.5, A)')
     &                 'value read = ', dvalue, 
     &                 '; check cdfile formats.'
                  call fcerr(context)
               end if
               call fcerrm(status)
               go to 999
               end if
 25         continue
 30      continue
         naxis2=naxis2+1
 100  continue

      go to 999

 997  continue
C       error opening template file
      context='error opening data template file'
      call fcerr(context)
      status=104
      return

 998  continue
C       error reading template file
      context='error reading data template file'
      call fcerr(context)
      status=108

 999  continue
C       update the header with the correct number of rows in the table
      call ftmkyj(funit,'NAXIS2',naxis2,'&',status)

 9999 if (txtunit.eq.aunit) close(unit=aunit)
      end

