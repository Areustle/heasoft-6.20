C******************************************************************************
C SELECTOR TASK:
C      ftabcopy
C
C FILE:
C      ftabcopy.f
C
C DESCRIPTION:
C      copy selected columns from one fits file
C      into another new fits file
C
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HISTORY:
C       10/23/92 (EAG) - added history parameter
C       3/15/93  (EAG) - added copyprime parameter
C       6/15/93  (EAG) - changed from FPROJECT, add table type option
C                        allow for new FIMCOL format
C       11/23/93 (EAG) - call fnmcol with one row at a time for speed
C       9/21/95  Pence - bug in copyall option; now support up to 999 columns
C       12/29/1995 Jeff Guerber - fimvcl, ficprt: changed lengths of ttype,
C           ntype, colist to character(40); fimvtk: keyval, comment to char*80
C       2/25/96 (Srilal) 3.0d - timestamp added
C       3/5/97 (Banashree M Seifert)V4.0
C                - added user option for no. of rows required
C                  by default, it is '-' which means all rows
C       10/15/97 PDW 4.0a Replace old get header routines
C       01/27/99 (AM)  - Added a 'call ftupch( inlist )' in ficprt subroutine
C                        which fixes a bug resulting in a 'floating point 
C                        exception' error for the low case input string. 
C                        Added the WCS related keywords TLMIN, TLMAX, TCTYP,
C                        TCRCL, TCDLT, TCRPX, TCROT, and TCRVL to copy from 
C                        the input column to the output column. 
C       09/29/99 PDW 4.0c Undo problematic ftupch call
C       09/30/99 PDW 4.0d Use proper input column numbers in fttscl and also
C                         do it for the output file (in firscl)
C       05/17/05 WDP 4.0e Add copykeys parameter
C
C NOTES:
C      ftabcopy supported in IRAF and HOST environments
C      unique flag currently not supported
C      snull parameter does not appear to be used
C
C MAKE:
C      HOST: make -f mkhftabcopy
C      IRAF: xc -c xftabcopy.x ftabcopy.f
C            xc -p stsdas xftabcopy.o ftabcopy.o -lmisc -lfitsio -liraf77 -o f.e
C
C USAGE:
C      HOST: hftabcopy
C      IRAF: ftabcopy
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      outfile - output FITS file
C      inlist  - list of column names to be copied
C       outtype - output extension type
C       inull  - the null value to use for ASCII->BINARY
C       snull  - the null value to use for BINARY->ASCII
C      unique  - row uniqueness flag
C      allflg  - copy all other extensions flag
C      history - whether to write history records
C       copyprime - whether to copy primary array
C
C CALLED ROUTINES:
C      subroutine gtabcy - gets parameters from environment
C      subroutine fimvcl - main subroutine for moving columns
C
C******************************************************************************
      subroutine ftabcy
      character(160) infile,outfile
      character(80) inlist, rowlist, outtype, snull
      logical      unique, allflg, history, copyprime, copykeys
      integer status, inull

      character(40) taskname
      common /task/ taskname
      common /snldef/ snull

      taskname = 'ftabcopy V4.0e'
      call ftcmsg

C get parameters from parameter file
      call gtabcy (infile,outfile,inlist,rowlist,outtype,inull, 
     &   snull,unique,allflg,history,copyprime,copykeys,status)
      if (status .eq. 0) then

C move columns to new FITS file
          call fimvcl(infile,outfile,inlist,rowlist,outtype,inull,
     &     snull,unique,allflg,history,copyprime,copykeys,status)
      end if
      end

C******************************************************************************
C SUBROUTINE:
C      gtabcy
C
C DESCRIPTION:
C      read in parameters from parameter file
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HISTORY:
C       10/23/92 (EAG) - added history parameter
C       3/15/93  (EAG) - added copyprime parameter
C       3/5/97   (Banashree M seifert)  
C              - added list of rows to be copied (character* rowlist)
C
C NOTES:
C       gtabcy uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gtabcy(infile,outfile,inlist,rowlist, outtype,inull,snull,unique,
C                       allflg,history, status)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output FITS file
C      inlist  - list of column names to be copied
C      rowlist - list of rows to be copied
C       outtype - type of output extension
C       inull  - null value to use for integer columns in ASCII->BINARY
C       snull  - null value to use for ascii columns in BINARY->ASCII
C      unique  - row uniqueness flag
C      allflg  - copy all other extensions flag
C      history - whether to write history record
C      copyprime - whether to copy primary array
C      copykeys - whether to copy all the table header keywords
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine uclgsb - get boolean parameter
C      subroutine fcecho - echo message to terminal
C
C******************************************************************************
      subroutine gtabcy (infile, outfile, inlist, rowlist, outtype, 
     &  inull, snull, unique, allflg, history, copyprime, copykeys,
     &  irafsts)

      character*(*) infile,outfile
      character*(*) inlist, rowlist, outtype, snull
      logical       unique,allflg, history, copyprime, copykeys
      character(80)  context
      integer       irafsts, inull

C Initialize variables
      irafsts = 0

C make F77 interface calls

C ---   Get the name of the input FITS file ---

      call uclgst('infile',infile,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the output FITS file ---

      call uclgst('outfile',outfile,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the list of columns names to be copied  ---

      call uclgst('columns',inlist,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get COLUMNS parameter(s)'
         call fcerr(context)
         goto 999
      endif

C ---   Get the list of columns names to be copied  ---

      call uclgst('rows',rowlist,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get ROWS parameter(s)'
         call fcerr(context)
         goto 999
      endif
  
C ---   Get the output table type value ---

      call uclgst('outtype', outtype,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get OUTTYPE parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the integer null value to use ---

      call uclgsi ('inull', inull, irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get INULL parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the string null to use ---

      call uclgst ('snull', snull, irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get SNULL parameter'
         call fcerr(context)
         goto 999
      endif

C --- Get the uniqueness boolean flag's value ---

      call uclgsb('unique',unique,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get UNIQUE parameter'
         call fcerr(context)
         goto 999
      endif

C --- Get the copy all other extensions boolean flag's value ---

      call uclgsb('copyall',allflg,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get COPYALL parameter'
         call fcerr(context)
         goto 999
      endif

C --- Get the copy primary array boolean flag's value ---

      call uclgsb('copyprime', copyprime, irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get COPYPRIME parameter'
         call fcerr(context)
         goto 999
      endif

C --- Get the copy header keywords flag's value ---

      call uclgsb('copykeys', copykeys, irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get COPYKEYS parameter'
         call fcerr(context)
         goto 999
      endif
C --- Get the history boolean flag's value ---

      call uclgsb('history',history,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get HISTORY parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue
      return
      end


C******************************************************************************
C SUBROUTINE:
C      fimvcl
C
C DESCRIPTION:
C      main subroutine for moving columns from input FITS file
C      to output FITS file
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HISTORY:
C      1/8/92 JKB Improved error handling on existing INPUT and OUTPUT files
C      1/8/92 JKB Primary arrays may now be projected to output file
C      1/15/92 JKB Check for goodness of column list before opening OUTFILE
C      10/23/92 EAG Added history parameter
C       3/15/93 EAG Added copyprime parameter
C       6/15/93 EAG Add table type parameter
C       3/6/97  Banashree M Seifert
C                . introduced the option so that user can specify
C                  row nos. wanted instead of going for full rows of
C                  input file
C
C       02/18/99 (AM)  - Replaced 'call ftmkyj()'  by  'call ftukyj()'
C NOTES:
C
C
C USAGE:
C       call fimvcl(infile,outfile,inlist,rowlist,outtype,inull,snull,unique,
C                   allflg,
C     &                    history, copyprime, status)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output FITS file
C      inlist  - list of column names to be copied
C     rowlist  - list of rows to be copied 
C       outtype - type of output extension
C       inull - integer null value to use
C       snull - string null value to use
C      unique  - row uniqueness flag
C      allflg  - copy all other extensions flag
C      history - whether to write history record
C       copyprime - whether to copy the primary array
C       copykeys - whether to copy all the table keywords
C       status - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      iunit    - input unit number
C      ounit    - output unit number
C      filename - input FITS file name
C      extnumb  - extension number
C      hdutype  - header unit type
C      excount  - extension counter
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcpars - parse input into filename and extension
C      subroutine ftopen - open FITS file
C      subroutine ftinit - initialize new FITS file
C      subroutine ftcopy - copy verbatum a FITS extension
C      subroutine ftghpr - get primary header from FITS file
C      subroutine ftphpr - put primary header to FITS file
C      subroutine ftpdef - define FITS primary array
C      subroutine ftmrhd - relative move to FITS extension
C      subroutine ftcrhd - create FITS header
C      subroutine ficprt - copy parts of a FITS extension
C      subroutine ftclos - close a FITS file
C      subroutine fcecho - echo message to terminal
C
C******************************************************************************
      subroutine fimvcl (infile, outfile, inlist,rowlist, outtype,
     &  inull,snull,unique,allflg,history,copyprime,copykeys, status)

      character*(*) infile,outfile
      character*(*) inlist, rowlist,outtype, snull
      logical       unique,allflg, history, copyprime, copykeys

      integer       i,status,nrows,rowlen,nocolumns, inull
      integer       iunit,ounit,extnumb
      logical       simple,extend,goodlist,goodrow,exact,negflg
      integer       bitpix,naxis,naxes(99),pcount,gcount
      integer       block,nmove,hdutype,excount,maxcl
      parameter ( maxcl = 999 )
      character(160) filename
      character(80)  context,extname
      character(40)  colist(maxcl),ttype(maxcl)
      character(16)  tform(maxcl)
      character(25)  tunit(maxcl)
      integer       tbcol(maxcl),tfields
      integer       varidat,ohtype,range(15),tot_rowrange
      integer       rowrange1(15),rowrange2(15)

C Initialize variables
      iunit = 15
      ounit = 16
      status = 0
      exact = .false.
      negflg = .false.

C parse the output table type keyword
      call ftupch (outtype)
      if (outtype .eq. 'ASCII') then
         ohtype = 1
         status = 0
      else if (outtype .eq. 'BINARY') then
         ohtype = 2
         status = 0
      else if (outtype .eq. 'SAME') then
         ohtype = -1
         status = 0
      else
         context = ' Unknown output extenstion type requested'
         call fcerr (context)
         status = 1
         goto 1000
      endif


C ---   Parse the INFILE into a filename and extension number ---

      call fcpars(infile,filename,extnumb,status)

C EAG 8/25/93 default to 1st extension
      if (extnumb .eq. -99) extnumb = 1

C ---   Open Existing input FITS file ---

      call ftopen(iunit,filename,0,block,status)
      if ( status .ne. 0 ) then
         context = ' Unable to open INFILE'
         call fcerr(context)
         goto 1000
      endif

C get the column names and the range of rows for an ASCII extension
C --- Check that extension exists & that column list is a subset

      if ( extnumb .ge. 1 ) then
         call ftmrhd(iunit,extnumb,hdutype,status)
         if ( status .ne. 0 ) then
            context = 'Extension Does Not Exist For Input FITS File'
            call fcerr(context)
            status = 0
            call ftclos(iunit,status)
            goto 1000
         else
            if ( hdutype .eq. 1 ) then
               call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &              tform,tunit,extname,status)
            else if ( hdutype .eq. 2 ) then
               call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &              extname,varidat,status)
            else
               context = 'Header Unit Type For Extension Not Supported'
               call fcerr(context)
               status = 0
               call ftclos(iunit,status)
               goto 1000
            endif
            if ((inlist .eq. ' ') .or. (inlist .eq. '-')) then
               do 10 i = 1, tfields
                  colist(i) = ttype(i)
 10            continue
               nocolumns = tfields
            else
               call fcgcls(inlist,colist,nocolumns,negflg)
            endif
            call fccmpl(nocolumns,tfields,colist,ttype,negflg,goodlist)
            if ( .not. goodlist ) then
               status = 0
               call ftclos(iunit,status)
               goto 1000
            endif
         endif
         call ftmrhd(iunit,-extnumb,hdutype,status)
      endif

C  check that rows list is allowable
      goodrow=.true.
      call fccmpr(rowlist,goodrow)
      if (.not. goodrow)  goto 999 

C  determine which rows are to be included and get the row ranges
C  and get the ranges(range(i)) of rows asked for and then total 
C  (tot_range) no. of rows

      call fcgrgs(rowlist,nrows,numranges,rowrange1,rowrange2)
      tot_rowrange = 0
      do i=1,numranges
         range(i) = rowrange2(i) - rowrange1(i) +1
         tot_rowrange = tot_rowrange + range(i)
      enddo 

C ---   Open Output FITS file ---

      status = 0
      call ffinit(ounit,outfile,status)
      if ( status .ne. 0 ) then
         context = ' Unable to open OUTFILE, may exist? ' // outfile
         call fcerr(context)
         status = 0
         call ftclos(iunit,status)
         goto 1000
      endif

C ---   Provide a primary header to the outfile based on allflg ---
      if ((allflg).or.(copyprime).or.(extnumb.eq.0)) then
         call ftcopy(iunit,ounit,0,status)
      else
         call ftghpr(iunit,99,simple,bitpix,naxis,naxes,
     &   pcount,gcount,extend,status)

         simple = .true.
         naxis = 0
         pcount = 0
         gcount = 1
         call ftphpr(ounit,simple,bitpix,naxis,naxes,
     &        pcount,gcount,extend,status)
         call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,status)
      endif
      if (status .ne. 0) goto 999

C ---   If all extension should be copied then copy each extension ---
C ---   else only copy the extension numbered extnumb              ---

      excount = 0
      if ( allflg ) then
         nmove = 1
 20      call ftmrhd(iunit,nmove,hdutype,status)
         if (status.eq.0) then
            excount = excount + 1
            if ( excount.ne.extnumb ) then
               call ftcrhd(ounit,status)
               call ftcopy(iunit,ounit,0,status)
            else
               call ftcrhd(ounit,status)
               if (ohtype.lt.0) ohtype = hdutype

               call ficprt (iunit,ounit,hdutype,ohtype,inlist,
     &              rowlist,infile,inull,copykeys,history,numranges, 
     >              range,rowrange1,rowrange2,status)

               call ftmkyj(ounit,'NAXIS2',tot_rowrange,'&',status)
            endif
            goto 20
         else if (status .eq. 107)then
            status=0
         endif
      else if ( extnumb .ne. 0 ) then
         nmove = extnumb
         call ftmrhd(iunit,nmove,hdutype,status)
         call ftcrhd(ounit,status)
         if (ohtype.lt.0) ohtype = hdutype

         call ficprt (iunit,ounit,hdutype,ohtype,inlist,
     &        rowlist,infile,inull,copykeys,history,numranges,range,
     >        rowrange1,rowrange2,status)

c -- Alex ---
         call ftmkyj(ounit,'NAXIS2',tot_rowrange,'&',status)
c -----------               

      endif

 999  continue
      if (status .ne. 0) then
         call fcerrm(status)
         status = 0
         call ftdelt (ounit, status)
         status  = 0
      else
         call ftclos(ounit,status)
      endif

      call ftclos(iunit,status)
 1000 continue
      return
      end


C******************************************************************************
C SUBROUTINE:
C      ficprt
C
C DESCRIPTION:
C      copy designated portion of extension
C
C AUTHOR/DATE:
C      James Kent Blackburn
C
C MODIFICATION HISTORY:
C       10/23/92 EAG - added history parameter
C       12/29/1995 Jeff Guerber - changed column-name variables ttype, ntype,
C            colist to character(40).
C        3/6/1997 Banashree M Seifert
C                      . modifications needed for accomodating the new
C                        option (row option for user). 
C
C       02/18/99  (AM) - Initialized incol(i) before getting the values. 
C                        . Replaced NAXIS1 by NAXIS2 in 'call 
C                          ftgkyj(iunit,'NAXIS2',nrows2,context,status)
C       09/29/99  PDW  - Undo problematic ftupch call
C
C NOTES:
C
C
C USAGE:
C      call ficprt(iunit,ounit,hdutype,ohtype,inlist,rowlist,
C                  fname,inull,copykeys,history,numranges,range,
C                  rowrange1,rowrange2,status)
C
C ARGUMENTS:
C      iunit   - input unit number
C      ounit   - output unit number
C      hdutype - header unit type
C       ohtype - output extension type
C      inlist  - input list of column names
C     rowlist  - input list of rows
C    numranges - total no. of ranges og rows
C      ranges  - ranges of each numranges
C   rowrange1  - lowest range
C   rowrange2  - highest range 
C      fname   - input FITS file name
C       inull  - null value to use
C     copykeys - whether to copy all header keywords
C      history - whether to write history record
C      status  - status integer
C
C PRIMARY LOCAL VARIABLES:
C      colist - true list of column names
C      negflg - negate name list flag
C      goodlist - good input list flag
C      colno - column number
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      integer function fcafml - returns ascii format length
C      subroutine ftghtb - get FITS ascii table header
C      subroutine fcgcls - get true column list from string
C      subroutine fccmpl - compare to string lists
C      subroutine ftgcno - get number of keywords
C      subroutine ftphtb - put FITS ascii table header
C      subroutine fimvtk - move ascii table specifiC keywords
C      subroutine ftadef - define ascii table
C      subroutine firscl - reset scale and offset
C      subroutine fnmcol - move each column, taking NULLS into account
C      subroutine ftghbn - get FITS binary header
C      subroutine ftphbn - put FITS binary header
C      subroutine fimvbk - move binary table specifiC keywords
C      subroutine ftbdef - define binary table
C      subroutine fcecho - echo message to terminal
C
C******************************************************************************
      subroutine ficprt(iunit,ounit,hdutype,ohtype,inlist,
     & rowlist,fname,inull,copykeys,history,numranges,range,
     & rowrange1,rowrange2,status)

      integer     maxcl
      parameter ( maxcl = 999 )
      integer       iunit,ounit,hdutype,status,ohtype
      character*(*) inlist,rowlist
      character(40)  colist(maxcl),ttype(maxcl),ntype(maxcl)
      character(40)  extname
      character(80)  context
      character*(*) fname
      logical     negflg, history, copykeys
      integer     i,colno,pcount,nocolumns,incol(maxcl)
      integer     nrows,rowlen,nrowlen,nonul,clnul(maxcl),nrows2
      logical     goodlist,exact
      character(16) tform(maxcl),snul(maxcl),nform(maxcl)
      character(25) tunit(maxcl),nunit(maxcl)
      integer     tbcol(maxcl),ntbcol(maxcl),tfields,nfields
      integer     varidat,irow,orow,inull,tasklen,fcstln
     
      integer numranges,rowrange1(*),rowrange2(*)
      integer range(*),one_go


      character(40) taskname
      common /task/ taskname

C --- get the header information

      status  = 0
      if ( hdutype .eq. 1 ) then
       rowlen  = 0
       call ftghtb(iunit,maxcl,rowlen,nrows,tfields,
     &  ttype,tbcol,tform,tunit,extname,status)
         varidat = 0
      else
       nrows = 0
       call ftghbn (iunit,maxcl,nrows,tfields,ttype,
     &  tform,tunit,extname,varidat,status)
      endif


C ---   parse the inlist string into a real list of strings

      if ((inlist .eq. ' ') .or. (inlist .eq. '-')) then
         do 10 i = 1, tfields
            colist(i) = ttype(i)
 10      continue
         nocolumns = tfields
      else

C Alex   convert the input string into uppercase
C (PDW    Let's not)
C         call ftupch( inlist )
C ---
         call fcgcls(inlist,colist,nocolumns,negflg)
      endif
            
      goodlist = .false.

      call fccmpl(nocolumns,tfields,colist,ttype,
     &   negflg,goodlist)
           

      if ( .not. goodlist ) goto 999
      nfields = nocolumns

C --- Alex    changed  exact = .true. to 
       exact = .false.

      do 60 i = 1, nfields
         call ftgcno(iunit,exact,colist(i),colno,status)
         ntype(i) = ttype(colno)
         nform(i) = tform(colno)
         nunit(i) = tunit(colno)
         incol(i) = colno
 60   continue

      if (ohtype.ne.hdutype) call fchfmt(hdutype,ohtype,nform,
     & nfields,status)

      if (ohtype .eq. 1) then
c Alex :
         nrowlen = 0
c -----
         call ftgabc (nfields,nform,1,nrowlen,ntbcol,status)

         call ftphtb(ounit,nrowlen,nrows,nfields,ntype,
     &      ntbcol,nform,nunit,extname,status)

      else
         call ftphbn(ounit,nrows,nfields,ntype,nform,
     &      nunit,extname,varidat,status)

      endif


      call fimvtk(iunit,ounit,nocolumns,incol,
     &     nonul,clnul,snul,status)


C check for problems copying NULL from ASCII to BINARY tables

      if ((nonul.gt.0).and.(hdutype.eq.1).and.
     & (ohtype.eq.2)) call ftabnl(ounit,nonul,clnul,snul,
     & nform,inull,status)


      if (status .ne. 0) goto 999

      if (copykeys) then
         call xcopynoscale(iunit,ounit,status)
      endif
      
      if (history) then
         tasklen=fcstln(taskname)+1
         context=taskname(1:tasklen)//'copied columns from '//fname
         call ftphis(ounit,context,status)
	 call timestamp(ounit)
      endif

      if (ohtype .eq. 1) then
         call ftadef(ounit,nrowlen,nfields,ntbcol,nform,
     &        nrows,status)
      else
         call ftbdef(ounit,nfields,nform,pcount,nrows,status)
      endif

C read without scaling to get it right
      call firscl(iunit,ounit,nocolumns,incol,status)

C tell FITSIO about the defined NULLs

      if (nonul .gt. 0) then
         if (ohtype .eq. 2) then
            call fistbn(ounit,nonul,clnul,snul,status)
         else
            call fistan(ounit,nonul,clnul,snul,status)
         endif
      endif

      if (status .ne. 0) goto 999

CCC      irow = 1
CCC      orow = 1

c irow = the first row no. to look in the input file to start
c orow = the row no. of the output 
C Do about as many rows at a time as will fit in 10 FITS blocks (28800 bytes)
c one_go= the no. of rows which could be accomodated in 10 FITS block
 
      status = 0

c --- Alex --- there should be NAXIS2 not NAXIS1 

      call ftgkyj(iunit,'NAXIS2',nrows2,context,status)

c -----------------------------------------------------
      one_go =28800/nrows2+1
      irow = rowrange1(1)
      orow = 1

      do i=1,numranges
120    nrows2=min(one_go,range(i))

         call fnmcol(iunit,ounit,irow,orow,nfields,
     &    nrows2,nform,incol,ohtype,status)


C        Note: fnmcol internally increments irow and orow

         range(i)=range(i)-nrows2
         if (range(i) .gt. 0)go to 120
         
C        if there are more input ranges of rows, then go for that
         if(i .lt. numranges) then
            irow = rowrange1(i+1)
            orow = rowrange2(i)+1
         endif
      enddo

 999  continue
 1000 format(a)
      return
      end


C******************************************************************************
C SUBROUTINE:
C      firscl
C
C DESCRIPTION:
C      Reset TSCALE and TZERO for input ascii tables
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HISTORY:
C      PDW 09/30/99: Use proper input column numbers in fttscl and also
C                    do it for the output file
C
C NOTES:
C
C
C USAGE:
C      call firscl(iunit,ounit,nocol,incol,status)
C
C ARGUMENTS:
C      iunit  - input FITS file unit number
C      ounit  - output FITS file unit number
C      nocol  - number of columns
C      incol  - column numbers of input columns
C      status - return status variable
C
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED ROUTINES:
C      subroutine fttscl - set ascii table scale and offset
C
C******************************************************************************
      subroutine firscl(iunit,ounit,nocol,incol,status)

      integer     maxcl
      parameter ( maxcl = 999 )
      integer iunit,nocol,status,i,ounit,incol(maxcl)

      do 10 i = 1, nocol
         call fttscl(iunit,incol(i),1.0D0,0.0D0,status)
         call fttscl(ounit,i,1.0D0,0.0D0,status)
 10   continue
      return
      end


C******************************************************************************
C SUBROUTINE:
C      fimvtk
C
C DESCRIPTION:
C      move TNULL, TSCAL, TZERO keywords in an ASCII table
C       this works for BINARY, too (EAG 6/15/93)
C
C AUTHOR/DATE:
C      James Kent Blackburn
C
C MODIFICATION HISTORY:
C       Jeff Guerber 12/10/1995 - changed keyval, comment from char*16 to *80
C
C NOTES:
C
C
C USAGE:
C      call fimvtk(iunit,ounit,nocolumns,map,nonul,clnul,snul,status)
C
C ARGUMENTS:
C      iunit - input unit number
C      ounit - output unit number
C      nocolumns - number of columns
C      map - mapping between input and output FITS file column numbers
C      nonul - number of null columns
C      clnul - array of columns which have nulls
C      snul - array of string values to represent the null values
C      status  - error number
C
C PRIMARY LOCAL VARIABLES:
C      keyval  - keyword value
C      keyroot - root name for keyword
C      comment - comment for keyword
C
C CALLED ROUTINES:
C      subroutine ftkeyn - build a keyword
C      subroutine ftgkys - get string keyword record
C      subroutine ftpkys - put string keyword record
C      subroutine ftgkyd - get double keyword record
C      subroutine ftpkyd - put double keyword record
C
C******************************************************************************
      subroutine fimvtk(iunit,ounit,nocolumns,map,
     &     nonul,clnul,snul,status)

      integer    maxcl
      parameter  ( maxcl = 999 )
      integer    iunit,ounit,nocolumns,map(maxcl),status
      integer    i,nonul,clnul(maxcl)
      double precision  value
      character(16)  keyroot, keyword, snul(maxcl)
      character(80)  keyval, comment

      keyroot = 'TNULL'
      nonul = 0

      do 10 i = 1, nocolumns
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkys(iunit,keyword,keyval,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkys(ounit,keyword,keyval,comment,status)
            nonul = nonul + 1
            snul(nonul) = keyval
            clnul(nonul) = i
         else
            status = 0
         endif
 10   continue

      do 20 i = 1,nocolumns
         keyroot = 'TSCAL'
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkyd(iunit,keyword,value,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkyd(ounit,keyword,value,15,comment,status)
         else
            status = 0
         endif
         keyroot = 'TZERO'
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkyd(iunit,keyword,value,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkyd(ounit,keyword,value,15,comment,status)
         else
            status = 0
         endif
C --- Alex
         keyroot = 'TLMIN'
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkyd(iunit,keyword,value,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkyd(ounit,keyword,value,15,comment,status)
         else
            status = 0
         endif
         keyroot = 'TLMAX'
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkyd(iunit,keyword,value,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkyd(ounit,keyword,value,15,comment,status)
         else
            status = 0
         endif
         keyroot = 'TCTYP'
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkyd(iunit,keyword,value,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkyd(ounit,keyword,value,15,comment,status)
         else
            status = 0
         endif
         keyroot = 'TCRCL'
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkyd(iunit,keyword,value,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkyd(ounit,keyword,value,15,comment,status)
         else
            status = 0
         endif
         keyroot = 'TCDLT'
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkyd(iunit,keyword,value,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkyd(ounit,keyword,value,15,comment,status)
         else
            status = 0
         endif
         keyroot = 'TCRPX'
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkyd(iunit,keyword,value,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkyd(ounit,keyword,value,15,comment,status)
         else
            status = 0
         endif
         keyroot = 'TCROT'
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkyd(iunit,keyword,value,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkyd(ounit,keyword,value,15,comment,status)
         else
            status = 0
         endif
         keyroot = 'TCRVL'
         call ftkeyn(keyroot,map(i),keyword,status)
         call ftgkyd(iunit,keyword,value,comment,status)
         if ( status .eq. 0 ) then
            call ftkeyn(keyroot,i,keyword,status)
            call ftpkyd(ounit,keyword,value,15,comment,status)
         else
            status = 0
         endif
C ---
 20   continue
      return
      end

C******************************************************************************
C SUBROUTINE:
C      fchfmt
C
C DESCRIPTION:
C      change TFORM keywords from ASCII to BINARY or BINARY to ACSII
C
C AUTHOR/DATE:
C      Emily A. Greene
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C
C USAGE:
C      call fchfmt (ihtype, ohtype, tform, ncols, status)
C
C ARGUMENTS:
C       ihtype - header type of input file
C       ohtype - header type of output file
C       tform  - the input and output format
C       ncols  - number of columns
C      status  - error number
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine fchfmt (ihtype,ohtype,tform,ncols,status)

      integer ihtype, ohtype, ncols, status
      character*(*) tform(ncols)

      character(80) context, width_str
      integer dtype, repeat, width, i, j, k


      status = 0      

      if (ihtype .eq. ohtype) return

      if (ihtype .eq. 1) then
C ASCII to BINARY transformation
         do 100 i = 1, ncols
            call ftasfm (tform(i),dtype,width,repeat,status)
            if (dtype .eq. 41) then
               tform(i) = 'J'
            else if (dtype .eq. 16) then
c              Invert order of Aw to wA for binary table:
               write(width_str,'(i10)') width
               k = 0
               do j = 1, len(width_str)
                  if (width_str(j:j) .ne. ' ') then
                     k = k + 1
                     tform(i)(k:k) = width_str(j:j)
                  endif
               enddo
               tform(i) = tform(i)(1:k) // 'A'
            else if (dtype .eq. 42) then
               tform(i) = 'E'
            else if (dtype .eq. 82) then
               tform(i) = 'D'
            endif
 100     continue

      else

C BINARY to ASCII transformation

         do 200 i = 1, ncols
            call ftbnfm (tform(i), dtype, repeat, width,
     &           status)
            if (dtype .lt. 0) then
               context = ' Cannot turn variable length'
     &              // ' records into ASCII table'
               call fcerr (context)
               status = 1
               goto 999
            else if (dtype .eq. 16) then
               if (repeat .ne. width) then
                  context = ' Cannot turn vector column'
     &                 // ' into ASCII table'
                  call fcerr (context)
                  status = 1
                  goto 999
               endif
            else if (repeat .gt. 1) then
               context = ' Cannot turn vector column'
     &              // ' into ASCII table'
               call fcerr (context)
               status = 1
               goto 999
            else if (dtype .eq. 11) then
               tform(i) = 'I10'
            else if (dtype .eq. 14) then
               context = ' Logical in ASCII table is'
     &              // ' not supported'
               call fcerr (context)
               status = 1
               goto 999
c                               tform(i) = 'A1'
            else if (dtype .eq. 21) then
               tform(i) = 'I10'
            else if (dtype .eq. 41) then
               tform(i) = 'I10'
            else if (dtype .eq. 42) then
               tform(i) = 'E14.7'
            else if (dtype .eq. 82) then
               tform(i) = 'D23.15'
            else
               context = ' Cannot convert complex data'
     &              // ' into ASCII table'
               call fcerr (context)
               status = 1
               goto 999
            endif

 200     continue
      endif

 999  return
      end

C******************************************************************************
C SUBROUTINE:
C      ftabnl
C
C DESCRIPTION:
C      check that Nulls from ASCII table will be OK in BINARY table
C
C AUTHOR/DATE:
C      Emily A. Greene
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C
C USAGE:
C       call ftabnl (ounit, nonul, clnul, snul, nform, inull, status)
C
C ARGUMENTS:
C       ounit  - output unit number
C       nonul  - number of nulls defined
C       clnul  - column number of the null
C       snul   - string containing null
C       nform  - the format of the column
C       inull  - the integer value to use
C      status  - error number
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine ftabnl(ounit,nonul,clnul,snul,nform,inull,status)

      integer maxcl
      parameter (maxcl = 999)

      integer nonul, clnul(nonul), status, inull, ounit
      character*(*) snul(nonul), nform(maxcl)

      integer i, ivalue
      double precision dvalue
      character(8) keyword

      status = 0

      do 100 i = 1, nonul
C ascii columns are OK regardless
         if (index(nform(clnul(i)),'A') .gt. 0) then
            goto 100
         else if (index(nform(clnul(i)),'J') .gt. 0) then
C try reading as an integer
            read(snul(i),'(i10)',err=999) ivalue
         else
C try reading as a double
            read(snul(i),'(D23.15)',err=999) dvalue
         endif
         goto 100

C we need to use inull as the null value
 999     write (snul(i), '(i10)') inull
         call ftkeyn ('TNULL', clnul(i), keyword, status)
         call ftmkyj (ounit, keyword, inull, '&', status)

 100  continue

      return
      end

C******************************************************************************
C SUBROUTINE:
C      fistbn
C
C DESCRIPTION:
C      Reset TNULL values for a binary table extension
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HISTORY:
C      6/17/93 EAG Input NULL value is a string
C
C NOTES:
C
C
C USAGE:
C      call fistbn(ounit,nonul,clnul,snul,status)
C
C ARGUMENTS:
C      ounit  - input FITS file unit number
C      nocol  - number of columns
C      clnul - array of columns which have nulls
C      snul - array of string values to represent the null values
C      status - return status variable
C
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED ROUTINES:
C      subroutine fttnul - set binary table nulls
C
C******************************************************************************
      subroutine fistbn(ounit,nonul,clnul,snul,status)

      integer ounit,nonul,clnul(nonul),inul,status
      character*(*) snul(nonul)
      integer i
      character(80) context, keyword

      status = 0

      do 10 i = 1, nonul
         read (snul(i), '(I10)', err=999) inul
         call fttnul(ounit,clnul(i),inul,status)
         call ftkeyn ('TNULL', clnul(i), keyword, status)
         call ftmkyj (ounit, keyword, inul, '&', status)
 10   continue
      return

 999  context = ' Error parsing NULL keyword in BINARY table'
      call fcerr (context)
      status = 1

      return
      end


C******************************************************************************
C SUBROUTINE:
C      fistan
C
C DESCRIPTION:
C      Reset TNULL values for an ascii table extension
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C
C USAGE:
C      call fistan(ounit,nonul,clnul,snul,status)
C
C ARGUMENTS:
C      ounit  - input FITS file unit number
C      nonul  - number of columns with nulls
C      clnul - array of columns which have nulls
C      snul - array of string values to represent the null values
C      status - return status variable
C
C PRIMARY LOCAL VARIABLES:
C
C
C CALLED ROUTINES:
C      subroutine ftsnul - set ascii table nulls
C
C******************************************************************************
      subroutine fistan(ounit,nonul,clnul,snul,status)

      integer maxcl
      parameter ( maxcl = 999 )
      integer ounit,nonul,clnul(maxcl),status
      character*(*) snul(maxcl)
      integer i
     
      status = 0

      do 10 i = 1, nonul
         call ftsnul(ounit,clnul(i),snul(i),status)
 10   continue
      return
      end

