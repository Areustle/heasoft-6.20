C******************************************************************************
C SELECTOR TASK:
C      fappend
C
C FILE:
C      fappend.f
C
C DESCRIPTION:
C      Appends a extension from one FITS file to another FITS file.
C
C AUTHOR/DATE:
C      Janice Tarrant  1/28/92
C
C MODIFICATION HISTORY:
C       10/22/92 (EAG) - added history parameter: whether to add
C                        history records to outfile
C       12/21/92 (EAG) - modified to accept a list of files to append
C       8/19/94 3.0a (EAG) - neaten up
C       9/19/95 3.0b (Srilal) - timestamp added
C
C NOTES:
C      fappend supported in IRAF and HOST environments
C
C USAGE:
C      HOST: call fapped
C      IRAF: task fapped
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile    - input FITS file and extension number
C      outfile   - output FITS file
C      pkeywords - copy extra primary keywords flag
C       history - whether to add history keyword to outfile
C
C CALLED ROUTINES:
C      subroutine gapped - gets parameters from parameter file
C      subroutine fiapex - appends extension to FITS file
C
C******************************************************************************
      subroutine fapped
      character(160) infile, outfile
      logical pkeywords, history
      character(40) taskname
      common /task/ taskname

      taskname = 'fappend3.0a'

      call ftcmsg

C  get parameters from parameter
      call gapped(infile,outfile,pkeywords, history)

C  append extension to FITS file
      call fiapex(infile,outfile,pkeywords, history)

      return
      end



C******************************************************************************
C SUBROUTINE:
C      gapped
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Janice Tarrant  1/28/92
C
C MODIFICATION HISTORY:
C	10/22/92 (EAG) - added HISTORY keyword
C
C NOTES:
C       gapped uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gapped(infile,outfile,pkeywords, history)
C
C ARGUMENTS:
C      infile    - input FITS file and extension number
C      outfile   - output FITS file
C      pkeywords - copy extra primary keywords flag
C	history - whether to add history keyword to outfile
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C****************************************************************************** 
      subroutine gapped(infile,outfile,pkeywords, history)
      character*(*) infile, outfile
      logical pkeywords, history

      character(80) context
      integer status

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

C  get the primary keywords flag
      call uclgsb('pkeywds',pkeywords,status)
      if (status .ne. 0) then
         context = 'could not get PKEYWDS parameter'
         call fcerr(context)
         goto 999
      endif

C  get the history flag
      call uclgsb('history', history,status)
      if (status .ne. 0) then
         context = 'could not get HISTORY parameter'
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



C******************************************************************************
C SUBROUTINE:
C      fiapex
C
C DESCRIPTION: 
C      Appends an extension to another FITS file
C
C AUTHOR:  
C      Janice Tarrant  1/28/92
C
C MODIFICATION HISTORY:
C	10/22/92 (EAG) - added history parameter
C
C NOTES:
C       fiapex uses FITSIO calls to read and write to FITS file
C
C USAGE:
C      call fiapex(infile,outfile,pkeywords, history)
C
C ARGUMENTS:
C      infile    - input FITS file and extension number
C      outfile   - output FITS file
C      pkeywords - copy extra primary keywords flag
C	history  - whether to write history keyword to outfile
C
C PRIMARY LOCAL VARIABLES:
C      filename  - name of FITS file
C      context   - error message 
C      errstr    - fitsio error message string
C      history   - HISTORY keyword comment string
C      extnum    - FITS file extension number
C      ftstatus  - fitsio error number
C      htype     - FITS header type
C      iunit     - input unit number
C      ounit     - output unit number
C      block     - blocksize
C
C CALLED ROUTINES:
C      subroutine fcerrm - echo error message to terminal
C      subroutine fcecho - echo message to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine fimpxk - move primary's extra keywords
C      subroutine ftclos - close a FITS file
C      subroutine ftcopy - copy verbatum a FITS extension
C      subroutine ftcrhd - create a new header
C      subroutine ftmrhd - relative move to FITS header
C      subroutine ftopen - open a FITS file
C      subroutine ftphis - put history keyword record
C
C****************************************************************************** 
      subroutine fiapex(infile,outfile,pkeywords, history)
      character*(*) infile, outfile
      logical pkeywords, history
      character(160) filename
      character(80) context, errstr, hist
      logical inopen, outopen
      integer extnum, ftstatus, iunit, block, ounit, htype, nkeys,
     &     nmore, nexkeys, ncomms, nhists

      integer i, numfiles, maxf
      parameter (maxf = 512)
      character(160) filelist(maxf)
      logical negflag

C  initialize variables
      ftstatus = 0
      iunit = 15
      ounit = 16
      inopen = .false.
      outopen = .false.

C  open the output FITS file
      call ftopen(ounit,outfile,1,block,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to open outfile'
         call fcerr(context)
         goto 999
      endif
      outopen = .true.

C  move to the end of the output file (look for end-of-file error message)
 10   call ftmrhd(ounit,1,htype,ftstatus)
      if (ftstatus .eq. 0)  goto 10
      if (ftstatus .eq. 107) then
         ftstatus=0
      else
         context='error finding end of outfile'
         call fcerr(context)
         goto 999
      end if

C get list of input filenames
      call fcgcls (infile, filelist, numfiles, negflag)

C loop over files to be appended
      do 100 i = 1, numfiles

C  get the filename and extension number
         call fcpars(filelist(i),filename,extnum,ftstatus)

C EAG 8/25/93 default to first extension
         if (extnum .eq. -99) extnum = 1

C  open the input FITS file
         if (inopen) call ftclos(iunit, ftstatus)
         call ftopen(iunit,filename,0,block,ftstatus)
         if (ftstatus .ne. 0) then
	    context = 'unable to open infile'
	    call fcerr(context)
	    goto 999
         endif
         inopen = .true.

C  check that extension exists
         if (extnum .ge. 1) then
	    call ftmrhd(iunit,extnum,htype,ftstatus)
	    if (ftstatus .ne. 0) then
               errstr = 'error moving to extension number '
               write(context,1000) errstr,extnum
 1000          format(A34,I3)
               call fcerr(context)
               goto 999
	    endif
         endif

C  get the number of extra keywords, including comment and history keywords
         ncomms = 4
         nhists = 1
         if (pkeywords) then
	    call ftmahd(iunit,1,htype,ftstatus)
	    call ftghsp(iunit,nkeys,nmore,ftstatus)
	    nexkeys = nkeys + ncomms + nhists
	    call ftmrhd(iunit,extnum,htype,ftstatus)
         else
	    nexkeys = 1
         endif

C  create the new empty extension in the output file
         call ftcrhd(ounit,ftstatus)

C  copy the extension from the input file to the output file
         call ftcopy(iunit,ounit,nexkeys,ftstatus)

C  append the extra keywords to the output file
         if (pkeywords .and. extnum .gt. 0) then
	    call ftmahd(iunit,1,htype,ftstatus)
	    call fimpxk(iunit,ounit,ftstatus)
         endif

C  append a history keyword to the new file
         if (history) then
            hist = 'TASK: FAPPND on FILENAME: '//infile
            call ftphis(ounit,hist,ftstatus)
	    call timestamp(ounit)
         endif

C loop back for any more extension
         if (ftstatus .ne. 0) goto 999
 100  continue

C  close the FITS files, exit on fitsio error
 999  continue
      if (ftstatus .ne. 0) then
         call fcerrm(ftstatus)
         ftstatus = 0
      end if

C attempt to physically close the files, regardless of any error condition
      if (inopen)   call ftclos(iunit,ftstatus)
      if (outopen)  call ftclos(ounit,ftstatus)
      return
      end



C******************************************************************************
C SUBROUTINE:
C      fimpxk
C
C DESCRIPTION:
C      Moves the primary's extra keywords, i.e., the keywords which
C      don't contain: SIMPLE, BITPIX, NAXIS, EXTEND and END from the
C      input file to the output file
C
C AUTHOR/DATE:
C      Janice Tarrant  1/7/92
C
C MODIFICATION HISTORY:
C       
C NOTES:
C       
C USAGE:
C      call fimpxk(iunit,ounit,ftstatus)
C
C ARGUMENTS:
C      iunit  - input unit number
C      ounit  - output unit number
C      ftstatus - error number
C
C PRIMARY LOCAL VARIABLES:
C      i*      - index to substring 
C      l*      - substring presence flag
C      nkeys   - number of keywords
C      copyflg - copy keyword flag
C
C CALLED ROUTINES:
C      subroutine ftghsp - get number of keywords in the CHU
C      subroutine ftgrec - get keyword record
C      subroutine ftpcom - put comment keyword
C      subroutine ftprec - put keyword record
C
C******************************************************************************
      subroutine fimpxk(iunit,ounit,ftstatus)
      integer iunit,ounit,ftstatus

      integer i1,i2,i3,i4
      integer i,nkeys,nmore
      logical l1,l2,l3,l4,copyflg
      character(80)  record

C  deliniate the extra keywords with comments
      record = ' '
      call ftprec(ounit,record,ftstatus)
      record = 'The following are the optional keywords from the '
      call ftpcom(ounit,record,ftstatus)
      record = 'primary header unit for this extension:'
      call ftpcom(ounit,record,ftstatus)

C  get the number of keywords in the header
      call ftghsp(iunit,nkeys,nmore,ftstatus)

C  if keyword is not 'simple', add it to the new header
      do 10 i = 1, nkeys
         call ftgrec(iunit,i,record,ftstatus)
         i1 = index(record(1:8),'SIMPLE  ')
         i2 = index(record(1:8),'BITPIX  ')
         i3 = index(record(1:5),'NAXIS')
         i4 = index(record(1:8),'EXTEND  ')
         l1 = i1 .eq. 0
         l2 = i2 .eq. 0
         l3 = i3 .eq. 0
         l4 = i4 .eq. 0
         copyflg = l1 .and. l2 .and. l3 .and. l4
         if (copyflg .and. (record .ne. ' ')) then
            call ftprec(ounit,record,ftstatus)
         end if
 10   continue
      record = ' '
      call ftprec(ounit,record,ftstatus)

      return
      end
