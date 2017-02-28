C******************************************************************************
C SELECTOR TASK:
C      fextract
C
C FILE:
C      fextract.f
C
C DESCRIPTION:
C      Extracts a primary array or extension from one FITS file and
C      makes a new FITS file.
C
C AUTHOR/DATE:
C      Janice Tarrant  1/6/92
C
C MODIFICATION HISTORY:
C       3/9/93 EAG  Allow for placing binary extension into primary array.or
C                   primary array into image extension.
C       8/23/94 EAG 3.0a - clobber capability
C       2/25/96 (Srilal) 3.0b - timestamp added
C
C NOTES:
C
C USAGE:
C      HOST: call fextct
C      IRAF: task fextct
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile    - input FITS file and extension number
C      outfile   - output FITS file and possible extension number
C
C CALLED ROUTINES:
C      subroutine gextrt - gets parameters from parameter file
C      subroutine fixdat - extracts data to new FITS file
C
C******************************************************************************
      subroutine fextrt
      character(160) infile, outfile
      character(40) taskname
      common /task/ taskname

      taskname = 'fextract3.0b'

      call ftcmsg

C  get parameters from parameter
      call gextrt(infile,outfile)

C  extract data to new FITS file
      call fixdat(infile,outfile)

      return
      end


C******************************************************************************
C SUBROUTINE:
C      gextrt
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Janice Tarrant  1/6/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C       gextrt uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gextrt(infile,outfile)
C
C ARGUMENTS:
C      infile    - input FITS file and extension number
C      outfile   - output FITS file
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
      subroutine gextrt(infile,outfile)
      character*(*) infile, outfile

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

 999  continue
      if (status .ne. 0) then
         call fcerrm(status)
         stop
      endif

      return
      end


C******************************************************************************
C SUBROUTINE:
C      fixdat
C
C DESCRIPTION: 
C      Extracts primary array or extension to make a new FITS file
C
C AUTHOR:  
C      Janice Tarrant  1/6/92
C
C MODIFICATION HISTORY:
C	3/9/93 EAG  Allow for placing image extension in primary array,
C		    or primary array into image extension
C
C NOTES:
C       fixdat uses FITSIO calls to read and write to FITS file
C
C USAGE:
C      call fixdat(infile,outfile)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output FITS file and possible extension number
C
C PRIMARY LOCAL VARIABLES:
C      filename  - name of FITS file
C      context   - error message 
C      errstr    - fitsio error message string
C      history   - HISTORY keyword comment string
C      simple    - flag for simple header
C      extend    - flag for extensions
C      extnum    - FITS file extension number
C      ftstatus  - fitsio error number
C      htype     - FITS header type
C      iunit     - input unit number
C      ounit     - output unit number
C      block     - blocksize
C      bitpix    - bits per pixel
C      naxis     - number of dimensions in primary array
C      naxes     - size of each dimension in primary array
C      pcount    - value of PCOUNT keyword
C      gcount    - value of GCOUNT keyword
C
C CALLED ROUTINES:
C      subroutine fcerrm - echo error message to terminal
C      subroutine fcecho - echo message to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine fimpxk - move primary's extra keywords
C      the FITSIO library - all subroutine names beginning with 'ft'
C
C****************************************************************************** 
      subroutine fixdat(infile,outfile)
      character*(*) infile, outfile
      character(160) filename, file2
      character(80) context, errstr, history
      logical simple, extend, inopen, outopen
      integer extnum, ftstatus, iunit, block, ounit, htype, bitpix,
     &     naxis, naxes(99), pcount, gcount, outext

C  initialize variables
      ftstatus = 0
      iunit = 15
      ounit = 16
      inopen = .false.
      outopen = .false.

C  get the filename and extension number
      call fcpars(infile,filename,extnum,ftstatus)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

C  open the input FITS file
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
 1000       format(A34,I3)
            call fcerr(context)
            goto 999
         endif
c	    call ftmrhd(iunit,-extnum,htype,ftstatus)
      endif

C  get the output filename and extension number
      call fcpars(outfile,file2,outext,ftstatus)

Calex get the root file name from the input file name containing extension 

      call ftrtnm( file2, file2, ftstatus )
      if (ftstatus .ne. 0) then
         context = 'unable to get the root file name'
         call fcerr(context)
         goto 999
      endif      


C EAG 8/25/93 default to 1st extension
      if (outext .eq. -99) outext = 1

C only 0 or 1 output extension number is allowed
      if ((outext .ne. 0) .and. (outext .ne. 1)) then
         context = ' WARNING: output to primary array ' //
     &        'or first extension only'
         call fcecho (context)
         if (extnum .eq. 0) then
            outext = 0
            context = ' placing in primary array'
         else
            outext = 1
            context = ' placing in first extension'
         endif
         call fcecho (context)
      endif

C check for case when input primary array, and no specified extension
C	default for this case is copy to primary array
      if ((extnum .eq. 0) .and. (index(outfile,'[') .le. 0)) 
     &     outext = 0

C also, only primary array or image extension can go to primary array
      if (outext .eq. 0) then
         if (extnum .ne. 0) then
            if (htype .ne. 0) then
               context = ' cannot put table into primary array'
               call fcerr (context)
               goto 999
            endif
         endif
      endif

C  open the output FITS file
      call ffinit(ounit,file2,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to open outfile, may exists? ' // file2
         call fcerr(context)
         goto 999
      endif
      outopen = .true.

C  extract to primary array in the new file
      if (outext .eq. 0) then
         call ftcopy(iunit,ounit,1,ftstatus)

C  add a history keyword to the primary header
         history = 'TASK: FEXTRACT on FILENAME: '//filename
         call ftphis(ounit,history,ftstatus)

C  construct a simple primary header for the new file
      else
         simple = .true.
         bitpix=16
         naxis = 0
         pcount = 0
         gcount = 1
         extend=.true.
         call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,
     &        gcount,extend,ftstatus)
         call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,ftstatus)

C  extract the extension to the new file
c	    call ftmrhd(iunit,extnum,htype,ftstatus)
         call ftcrhd(ounit,ftstatus)
         call ftcopy(iunit,ounit,1,ftstatus)

C  append a history keyword to the new file
         history = 'TASK: FEXTRACT on FILENAME: '//filename
         call ftphis(ounit,history,ftstatus)
	 call timestamp(ounit)
      endif

 999  continue

C  print any non-zero error status
      if (ftstatus .ne. 0) then
         call fcerrm(ftstatus)
         ftstatus = 0
         if (outopen) call ftdelt (ounit, ftstatus)
         outopen = .false.
      endif

C  attempt to close the files, even if there was an error
      ftstatus = 0
      if (inopen)  call ftclos(iunit,ftstatus)
      ftstatus = 0
      if (outopen)  call ftclos(ounit,ftstatus)

      return
      end

