*****************************************************************************e 
C SELECTOR TASK:
C      fmrgmsk 
C
C FILE:
C      fmrgmsk.f 
C
C DESCRIPTION: 
C      Creates a an output image which is a result of masking of two images.
C
C AUTHOR:  
C      Vidya Sagar. Aug '92
C
C MODIFICATION HISTORY:
C	4/7/93 EAG - fixed bugs in AND and XOR mode, increases size allowed
C	4/9/93 EAG - re-vamp to allow for infinite sized masks
C	4/20/93 EAG - added SUB option
C      12/03/97 PDW 2.2c - Introduce clobber parameter... sort of...
C                          call ffinit--an ftool utility--instead of ftinit
C      12/20/99 NG 2.3 - Updated for reading compressed images.
C
C NOTES:
C      fmrgmsk is supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      inmsk1 	 - Ist input FITS file and extension number
C      inmsk2 	 - 2nd input FITS file and extension number
C      outfil   - output FITS(masked) file
C      ops       - masking operation to be performed (Egs: OR, AND)
C
C CALLED ROUTINES:
C      subroutine gmrgmk - gets parameters from environment
C      subroutine f2dwrt - write result to output MASK file.
C
C****************************************************************************** 
	subroutine fmrgmk
C
	character *160 	inmsk1 
	character *160 	inmsk2 
	character *160 	outfil 
	character *160 	files(100)
        character * 4   ops

        integer         bitpx
        integer         naxis1
        integer         naxis2
        integer         no
        integer         status

C Set up an area to hold the data while processing.  The maximum number
C	of open FITS file is 12, and pick the number of elements to
C	process at once to be 1024 (or a 32 X 32 array)
	integer		maxdim
	integer		maxfiles
	parameter (maxdim = 1024)
	parameter (maxfiles = 12)

        real            msk1v(maxdim*2)
	common /ftgcmm/ msk1v

	character(40) taskname
	common /task/ taskname

	taskname = 'fmrgmsk2.2c'
	inmsk1 = ' '
	inmsk2 = ' '
        bitpx = 0
        naxis1 = 0
        naxis2 = 0
        status = 0

C  get the parameters from the par file
	call gmrgmk(inmsk1,inmsk2,outfil,ops,status)

C  check if MASK files are in order.

        call gchkfl(inmsk1,inmsk2,files,no,naxis1,
     +              naxis2,bitpx,status)
        if (status .ne. 0) return

C  read in the FITS files perform operations and write the results
C  to the output MASK file.
C  the data area, msk1v, is split up here so that the first maxdim
C  values are used for the mask that will eventually be output and
C  the rest of the area is for the maxfiles-1 possible input files
	call f2dwrt(inmsk1,inmsk2,outfil,msk1v,msk1v(maxdim+1),
     +              files,no,bitpx,naxis1,naxis2,ops,status)

	return
	end

C****************************************************************************** 
C SUBROUTINE:
C     gmrgmk 
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gmrgmk uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gmrgmk(inmsk1,inmsk2,outfil,ops,status)
C
C ARGUMENTS:
C      inmsk1 	 - Ist input FITS file and extension number
C      inmsk2 	 - 2nd input FITS file and extension number
C      outfil   - output FITS(masked) file
C      ops       - masking operation to be performed (Egs: OR, AND)
C
C PRIMARY LOCAL VARIABLES:
C      contxt - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C
C****************************************************************************** 
	subroutine gmrgmk(inmsk1,inmsk2,outfil,ops, status)
C
	character*(*) inmsk1,inmsk2
        character*(*) outfil
        character(4)   ops
        character(80)  contxt

        integer status

C  initialize variables
	status = 0

C  get the name of the input FITS file
	call uclgst('inmsk1',inmsk1,status)
	if (status .ne. 0) then
	    contxt = 'could not get INMASK1 parameter'
	    call fcerr(contxt)
	    goto 999
	endif


C  get the name of the input FITS file
	call uclgst('inmsk2',inmsk2,status)
	if (status .ne. 0) then
	    contxt = 'could not get INMASK2 parameter'
	    call fcerr(contxt)
	    goto 999
	endif

C  get the name of the output FITS file
	call uclgst('outfil',outfil,status)
	if (status .ne. 0) then
	    contxt = 'could not get OUTMSK parameter'
	    call fcerr(contxt)
	    goto 999
	endif

C  get the ops variable name
	call uclgst('ops',ops,status)
	if (status .ne. 0) then
	    contxt = 'could not get OPS parameter'
	    call fcerr(contxt)
	    goto 999
	endif

999	continue

	return
	end
C****************************************************************************** 
C SUBROUTINE:
C      f2dwrt
C
C DESCRIPTION: 
C      Reads 2-d images from input files and the resultant mask obtained by
C      performing the mask operation requested by the user.
C
C AUTHOR:  
C      Vidya Sagar Aug '92 
C
C MODIFICATION HISTORY:
C	4/9/93 EAG - modified to allow for infinite image size
C
C NOTES:
C      f2dwrt uses FITSIO calls to read image file
C
C USAGE:
C	call f2dwrt(inmsk1,inmsk2,outfil,files,no,bitpx,
C                 naxis1,naxis2,ops,status)
C
C ARGUMENTS:
C      inmsk1 	 - Ist input FITS file and extension number
C      inmsk2 	 - 2nd input FITS file and extension number
C      outfil   - output FITS(masked) file
C	files   - array of input file names
C      bitpx    - format of data
C      naxis1    - Ist dimension of array
C      naxis2    - wnd dimesnion of array
C      ops       - masking operation to be performed (Egs: OR, AND)
C	status    - status of operation
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ftclos - close a FITS file
C      subroutine ftgbnh - get BINARY table header
C      subroutine ftgtbh - get ASCII table header
C      subroutine ftmahd - absolute move to FITS header
C      subroutine ftopen - open a FITS file
C
C****************************************************************************** 
	subroutine f2dwrt(inmsk1,inmsk2,outfil,msk1v,msk2v,
     +             files,no,bitpx,naxis1,naxis2,ops,status)

        integer no
        character*(*) inmsk1, inmsk2
	character*(*) outfil
        character*(*) files(no)
	character(160) filename
	character(80)  contxt
        character(4)   ops

        integer i
        integer j
        integer block
        integer bitpx
	integer naxis1,naxis2
	integer status,ounit, extnum, oextnum
        integer hdtype
        integer pcount,gcount,naxis,naxes(2)
        integer k
        integer nvals, remain, fpixel, nelem
	integer filesleft, filebegin, fileend

	integer maxdim
	integer maxfiles
	parameter (maxdim = 1024)
	parameter (maxfiles = 12)
      
        real    msk1v(maxdim)
        real    msk2v(maxdim)
        integer iunit(maxfiles-1)
 
        logical anyf
        logical simple
        logical extend
        logical temp1,temp2
        logical ntemp1,ntemp2
	logical flagvals(maxdim)

C  initialize variables

C  get the input FITS filename and extension number
	do 10 i = 1, maxfiles-1
        iunit(i) = 13 + i
10	continue
        ounit = 25
        nvals = naxis1 * naxis2
        status = 0

C --- First open the first input file which contains the mask pattern.

	call fcpars(files(1),filename,extnum,status)
C EAG 8/25/93 default to primary array
	if (extnum .eq. -99) extnum = 0

C  open the input file.

	call ftopen(iunit(1),filename,0,block,status)

	if (status .ne. 0) then
	    contxt = 'unable to open infile'
	    call fcerr(contxt)
	    return
	endif

C and copy it to the output file for our basis
	call fcpars (outfil, filename, oextnum, status)
C EAG 8/25/93 default to primary array
	if (oextnum .eq. -99) oextnum = 0

	call ffinit (ounit, filename, status)
	if (oextnum .eq. 0) then
		call ftcopy (iunit(1), ounit, 0, status)
	else
C write a simple primary array header since we're putting this
C	into an image extension
		simple = .true.
		naxis = 0
		naxes(1) = 0
		naxes(2) = 0
		pcount = 0
		gcount = 1
		extend = .true.
		call ftphpr (ounit, simple, bitpx, naxis, naxes, pcount,
     &                       gcount, extend, status)
		call ftpdef (ounit, bitpx, naxis, naxes, pcount, gcount,
     &                       status)
        	call ftmahd(iunit(1),extnum+1,hdtype,status)
		call ftcrhd (ounit, status)
		call ftcopy (iunit(1), ounit, 0, status)
	endif
	call ftclos (iunit(1), status)

        if (status .ne. 0)  goto 999

C now loop over the input files in sets of maxfiles-1
	do 900 filebegin = 2, no, maxfiles-1
	   filesleft = no - filebegin + 1

C figure how many files we are dealing with in this loop
	   fileend = min(maxfiles-1, filesleft)

C open up all the input files  and move to correct extension
	   do 100 i = filebegin, fileend+filebegin-1
		call fcpars (files(i), filename, extnum, status)
C EAG 8/25/93 default to primary array
		if (extnum .eq. -99) extnum = 0
		call ftopen (iunit(i-filebegin+1), filename, 0, block,
     &                       status)
		call ftmahd (iunit(i-filebegin+1), extnum+1, hdtype,
     &                       status)
100	   continue

C and now loop over the array, grabbing 1024 elements at a time
	   remain = nvals
	   fpixel = 1
200	   if (remain .ge. maxdim) then
		nelem = maxdim
	   else
		nelem = remain
	   endif

C	first the "main" one
	   anyf = .false.
	   call ftgpfe (ounit, 1, fpixel, nelem, msk1v, flagvals,
     &                  anyf, status)

C --- Now read the rest of the files

        do 301 k = 1, fileend

	   call ftgpfe (iunit(k), 1, fpixel, nelem, msk2v, flagvals,
     &                  anyf, status)
          if (status .ne. 0)  goto 870

          if (ops .eq. 'OR') then
                 do 1202 j = 1, nelem
                   temp1 = .false.
                   temp2 = .false.
                   if (msk1v(j) .ge. 1) temp1 = .true.
                   if (msk2v(j) .ge. 1) temp2 = .true.
                   if (temp1 .or. temp2) msk1v(j) = 1.0
1202             continue

          else if (ops .eq. 'AND') then
              do 2202 j = 1, nelem
                   temp1 = .false.
                   temp2 = .false.
                   if (msk1v(j) .ge. 1.0) temp1 = .true.
                   if (msk2v(j) .ge. 1.0) temp2 = .true.
                   msk1v(j) = 0.0
                   if (temp1 .and. temp2) msk1v(j) = 1.0
2202           continue

          else if (ops .eq. 'XOR') then
              do 3202 j = 1, nelem
                   temp1 = .false.
                   temp2 = .false.
                   if (msk1v(j) .ge. 1.0) temp1 = .true.
                   if (msk2v(j) .ge. 1.0) temp2 = .true.
                   msk1v(j) = 0.0
                   ntemp1 = (.not. temp1)
                   ntemp2 = (.not. temp2)
                   if ((temp1 .and. ntemp2) .or. (temp2 .and. ntemp1))
     +                 msk1v(j) = 1.0
 
3202           continue

          else if (ops .eq. 'ADD') then
              do 4202 j = 1, nelem
                 msk1v(j) = msk1v(j) + msk2v(j) 
4202          continue

           else if (ops .eq. 'SUB') then
             do 5202 j = 1, nelem
               msk1v(j) = msk1v(j) - msk2v(j)
5202         continue

          else
          endif
301    continue

       status = 0
       call ftppre(ounit,1,fpixel,nelem,msk1v,status)

	remain = remain - nelem
	fpixel = fpixel + nelem
	if (remain .gt. 0) goto 200

870	if (status .ne. 0) call fcerrm(status)
	do 880 i = 1, fileend
		status = 0
		call ftclos(iunit(i), status)
880	continue

C update the files left

900	continue

999    continue
	if (status .ne. 0) call fcerrm(status)
	status = 0
	call ftclos (ounit, status)


       return
       end


C****************************************************************************** 
C SUBROUTINE:
C      gkywrs
C
C DESCRIPTION: 
C      Get necessary keywords from input FITS file.
C
C AUTHOR:  
C        S.R.K. Vidya Sagar (HSTX)
C
C MODIFICATION HISTORY:
C
C USAGE:
C      call gkywrs(infile,bitpx,naxis1,naxis2,status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      bitpx   - denotes format of input values
C      naxis1   - Ist dimesnion of input image array,
C      naxis2   - 2nd dimesnion of input image array,
C
C PRIMARY LOCAL VARIABLES:
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C
C****************************************************************************** 

        subroutine gkywrs(infile,bitpx,naxis1,naxis2,status)

        integer iunit
        integer extnum
        integer block
        integer naxis1
        integer naxis2
        integer bitpx
        integer hdtype
        integer status, ftstat

        character*(*) infile
        character(160) filnm
        character(80) contxt
        integer*4 naxes(2)
        integer naxis

C  open the input FITS file

        iunit = 15
        status = 0

	call fcpars(infile,filnm,extnum,status)
C EAG 8/25/93 default to primary array
	if (extnum .eq. -99) extnum = 0
	call ftopen(iunit,filnm,0,block,status)

	if (status .ne. 0) then
	    contxt = 'unable to open infile'
	    call fcerr(contxt)
	    return
	endif
     
        call ftmahd(iunit,extnum+1,hdtype,status)
	if (hdtype .ne. 0) then
		contxt = ' FMRGMSK only works on images'
		call fcerr (contxt)
		status = 1
	endif

C        call ftgkyj(iunit,'BITPIX',bitpx,contxt,status)
C        call ftgkyj(iunit,'NAXIS1',naxis1,contxt,status)
C        call ftgkyj(iunit,'NAXIS2',naxis2,contxt,status)
        call ftgipr(iunit,2,bitpx,naxis, naxes,status)
        naxis1 = naxes(1)
        naxis2 = naxes(2)

	if (status .ne. 0)  call fcerrm(status)
	ftstat = 0
        call ftclos(iunit,ftstat)

        return
        end

***************************************************************************** 
C
C      gchkfl
C
C
C DESCRIPTION: 
C      Checks to see if input MASK files are more than one.
C
C AUTHOR:  
C      Vidya Sagar. Aug '92
C
C MODIFICATION HISTORY:
C	4/9/93 EAG - allow for either/both input files to be in a file
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      inmsk1 	 - Ist input FITS file and extension number
C      inmsk2 	 - 2nd input FITS file and extension number
C      files     - array containing the file names
C
***************************************************************************** 

        subroutine gchkfl(inmsk1,inmsk2,files,no,naxis1,
     +                    naxis2,bitpx,status)

        character*(*) inmsk1,inmsk2
        character(160) files(100)
	character(80)  contxt

        integer      no
        integer      naxis1
        integer      naxis2
        integer      bitpx
        integer      status 
	integer	newfiles
	logical negflag

C --- Check to see if the first file is a single file or
C --- it is a file which contains filenames.
C --- "@" at the first character position  denotes that the value
C --- contained in INMASK1 denotes a filename containing filenames.

        status = 0

C find the file(s) in the first input string
	if (inmsk1 .ne. ' ') then
		call fcgcls (inmsk1, files, no, negflag)
	else
		no = 0
	endif

C and the file(s) in the second one
	if (inmsk2 .ne. ' ') call fcgcls (inmsk2, files(no+1), newfiles,
     &                                    negflag)
	no = no + newfiles

	if (no .gt. 100) then
		contxt = ' Too many files requested, proceeding with 100'
		call fcecho (contxt)
		no = 100
	else if (no .le. 0) then
		contxt = ' No input files found'
		call fcerr (contxt)
		status = 1
		return
	endif

        call gchkpr(files,no,naxis1,naxis2,bitpx,status)

        return
        end

***************************************************************************** 
C      gchkpr
C
C DESCRIPTION: 
C      Checks to see keywords naxis1 and naxis2 of different 
C      mask files are the same.
C
C AUTHOR:  
C      Vidya Sagar. Aug '92
C
C MODIFICATION HISTORY:
C	4/9/93 EAG all files in files variable, checked correctly
C
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      files     - array containing the file names
C	no       - number of files in files
C	naxis1   - returned naxis1 value
C	naxis2   - returned naxis2 value
C	bitpx    - returned bitpix value
C
***************************************************************************** 


        subroutine gchkpr(files,no,naxis1,naxis2,bitpx,status)

        integer no 
        character(160) files(no)
        
        integer i
        integer bitpx1
        integer bitpx2
        integer bitpx
        integer minbpx
        integer maxbpx
        integer naxis1
        integer naxis2
        integer naxis3
        integer naxis4

        integer status

        logical ok

        ok = .false.
        status = 0

        call gkywrs(files(1),bitpx1,naxis1,naxis2,status)
        minbpx = bitpx1
        maxbpx = bitpx1

        do 10 i = 2, no

           call gkywrs(files(i),bitpx2,naxis3,naxis4,status)
           ok =((naxis1 .eq. naxis3) .and. (naxis2 .eq. naxis4))
           minbpx = min(minbpx,bitpx2)
           maxbpx = max(maxbpx,bitpx2)

           if (.not. ok) then
               status = 1
               call fcerr('keywords of MASK files do not match')
               return
           endif

10      continue

        if (minbpx .lt. 0) then
           bitpx = minbpx
        else
           bitpx = maxbpx
        endif

        return
        end
