C***************************************************************************** 
C SELECTOR TASK:
C      fkeyprint
C
C AUTHOR:  
C      Vidya Sagar Aug '92 
C
C FILE:
C      fkeyprint.f 
C
C DESCRIPTION: 
C      Extracts the records containing the keyword input by the
C      user.
C
C AUTHOR:  
C      Vidya Sagar. Aug '92
C
C MODIFICATION HISTORY:
C       10/9/92 (EAG) - changed length of keyname string output
C                     - changed output file to hidden parameter
C                     - allow for multiple input files
C       10/26/92 (EAG)- fixed input file not found core dump
C                     - changed output file parameter name to outfile
C       8/25/94 (EAG) 3.0a - clear fitsio error stack, clobber
C       12/13/95 (WDP) - changed to support long continuation keywords
C       01/17/96 (Srilal) - correctly prints 8 characters long keywords.
C
C NOTES:
C      fkeyprint supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile    - input FITS file and extension number
C      outfil   - output file
C      keynam    - Name of the keyword to be extracted
C      exact     - flag to indicate if exact match is neeeded
C
C CALLED ROUTINES:
C      subroutine gkeyp - gets parameters from environment
C      subroutine fextat - Extracts records
C
C****************************************************************************** 
        subroutine fkeypt
C
        character * 8   keynam
        character(160)   infile 
        character(160)   outfil 
       
        logical         exact
     
        integer         ftstat
        character(40) taskname
        common /task/ taskname

        taskname = 'fkeyprint3.2'
        infile = ' '
        outfil = ' '
        ftstat = 0

        call ftcmsg

C  get the parameters from the par file

        call gkeypt(infile,outfil,keynam,exact,ftstat)
        if (ftstat .ne. 0) goto 999

C  Extract records from the FITS file

        call ftupch(keynam)
        call fextat(infile,outfil,keynam,exact,ftstat)

999     continue
        if (ftstat .ne. 0) call fcerrm(ftstat)
        return
        end

C******************************************************************************
C SUBROUTINE:
C     gkeypt 
C
C AUTHOR:  
C      Vidya Sagar Aug '92 
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gkeypt uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gkeypt(infile,outfil,keynam,exact,ftstat)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      outfil   - output file
C      keynam   - keyword the user is looking for
C      exact    - Flag to indicate if an exact match is needed for keynam
C      ftstat   - Status flag. Non-zero means error occured
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C
C****************************************************************************** 
	subroutine gkeypt(infile,outfil,keynam,exact,ftstat)
C
        character(8)   keynam
	character(80)  context
	character*(*) infile,outfil 

	integer ftstat

        logical exact
C
C  initialize variables
C
	ftstat = 0
C
C  get the name of the input FITS file

	call uclgst('infile',infile,ftstat)
	if (ftstat .ne. 0) then
	    context = 'could not get INFILE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the keyword under search.

	call uclgst('keynam',keynam,ftstat)
	if (ftstat .ne. 0) then
	    context = 'could not get KEYNAME parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the OUTPUT file

	call uclgst('outfile',outfil,ftstat)
	if (ftstat .ne. 0) then
	    context = 'could not get OUTPUT parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the EXACT flag (T for exact and F for false for non-exact)

	call uclgsb('exact',exact,ftstat)
	if (ftstat .ne. 0) then
	    context = 'could not get EXACT parameter'
	    call fcerr(context)
	    goto 999
	endif

999     continue

	return
	end

C******************************************************************************
C SUBROUTINE:
C      fextat
C
C DESCRIPTION: 
C      Extract matching records from the FITS file.
C
C AUTHOR:  
C      Vidya Sagar Aug '92 
C
C MODIFICATION HISTORY:
C
C NOTES:
C      fextat uses FITSIO calls to read FITS file
C
C USAGE:
C      call fextat(filnam,outfil,keynam,exact,ftstat)
C
C ARGUMENTS:
C      filnam   - input FITS file and extension number
C      outfil   - output file
C      keynam   - keyword the user is looking for
C      exact    - Flag to indicate if an exact match is needed for keynam
C      ftstat   - Status flag. Non-zero means error occured
C
C PRIMARY LOCAL VARIABLES
C      filnam     - name of FITS file
C      context    - error message 
C      errstr     - fitsio error message string
C      extnum     - FITS file extension number
C
C CALLED ROUTINES:
C      subroutine ftclos - close a FITS file
C      subroutine fcpars - parse off filename and extension number
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - report an error number to terminal
C      subroutine ftmrhd - relative move to FITS header
C      subroutine ftopen - open a FITS file
C      subroutine fcgcls - get list of input file names
C	function fcstln  - the index of the last non-blank character of string
C
C****************************************************************************** 
      subroutine fextat(filnam,outfil,keynam,exact,ftstat)

      integer maxf
      parameter (maxf = 4096)

      character(80)  card
      character(80)  context
      character(80)  context1
      character(80)  errstr
      character*(*) filnam,outfil
      character(160)  infile
      character(8)   keynam
      character(160)  filelist(maxf)

      integer block
      integer extnum
      integer ftstat
      integer hdtype
      integer i
      integer iunit
      integer j
      integer m
      integer ounit
      integer numfiles
      integer count
      integer fcstln

      logical exact
      logical outopen, inopen
      logical print
      logical repeat
      logical negflag

C  initialize variables

      ftstat = 0
      iunit  = 15 
      ounit  = 16
      outopen = .false.
      inopen = .false.

C --- Check to see if the user opted for output to the screen
C --- or a file.

      if (outfil .ne. 'STDOUT') then
         call faopen (ounit, outfil, 2, 0, ftstat)
         if (ftstat .ne. 0) then
            context = 'error opening OUTPUT file, may exist? ' // outfil
            call fcerr (context)
            goto 999
         else
            outopen = .true.
         endif
      else
         outopen = .false.
      endif 
      
C --- Find the length of the string
      do 20 i = 1, 9
         m=i-1
         if (keynam(i:i) .eq. ' ') goto 200
 20   continue

 200  continue

C --- Exit if user entered no keyname to be searched for.

      if (m .eq. 0) then
         call fcerr(' Blank keyname specified - EXITING')
         goto 999 
      endif


      negflag = .false.

C get list of filenames, if any
      call fcgcls (filnam, filelist, numfiles, negflag)

C loop over all files
      do 100 count = 1, numfiles

C initialize some things
         inopen = .false.
         j = 1

C  get the input FITS filename and extension number
         call fcpars(filelist(count),infile,extnum,ftstat)

C EAG 8/25/93 default to all extensions

C --- If user wanted all extensions to be read then do so.

         if (extnum .lt. 0) then
            repeat = .true. 
            extnum = 0
         else
            repeat = .false.
         endif

C  open the input FITS file for read and write

         call ftopen(iunit,infile,0,block,ftstat)

         if (ftstat .ne. 0) then
	    context = ' unable to open: ' //
     &                infile(1:fcstln(infile))
	    call fcerr(context)
	    goto 999
         else
	    inopen = .true.

C -- You are at header 0 so skip advancing to 0 header.

            if (repeat .or. extnum .eq. 0) goto 4000
         endif

 3000    continue

C  move to the extension number

         ftstat = 0
         call ftmahd(iunit,extnum+1,hdtype,ftstat)
         if (ftstat.ne. 0) then
            if (.not. repeat) then
               errstr = 'error moving to extension number '
               write(context1,34) errstr, extnum
               call fcerr(context1)
               goto 999
            else
               ftstat = 0
               goto 99
            endif
         endif

 4000    continue

C --- Remember the extension number as a character string

         write(context,31)extnum

C --- if output is a file then write to it to the file.
C --- if output is the terminal then display on it.

C ---       Begin IF

         if (outopen) then

            if (j .eq. 1) then
               write(ounit,30)' '
               context1 = '# FILE: '
               context1(9:50) = infile
               write(ounit,33)context1(1:50)
            endif

            if (j .eq. 1) then
               context1 = '# KEYNAME: '
               context1(12:19) = keynam
               write(ounit,32) context1
            endif

            write(ounit,30)' '
            write(ounit,30) context

         else

            if (j .eq. 1) then
               call fcecho(' ')
               context1 = '# FILE: '
               context1(9:50) = infile 
               call fcecho(context1(1:50))
            endif

            if (j .eq. 1) then
               context1 = '# KEYNAME: '
               context1(12:19) = keynam
               call fcecho(context1)
            endif

            call fcecho(' ')
            call fcecho(context)
         endif

C --- Now attempt reading the records and write them (if found)
C --- to the output device.

         print = .false.

         do 10 i = 1,99999
            call ftgrec(iunit,i,card,ftstat)
            if (ftstat .ne. 0) goto 9999

            if (print) then
C               we printed the previous card, so check for continuation
                if (card(1:10) .ne. 'CONTINUE  ')print = .false.
            end if

C --- Now find the match according to whether it is exact or not.
C --- If exact match is required then the characters following
C --- the keyword until and including the 8th character should be
C --- blanks.
            if (.not. print) then
              if (exact) then

                if (card(1:8) .eq. keynam) print = .true.

C --- if match need not be exact then the requirtment is only that
C --- any keyword which contains the keyname along with other 
C --- characters can be printed.

              else
                if (card(1:m) .eq. keynam) print = .true.
              endif 
            end if

C --- If a match is found then print the whole card.

            if (print) then
               if (outopen) then 
                  write(ounit,30)card      
               else
                  call fcecho(card)
               endif
            endif
 10      continue

 9999    continue
         ftstat = 0
         call ftcmsg

C --- If more than one extension is requested then repeat the
C --- above procedure  by incrementing the extension number by 1
C --- until all the extensions are exhausted.

         if (repeat) then
            extnum = extnum + 1
            j = j + 1
            goto 3000
         endif
 99      if (inopen) call ftclos(iunit,ftstat)
         inopen = .false.

C  loop back for next requested input file
 100  continue

 30   format(a)
 31   format('# EXTENSION: ',i4)
 32   format(a17)
 33   format(a50)
 34   format(a50,i6,i6)

 999  continue
      ftstat = 0
      if (inopen) call ftclos(iunit,ftstat)
      if (outopen) close(ounit)

      return
      end

