C***************************************************************************** 
C SELECTOR TASK:
C      fmodcomm
C
C AUTHOR:  
C      Mike Tripicco, June 1995 
C
C FILE:
C      fmodcomm.f 
C
C DESCRIPTION: 
C      Replaces text in a COMMENT|HISTORY|<blank> keyword record
C
C MODIFICATION HISTORY:
C
C NOTES:
C      Routine demands unique match to keytype && template:
C        The template serves to identify the record; if multiple 
C        matches occur, user is prompted for another template
C        (unless "firstmatch" parameter is set to "yes").
C
C      New text is limited to 350 characters (5 header records). New
C        records of the same keytype will be created as necessary.
C      
C      (New text read from a file via @filename is limited to 99 lines)
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile     -- input FITS file and extension number
C      template   -- Subset of text line to be modified
C      newtext    -- Replacement text for matched line (can use @filename)
C      keytype    -- keyword type to be modified (COMMENT|HISTORY|<blank>)
C      firstmatch -- force operation on first matching record?
C
C CALLED ROUTINES:
C      subroutine ftcmsg -- clear entire FITSIO error message stack
C      subroutine gmodcm -- gets parameters from environment
C      subroutine modcom -- locates record and inserts text
C
C****************************************************************************** 
        subroutine fmodcm
 
        character*  8   keytype
        character(160)   infile 
        character* 80   template
        character(350)   newtext
       
        integer         ftstat
        logical         firstmatch

        character(40) taskname
        common /task/ taskname

        taskname = 'fmodcomm'

C  initialize variables and clear error message stack

        ftstat = 0
        call ftcmsg

C  get the parameters from the .par file

        call gmodcm(infile,template,newtext,keytype,firstmatch,ftstat)
        if (ftstat .ne. 0) goto 999

C  find the specified comment/history record from the FITS file
C   and replace the comment field with new text

        call modcom(infile,template,newtext,keytype,firstmatch,ftstat)
        if (ftstat .ne. 0) goto 999

999     continue
        if (ftstat .ne. 0) call fcerrm(ftstat)
        return
        end

C******************************************************************************
C SUBROUTINE:
C     gmodcm 
C
C AUTHOR:  
C      Mike Tripicco June '95 
C
C DESCRIPTION: 
C      Get parameters from .par file
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call gmodcm(infile,template,newtext,keytype,firstmatch,ftstat)
C
C ARGUMENTS:
C      infile     - input FITS file and extension number
C      keytype    - keyword the user is looking for (COMMENT|HISTORY|<blank>)
C      template   - string to match in all records of the specified keytype
C      newtext    - replacement string for template (or @filename)
C      firstmatch - force operation on first matching record?
C      ftstat     - Status flag. Non-zero means error occured
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C
C****************************************************************************** 
	subroutine gmodcm(infile,template,newtext,keytype,firstmatch,
     &                    ftstat)
 
        character(8)   keytype
	character(80)  context,template
	character(350) newtext
	character*(*) infile

	integer ftstat
        logical firstmatch

C  initialize variables

	ftstat = 0

C  get the name of the input FITS file

	call uclgst('infile',infile,ftstat)
	if (ftstat .ne. 0) then
	    context = 'error getting FILENAME parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the string to be searched for

	call uclgst('template',template,ftstat)
	if (ftstat .ne. 0) then
	    context = 'error getting TEMPLATE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the string to be put in place of the comment field specified
C   by keytype and template

	call uclgst('newtext',newtext,ftstat)
	if (ftstat .ne. 0) then
	    context = 'error getting NEWTEXT parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the keyword type to be acted upon (defaults to COMMENT)

	call uclgst('keytype',keytype,ftstat)
	if (ftstat .ne. 0) then
	    context = 'error getting KEYTYPE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the firstmatch parameter (defaults to no)

	call uclgsb('firstmatch',firstmatch,ftstat)
	if (ftstat .ne. 0) then
	    context = 'error getting FIRSTMATCH parameter'
	    call fcerr(context)
	    goto 999
	endif

999     continue

	return
	end

C******************************************************************************
C SUBROUTINE:
C      modcom
C
C DESCRIPTION: 
C      Match COMMENT/HISTORY/<blank> keyword and template text
C        in the FITS file headers and replace entire line
C        with a new text string
C
C AUTHOR:  
C      Mike Tripicco, June 1995 
C
C MODIFICATION HISTORY:
C
C NOTES:
C      demands a unique match to keyword & template
C
C USAGE:
C      call modcom(infile,template,newtext,keytype,firstmatch,ftstat)
C
C ARGUMENTS:
C      infile     - input FITS file and extension number
C      template   - template for text which the user wants to replace/edit
C      newtext    - replacement text (max 350 char) or @filename (<=99 lines)
C      keytype    - keyword the user is looking for (COMMENT/HISTORY/<blank>)
C      firstmatch - force operation on first matching record?
C      ftstat     - Status flag. Non-zero means error occurred
C
C PRIMARY LOCAL VARIABLES
C      filnam   - name of FITS file
C      extnum   - FITS file extension number
C      context  - error message 
C      newcard  - replacement text for one record
C
C CALLED ROUTINES:
C      subroutine fcerrm - report an error number to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ftopen - open a FITS file
C      subroutine ftclos - close a FITS file
C      subroutine ftmahd - move to a specified HDU in the FITS file
C      subroutine ftghsp - return number of exiting keywords in the CHU
C      subroutine ftgkyn - get name, value & comment of nth keyword in CHU
C      subroutine ftmrec - modify the nth 80-char header record in the CHU
C      subroutine ftirec - insert new keyword record into the CHU
C      subroutine ftprec - append new keyword record into the CHU
C      function fcstln   - index of the last non-blank character of string
C      function stgcmp   - is a string a template of a larger string? 
C
C****************************************************************************** 
      subroutine modcom(infile,template,newtext,keytype,
     +                  firstmatch,ftstat)

      character*(*) infile
      character(350) newtext
      character(80)  ntlist(99)
      character(160) filenam
      character(80)  context,val,comm,newcard,template
      character(8)   kwd, keytype
      logical       firstmatch

      integer keynbr, blksiz, extnum
      integer ftstat, hdtype, iunit
      integer numkeys, dummy, numcrds
      integer ntnum

      integer fcstln
      logical stgcmp

      logical match,hit,negflag

C  initialization

      match  = .false.
      hit    = .false.
      iunit  = 10

C  keytype must be COMMENT, HISTORY, or <blank>
C  (only the first one or two characters are used to decide)
C   NB: xpi doesn't support "enumerated string" parameters...

      if (keytype(1:1) .eq. 'C' .or. keytype(1:1) .eq. 'c') then
        keytype='COMMENT '
        goto 111
      endif

      if (keytype(1:1) .eq. 'H' .or. keytype(1:1) .eq. 'h') then
        keytype='HISTORY '
        goto 111
      endif

      if (keytype(1:1) .eq. ' ' .or. keytype(1:2) .eq. '<b' .or.
     $  keytype(1:1) .eq. 'B' .or. keytype(1:1) .eq. 'b' .or.
     $  keytype(1:2) .eq. '<B') then
        keytype='        '
        goto 111
      endif

      write (context, 27) keytype
   27 format ('Illegal keyword specified: ',a8)
      call fcerr(context)
      goto 999

  111 continue

C  get the input FITS filename and extension number

      call fcpars(infile,filenam,extnum,ftstat)
      if (ftstat .ne. 0) then
	 context = 'Unable to parse ' // infile(:fcstln(infile))
	 call fcerr(context)
	 goto 999
      endif

C  default to primary array unless user requests other extension

      if (extnum .lt. 0) extnum = 0

C  open the input FITS file for read/write
 
      call ftopen(iunit,filenam,1,blksiz,ftstat)
      if (ftstat .ne. 0) then
	 context = 'Unable to open ' // filenam(1:fcstln(filenam))
	 call fcerr(context)
	 goto 999
      endif

C  no need to advance to extensions if primary header selected

      if (extnum .eq. 0) goto 4000

C  otherwise move to the requested extension number

      call ftmahd(iunit,extnum+1,hdtype,ftstat)
      if (ftstat.ne. 0) then
        write(context,33) extnum
   33   format('error moving to extension number ',i6)
        call fcerr(context)
        goto 999
      endif

 4000 continue

C  get the number of existing keywords in the CHU

      call ftghsp(iunit,numkeys,dummy,ftstat)

C  find header record with specified keytype which
C    matches the text template uniquely (unless firstmatch is .true.)

      do 222 i=1,numkeys

        call ftgkyn(iunit,i,kwd,val,comm,ftstat)
        if (ftstat .ne. 0) then
          write(context,35) i
   35     format('error reading keyword # ',i2)
          call fcerr(context)
          goto 999
        endif

        if (kwd .ne. keytype) goto 222
        match=stgcmp(template(1:fcstln(template)),comm(1:fcstln(comm)))
        if (.not. match) goto 222

C  following is invoked if multiple matches were found:
C    in default case complain about multiple matches
C    but if firstmatch = .true., then exit loop on 1st match

	if (hit .and. (.not. firstmatch)) then
          context = ' ambiguous template specified - multiple matches'
          call fcerr(context)
          goto 999
        endif
        hit = .true.
        keynbr = i
        if (firstmatch) goto 223

  222 continue

C  if no match was found then complain & exit

      if (.not. hit) then
        if (keytype .eq. '        ') then
          context =' no match for \"' // template(1:fcstln(template)) //
     &             '\" in a <blank>  keyword record'
        else
          context =' no match for \"' // template(1:fcstln(template)) //
     &             '\" in a ' // keytype // ' keyword record'
        endif
        call fcerr(context)
        goto 999
      endif

  223 continue

C  reread the matched record (by keyword number)

      call ftgkyn(iunit,keynbr,kwd,val,comm,ftstat)
      if (ftstat .ne. 0) then
        write(context,41) keynbr
   41   format('error reading record # ',i2)
        call fcerr(context)
        goto 999
      endif

C  Construct as many new (char*80) cards as are necessary using the 
C    existing keyword and newtext (value is nul in COMMENT/HISTORY/blank 
C    keyword records). Up to the first 70 chars, just modify existing
C    comment, thereafter use insert (or append if at END).

      if (newtext(1:1) .ne. '@') then

        numcrds = (fcstln(newtext)/71) + 1
        do 333 i=1,numcrds
          do 10 j=1,80
            newcard(j:j)=' '
  10      continue

          newcard(1:fcstln(kwd))=kwd

          if (i .lt. numcrds) newcard(11:80)=
     +      newtext(1+(i-1)*70:70+(i-1)*70)

          if (i .eq. numcrds)
     +      newcard(11:10+fcstln(newtext)-(i-1)*70)=
     +      newtext(1+(i-1)*70:70+(i-1)*70)

C  overwrite the matched hdr record using the newly constructed card

          if (i .eq. 1) then

            call ftmrec(iunit,keynbr,newcard,ftstat)
            if (ftstat .ne. 0) then
              write(context,43) keynbr
  43          format('error modifying record # ',i2)
              call fcerr(context)
              goto 999
            endif

          else

C  insert additional (keytype) header records if necessary
C     ***but***
C  if the matched record was the last (non-END) header record
C  we have to use ftprec (append) rather than ftirec (insert)

            if (keynbr .eq. numkeys) then
              call ftprec(iunit,newcard,ftstat)
            else
              call ftirec(iunit,keynbr+i-1,newcard,ftstat)
            endif
            if (ftstat .ne. 0) then
              write(context,45) keynbr+i-1
  45          format('error inserting record # ',i2)
              call fcerr(context)
              goto 999
            endif

          endif 
 333    continue
       
      else

        call fcgcls(newtext,ntlist,ntnum,negflag)
        do 444 i=1,ntnum
          do 399 j=1,80
            newcard(j:j)=' '
 399      continue
          newcard(1:fcstln(kwd))=kwd
          newcard(11:10+fcstln(ntlist(i)))=ntlist(i)
c         write(6,*) 'ntlist(',i,') is: ',ntlist(i)
          if (i .eq. 1) then
            call ftmrec(iunit,keynbr,newcard,ftstat)
          elseif (keynbr .eq. numkeys) then
            call ftprec(iunit,newcard,ftstat)
          else
            call ftirec(iunit,keynbr+i-1,newcard,ftstat)
          endif
  444   continue

      endif
 
 999  continue
      call ftclos(iunit,ftstat)

      return
      end

C****************************************************************************** 
C FUNCTION:
C      stgcmp
C
C DESCRIPTION:
C      Compares a test string with a reference string
C      
C AUTHOR/DATE:
C      Mike Tripicco, June 1995
C
C MODIFICATION HISTORY:
C       
C NOTES:
C      returns a logical variable: .true. if a match exists, else .false.
C      if test_string is longer than reference_string then returns .false.
C
C USAGE:
C      x = stgcmp(test_string,reference_string)
C
C ARGUMENTS:
C      test_string      - template to be searched for in reference_string
C      reference_string - string to be searched for test_string
C
C PRIMARY LOCAL VARIABLES:
C      testlen,reflen - length of strings
C
C CALLED ROUTINES:
C      fcstln - Finds length of character string throwing out end spaces
C******************************************************************************
      logical function stgcmp(teststr,refstr)

      character*(*) teststr,refstr
      integer testlen,reflen,i,fcstln

      stgcmp = .false.

      testlen=fcstln(teststr)
      reflen=fcstln(refstr)
      if (testlen .gt. reflen) goto 99

      do 10 i=1,reflen-testlen+1
        if (refstr(i:i+testlen-1) .eq. teststr) then
          stgcmp=.true.
          goto 99
        endif
   10 continue
 
   99 continue

      return
      end
