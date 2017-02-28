
C******************************************************************************
C SUBROUTINE:
C      fcgcls
C
C DESCRIPTION:
C      gets the list and number of columns names or filenames
C
C AUTHOR/DATE:
C      Janice Tarrant  12/23/91
C
C MODIFICATION HISTORY:
C      Tarrant  1/8/92  changed column parser to accept column names
C        with embedded spaces
C       Greene 8/25/92  changed to allow for blank lines in input file
C       Greene 8/4/93   do not break on , enclosed in []
C       4/12/94 EAG     Stop after 999 values
C      10/20/94 EAG 3.1a - allow for extra spaces between items
C      06/16/00 PDW 3.2  - Detect data on same line as EOF
C      07/06/00 PDW 3.3  - Don't print warning at line #999, wait
C                          until after a 1000th line is found
C      08/21/03 WDP 3.4  - ignore spaces as a filename separator if
C                          they occur within square brackets
C
C NOTES:
C
C USAGE:
C      call fcgcls(columns,colist,numcols,negflag)
C
C ARGUMENTS:
C      columns - string list of column names to translate
C      colist  - array of separate column names
C      numcols - number of column names in list
C      negflag - exclude name flag
C
C PRIMARY LOCAL VARIABLES:
C      col_fname   - name of file containing column names
C      context     - error message
C      colen       - length of column string
C      cname_index - position of name in column string
C      funit       - unit number associated with input file
C      ios         - I/O status
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      function   fcstln - returns length of character string (integer)
C
C******************************************************************************
      subroutine fcgcls(columns,colist,numcols,negflag)

      character*(*) columns
      character*(*) colist(4096)
      integer numcols
      logical negflag
      logical done
      character(255) tmpname
      character(160) col_fname
      character(80) context
      integer i, j, fcstln, colen, cname_index, funit, ios, inbracket

C  find position of first nonwhite character
      colen = fcstln(columns)
      do 10 i = 1, colen
         if (columns(i:i) .ne. ' ') goto 11
 10   continue
C  (JCI) escape if the input string is blank, otherwise it core dumps.
C  If you get here, the string is empty
      numcols = 0
      goto 999
 11   cname_index = i

C  if first character is @ then the column names are in a file
      funit = 17
      if (columns(cname_index:cname_index) .eq. '@') then
         col_fname = columns(cname_index+1:)
         open(unit=funit,file=col_fname,iostat=ios,status='old')
         if (ios .ne. 0) then
            context = 'FCGCLS 3.3: file containing column ' //
     &           'list not found'
            call fcerr(context)
            close(funit)
            goto 999
         endif

         done = .false.
         numcols = 0
C    Clear element so that we can detect data+EOF events (PDW)
 20      tmpname = ' '
         read(funit,1000,end=25) tmpname
 1000    format(A)
         goto 26

C    Skip blank lines (EAG 8/25/92) and deal with data+EOF lines (PDW 6/00)
 25      done = .true.
 26      if (fcstln(tmpname) .gt. 0) then
            if (numcols .ge. 4096) then
               call fcecho
     &              (' WARNING:FCGCLS 3.3: truncating at 4096 items')
               goto 30
            endif
            numcols = numcols + 1
            colist(numcols) = tmpname
         endif
         if (.not.done) goto 20

 30      close(funit)

C  copy column names into column list
      else
         inbracket = 0
         i = cname_index
         j = 1
 40      if (i .eq. colen) goto 42
         if (columns(i:i) .eq. '"') then
            i = i + 1
            cname_index = i
 43         if (columns(i:i) .eq. '"') goto 44
            i = i + 1
            goto 43
 44         colist(j) = columns(cname_index:i-1)
            i = i + 2
            cname_index = i
            if ( i .ge. colen ) goto 45
            j = j + 1
            goto 40
         endif

C check if we are within square brackets (WDP 8/20/03)
         if (columns(i:i) .eq. '[') then
            inbracket = 1
         elseif (columns(i:i) .eq. ']')then
            inbracket = 0
         endif

C check for column name separators
C space is a separator, unless within square brackets
         if (columns(i:i) .eq. ' ' .or. columns(i:i) .eq. ',') then
            if (inbracket .le. 0) goto 41
         endif
         i = i + 1
         goto 40

 41      colist(j) = columns(cname_index:i-1)
 100     i = i + 1

C deal with multiple spaces between items
         if (columns(i:i) .eq. ' ') goto 100

         cname_index = i
         j = j + 1
         goto 40
 42      colist(j) = columns(cname_index:i)
 45      continue
         numcols = j
      endif

C  check for exclude column flag
      negflag = .false.
      if (colist(1)(1:1) .eq. '-') then
         negflag = .true.
         colen = fcstln(colist(1))
         do 50 i = 1, colen
            colist(1)(i:i) = colist(1)(i+1:i+1)
 50      continue
      endif

 999  continue

      return
      end
