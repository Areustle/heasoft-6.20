


C******************************************************************************
C SUBROUTINE:
C      fcgrgs
C
C DESCRIPTION:
C      Gets the number of row ranges and their limits
C
C AUTHOR/DATE:
C      Janice Tarrant  12/24/91
C
C MODIFICATION HISTORY:
C       EAG 1/22/93 - Added BN to format statement for VAX
C       EAG 12/2/93 - Trap read errors
C       EAG 9/7/94  - return 1:nrows if ' ' or '-' are passed in
C       Ziqin Pan 8/16/2004 - raise the rowrange limit from 15 to 100.
C
C NOTES:
C
C USAGE:
C      call fcgrgs(rows,nrows,numranges,rowrange1,rowrange2)
C
C ARGUMENTS:
C      rows      - string list of row ranges to translate
C      nrows     - number of rows in FITS file
C      numranges - number of row ranges in list
C      rowrange1 - array of beginning row for each range
C      rowrange2 - array of end row for each
C
C PRIMARY LOCAL VARIABLES:
C      rowlist     - array of separate row ranges
C      range       - range flag, indicates single or range of rows
C      rowlen      - length of row ranges string
C      rlist_index - position of range in row ranges string
C      rangelen    - length of range string
C      range_index - position of range delimiter in range
C
C CALLED ROUTINES:
C      function   fcstln - returns length of character string (integer)
C
C******************************************************************************
      subroutine fcgrgs(rows,nrows,numranges,rowrange1,rowrange2)

      character*(*) rows
      integer nrows, numranges, rowrange1(*), rowrange2(*)
      character(30) rowlist(100)
      character(80) context
      logical range
      integer i, j, fcstln, rowlen, rlist_index, rangelen,
     &     range_index

      range_index = 0

C trap ' ' and '-' and return 1:nrows
      if ((rows .eq. ' ') .or. (rows .eq. '-')) then
         numranges = 1
         rowrange1(1) = 1
         rowrange2(1) = nrows
         return
      endif

C  get the number and list of row ranges
      rowlen = fcstln(rows)
      rlist_index = 1
      i = 1
      j = 1
 10   if (i .eq. rowlen) goto 12
      if (rows(i:i) .eq. ',') goto 11
      i = i + 1
      goto 10
 11   rowlist(j) = rows(rlist_index:i-1)
      i = i + 1
      rlist_index = i
      j = j + 1
      goto 10
 12   rowlist(j) = rows(rlist_index:i)
      numranges = j

C  get the row range limits
      do 20 i = 1, numranges
C  search for - range separator
         rangelen = fcstln(rowlist(i))
         range = .false.
         do 30 j = 1, rangelen
            if (rowlist(i)(j:j) .eq. '-') then
               range_index = j
               range = .true.
            endif
 30      continue
         if (range) then
            if (range_index .eq. 1) then
               rowrange1(i) = 1
               read(rowlist(i)(2:rangelen),1000,err=999)
     &              rowrange2(i)
            else if (range_index .eq. rangelen) then
               read(rowlist(i)(1:rangelen-1),1000,err=999)
     &              rowrange1(i)
               rowrange2(i) = nrows
            else
               read(rowlist(i)(1:range_index-1),1000,err=999)
     &              rowrange1(i)
               read(rowlist(i)(range_index+1:rangelen),1000,
     &              err=999) rowrange2(i)
            endif
         else
            read(rowlist(i),1000,err=999) rowrange1(i)
            rowrange2(i) = rowrange1(i)
         endif
C  check for row number too large
         if (rowrange1(i) .gt. nrows) rowrange1(i) = nrows
         if (rowrange2(i) .gt. nrows) rowrange2(i) = nrows
 20   continue

 60   continue
      return

C error in parsing row range list
 999  context = ' Error in parsing ranges ' // rowlist(i)
      call fcerr (context)
      rowrange1(i) = 1
      rowrange2(i) = 1
      numranges = i

 1000 format(BN,I9)
      return
      end
