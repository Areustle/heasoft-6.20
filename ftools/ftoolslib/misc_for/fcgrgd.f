C****************************************************************************
C SUBROUTINE:
C      fcgrgd
C
C DESCRIPTION:
C      Gets the number of real*8 ranges and their limits
C      Based on fcgrgs by J. Tarrant
C
C AUTHOR/DATE:
C      James Lochner 3/9/94
C
C MODIFICATION HISTORY:
C      EAG 9/7/94 set to abs_min:abs_max if rows is ' ' or -
C
C NOTES:
C
C USAGE:
C      call fcgrgd(rows,abs_min,abs_max,numranges,rowrange1,rowrange2)
C
C ARGUMENTS:
C      rows      - string list of real*8 ranges to translate
c      abs_min   - absolute minimum value allowed
c      abs_max   - absolute maximum value allowed
C      numranges - number of real*8 ranges in list
C      rowrange1 - array of beginning values for each range
C      rowrange2 - array of end values for each range
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
      subroutine fcgrgd(rows,abs_min,abs_max,numranges,range1,range2)

      character*(*) rows
      integer numranges
      double precision range1(15), range2(15), abs_min, abs_max
      character(30) rowlist(15)
      character(80) context
      logical range
      integer i, j, fcstln, rowlen, rlist_index, rangelen,
     &     range_index

      range_index = 0

C if rows is ' ' or '-' return abs_min:abs_max
      if ((rows .eq. ' ') .or. (rows .eq. '-')) then
         numranges = 1
         range1(1) = abs_min
         range2(1) = abs_max
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
               range1(i) = abs_min
               read(rowlist(i)(2:rangelen),1000,err=999)
     &              range2(i)
            else if (range_index .eq. rangelen) then
               read(rowlist(i)(1:rangelen-1),1000,err=999)
     &              range1(i)
               range2(i) = abs_max
            else
               read(rowlist(i)(1:range_index-1),1000,err=999)
     &              range1(i)
               read(rowlist(i)(range_index+1:rangelen),1000,
     &              err=999) range2(i)
C               write(6,*) range1(i),range2(i)
            endif
         else
            read(rowlist(i),1000,err=999) range1(i)
            range2(i) = range1(i)
         endif
C  check for range values too small or too large
         if (range1(i) .lt. abs_min) range1(i) = abs_min
         if (range2(i) .gt. abs_max) range2(i) = abs_max
 20   continue

 60   continue
      return

C error in parsing row range list
 999  context = ' Error in parsing ranges ' // rowlist(i)
      call fcerr (context)
      range1(i) = 1
      range2(i) = 1
      numranges = i

 1000 format(BN,f14.8)
      return
      end
