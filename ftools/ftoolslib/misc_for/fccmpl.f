
C******************************************************************************
C SUBROUTINE:
C      fccmpl
C
C DESCRIPTION:
C      This subroutine will compare two lists of strings to verify if
C      the first list is a subset of the second list and return the
C      result of the test in the logical variable SUBSET.  If NEGFLG
C      is true, then modify list1 to return the complementary set of names
C      (requires LIST1 to be dimensioned at least as large as LIST2).
C
C AUTHOR/DATE:
C      Kent Blackburn 12/5/91
C
C MODIFICATION HISTORY:
C      W Pence 9/18/95 - efficiency improvements
C          MJT 4/24/96 - checks for "sensecase" parameter
C                        in the calling FTOOL
C      Jeff Guerber 12/9/1996 - Disabled case-checking, for both the subset
C          test and complimentary list.  Check all of list1 and report which
C          ones failed in the error message.
C
C NOTES:
C      12/9/1996: This routine is now CASE-INSENSITIVE.
C
C      NEGFLG only works if the subset test succeeds.  It REQUIRES list1 to be
C      dimensioned at least as large as list2, but the test itself doesn't.
C
C USAGE:
C      call fccmpl(nolist1,nolist2,list1,list2,negflg,subset)
C
C ARGUMENTS:
C      nolist1 - number of names in string list1
C      nolist2 - number of names in string list2
C      list1   - first list of names
C      list2   - second list of names
C      negflag - exclude name flag
C      subset  - flag indicates if list1 is a subset of list2
C
C PRIMARY LOCAL VARIABLES:
C      newlist - list of names created from logical not of list1
C      remove  - flag for names to be removed from list
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine ftupch - uppercase a string (from Fitsio)
C
C******************************************************************************
      subroutine fccmpl(nolist1,nolist2,list1,list2,negflg,subset)

      integer       nolist1,nolist2
      character*(*) list1(*),list2(*)
      logical       negflg,subset,sensecase
      integer       i,j,k
      character(80)  temp1,temp2

C  First look for a sensecase parameter in calling FTOOL, then
C ---  test that all names in list1 are in list2 ---
C      call uclgsb('sensecase',sensecase,status)
C      if(status .ne. 0)then
C        sensecase = .true.
C        status = 0
C      endif

      sensecase = .false.

      subset=.true.
      do 10 i = 1, nolist1
        temp1 = list1(i)
        if (.not.sensecase) call ftupch(temp1)
        do 20 j = 1, nolist2
          temp2 = list2(j)
          if (.not.sensecase) call ftupch(temp2)
          if ( temp1 .eq. temp2 ) goto 10
20      continue

C       this one was not in list2, so list1 is not a subset
        subset=.false.
        call fcecho('FCCMPL: not found in table: '//temp1)
10    continue

C ---   If negation flag is set then build a new 'complementary'
C ---   list of  names by removing the names in list1 from list2

C ---   Use complementary space in list1 to store list of columns to keep
C       Note:  list1 must be dimensioned at least as big as list2

      if ( negflg .and. subset ) then
         k = nolist2
         do 30 i = nolist2,1,-1
            temp2 = list2(i)
            if (.not.sensecase) call ftupch(temp2)
            do 40 j = 1, nolist1
               if (.not.sensecase) call ftupch(temp1)
               temp1 = list1(j)
               if ( temp2 .eq. temp1 )go to 30
 40         continue

C           column is not on removal list, so add it to the list to keep
            list1(k) = list2(i)
            k = k - 1
 30      continue

C ---     Copy the columns to keep to the beginning of list1

         nolist1 = nolist2-k
         do 50 i = 1, nolist1
            list1(i) = list1(i+k)
 50      continue
      end if
      return
      end
