
C******************************************************************************
C SUBROUTINE:
C      fcclwd
C
C DESCRIPTION:
C      Get column width for column's name, unit and data
C
C AUTHOR/DATE:
C      Janice Tarrant  12/12/91
C
C MODIFICATION HISTORY:
C      1/15/92 KB/JT Added align parameter
C      1/24/92 Tarrant  Added unit length
C     12/22/92 EAG   Alllow for variable length vectors
C       2/12/93 EAG  allow for TDISPn width definition
C       3/17/93 EAG  correct for ascii output taking too many characters
C      12/8/94 JKB initialized status
C
C NOTES:
C
C USAGE:
C      call fcclwd(name,unit,form,htype,showcol,showunit,align,lname,
C                  lunit,lform)
C
C ARGUMENTS:
C      name     - column name string
C      unit     - column unit string
C      form     - column format string
C      htype    - header type
C      showcol  - flag for column names
C      showunit - flag for column units
C      align    - flag for column width
C      lname    - length of column names
C      lunit    - length of column units
C      lform    - length of column formats
C
C PRIMARY LOCAL VARIABLES:
C      lstring - length of column string
C      lform   - length of column format string
C      dtype   - data type of keyword
C      repeat  - length of element vector
C      width   - width of character string field
C      status  - fitsio returned error status code
C
C CALLED ROUTINES:
C      function   fcafml - ASCII format length (integer)
C      function   fcbfml - binary format length (integer)
C      function   fcstln - returns length of character string (integer)
C      subroutine ftbnfm - get binary table data format
C
C******************************************************************************
      subroutine fcclwd(name,unit,form,htype,showcol,showunit,
     &     align, lname,lunit,lform)
      character*(*) name, unit, form
      logical showcol, showunit, align
      integer htype, lname, lunit, lform

      integer fcstln, fcafml, fcbfml, dtype, repeat, width, status,
     &     lcol

      lcol = 0

C  JKB:12/8/94 Discovered that status wasn't initiallized!!!
      status = 0

C  get the column name length
      lname = fcstln(name)

C  get the column unit length
      lunit = fcstln(unit)

C  get the column format length for an ASCII file
      if (htype .eq. 1) then
         lform = fcafml(form)
      endif

C  get the column format length for a BINARY file
C  if lform .ne. 0, then the width has already been determined from
C  the relavent TDISPn value.  But we still want things alligned ...
      if ((htype .eq. 2) .and. (lform .eq. 0)) then
         call ftbnfm(form,dtype,repeat,width,status)
         dtype = abs(dtype)
         lform = fcbfml(dtype,repeat,width)
      endif

C  if align true, column width is set to the larger of name, unit or
C  form length
      if (align) then
         if (showcol .and. showunit) then
            if (lname .gt. lunit) then
               if (lname .gt. lform) then
                  lcol = lname
               else
                  lcol = lform
               endif
            else
               if (lunit .gt. lform) then
                  lcol = lunit
               else
                  lcol = lform
               endif
            endif
         endif
         if (showcol .and. (.not. showunit)) then
            if (lname .gt. lform) then
               lcol = lname
            else
               lcol = lform
            endif
         endif
         if ((.not. showcol) .and. showunit) then
            if (lunit .gt. lform) then
               lcol = lunit
            else
               lcol = lform
            endif
         endif
         if ((.not. showcol) .and. (.not. showunit))  lcol = lform
         lname = lcol
         lunit = lcol
      endif

      return
      end
