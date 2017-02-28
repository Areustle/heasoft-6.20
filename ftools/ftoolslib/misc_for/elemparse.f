

C******************************************************************************
C subroutine:
C      elemparse
C
C FILE:
C      elemparse.f
C
C DESCRIPTION:
C       This routine parses input element specified as dim1, dim2, etc.
C       and returns the felem (single vector element number).
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       November, 1993
C
C MODIFICATION HISTORY:
C       2/15/94 EAG 1.1 Increase maximum number of dimensions
C                       Allow for ; as well as ,
C       12/9/94 EAG 1.2 Return 1 if nothing specified
C       12/29/97 MJT 1.3 Fixed illegal list-directed internal read
C
C NOTES:
C
C CALLING SEQUENCE:
C       call elemparse (string, iunit, colno, felem, status)
C
C ARGUMENTS:
C       string - the input element specification
C       iunit  - the input unit number
C       colno  - the input column number
C       felem  - the output element number
C       status - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C*****************************************************************************
      subroutine elemparse (string, iunit, colno, felem, status)

      integer maxdims
      parameter (maxdims = 10)

      character*(*) string
      integer iunit, felem, status, colno

      integer firstchar, lastchar, fcstln
      integer commapos, ndims, nelem(maxdims)
      integer naxis, naxes(maxdims), i
      character(80) context
      character(3) version

      version = '1.3'

C return 1 if nothing specified
      if ((string .eq. ' ') .or. (string .eq. '()') .or.
     &     (string .eq. '[]')) then
         felem = 1
         return
      endif

C parse the input string
      firstchar = 1
      lastchar = fcstln(string)
      if ((string(1:1) .eq. '(') .or. (string(1:1) .eq. '['))
     &     firstchar = 2
      if ((string(lastchar:lastchar) .eq. ')') .or.
     &     (string(lastchar:lastchar) .eq. ']')) lastchar = lastchar - 1
      ndims = 1

C Note that either , or ; may seperate the elements, but only one
C element can be specified
 10   commapos = index(string(firstchar:lastchar),',') + firstchar - 1
      if (commapos .le. firstchar) commapos =
     &     index(string(firstchar:lastchar),';') + firstchar - 1
      if (commapos .le. firstchar) commapos = lastchar + 1
C Can't use '(I)' as before -- hoping '(BN,I20)' will do...
      read (string(firstchar:commapos-1), '(BN,I20)',
     &     err=900) nelem(ndims)
      if (commapos .ge. lastchar) goto 100
      ndims = ndims + 1
      if (ndims .gt. maxdims) then
         context='ELEMPARS'//version//' Maximum dimensions exceeded'
         call fcerr (context)
         status = 11
         goto 999
      endif

      firstchar = commapos + 1
      goto 10

 100  call ftgtdm (iunit, colno, maxdims, naxis, naxes, status)
      if ((naxis .ne. ndims) .or. (status .ne. 0)) then
         context = 'ELEMPARS' // version // ' number of dimensions'
     &        // ' do not match' // string
         call fcerr (context)
         goto 999
      endif

C calculate the element number
      felem = nelem(1)
      do 200 i = 2, naxis
         felem = felem + naxes(i-1)*(nelem(i)-1)
 200  continue
      goto 999

 900  context = 'ELEMPARS' // version // ' Error parsing ' // string
      call fcerr (context)
      status = 10

 999  return
      end
