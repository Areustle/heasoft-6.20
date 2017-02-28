C******************************************************************************
C SUBROUTINE:
C      fcsrts
C
C DESCRIPTION:
C      Sort a string array into ascending order
C
C AUTHOR/DATE:
C      James Kent Blackburn 1/27/92
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C      fortran code modified from numerical recipes
C
C USAGE:
C      call fcsrts(nrec,array)
C
C ARGUMENTS:
C      nrec  - number of records
C      array - array of strings
C
C PRIMARY LOCAL VARIABLES:
C      q - temp storage for string value
C
C CALLED ROUTINES:
C
C******************************************************************************
        subroutine fcsrts(nrec,array)

        integer nrec
        character*(*) array(nrec)
        integer i,j,l,ir
        character(80) q

        if(nrec.eq.1) return
        l = nrec / 2 + 1
        ir = nrec
  10    continue
          if ( l .gt. 1 ) then
            l = l - 1
            q = array(l)
          else
            q = array(ir)
            array(ir) = array(1)
            ir = ir - 1
            if ( ir .eq. 1 ) then
              array(1) = q
              return
            endif
          endif
          i = l
          j = l + l
  20      if ( j .le. ir ) then
            if ( j .lt. ir ) then
              if ( array(j) .lt. array(j+1) ) j=j+1
            endif
            if ( q .lt. array(j)) then
              array(i) = array(j)
              i = j
              j = j + j
            else
              j = ir + 1
            endif
            goto 20
          endif
          array(i) = q
        goto 10
        end
