C******************************************************************************
C SUBROUTINE:
C      fcidxl
C
C DESCRIPTION:
C      Index a logical array such that array(index(i)) 
C      is in ascending order
C
C AUTHOR/DATE:
C      James Kent Blackburn 1/15/91
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C      fortran code modified from numerical recipes
C
C USAGE:
C      call fcidxl(nrec,array,index)
C
C ARGUMENTS:
C      nrec - number of records
C      array - array of logicals
C      index - array for indexing
C
C PRIMARY LOCAL VARIABLES:
C      indext - temp storage for index
C      q      - temp storage for index value
C
C CALLED ROUTINES:
C
C******************************************************************************
        subroutine fcidxl(nrec,array,index)

        integer nrec,index(nrec)
        logical array(nrec)
        integer i,j,l,ir,indext
        logical q

C  initialize index to sequential
        do 10 j = 1, nrec
          index(j) = j
  10    continue
        if(nrec.eq.1) return
        l = nrec / 2 + 1
        ir = nrec
  20    continue
          if ( l .gt. 1 ) then
            l = l - 1
            indext = index(l)
            q = array(indext)
          else
            indext = index(ir)
            q = array(indext)
            index(ir) = index(1)
            ir = ir -1
            if ( ir .eq. 1 ) then
              index(1) = indext
              return
            endif
          endif
          i = l
          j = l + l
  30      if ( j .le. ir ) then
            if ( j .lt. ir ) then
              if (fzext(array(index(j))) .lt. 
     $              fzext(array(index(j+1)))) j = j + 1
            endif
          if (fzext(q) .lt. fzext(array(index(j)))) then
            index(i) = index(j)
            i = j
            j = j + j
          else
            j = ir + 1
          endif
        goto 30
        endif
        index(i) = indext
        goto 20
        end