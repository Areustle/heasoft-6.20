*+RMVEXSP
      subroutine rmvexsp(instr,newstr)
c     --------------------------------
c --- DESCRIPTION ----------------------------------------------------
c
c This routine compresses more than one blank to a single blank in
c a string
c
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) instr,newstr
      integer i,j,length
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments ...
c
c instr      char   : Character string
c newstr     char   : New character string without extra blanks
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c Rehana Yusaf (1993 March 30)
c
      character(5) version
      parameter (version = '1.0.0')
*-
c --------------------------------------------------------------------
c
c --- COMPRESSING MORE THAN ONE BLANK ---
c
      length=LEN(instr)
      do i=1,length
        newstr(i:i)=' '
      enddo
      j = 1
      do i=1,length
        IF (instr(i:i+1).NE.'  ') THEN
          newstr(j:j) = instr(i:i)
          j = j+1
        ENDIF
      enddo
      return
      end
c --------------------------------------------------------------------
c     END OF RMVEXSP
c -------------------------------------------------------------------- 
