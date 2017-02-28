c
c  PROGRAM:  REMOVENULL
c
c  DESCRIPTION:  Removes nulls from strings read by a C routine
c
c  Modification History
c  Sandhia Bansal    03/21/02  
c
c
      subroutine removeNull(fname)

      integer        n
      character*(*)  fname



      n = index(fname, '\0')
      fname(n:n) = ' '
      n = index(fname, ' ') - 1

      return
      end
