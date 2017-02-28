C
      subroutine initintarray(array, nelem)
      integer nelem, i
      integer array(nelem)

      do 10 i = 1, nelem
         array(i) = 0
 10   continue
      return
      end
