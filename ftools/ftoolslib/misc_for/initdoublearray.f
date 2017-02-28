C
      subroutine initdoublearray(array, nelem)
      integer nelem, i
      double precision array(nelem)

      do 10 i = 1, nelem
         array(i) = 0.
 10   continue
      return
      end
