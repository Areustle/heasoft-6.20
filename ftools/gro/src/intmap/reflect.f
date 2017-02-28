*     ***********************************************************
*     Subroutine REFLECT inverts the byte order of a variable or
*     a variable string.  It is used to convert data written on
*     a Sun sytem for use on a Linux system and vice versa.
*
*     Rules of Conversion:
*     --------------------
*       #  characters, character*n, and byte quantities are unchanged
*          between the two systems.
*
*       #  two, four, and eight byte variables need to be inverted
*          end-to-end.  This routine does that.
*
*      Calling Sequence:
*      -----------------
*
*      call reflect( data, byte_length, byte_total )
*
*      where
*
*           data         I/O    I,R,L   Any type of data to be inverted.
*           byte_length  I      I*4     byte length of each quatity.  
*                                       Inversion occurs over these
*                                       byte lengths.
*           byte_total   I      I*4     Total byte count of data.
*
*      NOTE:  Both byte counts must be even numbers.
*
*
*      example:  Suppose prob is a 20-element real*8 variable to be
*                inverted.  Then 
*                call reflect( prob, 8, 160) where 160 is 20*8.
*
*     ***************************************************************
*
*     Written 7/11/01.  D.L.Bertsch
*
*     ***************************************************************
*     ***************************************************************




      subroutine reflect( data, byte_length, byte_total )


      character(1)          data(1), save
      integer       byte_length, byte_total

      nvals = byte_total/byte_length
      iup = byte_length/2

      do n = 1, nvals
         do i = 1, iup
            ind = byte_length*(n-1) + i
            jword = 1 + (ind-1)/byte_length
            k = 2*jword*byte_length - byte_length + 1 - ind
            save = data(ind)
            data(ind) = data(k)
            data(k) = save
            itemp = ichar(data(ind))
            ktemp = ichar(data(k))
            j=k
         end do
      end do

      return
      end
