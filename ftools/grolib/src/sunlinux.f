*      **************************************************************
*      SUNLINUX transforms data written on either a SUN system or on
*      a PC Linux system the counterpart system format.
*
*      Calling Sequence
*      ----------------
*
*           call sunlinux( data, nvars, lmap )
*
*           Where
*
*           List Var.   Type     In/Out    Description
*          ----------   ----     ------    ------------------------------
*            data       Char()   I/O       Character array of input data
*                                          from an unformatted read.  It
*                                          is assumed that actual variables
*                                          of interest are equivalenced to
*                                          this array in the calling 
*                                          program.
*            nvars       I*4     I         The number of variables
*                                          equivalenced to the data array
*                                          in the calling program.
*            lmap        I*4()   I         An array of byte lengths for
*                                          each of the variables that
*                                          are equivalenced to the data
*                                          array.
*
**********
*
*     Example:
*
*     In calling program
*
*        character    input(24)
*        real*8       var1
*        real         var2
*        integer      ivar1
*        integer*2    ivar2(2)
*        character    ivar3
*        byte         ivar4(3)
*
*        equivalence  (input(1),var1), (input(9),var2),
*    +                (input(13),ivar1), (input(17),ivar2(1)),
*    +                (input(21),ivar3), (input(22),ivar4)
*
*        data   nbr/9/, lv/ 8 ,4, 4, 2, 2, 1, 1, 1, 1 /
*
*         (Note there are 9 variables counting each member of a dimensiones
*          set, and the sum of the lv lengths (in bytes) equals the 
*          dimension of input.
*
*        call sunlinux( data, nbr, lv )    ! data written on one system is
*                                          ! now converted to the other.
*
**************************************************************************
*
*     Calls the internal routine, REFL
*
*     Written 7/12/01.  D.L.Bertsch
*
**************************************************************************


      subroutine sunlinux( data, nvars, lmap )


      character      data(1)
      integer        nvars, lmap(1)


      loc = 1

      do n = 1, nvars
         len = lmap(n)
         if( len.gt.1 ) call refl( data(loc), len, len )
         loc = loc + len
      end do

      return
      end


*     ***********************************************************
*     Subroutine REFL inverts the byte order of a variable or
*     a variable string.  It is used to convert data written on
*     a Sun sytem for use on a Linux system and vice versa.
*     (Same as routine, REFLECT.)
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
*      call refl( data, byte_length, byte_total )
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
*                call refl( prob, 8, 160) where 160 is 20*8.
*
*     ***************************************************************
*
*     Written 7/11/01.  D.L.Bertsch
*
*     ***************************************************************
*     ***************************************************************




      subroutine refl( data, byte_length, byte_total )


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
         end do
      end do

      return
      end
