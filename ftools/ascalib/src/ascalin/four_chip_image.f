cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Program: four_chip_image
c
c     Convert between SIS raw and sat coordinates.
c
c     Eric Gotthelf, Nov 1992.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        subroutine four_chip_image(i, j, chip, x, y)
        integer i, j, chip
        real    x, y
        
        include 'asca_defs.inc'
        include 'asca_common.inc'

        x = 0.0
        y = 0.0

        if (chip .eq. 0) then
           
           x = tr0(1) + real(i) * tr0(2) + real(j) * tr0(3)
           y = tr0(4) + real(i) * tr0(5) + real(j) * tr0(6)

        else if (chip .eq. 1) then

           x = tr1(1) + real(i) * tr1(2) + real(j) * tr1(3)
           y = tr1(4) + real(i) * tr1(5) + real(j) * tr1(6)

        else if (chip .eq. 2) then

           x = tr2(1) + real(i) * tr2(2) + real(j) * tr2(3)
           y = tr2(4) + real(i) * tr2(5) + real(j) * tr2(6)
           
        else if (chip .eq. 3) then
           
           x = tr3(1) + real(i) * tr3(2) + real(j) * tr3(3)
           y = tr3(4) + real(i) * tr3(5) + real(j) * tr3(6)

        end if

        end
