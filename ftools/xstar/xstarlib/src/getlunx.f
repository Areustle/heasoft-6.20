C------------------------------------------------------------------------------
        subroutine getlunx(iounit)

C       get an unallocated logical unit number
C
C        O  (i) iounit - An unopened logical unit number
C       
C       James Peachey, HEASARC/GSFC/NASA  Hughes STX, November, 1996
C       Copied with minor changes from the FITSIO routine ftgiou.

        integer iounit

        iounit=0
        call lunlstx(iounit)
        end
