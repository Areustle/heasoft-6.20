C------------------------------------------------------------------------------
        subroutine frelunx(iounit)

C       free specified logical unit number; if iounit=-1, then free all units
C       
C        I  (i) iounit - The logical unit number to be freed
C       
C       James Peachey, HEASARC/GSFC/NASA  Hughes STX, November, 1996
C       Copied with minor changes from the FITSIO routine ftfiou.

        integer iounit

        call lunlstx(iounit)
        end
