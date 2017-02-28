      subroutine isitfits(lui,cfil,status)
      implicit none 
c Find out whether an external file IS in FITS format.
 
c   I   lui    (i) = input file unit number.  The lu is provided by the
c                    calling routine, to avoid possible conflicts with other
c                    lu's.  It is opened and closed in this routine.
c   I   cfil   (c) = input file name
c   0   status (i) = 0 if the file conforms to FITS standard and no
c                    FITSIO errors are encountered,
c                    nonzero otherwise.

c Subroutines called: ftopen, ftgkyl, ftclos

c Author:  Eric Lufkin, HEASARC/GSFC, August 1993
 
      character*(*) cfil
      integer status,lui,block,rwmode
      data block,rwmode / 2880, 0 /

      call ftopen(lui,cfil,rwmode,block,status)
      call ftclos(lui,status)

      return
      end
C
C

