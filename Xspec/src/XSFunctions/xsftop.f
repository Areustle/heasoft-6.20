
      SUBROUTINE xsftop(funit,fname,rwmode,block,status)

      INTEGER funit,rwmode,block,status
      CHARACTER*(*) fname

C       wrapper for opening fits files which operates on the filename
C       with clnfln and trlnam before call FTOPEN
C
C       funit   i  Fortran I/O unit number
C       fname   c  name of file to be opened
C       rwmode  i  file access mode: 0 = readonly; else = read and write
C       block   i  returned record length blocking factor
C       status  i  returned error status (0=ok)
C
C       dah - 3/18/96

      character(255) ctmp, trfnam, clnfln
      EXTERNAL trfnam, clnfln

      ctmp = trfnam(clnfln(fname))
      CALL ftopen(funit,ctmp,rwmode,block,status)

      RETURN
      END

