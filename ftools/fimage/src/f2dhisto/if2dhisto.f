C      File name: if2dhisto.f
C
C      Subroutine: if2dhisto 
C
C      Author/Date: Ning Gan August 1998
C
C      Description: Fortran wrapper for IRAF to call f2dhisto 
C
C
      subroutine if2dhisto()

      call iraf_f2dhisto()

      return

      end
