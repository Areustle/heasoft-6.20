C      File name: ifhisto.f
C
C      Subroutine: fhisto
C
C      Author/Date: Ning Gan August 1998
C
C      Description: Fortran wrapper for IRAF to call fhisto
C
C
      subroutine ifhist()

      call iraf_fhisto()

      return

      end
