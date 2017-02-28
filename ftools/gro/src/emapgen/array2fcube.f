C
C
C  This routine takes a 1 dimnesional array converts it to a 
C three dimensional data cube and writes it into the primary 
C image region of a fits file.  The code assumes that the 
C fits file has already been opened.
C
C
C  lun_fits - the logical number for the fits file
C  cube - the data cube that the one dimensional data 
C        will be placed in.  In the program calling ARRAY2FCUBE 
C        this variable will be a one dimensional array, but inside 
C        ARRAY2CUBE the data are converted to a cube.
C  NAXIS1 - first dimension of the data cube
C  NAXIS2 - second dimension of the data cube  
C  NAXIS3 - third dimension of the data cube
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ARRAY2FCUBE(LUN_FITS,CUBE,NAXIS1,NAXIS2,NAXIS3,
     &   status)
      implicit none 

      INTEGER NAXIS1,NAXIS2,NAXIS3,LUN_FITS,STATUS,GROUP
      INTEGER*2 CUBE(NAXIS1,NAXIS2,NAXIS3)


C
C    Group is a way of somehow connecting images.  It is mostly used 
C by radio astronomers.  For most purposes, it can be set to 1.
C


      GROUP = 1
         call ftp3di(lun_fits,group,naxis1,naxis2, 
     &    naxis1,naxis2,naxis3,cube,status)
      RETURN
CHCCCCCCCCCCCCCCCCCCCCC END MAPGEN.SOURCE(ARRAY2FCUBE) CCCCCCCCCCCCCCCCCCCCCC
      END
