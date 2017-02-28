C*******************************************************************
      SUBROUTINE write_deftype2(fits_unit, status)
C*******************************************************************
C
C     Input parameters:
      integer           fits_unit
C
C     Output parameters:
      integer       status
      character(20)  EXT_NAME_VAL
      PARAMETER(EXT_NAME_VAL='RATE')
C
      integer       tfields, varidat, nrows
      parameter     (tfields=2,varidat=0,nrows=100)
      character(4)   tform(tfields)
      character(30)  ttype(tfields)
      character(15)  tunit(tfields)
C
      tform(1) = '1E'
      ttype(1) = 'RATE'
      tunit(1) = 'count/s'
C
      tform(2) = '1E'
      ttype(2) = 'ERROR'
      tunit(2) = 'count/s'
C
      CALL FTPHBN(fits_unit,nrows,tfields,ttype,tform,tunit,
     &     EXT_NAME_VAL,varidat, status)
C
C     Define the columns in the array:
      CALL FTBDEF(fits_unit, tfields, tform, varidat, nrows, status)

      RETURN 
      END
