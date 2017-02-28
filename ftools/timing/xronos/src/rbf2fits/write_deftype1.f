C*******************************************************************
      SUBROUTINE write_deftype1(fits_unit, status)
C*******************************************************************
C
C     Input parameters:
      integer           fits_unit
C
C     Output parameters:
      integer       status

      integer       tfields, varidat, nrows, tnull
      character(20)  EXT_NAME_VAL
      PARAMETER(EXT_NAME_VAL='COUNTS', tnull = 254)
C
      parameter     (tfields = 1, varidat = 0, nrows = 100)
      character(4)   tform(tfields)
      character(30)  ttype(tfields)
      character(15)  tunit(tfields)
C
      tform(1) = '1B'
      ttype(1) = 'COUNT'
      tunit(1) = 'count'
C
      CALL FTPHBN(fits_unit,nrows,tfields,ttype,tform,tunit,
     &     EXT_NAME_VAL,varidat, status)
C
C     Define the columns in the array:
      CALL FTBDEF(fits_unit, tfields, tform, varidat, nrows, status)

C     Define undefined pixel value.
      CALL FTTNUL(fits_unit, 1, tnull, status)

      RETURN 
      END
