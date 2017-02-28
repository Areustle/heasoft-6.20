C*******************************************************************
      SUBROUTINE write_deftype0(fits_unit, status)
C*******************************************************************
C   
C     Input parameters:
      integer       fits_unit
C
C     Output parameters:
      integer       status
      character(20)  EXT_NAME_VAL
      PARAMETER(EXT_NAME_VAL='RATE')
C
      integer       tfields, varidat, nrows
      parameter     (tfields=4,varidat=0,nrows=100)
      character(4)   tform(tfields)
      character(30)  ttype(tfields)
      character(15)  tunit(tfields)
c
      tform(1) = '1D'
      ttype(1) = 'TIME'
      tunit(1) = 's'
c
      tform(2) = '1E'
      ttype(2) = 'FRACEXP'
      tunit(2) = ' '
c
      tform(3) = '1E'
      ttype(3) = 'RATE'
      tunit(3) = 'count/s'
c
      tform(4) = '1E'
      ttype(4) = 'ERROR'
      tunit(4) = 'count/s'
c
      CALL FTPHBN(fits_unit,nrows,tfields,ttype,tform,tunit,
     &           EXT_NAME_VAL, varidat, status)
c
c     Define the columns in the array:
      CALL FTBDEF(fits_unit,tfields, tform, varidat, nrows, status)
c
      RETURN 
      END
