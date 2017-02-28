CH  Lorraine Breedon (1.0.0 05 Apr 1999) Original working version

C  This is a subroutine to write OSO-8 B/C  rates data for a selected
c X-ray source
 
      SUBROUTINE OSOWR_RATES_BC(fitsunit,rates_out,row,tsec,
     &                      sct,esct,slt,sht,ns,srat,bbnrm,
     &                      area,status)
 
      IMPLICIT NONE
 
C Local variables

      integer fitsunit, row, status
      character(80)  errm
      character*(*) rates_out
      real sct,esct,bbnrm,slt,sht,srat,area,realns
      double precision tsec
      integer ns

      
 
      realns=real(ns)
      call ftpcld(fitsunit, 1, row, 1, 1, tsec, status)
      call ftpcle(fitsunit, 2, row, 1, 1, sct, status)
      call ftpcle(fitsunit, 3, row, 1, 1, esct, status)
      call ftpcle(fitsunit, 4, row, 1, 1, slt, status)
      call ftpcle(fitsunit, 5, row, 1, 1, sht, status)
      call ftpcle(fitsunit, 6, row, 1, 1, realns, status)
      call ftpcle(fitsunit, 7, row, 1, 1, srat, status)
      call ftpcle(fitsunit, 8, row, 1, 1, bbnrm, status)
      call ftpcle(fitsunit, 9, row, 1, 1, area, status)

      IF ( status.NE.0 ) THEN
         WRITE (errm,
     & '( '' Problem writing to output light-curve : '',a80)')
     & rates_out
         CALL XAERROR(errm,1)
         return
      ENDIF


      return

      end


 
