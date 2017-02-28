      subroutine go_imgext(Lun, Extnum, Status)

      implicit none
      include '../include/maxvals.inc'
c
c  Searches fits file for image (extension, or
c  compressed in bin table), returns extension number
c  Routine exits with CHDU set to image 
c
c  I  Lun    (i)  Logical unit of open FITS file
c I/O Extnum (i)  Extension number of event table
c  O  Status (i)  Error flag (0=OK)
c
      integer Lun, Extnum, Status

      include '../include/io.inc'
c
c  Local variables
c
      integer numimgkeys, numimgvals, hdunum, hdutype
      parameter (numimgkeys = 2, numimgvals = 2)
      character(20) imgkeys(numimgkeys), imgvals(numimgvals)
      data imgkeys /'XTENSION','ZIMAGE'/
      data imgvals /'IMAGE', 'T'/

      if ( Extnum.lt.0 ) then
         call matchext(Lun,imgkeys,numimgkeys,imgvals,numimgvals,
     &                 hdunum,Status)
      else
         call ftmahd(Lun, Extnum+1, hdutype, Status)
      endif

      if ( Status.eq.0 ) then
         Extnum = hdunum - 1
         write(ZWRite,*) ' Found image in extension ', Extnum
         call xwrite(ZWRite, 15)
      endif

      return
      end
