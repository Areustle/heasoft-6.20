      subroutine go_evttbl(Lun, Extnum, Nrows, Status)

      implicit none
      include '../include/maxvals.inc'
c
c  Searches fits file for event table, returns extension
c  number of event table and number of rows
c  Routine exits with CHDU set to event table
c
c  I  Lun    (i)  Logical unit of open FITS file
c I/O Extnum (i)  Extension number of event table
c  O  Nrows  (i)  Number of rows in event table
c  O  Status (i)  Error flag (0=OK)
c
c
      integer Lun, Extnum, Nrows, Status

      include '../include/io.inc'
c
c  Local variables
c
      integer numevtkeys, numevtvals, hdunum, hdutype
      parameter (numevtkeys = 2, numevtvals = 1)
      character(8) evtkeys(numevtkeys), evtvals(numevtvals)
      data evtkeys /'EXTNAME','HDUCLAS1'/
      data evtvals /'EVENT*'/

      character(80) comment

      Nrows = 0

      if ( Extnum.lt.0 ) then
         call matchext(Lun,evtkeys,numevtkeys,evtvals,numevtvals,
     &                 hdunum,Status)
      else
         hdunum = Extnum + 1
         call ftmahd(Lun, hdunum, hdutype, Status)
      endif

      if ( Status.eq.0 ) then
         call FTGKYJ(Lun,'NAXIS2',Nrows,comment,Status)
         if ( Extnum.lt.0 ) then
            write(ZWRite,*) ' Found event table in extension ', hdunum-1
            call xwrite(ZWRite, 15)
         endif
         Extnum = hdunum - 1
      endif

      return
      end
