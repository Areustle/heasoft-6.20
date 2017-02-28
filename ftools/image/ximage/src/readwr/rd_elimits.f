      subroutine rd_elimits(Lun, Ei, Emin, Emax, Status)
c
c  Read TLMIN and TLMAX into Emin, Emax if not already set
c
c   I  lun    (i)   Logical unit of open image FITS file
c   I  ei     (i)   Energy column index
c  I/O emin   (i)   Minimum energy
c  I/O emax   (i)   Maximum energy
c   O  status (i)   Error flag (0=OK)
c
      integer Lun, Ei, Emin, Emax, Status
c
c  Local variables
c
      integer i, ival

      if ( Ei.le.0 ) return

      Status = 0

      if ( Emin.eq.0 ) then
         call ftgknj(Lun,'TLMIN',Ei,1,ival,i,Status)
         if ( i.le.0 ) Status = -1
         if ( Status.eq.0 ) then
            call xwrite(' Using TLMIN of Ecol for Emin', 15)
            Emin = ival
         endif
      endif

      Status = 0

      if ( Emax.eq.0 ) then
         call ftgknj(Lun,'TLMAX',Ei,1,ival,i,Status)
         if ( i.le.0 ) Status = -1
         if ( Status.eq.0 ) then
            Emax = ival
            call xwrite(' Using TLMAX of Ecol for Emax', 15)
         endif
      endif
c
c  If TLMIN, TLMAX not found, leave Emin/max alone
c
      Status = 0

      return
      end
