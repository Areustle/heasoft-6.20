      subroutine xrphaopt(iopt,itype,emax,phalo,phahi,ivect,ierr)

c apply PHA OPTions  

c This routine checks maximum and minumum energy channel options.

c The routine issues a warning if:

c    Itype = 2 (binned data) and the maximum channel number (iopt[4]) or 
c    the minimum (iopt[1]) exceed the total number of channels contained 
c    in the file.

c    Itype = 1 (event list) and no PHA column (ivect[9]) was found.

c Default values are << -1 and >> 100000 for phalo and phahi, respectively.  
c If no bounds are specified in iopt, the PHA column is ignored.

c  I  iopt   (i)  Vector of option flags
c  I  itype  (i)  = 1 for events, 2 for binned data.
c  I  emax   (i)  Number of energy channels in the data.
c  O  phahi  (i)  Upper bound for energy channel
c  O  phalo  (i)  Lower bound for energy channel
c I/O ivect  (i)  Vector of column numbers.
c  O  ierr   (i)  error status

c Author: eal  NASA/Goddard/HSTX  February, 1994

      character(80) errm
      integer iopt(*),phahi,phalo,phamin,phamax,ivect(*),emax,itype,ierr
      parameter (phamin = -999999999, phamax = 999999999)

      if(ierr.ne.0) return

c Set min and max PHA value.

      phalo = phamin
      phahi = phamax
      if(iopt(1).gt.0) phalo = iopt(1)
      if(iopt(4).gt.0) phahi = iopt(4)
      if((phalo.eq.phamin).and.(phahi.eq.phamax)) ivect(9) = 0

c Check for compatability between the option and the file.

      if((iopt(1).gt.0).or.(iopt(4).gt.0)) then
         if((ivect(9).eq.0).and.(itype.eq.1)) then
            ierr = 1057
            errm = '  No PHA column found' //
     &             ' -- Energy options not applied.'
         elseif((itype.ge.2).and.((iopt(1).gt.emax)
     &                       .or. (iopt(4).gt.emax))) then
            ierr = 1058
            errm = '  Too few energy channels in the data' //
     &             ' -- Energy options not applied.'
         endif
      endif

      IF(ierr.gt.0) THEN
         CALL xwarn(errm,1)
         iopt(1) = 0
         iopt(4) = 0
         phalo = phamin
         phahi = phamax
      ENDIF

      return
      end
