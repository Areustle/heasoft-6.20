      subroutine xrphaopt(iopt,itype,emax,phalo,phahi,ivect,ierr)
      implicit none
c SET PHA bounds for XRonos applications.  Default values are << -1 and
c >> 100000 for phalo and phahi, respectively.  If no bounds are specified
c in iopt, the PHA column is ignored.

c  I  iopt   (i)  Vector of option flags
c  I  itype  (i)  = 1 for events, 2 for binned data.
c  I  emax   (i)  Number of energy channels in the data.
c  O  phahi  (i)  Upper bound for energy channel
c  O  phalo  (i)  Lower bound for energy channel
c I/O ivect  (i)  Vector of column numbers.
c  O  ierr   (i)  error status

c Author: eal  NASA/Goddard/HSTX  February, 1994
c Revised: eal                    April, 1994

c Subroutines called: none

      character(80) errm
      integer iopt(*),phahi,phalo,phamin,phamax,ivect(*),emax,itype,ierr
      parameter (phamin = -999999999, phamax = 999999999)

      if(ierr.ne.0) return

c Set min and max PHA value.

      phalo = phamin
      phahi = phamax
      if(iopt(1).ge.0) phalo = iopt(1)
      if(iopt(4).gt.0) phahi = iopt(4)
      if((phalo.eq.phamin).and.(phahi.eq.phamax)) ivect(9) = 0

c Check for compatability between the option and the file.

      if((iopt(1).ge.0).or.(iopt(4).gt.0)) then
         if((ivect(9).eq.0).and.(itype.eq.1)) then
            ierr=5
            errm = ' No PHA column found or VC Option is set = 0'
            call xwrite(errm,1)
            errm ='      -- Energy options not applied.'
            call xwrite(errm,1)
         elseif((itype.ge.2).and.((iopt(1).gt.emax)
     &                       .or. (iopt(4).gt.emax))) then
            ierr=5
            errm = ' Too few energy channels in the data --'
            call xwrite(errm,1)
            errm = 'Energy options not applied.'
            call xwrite(errm,1)
         endif
      endif

      IF(ierr.gt.0) THEN
         ierr=0
         iopt(1) = 0
         iopt(4) = 0
         phalo = phamin
         phahi = phamax
      ENDIF

      return
      end
