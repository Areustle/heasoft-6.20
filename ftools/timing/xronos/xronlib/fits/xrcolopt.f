      subroutine xrcolopt(iopt,nfield,itype,ivect,ierr)
      implicit none

c apply XRonos COLumn OPTions.  

c This routine overrides the column numbers passed to it in IVECT
c (presumably having been been set by subroutine xrftgcol).

c If itype = 1 (event list) the RATE and ERROR column (IVECT(2) and
c IVECT(3) and IVECT(4) ) are turned off.

c If both a deadtime column and a fractional exposure column are
c present, the deadtime column gets turned off.

c File options are not applied if any exceed the available number of columns.
c
c   I  iopt       array for file options
c   I  nfield     Total number of columns in the FITS extension.
c   I  itype      = 1 for event lists
c   O  ivect(n)   The column numbers for TIME (1), RATE (2), ERROR (3),
c                                        and Dead time (4), exposure (6)
c                                        PHA (9), among others not rel. here
c   O  ierr       status = 1056 if any relevant iopt > nfield

c File options relevant to this subroutine.           Xronos file option:
c      iopt(5) = n to assign column n to X-axis (time)   --  VXn
c      iopt(6) = n to assign column n to Y-axis          --  VYn
c      iopt(7) = n to assign column n to PHA             --  VCn
c      iopt(8) = n to assign column n to Dead time       --  VEn
c      iopt(9) = n to assign column n to Y-error         --  VSn
c 
c dec 1999
c If iopt(7) =0 set ivect(9) =0 no PHA used for event file
c

      include '../include/io.inc'
      integer iopt(*),nfield,ivect(*),ierr,itype
      character(80) context
      parameter (subname = 'xrcolopt:')

c Errors in infile options

      if(ierr.ne.0) return

      IF (iopt(5).GT.nfield.or.iopt(6).GT.nfield.or.iopt(7).GT.nfield
     &.or.iopt(8).GT.nfield.or.iopt(9).GT.nfield) THEN
         ierr=1056
         context='File options invalid.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         return
      ENDIF

c Apply command line file options.
      if (iopt(5).NE. 0) ivect(1) = iopt(5)
      if (iopt(6).GT. 0) ivect(2) = iopt(6)
      if (iopt(7).GT. 0) ivect(9) = iopt(7)
      if (iopt(7).EQ.-99) ivect(9) = 0
      if (iopt(9).GT. 0) ivect(3) = iopt(9)
      if (iopt(8).GT. 0) ivect(4) = iopt(8)
      if (iopt(8).LE.-1) ivect(4) = 0

c Turn off irrelevant columns.

c Case of event lists.

      if(itype.eq.1) then
         ivect(2) = 0
         ivect(3) = 0
         ivect(4) = 0
      endif

c Case of both DEADC and FRACEXP columns present.

      if(ivect(6).gt.0) ivect(4) = 0

      return
      end
