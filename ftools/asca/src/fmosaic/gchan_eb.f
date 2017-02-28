c=======================================================================
      subroutine gchan_eb(fn,e1,e2,nchan)
c-----------------------------------------------------------------------
c     Read the energy limits from a given file (*.rmf)
c     input:
c     fn - file name 
c     output:
c     nchan - number of channels
c     e1,e2 - arrays of channel boundaries
c-----------------------------------------------------------------------
      implicit none
      integer nchan
      character*(*) fn
      real e1(*),e2(*)

c---- for fitsio lib
      integer iunit,status
      integer rwmode,block,hdutype
      logical exact,anyf
      real*4 enull
      character comment*80
      integer ie1,ie2
      

      iunit=22
      status=0
c==== open fits file
      rwmode=0
      call ftopen(iunit,fn,rwmode,block,status)
c==== move to boundaries extension
      call ftmahd(iunit,3,hdutype,status)

c==== number of events in list
      call ftgkyj(iunit,'NAXIS2',nchan,comment,status)
c==== get columns no
      exact=.false.
      enull=0.e0
      call ftgcno(iunit,exact,'E_MIN',ie1,status)
      call ftgcno(iunit,exact,'E_MAX',ie2,status)
      call ftgcve(iunit,ie1,1,1,nchan,enull,e1,anyf,status)
      call ftgcve(iunit,ie2,1,1,nchan,enull,e2,anyf,status)

c==== close file
      call ftclos(iunit,status)

      if(status.ne.0) print*,'bad status in gchan_eb',status
      print*,'Read energy boundaries; done; nchan=',nchan

      return
      end





