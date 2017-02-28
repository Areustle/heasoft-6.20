c==============================================================         
      subroutine get_rig(fn,drig,rigstp,ndrig)
c--------------------------------------------------------------         
c     read data on rigidity and make the distribution over cor_min
c     during GTI for a given event list
c
c     Note:
c     2. DRIG=fraction of time (within GTI) at given rigidity
c     3. DRIG(I) corresponds to CORMIN: RIGSTP*(I-1) : RIGSTP*I 
c     Created: Tue May 17 15:58:18 EDT 1994
c    Modified: Wed Sep 22 17:48:27 EDT 1999  (ilana Harrus)
c--------------------------------------------------------------         
      implicit none
      include 'common_evts.inc'

c---- for fitsio lib
      integer iunit
      integer rwmode,block,hdutype,status
      character errtext*30, comment*80
      logical exact,anyf
      integer*2 inull
      real*8 dnull
      real*4 enull
c---- rigidity data
      integer ntmax
      parameter(ntmax=30000)
      real*8 trig(ntmax)
      real cor_min(ntmax)
      integer nt
      real drig(*),rigstp
      real*8 dt,dts,dtl
      integer ndrig,nw
      real*8 t,texp
      real avrig
      character*(*) fn


c----- misc
      integer ic
      integer i,l,j,iw,l0
      integer len_trim


c==== MKF file for given event list
      print*,'Read COR_MIN data from file ',fn(:len_trim(fn))
c==== open fits file
      iunit=22
      rwmode=0
      status=0
      call ftopen(iunit,fn,rwmode,block,status)
c==== move to DATA extension
      call ftmahd(iunit,2,hdutype,status)
c---- Number of data points
      call ftgkyj(iunit,'NAXIS2',nt,comment,status)
      print*,' Total number of points in the MKF file:',nt
      if(nt.gt.ntmax) then
         print*,' >>> ERROR: get_rig: nt > ntmax. Stopping...'
         stop
      endif
c---- columns containing TIME 
      exact=.false.
      inull=0
      dnull=0.d0
      enull=0.e0
      call ftgcno(iunit,exact,'TIME',ic,status)
      call ftgcvd(iunit,ic,1,1,nt,dnull,trig,anyf,status)
c---- Find column with cut-off rigidity
c---- In old file (COR_MIN) (COR_MAX==COR_MIN)
c---- in new files (COR) 
c---- Therefore check both
      call ftgcno(iunit,exact,'COR',ic,status)
      if(status.ne.0) then
         print*,'Seems to be old format of mkf file?'
         status=0.
         call ftgcno(iunit,exact,'COR_MIN',ic,status)
         if(status.ne.0) print*,'COR column is absent in mkf file?'
      endif
      call ftgcve(iunit,ic,1,1,nt,enull,cor_min,anyf,status)
c==== close file
      call ftclos(iunit,status)

c==== final status
      call ftgerr(status,errtext)
      print*,' END reading mkf file'
      print*,' FITSIO context: ',errtext 

c==== fill drig      
      do i=1,ndrig
         drig(i)=0.
      end do 

c==== Divide any GTI on "nw" steps
      nw=25.
      texp=0.
c---- Loop over GTI
c     l0=2    Ilana's code
      l0=1
      do i=1,ngti
c         print*,'stop(i),start(i)',i,stop(i),start(i),ngti
         dtl=stop(i)-start(i)
         dts=1.
c------- How many pieces we need in order to have ~ dts sec resolution
         nw=int(dtl/dts)+1
c------- Duration of each piece
         dt=dtl/float(nw)
c------- Loop over small pieces in each GTI
         do j=1,nw
c.......... Approximate time of small piece
            t=(start(i)+dble(dt*(j-0.5)))
            texp=texp+dt
c.......... Look over MKF measurements and find proper time
c            do l=l0-1,nt-1    This led to core-dump
            do l=l0,nt-1
               if(t.ge.trig(l).and.t.le.trig(l+1)) then
c................ When TIME found calculate index of the DRIG array
                  iw=(cor_min(l)+cor_min(l+1))/2./rigstp+1
c................ Check that "iw" is valid index
                  if(iw.gt.0.and.iw.le.ndrig) then
c::::::::::::::::::: Fill drig array
                     drig(iw)=drig(iw)+dt
                  else
                     print*,'!! DRIG: bad value of COR_MIN in MKF file'
                     print*,iw,l
                     print*,cor_min(l),cor_min(l+1)
                  endif
c................ store the last used point in MKF file
                  l0=l
                  goto 100 
               endif
            end do 
            print*,'!! GID trouble: measurements not Found'
            stop
 100        continue
         end do 
      end do 
c==== Normalize DRIG to the whole exposure
c      print 20,texp
 20   format('Estimated exposure:',f15.4,' sec.',/,
     >    'R1     R2    fraction of time')
      avrig=0
      do i=1,ndrig
         drig(i)=drig(i)/texp
         avrig=avrig+rigstp*(i-0.5)*drig(i)
c         print 30,rigstp*(i-1),rigstp*i,drig(i)
 30      format(f4.1,3x,f4.1,5x,f8.3)
      end do 
      print*,' average rigidity=',avrig

      return
      end





