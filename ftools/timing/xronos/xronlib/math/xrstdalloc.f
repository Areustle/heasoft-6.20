C allocation related routines for Xronos

      subroutine xrstdalloc(nser,nper,rntsta,dntsta,intsta,
     $     dpera,dpdota,chival,chierr,imode,status)
      implicit none
C     (de)allocates memory for the middle-sized arrays in XRONOS that
C     will only be "freed" at the end of the run
C
C     imode=1  allocate, imode=2 deallocate
C
     
      integer nv,nser,nper,
     $     rntsta,dntsta,intsta,dpera,dpdota,chival,chierr,xr,sxr,
     $     imode,status
      character(80) context

      if (imode.eq.1) then
C     dpera,dpdota are (#periods searched) arrays
         nv=nper
         call udmget(nv,7,dpera,status)
         call udmget(nv,7,dpdota,status)
      else
         call udmfre(dpera,7,status)
         call udmfre(dpdota,7,status)
      endif      

      if (imode.eq.1) then
         nv = nper*nser
C     chival, chierr are (#periods * #series ) arrays
         call udmget(nv,6,chival,status)
         call udmget(nv,6,chierr,status)
      else
         call udmfre(chival,6,status)
         call udmfre(chierr,6,status)
      endif

      if (imode.eq.1) then
C     intsta is a (20,#series,#periods) integer array
         nv=20*nser*nper
         call udmget(nv,4,intsta,status)
C     rntsta is a (20,#series,#periods) real array
         call udmget(nv,6,rntsta,status)
C     dntsta is a (20,#series,#periods) double array
         call udmget(nv,7,dntsta,status)
      else
         call udmfre(intsta,4,status)
         call udmfre(rntsta,6,status)
         call udmfre(dntsta,7,status)
      endif
      
      if(status.ne.0) then
         context='Couldn''t allocate standard arrays'
      endif
         
      return
      end



      integer function get_intsta(intsta,nser,nper,i,j,k)
      integer intsta(20,nser,nper)
      integer i,j,k,nser,npi

      get_intsta=intsta(i,j,k)

      return
      end


      subroutine lc_pintsta4(intsta,nser,iset)
      integer iset,nser,intsta(20,nser),m
      do m=1,nser
         intsta(4,m) = iset
      enddo
      return
      end
