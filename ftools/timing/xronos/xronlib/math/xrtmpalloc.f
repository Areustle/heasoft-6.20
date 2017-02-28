      subroutine xrtmpalloc(n,itype,narrays,imode,a,b,c,status)

C     (De)Allocates up to three arrays and tests for  success
C     
C     n       I  Size of arrays (nbin-no. pts. in accumulation or result arrays
C                                nser-number of series
C                                nper-number of periods (1 except efsearch)
C                                varies-varies from tool to tool
C                yi,syi,expi  n = nbin*nser*nper   real   (itype 6)
C                yp,syp       n = varies           real   (itype 6)
C                yr,syr,expr  n = varies*nbin*nser real   (itype 6)
C                xr,sxr       n = nbin             double (itype 7)
C                Note: n is not used when deallocating
C     itype   I  Type of array being allocated (see dmcommon.inc for details)
C     narrays I  Number of arrays to allocate (2 or 3 usually)
C     imode   I  1- allocate 2- deallocate
C     a,b,c  I/O Addresses for the arrays
C     status  O  Success or not
C

      include '../include/io.inc'
      integer n,itype,narrays,imode,a,b,c,status
      parameter (subname = 'xrtmpalloc:')

      if(status.ne.0) return

      if(imode.eq.1) then
         if(narrays.ge.1) 
     $        call udmget(n,itype,a,status)
         if(narrays.ge.2) 
     $        call udmget(n,itype,b,status)
         if(narrays.ge.3) 
     $        call udmget(n,itype,c,status)
         errm = subname//' '//'Couldn''t allocate large arrays'
         if(status.ne.0) call xaerror(errm, 5)
      else
         if(narrays.ge.1) 
     $        call udmfre(a,itype,status)
         if(narrays.ge.2) 
     $        call udmfre(b,itype,status)
         if(narrays.ge.3) 
     $        call udmfre(c,itype,status)
         errm = subname//' '//'Couldn''t deallocate large arrays'
         if(status.ne.0)
     $        call xaerror(errm, 5)
      endif
      
      return
      end
