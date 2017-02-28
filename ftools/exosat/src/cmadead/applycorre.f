      subroutine applycorre(npoint,bin,rate,error,fracexp,minexpo,
     &     status)

C APPLY the CORREction to the count rate (read from LC file)
C using the fractional exposure array (generated from the event files)
C I   npoint   Number of rows in lightcurve 
C I   bin      Integration time
C I/O rate     Count rate. Output is corrected for deadtime
C O   error    Count rate error
C I   minexpo  Minimum fractional exposure cutoff

      implicit none

      integer npoint,i,status
      real rate(npoint),error(npoint)
      real*8 fracexp(npoint),bin,minexpo
      character(160) subname
      character(255) string
      PARAMETER (subname = 'applycorre:')


      if (status.ne.0) return
      write(string,10) npoint
 10   format("Number of rows in lightcurve",I6)
      call xwrite(string,30)
      write(string,20) bin,minexpo
 20   format("Bin size, minexpo ",F6.3,1x,F6.3)
      call xwrite(string,30)
      do i=1,npoint
         if(fracexp(i).ge.minexpo) then
            if(fracexp(i).gt.0.0) then
               rate(i)=rate(i)/fracexp(i)
            else
               rate(i)=0.0
            endif
         else
            fracexp(i)=0.0d0
            rate(i)=rate(i)
         endif
      
         error(i)=sqrt(rate(i)*bin)/bin
      enddo
      end
