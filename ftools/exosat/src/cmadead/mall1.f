C allocation

      subroutine MALL1(npoint,time,rate,error,fracexp,imode,status)
C 
C (de)allocates memory for TIME and RATE array 
C
C I   npoint  (i)
C I   time    (i)
C I   rate    (i)
C I   error   (i)
C I   fracexp (i)
C I   imode   (i) imode=1  allocate, imode=2 deallocate
C I/O status  (i)    
C
      implicit none
C Input     
      INTEGER*4 npoint,time,rate,error,fracexp,imode,status
C Local
      INTEGER*4 nv

 
      character(160) context,subname
      character(255) errm
      subname='mall1:'
C
C
      time = 0
      rate = 0
      error = 0 
      fracexp = 0

      if (imode.eq.1) then
         nv=npoint
         call udmget(nv,7,time,status)
         call udmget(nv,6,rate,status)
         call udmget(nv,6,error,status)
         call udmget(nv,7,fracexp,status)
         context='Couldn''t allocate standard arrays'
      else
         call udmfre(time,7,status)
         call udmfre(rate,6,status)
         call udmfre(error,6,status)
         call udmfre(fracexp,7,status)
         context='Couldn''t deallocate standard arrays'
      endif      
c
      
      if(status.ne.0) then
         errm=subname//' '//context
         CALL xaerror(errm,5)
      endif
         
      return
      end
