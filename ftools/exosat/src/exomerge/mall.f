C allocation

      subroutine MALL(nevents,shftime,x,y,pha,nexp,exptime,frac,dead,
     &     nobc,obctime,obcshftime,prtime,bctime,bad,samples,imode,
     &     status)
C 
C (de)allocates memory for TIME and RATE array 
C
C I   nevents
C I   shftime  
C I   x 
C I   y 
C I   pha   
C I   imode   imode=1  allocate, imode=2 deallocate
C I/O status  
C
      implicit none
C Input     
      INTEGER*4 nevents,shftime,x,y,pha,imode,status
      INTEGER*4 nexp,exptime,frac,dead
      INTEGER*4 nobc,obctime,obcshftime,prtime,bctime,bad,samples

C Local
      INTEGER*4 nv

 
      character(160) context,subname
      character(255) errm
      subname='mall1:'
C
C
      shftime = 0
      x = 0
      y = 0 
      pha = 0 
      exptime = 0
      frac = 0
      dead = 0
      obctime = 0
      obcshftime = 0
      prtime = 0
      bctime = 0
      bad = 0 
      samples = 0

      if (imode.eq.1) then
         nv=nevents
         call udmget(nv,7,shftime,status)
         call udmget(nv,4,x,status)
         call udmget(nv,4,y,status)
         call udmget(nv,4,pha,status)
         nv=nexp
         call udmget(nv,7,exptime,status)
         call udmget(nv,6,frac,status)
         call udmget(nv,6,dead,status)
         nv=nobc
         call udmget(nv,7,obctime,status)
         call udmget(nv,4,obcshftime,status)
         call udmget(nv,7,prtime,status)
         call udmget(nv,6,bctime,status)
         call udmget(nv,3,bad,status)
         call udmget(nv,3,samples,status)
         context='Couldn''t allocate standard arrays'
      else
         call udmfre(shftime,7,status)
         call udmfre(x,4,status)
         call udmfre(y,4,status)
         call udmfre(pha,4,status)

         call udmfre(exptime,7,status)
         call udmfre(frac,6,status)
         call udmfre(dead,6,status)

         call udmfre(obctime,7,status)
         call udmfre(obcshftime,4,status)
         call udmfre(prtime,7,status)
         call udmfre(bctime,6,status)
         call udmfre(bad,3,status)
         call udmfre(samples,3,status)
        context='Couldn''t deallocate standard arrays'
      endif      
c
      
      if(status.ne.0) then
         errm=subname//' '//context
         CALL xaerror(errm,5)
      endif
         
      return
      end
