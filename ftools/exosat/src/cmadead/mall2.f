C allocation

      subroutine MALL2(npoint,gndtime,shftime,prtime,bctime,bad,samples,
     &     imode,status)
C 
C (de)allocates memory for GNDTIME,SHFTIME,PRTIME,BCTIME,BAD and SAMPLES array 
C
C I   npoint  (i)
C I   *time   (i)
C I   bad     (i)
C I   samples (i)
C I   imode   (i) imode=1  allocate, imode=2 deallocate
C I/O status  (i)    
C
      implicit none
C Input     
      INTEGER*4 npoint,imode,status
      INTEGER*4 gndtime,shftime,prtime,bctime,bad,samples


C Local

      INTEGER*4 nv
      CHARACTER  context*255, errm*255, subname*160

      subname='mall2:'

      IF (status.ne.0) RETURN
C
      gndtime = 0
      shftime = 0
      prtime = 0
      bctime = 0
      bad = 0 
      samples = 0

      if (imode.eq.1) then
         nv=npoint
         call udmget(nv,7,gndtime,status)
         call udmget(nv,4,shftime,status)
         call udmget(nv,7,prtime,status)
         call udmget(nv,6,bctime,status)
         call udmget(nv,3,bad,status)
         call udmget(nv,3,samples,status)
         context='Couldn''t allocate standard arrays'
      else
         call udmfre(gndtime,7,status)
         call udmfre(shftime,4,status)
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
