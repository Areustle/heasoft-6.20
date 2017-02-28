      subroutine exogti(cfile,nfiles,gtilist,ngti)

C This routine will read the first table extension of the event files STDGTI
C and find the start and stop time of the observation set.
C
C I   cfile     List of event files
C I   nfiles    number of event files
C O   gtilist   The list of start and stop times for the GTI.
c O   ngti      The number of rows in the gtilist = nfiles

      implicit none
      
      INTEGER*4 i,nhdu,status,iounit,hdutype,readwrite,blocksize,
     &     nfiles,ngti,gtilist(256,2),nullj,j,start,stop
c     &     ,old_stop
      character(160) cfile(256),filename
      character subname*50,context*160,errm*255
      CHARACTER errtxt*30,stat*15
      LOGICAL anynull

      readwrite=0
      subname="exogti: "
      errtxt=" "
      nhdu=0
      nullj=0
      j=1
      status=0
      do i=1,nfiles
         filename=cfile(i)
         CALL FTGIOU(iounit,status)
         if(status.ne.0) then
            context="Can't get unit number"
            goto 999
         endif
         
C========================================================
C        Open the FITS file - READ
C========================================================
         CALL FTOPEN(iounit,filename,readwrite,blocksize,status)
         if(status.ne.0) then 
            CALL FTGERR(status,errtxt)
            context="Can't find "//filename
            goto 999
         endif
         
C        
C        Go to the STDGTI extension
C     
         nhdu=2
         CALL FTMAHD(iounit,nhdu,hdutype,status)
C START time
         CALL FTGCVJ(iounit,1,1,1,1,nullj,start,anynull,
     &        status)
         if(status.ne.0) then 
            context="ERROR: Reading STDGTI table."
            goto 999
         endif
C STOP time
         CALL FTGCVJ(iounit,2,1,1,1,nullj,stop,anynull,
     &        status)
         if(status.ne.0) then 
            context="ERROR: Reading STDGTI table."
            goto 999
         endif

         gtilist(i,1)=start
         gtilist(i,2)=stop

C For merging of GTI
c         if(i.eq.1) then
c            gtilist(j,1)=start
c         elseif((start-old_stop).gt.8) then
c            gtilist(j,2)=old_stop
c            j=j+1
c            gtilist(j,1)=start
c         endif
c         old_stop=stop
      enddo

 900  continue
      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)

c      gtilist(j,2)=old_stop
c      ngti=j
      ngti=nfiles

      if (status.eq.0) goto 1000
 999  continue
      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)
      write(stat,*) status
      CALL RMVXBK(stat)
      errm=subname//' '//context//', '//errtxt//' '//stat
      CALL RMVXBK(errm)
      CALL xaerror(errm,5)
 1000 continue
      return
      end
 
