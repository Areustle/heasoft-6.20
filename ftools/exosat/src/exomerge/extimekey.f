      subroutine extimekey(cfile,first_obs,last_obs,stimekeys,status)

C This routine will read the second table extension of the event files 
C and find the first and last observation, set the tstart and tstop values,
C set the DATE- and TIME- keywords
C
C I   cfile     List of event files
C I   first_obs Element in cfile array containing filename whose tstart is the
C               tstart of the merged file
C I   last_obs  Same as first_obs but for the tstop time
C
C O   stimekeys The strings given in DATE-OBS, TIME-OBS,DATE-END,TIME-END
C               The first two keys are from the first observation
c               determined by the tstart time. The last two keys are
c               from the last observation

      implicit none
      
      INTEGER*4 nhdu,status,iounit,hdutype,readwrite,blocksize,
     &     first_obs,last_obs
      character(160) cfile(256),filename
      character(8) keyword
      character(80) stimekeys(4)
      character subname*50,context*160,comment*80,errm*255
      CHARACTER errtxt*30,stat*15

      readwrite=0
      subname="extimekey: "
      errtxt=" "
      nhdu=0
      filename=cfile(first_obs)
      CALL FTGIOU(iounit,status)
      if(status.ne.0) then
         context="ERROR: Can't get unit number"
         goto 999
      endif
         
C========================================================
C        Open the FITS file - READ
C========================================================
      CALL FTOPEN(iounit,filename,readwrite,blocksize,status)
      if(status.ne.0) then 
         CALL FTGERR(status,errtxt)
         context="ERROR: Can't find "//filename
         goto 999
      endif
         
C        
C        Go to the EVENTS extension
C     
      
      nhdu=3
      CALL FTMAHD(iounit,nhdu,hdutype,status)

C DATE-OBS
      keyword="DATE-OBS"
      CALL FTGKYS(iounit,keyword,stimekeys(1),comment,status)
      if(status.ne.0) then 
         context="ERROR: DATE-OBS keyword not found."
         goto 999
      endif
      CALL RMVBLK(stimekeys(1))

C TIME-OBS
      keyword="TIME-OBS"
      CALL FTGKYS(iounit,keyword,stimekeys(2),comment,status)
      if(status.ne.0) then 
         context="ERROR: TIME-OBS keyword not found."
         goto 999
      endif
      CALL RMVBLK(stimekeys(2))
      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)

      filename=cfile(last_obs)
      CALL FTGIOU(iounit,status)
      if(status.ne.0) then
         context="ERROR: Can't get unit number"
         goto 999
      endif


C========================================================
C        Open the FITS file - READ
C========================================================
      CALL FTOPEN(iounit,filename,readwrite,blocksize,status)
      if(status.ne.0) then 
         CALL FTGERR(status,errtxt)
         context="ERROR: Can't find "//filename
         goto 999
      endif

C        
C        Go to the EVENTS extension
C     
      
      nhdu=3
      CALL FTMAHD(iounit,nhdu,hdutype,status)

C DATE-END
      keyword="DATE-END"
      CALL FTGKYS(iounit,keyword,stimekeys(3),comment,status)
      if(status.ne.0) then 
         context="ERROR: DATE-END keyword not found."
         goto 999
      endif
      CALL RMVBLK(stimekeys(3))

C TIME-OBS
      keyword="TIME-END"
      CALL FTGKYS(iounit,keyword,stimekeys(4),comment,status)
      if(status.ne.0) then 
         context="ERROR: TIME-END keyword not found."
         goto 999
      endif
      CALL RMVBLK(stimekeys(4))
 900     continue
         CALL ftclos(iounit,status)
         CALL ftfiou(iounit,status)
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
 
