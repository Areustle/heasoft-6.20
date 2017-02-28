      subroutine exmerge(cfile,nfiles,nevents,shftime,x,y,pha,nexp,
     &     exptime,frac,dead,nobc,obctime,obcshftime,prtime,bctime,bad,
     &     samples,hkstart,status)

C This routine will read all of the rows from the 
C     EVENTS, EXPOSURE and OBC_PACKET extensions
C
C I   cfile     List of event files
C I   nfiles    number of event files
C I/O nevents   number of lines in merged EVENTS extension
C I/O nexp      number of lines in merged EXPOSURE extension
C I/O nobc      number of lines in merged OBC_PACKET extension
C O   shftime   SHF key of record
C O   x         Detector X pixel
C O   y         Detector Y pixel
C O   pha       PHA 
C O   exptime   SHF key of record
C O   frac      Fractional Exposure
C O   dead      Deadtime correction
C O   obctime
C O   obcshftime
C O   prtime
C O   bctime
C O   bad 
C O   samples
C O   hkstart   The 1st HK time in spacecraft clock

      implicit none
C EVENTS extension
      character(160) cfile(256),filename      
      INTEGER*4 nfiles,nevents
      REAL*8 shftime(nevents)
      INTEGER*4 x(nevents), y(nevents),pha(nevents)
C EXPOSURE extension
      INTEGER*4 nexp
      REAL*8 exptime(nexp)
      REAL frac(nexp),dead(nexp)
C OBC_PACKET extension
      INTEGER*4 nobc, hkstart(99)
      REAL*8 obctime(nobc),prtime(nobc)
      REAL bctime(nobc)
      INTEGER*4 obcshftime(nobc)
      INTEGER*2 bad(nobc),samples(nobc)

      INTEGER*4 i,nhdu,status,iounit,hdutype,readwrite,blocksize,
     &     nullj,j,jj,jjj,ii
      INTEGER*4 naxis2,nrows,srow,k,kk,kkk,steps,even_steps
      INTEGER*2 nulli
      REAL*8 nulld
      REAL nulle
      character(8) keyword
      character subname*50,context*160,comment*80,errm*255,string*255
      CHARACTER errtxt*30,stat*15
      LOGICAL anynull

      readwrite=0
      subname="exmerge: "
      errtxt=" "
      nhdu=0
      nulli=0
      nullj=0
      nulle=0.0
      nulld=0.0d0
      j=0
      jj=0
      jjj=0
      k=1
      kk=1
      kkk=1
      do ii=1,nfiles
         filename=cfile(ii)
         
         CALL FTGIOU(iounit,status)
         if(status.ne.0) then
            context="ERROR: Can't get unit number"
            goto 999
         endif
         
C========================================================
C        Open the FITS file
C========================================================
         CALL FTOPEN(iounit,filename,readwrite,blocksize,status)
         if(status.ne.0) then 
            CALL FTGERR(status,errtxt)
            context="ERROR: Can't find "//filename
            goto 999
         endif
         string="Now reading "//filename
         CALL RMVXBK(string)
         CALL xwrite(string,10)
C.......................................................
C Go to the EVENTS extension
C.......................................................

         nhdu=3
         CALL FTMAHD(iounit,nhdu,hdutype,status)
C NAXIS2 of EVENTS extension
         keyword="NAXIS2"
         CALL FTGKYJ(iounit,keyword,naxis2,comment,status)
         if(status.ne.0) then 
            context="ERROR: NAXIS2 keyword not found."
            goto 999
         endif
         
C Determine the optimum number of rows I should read at once       

         CALL FTGRSZ(iounit,nrows,status)
         even_steps=mod(naxis2,nrows)
         if(nrows.gt.naxis2) then
            nrows=naxis2
            steps=1
         elseif(nrows.eq.naxis2) then
            steps=1
         else
            if(even_steps.eq.0) then
               steps=(naxis2/nrows)
            else
               steps=(naxis2/nrows)+1
            endif
         endif
         srow=1
         write(string,10) naxis2,nrows
 10      format("   Reading EVENTS. Number of rows ",I9,
     &        " in steps of",I9)
         CALL RMVXBK(string)
         CALL xwrite(string,15)
         do i=1,steps
            if( (srow+nrows).gt.naxis2) then
               nrows=naxis2-(srow-1)
               write(string,41) nrows
 41            format("Number of rows reduced to ",I9)
               CALL RMVXBK(string)
               CALL xwrite(string,20)
            endif
            write(string,40) naxis2,nrows, srow,i
 40         format("Naxis2, Nrows, Startrow, Step# ",I9,I9,I9,I9)
            CALL RMVXBK(string)
            CALL xwrite(string,20)
C SHFTIME
            CALL FTGCVD(iounit,1,srow,1,nrows,nulld,shftime(k),anynull,
     &           status)
C X
            CALL FTGCVJ(iounit,2,srow,1,nrows,nullj,x(k),anynull,
     &           status)
C Y
            CALL FTGCVJ(iounit,3,srow,1,nrows,nullj,y(k),anynull,
     &           status)
C PHA
            CALL FTGCVJ(iounit,4,srow,1,nrows,nullj,pha(k),anynull,
     &           status)
            if(status.ne.0) then 
               if(status.ne.107) then
                  context="ERROR: Reading EVENTS table."
                  goto 999
               else
                  status=0
               endif
            endif
            srow=i*nrows+1
            k=j+i*nrows+1
         enddo
         j=j+naxis2
         k=j+1

C.......................................................
C Go to the EXPOSURE extension
C.......................................................
         nhdu=4
         CALL FTMAHD(iounit,nhdu,hdutype,status)
C NAXIS2 of EXPOSURE extension
         keyword="NAXIS2"
         CALL FTGKYJ(iounit,keyword,naxis2,comment,status)
         if(status.ne.0) then 
            context="ERROR: NAXIS2 keyword not found."
            goto 999
         endif
C Determine the optimum number of rows I should read at once       

         CALL FTGRSZ(iounit,nrows,status)
         even_steps=mod(naxis2,nrows)
         if(nrows.gt.naxis2) then
            nrows=naxis2
            steps=1
         elseif(nrows.eq.naxis2) then
            steps=1
         else
            if(even_steps.eq.0) then
               steps=(naxis2/nrows)
            else
               steps=(naxis2/nrows)+1
            endif
         endif
         srow=1
         write(string,20) naxis2,nrows
 20      format("   Reading EXPOSURE. Number of rows ",I9,
     &        " in steps of",I9)
         CALL RMVXBK(string)
         CALL xwrite(string,15)
         do i=1,steps
            if( (srow+nrows).gt.naxis2) then
               nrows=naxis2-(srow-1)
            endif
C EXPTIME
            CALL FTGCVD(iounit,1,srow,1,nrows,nulld,exptime(kk),anynull,
     &           status)
C FRAC
            CALL FTGCVE(iounit,2,srow,1,nrows,nulle,frac(kk),anynull,
     &           status)
C DEAD
            CALL FTGCVE(iounit,3,srow,1,nrows,nulle,dead(kk),anynull,
     &           status)
            if(status.ne.0) then 
               if(status.ne.107) then
                  context="ERROR: Reading EXPOSURE table."
                  goto 999
               else
                  status=0
               endif
            endif
            srow=i*nrows+1
            kk=jj+i*nrows+1
         enddo
         jj=jj+naxis2
         kk=jj+1

C.......................................................
C Go to the OBC_PACKET extension
C.......................................................
         nhdu=5
         CALL FTMAHD(iounit,nhdu,hdutype,status)
C NAXIS2 of OBC_PACKET extension
         keyword="NAXIS2"
         CALL FTGKYJ(iounit,keyword,naxis2,comment,status)
         if(status.ne.0) then 
            context="ERROR: NAXIS2 keyword not found."
            goto 999
         endif
C HKSTART used in computing binning for deadtime corrections
         keyword="HKSTART"
         CALL FTGKYJ(iounit,keyword,hkstart(ii),comment,status)
         if(status.ne.0) then 
            context="ERROR: HKSTART keyword not found."
            goto 999
         endif
        
C Determine the optimum number of rows I should read at once       

         CALL FTGRSZ(iounit,nrows,status)
         even_steps=mod(naxis2,nrows)
         if(nrows.gt.naxis2) then
            nrows=naxis2
            steps=1
         elseif(nrows.eq.naxis2) then
            steps=1
         else
            if(even_steps.eq.0) then
               steps=(naxis2/nrows)
            else
               steps=(naxis2/nrows)+1
            endif
         endif
         srow=1
         write(string,30) naxis2,nrows
 30      format("   Reading OBC_PACKET. Number of rows ",I9,
     &        " in steps of",I9)
         CALL RMVXBK(string)
         CALL xwrite(string,15)
         nulld=0.0d0
         do i=1,steps
            if((srow+nrows).gt.naxis2) then
               nrows=naxis2-(srow-1)
               write(string,41) nrows
               CALL RMVXBK(string)
               CALL xwrite(string,20)
            endif
            write(string,40) naxis2,nrows, srow,i
            CALL RMVXBK(string)
            CALL xwrite(string,20)
C OBCTIME
            CALL FTGCVD(iounit,1,srow,1,nrows,nulld,obctime(kkk),
     &           anynull,status)
C OBCSHFTIME
            CALL FTGCVJ(iounit,2,srow,1,nrows,nullj,obcshftime(kkk),
     &           anynull,status)
C PRTIME
            CALL FTGCVD(iounit,3,srow,1,nrows,nulld,prtime(kkk),anynull,
     &           status)
C BCTIME
            CALL FTGCVE(iounit,4,srow,1,nrows,nulle,bctime(kkk),anynull,
     &           status)
C BAD
            CALL FTGCVI(iounit,5,srow,1,nrows,nulli,bad(kkk),anynull,
     &           status)
C SAMPLES
            CALL FTGCVI(iounit,6,srow,1,nrows,nulli,samples(kkk),
     &           anynull,status)
            if(status.ne.0) then 
               if(status.ne.107) then
                  context="ERROR: Reading OBC_PACKET table."
                  goto 999
               else
                  status=0
               endif
            endif
            srow=i*nrows+1
            kkk=jjj+i*nrows+1
         enddo
         jjj=jjj+naxis2
         kkk=jjj+1
C
C The last row in the OBC_PACKET table is often zero correct the merged arrays 
C if this occurs
C
         if(obcshftime(kkk-1).eq.0) then
            jjj=jjj-1
            kkk=kkk-1
            srow=srow-1
            nobc=nobc-1
         endif
         CALL ftclos(iounit,status)
         CALL ftfiou(iounit,status)
C***************************************
C Now return to read the next FITS file
C***************************************


      enddo
      if (status.eq.0) goto 1000
 900  continue
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
 
