      subroutine exowrite(star,outfile,cfile,nfiles,tlist,stimekeys,
     &     ra_nom,dec_nom,ontime,deadc,minwait,
     &     roll90,ngti,gtilist,nevents,shftime,x,y,pha,nexp,
     &     exptime,frac,dead,nobc,obctime,obcshftime,prtime,bctime,bad,
     &     samples,hkstart,clobber,status)

C This routine will write all of the rows for the GTI, EVENTS, EXPOSURE,
c and OBC_PACKET extensions. 
C
C I   star      Star Tracking Mode "STR1" or "STR2" or "NONE"
C I   outfile   Filename of output FITS event file
C I   cfile     List of event files
C I   nfiles    number of event files
C I   tlist     start and stop times, TSTART=tlist(1,1), TSTOP=tlist(nfiles,2)
C I   stimekeys Values for keywords DATE-OBS, TIME-OBS, DATE-END, TIME-END
C I   ra_nom    Nominal RA(CMA coords) if STR1, corrected for FSS in exfssco 
C I   dec_nom   Nominal Dec(CMA coords)if STR1, corrected for FSS in exfssco
C I   ontime    Sum of the ONTIME values from event files
C I   deadc     Weighted mean DEADC. Weighted by ONTIME
C I   minwait   Smallest MINWAIT value from event files
C I   roll90    The roll angle + 90 deg of the spacecraft in ST coords.
C I   ngti      Number of lines of Good Time Intervals
C I   gtilist   Array containing the merged GTI
C I   nevents   number of lines in merged EVENTS extension
C I   shftime   SHF key of record
C I   x         Detector X pixel
C I   y         Detector Y pixel
C I   pha       PHA 
C I   nexp      number of lines in merged EXPOSURE extension
C I   exptime   SHF key of record
C I   frac      Fractional Exposure
C I   dead      Deadtime correction
C I   nobc      Number of lines in merged OBC_PACKET extension
C I   hkstart   Time of 1st HK record in spacecraft clock 
C I   clobber   Overwrite event file if it exists?


      implicit none
      character*(*) outfile
      character(16) ttype(6),tform(6),tunit(6),extname
      character(80) star
      INTEGER*4 varidat,tfields,colnum,frow,felem,nelements
      INTEGER*4 tlist(256,2),tstart,tstop,telapse
C STDGTI extension
      INTEGER*4 ngti,gtilist(256,2)
      REAL*8 gti_start(256),gti_stop(256)
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
      INTEGER*4 nobc,hkstart(99)
      REAL*8 obctime(nobc),prtime(nobc)
      REAL bctime(nobc)
      INTEGER*4 obcshftime(nobc)
      INTEGER*2 bad(nobc),samples(nobc)

      INTEGER*4 i,nhdu,status,ounit,hdutype,readwrite,blocksize,
     &    nullj,j,jj,jjj,iunit,morekeys,ontime,dlu
      INTEGER*4 nrows,srow,k,kk,kkk,steps,even_steps,decimals
      INTEGER*2 nulli
      REAL*8 nulld,ra_nom,dec_nom,roll90
      REAL nulle,deadc,minwait
      character(8) keyword
      character(80) stimekeys(4)
      character subname*50,context*160,comment*80,errm*255
      CHARACTER errtxt*30,stat*15,string*255
      LOGICAL clobber,exists

      morekeys=0
      readwrite=0
      subname="exowrite: "
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

      tstart=tlist(1,1)
      tstop=tlist(nfiles,2)
      telapse=(tstop-tstart)


      CALL FTGIOU(ounit,status)
      if(status.ne.0) then
         context="ERROR: Can't get unit number"
         goto 999
      endif
      
      inquire(file=outfile,exist=exists)
      if(exists) then
         if(clobber) then
            call getlun(dlu)
            open(dlu,file=outfile,status='old',err=990)
            close(dlu,status='delete',err=990)
            call frelun(dlu)
         else
            context=outfile
            errtxt="file already exists"
            CALL RMVXBK(context)
            goto 999
         endif
      endif



C========================================================
C     Create the FITS event file
C========================================================
      CALL FTINIT(ounit,outfile,blocksize,status)
c      CALL ffinit(ounit,outfile,status)
      if(status.ne.0) then 
            CALL FTGERR(status,errtxt)
            context="ERROR: Can't initialize "//outfile
            goto 999
      endif
         
 20   continue


C Open original FITS event file to use as the template
         CALL FTGIOU(iunit,status)
         filename=cfile(1)
         readwrite=0
         CALL FTOPEN(iunit,filename,readwrite,blocksize,status)
         if(status.ne.0) then
            CALL FTGERR(status,errtxt)
            context="ERROR: Can't open "//filename//" for copying"
            goto 999
         endif
C Copy the FITS file from the first input FITS event file
C and update/modify as necessary
         CALL FTCOPY(iunit,ounit,morekeys,status)
c         call ffcphd(iunit,ounit,status)
         if(status.ne.0) then
            CALL FTGERR(status,errtxt)
            context="ERROR: Can't copy "//filename//" to "//outfile
            goto 999
         endif
c      endif
C.......................................................
C Primary Header
C.......................................................
C Modify keywords

C RA_NOM, DEC_NOM
      if (star.eq."STR1") then
         decimals=8
         comment="&"
         keyword="RA_NOM"
         CALL FTMKYD(ounit,keyword,ra_nom,decimals,comment,status)
         comment="&"
         keyword="DEC_NOM"
         CALL FTMKYD(ounit,keyword,dec_nom,decimals,comment,status)
      endif
C ONTIME
      comment="&"
      keyword="ONTIME"
      CALL FTMKYJ(ounit,keyword,ontime,comment,status)
C DEADC
      comment="&"
      keyword="DEADC"
      CALL FTMKYF(ounit,keyword,deadc,3,comment,status)
C MINWAIT
      comment="&"
      keyword="MINWAIT"
      CALL FTMKYE(ounit,keyword,minwait,4,comment,status)
C DATE-OBS,TIME-OBS,DATE-END,TIME-END
      comment="&"
      keyword="DATE-OBS"
      CALL FTMKYS(ounit,keyword,stimekeys(1),comment,status)
      keyword="TIME-OBS"
      CALL FTMKYS(ounit,keyword,stimekeys(2),comment,status)
      keyword="DATE-END"
      CALL FTMKYS(ounit,keyword,stimekeys(3),comment,status)
      keyword="TIME-END"
      CALL FTMKYS(ounit,keyword,stimekeys(4),comment,status)
C TSTART, TSTOP
      comment="&"
      keyword="TSTART"
      CALL FTMKYJ(ounit,keyword,tstart,comment,status)
      comment="&"
      keyword="TSTOP"
      CALL FTMKYJ(ounit,keyword,tstop,comment,status)
C TELAPSE
      comment="&"
      keyword="TELAPSE"
      CALL FTMKYJ(ounit,keyword,telapse,comment,status)
C Remove CHECKSUM, DATASUM
      keyword="CHECKSUM"
      CALL FTDKEY(ounit,keyword,status)
      keyword="DATASUM"
      CALL FTDKEY(ounit,keyword,status)

C.......................................................
C Go to the GTI extension
C.......................................................
      nhdu=2
      CALL FTCRHD(ounit,status)
      CALL FTMAHD(iunit,nhdu,hdutype,status)
      CALL FTCOPY(iunit,ounit,morekeys,status)
      tfields=2
      ttype(1)='START'
      ttype(2)='STOP'
      tform(1)='D'
      tform(2)='D'
      tunit(1)='s'
      tunit(2)='s'
      extname='STDGTI'
      nrows=ngti
      varidat=0
c      CALL FTPHBN(ounit,nrows,tfields,ttype,tform,tunit,extname,varidat,
c     &     status)

C Modify keywords

      comment="&"
      keyword="NAXIS2"
      CALL FTMKYJ(ounit,keyword,ngti,comment,status)
C DATE-OBS,TIME-OBS,DATE-END,TIME-END
      comment="&"
      keyword="DATE-OBS"
      CALL FTMKYS(ounit,keyword,stimekeys(1),comment,status)
      keyword="TIME-OBS"
      CALL FTMKYS(ounit,keyword,stimekeys(2),comment,status)
      keyword="DATE-END"
      CALL FTMKYS(ounit,keyword,stimekeys(3),comment,status)
      keyword="TIME-END"
      CALL FTMKYS(ounit,keyword,stimekeys(4),comment,status)
C Remove CHECKSUM, DATASUM
      keyword="CHECKSUM"
      CALL FTDKEY(ounit,keyword,status)
      keyword="DATASUM"
      CALL FTDKEY(ounit,keyword,status)
      colnum=1
      frow=1
      felem=1
      nelements=ngti
      do i=1,ngti
         gti_start(i)=gtilist(i,1)
         gti_stop(i)=gtilist(i,2)
      enddo
      CALL FTPCLD(ounit,colnum,frow,felem,nelements,gti_start,status)
      colnum=2
      CALL FTPCLD(ounit,colnum,frow,felem,nelements,gti_stop,status)

C.......................................................
C Go to the EVENTS extension
C.......................................................
      nhdu=3
      CALL FTCRHD(ounit,status)
      CALL FTMAHD(iunit,nhdu,hdutype,status)
      CALL FTCOPY(iunit,ounit,morekeys,status)
      tfields=4
      ttype(1)='TIME'
      ttype(2)='X'
      ttype(3)='Y'
      ttype(4)='PHA'
      tform(1)='D'
      tform(2)='I'
      tform(3)='I'
      tform(4)='I'
      tunit(1)='s'
      tunit(2)='pixel'
      tunit(3)='pixel'
      tunit(4)='chan'
      extname='EVENTS'
      nrows=nevents
      varidat=0
c      CALL FTPHBN(ounit,nrows,tfields,ttype,tform,tunit,extname,varidat,
c     &     status)
      
      

C Modify keywords
      comment="&"
      keyword="NAXIS2"
      CALL FTMKYJ(ounit,keyword,nevents,comment,status)
C TCRVL2, TCRVL3
      if (star.eq."STR1") then
         decimals=8
         comment="&"
         keyword="TCRVL2"
         CALL FTMKYD(ounit,keyword,ra_nom,decimals,comment,status)
         comment="&"
         keyword="TCRVL3"
         CALL FTMKYD(ounit,keyword,dec_nom,decimals,comment,status)
      endif
C TSTART, TSTOP
      comment="&"
      keyword="TSTART"
      CALL FTMKYJ(ounit,keyword,tstart,comment,status)
      comment="&"
      keyword="TSTOP"
      CALL FTMKYJ(ounit,keyword,tstop,comment,status)
C RA_NOM, DEC_NOM
      if (star.eq."STR1") then
         decimals=8
         comment="&"
         keyword="RA_NOM"
         CALL FTMKYD(ounit,keyword,ra_nom,decimals,comment,status)
         comment="&"
         keyword="DEC_NOM"
         CALL FTMKYD(ounit,keyword,dec_nom,decimals,comment,status)
      endif
C TELAPSE
      comment="&"
      keyword="TELAPSE"
      CALL FTMKYJ(ounit,keyword,telapse,comment,status)
C ONTIME
      comment="&"
      keyword="ONTIME"
      CALL FTMKYJ(ounit,keyword,ontime,comment,status)
C DEADC
      comment="&"
      keyword="DEADC"
      CALL FTMKYF(ounit,keyword,deadc,3,comment,status)
C DATE-OBS,TIME-OBS,DATE-END,TIME-END
      comment="&"
      keyword="DATE-OBS"
      CALL FTMKYS(ounit,keyword,stimekeys(1),comment,status)
      keyword="TIME-OBS"
      CALL FTMKYS(ounit,keyword,stimekeys(2),comment,status)
      keyword="DATE-END"
      CALL FTMKYS(ounit,keyword,stimekeys(3),comment,status)
      keyword="TIME-END"
      CALL FTMKYS(ounit,keyword,stimekeys(4),comment,status)
C TCROT3 and TROTA3
      if (star.eq."STR1") then
         decimals=8
         comment="&"
         keyword="TROTA3"
         CALL FTMKYD(ounit,keyword,roll90,decimals,comment,status)
         comment="&"
         keyword="TCROT3"
         CALL FTMKYD(ounit,keyword,roll90,decimals,comment,status)
      endif
C Remove CHECKSUM, DATASUM
      keyword="CHECKSUM"
      CALL FTDKEY(ounit,keyword,status)
      keyword="DATASUM"
      CALL FTDKEY(ounit,keyword,status)
      if(status.ne.0) then
         status=0
      endif
c         
C Determine the optimum number of rows I should write at once       
      CALL FTGRSZ(ounit,nrows,status)
      even_steps=mod(nevents,nrows)
      if(nrows.gt.nevents) then
         nrows=nevents
         steps=1
      elseif(nrows.eq.nevents) then
         steps=1
      else
         if(even_steps.eq.0) then
            steps=(nevents/nrows)
         else
            steps=(nevents/nrows)+1
         endif
      endif
      srow=1
      write(string,10) nevents,nrows
 10   format("   Writing EVENTS. Number of rows ",I9,
     &     " in steps of",I9)
      CALL RMVXBK(string)
      CALL xwrite(string,10)
      do i=1,steps
         if((srow+nrows).gt.nevents) then
            nrows=nevents-(srow-1)
         endif
         colnum=1
         frow=srow
         felem=1
         nelements=nrows
C SHFTIME
         CALL FTPCLD(ounit,colnum,frow,felem,nelements,shftime(k),
     &        status)
C X
         colnum=2
         CALL FTPCLJ(ounit,colnum,frow,felem,nelements,x(k),
     &        status)
C Y
         colnum=3
         CALL FTPCLJ(ounit,colnum,frow,felem,nelements,y(k),
     &        status)
C PHA
         colnum=4
         CALL FTPCLJ(ounit,colnum,frow,felem,nelements,pha(k),
     &        status)
         if(status.ne.0) then 
            if(status.ne.107) then
               context="ERROR: Writing EVENTS table."
               goto 999
            else
               status=0
            endif
         endif
         srow=i*nrows+1
         k=j+i*nrows+1
      enddo
      j=j+nevents
      k=j+1

C.......................................................
C Go to the EXPOSURE extension
C.......................................................
      nhdu=4
      CALL FTCRHD(ounit,status)
      CALL FTMAHD(iunit,nhdu,hdutype,status)
      CALL FTCOPY(iunit,ounit,morekeys,status)
      tfields=3
      ttype(1)='TIME'
      ttype(2)='FRACEXP'
      ttype(3)='DEADTEL'
      tform(1)='D'
      tform(2)='E'
      tform(3)='E'
      tunit(1)='s'
      tunit(2)=' '
      tunit(3)=' '
      extname='EXPOSURE'
      nrows=nexp
      varidat=0
c      CALL FTPHBN(ounit,nrows,tfields,ttype,tform,tunit,extname,varidat,
c    &     status)
c
C Modify keywords
C NAXIS2 of EXPOSURE extension
      comment="&"
      keyword="NAXIS2"
      CALL FTMKYJ(ounit,keyword,nexp,comment,status)
C TSTART, TSTOP
      comment="&"
      keyword="TSTART"
      CALL FTMKYJ(ounit,keyword,tstart,comment,status)
      comment="&"
      keyword="TSTOP"
      CALL FTMKYJ(ounit,keyword,tstop,comment,status)
C RA_NOM, DEC_NOM
      if (star.eq."STR1") then
         decimals=8
         comment="&"
         keyword="RA_NOM"
         CALL FTMKYD(ounit,keyword,ra_nom,decimals,comment,status)
         comment="&"
         keyword="DEC_NOM"
         CALL FTMKYD(ounit,keyword,dec_nom,decimals,comment,status)
      endif
C ONTIME
      comment="&"
      keyword="ONTIME"
      CALL FTMKYJ(ounit,keyword,ontime,comment,status)
C DATE-OBS,TIME-OBS,DATE-END,TIME-END
      comment="&"
      keyword="DATE-OBS"
      CALL FTMKYS(ounit,keyword,stimekeys(1),comment,status)
      keyword="TIME-OBS"
      CALL FTMKYS(ounit,keyword,stimekeys(2),comment,status)
      keyword="DATE-END"
      CALL FTMKYS(ounit,keyword,stimekeys(3),comment,status)
      keyword="TIME-END"
      CALL FTMKYS(ounit,keyword,stimekeys(4),comment,status)
C TELAPSE
      comment="&"
      keyword="TELAPSE"
      CALL FTMKYJ(ounit,keyword,telapse,comment,status)
C Remove CHECKSUM, DATASUM
      keyword="CHECKSUM"
      CALL FTDKEY(ounit,keyword,status)
      keyword="DATASUM"
      CALL FTDKEY(ounit,keyword,status)
      if(status.ne.0) then
         status=0
      endif
C Determine the optimum number of rows I should write at once       
      
      CALL FTGRSZ(ounit,nrows,status)
      even_steps=mod(nexp,nrows)
      if(nrows.gt.nexp) then
         nrows=nexp
         steps=1
      elseif(nrows.eq.nexp) then
         steps=1
      else
         if(even_steps.eq.0) then
            steps=(nexp/nrows)
         else
            steps=(nexp/nrows)+1
         endif
      endif
      srow=1
      write(string,30) nexp,nrows
 30   format("   Writing EXPOSURE. Number of rows ",I9,
     &     " in steps of",I9)
      CALL RMVXBK(string)
      CALL xwrite(string,10)
      do i=1,steps
         if( (srow+nrows).gt.nexp) then
            nrows=nexp-(srow-1)
         endif
         colnum=1
         frow=srow
         felem=1
         nelements=nrows
C EXPTIME
         CALL FTPCLD(ounit,colnum,frow,felem,nelements,exptime(kk),
     &        status)
C FRAC
         colnum=2
         CALL FTPCLE(ounit,colnum,frow,felem,nelements,frac(kk),
     &        status)
C DEAD
         colnum=3
         CALL FTPCLE(ounit,colnum,frow,felem,nelements,dead(kk),
     &        status)
         if(status.ne.0) then 
            if(status.ne.107) then
               context="ERROR: Writing EXPOSURE table."
               goto 999
            else
               status=0
            endif
         endif
         srow=i*nrows+1
         kk=jj+i*nrows+1
      enddo
      jj=jj+nexp
      kk=jj+1
      
C.......................................................
C Go to the OBC_PACKET extension
C.......................................................
      nhdu=5
      CALL FTCRHD(ounit,status)
      CALL FTMAHD(iunit,nhdu,hdutype,status)
      CALL FTCOPY(iunit,ounit,morekeys,status)
      tfields=6
      ttype(1)='TIME'
      ttype(2)='SHFTIME'
      ttype(3)='PRTIME'
      ttype(4)='BCTIME'
      ttype(5)='BAD'
      ttype(6)='SAMPLES'
      tform(1)='D'
      tform(2)='J'
      tform(3)='D'
      tform(4)='E'
      tform(5)='I'
      tform(6)='I'
      tunit(1)='s'
      tunit(2)='s'
      tunit(3)='s'
      tunit(4)='s'
      tunit(5)=' '
      tunit(6)=' '
      extname='OBC_PACKET'
      nrows=nobc
      varidat=0
c      CALL FTPHBN(ounit,nrows,tfields,ttype,tform,tunit,extname,varidat,
c     &     status)
      
C Modify keywords
C NAXIS2 of OBC_PACKET extension
      comment="&"
      keyword="NAXIS2"
      CALL FTMKYJ(ounit,keyword,nobc,comment,status)
C DATE-OBS,TIME-OBS,DATE-END,TIME-END
      comment="&"
      keyword="DATE-OBS"
      CALL FTMKYS(ounit,keyword,stimekeys(1),comment,status)
      keyword="TIME-OBS"
      CALL FTMKYS(ounit,keyword,stimekeys(2),comment,status)
      keyword="DATE-END"
      CALL FTMKYS(ounit,keyword,stimekeys(3),comment,status)
      keyword="TIME-END"
      CALL FTMKYS(ounit,keyword,stimekeys(4),comment,status)
C TSTART, TSTOP
      comment="&"
      keyword="TSTART"
      CALL FTMKYJ(ounit,keyword,tstart,comment,status)
      comment="&"
      keyword="TSTOP"
      CALL FTMKYJ(ounit,keyword,tstop,comment,status)
C MINWAIT
      comment="&"
      keyword="MINWAIT"
      CALL FTMKYE(ounit,keyword,minwait,4,comment,status)
C HKKEYS - the number of HKSTRT keywords present = number of GTIs
      comment="Number of HKSTRT keywords"
      keyword="HKKEYS"
      CALL FTPKYJ(ounit,keyword,ngti,comment,status)
C HKSTRT(1-ngti) - keywords containing the HKSTART of each event file
      comment="Time to 1st HK in SCC&"
      keyword="HKSTRT"
      CALL FTPKNJ(ounit,keyword,1,ngti,hkstart,comment,status)
C Remove HKSTART
      keyword="HKSTART"
      CALL FTDKEY(ounit,keyword,status)
C Remove CHECKSUM, DATASUM
      keyword="CHECKSUM"
      CALL FTDKEY(ounit,keyword,status)
      keyword="DATASUM"
      CALL FTDKEY(ounit,keyword,status)
      if(status.ne.0) then
         status=0
      endif
c      
C Determine the optimum number of rows I should write at once       
c      
      CALL FTGRSZ(ounit,nrows,status)
      even_steps=mod(nobc,nrows)
      if(nrows.gt.nobc) then
         nrows=nobc
         steps=1
      elseif(nrows.eq.nobc) then
         steps=1
      else
         if(even_steps.eq.0) then
            steps=(nobc/nrows)
         else
            steps=(nobc/nrows)+1
         endif
      endif
      srow=1
      write(string,40) nobc,nrows
 40   format("   Writing OBC_PACKET. Number of rows ",I9,
     &     " in steps of",I9)
      CALL RMVXBK(string)
      CALL xwrite(string,10)
      do i=1,steps
         if((srow+nrows).gt.nobc) then
            nrows=nobc-(srow-1)
         endif
         colnum=1
         frow=srow
         felem=1
         nelements=nrows
C OBCTIME
         CALL FTPCLD(ounit,colnum,frow,felem,nelements,obctime(kkk),
     &        status)
C OBCSHFTIME
         colnum=2
         CALL FTPCLJ(ounit,colnum,frow,felem,nelements,obcshftime(kkk),
     &        status)
C PRTIME
         colnum=3
         CALL FTPCLD(ounit,colnum,frow,felem,nelements,prtime(kkk),
     &        status)
C BCTIME
         colnum=4
         CALL FTPCLE(ounit,colnum,frow,felem,nelements,bctime(kkk),
     &        status)
C BAD
         colnum=5
         CALL FTPCLI(ounit,colnum,frow,felem,nelements,bad(kkk),
     &        status)
C SAMPLES
         colnum=6
         CALL FTPCLI(ounit,colnum,frow,felem,nelements,samples(kkk),
     &        status)
         if(status.ne.0) then 
            if(status.ne.107) then
               context="ERROR: Writing OBC_PACKET table."
               goto 999
            else
               status=0
            endif
         endif
         srow=i*nrows+1
         kkk=jjj+i*nrows+1
      enddo
      jjj=jjj+nobc
      kkk=jjj+1
      
      CALL ftclos(ounit,status)
      CALL ftfiou(ounit,status)
      CALL ftclos(iunit,status)
      CALL ftfiou(iunit,status)
      
      if (status.eq.0) goto 1000
 900  continue
      CALL ftclos(ounit,status)
      CALL ftfiou(ounit,status)
      CALL ftclos(iunit,status)
      CALL ftfiou(iunit,status)

      if (status.eq.0) goto 1000
 990  context = 'Could not open output file' // outfile
 999  continue
      CALL ftclos(ounit,status)
      CALL ftfiou(ounit,status)
      CALL ftclos(iunit,status)
      CALL ftfiou(iunit,status)
      write(stat,*) status
      CALL RMVXBK(stat)
      errm=subname//' '//context//', '//errtxt//' '//stat
      CALL RMVXBK(errm)
      CALL xaerror(errm,5)
 1000 continue
      return
      end
 
