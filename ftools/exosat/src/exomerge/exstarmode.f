
      subroutine exstarmode(cfile,nfiles,star,tlist,nevents,nexp,nobc,
     &     on,dead,minwait,clobber,outfile,status)

C This routine will read the keywords STAR_PNT, FILTER, OBCMODE, BKGFILTR,
c INSTRUME, and make sure they are the same for all files. BKDFILTR will
C be allowed to vary but it will be checked and a warning given if it differs
C
C I   cfile    List of event files
C I   nfiles   Number of event files
C O   star     Character string ("STR1" / "STR2")
C O   tlist    List of start and stop times for the event files
C O   nevents  Sum of NAXIS2 values for EVENTS extension
C O   nexp     Sum of NAXIS2 values for EXPOSURE extension
C O   nobc     Sum of NAXIS2 values for OBC_PACKET extension
C O   on       Sum of ONTIME values
C O   dead     Weighted mean DEADC value. Weighted by ONTIME
C O   minwait  The minimum MINWAIT value for the event files
C O   clobber  Overwrite if file exists?
C
      implicit none

      character*(*) outfile
      INTEGER*4 i,nhdu,status,iounit,hdutype,readwrite,blocksize,
     &     nfiles,tlist(256,2),nevents,naxis2,nexp,nobc,ontime(256)
      integer*4 on,dlu
      REAL deadc(256),dead,minwait,wait,deadon
      character(160) cfile(256),filename
      character(8) keyword
      character(80) star,old_star,extname
      character(80) instrume,old_instrume,filter,old_filter,obcmode,
     &     old_obcmode,grating,old_grating,bkgfiltr,old_bkgfiltr
      character subname*50,context*160,comment*80,errm*255
      CHARACTER errtxt*30,stat*15
      LOGICAL FIRST,clobber,exists

c  Initialize to avoid warning
      on = 0
      deadon = 0.
c  --

      subname="exstarmode: "
      errtxt=" "
      nhdu=0
      FIRST=.true.
      minwait=9999.0
      iounit=0
      readwrite=0

      old_star=star
      old_bkgfiltr=bkgfiltr
      old_grating=grating
      old_obcmode=obcmode
      old_filter=filter
      old_instrume=instrume


C Check if output file exists and if it does whether the user wants
C it clobbered or not. If not get out.
      inquire(file=outfile,exist=exists)
      if(exists) then
         if(clobber) then
            call getlun(dlu)
            open(dlu,file=outfile,status='old',err=990)
            close(dlu,status='delete',err=990)
            call frelun(dlu)
         else
            status=-1
            context=outfile
            errtxt="file already exists"
            CALL RMVXBK(context)
            goto 999
         endif
      endif

C First make sure that all of the following keywords have the same values
C in each event file

      nevents=0
      nexp=0
      nobc=0
      do i=1,nfiles
         filename=cfile(i)
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
C Find the keywords in the EVENTS extension
         nhdu=3
         CALL FTMAHD(iounit,nhdu,hdutype,status)

C Just in case. Get the extension name EXTNAME and check if it is EVENTS
         keyword="EXTNAME"
         CALL FTGKYS(iounit,keyword,extname,comment,status)
         if(status.ne.0) then 
            context="ERROR: EXTNAME keyword not found."
            goto 999
         endif
         CALL RMVBLK(extname)
         if(extname.ne."EVENTS") then
            context="ERROR:"
            errtxt=" not EVENTS extension"
            errm=subname//' '//context//', '//errtxt            
            CALL RMVXBK(errm)
            CALL XAERROR(errm,5)
            status=10
            goto 999 
         endif

C NAXIS2 of EVENTS extension
         keyword="NAXIS2"
         CALL FTGKYJ(iounit,keyword,naxis2,comment,status)
         if(status.ne.0) then 
            context="ERROR: NAXIS2 keyword not found."
            goto 999
         endif
         nevents=nevents+naxis2

C TSTART and TSTOP
         keyword="TSTART"
         CALL FTGKYJ(iounit,keyword,tlist(i,1),comment,status)
         if(status.ne.0) then 
            context="ERROR: TSTART keyword not found."
            goto 999
         endif
         keyword="TSTOP"
         CALL FTGKYJ(iounit,keyword,tlist(i,2),comment,status)
         if(status.ne.0) then 
            context="ERROR: TSTOP keyword not found."
            goto 999
         endif

C INSTRUME
         keyword="INSTRUME"
         CALL FTGKYS(iounit,keyword,instrume,comment,status)
         if(status.ne.0) then 
            context="ERROR: INSTRUME keyword not found."
            goto 999
         endif
         CALL RMVBLK(instrume)

C FILTER
         keyword="FILTER"
         CALL FTGKYS(iounit,keyword,filter,comment,status)
         if(status.ne.0) then 
            context="ERROR: FILTER keyword not found."
            goto 999
         endif
         CALL RMVBLK(filter)
C ONTIME
         keyword="ONTIME"
         CALL FTGKYJ(iounit,keyword,ontime(i),comment,status)
         if(status.ne.0) then 
            context="ERROR: ONTIME keyword not found."
            goto 999
         endif
C DEADC
         keyword="DEADC"
         CALL FTGKYE(iounit,keyword,deadc(i),comment,status)
         if(status.ne.0) then 
            context="ERROR: ONTIME keyword not found."
            goto 999
         endif
C OBCMODE
         keyword="OBCMODE"
         CALL FTGKYS(iounit,keyword,obcmode,comment,status)
         if(status.ne.0) then 
            context="ERROR: OBCMODE keyword not found."
            goto 999
         endif
         CALL RMVBLK(obcmode)
         
C GRATING
         keyword="GRATING"
         CALL FTGKYS(iounit,keyword,grating,comment,status)
         if(status.ne.0) then 
            context="ERROR: GRATING keyword not found."
            goto 999
         endif
         CALL RMVBLK(grating)

C BKGFILTR
         keyword="BKGFILTR"
         CALL FTGKYS(iounit,keyword,bkgfiltr,comment,status)
         if(status.ne.0) then 
            context="ERROR: BKGFILTR keyword not found."
            goto 999
         endif
         CALL RMVBLK(bkgfiltr)

C STAR_PNT
         keyword="STAR_PNT"
         CALL FTGKYS(iounit,keyword,star,comment,status)
         if(status.ne.0) then 
            context="ERROR: STAR_PNT keyword not found."
            goto 999
         endif
         CALL RMVBLK(star)
C
C Find the NAXIS2 keyword in the EXPOSURE extension
C
         nhdu=4
         CALL FTMAHD(iounit,nhdu,hdutype,status)
C Just in case. Get the extension name EXTNAME and check if it is EVENTS
         keyword="EXTNAME"
         CALL FTGKYS(iounit,keyword,extname,comment,status)
         if(status.ne.0) then 
            context="ERROR: EXTNAME keyword not found."
            goto 999
         endif
         CALL RMVBLK(extname)
         if(extname.ne."EXPOSURE") then
            context="ERROR:"
            errtxt=" not EXPOSURE extension"
            errm=subname//' '//context//', '//errtxt            
            CALL RMVXBK(errm)
            CALL XAERROR(errm,5)
            status=10
            goto 999
         endif
C NAXIS2 of EXPOSURE extension
         keyword="NAXIS2"
         CALL FTGKYJ(iounit,keyword,naxis2,comment,status)
         if(status.ne.0) then 
            context="ERROR: NAXIS2 keyword not found."
            goto 999
         endif
         nexp=nexp+naxis2

C
C Find the NAXIS2 keyword in the OBC_PACKET extension
C
         nhdu=5
         CALL FTMAHD(iounit,nhdu,hdutype,status)
C Just in case. Get the extension name EXTNAME and check if it is EVENTS
         keyword="EXTNAME"
         CALL FTGKYS(iounit,keyword,extname,comment,status)
         if(status.ne.0) then 
            context="ERROR: EXTNAME keyword not found."
            goto 999
         endif
         CALL RMVBLK(extname)
         if(extname.ne."OBC_PACKET") then
            context="ERROR:"
            errtxt=" not OBC_PACKET extension"
            errm=subname//' '//context//', '//errtxt            
            CALL RMVXBK(errm)
            CALL XAERROR(errm,5)
            status=10
            goto 999
         endif
C NAXIS2 of OBC_PACKET extension
         keyword="NAXIS2"
         CALL FTGKYJ(iounit,keyword,naxis2,comment,status)
         if(status.ne.0) then 
            context="ERROR: NAXIS2 keyword not found."
            goto 999
         endif
         nobc=nobc+naxis2
C MINWAIT
         keyword="MINWAIT"
         CALL FTGKYE(iounit,keyword,wait,comment,status)
         if(status.ne.0) then 
            context="ERROR: NAXIS2 keyword not found."
            goto 999
         endif
         if(wait.lt.minwait) then
            minwait=wait
         endif
 900     continue
         CALL ftclos(iounit,status)
         CALL ftfiou(iounit,status)
         if(FIRST) then
            old_star=star
            old_bkgfiltr=bkgfiltr
            old_grating=grating
            old_obcmode=obcmode
            old_filter=filter
            old_instrume=instrume
         else
            if(bkgfiltr.ne.old_bkgfiltr) then
               context=" Background Filters differ"
               errtxt="Merging will continue"
               errm=subname//' '//context//', '//errtxt      
               CALL RMVXBK(errm)
               CALL XWARN(errm,5)
            endif
            context="ERROR: Files can not be merged"
            if(instrume.ne.old_instrume) then
               errtxt=" Instruments differ."
               errm=subname//' '//context//', '//errtxt
               CALL RMVXBK(errm)
               CALL XAERROR(errm,5)
               status=10
               goto 999
            elseif(filter.ne.old_filter) then
               errtxt=" Filters differ."
               errm=subname//' '//context//', '//errtxt 
               CALL RMVXBK(errm)
               CALL XAERROR(errm,5)
               status=10
               goto 999
            elseif(obcmode.ne.old_obcmode) then
               errtxt=" OBC modes differ."
               errm=subname//' '//context//', '//errtxt
               CALL RMVXBK(errm)
               CALL XAERROR(errm,5)
               status=10
               goto 999
            elseif(grating.ne.old_grating) then
               errtxt=" Gratings differ."
               errm=subname//' '//context//', '//errtxt            
               CALL RMVXBK(errm)
               CALL XAERROR(errm,5)
               status=10
               goto 999
            elseif(star.ne.old_star) then
               errtxt=" ST modes differ."
               errm=subname//' '//context//', '//errtxt   
               CALL RMVXBK(errm)
               CALL XAERROR(errm,5)
               status=10
               goto 999
            endif
         endif
         FIRST=.false.
      enddo
C Compute the Weighted Mean DEADC and the ONTIME
      do i=1,nfiles
         on=on+ontime(i)
         deadon=deadon+deadc(i)*ontime(i)
      enddo
      dead=deadon/float(on)

      if (status.eq.0) goto 1000
 990  context = 'Could not open output file' // outfile
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
 
