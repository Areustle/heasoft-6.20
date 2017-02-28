      subroutine writearray(infile,outfile,npoint,inttime,
     &     time,rate,error,fracexp,minexpo,clobber,status)

C Writes the output LC FITS file with corrected fractional exposure
C Only writes those rows of LC that span the event file
C
C I   infile     Input FITS file
C O   outfile    Output FITS file
C I   npoint     Number of rows in lightcurve
C I   inttime    Integration time
C I   time       time bin
C I   rate       Count rate
C I   error      Count rate error
C I   fracexp    Fractional exposure
C I   minexpo    Minimum exposure
C I   clobber    Overwrite output file if it exists?

      implicit none
      character(100) astring
      INTEGER*4 readwrite,status,ounit,iunit,blocksize,i
      INTEGER*4 npoint,colnum,morekeys,frow,felem,nelems,row1
      INTEGER*4 endloop,num,dlu
      REAL*8 inttime, time(npoint),fracexp(npoint),minexpo,bincount
      REAL*4 rate(npoint), error(npoint)
      CHARACTER errm*255,subname*50,context*160,errtxt*30,comment*80
      CHARACTER stat*15
      CHARACTER*(*) outfile, infile
      LOGICAL clobber,exists
C========================================================
C Copy the input Light Curve FITS file - infile
C to output FITS Light Curve file - outfile
C========================================================
      if(status.ne.0) return

      subname='writearray:'
      readwrite=0
      row1=0
      status=0
      iunit=0
      ounit=0
      morekeys=0
      endloop=0
      errtxt=' '
c      string=' '
C
C Open original lightcurve file
C


      CALL FTGIOU(iunit,status)
      CALL FTOPEN(iunit,infile,readwrite,blocksize,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't open "//infile//" for copying"
         goto 999
      endif

C
C Check if output file exists
C If it does and clobber is set then delete it

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

C USING FITSIO
c      CALL FTGIOU(ounit,status)
c      CALL FTOPEN(ounit,outfile,readwrite,blocksize,status)
c      if(status.eq.0) then
C       BAD File already exists. Delete it.
c         if(clobber) then 
c            CALL FTDELT(ounit,status)
c            if(status.ne.0) then
c               CALL FTGERR(status,errtxt)
c               CALL FTFIOU(ounit,status)
c               context="Can't delete "//outfile//" Already exists"
c               goto 999
c            endif
c            CALL FTFIOU(ounit,status)
c         else
c            CALL FTGERR(status,errtxt)
c            CALL FTFIOU(ounit,status)
c            context="File "//outfile//" already exists"
c            goto 999
c         endif
c      elseif(status.eq.104) then
C GOOD File does not exist. 
c         status=0
c         CALL ftclos(ounit,status)
c         CALL FTFIOU(ounit,status)
c      else
C ? Some other problem occurred
c         CALL FTGERR(status,errtxt)
c         CALL FTFIOU(ounit,status)
c         context="File "//outfile//" couldn't be opened"
c         goto 999
c      endif


C****************
C At this point the output file should not already exist
C****************

C 
C Open the output lightcurve file
C
      CALL FTGIOU(ounit,status)
      CALL FTINIT(ounit,outfile,blocksize,status)
      if(status.ne.0) then
         context="Problem initializing "//outfile
         goto 999
      endif
c      CALL ffinit(ounit,outfile,status)
C
C Check if the file already exists
C

C
C        Output file doesn't exist. Copy Primary Header
C
      CALL FTCOPY(iunit,ounit,morekeys,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't copy "//infile//" to "//outfile
         goto 999
      endif
C        
C       Copy the RATE extension
C

         CALL FTCRHD(ounit,status)
         CALL FTMNHD(iunit,2,'RATE',0,status)
c         CALL FTCOPY(iunit,ounit,morekeys,status)
C Copy only header
C
         call ftcphd(iunit,ounit,status)
         if(status.ne.0) then
            CALL FTGERR(status,errtxt)
            context="Can't move to RATE extension"
            goto 999
         endif
C     Copy infile to outfile

         if(row1.le.0) then
            row1=1
            endloop=npoint
         else
            endloop=(row1+npoint-1)
         endif
         
         num=0
         bincount=0

         do i=row1,endloop
            colnum=1
            frow=1
            felem=1
            nelems=1
c            write(6,*)
c     &           "loop index,time,rate,error,fracexp",
c     &           i,time(i),rate(i),error(i),fracexp(i)
            if(fracexp(i).ge.minexpo) then
               num=num+1
               colnum=1
               CALL FTPCLD(ounit,colnum,num,felem,nelems,
     &              time(i),status)
               if(status.ne.0) then
                  CALL FTGERR(status,errtxt)
                  context= "Can't write TIME columns"
                  goto 999
               endif
               colnum=2
               CALL FTPCLE(ounit,colnum,num,felem,nelems,
     &              rate(i),status)
               if(status.ne.0) then
                  CALL FTGERR(status,errtxt)
                  context= "Can't write RATE columns"
                  goto 999
               endif
               
C The error is just SQRT(rate*bin)/bin i.e. SQRT(counts)/bin
               colnum=3
               CALL FTPCLE(ounit,colnum,num,felem,nelems,
     &              error(i),status)
               if(status.ne.0) then
                  CALL FTGERR(status,errtxt)
                  context="Can't write ERROR columns"
                  goto 999
               endif
               colnum=4
               CALL FTPCLD(ounit,colnum,num,felem,nelems,
     &              fracexp(i),status)
               if(status.ne.0) then
                  CALL FTGERR(status,errtxt)
                  context="Can't write FRACEXP columns"
                  goto 999
               endif
            endif
            bincount=bincount+inttime
         enddo
         comment='&'
         CALL FTMKYJ(ounit,'NAXIS2',num,comment,status)
         write(astring,20) num
 20      format("Writing ",I4," rows ")
         CALL xwrite(astring,10)
C        CALL FTPKYE(ounit,'EXPMIN',expmin,4,comment,status)
C
C       Copy GTI extension
C
         CALL FTCRHD(ounit,status)
         CALL FTMNHD(iunit,2,'GTI',0,status)
         CALL FTCOPY(iunit,ounit,morekeys,status)
         if(status.ne.0) then
            CALL FTGERR(status,errtxt)
            context="Can't move to GTI extension"
            goto 999
         endif
         CALL ftclos(ounit,status)
         CALL ftfiou(ounit,status)
         CALL ftclos(iunit,status)
         CALL ftfiou(iunit,status)
         if(status.eq.0) goto 1000
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
      end

