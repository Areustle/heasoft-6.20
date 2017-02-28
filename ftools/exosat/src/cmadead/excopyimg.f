      subroutine excopyimg(infile,outfile,clobber,status)

C I   infile     Input FITS file
C I   outfile    Output FITS file
C I   clobber    Overwrite output file if it exists?

      implicit none

      INTEGER*4 readwrite,status,nhdu,ounit,iunit,blocksize
      INTEGER*4 hdutype,morekeys,lenact
      CHARACTER errm*255,subname*50,context*160,errtxt*30
      CHARACTER stat*15
      CHARACTER*(*) outfile, infile
      LOGICAL clobber
C========================================================
C Copy the input image FITS file - infile
C to output FITS image file - outfile
C========================================================

      if(status.ne.0) return

      subname='excopyimg:'
      readwrite=0
      status=0
      errtxt=' '
C
C Open original image file
C
      CALL FTGIOU(iunit,status)
      CALL FTOPEN(iunit,infile,readwrite,blocksize,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't open "//infile//" for copying"
         goto 999
      endif
C 
C Open the output image file
C
      CALL FTGIOU(ounit,status)
      CALL FTINIT(ounit,outfile,blocksize,status)
C
C Check if the file already exists
C
      if(status.eq.0) then
C
C        Output file doesn't exist
C
         CALL FTCOPY(iunit,ounit,morekeys,status)
         if(status.ne.0) then
            CALL FTGERR(status,errtxt)
            context="Can't copy "//infile//" to "//outfile
            goto 999
         endif
C        
C       Copy the GTI extension
C
         nhdu=2
         CALL FTCRHD(ounit,status)
         CALL FTMAHD(iunit,nhdu,hdutype,status)
         CALL FTCOPY(iunit,ounit,morekeys,status)
         if(status.ne.0) then
            CALL FTGERR(status,errtxt)
            context="Can't move to GTI extension"
            goto 999
         endif

      else
C
C    File already exists.
C
         if((.not.clobber)) then
            CALL FTGERR(status,errtxt)
            context=outfile(1:lenact(outfile))//" already exists"
            goto 999
         else
            CALL ftclos(ounit,status)
            CALL ftclos(iunit,status)
            CALL ftfiou(ounit,status)
            CALL ftfiou(iunit,status)
            status=0
            goto 1000
         endif
      endif
 20   continue
      CALL ftclos(ounit,status)
      CALL ftclos(iunit,status)
      CALL ftfiou(ounit,status)
      CALL ftfiou(iunit,status)
      if(status.eq.0) goto 1000
 999  continue
      CALL ftclos(ounit,status)
      CALL ftclos(iunit,status)
      CALL ftfiou(ounit,status)
      CALL ftfiou(iunit,status)
      write(stat,*) status
      errm=subname//' '//context//', '//errtxt//' '//stat
      CALL RMVXBK(errm)
      CALL xaerror(errm,5)
 1000 continue
      end

