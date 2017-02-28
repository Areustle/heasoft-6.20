      subroutine EXTABLE(filename,name,naxis2,ext,status)

C EXOSAT Table Read
C for given unit number and table name returns NAXIS2 and extension number
C
C  I filename    Name of FITS file
C  I name        name of table EXTNAME
C  O naxis2      Number of rows in the table
C  O ext         Table extension number
      
      IMPLICIT NONE

C Input/Output variables
      CHARACTER name*255,filename*160
      INTEGER*4 naxis2,ext,status

C Local variables
      INTEGER*4 unit,hdutype,blocksize,readwrite
      INTEGER*4 lenact
      CHARACTER errm*255,context*160,tname*80
      CHARACTER errtxt*30, stat*15
      CHARACTER subname*50,comment*80

C hdutype = 1 -- ASCII table
C hdutype = 2 -- Binary table

C ext = 1 for the primary HDU

      if(status.ne.0) return

      subname='extable:'
      errm=' '
      stat=' '
      errtxt=' '
      CALL RMVBLK(name)
      readwrite=0
      ext=1

      CALL FTGIOU(unit,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context= "Can't get unit number"
         goto 999
      endif
      filename=filename(1:lenact(filename))
      CALL FTOPEN(unit,filename,readwrite,blocksize,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context= "Can't find "//filename
         goto 999
      endif

 10   continue
      ext=ext+1

      CALL FTMAHD(unit,ext,hdutype,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context='Could not move to extension'
         goto 999
      endif
      CALL FTGKYS(unit,'EXTNAME',tname,comment,status)


      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context='Could not get extension name keyword'
         goto 999
      endif
      CALL RMVBLK(tname)

C Checks the extension name against the input name to make sure I'm 
C reading the right one
      if(name.eq.tname) then
         CALL FTGKYJ(unit,'NAXIS2',naxis2,comment,status)
         if(status.ne.0) then
            CALL FTGERR(status,errtxt)
            context='Could not get NAXIS2 value'
            goto 999
         endif
         goto 900
      else
         goto 10
      endif
 900  continue
      CALL ftclos(unit,status)
      CALL ftfiou(unit,status)
 
      if (status.eq.0) goto 1000
 999  continue
      CALL ftclos(unit,status)
      CALL ftfiou(unit,status)
      write(stat,*) status
      CALL RMVXBK(stat)
      errm=subname//' '//context//', '//errtxt//' '//stat
      CALL RMVXBK(errm)
      CALL xaerror(errm,5)
 1000 continue
      RETURN
      END
