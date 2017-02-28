c
      SUBROUTINE getfiles(evtfile, cfile, nfiles, status)
c
c Get names of input files.  If filename begins with '@', the file is a
c list of files to use for that series. 
c
c  I  evtfile     list of event files
c  O  cfile       character array containing event filenames
c  O  nfiles      number of event files
c  I/O status
c 
c Input/output variables 
      implicit none 
      INTEGER*4 nfiles,nfilemax,status
      PARAMETER (nfilemax=100) 
      CHARACTER*(*) evtfile
      character(160) cfile(nfilemax)  
c
c Local variable
      INTEGER*4 luin,lenact,i, ierr
      character(160) subname, fulevtfile
      character(255)  errm ,context
      PARAMETER (subname = 'getfiles:')
c
      if(status.ne.0) return
      ierr=0
c
c get file
      if(evtfile(1:1).eq.'@') then
         call getlun(luin)
         fulevtfile= evtfile(2:lenact(evtfile))
         call openwr(luin,fulevtfile,'OLD',' ',
     &         ' ',0,1, status)
c         call openwr(luin,evtfile(2:lenact(evtfile)),'OLD',' ',' ',0,1,
c     &         status)

         if(status.ne.0) then
            context='Couldn''t open file '//evtfile
            GOTO 999
         endif
c
c     loop over all files in a given series

         i=1
         do while (ierr.eq.0)
            cfile(i) = ' '
            read (luin,'(a)', iostat=ierr)cfile(i)
            i=i+1
            if(ierr.gt.0) then
               context="Input error? in "//evtfile
               GOTO 999
            endif
            IF(i.ge.nfilemax)THEN
               write(context,'(''Too many input files '')')
               call xwrite(context,5)
               write(context, '(''Use maximun '', I3)') nfilemax
               call xwrite(context,5)
               ierr=1    
            ENDIF 
         enddo
         close(luin)
         call frelun(luin) 
         nfiles=i-2
      else 
         cfile(1)=evtfile
         nfiles=1
      endif
c
c
      status=0
      return  
 999  continue 
      errm=subname//' '//context
      CALL RMVXBK(errm)
      call xaerror(errm, 5)
      return
      end
