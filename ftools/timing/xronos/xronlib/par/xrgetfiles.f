C***************************************************************
C
C subroutine:  xrgetfiles
C     Get names of input files.  Asks for nser file names.  If filename
C     begins with '@', the file is a list of files to use for that series.
C     a line in an '@' beginning with '///' implies that the next file
C     begins the next series and this routine will then *NOT* prompt
C     for a file for the next series but read from the same '@' file.
C     (LEB the above is gibberish, I've got to find a better way to say this)
C     Tests immediately for file existence.
C
C written by:
C      Lawrence E Brown
C      HEASARC/GSFC/NASA  Hughes STX
C      3/30/95
C
C modification history:
C
C notes:
C     This routine should be followed by xrcheckfiles
C
C calling sequence:
C      call xrgetfiles(nser,nfil,cfile,status)
C
C variables:
C
C     nser         I  Number of series for this task
C     nfil(array)  O  Number of files in each series (array)
C     cfile(array) O  Array of file names (+options)
C     
C********************************************************************
      SUBROUTINE xrgetfiles( nser, nfil, cfile, status)
      implicit none 
      include '../include/io.inc'
      include '../include/xronos.inc'

      character(160) cfile_in,tfile,cfil
      logical need_a_file,file_exist,at_file
      integer m,iblank,extnum,luin,ifile
      integer lenact
      external lenact

      include '../include/xronos_init.inc'
      parameter (subname = 'xrgetfiles:')

      
      if(status.ne.0) return
      
      need_a_file=.true.

c  Initialize to avoid warning
      at_file = .false.
c  --

c
c big loop for nser series
      DO m = 1, nser
C     need_a_file should be false if you've got more than one series
C     in an "@" file.  This is done by putting a '///' on a line by itself
C     in the file
         if(need_a_file) then
            at_file=.false.
            call uclgst(cfile_pars(m),cfile_in,status)
            if (status.ne.0) then
               write(context,'(''Error getting CFILE'',I1,
     $              '' parameter'')') m
               errm = subname//' '//context
               call xaerror(errm, 5)
               goto 999
            endif
            if(cfile_in(1:1).eq.'@') then
               at_file=.true.
               cfile_in=cfile_in(2:)
            endif
         endif
         if(.not.at_file) then
C     single file name, just set cfile and loop back for next series 
C     if necessary
            need_a_file = .true.
            nfil(m) = 1
            cfile(nfil(m),m)= cfile_in
            iblank=index(cfile(nfil(m),m),' ')
            cfile_in=cfile(nfil(m),m)(1:iblank-1)
C     strip FTOOLS style extension off
            call xrstrext(cfile_in,tfile,extnum,status)
            inquire(file=tfile,exist=file_exist)
            if(.not.file_exist) then
               context = cfile(nfil(m),m)(1:lenact(cfile(nfil(m),m)))//
     $              ' does not exist'
               errm = subname//' '//context
               call xaerror(errm, 5)
               status=1010
               goto 999
            endif
         else
C     @file name, so read them all in
            if(need_a_file) then
               call getlun(luin)
               call openwr(luin,cfile_in,'OLD',' ',' ',0,1,status)
               call frelun(luin)
            endif
            need_a_file = .true.
            if(status.ne.0) then
               context='Couldn''t open file'//cfile_in
               errm = subname//' '//context
               call xaerror(errm, 5)
               status= 1003          
               goto 999
            endif
C     loop over all files in a given series
            do ifile=1,nfilemax+1
               cfil = ' '
 11            read(luin,101,end=1000) cfil
c     ignore blank lines
               if(lenact(cfil).eq.0) goto 11
 101           format(a)
               if(cfil.ne.'///') then
                  if(nfil(m).eq.nfilemax) then
C     deal with too many files
                     write(context,
     $                    '(''Maximum number of files per series is'',
     $                    I3)') nfilemax
                     call xwrite(context,5)
                     write(context,
     $                    '(''Ignoring extra files in series'',I1)')m
                     call xwrite(context,5)
 10                  read(luin,101,end=1000) cfil
                     if(cfil.ne.'///') then
                        goto 10
                     else
                        need_a_file=.false.
                        goto 1000
                     endif
                  endif
                  nfil(m)=nfil(m)+1
                  cfile(nfil(m),m)=cfil
                  iblank=index(cfile(nfil(m),m),' ')
                  cfil=cfile(nfil(m),m)(1:iblank-1)
C     strip FTOOLS style extension off
                  call xrstrext(cfil,tfile,extnum,status)
                  inquire(file=tfile,exist=file_exist)
                  if(.not.file_exist) then
              context = cfile(nfil(m),m)(1:lenact(cfile(nfil(m),m)))//
     $                    'does not exist'
                     errm = subname//' '//context
                     call xaerror(errm, 5)
                     status=1010
                     goto 999
                  endif
               else
c     we've got another series in this file
                  need_a_file=.false.
                  goto 1000
               endif
            enddo
 1000       continue
            if(need_a_file) close(luin)
         endif
      enddo

 999  return
      end

