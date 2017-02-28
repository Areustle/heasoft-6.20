c This file contains all the little system-dependent subroutines
c for XSELECT
c -- This is the SUN version; XSL_SUN.
c
c     Alan Smale & Jim Ingham, 1992/1993
c
c List of routines in this directory grouped by function:
c
c Routines to deal with command files
c     XSL_WRTCF   writes a command to the command file
c     XSL_CLCF    closes the command file
c
c Routines to deal with the data directories
c     XSL_DATDIR  prepends data directory path to filename
c
c Routines to remove one or more files, or rename a file
c     XSL_GETRM   get the string necessary to remove a given file
c     XSL_RMFILE  removes a file
c     XSL_RMWORK  remove ALL the given WORKspace files
c     XSL_RENAME  rename a file
c
c Routines to list files or read in an Ascii listing
c     XSL_LIST    list files with a given filetype in directory
c     XSL_SHLIST  read in an ascii listing
c
c These routines are stored in alphabetical order in this file.
c
c Ning Gan: removed XSL_RUNCF.   1999/04/09
c
c ---------------------------------------------
      subroutine XSL_NUMSORT(infile,colnum,ASCEND,status)
c ---------------------------------------------
c This sorts the file infile on the column colnum, in ascending numerical
c order if ASCEND is true, descending otherwise.
c For now, assume colnum < 10.
c J Ingham June/94

      character*(*) infile
      integer status,colnum,LENACT,len1
      logical ASCEND
      character(255) cmdfil, lstfil,errfil,wrkdir
      character(1024) comlin
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

      len1 = LENACT(infile)
      if(ASCEND) then
         write(comlin,95) colnum-1, colnum,infile(:len1),infile(:len1)
 95      format('sort -n +',I1,' -',I1,' -o ',a,' ',a)
      else
         write(comlin,96) colnum-1, colnum,infile(:len1),infile(:len1)
 96      format('sort -nr +',I1,' -',I1,' -o ',a,' ',a)
      endif
      call XSL_SPAWN(comlin,status)

      return
      end
c
c
c ---------------------------------------------------------
      subroutine XSL_CAT(regvec,nregfl,regfil,MAXFIL)
c ---------------------------------------------------------
c  Cats the files in regvec into the file regfil.  Assumes all are in
c  wrkdir
c    Jim Ingham  5/93

      implicit none
      integer MAXFIL,nregfl,i,status,olun,ilun,LENACT
      character*(*) regfil,regvec(MAXFIL)
      character(10240) instr

      call XSL_RMFILE(regfil)
      call GETLUN(olun)
      call XSL_OPEN(olun,regfil,'NEW',' ',' ',0,0,status)
      if(status.ne.0 ) then
         instr = 'Error opening outfile '//regfil
         call XWRITE(instr,5)
         return
      endif

      IF(nregfl.gt.0) THEN
         call GETLUN(ilun)
         do i=1,nregfl
            call XSL_OPEN(ilun,regvec(i),'OLD',' ',' ',0,0,status)
            if( status.ne.0) then
               instr = 'Error opening infile '//regvec(i)
               call XWRITE(instr,5)
               close(olun)
               call FRELUN(ilun)
               call FRELUN(olun)
               return
            endif
            do while ( status .eq. 0 )
               read(ilun,'(a)',end=99) instr
               write(olun,'(a)',err = 999,iostat = status )
     &              instr(:LENACT(instr))
            enddo
 99         close(ilun)
         enddo
      ELSE
         return
      ENDIF

      close(olun)
      call FRELUN(ilun)
      call FRELUN(olun)

      return

 999  instr = 'Error writing to output file '//regfil
      call XWRITE(instr,5)
      close(olun)
      close(ilun)
      call FRELUN(ilun)
      call FRELUN(olun)
      return

      end


c
c
c ------------------------------------------------
      subroutine XSL_CD(ilun,newdir)
c ------------------------------------------------
c Puts a string into the shell script with LUN ilun to change
c dir. to newdir
c   Jim Ingham, April 1993

      implicit none
      character*(*) newdir
      character(255) str1
      integer ilun,len1,LENACT

      len1 = LENACT(newdir)
      str1 = 'cd '//newdir(1:len1)
      write(ilun,53) str1(1:len1+3)

 53   format(a)

      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_CLCF(ilun)
c ---------------------------------------------
c Closes the command file.
c     Alan Smale, June 1992
      implicit none

      character(80) str1
      integer ilun,len1,LENACT
      str1 = 'exit 0'
      len1 = LENACT(str1)
      write(ilun,53) str1(1:len1)

      close(ilun)
      call FRELUN(ilun)

 53   format(a)
      return
      end

c
c
c ---------------------------------------------
      subroutine XSL_CLEANPAR()
c ---------------------------------------------
c  This subroutine cleans up various ftools par files on exit.
c  Jim Ingham 5/93

      character(255) cmdfil, lstfil,errfil,str1,wrkdir
      integer ilun,status
      logical ECHO
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

      ECHO = .FALSE.

      call XSL_OPCF(cmdfil,ilun)
      str1 = 'pset fdump.par infile=" " columns="-"'
      write(ilun,50) str1
 50   format(a)
      close(ilun)
      call FRELUN(ilun)
      call XSL_RUNCF(cmdfil,ECHO,status)
      return
      end
c
c ----------------------------------------
      subroutine XSL_COMSAO(string,imfil,mode)
c ----------------------------------------
c Writes a string for saoimage or saotng, in the Bourne Shell
c mode = 0, normal operation
c mode = 1, kill regular and error output
c      J. Ingham 10/93
c      Mod: Jeff Guerber Jun 1998.  Init. ierr before call xsl_uclgst

      integer mode,len1,LENACT,ierr
      character*(*) string, imfil
      character(72) disptool

      ierr = 0
      call xsl_uclgst('imagedisp', disptool, ierr)
      IF ( ierr .NE. 0 ) RETURN

      len1 = LENACT(imfil)
      string = disptool(:LENACT(disptool))//' '//imfil(1:len1)

      len1 = LENACT(string)
      IF ( mode .EQ. 1 ) string(len1+1:) = ' 1> /dev/null 2>&1'

      len1 = LENACT(string)
      string(len1+1:) = ' &'

      return
      end

c
c
c ----------------------------------------
      subroutine XSL_COPY2(file1,file2,cpstr)
c ----------------------------------------
c
c

      character*(*) file1, file2, cpstr
      integer LENACT

      cpstr = '/bin/cp '//file1(:LENACT(file1))//' '
     &     //file2(:LENACT(file2))

      return
      end

c
c ---------------------------------------------
      subroutine XSL_COPY(file1,file2,status)
c ---------------------------------------------
c Copies file1 to file2, prompting for clobber if the file exists.
C If the first character is a !, then the copy is forced.
c     J. Ingham 5/93
      implicit none

      character*(*) file1,file2
      character(255) string
      integer status,LENACT,lstring
      logical CLOBBR

      CLOBBR = .FALSE.
      status = 0
c Get rid of the '!', and set CLOBBR:
      IF(file2(1:1).eq.'!') THEN
         file2 = file2(2:)
         CLOBBR = .TRUE.
      ENDIF
c Check for source file:
      call XSL_EXIST(file1,status)
      IF(status.ne.0) THEN
         string = 'The source file: '//file1(:LENACT(file1))//
     &             'doesn''t exist.'
         call XWRITE(string,5)
         status = -20
      ENDIF

c Test for existence of target file:

      call XSL_EXIST(file2,status)
      if(status.EQ.0.AND. .NOT.  CLOBBR) THEN
         call xsl_uclgsb('clobberit',CLOBBR,status)
         IF(.not.CLOBBR) THEN
            status = -10
            return
         ENDIF
      ELSE
         status = 0
      ENDIF

c Do the copy:

      string = '/bin/cp '//file1(1:lenact(file1))//' '//
     &                      file2(1:lenact(file2))
      lstring = lenact(string)
c MJT 16July96 (g77/linux) doesn't have system function
c      status = system(string(:lstring))
      call xsl_spawn(string(:lstring),status)

      return
      end
c
c
c
c ----------------------------------------------
      subroutine XSL_DATDIR(filenm,datdir,remove)
c ----------------------------------------------
c This takes a filename and prepends to it the datdir path
c It returns the final length, but you don't need to pass it
c the original length.  Also check to see that the datdir is not
c already prepended.
c     Jim Ingham  March 1992


      character*(*) filenm,datdir
      integer len1,LENACT,len2,len3,remove
      character(266)  str1

c If the filename starts with "/" and remove=0 then we will assume that 
c it is an absolute path and not prepend the data directory

      IF ( remove .EQ. 0 .AND. filenm(1:1) .EQ. '/' ) RETURN

c If the filename starts with ftp then it is a network file so we don't
c want to prepend the datadir

      IF ( filenm(1:4) .EQ. 'ftp:' ) RETURN

C  Don't prepend twice!      

      len1 = LENACT(datdir)
      len2 = LENACT(filenm)
      if(index(filenm(1:len2),datdir(1:len1)).ne.0) then
         return
      endif
      if(remove.gt.len2) then
         call XWRITE('Attempt to remove more than there is',5)
         return
      endif
      len3 = LEN(filenm)

      if(len1+len2-remove .gt. len3) then
          call XWRITE('Filename too long, it would be truncated',5)
          return
      endif

      write(str1,17)remove
17    format('remove = ',i4)
      call xwrite(str1,15)

      str1 = 'datdir = '//datdir(:len1)//'!'
      call xwrite(str1,15)

      IF(datdir(len1:len1).eq.'/') THEN
         IF(filenm(remove+1:remove+1).eq.'/') THEN
            filenm = datdir(:len1)//filenm(remove+2:len2)
         ELSE
            filenm = datdir(:len1)//filenm(remove+1:len2)
         ENDIF
      ELSE
         IF(filenm(remove+1:remove+1).eq.'/') THEN
            filenm = datdir(1:len1)//filenm(remove+1:len2)
         ELSE
            filenm = datdir(1:len1)//'/'//filenm(remove+1:len2)
         ENDIF
      ENDIF
      str1 = 'filenm = '//filenm(1:lenact(filenm))//'!'
      call xwrite(str1,15)
      return
      end

c -----------------------------------------------------------------
      subroutine XSL_FDUMP2(dmpfil,displi,rows,pgwth,prhead)
c -----------------------------------------------------------------
c  This routine displays the FITS form of the observation catalogue.
C  if the flagfv is set, call fv, othewise call xsl_fdump
c
      implicit none
      include 'xselplt.inc'
      integer status,LENACT
      character*(*) dmpfil,displi,prhead,rows
      character(255) str1
      integer pgwth
      integer extn
      character(5) sextn
      character(255) prefix
      character(255) rootname
      integer iunit
      integer hdutype
      integer onlyread
      integer block

      status = 0

C     using the fv instead
      if (flagfv .eq. 1) then
          call getenv("FTOOLS",prefix)
          open (unit = 28,
     &      file = prefix(:lenact(prefix))//"/lib/txselect/xsel_fv.fv",
     &      form = "formatted",status = "old")
          open (unit = 29, file = "xsel_runfv.fv",
     &      form = "formatted", status = "unknown")
78        continue
          str1 = " "
          read(28,98,end = 198)str1
98        format(a)
          write(29,98)str1(:lenact(str1))
          goto 78
198       str1 = " "
          write(29,98)str1(:lenact(str1))
          call ftextn(dmpfil(:lenact(dmpfil)),extn, status)
          call ftrtnm(dmpfil(:lenact(dmpfil)),rootname, status)
          if(extn.eq.-99) extn = 2
          write (sextn, '(1x,i3,1x)')extn
          if (prhead .eq. "NO" ) then
              str1 = "xsl_table "
          else
              str1 = "xsl_hdu "
          endif
C
C         if the HDU is an image, print the header only.
C
          onlyread = 0
          call ftopen(iunit, rootname(:lenact(rootname)), 
     &       onlyread, block, status)
          call ftmahd(iunit,extn, hdutype, status)
          if (hdutype.eq.0) str1 = "xsl_header "  
          call ftclos(iunit,status)

          str1 = str1(:lenact(str1))//" "//
     *         rootname(:lenact(rootname))//sextn
          write(29,98)str1
          close(28)
          close(29)
          str1 = "fv xsel_runfv.fv &"
          call xsl_spawn(str1(:lenact(str1)), status)
       else 
          call XSL_FDUMP(dmpfil,displi,rows,pgwth,prhead)
       endif
       return
       end

c -----------------------------------------------------------------
      subroutine XSL_FDUMP(dmpfil,displi,rows,pgwth,prhead)
c -----------------------------------------------------------------
c  This routine displays the FITS form of the observation catalogue.
c
      character(255) cmdfil, lstfil,errfil,str1,str3,wrkdir
      integer ilun,status,len1,len2,len3,LENACT
      logical ECHO,TDISP
      character*(*) dmpfil,displi,prhead,rows
      integer pgwth
      common /xselcmd/ cmdfil, lstfil,errfil, wrkdir

      ECHO = .FALSE.
      status = 0

      call xsl_uclgsb('tdisp',TDISP,status)

      write(str3,105,err=955,iostat=status) pgwth
 105  format('pagewidth = ',I3)
 955  IF(status.ne.0) THEN
         call XWRITE('Error in pagewidth parameter, '//
     &        'setting to 80',5)
         str3 = 'pagewidth = 80'
         status = 0
      ENDIF
      len1 = LENACT(dmpfil)
      len2 = LENACT(displi)
      len3 = LENACT(str3)

c  Now construct the command string

      str1 = 'fdump '//dmpfil(1:len1)//' '//
     &               'STDOUT'//' '//
     &               displi(1:len2)//' '//
     &                rows(:LENACT(rows))//' '//
     &               'prhead = '//prhead//' '//
     &                str3(1:len3)//' '//
     &               'wrap = yes'//' '//
     &               'showunit = no'
      if (TDISP) THEN
         str1 = str1(:LENACT(str1))//' '//'tdisp = yes'
      else
         str1 = str1(:LENACT(str1))//' '//'tdisp = no'
      endif


      call XSL_OPCF(cmdfil,ilun)
      len1 = LENACT(str1)
      write(ilun,50) str1(1:len1)
      close(ilun)
      call FRELUN(ilun)
      call XSL_RUNCF(cmdfil,ECHO,status)
 50   format(a)
      return
      end

c
c
c ---------------------------------------------
      subroutine XSL_FILENAME(fullnm,filenm,len1)
c ---------------------------------------------
c This subroutine strips the filename off the path given by fullnm.
c len1 is input as the length of the string filenm, and is returned
c as the length of filenm.
c
      implicit none
      character*(*) fullnm,filenm
      integer len1,i

      do i=len1,1,-1
         if(fullnm(i:i).eq.'/') goto 10
      enddo
      filenm=fullnm
      goto 20
 10   filenm = fullnm(i+1:len1)

      len1 = len1 - i

 20   return
      end
c
c
c ------------------------------------------------
      subroutine XSL_GETEXT(CNAM, CEXT)
c ------------------------------------------------
c  This puts the extension of name CNAM into CEXT.  It does
c  what the XANADU routine does, but more simple-mindedly
c  Jim Ingham 9/15/93

      CHARACTER CNAM*(*), CEXT*(*)
      integer len1,LENACT,i

      len1 = LENACT(cnam)
      do i=len1,1,-1
         if(cnam(i:i).eq.'.') goto 70
         if(cnam(i:i).eq.'/') goto 80
      enddo

c Come here if there is no dot in the filename
 80   cext = ' '
      goto 999

c Come here, i is the index of the last dot.
 70   cext = cnam(i+1:len1)

 999  return
      end

c
c
c ---------------------------------------------
      subroutine XSL_GETRM(str,fname)
c ---------------------------------------------
c Given a filename 'fname', put the correct command
c into 'str' to remove it. i.e. the Unix/Ultrix
c version should return str='rm -f fname' and the
c Dec version, str='delete fname.xxx;'
c
c     Alan Smale, March 1993
c
      implicit none

      character*(*) str, fname
      integer length
      integer LENACT

c Find out how long the filename is, make the command, find how long
c the command is, spawn the command.

      length = LENACT( fname )

      str = '/bin/rm -f '//fname(1:length)

      return
      end
c
c
c$$$c NOW USING ROUTINE IN xsel_unix_c.c
c$$$c ---------------------------------------------
c$$$      subroutine XSL_LIST(file,lststr,datdir,curdir,cmdfil)
c$$$c ---------------------------------------------
c$$$c Lists all files in directory datdir matching lststr
c$$$c into file FILE, which is put in curdir
c$$$c     Alan Smale, Oct 1992
c$$$c     Jim Ingham, Early 1993
c$$$      implicit none
c$$$
c$$$      character*(*) file, lststr,cmdfil,datdir,curdir
c$$$      character(255) str1,str3
c$$$      integer LENACT,len1,len2,len4,ilun,status
c$$$      logical ECHO
c$$$
c$$$      ECHO = .FALSE.
c$$$
c$$$      call XSL_OPCF(cmdfil,ilun)
c$$$
c$$$
c$$$c delete the file before creating it
c$$$      call XSL_RMFILE(file)
c$$$
c$$$      if(datdir.ne.'NONE') then
c$$$
c$$$         call XSL_CD(ilun,datdir)
c$$$      endif
c$$$
c$$$      str3 = file
c$$$      call XSL_DATDIR(str3,curdir,0)
c$$$      len1 = LENACT(str3)
c$$$
c$$$      len2 = LENACT(lststr)
c$$$      str1 = '(cd '//datadir(:LENACT(datadir))//';/bin/ls '
c$$$     &                 //lststr(1:len2)//' > '//str3(1:len1)//
c$$$     &     ' 2> /dev/null)'
c$$$      len4 = LENACT( str1 )
c$$$
c$$$      write(ilun,53) str1(1:len4)
c$$$      call XSL_CD(ilun,curdir)
c$$$      call XSL_CLCF(ilun)
c$$$      call XSL_RUNCF(cmdfil,ECHO,status)
c$$$ 53   format(a)
c$$$      return
c$$$      end
c
c
c ---------------------------------------------
      subroutine XSL_MESSAGE(ilun,string)
c ---------------------------------------------
c  Puts a message in the command file
c  Jim Ingham, April 1992

      implicit none
      character*(*) string
      character(255) message
      integer len1, ilun, LENACT

      len1 = LENACT(string)
      message = 'echo '//string(1:len1)

      write(ilun,53) message(1:len1+5)
 53   format(a)
      return
      end

c
c ---------------------------------------------
      subroutine XSL_OPCF(cmdfil,ilun)
c ---------------------------------------------
c Wrap-up to open command file
c   kaa  10/7/93

      character*(*) cmdfil
      integer ilun, ierr
      integer LENACT
      external LENACT
      character(256) str

c delete any extant command file

      call XSL_RMFILE(cmdfil)

c get a free LUN

      call GETLUN(ilun)

c open the file

      call XSL_OPEN(ilun, cmdfil, 'new', ' ', ' ', 0, 0, ierr)
      if (ierr .ne. 0) then
         write(str,'(a,a)')' Failed to open ',cmdfil(:LENACT(cmdfil))
         call xwrite(str,5)
         write(str,'(a,i3)') ' iostat = ',ierr
         call xwrite(str,5)
      endif
      write(ilun,'(a)') '#!/bin/sh'

      return
      end
c
c
c
c
c-----------------------------------------------
      subroutine XSL_PATHEND(datdir,str1,len1)
c-----------------------------------------------
c This returns the last member of directory path datdir in str1
c len1 is the number of characters in str1
c J. Ingham 6/93
      character*(*) datdir, str1
      integer len1,LENACT,i,len2

      len1 = LENACT(datdir)
      len2 = 0
c     Don't include the final '/', there may be more than one:
      DO i=len1,1,-1
         IF(datdir(i:i).eq.'/') THEN
            len2 = i - 1
         ELSE
            goto 25
         ENDIF
      ENDDO
 25   IF(len2.eq.0) THEN
c If the path is /, there is no last member
         continue
      ELSE IF(datdir(len2:len2).eq.'.') THEN
c If the datdir is given as ./ or ., don't know the last member.
         len2 = 0
      ELSE
         len1 = len2
         call XSL_FILENAME(datdir,str1,len1)
      ENDIF

      return
      end
c
c
c ----------------------------------------------
      subroutine XSL_RELDIR(dir1,relpath,dir2,status)
c ----------------------------------------------
c Appends the relative path, relpath, to dir1 to get dir2
c Assumes dir1 exists, checks for existance of dir2.
c Jim Ingham April 1993
      implicit none

      character*(*) dir1,dir2,relpath
      integer len1,len2,LENACT,status

      len1 = LENACT(dir1)
      len2 = LENACT(relpath)
      If(relpath.eq.'/.'.or.relpath.eq.'.'.or.relpath.eq.'./'.or.
     &   len2.eq.0)then
         dir2 = dir1
      else
         IF(dir1(len1:len1).eq.'/') then
            IF(relpath(1:1).eq.'/') THEN
               dir2 = dir1(1:len1-1)//relpath(1:len2)
            else
               dir2 = dir1(1:len1)//relpath(1:len2)
            endif
         ELSE IF(relpath(1:1).eq.'/') THEN
            dir2 = dir1(1:len1)//relpath(1:len2)
         ELSE
            dir2 = dir1(:len1)//'/'//relpath(1:len2)
         ENDIF
         len1 = 0
         call XSL_CHKDIR(dir2,status)
         if(status.eq.0) then
            call XSL_EXPAND_DIR(dir2,len1)
         endif
      endif

      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_RENAME(fname1,fname2,mode,string,len2,ierr)
c ---------------------------------------------
c Changes a filename using MV or RENAME
c Moves/Renames FILE1 to FILE2.
c
c Two modes of operation
c     MODE = 1    Do the rename by single spawn
c     MODE = 2    Create the string and pass it back, no spawn
c
c     Alan Smale; June, Nov 1992

      implicit none

      character*(*) string,fname1,fname2
      integer len1, len2, mode, ierr
      integer LENACT

      len1 = LENACT( fname1 )
      string='/bin/mv -f '//fname1(1:len1)//' '//fname2(1:len2)
      len2 = LENACT( string )

      if( MODE .eq. 1)then
c MJT 16July96 (g77/linux) doesn't have system function
c         ierr = system(string)
         call xsl_spawn(string,ierr)
      endif

      return
      end
c
c
c
c Now use the routine in xsel_unix_c.c
c ---------------------------------------------
c      subroutine XSL_RMFILE(fname)
c ---------------------------------------------
c Removes a file.
c     Alan Smale, June 1992
c      implicit none

c      character(255) string
c      character*(*) fname
c      integer length,ierr
c      integer LENACT,SYSTEM

c Find out how long the filename is, make the command, find how long
c the command is, spawn the command.

c      length = LENACT( fname )
c      string='/bin/rm -f '//fname(1:length)
c      length = LENACT( string )

c      ierr = SYSTEM(string(1:length))
c      call SPAWN(string(1:length),length,ierr)

c      return
c      end
c
c
c
c ---------------------------------------------
      subroutine XSL_RMWORK(fnames,N)
c ---------------------------------------------
c Upgraded version based on XSL_RMFILE.
c Removes several workspace files at a shot, contained
c in a character array FNAMES with length N.   Alan Nov 92
c (Note: all files will be the same length.)

      implicit none

      integer N
      character(255) string
      character*(*) fnames(N)
      integer len1, len2, ierr
      integer LENACT
      integer i

      len1 = LENACT( fnames(1) )

      do i=1,N
         string='/bin/rm -f '//fnames(i)(1:len1)

         len2 = LENACT( string )
c MJT 16July96 (g77/linux) doesn't have system function
c         ierr = SYSTEM(string(1:len2))
         call xsl_spawn(string(1:len2),ierr)
      end do

      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_SHLIST(file)
c ---------------------------------------------
c Show list, in other words display an ASCII file.
c For example, an observation catalogue.
c
c This will get more sophisticated (i.e. reading in
c the file and manipulating it) when the listing
c gets more sophisticated
c     Alan Smale, Oct 1992
      implicit none

      character*(*) file
      character(255) str1
      integer len1, ierr
      integer LENACT

      len1 = LENACT( file )

      str1 = '/bin/cat '//file(1:len1)
      len1 = LENACT( str1 )

c MJT 16July96 (g77/linux) doesn't have system function
c      ierr = SYSTEM(str1(:len1))
      call xsl_spawn(str1(:len1),ierr)

      return
      end
c
c
c ------------------------------------------------
      subroutine XSL_XTEND(CNAM, CEXT,status)
c ------------------------------------------------
c  This replaces the extension of name CNAM with CEXT.  If
c  CEXT starts with a '-', then it is only apended if there is
c  no extension.  If CEXT is blank, the extension is removed.
c  Jim Ingham 9/15/93

      CHARACTER CNAM*(*), CEXT*(*)
      integer len1,len2,len3,LENACT,i,status,point

      status = 0
      len1 = LENACT(cnam)
      len2 = LENACT(cext)
      len3 = len(cnam)

      do i=len1,1,-1
         if(cnam(i:i).eq.'.') then
            point = i
            goto 70
         endif
         if(cnam(i:i).eq.'/') then
            point = i
            goto 80
         endif
      enddo

c Come here if there is no dot in the filename
 80   IF(len2.eq.0) THEN
c Return no extension if CEXT is blank
         continue
      ELSE IF(len1+len2+1.gt.len3) THEN
         call XWRITE('Filename variable too short in XSL_XTEND',5)
         status = -20
      ELSE IF(cext(1:1).eq.'-') THEN
         cnam = cnam(:len1)//'.'//cext(2:len2)
      ELSE
         cnam = cnam(:len1)//'.'//cext(:len2)
      ENDIF
      goto 999

c Come here, i is the index of the last dot.
 70   IF (len2.eq.0) THEN
c Strip the extension if CEXT is blank:
         cnam = cnam(:point-1)
      ELSE IF(len1+len2.gt.len3) THEN
         call XWRITE('Filename variable too short in XSL_XTEND',5)
         status = -20
      ELSE IF(cext(1:1).ne.'-') THEN
         cnam = cnam(:point)//cext(:len2)
      ENDIF

 999  RETURN
      END

