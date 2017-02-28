      subroutine lkupfile(filename, ext, desc, status)
      implicit none
c
c find file in ximage installation area (e.g. header template files)
c
c I/O filename (s) Location of file
c  I  ext      (s) File extension
c  I  desc     (s) File description
c  O  status   (i) Error flag (0=OK)
c
      character*(*) filename, ext, desc
      integer status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/sitedef.inc'
c
c  Local variables
c
      integer LENACT
      logical there

      character(300) ds
      character*(MAX_FILELEN) tmpfile

      status = 0

      ds = filename
c
c  List available files in distribution for ?
c
      if ( ds.eq.'?' ) then
         filename = ' '
         call PTEND(Cxan,Cpar,filename)
         ds = 'show_ximfiles.pl '//filename(:LENACT(filename))//
     &        ' '//ext(:LENACT(ext))
         write(ZWRite,*) 'Available ',desc(:LENACT(desc)),' files:'
         call xwrite(ZWRite, 10)
         call spawn(ds,LENACT(ds),status)
         status = -1
         return
      endif
c
c  Search distribution then local directory for file
c  If blank, use 'default'
c
      call XWRITE (' Search path --', 20)
      write (ZWRite,'(2a)') ' File: ', filename(:LENACT(filename))
      call XWRITE (ZWRite, 20)
      write (ZWRite,'(2a)') ' Extension: ', ext(:LENACT(ext))
      call XWRITE (ZWRite, 20)

      if ( filename.ne.' ' ) then

         tmpfile = filename
         call qustrip(tmpfile)
         call XTEND(tmpfile,ext)
         call PTEND(Cxan,Cpar,tmpfile)
         inquire (FILE=tmpfile, EXIST=there)

         if ( there ) then
            filename = tmpfile
         else
            tmpfile = filename
            call qustrip(tmpfile)
            call XTEND(tmpfile,ext)
            inquire (FILE=tmpfile, EXIST=there)

            if ( there ) then
               filename = tmpfile
            else
               inquire (FILE=filename, EXIST=there)
            endif

         endif

      else

         ds = 'default'
         filename = 'default.'//ext(1:LENACT(ext))
         call PTEND(Cxan,Cpar,filename)
         inquire (FILE=filename, EXIST=there)

      endif

      if ( .not.there ) then
         status = -1
         write(ZWRite,*) ' Failed to find ',desc(:LENACT(desc)),': ',
     &                   ds(:LENACT(ds))
         call XWRITE (ZWRite, 10)
      endif

      return
      end
