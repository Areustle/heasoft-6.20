      subroutine gt_fitstype (Filename,Filetype,Extnum,Status)
      implicit none
c
c This routine allows the user to type read/fits /whatever and have
c it pick the right type of fits file to read.
c
c It firsts looks to see if there is a primary array.  If so, it returns
c type 'IMG'.  Else it tries to find a binary table extension named
c events.  If it succeds then it returns type 'EVT'   Else, it
c gives a message and returns type ' '
c
c  I  Filename    (c)  Filename given by user may include extension
c  O  Filetype    (c)  'EVT' (event list), 
c                      'IMG' (image), or ' ' (unknown)
c  O  Extnum      (i)  Extension number
c  O  Status      (i)  Error flag  (0 = OK)
c
      character*(*) Filename, Filetype
      integer Extnum, Status

      include '../include/io.inc'
c
c Local variables
c
      integer LENACT
      integer lun, block, nrows, hdutype
      logical simple, extend
      LOGICAL ZIMAGE
      integer naxis, naxes(3)
      integer bitpix, pcount, gcount, tchat, lchat
      character(80) comment
      character(30) exttype, errmsg

      Filetype = ' '
c
c open fits file
c
      Status = 0
      block = 1
      call GETLUN(lun)
      call FTOPEN(lun,Filename,0,block,Status)
      if ( Status.ne.0 ) then
         write(ZWRite,'(2a)') ' Failed to open ',
     &                        Filename(:LENACT(Filename))
         call xwrite(ZWRite,10)
         call ftgerr(Status, errmsg)
         call upc(errmsg(1:1))
         write(ZWRite,'(2a)') ' ', errmsg
         call xwrite(ZWRite, 10)
         call xgtcht(tchat, lchat)
         if ( tchat.ge.20 ) call ftrprt('STDOUT', Status)
         call FTCLOS(lun, Status)
         call FRELUN(lun)
         return
      endif

      if ( Extnum.gt.0 ) then
         call ftmahd (lun, Extnum+1, hdutype, Status)
         if ( Status.ne.0 ) then
            write(ZWRite,'(a,i3,a,a)') ' Extension ', Extnum,
     &               ' does not exist in ',Filename(:LENACT(Filename))
            call XERROR(ZWRite,10)
            return
         endif
      else
         bitpix = -999
         call FTGHPR(lun,3,simple,bitpix,naxis,naxes,pcount,gcount,
     &               extend,Status)
         if ( Status.eq.0 .and. naxis.ge.2 ) then
            Filetype = 'IMG'
            Extnum = 0
            call FTCLOS(lun,Status)
            call FRELUN(lun)
            return
         endif
      endif

      Status = 0

*
* Find the events table
*
      if ( Extnum.lt.0 ) then 
         call go_evttbl(lun, Extnum, nrows, status)
         if ( status.eq.0 ) then
c
c           FOUND events table, Check for empty events table
c
            if ( nrows.eq.0 ) then
               call XWRITE(' The events table is empty' ,10)
               call FTCLOS(lun,Status)
               Status = -1
               call FRELUN(lun)
               return
            endif
         else
c
c           NO EVENTS, Look for image extension
c
            status = 0
            call go_imgext(lun, Extnum, status)
            if ( status.ne.0 ) then
               call XERROR(
     &            ' Fits file has no 2d primary array, IMAGE'//
     &            ' extension or EVENTS extension', 5)
               call FTCLOS(lun,Status)
               Status = -1
               call FRELUN(lun)
               return
            endif
         endif

      endif
c
c Assume it's an event table
c
      Filetype = 'EVT'
c
c Unless XTENSION = 'IMAGE' 
c
      call ftgkys (lun,'XTENSION',exttype,comment,status)
      call upc(exttype)
      if ( exttype.eq.'IMAGE' ) then
         Filetype = 'IMG'
      endif
c
c Or XTENSION = 'BINTABLE' and EXTNAME = 'COMPRESSED_IMAGE'
c
      if ( exttype.eq.'BINTABLE' ) then
         ZIMAGE = .FALSE.
         call ftgkyl (lun,'ZIMAGE',ZIMAGE,comment,status)
         IF ( STATUS.EQ.202 ) THEN
            STATUS = 0
         ELSEIF ( ZIMAGE ) THEN
            FILETYPE = 'IMG'
         ENDIF
      endif
 
      call FTCLOS(lun,Status)
      call FRELUN(lun)
 
      return
      end
