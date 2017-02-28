      subroutine rd_detkeys (Lun, Unknown, Telescop, Instrume, Detnam,
     &                       Filter, Itel, Status)
      
      implicit none
      integer*4 Lun, Itel, Status
      logical Unknown
      character*(*) Telescop, Instrume, Detnam, Filter
C
C  Given a unit number for an open FITS file, reads and returns
c  keywords identifying the detector and filter:
C  TELESCOP, INSTRUME, DETNAM, FILTER
C
c   I   Lun      (i) Logical unit of open FITS file
c   I   Unknown  (l) If true, act as if keywords not found
c   O   Telescop (c) TELESCOP keyword
c   O   Instrume (c) INSTRUME keyword
c   O   Detnam   (c) DETNAM keyword
c   O   Filter   (c) FILTER keyword
c   O   Itel     (i) Index for matching detector
c   O   Status   (i) Error flag (0=OK)
c
      include '../include/startup.inc'
      include '../include/io.inc'
c      
c Local variables
c
      integer*4 i, DETIDX, LENACT
      character(80) comment, grating
      integer curhdu, outhdu, hdutype, telstat
      character(240) ds

      Telescop = ' '
      Instrume = ' '
      Detnam = ' '
      Filter = ' '
      Itel = -2
      grating = ' '

      Status = 0
c
      if ( Unknown ) goto 300

c get identifying keywords
c
      call FTGKYS(Lun,'TELESCOP',Telescop,comment,Status)
      telstat = Status
      call FTGKYS(Lun,'INSTRUME',Instrume,comment,Status)
      call FTGKYS(Lun,'DETNAM',Detnam,comment,Status)
      call FTGKYS(Lun,'FILTER',Filter,comment,Status)
      call ftgkys(Lun,'GRATING',grating,comment,Status)
      Status = 0
c
c  Try looking in other extensions if failed
c
      if ( telstat.ne.0 ) then
         call ftghdn(Lun, curhdu)
         call matchext(Lun,'TELESCOP',1,'*',1,outhdu,Status)
         if ( Status.eq.0 ) then
            call ftgkys(Lun,'TELESCOP',Telescop,comment,Status)
            telstat = Status
            call ftgkys(Lun,'INSTRUME',Instrume,comment,Status)
            call ftgkys(Lun,'DETNAM',Detnam,comment,Status)
            call ftgkys(Lun,'FILTER',Filter,comment,Status)
            call ftgkys(Lun,'GRATING',grating,comment,Status)
            Status = 0
            call ftmahd(Lun, curhdu, hdutype, Status)
            if ( Status.ne.0 ) then
               call xwrite(' rd_detkeys: Failed to move to original HDU'
     &                     , 10)
               goto 300
            endif
         endif
      endif

      if ( telstat.eq.0 ) then
         i = INDEX(Instrume,'-')
         if ( i.ne.0 ) Instrume(i:) = ' '

         Itel = DETIDX(Telescop, Instrume, Detnam)
      endif

      call upc(Telescop)
      call upc(Instrume)
      call upc(Detnam)

  300 if ( Itel.le.0 ) then
         call XWARN(' Unknown telescope',10)
      endif
      ds = Telescop(1:LENACT(Telescop))//' '//
     &     Instrume(1:LENACT(Instrume))//' '//
     &     Detnam(1:LENACT(Detnam))
      call rmvxbk(ds)
      if ( ds.ne.' ' ) then
         write(ZWRite,'(2a)') ' Telescope ', ds(:LENACT(ds))
         call XWRITE(ZWRite, 10)
      endif 

      if ( grating.ne.' ' ) then
         call upc(grating)
         if ( grating.ne.'NONE' ) then
             write(ZWRite,*) ' GRATING = ', grating(:LENACT(grating))
             call xwarn(ZWRite, 10)
         endif
      endif

      return
      end
