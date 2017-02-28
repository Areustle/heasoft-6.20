      subroutine rd_objkey(Lun, Object, Ierr)

      implicit none
c
c  Read object keyword
c
c  I  Lun    (i)  Logical unit of open FITS file
c  O  Object (c)  Value of OBJECT keyword
c  O  Ierr   (i)  Error flag (0=OK)
c
      integer Lun, Ierr
      character*(*) Object
c
c  Local variables
c
      integer mvstat
      character(80) comment
      integer curhdu, outhdu, hdutype

      Ierr = 0
      Object = ' ' 

      call FTGKYS(lun,'object',Object,comment,Ierr)
c
c Try looking in other extensions
c
      if ( Ierr.ne.0 ) then
         call ftghdn(Lun, curhdu)
         call matchext(Lun,'OBJECT',1,'*',1,outhdu,Ierr)
         if ( Ierr.eq.0 ) then
            call ftgkys(Lun,'object',Object,comment,Ierr)
            mvstat = 0
            call ftmahd(Lun, curhdu, hdutype, mvstat)
         endif
         if ( Ierr.ne.0 ) then
            call XWARN(' Warning: No object',15)
         else
            call XWRITE(' Using OBJECT keyword', 20)
         endif
      endif

      return
      end
