      subroutine getcol(Lun, Colname, Icol, Status)
      implicit none
c
c  Retrieve column index
c
c  I  Lun     (i) Logical unit of open event file
c I/O Colname (c) Column name
c  O  Icol    (i) Column index
c  O  Status  (i) Error flag (0=OK)
c
      integer*4 Lun
      character*(*) Colname
      integer*4 Icol, Status

      integer*4 LENACT

      include '../include/io.inc'
c
c  Local variables
c
      character(100) ds
      character(10) str
      integer*4 slen

      Icol = -1
      if ( Colname.eq.' ' ) return
      if ( Status.ne.0 ) return
c
c Get the column index
c    Take first match (status=237 means more than one match)
c                     (i.e. Colname can have wildcards )
c
      call FTGCNN(Lun,.FALSE.,Colname,ds,Icol,Status)
      if ( Status.eq.237 ) Status = 0
      if ( Status.eq.0 ) then
         call xistr(Icol, str, slen)
         write(ZWRite,'(5a)') ' Got ',ds(1:LENACT(ds)),' column (',
     &                     str(:slen), ')'
         call XWRITE(ZWRite,15)
         call UPC(ds)
         Colname = ds
      else
         write(ZWRite,'(3a)') ' Error getting ',
     &                        Colname(1:LENACT(Colname)),' column'
         call XERROR(ZWRite,5)
      endif

      return
      end
