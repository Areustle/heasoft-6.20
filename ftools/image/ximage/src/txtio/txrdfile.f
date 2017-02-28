      subroutine txrdfile(filename, status)
      implicit none
c
c  Reads text file into txtio buffer
c
c  I  filename  (s)  Text file location
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename
      integer status

      include 'txtio.inc'
      include '../include/io.inc'
c
c  Local variables
c
      integer lun, i

      if ( status.ne.0 ) return

      if ( filename.eq.' ' ) then
         status = -1
         return
      endif

      if ( txtqfile.eq.filename ) return
      call txinit(status)

      call getlun(lun)
      call openwr(lun,filename,'old',' ',' ',0,1,status)
      if ( status.ne.0 ) then
         call XWRITE(' Could not open text file', 10)
         call frelun(lun)
         return
      endif

      call XWRITE(' Reading text file into txtio buffer', 25)
      call XWRITE(filename, 30)
      call rdqdp(0, lun, filename, qbuf, mxqbuf, qerr, mxqcol,
     &           nqrows, nqpts, nqcol, qcom, mxqcom, nqcom, status)

      close(lun)
      call frelun(lun)

      if ( status.eq.0 ) then
         txtqfile = filename
         txtqmode = 0
c
c       Add error columns to nqcol
c
         do i = 1, nqcol
            nqcol = nqcol + qerr(i)
         enddo
         write(ZWRite,*) ' Columns read in: ', nqcol
         call rmvxbk(ZWRite(2:))
         call xwrite(ZWRite, 20)
      else
         call XWRITE(' rdqdp: Read failed', 10)
      endif

      return
      end
