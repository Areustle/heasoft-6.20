      subroutine rdtxtcol(filename, icol, undef, rary, rdim, rnum,
     &                    status)
      implicit none
c
c  Reads icol-th column in text file and saves column values in rary
c
c  I  filename  (s)  Text file location
c  I  icol      (i)  Column number
c  I  undef     (s)  If value in column matches this, set to 0.
c  O  rary      (r)  Real array
c  I  rdim      (i)  Dimension of real array
c  O  rnum      (i)  Number of values saves to rary
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename, undef
      integer icol, rdim, rnum, status
      real*4 rary(rdim)

      include '../include/io.inc'
c
c  Local variables
c
      integer lun, i, j, k, llen, LENACT
      character(100) line
      real*8 dd

      status = 0

      call getlun(lun)
      call openwr(lun,filename,'old',' ',' ',0,1,status)
      if ( status.ne.0 ) then
         call XWRITE(' Could not open text file', 10)
         call frelun(lun)
         return
      endif

      rnum = 1
      do while (.TRUE.)
         status = 0
         read(lun,'(a)',END=50,ERR=40) line
         if ( line(1:1).ne.'!' ) then
            call RMVXBK(line)
            llen = LENACT(line)
            k = 1
            i = 1
            j = 1
            do while ( k.lt.icol .and. j.gt.0 )
               j = INDEX(line(i:llen),' ')
               i = i + j
               k = k + 1
            enddo
            if ( j.eq.0 ) then
               call XWRITE(' Not enough columns in file', 5)
               goto 40
            endif
            j = INDEX(line(i:),' ') + i - 2
            if ( Undef.ne.' ' .and. line(i:j).eq.Undef ) then
               dd = 0.
            else
               call strnum(line(i:j), 8, dd, status)
            endif
            rary(rnum) = dd
            if ( rnum.gt.rdim ) then
               call XWRITE(' Buffer cannot hold all column values', 5)
               write(ZWRite, *) ' Buffer dimension', rdim
               call XWRITE(ZWRite, 15)
               goto 40
            endif
            rnum = rnum + 1
         endif
      enddo

  40  call XWRITE (' Error reading text file', 5)
      rnum = 0
      status = -1

  50  continue

      rnum = rnum - 1

      close(lun)
      call frelun(lun)

      return
      end
