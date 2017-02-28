      subroutine gridlab(x, y)
      implicit none
c
c  Label grid line
c
c  I  x/y      (r)  Location of grid line, viewport intersection (det)
c
      real*4 x, y

      include 'gridlab.inc'
      include 'greg.inc'
c
c  Local variables
c
      integer col1, col2, status
      character(20) str1, str2
      real*8 dd
      real*4 dr1(3), dr2(3)
      character(2) plottyp
      real frac, fd, fs
      character(120) string
      REAL chsize 
      real gallii,galbii
      INTEGER lenact
      character(1) bs

      bs = CHAR(92)

      chsize = 0.75
c
      if ( x.eq.gx2 ) then
         plottyp = 'RV'
         frac = (y-gy1)/(gy2-gy1)
         fd = 0.5
         fs = 0.0 
      elseif ( y.eq.gy2 ) then
         plottyp = 'T'
         frac = (x-gx1)/(gx2-gx1)
         fd = 0.5
         fs = 0.5
      else
         call XWRITE(' Invalid grid label location', 10)
         return
      endif      
      if ( frac.lt.0.0 .or. frac.gt.1.0 ) return 
      string = ' '
      
      if ( coorsys.eq.'CEL' ) then
         if ( axisnum.eq.1 ) then
            dd = 0.
            do while ( labval.ge.0 ) 
               labval = labval - 360.
            enddo
            do while ( labval.lt.0 ) 
               labval = labval + 360.
            enddo
            call cnv_radec(str1, str2, labval, dd, dr1, dr2, 2, 0,
     &                     status)
            col1 = index(str1,' ')
            col2 = col1 + 3
            string = str1(1:col1-1)//bs//'uh'//bs//'d'//
     &               str1(col1+1:col2-1)//bs//'um'//bs//'d'//
     &               str1(col2+1:LENACT(str1))//bs//'us'//bs//'d'
         elseif ( axisnum.eq.2 ) then
            if ( plottyp.eq.'T' ) fd = 1.5
            dd = 0.
            call cnv_radec(str1, str2, dd, labval, dr1, dr2, 2, 0,
     &                     status)
            col1 = index(str2,' ')
            col2 = col1 + 3
            string = str2(1:col1-1)//bs//'(0718)'//str2(col1+1:col2-1)//
     &               bs//'(0716)'//str2(col2+1:LENACT(str2))//bs//
     &               '(0717)'
         else
            call XWRITE(' Invalid axis number for grid line', 10)
            return
         endif
      elseif ( coorsys.eq.'GAL' ) then
         if ( axisnum.eq.1 ) then
            gallii = labval
            if ( gallii.lt.0 ) gallii = gallii + 360.0
            if ( gallii.gt.360.0 ) gallii = gallii - 360.0
            write (string,1010) gallii
1010        format(f6.2,'(LII)')
         elseif ( axisnum.eq.2 ) then
            if ( plottyp.eq.'T' ) then
               fd = 1.5
            else
               fd = 6.0
            endif
            galbii = labval
            write (string,1011) galbii
1011        format(f6.2,'(BII)')
         else
            call XWRITE(' Invalid axis number for grid line', 10)
            return
         endif
      else
         call XWRITE(' Invalid coordinate type for grid', 10)
         return
      endif

      call PGSAVE
      call PGSCH(chsize)
      call PGSCI(1)
      call PGSCF(1)
      call PGSLW(1)
      call PGMTXT(plottyp,fd,frac,fs,string)
      call PGUNSA
      
      RETURN
      END
 
