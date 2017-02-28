      subroutine cnv_radec (str_ra, str_dec, deg_ra, deg_dec, 
     &                      ary_ra, ary_dec, mode, decp, ierr)
      implicit none
c
c  Mode determines input format of RA/Dec.  This routine converts
c  the input format into the other two formats.
c
c  I/O  str_ra  (c)  RA in string format
c  I/O  str_dec (c)  Dec in string format
c  I/O  deg_ra  (d)  RA in decimal degrees
c  I/O  deg_dec (d)  Dec in decimal degress
c  I/O  ary_ra  (r)  RA in hr, mn, sec array
c  I/O  ary_dec (r)  Dec in deg, mn, sec, array
c   I   mode    (i)  Mode of conversion (1=str_* as input, 
c                    2=deg_* as input, 3=ary_* as input)
c   I   decp    (i)  Number of decimal places to print in str_ra/dec
c                    (only used in mode 2 and 3)
c   O   ierr    (i)  Error flag (0=OK)
c
      character*(*) str_ra, str_dec
      real*8 deg_ra, deg_dec
      real*4 ary_ra(3), ary_dec(3)
      integer mode, decp, ierr
c
c  Local variables
c
      character(1) sign
      integer*4 i, hr, deg, mn, slen, LENACT
      real*4 sec
      character(30) dumra, dumdec
      character(10) secstr, secfmt
      logical ISITDG
 
      ierr = 0
      write(secfmt, '(a,i2,a,i2,a)') '(f',decp+3,'.',decp,')'
      call RMVBLK(secfmt)
 
      if ( mode.eq.1 ) then
         if ( str_ra.ne.' ' ) then
            dumra = str_ra(:MIN(lenact(str_ra),len(dumra)))
            call qustrip(dumra)
            do i = 1, lenact(dumra)
               if ( dumra(i:i).eq.':' ) dumra(i:i) = ' '
               if ( dumra(i:i).eq.'h' ) dumra(i:i) = ' '
               if ( dumra(i:i).eq.'m' ) dumra(i:i) = ' '
               if ( dumra(i:i).eq.'s' ) dumra(i:i) = ' '
            enddo
            IF ( .NOT.ISITDG(dumra) ) THEN
               CALL XRDANG(dumra,hr,mn,sec)
               CALL CHRA(deg_ra,hr,mn,sec,0) 
            ELSE
               CALL STRNUM(dumra,8,deg_ra,ierr)
               CALL CHRA(deg_ra,hr,mn,sec,1) 
               IF ( ierr.NE.0 ) RETURN
            ENDIF
            ary_ra(1) = hr
            ary_ra(2) = mn 
            ary_ra(3) = sec 
         endif
         if ( str_dec.ne.' ' ) then
            dumdec = str_dec(:MIN(lenact(str_dec),len(dumdec)))
            call qustrip(dumdec)
            do i = 1, lenact(dumdec)
               if ( dumdec(i:i).eq.':' ) dumdec(i:i) = ' '
            enddo
            IF ( .NOT.ISITDG(dumdec) ) THEN
               CALL XRDANG(dumdec,deg,mn,sec)
               CALL CHDEC(deg_dec,deg,mn,sec,0)
            ELSE
               CALL STRNUM(dumdec,8,deg_dec,ierr)
               CALL CHDEC(deg_dec,deg,mn,sec,1)
               IF ( ierr.NE.0 ) RETURN
            ENDIF
            ary_dec(1) = deg
            ary_dec(2) = mn 
            ary_dec(3) = sec 
         endif
      elseif ( mode.eq.2 ) then
         if ( deg_ra.ne.-100 ) then
            CALL CHRA(deg_ra,hr,mn,sec,1) 
            write(secstr, secfmt) sec
            call rmvblk(secstr)
            slen = LENACT(secstr)
            if ( index(secstr,'.').eq.2 ) then
               secstr = '0'//secstr(:slen)
               slen = slen + 1
            endif
            if ( secstr(slen:slen).eq.'.' ) slen = slen - 1
            write(str_ra,901) hr,mn,secstr(1:slen)
            ary_ra(1) = hr
            ary_ra(2) = mn 
            ary_ra(3) = sec 
         endif
         if ( deg_dec.ne.-100 ) then
            CALL CHDEC(deg_dec,deg,mn,sec,1)
            if ( deg_dec.lt.0 ) then
               sign = '-'
               deg = abs(deg)
               mn = abs(mn)
               sec = abs(sec)
            else
               sign = '+'
            endif
            write(secstr, secfmt) sec
            call rmvblk(secstr)
            slen = LENACT(secstr)
            if ( index(secstr,'.').eq.2 ) then
               secstr = '0'//secstr(:slen)
               slen = slen + 1
            endif
            if ( secstr(slen:slen).eq.'.' ) slen = slen - 1
            write(str_dec,902) sign,deg,mn,secstr(1:slen)
            ary_dec(1) = deg
            ary_dec(2) = mn 
            ary_dec(3) = sec 
         endif
      elseif ( mode.eq.3 ) then
         hr = int(ary_ra(1))
         mn = int(ary_ra(2))
         sec = ary_ra(3)
         if ( hr.ne.-100 ) then
            CALL CHRA(deg_ra,hr,mn,sec,0) 
            write(secstr, secfmt) sec
            call rmvblk(secstr)
            slen = LENACT(secstr)
            if ( index(secstr,'.').eq.2 ) then
               secstr = '0'//secstr(:slen)
               slen = slen + 1
            endif
            if ( secstr(slen:slen).eq.'.' ) slen = slen - 1
            write(str_ra,901) hr,mn,secstr(1:slen)
         endif
         deg = int(ary_dec(1))
         mn = int(ary_dec(2))
         sec = ary_dec(3)
         if ( deg.ne.-100 ) then
            CALL CHDEC(deg_dec,deg,mn,sec,0)
            if ( deg_dec.lt.0 ) then
               sign = '-'
               deg = abs(deg)
               mn = abs(mn)
               sec = abs(sec)
            else
               sign = '+'
            endif
            write(secstr, secfmt) sec
            call rmvblk(secstr)
            slen = LENACT(secstr)
            if ( index(secstr,'.').eq.2 ) then
               secstr = '0'//secstr(:slen)
               slen = slen + 1
            endif
            if ( secstr(slen:slen).eq.'.' ) slen = slen - 1
            write(str_dec,902) sign,deg,mn,secstr(1:slen)
         endif
      else
         ierr = 99
      endif
         
      return
 901  format (i2.2,1x,i2.2,1x,a)
 902  format (a,i2.2,1x,i2.2,1x,a)
c901  format (1x,i2.2,i3.2,f6.2)
c902  format (a,i2.2,i3.2,f6.2)
      end
