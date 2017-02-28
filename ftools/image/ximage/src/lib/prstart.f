      subroutine prstart (standalone, prname, version, status)
      implicit none
c
c  Prints startup message
c
c  I  standalone (l)  Interactive execution (y/n)
c  O  prname     (s)  Program name
c  O  version    (s)  Program version
c  O  status     (i)  Error flag (0=OK)
c
      logical standalone
      character*(*) prname, version
      integer*4 status
c
c  Externals
c
      include '../include/maxvals.inc'
      include '../include/sitedef.inc'
      include '../include/io.inc'

      integer*4 LENACT
c
c  Local variables
c
      integer*4 numskeys, maxskeys, hdtype
      parameter(maxskeys = 25)
      character(8) skeys(maxskeys)
      character(80) svals(maxskeys)
      character(80) card, value, comment

      character(1) bordchr
      integer*4 maxscrw, scrnwid
      parameter(maxscrw = 80)
      character*(maxscrw) line, bordmsg, msgstr
      integer*4 msgnum, tmplen, obeg
      character(20) bordsty, fmt, msgkey, bmsgjust, msgjust

      character*(MAX_FILELEN) msgfile
      character(255) tmpstr
      integer*4 lun, i
      real*8 dd

      status = 0
      msgfile = 'ximage.new'
c     Call xchaty (xanlib/xparse) to set default trmcht/logcht levels:
      call xchaty (10, 10)
      call PTEND(CXAn,CMAn,msgfile)
      INQUIRE (FILE=msgfile,IOSTAT=status)
      IF ( status.EQ.0 ) THEN
         call GETLUN(lun)
         call OPENWR(lun,msgfile,'old',' ',' ',0,1,status)
         IF ( status.NE.0 ) THEN
            CALL XWRITE('*WARNING*: Unable to open startup file',10)
            CLOSE (lun)
            CALL FRELUN(lun)
            RETURN
         ELSE
            numskeys = 0
            DO WHILE ( status.EQ.0 )
               tmpstr = ' '
               READ (lun,'(a)',IOSTAT=status) tmpstr
               if ( tmpstr(1:1).ne.'#' .and. tmpstr.ne.' ' ) then
                  call ftgthd(tmpstr, card, hdtype, status)
                  if ( hdtype.ne.0 ) status = -1
                  call ftpsvc(card, value, comment, status)
                  if ( status.eq.0 ) then
                     numskeys = numskeys + 1
                     if ( numskeys.gt.maxskeys ) then
                        call XWRITE(' Startup file too long', 10)
                        close(lun)
                        call frelun(lun)
                        return
                     endif
                     skeys(numskeys) = card(1:8)
                     call rmvchr(value, '''')
                     svals(numskeys) = value
                     tmplen = LENACT(value)
                     obeg = tmplen
                     do while ( value(tmplen:tmplen).eq.'&' )
                        READ (lun,'(a)',IOSTAT=status) tmpstr
                        call ftgthd(tmpstr, card, hdtype, status)
                        if ( hdtype.ne.0 ) status = -1
                        call ftpsvc(card, value, comment, status)
                        if ( card(1:8).ne.'CONTINUE' ) status = -1
                        if ( status.eq.0 ) then
                           call rmvchr(value, '''')
                           tmplen = LENACT(value)
                           if ( obeg+tmplen-1.le.80 ) then
                              svals(numskeys)(obeg:) = value(:tmplen)
                              obeg = obeg + tmplen - 1
                           else
                              status = -1
                           endif
                        endif
                        if ( status.ne.0 ) then
                           call XWRITE(' Error in long string', 10)
                           close(lun)
                           call frelun(lun)
                           return
                        endif
                     enddo
                  else
                     call XWRITE(' Bad format in startup file', 10)
                     close (lun)
                     call frelun(lun)
                     return
                  endif
               ENDIF
            ENDDO
            CLOSE (lun)
            CALL FRELUN(lun)
         ENDIF
      ENDIF
c
c  Append time and date to keyword list
c
      if ( numskeys+2.gt.maxskeys ) then
         call XWRITE(' Not enough start keys for date/time', 10)
         status = -1
         return
      endif
      numskeys = numskeys + 1
      skeys(numskeys) = 'DATE'
      svals(numskeys) = ' '
      call GETDAT(svals(numskeys))
      numskeys = numskeys + 1
      skeys(numskeys) = 'TIME'
      svals(numskeys) = ' '
      call GETTIM(svals(numskeys))
c
c  Get required parameters
c
      call matchkey('VERSION', skeys, numskeys, i, status)
      if ( status.ne.0 ) then
         call XWRITE(' VERSION not found', 10)
         return
      endif
      version = svals(i)(1:LEN(version))

      call matchkey('PRNAME', skeys, numskeys, i, status)
      if ( status.ne.0 ) then
         call XWRITE(' PRNAME not found', 10)
         return
      endif
      prname = svals(i)(1:LEN(prname))
      if ( .not.standalone ) return
c
c  Write the startup banner
c   (Error conditions don't stop the rest)
c
      call matchkey('SCRNWID', skeys, numskeys, i, status)
      if ( status.eq.0 ) then
         call strnum(svals(i), -4, dd, status)
         scrnwid = int(dd)
      endif
      if ( scrnwid.gt.maxscrw .or. status.ne.0 ) scrnwid = maxscrw
         
      bordsty = ' '
      call matchkey('BORDSTY', skeys, numskeys, i, status)
      if ( status.eq.0 ) then
         bordsty = svals(i)
         call upc(bordsty)
      endif

      bordchr = ' '
      call matchkey('BORDCHR', skeys, numskeys, i, status)
      if ( status.eq.0 ) bordchr = svals(i)(1:1)
       
      bordmsg = ' '
      call matchkey('BORDMSG', skeys, numskeys, i, status)
      if ( status.eq.0 ) then
         call streval(svals(i), skeys, svals, numskeys, bordmsg, 
     &                status)
      endif
      
      bmsgjust = 'center'
      call matchkey('BMSGJUST', skeys, numskeys, i, status)
      if ( status.eq.0 ) bmsgjust = svals(i)(:LEN(bmsgjust))

      if ( bordsty.eq.'BOX' .or. bordsty.eq.'LINE' ) then
         write(fmt, '(a,i5,3a)') '(',scrnwid, '(''', bordchr, '''))'
         call rmvblk(fmt)
         write(ZWRite, fmt)
         call xwrite (ZWRite, 10)
      endif

      line = ' '
      if ( bordsty.eq.'BOX' ) then
         call strjust(bordmsg, bmsgjust, scrnwid-4, line(3:scrnwid-2))
         line(1:1) = bordchr(1:1)
         line(scrnwid:scrnwid) = bordchr(1:1)
      else
         call strjust(bordmsg, bmsgjust, scrnwid, line)
      endif
      write(ZWRite, '(a)') line
      call xwrite (ZWRite, 10)

      if ( bordsty.eq.'BOX' .or. bordsty.eq.'LINE' ) then
         write(fmt, '(a,i5,3a)') '(',scrnwid, '(''', bordchr, '''))'
         call rmvblk(fmt)
         write(ZWRite, fmt)
         call xwrite (ZWRite, 10)
      endif
c
c  Write out other messages (MSG#)
c
      msgnum = 1
      status = 0
      do while ( status.eq.0 ) 
         write(msgkey, '(a,i3)') 'MSG', msgnum
         call rmvblk(msgkey)
         call matchkey(msgkey, skeys, numskeys, i, status)
         if ( status.eq.0 ) then
            call streval(svals(i), skeys, svals, numskeys, msgstr, 
     &                   status)
            if ( status.eq.0 ) then
               write(msgkey, '(a,i3)') 'MSGJUST', msgnum
               call rmvblk(msgkey)
               call matchkey(msgkey, skeys, numskeys, i, status)
               if ( status.eq.0 ) then
                  msgjust = svals(i)(:LEN(msgjust))
               else
                  msgjust = 'center'
               endif
               call strjust(msgstr, msgjust, scrnwid, line)
               write(ZWRite, '(a)') line
               call xwrite (ZWRite, 10)
            endif
            status = 0
            msgnum = msgnum + 1
         endif
      enddo

      status = 0

      return
      end
