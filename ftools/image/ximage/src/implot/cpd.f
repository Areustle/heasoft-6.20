      SUBROUTINE cpd(cmdid,status)
      implicit none
c
c  Change and set plot device
c    I   cmdid  (i)  Command id
c    O   status (i)  error return
c
      INTEGER*4 cmdid, status

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
c
c local variables
c
      INTEGER*4 argc, LENACT, di
      character(80) prompt, ds
      character(8) devkey(1)
      character(20) opntype, reqtype
      character*(MAX_FILELEN) device, reply, opnfile, reqfile
      integer iot, iof, irt, irf
      logical leaveopen, readonly, global

      leaveopen = .FALSE.
      readonly = .FALSE.
      global = .TRUE.

      Status = 0

      CALL GPARL(Cmdid,'LEAVEOPEN',leaveopen,status)
      if ( status.ne.0 ) return
c
c get new plot device from argument
c
      call numcarg(cmdid,argc,status)
      if ( argc.eq.1 ) then
         call nextcarg(cmdid,device, MAX_FILELEN, status)
         call tclvars('default(device)', device, readonly, global,
     &                status)
      elseif ( argc.eq.0 ) then
c
c print the current plot device
c
         device = ' '
         call tclress('set default(device)', device, MAX_FILELEN,
     &                status)
         IF ( device.EQ.' ' ) THEN
            WRITE (ZWRite,'('' Plot device undefined'')')
         ELSE
            WRITE (ZWRite,'('' Current plot device is '',a)') 
     &          device(:LENACT(device))
         ENDIF
         CALL XWRITE(ZWRite,10)
c
c ask to insert the plot device
c
         CALL XWRITE('Device names are preceded by a slash',10)
         CALL XWRITE(' e.g. /te for tektronics',10)
         prompt = 'Enter plot device [? for a list] '
         CALL XCREAD(prompt,reply,status)
         IF ( status.NE.0 ) THEN
            Status = 1
            RETURN
         ENDIF
c
c if reply  = ? list available plot device
c           = empty do not change device
         DO WHILE ( reply(1:1) .EQ. '?' )
            call PGLDEV
            call XCREAD(prompt,reply,status)
            IF ( status.NE.0 ) THEN
               Status = 1
               RETURN
            ENDIF
         ENDDO
         IF ( reply(1:1) .NE. ' ' ) THEN
            call tclvars('default(device)', reply, readonly, global,
     &                   status)
         ELSE
            call XWRITE(' Plot device not changed',10)
            status = 1
         ENDIF
      else
         call xwrite(" Usage: cpd <device>", 5)
         status = -1
      endif
c
c if device = ? list available plot device
      if ( device.eq.'?' ) then
         call PGLDEV
         RETURN
      endif
c
c  Close device unless open is /XTK and requested is /XTK
c

      if ( status.eq.0 ) then
c
c  Find file and type of open and requested devices
c
         opntype = ' '
         opnfile = ' '
         reqtype = ' '
         reqfile = ' '
         irt = 0
         call pgqinf('TYPE', opntype, iot)
         call pgqinf('FILE', opnfile, iof)
         di = index(device, '/')
         if ( di.gt.0 ) then
            reqtype = device(di+1:LENACT(device))
            call UPC(reqtype)
            irt = LENACT(reqtype)
            if ( di.gt.1 ) then
               irf = di-1
               reqfile = device(1:irf)
            endif
         endif
c        *** If requested and open type are both XTK, 
c        *** automatically set leaveopen to true
         if ( opntype(1:iot).eq.'XTK' .and. reqtype(1:irt).eq.'XTK' ) 
     &      leaveopen = .TRUE.

c        *** If leaving open, make sure open device is */XTK
         if ( opntype(1:iot).ne.'XTK' .and. leaveopen) then
            call xwarn(' Only /xtk devices may be left open', 10)
            leaveopen = .FALSE.
         endif
         if ( .not.leaveopen ) call closepg()

c        *** Is requested device */XTK?
         devkey(1) = 'XTK'
         call matchkey(reqtype(1:irt), devkey, 1, di, status)
         if ( status.eq.0 ) then
c           *** If requested device an already open /XTK
c           *** Switch to it with pgslct
            write(ds,'(2a)') 'pgtk::id ', device(:LENACT(device))
            call tclresi(ds, di, status)
            if ( status.ne.0 ) return
            if ( di.gt.0 ) then
               call pgslct(di)
               call tclress('set pgtk::${pgtk::state}(mapid)', 
     &                      ds, 80, status)
               if ( status.eq.0 ) then
                  call setdismap(ds, status)
               endif
            endif
         else
            status = 0
         endif
      endif

      RETURN
      END
