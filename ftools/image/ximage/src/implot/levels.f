      subroutine levels(Cmdid,Map,Szx,Szy,Mapid,
     &                  Numload,Ldlevs,Status)

      implicit none

      include '../include/maxvals.inc'
      include '../include/io.inc'
c
c  Perform various function related to levels. 
c  The levels calculated from display can be viewed and saved using the 
c  SAVE and SHOW qualifiers. User defined levels can be imported by
c  reading from a file and using the LOAD qualifier (and FILE qualifier) 
c  If NUMBER is specified the number of levels is set to that value for all
c  the display unless change (Check the conflict with contour...)
c  
c  I  cmdid        (i) Command id
c  I  map          (r) Image map
c  I  szx/y        (i) Size of image map
c  I  mapid        (s) Map id string
c I/O numload      (i) Number of levels loaded
c I/O ldlevs       (r) Loaded levels
c  O  status       (i) Error flag (0=OK)
c
      integer Cmdid, Szx, Szy, Numload, Status
      real*4 Map(Szx,Szy), Ldlevs(*)
      character*(*) Mapid
c
c  Local variables
      INTEGER argc, i, numlevs, number, sigfig
      CHARACTER*(MAX_FILELEN) filename
      LOGICAL save, load, show, reset
      LOGICAL histo, linear, log, sroot, ISLOADED
      integer levels_flag, xmin, xmax, ymin, ymax
      real*4 RNULL, minlev, maxlev, r_black, r_white, val
      real*8 bscale, bzero, dd
      parameter(sigfig = 8)
      character(50) ds
      integer numlist, slen

      logical readonly, global

      reset = .FALSE.
      save = .FALSE.
      load = .FALSE.
      filename = ' '
      show = .FALSE.
      number = -1
      histo = .FALSE.
      linear = .FALSE.
      log = .FALSE.
      sroot = .FALSE.
      minlev = RNULL()
      maxlev = RNULL()

      Status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'RESET',reset,Status)
      CALL GPARL(Cmdid,'SAVE',save,Status)
      CALL GPARL(Cmdid,'LOAD',load,Status)
      CALL GPARS(Cmdid,'FILE',filename,Status)
      CALL GPARL(Cmdid,'SHOW',show,Status)
      CALL GPARI(Cmdid,'NUMBER',number,Status)
      CALL GPARL(Cmdid,'HISTO',histo,Status)
      CALL GPARL(Cmdid,'LINEAR',linear,Status)
      CALL GPARL(Cmdid,'LOG',log,Status)
      CALL GPARL(Cmdid,'SQRT',sroot,Status)
      CALL GPARR(Cmdid,'MINLEVEL',minlev,Status)
      CALL GPARR(Cmdid,'MAXLEVEL',maxlev,Status)
      CALL GPARLR(Cmdid,'LIST',Ldlevs,numlist,MAX_NUMLEVS,Status)
      if ( Status.ne.0 ) return
c
c enter filename if not specified
c
      IF ( (save .or. load) .and. filename.eq.' ' ) THEN
         DO WHILE ( .TRUE. )
            CALL XCREAD('Enter File Name :',filename,Status)
            IF ( Status.EQ.0 .AND. filename.NE.' ' ) THEN
               GOTO 200
            ENDIF
         ENDDO
      ENDIF
 200  CONTINUE
c
      call qustrip(filename)
c
c Get default number of levels
c 
      call tclresi('set default(numlevs)', numlevs, status)
      if ( reset ) number = DEF_NUMLEVS
c
c Levels 0= linear 1= histo 2=log 3= from file read and set in level
c command 10=sroot
      levels_flag = -1
      IF ( linear ) THEN
         levels_flag = 0
      ELSEIF ( histo ) THEN
         levels_flag = 1
      ELSEIF ( log ) THEN
         levels_flag = 2
      ELSEIF ( sroot ) THEN
         levels_flag = 10
      ENDIF
      if ( levels_flag.ge.0 .and. .not.isloaded(Mapid) ) then
         call XWRITE(" No image loaded to calculate levels from", 5)
         Status = -1
         return
      endif
c
      if ( number.gt.MAX_NUMLEVS ) then
         call xistr(MAX_NUMLEVS, ds, slen)
         write (ZWRite,'(2a)') 
     &    ' Number of levels must be between 1 and ', ds(1:slen)
         call XWRITE(ZWRite, 10)
         number = -1
         call XWRITE (' Ignoring NUMBER parameter...', 10)
      elseif ( number.gt.0 ) then
         numlevs = number
c
c  Only print 'Set to n levels' message when NOT calculating levels
c
         if ( levels_flag.lt.0 ) then
            call xistr(number, ds, slen)
            write(ZWRite,'(3a)') ' Set to ', ds(1:slen),' levels'
            call XWRITE (ZWRite, 10)
         endif
      endif
c
c  If no action requested, show levels
c
      if ( number.eq.-1 .and. levels_flag.lt.0 .and.
     &     .not.(save.or.load.or.numlist.gt.0) ) show = .TRUE.
      
      if ( save .and. load ) then
         call XWRITE (' SAVE and LOAD options are incompatible', 10)
         return
      endif
      if ( filename.ne.' ' .and. .not.save .and. .not.load ) then
         call XWRITE (' SAVE or LOAD required with filename', 10)
         return
      endif

      if ( levels_flag.ge.0 ) then

         xmin = 1
         xmax = Szx
         ymin = 1
         ymax = Szy
c
c read from the internal header the minimum photon/pixel
c to set the black
c
         call gheadd (Mapid, 'BSCALE', bscale, 0, status)
         call gheadd (Mapid, 'BZERO', bzero, 0, status)
         call gheadd (Mapid, 'DATAMIN', dd, 0, status)
         r_black = dd/bscale - bzero
c
c read from the internal header the minimum photon/pixel
c to set the white
c
         call gheadd (Mapid, 'DATAMAX', dd, 0, status)
         r_white = dd/bscale - bzero

         Numload = numlevs

         call calclevs(Map,Szx,Szy,xmin,xmax,ymin,ymax,
     &                 minlev,maxlev,r_black,r_white,levels_flag,
     &                 Numload,Ldlevs,status)
         if ( status.ne.0 ) return

      endif

      if ( save ) then
c
c open file to store levels value
c
         if ( Numload.le.0 ) then
            call XWRITE(' No levels set', 10)
            return
         endif
         call txinit(status)
         call txwrcom(filename, '! ', status)
         call txwrcom(filename, '! XIMAGE levels file', status)
         call txwrcom(filename, '! ', status)
         call txwrcol(filename, Ldlevs, Numload, status)
         call txwrfile(filename, status)

      else if ( load ) then
c
c  If numlevs set by user, go with it
c  Otherwise, let file determine number of levels
c
         call read_levels(filename,MAX_NUMLEVS,Numload,Ldlevs,Status)
         if ( Numload.eq.0 .or. Status.ne.0 ) then
            call XWRITE (' Failed to load any levels.',10)
            return
         endif

      else if ( numlist.gt.0 ) then
c
c  Levels given as list
c
         Numload = numlist

      endif
c
      if ( show ) then
         call xistr(numload, ds, slen)
         write(ZWRite,'(2a)') ' Current levels : ',ds(:slen)
         call RMVXBK(ZWRite)
         call XWRITE (ZWRite,10)
         DO i = 1, numload
            val = ldlevs(i)
            if ((abs(val).lt.100000.0 .and. abs(val).ge.0.01)
     &                                 .or. val.eq.0.0 ) then
               WRITE (zwrite,'(1x,''level'',i4,''= '',f11.4)')
     &                                      i, ldlevs(i)
            else
               WRITE (zwrite,'(1x,''level'',i4,''= '',1pe15.8)')
     &                                      i, ldlevs(i)
            endif
            CALL xwrite (zwrite,10)
         ENDDO
      endif
c
c  Export levels as list to Tcl
c
      readonly = .FALSE.
      global = .TRUE.
      call tclvarlr('levels(list)', Ldlevs, Numload, readonly, global,
     &              status)
c
c  Update number of levels variable
c
      call tclvari('default(numlevs)', numlevs, readonly, global,
     &             status)

      status = 0
      return
      end
