      SUBROUTINE CCT(Cmdid, Status)
      IMPLICIT NONE
c
c  Change color table
c
c  I  Cmdid   (i)  Command id
c  O  Status  (i)  Error flag (0=OK)
c
      integer Cmdid, Status

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/colordef.inc'
      INCLUDE '../include/sitedef.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      integer jj, argc
      LOGICAL isdisplay, ismouse, ishardcopy
      LOGICAL cont, match, exact, casesen, unique
      INTEGER icoltab, dirsta, dirend, loopdir
      REAL xw1, xw2, yw1, yw2
      REAL b1, b2, c1, c2, sign, RNULL
      REAL x, y
      INTEGER PGCURS
      character(1) ch

      INTEGER*4 LENACT
      INTEGER i, icur, ierr
      character(80) pstr
      character(10) reply
      CHARACTER*(MAX_FILELEN) colfile, tempfile, curfile, colstart,
     &                        outfile
      LOGICAL break , there, inlist, ISRNULL
      LOGICAL loop , nobreak , user_col, setctab
      REAL contrast, bright, tmpcontra, tmpbright
      LOGICAL reverse, reset, cursor, bcreset
      real*8 dd
      logical readonly, global

      loop = .FALSE.
      ierr = 0
      nobreak = .FALSE.
      user_col = .FALSE.
      setctab = .FALSE.
      reset = .FALSE.
      bcreset = .FALSE.
      reverse = .FALSE.
      cursor = .FALSE.
      tmpcontra = RNULL()
      tmpbright = -1.0
      tempfile = ' '
      colfile = ' '
      curfile = ' '
      outfile = ' '
c
c  Retrieve argument as color table file
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.eq.1 ) then
         call nextcarg(cmdid,tempfile, MAX_FILELEN, status)
         argc = argc - 1
      endif
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'LOOP',loop,status)
      CALL GPARL(Cmdid,'NO_BREAK',nobreak,status)
      CALL GPARL(Cmdid,'USER_COL',user_col,status)
      CALL GPARL(Cmdid,'SET',setctab,status)
      CALL GPARL(Cmdid,'RESET',reset,status)
      CALL GPARR(Cmdid,'CONTRAST',tmpcontra,status)
      CALL GPARR(Cmdid,'BRIGHTNESS',tmpbright,status)
      CALL GPARL(Cmdid,'REVERSE',reverse,status)
      CALL GPARL(Cmdid,'CURSOR',cursor,status)
      CALL GPARL(Cmdid,'BCRESET',bcreset,status)
      CALL GPARS(Cmdid,'OUTFILE',outfile,status)
      if ( status.ne.0 ) return
c
c    Reset halts everything... Nothing more needs to be done
c
      IF (reset) THEN
         colstart = ZTAb_name(1)
         call PTEND(CXAn, CPAr, colstart)
         call read_coltab(CTAB_DEFAULT,colstart)
         if ( .not.ishardcopy() ) then
            call copy_coltab(CTAB_DEFAULT)
            call info_coltab(CTAB_DEFAULT,curfile,contrast,bright)
            goto 600
         endif
         return
      ENDIF
c
c  If a list of color tables is all that is needed
c
      IF ( tempfile.eq.'?' ) then
         call list_col_tab
         return
      ENDIF
c
c  If outfile, write the file and return
c
      IF ( outfile.ne.' ' ) then
         call qustrip(outfile)
         call wrcoltab(outfile, status)
         return
      ENDIF
c
c  Check qualifiers
c
      if ( cursor .and. .not.ismouse() ) then
         call XWRITE(' Non-interactive device: CURSOR option off',10)
         cursor = .false.
      endif

      icoltab = CTAB_CURRENT

      if ( ishardcopy() ) then
         if ( setctab ) then
            icoltab = CTAB_DEFAULT
         elseif ( tempfile.ne.' ' ) then
            call XWRITE (
     &         ' Unable to change color table on this device', 10)
            return
         endif
      endif

      if ( .not.isdisplay() ) then
         if ( setctab ) then
            icoltab = CTAB_DEFAULT
         elseif ( tempfile.ne.' ' ) then
            call XWRITE (
     &         ' Must display an image to try color table', 10)
            return
         else
         endif
      endif

      IF ( nobreak ) THEN
         if ( .not.loop ) then
            call XWRITE('NO_BREAK requires LOOP qualifier',10)
            return
         endif
         break = .FALSE.
      ELSE
         break = .TRUE.
      ENDIF
c
c  Adjust brightness/contrast
c
      call info_coltab(icoltab,curfile,contrast,bright)
      if ( .not.ISRNULL(tmpcontra) ) then
         contrast = tmpcontra
      endif
      if ( tmpbright.ge.0.0 ) then
         bright = tmpbright
      endif
      if ( bcreset ) then
         contrast = 1.0
         bright = 0.5
      else if ( reverse ) then
         contrast = -contrast
         bright = 1.0 - bright
      endif
      call mod_coltab(icoltab,contrast,bright)
      call refresh_coltab
c
c  Change color table
c
c  Loop thru all available tables
c
      IF ( loop ) THEN
         i = 1
         icur = 0
         match = .FALSE.
         casesen = .TRUE.
         do while ( i.le.ZTAbnum .and. .not.match )
            tempfile = ZTAb_name(i)
            CALL PTEND(Cxan,Cpar,tempfile)
            call FTCMPS(curfile,tempfile,casesen,match,exact)
            if ( match ) icur = i
            i = i + 1
         enddo

         i = icur + 1
         if ( i.gt.ZTAbnum ) i = 1

         unique = .TRUE.
         DO WHILE ( unique )

            if ( i.eq.icur ) unique = .FALSE.

            if ( i.eq.0 ) then
               tempfile = curfile
               WRITE (*,*) 'Color table ' , tempfile(:LENACT(tempfile))
               call xtend(tempfile,'tab')
               colfile = tempfile
            else
               tempfile = ZTAb_name(i)
               WRITE (*,*) 'Color table ' , tempfile(:LENACT(tempfile))
               call xtend(tempfile,'tab')
               colfile = tempfile
               call DIRPOS(colfile,dirsta,dirend)
               if ( dirend.eq.0 ) CALL PTEND(Cxan,Cpar,colfile)
            endif

            call read_coltab (icoltab,colfile)
            call mod_coltab (icoltab,contrast,bright)
            call refresh_coltab
            loopdir = 1
            IF ( break .and. unique ) THEN
               reply = ' '
               pstr = ' <CR> continues, "b" goes back, '
     &                 //'any other value exits>'
               call PROMPT(pstr,0)
               read(*,'(a)') reply
               CALL UPC(reply)
               IF  (reply.ne.' ' .and. reply(1:1).ne.'B') THEN
                  ierr = 1
                  goto 500
               ENDIF
               if ( reply(1:1).eq.'B' ) loopdir = -1
            ENDIF

            i = i + loopdir
c
c  Loop boundary cases
c
            if ( i.lt.1 ) then
               if ( icur.eq.0 ) then
                  i = 0
               else
                  i = ZTAbnum
               endif
            endif
            if ( i.gt.ZTAbnum ) then
               if ( icur.eq.0 ) then
                  i = 0
               else
                  i = 1
               endif
            endif
         ENDDO
      ELSE
c
c  Check to see if it's in color table list
c
         IF ( tempfile.NE.' ' ) THEN
            inlist = .FALSE.
            if ( .not.user_col ) then
               call xtend(tempfile,'tab')
               call gmatch(ZTAb_name,ZTAbnum,tempfile,jj,Status)
               if ( Status.eq.0 ) inlist = .TRUE.
               Status = 0
            endif
c
            INQUIRE (FILE=tempfile,EXIST=there)
            if ( .not.there ) then
               call xtend(tempfile,'tab')
               INQUIRE (FILE=tempfile,EXIST=there)
            endif
            colfile = tempfile
            IF ( there .and. .not.inlist ) THEN
               CALL XWRITE(' Color Table from user',5)
            ELSE
               CALL PTEND(Cxan,Cpar,colfile)
               INQUIRE (FILE=colfile,EXIST=there)
               IF ( .not.there ) THEN
                  CALL XWRITE(' Color Table not found',5)
                  colfile = ' '
               ENDIF
            ENDIF
            if ( colfile.ne.' ' ) then
               call read_coltab(icoltab,colfile)
               call mod_coltab(icoltab,contrast,bright)
            endif
         ENDIF
      ENDIF
c
c  Set brightness/contrast interactively
c
      if ( cursor ) then

         call xwrite(' Select a point on the image with left button.', 
     &                 10)
         call xwrite(' Brightness increases from left to right.', 10)
         call xwrite(' Contrast increases from bottom to top.', 10)
         call xwrite(' Center button resets, right button ends.', 10)

         call pgqwin (xw1, xw2, yw1, yw2)

         x = 0.5
         y = 1.0
         b1 = 0.0
         b2 = 1.0
         c1 = 0.0
         c2 = 2.0
         sign = +1.0

         call pgswin (b1, b2, c1, c2)

         cont = .true.
         if ( contrast.lt.0 ) sign = -1.0
         do while (cont) 
            ierr = pgcurs(x, y, ch)
            if ( ch.eq.CHAR(0) .or. ch.eq.'x' .or. ch.eq.'X' ) then
               call pgswin (xw1, xw2, yw1, yw2)
               return
            else if ( ch.eq.'a' .or. ch.eq.'A' ) then
               bright = MAX(b1, MIN(b2, x))
               if ( y.lt.1.0 ) then
                  contrast = sign*SQRT(MAX(c1, y))
               else
                  contrast = sign*MIN(c2,y)**2
               endif
            else if ( ch.eq.'d' .or. ch.eq.'D' ) then
               bright = 0.5
               contrast = sign
            endif
            write(ZWRite,99000) bright, contrast
            call xwrite(ZWRite, 10)
            call mod_coltab(CTAB_CURRENT,contrast,bright)
            call refresh_coltab
         enddo

         call pgswin (xw1, xw2, yw1, yw2)

      endif

 500  if ( setctab .and. icoltab.eq.CTAB_CURRENT ) then 
         call copy_coltab(CTAB_CURRENT)
      endif
 600  call refresh_coltab
c
c  Set cct variable in Tcl
c
      readonly = .TRUE.
      global = .FALSE.
      call info_coltab(CTAB_CURRENT,tempfile,contrast,bright)
      call tclvars('cct(current)', tempfile, readonly, global, status)
      call tclvarr('cct(bright)', bright, readonly, global, status)
      call tclvarr('cct(contrast)', contrast, readonly, global, status)
      call info_coltab(CTAB_DEFAULT,tempfile,contrast,bright)
      call tclvars('cct(default)', tempfile, readonly, global, status)
      call tclvarr('cct(defbri)', bright, readonly, global, status)
      call tclvarr('cct(defcon)', contrast, readonly, global, status)
 
      status = 0
      RETURN
99000 format (' Brightness = ',f6.1,', Contrast = ',f6.1)
      END
