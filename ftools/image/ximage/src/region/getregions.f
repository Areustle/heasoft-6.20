      SUBROUTINE getregions(cmdid, regtype, status)
      implicit none
c
c This routine work for the circle and box command 
c The boxreg and the circlereg have been combined in one 
c since the code is the same. 
c I  cmdid    (i)  command id
c I  regtype  (i)  1= circle 2=box 
c O  Status   (i)  status error 
c
      INTEGER*4 cmdid,regtype,status 

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
      INTEGER LENACT, n, lun, argc
      INTEGER color, lwidth, lstyle, excolor, exlwidth, exlstyle
      REAL*4 xpos, ypos, xpos1, ypos1, r, pnts(4)
      real*4 xcen, ycen, xwid, ywid
      CHARACTER*(MAX_FILELEN) filename, regionfile, fileimage 
      CHARACTER*(MAX_IDSTR) Mapid, wcsid
      LOGICAL isloaded, isdisplay, ismouse, cursor
      LOGICAL*4 excluded , new , displayonly, there
      integer ireg, numreg
c 
c initialize variable
      regionfile = ' '
      filename = ' '
      excluded = .FALSE.
      new = .FALSE.
      displayonly = .FALSE.
      color = -1
      lwidth = -1
      lstyle = -1
      excolor = -1
      exlwidth = -1
      exlstyle = -1
      xpos = 0.
      ypos = 0.
      xpos1 = 0.
      ypos1 = 0.
c
c  Retrieve argument as regionfile
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'REGIONFILE',regionfile,status)
      CALL GPARL(Cmdid,'EXCLUDED',excluded,status)
      CALL GPARL(Cmdid,'NEW',new,status)
      CALL GPARL(Cmdid,'DISPLAYONLY',displayonly,status)
      CALL GPARI(Cmdid,'COLOR',color,status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,status)
      CALL GPARI(Cmdid,'LSTYLE',lstyle,status)
      CALL GPARI(Cmdid,'EXCOLOR',excolor,status)
      CALL GPARI(Cmdid,'EXLWIDTH',exlwidth,status)
      CALL GPARI(Cmdid,'EXLSTYLE',exlstyle,status)
      if ( status.ne.0 ) return
c
c check if image loaded and display open
      Mapid='DIS'
      IF ( .not.isloaded(Mapid)) THEN
         CALL XWARN('No image loaded',5)
         status=-1
         RETURN
      ENDIF
      IF ( .not.isdisplay() ) then
         call XWARN(' No display',5)
         status=-1
         RETURN
      ENDIF
      IF ( .not.ismouse() .and. .not.displayonly ) then
         call XWARN(' Non-interactive device: DISPLAYONLY option set',5)
         displayonly = .TRUE.
      ENDIF
c
c  Line properties logic
c   ( If user doesn't set colors, include=3 exclude=2  )
c   ( If user sets include, exclude inherits values
c     unless exclude values are explicitly set          )
c
      if ( color.lt.0 ) then
         color = 3
      else
         if ( excolor.lt.0 ) excolor = color
      endif
      if ( excolor.lt.0 ) excolor = 2
      if ( lwidth.le.0 ) then
         lwidth = 1
      else
         if ( exlwidth.le.0 ) exlwidth = lwidth
      endif
      if ( lstyle.le.0 ) then
         lstyle = 1
      else
         if ( exlstyle.le.0 ) exlstyle = lstyle
      endif
c
      IF ( regionfile.eq.' ' ) then
         if ( filename.ne.' ' ) then
            regionfile = filename
         else
            call gheads(mapid, 'ROOT', fileimage, 0, status)
            regionfile = fileimage(1:LENACT(fileimage))//'.reg'
         endif
      ELSE
         call qustrip(regionfile)
      ENDIF
      INQUIRE(FILE=regionfile,EXIST=there)
      IF (.NOT.there)THEN
         call xtend(regionfile,'reg')
         IF ( new ) THEN
            CALL XWRITE('Create Regionfile: ',10)
            CALL XWRITE(regionfile,10)
         ELSE
            INQUIRE(FILE=regionfile,EXIST=there)
            if ( .not.there ) then
               CALL XWRITE('Regionfile not found: ',10)
               CALL XWRITE(regionfile,10)
               RETURN
            ENDIF
         ENDIF
      ENDIF
c
      IF ( .NOT.new ) THEN
         call gheads(mapid, 'WCSID', wcsid, 0, status)
         call setregwcs(wcsid, status)
         call xinitreg(regionfile, numreg, status)
         do ireg = 1, numreg
            call plotreg(ireg, color, lwidth, lstyle, excolor,
     &                   exlwidth, exlstyle, status)
         enddo
         call xfreereg(status)

      ENDIF
      IF ( displayonly .or. status.ne.0 ) goto 500
c 
      cursor = .TRUE.
      if ( regtype.eq.1 ) then
         CALL XWRITE(' Select center of circle',5)
         call inxypix(cursor, xpos, ypos)
         CALL XWRITE(' Select a point on the circle',5)
         call inxypix(cursor,xpos1,ypos1)
      else 
         CALL XWRITE(' Select box',5)
         call tclreslr('select box', pnts, n, 4, status)
         if ( status.ne.0 ) goto 500
         xpos = pnts(1)
         ypos = pnts(2)
         xpos1 = pnts(3)
         ypos1 = pnts(4)
      endif

      CALL GETLUN(lun)
      IF ( new ) THEN
         CALL OPENWR(lun,regionfile,'NEW',' ','L',0,0,status)
      ELSE
         CALL OPENWR(lun,regionfile,'OLD','E','L',0,0,status)
      ENDIF
      IF ( status.NE.0 ) THEN
         IF ( .not.new ) CALL OPENWR(lun,regionfile,'NEW',' ','L',0,0,
     &                               status)
         IF ( status.NE.0 ) THEN
            CALL XERROR(' Error opening '//regionfile,5)
         ELSE
            CALL XWARN(' Using new file '//regionfile,5)
         ENDIF
      ENDIF
      IF ( status.EQ.0 ) THEN
         IF(regtype.eq.1)THEN
            r = SQRT((xpos1-xpos)**2.+(ypos1-ypos)**2.)
            IF ( excluded ) THEN
               WRITE (lun,99002) xpos , ypos , r
            ELSE
               WRITE (lun,99001) xpos , ypos , r
            ENDIF
         ELSEIF (regtype.eq.2)THEN
            IF ( excluded ) THEN
               WRITE (lun,99004) xpos + (xpos1-xpos)/2. ,
     &         ypos + (ypos1-ypos)/2. , ABS(xpos1-xpos) ,
     &         ABS(ypos1-ypos)
            ELSE
               WRITE (lun,99003) xpos + (xpos1-xpos)/2. ,
     &         ypos + (ypos1-ypos)/2. , ABS(xpos1-xpos) ,
     &          ABS(ypos1-ypos)
            ENDIF
         ELSE
            call xwrite ('Unknown region', 10)
         ENDIF 
      ENDIF
      CLOSE (lun)
      CALL FRELUN(lun)
      IF (regtype.eq.1)THEN 
          if ( excluded ) then
             call jrncir(xpos,ypos,r,excolor,exlwidth,exlstyle)
          else
             call jrncir(xpos,ypos,r,color,lwidth,lstyle)
          endif
      ELSEIF(regtype.eq.2) THEN
          xcen = (xpos + xpos1)/2.
          ycen = (ypos + ypos1)/2.
          xwid = abs(xpos - xpos1)
          ywid = abs(ypos - ypos1)
          if ( excluded ) then
             call jrnbox(xcen,ycen,xwid,ywid,0.,
     &                   excolor,exlwidth,exlstyle)
          else
             call jrnbox(xcen,ycen,xwid,ywid,0.,
     &                   color,lwidth,lstyle)
          endif
      ENDIF  

 500  continue
      RETURN
99001 FORMAT (' CIRCLE(',f10.2,',',f10.2,',',f10.2,')')
99002 FORMAT ('-CIRCLE(',f10.2,',',f10.2,',',f10.2,')')
99003 FORMAT (' BOX(',f10.2,',',f10.2,',',f10.2,',',f10.2,',0)')
99004 FORMAT ('-BOX(',f10.2,',',f10.2,',',f10.2,',',f10.2,',0)')
      END
