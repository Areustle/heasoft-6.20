      SUBROUTINE SCALE(Cmdid, Vpnum, Vpset, Status)
      IMPLICIT NONE
c
c  Plot scale for image
c
c  I  Cmdid  (i)  Command id
c  I  Vpnum  (i)  Current viewport number (for state only)
c  I  Vpset  (r)  Viewport to plot scale in relation to
c  O  Status (i)  Error flag (0=OK)
c
      INTEGER Cmdid, Vpnum, Status
      REAL*4 Vpset(4)

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 nxsub , nysub
      INTEGER*4 i , step , numlevs, ilev , LENACT
      INTEGER*4 mm , pp , nc , minci, maxci
      INTEGER*4 argc, ntest
      REAL*4 lev(MAX_NUMLEVS)
      REAL*8 bscale, bzero, datamin, datamax
      LOGICAL*4 horiz, isdisplay, plottxt

      REAL*4 min_inten , max_inten , inten
      REAL*4 xtick , ytick 
      REAL*4 min , max , xlef , xrig
      REAL*4 xpos , ypos
      REAL*4 sxvp1 , sxvp2 , syvp1 , syvp2
      REAL*4 xvp1 , xvp2 , yvp1 , yvp2
      REAL*4 xw1 , xw2 , yw1 , yw2
      REAL*4 xch, ych, xpch, ypch
 
      CHARACTER*(MAX_IDSTR) mapid
      character(16) xopt , yopt
      character(30) nlab
 
      INTEGER ndivs, color, lwidth
      INTEGER nside, bot, top, lft, rgt
      PARAMETER (bot=1, top=2, lft=3, rgt=4)
      REAL*4 csize, margin, thick, txtspace, lblspace
      LOGICAL curvp, vertical, bottom, toplog, left, right, lfrt
      character(10) font
      character(80) xlab

      mapid = 'DIS'

      color = 1
      csize = -1.0
      lwidth = 1
      font = ' '

      ndivs = 0
      xlab = ' '
      nside = bot
      curvp = .FALSE.
      vertical = .FALSE.
      bottom = .FALSE.
      toplog = .FALSE.
      left = .FALSE.
      right = .FALSE.
      margin = -999.0
      thick = 0.7
      txtspace = 0.5
      plottxt = .TRUE.
c
c  Retrieve argument as label
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)
      
      CALL GPARL(Cmdid,'CURVP',curvp,status)
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      CALL GPARS(Cmdid,'FONT',font,Status)
      CALL GPARS(Cmdid,'LABEL',xlab,status)
      CALL GPARI(Cmdid,'NO_OF_DIVISIONS',ndivs,status)
      CALL GPARL(Cmdid,'VERTICAL',vertical,status)
      CALL GPARL(Cmdid,'BOTTOM',bottom,status)
      CALL GPARL(Cmdid,'TOP',toplog,status)
      CALL GPARL(Cmdid,'LEFT',left,status)
      CALL GPARL(Cmdid,'RIGHT',right,status)
      CALL GPARR(Cmdid,'MARGIN',margin,Status)
      CALL GPARR(Cmdid,'THICKNESS',thick,Status)
      CALL GPARR(Cmdid,'SPACING',txtspace,Status)
      if ( status.ne.0 ) return
c
c  Check current status
c
      if ( .not.isdisplay() ) then
         call XWARN('No display',5)
         return
      endif
c
c Check if display/left, /right in use
c
      call tclresl('info exists pgtk::lfrt', lfrt, status)
c
c get level values
c
      call tclreslr('pgtk::curlevs', lev, numlevs, MAX_NUMLEVS, status)
      if ( status.ne.0 ) then
         call XWARN(' Failed to retrieve levels for scale...',10)
         return
      endif
      if ( numlevs.eq.0 ) then
         call XWARN(' No image levels for scale...',10)
         return
      endif
c
c  Process qualifiers
c
      call qustrip(xlab)
c
c    Text attributes
c
      call PGSAVE
      call text_pgstate(color, csize, lwidth, font)
      if ( csize.eq.0. ) plottxt = .FALSE.
c
c    Number of divisions to label
c
      if ( ndivs.ne.0 ) then
         if ( MOD(numlevs,ndivs).ne.0 ) then
            write(ZWRite,'(a,i2,a)') 
     &          ' NO_OF_DIVISIONS must divide number of levels (',
     &             numlevs,') evenly.'
            call XWRITE (ZWRite, 10)
            ndivs = 0
         endif
      endif
      if ( ndivs.eq.0 ) then
         ndivs = numlevs
         IF ( numlevs.GT.5 ) THEN
            i = numlevs
            DO WHILE ( i.GE.numlevs/5 )
               ntest = numlevs/i
               IF ( ntest*i.EQ.numlevs ) ndivs = ntest
               i = i - 1
            ENDDO
         ELSE
            ndivs = numlevs
         ENDIF
      endif
c
c    Position on viewport
c
      if ( vertical .or. right ) then
         nside = rgt
         horiz = .false.
      else if ( toplog ) then
         nside = top
         horiz = .true.
      else if ( left ) then
         nside = lft
         horiz = .false.
      else
         nside = bot
         horiz = .true.
      endif
c
c  Scale and bias
c
      call gheadd(mapid, 'BSCALE', bscale, 0, status)
      call gheadd(mapid, 'BZERO', bzero, 0, status)
      call gheadd(mapid, 'DATAMIN', datamin, 0, status)
      call gheadd(mapid, 'DATAMAX', datamax, 0, status)
      min_inten = datamin
      max_inten = datamax
 
c inquire viewport and world coordinates of the image plot
 
      CALL PGQVP(0,sxvp1,sxvp2,syvp1,syvp2)
      CALL PGQWIN(xw1,xw2,yw1,yw2)
      if ( lfrt .or. curvp .or. Vpset(1).lt.0.0 ) then
         CALL PGQVP(0,xvp1,xvp2,yvp1,yvp2)
      else
         xvp1 = Vpset(1)
         xvp2 = Vpset(2)
         yvp1 = Vpset(3)
         yvp2 = Vpset(4)
      endif
c
c If using vp config (and not left/right), assume no labels 
c  and close scale
c
      if ( Vpnum.gt.0 .and. margin.le.-999.0 .and.
     &     .not.lfrt ) margin = 1.0
c
c definitions for the scale plot
c
      min = 0.
      max = numlevs
c
c Setup viewport/window and label spacing for the scale plot
c
      call PGQCS(0, xch, ych)

      if ( nside.eq.top ) then
         if ( margin.le.-999.0 ) margin = 1.0
         CALL PGSVP(xvp1,xvp2,yvp2+margin*ych,yvp2+(margin+thick)*ych)
         CALL PGSWIN(min,max,0.,1.)
      else if ( nside.eq.bot ) then
         if ( margin.le.-999.0 ) margin = 4.0
         CALL PGSVP(xvp1,xvp2,yvp1-(margin+thick)*ych,yvp1-margin*ych)
         CALL PGSWIN(min,max,0.,1.)
      else if ( nside.eq.lft ) then
         if ( margin.le.-999.0 ) margin = 4.0
         CALL PGSVP(xvp1-(margin+thick)*xch,xvp1-margin*xch,yvp1,yvp2)
         CALL PGSWIN(0.,1.,min,max)
      else if ( nside.eq.rgt ) then
         if ( margin.le.-999.0 ) margin = 1.0
         CALL PGSVP(xvp2+margin*xch,xvp2+(margin+thick)*xch,yvp1,yvp2)
         CALL PGSWIN(0.,1.,min,max)
      else
         call XWARN (' Invalid SIDE - Abort...', 10 )
         call PGUNSA
         return
      endif
c
c plot colour scale
c
      xrig = 0.
      call PGQCIR(minci,maxci)
 
      DO 300 i = minci+1, maxci
         xlef = xrig
         xrig = i - minci
         CALL PGSCI(i)
         IF ( horiz ) THEN
            CALL PGRECT(xlef,xrig,0.,1.)
         ELSE
            CALL PGRECT(0.,1.,xlef,xrig)
         ENDIF
 300  CONTINUE
 
c plot frame
 
      IF ( horiz ) THEN
         yopt = 'ABC'
         ytick = 1.
         nysub = 1
 
         xopt = 'ABCT'
         xtick = (max-min)/ndivs
         nxsub = 1
      ELSE
         xopt = 'ABC'
         xtick = 1.
         nxsub = 1
 
         yopt = 'ABCT'
         ytick = (max-min)/ndivs
         nysub = 1
      ENDIF
      CALL PGSCI(color)
      CALL PGBOX(xopt,xtick,nxsub,yopt,ytick,nysub)
 
c write numeric labels
 
      ilev = 1
c  Initialize to avoid warning
      step = 0
c  --

      call PGQCS(4, xpch, ypch)
 
      if ( nside.eq.top ) then
         step = xtick
         xpos = min
         ypos = 1.0 + txtspace*ypch
      else if ( nside.eq.bot ) then
         step = xtick
         xpos = min
         ypos = -(0.7 + txtspace)*ypch
      else if ( nside.eq.lft ) then
         step = ytick
         ypos = min
         xpos = -txtspace*xpch
      else if ( nside.eq.rgt ) then
         step = ytick
         ypos = min
         xpos = 1.0 + (0.7 + txtspace)*xpch
      endif

      write(ZWRite,*) ' Scaling, Bias ', bscale, bzero
      call XWRITE(ZWRite, 15)

      DO 400 i = 1 , ndivs
         inten = lev(ilev)*bscale + bzero
         CALL DEC_TO_EXP(inten,mm,pp,2)
         CALL PGNUMB(mm,pp,0,nlab,nc)
         IF ( horiz ) THEN
            if ( plottxt ) CALL PGPTXT(xpos,ypos,0.,0.5,nlab(1:nc))
            xpos = xpos + step
         ELSE
            if ( plottxt ) CALL PGPTXT(xpos,ypos,90.,0.5,nlab(1:nc))
            ypos = ypos + step
         ENDIF
         ilev = ilev + step
 400  CONTINUE

c
c  Special code for offsetting last level
c
      max_inten = lev(numlevs)*bscale+bzero
      CALL DEC_TO_EXP(max_inten,mm,pp,2)
      CALL PGNUMB(mm,pp,0,nlab,nc)
 
      IF ( horiz ) THEN
         xpos = real(numlevs) - 1.
         if ( plottxt ) CALL PGPTXT(xpos,ypos,0.,0.5,nlab(1:nc))

      ELSE
         ypos = real(numlevs) - 1.
         if ( plottxt ) CALL PGPTEXT(xpos,ypos,90.,0.5,nlab(1:nc))
      ENDIF

c write text label

      if ( nside.eq.top ) then
         lblspace = 2.0*txtspace+0.7
         call PGMTXT('T',lblspace,0.5,0.5,xlab(1:LENACT(xlab)))
      else if ( nside.eq.bot ) then
         lblspace = 2.0*(txtspace+0.7)
         call PGMTXT('B',lblspace,0.5,0.5,xlab(1:LENACT(xlab)))
      else if ( nside.eq.lft ) then
         lblspace = 2.0*txtspace+0.7
         call PGMTXT('L',lblspace,0.5,0.5,xlab(1:LENACT(xlab)))
      else if ( nside.eq.rgt ) then
         lblspace = 2.0*(txtspace+0.7)
         call PGMTXT('R',lblspace,0.5,0.5,xlab(1:LENACT(xlab)))
      endif
c
c  Restore properties
c
      CALL PGSVP(sxvp1,sxvp2,syvp1,syvp2)
      CALL PGSWIN(xw1,xw2,yw1,yw2)
      call PGUNSA
c
c  Journal command
c
      call jrncmd(Cmdid, status)
 
      status = 0
      RETURN
      END
