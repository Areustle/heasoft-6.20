      subroutine mkcinit(rfile, gfile, bfile, outfile, cutmode, 
     &                   cut, base, tchat, lchat, clobber,status)
      implicit none
c
c
c INITialize files and parameters for program mkcolor
c read parameters in the parameters file.
c
c  O  rfile     (s)  red image file
c  O  gfile     (s)  green image file
c  O  bfile     (s)  blue image file
c  O  outfile   (s)  output file
c  O  cutmode   (i)  0=hist, 1=linear, 2=maxcol, 3=log
c  O  cut       (r)  Cut values
c  O  base      (i)  Base for color encoding
c  O  tchat     (i)  Chattiness terminal
c  O  lchat     (i)  Chattiness log 
c  O  status    (i)  Error return
c  O  clobber   (l)  Overwrite output file if it exists?
c
      CHARACTER*(*) rfile, gfile, bfile, outfile
      INTEGER*4 base, cutmode, status, tchat, lchat
      REAL*4 cut(6)
      LOGICAL clobber
c
c  Local variables
c
      integer i
      character(10) cutstr
      character(80) errm
c
      errm = ' '

c red image file name 
      CALL uclgst('rfile',rfile,status)
      errm= 'Unable to get RFILE'
      IF(status.NE.0) goto 999

c green image file name 
      CALL uclgst('gfile',gfile,status)
      errm= 'Unable to get GFILE'
      IF(status.NE.0) goto 999

c blue image file name 
      CALL uclgst('bfile',bfile,status)
      errm= 'Unable to get BFILE'
      IF(status.NE.0) goto 999

c
c output file name
      CALL uclgst('outfile',outfile,status)
      errm= 'Unable to get OUTFILE'
      IF(status.NE.0) goto 999
c
c base for encoding
      CALL uclgsi('base',base,status)
      errm ='Unable to get BASE'
      IF(status.NE.0) goto 999
c
c cut mode
c
c  0 = use histogram
c  1 = use linear partition scaled by intensity
c  2 = use linear partition scaled by maximum color
c  3 = use logarithmic partition scaled by intensity
c
      CALL uclgst('cutmode',cutstr,status)
      errm= 'Unable to get CUTMODE'
      IF(status.NE.0) goto 999
      call upc(cutstr)
      if ( cutstr(1:4).eq.'HIST' ) then
         cutmode = 0
      elseif ( cutstr(1:3).eq.'LIN' ) then
         cutmode = 1
      elseif ( cutstr(1:3).eq.'MAX' ) then
         cutmode = 2
      elseif ( cutstr(1:3).eq.'LOG' ) then
         cutmode = 3
      else
         errm = 'Invalid CUTMODE (Use HIST, LINEAR, LOG or MAXCOL)'
         status = -1
         goto 999
      endif
      do i = 1, 6
         cut(i) = -1.0
      enddo
      if ( cutmode.eq.1 .or. cutmode.eq.2 .or. cutmode.eq.3 ) then
         CALL uclgsr('rmin',cut(1),status)
         errm ='Unable to get RMIN'
         IF(status.NE.0) goto 999

         CALL uclgsr('rmax',cut(2),status)
         errm ='Unable to get RMAX'
         IF(status.NE.0) goto 999
         if ( cut(1).gt.cut(2) ) then
            errm = ' Bad cut value (rmin >= rmax)'
            status = -1
            goto 999
         endif
         
         CALL uclgsr('gmin',cut(3),status)
         errm ='Unable to get GMIN'
         IF(status.NE.0) goto 999

         CALL uclgsr('gmax',cut(4),status)
         errm ='Unable to get GMAX'
         IF(status.NE.0) goto 999
         if ( cut(3).gt.cut(4) ) then
            errm = ' Bad cut value (gmin >= gmax)'
            status = -1
            goto 999
         endif
         
         CALL uclgsr('bmin',cut(5),status)
         errm ='Unable to get BMIN'
         IF(status.NE.0) goto 999

         CALL uclgsr('bmax',cut(6),status)
         errm ='Unable to get BMAX'
         IF(status.NE.0) goto 999
         if ( cut(5).gt.cut(6) ) then
            errm = ' Bad cut value (bmin >= bmax)'
            status = -1
            goto 999
         endif
         
      endif
c
c terminal chat
      CALL uclgsi('tchat',tchat,status)
      errm ='Unable to get TCHAT'
      IF(status.NE.0) goto 999

c
c log chat
      CALL uclgsi('lchat',lchat,status)
      errm= 'Unable to get LCHAT'
      IF(status.NE.0) goto 999 

c
c clobber?
      CALL uclgsb('clobber',clobber,status)
      errm ='Unable to get CLOBBER'
      IF(status.NE.0) goto 999


      IF(status.eq.0) goto 1000
c
999   CONTINUE
      call xaerror(errm,5)
1000  CONTINUE
      RETURN
      END
