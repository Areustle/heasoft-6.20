      subroutine mkhinit(afile, bfile, outfile, sclmode, sclfact,
     &                   nbins, cut, tchat, lchat, clobber,status)
      implicit none
c
c
c INITialize files and parameters for program mkcolor
c read parameters in the parameters file.
c
c  O  afile     (s)  red image file
c  O  bfile     (s)  green image file
c  O  outfile   (s)  output file
c  O  sclmode   (i)  0=exp, 1=linear, 2=hist
c  O  sclfact   (r)  scaling factor
c  O  nbins     (i)  number of bins
c  O  cut       (i)  image value cut (1)=amin (2)=bmin
c  O  tchat     (i)  Chattiness terminal
c  O  lchat     (i)  Chattiness log 
c  O  status    (i)  Error return
c  O  clobber   (l)  Overwrite output file if it exists?
c
      CHARACTER*(*) afile, bfile, outfile
      INTEGER*4 sclmode, cut(2), status, tchat, lchat
      REAL sclfact
      LOGICAL clobber
c
c  Local variables
c
      INTEGER nbins
      character(10) sclstr
      character(80) errm
c
      errm = ' '

c image A file name 
      CALL uclgst('afile',afile,status)
      errm= 'Unable to get AFILE'
      IF(status.NE.0) goto 999

c image B file name 
      CALL uclgst('bfile',bfile,status)
      errm= 'Unable to get BFILE'
      IF(status.NE.0) goto 999
c
c output file name
      CALL uclgst('outfile',outfile,status)
      errm= 'Unable to get OUTFILE'
      IF(status.NE.0) goto 999
c
c scaling mode
c
c  0 = use exponent -> sclfact*exp(hardness)
c  1 = use linear   -> sclfact*(hardness+1)
c
      CALL uclgst('sclmode',sclstr,status)
      errm= 'Unable to get SCLMODE'
      IF(status.NE.0) goto 999
      call upc(sclstr)
      if ( sclstr(1:4).eq.'EXP' ) then
         sclmode = 0
      elseif ( sclstr(1:3).eq.'LIN' ) then
         sclmode = 1
      elseif ( sclstr(1:4).eq.'HIST' ) then
         sclmode = 2
      elseif ( sclstr(1:4).eq.'NONE' ) then
         sclmode = -1
      else
         errm = 'Invalid SCLMODE (Use EXP, LIN, or HIST)'
         status = -1
         goto 999
      endif

      if ( sclmode.eq.2 ) then
         CALL uclgsi('nbins',nbins,status)
         errm ='Unable to get NBINS'
         IF(status.NE.0) goto 999
      elseif ( sclmode.eq.0 .or. sclmode.eq.1 ) then
         CALL uclgsr('sclfact',sclfact,status)
         errm ='Unable to get SCLFACT'
         IF(status.NE.0) goto 999
      endif

      CALL uclgsi('amin',cut(1),status)
      errm ='Unable to get AMIN'
      IF(status.NE.0) goto 999

      CALL uclgsi('bmin',cut(2),status)
      errm ='Unable to get BMIN'
      IF(status.NE.0) goto 999
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
