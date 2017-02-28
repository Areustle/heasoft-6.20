C****************************************************************************
C SELECTOR TASK:
C      sky2xy
C
C FILE:
C      sky2xy.f
C
C DESCRIPTION:
C       This routine takes an input image or table and input celestial
C       coordinates and returns the pixel values.
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC/Hughes STX
C       February, 1995
C
C MODIFICATION HISTORY:
C       Jeff Guerber, RSTX/GSFC Oct. 1998   Dumps with Oedipus Fitsio because
C       `keyword', used for getting TCTYPn, was never declared as a string!
C       Fixed, and added `implicit none's.  Also use `comment' consistently
C       for all ftgky*.
C
C NOTES:
C      sky2xy supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C     infile    - string - input FITS file names
C     xsky      - double - input X sky coordinate value
C     ysky      - double - input Y sky coordinate value
C     xcol      - string - input X coordinate column name
C     ycol      - string - input Y coordinate column name
C     xpix      - double - output X coordinate pixel value
C     ypix      - double - output Y coordinate pixel value
C     sensecase - logical          - be case sensitive about column names
C     tchat     - integer          - terminal chattiness level
C     lchat     - integer          - logfile chattiness level
C
C CALLED ROUTINES:
C
C****************************************************************************

      subroutine sky2xy

      implicit none
      character(160) infile
      character(40) xcol, ycol
      logical sensecase
      integer tchat, lchat
      double precision xsky, ysky

      integer status

      character(40) taskname
      common /task/ taskname

      taskname = 'SKY2XY v1.1'

c initialize variables
      infile = ' '

      status = 0

C  zero out the FITSIO error stack
      call ftcmsg

C  get the parameters from the par file
      call gsky2xy (infile, xsky, ysky, xcol, ycol, sensecase, tchat,
     &     lchat, status)
      if (status .ne. 0) goto 999

C set up chattiness.  Log file is set using FLOGFILE environment variable
      call xchaty (tchat, lchat)

C calculate the transformation
      call csky2xy (infile, xsky, ysky, xcol, ycol, sensecase, status)

 999  if (status .ne. 0) call fcerrm (status)

      return
      end


C****************************************************************************
C SUBROUTINE:
C      gsky2xy
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC / Hughes STX
C       February, 1995
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gsky2xy uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gsky2xy (infile, xsky, ysky, xcol, ycol, sensecase, tchat,
C     &     lchat, status)
C
C ARGUMENTS:
C     infile    - string - input FITS file names
C     xsky      - double - input X sky coordinate value
C     ysky      - double - input Y sky coordinate value
C     xcol      - string - input X coordinate column name
C     ycol      - string - input Y coordinate column name
C     sensecase - logical          - be case sensitive about column names
C     tchat     - integer          - terminal chattiness level
C     lchat     - integer          - logfile chattiness level
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C
C****************************************************************************

      subroutine gsky2xy (infile, xsky, ysky, xcol, ycol, sensecase,
     &     tchat, lchat, status)

      implicit none
      character*(*) infile, xcol, ycol
      integer tchat, lchat, status
      logical sensecase
      double precision xsky, ysky

      character(80) context

C  get the name of the input FITS file
      call uclgst ('infile', infile, status)
      if (status .ne. 0) then
         context = 'could not get INFILE parameter'
         call fcerr (context)
         goto 999
      endif

C  get X pixel value
      call uclgsd ('xsky', xsky, status)
      if (status .ne. 0) then
         context = 'could not get XSKY parameter'
         call fcerr(context)
         goto 999
      endif

C  get Y pixel value
      call uclgsd ('ysky', ysky, status)
      if (status .ne. 0) then
         context = 'could not get YSKY parameter'
         call fcerr(context)
         goto 999
      endif

C  get the xcol to output
      call uclgst ('xcol', xcol, status)
      if (status .ne. 0) then
         context = 'could not get XCOL parameter'
         call fcerr(context)
         goto 999
      endif

C  get the ycol to output
      call uclgst ('ycol', ycol, status)
      if (status .ne. 0) then
         context = 'could not get YCOL parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to be case sensistive about column names
       call uclgsb ('sensecase', sensecase, status)
       if (status .ne. 0) then
           context = 'could not get SENSECASE parameter'
           call fcerr(context)
           goto 999
       endif

C  get whether to be case sensistive about column names
       call uclgsi ('tchat', tchat, status)
       if (status .ne. 0) then
           context = 'could not get TCHAT parameter'
           call fcerr(context)
           goto 999
       endif

C  get whether to be case sensistive about column names
       call uclgsi ('lchat', lchat, status)
       if (status .ne. 0) then
           context = 'could not get LCHAT parameter'
           call fcerr(context)
           goto 999
       endif

 999  continue
      if (status .ne. 0)  call fcerrm(status)

      return
      end


C****************************************************************************
C SUBROUTINE:
C      csky2xy
C
C DESCRIPTION:
C      Calculate the sky coordinates
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC / Hughes STX
C       February, 1995
C
C MODIFICATION HISTORY:
c       Banashree M Seifert ( March 17, 1997)
c          . the par file updated so that it writes the parameters on
c            completion of the task.  The parameters are
c            coord, cdelt1, cdelt2, crpix1, crpix2, crval1, crval2,
c            radecsys, equinox, rot and valid
C
C NOTES:
C
C USAGE:
C      call csky2xy (infile, xsky, ysky, xcol, ycol, sensecase, status)
C
C ARGUMENTS:
C     infile    - string - input FITS file names
C     xsky      - double - input X sky coordinate value
C     ysky      - double - input Y sky coordinate value
C     xcol      - string - input X coordinate column name
C     ycol      - string - input Y coordinate column name
C     sensecase - logical          - be case sensitive about column names
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C*************************************************************************

      subroutine csky2xy (infile, xsky, ysky, xcol, ycol, sensecase,
     &     status)

c -------------------------------------------------------------------
      implicit none
      character*(*) infile, xcol, ycol
      double precision xsky, ysky
      logical sensecase
      integer status

      character(80) context, comment
      character(160) filename
      character(16) coordtype
      integer extno, iunit, block, htype, xcolno, ycolno, fstat
      double precision xrval, yrval, xrpix, yrpix, xinc, yinc, rot
      double precision xpix, ypix

      real equinox
      character(8) radecsys
      character(40) xtype, ytype
      character(10) keyword
      logical valid
c ------------------------------------------------------------------
c initialise valid parameter to false
      status=0
      valid=.false.
      call uclpsb('valid', valid, status)
      if(status .ne. 0) then
         context= ' Error initialising valid parameter'
         call fcerr (context)
         goto 998
      endif


C open the input file
      call fcpars (infile, filename, extno, status)
      if (extno .eq. -99) extno = 0

      call ftgiou (iunit, status)
      call ftopen (iunit, filename, 0, block, status)
      if (status .ne. 0) then
         context = ' Error opening input file ' // filename
         call fcerr (context)
         goto 998
      endif

C move to the requested extension
      call ftmahd (iunit, extno+1, htype, status)

c Look for RADECSYS/RADESYS & EQUINOX keywords (optional)

      status=0
      call ftgkys(iunit,'RADECSYS',radecsys,comment,status)
      if (status .ne. 0) then
         status = 0
         call ftgkys(iunit,'RADESYS',radecsys,comment,status)
         if (status .ne. 0) then
            status = 0
            radecsys = ' '
         endif
      endif

      call ftgkye(iunit,'EQUINOX',equinox,comment,status)
      if (status .ne. 0) then
         status = 0
         equinox = 0.0
      endif

C read the appropriate values
      if (htype .eq. 0) then
C        is an image
         call ftgics (iunit, xrval, yrval, xrpix, yrpix, xinc, yinc,
     &        rot, coordtype, status)

         call ftgkys (iunit, 'CTYPE1', xtype, comment, status)
         if (status .eq. 202) then
            call ftcmsg
            status = 0
            xtype = 'NONE'
         endif
         call ftgkys (iunit, 'CTYPE2', ytype, comment, status)
         if (status .eq. 202) then
            call ftcmsg
            status = 0
            ytype = 'NONE'
         endif
      else
C first get the X and Y column numbers
         call ftgcno (iunit, sensecase, xcol, xcolno, status)
         if (status .ne. 0) then
            context = ' Requested column does not exist ' // xcol
            call fcerr (context)
            goto 998
         endif
         call ftgcno (iunit, sensecase, ycol, ycolno, status)
         if (status .ne. 0) then
            context = ' Requested column does not exist ' // ycol
            call fcerr (context)
            goto 998
         endif

C get the TCTYP keywords
         call ftkeyn ('TCTYP', xcolno, keyword, status)
         call ftgkys (iunit, keyword, xtype, comment, status)
         if (status .eq. 202) then
            call ftcmsg
            status = 0
            xtype = 'NONE'
         endif
         call ftkeyn ('TCTYP', ycolno, keyword, status)
         call ftgkys (iunit, keyword, ytype, comment, status)
         if (status .eq. 202) then
            call ftcmsg
            status = 0
            ytype = 'NONE'
         endif

C get the coordinates
         call ftgtcs (iunit, xcolno, ycolno, xrval, yrval, xrpix, yrpix,
     &        xinc, yinc, rot, coordtype, status)
      endif
      if (status .ne. 0) then
         write (context,'(a41,i3)')
     &        ' Error reading WCS keywords in extension ', extno
         call fcerr (context)
         goto 998
      endif

C calculate the celestial coordinates
      call ftxypx (xsky, ysky, xrval, yrval, xrpix, yrpix, xinc, yinc,
     &     rot, coordtype, xpix, ypix, status)
      if (status .ne. 0) then
         context = ' Error transforming to pixel coordinate'
         call fcerr (context)
         goto 998
      endif

C write the results to screen and par file
      write (context, 1000) xsky, ysky
 1000 format (' Input sky coordinates: ',1pe23.13,',',e23.13)
      call xwrite (context, 5)
      write (context, 1001) xpix, ypix
 1001 format (' Output pixel coordinates: ',1pe23.13,',',e23.13)
      call xwrite (context, 5)


      context = ' Error updating xpix in par file'
      status=0
      call uclpsr ('xpix', real(xpix), status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating ypix in par file'
      status=0
      call uclpsr ('ypix', real(ypix), status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating radecsys in par file'
      status=0
      call uclpst ('radecsys', radecsys, status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating ctype1 in par file'
      status=0
      call uclpst ('ctype1', xtype, status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating ctype2 in par file'
      status=0
      call uclpst ('ctype2', ytype, status)
      if (status .ne. 0) call fcerr (context)


      context = ' Error updating equinox in par file'
      status=0
      call uclpsr ('equinox', equinox, status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating xref value in par file'
      status=0
      call uclpsr ('crval1', real(xrval), status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating reference xpix in par file'
      status=0
      call uclpsr ('crpix1', real(xrpix), status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating reference ypix in par file'
      status=0
      call uclpsr ('crpix2', real(yrpix), status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating yref value in par file'
      status=0
      call uclpsr ('crval2', real(yrval), status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating xincrement in par file'
      status=0
      call uclpsr ('cdelt1', real(xinc), status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating yincrement in par file'
      status=0
      call uclpsr ('cdelt2', real(yinc), status)
      if (status .ne. 0) call fcerr (context)

      context = ' Error updating rot in par file'
      status=0
      call uclpsr ('rot', real(rot), status)
      if (status .ne. 0) call fcerr (context)

 998  if (status .eq. 0) then
          valid=.true.
          call uclpsb('valid', valid, status)
      endif

      fstat = 0
      call ftclos (iunit, fstat)
      call ftfiou (iunit, fstat)

 999  return
      end
