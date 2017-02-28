C******************************************************************************
C SUBROUTINE:
C      mkf2ReadInfFil
C
C DESCRIPTION:
C      Get parameters from parameter file for mkfilter program
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX / NASA / GSFC
C       25 March, 1994
C
C MODIFICATION HISTORY:
C      Jeff Guerber, RSTX/GSFC, Aug. 1998.  Added redo_satf.
C      Jeff Guerber, RSTX/GSFC, Aug. 1998.  Added margin.
C
C NOTES:
C      mkf2ReadInfFil uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C    from FORTRAN:
C      call mkf2ReadInfFil (s0_hk, s1_hk, g2_hk, g3_hk, rig_file,
C                           orb_file, leap_file, att_file, euler,
C                           binwidth, outfile, redo_satf, status)
C
C    from C:
C      mkf2ReadInfFil (s0_hk, s1_hk, g2_hk, g3_hk, rig_file,
C                      orb_file, leap_file, att_file, euler,
C                      binwidth, outfile, redo_satf, status)
C
C ARGUMENTS:
C      s0_hk    - Path and name of SIS0 HK file (CHAR)
C      s1_hk    - Path and name of SIS1 HK file (CHAR)
C      g2_hk    - Path and name of GIS2 HK file (CHAR)
C      g3_hk    - Path and name of GIS3 HK file (CHAR)
C      rig_file - Path and name of rigidity data file (CHAR)
C      orb_file - Path and name of orbit file (CHAR)
C      leap_file- Path and name of the leapsecond file (CHAR)
C      att_file - Path and name of attitude file (optional) (CHAR)
C      euler    - Array of 3 euler angles (REAL(3))
C      binwidth - Bin width, in seconds (DOUBLE)
C      outfile  - name of the output FITS file (CHAR)
C      redo_satf- should the SIS saturation flags be recalculated
C      status   - status of operation (INT)
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      eulerstr - string holding 3 euler angle before parsing
C
C CALLED ROUTINES:
C      subroutine fcerr - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsr - get real parameter
C
C******************************************************************************

      subroutine mkf2ReadInfFil (s0_hk, s1_hk, g2_hk, g3_hk,
     &     rig_file, orb_file, leap_file, att_file, euler,
     &     binwidth, outfile, redo_satf, att_margin, status)

      character*(*) s0_hk, s1_hk, g2_hk, g3_hk
      character*(*) rig_file, orb_file, leap_file, att_file, outfile
      real euler(3)
      double precision binwidth, att_margin
      integer status, iatt
      logical redo_satf

      character(80) context, eulerstr

      iatt = 0

C  get the name of the SIS0 HK file
      call uclgst ('s0_hk', s0_hk, status)
      if ((status .ne. 0) .or. (s0_hk .eq. ' ')) then
         context = 'could not get s0_hk parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the SIS1 HK file
      call uclgst ('s1_hk', s1_hk, status)
      if ((status .ne. 0) .or. (s1_hk .eq. ' ')) then
         context = 'could not get s1_hk parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the GIS2 HK file
      call uclgst ('g2_hk', g2_hk, status)
      if ((status .ne. 0) .or. (g2_hk .eq. ' ')) then
         context = 'could not get g2_hk parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the GIS3 HK file
      call uclgst ('g3_hk', g3_hk, status)
      if ((status .ne. 0) .or. (g3_hk .eq. ' ')) then
         context = 'could not get g3_hk parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the input rigidity file
      call uclgst ('rigidity', rig_file, status)
      if ((status .ne. 0) .or. (rig_file .eq. ' ')) then
         context = 'could not get rigidity parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the input orbit file
      call uclgst ('orbit', orb_file, status)
      if ((status .ne. 0) .or. (orb_file .eq. ' ')) then
         context = 'could not get orbit parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the (FITS) leapsecond data file
      call uclgst ('leapsec', leap_file, status)
      if ((status .ne. 0) .or. (leap_file .eq. ' ')) then
         context = 'could not get leapsec parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the input attitude file (not required)
      call uclgst ('attitude', att_file, status)
      if ( att_file .eq. ' ' ) then
         iatt = 2
      else
         iatt = 1
      endif
      if (status .ne. 0) then
         context = 'could not get attitude parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the input euler angles
      call uclgst ('euler', eulerstr, status)
      read (eulerstr, *, err=888, iostat=status)
     &     euler(1), euler(2), euler(3)
 888  if (status .ne. 0) then
         context = 'could not get euler parameters'
         call fcerr(context)
         goto 999
      endif

C  get the name of the input binwidth
      call uclgsd ('binwidth', binwidth, status)
      if ((status .ne. 0) .or. (binwidth .le. 0.)) then
         context = 'could not get binwidth parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the output FITS file
      call uclgst ('outfile', outfile, status)
      if ((status .ne. 0) .or. (outfile .eq. ' ')) then
         context = 'could not get outfile parameter'
         call fcerr(context)
         goto 999
      endif

C  get flag saying whether to recalculate the SIS saturation flags
      call uclgsb ('redosatf', redo_satf, status )
      if (status .ne. 0) then
         context = 'could not get redosatf parameter'
         call fcerr(context)
         goto 999
      endif

C  get the margin for extrapolating the attitude file
      call uclgsd ('attmargin', att_margin, status)
      if ( status .ne. 0 ) then
         context = 'could not get attmargin parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue

      if ( status .eq. 0 ) status = iatt
      return
      end


C******************************************************************************
