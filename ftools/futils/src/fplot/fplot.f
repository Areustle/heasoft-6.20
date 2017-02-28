C****************************************************************************
C SELECTOR TASK:
C      fplot
C
C FILE:
C      fplot.f
C
C DESCRIPTION:
C      plots columns from a table
C
C AUTHOR/DATE:
C      James Kent Blackburn 10/09/92
C
C MODIFICATION HISTORY:
C       Rewrite of Brendan Perry's plot routine for the
C       FTOOLS environment
C       5/19/93 EAG allow for vector columns, plot versus point number
C                   allow offset for plot
C       7/1/93  EAG better labeling, easy change for number of ys
C       12/30/93 EAG allow for multiple plots
C       1/5/94   EAG print offset in non-zero
C       9/1/94 EAG 3.0c clear FITSIO stack, neaten up, dynamic memory
C       12/16/94 EAG 3.3a Allow for XTE vector plotting, add binmode parameter
C       7/13/95 MJT changed offset scheme so that page 1 offset holds
C                   throughout all pages
C      11/30/1995 JRG 3.4b - doplot: fixed TTYPE.
C      12/14/1995 JRG 3.4c - doplot: fixed bug in call to PLT.
C       4/24/96 MJT Added sensecase parameter
C       3/24/97 To increase the number of points change the following 
C                  maxpts  in fplot
C                  maxrow  in doplot 
C                  maxrow  in bldarray
C               and also the value in the paramater file
C               Now plot 100000 points.
C       9/04/97 Banashree M Seifert
C               . changed subroutine BLDCMD so that when prompted for
C                 Any legal PLT command[], and user types quit/q/exit
C                 then fplot quits after plotting.
C       10/15/97 PDW 3.5b Replace old get header routines
C                         and use standardized maxcl for array parameter
C        1/13/98 PDW 3.6 *Use dynamic memory to hold data array. Size
C                         limited only by amount of available memory
C                        *Fixed problem with handling of vector columns
C                        *No longer necessary to have fixed vector length
C                         so long as all columns end up as same size, ie
C                         binmode=SUM or MEAN
C                        *When no X column specified, index is now continuous
C                         over multiple plots (eg, points 1-20 then 21-40, etc.)
C                         instead of beginning at 1 each time.
C    04/23/1998 toliver 3.7 bldcmd: introduce last command as 'plot' before
C                                   it reads PLTCMD from user *only* when
C                                   PLTCMD is quit/q or exit.
C    12/06/1999 LEB 3.8 Added the ability to stack plots of vector elements
C                       with the binmode=STACK parameter value           
C
C NOTES:
C      fplot supported in IRAF and HOST environments
C      assuming that X11 and xanadu are available
C
C USAGE:
C      fplot
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      xparm   - x parameter
C      rows    - range of rows to plot
C      yparm1  - 1st y parameter
C      yparm2  - 2nd y parameter (optional)
C      yparm3  - 3rd y parameter (optional)
C      yparm4  - 4th y parameter (optional)
C      offset - whether to offset the X values to start at 0
C      device  - plot device type
C      sensecase/exact - be case-sensitive with column names? (v3.5a)
C      pltcmd  - PLT command or command file
C      maxpts - maximum number of points per graph
C
C CALLED ROUTINES:
C      subroutine gplot  - gets parameters from environment
C      subroutine doplot - read in data and do the plot
C
C****************************************************************************

      subroutine   fplot
      character(160) infile
      character(80) xparm,rows
      character(160) device, pltcmd
      character(160) yparm
      character(8) binmode
      logical ckerr, offset, sensecase
      integer maxpts, status

      character(40) taskname
      common /task/ taskname

      taskname = 'fplot3.8'
      call ftcmsg

C initialize parameters to blanks
      infile = ' '
      xparm  = ' '
      yparm = ' '
      rows   = ' '
      device = ' '
      pltcmd = ' '
      binmode = 'DEFAULT'
      ckerr  = .false.
      offset = .false.
      sensecase = .true.
      status = 0
      maxpts = 100000

C get parameters from parameter file
      call gplot(infile,xparm,rows,device,pltcmd,
     &     yparm, offset, maxpts, binmode, sensecase, status)
      if (status .ne. 0) goto 999

C get data from FITS file and do the plot of the data set
      call doplot(infile,xparm,rows,device, pltcmd,
     &     yparm, offset, maxpts, binmode, sensecase, status)

      if (status .ne. 0) call fcerrm (status)

 999  return
      end


C****************************************************************************
C SUBROUTINE:
C      gplot
C
C DESCRIPTION:
C      read in parameters from parameter file
C
C AUTHOR/DATE:
C      James Kent Blackburn 10/09/92
C
C MODIFICATION HISTORY:
C	5/19/93 EAG add offset parameter
C	12/30/93 EAG add maxpts parameter
C      4/24/96  MJT add sensecase parameter
C
C NOTES:
C       gplot uses F77/VOS like calls to read parameter
C
C USAGE:
C      call gplot(infile,xparm,rows,device,pltcmd,
C    &            yparm1,yparm2,yparm3,yparm4, offset,
C    &            maxpts, sensecase, status)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      xparm   - x parameter
C      rows    - range of rows to plot
C      yparm1  - 1st y parameter
C      yparm2  - 2nd y parameter (optional)
C      yparm3  - 3rd y parameter (optional)
C      yparm4  - 4th y parameter (optional)
C	offset - whether to start X axis at 0
C      device  - plot device type
C      pltcmd  - PLT command or command file
C	maxpts - maximum number of points per graph
C      sensecase - Be case-sensitive about column names?
C	status - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine fcecho - echo message to terminal
C
C****************************************************************************

      subroutine gplot(infile,xparm,rows,device,pltcmd,
     &     yparm, offset, maxpts, binmode, sensecase, status)

      character*(*) infile,xparm,rows,device,pltcmd
      character*(*) yparm, binmode
      character(80)  context
      logical       offset, sensecase
      integer       maxpts, status

C ---   Get the name of the input FITS file ---

      call uclgst('infile',infile,status)
      if ( status .ne. 0 ) then
         context = 'Could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the x parameter ---

      call uclgst('xparm',xparm,status)
      if ( status .ne. 0 ) then
         context = 'Could not get x axis parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the parameters ---

      call uclgst('yparm',yparm,status)
      if ( status .ne. 0 ) then
         context = 'Could not get y axis parameters'
         call fcerr(context)
         goto 999
      endif

C ---   Get the row range parameter ---

      call uclgst('rows',rows,status)
      if ( status .ne. 0 ) then
         context = 'Could not get row range parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the plot device type ---

      call uclgst('device',device,status)
      if ( status .ne. 0 ) then
         context = 'Could not get device type parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the pltcmd parameter ---

      call uclgst('pltcmd',pltcmd,status)
      if ( status .ne. 0 ) then
         context = 'Could not get pltcmd parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the offset parameter ---

      call uclgsb('offset', offset, status)
      if ( status .ne. 0 ) then
         context = 'Could not get offset parameter'
         call fcerr(context)
         goto 999
       endif
       
C ---   Get the sensecase parameter ---
      call uclgsb('sensecase',sensecase,status)
      if(status .ne. 0)then
        context = 'Could not get sensecase parameter'
        call fcerr(context)
        goto 999
      endif
      
C ---   Get the maxpts parameter ---

      call uclgsi('maxpts', maxpts, status)
      if ( status .ne. 0 ) then
         context = 'Could not get maxpts parameter'
         call fcerr(context)
         goto 999
      endif

c get the binning mode
      call uclgst('binmode',binmode,status)
      if ( status .ne. 0 ) then
         context = 'Could not get binmode parameter'
         call fcerr(context)
         goto 999
      endif
      call ftupch (binmode)
      if ((binmode .ne. 'SUM') .and. (binmode .ne. 'MEAN') .and.
     &    (binmode .ne. 'STACK') .and. (binmode .ne. 'DEFAULT')) then
         context = ' Unknown binmode value ' // binmode
         call fcerr (context)
         status = 1
         goto 999
      endif

 999  continue
      return
      end


C****************************************************************************
C SUBROUTINE:
C      par2de
C
C DESCRIPTION:
C      parses column parameter into data column and error column names
C
C AUTHOR/DATE:
C      James Kent Blackburn 10/15/92
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C USAGE:
C      call par2de(parm,dcol,ecol)
C
C ARGUMENTS:
C      parm  - x parameter
C      dcol  - data column name
C      ecol  - data column name
C
C PRIMARY LOCAL VARIABLES:
C      inlen   - length of parameter string
C      irbrk   - position of right bracket
C      ilbrk   - position of left bracket
C      chars   - number of characters in error string
C      status  - error number
C
C CALLED ROUTINES:
C      function fcstln - length of string with trailing blanks removed
C
C****************************************************************************

      subroutine par2de(parm,dcol,ecol)

      character*(*) parm,dcol,ecol
      integer       i,inlen,irbrk,ilbrk,chars,fcstln

C ---   Find the last non blank character ---
      ilbrk = 0
      irbrk = 0
      inlen = fcstln(parm)

C ---   Determine if any error column is attached ---

      if ( parm(inlen:inlen) .eq. ']' ) then
         irbrk = inlen
         do 10 i = inlen-1,1,-1
            ilbrk = i
            if ( parm(i:i) .eq. '[' ) goto 50
 10      continue
      else if ( parm(inlen:inlen) .eq. '}' ) then
         irbrk = inlen
         do 20 i = inlen-1,1,-1
            ilbrk = i
            if ( parm(i:i) .eq. '{' ) goto 50
 20      continue
      else if ( parm(inlen:inlen) .eq. '.' ) then
         irbrk = inlen
         do 30 i = inlen-1,1,-1
            ilbrk = i
            if ( parm(i:i) .eq. '.' ) goto 50
 30      continue
      else
         ilbrk = inlen + 1
         irbrk = 0
      endif

C ---     extract the data and error number ---

 50   continue

      if ((ilbrk-1) .ge. 1) then
         dcol = parm(1:(ilbrk - 1))
      else
         dcol = ' '
      endif
      chars = irbrk - ilbrk-1
      if (chars .gt. 0) then
         ecol = parm((ilbrk+1):(irbrk-1))
      else
         ecol = ' '
      endif

      return
      end

C****************************************************************************
C SUBROUTINE:
C      fxparm
C
C DESCRIPTION:
C      separate data and error columns from parameter
C
C AUTHOR/DATE:
C      James Kent Blackburn 10/09/92
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C       fxparm uses F77/VOS like calls to write parameters
C
C USAGE:
C      call fxparm(iunit,parm,dcol,ecol,stat)
C
C ARGUMENTS:
C      nocol - number of columns in FITS file extension
C      list  - array of column names in FITS extension
C      parm  - parameter
C      dcol  - data column name
C      ecol  - data column name
C      stat  - status flag set when column not found
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine ft____ - FITSIO subroutines
C      subroutine fccmpl - compare 2 lists for a match
C      subroutine fcecho - echo message to terminal
C
C****************************************************************************

      subroutine fxparm(nocol,list,parm,dcol,ecol,subset,stat)

      integer maxcl
      parameter     (maxcl = 999)

      integer       nocol,stat,n
      character*(*)  parm,dcol,ecol
      character*(*)  list(maxcl), subset
      character(80)  context
      logical       match,negflg

      if (stat .eq. 0) then

C ---- separate parameter into data and error column names
         call par2de(parm,dcol,ecol)
         call rmsubset (1, dcol, subset, stat)

C ---- verify existence of data column in FITS file
         if ((dcol .ne. ' ') .and. (dcol .ne. '-')) then
	    n = 1
	    negflg = .false.
	    call fccmpl(n,nocol,dcol,list,negflg,match)
	    if (.not. match) then
               context = ' data column not found: ' // dcol
               call fcerr(context)
               stat = 1
	    endif
         endif

C ---  verify existence of error column in FITS file
         if (ecol .ne. ' ') then
	    n = 1
	    negflg = .false.
	    call fccmpl(n,nocol,ecol,list,negflg,match)
	    if (.not. match) then
               context = ' error column not found: ' // ecol
               call fcerr(context)
               stat = 1
	    endif
         endif
      endif
      return
      end


C****************************************************************************
C SUBROUTINE:
C      bldcmd
C
C DESCRIPTION:
C      build some basic commands to pass along to the plot routine
C
C AUTHOR/DATE:
C      James Kent Blackburn 10/15/92
C
C MODIFICATION HISTORY:
C      4/24/96 MJT - modified to work case-insensitive if requested
C      9/04/97 Banashree M Seifert  - 
C         . modified so that cmd array is written even one enters @filename
C         . introduced last command as 'plot' before it reads PLTCMD from
C           user. This is done so that, if user inputs quit/q or exit at the
C           PLT command, then fplot plots the requested plotting and exits
C           instead of coming to the 'PLT>' command.
C           So, when an user inputs exit/quit at the PLT command, then
C           a plot will be shown and comes out with shell prompt.
C   04/23/1998 toliver - introduce last command as 'plot' before it reads
C           PLTCMD from user *only* when PLTCMD is quit/q or exit.
C
C NOTES:
C
C USAGE:
C      call bldcmd(device,pltcmd,title,ttype,tunit,mxcol,
C  &               iery,ncmd,commnd,doffset,exact)
C
C ARGUMENTS:
C      device  - plot device type
C      pltcmd  - PLT command or command filename(@filename)
C      title   - title of graph
C      ttype   - column names in FITS file
C      tunit   - units of columns in FITS file
C      mxcol   - total columns of data to plot
C      iery    - integer array of column error type
C      xdcol   - x data column name
C      ncmd    - number of commands in array
C      commnd  - array of commands for plot routine
C      exact   - column names case-sensitive?
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      function fsctln   - length of string
C      subroutine upc    - XANLIB routine to convert a character string
C                          to upper case
C
C****************************************************************************

      subroutine bldcmd(device,pltcmd,title,tfields,ttype,tunit,
     &     mxcol,iery,xdcol,ydcol,nyparm,ncmd,cmd, doffset, binmode,
     &     exact, stackGNames)

      integer       tfields,ncmd,mxcol,iery(1),maxcl,maxcol,nyparm
      parameter     (maxcl = 999)
      parameter     (maxcol = 10)
      character*(*)  device,pltcmd
      character*(*) title,ttype(maxcl),tunit(maxcl),xdcol,cmd(20)
      character*(*) ydcol(nyparm), binmode, stackGNames(maxcol)
      character(40)  temp1,temp2
      integer       i,k,noerr,fcstln
      double precision doffset
      logical       exact
      CHARACTER * 5 quitcm

      noerr = 0
      ncmd = 0

      ncmd = ncmd + 1
      cmd(ncmd) = 'Device ' // device
      ncmd = ncmd + 1
      if ((xdcol .ne. ' ') .and. (xdcol .ne. '-')) then
         do 10 k = 1 , tfields
            temp1 = xdcol
            temp2 = ttype(k)
            if (.not.exact) then
               call ftupch(temp1)
               call ftupch(temp2)
            endif
            if ( temp1 .eq. temp2 ) then
               if (tunit(k) .ne. ' ')then
                  cmd(ncmd) = 'LA X ' // ttype(k)(1:fcstln(ttype(k)))
     &                        // '  ' // tunit(k)
               else
                  cmd(ncmd) = 'LA X ' // ttype(k)
               end if
               go to 20
            endif
10       continue
      else
         cmd(ncmd) = 'LA X Point Number'
      endif
20    continue

      if (binmode .ne. 'STACK') then
         do 28 i = 1, nyparm
            ncmd = ncmd + 1
            do 25 k = 1, tfields
               temp1 = ydcol(i)
               temp2 = ttype(k)
               if (.not.exact) then
                  call ftupch(temp1)
                  call ftupch(temp2)
               endif
               if ( temp1 .eq. temp2 ) then         
                  if (tunit(k) .ne. ' ')then
                     cmd(ncmd) = 'LA G   ' //
     &                    ttype(k)(1:fcstln(ttype(k)))
     &                    // '  ' // tunit(k)
                  else
                     cmd(ncmd) = 'LA G   ' // ttype(k)
                  end if
                  write (cmd(ncmd)(5:5), '(i1)') i+1
               endif
 25         continue
 28      continue
      endif
C      if (binmode .eq. 'STACK') then
C         write(cmd(ncmd)(4:5),'(a2)') 'OY'
C      endif
      if (nyparm .ge. 4 .or. binmode .eq. 'STACK') then
         ncmd = ncmd + 1
         cmd(ncmd) = 'CSIZE .6'
      endif

      if (binmode .eq. 'STACK') then
C     in STACK mode, all columns have errors if any of them do and
C     the X axis is *always* just point number (so no X error)
         if (iery(2) .ne. 0 ) then
            ncols  = (mxcol - 1) / 2
         else
            ncols = (mxcol - 1)
         endif
         do 29 i = 2, ncols + 1
            ncmd = ncmd + 1
            cmd(ncmd) = 'LA G   '//stackGNames(i)
            write (cmd(ncmd)(5:5), '(i1)') i
 29      continue
      endif
      ncmd = ncmd + 1
      do 30 i = 1, mxcol + 1
         IF ( iery(i) .eq. 1 ) noerr = noerr + 1
30    continue
      if ( mxcol .ge. 3 .or. noerr .ge. 2 .or. binmode .eq. "STACK")
     &     then
         cmd(ncmd) = 'PLOT VERT'
         ncmd = ncmd + 1
      endif

      cmd(ncmd) = 'LAB T Plot of file ' // title(:fcstln(title))
      if (doffset .ne. 0.D0) then
         ncmd = ncmd + 1
         write (cmd(ncmd), 1001) doffset
1001     format ('LA OT Offset by ',1PG15.10)
      endif

C
C     If PLT command is quit/q or exit, then preface it with a "plot"
C        command before adding it to the command buffer.
C
      quitcm = pltcmd
      CALL upc (quitcm)
      IF ((quitcm .EQ. 'Q    ') .OR. (quitcm .EQ. 'QU   ') .OR.
     2    (quitcm .EQ. 'QUI  ') .OR. (quitcm .EQ. 'QUIT ') .OR.
     3    (quitcm .EQ. 'EX   ') .OR. (quitcm .EQ. 'EXI  ') .OR.
     4    (quitcm .EQ. 'EXIT ')) THEN
         ncmd = ncmd + 1
         cmd (ncmd) = 'plot'
      ENDIF

      if (pltcmd .ne. ' ') then
         ncmd = ncmd + 1
         cmd(ncmd) = pltcmd
      endif
 
      return
      end

C****************************************************************************
C SUBROUTINE:
C      bldarr
C
C DESCRIPTION:
C      build the array of data and associated errors for plot
C
C AUTHOR/DATE:
C      James Kent Blackburn 10/19/92
C
C MODIFICATION HISTORY:
C      4/24/96 MJT - added "exact" argument
C      1/13/97 PDW - make use of dynamic memory and handle vector
C                    columns and modes better
C
C NOTES:
C
C USAGE:
C      bldarr(iunit,numranges,rowrange1,rowrange2,dcol,ecol,
C    &	      nvec,mxcol,daterr,iery,offset,doffset,
C    &        exact, status)
C
C
C ARGUMENTS:
C      iunit     - input FITS file unit number
C      numrange  - number of ranges for row in input FITS file
C      rowrange1 - array of starting row number
C      rowrange2 - array of stopping row number
C      repeat - integer - input - repeat count of the requested column
C      maxpts - integer - input - maximum number of points to return
C      mode - integer - input - what to do with vectors:
C                               0: not a vector
C                               1: sum up all elements of the vector
C                               2: average all elements of the vector
C                               3: use as a vector
C                               4: return single element in felem
C      binmode - How to handle vector columns (string)
C      felem - integer - input - element number to read
C      dcol      - data column name
C      ecol      - error column name
C      nvec      - number data columns
C      mxcol     - number of data and error columns
C      daterr    - 2D array containing data and error values
C      iery      - array for error column type
C      doffset   - value of the offset
C      needdoff  - do we need to compute doffset?
C      exact     - column names case-sensitive?
C      status    - FITSIO integer error flag
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine ft___  - FITSIO subroutines
C      subroutine fcecho - echo message to terminal
C      function   fsctln - length of string
C
C****************************************************************************

      subroutine bldarr(iunit,numranges,rowrange1,rowrange2, repeat,
     &     maxpts, mode, binmode, felem, dcol, ecol, nvec, mxcol, 
     &     daterr, iery, doffset, needdoff, exact, stackGNames, status)

      integer iunit,numranges,nvec,mxcol,status,maxcol,
     &     rowrange1(numranges),rowrange2(numranges),iery(1), repeat
      integer maxy
      parameter (maxy = 8)
      parameter     (maxcol = 10)
C     daterr needs 2*maxcol in the y dimension to allow for
C     error columns
      real          daterr(maxpts,maxcol*2)
      real	      nullr4
      parameter     (nullr4 = -1.2e-34)
      double precision nullr8
      parameter     (nullr8 = -1.2e-34)
      character*(*) dcol,ecol,binmode,stackGNames(maxcol)
      integer i, j, colno, ecolno, felem,inrow,frow,nelem, maxpts, mode
      logical exact,anyf,needdoff
      double precision doffset
      integer p_val

C ********************************************************************
C
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C     datatype gives a symbolic code for the data type, e.g.,  4 = Integer*4
C     1 is boolean
C     2 is character
C     3 is short integer
C     4 is integer
C     5 is long integer
C     6 is single precision real
C     7 is double precision real
C     8 complex
C ********************************************************************

C ---	Initialize some things
C      exact = .false.
      if (status .ne. 0) return

C set up the appropriate number of elements
      if (mode .eq. 3) then
         nelem = repeat
      else
         nelem = 1
      endif


      if (ecol .ne. ' ') then
         call ftgcno(iunit,exact,ecol,ecolno,status)
      endif


C ---	Fill data column
      if (dcol .ne. ' ') then
         p_val = 0
         call udmget( repeat, 7, p_val, status )
         if( status.ne.0 ) then
            call fcerr('Could not allocate memory for data column')
            return
         endif
         if (binmode .ne. 'STACK') then
            mxcol = mxcol + 1
            nvec = nvec + 1
         endif
         call ftgcno(iunit,exact,dcol,colno,status)
         inrow = 1

C only make the call to get doffset for page 1, then keep it (MJT)
         if (needdoff) then
           call ftgcvd (iunit, colno, rowrange1(1), felem,
     &        1,nullr8,doffset,anyf, status)
           needdoff = .false.
         endif

         do 10 i = 1, numranges
            do 9 frow = rowrange1(i), rowrange2(i)
               if (binmode .eq. 'STACK') then
                  mxcol = mxcol + 1
                  nvec = nvec + 1
                  if (nvec - 1 .gt. maxy) then
                     status = 10
                     call fcerr('Too many rows in STACK mode')
                     return
                  endif
                  stackGNames(nvec) = 'ROW '
                  write (stackGNames(nvec)(5:6), '(i2)') frow
                  iery(nvec)=0
               endif
C get the values as doubles based on the requested mode
               call ftgcvd(iunit,colno,frow,felem,repeat,nullr8,
     &              MEMD(p_val),anyf,status)
               if ((mode .eq. 1) .or. (mode .eq. 2)) then
C we need to sum up the array
                  do 4 j = 2,repeat
                     MEMD(p_val) = MEMD(p_val) + MEMD(p_val+j-1)
 4                continue
                  if (mode .eq. 2) MEMD(p_val) = MEMD(p_val) / repeat
               endif

C     doffset is 0.D0 unless needdoff was initially true
               do 5 j = 1, nelem
                  daterr(inrow+j-1,mxcol) = MEMD(p_val+j-1) - doffset
 5             continue
               
               if (binmode .ne. 'STACK') then
                  inrow = inrow + nelem
               endif
               
               if (ecol .ne. ' ' .and. binmode .eq. 'STACK') then
C              For stacked vector plots we need to do the error columns 
C              here in this loop
                  iery(nvec) = 1
                  mxcol = mxcol + 1
                  inrow = 1
                  call ftgcve(iunit,ecolno,frow,felem,nelem,nullr4,
     &                 daterr(inrow,mxcol),anyf,status)
               endif
 9          continue
 10      continue



C ---	Fill Error Column
         if (ecol .ne. ' ' .and. binmode .ne. 'STACK') then
            iery(nvec) = 1
            mxcol = mxcol + 1
            inrow = 1
            do 20 i = 1, numranges
               frow = rowrange1(i)
               if (mode .eq. 3) then
                  nelem = (rowrange2(i) - rowrange1(i) + 1) * repeat
               else
                  nelem = (rowrange2(i) - rowrange1(i) + 1)
               endif
               call ftgcve(iunit,ecolno,frow,felem,nelem,nullr4,
     &              daterr(inrow,mxcol),anyf,status)
               inrow = inrow + nelem
 20         continue
         else if (binmode .ne. 'STACK') then
            iery(nvec) = 0
         endif
         call udmfre( p_val, 7, status )
      endif

      return
      end

C****************************************************************************
C SUBROUTINE:
C      doplot
C
C DESCRIPTION:
C      Read data and make the plot.
C
C AUTHOR/DATE:
C      James Kent Blackburn 10/09/92
C
C MODIFICATION HISTORY:
C
C    Jeff Guerber 11/30/1995 - Changed TTYPE to char*40
C    Jeff Guerber 12/14/1995 - Fixed bug in call to PLT. IERY was also used
C        for IERROR, causing strange behavior when PLTCMD was "plot" (or "p"):
C        a normal plot would flash on the screen, then be replaced with one
C        that used the value of Y as X error bars.  Another P would fix it.
C        Apparently PLT uses IERROR internally, then sets it back (or to 0?)
C        before prompting the user.
C
C       4/24/96 MJT - sensecase/exact modifications
C       1/13/97 PDW - dynamic memory added, fixed bug with using vector
C                     columns, and eliminated need for constant repeat
C                     counts when binmode is SUM or MEAN (ie it gets
C                     reduced to a single value anyway)
C
C NOTES:
C       doplot uses F77/VOS like calls to read parameter
C
C USAGE:
C      call doplot(infile,xparm,rows,device,pltcmd,
C    &     yparm,offset, maxpts, binmode, exact, status)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      xparm   - x parameter
C      rows    - range of rows to plot
C      device  - plot device type
C      pltcmd  - PLT command or command file
C      yparm   - y parameters
C      offset  - whether to offset graph
C      maxpts  - the maximum number of points per graph
C      binmode - How to handle vector columns (string)
C      exact   - column names case-sensitive?
C      status  - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C      iunit    - input unit number
C      filename - input FITS file name
C      extnumb  - extension number
C      hdutype  - header unit type
C      excount  - extension counter
C      needdoff - need to compute doffset?
C      xrepeat  - repeat count of the x column
C      yrepeat  - repeat count of the y columns
C      mode     - How to handle vector columns (1-Sum, 2-Avg, 3-Expand, 4-One)
C      context  - error message
C
C CALLED ROUTINES:
C      subroutine ft____ - FITSIO subroutines
C      subroutine gplot  - gets parameters from environment
C      subroutine pplot  - puts parameters to environment
C      subroutine fxparm - extracts the data and error columns
C      subroutine bldarr - adds a vector to the PLT data/error array
C      subroutine bldcmd - builds the PLT command array
C      subrouting plt    - calls the QDP/PLT plot package
C      subroutine fxparm - expand out parameter into column names
C      subroutine fcecho - echo message to terminal
C
C****************************************************************************

      subroutine doplot(infile,xparm,rows,device,pltcmd,
     &     yparm,offset, maxpts, binmode, exact, status)

      integer maxy
      parameter (maxy = 8)
      character*(*) infile,xparm,rows,device,pltcmd
      character*(*) yparm, binmode
      character(80)  context,xdcol,xecol,ydcol(maxy)
      character(80)  yecol(maxy), yparmlist(maxy)
      character(160) cmdseq(20)
      character(160) filename
      character(40)  ysubset(maxy), xsubset
      integer       status, errsts, nyparm, maxpts, ycolno
      integer       iunit,extnumb,hdutype,tfields,npts, mode
      integer       maxcl, maxcol
      integer       xfelem, yfelem(maxy)
      integer       xrepeat, yrepeat(maxy)
      parameter     (maxcl = 999)
      parameter     (maxcol = 10)
      integer       p_data
      character(80)  stackGNames(maxcol)
C     real          daterr(maxpts,maxcol*2)
      integer       iery(maxcol),i,rowlen,nrows,tbcol,varidat
      integer       nvec, mxcol, ncmd, block
      integer       colno, newpoints, totalpts, dtype, width
      integer       numranges, rowrange1(maxcl), rowrange2(maxcl)
      integer	    newranges, new1range(maxcl), new2range(maxcl)
      integer       ierror
      logical       exact,goodrows, offset, negflag, needdoff
      character(40)  ttype(maxcl)
      character(16)  tform(maxcl)
      character(25)  tunit(maxcl)
      character(80)  extname
      double precision doffset

C ********************************************************************
C
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C     datatype gives a symbolic code for the data type, e.g.,  4 = Integer*4
C     1 is boolean
C     2 is character
C     3 is short integer
C     4 is integer
C     5 is long integer
C     6 is single precision real
C     7 is double precision real
C     8 complex
C ********************************************************************

C      common /ftgcmm/ daterr

C Initialize variables
      iunit = 15
      status = 0
      errsts = 0
      xdcol = ' '
      xecol = ' '
      doffset = 0.D0
      needdoff = offset

C ---   Parse the INFILE into a filename and extension number ---

 10   continue

      call fcpars(infile,filename,extnumb,status)

C EAG 8/25/93 default to 1st extension
      if (extnumb .eq. -99) extnumb = 1

C ---	If file name or extension not found set status flag ---

      call ftopen(iunit,filename,0,block,status)
      if (status .ne. 0 ) then
         context = 'Unable to open FITS file: ' // filename
         call fcerr(context)
         goto 30
      endif

C --- Check that extension exists & that column list is a subset

      if ( extnumb .ge. 1 ) then
         call ftmrhd(iunit,extnumb,hdutype,status)
         if ( status .ne. 0 ) then
	    context = 'Extension Not Found in FITS File: ' // infile
	    call fcerr(context)
	    call ftclos(iunit,errsts)
	    goto 30
         else
	    if ( hdutype .eq. 1 ) then
               call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &              tform,tunit,extname,status)
	    else if ( hdutype .eq. 2 ) then
               call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &              extname,varidat,status)
	    else
               context = 'Header Unit Type For Extension Not Supported'
               call fcerr(context)
               call ftclos(iunit,errsts)
               goto 30
	    endif
         endif
      else
         context = 'Illegal extension supplied: ' // infile
         call fcerr(context)
         call ftclos(iunit,errsts)
         goto 30
      endif
      call fcgcls (yparm, yparmlist, nyparm, negflag)
      if (nyparm .gt. maxy) then
         context = ' Too many y axes requested'
         call fcerr (context)
         call ftclos(iunit,errsts)
         goto 30
      endif

C ---   Parse the x-y parameters into a data and error column ---

      call fxparm(tfields,ttype,xparm,xdcol,xecol,xsubset,status)
      do 12 i = 1, nyparm
         call fxparm (tfields, ttype, yparmlist(i), ydcol(i),
     &        yecol(i), ysubset(i), status)
 12   continue

      if (status .ne. 0 ) then
         call ftclos(iunit,errsts)
         goto 30
      endif

C find the repeat counts of all columns:  must all be the same
      if (hdutype .eq. 1) then
         xrepeat = 1
      else
         colno = 0
         call ftgcno (iunit, exact, xdcol, colno, status)
C column not found, assume want to graph against point number
         if ((status .ne. 0) .or. (colno .eq. 0)) then
            xrepeat = 0
            status = 0
            call ftcmsg
         else
            call ftbnfm (tform(colno), dtype, xrepeat, width, status)
            if (xsubset .ne. ' ') then
               xrepeat = 1
               call elemparse (xsubset, iunit, colno, xfelem, status)
            else
               xfelem = 1
            endif
         endif
      endif

      if (status .ne. 0 ) then
         call ftclos(iunit,errsts)
         goto 30
      endif

C figure out what mode we want
C
C if X column a vector without an index, plot ALL data
C if binmode=STACK, this is still the case
      if (xrepeat .gt. 1) then
         mode = 3
      else if (binmode .eq. 'SUM') then
         mode = 1
      else if (binmode .eq. 'MEAN') then
         mode = 2
      else if (xrepeat .eq. 1) then
         mode = 4
      else
         mode = -1
      endif

C check the Y columns and get the starting element number
      do 16 i = 1, nyparm
         if (ysubset(i) .eq. ' ') then
            call ftgcno (iunit, exact, ydcol(i), colno, status)
            call ftbnfm (tform(colno), dtype, yrepeat(i), width, status)
            if (status .ne. 0) then
               call fcerrm (status)
               goto 30
            endif
            yfelem(i) = 1
            if (mode.eq.-1) then
               xrepeat = yrepeat(i)
               mode = 3
            else if ( (mode.eq.3 .and. yrepeat(i).ne.xrepeat) .or.
     &                (mode.eq.4 .and. yrepeat(i).ne.1) ) then
               status = 1
               context = ' Cannot plot vectors of different length'
               call fcerr(context)
               call ftclos(iunit,errsts)
               goto 30
            endif
         else if (mode.eq.3 .and. xrepeat.gt.1) then
            status = 1
            context = 'Indexed column used where vector needed'
            call fcerr(context)
            call ftclos(iunit,errsts)
            goto 30
         else
            if (mode.eq.-1) mode = 4
            call ftgcno (iunit, exact, ydcol(i), ycolno, status)
            call elemparse (ysubset(i), iunit, ycolno, yfelem(i),
     &           status)
            yrepeat(i) = 1
         endif
 16   continue

      if (status .ne. 0 ) then
         errsts = 0
         call ftclos(iunit,errsts)
         goto 30
      endif

C ---   check that rows list is allowable

      call fccmpr(rows,goodrows)
      if (.not. goodrows) then
         status = 1
         context = 'Illegal row range supplied: ' // rows
         call fcerr(context)
         call ftclos(iunit,errsts)
         goto 30
      endif

C ---   determine which rows are to be included and get the row ranges

      if (rows .eq. '-') then
         numranges = 1
         rowrange1(1) = 1
         rowrange2(1) = nrows
      else
         call fcgrgs(rows,nrows,numranges,rowrange1,rowrange2)
      endif

 30   if (status .ne. 0) return

C ---   Got this far so we can get data ready

C ---   Build the data vector array and error type array ---

      npts = 1
      totalpts = 0
      do 19 i = 1, numranges
         if (binmode .eq. 'STACK') then
            totalpts = yrepeat(1)
         elseif (mode.eq.3) then
               totalpts = totalpts + (rowrange2(i) - rowrange1(i) + 1)
     &              * xrepeat
         else
            totalpts = totalpts + (rowrange2(i) - rowrange1(i) + 1)
         endif
 19   continue

      if (maxpts.gt.totalpts) maxpts = totalpts
      p_data = 0
      call udmget( maxpts*maxcol*2, 6, p_data, status )
      if( status.ne.0 ) then
         context='Not enough memory to allocate data array'
         call fcerr(context)
         goto 1000
      endif

 20   nvec = 0
      mxcol = 0

C set up the number of points for this graph
      call getpts (numranges, rowrange1, rowrange2, xrepeat, maxpts,
     &     mode, binmode, npts, newranges, new1range, new2range, 
     &     newpoints, status)


C and the X values
      if ((xdcol .ne. ' ') .and. (xdcol .ne. '-')) then
         call bldarr(iunit, newranges, new1range, new2range, xrepeat,
     &        maxpts, mode, binmode, xfelem,
     &        xdcol, xecol, nvec, mxcol, MEMR(p_data), iery,
     &        doffset, needdoff, exact, stackGNames, status)
      else
C use point number for X axis
         mxcol = mxcol + 1
         nvec = nvec + 1
         iery(nvec) = 0
         if ( binmode .ne. 'STACK') then
            maxx = newpoints
         else 
            maxx = yrepeat(1)
         endif
         do 50 i = 1, maxx
            MEMR(p_data + maxpts*(mxcol-1) + (i-1)) = i + 
     &                                                npts-newpoints-1
 50      continue
      endif

C build up the Y values
      do 60 i = 1, nyparm
         call bldarr(iunit, newranges, new1range, new2range, yrepeat(i),
     &        maxpts, mode, binmode, yfelem(i),
     &        ydcol(i), yecol(i), nvec, mxcol, MEMR(p_data), iery,
     &        0.D0, .false., exact, stackGNames, status)
 60   continue

      if (status .ne. 0) goto 999

C ---	Build the Command sequence for the PLOT Package

      call bldcmd(device,pltcmd,filename,tfields,ttype,tunit,
     &     mxcol,iery,xdcol,ydcol,nyparm,ncmd,cmdseq, doffset,
     &     binmode, exact, stackGNames)

C ---	Call the plot subroutine

      if ( binmode .eq. 'STACK') then
         newpoints = yrepeat(1)
      endif
      

      call plt(MEMR(p_data),iery,maxpts,newpoints,nvec,cmdseq,ncmd,
     &     ierror)


C loop back for more, if needed
      if (npts .lt. totalpts) goto 20

      call udmfre( p_data, 6, status )

 999  continue
      call ftclos(iunit,status)
 1000 continue

      return
      end

C******************************************************************************
C SUBROUTINE:
C      getpts
C
C DESCRIPTION:
C	set up range parameters such that maxpts are included, starting
C	at npts, and updating npts afterward
C
C AUTHOR/DATE:
C	Emily A. Greene
C	Hughes STX
C	30 December, 1993
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C USAGE:
C	call getpts (numranges, rowrange1, rowrange2, repeat, maxpts,
C                    mode, binmode, npts, newranges, new1range, new2range,
C                    newpoints, status)
C
C ARGUMENTS:
C	numranges - the input total number of ranges
C	rowrange1 - the input start of ranges
C	rowrange2 - the input stop of ranges
C	repeat    - the repeat count for this column
C	maxpts    - the maximum number of points to return
C      mode - integer - input - what to do with vectors:
C                               0: not a vector
C                               1: sum up all elements of the vector
C                               2: average all elements of the vector
C                               3: use as a vector
C                               4: return single element in felem
C      binmode - char*(*) - input - what to do with vector columns 
C	npts      - the starting point this time / returned next starting point
C	newranges - returned new number of ranges to use
C	new1range - returned start of new ranges
C	new2range - returned stop of new ranges
C	newpoints - number of points included in new ranges
C	status    - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************

      subroutine getpts (numranges, rowrange1, rowrange2, repeat,
     &     maxpts, mode, binmode, npts, newranges, new1range, new2range,
     &     newpoints, status)

      integer numranges, rowrange1(numranges), rowrange2(numranges)
      integer repeat, maxpts, npts, mode, maxpoints
      integer newranges, new1range(numranges), new2range(numranges)
      integer newpoints, status
      character*(*) binmode

      integer pointer, oldpointer, i, j, nelem

      if (binmode .eq. 'STACK') then
C     Don't adjust anything
         newranges = numranges
         npts = maxpts
         do i = 1,newranges
            new1range(i) = rowrange1(i)
            new2range(i) = rowrange2(i)
         enddo
         return
      endif
         
      
      if (mode.eq.3) then
         nelem = repeat
      else
         nelem = 1
      endif



C Find the starting point
      pointer = 1
      do 10 i = 1, numranges
         oldpointer = pointer
         pointer = pointer + (rowrange2(i)-rowrange1(i)+1)*nelem
         if (pointer .ge. npts) then
C the start point is in this range
            new1range(1) = rowrange1(i) + (npts - oldpointer)/nelem
            goto 20
         endif
 10   continue

C if we are here, there is an error
      status = 10

C find the number of ranges that contain maxpts, and the correct start and stops
 20   newranges = 0
      newpoints = 0
      do 30 j = i, numranges
         newranges = newranges + 1
         newpoints = newpoints +
     &        (rowrange2(j)-new1range(newranges)+1)*nelem
         if (newpoints .ge. maxpts) then
            new2range(newranges) = rowrange2(j) -
     &           (newpoints-maxpts+nelem-1)/nelem
            newpoints = newpoints -
     &           (rowrange2(j)-new2range(newranges))*nelem
            goto 900
         endif
 30   continue

C if we are here, all of the values can be taken in this set
      new2range(newranges) = rowrange2(numranges)

 900  npts = npts + newpoints

 999  return
      end
