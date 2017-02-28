C******************************************************************************
C SUBROUTINE:
C      open_asp_file (formally fgafil)
C
C DESCRIPTION:
C      open the attitude file
C
C AUTHOR/DATE:
C       Emily A. Greene    Fall 1992
C	Hughes STX
C
C MODIFICATION HISTORY:
C       
C     Modified fgafil for ASCALIN arguments and error checking, EVG Aug 1993.
C     Modified to read the attitude correction keyword, KM March 2005.
C
C NOTES:
C
C USAGE:
C      call open_asp_file (aspect, aunit, asp_time, atimcl, qcolmn, status)
C
C ARGUMENTS:
C  input:
C	aspect   - attitude file name and extension
C	aunit    - attitude file unit number
C	atimcl   - attitude file time column name
C	qcolmn     - attitude file q parameter column name
C	tzero    - time zero in event file
C	dzero    - date zero in event file
C  output:
C	ahtype   - attitude file header type
C	atime_col- attitude file time column number
C	q_col    - attitude file q parameter column number
C	nattitude- number of rows in file
C       status   - error number
C
C PRIMARY LOCAL VARIABLES:
C      contxt - error message
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - write error message to terminal
C
C******************************************************************************

        subroutine open_asp_file (aspect, aunit, asp_time, atimcl, 
     &     qcolmn, qstat, status)
 
        implicit none

	character*(*) aspect, asp_time, atimcl, qcolmn, qstat
	integer aunit, status
        
        include 'asca_defs.inc'
        
        integer stat, maxcl
        parameter( maxcl = 99 )
        integer row, ahtype, nrecords, xtensn, rwmode
        integer block, rowlen, vardat, tfield, tbcol(maxcl)
        character(80) ttype(maxcl), tform(maxcl), tunit(maxcl)

	character(80) contxt, filenm, extnam
	character(20) keywd
	logical exact
        
        include 'asca_common.inc'

        stat = 0
        block = 0
	rwmode = 0
	exact = .false.

C     open the attitude file
        
	call fcpars (aspect, filenm, xtensn, status)
	call ftopen (aunit, filenm, rwmode, block, status)
        contxt = 'Unable to open attitude file'
	if (status .ne. 0) goto 999
        
        if (xtensn .eq. -99) xtensn = 1
        if (xtensn .le. 0) then
           contxt = ' primary extension not supported'           
           status = 1
           goto 998
        end if

	call ftmrhd (aunit, xtensn, ahtype, status)
        contxt = 'Error moving to attitude file extention'
	if (status .ne. 0) goto 998
        
C     get header depending on extension type
        
        if (ahtype .eq. 1) then
           call ftghtb (aunit, maxcl, rowlen, nattitude, tfield, ttype,
     &          tbcol, tform, tunit, extnam, status)
        else if (ahtype .eq. 2) then
           call ftghbn (aunit, maxcl, nattitude, tfield, ttype, tform, 
     &          tunit, extnam, vardat, status)
        else
           contxt = 'File extension type not supported'
           goto 998
        endif

C     find the data of this file
        
        call ftgkys(aunit, 'DATE', asp_time, contxt, status)
        if (status .eq. 202) then
           status = 0
           asp_time = 'NOT AVAILABLE'
        end if
        
C     find the total number of rows in the file and start and stop times
        
	call ftgkyj (aunit, 'NAXIS2', nattitude, contxt, status)
	call ftgkyd (aunit, 'MTIME0', astart, contxt, status)
	call ftgkyd (aunit, 'MTIME1', astop, contxt, status)
        contxt = 'Cannot determine attitude MTIME0 or MTIME1'

	if (status .ne. 0) goto 998

C     attempt to read the "Gotthelf Correction" keywords
        call ftgkye (aunit, 'GDETXOFF', gdetxoff, contxt, status)
        if (status .eq. 202) then
c         GDETXOFF keyword not found - Rev2 (or earlier) attitude file
          status = 0
          gdetxoff = 0.0
          gdetyoff = 0.0
          sdetxoff = 0.0
          sdetyoff = 0.0
        else
c         The keywords are in arcminutes - convert to radians for later use
          gdetxoff = gdetxoff / 60.0 * deg_to_rad
          call ftgkye (aunit, 'GDETYOFF', gdetyoff, contxt, status)
          gdetyoff = gdetyoff / 60.0 * deg_to_rad
          call ftgkye (aunit, 'SDETXOFF', sdetxoff, contxt, status)
          sdetxoff = sdetxoff / 60.0 * deg_to_rad
          call ftgkye (aunit, 'SDETYOFF', sdetyoff, contxt, status)
          sdetyoff = sdetyoff / 60.0 * deg_to_rad
          contxt = 'Error reading coordinate offsets'
          if (status .ne. 0) goto 998
        end if

        call ftgcno (aunit, exact, atimcl, atime_col, status)        
	call ftgcno (aunit, exact, qcolmn, q_col, status)
        if (status .eq. 219) then
           status = 0
           call ftgcno (aunit, exact, 'QPARAM', q_col, status)
           if (status .eq. 219) then
              status = 0
              call ftgcno (aunit, exact, 'QPARM', q_col, status)
           end if
        end if
        call ftgcno (aunit, exact, qstat, qstat_col, status)
        contxt = 'Error finding attitude TIME, Q, SENSOR column '
	if (status .ne. 0) goto 998

C     check if event file and attitude file times are in range
        
        astart = astart - 0.0001
        astop  = astop  + 0.0001

 999    continue
        if (status .ne. 0) call fcerr(contxt)
        return
        
 998    call fcerr(contxt)
        stat = 0
        call  ftclos (aunit, stat)

        end


