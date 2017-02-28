*+GTDATS
        subroutine gtdats(day, month, year, ddmmyy)
        implicit none
        character(2) day, month
	character(4) year
	character(8) ddmmyy

C-----------------------------------------------------------------------
C Description: Gets the current system date and time and returns the
C              day of the month, month number and year as character 
C              strings to day, month, and year respectively.  Also
C              returns the day, month and year in the format dd/mm/yy
C              to the character string ddmmyy.
C	       Errors can be detected by ddmmyy = 00/00/00
C
C Arguments:   day    (r): the day of the month (1-31)
C              month  (r): the month number (1-12)
C              year   (r): the year (1993-2092)
C              ddmmyy (r): the date in dd/mm/yy format
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C  Ron Zellar  (1.0.0: 93 Sep 15) Original Version
C  Ian M George(1.1.0: 96 Feb 06) Replaced gtdati w/ fitsio/ftgsdt
C
C $Id: gtdats.f,v 3.7 2013/05/21 19:08:17 irby Exp $
C
C $Log: gtdats.f,v $
C Revision 3.7  2013/05/21 19:08:17  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 3.6  1996/08/09 15:48:54  oneel
C Yet another status variable not inited.  Loses on Irix and others.
C
C
	character(7) version
	parameter (version = '1.1.0')
C
C-----------------------------------------------------------------------
*-
	character(6) subname 
	parameter (subname = 'gtdats')
	integer id,im,iy
	integer status
	character(4) cd,cm
	character(2) yy

C Get the day month and year
        status = 0
	call ftgsdt(id,im,iy,status)
	if(status.ne.0) then
	  call wterrm(subname,version,
     &		'Failed to obtain current system date')
	  goto 999
	endif

C Convert integers to characters
	write(cd,'(I4)')id
	write(cm,'(I4)')im
	write(year,'(I4)')iy

C Take the section of the string containing info.
	day = cd(3:4)
	month = cm(3:4)
	yy = year(3:4)

C Replace leading blanks with 0's.
	if (day(1:1) .eq. ' ') day(1:1) = '0'
	if (month(1:1) .eq. ' ') month(1:1) = '0'

C Construct the date in dd/mm/yy format
	ddmmyy = day//'/'//month//'/'//yy

999	if(status.ne.0) then
	    ddmmyy = '00/00/00'
	endif

        return
        end
