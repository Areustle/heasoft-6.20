*+GT_CSYSNMS
        subroutine gt_csysnms(chatter, csys, 
     &		nam1, nam2, ierr)

	IMPLICIT NONE
	integer chatter, ierr
	character*(*) csys, nam1, nam2
c 
c Description:
c  Returns the standard OGIP CALDB column names for a given spatial 
c  coordinate system (csys)
c
c Passed parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  CSYS          i   : Coordinate system in use
c  nam1            o : Column name for one coordinate
c  nam2            o : Column name for other coordinate
c  IERR            o : Error status flag (0 = OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c
c Compilation & Linking
c  link with CALLIB & FTOOLS
c
c Origin:
c  Original 
c
c Authors/Modification History:
c  Ian M George     (1.0.0:93 Feb 22), original
c  Ian M George     (1.0.1:93 Mar 30), added XMA_CART coords
c  Ian M George	    (1.1.0:94 Jan 06), LINX,LINY --> DETX,DETY
c  Ian M George	    (1.1.1:94 Aug 11), changed characters to *(*)
	character(7) version
	parameter (version = '1.1.1')
*- 

c Internals
	integer status
	character(80) message
        character(30)  errstr, wrnstr
c Initialize
	status = 0
        errstr = '** GT_CSYSNMS ERROR: '
        wrnstr = '** GT_CSYSNMS WARNING: '
	

c Give user info if requested
        if(chatter.GE.20) then
             message = ' ... using GT_CSYSNMS '// version
	     call fcecho(message)
        endif

c Main If block
	if(csys.EQ.'RAW_DET') then
		nam1 = 'RAWX'
		nam2 = 'RAWY'
	elseif(csys.EQ.'LIN_DET') then
		nam1 = 'DETX'
		nam2 = 'DETY'
	elseif(csys.EQ.'PHY_DET') then
		nam1 = 'PHYX'
		nam2 = 'PHYY'
	elseif(csys.EQ.'XMA_POL') then
		nam1 = 'THETA'
		nam2 = 'PHI'
	elseif(csys.EQ.'XMA_CART') then
		nam1 = 'ALPHA'
		nam2 = 'BETA'
	else
		ierr = 1
		message = errstr // 'Unknown Coord System'
		call fcecho(message)
		message = wrnstr // 'Assuming CSYS = LIN_DET'
		call fcecho(message)
		nam1 = 'DETX'
		nam2 = 'DETY'
	endif		


	return
	end
	
