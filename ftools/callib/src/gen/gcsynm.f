*+GCSYNM
        subroutine gcsynm(chatter, mode, csys, 
     &		nam1, nam2, unit1, unit2, ierr)

	IMPLICIT NONE
	integer chatter, ierr, mode
	character*(*) csys, nam1, nam2, unit1, unit2
c 
c Description:
c  Returns 
c   EITHER (mode 0) the standard OGIP CALDB column names (or Keyword if 
c                   Greenbank Convention used) and default units for a 
c		    given spatial coordinate system.
c       OR (mode 1) the spatial coordinate system in use (along with the 
c		    default units) for a given coordinate system string
c  Extremely crude, but hopefully easy to maintain
c
c Passed parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  MODE          i   : Operation mode: 
c				Mode = 0: i/p CSYS, o/p NAM1/NAM2
c                       	Mode = 1: i/p NAM1/NAM2, o/p CSYS
c  CSYS          i/o : Coordinate system in use
c  NAM1          i/o : Column name (or keyword) for one coordinate
c  NAM2          i/o : Column name (or keyword) for other coordinate
c  UNIT1	   o : Default units for NAM1
c  UNIT2	   o : Default units for NAM2
c  IERR            o : Error status flag (0 = OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine CLENACT    : (CALLIB) Returns actual length of string
c  subroutine CRMVLBK    : (CALLIB) Remove leading blanks from string
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c
c Compilation & Linking
c  link with CALLIB & FTOOLS
c
c Origin:
c  Adapted from (and REPLACES) gt_csysnms (v1.1.1) 
c
c Authors/Modification History:
c  Ian M George     (1.0.0:95 May 15), original
c  Ian M George     (1.1.0:95 Jul 11), added XMA_DCOS system
c  Ian M George     (1.1.1:96 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '1.1.1')
*- 
c Internals
	character(6) subname
	parameter (subname ='gcsynm')
	integer icsys, icoord1, icoord2
	parameter (icsys = 6)
	integer status, ilen, clenact, i
	character(80) message
	character(8) scsys(icsys), snam1(icsys), snam2(icsys)
	character(8) sunit1(icsys), sunit2(icsys)
	logical qok
c Initialize
	data scsys 
     &  /'RAW_DET','LIN_DET','PHY_DET','XMA_POL','XMA_CART','XMA_DCOS'/
	data snam1 
     &  /'RAWX',   'DETX',   'PHYX',   'THETA',  'ALPHA',  'COS_ALPHA'/
	data snam2 
     &	/'RAWY',   'DETY',   'PHYY',   'PHI',    'BETA',  'COS_BETA'/
	data sunit1
     &	/'pixel',  'pixel',  'mm',     'deg',    'deg', ' '/
	data sunit2
     &	/'pixel',  'pixel',  'mm',     'deg',    'deg', ' '/
	status = 0

c Give user info if requested
	message = 'using '//subname//' '//version
	call wtinfo(chatter,20,1,message)

c Main If block
c ------------------------------------------------------------------------
c ... Mode zero, when csys claimed to be known
	if(mode.EQ.0) then
876	  if(csys.EQ.' ') then
	        call wterrm(subname,version,
     &			'Unspecified Coord System')
		call wtinfo(chatter,1,1,
     &		'Expecting non-blank string when in mode 0')
		ierr = 1
		goto 998
	  else
	        do i = 1, icsys
		  if(csys.EQ.scsys(i)) then
			nam1 = snam1(i)
			nam2 = snam2(i)
			unit1 = sunit1(i)
			unit2 = sunit2(i)
			ierr = 0
		  	goto 998
		   endif
		enddo
	  endif
c         ... attempt to parse the csys string
	  unit1 = 'UNKNOWN'
	  unit2 = 'UNKNOWN'
	  qok = .false.
	  call crmvlbk(csys)
	  ilen = clenact(csys)
	  if(csys(1:1).EQ.',') goto 123
	  do i = 2, ilen
		   if(csys(i:i).EQ.',') then
			nam1 = csys(1:i-1)
			nam2 = csys(i+1:ilen)
		        qok = .true.
			goto 123
		   endif
	  enddo
123	  if(.NOT.qok) then
		call wterrm(subname,version,
     &			'Unable to parse Coord System')
	           ierr = 1
		   goto 998 		
	  else
	     call wtwarm(subname,version,chatter,15,
     &			'Unknown units')
	  endif
c ------------------------------------------------------------------------
c ... Mode one, when either/both the cnams claim to be known
	elseif(mode.EQ.1) then
	  icoord1 = 0
	  icoord2 = 0
c ... 1st coord
	  do i = 1, icsys
	     if(nam1.EQ.snam1(i)) then
		icoord1 = i
	     endif
	  enddo
c ... 2nd coord
	  do i = 1, icsys
	     if(nam2.EQ.snam2(i)) then
		icoord2 = i
	     endif
	  enddo
c ... Now sort out what's going on
	  if(icoord1.EQ.icoord2) then
		if(icoord1.EQ.0) then
		  call wterrm(subname,version,
     &			'Unmatched Coord Col/Key')
		  call fcecho(message)
		  csys = 'UNKNOWN'
		  unit1 = 'UNKNOWN'
		  unit2 = 'UNKNOWN'
		  ierr = 1
		  goto 998
		else
		  csys = scsys(icoord1)
		  nam1 = snam1(icoord1)
		  nam2 = snam2(icoord1)
		  unit1 = sunit1(icoord1)
		  unit2 = sunit2(icoord1)
		  ierr = 0
		  goto 998
		endif
	  elseif((icoord1.GT.0).AND.(icoord2.EQ.0))then
		  csys = scsys(icoord1)
		  nam1 = snam1(icoord1)
		  nam2 = snam2(icoord1)
		  unit1 = sunit1(icoord1)
		  unit2 = sunit2(icoord1)
		  ierr = 0
		  goto 998
	  elseif((icoord1.EQ.0).AND.(icoord2.GT.0))then
		  csys = scsys(icoord2)
		  nam1 = snam1(icoord2)
		  nam2 = snam2(icoord2)
		  unit1 = sunit1(icoord2)
		  unit2 = sunit2(icoord2)
		  ierr = 0
		  goto 998
	  else
		call wterrm(subname,version,
     &			'Mismatched Coord Col/Key')
		  message = 'Coord1 Name implies '//
     &			scsys(icoord1)
		  call wtinfo(chatter,1,1,message)
		  message = 'Coord2 Name implies '//
     &			scsys(icoord2)
		  call wtinfo(chatter,1,1,message)
		  csys = 'UNKNOWN'
		  unit1 = 'UNKNOWN'
		  unit2 = 'UNKNOWN'
		  ierr = 1
		  goto 998
	  endif
c ------------------------------------------------------------------------
	else
	  ierr = 1
	  call wterrm(subname,version,'Unsupported MODE')
	  goto 998
	endif		
c ------------------------------------------------------------------------

998	if(ierr.NE.0) then
	   call wterrm(subname,version,'Incomplete Execution')
	else
	   call wtinfo(chatter,20,2,'Coord system defined')
	endif

	return
	end
	
