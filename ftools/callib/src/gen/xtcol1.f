*+XTCOL1
	subroutine xtcol1(iunit, chatter, maxen, maxc1, maxc2,
     &		enlo, enhi, enunt, csys, c1lo, c1hi, c1unt,
     &		c2lo, c2hi, c2unt, ien, engrd, ic1, c1grd,
     &		ic2, c2grd, collresp, work,
     &		ierr)

	IMPLICIT NONE
	integer iunit, chatter, ierr
	integer maxc1, maxc2, maxen
	integer ien, ic1, ic2
	real enlo, enhi, c1lo, c1hi, c2lo, c2hi
	real engrd(2,maxen), c1grd(2,maxc1), c2grd(2,maxc2)
	real collresp(maxen,maxc1,maxc2)
	real work(maxen*maxc1*maxc2)
	character*(*) enunt, csys, c1unt, c2unt
c 
c Description:
c  Extractor for a COLLRESP extension in one of the formats conforming to 
c the HDUVERS2='1.*.*' family. Returned is the sub-cube of stored data 
c values and corresponding axis grids defined by the passed ranges of 
c energy & spatial coordinates. 
c  Currently only the following formats are supported (see OGIP/92-022)
c   HDUVERS2 = '1.0.0'
c   HDUCERS2 = '1.1.0'
c  Assumes the FITS file is open and at the appropriate HDU
c
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open COLLRESP file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  MAXEN         i   : Max no. of energies allowed (array definitions)
c  MAXC1         i   : Max no. of coord1 values allowed (array definitions)
c  MAXC2         i   : Max no. of coord2 values allowed (array definitions)
c  ENLO          i   : Lowest requested energy (in units given by ENUNT)
c  ENHI          i   : Highest requested energy (in units given by ENUNT)
c  ENUNT         i   : Unit string for ENLO & ENHI
c  CSYS          i   : Spatial coordinate system used for C1LO, C1HI, C2LO etc 
c  C1LO          i   : Lowest requested value for coord1 (in C1UNT units)
c  C1HI          i   : Highest requested value for coord1 (in C1UNT units)
c  C1UNT         i   : Unit string for C1LO & C1HI
c  C2LO          i   : Lowest requested value for coord2 (in C2UNT units)
c  C2HI          i   : Highest requested value for coord2 (in C2UNT units)
c  C2UNT         i   : Unit string for C2LO & C2HI
c  IEN             r : Number of Energies grid pts returned
c  ENGRD           r : 2-D array containing Energies grid pts 
c  IC1             r : Number of Coord1 grid pts returned
c  C1GRD           r : 2-D array containing Coord1 grid pts (in C1UNT units)
c  IC2             r : Number of Coord2 grid pts returned
c  C2GRD           r : 2-D array containing Coord2 grid pts (in C2UNT units)
c  COLLRESP        r : 3-D array containing collimator response data values
c  WORK          i   : 1-D workspace array of same size as COLLRESP
c  IERR            o : Error flag (0 = OK)
c
c Called Routines:
c  subroutine CRMVBLK    : (CALLIB) Removes blanks from a string
c  subroutine FCECHO     : (FTOOLS) Writes to STDOUT
c  subroutine FTGBCL     : (FITSIO) Gets BINTABLE info
c  subroutine FTGCNO     : (FITSIO) Gets column number
c  subroutine FTGCVE	 : (FITSIO) Reads BINTABLE dataset
c  subroutine FTGHBN     : (FITSIO) Gets essential BINTABLE header info
c  subroutine FTGKYx   	 : (FITSIO) Reads keyword value
c  subroutine GT_CSYSNMS : (CALLIB) Gets CALDB coord system col names
c  subroutine WT_FERRMSG : (CALLIB) Writes FITSIO error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0; 1995 May 24), original - mega-crude version
c  Ian M George     (1.1.0: 1996 Feb 06), fixed Greenbank Conv for ENERG_*
c                                         and added wtinfo & friends
	character(7) version
	parameter (version = '1.1.0')
*- 
c Internals
	character(6) subname
	parameter (subname = 'xtcol1')
	integer maxelem
	parameter (maxelem=999)
	character(20) dummy20
	double precision doubdum
	integer status, nfields
	integer i, j, k, jj
	integer irows, ncols, ivar, frow, felem
	integer enull
        parameter (nfields=3)
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
	character(20) crspvers
	character(40) comm
	character(160) message
	logical anyflg
	integer icol, ndim
	character(20) coordtyp(3), coordnam(2,3)
	character(20) units
	integer coordcol(2,3), coordnpts(3)	
	integer edim, c1dim, c2dim, nelem
	real ufactor
	real buffer(2,maxelem)
	integer estart, estop
	integer c1start, c1stop, c2start, c2stop
	integer fpixel(4), lpixel(4), incs(4)
	logical qenerg
	
c Initialize
	icol = 0
	ndim = 0
	ierr = 0
	status = 0
        edim = 0
	c1dim = 0
	c2dim = 0
	estart = 0
	estop = 0
	c1start = 0
	c1stop = 0
	c2start = 0
	c2stop = 0

c Give user info if requested
        message = ' using '//subname//' '//version
        call wtinfo(chatter,20,1,message)


c More info for the really keen user
	message = 'sub-cube requested of '//subname//':'
	call wtinfo(chatter,25,2,message)
	if(enlo.LE.enhi) then
	   write(message,'(a,f12.6,a,f12.6,a,a)')
     &	     'energy range: ',enlo,' - ',enhi,' ', enunt
	   call wtinfo(chatter,25,3,message)
	else
	   call wtinfo(chatter,25,3,
     &	    'energy range: ALL (whatever limits within dataset)')
	endif
	if(c1lo.LE.c1hi) then
	   write(message,'(a,f12.6,a,f12.6,a,a)')
     &	     'coord-1 range: ',c1lo,' - ',c1hi,' ', c1unt
	   call wtinfo(chatter,25,3,message)
	else
	   call wtinfo(chatter,25,3,
     &	    'coord-1 range: ALL (whatever limits within dataset)')
	endif
	if(c2lo.LE.c2hi) then
	   write(message,'(a,f12.6,a,f12.6,a,a)')
     &	     'coord-2 range: ',c2lo,' - ',c2hi,' ', c2unt
	   call wtinfo(chatter,25,3,message)
	else
	   call wtinfo(chatter,25,3,
     &	    'coord-2 range: ALL (whatever limits within dataset)')
	endif

c --- READING KEYWORD VALUES ---

      status = 0
      call ftghbn(iunit,nfields,irows,ncols,ttype,tform,tunits,
     &            comm,ivar,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version, status,
     &          ' reading binary header info')
        ierr = 2
        goto 998
      ENDIF
      IF (irows.GT.1) then
        call wtwarm(subname, version, 1, 1,
     &          ' Dataset contains more than one row')
        call wtinfo(chatter,1,1,
     &    ' ... Sorry, this version will ignore all but 1st row')
      ENDIF

c Sort our whether we have energy columns present
        qenerg = .false.
        do i = 1, ncols
          if(ttype(i).EQ.'ENERG_LO') then
                qenerg = .true.
          elseif(ttype(i).EQ.'ENERG_HI') then
                qenerg = .true.
          endif
        enddo


c CRSPVERS ...
      crspvers = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',crspvers,comm,status)
      IF (status.ne.0) then
        call wtwarm(subname, version, 20, 1,
     &          'Problem reading HDUVERS2')
      ENDIF
      IF (crspvers.EQ.'  ') THEN
        status = 0
        call ftgkys(iunit,'CRSPVERS',crspvers,comm,status)
        if(status.ne.0) then
          call wtwarm(subname, version, 20, 1,
     &          'Problem reading CRSPVERS')
        endif
      ENDIF


c Find the column number of the coll.cube dataset
        call ftgcno(iunit, .false., 'COLLRESP', icol, status)
        IF (status.NE.0) THEN
            call wtferr(subname,version, status,
     &                  ' COLLRESP column not present')
            ierr = 1
            goto 998
        ENDIF

c Get all the info regarding the storage of the coll.cube dataset
        call gcrefs (iunit, chatter, icol, 3, ndim,
     &          coordtyp, coordnam, coordcol, coordnpts, ierr)
        IF(ierr.NE.0) then
                goto 998
        endif

c Cycle through the dimensions found, assigning pointers to the relevant dims
        do i = 1, ndim
           if(coordtyp(i).EQ.'ENERGY') then
                edim = i
           elseif(coordtyp(i).EQ.'COORD-1') then
                c1dim = i
           elseif(coordtyp(i).EQ.'COORD-2') then
                c2dim = i
           endif
        enddo

c ---------------------- Start ENERGY GRID PROCESSING ------------------------
c ... Get the energy Lower bounds grid & put into buffer
	if(qenerg) then
 	  call ftgbcl(iunit, coordcol(1,edim), dummy20, units, dummy20,
     &			nelem, doubdum,doubdum,enull,dummy20,status)
	  if(status.NE.0)then
            call wtferr(subname,version, status,
     &                  ' FTGBCL call for ENERG_LO')
            ierr = 1
	    goto 998
	  elseif(nelem.GT.maxelem) then
	    call wterrm(subname,version, 'Internal arrays too small')
            ierr = 1
	    goto 998
	  elseif(units.NE.enunt) then
            call wterrm(subname,version,'Units mis-match for ENERG_LO')
 	    call wtinfo(chatter,1,2,
     &                  'currently unable to translate units')
            ierr = 1
	    goto 998
	  else
	    ufactor = 1.0
      	  endif
      	  status = 0
	  frow = 1
      	  felem = 1
	  enull = 0
      	  call ftgcve(iunit, coordcol(1,edim), frow, felem, nelem,
     &		  enull, work, anyflg, status)
      	  IF(status.NE.0) THEN
            call wtferr(subname,version, status,
     &                  'FTGCVE call for ENERG_LO')
            ierr = 1
	    goto 998
      	  ENDIF
	  do i = 1, nelem
		buffer(1,i) = ufactor*work(i)	
	  enddo
	else
          call ftgkye(iunit,'ENERG_LO', buffer(1,1), dummy20,
     &                  status)
          if(status.ne.0) then
            call wtferr(subname, version, status,
     &                  'Reading ENERG_LO keyword')
            ierr = 1
            goto 998
          endif
          nelem = 1
          units = 'keV'
	  if(units.NE.enunt) then
            call wterrm(subname,version,'Units mis-match for ENERG_LO')
 	    call wtinfo(chatter,1,2,
     &                  'currently unable to translate units')
            ierr = 1
	    goto 998
	  endif
	endif

c ... Get the energy Upper bounds grid & put into buffer
	if(qenerg) then
	  call ftgbcl(iunit, coordcol(2,edim), dummy20, units, dummy20,
     &			nelem, doubdum,doubdum,enull,dummy20,status)
	  if(status.NE.0)then
            call wtferr(subname,version, status,
     &                  ' FTGBCL call for ENERG_LO')
            ierr = 1
	    goto 998
	  elseif(nelem.GT.maxelem) then
	    call wterrm(subname,version, 'Internal arrays too small')
            ierr = 1
	    goto 998
	  elseif(units.NE.enunt) then
            call wterrm(subname,version,'Units mis-match for ENERG_HI')
 	    call wtinfo(chatter,1,2,
     &                  'currently unable to translate units')
            ierr = 1
	    goto 998
	  else
		ufactor = 1.0
      	  endif
      	  status = 0
	  frow = 1
      	  felem = 1
	  enull = 0
      	  call ftgcve(iunit, coordcol(2,edim), frow, felem, nelem,
     &		  enull, work, anyflg, status)
      	  IF (status.NE.0) THEN
            call wtferr(subname,version, status,
     &                  'FTGCVE call for ENERG_HI')
            ierr = 1
	    goto 998
      	  ENDIF
	  do i = 1, nelem
		buffer(2,i) = ufactor*work(i)	
	  enddo
	else
          call ftgkye(iunit,'ENERG_HI', buffer(2,1), dummy20,
     &                  status)
          if(status.ne.0) then
            call wtferr(subname, version, status,
     &                  'Reading ENERG_HI keyword')
            ierr = 1
            goto 998
          endif
          nelem = 1
          units = 'keV'
	  if(units.NE.enunt) then
            call wterrm(subname,version,'Units mis-match for ENERG_HI')
 	    call wtinfo(chatter,1,2,
     &                  'currently unable to translate units')
            ierr = 1
	    goto 998
	  endif
	endif

	if(enlo.GT.enhi) then
		estart = 1
		estop = nelem
	else
c
	do i = 1, nelem
		if(buffer(1,i).GT.enlo)then
			if(i.GT.1) then
				estart = i - 1
			else
				estart = 1
			endif
			goto 654
		endif
	enddo

654 	continue
	do i = 1, nelem
		if(buffer(2,i).GT.enhi)then
			if(i.GT.1) then
				estop = i - 1
			else
				estop = 1
			endif
			goto 655
		endif
	enddo

	endif

655	continue

	if(estop.LT.estart) then
	   call wterrm(subname,version,'No overlap found in ENERGY')
	   ierr = 1
	   go to 998
	elseif((coordcol(1,edim).EQ.coordcol(2,edim)).AND.
     &	  		(estop.EQ.estart)) then
		estop = estop + 1
	endif

	ien = estop - estart + 1
	j = 0
	do i = estart, estop
		j = j + 1
		engrd(1,j) = buffer(1,i)
		engrd(2,j) = buffer(2,i)
	enddo
c ---------------------- End ENERGY GRID PROCESSING ------------------------


c ---------------------- Start COORD-1 GRID PROCESSING ------------------------
c ... Get the coord-1 Lower bounds grid & put into buffer
	call ftgbcl(iunit, coordcol(1,c1dim), dummy20, units, dummy20,
     &			nelem, doubdum,doubdum,enull,dummy20,status)
	if(status.NE.0)then
             call wtferr(subname,version, status,
     &              ' FTGBCL call for COORD-1 LO')
             ierr = 1
	     goto 998
	elseif(nelem.GT.maxelem) then
	     call wterrm(subname,version, 'Internal arrays too small')
             ierr = 1
	     goto 998
	elseif(units.NE.c1unt) then
	     call wtferr(subname,version, status,
     &		'Units mis-match for COORD-1 LO')
 	call wtinfo(chatter,1,2,'currently unable to translate units')
             ierr = 1
	     goto 998
	else
	     ufactor = 1.0
      	endif
      	status = 0
	frow = 1
      	felem = 1
	enull = 0
      	call ftgcve(iunit, coordcol(1,c1dim), frow, felem, nelem,
     &		  enull, work, anyflg, status)
      	IF (status.NE.0) THEN
	    call wtferr(subname,version,status,
     &		'FTGCVE call for COORD-1 LO')
            ierr = 1
	    goto 998
      	ENDIF
	do i = 1, nelem
		buffer(1,i) = ufactor*work(i)	
	enddo

c ... Get the Coord-1 Upper bounds grid & put into buffer
	call ftgbcl(iunit, coordcol(2,c1dim), dummy20, units, dummy20,
     &			nelem, doubdum,doubdum,enull,dummy20,status)
	if(status.NE.0)then
             call wtferr(subname,version, status,
     &              ' FTGBCL call for COORD-1 HI')
             ierr = 1
	     goto 998
	elseif(nelem.GT.maxelem) then
	     call wterrm(subname,version, 'Internal arrays too small')
             ierr = 1
	     goto 998
	elseif(units.NE.c1unt) then
	     call wtferr(subname,version, status,
     &		'Units mis-match for COORD-1 HI')
             call wtinfo(chatter,1,2,
     & 		'currently unable to translate units')
             ierr = 1
	     goto 998
	else
	     ufactor = 1.0
      	endif
      	status = 0
	frow = 1
      	felem = 1
	enull = 0
      	call ftgcve(iunit, coordcol(2,c1dim), frow, felem, nelem,
     &		  enull, work, anyflg, status)
      	IF (status.NE.0) THEN
	    call wtferr(subname,version,status,
     &		'FTGCVE call for COORD-1 HI')
            ierr = 1
	    goto 998
      	ENDIF
	do i = 1, nelem
	    buffer(2,i) = ufactor*work(i)	
	enddo

	if(c1lo.GT.c1hi) then
		c1start = 1
		c1stop = nelem
	else
c
	do i = 1, nelem
		if(buffer(1,i).GT.c1lo)then
			if(i.GT.1) then
				c1start = i - 1
			else
				c1start = 1
			endif
			goto 656
		endif
	enddo

656 	continue
	do i = 1, nelem
		if(buffer(2,i).GT.c1hi)then
			if(i.GT.1) then
				c1stop = i - 1
			else
				c1stop = 1
			endif
			goto 657
		endif
	enddo

	endif

657	continue


	if(c1stop.LT.c1start) then
	    call wterrm(subname,version,'No overlap found in COORD-1')
	    ierr = 1
	    go to 998
	elseif((coordcol(1,c1dim).EQ.coordcol(2,c1dim)).AND.
     &	  		(c1stop.EQ.c1start)) then
		c1stop = c1stop + 1
	endif

	ic1 = c1stop - c1start + 1
	j = 0
	do i = c1start, c1stop
		j = j + 1
		c1grd(1,j) = buffer(1,i)
		c1grd(2,j) = buffer(2,i)
	enddo
	
c ---------------------- End COORD-1 GRID PROCESSING ------------------------


c ---------------------- Start COORD-2 GRID PROCESSING ------------------------
c ... Get the coord-2 Lower bounds grid & put into buffer
	call ftgbcl(iunit, coordcol(1,c2dim), dummy20, units, dummy20,
     &			nelem, doubdum,doubdum,enull,dummy20,status)
	if(status.NE.0)then
             call wtferr(subname,version, status,
     &              ' FTGBCL call for COORD-2 LO')
             ierr = 1
	     goto 998
	elseif(nelem.GT.maxelem) then
	     call wterrm(subname,version, 'Internal arrays too small')
             ierr = 1
	     goto 998
	elseif(units.NE.c2unt) then
             call wtferr(subname,version, status,
     &          'Units mis-match for COORD-2 LO')
             call wtinfo(chatter,1,2,
     & 		'currently unable to translate units')
             ierr = 1
	     goto 998
	else
	     ufactor = 1.0
      	endif
      	status = 0
	frow = 1
      	felem = 1
	enull = 0
      	call ftgcve(iunit, coordcol(1,c2dim), frow, felem, nelem,
     &		  enull, work, anyflg, status)
      	IF (status.NE.0) THEN
            call wtferr(subname,version,status,
     &          'FTGCVE call for COORD-2 LO')
            ierr = 1
	    goto 998
      	ENDIF
	do i = 1, nelem
	    buffer(1,i) = ufactor*work(i)	
	enddo

c ... Get the Coord-2 Upper bounds grid & put into buffer
	call ftgbcl(iunit, coordcol(2,c2dim), dummy20, units, dummy20,
     &			nelem, doubdum,doubdum,enull,dummy20,status)
	if(status.NE.0)then
             call wtferr(subname,version, status,
     &              ' FTGBCL call for COORD-2 HI')
             ierr = 1
             goto 998
	elseif(nelem.GT.maxelem) then
             call wterrm(subname,version, 'Internal arrays too small')
             ierr = 1
             goto 998
	elseif(units.NE.c2unt) then
             call wtferr(subname,version, status,
     &          'Units mis-match for COORD-2 HI')
             call wtinfo(chatter,1,2,
     & 		'currently unable to translate units')
             ierr = 1
             goto 998
	else
	     ufactor = 1.0
      	endif
      	status = 0
	frow = 1
      	felem = 1
	enull = 0
      	call ftgcve(iunit, coordcol(2,c2dim), frow, felem, nelem,
     &		  enull, work, anyflg, status)
      	IF (status.NE.0) THEN
            call wtferr(subname,version,status,
     &          'FTGCVE call for COORD-2 HI')
            ierr = 1
            goto 998
      	ENDIF
	do i = 1, nelem
	    buffer(2,i) = ufactor*work(i)	
	enddo

	if(c2lo.GT.c2hi) then
		c2start = 1
		c2stop = nelem
	else
c
	do i = 1, nelem
		if(buffer(1,i).GT.c2lo)then
			if(i.GT.1) then
				c2start = i - 1
			else
				c2start = 1
			endif
			goto 658
		endif
	enddo

658 	continue
	do i = 1, nelem
		if(buffer(2,i).GT.c2hi)then
			if(i.GT.1) then
				c2stop = i - 1
			else
				c2stop = 1
			endif
			goto 659
		endif
	enddo

	endif

659	continue


	if(c2stop.LT.c2start) then
            call wterrm(subname,version,'No overlap found in COORD-2')
            ierr = 1
            go to 998
	elseif((coordcol(1,c2dim).EQ.coordcol(2,c2dim)).AND.
     &	  		(c2stop.EQ.c2start)) then
		c2stop = c2stop + 1
	endif


	ic2 = c2stop - c2start + 1
	j = 0
	do i = c2start, c2stop
		j = j + 1
		c2grd(1,j) = buffer(1,i)
		c2grd(2,j) = buffer(2,i)
	enddo
	

c ---------------------- End COORD-2 GRID PROCESSING ------------------------

c Temp checks
	if(qenerg) then
	  if((edim.NE.1).OR.(c1dim.NE.2).OR.(c2dim.NE.3)) then
	    call wterrm(subname,version,
     &	'Unsupported ordering of the dimensions of COLRESP dataset')
	    ierr = 1
	    goto 998
	  endif
        endif


c Read the COLLRESP dataset
c	write(message,'(a,i12)') 'Dimensionality of COLLRESP cube: ',
c     &		ndim	
c	call wtinfo(chatter,20,2,message)

	if(qenerg) then
	  fpixel(edim) = estart
	  lpixel(edim) = estop
	else 
	  ien = 1
	endif
	fpixel(c1dim) = c1start
	lpixel(c1dim) = c1stop
	fpixel(c2dim) = c2start
	lpixel(c2dim) = c2stop
	fpixel(ndim+1) = 1
	lpixel(ndim+1) = 1

	do i = 1, ndim+1
		incs(i) = 1
	enddo

	enull = 0.0
	call ftgsve(iunit, icol, ndim, coordnpts, 
     &		fpixel, lpixel, incs, enull, work, anyflg, status)
      	IF (status.NE.0) THEN
	     call wtferr(subname,version,status,
     &		'FTGCVE call for COLLRESP sub-cube')
             ierr = 1
	     goto 998
      	ENDIF

	jj = 0
	do k = 1, ic2
	   do j = 1, ic1
	      do i = 1, ien
		jj = jj + 1
		    collresp(i,j,k) = work(jj)	
	      enddo
	   enddo
	enddo

c -----


998     if(ierr.NE.0) then
	    call wterrm(subname,version,
     &           ' Extension NOT successfully read')
        else
	    call wtinfo(chatter,20,1,
     &		'COLLRESP extension read successfully')
        endif

	return
	end

