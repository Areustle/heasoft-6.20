*+ ME_DETNAM
      	SUBROUTINE me_detnam(chatter,ndets,dets,gas,detnam,status)

	IMPLICIT NONE
	integer chatter, ndets, status
	character*(*) gas
	character*(*) dets(8)
	character*(*) detnam
c
c Description
c   Works out the official OGIP Detnam string for the EXOSAT ME 
c
c Passed Parameters
c  CHATTER	i   : Chatter flag (<5=quite,10=normal,>20=silly)
c  NDETS 	i   : Number of detectors in array
c  DETS 	i   : Array of detector names
c  GAS    	i   : Code of gas in use ('AR','XE','AX'= AR+XE)
c  DETNAM         o : Output string containing the detector name
c  STATUS         o : Return error flag (0 = OK)
c
c Called Routines
c  subroutine FCECHO            : (FTOOLS) writes to standard o/p
c  subroutine WT_FERRMSG        : (CALLIB) writes FITSIO error message etc
c
c Origin
c   Hacked from Lorella's WRITE_DETNAM code
c
c Authors/Modification History
c  Ian M George    (1.0.0: 1993 Oct 08), original
c  Ian M George    (2.0.0: 1994 Jan 28), rewrite to make more general
	character(7) version
	parameter (version = ' 2.0.0 ')
*-
c Internals
	integer fcstln, i, clenact
	character(20) detstr, detstp
        character(80)  first, second
	character(40) errstr,wrnstr
	character(70) subinfo
	logical q(8),qq1,qq2,qq3,qq4,qh1,qh2
c Initialize
	status = 0
        errstr = '** ME_DETNAM '//version//' ERROR :'
        wrnstr = '** ME_DETNAM '//version//' WARNING :'
	do i = 1, 8
	   q(i) = .false.
	enddo
	qq1 = .false.
	qq2 = .false.
	qq3 = .false.
	qq4 = .false.
	qh1 = .false.
	qh2 = .false.

c Stick in a message for the really keen user
      IF (chatter.GE.20) THEN
         subinfo = ' ... using ME_DETNAM Ver '//version
         call fcecho(subinfo)
         subinfo = ' ...... trying to construct DETNAM string'
         call fcecho(subinfo)
      ENDIF

c Check for sillies
	if(ndets.EQ.1) then
	  detnam = dets(1)
	  return
	endif

c Calculate the first and last detectors
	detstr = dets(1)
	detstp = dets(ndets)

      IF (chatter.GE.25) THEN
         write(subinfo,'(a,i12)') ' ...... Number of detectors  :',
     &                            ndets
         call fcecho(subinfo)
      ENDIF

c Set logicals as to which detectors are present
	do i = 1, ndets
	  if(dets(i)(:5).EQ.'DET-A') q(1) = .true.
	  if(dets(i)(:5).EQ.'DET-B') q(2) = .true.
	  if(dets(i)(:5).EQ.'DET-C') q(3) = .true.
	  if(dets(i)(:5).EQ.'DET-D') q(4) = .true.
	  if(dets(i)(:5).EQ.'DET-E') q(5) = .true.
	  if(dets(i)(:5).EQ.'DET-F') q(6) = .true.
	  if(dets(i)(:5).EQ.'DET-G') q(7) = .true.
	  if(dets(i)(:5).EQ.'DET-H') q(8) = .true.
	enddo

c .... working our way up
c Quad-1
	if(q(1).and.q(2)) then
		if(ndets.eq.2) then
			first = 'QUAD1'
			goto 987
		else
			qq1 = .true.
		endif
	endif
c Quad-2
	if(q(3).and.q(4)) then
		if(ndets.eq.2) then
			first = 'QUAD2'
			goto 987
		else
			qq2 = .true.
		endif
	endif
c Quad-3
	if(q(5).and.q(6)) then
		if(ndets.eq.2) then
			first = 'QUAD3'
			goto 987
		else
			qq3 = .true.
		endif
	endif
c Quad-4
	if(q(7).and.q(8)) then
		if(ndets.eq.2) then
			first = 'QUAD4'
			goto 987
		else
			qq4 = .true.
		endif
	endif
c Corners
	if(q(1).and.q(4).and.q(5).and.q(8)) then
		if(ndets.eq.4) then
			first = 'CORN'
			goto 987
		endif
	endif

c Half1
	if(qq1.and.qq2) then
		if(ndets.eq.4) then
			first = 'HALF1'
			goto 987
		else
			qh1 = .true.
		endif
	endif
c Half2
	if(qq3.and.qq4) then
		if(ndets.eq.4) then
			first = 'HALF2'
			goto 987
		else
			qh2 = .true.
		endif
	endif
c All Detectors
	if(qh1.and.qh2) then
		first = 'ALL'
		goto 987
	endif

c Non-obvious combination of detectors, construct a string
	first = ' '
c .. Half-1 detectors
	if(q(1)) then
	   if(qh1) then
		first = 'HALF1'
	   elseif(qq1) then
		first = 'QUAD1'
	   else
		first = 'DET-A'
	   endif
	elseif(q(2)) then
	   first = 'DET-B'
	endif

	if(q(3)) then
	   if(qq2) then
		first = first(:clenact(first))//',QUAD2'
	   else
		first = first(:clenact(first))//',DET-C'
	   endif
	elseif(q(4)) then
	   first =first(:clenact(first))//',DET-D'
	endif
c .. Half-2 detectors
	if(q(5)) then
	   if(qh2) then
		first = first(:clenact(first))//',HALF2'
	   elseif(qq3) then
		first = first(:clenact(first))//',QUAD3'
	   else
		first = first(:clenact(first))//',DET-E'
	   endif
	elseif(q(6)) then
	   first = first(:clenact(first))//',DET-F'
	endif
	if(q(7)) then
	   if(qq4) then
		first = first(:clenact(first))//',QUAD4'
	   else
		first = first(:clenact(first))//',DET-G'
	   endif
	elseif(q(8)) then
	   first =first(:clenact(first))//',DET-H'
	endif
c .. remove any blanks
	call crmvblk(first)
c .. strip off any leading comma
	if(first(1:1).EQ.',') then 
	   first = first(2:clenact(first))
	endif


C  
C        Decode second part ME detector
987	 if (gas.eq.'AR'.OR.gas.EQ.'XE') then
	     second= gas
	 elseif (gas.eq.'AX') then
	     second= 'AR+XE'
	 else
	   subinfo = errstr // ' Unknown ME gas: '// gas
	   call fcecho(subinfo)
           status=1
	   goto 998
         endif

C         
C  Obtain ME string
         DETNAM = first(1:fcstln(first))//' '//
     &     second(1:fcstln(second))

      IF (chatter.GE.20) THEN
         subinfo = ' ...... DETNAM string constructed: '//DETNAM
         call fcecho(subinfo)
      ENDIF


998     if(status.NE.0) then
          subinfo = wrnstr // 'DETNAM string will be set to NONE'
          call fcecho(subinfo)
	  detnam = 'NONE'
        endif
 



      RETURN
      END
