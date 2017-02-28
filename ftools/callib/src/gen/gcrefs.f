*+GCREFS
	subroutine gcrefs(iunit, chatter, icnum, maxdim,
     &		ndim, octyp, ocnam, ocnum, onpts,
     &		ierr) 

	IMPLICIT NONE
	integer chatter, ierr
	integer iunit
	integer icnum, maxdim, ndim
	integer onpts(maxdim), ocnum(2,maxdim)
	character*(*) octyp(maxdim), ocnam(2,maxdim)
c 
c Description:
c  Reads the TDIM, CTYP & CREF keywords for column icnum of a BINTABLE, and 
c returns a bunch of info regarding the ordering of the putative n-dimensional 
c dataset.
c
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  ICNUM         i   : column number for which info required
c  MAXDIM        i   : Max number of dimensions allowed (should be >3)
c  NDIM            o : Actual no. dimensions for column ICNUM
c  OCTYP           o : Array of CTYP values for column ICNUM 
c  OCNAM           o : Array of column names containing grid for col ICNUM
c  OCNUM           o : Array of column nos. corresponding to OCNAM array
c  ONPTS           o : Array containing number of values in each dimension
c  IERR            o : Error flag (0 = OK)
c
c Called Routines:
c  subroutine CLENACT    : (CALLIB) Finds length of a string
c  subroutine CRMVBLK    : (CALLIB) Removes spaces from a string
c  subroutine FCECHO     : (FTOOLS) Writes to STDOUT
c  subroutine FTGKYx   	 : (FITSIO) Reads keyword value
c  subroutine FTGCNO     : (FITSIO) Gets column number
c  subroutine FTGTDM     : (FITSIO) Reads TDIM keyword
c  subroutine WT_FERRMSG : (CALLIB) Writes FITSIO error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0; 1995 May 22), original
c  Ian M George     (2.0.0; 1995 Jul 05), rewrite of parser
c  Ian M George     (2.0.1; 1996 Feb 05), add wtinfo & friends
	character(7) version
	parameter (version = '2.0.1')
*- 
c Internals
	character(6) subname
	parameter (subname = 'gcrefs')
	integer status, i, j, ifound, clenact, ilen
	integer maxdims
	parameter (maxdims=10)
	integer start(maxdims), stop(maxdims)
	integer idiv, itmp
	character(80) keywrd, string, comm, string2
	character(80) message

c Initialize
	ndim = 0
	ierr = 0
	status = 0
	itmp = 0

c Give user info if requested
        message = ' using '//subname//' '//version
        call wtinfo(chatter,20,1,message)

c Suss out the TDIM keyword
	call ftgtdm(iunit, icnum, maxdim, ndim, onpts, status)
	if(status.NE.0) then
	   call wtferr(subname,version,status,
     &		'Error Reading TDIM keyword')
		ierr = status
		goto 998
	endif


c Suss out the CREF keyword
	status = 0
	write(keywrd,'(a,i12)') 'CREF', icnum
	call crmvblk(keywrd)
	call ftgkys(iunit, keywrd, string, comm, status)
      	IF(status.ne.0) THEN
           message = 'Error reading '//keywrd
	   call wtferr(subname,version,status,message)
	   ierr = 1
	   goto 998
      	ENDIF

c Now parse CREF (!!)
	call crmvblk(string)
	ilen = clenact(string)
	message = 'CREF string: ' //string(:ilen)
	call wtinfo(chatter,20,3,message)
	if((string(1:1).NE.'(').OR. 
     &		(string(ilen:ilen).NE.')')) then
	   call wterrm(subname,version, ' CREF syntax error')
	   ierr = 1
	   goto 998
	endif	


c ... First pass
	ifound = 0
	start(1) = 2
	do i = 2, ilen-1
	   if(string(i:i) .EQ. ',') then
	     ifound = ifound + 1
	     stop(ifound) = i-1
	     start(ifound+1) = i+1
	   endif
	enddo
	stop(ifound+1) = ilen-1 
	ifound = ifound + 1

	do j = 1, ifound
	   idiv = 0
	   do i = start(j),stop(j)
	     if(string(i:i) .EQ. ':') then
		idiv = idiv + 1
		itmp = i
	     endif
	   enddo
	   if(idiv.GT.1) then
	        call wterrm(subname,version, ' CREF syntax error')
		message = ' Problematic sub-string: '// 
     &			string(start(j):stop(j))
	   	call wtinfo(chatter,1,3,message)
	   	ierr = 1
	   	goto 998
	   elseif(idiv.eq.1) then
	     ocnam(1,j) = string(start(j):itmp-1)
	     ocnam(2,j) = string(itmp+1:stop(j))
	   else
	     ocnam(1,j) = string(start(j):stop(j))
	     ocnam(2,j) = ocnam(1,j) 
	   endif
	enddo

c Check up on the dimensions 
	if(ifound.GT.maxdim) then
	   call wterrm(subname,version, ' Passed arrays too small')
	   write(message,'(a,i12)') 
     &		' No. dimensions found:            ', ifound
	   call wtinfo(chatter,1,2,message)
	   write(message,'(a,i12)') 
     &		' Max dimensions in passed arrays: ', maxdim
	   call wtinfo(chatter,1,2,message)
	   ierr = 1
	   goto 998
	elseif(ifound.NE.ndim) then
	   call wtwarm(subname,version,chatter,1,
     &		'Dimensions appear Inconsistent')
	   write(message,'(a,i12)') ' ... TDIM implies ', ndim
	   call wtinfo(chatter,1,2,message)
	   write(message,'(a,i12)') ' ... CREF implies ', ifound
           call wtinfo(chatter,1,2,message)
	   call wtinfo(chatter,1,2,'using TDIM value')
	endif
c Go find the column numbers 
	do i = 1, ifound
	   do j = 1, 2
	     status = 0
	     call ftgcno(iunit,.false.,ocnam(j,i),ocnum(j,i),status)
	     if(status.NE.0) ocnum(j,i) = -999
	   enddo
	enddo

c Now go for the CTYP keywords
	do i = 1, ndim
	   write(keywrd,'(i12,a,i12)') i, 'CTYP', icnum
	   call crmvblk(keywrd)
	   call ftgkys(iunit, keywrd, string, comm, status)
      	   IF(status.ne.0) THEN
                message = 'Error reading '//keywrd
	        call wtferr(subname,version,status,message)
	        ierr = 1
		goto 998
	   ELSE
		octyp(i) = string
      	   ENDIF
	enddo

c Dump stuff to the debugging user
	if(chatter.GT.20) then
	   write(message,'(a,i12)') 
     &		'No. dimensions in dataset: ', ndim
	   call wtinfo(chatter,20,2,message)
	   do i = 1, ndim
		write(string,'(a,i12)') 'd',i
		call crmvblk(string)
		write(string,'(a,a,i12,a)') 
     &		string(:clenact(string)),
     &		   ' -- ', onpts(i), ' elements -- ' 
		call rmvexsp(string,string2)
		message = string2(:clenact(string2))// ' '//
     &			octyp(i)(:clenact(octyp(i))) // ' ('//
     &			ocnam(1,i)(:clenact(ocnam(1,i)))//':'//
     &			ocnam(2,i)(:clenact(ocnam(2,i))) //')'
		call wtinfo(chatter,20,3,message)
	   enddo	   
	endif


c -----------------------------------------------------------------



998     if(ierr.NE.0) then
                call wterrm(subname,version,
     :                  ' FATAL: Unable to continue')
        endif

	return
	end

