*+CBDCOM
C 
		subroutine cbdcom(str1,str2,pres,vres,ures,status)

	implicit none

	character*(*) str1,str2
	logical       pres,vres,ures  
	integer       status
              
C-----------------------------------------------------------------------
C Description: Compares the values in str1 with the values in str2.
C              Should the 'param' expression (see below) of str1 equal 
C              the 'param' expression of str2, then the pres argument is
C              returned as TRUE.  If any value descriptors in str1 
C              contain a value which is also contained in the value 
C              descriptors of str2, then vres is returned as TRUE.
C              If there is a problem parsing the CBD strings, then a 
C              non-zero status is returned.
C
C              NOTE: no comparison is made between the units strings in
C              this version.  However, this routine should eventually be
C              modified so that the values of cbd1 are converted to the 
C              units of cbd2 if the units strings do not match and the
C              param strings do match.
C
C              The strings cbd1 and cbd2 should be in the CBDnXXXX 
C              keyword value format.  i.e. 
C
C              param(valdes1,valdes2,...,valdesn)units
C
C              where param is the boundary parameter, valdesn is the nth
C              value descriptor, and units is the physical units of 
C              valdesn.  The value descriptor has three forms.  To 
C              describe a single value, valdesn = 'value'.  To describe
C              a continuous range of values, valdesn = 'minval-maxval'.
C              To describe a character string, valdesn = 'string'.  
C              (Enclose string in double quotes to force interpretation
C              as a string, e.g. "0123" would be interpreted as 0123,
C              and not as 123.)
C
C Arguments:   str1    (i): string containing values in the CBDnXXXX
C                           format
C              str2    (i): string containing values in the CBDnXXXX
C                           format
C              pres    (r): .true. if str1 has a boundary param which
C                           matches the boundary param of str2.
C              vres    (r): .true. if str1 has a value contained in str2
C                           otherwise .false.
C              ures    (r): .true. if str1 has the same unit as str2,
C                           otherwise .false.
C              status  (r): the success status of this routine 0=OK
C
C Origin:      written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jan 31, 1994 -- Original version
C              Ron Zellar Jun 30, 1994 -- Removed result argument and
C                                         replaced with pres and vres
C    MFC Nov 2005 - Version 2.0: added units test (ures parameter)
C-----------------------------------------------------------------------
*- Version 2.0

        integer n,type,errstat,lpos,rpos,index,brkt1,brkt2
        integer brkt1e, brkt2e,len1, len2,fcstln,unitlen
        real minval,maxval,value
        character(80) unit1, unit2,param1,param2

C	initialize the pointer, result variables, and status flags
	n = 0
	pres = .false.
	vres = .false.
	ures = .false.
	status = 0
        unitlen=0
        
C	Compare the param values
	brkt1 = index(str1,'(')
	brkt2 = index(str2,'(')

	if ( (brkt1 .eq. 0) .or. (brkt2 .eq. 0) ) then
		status = 1
		return
        endif
        param1=str1(:(brkt1-1))
        param2=str2(:(brkt2-1))
C	if( str1(:(brkt1-1)) .eq. str2(:(brkt2-1)) ) then
        if( param1 .eq. param2 ) then
   	       pres = .true.
C              check that the units are the same
               brkt1e=index(str1,')')
               len1=len(str1)
	       brkt2e=index(str2,')')
	       len2=len(str2)
	       unit1=str1(brkt1e+1:)
               if (fcstln(unit1).eq.0) unit1='No Unit Specified'
               unit1=unit1(:fcstln(unit1))
               unit2=str2(brkt2e+1:)
               if (fcstln(unit2).eq.0) unit2='No Unit Specified'
               unit2=unit2(:fcstln(unit2))
C               write(*,*),str1,str2
C               write(*,*) 'CBDCOM; unit1, unit2',unit1,unit2
	       if(unit1.ne.unit2) then
                    ures=.false.
		 else 
                    ures=.true.
               endif
C               write(*,*)'URES =',ures
c               if (.NOT.URES) then 
c                 write(*,*)'-------'
c                 write(*,*) 'WARNING: UNITS MISMATCH'            
c                 write(*,*) '  Param1= ',param1(:fcstln(param1)),
c     &            ' UNIT = ',unit1
c                 write(*,*) '  Param2= ',param2(:fcstln(param2)),
c     &            ' UNIT = ',unit2
c                 write(*,*)'-------'
c               else  
c                    write(*,*) 'WARNING: Parameter Overlap'
c                    write(*,*) ' Param1= ',param1(:fcstln(param1)),
c     &              ' Unit = ',unit1
c                    write(*,*) ' Param2= ',param2(:fcstln(param2)),
c    &              ' Unit = ',unit2
c               endif
        endif

C	Top of the comparison loop
100     continue

C       Move the pointer to the next value descriptor
        call cbdmp(str1,n,errstat)
        if ( (errstat .ne. 0) .and. (errstat .ne. 2) ) then
                status = 1
                return
        endif

C       if errstat = 2 then finished examining the string
        if (errstat .eq. 2) return

C       Determine the type of value descriptor
        call cbdtyp(str1,n,type,errstat)
        if (errstat .ne. 0) then
                status = 2
                return
        endif

C	if string -> get str1 string and compare to str2,
C       if single value -> get str1 real value and compare to str2,
C       if range -> get str1 range and compare range to str2.

	if (type .eq. 1) then
		call cbdgs(str1,n,lpos,rpos,errstat)
		if (errstat .ne. 0) then
			status = 3
			return
		endif

		call cbdcs(str2,str1(lpos:rpos),vres,errstat)
		if (errstat .ne. 0) then
			status = 4
			return
		endif

		if (vres) return

	else if (type .eq. 2) then
		call cbdgrg(str1,n,minval,maxval,errstat)
		if (errstat .ne. 0) then
			status = 5
			return
		endif

		call cbdcrg(str2,minval,maxval,vres,errstat)
		if (errstat .ne. 0) then
			status = 6
			return
		endif

		if (vres) return

	else if (type .eq. 3) then
		call cbdgr(str1,n,value,errstat)
		if (errstat .ne. 0) then
			status = 7
			return
		endif

		call cbdcr(str2,'eq',value,vres,errstat)
		if (errstat .ne. 0) then
			status = 8
			return
		endif

		if (vres) return

	endif

                   
C	Go back to the top of the search loop
	goto 100

	end 
C
