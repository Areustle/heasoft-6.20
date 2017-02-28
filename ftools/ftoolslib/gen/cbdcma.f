*+CBDCMA
		subroutine cbdcma(bval,nbval,cbdval,ncbdval,chatter, 
     &           conflict, pmatch,valtest, unitstest,  status)
C-----------------------------------------------------------------------
C Description: Compares the cbd strings contained in the bval and cbdval
C              The variable "conflict" is returned true whenever
C              NEED TO DECIDE ON THE LOGIC AND DOCUMENT IT HERE!
C
C Arguments:   bval    (i): array containing the cbd strings 
C                    from the caldb.indx file to which
C                    the cbdval strings (from the calibration 
C                    file)will be compared
C       nbval   (i): number of elements contained in bval
C       cbdval  (i): array containing the cbd strings from the 
C                    calibration file that get
C                    compared to the bval cbd strings (from the 
C                    caldb.indx file)
C       ncbdval (i): number of elements contained in cbdval
C       chatter (i): value of the chatter parameter
C       conflict(r): TRUE if one of three conditions is met
C                    1. all elements of either the cbdval or 
C                       bval arrays contain only the value 'NONE'. 
C                    2. the boundaries of one array do not appear
C                       as the boundaries of the other array.
C                    3. a boundary/value element is found in one
C                       array which overlaps a boundary/value
C                       element found in the other array.
C       pmatch (r):  an array of nbval elements 
C                    listing the parameter name matches
C       valtest(r):  an array  of nbval elements listing the
C                    results of the test of the boundary  values 
C                    for matching parameters
C       unitstest(r): an array of nbval elements listing the
C                    results of the test of units for the boundary
C                    parameters
C       status  (r): the success status of this routine. 0 = OK
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar May 24, 1994 -- Original Version
C              Ron Zellar Jun 30, 1994 -- added nbval and ncbdval
C                                         arguments and included evalu-
C                                         ation of condition (2) above.
C   Mike Corcoran Nov 2005 -- CHANGES:
C            1) TEST that ALL VALUES OF BVAL AND CBDVAL arrays are
C            filled with NONE, not just the first value
C
C   Mike Corcoran Nov 2005 -- CHANGES:
C            Significantly changed the logic of this subroutine.
C            First it compares every NON-NONE parameter name in the bval
C            array with every NON-NONE parameter in the cbdval array, and
C            creates an array of parameter matches.  Then for every matching
C            parameter, it checks to see whether the values overlap.  If
C            there's at least one parameter who's values don't overlap,
C            then conflict=.false.; if ALL the parameters overlap, then
C            conflict=.true.
C
C-----------------------------------------------------------------------
*- Version 2.0

        implicit none
        integer nbval,ncbdval
        character*(*)bval(nbval),cbdval(ncbdval)
        character(160) context
        character(80) cbdvaltest, bvaltest
        integer pmatch(nbval),chatter, fcstln,pmatchnum,conflicttest
        logical conflict
        logical valtest(nbval), unitstest(nbval)
        integer status

	integer i,j,errstat,test
	logical pres, vres, ures  

        
C	Initialize the result variable and status variable
C	conflict = .true. 
	status = 0
	do 5 i=1,nbval
	   pmatch(i)   =-1
	   valtest(i)  =.false.                                    
	   unitstest(i)=.false.
    5    CONTINUE

C       If either array is completely filled with 'NONE' values then
C       that array is valid for all boundaries, hence an overlap 
C       and set result to .true.
        test=0
        do 10 i=1,nbval
          if (bval(i).ne.'NONE') test=test+1
   10   CONTINUE 
C        write(*,*)'TEST =', test
        if (test.eq.0) then 
              conflict=.true.
	      write(*,*) 'All Boundary Values in the cif are "NONE"
     &        for this calibration type: CONFLICT'
	      return
	endif
	test=0
	do 20 i=1,ncbdval
	  if (cbdval(i).ne.'NONE') test=test+1
   20   CONTINUE 
          if (test.eq.0) then 
              conflict=.true.
	      write(*,*) 'All Boundary Values in the Cal File are "NONE"
     &         : CONFLICT'
	      return
	 endif                                                                


C
C   Compare the array values which are not equal to 'NONE'
C

C       pmatchnumn is the number of matching parameters
        pmatchnum=0 
	do 501 i=1,nbval
	  if (bval(i).ne.'NONE') then             
	    do 500 j=1,ncbdval
            if (cbdval(j).ne.'NONE') then  
	      call cbdcom(bval(i),cbdval(j),pres,vres,ures,errstat)
	      if (errstat .ne. 0) then
	           status = 1
	           return
	      endif
              if (pres) then
	          pmatch(i)     =j
                  valtest(i)    =vres    
                  unitstest(i)  =ures
		 pmatchnum=pmatchnum+1
              endif
           endif
500	  continue
         endif
	      
C             MFC

C	      The condition 'pres=TRUE && vres=FALSE --> return'                                        
C             forces non-duplication of parameter boundaries within
C             an array.

C	      If pres is true then we've either found condition (3)
C             or the two arrays are valid for the same parameter but
C             have different values.

C	      if (pres) then
C
C	           If vres is true then we've found condition (3).
C	           Otherwise, the arrays are independent and result
C                  is false.
C                     
C	           if (vres) result = .true.
C
C	           return
C
C	      endif

         
C       write(*,*) i, pmatch(i),valtest(pmatch(i))
501	continue
502	continue

C	If we got here then pres never became true and there are no
C       explicit boundary matches between the two arrays. Thus both
C       arrays claim validity over the same parameter space and 
C       result should be true.
C
C	result = .true.
C

C      
C      Now set result to true if for any matching parameter, values also
C      match (this assumes that the units are correct, but they 
C      shouldn't be different in the boundaries for the same parameter 
C      names)
C
       conflicttest=0
          if(chatter.ge.5) then
                  write(*,*) 'Conflict TEST'
                  write(*,*) i,pmatch(i),valtest(i)
                  endif
       do 510 i=1,nbval
          if (pmatch(i).gt.0) then 
             if (valtest(i)) then 
               conflicttest=conflicttest+1
               if (chatter.ge.1) then
                 cbdvaltest=cbdval(pmatch(I))
                 cbdvaltest=cbdvaltest(:fcstln(cbdvaltest))
                 bvaltest=bval(i)
                 bvaltest=bvaltest(:fcstln(bvaltest))
		write(context,'(a)') 'File Parameter '//cbdvaltest
                 call fcecho(context)
                 write(context,'(a)')' value overlaps CIF parameter '//
     &           bvaltest
                 call fcecho(context)
                 endif
             else 
                 if (chatter.ge.1) then
                    cbdvaltest=cbdval(pmatch(I))
                    cbdvaltest=cbdvaltest(:fcstln(cbdvaltest))
                    bvaltest=bval(i)
                    bvaltest=bvaltest(:fcstln(bvaltest))
                    write(context,'(a)') 'File Parameter '//cbdvaltest
                    call fcecho(context)
                    write(context,'(a)') ' value DOES NOT overlap CIF '
     &               //bvaltest
                    call fcecho(context)
c                    write(*,*),valtest(i),i
                    endif

             endif
	 endif

C     if all the matching parameters have overlapping values then 
C	conflict is true; otherwise there's at least one parameter
C	who's values don't overlap which means conflict=.false.

       if (conflicttest.eq.pmatchnum) then 
          conflict=.true. 
	  else 
           conflict=.false.
	  endif
C        if (chatter.ge.1) write(*,*) 'conflicttest,pmatchnum,conflict ',
C     &     conflicttest,pmatchnum,conflict
 510   CONTINUE
       if (chatter.ge.5) then 
          write(*,*) 'PMATCH Array'
          write(*,*) pmatch
          write(*,*) 'Valtest Array'
          write(*,*) valtest
          write(*,*) 'Unitstest Array'
          write(*,*) unitstest
          write(*,*) 'Conflict = ',conflict      
       endif
       return
       end 
C
	
