*+RDGKOR1
c     -------------------------------------------------------
      subroutine rdgkor1(iunit,y_1,lf,hf,max_dim,nrows,
     &            instrume,ierr,chatter)
c     -------------------------------------------------------
c --- VARIABLES ------------------------------------------------------
c
        IMPLICIT NONE
        character*(*) instrume
        integer iunit,chatter,max_dim
	integer ierr,nrows
        real y_1(max_dim)
	real lf(max_dim),hf(max_dim)
c
c --- DESCRIPTION -----------------------------------------------------
c
c   This subroutine reads PSPC Gain kor FITS extension.
c !!! Note !!! the i/p file is assumed to have been opened, and wound to the 
c              desired location.
c
c PASSED PARAMETERS
c
c  iunit        i   : FORTRAN unit number of open ADC file
c  chatter      i   : chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  y_1          r   : array containing y_1 data as described in cal/ros/95-010
c  lf           r   : Pos - dependent low-frq term in SG correction
c  hf           r   : Pos - dependent high-fre term in SG correction
c  max_dim      i   : maximum array dimensions
c
c AUTHORS/MODIFICATION HISTORY
c
c   Rehana Yusaf (1.0.0: 1995 Sept)
      character(5) version
      parameter (version = '1.0.0')
*-
c -----------------------------------------------------------------------
c Internals 
      	integer status
      	integer frow,felem,colnum
        real enull
        logical anyf
	character(30) errstr, wrnstr,comm
      	character(70) subinfo

c Initialization

	ierr = 0
	status = 0
	errstr = ' RDGKOR1'//version//' ERROR:'
	wrnstr = ' RDGKOR1'//version//' WARNING:'

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         subinfo = ' ... using RDGKOR1 Ver '//version
         call fcecho(subinfo)
      ENDIF

      frow = 1
      felem = 1

c --- READ INSTRUME ---

      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      subinfo = errstr//' reading INSTRUME keyword'
      call wt_ferrmsg(status,subinfo)

c --- READ Y_1 COLUMN ---

      status = 0
      call ftgcno(iunit,.false.,'Y_1',colnum,status)
      subinfo = errstr//' problem finding Y_1 column'
      call wt_ferrmsg(status,subinfo)
      if (status.NE.0) THEN
         ierr = 2
         goto 998
      endif
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,nrows,
     &            enull,y_1,anyf,status)
	if(status.NE.0) then
		subinfo = errstr // ' reading Y_1 Data'
		call wt_ferrmsg(status, subinfo)
		ierr = 2
		goto 998
	 endif

c --- read SGC_LF_Y ---

      status = 0
      call ftgcno(iunit,.false.,'SGC_LF_Y',colnum,status)
      subinfo = errstr//' problem finding SGC_LF_Y column'
      call wt_ferrmsg(status,subinfo)
      if (status.NE.0) THEN
         ierr = 2
         goto 998
      endif
      enull = 0
      CALL ftgcve(iunit,colnum,frow,felem,nrows,enull,
     &            lf,anyf,status)
      if(status.NE.0) then
		subinfo = errstr // ' reading SGC_LF_Y Data'
		call wt_ferrmsg(status, subinfo)
		ierr = 3
		goto 998
      endif

c --- READ SGC_HF_Y --- 

      status = 0
      call ftgcno(iunit,.false.,'SGC_HF_Y',colnum,status)
      subinfo = errstr//' problem finding SGC_HF_Y column'
      call wt_ferrmsg(status,subinfo)
      if (status.NE.0) THEN
         ierr = 2
         goto 998
      endif
      enull = 0
        frow=1
        felem=1
        CALL ftgcve(iunit,colnum,frow,felem,nrows,enull,
     &   hf,anyf,status)
        if(status.NE.0) then
          subinfo = errstr // ' reading SGC_HF_Y Data'
          call wt_ferrmsg(status, subinfo)
          ierr = 4
          goto 998
        endif

	if(chatter.GE.20) then
	   subinfo = ' ... read the GKOR data Extension'
	   call fcecho(subinfo)
	endif

998	if(ierr.NE.0) then
	  subinfo = errstr // 'FATAL: Extension not read'
	  call fcecho(subinfo)
	endif

      return
      end
c --------------------------------------------------------------------
c     END OF RDGKOR1
c --------------------------------------------------------------------   
