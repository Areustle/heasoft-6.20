*+RDSCAL1
c     -------------------------------------------------------
      subroutine rdscal1(iunit,pi,ga,max_dim,
     &            nchan,ierr,chatter)
c     -------------------------------------------------------
c --- VARIABLES ------------------------------------------------------
c
        IMPLICIT NONE
        integer iunit,chatter,max_dim,nchan
	integer ierr
        integer pi(max_dim)
	real ga(max_dim)
c
c --- DESCRIPTION -----------------------------------------------------
c
c   This subroutine reads a PSPC Window correction - energy dependent
c aplitude FITS extension.
c !!! Note !!! the i/p file is assumed to have been opened, and wound to the 
c              desired location. The file is left open at the end of the 
c              newly written extension on return and MUST be closed 
c              using FTCLOS or another extension written starting with FTCRHD
c	       in order that the mandatory END keyword is written              
c In all cases, the 1.*.* family of formats consists of a BINTABLE extension, 
c with the number of rows equal to the number of channels passed .
c
c PASSED PARAMETERS
c
c  iunit        i   : FORTRAN unit number of open ADC file
c  chatter      i   : chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  pi           i   : array containing pulse invarient channel
c  ga           r   : Energy-dependent amplitude for window correction
c  max_dim      i   : maximum array dimensions
c  nchan        i   : number of rows
c
c FORMAT WRITTEN
c
c  Each row consists of the following columns/contents:
c
c PI          : Pulse invarient channel
c WC_GA_E     : Energy-dependent amplitude for window correction
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
      	integer frow,felem,colnum,inull
        real enull
	character(30) errstr, wrnstr
      	character(70) subinfo
        logical anyf

c Initialization

	ierr = 0
	status = 0
	errstr = ' RDSCAL1'//version//' ERROR:'
	wrnstr = ' RDSCAL1'//version//' WARNING:'

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         subinfo = ' ... using RDSCAL1 Ver '//version
         call fcecho(subinfo)
      ENDIF

      frow = 1
      felem = 1

c --- READ pi COLUMN ---

      status = 0
      call ftgcno(iunit,.false.,'PI      ',colnum,status)
      subinfo = errstr//' problem finding PI column'
      call wt_ferrmsg(status,subinfo)
      if (status.NE.0) THEN
         ierr = 2
         goto 998
      endif
      inull = 0
      call ftgcvj(iunit,colnum,frow,felem,nchan,inull,pi,
     & anyf,status)
	if(status.NE.0) then
		subinfo = errstr // ' reading PI Data'
		call wt_ferrmsg(status, subinfo)
		ierr = 2
		goto 998
	 endif

c --- READ WC_GA_E ---

      status = 0
      call ftgcno(iunit,.false.,'WC_GA_E ',colnum,status)
      subinfo = errstr//' problem finding WC_GA_E column'
      call wt_ferrmsg(status,subinfo)
      if (status.NE.0) THEN
         ierr = 2
         goto 998
      endif
      enull = 0
      CALL ftgcve(iunit,colnum,frow,felem,nchan,enull,ga,
     & anyf,status)
      if(status.NE.0) then
		subinfo = errstr // ' reading WC_GA_E Data'
		call wt_ferrmsg(status, subinfo)
		ierr = 3
		goto 998
      endif

      if(chatter.GE.20) then
        subinfo = 
     &  ' ... read the window correction data Extension'
        call fcecho(subinfo)
      endif

998	if(ierr.NE.0) then
	  subinfo = errstr // 'FATAL: Extension not read '
	  call fcecho(subinfo)
	endif

      return
      end
c --------------------------------------------------------------------
c     END OF RDSCAL1
c --------------------------------------------------------------------   
