
*+RDADC1
c     -------------------------------------------------------
      subroutine rdadc1(iunit,ph_1,bin_low,bin_high,max_bin,
     &            nchan,ierr,chatter)
c     -------------------------------------------------------
c --- VARIABLES ------------------------------------------------------
c
        IMPLICIT NONE
        integer iunit,chatter,max_bin,nchan
	integer ierr
        integer ph_1(max_bin)
	real bin_low(max_bin),bin_high(max_bin)
c
c --- DESCRIPTION -----------------------------------------------------
c
c   This subroutine reads a PSPC ADC non-linearity correction 
c FITS extension.
c !!! Note !!! the i/p file is assumed to have been opened, and wound to the 
c              desired location. The file is left open at the end of the 
c              newly written extension on return and MUST be closed 
c              using FTCLOS.
c
c PASSED PARAMETERS
c
c  iunit        i   : FORTRAN unit number of open ADC file
c  chatter      i   : chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  ph_1         i   : array containing ph_1 data as described in cal/ros/95-010
c  bin_low      r   : Lower boundary for ADC bin
c  bin_high     r   : Upper boundary  for ADC bin
c  max_bin      i   : maximum array dimensions
c  nchan        i   : number of bins
c
c FORMAT READ
c
c  Each row consists of the following columns/contents:
c
c PH_1        :
c ADCNL_LO    : Lower boundary for ADC bin
c ADCNL_HI    : Upper boundary for ADC bin
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
	errstr = ' RDADC1'//version//' ERROR:'
	wrnstr = ' RDADC1'//version//' WARNING:'

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         subinfo = ' ... using RDADC1 Ver '//version
         call fcecho(subinfo)
      ENDIF

      frow = 1
      felem = 1

c --- READ PHA_1 COLUMN ---
      
      status = 0
      call ftgcno(iunit,.false.,'PH_1',colnum,status)
      subinfo = errstr//' problem finding PH_1 column'
      call wt_ferrmsg(status,subinfo)
      if (status.NE.0) THEN
         ierr = 2
         goto 998
      endif
      enull = 0
      inull = 0
      call ftgcvj(iunit,colnum,frow,felem,nchan,inull,
     & ph_1,anyf,status)
	if(status.NE.0) then
		subinfo = errstr // ' reading PH_1 Data'
		call wt_ferrmsg(status, subinfo)
		ierr = 2
		goto 998
	 endif

c --- READ ADC_LO ---

      status = 0
      call ftgcno(iunit,.false.,'ADC_LO',colnum,status)
      subinfo = errstr//' finding ADC_LO column'
      call wt_ferrmsg(status,subinfo)
      IF (status.NE.0) THEN
        ierr = 2
        goto 998
      ENDIF
      frow = 1
      felem = 1
      CALL ftgcve(iunit,colnum,frow,felem,nchan,enull,
     &            bin_low,anyf,status)
      if(status.NE.0) then
		subinfo = errstr // ' reading ADC_LOW Data'
		call wt_ferrmsg(status, subinfo)
		ierr = 3
		goto 998
      endif

c --- READ BIN_HIGH --- 

      status = 0
      call ftgcno(iunit,.false.,'ADC_HI',colnum,status)
      subinfo = errstr//' finding ADC_HI column'
      call wt_ferrmsg(status,subinfo)
      IF (status.NE.0) THEN
        ierr = 2
        goto 998
      ENDIF

      frow=1
      felem=1
      CALL ftgcve(iunit,colnum,frow,felem,nchan,enull,
     &            bin_high,anyf,status)
      if(status.NE.0) then
          subinfo = errstr // ' reading ADC_HI Data'
          call wt_ferrmsg(status, subinfo)
          ierr = 4
          goto 998
      endif

      if(chatter.GE.20) then
        subinfo = ' ... read the ADC data Extension'
        call fcecho(subinfo)
      endif

998   if(ierr.NE.0) then
        subinfo = errstr // 'FATAL: ADC Extension not read'
        call fcecho(subinfo)
      endif

      return
      end
c --------------------------------------------------------------------
c     END OF RDADC1
c --------------------------------------------------------------------   
