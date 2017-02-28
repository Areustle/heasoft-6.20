*+RDSPA1
c     -------------------------------------------------------
      subroutine rdspa1(iunit,ph_3,hf,max_dim,
     &            nchan,ierr,chatter)
c     -------------------------------------------------------
c --- VARIABLES ------------------------------------------------------
c
        IMPLICIT NONE
        integer iunit,chatter,max_dim,nchan
	integer ierr
        integer ph_3(max_dim)
	real hf(max_dim)
c
c --- DESCRIPTION -----------------------------------------------------
c
c   This subroutine reads a PSPC spatial gain correction 
c FITS extension.
c !!! Note !!! the oip file is assumed to have been opened, and wound to the 
c              desired location.
c
c PASSED PARAMETERS
c
c  iunit        i   : FORTRAN unit number of open ADC file
c  chatter      i   : chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  ph_3         i   : array containing ph_1 data as described in cal/ros/95-010
c  hf           r   : High-freq Amp for Spatial Gain Correction
c  max_dim      i   : maximum array dimensions
c  nchan        i   : number of rows
c
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
	errstr = ' RDSPA1'//version//' ERROR:'
	wrnstr = ' RDSPA1'//version//' WARNING:'

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         subinfo = ' ... using RDSPA1 Ver '//version
         call fcecho(subinfo)
      ENDIF

      frow = 1
      felem = 1

c --- READ PHA_3 COLUMN ---

      status = 0
      call ftgcno(iunit,.false.,'PH_3',colnum,status)
      subinfo = errstr//' problem finding PH_3 column'
      call wt_ferrmsg(status,subinfo)
      if (status.NE.0) THEN
         ierr = 2
         goto 998
      endif

      inull = 0
      call ftgcvj(iunit,colnum,frow,felem,nchan,inull,
     &            ph_3,anyf,status)
	if(status.NE.0) then
		subinfo = errstr // ' reading PH_3 Data'
		call wt_ferrmsg(status, subinfo)
		ierr = 2
		goto 998
	 endif

c --- WRITE SGC_HF_E ---

      status = 0
      call ftgcno(iunit,.false.,'SGC_HF_E',colnum,status)
      subinfo = errstr//' problem finding SGC_HF_E column'
      call wt_ferrmsg(status,subinfo)
      if (status.NE.0) THEN
         ierr = 2
         goto 998
      endif
      enull = 0
      status = 0
      CALL ftgcve(iunit,colnum,frow,felem,nchan,enull,
     &            hf,anyf,status)
      if(status.NE.0) then
		subinfo = errstr // ' reading SGC_HF_E Data'
		call wt_ferrmsg(status, subinfo)
		ierr = 3
		goto 998
      endif

      if(chatter.GE.20) then
        subinfo = ' ... read the spagain data Extension'
        call fcecho(subinfo)
      endif

998	if(ierr.NE.0) then
	  subinfo = errstr // 'FATAL: Extension not read '
	  call fcecho(subinfo)
	endif

      return
      end
c --------------------------------------------------------------------
c     END OF RDSPA1
c --------------------------------------------------------------------   
