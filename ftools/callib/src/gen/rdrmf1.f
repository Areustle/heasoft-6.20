*+RDRMF1
c       ---------------------------------------------------------
        subroutine rdrmf1(iunit, chatter,matext,
     &          telescop, instrume, detnam, filter, areascal,
     &          ichan, ienerg, energ_lo, energ_hi,
     &          imaxgrp, ngrp, F_chan, N_chan,
     &          fmatrix, lo_thresh, maxchan,maxen,
     &          rmfversn,hduclas3,ierr)
c       ---------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c
c Reads the RMF extension for an RMFVERSN = 1992a RMF file
c The file is assumed to conform to the HDUVERS2='1.*.*' family.
c Currently the OGIP formats supported are
c HDUVERS2 = '1.0.0'
c HDUVERS2 = '1.1.0'
c see OGIP/92-002a
c The HDU CLASS keywords have only been currently introduced thus DO NOT
c have to be present to use this reader.
c
c Assumes the FITS file is open.
c !!! NOTE !!! File is left open at end
c     ... close file using FTCLOS, or
c     ... read another extension
c
c Columns read are ...
c
c ENERG_LO      : Low energy bound for row
c ENERG_HI      : High energy bound for row
c N_GRP         : Number of channel subsets for row
c F_CHAN        : Firstchannel in each subset for row
c N_CHAN        : Number of channels for each subset for row
c MATRIX        : (non-zero) Matrix element for row
c
c Keywords read ...
c
c TELESCOP      : Mission/Telescop name , if not present set to UNKNOWN
c INSTRUME      : Instrument/Detector name, if not present set to UNKNOWN 
c DETNAME       : Specific detector name, if not present set to NONE 
c FILTER        : Filter in use, if not present set to NONE
c EFFAREA       : Effective area, if not present,set to 1
c LO_THRESH     : Threshold used to construct the MATRIX, if not present,
c                 set to 0
c HDUCLAS3      : Values describing data, OGIP approved - REDIST,FULL,
c		  DETECTOR
c RMFVERSN      : OGIP RMF version
c
c --- VARIABLES ------------------------------------------------------
c          
	IMPLICIT NONE
	integer chatter, ierr, maxchan,maxen
	integer iunit
	integer ichan, ienerg, imaxgrp
	integer ngrp(maxen), F_chan(maxen,*)
	integer N_chan(maxen,*)
	real areascal, lo_thresh
	real energ_lo(maxen), energ_hi(maxen)
	real fmatrix(maxchan,maxen)
        character*(*) rmfversn
        character*(*) hduclas3
	character*(*) telescop, instrume, detnam, filter
        character*(*) matext
c 
c --- VARIABLE DIRECTORY ----------------------------------------------
c 
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  MATEXT        o   : extension name SPECRESP/MATRIX
c  TELESCOP      o   : String listing telescope/mission
c  INSTRUME      o   : String listing instrument/detector
c  DETNAM        o   : String listing specific detector name   
c  FILTER        o   : String listing instrument filter in use
c  AREASCAL      o   : Area scaling factor
c  RMFVERSN      o   : RMF version
c  HDUCLAS3      o   : Hduclas3 keyword value
c  ICHAN         o   : No. channels in the full array
c  IENERG        o   : No. energy bins
c  ENERG_LO      o   : Array containing lower bound to each energy bin
c  ENERG_HI      o   : Array containing upper bound to each energy bin
c  NGRP          o   : Array containing no. channel subsets at each energy
c  IMAXGRP       o   : Max no. grps in any given row
c  F_CHAN        o   : Array containing 1st chan of each subset at each energy
c  N_CHAN        o   : Array containing no. chans within each subset 
c                           at each energy
c  FMATRIX       o   : Array containing the full matrix
c  LO_THRESH     o   : The lower threshold used to construct the matrix
c  IERR            o : Error flag (0 = OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c  subroutine WT_FERRMSG : (CALLIB) Writes FITSIO error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c
c Rehana Yusaf (1993 July 26 : 1.0.0; WT_RMF1992A.F (CALLIB) used as basis
c Rehana Yusaf (1993 Oct 27 ) 1.0.1; added arguments for rmfversn and 
c				     hduclas3, also the name has been 
c				     changed from rd_rmf1992a.
c				     In addition if extname is not found 
c	                             HDUCLAS1='RESPONSE' is searched for.
c				     and HDUCLAS2='RSP_MATRIX'
c Rehana Yusaf (1993 Nov 10) 1.0.2;  HDUVERS2 is read to obtain rmfversn
c				     if HDUVERSN not present then RMFVERSN
c				     is read. Prev' only RMFVERSN read
c Ian M George (93 Nov 17) 1.1.0     Took out searching for correct xtens
c					(this is now responsibilty of main)
c Rehana Yusaf (94 Jan 11) 1.1.1;    Remove mvalues array and read matrix
c				     matrix values straight into fmatrix
c
c Rehana Yusaf (1994 June 24)1.1.1;  Make the routine less verbose, that
c                                    is only print warnings at chatter>30 
	character(7) version
	parameter (version = '1.1.1')
*-
c ------------------------------------------------------------------------- 
c
c Internals
c
	integer status
	integer i, k, siz_mat, siz_ngrp
	integer ie,j, ic, frow,felem,colnum,inull
        character(70) desc
	character(80) message
        character(27)  errstr, wrnstr
        character(30) comm
	integer ivalues(10)
	real enull
        logical anyflg
c
c --- INITIALISE ---
c
      ierr = 0
      status = 0
      errstr = ' ERROR:RDRMF1 Ver '//version//':'
      wrnstr = ' WARNING :RDRMF1 Ver '//version//':'
	
c
c --- GIVE USER INFO IF REQUESTED ---
c
      IF (chatter.GE.15) THEN
        message = ' ... using RDRMF1 '// version
        call fcecho(message)
      ENDIF

c
c --- READING KEYWORD VALUES ---
c
c     NAXIS2 ...
c
      status = 0
      call ftgkyj(iunit,'NAXIS2',ienerg,comm,status)
      message = errstr//' reading NAXIS2 value '
      IF (status.NE.0) THEN
        ierr = 4
        call wt_ferrmsg(status,message)
        return
      ENDIF
      IF (ienerg.GT.maxen) THEN
        ierr = 4
        message = errstr//' Energy Array dimension is too small !'
        call fcecho(message)
        return
      ENDIF

c HDUCLAS3 ...

      hduclas3 = '  '
      status = 0
      call ftgkys(iunit,'HDUCLAS3',hduclas3,comm,status)
      message = wrnstr//' reading HDUCLAS3 '
      IF (chatter.GE.30) THEN
        call wt_ferrmsg(status,message)
      ENDIF

c RMFVERSN ...

      rmfversn = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',rmfversn,comm,status)
      message = wrnstr//' reading HDUVERS2/RMFVERSN'
      IF (chatter.GE.30) THEN
        call wt_ferrmsg(status,message)
      ENDIF         
      IF (rmfversn.EQ.'  ') THEN
        status = 0
        call ftgkys(iunit,'RMFVERSN',rmfversn,comm,status) 
        message = wrnstr//' reading RMFVERSN'
        IF (chatter.GE.30) THEN
          call wt_ferrmsg(status,message)
        ENDIF
      ENDIF

c     TELESCOP ...

      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      IF (chatter.GE.20) THEN
        message = wrnstr//' reading TELESCOP '
        call wt_ferrmsg(status,message)      
      ENDIF
      IF (status.EQ.202) THEN
        telescop = 'UNKNOWN'
      ENDIF 	

c     INSTRUME ...

      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      IF (chatter.GE.20) THEN
        message = wrnstr//' reading INSTRUME '
        call wt_ferrmsg(status,message)
      ENDIF
      IF (status.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF         

c     FILTER ...

      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      IF (chatter.GE.30) THEN
        message = wrnstr//' reading FILTER '
        call wt_ferrmsg(status,message)
      ENDIF
      IF (status.EQ.202) THEN
        filter = 'NONE'
      ENDIF         

c     DETNAM ...

      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      IF (chatter.GE.30) THEN
        message = wrnstr//' reading DETNAM '
        call wt_ferrmsg(status,message)
      ENDIF
      IF (status.EQ.202) THEN
        detnam = 'NONE'
      ENDIF

c     EFFAREA ...

      status = 0
      call ftgkye(iunit,'EFFAREA',areascal,comm,status)
      IF (chatter.GE.30) THEN
        message = wrnstr//' reading EFFAREA '
        call wt_ferrmsg(status,message)
      ENDIF
      IF (status.NE.0) THEN
        areascal = 1
      ENDIF

c     LO_THRESH ...

      status = 0
      call ftgkye(iunit,'LO_THRESH',lo_thresh,comm,status)
      IF (chatter.GE.30) THEN
        message = wrnstr//' reading LO_THRESH '
        call wt_ferrmsg(status,message)
      ENDIF
      IF (status.NE.0) THEN
        lo_thresh = 0
      ENDIF

c     DETCHANS ...

      status = 0
      call ftgkyj(iunit,'DETCHANS',ichan,comm,status)
      message = errstr//' reading DETCHANS '
      call wt_ferrmsg(status,message)   
      IF (status.NE.0) THEN
        ierr = 1
        return
      ENDIF
      IF (ichan.GT.maxchan) THEN
        ierr = 4
        message = errstr//' Channel Array dimension is too small !'
        call fcecho(message)
        return
      ENDIF

      IF (chatter.GE.20) THEN
        desc = '     ... read KEYWORD values '
        call fcecho(desc)
      ENDIF
c
c --- READ DATA ---
c
      
c     ENERG_LO ...

      frow = 1
      felem = 1
      status = 0
      call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
      IF (status.NE.0) THEN
         message = errstr//' ENERG_LO column not present !'
         call fcecho(message)
         ierr = 4
         return
      ENDIF
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,ienerg,enull,energ_lo,
     &            anyflg,status)      
      IF (status.NE.0) THEN
        message = errstr//' reading ENERG_LO column'
        call fcecho(message)
        ierr = 1
        return
      ENDIF

c     ENERG_HI ...
         
      status = 0
      call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
      If (status.NE.0) THEN
         message = errstr//' ENERG_HI column not present !'
         call fcecho(message)
         ierr = 4
         return
      ENDIF
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,ienerg,enull,energ_hi,
     &            anyflg,status)
      IF (status.NE.0) THEN
        message = errstr//' reading ENERG_HI column'
        call fcecho(message)
        ierr = 1
        return
      ENDIF      

c     NGRP ...

      status = 0
      call ftgcno(iunit,.false.,'N_GRP',colnum,status)
      If (status.NE.0) THEN
         message = errstr//' N_GRP column not present !'
         call fcecho(message)
         ierr = 4
         return
      ENDIF
      inull = 0
      call ftgcvj(iunit,colnum,frow,felem,ienerg,inull,ngrp,
     &            anyflg,status)
      IF (status.NE.0) THEN
        message = errstr//' reading N_GRP column'
        call fcecho(message)
        ierr = 1
        return
      ENDIF    

c     F_CHAN ...

      status = 0
      call ftgcno(iunit,.false.,'F_CHAN',colnum,status)
      If (status.NE.0) THEN
         message = errstr//' F_CHAN column not present !'
         call fcecho(message)
         ierr = 4
         return
      ENDIF
      do i=1,ienerg
        inull = 0
        call ftgcvj(iunit,colnum,i,felem,ngrp(i),inull,
     &            ivalues,anyflg,status)
        IF (status.NE.0) THEN
          message = errstr//' reading F_CHAN column'
          call fcecho(message)
          ierr = 1
          return
        ENDIF 
        do j=1,ngrp(i)
           F_chan(i,j) = ivalues(j)
        enddo
      enddo

c     N_CHAN ...

      status = 0
      call ftgcno(iunit,.false.,'N_CHAN',colnum,status)
      If (status.NE.0) THEN
         message = errstr//' N_CHAN column not present !'
         call fcecho(message)
         ierr = 4
         return
      ENDIF
      do i=1,ienerg
        inull = 0
        call ftgcvj(iunit,colnum,i,felem,ngrp(i),inull,
     &              ivalues,anyflg,status)
        IF (status.NE.0) THEN
          message = errstr//' reading N_CHAN column'
          call fcecho(message)
          ierr = 1
          return
        ENDIF  
        do j=1,ngrp(i)
          N_chan(i,j) = ivalues(j)
        enddo
      enddo

c     MATRIX ...


c     initialise matrix array ...

      do i=1,ichan
        do j=1,ienerg
          fmatrix(i,j) = 0.0
        enddo
      enddo

c     imaxgrp ...

      siz_ngrp = 0
      do i=1,ienerg
        siz_ngrp = MAX(siz_ngrp,ngrp(i))         
      enddo
      imaxgrp = siz_ngrp

c     read matrix ...

      status = 0
      call ftgcno(iunit,.false.,'MATRIX',colnum,status)
      If (status.NE.0) THEN
         message = errstr//' MATRIX column not present !'
         call fcecho(message)
         ierr = 4
         return
      ENDIF
      do ie=1,ienerg
        siz_mat = 0
        do j=1,ngrp(ie)
          siz_mat = siz_mat + N_chan(ie,j)
        enddo
        k = 0
        do j=1,ngrp(ie)
          do ic=F_chan(ie,j),F_chan(ie,j)+N_chan(ie,j) - 1
            k=k+1
            enull = 0
            call ftgcve(iunit,colnum,ie,k,1,enull,
     &              fmatrix(ic,ie),anyflg,status)
            IF (status.NE.0) THEN
              message = errstr//' reading MATRIX column'
              call wt_ferrmsg(status,message)
              ierr = 1
              return
            ENDIF
          enddo
        enddo
      enddo
      IF (chatter.GE.20) THEN
        desc = '     ... read RSP_MATRIX data'
        call fcecho(desc)
      ENDIF  
      return
      end
c ----------------------------------------------------------------------
c     END OF RDRMF1
c ----------------------------------------------------------------------

  
