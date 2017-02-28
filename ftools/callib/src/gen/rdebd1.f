*+RDEBD1
c     --------------------------------------------------------
	subroutine rdebd1(iunit,chatter,maxchan, 
     &		telescop,instrume,detnam,filter,areascal, 
     &		iebound,channel,e_min,e_max,rmfversn,ierr)
c     --------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c
c  Reads the EBOUNDS extension for an RMFVERSN=1992a RMF file
c Assumes the extension conforms to HDUVERS2='1.*.*' family
c Currently the following formats are supported -
c HDUVERS2='1.0.0'
c HDUVERS2='1.1.0'
c see OGIP/92-002a
c The HDU CLASS keywords have only been currently introduced thus DO NOT
c have to be present to use this reader.  
c
c  Assumes the FITS is open.
c  !!! Note !!!! File is left open at the end
c      ... close file using FTCLOS, or
c      ... read another extension
c
c  Columns read are ...
c
c  CHANNEL     : Channel numbers
c  E_MIN       : Nominal lower energy bound
c  E_MAX       : Nominal upper energy bound
c
c  Keywords read ...
c
c  TELESCOP : Mission/Telescope name, NOTE: If not present set to UNKNOWN
c  INSTRUME : Instrument/Detector name NOTE: If not present set to UNKNOWN
c  DETNAM   : Specific detector name NOTE: If not present set to NONE
c  FILTER   : Filter in use, if not present set to NONE
c  EFFAREA  : Areascaling factor, if not present set to 1
c  RMFVERSN : RMF version
c  
c --- VARIABLES -------------------------------------------------------
c                                                      
	IMPLICIT NONE
	integer chatter, ierr, maxchan
	integer iunit
	integer iebound, channel(maxchan)
	real areascal
	real e_min(maxchan), e_max(maxchan)
        character*(*) rmfversn
	character*(*) telescop, instrume, detnam, filter
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c Passed parameters
c
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (0 quite,10 normal,>20 silly)
c  TELESCOP      o   : String listing telescope/mission
c  INSTRUME      o   : String listing instrument/detector
c  DETNAM        o   : String listing specific detector name
c  FILTER        o   : String listing instrument filter in use
c  AREA          o   : Area scaling factor
c  IEBOUND       o   : No. channels in the full array
c  CHANNEL       o   : Channel array
c  E_MIN         o   : Array containing min nominal energy bound to each chan
c  E_MAX         o   : Array containing max nominal energy bound to each chan
c  RMFVERSN      o   : RMF version
c  IERR          o   : Error Flag, ierr = 0 okay
c                                  ierr = 1 error finding extension
c                                  ierr = 2 error finding Column number
c                                  ierr = 3 error reading data
c                                  ierr = 4 NAXIS2 not found
c				   ierr = 5 maxchan, array size NOT large enough
c
c --- CALLED ROUTINES --------------------------------------------------
c
c  subroutine FTMRHD     : (FITSIO) Move to extension
c  subroutine FTGKYS     : (FITSIO) Read FITS extension header keyword 
c  subroutine FTGCNO     : (FITSIO) Get column number
c  subroutine FTGCVx     : (FITSIO) Read data in x format
c  subroutine WT_FERRMSG : (CALLIB) Dumps FITSIO Error message etc
c
c --- Compilation & Linking --------------------------------------------
c
c  link with FITSIO & CALLIB & FTOOLS
c
c --- Authors/Modification History: ------------------------------------
c
c Rehana Yusaf (1993 July 15)
c Rehana Yusaf (1993 Oct 27) 1.0.1; Rename from RD_EBD1992a, and
c				    additional argument passed, rmfversn
c 				    Also HDUCLASS is now used to find
c				    extension if not present then extname
c                                   searched for.
c Rehana Yusaf (1993 Nov 10) 1.0.2; Read HDUVERS2 keyword to get rmfversn
c 				    if HDUVERSN not present the read RMFVERSN
c				    previously RMFVERSN was read
c Ian M George (93 Nov 17) 1.1.0; Took out extension searching stuff
c Rehana Yusaf (1994 Jan 11) 1.1.1; Additional argument, maxchan - array
c                                   dimension, compared to Naxis2 and error out
c                                   if too small
c Rehana Yusaf (1994 June 24) 1.1.2;Less verbose;Only printer warnings at
c                                   high chatter. Also add fcecho call
c                                   after channel error check

	character(7) version
	parameter (version = '1.1.2')
*- 
c ----------------------------------------------------------------------
c
c --- INTERNALS ---
c
        character(25) errstr, wrnstr
        character(30) comm
        character(70) desc, errinfo
        integer status,  inull, felem, frow, colnum
        real enull
        logical anyflg, foundcol
c
c --- INITIALISATION ---
c
      ierr = 0
      status = 0 
      errstr = ' ERROR : RDEBD1 Ver '//version//':'
      wrnstr = ' WARNING : RDEBD1 Ver '//version//':'
c
c --- USER INFO ---
c
      IF (chatter.GE.15) THEN
        desc = ' ... using RDEBD1 ' // version
	call fcecho(desc)
      ENDIF
c
c --- READING DATA ---
c

c NAXIS2 ...

      status = 0
      call ftgkyj(iunit,'NAXIS2',iebound,comm,status)
      errinfo = errstr//' reading NAXIS2 value '
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        return
      ENDIF
      IF (iebound.GT.maxchan) THEN
        errinfo = errstr//' Channel array dimension exceeded !'
        call fcecho(errinfo)
        ierr=5
        return
      ENDIF

c RMFVERSN ...

      status = 0
      rmfversn = '  '
      call ftgkys(iunit,'HDUVERS2',rmfversn,comm,status)
      errinfo = wrnstr//' reading HDUVERS2/RMFVERSN'
      IF (rmfversn.EQ.'   ') THEN
        status = 0
        call ftgkys(iunit,'RMFVERSN',rmfversn,comm,status)
        errinfo = wrnstr//' reading RMFVERSN'
      ENDIF
      IF (chatter.GE.30) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF

c TELESCOP ...

      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      errinfo = wrnstr//' reading TELESCOP'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (status.EQ.202) THEN
        telescop = 'UNKNOWN'
      ENDIF 

c INSTRUME ...

      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      errinfo = wrnstr//' reading INSTRUME'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (status.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF 

c FILTER ...

      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      errinfo = wrnstr//' reading FILTER'
      IF (chatter.GE.30) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (status.EQ.202) THEN
        filter = 'NONE'
      ENDIF

c DETNAM ...

      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      errinfo = wrnstr//' reading DETNAM'
      IF (chatter.GE.30) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (status.EQ.202) THEN
        detnam = 'NONE'
      ENDIF   

c EFFAREA ...

      status = 0
      call ftgkye(iunit,'EFFAREA',areascal,comm,status)
      errinfo = wrnstr//' reading EFFAREA'
      IF (chatter.GE.30) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (status.NE.0) THEN
        areascal = 1
      ENDIF

c CHANNEL COLUMN NUMBER ...
 
      status = 0
      foundcol = .true.
      call ftgcno(iunit,.false.,'CHANNEL',colnum,status)
      IF (status.NE.0) THEN
        foundcol = .false.
      ENDIF
      IF (.NOT.foundcol) THEN
        errinfo = errstr//' CHANNEL COLUMN not present'
        call fcecho(errinfo)
        ierr = 2
        return
      ENDIF

c READ CHANNEL COLUMN ...

      status = 0
      frow = 1
      felem = 1
      inull = 0
      call ftgcvj(iunit,colnum,frow,felem,iebound,inull,channel,
     &            anyflg,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' reading CHANNEL column !'
        call fcecho(errinfo)
        ierr = 3
        return
      ENDIF

c E_MIN COLUMN NUMBER ...

      status = 0
      foundcol = .true.
      call ftgcno(iunit,.false.,'E_MIN',colnum,status)
      IF (status.NE.0) THEN
        foundcol = .false.
      ENDIF
      IF (.NOT.foundcol) THEN
        errinfo = errstr//' E_MIN COLUMN not present'
        call fcecho(errinfo)
        ierr = 2
        return
      ENDIF
    
c READ E_MIN ...

      status = 0 
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,iebound,enull,e_min,
     &            anyflg,status) 
      IF (status.NE.0) THEN
        errinfo = errstr//' reading E_MIN column !'
        call fcecho(errinfo)
        ierr = 3
        return
      ENDIF      
      
c E_MAX COLUMN NUMBER ...

      status = 0
      foundcol = .true.
      call ftgcno(iunit,.false.,'E_MAX',colnum,status)
      IF (status.NE.0) THEN
        foundcol = .false.
      ENDIF
      IF (.NOT.foundcol) THEN
        errinfo = errstr//' E_MAX COLUMN not present'
        call fcecho(errinfo)
        ierr = 2
        return
      ENDIF    

c READ E_MAX ...

      status = 0
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,iebound,enull,e_max,
     &            anyflg,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' reading E_MAX column !'
        call fcecho(errinfo)
        ierr = 3
        return
      ENDIF
      IF (chatter.GE.20) THEN
        desc = '      ... read EBOUNDS data '
        call fcecho(desc)
      ENDIF
      return
      end
c --------------------------------------------------------------------
c     END OF RDEBD1
c --------------------------------------------------------------------   
