
*+RDOHT1
c     ---------------------------------------------------------------
      subroutine rdoht1(iunit,telescop,instrume,filter,
     &                  texpos,areascal,detchans,chantyp,nrad,theta_min, 
     &			theta_max,sohist,bohist,ierr,chatter)
c     ---------------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------------
c
c This subroutine reads an
c HDUCLAS = OGIP
c HDUCLAS1=SPECTRUM
c HDUCLAS2=DETECTOR 
c FITS extension, the file is assumed to be at desired location.
c !!! Note !!! the i/p file is assumed to have been opened, and is left 
c open at the end. Close the file using FTCLOS or read another extension.
c
c The HDUVERS1 format consists of a BINTABLE extension,with
c the number of rows equal to the number of off-axis angles passed, and
c the following columns/contents:
c
c  THET_MIN - (real) theta min for this histogram bin
c  THET_MAX - (real) theta min for this histogram bin
c  SOU_FRAC - (real) weighting factor for SOURCE region
c  BKG_FRAC - (real) weighting factor for BKGD region
c
c The following keywords are also read :
c
c  INSTRUME= Name of the instrument 
c  FILTER  - Name of filter in use (if any)
c  EXPOSURE- Total exposure (s) for entire integration (incl deadtime etc)
c  AREASCAL- Area scaling factor for PHA dataset           
c  DETCHANS- Max no. of detector channels in integration
c  CHANTYPE- Type of PHA channels in use ('PHA' or 'PI'[usual])
c                                                                  
c --- VARIABLES ---------------------------------------------------------
c
        IMPLICIT NONE
        integer iunit,chatter
	integer detchans, ierr, nrad
	real texpos,areascal
	real theta_min(*), theta_max(*)
	real sohist(*), bohist(*)
	character(16) instrume,filter,telescop
	character(16) chantyp
c 
c --- VARIABLE DIRECTORY -------------------------------------------------
c
c Passed Parameters ...
c
c  IUNIT       int  i: FORTRAN unit number of open PHA file
c  CHATTER     int  i: chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  TELESCOP    char o: Telescope name
c  INSTRUME    char o: Instrument name 
c  FILTER      char o: Name of filter
c  TEXPOS      real o: Exposure time
c  AREASCAL    real o: Area scaling factor
c  DETCHANS    int  o: Max number of detector channels
c  CHANTYPE    char o: Type of channels
c  NRAD        int  o: Number of rows of data
c  THETA_MIN   real o: Theta min for this histogram bin
c  THETA_MAX   real o: Theta max for this histogram bin
c  SO_HIST     real o: Weighting factors for source region
c  SO_BIST     real o: Weighting fcator for BKGD region
c  IERR        int  o: Error flag, 0 is okay 
c                          
c --- CALLED ROUTINES ---------------------------------------------
c
c FTMRHD        : (FITSIO) Move to extension
c FTGKY(sije)   : (FITSIO) Read extension header keyword, 
c                 s for string, j for integer etc
c FTGCNO        : (FITSIO) Get column number
c FTGCVx        : (FITSIO) Read data in x format
c WT_FERRMSG    : (CALLIB) Dumps FITSIO and routine error message
c 
c --- LINK/COMPILATION --------------------------------------------
c 
c FITSIO and CALLIB
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf 1.0.0 ; WT_PSCC1992A.F (IMG) used as a basis
c Rehana Yusaf 1.1.0 (1994 Feb 7) ; Change name to RDOHT1, no longer
c                                   ROSAT specific
c Ian M George 1.1.1 (94 Aug 17) deleted unused variables (dtype,htype,nhdu)
*-
      character(7) version
      parameter (version = '1.1.1')
c
c --- INTERNALS ---------------------------------------------------
c
      	integer frow,felem,colnum,status
        real enull
	character(30) errstr, wrnstr,comm
      	character(70) subinfo,errinfo
        logical anyflg
c
c --- INITIALISATION ---
c
	ierr = 0
	status = 0
	errstr = ' ERROR:RDOHT1 Ver '//version//':'
	wrnstr = ' WARNING:RDOHT1 Ver '//version//':'
c
c --- USER INFO ---
c
      IF (chatter.GE.15) THEN
         subinfo = ' ... using RDOHT1 Ver '//version
         call fcecho(subinfo)
      ENDIF
c
c --- READ HEADER KEYWORDS ---
c

c NAXIS2 ...

      status = 0
      call ftgkyj(iunit,'NAXIS2',nrad,comm,status)
      errinfo = errstr//' reading NAXIS2 value '
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        return
      ENDIF

c TELESCOPE ...

      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      errinfo = errstr//' reading TELESCOP'
      call wt_ferrmsg(status,errinfo)
      IF (status.EQ.202) THEN
        telescop = 'UNKNOWN'
        IF (chatter.GE.5) THEN
          subinfo = wrnstr//' Telescope keyword not present,'
          call fcecho(subinfo)
        ENDIF
      ENDIF     
      

c INSTRUME ...

      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      errinfo = errstr//' reading INSTRUME'
      call wt_ferrmsg(status,errinfo)
      IF (status.EQ.202) THEN
        instrume = 'UNKNOWN'
        IF (chatter.GE.5) THEN
           subinfo = wrnstr//' there is no INSTRUMENT keyword'
           call fcecho(subinfo)
        ENDIF
      ENDIF

c FILTER ...

      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      errinfo = errstr//' reading FILTER'
      IF (chatter.GE.15) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (status.EQ.202) THEN
        filter = 'NONE'
      ENDIF   

c DETCHANS ...

      status = 0
      call ftgkyj(iunit,'DETCHANS',detchans,comm,status)
      errinfo = errstr//' reading DETCHANS'
      call wt_ferrmsg(status,errinfo)

c EXPOSURE ...

      status = 0
      call ftgkye(iunit,'EXPOSURE',texpos,comm,status)
      errinfo = errstr//' reading EXPOSURE'
      call wt_ferrmsg(status,errinfo) 
c
c --- READ DATA, IF PRESENT ---
c

      IF (nrad.EQ.0) THEN
        IF (chatter.GE.20) THEN
           subinfo = ' There is no data in the DETECTOR extension'
           call fcecho(subinfo)
        ENDIF
        return
      ENDIF

c THETA_MIN column number ...

      status = 0
      call ftgcno(iunit,.false.,'THET_MIN',colnum,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' THET_MIN column not present'
        call fcecho(errinfo)
        ierr = 2
        return
      ENDIF

c READ THETA_MIN COLUMN ...

      status = 0
      frow = 1
      felem = 1
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,nrad,enull,theta_min,
     &            anyflg,status)
      errinfo = errstr//' reading THETA_MIN column'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 3
        return
      ENDIF
 
c THETA_MAX column number ...

      status = 0
      call ftgcno(iunit,.false.,'THET_MAX',colnum,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' THET_MAX column not present'
        call fcecho(errinfo)
        ierr = 2
        return
      ENDIF

c READ THETA_MAX COLUMN ...

      status = 0
      frow = 1
      felem = 1
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,nrad,enull,theta_max,
     &            anyflg,status)
      errinfo = errstr//' reading THETA_MAX column'
      call wt_ferrmsg(status,errinfo) 
      IF (status.NE.0) THEN
        ierr = 3
        return
      ENDIF   

c SOU_FRAC column number ...

      status = 0
      call ftgcno(iunit,.false.,'SOU_FRAC',colnum,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' SOU_FRAC column not present'
        call fcecho(errinfo)
        ierr = 2
        return
      ENDIF

c READ SOU_FRAC COLUMN ...

      status = 0
      frow = 1
      felem = 1
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,nrad,enull,sohist,
     &            anyflg,status)
      errinfo = errstr//' reading SOU_FRAC column'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 3
        return
      ENDIF                  

c BKG_FRAC column number ...

      status = 0
      call ftgcno(iunit,.false.,'BKG_FRAC',colnum,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' BKG_FRAC column not present'
        call fcecho(errinfo)
        ierr = 2
        return
      ENDIF

c READ BKG_FRAC COLUMN ...

      status = 0
      frow = 1
      felem = 1
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,nrad,enull,bohist,
     &            anyflg,status)
      errinfo = errstr//' reading BKG_FRAC column'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN       
        ierr = 3
        return
      ENDIF
      IF (chatter.GE.20) THEN
        subinfo = '     ... read DETECTOR extension'
        call fcecho(subinfo)
      ENDIF
      return
      end
c ---------------------------------------------------------------------
c     END OF RDOHT1
c ---------------------------------------------------------------------

