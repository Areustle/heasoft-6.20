*+RDRMF4
      SUBROUTINE RDRMF4(Iunit,Chatter,Qorder,Maxen,Maxgrp,Maxelt,
     &                  Rmfversn,Hduclas3,Telescop,Instrume,Detnam,
     &                  Filter,Areascal,Chantype,Flchan,Numchn,
     &                  Nenerg,Numgrp,Numelt,Energ_lo,Energ_hi,Ngrp,
     &                  F_chan,N_chan,Isorder,Order,Fmatrix,Lo_thresh,
     &                  Ierr)
 
      IMPLICIT NONE
      INTEGER Maxelt , Maxen , Maxgrp
      INTEGER Chatter , Ierr 
      INTEGER Iunit , Flchan
      INTEGER Numchn, Nenerg , Numgrp , Numelt
      INTEGER Ngrp(Maxen) , F_chan(Maxgrp)
      INTEGER N_chan(Maxgrp), Order(Maxgrp)
      REAL Areascal , Lo_thresh
      REAL Energ_lo(Maxen) , Energ_hi(Maxen)
      REAL Fmatrix(Maxelt)
      CHARACTER*(*) Chantype
      CHARACTER*(*) Rmfversn
      CHARACTER*(*) Hduclas3
      CHARACTER*(*) Telescop , Instrume , Detnam , Filter
      LOGICAL Qorder, Isorder
 
c --- DESCRIPTION -----------------------------------------------------
c
c Reads the RMF extension for an RMF file conforming to the HDUVERS='1.*.*' 
c family.
c Currently the OGIP formats supported are
c HDUVERS2 = '1.0.0'
c HDUVERS2 = '1.1.0'
c HDUVERS2 = '1.2.0'
c HUDVERS  = '1.3.0'
c see OGIP/92-002a
c
c Assumes the FITS file is open.
c !!! NOTE !!!
c     - The format definitions prior to HDUVERS2 = '1.2.0' did not require
c       TLMIN/MAX keywords for the F_CHAN column. This leads to an incorrect
c       indexing for the fmatrix array for detectors whose 1st channel is
c       numbered zero. This code will be able to correct for this bug for
c       matrices written using formats prior to HDUVERS2 = '1.2.0' only if
c       there happens to be a row in the BINTABLE which includes F_CHAN = 0.
c       This is often the case, but cannot be guaranteed.
c     - File is left open at end
c       ... close file using FTCLOS, or
c       ... read another extension
c
c Columns read are ...
c  ENERG_LO      : Low energy bound for row
c  ENERG_HI      : High energy bound for row
c  N_GRP         : Number of channel subsets for row
c  F_CHAN        : Firstchannel in each subset for row
c  N_CHAN        : Number of channels for each subset for row
c  MATRIX        : (non-zero) Matrix element for row
c  ORDER         : (optional) The grating order for the channel subset
c Keywords read ...
c  TELESCOP      : Mission/Telescop name , if not present set to UNKNOWN
c  INSTRUME      : Instrument/Detector name, if not present set to UNKNOWN
c  DETNAME       : Specific detector name, if not present set to NONE
c  FILTER        : Filter in use, if not present set to NONE
c  TLMIN/MAX	 : (for F_CHAN col) for legal min & max for channel numbering
c  EFFAREA       : Effective area, if not present,set to 1
c  CHANTYPE      : Type of detector channel in use (PHA, PI)
c  LO_THRES      : Threshold used to construct the MATRIX, if not present,
c                  set to 0
c  HDUCLAS3      : Values describing data, OGIP approved - REDIST,FULL,
c		   DETECTOR
c  HDUVERS       : OGIP RMF version (HDUVERS2 in older files)
c
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  QORDER        i   : True if ORDER column to be read
c  MAXEN         i   : Size of energy arrays
c  MAXGRP        i   : Size of channel groups arrays
c  MAXELT        i   : Size of FMATRIX array
c  RMFVERSN        o : RMF version (HDUVERS keyword value)
c  HDUCLAS3        o : Hduclas3 keyword value
c  TELESCOP        o : String listing telescope/mission
c  INSTRUME        o : String listing instrument/detector
c  DETNAM          o : String listing specific detector name
c  FILTER          o : String listing instrument filter in use
c  AREASCAL        o : Area scaling factor
c  CHANTYPE        o : Type of detector channel in use
c  FLCHAN          o : Lowest legal channel for this detector
c  NUMCHN          o : Number of channels
c  NENERG          o : No. energy bins
c  NUMGRP          o : No. channel groups
c  NUMELT          o : No. response elements
c  ENERG_LO        o : Array containing lower bound to each energy bin
c  ENERG_HI        o : Array containing upper bound to each energy bin
c  NGRP            o : Array containing no. channel subsets at each energy
c  F_CHAN          o : Array containing 1st chan of each subset at each energy
c  N_CHAN          o : Array containing no. chans within each subset
c                           at each energy
c  ISORDER         o : True if ORDER exists
c  ORDER           o : Grating order for channel subsets
c  FMATRIX         o : Array containing the full matrix
c  LO_THRESH       o : The lower threshold used to construct the matrix
c  IERR            o : Error flag (0 = OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine FTGCNO            :(FITSIO) returns table column number
c  subroutine FTGCV*            :(FITSIO) reads vector column
c  subroutine FTGKY*            :(FITSIO) reads individual kywds from header
c  subroutine WTERRM            :(CALLIB) writes error message to STDOUT
c  subroutine WTFERR            :(CALLIB) writes error msg (+fitsio) to STDOUT
c  subroutine WTFWRN            :(CALLIB) writes warning msg (+fitsio) to STDOUT
c  subroutine WTINFO            :(CALLIB) writes info message to STDOUT
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c PARENT Routine (wtrmf1.f) history
c 	Rehana Yusaf (1.0.0:93 Jul 26) WT_RMF1992A.F (CALLIB) used as basis
c 	Rehana Yusaf (1.0.1:93 Oct 27) added arguments for rmfversn and
c					hduclas3, also the name has been
c				     	changed from rd_rmf1992a. In addition
c					if extname is not found
c					HDUCLAS1='RESPONSE' is searched for.
c					and HDUCLAS2='RSP_MATRIX'
c 	Rehana Yusaf (1.0.2:93 Nov 10) HDUVERS2 is read to obtain rmfversn if
c					HDUVERSN not present then RMFVERSN is
c					read. Prev' only RMFVERSN read
c 	Ian M George (1.1.0: 93 Nov 17) Took out searching for correct xtens
c					(this is now responsibilty of main)
c 	Rehana Yusaf (1.1.1:94 Jan 11) Remove mvalues array and read matrix
c					matrix values straight into fmatrix
c 	Rehana Yusaf (1.1.1: 94 Jun 24) Make the routine less verbose, that is
c					only print warnings at chatter>30
c PARENT Routine (wtrmf2.f) history
c 	Ian M George (1.0.0:95 Nov 22) Copied from wtrmf1.f (1.1.1), but
c					chantype added as passed parameter
c 	Ian M George (1.0.1:95 Nov 29) Added wtinfo & friends
c THIS ROUTINE (wtrmf3.f) history
c Ian M George (1.0.0:96 Oct 04) Copied from wtrmf2.f (1.0.1), suuport for
c                             	HDUVERS2 = '1.2.0' added (and flchan added
c                               as passed parameter
c
c Banashree M Seifert (1.1.0 Oct 10, 1996)
c       . wrtstr was char*80 and made *8
c
c Banashree M Seifert (1.2.0 Nov13, 1996)
c       . flchan needed to be initialised
c
c kaa (1.3.0 Dec 30, 1998)
c       . modified for version 1.3.0 of the response file format and
c         converted to RDRMF4.
c
c AM  (1.3.0 Apr 9, 1999)
c       . updated to add a warning when Maxelt < Numelt 
c PDW (1.3.1 Aug 6, 1999)
c       . Properly pass error status to WTFERR when nonCFITSIO error occurs
c NG (1.3.2 Aug 30, 1999)
c       . set the enull and inull to -9.11e11 and -32767 (was 0 before) 
C         to avoid to be coincidence with value 0. 
c
c --------------------------------------------------------------------
      character(7) VERSION
      PARAMETER (VERSION='1.3.2')
*-
c Internals
      character(6) SUBNAME
      PARAMETER (SUBNAME='rdrmf4')

      REAL enull
      INTEGER colnum(7)
      INTEGER i , iread , igrp , ioff , status
      INTEGER ie , frow , felem , inull
      character(80) message
      character(8) wrtstr, colnam(7)
      character(30) comm
      LOGICAL anyflg , qflchan_prob

      DATA colnam /'ENERG_LO', 'ENERG_HI', 'N_GRP', 'F_CHAN', 'N_CHAN',
     &             'MATRIX', 'ORDER' /

c Initialise
      Ierr = 0
      status = 0
 
c User info, if requested
      message = 'using '//SUBNAME//' '//VERSION
      CALL WTINFO(Chatter,15,1,message)
 
c Get necessary keywords
c NAXIS2 ...
      status = 0
      CALL FTGKYJ(Iunit,'NAXIS2',Nenerg,comm,status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,'reading NAXIS2 keyword')
         Ierr = 4
         GOTO 200
      ENDIF
      IF ( Nenerg.GT.Maxen ) THEN
         Ierr = 4
         CALL WTFERR(SUBNAME,VERSION,1,
     &               'Energy Array dimension is too small')
         GOTO 200
      ENDIF
 
c HDUCLAS3 ...
      Hduclas3 = '  '
      status = 0
      CALL FTGKYS(Iunit,'HDUCLAS3',Hduclas3,comm,status)
      CALL WTFWRN(SUBNAME,VERSION,Chatter,30,status,
     &            'Problem reading HDUCLAS3 keyword')
 
c HDUVERS/HDUVERS2 ...

      Rmfversn = ' '
      status = 0
      CALL FTGKYS(Iunit,'HDUVERS',Rmfversn,comm,status)
      IF ( status .NE. 0 ) THEN
         status = 0
         CALL FTGKYS(Iunit,'HDUVERS2',Rmfversn,comm,status)
         IF ( status .NE. 0 ) THEN
            status = 0
            CALL FTGKYS(Iunit,'HDUVERS1',Rmfversn,comm,status)
         ENDIF
      ENDIF
      CALL WTFWRN(SUBNAME,VERSION,Chatter,30,status,
     &  'Problem reading HDUVERS, HDUVERS1 or HDUVERS2 keyword')

c If HDUVERS < 1.2.0, then we have a potential problem regarding what the
c first legal channel is for this detector. This will require a check later
c so set a flag.
      IF ( (Rmfversn.EQ.'1.0.0') .OR. (Rmfversn.EQ.'1.1.0') .OR.
     &     (Rmfversn.EQ.' ') ) THEN
         qflchan_prob = .TRUE.
      ELSE
         qflchan_prob = .FALSE.
      ENDIF
 
c TELESCOP ...
      status = 0
      CALL FTGKYS(Iunit,'TELESCOP',Telescop,comm,status)
      CALL WTFWRN(SUBNAME,VERSION,Chatter,20,status,
     &            'Problem reading TELESCOP keyword')
      IF ( status.EQ.202 ) Telescop = 'UNKNOWN'
 
c INSTRUME ...
      status = 0
      CALL FTGKYS(Iunit,'INSTRUME',Instrume,comm,status)
      CALL WTFWRN(SUBNAME,VERSION,Chatter,20,status,
     &            'Problem reading INSTRUME keyword')
      IF ( status.EQ.202 ) Instrume = 'UNKNOWN'
 
c FILTER ...
      status = 0
      CALL FTGKYS(Iunit,'FILTER',Filter,comm,status)
      CALL WTFWRN(SUBNAME,VERSION,Chatter,30,status,
     &            'Problem reading FILTER keyword')
      IF ( status.EQ.202 ) Filter = 'NONE'
 
c DETNAM ...
      status = 0
      CALL FTGKYS(Iunit,'DETNAM',Detnam,comm,status)
      IF ( status.EQ.202 ) THEN
         status = 0
         CALL FTGKYS(Iunit,'DETNAME',Detnam,comm,status)
         IF ( status.EQ.202 ) Detnam = 'NONE'
      ENDIF
 
c EFFAREA ...
      status = 0
      CALL FTGKYE(Iunit,'EFFAREA',Areascal,comm,status)
      IF ( status.NE.0 ) Areascal = 1
 
c LO_THRESH ...
      status = 0
      LO_THRESH = 0
      CALL FTGKYE(Iunit,'LO_THRES',Lo_thresh,comm,status)
      CALL WTFWRN(SUBNAME,VERSION,Chatter,30,status,
     &            'Problem reading LO_THRES keyword')
      IF ( status.NE.0 ) Lo_thresh = 0
 
c DETCHANS ...
      status = 0
      CALL FTGKYJ(Iunit,'DETCHANS',Numchn,comm,status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem reading DETCHANS keyword')
         Ierr = 1
         GOTO 200
      ENDIF

c CHANTYPE
      status = 0
      CALL FTGKYS(Iunit,'CHANTYPE',Chantype,comm,status)
      IF ( status.NE.0 ) THEN
         CALL WTFWRN(SUBNAME,VERSION,Chatter,20,status,
     &               'Problem reading CHANTYPE keyword from RMF')
         CALL WTINFO(Chatter,1,1,'setting CHANTYPE = UNKNOWN')
         Chantype = 'UNKNOWN'
         status = 0
      ENDIF
 
 
c Reassure user, if requested
      CALL WTINFO(Chatter,20,1,'read all the keywords')
      CALL WTINFO(Chatter,20,1,'reading the data')

c Find the column numbers. Note the special handling of the ORDER column,
c which is optional

      status = 0
      DO i = 1, 6
         CALL FTGCNO(Iunit,.FALSE.,colnam(i),colnum(i),status)
         IF ( status.NE.0 ) THEN
            message = 'Cannot find column for '//colnam(i)
            CALL WTFERR(SUBNAME,VERSION,status,message)
            Ierr = 4
            GOTO 200
         ENDIF
      ENDDO

      CALL FTGCNO(Iunit,.FALSE.,colnam(7),colnum(7),status)
      IF ( status .EQ. 0 ) THEN
         Isorder = .TRUE.
      ELSE
         Isorder = .FALSE.
         status = 0
      ENDIF

c Get the energies (ENERG_LO and ENERG_HI)

      frow = 1
      felem = 1
      status = 0
C      enull = 0.
      enull = -9.11E11

      CALL FTGCVE(Iunit,colnum(1),frow,felem,Nenerg,enull,Energ_lo,
     &            anyflg, status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem reading ENERG_LO column')
         Ierr = 1
         GOTO 200
      ENDIF

      CALL FTGCVE(Iunit,colnum(2),frow,felem,Nenerg,enull,Energ_hi,
     &            anyflg, status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem reading ENERG_HI column')
         Ierr = 1
         GOTO 200
      ENDIF
 
c Get the number of groups for each channel (N_GRP)

      inull = -32767
      CALL FTGCVJ(Iunit,colnum(3),frow,felem,Nenerg,inull,Ngrp,anyflg,
     &            status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem reading N_GRP column')
         Ierr = 1
         GOTO 200
      ENDIF

c Find the total number of groups

      Numgrp = 0
      DO i = 1, Nenerg
         Numgrp = Numgrp + Ngrp(i)
      ENDDO
      IF ( Numgrp .GT. Maxgrp ) THEN
         Ierr = 4
         CALL WTFERR(SUBNAME,VERSION,1,
     &               'Group Array dimension is too small')
         GOTO 200
      ENDIF

c Get the group information (F_CHAN and N_CHAN).

      ioff = 1
      DO ie = 1, Nenerg

         CALL FTGCVJ(Iunit,colnum(4),ie,felem,Ngrp(ie),inull,
     &               F_chan(ioff),anyflg,status)
         IF ( status.NE.0 ) THEN
            CALL WTFERR(SUBNAME,VERSION,status,
     &                  'Problem reading F_CHAN column')
            Ierr = 1
            GOTO 200
         ENDIF

         CALL FTGCVJ(Iunit,colnum(5),ie,felem,Ngrp(ie),inull,
     &               N_chan(ioff),anyflg,status)
         IF ( status.NE.0 ) THEN
            CALL WTFERR(SUBNAME,VERSION,status,
     &                  'Problem reading F_CHAN column')
            Ierr = 1
            GOTO 200
         ENDIF

         IF ( Qorder .AND. Isorder ) THEN

            CALL FTGCVJ(Iunit,colnum(7),ie,felem,Ngrp(ie),inull,
     &                  Order(ioff),anyflg,status)
            IF ( status.NE.0 ) THEN
               CALL WTFERR(SUBNAME,VERSION,status,
     &                     'Problem reading F_CHAN column')
               Ierr = 1
               GOTO 200
            ENDIF

         ENDIF

         ioff = ioff + Ngrp(ie)

      ENDDO

c Calculate the number of response elements, Numelt,  stored in the file

      Numelt = 0
      DO i = 1 , Numgrp
         Numelt = Numelt + N_chan(i)
      ENDDO

      IF ( Maxelt .LT. Numelt ) THEN
         CALL WTFWRN(SUBNAME,VERSION,Chatter,40,status,
     &   'Number of matrix elements >  maximum array size')
      ENDIF

c OK, if we're reading a dataset created with HDUVERS2 < 1.2.0, then we have
c a potential problem regarding what the first legal channel is for this
c detector. Try and check to see if F_CHAN is ever zero, and if so assume
c that the first legal channel is also zero. THIS WILL OFTEN CATCH THE PROBLEM,
c BUT NOT ALWAYS - although there is little else we can do.
c If we're reading a dataset created with HDUVERS2 >= 1.2.0, then the TLMIN
c keyword should be present, so go read it.

      IF ( qflchan_prob ) THEN
         Flchan = 1
         DO i = 1 , Numgrp
            IF ( F_chan(i).EQ.0 ) Flchan = 0
         ENDDO
      ELSE
         WRITE (wrtstr,'(a,i1)') 'TLMIN' , colnum(4)
         CALL CRMVBLK(wrtstr)
         status = 0
         CALL FTGKYJ(Iunit,wrtstr,Flchan,comm,status)
         IF ( status.NE.0 ) THEN
            message = 'Problem reading '//wrtstr//' keyword'
            CALL WTFWRN(SUBNAME,VERSION,Chatter,1,status,message)
            message = 'assuming first legal channel in the detector is '
     &                //'numbered channel 1'
            CALL WTINFO(Chatter,1,1,message)
            Flchan = 1
            status = 0
         ENDIF
      ENDIF
 

c Now read in the actual matrix.

      igrp = 0
      ioff = 1
      DO ie = 1 , Nenerg

         iread = 0
         DO i = 1, Ngrp(ie)
            igrp = igrp + 1
            iread = iread + N_chan(igrp)
         ENDDO

         CALL FTGCVE(Iunit,colnum(6),ie,1,iread,enull,Fmatrix(ioff),
     &               anyflg,status)
         IF ( status.NE.0 ) THEN
            CALL WTFERR(SUBNAME,VERSION,status,
     &                  'Problem reading MATRIX column')
            Ierr = 1
            GOTO 200
         ENDIF

         ioff = ioff + iread

      ENDDO

 200  IF ( Ierr.NE.0 ) THEN
         CALL WTERRM(SUBNAME,VERSION,' Fatal error - aborting RMF read')
      ELSE
         CALL WTINFO(Chatter,20,1,'successfully read RSP_MATRIX data')
      ENDIF
 
 
      RETURN
      END
c ----------------------------------------------------------------------
c     END OF RDRMF4
c ----------------------------------------------------------------------
 
 










