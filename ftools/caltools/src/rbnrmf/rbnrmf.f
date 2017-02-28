*+RBNRMF
c     -----------------
      subroutine rbnrmf
c     -----------------
c --- DESCRIPTION ------------------------------------------------------
c This task rebins an OGIP standard RMF file, that is both the EBOUNDS
c and SPECRESP extensions.
c ----------------------------------------------------------------------
c --- VARIABLES --------------------------------------------------------
c  
      IMPLICIT NONE
      character(255) infile,outfile,ebdfile,binfile,ebinfile
      integer errflg, chatter, nchan, ebfact
      character(14) cmpmode
      character(1) fchan_out
      logical killit,file,efile
c
c --- LOCALS ---
c

      character(70) termdesc
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c infile     char   : input RMF filename
c ebdfile    char   : input EBD filename - usually same as RMF filename
c outfile    char   : output filename
c channel    int    : Channel array
c nchan      int    : Number of final channels, after compression
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------------
c
c Rehana Yusaf (1993 July 14)
c Rehana Yusaf (1993 Oct 19) 1.0.1; Replace CMP_FACT with RMF_CFACT
c                                   this is a temporary measure
c Rehana Yusaf (1993 Oct 26) 1.0.2; Use CMP_FACT and allow non-linear
c   				    compression. CMP_MODE paramter is
c				    introduced to describe compression.
c Rehana Yusaf (1993 Nov 22) 1.0.3; RBNR_COMP has been updated as RDEBD1
c				    and RDRMF1 have been updated. The
c				    extension search has been removed from
c                                   the readers and FNDEXT and FNDHDU are
c				    used instead.
c                                   RBNR_GP updated to read additional
c                                   parameter ebdfile.
c Rehana Yusaf (1993 Dec 3) 1.0.4;  Additional parameter fchan value passed
c                                   to cmp_fact (called by rbnr_comp)
c Rehana Yusaf (1994 Jan 11) 1.0.5; reduce memory requirements by reducing
c				    maxgrp to 3, and maxen to 1234 (from 2048)
c                                   also maxchan reduced to 4096 (from 5012)
c  Rehana Yusaf (1994 Feb 27) 1.0.6; Bug-fix, in rbnr_comp, nsearch changed
c                                   from 999 to 50
c Ian M George  (1994 Feb 09) 2.0.0; added Dynamic Memory Allocation
c Rehana Yusaf (1994 April 1) 2.0.1; DMA bugfix ,check status flag after 
c                                    every udmget call.
c Rehana Yusaf (1994 sept 12) 2.0.2; spelling correction, fatel -> fatal
c                                    ck_file has an additional argument
c                                    killit (clobber) read from parfile
c Rehana Yusaf (1994 Sept 22) 2.0.3; forgot to do the above !
c Rehana Yusaf (1995 June 1) 2.0.4; improve DMA allocation in rbnr_comp
c Rehana Yusaf (1995 July 31) 3.0.0; add functionality - binning info
c                     			can be read from an ascii file
c Rehana Yusaf (1995 Aug 10) 3.0.1; Bug-fix in ck_binchan - it assumed
c                                   incorrectly that the starting channel
c                                   is => 1.
c Rehana Yusaf (1995 Aug 14) 3.0.2; bug-fix in rbnr_rmf fmatrix(k,ie)
c                                   changed to fmatrix(shft+k,ie) to
c                                   account for fchan = 0, where shft=1-fchan
c Rehana Yusaf (1995 Aug 27) 3.0.3; minor update, addd INQUIRE for binfile
c Rehana Yusaf (1995 Oct 13) 3.0.4; increase maxcomp from 32 to 256
c                                   a comp fact of -1 omits the channel
c Ian M George (4.0.0:96 Oct 04) updated the depths to use rdrmf3,rdebd3,wtrmf3
c				& wtebd3 subroutines, which try their best to 
c				not to get the indexing of the matrix incorrect
c				for detectors whose first channel is numbered 
c				channel zero
c 
c Banashree M Seifert (4.1.0: Oct16,1996)
c          . modification in internal write format
c            instead of i its i8 now
c
c Banashree M Seifert (4.2.0: Nov12,1996)
c          . added provision to define the first channel in output
c            to be defined by user to be one of the following three
c             % --> same as input file (by default)
c             1 --> fchan=1
c             0 --> fchan=0
c            so added one subroutine ADJUST_CHANNEL1 and ADJUST_CHANNEL2
c            For this a new parameter is added in parameter file
c            as fchan so that on command line it can be
c            RBNRMF FCHAN=% or RBNRMF FCHAN=1 or RBNRMF FCHAN=0
c            this fchan is read as fchan_out in gp subroutine
c            So, a new variable fchan_out is introduced here
c            where fchan_out = character(1) 
c
c          . introduced new screen display routines wterrm, etc.
c
c Banashree M Seifert (4.3.0: Nov21 1996)
c           . replaced by call to grprmf in place of call to grp_rmf
c              change in grprmf contains passing parameter first channel
c              no. (0 or 1)
c
c Banashree M Seifert (4.4.0: Mar 12, 1997)
c           . in subroutine maxgrp was changed from 3 to 10
c
c Peter D Wilson (4.4.1: Jul 01, 1998)
c           . Updated for new FCPARS/FTOPEN behavior
c
c MJT (4.4.2: 10July1998)
c           . Moved status initialization to be done BEFORE it's needed...
c NG (4.4.3: 17Jun1999) 
C           . Change the algorithm of the rebin. Preserved the group 
C             structure
c NG (4.4.4: 29Jun1999)
C           . Bug fix for calling of adjust_channel2.
C           . some cleanups and bug fixes on rbnr_rmf
c NG (4.4.5: 05AUG1999)
C           . Fixed the bug for xte mission.
c NG (4.4.6: 09AUG1999)
C           . Updated the numgrp keyword for the rebinned rmf file. 
c NG (4.4.7: 20OCT1999)
C           . Corrected a bug in rbnr_rmf. 
c NG (4.4.8: 20MAR2000)
C           . Extended the length of the filename to 255. 
c PDW (4.4.9: 14APR2000)
c           . Fix syntax error in rbnr_comp
c           . Initialize array in rbnr_rmf
c NG  (4.5.0:) 22May2000)  
c             Added check for the mandatory keywords               
c NG  (4.5.1:) 02Jun2000)  
c             Fixed bug if the compression factor is -1.            
c NG  (4.5.2:) 04Sep2002)  
c             Fixed seg fault if extra extensions are copied.
c kaa (5.0.0:  20Oct2006)
c             Added option to bin in energy dimension as well as channel
c kaa (5.0.1:  10jan2007)
c             Fixed a couple of bugs and improved diagnostic messages. Bugs 
c             fixed are
c                  1. If there were additional extensions to be copied a seg 
c                     fault could result.
c                  2. If the EBD and MATRIX extensions used different 
c                     definitions of the first channel (ie 0 in EBD and 1 in 
c                     MATRIX) then the definition from the EBD was incorrectly 
c                     used when rebinning the matrix.
c kaa (5.0.2: 22feb2008)
c             Ensured that NUMGRP is updated correctly for the output response.
c kaa (5.0.3: 26oct2010)
c             Added check for THRESH=0 in RBN_ERMF to avoid seg. fault.
c ------------------------------------------------------------------------

      character(5) version
      parameter (version = '5.0.3')
      character(7) taskname
      parameter (taskname ='rbnrmf')
c ----------------------------------------------------------------------
c
c --- GET PARAMETERS ---
c
      errflg = 0

      call rbnr_gp(infile,ebdfile,outfile,nchan,cmpmode,file,
     & binfile,ebfact,efile,ebinfile,fchan_out,killit,chatter,errflg)
       IF (errflg.NE.0) THEN
        termdesc='returning from rbnr_gp'

        call wterrm(taskname,version,termdesc)
        goto 100
       ENDIF

      call wtbegm(taskname,version,chatter)

c --- COMPRESS RMF DATA ---

       call rbnr_comp(infile,ebdfile,outfile,nchan,cmpmode,
     &                file,binfile,efile,ebinfile,ebfact,version,
     &                fchan_out,killit,errflg,chatter)
       IF (errflg.NE.0) THEN
        termdesc='returning from rbnr_comp'
        call wterrm(taskname,version,termdesc)
        goto 100
       ENDIF


 100  call wtendm(taskname,version,errflg,chatter)

      end
c ----------------------------------------------------------------
c     END OF RBNRMF
c ----------------------------------------------------------------


*+RBNR_GP
c     ---------------------------------------------------------
      subroutine rbnr_gp(infile,ebdfile,outfile,nchan,cmpmode,
     &                   qfile,binfile,ebfact,qefile,ebinfile,fchan_out,
     &                   killit,chatter,status)
c     ---------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile, cmpmode,ebdfile,binfile
      character*(*) fchan_out, ebinfile
      character(255) tbinfile
      integer chatter,nchan,status,ebfact
      character(70) desc,defval
      logical ext,valfil,val_cmp,qfile,qefile
      integer flen, fcstln,n_ill
      character(70) ill_files(5)
      character(255) ebdfilename, rmffilename
      logical killit
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c Arguments ...
c
c infile     char   : input file name
c ebdfile    char   : ebdfile input file, usually same as infile
c outfile    char   : Output filename
c nchan      int    : final number of channels, entered directly/from pha
c cmpmode    char   : compression mode for channels
c qfile      logical: true if a binfile is input
c binfile    char   : filename with channel binning information
c ebfact     int    : binning factor in the energy dimension
c qefile     logical: true if an ebinfile is input
c ebinfile   char   : filename with energy binning information
c fchan_out  char   : first channel number for output response
c killit     logical: if true then clobber existing output file
c chatter    int    : Chattiness flag, >20 verbose
c status     int    : Error flag
c
c --- CALLED ROUTINES -------------------------------------------------
c
c UCLGST     : (HOST) Get string input
c UCLGSI     : (HOST) Get integer input
c FCECHO     : (FTOOLS) Screen write
c
c --- COMPILATION/LINKING ---------------------------------------------
c
c CALTOOLS, FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1993 July 15)
c Rehana Yusaf (1993 Nov 22) 1.0.1; Additional parameter - ebdfile read
c Rehana Yusaf (1994 Sept 13) 1.0.2; read clobber 
c Rehana Yusaf (1995 July 31) 1.0.3; read binfile
c Rehana Yusaf (1995 Aug 27) 1.0.4; add INQUIRE for RBNRMF
c
c Banashree M Seifert(1996 Nov 12) 2.0.0:
c            . added one parameter fchan_out
c              this parameter gives the user option to define the first
c              channel in the output file to be 0 or 1
c              This is a hidden parameter and fchan_out=% by default in
c              which case it will take the first channel no. to be
c              same as input file
c
c            . introduced screen display routines wterrm, etc.
c Peter D Wilson (1998 Jul 01) 2.0.1:
c            . Dropped INQUIRE tests.  Use ftrtnm to strip off extension
c kaa (2006 Oct 20) 3.0.0
c              Added parameters to bin on energy axis
c -------------------------------------------------------------------
      character(8) subname
      parameter (subname='rbnr_gp')
      character(5) version 
      parameter (version = '3.0.0')
*-
c ---------------------------------------------------------------------
c

      IF ( status .NE. 0 ) RETURN

      nchan = 0
      valfil = .true.

c GET INFILE

      call uclgst('infile',infile,status)
      desc = 'getting infile parameter !'
      IF (status.NE.0) GOTO 999
      call crmvlbk(infile)
      IF (infile.EQ.'  ') THEN
         desc = 'input file has to be entered !'
         status = 11
         GOTO 999
      ENDIF
C PDW 7/1/98: Drop INQUIRE. Use ftrtnm to strip off extension
      call ftrtnm( infile, rmffilename, status )
      n_ill = 1
      ill_files(n_ill) = rmffilename

c GET EBDFILE 

      call uclgst('ebdfile',ebdfile,status)
      desc = 'getting ebdfile parameter !'
      IF (status.NE.0) GOTO 999

      call crmvlbk(ebdfile)
      IF (ebdfile.EQ.'%') THEN
        ebdfile = rmffilename
      ELSE
C PDW 7/1/98: Drop INQUIRE. Use ftrtnm to strip off extension
        call ftrtnm( ebdfile, ebdfilename, status )
        IF (ebdfilename.NE.rmffilename) THEN
          n_ill = 2
          ill_files(2) = ebdfilename
        ENDIF 
      ENDIF

c PUT DEFAULT EBDFILE 

      defval = '%'
      call uclpst('ebdfile',defval,status)
      desc = 'putting default ebdfile parameter !'
      IF (status.NE.0) GOTO 999

c GET BINFILE value

      qfile = .false.
      call uclgst('binfile',binfile,status)
      desc = 'reading binfile parameter !'
      IF (status.NE.0) GOTO 999
      call crmvlbk(binfile)
      tbinfile = binfile
      call ftupch(tbinfile)
      IF ((tbinfile.NE.'NONE').AND.(tbinfile.NE.'  ')) THEN
        flen = fcstln(binfile)
        INQUIRE(FILE=binfile(:flen),EXIST=ext)
        IF (.NOT.ext) THEN
          desc = 'File does not EXIST :'//binfile
          status = 11
          GOTO 999
        ENDIF
        qfile = .true.
        n_ill = n_ill + 1
        ill_files(n_ill) = binfile
      ENDIF

c PUT DEFAULT BINFILE

      defval = 'NONE'
      call uclpst('binfile',defval,status)
      desc = 'putting default binfile parameter !'
      IF (status.NE.0) GOTO 999

c get the first channel no user wants to be  --> default= '%'

      call uclgst('fchan',fchan_out,status)
      desc = 'getting first channel parameter '
      IF (status.NE.0) GOTO 999
           
c GET NCHAN value 

      IF (.NOT.qfile) THEN 
        call uclgsi('nchan',nchan,status)
        desc = 'getting nchan parameter !'
        IF (status.NE.0) GOTO 999

c GET CMPMODE

        call uclgst('cmpmode',cmpmode,status)
        desc = 'getting cmpmode parameter !'
        IF (status.NE.0) GOTO 999

c CHECK CMPMODE VALIDITY

        call ftupch(cmpmode)
        call crmvlbk(cmpmode)
        val_cmp = .false.
        IF (cmpmode(1:1).EQ.'F') THEN
          val_cmp = .true.
        ELSEIF (cmpmode(1:1).EQ.'L') THEN
          val_cmp = .true.
        ELSEIF (cmpmode(1:1).EQ.'B') THEN
          val_cmp = .true.
        ENDIF
        IF (.NOT.val_cmp) THEN
          desc = 'Invalid CMPMODE !'
          call wterrm(subname,version,desc)
          desc=' VALID CMPMODES: LINEAR,FAINT2BRIGHT and BRIGHT2LINEAR'
          status = 11
          GOTO 999
        ENDIF
      ENDIF

c GET OUTFILE 

      call uclgst('outfile',outfile,status)
      desc = 'getting outfile parameter !'
      IF (status.NE.0) GOTO 999
      call crmvlbk(outfile)
      desc='outfile must be entered !'
      IF (outfile.EQ.' ') THEN
        status = 11
        GOTO 999
      ENDIF

c GET EBINFILE value

      qefile = .false.
      call uclgst('ebinfile',ebinfile,status)
      desc = 'reading ebinfile parameter !'
      IF (status.NE.0) GOTO 999
      call crmvlbk(ebinfile)
      tbinfile = ebinfile
      call ftupch(tbinfile)
      IF ((tbinfile.NE.'NONE').AND.(tbinfile.NE.'  ')) THEN
        flen = fcstln(ebinfile)
        INQUIRE(FILE=ebinfile(:flen),EXIST=ext)
        IF (.NOT.ext) THEN
          desc = 'File does not EXIST :'//ebinfile
          status = 11
          GOTO 999
        ENDIF
        qefile = .true.
        n_ill = n_ill + 1
        ill_files(n_ill) = ebinfile
      ENDIF

c PUT DEFAULT EBINFILE

      defval = 'NONE'
      call uclpst('ebinfile',defval,status)
      desc = 'putting default ebinfile parameter !'
      IF (status.NE.0) GOTO 999

c GET EBFACT value 

      IF (.NOT.qefile) THEN 
        call uclgsi('ebfact',ebfact,status)
        desc = 'getting ebfact parameter !'
        IF (status.NE.0) GOTO 999
      ENDIF

c GET CLOBBER

      call uclgsb('clobber',killit,status)
      desc = 'getting clobber parameter !'
      IF (status.NE.0) GOTO 999

c CHECK WHETHER THE OUTPUT FILE SPECIFIED IS OK

      call ck_file(outfile,ill_files,n_ill,valfil,
     &             killit,chatter)
      desc = 'invalid outfile !'
      IF (.NOT.valfil) THEN
        status = 11
        GOTO 999
      ENDIF

c GET CHATTER 

      call uclgsi('chatter',chatter,status)
      desc = 'getting chatter parameter !'
      IF (status.NE.0) GOTO 999

 999  CONTINUE
      IF ( status .NE. 0 ) call wterrm(subname,version,desc)
        
      return
      end
c ---------------------------------------------------------------------
c     END OF RBNR_GP
c ---------------------------------------------------------------------


*+RBNR_COMP
c     -----------------------------------------------------------------
      subroutine rbnr_comp(infile,ebdfile,outfile,nchan,cmpmode,
     & file,binfile,efile,ebinfile,ebfact,mnver,fchan_out,killit,
     & errflg,chatter)
c     -----------------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c This routine rebins the channels, in the EBOUNDS, and SPECRESP
c extensions of a user defined RMF file and optionally the energies then
c writes the rebinned data to an output file.
c 
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile, mnver, ebinfile
      character*(*) binfile,cmpmode,ebdfile,fchan_out
      integer errflg, chatter, nchan, ebfact
      logical killit,file,efile
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c Arguments ...
c
c infile     char   : input filename
c ebdfile    char   : input filename for EBOUNDS extension
c outfile    char   : output filename
c nchan      int    : Final number of channels
c cmpmode    char   : Compression mode for channels
c file       logical: True if a channel binning file is required
c binfile    char   : File with channel binning instructions
c efile      logical: True if an energy binning file is required
c ebinfile   char   : File with energy binning instructions
c ebfact     int    : Binning factor for energy dimension
c mnver      char   : Main task version number
c fchan_out  char   : First channel to use for output response
c killit     logical: If true overwrite any existing output file
c errflg     int    : error flag, 0 is ok
c chatter    int    : Chattines flag
c
c --- COMPILATION/LINKING ------------------------------------------
c
c FTOOLS,CALLIB,FITSIO
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1993 July 15)
c Rehana Yusaf (1993 Nov 22) 1.0.1; FNDHDU and FNDEXT used instead
c				    of extension search in the
c                                   readers, RDEBD1 and RDRMF1. 
c Rehana Yusaf (1993 Dec 3) 1.0.2;  Pass ebdchan(1) to cmp_fact
c Rehana Yusaf (1994 Jan 11) 1.0.3; Reduce maxgrp to 3 (from 10)
c                                   and maxchan to 4096 (from 5012)
c                                   and maxen to 1234 (from 2048)
c Rehana Yusaf (1994 27 Feb) 1.0.4; Bug-fix nsearch changed from
c                                   999 to 50
c Ian M George  (1994 Feb 09) 2.0.0; added Dynamic Memory Allocation
c Rehana Yusaf  (1995 june 1) 2.0.1; improve DMA by only reading NAXIS2
c Rehana Yusaf (1995 July 31) 2.0.2; add reading binning from ascii file
c                                    option
c Ian M George (3.0.0:96 Oct 04) updated to use rdrmf3,rdebd3,wtrmf3 & wtebd3 
c				subroutines, which try their best to not to 
c				get the indexing of the matrix incorrect
c				for detectors whose first channel is numbered 
c				channel zero
c
c Banashree M Seifert(3.1.0: Nov12, 1996)
c           . introduced screen display routines wterrm, etc.
c           . introduced first chan option (fchan_out)
c
c Banashree M Seifert(3.2.0: Mar 12, 1997)
c           .  parameter maxgrp set to 10 instead of 3
c
c Peter D Wilson (3.2.1: Jul 01, 1998)
c           . Updated for new FCPARS/FTOPEN behavior
c
c Alex M. (3.2.2: Feb 11, 1999)
c           . Added 'call RMFSZ'. Replaced RDRMF3/WTRMF3 by RDRMF4/WTRMF4
c             Removed call to GRPRMF. Changed the parameter list in 
c             RBNR_RMF. 
c
c Peter D Wilson (3.2.3): Apr 14, 2000)
c           . Fix double-comma in wterrm call
c
c kaa (4.0.0: Oct 20, 2006)
c           . Added support for binning in energy dimension
c ------------------------------------------------------------------- 

      character(10) subname
      parameter (subname='rbnr_comp')
      character(5) version
      parameter (version = '4.0.0')

c ------------------------------------------------------------------
C **** DYNAMIC MEMORY ALLOCATION ****
C  the following MEM common block definition is in the system iraf77.inc 
C  file
      INTEGER*4        MEMI(100)
      REAL             MEMR(100)
      EQUIVALENCE (MEMI, MEMR)
      COMMON /MEM/ MEMR
c ************************************
c MAX ARRAY SIZES
      integer maxen, maxchan, maxgrp, maxelt

c INTERNALS
c ... parameters & static arrays
      integer maxcomp
      parameter (maxcomp = 256)
      integer nfacts, cfacts(maxcomp)
      integer stbins(maxcomp), endbins(maxcomp)
      character(16) ebdtlscop, ebdinstrum, ebdfilt,ebddet
      character(16) rmftlscop, rmfinstrum, rmffilt,rmfdet        
      real ebdarea
      integer iunit, ounit, n_ebd, cerr, ebdcerr
      integer  nkeys, nmore, nk_comm, nk_hist
      integer imove,next(50),ninstr,nfound,nsearch
      character(20) instr(50),outhdu(9,50),extname(50),outver(9,50)
	character(20) rmfchantyp, ebdchantyp
      integer numgrp,rmfchan,nenerg,detchans,fchan,efchan
      integer numelt,nebin
      integer keyspres,j,i,rmfextno,ebdextno,iunit2
      real lo_thresh, rmfarea
      integer block,rw,htype,lchan, rmfflchan
      character(30) comment
      character(4) cstart, cend, compchar
      character(70) bincomm, exhist,errinfo,desc,message
      character(80) rec, hist(5), comm(5)
      logical man_key, qorder, isorder
      character(5) rsp_rmfversn,ebd_rmfversn
      character(20) hduclas3,taskname
      character(255) ebdfilename,rmffilename
      character(8) task,dmode,keys(50)
      character(80) rmf_extname,rmf_extcomm
      character(80) ebd_extname,ebd_extcomm
     
      integer channel1

c ... pointers to "arrays" to be dynamically allocated
        integer p_ngrp, p_fmatrix, p_F_chan, p_N_chan
        integer p_energ_lo, p_energ_hi, p_e_min, p_e_max
	integer p_ebdchan,p_order
       
        INTEGER p_ngrp_comp,p_F_ch,p_N_ch
        INTEGER p_fmatrix_comp,p_buffer1,p_buffer2
        logical ismankey

c ... "arrays" to be dynamically allocated
c       integer ngrp(maxen)             real fmatrix(maxelt)
c       integer F_chan(maxgrp)          integer N_chan(maxen)
c       real energ_lo(maxen)            real energ_hi(maxen)
c       real e_min(maxchan)             real e_max(maxchan)
c	integer ebdchan(maxchan)        

c
c --- USER INFO ---
c
      desc = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,1,desc)

      isorder = .false.
      qorder  = .false.


c
c --- FIND EBOUNDS EXTENSION ---
c
      errflg = 0
      call fcpars(ebdfile,ebdfilename,ebdextno,errflg)
C PDW 7/1/98: Use ftrtnm to strip off extension
      call ftrtnm( ebdfile, ebdfilename, errflg )
      call crmvlbk(ebdfilename)
c MJT 10July98 this would be more useful BEFORE fcpars...
c      errflg = 0
      block = 2880
      rw = 0
      call cgetlun(iunit)
C PDW 7/1/98: Use original filename
      call ftopen(iunit,ebdfile,rw,block,errflg)
      errinfo = 'opening EBOUNDS file '// ebdfile // '!'
      IF (errflg.NE.0) goto 482
c
c --- FIND EXTNUM if EXTNUM IS NOT SPECIFIED AT THE END OF EBDFILE ---
c
      IF (ebdextno.LE.0) THEN
        ninstr = 2 
        instr(1) ='RESPONSE'
        instr(2) = 'EBOUNDS' 
        nsearch = 50 
        call fndhdu(chatter,iunit,ninstr,instr,nsearch,nfound,next,
     &              outhdu,outver,extname,errflg)

c --- CHECK FOR (old-style) EXTNAME if HDUCLASS not found ---

        IF (nfound.LE.0) THEN
            errinfo = 'Ext with allowed HDUCLASn keywords not found'
            call wtwarm(subname,version,chatter,20,errinfo)
            errinfo = 'searching for EXTNAME= EBOUNDS'
            call wtwarm(subname,version,chatter,20,errinfo)
            call fndext(chatter,iunit,'EBOUNDS',
     &           nsearch,nfound,next,outhdu,outver,extname,errflg)
        ENDIF

        IF (nfound.LE.0) THEN
           errinfo = 'EBOUNDS extension not found '
           errflg = 1
           goto 482
        ENDIF         

        IF (nfound.GT.1) THEN
          errinfo = 'Input file contains >1 EBOUNDS datasets'
          call wtwarm(subname,version,chatter,0,errinfo)
          write(errinfo,'(a,i2,a)')'... ',nfound,' extensions found'
          call wtwarm(subname,version,chatter,0,errinfo)
          do i=1,nfound
            write(errinfo,'(a,i2,a)')'... Ext ',next(i),':'
            call wtwarm(subname,version,chatter,0,errinfo)
            write(errinfo,'(4X,a,a)')'EXTNAME = ',extname(i)
            call wtwarm(subname,version,chatter,0,errinfo)
            do j=1,4
              write(errinfo,'(4X,a,i2,2a)')'HDUCLAS',j,' = ',outhdu(j,i)
              call wtwarm(subname,version,chatter,0,errinfo)
            enddo
          enddo
          errinfo =
     & '... Extension number must be specified via ebdfile parameter'
          call wterrm(subname,version,errinfo)
          errinfo = ' ... for example INPUT.EBD[1]'
          call wtinfo(chatter,0,2,errinfo)
          errinfo = ' ... ebdfile is a hidden parameter'
          call wtinfo(chatter,0,2,errinfo)
          errflg = 1
          goto 482
        ENDIF
      ENDIF

c --- MOVE TO APPROPRIATE PLACE IN FILE ---

      IF (ebdextno.LE.0) THEN
        IF (next(1).GT.0) THEN
           imove = next(1)
           errflg = 0
           call ftmrhd(iunit,imove,htype,errflg)
           errinfo = 'Problem moving to EBOUNDS xtens'
           IF (errflg.NE.0) goto 482
        ENDIF
        ebdextno = next(1)
      ELSE
        errflg = 0
        call ftmahd(iunit,ebdextno+1,htype,errflg)
        errinfo = 'Problem moving to EBOUNDS extension'
        IF (errflg.NE.0) goto 482
      ENDIF

c -- Alex -- Get the channel array size maxchan 

      maxchan = 0

      errflg = 0
      call ftgkyj(iunit,'DETCHANS',maxchan,comment,errflg)
      errinfo = 'reading DETCHANS value'
      call wtfwrn(subname,version,chatter,0,errflg,errinfo)
c Alex      maxchan = maxchan + 2
      write(errinfo,'(a,i8)') ' Maxchan array :',maxchan
      call wtinfo(chatter,30,1,errinfo)

c -------------------------------------------------------------------

      IF (errflg.NE.0) goto 482

c Alex --- Initialize pointers

        p_e_min   = 0
        p_e_max   = 0
        p_ebdchan = 0

c ---------------------------- 


        call udmget(maxchan,6,p_e_min,errflg)
        errinfo = 'Failed to allocate Dynamic Memory for EBOUNDS data'
        IF (errflg.NE.0) goto 482
        call udmget(maxchan,6,p_e_max,errflg)
        IF (errflg.NE.0) goto 482
        call udmget(maxchan, 4, p_ebdchan, errflg)
        IF (errflg.NE.0) goto 482

c
c --- READ EBOUNDS EXTENSION ---
c
      errflg = 0
      detchans = 0

      ebd_extname =' '
      ebd_extcomm =' '
      call rdebd4(iunit,chatter,ebd_extname,ebd_extcomm,
     &  maxchan,ebdtlscop,ebdinstrum,
     &  ebddet,ebdfilt,ebdarea,ebdchantyp,fchan, 
     &  n_ebd,MEMI(p_ebdchan),MEMR(p_e_min),MEMR(p_e_max),
     &  ebd_rmfversn,errflg) 
      errinfo = 'reading EBOUNDS extension'
      IF (errflg.NE.0) goto 482

c --- VALID COMPRESSION ? ...GET COMPRESSION FACTOR ---

      call gt_fchan(MEMI(p_ebdchan),fchan,
     &              lchan,n_ebd,errflg,chatter)
      task = 'RBNRMF'
      dmode = '   '
      IF (detchans.EQ.0) THEN
        detchans = n_ebd
      ENDIF

      IF (file) THEN
         call cmp_binfile(binfile,nchan,detchans,fchan,
     &                    lchan,nfacts,stbins,endbins,cfacts,
     &                    maxcomp,errflg,chatter)
         errinfo = 'returning from cmp_binfile reading binning info'
         IF (errflg.NE.0) goto 482
      ELSE
         IF ( nchan .NE. n_ebd ) THEN
            call cmp_fact(nchan,ebdtlscop,ebdinstrum,n_ebd,
     &                    dmode,cmpmode,MEMI(p_ebdchan),detchans,
     &                    fchan,nfacts,stbins,endbins,cfacts,task,
     &                    errflg,chatter)
            errinfo='returning from cmp_fact compression factor'
            IF (errflg.NE.0) goto 482
         ELSE
            nfacts = 1
            stbins(1) = fchan
            endbins(1) = fchan + nchan - 1
            cfacts(1) = 1
         ENDIF
      ENDIF
c
c --- COMPRESS EBOUNDS DATA ---
c
      ebdcerr = 0

      call rbnr_ebd(n_ebd,MEMI(p_ebdchan),MEMR(p_e_min),MEMR(p_e_max),
     &		    nfacts,stbins,endbins,cfacts,fchan,
     &              ebdcerr,chatter)

      errinfo='returning from rbnr_ebd (rebinning EBD)'
      if(ebdcerr .ne. 0) goto 482

c before writing to output file, to check what is the first channel
c user wants and set rmfflchan according to that

      if(fchan_out .eq. '%') then
         efchan=fchan
      elseif(fchan_out .eq. '0') then
         efchan=0
      elseif(fchan_out .eq. '1') then
         efchan=1
      else
         errinfo='first channel should be either  % /0 /1'
         errflg=1
         goto 482
      endif

c now adjust the channel array so that it starts from rmfflchan
c and not from  mandatory rmfflchan=1

      call adjust_channel1(MEMI(p_ebdchan),efchan,n_ebd,errflg,chatter)
      errinfo='returning from adjust_channel1 for ebd'
      if(errflg .ne. 0) goto 482

c --- READ RMF DATA ---
c
c --- FIND RMF EXTENSION ---

      call fcpars(infile,rmffilename,rmfextno,errflg)
C PDW 7/1/98: Use ftrtnm to strip off extension
      call ftrtnm( infile, rmffilename, errflg)
      call crmvlbk(rmffilename)
      IF (rmffilename.EQ.ebdfilename) THEN
         iunit2=iunit
         call ftmahd(iunit2,1,htype,errflg)
      ELSE
        errflg = 0
        call ftclos(iunit,errflg)
        errflg = 0
        block = 2880
        rw = 0
        call cgetlun(iunit2)
        call ftopen(iunit2,infile,rw,block,errflg)
        errinfo = 'opening RMF file !'
        IF (errflg.NE.0) goto 482
      ENDIF


c --- FIND EXTNUM if EXTNUM IS NOT SPECIFIED AT THE END OF INFILE ---
c
      IF (rmfextno.LE.0) THEN
        ninstr = 2 
        instr(1) = 'RESPONSE'
        instr(2) = 'RSP_MATRIX'
        nsearch = 50 
        call fndhdu(chatter,iunit2,ninstr,instr,nsearch,nfound,next,
     &              outhdu,outver,extname,errflg)

c --- CHECK FOR (old-style) EXTNAME if HDUCLASS not found ---

        IF (nfound.LE.0) THEN
           errinfo = 'Ext with allowed HDUCLASn keywords not found'
           call wtwarm(subname,version,chatter,20,errinfo)
           errinfo = 'searching for EXTNAME= MATRIX'
           call wtinfo(chatter,20,2,errinfo)
           call fndext(chatter,iunit2,'MATRIX',
     &           nsearch,nfound,next,outhdu,outver,extname,errflg)
        ENDIF

        IF (nfound.LE.0) THEN
           errinfo = 'searching for EXTNAME=SPECRESP'
           call wtinfo(chatter,0,2,errinfo) 
           call fndext(chatter,iunit2,'SPECRESP MATRIX',
     &           nsearch,nfound,next,outhdu,outver,extname,errflg)
        ENDIF

        IF (nfound.LE.0) THEN
           errinfo = 'RSP_MATRIX extension not found '
           errflg = 1
           goto 482
        ENDIF

        IF (nfound.GT.1) THEN
          errinfo =  ' Input file contains >1 MATRIX datasets'
          call wterrm(subname,version,errinfo)
          write(errinfo,'(a,i2,a)')'... ',nfound,' extensions found'
          call wtinfo(chatter,0,1,errinfo)
          do i=1,nfound
            write(errinfo,'(a,i2,a)')'... Ext ',next(i),':'
            call wtinfo(chatter,0,1,errinfo)
            write(errinfo,'(4X,a,a)')'EXTNAME = ',extname(i)
            call wtinfo(chatter,0,1,errinfo)
            do j=1,4
              write(errinfo,'(4X,a,i2,2a)')'HDUCLAS',j,' = ',outhdu(j,i)
              call wtinfo(chatter,0,1,errinfo)
            enddo
          enddo
          errinfo =
     & '... Extension number must be specified via infile parameter'
          call wtinfo(chatter,0,1,errinfo)
          errinfo = 'for example INPUT.RMF[1]'
          errflg = 1
          goto 482
        ENDIF
      ENDIF

c --- MOVE TO APPROPRIATE PLACE IN FILE ---

      IF (rmfextno.LE.0) THEN
        IF (next(1).GT.0) THEN
           imove = next(1)
           call ftmrhd(iunit2,imove,htype,errflg)
           errinfo =  'Problem moving to MATRIX xtens'
           IF (errflg.NE.0) goto 482
        ENDIF
        rmfextno = next(1)
      ELSE
        call ftmahd(iunit2,rmfextno+1,htype,errflg)
        errinfo = 'Problem moving to MATRIX extension'
        IF (errflg.NE.0) goto 482
      ENDIF

c -- Alex -- Get the array sizes: maxen, maxgrp, maxelt, and maxchan

      maxen  = 0
      maxgrp = 0
      maxelt = 0

      CALL RMFSZ( iunit2,chatter,maxen,maxgrp,maxelt,errflg)
      errinfo = 'reading array sizes: maxen, maxgrp, maxelt '
      IF (errflg.ne.0) goto 482

      WRITE(errinfo, '(3(a,i8))') 'Input nenerg = ', maxen, 
     &                    ' ngroup = ', maxgrp, ' nelt = ', maxelt
      call wtinfo(chatter,10,1,errinfo)

      maxchan = 0
      errflg = 0
      call ftgkyj(iunit2,'DETCHANS',maxchan,comment,errflg)
      errinfo = 'reading DETCHANS value'
      call wtfwrn(subname,version,chatter,0,errflg,errinfo)
C Alex      maxchan = maxchan + 2
      write(errinfo,'(a,i8)') ' Maxchan array :',maxchan
      call wtinfo(chatter,30,1,errinfo)

c -----------------------------------------------------------------

c 
c Allocate dynamic memory for RMF data arrays
c
      errflg = 0
      errinfo = 'Failed to allocate dynamic Memory '
     &//'for RMF data'

      p_energ_lo = 0
      p_energ_hi = 0
      p_ngrp = 0
      p_fmatrix = 0
      p_F_chan = 0
      p_N_chan = 0
      p_order = 0
      p_fmatrix_comp = 0
      p_ngrp_comp = 0
      p_F_ch = 0
      p_N_ch = 0
      p_buffer1 = 0
      p_buffer2 = 0

      call udmget(maxen, 6, p_energ_lo, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxen, 6, p_energ_hi, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxen, 4, p_ngrp, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxelt, 6, p_fmatrix, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxgrp, 4, p_F_chan, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxgrp, 4, p_N_chan, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxgrp, 4, p_order, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxelt, 6, p_fmatrix_comp, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxen, 4, p_ngrp_comp, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxgrp, 4, p_F_ch, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxgrp, 4, p_N_ch, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxchan, 6, p_buffer1, errflg)
      IF (errflg.NE.0) goto 482

      call udmget(maxchan, 6, p_buffer2, errflg)
      IF (errflg.NE.0) goto 482

      rmf_extname =' '
      rmf_extcomm = ' '
      call rdrmf5(iunit2,chatter,rmf_extname,rmf_extcomm,
     &            qorder,maxen,maxgrp,maxelt,rsp_rmfversn,hduclas3,
     &            rmftlscop,rmfinstrum,rmfdet,rmffilt,rmfarea,
     &            rmfchantyp,rmfflchan,rmfchan,nenerg,numgrp,numelt,
     &            MEMR(p_energ_lo),MEMR(p_energ_hi),MEMI(p_ngrp),
     &            MEMI(p_F_chan),MEMI(p_N_chan),isorder,MEMI(p_order),
     &            MEMR(p_fmatrix),lo_thresh,errflg)
      errinfo = 'reading RMF extension '
      IF (errflg.NE.0) goto 482

      call ftclos(iunit2,errflg)

c
c --- REBIN RMF DATA IN CHANNEL DIMENSION ---
c

       call rbnr_rmf(nenerg,numelt,numgrp,maxchan,maxen,maxgrp,maxelt,
     &  MEMR(p_fmatrix),MEMI(p_ngrp),MEMI(p_F_chan),MEMI(p_N_chan),
     &  MEMR(p_fmatrix_comp),
     &  MEMI(p_ngrp_comp),MEMI(p_F_ch),MEMI(p_N_ch),
     &  MEMR(p_buffer1),MEMR(p_buffer2),lo_thresh,nfacts,stbins,
     &  endbins,cfacts,rmfflchan,chatter,errflg)
       errinfo = 'rebinning RMF data'
       IF (errflg.NE.0) goto 482

c
c --- SETUP ARRAYS FOR REBINNING IN ENERGY DIMENSION ---
c

       IF ( efile ) THEN
          call cmp_binfile(ebinfile,nebin,nenerg,1,nenerg,nfacts,
     &                     stbins,endbins,cfacts,maxcomp,errflg,chatter)
          errinfo = 'returning from cmp_binfile reading binning info'
          IF (errflg.NE.0) goto 482
       ELSE
          nfacts = 1
          stbins(1) = 1
          endbins(1) = nenerg
          cfacts(1) = ebfact
       ENDIF

c
c --- REBIN RMF DATA IN ENERGY DIMENSION ---
c Note that the first channel has been set to 1 in rbnr_rmf so
c pass this information into rbnr_ermf
c

      IF ( .NOT.(nfacts.EQ.1 .AND. cfacts(1).EQ.1) ) THEN
         call rbnr_ermf(nenerg,numelt,numgrp,maxchan,maxen,maxgrp,
     &  maxelt,MEMR(p_fmatrix),MEMI(p_ngrp),MEMI(p_F_chan),
     &  MEMI(p_N_chan),MEMR(p_energ_lo),MEMR(p_energ_hi),
     &  MEMR(p_fmatrix_comp),MEMI(p_ngrp_comp),MEMI(p_F_ch),
     &  MEMI(p_N_ch),MEMR(p_buffer1),MEMR(p_buffer2),lo_thresh,nfacts,
     &  stbins,endbins,cfacts,1,chatter,errflg)
         errinfo = 'rebinning RMF data'
         IF (errflg.NE.0) goto 482
      ENDIF

c --- WRITE DATA ---

      call copyphd(rmffilename,outfile,killit,errflg,chatter)
c Ziqin --- copy the extension before rmfextno & eboundextno
      IF (ebdfilename.EQ.rmffilename) THEN
        call rbnr_copyrem(rmffilename,outfile,ebdextno,rmfextno,
     &                    -1,errflg,chatter)
      ENDIF
      call cgetlun(iunit)
      block = 2280
C PDW 7/1/98: Use original filename
C      call ftopen(iunit,rmffilename,0,block,errflg)
      call ftopen(iunit,infile,0,block,errflg)

      errflg = 0
      block = 2280
      call cgetlun(ounit)
      call ftopen(ounit,outfile,1,block,errflg)
      nk_hist = 0 
      nk_comm = 0 

cccc --- WRITE RMF EXTENSION ---
cccc Note: rmfflchan set to unity since rebined version always starts 
cccc  at unity
ccc	rmfflchan = 1

c before writing to output file, to check what is the first channel
c user wants and set rmfflchan according to that

     
      if(fchan_out .eq. '%') then
         channel1=rmfflchan
      elseif(fchan_out .eq. '0') then
         channel1=0
      elseif(fchan_out .eq. '1') then
         channel1=1
      else
         errinfo='first channel should be either  % /0 /1'
         errflg=1
         goto 482
      endif

c now adjust the channel array so that it starts from rmfflchan 
c and not from  mandatory rmfflchan=1

      call adjust_channel2(MEMI(p_F_chan),channel1,maxen,
     &                     maxgrp,errflg,chatter) 
      errinfo='returning from adjust_channel2 for rmf'
      if(errflg .ne. 0) goto 482

      IF (ebdfilename.EQ.rmffilename) THEN
         IF( rmfextno.gt. ebdextno) goto 504
      ENDIF

502   CONTINUE 

      call wtrmf5(ounit,chatter,rmf_extname,rmf_extcomm,
     & nk_hist,hist,nk_comm,
     & comm,rsp_rmfversn,hduclas3,rmftlscop,rmfinstrum,
     & rmfdet,rmffilt,rmfarea,rmfchantyp,channel1,
     & numelt,nchan,nenerg,numgrp,
     & MEMR(p_energ_lo),MEMR(p_energ_hi),MEMI(p_ngrp),
     & MEMI(p_f_chan),MEMI(p_N_chan),qorder,MEMI(p_order),
     & MEMR(p_fmatrix),lo_thresh,errflg)
      errinfo = 'writing RMF extension !'
      IF (errflg.NE.0) goto 482

c
c ----- copy the EXTNAME value from the input
c       rmf file to the output rmf file
c
c      errflg = 0
c      call ftgkys(iunit, 'EXTNAME', rec, comment, errflg)
c      call ftukys(ounit, 'EXTNAME', rec, comment, errflg)


c
c --- WRITE EXTRA HISTORY ---
c
      errflg = 0
      call ftpdat(ounit,errflg)
      taskname = task//mnver
      errflg = 0
      call ftpkys(ounit,'CREATOR',taskname,
     & ' s/w task which wrote this dataset',errflg)
      exhist = ' The original file was '//infile
      errflg = 0
      call ftphis(ounit,exhist,errflg)
c
c --- WRITE REBIN COMMENT ---
c
      do i=1,nfacts
        write(cstart,100,IOSTAT = cerr) stbins(i)
        write(cend,100,IOSTAT = cerr) endbins(i)
        write(compchar,100,IOSTAT = cerr) cfacts(i)
        bincomm = ' DATA REBINNED FROM '//cstart//' TO '
     &//cend//' WITH A COMPRESSION FACTOR : '//compchar
        errflg = 0
        call rmvexsp(bincomm,exhist)
        call ftpcom(ounit,exhist,errflg)
      enddo       
        write(exhist,'(a,i1)')' With user defined first channel =',
     >                         channel1
        call ftpcom(ounit,exhist,errflg)

c
c --- WRITE REMAINING KEYWORDS IN RMF EXTENSION ---
c
	message = taskname//
     &	  ': following keywords copied from i/p file '//infile
      call ftpcom(ounit,message,errflg)
      call ftmahd(iunit,rmfextno+1,htype,errflg)
      call getkeys(ounit,keyspres,keys,chatter,errflg)
      call ftghsp(iunit,nkeys,nmore,errflg)
      do i=1,nkeys
        errflg = 0
        call ftgrec(iunit,i,rec,errflg)
        errinfo='reading RMF record'
        call wtfwrn(subname,version,chatter,0,errflg,errinfo)
        man_key = .false.
        do j=1,keyspres
          IF (rec(1:8).EQ.keys(j).or.ismankey(rec)) THEN
            man_key = .true.
          ENDIF
        enddo
        IF (.NOT.man_key) THEN
          call ftprec(ounit,rec,errflg)
        ENDIF
        errinfo = 'writing non-mandatory record '
        call wtfwrn(subname,version,chatter,0,errflg,errinfo)
      enddo      

      IF (ebdfilename.EQ.rmffilename) THEN
         IF( rmfextno.gt. ebdextno) goto 505
      ENDIF
503   CONTINUE
      IF (ebdfilename.EQ.rmffilename) THEN
        call rbnr_copyrem(rmffilename,outfile,ebdextno,rmfextno,
     &                    0,errflg,chatter)
      ENDIF
      IF (ebdfilename.EQ.rmffilename) THEN
         IF( rmfextno.gt. ebdextno) goto 502
      ENDIF

504   CONTINUE
c --- WRITE EBOUNDS EXTENSION ---
cccc Note: fchan set to unity since rebined version always starts 
cccc  at unity
cccc	efchan = 1

      call wtebd4(ounit,chatter,ebd_extname,ebd_extcomm,
     &  nk_hist,hist,nk_comm,
     &  comm,ebd_rmfversn,ebdtlscop,ebdinstrum,ebddet,
     &  ebdfilt,ebdarea,ebdchantyp,efchan,n_ebd,MEMR(p_e_min),
     &  MEMR(p_e_max),errflg)
      errinfo = 'writing EBOUNDS extension'
      IF (errflg.NE.0) GOTO 482

c      
c --- WRITE EXTRA HISTORY ---
c  
      call ftpdat(ounit,errflg) 
      taskname = task//mnver 
      errflg = 0 
      call ftpkys(ounit,'CREATOR',taskname,
     &' s/w task which wrote this dataset',errflg) 
      exhist = ' The original file was '//ebdfile
      call ftphis(ounit,exhist,errflg)  
c
c --- WRITE REBIN COMMENT ---
c
      do i=1,nfacts
        write(cstart,100,IOSTAT = cerr) stbins(i)
        write(cend,100,IOSTAT = cerr) endbins(i)
        write(compchar,100,IOSTAT = cerr) cfacts(i)
        bincomm = ' DATA REBINNED FROM '//cstart//' TO '
     &//cend//' WITH A COMPRESSION FACTOR : '//compchar
        call rmvexsp(bincomm,exhist)
        errflg = 0
        call ftpcom(ounit,exhist,errflg)
      enddo
       
        write(exhist,'(a,i1)')' With user defined first channel =',
     >                         efchan
        call ftpcom(ounit,exhist,errflg)

c --- WRITE REMAINING KEYWORDS IN EBOUNDS EXTENSION ---
c
      IF (ebdfilename.EQ.rmffilename) THEN
        call ftmahd(iunit,ebdextno+1,htype,errflg)
        iunit2=iunit
      ELSE
        errflg = 0
        call ftclos(iunit,errflg)
        errflg = 0
        call cgetlun(iunit2)
C PDW 7/1/98: Use original filename
C        call ftopen(iunit2,ebdfilename,0,block,errflg)
        call ftopen(iunit2,ebdfile,0,block,errflg)
        errflg = 0
        call ftmahd(iunit2,ebdextno+1,htype,errflg)
      ENDIF
      errflg = 0
	message = taskname//
     &	  ': following keywords copied from i/p file '//ebdfile
      call ftpcom(ounit,message,errflg)
      call getkeys(ounit,keyspres,keys,chatter,errflg)
      call ftghsp(iunit2,nkeys,nmore,errflg)     
      do i=1,nkeys
        errflg = 0
        call ftgrec(iunit2,i,rec,errflg)
        errinfo = 'readind record from EBOUNDS ext'
        call wtfwrn(subname,version,chatter,0,errflg,errinfo)
        man_key = .false.
        do j=1,keyspres
          IF (rec(1:8).EQ.keys(j).or.ismankey(rec)) THEN
            man_key = .true.
          ENDIF
        enddo
        IF (.NOT.man_key) THEN
          call ftprec(ounit,rec,errflg)
        ENDIF
        errinfo = 'writing non-mandatory record '
        call wtfwrn(subname,version,chatter,0,errflg,errinfo)
      enddo

      IF (ebdfilename.EQ.rmffilename) THEN
      IF( rmfextno.gt. ebdextno) THEN
          goto 503
      ENDIF
      ENDIF

505   CONTINUE
c
c --- MAKE COMPLETE COPIES OF ANY OTHER EXTENSIONS IN INFILE ---
c
      IF (ebdfilename.EQ.rmffilename) THEN
        call rbnr_copyrem(rmffilename,outfile,ebdextno,rmfextno,
     &                    1,errflg,chatter)

      ENDIF
  100 format(i4)

      errflg = 0
      call ftclos(iunit2,errflg)
      errflg = 0
      call ftclos(ounit,errflg)

c Check for errors
482   if(errflg.ne.0) then
         call wterrm(subname,version,errinfo)
         errflg = 1
         return
      endif

c Free the dynamic Memory

        call udmfre(p_ngrp,4,errflg)
        message = ' Failed to deallocate memory for ngrp'
        IF (errflg.NE.0) goto 485
        call udmfre(p_fmatrix,6,errflg)
        message = ' Failed to deallocate memory for fmatrix'
        IF (errflg.NE.0) goto 485
        call udmfre(p_F_chan, 4, errflg)
        message = ' Failed to deallocate memory for F_chan'
        IF (errflg.NE.0) goto 485
        call udmfre(p_N_chan, 4, errflg)
        message = ' Failed to deallocate memory for N_chan'
        IF (errflg.NE.0) goto 485
        call udmfre(p_order, 4, errflg)
        message = ' Failed to deallocate memory for order'
        IF (errflg.NE.0) goto 485
        call udmfre(p_energ_lo, 6, errflg)
        message = ' Failed to deallocate memory for energ_lo'
        IF (errflg.NE.0) goto 485
        call udmfre(p_energ_hi, 6, errflg)
        message = ' Failed to deallocate memory for energ_hi'
        IF (errflg.NE.0) goto 485
        call udmfre(p_e_min, 6, errflg)
        message = ' Failed to deallocate memory for e_min'
        IF (errflg.NE.0) goto 485
        call udmfre(p_e_max, 6, errflg)
        message = ' Failed to deallocate memory for e_max'
        IF (errflg.NE.0) goto 485

        call udmfre(p_fmatrix_comp, 6, errflg)
        message = ' Failed to deallocate memory for matrix_comp'
        IF (errflg.NE.0) goto 485
        call udmfre(p_ngrp_comp, 4, errflg)
        message = ' Failed to deallocate memory for ngrp_comp'
        IF (errflg.NE.0) goto 485
        call udmfre(p_F_ch, 4, errflg)
        message = ' Failed to deallocate memory for F_ch'
        IF (errflg.NE.0) goto 485
        call udmfre(p_N_ch, 4, errflg)
        message = ' Failed to deallocate memory for N_ch'
        IF (errflg.NE.0) goto 485
        call udmfre(p_buffer1, 6, errflg)
        message = ' Failed to deallocate memory for buffer1'
        IF (errflg.NE.0) goto 485
        call udmfre(p_buffer2, 6, errflg)
        message = ' Failed to deallocate memory for buffer2'
        IF (errflg.NE.0) goto 485
	call udmfre(p_ebdchan, 4, errflg)
        message = ' Failed to deallocate memory for ebdchan'
        IF (errflg.NE.0) goto 485

 485    if(errflg.NE.0) then
           call wterrm(subname,version,message)
           errflg = 99
        endif

      return
      end
c ----------------------------------------------------------------------
c     END OF RBNR_COMP
c ----------------------------------------------------------------------


*+RBNR_RMF
c Alex  -------------------------------------------------------
      subroutine rbnr_rmf(NENERG,NUMELT,NUMGRP,MAXCHAN,MAXEN,MAXGRP,
     &                    MAXELT,FMATRIX,NGRP,F_CHAN,N_CHAN,
     &                    FMATRIX_COMP,NGRP_COMP,F_CH,N_CH,BUFFER1,
     &                    BUFFER2,THRESH,NFACTS,STBINS,ENDBINS,CFACTS,
     &                    FCHAN,CHATTER,ERR)

c --- DESCRIPTION --------------------------------------------------
c
c This routine rebins RMF channels, and the energy arrays are
c changed accordingly.
c
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter, err, nfacts,nenerg,maxelt,fchan
      integer stbins(*), endbins(*), cfacts(*), maxchan,maxen
      INTEGER NUMELT, NUMGRP

      INTEGER numgrp_comp,MAXGRP
      INTEGER INDEX,Irmf,Imtrx,Igr,J,CH,Jmax,Kmax

      INTEGER NGRP(MAXEN),NGRP_COMP(MAXEN),F_CHAN(MAXGRP)
      INTEGER N_CHAN(MAXGRP),F_CH(MAXGRP),N_CH(MAXGRP)

      REAL THRESH,FMATRIX(MAXELT),FMATRIX_COMP(MAXELT)
      REAL BUFFER1(MAXCHAN),BUFFER2(MAXCHAN)

      integer new, i, stbin, stnew, endbin, nbin
      integer shft, endnew,ie,k
      integer first_chan(500),last_chan(500)
      integer igr2,itmp,igr3
      integer ibeg, iend
      logical curbin
      character(70) desc
      logical  counted, tcounted
c
c --- VARIABLE DIRECTORY -------------------------------------------
c  NENERG     int     : Number of energies in the RMF
c  NUMELT     int     : Number of response elements
c  MAXCHAN    int     : Size of channel arrays
c  MAXEN      int     : Size of energy arrats
c  MAXGRP     int     : Size of group arrays
c  MAXELT     int     : Size of arrays for response matrix elements
c  FMATRIX    real    : Response matrix elements
c  NGRP       int     : Number of response groups for each energy
c  F_CHAN     int     : First channel in response group
c  N_CHAN     int     : Number of channels in response group
c  FMATRIX_COMP real  : Temporary array for response elements
c  NGRP_COMP  int     : Temporary array for number of response groups
c  F_CH       int     : Temporary array for first channel
c  N_CH       int     : Temporary array for number of channels
c  BUFFER1    real    : Temporary work array
c  BUFFER2    real    : Temporary work array
c  THRESH     real    : Minimum value to include in response
c  NFACTS     int     : Number of compression intervals
c  STBINS     int     : Start of compression interval
c  ENDBINS    int     : End of compression interval
c  CFACTS     int     : Compression factor
c  FCHAN      int     : First channel
c  CHATTER    int     : Verbosity
c  ERR        int     : Error flag
c
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1993 July 27)
c Rehana Yusaf (1995 Aug 14) 1.0.1; stnew.GE.endbin changed to
c                                   stnew.GT.endbin - this was
c                                   only a problem for a comp fact
c                                   of 1.
c Rehana Yusaf   (1995 Oct 27) 1.0.2; if comp fact -1 then channel
c                                   is omitted
c
c AM (March 1999) 1.0.3;  Modified the algorithm to incorporate the fact that 
c                         the arrays are now 1-dimensional. Added a block that
c                         calculates the total number of elements and number 
c                         of channel groups for a compressed matrix.
c Ning Gan (1999 Jun 17)  1.0.4; Instead of deciding the rebined matrix
C                                by the threshhold, preserve the group
C                                structure of the original matrix. 
C                                Rebinned F_CH is started from 1.
c Ning Gan (1999 Jun 29)  1.0.5; Some cleanups and bug fixes on rbnr_rmf
c Ning Gan (1999 Aug 05)  1.0.6; Fixed the errors for xte.
c Ning Gan (1999 Aug 09)  1.0.7; Updated the value numgrp_comp
c Ning Gan (1999 OCt 20)  1.0.8; Bug fix.
c PDW      (2000 Apr 14)  1.0.9; Initialize NGRP_COMP array at top of loop

      character(5) version 
      parameter (version = '1.0.9')
*-
c ------------------------------------------------------------------
c
c --- USER INFO ---
c
      IF (chatter.GE.10) THEN
        desc = ' ... using RBNR_RMF Ver '//version
        call fcecho(desc)
      ENDIF

c
c Alex -----------------  BEGIN COMPRESSING  ------------- 
c

       INDEX = 0 
       Irmf  = 0
       Imtrx = 0
       Igr   = 0
       igr3  = 0

       SHFT = 1 - FCHAN

       numgrp_comp = 0

C Alex ---- Initialize the output F_CH() and N_CH() arrays

       DO J = 1,MAXGRP 
         F_CH(J) = 0
         N_CH(J) = 0
       ENDDO

c Alex ---- Initialize the output FMATRIX_COMP() array

       DO J = 1,MAXELT
         FMATRIX_COMP(J) = 0.E0
       ENDDO

c --------------------------------------------------------

       DO IE = 1,NENERG

       NGRP_COMP(IE) = 0

c Alex ---- Initialize the BUFFER array

	DO CH = 1,MAXCHAN
          BUFFER1(CH)   = 0.E0
          BUFFER2(CH)  = 0.E0
        ENDDO

c Alex ---- "Uncompress" matrix array and copy its elements for each energy 
c            row to the BUFFER1() array

	    Jmax = NGRP(IE)

	      DO J = 1,Jmax
                 Igr = Igr + 1
	         Kmax = N_CHAN(Igr)
                 first_chan(J) = f_chan(igr)
                 last_chan(J) = f_chan(igr) + Kmax -1
		 DO K = 1, Kmax
                    Imtrx = Imtrx + 1
                    CH = F_CHAN(Igr) -FCHAN + K  
                    BUFFER1(CH) = BUFFER1(CH) + FMATRIX(Imtrx)
                 ENDDO
              ENDDO
c
c   skip the 0 matrix.
c
           if(Jmax.eq.0) goto 64
                 
c
c   sort the first_chan(array) 
c 
           do i = 1,Jmax 
              do j = i+1, Jmax 
                 if(first_chan(j).lt.first_chan(i)) then 
                      itmp = first_chan(i) 
                      first_chan(i) = first_chan(j)
                      first_chan(j) = itmp
                      itmp = last_chan(i) 
                      last_chan(i) = last_chan(j)
                      last_chan(j) = itmp
                 endif
              enddo
           enddo 
     

c Alex ---  BEGIN COMPRESSING THE  CHANNELS  

        NEW   = 0
        igr2 = 1
        ibeg = first_chan(igr2)
        iend = last_chan(igr2)
        counted = .false.
        DO I = 1,NFACTS
	  STBIN = STBINS(I)
          ENDBIN = ENDBINS(I)
          STNEW  = STBIN
          NBIN   = CFACTS(I)
          IF(NBIN.GT.0) THEN
            CURBIN = .TRUE.
            DO WHILE(CURBIN)
              ENDNEW = STNEW + NBIN - 1
              NEW = NEW + 1
              DO K=STNEW,ENDNEW
                  BUFFER2(NEW) = BUFFER2(NEW)+BUFFER1(K+SHFT)
              ENDDO
C -------------------  THE CHANNELS ARE COMPRESSED ------------------------
C
C If a new group starts in the bin, 
C
54            if (ibeg.ge.stnew .and. ibeg.le.endnew.and. 
     *            iend.gt.endnew ) then
              
                if(.not.counted) then 
                    NGRP_COMP(IE)  = NGRP_COMP(IE)+1 
                    INDEX = INDEX + 1
                    IF (INDEX.GT.MAXGRP) THEN
                       DESC = 
     *                 'RBNR_RMF: Insufficient memory for N_ch, F_ch'
                       CALL FCECHO( DESC )
                       ERR = 1
                       RETURN
                    ENDIF
                    F_CH(INDEX) = NEW 
                    N_CH(INDEX) = 0
                    counted = .true. 
                endif
                Irmf = Irmf + 1
                FMATRIX_COMP(Irmf) = BUFFER2(NEW)
                N_CH(INDEX) = N_CH(INDEX) + 1
C
C  If the bin is inside the group,
C
              else if(ibeg.lt.stnew .and. iend.gt.endnew ) then 
                if(.not.counted) then 
                    NGRP_COMP(IE)  = NGRP_COMP(IE)+1 
                    INDEX = INDEX + 1
                    IF (INDEX.GT.MAXGRP) THEN
                       DESC = 
     *                 'RBNR_RMF: Insufficient memory for N_ch, F_ch'
                       CALL FCECHO( DESC )
                       ERR = 1
                       RETURN
                    ENDIF
                    F_CH(INDEX) = NEW 
                    N_CH(INDEX) = 0
                    counted = .true. 
                endif
                Irmf = Irmf + 1
                FMATRIX_COMP(Irmf) = BUFFER2(NEW)
                N_CH(INDEX) = N_CH(INDEX) + 1
C
C If a group ends in the bin...
C
              else if (iend.ge.stnew.and.iend.le.endnew .and.
     *                 ibeg.lt.stnew) then
                itmp = iend
                tcounted = counted
                if(igr2.lt.Jmax) then
                  igr3 = igr2 + 1
                  ibeg = first_chan(igr3)
                  iend = last_chan(igr3)
                  counted = .false.
                endif
C
C               If it is the last group or the current group occupies 
C               more space in  the current bin than the next group...
C
                if(itmp - stnew.gt.endnew-ibeg.or.igr2.eq.Jmax) then 
                     if(.not.tcounted) then 
                        NGRP_COMP(IE)  = NGRP_COMP(IE)+1 
                        INDEX = INDEX + 1
                        IF (INDEX.GT.MAXGRP) THEN
                           DESC = 
     *                 'RBNR_RMF: Insufficient memory for N_ch, F_ch'
                           CALL FCECHO( DESC )
                           ERR = 1
                           RETURN
                        ENDIF
                        F_CH(INDEX) = NEW 
                        N_CH(INDEX) = 0 
                     endif
                     Irmf = Irmf + 1
                     FMATRIX_COMP(Irmf) = BUFFER2(NEW)
                     N_CH(INDEX) = N_CH(INDEX) + 1
C
C               If the current group occupies less space in
C               the current bin than the next group...
C
                else if (igr2.lt.Jmax.and.ibeg.lt.endnew) then 
                     igr2 = igr3
                     goto 54
                endif
                if(igr2.ge.Jmax) goto 64
                igr2 = igr3
C
C If the group is inside the bin,
C              
              else if(ibeg.ge.stnew .and. iend.le.endnew) then 
                if(.not.counted) then 
                    NGRP_COMP(IE)  = NGRP_COMP(IE)+1 
                    INDEX = INDEX + 1
                    IF (INDEX.GT.MAXGRP) THEN
                       DESC = 
     *                'RBNR_RMF: Insufficient memory for N_ch, F_ch'
                       CALL FCECHO( DESC )
                       ERR = 1
                       RETURN
                    ENDIF
                    F_CH(INDEX) = NEW 
                    N_CH(INDEX) = 1
                    counted = .true.
                endif
                Irmf = Irmf + 1
                FMATRIX_COMP(Irmf) = BUFFER2(NEW)
                if(igr2.lt.Jmax) then
                   igr2 = igr2 + 1
                   ibeg = first_chan(igr2)
                   iend = last_chan(igr2)
                   counted = .false.
                else 
                   goto 64
                endif
C
C If the group is outside and before the bin,
C              
              else if(iend.lt.stnew) then 
                 if(igr2.lt.Jmax) then
                     igr2 = igr2 + 1
                     ibeg = first_chan(igr2)
                     iend = last_chan(igr2)
                     counted = .false.
                  else 
                     goto 64
                  endif
                  goto 54
               endif
               STNEW = ENDNEW + 1
               IF(STNEW.GT.ENDBIN) THEN
                  CURBIN = .FALSE.
               ENDIF
             ENDDO
           ENDIF
        ENDDO
64      continue
       numgrp_comp = numgrp_comp + NGRP_COMP(IE)
       ENDDO

c Alex ----     THE CHANNEL REBINNING IS DONE FOR ALL ENERGIES

c Copy the temporary arrays back into the RMF arrays

       numelt = Irmf
       numgrp = numgrp_comp
       DO i = 1, numelt
          FMATRIX(i) = FMATRIX_COMP(i)
       ENDDO
       DO i = numelt+1, maxelt
          FMATRIX(i) = 0.
       ENDDO
       DO i = 1, nenerg
          NGRP(i) = NGRP_COMP(i)
       ENDDO
       DO i = 1, numgrp_comp
          F_CHAN(i) = F_CH(i)
          N_CHAN(i) = N_CH(i)
       ENDDO
       DO i = numgrp_comp+1, MAXGRP
          F_CHAN(i) = 0
          N_CHAN(i) = 0
       ENDDO
           

c ----------------------------------------------------------------------
      
      IF (chatter.GE.20) THEN
        desc = ' RBNR_RMF : matrix is compresssed'
        call fcecho(desc)
      ENDIF
      return
      end

c -----------------------------------------------------------------------
c     END OF RBNR_RMF
c -----------------------------------------------------------------------
   

*+RBNR_ERMF
c --------------- -------------------------------------------------------
      subroutine rbnr_ermf(NENERG,NUMELT,NUMGRP,MAXCHAN,MAXEN,MAXGRP,
     & MAXELT,FMATRIX,NGRP,F_CHAN,N_CHAN,ELOW,EHIGH,FMATRIX_COMP,
     & NGRP_COMP,F_CH,N_CH,BUFFER1,BUFFER2,THRESH,NFACTS,STBINS,
     & ENDBINS,CFACTS,FCHAN,CHATTER,ERR)

c     -------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c This routine rebins the RMF in the energy dimension
c
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      integer CHATTER, ERR, NFACTS, NENERG, MAXELT, FCHAN
      integer MAXCHAN, MAXEN, NUMELT, MAXGRP, NUMGRP

      integer STBINS(*), ENDBINS(*), CFACTS(*)
      INTEGER NGRP(MAXEN),NGRP_COMP(MAXEN),F_CHAN(MAXGRP)
      INTEGER N_CHAN(MAXGRP),F_CH(MAXGRP),N_CH(MAXGRP)

      REAL THRESH,FMATRIX(MAXELT),FMATRIX_COMP(MAXELT)
      REAL ELOW(MAXEN),EHIGH(MAXEN)
      REAL BUFFER1(MAXCHAN),BUFFER2(MAXCHAN)

      REAL estart

      integer i, j, ie, ifacts, ormf, ogrp, oen
      integer irmf, igr, icount

      character desc*72

      logical qgroup
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c kaa (2006 Oct 20)  based on rbnr_rmf

      character(5) version 
      parameter (version = '1.0.0')
*-
c ------------------------------------------------------------------
c
c --- USER INFO ---
c
      IF (chatter.GE.10) THEN
        desc = ' ... using RBNR_ERMF Ver '//version
        call fcecho(desc)
      ENDIF

c
c Initialize the output arrays and counters
c

      ormf  = 0
      ogrp = 0
      oen = 0

      DO J = 1,MAXGRP 
         F_CH(J) = 0
         N_CH(J) = 0
      ENDDO

      DO J = 1,MAXELT
         FMATRIX_COMP(J) = 0.E0
      ENDDO

      DO j = 1, MAXCHAN
         BUFFER1(j) = 0
      ENDDO

c If the threshold has been set to zero then reset to the minimum value
c in the current matrix

      IF ( THRESH .EQ. 0. ) THEN
         THRESH = 1.0e35
         DO j = 1, numelt
            THRESH = MIN(THRESH, FMATRIX(j))
         ENDDO
      ENDIF

      IF ( THRESH .EQ. 0 ) THEN
         CALL fcecho(' ')
         desc = 
     & 'Threshold set to zero - please set LO_THRES in the input RMF'
         CALL fcecho(desc)
         CALL exit(1)
      ENDIF

c Initialize the pointer for the compression arrays

      ifacts = 1
      icount = 0

c Initialize the pointers for the input rmf

      irmf = 0
      igr = 0

c Loop over the energies

      estart = ELOW(1)

      DO ie = 1,NENERG

c Expand out the response for this energy and add into the BUFFER1 array

         DO j = 1, NGRP(ie)
            igr = igr + 1
            DO i = F_CHAN(igr), F_CHAN(igr)+N_CHAN(igr)-1
               irmf = irmf + 1
               BUFFER1(i+1-FCHAN) = BUFFER1(i+1-FCHAN) + FMATRIX(irmf)
            ENDDO
         ENDDO

c Check whether this is the last energy in a bin. If it is then load the
c contents of the BUFFER1 array into the output RMF and reinitialize the array

         icount = icount + 1
         IF ( icount .EQ. cfacts(ifacts) ) THEN

            oen = oen + 1
            ELOW(oen) = estart
            EHIGH(oen) = EHIGH(ie)
            estart = EHIGH(ie)

            DO i = 1, MAXCHAN
               BUFFER1(i) = BUFFER1(i)/cfacts(ifacts)
            ENDDO

            qgroup = .FALSE.
            NGRP_COMP(oen) = 0
            DO i = 1, MAXCHAN
               IF ( BUFFER1(i) .GE. THRESH ) THEN
                  ormf = ormf + 1
                  FMATRIX_COMP(ormf) = BUFFER1(i)
                  IF ( qgroup ) THEN
                     N_CH(ogrp) = N_CH(ogrp) + 1
                  ELSE
                     ogrp = ogrp + 1
                     F_CH(ogrp) = i + FCHAN-1
                     N_CH(ogrp) = 1
                     NGRP_COMP(oen) = NGRP_COMP(oen) + 1
                     qgroup = .TRUE.
                  ENDIF
               ELSE
                  IF ( qgroup ) qgroup = .FALSE.
               ENDIF
            ENDDO
            icount = 0
            DO i = 1, MAXCHAN
               BUFFER1(i) = 0.
            ENDDO

         ENDIF

c Check whether we need to move onto the next set of bins with a different
c compression factor.

         IF ( ie .EQ. endbins(ifacts) ) ifacts = ifacts + 1

      ENDDO

c Copy the temporary arrays back into the RMF arrays

      numelt = ormf
      DO i = 1, numelt
         FMATRIX(i) = FMATRIX_COMP(i)
      ENDDO
      DO i = numelt+1, maxelt
         FMATRIX(i) = 0.
      ENDDO

      nenerg = oen
      DO i = 1, nenerg
         NGRP(i) = NGRP_COMP(i)
      ENDDO
      DO i = nenerg+1, MAXEN
         NGRP(i) = 0
      ENDDO

      numgrp = ogrp
      DO i = 1, numgrp
         F_CHAN(i) = F_CH(i)
         N_CHAN(i) = N_CH(i)
      ENDDO
      DO i = numgrp+1, MAXGRP
         F_CHAN(i) = 0
         N_CHAN(i) = 0
      ENDDO
           
      IF (chatter.GE.20) THEN
        desc = ' RBNR_ERMF : matrix is compresssed'
        call fcecho(desc)
      ENDIF

      return
      end

c -----------------------------------------------------------------------
c     END OF RBNR_RMF
c -----------------------------------------------------------------------
   
*+RBNR_EBD
c     -------------------------------------------------------
      subroutine rbnr_ebd(n_ebd,ebdchan,e_min,e_max,nfacts,
     &              stbins,endbins,cfacts,fchan,err,chatter)
c     -------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c This routine rebins EBOUNDS channels, and the energy arrays are
c changed accordingly.
c
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      integer n_ebd, chatter, err, nfacts, fchan
      integer stbins(*), endbins(*), cfacts(*)
      integer ebdchan(*)
      real e_min(*), e_max(*)
c
c --- LOCALS ---
c
      integer new, i, stbin, stnew, endbin, nbin, endnew,shft
      logical curbin
      character(70) desc
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1993 July 16)
c Rehana Yusaf (1995 Aug 14) 1.0.1; change stnew.GE.endbin to
c                                   stnew.GT.endnew this was
c                                   only a problem for comp fact 1
c Rehana Yusaf (1995 Oct 27) 1.0.2; comp fact of -1 omits channel
c
      character(5) version 
      parameter (version = '1.0.2')
*-
c ------------------------------------------------------------------
c
c --- USER INFO ---
c
      IF (chatter.GE.10) THEN
        desc = ' ... using RBNR_EBD Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- COMPRESS --- 
c

      shft = 1 - fchan
      new = 1
      do i=1,nfacts
        stbin = stbins(i)
        endbin = endbins(i)
        stnew = stbin
        nbin = cfacts(i)
        IF (nbin.GT.0) THEN
          curbin = .true.
          do WHILE(curbin)
            endnew = stnew + nbin - 1
            ebdchan(new) = new
            e_min(new) = e_min(stnew+shft)
            e_max(new) = e_max(endnew+shft)
            new = new + 1
            stnew = endnew + 1
            IF (stnew.GT.endbin) THEN
              curbin = .false.
            ENDIF
          enddo
        ENDIF
      enddo
      n_ebd = new - 1
      return
      end
c -----------------------------------------------------------------------
c     END OF RBNR_EBD
c -----------------------------------------------------------------------
   


*+RBNR_COPYEM
c     ------------------------------------------------------
      subroutine rbnr_copyrem(infile,outfile,ebdextno,specextno,
     &                        flg,errflg,chatter)
c     ------------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c This routine copies any extensions, other than the EBOUNDS, and
c SPECRESP from infile to outfile.
c --------------------------------------------------------------------
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile
      integer chatter, errflg,ebdextno,specextno,flg
c
c --- INTERNALS ------------------------------------------------------
c
      integer ignore1,ignore2,iunit,block,i
      logical endfile,pkey,hist
      character(255) ap_in
      character(4) cnum
      integer htype, nhdu,status,tothd,iend,end_num,ierr
      character(32) errstr
      character(70) desc,errinfo
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments ...
c
c infile     char     : input filename
c outfile    char     : output filename
c errflg     int      : error flag
c chatter    int      : Chattiness flag
c
c --- AUTHORS/MODIFICATION -------------------------------------------
c
c Rehana Yusaf (1993 August 1)
c Rehana Yusaf (1993 Nov 22) 1.0.1; spexextno and ebdextno now passed
      character(5) version
      parameter (version = '1.0.2')
*-
c --------------------------------------------------------------------
c
c --- USER INFO ---
c
      errstr = ' ERROR : RBNR_COPYREM Ver '//version//':'
      IF (chatter.GE.10) THEN
        desc = ' ... using RBNR_COPYREM Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- DETERMINE SPECRESP AND EBOUNDS EXT Nos ---
c
      status = 0
      call cgetlun(iunit)
      call ftopen(iunit,infile,0,block,status)
      errinfo = errstr//' opening infile !'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
       errflg = 1
       return
      ENDIF
      endfile = .false.
      nhdu = 1
      tothd = 0
      do WHILE (.NOT.endfile)
        status = 0
        call ftmahd(iunit,nhdu,htype,status)
        IF ((status.EQ.107).OR.(status.EQ.207)) THEN
           endfile = .true.
           tothd = nhdu - 2
        ENDIF
        nhdu = nhdu + 1
      enddo
      
c
c --- COPY EXTENSIONS, OTHER THAN EBOUNDS OR SPECRESP ---
c
      IF (specextno.LT.ebdextno) THEN
        ignore1 = specextno
        ignore2 = ebdextno
      ELSE
        ignore1 = ebdextno
        ignore2 = specextno
      ENDIF

      iend = index(infile(1:),' ') - 1

c ... COPY EXTENSIONS BEFORE IGNORE1

      IF (flg.eq.-1) THEN

        pkey = .false.
        hist = .false.
        do i=1,(ignore1-1)
          write(cnum,100,IOSTAT=ierr) i       
          call crmvlbk(cnum)
          end_num = index(cnum(1:),' ') - 1
          IF (end_num.EQ.0) THEN
            end_num = 4
          ENDIF
          ap_in = infile(1:iend)//'['//cnum(1:end_num)//']'
          status = 0
          call cfappend(ap_in,outfile,pkey,hist,chatter)
        enddo

      ENDIF

c ... COPY EXTENSIONS INBETWEEN IGNORE1 AND IGNORE2

      IF(flg.eq. 0) THEN

        IF ((ignore1+1).NE.ignore2) THEN
          do i=(ignore1+1),ignore2-1
            write(cnum,100,IOSTAT=ierr) i
            call crmvlbk(cnum)
            end_num = index(cnum(1:),' ') - 1
            IF (end_num.EQ.0) THEN
              end_num = 4
            ENDIF
            ap_in = infile(1:iend)//'['//cnum(1:end_num)//']'
            status = 0
            call cfappend(ap_in,outfile,pkey,hist,chatter)          
          enddo
        ENDIF

      ENDIF

c ... COPY EXTENSIONS AFTER IGNORE2 

      IF(flg.eq.1) THEN

        IF (ignore2.NE.tothd) THEN
          do i=(ignore2+1),tothd
            write(cnum,100,IOSTAT=ierr) i
            call crmvlbk(cnum)
            end_num = index(cnum(1:),' ') - 1
            IF (end_num.EQ.0) THEN
              end_num = 4
            ENDIF
            ap_in = infile(1:iend)//'['//cnum(1:end_num)//']'
            status = 0
            call cfappend(ap_in,outfile,pkey,hist,chatter)    
          enddo
        ENDIF

      ENDIF

  100 FORMAT(I4)

      status = 0
      call ftclos(iunit,status)
      errinfo = errstr//' closing infile !'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
       errflg = 1
       return
      ENDIF

      return
      end
c ---------------------------------------------------------------
c     END OF RBNR_COPYREM
c --------------------------------------------------------------- 

*+CMP_BINFILE
c     ------------------------------------------------------ 
      subroutine cmp_binfile(binfile,nchan,detchans,
     &       fchan,lchan,nfacts,stbins,endbins,cfacts,
     &       maxcomp,errflg,chatter)
c     ------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c This subroutine reads binning info from an ascii file
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      character*(*) binfile
      integer nchan,detchans,fchan,maxcomp,nfacts,lchan
      integer stbins(maxcomp),endbins(maxcomp),cfacts(maxcomp)
      integer errflg,chatter
c
c --- VARIABLE DIRECTORY ------------------------------------------
c
c fchan    int  : starting channel 
c binfile  char : binning filename
c nchan  int  : Number of channels after compression
c detchans int  : Number of possible detector channels
c nchan    int  : Actual number of channels
c nfacts   int  : Number of compression factors
c cfacts   int  : Array of compression factors
c stbins   int  : Array of compression starting points for each cfact
c endbins  int  : Array of compression ending points for each cfact
c errflg   int  : Error flag
c chatter  int  : chatter flag
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
      character(5) version
      parameter (version = '1.0.0')
c  Rehana Yusaf (1995 August 1) 1.0.0;
c 
*-
c -------------------------------------------------------------------
c --- INTERNALS ---

      character(70) subinfo,errinfo
      character(5) chanchar,chanchar2,comp
      character(28) errstr
      integer st_bin,end_bin,c_fact,i,iunit,status
      integer prev_stbin,prev_endbin,curchans
c
c --- USER INFO ---
c
      errstr = ' ERROR:CMP_BINFILE Ver'//version//':'
      IF (chatter.GE.15) THEN
        subinfo = ' ... using CMP_BINFILE Ver '//version
        call fcecho(subinfo)
      ENDIF
c
c --- OPEN BINFILE ---
c
      call ftgiou(iunit,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr// ' problem getting free lun'
        call fcecho(errinfo)
        return
      ENDIF
      open(unit=iunit,file=binfile,status='old')
c
c --- READ COMPRESSION DATA FROM BINFILE ---
c
      nfacts = 0
      status = 0
      prev_stbin = 0
      prev_endbin = 0
      do i=1,maxcomp + 1
        read(iunit,*,IOSTAT=status,end=100)st_bin,end_bin,c_fact
        IF (status.NE.0) THEN
          subinfo = errstr//' invalid number in file'
          call fcecho(subinfo)
          errflg = 1
          return
        ENDIF
        IF ((nfacts + 1).GT.maxcomp) THEN
          subinfo = ' The compression array sizes have been exceeded'
          call fcecho(subinfo)
          write(subinfo,'(a,i8)')' The max array dimension is ',maxcomp
          call fcecho(subinfo)
          errflg = 3
          goto 200
        ENDIF
        IF (MOD((end_bin - st_bin + 1),c_fact).NE.0) THEN
          subinfo = ' Compression factor is not exact divisor of'
          call fcecho(subinfo)
          subinfo = ' starting and ending channels for this bin'
          call fcecho(subinfo)
          write(errinfo,350)st_bin,end_bin
          call rmvexsp(errinfo,subinfo)
          call fcecho(subinfo)
          write(errinfo,400) c_fact
          call rmvexsp(errinfo,subinfo)
          call fcecho(subinfo)
          errflg = 2
          goto 200
        ENDIF
        IF (nfacts.GE.1) THEN
          IF ((endbins(nfacts) +1).LT.st_bin) THEN
             nfacts = nfacts + 1
             stbins(nfacts) = endbins(nfacts - 1) + 1
             endbins(nfacts) = st_bin - 1
             cfacts(nfacts) = 1
          ENDIF
        ENDIF
        IF ((nfacts.EQ.0).AND.(st_bin.GT.fchan)) THEN
          nfacts = nfacts + 1
          stbins(nfacts) = fchan
          endbins(nfacts) = st_bin - 1
          cfacts(nfacts) = 1
        ENDIF
        nfacts = nfacts + 1
        stbins(nfacts) = st_bin
        endbins(nfacts) = end_bin
        cfacts(nfacts) = c_fact
        call ck_binchan(stbins(nfacts),endbins(nfacts),
     &    lchan,fchan,prev_stbin,prev_endbin,errflg,chatter)
        IF (errflg.NE.0)  THEN
           goto 200
        ENDIF
        prev_stbin = stbins(nfacts)
        prev_endbin = endbins(nfacts)
      enddo

 100  IF (endbins(nfacts).LT.lchan) THEN
        IF ((nfacts + 1).GT.maxcomp) THEN
          subinfo = ' The compression array sizes have been exceeded'
          call fcecho(subinfo)
          write(subinfo,'(a,i8)')' The max array dimension is ',maxcomp
          call fcecho(subinfo)
          errflg = 3
          goto 200
        ENDIF
        nfacts = nfacts + 1
        stbins(nfacts) = endbins(nfacts - 1) + 1
        endbins(nfacts)= lchan
        cfacts(nfacts) = 1
      ENDIF

c USER INFO 

      nchan = 0
      do i=1,nfacts
        IF (chatter.GE.20) THEN
          write(chanchar,300,IOSTAT=status) stbins(i)
          write(chanchar2,300,IOSTAT=status) endbins(i)
          write(comp,300,IOSTAT=status) cfacts(i)
          errinfo = chanchar//' - '//chanchar2
     &//' are binned with compression factor '//comp
          call rmvexsp(errinfo,subinfo)
          call fcecho(subinfo)
        ENDIF
        curchans =
     &  ((endbins(i) - stbins(i)+1)/cfacts(i))
        IF (curchans.GT.0) THEN
          nchan = nchan + curchans
        ENDIF
      enddo
      IF (chatter.GE.20) THEN
        write(subinfo,'(a,i8)')' final number of channels :',nchan
        call fcecho(subinfo)
      ENDIF
  200 close(unit=iunit)
      status = 0
      call ftfiou(iunit,status)
  300 FORMAT(I4)
  350 FORMAT(' starting channel:',I4,' ending channel:',I4)
  400 FORMAT(' with compression factor:',I4)
      return
      end
c     -------------------------------------------------------------
c     END OF CMP_BINFILE
c     ------------------------------------------------------------- 
     
          
*+CK_BINCHAN
c     -----------------------------------------------------
      subroutine ck_binchan(stchan,endchan,lchan,fchan,
     &                      prev_stchan,prev_endchan,
     &                      perr,chatter)
c     -----------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c
c This routine checks the validity of the channels used for rebinning.
c
c --- VARIABLES ------------------------------------------------------      
c
      IMPLICIT NONE
      integer stchan,endchan,lchan,perr,chatter
      integer fchan,prev_stchan,prev_endchan
c
c --- INTERNALS ------------------------------------------------------
c
      character(70) desc,indesc
      integer ierr
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments ...
c
c stchan       int    : Lower Channel value
c endchan      int    : Higher Channel value
c lchan        int    : Last channel
c fchan        int    : first channel
c prev_stchan  int    : Previous lower channel value, if none then pass 0
c prev_endchan int    : Previous higher channel value, if noen then pass 0
c perr      int    : Parser error flag
c                    perr = 0    Okay
c                    perr = 1    minchan < 0 or minchan > lchan
c                    perr = 2    maxchan < 0
c                    perr = 3    maxchan > lchan
c                    perr = 4    minchan > maxchan
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c Rehana Yusaf (1995 August 3)1.0.0; based on ck_chan in GRPPHA
      character(5) version
      parameter (version = '1.0.0')
*-
c --------------------------------------------------------------------
c
c --- CHECK THAT CHANNELS ARE WITHIN RANGE ---
c
      perr = 0
      IF ((stchan.LT.fchan).OR.(stchan.GT.lchan)) THEN
        perr = 1
        ierr = 0
        write(indesc,100,IOSTAT=ierr) stchan
        IF (ierr.EQ.0) THEN
          call rmvexsp(indesc,desc)
          IF (chatter.GT.0) THEN
            call fcecho(desc)
          ENDIF
        ENDIF
        write(indesc,150,IOSTAT=ierr) fchan,lchan
        IF (ierr.EQ.0) THEN
          call rmvexsp(indesc,desc)
          IF (chatter.GT.0) THEN
            call fcecho(desc)
          ENDIF
        ENDIF
        IF (stchan.EQ.endchan) THEN
          return
        ENDIF
      ENDIF            
      IF ((endchan.LT.fchan).OR.(endchan.GT.lchan)) THEN
        ierr = 0
        write(indesc,100,IOSTAT=ierr) endchan
        IF (ierr.EQ.0) THEN
          call rmvexsp(indesc,desc)
          IF (chatter.GT.0) THEN
            call fcecho(desc)
          ENDIF
        ENDIF
        write(indesc,150)fchan,lchan
        call rmvexsp(indesc,desc)
        call fcecho(desc)
        perr = 2
      ENDIF
c
c --- CHECK THAT STCHAN >=  ENDCHAN---
c
      IF (endchan.LT.stchan) THEN
        perr = 4
        ierr = 0
        write(indesc,200,IOSTAT=ierr) endchan,stchan
        IF (ierr.EQ.0) THEN
          call rmvexsp(indesc,desc)
          call fcecho(desc)
        ENDIF
      ENDIF
c
c --- CHECK THAT PREV_ENDCHAN < STCHAN ---
c
      IF (prev_endchan.NE.0) THEN
       IF ((prev_endchan.GE.stchan)
     &     .OR.(prev_stchan.GE.stchan)) THEN
        perr = 5
        write(desc,300) 
        call fcecho(desc)
        write(indesc,320) prev_stchan,prev_endchan,
     &  stchan,endchan
        call rmvexsp(indesc,desc)
        call fcecho(desc)
       ENDIF
      ENDIF
 100  FORMAT(' Channel',I6,' is not within the channel range !')
 150  FORMAT(' Channel range is ',I6,' - ',I6)
 200  FORMAT(' Channel ',I6,' is less than channel ',I6,' !')
 300  FORMAT
     &(' There is an overlap with the previous binning channels')
 320  FORMAT(I6,' - ',I6,' and the current binning ',I6,' - ',I6)
      return
      end
c ------------------------------------------------------------------
c     END OF CK_BINCHAN
c ------------------------------------------------------------------

*+ADJUST_CHANNEL1

      subroutine adjust_channel1(channel,channel1,nchan,errflg,chatter)

c -----------------------------------------------------------------
c This subroutine adjust the first channel no and following channels
c as required by user instead of enforcing to 1
c -----------------------------------------------------------------
c ------------------ variables -----------------------------
      implicit none
      integer nchan
      integer channel(nchan),channel1,errflg,chatter

c ------------ internal variables -------------------------

      character(170) subinfo
      integer i

c ------------------ Authors/modifications ---------------------
c Banashree Mitra Seifert (1996, Nov)1.0.0:
c      . To adjust the channel nos. according to user's prescription
c        in the main routine
c --------------------------------------------------------------

      character(15) subname
      parameter (subname='adjust_channel')
      character(5) version
      parameter (version='1.0.0')

c ----------------------------------------------------------------
      subinfo= 'using '//subname//'Ver '//version

      if(channel1 .eq. 0) then
         if(channel(1) .ne. 0) then
            do i=1,nchan
               channel(i)=channel(i) -1
            enddo
         endif
      else
         if(channel(1) .eq. 0) then
            do i=1,nchan
               channel(i)=channel(i) +1
            enddo
         else
            subinfo= 'does not need any adjustment'
            call wtinfo(chatter,30,3,subinfo)
         endif
      endif

      errflg = 0
      return
      end

c ------------------------------------------------------------------
c                END OF ADJUST_CHANNEL1
c -----------------------------------------------------------------

*+ADJUST_CHANNEL2

      subroutine adjust_channel2(f_chan,channel1,maxen,maxgrp,
     >                           errflg,chatter)

c -----------------------------------------------------------------
c This subroutine adjust the first channel no. and following channels,
c as required by user instead of enforcing to 1
c -----------------------------------------------------------------
c ------------------ variables -----------------------------
      implicit none
      integer channel1,maxen,maxgrp,errflg,chatter
      integer f_chan(maxgrp)

c ------------ internal variables -------------------------

      character(170) subinfo
     
c ------------------ Authors/modifications ---------------------
c Banashree Mitra Seifert (1996, Nov)1.0.0:
c      . To adjust the channel nos. according to user's prescription
c        in the main routine
C Ning Gan (1998 Nov) 1.0.1
C      . Discontinue use of adjust_channel1
c --------------------------------------------------------------

      character(15) subname
      parameter (subname='adjust_channel2')
      character(5) version
      parameter (version='1.0.0')

c ----------------------------------------------------------------
      subinfo= 'using '//subname//'Ver '//version
      call wtinfo(chatter,10,2,subinfo)

      call adjust_channel1(f_chan,channel1,maxgrp,errflg,chatter)
      errflg = 0
      return
      end

c ------------------------------------------------------------------
c              END OF ADJUST_CHANNEL2
c ------------------------------------------------------------------
    
C
C    check for the mandatory keywords. 
C 
        logical function ismankey (record)
        implicit none
        character*(*) record
        logical copyflag 
        logical l1,l2,l3,l4,l5,l6,l7,l8,l9,l10
        logical l11, l12,l13,l14,l15,l16, l17, l18, l19, l20 
        logical l21, l22, l23, l24, l25, l26, l27, l28, l29, l30
        logical l31, l32, l33, l34, l35,l36, l37, l38, l39, l40
        logical l41, l42
        integer fcstln


         l1 = index(record(1:6),'SIMPLE') .le. 0
         l2 = index(record(1:6),'BITPIX') .le. 0
         l3 = index(record(1:5),'NAXIS') .le. 0
         l4 = index(record(1:6),'EXTEND') .le. 0
         l5 = index(record(1:8),'XTENSION') .le. 0
         l6 = index(record(1:6),'PCOUNT') .le. 0
         l7 = index(record(1:6),'GCOUNT') .le. 0
         l8 = index(record(1:7),'TFIELDS') .le. 0
         l9 = index(record(1:5),'TTYPE') .le. 0
         l10 = index(record(1:5),'TBCOL') .le. 0
         l11 = index(record(1:5),'TFORM') .le. 0
         l12 = index(record(1:5),'TSCAL') .le. 0
         l13 = index(record(1:5),'TZERO') .le. 0
         l14 = index(record(1:5),'TNULL') .le. 0
         l15 = index(record(1:5),'TUNIT') .le. 0
         l16 = index(record(1:5),'THEAP') .le. 0
         l17 = index(record(1:4),'TDIM') .le. 0
         l18 = index(record(1:5),'TDISP') .le. 0
         l19 = index(record(1:6),'GROUPS') .le. 0
         l20 = index(record(1:6),'BSCALE') .le. 0
         l21 = index(record(1:5),'BZERO') .le. 0
         l22 = index(record(1:5),'BUNIT') .le. 0
         l23 = index(record(1:5),'BLANK') .le. 0
         l24 = index(record(1:5),'CTYPE') .le. 0
         l25 = index(record(1:5),'CRPIX') .le. 0
         l26 = index(record(1:5),'CROTA') .le. 0
         l27 = index(record(1:5),'CRVAL') .le. 0
         l28 = index(record(1:5),'CDELT') .le. 0
         l29 = index(record(1:3),'END') .le. 0
         l30 = index(record(1:7),'EXTNAME') .le. 0
         l31 = index(record(1:5),'TLMIN') .le. 0
         l32 = index(record(1:5),'TLMAX') .le. 0
         l33 = index(record(1:5),'OPTIC') .le. 0
         l34 = index(record(1:5),'TCRPX') .le. 0
         l35 = index(record(1:5),'TCRVL') .le. 0
         l36 = index(record(1:5),'TCDLT') .le. 0
         l37 = index(record(1:5),'TCTYP') .le. 0
         l38 = index(record(1:5),'TCROT') .le. 0
         l39 = index(record(1:6), 'PLTSCL') .le. 0
         l40 = index(record(1:3), 'TCD') .le. 0 .and.
     &        fcstln(record) .ge. 7
         l41 = index(record(1:8), 'CHECKSUM') .le. 0
         l42 = index(record(1:7), 'DATASUM') .le. 0

C check if this is a good record
         copyflag = l1 .and. l2 .and. l3 .and. l4 .and. l5
     &        .and. l6 .and. l7 .and. l8 .and. l9 .and. l10
     &        .and. l11 .and. l12 .and. l13 .and. l14 .and. l15
     &        .and. l16 .and. l17 .and. l18 .and. l19 .and. l20
     &        .and. l21 .and. l22 .and. l23 .and. l24 .and. l25
     &        .and. l26 .and. l27 .and. l28 .and. l29 .and. l30
     &        .and. l31 .and. l32 .and. l33 .and. l34 .and. l35
     &        .and. l36 .and. l37 .and. l38 .and. l39 .and. l40
     &        .and. l41 .and. l42 
         ismankey = .not.copyflag
         return
         end 

