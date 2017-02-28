          SUBROUTINE addrmf
*  kaa 6/9/94
*
*   An FTOOL to add together RMFs.
*
* History :
*     v1.01  kaa  3/28/96   Added chantype support (uses wtrmf2 in place
*                           of wtrmf1).
*     v1.02  kaa  10/8/96   Added channel 0 support (uses wtrmf3 in place
*                           of wtrmf2).
*     v1.03  kaa  11/29/96  Further fixes for channel 0 support (uses rdrmf3
*                           to read response matrices).
*
*     v1.04  Banashree M Seifert  03/12/97  
*               . added option for input file list, as below:
*                 Extra option added so that instead of ascii filename
*                 user can also input filenames, and if this is the case,
*                 then user is asked for weight factors
*                 corrections to --   gpaddr.f
*                                     rdlstf.f
*                                     chkrmf.f
*
*     v1.05  Banashree M Seifert  04/09/97
*                 . rmfsiz = 2880 initialised before call to ftinit
*
*     v1.11  Peter D Wilson 02/20/98
*                 . Preserve history keywords of RSP extension of
*                   first RMF file
*
*     v1.12  Ning Gan 8/24/98
*                 . Bug fixes in chkrmf. Return if status != 0 ! 
*
*     v1.13  kaa 11/30/98
*                 . Removed references to EFFAREA keyword since it is 
*                   not part of the standard
*     
*     v1.14  Alex M. 02/11/99
*                 . Extended the list of parameters of subroutines 
*                   chkrmf and wadrmf to include maxne, maxgrp, and 
*                   maxelt. Replaced the wtrmf3 by a new version, 
*                   wtrmf4.
*
*     v1.15  Ning Gan 03/17/00
*                 . Increase sizes of strings of contxt, comment,
*                   instrg, and outstr to 511.  
*                 . Increase the total number of arf files from 100 to 2500.
*     v1.16  M Tripicco 10/31/00
*                 . Increased history/comment string length to 72 to match
*                 . new CFITSIO default and fcrhky (reads text from col 9)
*     v1.17  Ning Gan 12/18/00
*                 . Increased sizes of group and element array.
*                 . Added the error check for dimensions of group and
*                   element array.
*                 . Freed buffer array to avoid memory leak.    
*     v1.18  kaa  8/15/01
*                 . Fixed bug in CHKRMF which set MAXGRP, MAXELT from the
*                   file read rather than the maxima of all files.
*                 . Grabbed 10*MAXELT memory for the response elements instead
*                   of NFILES*MAXELT. This should be enough and should lead
*                   to fewer out of memory problems.
*    v1.19   kaa  8/16/01
*                 . Change that to MAX(NFILES,10)*MAXELT to minimize memory
*                   use when a small number of files are added together.
*    v1.20   kaa  1/3/06
*                   Minor fix to CHKRMF to handle correctly case of no EXTNAME
*                   for the EBOUNDS extension.
*    v1.21   bki  4/1/09
*                   Revision 1.19 should have used MIN(NFILES,10)*MAXELT.
*
* Parameters required by ADDRMF are :
*   listfil   s     File containing list of RMFs to be added
*   rmffile   s     Output RMF file
*   clobber   b     Overwrite output file ?
*   mode      s     Mode

* Local variables
*    clobber       overwrite output RMF.
*    rmffil        Output RMF.
*    inrmfs        The input RMFs.
*    infact        The weighting factors for the input RMFs.
*    iormfs        I/O units for the input RMFs.
*    rspval        Workspace array of response values for a given energy.
*    nfiles        Number of input RMFs.
*    nphbns        Number of PHA in the RMF.
*    thresh        Threshold below which RMF values are ignored
*    telescop      Telescope name
*    instrume      Instrument name
*    detnam        Detector name
*    filter        Filter name
*    chntyp        Detector channel type (PI or PHA)
*    flchan        First channel (usually either 0 or 1) in RESPONSE
*    maxne         Size of energy array
*    maxgrp        Size of response group array
*    maxelt        Size of RSP MATRIX array
*    hist          History records
*    nhist         Number of above
*    comm          Comment records
*    ncomm         Number of above
*    order         If true order information will be written
*    qorder        Grating order to which the response group belongs
*    hduvers       The RSP_MATRIX extension file format version
*    hduclas3      The RSP_MATRIX extension type.

* pointers for dynamic arrays

      INTEGER rspval, renlo, renhi, rngrp
      INTEGER rfchan, rnchan, rorder
      INTEGER rspval1, rspval2, rngrp1, rngrp2
      INTEGER rfchan1, rfchan2, rnchan1, rnchan2
      INTEGER buffer

* arrays for the input RMFs

      INTEGER MXRMFS
      PARAMETER(MXRMFS=2500)

      character(255) inrmfs(MXRMFS)
      REAL          infact(MXRMFS)

      INTEGER MXGRPS
      PARAMETER (MXGRPS=10)

* local variables

      REAL thresh

c Alex --- added maxne, maxgrp, and maxelt

      INTEGER rmfu,rmfsiz,ifile,nfiles,nphbns,status
      INTEGER outlen,iunit,maxne,maxgrp,maxelt,nhist,ncomm
      INTEGER numelt,numgrp
      INTEGER flchan

      LOGICAL clobber,qorder,RMFUOPEN

      character(255) rmffil
      character(511) contxt
      character(16) telescop,instrume,detnam,filter,chntyp
c 2000Oct31 MJT upped these to 72 for new CFITSIO default
      character(72) hist(MXRMFS+1),comm(10)
      character(5) hduvers
      character(20) hduclas3
      character(80) extnm,extnm_comm

* Dynamic memory allocation stuff
C  the following MEM common block definition is in the system iraf77.inc file
C
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB,MEMS,MEMI,MEML,MEMR,MEMD,MEMX)
      COMMON /MEM/ MEMD

C     datatype gives a symbolic code for the data type, e.g.,  
C     4 is Integer*4
C     1 is boolean
C     3 is short integer
C     4 is integer
C     5 is long integer
C     6 is single precision real
C     7 is double precision real
C     8 is complex

* External routines

      INTEGER FCSTLN
      EXTERNAL FCSTLN

* necessary for some ftools

      character(40) taskname
      COMMON /task/ taskname

      CALL fcecho('ADDRMF vers 1.21  1 April 2009.')

      taskname = 'addrmf v1.21'

* Get the parameters from "addrmf.par" file using gpaddr.
   
      status = 0
      call gpaddr(MXRMFS,infact,inrmfs,nfiles,rmffil,clobber,status)

      contxt = 'Error in returning from gpaddr'
      if (status .ne. 0) goto 999

* Use I/O unit 8 for the list files, 9 for the input RMFs 
* and 10 for the output RMF.

      iunit = 9
      rmfu = 10

* create the output rmf fits file - if it already exists and clobber is set 
* then overwrite.

      outlen = FCSTLN(rmffil)

      IF ( clobber ) CALL delfil(rmffil(:outlen))
      rmfsiz = 2880

      STATUS = 0
      RMFUOPEN = .FALSE.
      CALL ftinit(rmfu,rmffil(:outlen),rmfsiz,status)      
      contxt = 'Error creating output RMF file'
      IF ( status .NE. 0 ) GOTO 999
      RMFUOPEN = .TRUE.

* write filenames into the history keywords

      hist(1) = 'RMFs added using '//taskname
      DO ifile = 1, nfiles
         WRITE(hist(ifile+1),'(1pg12.5,1x,a)') infact(ifile),
     &               inrmfs(ifile)(1:MIN(59,FCSTLN(inrmfs(ifile))))
      ENDDO

      nhist = nfiles + 1
      ncomm = 0

c Alex --- initialize the variables( dosn't hurt ! ):

      qorder   = .false.
      nphbns   = 0
      maxne    = 0
      maxgrp   = 0
      maxelt   = 0
      flchan   = 0
      thresh   = 0.
      STATUS   = 0
      TELESCOP = ' '
      INSTRUME = ' '
      DETNAM   = ' '
      FILTER   = ' '
      CHNTYP   = ' '
      HDUVERS  = ' '
      HDUCLAS3 = ' '
      extnm    = ' '
      extnm_comm = ' '


* get the number of PHA bins and energies and run some checks to see 
* whether the RMFs ought to be added together. Also gets a bunch of
* keywords.

      CALL chkrmf(nfiles,inrmfs,infact,iunit,nphbns,maxne,
     & maxgrp,maxelt,telescop,instrume,detnam,filter,chntyp,
     & flchan,thresh,hduvers,hduclas3,contxt,
     & extnm,extnm_comm,status)
      IF (status .NE. 0) GOTO 999

C reset MAXGRP and MAXELT to take into account that adding together the
C rmfs may require more elements than in any single rmf.
 
      maxgrp = maxelt/2
      maxelt = MIN(nfiles,10)*maxelt

* grab the memory for the arrays

      rspval = 0
      CALL udmget(maxelt,6,rspval,status)
      WRITE(contxt, '(a,i8,a)')
     &  'Insufficient memory for the RSPVAL array : ', maxelt, 
     &  ' elements'
      IF ( status .NE. 0 ) GOTO 999

      renlo = 0
      CALL udmget(maxne, 6, renlo, status)
      contxt = 'Insufficient memory for the RENLO array'
      IF ( status .NE. 0 ) GOTO 999

      renhi = 0
      CALL udmget(maxne, 6, renhi, status)
      contxt = 'Insufficient memory for the RENHI array'
      IF ( status .NE. 0 ) GOTO 999

      rngrp = 0
      CALL udmget(maxne, 4, rngrp, status)
      contxt = 'Insufficient memory for the RNGRP array'
      IF ( status .NE. 0 ) GOTO 999

      rfchan = 0
      CALL udmget(maxgrp, 4, rfchan, status)
      contxt = 'Insufficient memory for the RFCHAN array'
      IF ( status .NE. 0 ) GOTO 999

      rnchan = 0
      CALL udmget(maxgrp, 4, rnchan, status)
      contxt = 'Insufficient memory for the RNCHAN array'
      IF ( status .NE. 0 ) GOTO 999

      rorder = 0
      CALL udmget(maxgrp, 4, rorder, status)
      contxt = 'Insufficient memory for the RORDER array'
      IF ( status .NE. 0 ) GOTO 999

* grab the memory for the work arrays used by ADDRMF. As a precaution
* we take double the size of the input arrays since the resulting matrix
* may have more groups and response elements than the input.

      rngrp1 = 0
      CALL udmget(maxne, 4, rngrp1, status)
      contxt = 'Insufficient memory for the RNGRP1 array'
      IF ( status .NE. 0 ) GOTO 999

      rngrp2 = 0
      CALL udmget(maxne, 4, rngrp2, status)
      contxt = 'Insufficient memory for the RNGRP2 array'
      IF ( status .NE. 0 ) GOTO 999

      rfchan1 = 0
      CALL udmget(maxgrp, 4, rfchan1, status)
      contxt = 'Insufficient memory for the RFCHAN1 array'
      IF ( status .NE. 0 ) GOTO 999

      rfchan2 = 0
      CALL udmget(maxgrp, 4, rfchan2, status)
      contxt = 'Insufficient memory for the RFCHAN2 array'
      IF ( status .NE. 0 ) GOTO 999

      rnchan1 = 0
      CALL udmget(maxgrp, 4, rnchan1, status)
      contxt = 'Insufficient memory for the RNCHAN1 array'
      IF ( status .NE. 0 ) GOTO 999

      rnchan2 = 0
      CALL udmget(maxgrp, 4, rnchan2, status)
      contxt = 'Insufficient memory for the RNCHAN2 array'
      IF ( status .NE. 0 ) GOTO 999

      rspval1 = 0
      CALL udmget(maxelt, 6, rspval1, status)
      contxt = 'Insufficient memory for the RSPVAL1 array'
      IF ( status .NE. 0 ) GOTO 999

      rspval2 = 0
      CALL udmget(maxelt, 6, rspval2, status)
      contxt = 'Insufficient memory for the RSPVAL2 array'
      IF ( status .NE. 0 ) GOTO 999

      buffer = 0
      CALL udmget(2*nphbns, 6, buffer, status)
      contxt = 'Insufficient memory for the BUFFER array'
      IF ( status .NE. 0 ) GOTO 999
      
* copy the primary header and EBOUNDS array from the first RMF into the 
* output RMF. adds the history keywords to the primary header

      CALL cphdeb(iunit,inrmfs(1),rmfu,nhist,hist,contxt,status)
      IF ( status .NE. 0 ) GOTO 999

* this is the subroutine that actually does all the work - create the 
* new matrix and associated arrays.

       CALL wadrmf(nfiles,inrmfs,infact,iunit,nphbns,
     & maxne,maxgrp,maxelt,thresh,flchan,MEMR(rspval),
     & MEMI(rngrp),MEMI(rfchan),MEMI(rnchan),MEMI(rorder),
     & MEMR(renlo),MEMR(renhi),MEMR(rspval1),MEMR(rspval2),
     & MEMI(rngrp1),MEMI(rngrp2),MEMI(rfchan1),MEMI(rfchan2),
     & MEMI(rnchan1),MEMI(rnchan2),MEMR(buffer),numgrp,numelt,
     & contxt,extnm,extnm_comm,extstatus)
       if (status.eq.1101) then 
         contxt = 'The maxelt is too small. Increase it in program.'
       endif
       if (status.eq.1102) then 
         contxt = 'The maxgrp is too small. Increase it in program.'
       endif
       IF (status.NE.0) GOTO 999

* now write out the RMF extension

c      CALL wtrmf4(rmfu,30,nhist,hist,ncomm,comm,hduvers,hduclas3,
c     & telescop,instrume,detnam,filter,1.0,chntyp,flchan,numelt, 
c     & nphbns,maxne,numgrp,MEMR(renlo),MEMR(renhi),MEMI(rngrp), 
c     & MEMI(rfchan),MEMI(rnchan),qorder,MEMI(rorder),
c     & MEMR(rspval),thresh,status)
      CALL wtrmf5(rmfu,30,extnm,extnm_comm,
     & nhist,hist,ncomm,comm,hduvers,hduclas3,
     & telescop,instrume,detnam,filter,1.0,chntyp,flchan,numelt, 
     & nphbns,maxne,numgrp,MEMR(renlo),MEMR(renhi),MEMI(rngrp), 
     & MEMI(rfchan),MEMI(rnchan),qorder,MEMI(rorder),
     & MEMR(rspval),thresh,status)

      IF ( status .NE. 0 ) GOTO 999

c Alex --- Reallocate memory

      call udmfre(rspval,6,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(renlo,6,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(renhi,6,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rngrp,4,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rfchan,4,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rnchan,4,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rorder,4,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rngrp1,4,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rngrp2,4,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rfchan1,4,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rfchan2,4,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rnchan1,4,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rnchan2,4,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rspval1,6,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(rspval2,6,status)
      IF (STATUS .NE. 0) GOTO 999

      call udmfre(buffer, 6, status)
      IF ( status .NE. 0 ) GOTO 999

* and close the output file

      status = 0
      IF(RMFUOPEN) THEN
      CALL ftclos(rmfu,status)
      status = 0
      ENDIF

      return

 999  continue
      call fcerr(contxt)
      return

      end
************************************************************************






