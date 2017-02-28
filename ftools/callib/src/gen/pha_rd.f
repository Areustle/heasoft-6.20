
*+PHA_RD
c     ----------------------------------------------------------------
      subroutine pha_rd(infile,channel,counts,dtype,qerror,serr,
     &                  qsys,syserr,qqual,qualty,qgroup,grping,
     &                  qascale,ascale,qbscale,bscale,
     &                  rcts,nchan,detchan,phsize,texpos,shkeys,
     &                  skeys,ckeys,cform,ckrec,keydesc,nckeys,
     &                  hduclas2,phaversn,fchan,lchan,extnum,ctype,
     &                  instrum,tlscpe,dmode,cont,chatter)
c    ----------------------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c
c Reads SPECTRUM extension in PHA file, by calling RFTSPF, and 
c subsequently displays header
c
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE 
      character*(*) infile
      character*(*) hduclas2, phaversn, instrum, tlscpe, dmode
      character*(*) shkeys(*)
      integer phsize,dtype,chatter,skeys,fchan,lchan,extnum 
      integer channel(*),counts(*),qualty(*)
      integer grping(*), nckeys,detchan
      real serr(*),syserr(*),ascale(*),bscale(*),rcts(*),texpos
      logical qerror,qsys,qqual,qgroup,cont,qascale,qbscale
      character*(*) ckeys(*)
      character*(*) ckrec(*)
      character*(*) cform(*)
      character*(*) keydesc(*)
      character*(*) ctype
c
c --- INTERNALS -------------------------------------------------------
c
      character(20) filter
      character(120) xflt(9)
      character(120) backfile,corfile,respfile,ancrfile,curx
      character(11) detnam
      character(11) blank
      integer status,ierr,nchan,i,n_xflt,j,max_xflt
      integer iunit,strlen,clenact,fcstln
      real cscale
      character(70) desc,line,subinfo
      character(12) cnum
      character(4) xnum
cccc      character(6) xnum
      logical pois
      character(20) instr(9), outhdu(9,50),extname,outver(99)
      integer nsearch,next(99),ninstr
      character(20) extnames(50)
      integer errflg

c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c Arguments ...
c
c infile     char   : input file name (user defined)
c outfile    char   : Output filename (user defined)
c chatter    int    : Chattines flag, >20 verbose (user defined)
c subinfo    char   : User Routine/Version information
c phsize     int    : Array dimension
c channel    int    : Array of channels 
c counts     int    : Array of observed counts 
c rcts       real   : Array of count rate 
c dtype      int    : datatype, 1 if rcts ,2 if counts
c qualty     int    : Array of quality flag
c grping     int    : Array of grouping flag
c serr       real   : Observed statistical error
c syserr     real   : Fractional systematic error
c ascale     real   : Array of areascal
c bscale     real   : Array of backscal
c qgroup     logical: True if data is grouped
c qqual      logical: True if data has quality flags
c qerror     logical: True if statistical errors included
c qsys       logical: True if systematic errors included
c qascale    logical: True if vector ascale
c qbscale    logical: True if vector bscale
c nchan      int    : No. of detector channels
c cont       logical: False if nchan > phsize
c
c Internals ...
c
c tlscpe     char   : Telescope/Satellite name
c instrum    char   : Instrument name
c filter     char   : Instrument filter
c phaversn   char   : Version of FITS standard
c backfile   char   : Corresponding Background file
c corfile    char   : Corresponding Correction file
c respfile   char   : Associated redistribution matrix file
c ancrfile   char   : Corresponding ancillary response file
c ctype      char   : Channel type, corrected in any way 
c xflt(9)    char   : XSPEC selection filter description
c n_xflt     int    : Number of XFLT
c pois       logical: True if poission errors are appropriate to data
c status     int    : Error status flag
c ierr       int    : Error flag
c nchan      int    : Number of Detector Channels
c texpos     real   : Exposure time
c cscale     real   : Correction scaling factor
c desc       char   : Character string used for output
c line       char   : Underlining output
c subinfo    char   : User Routine/version information
c cnum       char   : Character used to display numerical values 
c
c --- CALLED ROUTINES -------------------------------------------------
c
c subroutine RDPHA2      : (CALLIB) Reads PHA extension 
c subroutine FCECHO      : (FTOOLS) Standalone write
c subroutine FNDHDU      : Find desired extension using hduclasn
c subroutine FNDEXT      : Find desired extension using extname 
c 
c --- COMPILATION AND LINKING -----------------------------------------
c
c Link with FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1993 March 30)
c Rehana Yusaf (1993 June 29)   :XFLT keywords read, mandatory keywords
c                                stored as character array, in order
c                                to display later.
c Rehana Yusaf (1993 Oct 10) 1.0.2 : replace RFTSPF with RDPHA1
c			             this has better error handling
c Rehana Yusaf (1993 Nov 19) 1.0.3; additional arguments - hduclas2,
c				    phaversn,fchan and extnum
c                                   In addition the SPECTRUM extension
c				    search has been extracted from the
c				    general reader, rdpha1 and fndhdu
c			            and fndext are used instead
c Rehana Yusaf (1994 July 14) 1.0.4; Mandatory display changed;if
c                                   keyword value > 14 then omit desc.
c                                   chkey keyword variables increased
c                                   to char*120
c Rehana Yusaf (1995 April 28th) 1.0.5; increase filename to 160
c
c Banashree Mitra Seifert (March,1996) 1.1.0:
c        . added screen display
c        . replaced by MVEXT
c        . format 200 changed from 14 to I6
c Banashree Mitra Seifert (July 1996) 1.2.0:
c        . added extnum=next(1)
c        . ctype introduced
c
c Banashree Mitra Seifert (Sept 1996) 1.3.0:
c        . xnum was changed to charecter *6 but was not correct
c          it should be character(4) and so the format instead of
c          200 was given the format i4
c Peter D Wilson (June 29, 1998) 1.3.1:
c        . Added max_xflt parameter to rdpha1 function call
c kaa (June 6, 2001) 1.3.2:
c        . Added support for vector AREASCAL and BACKSCAL
c ------------------------------------------------------------

      character(7) subname
      character(5) version
      parameter (version = '1.3.2')
*-
c ---------------------------------------------------------------------
      subname='pha_rd'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,1,subinfo)

c   
c --- OPEN PHAFILE ---
c
       status = 0
       extname='SPECTRUM'
c       ninstr = 2
       ninstr = 1
       instr(1) = 'SPECTRUM'
c       instr(2) = 'TOTAL'
       nsearch = 50 
       call mvext (0, infile, iunit, ninstr, instr, nsearch, next,
     >             outhdu, extnames, outver, extname, status, chatter)
       if (status .ne. 0) then
           subinfo='opening the input PHA file'
           call wtferr(subname,version,status,subinfo)
           call ftclos(iunit,status)
           subinfo = 'closing input PHA file'
           call wtferr(subname,version,status,subinfo)
           errflg=1
           return
       else
           extname =extnames(1)
       endif
 
      extnum=next(1)
        
      backfile = ' '
      corfile = ' '
      respfile = ' '
      ancrfile = ' '
      do i=1,9
        xflt(i) = '   '
      enddo
      max_xflt=9
c
c --- READ PHA EXTENSION ---
c
      call rdpha2(iunit,phsize,nchan,tlscpe,instrum,detnam,
     &            filter,detchan,texpos,qascale,ascale,qbscale,bscale,
     &            cscale,backfile,
     &		  corfile,respfile,ancrfile,xflt,max_xflt,n_xflt,dmode,
     &		  ctype,phaversn,hduclas2,fchan,channel,dtype,counts,
     &            rcts,qerror,serr,qsys,
     &            syserr,qqual,qualty,qgroup,grping,pois,ierr,chatter)

      call ftclos(iunit,status)
      subinfo = 'closing PHA file'
      call wtferr(subname,version,status,subinfo)
      call ftfiou(iunit,status)
      IF (ierr.NE.0) THEN
        subinfo = 'reading PHA FILE'
        call wterrm(subname,version,subinfo)
        cont = .false.
        return
      ENDIF

      lchan = channel(nchan)

c --- SET_KEY FOR CHKEY COMMAND ---

      call set_key(tlscpe,instrum,filter,texpos,qascale,ascale(1),
     &             qbscale,bscale(1),
     &             cscale,backfile,corfile,respfile,ancrfile,
     &             xflt,n_xflt,ckeys,nckeys,cform,
     &             ckrec,keydesc,ierr,chatter)    
c
c --- DISPLAY MANDATORY KEYWORDS ---
c 
      skeys = 1
      blank = ' '
      shkeys(skeys) = blank
      line = ' -------------------------'
      desc = ' MANDATORY KEYWORDS/VALUES'
      skeys = skeys + 1
      shkeys(skeys) = line
      skeys = skeys + 1
      shkeys(skeys) = desc
      skeys = skeys + 1
      shkeys(skeys) = line
      write(desc,300)
      skeys = skeys + 1
      shkeys(skeys) = desc
      skeys = skeys + 1
      shkeys(skeys) = desc
c      desc = ' EXTNAME   - '//'SPECTRUM'
      desc = ' EXTNAME   - '//extname
      desc(30:) = 'Name of this BINTABLE' 
      skeys = skeys + 1
      shkeys(skeys) = desc
      desc = ' TELESCOP  - '//tlscpe(1:11)
      desc(30:) = 'Mission/Satellite name'
      skeys = skeys + 1
      shkeys(skeys) = desc        
      desc = ' INSTRUME  - '//instrum(1:11)
      desc(30:) = 'Instrument/Detector'
      skeys = skeys + 1
      shkeys(skeys) = desc  
      desc = ' FILTER    - '//filter(1:11)
      desc(30:) = 'Instrument filter in use'
      skeys = skeys + 1
      shkeys(skeys) = desc  
      write(cnum,100) texpos
      call crmvlbk(cnum)
      desc = ' EXPOSURE  - '//cnum
      desc(30:) = 'Integration time (in secs) of PHA data'
c
c  Only let the user change AREASCAL and BACKSCAL if they are not vectors
c
      IF ( .NOT.qascale ) THEN
         skeys = skeys + 1
         shkeys(skeys) = desc  
         write(cnum,100) ascale(1)
         call crmvlbk(cnum)
         desc = ' AREASCAL  - '//cnum
         desc(30:) = 'Area scaling factor'
      ENDIF
      IF ( .NOT.qbscale ) THEN
         skeys = skeys + 1
         shkeys(skeys) = desc
         write(cnum,100) bscale(1)
         call crmvlbk(cnum)
         desc = ' BACKSCAL  - '//cnum
         desc(30:) = 'Background scaling factor'
      ENDIF
      skeys = skeys + 1
      shkeys(skeys) = desc
      strlen = clenact(backfile)
      IF (strlen.LE.14) THEN
         desc = ' BACKFILE  - '//backfile(1:14)
         desc(30:)= 'Associated background file'
      ELSE
         desc = ' BACKFILE  - '//backfile
         IF (strlen.GT.57) THEN
           desc(68:70)='...'
         ENDIF
      ENDIF
      skeys = skeys + 1
      shkeys(skeys) = desc
      write(cnum,100) cscale
      call crmvlbk(cnum)
      desc = ' CORRSCAL  - '//cnum
      desc(30:) = 'Correlation scaling factor'
      skeys = skeys + 1
      shkeys(skeys) = desc
      strlen = clenact(corfile)
      IF (strlen.LE.14) THEN
         desc = ' CORRFILE  - '//corfile(1:14)
         desc(30:)= 'Associated correlation file'
      ELSE
         desc = ' CORRFILE  - '//corfile
         IF (strlen.GT.57) THEN
           desc(68:70)='...'
         ENDIF
      ENDIF
      skeys = skeys + 1
      shkeys(skeys) = desc
      strlen = fcstln(respfile)
      IF (strlen.LE.14) THEN
         desc = ' RESPFILE  - '//respfile(1:14)
         desc(30:)= 'Associated redistribution matrix file'
      ELSE
         desc = ' RESPFILE  - '//respfile
         IF (strlen.GT.57) THEN
           desc(68:70)='...'
         ENDIF
      ENDIF
      skeys = skeys + 1
      shkeys(skeys) = desc
      strlen = clenact(ancrfile)
      IF (strlen.LE.14) THEN
         desc = ' ANCRFILE  - '//ancrfile(1:14)
         desc(30:)= 'Associated ancillary response file' 
      ELSE
         desc = ' ANCRFILE  - '//ancrfile
         IF (strlen.GT.57) THEN
           desc(68:70)='...'
         ENDIF
      ENDIF
      skeys = skeys + 1
      shkeys(skeys) = desc
      do i=1,n_xflt
ccc        write(xnum,200) i
ccc this modofication of write statement was done by BMS (ver 1.3.0)

        write(xnum,'(i4)') i
        do j=1,4
          IF (xnum(j:j).EQ.' ') THEN
             xnum(j:j) = '0'
          ENDIF
        enddo
        curx = '       '
        curx = xflt(i)
        strlen = clenact(xflt(i))
        IF (strlen.LE.14) THEN
          desc =' XFLT'//xnum//'  - '//curx(1:14)
          desc(30:) = 'XSPEC selection filter'
        ELSE
          desc =' XFLT'//xnum//'  - '//curx
          IF (strlen.GT.57) THEN
            desc(68:70)='...'
          ENDIF
        ENDIF
        skeys = skeys + 1
        shkeys(skeys) = desc
      enddo
      IF (pois) THEN
        cnum = 'TRUE'
      ELSE
        cnum = 'FALSE'
      ENDIF
      desc = ' POISSERR  - '//cnum
      desc(30:) = 'Whether Poissonian errors apply'
      skeys = skeys + 1
      shkeys(skeys) = desc
      desc = ' CHANTYPE  - '//ctype
      desc(30:) = 'Whether channels have been corrected'
      skeys = skeys + 1
      shkeys(skeys) = desc
      write(cnum,200) fchan
      call crmvlbk(cnum)
      desc = ' TLMIN1    - '//cnum
      desc(30:) = 'First legal Detector channel'
      skeys = skeys + 1
      shkeys(skeys) = desc 
      write(cnum,200) detchan
      call crmvlbk(cnum)
      desc = ' DETCHANS  - '//cnum
      desc(30:) = 'No. of legal detector channels'
      skeys = skeys + 1
      shkeys(skeys) = desc
      write(cnum,200) nchan
      call crmvlbk(cnum)
      desc = ' NCHAN     - '//cnum
      desc(30:) = 'No. of detector channels in dataset'
      skeys = skeys + 1
      shkeys(skeys) = desc
      desc = ' PHAVERSN  - '//phaversn
      desc(30:) = 'OGIP FITS version number'
      skeys = skeys + 1
      shkeys(skeys) = desc   
      IF (qerror) THEN
        cnum='TRUE'
      ELSE
        cnum = 'FALSE'
      ENDIF 
      desc = ' STAT_ERR  - '//cnum
      desc(30:) = 'Statistical Error'
      skeys = skeys + 1
      shkeys(skeys) = desc   
      IF (qsys) THEN
        cnum='TRUE'
      ELSE
        cnum = 'FALSE'
      ENDIF    
      desc = ' SYS_ERR   - '//cnum
      desc(30:)='Fractional Systematic Error'
      skeys = skeys + 1
      shkeys(skeys) = desc   
      IF (qqual) THEN
        cnum='TRUE'
      ELSE
        cnum = 'FALSE'
      ENDIF    
      desc = ' QUALITY   - '//cnum
      desc(30:) = 'Quality Flag'
      skeys = skeys + 1
      shkeys(skeys) = desc
      IF (qgroup) THEN
        cnum='TRUE'
      ELSE
        cnum = 'FALSE'
      ENDIF
      desc = ' GROUPING  - '//cnum
      desc(30:) = 'Grouping Flag'
      skeys = skeys + 1
      shkeys(skeys) = desc  
      write(desc,300) 
      skeys = skeys + 1
      shkeys(skeys) = desc  
      skeys = skeys + 1
      shkeys(skeys) = desc  
c
c --- INITIALISATION ---
c
      IF (.NOT.qsys) THEN
        do i=1,phsize
          syserr(i) = 0
        enddo
      ENDIF
      IF (.NOT.qqual) THEN
        do i=1,phsize
           qualty(i) = 0
        enddo
      ENDIF 
      IF (.NOT.qgroup) THEN
        do i=1,phsize
          grping(i) = 1
        enddo
      ENDIF
  100 format(1P, G12.5)
  200 format(I6)
  300 format(' ',68('-'))
  400 format(A4)
      return
      end

c ---------------------------------------------------------------
c        END OF PHA_RD 
c ---------------------------------------------------------------
