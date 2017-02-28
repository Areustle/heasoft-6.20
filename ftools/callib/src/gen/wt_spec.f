
*+WT_SPEC
c    --------------------------------------------------------------
      subroutine wt_spec(infile,outfile,hduclas2,phaversn,fchan,
     &                   channel,counts,rcts,dtype,qerror,serr,
     &                   qsys,syserr,qqual,qualty,qgroup,grping,
     &                   qascale,ascale,qbscale,bscale,
     &                   nchan,detchans,conv,phsize,specext,mnver,task,
     &                   n_comm,comms,ckeys,cform,ckrec,cdesc,nckeys,
     &                   cmpmode,ctype,errflg,chatter)
c    --------------------------------------------------------------
c
c --- DESCRIPTION -------------------------------------------------
c
c This subroutine writes the SPECTRUM extension with the current
c changes. It is appended onto the Primary Header. In OGIP standard
c format. Written for the HEASARC FTOOLS tasks GRPPHA.
c
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,outfile,ckeys(*),ckrec(*)
      character*(*) cform(*),cdesc(*),cmpmode,ctype
      character*(*) task, comms(*), hduclas2, phaversn
      integer phsize,nchan,chatter,specext,nckeys,n_comm,fchan
      integer channel(*),counts(*),qualty(*)
      integer grping(*),dtype,errflg,detchans
      integer*2 conv(*)
      real serr(*),syserr(*),rcts(*),ascale(*),bscale(*)
      logical qerror,qsys,qqual,qgroup,long_str
      logical qascale,qbscale
      character(5) mnver
c
c --- VARIABLE DIRECTORY ------------------------------------------
c
c Arguments ...
c
c infile      char  : PHA filename, user defined
c outfile     char  : Output filename, user defined
c chatter     int   : Chattines flag
c phsize      int   : Array dimensions
c channel     int   : Array of detector channels
c dtype       int   : Datatype, 1 if counts, 2 is rcts
c counts      int   : Array of observed counts
c rcts        real  : Array of count rate
c qualty      int   : Array of qualty flag
c grping      int   : Array of grouping flag
c serr        real  : Array of Observed statistical errors
c syserr      real  : Array of Fractional systematic error
c ascale      real  : Array of areascal values
c bscale      real  : Array of backscal values
c qgroup      logical: True if data is grouped
c qqual       logical: True if data has qualty flags
c qerror      logical: True if statistical errors included
c qsys        logical: True if systematic errors included
c qascale     logical: True if vector areascal
c qbscale     logical: True if vector backscal
c ckeys       char   : Keywords that can have their values changed
c cform       char   : contains keywords and their form ; "S" or "E"
c                      in the form RESPFILE S
c ckrec       char   : Records of ckeys
c cdesc       char   : CHKEY record descriptions
c nckeys      int    : counter for above                          
c
c --- CALLED ROUTINES ---------------------------------------------
c
c FTOPEN        : (FITSIO) Opens FITS file
c FTCRHD        : (FITSIO) Creates header
c FTMAHD        : (FITSIO) Move to specified header number
c FTMRHD        : (FITSIO) Move a specified number of headers
c                 forward or backward
c FTGHSP        : (FITSIO) Obtain the number of keywords
c FTPHIS        : (FITSIO) Write history keywords
c FTBDEF        : (FITSIO) Define Binary header
c FTPCL(IE)     : (FITSIO) Write (integer real) column
c FTCLOS        : (FITSIO) Close FITS FILE
c 
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1993 April 7)
c Rehana Yusaf (1993 June 23) : Add history about this routine
c Rehana Yusaf (1993 July 6)  : CKEYS, and CKREC are used
c Rehana Yusaf (1993 July 19) : Comment array is passed to routine
c Rehana Yusaf (1993 August 6) (1.0.4): Use FTPREC instead of CFTPREC,
c                                illegal characters in comments or
c                                history, no longer supported                              
c Rehana Yusaf (1993 Nov 18) 1.0.5; hduclas2,phaversn, detchans and
c                                fchan are passed to this subroutine
c				 In addition new keyword CREATOR is
c                                used to write taskname and version
c Rehana Yusaf (1993 Dec 1) 1.0.6; Fix getkeys related bug, some keywords
c		                 were written twice.
c Rehana Yusaf (1993 Dec 6) 1.0.7; Fix chkey bug - chkey keywords
c				 were not being written after last
c                                change
c Rehana Yusaf (1994 Feb 23) 1.0.8; Write counts column in J form
c                                prev written as I.
c Rehana Yusaf (1994 July 7) 1.0.9; additional argument, cform,cdesc
c Rehana Yusaf (1994 Sept 14) 1.1.0; use FTPKYL to write long strings
c Rehana Yusaf (1994 Oct 26) 1.1.1; If any of the chkeys are not in the
c                                   original file then still write them
c Rehana Yusaf (1994 Dec 19) 1.1.2; POISERR = T when qerror = T
c                                   POISERR = F when false
c                                   bug fix, write 'CHANNEL' not ' CHANNEL'
c Rehana Yusaf (1995 Dec 14) 1.1.3; addd wtinfo and friends
c Rehana Yusaf (1996 Feb 22) 1.1.4; added cmpmode
c
c Banashree Mitra Seifert (1996, March) 1.2.0:
c        . corrected call for wtwarm
c        . variable sizes to carry over from calling routine instead of
c          defining fixed as phsize
c
c  Banashree Mitra Seifert (1996, JulY) 1.3.0:
c        . CHANTYPE included to write in the output file
c
c Banashree Mitra Seifert (1996, Sept12)1.4.0:
c        . if POISSON error not applicable, then POISSERR=FALSE
c          (it was by mistake TRUE)
c
c Banashree M Seifert (1997, May 15) 1.5.0:
c        . parameter hist increased from charecter*80 to 200 so
c          it can write long infile names
c kaa (2001 June 6) 1.6.0:
c        . Added support for vector AREASCAL and BACKSCAL
c bki (2007 Aug 27) 1.7.0:
c        . Use new routine updpha instead of wtpha2
c ------------------------------------------------------------------
      character(5) version
      parameter (version = '1.7.0')
*-
c -----------------------------------------------------------------
c
c
c --- INTERNALS ---------------------------------------------------
c
      INTEGER MAXKEYS
      PARAMETER(MAXKEYS=100)
      character(7) subname
      parameter (subname='wt_spec')
      character(80) subinfo
      character(8) key,keys(MAXKEYS),keyval
      character(10) form
      character(5) hduvers
      character(80) rec
      character(200) hist
      character(136) temprec
      character(28) taskname,dummystr,dumcom
      REAL rval
      integer nfields,status,keyspres,lchan,pos
      parameter (nfields = 6)
      character(16) ttype(nfields),tform(nfields),tunit(nfields)
      integer i,rw,block,nkeys,nmore,key_val,vd,step,j,len,fcstln
      integer htype,frow,felem,colnum,iunit,ounit,iend,tfields
      logical man_key,at_end,chkey,wt_chkeys(64)

c --- USER INFO ---
c
      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,15,1,subinfo)
c
c --- OPEN FILES ---
c
      status = 0
      call ftgiou(iunit,status)
      IF (status.NE.0) THEN
        subinfo = ' cannot obtain free lun'
        call wterrm(subname,version,subinfo)
        errflg = status
        return
      ENDIF
      rw = 0
      status = 0
      block = 2880
      call ftopen(iunit,infile,rw,block,status)
      IF (status.NE.0) THEN
        subinfo = ' problem opening file:'//infile
        call wtferr(subname,version,status,subinfo)
        errflg = status
      ENDIF

c MOVE TO SPECTRUM EXTENSION IN INPUT FILE

      status = 0
      call ftmahd(iunit,specext+1,htype,status)
      status = 0
      call ftghsp(iunit,nkeys,nmore,status)
      rw = 1
      block = 2880
      status = 0
      call ftgiou(ounit,status)
      IF (status.NE.0) THEN
        subinfo = ' cannot obtain free lun'
        call wterrm(subname,version,subinfo)
        errflg = status
        return
      ENDIF
      call ftopen(ounit,outfile,rw,block,status)
      subinfo = ' opening new file:'//outfile
      If (status.NE.0) THEN 
        call wtferr(subname,version,status,subinfo)
        errflg = 1
        return
      ENDIF
c
c --- MOVE TO END OF OUTFILE, IN ORDER TO APPEND EXTENSION ---
c
      status = 0
      step = 1
      at_end = .false.
      do WHILE(.NOT.at_end)
        call ftmrhd(ounit,step,htype,status)
        IF (status.EQ.107) THEN
          at_end =.true.
          status = 0
        ENDIF
      enddo 

c
c --- WRITE BASIC SPECTRAL DATA ---
c Leave some of the keyword entries blank since we will fill them
c in below. updpha takes data as a real regardless of dtype so make
c sure to convert if necessary

      IF (dtype .EQ. 1) THEN
         do i=1,nchan
           rcts(i) = FLOAT(counts(i))
         enddo
      ENDIF

      status = 0
      CALL updpha(iunit,ounit,chatter,0,' ',0,
     &            ' ',' ',' ',' ',' ',
     &            phaversn,hduclas2,fchan,1.,qascale,ascale,
     &            ' ',qbscale,bscale,' ',
     &            1.,' ',' ',detchans,ctype,
     &            channel,rcts,dtype,qerror,serr,qsys,
     &            syserr,qqual,qualty,qgroup,grping,nchan,
     &            status)

c
c --- WRITE THE REMAINING KEYWORDS ---
c

      status =0
      call ftgkys(iunit,'EXTNAME',dummystr,dumcom,status)
      status =0
      call ftukys(ounit,'EXTNAME',dummystr,dumcom,status)

      do j=1,nckeys
       wt_chkeys(j) = .false.
      enddo

      long_str = .false.
      status = 0
      call getkeys(ounit,keyspres,keys,chatter,status)
      IF ( keyspres .GT. MAXKEYS ) THEN
         subinfo = 'Programming error - increase MAXKEYS'
         call wtferr(subname,version,1,subinfo)
      ENDIF
      do i=1,nkeys
        status = 0
        call ftgrec(iunit,i,rec,status)
        man_key=.false.
        chkey = .false.
        len = 80
        do j=1,nckeys
          temprec = ckrec(j)
          key = temprec(1:8)
          IF (rec(1:8).EQ.key) THEN
            chkey =.true.
            form = cform(j)
            IF ((key.EQ.form(1:8)).AND.(form(10:10).EQ.'S')) THEN
              len = fcstln(ckrec(j))
              status = 0
              temprec = ckrec(j)
              IF (len.GT.68) THEN
               call ftgkys(iunit,key,dummystr,dumcom,status)
               subinfo = ' moving to next record'
               IF (chatter.GE.20) THEN
                call wtferr(subname,version,status,subinfo)
               ENDIF
              ENDIF
              status = 0
              IF ((len-11).LT.68) THEN
               call ftukys(ounit,key,temprec(11:len),cdesc(j),status)
              ELSE
               call ftukls(ounit,key,temprec(11:len),cdesc(j),status)
               long_str = .true.
              ENDIF
              subinfo = ' writing'//key//' keyword'
              call wtferr(subname,version,status,subinfo)
            ELSE
              temprec = ckrec(j)
              len = fcstln(ckrec(j))
              READ(temprec(11:len),*) rval
              CALL ftukye(ounit,key,rval,6,cdesc(j),status)
              subinfo = ' writing '//key//' keyword'
              call wtferr(subname,version,status,subinfo)
            ENDIF
            wt_chkeys(j) = .true.
          ENDIF
        enddo
        IF (.NOT.chkey) THEN
           do j=1,keyspres
             IF (keys(j).EQ.rec(1:8)) THEN
               man_key = .true.
             ENDIF
           enddo
           IF ((rec(1:8).EQ.'QUALITY').OR.(rec(1:8).EQ.'GROUPING')
     &       .OR.(rec(1:8).EQ.'SYS_ERR')) THEN
               man_key =.true.
           ENDIF
           IF ((rec(1:5).EQ.'TTYPE').OR.(rec(1:5).EQ.'TUNIT')
     &       .OR.(rec(1:5).EQ.'TFORM')) THEN
               man_key = .true.
           ENDIF
        ENDIF
ccccccccccccc
        IF ((.NOT.man_key).AND.(.NOT.chkey)) THEN
          call ftprec(ounit,rec,status)
        ENDIF
        subinfo = ' writing record'
        call wtferr(subname,version,status,subinfo)
ccccccccccccccccccc
      enddo

c
c --- Write CHKEY keywords that are not in the input file ---
c
      do j=1,nckeys
        IF (.NOT.wt_chkeys(j)) THEN
           temprec = ckrec(j)
           key = temprec(1:8)
           form = cform(j)
           IF ((key.EQ.form(1:8)).AND.(form(10:10).EQ.'S')) THEN
              len = fcstln(ckrec(j))
              status = 0
              IF ((len-11).LT.68) THEN
               call ftukys(ounit,key,temprec(11:len),cdesc(j),status)
              ELSE
               call ftukls(ounit,key,temprec(11:len),cdesc(j),status)
               long_str = .true.
              ENDIF
              subinfo = ' writing'//key//' keyword'
              call wtferr(subname,version,status,subinfo)
           ELSE
              pos = index(ckrec(j)(12:),' ')
cccc              pos = 12 + pos
              pos = 12 + pos-1
              IF (pos.LT.32) THEN
                pos = 31
              ENDIF
              temprec = ckrec(j)(1:pos)//'/'//cdesc(j)
cccc              call ftprec(ounit,temprec(1:len),status)
              call ftprec(ounit,temprec,status)
              subinfo = ' writing '//key//' keyword'
              call wtferr(subname,version,status,subinfo)
           ENDIF
cccc           subinfo = key//' not present in infile'
cccc           call wtinfo(chatter,20,2,subinfo)
        ENDIF
      enddo
c
c --- WRITE DATE AND CREATOR KEYWORDS ---
c
      taskname = task//' '//mnver

      status = 0
      call ftukys(ounit,'CREATOR',taskname(1:17),
     &     's/w task which wrote this dataset',status)
      subinfo = ' writing CREATOR keyword'
      call wtferr(subname,version,status,subinfo)

      status = 0
      call ftpdat(ounit,status)
      subinfo = ' writing DATE keyword'
      call wtferr(subname,version,status,subinfo)
c
c --- WRITE EXTRA HIST KEYWORDS ---
c
      IF (long_str) THEN
        call ftplsw(ounit,status)
      ENDIF
      status = 0
      call ftupch(task)
      if (task.eq.'RBNPHA') then
        call ftpkys(ounit,'CMPMODE',
     &              cmpmode,' compression mode',status)
        subinfo = ' problem writing CMPMODE keyword'
        call wtfwrn(subname,version,chatter,15,status,subinfo)
      endif
      status = 0
      hist = ' This extension has been written by WT_SPEC Ver '
     &//version
      call ftphis(ounit,hist,status)
      iend = index(infile(1:),' ')
      hist = ' The original fits file was '//infile(1:iend)
      call ftphis(ounit,hist,status)
      iend = index(outfile(1:),' ')
      hist = ' This is file : '//outfile(1:iend)
c
c --- WRITE ADDITIONAL COMMENT(S) IF PASSED ---
c
      do i=1,n_comm
        status = 0
        call ftpcom(ounit,comms(i),status)
      enddo
      status = 0
c
c --- CLOSE THE FITS FILES ---
c
 999  status = 0
      call ftclos(iunit,status)
      status = 0
      call ftclos(ounit,status)
      call ftfiou(iunit,status)
      call ftfiou(ounit,status)
      return
      end
c ------------------------------------------------------------------
c     END OF WT_SPEC
c ------------------------------------------------------------------


 
