*+CMPPHA
c      -----------------
       subroutine cmppha
c      -----------------
c --- DESCRIPTION -----------------------------------------------------
c CMPPHA converts a typeII pha file to a type I pha file.
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE
      character(160) infile, outfile
      character(80) context,desc,rowexp,dclnam
      character(80) backfexp,rmfexp,arfexp,corfexp
      character(40) termdesc
      character(8) task
      character(6) cmpmode
      integer errflg,chatter
      logical killit
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c infile     char   : Input filename
c outfile    char   : Output filename
c chatter    int    : Chattiness flag (>20 verbose)
c --- CALLED ROUTINES -------------------------------------------------
c
c CMP_GP     : Gets parameters
c 
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1995 May 8) 1.0.0;
c Peter D Wilson (1998 June 29) 1.0.1:
c       . Eliminated INQUIRE call on infile
C Ning Gan (1999 March 22) 1.0.2
C       Expanded the maxchan from 256 to 256*256
C Ning Gan (1999 March 22) 1.0.3
C       The nchan is changed to be decided by tform of channel instead.
C Koji Mukai (2000 September 18) 1.1
C       Introduced a new hidden parameter, rows, which controls which
C       spectra to squish/expand.  The default for rows would be "-"
C       meaning all rows.
C Koji Mukai (2000 October 17) 1.1.1
C       Introduced a new hidden parameter, datacol, which allows users
C       to override the standard Type II PHA file column name (which is
C       the default, " ") so it can produce BACKGROUND_UP/DOWN spectra.
C Ken Ebisawa (2003 April 14) 1.2
C       Two bugs were fixed:
C       (1) When there is only one spectrum in the input file, and/or
c           one spectrum is output, the program fails (blank file is     
c           written).
C       (2) ANCRFILE, RESPFILE etc values in the input spectral columns
C           (if any), are not copied to the output file keywords besides
C           the first row.
C Bryan Irby (2003 August) 1.3
C       Increased input spectra limit from 256 to 1024
C Bryan Irby (2009 March) 1.4
C       Increase maxcol to handle more than 20 columns. In cmp_ex_wt: 
C       When wtpha1 returns an error, set forl=1 so that the output file
C       is closed.

      character(5) version
      parameter (version = '1.4')
      character(40) taskname
      COMMON/task/taskname
      taskname='CMPPHA'//version
*-
c ---------------------------------------------------------------------
c
c --- GET PARAMETERS ---
c
      context = 'fatal error'
      termdesc =' CMPPHA Ver '//version//' terminated !'
      errflg = 0
      call cmp_gp(infile,outfile,cmpmode,rowexp,dclnam,backfexp,rmfexp,
     &            arfexp,corfexp,killit,errflg,chatter)
      IF (errflg.NE.0) THEN
        call fcerr(context)
        call fcecho(termdesc)
        return
      ENDIF
c
c --- USER INFO ---
c
      IF (chatter.GE.1) THEN
        desc = ' Main CMPPHA Ver '//version
        call fcecho(desc)
      ENDIF
      task = 'CMPPHA'
c
c --- READ INFILE, APPLY CMPMODE AND WRITE OUTFILE ---
c
      call cmp_dma_rd_wt(infile,outfile,cmpmode,rowexp,dclnam,
     &              backfexp,rmfexp,arfexp,corfexp,taskname,
     &              chatter,killit,errflg)
      IF (chatter.GE.1) THEN
        desc = ' Completed CMPPHA Ver '//version
        call fcecho(desc)
      ENDIF
      return
      end
c ----------------------------------------------------------------------
c     END OF MAIN CMPPHA
c ----------------------------------------------------------------------
    
*+CMP_GP
c     --------------------------------------------------------
      subroutine cmp_gp(infile,outfile,cmpmode,rowexp,dclnam,backfexp,
     &           rmfexp,arfexp,corfexp,killit,errflg,chatter)
c     --------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile, cmpmode, rowexp, dclnam
      character*(*) backfexp,rmfexp,arfexp,corfexp
      logical killit
      integer errflg,chatter

c
c --- ARGUMENT DIRECTORY ---------------------------------------------
c
c infile     char   : input file name
c outfile    char   : Output filename
c cmpmode    char   : Expansion mode Squish or Expand
c rowexp     char   : Input rows to squish/expand
c dclnam     char   : Input data column name to operate on
c backfexp   char   : Background file name to be written in o/p file keyword
c rmfexp     char   : RMF file name to be written in o/p file keyword
c arfexp     char   : ARF file name to be written in o/p file keyword
c corfexp    char   : Correction file name to be written in o/p file keyword
c killit     bool   : Overwrite output file if it already exists?
c chatter    int    : Chattiness flag, >20 verbose
c errflg     int    : Error flag
c
c --- INTERNAL VARIABLES ---------------------------------------------
c
      character(160) filename
      character(160) ill_files(5)
      integer n_ill
      character(26) errstr, wrnstr
      integer status, extnum
      character(70) desc
      logical ext,val_cmp,valfil
      integer flen, fcstln
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
c Rehana Yusaf (1995 May 8) 1.0.0;
c Peter D Wilson (1998 June 29) 1.0.1:
c       . Eliminated INQUIRE call on infile
c Koji Mukai (2000 September 18) 1.1.0:
c         Added 'rows' parameter,
c Koji Mukai (2000 October 17) 1.1.1:
c         Added 'datacol' parameter,

      character(5) version 
      parameter (version = '1.2')
*-
c ---------------------------------------------------------------------
c
      errstr = ' ERROR : CMPPHA Ver '//version//':'
      wrnstr = ' WARNING : CMPPHA Ver '//version//':'

c GET INFILE

      status = 0
      call uclgst('infile',infile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting infile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(infile)
      IF (infile.EQ.'  ') THEN
        errflg = 1
        desc = ' Input PHA file not entered !'
        call fcecho(desc)
        return
      ENDIF
C PDW 6/29/98: Leave it to FTOPEN to determine
C      call fcpars(infile,filename,extnum,status)
C      ext = .true.
C      flen = fcstln(filename)
C      INQUIRE(FILE=filename(:flen),EXIST=ext)
C      IF (.NOT.ext) THEN
C        errflg = 1
C        desc = errstr//' File does not EXIST :'//filename
C        call fcecho(desc)
C        return
C      ENDIF

c GET OUTFILE 

      status = 0
      call uclgst('outfile',outfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting outfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET CLOBBER

      call uclgsb('clobber',killit,status)
      IF (status.NE.0) THEN
        killit = .false.
        desc = errstr//' ... getting clobber value '
        call fcecho(desc)
      ENDIF


c OUTFILE VALIDATION 

      call crmvlbk(outfile)
      IF (outfile.EQ.'  ') THEN
        desc = errstr//' outfile must be entered !!'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      n_ill = 1
C PDW 6/29/98: Strip off possible extension number first
C      ill_files(1) = infile
      call ftrtnm(infile, ill_files(1), status)
      call ck_file(outfile,ill_files,n_ill,valfil,
     &             killit,chatter)
      IF (.NOT.valfil) THEN
        errflg = 2
        return
      ENDIF

c GET CMPMODE

      status = 0
      call uclgst('cmpmode',cmpmode,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting cmpmode parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF           

c CHECK CMPMODE VALIDITY

      call ftupch(cmpmode)
      call crmvlbk(cmpmode)
      val_cmp = .false.
      IF (cmpmode(1:1).EQ.'E') THEN
        val_cmp = .true.
      ELSEIF (cmpmode(1:1).EQ.'S') THEN
        val_cmp = .true.
      ENDIF
      IF (.NOT.val_cmp) THEN
        desc = errstr//' Invalid CMPMODE !' 
        call fcecho(desc)
        desc = ' VALID CMPMODES : Squish or Expand'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF 

c GET ROWEXP

      status = 0
      call uclgst('rows',rowexp,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting rows parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF           

c GET DCLNAM

      status = 0
      call uclgst('datacol',dclnam,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting datacol parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF           

c GET BACKFEXP

      status = 0
      call uclgst('backfile',backfexp, status)
      IF ((status.NE.0).OR.(backfexp.EQ.'  ')) THEN
        desc = errstr//' Getting BACKFILE Parameter'
        call fcecho(desc)
        desc = ' setting BACKFILE to NONE'
        call fcecho(desc)
        backfexp = 'NONE'
        status = 0
      ELSEIF ((backfexp.EQ.'none').OR.(backfexp.EQ.'NULL').OR.
     &          (backfexp.EQ.'null')) THEN
        backfexp = 'NONE'
      ENDIF

c GET CORFEXP

      status = 0
      call uclgst('corrfile',corfexp, status)
      IF ((status.NE.0).OR.(corfexp.EQ.'  ')) THEN
        desc = errstr//' Getting CORFILE Parameter'
        call fcecho(desc)
        desc = ' setting CORFILE to NONE'
        call fcecho(desc)
        corfexp = 'NONE'
        status = 0
      ELSEIF ((corfexp.EQ.'none').OR.(corfexp.EQ.'NULL').OR.
     &          (corfexp.EQ.'null')) THEN
        corfexp = 'NONE'
      ENDIF

c GET ARFEXP

      status = 0
      call uclgst('arfile',arfexp, status)
      IF ((status.NE.0).OR.(arfexp.EQ.'  ')) THEN
        desc = errstr//' Getting ARFILE Parameter'
        call fcecho(desc)
        desc = ' setting ARFILE to NONE'
        call fcecho(desc)
        arfexp = 'NONE'
        status = 0
      ELSEIF ((arfexp.EQ.'none').OR.(arfexp.EQ.'NULL').OR.
     &          (arfexp.EQ.'null')) THEN
        arfexp = 'NONE'
      ENDIF

c GET RMFEXP

      status = 0
      call uclgst('rmfile',rmfexp, status)
      IF ((status.NE.0).OR.(rmfexp.EQ.'  ')) THEN
        desc = errstr//' Getting RMFEXP Parameter'
        call fcecho(desc)
        desc = ' setting RMFEXP to NONE'
        call fcecho(desc)
        rmfexp = 'NONE'
        status = 0
      ELSEIF ((rmfexp.EQ.'none').OR.(rmfexp.EQ.'NULL').OR.
     &          (rmfexp.EQ.'null')) THEN
        rmfexp = 'NONE'
      ENDIF

c GET CHATTER 

      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting chatter parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF    
      return
      end
c ---------------------------------------------------------------------
c     END OF CMP_GP
c ---------------------------------------------------------------------


*+CMP_DMA_RD_WT
c    -------------------------------------------------------------
      subroutine cmp_dma_rd_wt(infile,outfile,cmpmode,rowexp,
     &           dclnam,backfexp,rmfexp,arfexp,corfexp,taskname,
     &           chatter,killit,errflg)
c    -------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c This subroutine reads the input TYPE II phafile row by row and
c applies the cmpmode. Subsequently the output file is written.
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,outfile,cmpmode,rowexp,dclnam
      character*(*) backfexp,rmfexp,arfexp,corfexp,taskname
      integer chatter,errflg
      logical killit
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c Rehana Yusaf (1995 May 9) 1.0.0;
c
c Koji Mukai (2000 September 18) 1.1.0;
c   Now allows processing of selected rows only.
c Koji Mukai (2000 October 17) 1.1.1;
c   Now allows using a non-standard column name.
      character(6) version
      parameter (version = '1.1.1')
*-
c ----------------------------------------------------------------
C **** DYNAMIC MEMORY ALLOCATION ****
C  the following MEM common block definition is in the system iraf77.inc
C  file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c INTERNALS
      character(33) errstr,wrnstr
      character(80) subinfo,subinfo2,message
      integer iunit
      integer nsearch,ninstr,status
      parameter (nsearch = 50)
      integer next(nsearch)
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9)
      character(8) extname
      integer maxchan,p_channel,p_ipha,p_grping,p_sq_pha
      integer p_qualty,p_pha,p_staterr,p_sysfrc
      integer p_sq_grping,p_sq_qual
      integer p_sq_ipha,p_sq_chan,p_sq_staterr,p_sq_sysfrc
      integer p_areascal,p_backscal,p_corscal,p_exposure

c
c --- USER INFO ---
c
      errstr = ' ERROR : CMP_DMA_RD_WT '//version 
      wrnstr = ' WARNING : CMP_DMA_RD_WT '//version
      IF (chatter.GE.10) THEN
        subinfo = ' using CMP_DMA_RD_WT Ver '//version
        call fcecho(subinfo)
      ENDIF
c
c --- DETERMINE MAXCHAN ---
c
      call cgetlun(iunit)
      ninstr = 1
      instr(1) = 'SPECTRUM'
      call mvext(0,infile,iunit,ninstr,instr,nsearch,next,outhdu,
     &           extnames,outver,extname,errflg,chatter)
      IF (errflg.NE.0) THEN
       subinfo = errstr//' problem moving to SPECTRUM ext'
       call fcecho(subinfo)
       return
      ENDIF
      maxchan = 1024*1024

      IF (chatter.GE.30) THEN
        write(subinfo,'(a,i12)') ' Maximum array size:',maxchan
        call rmvexsp(subinfo,subinfo2)
        call fcecho(subinfo2)
      ENDIF
c
c --- SET DMA ARRAYS ---
c
        p_areascal = 0
        p_backscal = 0
        p_corscal = 0
        p_channel = 0
        p_exposure = 0
        p_grping = 0
        p_ipha = 0
        p_pha = 0
        p_qualty = 0
        p_sq_chan = 0
        p_sq_ipha = 0
        p_sq_grping = 0
        p_sq_pha = 0
        p_sq_qual = 0
        p_sq_staterr = 0
        p_sq_sysfrc = 0
        p_staterr = 0
        p_sysfrc = 0

        status = 0 
        call udmget(maxchan, 4, p_channel, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 4, p_ipha, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 4, p_qualty, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 4, p_grping, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 4, p_sq_ipha, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 4, p_sq_grping, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 4, p_sq_qual, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 4, p_sq_chan, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 6, p_staterr, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 6, p_sysfrc, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 6, p_pha, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 6, p_sq_pha, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 6, p_sq_staterr, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 6, p_sq_sysfrc, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 6, p_areascal, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 6, p_backscal, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 6, p_corscal, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        status = 0
        call udmget(maxchan, 6, p_exposure, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
  50    if(status.NE.0) then
                message = errstr// ' Failed to allocate Dynamic Memory'
                call fcecho(message)
                errflg = -1
                status = 0
                call ftclos(iunit,status)
                return 
        endif 
        
c ---   READ/SQUISH or EXPAND DATA ---

        call cmp_rd_wt(iunit,infile,outfile,cmpmode,rowexp,
     &     dclnam,backfexp,rmfexp,arfexp,corfexp,taskname,
     &     MEMI(p_channel),MEMI(p_ipha),MEMI(p_grping),
     &     MEMI(p_qualty),MEMI(p_sq_chan),MEMI(p_sq_ipha),
     &     MEMI(p_sq_qual),MEMI(p_sq_grping),MEMR(p_pha),
     &     MEMR(p_staterr),MEMR(p_sysfrc),MEMR(p_sq_pha),
     &     MEMR(p_sq_staterr),MEMR(p_sq_sysfrc),MEMR(p_areascal),
     &     MEMR(p_backscal),MEMR(p_corscal),MEMR(p_exposure),
     &     maxchan,chatter,killit,errflg)
        return
        end
c -------------------------------------------------------------------------
c     END OF CMP_DMA_RD_WT
c -------------------------------------------------------------------------


*+CMP_RD_WT
c    -------------------------------------------------------------
      subroutine cmp_rd_wt(iunit,infile,outfile,cmpmode,rowexp,
     &        dclnam,backfexp,rmfexp,arfexp,corfexp,taskname,
     &        channel,ipha,grping,
     &        qualty,sq_chan,sq_ipha,
     &        sq_qual,sq_grping,pha,
     &        staterr,sysfrc,sq_pha,
     &        sq_staterr,sq_sysfrc,areascal,backscal,corscal,
     &        exposure,maxchan,chatter,killit,errflg)
c    -------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c This subroutine reads the input TYPE II phafile row by row and
c applies the cmpmode. Subsequently the output file is written.
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      integer iunit
      character*(*) infile,outfile,cmpmode,rowexp,dclnam
      character*(*) backfexp,rmfexp,arfexp,corfexp,taskname
      integer channel(*),ipha(*),grping(*)
      integer qualty(*),sq_chan(*),sq_ipha(*)
      integer sq_qual(*),sq_grping(*)
      real pha(*),staterr(*),sysfrc(*),sq_pha(*)
      real sq_staterr(*),sq_sysfrc(*)
      real areascal(*),backscal(*),corscal(*)
      real exposure(*)
      integer maxchan,chatter,errflg
      logical killit
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c Rehana Yusaf (1995 May 9) 1.0.0;
c
c Koji Mukai (2000 September 18) 1.1.0;
c     Now allows operation on selected rows
c Koji Mukai (2000 October 17) 1.1.1;
c     Now allows using a non-standard column name.
c Ken Ebisawa (2003 April 14)
c     Fixed two bugs.
C       (1) When there is only one spectrum in the input file, and/or
c           one spectrum is output, the program fails (blank file is     
c           written).
C       (2) ANCRFILE, RESPFILE etc values in the input spectral columns
C           (if any), are not copied to the output file keywords besides
C           the first row.

      character(6) version
      parameter (version = '1.2')
*-
c ----------------------------------------------------------------
c INTERNALS
      character(33) errstr,wrnstr,comm
      character(80) subinfo,subinfo2
      integer lenact
      integer ncol,maxcol
      parameter (maxcol = 100)
      character(20) ttype(maxcol),tunit(maxcol)
      character(5) tform(maxcol)
      logical anyflg,pois
      integer i,ivar,nspecs,status,felem,inull,nchan
      integer fchan,lchan,forl
      integer chancol,ratecol,ctscol,statcol,syscol,qualcol
      integer grpcol,dtype
      integer nranges, frows(15), lrows(15), k
      real totexp
      real enull
      logical qerror,qsys,qqual,qgroup
      character(16) telescop(1024),instrume(1024),detnam(1024)
      character(16) filter(1024)
      character(160) rmfile(1024),arfile(1024),corfile(1024)
      character(160) backfile(1024)
      integer detchans
      character(16) hduclas2,chantype 
c
c --- USER INFO ---
c
      errstr = ' ERROR : CMP_RD_WT Ver '//version
      wrnstr = ' WARNING : CMP_RD_WT '//version
      IF (chatter.GE.10) THEN
        subinfo = ' using CMP_RD_WT Ver '//version
        call fcecho(subinfo)
      ENDIF
c
c --- OPEN INFILE ---
c
c      call cgetlun(iunit)
c      ninstr = 1
c      instr(1) = 'SPECTRUM'
c      call mvext(0,infile,iunit,ninstr,instr,nsearch,next,outhdu,
c     &           extnames,outver,extname,errflg,chatter)
c      IF (errflg.NE.0) THEN
c       subinfo = errstr//' problem moving to SPECTRUM ext'
c       call fcecho(subinfo)
c       return
c      ENDIF
c
c --- READ NSPEC and DETERMINE/LOCATE COLUMNS ---
c
      call ftghbn(iunit,maxcol,nspecs,ncol,ttype,tform,tunit,
     &            comm,ivar,errflg)
      subinfo = errstr//' reading binary header info'
      call wt_ferrmsg(errflg,subinfo)
      IF (errflg.NE.0) THEN
        errflg = 2
        return
      ENDIF

      IF (chatter.GE.15) THEN
        write(subinfo,'(a,i12,a)') ' There are ',nspecs,' spectra'
        call rmvexsp(subinfo,subinfo2)
        call fcecho(subinfo2)
      ENDIF

c Parse the 'rows' parameter
      call fcgrgs(rowexp,nspecs,nranges,frows,lrows)
      IF (nranges.LE.0) THEN
        errflg = 5
        subinfo = errstr//' parsing the ROWS parameter'
        call wt_ferrmsg(errflg,subinfo)
        return
      END IF

c DETERMINE WHICH COLUMNS ARE PRESENT AND SET LOGICALS ACCORDINGLY

      dtype = 0
      qerror = .false.
      qsys = .false.
      qqual = .false.
      qgroup = .false.
      totexp = 0.0
      do i=1,ncol
        IF (ttype(i).EQ.'COUNTS') THEN
          dtype = 1
          call ftgcno(iunit,.false.,'COUNTS',ctscol,errflg)
          subinfo = errstr//' finding COUNTS column number'
          call wt_ferrmsg(errflg,subinfo)
          IF (errflg.NE.0) THEN
            return
          ENDIF
        ELSEIF (ttype(i).EQ.'RATE') THEN
          dtype = 2
          call ftgcno(iunit,.false.,'RATE',ratecol,errflg)
          subinfo = errstr//' finding RATE column number'
          call wt_ferrmsg(errflg,subinfo)
          IF (errflg.NE.0) THEN
            return
          ENDIF
        ELSEIF (ttype(i).EQ.'STAT_ERR') THEN
          qerror = .true.
          call ftgcno(iunit,.false.,'STAT_ERR',statcol,errflg)
          subinfo = errstr//' finding STAT_ERR column number'
          call wt_ferrmsg(errflg,subinfo)
          IF (errflg.NE.0) THEN
            return
          ENDIF
        ELSEIF (ttype(i).EQ.'SYS_ERR') THEN
          qsys = .true.
          call ftgcno(iunit,.false.,'SYS_ERR',syscol,errflg)
          subinfo = errstr//' finding SYS_ERR column number'
          call wt_ferrmsg(errflg,subinfo)
          IF (errflg.NE.0) THEN
            return
          ENDIF
        ELSEIF (ttype(i).EQ.'QUALITY') THEN
          qqual = .true.
          call ftgcno(iunit,.false.,'QUALITY',qualcol,errflg)
          subinfo = errstr//' finding QUALITY column number'
          call wt_ferrmsg(errflg,subinfo)
          IF (errflg.NE.0) THEN
            return
          ENDIF
        ELSEIF (ttype(i).EQ.'GROUPING') THEN
          qgroup =.true.
          call ftgcno(iunit,.false.,'GROUPING',grpcol,errflg)
          subinfo = errstr//' finding GROUPING column number'
          call wt_ferrmsg(errflg,subinfo)
          IF (errflg.NE.0) THEN
            return
          ENDIF
        ENDIF
      enddo

      IF (dclnam.NE.' ') THEN
c       Overriding the default column name --- assume the same type
c       (COUNTS or RATE) as the mandatory column
        IF (dtype.EQ.1) THEN
          call ftgcno(iunit,.false.,dclnam,ctscol,errflg)
        ELSE
          call ftgcno(iunit,.false.,dclnam,ratecol,errflg)
        ENDIF
        subinfo = errstr//' finding '//dclnam//' column number'
        call wt_ferrmsg(errflg,subinfo)
        IF (errflg.NE.0) THEN
          return
        ENDIF
      ENDIF

      call ftgcno(iunit,.false.,'CHANNEL',chancol,errflg)
      subinfo = errstr//' finding CHANNEL column number'
      call wt_ferrmsg(errflg,subinfo)
      IF (errflg.NE.0) THEN
         return
      ENDIF
      IF (chatter.GE.20) THEN
         subinfo = ' Columns relevant to outfile :'
         call fcecho(subinfo)
         subinfo = ' CHANNEL'
         IF (dtype.EQ.1) THEN
           subinfo = subinfo(1:lenact(subinfo)) //' COUNTS'
         ELSE
           subinfo = subinfo(1:lenact(subinfo)) //' RATE'
         ENDIF
         IF (qerror) THEN
           subinfo = subinfo(1:lenact(subinfo)) //' STAT_ERR'
         ENDIF
         IF (qsys) THEN
           subinfo = subinfo(1:lenact(subinfo)) //' SYS_ERR'
         ENDIF
         call fcecho(subinfo)
         IF (qqual) THEN
           subinfo = ' QUALITY'
         ENDIF
         IF (qgroup) THEN
           subinfo = subinfo(1:lenact(subinfo)) //' GROUPING'
         ENDIF
         IF (qqual.OR.qgroup) THEN
           call fcecho(subinfo)
         ENDIF
      ENDIF

c --- READ PHA COLUMNS/KEYWORDS SUCH AS BACKFILE ---

      call cmp_phakeys(iunit,ttype,ncol,telescop,instrume,
     & detnam,filter,nspecs,exposure,areascal,backscal,corscal,
     &         rmfile,arfile,corfile,backfile,rmfexp,arfexp,
     &         corfexp,backfexp,cmpmode,hduclas2,detchans,
     &         nchan,chancol,chantype,fchan,lchan,
     &         pois,dtype,errflg,chatter)

      IF (fchan.EQ.-99) THEN
c       TLMIN not found --- guess.
        fchan = 1
      END IF
      IF (lchan.EQ.-99) THEN
c       TLMAX not found --- calculate.
        lchan = fchan + nchan - 1
      END IF

c --- READ PHA DATA row by row ---

      do k = 1, nranges
       do i = frows(k), lrows(k)
*      do i=1,nspecs
        status = 0
c       cmp_sq etc. used to compare i with 1 or nspecs to see whether
c       it was the first or last call to it; with the option to run
c       on restricted rows, we need to pass a new parameter, forl
c
c     2003-04-13:  A bug was found such that cmppha fails when
c     there is only a single spectral file (single row).
c     nranges is the number of rows selected
c     It is possible that frows(k)=lrows(k)
c     So i is the actual row number to be extracted
c     Ken Ebisawa
c
        if (i.eq.frows(1)) then
          if (i.eq.lrows(nranges)) then
             forl = -2
          else
             forl = -1
          endif
        else if (i.eq.lrows(nranges)) then
          forl = 1
        else
          forl = 0
        end if
        inull = 0
        call ftgcvj(iunit,chancol,i,1,nchan,inull,channel,
     &            anyflg,status)
        subinfo = errstr//' reading CHANNEL column'
        call wt_ferrmsg(status,subinfo)
        IF (status.NE.0) THEN
          errflg = 5
          return
        ENDIF  
        IF (dtype.EQ.1) THEN
          inull = 0
          status = 0
          felem = 1
          call ftgcvj(iunit,ctscol,i,1,nchan,inull,ipha,
     &            anyflg,status)
          subinfo = errstr//' reading COUNTS column'
          call wt_ferrmsg(status,subinfo)
          IF (status.NE.0) THEN
            errflg = 5
            return
          ENDIF   
         ELSEIF (dtype.EQ.2) THEN
          status = 0
          felem = 1
          enull = 0
          call ftgcve(iunit,ratecol,i,1,nchan,enull,pha,
     &            anyflg,status)
          subinfo = errstr//' reading RATE column'
          call wt_ferrmsg(status,subinfo)
          IF (status.NE.0) THEN
            errflg = 5
            return
          ENDIF
         ENDIF
         IF (qerror) THEN
          status = 0
          felem = 1
          enull = 0
          call ftgcve(iunit,statcol,i,1,nchan,enull,staterr,
     &            anyflg,status)
         ENDIF
         IF (qsys) THEN
          status = 0
          felem = 0
          enull = 0
          call ftgcve(iunit,syscol,i,1,nchan,enull,sysfrc,
     &            anyflg,status)
          subinfo = errstr//' reading SYS_ERR column'
          call wt_ferrmsg(status,subinfo)
          IF (status.NE.0) THEN
            errflg = 5
            return
          ENDIF
         ENDIF
         IF (qqual) THEN
           status = 0
           felem = 0
           inull = 0
           call ftgcvj(iunit,qualcol,i,1,nchan,inull,qualty,
     &             anyflg,status)
           subinfo = errstr//' reading QUALITY column'
           call wt_ferrmsg(status,subinfo)
         ENDIF
         IF (qgroup) THEN
           status = 0
           felem = 0
           inull = 0
           call ftgcvj(iunit,grpcol,i,1,nchan,inull,grping,
     &             anyflg,status)
           subinfo = errstr//' reading GROUPING column'
           call wt_ferrmsg(status,subinfo)
         ENDIF
         IF (cmpmode(1:1).EQ.'S') THEN
           call cmp_sq(i,forl,channel,fchan,nchan,ipha,pha,dtype,
     &       maxchan,qerror,qsys,staterr,sysfrc,
     &       qqual,qualty,sq_qual,qgroup,grping,sq_grping,
     &       exposure(i),sq_chan,sq_pha,sq_ipha,
     &       sq_staterr,sq_sysfrc,totexp,chatter,errflg)
         ELSEIF (cmpmode(1:1).EQ.'E') THEN
cebi    2003-04-14 Ken Ebisawa
cebi    Originally, (i) in the following sentence were (1),
cebi    besides exposure(i).  This caused a problem that
cebi    correct ANCRFILE, RESPFILE etc values in the existent columns
cebi    are not copied to the output file header.
          call cmp_ex_wt(outfile,infile,i,telescop(i),instrume(i),
     &           detnam(i),filter(i),exposure(i),areascal(i),
     &          backscal(i),corscal(i),backfile(i),rmfile(i),
     &          arfile(i),corfile(i),rowexp,dclnam,
     &          dtype,qerror,qsys,qqual,qualty,qgroup,grping,
     &          channel,pha,ipha,staterr,maxchan,sysfrc,
     &          hduclas2,detchans,nchan,chantype,killit,pois,
     &          fchan,taskname,forl,chatter,errflg)
         ENDIF
        enddo
       enddo

       IF (cmpmode(1:1).EQ.'S') THEN
           call cmp_sq_wt(outfile,infile,telescop(1),instrume(1),
     &          detnam(1),filter(1),totexp,areascal(1),
     &          backscal(1),corscal(1),backfile(1),rmfile(1),
     &          arfile(1),corfile(1),rowexp,dclnam,
     &          dtype,qerror,qsys,qqual,sq_qual,qgroup,
     &          sq_grping,sq_chan,sq_pha,sq_ipha,sq_staterr,
     &          maxchan,sq_sysfrc,hduclas2,detchans,nchan,
     &          chantype,killit,pois,fchan,taskname,
     &          chatter,errflg)
       ENDIF
       return
       end
c ------------------------------------------------------------------------
c      END OF CMP_RD_WT
c ------------------------------------------------------------------------

*+
c     --------------------------------------------------------
      subroutine cmp_sq_wt(outfile,infile,telescop,instrume,
     &          detnam,filter,exposure,ascal,
     &          bscal,cscal,backfile,respfile,
     &          arfile,corfile,rowexp,dclnam,
     &          dtype,qerror,qsys,qqual,qualty,qgroup,
     &          grping,chan,pha,ipha,staterr,
     &          maxchan,sysfrc,hduclas2,detchans,nchan,
     &          chantype,killit,pois,fchan,
     &          taskname,chatter,errflg)
c     --------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------------
c This routine opens a new file, writes a null primary array, and an OGIP
c standard FITS PHA extension.
c --- VARIABLES ----------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) outfile,telescop,instrume,detnam,filter
      character*(*) backfile,respfile,arfile,infile
      character*(*) rowexp,dclnam
      character*(*) hduclas2,corfile,chantype,taskname
      real ascal,bscal,cscal,exposure
      real pha(*),staterr(*),sysfrc(*)
      integer maxchan,chan(*),ipha(*),i
      integer chatter,errflg,detchans,nchan
      integer dtype,qualty(*),grping(*)
      integer fchan
      logical killit,pois,qerror,qsys,qqual,qgroup
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------------
c
c Rehana Yusaf (23 May 1995) 1.0.0;
      character(5) version
      parameter (version = '1.0.0')
c
*-
c ------------------------------------------------------------------------
c     INTERNALS
      integer ounit,nk_hist,nk_comm,status
      character(30) errstr,wrnstr,phaversn
      character(70) subinfo,errinfo,hist(4),comm(4)
c
c --- USER INFO ---
c
      IF (chatter.GE.15) THEN
        subinfo = ' ... using CMP_SQ_WT Ver'//version
        call fcecho(subinfo)
      ENDIF
c
c --- OPEN FITS FILE ---
c
      call cgetlun(ounit)
      call opnpa(outfile,chatter,ounit,killit,errflg)
      IF (errflg.NE.0) THEN
       errinfo=errstr//'opening and writing primary to outfile'
       call fcecho(errinfo)
       goto 100
      ENDIF
c
c --- WRITE PHA extension ---
c
      nk_hist = 1
      hist(1) = 'infile: '//infile
      IF (rowexp.NE.'-') THEN
        nk_hist = nk_hist + 1
        hist(nk_hist) = 'Rows: '//rowexp
      ENDIF
      IF (dclnam.NE.' ') THEN
        nk_hist = nk_hist + 1
        hist(nk_hist) = 'Data column: '//dclnam
      ENDIF

      nk_comm = 0
      phaversn = '1.1.0'
      IF (dtype.EQ.1) THEN
        do i=1,nchan
         pha(i) = ipha(i)
        enddo
      ENDIF
      call wtpha1(ounit,chatter,nk_hist,hist,nk_comm,
     &            comm,telescop,instrume,detnam,filter, 
     &            phaversn,hduclas2,fchan,exposure,ascal,
     &            backfile,bscal,corfile,cscal,respfile,
     &            arfile,detchans,chantype,chan,pha,dtype,
     &            qerror,staterr,qsys,sysfrc,qqual,qualty,qgroup,
     &            grping,nchan,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr//' problem writing PHA ext'
        call fcecho(errinfo)
        goto 100
      ENDIF

      status = 0
      call ftpdat(ounit,status)
      status = 0
      call ftpkys(ounit,'CREATOR',taskname,
     &     's/w task which wrote this dataset',status)
      IF (chatter.GE.10) THEN
        errinfo = wrnstr//' writing CREATOR '
        call wt_ferrmsg(status,errinfo)
      ENDIF
      status = 0
      call ftphis(ounit,' Squish mode of CMPPHA',status)
      IF (chatter.GE.10) THEN
        errinfo = wrnstr//' writing CMPPHA'
        call wt_ferrmsg(status,errinfo)
      ENDIF
 100  status = 0
      call ftclos(ounit,status)
      return
      end
c ------------------------------------------------------------------
c     END OF CMP_SQ_WT
c ------------------------------------------------------------------

*+
c     --------------------------------------------------------
      subroutine cmp_ex_wt(outfile,infile,specnum,telescop,
     &          instrume,detnam,filter,exposure,ascal,
     &          bscal,cscal,backfile,respfile,
     &          arfile,corfile,rowexp,dclnam,
     &          dtype,qerror,qsys,qqual,qualty,qgroup,
     &          grping,chan,pha,ipha,staterr,
     &          maxchan,sysfrc,hduclas2,detchans,nchan,
     &          chantype,killit,pois,fchan,
     &          taskname,forl,chatter,errflg)
c     --------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------------
c This routine, if forl is -1, opens a new file, writes a null primary
c array, and an OGIP standard FITS PHA extension. It is used by the expand
c mode of CMPPHA.  If forl is 0 or 1 then a new PHA extension is appended,
c and if it is 1, the file is closed.
c --- VARIABLES ----------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) outfile,telescop,instrume,detnam,filter
      character*(*) backfile,respfile,arfile,infile
      character*(*) hduclas2,corfile,chantype,taskname
      character*(*) rowexp,dclnam
      real ascal,bscal,cscal,exposure
      real pha(*),staterr(*),sysfrc(*)
      integer maxchan,chan(*),ipha(*),i,specnum,forl
      integer chatter,errflg,detchans,nchan
      integer dtype,qualty(*),grping(*)
      integer fchan
      logical killit,pois,qerror,qsys,qqual,qgroup
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------------
c
c Rehana Yusaf (28 Aug 1995) 1.0.0;
c
c Koji Mukai (2000 September 20) 1.1.0;
c     Now uses 'forl' rather than comparision of specnum with 1 and/or nspecs
c     (specnum is now an orphan)
c
c Bryan Irby (2005 June 2) 1.1.1:
c     - SAVE ounit to prevent garbled file unit on Mac OS X.
c     - initialize detchans to prevent garbled value (on Mac OS X) if it
c       isn't found by ftgkyj
c
c Bryan Irby (2009 March 16) 1.1.2:
c     - When wtpha1 returns an error, set forl=1 so that the output file
c       is closed.
c
      character(5) version
      parameter (version = '1.1.2')
c
*-
c ------------------------------------------------------------------------
c     INTERNALS
      integer ounit,nk_hist,nk_comm,status
      character(30) errstr,wrnstr,phaversn
      character(70) subinfo,errinfo,hist(4),comm(4)

      SAVE ounit
c
c --- USER INFO ---
c
      IF (chatter.GE.15) THEN
        subinfo = ' ... using CMP_EX_WT Ver'//version
        call fcecho(subinfo)
      ENDIF
c
c --- OPEN FITS FILE ---
c
      IF (forl.LT.0) THEN
        call cgetlun(ounit)
        call opnpa(outfile,chatter,ounit,killit,errflg)
        IF (errflg.NE.0) THEN
         errinfo=errstr//'opening and writing primary to outfile'
         call fcecho(errinfo)
         goto 100
        ENDIF
      ENDIF
c
c --- WRITE PHA extension ---
c
      nk_hist = 1
      hist(1) = 'infile: '//infile
      IF (rowexp.NE.'-') THEN
        nk_hist = nk_hist + 1
        hist(nk_hist) = 'Rows: '//rowexp
      ENDIF
      IF (dclnam.NE.' ') THEN
        nk_hist = nk_hist + 1
        hist(nk_hist) = 'Data column: '//dclnam
      ENDIF
      nk_comm = 0
      phaversn = '1.1.0'
      IF (dtype.EQ.1) THEN
        do i=1,nchan
         pha(i) = ipha(i)
        enddo
      ENDIF
      call wtpha1(ounit,chatter,nk_hist,hist,nk_comm,
     &            comm,telescop,instrume,detnam,filter, 
     &            phaversn,hduclas2,fchan,exposure,ascal,
     &            backfile,bscal,corfile,cscal,respfile,
     &            arfile,detchans,chantype,chan,pha,dtype,
     &            qerror,staterr,qsys,sysfrc,qqual,qualty,qgroup,
     &            grping,nchan,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr//' problem writing PHA ext'
        call fcecho(errinfo)
        forl=1
        goto 100
      ENDIF

      status = 0
      call ftpdat(ounit,status)
      status = 0
      call ftpkys(ounit,'CREATOR',taskname,
     &     's/w task which wrote this dataset',status)
      IF (chatter.GE.10) THEN
        errinfo = wrnstr//' writing CREATOR '
        call wt_ferrmsg(status,errinfo)
      ENDIF
      status = 0
      call ftphis(ounit,' Expand mode of CMPPHA',status)
      IF (chatter.GE.10) THEN
        errinfo = wrnstr//' writing HISTORY'
        call wt_ferrmsg(status,errinfo)
      ENDIF

 100  status = 0
      IF (forl.EQ.+1.or.forl.EQ.-2) THEN
        call ftclos(ounit,status)
        errinfo = errstr//' closing outfile'
        call wt_ferrmsg(status,errinfo)
      ENDIF
      return
      end
c ------------------------------------------------------------------
c     END OF CMP_EX_WT
c ------------------------------------------------------------------

*+CMP_SQ
c     --------------------------------------------------------
      subroutine cmp_sq(nspec,forl,channel,fchan,nchan,ipha,
     &   pha,dtype,maxchan,qerror,qsys,staterr,sysfrc,
     &   qqual,qualty,sq_qual,qgroup,grping,sq_grping,
     &   exposure,sq_chan,sq_pha,sq_ipha,
     &   sq_staterr,sq_sysfrc,totexp,
     &   chatter,errflg)
c     --------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------
c This subroutine compresses (adds) many spectra together.
c Each spectrum should have the same number of channels.
c -------------------------------------------------------------
c --- VARIABLE DIRECTORY ------------------------------------------
c
      IMPLICIT NONE 
      integer errflg,chatter,maxchan,nspec,fchan,nchan
      integer channel(*), ipha(*),forl,dtype
      real staterr(*),sysfrc(*),pha(*),exposure
      integer sq_chan(*),sq_ipha(*)
      real sq_pha(*),sq_sysfrc(*),sq_staterr(*),totexp
      integer sq_qual(*),sq_grping(*),qualty(*),grping(*)
      logical qsys,qerror,qqual,qgroup
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1995 May 11) 1.0.0; 
c
c Koji Mukai (2000 September 20) 1.1.0;
c     now uses value of 'forl' instead of comparison of nspec and 1/nspecs
c     to see if this is the first call, the last call, or neither
c
      character(5) version
      parameter (version = '1.1.0')
*-
c -----------------------------------------------------------------
c INTERNALS
c
      character(30) errstr,wrnstr
      character(132) subinfo,errinfo
      integer i

c --- USER INFO ---
 
      IF (chatter.GE.40) THEN
        subinfo = ' ... using CMP_SQ Ver '//version
        call fcecho(subinfo)
      ENDIF

      errstr = ' ERROR: CMP_SQ Ver '//version
      wrnstr = ' WARNING: CMP_SQ Ver '//version

c --- SOME ERROR CHECKS ---

c Ensure that each spectrum has the same starting channel

c      IF (nspec.EQ.1) THEN
c        fchan = channel(1)
c      ENDIF
      IF (channel(1).NE.fchan) THEN
          subinfo = errstr
     &//' Spectra contain inconsistent starting channels'
        call fcecho(subinfo)
      ENDIF

c --- POPULATE SQUISH ARRAYS ---

      do i=1,nchan
        IF (forl.LE.-1) THEN
         sq_chan(i) = channel(i)
        ELSEIF (sq_chan(i).NE.channel(i)) THEN
           write(errinfo,'(a,i12,a)')' Row',nspec,
     & ' spectrum contains channel numbers that are inconsistent'
           call fcecho(errinfo)
           errinfo = ' with previous spectra'
           call fcecho(errinfo)
           errflg = 1
           return
        ENDIF
        IF (dtype.EQ.1) THEN
          sq_ipha(i) = sq_ipha(i) + ipha(i)
        ELSEIF (dtype.EQ.2) THEN
          sq_pha(i) = sq_pha(i) + pha(i) * exposure
        ENDIF
c       This must have been a bug, adding exposure inside the 1-nchan loop.
c       There is one statement outside this loop, which is as it should be.
c        totexp = totexp + exposure
c                       commented out, KM, 2000 September 20
        IF (qerror) THEN
           sq_staterr(i) = sq_staterr(i) + staterr(i)*staterr(i)
        ENDIF
        IF (qsys) THEN
           IF (forl.LE.-1) THEN
             sq_sysfrc(i) = sysfrc(i)
           ELSE
             IF (sq_sysfrc(i).NE.sysfrc(i)) THEN
               qsys = .false.
               write(errinfo,'(a,a,i12,a)') wrnstr,' Row ',nspec,
     &' spectrum systematic errors are differant to previous spectra'
               call fcecho(errinfo)
               errinfo = ' Systematic errors are no longer applied'
               call fcecho(errinfo)
             ENDIF
           ENDIF
        ENDIF
        IF (qqual) THEN
           IF (qualty(i).NE.0) THEN
             sq_qual(i) = qualty(i)
           ENDIF       
        ENDIF
        IF (qgroup) THEN
           IF (forl.LE.-1) THEN
             sq_grping(i) = grping(i)
           ELSEIF (grping(i).NE.sq_grping(i)) THEN
             write(errinfo,'(a,a,i12,a)')wrnstr,' Row ',nspec,
     &' spectrum grouping is differant to previous spectra'
             call fcecho(errinfo)
             errinfo = ' Grouping is no longer applied'
             call fcecho(errinfo)
             qgroup = .false.
           ENDIF
        ENDIF 
      enddo
      totexp = totexp + exposure
      IF ((forl.EQ.-2).OR.(forl.EQ.+1)) THEN
        IF (dtype.EQ.2) THEN
         do i=1,nchan
           sq_pha(i) = sq_pha(i)/totexp
         enddo
        ENDIF
        IF (qerror) THEN
          do i=1,nchan
            sq_staterr(i) = SQRT(sq_staterr(i))
          enddo
        ENDIF
      ENDIF 
      return
      end
c -----------------------------------------------------------------
c     END OF CMP_SQ 
c ----------------------------------------------------------------- 


*+CMP_PHAKEYS
c     ---------------------------------------------------------------
      subroutine cmp_phakeys(iunit,ttype,ncol,telescop,instrume,
     &  detnam,filter,nspecs,exposure,areascal,backscal,corscal,
     &  rmfile,arfile,corfile,backfile,rmfexp,arfexp,
     &  corfexp,backfexp,cmpmode,hduclas2,detchans,nchan,
     &  chancol,chantype,fchan,lchan,
     &  pois,dtype,errflg,chatter)
c     ---------------------------------------------------------------
c
c --- VARIABLES ---
c
      IMPLICIT NONE
      character*(*) telescop(*),instrume(*),hduclas2
      character*(*) detnam(*),filter(*),ttype(*),chantype
      integer iunit,errflg,chatter,nspecs,ncol,detchans
      integer chancol,nchan,dtype,fchan,lchan
      character*(*) cmpmode,rmfexp,arfexp,corfexp,backfexp
      character*(*) rmfile(*),arfile(*),corfile(*),backfile(*)
      real exposure(*),areascal(*),backscal(*),corscal(*)
      logical pois
c
c --- MODIFICATION HISTORY ------------------------------------------
c
c                          Rehana Yusaf (May 16 1995) 1.0.0   
      character(5) version
      parameter (version = '1.0.0')
*-
c ------------------------------------------------------------------
c
c INTERNALS

      logical qcol
      integer i,status
      character(28) errstr,comm,wrnstr
      character(80) errinfo,subinfo
      character(8) tlchar
      real edefval,err
      integer datacode,repeat,width
c
c --- USER INFO ---
c
      errstr = ' ERROR: CMP_PHAKEYS Ver '//version
      wrnstr = ' WARNING: CMP_PHAKEYS Ver '//version
      IF (chatter.GE.10) THEN
        subinfo = ' ... using CMP_PHAKEYS Ver '//version
        call fcecho(subinfo)
      ENDIF
c
c --- DETERMINE TELESCOPE VALUE ---
c
      status = 0
      call rdkeycols(iunit,ttype,ncol,nspecs,'TELESCOP','UNKNOWN',
     &                     telescop,qcol,status,chatter)
      IF (cmpmode(1:2).EQ.'SQ') THEN
        IF (qcol) THEN
          do i=2,nspecs
            IF (telescop(i).NE.telescop(1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' from differant TELESCOPES'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errflg =1
              return
            ENDIF
          enddo
        ENDIF
      ENDIF 
c
c --- DETERMINE INSTRUME ---
c
      status = 0
      call rdkeycols(iunit,ttype,ncol,nspecs,'INSTRUME','UNKNOWN',
     &                     instrume,qcol,status,chatter)
      IF (cmpmode(1:2).EQ.'SQ') THEN
        IF (qcol) THEN
          do i=2,nspecs
            IF (instrume(i).NE.instrume(1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' from differant INSTRUMES'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errflg =1
              return
            ENDIF
          enddo
        ENDIF
      ENDIF 
c
c --- DETERMINE DETNAM VALUE ---
c
      status = 0
      call rdkeycols(iunit,ttype,ncol,nspecs,'DETNAM','UNKNOWN',
     &                     detnam,qcol,status,chatter)
      IF (cmpmode(1:2).EQ.'SQ') THEN
        IF (qcol) THEN
          do i=2,nspecs
            IF (detnam(i).NE.detnam(1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' from differant DETECTORS'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errflg =1
              return
            ENDIF
          enddo
        ENDIF
      ENDIF 
c
c --- DETERMINE FILTER VALUE ---
c
      status = 0
      call rdkeycols(iunit,ttype,ncol,nspecs,'FILTER','UNKNOWN',
     &                     filter,qcol,status,chatter)
      IF (cmpmode(1:2).EQ.'SQ') THEN
        IF (qcol) THEN
          do i=2,nspecs
            IF (detnam(i).NE.detnam(1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' with differant FILTERS'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errflg =1
              return
            ENDIF
          enddo
        ENDIF
      ENDIF
c
c --- DETERMINE RESPFILE VALUE ---
c
      IF (rmfexp(1:1).EQ.'%') THEN
        status = 0
        call rdkeycols(iunit,ttype,ncol,nspecs,'RESPFILE','NONE',
     &                     rmfile,qcol,status,chatter)
        IF (cmpmode(1:2).EQ.'SQ') THEN
         IF (qcol) THEN
          do i=2,nspecs
            IF (rmfile(i).NE.rmfile(i-1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' with differant RESPFILE names'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errinfo = ' RESPFILE is set to NONE'
              call fcecho(errinfo)
              rmfile(i) = 'NONE'
            ENDIF
          enddo
          rmfile(1) = rmfile(2)
         ENDIF
        ENDIF
      ELSE
        do i=1,nspecs
          rmfile(i) = rmfexp
        enddo
      ENDIF
c
c --- DETERMINE ARFILE VALUE ---
c
      status = 0
      IF (arfexp(1:1).EQ.'%') THEN
        status = 0
        call rdkeycols(iunit,ttype,ncol,nspecs,'ANCRFILE','NONE',
     &                     arfile,qcol,status,chatter)
        IF (cmpmode(1:2).EQ.'SQ') THEN
         IF (qcol) THEN
          do i=2,nspecs
            IF (arfile(i).NE.arfile(i-1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' with differant ANCRFILE names'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errinfo = 'ANCRFILE is set to NONE'
              arfile(i) = 'NONE'
            ENDIF
          enddo
          arfile(1) = arfile(2)
         ENDIF
        ENDIF
      ELSE
        do i=1,nspecs
          arfile(i) = arfexp
        enddo
      ENDIF
c
c --- DETERMINE CORRFILE VALUE ---
c
      status = 0
      IF (corfexp(1:1).EQ.'%') THEN
        status = 0
        call rdkeycols(iunit,ttype,ncol,nspecs,'CORRFILE','NONE',
     &                     corfile,qcol,status,chatter)
        IF (cmpmode(1:2).EQ.'SQ') THEN
         IF (qcol) THEN
          do i=2,nspecs
            IF (corfile(i).NE.corfile(i-1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' with differant CORRFILE names'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errinfo = 'CORRFILE is set to NONE'
              arfile(i) = 'NONE'
            ENDIF
          enddo
          corfile(1) = corfile(2)
         ENDIF
        ENDIF
      ELSE
        do i=1,nspecs
          corfile(i) = corfexp
        enddo
      ENDIF
c
c --- DETERMINE BACKFILE VALUE ---
c
      status = 0        
      IF (backfexp(1:1).EQ.'%') THEN
        status = 0
        call rdkeycols(iunit,ttype,ncol,nspecs,'BACKFILE','NONE',
     &                     backfile,qcol,status,chatter)
        IF (cmpmode(1:2).EQ.'SQ') THEN
         IF (qcol) THEN
          do i=2,nspecs
            IF (backfile(i).NE.backfile(i-1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' with differant BACKFILE names'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errinfo = 'BACKFILE is set to NONE'
              backfile(i) = 'NONE'
            ENDIF
          enddo
          backfile(1) = backfile(2)
         ENDIF
        ENDIF
      ELSE
        do i=1,nspecs
          backfile(i) = backfexp
        enddo
      ENDIF
c
c --- DETERMINE AREASCAL VALUE ---
c
      status = 0
      edefval = 1.0
      call rdkeycole(iunit,ttype,ncol,nspecs,'AREASCAL',edefval,
     &                     areascal,qcol,status,chatter)
      IF (cmpmode(1:2).EQ.'SQ') THEN
        IF (qcol) THEN
          do i=2,nspecs
            err = ABS(areascal(i) - areascal(i-1))
            IF (err.NE.(0.01)) THEN
              errinfo = wrnstr//' Differant area scaling factors'
              call fcecho(errinfo)
              errinfo = ' The differance is >1%'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errflg =1
              return
            ENDIF
          enddo
        ENDIF
      ENDIF
c
c --- DETERMINE BACKSCAL VALUE ---
c
      status = 0
      edefval = 1.0
      call rdkeycole(iunit,ttype,ncol,nspecs,'BACKSCAL',edefval,
     &                     backscal,qcol,status,chatter)
      IF (cmpmode(1:2).EQ.'SQ') THEN
        IF (qcol) THEN
          do i=2,nspecs
            err = ABS(backscal(i) - backscal(i-1))
            IF (err.NE.(0.01)) THEN
              errinfo = wrnstr//' Differant area scaling factors'
              call fcecho(errinfo)
              errinfo = ' The differance is >1%'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errflg =1
              return
            ENDIF
          enddo
        ENDIF
      ENDIF
c
c --- DETERMINE CORSCAL VALUE ---
c
      status = 0
      edefval = 1.0
      call rdkeycole(iunit,ttype,ncol,nspecs,'CORRSCAL',edefval,
     &                     corscal,qcol,status,chatter)
      IF (cmpmode(1:2).EQ.'SQ') THEN
        IF (qcol) THEN
          do i=2,nspecs
            err = ABS(corscal(i) - corscal(i-1))
            IF (err.NE.(0.01)) THEN
              errinfo = wrnstr//' Differant area scaling factors'
              call fcecho(errinfo)
              errinfo = ' The differance is >1%'
              call fcecho(errinfo)
              errinfo = ' In SQUISH mode this is a problem'
              call fcecho(errinfo)
              errflg =1
              return
            ENDIF
          enddo
        ENDIF
      ENDIF

c DETERMINE EXPOSURE VALUE ---

      status = 0
      edefval = 1.0
      call rdkeycole(iunit,ttype,ncol,nspecs,'EXPOSURE',edefval,
     &                     exposure,qcol,status,chatter)
      IF (cmpmode(1:2).EQ.'SQ') THEN
       IF ((dtype.EQ.2).AND.(status.EQ.1)) THEN
        errinfo = errstr //' EXPOSURE value is needed !'
        call fcecho(errinfo)
        errflg = 1
       ENDIF
      ENDIF

c READ HDUCLAS2 VALUE

      status = 0
      hduclas2 = ' '
      call ftgkys(iunit,'HDUCLAS2',hduclas2,comm,status)
      errinfo = wrnstr//' reading HDUCLAS2'
      IF (chatter.GE.30) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF

c READ CHANTYPE VALUE

      status = 0
      chantype = 'UNKNOWN'
      call ftgkys(iunit,'CHANTYPE',chantype,comm,status)
      IF (chatter.GE.10) THEN
        errinfo = wrnstr//' reading CHANTYPE keyword'
        call wt_ferrmsg(status,errinfo)
      ENDIF

c READ DETCHANS VALUE

      detchans = 0
      call ftgkyj(iunit,'DETCHANS',detchans,comm,status)
      errinfo = errstr//' reading DETCHANS'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF


C Read Nchan from the tform keyword instead. 
      status = 0
      call ftgtcl(iunit,chancol,datacode,repeat,width,status)
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF 
      IF (status.EQ.0) THEN
        nchan = repeat
        IF (detchans.EQ.0) THEN
          detchans = nchan
        END IF
      ELSE
        nchan = detchans
      ENDIF

c READ NCHAN (TLMIN2 value and TLMAX2 value)

      IF (chancol.LE.9) THEN
       write(tlchar,100) chancol
      ELSE
       write(tlchar,150) chancol
      ENDIF
      status = 0
      fchan = -99
      call ftgkyj(iunit,tlchar,fchan,comm,status)
      errinfo = errstr//' reading TLMIN'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (chancol.LE.9) THEN
       write(tlchar,200) chancol
      ELSE
       write(tlchar,250) chancol
      ENDIF
      lchan = -99
      call ftgkyj(iunit,tlchar,lchan,comm,status)
      errinfo = errstr//' reading TLMAX'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
C      IF (status.EQ.0) THEN
C       nchan = lchan - fchan + 1
C      ELSE
C        nchan = detchans
C      ENDIF

c POIS ...

      status = 0
      pois = .false.
      call ftgkyl(iunit,'POISSERR',pois,comm,status)
      IF (chatter.GE.15) THEN
        errinfo = wrnstr//' reading POISSERR keyword'
        call wt_ferrmsg(status,errinfo)
        errinfo = 'POISERR assumed to be false'
        call fcecho(errinfo)
      ENDIF

 100  FORMAT('TLMIN',I1)
 150  FORMAT('TLMIN',I2)
 200  FORMAT('TLMAX',I1)
 250  FORMAT('TLMAX',I2)
       
      return
      end
c ------------------------------------------------------------------
c     END OF CMP_PHAKEYS 
c ------------------------------------------------------------------


