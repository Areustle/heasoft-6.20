
*+BCMPPHA
c      -----------------
       subroutine bcmppha
c      -----------------
c --- DESCRIPTION -----------------------------------------------------
c BCMPPHA reads a BATSE typeII pha file to make Source & Background
c files in type I pha format.
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE
      character(160) infile, outfile
      character(80) context,desc,backfexp,rmfexp,arfexp,corfexp
      character(40) termdesc
      character(8) task
      character(6) filemode,list_lc
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
c CMP_GP_BATSE     : Gets parameters
c 
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1995 May 8) 1.0.0;
c Peter D Wilson (1998 June 29) 1.0.1:
c       . Eliminated INQUIRE call on infile
c Ken Watanabe (1998 August) Converted for BATSE Trigger Data
c
c C. Pan ( March 2002) added agraphics display capability.  
c        Plot channel number  vs. summed spectra counts to help
c        to determine a source region or  a background  region 
c        of the BATSE trigger data.

      character(5) version
      parameter (version = '1.0.0')
      character(40) taskname
      integer nb1,nb2,nb3,nb4,ns1,ns2
c     ,chan_start,chan_stop,detchans
      COMMON/task/taskname
      taskname='BCMPPHA'//version
*-
c ---------------------------------------------------------------------
c
c --- GET PARAMETERS ---
c
      context = 'fatal error'
      termdesc =' BCMPPHA Ver '//version//' terminated !'
      errflg = 0
      call cmp_gp_batse(infile,outfile,filemode,backfexp,rmfexp,arfexp,
     &      corfexp,killit,errflg,chatter,nb1,nb2,nb3,nb4,ns1,ns2,
     &      list_lc)
      IF (errflg.NE.0) THEN
        call fcerr(context)
        call fcecho(termdesc)
        return
      ENDIF
c
c --- USER INFO ---
c
      IF (chatter.GE.1) THEN
        desc = ' Main BCMPPHA Ver '//version
        call fcecho(desc)
      ENDIF
      task = 'BCMPPHA'
c
c --- READ INFILE, APPLY FILEMODE AND WRITE OUTFILE ---
c

      call cmp_dma_rd_wt_batse(infile,outfile,filemode,
     &              backfexp,rmfexp,arfexp,corfexp,taskname,
     &              chatter,killit,errflg,nb1,nb2,nb3,nb4,ns1,ns2,
     &              list_lc)
      IF (chatter.GE.1) THEN
        desc = ' Completed BCMPPHA Ver '//version
        call fcecho(desc)
      ENDIF

      return
      end

c ----------------------------------------------------------------------
c     END OF MAIN BCMPPHA
c ----------------------------------------------------------------------
    
*+CMP_GP_BATSE
c     --------------------------------------------------------
      subroutine cmp_gp_batse(infile,outfile,filemode,backfexp,rmfexp,
     &           arfexp,corfexp,killit,errflg,chatter,nb1,nb2,nb3,nb4,
     &           ns1,ns2,list_lc)
c     --------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile,filemode,list_lc
c     ,plot_lc*3
      character*(*) backfexp,rmfexp,arfexp,corfexp
c      character(160) filename
      character(160) ill_files(5)
      integer errflg,chatter,n_ill
      character(26) errstr, wrnstr,data_dir*10
      integer status,nb1,nb2,nb3,nb4,ns1,ns2
c      integer maxchan
c      integer*4 jj , i , k , slen , out , nlen     
      character(70) desc
      logical val_cmp,valfil,killit     
c      logical ext
c      integer flen, fcstln
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c infile     char   : input file name
c outfile    char   : Output filename
c chatter    int    : Chattiness flag, >20 verbose
c errflg     int    : Error flag
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
c Ken Watanabe (1998 August) Converted for BATSE Trigger Data
c
      character(5) version 
      parameter (version = '1.0.0')
*-
c ---------------------------------------------------------------------
c
      errstr = ' ERROR : BCMPPHA Ver '//version//':'
      wrnstr = ' WARNING : BCMPPHA Ver '//version//':'

c GET INFILE

      status = 0
      call uclgst('data_dir',data_dir,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting data_dir parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(data_dir)
      IF (data_dir.EQ.' ') THEN
        data_dir='./'
      ENDIF
      call crmvblk(data_dir)
    
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

      infile = data_dir//'/'//infile
      call crmvblk(infile)

C PDW 6/29/98: Leave it to FTOPEN to determine
       status = 0
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

      outfile = data_dir//'/'//outfile
      call crmvblk(outfile)    
      
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

c GET FILEMODE

      status = 0
      call uclgst('filemode',filemode,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting filemode parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF           

c CHECK FILEMODE VALIDITY

      call ftupch(filemode)
      call crmvlbk(filemode)
      val_cmp = .false.

      IF (filemode(1:1).EQ.'B') THEN
        val_cmp = .true.

      ELSEIF (filemode(1:1).EQ.'S') THEN
        val_cmp = .true.
      ENDIF
      IF (.NOT.val_cmp) THEN
        desc = errstr//' Invalid FILEMODE !' 
        call fcecho(desc)

        desc = ' VALID FILEMODES : Background or Source'    
        call fcecho(desc)
        errflg = 1
        return
      ENDIF 


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c GET LIST_LC

      status = 0
      call uclgst('list_lc',list_lc,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting list_lc parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF           
      call ftupch(list_lc)
      call crmvlbk(list_lc)



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
c     END OF CMP_GP_BATSE
c ---------------------------------------------------------------------


*+CMP_DMA_RD_WT_BATSE
c    -------------------------------------------------------------
      subroutine cmp_dma_rd_wt_batse(infile,outfile,filemode,
     &           backfexp,rmfexp,arfexp,corfexp,taskname,
     &           chatter,killit,errflg,nb1,nb2,nb3,nb4,ns1,ns2,
     &           list_lc)
c    -------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c This subroutine reads the input TYPE II phafile row by row and
c applies the filemode. Subsequently the output file is written.
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,outfile,filemode,taskname,list_lc
      character*(*) backfexp,rmfexp,arfexp,corfexp
      integer chatter,errflg,nb1,nb2,nb3,nb4,ns1,ns2
      integer chan_start, chan_stop
      logical killit
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c Rehana Yusaf (1995 May 9) 1.0.0;
c
c Ken Watanabe (1998 August) Converted for BATSE Trigger Data
c
      character(6) version
      parameter (version = '1.0.0')
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
      errstr = ' ERROR : CMP_DMA_RD_WT_BATSE '//version 
      wrnstr = ' WARNING : CMP_DMA_RD_WT_BATSE '//version
      IF (chatter.GE.10) THEN
        subinfo = ' using CMP_DMA_RD_WT_BATSE Ver '//version
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
      maxchan = 256

      IF (chatter.GE.30) THEN
        write(subinfo,'(a,i12)') ' Maximum array size:',maxchan
        call rmvexsp(subinfo,subinfo2)
        call fcecho(subinfo2)
      ENDIF
c
c --- SET DMA ARRAYS ---
c
        p_channel = 0
        p_ipha = 0 
        p_qualty = 0
        p_grping = 0
        p_sq_ipha = 0 
        p_sq_grping = 0
        p_sq_qual = 0
        p_sq_chan = 0
        p_staterr = 0
        p_sysfrc = 0
        p_pha = 0
        p_sq_pha = 0 
        p_sq_staterr = 0
        p_sq_sysfrc = 0
        p_areascal = 0
        p_backscal = 0
        p_corscal = 0
        p_exposure = 0

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
        
c ---   READ/SOURCE or BACKGROUND DATA ---
 

        call cmp_rd_wt_batse(iunit,infile,outfile,filemode,
     &     backfexp,rmfexp,arfexp,corfexp,taskname,
     &     MEMI(p_channel),MEMI(p_ipha),MEMI(p_grping),
     &     MEMI(p_qualty),MEMI(p_sq_chan),MEMI(p_sq_ipha),
     &     MEMI(p_sq_qual),MEMI(p_sq_grping),MEMR(p_pha),
     &     MEMR(p_staterr),MEMR(p_sysfrc),MEMR(p_sq_pha),
     &     MEMR(p_sq_staterr),MEMR(p_sq_sysfrc),MEMR(p_areascal),
     &     MEMR(p_backscal),MEMR(p_corscal),MEMR(p_exposure),
     &     maxchan,chatter,killit,errflg,nb1,nb2,nb3,nb4,ns1,ns2,
     &     list_lc,chan_start,chan_stop)
  
        return
        end
c -------------------------------------------------------------------------
c     END OF CMP_DMA_RD_WT_BATSE
c -------------------------------------------------------------------------


*+CMP_RD_WT_BATSE
c    -------------------------------------------------------------
      subroutine cmp_rd_wt_batse(iunit,infile,outfile,filemode,
     &        backfexp,rmfexp,arfexp,corfexp,taskname,
     &        channel,ipha,grping,
     &        qualty,sb_chan,sb_ipha,
     &        sb_qual,sb_grping,pha,
     &        staterr,sysfrc,sb_pha,
     &        sb_staterr,sb_sysfrc,areascal,backscal,corscal,
     &        exposure,maxchan,chatter,killit,errflg,
     &        nb1,nb2,nb3,nb4,ns1,ns2,list_lc,chan_start,chan_stop)
c    -------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c This subroutine reads the input TYPE II phafile row by row and
c applies the filemode. Subsequently the output file is written.
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,outfile,filemode,taskname,list_lc
      character*(*) backfexp,rmfexp,arfexp,corfexp
      integer chatter,errflg,maxchan,nb1,nb2,nb3,nb4,ns1,ns2
      integer chan_start, chan_stop
      logical killit
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c Rehana Yusaf (1995 May 9) 1.0.0;
c
c Ken Watanabe (1998 August) Converted for BATSE Trigger Data
c
      character(6) version
      parameter (version = '1.0.0')
*-
c ----------------------------------------------------------------
c INTERNALS
      character(33) errstr,wrnstr,comm
      character(80) subinfo,subinfo2
      integer iunit,lenact
      integer ncol,maxcol
      parameter (maxcol = 20)
      character(20) ttype(maxcol),tunit(maxcol)
      character(5) tform(maxcol)
      logical anyflg,pois
      integer i,k,ivar,nspecs,status,felem,inull,nchan
      integer fchan
      integer chancol,ratecol,ctscol,statcol,syscol,qualcol,spencol
      integer grpcol,dtype
      integer channel(*),ipha(*),spec_num(12800)
      real staterr(*),sysfrc(*),pha(*)
      real pha_temp(12800,256),err_temp(12800,256) 
      real enull
      logical qerror,qsys,qqual,qgroup
      integer sb_grping(*),sb_qual(*),sb_chan(*)
      integer qualty(*),grping(*),sb_ipha(*)
      real sb_pha(*),sb_staterr(*),sb_sysfrc(*)
      character(16) telescop(256),instrume(256),detnam(256)
      character(16) filter(256)
      character(80) rmfile(256),arfile(256),corfile(256)
      character(80) backfile(256)
      real areascal(*),backscal(*),corscal(*)
      real exposure(*),totexp
      integer detchans
      character(16) hduclas2,chantype
c
c --- USER INFO ---
c
      errstr = ' ERROR : CMP_RD_WT_BATSE Ver '//version
      wrnstr = ' WARNING : CMP_RD_WT_BATSE '//version
      IF (chatter.GE.10) THEN
        subinfo = ' using CMP_RD_WT_BATSE Ver '//version
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
c --- READ NSPEC TTYPE, NCOL and DETERMINE/LOCATE COLUMNS ---
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


c DETERMINE WHICH COLUMNS ARE PRESENT AND SET LOGICALS ACCORDINGLY

      dtype = 0
      qerror = .false.
      qsys = .false.
      qqual = .false.
      qgroup = .false.
      totexp = 0
      do i=1,ncol
        IF (ttype(i).EQ.'SPEC_NUM') THEN
          dtype = 0
          call ftgcno(iunit,.false.,'SPEC_NUM',spencol,errflg)
          subinfo = errstr//' finding SPEC_NUM column number'
          call wt_ferrmsg(errflg,subinfo)
          IF (errflg.NE.0) THEN
            return
          ENDIF
        ELSEIF (ttype(i).EQ.'COUNTS') THEN
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

      call cmp_phakeys_batse(iunit,ttype,ncol,telescop,instrume,
     & detnam,filter,nspecs,exposure,areascal,backscal,corscal,
     &         rmfile,arfile,corfile,backfile,rmfexp,arfexp,
     &         corfexp,backfexp,filemode,hduclas2,detchans,
     &         nchan,chancol,chantype,
     &         pois,dtype,errflg,chatter,chan_start,chan_stop)
        
c --- READ SPEC_NUM COL ---
      status =0
      call ftgcvj(iunit,spencol,1,1,nspecs,inull,spec_num,
     &            anyflg,status) 
      subinfo = errstr//' reading SPEC_NUM column'
        status = 0
        call wt_ferrmsg(status,subinfo)
        IF (status.NE.0) THEN
          errflg = 5
          return
        ENDIF  

c --- READ PHA DATA row by row --- 
      
      do i=1,nspecs      
       status = 0
        inull = 0
        call ftgcvj(iunit,chancol,i,chan_start,nchan,inull,channel,
     &            anyflg,status)     
        subinfo = errstr//' reading CHANNEL column'
        status = 0
        call wt_ferrmsg(status,subinfo)
        IF (status.NE.0) THEN
          errflg = 5
          return
        ENDIF  
       
        IF (dtype.EQ.1) THEN
                  inull = 0
          status = 0
          felem = 1
          call ftgcvj(iunit,ctscol,i,chan_start,nchan,inull,ipha,
     &            anyflg,status)
          subinfo = errstr//' reading COUNTS column'
          status = 0
          call wt_ferrmsg(status,subinfo)
          IF (status.NE.0) THEN
            errflg = 5
            return
          ENDIF   
         ELSEIF (dtype.EQ.2) THEN
          status = 0
          felem = 1
          enull = 0
     
          call ftgcve(iunit,ratecol,i,chan_start,nchan,enull,pha,
     &            anyflg,status)
          
          do k=1,nchan
             pha_temp(i,k)=pha(k)
          enddo 

          subinfo = errstr//' reading RATE column'
          status = 0
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
          call ftgcve(iunit,statcol,i,chan_start,nchan,enull,staterr,
     &            anyflg,status)

           do k=1,nchan
             err_temp(i,k)=staterr(k)
          enddo
  
         ENDIF
         IF (qsys) THEN
          status = 0
          felem = 0
          call ftgcve(iunit,syscol,i,chan_start,nchan,enull,sysfrc,
     &            anyflg,status)
          subinfo = errstr//' reading SYS_ERR column'
          status = 0
          call wt_ferrmsg(status,subinfo)
          IF (status.NE.0) THEN
            errflg = 5
            return
          ENDIF
         ENDIF
         IF (qqual) THEN
           status = 0
           felem = 0
           call ftgcvj(iunit,qualcol,i,chan_start,nchan,inull,qualty,
     &             anyflg,status)
           subinfo = errstr//' reading QUALITY column'
           call wt_ferrmsg(status,subinfo)
         ENDIF
         IF (qgroup) THEN
           status = 0
           felem = 0
           call ftgcvj(iunit,grpcol,i,chan_start,nchan,inull,grping,
     &             anyflg,status)
           subinfo = errstr//' reading GROUPING column'
           call wt_ferrmsg(status,subinfo)
         ENDIF
         
         IF ((filemode(1:1).EQ.'S') .or.(filemode(1:1).EQ.'B')) THEN                

           call cmp_sb(i,nspecs,channel,fchan,nchan,ipha,pha,dtype, 
     &       maxchan,qerror,qsys,staterr,sysfrc,
     &       qqual,qualty,sb_qual,qgroup,grping,sb_grping,
     &       exposure(i),sb_chan,sb_pha,sb_ipha,
     &       sb_staterr,sb_sysfrc,totexp,chatter,errflg)
         ENDIF
       enddo
   
       IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN
           call src_bkg_batse(pha_temp,sb_pha,err_temp,sb_staterr,
     &           nspecs,nchan,filemode,nb1,nb2,nb3,nb4,ns1,ns2,list_lc)

           call cmp_sb_wt(outfile,infile,telescop(1),instrume(1),   
     &          detnam(1),filter(1),
     &          totexp,areascal(1),backscal(1),corscal(1),
     &          backfile(1),rmfile(1),arfile(1),corfile(1),
     &          dtype,qerror,qsys,qqual,sb_qual,qgroup,
     &          sb_grping,sb_chan,sb_pha,sb_ipha,sb_staterr,
     &          maxchan,sb_sysfrc,hduclas2,detchans,nchan,
     &          chantype,killit,pois,taskname,chatter,errflg,filemode)     
       ENDIF 
       return
       end
c ------------------------------------------------------------------------
c      END OF CMP_RD_WT_BATSE
c ------------------------------------------------------------------------

*+SRC_BKG_BATSE
c     -------------------------------------------------------------
      subroutine src_bkg_batse(pha_temp,sb_pha,err_temp,sb_staterr,
     &           nspecs,nchan,filemode,nb1,nb2,nb3,nb4,ns1,ns2,list_lc)

      IMPLICIT NONE

c Ken Watanabe (1998 August) 
c Makes a Source file and a Background file to be used with the XSPEC package
c for the BATSE burst data analysis. 
c
C      
C      The character variable below has been added 
C      so that the code can use the fitsio routine 
C      fcecho. 
C      
C      Jeff Silvis 
C      RSTX 
C      21  August  98
 
      character(250) FCECHO_STR 

      integer nspecs,nchan,i,j,nb1,nb2,nb3,nb4,ns1,ns2,inb,ins,status
      real integ_bkg,integ_src,integ_bkg_err,integ_src_err
      real pha_sum(12800),err_sum(12800)
      real pha_temp(12800,256),err_temp(12800,256) 
      real sb_pha(*),sb_staterr(*),temp_lc
      character(6) filemode,list_lc,plot_lc
      integer errflg
      character(26) errstr
      character(70) desc
      errstr = ' ERROR : BCMPPHA'

      IF (list_lc(1:2).EQ.'YE') THEN
         do i=1,nspecs
            temp_lc = 0.0  
            do j=1,nchan
               temp_lc = temp_lc +  pha_temp(i,j) 
            enddo 
            WRITE(FCECHO_STR,'(A,I12,A,G15.7)') 
     & 'Channel:',i,' Total Spectrum:',temp_lc
            call FCECHO(FCECHO_STR) 
         enddo 
       ENDIF         
       
c  get the sum of spectra for all channal
        do i = 1,nspecs
           err_sum(i) = 0
           do j=1,nchan
             err_sum(i)=err_sum(i) + err_temp(i,j)
          enddo
        enddo
     
        do i = 1,nspecs
           pha_sum(i) = 0
           do j=1,nchan
             pha_sum(i)=pha_sum(i) + pha_temp(i,j)
          enddo
        enddo
     
c  GET BACKGROUND PARAMETERS 
       IF (filemode(1:1).EQ.'B') THEN
          status = 0
          call get_bs_chan(nb1,nb2,nb3,nb4,pha_sum,nspecs,4,status,'b')
          IF (status.NE.0) THEN
          desc = errstr//' .. getting Background parameter !'
          call fcecho(desc)
          errflg = 1
          return
          ENDIF
          
c  GET SOURCE PARAMETERS  
       ELSEIF (filemode(1:1).EQ.'S') THEN
         status = 0
         call uclgst('plot_lc',plot_lc,status)
         IF (status.NE.0) THEN
           desc = errstr//' .. getting plot_lc parameter !'
           call fcecho(desc)
           errflg = 1
           return
         ENDIF           
c         call ftupch(plot_lc)
c         call crmvlbk(plot_lc)
         IF (plot_lc .eq. 'no' .or. plot_lc .eq. 'No' .or.
     &      plot_lc .eq. 'NO') THEN
            status = 0
            call uclgsi('ns1',ns1,status)
            IF (status.NE.0) THEN
              desc = errstr//' .. getting Source parameter !'
              call fcecho(desc)
              errflg = 1
              return
            ENDIF
            status = 0
            call uclgsi('ns2',ns2,status)
            IF (status.NE.0) THEN
            desc = errstr//' .. getting Source parameter !'
            call fcecho(desc)
            errflg = 1
            return
            ENDIF
         ELSE  
            status =0
            call get_bs_chan(ns1,ns2,0,0,pha_sum,nspecs,2,status,'s')
            IF (status.NE.0) THEN
              desc = errstr//' .. getting Source parameter !'
              call fcecho(desc)
              errflg = 1
              return
             ENDIF         
         ENDIF 
       ENDIF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      do j=1,nchan
         integ_bkg = 0.0
         integ_src = 0.0 
         integ_bkg_err = 0.0
         integ_src_err = 0.0 

         IF (filemode(1:1).EQ.'B') THEN
           inb=0
           do i=nb1,nb2
              integ_bkg = integ_bkg + pha_temp(i,j) 
              integ_bkg_err = integ_bkg_err + err_temp(i,j)**2
              inb= inb + 1               
           enddo

           do i=nb3,nb4
              integ_bkg = integ_bkg + pha_temp(i,j) 
              integ_bkg_err = integ_bkg_err + err_temp(i,j)**2
              inb= inb + 1               
           enddo
              sb_pha(j) = integ_bkg/inb 
              sb_staterr(j)=SQRT(integ_bkg_err)/inb             
 
          ELSEIF (filemode(1:1).EQ.'S') THEN                 
           ins=0
           do i=ns1,ns2 
             integ_src = integ_src + pha_temp(i,j)
             integ_src_err = integ_src_err + err_temp(i,j)**2
             ins = ins + 1   
           enddo
            sb_pha(j)=integ_src/ins
            sb_staterr(j)=SQRT(integ_src_err)/ins      
          ENDIF
      enddo 
      return
      end
c ------------------------------------------------------------------------
c      END OF SRC_BKG_BATSE
c ------------------------------------------------------------------------

*+CMP_SB_WT
c     --------------------------------------------------------
      subroutine cmp_sb_wt(outfile,infile,telescop,instrume,
     &          detnam,filter,
     &          exposure,ascal,bscal,cscal,
     &          backfile,respfile,arfile,corfile,
     &          dtype,qerror,qsys,qqual,qualty,qgroup,
     &          grping,chan,pha,ipha,staterr,
     &          maxchan,sysfrc,hduclas2,detchans,nchan,
     &          chantype,killit,pois,taskname,chatter,errflg,filemode)
c     --------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------------
c This routine opens a new file, writes a null primary array, and an OGIP
c standard FITS PHA extension.
c --- VARIABLES ----------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) outfile,telescop,instrume,detnam,filter
      character*(*) backfile,respfile,arfile,infile
      character*(*) hduclas2,corfile,chantype,taskname
      real ascal,bscal,cscal,exposure
      real pha(*),staterr(*),sysfrc(*)
      integer maxchan,chan(*),ipha(*),i
      integer chatter,errflg,detchans,nchan
      integer dtype,qualty(*),grping(*)
      logical killit,pois,qerror,qsys,qqual,qgroup
      character(6)  filemode
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
        subinfo = ' ... using CMP_SB_WT Ver'//version
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
      hist(1) = 'infile :'//infile
      nk_comm = 0
      phaversn = '1.1.0'
      IF (dtype.EQ.1) THEN
        do i=1,nchan
         pha(i) = ipha(i)
        enddo
      ENDIF
    
      errflg =0
      call wtpha1p(ounit,chatter,nk_hist,hist,nk_comm,
     &            comm,telescop,instrume,detnam,filter, 
     &            phaversn,hduclas2,chan(1),exposure,ascal,
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
      IF (filemode(1:1).EQ.'S') THEN
         call ftphis(ounit,' SOURCE mode of BCMPPHA',status)
      ELSEIF (filemode(1:1).EQ.'B') THEN
         call ftphis(ounit,' BACKGROUND mode of BCMPPHA',status)
      ENDIF
      IF (chatter.GE.10) THEN
        errinfo = wrnstr//' writing BCMPPHA'
        call wt_ferrmsg(status,errinfo)
      ENDIF
 100  status = 0
      call ftclos(ounit,status) 
      return
      end
c ------------------------------------------------------------------
c     END OF CMP_SB_WT
c ------------------------------------------------------------------
*+CMP_SB
c     --------------------------------------------------------
      subroutine cmp_sb(nspec,nspecs,channel,fchan,nchan,ipha,
     &   pha,dtype,maxchan,qerror,qsys,staterr,sysfrc,
     &   qqual,qualty,sb_qual,qgroup,grping,sb_grping,
     &   exposure,sb_chan,sb_pha,sb_ipha,
     &   sb_staterr,sb_sysfrc,totexp,
     &   chatter,errflg)
c     --------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------
c This subroutine
c Each spectrum should have the same number of channels.
c -------------------------------------------------------------
c --- VARIABLE DIRECTORY ------------------------------------------
c
      IMPLICIT NONE 
      integer errflg,chatter,maxchan,nspec,fchan,nchan
      integer channel(*), ipha(*),nspecs,dtype
      real staterr(*),sysfrc(*),pha(*),exposure
      integer sb_chan(*),sb_ipha(*)
      real sb_pha(*),sb_sysfrc(*),sb_staterr(*),totexp
      integer sb_qual(*),sb_grping(*),qualty(*),grping(*)
      logical qsys,qerror,qqual,qgroup
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1995 May 11) 1.0.0; 
c
      character(5) version
      parameter (version = '1.0.0')
*-
c -----------------------------------------------------------------
c INTERNALS
c
      character(30) errstr,wrnstr
      character(256) subinfo,errinfo
      integer i

c --- USER INFO ---
 
      IF (chatter.GE.40) THEN
        subinfo = ' ... using CMP_SRC Ver '//version
        call fcecho(subinfo)
      ENDIF

      errstr = ' ERROR: CMP_SRC Ver '//version
      wrnstr = ' WARNING: CMP_SRC Ver '//version

c --- SOME ERROR CHECKS ---

c Ensure that each spectrum has the same starting channel

      IF (nspec.EQ.1) THEN
        fchan = channel(1)
      ENDIF
      IF (channel(1).NE.fchan) THEN
          subinfo = errstr
     &//' Spectra contain inconsistant starting channels'
        call fcecho(subinfo)
      ENDIF

c --- POPULATE SOURCE ARRAYS ---

      do i=1,nchan
        IF (nspec.EQ.1) THEN
         sb_chan(i) = channel(i)
        ELSEIF (sb_chan(i).NE.channel(i)) THEN
           write(errinfo,'(a,i12,a)')' Row',nspec,
     & ' spectrum contains channel numbers that are inconsistant'
           call fcecho(errinfo)
           errinfo = ' with previous spectra'
           call fcecho(errinfo)
           errflg = 1
           return
        ENDIF
c
        IF (dtype.EQ.1) THEN
          sb_ipha(i) = sb_ipha(i) + ipha(i)
        ELSEIF (dtype.EQ.2) THEN
c          sb_pha(i) = sb_pha(i) + pha(i) * exposure
        ENDIF
        totexp = totexp + exposure
        IF (qerror) THEN
c           sb_staterr(i) = sb_staterr(i) + staterr(i)
        ENDIF
c
        IF (qsys) THEN
           IF (nspec.EQ.1) THEN
             sb_sysfrc(i) = sysfrc(i)
           ELSE
             IF (sb_sysfrc(i).NE.sysfrc(i)) THEN
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
             sb_qual(i) = qualty(i)
           ENDIF       
        ENDIF
        IF (qgroup) THEN
           IF (nspec.EQ.1) THEN
             sb_grping(i) = grping(i)
           ELSEIF (grping(i).NE.sb_grping(i)) THEN
             write(errinfo,'(a,a,i12,a)')wrnstr,' Row ',nspec,
     &' spectrum grouping is differant to previous spectra'
             call fcecho(errinfo)
             errinfo = ' Grouping is no longer applied'
             call fcecho(errinfo)
             qgroup = .false.
           ENDIF
        ENDIF 
      enddo
c
      return
      end
c -----------------------------------------------------------------
cc     END OF CMP_SQ 
c     END OF CMP_SB 
c ----------------------------------------------------------------- 
*+CMP_PHAKEYS_BATSE
c     ---------------------------------------------------------------
      subroutine cmp_phakeys_batse(iunit,ttype,ncol,telescop,instrume,
     &  detnam,filter,nspecs,exposure,areascal,backscal,corscal,
     &  rmfile,arfile,corfile,backfile,rmfexp,arfexp,
     &  corfexp,backfexp,filemode,hduclas2,detchans,nchan,
     &  chancol,chantype,
     &  pois,dtype,errflg,chatter,chan_start,chan_stop)
c     ---------------------------------------------------------------
c
c --- VARIABLES ---
c
      IMPLICIT NONE
      character*(*) telescop(*),instrume(*),hduclas2
      character*(*) detnam(*),filter(*),ttype(*),chantype
      integer iunit,errflg,chatter,nspecs,ncol,detchans
      integer chancol,nchan,dtype,chan_start,chan_stop
      character*(*) filemode,rmfexp,arfexp,corfexp,backfexp,chanmax*4
      character*(*) rmfile(*),arfile(*),corfile(*),backfile(*)
      real exposure(*),areascal(*),backscal(*),corscal(*)
      logical pois
c
c --- MODIFICATION HISTORY ------------------------------------------
c
c                          Rehana Yusaf (May 16 1995) 1.0.0   
c Ken Watanabe (1998 August) Converted for BATSE Trigger Data
c
      character(5) version
      parameter (version = '1.0.0')
*-
c ------------------------------------------------------------------
c
c INTERNALS

      logical qcol
      integer i,status,fchan,lchan, nspecs_
      character(28) errstr,comm,wrnstr
      character(80) errinfo,subinfo,desc
      character(8) tlchar
      real edefval,err

c
c --- USER INFO ---
c
      errstr = ' ERROR: CMP_PHAKEYS_BATSE Ver '//version
      wrnstr = ' WARNING: CMP_PHAKEYS_BATSE Ver '//version
      IF (chatter.GE.10) THEN
        subinfo = ' ... using CMP_PHAKEYS_BATSE Ver '//version
        call fcecho(subinfo)
      ENDIF
c
c --- DETERMINE TELESCOPE VALUE ---
c
      status = 0
      nspecs_ = nspecs
      if (nspecs .gt. 300) nspecs_=1
c
      call rdkeycols(iunit,ttype,ncol,nspecs_,'TELESCOP','UNKNOWN',
     &                     telescop,qcol,status,chatter)

      IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN
        IF (qcol) THEN
          do i=2,nspecs
            IF (telescop(i).NE.telescop(1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' from differant TELESCOPES'
              call fcecho(errinfo)
              errinfo = ' this is a problem'
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
      call rdkeycols(iunit,ttype,ncol,nspecs_,'INSTRUME','UNKNOWN',
     &                     instrume,qcol,status,chatter)

      IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN
        IF (qcol) THEN
          do i=2,nspecs
            IF (instrume(i).NE.instrume(1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' from differant INSTRUMES'
              call fcecho(errinfo)
              errinfo = 'this is a problem' 
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
      call rdkeycols(iunit,ttype,ncol,nspecs_,'DETNAM','UNKNOWN',
     &                     detnam,qcol,status,chatter)

      IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN
        IF (qcol) THEN
          do i=2,nspecs
            IF (detnam(i).NE.detnam(1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' from differant DETECTORS'
              call fcecho(errinfo)
              errinfo = 'this is a problem' 
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
      call rdkeycols(iunit,ttype,ncol,nspecs_,'FILTER','UNKNOWN',
     &                     filter,qcol,status,chatter)

      IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN 
        IF (qcol) THEN
          do i=2,nspecs
            IF (detnam(i).NE.detnam(1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' with differant FILTERS'
              call fcecho(errinfo)
              errinfo = 'this is a problem'  
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
        call rdkeycols(iunit,ttype,ncol,nspecs_,'RESPFILE','NONE',
     &                     rmfile,qcol,status,chatter)

        IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN 
         IF (qcol) THEN
          do i=2,nspecs
            IF (rmfile(i).NE.rmfile(i-1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' with differant RESPFILE names'
              call fcecho(errinfo)
              errinfo = 'this is a problem'    
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
        call rdkeycols(iunit,ttype,ncol,nspecs_,'ANCRFILE','NONE',
     &                     arfile,qcol,status,chatter)

         IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN  
         IF (qcol) THEN
          do i=2,nspecs
            IF (arfile(i).NE.arfile(i-1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' with differant ANCRFILE names'
              call fcecho(errinfo)
              errinfo = 'this is a problem' 
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
        call rdkeycols(iunit,ttype,ncol,nspecs_,'CORRFILE','NONE',
     &                     corfile,qcol,status,chatter)
        IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN
         IF (qcol) THEN
          do i=2,nspecs
            IF (corfile(i).NE.corfile(i-1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' with differant CORRFILE names'
              call fcecho(errinfo)
              errinfo = 'this is a problem'    
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
        call rdkeycols(iunit,ttype,ncol,nspecs_,'BACKFILE','NONE',
     &                     backfile,qcol,status,chatter)

        IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN  
         IF (qcol) THEN
          do i=2,nspecs
            IF (backfile(i).NE.backfile(i-1)) THEN
              errinfo = errstr//' File contains spectra'
     &//' with differant BACKFILE names'
              call fcecho(errinfo)
              errinfo = 'this is a problem'   
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
      call rdkeycole(iunit,ttype,ncol,nspecs_,'AREASCAL',edefval,
     &                     areascal,qcol,status,chatter)

      IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN
        IF (qcol) THEN
          do i=2,nspecs
            err = ABS(areascal(i) - areascal(i-1))
            IF (err.NE.(0.01)) THEN
              errinfo = wrnstr//' Differant area scaling factors'
              call fcecho(errinfo)
              errinfo = ' The differance is >1%'
              call fcecho(errinfo)
              errinfo = 'this is a problem' 
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
      call rdkeycole(iunit,ttype,ncol,nspecs_,'BACKSCAL',edefval,
     &                     backscal,qcol,status,chatter)

      IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN
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
      call rdkeycole(iunit,ttype,ncol,nspecs_,'CORRSCAL',edefval,
     &                     corscal,qcol,status,chatter)

      IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN 
        IF (qcol) THEN
          do i=2,nspecs
            err = ABS(corscal(i) - corscal(i-1))
            IF (err.NE.(0.01)) THEN
              errinfo = wrnstr//' Differant area scaling factors'
              call fcecho(errinfo)
              errinfo = ' The differance is >1%'
              call fcecho(errinfo)
              errinfo = 'this is a problem'   
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
      call rdkeycole(iunit,ttype,ncol,nspecs_,'EXPOSURE',edefval,
     &                     exposure,qcol,status,chatter)

      IF ((filemode(1:1).EQ.'S') .or. (filemode(1:1).EQ.'B')) THEN
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
      status = 0

      call ftgkyj(iunit,'DETCHANS',detchans,comm,status)

      errinfo = errstr//' reading DETCHANS'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF

c READ NCHAN (TLMIN2 value and TLMAX2 value)

      IF (chancol.LE.9) THEN
       write(tlchar,100) chancol
      ELSE
       write(tlchar,150) chancol
      ENDIF

      status = 0
      call uclgsi('chan_start',chan_start,status)
      IF (status.NE.0) THEN
      desc = errstr//' .. getting chan_start !'
      call fcecho(desc)
      errflg = 1
      return
      ENDIF
       
      status = 0
      chan_stop = detchans
      call uclgsi('chan_stop',chan_stop,status)
   
      IF ((status.NE.0).OR.(chan_stop.gt.detchans)) THEN
       chan_stop = detchans
       write(chanmax,*) detchans
       desc = 'Chan_stop is set to the maximum channel'//chanmax//'!'
       call fcecho(desc)
      ENDIF 
  
      status = 0
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
      status = 0
      call ftgkyj(iunit,tlchar,lchan,comm,status)
      errinfo = errstr//' reading TLMAX'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (status.EQ.0) THEN
        nchan = lchan - fchan + 1
      ELSE
c        nchan = detchans
        nchan = chan_stop - chan_start + 1
      ENDIF
 
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
c     END OF CMP_PHAKEYS_BATSE 
c ------------------------------------------------------------------













