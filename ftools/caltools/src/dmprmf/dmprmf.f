
*+DMPRMF
      subroutine dmprmf
c     ------------------
c
c --- DESCRIPTION ----------------------------------------------------
c This is the main program for reading and displaying response data
c from a FITS file. 
c --------------------------------------------------------------------
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      character(80) infile,ebdfile,outfile,imagefile
      logical dispebd,disprmf,disphd,dispmat
      integer chatter
      logical cont,page,dmpimg,primary,killit
c
c --- INTERNALS ------------------------------------------------------
c
      character(40) subinfo,termdesc,context
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c infile     char   : Input file name (user defined)
c ebdfile    char   : File containing Ebounds extension 
c dispebd    logical: 'Y' if user wants to display EBOUNDS data
c disprmf    logical: 'Y' if user wants to display RMF data
c disphd     logical: 'Y' if user wants to display full header
c dispmat    logical: 'Y' if user wants to display full matrix
c outfile    char   : Ascii outfile name,default no output file
c imagefile  char   : Output filename for response stored as image
c chatter    int    : Chatter flag (<5 quiet,>20 noisy)
c cont       logical: True if no error encountered
c
c --- CALLED ROUTINES ---
c
c subroutine RMF_GP   : Gets user defined parameters
c subroutine RMF_DISP : Displays Response data, EBOUNDS and
c                       RMF extensions
c subroutine FCECHO   : (FTOOLS) Writes to screen, standalone write
c
c --- LINKING AND COMPILATION ----------------------------------------
c
c Linking with FTOOLS,FITSIO and CALTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c Rehana Yusaf (March 1993)1.0.0
c Rehana Yusaf (June 21) 1.0.1;    Minor changes made for UNIX compiler,
c                                  replacing ((NOT)) with (.NOT.) !
c Rehana Yusaf (august 2) 1.0.2;   Change to subroutine RMF_RMF, and MAN_KEY
c                                  FCERR used for fatal error
c Rehana Yusaf (Nov 1) 1.0.3;      Change to subroutine RMF_GP
c Rehana Yusaf (Nov 15) 1.0.4;     Change to RMF_GP
c Rehana Yusaf (1993 Dec 6) 1.0.5; Update RMF_RMF - matvals
c                                  array size is increased. Also minor
c                                  modification to scaler routine
c Rehana Yusaf (1994 12 April) 1.0.6;
c                                  . Add ebdfile parameter, to allow for
c                                    Ebounds extension to be in a 
c                                    seperate file
c                                  . Add page parameter, the default is
c                                    true, output can be paged, pagelength
c                                    is 23.
c                                  . response can be written as image
c                                    in prmary array of FITS file
c                                
c Rehana Yusaf (1994 12 Sept) 1.0.7; . add clobber parameter (killit)
c                                    . update ck_file call to add killit   
c Rehana Yusaf (1995 Mar 16) 1.0.8;  . Bugfix, imagefile option works correctly
c                                      now, killit passed to rmf_img
c Banashree M Seifert (1996 Oct) 1.1.0:
c          . modification in one subroutine par_cmd for LINUX
C
C toliver (1998 01 Jun) 1.1.1;  Modify subroutine rmf_rmf to display E_MAX
C                               and E_MIN values above 99.999999 per thread
C                               ideas/dmprmf_980424
C Peter D Wilson (1998 July 01) 1.1.2:
C          . Updated for new FCPARS behavior
C kaa (1998 Nov 30) 1.1.3:
C          . Removed reference to EFFAREA keyword since it is not part of
C            the standard
C ngan  (1999 Oct 1) 1.1.4:
C          . Corrected the wrong size of the full rmf matrix.  
C Ziqin Pan (2004 January 6) 1.1.5:
C          . Fix a bug when input file not existing 
C B.Irby (2005 August 31) 1.1.6:
C          . Removed uninitialized variable "defval" (sent to uclpst as garbage)
C            and replaced with the (presumably) intended value of "outfile".
C
c ---------------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.6')
      character(40) taskname
ccc      COMMON/task/taskname
      taskname = 'DMPRMF '//version
*-
c --------------------------------------------------------------------
c 
c --- GET PARAMETERS ---
c
      context = 'fatal error'
      termdesc =' DMPRMF TERMINATED !'
      cont = .true.
      call rmf_gp(infile,ebdfile,dispebd,disprmf,disphd,
     &            dispmat,dmpimg,outfile,imagefile,primary,
     &            page,killit,cont,chatter)
      IF (.NOT.cont) THEN
        call fcerr(context)
        call fcecho(termdesc)
        return
      ENDIF
      If (chatter.GE.1) THEN
        subinfo = ' Main DMPRMF Ver '//version
        call fcecho(subinfo)
      ENDIF
c
c --- DISPLAY RESPONSE DATA ---
c
      cont =.true.
      call rmf_disp(infile,ebdfile,dispebd,disprmf,disphd,
     &              dispmat,primary,outfile,page,killit,
     &              cont,chatter)
      IF (.NOT.cont) THEN
        call fcerr(context)
        call fcecho(termdesc)
      ENDIF                
c
c --- DUMP RESPONSE AS IMAGE IF IMAGE FILENAME ENTERED ---
c
      IF (dmpimg) THEN
        call rmf_dmpimg(infile,imagefile,taskname,
     &                      killit,cont,chatter)
      ENDIF 
      IF (.NOT.cont) THEN
        call fcerr(context)
        call fcecho(termdesc)
      ENDIF
      IF (chatter.GE.1) THEN
        subinfo = ' Program DMPRMF Ver '//version//' COMPLETED'
        call fcecho(subinfo)
      ENDIF
      return
      end
c
c --- END OF MAIN ---
c

*+RMF_GP
      subroutine rmf_gp(infile,ebdfile,dispebd,disprmf,disphd,
     &                  dispmat,dmpimg,outfile,imagefile,primary,
     &                  page,killit,cont,chatter)
c     ---------------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c
c This routine obtains the user defined paramters by linking with HOST
c
c --- VARIABLES -------------------------------------------------------
C
      IMPLICIT NONE
      character*(*) infile,outfile,ebdfile,imagefile
      logical dispebd,disprmf,disphd,dispmat
      integer chatter
      logical cont,page,dmpimg,primary,killit
c
c --- INTERNALS -------------------------------------------------------
c
      character(80) ill_files(5),rmfilename,ebdfilename
      character(26) errstr
      character(70) errinfo
      integer errflag, n_ill,extnum
      logical ext,valfil
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c infile   char     : FITS response filename (user defined)
c outfile  char     : Output filename (Ascii)
c dispebd  char     : 'Y' if user wants to display EBOUNDS data
c disprmf  char     : 'Y' if user wants to display RMF data
c dispmat  char     : 'Y' if user wants to display full matrix
c disphd   char     : 'Y' if user wants to display full header
c chatter  int      : Chattines flag, (<5quiet,>20 noisy)
c errflag  int      : Error flag
c errstr   char     : Routine error string
c errinfo  char     : Information about particular error
c cont     logical  : True if no error is encountered
c ext      logical  : True if infile exists
c
c --- CALLED ROUTINES -------------------------------------------------
c
c subroutine UCLGST : (HOST) Obtains string parameter
c subroutine UCLGSI : (HOST) Obtains integer paramter
c subroutine FCECHO : (FTOOLS) Writes to screen
c subroutine CK_FILE: (FTOOLS) Check outfile validity
c
c --- COMPILATION/LINKING ---------------------------------------------
c
c Link with HOST and FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1993 MARCH)
c Rehana Yusaf (1993 August) 1.0.1: CK_FILE added, to allow overwrite
c                            of existing file, except infile
c Rehana Yusaf (1993 Nov 1) 1.0.2; If no outfile entered do not call
c			     CK_FILE
c Rehana Yusaf (1993 Nov 15) 1.0.3; If no input file entered then 
c 			     return to main
c Rehana Yusaf (1994 April 12) 1.0.4;
c                            . Add ebdfile parameter
c                            . Use fcpars, to allow extension number
c                              to be specified as part of filename
c                            . Add page parameter
c                            . Add imagefile parameter
c                            . Add primary parameter
c                            
c Rehana Yusaf (1994 Sept 13) 1.0.5; read clobber (killit)
c Peter Wilson (1998 July 01) 1.0.6: Drop INQUIRE. Use ftrtnm instead of fcpars
      character(5) version
      parameter (version = '1.0.6')
*-
c ---------------------------------------------------------------------
c
c --- GET FILENAME ---
c
      errstr = ' ERROR : RMF_GP Ver '//version
      errflag = 0
      call uclgst('infile',infile,errflag)
      IF (errflag.NE.0) THEN
        errinfo = errstr//' ... getting infile parameter'
        call fcecho(errinfo)
        cont = .false.
        return
      ENDIF  
      call crmvlbk(infile)
      IF (infile.EQ.'   ') THEN
        errinfo = errstr//' input file not entered !'
        call fcecho(errinfo)
        cont = .false.
        return
      ENDIF
C PDW 7/1/98: Drop INQUIRE.  Replace fcpars with ftrtnm
C      call fcpars(infile,rmfilename,extnum,errflag)
      call ftrtnm( infile, rmfilename, errflag )
C      INQUIRE(FILE=rmfilename,EXIST=ext)
C      IF (.NOT.ext) THEN
C        errinfo = errstr//' input file does not exist !'
C        call fcecho(errinfo)
C        cont = .false.
C        return
C      ENDIF
      ill_files(1) = rmfilename
      n_ill = 1
c
c --- READ EBDFILENAME ---
c
      call uclgst('ebdfile',ebdfile,errflag)
      IF (errflag.NE.0) THEN
        errinfo = errstr//' ... getting ebdfile parameter'
        call fcecho(errinfo)
        cont = .false.
        return
      ENDIF 
      call crmvlbk(ebdfile)
C PDW 7/1/98: Drop INQUIRE.  Replace fcpars with ftrtnm
C      call fcpars(ebdfile,ebdfilename,extnum,errflag)
      call ftrtnm( ebdfile, ebdfilename, errflag )
      IF ((ebdfilename.EQ.'%').OR.(ebdfile.EQ.' ')) THEN
        ebdfile = rmfilename
      ELSE
C        INQUIRE(FILE=ebdfilename,EXIST=ext)
C        IF (.NOT.ext) THEN
C          errinfo = errstr//' input file does not exist !'
C          call fcecho(errinfo)
C          cont = .false.
C          return
C        ENDIF
        n_ill = n_ill + 1
        ill_files(n_ill) = ebdfilename 
      ENDIF
c
c --- GET CHATTER FLAG ---
c
      errflag = 0
      call uclgsi('chatter',chatter,errflag)
      IF (errflag.NE.0) THEN
        errinfo = errstr//' ... getting chatter parameter'
        call fcecho(errinfo)
        cont = .false.
        return
      ENDIF
c
c --- GET DISPEBD ---
c
      errflag = 0
      call uclgsb('dispebd',dispebd,errflag)
      IF (errflag.NE.0) THEN
         errinfo = errstr//' ... getting dispebd parameter'
         call fcecho(errinfo)
         cont =.false.
         return
      ENDIF
c
c --- GET DISPRMF ---
c
      errflag = 0
      call uclgsb('disprmf',disprmf,errflag)
      IF (errflag.NE.0) THEN
         errinfo = errstr//' ... getting disprmf parameter'
         call fcecho(errinfo)  
         cont = .false.
         return
      ENDIF
c
c --- GET DISPHD ---
c
      errflag = 0
      call uclgsb('disphd',disphd,errflag)
      IF (errflag.NE.0) THEN
         errinfo = errstr//' ... getting disphd paramter'
         call fcecho(errinfo)
         cont = .false.
         return
      ENDIF
c
c --- GET DISPMAT INFO IF DISPRMF = 'Y' ---
c
      IF (disprmf) THEN
        errflag = 0
        call uclgsb('dispmat',dispmat,errflag)
        IF (errflag.NE.0) THEN
           errinfo = errstr//' ... getting dispmat parameter'
           call fcecho(errinfo)
           cont = .false.
           return
        ENDIF
      ENDIF
c
c --- GET CLOBBER ---
c
      killit = .false.
      errflag = 0
      call uclgsb('clobber',killit,errflag)
      IF (errflag.NE.0) THEN
         errinfo = errstr//' ... getting killit parameter'
         call fcecho(errinfo)
         cont = .false.
         return
      ENDIF
c
c --- GET IMAGEFILE PARAMETER ---
c
      errflag = 0
      call uclgst('imagefile',imagefile,errflag)
      IF (errflag.NE.0) THEN
         errinfo = errstr//' ... getting imagefile parameter'
         call fcecho(errinfo)
         cont = .false.
         return
      ENDIF
      call crmvlbk(imagefile)
      IF (imagefile.EQ.' ') THEN
        dmpimg = .false.
      ELSE
        dmpimg = .true.
        call ck_file(imagefile,ill_files,n_ill,valfil,killit,chatter)
        IF (.NOT.valfil) THEN
          errinfo = errstr//' invalid imagefilename !'
          call fcecho(errinfo)
          cont = .false.
          return
        ENDIF
        n_ill = n_ill + 1
        ill_files(n_ill) = imagefile
      ENDIF

c
c --- GET OUTPUT FILENAME ---
c
      errflag = 0
      call uclgst('outfile',outfile,errflag)
      IF (errflag.NE.0) THEN
         errinfo = errstr//' ... getting output filename parameter'
         call fcecho(errinfo)
         cont = .false.
         return
      ENDIF
c
c --- CHECK OUTFILE---
c
      call crmvlbk(outfile)
      valfil = .true.
      IF (outfile.NE.'  ') THEN
        call ck_file(outfile,ill_files,n_ill,valfil,killit,chatter)
        IF (dmpimg) THEN
          IF (outfile.EQ.imagefile) THEN
            errinfo = errstr//'outfile name same as imagefile'
            call fcecho(errinfo)
            errinfo = ' differant names should be used'
            call fcecho(errinfo)
            cont = .false.
            return
          ENDIF
        ENDIF 
        IF (.NOT.valfil) THEN
          errinfo = errstr//' invalid outfile name !'
          call fcecho(errinfo)
          cont = .false.
          return
        ENDIF
        errflag = 0
        call uclpst('outfile',outfile,errflag)
        IF (errflag.NE.0) THEN
         errinfo = errstr//'... writing default output filename '
         call fcecho(errinfo)
         cont = .false.
         return
        ENDIF
        n_ill = n_ill + 1
        ill_files(n_ill) = outfile
      ENDIF
c
c--- GET PAGE PARAMETER ---
c
      call uclgsb('page',page,errflag)
      IF (errflag.NE.0) THEN
         errinfo = errstr//' ... getting page parameter'
         call fcecho(errinfo)
         cont = .false.
         return
      ENDIF
      call uclpsb('page',.true.,errflag)
      IF (errflag.NE.0) THEN
         errinfo = errstr//' ... putting default page parameter'
         call fcecho(errinfo)
         cont = .false.
         return
      ENDIF
      call uclgsb('primary',primary,errflag)
      IF (errflag.NE.0) THEN
         errinfo = errstr//'... getting primary parameter'
         call fcecho(errinfo)
         cont = .false.
         return
      ENDIF     
      return
      end
c
c --- END OF RMF_GP ---------------------------------------------------
c

*+RMF_DISP
      subroutine rmf_disp(infile,ebdfile,dispebd,disprmf,disphd,
     &                    dispmat,primary,outfile,page,killit,
     &                    cont,chatter)
c     -----------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c
c This routine displays the response data, it displays ... 
c EBOUNDS extension header, and EBOUNDS data columns, and
c RMF extension header, and RMF data columns.
c
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,outfile,ebdfile
      logical dispebd,disprmf,disphd,dispmat,killit
      integer chatter
      logical cont,page,primary
c
c --- INTERNAL VARIABLES ---
c
      integer status,iunit,ierr,ounit,rw,block,ln_cnt
      character(30) subinfo
      character(28) errstr,wrnstr
      character(70) errinfo 
      logical wtfile


c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c Arguments ...
c
c infile   char     : FITS RMF filename (user defined)
c outfile  char     : Output filename, ascii
c dispebd  char     : 'Y' if user wants to display EBOUNDS data
c disprmf  char     : 'Y' if user wants to display RMF data
c disphd   char     : 'Y' if user wants to display full header
c dispmat  char     : 'Y' if user wants to display full matrix
c chatter  int      : Chattiness flag (>20 noisy)
c cont     logical  : True if no error encoutered
c
c Internals ...
c
c subinfo  char     : Routine info for user
c errstr   char     : Error string for this routine
c wrnstr   char     : warning string for this routine
c errinfo  char     : Error info for a particular error
c status   int      : Error flag
c rw       int      : Read/Write mode for FITS file, O=readonly
c block    int      : FITSIO record blocking factor
c iunit    int      : Fortran i/o unit
c wtfile   logical  : True if writing to output file
c
c --- CALLED ROUTINES --------------------------------------------------
c
c subroutine FCECHO     : (FTOOLS) Writes string to screen
c subroutine FTOPEN     : (FITSIO) Opens FITS file
c subroutine WT_FERRMSG : (CALLIB) Writes FITSIO and routine error messages
c subroutine RMF_PRHD   : Prints primary header
c subroutine RMF_EBD    : Displays EBOUNDS extension 
c subroutine RMF_RMF    : Displays RMF extension
c
c --- LINKING/COMPILATION ----------------------------------------------
c
c Linking with FTOOLS and FITSIO
c
c --- AUTHORS/MODIFICATION ---------------------------------------------
c
c Rehana Yusaf (1993 March)
c Rehana Yusaf (1993 May 3)
c Rehana Yusaf (1994 April 12) ; Add ebdfile parameter and page parameter
c                                Add primary - only display primary
c                                header if primary='yes'
c Rehana Yusaf (1994 Sept 13) ; pass in killit
      character(5) version
      parameter (version = '1.0.2')
*-
c ----------------------------------------------------------------------
c
c --- USER INFO ---
c
      IF (chatter.GE.10) THEN
        subinfo = ' ... using RMF_DISP Ver '//version
        call fcecho(subinfo)
      ENDIF
      errstr = ' ERROR : RMF_DISP Ver '// version
      wrnstr = ' WARNING : RMF_DISP Ver '//version
c
c --- DISPLAY PRIMARY HEADER IF USER CHOOSES TO DISPLAY IT ---
c
      IF (primary) THEN
        status = 0 
        rw = 0
        block = 2880
        call cgetlun(iunit)
        call ftopen(iunit,infile,rw,block,status)
        errinfo = errstr//' ... opening RMF file '
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          cont = .false.
          return
        ENDIF
      ENDIF
c
c --- OPEN OUTPUT FILE ---
c
      IF (outfile.EQ.'  ') THEN
        wtfile = .false.
      ELSE
        wtfile = .true.
        ierr=0
        call cgetlun(ounit)
        call opasci(ounit,outfile,2,80,killit,chatter,ierr)
c        open (UNIT=ounit,FILE=outfile,STATUS='new',IOSTAT=ierr)
        IF (ierr.NE.0) THEN
          errinfo = errstr//' ... opening output file'
          call fcecho(errinfo)
          cont = .false.
          return
        ENDIF
      ENDIF
c
c --- READ AND DISPLAY PRIMARY HEADER ---
c
      ln_cnt = 0
      IF (primary) THEN
        call rmf_prhd(iunit,ounit,wtfile,page,ln_cnt,chatter)          
        status = 0
        call ftclos(iunit,status)
        errinfo = errstr//' ... closing RMF file'
        call wt_ferrmsg(status,errinfo)
      ENDIF
c
c --- READING AND DISPLAYING EBOUNDS EXTENSION IF DISPEBD = 'Y'---
c
      cont = .true.
      IF (dispebd) THEN
        call rmf_ebd(ebdfile,disphd,ounit,wtfile,
     &               page,ln_cnt,cont,chatter)
        IF (.not.cont) THEN
          errinfo=errstr//' EBOUNDS display not completed !'
          call fcecho(errinfo)
          cont =.true.
        ENDIF
      ENDIF
c
c --- READING AND DISPLAYING RMF EXTENSION IF DISPRMF = 'Y'---
c
      cont = .true.
      IF (disprmf) THEN
        call rmf_rmf(infile,disphd,dispmat,
     &               ounit,wtfile,page,ln_cnt,
     &               cont,chatter)
        IF (.NOT.cont) THEN
          errinfo=errstr//' RMF display not completed !'
          call fcecho(errinfo)
          cont =.true.
        ENDIF   
      ENDIF
c
c --- CLOSE OUTPUT FILE
c
      close(ounit,IOSTAT=ierr)
      return
      end
c
c --- END OF RMF_DISP ----------------------------------------------
c

      
*+RMF_PRHD
      subroutine rmf_prhd(iunit,ounit,wtfile,page,ln_cnt,chatter)
c     ----------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c     This routine reads and writes the primary header array. It
c     displays keywords to the screen ...
c
c     SIMPLE    : Does the FITS file conform to all FITS standards
c     BITPIX    : Number of bits per pixel
c     NAXIS     : Number of data axes
c     EXTEND    : T, True if there may be extensions following PRIMARY
c     DATE      : Date
c     CONTENT   : File contents
c     RMFVERSN  : OGIP FITS version
c     ORIGIN    : Place of File creation
c     
c --- VARIABLES ----------------------------------------------------
c 
      IMPLICIT NONE
      integer iunit,chatter,ounit,ln_cnt
      logical wtfile,page
c
c --- INTERNALS ----------------------------------------------------
c
      integer status,key_no,ierr
      logical endhead
      character(80) card,header,line,subinfo,desc
      character(30) errstr
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c Arguments ...
c
c iunit      int    : Fortran input unit
c ounit      int    : Fortran output unit
c chatter    int    : chattiness flag, > 20 verbose
c wtfile     logical: True if writing to output file
c
c Internals ...
c
c status     int    : FITSIO error flag
c key_no     int    : Record/Card No to be read from file
c card       char   : Current card
c header     char   : Used for output purposes
c line       char   : Used for underline output purposes
c endhead    logical: endhead = .true. when end of primary header
c subinfo    char   : Routine information
c errstr     char   : Routine Error string
c
c --- CALLED ROUTINES ----------------------------------------------
c
c subroutine FTGREC    : (FITSIO) Routine to read FITS record 
c subroutine WT_FERRMSG: (CALTOOLS) Routine to write routine and
c                         FITSIO error messages
c subroutine FCECHO    : (FTOOLS), standalone write to screen
c
c --- LINKING AND COMPILATION --------------------------------------
c
c FTOOLS, FITSIO and CALTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1993 March)
c Rehana Yusaf (1994 April 13) 1.0.1; Add page and ln_cnt arguments
c                                     USE pg_fcecho to page output
      character(5) version
      parameter (version = '1.0.1')
*-
c ------------------------------------------------------------------
c
c  --- USER INFO ---
c
      IF (chatter.GE.10) THEN
        subinfo = ' ... using RMF_PRHD Ver '//version
        call fcecho(subinfo)
      ENDIF
      errstr = ' ERROR : RMF_PRHD Ver '//version
c
c --- READ AND DISPLAY PRIMARY HEADER ---
c
      endhead = .false.
      key_no = 1
      line =   ' -------------- '
      call pg_fcecho(line,page,ln_cnt,status)
      IF (status.NE.0) THEN
        return
      ENDIF
      IF (wtfile) THEN
        write(ounit,100,IOSTAT=ierr) line
      ENDIF
      header = ' PRIMARY HEADER '
      call pg_fcecho(header,page,ln_cnt,status)
      IF (status.NE.0) THEN
        return
      ENDIF
      call pg_fcecho(line,page,ln_cnt,status)
      IF (status.NE.0) THEN
        return
      ENDIF
      IF (wtfile) THEN
        write(ounit,100,IOSTAT=ierr) header
        write(ounit,100,IOSTAT=ierr) line
      ENDIF
      header = ' '
      call pg_fcecho(header,page,ln_cnt,status)
      IF (status.NE.0) THEN
        return
      ENDIF
      IF (wtfile) THEN
        write(ounit,100,IOSTAT=ierr) header
      ENDIF
      do WHILE (.NOT.endhead)
        status = 0
        call ftgrec(iunit,key_no,card,status)
        call wt_ferrmsg(status,errstr)
        IF (status.EQ.0) THEN
          call pg_fcecho(card,page,ln_cnt,status)
          IF (status.NE.0) THEN
            return
          ENDIF
          IF (wtfile) THEN
            write(ounit,100,IOSTAT=ierr) card
          ENDIF
        ELSE
          desc = errstr//' invalid record encountered in header'
          call pg_fcecho(desc,page,ln_cnt,status)
          IF (wtfile) THEN
            write(ounit,100,IOSTAT=ierr) desc
          ENDIF
          status = 0
        ENDIF
        IF (card(1:3).EQ.'END') THEN
           endhead = .true.
        ENDIF
        key_no = key_no + 1
      enddo                
 100  format(A80)
      return
      end     
c
c --- END OF SUBROUTINE RMF_PRHD ---------------------------------
c

*+RMF_EBD
      subroutine rmf_ebd(ebdfile,disphd,ounit,
     &                   wtfile,page,ln_cnt,cont,chatter)
c     ---------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c
c       This routine reads,and displays the EBOUNDS header and
c     data. If disphd (user defined) is 'Y' then the full header
c     is displayed, otherwise only the standard OGIP keywords are
c     displayed.
c
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      integer iunit,chatter,ounit,ln_cnt
      logical disphd
      character*(*) ebdfile 
      logical cont,wtfile,page
c
c --- INTERNALS --------------------------------------------------
c
      character(26) errstr,wrnstr
      character(70) errinfo,subinfo
      character(70) ebdchan
      character(80) card,header,data,blank,line
      character(40) comm
      integer status,key_no,chancol,emincol,emaxcol
      integer npha,chanval,ran1,ran2
      integer felem,nelems,inull,row,perr,ierr
      real eminval,emaxval,enull
      logical endhead,anyflg

c VARIABLES FOR MVEXT

      integer nsearch,ninstr
      parameter (nsearch = 50)
      integer next(nsearch)
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9)
      character(8) extname

c
c --- VARIABLE DIRECTORY -----------------------------------------
c
c Arguments ...
c
c iunit      int    : Fortran i/o unit
c disphd     char   : 'Y' if user wants to display full header
c outfile    char   : User defined output filename, default to screen
c chatter    int    : Chattines flag (>20 verbose)
c ounit      int    : Fortran i/o unit
c 
c Internals ...
c
c errstr     char   : Routine error string
c wrnstr     char   : Routine warning string
c errinfo    char   : Additional error information
c blank      char   : Used for blank line output
c line       char   : Used for underline output
c card       char   : FITS record
c header     char   : Used for output
c data       char   : Used for outputing data
c comm       char   : FITSIO comments
c extname    char   : FITS extension name
c chnum      char   : Char used for outputing Channel numbers
c cmin       char   : Char used for outputing E_MIN values
c cmax       char   : Char used for outputing E_MAX values
c status     int    : FITSIO error flag
c nhdu       int    : No. of header unit
c htype      int    : Header unit type
c key_no     int    : Record number
c chancol    int    : "CHANNEL" Column No.
c emincol    int    : "E_MIN" Column No.
c emaxcol    int    : "E_MAX" Column No.
c npha       int    : Counter for number of pha values
c snum       int    : Start number for reading TTYPEx ,x starts at snum
c ncols      int    : Number of data columns in extension
c chanval    int    : Channel value
c eminval    real   : E_MIN value
c emaxval    real   : E_MAX value
c felem      int    : First pixel of vector, for FITSIO read
c nelems     int    : Number of data items to read
c inull      int    : Null value
c enull      real   : real null value
c row        int    : Row number
c findext    logical: TRUE if desired extension found
c endhead    logical: TRUE if pointer at end of header
c anyflg     logical: TRUE if any returned data values are undefined
c foundcol   logical: TRUE if desired column found
c
c --- CALLED ROUTINES --------------------------------------------
c
c subroutine FTMAHD   : (FITSIO) Move to header unit
c subroutine FTGKYx   : (FITSIO) To read keyword of type x,
c                       for example s (string)
c subroutine FTGREC   : (FITSIO) Reads FITS record
c subroutine FTGCVx   : (FITSIO) Reads data column of type x
c subroutine WT_FERRMSG: (CALTOOLS) Writes FITSIO and routine
c                         error message if appropriate
c subroutine FCECHO   : (FTOOLS) standalone screen write
c subroutine MAN_KEY  : Writes mandatory keywords, if necessary
c subroutine MVEXT    : (CALLIB) Open infile and move to desired 
c                       extension
c 
c --- COMPILATION AND LINKING ------------------------------------
c
c Link with FTOOLS and FITSIO and CALTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c Rehana Yusaf (1993 March 19)
c Rehana Yusaf (1994 April 13) 1.0.1; Add page and ln_cnt arguments 
c                                     . Add mvext
c                                     . Use pg_fcecho to page output
c                                     . format for E_MIN E_MAX display
c                                       changed from f10.7 to f12.7
c                                       to allow more dynamic range
c Rehana Yusaf (1994 May 24) 1.0.2;   mvext has an additional passsed
c                                     parameter
      character(5) version
      parameter (version = '1.0.2')
*-
c ---------------------------------------------------------------- 
c
c --- INITIALISATION ---
c
      errstr = ' ERROR : RMF_EBD Ver '//version
      wrnstr = ' WARNING : RMF_EBD Ver '//version
      blank = '  '
c
c --- USER INFO ---
c
      IF (chatter.GE.10) THEN
         subinfo = ' ... using RMF_EBD Ver '//version
         call fcecho(subinfo)
      ENDIF
c
c ---   MOVE TO EBOUNDS EXTENSION IN FILE ---
c
      status = 0
      ninstr = 2
      instr(1) ='RESPONSE'
      instr(2) = 'EBOUNDS'
      extname = 'EBOUNDS'
      call mvext(0,ebdfile,iunit,ninstr,instr,nsearch,next,outhdu,
     &           extnames,outver,extname,status,chatter)
      IF (status.NE.0) THEN
        cont = .false.
        return
      ENDIF
c
c --- READ HEADER AND DISPLAY FULL HEADER IF DISPHD = 'Y'---
c
      IF (disphd) THEN
        line =   ' ----------------------------- '
        IF (wtfile) THEN
          write(ounit,400,IOSTAT=ierr) blank
          write(ounit,400,IOSTAT=ierr) line
        ENDIF
        call pg_fcecho(blank,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 100 
        ENDIF
        call pg_fcecho(line,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 100 
        ENDIF
        header = ' FULL EBOUNDS EXTENSION HEADER '
        IF (wtfile) THEN
          write(ounit,400,IOSTAT=ierr) header
          write(ounit,400,IOSTAT=ierr) line
          write(ounit,400,IOSTAT=ierr) blank
        ENDIF
        call pg_fcecho(header,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 100 
        ENDIF
        call pg_fcecho(line,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 100 
        ENDIF
        call pg_fcecho(blank,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 100 
        ENDIF
        key_no = 1
        endhead = .false.
        do WHILE (.NOT.endhead)
          status = 0
          call ftgrec(iunit,key_no,card,status)
          call pg_fcecho(card,page,ln_cnt,status)
          IF (status.NE.0) THEN
            goto 100 
          ENDIF
          IF (wtfile) THEN
            write(ounit,400,IOSTAT=ierr) card
          endif
          IF (card(1:3).EQ.'END') THEN
            endhead = .true.
          ENDIF
          key_no = key_no + 1
        enddo
      ELSE
c
c --- READ MANDATORY HEADER KEYWORDS, DISPLAY IF DISPHD = 'N' ---
c
        call man_key(iunit,extname,ounit,wtfile,
     &                page,ln_cnt,chatter)
      ENDIF
c
c --- READ AND DISPLAY EBOUNDS DATA, CHAN,E_MIN,E_MAX ---
c
      status = 0
      call ftgkyj(iunit,'NAXIS2',npha,comm,status)
      errinfo=errstr//' ... getting Naxis2 parameter'
      call wt_ferrmsg(status,errinfo)
      status = 0
c
c --- GET RANGE OF CHANNELS TO BE DISPLAYED ---
c
      call pg_fcecho(blank,page,ln_cnt,status)
      IF (status.NE.0) THEN
         goto 100 
      ENDIF
  20  status = 0
      call uclgst('ebdchan',ebdchan,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' ... getting ebdchan parameter'
        call fcecho(errinfo)
      ENDIF
      perr = 0
      call ftupch(ebdchan)
      call crmvlbk(ebdchan)
      IF (ebdchan(1:3).NE.'ALL') THEN
        call par_cmd(ebdchan,ran1,ran2,perr,npha)
        IF (perr.NE.0) THEN
          errinfo=errstr// 'invalid command syntax'
          call fcecho(errinfo)
          errinfo = ' SYNTAX : minchan - maxchan '
          call fcecho(errinfo)
          errinfo = ' For example : 1-20'
          call fcecho(errinfo)
          errinfo = ' Try Again !'
          call fcecho(errinfo)
          goto 20
        ENDIF
      ELSE
        ran1 = 1
        ran2 = npha
      ENDIF
c
c --- FIND COLUMN NUMBERS FOR CHANNEL,E_MIN and E_MAX ---
c
      status = 0
      call ftgcno(iunit,.FALSE.,'CHANNEL',chancol,status)
      errinfo = errstr// ' error finding CHANNEL COLUMN !'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        cont = .false.
        return
      ENDIF
      status = 0
      call ftgcno(iunit,.FALSE.,'E_MIN',emincol,status)
      errinfo = errstr// ' error finding E_MIN COLUMN !'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        cont = .false.
        return
      ENDIF     
      status = 0
      call ftgcno(iunit,.FALSE.,'E_MAX',emaxcol,status)
      errinfo = errstr// ' error finding E_MAX COLUMN !'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        cont = .false.
        return
      ENDIF     
c
c --- READ AND DISPLAY DATA ROW AT A TIME ---
c
      IF (wtfile) THEN
        write(ounit,400,IOSTAT=ierr) blank
      ELSE
        call pg_fcecho(blank,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 100
        ENDIF
      ENDIF
      line =   ' ------------------------------------'
      header = ' CHANNEL     E_MIN keV      E_MAX keV'
      IF (wtfile) THEN
        write(ounit,400,IOSTAT=ierr) line
        write(ounit,400,IOSTAT=ierr) header
        write(ounit,400,IOSTAT=ierr) line
      ELSE
        call pg_fcecho(line,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 100
        ENDIF
        call pg_fcecho(header,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 100
        ENDIF
        call pg_fcecho(line,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 100
        ENDIF
      ENDIF
      felem = 1
      nelems = 1
      do row=ran1,ran2
c --- READ ROW OF DATA
        status = 0
        inull = 0
        call ftgcvj(iunit,chancol,row,felem,nelems,inull,chanval,
     &              anyflg,status)
        enull = 0
        status = 0
        call ftgcve(iunit,emincol,row,felem,nelems,enull,eminval,
     &              anyflg,status)
        enull = 0
        status = 0
        call ftgcve(iunit,emaxcol,row,felem,nelems,enull,emaxval,
     &              anyflg,status)
c --- DISPLAY ROW OF DATA
        ierr = 0
        write(data,300,IOSTAT=ierr) chanval,eminval,emaxval
        IF (ierr.NE.0) THEN
          errinfo = errstr//' error writing data row '
          call fcecho(errinfo)
          write(ounit,400,IOSTAT=ierr) errinfo
        ELSE
          IF (.NOT.wtfile) THEN
            call pg_fcecho(data,page,ln_cnt,status)
            IF (status.NE.0) THEN
              goto 100
            ENDIF
          ELSE
            write(ounit,400,IOSTAT=ierr) data
          ENDIF
        ENDIF
      enddo
 100  status = 0
      call ftclos(iunit,status)
      errinfo = errstr//' closing EBOUNDS file'
      call wt_ferrmsg(status,errinfo)
 200  format(f7.4)
 300  format(I4,6X,f12.7,3X,f12.7)
 400  format(A80)
      return
      end
c
c --- END OF SUBROUTINE RMF_EBD ----------------------------------
c 

*+MAN_KEY
      subroutine man_key(iunit,extname,ounit,wtfile,
     &                   page,ln_cnt,chatter)
c     ------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c
c This routine reads and displays OGIP mandatory Keywords ...
c
c TELESCOP    : Mission/Telescope name
c INSTRUME    : Instrument/Detector name
c RMFVER      : OGIP FITS version
c FILTER      : Filter status
c DETCHANS    : No. of PHA entries
c
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      integer iunit,chatter,ounit,ln_cnt
      character(8) extname
      logical wtfile,page
c
c --- INTERNALS ---------------------------------------------------
c
      character(26) errstr,comm
      character(8) telescop,instrume,rmfver,filter
      character(10) cnum
      character(80) header,blank,line,subinfo,errinfo
      integer status,detchans,erange,ierr
c
c --- VARIABLE DIRECTORY ------------------------------------------
c
c Arguments ...
c
c iunit      int    : Fortran input unit  
c extname    char   : Character name
c 
c Internals ...
c
c telescop   char   : Mission/Telescope name
c instrume   char   : Instrument/Detector name
c rmfver     char   : OGIP version of FITS file
c filter     char   : Filter, if not present NONE
c detchans   int    : No. of Pha entries
c cnum       char   : Character used for output for integer value,detchans  
c header     char   : Used for each line of output, using FCECHO
c blank      char   : Used for blank line output
c line       char   : Used for underline output
c status     int    : FITSIO Error flag
c
c --- CALLED ROUTINES ---------------------------------------------
c
c subroutine FTGKYx     : (FITSIO) Reads Keywords of type x
c subroutine FCECHO     : (FTOOLS), standalone write to screen
c subroutine WT_FERRMSG : (CALTOOLS), writes routine and FITS error
c                         message if necessary
c
c --- COMPILATION AND LINKING -------------------------------------
c
c Link with FTOOLS,FITSIO and CALTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1993 March 19)
c Rehana Yusaf (1993 August 2) 1.0.1; Minor change to detector/instrume
c                             display, add space
c Rehana Yusaf (1994 April 13) 1.0.2; Add page and ln_cnt arguments
      character(5) version
      parameter (version = '1.0.2')
*-
c -----------------------------------------------------------------
c
c --- USER INFO ---
c
      IF (chatter.GE.10) THEN
         subinfo = ' ... using MAN_KEY Ver '//version
         call fcecho(subinfo)
      ENDIF
      errstr =' ERROR : MAN_KEY Ver '//version
c
c --- READ MANDATORY KEYWORDS ---
c
      blank = '  '
      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      status = 0
      call ftgkys(iunit,'RMFVERSN',rmfver,comm,status)
      status = 0
      call ftgkyj(iunit,'DETCHANS',detchans,comm,status)
      IF ((extname.EQ.'SPECRESP').OR.
     &          (extname.EQ.'MATRIX')) THEN
        status = 0
        call ftgkyj(iunit,'NAXIS2',erange,comm,status)
      ENDIF
c
c --- DISPLAY MANDATORY HEADER KEYWORDS ---
c
      call fcecho(blank)
      IF (wtfile) THEN
        write(ounit,300,IOSTAT=ierr) blank
      ENDIF
      line =   ' ------------------------------------- '
      call fcecho(line)
      IF (wtfile) THEN
        write(ounit,300,IOSTAT=ierr) line
      ENDIF
      header = ' MANDATORY '//extname//' HEADER INFORMATION '
      call fcecho(header)
      call fcecho(line)
      call fcecho(blank)
      IF (wtfile) THEN
        write(ounit,300,IOSTAT=ierr) header
        write(ounit,300,IOSTAT=ierr) line
        write(ounit,300,IOSTAT=ierr) blank
      ENDIF
      header = ' Detector Identity     : '//telescop//' '
     &//instrume
      call fcecho(header)
      IF (wtfile) THEN
        write(ounit,300,IOSTAT=ierr) header
      ENDIF
      ierr = 0
      write(cnum,100,IOSTAT=ierr) detchans
      call crmvlbk(cnum)
      IF (ierr.NE.0) THEN
        errinfo = errstr//' error writing detchans'
        call fcecho(errinfo)
        IF (wtfile) THEN
          write(ounit,300,IOSTAT=ierr) errinfo
        ENDIF
        ierr = 0
      ELSE
        header = ' No. of PHA Channels   : '//cnum
        call fcecho(header)
        IF (wtfile) THEN
          write(ounit,300,IOSTAT=ierr) header
        ENDIF
      ENDIF
      call crmvlbk(filter)
      header = ' Filter                : '//filter
      call fcecho(header)
      IF (wtfile) THEN
        write(ounit,300,IOSTAT=ierr) header
      ENDIF 
      IF ((extname.EQ.'SPECRESP').OR.(extname.EQ.'MATRIX')) THEN
        write(cnum,100,IOSTAT=ierr) erange
        call crmvlbk(cnum)
        IF (ierr.NE.0) THEN
          errinfo = errstr//' writing energy ranges'
          call fcecho(errinfo)
          IF (wtfile) THEN
            write(ounit,300,IOSTAT=ierr) errinfo
          ENDIF     
          ierr = 0
        ELSE
          header = ' No. of Energy Ranges  : '//cnum
          call fcecho(header)
          IF (wtfile) THEN
            write(ounit,300,IOSTAT=ierr) header
          ENDIF
        ENDIF
      ENDIF
      call crmvlbk(rmfver)
      header = ' OGIP version of FITS  : '//rmfver
      call fcecho(header)
      IF (wtfile) THEN
        write(ounit,300,IOSTAT=ierr) header
      ENDIF   
 100  format(I4)
 200  format(f7.4)
 300  format(A80)
      return
      end      
c
c --- END OF SUBROUTINE MAN_KEY ------------------------------------
c


*+GET_NPHA
      subroutine get_npha(iunit,npha)
c     -------------------------------
c
      integer iunit,npha
      character(8) extname
      integer nhdu,htype,status
      logical findext
      character(30) comm
c
c
c
*-

      nhdu = 2
      findext = .false.
      do WHILE(.NOT.findext)
        status = 0
        call ftmahd(iunit,nhdu,htype,status)
        call ftgkys(iunit,'EXTNAME',extname,comm,status)
        IF (extname.EQ.'EBOUNDS'.OR.extname.EQ.'SPECRESP'
     &.OR.extname.EQ.'MATRIX') THEN
          findext=.true.
        ENDIF
      enddo
      call ftgkyj(iunit,'DETCHANS',npha,comm,status)
      return
      end


*+RMF_RMF
      subroutine rmf_rmf(infile,disphd,dispmat,
     &                   ounit,wtfile,page,
     &                   ln_cnt,cont,chatter)
c     --------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c
c This routine reads and displays the RMF extension of a FITS format
c RESPONSE file.
c
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      integer iunit,chatter,ounit,ln_cnt
      character*(*) infile 
      logical disphd,dispmat
      logical cont,wtfile,page
c
c --- INTERNALS --------------------------------------------------------
c
      character(30) errstr,wrnstr
      character(80) errinfo,blank,line,header,card,subinfo
      character(80) datastr,undline
      character(70) rmfener
      character(40) comm
      character(9) teff,curgrp,cscale
      character(10) cmin,cmax
      character(4) cgrp
      character(5) cresp,cbin1,cbin2
      integer felem,nelems,inull,row,fchancol,nchancol,i,k,l
      integer nrmf,mincol,emaxcol,grpcol,matcol
      integer maxgrp,maxmat,tempfin
      parameter (maxgrp=256)
      parameter (maxmat = 4096)
      integer status,key_no,stnum,finum,counter,ran1,ran2
      integer*2 resp,binlo,binhi,st,fin,counter2
      real enull,eminval,emaxval,matvals(maxmat),eff,scale
      integer grpval,fchans(maxgrp),nchans(maxgrp),perr,ierr
      logical endhead,anyflg,enddata

c VARIABLES FOR MVEXT

      integer nsearch,ninstr
      parameter (nsearch = 50)
      integer next(nsearch),extnum,htype
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9)
      character(16) extname
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c Arguments ...
c
c iunit      int    : Fortran input unit
c ounit      int    : Fortran i/o unit
c chatter    int    : Chatter flag (>20 verbose)
c outfile    char   : Output filename, default is no ootput file
c disphd     char   : 'Y' if user wants to display full header
c dispmat    char   : 'Y' if user want to display full matrix
c cont       logical: True if no error is encountered
c wtfile     logical: True if writing to file
c
c Internals ...
c
c subinfo    char   : User information about routine
c errstr     char   : Error string for this routine
c wrnstr     char   : Warning string for this routine
c errinfo    char   : additional error information
c blank      char   : Used for blank line output to screen
c line       char   : Used for underline output to screen
c header     char   : Used for screen output
c card       char   : For reading record from FIT file
c datastr    char   : String containing data for screen output
c undline    char   : For underline screen output
c comm       char   : For reading comment field of FITS record
c extname    char   : FITS extension name
c eminval    real   : Minimum energy value
c emaxval    real   : Maximum energy value
c eff        real   : Sum of Channels for group
c grpval     int    : No. of groups in energy row
c scale      real   : Scaling factor used to scale data read from file
c resp       int    : No. of response values
c binlo      int    : Lower bin range of a group
c binhi      int    : Upper bin range of a group
c cmin       char   : Used for output of eminval
c cmax       char   : Used for output of emaxval
c teff       char   : Used for output of eff
c curgrp     char   : Current group No.
c cgrp       char   : For output of grpval value
c cresp      char   : For output of resp value
c cbin1      char   : For output of binlo value 
c cbin2      char   : Used for binhi output
c fchans     int    : Array of starting channel for each respective group
c nchans     int    : Array of number of channels in each group
c maxgrp     int    : Array dimension for fchans and nchans
c maxmat     int    : Array dimension for matvals
c matvals    real   : Array of Matrix values for a group
c fchancol   int    : Column number of fchans
c nchancol   int    : Column number of nchans
c mincol     int    : Column number of eminval
c emaxcol    int    : Column number of emaxval
c grpcol     int    : Column number of grpval
c matcol     int    : Column number of matvals
c nrmf       int    : Counter for number of RMF data rows
c row        int    : Loop counter
c felem      int    : First pixel of element vector to be read
c nelems     int    : Number of elements to be read
c inull      int    : Null integer value for fitsio
c enull      real   : Null real value for fitsio
c snum       int    : Starting number for reading Keywords
c ncols      int    : Number of columns in extension
c nhdu       int    : Header number unit
c status     int    : Error status flag
c htype      int    : Type of hdu
c key_no     int    : Sequence number of keyword record
c rmfener    char   : User command, energy range
c ran1       int    : Lower value of range to be displayed
c ran1       int    : Upper range value
c findext    logical: TRUE when desired extension is found
c endhead    logical: True when end of extension header
c anyflg     logical: Set to true by fitsio when any of the 
c                     returned data values are undefined
c foundcol   logical: True when column found
c enddata    logical: True when all the channel in a group have 
c                     been displayed on the screen
c    
c
c --- CALLED ROUTINES -------------------------------------------------- 
c
c subroutine FTMAHD   : (FITSIO) Move to header unit
c subroutine FTGKYx   : (FITSIO) To read keyword of type x,
c                       for example s (string)
c subroutine FTGREC   : (FITSIO) Reads FITS record
c subroutine FTGCVx   : (FITSIO) Reads data column of type x
c subroutine WT_FERRMSG : (CALTOOLS) Writes FITSIO and routine
c                         error message if appropriate
c subroutine FCECHO   : (FTOOLS) standalone screen write
c subroutine MAN_KEY  : Writes mandatory keywords, if necessary
c subroutine MVEXT    : (CALLIB) Open infile and move to desired 
c                       extension
c subroutine MVER     : (CALLIB) Move to desired extension, assuming
c                       file is already open
c                                                   
c
c --- LINKING/COMPILATION ----------------------------------------------
c
c Link with FITSIO,FTOOLS and CALTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------------
c
c Rehana Yusaf (1993 March 19)
c Rehana Yusaf (1993 August 2) 1.0.1; Fix bug, full header is now
c                              displayed to file, if requested.
c Rehana Yusaf (1993 Dec 6) 1.0.1; Increase matvals array size (should
c				  be as large as DETCHANS)
c Rehana Yusaf (1994 April 13) 1.0.2; Add page and ln_cnt arguments
c                                 Add mvext
c                                 add pg_fcecho
c Rehana Yusaf (1994 May 24) 1.0.3;MVEXT has an additional passed 
c                                  parameter, rwmode
c                                  MVER added to move to MATRIX extension
c                                  if not SPECRESP ext
c                                  DISPMAT no longer redundant, if "N"
c                                  then a concise rmf display
C toliver (1998 01 Jun) 1.0.4;  Increased sizes of CMIN and CMAX arrays and
C                               format field size to allow displaying of E_MAX
C                               and E_MIN values above 99.999999 per thread
C                               ideas/dmprmf_980424
C
C James Peachey (1999 Sep 9) 1.0.5; Increased by one character the
C     width of output format in rmf_rmf to accomodate Astro-E rmfs.
C
      character(5) version
      parameter (version = '1.0.5')
*-
c ---------------------------------------------------------------------- 
c
c --- USER INFO ---
c
      IF (chatter.GE.10) THEN
        subinfo = ' ... using RMF_RMF Ver '//version
        call fcecho(subinfo)
      ENDIF
c
c --- MOVE TO MATRIX EXTENSION ---
c
      errstr = ' ERROR : RMF_RMF Ver '//version
      wrnstr = ' WARNING : RMF_RMF Ver '//version
      blank = '  '
      ninstr = 2
      instr(1) = 'RESPONSE'
      instr(2) = 'RSP_MATRIX'
      status = 0
      extname = 'SPECRESP MATRIX'
      call mvext(0,infile,iunit,ninstr,instr,nsearch,next,
     &           outhdu,extnames,outver,extname,status,chatter)
      IF(status.eq.1) THEN
         cont=.false.
         goto 70
      endif

      IF (status.NE.0) THEN
       status = 0
       call ftmahd(iunit,1,htype,status)
       subinfo=errstr//'moving to primary extension'
       call wt_ferrmsg(status,subinfo)
       IF (status.NE.0) THEN
         goto 70
       ENDIF
       extname = 'MATRIX'
       extnum = 0
       call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &           outhdu,extnames,outver,extname,status,chatter)
       IF (status.NE.0) THEN
         cont = .false.
         goto 70
       ENDIF
      ENDIF
c
c --- READ AND DISPLAY FULL HEADER IF DISPHD = 'Y'---
c
      IF (disphd) THEN
        line =   ' ------------------------------------ '
        call pg_fcecho(blank,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 70
        ENDIF
        call pg_fcecho(line,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 70
        ENDIF
        IF (wtfile) THEN
          write(ounit,500,IOSTAT=ierr) blank
          write(ounit,500,IOSTAT=ierr) line
        ENDIF
        header = ' FULL SPECRESP/MATRIX EXTENSION HEADER '
        call pg_fcecho(header,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 70
        ENDIF
        call pg_fcecho(line,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 70
        ENDIF
        call pg_fcecho(blank,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 70
        ENDIF
        IF (wtfile) THEN
          write(ounit,500,IOSTAT=ierr) header
          write(ounit,500,IOSTAT=ierr) line
          write(ounit,500,IOSTAT=ierr) blank
        ENDIF
        key_no = 1
        endhead = .false.
        do WHILE (.NOT.endhead)
          status = 0
          call ftgrec(iunit,key_no,card,status)
          call pg_fcecho(card,page,ln_cnt,status)
          IF (status.NE.0) THEN
            goto 70
          ENDIF
          IF (wtfile) THEN
            write(ounit,500,IOSTAT=ierr) card
          ENDIF
          IF (card(1:3).EQ.'END') THEN
            endhead = .true.
          ENDIF
          key_no = key_no + 1
        enddo   
      ELSE
c
c --- READ MANDATORY HEADER KEYWORDS, AND DISPLAY THEM ---
c
        call man_key(iunit,extname,ounit,wtfile,
     &               page,ln_cnt,chatter)
      ENDIF
c
c --- GET No. OF ENERGY ROWS
c
      status = 0
      call ftgkyj(iunit,'NAXIS2',nrmf,comm,status)
      errinfo = errstr//' reading No. of energy rows !'
      call wt_ferrmsg(status,errstr)
      IF (status.NE.0) THEN
        cont = .false.
        goto 70
      ENDIF
c
c --- OBTAIN RMFENER, THE RANGE OF ENERGYS TO BE DISPLAYED ---
c
      IF (wtfile) THEN
        write(ounit,500,IOSTAT=ierr) blank
      ENDIF
      call pg_fcecho(blank,page,ln_cnt,status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
  20  status = 0
      call uclgst('rmfener',rmfener,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' ... getting rmfener parameter'
        call fcecho(errinfo)
      ENDIF   
      call ftupch(rmfener)
      call crmvlbk(rmfener)
      IF (rmfener(1:3).NE.'ALL') THEN
         perr = 0
         call par_cmd(rmfener,ran1,ran2,perr,nrmf)
         IF (perr.NE.0) THEN
           errinfo = errstr//' invalid range syntax !'
           call fcecho(errinfo)
           errinfo = ' SYNTAX : minchan - maxchan'
           call fcecho(errinfo)
           errinfo = ' For example : 1-20'
           call fcecho(errinfo)
           errinfo = ' Try Again !'
           call fcecho(errinfo)
           goto 20
         ENDIF
      ELSE
         ran1 = 1
         ran2 = nrmf
      ENDIF
c
c --- READ AND DISPLAY RMF DATA ,THAT IS ---
c --- ENERGY_LO,ENERGY_HI,N_GRP,F_CHAN,N_CHAN AND MATRIX ---
c
c --- FIND COLUMN NUMBERS ---
c
      status = 0
      call ftgcno(iunit,.FALSE.,'ENERG_LO',mincol,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' finding ENERG_LO column'
        call fcecho(errinfo)
        cont = .false.
        return
      ENDIF
      
      status = 0
      call ftgcno(iunit,.FALSE.,'ENERG_HI',emaxcol,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' finding ENERG_HI column'
        call fcecho(errinfo)
        cont = .false.
        return
      ENDIF
  
      status = 0
      call ftgcno(iunit,.FALSE.,'N_GRP',grpcol,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' finding N_GRP column'
        call fcecho(errinfo)
        cont = .false.
        return
      ENDIF
  
      status = 0
      call ftgcno(iunit,.FALSE.,'F_CHAN',fchancol,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' finding F_CHAN column'
        call fcecho(errinfo)
        cont = .false.
        return
      ENDIF

      status = 0
      call ftgcno(iunit,.FALSE.,'N_CHAN',nchancol,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' finding N_CHAN column'
        call fcecho(errinfo)
        cont = .false.
        return
      ENDIF         
  
      status = 0
      call ftgcno(iunit,.FALSE.,'MATRIX',matcol,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' finding MATRIX column'
        call fcecho(errinfo)
        cont = .false.
        return
      ENDIF         
c
c --- READ AND DISPLAY DATA ---
c
      header = ' RMF DATA'
      line   = ' --------'
      IF (.NOT.wtfile) THEN
        call pg_fcecho(blank,page,ln_cnt,status)        
        IF (status.NE.0) THEN
          goto 70
        ENDIF
        call pg_fcecho(line,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 70
        ENDIF
        call pg_fcecho(header,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 70
        ENDIF
        call pg_fcecho(line,page,ln_cnt,status)
        IF (status.NE.0) THEN
          goto 70
        ENDIF
      ELSE
        write(ounit,500,IOSTAT=ierr) blank
        write(ounit,500,IOSTAT=ierr) line
        write(ounit,500,IOSTAT=ierr) header
        write(ounit,500,IOSTAT=ierr) line
      ENDIF
      felem = 1
c
c --- DISPLAY USER DEFINED RANGE OF ENERGYS ---
c
      do row=ran1,ran2
        felem = 1
        nelems = 1
        status = 0
        enull = 0
        call ftgcve(iunit,mincol,row,felem,nelems,enull,eminval,
     &              anyflg,status)
        errinfo = errstr//' reading ENERG_LO column'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          cont = .false.
          return 
        ENDIF
        enull = 0
        status = 0
        call ftgcve(iunit,emaxcol,row,felem,nelems,enull,emaxval,
     &              anyflg,status)
        errinfo = errstr//' reading ENERG_HI column'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          cont = .false.
          return
        ENDIF          
        status = 0
        inull = 0
        call ftgcvj(iunit,grpcol,row,felem,nelems,inull,grpval,
     &              anyflg,status)
        errinfo = errstr// ' reading N_GRP column'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          cont = .false.
          return
        ENDIF
        enull = 0      
        status = 0
        inull = 0
        IF (grpval.GT.maxgrp) THEN
          errinfo = wrnstr//' DMPRMF cannot display >256 groups'
          call fcecho(errinfo)
          goto 60
        ENDIF
        nelems = grpval
        call ftgcvj(iunit,fchancol,row,felem,nelems,inull,
     &              fchans,anyflg,status)
        errinfo = errstr//' reading F_CHAN column'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          cont = .false.
          return
        ENDIF      
        status = 0
        inull = 0
        nelems = grpval
        call ftgcvj(iunit,nchancol,row,felem,nelems,inull,
     &              nchans,anyflg,status)
        errinfo = errstr//' reading N_CHAN column'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          cont = .false.
          return
        ENDIF      
c
c --- DISPLAY CURRENT ROW INFO ---
c    
        write(undline,80)
        IF (wtfile) THEN
          write(ounit,500,IOSTAT=ierr) undline
        ELSE
          call pg_fcecho(undline,page,ln_cnt,status)
          IF (status.NE.0) THEN
            goto 70
          ENDIF
        ENDIF
        ierr = 0
        write(cmin,100,IOSTAT=ierr) eminval
        write(cmax,100,IOSTAT=ierr) emaxval
        write(curgrp,200,IOSTAT=ierr) row
        IF (ierr.NE.0) THEN
          errinfo = errstr//' writing energy range,e_min,e_max'
          call pg_fcecho(errinfo,page,ln_cnt,status)
          IF (status.NE.0) THEN
            goto 70
          ENDIF
          IF (wtfile) THEN
            write(ounit,500,IOSTAT=ierr) errinfo
          ENDIF
          ierr = 0
        ELSE
          header = ' Energy Range '//curgrp//' E_MIN : '//cmin
     &//' E_MAX : '//cmax
          IF (.NOT.wtfile) THEN        
            call pg_fcecho(header,page,ln_cnt,status)
          IF (status.NE.0) THEN
            goto 70
          ENDIF
          ELSE
            write(ounit,500,IOSTAT=ierr) header
          ENDIF               
        ENDIF
        write(cgrp,200,IOSTAT=ierr) grpval
        IF (ierr.EQ.0) THEN
          header = ' No. of Channel groups : '//cgrp
          IF (.NOT.wtfile) THEN
            call pg_fcecho(header,page,ln_cnt,status)
            IF (status.NE.0) THEN
              goto 70
            ENDIF
          ELSE
            write(ounit,500,IOSTAT=ierr) header
          ENDIF     
        ELSE
          errinfo = errstr//' writing channel groups'
          call fcecho(errinfo)
          IF (wtfile) THEN
            write(ounit,500,IOSTAT=ierr) errinfo
          ENDIF     
          ierr = 0
        ENDIF
c
c --- READ AND DISPLAY DATA FOR EACH GROUP RESPECTIVELY FOR THIS ROW ---
c
        k = 0
        do i=1,grpval
           binlo = fchans(i)
           binhi = binlo + nchans(i) - 1
           counter = 0
           stnum = k + 1
           finum = k + nchans(i)
           tempfin = finum
           IF (nchans(i).GT.maxmat) THEN
            errinfo =' Only first 4096 response values displayed!'  
     &//' for next group'
            call fcecho(errinfo)
            errinfo = errstr//' Array values too small'
            finum = k+ maxmat - 1
           ENDIF
           k = tempfin 
           eff = 0
           do l = stnum,finum
             status = 0
             nelems = 1
             felem = l
             enull = 0
             counter = counter + 1
             call ftgcve(iunit,matcol,row,felem,nelems,enull,
     &                   matvals(counter),anyflg,status)
             eff = eff + matvals(counter)
           enddo
           ierr = 0
           write(curgrp,200,IOSTAT=ierr) i
           IF (ierr.EQ.0) THEN
             header = ' GROUP '//curgrp
             IF (.NOT.wtfile) THEN
               call pg_fcecho(header,page,ln_cnt,status)
               IF (status.NE.0) THEN
                goto 70
               ENDIF
             ELSE
               write(ounit,500,IOSTAT=ierr) header
             ENDIF     
           ELSE
             errinfo = wrnstr//' cannot write current group number'
             call fcecho(errinfo)
             IF (wtfile) THEN
               write(ounit,500,IOSTAT=ierr) errinfo
             ENDIF 
           ENDIF
           ierr = 0
           resp = nchans(i)
           write(cresp,250,IOSTAT=ierr) resp
           write(teff,400,IOSTAT=ierr) eff
           IF (ierr.EQ.0) THEN
             header = ' No. of Response Values : '//cresp
     &      //'  Total Efficiency : '//teff
             IF (.NOT.wtfile) THEN
               call pg_fcecho(header,page,ln_cnt,status)
               IF (status.NE.0) THEN
                goto 70
               ENDIF
             ELSE
               write(ounit,500,IOSTAT=ierr) header
             ENDIF    
           ELSE
             errinfo = errstr//' writing response and efficiency'
             call fcecho(errinfo)
             IF (wtfile) THEN
               write(ounit,500,IOSTAT=ierr) errinfo
             ENDIF    
             ierr = 0
           ENDIF
         IF (dispmat) THEN
           call scaler(matvals,stnum,finum,scale)
           write(cbin1,250,IOSTAT=ierr) binlo
           write(cbin2,250,IOSTAT=ierr) binhi
           write(cscale,400,IOSTAT=ierr) scale
           IF (ierr.EQ.0) THEN
             header = ' Bin Range : '//cbin1//'  to'//cbin2
     &     //'  Response Value Scaler : '//cscale
             IF (.NOT.wtfile) THEN
               call pg_fcecho(header,page,ln_cnt,status)
               IF (status.NE.0) THEN
                 goto 70
               ENDIF
             ELSE
               write(ounit,500,IOSTAT=ierr) header
             ENDIF    
           ELSE
             errinfo = errstr//' writing Bin Range and scaler'
             call pg_fcecho(errinfo,page,ln_cnt,status)
             IF (status.NE.0) THEN
               goto 70
             ENDIF
             IF (wtfile) THEN
               write(ounit,500,IOSTAT=ierr) errinfo
             ENDIF   
             ierr = 0
           ENDIF
           enddata =.false.
           counter2 = counter
           st = 1
           do WHILE (.NOT.enddata)
             IF (counter2.LE.6) THEN
                ierr = 0
                write(datastr,300,IOSTAT=ierr)
     &                       (scale*matvals(l),l=st,counter)
                enddata=.true.
                IF (ierr.EQ.0) THEN
                  IF (.NOT.wtfile) THEN
                    call pg_fcecho(datastr,page,ln_cnt,status)
                    IF (status.NE.0) THEN
                      goto 70
                    ENDIF
                  ELSE
                    write(ounit,500,IOSTAT=ierr) datastr
                  ENDIF    
                ELSE
                  errinfo = errstr//' writing data in bin'
                  call fcecho(errinfo)
                  IF (wtfile) THEN
                    write(ounit,500,IOSTAT=ierr) errinfo
                  ENDIF 
                  ierr = 0
                ENDIF
             ELSE
                ierr = 0
                fin = st + 5
                write(datastr,300,IOSTAT=ierr) 
     &                        (scale*matvals(l),l=st,fin)
                IF (ierr.EQ.0) THEN
                  IF (wtfile) THEN
                    write(ounit,500,IOSTAT=ierr) datastr
                  ELSE
                    call pg_fcecho(datastr,page,ln_cnt,status)
                    IF (status.NE.0) THEN
                      goto 70
                    ENDIF
                  ENDIF      
                ELSE
                  errinfo = errstr//' writing data in bin'
                  call fcecho(errinfo)
                  IF (wtfile) THEN
                    write(ounit,500,IOSTAT=ierr) errinfo
                  ENDIF
                ENDIF
                counter2 = counter2 - 6
                st = fin + 1 
             ENDIF
           enddo
          ENDIF
        enddo  
  60  enddo
      IF (wtfile) THEN
       write(ounit,500,IOSTAT=ierr) undline
      ELSE
       call fcecho(undline)
      ENDIF
  70  status = 0
      call ftclos(iunit,status)
      errinfo = errstr//' closing RMF file'
      call wt_ferrmsg(status,errinfo)
  80  format (72('-'))
 100  format(f10.6)
 200  format(I4)
 250  format(I5)
 300  format(6(1X,f12.6))
 400  format (1pg9.2)
 500  format(A80)
      return
      end 
c
c --- END OF SUBROUTINE RMF_RMF ----------------------------------------
c
        

*+SCALER
      subroutine scaler(matvals,stnum,finum,scale)
c     --------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c
c This routine calculates a scaling factor for data, by taking
c the maximum value in the data set into account.
c
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE 
      real matvals(*),scale
      integer i,stnum,finum
      real maxdata
c
c --- VARIABLE DIRECTORY -----------------------------------------
c

c --- AUTHORS/MODIFICATION HISTORY--------------------------------
c
c Rehana Yusaf (1993 March 23): Essentially this code is extracted
c                               from RDRSP (M A Sweeney) and
c                               minor modifications have been made
c Rehana Yusaf (1993 Dec 7) 1.0.1 ; Change 10** to 10.0 ** the
c				prevoius setup caused problems with
c				maxdata values <10E-14

      character(5) version
      parameter (version ='1.0.1')
*-
c ----------------------------------------------------------------

      maxdata = 0
      do i=stnum,finum
         maxdata = max(maxdata,matvals(i))
      enddo       
      IF ((0.0.LT.maxdata).AND.(maxdata.LE.1.0)) THEN
        scale = (10.0)**(-INT(LOG10(maxdata))+1)
      ELSEIF (maxdata.GE.100.0) THEN
        scale = (10.0)**(1-INT(LOG10(maxdata)))
      ELSE
        scale = 1.0   
      ENDIF
      return
      end
c
c --- END OF SUBROUTINE SCALER ------------------------------------
c 


*+PAR_CMD
      subroutine par_cmd(icmd,ran1,ran2,perr,max)
c     -------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c
c This routine parses a user input command string, containing a range.
c Two numbers are extracted from the string, and converted to integer.
c
c --- VARIABLES -------------------------------------------------------
c
      character(70) icmd,cmd,desc,odesc
      character(26) errstr
      character(4) cran1,cran2
      integer ran1,ran2,len,perr,max
      integer pos,beg,end,ierr
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c Argumnents ...
c
c icmd      char    : Command string
c ran1      int     : lower range 
c ran2      int     : upper range
c max       int     : No. of pha values or energy ranges
c perr      int     : Error flag, perr = 0   okay
c                                        1   ran1 is invalid number
c                                        2   ran1<0 or ran1>max
c                                        3   ran2 is invalid number
c                                        4   ran2<0
c                                        5   ran1>ran2
c
c Internals ...
c
c cmd       char    : Command string, after extra spaces removed
c cran1     char    : substring of cmd, containing lower range
c cran2     char    : substring of cmd, containing upper range    
c len       int     : length of int in cran1, and cran2
c
c --- CALLED ROUTINES -------------------------------------------------
c
c FCECHO              : (FTOOLS) screen write
c RMVEXSP             : Compresses more than one blank to single blank
c
c --- COMPILATION AND LINKING -----------------------------------------
c
c Link with CALTOOLS
c 
c --- AUTHORS/COMPILATION ---------------------------------------------
c
c Rehana Yusaf (1993 March 26)
c Rehana Yusaf (1993 May 4) Update, add error flag
c
c Banashree M Seifert (1996 Oct 11) 1.1.0:
c       . format at the end of this subroutine changed from I to i5
c          (LINUX problem)
c ---------------------------------------------------------------------

      character(5) version
      parameter (version = '1.1.0')
*-
c ---------------------------------------------------------------------
c
c --- EXTRACT LOWER RANGE VALUE ---
c
      perr = 0 
      errstr = ' ERROR : PAR_CMD Ver '//version
      call rmvexsp(icmd,cmd)
      pos = index(cmd,'-')
      end = pos -1
      beg = 1
      cran1 = cmd(beg:end)
      len = end
      read(cran1(1:len),100,IOSTAT=ierr) ran1
      IF (ierr.NE.0) THEN
        desc = errstr//' invalid number :'//cran1
        call fcecho(desc)
        perr = 1
        return
      ENDIF

c     Check validity of lower range, must be > 0 and <= maxvalue

      IF ((ran1.LE.0).OR.(ran1.GT.max)) THEN
        perr = 2
        desc = errstr//' lower range value outside limits'
        call rmvexsp(desc,odesc)
        call fcecho(odesc)
        return
      ENDIF
c
c --- EXTRACT UPPER RANGE VALUE ---
c
      beg = end + 2
      pos = index(cmd,' ')
      len = pos - beg
      end = pos -1
      cran2 = cmd(beg:end)
      read(cran2(1:len),100,IOSTAT=ierr) ran2
      IF (ierr.NE.0) THEN
        desc = errstr//' invalid number :'//cran2
        call fcecho(desc)
        perr = 3
        return
      ENDIF 

c Check validity of upper range value, >0, and if >max set to max

      IF (ran2.LE.0) THEN
        perr = 4
        desc = errstr//' upper range value out of limits'
        call rmvexsp(desc,odesc)
        call fcecho(odesc)
        return
      ENDIF  
      IF (ran2.GT.max) THEN
        ran2 = max
      ENDIF
c
c --- VALIDITY CHECK, MINRANGE => MAXRANGE ---
c
      IF (ran1.GT.ran2) THEN
        write(desc,200,IOSTAT=ierr)ran1,ran2
        IF (ierr.EQ.0) THEN
          call rmvexsp(desc,odesc)                       
          call fcecho(odesc)
        ENDIF
        perr = 5
      ENDIF
 100  format(i5)
 200  format (' Lower range :',i5,' is larger than upper range :',i5)
      return
      end
c 
c --- END OF SUBROUTINE PAR_CMD ---------------------------------------
c
  


*+RMF_DMPIMG
c     --------------------------------------------------- 
      subroutine rmf_dmpimg(infile,imagefile,taskname,
     &                      killit,cont,chatter)
c     --------------------------------------------------- 
c --- DESCRIPTION --------------------------------------------------
c This routine reads a RESPONSE matrix and writes it in the form of
c and image FITS file. 
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,imagefile,taskname
      integer chatter
      logical cont,killit
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c infile       char    : input response filename
c imagefile    char    : output image filename
c chatter      int     : verbose if > 20
c cont         logical : false if error detected
c
c --- CALLED ROUTINES ----------------------------------------------
c
c MVEXT (CALLIB)       : Open file and locate desired extension
c RMF_IMG              : Reads response matrix then writes it to output
c RMFSZ                : Returns the array sizes, maxne, maxgrp, and 
c                        maxelt
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1994 April 15) 1.0.0; 
c Alex M. (Feb 1999) 1.1.3: Added new variables maxne, maxgrp, and 
c                           maxelt. Added 'call RMFSZ()'.
c
      character(5) version
      parameter (version='1.0.0')
*-
c ------------------------------------------------------------------
c
c --- LOCALS ---
c
      character(70) subinfo,errinfo
      character(30) errstr,comm
C --- Alex --- added new variables maxchan, maxne, maxgrp, and maxelt:
c maxchan - size channel array;
c maxne   - size of energy array;
c maxgrp  - size of response group array;
c maxelt  - size of response matrix. 

      integer iunit,status,ichan,nrmf,maxchan,maxne,maxgrp,maxelt

c VARIABLES FOR MVEXT

      integer nsearch,ninstr
      parameter (nsearch = 50)
      integer next(nsearch),htype,extnum
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9)
      character(8) extname      

c
c --- USER INFO ---
c
      IF (chatter.GE.15) THEN
        subinfo = ' ... using RMF_DMPIMG Ver '//version
        call fcecho(subinfo) 
      ENDIF
c
c --- Move to desired extension ---
c
      ninstr = 2
      instr(1) = 'RESPONSE'
      instr(2) = 'RSP_MATRIX'
      status = 0
      extname = 'SPECRESP MATRIX'
      call mvext(0,infile,iunit,ninstr,instr,nsearch,next,
     &           outhdu,extnames,outver,extname,status,chatter)

      IF (status.eq.1) then
         cont = .false.
         goto 70
      ENDIF
         
     
      IF (status.NE.0) THEN
       status = 0
       call ftmahd(iunit,1,htype,status)
       subinfo=errstr//'moving to primary extension'
       call wt_ferrmsg(status,subinfo)
       IF (status.NE.0) THEN
         cont = .false.
         goto 70
       ENDIF
       extname = 'MATRIX'
       extnum = 0
       call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &           outhdu,extnames,outver,extname,status,chatter)
       IF (status.NE.0) THEN
         cont = .false.
         goto 70
       ENDIF
      ENDIF

c --- Alex ---  initialize array sizes :

      maxne   = 0 
      maxgrp  = 0
      maxelt  = 0
      maxchan = 0

c --- Alex --- Get the values of array sizes maxne, maxgrp, and maxelt :

      status = 0
      call rmfsz( iunit, chatter, maxne, maxgrp, maxelt, status )
      errinfo = errstr//' reading maxne, maxgrp, and maxelt '
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        cont = .false. 
        goto 70 
      ENDIF

c
c --- GET NPHA VALUE ---
c
      status = 0
      call ftgkyj(iunit,'DETCHANS',maxchan,comm,status)
      errinfo = errstr//' reading DETCHANS '
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        cont = .false. 
        goto 70 
      ENDIF
c
c --- CALL RMF_IMG to read and write response matrix ---
c --- Alex --- added two arguments maxgrp and maxelt
      call rmf_img(iunit,imagefile,maxne,maxchan,maxgrp,maxelt,
     &             taskname,killit,cont,chatter)
  70  IF (.NOT.cont) THEN
        errinfo = errstr//' incomplete image dump'
        call fcecho(errinfo)
      ENDIF

      status = 0
      call ftclos(iunit,status)
      errinfo = errstr//' closing RMF file'
      call wt_ferrmsg(status,errinfo)
      return
      end
c ---------------------------------------------------------------
c     END OF RMF_DMPIMG
c ---------------------------------------------------------------


*+RMF_IMG
c     ----------------------------------------------------------- 
      subroutine rmf_img(iunit,imagefile,imaxne,imaxchan,imaxgrp,
     &                   imaxelt,taskname,killit,cont,chatter)
c     -----------------------------------------------------------
c --- DESCRIPTION  ----------------------------------------------------
c
c This routine reads a response matrix, and writes it as an image to
c a FITS file
c ---------------------------------------------------------------------
c --- VARIABLES ------------------------------------------------------- 
c
      IMPLICIT NONE
      character*(*) imagefile,taskname 
      integer iunit,chatter
      integer imaxne,imaxchan,imaxgrp,imaxelt
      logical cont,killit
c
c --- CALLED ROUTINES -------------------------------------------------
c
c RDRMF1   (CALLIB) : Reads OGIP format Response matrix extension
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1994 April 15) 1.0.0;
c  Rehana Yusaf (1994 Sept 13) 1.0.1; pass in killit, use opfits
c                                     instead of ftinit
c Banashree M Seifert (Oct 1996) 1.1.0:
c          . replaced rdrmf1 by rdrmf3
c Alex M. (Feb 1999) 1.1.3: Added two more arguments to the subroutine 
c                           RMF_IMG: imaxgrp and imaxelt. Replaced 
c                           RDRMF3 by RDRMF4.  
C ngan  (1999 Oct 1) 1.1.4:
C          . Corrected the wrong size of the full rmf matrix.  
C
c ----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.4')
*-
c ---------------------------------------------------------------------
c
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
c ************************************
c --- LOCALS ---
c
c ... pointers to "arrays" to be dynamically allocated
c --- Alex --- added p_order
      integer p_ngrp, p_fmatrix, p_F_chan, p_N_chan
      integer p_order, p_energ_lo, p_energ_hi

c ... "arrays" to be dynamically allocated
c     integer ngrp(imaxne)             real fmatrix(imaxelt)
c     integer F_chan(imaxne,imaxgrp)   integer N_chan(imaxne,imaxgrp)
c     real energ_lo(imaxne)            real energ_hi(imaxne)

      character(30) errstr
      character(70) subinfo,message,errinfo
      character(16) rmftlscop, rmfinstrum, rmffilt,rmfdet
      character(8) matext,rmfchantype
      character(5) rsp_rmfversn
      character(20) hduclas3
      real lo_thresh, rmfarea
      integer frmfchan
      integer rmfchan,ienerg,block,ounit,bitpix,naxes(2)
      integer naxis,gcount,pcount,status
      logical simple,extend,isorder,qorder

c --- DMA ---

      isorder  = .false.
      qorder   = .false.
      if(imaxelt .lt. imaxne*imaxchan) imaxelt = imaxne*imaxchan

c Allocate dynamic memory

        p_ngrp = 0
        p_fmatrix = 0
        p_F_chan = 0
        p_N_chan = 0
        p_order = 0
        p_energ_lo = 0
        p_energ_hi = 0

        status = 0
        call udmget(imaxne, 4, p_ngrp, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        call udmget(imaxelt, 6, p_fmatrix, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        call udmget(imaxgrp, 4, p_F_chan, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        call udmget(imaxgrp, 4, p_N_chan, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        call udmget(imaxgrp, 4, p_order, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        call udmget(imaxne, 6, p_energ_lo, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        call udmget(imaxne, 6, p_energ_hi, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
 50     IF (status.NE.0) then
              message = errstr// ' Failed to allocate Dynamic Memory'
              call fcecho(message)
              status = -1
              goto 100 
        ENDIF 
c
c --- READ RESPONSE MATRIX ( RDRMF3 HAS BEEN REPLACED BY RDRM4 --- Alex ) ---
c
      call rdrmf4(iunit,chatter,qorder,imaxne,imaxgrp,imaxelt,
     & rsp_rmfversn,hduclas3,rmftlscop,rmfinstrum,rmfdet,
     & rmffilt,rmfarea,rmfchantype,frmfchan,rmfchan,
     & ienerg,imaxgrp,imaxelt,MEMR(p_energ_lo),
     & MEMR(p_energ_hi),MEMI(p_ngrp),MEMI(p_F_chan),
     & MEMI(p_N_chan),isorder,MEMI(p_order),
     & MEMR(p_fmatrix),lo_thresh,status)
      IF (status.NE.0) THEN
        cont = .false.
        goto 100
      ENDIF
c
c --- WRITE RESPONSE MATRIX AS AN IMAGE ---
c
      block = 2880
      call cgetlun(ounit)
c      call ftinit(ounit,imagefile,block,status)
      call opfits(ounit,imagefile,killit,chatter,status)
      errinfo = errstr//' opening imagefile'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        cont = .false.
        goto 100
      ENDIF
      simple = .true.
      bitpix = -32
      naxis = 2
      naxes(1) = rmfchan    
      naxes(2) = ienerg 
      pcount   = 0
      gcount   = 1
      extend   = .TRUE.

c --- WRITE MANDATORY PRIMARY ARRAY KEYWORDS ---

      status = 0
      call ftpprh(ounit,simple,bitpix,naxis,naxes,pcount,gcount,
     &          extend,status)
      errinfo = errstr//' writing mandatory keywords'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        cont = .false.
        goto 100 
      ENDIF
c
c --- TELESCOPE/INSTRUME ---
c
      status = 0
      call ftpkys(ounit,'TELESCOP',rmftlscop,
     &'Mission/Telescope name',status)
      errinfo = errstr//' problem writing TELESCOP keyword'
      call wt_ferrmsg(status,errinfo)

      status = 0
      call ftpkys(ounit,'INSTRUME',rmfinstrum,
     &'Instrument name',status)
      errinfo = errstr//' problem writing INSTRUM keyword'
      call wt_ferrmsg(status,errinfo)

c HDUCLASS and HDUVERS ...

      status = 0
      call ftpkys(ounit,'HDUCLASS','OGIP',
     &'format conforms to OGIP standard',status)
      errinfo = errstr//' Problem writing HDUCLASS keyword'
      call wt_ferrmsg(status,errinfo)

      status = 0
      call ftpkys(ounit,'HDUCLAS1','IMAGE',
     &'dataset is an image',status)
      errinfo = errstr//' problem writing HDUCLAS1 keyword'
      call wt_ferrmsg(status,errinfo)

      status = 0
      call ftpkys(ounit,'HDUVERS1','1.0.0',
     &'Version of family of formats',status)
      errinfo = errstr//' writing HDUVERS1 keyword'
      call wt_ferrmsg(status,errinfo)

      status = 0
      call ftpkys(ounit,'HDUVERS2','1.0.0',
     &'Version of format',status)
      errinfo = errstr//' writing HDUVERS2 keyword'
      call wt_ferrmsg(status,errinfo)
      status = 0
      call ftpdat(ounit,status)
      status = 0
      call FTPKYS(ounit,'CREATOR',
     &                  taskname,
     &             's/w task which wrote this dataset',
     &                  status)
      errinfo = errstr//' problem writing CREATOR keyword'
      IF (chatter.GE.15) THEN
        call wt_ferrmsg(status,subinfo)
      ENDIF


c --- DEFINE DATA STRUCTURE ---

      call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,status)
      errinfo = errstr//' defining primary data structure'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        status = 2
        goto 100 
      ENDIF
      IF (chatter.GE.20) THEN
        subinfo = '     ... data strucure has been defined'
        call fcecho(subinfo)
      ENDIF

c --- WRITE DATA ---

      status = 0
      call ftp2de(ounit,0,rmfchan,naxes(1),naxes(2),
     &            MEMR(p_fmatrix),status)
      errinfo = errstr//' writing primary data '
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        status = 3
      ENDIF
      IF (chatter.GE.20) THEN
        subinfo = '    ... data has been written'
        call fcecho(subinfo)
      ENDIF
      status = 0
      call ftclos(ounit,status)
      errinfo = errstr//' closing imagefile'
      call wt_ferrmsg(status,errinfo)
      
c *****
c Free the dynamic Memory
  100   call udmfre(p_ngrp,4,status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_fmatrix,6,status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_F_chan, 4, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_N_chan, 4, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_order, 4, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_energ_lo, 6, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_energ_hi, 6, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
 485    IF (status.NE.0) then
                message = errstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
                status= 99
        ENDIF
        return
        end
c -------------------------------------------------------------------
c     END OF RMF_IMG 
c -------------------------------------------------------------------  
