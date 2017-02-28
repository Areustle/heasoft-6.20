
*+RBNPHA
c      -----------------
       subroutine rbnpha
c      -----------------
c --- DESCRIPTION -----------------------------------------------------
c RBNPHA physically rebins data in an OGIP standard PHA spectrum, using
c a user input for the resultant number of  channels, to determine  the
c compression factor, or in some cases the DATAMODE keyword defines
c compression.
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE
      character(180) infile,binfile,outfile,subinfo
      character(20) instrum, tlscop
      character(70) hduclas2, shkeys(40)
      character(10) cform(64)
      character(60) keydesc(64)
      character(8) ckeys(64),ctype
      character(132) ckrec(64)
      character(8) task,dmode
      character(5) phaversn
      character(14) cmpmode
      character(100) bincomm
      character(8) cstart,cend,compchar
      real texpos
      integer finchan, chatter, nckeys, n_comm
      integer nfacts,nchan,dtype,errflg,detchans,fchan,lchan,extnum
      integer phsize, cerr,i,ineed,iget,status,channel1,skeys
      integer p_channel, p_counts, p_grping,p_qualty
      integer p_conv
      integer p_rcts, p_serr, p_syserr
      INTEGER p_ascale, p_bscale
      INTEGER p_tcts, p_trcts, p_tserr, p_tsyserr, p_tascale, p_tbscale
      logical qerror, qsys, qqual, qgroup, killit,file,cont
      LOGICAL qascale, qbscale
      integer maxcomp
      parameter (maxcomp=256)
      integer cfacts(maxcomp),stbins(maxcomp),endbins(maxcomp)
      character(100) comms(maxcomp)
      character(7) error
      logical properr
    
      character(1) fchan_out
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c infile     char   : Input filename
c outfile    char   : Output filename
c finchan    int    : Final number of channels (user defined)
c chatter    int    : Chattiness flag (>20 verbose)
c cfacts     int    : conpression factor(s)
c stbins     int    : Array containing starting channel(s) for rebinning
c endbins    int    : Array containing ending channel(s) for rebinning
c nfacts     int    : counter for number of compression factors
c nchan      int    : Number of channels in input file
c dtype      int    : datatype, 1 is counts, and 2 is rate
c instrum    int    : Instrument used
c task       char   : Task name : RBNPHA
c phsize     int    : Array dimension
c channel    int    : Array containing Channel numbers
c counts     int    : Array for counts
c rate       real   : Array containing count rate
c grping     int    : Grouping Array
c qualty     int    : Array containg quality flags
c conv       int*2  : conversion array
c serr       real   : statistcal errors
c syserr     real   : fractional systematic errors
c ascale     real   : Array of areascal values
c bscale     real   : Array of backscal values
c qerror     logical: true if statistical errors 
c qsys       logical: true if systematic errors
c qqual      logical: true if quality flags set
c qgroup     logical: true if grouping flag is set
c qascale    logical: true if vector areascal
c qbscale    logical: true if vector backscal
c --- CALLED ROUTINES -------------------------------------------------
c
c RBN_GP     : Gets parameters
c CMP_FACT   : Determine type of compression factor(s)
c RBN_COMP   : Compress data
c WT_COM     : Write output file, with compressed PHA spectrum
c 
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1993 July 12)
c Rehana Yusaf (1993 August 27) 1.0.1;task is an additional parameter
c                                     for CMP_FACT.
c                               fcerr added for fatal error
c Rehana Yusaf (1993 Oct 19) 1.0.2; Task has been updated to handle 
c                                   non-uniform compression.
c                                   main intended use is for ASCA
c                                   SIS mode data 
c Rehana Yusaf (1993 Nov 21) 1.0.3; Fixed bug. (only there for one day!)
c                                   Task updated to handle missing
c                                   channels (RBN_COMP)     
c                                   WT_COM (CALLIB) has been updated
c                                   to incoorperate HDUCLASS keywords
c				    this task now calls it with the
c                                   additional keywords
c Rehana Yusaf (1993 Dec 1) 1.0.4; Replace DETCHANS with FINCHAN in
c			           wt_com call
c
c Rehana Yusaf (1993 Dec 3) 1.0.5; Pass fchan to cmp_fact
c Rehana Yusaf (1994 Feb 23) 1.0.6; RBN_COMP updated
c Rehana Yusaf (1994 April 1) 1.0.7; Remove "This task physically rebins
c                                    dataset" screen display
c Rehana Yusaf (1994 July 20) 1.0.8; extra argument, cform, passed to
c                                    wt_com
c Rehana Yusaf (1994 Sept 3) 1.0.9; ck_file has an additional argument
c                                   rbn_gp updated to read clobber
c                                   opnpa used instead of op_npa
c Rehana Yusaf (1995 Jan 26) 1.1.1; update rbn_pha
c Rehana Yusaf (1995 April 28) 1.1.2; increase infile length to 160
c                                     update rbn_gp and rbn_pha
c Rehana Yusaf (1995 Aug 23) 1.1.3; minor cosmetic change to RBN_GP
c Rehana Yusaf (1995 Dec 13) 1.1.5; add wtinfo and friends
c Rehana Yusaf (1996 Jan 2) 1.1.6; bug-fix in pha_rd
c Rehana Yusaf (1996 Feb 5) 1.1.7; bug-fix in rbn_comp
c Rehana yusaf (1996 Feb 22) 1.1.8; added cmpmode to wt_com
c
c Banashree M Seifert (1996, March) 2.0.0:
c              . added dynamic memory allocation
c              . relaxed the arraysize from fixed array size to
c                variable array size
c              . introduced screen display subroutines
c              . Filesize *180
c              . check for instrument and telescope is introduced
c                such that it will warn if telescope and instrument
c                are other than ASCA and SIS0/SIS1
c              . added option to read a binning file
c              . added properr = yes or no
c              . added prompt for error type -- POISS-1, etc.
c
c Banashree M Seifert (1996, Nov8) 2.1.0:
c              . added provision to define the first channel in output
c                to be defined by user to be one of the following three 
c                 % --> same as input file (by default)
c                 1 --> fchan=1
c                 0 --> fchan=0
c                so added one subroutine ADJUST_CHANNEL
c                For this a new parameter is added in parameter file
c                as fchan so that on command line it can be
c                RBNPHA FCHAN=% or RBNPHA FCHAN=1 or RBNPHA FCHAN=0
c                this fchan is read as fchan_out in gp subroutine
c Peter D Wilson (1998, June 23) 2.1.1:
c              . Mods to make it compatible with new extended filenames
c Peter D Wilson (June 29, 1998) 2.1.2:
c              . Added max_xflt parameter to rdpha1 function call
c kaa (June 6, 2001) 2.2.0:
c                Added support for vector AREASCAL and BACKSCAL
c Irby (Aug 10, 2006) 2.2.1:
c                Initialize n_ill before using it in call to ftrtnm.
c                   
c --------------------------------------------------------------------
c --- DYNAMIC MEMORY ALLOCATION ---
c  the following MEM common block definition is in the system iraf77.inc
c  file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c ----------------------------------------------------------------------+

      character(5) version
      parameter (version = '2.2.1')
      character(7) taskname
      parameter (taskname='rbnpha')
*-
c ---------------------------------------------------------------------
      subinfo ='using '//taskname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- GET PARAMETERS ---

      errflg = 0
      call rbn_gp(infile,binfile,outfile,finchan,cmpmode,properr,
     >            error,killit,file,fchan_out,errflg,chatter)

      IF (errflg.NE.0) THEN
        goto 80
      ENDIF

c --- USER INFO ---

      nckeys = 0
      task = 'rbnpha'
      call wtbegm(taskname,version,chatter)

      call open_pha(infile,phsize,errflg,chatter)
      if (errflg .ne. 0) then
          subinfo='returning from open_pha'
          call wterrm(taskname,version,subinfo)
          goto 80 
      endif
 
c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      p_serr = 0
      p_syserr = 0
      p_rcts = 0
      p_ascale = 0
      p_bscale = 0
      p_channel = 0
      p_counts = 0
      p_qualty = 0
      p_grping = 0
      p_conv = 0

      iget=0
      status = 0
      if(phsize .lt. 50) phsize=50
      call udmget(phsize, 6, p_serr, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phsize*4

      status = 0
      call udmget(phsize, 6, p_syserr, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phsize*4

      status = 0
      call udmget(phsize, 6, p_rcts, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phsize*4

      status = 0
      call udmget(phsize, 6, p_ascale, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phsize*4

      status = 0
      call udmget(phsize, 6, p_bscale, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phsize*4

      status = 0
      call udmget(phsize, 4, p_channel, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phsize*4

      status = 0
      call udmget(phsize, 4, p_counts, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phsize*4

      status = 0
      call udmget(phsize, 4, p_qualty, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phsize*4

      status = 0
      call udmget(phsize, 4, p_grping, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phsize*4

      status = 0
      call udmget(phsize, 3, p_conv, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+phsize*4


      ineed = 8*phsize*4

      write(subinfo, '(a,i10)')'DMAsize required for this task=', ineed
           call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,i10)')'total bytes of memory I get   =',iget
           call wtinfo(chatter,10,1,subinfo)

 50   if (status .ne. 0) then
          errflg = -1
          subinfo='failed to allocate dynamic memory '
          call wtinfo(chatter,0,1,subinfo)
          goto 80 
      endif

c --- READ PHA FILE ---
c
      cont = .true.
      call pha_rd(infile,MEMI(p_channel),MEMI(p_counts),dtype,
     >            qerror,MEMR(p_serr),qsys,MEMR(p_syserr),qqual,
     >            MEMI(p_qualty),qgroup,MEMI(p_grping),qascale,
     &            MEMR(p_ascale),qbscale,MEMR(p_bscale),
     &            MEMR(p_rcts),nchan,detchans,phsize,texpos,shkeys,
     >            skeys,ckeys,cform,ckrec,keydesc,nckeys,
     &            hduclas2,phaversn,fchan,lchan,extnum,ctype,
     &            instrum,tlscop,dmode,cont,chatter)
      
      IF (.NOT.cont) THEN
        subinfo='returning from reading pha file'
        call wterrm(taskname,version,subinfo)
        errflg=1
        goto 80
      ENDIF
      IF (errflg.NE.0) goto 80
c 
c --------------------------------------------------------------
c if file=T, then read the binning info
c if file=F, then lookfor cmpmode and compression factor
c ---------------------------------------------------------------
c     case for file=.TRUE.

      if(file) then 
         cmpmode='NONE'
         call read_binfile(binfile,finchan,detchans,fchan,
     >                     lchan,nfacts,stbins,endbins,cfacts,
     >                     maxcomp,errflg,chatter)    

         if(errflg .ne. 0) then
            suBinfo='returning from reading binfile'
            call wterrm(taskname,version,subinfo)
            goto 80
         endif


      else

c     case for file=.false.

      if((cmpmode(1:1) .eq. 'B') .or. (cmpmode(1:1) .eq. 'F')) then 
         if(tlscop(1:4) .ne. 'ASCA') then
            subinfo='Compression mode is designed for ASCA SIS'
            call wtwarm(taskname,version,chatter,0,subinfo) 
            subinfo= 'input PHA is from '
            call wtinfo(chatter,0,0,subinfo)
            subinfo='TELESCOPE='//tlscop
            call wtinfo(chatter,0,1,subinfo)
            subinfo= 'INSTRUMENT='//instrum
            call wtinfo(chatter,0,1,subinfo)
         endif
      endif

c --- COMPRESSION FACTOR(s) ---

      call cmp_fact(finchan,tlscop,instrum,nchan,dmode,cmpmode,
     &              MEMI(p_channel),detchans,fchan,
     &              nfacts,stbins,endbins,cfacts,task,errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 80
      ENDIF 

      endif
c -------------------------------------------------------------------
c    end of getting bining info
c -------------------------------------------------------------------
c
c --- COMPRESS ---
c
      p_tcts = 0
      p_trcts = 0
      p_tserr = 0
      p_tsyserr = 0
      p_tascale = 0
      p_tbscale = 0
      status = 0
      call udmget(phsize, 4, p_tcts, status)
      if (status .ne. 0) goto 50
      call udmget(phsize, 6, p_trcts, status)
      if (status .ne. 0) goto 50
      call udmget(phsize, 6, p_tserr, status)
      if (status .ne. 0) goto 50
      call udmget(phsize, 6, p_tsyserr, status)
      if (status .ne. 0) goto 50
      call udmget(phsize, 6, p_tascale, status)
      if (status .ne. 0) goto 50
      call udmget(phsize, 6, p_tbscale, status)
      if (status .ne. 0) goto 50

      call rbn_comp(MEMI(p_channel),MEMI(p_counts),MEMR(p_rcts),
     >              dtype,
     >              qerror,MEMR(p_serr),qsys,MEMR(p_syserr),
     >              qqual,MEMI(P_qualty),qgroup,MEMI(p_grping),
     >              qascale,MEMR(p_ascale),qbscale,MEMR(p_bscale),
     >              nchan,phsize,nfacts,stbins,endbins,cfacts,
     >              properr,error,finchan,errflg,chatter,MEMI(p_tcts),
     >              MEMR(p_trcts),MEMR(p_tserr),MEMR(p_tsyserr),
     >              MEMR(p_tascale),MEMR(p_tbscale))
      IF (errflg.NE.0) THEN
        goto 80
      ENDIF  

      CALL udmfre(p_tcts, 4, status)
      CALL udmfre(p_trcts, 6, status)
      CALL udmfre(p_tserr, 6, status)
      CALL udmfre(p_tsyserr, 6, status)
      CALL udmfre(p_tascale, 6, status)
      CALL udmfre(p_tbscale, 6, status)

c
c --- WRITE OUTPUT FILE ---
c
      n_comm = 0
      do i=1, nfacts
        n_comm = n_comm + 1
        write(cstart,100,IOSTAT = cerr) stbins(i)
        write(cend,100,IOSTAT = cerr) endbins(i)
        write(compchar,100,IOSTAT = cerr) cfacts(i)
        bincomm = ' Data rebinned from '//cstart//' TO '
     &//cend//' with a compression factor : '//compchar
        call rmvexsp(bincomm,comms(i))
      enddo


c before writing to output file, to check what is the first channel
c user wants and set channel1 according to that

      if(fchan_out .eq. '%') then
         channel1=fchan
      elseif(fchan_out .eq. '0') then
         channel1=0
      elseif(fchan_out .eq. '1') then
         channel1=1
      else
         subinfo='first channel should be either  % /0 /1'
         call wterrm(taskname,version,subinfo)
         errflg=1
         goto 80
      endif

      n_comm=n_comm+1
      write(subinfo,'(a,i2)')' With user defined first channel as', 
     >                        channel1
      comms(n_comm)=subinfo

c now adjust the channel array so that it starts from channel1
c and not from channel1=1

      call adjust_channel(MEMI(p_channel),channel1,nchan,
     >                    errflg,chatter)

      if(errflg .ne. 0)then
         subinfo='returning from adjust_channel'
         call wterrm(taskname,version,subinfo)
         goto 80
      endif

c now channels are adjusted and so write to output

      call wt_com(infile,outfile,hduclas2,phaversn,channel1,extnum,
     &            MEMI(p_channel),MEMI(p_counts),MEMR(p_rcts),
     &            dtype,qerror,MEMR(p_serr),qsys,MEMR(p_syserr),
     >            qqual,MEMI(p_qualty),qgroup,MEMI(p_grping),
     &            qascale,MEMR(p_ascale),qbscale,MEMR(p_bscale),nchan,
     >            finchan,MEMS(p_conv),phsize,version,task,n_comm,
     >            comms,ckeys,cform,ckrec,keydesc,nckeys,killit,
     >            cmpmode,ctype,errflg,chatter)

c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_serr, 6, status)
      status = 0
      call udmfre(p_syserr, 6, status)
      status = 0
      call udmfre(p_rcts, 6, status)
      status = 0
      call udmfre(p_ascale, 6, status)
      status = 0
      call udmfre(p_bscale, 6, status)
      status = 0
      call udmfre(p_channel, 4, status)
      status = 0
      call udmfre(p_counts, 4, status)
      status = 0
      call udmfre(p_qualty, 4, status)
      status = 0
      call udmfre(p_grping, 4, status)
      status = 0
      call udmfre(p_conv, 3, status)

      if (status .ne. 0) then
          errflg=1
          subinfo= 'failed to de-allocate memory '
          call wterrm(taskname,version,subinfo)
      endif

  80  call wtendm(taskname,version,errflg,chatter)
 100  FORMAT(I8)
      return
      end
c ----------------------------------------------------------------------
c     END OF MAIN RBNPHA
c ----------------------------------------------------------------------
    
*+RBN_GP
c     ------------------------------------------------------
      subroutine rbn_gp(infile,binfile,outfile,finchan,cmpmode,
     &                  properr,error,killit,file,fchan_out,
     >                  errflg,chatter)
c     ------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,binfile,outfile,cmpmode
      character(180) filename
      character(180) ill_files(5)
      integer errflg,chatter,finchan,n_ill
      integer status, extnum
      character(270) desc
      logical ext,val_cmp,valfil,killit,file
      integer flen, fcstln
      character(180) tbinfile
      character*(*) fchan_out
     
      character*(*) error
      logical properr
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c infile     char   : input file name
c outfile    char   : Output filename
c finchan    int    : Resultant number of channels in outfile
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
c Rehana Yusaf (1993 July 12)
c Rehana Yusaf (1993 Oct 19) 1.0.1; Add new parameter cmpmode to define
c                                   type of compression
c Rehana Yusaf (1994 Sept 13) 1.0.2; additional parameter is read; clobber
c Rehana Yusaf (1995 April 28) 1.0.3; increase ill_files and filename
c                                     to 160
c Rehana Yusaf (1995 Dec 13) 1.0.4; Add wtinfo and friends
c
c Banashree M Seifert(1996 March) 2.0.0:
c            . Made filenames as *180
c            . added properr and error type
c
c Banashree M Seifert(1996 Nov 8) 2.1.0:
c            . added one parameter fchan_out
c              this parameter gives the user option to define the first
c              channel in the output file to be 0 or 1
c              This is a hidden parameter and fchan_out=% by default in
c              which case it will take the first channel no. to be
c              same as input file
c Peter D Wilson(1998 June 23) 2.1.1:
c            . Removed INQUIRE test on input file... problems with new
c              extended syntax
c            . Call ftrtnm to strip off extension before calling ck_file
c -------------------------------------------------------------------
      character(6) subname
      parameter (subname='rbn_gp')
      character(5) version 
      parameter (version = '2.1.1')
*-
c ---------------------------------------------------------------------
c INITIALIZE

      n_ill = 0
c

c GET INFILE

      status = 0
      call uclgst('infile',infile,status)
      IF (status.NE.0) THEN
        desc='getting infile parameter'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(infile)
      IF (infile.EQ.'  ') THEN
        errflg = 1
        desc='input PHA file not entered'
        call wterrm(subname,version,desc)
        return
      ENDIF
C PDW 6/23/98: Leave this for FTOPEN to determine
C      call fcpars(infile,filename,extnum,status)
C      ext = .true.
C      flen = fcstln(filename)
C      INQUIRE(FILE=filename(:flen),EXIST=ext)
C      IF (.NOT.ext) THEN
C        errflg = 1
C        desc = 'file does not exist:'//filename
C        call wterrm(subname,version,desc)
C        return
C      ENDIF

c GET BINFILE

      file = .false.
      call uclgst('binfile',binfile,status)
      if (status .ne. 0) then
        desc = 'reading binfile parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      endif
      call crmvlbk(binfile)
      tbinfile = binfile
      call ftupch(tbinfile)
      if((tbinfile .ne. 'NONE') .and. (tbinfile .ne. '  '))then
          flen = fcstln(binfile)
          INQUIRE(FILE=binfile(:flen),EXIST=ext)
          if (.NOT. ext) then
            errflg = 1
            desc = 'File does not EXIST :'//binfile
            call wterrm(subname,version,desc)
            return
          endif
          file = .true.
          n_ill = n_ill + 1
          ill_files(n_ill) = binfile
      endif

c GET CLOBBER

      call uclgsb('clobber',killit,status)
      IF (status.NE.0) THEN
        killit = .false.
        desc = 'getting clobber value '
        call wterrm(subname,version,desc)
      ENDIF

c GET CHATTER

      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        desc = 'getting chatter parameter '
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF

c get the first channel no user wants to be  --> defalut= '%'

      status = 0
      call uclgst('fchan',fchan_out,status)
      IF (status.NE.0) THEN
        desc = 'getting first channel parameter '
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF

c If binfile is not supplied then look for FINCHAN and CMPMODE
c     GET FINCHAN 
    
      if(.not. file) then 
         status = 0
         call uclgsi('finchan',finchan,status)
         IF (status.NE.0) THEN
           desc = 'getting finchan parameter'
           call wterrm(subname,version,desc)
           errflg = 1
           return
         ENDIF       

c     GET CMPMODE

         status = 0
         call uclgst('cmpmode',cmpmode,status)
         IF (status.NE.0) THEN
           desc = 'getting cmpmode parameter !'
           call wterrm(subname,version,desc)
           errflg = 1
           return
         ENDIF           

c     CHECK CMPMODE VALIDITY

         call ftupch(cmpmode)
         call crmvlbk(cmpmode)
         val_cmp = .false.
         IF (cmpmode(1:1).EQ.'F') THEN
           val_cmp = .true.
           cmpmode = 'FAINT2BRIGHT'
         ELSEIF (cmpmode(1:1).EQ.'L') THEN
           val_cmp = .true.
           cmpmode = 'LINEAR'
         ELSEIF (cmpmode(1:1).EQ.'B') THEN
           val_cmp = .true.
           cmpmode = 'BRIGHT2LINEAR'
         ENDIF
         IF (.NOT.val_cmp) THEN
           desc = 'invalid cmpmode' 
           call wterrm(subname,version,desc)
           desc = 'valid cmpmodes - linear,faint2bright and '
     >            //'bright2linear'
           call wtinfo(chatter,0,1,desc)
           errflg = 1
           return
         ENDIF 
      endif

c GET OUTFILE 

      status = 0
      call uclgst('outfile',outfile,status)
      IF (status.NE.0) THEN
        desc='getting outfile parameter'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF

c OUTFILE VALIDATION

      call crmvlbk(outfile)
      IF (outfile.EQ.'  ') THEN
        desc = 'outfile must be entered'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF
      n_ill = n_ill+1
C PDW 6/23/98: Call ftrtnm to strip off extension number before calling ck_file
C     ill_files(n_ill) = infile
      call ftrtnm(infile, ill_files(n_ill), status)
      call ck_file(outfile,ill_files,n_ill,valfil,
     &             killit,chatter)
      IF (.NOT.valfil) THEN
        errflg = 2
        return
      ENDIF

c ----------------- read in properr -----------------------

      status = 0
      call uclgsb('properr', properr, status)
      if (status .ne. 0) then
          desc='getting error prop. parameter'
          call wterrm(subname,version,desc)
          properr=.true.
      endif

c ----------------- type of error -------------------------
      if(.not. properr) then
         status = 0
         call uclgst('error', error,status)
         if (status .ne. 0) then
             desc='getting type of error'
             call wterrm(subname, version,desc)
             errflg=1
             return
         endif
      endif

      call crmvlbk(error)
      call ftupch (error)

c end of reading par file -------------------------

      return
      end
c ---------------------------------------------------------------------
c     END OF RBN_GP
c ---------------------------------------------------------------------

*+OPEN_PHA

      subroutine open_pha(infile,phsize,errflg,chatter)
c -------------------------------------------------------------------
c This routine opens the PHA file and gets the PHA size from it
c ------------------------------------------------------------------

      implicit none
      character*(*) infile
      integer phsize,errflg,chatter

c -------------------- internal variables ----------------------------
      integer iunit,nsearch, status,next(50),ninstr
      character(100) subinfo,comm
      character(20) instr(50),outhdu(9,50),outver(9,50),extname
      character(20) extnames(9,50)

c -------------- Authors/modifications ----------------------------
c Banashree Mitra Seifert (1996 March) 1.0.0:
c -----------------------------------------------------------------
      character(9) subname
      character(5) version

      parameter(version='1.0.0')
      subname ='open_pha'
      subinfo = 'using '//subname//version
      call wtinfo(chatter,10,1,subinfo)

c ----------- opening pha file -----------------------------------------

      ninstr = 1
      instr(1) = 'SPECTRUM'
      extname='SPECTRUM'
      nsearch = 50
      status = 0

      call mvext(0,infile,iunit,ninstr,instr,nsearch,next,outhdu,
     &         extnames,outver,extname,status,chatter)

       if (status .ne. 0) then
           subinfo='opening the input PHA file'
           call wtferr(subname,version,status,subinfo)
           call ftclos(iunit,status)
           subinfo = 'closing input PHA file'
           call wtferr(subname,version,status,subinfo)
           errflg=1
           return
       endif

c read naxis2 (array size)

       status = 0
       call ftgkyj(iunit,'NAXIS2',phsize,comm,status)
        subinfo = 'reading NAXIS2'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif

       call ftclos(iunit,status)
       if(status .ne. 0) then
          subinfo='closing PHA file'
          call wterrm(subname,version,subinfo)
       endif

       write(subinfo,'(a,i10)')'PHA array size = ',phsize
       call wtinfo(chatter,10,1,subinfo)

       return
       end

c --------------------------------------------------------------
c                 end of open_pha
c --------------------------------------------------------------

*+ADJUST_CHANNEL

      subroutine adjust_channel(channel,channel1,nchan,errflg,chatter)

c -----------------------------------------------------------------
c This subroutine adjust the channel no as required by user
c -----------------------------------------------------------------

c ------------------ variables -----------------------------
      implicit none
      integer channel(*),channel1,nchan,errflg,chatter

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
      call wtinfo(chatter,10,2,subinfo)

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
         endif
      endif
 
      errflg = 0
      return
      end

c ------------------------------------------------------------------
c                end of adjust_channel 
c ----------------------------------------------------------------- 

*+RBN_COMP
c     -----------------------------------------------------------
      subroutine rbn_comp(channel,counts,rcts,dtype,qerror,serr,
     &               qsys,syserr,qqual,qualty,qgroup,grping,
     &               qascale,ascale,qbscale,bscale,nchan,
     &               phsize,nfacts,stbins,endbins,cfacts,
     >               properr,error,finchan,
     &               errflg,chatter,tcts,trcts,tserr,tsyserr,
     &               tascale,tbscale)      
c     -----------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c This subroutine compresses the data by a given compression factor(s)
c ---------------------------------------------------------------------
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE 
      integer phsize, errflg, chatter, finchan
      integer nfacts, stbins(*), endbins(*), cfacts(*)
      integer counts(*), channel(*), dtype, nchan
      integer qualty(*), grping(*), tcts(*)
      real rcts(*), serr(*), syserr(*), ascale(*), bscale(*)
      real trcts(*), tserr(*), tsyserr(*), tascale(*), tbscale(*)
      logical qerror, qqual, qgroup, qsys, qascale, qbscale

      character*(*) error
      logical properr

c
c --- LOCAL VARIABLES ...
c
      integer new, stbin, endbin,l,ch_cnt
      integer stnew, endnew, nbin, i, j, k,stchan,endchan
      real sumerr, sumsys
      logical badqual, curbin,findchan
      logical findchan2,chan_srch,misbin
      character(80) desc

      real calcpois,calcpois2,calcpois3
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1993 July 13)
c Rehana Yusaf (1993 Oct 19) 1.0.1; Update so that the channel array
c                                   values are used NOT the index
c Rehana Yusaf (1993 Oct 20) 1.0.2; Missing channels handled correctly
c Rehana Yusaf (1994 Feb 23) 1.0.3; BUGFIX, terminating curbin condition
c                                   changed from - stchan.GE.endbin
c                                   to           - stchan.GT.endbin
c                                   to cope with compression factor of 1
c Rehana Yusaf (1995 Dec 13) 1.0.4 ; add wtinfo and friends
c Rehana Yusaf (1996 Jan 5) 1.0.5; bug-fix j =1 not j = stchan
c                                  this is a problem if stchan is 0
c
c Banashree Mitra Seifert (1996 June) 2.0.0:
c           . Made variable arrays to carry dimension from
c             calling routines
c           . modified error calculation
c             a. corrected the systematic error calculation
c             b. added propagation of error
c kaa (2001 June 6) 2.1.0:
c           . Added support for vector AREASCAL and BACKSCAL
c ---------------------------------------------------------------
       character(9) subname
       parameter (subname='rbn_comp')
       character(5) version
       parameter (version = '2.1.0')
*-
c ---------------------------------------------------------------------
c
c --- USER INFO ---
c  
      desc = ' using '//subname//' '//version
      call wtinfo(chatter,10,1,desc)
c
c --- INITIALISATION ---
c
      do i=1,nchan
        tcts(i) = counts(i)
        trcts(i) = rcts(i)
        tserr(i) = serr(i)
        tsyserr(i) = syserr(i)
        tascale(i) = ascale(i)
        tbscale(i) = bscale(i)
        counts(i) = 0
        rcts(i) = 0
        serr (i) = 0
        syserr(i) = 0
        ascale(i) = 0.
        bscale(i) = 0.
      enddo

        stnew = 0
        endnew = 0
c
c --- COMPRESS DATA ---
c
      new = 1
      ch_cnt = 0
      j = 1
      do i=1,nfacts
        stbin = stbins(i)
        endbin = endbins(i)
        stchan = stbin
        nbin = cfacts(i)
     
        if(nbin .lt. 0) then
           curbin=.false.
        else
           curbin = .true.
        endif

         do WHILE(curbin)
          sumsys = 0
          sumerr = 0
          endchan = stchan + nbin - 1
          badqual = .false.

c FIND CHANNEL ARRAY INDEX ...

          findchan = .false.
          findchan2 = .false.
          chan_srch = .true.
          do WHILE(chan_srch)
            IF (channel(j).EQ.stchan) THEN
              findchan = .true.
              stnew = j
            ENDIF
            IF (channel(j).EQ.endchan) THEN
              findchan2 = .true.
              endnew = j
            ENDIF
            IF ((.NOT.findchan2).AND.
     &        (channel(j).GT.endchan)) THEN
              chan_srch = .false.
            ELSE
              j = j + 1
            ENDIF 
            IF ((findchan).AND.(findchan2)) THEN
              chan_srch = .false.
            ELSEIF ((.NOT.findchan).AND.findchan2) THEN
              chan_srch = .false.
            ENDIF
          enddo  
          misbin = .false.

c STCHAN AND ENDCHAN NOT FOUND ...

          IF ((.NOT.findchan).AND.(.NOT.findchan2)) THEN
            misbin = .true.

c STCHAN FOUND BUT ENDCHAN NOT FOUND ... 

          ELSEIF ((findchan).AND.(.NOT.findchan2)) THEN
            l = stnew 
            do WHILE(.NOT.findchan2)
              IF ((channel(l).GE.stchan).AND.
     &           (channel(l+1).GT.endchan)) THEN
                 endnew = l
                 findchan2 = .true.
                 qqual = .true.
                 badqual = .true.
               ENDIF
               l = l + 1
             enddo

c ENDCHAN FOUND BUT STCHAN NOT  FOUND ...

           ELSEIF ((.NOT.findchan).AND.(findchan2)) THEN
             l = 1 
             do WHILE(.NOT.findchan)
               IF ((channel(l).GE.stchan).AND.
     &              (channel(l+1).GE.endchan)) THEN
                  stnew = l
                  findchan = .true.
                  qqual = .true.
                  badqual = .true.
                ENDIF
                l = l+1
              enddo
    
c NO MISSING CHANNEL FOR THIS BIN CASE ...

           ELSE
              endnew = stnew + nbin - 1
          ENDIF
          IF (.NOT.misbin) THEN
           ch_cnt = ch_cnt + 1
           channel(ch_cnt) = new

           do k = stnew, endnew

c REBIN COUNTS/RATE

            IF (dtype.EQ.1) THEN
              counts(ch_cnt) = counts(ch_cnt) + tcts(k)
            ELSE
              rcts(ch_cnt) = rcts(ch_cnt) + trcts(k)
            ENDIF

c REBIN ERRORS IF PRESENT

            IF (qerror) THEN
                if(properr) then
                   sumerr = sumerr + tserr(k)**2
                endif
            ENDIF

c REBIN SYSTEMATIC ERRORS IF PRESENT 

            IF (qsys) THEN
              IF (dtype.EQ.1) THEN
                sumsys = sumsys + (tsyserr(k)*tcts(k))
              ELSE
                sumsys = sumsys + (tsyserr(k)*trcts(k))
              ENDIF
            ENDIF

c CHECK FOR ANY BAD QUALITY CHANNELS, (STILL INCLUDED, QUALITY SET TO BAD)

            IF (qqual) THEN
              IF (qualty(k).NE.0) THEN
                badqual = .true.
              ENDIF
            ENDIF

c AVERAGE THE AREASCAL and BACKSCAL

            ascale(ch_cnt) = ascale(ch_cnt) + tascale(k)
            bscale(ch_cnt) = bscale(ch_cnt) + tbscale(k)

          enddo

          ascale(ch_cnt) = ascale(ch_cnt) / (endnew-stnew+1)
          bscale(ch_cnt) = bscale(ch_cnt) / (endnew-stnew+1)

          IF (qqual) THEN
            IF (badqual) THEN
              qualty(ch_cnt) = 5
            ELSE
              qualty(ch_cnt) = 0
            ENDIF
          ENDIF
          IF (qerror) THEN

c             for error propagation, error=sqrt(e1^2+e2*2+....)

              if(properr) then
                 IF (sumerr.NE.0) THEN
                   serr(ch_cnt) = SQRT(sumerr)
                 ELSE
                   serr(ch_cnt) = sumerr
                 ENDIF

c             for no error propagation, calculate POISS-1,POISS-2 etc

              else
                 if (error .eq. 'GAUSS') then
                     if(dtype .eq. 1) then
                        serr(ch_cnt) = 1./sqrt(real(counts(ch_cnt)))
                     else
                        serr(ch_cnt) = 1./sqrt(rcts(ch_cnt))
                     endif
                 elseif (error .eq. 'POISS-1') then
                        if(dtype .eq. 1) then
                           serr(ch_cnt) = calcpois (counts(ch_cnt))
                        else
                           serr(ch_cnt) = calcpois (INT(rcts(ch_cnt)))
                        endif
                 elseif (error .eq. 'POISS-2') then
                        if(dtype .eq. 1) then
                           serr(ch_cnt) = calcpois2 (counts(ch_cnt))
                        else
                           serr(ch_cnt) = calcpois2 (INT(rcts(ch_cnt)))
                        endif
                 elseif (error .eq. 'POISS-3') then
                        if(dtype .eq. 1) then
                           serr(ch_cnt) = calcpois3 (counts(ch_cnt))
                        else
                           serr(ch_cnt) = calcpois3 (INT(rcts(ch_cnt)))
                        endif
                 else
                        call wterrm(subname,version,
     >                      'type of error calculation is not defined')
                 endif
              endif

          ENDIF
          IF (qsys) THEN
            IF (dtype.EQ.1) THEN
              IF ((counts(ch_cnt).NE.0).AND.(sumsys.NE.0)) THEN
                syserr(ch_cnt) = sumsys/counts(ch_cnt)
              ELSE
                syserr(ch_cnt) = sumsys
              ENDIF
            ELSE
              IF ((rcts(ch_cnt).NE.0).AND.(sumsys.NE.0)) THEN
                syserr(ch_cnt) = sumsys/rcts(ch_cnt)
              ELSE
                syserr(ch_cnt) = sumsys
              ENDIF
            ENDIF
           ENDIF
          ENDIF 
          stchan = endchan + 1
          IF (stchan.GT.endbin) THEN
            curbin = .false.
          ENDIF
          new = new + 1
        enddo
      enddo
      nchan = ch_cnt 
      qgroup = .false.
      return
      end
c --------------------------------------------------------------------
c     END OF RBN_COMP
c --------------------------------------------------------------------

*+READ_BINFILE
c     ------------------------------------------------------
      subroutine read_binfile(binfile,finchan,detchans,
     &       fchan,lchan,nfacts,stbins,endbins,cfacts,
     &       maxcomp,errflg,chatter)
c     ------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c This subroutine reads binning info from an ascii file
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      character*(*) binfile
      integer finchan,detchans,fchan,maxcomp,nfacts,lchan
      integer stbins(maxcomp),endbins(maxcomp),cfacts(maxcomp)
      integer errflg,chatter
c
c --- VARIABLE DIRECTORY ------------------------------------------
c
c fchan    int  : starting channel
c binfile  char : binning filename
c finchan  int  : Number of channels after compression
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
c Banashree Mitra Seifert (1996 June) 1.0.0; 
c       . This routine is added from RBNRMF to check the validity 
c         of the channels used for rebinning
c --------------------------------------------------------------------
      character(13) subname     
      parameter (subname='read_binfile')
      character(5) version
      parameter (version = '1.0.0')
c
*-
c -------------------------------------------------------------------
c --- INTERNALS ---

      character(70) subinfo,errinfo
      character(8) chanchar,chanchar2,comp
      integer st_bin,end_bin,c_fact,i,iunit,status
      integer prev_stbin,prev_endbin,curchans
c
c --- USER INFO ---
c
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)
c
c --- OPEN BINFILE ---
c
      call ftgiou(iunit,errflg)
      IF (errflg.NE.0) THEN
        errinfo = 'problem getting free lun'
        call wterrm(subname,version,errinfo)
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
          subinfo = 'invalid number in file'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
        ENDIF
        IF ((nfacts + 1).GT.maxcomp) THEN
          subinfo = ' The compression array sizes have been exceeded'
          call wtinfo(chatter,0,1,subinfo)
          write(subinfo,'(a,i12)')' The max array dimension is ',maxcomp
          call wtinfo(chatter,0,1,subinfo)
          errflg = 3
          goto 200
        ENDIF
        IF (MOD((end_bin - st_bin + 1),c_fact).NE.0) THEN
          subinfo = ' Compression factor is not exact divisor of'
          call wtinfo(chatter,0,1,subinfo)
          subinfo = ' starting and ending channels for this bin'
          call wtinfo(chatter,0,1,subinfo)
          write(errinfo,350)st_bin,end_bin
          call rmvexsp(errinfo,subinfo)
          call wtinfo(chatter,0,1,subinfo)
          write(errinfo,400) c_fact
          call rmvexsp(errinfo,subinfo)
          call wtinfo(chatter,0,1,subinfo)
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
          subinfo = 'The compression array sizes have been exceeded'
          call wtinfo(chatter,0,1,subinfo)
          write(subinfo,'(a,i12)')' The max array dimension is ',maxcomp
          call wtinfo(chatter,0,1,subinfo)
          errflg = 3
          goto 200
        ENDIF
        nfacts = nfacts + 1
        stbins(nfacts) = endbins(nfacts - 1) + 1
        endbins(nfacts)= lchan
        cfacts(nfacts) = 1
      ENDIF

c USER INFO

      finchan = 0
      do i=1,nfacts
         write(chanchar,300,IOSTAT=status) stbins(i)
         write(chanchar2,300,IOSTAT=status) endbins(i)
         write(comp,300,IOSTAT=status) cfacts(i)
         errinfo = chanchar//' - '//chanchar2
     &//' are to be binned with compression factor '//comp
         call rmvexsp(errinfo,subinfo)
         call wtinfo(chatter,20,1,subinfo)
         curchans = ((endbins(i) - stbins(i)+1)/cfacts(i))
        IF (curchans.GT.0) THEN
          finchan = finchan + curchans
        ENDIF
      enddo
      write(subinfo,'(a,i8)')' final number of channels :',finchan
      call wtinfo(chatter,20,1,subinfo)

  200 close(unit=iunit)
      status = 0
      call ftfiou(iunit,status)
  300 FORMAT(I8)
  350 FORMAT(' starting channel:',I8,' ending channel:',I8)
  400 FORMAT(' with compression factor:',I8)
      return
      end

c ----------------------------------------------------------------
c                   END OF CMP_BINFILE
c -----------------------------------------------------------------

*+CK_BINCHAN
c     -----------------------------------------------------
      subroutine ck_binchan(stchan,endchan,lchan,fchan,
     &                      prev_stchan,prev_endchan,
     &                      perr,chatter)

c --- DESCRIPTION ----------------------------------------------------
c
c This routine checks the validity of the channels used for rebinning.
c
c --- VARIABLES ------------------------------------------------------

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

