*     +GRPPHA
      subroutine grppha
c     -----------------
c --- DESCRIPTION --------------------------------------------------
c 
c This is the main task routine of a program which manipulates the
c contents of an OGIP format FITS PHA file. This program is 
c similiar to CHANPHA, which manipulates the 'old' style PHA files.
c 
c    
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character(70) shkeys(40)
      character(180) infile,outfile
      character(20) hduclas2,instrum,tlscpe,dmode
      character(5) phaversn      
      character(11) ctype
      character(100) subinfo
      character(8) ckeys(64)
      character(10) cform(64)
      character(132) ckrec(64)
      character(60) keydesc(64)
      real texpos
      integer phsize,detchans
      integer chatter,skeys,nckeys
      integer dtype,nchan,extflag
      integer fchan,lchan,extnum
      integer errflg,iget,ineed,status,i
      integer p_channel,p_counts,p_qualty,p_grping,p_conv
      integer p_serr,p_syserr,p_rcts,p_ascale,p_bscale
      logical qerror,qsys,qqual,qgroup,cont,killit
      logical qascale, qbscale
c   
c --- VARIABLE DIRECTORY -------------------------------------------
c
c infile     char   : input file name (user defined)
c outfile    char   : Output filename (user defined)
c chatter    int    : Chattines flag, >20 verbose (user defined)
c subinfo    char   : User Routine/Version information
c phsize     int    : Array dimension
c channel    int    : Array of channels 
c counts     int    : Array of observed counts 
c rcts       real   : Array of count rate 
c dtype      int    : datatype, 1 if counts,2 if rcts
c qualty     int    : Array of quality flag
c grping     int    : Array of grouping flag
c serr       real   : Observed statistical error
c syserr     real   : Fractional systematic error
c ascale     real   : Array of areascal values
c bscale     real   : Array of backscal values
c qgroup     logical: True if data is grouped
c qqual      logical: True if data has quality flags
c qerror     logical: True if statistical errors included
c qsys       logical: True if systematic errors included
c qascale    logical: True if vector areascal
c qbscale    logical: True of vector backscal
c nchan      int    : Number of detector channels
c cont       logical: False if program is to be terminated
c texpos     real   : Exposure time
c conv       int*2  : Conversion array, to convert from i*4 to i*2
c
c --- CALLED ROUTINES ----------------------------------------------
c
c subroutine PHA_GP    : Get user defined parameters
c subroutine PHA_RD    : (CALLIB) Read Pha file, and display header
c subroutine PHA_COM   : Obtain, and obeys user commands
c subroutine FCECHO    : (FTOOLS) Standalone screen write
c
c --- COMPILATION AND LINKING --------------------------------------
c
c Link with FTOOLS,CALTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1993 March 30)
c Rehana Yusaf (1993 May 25) :Change made to warn user that existing
c                             files will not be overwritten.
c
c Rehana Yusaf (1993 June 17) : Changes made to allow truncated
c                               command strings. SHOW display is
c                               changed.
c Rehana Yusaf (1993 June 21) : File overwrite is allowed, using
c                               !filename
c Rehana Yusaf (1993 June 28) : SHOW KEYS, mandatory keyword 
c                               display is added to SHOW command
c                               family
c Rehana Yusaf (1993 July 19) : WT_COM, and all the routines, it
c                               calls have been added to CALLIB.
c                               RFTSPF has also been added. Thus
c                               they have been removed fron the
c                               main source file.
c Rehana Yusaf (1993 August 2) 1.0.6: extract CK_FILE and generalise
c Rehana Yusaf (1993 Oct 18) 1.07: Changes have been made to several
c                               subroutines -
c                               PHA_RD calls RDPHA1 instead of RFTSPF
c                               SET_QUAL has been updated so that
c                               the Channel array index is not used 
c                               but the value stored is used
c                               SYS_COM is updated so that the 
c                               channel array value is used as
c                               described above 
c Rehana Yusaf (1993 Nov 21) 1.0.8; BAD PARSER (called from bad_com,
c			        which is called from pha_com) has
c			        been updated to remove a bug. PHA_RD
c                               has been updated to use FNDHDU and
c                               FNDEXT - this moves to the approriate
c			        extension and warns the user if more than
c				one SPECTRUM extension is found - also
c				INFILE[extnum] can be used to specify the
c                               desired extension. PHA_COM has been updated
c                               to pass additional HDUCLASS keywords. 
c Rehana Yusaf (1994 Jan 10) 1.0.9;
c   GT_COM a new subroutine has been added for additional funtionality. It
c     allows GRPPHA to be used in a non-interactive form (ie from XSELECT)
c     The command line can contain commands from more than one command family
c     by seperating them with an ampersand '&', for example
c     GROUP 1 4096 8 & BAD 1-4000,4096 & exit NEW.PHA
c   DISP_MIS has been added to display missing channels in a setting
c   FNDCHAN has been added, to find channels in a setting
c   SYNTAX changes - commas are allowed as seperators as well as spaces
c   SYSTEMATICS has been made consistent with the other commands by allowing
c   SYS MINCHAN-MAXCHAN ERROR as well as SYS CHANNEL ERROR
c   SH_COM has been updated NOT to USE large arrays for display purposes
c   
c Rehana Yusaf (1994 March 14) 1.1.0; bug-fix, there was a problem setting
c                                     bad minchan-maxchan where maxchan ==
c                                     detchans, and sys minchan-maxchan
c                                     SYS_COM and SET_QUAL updated.
c Rehana Yusaf (1994 June 22) 1.1.1;. bug-fix in SH_COM, grouping display
c                                   . update SYS_PAR,GRP_PAR,GB_PAR
c                                     to use cgetlun instead of iunit=5
c                                     as 5 is standard output and XPI
c                                     is confused. 
 
c Rehana Yusaf (1994 July 20)1.1.2; . Main reason for enhancement is
c                                     to allow longer filenames for
c                                     CHKEY commands. A fitsio
c                                     continuation convention is 
c                                     used to cope with filenames > 68
c                                     WT_COM (CALLIB) was also updated
c                                    . PHA_COM; command length increased
c                                      to 136
c                                    . PHA_RD; manadatory keyword display
c                                      updated, description omitted, if
c                                      keyword value is too long.
c                                    . UPDATE; mandatory display change
c                                    . GRP_PAR,SYS_PAR,GB-PAR
c                                      all updated to allow 6 settings
c                                      on command line NOT 3.
c                                    . CHK_COM, max string length is 120
c                                    . HLP_* updated to reflect changes
c                                    . SET_KEY updated
c                                    . PAR_COM and PAR_SUB updated for
c                                      longer command length
c                                    . SH_COM has SHOW INFILE & SHOW CHKEYS
c                                    . in addition cgetlun has been
c                                      replaced by fitsio ftgiou/ftfiou
c Rehana Yusaf (1994 Sept 8)         . bug-fix in chk_com
c                                    . read clobber parameter (pha_com)
c                                    . ck_file has additional argument
c                                
c Rehana Yusaf (1995 Jan 27)2.0.0;   . add dump command
c                                    . increase infile to 80
c                                    . allow infile to be overwritten
c                                    . 2 new routines, DUMP_COM
c                                      HLP_DUMP
c Rehana Yusaf (1995 March 22) 2.0.1;. add space between channel range
c                                      when dumped, update to DUMP_COM
c Rehana Yusaf (1995 April 28) 2.0.2;  increase infile and outfile
c                                      length, this involves an update
c                                      to pha_gp, pha_com,pha_rd,sh_com
c                                      sys_com,gb_par,par_com,sub_par,chk_com
c                                      dump_com 
c Rehana Yusaf (1995 Aug 27) 2.0.3;    improve non-interactive mode
c                                      by adding additional hidden comm
c                                      parameter - see pha_gp
c                                      improve warnings in grpm_com,grp_par
c                                      and ck_chan, use channel value not
c                                      array index
c Rehana Yusaf (1996 Feb 22) 2.0.4;    added ' ' to wt_com call - for
c                                      cmpmode argument
c Banashree Mitra Seifert (1996, March) 2.1.0:
c              . added dynamic memory allocation
c              . relaxed the arraysize from fixed array size to
c                variable array size
c              . introduced screen display subroutines
c              . few more extra bug eradicated
c Banashree Mitra Seifert (1996, July) 2.2.0:
c              . added extnum=next(1) in pha_rd subroutine so that
c                the output file doesnot write the original
c                spectrum extension
c 
c Banashree Mitra Seifert (1996, July) 2.3.0:
c              . ctype included so that it writes in the output file the
c                CHANTYPE from input. If input doent have CHANTYPE then
c                it writes CHANTYPE=UNKNOWN
c              . another problem fixed in ck_pha [key(1:8) instead of key]
c
c Banashree M Seifert (1996, Oct29) 2.4.0:
c              . show keywords command was not showing changed keywds
c                this is corrected
c              . after the task is done, it writes to screen the output
c                filename as requested by one of the users
c
c Banashree M Seifert (1996, Nov13) 2.5.0:
c              . open_pha1 subroutine is changed 
c
c Banashree M Seifert (1996, Dec) 2.6.0:
c              . grp_par subroutine status was replaced by errflg
c
c Banashree Mitra Seifert (1997, August) 2.7.0:
c       . Introduced case for last channel when command is 
c         "show grouping"
c       . Also, i=1,nchan+1 is replaced by i=1,nchan
c        (these two modifications are done in the subroutine sh_com only)
c Peter D Wilson (1998 June 23) 2.8.0:
c       . Updated for new extended filename syntax
c Peter D Wilson (June 29, 1998) 2.8.1:
c        . Added max_xflt parameter to rdpha1 function call
c Peter D Wilson (Apr 1, 1999) 2.8.2:
c        . Initialize the damn errflg so it works under IRIX
c toliver (July 22, 1999) 2.9.0:
c        . Use dynamic allocation for local array in subroutine grp_com
c kaa (June 6, 2001) 3.0.0:
c        . Added support for vector AREASCAL and BACKSCAL
c        . PHA_RD and SET_KEY routines moved to CALLIB
c kaa (Oct 10, 2005) 3.0.1
c        . Fixed so bad channels that were written with grouping of 0
c        . now have grouping of 1. This for conformity with standard.
c --------------------------------------------------------------------

c --- DYNAMIC MEMORY ALLOCATION ---
c  the following MEM common block definition is in the system iraf77.inc
c  file
      INTEGER*4        MEMI(100)
      REAL             MEMR(100)
      EQUIVALENCE (MEMI, MEMR)
      COMMON /MEM/ MEMR
c ----------------------------------------------------------------------+
      character(5) version
      parameter (version = '3.0.1')
      character(7) taskname
c      COMMON/task/taskname
      taskname = 'grppha'
*-
c ------------------------------------------------------------------
c 
c --- GET PARAMETERS ---
c
      errflg = 0
      call wtbegm(taskname,version,chatter)

      extflag = 0
      call pha_gp(infile,outfile,extflag,killit,chatter)
      IF (extflag.NE.0) THEN
          subinfo='returning from pha_gp'
          call wterrm(taskname,version,subinfo)
          goto 100
      ENDIF

      call open_pha1(infile,phsize,errflg,chatter)
      if (errflg .ne. 0) then
          subinfo='returning from open_pha1'
          call wterrm(taskname,version,subinfo)
          goto 100
      endif

c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      p_serr = 0 
      p_syserr = 0
      p_rcts = 0
      p_channel = 0
      p_counts = 0
      p_qualty = 0
      p_grping = 0
      p_conv = 0 
      p_ascale = 0
      p_bscale = 0

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
      call udmget(phsize, 4, p_conv, status)
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


 50   ineed = 8*phsize*4 

      write(subinfo, '(a,i10)')'DMAsize required for this task=', ineed
           call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,i10)')'total bytes of memory I get   =',iget
           call wtinfo(chatter,10,1,subinfo)

      if (status .ne. 0) then
          errflg = -1
          subinfo='failed to allocate dynamic memory '
          call wtinfo(chatter,0,1,subinfo)
          goto 100
      endif
   

c --- READ PHAFILE ---

      cont = .true.
      call pha_rd(infile,MEMI(p_channel),MEMI(p_counts),dtype,
     >            qerror,MEMR(p_serr),qsys,MEMR(p_syserr),qqual,
     >            MEMI(p_qualty),qgroup,MEMI(p_grping),qascale,
     &            MEMR(p_ascale),qbscale,MEMR(p_bscale),
     &            MEMR(p_rcts),nchan,detchans,phsize,texpos,shkeys,
     >            skeys,ckeys,cform,ckrec,keydesc,nckeys,
     &            hduclas2,phaversn,fchan,lchan,extnum,ctype,
     &            instrum,tlscpe,dmode,cont,chatter)
      
      IF (.NOT.cont) THEN
        subinfo='returning from reading pha file'
        call wterrm(taskname,version,subinfo)
        errflg=1
        goto 100
      ENDIF

c --- WRITE THE FILE INFO

      DO i = 1, skeys
         call wtinfo(chatter,0,0,shkeys(i))
      ENDDO

c --- READ AND OBEY COMMANDS ---

      call pha_com(infile,outfile,MEMI(p_channel),MEMI(p_counts),
     >             dtype,qerror,MEMR(p_serr),qsys,MEMR(p_syserr),
     >             qqual,MEMI(p_qualty),qgroup,MEMI(p_grping),
     &             qascale,MEMR(p_ascale),qbscale,MEMR(p_bscale),
     >             MEMR(p_rcts),nchan,detchans,MEMI(p_conv),
     &             phsize,texpos,shkeys,skeys,ckeys,cform,ckrec,
     &             keydesc,nckeys,version,
     &             hduclas2,phaversn,fchan,extnum,ctype,errflg,
     >             killit,chatter)

      if(errflg .ne. 0) then
         subinfo='error returning from pha_com'
         call wterrm(taskname,version,subinfo)
         errflg=1
         goto 100 
      endif

c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_serr, 6, status)
      status = 0
      call udmfre(p_syserr, 6, status)
      status = 0
      call udmfre(p_rcts, 6, status)
      status = 0
      call udmfre(p_channel, 4, status)
      status = 0
      call udmfre(p_counts, 4, status)
      status = 0
      call udmfre(p_qualty, 4, status)
      status = 0
      call udmfre(p_grping, 4, status)
      status = 0
      call udmfre(p_conv, 4, status)
      status = 0
      call udmfre(p_ascale, 6, status)
      status = 0
      call udmfre(p_bscale, 6, status)

      if (status .ne. 0) then
          subinfo= 'failed to de-allocate memory '
          call wterrm(taskname,version,subinfo)
          errflg=1
      endif

 100  call wtendm(taskname,version,errflg,chatter)
      return
      end
c -------------------------------------------------------------------
c     END OF MAIN TASK 
c -------------------------------------------------------------------

*+PHA_GP
c     --------------------------------------------------------
      subroutine pha_gp(infile,outfile,extflag,killit,chatter)
c     --------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c This routine obtains user defined parameters
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,outfile
      integer chatter,extflag
      logical killit
c
c --- INTERNALS -----------------------------------------------------
c
      integer status
      character(70) subinfo
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c infile     char   : Input filename
c outfile    char   : Output filename, default, no output file
c chatter    int    : Chattiness flag, >20    
c extflag    int    : If extflag = 0,okay if 1, infile does not exist
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine UCLGST : (HOST) Used to read string parameter
c subroutine UCLGSI : (HOST) Used to read integer parameter 
c subroutine FCECHO : (FTOOLS) Standalone screen write
c
c --- COMPILATION AND LINKING ---------------------------------------
c
c Link with FTOOLS,CALTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 March 30)
c Rehana Yusaf (1993 May4) : Read default,defval1, value of outfile 
c                            parameter and then write it to outfile
c                            in par file. This is to work around the
c                            SAO host interface feature which always
c                            learns the parameter value.
c Rehana Yusaf (1993 May 25): Warn user if outfile already exists.
c                             Also the default value GRPPHA.OUT,
c                             no longer serves a purpose, instead
c                             a blank default is used.
c Rehana Yusaf (1993 Nov 3) 1.0.3; If infile not entered then return
c	                      to main
c Rehana Yusaf (1994 Sept 13) 1.0.4; read clobber, and pass back as killit
c Rehana Yusaf (1995 Jan 30) 1.0.5; outfile existence is not tested
c Rehana Yusaf (1995 April 28) 1.0.6; increase curfile to 160
c
c Banashree Mitra Seifert (1996, March) 1.1.0:
c          . Introduced screen display routine
c          . filename *180
c Peter D Wilson (1998 June 23) 1.1.1:
c          . Remove INQUIRE test for infile... fails with new extended
c            filename syntax
c --------------------------------------------------------------------

      character(8) subname
      character(5) version
      parameter (version = '1.1.1')
*-
c -------------------------------------------------------------------

      subname='pha_gp'
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- GET INPUT FILE NAME ---

      extflag = 0
      status = 0
      call uclgst('infile',infile,status)
      IF (status.NE.0) THEN
        subinfo = ' getting infile parameter'
        call wterrm(subname,version,subinfo)
        extflag = 1
        return 
      ENDIF
      call crmvlbk(infile)
      IF (infile.EQ.'    ') THEN
        subinfo = 'input file must be entered !'
        call wterrm(subname,version,subinfo)
        extflag = 1
        return 
      ENDIF
      status = 0

c
c --- GET OUTPUT FILENAME ---
c
      status = 0
      call uclgst('outfile',outfile,status)
      IF (status.NE.0) THEN
        subinfo = 'getting outfile parameter'
        call wterrm(subname,version,subinfo) 
        extflag = 1
        return
      ENDIF
      call crmvlbk(outfile)
c
c --- GET CHATTER FLAG ---
c
      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        subinfo = 'getting chatter parameter'
        call wterrm(subname,version,subinfo)
        extflag = 1
        return
      ENDIF
c
c --- GET CLOBBER ---
c
      status = 0
      call uclgsb('clobber',killit,status)
      IF (status.NE.0) THEN
        subinfo = 'getting clobber'
        call wterrm(subname,version,subinfo)
        extflag = 1
        return
      ENDIF
      return
      end

c ------------------------------------------------------------------
c                      END OF PHA_GP 
c ------------------------------------------------------------------

*+OPEN_PHA1

      subroutine open_pha1(infile,phsize,errflg,chatter)

c --------------------------------------------------------------------
c this routine opens the PHA file and reads the PHA size
c ---------------------------------------------------------------------
      implicit none
      character*(*) infile
      integer phsize,errflg,chatter


c -------------------- internal variables ----------------------------
      integer iunit,nsearch, status,next(50),ninstr, i,htype
      integer extno
      character(100) subinfo,comm
      character(20) instr(50),outhdu(9,50),outver(9,50),extname
      character(20) extnames(50)

c --------------- author/modifications -------------------------------
c Banashree M Seifert (1996 May) 1.0.0:
c
c Banashree M Seifert (1996 Nov 13)1.1.0:
c          . if nore than one HDUCLAS1=SPECTRUM IS FOUND then move to
c            the extension which matches extname
c -----------------------------------------------------------------
      character(9) subname
      character(5) version
      parameter(version='1.1.0')
      subname ='open_pha1'
*_

      subinfo = 'using '//subname//version
      call wtinfo(chatter,10,1,subinfo)

c ----------- opening pha file -----------------------------------------

       status = 0
       extname='SPECTRUM'
       ninstr = 1
       instr(1) = 'SPECTRUM'
ccc       instr(2) = 'TOTAL'
       nsearch = 50

       call mvext (0, infile, iunit, ninstr, instr, nsearch, next,
     >             outhdu, extnames, outver, extname, status, chatter)

c      status=2 --> more than one extensions found

       if(status .eq. 2) then
          i=1
          extno=0
          do while (next(i) .ne. 0) 
             if(extnames(i)(1:8) .eq. extname) then
                extno=extno +1
             endif
             i=i+1
          enddo
          if(extno .gt. 1) then
             subinfo='more than one SPECTRUM extension found'
             call wterrm(subname,version,subinfo)
             subinfo='you should specify the extension number with'
     >               //' input file'
             call wtinfo(chatter,0,0,subinfo)
             subinfo='e.g. infile[extno]'
             call wtinfo(chatter,0,0,subinfo)
             errflg=1
             return
          else
             status=0
             call ftmahd(iunit,extno+1,htype,status)
             if (status. ne. 0) then
                 subinfo = 'Problem moving to '//extname
     &                     //'xtens'
                 call wterrm(subname,version,subinfo)
                 errflg = 4
                 return
             endif
          endif
       endif

       if ((status .ne. 0) .and. (status .ne. 2)) then
           subinfo='opening the input PHA file'
           call wtferr(subname,version,status,subinfo)
           status=0
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
c                 end of open_pha1
c --------------------------------------------------------------

*+PHA_COM

      subroutine pha_com(infile,outfile,channel,counts,dtype,
     &                 qerror,serr,qsys,syserr,qqual,qualty,
     &                 qgroup,grping,qascale,ascale,qbscale,
     &                 bscale,rcts,nchan,detchans,conv,phsize,
     &                 texpos,shkeys,skeys,ckeys,cform,ckrec,
     &                 keydesc,nckeys,mnver,
     &                 hduclas2,phaversn,fchan,extnum,ctype,errflg,
     >                 killit,chatter) 

c --- DESCRIPTION -------------------------------------------------
c
c This subroutine reads the user command and calls the appropriate
c routine to execute it.
c
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,outfile,shkeys(*),ckrec(*),phaversn
      character*(*) cform(*), ckeys(*), keydesc(*),hduclas2
      character(160) tfile,template
      character(70) comms
      character(6) task
      integer dtype,phsize,chatter,nchan,skeys,nckeys
      integer channel(*),counts(*),n_comm,fchan,extnum
      integer grping(*),qualty(*),errflg,detchans
      integer nprompt
ccc      integer*2 conv(*)
      integer conv(*)
      real rcts(*),serr(*),syserr(*),ascale(*),bscale(*),texpos
      logical qerror,qsys,qqual,qgroup,killit,create,com_create
      logical qascale,qbscale
      character(5) mnver
      character(5) tname
      character*(*) ctype
c
c --- INTERNALS ---------------------------------------------------
c
      character(172) command,sep_com(20)
      character(516) comm
      character(160) ill_files(5),curfile
      character(70) subinfo
      character(20) com_strgs(20),sub_coms(20)
      integer status,n_com,n_sub,n_ill,ncomms,i
      logical end_com,val_com,valfil
c
c --- VARIABLE DIRECTORY ------------------------------------------
c
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
c subinfo     char  : Subroutine info for user
c qualty      int   : Array of qualty flag
c grping      int   : Array of grouping flag
c serr        real  : Array of Observed statistical errors
c syserr      real  : Array of Fractional systematic error
c qascale     logical: True if vector areascal
c ascale      real  : Array of areascal values
c qbscale     logical: True if vector backscal
c bscale      real  : Array of backscal values
c com_strgs   char  : Array of allowed commands
c sub_coms    char  : Array of sub-commands used with SHOW and RESET
c n_com       int   : No. of commands
c n_sub       int   : No. of sub commands
c val_com     logical: true of valid command
c qgroup      logical: True if data is grouped
c qqual       logical: True if data has qualty flags
c qerror      logical: True if statistical errors included
c qsys        logical: True if systematic errors included
c shkeys      char   : Array containing mandatory keywords and description
c skeys       int    : counter for shkeys
c
c Internals ...
c
c comm        char   : Command string
c command     char   : Command string after extra spaces are removed
c errinfo     char   : error information for user
c desc        char   : Used for string output to screen
c curfile     char   : Variable used for current file to write to
c errstr      char   : Routine error string
c subinfo     char   : Subroutine information
c status      int    : Error flag for getting command parameter
c end_com    logical : True when user exits or quits
c ill_files   char   : Array of file(s) that cannot be overwritten
c n_ill       int    : Number of files that cannot be overwritten
c ext        logical : True if file exists
c valfil      char   : true if valid output filename
c
c --- CALLED ROUTINES ---------------------------------------------
c
c UCLGST             : (HOST) Get command parameter
c UCPLST             : (HOST) Put command parameter
c SET_COM            : Sets commands array
c PAR_COM            : Command parser (allowing truncation)
c GRPM_COM           : Executes GROUP MIN command
c GRP_COM            : Executes GROUP MINCHAN MAXCHAN NUMCHAN comm'
c SYS_COM            : Executes the SYSTEMATICS command
c BAD_COM            : Executes the BAD command,sets quality to bad
c GOOD_COM           : Executes the GOOD command,sets quality to good
c SH_COM             : Shows users the current status
c RSET_COM           : Executes reseting of columns
c WT_COM             : Write to specified file, and continue
c INQUIRE            : (FORTRAN) does file exist ?
c SET_COM            : Sets up array with all command strings
c PAR_COM            : Command family parser
c CK_FILE            : Checks validity of output filename
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1993 March 30)
c Rehana Yusaf (1993 May 4) : defval2, the default value of GRPPHA
c                             prompt is read, and then written to
c                             'comm' parameter in the PAR file                
c Rehana Yusaf (1993 May 25): User is warned that if a file already
c                             exists, it will not be overwritten.
c Rehana Yusaf (1993 June 14): Subroutines SET_COM and PAR_COM are
c                              added.
c Rehana Yusaf (1993 june 21): Subroutine CK_FILE added, checks 
c                              output filename validity
c                              file overwrite is allowed.
c Rehana Yusaf (1993 June 28): SHOW KEY added to show family    
c Rehana Yusaf (1993 August 2): 1.0.5;CK_FILE made more general
c Rehana Yusaf (1994 Jan 10) 1.0.6; GT_COM added to allow several
c                             command families on the command line
c			      ie GROUP 1 10 2 & BAD 20-30 & exit NEW.PHA
c                             GT_COM seperates the commands. This
c                             additional functionality has been introduced
c                             mainly for XSELECT to spawn GRPPHA in a 
c                             non-interactive manner.
c Rehana Yusaf (1994 July 15)1.0.7; command length increased to 396
c                                   and individual commands to 136
c Rehana Yusaf (1994 Sept 13) 1.0.8; ck_file has additional argument, killit
c Rehana Yusaf (1995 Jan 28) 1.0.9;  add dump_com, new command
c                                    allow infile to be overwritten
c Rehana Yusaf (1995 April 28) 2.0.0; increase filenames to 160
c                                     increase command to 172 (was 136)
c                                     increase comm to 516 (was 396)
c Rehana Yusaf (1995 Aug 28) 2.0.1;   When XPI reads comm from the
c                                     command line, it only does it
c                                     once. This can result in a
c                                     command being executed forever.
c                                     The second time around a differant
c                                     parameter is read - comm2
c Banashree Mitra Seifert (1996 March) 2.1.0:
c          . introduced screen display routine
c Peter D Wilson (1998 June 23) 2.1.1:
c          . Must call ftrtnm when testing if outfile is same as infile
c kaa (2001 June 6) 2.1.2:
c          . Added support for vector AREASCAL and BACKSCAL
c
c -----------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '2.1.2')
*-
c -----------------------------------------------------------------

      subname = 'pha_com'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

      task = 'grppha'
      tname = 'grppha'
      n_comm = 0
      n_ill = 0
      com_create = .false.
c
c --- GET USER COMMAND ---
c
      nprompt = 0
      call set_com(com_strgs,n_com,sub_coms,n_sub)
      end_com = .false.
      do WHILE(.NOT.end_com)
         curfile = '  '
         status = 0
         IF (nprompt.GT.0) THEN
           call uclgst('tempc',comm,status)
           nprompt = nprompt + 1
         ELSE
           call uclgst('comm',comm,status)
           nprompt = nprompt + 1
         ENDIF
         IF (status.NE.0) THEN
           subinfo = 'getting command parameter'
           call wterrm(subname,version,subinfo) 
           errflg=1
           return
         ENDIF
         status = 0

         call gt_com(comm,sep_com,ncomms,chatter)
         do i=1,ncomms
           call par_com(sep_com(i),com_strgs,n_com,command,val_com,
     >                  chatter)
cccc           IF ((command(1:10).EQ.'GROUP MIN ')THEN 
           IF ((command(1:10).EQ.'GROUP MIN ') .or. 
     >         (command(1:10).EQ.'GROUP min '))THEN

            call grpm_com(command,qualty,counts,
     &                  rcts,dtype,grping,qgroup,
     &     qqual,nchan,phsize,texpos,channel,chatter)
           ELSEIF (command(1:6).EQ.'GROUP ') THEN
            call grp_com(command,channel,grping,phsize,
     &                   qgroup,nchan,detchans,chatter)
           ELSEIF (command(1:12).EQ.'SYSTEMATICS ') THEN
            call sys_com(command,channel,syserr,phsize,nchan,
     &                   detchans,qsys,chatter)
           ELSEIF (command(1:4).EQ.'BAD ') THEN
            call bad_com(command,channel,qqual,qualty,nchan,
     &              detchans,phsize,chatter)
           ELSEIF (command(1:5).EQ.'GOOD ') THEN
            call good_com(command,channel,qqual,qualty,nchan,
     &              detchans,phsize,chatter)  
           ELSEIF (command(1:5).EQ.'SHOW ') THEN
            call sh_com(command,qsys,syserr,qqual,qualty,
     &              qgroup,grping,channel,nchan,phsize,
     &              sub_coms,n_sub,shkeys,skeys,cform,ckeys,
     &              ckrec,nckeys,infile,chatter)
           ELSEIF (command(1:6).EQ.'RESET ') THEN
            call rset_com(command,qsys,syserr,qqual,qualty,
     &                    qgroup,grping,nchan,phsize,
     &                    sub_coms,n_sub,chatter)
           ELSEIF (command(1:5).EQ.'CHKEY') THEN
            call chk_com(command,ckeys,cform,ckrec,keydesc,
     &                   nckeys,chatter)
           ELSEIF (command(1:4).EQ.'DUMP') THEN
            call dump_com(command,qsys,syserr,qqual,qualty,
     &                    qgroup,grping,channel,nchan,phsize,
     &                    sub_coms,n_sub,killit,infile,chatter)
           ELSEIF (command(1:4).EQ.'QUIT ') THEN
            end_com=.true.
           ELSEIF (command(1:5).EQ.'WRITE') THEN
            curfile = command(6:)
            call crmvlbk(curfile)
            IF (curfile.EQ.'  ') THEN
              subinfo=' Filename required with WRITE command -'
     >             //' Try Again'
              call wtinfo(chatter,10,2,subinfo)
            ELSE
C PDW 6/23/98: Call ftrtnm to strip off any extension number on infile
             call ftrtnm(infile,template,errflg)
             IF ((curfile.EQ.template).OR.
     &        ((curfile(1:1).EQ.'!').AND.(curfile(2:).EQ.template)))THEN
                call cr_tmpfile(template,killit,curfile,tfile,
     &                          tname,create,errflg,chatter)
                IF (errflg.GT.0) THEN
                  subinfo ='Try this command with a differant filename'
                  call wtinfo(chatter,10,2,subinfo)
                  goto 99
                ELSE
                  errflg = 0
                ENDIF
              ELSE
                  create=.false.
              ENDIF
              n_ill = 0
              call ck_file(curfile,ill_files,n_ill,valfil,
     &                     killit,chatter)
              IF (valfil) THEN
                IF (create) THEN
                  template = tfile
                ELSE
                  template = infile
                ENDIF
                errflg = 0

                call wt_com(template,curfile,hduclas2,phaversn,fchan,
     &                  extnum,channel,counts,rcts,
     &                  dtype,qerror,serr,
     &                  qsys,syserr,qqual,qualty,qgroup,grping,
     &                  qascale,ascale,qbscale,bscale,
     &                  nchan,detchans,conv,phsize,mnver,task,n_comm,
     &                  comms,ckeys,cform,ckrec,keydesc,
     &                  nckeys,killit,' ',ctype,errflg,chatter)
                IF (errflg.EQ.0) THEN
                  subinfo ='Changes written to file : '//curfile
                  call wtinfo(chatter,10,2,subinfo)
                ELSE
                  subinfo = ' Error writing to file !'
                  call wtinfo(chatter,10,2,subinfo)
                ENDIF
                IF (create) THEN
                  infile = tfile
                  create = .false.
                  com_create = .true.
                ENDIF
              ENDIF 
            ENDIF
           ELSEIF (command(1:4).EQ.'EXIT') THEN
            curfile = command(5:)
            call crmvlbk(curfile)
            IF (curfile.NE.'  ') THEN
              outfile = curfile
            ENDIF
            call crmvlbk(outfile)
            IF (outfile.EQ.'  ') THEN
              subinfo = 'filename not entered'
              call wtinfo(chatter,10,2,subinfo)
              subinfo = 'try EXIT with a filename'
              call wtinfo(chatter,10,2,subinfo)
            ELSE
C PDW 6/23/98: Call ftrtnm to strip off any extension number on infile
             call ftrtnm(infile,template,errflg)
              IF ((outfile.EQ.template).OR.
     &           ((outfile(1:1).EQ.'!').AND.
     >            (outfile(2:).EQ.template))) THEN
                  call cr_tmpfile(template,killit,outfile,tfile,
     &                          tname,create,errflg,chatter)
                  IF (errflg.GT.0) THEN
                    subinfo='try this command with a differant filename'
                    call wtinfo(chatter,10,2,subinfo)
                    goto 99
                  ELSE
                    errflg = 0
                  ENDIF
              ELSE
                 create=.false.
              ENDIF
              call ck_file(outfile,ill_files,n_ill,valfil,
     &                     killit,chatter)
              IF (valfil) THEN
                IF (create) THEN
                  template = tfile
                ELSE
                  template = infile
                ENDIF
                errflg = 0

                call wt_com(template,outfile,hduclas2,phaversn,fchan,
     &                  extnum,channel,counts,rcts,
     &                  dtype,qerror,serr,
     &                  qsys,syserr,qqual,qualty,qgroup,grping,
     &                  qascale,ascale,qbscale,bscale,
     &                  nchan,detchans,conv,phsize,mnver,task,n_comm,
     &                  comms,ckeys,cform,ckrec,keydesc,
     &                  nckeys,killit,' ',ctype,errflg,chatter)   
                IF (errflg.EQ.0) THEN
                 subinfo='exiting, changes written to file : '
     >                   //outfile
                 call wtinfo(chatter,0,2,subinfo)
                ELSE
                 subinfo='exiting, error in writing to final'
                 call wtinfo(chatter,0,2,subinfo)
                ENDIF
                IF (create) THEN
                  infile = tfile
                  create = .false.
                  com_create = .true.
                ENDIF
                end_com = .true.
              ENDIF
            ENDIF
           ELSEIF (command(1:4).EQ.'HELP') THEN
            call hlp_com(command,com_strgs,n_com,chatter)
           ELSEIF (command(1:2).NE.'  ') THEN
            subinfo='invalid command : '//sep_com(i)
            call wtinfo(chatter,10,2,subinfo)
            subinfo=' try again !'
            call wtinfo(chatter,10,2,subinfo)
           ENDIF
   99   enddo
      enddo
      IF (com_create) THEN
        call delfil(infile)
      ENDIF
      return
      end
c -------------------------------------------------------------------
c     END OF PHA_COM      
c -------------------------------------------------------------------           


*+GRP_COM
      subroutine grp_com(command,channel,grping,phsize,
     &                   qgroup,nchan,detchans,chatter)

c --- DESCRIPTION ---------------------------------------------------
c
c This subroutine executes the GROUP MINCHAN MAXCHAN NUMCHAN command.
c If a grouping overlaps with a previous grouping, the new grouping 
c starts after the overlap if applicable.  
c 
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE 
      character*(*) command
      integer phsize,nchan,chatter,detchans
      integer channel(*),grping(*)
      logical qgroup
c
c --- INTERNALS -----------------------------------------------------
c
      character(100) subinfo,sub 
      character(8) cnum,cnum2
      integer stbin,endbin,i,j,k,nocom
      integer maxgcom,ierr,stchan,endchan 
      integer l,perr,pbinst,pbinend
      parameter (maxgcom = 500)
      integer minchan(maxgcom),maxchan(maxgcom),numchan(maxgcom)
      logical findchan,finbinn,prev_bin,ck_bin,findchan2,mis_inbin
      integer maxmiss,nmiss,curchan
      parameter (maxmiss = 10)
      integer missarray(maxmiss)
      character(11) com_syn
      
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c Arguments ...
c
c command     char  : User input command string
c phsize      int   : Array dimension
c nchan       int   : Detchans, read from pha file
c chatter     int   : Chatter flag, >20 verbose
c channel     int   : Array of channels
c grping      int   : Array of grouping flag
c qgroup     logical: True if grouping any flags are set
c       
c
c --- CALLED ROUTINES ---
c
c GRP_PAR           : Parses the command string,and returns error
c                     if syntax is incorrect
c FCECHO            : (FTOOLS) Write to screen
cc
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 March 30)
c Rehana Yusaf (1993 June 22) : Correct group overlap check !
c Rehana Yusaf (1993 Oct 19) 1.0.2; Check for leading missing
c                                  channels
c Rehana Yusaf (1993 Dec 9) 1.0.3; Check for missing channels
c				   throughout dataset and 
c                                  additional parameter passed
c                                  to this subroutine - DETCHANS
c Rehana Yusaf (1995 Jan 28) 1.0.4;   increase maxgcom to 500 (from 100)
c
c Banashree Mitra Seifert (1996, March)1.1.0:
c       . introduces screen display routines
c       . format changed from I4 to I8 
c toliver (1999 July 22) 1.2.0; Dynamically allocate bk_grp to match size
c                               of grping array (it was hard-wired at 5012
c                               elements)
c -------------------------------------------------------------------

c --- DYNAMIC MEMORY ALLOCATION ---

      INTEGER*4        MEMI(100)
      REAL             MEMR(100)
      EQUIVALENCE (MEMI, MEMR)
      COMMON /MEM/ MEMR

      integer bk_grp, status
c ----------------------------------------------------------------------+

      character(8) subname
      character(5) version
      parameter (version = '1.2.0')
*-
c -------------------------------------------------------------------
 
      subname = 'grp_com'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

      stbin=0
      endbin=0
      status = 0
c
c     The "4" in the second parameter requests an integer array be created
c
      bk_grp = 0
      call udmget(nchan, 4, bk_grp, status)
      if (status .ne. 0) then
          subinfo='failed to allocate dynamic memory for bk_grp array'
          call wtinfo(chatter,0,1,subinfo)
      endif

c
c --- CALLING GROUP PARSER ---
c
      ierr = 0
      perr = 0
      call grp_par(command,minchan,maxchan,numchan,nocom,
     &             nchan,chatter,perr,channel,ierr)
      IF ((perr.NE.0).OR.(ierr.NE.0)) THEN
        subinfo = 'Command not executed, Try again !'
        call wtinfo(chatter,10,2,subinfo)
        subinfo = ' SYNTAX :  GROUP MINCHAN MAXCHAN NUMCHAN'
        call wtinfo(chatter,10,2,subinfo)
        subinfo = ' OR     :  GROUP MINCHAN-MAXCHAN NUMCHAN'
        call wtinfo(chatter,10,2,subinfo)
        subinfo = ' FOR EXAMPLE : GROUP 10 100 10'
        call wtinfo(chatter,10,2,subinfo)
        subinfo = ' see ''HELP GROUP'''
        call wtinfo(chatter,10,2,subinfo)
        IF ((perr.NE.0).AND.(chatter.GE.30)) THEN
          call perr_wt(perr,chatter)
        ENDIF
        return
      ENDIF
c
c --- SETTING BACKUP ARRAY ---
c
      do i=1,nchan             
        MEMI(bk_grp+(i-1)) = grping(i)
      enddo  
c
c --- BINNING DATA ---
c
      com_syn = 'GROUP'
      ck_bin = .false.
      do i=1,nocom
        stchan = minchan(i)
        finbinn =.false.    
        do WHILE(.NOT.finbinn)
          endchan = stchan + numchan(i) - 1
          findchan = .false.
          findchan2 = .false.
          j = 1
          do WHILE((.NOT.findchan2).AND.(j.LE.nchan))
            IF (channel(j).EQ.stchan) THEN
              stbin = j
              findchan = .true.
            ENDIF
            IF (channel(j).EQ.endchan) THEN
              findchan2 = .true.
              endbin = j
            ENDIF
            j = j + 1
          enddo

c --- CHECK if findchan and findchan2 BUT missing channels in-between

          mis_inbin = .false.
          nmiss = 0
          curchan = stchan + 1
          IF (findchan.AND.findchan2) THEN
            do k=stbin+1,endbin
              IF (channel(k).NE.curchan) THEN
                do l=curchan,(channel(k) - 1)
                   nmiss = nmiss + 1
                   IF (nmiss.LE.maxmiss) THEN
                     missarray(nmiss) = l
                   ENDIF
                 enddo
                 mis_inbin = .true.
              ENDIF
              curchan = channel(k) + 1 
            enddo
          ENDIF
          IF ((mis_inbin).AND.(chatter.GT.0)) THEN
            call disp_mis(missarray,maxmiss,nmiss,stchan,endchan,
     &                    com_syn,chatter)
            write(sub,700) stchan,endchan
            call rmvexsp(sub,subinfo) 
            call wtinfo(chatter,0,2,subinfo)
          ENDIF

c --- PRINT APPROPRIATE ERROR IF MISSING CHANNELS ...
   
           IF ((.NOT.findchan).OR.(.NOT.findchan2)) THEN

c --- Check if stbin channel and endbin channel NOT FOUND

            IF ((.NOT.findchan).AND.(.NOT.findchan2)) THEN
              write(subinfo,400) stchan,endchan
              call rmvexsp(subinfo,sub)
              call wtinfo(chatter,0,2,sub) 

c --- Check if stbin NOT FOUND

            ELSEIF (.NOT.findchan) THEN
              write(subinfo,500) stchan
              call rmvexsp(subinfo,sub)
              call wtwarm(subname,version,chatter,0,sub) 
              write(subinfo,600) stchan, endchan
              call rmvexsp(subinfo,sub)
              call wtinfo(chatter,0,2,sub) 

            ELSEIF (.NOT.findchan2) THEN
                write(subinfo,500) endchan
                call rmvexsp(subinfo,sub)
                call wtwarm(subname,version,chatter,0,sub)
                write(subinfo,600) stchan, endchan
                call rmvexsp(subinfo,sub)
                call wtinfo(chatter,0,2,sub) 
            ENDIF
            stchan = endchan + 1
            
            IF ((stchan.GT.maxchan(i)).OR.(stbin.GT.nchan)) THEN
              finbinn =.true.
            ENDIF


c --- Check for spare channels, not within range

          ELSEIF ((endchan.GT.maxchan(i)).OR.(endbin.GT.nchan)) THEN
            write(cnum,200) stchan
            IF (maxchan(i).EQ.stchan) THEN
                subinfo = 'Spare Channel  : '//cnum
                call wtinfo(chatter,0,2,subinfo) 
            ELSE
              IF ((endbin.GT.nchan).AND.
     &             (maxchan(i).GT.nchan)) THEN
                write(cnum2,200) channel(nchan) 
              ELSE
                write(cnum2,200) maxchan(i)
              ENDIF
               subinfo ='Spare Channels : '//cnum//'-'//cnum2
            ENDIF
            call wtinfo(chatter,0,2,subinfo) 

            finbinn = .true.
          ELSE

c --- Check to ensure that previous binning is not overwritten ---

            prev_bin = .false.
            do l=stbin,endbin
              IF (MEMI(bk_grp+(l-1)).EQ.1) THEN
                IF (L.LT.nchan) THEN
                  IF (MEMI(bk_grp+(l)).EQ.-1) THEN
                    prev_bin = .true.
                  ENDIF
                ENDIF
              ELSEIF (MEMI(bk_grp+(l-1)).EQ.-1) THEN
                prev_bin = .true.
              ENDIF
            enddo
            IF (prev_bin) THEN
              IF (.NOT.ck_bin) THEN
                pbinst = stchan
                ck_bin = .true.
              ENDIF
              pbinend = endchan
            ELSE
              IF (ck_bin) THEN
                write(subinfo,300) pbinst,pbinend
                call rmvexsp(subinfo,sub)
                call wtinfo(chatter,0,2,sub) 
                ck_bin = .false.
              ENDIF
            ENDIF
            IF (.NOT.prev_bin) THEN
              grping(stbin) = 1
              MEMI(bk_grp+(stbin-1)) = 1
              do k=(stbin+1),endbin
                grping(k) = -1
                MEMI(bk_grp+(k-1)) = -1
              enddo
            ENDIF
          ENDIF
          stbin = endbin + 1
          stchan = endchan + 1
          IF ((stchan.GT.maxchan(i)).OR.(stbin.GT.nchan)) THEN
            finbinn =.true.
          ENDIF
        enddo
      enddo
      IF (ck_bin) THEN
          write(subinfo,300) pbinst,pbinend
          call rmvexsp(subinfo,sub)
          call wtinfo(chatter,0,2,sub) 
      ENDIF
      qgroup = .false.
      do i=1,nchan
        IF (grping(i).EQ.-1) THEN
          qgroup = .true.
        ENDIF
      enddo

c
c     Release the memory that was allocated with udmget
c
      call udmfre (bk_grp, 4, status)

  200 format(I8)
  300 format(' Overlap with pre-existing grouping for channels ',I8,
     &' - ',I8)
  400 format(' Channels ',I8,' to ',I8,' not present')
  500 format(' Channel ',I8,' not present')
  600 format(' Spare Channels ',I8,' to ',I8)
  700 format(' Grouping ',I8,' to ',I8,' is still performed')
      return
      end
c --------------------------------------------------------------------
c     END OF GRP_COM
c --------------------------------------------------------------------
          
*+SYS_COM
c     --------------------------------------------------------
      subroutine sys_com(command,channel,syserr,phsize,nchan,
     &                   detchans,qsys,chatter)
c     --------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c
c This routine adds Fractional Systematic Errors.
c
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      character*(*) command
      integer phsize,chatter,nchan,detchans
      real syserr(*)
      integer channel(*)
      logical qsys
c
c --- INTERNALS --------------------------------------------------
c
      character(80) subinfo
      integer stchan,endchan,i,k,nocom
      integer maxscom,ierr,perr
      parameter (maxscom = 50)
      integer minchan(maxscom),maxchan(maxscom)
      real err(maxscom)
      logical findchan,findchan2,setsys
      character(11) com_syn
c
c --- VARIABLE DIRECTORY -----------------------------------------
c
c
c Arguments ...
c
c command     char  : User input command string
c phsize      int   : Array dimension
c nchan       int   : Detchans, read from pha file
c chatter     int   : Chatter flag, >20 verbose
c channel     int   : Array of channels
c syserr      real  : Array of systematic errors
c qsys       logical: True if systematics errors are present
c
c --- CALLED ROUTINES --------------------------------------------
c
c SYS_PAR           : Parser for command string
c FCECHO            : Screen write
c FNDCHAN           : Determine stchan and endchan
c
c --- AUTHORS/MODIFICATION ---------------------------------------
c
c Rehana Yusaf (1993 April 13)
c
c Rehana Yusaf (1993 Oct 18th) 1.0.1; Channel array value is used
c                                     NOT the index
c Rehana Yusaf (1993 Dec 15) 1.0.2; Detchans passed to this routine
c				    missing channels handled 
c                                   FNDCHAN used to find stchan and
c				    endchan
c Rehana Yusaf (1994 March 14) 1.0.3; Terminating condition for
c                                   setsys changed - bugfix
c Banashree Mitra Seifert (1996 Mach) 1.1.0:
c          . introduced screen display routine
c ----------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c ----------------------------------------------------------------

      subname='sys_com'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- CALLING SYS PARSER ---
c
      perr = 0
      ierr = 0
      call sys_par(command,minchan,maxchan,err,nocom,
     &             nchan,chatter,perr,channel,ierr)
c      IF ((perr.NE.0).OR.(ierr.NE.0)) THEN
      IF (ierr.NE.0) THEN
        subinfo='Command not executed, Try Again !'
        call wterrm(subname,version,subinfo)
        subinfo = ' SYNTAX : SYSTEMATICS MINCHAN-MAXCHAN ERROR'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' SYNTAX2: SYSTEMATICS CHANNEL ERROR'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' FOR EXAMPLE : SYSTEMATICS 10-50 0.02 60 0.01'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' Will apply a 2% systematic error to Channels 10 to '
     &//' 50 (incl), and a '
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' 1% systematic error to channel 60'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' see ''HELP SYSTEMATICS'''
        call wtinfo(chatter,0,1,subinfo)
        IF ((perr.NE.0).AND.(chatter.GE.30)) THEN
          call perr_wt(perr,chatter)
        ENDIF
      ENDIF
      IF ((perr.NE.0).AND.(chatter.GE.30)) THEN
          call perr_wt(perr,chatter)
      ENDIF
      IF ((perr.NE.0).OR.(ierr.NE.0)) THEN
        return
      ENDIF
c
c --- FIND STARTING AND ENDING CHANNEL FOR ADDING ERROR TO ---
c
      com_syn = 'SYSTEMATICS'
      do i=1,nocom
        call fndchan(minchan(i),maxchan(i),channel,nchan,
     &               stchan,endchan,findchan,findchan2,
     &               com_syn,chatter)
c
c --- SETUP SYSERR ARRAY ---
c
        setsys = .true.
        k = 1
        IF (findchan.AND.findchan2) THEN
           do WHILE(setsys)
             IF (channel(k).EQ.stchan) THEN
              syserr(k) = err(i)
              IF (stchan.EQ.endchan) THEN
                setsys = .false.
              ENDIF
             ELSEIF ((channel(k).GT.stchan).AND.
     &       (channel(k).LE.endchan)) THEN
              syserr(k) = err(i)
              IF (channel(k).EQ.endchan) THEN
                setsys = .false.
              ENDIF
             ENDIF
             k = k+1
           enddo
        ENDIF
      enddo
      qsys =.false.
      i = 1
      do WHILE((i.LE.nchan).AND.(.NOT.qsys)) 
        IF (syserr(i).NE.(0.0)) THEN
          qsys = .true.
        ENDIF
        i = i + 1
      enddo
      return
      end
c --------------------------------------------------------------------
c     END OF SYS_COM
c --------------------------------------------------------------------

*+BAD_COM

c     -------------------------------------------------------
      subroutine bad_com(command,channel,qqual,qualty,nchan,
     &                   detchans,phsize,chatter)
c     -------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c
c This subroutine sets the bad channels. That is the quality flag
c is set to 5, as defined by OGIP standard.
c
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      character*(*) command
      integer phsize,nchan,chatter,detchans
      integer qualty(*),channel(*)
      logical qqual
c
c --- INTERNALS --------------------------------------------------
c  
      character(100) subinfo
      integer maxbcom,nocom,badint,ierr,perr,i
      parameter (maxbcom = 100)
      integer minchan(maxbcom),maxchan(maxbcom)
      
c
c --- VARIABLE DIRECTORY -----------------------------------------
c
c
c Arguments ...
c
c command     char  : User input command string
c phsize      int   : Array dimension
c nchan       int   : Detchans, read from pha file
c chatter     int   : Chatter flag, >20 verbose
c channel     int   : Array of channels
c qualty      int   : Array of qualty flags
c qqual      logical: True if any quality flags are set to non-zero
c
c --- CALLED ROUTINES --------------------------------------------
c
c GB_PAR            : Parser for command string
c FCECHO            : (FTOOLS) Write to screen
c SET_QUAL          : Sets quality to passed value, 5 here
c 
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c Rehana Yusaf (1993 April 13)
c Rehana Yusaf (1993 Oct 18) 1.0.1; Pass nchan to SET_QUAL
c Rehana Yusaf (1993 Dec 14) 1.0.2; detchans passed to this routine
c
c Banashree Mitra Seifert (1996 March)1.1.0:
c      . introduced screen display routine
c ------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c ----------------------------------------------------------------
      subname='bad_com'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- CALL GOOD/BAD PARSER ---
c
      ierr = 0
      perr = 0
      call gb_par(command,minchan,maxchan,nocom,
     &            nchan,channel,chatter,perr,ierr)
c      IF ((perr.NE.0).OR.(ierr.NE.0)) THEN
      IF (ierr.NE.0) THEN
        subinfo ='Command not executed, Try Again !'
        call wterrm(subname,version,subinfo)
        subinfo = ' SYNTAX : BAD MINCHAN-MAXCHAN '
        call wtinfo(chatter,0,1,subinfo) 
        subinfo = ' SYNTAX2 : BAD CHANNEL'
        call wtinfo(chatter,0,1,subinfo) 
        subinfo = ' FOR EXAMPLE : BAD 10-100 105'
        call wtinfo(chatter,0,1,subinfo) 
        subinfo = ' Will define channels 10 thru 100 and 105 to be bad'
        call wtinfo(chatter,0,1,subinfo) 
        subinfo = ' see ''HELP BAD'''
        call wtinfo(chatter,0,1,subinfo) 
      ENDIF
      IF ((perr.NE.0).AND.(chatter.GE.30)) THEN
        call perr_wt(perr,chatter)
      ENDIF
      IF ((perr.NE.0).OR.(ierr.NE.0)) THEN
        return
      ENDIF
c
c --- SETUP QUALITY ARRAY ---
c
      badint = 5
      call set_qual(minchan,maxchan,nocom,badint,
     &              qualty,channel,nchan,chatter)
      qqual = .false.
      do i=1,nchan
        IF (qualty(i).NE.0) THEN
           qqual = .true.
        ENDIF
      enddo
      return
      end
c ------------------------------------------------------------------
c     END OF BAD_COM
c ------------------------------------------------------------------

*+GOOD_COM
c     ---------------------------------------------------------
      subroutine good_com(command,channel,qqual,qualty,nchan,
     &                    detchans,phsize,chatter)
c     ---------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c
c This subroutine sets the quality flag to 'good', that is 0,as
c defined by OGIP standard.
c
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      character*(*) command
      integer phsize,nchan,chatter,detchans
      integer qualty(*),channel(*),i
      logical qqual
c
c --- INTERNALS --------------------------------------------------
c
      character(70) subinfo
      integer maxgcom,goodint,perr,ierr
      parameter (maxgcom = 100)
      integer minchan(maxgcom),maxchan(maxgcom)
      integer nocom
c
c --- VARIABLE DIRECTORY -----------------------------------------
c
c
c Arguments ...
c
c command     char  : User input command string
c phsize      int   : Array dimension
c nchan       int   : Detchans, read from pha file
c chatter     int   : Chatter flag, >20 verbose
c channel     int   : Array of channels
c qualty      int   : Array of qualty flag
c qqual      logical: True if any quality flags are set to non-zero
c                                           
c
c --- CALLED ROUTINES --------------------------------------------
c
c GB_PAR            : Parser for command string
c FCECHO            : (FTOOLS) write to screen
c SET_QUAL          : Sets quality flag to passes value, 0 here
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c Rehana Yusaf (1993 April 13)
c Rehana Yusaf (1993 Oct 18) 1.0.1; Pass nchan to SET_QUAL
c Rehana Yusaf (1993 Dec 14) 1.0.2; detchans passed to this routine
c
c Banashree Mitra Seifert (1996 March)1.1.0:
c          . Introduced screen display routine
c ---------------------------------------------------------------
      character(9) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c ----------------------------------------------------------------
       subname ='good_com'
       subinfo = 'using '//subname//' Ver '//version
       call wtinfo(chatter,10,2,subinfo)

c --- CALL GOOD/BAD PARSER ---

      ierr = 0
      perr = 0
      call gb_par(command,minchan,maxchan,nocom,
     &            nchan,channel,chatter,perr,ierr)
c      IF ((perr.NE.0).OR.(ierr.NE.0)) THEN
      IF (ierr.NE.0) THEN
        subinfo='Command not executed, Try Again !'
        call wterrm(subname,version,subinfo)
        subinfo=' SYNTAX : GOOD MINCHAN-MAXCHAN '
        call wtinfo(chatter,0,1,subinfo) 
        subinfo=' SYNTAX2 : GOOD CHANNEL'
        call wtinfo(chatter,0,1,subinfo) 
        subinfo=' FOR EXAMPLE : GOOD 10-100 105'
        call wtinfo(chatter,0,1,subinfo) 
        subinfo='Channels 10 thru 100, and 105 are defined to be good'
        call wtinfo(chatter,0,1,subinfo) 
        subinfo='see ''HELP GOOD'''
        call wtinfo(chatter,0,1,subinfo) 
      ENDIF
      IF ((perr.NE.0).AND.(chatter.GE.30)) THEN
        call perr_wt(perr,chatter)
      ENDIF
c
c --- SET QUALITY FLAG ---
c
      goodint = 0
      call set_qual(minchan,maxchan,nocom,goodint,
     &              qualty,channel,nchan,chatter)
c
c --- CHECK IF ALL QUALITY FLAGS ARE GOOD ---
c
      qqual = .false.
      do i=1,nchan
        IF (qualty(i).NE.0) THEN
          qqual = .true.
        ENDIF
      enddo
      return
      end
c --------------------------------------------------------------------
c     END OF GOOD_COM
c --------------------------------------------------------------------

*+SET_QUAL
c     ----------------------------------------------------------
      subroutine set_qual(minchan,maxchan,nocom,qualflg,
     &                    qualty,channel,nchan,chatter)
c     ----------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c
c This subroutine sets the quality flag, to the value passed in,
c that is qualflg.
c
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      integer minchan(*),maxchan(*),nocom,qualflg
      integer channel(*),chatter,qualty(*),nchan
c
c --- INTERNALS --------------------------------------------------
c
      integer i,k,stchan,endchan
      character(100) subinfo
      logical findchan,findchan2,setqual
      character(11) com_syn
c
c --- VARIABLE DIRECTORY -----------------------------------------
c
c Arguments ...
c 
c minchan      int  : Starting channel for quality set
c maxchan      int  : Finishing channel for quality set
c nocom        int  : Counter for number of quality sets
c qualflg      int  : Quality flag
c channel      int  : Array of channels
c chatter      int  : Chattiness flag >20 verbose
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c Rehana Yusaf (1993 April 14)
c Rehana Yusaf (1993 Oct 18) 1.0.1; Channel array value is used
c                                   instead of index, AND check for
c                                   missing channels 
c Rehana Yusaf (1993 Dec 14) 1.0.2; If stchan and endchan NOT present
c                                   but channels in-between are present
c                                   set quality flag
c                                   Strip out channel checking part
c                                   and use FNDCHAN
c Rehana Yusaf (1994 March 14) 1.0.3; Bug-fix, terminating condition
c                                   for setqual changed 
c                                   
c Banashree Mitra Seifert (1996 March)1.1.0:
c         . Introduced screen display routine
c ----------------------------------------------------------------
      character(9) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c ----------------------------------------------------------------

      subname='set_qual'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- FIND STARTING AND END CHANNELS TO SET QUALITY ---
c
      com_syn = 'QUALITY'
      do i=1,nocom
        call fndchan(minchan(i),maxchan(i),channel,nchan,
     &               stchan,endchan,findchan,findchan2,
     &               com_syn,chatter)
c
c --- SETUP QUALITY ARRAY ---
c
        setqual = .true.
        k = 1
        IF (findchan.AND.findchan2) THEN
          do WHILE(setqual)
            IF (channel(k).EQ.stchan) THEN
              qualty(k) = qualflg
              IF (stchan.EQ.endchan) THEN
                setqual =.false.
              ENDIF
            ELSEIF ((channel(k).GT.stchan).AND.
     &(channel(k).LE.endchan)) THEN
               qualty(k) = qualflg
               IF (channel(k).EQ.endchan) THEN
                 setqual = .false.
               ENDIF
            ENDIF
            k = k+1
          enddo
        ENDIF 
      enddo
      return
      end
c --------------------------------------------------------------------
c     END OF SET_QUAL      
c --------------------------------------------------------------------


*+RSET_COM
c     -----------------------------------------------------------
      subroutine rset_com(comm,qsys,syserr,qqual,qualty,qgroup,
     &                    grping,nchan,phsize,sub_coms,
     &                    n_sub,chatter)
c     -----------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c This routine resets the grouping, quality or systematic errors
c column, or all three to zero.
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) comm
      character(172) command
      character(20) sub_coms(20)
      integer phsize,chatter,nchan,n_sub
      integer grping(*),qualty(*)
      real syserr(*)
      logical qsys,qqual,qgroup
c
c --- INTERNALS -----------------------------------------------------
c
      character(70) subinfo
      integer i,nclash
      logical val_sub
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c Arguments ...
c
c chatter     int   : Chattines flag
c phsize      int   : Array dimensions
c qualty      int   : Array of qualty flag
c grping      int   : Array of grouping flag
c serr        real  : Array of Observed statistical errors
c syserr      real  : Array of Fractional systematic error
c qgroup      logical: True if data is grouped
c qqual       logical: True if data has qualty flags
c qerror      logical: True if statistical errors included
c qsys        logical: True if systematic errors included
c command     char   : Command string
c
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 April 12)
c Rehana Yusaf (1993 June 15) : Add PAR_SUB, allow trunca' for ALL etc
c Rehana Yusaf (1995 April 28) 1.0.2; increase command from 70 to 160
c
c Banashree Mitra Seifert (1996 March) 1.1.0:
c      . Introduced screen display routine
c --------------------------------------------------------------
       character(8) subname
       character(5) version
       parameter (version = '1.1.0')
*-
c -------------------------------------------------------------------
      subname='rset_com'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- RESET ---

      call par_sub(comm,sub_coms,n_sub,command,val_sub,nclash,chatter)
      IF (command(7:11).EQ.'GROUP') THEN
        qgroup = .false.
        do i=1,nchan
          grping(i) = 1
        enddo
      ELSEIF (command(7:13).EQ.'QUALITY') THEN
        qqual = .false.
        do i=1,nchan
          qualty(i) = 0
        enddo
      ELSEIF (command(7:9).EQ.'SYS') THEN
        qsys = .false.
        do i=1,nchan
          syserr(i) = 0
        enddo
      ELSEIF (command(7:9).EQ.'ALL') THEN
        qgroup = .false.
        do i=1,nchan
          grping(i) = 1
        enddo 
        qqual = .false.
        do i=1,nchan
          qualty(i) = 0
        enddo   
        qsys = .false.
        do i=1,nchan
          syserr(i) = 0
        enddo
      ELSE
        subinfo = ' Command not exexcuted, Try Again !'
        call wterrm(subname,version,subinfo) 
        subinfo = ' SYNTAX 1 : RESET GROUP'
        call wtinfo(chatter,0,1,subinfo) 
        subinfo = ' SYNTAX 2 : RESET QUALITY'
        call wtinfo(chatter,0,1,subinfo) 
        subinfo = ' SYNTAX 3 : RESET SYSTEMATICS'
        call wtinfo(chatter,0,1,subinfo) 
        subinfo = ' SYNTAX 4 : RESET ALL'
        call wtinfo(chatter,0,1,subinfo) 
      ENDIF
      return
      end

c --------------------------------------------------------------------
c     END OF RSET_COM
c --------------------------------------------------------------------  

*+SH_COM
c     -------------------------------------------------------------
      subroutine sh_com(comm,qsys,syserr,qqual,qualty,
     &                  qgroup,grping,channel,nchan,phsize,
     &                  sub_coms,n_sub,shkeys,skeys,cform,ckeys,
     &                  ckrec,nckeys,infile,chatter)
c     -------------------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c
c This routine displays the current grouping,quality, or sys_error.
c AND mandatory keywords.
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) comm,infile
      character(172) command
      character*(*) shkeys(*), cform(*), ckrec(*),ckeys(*)
      character*(*) sub_coms(*)
      integer phsize,chatter,nchan,n_sub,skeys,nckeys
      integer grping(*),qualty(*),channel(*)
      real syserr(*)
      logical qsys,qqual,qgroup
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c
c Arguments ...
c
c chatter     int   : Chattines flag
c phsize      int   : Array dimensions
c qualty      int   : Array of qualty flag
c grping      int   : Array of grouping flag
c serr        real  : Array of Observed statistical errors
c syserr      real  : Array of Fractional systematic error
c qgroup     logical: True if data is grouped
c qqual      logical: True if data has qualty flags
c qerror     logical: True if statistical errors included
c qsys       logical: True if systematic errors included
c comm        char  : Command string
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c Rehana Yusaf (1993 April 12)
c Rehana Yusaf (1993 June 15) : Add PAR_SUB, allows truncation of ALL
c                               QUALITY etc. More concise display.
c
c Rehana Yusaf (1993 June 17) : Add check in grouping display for
c                               0 flag (undefined) occurs if group min
c                               has been performed, and spare channels
c                               are left at the end. Other occurances
c                               are outwith GRPPHA commands.
c Rehana Yusaf (1993 June 28) : Add SHOW KEY
c Rehana Yusaf (1994 Januray 6) 1.0.4; Fix bug in grouping display -
c                               occurred when singles channels are
c                               inbetween a change in group in bin
c                               min max arrays reduced from 5012 to
c                               2 ! the large arrays were needed for 
c                               the original (more verbose) grouping
c                               display
c Rehana Yusaf (1994 June 20) 1.0.5; Bug fix, gr 1 40 5 41 90 6
c                                    displayed 1 5 grouped by 5
c                                              41 90 grouped by 6
c                                    this has been corrected to
c                                              1 40 grouped by 5
c                                              41 90 grouped by 6
c                                    In addition undefined channels
c                                    at the end are no longer displayed
c                                    twice !
c Rehana Yusaf (1994 July 18) 1.0.6; Enhancement, add SHOW CHKEYS and 
c                                    SHOW chkeyword i.e SHOW RESPFILE
c Rehana Yusaf (1995 April 28) 1.0.7; increase  command from 136 to 172
c Banashree Mitra Seifert (1996, March) 1.1.0:
c       . Introduced screen display routine
c Banashree Mitra Seifert (1997, August) 1.2.0:
c       . Introduced case for last channel when command is 
c         "show grouping"
c       . Also, i=1,nchan+1 is replaced by i=1,nchan
c 
c ------------------------------------------------------------------
      character(7) subname
      character(5) version
      parameter (version = '1.2.0')
*-
c --- INTERNALS ------------------------------------------------------
c
      character(70) subinfo,datastr
      character(70) blank,line
      character(136) long_desc
      real fract,curerror
      integer i,min(2),max(2),igrps,end,nclash
      integer qual2,curqual,singles,clenact
      integer singst,singend,j,zerost,zeroend,zeros
      integer minchan1,maxchan1,minchan2,maxchan2,prev_nbin,nbin
      logical wr_grp1,wr_grp2,wr_qual1,wr_qual2,wr_sys1,wr_sys2
      logical stbin,inbin,in_sing,wr_sing
      logical wr_zero,in_zero,execute,val_sub,bad
      character(8) key

c --- USER INFO ---

      subname='sh_com'
      subinfo = 'using '//subname//' Ver '// version
      call wtinfo(chatter,10,2,subinfo)

      execute = .false.
c
c --- DISPLAY APPROPRIATE INFO ---
c
      call par_sub(comm,sub_coms,n_sub,command,val_sub,nclash,chatter)
      blank = ' '
      IF ((command(6:10).EQ.'GROUP').OR.(command(6:9).EQ.'ALL')) THEN
        execute = .true.
        line = ' -------- '
        subinfo = ' GROUPING '
        call wtinfo(chatter,0,1,blank)
        call wtinfo(chatter,0,1,line)
        call wtinfo(chatter,0,1,subinfo)
        call wtinfo(chatter,0,1,line)
        IF (qgroup) THEN

c --- FIND GROUPED DATASETS ---

          stbin = .true.
          inbin = .false.
          igrps = 0
          singles = 0
          wr_grp1 = .false.
          wr_grp2 = .false.
          in_sing = .false.
          wr_sing = .false.
          wr_zero = .false.
          in_zero = .false.
          zeros = 0

c --- DISPLAY GROUPED DATA ---

          write(subinfo,450)
          call wtinfo(chatter,0,1,subinfo) 
          subinfo = ' Channel Grouping (Channel-Channel) : '
          call wtinfo(chatter,0,1,subinfo) 

ccc          do i=1,nchan+ 1
          do i=1,nchan
            IF (grping(i).EQ.1) THEN
c --- Check for single channels 

              IF (grping(i+1).EQ.1) THEN
                IF (singles.EQ.0) THEN
                   singst = channel(i)
                ENDIF
                singles = singles + 1
                in_sing = .true.

c --- Check for end of bin,when single (ungrouped) channels start

                IF (inbin) THEN
                  max(igrps) = channel(i-1)
                  inbin = .false.
                  stbin = .true.

c --- Check if bin factor has changed

                  IF (igrps.GT.1) THEN
                    prev_nbin = max(igrps-1) - min(igrps-1) + 1
                    nbin = max(igrps) - min(igrps) + 1
                    IF (prev_nbin.NE.nbin) THEN        
                      wr_grp1 = .true.
                      maxchan1 = max(igrps-1)
                      wr_grp2 =.true.
                      minchan2 = min(igrps)
                      maxchan2 = max(igrps)
                    ELSE
                      wr_grp1 = .true.
                      maxchan1 = max(igrps)
                    ENDIF
                  ELSE
                    wr_grp1 = .true.
                    maxchan1 = max(igrps)
                    prev_nbin = max(igrps) - minchan1 + 1
                  ENDIF          
                  igrps = 0
                ENDIF

c --- Check for end of zero (undefined) grouping
 
                IF (in_zero) THEN
                  in_zero = .false.
                  wr_zero =.true.
                  zeroend = channel(i-1)
                ENDIF          
              ELSE
                IF (in_zero) THEN
                  in_zero = .false.
                  wr_zero =.true.
                  zeroend = channel(i-1)
                ENDIF

c --- Check for end of single (ungrouped) channels

                IF (in_sing) THEN
                  in_sing = .false.
                  wr_sing = .true.
                  IF (grping(i+1).EQ.-1) THEN
                    singend = channel(i-1)
                  ELSE
                    singend = channel(i)
                  ENDIF
                ENDIF
                IF (i.EQ.nchan) THEN
                  IF (.NOT.wr_sing) THEN
                    singst = channel(i)
                    wr_sing = .true.
                  ENDIF
                  singend = channel(i)
                  in_sing =.false.
                ENDIF

c --- Check for start of grouped channels

                IF (grping(i+1).EQ.-1) THEN
                 IF (stbin) THEN  
                   IF ((i+1).LE.nchan) THEN
                     igrps = igrps + 1
                     min(igrps) = channel(i)
                     minchan1 = min(igrps)
                     inbin = .true.
                     stbin = .false.
                   ENDIF

c --- Check for end of grouped data when grping flag is 1

                 ELSE 
                   max(igrps) = channel(i-1)

c --- Check if grouping factor has changed

                   IF ((igrps.GT.1).AND.
     &               (singend.NE.(min(igrps)-1)).AND.
     &               (zeroend.NE.(min(igrps)-1))) THEN
                     prev_nbin = max(igrps-1) - min(igrps-1) + 1
                     nbin = max(igrps) - min(igrps) + 1
                     IF (prev_nbin.NE.nbin) THEN
                       wr_grp2 = .true.
                       minchan2 = minchan1
                       maxchan2 = max(igrps-1)
                       nbin = prev_nbin
                       min(igrps-1) = min(igrps)
                       max(igrps-1) = max(igrps)
                       igrps = igrps - 1
                       minchan1 = min(igrps)
                     ELSE
                       min(igrps-1) = min(igrps)
                       max(igrps-1) = max(igrps)
                     ENDIF
                   ENDIF
                   IF (((i+1).LE.nchan).AND.(igrps.EQ.1)) THEN
                     igrps = igrps + 1
                   ENDIF
                   min(igrps) = channel(i)
                  ENDIF
                ELSE
                  IF (inbin) THEN
                    max(igrps) = channel(i-1)
                    stbin = .true.
                    inbin = .false.
                    maxchan1 = max(igrps)
                    prev_nbin = max(igrps) - min(igrps) + 1
                    wr_grp1 = .true.
                  ENDIF
                ENDIF
              ENDIF

c --- Check for end of grouped channels when grping flag is 0

            ELSEIF (grping(i).EQ.0) THEN
              IF (inbin) THEN
                max(igrps) = channel(i-1)
                stbin = .true.
                inbin = .false.

c --- Check if grouping factor has changed

                IF ((igrps.GT.1).AND.
     &                   (singend.NE.(min(igrps)-1)).AND.
     &                   (zeroend.NE.(min(igrps)-1))) THEN
                  prev_nbin = max(igrps-1) - min(igrps-1) + 1
                  nbin = max(igrps) - min(igrps) + 1
                  IF (prev_nbin.NE.nbin) THEN
                    wr_grp1 = .true.
                    maxchan1 = max(igrps-1)
                    wr_grp2 = .true.
                    minchan2 = min(igrps)
                    maxchan2 = max(igrps)
                  ELSE
                    wr_grp1 = .true.
                    maxchan1 = max(igrps)
                  ENDIF
                ELSE
                  wr_grp1 = .true.
                  maxchan1 = max(igrps)
                  prev_nbin = max(igrps) - minchan1 + 1
                ENDIF
                igrps = 0

c --- Check for end of single (ungrouped) channels when grping flag is 0

              ELSEIF (in_sing) THEN
                in_sing = .false.
                wr_sing = .true.
                singend = channel(i-1)
              ENDIF
              
c --- Check for start of zero (undefined) channels

              IF (i.LT.nchan) THEN
                IF (zeros.EQ.0) THEN
                  zerost = channel(i)
                ENDIF
                zeros = zeros + 1
                in_zero = .true.
              ENDIF

c --- If end of channels then set end of zero channels,wr_zero=true

              IF (i.EQ.nchan) THEN
                IF (zeros.EQ.0) THEN
                  zerost = channel(i)
                ENDIF
                zeros = zeros + 1
                zeroend = channel(i)
                in_zero = .false.
                wr_zero = .true.                
              ENDIF

c -- this case of checking if i=nchan is added in V2.7.0 ( main grppha)
c    and V1.2.0 for this subroutine
c    since, if last channel is -1 && in the same group as 
c    in the previous channels, this was not checked

            elseif (i .eq. nchan) then
                 max(igrps) = channel(i)
                 if(igrps .gt. 1) then
                    prev_nbin = max(igrps-1) - min(igrps-1) + 1 
                    nbin = max(igrps) - min(igrps) + 1 
                    if(prev_nbin .ne. nbin)then
                       wr_grp1 = .true.
                       maxchan1 = max(igrps-1)
                       wr_grp2 = .true.
                       minchan2 = min(igrps)
                       maxchan2 = max(igrps)
                    else
                       wr_grp1 = .true.
                       maxchan1 = max(igrps)
                    endif
                 else
                    wr_grp1 = .true.
                    maxchan1 = max(igrps)
                    prev_nbin = max(igrps) - minchan1 + 1
                 endif
                 igrps = 0
            endif

c --- Print grouped channels 

            IF (wr_grp1) THEN
              write(datastr,500) minchan1,maxchan1,prev_nbin
              call wtinfo(chatter,0,1,datastr)
              wr_grp1 = .false.
            ENDIF
            IF (wr_grp2) THEN
              write(datastr,500) minchan2,maxchan2,nbin
              call wtinfo(chatter,0,1,datastr)
              wr_grp2 = .false.
            ENDIF   

c --- Print single channels

            IF (wr_sing) THEN
              write(datastr,800) singst,singend
              call wtinfo(chatter,0,1,datastr)
              wr_sing = .false.
              singles = 0
            ENDIF
            
c --- Print zero (undefined) grouping channels

            IF (wr_zero) THEN
              bad = .false.
              do j=zerost,zeroend
                IF (qualty(j).NE.0) THEN
                  bad = .true.
                ENDIF
              enddo
              IF (bad) THEN
                write(datastr,950) zerost,zeroend
              ELSE
                write(datastr,900) zerost,zeroend
              ENDIF
              call wtinfo(chatter,0,1,datastr)
              wr_zero = .false.
              zeros = 0
            ENDIF         
          enddo
          write(subinfo,450)
          call wtinfo(chatter,0,1,subinfo)
        ELSE
          subinfo= ' Data is not grouped '
          call wtinfo(chatter,0,1,subinfo)
        ENDIF
      ENDIF

      IF ((command(6:12).EQ.'QUALITY').OR.
     &              (command(6:9).EQ.'ALL')) THEN
        execute = .true.
        subinfo = ' QUALITY '
        line = ' ------- '
        call wtinfo(chatter,0,1,subinfo)
        call wtinfo(chatter,0,1,blank)
        call wtinfo(chatter,0,1,line)
        call wtinfo(chatter,0,1,subinfo)
        call wtinfo(chatter,0,1,line)
        IF (qqual) THEN
          
c --- FIND CHANNELS THAT ARE NOT GOOD ---

          stbin = .true.
          inbin = .false.
          curqual = 0
          wr_qual1= .false.
          wr_qual2 = .false.
          subinfo=' Bad Channels (Channel - Channel)'
          call wtinfo(chatter,0,1,subinfo)
          write(subinfo,650)
          call wtinfo(chatter,0,1,subinfo)
          do i=1,nchan+1
            IF (qualty(i).NE.0) THEN
              IF (stbin) THEN
                curqual = qualty(i)
                minchan1=channel(i)
                inbin = .true.
                stbin = .false.
              ELSE
                IF (curqual.NE.qualty(i)) THEN
                  minchan2 = minchan1
                  maxchan2 = channel(i-1)
                  qual2 = curqual
                  curqual = qualty(i)
                  wr_qual2 = .true.
                  minchan1 = channel(i)
                  IF (i.EQ.nchan) THEN
                    maxchan1 = channel(i)
                    wr_qual1 = .true.
                    inbin = .false.
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              stbin = .true.
              IF (inbin) THEN
                maxchan1 = channel(i-1)
                wr_qual1=.true.
              ENDIF
              inbin = .false.
            ENDIF
            IF (wr_qual2) THEN
              wr_qual2 = .false.
              write(datastr,600) minchan2,maxchan2,qual2
              call wtinfo(chatter,0,1,datastr)
            ENDIF
            IF (wr_qual1) THEN
              wr_qual1 = .false.
              write(datastr,600) minchan1,maxchan1,curqual
              call wtinfo(chatter,0,1,datastr)
            ENDIF
          enddo

c--- WRITE QUALITY FLAG ---

          call wtinfo(chatter,0,1,subinfo)
        ELSE
          subinfo = ' Quality is not set, all data is good'
          call wtinfo(chatter,0,1,subinfo)
        ENDIF
      ENDIF
c
c --- DISPLAY SYSTEMATIC ERRORS ---
c
      IF ((command(6:8).EQ.'SYS').OR.
     &                     (command(6:8).EQ.'ALL')) THEN
        execute = .true.
        subinfo = ' SYSTEMATIC ERRORS '
        line = ' ----------------- '
        call wtinfo(chatter,0,1,blank)
        call wtinfo(chatter,0,1,line)
        call wtinfo(chatter,0,1,subinfo)
        call wtinfo(chatter,0,1,line)
        IF (qsys) THEN
          
c --- FIND CHANNELS THAT HAVE SYSTEMATIC ERRORS ---

          stbin = .true.
          inbin = .false.
          wr_sys1 = .false.
          wr_sys2 =.false.
          curerror  = 0
          subinfo=' Systematic Errors (Channel - Channel) :'
          call wtinfo(chatter,0,1,subinfo)
          write(subinfo,650)
          call wtinfo(chatter,0,1,subinfo)
          do i=1,nchan+1
            IF (syserr(i).NE.0) THEN
              IF (stbin) THEN
                minchan1 = channel(i)
                curerror = syserr(i)
                inbin = .true.
                stbin = .false.
              ELSE
                IF (curerror.NE.syserr(i)) THEN
                  minchan2 = minchan1
                  maxchan2 = channel(i-1)
                  fract = curerror
                  curerror = syserr(i)
                  wr_sys2=.true.
                  minchan1 = channel(i)
                  IF (i.EQ.nchan) THEN
                    maxchan1 = channel(i)
                    wr_sys1 = .true.
                    inbin = .false.
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              stbin = .true.
              IF (inbin) THEN
                maxchan1 = channel(i-1)
                wr_sys1 = .true.
              ENDIF
              inbin = .false.
            ENDIF
            IF (wr_sys2) THEN
              write(datastr,700) minchan2,maxchan2,fract
              wr_sys2 = .false.
              call wtinfo(chatter,0,1,datastr)
            ENDIF
            IF (wr_sys1) THEN
              write(datastr,700) minchan1,maxchan1,curerror
              wr_sys1 = .false.
              call wtinfo(chatter,0,1,datastr)
            ENDIF
          enddo

c--- WRITE SYSTEMATIC ERROR FLAG ---

          call wtinfo(chatter,0,1,subinfo)
        ELSE
          subinfo='Systematic Errors have not been applied '
          call wtinfo(chatter,0,1,subinfo)
        ENDIF
      ENDIF
      IF (command(6:13).EQ.'KEYWORDS') THEN
        execute = .true.
        call update(cform,ckrec,nckeys,shkeys,skeys,
     &              qsys,qqual,qgroup,chatter)
        do i=1,skeys
          call wtinfo(chatter,0,0,shkeys(i))
        enddo
      ENDIF  

c --- DISPLAY INFILE ---

      IF (command(6:11).EQ.'INFILE') THEN
        execute = .true.
        call wtinfo(chatter,0,1,blank)
        long_desc = ' '
        long_desc = ' Input file : '//infile
        subinfo=long_desc(1:clenact(long_desc))
        call wtinfo(chatter,0,1,subinfo)
      ENDIF

c --- DISPLAY SHOW CHKEYS ---

      IF (command(6:11).EQ.'CHKEYS') THEN
        execute = .true.
        i = 1
        call wtinfo(chatter,0,1,blank)
        do WHILE(i.LE.nckeys) 
          IF ((i+5).LT.nckeys) THEN
            end = i+5
          ELSE
            end = nckeys
          ENDIF
          write(subinfo,1000) (ckeys(j),j=i,end)
          call wtinfo(chatter,0,1,subinfo)
          i = i+ 6
        enddo
      ENDIF 

c --- CHECK FOR SHOW chkeyword and display ---

      IF (.NOT.execute) THEN
        call par_sub(comm,ckeys,nckeys,command,val_sub,nclash,chatter)
        i = 1
        call wtinfo(chatter,0,1,blank)
        do WHILE ((i.LE.nckeys).AND.(.NOT.execute))
          key = ckrec(i)
          IF (command(6:13).EQ.key) THEN
            execute = .true.
            long_desc = ckrec(i)
            subinfo=long_desc(1:clenact(long_desc))
            call wtinfo(chatter,0,1,subinfo)
          ENDIF
          i = i + 1
        enddo
      ENDIF  

c --- DISPLAY SHOW SYNTAX if neccessary ---

      IF ((.NOT.execute).AND.(nclash.LE.1)) THEN
        subinfo = 'Command not executed, Try Again !'
        call wterrm(subname,version,subinfo)
        subinfo = ' SYNTAX 1 : SHOW GROUP'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' SYNTAX 2 : SHOW QUALITY '
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' SYNTAX 3 : SHOW SYSTEMATICS '
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' SYNTAX 4 : SHOW ALL'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' SYNTAX 5 : SHOW KEYWORDS'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' SYNTAX 6 : SHOW INFILE'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' SYNTAX 7 : SHOW CHKEYS'
        call wtinfo(chatter,0,1,subinfo)
        subinfo='in addition any of the CHKEY keywords can be displayed'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' in full, for example SHOW RESPFILE'
        call wtinfo(chatter,0,1,subinfo)
      ENDIF      
      call wtinfo(chatter,0,1,blank)
 450  FORMAT(1X,45('-'))
 500  FORMAT(1X,I8,' -',I8,2X,'are grouped by a factor ',I8)
 600  FORMAT(1X,I8,' -',I8,2X,'have quality ',I1)
 650  FORMAT(1X,45('-'))
 700  FORMAT(1X,I8,' -',I8,2X,'have systematic error ',F6.4)
 800  FORMAT(1X,I8,' -',I8,2X,'are single channels ')
 900  FORMAT(1X,I8,' -',I8,2X,'of undefined grouping ')
 950  FORMAT(1X,I8,' -',I8,2X,'of undefined grouping',
     &       ' (Channel quality=bad)')
 1000 FORMAT(1X,6(A8,2X))
      return
      end
c --------------------------------------------------------------------
c     END OF SH_COM
c --------------------------------------------------------------------

*+GRPM_COM
c     ---------------------------------------------------------
      subroutine grpm_com(command,qualty,counts,rcts,dtype,
     &                    grping,qgroup,qqual,nchan,phsize,
     &                    texpos,channel,chatter)
c     ---------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------
c
c This routine implements the 'GROUP MIN threshhold' command.
c
c --- VARIABLES -------------------------------------------------
c
      IMPLICIT NONE
      character*(*) command
      integer phsize,dtype,channel(*)
      integer grping(*),counts(*),qualty(*)
      integer chatter,nchan
      real rcts(*),texpos
      logical qgroup,qqual
c
c --- INTERNALS -------------------------------------------------
c
      integer step,i,k,l,beg,end,pos,len
      integer thold,remain,ibin,ierr
      character(70) subinfo,datastr
      character(5) cnum
      real sumrcts,rawcts
      logical rebin,setspare 
c 
c --- VARIABLE DIRECTORY ----------------------------------------
c
c Arguments ...
c
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
c qgroup      logical: True if data is grouped
c qqual       logical: True if data has qualty flags
c qerror      logical: True if statistical errors included
c qsys        logical: True if systematic errors included
c nchan       int    : No. of detchans                           
c command     char   : Command string
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------
c
c Rehana Yusaf (1993 April 14)
c
c Rehana Yusaf (1993 June 17) : Add error check for rcnts
c Rehana Yusaf (1995 Aug 31) : Spare channels warning was using
c                              channel index not channel value !
c Banashree Mitra Seifert (March 1996)1.1.0:
c         . Introduced screen display subroutine
c         . changed format 300 to make room for higher channel no.
c --------------------------------------------------------------
      character(8) subname 
      character(5) version
      parameter (version = '1.1.0')
*-
c ----------------------------------------------------------------
      subname='grpm_com'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- PARSE COMMAND STRING ---

      beg = 11
      pos = index(command(beg:),' ')
      len = pos - 1
      end = beg+pos - 1 
      cnum = command(beg:end) 
      read(cnum(1:len),100,IOSTAT=ierr) thold
c
c --- CHECK FOR VALID NUMBER
c
      IF (thold.EQ.0) THEN
        subinfo='No Minimum counts value !'
        call wtwarm(subname,version,chatter,0,subinfo)
      ENDIF
      IF (ierr.NE.0) THEN
        subinfo = 'invalid integer on command line !'
        call wtwarm(subname,version,chatter,0,subinfo)
      ENDIF
      IF ((ierr.NE.0).OR.(thold.EQ.0)) THEN
        subinfo = ' SYNTAX : GROUP MIN RCNTS'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' FOR EXAMPLE : GROUP MIN 60'
        call wtinfo(chatter,0,1,subinfo)
        subinfo = ' see ''HELP GROUP'''
        call wtinfo(chatter,0,1,subinfo)
        return
      ENDIF 
c
c --- GROUPING ---
c
      sumrcts = 0
      rebin = .false.
      step = 1
      i = 1
      setspare = .false.
      do WHILE(i.LE.nchan)
        step = 1
        IF (dtype.EQ.1) THEN
          rawcts = counts(i)
        ELSE
          rawcts = rcts(i) * texpos
        ENDIF
        IF (qualty(i).EQ.0) THEN
          sumrcts = sumrcts + rawcts
          IF (sumrcts.LT.thold) THEN
            rebin = .true.
            ibin = i
            k = 0
            remain = nchan - ibin
c
c -- BIN UNTIL THRESHOLD COUNTS ARE REACHED ---
c
            do WHILE(rebin.AND.(k.LT.remain)) 
              k = k + 1
              IF (dtype.EQ.1) THEN
                rawcts = counts(ibin+k)
              ELSE
                rawcts = rcts(ibin+k) * texpos
              ENDIF
              IF (qualty(ibin + k).EQ.0) THEN
                sumrcts = sumrcts + rawcts
              ENDIF
              IF (sumrcts.GE.thold) THEN
                rebin = .false.
              ENDIF
            enddo
            IF (rebin) THEN
               setspare = .true.
            ENDIF
            IF (.NOT.setspare) THEN
              grping(ibin) = 1
              do l=1,k
                grping(ibin + l) = -1
              enddo
c
c --- SET SPARE CHANNELS IF ANY AT END TO 1,SET QUALITY AT END TO 2 ---
c
            ELSE
              write(datastr,300,IOSTAT=ierr) 
     &        channel(ibin),channel(ibin+k)
              IF (ierr.EQ.0) THEN
                call wtinfo(chatter,9,1,datastr)
              ENDIF
              grping(ibin) = 1
              IF( qualty(ibin) .EQ. 0 ) qualty(ibin) = 2
              do l=1,k
                grping(ibin + l) = 1
                IF( qualty(ibin+l) .EQ. 0 ) qualty(ibin + l) = 2
              enddo
              qqual = .true.
            ENDIF
            step = k+1
          ELSE
            grping(i) = 1
          ENDIF
c
c --- CASE WHERE QUALTY IS NOT 0 (BAD CHAN) ---
c
        ELSE
          grping (i) = 1
        ENDIF
        sumrcts = 0.0
        i = i + step
      enddo
      qgroup = .true.

c MJT 16July96 (g77/linux) 'bare' format error, guessing I8
c  100 format(I)
  100 format(I8)
  300 format(' Spare channels ',I8,':',I8)
      return
      end
c ---------------------------------------------------------------------
c     END OF GRPM_COM
c ---------------------------------------------------------------------
                


*+grp_par
c     ---------------------------------------------------------
      subroutine grp_par(comm,minchan,maxchan,numchan,
     &                   nocom,nchan,chatter,perr,channel,ierr)
c     ---------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c
c This is a Parser for the GROUP command. The allowed variations 
c are :    GROUP FILENAME
c and      GROUP MINCHAN MAXCHAN NUMCHAN i.e GROUP 10 300 3
c or       GROUP MINCHAN-MAXCHAN NUMCHAN i.e GROUP 10-300 2
c In the later case, where 3 sets of groupings are allowed.
c A checking routine is used to ensure that minchan < maxchan,
c and minchan <= nchan. 
c
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      character*(*) comm
      integer minchan(*),maxchan(*),numchan(*),nocom
      integer perr,nchan,chatter,channel(*)
c
c --- INTERNALS ---------------------------------------------------
c
      character(172) command
      character(80) subinfo,comfile
      character(10) cnum
      integer pos,beg,end,num(3),aft,i,maxcom,iunit
      integer len,ierr,dash_pos,comma_pos,errflg
      logical endcom,ext
c
c --- VARIABLE DIRECTORY ------------------------------------------
c
c Arguments ...
c
c comm      char   : user defined command
c minchan   int    : Array used for Lower Channel in grouping
c maxchan   int    : Array used for Upper Channel in grouping
c numchan   int    : Array of number of Channels in each grouping
c nocom     int    : Counter for No. of groupings
c nchan     int    : Counter for No. of detchans
c chatter   int    : Chattines Flag
c perr      int    : Parser error flag
c                    perr = 0    Okay
c                    perr = 1    minchan < 0 or minchan > lchan
c                    perr = 2    maxchan < 0
c                    perr = 3    maxchan > lchan
c                    perr = 4    minchan > maxchan
c                    perr = 5    file does not exist
c                    perr = 7    numchan is blank (0)
c
c ierr      int    : Fortran IOSTAT error flag
c
c --- CALLED ROUTINES ---------------------------------------------
c
c FCECHO           : (FTOOLS) Screen writing routine
c INQUIRE          : (FORTRAN) Checks that file exists
c CK_CHAN          : Channel checking routine,returns perr
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1993 March 30)
c Rehana Yusaf (1994 Jan 5) 1.0.1; Allow GROUP MINCHAN-MAXCHAN NUM
c                                 and commas inbetween settings
c Rehana Yusaf (1994 June 22) 1.0.2; Update OPEN statement, unit=5
c                                 should not be used as 5, is used for
c                                 standard output. HOST interface
c                                 did not have a problem with this 
c                                 but XPI does. 
c Rehana Yusaf (1994 July 20) 1.0.3; increase command line settings to 6
c                                 from 3
c Rehana Yusaf (1995 Jan 28) 1.0.4; increase maxcom from 100 to 500
c Rehana Yusaf (1995 Aug 31) 1.0.5; pass channel into this sub
c
c Banashree Mitra Seifert (March, 1996)1.1.0:
c       . Introduced screen display routine
c
c Banashree M Seifert (Dec, 1996) 1.2.0:
c       . status used to open file status=old and also to define
c         return status
c         it is now modified to errflg
c -------------------------------------------------------------------------
c --------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.2.0')
*-
c -----------------------------------------------------------------

      subname='grp_par'
      errflg = 0
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- REMOVE EXTRA BLANKS IN COMMAND STRING ---

      call rmvexsp(comm,command)
c
c --- DETERMINE IF COMMAND CONTAINS FILENAME ---
c
      maxcom = 500
      pos = index(command(7:),' ')
      IF (pos.EQ.1) THEN
        ierr = 1
        return
      ENDIF
      IF (command(pos+7:).EQ.'  ') THEN
        comfile = command(7:)
        INQUIRE(FILE=comfile,EXIST=ext)
        IF (.NOT.ext) THEN
           subinfo = 'File does not exist :'//comfile
           call wterrm(subname,version,subinfo)
           perr = 5
           return
        ENDIF
        call ftgiou(iunit,errflg)
        open(iunit,file=comfile,status='old')

        do i=1,maxcom

c        Read data free format from file

          ierr = 0
          read(iunit,*,IOSTAT=ierr,end=100) minchan(i),
     &                                  maxchan(i),numchan(i)

          IF (ierr.NE.0) THEN
            subinfo = 'Invalid number in file !'
            call wterrm(subname,version,subinfo)
            return
          ENDIF
          call ck_chan(minchan(i),maxchan(i),channel(1),
     &                          channel(nchan),perr,chatter)
          IF (perr.EQ.3) THEN
            perr = 0
            call ftclos (iunit,errflg)
            errflg = 0
            call ftfiou(iunit,errflg)
          ENDIF
          IF (perr.NE.0) THEN
            subinfo =' INVALID SYNTAX IN FILE :'//comfile
            call wtinfo(chatter,0,1,subinfo)
            errflg = 0
            call ftclos (iunit,errflg)
            call ftfiou(iunit,errflg)
            return
          ENDIF
        enddo
        subinfo = 'maximum No. of groupings allowed is 500'
        call wtwarm(subname,version,chatter,0,subinfo)
  100   nocom = i-1
        IF (ierr.EQ.(-1)) THEN
          ierr = 0
        ENDIF
            call ftclos (iunit,errflg)
        errflg = 0
        call ftfiou(iunit,errflg)
      ELSE
c
c --- PARSE COMMAND STRING IF NOT FILENAME ---
c
        aft = 7
        endcom=.false.
        nocom = 0
        do WHILE(.NOT.endcom)
          beg = aft

c --- GET FIRST NUMBER IN GROUP SETTING ---

          pos = index(command(beg:),' ')
          dash_pos = index(command(beg:),'-')
          IF ((dash_pos.NE.0).AND.(dash_pos.LT.pos)) THEN
            pos = dash_pos
          ENDIF
          end = beg + pos -1
          cnum = command(beg:end)
          len = pos - 1
          read (cnum(1:len),200,IOSTAT=ierr) num(1)
          IF (ierr.NE.0) THEN
            subinfo='Invalid number : '//cnum(1:len)
            call wterrm(subname,version,subinfo) 
            return
          ENDIF

c --- GET SECOND NUMBER IN GROUP SETTING ---
       
          beg = end + 1
          IF (command(beg-1:beg).EQ.'- ') THEN
             beg = beg + 1
          ELSEIF (command(beg-1:beg).EQ.' -') THEN
             IF (command(beg+1:beg+1).EQ.' ') THEN
               beg = beg + 2
             ELSE
               beg = beg + 1
             ENDIF
          ENDIF 
          pos = index(command(beg:),' ')
          end = beg + pos -1
          cnum = command(beg:end)
          len = pos - 1
          read (cnum(1:len),200,IOSTAT=ierr) num(2)
          IF (ierr.NE.0) THEN
            subinfo='Invalid number : '//cnum(1:len)
            call wterrm(subname,version,subinfo) 
            return
          ENDIF

c --- GET NUMCHAN ---

          beg = end + 1
          pos = index(command(beg:),' ')
          comma_pos = index(command(beg:),',')
          IF ((comma_pos.NE.0).AND.(comma_pos.LT.pos)) THEN
            pos = comma_pos 
          ENDIF
          end = beg + pos -1
          cnum = command(beg:end)
          len = pos - 1
          read (cnum(1:len),200,IOSTAT=ierr) num(3)
          IF (ierr.NE.0) THEN
            subinfo='Invalid number : '//cnum(1:len)
            call wterrm(subname,version,subinfo) 
            return
          ENDIF
          IF (num(3).EQ.0) THEN
           perr = 7
           subinfo='Number of channels in group must be defined'
           call wterrm(subname,version,subinfo) 
           return
          ENDIF
          nocom = nocom + 1
          call ck_chan(num(1),num(2),channel(1),
     &                               channel(nchan),perr,chatter)

          IF (perr.EQ.3) THEN
            perr = 0
          ENDIF 
          IF (perr.NE.0) THEN
            return
          ENDIF
          minchan(nocom)=num(1)
          maxchan(nocom)=num(2)
          numchan(nocom)=num(3)
          print *, "minchan(nocom)", minchan(nocom)
          print *, "maxchan(nocom)", maxchan(nocom)
          print *, "numchan(nocom)", numchan(nocom)
          aft = end + 1
          IF (command(aft-1:aft).EQ.', ') THEN
            aft = aft + 1
          ELSEIF (command(aft-1:aft).EQ.' ,') THEN
            IF (command(aft+1:aft+1).EQ.' ') THEN
              aft = aft + 2
            ELSE
              aft = aft + 1
            ENDIF
          ENDIF
          IF (command(aft:aft).EQ.' ') THEN
            endcom = .true.
          ELSE
            IF (nocom.EQ.6) THEN
              subinfo='maximum No. of command line groupings is 6'
              call wtwarm(subname,version,chatter,10,subinfo)
              endcom = .true.
            ENDIF
          ENDIF
        enddo
      ENDIF    
 200  FORMAT(I6)   
       return
       end
c ---------------------------------------------------------------------         
c     END OF GRP_PAR
c ---------------------------------------------------------------------

*+SYS_PAR
c     ---------------------------------------------------
      subroutine sys_par(comm,minchan,maxchan,err,
     &                   nocom,nchan,chatter,perr,channel,ierr)
c     ---------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c
c Parser for SYSTEMATICS command. The allowed suntax is ...
c
c SYSTEMATICS FILENAME
c SYSTEMATICS MINCHAN-MAXCHAN ERROR ie SYSTEMATICS 10-20 0.03
c - MINCHAN to MAXCHAN inclu have error applied
c SYSTEMATICS CHANNEL ERROR ie SYSTEMATICS 30 0.04
c - CHANNEL has error applied
c
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      character*(*) comm
      integer minchan(*),maxchan(*),nocom,chatter
      integer perr,ierr,nchan,channel(*)
      real err(*)
c
c --- INTERNALS ---------------------------------------------------
c
      character(172) subinfo,command,comfile
      character(5) cnum
      character(6) crnum
      real curerr
      integer pos,beg,end,num,aft,i,maxcom,iunit
      integer len,sp_pos,dash_pos,comma_pos,status
      logical endcom,ext
c
c --- VARIABLE DIRECTORY ------------------------------------------
c
c Arguments ...
c
c comm      char   : user defined command
c minchan   int    : Array used for Lower Channel in grouping
c maxchan   int    : Array used for Upper Channel in grouping
c err       real   : Array used for errors
c nocom     int    : Counter for No. of groupings
c nchan     int    : Counter for No. of detchans
c chatter   int    : Chattines Flag
c perr      int    : Parser error flag
c                    perr = 0    Okay
c                    perr = 1    minchan < 0 or minchan > nchan
c                    perr = 2    maxchan < 0
c                    perr = 3    maxchan > nchan
c                    perr = 4    minchan > maxchan
c                    perr = 5    File does not exist
c                    perr = 6    minchan-maxchan-error syntax error
c
c ierr      int    : Fortran IOSTAT error flag
c
c --- CALLED ROUTINES ---------------------------------------------
c
c FCECHO           : (FTOOLS) Screen writing routine
c INQUIRE          : (FORTRAN) Checks that file exists
c CK_CHAN          : Channel checking routine,returns perr
c
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1993 March 30)
c Rehana Yusaf (1993 Dec 29) 1.0.1; Add syntax MINCHAN-MAXCHAN ERROR
c                                   to be consistent with other commands 
c Rehana Yusaf (1994 March 14) 1.0.2; zero error can now be applied
c Rehana Yusaf (1994 June 22) 1.0.3; use cgetlun instead of unit=5
c Rehana Yusaf (1994 July 14) 1.0.4; increase maxcom on commandline to 6
c Rehana Yusaf (1995 April 28) 1.0.5; increase command from 136 to 172
c
c Banashree Mitra Seifert (March 1996) 1.1.0:
c         . Introduced screen display routines
c ---------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -----------------------------------------------------------------

      subname='sys_par'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- REMOVE EXTRA BLANKS IN COMMAND STRING ---
c
      call rmvexsp(comm,command)
c
c --- DETERMINE IF COMMAND CONTAINS FILENAME ---
c
      maxcom = 100
      ierr = 0
      perr = 0
      pos = index(command(13:),' ')
      IF (pos.EQ.1) THEN
        ierr = 1
        return
      ENDIF
      IF (command(pos+13:pos+13).EQ.' ') THEN
        comfile = command(13:)
        INQUIRE(FILE=comfile,IOSTAT=ierr,EXIST=ext)
        IF (.NOT.ext) THEN
          subinfo = 'file does not exist:'//comfile
          call wterrm(subname,version,subinfo)
          perr = 5
          return
        ENDIF
        status = 0
        call ftgiou(iunit,status)
        open(unit=iunit,file=comfile,status='old')
        do i=1,maxcom
          read(iunit,*,end=100,IOSTAT=ierr) minchan(i),maxchan(i),err(i)
        enddo
        IF (ierr.NE.0) THEN
          subinfo='INVALID NUMBER IN FILE : '//comfile
          call wtwarm(subname,version,chatter,1,subinfo)  
          close (unit =iunit)
          status = 0
          call ftfiou(iunit,status)
          return
        ENDIF
        call ck_chan(minchan(i),maxchan(i),channel(1),
     &                               channel(nchan),perr,chatter)
        IF (perr.NE.0) THEN
          subinfo= ' INVALID SYNTAX IN FILE :'//comfile
          call wtinfo(chatter,1,1,subinfo)
          close(unit=iunit)
          status = 0
          call ftfiou(iunit,status)
          return
        ENDIF
        subinfo = 'maximum No. of systematics allowed is 100'
        call wtwarm(subname,version,chatter,1,subinfo)  
  100   nocom = i-1
        IF (ierr.EQ.(-1)) THEN
          ierr = 0
        ENDIF
        close (unit=iunit)
        status = 0
        call ftfiou(iunit,status)
      ELSE
c
c --- PARSE COMMAND STRING IF NOT FILENAME ---
c
        aft = 13
        endcom=.false.
        nocom = 0
        do WHILE(.NOT.endcom)
          beg = aft
          dash_pos = 0
          sp_pos = 0
          sp_pos = index(command(beg:),' ')
          dash_pos = index(command(beg:),'-')

c --- CHECK FOR SPACE BEFORE DASH ... MINCHAN -MAXCHAN !

          IF (((dash_pos)-(sp_pos)).EQ.1) THEN
            sp_pos = dash_pos + 1
          ENDIF


c --- SINGLE CHANNEL CASE : SYSTEMATICS CHANNEL ERROR

          IF ((dash_pos.EQ.0).OR.(sp_pos.LT.dash_pos)) THEN
            end = beg+sp_pos - 1
            cnum = command(beg:end)
            len = sp_pos - 1
            read(cnum(1:len),200,IOSTAT=ierr) num
            IF (ierr.NE.0) THEN
              subinfo = ' INVALID NUMBER ON COMMAND LINE'
              call wterrm(subname,version,subinfo)
              return
            ENDIF
            nocom = nocom + 1
            minchan(nocom) = num
            maxchan(nocom) = num
          ELSE  

c --- SYSTEMATICS MINCHAN-MAXCHAN ERROR CASE 

            end = beg + dash_pos - 1
            cnum = command(beg:end)
            len = dash_pos - 1

c --- CHECK FOR EXTRA SPACE BEFORE DASH, DO NOT READ IT !

             IF (cnum(len:len).EQ.' ') THEN
               len = len - 1
             ENDIF
             read(cnum(1:len),200,IOSTAT=ierr) num
             IF (ierr.NE.0) THEN
              subinfo = ' INVALID NUMBER ON COMMAND LINE'
              call wterrm(subname,version,subinfo)
              return
             ENDIF
             nocom = nocom + 1
             minchan(nocom) = num
             beg = end + 1

c --- CHECK FOR ADDITIONAL SPACE AFTER DASH ... MINCHAN- MAXCHAN

             IF (command(beg:beg).EQ.' ') THEN
               beg = beg + 1
             ENDIF
             sp_pos = index(command(beg:),' ')
             dash_pos = index(command(beg:),'-')
         
c --- CHECK THAT SYNTAX ERROR systematics minchan-maxchan- .. is NOT made

             IF (dash_pos.NE.0) THEN
                IF (dash_pos.LT.sp_pos)  THEN
                  subinfo = ' INVALID SYNTAX'
                  call wterrm(subname,version,subinfo)
                  perr = 6
                  return
                ENDIF
              ENDIF
 
              end = beg + sp_pos - 1
              cnum = command(beg:end)
              len = sp_pos - 1
              read(cnum(1:len),200,IOSTAT=ierr) num
              IF (ierr.NE.0) THEN
                subinfo = ' INVALID NUMBER ON COMMAND LINE'
                call wterrm(subname,version,subinfo)
                return
              ENDIF
              maxchan(nocom) = num
          ENDIF

c --- READ ERROR 

          beg = end + 1
          comma_pos = index(command(beg:),',')
          pos = index(command(beg:),' ')               
          IF ((comma_pos.NE.0).AND.(comma_pos.LT.pos)) THEN
            pos = comma_pos
          ENDIF
          end = beg + pos - 1
          crnum = command(beg:end)
          len = pos - 1   
          read(crnum(1:len),*,IOSTAT=ierr) curerr
          IF (ierr.NE.0) THEN
            subinfo='INVALID REAL NUMBER ON COMMAND LINE'
            call wterrm(subname,version,subinfo)
            return
          ENDIF
          err(nocom) = curerr
          call ck_chan(minchan(nocom),maxchan(nocom),channel(1),
     &                       channel(nchan),perr,chatter)
          IF (perr.NE.0) THEN
            return
          ENDIF
          aft = end+ 1
          IF (command(aft-1:aft).EQ.', ') THEN
            aft = aft + 1
          ELSEIF (command(aft-1:aft).EQ.' ,') THEN
            IF (command(aft+1:aft+1).EQ.' ') THEN
              aft = aft + 2
            ELSE
              aft = aft + 1
            ENDIF
          ENDIF
          IF (command(aft:aft).EQ.' ') THEN
            endcom = .true.
          ELSE
            IF (nocom.EQ.6) THEN
              subinfo ='maximum No. of command line systematics is 6'
              call wtwarm(subname,version,chatter,1,subinfo)
              endcom = .true.
            ENDIF
          ENDIF
        enddo
      ENDIF    
 200  format(I6)   
      return
      end
c -------------------------------------------------------------------
c     END OF SYS_PAR
c -------------------------------------------------------------------

*+GB_PAR
c     --------------------------------------------------------
      subroutine gb_par(comm,minchan,maxchan,nocom,
     &                  nchan,channel,chatter,perr,ierr)
c     --------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c 
c Parser for good and bad command. Allowed syntax is ...
c
c BAD FILENAME
c BAD MINCHAN-MAXCHAN
c BAD CHANNEL
c BAD CHANNEL MINCHAN-MAXCHAN ie channel is set to bad, and 
c                               minchan-maxchan are set to bad incl
c The same applys to GOOD.
c
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      character*(*) comm
      integer minchan(*),maxchan(*),nocom,chatter
      integer nchan,ierr,perr,channel(*)
c
c --- INTERNALS ---------------------------------------------------
c
      character(80) subinfo,comfile
      character(172) command
      character(5) cnum
      integer pos,beg,end,num,aft,i,maxcom,iunit
      integer len,stint,dash_pos,sp_pos,comma_pos,status
      logical endcom,ext
c
c --- VARIABLE DIRECTORY ------------------------------------------
c
c Arguments ...
c
c comm      char   : user defined command
c minchan   int    : Array used for Lower Channel in grouping
c maxchan   int    : Array used for Upper Channel in grouping
c numchan   int    : Array of number of Channels in each grouping
c nocom     int    : Counter for No. of groupings
c nchan     int    : Counter for No. of detchans
c chatter   int    : Chattines Flag
c perr      int    : Parser error flag
c                    perr = 0    Okay
c                    perr = 1    minchan < 0 or minchan > nchan
c                    perr = 2    maxchan < 0
c                    perr = 3    maxchan > nchan
c                    perr = 4    minchan > maxchan
c                    perr = 5    file does not exist
c                    perr = 6    minchan-maxchan-channel,syntax err
c
c ierr      int    : Fortran IOSTAT error flag
c
c --- CALLED ROUTINES ---------------------------------------------
c
c FCECHO           : (FTOOLS) Screen writing routine
c INQUIRE          : (FORTRAN) Checks that file exists
c CK_CHAN          : Channel checking routine,returns perr
c
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1993 March 30)
c Rehana Yusaf (1993 Nov 2) 1.0.1; Bug removed - There was a problem
c                                  with the parser for one digit numbers 
c                                  at the end of more than one command case
c Rehana Yusaf (1994 Jan 3) 1.0.2; Allow commas inbetween settings
c Rehana Yusaf (1994 June 22) 1.0.3; Update to use cgetlun instead
c                                   of unit=5
c Rehana Yusaf (1994 July 14) 1.0.4; increase maxcom on commandline to 6
c Rehana yusaf (1995 April 28) 1.0.5; increase command from 136 to 172
c
c Banashree Mitra Seifert (March 1996) 1.1.0:
c       . Introduced screen display routines
c ------------------------------------------------------------------
      character(7) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -----------------------------------------------------------------
      subname='gb_par'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)
      
c
c --- REMOVE EXTRA BLANKS IN COMMAND STRING ---
c
      call rmvexsp(comm,command)
c
c --- DETERMINE IF COMMAND CONTAINS FILENAME OR SINGLE CHANNEL ---
c --- OR MINCHAN-MAXCHAN ---
c
      ierr = 0
      perr = 0
      stint = 0
      IF (command(1:3).EQ.'BAD') THEN
        stint = 5
      ELSEIF (command(1:4).EQ.'GOOD') THEN
        stint = 6
      ENDIF
      maxcom = 100
      pos = index(command(stint:),' ')
      comma_pos = index(command(stint:),',')
      IF ((comma_pos.NE.0).AND.(comma_pos.LT.pos)) THEN
        pos = comma_pos
        IF (command(stint+pos-1:stint+pos).EQ.', ') THEN
          pos = pos + 1
        ENDIF 
      ENDIF

c --- CHECK FOR NO CHANNEL OR FILENAME ENTRY

      IF (pos.EQ.1) THEN
        ierr = 1
        return
      ENDIF

      IF (command(pos+stint:pos+stint).EQ.' ') THEN
        dash_pos = index(command(stint:),'-')
        IF ((dash_pos.NE.0).AND.(dash_pos.LT.pos)) THEN

c --- MINCHAN-MAXCHAN, NOCOM = 1 CASE

          cnum = command(stint:stint+dash_pos-1)
          len = dash_pos - 1
          read(cnum(1:len),200,IOSTAT=ierr) num
          IF (ierr.NE.0) THEN
            subinfo ='INVALID NUMBER ON COMMAND LINE : '//cnum
            call wterrm(subname,version,subinfo)
            return
          ENDIF
          nocom = 1
          minchan(nocom) = num
          beg = stint + dash_pos
          end = stint+pos
          cnum = command(beg:end)
          len = pos - dash_pos - 1
          read(cnum(1:len),200,IOSTAT=ierr) num
          IF (ierr.NE.0) THEN
            subinfo =' INVALID NUMBER ON COMMAND LINE : '//cnum
            call wterrm(subname,version,subinfo)
            return
          ENDIF 
          maxchan(nocom) = num
          call ck_chan(minchan(nocom),maxchan(nocom),channel(1),
     &                                 channel(nchan),perr,chatter)
          IF (perr.NE.0) THEN
            subinfo = ' INVALID SETTING ON COMMAND LINE'
            call wterrm(subname,version,subinfo)
            return
          ENDIF
        ELSE     

c --- SINGLE CHANNEL, NOCOM = 1 CASE

          cnum = command(stint:stint+pos-1)
          len = pos-1          
          read(cnum(1:len),200,IOSTAT=ierr) num
          IF (ierr.EQ.0) THEN
            minchan(1) = num
            maxchan(1) = num
            nocom = 1
            call ck_chan(minchan(1),maxchan(1),channel(1),
     &                         channel(nchan),perr,chatter)
            IF (perr.NE.0) THEN
              subinfo = ' INVALID SYNTAX ON COMMAND LINE'
              call wterrm(subname,version,subinfo)
            ENDIF        
            return
          ELSE

c --- FILENAME ---

            ierr = 0
            comfile = command(stint:)    
            INQUIRE(FILE=comfile,IOSTAT=ierr,EXIST=ext)
            IF (.NOT.ext) THEN
              subinfo=' File does not exist :'//comfile
              call wterrm(subname,version,subinfo)
              perr = 5
              return
            ENDIF
            status = 0
            call ftgiou(iunit,status)
            open(unit=iunit,file=comfile,status='old')
            do i=1,maxcom
              read(iunit,*,IOSTAT=ierr,end=100) minchan(i),maxchan(i)
            enddo
            IF (ierr.NE.0) THEN
              subinfo =' INVALID NUMBER IN FILE : '//comfile
              call wterrm(subname,version,subinfo)
              close(unit=iunit)
              status = 0
              call ftfiou(iunit,status)
              return
            ENDIF
            call ck_chan(minchan(i),maxchan(i),channel(1),
     &                                   channel(nchan),perr,chatter)
            IF (perr.NE.0) THEN
              subinfo= ' INVALID SYNTAX IN FILE : '//comfile
              call wterrm(subname,version,subinfo)
              close (unit=iunit)
              status = 0
              call ftfiou(iunit,status)
              return
            ENDIF
            subinfo = 'maximum No. of Quality Changes is 100'
            call wtwarm(subname,version,chatter,1,subinfo)
  100       nocom = i-1
            IF (ierr.EQ.(-1)) THEN
              ierr=0
            ENDIF
            close (unit=iunit)
            status = 0
            call ftfiou(iunit,status)
          ENDIF
        ENDIF
      ELSE
c
c --- PARSE COMMAND STRING IF NOT FILENAME OR MORE THAN ONE CHANNEL ---
c
        aft = stint
        endcom=.false.
        nocom = 0
        do WHILE(.NOT.endcom)
          beg = aft
          dash_pos = index(command(beg:),'-')
          sp_pos = index(command(beg:),' ')
          comma_pos = index(command(beg:),',')
c
c --- CHECK FOR SPACE BEFORE DASH... MINCHAN -MAXCHAN !
c   
          IF (((dash_pos)-(sp_pos)).EQ.1) THEN
            sp_pos = dash_pos + 1
          ENDIF
          IF ((comma_pos.NE.0).AND.(comma_pos.LT.sp_pos)) THEN
            sp_pos = comma_pos
          ENDIF 

c --- SINGLE CHANNEL CASE : GOOD/BAD CHANNEL

          IF ((dash_pos.EQ.0).OR.(sp_pos.LT.dash_pos)) THEN
            end = beg + sp_pos - 1
            cnum = command(beg:end)
            len = sp_pos - 1
            read(cnum(1:len),200,IOSTAT=ierr) num
            IF (ierr.NE.0) THEN
              subinfo='INVALID NUMBER ON COMMAND LINE :'//cnum
              call wterrm(subname,version,subinfo)
              return
            ENDIF
            nocom = nocom + 1
            minchan(nocom) = num
            maxchan(nocom) = num
            call ck_chan(minchan(nocom),maxchan(nocom),
     &                    channel(1),channel(nchan),perr,chatter)
            IF (perr.NE.0) THEN
              subinfo = ' INVALID SYNTAX ON COMMAND LINE'
              call wterrm(subname,version,subinfo)
              return
            ENDIF

c --- CHANNEL PAIRS CASE : GOOD/BAD MINCHAN-MAXCHAN

          ELSE
            end = beg + dash_pos - 1
            cnum = command(beg:end)
            len = dash_pos - 1
c
c --- CHECK FOR EXTRA SPACE BEFORE DASH, DO NOT READ IT !
c
            IF (cnum(len:len).EQ.' ') THEN
              len = len -1
            ENDIF
            read(cnum(1:len),200,IOSTAT=ierr) num
            IF (ierr.NE.0) THEN
              subinfo = ' INVALID NUMBER ON COMMAND LINE :'//cnum
              call wterrm(subname,version,subinfo)
              return
            ENDIF
            nocom = nocom + 1
            minchan(nocom) = num
            beg = end + 1
c
c --- CHECK FOR ADDITIONAL SPACE AFTER DASH ... MINCHAN- MAXCHAN !
c
            IF (command(beg:beg).EQ.' ') THEN
              beg=beg+1
            ENDIF
            sp_pos = index(command(beg:),' ')
            dash_pos = index(command(beg:),'-')

c --- Allow comma ---

            comma_pos = index(command(beg:),',')
            IF ((comma_pos.NE.0).AND.(comma_pos.LT.sp_pos)) THEN
              sp_pos = comma_pos
            ENDIF


c --- Check that syntax error good minchan-maxchan-channel is not made

            IF (dash_pos.NE.0) THEN
              IF (dash_pos.LT.sp_pos) THEN
               subinfo =' INVALID SYNTAX ! minchan-maxchan-channel'
               call wterrm(subname,version,subinfo)
               perr = 6
               return
              ENDIF
            ENDIF

c --- READ MAXCHAN VALUE ---

            end = beg + sp_pos - 1
            cnum = command(beg:end)
            len = sp_pos - 1
            read(cnum(1:len),200,IOSTAT=ierr) num
            IF (ierr.NE.0) THEN
              subinfo =' INVALID NUMBER ON COMMAND LINE :'//cnum
              call wterrm(subname,version,subinfo)
              return
            ENDIF
            maxchan(nocom) = num
          ENDIF
          call ck_chan(minchan(nocom),maxchan(nocom),channel(1),
     &                   channel(nchan),perr,chatter)
          IF (perr.NE.0) THEN
             subinfo=' INVALID SYNTAX ON COMMAND LINE '
             call wterrm(subname,version,subinfo)
             return
          ENDIF 
          aft = end + 1

c --- Allow commas ---

          IF (command(aft-1:aft).EQ.', ') THEN
            aft = aft + 1
          ELSEIF (command(aft-1:aft).EQ.' ,') THEN
            IF (command(aft+1:aft+1).EQ.' ') THEN
              aft = aft + 2
            ELSE
              aft = aft + 1
            ENDIF 
          ENDIF

c --- Check for end of settings ---

          IF (command(aft:aft).EQ.' ') THEN
            endcom = .true.
          ELSE
            IF (nocom.EQ.6) THEN
              subinfo='maximum No. of command line Quality sets is 6'  
              call wtwarm (subname,version,chatter,1,subinfo)
              endcom = .true.
            ENDIF
          ENDIF
        enddo
      ENDIF
 200  format(I6)   
      return
      end
c ---------------------------------------------------------------------         
c     END OF GB_PAR
c ---------------------------------------------------------------------

*+CK_CHAN
c     ---------------------------------------------------------
      subroutine ck_chan(chanlo,chanhi,fchan,lchan,perr,chatter)
c     ---------------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c
c This routine checks the validity of the channels used for grouping,
c systematics, and good/bad commmand. It checks that the channels are
c within the range, and that chanhi is >= chanlo.
c
c --- VARIABLES ------------------------------------------------------      
c
      IMPLICIT NONE
      integer chanlo,chanhi,lchan,perr,chatter,fchan
c
c --- INTERNALS ------------------------------------------------------
c
      character(100) subinfo,sub
      integer ierr
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments ...
c
c chanlo    int    : Lower Channel value
c chanhi    int    : Higher Channel value
c nchan     int    : No. of detchans
c perr      int    : Parser error flag
c                    perr = 0    Okay
c                    perr = 1    minchan < 0 or minchan > lchan
c                    perr = 2    maxchan < 0
c                    perr = 3    maxchan > lchan
c                    perr = 4    minchan > maxchan
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c Rehana Yusaf (1993 April 20)
c Rehana Yusaf (1993 Dec 13) 1.0.1; Write nchan value if out of
c				    range
c Rehana Yusaf (1995 Aug 31) 1.0.2; use lchan (channel(nchan)) instead
c                                   of nchan. Use fchan
c Banashree M Seifert (1996 March)1.1.0:
c       . Introduced screen display routines
c ----------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c --------------------------------------------------------------------
      subname='ck_chan'
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,20,2,subinfo)
c
c --- CHECK THAT CHANNELS ARE WITHIN RANGE ---
c
      perr = 0

      IF ((chanlo.LT.fchan).OR.(chanlo.GT.lchan)) THEN
        perr = 1
        ierr = 0
        write(sub,100,IOSTAT=ierr) chanlo
        IF (ierr.EQ.0) THEN
          call rmvexsp(sub,subinfo)
          call wtinfo(chatter,0,2,subinfo)
        ENDIF
        IF (chanlo.GT.lchan) THEN
          write(sub,150) fchan,lchan
          call rmvexsp(subinfo,sub)
          call wtinfo(chatter,0,2,subinfo)
        ENDIF
        IF (chanlo.EQ.chanhi) THEN
          return
        ENDIF
      ENDIF            
      IF ((chanhi.LT.fchan).OR.(chanhi.GT.lchan)) THEN
        ierr = 0
        write(sub,100,IOSTAT=ierr) chanhi
        IF (ierr.EQ.0) THEN
          call rmvexsp(sub,subinfo)
          call wtinfo(chatter,0,2,subinfo)
        ENDIF
        IF (chanhi.GT.lchan) THEN
          write(sub,170)lchan
          call rmvexsp(sub,subinfo)
          call wtinfo(chatter,0,2,subinfo)
          chanhi = lchan
        ELSE
          perr = 2
        ENDIF
      ENDIF
c
c --- CHECK THAT CHANHI >= CHANLO ---
c
      IF (chanhi.LT.chanlo) THEN
        perr = 4
        ierr = 0
        write(sub,200,IOSTAT=ierr) chanhi,chanlo
        IF (ierr.EQ.0) THEN
          call rmvexsp(sub,subinfo)
          call wtinfo(chatter,0,2,subinfo)
        ENDIF
      ENDIF
 100  FORMAT(' Channel',I6,' is not within the channel range !')
 150  FORMAT(' Channel range : ',I6,' - ',I6)
 170  FORMAT(' Upper channel reset to last channel : ',I6)
 200  FORMAT(' Channel ',I6,' is less than channel ',I6,' !')


      return
      end
c ------------------------------------------------------------------
c     END OF CK_CHAN
c ------------------------------------------------------------------
 
*+PERR_WT
c     --------------------------------
      subroutine perr_wt(perr,chatter)
c     --------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c    For a Verbose chatter,20,this routine writes the Parser error
c    Flag descriptions. This may be usefull for debugging purposes.
c
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      integer perr,chatter,ierr
      character(70) subinfo 
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c perr       int    : Parser error flag value
c                     perr = 0     OKAY
c                            1     minchan < 0 or minchan > nchan
c                            2     maxchan < 0
c                            3     maxchan > nchan
c                            4     minchan > maxchan
c                            5     File does not exist
c                            6     minchan-maxchan-channel syntax err
c  
c chatter    int    : Chattiness parameter
c desc       char   : Used for screen write
c cnum       char   : Used to write perr 
c
c --- CALLED ROUTINES ----------------------------------------------
c
c FCECHO            : (FTOOLS) Screen write
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (28 April 1993)
c
c Banashree Mitra Seifert (March 1996) 1.1.0:
c              . Introduced screen display routines
c -----------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version='1.1.0')
*-
c ------------------------------------------------------------------
      subname='perr_wt'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)


c --- DISPLAYING PERR NOTATION ---

      write(subinfo,100,IOSTAT=ierr) perr
      call wtinfo(chatter,10,2,subinfo)
      subinfo = ' Parser Error flags ...'
      call wtinfo(chatter,10,2,subinfo)
      subinfo = ' perr = 0     okay'
      call wtinfo(chatter,10,2,subinfo)
      subinfo = '        1     minchan < 0 or minchan > nchan'
      call wtinfo(chatter,10,2,subinfo)
      subinfo = '        2     maxchan < 0'
      call wtinfo(chatter,10,2,subinfo)
      subinfo = '        3     maxchan > nchan'
      call wtinfo(chatter,10,2,subinfo)
      subinfo = '        4     minchan > maxchan'
      call wtinfo(chatter,10,2,subinfo)
      subinfo = '        5     file does not exist'
      call wtinfo(chatter,10,2,subinfo)
      subinfo = '        6     minchan-maxchan-channel syntax error'
      call wtinfo(chatter,10,2,subinfo)
 100  format(' Parser Error : ',I1)
      return
      end
c ------------------------------------------------------------------
c     END PERR_WT
c ------------------------------------------------------------------    



*+SET_COM
c     --------------------------------------------------
      subroutine set_com(com_strgs,n_com,sub_coms,n_sub)
c     --------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c
c This routine defines a character array with all possible commands. 
c The array is later used by the command parser. Similarly another
c array is set for sub commands.
c
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) com_strgs(*),sub_coms(*)
      integer n_sub,n_com
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c com_strgs    char     : Array of possible GRPPHA commands
c sub_coms     char     : Array of sub_commands used with SHOW and RESET
c n_com        int      : counter for number of commands
c n_sub        int      : counter for number of subcommands
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1993 June 14)
c Rehana Yusaf (1994 July 18) 1.0.1; Add CHKEY and INFILE to sub_coms
c rehana Yusaf (1995 Jan 27) 1.0.2; Add DUMP command 
      character(5) version
      parameter (version = '1.0.2')
*-
c ---------------------------------------------------------------------
c
c --- SET ARRAY COM_STRGS ... ---
c
      com_strgs(1) = 'GROUP'
      com_strgs(2) = 'SYSTEMATICS'
      com_strgs(3) = 'GOOD'
      com_strgs(4) = 'BAD'
      com_strgs(5) = 'SHOW'
      com_strgs(6) = 'WRITE'
      com_strgs(7) = 'QUIT'
      com_strgs(8) = 'EXIT'
      com_strgs(9) = 'RESET'
      com_strgs(10)= 'HELP'
      com_strgs(11)= 'COMMANDS'
      com_strgs(12)= '?'
      com_strgs(13)= 'CHKEY'
      com_strgs(14)= 'DUMP'
      n_com = 14
c
c --- SET ARRAY SUB_COMS ... ---
c
      sub_coms(1) = 'ALL'
      sub_coms(2) = 'GROUPING'
      sub_coms(3) = 'SYSTEMATICS'
      sub_coms(4) = 'QUALITY'
      sub_coms(5) = 'KEYWORDS'
      sub_coms(6) = 'INFILE'
      sub_coms(7) = 'CHKEYS'
      n_sub = 7
      return
      end
c ----------------------------------------------------------------
c     END OF SET_COM
c ----------------------------------------------------------------


*+PAR_COM
c     -----------------------------------------------------------
      subroutine par_com(in_comm,com_strgs,n_com,command,val_com,
     >                   chatter)
c     -----------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c This subroutine is a command parser. It compares the current user
c command with an array of allowed commands. Command truncation is
c taken into account. If an ambiguous command truncation is 
c encountered, the user is warned. NOTE this routine is only a 
c parser for each command family, that is each command family, such 
c as group has it's own specific parser.
c
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) in_comm,command
      character(172) comm
      character(172) g_tmp
      character(80) subinfo 
      character*(*) com_strgs(*)
      character(20) clash(20),com_tmp,val_strg
      logical val_com
      integer n_com,i,n_clash,len,len2,com_len,chatter
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c in_comm    char    : user input command
c command    char    : complete command, after truncation comparison
c comm       char    : command without leading blanks and extra spaces
c com_strgs  char    : Array containing all allowed commands
c clash      char    : Array containing command possibilities, in cases
c                      of ambiguities
c n_com      int     : Number of allowed commands
c n_clash    int     : Number of ambiguities
c len        int     : Length of first word of command string
c len2       int     : Length of second word of command string
c com_len    int     : complete command length,e.g for group it is 5
c val_com    logical : if valid command true, else false
c
c --- CALLED ROUTINES ----------------------------------------------
c
c subroutine CRMVLBK : (CALLIB) Removes leading blanks from string
c subroutine RMVEXSP : (CALLIB) Compresses more than one blank to one
c subroutine FTUPCH  : (FITSIO) Makes string upper case
c function   INDEX   : (FORTRAN) gives index number of first occurance
c                      of specified substring.
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1993 June 14)
c Rehana Yusaf (1995 April 28) ; increase g_tmp to 172 from 70
c
c Banashree Mitra Seifert (1996 March) 1.1.0:
c            . Introduced screen display routines     
c            . changed call to ftupch
c -------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c ------------------------------------------------------------------
      subname='par_com'
      subinfo='using '//subname //' Ver '//version
      call wtinfo(chatter,30,2,subinfo)

      val_com = .false.
      n_clash = 0
      do i=1,n_com
        clash(i) = '  '
      enddo
      call crmvlbk(in_comm)
      call rmvexsp(in_comm,comm)
c
c --- COMPARE INPUT COMMAND WITH VALID COMMAND STRINGS, 
c --- ALLOWING TRUNCATION ---
c
      len = index(comm(1:),' ') -1
      IF (len.EQ.0) THEN
        command = '  '
        return
      ENDIF
      call ftupch(comm(1:len))
      do i=1,n_com
        val_strg = com_strgs(i)
        IF (val_strg(1:len).EQ.comm(1:len)) THEN
           com_tmp = val_strg
           n_clash = n_clash + 1
           clash(n_clash) = com_tmp
           val_com = .true.
        ENDIF
      enddo
      IF (.NOT.val_com) THEN
        command = comm
        return
      ENDIF
c
c --- CHECK FOR MORE THAN ONE COMMAND FOUND ---
c
      IF (n_clash.GT.1) THEN
        subinfo = ' Ambiguous command truncation !'
        call wtwarm(subname,version,chatter,1,subinfo)
        subinfo = ' Possible commands : '
        call wtinfo(chatter,0,1,subinfo)
        do i=1,n_clash
          subinfo = ' '//clash(i)
          call wtinfo(chatter,0,1,subinfo)
        enddo
        command = 'INVALID'
        return
      ENDIF
c
c --- SPECIAL CASE OF GROUP MIN ---
c
      IF (com_tmp(1:5).EQ.'GROUP') THEN
        len2 = index(comm(len+2:),' ') - 1
        g_tmp = comm
        call ftupch(g_tmp)
        IF (g_tmp(len+2:len+2+len2).EQ.'MIN') THEN
          com_tmp = 'GROUP MIN'
          len = len+1+len2
        ENDIF
      ENDIF
c
c --- SPECIAL CASE OF HELP,COMMANDS, OR ? ---
c
      IF ((com_tmp.EQ.'HELP').OR.(com_tmp.EQ.'?')
     &           .OR.(com_tmp.EQ.'COMMANDS')) THEN
        com_tmp = 'HELP'
      ENDIF
      IF (com_tmp(1:9).EQ.'GROUP MIN') THEN
        com_len = 9
      ELSE
        com_len = index(com_tmp(1:),' ') - 1
      ENDIF
      command = com_tmp(1:com_len)//comm(len+1:)
      return
      end   

c ------------------------------------------------------------------
c     END OF PAR_COM 
c ------------------------------------------------------------------


*+PAR_SUB
c     ---------------------------------------------------------------
      subroutine par_sub(comm,sub_coms,n_sub,command,val_sub,n_clash,
     >                   chatter)
c     ---------------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c     This routine is a parser for a set of sub_commands, currently
c it is called by RSET_COM, and SH_COM. Truncation is allowed, the
c truncated command is determined and then the complete command is 
c used in the rest of the program.
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) comm,command
      character*(*) sub_coms(*)
      character(20) clash(20),com_tmp,val_strg
      character(80) subinfo 
      character(172) g_tmp
      integer n_sub,i,n_clash
      logical val_sub
      integer flen,len,len2,com_len,chatter
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c Arguments ...
c 
c comm       char   : input command string
c commmand   char   : command string with complete commands, not 
c                     truncations
c sub_coms   char   : Array of allowed sub_commands
c n_sub      int    : No. of allowed sub_commands
c val_sub    logical: true if valid command
c
c Internals ...
c
c i          int    : counter for loop
c clash      char   : Array containing command possibilities in case
c                     of ambiguities
c n_clash    int    : No. of ambiguities
c com_tmp    char   : temp string used for complete command found
c val_strg   char   : Used for comparison purposes
c flen        int   : used for first word lengths
c len         int   : used for word lengths
c len2        int   : length of third word, used in special case only
c g_tmp       char  : used in special case only as temp' string
c com_len     int   : length of sub_command
c
c --- CALLED ROUTINES -------------------------------------------------
c
c subroutine FTPUPCH    : (FITSIO) Makes string upper case
c function   INDEX      : (FORTRAN) gives index number of first
c                          occurance of specified sub_string
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1993 June 15)
c Rehana Yusaf (1993 June 29) : Minor modification to allow use of
c                               CHKEY sub commands
c Rehana Yusaf (1994 July 18) 1.0.2; additional argument ; n_clash
c Rehana yusaf (1995 April 28) 1.0.3; increase g_tmp to 172
c
c Banashree Mitra Seifert (1996 March)1.1.0:  
c       . Introduced screen display routines
c ------------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c ---------------------------------------------------------------------
      subname='sub_par'
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,30,2,subinfo)

      val_sub = .false.
      n_clash = 0
      do i=1,n_sub
         clash(i) = '  '
      enddo
c
c --- COMPARE INPUT STRING WITH VALID COMMAND STRINGS,
c --- ALLOWING TRUNCATION ---
c
      flen = index(comm(1:),' ')
      len = index(comm(flen+1:),' ') - 1
      IF (len.EQ.0) THEN
         command = comm
         return
      ENDIF
      call ftupch(comm(flen+1:flen+len))
      do i=1,n_sub
        val_strg = sub_coms(i)
        IF (val_strg(1:len).EQ.comm(flen+1:flen+len)) THEN
          com_tmp = val_strg
          n_clash = n_clash + 1
          clash(n_clash) = com_tmp
          val_sub = .true.
        ENDIF
      enddo
      IF (.NOT.val_sub) THEN
         command = comm
         return
      ENDIF
c
c --- CHECK FOR MORE THAN ONE COMMAND FOUND ---
c
      IF (n_clash.GT.1) THEN
        subinfo = ' Ambiguous command truncation !'
        call wterrm(subname,version,subinfo) 
        subinfo = ' Possible subcommands :'
        call wtinfo(chatter,0,1,subinfo)
        do i=1,n_clash
          subinfo = ' '//clash(i)
          call wtinfo(chatter,0,1,subinfo)
        enddo
        command = comm
        return
      ENDIF   
c
c --- SPECIAL CASE OF GROUP MIN ---
c
      IF (com_tmp(1:5).EQ.'GROUP') THEN
        len2 = index(comm(len+2:),' ') - 1
        g_tmp = comm
        call ftupch(g_tmp)
        IF (g_tmp(len+2:len+2+len2).EQ.'MIN') THEN
          com_tmp = 'GROUP MIN'
          len = len+2+len2
        ENDIF
      ENDIF
c
c --- SPECIAL CASE OF HELP,COMMANDS, OR ? ---
c
      IF ((com_tmp.EQ.'HELP').OR.(com_tmp.EQ.'?')
     &           .OR.(com_tmp.EQ.'COMMANDS')) THEN
        com_tmp = 'HELP'
      ENDIF
      IF (com_tmp.EQ.'GROUP MIN') THEN
        com_len = 9
      ELSE
        com_len = index(com_tmp(1:),' ') - 1
      ENDIF
      command = comm(1:flen)//com_tmp(1:com_len)//comm(flen+len+1:)
      return
      end
c ---------------------------------------------------------------------
c     END OF SUB_PAR
c ---------------------------------------------------------------------        
*+HLP_COM
c     ---------------------------------------------------
      subroutine hlp_com(command,com_strgs,n_com,chatter)
c     ---------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c This subroutine gives the user an interactive help.
c
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) command,com_strgs(*)
      integer n_com,chatter
c
c --- INTERNALS --- 
c
      character(70) subinfo,hlpcom
      logical val_sub
      integer n_clash
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c Arguments ...
c
c command     char    : command string
c com_strgs   char    : Array of available commands
c n_com       int     : No. of available commands
c chatter     int     : Chattiness flag
c 
c Internals ...
c
c errstr      char    : error string
c desc        char    : user info
c val_sub     logical : true if valid subcommand
c hlpcom      char    : local command string
c
c --- CALLED ROUTINES ---------------------------------------------
c
c subroutine HLP_HLP  : Top Level help ,list of commands displayed
c 
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Rehana Yusaf (1993 June 21)
c Rehana Yusaf (1995 Jan 30) 1.0.1; add dump command
c
c Banashree Mitra Seifert (Mar 1996) 1.1.0:
c         . Introduced screen display routines
c -----------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -----------------------------------------------------------------
      subname='hlp_com'
      subinfo= 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)
c
c --- PARSE COMMAND STRING ---
c
      call par_sub(command,com_strgs,n_com,hlpcom,
     &             val_sub,n_clash,chatter)
     
c
c --- CALL APPRORIATE HELP ROUTINE ---
c
      IF (hlpcom.EQ.'HELP GROUP') THEN
        call hlp_grp(chatter)
      ELSEIF (hlpcom.EQ.'HELP BAD') THEN
        call hlp_bad(chatter)
      ELSEIF (hlpcom.EQ.'HELP GOOD') THEN
        call hlp_gd(chatter)
      ELSEIF (hlpcom.EQ.'HELP SYSTEMATICS') THEN
        call hlp_sys(chatter)
      ELSEIF (hlpcom.EQ.'HELP SHOW') THEN
        call hlp_sh(chatter)
      ELSEIF (hlpcom.EQ.'HELP RESET') THEN
        call hlp_rset(chatter)
      ELSEIF (hlpcom.EQ.'HELP WRITE') THEN
        call hlp_wt(chatter)
      ELSEIF (hlpcom.EQ.'HELP EXIT') THEN
        call hlp_ext(chatter)
      ELSEIF (hlpcom.EQ.'HELP QUIT') THEN
        call hlp_qt(chatter)
      ELSEIF (hlpcom.EQ.'HELP CHKEY') THEN
        call hlp_chg(chatter)
      ELSEIF (hlpcom.EQ.'HELP DUMP') THEN
        call hlp_dump(chatter)
      ELSE
        call hlp_hlp(chatter)
      ENDIF
      return
      end
c -------------------------------------------------------------------
c     END OF HLP_COM
c -------------------------------------------------------------------

*+HLP_HLP
c     ---------------------------
      subroutine hlp_hlp(chatter)
c     ---------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays available commands
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo
      character(70) hlp_strgs(25)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlpstrgs   char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 June 21)
c Rehana Yusaf (1993 July 6) : Add CHKEY command
c Rehana Yusaf (1994 January 7) 1.0.2; Add sentence explaining the
c				option of having more than one
c			 	command family on the command line
c Rehana Yusaf (1995 Jan 30) 1.0.3; add "DUMP" command
c
c Banashree Mitra Seifert (Mar 1996) 1.1.0:
c           . Introduced screen display routines
c -------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -------------------------------------------------------------------
      subname='hlp_hlp'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c
c --- HELP STRGS ---
c

      hlp_strgs(1)=' The following "families" of command strings'
     &//' are currently available:'
      hlp_strgs(2)=' '
      hlp_strgs(3)=' group       - to group (or rebin) channels'
      hlp_strgs(4)=' bad         - to set channels to bad quality'
     &//' (ignored by XSPEC etc)'
      hlp_strgs(5)=' good        - to (re)set channels to good quality'
      hlp_strgs(6)=' systematics - to set the fractional systematic'
     &//' error of the data'
      hlp_strgs(7)=' show        - to display the current settings to'
     &//' the terminal'
      hlp_strgs(8)=' reset       - to remove any current settings'
      hlp_strgs(9)=' chkey       - to change keyword value'
      hlp_strgs(10)=' dump        - to dump current setting to an'
     &//' ascii file'

      hlp_strgs(11)=' write       - to write a new PHA file with the'
     &//' current settings'
      hlp_strgs(12)=' exit        - to exit the task (writing a'
     &//' new file)'
      hlp_strgs(13)=' quit        - to quit the task'
      hlp_strgs(14)=' commands    - this listing'  
      hlp_strgs(15)=' help        - this listing'
      hlp_strgs(16)=' ?           - this listing'
      hlp_strgs(17)=' '          
      hlp_strgs(18)=' Several commands can be  specified on the command'
     &//' line by seperating'
      hlp_strgs(19)=' them with an ampersand ''&'''  
      hlp_strgs(20)=' '
      hlp_strgs(21)=' more detailed help and the syntaxes of all the'
     &//' commands within each' 
      hlp_strgs(22)=' of these families is available by typing'
     &//' "HELP {family}" at the' 
      hlp_strgs(23)=' grppha> prompt.'
      hlp_strgs(24)=' '
      nhlp = 24
c
c --- PRINT TO SCREEN ---
c
      call wtinfo(chatter,0,1,' ')
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c -------------------------------------------------------------------
c     END OF HLP_HLP
c -------------------------------------------------------------------

*+HLP_GRP
c     ---------------------------
      subroutine hlp_grp(chatter)
c     ---------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays grouping help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlpstrgs   char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 June 21)
c Rehana Yusaf (1994 July 14) 1.0.1; increase maxcom on command line
c                                    to 6.
c
c Banashree Mitra Seifert (March 1996)1.1.0:
c       . Introduced screen display routines
c --------------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.0')
*-
c -------------------------------------------------------------------

c --- HELP STRGS ---

      hlp_strgs(1) =' '
      hlp_strgs(2) =' -----'
      hlp_strgs(3) =' GROUP'
      hlp_strgs(4) =' -----'
      hlp_strgs(5) =' This command family  sets  the  grouping flags'
     &//' such  that the  PHA'
      hlp_strgs(6) =' dataset  can be rebinned. None of the commands'
     &//' change the observed'
      hlp_strgs(7) =' counts  vs PHA  channel dataset in any way, but'
     &//' instead  fills the'
      hlp_strgs(8) =' GROUPING column with appropriate flags to'
     &//' delineate which channels'
      hlp_strgs(9) =' start each ''new bin'',and which are part'
     &//' of a continuing bin.'
      hlp_strgs(10) =' '
      hlp_strgs(11)=' There are 3 methods by which the grouping'
     &//' can be set:'
      hlp_strgs(12)=' '
      hlp_strgs(13)=' grppha>GROUP MINCHAN MAXCHAN NCHAN'
      hlp_strgs(14)=' The data is grouped from MINCHAN to MAXCHAN'
     &//' (inclusive) with NCHAN'
      hlp_strgs(15)=' bins in each group.Any ''spare'' channels will'
     &//' be left ungrouped and'
      hlp_strgs(16)=' the user informed. Any grouping requested'
     &//' which partially overlaps'
      hlp_strgs(17)=' a  pre-existing  grouping will be ignored'
     &//'  and the  user informed.'
      hlp_strgs(18)=' A maximum of 6 sets of groupings is allowed on'
     &//' the command line.'
      hlp_strgs(19)=' '
      hlp_strgs(20)=' grppha>GROUP GRP_FILE.DAT'
      hlp_strgs(21)=' The grouping information is read (free-format)'
     &//' from the  data file'
      hlp_strgs(22)=' "GRP_FILE.DAT". This file is in ASCII format'
     &//' and can consist of up'
      hlp_strgs(23)=' to 100  lines ( sets of  groupings, one per line)'
     &//' with  the syntax'
      hlp_strgs(24)=' MINCHAN MAXCHAN NCHAN,where these have the'
     &//' same meanings as above.'
      hlp_strgs(25)=' The rules regarding spare and overlapping'
     &//' groupings  are as above.'
      hlp_strgs(26)=' '
      hlp_strgs(27)=' grppha>GROUP MIN RCNTS'
      hlp_strgs(28)=' The grouping is set such that each new grouping'
     &//' contains a minimum'
      hlp_strgs(29)=' of RCNTS counts in each bin. Channels that are'
     &//' defined as  BAD are' 
      hlp_strgs(30)=' not included.Any spare channels at the end of'
     &//' the data are defined'
      hlp_strgs(31)=' BAD by software (QUALITY=2).'
      hlp_strgs(32)=' '
      nhlp = 32
c
c --- PRINT HELP TO SCREEN ---
c
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c -------------------------------------------------------------------
c     END OF HLP_GRP
c -------------------------------------------------------------------

*+HLP_BAD
c     ---------------------------
      subroutine hlp_bad(chatter)
c     ---------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays bad help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo 
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlp_strgs  char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 June 21)
c Rehana Yusaf (1994 July 14) 1.0.1; maximum number of settings on
c                                    command line increased to 6
c Banashree Mitra Seifert(March 1996) 1.1.0:
c      . Introduced screen display routines
c ---------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -------------------------------------------------------------------
      subname='hlp_bad'
      subinfo= 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- BAD HELP STRGS ---

      hlp_strgs(1) =' '
      hlp_strgs(2) =' ---'
      hlp_strgs(3) =' BAD'
      hlp_strgs(4) =' ---'
      hlp_strgs(5) =' This command family  sets  the  quality flags'
     &//' such that the specified'
      hlp_strgs(6) =' channels can be ignored by certain subsequent'
     &//' commands and downstream'
      hlp_strgs(7) =' software  (eg XSPEC). The quality  flags of'
     &//' unspecified  channels are'
      hlp_strgs(8) =' unchanged.'
      hlp_strgs(9) =' '
      hlp_strgs(10)=' There are 2 methods whereby channels can be'
     &//' set bad:'
      hlp_strgs(11)=' '
      hlp_strgs(12)=' grppha>BAD MINCHAN-MAXCHAN'
      hlp_strgs(13)=' Channels between MINCHAN and MAXCHAN (inclusive)'
     &//' are set bad (Quality'
      hlp_strgs(14)=' = 5).Note the hyphen - should this not  be'
     &//' present, the task will set'
      hlp_strgs(15)=' MINCHAN and  MAXCHAN (only)  to bad,  leaving'
     &//'  all channels inbetween'
      hlp_strgs(16)=' with their previous quailty flags.A maximum of'
     &//' 6 sets of channels are'
      hlp_strgs(17)=' allowed on the command line.'
      hlp_strgs(18)=' '
      hlp_strgs(19)=' grppha>BAD BADFILE.DAT'
      hlp_strgs(20)=' The quality  information  is read (free -'
     &//' format)  from the data file'
      hlp_strgs(21)=' "BADFILE.DAT". This file is in ASCII format'
     &//'  and can consist of up to'
      hlp_strgs(22)=' 100  lines  (sets  of channel ranges, one per'
     &//' line)  with the  syntax'
      hlp_strgs(23)=' MINCHAN  MAXCHAN where these  have the same'
     &//' meanings as above. NOTE :'
      hlp_strgs(24)=' Unlike the  command line, dashes are illegal'
     &//'  syntax in the file, and'
      hlp_strgs(25)=' single  channels  which are to be  set bad' 
     &//' must be specified setting'
      hlp_strgs(26)=' MAXCHAN to MINCHAN explicitly.'
      hlp_strgs(27)=' '
      nhlp= 27
c
c --- PRINT HELP TO SCREEN ---
c
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c ----------------------------------------------------------------------
c     END OF HLP_BAD
c ----------------------------------------------------------------------

c -------------------------------------------------------------------

*+HLP_GD
c     ---------------------------
      subroutine hlp_gd(chatter)
c     ---------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays good help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlp_strgs  char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 June 21)
c Rehana Yusaf (1994 July 14) 1.0.1; maxcom on command line increased
c                                    to 6
c Banashree Mitra Seifert (March 1996) 1.1.0:
      character(7) subname
      character(5) version
      parameter (version='1.1.0')
*-
c -------------------------------------------------------------------
      subname='hlp_gd'
      subinfo= 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)
c 
c --- BAD HELP STRGS ---
c
      hlp_strgs(1) =' '
      hlp_strgs(2) =' ----'
      hlp_strgs(3) =' GOOD'
      hlp_strgs(4) =' ----'
      hlp_strgs(5) =' This command family  sets  the quality'
     &//' flags such  that the specified'
      hlp_strgs(6) =' channels  are  considered  good  (Quality'
     &//' = 0). The quality  flags of'
      hlp_strgs(7) =' unspecified channels are unchanged. '
      hlp_strgs(8) =' '
      hlp_strgs(9) =' There are 2 methods whereby channels can be'
     &//' set good:'
      hlp_strgs(10)=' '
      hlp_strgs(11)=' grppha>GOOD MINCHAN-MAXCHAN'
      hlp_strgs(12)=' Channels between  MINCHAN and MAXCHAN'
     &//' (inclusive) are set good should'
      hlp_strgs(13)=' they not be so already.As in the case of the'
     &//' BAD command,note the use'
      hlp_strgs(14)=' of the hyphen - should this not be present,'
     &//' the task will set MINCHAN'
      hlp_strgs(15)=' and MAXCHAN (only) to good, leaving all'
     &//' channels inbetween with their'
      hlp_strgs(16)=' previous quality flags.A maximum of 6 sets'
     &//' of channels are allowed on'
      hlp_strgs(17)=' the command line.'
      hlp_strgs(18)=' '
      hlp_strgs(19)=' grppha>GOOD GOODFILE.DAT'
      hlp_strgs(20)=' The quality  information is  read (free -'
     &//' format) from  the data file'
      hlp_strgs(21)=' "GOODFILE.DAT". This  file is in  ASCII format'
     &//' and can  consist of up'
      hlp_strgs(22)=' to 100 lines (sets of channel ranges, one'
     &//'  per line) with  the syntax'
      hlp_strgs(23)=' MINCHAN MAXCHAN where  these have the same'
     &//'  meanings as above. NOTE :'
      hlp_strgs(24)=' Unlike the  command line, dashes are illegal'
     &//'  syntax in the file, and'
      hlp_strgs(25)=' single  channels which are  to be set good must'
     &//'  be specified setting'
      hlp_strgs(26)=' MAXCHAN to MINCHAN explicitly.'
      hlp_strgs(27)=' '
      nhlp=27
c
c --- PRINT HELP TO SCREEN ---
c
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c -------------------------------------------------------------------
c     END OF HLP_GD
c -------------------------------------------------------------------
 
*+HLP_SYS
c     ---------------------------
      subroutine hlp_sys(chatter)
c     ---------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays systematics help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo 
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlp_strgs  char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 June 21)
c Rehana Yusaf (1994 July 14)1.0.1; max commands on command line
c                                   have been increased to 6
c Banashree Mitra Seifert (1996 March) 1.1.0:
c         . Introduced screen display routines
c -------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -------------------------------------------------------------------
      subname = 'hlp_sys'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- HELP STRGS ---
c
      hlp_strgs(1) =' '
      hlp_strgs(2) =' -----------'
      hlp_strgs(3) =' SYSTEMATICS'
      hlp_strgs(4) =' -----------'
      hlp_strgs(5) =' This command family  sets the fractional'
     &//'  systematic error for each'
      hlp_strgs(6) =' PHA   channel  which should  be  combined'
     &//'  with  the  corresponding'
      hlp_strgs(7) =' statistical error on the data to  define'
     &//'  the true (total) error on'
      hlp_strgs(8) =' the data.It is stressed that this command'
     &//' obviously does NOT change'
      hlp_strgs(9) =' the observed (statistical)error associated'
     &//' with the PHA data.Rather'
      hlp_strgs(10)=' SYS_ERR  column is  filled  with  the '
     &//' appropriate values,  and the'
      hlp_strgs(11)=' command is therefore reversible.'
      hlp_strgs(12)='  '    
      hlp_strgs(13)=' There are 2 methods whereby the systematic'
     &//' errors can be set:'
      hlp_strgs(14)=' '
      hlp_strgs(15)=' grppha>SYSTEMATICS MINCHAN-MAXCHAN ERR'
      hlp_strgs(16)=' Channels  between  MINCHAN  and  MAXCHAN'
     &//' (inclusive)  will  have  a'
      hlp_strgs(17)=' fractional systematic error of ERR  defined'
     &//' (ERR = 0.03 corresponds'
      hlp_strgs(18)=' to a  systematic  error of 3%  of the'
     &//'  observed PHA  count rate for'
      hlp_strgs(19)=' that channel). A maximum of 6 errors'
     &//'  are permitted  on the command'
      hlp_strgs(20)=' line.'
      hlp_strgs(21)=' '
      hlp_strgs(22)=' grrpha>SYSTEMATICS SYSFILE.DAT'
      hlp_strgs(23)=' The information regarding the fractional'
     &//' systematic  errors is read'
      hlp_strgs(24)=' (free-format)from the data file "SYSFILE.'
     &//'DAT".This file is in ASCII'
      hlp_strgs(25)=' format and  can consist of up to 100 lines'
     &//' (sets of channel ranges,'
      hlp_strgs(26)=' one per line) with the syntax MINCHAN MAXCHAN'
     &//' ERR where these  have'
      hlp_strgs(27)=' the same meanings as above.'
      hlp_strgs(28)=' '
      nhlp = 28
c
c --- PRINT HELP TO SCREEN ---
c
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c --------------------------------------------------------------------
c     END OF HLP_SYS
c -------------------------------------------------------------------- 

*+HLP_SH
c     ---------------------------
      subroutine hlp_sh(chatter)
c     ---------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays show help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlp_strgs  char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 June 21)
c Rehana Yusaf (1993 July 6) : Add SHOW KEYWORDS
c 
c Rehana Yusaf (1994 July 18) 1.0.2; added SHOW CHKEYS and SHOW INFILE
c                                    also chkey keywords can be displayed
c Banashree Mitra Seifert (1996, Mar) 1.1.0:
c           . Introduced screen display routines
c -------------------------------------------------------------------
      character(7) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -------------------------------------------------------------------
      subname='hlp_sh'
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- HELP STRGS ---
c
      hlp_strgs(1) =' '
      hlp_strgs(2) =' ----'
      hlp_strgs(3) =' SHOW'
      hlp_strgs(4) =' ----'
      hlp_strgs(5) =' These commands display the current settings'
     &//' to the screen in a'
      hlp_strgs(6) =' concise format.'
      hlp_strgs(7) =' '
      hlp_strgs(8) =' grppha>SHOW GROUPING'
      hlp_strgs(9) =' Displays the current channel GROUPING cards'
     &//' to the screen.'
      hlp_strgs(10)=' '
      hlp_strgs(11)=' grppha>SHOW QUALITY'
      hlp_strgs(12)=' Displays current QUALITY flags to'
     &//' the screen.'
      hlp_strgs(13)=' '
      hlp_strgs(14)=' grppha>SHOW SYSTEMATICS'
      hlp_strgs(15)=' Displays current fractional SYSTEMATIC errors'
     &//' to the screen.'
      hlp_strgs(16)=' '
      hlp_strgs(17)=' grppha>SHOW ALL'
      hlp_strgs(18)=' Displays the current channel GROUPING cards,'
     &//' QUALITY flags and'
      hlp_strgs(19)=' fractional SYSTEMATIC errors to the screen.'
      hlp_strgs(20)=' '
      hlp_strgs(21)=' grppha>SHOW KEYWORDS'
      hlp_strgs(22)=' Displays the current mandatory keyword values'
      hlp_strgs(23)=' '
      hlp_strgs(24)=' grppha>SHOW INFILE'
      hlp_strgs(25)=' Displays input filename'
      hlp_strgs(26)=' '
      hlp_strgs(27)=' grppha>SHOW CHKEYS'
      hlp_strgs(28)=' Displays names of keywords that can have their '
     &//'values changed.'
      hlp_strgs(29)=' '
      hlp_strgs(30)=' in addition  any of the CHKEY keywords can  be'
     &//' displayed in full, for'
      hlp_strgs(31)=' example SHOW RESPFILE, will  display the '
     &//' current value  of RESPFILE.' 
      hlp_strgs(32)=' '
      nhlp = 32
c
c --- PRINT HELP TO SCREEN ---
c
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c ------------------------------------------------------------------
c     END OF HLP_SH
c ------------------------------------------------------------------

*+HLP_RSET
c     ----------------------------
      subroutine hlp_rset(chatter)
c     ----------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays reset help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo 
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlp_strgs  char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 June 21)
c
c Banashree Mitra Seifert (1996 March)1.1.0:
c         . Introduced screen display routines
c ------------------------------------------------------------------
      character(9) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -------------------------------------------------------------------
      subname = 'hlp_rset'
      subinfo= 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- HELP STRGS ---
c
      hlp_strgs(1) =' '
      hlp_strgs(2) =' -----'
      hlp_strgs(3) =' RESET'
      hlp_strgs(4) =' -----'
      hlp_strgs(5) =' These commands reset  the current settings'
     &//'  to ''null'', NOT to those'
      hlp_strgs(6) =' in the original input file.'
      hlp_strgs(7) =' '
      hlp_strgs(8) =' grppha>RESET GROUPING'
      hlp_strgs(9) =' Reset all the channel GROUPING flags to 1,'
     &//' that is to unbinned.NOTE:'
      hlp_strgs(10)=' The grouping is not reset to that of the'
     &//' original file.'
      hlp_strgs(11)=' '
      hlp_strgs(12)=' grppha>RESET QUALITY'
      hlp_strgs(13)=' Reset all the channel QUALITY flags to good'
     &//' (Qual = 0),regardless of'
      hlp_strgs(14)=' the original setting.'
      hlp_strgs(15)=' '
      hlp_strgs(16)=' grppha>RESET SYSTEMATICS'
      hlp_strgs(17)=' Reset the fractional SYSTEMATIC errors'
     &//' to zero.'
      hlp_strgs(18)=' '
      hlp_strgs(19)=' grppha>RESET ALL'
      hlp_strgs(20)=' Reset all the channel GROUPING  card, QUALITY'
     &//'  flags and  fractional'
      hlp_strgs(21)=' SYSTEMATIC errors.'
      hlp_strgs(22)=' '
      nhlp = 23
c
c --- PRINT HELP TO SCREEN ---
c
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c ------------------------------------------------------------------
c     END OF HLP_RSET
c ------------------------------------------------------------------

*+HLP_WT
c     ---------------------------
      subroutine hlp_wt(chatter)
c     ---------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays write help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo 
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlpstrgs   char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 June 21)
c
c Banashree Mitra Seifert (March 1996) 1.1.0:
c           . Introduced screen display routines
c --------------------------------------------------------------------
      character(7) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -------------------------------------------------------------------
      subname='hlp_wt'
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- HELP STRGS ---

      hlp_strgs(1) =' ' 
      hlp_strgs(2) =' -----'
      hlp_strgs(3) =' WRITE'
      hlp_strgs(4) =' -----'
      hlp_strgs(5) =' Writes an output file with the current'
     &//' settings'
      hlp_strgs(6) =' '
      hlp_strgs(7) =' grppha>WRITE ABCD.PHA'
      hlp_strgs(8) =' Writes new PHA file called "ABCD.PHA" with'
     &//'  the current settings,and'
      hlp_strgs(9) =' including copies of any other extensions present'
     &//' within the original'
      hlp_strgs(10) =' input PHA file.This command does not stop'
     &//' the task,thus settings can'
      hlp_strgs(11) =' be altered further and subsequantly written'
     &//' to another file.'
      hlp_strgs(12) =' '
      hlp_strgs(13) =' grppha>WRITE !ABCD.PHA'
      hlp_strgs(14) =' A new PHA file called "ABCD.PHA" will be'
     &//' written,as described above.'
      hlp_strgs(15) =' The ! indicates that if the  file already'
     &//'  exits, it is OVERWRITTEN.'
      hlp_strgs(16) =' '
      nhlp = 16
c
c --- PRINT HELP TO SCREEN ---
c
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c --------------------------------------------------------------------
c     END OF HLP_WT
c --------------------------------------------------------------------

*+HLP_EXT
c     ---------------------------
      subroutine hlp_ext(chatter)
c     ---------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays exit help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlpstrgs   char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 June 21)
c
c Banashree Mitra Seifert (1996 March)1.1.0:
c           . Introduced screen display routines
c ------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -------------------------------------------------------------------
      subname = 'hlp_ext'
      subinfo= 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- HELP STRGS ---
c
      hlp_strgs(1) =' '
      hlp_strgs(2) =' ----'
      hlp_strgs(3) =' EXIT'
      hlp_strgs(4) =' ----'
      hlp_strgs(5) =' Exits the task'
      hlp_strgs(6) =' '
      hlp_strgs(7) =' grppha>EXIT'
      hlp_strgs(8) =' Exit the task,first writing a new  PHA file with'
     &//' the name  specified'
      hlp_strgs(9) =' by the input parameter outfile with the final'
     &//' settings,and including'
      hlp_strgs(10)=' copies of any other extensions present within'
     &//' the original input PHA'
      hlp_strgs(11) =' file.'
      hlp_strgs(12) =' '
      hlp_strgs(13) =' grppha>EXIT ABCD.PHA'
      hlp_strgs(14) =' Exit the task, first  writing to a  new PHA file'
     &//'  called "ABCD.PHA",'
      hlp_strgs(15) =' including copies of any  other extensions within'
     &//'  the original input'
      hlp_strgs(16) =' PHA file.ABCD.PHA overides the name specified by'
     &//' the input parameter'
      hlp_strgs(17) =' outfile.'
      hlp_strgs(18) =' '
      hlp_strgs(19) = ' grppha>EXIT !ABCD.PHA'
      hlp_strgs(20) =' Exit the task, first  writing a new  PHA file,'
     &//' called "ABCD.PHA" as'
      hlp_strgs(21) =' described above. The ! indicates that'
     &//' if ABCD.PHA already exists it'
      hlp_strgs(22) =' is OVERWRITTEN.'
      hlp_strgs(23) =' '
      nhlp = 23
c
c --- PRINT HELP TO SCREEN ---
c
      do i=1,nhlp
ccc        call wtinfo(chatter,0,1,'hlp_strgs(i)')
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c ------------------------------------------------------------------
c     END OF HLP_EXT
c ------------------------------------------------------------------

*+HLP_QT
c     ---------------------------
      subroutine hlp_qt(chatter)
c     ---------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays quit help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlpstrgs   char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 June 21)
c
c Banashree Mitra Seifert (Mar 1996)1.1.0:
c        . Introduced screen display routines
c ------------------------------------------------------------
      character(7) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c -------------------------------------------------------------------
      subname = 'hlp_qt'
      subinfo ='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- HELP STRGS ---
c
      hlp_strgs(1) =' '
      hlp_strgs(2) =' ----'
      hlp_strgs(3) =' QUIT'
      hlp_strgs(4) =' ----'
      hlp_strgs(5) =' Quits the task'
      hlp_strgs(6) =' '
      hlp_strgs(7) =' grppha>QUIT'
      hlp_strgs(8) =' Exit the task without writing a new'
     &//' PHA file.'
      hlp_strgs(9) =' '
      nhlp = 9
c
c --- PRINT HELP TO SCREEN ---
c
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c -----------------------------------------------------------------
c     END OF HLP_QT
c -----------------------------------------------------------------
 
*+HLP_CHG
c     ----------------------------
      subroutine hlp_chg(chatter)
c     ----------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays chkey help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlp_strgs  char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1993 July 6)
c Rehana Yusaf (1994 July 18) 1.0.1; maximum string length
c                                    is now 120
c Banashree Mitra Seifert (1996 March) 1.1.0:
c        . Introduced screen display routines
c ----------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.0.1')
*-
c -------------------------------------------------------------------
      subname = 'hlp_chg'
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- HLP_STRGS ---

      nhlp = 1
      hlp_strgs(nhlp) =' -----'
      nhlp = nhlp + 1
      hlp_strgs(nhlp) =' CHKEY'
      nhlp = nhlp + 1
      hlp_strgs(nhlp) =' -----'
      nhlp = nhlp + 1
      hlp_strgs(nhlp)=' '
      hlp_strgs(nhlp)=' This  command  allows a  keyword  value  to'
     &//'  be  changed. Users are'
      nhlp = nhlp + 1
      hlp_strgs(nhlp)=' permitted to change the values of a '
     &//' sub-section  of the  Mandatory'
      nhlp = nhlp + 1
      hlp_strgs(nhlp) =' keywords.'
      nhlp = nhlp + 1
      hlp_strgs(nhlp) = ' '
      nhlp = nhlp + 1
      hlp_strgs(nhlp)=' grppha>CHKEY KEYWORD NEWVALUE'
      nhlp = nhlp + 1
      hlp_strgs(nhlp)=' The value  of KEYWORD  is  changed  to NEWVALU'
     &//'E. NOTE : The maximum'
      nhlp=nhlp+1
      hlp_strgs(nhlp) =' allowed length of a  number is 20, and the'
     &//' maximum character string'
      nhlp=nhlp+1
      hlp_strgs(nhlp) =' length (for filenames) is 120.NOTE:In order' 
     &//' to allow string lengths'
      nhlp = nhlp + 1
      hlp_strgs(nhlp) =' > 68 a fitsio  continuation  convention  is '
     &//'used , this  may  cause'
      nhlp = nhlp + 1
      hlp_strgs(nhlp) =' problems if downstream software has not been'
     &//' appropriately modified.'
      nhlp = nhlp + 1
      hlp_strgs(nhlp) = ' '
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c -----------------------------------------------------------------------
c     END OF HLP_CHG
c -----------------------------------------------------------------------

       

*+CHK_COM
c     ---------------------------------------------------------
      subroutine chk_com(comm,ckeys,cform,ckrec,keydesc,
     &                   nckeys,chatter)
c     ---------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c CHKEY command routine, allows some mandatory keywords to be changed.
c The array CKEYS lists the allowed keywords.
c ---------------------------------------------------------------------
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) comm,ckrec(*)
      character*(*) cform(*)
      character*(*) ckeys(*)
      character*(*) keydesc(*)
      integer chatter,nckeys
c
c --- INTERNALS -------------------------------------------------------
c
      character(80) subinfo
      character(172) command
      character(132) temprec
      character(10) key
      character(10) curform
      character(120) newstr
      character(1) keyform
      integer i,j,len,sublen,ierr,errflg,end,endpos,nclash
      real*8 eval
      character(20) cnum
      logical val_sub, findkey, update
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c 
c Arguments ...
c
c comm       char   : command string
c ckeys      char   : array of keywords, that have their values changed
c cform      char   : array of keywords, and their formats
c ckrec      char   : array of record for each keyword
c keydesc    char   : array of keyword description
c nckeys     int    : counter for number of keywords
c chatter    int    : Chattiness flag (>20 verbose)
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c     Rehana Yusaf (1993 June 29) 
c Rehana Yusaf (1994 July 7) 1.0.1; Allow longer string lengths, upto 120
c                                   also ckrec now only contains record
c                                   no description
c Rehana Yusaf (1994 Sept 8) 1.0.2; bug-fix, ckrec had remainder of old
c                                   keyword value after change.
c Rehana Yusaf (1995 April 28) 1.0.3; increase subcommand length to 172 (136)
c
c Banashree Mitra Seifert (March 1996) 1.1.0:
c         . Introduced screen display routines
c
c Banashree Mitra Seifert (July 1996) 1.2.0:
c         . problem fixed
c
c Banashree Mitra Seifert (Oct 29, 1996) 1.3.0:
c         . in case of 'E', key was changed to key(1:8) on line# 6070
c ---------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.3.0')
*-
c ---------------------------------------------------------------------
      subname= 'chk_com'
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- SUBCOMMAND PARSER - WHICH KEYWORD ? ---

      call par_sub(comm,ckeys,nckeys,command,val_sub,nclash,chatter)

c --- FIND AND UPDATE KEYWORD VALUE IN CKREC ---

      findkey = .false.
      i = 0
      errflg = 0
      sublen = index(command(7:),' ')
      IF ((sublen.EQ.0).OR.(sublen.GT.9)) THEN
        sublen = 9
      ENDIF
      sublen = sublen - 1
      do WHILE ((.NOT.findkey).AND.(i.LT.nckeys))
        i = i + 1
        key = cform(i)
        endpos = index(key(1:),' ') 
        IF (endpos.EQ.0) THEN
          endpos = 8
        ENDIF
        curform = cform(i)
        IF (command(7:6+sublen).EQ.key(1:endpos)) THEN
          findkey = .true.
          keyform = curform(10:10)

          len = index(command(8+sublen:),' ')

          IF (keyform.EQ.'E') THEN
            IF (len.EQ.0) THEN
              subinfo = 'value not entered (or too large)!'
              call wterrm(subname,version,subinfo)
              subinfo = ' Try Again !'
              call wtinfo(chatter,0,1,subinfo)
              errflg = 1
            ELSEIF (len.GT.21) THEN
              subinfo = ' maximum allowed value length is 20 !'
              call wterrm(subname,version,subinfo)
              subinfo = 'Try Again !'
              call wtinfo(chatter,0,1,subinfo)
              errflg = 1
            ENDIF
            len = len - 1
            ierr = 0
            IF (errflg.EQ.0) THEN
            read(command(8+sublen:7+sublen+len),*,IOSTAT=ierr) eval
            IF (ierr.NE.0) THEN
              subinfo = 'in value: '
     >                //command(8+sublen:7+sublen+len)//'end'
              call wterrm(subname,version,subinfo)
              errflg = 2
            ENDIF
c
c --- UPDATE CKREC ---
c
            IF (errflg.EQ.0) THEN
            j= 0
            update = .false.
            do WHILE ((.NOT.update).AND.(j.LT.nckeys))
              j = j+1
              temprec = ckrec(j)
c next line was changed from key --> key(1:8) V1.3.0 
              IF (key(1:8).EQ.temprec(1:8)) THEN
                update = .true.
                write(cnum,100) eval
                temprec(11:30) = cnum
                ckrec(j) = temprec
              ENDIF
            enddo
            ENDIF
            ENDIF
          ELSEIF (keyform.EQ.'S') THEN
            IF (len.EQ.0) THEN
              subinfo = 'string not entered (or too large) !'
              call wterrm(subname,version,subinfo)
              subinfo = ' Try Again !'
              call wtinfo(chatter,0,1,subinfo)
              errflg = 1
            ELSEIF (len.GT.120) THEN
              subinfo = 'maximum string length is 120 !'
              call wterrm(subname,version,subinfo)
              subinfo = ' Try Again !'
              call wtinfo(chatter,0,1,subinfo)
              errflg = 1
            ELSEIF (len.GT.68) THEN
               subinfo = 'string length exceeds 68 characters !'
               call wtwarm(subname,version,chatter,5,subinfo)
               subinfo = 'using fitsio continuation convention'
               call wtwarm(subname,version,chatter,5,subinfo)
            ENDIF
            len = len - 1

            IF (errflg.EQ.0) THEN
              newstr = command(8+sublen:7+sublen+len)
              endpos = index(newstr(1:),' ')
              IF (endpos.LT.20) THEN
                endpos = 18
              ENDIF
c
c --- UPDATE CKREC ---
c
            j = 0
            update = .false.
            do WHILE ((.NOT.update).AND.(j.LT.nckeys))
              j = j+1
              temprec = ckrec(j)
c the next line is changed in version 1.2.0 from key to key(1:8)
              IF (key(1:8).EQ.temprec(1:8)) THEN
                update = .true.
                temprec(11:) = newstr(1:)
c                temprec(12+endpos:12+endpos) = ''''
                ckrec(j) = temprec
              ENDIF
            enddo
            ENDIF
          ENDIF
        ENDIF
      enddo
c
c --- PRINT SYNTAX IF ERROR FLAG NOT 0 OR NOT VAL_SUB ---
c
      IF (.NOT.val_sub) THEN
        subinfo = 'invalid keyword, Try Again !'
        call wterrm(subname,version,subinfo)
      ENDIF
      IF ((errflg.NE.0).OR.(.NOT.val_sub)) THEN
        subinfo = ' SYNTAX : CHKEY KEYWORD NEWVALUE'
        call wtinfo(chatter,0,2,subinfo)
        subinfo = ' FOR EXAMPLE : chkey areascal 1.0 '
        call wtinfo(chatter,0,2,subinfo)
      ENDIF
c
c --- PRINT KEYWORDS THAT CAN BE CHANGED , IF COMMAND NOT EXECUTED ---
c
      IF ((.NOT.val_sub).OR.(.NOT.findkey)) THEN
        subinfo = ' Keywords that can have their values changed :'
        call wtinfo(chatter,0,2,subinfo)
        i = 1
        do WHILE (i.LT.nckeys) 
          IF ((i+5).LT.nckeys) THEN
            end = i+5
          ELSE
            end = nckeys
          ENDIF
          write(subinfo,200) (ckeys(j),j=i,end)
          call wtinfo(chatter,0,2,subinfo)
          i = i + 6
        enddo
      ENDIF
  100 FORMAT(E20.13)
  200 FORMAT(1X,6(A8,2X))   
      return
      end
c -----------------------------------------------------------------
c     END OF CHK_COM
c -----------------------------------------------------------------


*+UPDATE
c     ----------------------------------------------------------
      subroutine update(cform,ckrec,nckeys,shkeys,skeys,
     &                  qsys,qqual,qgroup,chatter)
c     ----------------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c
c This subroutine updates SHKEYS, the array used to display keywords,
c and their values. This needs to be updated using CKREC, which 
c contains the keywords and their current values (which can be changed
c by the user with the CHKEY command)
c CKREC format :
c 123456789012345678901234567890123456789012345678901234567890123456789012
c BACKFILE= 'none              '
c AREASCAL= 0.1000000000000E+01 
c
c SHKEYS format :
c  BACKFILE  - none     Coresponding background file 
c  AREASCAL  - 0.1E+01  area scaling factor
c
c --------------------------------------------------------------------
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) cform(*), ckrec(*), shkeys(*)
      logical qsys, qqual, qgroup
      integer skeys, nckeys, chatter
c
c --- INTERNALS ------------------------------------------------------
c
      character(72) temprec,commnt
      character(70) tempsh, subinfo
      character(8) chkey, shkey
      character(10) tempform
      character(1) keyform
      character(20) chk_num
      character(14) sh_num
      integer i,j,k,ierr,strlen,clenact
      real*8 eval
      logical findform, updated
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments ...
c
c cform      char   : array of keywords and format eg AREASCAL E
c ckrec      char   : Array of keyword records
c nckeys     int    : counter for CFORM and CKREC
c shkeys     char   : Array of display keyword records
c skeys      int    : counter for SHKEYS
c qsys       logical: True if systematic fractional errors applied
c qqual      logical: True if quality flags set
c qgroup     logical: True if grouping flags set
c chatter    int    : Chattiness flag
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c Rehana Yusaf (1993 July 2)  : 
c Rehana Yusaf (1994 July 19) 1.0.1; modified to add complete filenames
c                                    to shkeys arrays, desc is omitted
c                                    if filename > 14
c Banashree Mitra Seifert (March 1996) 1.1.0:
c         . Introduced screen display routines
c ---------------------------------------------------------------------
      character(7) subname
      character(5) version
      parameter (version = '1.1.0')
*-
c --------------------------------------------------------------------
      subname='update'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c --- UPDATE SHKEYS ---
c
      do i=1,nckeys
        temprec = ckrec(i)
        chkey = temprec(1:8)
        k = 0
        findform = .false.
        do WHILE(.NOT.findform)
          k = k+1
          tempform = cform(k)
          IF (chkey.eq.tempform(1:8)) THEN
            keyform = tempform(10:10)
            findform = .true.
          ENDIF
        enddo
        updated = .false.
        j = 0
        do WHILE ((.NOT.updated).AND.(j.LT.skeys))
          j = j+1
          tempsh = shkeys(j)
          commnt=tempsh(30:)
          shkey = tempsh(2:9)
          IF (chkey.EQ.shkey) THEN
            IF (keyform.EQ.'E') THEN
              chk_num = temprec(12:30)
              ierr = 0
              read(chk_num,*,IOSTAT=ierr) eval
              write(sh_num,200,IOSTAT=ierr) eval
              IF (ierr.NE.0) THEN
                subinfo = 'reading real value'
     &                    //' from keywords record'
                call wterrm(subname,version,subinfo)
              ENDIF
              call crmvlbk(sh_num)
              tempsh(14:27) = sh_num
            ELSEIF (keyform.EQ.'S') THEN
              strlen = clenact(temprec(11:))
              if(tempsh(6:9) .eq. 'FILE') then
                 commnt=commnt(index(commnt(1:),'A'):)
              endif

              IF (strlen.LE.14) THEN  
                tempsh(14:27) = temprec(11:25)
                tempsh(28:)='  '//commnt
              ELSE
                tempsh(14:13+strlen) = temprec(11:)
                tempsh(14+strlen:)=' '//commnt
                IF (strlen.GT.57) THEN
                  tempsh(68:70) = '...'
                ENDIF
              ENDIF
            ENDIF
            updated = .true.
            shkeys(j) = tempsh
          ENDIF
        enddo
      enddo
      do i=1,skeys
        tempsh = shkeys(i)
cccc        shkey = tempsh(2:9)
        shkey = tempsh(1:8)
        IF ('SYS_ERR '.EQ.shkey) THEN
          IF (qsys) THEN
            sh_num = 'TRUE'
          ELSE
            sh_num = 'FALSE'
          ENDIF
        tempsh(14:27) = sh_num
cccc        tempsh(13:27) = sh_num
        ELSEIF ('QUALITY '.EQ.shkey) THEN
          IF (qqual) THEN
            sh_num = 'TRUE'
          ELSE
            sh_num = 'FALSE'
          ENDIF  
        tempsh(14:27) = sh_num  
cccc        tempsh(13:27) = sh_num  
        ELSEIF ('GROUPING'.EQ.shkey) THEN
          IF (qgroup) THEN
            sh_num = 'TRUE'
          ELSE
            sh_num = 'FALSE'
          ENDIF  
        tempsh(14:27) = sh_num  
ccc        tempsh(13:27) = sh_num  
        ENDIF
        shkeys(i) = tempsh
      enddo
  200 FORMAT(1pg12.5)
      return
      end
c -------------------------------------------------------------------
c     END OF UPDATE
c -------------------------------------------------------------------  

*+GT_COM
c     -----------------------------------------------------------
      subroutine gt_com(in_comm,sep_com,ncomms,chatter)
c     -----------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c This subroutine parsers the user command and seperates the command
c if neccessary - several command families can be specified by 
c seperating them with a ; For example ...
c GROUP 1 4098 8; BAD 1-500;exit
c
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) in_comm, sep_com(*) 
      character(70) subinfo
      integer ncomms,chatter
c
c --- VARIABLE DIRECTORY -------------------------------------------
c 
c comm     char     : command read from screen
c sep_com  char     : Array of commands - extracting from comm
c ncomms   int      : Number of commands extracted
c chatter  int      : Chattiness flag
c
c --- CALLED ROUTINES ----------------------------------------------
c
c subroutine CRMVLBK : (CALLIB) Removes leading blanks from string
c subroutine RMVEXSP : (CALLIB) Compresses more than one blank to one
c function   INDEX   : (FORTRAN) gives index number of first occurance
c                      of specified substring.
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1993 DEC 27)
c Rehana Yusaf (1994 January 7) 1.0.1; Change ':' to '&'
c Rehana Yusaf (1995 April 28) 1.0.2; increase comm from 398 to 516
c
c Banashree Mitra Seifert (1996 March) 1.1.0:
c        . Introduced screen display routines
c ---------------------------------------------------------------------
      character(7) subname
      character(5) version
      parameter (version = '1.0.2')
*-
c INTERNALS ...

      character(516) comm
      character(1) seperator 
      integer beg,end
      logical end_com
c ------------------------------------------------------------------
      subname='gt_com'
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c
c --- INITIALISATION ---
c
      call crmvlbk(in_comm)
      call rmvexsp(in_comm,comm)
c
c --- EXTRACT EACH COMMAND ---
c
        subinfo = ' GT_COM: complete command read in :'
        call wtinfo(chatter,25,2,subinfo)
        call wtinfo(chatter,25,2,comm)
        subinfo = ' Separated Commands ...'
        call wtinfo(chatter,25,2,subinfo)
      seperator = '&'
      ncomms = 0
      beg = 1
      end_com = .false.
      do WHILE (.NOT.end_com)
        call crmvlbk(comm(beg:))
        end = index(comm(beg:),seperator)
        IF (end.EQ.0) THEN
          end = index(comm(beg:),'  ')
          end_com = .true.
        ENDIF
        IF (end.NE.0) THEN
          end = end - 1
        ENDIF
        ncomms = ncomms + 1
        sep_com(ncomms) = comm(beg:(beg+end-1))
        subinfo=sep_com(ncomms)
        call wtinfo(chatter,25,2,subinfo)
        beg = beg+ end + 1 
        IF (comm(beg:beg+1).EQ.'  ') THEN
          end_com = .true.
        ENDIF
      enddo
c
      return
      end   

c ------------------------------------------------------------------
c     END OF GT_COM 
c ------------------------------------------------------------------

*+FNDCHAN
c     ----------------------------------------------------------
      subroutine fndchan(minchan,maxchan,channel,nchan,
     &                   stchan,endchan,findchan,findchan2,
     &                   com_syn,chatter)
c     ----------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c
c This subroutine determines STCHAN and ENDCHAN, it is called by
c GOOD, BAD and SYSTEMATICS commands 
c
c --- VARIABLES --------------------------------------------------
c
      IMPLICIT NONE
      integer minchan,maxchan
      integer channel(*),chatter,nchan,stchan,endchan
      character*(*) com_syn
ccc      character(11) com_syn
      logical findchan,findchan2
c
c --- INTERNALS --------------------------------------------------
c
      integer j,k,stbin,endbin,num
      character(80) subinfo,desc
      logical mis_inbin
      integer nmiss,maxmiss,curchan,l
      parameter (maxmiss = 10)
      integer missarray(maxmiss)
c
c --- VARIABLE DIRECTORY -----------------------------------------
c
c Arguments ...
c 
c minchan      int  : Starting channel for quality set
c maxchan      int  : Finishing channel for quality set
c stchan       int  : starting channel for setting
c endchan      int  : ending channel for setting
c channel      int  : Array of channels
c chatter      int  : Chattiness flag >20 verbose
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------
c
c Rehana Yusaf (1993 Dec 15) 1.0.0; Stripped Out from SET_QUAL
c
c Banashree Mitra Seifert (March 1996) 1.1.0:
c            . Introduced screen display routines
c --------------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.0.0')
*-
c ----------------------------------------------------------------
      subname = 'fndchan'
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)
c
c --- FIND STARTING AND END CHANNELS TO SET QUALITY ---
c
        j = 1
	stbin=0
	endbin=0
        findchan =.false.
        findchan2=.false.
        do WHILE(((.NOT.findchan).OR.(.NOT.findchan2))
     &.AND.(j.LE.nchan))
          IF (channel(j).EQ.minchan) THEN
            stchan = channel(j)
            stbin = j
            findchan = .true.
          ENDIF
          IF (channel(j).EQ.maxchan) THEN
            endchan = channel(j)
            endbin = j 
            findchan2 = .true.
          ENDIF
          j = j+1
        enddo

c
c --- CHECK THAT CHANNELS WERE FOUND ---
c

c Check if both start and finish channels not found ..

        IF ((.NOT.findchan).AND.(.NOT.findchan2)) THEN
          IF (minchan.NE.maxchan) THEN
           j = 1
           do WHILE((channel(j).LT.maxchan)
     &.AND.(j.LE.nchan))
             IF (channel(j).GT.minchan) THEN
               IF (.NOT.findchan) THEN
                 stchan = channel(j)
                 stbin = j
                 findchan = .true.
               ENDIF
             ENDIF
             IF ((channel(j+1).GT.maxchan)
     &.OR.(j.EQ.nchan))  THEN
               IF (findchan) THEN
                 endchan = channel(j)
                 endbin = j
                 findchan2 = .true.
               ENDIF
             ENDIF
             j = j + 1
           enddo
           IF (findchan.AND.findchan2) THEN
             IF ((minchan+1.EQ.stchan).AND.
     &(maxchan-1.EQ.endchan)) THEN
               write(subinfo,500) minchan,maxchan
             ELSEIF ((minchan+1.NE.stchan).AND.
     &(maxchan-1.EQ.endchan)) THEN
               write(subinfo,550) minchan,stchan-1,maxchan
             ELSEIF ((minchan+1.EQ.stchan).AND.
     &(maxchan-1.NE.endchan)) THEN
               write(subinfo,570) minchan,endchan+1,maxchan
             ELSE
               write(subinfo,600) minchan,stchan-1,
     &endchan+1,maxchan
             ENDIF
             call rmvexsp(subinfo,desc)
             subinfo = desc
               call wtwarm(subname,version,chatter,0,subinfo)
               write(subinfo,200) com_syn,stchan,endchan
               call rmvexsp(subinfo,desc)
               call wtinfo(chatter,0,2,desc)
           ELSE
             write(subinfo,300) minchan,maxchan
             call rmvexsp(subinfo,desc)
               subinfo= desc
               call wtwarm(subname,version,chatter,0,subinfo)
               subinfo = ' '//com_syn//' NOT been set for'
     &                    //' these channels !'
               call rmvexsp(subinfo,desc)
               call wtinfo(chatter,0,2,desc)
           ENDIF
          ELSE
           write(subinfo,100) minchan
           call rmvexsp(subinfo,desc)
           subinfo = desc
           call wtwarm(subname,version,chatter,0,subinfo)
           subinfo = ' '//com_syn//' not set for this Channel'
           call wtinfo(chatter,0,2,desc)
          ENDIF   
        ENDIF

c Check for case where endbin is NOT present ...

        IF ((.NOT.findchan2).AND.(findchan))THEN
           j = 1
           do WHILE((channel(j).LT.maxchan)
     &.AND.(j.LE.nchan))
             IF ((channel(j+1).GT.maxchan)
     &.OR.(j.EQ.nchan)) THEN
               findchan2 = .true.
               endchan = channel(j)
               endbin = j
             ENDIF
             j = j + 1
           enddo
           IF ((maxchan-1).EQ.endchan) THEN
             write(subinfo,100) maxchan
             call rmvexsp(subinfo,desc)
             subinfo = desc
             call wtwarm(subname,version,chatter,0,subinfo)
           ELSE
             num = endchan + 1
             write(subinfo,300) num, maxchan 
             call rmvexsp(subinfo,desc)
             subinfo = desc
             call wtwarm(subname,version,chatter,0,subinfo)
           ENDIF
           write(subinfo,200) com_syn,stchan,endchan 
           call rmvexsp(subinfo,desc)
           call wtinfo(chatter,0,2,desc)
        ENDIF

c Check for case where stchan is NOT present ...

        j = 1
        IF ((.NOT.findchan).AND.(findchan2)) THEN
          do WHILE(.NOT.findchan)
            IF ((channel(j).GT.minchan) 
     &.AND.(channel(j).LE.maxchan)) THEN
               stchan = channel(j)
               stbin = j
               findchan = .true.
            ENDIF
            j = j+ 1
          enddo
          IF ((minchan+1).EQ.stchan) THEN
            write(subinfo,100) minchan
            call rmvexsp(subinfo,desc)
            subinfo = desc
            call wtwarm(subname,version,chatter,0,subinfo)
            subinfo = ' '//com_syn//' not set for this channel'
            call rmvexsp(subinfo,desc)
            call wtinfo(chatter,0,2,desc)
          ELSE
            num = stchan - 1
            write(subinfo,300) minchan,num
            call rmvexsp(subinfo,desc)
            subinfo = desc
            call wtwarm(subname,version,chatter,0,subinfo)
            subinfo = ' '//com_syn//' NOT set for these channels'
            call rmvexsp(subinfo,desc)
            call wtinfo(chatter,0,2,desc)
          ENDIF
          write(subinfo,200)com_syn,stchan,endchan
          call rmvexsp(subinfo,desc)
          call wtinfo(chatter,0,2,desc)
        ENDIF

c --- CHECK if findchan and findchan2 BUT missing channels in-between
     
        IF (chatter.GE.5) THEN 
          mis_inbin = .false.
          nmiss = 0
          curchan = stchan + 1
          IF (findchan.AND.findchan2) THEN
            do k=stbin+1,endbin
              IF (channel(k).NE.curchan) THEN
                do l=curchan,(channel(k) - 1)
                   nmiss = nmiss + 1
                   IF (nmiss.LE.maxmiss) THEN
                     missarray(nmiss) = l
                   ENDIF
                 enddo
                 mis_inbin = .true.
              ENDIF
              curchan = channel(k) + 1 
            enddo
         ENDIF
         IF (mis_inbin) THEN
            call disp_mis(missarray,maxmiss,nmiss,stchan,endchan,
     &com_syn,chatter)
            desc = ' '//com_syn//' setting is still performed '
            call rmvexsp(desc,subinfo)
            call wtinfo(chatter,0,2,subinfo)
         ENDIF
       ENDIF

 100  FORMAT(' Channel ',I8,' not present in this PHA dataset')
 200  FORMAT(' ',A11,' set for Channels ',I8,' to ',I8)
 300  FORMAT(' Channels ',I8,' to ',I8,' are not present')
 500  FORMAT(' Channels ',I8,' and ',I8,' are not present')
 550  FORMAT(' Channels ',I8,' - ',I8,' and ',I8,' are not present')
 570  FORMAT(' Channels ',I8,' and ',I8,' - ',I8,' are not present')
 600  FORMAT(' Channels ',I8,' - ',I8,' and ',I8,' - ',I8,
     &' are not present')
      return
      end
c --------------------------------------------------------------------
c     END OF FNDCHAN 
c --------------------------------------------------------------------

*+DISP_MIS
c     -----------------------------------------------------------
      subroutine disp_mis(missarray,maxmiss,nmiss,stchan,endchan,
     &                    com_syn,chatter)
c     -----------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------------
c This subroutine displays missing channels inbetweem STCHAN and
c ENDCHAN.
c -------------------------------------------------------------------
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE 
      integer maxmiss,nmiss,stchan,endchan
      integer missarray(maxmiss),chatter
      character*(*) com_syn
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c missarray    int   : array containing the missing channels
c maxmiss      int   : Maximum array dimension
c nmiss        int   : Counter for number of missing channels
c stchan       int   : Starting channel !
c endchan      int   : Endchannel, the missing channels are inbetween
c		       stchan and endchan
c com_syn      char  : Character string containing command name
c chatter      int   : Chattiness flag
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf Dec 10 1993 1.0.0;
c
c Banashree Mitra Seifert (1996 March) 1.1.0:
c          . Introduced screen display routines
c ------------------------------------------------------------------
      character(9) subname
      character(5) version
      parameter (version = '1.0.0')
*-
c -------------------------------------------------------------------
c
c INTERNALS ...
c
      character(80) subinfo,sub1,sub2
      integer k
c
c --- USER INFO ---
c
      subname= 'disp_mis'
      subinfo= 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)


c --- DISPLAY MISSING CHANNELS ---

      IF (nmiss.EQ.1) THEN
        write(subinfo,100)missarray(nmiss),com_syn,stchan,endchan 
      ELSE
        write(sub2,200)com_syn,stchan,endchan
        call rmvexsp(sub2,sub1) 
        call wtinfo(chatter,0,1,sub1)
        IF (nmiss.LE.10) THEN
          write(subinfo,300) (missarray(k),k=1,nmiss)
        ELSE
          write(subinfo,300) (missarray(k),k=1,10) 
        ENDIF
      ENDIF
      call rmvexsp(subinfo,sub1)
      call wtinfo(chatter,0,1,sub1)
      IF (nmiss.GT.10) THEN
        write(subinfo,400)nmiss
        call rmvexsp(subinfo,sub1)
        call wtinfo(chatter,0,1,sub1)
        subinfo = ' only first 10 displayed here'
        call wtinfo(chatter,0,1,subinfo)
      ENDIF
c
c --- FORMATS ---
c
  100 FORMAT(1X,'Missing Channel : ',I6,' in ',A11,' setting',
     &I6,' to ',I6)
  200 FORMAT(1X,'Missing Channels in ',A11,' setting',I6,
     &' to ',I6,' :')
  300 FORMAT(10(1X,I6))
  400 FORMAT(1X,' There are ',I6,' missing Channels')
      return
      end
c -------------------------------------------------------------------
c     END OF DISP_MIS
c -------------------------------------------------------------------

       

*+DUMP_COM
c     -------------------------------------------------------------
      subroutine dump_com(comm,qsys,syserr,qqual,qualty,
     &                  qgroup,grping,channel,nchan,phsize,
     &                  sub_coms,n_sub,clobber,infile,chatter)
c     -------------------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c
c This routine "dumps" the current grouping, systematics or quality
c to an ascii file
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) comm,infile
      character(172) command
ccc      character(20) sub_coms(20)
      character*(*) sub_coms(*)
      integer phsize,chatter,nchan,n_sub
      integer grping(*),qualty(*),channel(*)
      real syserr(*)
      logical qsys,qqual,qgroup,clobber
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c
c Arguments ...
c
c chatter     int   : Chattines flag
c phsize      int   : Array dimensions
c qualty      int   : Array of qualty flag
c grping      int   : Array of grouping flag
c serr        real  : Array of Observed statistical errors
c syserr      real  : Array of Fractional systematic error
c qgroup     logical: True if data is grouped
c qqual      logical: True if data has qualty flags
c qerror     logical: True if statistical errors included
c qsys       logical: True if systematic errors included
c comm        char  : Command string
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c Rehana Yusaf (Jan 27 1995) 1.0.0;
c Rehana Yusaf (March 22 1995) 1.0.1; add space between min max 
c                                     channel ranges when dumped
c Rehana Yusaf (1995 April 28) 1.0.2; increase comand from 136 to 172
c
c Banashree Mitra Seifert (1996 March) 1.1.0:
c         . Introduced screen display routines
c --------------------------------------------------------------------
      character(9) subname 
      character(5) version
      parameter (version = '1.1.0')
*-
c --------------------------------------------------------------------
c
c
c --- INTERNALS ------------------------------------------------------
c
      character(70) subinfo,datastr
      real fract,curerror
      integer i,min(2),max(2),igrps,nclash,n_ill,ounit
      character(120) ill_files(5),filename
      integer curqual,singles,ierr
      integer singend,zeroend,zeros
      integer minchan1,maxchan1,minchan2,maxchan2,prev_nbin,nbin
      logical wr_grp1,wr_grp2,wr_qual1,wr_qual2,wr_sys1,wr_sys2
      logical stbin,inbin,in_sing,wr_sing
      logical wr_zero,in_zero,val_sub
      logical valfil,execute
c
c --- USER INFO ---
c
      subname = 'dump_com'
      subinfo = 'using '//subname//' Ver '// version
      call wtinfo(chatter,10,2,subinfo) 

      singend=0
      zeroend=0
      ierr = 0
      execute = .false.
      n_ill = 1
C PDW 6/23/98: Call ftrtnm to strip off any extension number
C     ill_files(1) = infile
      call ftrtnm(infile, ill_files(1), ierr)
      call par_sub(comm,sub_coms,n_sub,command,val_sub,nclash,chatter)

      IF (command(6:13).EQ.'GROUPING') THEN
        execute = .true.
        IF (.NOT.qgroup) THEN
          subinfo = 'The data is not grouped'
          call wterrm(subname,version,subinfo)
          subinfo = ' therefore the grouping is not '
     &//'dumped to an ascii file'
          call wtinfo(chatter,0,1,subinfo) 
          return
        ENDIF
        filename = command(15:)
        call crmvlbk(filename)
        IF (filename.EQ.' ') THEN
           subinfo = ' dump filename required'
           call wterrm(subname,version,subinfo)
           subinfo = ' SYNTAX : DUMP GROUPING filename'
           call wtinfo(chatter,0,1,subinfo) 
           return
        ENDIF
        call ck_file(filename,ill_files,n_ill,valfil,
     &               clobber,chatter)
        IF (.NOT.valfil) THEN
           return
        ENDIF
        call ftgiou(ounit,ierr)
        IF (ierr.NE.0) THEN
          subinfo = ' problem getting free lun'
          call wterrm(subname,version,subinfo)
          return
        ENDIF
        call opasci(ounit,filename,2,80,clobber,chatter,ierr) 
        IF (ierr.NE.0) THEN
          subinfo = ' opening DUMP file'
          call wterrm(subname,version,subinfo)
          return
        ENDIF

c --- FIND GROUPED DATASETS ---

          stbin = .true.
          inbin = .false.
          igrps = 0
          singles = 0
          wr_grp1 = .false.
          wr_grp2 = .false.
          in_sing = .false.
          wr_sing = .false.
          wr_zero = .false.
          in_zero = .false.
          zeros = 0

          do i=1,nchan+ 1
            IF (grping(i).EQ.1) THEN

c --- Check for single channels 

              IF (grping(i+1).EQ.1) THEN
                singles = singles + 1
                in_sing = .true.

c --- Check for end of bin,when single (ungrouped) channels start

                IF (inbin) THEN
                  max(igrps) = channel(i-1)
                  inbin = .false.
                  stbin = .true.

c --- Check if bin factor has changed

                  IF (igrps.GT.1) THEN
                    prev_nbin = max(igrps-1) - min(igrps-1) + 1
                    nbin = max(igrps) - min(igrps) + 1
                    IF (prev_nbin.NE.nbin) THEN        
                      wr_grp1 = .true.
                      maxchan1 = max(igrps-1)
                      wr_grp2 =.true.
                      minchan2 = min(igrps)
                      maxchan2 = max(igrps)
                    ELSE
                      wr_grp1 = .true.
                      maxchan1 = max(igrps)
                    ENDIF
                  ELSE
                    wr_grp1 = .true.
                    maxchan1 = max(igrps)
                    prev_nbin = max(igrps) - minchan1 + 1
                  ENDIF          
                  igrps = 0
                ENDIF

c --- Check for end of zero (undefined) grouping
 
                IF (in_zero) THEN
                  in_zero = .false.
                  wr_zero =.true.
                  zeroend = channel(i-1)
                ENDIF          
              ELSE
                IF (in_zero) THEN
                  in_zero = .false.
                  wr_zero =.true.
                  zeroend = channel(i-1)
                ENDIF

c --- Check for end of single (ungrouped) channels

                IF (in_sing) THEN
                  in_sing = .false.
                  wr_sing = .true.
                  IF (grping(i+1).EQ.-1) THEN
                    singend = channel(i-1)
                  ELSE
                    singend = channel(i)
                  ENDIF
                ENDIF
                IF (i.EQ.nchan) THEN
                  IF (.NOT.wr_sing) THEN
                    wr_sing = .true.
                  ENDIF
                  singend = channel(i)
                  in_sing =.false.
                ENDIF

c --- Check for start of grouped channels

                IF (grping(i+1).EQ.-1) THEN
                 IF (stbin) THEN  
                   IF ((i+1).LE.nchan) THEN
                     igrps = igrps + 1
                     min(igrps) = channel(i)
                     minchan1 = min(igrps)
                     inbin = .true.
                     stbin = .false.
                   ENDIF

c --- Check for end of grouped data when grping flag is 1

                 ELSE 
                   max(igrps) = channel(i-1)

c --- Check if grouping factor has changed

                   IF ((igrps.GT.1).AND.
     &               (singend.NE.(min(igrps)-1)).AND.
     &               (zeroend.NE.(min(igrps)-1))) THEN
                     prev_nbin = max(igrps-1) - min(igrps-1) + 1
                     nbin = max(igrps) - min(igrps) + 1
                     IF (prev_nbin.NE.nbin) THEN
                       wr_grp2 = .true.
                       minchan2 = minchan1
                       maxchan2 = max(igrps-1)
                       nbin = prev_nbin
                       min(igrps-1) = min(igrps)
                       max(igrps-1) = max(igrps)
                       igrps = igrps - 1
                       minchan1 = min(igrps)
                     ELSE
                       min(igrps-1) = min(igrps)
                       max(igrps-1) = max(igrps)
                     ENDIF
                   ENDIF
                   IF (((i+1).LE.nchan).AND.(igrps.EQ.1)) THEN
                     igrps = igrps + 1
                   ENDIF
                   min(igrps) = channel(i)
                  ENDIF
                ELSE
                  IF (inbin) THEN
                    max(igrps) = channel(i-1)
                    stbin = .true.
                    inbin = .false.
                    maxchan1 = max(igrps)
                    prev_nbin = max(igrps) - min(igrps) + 1
                    wr_grp1 = .true.
                  ENDIF
                ENDIF
              ENDIF

c --- Check for end of grouped channels when grping flag is 0

            ELSEIF (grping(i).EQ.0) THEN
              IF (inbin) THEN
                max(igrps) = channel(i-1)
                stbin = .true.
                inbin = .false.

c --- Check if grouping factor has changed

                IF ((igrps.GT.1).AND.
     &                   (singend.NE.(min(igrps)-1)).AND.
     &                   (zeroend.NE.(min(igrps)-1))) THEN
                  prev_nbin = max(igrps-1) - min(igrps-1) + 1
                  nbin = max(igrps) - min(igrps) + 1
                  IF (prev_nbin.NE.nbin) THEN
                    wr_grp1 = .true.
                    maxchan1 = max(igrps-1)
                    wr_grp2 = .true.
                    minchan2 = min(igrps)
                    maxchan2 = max(igrps)
                  ELSE
                    wr_grp1 = .true.
                    maxchan1 = max(igrps)
                  ENDIF
                ELSE
                  wr_grp1 = .true.
                  maxchan1 = max(igrps)
                  prev_nbin = max(igrps) - minchan1 + 1
                ENDIF
                igrps = 0

c --- Check for end of single (ungrouped) channels when grping flag is 0

              ELSEIF (in_sing) THEN
                in_sing = .false.
                wr_sing = .true.
                singend = channel(i-1)
              ENDIF
              
c --- Check for start of zero (undefined) channels

              IF (i.LT.nchan) THEN
                zeros = zeros + 1
                in_zero = .true.
              ENDIF

c --- If end of channels then set end of zero channels,wr_zero=true

              IF (i.EQ.nchan) THEN
                zeros = zeros + 1
                zeroend = channel(i)
                in_zero = .false.
                wr_zero = .true.                
              ENDIF
            ENDIF

c --- Print grouped channels to ascii file

            IF (wr_grp1) THEN
              write(datastr,500) minchan1,maxchan1,prev_nbin
              write(ounit,1010,IOSTAT=ierr) datastr
              IF (ierr.NE.0) THEN
                subinfo = ' writing grouping to file'
                call wterrm(subname,version,subinfo)
                goto 99
              ENDIF
              wr_grp1 = .false.
            ENDIF
            IF (wr_grp2) THEN
              write(datastr,500) minchan2,maxchan2,nbin
              write(ounit,1010,IOSTAT=ierr) datastr
              IF (ierr.NE.0) THEN
                subname = ' writing grouping to file'
                call wterrm(subname,version,subinfo)
                goto 99
              ENDIF
              wr_grp2 = .false.
            ENDIF   

c --- Print single channels

            IF (wr_sing) THEN
              wr_sing = .false.
              singles = 0
            ENDIF
            
c --- Print zero (undefined) grouping channels

            IF (wr_zero) THEN
              wr_zero = .false.
              zeros = 0
            ENDIF         
          enddo
      ENDIF
      IF (command(6:12).EQ.'QUALITY') THEN
        execute = .true.
        IF (.NOT.qqual) THEN
          subinfo = ' Quality flags have not been set'
          call wterrm(subname,version,subinfo)
          subinfo = ' therefore the quality is not '
     &//'dumped to an ascii file'
          call wtinfo(chatter,0,1,subinfo)
          return
        ENDIF
        filename = command(14:)
        call crmvlbk(filename)
        IF (filename.EQ.' ') THEN
           subinfo = ' dump filename required'
           call wterrm(subname,version,subinfo)
           subinfo = ' SYNTAX : DUMP QUALITY filename'
           call wtinfo(chatter,0,1,subinfo)
           return
        ENDIF
        call ck_file(filename,ill_files,n_ill,valfil,
     &               clobber,chatter)
        IF (.NOT.valfil) THEN
           return
        ENDIF
        call cgetlun(ounit)
        call opasci(ounit,filename,2,80,clobber,chatter,ierr)
        IF (ierr.NE.0) THEN
          subinfo = ' opening DUMP file'
          call wterrm(subname,version,subinfo)
          return
        ENDIF
          
c --- FIND CHANNELS THAT ARE NOT GOOD ---

          stbin = .true.
          inbin = .false.
          curqual = 0
          wr_qual1= .false.
          wr_qual2 = .false.
          do i=1,nchan+1
            IF (qualty(i).NE.0) THEN
              IF (stbin) THEN
                curqual = qualty(i)
                minchan1=channel(i)
                inbin = .true.
                stbin = .false.
              ELSE
                IF (curqual.NE.qualty(i)) THEN
                  minchan2 = minchan1
                  maxchan2 = channel(i-1)
                  curqual = qualty(i)
                  wr_qual2 = .true.
                  minchan1 = channel(i)
                  IF (i.EQ.nchan) THEN
                    maxchan1 = channel(i)
                    wr_qual1 = .true.
                    inbin = .false.
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              stbin = .true.
              IF (inbin) THEN
                maxchan1 = channel(i-1)
                wr_qual1=.true.
              ENDIF
              inbin = .false.
            ENDIF
            IF (wr_qual2) THEN
              wr_qual2 = .false.
              write(datastr,600) minchan2,maxchan2
              write(ounit,1010,IOSTAT=ierr) datastr
              IF (ierr.NE.0) THEN
                subinfo = ' writing quality to ascii file'
                call wterrm(subname,version,subinfo)
                goto 99
              ENDIF
            ENDIF
            IF (wr_qual1) THEN
              wr_qual1 = .false.
              write(datastr,600) minchan1,maxchan1
              write(ounit,1010,IOSTAT=ierr) datastr
              IF (ierr.NE.0) THEN
                subinfo = ' writing quality to ascii file'
                call wterrm(subname,version,subinfo)
                goto 99
              ENDIF
            ENDIF
          enddo
      ENDIF
c
c --- DISPLAY SYSTEMATIC ERRORS ---
c
      IF (command(6:16).EQ.'SYSTEMATICS') THEN
        execute = .true.
        IF (.NOT.qsys) THEN
          subinfo = ' Systematic errors have not been set'
          call wterrm(subname,version,subinfo)
          subinfo = ' therefore the systematics are not '
     &//'dumped to an ascii file'
          call wtinfo(chatter,0,1,subinfo)
          return
        ENDIF
        filename = command(18:)
        call crmvlbk(filename)
        IF (filename.EQ.' ') THEN
           subinfo = ' dump filename required'
           call wterrm(subname,version,subinfo)
           subinfo = ' SYNTAX : DUMP SYSTEMATICS filename'
           call wtinfo(chatter,0,1,subinfo)
           return
        ENDIF
        call ck_file(filename,ill_files,n_ill,valfil,
     &               clobber,chatter)
        IF (.NOT.valfil) THEN
           return
        ENDIF
        call cgetlun(ounit)
        call opasci(ounit,filename,2,80,clobber,chatter,ierr)
        IF (ierr.NE.0) THEN
          subinfo = ' opening DUMP file'
          call wterrm(subname,version,subinfo)
          return
        ENDIF

          
c --- FIND CHANNELS THAT HAVE SYSTEMATIC ERRORS ---

          stbin = .true.
          inbin = .false.
          wr_sys1 = .false.
          wr_sys2 =.false.
          curerror  = 0
          do i=1,nchan+1
            IF (syserr(i).NE.0) THEN
              IF (stbin) THEN
                minchan1 = channel(i)
                curerror = syserr(i)
                inbin = .true.
                stbin = .false.
              ELSE
                IF (curerror.NE.syserr(i)) THEN
                  minchan2 = minchan1
                  maxchan2 = channel(i-1)
                  fract = curerror
                  curerror = syserr(i)
                  wr_sys2=.true.
                  minchan1 = channel(i)
                  IF (i.EQ.nchan) THEN
                    maxchan1 = channel(i)
                    wr_sys1 = .true.
                    inbin = .false.
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              stbin = .true.
              IF (inbin) THEN
                maxchan1 = channel(i-1)
                wr_sys1 = .true.
              ENDIF
              inbin = .false.
            ENDIF
            IF (wr_sys2) THEN
              write(datastr,700) minchan2,maxchan2,fract
              wr_sys2 = .false.
              write(ounit,1010,IOSTAT=ierr) datastr
              IF (ierr.NE.0) THEN
                subinfo = ' writing systematics to file'
                call wterrm(subname,version,subinfo)
                goto 99
              ENDIF
            ENDIF
            IF (wr_sys1) THEN
              write(datastr,700) minchan1,maxchan1,curerror
              wr_sys1 = .false.
              write(ounit,1010,IOSTAT=ierr) datastr
              IF (ierr.NE.0) THEN
                subinfo = ' writing systematics to file'
                call wterrm(subname,version,subinfo)
                goto 99
              ENDIF
            ENDIF
          enddo
      ENDIF
  99  close(ounit,IOSTAT=ierr)
      call ftfiou(ounit,ierr)
      IF (.NOT.execute) THEN
         subinfo  = ' Command not executed, Try Again !'
         call wterrm(subname,version,subinfo)
         subinfo  = ' SYNTAX : DUMP GROUP GRPFILE.DAT'
         call wtinfo(chatter,0,1,subinfo)
         subinfo  = ' SYNTAX : DUMP SYSTEMATICS SYSFILE.DAT'
         call wtinfo(chatter,0,1,subinfo)
         subinfo = ' SYNTAX : DUMP QUALITY QUALFILE.DAT'
         call wtinfo(chatter,0,1,subinfo)
         subinfo = ' see ''HELP DUMP'''
         call wtinfo(chatter,0,1,subinfo)
      ENDIF
 500  FORMAT(1X,I8,1X,I8,2X,I8)
 600  FORMAT(1X,I8,1X,I8,2X)
 700  FORMAT(1X,I8,1X,I8,2X,F6.4)
 1010 FORMAT(A70)
      return
      end
c --------------------------------------------------------------------
c     END OF DUMP_COM
c --------------------------------------------------------------------

*+HLP_DUMP
c     ---------------------------
      subroutine hlp_dump(chatter)
c     ---------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c Displays dump help
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      integer chatter,i,nhlp
      character(70) subinfo 
      character(70) hlp_strgs(40)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c chatter    int    : Chattines flag, >20 verbose
c i          int    : counter in loop
c desc       char   : user info
c hlp_strgs  char   : Used for screen display
c nhlp       int    : No. of help strings
c
c --- CALLED ROUTINES -----------------------------------------------
c
c subroutine FCECHO  : (FTOOLS) screen write
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1995 Jan 29)
c
c Banashree Mitra Seifert (March 1996) 1.1.0:
c            . Introduced screen display routines
c -------------------------------------------------------------------
      character(9) subname
      character(5) version
      parameter (version = '1.0.0')
*-
c -------------------------------------------------------------------
      subname = 'hlp_dump'
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)
c
c --- HELP STRGS ---
c
      hlp_strgs(1) =' '
      hlp_strgs(2) =' ----'
      hlp_strgs(3) =' DUMP'
      hlp_strgs(4) =' ----'
      hlp_strgs(5) =' These commands dump the current settings'
     &//' to an ascii file'
      hlp_strgs(6) =' '
      hlp_strgs(7) =' grppha>DUMP GROUP GRP_FILE.DAT'
      hlp_strgs(8) =' Dumps the current GROUPING to GRP_FILE.DAT'
     &//',each row of which lists'
      hlp_strgs(9) =' the  values of  the MINCHAN MAXCHAN NCHAN  for'
     &//' a given  group. The'
      hlp_strgs(10)=' grouping information can then be read'
     &//' into GRPPHA via  the command' 
      hlp_strgs(11)=' grppha>GROUP GRP_FILE.DAT'
      hlp_strgs(12)=' '
      hlp_strgs(13)=' grppha>DUMP SYSTEMATICS SYSFILE.DAT'
      hlp_strgs(14)=' Dumps current systematic errors to SYSFILE.DAT'
     &//', each row  of which '
      hlp_strgs(15)=' lists  the values of MINCHAN MAXCHAN ERR. These '
     &//' can then be  read '
      hlp_strgs(16)=' into GRPPHA via the command grppha> SYSTEMATICS'
     &//' SYSFILE.DAT'
      hlp_strgs(17)=' '
      hlp_strgs(18)=' grppha>DUMP QUALITY QUALFILE.DAT'
      hlp_strgs(19)=' Dumps current quality settings to QUALFILE.DAT'
     &//', each  row of which '
      hlp_strgs(20)=' lists MINCHAN MAXCHAN.This can be read '
     &//'into grppha via the command'
      hlp_strgs(21)=' grppha>BAD QUALFILE.DAT. NOTE: only the bad'
     &//' quality  channels  are '
      hlp_strgs(22) = ' dumped'
      hlp_strgs(23)=' '
      nhlp = 23
c
c --- PRINT HELP TO SCREEN ---
c
      do i=1,nhlp
        call wtinfo(chatter,0,1,hlp_strgs(i))
      enddo
      return
      end
c ------------------------------------------------------------------
c     END OF HLP_DUMP
c ------------------------------------------------------------------
