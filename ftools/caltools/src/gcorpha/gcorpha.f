
*+GCORPHA
      subroutine gcorpa

c --------------- description ------------------------------------------
c This task shifts the channels of an OGIP standard RMF file,
c by a specified amount.
c --------------- variables ---------------------------------------------

      implicit none
      character(180) infil,outfil,shftfil
      character(180) subinfo
      integer errflg, chatter
      logical killit
     
c --------------- variable directory ------------------------------------
c infil    char   input  input PHA file
c outfil   char   input  output file name
c shftfil  char   input  input ascii filename containing shift values
c
c --------------- authors/modifications ----------------------------------
c
c Banashree M Seifert (May, 1996) 1.0.0:
c
c Banashree M Seifert (Nov 19, 1996) 1.1.0:
c         . modification made so as to deal with first channel
c           as 0 or 1 only in subroutine pha_shifting
c
c Banashree M Seifert (July, 1997) 1.2.0:
c         . gcorpha was not doing same way as gcorrmf. So, pha_shifting
c           has been modified such that they comply with each other. To
c           do so, new variable tmppha and itmppha and terror array have 
c           been added
c Peter D Wilson (June 29, 1998) 1.2.1:
c         . Added max_xflt parameter to rdpha1 function call
c Peter D Wilson (July 01, 1998) 1.2.2:
c         . Updated for new FCPARS behavior
c-----------------------------------------------------------------------

      character(5) version
      parameter (version = '1.2.2')
      character(8) taskname
      taskname ='GCORPHA'
*-
c --------------- get parameters -----------------------------------------
      call gcorpha_gp(infil,outfil,shftfil,killit,chatter,errflg)

      if(errflg .ne. 0) then
         subinfo='returning from gcorpha_gp'
         call wterrm(taskname,version,subinfo)
         go to 100
      endif

      call wtbegm(taskname,version,chatter)
    

c --------------------- do shifting of channels ------------------------

      call do_shift(infil,shftfil,outfil,taskname,version,
     >              errflg,killit,chatter)
 
      if(errflg .ne. 0) then
         subinfo='returning from do_shift'
         call wterrm(taskname,version,subinfo)
         go to 100
      endif

100   call wtendm(taskname,version,errflg,chatter)
      
      end
c --------------------------------------------------------------------
c                 end of main gcorpha
c --------------------------------------------------------------------

*+GCORPHA_GP

      subroutine gcorpha_gp(infil,outfil,shftfil,killit,chatter,errflg)

c ----------------- description ---------------------------------------
c reads user defined parameters (input file, output file and shift file 
c (where shift is defined) for the task
c ----------------- variables ----------------------------------------
      implicit none
      character*(*) infil, outfil, shftfil   
      integer chatter,errflg
      logical killit

c --------------- variable directory -----------------------------------
c
c infil    char : (input) PHA filename (user defined)
c outfil   char : (input) Output (FITS FILE) filename
c shftfil  char : (input) input ascii file for defining shift required
c chatter  int  : (input) chattiness flag 
c errflg   int  : error flag (no error=0), returned to main

c -------------- called routines ----------------------------------------

c --------------- authors/modification ---------------------------------
c
c Banashree M Seifert (May, 1996)
c
c Banashree M Seifert (Nov20, 1996) 1.1.0:
c           . minor cosmetics for error checking when outfile name
c             already exists
c Peter D Wilson (July 01, 1998) 1.1.1:
c           . Drop INQUIRE infil test. Incompatible with new filename syntax
c-----------------------------------------------------------------------
      character(5)  version
      parameter (version = '1.1.1')
      character(11) subname
*-
c --------------- internal declarations --------------------------------
      character(180) filename
      character(120) subinfo
      integer  status, extnum
      logical ext

c ----------------------------------------------------------------------
*-
      subname ='gcorpha_gp'
      subinfo  = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c ----------------- read in clobber -----------------------

      status = 0
      call uclgsb('clobber', killit, status)
      if (status .ne. 0) then
          subinfo = 'getting clobber parameter'
          call wterrm(subname,version,subinfo)
          killit =.false.
          status = 0
      endif

c ----------------- chatter parameter ------------------------------

      status = 0
      call uclgsi('chatter',chatter,status)
      if (status .ne. 0) then
          subinfo = 'getting chatter parameter'
          call wterrm(subname,version,subinfo)
          chatter = 9
          status = 0
      endif

c ----------------- get input file parameters --------------------------

      status = 0
      call uclgst('infil', infil, status)
      if (status .ne. 0) then
          subinfo = 'getting input file parameters !'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(infil)
      if (infil(1:2) .eq. '  ') then
          subinfo = 'input file has to be entered'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

C PDW 7/1/98: Don't bother! Let FTOPEN determine if file exists
C      call fcpars(infil, filename, extnum, status)
C      call crmvlbk(filename)
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if ((.NOT. ext) .OR. (filename  .eq.  '  ')) then
C          subinfo= 'input file does not exist !'
C          call wterrm(subname,version,subinfo)
C          subinfo = 'filename : '//filename
C          call wtinfo(chatter,0,2,subinfo)
C          errflg = 1
C          return
C      endif

c ---------------- outfile name ----------------------------------------

      call uclgst('outfil', outfil, status)
      if (status .ne. 0) then
          subinfo = 'getting output file parameter'
          call wterrm(subname,version,subinfo)
      endif
      call crmvlbk(outfil)

c ---- terminate with an error mesg if user does not give outfil -------

      if (outfil(1:2)  .eq.  '  ') then
          subinfo = 'output filename must be entered !'
          call wterrm(subinfo)
          errflg=1
          return
      endif

c ------------------ check validity of outfil --------------------------

      ext = .true.
      INQUIRE(FILE=outfil, EXIST=ext)
      if (ext) then
          subinfo='outfile already exists!'
          call wterrm(subname,version,subinfo)
          subinfo='try ! infront of outfile to overwrite'
          call wtinfo(chatter,0,2,subinfo)
          subinfo='e.g. !'//outfil
          call wtinfo(chatter,0,2,subinfo)
          errflg = 1
          return
      endif

c ---------------- shift file name -------------------------------------
      status=0
      call uclgst('shftfil',shftfil,status)
      if (status .ne. 0) then
          subinfo = 'getting shift file parameter'
          call wterrm(subname,version,subinfo)
      endif
      call crmvlbk(shftfil)
      ext = .true.
      INQUIRE(FILE=shftfil, EXIST=ext)
      if ((.NOT. ext) .or. (shftfil(1:2)  .eq.  '  ')) then
          subinfo= 'shift file does not exist !'
          call wterrm(subname,version,subinfo)
          subinfo = 'filename : '//shftfil
          call wtinfo(chatter,0,2,subinfo)
          errflg = 1
          return
      endif

      errflg = 0
      return
      end

c --------------------------------------------------------------------+
c                      end of get par 
c --------------------------------------------------------------------+

*+do_shift

      subroutine do_shift(infil,shftfil,outfil,taskname,ver,
     >                    errflg,killit,chatter)
c ------------------------------------------------------------------
c This subroutine does the required things to do for shift channel
c 1. gets the phasize from input PHA file
c 2. reads the input PHA file
c 3. reads the input shift file
c 4. does the shifting
c 5. writes the output file
c ------------------------------------------------------------------

      implicit none
      character*(*) infil,shftfil,outfil
      character*(*) taskname,ver
      integer errflg, chatter

c ----------------- internals -----------------------------------------

      integer maxpha
      character(100) subinfo
      integer iget,status

      character(20) tlscop,instrum,filter
      character(20) instr(9),outhdu(9,50),extname,outver(9,50)
      character(20) extnames(9,50)
      integer nsearch,next(50),ninstr
      integer iunit,nchan,n_xflt,max_xflt
      character(120) xflt(9)
      character(120) backfil,corfil,rmffil,arffil
      character(11) detnam,chantyp
      character(8) dmode
      character(5) phaversn,hduclas2
      integer fchan,detchan,dtype
      real texpos,areascal,backscal,corscal
      logical qgroup,qqual,qerror,qsys,pois
      integer p_group,p_qualty,p_chan,p_ipha,p_itmppha
      integer p_error,p_sysfrc,p_pha,p_tmppha,p_terror

      integer first_chan,last_chan,maxshft,nshft
      parameter (maxshft=10000)
      integer p_stbin,p_endbin,p_factor
      integer p_bin_factor,p_shft_fraction1,p_shft_fraction2
      
      integer ineed,block,ounit
      integer nk_hist,nk_comm
      character(120) history
      character(100) hist(10),comment(10)
      character(20) task
      logical killit

      integer nhdu,tothd,htype,spectrum_ext
      logical endfile

c --------------- author/modifications -------------------------------
c Banashree M Seifert (1996 May) 1.0.0:
c Peter D Wilson (June 29, 1998) 1.0.1:
c        . Added max_xflt parameter to rdpha1 function call
c
c -----------------------------------------------------------------
      character(9) subname
      parameter (subname ='do_shift')
      character(5) version
      parameter(version='1.0.1')
*_
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
 

      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c -----------------------------------------------------------------------
c open the file to get no. of PHA channel so that DMA can be done
c -----------------------------------------------------------------------

      call get_pha_size(infil,maxpha,errflg,chatter)

      if(errflg .ne. 0) then
         subinfo='returning from get_pha_size'
         call wterrm(subname,version,subinfo)
         return 
      endif

c ------------------ allocate DMA ------------------------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------

      p_error = 0
      p_terror = 0
      p_sysfrc = 0
      p_pha = 0
      p_tmppha = 0
      p_group = 0
      p_qualty = 0
      p_chan = 0
      p_ipha = 0
      p_itmppha = 0
      p_stbin = 0
      p_endbin = 0
      p_bin_factor = 0
      p_factor = 0
      p_shft_fraction1 = 0
      p_shft_fraction2 = 0

      iget=0
      status = 0

      call udmget(maxpha, 6, p_error, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      call udmget(maxpha, 6, p_terror, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 6, p_sysfrc, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4
  
      status = 0
      call udmget(maxpha, 6, p_pha, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 6, p_tmppha, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 4, p_group, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 4, p_qualty, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 4, p_chan, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 4, p_ipha, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4

      status = 0
      call udmget(maxpha, 4, p_itmppha, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxpha*4
 
      status = 0
      call udmget(maxshft, 4, p_stbin, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxshft*4

      status = 0
      call udmget(maxshft, 4, p_endbin, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxshft*4

      status = 0
      call udmget(maxshft, 6, p_bin_factor, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxshft*4

      status = 0
      call udmget(maxshft, 4, p_factor, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxshft*4

      status = 0
      call udmget(maxshft, 6, p_shft_fraction1, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxshft*4

      status = 0
      call udmget(maxshft, 6, p_shft_fraction2, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxshft*4

 50   ineed = 7*maxpha*4 + 6*maxshft*4

      write(subinfo, '(a,i10)')'DMAsize required for this task=', ineed
           call wtinfo(chatter,10,1,subinfo)
      write(subinfo,'(a,i10)')'total bytes of memory allocated =',iget
           call wtinfo(chatter,10,1,subinfo)

      if (status .ne. 0) then
          errflg = -1
          subinfo='failed to allocate dynamic memory '
          call wtinfo(chatter,0,1,subinfo)
          return
      endif

c ------------------------------------------------------------------
c DMA allocated and now open and read PHA file
c ---------------------------------------------------------------------
      
       status = 0
       extname='SPECTRUM'
       ninstr = 1
       instr(1) = 'SPECTRUM'
       nsearch = 50

       call mvext (0, infil, iunit, ninstr, instr, nsearch, next,
     >             outhdu, extnames, outver, extname, status, chatter)

       if (status .ne. 0) then
           subinfo='opening the input PHA file'
           call wtferr(subname,version,status,subinfo)
           call ftclos(iunit,status)
           subinfo = 'closing input PHA file'
           call wtferr(subname,version,status,subinfo)
           errflg=1
           return
       endif
      spectrum_ext=next(1)

      max_xflt=9
      call rdpha1(iunit,maxpha,nchan,tlscop,instrum,detnam,
     >            filter,detchan,texpos,areascal,backscal,
     >            corscal,backfil,corfil,rmffil,arffil,xflt,
     >            max_xflt,n_xflt,dmode,chantyp,phaversn,hduclas2,
     >            fchan,MEMI(p_chan),dtype,MEMI(p_ipha),
     >            MEMR(p_pha),qerror,MEMR(p_error),qsys,
     >            MEMR(p_sysfrc),qqual,MEMI(p_qualty),qgroup,
     >            MEMI(p_group),pois,errflg,chatter)

      if(errflg .ne. 0) then
         subinfo = 'reading PHA FILE'
         call wterrm(subname,version,subinfo)
      endif

c --------------- to find out how many extensions are there -----------------

      endfile =.false.
      nhdu = 1
      do while(.not. endfile)
        status = 0
        call ftmahd(iunit,nhdu,htype,status)
        if ((status .EQ. 107) .or. (status .eq. 207)) THEN
           endfile = .true.
           tothd = nhdu - 2
        endif
        nhdu = nhdu + 1
      enddo

c The input file is not closed since we need this file to copy other 
c existing extensions if any to the output file.  It is closed after
c that


c -----------------------------------------------------------------------
c opens the shift file (ascii file) and reads info for channel shift
c -----------------------------------------------------------------------

      first_chan=fchan
      last_chan =fchan+maxpha -1

      call read_shft(shftfil,first_chan,last_chan,nshft,
     >               MEMI(p_stbin),MEMI(p_endbin),MEMR(p_bin_factor),
     >               MEMI(p_factor),MEMR(p_shft_fraction1),
     >               MEMR(p_shft_fraction2),
     >               errflg,chatter)

      if(errflg .ne. 0) then
         subinfo = 'returning from reading shift file'
         call wterrm(subname,version,subinfo)
         return
      endif

c -------------------------------------------------------------------
c do the channel shifting
c -------------------------------------------------------------------

      call pha_shifting(maxpha,dtype,MEMI(p_ipha),MEMR(p_pha),
     >                  qerror,MEMR(p_error),MEMR(p_terror),
     >                  first_chan,last_chan,
     >                  nshft,MEMI(p_stbin),MEMI(p_endbin),
     >                  MEMR(p_bin_factor),MEMI(p_factor),
     >                  MEMR(p_shft_fraction1),MEMR(p_shft_fraction2),
     >                  MEMI(p_itmppha),MEMR(p_tmppha),
     >                  errflg,chatter)

      if(errflg .ne. 0) then
         subinfo = 'returning from pha_shifting'
         call wterrm(subname,version,subinfo)
         return
      endif
    
c -------------------------------------------------------------------
c writes to the output file
c -------------------------------------------------------------------   

      call copyphd(infil,outfil,killit,errflg,chatter)
      
      if(errflg .ne. 0) then
         subinfo = 'copying the primary header of the input PHA file'
         call wterrm(subname,version,subinfo)
         return
      endif

      status = 0
      call ftgiou(ounit,status)
      if (status .ne. 0) then
        subinfo = 'obtaining free lun'
        call wtferr(subname,version,status,subinfo)
        errflg = status
        return
      endif

      call ftopen(ounit,outfil,1,block,status)
      subinfo = ' opening new file:'//outfil
      If (status .ne. 0) then
        call wtferr(subname,version,status,subinfo)
        errflg = 1
        return
      endif

      nk_hist = 0
      nk_comm = 0

c -------------- write resultant pha to output file

      call wtpha1(ounit,chatter,nk_hist,hist,nk_comm,comment,
     >            tlscop,instrum,detnam,filter,phaversn,hduclas2,
     >            fchan,texpos,areascal,backfil,backscal,corfil,
     >            corscal,rmffil,arffil,detchan,chantyp,
     >            MEMI(p_chan),MEMR(p_pha),dtype,qerror,
     >            MEMR(p_error),qsys,MEMR(p_sysfrc),qqual,
     >            MEMI(p_qualty),qgroup,MEMI(p_group),nchan,errflg)
 
      if(errflg .ne. 0) then
         subinfo='returning from writing result to outfile '//outfil
         call wtinfo(chatter,10,1,subinfo)
         return
      endif

c ------------- write extra history ---------------------------------

      status = 0
      call ftpdat(ounit,status)

      status = 0
      task = taskname //version 
      call ftpkys(ounit,'CREATOR',task,
     & ' s/w task which created this dataset',status)
      history = 'The original file on which channel shift performed'
      status = 0
      call ftphis(ounit,history,status)
      history = 'was '// infil
      call ftphis(ounit,history,status)

c -------------- write channel shift info -----------------------------

      call write_shift_info(ounit,MEMI(p_stbin), MEMI(p_endbin),
     >                      MEMR(p_bin_factor),nshft,
     >                      errflg,chatter)

      if(errflg .ne. 0) then
         subinfo='returning from writing shift info to outfile '//outfil
         call wtinfo(chatter,10,1,subinfo)
         return
      endif

c now copy other extensions except SPECTRUM

      subinfo='copying other extensions'
      call wtinfo(chatter,10,2,subinfo)

      call ftcrhd(ounit,status)
      call ftcopy(iunit, ounit, 0, status)

      call ftclos(iunit,status)
      subinfo = 'closing PHA file'
      call wtferr(subname,version,status,subinfo)
      call ftfiou(iunit,status)

      call ftclos(ounit,status)
      subinfo = 'closing PHA file'
      call wtferr(subname,version,status,subinfo)
      call ftfiou(ounit,status) 

 100  continue  

c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_error, 6, status)
      status = 0
      call udmfre(p_terror, 6, status)
      status = 0
      call udmfre(p_sysfrc, 6, status)
      status = 0
      call udmfre(p_pha, 6, status)
      status = 0
      call udmfre(p_tmppha, 6, status)
      status = 0
      call udmfre(p_group, 4, status)
      status = 0
      call udmfre(p_qualty, 4, status)
      status = 0
      call udmfre(p_chan, 4, status)
      status = 0
      call udmfre(p_ipha, 4, status)
      status = 0
      call udmfre(p_itmppha, 4, status)
      status = 0
      call udmfre(p_stbin, 4, status)
      status = 0
      call udmfre(p_endbin, 4, status)
      status = 0
      call udmfre(p_bin_factor, 6, status)
      status = 0
      call udmfre(p_factor, 4, status)
      status = 0
      call udmfre(p_shft_fraction1, 6, status)
      status = 0
      call udmfre(p_shft_fraction2, 6, status)
 

      if (status .ne. 0) then
          subinfo= 'failed to de-allocate memory '
          call wterrm(subname,version,subinfo)
      endif

      return
      end

c --------------------------------------------------------------------
c                     end of do_shift
c --------------------------------------------------------------------

*+GET_PHA_SIZE

      subroutine get_pha_size(infil,maxpha,errflg,chatter)

c --------------------------------------------------------------------
c this routine returns (maxpha) the size of the PHA array        
c
c ------------------- variables ----------------------------------------

      implicit none
      character*(*) infil
      integer maxpha,errflg,chatter

c ------------------- internal variables -------------------------------
      integer iunit,nsearch, status,next(50),ninstr
      character(100) subinfo,comm
      character(20) instr(50),outhdu(9,50),outver(9,50),extname
      character(20) extnames(9,50)

c --------------- authors/modification ---------------------------------
c
c Banashree M Seifert (May 24, 1996) 1.0.0:
c
c -----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.0.0')
      character(13) subname
      parameter (subname='get_pha_size')
*-
c ---------------------------------------------------------------------

       subinfo = 'using '//subname//' Ver '//version
       call wtinfo(chatter,10,2,subinfo)

c ----------- opening input PHA file -------------------------------


       status = 0
       extname='SPECTRUM'
       ninstr = 1
       instr(1) = 'SPECTRUM'
       nsearch = 50

       call mvext (0, infil, iunit, ninstr, instr, nsearch, next,
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
           subinfo='moved to SPECTRUM extension'
           call wtinfo(chatter,10,1,subinfo)
       endif


c ----------- read the PHA size ----------------------------------

c read naxis2 (no. of channels)

       status = 0
       call ftgkyj(iunit,'NAXIS2',maxpha,comm,status)
       subinfo = 'reading NAXIS2'
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           errflg = 4
           return
       endif

       write(subinfo,'(a,i10)')'number of channels found = ',maxpha
       call wtinfo(chatter,10,1,subinfo)

       call ftclos(iunit,status)
       if(status .ne. 0) then
          subinfo='closing PHA file'
          call wterrm(subname,version,subinfo)
       endif

      return
      end

c ------------------------------------------------------------------
c               end of get_pha_size
c ------------------------------------------------------------------

*+READ_SHFT

      subroutine read_shft(shftfil,first_chan,last_chan,nshft,
     >                     stbin,endbin,bin_factor,factor,
     >                     shft_fraction1,shft_fraction2,
     >                     errflg,chatter)
 
c --------------------------------------------------------------------+
c This subroutine reads the user defined shift file which is an 
c ascii file 
c ------------------ variables ---------------------------------------+
      implicit none
      character*(*) shftfil
      integer first_chan,last_chan,errflg,chatter,nshft
      integer stbin(*),endbin(*),factor(*)
      real  bin_factor(*),shft_fraction1(*),shft_fraction2(*) 

c --------------------- authors/modifications -----------------------
c Banashree M Seifert (May 1996) 1.0.0:
c --------------------- internals ----------------------------------
      character(300) subinfo,info
      character(180) dumfil
      integer iunit,status,st_bin,end_bin
      real f
      integer lenact

      character(10) subname
      parameter (subname='read_shft')
      character(5) version
      parameter (version='1.1.0')
*-
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)


c -------------- get lun to open shft file ----------------------

      call ftgiou(iunit,errflg)
      if (errflg .ne. 0) then
        subinfo = 'problem getting free lun'
        call wterrm(subname,version,subinfo) 
        return
      endif

c --- open shftfile and read data from it -------------------------
c check whether endbin < startbin otherwise return with error
c check that the startbins are in increasing order of magnitude
c and donot overlap with the preceding one
c -----------------------------------------------------------------

      open(unit=iunit,file=shftfil,status='old')
 
      status=0
      nshft=0
     
 1    read(iunit,*,iostat=status,end=100) st_bin,end_bin,f

         if (status .ne. 0) then
             subinfo = 'invalid number in shft file'
             call wterrm(subname,version,subinfo)
             write(subinfo,*)'For your info, this is at line',
     >             nshft+1,' of the shift file' 
             call rmvexsp(subinfo,info)
             call wtinfo(chatter,9,0,info)
             errflg = 1
             return
         endif
     
         if (end_bin .gt. last_chan) then
             subinfo='chan no. for binning exceeds'
             call wtinfo(chatter,9,1,subinfo)
             subinfo='maximum chan of the input PHA file' 
             call wtinfo(chatter,9,1,subinfo)
             errflg = 1
             return
         endif

c        ------------------------------------------------------
c        checking the validity of bin shift as defined by user
c        ------------------------------------------------------

         if(st_bin .gt. end_bin) then
            write(subinfo,*)'start channel > end channel'
            call wterrm(subname,version,subinfo)
            write(subinfo,*)'This is in line ', nshft+1,
     >                      ' of the shift file'
            call wtinfo(chatter,9,1,subinfo)
            errflg=1
            return
         endif

         if (nshft .gt. 0) then
             if(st_bin .le. stbin(nshft)) then
                subinfo='channels should be in ascending order'
                call wterrm(subname,version,subinfo)
                write(subinfo,*)'For your info, this is at line',
     >                nshft+1,' of the shift file' 
                call rmvexsp(subinfo,info)
                call wtinfo(chatter,9,0,info)
                errflg=1
                return
             endif

             if(st_bin .le. endbin(nshft)) then
                 subinfo='overlap error in channel input'
                 call wterrm(subname,version,subinfo)
c MJT 10July96 (g77/linux) concatenation operator demands scalars..
                 write(dumfil,*) shftfil
                 write(subinfo,*)'For your info, this is at line',
c     >                nshft+1,' of the shift file ('//shftfil//')'
     >                nshft+1,' of the shift file ('//
     >                dumfil(1:lenact(dumfil))//')'

                 call rmvexsp(subinfo,info)
                 call wtinfo(chatter,9,0,info)
                 errflg=1
                 return
             endif
         endif

         nshft = nshft + 1
         stbin (nshft) = st_bin
         endbin(nshft) = end_bin
         bin_factor(nshft)=f
         factor(nshft) = int(f)  
         shft_fraction2(nshft)= abs(f - (int(f))) 
         shft_fraction1(nshft)= 1-shft_fraction2(nshft)

      goto 1

 100  continue 

      errflg=0
      return
      end 

c --------------------------------------------------------------------+
c                          end of read_shft
c --------------------------------------------------------------------+

*+PHA_SHIFTING
 
      subroutine pha_shifting(maxpha,dtype,ipha,pha,qerror,error,terror,
     >                        first_chan,last_chan,nshft,stbin,endbin,
     >                        bin_factor,factor,shft_fraction1,
     >                        shft_fraction2,itmppha,tmppha,
     >                        errflg,chatter)

c ---------------------------------------------------------------------
c This routine shifts the PHA channels according to BIN_FACTOR for
c various channels with STBIN and ENDBIN through the no. of shifts NSHFT
c ---------------------------------------------------------------------

      implicit none
      integer maxpha,dtype
      integer ipha(*),itmppha(*)
      integer first_chan,last_chan,nshft,stbin(*),endbin(*),factor(*)
      integer errflg,chatter
      real pha(*),tmppha(*),bin_factor(*),error(*), terror(*)
      real shft_fraction1(*),shft_fraction2(*)
      logical qerror
      
c ---------------- internals --------------------------------------
      character(180) subinfo
      integer ch,new_ch,i,ch_mv
      real errsq
      logical shift
c ------------------- authors/modifications ---------------------------
c Banashree M Seifert (May 1996) 1.0.0:
c
c Banashree M Seifert (Nov19 1996) 1.1.0:
c         . modifications done so as to work with first channel either
c           0 or 1
c           For this, one parameter ch_mv is introduced
c
c Banashree M Seifert (July 1997) 1.2.0:
c         . modifications made such that gcorpha works same as gcorrmf.
c           In doing so, new variables tmppha and itmppha and terror were 
c           needed to start with empty pha array.
c ---------------------------------------------------------------------
      character(13) subname
      parameter (subname='pha_shifting')
      character(5) version
      parameter (version='1.2.0')

*-
      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

      if(first_chan .eq. 0) then
         ch_mv=1
      else
         ch_mv=0
      endif
     
      do i=1,maxpha
         itmppha(i) = 0
          tmppha(i) = 0.
          terror(i) = 0.
      enddo

c Now go through nshft to shift the channels

      do i=1,nshft
         
c ------ if shift is towards left (i.e, -ve shift)

         if(bin_factor(i) .lt. 0.) then
            ch=stbin(i)+ch_mv
            shift=.true.
            do while (shift)
               new_ch = ch + factor(i)
               if(new_ch .ge. first_chan+ch_mv) then 
                   if(dtype .eq. 1) then
                      itmppha(new_ch+1)= itmppha(new_ch+1)+
     >                                ipha(ch)*shft_fraction2(i)

                      if(qerror) then
                         errsq = terror(new_ch+1)*terror(new_ch+1)+
     >                             shft_fraction2(i)*shft_fraction2(i)*
     >                             error(ch)*error(ch)
                         terror(new_ch+1)=sqrt(errsq)
                      endif

                      if(factor(i) .eq. 0) then
                         itmppha(new_ch) = ipha(ch)*shft_fraction1(i)

                         if(qerror)then
                            errsq = shft_fraction1(i)*shft_fraction1(i)*
     >                              error(ch)
                            terror(new_ch) = sqrt(errsq)
                         endif

                      else
                         itmppha(new_ch) = itmppha(new_ch) + 
     >                                  ipha(ch)*shft_fraction1(i)

                         if(qerror)then
                            errsq = terror(new_ch)*terror(new_ch) +
     >                              shft_fraction1(i)*shft_fraction1(i)*
     >                              error(ch)
                            terror(new_ch) = sqrt(errsq)
                         endif
                      endif

c now if dtyep=2

                   else
                      tmppha(new_ch+1) = tmppha(new_ch+1) +
     >                                pha(ch)*shft_fraction2(i)
                  
                      if(qerror) then
                         errsq = terror(new_ch+1)*terror(new_ch+1) +
     >                           shft_fraction2(i)*shft_fraction2(i)*
     >                           error(ch)*error(ch)
                         terror(new_ch+1) = sqrt(errsq)
                      endif

                      if(factor(i) .eq. 0) then
                         tmppha(new_ch)= pha(ch)*shft_fraction1(i)
                         
                         if(qerror) then
                            errsq = shft_fraction1(i)*shft_fraction1(i)*
     >                              error(ch)*error(ch)
                            terror(new_ch)= sqrt(errsq)
                         endif

                      else
                         tmppha(new_ch)= tmppha(new_ch) + 
     >                                pha(ch)*shft_fraction1(i)
                          
                         if(qerror) then
                            errsq = terror(new_ch)*terror(new_ch)+
     >                              shft_fraction1(i)*shft_fraction1(i)*
     >                              error(ch)*error(ch)
                            terror(new_ch) = sqrt(errsq)
                         endif

                      endif

                   endif

               else

               endif

               ch = ch+1
               if(ch .gt. endbin(i)+ch_mv) then
                  shift=.false.
               endif
            enddo 

c ------ if shift is towards right and also included the case when shift=0

         else
            ch=endbin(i)+ch_mv
            shift=.true.
            do while (shift)
               new_ch = ch + factor(i)
               if (new_ch .le. last_chan+ch_mv)then
            
                   if(dtype .eq. 1) then
                      if(new_ch+1 .le. last_chan+ch_mv) then 
                         itmppha(new_ch+1) = itmppha(new_ch+1) +
     >                                 ipha(ch)*shft_fraction2(i)

                         if(qerror)then
                            errsq = terror(new_ch+1)*
     >                              terror(new_ch+1) +
     >                              shft_fraction2(i)*shft_fraction2(i)*
     >                              error(ch)*error(ch)
                            terror(new_ch+1) = sqrt(errsq)
                         endif
                      endif

                      if(factor(i) .eq. 0) then
                         itmppha(new_ch) = ipha(ch)*
     >                                        shft_fraction1(i)

                         if(qerror)then
                            errsq = shft_fraction1(i)*shft_fraction1(i)*
     >                              error(ch)*error(ch)
                            terror(new_ch) = sqrt(errsq)
                         endif

                      else
                         itmppha(new_ch) = itmppha(new_ch) + 
     >                                  ipha(ch)*shft_fraction1(i)

                         if(qerror)then
                            errsq = terror(new_ch)*terror(new_ch) +
     >                              shft_fraction1(i)*shft_fraction1(i)*
     >                              error(ch)*error(ch)
                            terror(new_ch) = sqrt(errsq)
                         endif
                      endif
                   else
                      if(new_ch+1 .le. last_chan+ch_mv) then
                         tmppha(new_ch+1) = tmppha(new_ch+1) +
     >                                   pha(ch)*shft_fraction2(i)

                         if(qerror)then
                            errsq = terror(new_ch+1)*
     >                              terror(new_ch+1) +
     >                              shft_fraction2(i)*shft_fraction2(i)*
     >                              error(ch)*error(ch)
                            terror(new_ch+1) = sqrt(errsq)
                         endif
                      endif

                      if(factor(i) .eq. 0) then
                         tmppha(new_ch)= pha(ch)*
     >                                      shft_fraction1(i)
                         if(qerror)then
                            errsq = shft_fraction1(i)*shft_fraction1(i)*
     >                              error(ch)*error(ch)
                            terror(new_ch) = sqrt(errsq)
                         endif

                      else
                         tmppha(new_ch)= pha(new_ch) + 
     >                                pha(ch)*shft_fraction1(i)

                         if(qerror)then
                            errsq = terror(new_ch)*
     >                              terror(new_ch) +
     >                              shft_fraction1(i)*shft_fraction1(i)*
     >                              error(ch)*error(ch)
                            terror(new_ch) = sqrt(errsq)
                         endif

                      endif

                   endif

               else

               endif 
               ch=ch-1
               if(ch .lt. stbin(i)+ch_mv) then
                  shift=.false.      
               endif
            enddo

      endif

      enddo

      subinfo='PHA channels are shifted' 
      call wtinfo(chatter,10,1,subinfo)

c ----------------------------------------------------------------------
c changing ipha to pha to write in output as the writer subroutine
c accepts as real instead of integers
c ----------------------------------------------------------------------

      if (dtype .eq. 1) then
          do i=1,maxpha
             pha(i)= real(itmppha(i))
          enddo
      else
         do i=1,maxpha
             pha(i)= tmppha(i)
          enddo

      endif

      if(qerror)then
         do i=1,maxpha
            error(i) = terror(i)
         enddo 
      endif

      return
      end

c -------------------------------------------------------------------------
c                    end of pha_shifting
c -------------------------------------------------------------------------

*+WRITE_SHIFT_INFO

      subroutine write_shift_info(ounit,stbin,endbin,bin_factor,nshft,
     >                            errflg,chatter)

c ---------------------------------------------------------------------
c This routine writes the input shift info into the outfile as comments
c This need to be as a subroutine since dynamic memory has been allocated
c to these parameters in the called routine DO_SHIFT 
c ----------------------------------------------------------------------

      implicit none
      integer ounit,nshft,stbin(*),endbin(*)
      integer errflg, chatter
      real bin_factor(*)

c ------------------ internal variables -------------------------------
      character(100) subinfo
      character(120) bin_history,history
      integer i,status

c ---------------- authors/modifications ------------------------------
c Banashree M Seifert (May 1996) 1.0.0:
c
c ---------------------------------------------------------------------

      character(17) subname
      parameter (subname='write_shift_info')
      character(5) version
      parameter (version='1.0.0')
*-
c ----------------------------------------------------------------------

      subinfo='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)

c ----------------------------------------------------------------------

      history='channel shift information as follows:'
      call ftpcom(ounit,history,status)
      write(history,*)'      from   ','       to     ', '     by '
      call ftpcom(ounit,history,status)


      do i=1,nshft
         write(bin_history,100)stbin(i),endbin(i),bin_factor(i)
         call ftpcom(ounit,bin_history,status)
      enddo
 100  format(i10,2x,i10,2x,f10.2)
  
      errflg=0
      return
      end

c ----------------------------------------------------------------------
c                end of write_shift_info subroutine
c ----------------------------------------------------------------------


