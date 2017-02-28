
*+st2rpsf
      subroutine st2rpf
c
c --- DESCRIPTION ---------------------------------------------------------
c
c ST2RPSF reads a STW format FITS file and writes required data to an output
c file in CALRPSF format FITS file, that is an OGIP standard format for
c Radial profiles.
c
c ---------------------------- VARIABLES ------------------------------+

      IMPLICIT NONE
      character(180) infil,outfil
      character(100) source, subinfo
      character(8) telstw,inststw,telescope,instrume
      integer maxrad,maxtheta,chanmin,chanmax
      integer p_cts_arc2, p_err_arc2
      integer p_rad_lo, p_rad_hi, p_rad1, p_rad2
      integer p_net, p_neterr, p_area_wgt
      real arc_pix,pixsize, bkgd, sumrcts, bkgd_rad, sumtcts
      integer npts,chatter,extno
      integer p_n_pix
      integer ineed, iget, status, ierr
      logical cont,calc,killit
c
c --- VARIABLE DIRECTORY ---
c
c infil      char   : STW file name (user defined)
c outfil     char   : CALRPSF file name (user defined)
c maxrad     int    : Array dimensions
c net        real   : Array of observed radial profile in counts
c neterr     real   : Array of errors on net
c cts_arc2   real   : Array of observed radial profile in counts/arcmin^2
c err_arc2   real   : Array of error on cts_arc2
c rad_lo     real   : Array of lower edge of radial bins (in arcmin)
c rad_hi     real   : Array of upper edge of radial bins (in arcmin)
c rad1       real   : Array of lower edge of radial bins (in pixels)
c rad2       real   : Array of upper edge of radial bins (in pixels)
c n_pix      int    : Array of No of pixels in bins
c area_wgt   real   : Area weighting factor 
c arc_pix    real   : arcmins per pixel
c telstw     char   : telescope name
c inststw    char   : Instrument/Detector name
c pixsize    real   : Pixsize in degrees
c npts       int    : Counter for observed psf data
c chatter    int    : Chattiness flag (<5 quiet,>5normal,>20 noisy)
c subinfo    char   : Routine info for user
c cont      logical : IF false program stops
c 
c ---------------------- CALLED ROUTINES ------------------------------
c
c subroutine RPSF_GP    : Reads user defined parameters using XPI
c subroutine RPSF_RSTW  : Reads FITS format STW file
c subroutine RPSF_CONV  : Converts data into desired format
c subroutine RPSF_WT    : Writes data into calrpsf format output file
c
c ------------------ AUTHORS/MODIFICATION -----------------------------
c
c Rehana Yusaf (1993 Feb)
c Rehana Yusaf (1993 March 3)
c Rehana Yusaf (1993 August 17) Ver 1.0.2; update rpsf_gp, rpsf_src
c                               more error checking
c Rehana Yusaf (1993 September 3) 1.0.3; New more general radial profile
c                                 writing routine is used WT_RPSF1993a
c Rehana Yusaf (1994 Jan 13) 1.0.4; WT_RPSF1993a has been renamed to
c                                 WTRPF1, and HDUCLAS has been added.
c                                 New parameters - CHANMIN, CHANMAX
c                                 TELESCOPE and INSTRUMENT and BKGD
c                                 added 
c Rehana Yusaf (1994 Feb 3) 1.0.5; Write SUMRCTS to outfile
c Rehana Yusaf (1995 Jan 13) 1.0.6; update _gp to no longer use defval
c Rehana Yusaf (1995 Feb 21) 1.0.7; update to add option of calculating
c                                   the bkgd
c Rehana Yusaf (1995 April 25) 1.0.8; update to add clobber
c Banashree Mitra Seifert (1995 Dec) 2.0.0; modified to  
c           . filename = character(180)
c           . HDUCLAS introduced
c           . replaces calls to FNDHDU/EXT,FTMRHD,FTMAHD by MVEXT
c           . DMA introduced
c           . error checking modified
c           . screen display subroutines used
c             wtbegm,wtendm,wtinfo,wterrm,wtwarm 
c Peter D Wilson (1998 Jul 01) 2.0.1:
c           . Updated for new FCPARS behavior
c ----------------------------------------------------------------------
      character(5) version
      parameter (version = '2.0.1')
*-
c ------------- dynamic memory allocated array -------------------------
c      real cts_arc2(maxrad,maxtheta,1),err_arc2(maxrad,maxtheta,1)
c      real rad_lo(maxrad), rad_hi(maxrad),rad2(maxrad)
c      real rad1(maxrad),area_wgt(maxrad,maxtheta,1)
c      real net(maxrad),neterr(maxrad)
c      integer*4 n_pix(maxrad)
c ---------------- DYNAMIC MEMORY ALLOCATION --------------------------
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
c
      character(40) taskname
      character(7) subname

cccc      COMMON/task/taskname

      taskname = 'st2rpsf'
      subname ='st2rpsf'
      cont = .true.
c
c ---------------- GET PARAMETERS ------------------------------------
c
      call rpsf_gp(infil,extno,telescope,instrume,chanmin,chanmax,
     >             bkgd,outfil,calc,bkgd_rad,cont,chatter,killit)

      call wtbegm(taskname,version,chatter)

      IF (.NOT.cont) THEN
        go to 100
      ENDIF
       
      maxrad   = 1000
      maxtheta = 50

c ----------------------- Allocation of DMA ----------------------
c iget = bytes get added  after each call for UDMGET
c        (this is the actual count of bytes I am asking for)
c just to keep a count on how much memory is asking for
c ----------------------------------------------------------------
      iget=0
      status = 0

      p_rad_lo = 0
      p_rad_hi = 0
      p_rad1 = 0
      p_rad2 = 0
      p_net = 0
      p_neterr = 0 
      p_cts_arc2 = 0
      p_err_arc2 = 0
      p_area_wgt = 0
      p_n_pix = 0

      call udmget(maxrad, 6, p_rad_lo, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*4

      status = 0
      call udmget(maxrad, 6, p_rad_hi, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*4

      status = 0
      call udmget(maxrad, 6, p_rad1, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*4

      status = 0
      call udmget(maxrad, 6, p_rad2, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*4

      status = 0
      call udmget(maxrad, 6, p_net, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*4

      status = 0
      call udmget(maxrad, 6, p_neterr, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*4

      status = 0
      call udmget(maxrad*maxtheta*1, 6, p_cts_arc2, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*1*4

      status = 0
      call udmget(maxrad*maxtheta*1, 6, p_err_arc2, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*1*4

      status = 0
      call udmget(maxrad*maxtheta*1, 6, p_area_wgt, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+maxrad*maxtheta*1*4

      status = 0
      call udmget(maxrad, 4, p_n_pix, status)
      if (status .ne. 0) then
          goto 50
      endif
      iget = iget+2*maxrad*4

  50   ineed = 8*maxrad*4 + 3*maxrad*maxtheta*1*4 
      write(subinfo, '(a,i10)')'Memory requirements for this task  =', 
     >                         ineed
      call wtinfo(chatter,15,2,subinfo)
      write(subinfo,'(a,i10)')'Total memory dynamically allocated =',
     >                         iget
      call wtinfo(chatter,15,2,subinfo)

      if (status .ne. 0) then
         ierr = -1
         subinfo=' failed to allocate dynamic memory '
         call wterrm(subname,version,subinfo)
         goto 100
      endif
c ---------------------------------------------------------------------+
c                    End of DMA allocation 
c ---------------------------------------------------------------------+
c ------------------------ READ STW file ------------------------------+
c
      call rpsf_rstw(infil, extno, MEMI(p_n_pix), MEMR(p_net),
     >               MEMR(p_neterr), MEMR(p_rad1), MEMR(p_rad2), 
     >               source, npts, maxrad, telstw, inststw,
     >               cont, chatter)
      IF (.NOT.cont) THEN
        go to 100
      ENDIF
c
c ------ Check that telescope/instrument values that are read ----------
c ------------ from the file and defined by user match -----------------
c
      IF (telstw.NE.'UNKNOWN') THEN
        IF (telstw.NE.telescope) THEN
          subinfo = ' Telescope defined by user : '//telescope
          call wtinfo(chatter,1,1,subinfo)
          subinfo = ' Telescope value read from STW file : '//telstw
          call wtinfo(chatter,5,2,subinfo)
          subinfo = 'mismatch in defining telescope!'
          call wtwarm(subname,version,chatter,5,subinfo) 
          subinfo = 'User defined value used'
          call wtinfo(chatter,5,1,subinfo)
        ENDIF
      ENDIF

      IF (inststw.NE.'UNKNOWN') THEN
        IF (inststw.NE.instrume) THEN
          subinfo = ' Instrument defined by user : '//instrume
          call wtinfo(chatter,1,1,subinfo)
          subinfo = ' Instrument value read from STW file : '//inststw
          call wtinfo(chatter,1,1,subinfo)
          subinfo = 'mismatch in defining instrument!'
          call wtwarm(subname,version,chatter,5,subinfo)
          subinfo = 'User defined value used'
          call wtinfo(chatter,5,1,subinfo)
        ENDIF
      ENDIF
      telstw = telescope
      inststw = instrume 

c ---------------------------------------------------------------------+
c            CONVERT TO OGIP RPSF FORMAT            
c ---------------------------------------------------------------------+
      call rpsf_conv(MEMR(p_rad1), MEMR(p_rad2), MEMR(p_rad_lo),
     >               MEMR(p_rad_hi), MEMR(p_net), MEMR(p_neterr), 
     >               MEMR(p_cts_arc2), MEMR(p_err_arc2), source,
     >               arc_pix, pixsize, sumrcts, sumtcts, npts,
     >               maxrad,maxtheta, MEMI(p_n_pix), MEMR(p_area_wgt),
     >               bkgd, calc, bkgd_rad, cont, chatter)
      IF (.NOT.cont) THEN
          goto 100
      ENDIF
c ---------------------------------------------------------------------+
c                   WRITE RPSF FILE 
c ---------------------------------------------------------------------+
      call rpsf_wt(outfil, infil, version, MEMR(p_rad_lo), 
     >             MEMR(p_rad_hi), MEMR(p_cts_arc2), MEMR(p_err_arc2), 
     >             MEMR(p_area_wgt), pixsize, npts, maxrad, maxtheta, 
     >             telstw, inststw, bkgd, chanmin, chanmax,
     >             sumrcts, sumtcts, cont, chatter, killit)

      IF (.NOT.cont) THEN
          goto 100
      ENDIF   

c --------- free the dynamic memory -----------------------------

      status = 0
      call udmfre(p_rad_lo, 6, status)
      status = 0
      call udmfre(p_rad_hi, 6, status)
      status = 0
      call udmfre(p_rad1, 6, status)
      status = 0
      call udmfre(p_rad2, 6, status)
      status = 0
      call udmfre(p_net, 6, status)
      status = 0
      call udmfre(p_neterr, 6, status)
      status = 0
      call udmfre(p_cts_arc2, 6, status)
      status = 0
      call udmfre(p_err_arc2, 6, status)
      status = 0
      call udmfre(p_area_wgt, 6, status)
      status = 0
      call udmfre(p_n_pix, 4, status)

      if (status .ne. 0) then
          subinfo= ' failed to de-allocate memory '
          call wterrm(subname,version,subinfo)
          ierr=99
      endif

100   call wtendm(taskname,version,ierr,chatter)

      return
      end
c ----------------------------------------------------------------------
c                        END OF ST2RPSF 
c ----------------------------------------------------------------------

*+RPSF_GP
c ---------------------------------------------------------------------+
      subroutine rpsf_gp(infil, extno, telescope, instrume, chanmin, 
     >                   chanmax, bkgd, outfil, calc, bkgd_rad, cont, 
     >                   chatter, killit)

c -------------------------- DESCRIPTION ------------------------------
c
c This routine gets the user defined parameters, that is the input and
c output file names . In addition the Chatter flag is obtained.
c
c -------------------------- VARIABLES -------------------------------

      IMPLICIT NONE
      character*(*) infil, outfil
      character(180)  file, strbkgd, ill_files(5)
      character*(*) telescope, instrume
      character(80) errmess, subinfo,rdstr, rdstr2
      integer chanmin, chanmax, extno, try, n_ill
      integer err, chatter
      real bkgd, bkgd_rad
      logical cont, ext, calc, killit, valfil
c
c ------------------------ VARIABLE DIRECTORY ------------------------
c
c infil      char   : Name of FITS STW PSF file (user defined) 
c outfil     char   : Name of PSF output file (user defined)   
c err        int    : Error flag
c chatter    int    : Chattiness flag (<5 quiet,>5normal,>20 noisy)
c version    char   : Version of subroutine
c errstr     char   : Error string
c errmess    char   : Error message
c extno      int    : Extension number of RPSF data (default is 1)
c cont      logical : False, If infil does not exist
c ext       logical : True if infil exists
c
c ---------------------- COMPILATION AND LINKING ---------------------
c
c Link with HOST and FTOOLS
c 
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (1993 Feb)
c Rehana Yusaf (1993 August 17) Ver 1.0.1; Remove leading blanks from 
c                               infile and outfile
c Rehana Yusaf (1994 Jan 13) 1.0.2; Add new parameters TELESCOP and
c			        INSTRUM - also CHANMIN and CHANMAX
c                               (dependent on telescop and instrume  
c                                values). BKGD is also prompted for. 
c				 Also extname is no longer read
c Rehana Yusaf (1995 Jan 13) 1.0.3; defval is no longer read from parfile
c Rehana Yusaf (1995 Feb 21) 1.0.5; read in calc, if calc then code
c                                   calculates the bkgd
c Rehana Yusaf (1995 April 25) 1.0.6; read in clobber 
c
c Banashree Mitra Seifert (1996, Jan) 1.1.0:
c            . Screen display routines used
c              wterrm,wtinfo,wtwarm
c Peter D Wilson (1998 Jul 01) 1.1.1:
c            . Drop INQUIRE test on infil
c -----------------------------------------------------------------------
       character(5) version
       parameter (version = '1.1.1')
       character(7) subname
*-
c -----------------------------------------------------------------------
       subname='rpsf_gp'
       subinfo='using '//subname//' '//version
       call wtinfo(chatter,10,1,subinfo)
c
c ---------------- READING INPUT FILE PARAMETER --------------------------- 

       err=0
       call uclgst('infil',file,err)
       IF (err.NE.0) THEN
          errmess = ' Getting infil parameter'
          call wterrm(subname,version,errmess)
       ENDIF
       call crmvlbk(file)
       IF (infil.EQ.'  ') THEN
         errmess = ' Must enter infile name !'
         call wterrm(subname,version,errmess)
         cont = .false.
         returN
       ENDIF
       err = 0
       call fcpars(file,infil,extno,err)
       IF (extno.LE.0) THEN
         extno = 1
       ENDIF
C PDW 7/1/98: Don't bother! Let FTOPEN determine if file exists
C       INQUIRE(FILE=infil,EXIST=ext)
C       IF (.NOT.ext) THEN
C         errmess = 'Infile does not exist !'
C         call wterrm(subname,version,errmess)
C         cont = .false.
C         return
C       ENDIF

c ------------------- READ TELESCOPE NAME -------------------------------

       call uclgst('telescope',rdstr,err)
       IF (err.NE.0) THEN
          errmess = ' getting telescope parameter'
          call wterrm(subname,version,errmess)
       ENDIF
       call crmvlbk(rdstr)
       call rmvexsp(rdstr,rdstr2)
       telescope = rdstr2

c --------------------- READ INSTRUMENT NAME ------------------------------

       call uclgst('instrume',rdstr,err)
       IF (err.NE.0) THEN
          errmess = ' getting instrume parameter'
          call wterrm(subname,version,errmess)
       ENDIF
       call crmvlbk(rdstr)
       call rmvexsp(rdstr,rdstr2)
       instrume = rdstr2
       call ftupch(instrume)
       call ftupch(telescope)

c ------------ READ CHANMIN and CHANMAX if ROSAT PSPC --------------------

       IF ((telescope.EQ.'ROSAT').AND.(instrume(1:4).EQ.'PSPC')) THEN
         call uclgsi('chanmin',chanmin,err)
         If (err.NE.0) THEN
           errmess = ' getting chanmin parameter'
           call wterrm(subname,version,errmess)
         ENDIF
         call uclgsi('chanmax',chanmax,err)
         IF (err.NE.0) THEN
           errmess = ' getting chanmax parameter'
           call wterrm(subname,version,errmess)
         ENDIF
       ENDIF
       

c ------------------ READ BKGD ------------------------------------------

       calc = .false.
       try = 0
  90   call uclgst('bkgd',strbkgd,err)
       try = try + 1
       IF (err.NE.0) THEN
         errmess = ' getting bkgd parameter'
         call wterrm(subname,version,errmess)
       ENDIF
       call crmvlbk(strbkgd)
       call ftupch(strbkgd)
       IF (strbkgd(1:1).EQ.'C') THEN
         calc = .true.
         call uclgsr('bkgd_rad',bkgd_rad,err)
         IF (err.NE.0) THEN
           errmess = ' getting bkgd_rad parameter'
         call wterrm(subname,version,errmess)
         ENDIF
         IF (telescope.EQ.'ROSAT') THEN
          IF (instrume(1:4).EQ.'PSPC') THEN
            IF (bkgd_rad.LE.2) THEN
              errmess='The background estimate could be'
     >                //' contaminated by the PSF'
              call wtwarm(subname,version,chatter,5,errmess)
              errmess=' using a radius < 2 arcmins for the calc'
              call wtwarm(subname,version,chatter,5,errmess)
            ENDIF
          ELSEIF (instrume(1:3).EQ.'HRI') THEN
            IF (bkgd_rad.LE.(0.5)) THEN
              errmess='The background estimate could be'
     >               //' contaminated by the PSF' 
              call wtwarm(subname,version,chatter,5,errmess)
              errmess=' using a radius < 1/2 arcmin for the calc'
              call wtwarm(subname,version,chatter,5,errmess)
            ENDIF
          ENDIF
         ENDIF
       ELSE
         read(strbkgd,*,IOSTAT=err) bkgd
         IF (err.NE.0) THEN
           errmess='parsing bkgd parameter'
           call wterrm(subname,version,errmess)
           cont = .false.
         ENDIF
       ENDIF

c ---------------- READING OUTPUT FILE PARAMETER ------------------------

       call uclgst('outfil',outfil,err)
       IF (err.NE.0) THEN
          errmess = ' getting outfile parameter'
           call wterrm(subname,version,errmess)
       ENDIF
       call crmvlbk(outfil)
       IF (outfil.EQ.'   ') THEN
           errmess = ' must enter outfile name ! '
           call wterrm(subname,version,errmess)
         cont = .false.
         return
       ENDIF

c GET CLOBBER

      call uclgsb('clobber',killit,err)
      IF (err.NE.0) THEN
          killit = .false.
          errmess = 'getting clobber value'
          call wterrm(subname,version,errmess)
      ENDIF

c OUTFILE VALIDATION
c ----------------------------------------------------------------------+
      n_ill = 0
      call ck_file(outfil,ill_files,n_ill,valfil, killit,chatter)
      IF (.NOT.valfil) THEN
        cont = .false.
        return
      ENDIF

c -------------------- GETTING CHATTER FLAG ----------------------------

       call uclgsi('chatter',chatter,err)
       IF (err.NE.0) THEN
          errmess = ' getting chatter parameter'
           call wterrm(subname,version,errmess)
       ENDIF  
       return
       end
c ----------------------------------------------------------------------
c                END OF SUBROUTINE RPSF_GP 
c ----------------------------------------------------------------------

*+RPSF_RSTW
c ---------------------------------------------------------------------+
       subroutine rpsf_rstw(infil, extno, n_pix, net, neterr, rad1, 
     >                      rad2, source, npts, maxrad, telstw, inststw,
     >                      cont, chatter)
c
c --- DESCRIPTION ----------------------------------------------------
c
c This routine reads STW FITS file. It uses FITSIO to read columns ...
c
c PIXELS     : Column of No. of pixels in each bin
c NET        : Column of observed radial profile in counts
c NETERR     : Column of errors on net
c RAD1       : Column of lower edge of radial bins, in pixels
c RAD2       : Column of upper edge of radial bins, in pixels
c
c In addition to the standard keywords, it also reads ...
c
c TELESCOPE  : Mission/Telescope name
c INSTRUME   : Instrument/Detector name
c SOU_A      : Which defines Source information in string format.
c              The inner and outer radi , and No. of steps is extracted
c              from this string, in order to later calculate pixelsize.
c
c ----------------------------- VARIABLES -----------------------------------
c
      IMPLICIT NONE
      character*(*) infil
      character(180) filename
      character*(*) source
      character*(*) telstw,inststw
      integer maxrad, extno, npts, chatter
      real net(*), neterr(*), rad2(*), rad1(*)
      integer n_pix(*)
      logical cont
c     real net(maxrad),neterr(maxrad)
c     real rad2(maxrad),rad1(maxrad)
c     integer*4 n_pix(maxrad)
C
c -------------------------- INTERNAL VARIABLE ------------------------------
c
      character(70) sou1, subinfo
      character(40) comm,extinfo,errstr
      character(30) sou_comm
      integer maxcol,clenact
      parameter (maxcol=14)
      character(8) columns(maxcol)
      real enull
      integer status,block,htype,ncols,colnum
      integer felem,frow,iunit,find_nstep, extnum
      logical anyflg

c --------------------------- VARIABLE DIRECTORY -----------------------
c
c maxrad     int    : Array dimensions
c region     int    : Array of region Nos
c net        real   : Array of observed radial profile in counts
c neterr     real   : Array of errors on net
c rad2       real   : Array of upper edge of radial bins (in pixels)
c n_pix      int    : Array of No of pixels in bins
c npts       int    : Counter for observed psf data
c telstw     char   : Used to read Telescope/Mission name
c inststw    char   : Used to read Instrument/Detector name
c chatter    int    : Chattiness flag ( >20 noisy)
c extname    char   : Name of Extname keyword in STW file
c cont              : False if column is not found, program returns to main
c
c Internals ...
c
c errmess    char   : Error message text
c errstr     char   : Error text for this routine
c extinfo    char   : More error information
c version    char   : Subroutine version
c enull      real   : Used to represent undefined values in FITS file
c status     int    : Error flag for FITSIO call
c iunit      int    : Fortran unit number for file
c block      int    : FITSIO record blocking factor
c htype      int    : Type of header unit in FITS file
c felem      int    : First pixel of element array in FITS file
c frow       int    : Beginning row No. in FITS file
c inull      int    : Used to represent undefined values in FITS file
c colnum     int    : No. of column
c i,j        int    : Counters for loops
c anyflg     logical: True if any data undefined in FITS file
c upstw      char   : Extension name from file in upper case (for comparison)
c upext      char   : Extname in upper case
c extfind   logical : True if required extension is found
c     
c --- CALLED ROUTINES ---
c
c subroutine FTOPEN      : FITSIO routine to open FITS file
c subroutine FTCLOS      : FITSIO routine to close FITS file
c subroutine FTMAHD      : FITSIO routine to move to extension header
c subroutine FTGKYs      : FITSIO routine to read extension header keyword,
c                          where the s is for string
c subroutine FTGKYj      : FITSIO routine to read extension header keyword,
c                          where the j, is for an integer
c subroutine FTGKNS      : FITSIO routine to read extension header keyword,
c                          where a rootstring is given, thus an array of
c                          keywords can be read
c subroutine FTGCVe      : FITSIO routine to read columns of data in an
c                          extension. The e indicates that it is real data.
c
c subroutine FTGERR      : FITSIO routine, which gives error text for a
c                          given error flag.
c
c subroutine FCECHO      : FTOOLS library routine to write to screen
c subroutine WT_FERRMSG  : Writes Error text if required
c
c --- COMPILATION AND LINKING ---
c
c Link with FITSIO and FTOOLS
c     
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (Feb 1993)
c Rehana Yusaf (March 1993) : Add telescope and instrument read
c Rehana Yusaf (22 April 1993) : NET column read from STW file instead
c                                of cnts/pix, as conversion to
c                                cnts/arcmin^2 is more accurate using NET.
c                                This is due to rounding errors in cnts/pix.
c Rehana Yusaf (6 May)         : NETERR column read instead of err/pix for
c                                same reason as above.
c Rehana Yusaf (14 jan 1994) 1.0.2; move to extno as specified by user, extname
c                                search no longer needed
c Banashree Mitra Seifert (1996, Jan) 1.1.0:
c             . variable dimension carried from calling routine
c             . Introduced screen display routine
c               wtinfo,wterrm,wtferr 
c Peter D Wilson (1998 Jul 01) 1.1.1:
c             . Drop bogus call to fcpars
c --------------------------------------------------------------------
        character(5) version
        parameter (version = '1.1.1')
        character(10) subname
*-
c ------------------------ USER INFO ----------------------------------

        subname = 'rpsf_rstw'
        subinfo = 'using '//subname//' '//version
        call wtinfo(chatter,10,1,subinfo)

c ------------------------ OPENING STW FILE ------------------------------

C PDW 7/1/98: No need to call fcpars... done in rpsf_gp
C        status=0
C        call fcpars(infil,filename,extnum,status)
        status=0
        call cgetlun(iunit)
        call ftopen(iunit,infil,0,block,status)
        extinfo = 'opening file : '//infil
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,extinfo)
          cont = .false.
          go to 150
        ENDIF

c  <<<--- MOVING TO DATA EXTENSION --->>> 
   
        status = 0
        call ftmahd(iunit,extno+1,htype,status)
        extinfo = ' problem moving to RPSF data extension'
        call wtferr(subname,version,status,extinfo)
        IF (status.NE.0) THEN
          cont = .false.
          go to 150
        ENDIF

c ------------ READING INFORMATION FROM HEADER ---------------

        call ftgkyj(iunit,'NAXIS2',npts,comm,status)
        errstr='reading NAXIS2 parameter'
        call wtferr(subname,version,status,errstr)
        IF (status.NE.0) THEN
          cont = .false.
          go to 150
        ENDIF

        status = 0
        call ftgkns(iunit,'TTYPE',1,maxcol,columns,ncols,status)
        errstr='reading TTYPE parameter'
        call wtferr(subname,version,status,errstr)

        status = 0
        call ftgkys(iunit,'TELESCOP',telstw,comm,status)
        errstr='reading TELESCOP parameter'
        call wtferr(subname,version,status,errstr)
        IF (status.EQ.202) THEN
          telstw = 'UNKNOWN'
        ENDIF

        status = 0
        call ftgkys(iunit,'INSTRUME',inststw,comm,status)
        errstr='reading INSTRUME parameter'
        call wtferr(subname,version,status,errstr)
        IF (status.EQ.202) THEN
          inststw = 'UNKNOWN'
        ENDIF

        status = 0
        call ftgkys(iunit,'SOU_A',sou1,sou_comm,status)
        find_nstep = 0
        find_nstep = index(sou1(1:),'n=')
        IF (find_nstep.EQ.0) THEN
          source=sou1(:clenact(sou1))//''' '//sou_comm
        ELSE 
          source = sou1//sou_comm
        ENDIF
          extinfo = 'SOURCE string just read'
          call wtinfo(chatter,20,2,extinfo)
          call wtinfo(chatter,20,2,source)

c ------------------ CHECK TO FIND PIXELS COLUMN --------------------

        status = 0
        call ftgcno(iunit,.false.,'PIXELS',colnum,status)
        IF (status.NE.0) THEN
          extinfo='finding PIXELS column '
          call wtferr(subname,version,status,extinfo)
          cont = .false.
          goto 150
        ENDIF

c ---------------------- READING PIXELS COLUMN ------------------------
        frow = 1
        felem = 1
        enull = 0
        status = 0
        call ftgcvj(iunit,colnum,frow,felem,npts,enull,n_pix,anyflg,
     >              status)
        extinfo='reading pixels column'
        call wtferr(subname,version,status,extinfo)
        IF (status.NE.0) THEN
          cont = .false.
          go to 150
        ENDIF

c ------------------ CHECK TO FIND NET COLUMN ------------------------

        status = 0
        call ftgcno(iunit,.false.,'NET     ',colnum,status)
        IF (status.NE.0) THEN
          extinfo = 'finding NET Column '
          call wtferr(subname,version,status,extinfo)
          cont = .false.
          go to 150
        ENDIF

c ----------------- READING NET COLUMN ---------------------------------
        frow = 1
        felem = 1
        enull = 0
        status = 0
        call ftgcve(iunit,colnum,frow,felem,npts,enull,net,anyflg,
     >              status)
        extinfo='reading net column'
        call wtferr(subname,version,status,extinfo)
        IF (status.NE.0) THEN
          cont = .false.
          go to 150
        ENDIF

c --------------- CHECK TO FIND NETERR COLUMN ---------------------------

        status = 0
        call ftgcno(iunit,.false.,'neterr',colnum,status)
        IF (status.NE.0) THEN
          extinfo = 'finding NETERR column'
          call wtferr(subname,version,status,extinfo)
          cont = .false.
          goto 150
        ENDIF

c ---------------------- READING NETERR COLUMN----------------------------

        frow = 1
        felem = 1
        enull = 0
        status = 0
        call ftgcve(iunit,colnum,frow,felem,npts,enull,neterr,anyflg,
     >              status)
        call wtferr(subname,version,status,errstr)
        IF (status.NE.0) THEN
          cont = .false.
          go to 150
        ENDIF

c -------------------------- CHECK TO FIND RAD1 COLUMN -------------------

        status = 0
        call ftgcno(iunit,.false.,'RAD1',colnum,status)
        IF (status.NE.0) THEN
          extinfo = 'finding RAD1 column'
          call wtferr(subname,version,status,extinfo)
          cont = .false.
          goto 150
        ENDIF

c ---------------- READING RAD1 COLUMN ------------------------------------

        frow = 1
        felem = 1
        enull = 0
        status = 0
        call ftgcve(iunit,colnum,frow,felem,npts,enull,rad1,anyflg,
     >              status)
        extinfo = ' reading rad1 column'
        IF (status.NE.0) THEN
         extinfo = 'finding RAD1 column'
         call wtferr(subname,version,status,extinfo)
          cont = .false.
          go to 150
        ENDIF

c ------------------- CHECK TO FIND RAD2 COLUMN ------------------------

        status = 0
        call ftgcno(iunit,.false.,'RAD2',colnum,status)
        IF (status.NE.0) THEN
          extinfo = ' Error finding RAD2 column'
          call wtferr(subname,version,status,extinfo)
          cont = .false.
          goto 150
        ENDIF

c ------------ READING RAD2 COLUMN -------------------------------------

        frow = 1
        felem = 1
        enull = 0
        status = 0
        call ftgcve(iunit,colnum,frow,felem,npts,enull,rad2,anyflg,
     >              status)
        extinfo = ' reading rad2 column'
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,extinfo)
          cont = .false.
          goto 150
        ENDIF

c ----------------- CLOSE STW FITS FILE ----------------------------------

  150   status = 0
        call ftclos(iunit,status)
        extinfo = ' closing stw file'
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,extinfo)
          cont = .false.
        ENDIF
        call cfrelun(iunit)

        return  
        end

c ------------------ END OF SUBROUTINE RPSF_RSTW ------------------------ 

*+WT_COLS
c ---------------------------------------------------------------------+
      subroutine wt_cols(ncols, columns, maxcol, chatter)

c --- DESCRIPTION -------------------------------------------------
c
c This routine prints out a character array of column header names,
c using fcecho.
c
c --- VARIABLES ---------------------------------------------------

      IMPLICIT NONE
      character(80) info
      integer ncols,maxcol,i, chatter
      character(8) curcol,columns(maxcol)

c ------------------------ VARIABLE DIRECTORY ---------------------------
c
c info       char   : Comment string
c ncols      int    : No. of Columns
c maxcol     int    : Array dimension
c columns    char   : Array containing column names
c curcol     char   : current column name
c
c -------------------- CALLED ROUTINES ----------------------------------
c
c subroutine FCECHO : FTOOLS library routine to write to screen
c
c --------------------- LINKING AND COMPILATION -------------------------
c
c Link with FTOOLS 
c
c --------------------- AUTHORS -------------------------------------
c
c Rehana Yusaf ( Feb 1993)
c
c Banashree Mitra Seifert (1995 Dec) 1.1.0:
c                . made version no. character(5) instead of *4 (1.0)
c                . passed chatter
c                . screen display used
c                  wtinfo
c ------------------------------------------------------------------
      character(5) version
      parameter ( version = '1.1.0' )
*-

      info = 'The following columns are present in the input file :'
      call wtinfo(chatter,20,1,info)
      do i=1,ncols
         curcol = columns (i)
         call wtinfo(chatter,20,21,curcol)
      enddo
      return
      end
c ----------------------------------------------------------------------
c            END OF SUBROUTINE WT_COLS 
c ----------------------------------------------------------------------

*+RPSF_CONV
c ---------------------------------------------------------------------+
      subroutine rpsf_conv(rad1,rad2,rad_lo,rad_hi,net,neterr,
     >                     cts_arc2,err_arc2,source,arc_pix,pixsize,
     >                     sumrcts,sumtcts,npts,maxrad,maxtheta,n_pix,
     >                     area_wgt,bkgd,calc,bkgd_rad,cont,chatter)
c ---------------- DESCRIPTION ----------------------------------------+
c
c This routine converts data into OGIP CALRPSF format
c
c ----------------- VARIABLES -----------------------------------------

      IMPLICIT NONE
      character*(*) source
      character(80) errinfo, errmess
      character(200) subinfo
      integer maxrad, maxtheta
      real arc_pix, del_rad, pixsize, sumrcts, sumrcts_bkgd, area, pi
      real bkgd_rad, bkgd, sumpix, sumpix_bkgd, per_bkgd
      real sumpix2, sumcts, sumtcts
      real rad1(*), rad2(*), rad_lo(*), rad_hi(*)
      real net(*), neterr(*), area_wgt(maxrad,maxtheta,1),theocts
      real cts_arc2(maxrad,maxtheta,1), err_arc2(maxrad,maxtheta,1)
      integer npts, i, chatter
      integer n_pix(*)
      logical findpix, cont, calc
      
c     real rad1(maxrad),rad2(maxrad),arc_pix,del_rad,pixsize
c     real rad_lo(maxrad),rad_hi(maxrad),sumrcts,sumrcts_bkgd
c     real net(maxrad),area_wgt(maxrad,maxtheta,1),area,pi,bkgd_rad
c     real neterr(maxrad),bkgd,sumpix,sumpix_bkgd
c     real per_bkgd
c     real cts_arc2(maxrad,maxtheta,1),err_arc2(maxrad,maxtheta,1)
c     integer*4 n_pix(maxrad)
c
c ----------------------- VARIABLE DIRECTORY ----------------------------
c
c maxrad     int    : Array dimensions
c maxtheta   int    : Array of dimunsions
c rad1       real   : Array of lower edge of radial bins (in pixels)
c rad2       real   : Array of upper edge of radial bins (in pixels)     
c rad_lo     real   : Array of lower edge of radial bins (in arcmin)
c rad_hi     real   : Array of upper edge of radial bins (in arcmin)
c net        real   : Array of counts 
c neterr     real   : Array of errors on net
c cts_arc2   real   : Array of counts per arcmin^2
c err_arc2   real   : Array of errors on cts_arc2
c area_wgt   real   : Area weighting factor for each bin
c arc_pix    real   : arcmins per pixel
c del_rad    real   : del_rad = (outer radius - inner radius)/nbins
c cont      logical : False if error occurs
c                     (in arcmin)
c pixsize    real   : Pixsize in degrees
c npts       int    : Counter for No of radial values
c findpix    logical: True if still finding arcmins per pixel value
c chatter    int    : Chattiness flag (>20 noisy)
c subinfo    char   : Subroutine information
c
c --- CALLED ROUTINES ---
c
c subroutine RPSF_SRC      : String manipulation routine, which extracts
c                            values from source string
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (Feb 1993)
c Rehana Yusaf (23 April 1993) : Conversion to counts per arcmin^2 is now
c                                made using NET and N_PIX arrays. The
c                                CNT/PIX array was used before (no longer
c                                read in due to trucation -loss in precision)
c Rehana Yusaf (3 Sept)1.0.3; Area_wgt array added - new rpsf format
c Rehana Yusaf (1994 Feb 3) 1.0.4; Calculate sumrcts
c Rehana Yusaf (1995 Feb 21) 1.0.5; Calculate bkgd if user requests it
c                                   Also add more diagonostics at high chatter
c Banashree Mitra Seifert (1995 Dec)1.1.0:
c                 . Dimension carried from calling routine
c                 . filename *180 character
c                 . Replaced pi=3.141592654 by pi=4*atan(1)
c                 . screen display used
c                   wtinfo,wterrm,wtwarm  
c Rehana Yusaf (1996 Jan 23) 1.1.1; add theocts, previously
c                 => sumtcts = sumcts - sumpix * bkgd
c                 => now sumtcts = theocts - sumpix * bkgd
c ----------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.1')
      character(10) subname
*-
c ------------------------- USER INFO ----------------------------------

      pi = 4.*atan(1.) 
      subname = 'rpsf_conv'
      subinfo = 'using '//subname//' '//version
      call wtinfo(chatter,10,1,subinfo)

c ----------------------- OBTAIN DEL_RAD -----------------------------

      call rpsf_src(source,del_rad,cont,chatter)
      IF (.NOT.cont) THEN
          errinfo = ' SOU_A string unsuccesfully parsed'
          call wterrm(subname,version,errinfo)
          return
      ENDIF

c --------------- CALCULATE ARCMINS PER PIXEL --------------------------

      findpix = .false.
      i = 1
      do WHILE(.NOT.findpix)
        IF ((rad2(i)-rad1(i)).NE.0) THEN
           arc_pix = del_rad/(rad2(i)-rad1(i))
           findpix = .true.
        ELSE
           i = i + 1
        ENDIF
      enddo

c ---------- CALCULATE RAD_LO AND RAD_HI FROM ARC_PIX ------------------

      do i=1,npts
        rad_lo(i) = rad1(i) * arc_pix
        rad_hi(i) = rad2(i) * arc_pix
      enddo
      IF (bkgd_rad.GE.rad_hi(npts)) THEN
       errmess = ' inner radius for bkd calculation is > Outer radius' 
       call wterrm(subname,version,errmess)
       cont = .false.
       return
      ENDIF
      per_bkgd = bkgd_rad/real(rad_hi(npts))
      IF (per_bkgd.GE.(0.75)) THEN
        errmess = 'The inner radius for bkgd calculation is'
     >              //' > 75% of the total radius'
        call wtwarm(subname,version,chatter,9,errmess) 
      ENDIF

c ---------- CONVERT COUNTS AND ERROR INTO ARCMINS**2 UNITS ------------
      sumrcts = 0
      sumrcts_bkgd = 0
      sumpix_bkgd = 0
      do i=1,npts
       sumcts = sumcts + net(i)
       IF (calc) THEN
         IF (rad_lo(i).GE.bkgd_rad) THEN
           sumpix_bkgd = sumpix_bkgd + n_pix(i)
           sumrcts_bkgd = sumrcts_bkgd + net(i)
         ENDIF
       ENDIF
       cts_arc2(i,1,1) = (net(i)/real(n_pix(i))) * (1/(arc_pix**2))
       err_arc2(i,1,1) = (neterr(i)/real(n_pix(i))) * (1/(arc_pix**2))
      enddo
      sumpix = pi * (rad2(npts)**2 - rad1(1)**2)
c -------------------------------------------------------------------+
c ------------------- CALCULATE AREA_WGT RATIOS ---------------------

      sumpix2 = 0
      theocts = 0
      do i=1,npts
        area = pi * ((rad2(i)**2) - (rad1(i)**2))
        IF ((area.NE.0).AND.(n_pix(i).NE.0)) THEN
          area_wgt(i,1,1) = real(n_pix(i))/area
        ELSE
          errinfo = ' calculating area_wgt factor !'
          call wterrm(subname,version,errinfo)
        ENDIF
        sumpix2 = sumpix2 + n_pix(i)
        theocts = theocts + net(i)/area_wgt(i,1,1)
      enddo

c ---------------- CALCULATE PIXSIZE IN DEGREES ------------------------

      pixsize = arc_pix/real(60)
      IF (calc) THEN
        IF (sumrcts_bkgd.GE.0.0) THEN
          bkgd = sumrcts_bkgd/sumpix_bkgd
        ELSE
          errmess='The sum of the counts in the bkgd region is < 0'
          call wtwarm(subname,version,chatter,5,errmess)
          errmess=' The bkgd has been set to 0'
          call wtinfo(chatter,5,1,errmess)
          bkgd = 0
        ENDIF 
      ENDIF
      sumtcts = theocts - sumpix*bkgd
      sumrcts = sumcts - sumpix2*bkgd
      IF (calc) THEN
        IF (sumtcts.LE.0.0) THEN
          bkgd = (sumcts - 1)/sumpix
          sumtcts = 1
          sumrcts = sumcts - sumpix2*bkgd
          subinfo =' the background value has been calculated such'
     >               //' that the sum of the'
          call wtinfo(chatter,9,1,subinfo)
          subinfo=' theoretical counts is set to 1. If the background'
     >             //' is calculated in the usual way then sumtcts '
     >             // ' is -ve, this may be due to excluded regions.'
          call wtinfo(chatter,9,1,subinfo)
        ENDIF
      ENDIF

      write(subinfo,'(a,f20.9)') ' calculated bkgd value :',bkgd
      IF (calc) THEN
          call wtinfo(chatter,20,1,subinfo)
      ENDIF

       write(subinfo,'(a,F20.9)')' sum of pixels calculated using'
     >               //' area of circle:',sumpix
       call wtinfo(chatter,20,2,subinfo)
       write(subinfo,'(a,F20.9)')' actual sum of pixels :',sumpix2
       call wtinfo(chatter,20,2,subinfo)

      IF ((sumpix2/sumpix).LE.(0.9)) THEN
        subinfo='actual sum of pixels is < 90% of sum' 
     >         //' of pixels calculated using area of circle, '
     >         // 'where the outer radius is used' 
        call wtinfo(chatter,5,2,subinfo)
        subinfo ='note: this may be due to regions being excluded'
        call wtinfo(chatter,5,2,subinfo)
      ENDIF

       write(subinfo,'(a,F20.9)')'total sum of counts :',sumcts
       call wtinfo(chatter,20,2,subinfo)
       write(subinfo,'(a,F20.9)')'counts in source :',sumrcts
       call wtinfo(chatter,20,2,subinfo)
       write(subinfo,'(a,a,F20.9)')'theoretical sum of counts',
     >             '(corrected for any excl regions):',sumtcts
       call wtinfo(chatter,20,2,subinfo)
      
      IF (sumrcts.LT.0) THEN
        subinfo = 'sum of counts is negative'
        call wterrm(subname,version,subinfo)
        subinfo=' this may be due to background dominating'
     >            //' the source'
        call wtinfo(chatter,0,1,subinfo)
        cont = .false.
        return
      ENDIF

      IF (sumtcts.LT.0) THEN
        subinfo = 'theoretical Sum of counts is negative'
        call wterrm(subname,version,subinfo)
        subinfo=' this may be due to background dominating'
     >            //' the source'
        call wtinfo(chatter,0,1,subinfo)
        cont = .false.
        return
      ENDIF

      IF ((sumrcts/sumcts).LE.(0.1)) THEN
        subinfo ='source is less than 10% of total counts !'
        call wtwarm(subname,version,chatter,5,subinfo)
      ENDIF
      
      return
      end
c ---------------------------------------------------------------------
c               END OF SUBROUTINE RPSF_CONV           
c ---------------------------------------------------------------------

*+RPSF_WT
c ---------------------------------------------------------------------+
      subroutine rpsf_wt(outfil,infil,stver,rad_lo,
     >                   rad_hi,cts_arc2,err_arc2,area_wgt,pixsize,
     >                   npts,maxrad,maxtheta,telstw,inststw,
     >                   bkgd,chanmin,chanmax,
     >                   sumrcts,sumtcts,cont,chatter,killit)

c ----------------- DESCRIPTION -------------------------------------
c
c This routine writes to FITS output file in CALRPSF format. 
c It opens the FITS file and passes the data to WTRPF1
c which writes the data in the form of a BINARY TABLE extension. 
c Finally it closes the FITS file.
c
c ---------------------- VARIABLES --------------------------------

      IMPLICIT NONE
      character*(*) infil,outfil
      character*(*) telstw,inststw
      character*(*) stver
      integer maxrad,npts,chatter,maxtheta,chanmin,chanmax
      real cts_arc2(maxrad,maxtheta,*), err_arc2(maxrad,maxtheta,*)
      real bkgd,sumrcts,pixsize,sumtcts
      real rad_lo(*),rad_hi(*),area_wgt(maxrad,maxtheta,*)
      logical cont,killit

c -------------------------- INTERNAL VARIABLES ----------------------

      character(80) subinfo
      integer maxhist,maxcomm
      parameter (maxhist = 10)
      parameter (maxcomm = 10)
      character(80) hist(maxhist),comms(maxcomm),extinfo
      integer ounit,nk_hist,status,nk_comm,ierr
      real theta_lo(1), theta_hi(1),energ_lo(1),energ_hi(1)
      integer ntheta,nenerg
      character(8) extname
      character(16) hduclas3,taskname
      character(16) radunit,thetaunit,energunit,rpsfunit
      logical qarea,qerror

c ------------------------- VARIABLE DIRECTORY ------------------------
c
c Arguments ...
c
c outfil     char   : Output filename
c infil      char   : Input filename
c stver      char   : Main program version
c maxrad     int    : Array dimensions for radial profile data
c cts_arc2   real   : Array of radial profile in counts per arcmin^2
c err_arc2   real   : Array of errors on cts_arc2 (in arcmin^2)
c rad_lo     real   : Array of lower edge of radial bins (in arcmin)
c rad_hi     real   : Array of upper edge of radial bins (in arcmin)
c n_pix      int    : Array of No of pixels in bin
c Pixsize    real   : Pixsize in degrees
c npts       int    : Counter for No. radial profile values
c telstw     char   : Mission/Telescope name
c inststw    char   : Instrument/Detector name
c chatter    int    : Chattines flag (>20 noisy)
c
c Internals ...
c
c ounit      int    : Fortran file unit number
c maxhist    int    : Dimension for history comment array
c maxcomm    int    : Dimension for comments array
c nk_comm    int    : No. of lines of comments
c nk_hist    int    : No of lines of history comments
c status     int    : Error flag
c hist       char   : History comment array
c comms      char   : Comment array
c version    char   : Subroutine version
c errstr     char   : Error string for this routine
c errmess    char   : Error message obtained from FITSIO call
c
c --------------------- CALLED ROUTINES ---------------------------------
c
c subroutine OP_NA        : (FITSIO) Opens FITS file,writes header and null
c                            primary array
c subroutine FTGERR       : (FITSIO) Obtains appropriate error text
c subroutine WTRPF1       :  Writes data extension
c subroutine FTCLOS       : (FITSIO) Closes FITS file
c subroutine WT_FERRMSG   :  Writes Error text if required
c
c ---------------- COMPILATION AND LINKING ----------------
c
c Link with FITSIO and FTOOLS
c
c ---------------- AUTHORS/MODIFICATION HISTORY ----------------
c
c Rehana Yusaf (Feb 1993)
c Rehana Yusaf (2 Sept 1993) 1.0.1; Change call to WT_RPSF1993a - new
c                                   more general routine
c Rehana Yusaf (14 Jan 1994) 1.0.2; Renamed WT_RPSF1993a to WTRPF1, for
c                                   CALLIB naming convention
c				    Also additional keywords are written
c                                   BACKGRND and if ROSAT PSPC then
c				    CHANMIN and CHANMAX
c Rehana Yusaf (Feb 3 1994) 1.0.3; Write SUMRCTS   
c Rehana Yusaf (Mar 7 1995) 1.0.4; Write SUMTCTS, theoretical sum of
c                                  counts, should be same as SUMRCTS
c                                  when no regions are excluded 
c Rehana Yusaf (1995 April 25) 1.0.5; add killit (clobber) parameter
c
c Banashree Mitra Seifert (1995 Dec) 1.1.0:
c             . Filename character *180
c             . Dimensions to be carried over by calling routine
c             . Screen display is used
c               wtinfo,wtferr,wtfwrn
c -----------------------------------------------------------------
        character(5) version
        parameter ( version = '1.1.0' )
        character(8) subname
*-
c --------------------------------------------------------------
        subname = 'rpsf_wt'
        subinfo = 'using '//subname//' '//version
        call wtinfo(chatter,10,1,subinfo)

c -------------------- OPENING FITS FILE ------------------------
        call cgetlun(ounit)
        status = 0
        call opnpa(outfil,chatter,ounit,killit,status)
        extinfo = ' setting up primary header'
        call wtfwrn(subname,version,chatter,15,status,extinfo)
        IF (status.NE.0) THEN
          cont = .false.
          goto 200 
        ENDIF

c ----------------- SETTING UP HISTORY COMMENT FIELDS ----------------

        nk_hist = 2
        hist(1) = 'st2rpsf converts from STW -> RPSF format'
        hist(2) = 'STW FILE : '//infil
        nk_comm = 0

c ------- SETTING UP UNDEFINED ARRAYS ! THETA AND ENERG VALUES -------

        theta_lo(1) = -99
        theta_hi(1) = -99
        energ_lo(1) = -99
        energ_hi(1) = -99
        ntheta = 1
        nenerg = 1

c ---------------------- WRITING DATA --------------------------------

        radunit = 'arcmin'
        thetaunit = 'arcmin'
        energunit = 'KeV'
        rpsfunit = 'count/arcmin**2'
        qarea = .true.
        qerror = .true.
        ierr = 0
        extname = 'OBS RPSF'
        IF (bkgd.EQ.0.0) THEN 
          hduclas3 = 'NET'
        ELSE
          hduclas3 = 'TOTAL'
        ENDIF
        call wtrpf1(ounit,extname,hduclas3,npts,rad_lo,rad_hi,
     >              radunit,ntheta,theta_lo,theta_hi,thetaunit,
     >              nenerg,energ_lo,energ_hi,energunit,cts_arc2,
     >              qerror,err_arc2,rpsfunit,qarea,area_wgt,hist,
     >              nk_hist,comms,nk_comm,telstw,inststw,maxrad,
     >              maxtheta,ierr,chatter)
        IF (ierr.NE.0) THEN
           cont = .false.
           goto 200
        ENDIF

c ----------------------- CREATOR KEYWORD ----------------------------

        taskname = 'st2rpsf'//' '//stver
        status = 0
        call ftpkys(ounit,'CREATOR',taskname,
     >              's/w task which wrote this dataset',
     >               status)
        extinfo = ' writing CREATOR keyword'
        call wtfwrn(subname,version,chatter,15,status,extinfo)

c --------------- WRITE PIXELSIZE ----------------------------------

200     status = 0
        call ftpkye(ounit,'PIXSIZE',pixsize,8,'In decimal degrees',
     >              status)
        extinfo = ' writing PIXSIZE keyword'
        call wtferr(subname,version,status,extinfo)

c ------------------ WRITE BACKGROUND --------------------

        status = 0
        call ftpkye(ounit,'BACKGRND',bkgd,8,'In counts per pixel',
     >              status)      
        extinfo = ' writing BACKGRND keyword'
        call wtferr(subname,version,status,extinfo)

c ------------ WRITE CHANMIN and CHANMAX for ROSAT PSPC --------------

        status = 0
        call ftpkyj(ounit,'CHANMIN',chanmin,
     >            'Minimum PI channel for image',status)
        extinfo = ' writing CHANMIN keyword'
        call wtferr(subname,version,status,extinfo)
        status = 0
        call ftpkyj(ounit,'CHANMAX',chanmax,
     >            'Maximum PI channel for image',status)
        extinfo = ' writing CHANMAX keyword'
        call wtferr(subname,version,status,extinfo)

c --------------------- WRITE SUMRCTS -------------------------------

        status = 0
        call ftpkye(ounit,'SUMRCTS',sumrcts,8,
     >             'Sum of source counts under profile',status)
        extinfo = ' writing SUMRCTS'
        call wtferr(subname,version,status,extinfo)

c ------------------- WRITE SUMTCTS -----------------------------------+

        status = 0
        call ftpkye(ounit,'SUMTCTS',sumtcts,8,
     >              'Theoretical Sum source counts(corr for '
     >              //'excl regions)',status)
        extinfo = ' writing SUMTCTS'
        call wtferr(subname,version,status,extinfo)

c ----------------- CLOSE FITS FILE ---------------------------------

        status = 0
        call ftclos(ounit,status)
        extinfo='closing output file'
        call wtferr(subname,version,status,extinfo)
        IF (status.NE.0) THEN
           cont = .false.
        ENDIF
         call cfrelun(ounit)
        return
        end
c ---------------------------------------------------------------------+
c              END OF SUBROUTINE RPSF_WT                             
c ---------------------------------------------------------------------+

*+RPSF_SRC
c ---------------------------------------------------------------------+
      subroutine rpsf_src(sou_a,del_rad,cont,chatter) 

c ----------------------- DESCRIPTION ----------------------------------
c This subroutine is a parser for a particular character string. It
c extracts inner and outer radius values, as well as nsteps from the
c string. The units of inner and outer have to be arcsecs or arcmins
c otherwise this routine returns to main and stops.
c
c ------------------------- VARIABLES ----------------------------------

      character*(*) sou_a
      character(100) source
      character(70) subinfo,extinfo
      character(12) charnum,cnum
      character(1) dpost,spost
      real inner,outer,nbins,del_rad,cnvfact
      integer beg,end,pos,skipstr,i
      integer curpos,chatter,eflg,ck_char
      logical dunit,cont

c ------------------------ VARIABLE DIRECTORY ------------------------
c
c Passed Parameters ...
c
c source     char   : String containing source information
c del_rad    real   : Del_Rad = (outer rad - inner rad)/Nsteps (in arcmin)
c chatter    int    : Flag for writing to screen (>20 write)
c cont      logical : False if program is to be terminated
c
c Internals ...
c
c charnum    char   : String used for extracting numbers from source
c dpost      char   : Used for conversion factor ' or "
c inner      real   : Inner radial value in arcmins or arcsecs
c outer      real   : Outer radial value in arcmins or arcsecs
c nbins      real   : No of divisions of outer - inner
c cnvfact    real   : Conversion facter, to convert to arcmins
c beg        int    : Used as position marker when finding beginning of
c                     number in the source string
c end        int    : Used as position marker when finding end of
c                     number in the source string
c pos        int    : Used as position marker
c skipstr    int    : Counter for skipped expressions in source
c curpos     int    : Another position marker, used with function index
c subinfo    char   : User information about subroutine
c extinfo    char   : Additional comments
c
c
c --------------------------- CALLED ROUTINES -----------------------
c
c subroutine FCECHO    : (FTOOLS) Library routine to write to screen
c subroutine CRMVBXK   : (CALLIB) Compresses two or more blanks to one
c function INDEX       : (FORTRAN) Library funnction which searches
c                        for a substring in specified string
c
c ------------------ COMPILATION AND LINKING --------------------------
c
c Link with FTOOLS and XANLIB
c
c ---------------- AUTHORS/MODIFICATION HISTORY ------------------------
c
c Rehana Yusaf 
c Rehana Yusaf (18 august 1993) 1.0.1; add more error checking
c
c Banashree Mitra Seifert (1995 Dec) 1.1.0:
c             . Filename character *180
c             . Dimensions to be carried over by calling routine
c             . Screen display is used
c               wtinfo,wterrm
c -----------------------------------------------------------------
             
      character(5) version
      parameter (version = '1.1.0')
      character(9) subname
*-

c ---------- PROVIDING USER INFORMATION IF DESIRED -------------------

      subname = 'rpsf_src'
      subinfo = 'using '//subname//' '//version
      call wtinfo(chatter,10,1,subinfo)

      call crmvlbk(sou_a)
      call rmvexsp(sou_a,source)

      extinfo = ' Source string : '
      call wtinfo(chatter,20,2,extinfo)
      call wtinfo(chatter,20,2,source)

c ---------------- SKIPPING FIRST THREE STRINGS ---------------------

      i = 1
      pos = 0
      skipstr = 0
      do WHILE (skipstr.LT.3)
        curpos = index(source(i:),' ')
        skipstr = skipstr + 1
        i = i + curpos
        pos = pos + curpos
      enddo
      beg = pos + 1

c ---------------- READING  INNER RADIAL VALUE ---------------------

      dunit = .false.
      dpost = '"'
      pos = beg
      curpos = index(source(pos:),' ')
      end = beg + curpos - 2
      IF (source(end:end).EQ.dpost) THEN
         dunit = .true.
         end = end -1
      ENDIF
      eflg = 0
      charnum = source(beg:end)
      read(charnum,*,IOSTAT=eflg) inner
      IF (eflg.NE.0) THEN
        extinfo = 'rpsf_src : reading inner radius !'
        call wterrm(subname,version,extinfo)
        extinfo = ' extracting from :'//charnum
        call wterrm(subname,version,extinfo)
        cont = .false.
        return
      ENDIF

      extinfo = 'extracting inner radius from :'//charnum
      call wtinfo(chatter,30,3,extinfo)

c ---------- READING OUTER RADIAL VALUE -----------

      IF (dunit) THEN
        pos = end + 3
      ELSe
        pos = end + 2
      ENDIF
      beg = pos
      curpos = index(source(pos:),' ')
      end = beg + curpos - 3
      charnum = source(beg:end)
      read(charnum,*,IOSTAT=eflg) outer
      IF (eflg.NE.0) THEN
        extinfo = 'reading outer radius'
        call wterrm(subname,version,extinfo)
        extinfo = ' extracting from :'//charnum
        call wtinfo(chatter,1,1,extinfo)
        cont = .false.
        return
      ENDIF

c ------------------ READING CNVFACT -------------------------------

      i = end + 1
      IF (source(i:i).EQ.dpost) THEN
         cnvfact = 1/real(60)
      ELSE
         cnvfact = 1
      ENDIF              
      
      extinfo = ' extracting outer radius from :'//charnum
      call wtinfo(chatter,30,3,extinfo)

c ------- UNITS CHECKS (arcmins or arcsecs, otherwise invalid) -------

      spost = ''''
      IF ((source(i:i).NE.dpost).and.(source(i:i).NE.spost)) THEN
        extinfo='invalid units for inner and outer radius !'
        call wterrm(subname,version,extinfo)
        extinfo= 'should be arcsecs or arcmins '
        call wtinfo(chatter,0,1,extinfo)
        cont = .false.
        return
      ENDIF

c ----------------------- READING NBINS -----------------------------

      pos = end + 5
      beg = pos
      curpos = 0
      curpos = index(source(pos:),'.')
      IF (curpos.EQ.0) THEN
        curpos = index(source(pos:),' ')
      ENDIF
      end = beg + curpos 
      charnum = source(beg:end)
      ck_char = ichar(source(beg:beg))
      IF ((ck_char.LT.49).OR.(ck_char.GE.58)) THEN
        beg = beg + 1
      ENDIF
      ck_char = ichar(source(end:end))
      IF ((ck_char.LT.49).OR.(ck_char.GE.58)) THEN
        end = end + 1
      ENDIF    
      eflg = 0   
      read(charnum,*,IOSTAT=eflg) nbins
      IF (eflg.NE.0) THEN
        extinfo = 'reading nbins !'
        call wterrm(subname,version,extinfo)
        extinfo = ' extracting nbins from : '//charnum
        call wtinfo(chatter,0,1,extinfo)
        cont = .false.
        return
      ENDIF

      extinfo = ' extracting nbins from :'//charnum
      call wtinfo(chatter,30,3,extinfo)   
      

c -------------------- USER INFO ---------------------------------
c --------------- cnvfact ...
      IF (cnvfact.EQ.1) THEN
         extinfo = 'data in source already in arcmins,cnvfact=1'
      ELSE
         extinfo = 'data in source in arcsecs,cnvfact=1/60'
      ENDIF
      
      call wtinfo(chatter,30,3,extinfo)   
     
c --------------- inner radius ...
      inner = inner * cnvfact
      eflg = 0
      write(cnum,100,IOSTAT=eflg) inner
      extinfo = 'inner radius value (in arcmins) : '//cnum
      call wtinfo(chatter,20,2,extinfo)   
      
c ---------------- outer radius ...
      outer = outer * cnvfact
      eflg = 0
      write(cnum,100,IOSTAT=eflg) outer
      extinfo = ' Outer radius value (in arcmins) : '//cnum
      call wtinfo(chatter,20,2,extinfo)   

c --------------- Nbins ...
      eflg = 0
      write(cnum,100,IOSTAT=eflg)nbins 
      extinfo = ' N steps : '//cnum
      call wtinfo(chatter,20,2,extinfo)   

c ----------------- CALCULATE DEL_RAD ---------------------------

      del_rad = ((outer - inner)/nbins)
  100 FORMAT(F12.8) 
      return
      end
c ---------------------------------------------------------------------+
c                 END OF SUBROUTINE RPSF_SRC 
c ---------------------------------------------------------------------+

