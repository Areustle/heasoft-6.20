c There are two .INC files for XSELECT,
c
c	XSEL.INC contains system-dependent stuff, like
c		the names of directories. It also includes values
c		of parameters that apply throughut XSELECT
c		and do not change, e.g. MAXFIL, MAXLEN etc.
c		It also includes the version number.
c
c	XSELVAR.INC contains system-independent stuff.
c
c	This is XSELVAR.INC
c
c		Alan Smale, 1992 November.


c ---------------------------------------------
c Temporary filenames required by XSELECT
c FITS files --
      character(255) curfil,curfits,unbfil, merfil, mrgtif,mrghkf
      character(255) simfil,imfil,clnim,evnin,evnout,ffflt,ffilin
      character(255) regfil,detfil,sisreg,hktfl,ascflt,gtiflt, spcfil
      character(255) xwnflt,xphflt,xronwn,ascout
      character(255) work1f, work2f, wgti1f, wgti2f,wkhk1f,wkhk2f
      character(255) hkcurf 
      character(255) ffcurf
      character(255) usrdfe,dfefil,chocat
c The hot pixel list:
      character(255) hotpxl 

c The root string for the hkcurve files
c      character(255) hkcurn
c The root string for the ffcurve files
c     character(255) ffcurn
      
c The character string for the prompt
      character(255) prompt

c ASCII files --
      character(255) cmdfil, lstfil, obsfil, errfil, savfil
       
c The Observation catalogue files
      character(255) obscat(MAXOBS)
      
c The columns in the obscat
      character(8) catcol(MXCATC)

c The number of columns in the obscat
      integer ncatcl

c The loaded catalogue name
      character(255) catnam

c The MKF files
      character(255) mkfnam(MXNMKF)
      
c The file for the merged MKF file
      character(255) mermkf                  

c The list of parameters in the MKF file
      character(32) mkfpar(MXMKFP)

c The number of MKF parameters available
      integer nmkfpr

c The data, mkf, obscat, work and HK directory names
      character(255) datdir, mkfdir, catdir, wrkdir, hkdir

c The mkf default name
      character(128) mkfdnm      

c and default relative directory
      character(128) mkreld

c The session prefix
      character(255) prefix

c Filetype of data files and temporary files
      character(255) ftype, xsltyp

c The ref data directory:
      character(255) refdir

c Strings used, reused, reused again as temporary space
c      character(512) str1, str2, str3, str4
c Region and timing and intensity file lists, root names and indices:
      character(255) ctif,hndf,intf
      character(255) regvec(MXNSEL),detvec(MXNSEL),ascvec(MXNSEL)
      character(255) hndvec(MXNSEL),ctivec(MXNSEL),gtivec(MXNSEL)
      character(255) intvec(MXNSEL)
      integer CTINDX(MXNSEL),HNDNDX(MXNSEL),intndx(MXNSEL)

c This stores the expressions for the intensities:
      character(255) intexp(MXNSEL)
      
c The number of regions, ascii, fits and cursor entered gti files
      integer numreg,numdet,numasc,numgti,numcti,numint,numhnd

c The merge of all the FITS GTI files
      character(256) allgti
c Strings containing the keywords for TIME, PHA and units
c of time by this mission (given by keymis). Also X and Y for 
c images, and other stuff needed by the extractor.
      character(80) keytim, keypha, keygrd, keyccd
      character(80) keyuni, keymis, submis
      character(16) keyrat,keyrte, ratext
c This is the MAXIMUM of the TIMEDEL keywords for the read in files:
      double precision timedel
c The time reference for the spacecraft clock
      character(32) timesys
      double precision mjdreff
      integer mjdrefi
c Datamode
      character(16) datamode
c These are the possible position coordinates, and their size keywords:
      character(80) keyx(MXCORD), keyy(MXCORD)
      character(80) keyxsz(MXCORD), keyysz(MXCORD)
      character(80) xcolf,ycolf, xcolh, ycolh, xfkey,yfkey,xhkey,yhkey
      character(80) phamxkwd,gtinam,evtnam
      character(3) evtnum,gtinum,catnum
      character(80) keymis2
      
c The instrument name
      character(255) instru

c Real containing SIS echo fraction
      real sisecho

c Real with timepixr value
      real timepixr

c String containing maximum grade
      character(255) maxgrade

c String containing selection criteria
      character(2048) strsel(MXHIST),hkstr
      character(2048) ffstr(MXHIST)
      character(2048) catflt,choflt,catsel
c Number of selections in history buffer:
      integer nstrsel,nffstr
      
c String containing observation catalog parameter list
      character(255) obslis

c Integer for the pagewidth for FDUMP's
      integer pgwth
c String containing split threshold
      character(255) split
c Timing constants:
      double precision day2sec,sec2uni,day2uni

c ---------------------------------------------
c The filenames of the input FITS files.
      character(255) filenm(MAXFIL)
      
c The filenames of the corresponding GTI files
      character(255) gtifnm(MAXFIL)

c The filenames of the corresponding HK files
      character(255) hkflnm(MAXFIL)

c The index of the chosen files in the catalogue:
      integer catidx(MAXFIL)

c The filenames for the temporary workspace files.
      character(255) work1(MAXFIL), work2(MAXFIL)
      
c The filenames for the temporary workspaces for the GTI files
      character(255) gtiwk1(MAXFIL), gtiwk2(MAXFIL)

c The filenames for the temporary workspaces for the HK files
      character(255) hkwrk1(MAXFIL), hkwrk2(MAXFIL)

c The filenames internally stored by the observation catalogue
      character(255) catfil(MAXFIL)

c The filenames for output FITS files.
      character(255) filout(MAXFIL)
      
c A temporary file for working on the catalogue, etc
      character(255) tempfl 

c The list of the available device drivers for PGPLOT:
      character(10) devlst(MAXDEV)
c The number of devices currently loaded in PGPLOT:
      integer ndev
c ---------------------------------------------
c For BIN CURVE:
c Binsize for light curve creation
      real binsiz

c Start and end of time chunk for light curve creation
C      real tstart, tfin

c The time offset to get from extractor time to MJD:
      real timref

c Character strings corresponding to the above
C      character(20) sstart, sfin, sbinsz

c Number of bins in lightcurve
      integer nbins
            
c Upper and lower cutoffs for PHA, and the bounds
      integer phalcut, phahcut,phamax,phamin

c ---------------------------------------------
c For IMAGE (or BIN IMAGE)
c Binsizes in X and Y directions
      real xbinsz,ybinsz
c The offset and size factors
      integer xch,ych,xcf,ycf,sizeh,sizef
c Rebinning factor for the image
      integer xbinf

c For the GIS, the sizes for PI, X and RISE_TIME
      integer rxbval,ribval, phbval
C ----------------------------------------
C For Smooth image
c The X and Y windows for the boxcar smoothing
      real xwindow, ywindow
c The sigma and nsigma for the gaussian
      real sigma
      integer nsigma
c The boundary method, and smoothing method
      character(16) bound, smethod
c The value for the constant boundary method
      integer const
c Logicals, is there a smoothed file, has it been saved
      logical SMOOTH,SAVSMOOTH


c ---------------------------------------------
c For BIN SPEC
c The pha bin size, and the histogram rebinning factor
      integer phabin,extrbinh
c ---------------------------------------------
C For READ: 
c Character strings for file name index and constructed par-name
      character(2) filidx
      character(6) filpar

c Number of files read in
      integer nfiles

c Number of HK files read in
      integer nhkfil

c ---------------------------------------------
c For FILTER GRADE
      character(255) gfilter
c ---------------------------------------------
c For FILTER COLUMN
      character(255) colfilter
c ---------------------------------------------
c For FILTER PHASE
      real phase(2,MXPHAS)
      integer nphase
      double precision epoch,period

c ---------------------------------------------
c A bunch of random integers.

c Status of XPI and XSEL subroutines.
      integer status
      integer ierr

c The number of commands entered on the command line
      integer comno
      
c File I/O unit numbers
c      integer ilun, ilun2, ilun3
      
c HKMODE gives readin mode for the HK files
      integer hkmode
      
c Current Observation catalogue number
      integer obsno

c Number of files in the obscat
      integer nincat
      
c The number of MKF files
      integer nmkf

c General reusable integers for lengths of strings 
c      integer len1, len2, len3, len4, len5, len6
c ---------------------------------------------
c For HKBIN, FFBIN and PLOT

c Number of parameters to bin
      integer npar,nffpar
      
c Names of the parameters
      character(80) parlis(MAXCUR),ffplis(MAXCUR)
       
c Plotting device
      character(80) plotdv
c ---------------------------------------------
c SISCLEAN stuff
      integer cellsz, bkgthr, clnmet, cphal, cphah
      real logprb

c ---------------------------------------------
c Stuff for FAST mode
      integer arena,ario,stah,endh,ccdno
      character(4) in_or_out
c ---------------------------------------------
c LOGICALS
c
c Keep track of what operations have been performed. These 
c logicals are set according to whether their relevant commands
c have been performed, and checked whenever a 'subsequent'
c command is called (e.g. CURV checked to make sure a 
c BIN CURVE has been done before allowing a PLOT CURVE).

c Has data been read in?
      logical READ
      
c Has spectrum been accumulated?
      logical SPEC

c Has light curve been accumulated?
      logical CURV

c Has an image been accumulated?
      logical IMAGE
      
c Has a cleaned image been produced?
      logical CLEANI

c Is there a hot pixel removal region?
      logical CLEAN

c Did the CLEAN or DIRTY spectrum get extracted?
      logical DIRTY

c Has an FF curve been accumulated
      logical FFCURV 

c Has an HK curve been accumulated
      logical HKCURV

c Has a selection been applied?
      logical SELCT

c Has there been an HK selection?
      logical HKSEL
      
c Has there been a Filter file selection
      logical FFTFL

c Has the data been merged into one file?
      logical MERGED
      
c Does the data contain a GTI file?
      logical USEGTI
      
c Has the GTI data been merged into one file?
      logical MERGTI

c Are HK files being used?
      logical USEHK
      
c Have HK files been read in?
      logical HKREAD
      
c Are there many HK files?
      logical MANYHK
      
c Have the HK files been merged?      
      logical MERGHK

c Are the HK files in expanded form?
      logical EXPAND

c Are there many input data files? (as opposed to just one)
      logical MANY

c Has an observation catalogue (OBSCAT) been made?
      logical MADE

c Has an obscat been loaded externally
      logical LOADED

c Do MAKE OBSCAT and LOAD OBSCAT dump the catalogue
      logical SHOWOC

c Are we using 'workspace' files now rather than original
c input files? (i.e. as a result of a SELECT, F2B, etc
      logical WORK

c Are we using 'workspace' files now for the GTI, rather than original
c input files? (i.e. as a result of a SELECT, F2B, etc
      logical WRKGTI

c Are we using 'workspace' files now for the HK, rather than original
c input files? (i.e. as a result of a SELECT, F2B, etc
      logical WORKHK

c Have the files been filtered using the GTI
      logical FILTER
      
c Has there been a Region selected?
      logical REGION

c Is there an ascii time file in use
      logical ASCTFL
      
c Is there an xronos window file in use
      logical XWNTFL

c Is there a Xronos window file with phase info entered?
      logical XPHTFL

c Has the Xronos phase win. file been saved?
      logical XPHSAV
      
c Is there an fits gti time file in use
      logical FITTFL

c Is there a detector filter in use ?
      logical DETFL
      
c Are we using the output event list from the extractor$
      logical BININ

c Is the extractor putting out a filtered event list?
      logical BINOUT

c Has the filtered event list been  saved?
      logical EVTSAV

c Has the cleaned events file been saved?
      logical CLNSAV

c Do we want a weighted map?
      logical WTMAPB

c Do we want WMAP pixels outside the selected region set to -1?
      logical WTMAPFIX

c Do we need to invert the X-axis in the relation between WMAP and IMAGE pixels
      logical SWMAPX

c Do we need to invert the Y-axis in the relation between WMAP and IMAGE pixels
      logical SWMAPY

c ECHO the command file to the terminal?
      logical ECHO

c Are the events files time ordered?
      logical TORDER

c Have we output an unbinned light curve?
      logical UNBIN

c Do we want to use the QDP output?
      logical USEQDP

c Has FASTTIME been run on the data
      logical FAST

c Has FAINT to BRIGHT conversion been done?
      logical FAINT

c Is it Valid to make an image or spectrum:
      logical VIMAGE,VSPEC

c Is it valid to do REGION, PHA_CUTOFF filtering, or GISCLEAN
      logical VREGION, VPHACUT, VGISCLEAN

c Has an intensity GTI been created
      logical INTENS

c Does this mission have MKF files:
      logical HAVEMKF

c Should the adjustgti parameter be set when calling extractor
      logical ADJUSTGTI

c ---------------------------------------------
C COMMON BLOCKS
c ---------------------------------------------
c CHARACTER STRINGS

      common / xsel1 /  merfil, mrgtif, mrghkf, 
     +          work1f, work2f, wgti1f, wgti2f, wkhk1f,wkhk2f, 
     +          obsfil, obslis, datdir, mkfdir, hkdir,ffilin,allgti,
     +          hkstr,catflt,split, tempfl,evnin,evnout,usrdfe,dfefil,
     +          savfil,prefix,catnam,choflt,catsel,chocat,refdir,
     +          maxgrade,mermkf,hotpxl,catdir,bound,smethod,timesys
      common /binfiles/ curfil,curfits,unbfil,spcfil,imfil,simfil,
     +                    clnim,regfil,detfil,ascflt,hktfl,sisreg,
     +                    gtiflt,xwnflt,xphflt,xronwn,ascout,ffflt,
     +                    ctif,hndf,intf
      common /mission/  keytim, keypha, keygrd, keyccd, keyuni, 
     +                    keymis, submis,
     +                    keyrat,keyrte,ratext,phamxkwd,mkfdnm,mkreld,
     +                    gtinam,gtinum,catnum,evtnam,evtnum,keymis2,
     +                    xcolf,ycolf,xcolh, ycolh, xfkey,yfkey,
     +                    xhkey,yhkey,instru,datamode,in_or_out
      common /defaults/ ftype,filidx,filpar,xsltyp,prompt
      common /xselcmd/ cmdfil, lstfil,errfil, wrkdir

c CHARACTER ARRAYS

      common / xsel2 / filenm, work1, work2, filout,hkcurf,ffcurf,   
     +      gtiwk1, gtiwk2, gtifnm, hkwrk1, hkwrk2, hkflnm,parlis,
     +      plotdv, obscat,catfil,ascvec,ctivec,regvec,detvec,gtivec,
     +      hndvec,intvec,devlst,ffplis,mkfnam,mkfpar,intexp,
     +      keyx,keyy,keyxsz,keyysz,catcol,strsel,ffstr,gfilter,
     +      colfilter

c MISCELLANEOUS REALS & INTEGERS

      common / xsel3i / catidx,nbins, nfiles,nhkfil,npar,obsno,
     +                    status, ierr,hkmode,phabin,extrbinh,nffpar,
     +                    nmkf,nmkfpr,pgwth,comno,ndev,clnmet,
     +                    phahcut,phalcut,phamin,phamax,numhnd,
     +                    numcti,nincat,numreg,numdet,numasc,numgti,
     +                    numint,cphal,cphah,cellsz,bkgthr,ncatcl,
     +                    nphase,sigma,nsigma,const,nstrsel,nffstr,
     +                    arena,ario,stah,endh,ccdno,
     +                    ctindx,hndndx,intndx,rxbval,ribval, phbval,
     +                    xch,ych,xcf,ycf,sizeh,sizef

      common / xsel3r / binsiz,xbinsz, ybinsz, xbinf,sisecho,timepixr,
     +                    logprb,phase,timref,xwindow, ywindow
      common /xsel3d /  epoch,period,timedel,day2sec,sec2uni,
     +                    day2uni,mjdreff,mjdrefi

c LOGICALS

      common /xsel_log_gen / ECHO,SHOWOC,XPHSAV,EVTSAV,CLNSAV

      common / xsel_log_data / READ,MERGED, MANY, MADE, WORK,  
     +            USEGTI, MERGTI, WRKGTI, LOADED,
     +            BININ,BINOUT,TORDER,HKREAD,MANYHK,
     +            USEHK, MERGHK, WORKHK, EXPAND, UNBIN 
     +                   
      common /xsel_log_prod /SPEC, CURV, HKCURV, SELCT, IMAGE, CLEAN,
     +                         CLEANI, FFCURV, WTMAPB, WTMAPFIX, SWMAPX,
     +                         SWMAPY, FAST,
     +                         FAINT, DIRTY, USEQDP, SMOOTH, SAVSMOOTH,
     +                         VIMAGE,VSPEC,ADJUSTGTI

      common /xsel_log_filt /FILTER,REGION,ASCTFL,FITTFL,DETFL,
     +                         XWNTFL,FFTFL,HKSEL,XPHTFL,VREGION, 
     +                         VPHACUT, VGISCLEAN,INTENS,HAVEMKF

c Control variables 
c True if the batchmode parameter is set
      logical QBATCH
      common /xsel_control/QBATCH
c ---------------------------------------------
c [end of xselvar.inc]



