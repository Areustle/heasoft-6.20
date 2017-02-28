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
c	This is XSEL.INC
c
c		Alan Smale, 1992 November.


c Maximum number of data files to use. Note:
c   (1) Increasing MAXFIL requires you to change the XSEL.PAR file
c   (2) If you increase MAXFIL over 999 some surgery will be 
c needed in READ and XSL_I2CC

      integer MAXFIL

c Maximum length of strings

c      integer MAXLEN
      
c Maximum number of selection files allowed
 
      integer MXNSEL
      
c Maximum number of MKF files allowed

      integer MXNMKF

c The maximum number of MKF parameters allowed:

      integer MXMKFP

c THe maximum number of HK or MKF parameters to bin

      integer MAXCUR      

c The maximum number of devices for PGPLOT
      integer MAXDEV

c The maximum number of Coordinates for each mission
      integer MXCORD

c The maximum number of different obscat (one for each instrument)
      integer MAXOBS

c The maximum number of columns in the obscat
      integer MXCATC

c The maximum number of PHASE intervals allowed
      integer MXPHAS

c The size of the history buffers for selections
      integer MXHIST
c The version number

      character(5) xslver

c System disk, system directory, help file directory, program
c This stuff is, of course, system and platform specific.

      character(255) xsldsk, xsldir, xslnam
c ----------------------------------------
c This sets the array sizes for all the arrays:

      parameter(MAXFIL=500)
c      parameter(MAXLEN=256)
      parameter(MXNSEL=32)
      parameter(MXNMKF = 64)
      parameter(MAXCUR = 64)
      parameter(MAXDEV = 40)
      parameter(MXMKFP = 128)
      parameter(MXCORD = 3)
      parameter(MAXOBS = 64)
      parameter (MXCATC = 256)
      parameter (MXPHAS = 64)
      parameter (MXHIST = 64)
c ----------------------------------------
c This is the common block for the xpi command variables:

      common /xsel_xpi/  xsldsk,xsldir,xslnam,xslver



