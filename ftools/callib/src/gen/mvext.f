
*+MVEXT
c     -----------------------------------------------------
      subroutine mvext(rwmode,infile,iunit,ninstr,instr,
     &                 nsearch,next,outhdu,extnames,outver,
     &                 extname,errflg,chatter)
c     -----------------------------------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c This routine opens infile (passed filename) and moves to the
c desired extension, either specifed as INFILE[extnum] or by
c searching for HDUCLAS/EXTNAME values. If more than one of the
c desired extension is found an error is returned.
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,instr(*),extname
      integer iunit,ninstr,nsearch,errflg,chatter
      integer rwmode,next(*)
      character*(*) outhdu(9,*),extnames(*),outver(9,*)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c  rwmode    int    i : read/write mode, the file is opened using
c                       this mode - 0 is readonly,1 is read and write
c  infile    char   i : filename
c  iunit     int    o : i/o unit number
c  ninstr    int    i : Number of hduclas values to be searched for
c  instr     char   i : array of hduclass strings to serach for
c  nsearch   int    i : Number of extensions searched
c  extname   char   i : extension name to search for,if HDUCLAS keys 
c                       not present
c  extnames  char   o : array of extnames found
c  chatter   int    i : chattines flag
c  errflg    int    i : Error flag       = 0  okay
c                                        = 1  error opening file
c                                        = 2  >1 extensions found
c                                        = 3  0  extensions found
c                                        = 4  problem moving to ext  
c
c --- CALLED ROUTINES -----------------------------------------------
c
c MVER   : CALLIB routine which moves to desired extension
c WTINFO : LIBRARY/UTILTIES/GEN writes user info to screen
c WTFERR : LIBRARY/UTILTIES/GEN Writes error message to screen
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c     Rehana Yusaf (1994 March 21) 1.0.0;
c
c     Rehana Yusaf (1994 May 24) 1.1.0; subroutine MVER has been 
c                                       introduced. It moves to the
c                                       desired extension, assuming 
c                                       the file is already open.
c     Rehana Yusaf (1994 August 24) 1.1.1; bugfix:outver should be 2D !
c                                       as fnd* has it as 2D
c     Rehana Yusaf (1995 Dec 13) 1.1.2; use wtinfo and friends
c     Banashree M Seifert (1997, Mar27)1.2.0:
c               . report the name of the failed file
c ---------------------------------------------------------
      character(5) version
      parameter (version = '1.2.0')
*-
c -------------------------------------------------------------------
c
c --- LOCAL VARIABLES ---
c
      character(5) subname
      parameter (subname='mvext')
      character(170) subinfo
      character(512) filename
      integer extnum,status,block
c
c --- USER INFO ---
c
      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,15,1,subinfo)
      errflg = 0
c   
c --- OPEN INFILE ---
c
      status = 0
      call fcpars(infile,filename,extnum,status)
      IF (status.NE.0) THEN
        subinfo='error parsing infile '//filename
        call wtferr(subname,version,status,subinfo)
        errflg = 1
        return
      ENDIF
      call cgetlun(iunit)
      call ftopen(iunit,filename,rwmode,block,status)
      IF (status.NE.0) THEN
        subinfo='error opening infile '//filename
        call wtferr(subname,version,status,subinfo)
        errflg = 1
        return
      ENDIF
c
c --- MOVE TO DESIRED EXTENSION USING EXTNUM/HDUCLAS/EXTNAME ---
c
      call mver(iunit,extnum,ninstr,instr,
     &          nsearch,next,outhdu,extnames,outver,
     &          extname,errflg,chatter)
      return
      end
c --------------------------------------------------------------------
c     END OF MVEXT
c -------------------------------------------------------------------- 

