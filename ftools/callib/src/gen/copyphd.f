
*+COPYPHD
      subroutine copyphd(infile,outfile,killit,errflg,chatter)
c     -------------------------------------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c This subroutine copys the primary header from infile to output file
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,outfile
      integer chatter, errflg
      logical killit
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments ...
c
c infile     char   : Input filename
c outfile    char   : Output filename
c chatter    int    : Chattiness flag
c
c --- CALLED ROUTINES ------------------------------------------------
c
c FTOPEN            : (FITSIO) Open FITS file
c FTINIT            : (FITSIO) Open/Create new FITS file
c FTCOPY            : (FITSIO) Copy header and data
c FTCLOS            : (FITSIO) Close FITS file
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c Rehana Yusaf (1993 April 2) 
c Rehana Yusaf (1993 July 20) : use cgetlun to get unit numbers
c Rehana Yusaf (1993 Aug 5) 1.0.2: USE FTCOPY, instead of calling
c                           copying header and data using seperate routines 
c Rehana Yusaf (1994 July 20) 1.0.3; replace cgetlun with ftgiou
c Rehana Yusaf (1994 Sept 13) 1.0.4; additional argument killit, and 
c                                    use opfits instead of ftinit
c Rehana Yusaf (1995 Dec 14) 1.0.5; add wtinfo and friends 
      character(5) version
      parameter (version = '1.0.5')
*-
c --------------------------------------------------------------------
c
c --- INTERNALS -----------------------------------------------------
c
      character(7) subname
      parameter (subname='copyphd')
      character(70) subinfo
      integer block,iunit,ounit,status,nmore
c
c --- USER INFO ---
c
      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,20,2,subinfo)
c
c --- OPENING INPUT AND OUTPUT FILES ---
c
      status = 0
      call ftgiou(iunit,status)
      IF (status.NE.0) THEN
        subinfo = ' obtaining free lun'
        call wterrm(subname,version,subinfo)
        errflg = status
        return
      ENDIF
      call ftopen(iunit,infile,0,block,status)
      subinfo = ' opening infile '
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,subinfo)
        errflg = 1
        return
      ENDIF
      status = 0
      block = 2880
      call ftgiou(ounit,status)
      IF (status.NE.0) THEN
        subinfo = ' obtaining free lun'
        call wtferr(subname,version,status,subinfo)
        errflg = status
        return
      ENDIF
      call opfits(ounit,outfile,killit,chatter,status)
      subinfo = ' opening outfile '
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,subinfo)
        errflg = 1
        return
      ENDIF
c
c --- USE FTCOPY TO COPY HEADER AND DATA
c
      nmore = 0 
      call ftcopy(iunit,ounit,nmore,status)  
      subinfo = ' writing primary header and array'
      call wtferr(subname,version,status,subinfo)
      IF (status.NE.0) THEN
        errflg = 1
        return
      ENDIF
c
c --- CLOSE FILES ---
c
      status = 0
      call ftclos(iunit,status)
      status = 0
      call ftclos(ounit,status)
      status = 0
      call ftfiou(iunit,status)
      call ftfiou(iunit,status)
      return
      end
c -----------------------------------------------------------------
c     END OF COPYPHD
c -----------------------------------------------------------------

