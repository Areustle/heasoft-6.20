
*+RDETM1
c     -----------------------------------------------------------
      subroutine rdetm1(dmapfil,map_max,instrum,telescop,
     &                  chanmin,chanmax,tmap,chatter,ierr)
c     -----------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads a detector map from a FITS Primary Array
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer map_max,ierr,chatter,chanmin,chanmax
      real*4 tmap(map_max,map_max)
      character*(*) instrum,telescop,dmapfil
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c dmapfil    char   : detector map filename
c map_max    int    : Array dimensions
c chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c tmap       real   : detector map
c chanmin    int    : Minimum PI channel for image
c chanmax    int    : Maximum PI channel for image
c instrum    char   : Instrument/Detector name
c telescop   char   : Telescope name
c ierr       int    : Error flag, ierr = 0 okay
c                                 ierr = 1 error finding extension 
c                                 ierr = 2 Column/keyword number not found
c                                 ierr = 3 Error in reading data
c                                 ierr = 4 Mandatory keyword not found
c
c --- CALLED ROUTINES -----------------------------------------------------
c
c subroutine FTOPEN      : (FITSIO) opens FITS file
c subroutine FTGKY(sj)   : (FITSIO) reads keyword 
c subroutine FTCLOS      : (FITSIO) closes FITS file
c subroutine FTG2DE      : (FITSIO) read image data
c subroutine FCECHO      : FTOOLS routine to write to screen
c subroutine WT_FERRMSG  : Writes FITSIO error text if required
c
c --- COMPILATION AND LINKING ---------------------------------------------
c
c Link with FTOOLS - FITSIO, CALLIB
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c
c Rehana Yusaf (Oct 1 1993) 1.0.0;
c Rehana Yusaf (Jan 27 1995) 1.0.1; if chanmin/chanmax not found it is
c                                   not fatal 
c Rehana yusaf (Jan 11 1996) 1.0.2; add wtinfo,wtferr etc for screen 
c                                   display
       character(5) version
       parameter (version = '1.0.2' )
       character(6) subname
       parameter (subname = 'rdetm1')
*-
c _________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(70) subinfo,errinfo
      character(40) comm
      integer status,naxis1,naxis2
      integer block,iunit
      logical anyflg
c
c      --- USER INFO ---
c
       ierr = 0
       subinfo =' using '//subname//' '//version
       call wtinfo(chatter,15,1,subinfo)
c
c --- OPEN FILE ---
c
       call cgetlun(iunit)
       status = 0
       call ftopen(iunit,dmapfil,0,block,status)
       errinfo = ' opening DETECTOR MAP file !'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 1
         return
       ENDIF
c
c     --- READING KEYWORDS ---
c

c READ NAXIS1

       status = 0
       call ftgkyj(iunit,'NAXIS1',naxis1,comm,status)
       errinfo = ' reading NAXIS1'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF                   

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',naxis2,comm,status)
       errinfo = ' reading NAXIS2'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF

c check that array naxis1 and naxis2 are 512 

       IF ((naxis1.NE.512).OR.(naxis2.NE.512)) THEN
         errinfo = ' NAXIS1 and NAXIS2 should be 512'
         call wterrm(subname,version,errinfo)
         ierr = 5
         return
       ENDIF

c READ INSTRUMENT NAME

       status = 0
       call ftgkys(iunit,'INSTRUME',instrum,comm,status)
       errinfo = ' reading instrument keyword'
       call wtfwrn(subname,version,chatter,20,status,errinfo)
       IF (status.EQ.202) THEN
         instrum = 'UNKNOWN'
       ENDIF

c READ TELESCOP NAME

       status = 0
       call ftgkys(iunit,'TELESCOP',telescop,comm,status)
       errinfo = ' reading telescope keyword'
       call wtfwrn(subname,version,chatter,20,status,errinfo)
       IF (status.EQ.202) THEN
         instrum = 'UNKNOWN'
       ENDIF              

c READ CHANMIN 

       status = 0
       call ftgkyj(iunit,'CHANMIN',chanmin,comm,status)
       errinfo = ' reading CHANMIN keyword '
       call wtfwrn(subname,version,chatter,20,status,errinfo)
c       IF (status.NE.0) THEN
c         ierr = 2
c         return
c       ENDIF

c READ CHANMAX

       status = 0
       call ftgkyj(iunit,'CHANMAX',chanmax,comm,status)
       errinfo = ' reading CHANMAX keyword '
       call wtfwrn(subname,version,chatter,20,status,errinfo)
c       IF (status.NE.0) THEN
c         ierr = 2
c         return
c       ENDIF         

c
c --- READING DATA ---
c

       status = 0
       call ftg2de(iunit,0,0,512,naxis1,naxis2,tmap,anyflg,status)
       errinfo = ' reading image'
       call wtfwrn(subname,version,chatter,20,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF
       subinfo = ' data has been read'
       call wtinfo(chatter,20,2,subinfo)
       status = 0
       call ftclos(iunit,status)
       errinfo = ' closing file'
       call wtfwrn(subname,version,chatter,5,status,errinfo)
       return
       end
c ------------------------------------------------------------------------
c     END OF SUBROUTINE RDETM1
c ------------------------------------------------------------------------


