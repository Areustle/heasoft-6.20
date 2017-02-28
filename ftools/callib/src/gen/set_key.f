
*+SET_KEY
c     -------------------------------------------------------
      subroutine set_key(tlscpe,instrum,filter,texpos,qascale,ascale,
     &                   qbscale,bscale,cscale,backfile,corfile,
     &                   respfile,ancrfile,xflt,n_xflt,
     &                   ckeys,nckeys,cform,ckrec,keydesc,errflg,
     &                   chatter)
c     -------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c This subroutine sets arrays for use with the CHKEY command :
c CKEYS   : contains keywords that can be changed
c CFORM   : contains each keyword format
c KEYDESC : Keyword descriptions 
c CKREC   : Record for each keyword, with value and desc.
c Example setting :
c
c12345678901234567890123456789012345678901234567890123456789012345678901
c    AREASCAL= 0.1000000000000E+01 
c    
c CKREC(n) ^       
c          
c KEYS(n)    =
c 
c    AREASCAL
c
c CFORM(n)   =
c
c    AREASCAL E
c
c KEYDESC(n) =
c
c     nominal effective area
c
c ------------------------------------------------------------------
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) tlscpe, instrum, filter
      character*(*) xflt(*),backfile,corfile,respfile,ancrfile
      real texpos, ascale, bscale, cscale
      integer n_xflt, chatter, errflg, nckeys
      logical qascale, qbscale
      character*(*) ckeys(*)
      character*(*) cform(*)
      character*(*) ckrec(*)
      character*(*) keydesc(*)
c
c --- INTERNALS ----------------------------------------------------
c
      integer i,pos,j
      character(70) subinfo
      character(120) curx
      character(20) crnum
      character(4) cnum
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c Arguments ...
c
c tlscpe     char   : Telescope name
c instrum    char   : Instrument name
c filter     char   : Name of filter in use
c backfile   char   : name of background file
c corfile    char   : Name of correction scaling file
c respfile   char   : Name of response file
c ancrfile   char   : Name of Ancilliary response file
c xflt(9)    char   : XSPEC selection filter names
c texpos     real   : exposure time
c qascale    logical: true if vector areascal
c ascale     real   : area scaling factor (only use if .NOT.qascale)
c qbscale    logical: true if vector backscal
c bscale     real   : background scaling factor (only use if .NOT.qbscale)
c cscale     real   : correlation scaling factor
c n_xflt     int    : counter for xflt
c ckeys      char   : Array of keywords, that can have their values
c                     changed
c cform      char   : Array of keywords, and their formats
c ckrec      char   : Array of record for each keyword, with value
c                     and description
c nckeys     int    : counter for keywords
c chatter    int    : Chattiness flag (>20 verbose)
c
c --- AUTHORS/MODIFICATION -----------------------------------------
c
c Rehana Yusaf (1993 June 29)
c Rehana Yusaf (1994 July 13) 1.0.1; Omit "/" from keydesc
c
c Banashree Mitra Seifert (March 1996)1.1.0:
c        . Introduced screen display routines
c        . removed comments so that it writes telescope, instrument etc.
c kaa (2001 June 6) 1.2.0:
c        . Added support for vector AREASCAL and BACKSCAL
c ------------------------------------------------------------
      character(8) subname
      character(5) version
      parameter (version = '1.2.0')
*-
c ------------------------------------------------------------------
      subname = 'set_key'
      subinfo= 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)
 
c --- SETUP CHKEY ARRAYS ---

      nckeys = 1
      ckeys(nckeys) = 'TELESCOP'
      cform(nckeys) = 'TELESCOP S'
      pos = index(tlscpe(1:),' ')
      IF (pos.LT.20) THEN
        pos = 18
      ENDIF
cccc      keydesc(nckeys) ='TELESCOP '//' / Telescope (mission) name '
cccc      ckrec(nckeys) = 'TELESCOP= '''//tlscpe(1:pos)
cccc     &//''' / Telescope (mission) name '
      keydesc(nckeys) ='Telescope (mission) name '
      ckrec(nckeys) = 'TELESCOP= '//tlscpe(1:pos)
      nckeys = nckeys + 1
      ckeys(nckeys) = 'INSTRUME'
      cform(nckeys) = 'INSTRUME S'
       pos = index(instrum(1:),' ')
      IF (pos.LT.20) THEN
        pos = 18
      ENDIF  
cccc      keydesc(nckeys) ='INSTRUME '//' / Instrument name'
cccc      ckrec(nckeys) = 'INSTRUME= '''//instrum(1:pos)
cccc     &//''' / Instrument name'
      keydesc(nckeys) ='Instrument name'
      ckrec(nckeys) = 'INSTRUME= '//instrum(1:pos)
      nckeys = nckeys + 1
      ckeys(nckeys) = 'FILTER'
      cform(nckeys) = 'FILTER   S'
      pos = index(filter(1:),' ')
      IF (pos.LT.20) THEN
        pos = 18
      ENDIF  
cccc      keydesc(nckeys) ='FILTER   '//' / Instrument filter in use'
cccc      ckrec(nckeys) = 'FILTER  = '''//filter(1:pos)
cccc     &//''' / Instrument filter in use'
      keydesc(nckeys) ='Instrument filter in use'
      ckrec(nckeys) = 'FILTER  = '//filter(1:pos)
      nckeys = nckeys + 1
      ckeys(nckeys) = 'EXPOSURE'
      cform(nckeys) = 'EXPOSURE E'
cccc      keydesc(nckeys) = 'EXPOSURE '//' / Exposure time'
      keydesc(nckeys) = ' Exposure time'
      write(crnum,100) texpos
      ckrec(nckeys) = 'EXPOSURE= '//crnum
cccc     &//' / Exposure time'
      IF ( .NOT.qascale ) THEN
         nckeys = nckeys + 1
          ckeys(nckeys) = 'AREASCAL'
         cform(nckeys) = 'AREASCAL E'
cccc      keydesc(nckeys) = 'nominal effective area'
         keydesc(nckeys) = ' Area scaling factor  '
         write(crnum,100) ascale
         ckrec(nckeys) = 'AREASCAL= '//crnum
      ENDIF
      IF ( .NOT.qbscale ) THEN
         nckeys = nckeys + 1
         ckeys(nckeys) = 'BACKSCAL'
         cform(nckeys) = 'BACKSCAL E'
         keydesc(nckeys) = ' Background scale factor'
         write(crnum,100) bscale
         ckrec(nckeys) = 'BACKSCAL= '//crnum
      ENDIF
      nckeys = nckeys + 1
      ckeys(nckeys) = 'CORRSCAL'
      cform(nckeys) = 'CORRSCAL E'
      keydesc(nckeys) = ' Correlation scale factor'
      write(crnum,100) cscale
      ckrec(nckeys) = 'CORRSCAL= '//crnum
      nckeys = nckeys + 1
      ckeys(nckeys) = 'BACKFILE'
      cform(nckeys) = 'BACKFILE S'
      keydesc(nckeys) = 'Background FITS file'
      ckrec(nckeys) = 'BACKFILE= '//backfile
      nckeys = nckeys + 1
      ckeys(nckeys) = 'CORRFILE'
      cform(nckeys) = 'CORRFILE S'
      keydesc(nckeys)= 'Correlation FITS file'
      pos = index(corfile(1:),' ')
      ckrec(nckeys) = 'CORRFILE= '//corfile
      nckeys = nckeys + 1
      ckeys(nckeys) = 'RESPFILE'
      cform(nckeys) = 'RESPFILE S'
      keydesc(nckeys) ='redistribution matrix'
      ckrec(nckeys) = 'RESPFILE= '//respfile
      nckeys = nckeys + 1  
      ckeys(nckeys) = 'ANCRFILE'
      cform(nckeys) = 'ANCRFILE S'
      keydesc(nckeys) ='ancillary response'
      ckrec(nckeys) = 'ANCRFILE= '//ancrfile
      do i=1,n_xflt
        write(cnum,200) i
        do j=1,4
          IF (cnum(j:j).EQ.' ') THEN
            cnum(j:j) = '0'
          ENDIF
        enddo
        nckeys = nckeys + 1
        ckeys(nckeys) = 'XFLT'//cnum
        cform(nckeys) = 'XFLT'//cnum//' S'
        keydesc(nckeys) = 'XSPEC selection filter description'
        curx = xflt(i)
        ckrec(nckeys) =ckeys(nckeys)//'= '//curx
      enddo
 100  FORMAT(E20.13)
 200  FORMAT(I4)
      return
      end
c --------------------------------------------------------------------
c     END OF SET_KEY
c --------------------------------------------------------------------
