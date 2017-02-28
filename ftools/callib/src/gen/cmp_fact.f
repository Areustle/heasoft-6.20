
*+CMP_FACT
c     ----------------------------------------------------------------
      subroutine cmp_fact(finchan,tlscop,instrum,nchan,dmode,cmpmode,
     &                    channel,poschans,tlmin,
     &                    nfacts,stbins,endbins,cfacts,task,
     &                    errflg,chatter)
c     ----------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c This routine determines the compression factor(s), in order to rebin
c data. 
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE 
      character*(*) task,cmpmode,dmode,tlscop
      integer finchan, chatter, nfacts, errflg, nchan,poschans
      character*(*) instrum
      integer stbins(*), endbins(*), cfacts(*), channel(*),tlmin
c   
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c cmpmode   char  : compression mode - Linear,Faint2bright,bright2linear
c dmode     char  : Datamode
c finchan   int   : Number of desired channels after compression
c nchan     int   : Actual channels present in dataset
c poschans  int   : Number of possible channels (DETCHANS)
c tlscop    char  : Telescope name
c instrum   char  : Instrument name
c task      char  : Task that calls this routine - some of the code is
c                   task specific
c nfacts    int   : Number of compression factors
c cfacts    int   : Array of compression factors
c stbins    int   : Array of compression starting points for each cfact
c endbins   int   : Array of compression ending points for each cfact 
c channel   int   : Channel array
c tlmin     int   : Starting channel
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------------
c
c Rehana Yusaf (1993 July 13)
c Rehana Yusaf (1993 August 23) 1.0.1;Do not read infile, if 'NONE'
c                                     task added as passed parameter
c Rehana Yusaf (1993 Oct 19) 1.0.2; Infile no longer passed as an
c                                   argument, in fact datamode is passed
c                                   as an argument, as well as cmpmode
c Rehana Yusaf (1993 Dec 3) 1.0.3; Additional argument tlmin passed to
c                                  cope with ASCA instruments starting
c				   with Channel 0. 
c Rehana Yusaf (1994 Sept 12) 1.0.4; Only print warning about compression
c                                  factor/datamode mismatch at high chatter
c Rehana Yusaf (1995 Dec 14) 1.0.5; add wtinfo and friends
c
c Banashree Mitra Seifert (1996 May) 1.0.6:
c           . format increased from I4 to I6 at label 100
c           . character(6) instead of character(4) for chanchar,chanchar2,ccomp
c ---------------------------------------------------------------------
      character(5) version
      parameter (version = '1.0.6')
*-
c ----------------------------------------------------------------------
c
c
c --- LOCAL VARIABLES --------------------------------------------------
c
      character(8) subname
      parameter (subname='cmp_fact')
      integer  rem, comp, ierr, i
ccc      character(4) chanchar, chanchar2, ccomp
      character(6) chanchar, chanchar2, ccomp
      character(80) errinfo, desc 
c 
c --- USER INFO ---
c
      desc = ' using '//subname//' '//version
      call wtinfo(chatter,15,1,desc)

c --- COMPRESSION FACTOR/ERROR CHECKING ---

      IF (finchan.LT.0) THEN
        errinfo = ' -ve resultant channels, not allowed '
        call wterrm(subname,version,errinfo)
        errflg = 1
        return
      ELSEIF (finchan.EQ.poschans) THEN
        errinfo = ' final channels = present channels !'
        call wterrm(subname,version,errinfo)
        errflg = 1
        return
      ELSEIF (finchan.EQ.0) THEN
        errinfo = ' 0 final channels not allowed !!'
        errflg = 1
        call wterrm(subname,version,errinfo)
        return
      ENDIF

c ASCA SIS CHECKS ...

      call crmvlbk(tlscop)
      call crmvlbk(instrum)
      call crmvlbk(dmode)
      IF ((tlscop(1:5).EQ.'ASCA').AND.
     &     (instrum(1:3).EQ.'SIS')) THEN
        IF (dmode.NE.'  ') THEN
           IF (cmpmode(1:1).EQ.'F') THEN
             IF (dmode(1:5).NE.'FAINT') THEN
                errinfo = ' Datamode is '//dmode
                call wtwarm(subname,version,chatter,15,errinfo) 
                errinfo = ' Compression Mode is '//cmpmode
                call wtwarm(subname,version,chatter,15,errinfo)
           ELSEIF (cmpmode(1:1).EQ.'B') THEN
             IF (dmode(1:6).NE.'BRIGHT') THEN
                errinfo = ' datamode is '//dmode
                call wtwarm(subname,version,chatter,9,errinfo)
                errinfo = ' compression mode is '//cmpmode
                call wtwarm(subname,version,chatter,9,errinfo)
               ENDIF
             ENDIF          
           ENDIF
         ENDIF
      ENDIF

c --- IF FINCHAN NOT EXACT DIVISOR OF NCHAN THEN ERROR ---

      comp = poschans/finchan
      rem = MOD(poschans,finchan) 
      IF (rem.NE.0) THEN
        errinfo = ' finchan not exact divisor of'
     &//' original channels'
        call wterrm(subname,version,errinfo)
        write(chanchar,100,IOSTAT=ierr) finchan
        write(chanchar2,100,IOSTAT=ierr) poschans
        errinfo = chanchar//' is not a factor of '//chanchar2
        call rmvexsp(errinfo,desc)
        call wtinfo(chatter,0,1,desc)
        errflg = 2
        return
      ENDIF

c --- SETUP COMPRESSION ARRAYS ---

c LINEAR CASE ...

      IF (cmpmode(1:1).EQ.'L') THEN
        nfacts = 1 
        stbins(nfacts) = tlmin 
        endbins(nfacts) = tlmin + poschans - 1
        cfacts(nfacts) = comp

c FAINT2BRIGHT CASE ...

      ELSEIF (cmpmode(1:1).EQ.'F') THEN
        nfacts = 1
        comp = poschans/(2*finchan) 
        IF (comp.LT.1) THEN
          errinfo = ' Invalid compression ,< 1'
          call wterrm(subname,version,errinfo)
          errflg = 2
          return
        ENDIF 
        stbins(nfacts) = tlmin 
        endbins(nfacts) = tlmin + poschans/4 - 1
        cfacts(nfacts) = comp

        nfacts = nfacts + 1 
        stbins(nfacts) = endbins(nfacts - 1) + 1
        endbins(nfacts) = tlmin + poschans/2 - 1
        cfacts(nfacts) = comp * 2

        nfacts = nfacts + 1
        stbins(nfacts) = endbins(nfacts - 1) + 1
        endbins(nfacts) = tlmin + poschans - 1
        cfacts(nfacts) = comp * 4

c BRIGHT2LINEAR CASE ...

      ELSEIF (cmpmode(1:1).EQ.'B') THEN
        comp = (2*poschans)/finchan
        nfacts = 1
        stbins(nfacts) = tlmin 
        endbins(nfacts) = tlmin + poschans/2 - 1
        cfacts(nfacts) = comp

        nfacts = nfacts + 1
        stbins(nfacts) = endbins(nfacts-1) + 1
        endbins(nfacts) = tlmin + (3*poschans)/4 - 1
        cfacts(nfacts) = comp/2

        nfacts = nfacts + 1
        stbins(nfacts) = endbins(nfacts-1) + 1
        endbins(nfacts) =tlmin +  poschans - 1
        cfacts(nfacts) = comp/4
      ENDIF 

c USER INFO ...

      IF (chatter.GE.20) THEN
          do i=1,nfacts
          write(chanchar,100,IOSTAT=ierr) stbins(i)
          write(chanchar2,100,IOSTAT=ierr) endbins(i)
          write(ccomp,100,IOSTAT=ierr) cfacts(i)
          errinfo = chanchar//' - '//chanchar2
     &//' are binned with compression factor '//ccomp
          call rmvexsp(errinfo,desc)
          call wtinfo(chatter,20,2,desc)
        enddo
      ENDIF     
c  100 FORMAT(I4)
  100 FORMAT(i6)
      return
      end
c --------------------------------------------------------------------
c     END OF CMP_FACT
c --------------------------------------------------------------------

