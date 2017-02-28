C***********************************************************************
C TASK NAME: ascaray
C
C FILE NAME: ascaray.f
C
C DESCRIPTION: 
C     This ftool provides a ftool standard way of doing ASCA raytracing
C     It is version 1.0 and is based on the Japanese program 
C     ray2areav60cal
C     with enhancements from Keith Gendreau's version of the code.
C
C AUTHOR/DATE: Richard L. Fink 631 March 1997
C
C NOTES: 
C     
C     
C
C
C
C USAGE:
C     Host: ascaray
C     IRAF: ascaray
C
C ROUTINES IMPLEMENTED IN THIS FILE:
C     subroutine ascary - top level subroutine, called by IRAF or host
C         C wrapper in hascaray.c
C     subroutine ARPARAM - subroutine to get the par file params
C     subroutine ARXRT - subroutine to read the contents of the X-Ray
C                          Telescope definition file
C     subroutine ARTRACE - subroutine to trace photons at a specific
C                            energy
C
C MODIFICATION HISTORY:
C     
C     
C***********************************************************************

C***********************************************************************
C SUBROUTINE: ascary
C
C DESCRIPTION: top level subroutine
C
C
C AUTHOR/DATE: Richard L. Fink 631 March 1997
C
C NOTES:
C
C ARGUMENTS: none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine ARPARAM - uses xpi parameter interface to get 
C                            parameters
C     subroutine ARXRT   - read the XRT definition file
C     subroutine ARTRACE - raytrace a series of photons at an energy
C     subroutine raytraceffluctinit
C     subroutine raytracestprndinit  
C     subroutine henketable - create owens atomic constants
C library routine and their locations:
C - ftools/host/libftools.a
C     subroutine fcpars - extract file name and extention
C     subroutine fcerr  - write message to STDERR 
C - ftools/host/libfitsio.a
C     subroutine ftopen  - open a FITS file
C     subroutine ftinit - create a FITS file
C     subroutine ftphpr - create standard CHDU keywords
C     subroutine ftghpr - get standard CHDU keywords
C     subroutine ftmahd - move to an absolute HDU
C     subroutine ftibin - insert a binary table
C     subroutine ftptdm - put a TDIM keyword
C     subroutine ftpcom - put a COMMENT keyword
C     subroutine ftphis - put a HISTORY keyword
C     subroutine ftp2dj - write a 2D image to the primary array
C     subroutine ftclos - close a FITS file
C
C MODIFICATION HISTORY:
C     09/01/98 PDW - Fix wrong error code check for FTOPEN
C
C***********************************************************************

      subroutine ascary()
      implicit none

C Variables for Parameters
        integer OWENS
        Parameter (OWENS=1)
        integer NAGOYA
        Parameter (NAGOYA=2)
        integer FLATFIELD
        Parameter (FLATFIELD=0)
        integer BETAMODEL
        Parameter (BETAMODEL=1)
        integer EAMODE
        Parameter (EAMODE=2)
        integer PSFMODE
        Parameter (PSFMODE=3)
        integer COMMENTCARDS
        Parameter (COMMENTCARDS=60)

C Local dummy variables
        integer i,j
C Local variables
        character(80) context
        character(66) sccards(COMMENTCARDS)
        character(5) sversion
        integer status
        integer jiounit
        integer jengindex
        double precision denergy
        integer jextnum
        integer jrwmode
        integer jblock
        character(8) stype(84)
        character(8) sform(84)
        character(8) sunit(84)
        logical bsimple
        integer jbitpix
        integer jnaxis
        integer jnaxisdim(99)
        logical bextend
        integer jrows
        character(8) sextname
        character(80) scomment
        integer jrow
        double precision dgoldens
        double precision drough
        integer jimage(1024,1024)
        integer*2 iimage(1024,1024)
        integer jhdutype
        integer jpcount
        integer jgcount

C Variables for .par file  contents 
        integer jranseed
        integer jnph
        integer jdifftype
        logical bmask
        integer jregion
        real fconsct
        logical bflct
        integer jtelescope
        integer jxybins
        integer jconstants
        real fincang1
        real fincang2
        real fdiffang
        real fbeta
        real fenergy
        real frough
        real fsigma1
        real fsigma2
        real fcalss3
        real fcalsc3
        real fxymin
        real fxymax
        real fgoldens
        real falin
        character sxrtfile*255
        character sdefxrtfile*255
        character soutfile*255
        character sfilename*255
        logical bverbose
        logical bimage
        logical beefcalc
        integer jrunmode
        integer jnea
        integer jnpsf

C Commons for ANL core subroutines
      double precision r, beta, z
      Common /Raytracestruct/ r(120,4),beta(120,2),z(4)

      double precision sigma1, sigma2, amean, consct
      Common /Raytracebroad/ sigma1, sigma2, amean, consct

      double precision s3, c3
      Common /Raytracecalsct/ s3, c3

      integer iseed
      common /rndsd/ iseed

      double precision angin,angphi,diftyp,difrad,difbet
      common /ascaray_angle/ angin,angphi,diftyp,difrad,difbet

C Commons for ascaray
        integer jenergycol
        integer jtracedcol
        integer jfocalcol
        integer jtotal0col
        integer jtotal910col
        integer jtotal911col
        integer jtotal915col
        integer jtotal916col
        integer jtotal920col
        integer jtotal921col
        integer jtotal923col
        integer jtotal924col
        integer jtotal925col
        integer jtotal930col
        integer jtotal935col
        integer jtotal940col
        integer jtotal945col
        integer jtotal950col
        integer jtotal955col
        integer jtotal960col
        integer jtotal970col
        integer jtotal980col
        integer jtotal990col
        integer jeff0col
        integer jeff910col
        integer jeff911col
        integer jeff915col
        integer jeff916col
        integer jeff920col
        integer jeff921col
        integer jeff923col
        integer jeff924col
        integer jeff925col
        integer jeff930col
        integer jeff935col
        integer jeff940col
        integer jeff945col
        integer jeff950col
        integer jeff955col
        integer jeff960col
        integer jeff970col
        integer jeff980col
        integer jeff990col
        integer jarea0col
        integer jarea910col
        integer jarea911col
        integer jarea915col
        integer jarea916col
        integer jarea920col
        integer jarea921col
        integer jarea923col
        integer jarea924col
        integer jarea925col
        integer jarea930col
        integer jarea935col
        integer jarea940col
        integer jarea945col
        integer jarea950col
        integer jarea955col
        integer jarea960col
        integer jarea970col
        integer jarea980col
        integer jarea990col
        integer jeefnrmcol
        integer jeefprmcol
        integer jeefseccol
        integer jeefdircol
        integer jeefbakcol
        integer jeeftotcol
        integer jceefnrmcol
        integer jceefprmcol
        integer jceefseccol
        integer jceefdircol
        integer jceefbakcol
        integer jceeftotcol
        integer jeefradcol
        integer jeefringcol
        integer jeefpixscol
        integer jeefpcolcol
        integer jeefeefcol
        integer jeeferrcol
        integer jeefpsfcol
        integer jeefpsfercol
        integer jhitspscol

        common /fitscol1/ 
     &  jenergycol, jtracedcol, jfocalcol, jtotal0col, 
     &  jtotal910col, jtotal911col, jtotal915col, jtotal916col, 
     &  jtotal920col, jtotal921col, jtotal923col, jtotal924col, 
     &  jtotal925col, jtotal930col, jtotal935col, jtotal940col, 
     &  jtotal945col, jtotal950col, jtotal955col, jtotal960col, 
     &  jtotal970col, jtotal980col, jtotal990col, jeefnrmcol,
     &  jeefprmcol, jeefseccol, jeefdircol, jeefbakcol, 
     &  jeeftotcol, jceefnrmcol, jceefprmcol, jceefseccol, 
     &  jceefdircol, jceefbakcol, jceeftotcol, jeefradcol,
     &  jeefringcol, jeefpixscol, jeefpcolcol, jeefeefcol, 
     &  jeeferrcol, jeefpsfcol, jeefpsfercol, jhitspscol 
        common /fitscol2/
     &  jeff0col, jeff910col, jeff911col, jeff915col, 
     &  jeff916col, jeff920col, jeff921col, jeff923col, 
     &  jeff924col, jeff925col, jeff930col, jeff935col, 
     &  jeff940col, jeff945col, jeff950col, jeff955col,
     &  jeff960col, jeff970col, jeff980col, jeff990col, 
     &  jarea0col, jarea910col, jarea911col, jarea915col, 
     &  jarea916col, jarea920col, jarea921col, jarea923col,
     &  jarea924col, jarea925col, jarea930col, jarea935col, 
     &  jarea940col, jarea945col, jarea950col, jarea955col, 
     &  jarea960col, jarea970col, jarea980col, jarea990col


C Setup common block for taskname--necessary for fcerr
      character(40) taskname
      common /task/taskname

C Local Init
      taskname = 'ascaray'
      status = 0
      sversion = 'V1.0'
C create the comment cards for the FITS header
      i=0
      i=i+1
      sccards(i) =
     &'Created by ascaray '// sversion
      i=i+1
      sccards(i) =
     &'DESCRIPTION OF COLUMN NAMES ************************************'
      i=i+1
      sccards(i) =
     &'energy column is the photon energy in keV'
      i=i+1
      sccards(i) =
     &'traced column is the total number of photons run'
      i=i+1
      sccards(i) =
     &'focal column is the # of photons traced that hit the focal plane'
      i=i+1
      sccards(i) =
     &'Columns totalXXX, effXXX, and areaXXX have XXX defined below;'
      i=i+1
      sccards(i) =
     &'They are the TOTAL photons for each result code,'
      i=i+1
      sccards(i) =
     &'the EFFiciency of the mirrors at producing that result code, and'
      i=i+1
      sccards(i) =
     &'the telescope AREA in mm**2 for that result code'
      i=i+1
      sccards(i) =
     &'(effXXX times total telescope area)'
      i=i+1
      sccards(i) =
     &'Result codes (XXX) for photons:'
      i=i+1
      sccards(i) =
     &'  0 : normal end on focal plane'
      i=i+1
      sccards(i) =
     &'910 : hidden by primary housing'
      i=i+1
      sccards(i) =
     &'911 : hidden by primary housing side wall'
      i=i+1
      sccards(i) =
     &'915 : hidden by secondary housing'
      i=i+1
      sccards(i) =
     &'916 : hidden by secondary housing side wall'
      i=i+1
      sccards(i) =
     &'920 : hidden by 1st alignment bar'
      i=i+1
      sccards(i) =
     &'921 : hidden by 2nd alignment bar'
      i=i+1
      sccards(i) =
     &'923 : hidden by 3rd alignment bar'
      i=i+1
      sccards(i) =
     &'924 : hidden by 4th alignment bar'
      i=i+1
      sccards(i) =
     &'925 : hidden by 1&14 sector'
      i=i+1
      sccards(i) =
     &'930 : hidden by primary foil'
      i=i+1
      sccards(i) =
     &'935 : hidden by secondary foil'
      i=i+1
      sccards(i) =
     &'940 : absorbed by primary inner mirror'
      i=i+1
      sccards(i) =
     &'945 : absorbed by secondarr inner mirror'
      i=i+1
      sccards(i) =
     &'950 : absorbed on primary foil'
      i=i+1
      sccards(i) =
     &'955 : absorbed on secondary foil'
      i=i+1
      sccards(i) =
     &'960 : only primary reflect'
      i=i+1
      sccards(i) =
     &'970 : only secondary reflect'
      i=i+1
      sccards(i) =
     &'980 : no reflect'
      i=i+1
      sccards(i) =
     &'990 : abnormal reflected'
      i=i+1
      sccards(i) =
     &'The following eefXXX columns are photon counts by radius'
      i=i+1
      sccards(i) =
     &'and by the path of the photon to the focal plane'
      i=i+1
      sccards(i) =
     &'eefnrm column is the normal or result code 0 photons'
      i=i+1
      sccards(i) =
     &'eefprm column is the result code 960 photons'
      i=i+1
      sccards(i) =
     &'eefsec column is the result code 970 photons'
      i=i+1
      sccards(i) =
     &'eefdir column is the result code 980 photons'
      i=i+1
      sccards(i) =
     &'eefbak colums is the result code 990 photons'
      i=i+1
      sccards(i) =
     &'eeftot column is 0+960+970+980+990 codes'
      i=i+1
      sccards(i) =
     &'ceefnrm column is the cumulative eefnrm'
      i=i+1
      sccards(i) =
     &'ceefprm column is the cumulative eefprm'
      i=i+1
      sccards(i) =
     &'ceefsec column is the cumulative eefsec'
      i=i+1
      sccards(i) =
     &'ceefdir column is the cumulative eefdir'
      i=i+1
      sccards(i) =
     &'ceefbak column is the cumulative eefbak'
      i=i+1
      sccards(i) =
     &'ceeftot column is the cumulative eeftot'
      i=i+1
      sccards(i) =
     &'The following eefXXX columns are the Encircled Energy Function '
      i=i+1
      sccards(i) =
     &'calculations independent of photon path '
      i=i+1
      sccards(i) =
     &'WARNING: These eefXXX columns are calculated off of the internal'
      i=i+1
      sccards(i) =
     &'image and are sensitive to the run params xmin,xmax,ymin,ymax '
      i=i+1
      sccards(i) =
     &'eefrad column is the radii for the eefXXX columns'
      i=i+1
      sccards(i) =
     &'eefring'
      i=i+1
      sccards(i) =
     &'eefpixs'
      i=i+1
      sccards(i) =
     &'eefpcol'
      i=i+1
      sccards(i) =
     &'eefeef column is the Encircled Energy Function'
      i=i+1
      sccards(i) =
     &'eeferr column is the EEF error'
      i=i+1
      sccards(i) =
     &'eefpsf column is the Point Spread Function'
      i=i+1
      sccards(i) =
     &'eefpsfer colum is the PSF error'
      i=i+1
      sccards(i) =
     &'hitsps column is the 2D array of mirror impacts'
      i=i+1
      sccards(i) =
     &'   it is square and (0,0) based; the indici are '
      i=i+1
      sccards(i) =
     &'   (primaryhits,secondaryhits)'
      if (i .gt. COMMENTCARDS) then
         context = 'Buffer overflow in sccards array'
         goto 900
      endif

C Strip the par file of its contents
      call ARPARAM(jranseed,jnph,fincang1,fincang2,
     &              jdifftype, fdiffang, fbeta, bmask,
     &              jregion, fenergy, frough, fsigma1,
     &              fsigma2, fconsct, bflct, fcalss3,
     &              fcalsc3, sxrtfile, sdefxrtfile, soutfile,
     &              jtelescope, bimage, fxymin, fxymax, jxybins,
     &              fgoldens, falin, jconstants, beefcalc,
     &                    jrunmode, jnea, jnpsf,
     &              bverbose, status)
      if (status .ne. 0) then
          context = 'Error return from processing parameters contents'
          goto 900
      endif
        dgoldens = fgoldens
        drough = frough
        denergy = fenergy

C open the files
        jiounit = 10
C         Process the XRT file
        if (sxrtfile(1:7) .eq. 'DEFAULT') sxrtfile = sdefxrtfile
        call ARXRT(jiounit, sxrtfile, jtelescope, bflct,
     &                           bverbose, status)
C
C       open the OUTPUT file
C
*       Decompose dataname into file name and extension number
        call FCPARS( soutfile, sfilename, jextnum, status )
        if( status .ne. 0 ) then
          context =
     &      'Unable to parse output file name and extension number'
          goto 900
        end if

*       Ignore the extention number; extention will be added

*       Open the FITS file with rwmode=1 (read/write)
        jrwmode = 1
        jblock=0
        call FTOPEN( jiounit, sfilename, jrwmode, jblock, status )
        if (status .eq. 104) then
           status = 0
C           the file does not exist so we create it
           call FTINIT(jiounit, sfilename, jblock, status)
           if( status .ne. 0 ) then
              context='Unable to FTINIT FITS file:'//sfilename
             goto 900
           end if
C           create a default primary header
           bsimple = .true.
           jbitpix = 32
C           if we will write an image, leave space
           if (bimage) then
              jnaxis = 2
              jnaxisdim(1) = jxybins
              jnaxisdim(2) = jxybins
           else
              jnaxis = 0
              jnaxisdim(1) = 0
              jnaxisdim(2) = 0
           endif
           bextend = .true.
C           write the primary array keywords
           call FTPHPR(jiounit, bsimple, jbitpix, jnaxis, 
     &                        jnaxisdim, 0, 0, bextend, status)
           if( status .ne. 0 ) then
              context='Unable to FTPHPR FITS file:'//sfilename
             goto 900
           end if
        endif
        if( status .ne. 0 ) then
           context='Unable to open FITS file:'//sfilename
          goto 900
        end if

C now if we will write an image, check to see if the FITS CHU is kosher
        if (bimage) then
            call FTGHPR(jiounit, 99, bsimple, jbitpix, jnaxis,
     &                  jnaxisdim, jpcount, jgcount, bextend, 
     &                        status)
            if (jbitpix .ne. 32) then
                context = 'Can not overwrite existing primary header'
                goto 900
            endif
            if (jnaxis .ne. 2) then
                context = 'Can not overwrite existing primary header'
                goto 900
            endif
            if (jnaxisdim(1) .ne. jxybins) then
                context = 'Can not overwrite existing primary header'
                goto 900
            endif
            if (jnaxisdim(2) .ne. jxybins) then
                context = 'Can not overwrite existing primary header'
                goto 900
            endif
        endif

C add an extention
C        binary header
C 
C        the number of rows expected depends on the selected run mode
        if (jrunmode .eq. EAMODE) then
            jrows=300
        else if (jrunmode .eq. PSFMODE) then
            jrows=11
        else
            jrows=1
        endif
        stype(1) = 'energy'
        jenergycol = 1
        stype(2) = 'traced'
        jtracedcol = 2
        stype(3) = 'focal'
        jfocalcol = 3
        stype(4) = 'total0'
        jtotal0col = 4
        stype(5) = 'total910'
        jtotal910col = 5
        stype(6) = 'total911'
        jtotal911col = 6
        stype(7) = 'total915'
        jtotal915col = 7
        stype(8) = 'total916'
        jtotal916col = 8
        stype(9) = 'total920'
        jtotal920col = 9
        stype(10) = 'total921'
        jtotal921col = 10
        stype(11) = 'total923'
        jtotal923col = 11
        stype(12) = 'total924'
        jtotal924col = 12
        stype(13) = 'total925'
        jtotal925col = 13
        stype(14) = 'total930'
        jtotal930col = 14
        stype(15) = 'total935'
        jtotal935col = 15
        stype(16) = 'total940'
        jtotal940col = 16
        stype(17) = 'total945'
        jtotal945col = 17
        stype(18) = 'total950'
        jtotal950col = 18
        stype(19) = 'total955'
        jtotal955col = 19
        stype(20) = 'total960'
        jtotal960col = 20
        stype(21) = 'total970'
        jtotal970col = 21
        stype(22) = 'total980'
        jtotal980col = 22
        stype(23) = 'total990'
        jtotal990col = 23
        stype(24) = 'eff0'
        jeff0col = 24
        stype(25) = 'eff910'
        jeff910col = 25
        stype(26) = 'eff911'
        jeff911col = 26
        stype(27) = 'eff915'
        jeff915col = 27
        stype(28) = 'eff916'
        jeff916col = 28
        stype(29) = 'eff920'
        jeff920col = 29
        stype(30) = 'eff921'
        jeff921col = 30
        stype(31) = 'eff923'
        jeff923col = 31
        stype(32) = 'eff924'
        jeff924col = 32
        stype(33) = 'eff925'
        jeff925col = 33
        stype(34) = 'eff930'
        jeff930col = 34
        stype(35) = 'eff935'
        jeff935col = 35
        stype(36) = 'eff940'
        jeff940col = 36
        stype(37) = 'eff945'
        jeff945col = 37
        stype(38) = 'eff950'
        jeff950col = 38
        stype(39) = 'eff955'
        jeff955col = 39
        stype(40) = 'eff960'
        jeff960col = 40
        stype(41) = 'eff970'
        jeff970col = 41
        stype(42) = 'eff980'
        jeff980col = 42
        stype(43) = 'eff990'
        jeff990col = 43
        stype(44) = 'area0'
        jarea0col = 44
        stype(45) = 'area910'
        jarea910col = 45
        stype(46) = 'area911'
        jarea911col = 46
        stype(47) = 'area915'
        jarea915col = 47
        stype(48) = 'area916'
        jarea916col = 48
        stype(49) = 'area920'
        jarea920col = 49
        stype(50) = 'area921'
        jarea921col = 50
        stype(51) = 'area923'
        jarea923col = 51
        stype(52) = 'area924'
        jarea924col = 52
        stype(53) = 'area925'
        jarea925col = 53
        stype(54) = 'area930'
        jarea930col = 54
        stype(55) = 'area935'
        jarea935col = 55
        stype(56) = 'area940'
        jarea940col = 56
        stype(57) = 'area945'
        jarea945col = 57
        stype(58) = 'area950'
        jarea950col = 58
        stype(59) = 'area955'
        jarea955col = 59
        stype(60) = 'area960'
        jarea960col = 60
        stype(61) = 'area970'
        jarea970col = 61
        stype(62) = 'area980'
        jarea980col = 62
        stype(63) = 'area990'
        jarea990col = 63
        stype(64) = 'eefnrm'
        jeefnrmcol = 64
        stype(65) = 'eefprm'
        jeefprmcol = 65
        stype(66) = 'eefsec'
        jeefseccol = 66
        stype(67) = 'eefdir'
        jeefdircol = 67
        stype(68) = 'eefbak'
        jeefbakcol = 68
        stype(69) = 'eeftot'
        jeeftotcol = 69
        stype(70) = 'ceefnrm'
        jceefnrmcol = 70
        stype(71) = 'ceefprm'
        jceefprmcol = 71
        stype(72) = 'ceefsec'
        jceefseccol = 72
        stype(73) = 'ceefdir'
        jceefdircol = 73
        stype(74) = 'ceefbak'
        jceefbakcol = 74
        stype(75) = 'ceeftot'
        jceeftotcol = 75
        stype(76) = 'eefrad'
        jeefradcol = 76
        stype(77) = 'eefring'
        jeefringcol = 77
        stype(78) = 'eefpixs'
        jeefpixscol = 78
        stype(79) = 'eefpcol'
        jeefpcolcol = 79
        stype(80) = 'eefeef'
        jeefeefcol = 80
        stype(81) = 'eeferr'
        jeeferrcol = 81
        stype(82) = 'eefpsf'
        jeefpsfcol = 82
        stype(83) = 'eefpsfer'
        jeefpsfercol = 83
        stype(84) = 'hitsps'
        jhitspscol = 84
        sform(1) = '1D'
        sform(2) = '1J'
        sform(3) = '1J'
        sform(4) = '1J'
        sform(5) = '1J'
        sform(6) = '1J'
        sform(7) = '1J'
        sform(8) = '1J'
        sform(9) = '1J'
        sform(10) = '1J'
        sform(11) = '1J'
        sform(12) = '1J'
        sform(13) = '1J'
        sform(14) = '1J'
        sform(15) = '1J'
        sform(16) = '1J'
        sform(17) = '1J'
        sform(18) = '1J'
        sform(19) = '1J'
        sform(20) = '1J'
        sform(21) = '1J'
        sform(22) = '1J'
        sform(23) = '1J'
        sform(24) = '1D'
        sform(25) = '1D'
        sform(26) = '1D'
        sform(27) = '1D'
        sform(28) = '1D'
        sform(29) = '1D'
        sform(30) = '1D'
        sform(31) = '1D'
        sform(32) = '1D'
        sform(33) = '1D'
        sform(34) = '1D'
        sform(35) = '1D'
        sform(36) = '1D'
        sform(37) = '1D'
        sform(38) = '1D'
        sform(39) = '1D'
        sform(40) = '1D'
        sform(41) = '1D'
        sform(42) = '1D'
        sform(43) = '1D'
        sform(44) = '1D'
        sform(45) = '1D'
        sform(46) = '1D'
        sform(47) = '1D'
        sform(48) = '1D'
        sform(49) = '1D'
        sform(50) = '1D'
        sform(51) = '1D'
        sform(52) = '1D'
        sform(53) = '1D'
        sform(54) = '1D'
        sform(55) = '1D'
        sform(56) = '1D'
        sform(57) = '1D'
        sform(58) = '1D'
        sform(59) = '1D'
        sform(60) = '1D'
        sform(61) = '1D'
        sform(62) = '1D'
        sform(63) = '1D'
        sform(64) = '501J'
        sform(65) = '501J'
        sform(66) = '501J'
        sform(67) = '501J'
        sform(68) = '501J'
        sform(69) = '501J'
        sform(70) = '501J'
        sform(71) = '501J'
        sform(72) = '501J'
        sform(73) = '501J'
        sform(74) = '501J'
        sform(75) = '501J'
        sform(76) = '201E'
        sform(77) = '201E'
        sform(78) = '201E'
        sform(79) = '201E'
        sform(80) = '201E'
        sform(81) = '201E'
        sform(82) = '201E'
        sform(83) = '201E'
        sform(84) = '289J'
        sunit(1) = 'keV'
        sunit(2) = 'photons'
        sunit(3) = 'photons'
        sunit(4) = 'photons'
        sunit(5) = 'photons'
        sunit(6) = 'photons'
        sunit(7) = 'photons'
        sunit(8) = 'photons'
        sunit(9) = 'photons'
        sunit(10) = 'photons'
        sunit(11) = 'photons'
        sunit(12) = 'photons'
        sunit(13) = 'photons'
        sunit(14) = 'photons'
        sunit(15) = 'photons'
        sunit(16) = 'photons'
        sunit(17) = 'photons'
        sunit(18) = 'photons'
        sunit(19) = 'photons'
        sunit(20) = 'photons'
        sunit(21) = 'photons'
        sunit(22) = 'photons'
        sunit(23) = 'photons'
        sunit(24) = 'efficiency'
        sunit(25) = 'efficiency'
        sunit(26) = 'efficiency'
        sunit(27) = 'efficiency'
        sunit(28) = 'efficiency'
        sunit(29) = 'efficiency'
        sunit(30) = 'efficiency'
        sunit(31) = 'efficiency'
        sunit(32) = 'efficiency'
        sunit(33) = 'efficiency'
        sunit(34) = 'efficiency'
        sunit(35) = 'efficiency'
        sunit(36) = 'efficiency'
        sunit(37) = 'efficiency'
        sunit(38) = 'efficiency'
        sunit(39) = 'efficiency'
        sunit(40) = 'efficiency'
        sunit(41) = 'efficiency'
        sunit(42) = 'efficiency'
        sunit(43) = 'efficiency'
        sunit(44) = 'mm**2'
        sunit(45) = 'mm**2'
        sunit(46) = 'mm**2'
        sunit(47) = 'mm**2'
        sunit(48) = 'mm**2'
        sunit(49) = 'mm**2'
        sunit(50) = 'mm**2'
        sunit(51) = 'mm**2'
        sunit(52) = 'mm**2'
        sunit(53) = 'mm**2'
        sunit(54) = 'mm**2'
        sunit(55) = 'mm**2'
        sunit(56) = 'mm**2'
        sunit(57) = 'mm**2'
        sunit(58) = 'mm**2'
        sunit(59) = 'mm**2'
        sunit(60) = 'mm**2'
        sunit(61) = 'mm**2'
        sunit(62) = 'mm**2'
        sunit(63) = 'mm**2'
        sunit(64) = 'ph/bin'
        sunit(65) = 'ph/bin'
        sunit(66) = 'ph/bin'
        sunit(67) = 'ph/bin'
        sunit(68) = 'ph/bin'
        sunit(69) = 'ph/bin'
        sunit(70) = 'cph/bin'
        sunit(71) = 'cph/bin'
        sunit(72) = 'cph/bin'
        sunit(73) = 'cph/bin'
        sunit(74) = 'cph/bin'
        sunit(75) = 'cph/bin'
        sunit(76) = ' '
        sunit(77) = ' '
        sunit(78) = ' '
        sunit(79) = ' '
        sunit(80) = ' '
        sunit(81) = ' '
        sunit(82) = ' '
        sunit(83) = ' '
        sunit(84) = 'hits'
        sextname = 'ascaray'
C move to 2nd HDU
        call FTMAHD(jiounit, 2, jhdutype, status)
        if( status .ne. 0 .and. status .ne. 107) then
          write(context,*)'Error moving to extension #2 ', status
          goto 900
        end if
        status=0

C         insert a new binary table; forcing the existing down
        call FTIBIN( jiounit, jrows, 84, stype, sform, sunit, 
     &                     sextname, 0, status)
        if( status .ne. 0 ) then
          write(context,*)'Error creating requested extension', status
          goto 900
        end if
C        add a TDIM keyword for hitsps column
C        reuse variables
        jnaxis = 2
        jnaxisdim(1) = 17
        jnaxisdim(2) = 17
        call FTPTDM(jiounit, jhitspscol, jnaxis, jnaxisdim,
     &                    status)
        if( status .ne. 0 ) then
          context = 'Error adding TDIM for hitsps column'
          goto 900
        end if
C
C        write the COMMENT & HISTORY cards
C
        do 100 i=1,COMMENTCARDS
           call FTPCOM(jiounit, sccards(i), status)
           if( status .ne. 0 ) then
             context = 'Error creating comment card'
             goto 900
           end if
 100        continue

C        
        write(scomment,*) 'Random seed =',jranseed
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        write(scomment,*) 'Incident angle #1 =',fincang1
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        write(scomment,*) 'Incident angle #2 =',fincang2
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        if (jdifftype .eq. FLATFIELD) then
            scomment = 'Diffuse source type is FLAT FIELD'
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
            write(scomment,*) 'FLAT FIELD param =', fdiffang
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
        else if (jdifftype .eq. BETAMODEL) then
            scomment = 'Diffuse source type is BETA MODEL'
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
            write(scomment,*) 'BETA MODEL param =', fdiffang
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
            write(scomment,*) 'Beta model BETA =',fbeta
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
        else
            scomment = 'Diffuse source type is POINT SOURCE'
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
        endif
        if (bmask) then
           scomment = 'Telescope mask INCLUDED'
        else
           scomment = 'Telescope mask NOT included'
        endif
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        if (jregion .eq. 0) then
            scomment = 'Telescope region = QUADRANT'
        else if (jregion .eq. 1) then
            scomment = 'Telescope region = FULL TELESCOPE'
        else if (jregion .eq. 2) then
            scomment = 'Telescope region = SECTOR'
        else 
            scomment = 'Telescope region = UNKNOWN'
        endif
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        if (jrunmode .eq. EAMODE) then
            scomment = 'Run mode is NAGOYA EA'
        else if (jrunmode .eq. PSFMODE) then
            scomment = 'Run mode is NAGOYA PSF'
        else
            write(scomment,*)'Run mode is SINGLE ENERGY =',
     &                                fenergy, ' keV'
        endif
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        write(scomment,*)'Roughness param =', frough
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        write(scomment,*)'Lorenzian sigma1 param =', fsigma1
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        write(scomment,*)'Lorenzian inclination param =', falin
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        write(scomment,*)'Lorenzian sigma2 param =', fsigma2
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        write(scomment,*)'Scattering param =', fconsct
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        if (bflct) then
           scomment = 'FOIL Fluctuations ON'
        else
           scomment = 'FOIL Fluctuations OFF'
        endif
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        write(scomment,*)'CALS S3,C3 =', fcalss3,fcalsc3
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        scomment='XRT file is '//sxrtfile
        call FTPHIS( jiounit, scomment, status)
        if( status .ne. 0 ) then
          context = 'Error creating history card'
          goto 900
        end if
        if (jconstants .eq. NAGOYA) then
           scomment = 'Atomic constants are NAGOYA'
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
        else
           scomment = 'Atomic constants are OWENS'
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
           write(scomment,*)'Gold density =',fgoldens
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
        endif
        if (bimage) then
           write(scomment,*)'Image X-axis maximum =',fxymax
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
           write(scomment,*)'Image Y-axis maximum =',fxymax
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
           write(scomment,*)'Image X-axis minimum =',fxymin
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
           write(scomment,*)'Image Y-axis minimum =',fxymin
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
           scomment = 'BIN SIZE FOR EEF CALC IS 5'
            call FTPHIS( jiounit, scomment, status)
            if( status .ne. 0 ) then
              context = 'Error creating history card'
              goto 900
            end if
        endif

C Setup the global commons for the raytrace code
C        /Raytracecalsct/
        s3 = fcalss3
        c3 = fcalsc3
C        /rndsd/
        iseed = jranseed
        call Raytraceraninit(jranseed)
C        /angle/
C        convert incang1 from arcmin to degrees
        angin = dble(fincang1)/60.d0
        angphi = fincang2
        diftyp = dble(jdifftype)
        difrad = fdiffang
        difbet = fbeta
C        /Raytracebroad/
        sigma1 = fsigma1
C       Make sure that sigma1 is not exactly zero so that Raytraceffluctinit()
C       does not trigger a divide by zero
        if (sigma1 .eq. 0.0d0) sigma1 = 1.0d-8
        sigma2 = fsigma2
        amean = 0.0
        consct = fconsct
C initilizations for the ray trace code
        call Raytraceffluctinit(sigma1*6.d1,dble(falin))
        call Raytracestprndinit()
        if (jconstants .eq. OWENS) then
C           use the Gendreau subroutine to store the Owens/Henke
C           atomic constants
           call henketable()
        endif
C zero the image array if it will be used
        if (bimage) then
           do 202 i=1,1024
              do 201 j=1,1024
                 jimage(i,j) = 0
 201              continue
 202           continue
        endif

C execute the ray trace
C       select the mode we will run in according to the Nagoya standard
        jrow=0
        if (jrunmode .eq. EAMODE) then
           do  10 jengindex=1,1200
              if (jengindex.le.200.or.jengindex.ge.400) then
                 if (mod(jengindex,10).ne.0) go to 10
              endif
              denergy = jengindex * 1.d-2
              jrow=jrow+1
              call ARTRACE( jiounit, jimage, iimage, jrow, 
     &                              beefcalc, jrunmode, jnea, jnpsf,
     &                              jconstants, dgoldens, denergy,
     &                              drough, bimage, jnph, jregion,
     &                              fxymin, fxymax, jxybins,
     &                        bverbose, status)
 10           continue
        else if (jrunmode .eq. PSFMODE) then
           do 11 jengindex=1,100
              if (jengindex.ne.25) then
                 if (mod(jengindex,10).ne.0) goto 11
              endif
              denergy = jengindex * 0.1d0
              jrow=jrow+1
              call ARTRACE( jiounit, jimage, iimage, jrow, 
     &                              beefcalc, jrunmode, jnea, jnpsf,
     &                              jconstants, dgoldens, denergy,
     &                              drough, bimage, jnph, jregion,
     &                              fxymin, fxymax, jxybins,
     &                        bverbose, status)
 11           continue
        else
C           run the single request
           jrow=1
              call ARTRACE( jiounit, jimage, iimage, jrow, 
     &                              beefcalc, jrunmode, jnea, jnpsf,
     &                              jconstants, dgoldens, denergy,
     &                              drough, bimage, jnph, jregion,
     &                              fxymin, fxymax, jxybins,
     &                        bverbose, status)
        endif
C        write the image array if it was created
        if (bimage) then
           call FTMAHD( jiounit, 1, jhdutype, status)
           call FTP2DJ( jiounit, 0, jxybins, jxybins, jxybins, 
     &                        jimage, status)
            if( status .ne. 0 ) then
              context = 'Error creating primary array'
              goto 900
            end if
        endif
C
C  close the output FITS file
        call FTCLOS(jiounit, status)

C  normal escape to the calling environment
      return
C error return to calling environment
 900  continue
        call FCERR(context)
        call FTRPRT('STDERR', status)
        call FTCLOS(jiounit, status)
        return
      end

C***********************************************************************
C SUBROUTINE: ARPARAM
C
C DESCRIPTION: Reads the params from the par file via xpi
C
C AUTHOR/DATE: Richard L. Fink 631 March 1997
C
C NOTES: 
C
C
C ARGUMENTS:
C        jranseed        (o) : Starting seed for the random number generator
C        jnph                (o) : Number of photons to ray trace
C        jdifftype        (o) : Diffuse/point source type
C        bmask                (o) : Assume a mask over the telescope?
C        jregion        (o) : Region of the telescope to trace
C        fconsct        (o) : Consider the scattering componant?
C        bflct                (o) : Include foil misalignments?
C        jtelescope        (o) : Which telescope to assume?
C        fincang1        (o) : Photon incident angle in arcmin
C        fincang2        (o) : Photon phi rotation angle in degrees
C        fdiffang        (o) : Parameter for diffuse source types
C        fbeta                (o) : Beta param for Beta-type diffuse model
C        fenergy        (o) : X-ray energy in keV to trace
C        frough                (o) : Foil microroughness
C        fsigma1        (o) : Gamma for Lorenzian in sectors 3-12
C        fsigma2        (o) : Gamma for Lorentian in sectors 2/13
C        fcalss3        (o) : fscat calibration parameter(for normalization)
C        fcalsc3        (o) : fscat cal parameter(for characteristic length)
C        sxrtfile        (o) : XRT FITS descption file name
C        sdefxrtfile        (o) : Default XRT file name
C        soutfile        (o) : FITS file to output results to
C       bimage         (o) : Produce a PSF image true/false
C       fxymin         (o) : Minimum value for xy-axis for PSF image
C       fxymax         (o) : Maximum value for xy-axis for PSF image
C       jxybins        (o) : Number of bins on xy-axis for PSF image
C       fgoldens       (o) : Gold density for the mirror coating
C       falin                (o) : Lorenzian linear slope per arcmin
C        beefcalc        (o) : DO EEF CALC BY ENERGY?
C        jrunmode        (o) : Mode to run trace
C        jnea                (o) : Minimum number of traced photons in EA mode
C        jnpsf                (o) : Minimum number of traced photons in PSF mode
C        bverbose        (o) : Controls function tracing output
C       status                (o) : Returns a status flag (0 - no error)
C
C PRIMARY LOCAL VARIABLES:
C     context - error message
C
C CALLED ROUTINES:
C library routines and their locations:
C     subroutine uclgst - get string parameter - ftools/host/libhost.a
C     subroutine uclgsi - get integer parameter - ftools/host/libhost.a
C     subroutine uclgsr - get real parameter - ftools/host/libhost.a
C     subroutine uclgsb - get logical parameter - ftools/host/libhost.a
C     subroutine fcerr  - write message to stderr - ftools/host/libftools.a
C
C MODIFICATION HISTORY:
C
C
C***********************************************************************

C All parameter file I/O is isolated in this routine
      subroutine ARPARAM(jranseed,jnph,fincang1,fincang2,
     &              jdifftype, fdiffang, fbeta, bmask,
     &              jregion, fenergy, frough, fsigma1,
     &              fsigma2, fconsct, bflct, fcalss3,
     &              fcalsc3, sxrtfile, sdefxrtfile, soutfile,
     &              jtelescope, bimage, fxymin, fxymax, jxybins,
     &              fgoldens, falin, jconstants, beefcalc,
     &                    jrunmode, jnea, jnpsf, 
     &              bverbose, status)
        implicit none

C Calling sequence variables
        integer jranseed
        integer jnph
        integer jdifftype
        logical bmask
        integer jregion
        real fconsct
        logical bflct
        integer jtelescope
        integer jxybins
        integer jconstants
        real fincang1
        real fincang2
        real fdiffang
        real fbeta
        real fenergy
        real frough
        real fsigma1
        real fsigma2
        real fcalss3
        real fcalsc3
        real fxymin
        real fxymax
        real fgoldens
        real falin
        character*(*) sxrtfile
        character*(*) sdefxrtfile
        character*(*) soutfile
        logical bverbose
        logical bimage
        logical beefcalc
        integer jrunmode
        integer jnea
        integer jnpsf


C Variables for parameters
        integer OWENS
        Parameter (OWENS=1)
        integer NAGOYA
        Parameter (NAGOYA=2)
        integer FLATFIELD
        Parameter (FLATFIELD=0)
        integer BETAMODEL
        Parameter (BETAMODEL=1)
        integer EAMODE
        Parameter (EAMODE=2)
        integer PSFMODE
        Parameter (PSFMODE=3)

C Local variables
      integer status
      character(80) context


C The following "inherited status" protocol should be used in every ftool
C subroutine. A non zero status indicates an error occured already, so this
C subroutine should not take any action if this is the case.
      if(status .ne. 0) return
        
*       Reset status flag to no error
        status = 0

*       Get the name of the output result file 
        call UCLGST( 'outfile', soutfile, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the output result file name'
          goto 900
        endif

*       Get the name of the calibration (XRT definition ) file
        call UCLGST( 'xrtfile', sxrtfile, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the XRT definition file name'
          goto 900
        end if

*       Get the default XRT definition file name
        call UCLGST( 'defxrtfile', sdefxrtfile, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the default XRT definition file name'
          goto 900
        end if

*       Get the random number seed
        call UCLGSI( 'ranseed', jranseed, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the random number seed'
          goto 900
        end if

*       Get the run mode
        call UCLGSI( 'runmode', jrunmode, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the run mode'
          goto 900
        end if

        jnph = 0
        jnea = 0
        jnpsf = 0
        fenergy = 0.0
        if (jrunmode .eq. EAMODE) then        
*          Get the number of photons
           call UCLGSI( 'nea', jnea, status )
           if( status .ne. 0 ) then
             context = 'Failed to get the number of photons for EA MODE'
             goto 900
           end if
        else if (jrunmode .eq. PSFMODE) then
*          Get the number of photons
           call UCLGSI( 'npsf', jnpsf, status )
           if( status .ne. 0 ) then
             context = 'Failed to get the # of photons for PSF MODE'
             goto 900
           end if
        else
*          Get the number of photons
           call UCLGSI( 'nph', jnph, status )
           if( status .ne. 0 ) then
             context = 'Failed to get the number of photons'
             goto 900
           end if
*          Get the X-ray energy
           call UCLGSR( 'energy', fenergy, status )
           if( status .ne. 0 ) then
             context = 'Failed to get the X-ray energy'
             goto 900
           end if
        endif


*       Get the atomic constants source
        call UCLGSI( 'atomic', jconstants, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the atomic data source'
          goto 900
        end if

        fgoldens = 19.3
        if (jconstants .eq. OWENS) then
*          Get the gold density
           call UCLGSR( 'goldens', fgoldens, status )
           if( status .ne. 0 ) then
             context = 'Failed to get the gold density'
             write(context,*) fgoldens,status
             goto 900
           end if
        endif

*       Get the incident angle #1 in arcmin
        call UCLGSR( 'incang1', fincang1, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the incident angle #1'
          goto 900
        end if

*       Get the incident angle #2 in degrees (rotation)
        call UCLGSR( 'incang2', fincang2, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the incident angle #2'
          goto 900
        end if

*       Get the diffuse source type
        call UCLGSI( 'difftype', jdifftype, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the diffuse source type'
          goto 900
        end if

        fbeta = 0.0
        if (jdifftype .eq. BETAMODEL) then
C            BETA model param needed
C            Get the BETA model beta value
            call UCLGSR( 'beta', fbeta, status )
            if( status .ne. 0 ) then
              context = 'Failed to get the BETA model beta param'
              goto 900
            end if
        endif
        fdiffang = 0.0
        if (jdifftype .eq. FLATFIELD 
     &     .or. jdifftype .eq. BETAMODEL) then
C            DIFFANG param needed
C            Get the DIFFANG value
            call UCLGSR( 'diffang', fdiffang, status )
            if( status .ne. 0 ) then
              context = 'Failed to get diffuse source variation param'
              goto 900
            end if
        endif

*       Get the mask value
        call UCLGSB( 'mask', bmask, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the telescope mask param'
          goto 900
        end if

*       Get the region value
        call UCLGSI( 'region', jregion, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the telescope region param'
          goto 900
        end if

*       Get the telescope number value
        call UCLGSI( 'telescope', jtelescope, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the telescope number param'
          goto 900
        end if


*       Get the foil micro roughness
        call UCLGSR( 'rough', frough, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the microroughness'
          goto 900
        end if

*       Get the gamma of Lorentz function at 3-12sector (arcmin.)
        call UCLGSR( 'sigma1', fsigma1, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the gamma for sectors 3-12'
          goto 900
        end if

*       Get the linear slope of Lorentz function 
        call UCLGSR( 'lalin', falin, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the linear slope for lorenzian'
          goto 900
        end if

*       Get the gamma of Lorentz function at 2,13sector (arcmin.)
        call UCLGSR( 'sigma2', fsigma2, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the gamma for sectors 2,13'
          goto 900
        end if

*       Get the scattering flag
        call UCLGSR( 'consct', fconsct, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the consider scattering flag'
          goto 900
        end if

*       Get the foil fluctuation flag
        call UCLGSB( 'flct', bflct, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the foil fluctuation flag'
          goto 900
        end if

*       Get the fscat calibration parameter(for normalization)
        call UCLGSR( 'calss3', fcalss3, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the fscat cal param for norm'
          goto 900
        end if

*       Get the fscat calibration parameter(for characteristic length)
        call UCLGSR( 'calsc3', fcalsc3, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the fscat cal param for length'
          goto 900
        end if

*       Get the PSF image flag
        call UCLGSB( 'image', bimage, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the PSF image flag'
          goto 900
        end if
        fxymin=-12.8
        fxymax=12.8
        jxybins=1024
        if (bimage) then
*           Get the xy-axis minimum value
            call UCLGSR( 'xymin', fxymin, status )
            if( status .ne. 0 ) then
              context = 'Failed to get PSF image xy-axis minimum value'
              goto 900
            end if
*           Get the xy-axis maximum value
            call UCLGSR( 'xymax', fxymax, status )
            if( status .ne. 0 ) then
              context = 'Failed to get PSF image xy-axis maximum value'
              goto 900
            end if
*           Get the xy-axis bins value
            call UCLGSI( 'xybins', jxybins, status )
            if( status .ne. 0 ) then
              context = 'Failed to get PSF image xy-axis bins value'
              goto 900
            end if
        endif

*       Get the EEFCALC flag
        call UCLGSB( 'eefcalc', beefcalc, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the EEFCALC flag'
          goto 900
        end if

*       Get the verbose flag value
        call UCLGSB( 'verbose', bverbose, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the verbose flag value'
          goto 900
        end if


*       No error - return cleanly
        return

900     continue
*       Error condition occured --- return context
        call FCERR( context )

        end


C***********************************************************************
C SUBROUTINE: ARXRT
C
C DESCRIPTION: Opens, reads, and closes the XRT calibration file
C
C AUTHOR/DATE: Richard L. Fink 631 March 1997
C
C NOTES: 
C
C
C ARGUMENTS:
C       jiounit       (i) : Logical I/O unit from calling program
C        jtelescope        (i) : Which telescope to assume?
C        sxrtfile        (i) : XRT FITS descption file name
C       bflct          (i) : Apply foil fluctuations? (1=yes)
C        bverbose        (i) : Controls function tracing output
C       status                (o) : Returns a status flag (0 - no error)
C
C PRIMARY LOCAL VARIABLES:
C     context - error message
C
C CALLED ROUTINES:
C     function raytraceran - random number generator - rayanl.f
C library routines and their locations:
C - ftools/host/libftools.a
C     subroutine fcpars - parse file name/extention 
C     subroutine fcerr  - write message to stderr
C - ftools/host/libfitsio.a
C     subroutine FTOPEN - Open FITS file 
C     subroutine FTMRHD - Move to extention header
C     subroutine FTGHBN - Extract binary header info
C     subroutine FTCNO  - Determine column # from name
C     subroutine FTGKYD - Get keyword value 
C     subroutine FTGCVD - Get column value 
C     subroutine FTCLOS - Close FITS file 
C
C MODIFICATION HISTORY:
C
C
C**********************************************************************

        subroutine ARXRT (jiounit, sxrtfile, jtelescope, 
     &                     bflct, bverbose, status)

        implicit none

C Passed variables
        integer jiounit
        character*(*) sxrtfile
        integer jtelescope
        logical bflct
        logical bverbose
        integer status

C Local for PARAMETERS
        integer MAXCOLUMNS
        Parameter (MAXCOLUMNS=10)
        character(8) PTOPR
        Parameter (PTOPR='p_top_r')
        character(8) PBOTR
        Parameter (PBOTR='p_bot_r')
        character(8) STOPR
        Parameter (STOPR='s_top_r')
        character(8) SBOTR
        Parameter (SBOTR='s_bot_r')

C Local variables
        character context*80
        character sfilename*255
        integer jextnum
        integer jrwmode
        integer jblock
        integer jhdtype
        integer jmaxcolumns
        integer jfields
        integer jrowsfound
        character(8) stype(MAXCOLUMNS)
        character(8) sform(MAXCOLUMNS)
        character(8) sunit(MAXCOLUMNS)
        character sextname*80
        character scomment*255
        integer jpcount
        logical bexact
        logical banynull
        integer jptopr
        integer jpbotr
        integer jstopr
        integer jsbotr
        integer jindex
        double precision dpsize
        double precision dssize
        double precision dmisalign
        double precision dpfront
        double precision dpback
        double precision dsfront
        double precision dsback

C Commons
      double precision r, beta, z
      Common /Raytracestruct/ r(120,4),beta(120,2),z(4)

C Externals
        double precision raytraceran
        external raytraceran

C The following "inherited status" protocol should be used in every ftool
C subroutine. A non zero status indicates an error occured already, so this
C subroutine should not take any action if this is the case.
      if(status .ne. 0) return

*       Decompose dataname into file name and extension number
        call FCPARS( sxrtfile, sfilename, jextnum, status )
        if( status .ne. 0 ) then
          context =
     &      'Unable to parse data file name and extension number'
          goto 900
        end if

*       Check extension number for validity
        if( jextnum .eq. -99 ) then
           if (jtelescope .eq. 0) then
               jextnum = 1
           else
               jextnum = jtelescope
           endif
        else if( jextnum .eq. 0 ) then
          context = 'Primary array not supported'
          status = 999
          goto 900
        end if

*       Open the FITS file with rwmode=0 (readonly)
        jrwmode = 0
        jblock=0
        call FTOPEN( jiounit, sfilename, jrwmode, jblock, status )
        if( status .ne. 0 ) then
          context = 'Unable to open FITS file: ' // sfilename
          goto 900
        end if

C move to the extension requested
        call FTMRHD( jiounit, jextnum, jhdtype, status )
        if( status .ne. 0 ) then
          context = 'Error moving to requested extension'
          goto 900
        end if
        jmaxcolumns=MAXCOLUMNS
        if( jhdtype .eq. 2 ) then
          call FTGHBN( jiounit, jmaxcolumns, jrowsfound, jfields, 
     &                 stype, sform, sunit, sextname, jpcount, 
     &                 status )
        else
          context = 'File extension type not supported'
          goto 900
        end if
* Now find out the column numbers of 4 essential columns
        bexact = .true.
        call FTGCNO( jiounit,bexact, PTOPR, jptopr, status )
        if( status .ne. 0 ) then
          context = 'Primary TOP Radius not found: ' // PTOPR
          goto 900
        end if
        call FTGCNO( jiounit,bexact, PBOTR, jpbotr, status )
        if( status .ne. 0 ) then
          context = 'Primary BOTTOM Radius not found: ' // PBOTR
          goto 900
        end if
        call FTGCNO( jiounit,bexact, STOPR, jstopr, status )
        if( status .ne. 0 ) then
          context = 'Secondary TOP Radius not found: ' // STOPR
          goto 900
        end if
        call FTGCNO( jiounit,bexact, SBOTR, jsbotr, status )
        if( status .ne. 0 ) then
          context = 'Secondary BOTTOM Radius not found: ' // SBOTR
          goto 900
        end if
C Get the sizes of the mirrors
        call FTGKYD( jiounit, 'P_SIZE', dpsize, scomment, status)
        if( status .ne. 0 ) then
          context = 'PSIZE keyword not found in XRT cal file'
          goto 900
        end if
        call FTGKYD( jiounit, 'S_SIZE', dssize, scomment, status)
        if( status .ne. 0 ) then
          context = 'SSIZE keyword not found in XRT cal file'
          goto 900
        end if
C Get the distances to the mirror ends
        dpback=0.0d0
        call FTGKYD( jiounit, 'P_BACK', dpback, scomment, status)
        if ( status .ne. 0 ) then
           context = 'PBACK keyword not found in XRT cal file'
           goto 900
        end if
        z(2) = dpback
        dpfront=0.0d0
        call FTGKYD( jiounit, 'P_FRONT', dpfront, scomment, status)
        if ( status .ne. 0 ) then
           context = 'PFRONT keyword not found in XRT cal file'
           goto 900
        end if
        z(1) = dpfront
        dsback=0.0d0
        call FTGKYD( jiounit, 'S_BACK', dsback, scomment, status)
        if ( status .ne. 0 ) then
           context = 'SBACK keyword not found in XRT cal file'
           goto 900
        end if
        z(4) = dsback
        dsfront=0.0d0
        call FTGKYD( jiounit, 'S_FRONT', dsfront, scomment, status)
        if ( status .ne. 0 ) then
           context = 'SFRONT keyword not found in XRT cal file'
           goto 900
        end if
        z(3) = dsfront
C Get the fluctuation value if necessary
        dmisalign=0.0d0
        if (bflct) then
            call FTGKYD( jiounit, 'MISALIGN', dmisalign, 
     &                   scomment, status)
            if( status .ne. 0 ) then
              context = 'MISALIGN keyword not found in XRT cal file'
              goto 900
            end if
        endif

C Process the radii array
        if (jrowsfound .ne. 120) then
C           This should never happen
           context = 'Invalid number of rows in XRT cal file'
           go to 900
        endif
        do 100 jindex = 1,120
           call FTGCVD( jiounit, jptopr, jindex, 1, 1, 0.0d0, 
     &                  r(jindex,1), banynull, status)
           call FTGCVD( jiounit, jpbotr, jindex, 1, 1, 0.0d0, 
     &                  r(jindex,2), banynull, status)
           call FTGCVD( jiounit, jstopr, jindex, 1, 1, 0.0d0, 
     &                  r(jindex,3), banynull, status)
           call FTGCVD( jiounit, jsbotr, jindex, 1, 1, 0.0d0, 
     &                  r(jindex,4), banynull, status)
           if (bflct) then
C              randomly adjust the foil locations
              r(jindex,1) = r(jindex,1) 
     &                     + ((raytraceran()-0.5d0)*dmisalign)
              r(jindex,2) = r(jindex,2) 
     &                     + ((raytraceran()-0.5d0)*dmisalign)
              r(jindex,3) = r(jindex,3) 
     &                     + ((raytraceran()-0.5d0)*dmisalign)
              r(jindex,4) = r(jindex,4) 
     &                     + ((raytraceran()-0.5d0)*dmisalign)
           endif
C          We recalculate the mirror angles here simply because
C           the ray2areav60cal program did so. It would have been
C           necessary if foil fluctuation was on.
           beta(jindex,1) = (r(jindex,1)-r(jindex,2))/dpsize
           beta(jindex,2) = (r(jindex,3)-r(jindex,4))/dssize
 100        continue
C valid return
        call FTCLOS( jiounit, status )
        return

*       This is where serious error conditions should jump to
 900    continue
        call FCERR( context )
        call FTCLOS( jiounit, status )
        return

        end


C***********************************************************************
C SUBROUTINE: ARTRACE
C
C DESCRIPTION: Does a ray trace operation
C
C AUTHOR/DATE: Richard L. Fink 631 March 1997
C
C NOTES: 
C
C
C ARGUMENTS:
C       jiounit       (i) : Logical I/O unit from calling program
C        jimage                (i) : integrated PSF image over all energies
C        iimage                (i) : PSF image for single energy
C        jrow                (i) : The FITS table row to write to
C        beefcalc        (i) : Whether to do the the eef calc or not
C        jrunmode         (i) : Run mode (EA, PSF, Single energy)
C        jnea                (i) : Minimum # of photons for EA mode
C        jnpsf                (i) : Minimum # of photons for PSF mode
C        jconstants        (i) : Source of atomic constants (OWENS, NAGOYA)
C        dgoldens        (i) : Nominal gold density
C        denergy        (i) : X-ray energy in keV
C        drough                (i) : foil surface roughness param
C        bimage                (i) : Create the PSF image true/false
C        jnph                (i) : Number of photons to trace
C        jregion        (i) : Telescope region to use
C        fxymin                (i) : Minimum xyaxis value for image
C        fxymax                (i) : Maximum xyaxis value for image
C        jxybins        (i) : Number of xy-axis bins for image
C        bverbose        (i) : Controls function tracing output
C       status                (o) : Returns a status flag (0 - no error)
C
C PRIMARY LOCAL VARIABLES:
C     context - error message
C
C CALLED ROUTINES:
C     function raytraceran - random number generator - rayanl.f
C     subroutine refmkowens - make owens reflection table for current
C                                energy
C     function hdelta - calculate Owens optical delta for a 
C                        molecule
C     function hbeta  - calculate Owens optical beta for a 
C                        molecule
C     function raytraceclcdel - calculate optical delta for Nagoya Au
C     function raytraceclcbet - calculate optical beta for Nagoya Au
C     subroutine raytracephotongen - generates a photon to be traced
C     subroutine raytraceraytra - raytrace a single photon
C                                   using Nagoya atomic constants
C     subroutine owensraytra - raytrace a single photon
C                                using Owens Atomic constants
C library routines and their locations:
C - ftools/host/libftools.a
C     subroutine fcpars - parse file name/extention
C     subroutine fcerr  - write message to stderr
C - ftools/host/libfitsio.a
C     subroutine FTPCLD - write double precision scalar/vector to row
C     subroutine FTPCLJ - write integer*4 scalar/vector to row
C     subroutine FTPCLE - write real scalar/vector to row
C
C MODIFICATION HISTORY:
C
C
C***********************************************************************

        subroutine ARTRACE ( jiounit, jimage, iimage, jrow, 
     &                              beefcalc, jrunmode, jnea, jnpsf,
     &                              jconstants, dgoldens, denergy,
     &                              drough, bimage, jnph, jregion,
     &                              fxymin, fxymax, jxybins,
     &                        bverbose, status)

        implicit none

C Variables for passed parameters
        integer jiounit
        integer jxybins
        integer jimage(jxybins,jxybins)
        integer*2 iimage(jxybins,jxybins)
        integer jrow
        logical beefcalc
        integer jrunmode
        integer jnea
        integer jnpsf
        integer jconstants
        double precision dgoldens
        double precision denergy
        double precision drough
        logical bimage
        integer jnph
        integer jregion
        real fxymin
        real fxymax
        logical bverbose
        integer status

C Variables for Parameters
        integer OWENS
        Parameter (OWENS=1)
        integer NAGOYA
        Parameter (NAGOYA=2)
        integer EAMODE
        Parameter (EAMODE=2)
        integer PSFMODE
        Parameter (PSFMODE=3)
        double precision PI
        Parameter (PI=3.141592654d0)
        double precision DEGREE2RADIAN
        Parameter (DEGREE2RADIAN=1.745329252d-2)

C Local variables
        integer i,j
        double precision datomicdelta
        double precision datomicbeta
        character(80) context
        integer jeefnrm(501)
        integer jeefprm(501) 
        integer jeefsec(501) 
        integer jeefbak(501)
        integer jeefdir(501) 
        integer jeeftot(501)
        integer jphotonbyresult(0:999)
        double precision  dfracbyresult(0:999)
        double precision  dfracareabyresult(0:999)
        integer jmirrorhits(0:16,0:16)
        integer jtotaltraced
        integer j120bincnt
        double precision dfracofcircle
        double precision dfracofradius
        double precision drotateangle
        double precision dradius
        double precision dphotonx
        double precision dphotony
        double precision dphotonweight
        double precision dfplanex
        double precision dfplaney
        integer jfplanex
        integer jfplaney
        integer jradialbin
        double precision dx,dy
        double precision dcenterx,dcentery
        integer jphotoncondition
        double precision drotang
        integer jprimaryhits
        integer jsecondaryhits
        double precision dphotonincang1
        double precision dphotonincang2
        double precision dmirrorfrontarea
        logical bcorrupteef

C Local for old eefcalc3 subroutine now embedded
        real fradius(0:1024)
        real feef(0:1024)
        real fpsf(0:1024)
        real fdummypixs(0:1024)
        real fpixs(0:1024)
        real fring(0:1024)
        real fpcol(0:1024)
        real ferrpsf(0:1024)
        real ferreef(0:1024)
        integer jradialbinlimit
        real fxpixelsizemm
        real fypixelsizemm
        integer jphotonxcenter
        integer jphotonycenter
        integer jxcenter
        integer jycenter
        real fpixelradius
        integer jradialbinsize
        real fradialbinsize
        real foffsetradius


C Variables for common
      double precision angin,angphi,diftyp,difrad,difbet
      common /ascaray_angle/  angin,angphi,diftyp,difrad,difbet
      double precision r,beta,z
      common /Raytracestruct/ r(120,4),beta(120,2),z(4)

        integer jenergycol
        integer jtracedcol
        integer jfocalcol
        integer jtotal0col
        integer jtotal910col
        integer jtotal911col
        integer jtotal915col
        integer jtotal916col
        integer jtotal920col
        integer jtotal921col
        integer jtotal923col
        integer jtotal924col
        integer jtotal925col
        integer jtotal930col
        integer jtotal935col
        integer jtotal940col
        integer jtotal945col
        integer jtotal950col
        integer jtotal955col
        integer jtotal960col
        integer jtotal970col
        integer jtotal980col
        integer jtotal990col
        integer jeff0col
        integer jeff910col
        integer jeff911col
        integer jeff915col
        integer jeff916col
        integer jeff920col
        integer jeff921col
        integer jeff923col
        integer jeff924col
        integer jeff925col
        integer jeff930col
        integer jeff935col
        integer jeff940col
        integer jeff945col
        integer jeff950col
        integer jeff955col
        integer jeff960col
        integer jeff970col
        integer jeff980col
        integer jeff990col
        integer jarea0col
        integer jarea910col
        integer jarea911col
        integer jarea915col
        integer jarea916col
        integer jarea920col
        integer jarea921col
        integer jarea923col
        integer jarea924col
        integer jarea925col
        integer jarea930col
        integer jarea935col
        integer jarea940col
        integer jarea945col
        integer jarea950col
        integer jarea955col
        integer jarea960col
        integer jarea970col
        integer jarea980col
        integer jarea990col
        integer jeefnrmcol
        integer jeefprmcol
        integer jeefseccol
        integer jeefdircol
        integer jeefbakcol
        integer jeeftotcol
        integer jceefnrmcol
        integer jceefprmcol
        integer jceefseccol
        integer jceefdircol
        integer jceefbakcol
        integer jceeftotcol
        integer jeefradcol
        integer jeefringcol
        integer jeefpixscol
        integer jeefpcolcol
        integer jeefeefcol
        integer jeeferrcol
        integer jeefpsfcol
        integer jeefpsfercol
        integer jhitspscol

        common /fitscol1/ 
     &  jenergycol, jtracedcol, jfocalcol, jtotal0col, 
     &  jtotal910col, jtotal911col, jtotal915col, jtotal916col, 
     &  jtotal920col, jtotal921col, jtotal923col, jtotal924col, 
     &  jtotal925col, jtotal930col, jtotal935col, jtotal940col, 
     &  jtotal945col, jtotal950col, jtotal955col, jtotal960col, 
     &  jtotal970col, jtotal980col, jtotal990col, jeefnrmcol,
     &  jeefprmcol, jeefseccol, jeefdircol, jeefbakcol, 
     &  jeeftotcol, jceefnrmcol, jceefprmcol, jceefseccol, 
     &  jceefdircol, jceefbakcol, jceeftotcol, jeefradcol,
     &  jeefringcol, jeefpixscol, jeefpcolcol, jeefeefcol, 
     &  jeeferrcol, jeefpsfcol, jeefpsfercol, jhitspscol 
        common /fitscol2/
     &  jeff0col, jeff910col, jeff911col, jeff915col, 
     &  jeff916col, jeff920col, jeff921col, jeff923col, 
     &  jeff924col, jeff925col, jeff930col, jeff935col, 
     &  jeff940col, jeff945col, jeff950col, jeff955col,
     &  jeff960col, jeff970col, jeff980col, jeff990col, 
     &  jarea0col, jarea910col, jarea911col, jarea915col, 
     &  jarea916col, jarea920col, jarea921col, jarea923col,
     &  jarea924col, jarea925col, jarea930col, jarea935col, 
     &  jarea940col, jarea945col, jarea950col, jarea955col, 
     &  jarea960col, jarea970col, jarea980col, jarea990col


C Variables for Gendreau code
        integer jgold(10,2)
        integer compound(10,2)
        double precision dfilmthick
        double precision dfilmdensity
        double precision dfilmdelta
        double precision dfilmbeta

C Externals
        double precision raytraceclcdel, raytraceclcbet
        external raytraceclcdel, raytraceclcbet
        double precision hdelta, hbeta
        external hdelta, hbeta
        double precision raytraceran
        external raytraceran

C The following "inherited status" protocol should be used in every ftool
C subroutine. A non zero status indicates an error occured already, so this
C subroutine should not take any action if this is the case.
      if(status .ne. 0) return

C Initilization code
C
        bcorrupteef=.false.

C        set up the reflection tables for the current energy
              if (jconstants .eq. OWENS) then
C                  use the Gendreau code
C                  specify a pure gold formula for hdelta, hbeta
                  jgold(1,1) = 79
                  jgold(1,2) = 1
                  datomicdelta = hdelta(denergy*1000.0d0,
     &                                          jgold,1,dgoldens)
                  datomicbeta = hbeta(denergy*1000.0d0,
     &                                        jgold,1,dgoldens)
C                  simultate a dummy contamination layer for Gendreau code
C                  here we use a negligible layer of hydrogen
C                  rooting out the divide by zero that occurs without the layer
C                  will take too much time for this port
                  compound(1,1) = 79
                  compound(1,2) = 1
                  dfilmdensity = dgoldens
                  dfilmdelta = hdelta(denergy*1000.0d0,compound,
     &                                 1,dfilmdensity)
                  dfilmbeta = hbeta(denergy*1000.0d0,compound,
     &                                 1,dfilmdensity)
C                 dfilmdelta = 0.0d0
C                 dfilmbeta = 0.0d0
                  dfilmthick  =1.0d-8
                  call refmkowens(denergy, dfilmdelta, dfilmbeta,
     &                    datomicdelta,datomicbeta,
     &                    dfilmthick,drough,1)

              else if (jconstants .eq. NAGOYA) then
C                  use the ray2areav60cal code
C                  79 is Gold atomic number
                  datomicdelta = Raytraceclcdel(79,denergy)
                  datomicbeta  = Raytraceclcbet(79,denergy)
              else
C                  error!
                  context='Invalid atomic constants .par detected'
                  call FCERR(context)
                  return
              endif
C
C  MOST OF THE RAY2AREAV60CAL SUBROUTINE STRAYT IS INCLUDED HERE
C----------------------------------------------------------------
C zero out the array used to count photon impacts on the
C primary and secondary mirrors 
      do 10 i=0,16
        do 11 j=0,16
          jmirrorhits(i,j) = 0
 11     continue
 10   continue

C zero out the PSF image array only if we will do the eef calc
      if (beefcalc) then
         do 12 j=1,jxybins
            do 13 i=1,jxybins
               iimage(i,j)=0
 13         continue
 12      continue
      endif


C jphotonbyresult is the total of all the photons traced that had a 
C return code of jphotoncondition
C dfracbyresult is the fraction of all photons traced by 
C jphotoncondition
C dfracareabyresult is the fraction of the total telescope area by 
C jphotoncondition code
      jphotonbyresult(0) = 0
      dfracareabyresult(0) = 0.0d0
      dfracbyresult(0) = 0.0d0
      do 14 i=900,999
        jphotonbyresult(i) = 0
        dfracareabyresult(i) = 0.0d0
        dfracbyresult(i) = 0.0d0
 14   continue

C zero out arrays used for encircled energy function totals
C jeefnrm = encircled energy function normal photons 
C           jphotoncondition = 0 return from Raytraceraytra
C jeefprm = encircled energy function for photons that
C           undergo reflection off the primary mirror only
C           jphotoncondition = 960 return from Raytraceraytra
C jeefsec = encircled energy function for photons that
C           under go reflection off the secondary mirror only
C           jphotoncondition = 970 return from Raytraceraytra
C jeefdir = encircled energy function for photons that 
C           undergo NO reflection (direct)
C           jphotoncondition = 980 return from Raytraceraytra
C jeefbak = encircled energy function for abnormally
C           reflected photons (background)
C           jphotoncondition = 990 return from Raytraceraytra
C jeeftot = encircled energy function total
      do 15 i=1,501
         jeefnrm(i)=0
         jeefprm(i)=0
         jeefsec(i)=0
         jeefbak(i)=0
         jeefdir(i)=0
         jeeftot(i)=0
 15   continue

C     zero the total photon ray trace count
      jtotaltraced=0
C     zero the count of photons in the 1st 120 bins
      j120bincnt=0
C     zero the photon weight factor
      dphotonweight=0.d0

C START PHOTON RAY TRACE LOOP
 20   jtotaltraced = jtotaltraced + 1
      if (bverbose) then
         if (mod(jtotaltraced,10000) .eq. 0) then
             write (context,*) 'E =', denergy, 
     &                         ' Photons =',jtotaltraced
             call FCERR(context)
         endif
      endif
C     get the incident angle and rotation angle for the photon
C     this may be the value from the .par file IF using a point src
C     otherwise it will come from the model
      call Raytracephotongen(dphotonincang1,dphotonincang2)
C     compute a random position at the telescope for the photon
      dfracofcircle = raytraceran()
      if(jregion.eq.0) then
        drotateangle = dfracofcircle*2.0d0*PI/4.0d0
      elseif (jregion.eq.2) then
C         56 sectors in telescope
         drotateangle = dfracofcircle*2.0d0*PI/56.d0 + 2.0d0*PI/8.d0
      else
        drotateangle = 2.0*PI*dfracofcircle
      endif
      dfracofradius = raytraceran()
      dradius = r(1,4)**2 + (r(120,1)**2 - r(1,4)**2) *dfracofradius
      dradius = dsqrt(dradius)

      dphotonx  = dradius * dcos(drotateangle)
      dphotony  = dradius * dsin(drotateangle)
C     ray trace the photon
        if (jconstants .eq. NAGOYA) then
           call Raytraceraytra(denergy,
     &                           dphotonincang1, dphotonincang2,
     &                     dphotonx, dphotony,
     &                     drotateangle, dradius, dphotonweight,
     &                     dfplanex, dfplaney,
     &                          jphotoncondition,
     &                           jprimaryhits, jsecondaryhits)
        else if (jconstants .eq. OWENS) then
           call owensraytra(denergy,
     &                           dphotonincang1, dphotonincang2,
     &                     dphotonx, dphotony,
     &                     drotateangle,dradius,dphotonweight,
     &                     dfplanex,dfplaney,
     &                          jphotoncondition,
     &                           jprimaryhits, jsecondaryhits)
        else
            context = 'Invalid atomic contants requested'
            status = 999
            go to 900
        endif
        if (jregion.eq.2) then
C          ----------------------------------------------------------
C          rotate position with 45deg at focal plane
C          ----------------------------------------------------------
           dcenterx = 3500. * dsin(angin*DEGREE2RADIAN)
     &              * dcos(angphi*DEGREE2RADIAN)
           dcentery = 3500. * dsin(angin*DEGREE2RADIAN) 
     &              * dsin(angphi*DEGREE2RADIAN)
           dx = dfplanex - dcenterx
           dy = dfplaney - dcentery
           drotang=45.0d0*DEGREE2RADIAN
           dx = dx*dcos(drotang) + dy*dsin(drotang)
           dy = dy*dcos(drotang) - dx*dsin(drotang)
           dfplanex = dx + dcenterx
           dfplaney = dy + dcentery
        endif
C--------------------------------------------------------------------
C Build up the jmirrorhits array based on the number of hits on the
C primary mirror and secondary mirror
        if ( (jphotoncondition.eq.0)
     &       .or.(jphotoncondition.ge.960)) then
C           This only includes photons that get thru the reflection process

C           only track a maximum of 16 hits per mirror
           jprimaryhits = min(jprimaryhits,16)
           jsecondaryhits = min(jsecondaryhits,16)
           jmirrorhits(jprimaryhits,jsecondaryhits) = 
     &            jmirrorhits(jprimaryhits,jsecondaryhits) + 1
C
C            compute the impact point on the PSF image
           jfplanex = nint((dfplanex - fxymin) 
     *                      * dble(jxybins)
     &                      / (fxymax-fxymin)
     &                      - 0.5d0)
           jfplaney = nint((dfplaney - fxymin) 
     &                      * dble(jxybins)
     &                      / (fxymax-fxymin)
     &                      - 0.5d0)

C            are we are building the integrated PSF image?
           if (bimage) then
C               Make sure the pixel range is valid
               if (
     &            ((jfplanex.ge.1).and.(jfplanex.le.jxybins))
     &             .and.
     &            ((jfplaney.ge.1).and.(jfplaney.le.jxybins))
     &             ) then
                  jimage(jfplanex,jfplaney) =  
     &                            jimage(jfplanex,jfplaney) + 1
              endif
           endif
C        are we building the internal image?
           if (beefcalc) then
C           verify the pixel range is valid
               if (
     &            ((jfplanex.ge.1).and.(jfplanex.le.jxybins))
     &             .and.
     &            ((jfplaney.ge.1).and.(jfplaney.le.jxybins))
     &             ) then
                  iimage(jfplanex,jfplaney) =
     &                   iimage(jfplanex,jfplaney) + 1
C                  Because the Nagoya code uses an integer*2 array
C                  for internal use, it may overflow if more than
C                  32768 photons fall in a single bin; if this
C                  occurs, warn the user
                  if (iimage(jfplanex,jfplaney) .lt. 0) then
                     iimage(jfplanex,jfplaney)=0
                     if (.not. bcorrupteef) then
                        bcorrupteef = .true.
                        write(context,*)'EEF corrupted by overflow in ',
     &                                  'iimage array at photon',
     &                                        jtotaltraced
                        call FCERR(context)
                     endif
                  endif
              endif
           endif

C          == calculation of Encircled Energy Function ===============
C            calculate the position of the photon
           dx = dfplanex - 3500. * sin(angin*DEGREE2RADIAN) 
     &                              * cos(angphi*DEGREE2RADIAN)
           dy = dfplaney - 3500. * sin(angin*DEGREE2RADIAN) 
     &                              * sin(angphi*DEGREE2RADIAN)

C           compute the radial bin it would fall in
           jradialbin = int(dsqrt(dx*dx+dy*dy)/0.05d0)+1

C           accumlate a count of photons that fall in the 1st 120 bins
           if (jradialbin.le.120) j120bincnt = j120bincnt + 1

C           photons outside of the array size pile up at the end
           if(jradialbin.gt.500) jradialbin = 501

C           Total EEF by bin
           jeeftot(jradialbin) = jeeftot(jradialbin) + 1

           if(jphotoncondition.eq.0) then
C              Normal reflection photons
              jeefnrm(jradialbin) = jeefnrm(jradialbin) + 1
           else if(jphotoncondition.eq.960) then
C              Primary reflect only photons
              jeefprm(jradialbin) = jeefprm(jradialbin) + 1
           else if(jphotoncondition.eq.970) then
C              Secondary reflect only photons
              jeefsec(jradialbin) = jeefsec(jradialbin) + 1
           else if(jphotoncondition.eq.980) then
C              NO reflect photons
              jeefdir(jradialbin) = jeefdir(jradialbin) + 1
           else if(jphotoncondition.eq.990) then
C              Abnormal reflection path photons
              jeefbak(jradialbin) = jeefbak(jradialbin) + 1
           endif
C          ==========================================================
C        END ((jphotoncondition.eq.0).or.(jphotoncondition.ge.960))
        endif

C        total the ray traced photons by their result
        jphotonbyresult(jphotoncondition) = 
     &                 jphotonbyresult(jphotoncondition) + 1

C check for whether to stop the photon ray tracing loop
        if (jrunmode .eq. EAMODE) then
C           this is for a Nagoya EA file run so we need at least
C           X photons that landed in the 1st 120 radial bins
           if (j120bincnt.ge.jnea) goto 21
        else if(jrunmode .eq. PSFMODE) then
C           this is for a Nagoya PSF file run so we need at least
C           X photons to land in the 1st 120 radial bins
           if (j120bincnt.ge.jnpsf) goto 21
        else
C           ray trace exactly as many photons as asked for
           if (jtotaltraced.ge.jnph) goto 21
        endif

C        loop thru another photon
        goto 20

C END POINT AFTER ALL PHOTONS ARE RAY TRACED
 21   continue

C        total front area of mirror
        dmirrorfrontarea = (r(120,1)**2 - r(1,4)**2) * PI
C        fractional areas by photon result code
        dfracbyresult(0) = dble(jphotonbyresult(0))
     &                           / dble(jtotaltraced)
        dfracareabyresult(0) = dmirrorfrontarea 
     &                           * dfracbyresult(0)
        do 30 i=900,999
           dfracbyresult(i) = dble(jphotonbyresult(i))
     &                                / dble(jtotaltraced)
           dfracareabyresult(i) = dmirrorfrontarea
     &                                * dfracbyresult(i)
 30        continue

C     are we doing the EEF calc?
C            zero the working arrays
            do 50 i=0,1024
               fradius(i)=0.
               feef(i)=0.
               fpsf(i)=0.
               fdummypixs(i)=0.
               fpixs(i)=0.
               fring(i)=0.
               ferrpsf(i)=0.
 50            continue
C*********************************************************
        if (beefcalc .and. (.not. bcorrupteef)) then
C            set the radius bin size for the rings
C           IF YOU MODIFY THIS, ALSO MODIFY THE HISTORY CARD IN THE MAIN
            jradialbinsize=5
            fradialbinsize=real(jradialbinsize)
C             NOT ALL OF THE ARRAYS IS USED
C            A limit of 200 is used on the radial bins for 
C            non-obvious reasons
            jradialbinlimit=200
C            calculate needed params
C
C            calculate the size of an image pixel in mm
            fxpixelsizemm=(fxymax-fxymin)/float(jxybins)
            fypixelsizemm=(fxymax-fxymin)/float(jxybins)
C            calculate the offset center based on the photon mirror
C            entry point
            jxcenter = jxybins/2
            jycenter = jxybins/2
            jphotonxcenter = jxcenter
     &         +int(3500.*sin(angin*DEGREE2RADIAN)
     &                   *cos(angphi*DEGREE2RADIAN)
     &                   /fxpixelsizemm 
     &              +0.5)
            jphotonycenter = jycenter
     &         +int(3500.*sin(angin*DEGREE2RADIAN)
     &                   *sin(angphi*DEGREE2RADIAN)
     &                   /fypixelsizemm
     &              +0.5)



C
C            Scan the image and count the number of pixels in each
C            radius ring as defined by the binning size
            do 85 i=1,jxybins
               do 80 j=1,jxybins
                  fpixelradius=sqrt(
     &                               real((i-jxcenter)*(i-jxcenter)
     &                                   +(j-jycenter)*(j-jycenter)))
                  if (fpixelradius.eq.0.0) then
                     jradialbin=0
                  else
                     jradialbin = 
     &                   int(fpixelradius/fradialbinsize+1.)
                  endif
                  if (jradialbin.le.jradialbinlimit) then
                     fdummypixs(jradialbin) = 
     &                                fdummypixs(jradialbin) + 1.
                  endif
 80                continue
 85            continue

C--------- ring flux calc.
            do 100 j=1,jxybins
               do 95 i=1,jxybins
                  fpixelradius=sqrt(
     &                           real((i-jxcenter)*(i-jxcenter)
     &                               +(j-jycenter)*(j-jycenter)))
                  foffsetradius = sqrt(
     &                real( (i-jphotonxcenter)*(i-jphotonxcenter)
     &                    + (j-jphotonycenter)*(j-jphotonycenter)))
                  if (foffsetradius.eq.0.) then
                     jradialbin=0
                  else
                     jradialbin = 
     &                      int(foffsetradius/fradialbinsize + 1.)
                  endif
C WARNING The following comments were located in the original code
C- - - - - for GIS image - - - - - - - - - - - - - - - - - - - -
C            if ((jradialbin.le.jradialbinlimit).and.(fpixelradius.le.106)) then
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

                   if (jradialbin.le.jradialbinlimit) then
                      fpixs(jradialbin) = fpixs(jradialbin) + 1.
                      fring(jradialbin) = fring(jradialbin)
     &                                             + iimage(i,j)
                  endif
 95           continue
 100        continue
C--------- eef & psf calc
            fradius(0)=0.0
            feef(0)=fring(0)
            ferreef(0)=sqrt(fring(0))
            fpsf(0)=fring(0)/fpixs(0)
            ferrpsf(0)=sqrt(fring(0))/fpixs(0)
            fpcol(0)=fdummypixs(0)/fpixs(0)
            do 500 i=1,jradialbinlimit
               if (fpixs(i).lt.1.0) then
                  fradius(i)=real(i*jradialbinsize)*fxpixelsizemm
                  fpcol(i)=0.
                  feef(i)=0.
                  ferreef(i)=0.
                  fpsf(i)=0.
                  ferrpsf(i)=0.
               else
                  fradius(i)=real(i*jradialbinsize)*fxpixelsizemm
                  fpcol(i)=fdummypixs(i)/fpixs(i)
                  feef(i)=feef(i-1)+fring(i)*fpcol(i)
                  ferreef(i)=sqrt(ferreef(i-1)**2
     &                            +(fpcol(i)**2)*fring(i))
                  fpsf(i)=fring(i)/fpixs(i)
                  ferrpsf(i)=sqrt(fring(i))/fpixs(i)
               endif
 500               continue
        endif
C*****END OF EEF CALCULATION

C WRITE THE RESULTS TO THE FITS FILE
C
C        write the energy column
        call FTPCLD(jiounit, jenergycol, jrow, 1, 1, denergy,
     &                    status)
        if (status .ne. 0) then
           context = 'Failure writing energy column'
           goto 900
        endif
C        write the total photons traced column
        call FTPCLJ(jiounit, jtracedcol, jrow,1,1, jtotaltraced,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing traced column'
           goto 900
        endif
C        write the focal plane photon count
        j =    jphotonbyresult(0)
     &             + jphotonbyresult(960)
     &             + jphotonbyresult(970)
     &             + jphotonbyresult(980)
     &             + jphotonbyresult(990)
        call FTPCLJ(jiounit, jfocalcol, jrow,1,1, j,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing focal column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal0col, jrow,1,1, 
     &                    jphotonbyresult(0), status)
        if (status .ne.0) then
           context = 'Failure writing total0 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal910col, jrow,1,1, 
     &                    jphotonbyresult(910), status)
        if (status .ne.0) then
           context = 'Failure writing total910 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal911col, jrow,1,1, 
     &                    jphotonbyresult(911), status)
        if (status .ne.0) then
           context = 'Failure writing total911 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal915col, jrow,1,1, 
     &                    jphotonbyresult(915), status)
        if (status .ne.0) then
           context = 'Failure writing total915 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal916col, jrow,1,1, 
     &                    jphotonbyresult(916), status)
        if (status .ne.0) then
           context = 'Failure writing total916 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal920col, jrow,1,1, 
     &                    jphotonbyresult(920), status)
        if (status .ne.0) then
           context = 'Failure writing total920 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal921col, jrow,1,1, 
     &                    jphotonbyresult(921), status)
        if (status .ne.0) then
           context = 'Failure writing total921 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal923col, jrow,1,1, 
     &                    jphotonbyresult(923), status)
        if (status .ne.0) then
           context = 'Failure writing total923 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal924col, jrow,1,1, 
     &                    jphotonbyresult(924), status)
        if (status .ne.0) then
           context = 'Failure writing total924 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal925col, jrow,1,1, 
     &                    jphotonbyresult(925), status)
        if (status .ne.0) then
           context = 'Failure writing total925 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal930col, jrow,1,1, 
     &                    jphotonbyresult(930), status)
        if (status .ne.0) then
           context = 'Failure writing total930 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal935col, jrow,1,1, 
     &                    jphotonbyresult(935), status)
        if (status .ne.0) then
           context = 'Failure writing total935 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal940col, jrow,1,1, 
     &                    jphotonbyresult(940), status)
        if (status .ne.0) then
           context = 'Failure writing total940 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal945col, jrow,1,1, 
     &                    jphotonbyresult(945), status)
        if (status .ne.0) then
           context = 'Failure writing total945 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal950col, jrow,1,1, 
     &                    jphotonbyresult(950), status)
        if (status .ne.0) then
           context = 'Failure writing total950 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal955col, jrow,1,1, 
     &                    jphotonbyresult(955), status)
        if (status .ne.0) then
           context = 'Failure writing total955 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal960col, jrow,1,1, 
     &                    jphotonbyresult(960), status)
        if (status .ne.0) then
           context = 'Failure writing total960 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal970col, jrow,1,1, 
     &                    jphotonbyresult(970), status)
        if (status .ne.0) then
           context = 'Failure writing total970 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal980col, jrow,1,1, 
     &                    jphotonbyresult(980), status)
        if (status .ne.0) then
           context = 'Failure writing total980 column'
           goto 900
        endif
        call FTPCLJ(jiounit, jtotal990col, jrow,1,1, 
     &                    jphotonbyresult(990), status)
        if (status .ne.0) then
           context = 'Failure writing total990 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff0col, jrow,1,1, 
     &                    dfracbyresult(0), status)
        if (status .ne.0) then
           context = 'Failure writing eff0 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff910col, jrow,1,1, 
     &                    dfracbyresult(910), status)
        if (status .ne.0) then
           context = 'Failure writing eff910 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff911col, jrow,1,1, 
     &                    dfracbyresult(911), status)
        if (status .ne.0) then
           context = 'Failure writing eff911 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff915col, jrow,1,1, 
     &                    dfracbyresult(915), status)
        if (status .ne.0) then
           context = 'Failure writing eff915 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff916col, jrow,1,1, 
     &                    dfracbyresult(916), status)
        if (status .ne.0) then
           context = 'Failure writing eff916 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff920col, jrow,1,1, 
     &                    dfracbyresult(920), status)
        if (status .ne.0) then
           context = 'Failure writing eff920 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff921col, jrow,1,1, 
     &                    dfracbyresult(921), status)
        if (status .ne.0) then
           context = 'Failure writing eff921 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff923col, jrow,1,1, 
     &                    dfracbyresult(923), status)
        if (status .ne.0) then
           context = 'Failure writing eff923 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff924col, jrow,1,1, 
     &                    dfracbyresult(924), status)
        if (status .ne.0) then
           context = 'Failure writing eff924 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff925col, jrow,1,1, 
     &                    dfracbyresult(925), status)
        if (status .ne.0) then
           context = 'Failure writing eff925 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff930col, jrow,1,1, 
     &                    dfracbyresult(930), status)
        if (status .ne.0) then
           context = 'Failure writing eff930 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff935col, jrow,1,1, 
     &                    dfracbyresult(935), status)
        if (status .ne.0) then
           context = 'Failure writing eff935 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff940col, jrow,1,1, 
     &                    dfracbyresult(940), status)
        if (status .ne.0) then
           context = 'Failure writing eff940 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff945col, jrow,1,1, 
     &                    dfracbyresult(945), status)
        if (status .ne.0) then
           context = 'Failure writing eff945 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff950col, jrow,1,1, 
     &                    dfracbyresult(950), status)
        if (status .ne.0) then
           context = 'Failure writing eff950 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff955col, jrow,1,1, 
     &                    dfracbyresult(955), status)
        if (status .ne.0) then
           context = 'Failure writing eff955 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff960col, jrow,1,1, 
     &                    dfracbyresult(960), status)
        if (status .ne.0) then
           context = 'Failure writing eff960 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff970col, jrow,1,1, 
     &                    dfracbyresult(970), status)
        if (status .ne.0) then
           context = 'Failure writing eff970 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff980col, jrow,1,1, 
     &                    dfracbyresult(980), status)
        if (status .ne.0) then
           context = 'Failure writing eff980 column'
           goto 900
        endif
        call FTPCLD(jiounit, jeff990col, jrow,1,1, 
     &                    dfracbyresult(990), status)
        if (status .ne.0) then
           context = 'Failure writing eff990 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea0col, jrow,1,1, 
     &                    dfracareabyresult(0), status)
        if (status .ne.0) then
           context = 'Failure writing area0 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea910col, jrow,1,1, 
     &                    dfracareabyresult(910), status)
        if (status .ne.0) then
           context = 'Failure writing area910 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea911col, jrow,1,1, 
     &                    dfracareabyresult(911), status)
        if (status .ne.0) then
           context = 'Failure writing area911 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea915col, jrow,1,1, 
     &                    dfracareabyresult(915), status)
        if (status .ne.0) then
           context = 'Failure writing area915 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea916col, jrow,1,1, 
     &                    dfracareabyresult(916), status)
        if (status .ne.0) then
           context = 'Failure writing area916 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea920col, jrow,1,1, 
     &                    dfracareabyresult(920), status)
        if (status .ne.0) then
           context = 'Failure writing area920 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea921col, jrow,1,1, 
     &                    dfracareabyresult(921), status)
        if (status .ne.0) then
           context = 'Failure writing area921 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea923col, jrow,1,1, 
     &                    dfracareabyresult(923), status)
        if (status .ne.0) then
           context = 'Failure writing area923 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea924col, jrow,1,1, 
     &                    dfracareabyresult(924), status)
        if (status .ne.0) then
           context = 'Failure writing area924 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea925col, jrow,1,1, 
     &                    dfracareabyresult(925), status)
        if (status .ne.0) then
           context = 'Failure writing area925 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea930col, jrow,1,1, 
     &                    dfracareabyresult(930), status)
        if (status .ne.0) then
           context = 'Failure writing area930 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea935col, jrow,1,1, 
     &                    dfracareabyresult(935), status)
        if (status .ne.0) then
           context = 'Failure writing area935 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea940col, jrow,1,1, 
     &                    dfracareabyresult(940), status)
        if (status .ne.0) then
           context = 'Failure writing area940 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea945col, jrow,1,1, 
     &                    dfracareabyresult(945), status)
        if (status .ne.0) then
           context = 'Failure writing area945 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea950col, jrow,1,1, 
     &                    dfracareabyresult(950), status)
        if (status .ne.0) then
           context = 'Failure writing area950 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea955col, jrow,1,1, 
     &                    dfracareabyresult(955), status)
        if (status .ne.0) then
           context = 'Failure writing area955 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea960col, jrow,1,1, 
     &                    dfracareabyresult(960), status)
        if (status .ne.0) then
           context = 'Failure writing area960 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea970col, jrow,1,1, 
     &                    dfracareabyresult(970), status)
        if (status .ne.0) then
           context = 'Failure writing area970 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea980col, jrow,1,1, 
     &                    dfracareabyresult(980), status)
        if (status .ne.0) then
           context = 'Failure writing area980 column'
           goto 900
        endif
        call FTPCLD(jiounit, jarea990col, jrow,1,1, 
     &                    dfracareabyresult(990), status)
        if (status .ne.0) then
           context = 'Failure writing area990 column'
           goto 900
        endif
C        write the EEFNRM array
        call FTPCLJ(jiounit, jeefnrmcol, jrow,1, 501, jeefnrm,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefnrm column'
           goto 900
        endif
C        write the EEFPRM array
        call FTPCLJ(jiounit, jeefprmcol, jrow,1, 501, jeefprm,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefprm column'
           goto 900
        endif
C        write the EEFSEC array
        call FTPCLJ(jiounit, jeefseccol, jrow,1, 501, jeefsec,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefsec column'
           goto 900
        endif
C        write the EEFDIR array
        call FTPCLJ(jiounit, jeefdircol, jrow,1, 501, jeefdir,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefdir column'
           goto 900
        endif
C        write the EEFBAK array
        call FTPCLJ(jiounit, jeefbakcol, jrow,1, 501, jeefbak,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefbak column'
           goto 900
        endif
C        write the EEFTOT array
        call FTPCLJ(jiounit, jeeftotcol, jrow,1, 501, jeeftot,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eeftot column'
           goto 900
        endif
C        write the Cumulative EEFNRM array
        do 700 i=2,501
           jeefnrm(i) = jeefnrm(i) + jeefnrm(i-1)
 700        continue
        call FTPCLJ(jiounit, jceefnrmcol, jrow,1, 501, jeefnrm,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing ceefnrm column'
           goto 900
        endif
C        write the Cumulative EEFPRM array
        do 710 i=2,501
           jeefprm(i) = jeefprm(i) + jeefprm(i-1)
 710        continue
        call FTPCLJ(jiounit, jceefprmcol, jrow,1, 501, jeefprm,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing ceefprm column'
           goto 900
        endif
C        write the Cumulative EEFSEC array
        do 720 i=2,501
           jeefsec(i) = jeefsec(i) + jeefsec(i-1)
 720        continue
        call FTPCLJ(jiounit, jceefseccol, jrow,1, 501, jeefsec,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing ceefsec column'
           goto 900
        endif
C        write the Cumulative EEFDIR array
        do 730 i=2,501
           jeefdir(i) = jeefdir(i) + jeefdir(i-1)
 730        continue
        call FTPCLJ(jiounit, jceefdircol, jrow,1, 501, jeefdir,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing ceefdir column'
           goto 900
        endif
C        write the Cumulative EEFBAK array
        do 740 i=2,501
           jeefbak(i) = jeefbak(i) + jeefbak(i-1)
 740        continue
        call FTPCLJ(jiounit, jceefbakcol, jrow,1, 501, jeefbak,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing ceefbak column'
           goto 900
        endif
C        write the Cumulative EEFTOT array
        do 750 i=2,501
           jeeftot(i) = jeeftot(i) + jeeftot(i-1)
 750        continue
        call FTPCLJ(jiounit, jceeftotcol, jrow,1, 501, jeeftot,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing ceeftot column'
           goto 900
        endif

C        write the EEF radius array
        call FTPCLE(jiounit, jeefradcol, jrow,1, 201, fradius,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefrad column'
           goto 900
        endif

C fring(i)
        call FTPCLE(jiounit, jeefringcol, jrow,1, 201, fring,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefring column'
           goto 900
        endif

C fpixs(i)
        call FTPCLE(jiounit, jeefpixscol, jrow,1, 201, fpixs,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefpixs column'
           goto 900
        endif

C fpcol(i)
        call FTPCLE(jiounit, jeefpcolcol, jrow,1, 201, fpcol,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefpcol column'
           goto 900
        endif

C feef(i)
        call FTPCLE(jiounit, jeefeefcol, jrow,1, 201, feef,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefeef column'
           goto 900
        endif

C ferreef(i)
        call FTPCLE(jiounit, jeeferrcol, jrow,1, 201, ferreef,
     &                    status)
        if (status .ne.0) then
           write(context,*) 'Failure writing eeferr column ',status
           goto 900
        endif

C fpsf(i)
        call FTPCLE(jiounit, jeefpsfcol, jrow,1, 201, fpsf,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefpsf column'
           goto 900
        endif

C errpsf(i)
        call FTPCLE(jiounit, jeefpsfercol, jrow,1, 201, ferrpsf,
     &                    status)
        if (status .ne.0) then
           context = 'Failure writing eefpsfer column'
           goto 900
        endif
C Write jmirrorhits array to column
        call FTPCLJ(jiounit, jhitspscol, jrow, 1, 17*17, 
     &                    jmirrorhits, status)
        if (status .ne.0) then
           context = 'Failure writing hitsps column'
           goto 900
        endif


C Normal return
        return
C FITSIO error return
 900        continue
        call FCERR(context)
        return

        end
