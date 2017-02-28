c This file includes all mission-specific routines 
c The routines are...
c     XSL_FAINT                 ASCA SIS only
c     XSL_SISPI                 ASCA SIS only
c     XSL_FAST                  ASCA SIS only
c     XSL_GISCLEAN              ASCA GIS only
c     XSL_SISCLEAN              ASCA SIS only
c     XSL_SET_XYCENTER          ASCA and ROSAT only
c     XSL_MAKEO                 ROSAT reference
c     XSL_DATAMODE_SET          ASCA reference
c     XSL_GRPSAV                ASCA and XMM reference
c     XSL_CHOOSE                ASCA reference
c     XSL_CHKPHASIZE            ASCA reference
c     XSL_MISS_SETUP            ASCA SIS FAST and GIS PH only
c     XSL_PRE_EXTR              ASCA SIS FAST only
c     XSL_POST_EXTR             ASCA SIS and ROSAT pre-RDF 3 only
c     XSL_DET_LIST              XTE PCA only
c     XSL_CHIP_SEL              Chandra and XMM only
c     XSL_GTI_HDU               Chandra and XMM only
c     XSL_COR_DSS               Chandra and XMM only
c     DSSCAT                    Chandra and XMM only
c     XSL_XMM_SPEC              XMM only
c     XSL_5X5TO3X3              Suzaku only

c ---------------------------------------------
      subroutine XSL_FAINT()
c ---------------------------------------------
c
c Do the faint-to-bright conversion on all the files.
c (Should be changed to interrogate files about whether
c  the conversion is required.)
c
c Called by XSELECT main
c An ASCA  -specific task
c
c     Alan Smale 1992 Nov
c
c Merged with XSL_FAINTDFE which used to be in xsel_ftools.f:
c
c "This runs FAINTDFE on the input data files, and then merges the DFE
c files together, and places them in dfefil
c J. Ingham 5/94"
c
c and modified to cope with new faintdfe and faint in FTOOLS v3.5.
c The addition of the zerodef keyword makes it tricky to run them
c separately.  - KM, 1996 Feb.

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line:
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(255) str1, str2, str3, str4
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2, len3, len4, len5, len6, len7, len8, len9
c ---------------------------------------------
      real sis01echo, sis02echo, sis03echo
      real sis11echo, sis12echo, sis13echo
      integer i, zero
      real    binsec
      character(1) zerodef
      character(24) sbinsec
      character(255) dstfil
      logical SISPI, DODFE
      integer LENACT

      status = 0
      IF(.NOT.READ) THEN
         call XWRITE('No data read in yet',5)
         return
      ELSEIF(instru(1:3).ne.'SIS') THEN
         call XWRITE('FAINT can only be used for SIS data.',5)
         return
      ELSE IF(datamode.ne.'FAINT') THEN
         call XWRITE('FAINT can only be used for '//
     &        'SIS FAINT mode data',5)
         return
      ENDIF
      call xsl_uclgst('bright',str4,status)
      IF ( status .NE. 0 ) RETURN
      len2 = LENACT(str4)
      if(str4(1:1).ne.'b'.and.str4(1:1).ne.'B') THEN
         call XWRITE('Enter either "bright" or "bright2"',5)
         return
      endif
      IF(str4(len2:len2).eq.'2') THEN
         str4 = 'bright=no'
         datamode = 'BRIGHT2'
      ELSE
         str4 = 'bright=yes'
         datamode = 'BRIGHT'
      ENDIF
c     Maxgrade and echo should both be chooseable in BRIGHT and BRIGHT2.
      status = 0
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgst('maxgrade',maxgrade,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgsr('echo',sisecho,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgst('split',split,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgsr('sis01echo',sis01echo,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgsr('sis02echo',sis02echo,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgsr('sis03echo',sis03echo,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgsr('sis11echo',sis11echo,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgsr('sis12echo',sis12echo,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgsr('sis13echo',sis13echo,status)
      IF ( status .NE. 0 ) RETURN

      call xsl_uclgst('dfefile',usrdfe,status)
      IF ( status .NE. 0 ) RETURN
      DODFE = .false.
      if (usrdfe.eq.'NONE' .or. usrdfe.eq.'none') then
         usrdfe = '-'
      elseif(usrdfe .eq. 'MAKE' .or. usrdfe .eq. 'make' ) then

C First look in the REFDATA directory, for the file
c   faintdfe.qdp
c is kept.

         if (refdir.eq.'NONE') then
            call XWRITE('No refdata directory found.',5)
            status = -10
            return
         endif

         dstfil = refdir(:LENACT(refdir))//'faintdfe.tbl'

c Now get the binsec:
         call xsl_uclgsr('binsec',binsec,status)
         IF ( status .NE. 0 ) RETURN
         write(sbinsec,'(f16.4)',err=998) binsec

c Now get zerodef, if bright2
         if( datamode .eq. 'BRIGHT2' ) then
           status = 0
           call xsl_uclgsi( 'zerodef', zero, status )
           IF ( status .NE. 0 ) RETURN
           if( zero .ge. 0 .and. zero .le. 2 ) then
              write( zerodef, '(i1)' ) zero
           else
              call XWRITE('Error converting zerodef',5)
              status = -1022
              return
           end if
         else
           zerodef = '2'
         end if

         DODFE = .true.

      endif
      len6 = LENACT(usrdfe)

      call xsl_uclgsb('sispi',SISPI,status)
      IF ( status .NE. 0 ) RETURN
         
c Delete previous command file and work2 files

      call XSL_RMWORK(work2,nfiles)

c Open new command file

      call XSL_OPCF(cmdfil,ilun)

c If we've already performed a SELECTion, we'll be using
c the WORK1 filenames as input. Otherwise it'll be the original
c filenames. Either way, the WORK2 files are the output files, which
c are then copied (back) to the WORK1 files and the .WORK. logical
c reset for subsequent observations.

c Construct instruction strings controlling NFILES applications
c of FAINT.

      len3 = LENACT( split )
c      write(str3,57) sisecho,sis0echo,sis1echo
c 57   format(' echo = ',f10.6,' sis0echo = ',f10.6,
c     &                             ' sis1echo = ',f10.6)
c     Echo fraction now given as a set of 3 coefficients
c     with this, str3 should be ~160 char long, still leaves
c     ~200 char per name for the 3 workfiles
      write(str3,57) sisecho, sis01echo, sis02echo, sis03echo,
     &                        sis11echo, sis12echo, sis13echo
 57   format( ' echo=',f10.6, ' sis01echo=',f10.6, 1p,
     &        ' sis02echo=', e10.3, ' sis03echo=', e10.3, 0p,
     &        ' sis11echo=', f10.6, 1p, ' sis12echo=',e10.3,
     &        ' sis13echo=', e10.3 ) 
      len4 = LENACT(str3)
      len5 = LENACT(str4)
      DO i = 1, nfiles

         IF( WORK )then
            len1 = LENACT( work1(i) )

            if( DODFE ) then
               len7 = LENACT(dstfil)
               len8 = LENACT(sbinsec)
               len9 = LENACT( gtiwk2(i) )

               comlin='Running FAINTDFE on '//work1(i)(1:len1)
               call XSL_MESSAGE(ilun,comlin)

               comlin='faintdfe '//
     +           'infile='//work1(i)(1:len1)//' '//
     +           'outfile='//gtiwk2(i)(1:len9)//' '//
     +           'split='//split(1:len3)//' '//
     +           'binsec='//sbinsec(:len8)//' '//
     +           'tblfile='//dstfil(:len7)//' '//
     +           'zerodef='//zerodef//' '//
     +           'extname=EVENTS '//
     +           'mincounts=10 grades="0 2 3 4" '//
     +           'accuracy=1.e-4 specdump=NONE'
               call XSL_WRTCF(ilun,comlin,1)

               len2 = LENACT( work2(i) )

               comlin='faint '//
     +           'infile='//work1(i)(1:len1)//' '//
     +           'outfile='//work2(i)(1:len2)//' '//
     +           'split='//split(1:len3)//' '//
     +           str3(:len4)//' '//
     +           'maxgrade='//maxgrade(1:LENACT(maxgrade))//' '//
     +           'columns="PHAS" '//
     +           'phacol="PHA" '//
     +           'gradecol="GRADE" '// 
     +           'abovecol="NONE" '//
     +           'timecol=TIME idcol=CCDID '//
     +           'dfefile='//gtiwk2(i)(1:len9)//' '//
     +           'version=5.0 '//
     +           'qfancy="no" '//
     +           str4(:len5)//' '//
     +           'history=yes '//
     +           'copyall=yes '

            else

               len2 = LENACT( work2(i) )

               comlin='faint '//
     +           'infile='//work1(i)(1:len1)//' '//
     +           'outfile='//work2(i)(1:len2)//' '//
     +           'split='//split(1:len3)//' '//
     +           str3(:len4)//' '//
     +           'maxgrade='//maxgrade(1:LENACT(maxgrade))//' '//
     +           'columns="PHAS" '//
     +           'phacol="PHA" '//
     +           'gradecol="GRADE" '// 
     +           'abovecol="NONE" '//
     +           'timecol=TIME idcol=CCDID '//
     +           'dfefile='//usrdfe(:len6)//' '//
     +           'version=5.0 '//
     +           'qfancy="no" '//
     +           str4(:len5)//' '//
     +           'history=yes '//
     +           'copyall=yes '
            end if
            str2 = 'Running FAINT on '//work1(i)(1:len1)
            call XSL_MESSAGE(ilun,str2)
         ELSE
            len1 = LENACT( filenm(i) )

            if( DODFE ) then
               len7 = LENACT(dstfil)
               len8 = LENACT(sbinsec)
               len9 = LENACT( gtiwk2(i) )
                  
               comlin = 'Running FAINTDFE on '//filenm(i)(1:len1)
               call XSL_MESSAGE(ilun,comlin)

               comlin='faintdfe '// 'infile='//filenm(i)(1:len1)//' '//
     $              'outfile='//gtiwk2(i)(1:len9)//' '// 'split='
     $              //split(1:len3)//' '// 'binsec='//sbinsec(:len8)/
     $              /' '// 'tblfile='//dstfil(:len7)//' '// 'zerodef='
     $              //zerodef//' '// 'extname=EVENTS '//
     $              'mincounts=10 grades="0 2 3 4" '//
     $              'accuracy=1.e-4 specdump=NONE'
               call XSL_WRTCF(ilun,comlin,1)

               len2 = LENACT( work2(i) )
                  
               comlin='faint '//
     +           'infile='//filenm(i)(1:len1)//' '//
     +           'outfile='//work2(i)(1:len2)//' '//
     +           'split='//split(1:len3)//' '//
     +           str3(:len4)//' '//
     +           'maxgrade='//maxgrade(1:LENACT(maxgrade))//' '//
     +           'columns="PHAS" '//
     +           'phacol="PHA" '//
     +           'gradecol="GRADE" '// 
     +           'abovecol="NONE" '//
     +           'timecol=TIME idcol=CCDID '//
     +           'dfefile='//gtiwk2(i)(1:len9)//' '//
     +           'version=5.0 '//
     +           'qfancy="no" '//
     +           str4(:len5)//' '//
     +           'copyall=yes '
            else
              len2 = LENACT( work2(i) )
                  
               comlin='faint '//
     +           'infile='//filenm(i)(1:len1)//' '//
     +           'outfile='//work2(i)(1:len2)//' '//
     +           'split='//split(1:len3)//' '//
     +           str3(:len4)//' '//
     +           'maxgrade='//maxgrade(1:LENACT(maxgrade))//' '//
     +           'columns="PHAS" '//
     +           'phacol="PHA" '//
     +           'gradecol="GRADE" '// 
     +           'abovecol="NONE" '//
     +           'timecol=TIME idcol=CCDID '//
     +           'dfefile='//usrdfe(:len6)//' '//
     +           'version=5.0 '//
     +           'qfancy="no" '//
     +           str4(:len5)//' '//
     +           'copyall=yes '
            end if
            str2 = 'Running FAINT on '//filenm(i)(1:len1)
            call XSL_MESSAGE(ilun,str2)
         ENDIF

         call XSL_WRTCF(ilun,comlin,1)

c We've now sent the command to do the conversion.
c Now, all the selected files will be in WORK2, which should
c become the CURRENT files. Thus, copy the WORK2 files to WORK1


      END DO

      call XSL_CLCF(ilun)
   
c Run the command file
      call XSL_RUNCF(cmdfil,ECHO,status)
c Check for errors
      IF(status.eq.0) THEN

         if( DODFE ) then
            call XSL_RMFILE(dfefil)
            call XSL_CAT(gtiwk2,nfiles,dfefil,MAXFIL)
            call XSL_NUMSORT(dfefil,1,.TRUE.,status)
         endif

c Set the WORK logical
         WORK = .TRUE.
c And set the datamode to its new value:
         call XSL_SET(6)

c Reset the MERGED logical -- individual files have (probably) changed.
         MERGED = .FALSE.

c Open new command file

         call XSL_OPCF(cmdfil,ilun)
         do i=1,nfiles
            len1 = LENACT(work1(i))
            call XSL_RENAME(work2(i),work1(i),2,str1,len1,ierr)
            write(ilun,53) str1(1:len1)
         enddo
         call XSL_CLCF(ilun)
   
c Run the command file
         call XSL_RUNCF(cmdfil,ECHO,status)
c Now, fill the PI column if requested...
         IF ( SISPI ) THEN
            call XSL_SISPI()
            if ( status.ne.0 ) THEN
               call XWRITE('Warning: SIS PI columns not '//
     &              'successfully filled',5)
            endif               
         ELSE
c Finally reset the PHAMIN and PHAMAX (this is done in XSL_SISPI in
c the other branch:
            call XSL_CHKPHASIZE(1)
         ENDIF
c And set FAINT:
         FAINT = .TRUE.
      ELSE
c Set the datamode back to FAINT if we failed...
         datamode = 'FAINT'
      ENDIF

 53   format(a)
      return

c Come here upon error in binsec conversion
 998  call XWRITE('Error converting binsec',5)
      status = -22

      end

c ---------------------------------------------
      subroutine XSL_SISPI()
c ---------------------------------------------
c
c Fill the PI columns on all files
c
c Called by XSELECT main
c An ASCA  -specific task
c
c
c Modified to match SISPI v1.1 - KM, 1996 Feb

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line:
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(255) str1, str2
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2
c ---------------------------------------------
      character(255) pha2pi
      integer LENACT
      integer i

      status = 0
      IF(.NOT.READ) THEN
         call XWRITE('No data read in yet',5)
         return
      ELSEIF(instru(1:3).ne.'SIS') THEN
         call XWRITE('SISPI can only be used for SIS data.',5)
         return
      ELSE IF(datamode.eq.'FAST') THEN
         call XWRITE('SISPI can not be used for '//
     &        'SIS FAST mode data',5)
         return
      ENDIF
      call xsl_uclgst('pha2pi',pha2pi,status)
      IF ( status .NE. 0 ) RETURN
      if ( pha2pi.eq.'REF' .or. pha2pi .eq. 'ref') then
         if (refdir.eq.'NONE') then
            call XWRITE('No refdata directory found.',5)
            status = -10
            return
         endif

         pha2pi = refdir(:LENACT(refdir))//'sisph2pi.fits'

c     Allow for the use of 'default' value in sispi v1.1, but interpret
      else if( pha2pi .eq. 'DEFAULT' .or. pha2pi .eq. 'default' ) then
         call xsl_uclgst( 'defSISfile', pha2pi, status )
         IF ( status .NE. 0 ) RETURN
      endif

c     Allow for passing of shorthand to SISPI to interpret
      if( pha2pi .ne. 'AUTO' .and. pha2pi .ne. 'auto'
     &    .and. pha2pi .ne. 'CALDB' .and. pha2pi .ne. 'caldb'
     &    .and. pha2pi .ne. 'FTOOLS' .and. pha2pi .ne. 'ftools' ) then
         call XSL_EXIST(pha2pi,status)
         if ( status.ne.0) then
            str1 = 'Could not find calibration file '//
     &              pha2pi(:LENACT(pha2pi))
            call XWRITE(str1,5)
            return
         endif
      endif
      len1 = LENACT(pha2pi)
 
c Delete previous command file and work2 files

      call XSL_RMWORK(work2,nfiles)
      

c Open new command file

      call XSL_OPCF(cmdfil,ilun)

c If we've already performed a SELECTion, we'll be using
c the WORK1 filenames as input. Otherwise it'll be the original
c filenames. This is done in place for the work files, but not
c for the originals.

c Construct instruction strings controlling NFILES applications
c of SISPI.

      DO i = 1, nfiles

         IF( WORK ) THEN
            len2 = LENACT( work1(i) )

            str2 = 'Running SISPI on '//work1(i)(1:len2)
            call XSL_MESSAGE(ilun,str2)

c           Added offset=0.0 for sispi v1.1
            comlin='sispi '//
     +           'datafile='//work1(i)(1:len2)//' '//
     +           'calfile='//pha2pi(1:len1)//' '//
     +           'rawxcol=RAWX rawycol=RAWY gradecol=GRADE '//
     +           'PHACOL=PHA PICOL=PI idcol=CCDID '//
     &           'gainnom=-99.9 offset=0.0 launch=no '//
     &           'verbose=no '

            call XSL_WRTCF(ilun,comlin,1)

         ELSE

            len2 = LENACT( work1(i) )

            str2 = 'Running SISPI on '//work1(i)(1:len2)
            call XSL_MESSAGE(ilun,str2)

            call XSL_COPY2(filenm(i),work1(i),comlin)
            write(ilun,'(A)') comlin
c           Added offset=0.0 for sispi v1.1
            comlin='sispi '//
     +           'datafile='//work1(i)(1:len2)//' '//
     +           'calfile='//pha2pi(1:len1)//' '//
     +           'rawxcol=RAWX rawycol=RAWY gradecol=GRADE '//
     +           'PHACOL=PHA PICOL=PI idcol=CCDID '//
     &           'gainnom=-99.9 offset=0.0 launch=no '//
     &           'verbose=no '

            call XSL_WRTCF(ilun,comlin,1)

         ENDIF

c We've now sent the command to do the conversion.

      END DO

      call XSL_CLCF(ilun)
   
c Run the command file
      call XSL_RUNCF(cmdfil,ECHO,status)
c Check for errors
      IF(status.eq.0) THEN
c Set the WORK logical
         WORK = .TRUE.
c Reset the MERGED logical -- individual files have (probably) changed.
         MERGED = .FALSE.

C if the ENERGY column is not PI, set it to PI...
         IF ( keypha .ne. 'PI' ) THEN
            call XWRITE('PI column filled, setting PHANAME to PI',
     &           5)
            keypha = 'PI'
         ENDIF

c Finally reset the PHAMIN and PHAMAX:
         call XSL_CHKPHASIZE(1)

      ENDIF

      return
      end

c
c
c ---------------------------------------------
      subroutine XSL_FAST()
c ---------------------------------------------
c This task runs the fasttime ftool to convert FAST mode date
c to useable form.  It requires the source position, which can
c be entered in GIS or SIS pixels, and you can enter an optional
c on or off source filter.
c J. Ingham 1/94 

      include 'xsel.inc'
      include 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line:
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(255) str1, str2
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2,len3
c ---------------------------------------------      
      character(8) strtmp
      integer LENACT,imgctr,i,j
c transl is the matrix to translate DETX coordinates from Any inst. to the
c S0 or S1, in SIS pixels.  
c The first index is the original coordinate = {S0,S1,G2,G3}.
c The second is the target, and is {S0,S1}. 
c center(i) is the center ( in pixels ) of the {sis0,sis1,gis2,gis3} 
c Then scale(i) is the scale factor to {sis,gis}pixels -> sis pixels,
c so that, for example,
c
c source_x_sis0 = scale(3)*(source_x_gis1-center(3)) + 
C                                     trans(3,1)+center(1)
c
c Actually, what we want is the distance to the Physical edge of the chip
c so from:
c          imgctr = scale(i)*(iorig-center(i)) + trans(j,i)
c We need 
c          sourcx = imgctr - sign(imgctr)(422+2.4)
c Where 422 is the x-dimension of the chip, and 2.4 is half the 
c interchip gap.

      real scale(4),transl(4,2),center(4)
      integer iorig,itarg,sourcx
c This is for translating the DETY value
c      data ((transl(i,j),i=1,4),j=1,2) /0,-35.926,-31.851, -37.407,
c     &     35.926,0.0,4.07,-1.48/
c This is for translating the DETX value
      data ((transl(i,j),i=1,4),j=1,2) /0,2.22,28.15, -27.78,
     &     -2.22,0.0,25.93,-30.00/
      data (scale(i),i=1,4) / 1.0,1.0,9.26,9.26 /
      data (center(i),i=1,4) / 640.5,640.5,128.5,128.5 /
      status = 0
      IF(.NOT.READ) THEN
         call XWRITE('No data has been read in yet',5)
         return
      ENDIF

c Set the instrument if none is set:
      IF(instru.eq.'NONE') THEN      
         call XSL_SET(1)
      ENDIF
      IF(status.ne.0) THEN
         call XWRITE('Error setting instrument.',5)
         return
      ENDIF
      IF(instru.eq.'SIS0') THEN
         itarg = 1
      ELSEIF(instru.eq.'SIS1') THEN
         itarg = 2
      ELSE
         call XWRITE('Fast should only be used for ASCA SIS',5)
         return
      ENDIF


c Next get the image center:
      call XWRITE('Enter the DETX coordinate of the source',5)
      call XWRITE('and specify which instrument was used'//
     &     ' in this determination',5)
      call XWRITE('For the SIS, use an UNBINNED image '//
     &     'DETX = [1,1280]',5)
      call XWRITE('For the GIS, use DETX = [1,256]',5)
      call xsl_uclgsi('x_image_center',imgctr,status)
      IF ( status .NE. 0 ) RETURN
 77   call xsl_uclgst('from_instrument',strtmp,status)
      IF ( status .NE. 0 ) RETURN

      IF(strtmp(1:1).eq.'G'.or.strtmp(1:1).eq.'g') THEN
c convert the units:
         len1 = LENACT(strtmp)
         if(strtmp(len1:len1).eq.'2') then
            iorig = 3
         else if(strtmp(len1:len1).eq.'3') then
            iorig = 4
         else
            call XWRITE
     &           ('Please enter the instrument name and number',5)
            call XWRITE('or type ''q'' to quit',5)
            goto 77
         endif

      ELSE IF(strtmp(1:1).eq.'S'.or.strtmp(1:1).eq.'s') THEN         
c This is SIS, but is it the same detector?
c Now compare with strtmp, and 

         len1 = LENACT(strtmp)
         IF(strtmp(len1:len1).eq.'0') THEN
            iorig = 1
         ELSE IF(strtmp(len1:len1).eq.'1') THEN
            iorig = 2
         ELSE
            call XWRITE
     &           ('Please enter the instrument name and number',5)
            call XWRITE('or type ''q'' to quit',5)
            goto 77
         ENDIF
      ELSEIF(strtmp(1:1).eq.'Q'.or.strtmp(1:1).eq.'q') THEN
         return
      ELSE
         call XWRITE('Unrecognized instrument:',5)
         call XWRITE
     &        ('Please enter the instrument name and number',5)
         call XWRITE('or type ''q'' to quit',5)
         goto 77
      ENDIF
c So this is the conversion:

      imgctr = INT(scale(iorig)*(float(imgctr)-center(iorig))
     &     + transl(iorig,itarg))

c This gets the image center, now we want the distance to the
c readout edge: There are is a 4.8 pixel gap, then 422 pixels to
c the physical edge of the each chip, so
      IF(imgctr.gt.0) THEN
         sourcx = 424 - imgctr
      ELSE
         sourcx = 424 + imgctr
      ENDIF

c There are only 422 pixels in each chip, so:

      IF(sourcx.gt.422.or.sourcx.lt.0) THEN
         write(str1,'(a,i5)') 'Error in image offset, got: ', sourcx
         call XWRITE(str1,5)
         status = -10
         return
      ELSE
         write(str1,'(a,i5)') 'Got offset from chip edge: ',sourcx
         call XWRITE(str1,5)
      ENDIF

      
c Now that we have the source position, build up the command line for 
c the fasttime task.
c First give the user a chance to save the workspace or binin files:
      call XSL_SAVE_WS_OR_EVNIN()
c Delete previous command file and work2 files

      call XSL_RMWORK(work2,nfiles)
      call XSL_RMFILE(cmdfil)

c Open new command file

      call XSL_OPCF(cmdfil,ilun)

c If we've already performed a SELECTion,... we'll be using
c the WORK1 filenames as input. Otherwise it'll be the original
c filenames. Either way, the WORK2 files are the output files, which
c are then copied (back) to the WORK1 files and the .WORK. logical
c reset for subsequent observations.

c Construct instruction strings controlling NFILES applications
c of FAINT.

      write(str1,87) sourcx
 87   format('sourcey=',i4)
      len1 = LENACT(evnout)
      len2 = LENACT(work2(1))
      len3 = LENACT(str1)
      IF(BINOUT) THEN
         comlin='fasttime '//
     +        'infile='//evnout(:len1)//' '//
     +        'outfile='//work2(1)(:len2)//' '//
     +        str1(:len3)//' '//
     +        'timecol=TIME ycolumn=RAWY '//
     +        'sensecase=yes'
         str2 = 'Now processing '//evnout(:len1)
         call XSL_MESSAGE(ilun,str2)
         call XSL_WRTCF(ilun,comlin,1)
c The output of fast might be out of time order,so sort it...
         call XSL_GETRM(str2,evnin)
         write(ilun,'(a)') str2(:LENACT(str2))
c Now write the sort line:
         comlin = 'fmemsort '//     
     +        'infile='//work2(1)(1:len2)//' '//
     +        'outfile='//evnin(:LENACT(evnin))//' '//
     +        'column='//keytim(:LENACT(keytim))//' '//
     +        'method=insert ascend=yes copyall=yes history=yes'
         call XSL_WRTCF(ilun,comlin,1)
c Now remove the work2 file, to reduce the disk space used:
         call XSL_GETRM(str2,work2(1))
         write(ilun,'(a)') str2(:LENACT(str2))
      ELSE         
         DO i = 1, nfiles

            IF( WORK )then
               len1 = LENACT( work1(i) )
               len2 = LENACT( work2(i) )

               comlin='fasttime '//
     +              'infile='//work1(i)(1:len1)//' '//
     +              'outfile='//work2(i)(1:len2)//' '//
     +              str1(:len3)//' '//
     +              'timecol=TIME ycolumn=RAWY '//
     +              'sensecase=yes'
               str2 = 'Now processing '//work1(i)(1:len1)
               call XSL_MESSAGE(ilun,str2)
               call XSL_WRTCF(ilun,comlin,1)
c The output of fast might be out of time order,so sort it...
c First remove the work1 file:
               call XSL_GETRM(str2,work1(i))
               write(ilun,'(a)') str2(:LENACT(str2))
c Now write the sort line:
               comlin = 'fmemsort '//     
     +              'infile='//work2(i)(1:len2)//' '//
     +              'outfile='//work1(i)(1:len1)//' '//
     +              'column='//keytim(:LENACT(keytim))//' '//
     +              'method=insert ascend=yes copyall=yes history=yes'
               call XSL_WRTCF(ilun,comlin,1)
c Now remove the work2 file, to reduce the disk space used:
               call XSL_GETRM(str2,work2(i))
               write(ilun,'(a)') str2(:LENACT(str2))
            ELSE
               len1 = LENACT( filenm(i) )
               len2 = LENACT( work2(i) )
               
               comlin='fasttime '//
     +              'infile='//filenm(i)(1:len1)//' '//
     +              'outfile='//work2(i)(1:len2)//' '//
     +              str1(:len3)//' '//
     +              'timecol=TIME ycolumn=RAWY '//
     +              'sensecase=yes'
               str2 = 'Now processing '//filenm(i)(1:len1)
               call XSL_MESSAGE(ilun,str2)
               call XSL_WRTCF(ilun,comlin,1)
c See note above...
c First remove the work1 file:
               call XSL_GETRM(str2,work1(i))
               write(ilun,'(a)') str2(:LENACT(str2))
c Now write the sort line:
               comlin = 'fmemsort '//     
     +              'infile='//work2(i)(1:len2)//' '//
     +              'outfile='//work1(i)(:LENACT(work1(i)))//' '//
     +              'column='//keytim(:LENACT(keytim))//' '//
     +              'method=insert ascend=yes '//
     &              'copyall=yes history=yes'
               call XSL_WRTCF(ilun,comlin,1)
c Now remove the work2 file, to reduce the disk space used:
               call XSL_GETRM(str2,work2(i))
               write(ilun,'(a)') str2(:LENACT(str2))
            ENDIF

c We've now sent the command to do the conversion.
c Now, all the selected files will be in WORK2, which should
c become the CURRENT files. Thus, copy the WORK2 files to WORK1


         END DO
      ENDIF

      call XSL_CLCF(ilun)
      
c Run the command file
      call XSL_RUNCF(cmdfil,ECHO,status)
c Check for errors
      IF(status.eq.0) THEN
         FAST = .TRUE.
         IF(BINOUT) THEN
            BININ = .TRUE.
         ELSE
c Set the WORK logical
            WORK = .TRUE.
         
c Reset the MERGED logical -- individual files have (probably) changed.
            MERGED = .FALSE.
         ENDIF

c We also have to set the coordinates to the RAWX, RAWY set:
         call XWRITE('Setting X and Y to RAWX and RAWY',5)

         xcolf = keyx(3)
         ycolf = keyy(3)
         xfkey = keyxsz(3)
         yfkey = keyysz(3)

         xcolh = keyx(3)
         ycolh = keyy(3)
         xhkey = keyxsz(3)
         yhkey = keyysz(3)

c Also we don't want a weighted map for the FAST mode data, so
         call XWRITE('And turning off the weighted map',5)
         WTMAPB = .FALSE.

      ENDIF

      return
      end
 
c
c ----------------------------------------
      subroutine XSL_GISCLEAN()
c ----------------------------------------
c This does the GIS RTI cleaning using FLOOKUP
c Jim Ingham 4/94

      include 'xsel.inc'
      include 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line:
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(512) str1, str2, str4
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2
c ---------------------------------------------
      character(5) mxbnstr
      character(32) keycol,cmpcol
      character(255) tblnam
      integer LENACT
      integer i,stati,maxbns

      status = 0
      IF(.NOT.READ) THEN
         call XWRITE('No data read in yet',5)
         return
      ENDIF
      IF (keymis.ne.'ASCA'.or.datamode.ne.'PH') THEN
         call XWRITE('GISCLEAN can only be used for '//
     &        'ASCA GIS PH data',5)
         return
      ENDIF
      IF(.NOT.VGISCLEAN) THEN
         call XWRITE('Some of your input files have no '//
     &        'RISE TIME information, so you cannot',5)
         call XWRITE('use GISCLEAN.',5)
         return
      ENDIF

      call xsl_uclgst('keycol',keycol,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgst('compcol',cmpcol,status)
      IF ( status .NE. 0 ) RETURN
      
c First make sure we get the right lookup table:
c Get the TLMAX keyword for the PHA column.
c At the same time remove the scratch files:

      IF(BINOUT) THEN
         call XSL_GCOLKWI(evnout,'1',wrkdir,keycol,'TLMAX',maxbns,stati,
     &     1,1,status)
         call XSL_RMFILE(evnin)
      ELSE IF(WORK) THEN
         call XSL_GCOLKWI(work1(1),'1',wrkdir,keycol,'TLMAX',
     &     maxbns,stati,1,1,status)
         call XSL_RMWORK(work2,nfiles)
      ELSE
         call XSL_GCOLKWI(filenm(1),evtnum,datdir,keycol,'TLMAX',
     &     maxbns,stati,1,1,status)
         call XSL_RMFILE(work1)
      ENDIF
c      print*, 'got maxbins = ',maxbns

      call XSL_UCLGOT('rti_table_user',status)

      if(status .eq. 0) THEN 
         call xsl_uclgst('rti_table_user',tblnam,status)
         IF ( status .NE. 0 ) RETURN
      ELSE
         status = 0
c Build up the parameter name dynamically:
         call XSL_I2LJSTR(maxbns+1,mxbnstr)
         str1 = 'rti_table_'//mxbnstr

         call xsl_uclgst(str1,tblnam,status)
         IF(status.ne.0) THEN
            status = 0
            write(str4,55) maxbns
 55         format('There is no RTI table for ',i4,' PHA bins')
            call XWRITE(str4,5)
            return
         ENDIF
         if(refdir.eq.'NONE') then
            call XWRITE('No ref data directory found',5)
            status = -10
            return
         else
            tblnam = refdir(:LENACT(refdir))//tblnam
            str1 = 'Using table:'//tblnam
            call XWRITE(str1,5)
         endif
      ENDIF
      
      call XSL_EXIST(tblnam,status)
      if(status.ne.0) then
         str4 = 'Could not find RTI table: '//tblnam
         call XWRITE(str4,5)
         return
      endif

c Delete previous command file and work2 files

      call XSL_RMWORK(work2,nfiles)


c Open new command file

      call XSL_OPCF(cmdfil,ilun)

c If we've already performed a SELECTion, we'll be using
c the WORK1 filenames as input. Otherwise it'll be the original
c filenames. Either way, the WORK2 files are the output files, which
c are then copied (back) to the WORK1 files and the .WORK. logical
c reset for subsequent observations.

c Construct instruction strings controlling NFILES applications
c of FSLOOKUP.
      str1 =  tblnam(:LENACT(tblnam))//
     &     ' tcolumn='//keycol(:LENACT(keycol))//
     &     ' lcolumn=CHAN lbcol=RTI_MIN ubcol=RTI_MAX'//
     &     ' column='//cmpcol(:LENACT(cmpcol))//
     &     ' bounds=''[]'' history=yes copyall=yes'
      len1 = LENACT(str1) 

      IF(BINOUT) THEN
         call XSL_RMFILE(evnin)
         comlin = 'flookup '//evnout(:LENACT(evnout))//' '//
     &        evnin(:LENACT(evnin))//' '//str1(:len1)
         call XSL_WRTCF(ilun,comlin,1)
      ELSE
         
         DO i = 1, nfiles
            
            IF( WORK )then
               len2 = LENACT(work1(i))
               comlin = 'flookup '//work1(i)(:len2)//' '//
     &              work2(i)(:LENACT(work2(i)))//' '//str1(:len1)

               str2 = 'Now processing '//work1(i)(1:len2)
               call XSL_MESSAGE(ilun,str2)
            ELSE
               len2 = LENACT( filenm(i) )
               comlin = 'flookup '//filenm(i)(:len2)//' '//
     &              work1(i)(:LENACT(work2(i)))//' '//str1(:len1)
               
               str2 = 'Now processing '//filenm(i)(1:len2)
               call XSL_MESSAGE(ilun,str2)
            ENDIF

            len2 = LENACT( str2 )
            call XSL_WRTCF(ilun,comlin,1)
c We've now sent the command to do the conversion.
c Now, all the selected files will be in WORK2, which should
c become the CURRENT files. Thus, copy the WORK2 files to WORK1
            
            
         END DO
      ENDIF
      call XSL_CLCF(ilun)
   
c Run the command file
      call XSL_RUNCF(cmdfil,ECHO,status)
c Check for errors
      IF(status.eq.0) THEN
         CLEAN = .TRUE.
         IF(BINOUT) THEN
            BININ = .TRUE.
         ELSE IF (WORK) THEN
c We now copy the work2 files to work1
            MERGED = .FALSE.
            call XSL_OPCF(cmdfil,ilun)
            do i=1,nfiles
               len1 = LENACT(work1(i))
               call XSL_RENAME(work2(i),work1(i),2,str1,len1,ierr)
               write(ilun,'(a)') str1(1:len1)
            enddo
            call XSL_CLCF(ilun)
            
c Run the command file
            call XSL_RUNCF(cmdfil,ECHO,status)
         ELSE
c Set the WORK logical
            WORK = .TRUE.
         
c Reset the MERGED logical -- individual files have (probably) changed.
            MERGED = .FALSE.
         ENDIF
      ENDIF
        

      return
      end
c      
c      
c --------------------------------------------------------------
      subroutine XSL_SISCLEAN()
c --------------------------------------------------------------      
C This runs the cleansis ftool to remove hot pixels.
c clean_method 2 applies a Poisson filter
c clean_method 1 is a simple linear cutoff.  
C     Jim Ingham  5/93
c     Modified to use only cleansis 3/94
C     
      include 'xsel.inc'
      include 'xselvar.inc'
      include 'xselplt.inc'
c ---------------------------------------------
c Scratch variables
c The command line:
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(255) str2, str3, contxt
c File I/O unit numbers
      integer ilun,ilun2,len2
c ---------------------------------------------      
      integer  LENACT
      logical PLOT,SAOIM,ZERO,ITERAT,ANSW
      
cc      if ( keymis.ne.'ASCA'.or.instru(1:3).ne.'SIS') then
cc         contxt = 'Cleansis only works for ASCA SIS data'
cc         status = -10
cc         GOTO 999
cc      endif
      status = 0
c Get the method variable
      call xsl_uclgsi('clean_method',clnmet,status)
      contxt = 'Failed to get clean_method parameter'
      IF ( status .NE. 0 ) GOTO 999
      if (clnmet.ne.1 .and. clnmet.ne.2) then
         contxt = 'No such method'
         status = -10
         GOTO 999
      ENDIF
c There are now two plot and saoimage parameters, one for each method
      IF (clnmet.eq.1) then
c Get the SAOIM logical
         call xsl_uclgsb('saoimage',SAOIM,status)
         contxt = 'Failed to get saoimage parameter'
         IF ( status .NE. 0 ) GOTO 999
c Get the Gendreaux Plot logical
         call xsl_uclgsb('sis_plot',PLOT,status)
         contxt = 'Failed to get sis_plot parameter'
         IF ( status .NE. 0 ) GOTO 999
      ELSE
c Get the SAOIM logical
         call xsl_uclgsb('saoimage2',SAOIM,status)
         contxt = 'Failed to get saoimage2 parameter'
         IF ( status .NE. 0 ) GOTO 999
c Get the Gendreaux Plot logical
         call xsl_uclgsb('sis_plot2',PLOT,status)
         contxt = 'Failed to get sis_plot2 parameter'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF

      IF (PLOT) THEN
         IF (plotdv.eq.' '.or.plotdv.eq.'NONE') THEN
            call XSL_GETDEV()
            if (status.ne.0) then
               status = 0
               return
            endif
         ENDIF                   
      ENDIF
c Now get the rest of the parameters needed:

      IF(clnmet.eq.2) THEN
         call xsl_uclgsi('cellsize',cellsz,status)
         contxt = 'Failed to get cellsize parameter'
         IF ( status .NE. 0 ) GOTO 999
         call xsl_uclgsr('log_prob',logprb,status)
         contxt = 'Failed to get log_prob parameter'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF
      
      call xsl_uclgsi('bkg_threshold',bkgthr,status)
      contxt = 'Failed to get bkg_threshold parameter'
      IF ( status .NE. 0 ) GOTO 999
      call xsl_uclgsi('clean_phalow',cphal,status)
      contxt = 'Failed to get clean_phalow parameter'
      IF ( status .NE. 0 ) GOTO 999
      call xsl_uclgsi('clean_phahi',cphah,status)
      contxt = 'Failed to get clean_phahi parameter'
      IF ( status .NE. 0 ) GOTO 999
      call xsl_uclgsb('dirtysis',DIRTY,status)
      contxt = 'Failed to get dirtysis parameter'
      IF ( status .NE. 0 ) GOTO 999

      if(clnmet.eq.2) THEN
         call xsl_uclgsb('iterate',ITERAT,status)
         contxt = 'Failed to get iterate parameter'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF
     
      call xsl_uclgsb('clean_zero',ZERO,status)      
      contxt = 'Failed to get clean_zero parameter'
      IF ( status .NE. 0 ) GOTO 999

c We need to check if a filtered event list has been made yet:
c If there is, we will use it, and need only call cleansis,
c unless we need to make the image:        
c If there is only one file, we also need not run it 
c through the extractor, unless we need to make an image:
c Otherwise, run it through the extractor, with whatever coordinates and 
c filters have already been set, to create an output event list, then 
c feed this to CLEANSIS

      IF(BININ) THEN
         IF( CLEAN.AND.CLNSAV ) THEN
            continue
         ELSE
            IF(CLEAN) THEN
               call XWRITE
     &              ('Using the results of the previous SISCLEAN.',5)
               call XWRITE
     &              ('This will overwrite the these results.',5)
            ELSE 
               call XWRITE('SISCLEAN will overwrite the filtered'//
     &              'events list currently iput to the extractor.',5)
            ENDIF
            call xsl_uclgsb('save_file',ANSW,status)
            contxt = 'Failed to get save_file parameter'
            IF ( status .NE. 0 ) GOTO 999
            IF(ANSW) THEN
               call XSL_SAVE(3)
            ENDIF
            IF(.NOT.CLEAN) THEN
C We only want to reuse the evnin if it comes from cleansis
               call XSL_RMFILE(evnin)
               BININ = .FALSE.
            ENDIF
         ENDIF
      ELSE IF(nfiles.gt.1.and. .NOT. BINOUT) THEN
         call XWRITE('-------------------------------'//
     &        '----------',5)
         call XWRITE('Making EVENTS list for cleansis.',5)
         if(SAOIM) call XWRITE('Making IMAGE.',5)
         call XWRITE(' ',5)
         call XWRITE('This may take awhile.',5)
         call XWRITE(' ',5)
c This makes an image and an event list
         call XSL_BIN(4)
         call XWRITE(' ',5)
         IF(status.ne.0) THEN
            contxt = 'Error in SISCLEAN from XSL_BIN'
            goto 999
         ENDIF
      ENDIF
c Now make an image if there is not already one, and the user requests the
c hot pixel image.
      IF(SAOIM.and. .NOT. IMAGE) THEN
         call XWRITE('-------------------------------'//
     &        '----------',5)
         call XWRITE('Making IMAGE.',5)
c This just makes an image
         call XSL_BIN(1)
      ENDIF
c Fire up SAOImage if asked.
      if(SAOIM) THEN
         call XSL_SAOIMAGE(1)
      ENDIF

c If they want the Gendreaux plot, make it:
      IF(PLOT) THEN
         call XWRITE(' ',5)
         call XWRITE(' The following plot shows the '//
     &        'distribution of counts/pixel in the image.',5)
         call XWRITE(' The solid line is the integral '//
     &        'distribution.',5)
         call XWRITE(' The dotted line is that expected for a '//
     &        'field with no sources.',5)
         call XWRITE(' ',5)
         call XWRITE
     &        ('  Type quit at the PLT prompt to continue, ',5)
         call XWRITE(' ',5)
         call XWRITE
     &        ('Making the Pixel vrs. Counts per Pixel graph',5)
         call XSL_RMFILE(cmdfil)
         CALL XSL_OPCF(cmdfil,ilun)
         call XSL_RMFILE(tempfl)
         IF (BININ) THEN
            comlin = 'sishist '//evnin(:LENACT(evnin))//' '//
     &           tempfl(:LENACT(tempfl))//' rawxname=RAWX '//
     &           'rawyname=RAWY ccdidname=CCDID fit=yes rows=- '//
     &           'sensecase=no'
         ELSE IF(BINOUT) THEN
            comlin = 'sishist '//evnout(:LENACT(evnout))//' '//
     &           tempfl(:LENACT(tempfl))//' rawxname=RAWX '//
     &           'rawyname=RAWY ccdidname=CCDID fit=yes rows=- '//
     &           'sensecase=no'
         ELSE IF (WORK) THEN
            comlin = 'sishist '//work1(1)(:LENACT(work1(1)))//' '//
     &           tempfl(:LENACT(tempfl))//' rawxname=RAWX '//
     &           'rawyname=RAWY ccdidname=CCDID fit=yes rows=- '//
     &           'sensecase=no'
         ELSE
            comlin = 'sishist '//filenm(1)(:LENACT(filenm(1)))//' '//
     &           tempfl(:LENACT(tempfl))//' rawxname=RAWX '//
     &           'rawyname=RAWY ccdidname=CCDID fit=yes rows=- '//
     &           'sensecase=no'
         ENDIF
         call XSL_WRTCF(ilun,comlin,1)
C NOW PREPARE TO RUN FPLOT
c This is the PLT command file:
         call XSL_RMFILE(lstfil)
         call GETLUN(ilun2)
         call XSL_OPEN(ilun2,lstfil,'NEW',' ',' ',0,0,ierr)
         write(ilun2,*) 'LA T Plot of Counts Per Pixel/ # of Pixels'
         write(ilun2,*) 'LA X Number of Pixels'
         write(ilun2,*) 'LA Y Counts per Pixel'
         write(ilun2,*) 'DEVICE '//plotdv(:LENACT(plotdv))
         write(ilun2,*) 'PLOT OVERLAY'
         write(ilun2,*) 'LOG Y ON'
         write(ilun2,*) 'COLOR 2 on 2'
         write(ilun2,*) 'COLOR 3 on 3'
         write(ilun2,*) 'LSTYLE 2 ON 3'
         write(ilun2,*) 'label 2 vpos .7 .85 line 0 co 2 "Data"'
         write(ilun2,*) 'label 1 vpos .7 .82 line 0 lstyle 2 '//
     &        'co 3 "Poisson Fit"'
         write(ilun2,*) 'R 0 10 1'
         close(ilun2)
         call FRELUN(ilun2)
c This is the run line:
         comlin = fpltwin(:lenact(fpltwin))//' fplot '//
     &          tempfl(:LENACT(tempfl))//
     &        ' xparm=CTS_PXL yparm="ALLCCD,ALLCCD_FIT" '//
     &        'rows="-" device='//plotdv(:LENACT(plotdv))//
     &        ' pltcmd=@'//lstfil(:LENACT(lstfil))//
     &        ' offset=no maxpts=10000'//
     &        fpltbkg(:lenact(fpltbkg))
         call XSL_WRTCF(ilun,comlin,0)
         call XSL_CLCF(ilun)
         call XSL_RUNCF(cmdfil,ECHO,status)
         IF(status.ne.0) THEN
            contxt = 'Error in SISCLEAN from SISHIST'
            GOTO 999
         ENDIF
      ENDIF
      
c Now write out the string controlling cleansis      
c Now open the command file:

      call XSL_RMFILE(cmdfil)
      call XSL_RMFILE(tempfl)
      CALL XSL_OPCF(cmdfil,ilun)

      IF(BININ) THEN
         comlin = 'cleansis datafile='//evnin(:LENACT(evnin))//
     &        ' outfile='//tempfl(:LENACT(tempfl))
      ELSE IF(BINOUT) THEN
         comlin = 'cleansis datafile='//evnout(:LENACT(evnout))//
     &        ' outfile='//evnin(:LENACT(evnin))
      ELSEIF (WORK) THEN
         comlin = 'cleansis datafile='//
     &        work1(1)(:LENACT(work1(1)))//
     &        ' outfile='//evnin(:LENACT(evnin))
      ELSE
         comlin = 'cleansis datafile='//
     &        filenm(1)(:LENACT(filenm(1)))//
     &        ' outfile='//evnin(:LENACT(evnin))
      ENDIF         

c Now write out the phalmin and phahmax strings:
      write(str3,59) cphal,cphah
 59   format('phamin=',i6,' phamax=',i6)
c And the cell size,...
      if(clnmet.eq.2) THEN          
         write(str2,67) cellsz,logprb,bkgthr
 67      format('cellsize =',i3,' logprob=',e8.2,' bthresh=',i3)
      else
         write(str2,68) bkgthr
 68      format('cellsize=0 logprob=-5.0 bthresh=',i3)
      endif
      IF(clnmet.eq.2.and.ITERAT) THEN
         str3 = str3(:LENACT(str3))//' iterate=yes'
      ELSE
         str3 = str3(:LENACT(str3))//' iterate=no'
      ENDIF
      IF(ZERO) THEN
         str3 = str3(:LENACT(str3))//' zeroedge=yes'
      ELSE
         str3 = str3(:LENACT(str3))//' zeroedge=no'
      ENDIF

      IF(DIRTY) THEN
         str3 = str3(:LENACT(str3))//' dirtysis=yes'
      ELSE
         str3=str3(:LENACT(str3))//' dirtysis=no'
      ENDIF 

c Finally build up the command line:

      comlin = comlin(:LENACT(comlin))//' '//
     &     str2(:LENACT(str2))//' '//
     &     str3(:LENACT(str3))//' '//
     &     ' rawxcol=RAWX rawycol=RAWY '//
     &     ' chipcol='//keyccd(:LENACT(keyccd))//
     &     ' timecol='//keytim(:LENACT(keytim))//
     &     ' detxcol='//keyx(2)(:LENACT(keyx(2)))//
     &     ' detycol='//keyy(2)(:LENACT(keyy(2)))//
     &     ' skyxcol='//keyx(1)(:LENACT(keyx(1)))// 
     &     ' skyycol='//keyy(1)(:LENACT(keyy(1)))// 
     &     ' phacol='//keypha(:LENACT(keypha))//
     &     ' verbose=yes'
C         len1 = LENACT(comlin)
      call XSL_WRTCF(ilun,comlin,1)
      call XSL_CLCF(ilun)
      call XSL_RUNCF(cmdfil,ECHO,status)
      IF(status.ne.0) THEN
         contxt = 'Error in SISCLEAN from CLEANSIS'
         GOTO 999
      ELSE
         IF(BININ) THEN
c In this case, we must copy back from tempfl:
            len2 = LENACT(evnin)
            call XSL_RENAME(tempfl,evnin,1,str2,len2,status)
         ENDIF
c We will not erase the output event file evnout, since you might want
c to reuse it.

         BININ = .TRUE.
         CLEAN = .TRUE.
         CLNSAV = .FALSE.
         IF(status.ne.0) THEN
            contxt = 'Error in SISCLEAN from XSL_EXTRACT'
            GOTO 999
         ENDIF
      ENDIF

 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL xwrite(contxt, 5)
         WRITE(contxt, '(a,i5)') 'XSL_SISCLEAN:status=', status
         CALL xwrite(contxt, 5)
      ENDIF

      return  
      end
c
c
c ------------------------------------------------------------
      subroutine XSL_SET_XYCENTER()
c ------------------------------------------------------------
c Sets the center of the image to the center of the detector
c J. Ingham 9/94

      include 'xsel.inc'
      include 'xselvar.inc'

      integer xcenter(10,3),ycenter(10,3),i
      character(64) context
c These store the image centers:
c For ROSAT and for the ASCA SIS.  They are variable for the GIS,
c so we get them from the rxbval.  So:
c i=1 -> ROSAT PSPC
c i=2 -> ROSAT HRI
c i=3 -> ASCA SIS
c
c The j coordinate refers to the DET or RAW or SKY value...
      data (xcenter(1,i),i=1,3) /7680,4096,0/
      data (ycenter(1,i),i=1,3) /7680,4096,0/
      data (xcenter(2,i),i=1,3) /4096,2048,0/
      data (ycenter(2,i),i=1,3) /4096,2048,0/
      data (xcenter(3,i),i=1,3) /640,640,320/
      data (ycenter(3,i),i=1,3) /640,640,320/

      do i=1,3
         if (xcolf.eq.keyx(i)) goto 555
      enddo

      call XWRITE('Error getting coordinate',5)
      status = -10
      return

 555  continue

      if(keymis.eq.'ROSAT') THEN
         IF(instru(1:3).eq.'PSP') then
            xcf = xcenter(1,i)
            ycf = ycenter(1,i)
         ELSEIF(instru.eq.'HRI') then
            xcf = xcenter(2,i)
            ycf = ycenter(2,i)
         ELSEIF(instru.eq.'NONE') then
            call XWRITE('No instrument set, so I don''t know '//
     &           'where the image center should be.',5)
         ELSE
            call XWRITE('Unrecognized instrument for ROSAT',5)
            status = -20
         ENDIF
      ELSE IF (keymis.eq.'ASCA') THEN
         IF(instru(:3).eq.'SIS') then
            xcf = xcenter(3,i)
            ycf = ycenter(3,i)
         ELSEIF(instru(:3).eq.'GIS') then
            xcf = rxbval/2
            ycf = rxbval/2
         ELSEIF(instru.eq.'NONE') then
            call XWRITE('No instrument set, so I don''t know '//
     &           'where the image center should be.',5)
         ELSE
            call XWRITE('Unrecognized instrument for ASCA',5)
            status = -20
         ENDIF
      ELSEIF(keymis.eq.'NONE') then
            call XWRITE('No mission set, so I don''t know '//
     &                  'where the image center should be.',5)
      ELSE
         xcf = 0
         ycf = 0
         call XWRITE(
     & 'Use set XYCENTER to define the center of the output image',5)
      ENDIF

      write(context,'(a,I6,a,I6,a)') ' * Setting XYCENTER to ( ',
     &     xcf,' , ',ycf,' )'
      
      call XWRITE(context,5)

      return
      end
               
c
c
c ---------------------------------------------
      subroutine XSL_MAKEO(mode)
c ---------------------------------------------
c called by XSEL, this routine finds all the files with
c type (say) `.fits' in the current directory and lists them
c into a fits file for later use in selection, merging,
c etc.
c
c mode = 0 is normal mode. 
c mode = 1 is for the temporary catalogue from read.
c
c Called by XSELECT main
c
c     Alan Smale 1993 Feb
c
      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line:
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(512) str1, str2, str3, str4, newnam
      character(10) tcatnum
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2, len3, len4, len5
c ---------------------------------------------

      character(1024) catstr
      character(255) tmpcat
      integer i,j,ninvec,mode,lenn,lenb,lene,iflag,idelim
      character(80) lststr,outkey(3),outcom(3)
      character(8) keysname(4)
      character(256) keysval(4)
      character(16) tmpvec(MXCATC),incol(3)
      logical qdone, qskip
      integer LENACT
      integer NKEYS
      parameter(NKEYS = 3)
      data (outkey(i),i=1,NKEYS) /'INSTRUME','TELESCOP','OBJECT'/
      data (incol(i),i=1,NKEYS)  /'INSTRUME','TELESCOP','OBJECT'/
      data (outcom(i),i=1,NKEYS) /'The instrument for the catalogue',
     &     'The telescope for the catalogue',
     &     'The object for the catalogue'      /
      data keysname /'DATADIR','HKDIR','DATAMODE','LIST_STR'/
      status = 0

      if(mode.eq.0) then
         call xsl_uclgst('make_what',str1,status)
         IF ( status .NE. 0 ) RETURN
         call UPC(str1)
      else if (mode.eq.1) then
         str1 = 'OBSCAT'
      endif

      IF(index('OBSCAT',str1(:LENACT(str1))).ne.0)then
c ---------------------------------------------
c ** MAKE OBSCAT

c create a file called OBSFIL containing all the files in the 
c current directory with filetype FTYPE
c Making a new catalogue you must unload the old one
         LOADED = .FALSE.
c You must also set MADE to false in case the make fails:
         MADE = .FALSE.
c Also reset the catalogue selections:
         choflt = 'NONE'
         catsel = 'NONE'
         
         IF (mode.eq.0) THEN
c Now do a clear data, but keep datamode and datadir:
            str2 = datamode
            str1 = datdir
c Prompt for the instrument if it has not been set...
            IF(instru.eq.'NONE') THEN      
               call XSL_SET(1)
            ENDIF
            call XSL_CLEAR(2)
            datdir = str1(:min(lenact(str1),len(datdir)))
            datamode = str2(:min(lenact(str1),len(datamode)))

c We have to add datamode back to the prompt 

            call XSL_SET(6)

            call XSL_SET_DIR('data_dir',datdir,status)
            if (status.ne.0) then
               return
            endif

c Get the obsno

            CALL XSL_MDB_INST(keymis, submis, instru, obsno, status)

            IF ( obsno .GT. MAXOBS ) THEN
               str1 = 
     & 'Too many different instruments - increase MAXOBS in xsel.inc'
               call XWRITE(str1,5)
               status = -20
               return
            endif

c Now set the lststr. We set this parameter in the XSL_INST_SET routine
c and this is here to allow the user to override the default.

            call xsl_uclgst('lststr', lststr, status)
            if( status .NE. 0 .OR. obsno.lt.1) RETURN

c Now move an old catalogue out of the way:
            call XSL_MOVE_AWAY(obscat(obsno),newnam)
c Make a list of files, and read it in:
            call XSL_LIST(obsfil,lststr,datdir,wrkdir,status)
            nincat = 0
            call XSL_GET_LIST(catfil,nincat,MAXFIL,obsfil,status)
c  Make sure there was a match
            if(nincat.eq.0) then
               str1 = 'No files found for instrument '//instru
               call XWRITE(str1,5)
               status = 10
               return
            endif
            if(status.eq.-20) then
               write(str1,99) MAXFIL
 99            format('Too many files, stopping at ',i4)
               call XWRITE(str1,5)
               status = 0
            endif
         ELSE 
            catflt = 'NONE'
         ENDIF            
C Now get the HKDIR
         call xsl_uclgst('hk_dir',str3,status)
         IF ( status .NE. 0 ) RETURN
         call XSL_RELDIR(datdir,str3,hkdir,status)
         if ( status .ne. 0) then
            len1 = LENACT(hkdir)
            str1 = 'HK Directory '//hkdir(1:len1)//' does not exist'
            call XWRITE(str1,5)
            return
         endif

c  Now append the +catnum extension to the filenames, to pass on to ffilecat

         call XSL_RMFILE(obsfil)
         call GETLUN(ilun)
         call XSL_OPEN(ilun,obsfil,'NEW',' ',' ',0,0,ierr)
         if (ierr .ne. 0) then
            str1 = 'Cannot create file '//obsfil(:lenact(obsfil))
            call XWRITE(str1,5)
            str1 = 'Check that you have write-access to this directory'
            call XWRITE(str1,5)
            RETURN
         endif
         if (mode .eq. 0) then
            do i=1,nincat
               len1 = LENACT(catfil(i))
               write(ilun,79) catfil(i)(1:len1),catnum
            enddo
         else if (mode.eq.1) then
c For mode = 1, just put the filenm list into obsfil, and into catfil.
            do i=1,nfiles
               len1 = LENACT(filenm(i))
               catfil(i) = filenm(i)
cBEO
               tcatnum = catnum
               if (keymis .eq. 'ROSAT') then
                  call xsl_findext('EVENTS',catfil(i),tcatnum)
               end if
               write(ilun,79) catfil(i)(1:len1),tcatnum
 79            format(a,'+',a3)
            enddo
            nincat = nfiles
         endif

         close(ilun)
         call FRELUN(ilun)
         
         if ( mode .eq. 0 ) then
c  Get the selection string if there is one, 
c  if it is DEF, fill in its value here:
            call xsl_uclgst('cat_filt',catflt,status)               
            IF ( status .NE. 0 ) RETURN
            if(catflt.eq.'DEF'.or.catflt.eq.'def') then
               call XSL_GET_DEFOBSEL(instru,datamode,submis,keymis,
     &                               catflt)
               str2 = 'Using default selection expression: '
     &              //catflt(:LENACT(catflt))
               call XWRITE(str2,5)
            ENDIF         
         else
            catflt = 'NONE'
         endif

c OK, now make a FITS file containing important parameters using
c the FFILECAT task.

c Set up the command string to run ffilecat.  First construct 
c the default parameter list if no user defined list is specified.

         len4 = LENACT(wrkdir)
         call xsl_uclgst('obslist',obslis,status)
         IF ( status .NE. 0 ) RETURN

         CALL XSL_MDBS(keymis, submis, instru, datamode, 'catcol', 
     &                 catstr, status)
         IF ( status .NE. 0 ) THEN
            CALL XWRITE('Could not find catcol entry in MDB', 5)
            RETURN
         ENDIF

c Now parse catstr and set up the keyword array

         ncatcl = 0
         qdone = .FALSE.
         lenn = 0
         CALL xgtarg(catstr, lenn, lenb, lene, qskip, iflag, idelim)
         IF ( iflag .NE. 0 ) qdone = .TRUE.

         DO WHILE ( .NOT.qdone )

            ncatcl = ncatcl + 1
            catcol(ncatcl) = catstr(lenb:lene)      

            CALL xgtarg(catstr, lenn, lenb, lene, qskip, iflag, idelim)
            IF ( iflag .NE. 0 ) qdone = .TRUE.

         ENDDO

c If there is a user-list, merge it with the standard list

         len5 = LENACT(obslis)
         len1 = ncatcl
         len2 = MXCATC - ncatcl
         if(obslis(1:1).eq.'@') then
            call GETLUN(ilun)
            str2 = obslis(2:len5)
            call XSL_OPEN(ilun,str2,'OLD',' ',' ',0,0,ierr)
            if(ierr.ne.0) then
               call XWRITE
     +              ('Catalogue list not found; assuming default',5)
            else
               ierr = 0
               do i=1, len2
                  read(ilun,'(a)',end=236,err=234,iostat = ierr) str1
                  if(str1 .eq. ' ') goto 234
                  do j=1, len1
                     if(str1(1:8).eq.catcol(j)) goto 234
                  enddo
                  ncatcl = ncatcl + 1
                  catcol(ncatcl) = str1(1:8)
 234              IF(ierr.ne.0)THEN
                     write(str2,87) ierr,i,obslis(2:len5)
 87                  format('Error no. ',i4,' reading line ',
     &                    i3,' in file ',a)
                     call XWRITE(str2,5)
                     ierr = 0
                  ENDIF
               enddo
               str2 = 'Too many entries in file '//obslis(2:len5)
               call XWRITE(str2,5) 
               write(str2,88) len2 - 1
 88            format('Max is ',i4)
               call XWRITE(str2,5)
               str2 = 'Last element accepted was '//catcol(ncatcl)
               call XWRITE(str2,5)
 236           continue
            endif
            close(ilun)
            call FRELUN(ilun)
         ELSEIF( obslis.ne.'def') THEN
            call XSL_PARSE(obslis,tmpvec,ninvec,MXCATC,status)
            do i=1, ninvec
               do j=1, len1
                  if(str1(1:8).eq.catcol(j)) goto 244
               enddo
               IF(ncatcl.gt.MXCATC) THEN
                  ncatcl = ncatcl - 1
                  str2 = 'Too many entries in file '//obslis(2:len5)
                  call XWRITE(str2,5) 
                  write(str2,88) len2 - 1
                  call XWRITE(str2,5)
                  str2 = 'Last element accepted was '//catcol(ncatcl)
                  call XWRITE(str2,5)
                  goto 246
               ENDIF
               ncatcl = ncatcl + 1
               catcol(ncatcl) = tmpvec(i)(:min(lenact(tmpvec(i)),
     &                                         len(catcol(ncatcl))))
 244           CONTINUE
            enddo
         ENDIF
C Now write the list into the obslis file

 246     CONTINUE

         len1 = LENACT(prefix)
         str2 =prefix(:len1)//'_obslist.def' 
         call XSL_DATDIR(str2,wrkdir,0)
         call XSL_RMFILE(str2)
         call GETLUN(ilun)
         call XSL_OPEN(ilun,str2,'NEW',' ',' ',0,0,ierr)
         do i=1,ncatcl
            write(ilun,'(a)') catcol(i)
         enddo
         close(ilun)
         call FRELUN(ilun)
c Now direct ffilecat to _obslist.def
         len5 = LENACT(str2)
         obslis = 'keywords="@'//str2(1:len5)//'"'

c Remove the old catalog
         if(mode.eq.0) then
            call XSL_RMFILE(obscat(obsno))
         else if (mode.eq.1) then
            tmpcat = wrkdir(:LENACT(wrkdir))//prefix(:LENACT(prefix))//
     &           '_read_cat.xsl'
            call XSL_RMFILE(tmpcat)
         endif
         
         call XSL_OPCF(cmdfil,ilun)
         
c  Now get the length of the pertinent strings, with wrkdir prepended         
         len1 = LENACT(obslis)
         str2 = obsfil
         call XSL_DATDIR(str2,wrkdir,0)
         len2 = LENACT(str2)
c If there is a selection string, make a temp catalogue:
         IF(catflt.eq.'NONE') then
            if(mode.eq.0) then
               str3 = obscat(obsno)
               call XSL_DATDIR(str3,wrkdir,0)
            else if (mode.eq.1) then
               str3 = tmpcat
            endif
            len3 = LENACT(str3)
         ELSE
            str3 = tempfl 
            call XSL_DATDIR(str3,wrkdir,0)
            len3 = LENACT(str3)
c  Remove the temporary catalogue file:
            call XSL_RMFILE(str3)
         ENDIF

         comlin = 'ffilecat infile="@'//str2(1:len2)//'" '//
     &        'outfile='//str3(1:len3)//' '//
     &        obslis(1:len1)//' '//
     &        'maxlen=11'//' '//
     &        'minlen=0'//' '//
c     &        'aform=A11'//' '//
     &        'iform=I8'//' '//
     &        'eform="E8.2" '//
     &        'quiet=yes'//' '//
     &        'omit=no'
         call XSL_WRTCF(ilun,comlin,1)

c If the catalog includes the DATE-OBS and TIME-OBS keywords then sort
c on these

         iflag = 0
         DO i = 1, ncatcl
            IF ( catcol(i) .EQ. 'DATE-OBS' .OR.
     &           catcol(i) .EQ. 'TIME-OBS' ) iflag = iflag + 1
         ENDDO

         IF ( iflag .EQ. 2 ) THEN
            str4 = 'xsel_sort.tmp'
            CALL xsl_datdir(str4, wrkdir, 0)
            len4 = LENACT(str4)
            comlin = 'fmemsort infile='//str3(1:len3)//' '//
     &               'outfile='//str4(1:len4)//' '//
     &               'columns="DATE-OBS TIME-OBS" '//
     &               'method="shell" '//
     &               'ascend=yes load2mem=yes copyall=yes unique=no '//
     &               'history=yes clobber=yes'
            CALL XSL_WRTCF(ilun, comlin, 1)
            comlin = 'mv -f '//str4(1:len4)//' '//str3(1:len3)
            CALL XSL_WRTCF(ilun, comlin, 1)
         ENDIF

c Close and run the command file

         call XSL_CLCF(ilun)
         call XSL_RUNCF(cmdfil,ECHO,status)
c   Check that the catalogue was made
         if(status.ne.0) then
            call XWRITE
     &           ('Error making catalogue, no catalogue made',5)
            return
C Put in the datadir, etc, keywords:
         else
            keysval(1) = datdir
            keysval(2) = hkdir
            len1 = 2
            keysval(3) = datamode
            if ( mode.eq.0 ) then
               keysval(4) = lststr
               len1 = len1 + 1
            endif
            call XSL_PUTKWST(str3,'1',wrkdir,keysname,keysval,
     &                       len1,4,status)
         endif

c Now filter the catalogue, I have added mission and
c instrument dependent default catalogue selection expressions
c Also come here if the processing version was not recognized
c when the default catalogue was requested.  In that case, I have 
c set catflt to 'NONE'.

         IF(catflt.ne.'NONE'.and.catflt.ne.'none') then
            call XSL_CAT_FILT(str3, obscat(obsno),catflt,ECHO,status)
            if(status.ne.0) then
               str3 = 'Error filtering catalogue with: '
     &              //catflt(1:lenact(catflt))
               call XWRITE(str3,5)
               return
            endif
c Now set the catalogue name to the made catalogue
            catnam = obscat(obsno)         
         ELSE
c If there is no selection, rename the catalogue, and 
c determine the OBJECT, INSTRUME and TELESCOP keywords:
c Note that for mode = 1, there will never be a selection, so we will 
c come here.
            if(mode.eq.0) then
               catnam = obscat(obsno)
            else if (mode.eq.1) then
               catnam = tmpcat
            endif
            IF(str3.ne.catnam) THEN
               call XSL_RENAME(str3,catnam,1,str1,len1,status)
            ENDIF
            call XSL_COL2KEY(incol,outkey,outcom,NKEYS,NKEYS,
     &           catnam,-1,1,1,status)
         ENDIF
c     Set logical MADE 
         
         MADE = .TRUE.
         
c   Now display the catalogue
         IF(SHOWOC) THEN
            call XSL_DUMPCAT(.FALSE.,'-','no')
         ENDIF
      ELSE
         call XWRITE(' MAKE: Option not found ',5)
      ENDIF
      
      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_DATAMODE_SET(mode)
c ---------------------------------------------

      integer mode

      include 'xsel.inc'
      include 'xselvar.inc'

c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(255) str1
c ---------------------------------------------
      character(16) prevmod
      character(255) contxt
      integer LENACT

c If mode = 6, then set to the current value of datamode and don't prompt
c for a new datamode.

         IF ( mode .EQ. 6 ) THEN

            IF ( datamode .EQ. 'NONE' ) GOTO 100

         ELSE

c Check that an instrument has been set - if not tell the user to set one.

            IF ( instru .EQ. 'NONE' ) THEN
               CALL xwrite('You need to set the instrument first', 5)
               RETURN
            ENDIF

            prevmod = datamode
            call xsl_uclgst('set_str',datamode,status)
            IF(status.ne.0) THEN
               call XWRITE('Error getting set_str parameter',5)
               return
            ENDIF

            IF(datamode.eq.'NOT_ENTERED') then

               CALL XSL_MDBS(keymis, ' ', instru, ' ', 'modes', str1,
     &                       status)
               call XWRITE('The available datamodes are:',5)
               CALL XWRITE(str1, 5)
               call xsl_uclgst('set_mode',datamode,status)
               contxt = 'Failed to get instrument parameter'
               IF ( status .NE. 0 ) GOTO 99999

            ELSE

               call XSL_UCLPST('set_mode',datamode,status)
               contxt = 'Failed to put datamode parameter'
               IF ( status .NE. 0 ) GOTO 99999

            ENDIF

         ENDIF

c Identify the datamode

         CALL XSL_MDB_MODE(keymis, submis, instru, datamode, status)
         contxt = 'Cannot find datamode in MDB for '//
     &            keymis(:lenact(keymis))//' '//submis(:lenact(submis))
     &            //' '//instru(:lenact(instru))
         IF ( status .NE. 0 ) GOTO 99999

c Set the MDB info for this datamode

         CALL XSL_MDB_SET()

c special case for ASCA FAST mode

         if(keymis.eq.'ASCA') THEN
            IF(datamode.eq.'FAST')then
               VIMAGE = .FALSE.
               VREGION = .FALSE.
c We need to initialize the value of in_or_out in case the user does 
c not run SELECT FAST.  The value of 'BOTH' does not really imply that,
c more like 'BOTH or EITHER', as the data may have been selected before
c entering this XSELECT session.
               in_or_out = 'BOTH'
            ELSE IF(datamode.eq.'MPC')THEN
c SAEXTRCT only outputs a FITS light curve.
               USEQDP = .FALSE.
               VIMAGE = .FALSE.
               VREGION = .FALSE.
c Otherwise, since a previous MPC mode might have unset it, reset...
            ELSE
               IF(prevmod.eq.'MPC'.or.prevmod.eq.'FAST') THEN
                  CALL XSL_INST_SET(5)
               ENDIF
            ENDIF
         ENDIF
         
c Now reset the prompt

 100     CONTINUE

         prompt = prefix(:LENACT(prefix))
         IF(keymis.ne.'NONE') THEN
            prompt = prompt(:LENACT(prompt))//':' 
     &           //keymis(:LENACT(keymis))
         ENDIF
         IF(instru.ne.'NONE') then
            prompt = prompt(:LENACT(prompt))//'-'//
     &           instru(:LENACT(instru))
         ENDIF          
         IF(datamode.ne.'NONE') then
            prompt = prompt(:LENACT(prompt))//'-'//
     &           datamode(:LENACT(datamode))
         ENDIF          
         prompt = prompt(:LENACT(prompt))//' >'
c
99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i5)') ' XSL_DATAMODE_SET: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF


      return
      end

c
c ---------------------------------------------
      subroutine XSL_GRPSAV(outfile,mode)
c ---------------------------------------------
c Runs the appropriate grouping on the spectral file.  
c J. Ingham 3/94
      include 'xsel.inc'
      include 'xselvar.inc'

      character*(*) outfile
      character(3) old,new
      character(8) kwdnam(4)
      character(512) grpstr, rbnmod, tmpstr, rbnscript
      character(1024) comlin
      real etemp
      integer nchannels,rbnval,ilun,LENACT,stat,len1
      integer lowbnd,statv(4),sn_evtr(4),i,mode,hitrunc,lotrunc
      logical USEGRP,USERBN,GROUP,CLOBBR

c Put the extension on if it isn't present

      if(mode.eq.0) then
         call XSL_XTEND(outfile,'-pha',status)
      else
         call XSL_XTEND(outfile,'pha',status)
      endif

c If there has been any rebinning, we do no grouping

      IF(phabin.ne.1) THEN
         call XSL_COPY(spcfil,outfile,status)
         goto 500
      ENDIF

c Check whether the spectrum needs to be truncated. If so run fselect
c on the spectrum

      status = 0
      CALL XSL_MDBJ(keymis, submis, instru, datamode, 'trunc_spec_hi', 
     &              hitrunc, status)
      IF ( status .NE. 0 ) THEN
         hitrunc = phamax
         status = 0
      ENDIF
      CALL XSL_MDBJ(keymis, submis, instru, datamode, 'trunc_spec_lo', 
     &              lotrunc, status)
      IF ( status .NE. 0 ) THEN
         lotrunc = phamin
         status = 0
      ENDIF

      IF ( hitrunc .LT. phamax .OR. lotrunc .GT. phamin ) THEN
         hitrunc = MIN(hitrunc, phamax)
         lotrunc = MAX(lotrunc, phamin)
         call XSL_RMFILE(cmdfil)
         call XSL_RMFILE(work2(1))
         call XSL_OPCF(cmdfil,ilun)
         comlin = 'fselect '//spcfil(:LENACT(spcfil))//
     &               '+1 '//work2(1)(:LENACT(work2(1)))
         len1 = LENACT(comlin)
         WRITE(comlin(len1+1:),'(a,i8,a,i8,a)') ' "CHANNEL.le.', 
     &             hitrunc, '.and.CHANNEL.ge.',lotrunc,'" '
         len1 = LENACT(comlin)
         comlin(len1+1:) = ' histkw=yes copyall=yes'
         call XSL_WRTCF(ilun,comlin,1)
         write(comlin,'(a,I8,1x,a,a)') 'fparkey ',hitrunc,
     &            work2(1)(:LENACT(work2(1))),
     &            ' TLMAX1 comm=" " add=yes'
         call XSL_WRTCF(ilun,comlin,1)
         write(comlin,'(a,I8,1x,a,a)') 'fparkey ',lotrunc,
     &            work2(1)(:LENACT(work2(1))),
     &            ' TLMIN1 comm=" " add=yes'
         call XSL_WRTCF(ilun,comlin,1)
         write(comlin,'(a,I8,1x,a,a)') 'fparkey ',hitrunc-lotrunc+1,
     &            work2(1)(:LENACT(work2(1))),
     &            ' DETCHANS comm=" " add=yes'
         call XSL_WRTCF(ilun,comlin,1)
         len1 = LENACT(spcfil)
         call XSL_RENAME(work2(1),spcfil,2,comlin,len1,status)
         write(ilun,'(a)') comlin(:len1)
         call XSL_CLCF(ilun)
         call XSL_RUNCF(cmdfil,ECHO,status)
         if(status.eq.0) then
            write(comlin,'(a,I8,a,i8)') 'Truncated the spectrum to ',
     &                          lotrunc, ' - ', hitrunc
            call XWRITE(comlin,5)
         else
            call XWRITE('Error truncating spectral file', 5)
            status = -10
            return
         endif
      ENDIF

c Get the grouping command for the spectrum

      USEGRP = .FALSE.
      CALL XSL_MDBS(keymis, submis, instru, datamode, 'grpstr', 
     &              grpstr, status)
      IF ( status .EQ. 0 .AND. grpstr .NE. 'NONE' ) USEGRP = .TRUE.

c Get the rebinning arguments - number of output channels and compression
c model

      USERBN = .FALSE.
      rbnval = 0
      CALL XSL_MDBJ(keymis, submis, instru, datamode, 'rbnval', 
     &              rbnval, status)
      IF ( status .EQ. 0 ) THEN
         rbnmod = ' '
         CALL XSL_MDBS(keymis, submis, instru, datamode, 'rbnmod', 
     &                 rbnmod, status)
         IF ( status .EQ. 0 ) USERBN = .TRUE.
      ENDIF
      rbnscript = ' '
      CALL XSL_MDBS(keymis, submis, instru, datamode, 'rebinscript', 
     &     rbnscript, status)
      IF ( status .EQ. 0 ) USERBN = .TRUE.
      

c Special case for ASCA - sets the bad channels

      IF ( keymis .eq. 'ASCA' .AND. instru(1:1) .eq. 'S' .AND. 
     &     datamode .ne. 'FAINT' ) THEN

c Get the Sn_EVTRm keywords, look only in the first data file:

         if(instru.eq.'SIS0') then
            kwdnam(1) = 'S0_EVTR0'
            kwdnam(2) = 'S0_EVTR1'
            kwdnam(3) = 'S0_EVTR2'
            kwdnam(4) = 'S0_EVTR3'
         else if(instru.eq.'SIS1') then
            kwdnam(1) = 'S1_EVTR0'
            kwdnam(2) = 'S1_EVTR1'
            kwdnam(3) = 'S1_EVTR2'
            kwdnam(4) = 'S1_EVTR3'
         endif
         call XSL_GETKWI(filenm(1),'0',datdir,kwdnam,
     &           sn_evtr, statv,4,4,.FALSE.,status)
         call XSL_GCOLKWI(spcfil,'1',wrkdir,'CHANNEL','TLMAX',
     &           nchannels,stat,1,1,status)
c The SIS starts at 0.
         nchannels = nchannels + 1
         IF(status.ne.0) THEN
            call XWRITE('Error getting number of '//
     &              'channels from the spectrum file',5)
            write(comlin,'(a,i3)') 'Fitsio error no.: ',
     &              status
            goto 999
         ENDIF
         IF(datamode.eq.'BRIGHT'.or.datamode.eq.'FAST'
     &           .or.datamode.eq.'NONE') THEN
c Check that the rbnval is valid:
            stat = nchannels/2
            DO WHILE (rbnval.gt.stat
     &                .or.mod(stat-rbnval,2).ne.0)
               write(comlin,'(a,i4,a,i4,a)') 
     &           'The final number of channels must be < ', stat, 
     &           ', and mod(', stat, ' - final # of chans.,2) = 0'
               call XWRITE(comlin,5)
               call XWRITE('Or type 0 to quit',5)
               new = 'q'
c This is a hidden parameter, so we must unhide it:
               call xpiparmode('rbnval',new,old,status)
               call xsl_uclgsi('rbnval',rbnval,status)
               call xpiparmode('rbnval',old,new,status)
               IF ( status .NE. 0 ) GOTO 999
               if ( rbnval.le.0 ) GOTO 999
            ENDDO
                     
            lowbnd = 0
            do i=1,4
               IF(statv(i).eq.0) THEN
                  lowbnd = max(lowbnd,sn_evtr(i))
               ENDIF
            enddo
c Find out where the lowbnd falls in the non-linearly rebinned data:
            IF(lowbnd.le.nchannels/2) THEN
               etemp = float(lowbnd * rbnval)
     &               /float(2*nchannels)
            ELSE IF(lowbnd.le.(3*nchannels)/4) THEN
               etemp = float(lowbnd * rbnval)
     &                 /float(nchannels)
            ELSE
               etemp = float(2 * lowbnd * rbnval)
     &                 /float(nchannels)
            ENDIF
            lowbnd = int(etemp)
                     
            IF(etemp-lowbnd.gt.1.0e-7) THEN
               lowbnd = lowbnd + 1
            ENDIF

c NB the following code contained a typo so never had any effect...
c
c RBNPHA now generates a file that starts with channel 0 so replace
c 1-lowbnd by 0-(lowbnd-1)    kaa 11/30/96
c
c            lowbin = lowbnd - 1

         ELSE IF(datamode.eq.'BRIGHT2') THEN
c Check that rbnval < nchannels:
            DO WHILE ( rbnval .GT. nchannels )
               write(comlin,'(a,i5,a,i5)') 'Your rebinning value: ', 
     &          rbnval, ' is greater than the number of channels: ',
     &          nchannels
               call XWRITE(comlin,5)
               call XWRITE
     &                 ('Enter another value ( 0 to quit )',5)
               call xsl_uclgsi('rbnval', rbnval, status)
               if ( status .ne. 0 ) goto 999
               if ( rbnval .le. 0 ) goto 999
            enddo

c LOWBND is the maximum of the lowbounds found in the file                     
            lowbnd = 0
            do i=1,4
               IF(statv(i).eq.0) THEN
                  lowbnd = max(lowbnd,sn_evtr(i))
               ENDIF
            enddo
            etemp = float(lowbnd * rbnval)
     &              /float(nchannels)
            lowbnd = int(etemp)
                     
            IF(etemp-lowbnd.gt.1.0e-7) THEN
               lowbnd = lowbnd + 1
            ENDIF
         ENDIF

         write(grpstr,'(a,i4,a)') 'bad 0 -', lowbnd, 
     &                            '&show quality&exit'
         USEGRP = .TRUE.


      ENDIF

c Special case for ASCA GIS - don't do the grouping if the number of
c channels is < 1024. Set bad channels for 0-60/(1024/(nchannels+1)).

      IF (keymis .EQ. 'ASCA' .AND. instru(1:1).eq.'G') THEN
         call XSL_GCOLKWI(spcfil,'1',wrkdir,'CHANNEL','TLMAX',
     &        nchannels,stat,1,1,status)
         IF ( status .ne. 0 ) THEN
            call XWRITE('Error getting number of channels from'//
     &           ' the spectrum file',5)
            write(comlin,'(a,i3)') 'Fitsio error no.: ',status
            status = 0
            USEGRP = .FALSE.
            USERBN = .FALSE.
         ELSEIF ( nchannels .NE. 1023 ) THEN
            write(grpstr,'(a,i5,a)') 'bad 0 -', 
     &        INT(60/(1024/(nchannels+1))+0.9)-1, '&exit'
         ELSE
            tmpstr = grpstr
            write(grpstr,'(a,a)') 'bad 0 - 59&', 
     &        tmpstr(:lenact(tmpstr))
         ENDIF
      ENDIF

c Find out what the user wants to do if the group or rebin flags are set

      IF ( USEGRP .OR. USERBN ) THEN
         call XSL_UCLGOT('group_spectrum',status)
         IF ( status .ne. 0 ) THEN
            status = 0
            IF ( USERBN ) THEN
               IF ( rbnval .GT. 0 ) THEN
                  WRITE(comlin, '(a,i5,a)') 
     &             'The data will be rebinned to ', rbnval, ' channels'
                  CALL xwrite(comlin, 5)
                  comlin = 'using compression mode '//
     &                 rbnmod(:LENACT(rbnmod))
                  CALL xwrite(comlin, 5)
               ELSE
                  WRITE(comlin, '(a,i5,a)') 
     &             'The data will be rebinned using '//
     &             rbnscript(:LENACT(rbnscript))
                  CALL xwrite(comlin, 5)
               ENDIF
            ENDIF
            IF ( USEGRP ) THEN
               call XWRITE
     &      ('The data will be grouped using the following command:',5)
               call XWRITE(' ',5)
               call XWRITE(grpstr,5)
               call XWRITE(' ',5)
            ENDIF
         ENDIF
         call xsl_uclgsb('group_spectrum',GROUP,status)
         IF ( status .NE. 0 ) RETURN
         IF ( .NOT.GROUP ) THEN
            USEGRP = .FALSE.
            USERBN = .FALSE.
         ENDIF
      ENDIF

c Check for the existence of the output file:

      if (outfile(1:1) .eq. '!' ) then
         outfile = outfile(2:)
         call XSL_EXIST(outfile,status)
         if(status.EQ.0) THEN
            call XSL_RMFILE(outfile)
         endif
      else
         call XSL_EXIST(outfile,status)
         if ( status .eq. 0 ) then
            call xsl_uclgsb('clobberit',CLOBBR,status)
            IF ( status .NE. 0 ) RETURN
            IF(.not.CLOBBR) THEN
               status = -10
               goto 999
            ELSE
               call XSL_RMFILE(outfile)
            ENDIF
         endif
      endif

c If no rebinning or grouping then just copy the temporary spectrum to
c its final resting place

      IF ( .NOT.USERBN .AND. .NOT.USEGRP ) THEN

         call XSL_COPY(spcfil,outfile,status)

      ELSE

         call XSL_OPCF(cmdfil,ilun)

c Rebinning only...

         IF ( USERBN .and. .NOT.USEGRP ) THEN

            IF ( rbnval .GT. 0 ) THEN
               write(comlin,'(5a,i5,3a)') 'rbnpha infile=', 
     &           spcfil(:LENACT(spcfil)), ' outfile=',
     &           outfile(:LENACT(outfile)),' finchan=', rbnval,
     &           ' cmpmode=', rbnmod(:LENACT(rbnmod)), ' chatter=0'
            ELSE
               write(comlin,'(3(a,1x))') rbnscript(:LENACT(rbnscript)),
     &           spcfil(:LENACT(spcfil)), outfile(:LENACT(outfile))
               IF ( ECHO ) comlin(lenact(comlin)+1:) = ' yes'
            ENDIF

c Rebinning and grouping...

         ELSEIF ( USERBN .and. USEGRP ) THEN

            call XSL_RMFILE(tempfl)
            IF ( rbnval .GT. 0 ) THEN
               write(comlin,'(5a,i5,3a)') 'rbnpha infile=', 
     &           spcfil(:LENACT(spcfil)), ' outfile=',
     &           tempfl(:LENACT(tempfl)),' finchan=', rbnval,
     &           ' cmpmode=', rbnmod(:LENACT(rbnmod)), ' chatter=0'
            ELSE
               write(comlin,'(3(a,1x))') rbnscript(:LENACT(rbnscript)),
     &           spcfil(:LENACT(spcfil)), tempfl(:LENACT(tempfl))
               IF ( ECHO ) comlin(lenact(comlin)+1:) = ' yes'
            ENDIF
            call XSL_WRTCF(ilun,comlin,1)
            comlin='grppha infile = '//tempfl(:LENACT(tempfl))//' '//
     &        'outfile='//outfile(:LENACT(outfile))//' '//
     &        'comm="'//grpstr(:LENACT(grpstr))//
     &        '" chatter=0'

c Grouping only...

         ELSEIF ( USEGRP ) THEN

            comlin = 'grppha infile='//spcfil(:LENACT(spcfil))//' '//
     &        'outfile='//outfile(:LENACT(outfile))//' '//
     &        'comm="'//grpstr(:LENACT(grpstr))//
     &        '" chatter=0'

         ENDIF

         call XSL_WRTCF(ilun,comlin,1)
         call XSL_CLCF(ilun)
         call XSL_RUNCF(cmdfil,ECHO,status)

      ENDIF

 500  CONTINUE

c special processing for XMM EPIC spectrum to add BADPIX and EXPOSU extensions

      IF ( keymis .EQ. 'XMM' .AND. instru(1:4) .EQ. 'EMOS' )
     &  CALL XSL_XMM_SPEC(outfile)

c run response generation if required

      comlin = 'Wrote spectrum to '//outfile
      call XWRITE (comlin,5)
      call XSL_RSPSAV(outfile)
      RETURN
      
c come here from a failure

 999  CONTINUE
      comlin = 'Failed to write spectrum to '//outfile
      call XWRITE (comlin,5)
      RETURN

      end
c
c
c ---------------------------------------------
      subroutine XSL_CHOOSE()
c ---------------------------------------------
c Will select observations by OBSCAT directory number.
c
c Called by XSELECT main
c
c     Alan Smale 1993 February

      IMPLICIT NONE

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line string:
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(512) str1, str2, str3
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2, len3, len4
c ---------------------------------------------
      character(255) buffer,namvec(MAXFIL),hkext
      character(8) tform,tformval
      character(64) thismod
      character(256) tmpmod
      real buffer1(2,500)
      real rmin, rmax

      integer ibuf(2,500),vecint(MAXFIL),myindex(MAXFIL)
      integer nr, Nchoo, tmpstat
      integer i, j, k, nadded,diflen,extno,modcno,tdelcno
      integer rxbcno,tmprxb,ribcno,phbcno,tmpphb,tmprib
      double precision tmpdel
c      integer ilist(500)

      character(512) context
      integer rwmode, block, htype

      integer LENACT
      logical  anynul,QUIET,CLEAR,ISRANGE

      Nchoo=500
      status=0

      nr=20
      rmin = -100.
      rmax = 100000.
      status = 0
      IF(REGION.or.ASCTFL.or.FFTFL.or.XWNTFL.or.FITTFL)THEN
c First clear the read data, so choose starts from an empty list:
c Prompt for clear filters:
         call xsl_uclgsb('choose_clear',CLEAR,status)
         IF(status.ne.0) THEN
            call XWRITE('Error getting the choose_clear '//
     &           'parameter.',5)
            status = 0
            return
         ENDIF
         IF(CLEAR) THEN
c Clear all, but don't reset the instrument or plot device:

            str1 = instru
            str2 = plotdv
            str3 = prompt
            call XSL_CLEAR(1)
            instru = str1(:min(lenact(str1),len(instru)))
            plotdv = str2(:min(lenact(str2),len(plotdv)))
            prompt = str3(:min(lenact(str3),len(prompt)))
         ELSE
c Just clear the data:
            call XSL_CLEAR(2)
         ENDIF
      ELSE
         call XSL_CLEAR(2)
      ENDIF

c Get the quiet parameter
      call xsl_uclgsb('quiet',QUIET,status)
      IF(status.ne.0) THEN
         call  XWRITE('Error getting the quiet parameter',5)
         status = 0
         return
      ENDIF
c First, check to see that the obscat exists. (Note that it
c may have been made in a previous run of XSELECT.) If it
c exists (ierr=0), proceed with the CHOOSEing. Otherwise, complain
c and don't do anything.
      IF(.NOT.MADE.AND. .NOT. LOADED) then
          call XSL_MDB_INST(keymis,submis,instru,obsno,status)
          if(status .NE. 0 .OR. obsno.lt.1) return
          call XSL_EXIST(obscat(obsno),ierr)
      
          IF(ierr.eq.0)then
             MADE = .TRUE.
             catnam = obscat(obsno)
c If no READ has yet taken place, mission keywords must be set.
      
         ELSE
            call XWRITE('Obscat not found: use MAKE OBSCAT ',5)
            return
         ENDIF
      ENDIF

c CHOOSE RANGES WITHIN OBSCAT **
      call xsl_uclgst('choose_which',buffer,status)

      IF(status.ne.0) THEN
         call XWRITE('Error getting the choose_which parameter',5)
         status = 0
         return
      ENDIF
c Now see whether this is a range or a selection expression:

      call XSL_ISRANGE(buffer,ISRANGE)

      if(.NOT.ISRANGE) then
         choflt = buffer 
         len2 = LENACT(catnam)
         call XSL_DATDIR(chocat,wrkdir,0)
         len3 = LENACT(chocat)
         call XSL_RMFILE(chocat)
         len4 = LENACT(choflt)
         comlin = 'fselect '//catnam(1:len2)//' '//
     &           chocat(1:len3)//' expr="'//choflt(1:len4)//'" '//
     &           'histkw=no copyall=yes'
         call XSL_OPCF(cmdfil,ilun)
         call XSL_WRTCF(ilun,comlin,1)
         call XSL_CLCF(ilun)

         call XSL_RUNCF(cmdfil,ECHO,status)
         if(status.ne.0) then
            call XWRITE('Error filtering catalogue with: '
     &                                         //choflt(1:len4),5)
            return
         endif 

c read in contents of OBSCAT, or the temporary filtered obscat. 
c Note that each entry takes up two lines.
         
         call GETLUN(ilun)

         rwmode = 0
         call FTOPEN(ilun, chocat, rwmode, block, status)
      ELSE
         call XSL_UCLGSGPARSE(buffer,buffer1,Nchoo,rmin,rmax,nr,status)

         call GETLUN(ilun)

         rwmode = 0
         call FTOPEN(ilun, catnam, rwmode, block, status)
      ENDIF


      if (status .gt. 0) then
         context='10. Unable to open FITS file '//catnam
         call XWRITE(context,5)
         status = 0
         call FRELUN(ilun)
         return 
      endif

c go to first extension
      call FTMRHD(ilun, 1, htype, status)
      if(status .gt. 0) then
         context='Unable to move to ext#1 in '//catnam
         call XWRITE(context,5)
         status=0
         return 
      endif
         
c Reset the data and HK directories from the obscat:
      call FTGKYS(ilun,'DATADIR',datdir,str1,status)
      if(status.ne.0.and.datdir.eq.'NONE') then
         status = 0
         call XWRITE('DATADIR keyword not in catalogue',5)
         datdir = wrkdir
      endif
      call FTGKYS(ilun,'HKDIR',hkdir,str1,status)
      if(status.ne.0.and.hkdir.eq.'NONE') then
         status = 0
         call XWRITE('HKDIR keyword not in catalogue',5)
         hkdir = datdir 
      endif
c Now get the datamode keyword      
      tmpmod = 'NONE'
      call FTGKYS(ilun,'datamode',tmpmod,str1,status)
c Not all catalogues contain datamode!
      if(status.ne.0) then
         tmpmod = 'NONE'
         status = 0
      endif
c If there is more than one datamode in the obscat, see if they
c choose only one, set it to NONE for now...
      if(index(tmpmod,'::').ne.0) tmpmod = 'NONE'

c Now get the number of entries in the catalogue
      call FTGKYJ(ilun,'NAXIS2',nincat,str1,status)
      IF(status.ne.0) then
         context = 'Could not get NAXIS2 keyword from '
     &                                       //catnam
         call XWRITE(context,5)
         status = 0
         return 
      ENDIF

c Now process the input ranges:
c Put contents of BUFFER1 into IBUF

      IF(ISRANGE) then
         do i=1,nr
            if( buffer1(2,i).eq.rmax )then
               buffer1(2,i) = nincat
            endif
            if( buffer1(1,i).eq.rmin)then
               buffer1(1,i) = 1
            endif

            ibuf(1,i) = INT( buffer1(1,i) )
            ibuf(2,i) = INT( buffer1(2,i) )
         end do
         
c Check that the entries are legitimate:
         do i=1,nr 
            IF(ibuf(2,i).gt.nincat) THEN
               write(context,240) nincat
240            format('Selection out of bounds, only ',I3,
     &                         'entries in the catalogue')
               return
            ENDIF
         enddo

      ELSE
         nr = 1
         ibuf(1,1) = 1
         ibuf(2,1) = nincat
      ENDIF

c NOW we start to read in the files from the OBSCAT:
      
c If there is a datamode keyword, don't bother with the column!   
c Otherwise get its number:
      if(tmpmod.eq.'NONE') THEN
         call FTGCNO(ilun,.FALSE.,'DATAMODE',modcno,status)

c If there is no datamode column, set all to 'NONE'
         if(status.eq.219) THEN
            tmpmod = 'NONE'
            status = 0
c This is the flag not to look for the column
            modcno = -1
         else if (status.ne.0) then
            call XWRITE
     &           ('Error reading datamode column from obscat',5)
            write(str1,'(''Fitsio Error No.'',I4)') status
            return
         endif
      else
c This is the flag not to look for the column
         modcno = -1
      endif

c Now get the timedel column number

      call FTGCNO(ilun,.FALSE.,'TIMEDEL',tdelcno,status)
c Old Catalogues won't have TIMEDEL in them...
      if(status.eq.219) then
         status = 0
c tdelcno = -1 will be the flag that no timedel info is in the obscat...
         tdelcno = -1
      else if (status.ne.0) then
         call XWRITE
     &        ('Error reading TIMEDEL column from obscat',5)
         write(str1,'(''Fitsio Error No.'',I4)') status
         return
      else
         call FTKEYN('TFORM',tdelcno,tform,status)
         call FTGKYS(ilun,tform,tformval,str1,status)
C If the keyword was never found, then it will be an A type, not E.
         if ( index(tformval,'A').ne.0) then
            tdelcno = -1
         endif
      endif

c Now get the RAWXBINS column number
      status = 0
      call FTGCNO(ilun,.FALSE.,'RAWXBINS',rxbcno,status)
c Old Catalogues won't have RAWXBINS in them...
      if(status.eq.219) then
         status = 0
c rxbcno = -1 will be the flag that no rawxbins info is in the obscat...
         rxbcno = -1
      else if (status.ne.0) then
         call XWRITE
     &        ('Error reading RAWXBINS column from obscat',5)
         write(str1,'(''Fitsio Error No.'',I4)') status
         return
      else
         call FTKEYN('TFORM',rxbcno,tform,status)
         call FTGKYS(ilun,tform,tformval,str1,status)
C If the keyword was never found, then it will be an A type, not E.
         if ( index(tformval,'A').ne.0) then
            rxbcno = -1
         endif
      endif

c Now get the PHA_BINS column number
      status = 0
      call FTGCNO(ilun,.FALSE.,'PHA_BINS',phbcno,status)
c Old Catalogues won't have PHA_BINS in them...
      if(status.eq.219) then
         status = 0
c phacno = -1 will be the flag that no phabins info is in the obscat...
         phbcno = -1
      else if (status.ne.0) then
         call XWRITE
     &        ('Error reading PHA_BINS column from obscat',5)
         write(str1,'(''Fitsio Error No.'',I4)') status
         return
      else
         call FTKEYN('TFORM',phbcno,tform,status)
         call FTGKYS(ilun,tform,tformval,str1,status)
C If the keyword was never found, then it will be an A type, not E.
         if ( index(tformval,'A').ne.0) then
            phbcno = -1
         endif
      endif


c Now get the RISEBINS column number
      status = 0
      call FTGCNO(ilun,.FALSE.,'RISEBINS',ribcno,status)
c Old Catalogues won't have RISEBINS in them...
      if(status.eq.219) then
         status = 0
c rsbcno = -1 will be the flag that no phabins info is in the obscat...
         ribcno = -1
      else if (status.ne.0) then
         call XWRITE
     &        ('Error reading RISEBINS column from obscat',5)
         write(str1,'(''Fitsio Error No.'',I4)') status
         return
      else
         call FTKEYN('TFORM',ribcno,tform,status)
         call FTGKYS(ilun,tform,tformval,str1,status)
C If the keyword was never found, then it will be an A type, not E.
         if ( index(tformval,'A').ne.0) then
            ribcno = -1
         endif
      endif

C Now get the desired info out of the obscat, file by file:

C     First initialize some stuff:
      k = 0
      timedel = -1.0
      rxbval = 0
      ribval = -1
      phbval = -1
      
      do i = 1,nr
         do j = ibuf(1,i),ibuf(2,i)
            k = k + 1      
c Get the file names:
            call FTGCVS(ilun,1,j,1,1,' ',namvec(k),anynul,status)
c vecint contains the index in the catalogue.
            vecint(k) = j

            if(modcno.gt.0)then
c Get the datamode values:
               call FTGCVS(ilun,modcno,j,1,1,' ',
     &              thismod,anynul,status)
               if(tmpmod.eq.'NONE') then
                  tmpmod = thismod
               else if(index(tmpmod,thismod(:LENACT(thismod)))
     &                 .eq.0) then
                  tmpmod = tmpmod(:LENACT(tmpmod))//
     &                 '::'//thismod
               endif
            endif

            if(tdelcno .gt. 0.) then
c Get the timedel values
               call FTGCVD(ilun,tdelcno,j,1,1,-1.d0,
     &              tmpdel,anynul,status)
               if(.NOT.anynul) then
                  timedel = max(tmpdel,timedel)
               endif
            endif

c Get the RISE_BINS value:
            if(ribcno.gt.0) then               
               call FTGCVJ(ilun,ribcno,j,1,1,-999,
     &              tmprib,anynul,status)

C If first time through, set ribval, otherwise get minimum.  All we really
c care about is whether a 0 shows up here.  Then GISCLEAN is out...
               if(ribval.eq.-1) then
                  ribval = tmprib
               else if (.NOT.anynul) then
                  ribval = min(ribval,tmprib)
               endif
            endif

c Get the PHA_BINS values:
            if(phbcno.gt.0) then
               call FTGCVJ(ilun,phbcno,j,1,1,-999,
     &              tmpphb,anynul,status)
               if(phbval.eq.-1) then
                  phbval = tmpphb
               else if ((.NOT.anynul).and.(phbval.ne.tmpphb)) then
c If there are several values of pha_bins, then you cannot use PHA_CUTOFF,
c nor can you make a spectrum.
                  VSPEC = .FALSE.
                  VPHACUT = .FALSE.
               endif
            endif
c Get the RAWXBINS keyword
            if(rxbcno.gt.0) then
               call FTGCVJ(ilun,rxbcno,j,1,1,-999,
     &              tmprxb,anynul,status)
               if(rxbval .eq. 0) then
                  rxbval = tmprxb
               else if (rxbval.ne.tmprxb.and.VIMAGE) then
                  call XWRITE('There is more than one RAWXBINS '//
     &                 'value in the data you have chosen.',5)
                  write(str1,51) rxbval,tmprxb
 51               format('Got: ',i4,' and ',i4)
                  call XWRITE(str1,5)
c If there are several values of RAWXBINS, then you cannot use region
c filters, nor can you make an image.
                  VREGION = .FALSE.
                  VIMAGE = .FALSE.
               endif
            endif

c Get the TIMESYS and MJDREFI/F values from the event filenames

            tmpstat = 0
            status = 0
            CALL XSL_GETKWST(namvec(i),'0',datdir,'TIMESYS',timesys,
     &                       tmpstat,1,1,.TRUE.,status)

            mjdrefi = 0
            mjdreff = 0.0d0
            call XSL_GETKWD(namvec(i),'0',datdir,'MJDREF',
     &                      mjdreff,tmpstat,1,1,.TRUE.,status)
            IF ( tmpstat .EQ. 0 .AND. status .EQ. 0 ) THEN
               mjdrefi = INT(mjdreff)
               mjdreff = mjdreff - mjdrefi
            ELSE
               tmpstat = 0
               status = 0
               call XSL_GETKWD(namvec(i),'0',datdir,'MJDREFF',
     &                         mjdreff,tmpstat,1,1,.TRUE.,status)
               call XSL_GETKWI(namvec(i),'0',datdir,'MJDREFI',
     &                      mjdrefi,tmpstat,1,1,.TRUE.,status)
               IF ( tmpstat .NE. 0 .OR. status .NE. 0 ) THEN
                  CALL xwrite('No MJDREF(F/I) keyword(s) found',5)
                  tmpstat = 0
                  status = 0
               ENDIF
            ENDIF
            
         enddo
      enddo


      status = 0
      call FTCLOS(ilun,status)
      call FRELUN(ilun)

c Check the datamode:

      if(index(tmpmod,'::').ne.0) then
         call XWRITE
     &        ('   More than 1 datamode in the chosen data',5)
         str1 = '     Found: '//tmpmod
         call XWRITE(str1,5)
         call XWRITE('No files added',5)
         call XWRITE(' ',5)
         status = -20
         return
      else 
         datamode = tmpmod(:min(lenact(tmpmod),len(datamode)))
         IF(datamode.ne.'NONE') THEN
            str1 = '   Setting datamode to '//datamode
            call XWRITE(' ',5)
            call XWRITE(str1,5)
            call XWRITE(' ',5)
            call XSL_SET(6)
         ENDIF
      endif

c Now add them to the FILENM list
      call XSL_RDVECT(namvec,k,nfiles,filenm,myindex,nadded,
     &                                       datdir,0,.FALSE.) 

c Now set up the file index variable:
      do i=1,nfiles
         catidx(i) = vecint(myindex(i))
      enddo

c Set the flags to show a READ has taken place.
      if(nadded.eq.0) then
         call XWRITE('No files added',5)
         status = -20
         return
      endif
         
c check the TIMEDEL against binsize, and the phase filter:
      if (tdelcno.gt.0) then
C The TIMEDEL for FAST mode data is not correctly reported in the 
c input files.  Should be 16/1024.  Reset it here.
         if(keymis.eq.'ASCA'.and.datamode.eq.'FAST') then
            timedel = 1.0d0/64.0d0
         endif
         if(timedel.gt.0) then
            write(str1,551)timedel
 551        format('Got the minimum time resolution of the ',
     &           'chosen data: ',e12.5,',')
            call XWRITE(str1,5)
            if(timedel.gt.binsiz) then
               write(str1,552) binsiz
 552           format('This is greater than the chosen binsize: ',
     &              e12.5)
               call XWRITE(str1,5)
               write(str1,553) timedel
 553           format('Setting binsize =  ',
     &              e12.5) 
               call XWRITE(str1,5)
               binsiz = SNGL(timedel)
            endif
c If there is a phase filter already, check it.
            if(XPHTFL) then
               call XSL_CHKPHASE()
               if(status.ne.0) then
                  call XWRITE('Removing phase filter.',5)
                  XPHTFL = .FALSE.
               endif
            endif
         else
            call XWRITE('Could not get the minimum time '//
     &                  'resolution of the chosen data.',5)
         endif
      endif

C Check the RAWXBINS value, set the WMAP binning:
      if(rxbcno.gt.0) then
         if(rxbval .eq. -999 ) then
C This is the case when there is no RAWXBINS in a ASCA GIS file.
            continue
         else if(VIMAGE.and.rxbval.lt.256) then
            write(str1,555) rxbval
 555        format('Got RAWXBINS = ',i4,', setting WMAP binning to 1')
            call XWRITE(str1,5)
            extrbinh = 1
         else if(.NOT.VREGION .or. .NOT.VIMAGE) THEN
            call XWRITE('WARNING: Your chosen files contain '//
     &           'diverse X binnings. ',5)
            call XWRITE('You cannot use FILTER REGION'//
     &           ', or EXTRACT IMAGE.',5)
c Turn off the weighted map...
            WTMAPB = .FALSE.
         endif
      endif

      if(ribcno.ge.0) then
         if(ribval.le.1) then
            call XWRITE('WARNING: Some of your input files '//
     &           'have no RISE TIME information.',5)
            call XWRITE('You cannot use GISCLEAN.',5)
            VGISCLEAN = .FALSE.
         endif
      endif

      if(phbcno.ge.0) then
         if(.NOT.VSPEC) then
           call XWRITE('WARNING: Your chosen files contain '//
     &           'diverse PHA binnings, ',5)
            call XWRITE('You cannot use FILTER PHA_CUTOFF, or '//
     &          'EXTRACT SPECTRUM.',5)
         endif
      endif
      

      if(nfiles.gt.0)then
         call XSL_GET_EXTNO(filenm(1),datdir,evtnam,extno,evtnum,status)
         if(status.ne.0) then
            str1 = 'Could not find the event extension: '//
     &           evtnam(:LENACT(evtnam))//' in the input files'
            call XWRITE(str1,5)
            nfiles = 0
            READ = .FALSE.
         endif
      endif

c Now look in the files to get the TLMIN and TLMAX for the chosen
c energy column, and set the phamin and phamax to this:

      call XSL_CHKPHASIZE(0)

      
      if(nfiles.gt.0) THEN
         READ = .TRUE.
c If there were any files added, we need to reset merged
         WORK = .FALSE.
         FILTER = .FALSE.
c Do the FAST mode setup, if necessary
         IF(keymis.eq.'ASCA'.and.datamode.eq.'FAST') THEN
c This is a hack to get around the fact that when you use a selection
c expression in choose, the catidx refers to the temporary file chocat,
c not the actual loaded catalogue.
            if(ISRANGE) THEN
               call XSL_FAST_SETUP(catnam,instru,in_or_out,ccdno,arena,
     &           ario,stah,endh,catidx,nfiles,MAXFIL,status)
            else
               call XSL_FAST_SETUP(chocat,instru,in_or_out,ccdno,arena,
     &           ario,stah,endh,catidx,nfiles,MAXFIL,status)
            endif
         ENDIF      
      ELSE
         call XWRITE('No files added',5)
         status = -20
         return
      ENDIF
      if(nfiles.gt.1) THEN
         MANY = .TRUE.
c If there were any files added, we need to reset merged
         MERGED = .FALSE.
      ENDIF

c Echo back # of files
      call XWRITE(' ',5)
      write(str1,50) nfiles
 50   format('Number of files read in: ',i4)
      call XWRITE(str1,5)

c Now add in the HK and GTI files.  Only the default mode is available.  
c The data directory has already been added to filenm, so need not be 
c done again
c First the GTI files:

      IF ( gtinam .NE. 'NONE' ) THEN

         call XSL_GET_EXTNO(filenm(1),datdir,gtinam,extno,gtinum,status)

c if we can't find gtinam then check for both stdgti and gti

         if(status.ne.0) then
            status = 0
            call XSL_GET_EXTNO(filenm(1),datdir,'STDGTI',
     &                         extno,gtinum,status)
         endif
         if(status.ne.0) then
            status = 0
            call XSL_GET_EXTNO(filenm(1),datdir,'GTI',
     &                         extno,gtinum,status)
         endif

         if(status.ne.0) then
            str1 = 'Could not find the gti extension: '//
     &             gtinam(:LENACT(gtinam))//' in the input files'
            call XWRITE(str1,5)
            nfiles = 0
         endif

         do i=1,nfiles
            str1 = filenm(i)
            len1 = LENACT(str1)
            gtifnm(i) = str1(1:len1)//'+'//gtinum
         enddo
         if(nfiles.gt.0) then
            USEGTI = .TRUE.
            MERGTI = .FALSE.
            WRKGTI = .FALSE.
         endif
      ENDIF
    
c Next add the HK files, don't overwrite the previous ones however,
c since then read can be used to read in ones with other than the default
c HK files.
c      IF(nadded.lt.nfiles.and. .not. HKREAD) then
c         write(str1,105) nfiles-nadded
c105      format('There are no HK files for the first ',
c    &                   I2,' event files')                
c         call XWRITE(str1,5)
c        call XWRITE('To use HK files, clear and start over',5)
c         HKREAD = .FALSE.
c      ELSE
          call xsl_uclgst('hkext',hkext,status)
          IF ( status .NE. 0 ) RETURN
          call xsl_uclgsi('hkdifflen',diflen,status)
          IF ( status .NE. 0 ) RETURN
          do i=1,nadded
            str1 = filenm(i+nfiles-nadded)
            len1 = LENACT(str1)
            namvec(i) = str1(1:len1-diflen)//hkext
          enddo
C now add them to the list:
          len2 = LENACT(datdir) + 1
          len3 = 0
          call XSL_RDVECT(namvec,nadded,len3,hkflnm,myindex,
     &                                   len1,hkdir,len2,QUIET)
          If(nadded.ne.len1) then
c             call XWRITE('HK files were not found'//
c     &                                ' for some data files',5)
c             call XWRITE('To use HK files, clear and start over',
c     &            5)
             HKREAD = .FALSE.
             MANYHK = .FALSE.
          else          
             HKREAD = .TRUE.
             IF(nfiles.gt.1) MANYHK = .TRUE.
             MERGHK = .FALSE.
             nhkfil = nfiles
             WORKHK = .FALSE.
          endif
c      ENDIF 

c Do a 'show data'. Put an extra line to separate the error
c messages from the real stuff.

      IF ( READ )then
         call XWRITE('    ',5)
         call XWRITE('Files currently in use: ',5)
         call XWRITE(' ',5)
c         str1 = 'Datadir: '//datdir(:LENACT(datdir))
c         call XWRITE(str1,5)
         len2 = LEN(str1)-3
         do i=1,NFILES
            len1 = LENACT( filenm(i) )
            call XSL_FILENAME(filenm(i),str1,len1)
            write(str2,55) catidx(i), str1(:len1)
 55         format(i4,'   ',a)
            call XWRITE(str2,5)
         end do
         call XWRITE(' ',5)
      ELSE
         call XWRITE(' No data has been read in yet.',5)
      ENDIF

      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_CHKPHASIZE(fileid)
c ---------------------------------------------
c This checks the PHA size keywords for the keypha column.  It returns the
c max and min in phamax and phamin.
c fileid tells whether to look at the file space (0) or the workspace(1).

      include 'xsel.inc'
      include 'xselvar.inc'

      integer phalimits(2),statv(2),i,j,LENACT,fileid
      character(512) str1
      character(8) phalnames(2)
      logical SWITCHPI

c punt on several special cases (there ought to be a better way of handling
c this)

      IF ( keymis .EQ. 'XTE' .OR.
     &    (keymis .EQ. 'ASCA' .AND. datamode .EQ. 'FAINT') .OR. 
     &    (keymis .EQ. 'ASCA' .AND. datamode .EQ. 'MPC' ) .OR.
     &    (keymis .EQ. 'EINSTEIN' .AND. instru .EQ. 'MPC') ) RETURN

      IF ( keypha .EQ. 'NONE' ) RETURN

      phalnames(1) = 'TLMIN'
      phalnames(2) = 'TLMAX'
      phamin = 100000
      phamax = 0
      status = 0
      SWITCHPI = .TRUE.

      if(fileid.eq.0) then
         call XWRITE(' ',5)
         call XWRITE('Getting Min and Max for Energy Column...',5)
      endif

 80   i = 1
      do while ( i .le. nfiles )
         if(fileid.eq.0) then
            call XSL_GCOLKWI(filenm(i),evtnum,datdir,keypha,phalnames,
     &        phalimits,statv,2,2,status)
         else
            call XSL_GCOLKWI(work1(i),'1',wrkdir,keypha,phalnames,
     &        phalimits,statv,2,2,status)
         endif
c If there is no TLMAX for the chosen column, then try PHAMAX...
         if ( status.ne.0 .or. statv(1).ne.0.or.statv(2).ne.0) then
            status = 0
            if(fileid.eq.0) then
               call XSL_GETKWI(filenm(i),evtnum,datdir,phamxkwd,
     &           phalimits(2),statv(2),1,1,.FALSE.,status)
            else
               call XSL_GETKWI(work1(i),evtnum,wrkdir,phamxkwd,
     &           phalimits(2),statv(2),1,1,.FALSE.,status)
            endif
c If not this, bag the file...
            if(status.ne.0.or.statv(2).ne.0) then
               call XWRITE(' ',5)
               str1 = 'Could not find size keywords for column '
     &              //keypha(:LENACT(keypha))//
     &              ' for file:'
               call XWRITE(str1,5)
               if(fileid.eq.0) then
                  str1 = '   '//filenm(i)(:LENACT(filenm(i)))
               else
                  str1 = '   '//work1(i)(:LENACT(work1(i)))
               endif
               call XWRITE(str1,5)
               str1 = 'Tried TLMIN, TLMAX and '//phamxkwd
               call XWRITE(str1,5)
               if(fileid.eq.0) then
C
C A crude hack: The SIS default keypha -> PI in version 1.3
C but there are some old files with unfilled PI columns.
C This will offer to switch back to PHA if such is the case.
C Only check the first file, since if the PI column is not filled
C it will show up right away.
C
                  IF ( keymis(1:4) .eq.'ASCA' .and.
     &                 instru(1:3).eq.'SIS' .and.
     &                 keypha(1:2).eq.'PI' .and.
     &                 i .eq. 1 .and.
     &                 SWITCHPI) then
                     call XWRITE('This could be because your '//
     &                    'files have not had the PI column filled,',5)
                     call XWRITE('Or the problem could be with '//
     &                    'one file.',5)
                     call XWRITE('If you suspect the former, you'//
     &                    ' will need to run SISPI on these files.',5)
                     call XWRITE('In the meantime, you can switch'//
     &                    ' to the PHA column.',5)
                     status = 0
                     call xsl_uclgsb('switch',SWITCHPI,status)
                     IF ( status .NE. 0 ) RETURN
                     IF ( SWITCHPI ) THEN
                        keypha = 'PHA'
                        call XSL_UCLPST('phaname','PHA',status)
                        goto 80
                     ENDIF
                  ENDIF
                  call XWRITE('I will remove this file, as it '//
     &                 'will cause problems at the extract stage.',5)
                  if (i.lt.nfiles ) then
                     do j=i+1,nfiles
                        filenm(j-1) = filenm(j)
                        catidx(j-1) = catidx(j)
                     enddo
                     nfiles = nfiles-1
                  else
                     nfiles = nfiles - 1
                  endif
                  if( nfiles .eq. 0 ) then
                     return
                  else
                     i = i - 1
                  endif
               else
                  call XWRITE('This file may cause problems at '//
     &                 'the extract stage.',5)
               endif
            else
C Assume keypha runs from 0 to phamax-1 (True for ASCA)
               phalimits(1) = 0
               phalimits(2) = phalimits(2)-1
            endif
         else
c Come here if you found TLMIN and TLMAX
c If the TLMIN is > the TLMAX, bag this file...
            if ( fileid.eq.0.and.
     &           phalimits(2) .lt. phalimits(1) ) then
               str1 = 'TLMIN > TLMAX for '//
     &              keypha(:LENACT(keypha))//
     &              ' in file:'
               call XWRITE(str1,5)
               str1 = ' * '//filenm(i)
               call XWRITE(str1,5)
               call XWRITE
     &              ('Removing this file from the choose list',5)
               if (i.lt.nfiles ) then
                  do j=i+1,nfiles
                     filenm(j-1) = filenm(j)
                     catidx(j-1) = catidx(j)
                  enddo
                  nfiles = nfiles-1
               else
                  nfiles = nfiles - 1
               endif
               if( nfiles .eq. 0 ) then
                  return
               else
                  i = i - 1
               endif
            endif            
            
         endif
         if(phalimits(1).lt.phalimits(2)) then
            phamin = min(phalimits(1),phamin)
            phamax = max(phalimits(2),phamax)
         endif
         i = i + 1
      enddo
      if(fileid.eq.0) then
         write(str1,85) keypha(:LENACT(keypha)),phamin,phamax
 85      format('Got min and max for ',a,': ',i5,'  ',i5)
         call XWRITE(str1,5)
         call XWRITE(' ',5)
      endif
      if (phalcut .ne. -20 .and. phalcut.lt.phamin) then
         write(str1,87) keypha(:LENACT(keypha)),phalcut
 87      format('Warning: your ',a,' lower bound: ',i5,
     &        ' is below the minimum value.')
         call XWRITE(str1,5)
         call XWRITE(' ',5)
      endif
      if (phahcut .ne. -20 .and. phahcut.gt.phamax) then
         write(str1,89) keypha(:LENACT(keypha)),phahcut
 89      format('Warning: your ',a,' upper bound: ',i5,
     &        ' is above the maximum value.')
         call XWRITE(str1,5)
         call XWRITE(' ',5)
      endif


      return
      end
c

c
c ---------------------------------------------
      subroutine XSL_MISS_SETUP()
c ---------------------------------------------
c Does some mission-specific setup after files have been read in
c and a catalog created.

      include 'xsel.inc'
      include 'xselvar.inc'

      INTEGER MAXCOL, MAXVAR
      PARAMETER (MAXCOL=4, MAXVAR=24)
      INTEGER intvals(MAXCOL,MAXVAR), nvals(MAXCOL)
      INTEGER i

      character(72) newval(3)

      IF ( keymis .NE. 'ASCA' ) RETURN

c If the datamode is FAST, then do the FAST setup:

      if ( datamode.eq.'FAST' ) THEN

         call XSL_FAST_SETUP(catnam,instru,in_or_out,ccdno,arena,ario,
     &                       stah,endh,catidx,nfiles,MAXFIL,status)

      elseif ( datamode .eq. 'PH' ) THEN

         newval(1) = 'RAWXBINS'
         newval(2) = 'PHA_BINS'
         newval(3) = 'RISEBINS'
         call XSL_COL2INT(newval,intvals,nvals,3,MAXCOL,
     &                    catnam,catidx,nfiles,MAXFIL,status)
         if(nvals(1) .gt. 1) then
            call XWRITE('There is more than one '//
     &                  'RAWXBINS value in your file',5)
            call XWRITE('You cannot use region filters,'//
     &                  ' or make an image.',5)
            VREGION = .FALSE.
            VIMAGE = .TRUE.
         else
            rxbval = intvals(1,1)
         endif
         if(nvals(2).gt.1) then
            call XWRITE('There is more than one '//
     &                  'PHA_BINS value in your file',5)
            call XWRITE('You cannot use FILTER '//
     &                  'PHA_CUTOFF, or make a spectrum.',5)
            VSPEC = .FALSE.
            VPHACUT = .FALSE.
         else
            phbval = intvals(2,1)
         endif
         do i=1,nvals(3)
            if(intvals(3,i) .le. 1 ) then
               call XWRITE('There is no RISETIME '//
     &                     'information in your files.',5)
               call XWRITE('You cannot use GISCLEAN.',5)
               VGISCLEAN = .FALSE.
               go to 678
            endif
         enddo
 678     continue

      endif

      RETURN
      END

c
c ---------------------------------------------
      subroutine XSL_PRE_EXTR()
c ---------------------------------------------
c Does some mission-specific setup before doing the extract

      include 'xsel.inc'
      include 'xselvar.inc'

c If it is ASCA faint mode the run FAINT->BRIGHT conversion before
c continuing

      IF(keymis .EQ. 'ASCA' .AND. datamode.eq.'FAINT') THEN
         call XWRITE('Running faint before extract:',5)
         call XSL_FAINT()
      ENDIF

c If it is Suzaku XIS STANDARD mode then run 5x5 to 3x3 conversion if necessary

      IF ( keymis .EQ. 'SUZAKU' .AND. instru(1:3) .EQ. 'XIS' .AND.
     &     datamode .EQ. 'STANDARD' ) THEN
         CALL XSL_5X5TO3X3()
      ENDIF

      RETURN
      END

c
c ---------------------------------------------
      subroutine XSL_POST_EXTR(qspec, qevnt)
c ---------------------------------------------

      LOGICAL qspec, qevnt

      include 'xsel.inc'
      include 'xselvar.inc'

c Does some mission-specific stuff after doing the extract
c   qspec        i: true if spectrum was just extracted
c   qevnt        i: true if event list was just extracted

cc      INTEGER len1
      INTEGER len2, ilun

      CHARACTER(255) str1, str3, str4, contxt
cc      CHARACTER(255) stem
      CHARACTER(1024) str1lon

      INTEGER LENACT
      EXTERNAL LENACT

c If this is the ASCA SIS and the datamode is FAST then make a WMAP
c and attach it to the spectral file

      IF ( keymis.eq.'ASCA'.and.datamode.eq.'FAST' .and. qspec ) THEN

         call GETLUN(ilun)
         call XSL_RMFILE(cmdfil)
         call XSL_RMFILE(tempfl)
         call XSL_OPCF(cmdfil,ilun)
         write(str3,'(i4)') extrbinh
         status = 0
         call xsl_uclgst( 'wmapcalfile', str4, status )
         contxt = 'Failed to get wmapcalfile'
         IF ( status .NE. 0 ) GOTO 99999
         len2 = LENACT(str4)
               
         str1lon = 'fastwmap '//spcfil(:LENACT(spcfil))//' '//
     &             tempfl(:LENACT(tempfl))//' '//
     &             str3(:LENACT(str3))//' '//
     &             'calfile = '//str4(:LENACT(str4))
         call XSL_WRTCF(ilun,str1lon,1)
         call XSL_CLCF(ilun)

         call XSL_RUNCF(cmdfil,ECHO,status)

         contxt = 'Could not add WMAP to spectral file'
         if(status.ne.0) GOTO 99999

C Now move the result back to the spectral filename
         len2 = LENACT(spcfil)
         call XSL_RENAME(tempfl,spcfil,1,str1,len2,status)

      ENDIF

C If this is NuSTAR and a new event file was extracted then apply the livetime
C correction
cc
cc      IF ( keymis.eq.'NUSTAR' .and. (qevnt.OR.qspec) ) THEN
cc
C if this is NuSTAR and we need to run nulivetime or nubackscale then we will need
C the filename stem so generate this. Note that the stem includes a final A or B to
C indicate the unit. This is not required for the name of all hk files.
cc
cc         len1 = LENACT(filenm(1))
cc         call XSL_FILENAME(filenm(1),str1,len1)
cc         stem = str1(:LENACT(str1)-9)
cc
cc         IF ( qevnt ) THEN
cc
C generate the HK filename. this does assume that all the event files are from
C the same observation but I can't think why they should not be.
cc
cc            call GETLUN(ilun)
cc            call XSL_RMFILE(cmdfil)
cc            call XSL_RMFILE(tempfl)
cc            call XSL_OPCF(cmdfil,ilun)
cc            status = 0
cc
cc            str1lon = 'nulivetime infile='//evnout(:LENACT(evnout))//
cc     &           ' hkfile='//datdir(:LENACT(datdir)-9)//'hk/'//
cc     &           stem(:LENACT(stem))//'_fpm.hk'//
cc     &           ' outfile='//tempfl//
cc     &           ' clobber=yes chatter=3 history=yes'
cc            call XSL_WRTCF(ilun,str1lon,1)
cc            call XSL_CLCF(ilun)
cc            
cc            call XSL_RUNCF(cmdfil,ECHO,status)
cc
cc            contxt = 'Could not correct the event file exposure '//
cc     &           'for deadtime'
cc            if(status.ne.0) GOTO 99999
cc
C Now move the result back to the event filename
cc            len2 = LENACT(evnout)
cc            call XSL_RENAME(tempfl,evnout,1,str1,len2,status)
cc
cc         ENDIF

cc         IF ( qspec ) THEN
cc
cc            call GETLUN(ilun)
cc            call XSL_RMFILE(cmdfil)
cc            call XSL_RMFILE(tempfl)
cc            call XSL_OPCF(cmdfil,ilun)
cc            status = 0
cc
ccc Use the input event filename unless a new one has been extracted
cc
cc            IF ( BINOUT ) THEN
cc               str1 = evnout
cc            ELSE
cc               str1 = filenm(1)
cc            ENDIF
cc
cc            str1lon = 'nubackscale'//
cc     &           ' srcphafile='//spcfil(:LENACT(spcfil))//
cc     &           ' bkgphafile='//spcfil(:LENACT(spcfil))//
cc     &           ' srcoutfile='//tempfl(:LENACT(tempfl))//
cc     &           ' bkgoutfile='//tempfl(:LENACT(tempfl))//
cc     &           ' evtfile='//str1(:LENACT(str1))//
cc     &           ' mastaspectfile='//datdir(:LENACT(datdir))//
cc     &                           stem(:LENACT(stem)-1)//'_mast.fits'//
cc     &           ' attfile='//datdir(:LENACT(datdir)-9)//'auxil/'//
cc     &                      stem(:LENACT(stem)-1)//'_att.fits'//
cc     &           ' det1reffile='//datdir(:LENACT(datdir))//
cc     &                      stem(:LENACT(stem))//'_det1.fits'
cc            call XSL_WRTCF(ilun,str1lon,1)
cc            call XSL_CLCF(ilun)
cc            
cc            call XSL_RUNCF(cmdfil,ECHO,status)
cc
cc            contxt = 'Could not set the BACKSCAL in spectrum'
cc            if(status.ne.0) GOTO 99999
cc
ccC Now move the result back to the spectrum filename
cc            len2 = LENACT(spcfil)
cc            call XSL_RENAME(tempfl,spcfil,1,str1,len2,status)
cc
cc         ENDIF
cc
cc      ENDIF

99999 IF ( status .NE. 0 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i5)') ' XSL_POST_EXTR: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      RETURN
      END

c ---------------------------------------------
      subroutine XSL_DET_LIST(arglst, nargs, MAXARG)
c ---------------------------------------------

      INTEGER MAXARG, nargs
      CHARACTER*(*) arglst(MAXARG)

c Routine to expand out a list of detectors, translating wild cards
c and putting in the full XTE names.

      include 'xsel.inc'
      include 'xselvar.inc'

      INTEGER i, j, ifirst

      CHARACTER(3) tmpstr

      status = 0

      IF ( .NOT.(keymis .EQ. 'XTE' .AND. instru .EQ. 'PCA') ) RETURN

c First expand out any wildcards. Note that whenever a wildcard is
c expanded those left in the list are shifted up.

      i = 0

      DO WHILE ( i .LT. nargs )

         i = i + 1

c If the first character is a "-" then this will be a filter to be removed.
c Track this by setting ifirst=2, otherwise ifirst=1.

         IF ( arglst(i)(1:1) .EQ. '-' ) THEN
            ifirst = 2
         ELSE
            ifirst = 1
         ENDIF

c Check for a wildcard on the PCU number.

         IF ( arglst(i)(ifirst:ifirst) .EQ. '*' ) THEN

            IF ( nargs+4 .GT. MAXARG ) THEN
               status = 1
               RETURN
            ENDIF

            DO j = nargs, i+1, -1
               arglst(j+4) = arglst(j)
            ENDDO
            nargs = nargs + 4

            tmpstr = arglst(i)(ifirst:ifirst+2)
            DO j = 1, 5
               IF ( ifirst .EQ. 1 ) THEN
                  WRITE(arglst(i+j-1)(1:3),'(i1,a2)') (j-1), tmpstr(2:3)
               ELSE
                  WRITE(arglst(i+j-1)(1:4),'(a1,i1,a2)') '-', (j-1), 
     &                                                   tmpstr(2:3)
               ENDIF
            ENDDO

         ENDIF

c Second check for a wildcard on the layer

         IF ( arglst(i)(ifirst+1:ifirst+1) .EQ. '*' ) THEN

            IF ( nargs+2 .GT. MAXARG ) THEN
               status = 1
               RETURN
            ENDIF

            DO j = nargs, i+1, -1
               arglst(j+2) = arglst(j)
            ENDDO
            nargs = nargs + 2

            tmpstr = arglst(i)(ifirst:ifirst+2)
            DO j = 1, 3
               IF ( ifirst .EQ. 1 ) THEN
                  WRITE(arglst(i+j-1)(1:3),'(a1,i1,a1)') tmpstr(1:1),
     &                                                j, tmpstr(3:3)
               ELSE
                  WRITE(arglst(i+j-1)(1:3),'(a1,a1,i1,a1)') '-', 
     &                                   tmpstr(1:1), j, tmpstr(3:3)
               ENDIF
            ENDDO

         ENDIF

c Finally check for a wildcard on the anode side

         IF ( arglst(i)(ifirst+2:ifirst+2) .EQ. '*' ) THEN

            IF ( nargs+1 .GT. MAXARG ) THEN
               status = 1
               RETURN
            ENDIF

            DO j = nargs, i+1, -1
               arglst(j+1) = arglst(j)
            ENDDO
            nargs = nargs + 1

            tmpstr = arglst(i)(ifirst:ifirst+2)
            IF ( ifirst .EQ. 1 ) THEN
               arglst(i)   = tmpstr(1:2)//'L'
               arglst(i+1) = tmpstr(1:2)//'R'
            ELSE
               arglst(i)   = '-'//tmpstr(1:2)//'L'
               arglst(i+1) = '-'//tmpstr(1:2)//'R'
            ENDIF

         ENDIF

c end loop over the list of detectors

      ENDDO

c Now expand the 3-character abbreviation into the full detector name

      CALL xwrite(
     & 'Detectors just selected (- means detector will be removed)...',
     &            5)

      DO i = 1, nargs

         IF ( arglst(i)(1:1) .EQ. '-' ) THEN
            tmpstr = arglst(i)(2:4)
            arglst(i) = '-X'//tmpstr(2:3)//'SpecPcu'//tmpstr(1:1)
         ELSE
            tmpstr = arglst(i)(1:3)
            arglst(i) = 'X'//tmpstr(2:3)//'SpecPcu'//tmpstr(1:1)
         ENDIF

         CALL xwrite(arglst(i), 5)

      ENDDO

      RETURN
      END

c ---------------------------------------------
      subroutine XSL_CHIP_SEL(chipsel)
c ---------------------------------------------

      CHARACTER chipsel*(*)

c Routine to make new event files with events only for the selected chips.
c For the case of Chandra will translate I# or S# into chip number. For
c Chandra and XMM will check whether the chip number is within a valid range.
c This code is also used by SELECT EXPREF for Swift UVOT.

c kaa 9/16/99
c kaa 9/22/99  Also finds the min and max sky coordinates in the result 
c              and sets the xycenter and xysize appropriately
c kaa 3/23/16  Removed mission and instrument restriction

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c     Scratch variables
c The command line:
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(512) str1
      character(255) cstr
c Strings defining temporary filenames
      character(255) statout
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings
      integer len1, len2, len3
c HDU number for GTI
      INTEGER gtihdu
c Number of chips selected
      INTEGER nchsel
c Sky X,Y min and max
      DOUBLE PRECISION skyxrng(2), skyyrng(2)
c ---------------------------------------------
      DOUBLE PRECISION dnum
      integer LENACT
      integer i, j, ipt, idelim, nret, ichip

      INTEGER NCHIPS
      PARAMETER (NCHIPS=99)
      character(2) chipnm(0:NCHIPS), chipspec
      logical qchip(0:NCHIPS)

      DATA chipnm /'I0','I1','I2','I3','S0','S1','S2','S3','S4','S5',
     &             90*' '/
      DATA statout /'xsel_statout.tmp'/

      status = 0

c If keyccd has not been set or is NONE then cannot select.

      IF ( keyccd .EQ. 'NONE' .OR. lenact(keyccd) .EQ. 0 ) THEN
         CALL xwrite('No chip column was defined in the xselect.mdb', 5)
         RETURN
      ENDIF

c Give the user the chance to save the workspace or evnin files

      call XSL_SAVE_WS_OR_EVNIN()

c delete previous command file and work2 files

      call XSL_RMWORK(work2,nfiles)

c Open new command file

      call XSL_OPCF(cmdfil,ilun)

c If we've already performed a data-changing operation like a
c SELECT or a faint-to-bright conversion we'll be using
c the WORK1 filenames as input. Otherwise it'll be the original
c filenames. Either way, the WORK2 files are the output files, which
c are then copied (back) to the WORK1 files and the .WORK. logical
c reset for subsequent observations.

c Construct selection expression and place in cstr variable. Assume
c that chipsel contains string of numbers specifying chips (which
c can range from 0-9 for ACIS, 1-12 for PN, 1-7 for MOS, 1-99 for UVOT) 
c or two letter specifications (I0 to I3 and S0 to S5 for ACIS). Note 
c the somewhat convoluted code to check both these possible ways of 
c giving the chips.

      DO i = 0, NCHIPS
         qchip(i) = .FALSE.
      ENDDO

      CALL upc(chipsel)

c Parse the input string (assumed to be blank or comma delimited entries)
c to set the qchip flags.

      ipt = 0
      DO WHILE ( ipt .LE. len(chipsel) )

         CALL xgtstr(chipsel, ipt, 1, 'chip', 1, chipspec, nret, 
     &               status, idelim)

         IF ( status .EQ. 0 ) THEN

c If AXAF check for the names of the ACIS chips

            ichip = -1
            IF ( keymis .EQ. 'AXAF' ) THEN
               DO i = 0, 9
                  IF ( chipspec .EQ. chipnm(i) ) ichip = i
               ENDDO
            ENDIF

c If that doesn't identify the chip then just try to read the string

            IF ( ichip .EQ. -1 ) THEN
               IF ( lenact(chipspec) .EQ. 1 ) THEN
                  READ(chipspec,'(i1)',iostat=status) ichip
               ELSE
                  READ(chipspec,'(i2)',iostat=status) ichip
               ENDIF
            ENDIF

c Check that this is a valid chip

            IF ( keymis .EQ. 'AXAF' .AND. 
     &           (ichip .LT. 0. .OR. ichip .GT. 9) ) THEN
               cstr = chipspec//
     &             ' is not a valid chip for Chandra ACIS'
               CALL xwrite(cstr, 5)
            ELSEIF ( keymis .EQ. 'XMM' .AND. 
     &           (instru .EQ. 'EMOS1' .OR. instru .EQ. 'EMOS2')
     &           .AND. (ichip .LT. 1 .OR. ichip .GT. 7 ) ) THEN
               cstr = chipspec//
     &             ' is not a valid chip for XMM MOS'
               CALL xwrite(cstr, 5)
            ELSEIF ( keymis .EQ. 'XMM' .AND. instru .EQ. 'EPN'
     &           .AND. (ichip .LT. 1 .OR. ichip .GT. 12 ) ) THEN
               cstr = chipspec//
     &             ' is not a valid chip for XMM PN'
               CALL xwrite(cstr, 5)
            ELSEIF ( keymis .EQ. 'SWIFT' .AND. instru .EQ. 'UVOT'
     &           .AND. ichip .LT. 1 ) THEN
               cstr = chipspec//
     &             ' is not a valid chip for Swift UVOT'
               CALL xwrite(cstr, 5)
            ELSE
               qchip(ichip) = .TRUE.
            ENDIF

         ENDIF

      ENDDO

c Generate the selection string

      len3 = 1
      cstr = '"'
      nchsel = 0
      DO i = 0, NCHIPS

         IF ( qchip(i) ) THEN
            IF ( len3 .NE. 1 ) THEN
               cstr(len3+1:len3+6) = ' .OR. '
               len3 = len3 + 6
            ENDIF
            WRITE(cstr(len3+1:),'(a,i2)') 
     &         keyccd(:lenact(keyccd))//' .EQ. ', i
            len3 = lenact(cstr)
            nchsel = nchsel + 1
         ENDIF

      ENDDO

      status = 0

      IF ( nchsel .EQ. 0 ) THEN
         CALL xwrite('No valid chips selected - please try again', 5)
         RETURN
      ENDIF

      cstr(len3+1:len3+1) = '"'
      len3 = len3 + 1

c     Save this as one of the selection strings (without the double quotes)
c     so that it will show up correctly under the status

      nstrsel = nstrsel + 1
      strsel(nstrsel) = cstr(2:len3-1)

c     Construct instruction strings controlling NFILES applications
c     of FSELECT.  Remember to point at the event extension (evtnum)

      IF(BININ) THEN

         comlin='fselect '//
     +        'infile='//evnin(:LENACT(evnin))//' '//
     +        'outfile='//work2(1)(:LENACT(work2(1)))//' '//
     +        'expr='//cstr(1:len3)//' '//
     +        'histkw=yes copyall=yes keycopy=yes'
         call XSL_WRTCF(ilun,comlin,1)

      ELSE IF(BINOUT) THEN

         comlin='fselect '//
     +        'infile='//evnout(:LENACT(evnout))//' '//
     +        'outfile='//work2(1)(:LENACT(work2(1)))//' '//
     +        'expr='//cstr(1:len3)//' '//
     +        'histkw=yes copyall=yes keycopy=yes'
         call XSL_WRTCF(ilun,comlin,1)

      ELSE
         DO i = 1, nfiles

            IF( WORK )then
               len1 = LENACT( work1(i) )
               len2 = LENACT( work2(i) )

               comlin='fselect '//
     +              'infile='//work1(i)(1:len1)//' '//
     +              'outfile='//work2(i)(1:len2)//' '//
     +              'expr='//cstr(1:len3)//' '//
     +              'histkw=yes copyall=yes keycopy=yes'

               str1 = 'Processing file: '//work1(i)
               call XSL_MESSAGE(ilun,str1)
            ELSE
               len1 = LENACT( filenm(i) )
               len2 = LENACT( work2(i) )

               comlin='fselect '//
     +              'infile='//filenm(i)(1:len1)//'+'//evtnum//' '//
     +              'outfile='//work2(i)(1:len2)//' '//
     +              'expr='//cstr(1:len3)//' '//
     +              'histkw=yes copyall=yes keycopy=yes'
               str1 = 'Processing file: '//filenm(i)
               call XSL_MESSAGE(ilun,str1)

            ENDIF

            call XSL_WRTCF(ilun,comlin,1)

c     We've now sent the command to do the selection.
         END DO
      ENDIF

      call XSL_CLCF(ilun)

c     Run the command file
      call XSL_RUNCF(cmdfil,ECHO,status)

c     Set the SELCT logical
      if(status.eq.0) THEN
         SELCT = .TRUE.

c Finally copy the files over
         IF(BININ) THEN
c If BININ, then only the evnin file has been altered.
            len1 = LENACT(evnin)
            call XSL_RENAME(work2(1),evnin,1,str1,len1,ierr)
         ELSE IF(BINOUT) THEN
c If BININ, then only the evnin file has been altered.
            len1 = LENACT(evnout)
            call XSL_RENAME(work2(1),evnout,1,str1,len1,ierr)
         ELSE
c Otherwise, all the selected files will be in WORK2, which should
c become the CURRENT files. Thus, copy the WORK2 files to WORK1.

c Open new command file

            call XSL_OPCF(cmdfil,ilun)
            do i=1,nfiles
               len1 = LENACT(work1(i))
               call XSL_RENAME(work2(i),work1(i),2,str1,len1,ierr)
               write(ilun,53) str1(1:len1)
            enddo
            call XSL_CLCF(ilun)

c Run the command file
            call XSL_RUNCF(cmdfil,ECHO,ierr)
c Set the WORK logical, if it isn't set already.
            WORK = .TRUE.
c Reset the MERGED logical -- individual files have changed.
            MERGED = .FALSE.
         ENDIF
      ELSE
         call XWRITE('Error in Fselect, No selection made',5)
         RETURN
      ENDIF

c Now do the clever part to try to end up with the correct GTI extensions
c and data subspace keywords.
c Loop round the working files

      DO i = 1, nfiles

         len1 = LENACT ( work1(i) )
         len2 = LENACT ( work2(i) )

c Delete all the unwanted GTI extensions from the work1 file. Have to split 
c this up into individual command files since the HDU numbers change after
c each delete.

         DO j = 0, NCHIPS
            IF ( .NOT.qchip(j) ) THEN
               CALL XSL_GTI_HDU(work1(i), evtnam, keyccd, j, 
     &                          gtihdu)
               IF ( gtihdu .GE. 0 ) THEN
                  str1 = work1(i)(1:len1)//'+'
                  IF ( gtihdu .LE. 9 ) THEN
                     WRITE(str1(len1+2:),'(i1)') gtihdu
                  ELSEIF (  gtihdu .LE. 99 ) THEN
                     WRITE(str1(len1+2:),'(i2)') gtihdu
                  ENDIF
                  call XSL_OPCF(cmdfil,ilun)
                  comlin = 'fdelhdu '//
     &                     'infile='//str1(1:LENACT(str1))//' '//
     &                     'confirm=no proceed=yes'
                  CALL XSL_WRTCF(ilun, comlin, 1)
                  call XSL_CLCF(ilun)
                  call XSL_RUNCF(cmdfil,ECHO,status)
                  IF ( status .NE. 0 ) THEN
                     CALL XWRITE('Failed to delete GTI extension',5)
                  ELSE
                     CALL XSL_COR_DSS(work1(i), evtnam, keyccd, j)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO

      ENDDO

c Find the sky X,Y min and max using fstatistic 

      skyxrng(1) = 1.d20
      skyxrng(2) = -1.d20
      skyyrng(1) = 1.d20
      skyyrng(2) = -1.d20

      DO i = 1, nfiles

         len1 = LENACT(work1(i))
         len2 = LENACT(statout)

c Run fstatistic on the sky X axis

         call XSL_OPCF(cmdfil,ilun)

         comlin = 'fstatistic '//
     &            'infile='//work1(i)(1:len1)//' '//
     &            'colname='//xcolf(:LENACT(xcolf))//' '//
     &            'rows="-" minval=INDEF maxval=INDEF '//
     &            'outfile=!'//statout(1:len2)
         CALL XSL_WRTCF(ilun, comlin, 1)

         call XSL_CLCF(ilun)
         call XSL_RUNCF(cmdfil,ECHO,status)

         IF ( status .NE. 0 ) THEN
            CALL XWRITE('Failed to run fstatistic on sky X',5)
            RETURN
         ENDIF

c Read the fstatistic output file to get the minimum and maximum

         call GETLUN(ilun)
         call XSL_OPEN(ilun,statout,'OLD',' ',' ',0,0,status)
         READ(ilun,*)
         READ(ilun,*)
         READ(ilun,*)
         READ(ilun,'(a)') str1
         READ(str1(41:),*) dnum
         skyxrng(1) = MIN(skyxrng(1),dnum)
         READ(ilun,'(a)') str1
         READ(str1(41:),*) dnum
         skyxrng(2) = MAX(skyxrng(2),dnum)
         CLOSE(ilun)
         CALL frelun(ilun)

c Now repeat for the sky Y axis

         call XSL_OPCF(cmdfil,ilun)

         comlin = 'fstatistic '//
     &            'infile='//work1(i)(1:len1)//' '//
     &            'colname='//ycolf(:LENACT(ycolf))//' '//
     &            'rows="-" minval=INDEF maxval=INDEF '//
     &            'outfile=!'//statout(1:len2)
         CALL XSL_WRTCF(ilun, comlin, 1)

         call XSL_CLCF(ilun)
         call XSL_RUNCF(cmdfil,ECHO,status)

         IF ( status .NE. 0 ) THEN
            CALL XWRITE('Failed to run fstatistic on sky Y',5)
            RETURN
         ENDIF

         call GETLUN(ilun)
         call XSL_OPEN(ilun,statout,'OLD',' ',' ',0,0,status)
         READ(ilun,*)
         READ(ilun,*)
         READ(ilun,*)
         READ(ilun,'(a)') str1
         READ(str1(41:),*) dnum
         skyyrng(1) = MIN(skyyrng(1),dnum)
         READ(ilun,'(a)') str1
         READ(str1(41:),*) dnum
         skyyrng(2) = MAX(skyyrng(2),dnum)
         CLOSE(ilun)
         CALL frelun(ilun)

      ENDDO

c Reset the XYCENTER and XYSIZE to encompass the mins and maxs with 
c a little bit of margin

      xcf = INT((skyxrng(1)+skyxrng(2))/2.)
      ycf = INT((skyyrng(1)+skyyrng(2))/2.)

      sizef = MAX(INT(skyxrng(2)+0.999999)-INT(skyxrng(1))+1,
     &            INT(skyyrng(2)+0.999999)-INT(skyyrng(1))+1) + 2

      WRITE(str1,'(a,i6,a,i6)') 'Set XYCENTER to ', xcf, ', ', ycf
      CALL XWRITE(str1,5)
      WRITE(str1,'(a,i6,a,i6)') 'and XYSIZE   to ', sizef
      CALL XWRITE(str1,5)
      WRITE(str1,'(a,i6,a,i6)') 
     & 'with current binning image will be ', 
     & INT(FLOAT(sizef)/xbinf+0.99999), ' x ',
     & INT(FLOAT(sizef)/xbinf+0.99999)
      CALL XWRITE(str1,5)

 53   format(a)
      return
      end

c *********************************************************************
      SUBROUTINE XSL_GTI_HDU(filenm, evtnam, keyccd, chipno, gtihdu)

      IMPLICIT NONE

      INTEGER chipno, gtihdu
      CHARACTER filenm*(*), evtnam*(*), keyccd*(*)

c Routine to return the number of the HDU containing the GTI for the chip 
c requested. Uses the data subspace keywords.

c Arguments :
c    filenm        c      i: Event filename
c    evtnam        c      i: Name of the events extension
c    keyccd        c      i: The column name for the CCD number
c    chipno        i      i: Chip of interest
c    gtihdu        i      r: HDU number - return -1 if no extension found

      INTEGER MAXCOL
      PARAMETER (MAXCOL=100)

      INTEGER ilun, block, status, numcol, itcol, iccol, i, iccd
      INTEGER inum

      CHARACTER(50) string
      CHARACTER(70) comment
      CHARACTER(72) contxt
      CHARACTER(20) colnam(MAXCOL), gtinam
      CHARACTER(8) gtiref, ccdval

      LOGICAL qfound

      INTEGER lenact
      EXTERNAL lenact

      status = 0
      gtihdu = -1

c Open the event file

      CALL getlun(ilun)
      CALL ftopen(ilun, filenm, 0, block, status)
      contxt = 'XSL_GTI_HDU : Failed to open '//filenm(:lenact(filenm))
      IF ( status .NE. 0 ) THEN
         CALL frelun(ilun)
         CALL fcerr(contxt)
         CALL fcerrm(status)
         RETURN
      ENDIF         

c Move to the event extension

      CALL ftmnhd(ilun, 2, evtnam, 0, status)
      contxt = 'XSL_GTI_HDU : Failed to find event extension'
      IF ( status .NE. 0 ) GOTO 999

c Read the DSTYP keywords

      CALL ftgkns(ilun, 'DSTYP', 1, MAXCOL, colnam, numcol, status)
      contxt = 'XSL_GTI_HDU : Failed to read DSTYP keywords'
      IF ( status .NE. 0 ) GOTO 999

c Find the TIME and CCD number keywords.

      itcol = -1
      iccol = -1
      DO i = 1, numcol
         CALL upc(colnam(i))
         IF ( colnam(i)(1:4) .EQ. 'TIME' ) itcol = i
         IF ( colnam(i)(:lenact(keyccd)) .EQ. keyccd ) iccol = i
      ENDDO
      contxt = 'XSL_GTI_HDU: Failed to find DSTYP for TIME'
      IF ( itcol .EQ. -1 ) GOTO 999
      contxt = 'XSL_GTI_HDU: Failed to find DSTYP for '//
     &         keyccd(:lenact(keyccd))
      IF ( iccol .EQ. -1 ) GOTO 999

c Find which DSVAL points to the right CCD

      iccd = 0
      qfound = .FALSE.
      DO WHILE ( status .EQ. 0 .AND. .NOT.qfound )
         ccdval = 'DSVAL' 
         CALL dsscat(ccdval, iccd+1, iccol)
         CALL ftgkys(ilun, ccdval, string, comment, status)
         IF ( status .EQ. 0 ) THEN
            READ(string(index(string,':')+1:),*) inum
            IF (inum .EQ. chipno ) qfound = .TRUE.
            iccd = iccd + 1
         ENDIF
      ENDDO
      status = 0
      IF ( .NOT.qfound ) GOTO 999

c Construct the appropriate DSREF keyword name

      gtiref = 'DSREF'
      CALL dsscat(gtiref, iccd, itcol)

c and get the name making sure to remove any prepended colon. Don't throw
c an error because this may just be a CCD not included in the data.

      CALL ftgkys(ilun, gtiref, gtinam, comment, status)
      IF ( status .NE. 0 ) THEN
         status = 0
         GOTO 999
      ENDIF

      gtinam = gtinam(index(gtinam,':')+1:)

c Finally move to the HDU specified by this name and get its number - then
c subtract one to get the ftools counting scheme.

      CALL ftmnhd(ilun, 2, gtinam, 0, status)
      IF ( status .NE. 0 ) THEN
         status = 0
         GOTO 999
      ENDIF

      CALL ftghdn(ilun, gtihdu)
      gtihdu = gtihdu - 1


 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
      ENDIF

c Close the file and return

      CALL ftclos(ilun, status)
      CALL frelun(ilun)


      RETURN
      END

c ***********************************************************************

      SUBROUTINE XSL_COR_DSS(filenm, evtnam, keyccd, chipno)

      IMPLICIT NONE

      INTEGER chipno
      CHARACTER filenm*(*), evtnam*(*), keyccd*(*)

c Routine to remove data subspace keywords corresponding to the CCD chipno.

      INTEGER MAXCOL
      PARAMETER(MAXCOL=100)

      INTEGER iccol, numcol, ilun, block, status, i, j
      INTEGER iccd, inum

      LOGICAL qfound

      CHARACTER(20) colnam(MAXCOL), keynam
      CHARACTER(50) string
      CHARACTER(80) comment, contxt

      INTEGER lenact
      EXTERNAL lenact

      status = 0

c Open the event file

      CALL getlun(ilun)
      CALL ftopen(ilun, filenm, 1, block, status)
      contxt = 'XSL_COR_DSS : Failed to open '//filenm(:lenact(filenm))
      IF ( status .NE. 0 ) THEN
         CALL frelun(ilun)
         CALL fcerr(contxt)
         CALL fcerrm(status)
         RETURN
      ENDIF         

c Move to the event extension

      CALL ftmnhd(ilun, 2, evtnam, 0, status)
      contxt = 'XSL_COR_DSS : Failed to find event extension'
      IF ( status .NE. 0 ) GOTO 999

c Read the DSTYP keywords

      CALL ftgkns(ilun, 'DSTYP', 1, MAXCOL, colnam, numcol, status)
      contxt = 'XSL_COR_DSS : Failed to read DSTYP keywords'
      IF ( status .NE. 0 ) GOTO 999

c Find the CCD number keyword.

      iccol = -1
      DO i = 1, numcol
         CALL upc(colnam(i))
         IF ( colnam(i)(:lenact(keyccd)) .EQ. keyccd ) iccol = i
      ENDDO
      contxt = 'XSL_COR_DSS: Failed to find DSTYP for '//
     &         keyccd(:lenact(keyccd))
      IF ( iccol .EQ. -1 ) GOTO 999

c Find which DSVAL points to the right CCD

      iccd = 0
      qfound = .FALSE.
      DO WHILE ( status .EQ. 0 .AND. .NOT.qfound )
         keynam = 'DSVAL' 
         CALL dsscat(keynam, iccd+1, iccol)
         CALL ftgkys(ilun, keynam, string, comment, status)
         IF ( status .EQ. 0 ) THEN
            READ(string(index(string,':')+1:),*) inum
            IF (inum .EQ. chipno ) qfound = .TRUE.
            iccd = iccd + 1
         ENDIF
      ENDDO
      status = 0
      IF ( .NOT.qfound ) GOTO 999

c Loop round all the columns listed in the DSTYP keywords

      DO i = 1, numcol

c Knock out any DSVAL and DSREF for iccd

         status = 0
         keynam = 'DSVAL'
         CALL dsscat(keynam, iccd, i)
         CALL ftdkey(ilun, keynam, status)
         status = 0
         keynam = 'DSREF'
         CALL dsscat(keynam, iccd, i)
         CALL ftdkey(ilun, keynam, status)

c Shift down all DSVALs and DSREFs from iccd+1 and up

         j = iccd
         status = 0
         DO WHILE ( status .EQ. 0 )
            keynam = 'DSVAL'
            CALL dsscat(keynam, j+1, i)
            CALL ftgkys(ilun, keynam, string, comment, status)
            IF ( status .EQ. 0 ) THEN
               keynam = 'DSVAL'
               CALL dsscat(keynam, j, i)
               CALL ftukys(ilun, keynam, string, comment, status)
               keynam = 'DSVAL'
               CALL dsscat(keynam, j+1, i)
               CALL ftdkey(ilun, keynam, status)
            ENDIF
            j = j + 1
         ENDDO

         j = iccd
         status = 0
         DO WHILE ( status .EQ. 0 )
            keynam = 'DSREF'
            CALL dsscat(keynam, j+1, i)
            CALL ftgkys(ilun, keynam, string, comment, status)
            IF ( status .EQ. 0 ) THEN
               keynam = 'DSREF'
               CALL dsscat(keynam, j, i)
               CALL ftukys(ilun, keynam, string, comment, status)
               keynam = 'DSREF'
               CALL dsscat(keynam, j+1, i)
               CALL ftdkey(ilun, keynam, status)
            ENDIF
            j = j + 1
         ENDDO

      ENDDO

      status = 0

 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
      ENDIF

c Close the file and return

      CALL ftclos(ilun, status)
      CALL frelun(ilun)


      RETURN
      END

c ***********************************************************************

        SUBROUTINE DSSCAT(str,i,j)

        IMPLICIT NONE

        CHARACTER*(*) str
        INTEGER i, j

c Adds numbers to the data subspace keywords. Returns i//str//j unless
c i==1 in which case returns str//j.

        character(10) tmpsti, tmpstj

        INTEGER lenact
        EXTERNAL lenact

        IF ( i .LE. 0 .OR. j .LE. 0 ) RETURN

        IF(i.LT.10) THEN
           WRITE(tmpsti,'(I1)') i
        ELSE IF (i.lt.100) THEN
           WRITE(tmpsti,'(I2)') i
        ELSE IF (i.lt.1000) THEN
           WRITE(tmpsti,'(I3)') i
        ELSE IF (i.lt.10000) THEN
           WRITE(tmpsti,'(I4)') i
        ELSE IF (i.lt.100000) THEN
           WRITE(tmpsti,'(I5)') i
        ELSE IF (i.lt.1000000) THEN
           WRITE(tmpsti,'(I6)') i
        ENDIF

        IF(j.LT.10) THEN
           WRITE(tmpstj,'(I1)') j
        ELSE IF (j.lt.100) THEN
           WRITE(tmpstj,'(I2)') j
        ELSE IF (j.lt.1000) THEN
           WRITE(tmpstj,'(I3)') j
        ELSE IF (j.lt.10000) THEN
           WRITE(tmpstj,'(I4)') j
        ELSE IF (j.lt.100000) THEN
           WRITE(tmpstj,'(I5)') j
        ELSE IF (j.lt.1000000) THEN
           WRITE(tmpstj,'(I6)') j
        ENDIF

        IF ( i .GT. 1 ) THEN
           str = tmpsti(1:LENACT(tmpsti))//str(1:LENACT(str))
     &           //tmpstj(1:LENACT(tmpstj))
        ELSE
           str = str(1:LENACT(str))//tmpstj(1:LENACT(tmpstj))
        ENDIF

        RETURN

        END

c ---------------------------------------------
      subroutine XSL_XMM_SPEC(outfile)
c ---------------------------------------------

      IMPLICIT NONE
      CHARACTER*(*) outfile

c Adds the BADPIX and EXPOSU extensions to an XMM spectrum

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'

      INTEGER ilun, clen
      CHARACTER(255) evtfile, comlin

      INTEGER LENACT
      EXTERNAL LENACT

      status = 0

c The tricky thing here is that we have to pick up the correct file
c with the event data depending on whether it has been created or filtered
c or selected.

      IF ( BININ ) THEN
         evtfile = evnin
      ELSEIF ( BINOUT ) THEN
         evtfile = evnout
      ELSE
         IF ( nfiles .GT. 1 ) THEN
            CALL xwrite(
     &'XMM spectral processing will not work correctly with multiple',5)
            CALL xwrite(
     &'event files - use extract event then try again',5)
            status = 1
            RETURN
         ENDIF
         IF ( WORK ) THEN
            evtfile = work1(1)
         ELSE
            evtfile = filenm(1)
         ENDIF
      ENDIF

c Run the perl script

      call XSL_RMFILE(cmdfil)
      call XSL_OPCF(cmdfil, ilun)
      comlin = 'xsl_xmm_add_badpix_exposu '//outfile(:LENACT(outfile))//
     &         ' '//evtfile(:LENACT(evtfile))
      IF ( ECHO ) THEN
         clen = LENACT(comlin)
         comlin(clen+1:) = ' yes'
      ENDIF
      call XSL_WRTCF(ilun, comlin, 0)
      call XSL_CLCF(ilun)
      call XSL_RUNCF(cmdfil, ECHO, status)

      status = 0


      RETURN
      END

c ---------------------------------------------
      subroutine XSL_5X5TO3X3()
c ---------------------------------------------
c
c Do the Suzaku XIS 5x5 to 3x3 event file conversion if necessary
c
c  kaa 3/13/08

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line:
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(255) str1, str2
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2
c ---------------------------------------------

      logical do5x5(MAXFIL), all5x5, all3x3
      integer i, statv
      integer LENACT

      status = 0
      IF(.NOT.READ) THEN
         call XWRITE('No data read in yet',5)
         return
      ELSEIF(instru(1:3).ne.'XIS') THEN
         call XWRITE(
     &    '5x5 to 3x3 conversion is only run on XIS data.',5)
         return
      ELSE IF(datamode.ne.'STANDARD') THEN
         call XWRITE(
     &    '5x5 to 3x3 conversion is only run on STANDARD mode data.',5)
         return
      ENDIF

c We can return immediately if there has been a previous run of extract or sisclean
c or there is only one input file

      IF ( BININ .OR. BINOUT .OR. nfiles .EQ. 1 ) RETURN

c First find out whether 5x5 to 3x3 conversion will be needed. Only do this if
c we have a mixture of 5x5 and 3x3 files.

      all5x5 = .TRUE.
      all3x3 = .TRUE.
      DO i = 1, nfiles
         IF ( WORK ) then
            str1 = work1(i)
         ELSE
            str1 = filenm(i)
         ENDIF
         CALL xsl_getkwst(str1, '0', datdir, 'EDITMODE', str2, statv, 1,
     &                     1, .TRUE., status)
         IF ( statv .EQ. 0 .AND. str2(1:3) .EQ. '5x5' ) then
            do5x5(i) = .TRUE.
            all3x3 = .FALSE.
         ELSE
            do5x5(i) = .FALSE.
            all5x5 = .FALSE.
         ENDIF
      ENDDO

c If either all 3x3 or all 5x5 then we are done

      IF ( all3x3 .OR. all5x5 ) RETURN
         
c Delete previous command file and work2 files

      call XSL_RMWORK(work2,nfiles)

c Open new command file

      call XSL_OPCF(cmdfil,ilun)

c If we've already performed a SELECTion, we'll be using
c the WORK1 filenames as input. Otherwise it'll be the original
c filenames. Either way, the WORK2 files are the output files, which
c are then copied (back) to the WORK1 files and the .WORK. logical
c reset for subsequent observations.

c Construct instruction strings controlling NFILES applications
c of XIS5X5TO3X3.

      DO i = 1, nfiles

         IF ( WORK ) then
            str1 = work1(i)
         ELSE
            str1 = filenm(i)
         ENDIF
         len1 = LENACT( str1 )
         len2 = LENACT( work2(i) )

c Convert 5x5 to 3x3 if this file is 5x5 and we have a mixture of types

         IF ( do5x5(i) .AND. .NOT.all5x5 ) then

            comlin='xis5x5to3x3 '//
     +           'infile='//str1(1:len1)//' '//
     +           'outfile='//work2(i)(1:len2)//' '//
     +           'hkfile=none '//
     +           'anl_verbose=0 '//
     +           'anl_profile=no '//
     +           'num_event=-1 '//
     +           'event_freq=10000 '//
     +           'chatter=2 '

            str2 = 'Running xis5x5to3x3 on '//str1(1:len1)
            call XSL_MESSAGE(ilun,str2)

         ELSE

            comlin='cp '//str1(1:len1)//' '//work2(i)(1:len2)

         ENDIF

         call XSL_WRTCF(ilun,comlin,1)

c We've now sent the command to do the conversion.
c Now, all the selected files will be in WORK2, which should
c become the CURRENT files. Thus, copy the WORK2 files to WORK1

      END DO

      call XSL_CLCF(ilun)
   
c Run the command file
      call XSL_RUNCF(cmdfil,ECHO,status)
c Check for errors
      IF(status.eq.0) THEN

c Set the WORK logical
         WORK = .TRUE.

c Reset the MERGED logical -- individual files have (probably) changed.
         MERGED = .FALSE.

c Open new command file

         call XSL_OPCF(cmdfil,ilun)
         do i=1,nfiles
            len1 = LENACT(work1(i))
            call XSL_RENAME(work2(i),work1(i),2,str1,len1,ierr)
            write(ilun,'(a)') str1(1:len1)
         enddo
         call XSL_CLCF(ilun)
   
c Run the command file
         call XSL_RUNCF(cmdfil,ECHO,status)

      ENDIF

      return

      end

