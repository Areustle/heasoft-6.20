      subroutine ximcmd(cmdid, cstring, status)
      IMPLICIT NONE
c
c  I  cmdid    (i)  Command id (for quick access)
c  I  cstring  (s)  Command string
c  O  status   (i)  Error flag (0=OK)
c
      integer*4 cmdid
      character*(*) cstring
      integer*4 status
c
c       Image accumulation and analysis environment
c
      include '../include/maxvals.inc'
      include '../include/ximage.inc'
      include '../include/colordef.inc'
      include '../include/io.inc'
      include '../include/sitedef.inc'
      include '../include/dynmem.inc'
      include '../include/mapdef.inc'

c  Interface

      integer maxcmdnm, imap
      parameter (maxcmdnm = 50)
      character*(maxcmdnm) command
      character*(MAX_IDSTR) rdmapid
      logical savmode
      character(100) ds

c  External functions

      integer*4 LENACT

      if ( lenact(cstring).gt.maxcmdnm ) then
         call xwrite (' Maximum command name size exceeded', 5)
         return
      endif
c
c  Match to list of commands
c
      command = cstring
      call UPC(command)
c
c  Initialize command string for anything using XPARSE routines
c
      ZSTring = ' '
      ZPArse = 0
c
c  Start loop of commands
c
      status = 0
c
c  Set chat level
c
      if ( command.eq.'CHATTER' ) then
         call chatter(cmdid,status)
c
c  Calculate offset and rotation for coordinate correction
c
      elseif ( command.eq.'CALCCOR' ) then
         call calccor(cmdid,status)
c
c  Change plot device
c
      elseif ( command.eq.'CPD') then
         call cpd (cmdid,status)
c        * Reset viewport configuration
         if ( status.eq.0 .and. vpnum.gt.1 ) vpnum = 1
c
c  Manipulate maps
c
      elseif ( command.eq.'MAP' ) then
         call map(cmdid, status)
c
c  Show header
c
      elseif ( command.eq.'HEADER' ) then
         call header (cmdid, status)
c
c  Show current state
c
      elseif ( command.eq.'SHOW' ) then
         call show(cmdid,mapids(icurmap),version,
     &             vpfile,vpnum,vpset,status)
c
c  Manipulate mission database
c
      elseif ( command.eq.'CHMDB' ) then
         call chmdb(cmdid, status)
c
c  Print info, orientation, scale to display
c
      elseif ( command.eq.'IMINFO' ) then
         call iminfo(cmdid, status)
c
c  Get image from the HEASARC skyview facility
c
      elseif ( command.eq.'SKYVIEW' ) then
         call skyview (cmdid, status)
c
c  Retrieve finding chart from online service
c
      elseif ( command.eq.'FINDING_CHART' ) then
         call finding_chart(cmdid, status)
c
c  pix to ra
c
      elseif ( command.eq.'PIXEL_TO_RA_DEC' ) then
         call pix2ra(cmdid, status)
c
c  angular offset
c
      elseif ( command.eq.'OFFSET' ) then
         call offset(cmdid, status)
c
c  define area in the image and output on the screen the pointsc
c  coordinates
c
      elseif ( command.eq.'VALUE' ) then
         call mapidx('DIS',imap,status)
         call polygon(cmdid, memr(p_map(imap)), mszx(imap), mszy(imap), 
     &                mapids(imap), status)
c
c  convert coordinates
c
      elseif ( command.eq.'COORD' ) then
         call mapidx('CUR',imap,status)
         call coord(cmdid, mapids(imap), status)
c
c  Sum counts in selected area
c
      elseif ( command.EQ.'COUNTS' ) then
         call mapidx('CUR',imap,status)
         call counts(cmdid, memr(p_map(imap)), mszx(imap), mszy(imap),
     &               mapids(imap), status)
c
c  Calculate three sigma upper limit
c
      elseif ( command.EQ.'UPLIMIT' ) then
         call mapidx('CUR',imap,status)
         call uplimit(cmdid, memr(p_map(imap)), mszx(imap), mszy(imap),
     &               mapids(imap), status)
c
c  grid on screen
c
      elseif ( command.EQ.'OGRID' ) then
         call grid(cmdid, status)
c
c  Read image
c
      elseif ( command.eq.'READ_IMAGE' ) then

         call read_image (cmdid,mapids(icurmap),imgcnt,filename, 
     &                    rdmapid,status)
c
c        If first image, and current is MAP1, make copy in MAP2
c        Retains prior save/restore behavior 
c
         if ( status.eq.0 ) then
            call tclresl('set savmode', savmode, status)
            if ( imgcnt.eq.1 .and. savmode .and. status.eq.0 ) then
               write(ds,'(3a)') 'map copy ', rdmapid(:LENACT(rdmapid)),
     &                          ' $savmap'
               call tclrun(ds, status)
            endif
         endif
c
c        Reset title after every read
c
         uptitle = ' '
         lotitle = ' '
c
c  Write image
c
      elseif ( command.eq.'WRITE_IMAGE' ) then
         call write_image (cmdid, mapids(icurmap), status)
c
c  Perform arithmetic with maps
c
      elseif ( command.eq.'MARITH' ) then
         call marith(cmdid, mapids(icurmap), status)
c
c  Perform simple operations on maps
c
      elseif ( command.eq.'MOPER' ) then
         call moper(cmdid, mapids(icurmap), status)
c
c  Remap image to new coordinates
c
      elseif ( command.eq.'REMAP' ) then
         call remap(cmdid, mapids(icurmap), status)
c
c  Crop image
c
      elseif ( command.eq.'CROP' ) then
         call crop(cmdid, mapids(icurmap), status)
c
c  Rotate image
c
      elseif ( command.eq.'ROTATE' ) then
         call rotate(cmdid, memr(p_map(icurmap)), mszx(icurmap),
     &               mszy(icurmap),  mapids(icurmap), status)
c
c  Flip image
c
      elseif ( command.eq.'FLIP' ) then
         call flip(cmdid, mapids(icurmap), status)
c
c  Rebin image
c
      elseif ( command.eq.'REBIN' ) then
         call rebin(cmdid, mapids(icurmap), status)
c
c  Resize image. Change the pixel size within image
c  The new pixel value can be at maximum twice the original pixel size.
c
      elseif ( command.eq.'RESIZE' ) then
         call resize(cmdid, mapids(icurmap), status)
c
c  Rescale image
c
      elseif ( command.eq.'RESCALE' ) then
         call rescale(cmdid, memr(p_map(icurmap)), mszx(icurmap),
     &                mszy(icurmap),  mapids(icurmap), status)
c
c  Draw an x or y slice of the image
c
      elseif ( command.eq.'SLICE' ) then
         
         call mapidx('DIS',imap,status)
         call slice(cmdid, memr(p_map(imap)), mszx(imap), mszy(imap), 
     &              mapids(imap), status)
c
c  Plot image
c
      elseif ( command.eq.'PIMAGE' ) then
         call pimage(cmdid,memr(p_map(icurmap)),mszx(icurmap),
     &               mszy(icurmap),mapids(icurmap),
     &               uptitle,lotitle,vpconfig,maxvps,vpconnum, 
     &               vpnum,vpset,vpframe,numload,ldlevs,
     &               status) 
c
c  Plot contours
c
      elseif ( command.eq.'PCONTOUR' ) then
         call pcontour(cmdid,memr(p_map(icurmap)),mszx(icurmap),
     &                 mszy(icurmap),mapids(icurmap),
     &                 uptitle,lotitle,vpconfig,maxvps,vpconnum, 
     &                 vpnum,vpset,vpframe,numload,ldlevs,status) 
c
c  3D Surface
c
      elseif ( command.eq.'SURFACE' ) then
         call surface(cmdid, memr(p_map(icurmap)), mszx(icurmap),
     &                mszy(icurmap), mapids(icurmap),
     &                vpconfig, maxvps, vpconnum, vpnum, vpset, status)
c
c  Set color table. The table are read during the start up
c
      elseif ( command.eq.'CCT' ) then
         call cct(cmdid, status)
c
c  Set minimum level
c
      elseif ( command.eq.'SMC' ) then
         call smc(cmdid, status)
c
c  Show available colors
c
      elseif ( command.eq.'COLORS' ) then
         call colors(cmdid, vpset, status)

c
c  Show color scale
c
      elseif ( command.eq.'SCALE' ) then
         if ( vpfile.ne.' ' ) then
            call set_vpcbnds(maxvps,vpconfig,vpconnum,vpset)
         endif
         call scale (cmdid,vpnum,vpset,status)
c
c Set viewport
c
      elseif ( command.eq.'VIEWPORT' ) then
         call viewport(cmdid,maxvps,vpset,vpfile,vpnum,
     &                 vpconfig,vpconnum,vpframe,status)
c
c Manipulate levels
c
      elseif ( command.eq.'LEVELS' ) then
         call levels(cmdid,
     &               memr(p_map(icurmap)),mszx(icurmap),mszy(icurmap),
     &               mapids(icurmap),numload,ldlevs,status)
c
c Calculate levels from background
c
      elseif ( command.eq.'BGLEVELS' ) then
         call bglevels(cmdid,
     &               memr(p_map(icurmap)),mszx(icurmap),mszy(icurmap),
     &               mapids(icurmap),numload,ldlevs,status)
c
c Add a label in a specified (X,Y) place on the image
c
      elseif ( command.eq.'LABEL' ) then
         call label(cmdid,status)
c
c Add a label relative to the viewport
c
      elseif ( command.eq.'VPLABEL' ) then
         if ( vpfile.ne.' ' ) then
            call set_vpcbnds(maxvps,vpconfig,vpconnum,vpset)
         endif
         call vplabel(cmdid,vpset,status)
c
c Titling image
c
      elseif ( command.eq.'TITLE' ) then
         call title(cmdid,uptitle,lotitle,status)
c
c Print timestamp in corner of image
c
      elseif ( command.eq.'TIMESTAMP' ) then
         call timestamp(cmdid, status)
c
c  region circle
c
      elseif ( command.EQ.'CIRCLEREG' ) then
         call circlereg(cmdid,status)
c
c  region box
c
      elseif ( command.EQ.'BOXREG' ) then
         call boxreg(cmdid,status)
c
c  Close the display
c
      elseif ( command.eq.'CLOSE_PG_WINDOW' ) then
         call closepg()
c
c  Calculate background
c
      elseif ( command.eq.'BACKGROUND' ) then
         call background(cmdid, memr(p_map(icurmap)), mszx(icurmap), 
     &                   mszy(icurmap), mapids(icurmap), status)
c
c  PSF estimation
c
      elseif ( command.eq.'PSF' ) then
         call psf(cmdid, memr(p_map(icurmap)), mszx(icurmap), 
     &            mszy(icurmap), mapids(icurmap), status)
c
c  Find excesses
c
      elseif ( command.eq.'EXCESS' ) then
         call excess(cmdid, memr(p_map(icurmap)), mszx(icurmap),
     &               mszy(icurmap), mapids(icurmap), status)
c
c  Search sources
c
      elseif ( command.eq.'SEARCH' ) then
         call search(cmdid, memr(p_map(icurmap)), mszx(icurmap),
     &               mszy(icurmap), mapids(icurmap), status)
c
c  Source statistics
c
      elseif ( command.eq.'SOSTA' ) then
         call sosta(cmdid, memr(p_map(icurmap)), mszx(icurmap),
     &              mszy(icurmap), mapids(icurmap), status)
c
c  Remove sources
c
      elseif ( command.eq.'REMOVE_SOURCES' ) then
         call remove_sources(cmdid, memr(p_map(icurmap)), mszx(icurmap),
     &                       mszy(icurmap), mapids(icurmap), status)
c
c
c  Extract spectrum/lightcurve
c
      elseif ( command.eq.'EXTRACT' ) then
         call extract(cmdid, filename, status)
c
c  Smooth image
c
      elseif ( command.eq.'SMOOTH' ) then
         call smooth(cmdid, memr(p_map(icurmap)), mszx(icurmap),
     &               mszy(icurmap), mapids(icurmap), status)
c
c  Apply vignetting correction
c
      elseif ( command.eq.'VIGNETTING' ) then
         call vignetting(cmdid,mapids(icurmap), status)
c
c  Calculate centroid
c
      elseif ( command.eq.'CENTROID' ) then
         call centroid(cmdid, memr(p_map(icurmap)), mszx(icurmap),
     &               mszy(icurmap), mapids(icurmap), status)
c
c  Online help
c
      elseif ( command.eq.'HELP' ) then
         call help(cmdid, version, status)
c
c  Exit from Ximage
c
      elseif ( command.eq.'XAN::CLEANUP' ) then
         call pgend
         call setdefault(status)
c
c  Fail if not matched
c

      else
         call xwrite('Command not found in ximcmd:', 5)
         call xwrite(command, 5)
         status = -1

      endif

      RETURN
      END
