      subroutine xim_startup(Standalone, Prname, Version, Status)
      implicit none
c
c  XIMAGE startup process 
c  Prints startup message
c  Fills mission info and color table arrays
c
c  I  standalone   (l) Interactive exececution (y/n)
c  O  prname       (s) Program name
c  O  version      (s) Program version
c  O  status       (i) Error flag (0=OK)
c
      logical Standalone
      character*(*) Prname, Version
      integer*4 Status
c
c  Externals
c
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/sitedef.inc'
      INCLUDE '../include/colordef.inc'

      integer*4 LENACT
c
c Local variables
c
      integer*4 i , lun, ios
      character*(MAX_FILELEN) mdbfile , infile, colstart
      character(100) ds
c
      call GTINIT
c
c Print startup message
c
      call prstart(Standalone, Prname, Version, Status)
      if ( Status.ne.0 ) return
c
c Set default command file extension
c
      call XSETIN('xco')
c
c Load the satellite/detector from XIMAGE.INF
c
      mdbfile = 'ximage.mdb'
      call PTEND(CXAn,CMAn,mdbfile)
      call rdmdb(mdbfile,Status)
      IF ( Status.NE.0 ) RETURN
c
c Load the color table filenames
c
      call GETLUN(lun)
      infile = 'color_tables.dat'
      call PTEND(CXAn,CMAn,infile)
      call OPENWR(lun,infile,'old',' ',' ',0,1,ios)
c
      if ( ios.NE.0 ) then
         write (ds,99001) ios , infile(:LENACT(infile))
         call XWRITE(ds,5)
         call FRELUN(lun)
         Status = 1
         return
      endif
c
      i = 0
      ios = 0
      do while ( ios.EQ.0 .and. i.le.ZTAbmax )
         i = i + 1
         read (lun,'(a)',END=100) ZTAb_name(i)
      enddo
 100  close (lun)
      call FRELUN(lun)
      if ( i.gt.ZTAbmax ) then
         call xwarn(' Color table list truncated...', 5)
      endif
      ZTAbnum = i - 1
c
c Load default colour table
c
      if ( ZTAbnum.gt.0 ) then
         colstart = ZTAb_name(1)
         call PTEND(cxan, cpar, colstart)
         call XWRITE('Color table: ',20)
         call XWRITE(colstart(1:LENACT(colstart)),20)
         call read_coltab(CTAB_DEFAULT,colstart)
         call read_coltab(CTAB_CURRENT,' ')
      else
         call XWRITE(' No color tables read in', 5)
         Status = -1
      endif
c
c Initialize PGPLOT standard colors (0-15)
c
      do i = 0, 15
         PGRvals(i+1) = -1.0
         PGGvals(i+1) = -1.0
         PGBvals(i+1) = -1.0
      enddo
c
c Set null pixel color to first after standard colors
c
      NULlcol = 16

      return
99001 FORMAT (' Error',i4,' in opening the ',a)
      end
