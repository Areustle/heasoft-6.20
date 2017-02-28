C       SUBROUTINE ROIADJ
C
C
C  $Id: roiadj.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++     effect: Provide for Region of Interest adjustment
C
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C
C=======================================================================
C  $Log: roiadj.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:43  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/03/22  22:30:12  jae
c repaired ctlorg and ctlend to reflect ctlscl/2
c offset of bin center
c
c Revision 5.1  1996/02/29  20:53:15  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:10  jae
c Subroutine Module for like V5.00
c
C%   Changes:                                                           
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE ROIADJ
C
C     Common blocks used:
C
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/likrep.copy'

      save 

      character(80)  id
      character(50)  input
      integer       do_this
      logical       Lat_profile,Lat_pref,InLat,InLong,initial_adj
      data          Lat_pref /.true./
      data          initial_adj /.true./

      common /id/id

      id = '$Id: roiadj.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='ROIADJ'
      dt = CTLSCL/2.0
      do_this = 0
	

      if (initial_adj) then
         write(6,*)'The Region Of Interest (ROI) is a selected',
     &        ' rectangle within the map.'
         write(6,*)'It is the region profiled (command OP),',
     &        ' and the region for which'
         write(6,*)'each pixel is examined for a point source',
     &        ' (with the MS command in LIKE).'
         write(6,*)'Note that the ROI does not influence which',
     &        ' data is used by LIKE for a point'
         write(6,*)'source analysis for a specific point. This',
     &        ' is always the region within the' 
         write(6,*)
     &        'map within Ranal of the pixel containing the point.'
         write(6,*)
         initial_adj=.false.
      endif

      write(6,*)'Current ROI is:'
      if (coord_sys.eq.'C') then
         write(6,'("RA.",f8.3," to",f8.3,"; Dec.",f8.3," to",
     &        f8.3)') ROIORG(1),ROIEND(1),ROIORG(2),ROIEND(2)
      else
         write(6,'("Long.",f8.3," to",f8.3,"; Lat.",f8.3," to",
     &        f8.3)') ROIORG(1),ROIEND(1),ROIORG(2),ROIEND(2)
      endif

 1    write(6,*)
      if (coord_sys.eq.'C') then
         write(6,
     &        '("ROI_ADJ:Q; S; T; center; ra1,ra2,d1,d2; .; or ?>>",$)')
      else
         write(6,
     &        '("ROI_ADJ:Q; S; T; center; l1,l2,b1,b2; .; or ?>>",$)')
      endif

      READ(LU(12),'(A)') input

      if (input.eq.'?') then
         if (coord_sys.eq.'C') then
            write(6,'(
     & "              ROI adjustment Sub-menu:",/,
     & "Enter Q to return to main menu without making change;",/,
     & "      T to toggle the default profile direction;",/,
     & "      one number for the center of a square ROI;",/,
     & "      two numbers (ra,dec) for the center of a square ROI;",/,
     & "      four numbers (ra1,ra2,d1,d2) to specify the ROI;",/,
     & "      . to set ROI to the entire map;",/,
     & "      S to set ROI to a 20 deg sq centrered at the anal pt;",/,
     & "      ? print this list.")')
         else
            write(6,'(
     & "              ROI adjustment Sub-menu:",/,
     & "Enter Q to return to main menu without making change;",/,
     & "      T to toggle the default profile direction;",/,
     & "      one number for the center of a square ROI;",/,
     & "      two numbers (l,b) for the center of a square ROI;",/,
     & "      four numbers (l1,l2,b1,b2) to specify the ROI;",/,
     & "      . to set ROI to the entire map;",/,
     & "      S to set ROI to a 20 deg sq centrered at the anal pt;",/,
     & "      ? print this list.")')
         endif

         go to 1

      elseif (input.eq.'Q'.or.input.eq.'q'.or.input.eq.' ') then
         return

      elseif (input.eq.'S'.or.input.eq.'s') then
         xl1=srcL-10.
         xl2=srcL+10.
         xb1=srcB-10.
         xb2=srcB+10.
         call L_CHECK(xl1)
         call L_CHECK(xl2)

         do_this = 1
c         goto 103

      elseif (input.eq.'T'.or.input.eq.'t') then
         if (Lat_pref) then
            Lat_pref=.false.
            if (coord_sys.eq.'C') then
               write(6,'(
     &              "If center is in both RA and DEC range",/,
     &              " of map, a RA profile will be done.")')
            else
               write(6,'(
     &              "If center is in both long and lat range",/,
     &              " of map, a longitude profile will be done.")')
            endif
         else
            Lat_pref=.true.
            if (coord_sys.eq.'C') then
               write(6,*)
     &              "If center point is in both RA and DEC range",
     &              " of map, a DEC profile will be done."
            else
               write(6,*)
     &              'If center point is in both long and lat range',
     &              ' of map, a latitude profile will be done.'
            endif
         endif
         
         goto 1

      elseif (input.eq.'.') then
         CALL ROISET(CTLORG(1),CTLORG(2),CTLEND(1),CTLEND(2),
     &        ROIORG,ROIEND,ROIPRG,ROIPND)
         CALL ERROR(1,LOC)
         write(6,*)'Setting ROI to entire map:'
         go to 200

      else
         READ(input,*,end=102)xlc,xbc
         READ(input,*,end=101)xl1,xl2,xb1,xb2 
 100     call L_CHECK(xl1)
         call L_CHECK(xl2)
         if (signal.ne.' ') then
c     
c     L is off the map
c     
            SIGNAL=' '
         endif

         do_this = 1
         goto 103

 101     continue

         xl2=xlc
         call L_CHECK(xl2)
         if (signal.ne.' ') then
c
c     L is off the map
c
            SIGNAL=' '
            goto 1
         endif

         print *,'You have elected to set the roi to a square ',
     *        'centered on ', xlc, xbc
         write(6,'("Input half width:",$)')
         read(LU(12),*,end=1)width
         xl1=xlc-width
         xl2=xlc+width
         xb1=xbc-width
         xb2=xbc+width
         goto 100

 102     continue
         center=xlc

         if (center.ge.ctlorg(2)-dt.and.center.le.ctlend(2)+dt) then
            InLat=.true.
         else
            InLat=.false.
         endif

         xcenter=center
         call L_CHECK(xcenter)
         if (signal.ne.' ') then
c
c     L is off the map
c
            SIGNAL=' '
            InLong=.false.
         else
            InLong=.true.
         endif

         if ((.not.InLong).and.(.not.InLat)) then
            WRITE(LU(1),'("Center",f6.2," not in map. ROI ",
     &           "unchanged.")') center
            goto 1
         endif

         if ((.not.InLong).and.InLat) then
            Lat_profile=.false.
         elseif (InLong.and.(.not.InLat)) then
            Lat_profile=.true.
         else
            if (Lat_pref) then
               Lat_profile=.true.
            else
               Lat_profile=.false.
            endif
         endif

         if (.not.Lat_profile) then
c
c  longitude profile
c
            center=xcenter
            xl1=ctlorg(1)
            CALL MAPIXL(xl1,center,ILx,IBx)
            CALL ERROR(1,LOC)
C
C         Find pixel center:
C
            CALL MAPCOR (ILx,IBx,xl1,xb1)
            CALL ERROR(1,LOC)

            if (coord_sys.eq.'C') then
               WRITE(6,'(
     &              "Will center ROI strip on declination",f7.2,$)')xb1
            else
               WRITE(6,
     &              '("Will center ROI strip on latitude",f7.2,$)')xb1
            endif

            WRITE(LU(1),'("; Enter width?",$)')
            READ(LU(12),'(A)')input
            READ(input,*,end=1111)width
            npix2=(width/2.-ctlscl/2.)/ctlscl
            xl1=ctlorg(1)
            xb1=center-npix2*ctlscl
            xl2=ctlend(1)
            xb2=center+npix2*ctlscl
            
         else
c
c     latitude profile
c
            center=xcenter
            xb1=ctlorg(2)
            CALL MAPIXL(center,xb1,ILx,IBx)
            CALL ERROR(1,LOC)
C
C     Find pixel center:
C
            CALL MAPCOR (ILx,IBx,xl1,xb1)
            CALL ERROR(1,LOC)
            if (coord_sys.eq.'C') then
               WRITE(6,'("Will center ROI strip on RA",f7.2,$)')xl1
            else
               WRITE(6,'
     &              ("Will center ROI strip on longitude",f7.2,$)')xl1
            endif

            WRITE(LU(1),'("; Enter width?",$)')
            READ(LU(12),'(A)')input
            READ(input,*,end=1111)width
            npix2=(width/2.-ctlscl/2.)/ctlscl
            xl1=center-npix2*ctlscl
            xb1=ctlorg(2)
            xl2=center+npix2*ctlscl
            xb2=ctlend(2)
         endif

         do_this = 1
c 103     CALL ROISET(xl1,xb1,xl2,xb2,ROIORG,ROIEND,ROIPRG,ROIPND)
c         CALL ERROR(0,LOC)
c         goto 200
      endif


 103  if (do_this .eq. 1) then
         CALL ROISET(xl1,xb1,xl2,xb2,ROIORG,ROIEND,ROIPRG,ROIPND)
         CALL ERROR(0,LOC)
      endif

 200  if (coord_sys.eq.'C') then
         write(6,'("RA.",f8.3," to",f8.3,"; Dec.",f8.3," to",
     &        f8.3)') ROIORG(1),ROIEND(1),ROIORG(2),ROIEND(2)
      else
         write(6,'("Long.",f8.3," to",f8.3,"; Lat.",f8.3," to",
     &        f8.3)') ROIORG(1),ROIEND(1),ROIORG(2),ROIEND(2)
      endif

      goto 1

 1111 write(lu(1),*)'Invalid input, try again.'
      goto 1
      END
