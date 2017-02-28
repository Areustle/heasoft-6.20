C       subroutine profile
C
C
C  $Id: profile.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++   Effect: plot map profiles with PGPLOT subroutines
C
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C
C=======================================================================
C  $Log: profile.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/12/26 17:42:55  irby
C  Fix read/write statements (for f90 compatibility): add/remove commas and
C  fix lines over 72 chars.
C
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:21  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:39  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       subroutine profile
C  Common blocks used:
        INCLUDE '../COMMON/ctlrep.copy'
        INCLUDE '../COMMON/cnfrep.copy'
        INCLUDE '../COMMON/roirep.copy'
        INCLUDE '../COMMON/maprep.copy'
        INCLUDE '../COMMON/gasrep.copy'
        INCLUDE  '../COMMON/errrep.copy'
        INCLUDE '../COMMON/emprep.copy'
        INCLUDE '../COMMON/bmprep.copy'
        INCLUDE '../COMMON/likrep.copy'

        save

	character(80) id
	common /id/id
        logical longflag,maxset
        real gx(1000),gy(1000),gup(1000),gdn(1000),gas(1000),
     &       flat(1000),psf(1000)
        character(70) XSTRING,YSTRING,TITLE,DTEXT1
        CHARACTER input*80,guess*11,int_str*18

        REAL MAPVAL
	data  nabscissa /1/

	id = '$Id: profile.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
        LOC='PROFILE'
	if(jae_profile)then
		print *,' '
		print *,'LOC:',LOC
		print *,'publish:',publish
		print *,' '
	endif
         if(roiorg(1).gt.roiend(1)) then
            write(*,'("PROFILE: ROI LONGITUDE is smaller at end ",
     &                "than beginning.  Please change ROI."/
     &                " You may create this profile by writing ",
     &                "maps (OMx) with this ROI and then"/
     &                "running LIKE with them.")')
           return
         endif

c       set default plot parameters
        ratio=(roiend(1)-roiorg(1))/(roiend(2)-roiorg(2))
        if(ratio.lt.1.) then
          longflag=.false.
         else
          longflag=.true.
        endif

100     write(lu(1),'("Will plot ROI:",f7.2," to",
     &  f7.2,"; and",f7.2," to",f7.2/
     &  "OK? (cr to proceed with plot, P to adjust plot parameters,"
     &  ," or R to return to main menu)",$)')
     & roiorg(1),roiend(1),roiorg(2),roiend(2)
        read(lu(12),'(A)')input
	if(jae_profile)then
		print *,' '
		print *,'LOC:',LOC
		print *,'in 100 loop'
		print *,'publish:',publish
		print *,' '
	endif
	if(input.eq.'R'.or.input.eq.'r')then
		call pgend
		return
	endif
        if(input.eq.'p'.or.input.eq.'P') then
c         plot adjust menu section
        write(*,'("graphics device? (return for ",A10,"):",$)')g_device
        read(LU(12),'(A)')input
        if(input.ne.' ')g_device=input

        if(ratio.lt.1.) then
          guess='latitude'
          longflag=.false.
         else
          guess='longitude'
          longflag=.true.
        endif
           write(lu(1),'("Looks like you want to graph over ",A11,
     &    " - OK (cr)?",$)')guess
           READ(LU(12),'(A)') input
           if(input.ne.' ') then
             if(longflag) then
               longflag=.false.
              else
               longflag=.true.
             endif
           endif

         WRITE (LU(1),'(
     &''How many pixels to combine along abscissa (cr for'',
     &I4,'') ?'',$)')nabscissa
         READ(LU(12),'(A)') input
         if(input.ne.' ') READ(input,*,end=1111) nabscissa

           write(lu(1),'("Y axis Max value? (cr -> use data)",$)')
           READ(LU(12),'(A)') input
           if(input.ne.' ') then
             maxset=.true.
             read(input,*,end=1111)pmax
            else
             maxset=.false.
           endif
         else ! end plot adjust menu section
          if(input.ne.' ') return
        endif

        if(longflag) then
c       graph over longitude

        ip=0 ! point number
        ymax=-1.e10
        ymin=1.e10
        npixelsum=0
        do ixx=ROIPRG(1),ROIPND(1),nabscissa ! do all abscissa points
          ip=ip+1 ! point number
          if(ip.gt.1000) stop 10 ! storage excedded
          gx(ip)=0.
          gy(ip)=0.
          gas(ip)=0.
          flat(ip)=0.
          psf(ip)=0.
          npixel=0
          do ixxx=1,nabscissa ! integrate over abscissa direction for point
            ix=ixx+ixxx-1 ! abscissa pixel index
            call mapcor(ix,1,xl,xb)
            if(signal.ne.' ') then
              CALL ERROR(0,LOC)
             else
              do iother=ROIPRG(2),ROIPND(2)
c               integrate over orthogonal direction for point
                npixel=npixel+1
                gx(ip)=gx(ip)+xl
                gy(ip)=gy(ip)+mapval(map,ix,iother,CTLMSZ1,CTLMSZ2)
                flat(ip)=flat(ip) +
     &          Gbias*1.e-5*mapval(emap,ix,iother,CTLMSZ1,CTLMSZ2)
                gas(ip)=gas(ip) + 
     &              Gmult*mapval(gasmap,ix,iother,CTLMSZ1,CTLMSZ2)
                psf(ip)=psf(ip) + 
     &           mapval(bmap,ix,iother,CTLMSZ1,CTLMSZ2)
              end do
            endif
          end do ! end integration for point
          npixelsum=npixelsum+npixel
          gx(ip)=gx(ip)/float(npixel)
          gup(ip)=gy(ip)+sqrt(abs(gy(ip)))
          gdn(ip)=2.*gy(ip)-gup(ip)
          gas(ip)=gas(ip) + flat(ip)
          psf(ip)=psf(ip) + gas(ip)
          if(gup(ip).gt.ymax) ymax=gup(ip)
          if(gdn(ip).lt.ymin) ymin=gdn(ip)
          if(gy(ip).gt.ymax) ymax=gy(ip)
          if(gy(ip).lt.ymin) ymin=gy(ip)
        end do

         else
c       graph over latitude

        ip=0 ! point number
        ymax=-1.e10
        ymin=1.e10
        npixelsum=0
        do ixx=ROIPRG(2),ROIPND(2),nabscissa ! do all abscissa points
          ip=ip+1 ! point number
          if(ip.gt.1000) stop 10 ! storage excedded
          gx(ip)=0.
          gy(ip)=0.
          gas(ip)=0.
          flat(ip)=0.
          psf(ip)=0.
          npixel=0
          do ixxx=1,nabscissa ! integrate over abscissa direction for point
            ix=ixx+ixxx-1 ! abscissa pixel index
            call mapcor(1,ix,xl,xb)
            if(signal.ne.' ') then
              CALL ERROR(0,LOC)
             else
              do iother=ROIPRG(1),ROIPND(1)
c               integrate over orthogonal direction for point
                npixel=npixel+1
                gx(ip)=gx(ip)+xb
                gy(ip)=gy(ip)+mapval(map,iother,ix,CTLMSZ1,CTLMSZ2)
                flat(ip)=flat(ip) +
     &          Gbias*1.e-5*mapval(emap,iother,ix,CTLMSZ1,CTLMSZ2)
                gas(ip)=gas(ip)+Gmult*
     &          mapval(gasmap,iother,ix,CTLMSZ1,CTLMSZ2)
                psf(ip)=psf(ip)+mapval(bmap,iother,ix,CTLMSZ1,CTLMSZ2)
              end do
            endif
          end do ! end integration for point
          npixelsum=npixelsum+npixel
          gx(ip)=gx(ip)/float(npixel)
          gup(ip)=gy(ip)+sqrt(abs(gy(ip)))
          gdn(ip)=2.*gy(ip)-gup(ip)
          gas(ip)=gas(ip) + flat(ip)
          psf(ip)=psf(ip) + gas(ip)
          if(gup(ip).gt.ymax) ymax=gup(ip)
          if(gdn(ip).lt.ymin) ymin=gdn(ip)
        end do
        endif
        ymin=0. ! 3/5/92

        if(longflag) then
          if(coord_sys.eq.'G') then
            write(XSTRING,'("Galactic longitude (degrees)")')
           elseif(coord_sys.eq.'C') then
            write(XSTRING,'("Right Ascension (J2000; degrees)")')
           else
            write(XSTRING,'("Map longitude (degrees)")')
          endif
         else
          if(coord_sys.eq.'G') then
            write(XSTRING,'("Galactic latitude (degrees)")')
           elseif(coord_sys.eq.'C') then
            write(XSTRING,'("Declination (J2000; degrees)")')
           else
            write(XSTRING,'("Map latitude (degrees)")')
          endif
        endif

        if(longflag) then
          if(coord_sys.eq.'C') then
            int_str='declination range '
           else
            int_str=' latitude range   '
          endif
        write(YSTRING,'("Sum of counts for ",A18
     &  ,f7.2," to",f7.2)')int_str,ROIORG(2)-ctlscl/2.,
     &   ROIEND(2)+ctlscl/2.
         else
          if(coord_sys.eq.'C') then
            int_str='right ascen. range'
           else
            int_str=' longitude range  '
          endif
	  begin_long=ROIORG(1)-ctlscl/2.
	  end_long=ROIEND(1)+ctlscl/2.
	  if(begin_long.lt.0..and.end_long.lt.0.)then
	    begin_long=begin_long+360.
	    end_long=end_long+360.
	  endif
          write(YSTRING,'("Sum of counts for ",A18
     &   ,f7.2," to",f7.2)')int_str,begin_long,
     &    end_long
        endif

        if(publish) then
          TITLE=' '
         else
          write(TITLE,'("Likelihood Analysis Profile")')
        endif

        if(longflag) then
	if(gx(1).lt.0..and.gx(ip).lt.0.)then
	  do i=1,ip
	    gx(i)=gx(i)+360.
	  enddo
	endif
	endif

        CALL PGBEGIN(0,g_device,1,1)
        if(publish) CALL PGSLW(3)
        if (.not.maxset) pmax=ymax+0.20*(ymax-ymin)
        if(longflag) then
          CALL PGENV(gx(ip),gx(1),ymin,pmax,0,0)
          txtbgn=gx(ip)-0.05*(gx(ip)-gx(1))
         else
          CALL PGENV(gx(1),gx(ip),ymin,pmax,0,0)
          txtbgn=gx(1)+0.05*(gx(ip)-gx(1))
        endif
        CALL PGLABEL(XSTRING,YSTRING,TITLE)
        CALL PGPOINT(ip,gx,gy,17)
        CALL PGERRY(ip,gx,gup,gdn,1.)
        CALL PGSLS(4)
        CALL PGLINE(ip,gx,flat)
        CALL PGSLS(2)
        CALL PGLINE(ip,gx,gas)
        CALL PGSLS(1)
        CALL PGLINE(ip,gx,psf)
	if(.not.publish)then
        write(DTEXT1,'("Energy,",2i6,"; Gmult,",e11.4,"; Gbias",e11.4)')
     &  CTLEMN,CTLEMX,Gmult,Gbias
          CALL PGtext(txtbgn,ymin+0.95*(pmax-ymin),DTEXT1)
          CALL PGtext(txtbgn,ymin+0.91*(pmax-ymin),
     &        'CMAP:'//mapdoc(1)(1:60))
          CALL PGtext(txtbgn,ymin+0.87*(pmax-ymin),
     &        'GMAP:'//gasdoc(1))
          CALL PGtext(txtbgn,ymin+0.83*(pmax-ymin),
     &'Dotted line, isotropic; dashed line, GMAP; solid line, PSF')

	chisqd=0.
	n_points=0.
	do i=1,ip
	  if(psf(i).gt.0.) then
     	    chisqd=chisqd+ (psf(i)-gy(i))**2/psf(i)
	    n_points=n_points+1
	  endif
	enddo
	write(DTEXT1,'("CHI SQD",e11.4," for",I4," points.")')
     &                    chisqd,n_points
	CALL PGtext(txtbgn,ymin+0.79*(pmax-ymin),DTEXT1)

	if (NSOURCE.eq.0)then
	  DTEXT1='There are no PSFs modeled.'
	 else
	  write(DTEXT1,'(
     &   "There are ",I3," PSFs modeled. Number 1: ")')NSOURCE
	  write(DTEXT1(39:70),'(
     &   "At",f7.2,",",f6.2," are",f5.0," cnts; ")')
     &    SRC_PARMS(1,1),SRC_PARMS(1,2),SRC_PARMS(1,3)
	endif
	CALL PGtext(txtbgn,ymin+0.75*(pmax-ymin),DTEXT1)

c end if not publish 
	endif
	call pgend
        goto 100

1111    write(lu(1),*)'Invalid input, try again.'
        return

        end
c
