C       SUBROUTINE AUTO_OUT(I)
C
C  $Id: auto_out.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++     effect: Save PSF array information to file during iterations of
c		LM and LPO[N].  A PSF or LPIW format file is saved 
c		each iteration for LM and LPO[N] respectively
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     Integer   1-> PSF output format 2-> LPIW output format else return

C=======================================================================
C LIKE Version: 5.0 DELIVERED: July 18th 1996, Programmer J.A.Esposito
C+            Updated: by JAE
c
C=======================================================================
C  $Log: auto_out.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:27  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.6  1996/08/14  14:59:26  jae
c added type 4 for LPI sub M command (LPIM.*)
c output.
c
c Revision 5.5  1996/08/13  15:44:10  jae
c Added switch "3" to output from LPEA
c command.  Also took out return on !autooutput
c to enable final outputs automatically regardless
c of status of autooutput flag position.  This will
c cause files to be generated during LPI sub M every
c time a source is found bu that is a small price to
c pay.
c
c Revision 5.4  1996/07/31  17:14:02  jae
c included jae_auto_out flag
c
c Revision 5.3  1996/07/23  20:39:55  jae
c Revised output of coord_phr.
c
c Revision 5.2  1996/07/23  20:03:10  jae
c Added char*10 declaration for coord_phr
c variable.
c
c Revision 5.1  1996/07/18  19:36:15  jae
c Fixed type on line 126 (collumn 6 continuation)
c
c Revision 5.0  1996/07/18  14:46:43  jae
c Subroutine Module for like V5.00
c
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE AUTO_OUT(I)
C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/nmprep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
c
      character(24) fdate,fdate1
      character coord_phr*10
c
      character(150) moreinput
c
c
c
      LOC='AUTO_OUT'
      id='$id$'
      if(jae_auto_out)write(*,'("In routine ",a)') LOC
      if(I.ne.isav)nwrit=0
      isav=I
c
      fdate1=fdate()
      do j=1,24
         if(fdate1(j:j).eq.' ')fdate1(j:j)='-'
      enddo
c
      if(I.eq.1)then
         moreinput='LM.'//fdate1(1:19)//'     '
         goto 10
      elseif(I.eq.2)then
         moreinput='LPON.'//fdate1(1:19)//'     '
         goto 500
      elseif(I.eq.3)then
         moreinput='LPEA.'//fdate1(1:19)//'     '
         goto 500
      elseif(I.eq.4)then
         moreinput='LPIM.'//fdate1(1:19)//'     '
         goto 500
      else
         write(*,*)'***************************************'
         write(*,*)' '
         write(*,'("Improper output action number:",a)') I
         write(*,*)'Report this to like maintenance !'
         goto 701
      endif
c
c
c
 10   open(8,file=moreinput,err=699)
      nwrit=nwrit+1
      if(coord_sys.eq.'C') then
         coord_phr='RA     DEC'
      else
         coord_phr=' L       B'
      endif

      nc = index(cmapfile, ' ') - 1
      if(nc.le.0)nc=1
      write(8,'(
     &     "Other PSF MAP PARAMETERS for ",I5,"< E <",I5,
     *     " cnts file:",A)')
     &     CTLEMN,CTLEMX,cmapfile(1:nc)
      write(8,'("      NAME                ",A,
     &     "         CNTS   Sp. I.   Active      TS")')coord_phr(1:10)
      do nsrc=1,NSOURCE
         theLong=SRC_PARMS(nsrc,1)
         if(theLong.lt.0.)theLong=theLong+360.
         write(8,'(
     &        6x,A18,2f8.3,f10.1,f6.2,f9.2$)')
     &        SRC_NAMES(nsrc),theLong,(SRC_PARMS(nsrc,ip),ip=2,4),
     &        SRC_PARMS(nsrc,8)
         if(sv_dstsv(1,nsrc))then
            write(8,'(5x,F8.2)')svn_TS(nsrc)
         else
            write(8,*)' '
         endif
      enddo
      close(8)
      return
 500  continue
      nc = index(cmapfile, ' ') - 1
      if(nc.le.0)nc=1
      open(8,file=moreinput,err=699)
      nwrit=nwrit+1
      write(8,'("FULL PSF INFORMATION ",A,2I7," Z ",A)')
     &     coord_sys,CTLEMN,CTLEMX,cmapfile(1:nc)
      do jj=1,NSOURCE
         write(8,*)SRC_PARMS(jj,1),
     &        SRC_PARMS(jj,2),SRC_PARMS(jj,3),SRC_PARMS(jj,4),
     &        SRC_PARMS(jj,8),svn_TS(jj),sv_flx(1,jj),sv_flx(2,jj),
     &        sv_cnts(1,jj),sv_cnts(2,jj),sv_expose(jj),sv_upperlim(jj),
     &        sv_params(1,jj),sv_params(2,jj),sv_true_x(jj),
     &        sv_true_y(jj),best_x(jj),best_y(jj),sv_err68(jj),
     &        sv_err95(jj),sv_gal_long(jj),sv_gal_lat(jj),
     &        sv_cel_long(jj),sv_cel_lat(jj)
         write(8,*)(sv_dstsv(nn,jj),nn=1,10)
         write(8,'(A)')SRC_NAMES(jj)
      enddo
      close(unit=8)
      return
 699  continue
      close(unit=8)
      write(*,*)'********************************************* '
      write(*,*)' '
      write(*,'("ERROR: Could not open auto-output file: ",a)')
     &     moreinput(1:30)
 701  write(*,*)'Disabling auto-output option '
      write(*,*)' '
      write(*,*)'********************************************* '
      autooutput=.false.
      return
      end
	
