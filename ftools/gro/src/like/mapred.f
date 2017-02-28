c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPRED(MAP,MAPTYP,MAPDOC,FLAG)
C
C
C  $Id: mapred.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*             EFFECT: THE specified map (MAPFILE string in
C	cnfrep.copy) IS READ FROM FITS
C*    FLAG=.TRUE. generate error if map inconsistent with CTL
C*    FLAG=.FALSE. reset CTL parameters from values stored with map
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: mapred.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:39  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/03/06  21:28:35  jae
c *** empty log message ***
c
c Revision 5.1  1996/02/29  20:51:59  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:15  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPRED(MAP,MAPTYP,MAPDOC,FLAG)
C  Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'

      save

      character(80) id
      common /id/id
      CHARACTER maptyp*4
c      CHARACTER themapfile*80,ex_dir*60, maptyp*4
      character(70) MAPDOC(10)
      LOGICAL FLAG

      CHARACTER break*80

      id = '$Id: mapred.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      break='----------------------------------------'//
     &     '----------------------------------------'
      LOC='MAPRED '


      if (jae_mapred) write(*,*)'Inside routine mapred'

      gmap_E_dif=.false.
C
      WRITE (LU(1),'("Reading file: ",A)')MAPFILE
      WRITE (LU(1),*)break

c     call getenv('FITS_DIR',ex_dir)
c     i=index(ex_dir,' ')-1
c     if(i.lt.2)then
c     environmental variable not set - read from current directory
c     go to 20
c     endif
c     if(MAPFILE(1:i).eq.EX_DIR(1:i))goto 20
c     
c     themapfile=MAPFILE
c     MAPFILE=ex_dir(1:i)//"/"//MAPFILE
c     TMPLOC=' '
c     istatus=-1
c     istop=-1
c     if(jae_mapred)istop=0
c     CALL FITSERROR(istop,istatus,TMPLOC)
c     IF(SIGNAL.ne.' '.and.status.gt.0)return
c     IF(istatus.eq.0) THEN
c     SIGNAL=' '
c     go to 30
c     ELSE
c     MAPFILE=themapfile
c     i2=index(MAPFILE,' ')
c     
c     if(jae_mapred) WRITE(lu(1),'(
c     & "MAPFILE- ",A)')mapfile(1:i2)
c     if(jae_mapred)
c     &  WRITE(lu(1),*)"Will check given path directory."
c     ENDIF
c
 20   continue                  ! read from current directory
c     istop=-1
c     if(jae_mapred)istop=0
c     istatus=-1
c     SIGNAL=' '
c     CALL FITSERROR(istop,istatus,TMPLOC)
c     if(SIGNAL.ne.' ')return
C     
 30   continue 
      NFILE=0                   ! look at the header
      CALL FITRED(MAP,MAPTYP,MAPDOC,NFILE,flag)
      
      IF (SIGNAL.ne.' ') return

      if (maptyp.eq.'CMAP') Nfits_energy=NFILE

      if (NFILE.eq.0) NFILE=1     ! This happens for naxis=2

      if (.not.flag) then        ! ctl parms to be set by map
         if (NFILE.eq.1) THEN
            IFILE=1
            WRITE(LU(1),*)'Reading image with energy range: '
     &           ,energy(1,1),energy(2,1),' MeV'
         else
            WRITE(LU(1),'("THERE ARE ",i2," IMAGES with energy ",
     &           "ranges:")') NFILE

            do n=1,NFILE
               WRITE(LU(1),'(i3,i6," MeV - ",i6," MeV")')
     &              N,energy(1,N),energy(2,N)
            enddo

 40         WRITE(LU(1),*)'WHICH DO YOU WANT? '
            READ(LU(12),*,end=40)IFILE
            IF(IFILE.GT.NFILE.or.IFILE.lt.1) GOTO 40
         endif

         ctlemn=energy(1,IFILE)
         ctlemx=energy(2,IFILE)
         zenith_cut=zenith(IFILE)

      else                      ! ctl parms to agree with map - choose correct energy
         do IFILE=1,NFILE
	    if (abs(float(ctlemn)-energy(1,IFILE)).lt.0.2) then
               if (abs(float(ctlemx)-energy(2,IFILE)).lt.0.2) goto 50 
c     you got your map
            endif
         enddo
         
c     Can't find energy range
         SIGNAL='E'
         WRITE(SIGMSG,'("MAPRED: Energies",2f7.0," not in ",
     &        "fits file.")') ctlemn,ctlemx

         if (maptyp.ne.'GMAP') then
c     Toss in the towel
	    RETURN
         else
c     write(lu(1),*)'GMAP energy range does not match CMAP.'
c     maybe this is OK
	    if (NFILE.ne.1) then
               CALL ERROR(0,LOC)
               WRITE(6,*)
     &              'Wow - lots of software development has happened ',
     *              'since 1993.'
               WRITE(6,*)
     &              'Some one must now change this program.'
               WRITE(SIGMSG,*)'MAPRED: Multiple gas images.'
               SIGNAL='E'
               RETURN
	    endif

	    IFILE=1

	    if (abs(float(ctlemn)-energy(1,IFILE)).lt.0.2.and.
     &           ctlemx.gt.9999.and.energy(2,IFILE).gt.9999) then
               gmap_E_dif=.false. !no practicle difference
            else
               gmap_E_dif=.true.
	    endif
         endif
      endif
 50   continue

      if (MAPTYP.eq.'EMAP'.and.
     &     abs(zenith_cut-zenith(IFILE)).gt.0.1) then         
         SIGNAL='E'
         WRITE(SIGMSG,*)
     &        'MAPRED: Zenith cut of exposure map differs from counts.'
         RETURN
      endif

      CALL FITRED(MAP,MAPTYP,MAPDOC,IFILE,flag)
      RETURN
      
 1111 write(lu(1),*)'Invalid input, try again.'
      return      
      END
c
