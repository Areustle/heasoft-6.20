c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C        SUBROUTINE JFNDPOSA(jflag)
C
C
C  $Id: jfndposa.f,v 1.3 2013/05/21 19:08:25 irby Exp $
c
c
c       For LIKE v5.00
c       programmer: Joseph A. Esposito (jae@egret.gsfc.nasa.gov)
c       final form reached: 11-NOV-93
c
c++     effect: optimizes TS postion for starting positions from file read.
c               input files can have form of PSF file or
c
c               X     Y
c
c       where X,Y are (LONG,LAT) or (RA,Dec)
c
c==========================================================================
C  $Log: jfndposa.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:09  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:34  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:48:19  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:18  jae
c Subroutine Module for like V5.00
c
c% Changes:
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE JFNDPOSA(jflag)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
      logical jflag
      logical determined,FEXIST,jpsfflg,jpsftst
      character inp_jfile*50,text*150,input*50
      dimension svopos(2)
      character text2*30
c
      id = '$Id: jfndposa.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      LOC='JFNDPOSA'
c
      jpsfflg = JFALSE
      jpsftst = JFALSE
      jchs = 0
      do kk=1,50
         input(kk:kk)=' '
      enddo
c
      print *,'  You have requested the automated source locati',
     &     'on procedure.'
      print *,'  Sources will be added to the other PSF map'
      print *,' '
      CALL INTENT(determined)
      if(.not.determined)return
      if(coord_sys.eq.'G')then
         text2 = ' Galactic LON,LAT '
         jchs = 18
      else
         text2 = ' Celestial RA and DEC '
         jchs=22
      endif
      inp_jfile=' '
c
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
      print *,' '
      input = 'Enter the filename containing'//text2(1:jchs)//
     &     'positions'
      print *,input
      print *,' or a PSF filename with positions in ',text2
      write(*,'(" cr to abort: "$)')
      read(LU(12),'(A)')inp_jfile
      if(inp_jfile.eq.' '.or.inp_jfile.eq.' ')return
      text2 = text2(1:jchs-1)//': '
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
      INQUIRE (FILE=inp_jfile,EXIST=FEXIST)
      if(.not.FEXIST)then
         print *,' input file ',inp_jfile,' Does NOT EXIST'
         print *,' returning to main menu'
         print *,' '
         return
      endif
 8    print *,' '
      if(coord_sys.eq.'G')then
         if(jptype)write(input,
     &        '(" Map POINTING (Long,Lat) is: ",f7.2,",",f6.2)')
     *        SC_LJJ,SC_BJJ
         if(jptype.eqv.JFALSE)write(input,
     &        '(" Map CENTER (Long,Lat) is: ",f7.2,",",f6.2)')
     *        SC_LJJ,SC_BJJ
      else
         if(jptype)write(input,
     &        '(" Map POINTING (RA.DEC) is: ",f7.2,",",f6.2)')
     *        SC_RAJ,SC_DECJ
         if(jptype.eqv.JFALSE)write(input,
     &        '(" Map CENTER (RA.DEC) is: ",f7.2,",",f6.2)')
     *        SC_RAJ,SC_DECJ
      endif
c
      write(*,'(A)')input(1:43)
      print *,' Enter the maximum acceptance cone in degrees'
      print *,' cr to accept: ',aspj
      read(LU(12),'(a)') input
      numcar = index(input, ' ') - 1
      if(numcar.eq.0)goto 14
      read(input,*,end=8,err=8)aspj
c
 14   continue
      print *,' '
      open(1,file=inp_jfile,err=999)
      jevt = 0
      if(psfminj.lt.1e-10)psfminj = -1e-10
      if(.not.jpsfflg1)then
         print *,' Enter minimum acceptable TS'
         read(LU(12),*)psfminj
      endif
      do nn=1,150
         text(nn:nn)=char(0)
      enddo
 15   continue
      signal = ' '
      SIGMSG=' '
      jevt = jevt + 1
 16   continue
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
      if(jpsftst.eqv.JFALSE)then
         jpsftst = JTRUE
         read(1,'(a)',end = 997, err = 998) text
         numcar = index(text, ' ') - 1
         if(text(1:24).eq.'Other PSF MAP PARAMETERS'.or.
     &        text(1:13).eq.'PSF MAP PARMS'.or.text(1:3).eq.'PSF')then
c     
            write(*,'(A)')text(1:numcar)
            read(1,'(a)') text
            numcar = index(text, ' ') - 1
c     
            text = text(1:numcar)//' Aspect  '
            numcar=numcar + 9
            write(*,'(A)')text(1:numcar)
            jpsfflg = JTRUE
 17         continue
            read(1,'(a)',end=997,err=998) text
            numcar = index(text, ' ') - 1
            input=text(26:60)
            read(input,*,end=997,err=17)svopos(1),svopos(2)
            if(jpsfflg1)then
               srcN = text(7:24)
               read(text(26:60),*,end=17,err=17)srcL,srcB,counts,gamma
               p_srcL=srcL
               CALL L_CHECK(p_srcL)
               if(p_srcL.lt.ROIORG(1)-CTLSCL/2..or.p_srcL.gt.
     &              ROIEND(1)+CTLSCL/2.)GOTO 17
               if(srcB.lt.ROIORG(2)-CTLSCL/2..or.srcB.gt.
     &              ROIEND(2)+CTLSCL/2.)goto 17
c     
               if(coord_sys.eq.'G')then
                  pL = SC_LJJ
                  pB = SC_BJJ
               else
                  pL = SC_RAJ
                  pB = SC_DECJ
               endif
               tmpL = srcL
               tmpB = srcB
               aspect = gtcirc(tmpL,tmpB,pL,pB)
c     
               if(aspect.gt.aspj)goto 17
               nsrc = NSOURCE + 1
               CALL PIXEL_SELECT(JFALSE,jblank)
               SRC_PARMS(nsrc,8)=1. ! active
               CALL PSFADD(Counts,bmap,CTLMSZ1,CTLMSZ2)
               SRC_PARMS(nsrc,1)=srcL
               SRC_PARMS(nsrc,2)=srcB
               SRC_PARMS(nsrc,3)=Counts
               SRC_PARMS(nsrc,4)=gamma
               SRC_NAMES(nsrc)=srcN
               NSOURCE=NSOURCE+1
               write(*,'(/)')
               write(*,'(i3,1x,A,f7.2$)')NSOURCE,
     &              text(7:numcar),aspect
               LikTotaled=JFALSE
               CALL LIKTOT(map,bmap,gasmap,emap,
     &              CTLMSZ1,CTLMSZ2)
               if(srcN(1:3).eq.'GRO'.or.srcN(1:1).eq.
     &              ' '.or.srcN(1:1).eq.' ')goto 17
               if(coord_sys.ne.'G')then
                  CALL CELGALD('CG',srcL,
     &                 srcB,tmpL,tmpB,iiret)
               else
                  tmpL = srcL
                  tmpB = srcB
               endif
               sv_true_x(nsrc)=tmpL
               sv_true_y(nsrc)=tmpB
               sv_dstsv(6,nsrc)=JTRUE
               write(*,'("  ID"$)')
               goto 17
            endif
            goto 6257
         else
            read(text,*,end = 15, err = 6157)
     &           svopos(1),svopos(2)
            goto 6257
         endif
      endif
c     
      read(1,'(a)',end = 997,err=998) text
      numcar = index(text, ' ') - 1

      if(jpsfflg)then
         input=text(26:60)
         read(input,*,err=6157,end=16)svopos(1),svopos(2)
      else
         read(text,*,err=6157,end=16)svopos(1),svopos(2)
      endif
      goto 6257
 6157 continue
      print *,' Conversion error '
      write(*,'(A)')text(1:numcar)
      goto 15
c     
c
 6257 srcL=svopos(1)
      thelong = srcL
      CALL L_CHECK(thelong)
      if(signal.eq.'A')then
         print *,' POSITION:',svopos(1),svopos(2),
     &        ' is off the map. Reading next position'
         signal=' '
         goto 15
      endif
      iadd = 0
      if(thelong.lt.(srcL-1))iadd = 360
      if(thelong.gt.(srcL+1))iadd = -360
      srcB=svopos(2)
      if(coord_sys.eq.'G')then
         pL = SC_LII
         pB = SC_BII
      else
         pL = SC_RA
         pB = SC_DEC
      endif
      tmpL = srcL
      tmpB = srcB
      aspect = gtcirc(tmpL,tmpB,pL,pB)
      if(aspect.gt.aspj)then
         write(*,'(" Source at:",2(1x,f8.3),
     &        " is outside the cone: ",f6.2," < ",f6.2)')
     *        srcL,srcB,aspj,aspect
         goto 16
      endif
      signal=' '
      print *,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      print *,' '
      print *,'Position:',jevt,srcL,srcB
      srcL = thelong
      print *,' '
      if(jnmflg)then
         do nn=1,18
            srcN(nn:nn)=' '
         enddo
      endif
      CALL JFNDPOS(jflag,JFALSE)
c     
c
      if(signal.ne.' ')then
         print *,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
         print *,' '
         print *,' SIGNAL ERROR:',signal,'  ',SIGMSG
         print *,' ROIORG: ',ROIORG(1),ROIORG(2)
         print *,' ROIEND: ',ROIEND(1),ROIEND(2)
      endif
c     
      if(TS.lt.psfminj)then
         print *,' TS too low to save: ',TS,' < ',psfminj
         goto 15
      elseif(signal.eq.'R')then
         print *,' '
         CALL ERROR(0,LOC)
         signal=' '
         goto15
      endif
c
c
      aspect2 = gtcirc(srcL,srcB,svopos(1),svopos(2))
c     
      if(signal.ne.' '.and.signal.ne.'S')call error(0,LOC)
      srcL = srcL + iadd
c     
      if((signal.eq.'S'.and.jnmflg).or..not.jnmflg)srcN='            '
c     
      write(*,'(6x,A,/,A,2f8.3,/,"Counts: ",f10.2,
     &     "TS: ",f11.2,/,"Distance moved: ",f8.4)',err=996)
     &     srcN(1:12),text,srcL,srcB,counts,TS,aspect2
      aspect=aspect2
C
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
      goto 15
C     
C
 996  print *,' write error to LU 2'
 997  close(1)
      close(2)
      return
 998  print *,' READ ERROR ABORT'
      write(2,'(''READ ERROR ABORT'')')
      goto 997
 999  print *,' OPENING FILE ERROR ABORT'
      goto 997
      end
c
