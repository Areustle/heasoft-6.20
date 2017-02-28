C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE CNFCAL
C
C
C  $Id: cnfcal.f,v 1.4 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C*    CNFCAL = proc(PSMREP)  MODIFIES(PSMREP)
C*           effect: Calibration files  are opened
C=======================================================================
C LIKE Version: 4.8 DELIVERED: November 8th 1993, Programmer J.R. MATTOX
C+            Updated:    by  JRM
C=======================================================================
C  $Log: cnfcal.f,v $
C  Revision 1.4  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2006/04/17 20:54:28  irby
C  Updates for compliance with g95 and gfortran:
C
C  - Replace call to lnblnk with equivalent call to len_trim.
C    lnblnk (a routine which used to live in libg2c) is not currently
C    available with g95 or gfortran.
C
C  - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
C
C  - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
C
C  - Fix non-integer loop variables.
C
C  Revision 1.2  2002/12/26 17:42:53  irby
C  Fix read/write statements (for f90 compatibility): add/remove commas and
C  fix lines over 72 chars.
C
C  Revision 1.1  2002/04/16 20:27:28  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.3  1996/03/07  00:51:58  jae
c *** empty log message ***
c
c Revision 5.2  1996/03/06  23:06:03  jae
c added output debugging lines
c
c Revision 5.1  1996/02/29  20:47:13  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:18  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE CNFCAL

C     Common blocks used:
      INCLUDE '../COMMON/likrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/psmrep.copy'
      INCLUDE '../COMMON/jnfrep.copy'

      save

      integer       status
      character(80)  id, fname
      character(70)  calfile*70
      LOGICAL       FEXIST

      common /id/id


      id = '$Id: cnfcal.f,v 1.4 2013/05/21 19:08:25 irby Exp $'
      loc='CNFCAL'

      write(LU(1),*)
      write(LU(1),*) 'Reading EGRET Calibration files, this may ',
     *     'take a moment...'
      write(LU(1),*)
c     if(initial) WRITE(LU(1),*)'Using CALFILEs with suffix:',fileid

c
csb  create bin and fits sar calibration file names
c
      fname = 'sarfil' // fileid
      sarfile = cal_bin_dir(1:len_trim(cal_bin_dir)-1) // fname
      calfile = calib_dir(1:len_trim(calib_dir)-1) //
     *     fname(1:len_trim(fname)) // '.fits'

c     write(*,*),'sarfile: ', sarfile
c     write(*,*),'sarfilefits: ', calfile


      INQUIRE (FILE=calfile,EXIST=FEXIST)
      IF (.not.FEXIST) THEN
         SIGNAL='N'
         if (jae_cnfcal) write(*,*)"CNFCAL: SARFILE DOESN'T EXIST"
         WRITE(SIGMSG,*)"CNFCAL: calfile DOESN'T EXIST"
         RETURN
      ENDIF

      call fileexists(sarfile, clobber, status)
      call fits2cal(fileid, calfile, sarfile)

      OPEN(LU(21),FILE=sarfile,ACCESS='DIRECT',RECL=80,ERR=900)

c
csb  create bin and fits edp calibration file names
c

      fname = 'edpfil' // fileid
      edpfile = cal_bin_dir(1:len_trim(cal_bin_dir)-1) // fname
      calfile = calib_dir(1:len_trim(calib_dir)-1) //
     *     fname(1:len_trim(fname)) // '.fits'
      
c     write(*,*),'edpfile: ', edpfile
c     write(*,*),'edpfilefits: ', calfile


      INQUIRE (FILE=calfile,EXIST=FEXIST)
      IF (.not.FEXIST) THEN
         SIGNAL='N'
         WRITE(SIGMSG,*)"CNFCAL: EDPFILE DOESN'T EXIST"
         RETURN
      ENDIF

      call fileexists(edpfile, clobber, status)
      call fits2cal(fileid, calfile, edpfile)
      
      OPEN(LU(22),FILE=edpfile,ACCESS='DIRECT',RECL=408,ERR=900)

c
csb  create bin and fits psd calibration file names
c
      fname = 'psdfil' // fileid
      psdfile = cal_bin_dir(1:len_trim(cal_bin_dir)-1) // fname
      calfile = calib_dir(1:len_trim(calib_dir)-1) //
     *     fname(1:len_trim(fname)) // '.fits'
      
c     write(*,*),'psdfile: ', psdfile
c     write(*,*),'psdfilefits: ', calfile


      INQUIRE (FILE=calfile,EXIST=FEXIST)
      IF (.not.FEXIST) THEN
         SIGNAL='N'
         WRITE(SIGMSG,*) "CNFCAL: PSDFILE DOESN'T EXIST"
         RETURN
      ENDIF

      call fileexists(psdfile, clobber, status)
      call fits2cal(fileid, calfile, psdfile)

      OPEN(LU(23),FILE=psdfile,ACCESS='DIRECT',RECL=408,ERR=900)
C
      RETURN
C
 900  SIGNAL='A'
      WRITE(SIGMSG,'('' PSMCNF: Unable to open file. '')') 

      RETURN
      END
