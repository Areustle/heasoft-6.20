C*******************************************************************
      SUBROUTINE write_descriptive(fits_unit,newrec1,newrec2,flag,
     &                             status)
C*******************************************************************
C     
      include 'xrrbstr.h'
C
C     Input parameters:
      integer           fits_unit, flag
      RECORD/rnewbuf_rec_1/newrec1      
      RECORD/rnewbuf_rec_2/newrec2      
      RECORD/rnewbuf_me/newme
      RECORD/rnewbuf_gs/newgs
      RECORD/rnewbuf_le/newle
c     
c     flag 1 keywords only in the primary header
c          2 keywords in binary extention
c
c     Output parameters:
      integer       status
c
      INTEGER   LENACT
      character(11)  SHF_YMD
      EXTERNAL  LENACT, SHF_YMD
      integer  yr, daynum, mon, day, hr, mn, sec
      double precision dsec
      character(16)  time_string
      character(80) context
      character(20)  date_string
      INTEGER*2 camera, sdetstr, sdetstp, sndet
      INTEGER exp
C
C     Local parameters:
c
      character(8)   TELESCOP_KEY
      character(40)  TELESCOP_COM
      character(40)  TELESCOP_VAR
      PARAMETER(TELESCOP_KEY='TELESCOP')
      PARAMETER(TELESCOP_COM='mission or telescope name')
c
      character(8)   INSTRUME_KEY
      character(40)  INSTRUME_COM
      character(4)   INSTRUME_VAR
      PARAMETER(INSTRUME_KEY='INSTRUME')
      PARAMETER(INSTRUME_COM='instrument name')
c
      character(8)   DETNAM_KEY
      character(40)  DETNAM_COM
      character(17)  DETNAM_VAR
      PARAMETER(DETNAM_KEY='DETNAM')
      PARAMETER(DETNAM_COM='detector name on source')
c
      character(8)   FILTER_KEY
      character(10)  FILTER_VAR
      character(40)  FILTER_COM
      PARAMETER(FILTER_KEY='FILTER')
      PARAMETER(FILTER_COM='if filter has been used')
c
      character(8)   OBJECT_KEY
      character(40)  OBJECT_COM
      character(20)  OBJECT_VAR
      PARAMETER(OBJECT_KEY='OBJECT')
      PARAMETER(OBJECT_COM='name of the object')
c
      character(8)   RA_KEY
      character(40)  RA_COM
      PARAMETER(RA_KEY='RA_OBJ')
      PARAMETER(RA_COM='source R.A. in degree')
c
      character(8)   DEC_KEY
      character(40)  DEC_COM
      PARAMETER(DEC_KEY='DEC_OBJ')
      PARAMETER(DEC_COM='source Dec. in degree')
c
      character(8)   RA_NOM_KEY
      character(40)  RA_NOM_COM
      PARAMETER(RA_NOM_KEY='RA_PNT')
      PARAMETER(RA_NOM_COM='R.A. nominal pointing in degree')
c
      character(8)   DEC_NOM_KEY
      character(40)  DEC_NOM_COM
      PARAMETER(DEC_NOM_KEY='DEC_PNT')
      PARAMETER(DEC_NOM_COM='Dec. nominal pointing in degree')
c
      character(8)   ROLL_KEY
      character(40)  ROLL_COM
      PARAMETER(ROLL_KEY='ROLLANG')
      PARAMETER(ROLL_COM='EXOSAT roll angle definition in deg')
c
      character(8)   EQUINOX_KEY
      character(40)  EQUINOX_COM
      PARAMETER(EQUINOX_KEY='EQUINOX')
      PARAMETER(EQUINOX_COM='Equinox for ra and dec')
c
      character(8)   RADECSYS_KEY
      character(50)  RADECSYS_COM
      PARAMETER(RADECSYS_KEY='RADECSYS')
      PARAMETER
     & (RADECSYS_COM='world coord. system (FK5 or FK4)')
c
      character(8)   DATE_OBS_KEY
      character(40)  DATE_OBS_COM, DATE_OBS_Y2K_COM
      PARAMETER(DATE_OBS_KEY='DATE-OBS')
      PARAMETER(DATE_OBS_COM='date of observation start (DD/MM/YY)')
      PARAMETER(DATE_OBS_Y2K_COM=
     &                   'date of obs. start (YYYY-MM-DDThh:mm:ss)')
c
      character(8)   TIME_OBS_KEY
      character(40)  TIME_OBS_COM
      PARAMETER(TIME_OBS_KEY='TIME-OBS')
      PARAMETER(TIME_OBS_COM='time of obs. start (hh:mm:ss.ddddd)')
c
      character(8)   DATE_END_KEY
      character(40)  DATE_END_COM, DATE_END_Y2K_COM
      PARAMETER(DATE_END_KEY='DATE-END')
      PARAMETER(DATE_END_COM='date of observation end (DD/MM/YY)')
      PARAMETER(DATE_END_Y2K_COM=
     &                   'date of obs. end (YYYY-MM-DDThh:mm:ss)')
c
      character(8)   TIME_END_KEY
      character(40)  TIME_END_COM
      PARAMETER(TIME_END_KEY='TIME-END')
      PARAMETER(TIME_END_COM='time of obs. end (hh:mm:ss.ddddd)')
c
      character(8)   ORIGIN_KEY
      character(40)  ORIGIN_COM
      PARAMETER(ORIGIN_KEY='ORIGIN')
      PARAMETER(ORIGIN_COM='organization which created this file')
c
      REAL*8        RA_VAL, DEC_VAL, ROLL_VAL
      newgs=newrec2.newgs
      newme=newrec2.newme
      newle=newrec2.newle
c
c Write TELESCOP
       exp=newrec1.expt
       if ((exp.ge.1) .and. (exp.le.4)) then
          TELESCOP_VAR(1:) = 'EXOSAT'
       else if ((exp.ge.5) .and. (exp.le.6)) then
          TELESCOP_VAR(1:) = 'Einstein'
       else
          call xwrite(' Descriptive:Unknown mission!', 10)
          status=1
          return 
       endif
       CALL FTPKYS(fits_unit,TELESCOP_KEY,
     &       TELESCOP_VAR(1:LENACT(TELESCOP_VAR)),TELESCOP_COM, status)
c
c Write INSTRUME
       if (exp.eq.1) then 
          INSTRUME_VAR = 'CMA1'
       elseif(exp.eq.2) then
         INSTRUME_VAR = 'CMA2'
       elseif(exp.eq.3) then
         INSTRUME_VAR = 'ME'
       elseif(exp.eq.4) then
         INSTRUME_VAR = 'GSPC'
       elseif(exp.eq.5) then
         INSTRUME_VAR = 'SSS'
       elseif(exp.eq.6) then
         INSTRUME_VAR = 'MPC'
       else
         call xwrite(' Descriptive:Unknown experiment!', 10)
         status=1
         return
       endif
       CALL FTPKYS(fits_unit,INSTRUME_KEY, 
     &       INSTRUME_VAR(1:LENACT(INSTRUME_VAR)), INSTRUME_COM, status)
c
c Write DETNAM
       camera=newme.instrument 
       sdetstr=newme.startdet
       sdetstp=newme.stopdet
       sndet=newme.ndet
c
       if (exp.EQ.3) then
          CALL write_detnam(exp,camera,sdetstr,sdetstp,
     &                  sndet, detnam_var,status)
          CALL FTPKYS(fits_unit,DETNAM_KEY, 
     &       DETNAM_VAR(1:LENACT(DETNAM_VAR)),DETNAM_COM, status)
       endif
c
c     Write the FILTER keyword only for LE-experiment:
       if ((exp.eq.1) .or. (exp.eq.2)) then
          if (newle.nfw.eq.1) then
	     FILTER_VAR = 'CLOSED'
          elseif(newle.nfw.eq.2) then
	     FILTER_VAR = 'PPL'
          elseif(newle.nfw.eq.3) then
	     FILTER_VAR = '4Lx'
          elseif(newle.nfw.eq.4) then
             FILTER_VAR = ' '
          elseif(newle.nfw.eq.5) then
	     FILTER_VAR = 'Fe Cal'
          elseif(newle.nfw.eq.6) then
	     FILTER_VAR = 'Al/P'
          elseif(newle.nfw.eq.7) then
	     FILTER_VAR = '3Lx'
          elseif(newle.nfw.eq.8) then
	     FILTER_VAR = 'Bor'
          else
             write(context,
     &        '('' Descriptive:Unknown LE-Filter #:'', i4)') newle.nfw
	     call xwrite(context,10)
	     status=1
             return
          endif
          CALL FTPKYS(fits_unit,FILTER_KEY,FILTER_VAR,FILTER_COM,status)
       endif
c
c write object
       OBJECT_VAR = newrec1.source_name
       CALL FTPKYS(fits_unit, OBJECT_KEY, 
     &             OBJECT_VAR(1:LENACT(OBJECT_VAR)),OBJECT_COM, status)
c
c write Ra Dec object
       RA_VAL = Dble(newrec1.source_ra)/1.d9*360.d0
       CALL FTPKYG(fits_unit, RA_KEY, RA_VAL, 9, RA_COM, status)
       DEC_VAL = Dble(newrec1.source_dec)/1.d9*90.d0
       CALL FTPKYG(fits_unit, DEC_KEY, DEC_VAL, 9, DEC_COM, status)
c
c write Ra Dec pointing and roll only in extension  
       if (flag.eq.2) then
        RA_VAL = Dble(newrec1.ira)/1.d9*360.d0
        CALL FTPKYG(fits_unit,RA_NOM_KEY,RA_VAL,9,RA_NOM_COM,status)
        DEC_VAL = Dble(newrec1.idec)/1.d9*90.d0
        CALL FTPKYG(fits_unit,DEC_NOM_KEY,DEC_VAL,9,DEC_NOM_COM, status)
        ROLL_VAL = Dble(newrec1.roll)/10.d0
        CALL FTPKYG(fits_unit,ROLL_KEY,ROLL_VAL,9,ROLL_COM, status)
       endif
c     
c     The equinox and the World coordinate system are the same for
       CALL FTPKYE(fits_unit, EQUINOX_KEY, 1950.00, 2,
     &             EQUINOX_COM, status)
       CALL FTPKYS(fits_unit,RADECSYS_KEY, 'FK4',RADECSYS_COM,status)
c    
c date keywords for start and stop
c     Get the startshf time from the rbf
c
       CALL SHFTIM(newrec1.startshf, yr, daynum, hr, mn, sec)
       call xydmd(yr, daynum, mon, day)

       if ( yr.lt.1999 .and. yr.ge.1900 ) then
c
c     Write date in a special format (DD/MM/YY)
c
          yr = yr - 1900
          date_string = ' '
          write(date_string(1:2), '(I2.2)', iostat=status) day
          write(date_string(3:3), '(A1)', iostat=status) '/'
          write(date_string(4:5), '(I2.2)', iostat=status) mon
          write(date_string(6:6), '(A1)', iostat=status) '/'
          write(date_string(7:8), '(I2.2)', iostat=status) yr

          CALL FTPKYS(fits_unit,DATE_OBS_KEY,
     &       date_string(1:LENACT(date_string)),DATE_OBS_COM, status)
c
c     Write out the time in a special format (hh:mm:ss.ddddd):

          write(time_string(1:2), '(I2.2)', iostat=status) hr
          write(time_string(3:3), '(A1)', iostat=status) ':'
          write(time_string(4:5), '(I2.2)', iostat=status) mn
          write(time_string(6:6), '(A1)', iostat=status) ':'
          write(time_string(7:8), '(I2.2)', iostat=status) sec

          CALL FTPKYS(fits_unit, TIME_OBS_KEY, time_string(1:8),
     &       TIME_OBS_COM, status)
       else
c
c     Write date in a special format (YYYY-MM-DDThh:mm:ss)
c
          dsec = sec
          call fttm2s(yr, mon, day, hr, mn, dsec, 0, 
     &                date_string, status)

          CALL FTPKYS(fits_unit,DATE_OBS_KEY,
     &      date_string(1:LENACT(date_string)),DATE_OBS_Y2K_COM, status)
       endif
c
c     Get the endshf time from the rbf

       CALL SHFTIM(newrec1.endshf, yr, daynum, hr, mn, sec)
       call xydmd(yr, daynum, mon, day)

       if ( yr.lt.1999 .and. yr.ge.1900 ) then
c
c     Write date in a special format (DD/MM/YY)
c
          yr = yr - 1900
          date_string = ' '
          write(date_string(1:2), '(I2.2)', iostat=status) day
          write(date_string(3:3), '(A1)', iostat=status) '/'
          write(date_string(4:5), '(I2.2)', iostat=status) mon
          write(date_string(6:6), '(A1)', iostat=status) '/'
          write(date_string(7:8), '(I2.2)', iostat=status) yr

          CALL FTPKYS(fits_unit,DATE_END_KEY,date_string,
     &            DATE_END_COM,status)
c
c     Write out the time in a special format (hh:mm:ss.ddddd):
c
          write(time_string(1:2), '(I2.2)', iostat=status) hr
          write(time_string(3:3), '(A1)', iostat=status) ':'
          write(time_string(4:5), '(I2.2)', iostat=status) mn
          write(time_string(6:6), '(A1)', iostat=status) ':'
          write(time_string(7:8), '(I2.2)', iostat=status) sec

          CALL FTPKYS(fits_unit,TIME_END_KEY,time_string(1:8), 
     &            TIME_END_COM, status)
       else
c
c     Write date in a special format (YYYY-MM-DDThh:mm:ss)
c
          dsec = sec
          call fttm2s(yr, mon, day, hr, mn, dsec, 0, 
     &                date_string, status)

          CALL FTPKYS(fits_unit,DATE_END_KEY,
     &      date_string(1:LENACT(date_string)),DATE_END_Y2K_COM, status)

       endif
c
c     Write out other keywords
c
       CALL FTPKYS(fits_unit, ORIGIN_KEY,'HEASARC/GSFC',
     &           ORIGIN_COM,status)
       CALL FTPDAT(fits_unit, status)
      RETURN
      END

