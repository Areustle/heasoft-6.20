C*******************************************************************
      SUBROUTINE write_timing(fits_unit, newrec1, newrec2, timezero,
     &                        status)
C*******************************************************************
C
      include 'xrrbstr.h'
C
C     Input parameters:
      integer           fits_unit
      RECORD/rnewbuf_rec_1/newrec1
      RECORD/rnewbuf_rec_2/newrec2
c
C     Output parameters:
      integer       status
c
      INTEGER   LENACT
      REAL*8    XKMJD
c
      character(80) context
      character(8)   TIMESYS_KEY
      character(40)  TIMESYS_COM
      character(20) timesys
      PARAMETER(TIMESYS_KEY='TIMESYS')
      PARAMETER(TIMESYS_COM='ref. time in YYYY MM DD hh:mm:ss')
c
      character(8) TIMEREF_KEY
      character(20) TIMEREF_VAL
      character(40)	TIMEREF_COM
      PARAMETER(TIMEREF_KEY='TIMEREF')
      PARAMETER(TIMEREF_VAL='LOCAL')
      PARAMETER(TIMEREF_COM='reference frame for the times')
c
      character(8)   TASSIGN_KEY
      character(6)   TASSIGN_VAL
      character(40)  TASSIGN_COM
      PARAMETER(TASSIGN_KEY='TASSIGN')
      PARAMETER(TASSIGN_VAL='MADRID')
      PARAMETER(TASSIGN_COM='place of time assignment')
c
      character(8)   TIMEUNIT_KEY
      character(50)  TIMEUNIT_COM
      PARAMETER(TIMEUNIT_KEY='TIMEUNIT')
      PARAMETER(TIMEUNIT_COM='unit for TSTART, TSTOP, TIMEZER')
c
      character(8)   CLOCKAPP_KEY
      character(50)  CLOCKAPP_COM
      PARAMETER(CLOCKAPP_KEY='CLOCKAPP')
      PARAMETER(CLOCKAPP_COM='if clock correction applied')
c
c variable to be calculated 
      character(8)   MJDREF_KEY
      character(40)  MJDREF_COM
      PARAMETER(MJDREF_KEY='MJDREF')
      PARAMETER(MJDREF_COM='reference time in MJD')
c
      character(8)   TSTART_KEY
      character(45)  TSTART_COM
      PARAMETER(TSTART_KEY='TSTART')
      PARAMETER(TSTART_COM='start time counted from TIMESYS')
c
      character(8)   TSTARTI_KEY
      character(45)  TSTARTI_COM
      PARAMETER(TSTARTI_KEY='TSTARTI')
      PARAMETER(
     &    TSTARTI_COM='int. of start time counted from TIMESYS')
c
      character(8)   TSTARTF_KEY
      character(45)  TSTARTF_COM
      PARAMETER(TSTARTF_KEY='TSTARTF')
      PARAMETER(
     &    TSTARTF_COM='frac. of start time counted from TIMESYS')
c
      character(8)   TSTOP_KEY
      character(40)  TSTOP_COM
      PARAMETER(TSTOP_KEY='TSTOP')
      PARAMETER(TSTOP_COM='stop time counted from TIMESYS')
c
      character(8)   TSTOPI_KEY
      character(45)  TSTOPI_COM
      PARAMETER(TSTOPI_KEY='TSTOPI')
      PARAMETER(
     &    TSTOPI_COM='int. of stop time counted from TIMESYS')
c
      character(8)   TSTOPF_KEY
      character(40)  TSTOPF_COM
      PARAMETER(TSTOPF_KEY='TSTOPF')
      PARAMETER(
     &     TSTOPF_COM='frac. of stop time counted from TIMESYS')
c
      character(8)   TIMEZER_KEY
      character(60)  TIMEZER_COM
      PARAMETER(TIMEZER_KEY='TIMEZERO')
      PARAMETER(TIMEZER_COM='time offset from TIMESYS value')
c
      character(8)   TIMEZERI_KEY
      character(60)  TIMEZERI_COM
      PARAMETER(TIMEZERI_KEY='TIMEZERI')
      PARAMETER(TIMEZERI_COM='int. of time offset')
c
      character(8)   TIMEZERF_KEY
      character(60)  TIMEZERF_COM
      PARAMETER(TIMEZERF_KEY='TIMEZERF')
      PARAMETER(TIMEZERF_COM='frac. of time offset')
c
      character(8)   ONTIME_KEY
      character(40)  ONTIME_COM
      PARAMETER(ONTIME_KEY='ONTIME')
      PARAMETER(ONTIME_COM='time on source')
c
      character(8)   TIMEDEL_KEY
      character(60)  TIMEDEL_COM
      PARAMETER(TIMEDEL_KEY='TIMEDEL')
      PARAMETER(TIMEDEL_COM='integration time in TIMEUNIT')
c
      character(8)   SCCSTART_key
      character(60)  SCCSTART_com
      PARAMETER(SCCSTART_KEY='SCCSTART')
      PARAMETER(SCCSTART_COM='Space craft clock start ')
c
      character(8)   SCCSTOP_key
      character(60)  SCCSTOP_com
      PARAMETER(SCCSTOP_KEY='SCCSTOP')
      PARAMETER(SCCSTOP_COM='space craft clock stop')
c
      REAL*8  mjd_ref_time,timedel, tint, timezero
      REAL*8 dtsta, dtsto, firstbin, ontime 
      INTEGER*4 idtsto, exp, ntype 
      LOGICAL clockapp
c
c Write mjd=1980 1 1 00:00:00 timesys = 1980 1 1 00:00:00.00 
c timeref= local Tassign= madrid clockapp=no tunit=s
c Get the MJD equivalence of the reference time (1980.00):
c 
c change for the SSS
      mjd_ref_time = XKMJD(0)+1
      CALL FTPKYD(fits_unit,MJDREF_KEY,mjd_ref_time,16,
     &            MJDREF_COM, status)
      timesys='1980 1 1 00:00:00'
      CALL FTPKYS(fits_unit,TIMESYS_KEY,
     &            timesys,TIMESYS_COM,status)
      CALL FTPKYS(fits_unit,TIMEREF_KEY, 
     &            TIMEREF_VAL, TIMEREF_COM, status)
      CALL FTPKYS(fits_unit, TASSIGN_KEY,
     &            TASSIGN_VAL, TASSIGN_COM, status)
      clockapp=.false.
      CALL FTPKYL(fits_unit, CLOCKAPP_KEY,
     &            clockapp, CLOCKapp_COM, status)
      CALL FTPKYS(fits_unit,TIMEUNIT_KEY,
     &            's',TIMEUNIT_COM,status)
c
c     Write the bin-integration time:
      if ((newrec1.flagunit).eq.0) then
         tint= dfloat(newrec1.inttime)/16384.d0
         firstbin= dfloat(newrec1.timebin)/16384.d0
      else if ((newrec1.flagunit).eq.1) then
         tint= dfloat(newrec1.inttime)*1.D-6
         firstbin= dfloat(newrec1.timebin)*1.D-6
      else if ((newrec1.flagunit).eq.2) then
         tint= dfloat(newrec1.inttime)
         firstbin= dfloat(newrec1.timebin)
      else
         write(context, 200)newrec1.flagunit, newrec1.timebin,
     &          newrec1.inttime
200      format(' Unknown integration time flags:', 3(1X,i4))
         call xwrite(context,20)
      endif
c
      CALL FTPKYD(fits_unit, 
     &              TIMEDEL_KEY, tint, 16, TIMEDEL_COM, status)
c 
c start and stop time in second
      IF(newrec1.utctime1.ne.0) THEN
         dtsta=dble(newrec1.utctime1)+firstbin+dble(newrec1.MICRO1)/1.D6
         idtsto=newrec1.utctime1+(newrec1.endshf-newrec1.startshf)
         dtsto=dble(idtsto)+firstbin+dble(newrec1.MICRO2)/1.D6+8.d0     
      ELSE 
         dtsta=dble(newrec1.startshf)
         dtsto=dble(newrec1.endshf)+8.d0
      ENDIF
      CALL FTPKYD(fits_unit, 
     &              TSTART_KEY, dtsta, 16,TSTART_COM, status)
      CALL FTPKYD(fits_unit, 
     &              TSTOP_KEY, dtsto, 16,TSTOP_COM, status)
      exp=newrec1.expt
      ntype=newrec1.type
      IF (exp.eq.1.or.exp.eq.2.or.ntype.eq.0)THEN
        CALL FTPKYD(fits_unit, 
     &         TIMEZER_KEY, timezero, 16,TIMEZER_COM, status)
      ELSE
        CALL FTPKYD(fits_unit, 
     &              TIMEZER_KEY, dtsta, 16,TIMEZER_COM, status)
      ENDIF
      ontime=dtsto-dtsta
      CALL FTPKYD(fits_unit, 
     &              ONTIME_KEY, ontime, 16, ONTIME_COM, status)
c
c space craft clock value
      CALL FTPKYJ(fits_unit, SCCSTART_KEY, newrec1.reftime1, 
     &             SCCSTART_COM, status)
      CALL FTPKYJ(fits_unit, SCCSTOP_KEY, newrec1.reftime2, 
     &             SCCSTOP_COM, status)
c
c  in days
c      CALL xrshfcon(newrec1.UTCTIME1, iyr, iti(1), iti(2), iti(3),
c     &                 iti(4))
c      iti(5) = 0
c      CALL xrdhms(dti, sti, iti, 3)
c      dtsta = dti + firstbin/86400.D0 +
c     &           dble(newrec1.MICRO1)/1.D6/86400.D0
c         CALL xrshfcon(newrec1.UTCTIME1+newrec1.ENDSHF-newrec1.STARTSHF,
c     &                 iyr, iti(1), iti(2), iti(3), iti(4))
c      iti(5) = 0
c      CALL xrdhms(dti, sti, iti, 3)
c      dtsto = dti + firstbin/86400.D0 +
c     &           dble(newrec1.MICRO2)/1.D6/86400.D0
c      CALL FTPKYD(fits_unit,
c     &              TSTOP_KEY, dtsto, 16, TSTOP_COM, status)
c 
c      
c
      RETURN
      END
