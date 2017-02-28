c
c fitsread_local.inc
c
c   Common for fitsread   T.Takahashi
c  1993  8/21 H.Kubo SIS, GIS(x,t,rt)                     
c  1993  8/23 Y.Sekimoto GIS MPC mode                     
c  1993 11/10 H.Kubo GIS(RTI)
c  1994  5/25 H.Kubo keyword
c
c const
	Integer SENSOR_S0, SENSOR_S1
	Parameter(SENSOR_S0 = 0, SENSOR_S1 = 1)
	Integer SENSOR_S2, SENSOR_S3
	Parameter(SENSOR_S2 = 2, SENSOR_S3 = 3)
	Integer MAXWORD
	Parameter (MAXWORD = 100)
	Integer MAXROW
	Parameter (MAXROW = 3000)
c
	Real*8 evgen_dt
	Character*16 bit_rate
	Integer fits_nrecords
        Real*8  tstart, tstop
	Logical euler_flag
	Common /fitsread_local/ evgen_dt, tstart, tstop,
     &                          fits_nrecords, bit_rate, euler_flag
c
	Character*20 keyword(4,MAXWORD)
        Integer keyword_pos(2,MAXWORD)
        Integer keyword_num
        Integer entry_keyword
        Integer pos_time
	Common /fitsread_word/ keyword, keyword_pos, keyword_num,
     &                         entry_keyword, pos_time
c
c keyword(1,*)=TTYPE, keyword(2,*)=Instrument,
c keyword(3,*)=BNK size, keyword(4,*)=TFORM 
c keyword_pos(1,*)=ID number, keyword_pos(2,*)=column position
c keyword_num = number of keyword defined BNK
c entry_keyword= number of columns included in fitsfile
