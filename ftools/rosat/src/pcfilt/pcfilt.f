*+PCFILT
c      -----------------
       subroutine pcfilt 
c      -----------------
c --- DESCRIPTION -----------------------------------------------------
c PCFILT writes a ROSAT PSPC makefilter file, that is a file containing
c HK info, binned in a usefull way, for user-defined data screening in 
c xselect. This task takes an RDF ancillary file as input. Later it may
c be enhanced to read older formats.  
c
c --- CALLED ROUTINES -------------------------------------------------
c
c PCF_GP     : Gets parameters
c PCF_RDAT   : Reads data from RDF ancillary file
c PCF_GEO    : Steve Snowden code to sort through data
c              and write to outfile
c 
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1994 May 11)
c Rehana Yusaf (1995 Jan 13) 1.0.1; Minor update, to _sample 
c
c Banashree M Seifert (1997, Mar 25) 1.1.0:
c       . change to go to 200 instead of 70 when a error occurs
c       . change in PCF_GEO suboutine 
c         -- giving problem n asin/acos/atan2 when argument is zero
c Banashree M Seifert (1997, Sept,9) 1.2.0:
c       . bug fixed in subroutine PCF_GEO
c Peter D Wilson (1997, Dec 5) 1.3.0:
c       . change in PCF_RDAT
c         -- Dropped use of dummy dec_cas and ra_cas arrays which
c            were fixed sized... caused crashes on large data sets.
c            Now use dec_sc and ra_sc variables, but values are later
c            overwritten with real SC values.
c       . change in PCF_GEO
c         -- Output status variables weren't being set.  Overflow
c            errors caused by large nonzero values when later written
c            to an integer fits column. Now set to 0.
c Peter D Wilson (1997, Dec 5) 1.3.1:
c       . change in PCFILT
c         -- Dynamic memory request for hkp_rows and hksta_rows was
c            using DOUBLE data type, but arrays actually INTEGERs. Led
c            to overwriting random memory... OSF user encountered SegFault
c         -- hkp_rows and hksta_rows were not being freed on exit.  Calls
c            to udmfre added
c Peter D Wilson (1998 June 30) 1.3.2:
c       . Updated for new FCPARS behavior
c Ning Gan (2000 April 24) 1.3.3:
c       . Copied the keywords from input file to output file (Added two
C         routines (pcf_morekeys and cpkwd) 
c -------------------------------------------------------------------

      IMPLICIT NONE
      character(5) version
      parameter (version = '1.3.3')
      character(40) taskname
      COMMON/task/taskname
*-
c ---------------------------------------------------------------------
c
C --- DYNAMIC MEMORY ALLOCATION --- 
C  the following MEM common block definition is in the system iraf77.inc
C  file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c ---------------------------------------------------------------------
c VARIABLES ...

      character(80) infile, outfile,desc,subinfo
      character(16) telescope,instrume,detnam
      integer chatter,errflg
      integer status
      character(30) termdesc,context,errstr
      logical del_exist

c MAX ARRAY SIZES
      integer max_ep,max_att,max_hkp
      integer max_evr,max_sta,max_mkf

c COUNTERS ...

      integer n_ep,n_att,n_hkp,n_evr,n_sta,n_mkf 

c ... pointers to "arrays" to be dynamically allocated

c     for EPHEM data
      integer p_mjd_int,p_mjd_frac,p_sun_x
      integer p_sun_y,p_sun_z
      integer p_moon_x,p_moon_y,p_moon_z
      integer p_lon_east,p_lat_nort,p_gha,p_alt_sat
      integer p_sat_x,p_sat_y,p_sat_z
      real*8 mjdref
c     "arrays" to be dynamically allocated for EPHEM data
c     integer mjd_int(max_ep)
c     real*8 mjd_frac(max_ep),gha(max_ep)
c     real*8 sun_x(max_ep),sun_y(max_ep),sun_z(max_ep)
c     real*8 moon_x(max_ep),moon_y(max_ep),moon_y(max_ep)
c     real*8 lon_east(max_ep),lat_nort(max_ep),alt_sat(max_ep)

c     for ASPECT data
      integer p_time,p_roan_cas,p_ra_cas,p_dec_cas
      integer p_asp_qual,p_stt_qual,p_stat
c     "arrays" to be dynamically allocated for ASPECT data
c     real*8 time(max_att),ra_cas(max_att),dec_cas(max_att)
c     real*8 roan_cas(max_att),asp_qual(max_att),stt_qual(max_att)
c     integer status(max_att)

c     for HKP data
      integer p_hkp_time,p_miss,p_temp,p_press
      integer p_hvolt,p_filpos
c     "arrays" to be dynamically allocated for HKP data
c     integer miss(max_hkp),temp(max_hkp)
c     integer hvolt(max_hkp)

c     for EVENTS data
      integer p_ev_time,p_mv_aco,p_xtransm,p_a1_al
      integer p_sa_rate,p_xacc,p_a1al_mv,p_a1_ah
c     "arrays" to be dynamically allocated for EVENTS data
c     real*8 ev_time(max_evr)
c     integer*4 mv_aco(max_evr),xtransm(max_evr)
c     integer*4 a1_al(max_evr),sa_rate(max_evr),xacc(max_evr)

c     for HKSTA data
      integer p_sta_time
      integer p_hv_sta,p_carr_sta,p_gas_sta,p_det_sta
      integer p_temp_sta,p_lv_sta,p_inst_sta,p_filt_sta
      integer p_tel_sta,p_obi_num,p_cur_sta
c     "arrays" to be dynamically allocated for GSTAR data
c      real*8 sta_time(max_sta)
c      integer hv_sta(max_sta),carr_sta(max_sta),gas_sta(max_sta)
c      integer det_sta(max_sta),temp_sta(max_sta),lv_sta(max_sta)
c      integer inst_sta(max_sta),filt_sta(max_sta)
c      integer tel_sta(max_sta),obi_num(max_sta),cur_sta(max_sta)

c     for output data
      integer p_md,p_mag_ang,p_lval,p_zen_ang,p_ses_ang
      integer p_moon_ang,p_sun_ang,p_sat_lon,p_sat_lat
      integer p_vel_ang,p_oasp_qual,p_ostt_qual,p_ostat
      integer p_ora_sc,p_odec_sc,p_omv_aco,p_oxtransm,p_oa1_al
      integer p_osa_rate,p_oxacc,p_deadt,p_hkp_rows,p_hksta_rows
      integer p_rasat,p_decsat,p_oa1al_mv,p_oa1_ah,p_oap
c     "arrays" to be dynamically allocated
c     real*8 md(max_mkf),mag_ang(max_mkf),lval(max_mkf)
c     real*8 zen_ang(max_mkf),ses_ang(max_mkf),moon_ang(max_mkf)
c     real*8 sun_ang(max_mkf),sat_lon(max_mkf),sat_lat(max_mkf)
c     real*8 vel_ang(max_mkf)
c     real*8 oasp_qual(max_mkf),ostt_qual(max_mkf)
c     real*8 ora_sc(max_mkf),odec_sc(max_mkf)
c     integer ostat(max_mkf)
c     real*8 omv_aco(max_mkf),oxtransm(max_mkf),oa1_al(max_mkf)
c     real*8 osa_rate(max_mkf),oxacc(max_mkf)
c     real*4 deadt(max_mkf)
c     integer hkp_rows(max_mkf),hksta_rows(max_mkf)
       
c --- INITIALISATION ---

      taskname='PCFILT '
      context = 'fatal error'
      termdesc =' PCFILT Ver '//version//' terminated !'

c --- GET PARAMETERS ---

      errflg = 0
      call pcf_gp(infile,outfile,del_exist,errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 200 
      ENDIF

c
c --- USER INFO ---
c
      IF (chatter.GE.1) THEN
        desc = ' Main PCFILT Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- DETERMINE ARRAY DIMENSIONS ---
c
      call pcf_dims(infile,max_ep,max_att,max_hkp,max_evr,
     &                     max_sta,errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 200
      ENDIF
      IF (max_ep.LT.100) THEN
         max_ep = 100
      ELSEIF (max_att.LT.100) THEN
         max_att = 100
      ELSEIF (max_hkp.LT.100) THEN
         max_hkp = 100
      ELSEIF (max_evr.LT.100) THEN
         max_evr = 100
      ENDIF
      IF (max_sta.LT.100) THEN
         max_sta = 100
      ENDIF
      max_mkf = max_ep


c ALLOCATE DMA

      p_mjd_int = 0
      p_mjd_frac = 0
      p_sun_x = 0
      p_sun_y = 0
      p_sun_z = 0
      p_moon_x = 0
      p_moon_y = 0
      p_moon_z = 0
      p_lon_east = 0
      p_lat_nort = 0
      p_sat_x = 0
      p_sat_y = 0
      p_sat_z = 0
      p_gha = 0
      p_alt_sat = 0
      p_time = 0
      p_roan_cas = 0
      p_ra_cas = 0
      p_dec_cas = 0
      p_asp_qual = 0
      p_stt_qual = 0
      p_stat = 0
      p_hkp_time = 0
      p_miss = 0
      p_temp = 0
      p_press = 0
      p_hvolt = 0
      p_filpos = 0
      p_ev_time = 0
      p_mv_aco = 0
      p_xtransm = 0
      p_a1_al = 0
      p_sa_rate = 0
      p_xacc = 0
      p_a1al_mv = 0
      p_a1_ah = 0
      p_sta_time = 0
      p_hv_sta = 0
      p_carr_sta = 0
      p_gas_sta = 0
      p_det_sta = 0
      p_temp_sta = 0
      p_lv_sta = 0
      p_inst_sta = 0
      p_filt_sta = 0
      p_tel_sta = 0
      p_obi_num = 0
      p_cur_sta = 0
      p_md = 0
      p_mag_ang = 0
      p_lval = 0
      p_zen_ang = 0
      p_ses_ang = 0
      p_moon_ang = 0
      p_sun_ang = 0
      p_sat_lon = 0
      p_sat_lat = 0
      p_vel_ang = 0
      p_rasat = 0
      p_decsat = 0
      p_oasp_qual = 0
      p_ostt_qual = 0
      p_ostat = 0
      p_ora_sc = 0
      p_odec_sc = 0
      p_omv_aco = 0
      p_oxtransm = 0
      p_oa1_al = 0
      p_osa_rate = 0
      p_oxacc = 0
      p_oa1al_mv = 0
      p_oa1_ah = 0
      p_oap = 0
      p_deadt = 0
      p_hkp_rows = 0
      p_hksta_rows = 0

      status = 0
      call udmget(max_ep,4,p_mjd_int,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      status = 0
      call udmget(max_ep,7,p_mjd_frac,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      status = 0
      call udmget(max_ep,7,p_sun_x,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_sun_y,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_sun_z,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_moon_x,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_moon_y,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_moon_z,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_lon_east,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_lat_nort,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,4,p_sat_x,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,4,p_sat_y,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,4,p_sat_z,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_gha,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_ep,7,p_alt_sat,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF 
      call udmget(max_att,7,p_time,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_roan_cas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_ra_cas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_dec_cas,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_asp_qual,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,7,p_stt_qual,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_att,4,p_stat,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_hkp,7,p_hkp_time,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_hkp,4,p_miss,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_hkp,4,p_temp,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_hkp,4,p_press,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_hkp,4,p_hvolt,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_hkp,4,p_filpos,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,7,p_ev_time,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,4,p_mv_aco,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,4,p_xtransm,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,4,p_a1_al,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,4,p_sa_rate,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,4,p_xacc,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,4,p_a1al_mv,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_evr,4,p_a1_ah,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,7,p_sta_time,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,4,p_hv_sta,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,4,p_carr_sta,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,4,p_gas_sta,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,4,p_det_sta,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,4,p_temp_sta,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,4,p_lv_sta,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,4,p_inst_sta,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,4,p_filt_sta,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,4,p_tel_sta,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_sta,4,p_obi_num,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF

      call udmget(max_sta,4,p_cur_sta,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF


      call udmget(max_mkf,7,p_md,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_mag_ang,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF  
      call udmget(max_mkf,7,p_lval,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_zen_ang,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_ses_ang,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_moon_ang,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_sun_ang,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_sat_lon,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_sat_lat,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_vel_ang,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_rasat,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_decsat,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_oasp_qual,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_ostt_qual,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,4,p_ostat,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_ora_sc,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_odec_sc,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_omv_aco,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_oxtransm,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_oa1_al,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_osa_rate,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_oxacc,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_oa1al_mv,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_oa1_ah,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,7,p_oap,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,6,p_deadt,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,4,p_hkp_rows,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(max_mkf,4,p_hksta_rows,status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF

 50   IF (status.NE.0) THEN
        subinfo = errstr//' failed to allocate Dynamic memory'
        call fcecho(subinfo)
        errflg = 1
        goto 200
      ENDIF

c
c --- READ FILE ---
c

      call pcf_rdat(infile,telescope,instrume,detnam,
     &  max_ep,n_ep,MEMI(p_mjd_int),MEMD(p_mjd_frac),
     &  MEMD(p_sun_x),MEMD(p_sun_y),MEMD(p_sun_z),
     &  MEMD(p_moon_x),MEMD(p_moon_y),MEMD(p_moon_z),
     &  MEMD(p_lon_east),MEMD(p_lat_nort),
     &  MEMI(p_sat_x),MEMI(p_sat_y),MEMI(p_sat_z),MEMD(p_gha),
     &  MEMD(p_alt_sat),mjdref,max_att,n_att,MEMD(p_time),
     &  MEMD(p_roan_cas),MEMD(p_ra_cas),
     &  MEMD(p_dec_cas),MEMD(p_asp_qual),MEMD(p_stt_qual),
     &  MEMI(p_stat),max_hkp,n_hkp,MEMD(p_hkp_time),MEMI(p_miss),
     & MEMI(p_temp),MEMI(p_press),MEMI(p_hvolt),MEMI(p_filpos),
     &max_evr,n_evr,MEMD(p_ev_time),MEMI(p_mv_aco),MEMI(p_xtransm),
     &MEMI(p_a1_al),MEMI(p_sa_rate),MEMI(p_xacc),MEMI(p_a1al_mv),
     &MEMI(p_a1_ah),max_sta,n_sta,MEMD(p_sta_time),MEMI(p_hv_sta),
     &MEMI(p_carr_sta),MEMI(p_gas_sta),MEMI(p_det_sta),
     &MEMI(p_temp_sta),MEMI(p_lv_sta),MEMI(p_inst_sta),
     &MEMI(p_filt_sta),MEMI(p_tel_sta),MEMI(p_cur_sta),
     & MEMI(p_obi_num),errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 200
      ENDIF
c
c --- SORT THROUGH DATA ---
c
      call pcf_geo(max_ep,n_ep,MEMI(p_mjd_int),MEMD(p_mjd_frac),
     & MEMD(p_sun_x),MEMD(p_sun_y),MEMD(p_sun_z),MEMD(p_moon_x),
     & MEMD(p_moon_y),MEMD(p_moon_z),MEMD(p_lon_east),
     & MEMD(p_lat_nort),MEMI(p_sat_x),MEMI(p_sat_y),MEMI(p_sat_z),
     & MEMD(p_gha),MEMD(p_alt_sat),max_att,n_att,
     & MEMD(p_time),MEMD(p_roan_cas),MEMD(p_ra_cas),MEMD(p_dec_cas),
     & MEMD(p_asp_qual),MEMD(p_stt_qual),MEMI(p_stat),
     & max_evr,n_evr,MEMD(p_ev_time),
     & MEMI(p_mv_aco),MEMI(p_xtransm),MEMI(p_a1_al),MEMI(p_sa_rate),
     & MEMI(p_xacc),MEMI(p_a1al_mv),MEMI(p_a1_ah),max_sta,max_hkp,
     & MEMD(p_hkp_time),MEMD(p_sta_time),n_hkp,n_sta,max_mkf,n_mkf,
     & MEMD(p_md),MEMD(p_mag_ang),MEMD(p_lval),MEMD(p_zen_ang),
     & MEMD(p_ses_ang),MEMD(p_moon_ang),MEMD(p_sun_ang),MEMD(p_sat_lon),
     & MEMD(p_sat_lat),MEMD(p_vel_ang),MEMD(p_rasat),MEMD(p_decsat),
     & MEMD(p_oasp_qual),MEMD(p_ostt_qual),MEMI(p_ostat),MEMD(p_ora_sc),
     & MEMD(p_odec_sc),MEMD(p_omv_aco),MEMD(p_oxtransm),MEMD(p_oa1_al),
     & MEMD(p_osa_rate),MEMD(p_oxacc),MEMD(p_oa1al_mv),MEMD(p_oa1_ah),
     & MEMD(p_oap),MEMR(p_deadt),
     & MEMI(p_hkp_rows),MEMI(p_hksta_rows),mjdref,errflg,chatter)
      IF (errflg.NE.0) THEN
        goto 200
      ENDIF


c --- WRITE OUTFILE ---

      call pcf_wt(infile,outfile,telescope,instrume,detnam,
     & max_mkf,n_mkf,MEMD(p_md),
     & MEMD(p_mag_ang),MEMD(p_lval),
     & MEMD(p_zen_ang),MEMD(p_ses_ang),MEMD(p_moon_ang),
     & MEMD(p_sun_ang),MEMD(p_sat_lon),MEMD(p_sat_lat),
     & MEMD(p_vel_ang),MEMD(p_rasat),MEMD(p_decsat),
     & MEMD(p_oasp_qual),MEMD(p_ostt_qual),
     & MEMI(p_ostat),MEMD(p_ora_sc),MEMD(p_odec_sc),
     & MEMD(p_omv_aco),MEMD(p_oxtransm),MEMD(p_oa1_al),MEMD(p_osa_rate),
     & MEMD(p_oxacc),MEMD(p_oa1al_mv),MEMD(p_oa1_ah),MEMD(p_oap),
     & max_hkp,MEMI(p_miss),MEMI(p_temp),MEMI(p_press),MEMI(p_hvolt),
     & MEMI(p_filpos),max_sta,MEMI(p_hv_sta),MEMI(p_carr_sta),
     & MEMI(p_gas_sta),MEMI(p_det_sta),MEMI(p_temp_sta),
     & MEMI(p_filt_sta),MEMI(p_lv_sta),MEMI(p_inst_sta),
     & MEMI(p_tel_sta),MEMI(p_cur_sta),MEMI(p_obi_num),
     & MEMI(p_hkp_rows),MEMI(p_hksta_rows),MEMR(p_deadt),mjdref,
     & taskname,del_exist,errflg,chatter)
      IF (errflg.NE.0) THEN
        call fcerr(context)
        goto 200 
      ENDIF 


c FREE THE DYNAMIC MEMORY

 70   status = 0
      call udmfre(p_mjd_int,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_mjd_frac,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sun_x,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sun_y,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF 
      call udmfre(p_sun_z,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_moon_x,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_moon_y,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_moon_z,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_lon_east,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_lat_nort,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sat_x,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sat_y,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sat_z,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_gha,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_alt_sat,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_a1al_mv,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_a1_ah,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_time,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_roan_cas,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_ra_cas,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_dec_cas,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_asp_qual,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_stt_qual,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_stat,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_hkp_time,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_miss,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_temp,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_press,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_hvolt,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_filpos,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_ev_time,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_mv_aco,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_xtransm,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_a1_al,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sa_rate,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
c      call udmfre(p_sta_time,7,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
c      call udmfre(p_hv_sta,4,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
c      call udmfre(p_carr_sta,4,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
c      call udmfre(p_gas_sta,4,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
c      call udmfre(p_det_sta,4,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
c      call udmfre(p_temp_sta,4,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
c      call udmfre(p_lv_sta,4,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
c      call udmfre(p_inst_sta,4,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
c      call udmfre(p_filt_sta,4,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
c      call udmfre(p_tel_sta,4,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
c      call udmfre(p_obi_num,4,status)
c      IF (status.NE.0) THEN
c        goto 100
c      ENDIF
      call udmfre(p_md,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_mag_ang,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_lval,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF 
      call udmfre(p_zen_ang,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_ses_ang,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_moon_ang,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sun_ang,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sat_lon,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_sat_lat,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_vel_ang,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_rasat,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_decsat,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_oasp_qual,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_ostt_qual,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_ostat,4,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_ora_sc,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_odec_sc,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_oxtransm,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_oa1_al,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_osa_rate,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_oxacc,7,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_oap,7,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_deadt,6,status) 
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_hkp_rows,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF
      call udmfre(p_hksta_rows,4,status)
      IF (status.NE.0) THEN
        goto 100
      ENDIF

 100  IF (status.NE.0) THEN
       subinfo = errstr//'failed to de-allocate Dynamic Memory'
       call fcecho(subinfo)
       errflg = 99
      ENDIF
 200  call wtendm(taskname,version,errflg,chatter)

      end
c ----------------------------------------------------------------------
c     END OF MAIN PCFILT 
c ----------------------------------------------------------------------
    
*+PCF_GP
c     ------------------------------------------------------
      subroutine pcf_gp(infile,outfile,del_exist,errflg,chatter)
c     ------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile
      character(80) filename
      character(70) ill_files(5)
      character(30) errstr,wrnstr
      integer errflg,chatter,status,n_ill
      character(80) desc
      integer fcstln,flen,extnum
      logical ext,valfil,del_exist
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c infile     char   : input file name
c outfile    char   : Output filename
c chatter    int    : Chattiness flag, >20 verbose
c errflg     int    : Error flag
c
c --- CALLED ROUTINES -------------------------------------------------
c
c UCLGST     : (HOST) Get string input
c UCLGSI     : (HOST) Get integer input
c FCECHO     : (FTOOLS) Screen write
c
c --- COMPILATION/LINKING ---------------------------------------------
c
c CALTOOLS, FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1994 May 11)
c Rehana Yusaf (1994 Sept 28) 1.0.1; add clobber parameter
C Peter Wilson (1998 June 30) 1.0.2: Updated for new FCPARS behavior
      character(5) version 
      parameter (version = '1.0.2')
*-
c ---------------------------------------------------------------------
c
      errstr = ' ERROR : PCF_GP Ver '//version//':'
      wrnstr = ' WARNING : PCF_GP Ver '//version//':'

c GET INFILE

      status = 0
      call uclgst('infile',infile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting infile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(infile)
      IF (infile.EQ.'  ') THEN
        errflg = 1
        desc = ' Input Ancillary file not entered !'
        call fcecho(desc)
        return
      ENDIF
C PDW 6/30/98: Don't bother! Let FTOPEN determine if file exists
C      call fcpars(infile,filename,extnum,status)
C      ext = .true.
C      flen = fcstln(filename)
C      INQUIRE(FILE=filename(:flen),EXIST=ext)
C      IF (.NOT.ext) THEN
C        errflg = 1
C        desc = errstr//' File does not EXIST :'//filename
C        call fcecho(desc)
C        return
C      ENDIF

c GET OUTFILE 

      status = 0
      call uclgst('outfile',outfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting outfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c CLOBBER PARAMETER

      status = 0
      call uclgsb('clobber',del_exist,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting del_exist parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c OUTFILE VALIDATION 

      call crmvlbk(outfile)
      IF (outfile.EQ.'  ') THEN
        desc = errstr//' outfile must be entered !!'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      n_ill = 1
C PDW 6/30/90: Use ftrtnm to strip off extension number
C      ill_files(1) = infile
      call ftrtnm( infile, ill_files(1), status )
      call ck_file(outfile,ill_files,n_ill,valfil,del_exist,chatter)
      IF (.NOT.valfil) THEN
        errflg = 2
        return
      ENDIF

c GET CHATTER 

      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting chatter parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF        
      return
      end
c ---------------------------------------------------------------------
c     END OF PCF_GP
c ---------------------------------------------------------------------

 
*+PCF_DIMS
c     ------------------------------------------------------------
      subroutine pcf_dims(infile,max_ep,max_att,max_hkp,max_evr,
     &                     max_sta,ierr,chatter)
c     ------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------
c     This routine determines the array dimensions
c ----------------------------------------------------------------
c --- PASSED VARIABLES ---
c
      IMPLICIT NONE
      character*(*) infile
      integer max_ep,max_att,max_hkp,max_evr,ierr
      integer chatter,max_sta
c
c --- AUTHORS/MODIFICATION ---------------------------------------
c     Rehana Yusaf 1.0.0; DEC 1994
      character(5) version
      parameter (version = '1.0.0')
c ----------------------------------------------------------------
*-
c --- LOCAL VARIABLES
      character(70) subinfo,errinfo
      character(32) errstr,wrnstr,comm
      integer htype,extnum,status

c --- VARIABLES FOR MVEXT ---

      integer nsearch,ninstr
      parameter (nsearch = 50)
      integer next(nsearch),iunit
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9)
      character(8) extname
c
c --- USER INFO ---
c
      errstr = ' ERROR: PCF_DIMS Ver '//version//':'
      wrnstr = ' WARNING: PCF_DIMS Ver '//version//':'
      IF (chatter.GE.10) THEN
         subinfo = ' ... using PCF_DIMS Ver '//version
         call fcecho(subinfo)
      ENDIF
c
c --- OPEN FILE ---
c

c LOCATE EPHEM EXTENSION ...

      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'EPHEM'
      extname = 'EPHEM'
      call mvext(0,infile,iunit,ninstr,instr,nsearch,next,outhdu,
     &           extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        subinfo = errstr//' problem locating EPHEM extension'
        call fcecho(subinfo)
        goto 10
      ENDIF
      status = 0
      call ftgkyj(iunit,'NAXIS2',max_ep,comm,status)
      errinfo = errstr//' reading NAXIS2 from EPHEM ext'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        goto 10 
      ENDIF

      

c LOCATE ASPECT EXTENSION ...

      ierr = 0
      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'ASPECT'
      extname = 'ASPECT'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        errinfo = errstr//' moving to begining of infile'
        call fcecho(errinfo)
        goto 10
      ENDIF


ccc      extnum = 0
      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &           outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        errinfo = errstr//' locating ASPECT ext'
        call fcecho(errinfo)
        goto 10
      ENDIF
      status = 0
      call ftgkyj(iunit,'NAXIS2',max_att,comm,status)
      errinfo = errstr//' reading NAXIS2 from ASPECT ext'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        goto 10 
      ENDIF

c LOCATE HKP EXTENSION ...

      ierr = 0
      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'HKP'
      extname = 'HKP'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF

ccc      extnum = 0
      extnum=-1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &          outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
       errinfo = errstr//' locating HKP ext'
       call fcecho(errinfo)
       goto 10
      ENDIF
      status = 0
      call ftgkyj(iunit,'NAXIS2',max_hkp,comm,status)
      errinfo = errstr//' reading NAXIS2 from HKP ext'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        goto 10 
      ENDIF

c LOCATE EVRATE EXTENSION ...

      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'EVRATE'
      extname = 'EVRATE'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF

ccc      extnum = 0
      extnum=-1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &          outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        errinfo = errstr//' locating EVRATE ext'
        call fcecho(errinfo)
        goto 10
      ENDIF

      status = 0
      call ftgkyj(iunit,'NAXIS2',max_evr,comm,status)
      errinfo = errstr//' reading NAXIS2 from EVRATE ext'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        goto 10 
      ENDIF

      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'HKSTA'
      extname = 'HKSTA'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF
ccc      extnum = 0
      extnum=-1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &          outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        errinfo = errstr//' locating HKSTA ext'
        call fcecho(errinfo)
        goto 10
      ENDIF

      status = 0
      call ftgkyj(iunit,'NAXIS2',max_sta,comm,status)
      errinfo = errstr//' reading NAXIS2 from HKSTA ext'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        goto 10
      ENDIF

 10   status = 0 
      call ftclos(iunit,status)
      return
      end
c     -------------------------------------------------------------------
c     END OF PCF_DIMS
c     -------------------------------------------------------------------    


*+PCF_RDAT
c     --------------------------------------------------------- 
      subroutine pcf_rdat(infile,telescope,instrume,detnam,
     &                    max_ep,n_ep,mjd_int,mjd_frac,
     &            sun_x,sun_y,sun_z,moon_x,moon_y,moon_z,
     &                    lon_east,lat_nort,sat_x,sat_y,sat_z,
     &                    gha,alt_sat,mjdref,
     &                    max_att,n_att,time,roan_cas,ra_sc,
     &                    dec_sc,asp_qual,stt_qual,status,
     &                    max_hkp,n_hkp,hkp_time,miss,temp,press,
     &                    hvolt,filpos,max_evr,n_evr,ev_time,mv_aco,
     &                    xtransm,a1_al,sa_rate,xacc,a1al_mv,a1_ah,
     &                    max_sta,n_sta,sta_time,hv_sta,carr_sta,
     &                    gas_sta,det_sta,temp_sta,lv_sta,
     &                    inst_sta,filt_sta,tel_sta,cur_sta,obi_num,
     &                    ierr,chatter)
c     ----------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c This routine reads the required data from an RDF Ancillary file.
c ------------------------------------------------------------------
c --- VARIABLES ---
c 
      IMPLICIT NONE
      character*(*) infile,telescope,instrume,detnam
      integer ierr,chatter
      integer max_ep,n_ep
      integer mjd_int(max_ep)
      real*8 mjd_frac(max_ep),gha(max_ep),mjdref
      real*8 sun_x(max_ep),sun_y(max_ep),sun_z(max_ep)
      real*8 moon_x(max_ep),moon_y(max_ep),moon_z(max_ep)
      real*8 lon_east(max_ep),lat_nort(max_ep),alt_sat(max_ep)
      integer sat_x(max_ep),sat_y(max_ep),sat_z(max_ep)
      integer max_att,n_att
      real*8 time(max_att),ra_sc(max_att),dec_sc(max_att)
      real*8 roan_cas(max_att),asp_qual(max_att),stt_qual(max_att)
      integer status(max_att)
      integer max_hkp,n_hkp
      real*8 hkp_time(max_hkp)
      integer miss(max_hkp),temp(max_hkp),press(max_hkp)
      integer hvolt(max_hkp),filpos(max_hkp)
      integer max_evr,n_evr
      real*8 ev_time(max_evr)
      integer mv_aco(max_evr),xtransm(max_evr)
      integer*4 a1_al(max_evr),sa_rate(max_evr),xacc(max_evr)
      integer max_sta,n_sta,a1al_mv(max_evr),a1_ah(max_evr)
      real*8 sta_time(max_sta)
      integer hv_sta(max_sta),carr_sta(max_sta),gas_sta(max_sta)
      integer det_sta(max_sta),temp_sta(max_sta),lv_sta(max_sta)
      integer inst_sta(max_sta),filt_sta(max_sta),tel_sta(max_sta)
      integer obi_num(max_sta),cur_sta(max_sta)
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c ierr       int    : Error flag, 0 is okay
c chatter    int    : Chattiness flag >20 verbose, <5 quiet
c infile     char   : input filename
c max_ep     int    : Maximum array size for EPHEM ext data
c n_ep       int    : counter for EPHEM data
c max_att    int    : Maximum array size for ASPECT ext data
c n_att      int    : counter for ASPECT data
c max_hkp    int    : Maximum array size for HKP ext data
c n_hkp      int    : counter for hkp data
c max_evr    int    : Maximum array size for EVENTS ext data
c n_evr      int    : counter for events data
c max_sta    int    : Maximum array size for HKSTA ext data
c n_sta      int    : counter for HKSTA data
c
c --- CALLED ROUTINES ----------------------------------------------
c
c RDEPRD    : (CALLIB) Reads EPHEM extension in RDF Ancillary file
c RDATRD    : (CALLIB) Reads ASPECT extension in RDF Ancillary file
c RDHKRD    : (CALLIB) Reads HKP extension in RDF Ancillary file
c RDMVRD    : (CALLIB) Reads EVENTS ext in RDF Ancillary file
c RDSTRD    : (CALLIB) Reads HKSTA ext in RDF Ancillary file
c
c ---- LINKING/COMPILATION -----------------------------------------
c
c FITSIO,CALLIB,FTOOLS LIBRARY
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1994 May 17) 1.0.0;
c Peter D Wilson (1997 Dec 5) 1.0.1: Dropped use of dummy dec_cas
c                                    and ra_cas
c
      character(5) version
      parameter (version = '1.0.1')
*-
c ------------------------------------------------------------------
c --- LOCALS ---
c
      character(30) errstr,wrnstr,comm
      character(70) subinfo,message
      integer iunit,frow,felem,colnum,num
      integer inull,htype,extnum,errflg,mjdint
      integer i
c      integer tmp_att
c      parameter (tmp_att=100000)
      logical anyflg
      real*8 enull,mjdreal
c      real*8 dec_cas(tmp_att),ra_cas(tmp_att)

c VARIABLES FOR MVEXT

      integer nsearch,ninstr
      parameter (nsearch = 50)
      integer next(nsearch)
      character(20) extnames(nsearch),outhdu(9,nsearch)
      character(20) outver(nsearch) ,instr(9)
      character(8) extname 
c
c --- USER INFO ---
c
      errstr = ' ERROR: PCF_RDAT Ver '//version//':'
      wrnstr = ' WARNING: PCF_RDAT Ver '//version//':'
      IF (chatter.GE.10) THEN
        subinfo = ' using PCF_RDAT Ver '//version
        call fcecho(subinfo) 
      ENDIF
c
c --- OPEN FILE ---
c

c LOCATE EPHEM EXTENSION ...

      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'EPHEM'
      extname = 'EPHEM'
      call mvext(0,infile,iunit,ninstr,instr,nsearch,next,outhdu,
     &           extnames,outver,extname,ierr,chatter)       
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF

c READ EPHEM DATA ...

      call rdeprd(iunit,n_ep,max_ep,mjd_int,mjd_frac,
     &                  sun_x,sun_y,sun_z,
     &                  moon_x,moon_y,moon_z,
     &                  lon_east,lat_nort,sat_x,sat_y,sat_z,
     &                  gha,alt_sat,chatter,ierr)
      IF (ierr.NE.0) THEN
        subinfo = errstr//' reading EPHEM data'
        call fcecho(subinfo)
        goto 10 
      ENDIF
      ierr = 0
      call ftgkyj(iunit,'MJDREFI',mjdint,comm,ierr)
      IF (ierr.NE.0) THEN
        mjdint = 48043
        IF (chatter.GE.20) THEN
          subinfo =errstr//' reading MJDREFI keyword'
          call wt_ferrmsg(ierr,subinfo)
          subinfo = ' default value 48043 is used'
          call fcecho(subinfo)
        ENDIF
      ENDIF

      ierr = 0
      call ftgkyd(iunit,'MJDREFF',mjdreal,comm,ierr)
      IF (ierr.NE.0) THEN
        mjdreal = 8.7974537037007E-01
        IF (chatter.GE.20) THEN
          subinfo =errstr//' reading MJDREFI keyword'
          call wt_ferrmsg(ierr,subinfo)
          subinfo = ' default value 8.7974537037007E-01 is used'
          call fcecho(subinfo)
        ENDIF
      ENDIF
      mjdref = mjdint + mjdreal

c READ TELESCOPE,INSTRUME,DETNAM ...

c     TELESCOP ...

      ierr= 0
      call ftgkys(iunit,'TELESCOP',telescope,comm,ierr)
      IF (chatter.GE.20) THEN
        message = wrnstr//' reading TELESCOP '
        call wt_ferrmsg(ierr,message)
      ENDIF
      IF (ierr.EQ.202) THEN
        telescope = 'UNKNOWN'
      ENDIF

c     INSTRUME ...

      ierr= 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,ierr)
      IF (chatter.GE.20) THEN
        message = wrnstr//' reading INSTRUME '
        call wt_ferrmsg(ierr,message)
      ENDIF
      IF (ierr.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF

c     DETNAM ...

      ierr = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,ierr)
      IF (chatter.GE.30) THEN
        message = wrnstr//' reading DETNAM '
        call wt_ferrmsg(ierr,message)
      ENDIF
      IF (ierr.EQ.202) THEN
        detnam = 'NONE'
      ENDIF

c READ ASPECT EXTENSION ...

      do i=1,nsearch
         extnames(i)=' '
      enddo

      ierr = 0
      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'ASPECT'
      extname = 'ASPECT'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN 
        goto 10 
      ENDIF

c READ ASPECT EXTENSION ...

ccc      extnum = 0
      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &           outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF
      call ftghdn(iunit,num)

C   PDW 12/5/97
C ra_sc and dec_sc used as dummy variables for RA_CAS and DEC_CAS data
C Manually read data from RA_SC and DEC_SC columns below... ignore CAS

      call rdatrd(iunit,n_att,max_att,time,roan_cas,
     &                 ra_sc,dec_sc,chatter,ierr)
      IF (ierr.NE.0) THEN
        subinfo = errstr//' reading ASPECT data'
        call fcecho(subinfo)
        goto 10 
      ENDIF
      ierr= 0
      call ftgcno(iunit,.false.,'RA_SC',colnum,ierr)
      IF (ierr.NE.0) THEN
         subinfo= errstr//' RA_SC column not present in ASPECT!'
         call fcecho(subinfo)
         goto 10 
      ENDIF
      frow=1
      felem=1
      enull=0
      ierr=0
      call ftgcvd(iunit,colnum,frow,felem,n_att,enull,ra_sc,
     &             anyflg,ierr)
      subinfo = errstr//' reading ra_sc column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF
      ierr= 0
      call ftgcno(iunit,.false.,'DEC_SC',colnum,ierr)
      IF (ierr.NE.0) THEN
         subinfo= errstr//' DEC_SC column not present in ASPECT!'
         call fcecho(subinfo)
         goto 10 
      ENDIF
    
      frow=1
      felem=1
      enull=0
      ierr=0
      call ftgcvd(iunit,colnum,frow,felem,n_att,enull,dec_sc,
     &             anyflg,ierr)
      subinfo = errstr//' reading dec_sc column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF
      ierr= 0
      call ftgcno(iunit,.false.,'ASP_QUAL',colnum,ierr)
      IF (ierr.NE.0) THEN
        subinfo= errstr//' ASP_QUAL column not present in ASPECT!'
        call fcecho(subinfo)
        goto 10 
      ENDIF
      frow=1
      felem=1
      enull=0
      ierr=0
      call ftgcvd(iunit,colnum,frow,felem,n_att,enull,asp_qual,
     &             anyflg,ierr)
      subinfo = errstr//' reading asp_qual column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF
      ierr= 0
      call ftgcno(iunit,.false.,'STT_QUAL',colnum,ierr)
      IF (ierr.NE.0) THEN
       subinfo= errstr//' STT_QUAL column not present in ASPECT!'
       call fcecho(subinfo)
       goto 10 
      ENDIF
      frow=1
      felem=1
      enull=0
      ierr=0
      call ftgcvd(iunit,colnum,frow,felem,n_att,enull,
     &             stt_qual,anyflg,ierr)
      subinfo = errstr//' reading stt_qual column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF      

      ierr= 0
      call ftgcno(iunit,.false.,'STATUS',colnum,ierr)
      IF (ierr.NE.0) THEN
        subinfo= errstr//' STATUS column not present in ASPECT!'
        call fcecho(subinfo)
        goto 10 
      ENDIF
      frow=1
      felem=1
      inull=0
      ierr=0
      call ftgcvi(iunit,colnum,frow,felem,n_att,inull,
     &             status,anyflg,ierr)
      subinfo = errstr//' reading status column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
       goto 10 
      ENDIF

c LOCATE HKP EXTENSION ...

      ierr = 0
      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'HKP'
      extname = 'HKP'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF

c READ HKP EXTENSION ...
ccc      extnum = 0
      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &          outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
       goto 10 
      ENDIF
      call ftghdn(iunit,num)
      call rdhkrd(iunit,n_hkp,max_hkp,hkp_time,miss,temp,
     &                  press,hvolt,filpos,chatter,ierr)
      IF (ierr.NE.0) THEN
        subinfo = errstr//' reading HKP extension'
        call fcecho(subinfo)
        goto 10 
      ENDIF

c LOCATE EVRATE EXTENSION ...

      ninstr = 2 
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'EVRATE'
      extname = 'EVRATE'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF

c READ EVRATE EXTENSION ...
       
ccc      extnum = 0
      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &          outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF
      call ftghdn(iunit,num)
      call rdmvrd(iunit,n_evr,max_evr,ev_time,mv_aco,
     &                  xtransm,a1_al,xacc,chatter,ierr)
      IF (ierr.NE.0) THEN
        subinfo = errstr//' reading EVRATE extension'
        call fcecho(subinfo)
       goto 10 
      ENDIF
      ierr= 0
      call ftgcno(iunit,.false.,'SA_RATE',colnum,ierr)
      IF (ierr.NE.0) THEN
         subinfo= errstr//' SA_RATE column not present in EVRATE!'
         call fcecho(subinfo)
         goto 10 
      ENDIF
      frow=1
      felem=1
      inull=0
      ierr=0
      call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,
     &             sa_rate,anyflg,ierr)
      subinfo = errstr//' reading SA_RATE column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF
      ierr = 0
      call ftgcno(iunit,.false.,'A1AL_MV',colnum,ierr)
      IF (ierr.NE.0) THEN
         subinfo= errstr//' A1AL_MV column not present in EVRATE!'
         call fcecho(subinfo)
         goto 10
      ENDIF
      call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,
     &             a1al_mv,anyflg,ierr)
      subinfo = errstr//' A1AL_MV reading column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF
      ierr = 0
      call ftgcno(iunit,.false.,'A1_AH',colnum,ierr)
      IF (ierr.NE.0) THEN
         subinfo= errstr//' A1_AH column not present in EVRATE!'
         call fcecho(subinfo)
         goto 10
      ENDIF
      call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,
     &             a1_ah,anyflg,ierr)
      subinfo = errstr//' A1_AH reading column '
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10
      ENDIF

c LOCATE HKSTA EXTENSION ...

      ninstr = 2
      instr(1) = 'TEMPORALDATA'
      instr(2) = 'HKSTA'
      extname = 'HKSTA'
      call ftmahd(iunit,1,htype,ierr)
      subinfo = errstr//'moving to primary extension'
      call wt_ferrmsg(ierr,subinfo)
      IF (ierr.NE.0) THEN
        goto 10 
      ENDIF
ccc      extnum = 0
      extnum = -1
      call mver(iunit,extnum,ninstr,instr,nsearch,next,
     &          outhdu,extnames,outver,extname,ierr,chatter)
      IF (ierr.NE.0) THEN
         return
      ENDIF
      call ftghdn(iunit,num)
     
c READ HKSTA EXTENSION ...

      call rdstrd(iunit,n_sta,max_sta,sta_time,hv_sta,
     & carr_sta,gas_sta,det_sta,temp_sta,
     & lv_sta,inst_sta,filt_sta,
     & tel_sta,cur_sta,obi_num,chatter,ierr)
      IF (ierr.NE.0) THEN 
        subinfo = errstr//' reading HKSTA extension'
        call fcecho(subinfo)
        goto 10 
      ENDIF

   10 errflg = 0
      call ftclos(iunit,errflg)
      IF (errflg.NE.0) THEN
        subinfo = errstr//' closing infile'
        call fcecho(subinfo)
      ENDIF
      IF ((chatter.GE.20).AND.(ierr.EQ.0)) THEN
        subinfo = ' all data has been succesfully read'
        call fcecho(subinfo)
      ENDIF
      return
      end
c -------------------------------------------------------------------
c     END OF PCF_RDAT
c -------------------------------------------------------------------

 

*+PCF_GEO
         subroutine pcf_geo(max_ep,n_ep,mjd_int,mjd_frac,sun_x,
     &  sun_y,sun_z,moon_x,moon_y,moon_z,
     &  lon_east,lat_nort,sat_x,sat_y,sat_z,
     &  gha,alt_sat,max_att,n_att,time,
     &  roan_cas,ra_cas,dec_cas,asp_qual,stt_qual,stat,
     &  max_evr,n_evr,ev_time,mv_aco,xtransm,a1_al,sa_rate,xacc,
     &  a1al_mv,a1_ah,max_sta,max_hkp,hkp_time,sta_time,n_hkp,n_sta,
     &  max_mkf,n_mkf,md,mag_ang,lval,zen_ang,ses_ang,moon_ang,
     &  sun_ang,sat_lon,sat_lat,vel_ang,rsat,dcsat,oasp_qual,
     &  ostt_qual,ostat,ora_sc,odec_sc,omv_aco,oxtransm,oa1_al,
     &  osa_rate,oxacc,oa1al_mv,oa1_ah,oap,
     &  deadt,hkp_rows,hksta_rows,mjdref,ierr,chatter)
C
C --- PASSED VARIABLES ---
C
      IMPLICIT NONE
      integer ierr,chatter
      integer max_ep,n_ep
      integer mjd_int(max_ep)
      real*8 mjd_frac(max_ep),gha(max_ep)
      real*8 sun_x(max_ep),sun_y(max_ep),sun_z(max_ep)
      real*8 moon_x(max_ep),moon_y(max_ep),moon_z(max_ep)
      real*8 lon_east(max_ep),lat_nort(max_ep),alt_sat(max_ep)
      integer sat_x(max_ep),sat_y(max_ep),sat_z(max_ep)
      integer max_att,n_att
      real*8 time(max_att),ra_cas(max_att),dec_cas(max_att)
      real*8 roan_cas(max_att),asp_qual(max_att),stt_qual(max_att)
      integer stat(max_att),max_hkp,max_sta,n_sta,n_hkp
      real*8 hkp_time(max_hkp),sta_time(max_sta)
      integer hkp_rows(max_hkp)
      integer max_evr,n_evr
      real*8 ev_time(max_evr)
      integer*4 mv_aco(max_evr),xtransm(max_evr)
      integer*4 a1_al(max_evr),sa_rate(max_evr)
      integer*4 xacc(max_evr),a1al_mv(max_evr),a1_ah(max_evr)
      integer hksta_rows(max_sta)
      real*8 mjdref
      real*8 pi
        parameter (pi = 3.14159265d0)

C OUTPUT VARIABLES

      integer max_mkf,n_mkf
      real*8 md(max_mkf),lval(max_mkf),mag_ang(max_mkf)
      real*8 zen_ang(max_mkf),ses_ang(max_mkf),moon_ang(max_mkf)
      real*8 sun_ang(max_mkf),sat_lon(max_mkf),sat_lat(max_mkf)
      real*8 vel_ang(max_mkf),rsat(max_mkf),dcsat(max_mkf)
      real*8 oasp_qual(max_mkf),ostt_qual(max_mkf)
      integer ostat(max_mkf)
      real*8 ora_sc(max_mkf),odec_sc(max_mkf),oa1_al(max_mkf)
      real*8 oap(max_mkf)
      real*8 omv_aco(max_mkf),oxtransm(max_mkf),oa1_ah(max_mkf)
      real*8 oxacc(max_mkf),osa_rate(max_mkf),oa1al_mv(max_mkf)
      real*4 deadt(max_mkf)

C
C --- DESCRIPTION ------------------------------------------------------
C This program is a modification on Steve Snowden's GEOMETRY program.
C NOTE : Only the i/o has been changed, that is the data is passed
C to this routine in arrays, in the original it was read row by row.
C
C
C************************ FFORM VERSION 1.2 ************ 25-NOV-92 12:22
C
CA  author : SLS        date:  8-OCT-1993
C        
C
C   general description 
CG  This program sorts through the trend .SO and .AT files to create a
CG  new list with the .SO 60 second sample interval of a number of 
CG  geometric parameters.
C
C***********************************************************************
C
C --- AUTHORS/MODIFICATION HISTORY ---
C
C Steve Snowden 8 Oct 1993 ; Original
C Rehana Yusaf  26 May 1994 (1.1.0); Made this code into a subroutine
C                                    and passed in parameters 
C 
c Banashree M seifert (1.2.0)
c         . if condition moved from one place to 3 lines below it
c Banashree M seifert (1.3.0) Sept 9, 1997
c         . at the very end fcecho(status) is modified to fcecho(subinfo)
c Peter D Wilson (1.4.0) Dec 5, 1997
c         . explicitly zero out output status array variables
c -------------------------------------------------------------------

       character(5) version
       parameter (version = '1.4.0')
*-
        INTEGER*4    IA, IA1LL, IA1HL,IA1ALMV,
     +      IAEXE, IAXE,       IE, 
     +       IH,  
     +      IO,   
     +      IS, ISAAD,   IT1,  
     +      MV,   NROWSA, NROWSE, NROWSH, NROWSO, NROWSS,
     +          STATUS, 
     +      TOT1, TOT2
C
        REAL*8 A1HL, A1LL, AEXE,  AMV, AP, AXE,A1ALMV,   
     +      DECB, D1, D2,   DECLO, 
     +      DECLOOK, DECMO, DECMOON, DECSA, DECSAT, DECSU, DECSUN, 
     +      DIFFN, DIFFO, DR, DRA, HOUR, 
     +      MAGANG,   MONTH(12), MOONLOO,
     +      R, RAB, RAD, RALO, RALOOK, RAMO, RAMOON, RASA, RASAT, 
     +      RASU, RASUN, SAAD, SESANG, 
     +      SUNLOO, VELANG, X, XE,  Y,  Z,  ZENANG,
     +      XMOON, XSUN, YMOON, YSUN, ZMOON, ZSUN,SATX,SATY,SATZ,
     +       LATITUDE, LONGITUDE,RA,DEC,HOURANGLE
        DOUBLE PRECISION djm
        REAL BNORTH,BEAST,BDOWN,DEADTP,FLIVE,FLIVE2,FLIVE1
        REAL R4_A1LL, R4_AXE, R4_AEXE
C
        REAL*8 DD1, DD2, DDR, DDECLO, DDECSA, DDECVEL,
     +      MDJUL, DJULAT, DJULEV, DJULHK, DJULST, DRALO, DRASA, 
     +      DRAVEL, DSCS, DX, DXS, DY, DYS, DZ, DZS, TORBO
        LOGICAL LCHECK
        REAL ALT,BABS,BAB1,DIMO,LASA,LOSA,RL,YEAR
        INTEGER ICODE

        character(30) errstr
        character(70) errinfo,subinfo
        integer iymdf(4)
C
        DATA LCHECK /.TRUE./
        DATA RAD /57.2957795/
        DATA DRA /0.01745329252/
        DATA MONTH /-1.5, 30.5, 58.75, 89.75, 119.75, 150.75, 180.75,
     +      211.75, 242.75, 272.75, 303.75, 332.75/
C

        a1almv =0.0
        declook =0.0
        ralook =0.0
        djulev =0.0
        djulhk =0.0
        djulst =0.0
        dxs =0.0
        dys =0.0
        dzs =0.0
        torbo =0.0
         
        CALL INITIZE
        DEADTP = 234.0
        errstr = ' ERROR : PCF_GEO'//version//':'
        IF (chatter.GE.10) THEN
          subinfo = ' ... using PCF_GEO '//version
          call fcecho(subinfo)
        ENDIF
        IA = 0
        IE = 0
        IH = 0
        IS = 0

C
C  Start the loop over the data
C
C 
        NROWSO = n_ep
        NROWSE = n_evr
        NROWSS = n_sta
        NROWSH = n_hkp
        NROWSA = n_att
        IO = 0
        MAGANG = 0
        RL = 0
        ZENANG = 0
        SESANG = 0
        MOONLOO = 0
        SUNLOO = 0
        VELANG = 0
        STATUS = 0
        DO WHILE ((STATUS .EQ. 0) .AND. (IO .LT. NROWSO))
C
C  Read in the ORBIT time information
C  DATE    date of record in form of YYMMDD
C  SEC1    second of record
C  SEC2    subsecond of record, in units of 0.001 seconds
C
            IO = IO + 1
C            CALL FTGCFJ(LUNO,1,IO,1,1,DATE,FLAGVALO,ANYFO,STATUS)
C            CALL FTGCFJ(LUNO,2,IO,1,1,SEC1,FLAGVALO,ANYFO,STATUS)
C            CALL FTGCFJ(LUNO,3,IO,1,1,SEC2,FLAGVALO,ANYFO,STATUS)
C            SEC = SEC1 + SEC2/1000.D0
C
C  Translate the date
C  Find the fractional year for B field calculation.  This doesn't
C  need to be terribly accurate and the following code is only good to
C  half a day or so.
C
C            YEAR = AINT(DATE/10000.)
C            IM = (DATE - YEAR*10000)/100
C            ID = DATE - 10000*YEAR - 100*IM
C            YEAR = 1900. + YEAR + (MONTH(IM)+ID)/365.25
C
C  Convert date and time of record to YYYYMMDD.HHMMSS for use in 
C  conversion routine to Julian date
C
C            DT = 19000000.D0 + DATE
C            IHH = SEC/3600.
C            IM = (SEC - 3600.*IHH)/60.
C            SEC = (SEC - 3600.*IHH - 60.*IM)
C            DT = DT + IHH/100.D0 + IM/10000.D0 + SEC/1000000.D0
C
C  Find the Julian day
C
C            CALL DATEJD(DT,LCHECK,DJUL,IERR)
C            MD = DJUL - 2448000.D0
C In the RDF format the above calculations have already been done
C
c RY, NOTE: MDJUL is used instead of DJUL for determining approriate
c times for the ASPECT,EVRATE,HKP, and HKSTA data
c  
             md(IO) = mjd_int(IO) + mjd_frac(IO)
             MDJUL = md(IO) 
C
C  Set the hour angle for the conversion of ecf system to equatorial
C  HOURANGLE   Greenwich hour angle of satellite position in units of 
C  0.1 degree
C
C            CALL FTGCFJ(LUNO,17,IO,1,1,HOURANGLE,FLAGVALO,ANYFO,STATUS)
            HOURANGLE = gha(IO)
            HOUR = HOURANGLE
C
C  Set the solar coords
C  XSUN, YSUN, ZSUN are the unit vectors to the Sun in the ecf (Earth
C  Centered Frame) frame.  This is a rotating corrdinate system with
C  the X-axis fixed on the Greenwich meridian.  The units are 10^-8.
C
C            CALL FTGCFJ(LUNO,5,IO,1,1,XSUN,FLAGVALO,ANYFO,STATUS)
C            CALL FTGCFJ(LUNO,6,IO,1,1,YSUN,FLAGVALO,ANYFO,STATUS)
C            CALL FTGCFJ(LUNO,7,IO,1,1,ZSUN,FLAGVALO,ANYFO,STATUS)
            XSUN = sun_x(IO)
            YSUN = sun_y(IO)
            ZSUN = sun_z(IO)
c            X = XSUN/1.E8
c            Y = YSUN/1.E8
c            Z = ZSUN/1.E8
             X = XSUN
             Y = YSUN
             Z = ZSUN
            R = DSQRT(X*X + Y*Y + Z*Z)
c MJT 05July96 changing to radian-based dasin,datan2 for linux
c            DECSUN = DASIND(Z/R)
c            RASUN = DATAN2D(Y,X) + HOUR
            DECSUN = DASIN(Z/R)*180/pi
            RASUN = DATAN2(Y,X)*180/pi + HOUR
            IF(RASUN .LT. 0.) RASUN = RASUN + 360.
            IF(RASUN .GE. 360.) RASUN = RASUN - 360.
C
C  Set the lunar coords
C  XMOON, YMOON, ZMOON are the unit vectors to the Moon in the ecf 
C  (Earth Centered Frame) frame.  This is a rotating corrdinate system 
C  with the X-axis fixed on the Greenwich meridian.  The units are 10^-8.
C
C            CALL FTGCFJ(LUNO,8,IO,1,1,XMOON,FLAGVALO,ANYFO,STATUS)
C            CALL FTGCFJ(LUNO,9,IO,1,1,YMOON,FLAGVALO,ANYFO,STATUS)
C            CALL FTGCFJ(LUNO,10,IO,1,1,ZMOON,FLAGVALO,ANYFO,STATUS)
            XMOON = moon_x(IO)
            YMOON = moon_y(IO)
            ZMOON = moon_z(IO)
c            X = XMOON/1.E8
c            Y = YMOON/1.E8
c            Z = ZMOON/1.E8
             X = XMOON
             Y = YMOON
             Z = ZMOON
            R = DSQRT(X*X + Y*Y + Z*Z)
c MJT 05July96 changing to radian-based dasin,datan2 for linux
            DECMOON = DASIN(Z/R)*180/pi
            RAMOON = DATAN2(Y,X)*180/pi + HOUR
            IF(RAMOON .LT. 0.) RAMOON = RAMOON + 360.
            IF(RAMOON .GE. 360.) RAMOON = RAMOON - 360.
C
C  Set the altitude
C  ALTITUDE is the altitude in meters above the ellipsoid using
C  R-equator=6378.138 km and flattening = 1/298.255
C
C            CALL FTGCFJ(LUNO,16,IO,1,1,ALTITUDE,FLAGVALO,ANYFO,STATUS)

            ALT = REAL(alt_sat(IO)/1000.)
C
C  Set the satellite position
C  LONGITUDE  longitude east in ecf frame in 0.1 arc seconds
C  LATITUDE   latitude north in ecf frame in 0.1 arc seconds
C
C            CALL FTGCFJ(LUNO,14,IO,1,1,LONGITUDE,FLAGVALO,ANYFO,STATUS)
C            CALL FTGCFJ(LUNO,15,IO,1,1,LATITUDE,FLAGVALO,ANYFO,STATUS)
            LONGITUDE = lon_east(IO)
            LATITUDE = lat_nort(IO)
            LASA = LATITUDE
            SATX = DBLE(sat_x(IO))
            SATY = DBLE(sat_y(IO))
            SATZ = DBLE(sat_z(IO))
c MJT 05July96 changing to radian-based datan2 for linux
            DECSAT = DATAN2(SATZ,DSQRT(SATX**2+SATY**2))*180/pi
            LOSA = LONGITUDE
c MJT 05July96 changing to radian-based datan2 for linux
            RASAT =  DATAN2(SATY,SATX)*180/pi + gha(IO)
            IF(RASAT .LT. 0.) RASAT = RASAT + 360.
            IF(RASAT .GE. 360.) RASAT = RASAT - 360.
C
C  Find the look direction.  Search though the ATTITUDE data for
C  a close match.
C  DJULAT = Julian date of observation step
C  RALOOK = right ascension of look direction in degrees
C  DECLOOK = declination of look direction in degrees
C
            DIFFO = 10000.D0
            DIFFN = 1000.D0
            DO WHILE ((IA .LT. NROWSA) .AND. (DIFFO .GT. DIFFN))
                DIFFO = DIFFN
                IA = IA + 1
C                CALL FTGCFJ(LUNA,4,IA,1,1,IT1,FLAGVALA,
C     +                         ANYFA,STATUS)
C                CALL FTGCFI(LUNA,5,IA,1,1,IT2,FLAGVALA,
C     +                          ANYFA,STATUS)
C                DSCS = IT1 + IT2/64.D0
                DSCS = time(IA)
C                DJULAT = 2448044.379733750 + DSCS/86400.D0 
                DJULAT = mjdref + DSCS/86400.D0
                DIFFN = DABS(DJULAT-MDJUL)
            ENDDO
            IF (IA.GE.2) THEN 
              IA = IA - 1
            ENDIF
C
C  Check to see whether the time of the attitude entry is withing
C  five seconds.  If not, don't use it.  DIFFO should be the closest
C  time
C
            IF (DIFFO .LT. 6.94444D-4) THEN

C
C  Get the coordinates of the look direction
C  RA    right ascension of look direction in 0.1 arc seconds, year 2000
C  DEC   declination of look direction in 0.1 arc seconds, year 2000
C
C                CALL FTGCFJ(LUNA,7,IA,1,1,RA,FLAGVALA,ANYFA,STATUS)
C                CALL FTGCFJ(LUNA,9,IA,1,1,DEC,FLAGVALA,ANYFA,STATUS)
                RA = ra_cas(IA)
                DEC = dec_cas(IA)
C
C  Convert look direction to degrees
C
                RALOOK = RA
                DECLOOK = DEC
C
C  Convert the directions to radians for further processing
C
                DECLO = DRA*DECLOOK
                DECMO = DRA*DECMOON
                DECSA = DRA*DECSAT
                DECSU = DRA*DECSUN
                RALO = DRA*RALOOK
                RAMO = DRA*RAMOON
                RASA = DRA*RASAT
                RASU = DRA*RASUN
C
C  Get the ram angle, do it in double precision
C
                DDECSA = DECSA
                DRASA = RASA
                DDECLO = DECLO
                DRALO = RALO
C
C  Find the Catesian coords of the satellite position for this orbit 
C  entry.  This assumes a circular orbit.
C
                DX = DCOS(DRASA)*DCOS(DDECSA)
                DY = DSIN(DRASA)*DCOS(DDECSA)
                DZ = DSIN(DDECSA)
C
C  Only do the calculation if the previous orbit entry is within 
C  two and a half minutes.
C
c if(DXS .NE. 0) .OR. (DYS .NE. 0)) line has been move from
c just below IF(MDJUL-TORBO .LT. 1.7361111111D-3) THEN to where it is now

                IF(MDJUL-TORBO .LT. 1.7361111111D-3) THEN
                        DXS = DX - DXS
                        DYS = DY - DYS
                        DZS = DZ - DZS
                    IF((DXS .NE. 0) .OR. (DYS .NE. 0)) THEN
                        DDR = DSQRT(DXS*DXS + DYS*DYS + DZS*DZS)
                        DDECVEL = DASIN(DZS/DDR)
                        DRAVEL = DATAN2(DYS,DXS)

                        IF(DRAVEL .LT. 0.) DRAVEL = DRAVEL + 
     +                          6.283185307
                        DD1 = DSIN(0.5D0*(DDECVEL - DDECLO))
                        DD2 = DSIN(0.5D0*(DRAVEL - DRALO))
                        DDR = DD1*DD1 + 
     +                          DD2*DD2*DCOS(DDECVEL)*DCOS(DDECLO)
                        VELANG = 2.D0*DATAN2(DSQRT(DDR),DSQRT(1.-DDR))
                        VELANG = RAD*VELANG
                    ELSE
                        VELANG = 0.
                    ENDIF
                ELSE
                    VELANG = 0.
                ENDIF
                TORBO = MDJUL
                DXS = DX
                DYS = DY
                DZS = DZ
C
C  Find the observation zenith angle, the angle between the orbit
C  position in RA,Dec and the look direction in RA,Dec
C
                D1 = DSIN(0.5D0*(DECLO - DECSA))
                D2 = DSIN(0.5D0*(RALO - RASA))
                DR = D1*D1 + D2*D2*DCOS(DECLO)*DCOS(DECSA)
                ZENANG = 2.D0*DATAN2(DSQRT(DR),DSQRT(1.-DR))
                ZENANG = RAD*ZENANG
C
C  Find the Sun-Earth-satellite angle, the angle between the orbit
C  position in RA,Dec and the Sun position in RA,Dec
C
                D1 = DSIN(0.5D0*(DECSU - DECSA))
                D2 = DSIN(0.5D0*(RASU - RASA))
                DR = D1*D1 + D2*D2*DCOS(DECSU)*DCOS(DECSA)
                SESANG = 2.D0*DATAN2(DSQRT(DR),DSQRT(1.-DR))
                SESANG = RAD*SESANG
C
C  Find the Sun-look angle, the angle between the Sun position
C  in RA,Dec and the look direction in RA,Dec
C
                D1 = DSIN(0.5D0*(DECSU - DECLO))
                D2 = DSIN(0.5D0*(RASU - RALO))
                DR = D1*D1 + D2*D2*DCOS(DECSU)*DCOS(DECLO)
                SUNLOO = 2.D0*DATAN2(DSQRT(DR),DSQRT(1.-DR))
                SUNLOO = RAD*SUNLOO
C
C  Find the Moon-look angle, the angle between the Moon position
C  in RA,Dec and the look direction in RA,Dec
C
                D1 = DSIN(0.5D0*(DECMO - DECLO))
                D2 = DSIN(0.5D0*(RAMO - RALO))
                DR = D1*D1 + D2*D2*DCOS(DECMO)*DCOS(DECLO)
                MOONLOO = 2.D0*DATAN2(DSQRT(DR),DSQRT(1.-DR))
                MOONLOO = RAD*MOONLOO
C
C  Find the B field parameters.  The routines were taken from the
C  NSSDC
C
c added by RY, to obtain YEAR from Julian date
c
          djm = md(IO)
          call cdjcal(3,djm,iymdf,STATUS)
          IF (STATUS.NE.0) THEN
           errinfo = errstr//'calculating YEAR from Julian Date'
           call fcecho(errinfo)
          ENDIF
          YEAR = REAL(iymdf(1))
          call ccldj(iymdf(1),iymdf(2),iymdf(3),djm,STATUS)
                CALL FELDCOF(YEAR,DIMO,STATUS)
                IF (STATUS.NE.0) THEN
                  ierr = 2
                  return
                ENDIF 
                CALL FELDG(LASA,LOSA,ALT,BNORTH,BEAST,
     +                 BDOWN,BABS)
                CALL SHELLG(LASA,LOSA,ALT,DIMO,RL,ICODE,BAB1)
C
C  Get the Cartesian coords of the magnetic field in the equatorial
C  system,  this requires a transformation from the weird magnetic
C  field coords: +X is north, +Y is east, and +Z is down
C
              X = (-BNORTH*DSIN(DECSA) - BDOWN*DCOS(DECSA))*DCOS(RASA)
     +              - BEAST*DSIN(RASA)
              Y = (-BNORTH*DSIN(DECSA) - BDOWN*DCOS(DECSA))*DSIN(RASA)
     +              + BEAST*DCOS(RASA)
              Z = BNORTH*DCOS(DECSA) - BDOWN*DSIN(DECSA)
C
C  Convert to RA and Dec
C
                R = DSQRT(X*X + Y*Y + Z*Z)
c MJT 05July96 changing to radian-based asin,datan2 for linux
                DECB = ASIN(Z/R)*180/pi
                RAB  = DATAN2(Y,X)*180/pi
                IF(RAB .LT. 0.) RAB = RAB + 360.
C
C  Find the look direction-magnetic field angle
C
                D1 = DSIN(0.5D0*(DRA*DECB - DECLO))
                D2 = DSIN(0.5D0*(DRA*RAB - RALO))
                DR = D1*D1 + D2*D2*DCOS(DRA*DECB)*DCOS(DECLO)
                MAGANG = 2.D0*DATAN2(DSQRT(DR),DSQRT(1.-DR))
                MAGANG = RAD*MAGANG

c FOLLOWING ELSE added by RY

            ELSE
                MAGANG = 0
                RL = 0
                ZENANG = 0
                SESANG = 0
                MOONLOO = 0
                SUNLOO = 0
                VELANG = 0
            ENDIF
C
C  Sort through the event rate information.  Take the average over the
C  time interval +/- 30 seconds from the orbit entry
C
            DO WHILE ((IE .LT. NROWSE) .AND. 
     +                  (DJULEV .LT. MDJUL-3.47222D-4))
                IE = IE + 1
C                CALL FTGCFJ(LUNE,1,IE,1,1,IT1,FLAGVALE,
C     +                         ANYFE,STATUS)
                DSCS = ev_time(IE) 
                DJULEV = mjdref + DSCS/86400.D0
            ENDDO
            IF (IE.GE.2) THEN
              IE = IE - 1
            ENDIF
            AMV = 0.
            XE = 0.
            AXE = 0.
            AEXE = 0.
            A1LL = 0.
            A1HL = 0.
            SAAD = 0.
            TOT1 = 0.
            TOT2 = 0.
            DO WHILE ((IE .LT. NROWSE) .AND.
     +                  (DJULEV .LT. MDJUL+3.47222D-4))
C                CALL FTGCFJ(LUNE,1,IE,1,1,IT1,FLAGVALE,
C     +                         ANYFE,STATUS)
                IT1 = ev_time(IE)
                DSCS = IT1 
                DJULEV = mjdref + DSCS/86400.D0
                IF(DJULEV .LT. MDJUL+3.47222D-4) THEN
C                    CALL FTGCFJ(LUNE,2,IE,1,1,MV,FLAGVALE,
C     +                         ANYFE,STATUS)
C                    CALL FTGCFJ(LUNE,3,IE,1,1,IAEXE,FLAGVALE,
C     +                         ANYFE,STATUS)
C                    CALL FTGCFJ(LUNE,4,IE,1,1,IA1LL,FLAGVALE,
C     +                         ANYFE,STATUS)
C                    CALL FTGCFJ(LUNE,6,IE,1,1,IXE,FLAGVALE,
C     +                         ANYFE,STATUS)
C                    CALL FTGCFJ(LUNE,7,IE,1,1,IAXE,FLAGVALE,
C     +                         ANYFE,STATUS)
C                    CALL FTGCFJ(LUNE,8,IE,1,1,ISAAD,FLAGVALE,
C     +                         ANYFE,STATUS)
C                    CALL FTGCFJ(LUNE,9,IE,1,1,ISAADA,FLAGVALE,
C     +                         ANYFE,STATUS)
C                    CALL FTGCFJ(LUNE,10,IE,1,1,ISAADB,FLAGVALE,
C     +                         ANYFE,STATUS)
C                    CALL FTGCFJ(LUNE,11,IE,1,1,IA1HL,FLAGVALE,
C     +                         ANYFE,STATUS)
                    IT1 = ev_time(IE)
                    MV = mv_aco(IE)
                    IA1LL = a1_al(IE)
                    ISAAD = sa_rate(IE)
                    IAXE = xacc(IE)
                    IAEXE = xtransm(IE)
                    IA1HL = a1_ah(IE)
                    IA1ALMV = a1al_mv(IE) 

                    SAAD = SAAD + REAL(ISAAD)
C                    SAADA = SAADA + ISAADA
C                    SAADB = SAADB + ISAADB
                    TOT1 = TOT1 + 1
C
C  Check to see if the PSPC is on
C
                    IF(MV .GT. 0) THEN
                        TOT2 = TOT2 + 1
                        AMV = AMV + REAL(MV)
C                        XE = XE + IXE
                        AXE = AXE + REAL(IAXE)
                        AEXE = AEXE + IAEXE
                        A1LL = A1LL + REAL(IA1LL)
                        A1HL = A1HL + REAL(IA1HL)
                        A1ALMV = A1ALMV + REAL(IA1ALMV)
                    ENDIF
                    IE = IE + 1
                ENDIF
            ENDDO
            IF(TOT1 .GT. 0.) THEN
                SAAD = SAAD/TOT1
C                SAADA = SAADB/TOT1
C                SAADB = SAADA/TOT1
            ELSE
                SAAD = 0.
C                SAADA = 0.
C                SAADB = 0.
            ENDIF
            IF(TOT2 .GT. 0.) THEN
                AMV = AMV/TOT2
C                XE = XE/TOT2
                AXE = AXE/TOT2
                AEXE = AEXE/TOT2
                A1LL = A1LL/TOT2
                A1HL = A1HL/TOT2
                A1ALMV = A1ALMV/TOT2
                IF(A1LL .GT. 0.) THEN
                    AP = (XE - AEXE)/A1LL
                ELSE
                    AP = 0.
                ENDIF
            ELSE
                AMV = 0.
                AXE = 0.
                AEXE = 0.
                A1LL = 0.
                AP = 0.
                A1HL = 0.
                A1ALMV = 0.
            ENDIF
C
C  Sort through the housekeeping information.  
C
            DO WHILE ((IH .LT. NROWSH) .AND. 
     +                  (DJULHK .LT. MDJUL))
                IH = IH + 1
C                CALL FTGCFJ(LUNH,1,IH,1,1,IT1,FLAGVALH,
C     +                         ANYFH,STATUS)
                IT1 = hkp_time(IH)
                DSCS = IT1 
                DJULHK = mjdref + DSCS/86400.D0
            ENDDO
            hkp_rows(IO) = IH
            IF (IH.GE.2) THEN
              IH = IH - 1
            ENDIF
C            CALL FTGCFI(LUNH,2,IH,1,1,IMISS,FLAGVALH,ANYFH,STATUS)
C            CALL FTGCFI(LUNH,3,IH,1,1,ITEMP,FLAGVALH,ANYFH,STATUS)
C            CALL FTGCFI(LUNH,4,IH,1,1,IPRES,FLAGVALH,ANYFH,STATUS)
C            CALL FTGCFI(LUNH,7,IH,1,1,IFPOS,FLAGVALH,ANYFH,STATUS)
             
C  Sort through the status information.  Take the average over the
C  time interval +/- 30 seconds from the orbit entry
C
            DO WHILE ((IS .LT. NROWSS) .AND. 
     +                  (DJULST .LT. MDJUL))
                IS = IS + 1
C                CALL FTGCFJ(LUNS,1,IS,1,1,IT1,FLAGVALS,ANYFS,STATUS)
                IT1 = sta_time(IS)
                DSCS = IT1 
                DJULST = mjdref + DSCS/86400.D0
            ENDDO
           hksta_rows(IO) = IS
           IF (IS.GE.2) THEN
            IS = IS - 1
           ENDIF

C            CALL FTGCFI(LUNS,2,IS,1,1,IHV,FLAGVALS,ANYFS,STATUS)
C            CALL FTGCFI(LUNS,3,IS,1,1,ICAR,FLAGVALS,ANYFS,STATUS)
C            CALL FTGCFI(LUNS,4,IS,1,1,IGAS,FLAGVALS,ANYFS,STATUS)
C            CALL FTGCFI(LUNS,5,IS,1,1,IDET,FLAGVALS,ANYFS,STATUS)
C            CALL FTGCFI(LUNS,6,IS,1,1,ITEM,FLAGVALS,ANYFS,STATUS)
C            CALL FTGCFI(LUNS,7,IS,1,1,ILV,FLAGVALS,ANYFS,STATUS)
C            CALL FTGCFI(LUNS,8,IS,1,1,ICUR,FLAGVALS,ANYFS,STATUS)
C            CALL FTGCFI(LUNS,9,IS,1,1,INST,FLAGVALS,ANYFS,STATUS)
C            CALL FTGCFI(LUNS,10,IS,1,1,IFILT,FLAGVALS,ANYFS,STATUS)
C            CALL FTGCFI(LUNS,11,IS,1,1,ICON,FLAGVALS,ANYFS,STATUS)

C
C  Now write all this stuff out
C
C **RY** modified code so that data is NOT written row by row
C
C
C Calculate LIVE TIME
C
       IF (A1LL.GT.0.) THEN
       STATUS = 0
       R4_A1LL = REAL(A1LL)
       R4_AXE = REAL(AXE)
       R4_AEXE = REAL(AEXE)
       call livtim(R4_A1LL,DEADTP,R4_AXE,R4_AEXE,FLIVE1,
     +      FLIVE2,FLIVE,STATUS)
        IF (STATUS.NE.0) THEN
          errinfo = errstr//' error calculating LIVETIME'
          call fcecho(errinfo)
          STATUS = 0
          deadt(IO) = 0
        ELSE
          deadt(IO) = FLIVE
        ENDIF    
        ELSE
          deadt(IO) = 0.0
        ENDIF
        mag_ang(IO)  = MAGANG
        lval(IO)     = RL
        zen_ang(IO)  = ZENANG
        ses_ang(IO)  = SESANG
        moon_ang(IO) = MOONLOO 
        sun_ang(IO)  = SUNLOO
        sat_lon(IO)  = LOSA 
        sat_lat(IO)  = LASA 
        vel_ang(IO)  = VELANG
        rsat(IO)    = RASAT
        dcsat(IO)   = DECSAT

        ora_sc(IO)  = RALOOK
        odec_sc(IO) = DECLOOK
        
        omv_aco(IO)  = AMV
        oxtransm(IO) = AXE
        oa1_al(IO)   = A1LL
        osa_rate(IO) = SAAD
        oxacc(IO)    = AXE
        oa1al_mv(IO) = A1ALMV
        oa1_ah(IO)   = A1HL
        oap(IO)      = AP

C PDW 12/5/97: Make sure status variables are defined
        oasp_qual(IO) = 0
        ostt_qual(IO) = 0
        ostat(IO)     = 0
              
        
C            WRITE(25,2000) MD, RASAT, DECSAT, ALT, RALOOK, DECLOOK, 
C     +          RASUN, DECSUN, RAMOON, DECMOON
C 2000       FORMAT(1H ,F12.7,2F8.2,F8.1,6F8.2)
C            WRITE(35,2010) MD, RAB, DECB, MAGLAT, RL, 
C     +          ZENANG, SESANG, VELANG, MAGANG, SUNLOO, MOONLOO
C 2010       FORMAT(1H ,F12.7,10F8.2)
C            WRITE(45,2020) MD, AMV, AEXE, AP, SAAD, SAADA, SAADB, 
C     +          XE, AXE, A1LL, A1HL
C 2020       FORMAT(1H ,F12.7,2F7.1,F7.4,F7.2,2F9.0,4F7.1)
C            WRITE(55,2030) MD, IMISS, ITEMP, IPRES, IFPOS, IHV, ICAR, 
C     +          IGAS, IDET, ITEM, ILV, ICUR, INST, IFILT, ICON
C 2030       FORMAT(1H ,F12.7,4I6,11I5)
        ENDDO
C
        n_mkf = IO
        IF (chatter.GE.15) THEN
          write(subinfo,'(a,i4)') 'STATUS :',status
          call fcecho(subinfo)
        ENDIF
        return 
        END



*+PCF_WT
c     ----------------------------------------------------
      subroutine pcf_wt(infile, outfile,telescope,instrume,detnam,
     &                  max_mkf,n_mkf,md,mag_ang,lval,
     &                  zen_ang,ses_ang,moon_ang,sun_ang,
     &      sat_lon,sat_lat,vel_ang,rasat,decsat,asp_qual,
     &                  stt_qual,stat,ra_sc,dec_sc,
     &                  mv_aco,xtransm,a1_al,sa_rate,
     &                  xacc,a1al_mv,a1_ah,ap,
     &                  max_hkp,miss,temp,press,hvolt,filpos,
     &                  max_sta,hv_sta,carr_sta,gas_sta,
     &                  det_sta,temp_sta,filt_sta,lv_sta,
     &                  inst_sta,tel_sta,cur_sta,obi_num,
     &                  hkp_rows,hksta_rows,deadt,mjdref,
     &                  taskname,del_exist,errflg,chatter)
c     ----------------------------------------------------
c
c This routine writes a ROSAT MKF file by calling WTPCMK
c
c --- VARIABLES ---
c
      IMPLICIT NONE
      integer max_mkf,n_mkf,errflg,chatter
      character*(*) infile, outfile,telescope,instrume
      character*(*) detnam,taskname 
      real*8 md(max_mkf),mag_ang(max_mkf),lval(max_mkf)
      real*8 zen_ang(max_mkf),ses_ang(max_mkf),mjdref
      real*8 moon_ang(max_mkf),sun_ang(max_mkf)
      real*8 sat_lon(max_mkf),sat_lat(max_mkf)
      real*8 vel_ang(max_mkf),a1al_mv(max_mkf),a1_ah(max_mkf)
      real*8 rasat(max_mkf),decsat(max_mkf),ap(max_mkf)
      real*8 asp_qual(max_mkf),stt_qual(max_mkf)
      integer stat(max_mkf)
      real*8 ra_sc(max_mkf),dec_sc(max_mkf)
      real*8 mv_aco(max_mkf),xtransm(max_mkf)
      real*8 a1_al(max_mkf),sa_rate(max_mkf)
      real*8 xacc(max_mkf)
      real*4 deadt(max_mkf)
     
      integer max_hkp,max_sta
      integer miss(max_hkp),temp(max_hkp),press(max_hkp)
      integer hvolt(max_hkp),filpos(max_hkp)
      integer hv_sta(max_sta),carr_sta(max_sta)
      integer gas_sta(max_sta),det_sta(max_sta)
      integer temp_sta(max_sta),filt_sta(max_sta)
      integer lv_sta(max_sta),inst_sta(max_sta)
      integer tel_sta(max_sta),obi_num(max_sta),cur_sta(max_sta)
      integer hkp_rows(max_hkp),hksta_rows(max_sta)
      logical del_exist
c
c --- CALLED ROUTINES ---
c
c
c --- AUTHORS/MODIFICATION HISTORY ---
c
c Rehana Yusaf (June 1994) 1.0.0;
c
      character(5) version
      parameter (version = '1.0.1')
*-
c ----------------------------------------------------------------------
c
c LOCALS
c
      character(70) subinfo,errinfo,comms(2),hist(2)
      character(16) extname,hduclas3
      character(30) errstr
      integer ounit,nk_hist,nk_comm,status ,i
c
c --- USER INFO ---
c
      errstr = ' ERROR : PCF_WT Ver '//version
      IF (chatter.GE.10) THEN
         subinfo = ' ... using PCF_WT Ver '//version
         call fcecho(subinfo)
      ENDIF
c
c --- OPEN FILE AND WRITE NULL PRIMARY ARRAY ---
c
      call cgetlun(ounit)
      call opnpa(outfile,chatter,ounit,del_exist,errflg)
      IF (errflg.NE.0) THEN
       errinfo = errstr//'opening and writing primary to outfile'
       call fcecho(errinfo)
       goto 100
      ENDIF
c 
c --- WRITE OUTPUT MKF DATA ---
c
      nk_hist = 0
      nk_comm = 0
      status = 0
      extname = ' '
      hduclas3 = ' '
c
c --- Convert from MJD to secs ---
c
      do i=1,n_mkf
        md(i) = (md(i) - mjdref) * 86400.D0
      enddo
      call wtpcmkf(ounit,telescope,instrume,detnam,extname,
     &             md,mjdref,hduclas3,n_mkf,mag_ang,
     &             lval,zen_ang,ses_ang,moon_ang,sun_ang,
     &             sat_lon,sat_lat,vel_ang,rasat,decsat,
     &             asp_qual,stt_qual,stat,ra_sc,dec_sc,
     &             hkp_rows,miss,temp,press,hvolt,filpos,
     &             mv_aco,xtransm,a1_al,sa_rate,
     &             xacc,a1al_mv,a1_ah,ap,
     &             hksta_rows,hv_sta,carr_sta,gas_sta,det_sta,
     &             temp_sta,filt_sta,lv_sta,inst_sta,
     &             tel_sta,cur_sta,obi_num,deadt,
     &             nk_hist,hist,nk_comm,comms,
     &             status,chatter)


      IF (status.NE.0) THEN
        errinfo = errstr//' encountered writing MKF file'
        call fcecho(errinfo)
        errflg = status
      ENDIF
 
      status = 0
      call pcf_morekeys(infile,ounit,status)
      if(status.ne.0) goto 100

      status = 0
      call ftpdat(ounit,status)
      status= 0
      call FTPKYS(ounit,'CREATOR',
     &                  taskname,
     &             's/w task which wrote this dataset',
     &                  status)
      errinfo = errstr//' problem writing CREATOR keyword'
      IF (chatter.GE.15) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
 100  status = 0
      call ftclos(ounit,status)
      return
      end
c -------------------------------------------------------------------
c     END OF PCF_WT
c -------------------------------------------------------------------  
  
C
C Subroutine pcf_morekeys(infile,ounit,status)
C Routine to copy the keywords from input file to output file
C
C --- AUTHORS/MODIFICATION HISTORY ---
C
C Ning Gan (Apr 2000) 1.0.0;
C

      subroutine pcf_morekeys(infile, ounit,status)
      
      implicit none
      character*(*) infile 
      integer ounit
      integer iunit
      integer block
      integer iomode
      integer status
      integer i
      character(80) errinfo
      integer hdutype
      character(9) keylist(32)
      character(9) keyname
      character(63)  keyvalue
      character(20)  comment 
      

      iomode = 0
      status = 0     
      iunit = 0
      block = 0

      call ftopen(iunit,infile,iomode,block,status)
      if(status.ne.0)  then
         errinfo = 'Error opening input file: '//infile    
         call wt_ferrmsg(status,errinfo)
         return
      endif

      call ftmahd(iunit,2,hdutype,status)
      if(status.ne.0)  then
         errinfo = 'Error moving to the first EXT. of input file: '
     *       //infile     
         call wt_ferrmsg(status,errinfo)
         return
      endif
      
      keylist(1)  = 'OBS_MODE'
      keylist(2)  = 'IRAFNAME'
      keylist(3)  = 'MJDREFI'
      keylist(4)  = 'MJDREFF'
      keylist(5)  = 'ZERODATE'
      keylist(6)  = 'ZEROTIME'
      keylist(7)  = 'RDF_VERS'
      keylist(8)  = 'RDF_DATE'
      keylist(9)  = 'PROC_SYS'
      keylist(10) = 'PROCDATE'
      keylist(11) = 'REVISION'
      keylist(12) = 'FILTER'
      keylist(13) = 'OBJECT'
      keylist(14) = 'RA_NOM'
      keylist(15) = 'DEC_NOM'
      keylist(16) = 'ROLL_NOM'
      keylist(17) = 'EQUINOX'
      keylist(18) = 'OBS_ID'
      keylist(19) = 'ROR_NUM'
      keylist(20) = 'OBSERVER'
      keylist(21) = 'SETUPID'
      keylist(22) = 'DATE-OBS'
      keylist(23) = 'TIME-OBS'
      keylist(24) = 'DATE_END'
      keylist(25) = 'TIME_END'
      keylist(26) = 'MJD-OBS'
      keylist(27) = 'SCSEQBEG'
      keylist(28) = 'SCSEQEND'
      keylist(29) = 'NUM_OBIS'
      keylist(30) = 'LIVETIME'
      keylist(31) = 'DTCOR'
      keylist(32) = 'ONTIME'

      status = 0
      do i = 1, 32
          call cpkwd(iunit,ounit,keylist(i),status)
          if(status.ne.0)  then
             errinfo = 'Error copying '//keylist(i)//' keyword.'     
             call wt_ferrmsg(status,errinfo)
             status = 0
          endif 
      enddo
      
C     update the IRAFNAME keyword.
      i = 0
      i = index(infile,'_')
      if(i.ne.0) then  
          keyvalue = infile(1:i)//'stdgti.tab'
      else 
          keyvalue = infile//'_stdgti.tab'
      endif
      keyname = 'IRAFNAME'
      comment = 'IRAF file name'
      i = index(keyvalue,' ')
      call ftukys(ounit,keyname,keyvalue(1:i),comment,status)
      status = 0

      return
      end

C
C     subroutine cpkwd(iunit,ounit,keyname, status)
C Routine to copy a keyword from input file to output file
C
C --- AUTHORS/MODIFICATION HISTORY ---
C
C Ning Gan (Apr 2000) 1.0.0
C
      subroutine cpkwd(iunit,ounit,keyname, status) 

      implicit none
      integer iunit, ounit, status
      character*(*) keyname
      character(100) card
      character(80) errinfo

C     read the card from input file
      call ftgcrd(iunit,keyname,card,status) 
      if(status.ne.0) then 
         errinfo = 'Error reading '//keyname//' from input file.'     
         call wt_ferrmsg(status,errinfo)
         return
      endif

C     update the card in output file.
      call ftucrd(ounit,keyname,card,status) 
      if(status.ne.0) then 
         errinfo = 'Error writing '//keyname//' to the output file.'     
         call wt_ferrmsg(status,errinfo)
         return
      endif
      return
      end
