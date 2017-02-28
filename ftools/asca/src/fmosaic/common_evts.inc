C
C       common_evts.inc : common events block for mosaic analysis. 
C
C       Ilana Harrus 1999
C

c=====Events
      integer ngtim,nevtotfm
      parameter(ngtim=5000,nevtotfm=262144)
     
      integer*4 pha,pi,rawx,rawy
      integer*4 rise_time ,rti 
      integer*4 spread ,grade 
      integer*4 ccdid 
      real ene1,ene2 
      real detx,dety,skyx,skyy
c      real*8 time(nevtotfm)

      common/seventi/ pha(nevtotfm),pi(nevtotfm)
     & ,rawx(nevtotfm),rawy(nevtotfm)
     & ,rise_time(nevtotfm),rti(nevtotfm),spread(nevtotfm)
     & ,grade(nevtotfm),ccdid(nevtotfm)

      common/seventr/ene1(nevtotfm),ene2(nevtotfm)
     & ,detx(nevtotfm),dety(nevtotfm)
     & ,skyx(nevtotfm),skyy(nevtotfm)

c      common/seventdp/time(nevtotfm)


c---- Standard Good Time Intervals
       real*8 start,stop,exp_gti
       integer ngti
       common/sevlgti/ start(ngtim),stop(ngtim),exp_gti
       common/sevlgtii/ngti 


C------ sky
      real*8 sky_x0,sky_y0
      real*8 sky_ra0,sky_dec0
      real*8 sky_dx,sky_dy
      real*8 equinox
c------ linear.det.
      real*8 det_x0,det_y0
      real*8 detopt_x,detopt_y,skyopt_x,skyopt_y
c------ average values
      real*8 ra_pnt,dec_pnt,pa_pnt
c------ detector offsets for this list (formerely file detoffs.inc)
      real*8 detoff_x,detoff_y
c------sky (characters variables)
      character(12) radecsys,skytyp_ra,skytyp_dec

      common/sevlwcs1/sky_x0,sky_y0,sky_ra0,sky_dec0,sky_dx,sky_dy
     &,equinox
      common/sevlwcs2/det_x0,det_y0,detopt_x,detopt_y,skyopt_x,
     & skyopt_y,ra_pnt,dec_pnt, pa_pnt,detoff_x,detoff_y
      common/sevlwcs4/radecsys,skytyp_ra,skytyp_dec

c---- MISC keywords
      integer instr_id
      real*8 mjd_obs
      character(8) instrume,telescop,detnam
      character(20) date_obs,time_obs
      character(20) date_end,time_end
      character(40) fnmkf,rmfdir
      logical ccdpow

      common /sevlmisc1a/instr_id 
      common /sevlmisc1b/mjd_obs
      common /sevlmisc2/instrume,telescop,detnam
     &  ,date_obs,time_obs,date_end
     &  ,time_end,fnmkf,rmfdir
      common /sevlmisc3/ccdpow(4)
      
c---- Cut-off rigidity distributions for each event list
c---- prepared in make_hrig using mkf files
      integer nhrig
      parameter(nhrig=20)
      real hrig,hrigstp
      common/sevlhrig/hrig(nhrig),hrigstp










