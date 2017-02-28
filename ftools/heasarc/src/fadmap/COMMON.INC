C
C Ning Gan 6/23/1998
C      Expanded the date_obs... from 8 characters to 68 characters.
C COMMON BLOCKS FOR THE FADMAP PACKAGE
C WRITTEN BY: Brendan Perry
C       DATE: 9/1/93
C
C Modified version of H. Meyer Hasselwander's 'common.inc' file
C
	integer xpmax, phmax
	parameter (xpmax = 40000, phmax = 10000)

C
C GENERAL COMMON BLOCK DECLARATIONS
C
      COMMON /GENERAL/ xpcoord1 , xpcoord2 , phcoord1 , phcoord2,
     &                 start , stop , tstart , tstop, mappx, mappy
      common /gentxt/ context, select, telescop, obsid

      character(180) context
      character(18) telescop
      character(3) select , obsid
      REAL xpcoord1 , xpcoord2 , phcoord1 , phcoord2
      integer mappx, mappy
      DOUBLE PRECISION start , stop , tstart , tstop
      DIMENSION xpcoord1(xpmax) , xpcoord2(xpmax) ,
     &          phcoord1(phmax) , phcoord2(phmax)
C
C FADMAP RUNNING PARAMETERS
C
      COMMON /EVPAcom/ ENERL,ENERH,SG,SB,N_CHAN,N_THETA,N_INCL,INCLSIZ,
     &              GAMCL,EDTCL,FOVCL,PAIRA,SCINA,D_THETA,
     &              COORD,BGLINT,BGQUAT,BKG0,BK,BKG1,FILETYPE
      INTEGER gamcl,edtcl,fovcl,coord,n_chan,n_theta,n_incl
      INTEGER scina , paira , filetype
      REAL    enerl , enerh , sg , sb , bglint , bk , bgquat ,
     &        bkg0 , bkg1 , inclsiz , d_theta
C
C MAP PARAMETERS COMMON BLOCK
C
      COMMON/TOTMAP/ clo,cla,nlo,nla,dlo,dla,latsiz,nbins_lo,nbins_la
      INTEGER  nlo,nla,nbins_lo,nbins_la, latsiz
      REAL     clo,cla,dlo,dla
C
C WORKMAP PARAMETERS COMMON BLOCK
C

       common /WORKMP/ sr

	real sr(1000)

C
C CALIBRATION DATA COMMON BLOCK
C
      COMMON /CALIB/  redist_array ,
     &               ebounds_elo , ebounds_ehi , theta_array
      common /calibtxt/  data_file , effarea_file , redist_file ,
     &     ebounds_file, calpath
       character(160)   data_file , effarea_file , redist_file , 
     &               ebounds_file, calpath
      REAL           redist_array , theta_array
      REAL           ebounds_elo , ebounds_ehi
      DIMENSION      redist_array(1000) , theta_array(1000)
c theta_array (n_theta) read in but never used
C redist_array(n_chan)
      DIMENSION      ebounds_elo(1000) , ebounds_ehi(1000)
c ebounds_elo (n_chan), ebounds_ehi (n_chan)
C
C MORE CALIBRATION DATA COMMON BLOCK
C
      COMMON/CALPAR/ elog,effsa,enlgl,enlgh,bgincl,escal
	real  elog,effsa,enlgl,enlgh,bgincl,escal
      DIMENSION elog(1000),effsa(1000,2),bgincl(1000),
     &          escal(1000)
C
C FITS HEADER, ETC COMMON BLOCK
C
      COMMON /FITS/ iunit , bitpix , naxis , naxes , pcount , 
     &              gcount , tfield  , simple , extend , nrows ,
     &              theta , htype , block , raob ,decob , lob , 
     &              bob
	common /fitstxt/ ttype, tform, tunit, extnam, object,
     &     date_obs, time_obs, date_end, time_end

      character(20)  object
      character(16)  ttype , tform , tunit , extnam
      character(68)  date_obs , time_obs , date_end , time_end
      INTEGER       iunit , bitpix , naxis , naxes , pcount , gcount ,
     &              tfield , nrows , theta , htype , block
      LOGICAL       simple , extend 
      REAL          raob , decob , lob , bob
      DIMENSION     naxes(99) , ttype(99) , tform(99) , tunit(99)
C
C EXPOSURE DATA COMMON BLOCK
C
      COMMON /EXPOSURE/ tstartxp , tstopxp , ra_exp , dec_exp,
     &                  sensitiv , cntrate3 , Useft , Ruseft , Dels3,
     &                  Donexp , te0 , te15 , te20 , te25 , 
     &                  te30 , te35 , x_row , nrowsxp , l_exp , b_exp
      REAL              ra_exp , dec_exp , sensitiv , cntrate3 , Useft , 
     &                  Ruseft , Dels3 , l_exp , b_exp , te0 , te15 ,
     &                  te20 , te25 , te30 , te35, Donexp 
      DOUBLE PRECISION            tstartxp , tstopxp
      INTEGER           x_row , nrowsxp
      DIMENSION         tstartxp(xpmax) , tstopxp(xpmax) , 
     &                  ra_exp(xpmax) , dec_exp(xpmax) , 
     &                  te0(xpmax) , te15(xpmax) , te20(xpmax) ,
     &                  te25(xpmax) , te30(xpmax) , te35(xpmax),
     &                  sensitiv(xpmax) , cntrate3(xpmax) ,
     &                  l_exp(xpmax) , b_exp(xpmax)
C
C EVENT DATA COMMON BLOCK
C
      COMMON /PHOTON/ raph , decph ,
     &                engyph , viewph , eclassph , fovph , doneph ,
     &                nrowsph , lph ,
     &                bph , angleph , tstartph ,
     &                p_row
      INTEGER*2       eclassph , fovph , viewph
      INTEGER         p_row , doneph , nrowsph
      REAL            angleph
      REAL            raph , decph, lph, bph , 
     &                engyph
      DOUBLE PRECISION          tstartph
      DIMENSION       raph(phmax) , decph(phmax) ,
     &                engyph(phmax) , lph(phmax) , bph(phmax) ,
     &                viewph(phmax) , eclassph(phmax) , fovph(phmax),
     &                angleph(phmax),
     &                tstartph(phmax)

C
C common definition for dynamic memory
C
C  the following MEM common block definition is in the system iraf77.inc file
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C note:
C	datatype	value
C	logical		1
C	integer*2	3
C	Integer		4
C	Long Integer	5
C	Real		6
C	Double		7
C	Complex		8

