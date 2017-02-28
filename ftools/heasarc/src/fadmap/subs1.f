      SUBROUTINE EVPA
C
C--------------------------------------------------------------------
C AUTHOR: Brendan Perry
C   DATE: 9/8/93
C
C This subroutine retrieves the values for the Event Selection
C parameter card. It is part of the original subroutine CONCRD
C of the FADMAP package. The event selection parameter cards are
C now in the xpi interface file 'fadmap.par'
C
C 15Jul97 - (M.Tripicco) Changed name of subroutine DAT() to DATCNV()
C           to avoid conflict with common block in raysmith task
C 23JUN1998 - (Ning Gan) Changed the string length of start_time and 
C            stop_time to 68. use fts2tm to read the time.
C 09JUL1998 - (Ning Gan) The two digit year of start_date and stop_date 
C             are interpreted as 1900 + start_date(20 century). 
C            
C            
C--------------------------------------------------------------------
C
C
      INCLUDE 'COMMON.INC'
      INTEGER status
      real start_date , stop_date
      character(68) start_time , stop_time
C
C Event selection parameters
C
      CALL UCLGST('FILENAME',Data_file,status)
      CALL UCLGSI('COORD',Coord,status)      !coordinate system (2=gal)
      CALL UCLGSI('PAIRA',Paira,status)      !maximum pair opening angle
      CALL UCLGSR('SG',Sg,status)            !spectral index
      CALL UCLGSR('SB',Sb,status)            !instrumental bg intensity
      CALL UCLGSR('BGLINT',Bglint,status)    !instrumental bg
      CALL UCLGSR('BGQUAT',Bgquat,status)    !inclination dependance
      CALL UCLGSR('BKG0',Bkg0,status)        !Ib term
      CALL UCLGSR('BKG1',Bkg1,status)        !b/k term
      CALL UCLGSR('CLO',Clo,status)          !left corner long
      CALL UCLGSR('CLA',Cla,status)          !left corner lat
      CALL UCLGSI('GAMCL',Gamcl,status)      !gamma class (2,22,3)
      CALL UCLGSI('EDTCL',Edtcl,status)      !edit class (1,2,3)
      CALL UCLGSI('SCINA',Scina,status)      !photon incidence angle
      CALL UCLGSI('FOVCL',Fovcl,status)      !fov class (0,1,2,3,4,5)
      CALL UCLGSR('ENERL',Enerl,status)      !lowest energy (37.0)
      CALL UCLGSR('ENERH',Enerh,status)      !highest energy (7818.0)
      CALL UCLGSR('INCLSIZ',Inclsiz,status)  !angular interval (0.5)
      CALL UCLGSI('N_INCL',N_incl,status)    !angle files (7)
      CALL UCLGSI('N_THETA',N_theta,status)  !number of inclination angles
      CALL UCLGSR('D_THETA',D_theta,status)  !width of redist files
      CALL UCLGSR('DLO',Dlo,status)          !bin size in deg long
      CALL UCLGSR('DLA',Dla,status)          !bin size in deg lat
      CALL UCLGSI('NLO',Nlo,status)          !# of degrees long
      Nbins_lo = Nlo*INT(1/Dlo)              !30 * 1/0.5 = 60 bins
      CALL UCLGSI('NLA',Nla,status)          !# of degrees lat
      Nbins_la = Nla*INT(1/Dla)              !30 * 1/0.5 = 60 bins
      CALL UCLGSR('STARTDATE',start_date,status) !start date yyyy.ddd
      if(start_date .lt. 100.0) start_date = 1900 + start_date 
      CALL UCLGST('STARTTIME',start_time,status)  !start time hh:mm:ss
      CALL UCLGSR('STOPDATE',stop_date,status)   !stop date yyyy.ddd
      if(stop_date .lt. 100.0) stop_date = 1900 + start_date 
      CALL UCLGST('STOPTIME',stop_time,status)   !stop time hh:mm:ss
      call uclgst('CALPATH', calpath, status)    !path to calibration files
C
C retrieve the start and stop dates and times in a user friendly format
C (yy.ddd and hh:mm:ss) and convert to mjd for comparison to the exposure
C and photon start and stop times in MJD from the data file
C
C
      IF ( start_date.NE.0. ) THEN
         CALL TIMEBOUND(start_date,start_time,Start)
         CALL TIMEBOUND(stop_date,stop_time,Stop)
      ENDIF
 
      IF ( status.NE.0 ) THEN
         WRITE (context, 99001) ' error in subroutine EVPA: ', status
         call fcerr (context)
         WRITE (context,99002) ' **** FATAL ERROR, FADMAP EXITED ****'
         call fcerr (context)
         STOP
      ENDIF
      RETURN
99001 FORMAT (A27,I4)
99002 FORMAT (A40)
      END
C
C
C
C
      SUBROUTINE GET_ESCAL
C
C--------------------------------------------------------------------
C author: Brendan Perry
C   date: 10/15/93
C
C This subroutine interpolates the values of the ESCAL array
C using the elo and ehi values from the EBOUNDS FITS calibration
C file. This returns the ESCAL array to main program FADMAP.
C--------------------------------------------------------------------
C
C
      INCLUDE 'COMMON.INC'
      INTEGER i
 
      DO 100 i = 1 , N_chan
         Escal(i) = 10.**((ALOG10(Ebounds_ehi(i))+ALOG10(Ebounds_elo(i))
     &              )/2.)
         Elog(i) = ALOG10(Escal(i))
 100  CONTINUE
C
C DETERMINE THE HIGHEST AND LOWEST POSSIBLE ENERGY BOUNDS
C
C THESE WILL ONLY BE USED AS DEFAULTS IF THE USER SELECTS A WHOLE
C FILE, RATHER THAN THEIR OWN LOW AND HIGH ENERGY BOUNDARIES
C
      if (enerl .eq. 0.0 .AND. enerh .eq. 0.0) then
          enerl = Escal(1)
          enerh = Escal(N_chan)
      endif
 
      RETURN
      END
C
C
C
C
      SUBROUTINE SET_COORD
C
C--------------------------------------------------------------------
C This subroutine fills the exposure and photon arrays xpcoord and
C phcoord with the values of the coordinate system selected by the
C user, either Right Ascension/Declination or L/B.
C
C Written by: Brendan Perry
C       Date: 11/11/93
C--------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      INTEGER i
 
      IF ( Coord.EQ.1 ) THEN
         DO 50 i = 1 , Nrowsxp
            Xpcoord1(i) = Ra_exp(i)
            Xpcoord2(i) = Dec_exp(i)
 50      CONTINUE
         DO 100 i = 1 , Nrowsph
            Phcoord1(i) = Raph(i)
            Phcoord2(i) = Decph(i)
 100     CONTINUE
      ELSE
         DO 150 i = 1 , Nrowsxp
            Xpcoord1(i) = L_exp(i)
            Xpcoord2(i) = B_exp(i)
 150     CONTINUE
         DO 200 i = 1 , Nrowsph
            Phcoord1(i) = Lph(i)
            Phcoord2(i) = Bph(i)
 200     CONTINUE
      ENDIF
      RETURN
      END
C
C
C
C
      SUBROUTINE CHECK_EXPOSURE(Xcode)
C
C--------------------------------------------------------------------
C Author: Brendan Perry
C   date: 12/17/93
C
C This subroutine takes the entered arrays of cos-b exposure data file
C and checks the exposure for validity within user selected boundaries
C from the xpi interface parameter file. It returns an error code that
C is used for accounting purposes.
C
C--------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      INTEGER Xcode
C
C Tstart and tstop are the start and stop times, in mjd, of the
C entire file. If the usre selected start and stop times are not within
C the tstart and tstop range, the selection is invalid and the
C program is exited. This only works if Start is not 0. If start is 0,
C the user selected no time constraints within the file, so process
C all exposures and events that aren't invalid for other reasons
C
      IF ( (Tstop.LT.Start .OR. Tstart.GT.Stop) 
     &	    .AND. Start.NE.0. ) THEN
	    Xcode = 1
	    RETURN
      ENDIF
C
C The program wasn't designed to handle high latitude pointings
C
      IF ( Dec_exp(X_row).LT.-65. .OR. Dec_exp(X_row).GT.+65. ) THEN
         WRITE (context,99001) X_row , Dec_exp(X_row)
         call fcerr (context)
         Xcode = 2
         RETURN
      ENDIF
C
C If the current exposure is not within the start and stop time 
C constraints, do not process.
C
      IF ( Tstopxp(X_row).LT.Start) THEN
         WRITE (context,99002) X_row
         call fcerr (context)
         Xcode = 2
         RETURN
      ENDIF
C
C upon reaching a complete exposure outside the user selected time
C constraints, signal to stop processing the fits file (xcode=3)
C
      IF ( Tstartxp(X_row).GT.Stop
     &	    .AND. Start.NE.0. ) THEN
         WRITE (context,99003) X_row
         call fcerr (context)
         write (context, 99004)
         call fcerr (context)
         Xcode = 3
         RETURN
      ENDIF
 
      IF ( Fovcl.EQ.0 ) Useft = Te0(X_row)
      IF ( Fovcl.EQ.1 ) Useft = Te15(X_row)
      IF ( Fovcl.EQ.2 ) Useft = Te20(X_row)
      IF ( Fovcl.EQ.3 ) Useft = Te25(X_row)
      IF ( Fovcl.EQ.4 ) Useft = Te30(X_row)
      IF ( Fovcl.EQ.5 ) Useft = Te35(X_row)
 
      Dels3 = Cntrate3(X_row) - 23.2
      Ruseft = Useft*Sensitiv(X_row)
 
      RETURN
99001 FORMAT (' exposure number: ',i6,' not processed; declination: ',
     &        f8.3)
99002 FORMAT (' exposure number: ',i6,
     &        ' not processed; not within times selected')
99003 FORMAT (' end of exposure processing at exposure: ',i6)
99004 FORMAT (' outside user defined time constraint')
      END
C
C
C
C
      SUBROUTINE CHECK_EVENT(Icode)
C---------------------------------------------------------------------
C     THIS ROUTINE CHECKS THE EVENT AGAINST THE SPECIFIED SELECTION
C     CRITERIA AND DECIDES ON ACCEPTANCE. A MODIFICATION OF THE H.M.H
C     ROUTINE 'EVACC', BY B.H.P. SUMMER 1993
C-------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      INTEGER Icode
C
C     CHECK SPECIFIED PARAMETERS
C
C viewph = gamli file 'gamma class' e-,e+ pairs in 2 views 'PAIRCLAS'
C eclassph = edit class, FITS file 'EDITOR' column
C engyph = energy of the photon, 'ENERGY' column in FITS file
C fovph = photon file's 'Earth/Horizon in field of view class'
C angleph = photon file pair opening angle 'PAIRANGL' in FITS file
C tstartph = photon arrival time. Must be within exposure range.
C phcoord1 = photon arrival coordinate (ra or l)
C phcoord2 = photon arrival coordinate (dec or b)
C
C all the below are legitimate values to reject a particular photon:
C
C Correction code for when the exposure straddles the 0/359 degree
C boundary. It prevents the discarding of a photon at 358 degrees
C because it is too far away from an exposure centered on 5 degrees.
C
      IF ( (Phcoord1(P_row)-Clo).GT.180. ) THEN
         Phcoord1(P_row) = Phcoord1(P_row) - 360.0
      ENDIF
      IF ( (Phcoord1(P_row)-Clo).LT.-180. ) THEN
         Phcoord1(P_row) = Phcoord1(P_row) + 360.0
      ENDIF
C
C Each photon is checked to see if it falls within user specified 
C constraints and general default constraints.  
C 
C icode values:
C  icode = 1: the photon arrival time is greater than the stop time
C	      of the current exposure. return, get new exposure, reprocess
C  icode = 2: photon does not meet user specified criteria. do not process
C
C TSTART and TSTOP are the start and stop times for the file itself.
C  START and STOP are the user specified start and stop times within
C  		  the file.
C START and STOP must be within TSTART and TSTOP, but if not, the 
C subroutine CHECK_EXPOSURE will deal with it.
C 
C If START and STOP are both 0, the only time constraint is the start
C and stop times for the whole file, TSTART and TSTOP. This is the default.
C
C
      IF ( Tstartph(P_row).GT.Tstopxp(X_row) ) THEN
         Icode = 1 			!at end of exposure
         P_row = P_row - 1		!redo current photon with
         RETURN				!next exposure
      ELSEIF ( Tstartph(P_row).LT.Start .AND. 
     &	       Tstartph(P_row).LE.Stop  .AND. Start.NE.0. ) THEN
         Icode = 2
      ELSEIF ( Tstartph(P_row).GT.Stop .AND. Start.NE.0. ) THEN
         Icode = 3
      ELSEIF ( Phcoord1(P_row).LT.(Clo-Nlo/2.) .OR. Phcoord1(P_row)
     &         .GT.(Clo+Nlo/2.) ) THEN
c         WRITE (60,99001) P_row , X_row , (Clo-Nlo/2.) , (Clo+Nlo/2.) , 
c     &                    Phcoord1(P_row)
         Icode = 2
      ELSEIF ( Phcoord2(P_row).LT.(Cla-Nla/2.) .OR. Phcoord2(P_row)
     &         .GT.(Cla+Nla/2.) ) THEN
c         WRITE (60,99002) P_row , X_row , (Cla-Nla/2.) , (Cla+Nla/2.) , 
c     &                    Phcoord2(P_row)
         Icode = 2
      ELSEIF ( Tstartph(P_row).LT.Tstartxp(X_row) ) THEN
c         WRITE (60,99003) P_row , X_row , Tstartph(P_row) , 
c     &                    Tstartxp(X_row)
         Icode = 2
      ELSEIF ( Viewph(P_row).LT.Gamcl .AND. telescop.EQ.'COS-B' ) THEN
c         WRITE (60,99004) P_row , X_row , Viewph(P_row) , Gamcl
         Icode = 2
      ELSEIF ( Eclassph(P_row).LT.Edtcl .AND. telescop.EQ.'COS-B' ) THEN
c         WRITE (60,99005) P_row , X_row , Eclassph(P_row) , Edtcl
         Icode = 2
      ELSEIF ( Engyph(P_row).LT.Enerl ) THEN
c         WRITE (60,99006) P_row , X_row , Engyph(P_row) , Enerl
         Icode = 2
      ELSEIF ( Engyph(P_row).GE.Enerh ) THEN
c         WRITE (60,99007) P_row , X_row , Engyph(P_row) , Enerh
         Icode = 2
      ELSEIF ( Fovph(P_row).LT.Fovcl ) THEN
c         WRITE (60,99008) P_row , X_row , Fovph(P_row) , Fovcl
         Icode = 2
      ELSEIF ( Angleph(P_row).GT.Paira .AND. telescop.EQ.'COS-B') THEN
c         WRITE (60,99009) P_row , X_row , Angleph(P_row) , Paira
         Icode = 2
      ENDIF
      RETURN
 
C
C EVENT IS ACCEPTED IF ICODE = 0. INDICATES THE PHOTONS
C STARTTIME IS PAST THE CURRENT EXPOSURE END TIME. A NEW EXPOSURE
C NEEDS TO BE PROCESSED. THE CURRENT PHOTON WILL HAVE TO BE REPROCESSED,
C SO P_ROW IS LOWERED BY ONE.
C
99001 FORMAT (' photon: ',i5,' exposure: ',i5,' left: ',f8.3,' right: ',
     &        f8.3,' photon: ',f8.3)
99002 FORMAT (' photon: ',i5,' exposure: ',i5,' lower: ',f8.3,
     &        ' upper: ',f8.3,' photon: ',f8.3)
99003 FORMAT (' photon: ',i5,' exposure: ',i5,' tstartph: ',f12.4,
     &        ' Tstartxp: ',f12.4)
99004 FORMAT (' photon: ',i5,' exposure: ',i5,' Viewph: ',i5,' gamcl :',
     &        i5)
99005 FORMAT (' photon: ',i5,' exposure: ',i5,' eclass: ',i5,' edtcl: ',
     &        i5)
99006 FORMAT (' photon: ',i5,' exposure: ',i5,' energyph: ',f10.3,
     &        ' enerl: ',f8.3)
99007 FORMAT (' photon: ',i5,' exposure: ',i5,' energyph: ',f10.3,
     &        ' enerh: ',f8.3)
99008 FORMAT (' photon: ',i5,' exposure: ',i5,' fovph: ',i5,' fovcl: ',
     &        i5)
99009 FORMAT (' photon: ',i5,' exposure: ',i5,' angleph: ',f8.3,
     &        ' paira: ',i5)
      END
C
C
C
C
      SUBROUTINE ADD_EVENT(mapp, sarmat)
C
C--------------------------------------------------------------------
C Author: Brendan Perry
C   Date: 11/11/93
C
C This subroutine adds each accepted photon event into the mapp array.
C The mapp(i,j,1) array forms the counts map late in the main program
C--------------------------------------------------------------------
C
C
      INCLUDE 'COMMON.INC'
      INTEGER latbin , lonbin
      real mapp (mappx, mappy, 4), sarmat (mappx, mappy, 2)
C
C     INCREMENT MAP BIN CONTENT
C
      lonbin = NINT((Phcoord1(P_row)-(Clo-Nlo/2.))*1./Dlo)
      latbin = NINT((Phcoord2(P_row)-(Cla-Nla/2.))*1./Dla)
C
C if an event is 'good' in all respects, update the 'pixel' within
C the mapp array. mapp(x,y,1) is the source count map, mapp(x,y,2)
C is the background count map, mapp(x,y,3) is the source exposure
C map, and mapp(x,y,4) is the bacxkground exposure map.
C
      IF ( Sarmat(latbin,lonbin,1).GE.-0.1 ) THEN
         Mapp(latbin,lonbin,1) = Mapp(latbin,lonbin,1) + 1
      ENDIF
 
      RETURN
      END
C
C
C
C
      SUBROUTINE CREATE_FITS(mapp, Type)
C
C--------------------------------------------------------------------
C Author: Brendan Perry
C   Date: 10/10/93
C
C This subroutine creates and opens a FITS file that will be filled
C with data from the main program, FADMAP. The file created will be
C a standard FITS image file, with the NLO by NLA array residing in
C the primary array
C
C--------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      INTEGER group , Type , status 
      character(5) fn
      character(8) ctype1 , ctype2 
      character(20) origin , bunit
      character(40) hdu2 , hducom2 , comm1 , comm2
      character(160) filename
      REAL crval1 , crval2 , cdelt1 , cdelt2
      REAL crpix1 , crpix2
      real mapp (mappx, mappy, 4)  
      integer nelem, map_ptr
      logical mapok

      Iunit = 93
      mapok = .false.
      
C
C sc = source counts
C bc = background counts
C se = source exposure
C be = background exposure
C
      IF ( telescop.EQ.'COS-B') THEN
         fn = 'cosb_'
      ELSE
         fn = 'sas2_'
      ENDIF
      
      IF ( Type.EQ.1 ) THEN
         filename = fn // obsid(1:2) // '_sc.fits'
         hdu2 = 'TOTAL'
         bunit = 'count'
         hducom2 = 'Total: source+background'
      ELSEIF ( Type.EQ.2 ) THEN
         filename = fn // obsid(1:2) // '_bc.fits'
         hdu2 = 'BKG'
         bunit = 'count'
         hducom2 = 'Background only'
      ELSEIF ( Type.EQ.3 ) THEN
         filename = fn // obsid(1:2) // '_se.fits'
         hdu2 = 'EXPOSURE'
         bunit = 's'
         hducom2 = 'Source+background exposure map'
      ELSEIF ( Type.EQ.4 ) THEN
         filename = fn // obsid(1:2) // '_be.fits'
         hdu2 = 'EXPOSURE'
         bunit = 's'
         hducom2 = 'Background exposure'
      ENDIF
      
      
      CALL FTINIT(Iunit,filename,2880,status)
C
C PPPPP
C
C  PRIMARY ARRAY VALUES
C
C PPPPP
C
      Simple = .TRUE.
      Bitpix = -32
      Pcount = 0
      Gcount = 1
      group = 0
      Naxis = 2
      Extend = .FALSE.
      crval1 = Clo
      crval2 = Cla
      if (MOD(nbins_lo,2).ne.0) then
         crpix1 = (Nbins_lo)/2.
         crpix2 = (Nbins_la)/2.
         Naxes(1) = Nbins_lo
         Naxes(2) = Nbins_la
      else
         crpix1 = (Nbins_lo)/2. + 0.5
         crpix2 = (Nbins_la)/2. + 0.5
         Naxes(1) = Nbins_lo + 1
         Naxes(2) = Nbins_la + 1
      endif
      cdelt1 = Dlo
      cdelt2 = Dla
      IF ( Coord.EQ.1 ) THEN
         ctype1 = 'RA'
         ctype2 = 'DEC'
         comm1 = 'Coordinate is Right Ascension'
         comm2 = 'Coordinate is Declination'
      ELSE
         ctype1 = 'GLON'
         ctype2 = 'GLAT'
         comm1 = 'Coordinate is Galactic I'
         comm2 = 'Coordinate is Galactic I'
      ENDIF
      origin = 'NASA/HEASARC'
C
C   CALL SUBROUTINES TO WRITE THE PRIMARY HEADER
C
      CALL FTPHPR(Iunit,Simple,Bitpix,Naxis,Naxes,Pcount,Gcount,Extend,
     &     status)
      
      CALL FTPKYE(Iunit,'CRVAL1',crval1,8,
     &     'Ref point value, pixel center, (deg)',status)
      CALL FTPKYE(Iunit,'CRPIX1',crpix1,8,'Ref point pixel location',
     &     status)
      CALL FTPKYS(Iunit,'CTYPE1',ctype1,comm1,status)
      CALL FTPKYE(Iunit,'CDELT1',cdelt1,8,
     &     'Coordinate increment in degrees',status)
      CALL FTPKYE(Iunit,'CRVAL2',crval2,8,
     &     'Ref point value, pixel center, (deg)',status)
      CALL FTPKYE(Iunit,'CRPIX2',crpix2,8,'Ref point pixel location',
     &     status)
      CALL FTPKYS(Iunit,'CTYPE2',ctype2,comm2,status)
      CALL FTPKYE(Iunit,'CDELT2',cdelt2,8,
     &     'Coordinate increment in degrees',status)
      CALL FTPKYS(Iunit,'ORIGIN',origin,'Origin of fits file',status)
      CALL FTPDAT(Iunit,status)
      CALL FTPKYS(Iunit,'TELESCOP',telescop,'Telescope (mission) name',
     &     status)
      CALL FTPKYS(Iunit,'OBJECT',Object,'Nominal target',status)
      CALL FTPKYS(Iunit,'OBS_ID',Obsid,'Observation ID',status)
      CALL FTPKYE(Iunit,'E_MIN',Enerl,3,'Minimum energy (MeV)',status)
      CALL FTPKYE(Iunit,'E_MAX',Enerh,3,'Maximum energy (MeV)',status)
      IF ( telescop.EQ.'COS-B') THEN
         CALL FTPKYJ(Iunit,'ECLASS',Edtcl,'Edit class (1-3)',status)
         CALL FTPKYJ(Iunit,'GCLASS',Gamcl,'Gamma class (2,22,3)',status)
      ENDIF
      CALL FTPKYJ(Iunit,'FCLASS',Fovcl,'Earth in FOV class (0-5)',
     &     status)
      CALL FTPKYE(Iunit,'SPECIND',Sg,3,
     &     'Spectral index of assumed celestial intensity',
     &     status)
      CALL FTPKYE(Iunit,'SPECBKG',Sb,3,
     &     'Spectral index of instrumental BG intensity',status)
      CALL FTPKYE(Iunit,'INSTBKG',bglint,5,
     &     'A1 instrumental BG',status)
      CALL FTPKYE(Iunit,'BGINTDEP',bgquat,5,
     &     'A2 BKG inclination dependence',status)
      CALL FTPKYE(Iunit,'INSTBG_IB',bkg0,5,
     &     'Instrumental BKG term Ib',status)
      CALL FTPKYE(Iunit,'INSTBGEX',bkg1,5,
     &     'Inst BKG exponent parameter b/k',status)
      CALL FTPKYS(Iunit,'BUNIT',bunit,'pixel unit value',status)
      CALL FTPKYS(Iunit,'HDUCLASS','OGIP',
     &     'Format conforms to OGIP conventions',status)
      CALL FTPKYS(Iunit,'HDUCLAS1','IMAGE',
     &     'File is an image map',status)
      CALL FTPKYS(Iunit,'HDUCLAS2',hdu2,hducom2,status)
      
      CALL FTPDEF(Iunit,Bitpix,Naxis,Naxes,Pcount,Gcount,status)
C
C GET THE CORRECT 2-D MAP TO WRITE
C
      nelem = naxes(1)*naxes(2)
      map_ptr = 0
      call udmget (nelem, 6, map_ptr, status)
      if (status .ne. 0) then
         context = ' error allocating map memory'
         call fcerr (context)
         goto 1000
      else
         mapok = .true.
C initialize to 0
         call initrealarray(memr(map_ptr), nelem)
      endif
      CALL SEPARATE_MAPS(mapp, memr(map_ptr), Type)
C
C WRITE A 2-D IMAGE INTO THE FITS FILE
C
ceag      CALL FTP2DE(Iunit,group,1000,Naxes(1),Naxes(2),Map,
ceag     &     status)
      call ftppre (iunit, group, 1, naxes(1)*naxes(2), memr(map_ptr),
     &     status)

      if (status .ne. 0) goto 1000
      CALL FTCLOS(Iunit,status)
      IF ( status.EQ.0 ) THEN
         if (mapok) call udmfre (map_ptr, 6, status)
         RETURN
      ENDIF
 1000 call fcerr 
     &     ('**** ERROR in subroutine CREATE_FITS, FADMAP exited ****')
      status = 0
      if (mapok) call udmfre (map_ptr, 6, status)
      status = 0
      call ftclos (iunit, status)
      STOP
99001 FORMAT (i3.3)
99002 FORMAT (i2.2)
      END
C
C
C
C
      SUBROUTINE SEPARATE_MAPS(mapp, map, K)
C--------------------------------------------------------------------
C This subroutine separates the mapp(x,y,4) array into it's constituent
C count, exposure, flux and background maps, then returns the appropriate
C one to be written into fits
C--------------------------------------------------------------------
      INCLUDE 'COMMON.INC'
      INTEGER i , j , K

C yes, this is messy, but the DEC Alpha compiler insists!
      integer mapx, mapy
      equivalence (naxes(1), mapx)
      equivalence (naxes(2), mapy)
      real mapp (mappx, mappy, 4)
      real map (naxes(1), naxes(2))
C
C FILL IN THE ARRAY MAP WITH THE VALUES FROM THE PROPER PART OF THE
C PREVIOUSLY CREATED MAPP ARRAY
C
      DO 100 j = 1 , INT(Nlo/Dlo)
         DO 50 i = 1 , INT(Nla/Dla)
            Map(j,i) = Mapp(i,j,K)
 50      CONTINUE
 100  CONTINUE
 
      RETURN
      END
C
C
C
C
      SUBROUTINE TIMEBOUND(Time1,Time2,Mjd)
C
C--------------------------------------------------------------------
C Author: Brendan Perry
C   Date: 4/6/94
C
C This routine takes the user specified times in yyyy.ddd and hh:mm:ss
C format and converts it to mjd. This mjd is used to compare the
C individual exposure and photon start and stop times to check for
C validity within the selection constraints of the user
C--------------------------------------------------------------------
C
      character(68) Time2
      REAL Time1
      DOUBLE PRECISION Mjd
      INTEGER iy , id , ihh , imm , iss
      integer ky,kd,km 
      double precision ss
      integer istat
 
      istat = 0
      iy = INT(Time1)
      id = NINT((Time1-iy)*1000.)
      call fts2tm(Time2, ky, km,kd, ihh, imm,ss, istat)
      iss = int(ss)
      istat = 0
c      READ (Time2(1:2),FMT=*) ihh
c      READ (Time2(4:5),FMT=*) imm
c      READ (Time2(7:8),FMT=*) iss
 
      CALL DATCNV(iy,id,Mjd)
C
C return the whole mjd, including the fraction of day
C
      Mjd = Mjd + DBLE(ihh/24.+imm/1440.+iss/86400.)
 
      RETURN
      END
C
C
C
C
      SUBROUTINE DATCNV(Yeer,Dayno,Mjd)
C
C--------------------------------------------------------------------
C Author: Brendan Perry
C   Date: 4/11/92
C
C This routine converts year and day of year into year, month, day
C to pass to the xanadu/calib/etc subroutines ccaldj and ccldj to
C convert from yy.mm.dd to mjd.
C--------------------------------------------------------------------
C
      INTEGER Yeer , Dayno , im , id , off , j
      DOUBLE PRECISION Mjd
 
      off = 0
 
      IF ( MOD(Yeer,4).EQ.0 ) THEN
         off = 1
      ELSE
         off = 0
      ENDIF
      IF ( MOD(Yeer,100).EQ.0 .AND. MOD(Yeer,400).NE.0 ) off = 0
 
      IF ( Dayno.LE.31 ) THEN
         im = 01
         id = Dayno
      ELSEIF ( Dayno.GT.31 .AND. Dayno.LE.(59+off) ) THEN
         im = 02
         id = Dayno - 31
      ELSEIF ( Dayno.GT.(59+off) .AND. Dayno.LE.(90+off) ) THEN
         im = 03
         id = Dayno - 59 - off
      ELSEIF ( Dayno.GT.(90+off) .AND. Dayno.LE.(120+off) ) THEN
         im = 04
         id = Dayno - 90 - off
      ELSEIF ( Dayno.GT.(120+off) .AND. Dayno.LE.(151+off) ) THEN
         im = 05
         id = Dayno - 120 - off
      ELSEIF ( Dayno.GT.(151+off) .AND. Dayno.LE.(181+off) ) THEN
         im = 06
         id = Dayno - 151 - off
      ELSEIF ( Dayno.GT.(181+off) .AND. Dayno.LE.(212+off) ) THEN
         im = 07
         id = Dayno - 181 - off
      ELSEIF ( Dayno.GT.(212+off) .AND. Dayno.LE.(243+off) ) THEN
         im = 08
         id = Dayno - 212 - off
      ELSEIF ( Dayno.GT.(243+off) .AND. Dayno.LE.(273+off) ) THEN
         im = 09
         id = Dayno - 243 - off
      ELSEIF ( Dayno.GT.(273+off) .AND. Dayno.LE.(304+off) ) THEN
         im = 10
         id = Dayno - 273 - off
      ELSEIF ( Dayno.GT.(304+off) .AND. Dayno.LE.(334+off) ) THEN
         im = 11
         id = Dayno - 304 - off
      ELSEIF ( Dayno.GT.(334+off) .AND. Dayno.LE.(365+off) ) THEN
         im = 12
         id = Dayno - 334 - off
      ENDIF
 
      CALL CCALDJ(Yeer,im,id,Mjd,j)
 
      IF ( j.EQ.0 ) THEN
         RETURN
      ENDIF
      STOP
99001 FORMAT (i2.2)
      END
