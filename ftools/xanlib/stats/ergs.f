      SUBROUTINE ergs(telescope,model,flag_gal,slope_t,nh,filter,e1,e2,
     &                result)
c      returns conversion factor between 1 cma cts/s and ergs cm-2 s-1
c      for different assumptions on spectral shape
c      model 1 -> power law
c      model 2 -> black body
c      model 3 -> thermal bremsstrahlung
c      flag_gal -> 1 count rate is corrected for galactic absorption
c      flag_gal -> .ne.1 count rate is uncorrected for galactic absorption
c      slope_t is energy index if model is power law
c      slope_t is temperature in ev if model is black body or t.bremss.
c      nh is galactic nh in units of atoms cm-2
c      filter is cma filter
c      filter -> 2 ppl
c      filter -> 3 thick lexan
c      filter -> 6 al parilene
c      filter -> 7 thin lexan
c      filter -> 8 boron
c      e1,e2 are min amd max of energy band. only two energy bands
c      are supported 0.05-2.0 kev and 0.3-3.5 kev.
c      result is the returned conversion factor between 1 cts/s and
c      ergs/cm2/s
c      if icount=1 (i.e. first time subroutine is called) filein is
c      open, otherwise file is assumed to be already open
      INTEGER filter, flag_gal, zicount
      character(1) filt, prefix
      character(2) model_code, gal_corr
      character(60) filein
      character(50) directory
      CHARACTER*(*) telescope
      REAL*4 datain(10), nh, nh1, nh2, zgrid
      REAL*4 step_x, t1, t2, y1, y2, y3, y4, result, step_y
      REAL*4 slope_t, alpha1, alpha2, t, u, e1, e2, energy_band
      INTEGER*4 model, lenact, i,lun,ierr
      INTEGER*4 j, ii, jj, jk
 
      LOGICAL exosat, there
      COMMON /ff    / zgrid(50,50), zicount
      exosat = .FALSE.
      ierr = 0
      IF ( telescope.EQ.'L1' ) THEN
        prefix = 'L'
        directory = 'dbase:[calibration.exosat.cma1]'
        exosat = .TRUE.
      ELSE IF ( telescope.EQ.'L2' ) THEN
        prefix = 'K'
        directory = 'dbase:[calibration.exosat.cma2]'
        exosat = .TRUE.
      ELSE IF ( telescope.EQ.'HRI' ) THEN
        prefix = 'H'
        directory = 'dbase:[calibration.einstein.hri]'
      ELSE IF ( telescope.EQ.'IPC' ) THEN
        prefix = 'I'
        directory = 'dbase:[calibration.einstein.ipc]'
      ELSE IF ( telescope.EQ.'XMM' ) THEN
        prefix = 'X'
        directory = 'dbase:[calibration.xmm.ccd]'
      END IF
      IF ( flag_gal.EQ.1 ) THEN
        gal_corr = 'gc'
      ELSE
        gal_corr = 'gu'
      END IF
      IF ( zicount.EQ.1 ) THEN
        IF ( exosat ) THEN
          IF ( e1.EQ.0.05 .AND. e2.EQ.2.0 ) THEN
            energy_band = 1
          ELSE IF ( e1.EQ.0.3 .AND. e2.EQ.3.5 ) THEN
            energy_band = 2
          ELSE
            energy_band = 1
          END IF
          WRITE (filt,'(i1)') filter
        ELSE
          energy_band = 1
          filt = 'N'
        END IF
        IF ( energy_band.EQ.1 ) THEN
          IF ( model.EQ.1 ) THEN
            model_code = 'pl'
          ELSE IF ( model.EQ.2 ) THEN
            model_code = 'bb'
          ELSE IF ( model.EQ.3 ) THEN
            model_code = 'tb'
          END IF
        ELSE IF ( model.EQ.1 ) THEN
          model_code = 'p3'
        ELSE IF ( model.EQ.2 ) THEN
          model_code = 'b3'
        ELSE IF ( model.EQ.3 ) THEN
          model_code = 't3'
        END IF
        filein = directory(1:lenact(directory))
     &           //prefix//filt//model_code//gal_corr//'.dat'
        INQUIRE (FILE=filein,EXIST=there)
        IF ( .NOT.there ) THEN
          WRITE (*,'('' file '',a,'' not found '')') filein
          RETURN
        END IF
	call getlun(lun)
        call OPENWR(lun,filein,'old',' ',' ',0,1,ierr)
        DO i = 1, 50
          DO j = 1, 5
            READ (lun,'(10(e8.3))') (datain(ii),ii=1,10)
            DO jj = 1, 10
              jk = (j-1)*10 + jj
              zgrid(jk,i) = datain(jj)
            END DO
          END DO
        END DO
      END IF
      IF ( model.EQ.1 ) THEN
        step_y = alog10(5000.)/50.
        i = slope_t/.1 + 1
        IF ( i.LT.1 .OR. i.GT.50 ) THEN
          WRITE (*,*) ' slope outside allowed range '
          RETURN
        END IF
        j = (alog10(nh)-19.+step_y)/step_y
        IF ( j.LT.1 .OR. j.GT.50 ) THEN
          WRITE (*,*) ' Nh outside allowed range '
          RETURN
        END IF
        alpha1 = float(i-1)*.1
        alpha2 = alpha1 + .1
        nh1 = 10.**(float(j-1)*step_y+19.)
        nh2 = 10.**(float(j)*step_y+19.)
        t = (slope_t-alpha1)/(alpha2-alpha1)
        u = (nh-nh1)/(nh2-nh1)
      ELSE IF ( model.EQ.2 ) THEN
        step_x = alog10(400.)/50.
        step_y = alog10(50000.)/50.
        i = (alog10(slope_t)-1.+step_x)/step_x
        j = (alog10(nh)-18.+step_y)/step_y
        t1 = 10.**(float(i-1)*step_x+1.)
        t2 = 10.**(float(i)*step_x+1.)
        nh1 = 10.**(float(j-1)*step_y+18.)
        nh2 = 10.**(float(j)*step_y+18.)
        t = (slope_t-t1)/(t2-t1)
        u = (nh-nh1)/(nh2-nh1)
      ELSE IF ( model.EQ.3 ) THEN
        step_x = alog10(200.)/50.
        step_y = alog10(50000.)/50.
        i = (alog10(slope_t)-2.+step_x)/step_x
        j = (alog10(nh)-18.+step_y)/step_y
        t1 = 10.**(float(i-1)*step_x+1.)
        t2 = 10.**(float(i)*step_x+1.)
        nh1 = 10.**(float(j-1)*step_y+18.)
        nh2 = 10.**(float(j)*step_y+18.)
        t = (slope_t-t1)/(t2-t1)
        u = (nh-nh1)/(nh2-nh1)
      END IF
      y1 = zgrid(i,j)
      y2 = zgrid(i+1,j)
      y3 = zgrid(i+1,j+1)
      y4 = zgrid(i,j+1)
      result = (1.-t)*(1.-u)*y1 + t*(1.-u)*y2 + t*u*y3 + (1.-t)*u*y4 
      close(lun)
      call frelun(lun)
      RETURN
      END
