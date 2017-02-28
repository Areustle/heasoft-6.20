
c       Geometrical parameters of detectors, describing
c       bad region (excluded from analysis)
c
c       Created: Mon May 16 01:56:43 EDT 1994 
c               usable pixels selection stuff:
c               GISs: bright ring, cal.src
c               SISs: gaps, chip's sizes
c
c       All values - in terms of DETX,DETY coordinates
c
c       18.Jan.94 - Radius for GIS2 Cal Source changed to 24.0 pix
c
c       08.Mar.99 - GIS values updated to Rev 2 selection/ 
c                     the calibration circle is now an ellipse 
c                     (changed in valid_det_pix.f as well)
c                                         Ilana Harrus
c

c======	SIS0
c...... center of address space
	real sis0_centr_x,sis0_centr_y
c...... gap width 
	real sis0_gap_x,sis0_gap_y
c...... indiv chip sizes
	real sis0_chip_x,sis0_chip_y

	data sis0_centr_x,sis0_centr_y/640.5,640.5/
	data sis0_gap_x,sis0_gap_y/4.8,18.5/
	data sis0_chip_x,sis0_chip_y/422,420/

c======	SIS1 (currently, all values are the same as for SIS0)
c...... center of address space
	real sis1_centr_x,sis1_centr_y
c...... gap width 
	real sis1_gap_x,sis1_gap_y
c...... indiv chip sizes
	real sis1_chip_x,sis1_chip_y

	data sis1_centr_x,sis1_centr_y/640.5,640.5/
	data sis1_gap_x,sis1_gap_y/4.8,18.5/
	data sis1_chip_x,sis1_chip_y/422,420/


c====== GIS2
c......	bright ring (center and radius)
	real gis2_br_x,gis2_br_y,gis2_br_r
c...... calibr.source
	real gis2_cal_x,gis2_cal_y,gis2_cal_r1,gis2_cal_r2,angle_g2
        data gis2_br_x,gis2_br_y,gis2_br_r/128.5,128.5,88.0/
        data gis2_cal_x,gis2_cal_y/167.5,220.0/
        data gis2_cal_r1,gis2_cal_r2,angle_g2/24.66,28.95,245.298/

c       REV1 
c	real gis2_cal_x,gis2_cal_y,gis2_cal_r
c	data gis2_br_x,gis2_br_y,gis2_br_r/124.0,132.0,81.0/
c	data gis2_cal_x,gis2_cal_y,gis2_cal_r/166.0,221.0,24.0/

c====== GIS3
c......	bright ring (center and radius)
	real gis3_br_x,gis3_br_y,gis3_br_r
c...... calibr.source
        real gis3_cal_x,gis3_cal_y,gis3_cal_r1,gis3_cal_r2,angle_g3

        data gis3_br_x,gis3_br_y,gis3_br_r/128.5,128.5,88.0/
        data gis3_cal_x,gis3_cal_y/217.0,95.0/
        data gis3_cal_r1,gis3_cal_r2,angle_g3/21.56,25.92,169.216/

c       REV1 
c	real gis3_cal_x,gis3_cal_y,gis3_cal_r
c	data gis3_br_x,gis3_br_y,gis3_br_r/133.0,119.0,73.0/
c	data gis3_cal_x,gis3_cal_y,gis3_cal_r/205.0,99.0,8.0/

