/*
 *
 *    BS 31/08/2004 - Bug fixed when create and fill bintable
 *    FT 14/10/2004 - erased rows before update GTI Extension
 *    FT 19/10/2004 - UpdateGtiExt: added check on rows number
 *    BS 23/11/2004 - Added WTTimeTagFirstFrame to tag WT first frame time
 *    BS 24/01/2005 - Deleted TimePix not useful routine and modified
 *                    TimeRow routine
 *                  - Added SetDoubleColumnToNull routine to set to NULL
 *                    double column  
 *    BS 26/01/2005 - Bug fixed in TimeRow routine
 *    BS 03/02/2005 - Modified algorithm to calculate time pix for PDs and 
 *                    time row for WT
 *    BS 04/02/2005 - Modified TimePix prototype
 *    BS 08/02/2005 - Added routine 'CalculateNPixels' to calculate PD frames
 *                    dimension
 *    NS 05/06/2008 - Bug fixed in RADec2XY routine for 64-bit machine
 *                   
 * 
 */

#include <xrttimefunc.h>

/*
 *   TimePix
 *       Routine to compute pixel read out time
 *
 *
 *
 */

int TimePix(
	     double fst,      /*  Frame read out start time sec */ 
	     double fet,      /*  Frame read out end time   sec */
	     int pix_read,    /*  Number of pixels read */
	     double *time_pix /*  Time to read every pixels in PD mode sec */
	     )
{
  if(pix_read > PD_NPIXS)
    *time_pix=(fet - fst)/(pix_read - PD_NPIXS);
  else
    {
      headas_chat(CHATTY, "TimePix: Error: The total number of pixels read is too small.\n");
      return NOT_OK;
    }
  
  return OK;
  
  
  
}/*TimePix */


/*
 *   TimeRow
 *       Routine to compute row read out time
 *
 *
 *
 */
void TimeRow( 
	    double fst,
	    double fet,       /* Windowed Timing columns read */
	    double *time_row /* Time to read every pixels in PD mode sec*/
	    )
{

  *time_row=(fet - fst)/(WT_NROW - 1);

}/* TimeRow */


/* 
 *  WTTimeTag 
 *       Routine to tag windowed timing frame read out time
 *
 */
void WTTimeTag(
               double ro_time,  /* Read out time */
	       int    rawy,     /* Photon rawy coordinate */
               int    src_dety, /* Source dety coordinate */
	       double time_row, /* Row read out time */
	       double *time_tag /* Photon arrival time */
	       )
{
*time_tag = (ro_time +	(rawy - 0.5 - ((int)((601+src_dety)/10)))*time_row - 
						(((601+src_dety)%10 + 1)*(TIME_PAR/1000000.))); 

}/* WTTimeTag */

void WTTimeTagFirstFrame(
               double ro_time,  /* Read out time */
	       /*int    rawy,*/     /* Photon rawy coordinate */
               /*int    src_dety,*/ /* Source dety coordinate */
	       double time_row, /* Row read out time */
	       double *time_tag /* Photon arrival time */
	       )
{

  *time_tag = ro_time  - 0.5*time_row;
/*
 *time_tag = (ro_time +	(rawy - 0.5 + ((int)((601+src_dety)/10)))*time_row + 
 (((601+src_dety)%10 + 1)*(TIME_PAR/1000000.))); 
*/
}/* WTTimeTag */

 
/* 
 *  PDTimeTag 
 *       Routine to tag photodiode frame read out time
 *
 */

void PDTimeTag(
	       int    amp,       /* Read out amplifier */
               double ro_time,  /* Read out time */
	       int    offset,   /* Photon offset coordinate */
               int    src_detx, /* Source detx coordinate */
               int    src_dety, /* Source dety coordinate */
	       double time_pix,
	       double *time_tag /* Photon arrival time */
	       )
{

  if (amp == AMP1)
    *time_tag=( ro_time + 10./1000000. + (offset - src_detx - 
					  src_dety -1238.)*time_pix);
  else
    *time_tag=(ro_time + 10./1000000. + (offset + src_detx - 
					    src_dety - 1839.)*time_pix);
  
}/*PDTimetag */

/* 
 *  RADec2XY
 *       Routine to calculate X and Y coordinates from RA and Dec
 *
 */
int RADec2XY(double ra,double dec,int xsize,int ysize, double ranom,double decnom,double roll, double pixelsize, double *x, double *y)
{

  double  ximh, yimh, rar, decr, ranomr, decnomr, roll2 , gamma1;
  double  dera, radian, ralde, derar, aiynor, aixnor, aix, aiy, ralde_arg; 
  char taskname[MAXFNAME_LEN];

 get_toolnamev(taskname);

 if (xsize == 0 || ysize == 0)
   {
     x = 0;
     y = 0;
     headas_chat(CHATTY, "%s: Warning: RADec2XY: X or Y size is set to 0.\n", taskname); 
     return OK;
   }

 radian = 180.0/PI;
 roll2 = -roll + 270;
 gamma1 = -roll2/radian;
 ximh = xsize/2. + 0.5;
 yimh = ysize/2. + 0.5;

 dera = ranom - ra;

 if (dera < -300)
   dera= ranom + 360.0 - ra; 
 
 derar=dera/radian;
 decr=dec/radian;
 decnomr=decnom/radian;
 rar=ra/radian;
 ranomr=ranom/radian;

 ralde_arg=sin(decr)*sin(decnomr)+cos(decr)*cos(decnomr)*cos(derar);
 ralde=acos((ralde_arg>1)?1:ralde_arg);
 aixnor=0.0 ;
 aiynor=0.0 ;

 if(ralde != 0)
   {
     aixnor = ralde/sin(ralde)*radian/(pixelsize)*cos(decr)*sin(derar);
     aiynor = ralde/sin(ralde)*radian/pixelsize*(sin(decr)*cos(decnomr)-cos(decr)*sin(decnomr)*cos(derar));
   }

 aix = aixnor*cos(gamma1) - aiynor*sin(gamma1) + ximh;
 aiy = aiynor*cos(gamma1) + aixnor*sin(gamma1) + yimh;
 headas_chat(CHATTY, "%s: Info: RADec2XY: Sky coordinates calculated (x,y): (%f,%f)\n",taskname, aix,aiy);
 headas_chat(CHATTY, "%s: Info: RADec2XY: from (ra,dec): (%f,%f)\n",taskname,ra,dec);

 if(aix > xsize || aix < 1 || aiy > ysize || aiy < 1)
   {
     headas_chat(NORMAL, "%s: Error: RADec2XY: Sky X and/or Y Coordinates out of range.\n",taskname);
     goto radec_end;
   }


 *x = aix ;
 *y = aiy;

 return OK;

 radec_end:
 return NOT_OK;

}/* RADec2XY */


/****************************************
 *                                      *
 *  Routines to handle GTI extensions   *
 *                                      *
 ****************************************/
/*
 *
 *  WriteGTIExt
 *      Routine to create GTI extension
 */

int WriteGTIExt(FitsFileUnit_t inunit, FitsFileUnit_t outunit, int nrows, GtiCol_t *gti)
{
  int           i, ii,n=0;
  char          date[25];
  Bintable_t	table; 
  FitsHeader_t	newhead;
  char          taskname[FLEN_FILENAME];

  get_toolnamev(taskname);     

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );
  
  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(0, &table);
      
  /* Add extension name */
  AddCard(&newhead, KWNM_EXTNAME, S,KWVL_EXTNAME_GTI, CARD_COMM_EXTNAME);
  
  /* Add columns */
  AddColumn(&newhead, &table,CLNM_START,CARD_COMM_START, "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table,CLNM_STOP,CARD_COMM_STOP, "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  
  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&newhead, KWNM_DATE, S, date, CARD_COMM_DATE);
  
  /* Finish bintable header */
  EndBintableHeader(&newhead, &table);
  
  for (i=0; i<table.nColumns; i++)
    table.cols[i] = NULL;
  
  for ( ii=0; ii<table.nColumns; ii++) {
    table.cols[ii] = malloc(nrows*table.Multiplicity[ii]*table.DataColSize[ii]);
    
    if ( table.cols[ii] == NULL ) 
      {
	headas_chat(NORMAL, "%s: Error: BadpixBintable: Unable to create %s extension.\n",taskname,KWVL_EXTNAME_GTI);
	goto create_end;
      }
  }
  
  for (n = 0; n < nrows; n++)
    {
      DVEC(table,n,0) = gti[n].start; /* START */
      DVEC(table,n,1) = gti[n].stop;  /* STOP  */
      
    }
  
    
  /* Write bintable in file */
  FinishBintableHeader(outunit, &newhead, &table);  
  WriteFastBintable(outunit, &table, nrows, TRUE);
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);
  
  if(WriteGTIKeywords(outunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write standard keywords\n", taskname);
      headas_chat(NORMAL,"%s: Error: in %s extension.\n", taskname, KWVL_EXTNAME_GTI);
      goto create_end; 
   }

  if(CopyGTIKeywords(inunit,outunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write standard keywords\n", taskname);
      headas_chat(NORMAL,"%s: Error: in %s extension.\n", taskname, KWVL_EXTNAME_GTI);
    }

  if(UpdateGTIKeywords(outunit, nrows, gti))
    {
      headas_chat(CHATTY, "%s: Error: UpdateGTIExt: Unable to update GTI standard keywords.\n", taskname); 
      goto create_end;
    }     

 
  return OK;
  
 create_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;
  


}/* WriteGTIExt */

/* 
 *
 * WriteGTIKeywords
 *    Routine to write standard keywords in GTI extension
 *
 */

int WriteGTIKeywords(FitsFileUnit_t outunit)
{
  int status=OK;

  if(fits_update_key(outunit, TSTRING, KWNM_HDUCLASS, KWVL_HDUCLASS, CARD_COMM_HDUCLASS, &status))
    goto write_end;
  
  if(fits_update_key(outunit, TSTRING, KWNM_HDUCLAS1, KWVL_HDUCLAS1_G, CARD_COMM_HDUCLAS1_G , &status))
    goto write_end;

  if(fits_update_key(outunit, TSTRING, KWNM_HDUCLAS2,KWVL_HDUCLAS2_ST , CARD_COMM_HDUCLAS2_ST  , &status))
    goto write_end;
  
  if(fits_update_key(outunit, TSTRING,KWNM_TELESCOP ,KWVL_TELESCOP ,  CARD_COMM_TELESCOP , &status))
    goto write_end;
  
  if(fits_update_key(outunit, TSTRING,KWNM_INSTRUME ,KWVL_INSTRUME ,  CARD_COMM_INSTRUME , &status))
    goto write_end;
 
  return OK;

 write_end:
  return NOT_OK;
}/* WriteGTIKeywords */


/* 
 *
 * CopyGTIKeywords
 *    Routine to copy GTI standard keywords from input to output file
 *
 */
int CopyGTIKeywords(FitsFileUnit_t inunit,FitsFileUnit_t outunit)
{
  int                status=OK, val_int=0;
  double             val=0.;
  char		     keystr[90];
  char           taskname[FLEN_FILENAME];
 
  get_toolnamev(taskname);
  if(fits_movabs_hdu(inunit, 2, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU.\n", taskname);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TIMESYS, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TIMESYS);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TIMESYS, keystr, CARD_COMM_TIMESYS, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TIMESYS);
      goto copy_end;
    }
  
  if(!fits_read_key(inunit, TDOUBLE, KWNM_MJDREFF, &val, NULL, &status))
    {
      if(fits_update_key(outunit, TDOUBLE, KWNM_MJDREFF, &val, CARD_COMM_MJDREFF, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_MJDREFF);
	  goto copy_end;
	}
      if(fits_read_key(inunit, TDOUBLE, KWNM_MJDREFI, &val, NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_MJDREFI);
	  goto copy_end;
	}
      if(fits_update_key(outunit, TDOUBLE, KWNM_MJDREFI, &val, CARD_COMM_MJDREFI, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_MJDREFI);
	  goto copy_end;
	}
    }
  else
    {
      status=0;
      if(fits_read_key(inunit, TDOUBLE, KWNM_MJDREF, &val, NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_MJDREF);
	  goto copy_end;
	}
      if(fits_update_key(outunit, TDOUBLE, KWNM_MJDREF, &val, CARD_COMM_MJDREF, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_MJDREF);
	  goto copy_end;
	}
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TIMEREF, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TIMEREF);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TIMEREF, keystr, CARD_COMM_TIMEREF, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TIMEREF);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TASSIGN, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TASSIGN);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TASSIGN, keystr, CARD_COMM_TASSIGN, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TASSIGN);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TIMEUNIT, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TIMEUNIT);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TIMEUNIT, keystr, CARD_COMM_TIMEUNIT, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TIMEUNIT);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_DATEOBS, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_DATEOBS);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_DATEOBS, keystr, CARD_COMM_DATEOBS, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_DATEOBS);
      goto copy_end;
    }
  if(fits_read_key(inunit, TSTRING, KWNM_DATEEND, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_DATEEND);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_DATEEND, keystr, CARD_COMM_DATEEND, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_DATEEND);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TLOGICAL, KWNM_CLOCKAPP, &val_int, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_CLOCKAPP);
      status=0;
    }
  else if(fits_update_key(outunit, TLOGICAL, KWNM_CLOCKAPP, &val_int, CARD_COMM_CLOCKAPP, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_CLOCKAPP);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_OBS_ID, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_OBS_ID);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_OBS_ID, keystr, CARD_COMM_OBS_ID, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_OBS_ID);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TARG_ID, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TARG_ID);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TARG_ID, keystr, CARD_COMM_TARG_ID, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TARG_ID);
      goto copy_end;
    }
 
  if(fits_read_key(inunit, TSTRING, KWNM_SEG_NUM, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_SEG_NUM);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_SEG_NUM, keystr, CARD_COMM_SEG_NUM, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_SEG_NUM);
      goto copy_end;
    }
  if(fits_read_key(inunit, TDOUBLE, KWNM_EQUINOX, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_EQUINOX);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_EQUINOX, &val, CARD_COMM_EQUINOX, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_EQUINOX);
      goto copy_end;
    }
  if(fits_read_key(inunit, TSTRING, KWNM_RADECSYS, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_RADECSYS);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_RADECSYS, keystr, CARD_COMM_RADECSYS, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_RADECSYS);
      goto copy_end;
    }
  if(fits_read_key(inunit, TSTRING, KWNM_ORIGIN, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_ORIGIN);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_ORIGIN, keystr, CARD_COMM_ORIGIN, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_ORIGIN);
      goto copy_end;
    }
  if(fits_read_key(inunit, TSTRING, KWNM_CREATOR, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_CREATOR);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_CREATOR, keystr, CARD_COMM_CREATOR, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_CREATOR);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TLM2FITS, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TLM2FITS);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TLM2FITS, keystr, CARD_COMM_TLM2FITS, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TLM2FITS);
      goto copy_end;
    }
  if(fits_read_key(inunit, TDOUBLE, KWNM_RA_PNT, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_RA_PNT);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_RA_PNT, &val, CARD_COMM_RA_PNT, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_RA_PNT);
      goto copy_end;
    }
  if(fits_read_key(inunit, TDOUBLE, KWNM_DEC_PNT, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_DEC_PNT);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_DEC_PNT, &val, CARD_COMM_DEC_PNT, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_DEC_PNT);
      goto copy_end;
    }
  if(fits_read_key(inunit, TDOUBLE, KWNM_PA_PNT, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_PA_PNT);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_PA_PNT, &val, CARD_COMM_PA_PNT, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_PA_PNT);
      goto copy_end;
    }
  if(fits_read_key(inunit, TSTRING, KWNM_OBJECT, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_OBJECT);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_OBJECT, keystr, CARD_COMM_OBJECT, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_OBJECT);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TDOUBLE, KWNM_RA_OBJ, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_RA_OBJ);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_RA_OBJ, &val, CARD_COMM_RA_OBJ, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_RA_OBJ);
      goto copy_end;
    }
  if(fits_read_key(inunit, TDOUBLE, KWNM_DEC_OBJ, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_DEC_OBJ);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_DEC_OBJ, &val, CARD_COMM_DEC_OBJ, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_DEC_OBJ);
      goto copy_end;
    }
  if(fits_read_key(inunit, TDOUBLE, KWNM_TIERRELA, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TIERRELA);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_TIERRELA, &val, CARD_COMM_TIERRELA, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TIERRELA);
      goto copy_end;
    }

  if(fits_read_key(inunit, TDOUBLE, KWNM_TIERABSO, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TIERABSO);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_TIERABSO, &val, CARD_COMM_TIERABSO, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TIERABSO);
      goto copy_end;
    }


 return OK;

 copy_end:
 return NOT_OK;
}/* CopyGTIKeywords */

/*
 *
 * UpdateGTIExt
 *    Routine to update GTI extension
 *
 */
int UpdateGTIExt(FitsFileUnit_t outunit, int nrows, GtiCol_t *gti)
{
  int            status=OK, n=0;
  char           taskname[FLEN_FILENAME];
  double         *tmptime;
  int            numoldrows;
  char           comment[80]; 

  get_toolnamev(taskname);
  if(fits_movnam_hdu(outunit, BINARY_TBL, KWVL_EXTNAME_GTI, 0, &status))
    { 
      headas_chat(CHATTY, "%s: Error: UpdateGTIExt: Unable to move in %s extension.\n", taskname,KWVL_EXTNAME_GTI); 
      goto update_end; 
    } 

  /* Get the number of rows in the GTI binary table */
  if ( fits_read_key(outunit,TINT,KWNM_NAXIS2,&numoldrows,
		     comment, &status) )
    {
      headas_chat(CHATTY, "%s: Error: UpdateGTIExt: Unable to read %s keyword.\n", taskname,KWNM_NAXIS2);
      goto update_end; 
    }

  /* Delete numoldrows in GTI binary table */
  numoldrows = (long)numoldrows;  
  if ( fits_delete_rows(outunit, 1, numoldrows, &status) )
    {
      fprintf(stderr,"ERROR:cannot delete existing GTI rows \n");
      goto update_end; 
    }

  if ( nrows > 0 ) {

	tmptime=(double *)calloc(nrows, sizeof(double));

	for (n=0; n < nrows; n++)
	  tmptime[n] = gti[n].start;
	

	if(fits_write_col(outunit, TDOUBLE, 1, 1, 1, nrows, tmptime, &status))
	  { 
		headas_chat(CHATTY, "%s: Error: UpdateGTIExt: Unable to update GTI binary table\n", taskname); 
		goto update_end; 
	  } 
	
	for (n=0; n < nrows; n++)
	  tmptime[n] = gti[n].stop;
	

	if(fits_write_col(outunit, TDOUBLE, 2, 1, 1, nrows, tmptime, &status))
	  { 
		headas_chat(CHATTY, "%s: Error: UpdateGTIExt: Unable to update GTI binary table\n", taskname); 
		goto update_end; 
	  } 


	if(UpdateGTIKeywords(outunit, nrows, gti))
	  {
		headas_chat(CHATTY, "%s: Error: UpdateGTIExt: Unable to update GTI standard keywords.\n", taskname); 
		goto update_end;
	  }
  }

  return OK;
  
    update_end:
  return NOT_OK;



}/* UpdateGTIExt */

/*
 *
 * UpdateGTIKeywords
 *    Routine to update time keywords in GTI extension
 *
 */
int UpdateGTIKeywords(FitsFileUnit_t outunit, int nrows, GtiCol_t *gti)
{
  int            status=OK, n=0;
  char           taskname[FLEN_FILENAME];
  double         val=0., ontime=0.;


  val=gti[0].start;
  if(fits_update_key(outunit, TDOUBLE, KWNM_TSTART, &val, CARD_COMM_TSTART, &status))
    {
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: Unable to write %s keyword\n", taskname, KWNM_TSTART);
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: in %s extension.\n", taskname, KWVL_EXTNAME_GTI);
      goto update_end;
    }

  val=gti[nrows-1].stop;
  if(fits_update_key(outunit, TDOUBLE, KWNM_TSTOP, &val, CARD_COMM_TSTOP, &status))
    {
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: Unable to write %s keyword\n", taskname, KWNM_TSTOP);
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: in %s extension.\n", taskname, KWVL_EXTNAME_GTI);
      goto update_end;
    }

  val=gti[nrows-1].stop - gti[0].start;
  if(fits_update_key(outunit, TDOUBLE, KWNM_TELAPSE, &val, CARD_COMM_TELAPSE, &status))
    {
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: Unable to write %s keyword\n", taskname, KWNM_TELAPSE);
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: in %s extension.\n", taskname, KWVL_EXTNAME_GTI);
      goto update_end;
    }

  ontime=0.;
  for (n=0; n < nrows; n++)
    ontime+= (gti[n].stop - gti[n].start);

  if(fits_update_key(outunit, TDOUBLE, KWNM_ONTIME, &ontime, CARD_COMM_ONTIME, &status))
    {
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: Unable to write %s keyword\n", taskname, KWNM_ONTIME);
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: in %s extension.\n", taskname, KWVL_EXTNAME_GTI);
      goto update_end;
    }


  if(fits_read_key(outunit, TDOUBLE, KWNM_DEADC, &val, NULL, &status))
    {
      status=0;
      val=1.;
    }
  val = val * ontime;
  if(fits_update_key(outunit, TDOUBLE, KWNM_LIVETIME, &val, CARD_COMM_LIVETIME, &status))
    {
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: Unable to write %s keyword\n", taskname, KWNM_LIVETIME);
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: in %s extension.\n", taskname, KWVL_EXTNAME_GTI);
      goto update_end;
    }
  if(fits_update_key(outunit, TDOUBLE, KWNM_EXPOSURE, &val, CARD_COMM_EXPOSURE, &status))
    {
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: Unable to write %s keyword\n", taskname, KWNM_EXPOSURE);
      headas_chat(CHATTY, "%s: Error: UpdateGTIKeywords: in %s extension.\n", taskname, KWVL_EXTNAME_GTI);
      goto update_end;
    }


  return OK;
 update_end:
  return NOT_OK;

}/* UpdateGTIKeywords */
int SetDoubleColumnToNull(FitsFileUnit_t unit, unsigned nrows, unsigned colnum)
{
  
  double *col1;
  int    status = OK , anynull;
  double doublenull=-1.;

  col1=(double*)calloc((nrows),sizeof(double));

  if  (fits_read_col(unit,TDOUBLE,colnum,1,1, nrows, &doublenull,col1,
		     &anynull, &status))
    goto set_end;
    

  if(fits_write_colnull(unit, TDOUBLE, colnum, 1,1,nrows, col1, &doublenull, &status))
  goto set_end;
  

  free(col1);

  return OK;

    set_end:
  free(col1);
  return NOT_OK; 



}/*SetDoubleColumnToNull*/

int CalculateNPixels(double start, double stop, int *offsets)
{

  int tot=0, diff_602=0, diff_301=0, diff_200=0, diff_100=0;
  Offset_t buff_1, buff_2;
    
  tot = (int)((stop - start)/TIME_PIX);


  diff_602=abs(OFFSETS_602 - tot);
  diff_301=abs(OFFSETS_301 - tot);
  diff_200=abs(OFFSETS_200 - tot);
  diff_100=abs(OFFSETS_100 - tot);

  if(diff_602 > diff_301)
    {
      buff_1.diff=diff_301;
      buff_1.offset=OFFSETS_301;
    }
  else
    {
      buff_1.diff=diff_602;
      buff_1.offset=OFFSETS_602;
    }


  if(diff_200 > diff_100)
    {
      buff_2.diff=diff_100;
      buff_2.offset=OFFSETS_100;
    }
  else
    {
      buff_2.diff=diff_200;
      buff_2.offset=OFFSETS_200;
    }


  if (buff_1.diff > buff_2.diff)
    *offsets = buff_2.offset;
  else
    *offsets = buff_1.offset;


  /*  headas_chat(CHATTY, "CalculateNPixels: Info: The dimension of this PD frame is: %d pixels\n", *offsets);*/
  return OK;


}/* CalculateNPixels */ 
