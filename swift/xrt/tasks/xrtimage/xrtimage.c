/*
 * 
 *	xrtimage.c:
 *
 *	INVOCATION:
 *
 *		xrtimage [parameter=value ...]
 *
 *	DESCRIPTION:
 *               
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - BS 10/03/03 - First version
 *        0.1.1 - FT 15/05/03 - Added GetBiasValues, InitImagingVars,
 *                              input parameter 'bias', changed fits imaging
 *                              write function from 'fits_write_2d_int' (bug?)
 *                              to 'fits_write_img_int'
 *        0.2.0 - BS 23/06/03 - If PHA is already corrected added possibility to recompute it
 *                            - Added function to handle FITS file with two or more image extensions
 *                            - Added function to interpolate bias value
 *                            - Added routine to put to NULL bad and saturated pixels
 *        0.2.1 -    15/07/03 - Modified to use new prototype of 'CalGetFileName
 *        0.2.2 -    21/07/03 - Added 'subbias' parameter
 *        0.2.3 -    24/07/03 - Added handling of 'CTYPE' keywords
 *                            - Nothing will be done if 'bias' is zero
 *                            - Put 'XRTBPCNT' keyword in CHDU with number of pixels flagged
 *                            - Added function to create a new bad pixels primary header
 *        0.2.4 -    04/08/03 - Changed 'flagbp' parameter name in 'cleanbp'
 *         
 *        0.2.5 - RP 25/08/03 - Added input parameters screen, gtifile  
 *                            - Added function to write count image file  
 *        0.2.6 - FT 29/09/03 - Bug fixed
 *        0.2.7 - BS 29/10/03 - Deleted function to write count image file
 *        0.2.8 -    30/10/03 - Implemented routine to flag calibration sources
 *        0.2.9 -    03/11/03 - Replaced call to 'headas_parstamp()' with
 *                              'HDpar_stamp()'
 *       0.2.10 -    05/11/03 - Changed expression to select CALDB bias file
 *       0.2.11 -    12/11/03 - Using new calibration source coordinates (Analysis
 *                              performed by David Burrows using data from
 *                              30 May 2003 DitL test)
 *       0.2.12 -    21/11/03 - Added routine to handle a GTI files list
 *       0.2.13 -    25/11/03 - Bug fixed getting DEFAULT out bp filename
 *       0.2.14 -    29/12/03 - Modified to handle tdrss images
 *       0.2.15 -    23/03/04 - Deleted files list handle and modified to
 *                              use new caldb bad pixels file format
 *       0.2.16 -    24/03/04 - Minor changes
 *       0.2.17 -             - Deleted handling of 'outfile' parameter set to 'NONE'
 *       0.2.18 -    21/04/04 - After the subtraction set PHA to "0" if value 
 *                              is negative
 *       0.2.19 -    06/05/04 - Modified input parameters description and some input 
 *                              parameters name
 *       0.2.20 -    13/05/04 - Messages displayed revision
 *       0.2.21 -    24/06/04 - Bug fixed on 'fits_update_key' for 
 *                              LOGICAL keywords
 *       0.2.22 -    25/06/04 - Changed time vars initialization
 *                              and bugs fixed 
 *       0.2.23 -    08/07/04 - Added following messages displayed:
 *                              - list of cleaned bad pixels if 'chatter=5'
 *                              - Name of image file and number of HDU processed if (chatter>=3)
 *       0.3.0  -    28/07/04 - Added routines to create or update GTI extension
 *                            - Added Calibration sources file handling
 *                            - Changed bias datatype from int to real
 *       0.3.1  -    03/08/04 - Bug fixed 
 *       0.3.2  -    04/08/04 - Check on OBS_MODE keyword and in case of SLEW 
 *                              or SETTLING reject the HDU
 *       0.3.3  -    30/09/04 - Changed CALDB Badpixel Table  and On Ground Badpixels
 *                              query to take into account the change of the BPTABLE CCNM0001 value 
 *                              and a new CBD20001 keyword added to distingush 
 *                              ONBOARD badpixels from ONGROUND ones 
 *       0.3.4  -    05/10/04 - changed calculation of bias from CALDB file. No 
 *                              interpolation applied but last valid value taken
 *       0.3.5  -    02/12/04 - Minor changes on displayed messages
 *       0.3.6  -    12/05/05 - replaced SAT_BP with BAD_BP
 *       0.4.0  -    16/06/05 - Modified to handle version 8.9 of the on-board software 
 *                              Added 'hdfile' and 'biasdiff' input parameters
 *       0.4.1  -    24/06/05 - Implemented routine to take into account pixels in the
 *                              burned spot if ccdtemp greater than maxtemp 
 *       0.4.2  -    24/06/05 - Bug fixed on 'hdfile' query 
 *                            - Bug fixed when bias input par is positive and image is already
 *                              bias corrected
 *                            - Bug fixed calculating median value
 *       0.4.3  -    27/06/05 - Added warning when data are taken with new version
 *                              of the on-flight software and 'bias' input parameter
 *                              is set to a positive value 
 *       0.4.4  -    13/07/05 - Replaced ReadBintable with ReadBintableWithNULL
 *                              to perform check on undefined value reading hdfile
 *       0.4.5  -             - Bug fixed reading hdfile in old format
 *       0.4.6  -    21/02/06 - Modified to recognize on board software version
 *                              from 'IMBLVL' column values 
 *       0.4.7  - AB 17/05/06 - badpixel's TIME column handling implemented
 *       0.4.8  - FT 18/05/06 - New handle of Burned Spots implemented
 *                              
 *
 *      NOTE:
 *       
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */

#define TOOLSUB xrtimage  /* headas_main() requires that TOOLSUB be defined first */


/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtimage.h" 

/********************************/
/*         definitions          */
/********************************/

#define XRTIMAGE_C
#define XRTIMAGE_VERSION       "0.4.8"
#define PRG_NAME               "xrtimage"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrtimage_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtimage.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);  
 *           int headas_chat(int , char *, ...);
 *           void xrtimage_info(void);
 *           void get_history(int *);
 *	     void ReadInFileNamesFile(char *);
 *           char * strcpy(char *, char *);
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 10/03/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtimage_getpar()
{
  /*
   *  Get File name Parameters
   */
  
  /* Input IMAGING Files Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
      goto Error;
    }
  
  /* Output IMAGING File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;	
    }

  /* Screening input file for GTI? */
  if(PILGetBool(PAR_GTISCREEN, &global.par.gtiscreen))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GTISCREEN);
      goto Error;
    }

  if(global.par.gtiscreen)
    {
      /* GTI File Name */
      if(PILGetFname(PAR_GTIFILE, global.par.gtifile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GTIFILE);
	  goto Error;	
	}
    }

   
  /* Bias Value */

  /* subtracted bias ? */ 
  if(PILGetBool(PAR_SUBBIAS, &global.par.subbias))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SUBBIAS);
      goto Error;
    }

  if(global.par.subbias)
    {
      
      if(PILGetReal(PAR_BIAS, &global.par.bias)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIAS);
	  goto Error;	
	}
      
      if (global.par.bias < 0.)
	{
	  /* Input caldb bias Files Name */
	  if(PILGetFname(PAR_BIASFILE, global.par.biasfile)) 
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIASFILE);
	      goto Error;
	    }
	

	}
      if(PILGetInt(PAR_BIASDIFF, &global.par.biasdiff)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIASDIFF);
	  goto Error;	
	}
	
      
    }

  /* Clean bad pixels ? */ 
  if(PILGetBool(PAR_CLEANBP, &global.par.cleanbp))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CLEANBP);
      goto Error;
    }

  /* Clean calibration sources pixels ? */ 
  if(PILGetBool(PAR_CLEANSRC, &global.par.cleansrc))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CLEANSRC);
      goto Error;
    }

  if(global.par.cleansrc)
    {
      if(PILGetFname(PAR_SRCFILE, global.par.srcfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCFILE);
	  goto Error;
	}
    }

  if (global.par.cleanbp)
    {
      
      /* Get CALDB bad pixels file name */
      if(PILGetFname(PAR_BPFILE, global.par.bpfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BPFILE);
	  goto Error;
	}
      
      /* Get user bad pixels file name */ 
      if(PILGetFname(PAR_USERBPFILE, global.par.userbpfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_USERBPFILE);
	  goto Error;
	}
      
        /* Get on-board bad pixels file name */ 
      if(PILGetFname(PAR_BPTABLE, global.par.bptable))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BPTABLE);
	  goto Error;
	}
      
      /* Get output bad pixels file name */
      if(PILGetFname(PAR_OUTBPFILE, global.par.outbpfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTBPFILE);
	  goto Error;
	}

      if(PILGetReal(PAR_MAXTEMP, &global.par.maxtemp)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_MAXTEMP);
	  goto Error;	
	}

      


    }

  if(global.par.cleanbp || global.par.subbias)
    if(PILGetFname(PAR_HDFILE, global.par.hdfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HDFILE);
	goto Error;	
      }

  
  get_history(&global.hist);
  xrtimage_info();
  /* Check if input file is an ASCII fits files list */
  if (global.par.gtifile[0] == '@')
    ReadInFileNamesFile(global.par.gtifile);

  return OK;

 Error:
  return NOT_OK;
  
} /* xrtimage_getpar */

/*
 *
 *	ProcImage
 *
 *
 *	DESCRIPTION:
 *                 Routine to process image.
 *                 This routine subtract bias value and flag bad pixels
 *                 if 'cleanbp' and 'subbias' parameters are set
 *    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *           FitsHeader_t	RetrieveFitsHeader(FitsFileUnit_t unit);  
 *	     BOOL               ExistsKeyWord(const FitsHeader_t *header, const char *KeyWord,
 * 					      FitsCard_t **card);
 *           int                headas_chat(int , char *, ...);
 *           int                InitImagingVars ( FitsHeader_t );
 *           void		**ReadImage(FitsFileUnit_t unit,ValueTag_t type, int NullValue,
 *                                          unsigned xmin,unsigned xmax,unsigned ymin,unsigned ymax);
 *           int                AddBadPix(int x, int y, BadPixel_t pix, BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS]);
 *           int                fits_write_img_lng(outunit, 0, nElements*(j-global.im.ymin)+1, nElements,		
 *			                           &image[j][0], &status);
 *	
 *           void xrtimage_info(void);
 *
 *
 *
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 10/03/2003 - First version
 *
 */

int ProcImage(FitsFileUnit_t inunit, FitsFileUnit_t outunit, double *retbias, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS], int ext,int *checkext)
{
  int            i=0, j=0, status=OK, amp=0, tmp_rawx=0, tmp_rawy=0, first_x=0, first_y=0, last_x=0, last_y=0;
  int            iii=0;
  long           **image;
  double         bias_db=0., sens=0.00001, pha_db;
  double         tmp_pixx, tmp_pixy, tstart=0.0;
  short int      ev_bad_flag;
  BadPixel_t     pix;  
  FitsCard_t     *card;
  FitsHeader_t   head;
 

  TMEMSET0( &head, FitsHeader_t );
 
  /* Get Image dimension from header */
  head=RetrieveFitsHeader(inunit);    

  /* Get if needed amplifier number */


  /* Get start time */
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    tstart=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found.\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto proc_end;
    }


  if(global.par.cleanbp || global.par.cleansrc)
    {
      if (ExistsKeyWord(&head, KWNM_AMPNUM, &card))
	amp=card->u.JVal;
      else if (ExistsKeyWord(&head, KWNM_AMP, &card))
	amp=card->u.JVal;
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found.\n", global.taskname, KWNM_AMP);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  goto proc_end;
	}

      if (amp != AMP1 && amp != AMP2)
	{
	  headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname,amp);
	  headas_chat(CHATTY,"%s: Error: Values allowed for amplifier are: %d and %d.\n", global.taskname, AMP1, AMP2);
	  goto proc_end;
	}
    }
  
  

  /*---------------------------
   *       SUBTRACT BIAS
   *--------------------------*/
    if(global.par.subbias)
      {
	if(ComputeBiasValue(head, &bias_db))
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to calculate bias value.\n", global.taskname);
	    goto proc_end;
	  }

	/*if(!((bias_db >= (0. - sens)) && (bias_db  <= (0.+sens))) && global.par.subbias)*/

	headas_chat(CHATTY, "%s: Info: # %d HDU: bias value used is: %f.\n",global.taskname,(ext-1),bias_db);

      }
  
    /* Get image info from header */
    if ( InitImagingVars(head) )
      goto proc_end;

    
  /*
   *	read the map
   */

    if (!(image = (long **)ReadImage(inunit, J, global.im.nullval,
				     global.im.xmin,
				     global.im.xmax,
				     global.im.ymin,
				     global.im.ymax)))
      
      {
	headas_chat(MUTE, "%s: Error: Unable to read %d HDU\n", global.taskname, (ext-1));
	headas_chat(MUTE, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      	goto proc_end;
      }
    /*printf(">>>>>>>iii==%d HKROW == %d \n", iii, global.hkrow);*/
    if(global.par.cleanbp)
      {
	for(; iii<global.hkrow; iii++)
	  {
	    /*  printf(">> TSTART==%f HKTIME==%f\n", tstart,global.hkinfo[iii].hktime);*/ 
	    if((tstart >= global.hkinfo[iii].hktime - sens) && (tstart <= global.hkinfo[iii].hktime + sens))
	      {
		if((global.hkinfo[iii].temp > global.par.maxtemp-sens) && !((global.par.maxtemp >= 0.0 - sens)&&(global.par.maxtemp <= 0.0 + sens)))
		  {
		    headas_chat(NORMAL, "%s: Warning: the CCD temperature (%f) is greater than (%f)\n",global.taskname,global.hkinfo[iii].temp, global.par.maxtemp);
		    headas_chat(NORMAL, "%s: Warning: the burned spot will be flagged\n", global.taskname);
		  }  
		goto proc_image;
		/*printf("IMBLVL==%d -- MEDIAN==%d\n", global.hkinfo[i].imblvl,global.hkinfo[i].median);*/
	      }
	  }
	headas_chat(NORMAL, "%s: Error: Row containg HK information\n", global.taskname);
	headas_chat(NORMAL, "%s: Error: for image taken at %f time\n", global.taskname, tstart);
	headas_chat(NORMAL, "%s: Error: is not been found in %s file.\n", global.taskname, global.par.hdfile);
	goto proc_end;
	
	  
      }

 proc_image:
    for(i=0; i<global.im.naxis1 ; i++)
      {
	for(j=0; j<global.im.naxis2 ; j++)
	  {
	    
	    if(image[j][i] != SAT_VAL && image[j][i] != global.im.nullval) 
	      {
		if(!((bias_db >= (0. - sens)) && (bias_db  <= (0.+sens))) && global.par.subbias)
		  {  /* Subtract bias */
		    pha_db = image[j][i] - bias_db;
		    image[j][i]=(int)floor(pha_db + 0.5);
		    if(image[j][i] < 0)
		      image[j][i]=0;
		  }
	      }
	    else if (image[j][i] == SAT_VAL && global.par.cleanbp)
	      {

		 
		/*---------------------------------------- 
		 * Add saturated pixels in bad pixels map 
		 *----------------------------------------*/
		pix.bad_type = BAD_PIX; 	
		pix.xyextent = 1;
		pix.bad_flag = BAD_BP;
	      
		tmp_pixx = ((global.im.pixdeltx)*((double)i -  global.im.refx) +  global.im.refvalx + 1.);
		tmp_rawx = (int)tmp_pixx;

		tmp_pixy = ((global.im.pixdelty)*((double)j -  global.im.refy) +  global.im.refvaly + 1);
		tmp_rawy = (int)tmp_pixy;
	      
		if (amp == AMP1)
		  {
		
		    if (AddBadPix(tmp_rawx,tmp_rawy,pix,bp_table_amp1))
		      {
			headas_chat(CHATTY, "%s: Error: Unable to add saturated events in bad pixels list.\n", global.taskname);
			headas_chat(CHATTY, "%s: Error: Saturated event coordinates are: rawx=%d rawy=%d.\n", global.taskname, tmp_rawx, tmp_rawy);
			goto proc_end;
		      } /* if (AddBadPix ...) */
		  }
		else /* AMP == AMP2 */
		
		  {
		    if (AddBadPix(tmp_rawx,tmp_rawy,pix,bp_table_amp2))
		      {
			headas_chat(CHATTY, "%s: Error: Unable to add saturated events in bad pixels list.\n", global.taskname);
			headas_chat(CHATTY, "%s: Error: Saturated event coordinates are: rawx=%d rawy=%d.\n", global.taskname, tmp_rawx, tmp_rawy);
			goto proc_end;
		      } /* if (AddBadPix ...) */
		  }
	      
	      }
	  }/* loop on rawy */
      }/* loop on rawx */
  
    /*--------------------------------
     *      CLEAN BAD PIXELS 
     *-------------------------------*/
  
    if (global.par.cleanbp)
      {

	
		    

	if (amp == AMP1)
	  {
	    for (i=0; i<CCD_PIXS; i++)
	      {
		for (j=0; j<CCD_ROWS; j++)
		  {
		    if ( bp_table_amp1[i][j] != NULL)
		      {
			/* If temp is above threshold and maxtemp is not zero */		
			if((global.hkinfo[iii].temp > global.par.maxtemp-sens) && !((global.par.maxtemp >= 0.0 - sens)&&(global.par.maxtemp <= 0.0 + sens)))
			/* leave flag  alone  */		
			  ev_bad_flag=bp_table_amp1[i][j]->bad_flag;
			else
			/* otherwise  */		
			  {
			    ev_bad_flag=bp_table_amp1[i][j]->bad_flag;
			    /* if "Burned spot flag is on"  */		
                            if (bp_table_amp1[i][j]->bad_flag&EV_BURNED_BP)  
			      {
			    /* zero the  "Burned spot" flag   */		
				ev_bad_flag=bp_table_amp1[i][j]->bad_flag&~EV_BURNED_BP;
			    /* zero only for pure burned spots */		
				if(bp_table_amp1[i][j]->counter == 0 )
				  ev_bad_flag=GOOD;
			      }
			  }
			if(ev_bad_flag)
			  {

			    /* compute rawx */
			    tmp_pixx=(((double)i - global.im.refvalx - 1.)/global.im.pixdeltx + global.im.refx);
			    tmp_rawx = (int)floor(tmp_pixx + 0.5);
			    
			    tmp_pixy=(((double)j - global.im.refvaly - 1)/global.im.pixdelty + global.im.refy);
			    tmp_rawy = (int)floor(tmp_pixy + 0.5);
			    
			    /* compute rawy */
			    if(tmp_rawx <= global.im.xmax && tmp_rawx >= global.im.xmin &&
			       tmp_rawy <= global.im.ymax && tmp_rawy >= global.im.ymin)
			      {
				if (image[tmp_rawy][tmp_rawx]!=global.im.nullval)
				  {
				    headas_chat(CHATTY, "%s: Info: Pixels with RAWX = %d and RAWY = %d coordinates will be cleaned.\n", global.taskname, i,j);
				    image[tmp_rawy][tmp_rawx]=global.im.nullval;
				    global.bpcount++;
				  }
			      }
			  }
		      }
		  }/* loop on y */
	      }/* loop on x */
	  }/* AMP==AMP1 */
    
	else /* AMP == AMP2 */
	  {
	    for (i=0; i<CCD_PIXS; i++)
	      {
		for (j=0; j<CCD_ROWS; j++)
		  {
		    if ( bp_table_amp2[i][j] != NULL )
		      {
                       /* If temp is above threshold and maxtemp is not zero */
                        if((global.hkinfo[iii].temp > global.par.maxtemp-sens) && !((global.par.maxtemp >= 0.0 - sens)&&(global.par.maxtemp <= 0.0 + sens)))
                        /* leave flag  alone  */
                          ev_bad_flag=bp_table_amp2[i][j]->bad_flag;
                        else
                        /* otherwise  */
                          {
                            ev_bad_flag=bp_table_amp2[i][j]->bad_flag;
                            /* if "Burned spot flag is on"  */
                            if (bp_table_amp2[i][j]->bad_flag&EV_BURNED_BP)
                              {
                            /* zero the  "Burned spot" flag   */
                                ev_bad_flag=(bp_table_amp2[i][j]->bad_flag&~EV_BURNED_BP);
                            /* zero only for pure burned spots */
                                if(bp_table_amp2[i][j]->counter == 0 )
                                  ev_bad_flag=GOOD;
                              }
			  }
			if(ev_bad_flag)
			  {
			    tmp_pixx=(((double)i - global.im.refvalx - 1.)/global.im.pixdeltx + global.im.refx);
			    tmp_rawx = (int)floor(tmp_pixx + 0.5);
			    
			    tmp_pixy=(((double)j - global.im.refvaly - 1)/global.im.pixdelty + global.im.refy);
			    tmp_rawy = (int)floor(tmp_pixy + 0.5);
			    
			    if(tmp_rawx <= global.im.xmax && tmp_rawx >= global.im.xmin &&
			       tmp_rawy <= global.im.ymax && tmp_rawy >= global.im.ymin)
			      {
				if(image[tmp_rawy][tmp_rawx]!=global.im.nullval)
				  {
				    headas_chat(CHATTY, "%s: Info: Pixels with RAWX = %d and RAWY = %d coordinates will be cleaned.\n", global.taskname, i,j);
				    image[tmp_rawy][tmp_rawx]=global.im.nullval;
				    global.bpcount++;
				  }
			      }
			  }
		      }
	      
		  }/* loop on y */
	      }/* loop on x */
	  }/* Amp == amp2 */
      }
    
    /*-------------------------------------
     *   FLAG CALIBRATION SOURCES
     *------------------------------------*/
    if(global.par.cleansrc)
      {

	tmp_pixx = ((global.im.pixdeltx)*((double)global.im.xmin -  global.im.refx) +  global.im.refvalx + 1.);
	first_x = (int)tmp_pixx;

	tmp_pixx = ((global.im.pixdeltx)*((double)global.im.xmax -  global.im.refx) +  global.im.refvalx + 1.);
	last_x = (int)tmp_pixx;

	tmp_pixy = ((global.im.pixdelty)*((double)global.im.ymin -  global.im.refy) +  global.im.refvaly + 1.);
	first_y = (int)tmp_pixy;

	tmp_pixy = ((global.im.pixdelty)*((double)global.im.ymax -  global.im.refy) +  global.im.refvaly + 1.);
	last_y = (int)tmp_pixy;
		
		


	if(amp == AMP1)
	  {
	    
	    /* Check if sources shall be flagged */
	    if(((first_x - global.calsrc[0].detx + 1)*(first_x - global.calsrc[0].detx + 1) + (first_y - global.calsrc[0].dety + 1)*(first_y - global.calsrc[0].dety + 1) <= global.calsrc[0].r*global.calsrc[0].r) || ((first_x - global.calsrc[1].detx + 1)*(first_x - global.calsrc[1].detx + 1) + (last_y - global.calsrc[1].dety + 1)*(last_y - global.calsrc[1].dety + 1) <= global.calsrc[1].r*global.calsrc[1].r) || ((last_x - global.calsrc[2].detx + 1)*(last_x - global.calsrc[2].detx + 1) + (last_y - global.calsrc[2].dety + 1)*(last_y - global.calsrc[2].dety + 1) <= global.calsrc[2].r*global.calsrc[2].r) || ((last_x - global.calsrc[3].detx + 1)*(last_x - global.calsrc[3].detx + 1) + (first_y - global.calsrc[3].dety + 1)*(first_y - global.calsrc[3].dety + 1) <= global.calsrc[3].r*global.calsrc[3].r))
	      {
		
		for (i=0; i<global.im.naxis1; i++)
		  {
		    for (j=0; j<global.im.naxis2; j++)
		      {
		
			tmp_pixx = ((global.im.pixdeltx)*((double)i -  global.im.refx) +  global.im.refvalx + 1.);
			tmp_rawx = (int)tmp_pixx;

			tmp_pixy = ((global.im.pixdelty)*((double)j -  global.im.refy) +  global.im.refvaly + 1);
			tmp_rawy = (int)tmp_pixy;

			if(((tmp_rawx - global.calsrc[0].detx + 1)*(tmp_rawx - global.calsrc[0].detx + 1) +(tmp_rawy - global.calsrc[0].dety + 1)*(tmp_rawy - global.calsrc[0].dety + 1) <=global.calsrc[0].r *global.calsrc[0].r) || ((tmp_rawx - global.calsrc[1].detx + 1)* (tmp_rawx - global.calsrc[1].detx + 1)+ (tmp_rawy - global.calsrc[1].dety + 1)*(tmp_rawy - global.calsrc[1].dety + 1) <= global.calsrc[1].r*global.calsrc[1].r)  || ((tmp_rawx - global.calsrc[2].detx + 1)*(tmp_rawx - global.calsrc[2].detx + 1) +(tmp_rawy - global.calsrc[2].dety + 1)*(tmp_rawy - global.calsrc[2].dety + 1) <=global.calsrc[2].r *global.calsrc[2].r) || ((tmp_rawx - global.calsrc[3].detx + 1)*(tmp_rawx - global.calsrc[3].detx + 1) +(tmp_rawy - global.calsrc[3].dety + 1) *(tmp_rawy - global.calsrc[3].dety + 1)<=global.calsrc[3].r *global.calsrc[3].r))
			  {
			    if(image[j][i]!=global.im.nullval)
			      {
				headas_chat(CHATTY, "%s: Info: Pixels with RAWX = %d and RAWY = %d coordinates will be cleaned.\n", global.taskname, tmp_rawx,tmp_rawy);
				image[j][i]=global.im.nullval;
				global.bpcount++;
			      }
			  }
		      }/* loop on y */
		  }/* loop on x */
	      }/* flag src */
	  }/* AMP==AMP1 */
	else
	  {
	    
	    if(((last_x + global.calsrc[0].detx - CCD_PIXS)*(last_x + global.calsrc[0].detx - CCD_PIXS) + (first_y - global.calsrc[0].dety + 1)*(first_y - global.calsrc[0].dety + 1) <= global.calsrc[0].r*global.calsrc[0].r) || ((last_x + global.calsrc[1].detx - CCD_PIXS)*(last_x + global.calsrc[1].detx - CCD_PIXS) + (last_y - global.calsrc[1].dety + 1)*(last_y - global.calsrc[1].dety + 1) <= global.calsrc[1].r*global.calsrc[1].r) || ((first_x + global.calsrc[2].detx - CCD_PIXS)*(first_x + global.calsrc[2].detx - CCD_PIXS) + (last_y - global.calsrc[2].dety + 1)*(last_y - global.calsrc[2].dety + 1) <= global.calsrc[2].r*global.calsrc[2].r) || ((first_x + global.calsrc[3].detx - CCD_PIXS)*(first_x + global.calsrc[3].detx - CCD_PIXS) + (first_y - global.calsrc[3].dety + 1)*(first_y - global.calsrc[3].dety + 1) <= global.calsrc[3].r*global.calsrc[3].r))
	      {
		
		for (i=0; i<global.im.naxis1; i++)
		  {
		    for (j=0; j<global.im.naxis2; j++)
		      {
			
			tmp_pixx = ((global.im.pixdeltx)*((double)i -  global.im.refx) +  global.im.refvalx + 1.);
			tmp_rawx = (int)tmp_pixx;
			
			tmp_pixy = ((global.im.pixdelty)*((double)j -  global.im.refy) +  global.im.refvaly + 1);
			tmp_rawy = (int)tmp_pixy;

			if(((tmp_rawx + global.calsrc[0].detx - CCD_PIXS)*(tmp_rawx + global.calsrc[0].detx - CCD_PIXS) +(tmp_rawy - global.calsrc[0].dety + 1)*(tmp_rawy - global.calsrc[0].dety + 1) <=global.calsrc[0].r *global.calsrc[0].r) || ((tmp_rawx + global.calsrc[1].detx - CCD_PIXS)* (tmp_rawx + global.calsrc[1].detx - CCD_PIXS)+ (tmp_rawy - global.calsrc[1].dety + 1)*(tmp_rawy - global.calsrc[1].dety + 1) <= global.calsrc[1].r*global.calsrc[1].r)  || ((tmp_rawx + global.calsrc[2].detx - CCD_PIXS)*(tmp_rawx + global.calsrc[2].detx - CCD_PIXS) +(tmp_rawy - global.calsrc[2].dety + 1)*(tmp_rawy - global.calsrc[2].dety + 1) <=global.calsrc[2].r *global.calsrc[2].r) || ((tmp_rawx + global.calsrc[3].detx - CCD_PIXS)*(tmp_rawx + global.calsrc[3].detx - CCD_PIXS) +(tmp_rawy - global.calsrc[3].dety + 1) *(tmp_rawy - global.calsrc[3].dety + 1)<=global.calsrc[3].r *global.calsrc[3].r))
			
			  {
			    if(image[j][i]!=global.im.nullval)
			      {
				headas_chat(CHATTY, "%s: Info: Pixels with RAWX = %d and RAWY = %d coordinates will be cleaned.\n", global.taskname, tmp_rawx,tmp_rawy);
				image[j][i]=global.im.nullval;
				global.bpcount++;
			      }
			  }
			
		      }/* loop on y */
		  }/* loop on x */
	      }

	  }/* end amp==AMP2 */
      }

    /* Write array */

    for(j=0; j<global.im.naxis2 ; ++j)
      {
	fits_write_img_lng(outunit, 0, global.im.naxis1*(j-global.im.ymin)+1, global.im.naxis1,		
			   &image[j][0], &status);	      

	if ( status ) {
	  headas_chat(NORMAL, "%s: Error: Unable to write %d HDU\n", global.taskname, (ext-1));
	  headas_chat(NORMAL, "%s: Error: in %s temporary file.\n", global.taskname,global.tmpfile);
	  goto proc_end;
	}
      }
  
 
    *retbias = bias_db;

  return OK;
  
 proc_end:
  return NOT_OK;
}/* ProcImage */

/*
 *
 *	CheckGti
 *
 *
 *	DESCRIPTION:
 *                 Check if image tstart and tstop are between gti start and stop 
 *                
 *    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *
 *
 *
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - RP 27/08/2003 - First version
 */
int CheckGti(FitsHeader_t head, BOOL *check)
{
  int                   i;
  double                tstart=0., tstop=0.;
  BOOL                  first_time=1;
  FitsCard_t            *card;
  double                sens=0.00001;

  *check = FALSE;

  
  /* Get start time */
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    tstart=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto check_end;
    }		
	    
  /* Get stop time */
  if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
    tstop=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto check_end;
    }	


  for (i = 0; i< global.gti_rows; i++) 
    {
      
      if ((tstart>= (global.gtivalues[i].start-sens)) && (tstop <= (global.gtivalues[i].stop+sens))) 
	{


	  if ((tstart < (global.min_tstart +sens) ) || first_time)
	    global.min_tstart = tstart;
	  if ((tstop > (global.max_tstop -sens) ) || first_time)
	    global.max_tstop = tstop;

	  first_time=0;
	  *check = TRUE;
	  return OK;
	}
    }

  

  return OK;

 check_end:
  return NOT_OK;

} /* CheckGti*/
/*
 *
 *	GetBiasValues
 *
 *
 *	DESCRIPTION:
 *                 Routine to get bias values in CALDB 
 *                 bias file
 *    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *           int                strcasecmp(const char *, const char *);
 *           int                CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd,
 *                                             char *TimeEnd,const char *DataSet, char *CalFileName, char *expr);
 *           int                headas_chat(int , char *, ...);
 *           FitsFileUnit_t     OpenReadFitsFile(char *name);
 *           FitsHeader_t	RetrieveFitsHeader(FitsFileUnit_t unit); 
 *           int                fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, 
 *                                              int extver, int *status);
 *           void               GetBintableStructure(FitsHeader_t *header, Bintable_t *table, 
 *                                                   const int MaxBlockRows, const unsigned nColumns, 
 *                                                   const unsigned ActCols[]);
 *           int		GetColNameIndx(const Bintable_t *table, const char *ColName);
 *           void *             calloc(size_t nmemb, size_t size);
 *           int                ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, 
 *                                           const unsigned nColumns, const unsigned ActCols[], 
 *                                           const unsigned FromRow, unsigned *nRows);
 *           unsigned           ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *           int                CloseFitsFile(FitsFileUnit_t file);
 *
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 10/03/2003 - First version
 */
int GetBiasValues(void)
{
  int            n, status=OK;
  long           extno;
  unsigned       FromRow, ReadRows, OutRows, nCols, i; 
  BiasCol_t      biascol; 
  Bintable_t     table;             /* Bintable pointer */  
  FitsHeader_t   head;              /* Extension pointer */
  FitsFileUnit_t biasunit=NULL;        /* Bias file pointer */

  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  

 /* Derive CALDB bias filename */ 
  if (!(strcasecmp (global.par.biasfile, DF_CALDB)))
    {
      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_BIAS_DSET, global.par.biasfile, "datamode.eq.longima", &extno))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto get_end;
	}
      else
	headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.biasfile);
    }

  /* Open read only bias file */
  if ((biasunit=OpenReadFitsFile(global.par.biasfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.biasfile);
      goto get_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: reading '%s' file.\n", global.taskname, global.par.biasfile);
 
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL, biasunit, global.par.biasfile,global.taskname);

  /* move to  BIAS extension */
  if (fits_movnam_hdu(biasunit,ANY_HDU,KWVL_EXTNAME_BIAS,0,&status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname,KWVL_EXTNAME_BIAS);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.biasfile); 
      goto get_end;
    } 

  head = RetrieveFitsHeader(biasunit);

  /* Read bias bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;

  /* Get columns index from name */
  if ((biascol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
	{ 
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.biasfile);
	  goto get_end;
	}
      
      if ((biascol.BIAS = GetColNameIndx(&table, CLNM_BIAS)) == -1 )
	{ 
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BIAS);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.biasfile);
	  goto get_end;
	}
            
      global.caldbrows =table.MaxRows;
      
      /* Allocate memory to storage all coefficients */
      global.biasvalues=(BiasRow_t *)calloc(global.caldbrows, sizeof(BiasRow_t));
      
      /* Read blocks of bintable rows from input bias file */
      FromRow=1; 
      ReadRows = table.nBlockRows;
      OutRows=0;
      i=0;
      while (ReadBintable(biasunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
	{
	  for (n=0; n<ReadRows; ++n)
	    {
	      /* get columns value */
	      global.biasvalues[i].time=DVEC(table,n,biascol.TIME);
	      global.biasvalues[i].bias=DVEC(table,n,biascol.BIAS);
	      i++;
	    }
	  
	  
	  FromRow += ReadRows;
	  ReadRows = BINTAB_ROWS;
	}
      /* Free memory allocated with bintable data */
      ReleaseBintable(&head, &table);
      /*   } */     
  /* Close bias file */
  if (CloseFitsFile(biasunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.biasfile);
      goto get_end;
    }
  
  return OK;
  
 get_end:
   
  return NOT_OK;

}/* GetBiasValues */


/*
 *
 *	LoadGtiTable
 *
 *
 *	DESCRIPTION:
 *                 Load gti table (start,stop) 
 *                
 *    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *           int                CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd,
 *                                             char *TimeEnd,const char *DataSet, char *CalFileName, char *expr);
 *           int                headas_chat(int , char *, ...);
 *           FitsFileUnit_t     OpenReadFitsFile(char *name);
 *           FitsHeader_t	RetrieveFitsHeader(FitsFileUnit_t unit); 
 *           int                fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, 
 *                                              int extver, int *status);
 *           void               GetBintableStructure(FitsHeader_t *header, Bintable_t *table, 
 *                                                   const int MaxBlockRows, const unsigned nColumns, 
 *                                                   const unsigned ActCols[]);
 *           void *             calloc(size_t nmemb, size_t size);
 *           int                ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, 
 *                                           const unsigned nColumns, const unsigned ActCols[], 
 *                                           const unsigned FromRow, unsigned *nRows);
 *           unsigned           ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *           int                CloseFitsFile(FitsFileUnit_t file);
 *
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - RP 27/08/2003 - First version
 */
int LoadGtiTable(double *mintstart,double *maxtstop)
{
  
  int                   status = OK,i=0;
  char                  *gtifile, comm[32];
  BOOL                  first_file=1, first_time=1;
  double                sens=0.00001;
  GTIInfo_t             agti, bgti, gti;

  gtifile = strtok(global.par.gtifile, " \n\t;:");
  
  while(gtifile != NULL)
    {
      if(first_file)
	{
	  if(HDgti_read(gtifile, &gti, 0, 0, 0, 0, 0, &status))
	    {
	      fits_get_errstatus(status, comm);
	      headas_chat(CHATTY,"%s: Error: %s\n", global.taskname, comm); 
	      headas_chat(NORMAL,"%s: Error: Unable to read\n", global.taskname);
	      headas_chat(NORMAL,"%s: %s file.\n",global.taskname, gtifile);
	      goto Error;
	    }
	  else
	    first_file=0;
	}
      else
	{
	  if(HDgti_copy(&agti, &gti, &status))
	    {
	      fits_get_errstatus(status, comm);
	      headas_chat(CHATTY,"%s: Error: %s\n", global.taskname, comm); 
	      headas_chat(CHATTY,"%s: Error: Unable to copy GTI range.\n", global.taskname);
	      goto Error;

	    }
	  
	  if(HDgti_free(&gti))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to free allocated memory.\n", global.taskname);
	      goto Error;
	    }
	  
	  if(HDgti_read(gtifile, &bgti, 0, 0, 0, 0, 0, &status))
	    {
	      fits_get_errstatus(status, comm);
	      headas_chat(CHATTY,"%s: Error: %s\n", global.taskname, comm); 
	      headas_chat(NORMAL,"%s: Error: Unable to read\n", global.taskname);
	      headas_chat(NORMAL,"%s: %s file.\n",global.taskname, gtifile);
	      goto Error;
	    }

	  if(HDgti_merge(GTI_AND, &gti, &agti, &bgti, &status))
	    
	    {
	      fits_get_errstatus(status, comm);
	      headas_chat(CHATTY,"%s: Error: %s\n", global.taskname, comm); 
	      headas_chat(NORMAL,"%s: Error: Unable to merge GTI ranges.\n", global.taskname);
	      goto Error;
	    }
	
	  if(HDgti_free(&agti))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to free allocated memory.\n", global.taskname);
	      goto Error;
	    }     
	  if(HDgti_free(&bgti))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to free allocated memory.\n", global.taskname);
	      goto Error;
	    }


	}
      headas_chat(NORMAL, "%s: Info: '%s' file successfully read.\n", global.taskname, gtifile);

      gtifile += strlen(gtifile)+1;
      gtifile = strtok(gtifile, " \n\t;:");
      
    }
      /* fill gti array */

  if(gti.ngti)
    {
      first_time=1;
      global.gti_rows=gti.ngti;
      global.gtivalues =(GtiCol_t *)calloc(global.gti_rows, sizeof(GtiCol_t));
      headas_chat(CHATTY,"%s: Info: GTI ranges are:\n", global.taskname);
      for(i = 0; i < global.gti_rows; i++)
	{
	  global.gtivalues[i].start= gti.start[i]; /* Read start column */
	  global.gtivalues[i].stop = gti.stop[i];/* Read stop column */
	  headas_chat(CHATTY,"%s: Info: START = %15.6f  STOP = %15.6f\n",global.taskname,global.gtivalues[i].start,global.gtivalues[i].stop);

	  if  ((global.gtivalues[i].start < ( *mintstart + sens)) || first_time) 
	    *mintstart =  global.gtivalues[i].start; 
	  if  ((global.gtivalues[i].stop > ( *maxtstop - sens)) || first_time) 
	    *maxtstop =  global.gtivalues[i].stop;
	  
	  first_time=0;
	}        

    }
  else
    {
      global.gti_rows=gti.ngti;
      *mintstart=0.;
      *maxtstop=0.;
      headas_chat(NORMAL, "%s: Warning: GTI range is empty.\n", global.taskname);
    }
  
  return OK;

  
 Error:
  return NOT_OK;

} /* LoadGtiTable*/
/*
 *	xrtimage_work
 *
 *
 *	DESCRIPTION:
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           char * strtok(char *, char *);
 *           FitsFileUnit_t OpenReadFitsFile(char *name);
 *           int headas_chat(int , char *, ...);
 *           int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *           int CloseFitsFile(FitsFileUnit_t file);
 *           BOOL KeyWordMatch( FitsFileUnit_t unit, const char *KeyWord, ValueTag_t vtag, 
 *                              const void *NominalValue, void *ActValue);
 *           FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *           SPTYPE GetSVal(const FitsHeader_t *header, const char *KeyWord);
 *           char * strcpy(char *, char *);
 *           int fits_get_hdu_num(fitsfile *fptr, int *hdunum);
 *           int strcasecmp(const char *s1, const char *s2);
 *           char * SplitFilePath(const char *FilePath, char *DirName, char *BaseName);
 *           char * StripExtension(char * filename);
 *           char *strcat(char *dest, const char *src);
 *           BOOL FileExists(const char *FileName);
 *           FitsFileUnit_t OpenWriteFitsFile(char *name);
 *           int remove(const char *pathname);
 *           int fits_get_num_hdus(fitsfile *fptr, int *hdunum, int *status);
 *           int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype,
 *                               int *status ); 
 *           int fits_copy_hdu(fitsfile *infptr, fitsfile *outfptr, int morekeys, int *status); 
 *           int fits_update_key(fitsfile *fptr, int datatype, char *keyname, DTYPE *value,
 *                               char *comment, int *status);  
 *           int AddEventBintable(FitsFileUnit_t in, FitsFileUnit_t out);
 *           int HDpar_stamp(fitsfile *fptr, int hdunum)
 *           int ChecksumCalc(FitsFileUnit_t unit);
 * 
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 10/03/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrtimage_work()
{

  int                   status = OK, inExt=0, outExt=0, checkext=0,writext = 0, logical, gtiExt=0, type=0;   
  char                  extname[25];
  BOOL                  first_bp=0, build_bp=0, read_gti=1, slew=0;
  double                mingtitstart, maxgtitstop, bias_db=0.,sens=0.0000001;
  FitsCard_t            *card;
  FitsHeader_t          head, badhead;
  FitsFileUnit_t        inunit=NULL,  outunit=NULL, badunit=NULL;     /* Input and Output fits file pointers */
  static BadPixel_t     *bp_table_amp1[CCD_PIXS][CCD_ROWS],  *bp_table_amp2[CCD_PIXS][CCD_ROWS];
  BOOL                  checkgti=TRUE, nogti=FALSE;


  global.min_tstart = 0.;
  global.max_tstop = 0.;
  mingtitstart=0.;
  maxgtitstop=0.;
  global.oldsoft=0;
  global.hkrow=0;
  global.caldbrows=0;

  TMEMSET0( &head, FitsHeader_t ); 
  TMEMSET0( &badhead, FitsHeader_t ); 

  if(!global.par.subbias && !global.par.cleanbp && !global.par.gtiscreen && !global.par.cleansrc)
    {
      headas_chat(NORMAL,"%s: Warning: the input parameters:\n", global.taskname); 
      headas_chat(NORMAL,"%s: Warning: %s, %s, %s and %s\n", global.taskname, PAR_SUBBIAS,  PAR_CLEANBP, PAR_GTISCREEN, PAR_CLEANSRC);
      headas_chat(NORMAL,"%s: Warning: are set to 'no'. Nothing to be done!\n", global.taskname);
      return OK;
    }

  GetGMTDateTime(global.date);
  /* Get input file name */

  if(xrtimage_checkinput())
    goto Error;
    

 
  /* Open readonly input event file */
  if ((inunit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
    
  /* Check readout mode */
  
  /* Check if it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL,inunit,global.par.infile,global.taskname);

  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.infile);

  /* Get number of hdus in input events file */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
      
    }
  head=RetrieveFitsHeader(inunit);
  
  /* Get start time in primary header*/
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    global.tstart=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }		
    
  /* Get stop time in primary header*/
  if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
    global.tstop=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }	
  

  if(global.par.subbias || global.par.cleanbp) 
    {
      if(ReadHK())
	{
	  headas_chat(MUTE,"%s: Error: Unable to calculate  bias value\n", global.taskname);
	  headas_chat(MUTE,"%s: Error: using %s file.\n", global.taskname, global.par.hdfile);
	  goto Error;
	  
	}

      if (!global.oldsoft && global.par.subbias)
	{
	  headas_chat(NORMAL, "%s: Warning: '%s' file is in new format\n", global.taskname, global.par.hdfile);
	  headas_chat(NORMAL, "%s: Warning: but %s input parameter value is %f\n", global.taskname, PAR_BIAS, global.par.bias);
	  headas_chat(NORMAL, "%s: Warning: in this case the %s parameter\n", global.taskname, PAR_BIAS);
	  headas_chat(NORMAL, "%s: Warning: will be ignored.\n", global.taskname);
	}
     
    }



  if(global.par.gtiscreen)
    {

      /* Read GTI file */
      if (read_gti) 
	{
	  if (LoadGtiTable(&mingtitstart,&maxgtitstop))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to read GTI files.\n", global.taskname);
	      goto Error;
	    } 
	  else
	    read_gti=0;
	}
      
      if(global.gti_rows)
	if ((global.tstart > (maxgtitstop - sens) ) || (global.tstop < (mingtitstart + sens)) ) 
	  {
	    headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname); 
	    headas_chat(NORMAL, "%s: Error: %s file\n", global.taskname,global.par.infile); 
	    headas_chat(NORMAL, "%s: Error: are not included in the GTIs.\n", global.taskname);
	    headas_chat(CHATTY, "%s: Error: %s file range is:\n", global.taskname,global.par.infile); 
	    headas_chat(CHATTY, "%s: Error: TSTART = %f and TSTOP = %f\n",global.taskname,global.tstart,global.tstop);
	    headas_chat(CHATTY, "%s: Error: GTI range is:\n", global.taskname); 
	    headas_chat(CHATTY, "%s: Error: TSTART = %f and TSTOP = %f)\n",global.taskname, mingtitstart,maxgtitstop);
	    goto Error;
	    
	  }
      
      
      
      
      /* Check if GTI ext exists in input file*/
      if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_GTI, 0, &status))
	{
	  if (status == BAD_HDU_NUM)
	    {
	      
	      nogti=TRUE;
	      headas_chat(CHATTY,"%s: Info: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_GTI);
	      headas_chat(CHATTY,"%s: Info: '%s' file.\n",global.taskname, global.par.infile);
	      headas_chat(CHATTY,"%s: Info: The task will create and fill it.\n",global.taskname);
	  
	      status=0;
	    }
	  else
	    {
	      headas_chat(CHATTY,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_GTI);
	      headas_chat(CHATTY,"%s: : '%s' file.\n",global.taskname, global.par.infile);
	      goto Error;
	    }
	}
      else
	{
	  /* Get GTI ext number */
	  if (!fits_get_hdu_num(inunit, &gtiExt))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_GTI);
	      headas_chat(NORMAL,"%s: : '%s' file.\n",global.taskname, global.par.infile);
	      goto Error;
	    }
	}
  
    }
      
  if(global.par.cleansrc)
    {
      if(GetCalSourcesInfo())
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get Calibration sources information\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: but 'cleansrc' set to 'yes'.\n", global.taskname);
	  goto Error;
	}
      
    }

  if(!global.par.gtiscreen || global.gti_rows)
    {

        
      /* Create output file */
      if ((outunit = OpenWriteFitsFile(global.tmpfile)) <= (FitsFileUnit_t )0)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' temporary file. \n", global.taskname, global.tmpfile);
	  goto Error;
	}
      
      
      if(global.createbp)
	{  
	  
	  /* Build  bad pixels primary header */
	  if ((badunit=OpenWriteFitsFile(global.par.outbpfile)) <= (FitsFileUnit_t )0)
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
	      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, global.par.outbpfile);
	      goto Error;
	      
	    }
	  
	  badhead=NewPrimaryHeader(I, 0, 0, NULL, 1);
	  FinishPrimaryHeader(badunit, &badhead);
	  
	  if(WriteBpKey(inunit, badunit, global.par.outbpfile))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to write keywords\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n;", global.taskname, global.par.outbpfile);
	      goto Error;
	    }
	  build_bp=1;
	}
      

    
      /* If cleanbp set to yes and bad pixels or on-board table */
      /* set to CALDB, get DATE-OBS and TIME-OBS to query CALDB */
      if(global.par.cleanbp && (!strcmp(global.par.bpfile, DF_CALDB) || !strcmp(global.par.bptable, DF_CALDB)))
	{
	  head=RetrieveFitsHeader(inunit);
	  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
	    {
	      global.dateobs=card->u.SVal;
	      if(!(strchr(global.dateobs, 'T')))
		{
		  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
		    global.timeobs=card->u.SVal;
		  else
		    {
		      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
		      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
		      goto Error;
		      
		    }
		}    
	    }
	  
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      goto Error;
	      
	    }
	}
      /* Create temporary bad pixels map */
      if ((!first_bp) && global.par.cleanbp)
	{
	  /* Put bad pixels info into array */
	  if (CreateBPMap(bp_table_amp1, bp_table_amp2))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to create bad pixels list.\n", global.taskname);
	      goto Error;
	    }
	  first_bp=1;
	}
      
      outExt=1;
      /* Process all image extensions */
      while(outExt <= inExt && status == OK)
	{
	  if(fits_movabs_hdu( inunit, outExt, &type, &status ))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	      goto Error;
	    }
	  
	  if((outExt == 1 && inExt > 1) || type != IMAGE_HDU)
	    {
	      /* Copy primary empty hdu */
	      if(fits_copy_hdu(inunit, outunit, 0, &status))
		{ 
		  headas_chat(CHATTY,"%s: Error: Unable to copy Primary HDU\n",global.taskname);
		  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
		  headas_chat(CHATTY,"%s: Error: to %s temporary file.\n",global.taskname, global.tmpfile);
		  goto Error; 
		} 
	      head=RetrieveFitsHeader(inunit);
	      /* Check if primary is empty */
	      if(outExt == 1 && inExt > 1)
		if (ExistsKeyWord(&head, KWNM_NAXIS1, NULLNULLCARD) || ExistsKeyWord(&head, KWNM_NAXIS2, NULLNULLCARD))
		  {
		    headas_chat(NORMAL, "%s: Error: Primary HDU contains data.\n", global.taskname);
		    goto Error;
		  }
	    }
	  else
	    {	
	      
	      /* Get Image dimension from header */
	      head=RetrieveFitsHeader(inunit);    
	      slew=0;
	      if((ExistsKeyWord(&head, KWNM_OBS_MODE  , &card)))
		{
		  if(!strcmp (card->u.SVal, KWVL_OBS_MODE_S) || !strcmp (card->u.SVal, KWVL_OBS_MODE_ST))
		    {
		      slew=1;
		      headas_chat(NORMAL, "%s: Info: %s keyword in %d HDU set to %s\n", global.taskname,KWNM_OBS_MODE, (outExt-1),card->u.SVal);
		    }
		}
	      else
		{
		  headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE);
		  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
		}



	      checkgti=TRUE;
	      if(global.par.gtiscreen)
		{
		  if( CheckGti(head, &checkgti)) 
		    {
		      headas_chat(NORMAL, "%s: Error: Unable to obtain GTIs.\n", global.taskname);
		      goto Error;
		    }

		}

	      if (checkgti && !slew)  {
		writext++;
		if ( global.par.gtiscreen ) 
		  headas_chat(NORMAL, "%s: Info: Image in %d HDU is included in GTIs.\n", global.taskname, (outExt-1));
		/* Copy primary empty hdu */
		if(fits_copy_hdu(inunit, outunit, 0, &status))
		  {
		    headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n", global.taskname, (outExt-1));
		    headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
		    headas_chat(CHATTY,"%s: Error: to %s temporary file.\n",global.taskname, global.tmpfile);
		    goto Error;
		  }
		if(global.par.subbias)
		  {
		    /* Check if PHA is already corrected */
		    if(ExistsKeyWord(&head, KWNM_XRTPHACO, &card))
		      {
			if ( card->u.LVal )
			  {
			    headas_chat(NORMAL, "%s: Error: PHA is already corrected.\n", global.taskname);
			    goto Error;
			  }
			
		      }
		  }
		if(global.par.cleanbp)
		  {
		    if(ExistsKeyWord(&head, KWNM_XRTBPCNT, &card))
		      global.bpcount=card->u.IVal;
		    else
		      global.bpcount=0;
		  }
		
		/* Check readout mode of input events file */
		if(!(KeyWordMatch(inunit, KWNM_DATAMODE, S, KWVL_DATAMODE_IM, NULL)) && !(KeyWordMatch(inunit, KWNM_DATAMODE, S, KWVL_DATAMODE_IMS, NULL)) && !(KeyWordMatch(inunit, KWNM_DATAMODE, S, KWVL_DATAMODE_IML, NULL)))
		  {
		    headas_chat(NORMAL,"%s: Error: This task does not process the readout mode\n", global.taskname);
		    headas_chat(NORMAL,"%s: Error: of the %s file.\n", global.taskname, global.par.infile);
		    headas_chat(CHATTY,"%s: Error: Valid readout mode are: %s, %s and %s.\n", global.taskname, KWVL_DATAMODE_IM,KWVL_DATAMODE_IMS, KWVL_DATAMODE_IML );
		    if( CloseFitsFile(inunit))
		      {
			headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
			headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
			
		      } 
		    goto Error;
		    
		  }
	    
	    
		headas_chat(NORMAL, "%s: Info: Processing the HDU number %d\n", global.taskname,(outExt - 1) );
		/* Process Images */
		if (ProcImage(inunit, outunit, &bias_db, bp_table_amp1, bp_table_amp2, outExt,&checkext))
		  {
		    headas_chat(MUTE, "%s: Error: Unable to process %d HDU\n", global.taskname, (outExt - 1));
		    headas_chat(MUTE, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
		    goto Error;
		    
		  }
		       
		if(global.par.subbias)
		  {
		    /* Set XRTPHACO keyword to true */
		    logical=TRUE;
		    if(fits_update_key(outunit, TLOGICAL, KWNM_XRTPHACO, &logical, CARD_COMM_XRTPHACO, &status))
		      {
			headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTPHACO);
			headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
			goto Error;
		      }	      
		  
		
		    /* Add or update BIAS_VAL keyword */
		    if(fits_update_key(outunit, TDOUBLE, KWNM_BIASVAL, &bias_db, CARD_COMM_BIASVAL, &status))
		      {
			headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_BIASVAL);
			headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
			goto Error;
		      }
		  }
	      
		if(global.par.cleanbp)
		  {
		    /* Add or update XRTBPCNT keyword */
		    if(fits_update_key(outunit, TINT, KWNM_XRTBPCNT, &global.bpcount,CARD_COMM_XRTBPCNT, &status))
		      {
			headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTBPCNT);
			headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
			goto Error;
		      }
		

		    /* FT no more needed, the pure burned spots are erased (if needed ) from 
		       table in the CreateBPMap function */

/*  		    if((global.par.maxtemp > 0.0 - sens && global.par.maxtemp < 0.0 + sens)&&first_cut) */
/*  		      { */
/*  			CutBPFromTable(bp_table_amp1, (EV_BURNED_BP|EV_CAL_BP)); */
/*  			CutBPFromTable(bp_table_amp2, (EV_BURNED_BP|EV_CAL_BP)); */
/*  			first_cut=0; */
/*  		      } */


		  }
		/* Write history */
		if(global.hist)
		  {
		    
		    if(WriteHistBlock(outunit))
		      {
			headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
			goto Error;
		      }
		  }/* If (global.hist)*/
		
		if(build_bp)
		  {
		    /* Get extension name from input event file */
		    if (ExistsKeyWord(&head, KWNM_EXTNAME, &card))
		      sprintf(extname,"%s_BP", card->u.SVal);
		    else
		      strcpy(extname, "IMAGE_BP");
		    /* Build  BADPIX extension with data and append it to file */
		    if (BadpixBintable(badunit, bp_table_amp1, bp_table_amp2, extname))
		      {
			headas_chat(NORMAL, "%s: Error: Unable to create", global.taskname);
			headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outbpfile );
			goto Error;
			
		      }
		    
		    if (WriteBpKey(inunit, badunit, global.par.outbpfile))
		      {
			headas_chat(CHATTY, "%s: Error: Unable to add keywords", global.taskname);
			headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.outbpfile );
			goto Error;
			
		      }
		    
		    /* Deleted coordinates of saturated pixels from bad pixels map */
		    
		    CutBPFromTable(bp_table_amp1, BAD_BP);
		    CutBPFromTable(bp_table_amp2, BAD_BP);
		  }
		
	      } /* if (checkgti) */
	      else 
		{
		  if(slew)
		    headas_chat(NORMAL, "%s: Info: Image contained in %d HDU is in SLEW or SETTLING.\n",global.taskname, ( outExt-1));
		  else
		    headas_chat(NORMAL, "%s: Info: Image contained in %d HDU is not included in GTIs.\n",global.taskname, ( outExt-1));
		}
	    }
	  outExt++;
	}
    }

  /* Check number of extension*/
  if(writext == 0)
    {
      if(!slew)
	headas_chat(MUTE, "%s: Warning: All images are out of the GTIs range\n", global.taskname);
      else
	headas_chat(MUTE, "%s: Warning: All images are rejected\n", global.taskname);
      
      headas_chat(MUTE, "%s: Warning: no output file will be created.\n", global.taskname);
      if (FileExists(global.tmpfile))
	remove (global.tmpfile);
      if (FileExists(global.par.outbpfile))
	remove (global.par.outbpfile);
      return OK;
    } 
  else
    {
      if (writext == 1)
	headas_chat(NORMAL, "%s: Info: The output Image file includes %d image.\n",global.taskname, writext);
      else
	headas_chat(NORMAL, "%s: Info: The output Image file includes %d images.\n",global.taskname, writext);
    }
  if (global.par.gtiscreen) {
    
    /* Update header output file - tstart - tstop */
    if(fits_movabs_hdu( outunit, 1, NULL, &status ))
      { 
	headas_chat(CHATTY, "%s: Error: Unable to move in Primary HDU\n", global.taskname);
	headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	goto Error;
	 
      }
    if(fits_update_key(outunit, TDOUBLE, KWNM_TSTART, &global.min_tstart, CARD_COMM_TSTART, &status))
      { 
	headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_TSTART);
	headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	goto Error;
      }
    if(fits_update_key(outunit, TDOUBLE, KWNM_TSTOP, &global.max_tstop, CARD_COMM_TSTOP, &status))
      {
	headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_TSTOP);
	headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	goto Error;
      }
  }   
    
  if(global.par.gtiscreen)
    {
      /* Add or Update GTI extension in output file */

      if(nogti)
	{
	
	  if(WriteGTIExt( inunit, outunit, global.gti_rows,global.gtivalues ))
	    {
	      headas_chat(NORMAL, " %s: Error: Unable to add %s extension\n", global.taskname, KWVL_EXTNAME_GTI);
	      headas_chat(NORMAL, " %s: Error: in %s temporary file.\n", global.taskname, global.tmpfile);
	      goto Error;
	      
	    }
	}
      else
	{
	  if(UpdateGTIExt(outunit, global.gti_rows,global.gtivalues ))
	  {
	      headas_chat(NORMAL, " %s: Error: Unable to update %s extension\n", global.taskname, KWVL_EXTNAME_GTI);
	      headas_chat(NORMAL, " %s: Error: in %s temporary file.\n", global.taskname, global.tmpfile);
	      goto Error;
	  }
	}
    }



  /* Add checksum and datasum in all extensions */
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, global.par.outfile);
      goto Error;
    }
  
  /* close files */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname,global.par.infile);
      goto Error;
    }  

  /* close files */
  if(build_bp)
    {
      if (CloseFitsFile(badunit))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname,global.par.outbpfile);
	  goto Error;
	}  
    }

  if (!CloseFitsFile(outunit)) 
    {
      headas_chat(NORMAL,"%s: Info: File '%s' successfully written.\n",global.taskname,
		global.par.outfile);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname,global.par.outfile);
      goto Error;
    }
  /* rename temporary file into output event file */
  if (rename (global.tmpfile,global.par.outfile) == -1)
    {
      headas_chat(NORMAL, "%s: Error: Unable to rename temporary file. \n", global.taskname);
      goto Error;
    }
    
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  
  if(global.caldbrows)
    free(global.biasvalues);

  if(global.hkrow)
    free(global.hkinfo);

  return OK;

 Error:
  return NOT_OK;

} /* xrtimage_work */
/*
 *	xrtimage
 *
 *
 *	DESCRIPTION:
 *        
 *                Routine to assign grades to faint mode data
 * 		  Based on the CLASSIFY program
 *
 *	DOCUMENTATION:
 *
 *
 *      FUNCTION CALL:
 *             void set_toolversion(char *);
 *             void set_toolname(char *);   
 *             void get_toolname(char *);
 *             void get_toolnamev(char *);
 *             int headas_chat(int ,char *, ...);
 *
 *             void xrtimage_getpar(void);
 *             void xrtimage_info(void); 
 * 	       void xrtimage_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtimage()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTIMAGE_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);

  /* Get parameter values */ 
  if ( xrtimage_getpar() == OK ) {
    
    if ( xrtimage_work() ) {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto image_end;
    }
  }
  else
    goto image_end;
  return OK;

 image_end:

  if (FileExists(global.tmpfile))
    remove (global.tmpfile);

  if (FileExists(global.par.outbpfile))
    remove (global.par.outbpfile);

  return NOT_OK;
} /* xrtimage */
/*
 *	xrtimage_info: 
 *
 *
 *	DESCRIPTION:
 *         Display information about input parameter.
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *        0.1.1: - RP 27/8/2003 - Added screen,gtifile parameter 
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
void xrtimage_info(void)
{
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE," \t\tRunning '%s'\n",global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Image file                         :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Image file                        :'%s'\n",global.par.outfile);
  if(global.par.subbias||global.par.cleanbp)
    headas_chat(NORMAL,"Name of the input Header Packet Housekeeping file    :'%s'\n",global.par.hdfile);
  if (global.par.subbias)
    {
      headas_chat(CHATTY,"Correct PHA value?                                   : yes\n");
      if(global.par.bias < 0.)
	{
	  headas_chat(CHATTY,"Bias value                                           :'%f'\n",global.par.bias);
	  headas_chat(NORMAL,"Name of the input BIAS file                          :'%s'\n",global.par.biasfile);
	}
      else
	headas_chat(NORMAL,"Bias value                                           :'%f'\n",global.par.bias);
    }
  else
    headas_chat(CHATTY,"Correct PHA value?                                   : no\n");

  if (global.par.cleanbp)
    {
      headas_chat(NORMAL,"Bias Difference                                      :'%d'\n",global.par.biasdiff);
      headas_chat(CHATTY,"Clean Image from bad pixels?                         : yes\n");
      headas_chat(NORMAL,"Name of the input CALDB bad pixels file              :'%s'\n",global.par.bpfile);
      headas_chat(NORMAL,"Name of the input user  bad pixels file              :'%s'\n",global.par.userbpfile);
      headas_chat(NORMAL,"Name of the input on-board bad pixels table          :'%s'\n",global.par.bptable);
      headas_chat(NORMAL,"Name of the output bad pixels file                   :'%s'\n",global.par.outbpfile);
      headas_chat(NORMAL,"Temperature threshold to flag burned spot            :'%f'\n",global.par.maxtemp);
    }
  else
    headas_chat(CHATTY,"Clean Image from bad pixels?                         : no\n");

  
  if(global.par.gtiscreen)
    {
      headas_chat(NORMAL,"Name of the input GTIs file                          :'%s'\n",global.par.gtifile);
    }
  if (global.par.gtiscreen)
    headas_chat(CHATTY,"Screen for GTIs?                                     : yes\n");
  else 
    headas_chat(CHATTY,"Screen for GTIs?                                     : no\n");
  if (global.par.cleansrc)
    {
      headas_chat(CHATTY,"Clean Calibration sources?                           : yes\n");
      headas_chat(NORMAL,"Name of the input CALDB calibration sources file     :'%s'\n",global.par.srcfile);
    }
  else 
    headas_chat(CHATTY,"Clean Calibration sources?                           : no\n");
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords?                              : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords?                              : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output file?                               : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output file?                               : no\n");
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrtimage_info */


int InitImagingVars(  FitsHeader_t   head )
{

  FitsCard_t *card;

  /* Get Naxis1 */
  if(ExistsKeyWord(&head, KWNM_NAXIS1, &card))
    global.im.naxis1 =card->u.JVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_NAXIS1);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }


/* Get Naxis1 */
  if(ExistsKeyWord(&head, KWNM_NAXIS2, &card))
    global.im.naxis2 =card->u.JVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_NAXIS2);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* global.im.naxis1=GetJVal(&head, KWNM_NAXIS1);


     global.im.naxis2=GetJVal(&head, KWNM_NAXIS2);*/
  global.im.xmax = global.im.naxis1 - 1;
  global.im.ymax = global.im.naxis2 - 1;
  global.im.xmin = 0;
  global.im.ymin = 0;


  global.im.type = GetCoordinateType( head );
  if ( global.im.type == -1 ) 
    return NOT_OK;
  else if ( global.im.type == SKY || global.im.type == DET ) {
    headas_chat(NORMAL,"%s: Error: This task does not process Image with %s coordinate system.\n", global.taskname, FitsImageTypeString[global.im.type]);
    headas_chat(CHATTY,"%s: Error: Valid coordinate system is: RAW.\n", global.taskname);
    return NOT_OK;
  }
  
  
  if ( ExistsKeyWord(&head,KWNM_BLANK, &card) ){
	global.im.nullval = card->u.JVal;
  }
  else {
    global.im.nullval = FitsImageNullvalDef[global.im.type];
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_BLANK);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
  }

  if ( ExistsKeyWord(&head,KWNM_CRVAL1, &card) ){
    DValCust( card, &global.im.refvalx);
  }
  else {
    global.im.refvalx = FitsImageRefvalxDef[global.im.type];
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRVAL1);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
  }

  if ( ExistsKeyWord(&head,KWNM_CRVAL2, &card) ){
    DValCust( card, &global.im.refvaly);
  }
  else {
    global.im.refvaly = FitsImageRefvalyDef[global.im.type];
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRVAL2);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
  }

  if ( ExistsKeyWord(&head,KWNM_CRPIX1, &card) ){
    DValCust( card, &global.im.refx);
  }
  else {
    global.im.refx = FitsImageRefxDef[global.im.type];
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRPIX1);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
  }

  if ( ExistsKeyWord(&head,KWNM_CRPIX2, &card) ){
    DValCust( card, &global.im.refy);
  }
  else {
    global.im.refy = FitsImageRefyDef[global.im.type];
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRPIX2);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
  }

  if ( ExistsKeyWord(&head,KWNM_CDELT1, &card) ){
    DValCust( card, &global.im.pixdeltx);
  }
  else {
    global.im.pixdeltx = FitsImagePixdeltxDef[global.im.type];
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CDELT1);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
  }

  if(!global.im.pixdeltx)
    {
      headas_chat(NORMAL, "%s: Error: %s keyword is unset.\n",global.taskname,  KWNM_CDELT1);
      goto Error;
    }
      
  if ( ExistsKeyWord(&head,KWNM_CDELT2, &card) ){
    DValCust( card, &global.im.pixdelty);
  }
  else {
    global.im.pixdelty = FitsImagePixdeltyDef[global.im.type];
    headas_chat(NORMAL, "%s: Error: %s keyword is unset.\n",global.taskname, KWNM_CDELT2);
  }

  if(!global.im.pixdelty)
    {

      headas_chat(NORMAL, "%s: Error: %s keyword is unset.\n", global.taskname, KWNM_CDELT2);
      goto Error;
    }

  headas_chat(CHATTY,"%s: Info: Processing Image with %s Coordinate system\n", global.taskname, FitsImageTypeString[global.im.type]);
  headas_chat(CHATTY,"%s: Info: Reference Pixel are     : %f %f\n",global.taskname, global.im.refx,global.im.refy);
  headas_chat(CHATTY,"%s: Info: Reference Value are     : %f %f\n",global.taskname, global.im.refvalx,global.im.refvaly);
  headas_chat(CHATTY,"%s: Info: Delta Pixel values are  : %f %f\n",global.taskname, global.im.pixdeltx,global.im.pixdelty);

  return OK;

 Error:
  return NOT_OK;

}


int DValCust( FitsCard_t *card, double *cardval) {

  switch(card->ValueTag) {
  case J: /* integer */
    *cardval = ( double)card->u.JVal;
    break;
  case E:
  case F:	/* float */
    *cardval = ( double)card->u.EVal;
    break;
  case D:
  case G: /* double */
    *cardval = ( double)card->u.DVal;
    break;
  default:
    Error("InsertCard", "Unkown tag field detected");
    return NOT_OK;
  }
  return OK;
}

int JValCust( FitsCard_t *card, int *cardval) {
  switch(card->ValueTag) {
  case J:    /* integer */
    *cardval = ( int)card->u.JVal;
    break;
  case E:
  case F:    /* float */
    *cardval = ( int)card->u.EVal;
    headas_chat(NORMAL,"%s: Warning: The value %f of the %s keyword was trasformed into the integer value %d\n",global.taskname, card->u.EVal, card->KeyWord, *cardval);
    break;
  case D: 
 case G:    /* double */
    *cardval = ( int)card->u.DVal;
    headas_chat(NORMAL,"%s: Warning: The value %f of the %s keyword was trasformed into the integer value %d\n",global.taskname, card->u.DVal, card->KeyWord, *cardval);
    break;
  default:
    Error("InsertCard", "Unkown tag field detected");
    return NOT_OK;
  }
  return OK;
}

int GetCoordinateType ( FitsHeader_t   head ) {

  int        type, typex= -1, typey = -1;
  FitsCard_t *card;


  if ( ExistsKeyWord(&head,KWNM_CTYPE1, &card) ){
    if ( !strcmp( card->u.SVal, KWVL_CTYPE1_RAW ) ) typex = RAW;
    if ( !strcmp( card->u.SVal, KWVL_CTYPE1_DET ) ) typex = DET;
    if ( !strcmp( card->u.SVal, KWVL_CTYPE1_SKY ) ) typex = SKY;
  }
  else {

    headas_chat(NORMAL,"%s: Warning: %s keyword not found\n", global.taskname, KWNM_CTYPE1);
    headas_chat(NORMAL,"%s: Warning: in %s file.\n", global.taskname, global.par.infile);
    headas_chat(NORMAL,"%s: Info: Using RAW coordinate system.\n",global.taskname);
    typex = RAW;
  }

  if ( ExistsKeyWord(&head,KWNM_CTYPE2, &card) ){
    if ( !strcmp( card->u.SVal, KWVL_CTYPE2_RAW ) ) typey = RAW;
    if ( !strcmp( card->u.SVal, KWVL_CTYPE2_DET ) ) typey = DET;
    if ( !strcmp( card->u.SVal, KWVL_CTYPE2_SKY ) ) typey = SKY;
  }
  else {
    headas_chat(NORMAL,"%s: Warning: %s keyword not found\n", global.taskname, KWNM_CTYPE2);
    headas_chat(NORMAL,"%s: Warning: in %s file.\n", global.taskname, global.par.infile);
    headas_chat(NORMAL,"%s: Info: Using RAW coordinate system.\n",global.taskname);
    typey = RAW;
  }

  if ( typex == typey ) {
    type = typex;
  }
  else {
    headas_chat(CHATTY,"%s: Error: '%s' and '%s' keywords are not consistent\n",global.taskname,KWNM_CTYPE1,KWNM_CTYPE2);
    return -1;
  }

  return type;
}

int CreateBPMap(BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS])
{

  long   extno;
  char   expr[256];
  double sens=0.00001;

  /********************************
   *     Get CALDB bad pixels     *
   ********************************/
  
  if (strcasecmp(global.par.bpfile,DF_NONE) )
    {
      if ( !strcasecmp(global.par.bpfile,DF_CALDB) )
	{
	  sprintf(expr, "datamode.eq.shortima.and.type.eq.%s",KWVL_BADPIX_TYPE);
	  if (CalGetFileName( HD_MAXRET, global.dateobs, global.timeobs, DF_NOW, DF_NOW, KWVL_BADPIX_DSET,global.par.bpfile, expr, &extno ))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	      goto create_end;
	    }
	  extno++;
	}
      else
	{
	  if(CalGetExtNumber(global.par.bpfile, KWVL_EXTNAME_IMBAD, &extno))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_IMBAD);
	      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.bpfile);
	      goto create_end;
	    }
	}
      
      if(extno > 0)
	{
	  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.bpfile);
	  if (GetBadPix(global.par.bpfile, bp_table_amp1, bp_table_amp2, CBP, extno,global.tstart)) 
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to process\n", global.taskname);
 	      headas_chat(NORMAL, "%s: Error: %s bad pixels file.\n", global.taskname,global.par.bpfile );
	      goto create_end;
	    }
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: Unable to process\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s bad pixels file.\n", global.taskname,global.par.bpfile );
	  goto create_end;
	}
    }
  
  /********************************
   * Get user defined  bad pixels *
   ********************************/
  if (strcasecmp(global.par.userbpfile,DF_NONE))
    {
      if(CalGetExtNumber(global.par.userbpfile, KWVL_EXTNAME_IMBAD, &extno))
	{
	  headas_chat(NORMAL,"%s: Warning: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_IMBAD);
	  headas_chat(NORMAL,"%s: Warning: in '%s' file.\n",global.taskname, global.par.userbpfile);
	  headas_chat(NORMAL,"%s: Warning: Checking %s extension.\n",global.taskname,KWVL_EXTNAME_BAD);
	  
	  if(CalGetExtNumber(global.par.userbpfile, KWVL_EXTNAME_BAD, &extno))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
	      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.userbpfile);
	      goto create_end;
	    }
	}
    
      headas_chat(NORMAL, "%s: Info: Processing %d HDU", global.taskname, extno);
      headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.userbpfile);
      if (GetBadPix(global.par.userbpfile, bp_table_amp1, bp_table_amp2, UBP,extno, global.tstart)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to process\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s bad pixels file.\n", global.taskname,global.par.userbpfile );
	  goto create_end;
	}
    }


  /************************************
   * Get on-board defined  bad pixels *
   ************************************/

  if (strcasecmp(global.par.bptable,DF_NONE))
    {
      if ( !strcasecmp(global.par.bptable,DF_CALDB) )
	{
	  sprintf(expr, "datamode.eq.shortima.and.type.eq.%s",KWVL_BPTABLE_TYPE);
	  if (CalGetFileName( HD_MAXRET, global.dateobs, global.timeobs, DF_NOW, DF_NOW, KWVL_BPTABLE_DSET,global.par.bptable, expr, &extno ))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	      goto create_end;
	    }
	  extno++;
	}
      else
	{
	  if(CalGetExtNumber(global.par.bptable, KWVL_EXTNAME_IMBAD, &extno))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_IMBAD);
	      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.bptable);
	      goto create_end;
	    }
	}
      
      if(extno > 0)
	{
	  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.bptable);
	  if (GetBadPix(global.par.bptable, bp_table_amp1, bp_table_amp2, OBP, extno, global.tstart)) 
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to process\n", global.taskname);
 	      headas_chat(NORMAL, "%s: Error: %s bad pixels file.\n", global.taskname,global.par.bptable);
	      goto create_end;
	    }  
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: Unable to process\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s bad pixels file.\n", global.taskname,global.par.bptable);
	  goto create_end;
	}
      
      
    }
  

  /*******************************************
   *     Reject Burned spots if maxtemp == 0 *
   ******************************************/

  if ( (global.par.maxtemp >= 0.0 - sens)&&(global.par.maxtemp <= 0.0 + sens) ) {

    int  i, j ;
    
    for (i=0; i<CCD_PIXS; i++)
      {
	for (j=0; j<CCD_ROWS; j++)
	  {
	    if ( bp_table_amp1[i][j] != NULL )
	      {
		if (bp_table_amp1[i][j]->bad_flag&EV_BURNED_BP)  
		  {
		    bp_table_amp1[i][j]->bad_flag = bp_table_amp1[i][j]->bad_flag&~EV_BURNED_BP;
		    if ( bp_table_amp1[i][j]->counter == 0 ) {
		      bp_table_amp1[i][j] = NULL;
		    }
		  }
	      }
	    if ( bp_table_amp2[i][j] != NULL )
	      {
		if (bp_table_amp2[i][j]->bad_flag&EV_BURNED_BP)  
		  {
		    bp_table_amp2[i][j]->bad_flag = bp_table_amp2[i][j]->bad_flag&~EV_BURNED_BP;
		    if ( bp_table_amp2[i][j]->counter == 0 ) {
		      bp_table_amp2[i][j] = NULL;
		    }
		  }
	      }
	  }
      }
  }

  return OK;
  
 create_end:
  return NOT_OK;
  
  
}/* CreateBPMap */
/*
 *	WriteBPKeyword:
 *
 *
 *	DESCRIPTION:
 *          Routine to copy keywords from events to bad pixels file
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             headas_chat(int ,char *, ...);
 *
 *             int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void AddCard(FitsHeader_t *header, const char *KeyWord, const ValueTag_t vtag, 
 *	
 *       CHANGE HISTORY:
 *        0.1.0: - BS 22/07/2003 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int WriteBpKey(FitsFileUnit_t evunit, FitsFileUnit_t badunit, char *outbpfile)
{
  char           crval[FLEN_VALUE], taskname[FLEN_FILENAME], strhist[256], date[25];
  FitsCard_t     *card;
  Version_t      swxrtdas_v;          /* SWXRTDAS version */
  FitsHeader_t   evhead, badhead;
  

  
  TMEMSET0( &evhead, FitsHeader_t );
  TMEMSET0( &badhead, FitsHeader_t );

  get_toolnamev(taskname);
  
  GetSWXRTDASVersion(swxrtdas_v);

  evhead = RetrieveFitsHeader(evunit);
  badhead = RetrieveFitsHeader(badunit);
   
 /* Add creator */
  sprintf(crval,"%s", taskname);
  AddCard(&badhead, KWNM_CREATOR, S, crval,CARD_COMM_CREATOR); 

  /* Copy keywords from events file to badpixels file */
  if (ExistsKeyWord(&evhead, KWNM_TELESCOP, &card))
    AddCard(&badhead, KWNM_TELESCOP, S, card->u.SVal,CARD_COMM_TELESCOP);

  if (ExistsKeyWord(&evhead, KWNM_INSTRUME, &card))
    AddCard(&badhead, KWNM_INSTRUME, S, card->u.SVal,CARD_COMM_INSTRUME);
  
  if (ExistsKeyWord(&evhead, KWNM_OBS_ID, &card))
    AddCard(&badhead, KWNM_OBS_ID, S, card->u.SVal,CARD_COMM_OBS_ID);

  if (ExistsKeyWord(&evhead, KWNM_ORIGIN, &card))
    AddCard(&badhead, KWNM_ORIGIN, S, card->u.SVal,CARD_COMM_ORIGIN);

  /* Add history */
  GetGMTDateTime(date);
  sprintf(strhist, "File created by %s (%s) at %s", taskname, swxrtdas_v,date );
  AddHistory(&badhead, strhist);
    
  
  if(WriteUpdatedHeader(badunit, &badhead))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.outbpfile);
      goto write_end;
    }
 

  return OK;
 write_end:
  return NOT_OK;

 
}/* WriteBpKey */


/* Round Number */
double round_num(double num)
{
int i;
double mult = 1;
double result = num;

for (i=0; i <= NUM_ROUND_POS; i++)
mult *= 10;

/* multiply the original number to 'move' the decimal point to the right */
result *= mult;

/* drop off everything past the decimal point */
result = floor(result);

/* 'move' the decimal point back to where it started */
result /= mult;

return result;
}

int ComputeBiasValue(FitsHeader_t   head, double * bias)
{

  int            i=0;
  BOOL           found=0;
  double         tstart,bias_db=0.0,sens=0.00001;

  FitsCard_t     *card;




  /* Get start time */
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    tstart=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found.\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto bias_end;
    }


  /* Compute bias value */ 
  if(global.oldsoft)
    {
     
      if(global.par.bias < 0.)
	{
	  if(!global.caldbrows)
	    {
	      if(GetBiasValues())
		{
		  headas_chat(MUTE,"%s: Error: Unable to calculate  bias value\n", global.taskname);
		  headas_chat(MUTE,"%s: Error: using %s file.\n", global.taskname, global.par.biasfile);
		  goto bias_end;
		  
		}

	    }
	  
	  if(global.caldbrows)
	    {
	      if(global.caldbrows == 1)
		{
		  found=1;
		  bias_db = global.biasvalues[0].bias;
		}
	      else 
		{
		  if (tstart <= (global.biasvalues[0].time + sens))
		    bias_db = global.biasvalues[0].bias;
		  else
		    {
		      /* Compute bias value */
		      
		      for (i=1; i < global.caldbrows; i++)
			{
			  if (global.biasvalues[i].time > (tstart - sens))
			    {
			      bias_db = global.biasvalues[i-1].bias;
			      found=1;
			      i=global.caldbrows;
			    }
			}/* End for(i=0; ....) */
		      
		    }/* else  interpolate bias values */
		}/* else compute bias value */
	      if(!found)
		bias_db = global.biasvalues[global.caldbrows -1].bias;
	    }
	}
      else 
	bias_db=global.par.bias;
    }
  else
    {
      
      
      for(i=0; i<global.hkrow && !found; i++)
	{
	  /*printf(" TSTART==%f HKTIME==%f\n", tstart,global.hkinfo[i].hktime);*/
	  if((tstart >= global.hkinfo[i].hktime - sens) && (tstart <= global.hkinfo[i].hktime + sens))
	    {
	      /*printf("IMBLVL==%d -- MEDIAN==%d\n", global.hkinfo[i].imblvl,global.hkinfo[i].median);*/
	      found=1;
	      if(global.hkinfo[i].imblvl)
		{
		  if(abs(global.hkinfo[i].median - global.hkinfo[i].imblvl) >= global.par.biasdiff)
		    {
		      bias_db=(double)(global.hkinfo[i].median - global.hkinfo[i].imblvl);
		      
		    }
		  else
		    bias_db=0.0;
		}
	      else
		bias_db=(double)global.hkinfo[i].median;
	    }
	}
	  
      if(!found)
	{
	  headas_chat(NORMAL, "%s: Error: Row containg HK information\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: for image taken at %f time\n", global.taskname, tstart);
	  headas_chat(NORMAL, "%s: Error: is not been found in %s file.\n", global.taskname, global.par.hdfile);
	  goto bias_end;
	  
	}
      
    }
    
    
  *bias = bias_db;

  return OK;
  
 bias_end:
  return NOT_OK;

}/* ComputeBiasValue */
int WriteHistBlock(FitsFileUnit_t outunit)
{
  int    status=OK;
  char   comm[256]=""; 


  sprintf(global.strhist, "File modified  by %s (%s) at %s", global.taskname, global.swxrtdas_v,global.date );
  fits_write_history(outunit, global.strhist, &status);
  fits_write_history(outunit, comm, &status);
  if(global.par.cleanbp &&(strcasecmp(global.par.bpfile,DF_NONE) || strcasecmp(global.par.userbpfile,DF_NONE) || strcasecmp(global.par.bptable,DF_NONE)))
    {
      sprintf(comm, "Used following file(s) to get bad pixels: ");
      if(fits_write_history(outunit, comm, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto histo_end;
	  
	}
      if(strcasecmp(global.par.bpfile,DF_NONE) )
	{
	  sprintf(comm, "%s CALDB bad pixels file", global.par.bpfile);
	  
	  if(fits_write_history(outunit, comm, &status))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	      goto histo_end;
	    }
		      
	  if(strcasecmp(global.par.userbpfile,DF_NONE) )
	    {
	      sprintf(comm, "%s user bad pixels file",global.par.userbpfile);
	      if(fits_write_history(outunit, comm, &status))
		{
		  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
		  goto histo_end;
		  
		}
		
	    }      
	  if(strcasecmp(global.par.bptable,DF_NONE) )
	    {
	      sprintf(comm, "%s on-board bad pixels table",global.par.bptable);
	      if(fits_write_history(outunit, comm, &status))
		{
		  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
		  goto histo_end;
		  
		}
	    }
	}
    }
  if(global.par.bias < 0)
    {
      sprintf(comm, "Used %s CALDB file to get bias value",global.par.biasfile);
      if(fits_write_history(outunit, comm, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto histo_end;
	  
	}
    }/* if(global.par.bias)*/
		


 /* Add history if parameter history set */
  if(HDpar_stamp(outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto histo_end;
    }

  
  return OK;
  
 histo_end:
  return NOT_OK;

}/* WriteHistory() */

int xrtimage_checkinput(void)
{
  char                  stem[10];
  pid_t                 tmp;


  global.createbp=0;
  
  if(FileExists(global.par.outfile))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outfile);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outfile);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set,\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: the '%s' file will be overwritten.\n", global.taskname, global.par.outfile);
	  if(remove (global.par.outfile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
	      goto check_end;
	    } 
	}
    }
	
   
  /* Derive temporary event filename */
 
  tmp=getpid(); 
  sprintf(stem, "%dtmp", (int)tmp);
  DeriveFileName(global.par.outfile, global.tmpfile, stem);
  
  /* Check if tmp file exists to remove it*/
  if(FileExists(global.tmpfile))
    if(remove (global.tmpfile) == -1)
      {
	headas_chat(CHATTY, "%s: Error: Unable to remove\n", global.taskname);
	headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.tmpfile);
	goto check_end;
      }
  
  
  /* Check if badpixels file shall be created */
  if ( strcasecmp(global.par.outbpfile,DF_NONE) && global.par.cleanbp) 
    { 
      /* If global.par.outbpfile == DEFAULT build filename */ 
      if (!(strcasecmp (global.par.outbpfile, "DEFAULT")))
	{
	  strcpy(global.par.outbpfile, global.par.outfile);
	  StripExtension(global.par.outbpfile);
	  strcat(global.par.outbpfile, BPFITSEXT);
	  headas_chat(NORMAL, "%s: Info: Name for the bad pixels file is:\n",global.taskname);
	  headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.outbpfile);
	  
	}
      
      if ( FileExists(global.par.outbpfile) ) {
	headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outbpfile);
	if ( !headas_clobpar ) {
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outbpfile);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	  
	}
	else
	  { 
	    headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set,\n", global.taskname);
	    headas_chat(NORMAL, "%s: Warning: the '%s' file will be overwritten\n", global.taskname, global.par.outbpfile);
	    if(remove (global.par.outbpfile) == -1)
	      {
		headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outbpfile);
		goto check_end;
	      }  
	      
	  }
      }
      global.createbp=1;
    }
      

  return OK;

 check_end:
  return NOT_OK;

}
/*
 *
 *	GetCalSourcesInfo
 *
 *
 *	DESCRIPTION:
 *                 Routine to get calibration sources information
 *    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *           int                strcasecmp(const char *, const char *);
 *           int                CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd,
 *                                             char *TimeEnd,const char *DataSet, char *CalFileName, char *expr);
 *           int                headas_chat(int , char *, ...);
 *           FitsFileUnit_t     OpenReadFitsFile(char *name);
 *           FitsHeader_t	RetrieveFitsHeader(FitsFileUnit_t unit); 
 *           int                fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, 
 *                                              int extver, int *status);
 *           void               GetBintableStructure(FitsHeader_t *header, Bintable_t *table, 
 *                                                   const int MaxBlockRows, const unsigned nColumns, 
 *                                                   const unsigned ActCols[]);
 *           int		GetColNameIndx(const Bintable_t *table, const char *ColName);
 *           void *             calloc(size_t nmemb, size_t size);
 *           int                ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, 
 *                                           const unsigned nColumns, const unsigned ActCols[], 
 *                                           const unsigned FromRow, unsigned *nRows);
 *           unsigned           ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *           int                CloseFitsFile(FitsFileUnit_t file);
 *
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 29/07/2004 - First version
 */
int GetCalSourcesInfo(void)
{
  long           extno;

 /* Derive CALDB calibration sources filename */ 
  if (!(strcasecmp (global.par.srcfile, DF_CALDB)))
    {
      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_REGION_DSET, global.par.srcfile, "type.eq.CALSOURCE", &extno))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto get_end;
	}
      extno++;
    }
  
  else
    {
      if(CalGetExtNumber(global.par.srcfile, KWVL_EXTNAME_REGIONCAL, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s extension\n", global.taskname, KWVL_EXTNAME_REGIONCAL);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.srcfile);
	  goto get_end;
	}
	 
    } 

  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.srcfile);

  if(ReadCalSourcesFile(global.par.srcfile, global.calsrc, (int)extno))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get Calibratione Sources Position.\n", global.taskname);
      goto get_end;
    }

  return OK;
  
 get_end:
   
  return NOT_OK;

}/* GetCalSourceInfo */

/*
 *
 *  ReadHK
 *
 *  DESCRIPTION:
 *    Routine to take information  hd file
 *
 *
 */
int ReadHK(void)
{  
  int                status=OK, tmpmode=0, pxct=0, *median, iii=0, tot_imblvl=0;
  unsigned           ActCols[7], jj=0, level=0, mod=0, ind=0;
  unsigned           FromRow, ReadRows, n, nCols;
  HKCol_t            hkcol;
  Bintable_t	     table;
  FitsCard_t         *card;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;
  
  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
      
  if ((unit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }
  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.hdfile);  
  /* Move in frame extension in input hk file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_FRM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_FRM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hdfile);
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
	}
      
      goto ReadHK_end;
	  
    }
  
  head=RetrieveFitsHeader(unit);
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    {
      
      /*printf(">>>>> tstart=%5.5f -- hkstart=%5.5f\n", global.tstart,(card->u.DVal+TIMEDELT));*/
      if(global.tstart < (card->u.DVal - TIMEDELT))
	{
	  headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file are not included in the range time\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: of the %s file.\n", global.taskname, global.par.hdfile);
	  goto ReadHK_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }
  
  if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
    {
      
      /*printf(">>>>> tstop=%5.5f -- hkstop=%5.5f\n", global.tstop,card->u.DVal);*/
      if(global.tstop > (card->u.DVal + TIMEDELT))
	{
	  headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file are not included in the range time\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: of the %s file.\n", global.taskname, global.par.hdfile);
	  goto ReadHK_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }
  
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }
  
  /* Get needed columns number from name */
  
  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }
  
  /* TIME */
  if ((hkcol.TIME=ColNameMatch(CLNM_TIME, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }
  
  if ((hkcol.HPIXCT=ColNameMatch(CLNM_HPIXCT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist.\n", global.taskname, CLNM_HPIXCT);
      headas_chat(NORMAL, "%s: Info: This is an old format of the\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: '%s' file\n", global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: The bias will be correct using CALDB file.\n", global.taskname);
      global.oldsoft=1;
      
    }
  
  if (((hkcol.BIASPX=ColNameMatch(CLNM_BiasPx, &table)) == -1) && !global.oldsoft)
    {
      /* Se non ci sono queste colonne il software di bordo e' vecchio? */
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_BiasPx);
      headas_chat(NORMAL, "%s: Info: This is an old format of the\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: '%s' file\n", global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: The bias will be correct using CALDB file.\n", global.taskname);
      global.oldsoft=1;
      
    }
  
  if (((hkcol.IMBLVL=ColNameMatch(CLNM_IMBLVL, &table)) == -1)&& !global.oldsoft )
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_IMBLVL);
      headas_chat(NORMAL, "%s: Info: This is an old format of the\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: '%s' file\n", global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: The bias will be correct using CALDB file.\n", global.taskname);
      global.oldsoft=1;
    }
  if ((hkcol.CCDTEMP=ColNameMatch(CLNM_CCDTemp, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDTemp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }
     
  EndBintableHeader(&head, &table);
  
  FromRow = 1;
  ReadRows=table.nBlockRows;
 
  if(global.oldsoft)
    {
      nCols=3;
      ActCols[0]=hkcol.CCDFrame;
      ActCols[1]=hkcol.XRTMode;
      ActCols[2]=hkcol.TIME;
    }
  else
    {
      nCols=7;
      ActCols[0]=hkcol.CCDFrame;
      ActCols[1]=hkcol.XRTMode;
      ActCols[2]=hkcol.TIME;
      ActCols[3]=hkcol.HPIXCT;
      ActCols[4]=hkcol.BIASPX;
      ActCols[5]=hkcol.IMBLVL;
      ActCols[6]=hkcol.CCDTEMP;
    }
  global.hkrow=0;
  /* Read only the following columns */
  
  while(ReadBintableWithNULL(unit, &table, nCols, ActCols,FromRow,&ReadRows) == 0)
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  tmpmode=BVEC(table, n, hkcol.XRTMode);
	
	  if(tmpmode==XRTMODE_SIM || tmpmode==XRTMODE_LIM)
	    {
	      
	      if(!global.hkrow)
		{
		  global.hkrow++;
		  global.hkinfo = (HKInfo_t *)malloc(sizeof(HKInfo_t));
		}
	      else
		{
		  global.hkrow++;
		  global.hkinfo = (HKInfo_t *)realloc(global.hkinfo, (global.hkrow*sizeof(HKInfo_t)));
		  
		}
	      if(!global.oldsoft)
		{
		  global.hkinfo[global.hkrow-1].imblvl=IVEC(table, n, hkcol.IMBLVL);
		  pxct=IVEC(table, n, hkcol.HPIXCT);
		}
	      else
		pxct=0;
	      global.hkinfo[global.hkrow-1].xrtmode=tmpmode;
	      global.hkinfo[global.hkrow-1].hktime=DVEC(table, n, hkcol.TIME);
	      global.hkinfo[global.hkrow-1].ccdframe=VVEC(table, n, hkcol.CCDFrame);
	      global.hkinfo[global.hkrow-1].temp=EVEC(table, n, hkcol.CCDTEMP);

	      /* Calculate Median */


	      if(pxct )
		{
		  median=(int *)calloc(pxct, sizeof(int));
		  for (jj=0; jj< pxct; jj++)
		    {
		      level=IVECVEC(table,n,hkcol.BIASPX,jj);
		      
		      if(jj==0)
			median[jj]=level;
		      else if(level >= median[jj-1])
			median[jj]=level;
		      else
			{
			  for(iii=jj; iii >= 0 ; iii--)
			    {
			      if(!iii)
				median[iii]=level;
			      
			      else if (level < median[iii-1])
				{
				  median[iii]=median[iii-1];
				}
			      else
				{
				  median[iii]=level;
				  iii=0;
				}
			    }
			}
		    }
	  
		  mod=pxct%2;
		  if(mod)
		    global.hkinfo[global.hkrow-1].median=median[(pxct/2)];
		  else
		    {
		      ind=(int)(pxct/2.0);
		      if(!ind)
			global.hkinfo[global.hkrow-1].median=median[ind];
		      else
			global.hkinfo[global.hkrow-1].median=((median[ind] + median[ind - 1])/2.);
		    }
		}
	      else
		global.hkinfo[global.hkrow-1].median=0;
		
	      if(pxct)
		{
		  free(median);
		  pxct=0;
		  
		}

	      
	    }
	}
	
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  tot_imblvl=0;

  if(!global.oldsoft)
    {
      for (iii=0; iii < global.hkrow; iii++)
	{
	  tot_imblvl+=global.hkinfo[global.hkrow-1].imblvl;
	}

      if(tot_imblvl == 0)
	{
	  headas_chat(NORMAL, "%s: Info: This is an old format of the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Info: '%s' file\n", global.taskname, global.par.hdfile);
	  headas_chat(NORMAL, "%s: Info: The bias will be correct on ground.\n", global.taskname);
	  global.oldsoft=1;
	}
	  
	  
    }


  ReleaseBintable(&head, &table);  
  if( CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
      goto ReadHK_end; 
    }
  
  /*printf("TOTROWS== %d HDROWS==%d\n", iiiiiiii,global.biaslvlrow);*/
  /*for (ii=0; ii<global.hkrow; ii++)
    printf("HKTIME== %f BIASLVL==%d HKTEMP==%f\n", global.hkinfo[ii].hktime, global.hkinfo[ii].imblvl,global.hkinfo[ii].temp );
  */
  return OK;
  
 ReadHK_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;
  
}/* ReadHK */
