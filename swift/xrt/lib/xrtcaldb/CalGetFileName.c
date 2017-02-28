/*
 *	CalGetFileName: --- this function returns the Current Available 
 *                             Calibration file name ---
 *
 *	DESCRIPTION:
 *
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *
 *         FT, 05/05/02: 0.1.0: - first version
 *         BS, 09/09/02: 0.1.1: - Modified functions to locate CALDB file
 *                                name by observation date and time
 *             15/07/03: 0.1.2: - Used HEADAS routine 'HDgtcalf' instead of 'CalQueryDataBase'              
 *                              - Added check on date and time format
 *             24/07/03: 0.1.3  - Added check on date and time contents
 *             05/11/03: 0.1.4  - Added check on number of files found
 *             23/03/04: 0.1.5  - Added routine 'CalGetExtNum'     
 *             18/05/04: 0.1.6  - Messages displayed revision
 *             10/11/05: 0.1.7  - Bug fixed as reported in Tripicco mail (09/11/05)
 *	AUTHOR:
 *         Italian Swift Archive Center (FRASCATI)
 */

#include "xrtcaldb.h"

#define CALGETFILENAME_C
#define CALGETFILENAME_VERSION			"0.1.7"


		/********************************/
		/*        header files          */
		/********************************/


int CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd, char *TimeEnd,const char *DataSet, char *CalFileName, char *expr, long *extno)
{
  /* maxret                  - number of datasets to return
   * DateObs                 - Observation Start Date
   * TimeObs                 - Observation Start Time
   * DateEnd                 - Observation End Date
   * TimeEnd                 - Observation End Time
   * 
   *  If DATE-OBS and DATE-END formats are "yyyy-mm-ddThh:mm:ss" the routine divides them 
   *  in DateObs/DateEnd = "yyyy-mm-dd" and TimeObs/TimeEnd = "hh:mm:ss"
   *  If DateObs/DateEnd = "now" and TimeObs/TimeEnd = "now" gets the last version of the file 
   *  
   * DataSet                 - Calibration Dataset 
   * CalFileName             - Calibration filename 
   * expr                    - A boolean expression used to select on calibration boundary parameters
   */
  
  int nret=0, nfound=0, status=0, ii=0, match=0, exact=0; 
  char date_templ[]="*-*-*", time_templ[]="*:*:*";
  char **filenam;
  char **online;
  char *tmp1, *ObsDate, *EndDate;
  char           taskname[FLEN_FILENAME];
  
  get_toolnamev(taskname);     


  filenam=(char **)calloc(maxret, sizeof(char *));
  online=(char **)calloc(maxret, sizeof(char *));

  for (ii=0; ii<maxret;ii++)
  {  
    filenam[ii]=(char *)malloc((sizeof(char))*PIL_LINESIZE);
    online[ii]=(char *)malloc((sizeof(char))*20);
  }

  if((tmp1=strchr(DateObs, 'T')))
    {
      TimeObs=(char *)malloc(sizeof(char)*strlen(tmp1));
      strcpy(TimeObs, tmp1+1);
      ObsDate=(char *)malloc(sizeof(char)*(strlen(DateObs)-strlen(tmp1)));
      strncpy(ObsDate, DateObs, strlen(DateObs)-strlen(tmp1));
      ObsDate[strlen(DateObs)-strlen(tmp1)]='\0';
    }
  else
    {
      ObsDate=(char *)malloc(sizeof(char)*(strlen(DateObs)+1));
      strcpy(ObsDate, DateObs);
    } 


  if((tmp1=strchr(DateEnd, 'T')))
    {
      TimeEnd=(char *)malloc(sizeof(char)*strlen(tmp1));
      strcpy(TimeEnd, tmp1+1);
      EndDate=(char *)malloc(sizeof(char)*(strlen(DateEnd)-strlen(tmp1)));
      strncpy(EndDate, DateEnd, strlen(DateEnd)-strlen(tmp1));
      EndDate[strlen(DateEnd)-strlen(tmp1)]='\0';
    }
  else
    {
      EndDate=(char *)malloc(sizeof(char)*(strlen(DateEnd)+1));
      strcpy(EndDate, DateEnd);
    } 




  /*
   *	get file name from 
   *    calibration database
   */

  /* Check time keywords format */
  
  if(strcmp(DF_NOW, ObsDate) || strcmp(DF_NOW, TimeObs))
    {
      fits_compare_str(date_templ, ObsDate, TRUE, &match, &exact);
      if(match)
	{
	  fits_compare_str(time_templ, TimeObs, TRUE, &match, &exact);
	  if(!match)
	    {
	      headas_chat(NORMAL, "%s: Error: CalGetFileName: The format '%s' is illegal for the %s keyword.\n",  taskname, TimeObs,KWNM_TIMEOBS);
	      goto cal_end;
	    }
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: CalGetFileName: The format '%s' is illegal for %s keyword.\n",taskname,ObsDate,KWNM_DATEOBS);
	  goto cal_end;
	}
    }

  if(strcmp(DF_NOW, EndDate) || strcmp(DF_NOW, TimeEnd))
    {
      fits_compare_str(date_templ, EndDate, TRUE, &match, &exact);
      if(match)
	{
	  fits_compare_str(time_templ, TimeEnd, TRUE, &match, &exact);
	  if(!match)
	    {
	      headas_chat(NORMAL, "%s: Error: CalGetFileName: The format '%s' is illegal for %s keyword.\n",taskname,TimeEnd,KWNM_TIMEEND);
	      goto cal_end;
	    }
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: CalGetFileName: The format '%s' is illegal for %s keyword.\n",taskname,EndDate,KWNM_DATEEND);
	  goto cal_end;
	}
    }


  if(HDgtcalf(KWVL_TELESCOP, KWVL_INSTRUME, HD_DETNAM, 
	      HD_FILT, DataSet, ObsDate, TimeObs, EndDate, TimeEnd,
	      expr, maxret, PIL_LINESIZE, filenam, extno, online, &nret, &nfound, &status) || nfound == 0 )
    {
      headas_chat(NORMAL, "%s: Error: CalGetFileName: Unable to locate %s CALDB file.\n",taskname,DataSet);
      if(nfound == 0)
	{
	  headas_chat(NORMAL, "%s: Error: CalGetFileName: There are not files that satisfied the selection criteria.\n",taskname);
	  headas_chat(CHATTY, "%s: Error: CalGetFileName: Selection criteria used are:\n", taskname);
  headas_chat(CHATTY, "%s: Error: CalGetFileName: dataset=%s expr=\"%s\".\n",taskname,DataSet, expr);
	}
      goto cal_end;
    }

  if(nfound > 1)
    {
      headas_chat(NORMAL, "%s: Warning: CalGetFileName: There are %d files which match selection criteria.\n",taskname, nfound);
      headas_chat(CHATTY, "%s: Warning: CalGetFileName: Selection criteria are:\n",taskname);
      headas_chat(CHATTY, "%s: Warning: CalGetFileName: dataset: %s expr=\"%s\".\n",taskname,DataSet, expr);
    }


  strcpy(CalFileName, filenam[0]);


  free(filenam);
  free(online);
  
  headas_chat(CHATTY, "%s: Info: CalGetFileName: Found %s file\n", taskname, CalFileName);
  headas_chat(CHATTY, "%s: Info: CalGetFileName: in Calibration database.\n",taskname);

  return OK;

 cal_end:
  if(filenam)
    free(filenam);
  if(online)
    free(online);
  

  return NOT_OK;
  
} /* CalGetFileName */

extern int CalGetExtNumber(char * CalFileName, char *ExtName, long *extno)
{
  int            status, hdunum;
  FitsFileUnit_t unit=NULL;
  
  char           taskname[FLEN_FILENAME];

  get_toolnamev(taskname);     
  status=0;
  /* Open read only input event file */
  if ((unit=OpenReadFitsFile(CalFileName)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL, "%s: Error: CalGetExtNumber: Unable to open\n",taskname);
      headas_chat(NORMAL, "%s: Error: CalGetExtNumber: '%s' file.\n",taskname,CalFileName);
      goto cgen_end;
    }

  if (fits_movnam_hdu(unit, ANY_HDU,ExtName, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: CalGetExtNumber: Unable to find  '%s' extension\n",taskname, ExtName);  
      headas_chat(NORMAL,"%s: Error: CalGetExtNumber: in '%s' file\n",taskname, CalFileName);
      /* Close input event file */
      if (CloseFitsFile(unit))
	{
	  headas_chat(NORMAL, "%s: Error: CalGetExtNumber: Unable to close\n",taskname);
	  headas_chat(NORMAL, "%s: Error: CalGetExtNumber: '%s' file.\n",taskname,CalFileName);

	}
      goto cgen_end;
    }

  fits_get_hdu_num(unit, &hdunum);
  

  *extno = (long)hdunum;

  return OK;

 cgen_end:

  return NOT_OK;
}
