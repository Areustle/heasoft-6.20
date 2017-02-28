/*
 *	nu_caldb.c: 
 *
 *	DESCRIPTION:
 *
 *        Set of routines to read CALDB files
 *
 *	CHANGE HISTORY:
 *     
 *
 *                                     
 *
 *	AUTHOR:
 *		ASDC - ASI Science Data Center
 */	

#define NU_CALDB_C
#define NU_CALDB_VERSION		0.1.0


	/********************************/
	/*        header files          */
	/********************************/


/* caldb header */
#include "nu_caldb.h"




	/********************************/
	/*     Functions definition     */
	/********************************/


int CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd, char *TimeEnd,const char *DataSet, char *CalFileName, char *expr, long *extno, const char *instrument, const char *detnam)
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
   * instrument              - Instrument name
   * detnam                  - Detector ID
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


  if(HDgtcalf(KWVL_TELESCOP, instrument, detnam, 
	      HD_FILT, DataSet, ObsDate, TimeObs, EndDate, TimeEnd,
	      expr, maxret, PIL_LINESIZE, filenam, extno, online, &nret, &nfound, &status) || nfound == 0 )
    {
      headas_chat(NORMAL, "%s: Error: CalGetFileName: Unable to locate %s CALDB file.\n",taskname,DataSet);
      if(nfound == 0)
	{
	  headas_chat(NORMAL, "%s: Error: CalGetFileName: There are not files that satisfied the selection criteria.\n",taskname);
	  headas_chat(CHATTY, "%s: Error: CalGetFileName: Selection criteria used are:\n", taskname);
	  headas_chat(CHATTY, "%s: Error: CalGetFileName: instrument='%s' detnam='%s'\n",taskname,instrument, detnam);
	  headas_chat(CHATTY, "%s: Error: CalGetFileName: dataset='%s' expr=\"%s\".\n",taskname,DataSet, expr);
	}
      goto cal_end;
    }

  if(nfound > 1)
    {
      headas_chat(NORMAL, "%s: Warning: CalGetFileName: There are %d files which match selection criteria.\n",taskname, nfound);
      headas_chat(CHATTY, "%s: Warning: CalGetFileName: Selection criteria are:\n",taskname);
      headas_chat(CHATTY, "%s: Warning: CalGetFileName: instrument='%s' detnam='%s'\n",taskname,instrument, detnam);
      headas_chat(CHATTY, "%s: Warning: CalGetFileName: dataset='%s' expr=\"%s\".\n",taskname,DataSet, expr);
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
