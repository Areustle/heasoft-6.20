/*
 *	nu_badpix.c: 
 *
 *	DESCRIPTION:
 *
 *        Set of routines to read Bad Pixels files
 *
 *	CHANGE HISTORY:
 *	 0.1.0: - NS 26/04/2011 - First version
 *	 0.1.1: - NS 27/06/2011 - Added 'ReadEvtBadPixExt' and 'UpdateBPExtWithBadPixExtData' functions
 *                              - Modified 'ReadBadPixFile' function
 *	 0.1.2: - NS 04/08/2011 - Added new value 'EV_EDGE_BP' of the STATUS column
 *	 0.1.3: - NS 15/09/2011 - Added new value 'OBS_HOTFLICK_BP' of the BADFLAG column
 *                              - Added new values 'EV_HOTFLICK_BP' and 'EV_NEIGH_HF' of the STATUS column
 *	 0.1.4: - NS 10/10/2011 - Add 'TLMIN*' and 'TLMAX*' keywords in BADPIX extension for RAWX and RAWY columns
 *       0.1.5: - NS 17/11/2011 - Fixed bug in 64-bit architectures
 *       0.1.6: - NS 28/02/2012 - Added new value 'EV_DEPTHCUT' of the STATUS column
 *       0.1.7: - NS 09/05/2012 - Added new keywords in BADPIX extension
 *       0.1.8: - NS 11/05/2012 - Modified 'ReadDisPixFile' function
 *       0.1.9: - RF 05/04/2016 - Modified call 'CreateBPFile' and 'CreateBPExt' function
 *     
 *
 *                                     
 *
 *	AUTHOR:
 *		ASDC - ASI Science Data Center
 */	

#define NU_BADPIX_C
#define NU_BADPIX_VERSION		0.1.9


	/********************************/
	/*        header files          */
	/********************************/


/* badpix header */
#include "nu_badpix.h"

/* headas header */
#include "headas_utils.h"
#include "headas_stdio.h"

/* misc header */
#include "nu_termio.h"
#include "nu_misc.h"
#include "nu_lowlevel.h"

/* highfits header */
#include "nu_basic.h"
#include "nu_highfits.h"
#include "nu_defs.h"

#include "nustardasversion.h"


	/********************************/
	/*     Functions definition     */
	/********************************/


/*
 *
 *      ReadDisPixFile
 *
 *	DESCRIPTION:
 *           Routine to get disabled pixels map from input file
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadDisPixFile(DisPixData_t bpdata[DET_PIXS][DET_ROWS],char *filename, long extno, char *detnam, char* instr, double tstart, double tstop){

  int                n, i=0, j=0, status=OK, found=NOT_OK;
  int                inExt, totExt, rawx, rawy;
  double             time, time_stop;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_detnam[FLEN_VALUE];
  DisPixCol_t        bpcol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;
  char               taskname[MAXFNAME_LEN];
 
  get_toolnamev(taskname);  

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      bpdata[i][j].ninfo = 0;
    }
  }


  /* Open read only input file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", taskname, filename);
      goto ReadDisPixFile_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(unit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",taskname, filename); 
	  goto ReadDisPixFile_end;
	}
    }
  else
    {
      /* Get number of hdus in input file */
      if (fits_get_num_hdus(unit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", taskname, filename);
	  goto ReadDisPixFile_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to BADPIX extension with DETNAM=<detnam> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( unit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", taskname, filename);
	      goto ReadDisPixFile_end;
	    }
      
	  /* Retrieve header pointer */    
	  head=RetrieveFitsHeader(unit);    
	  
	  if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	    strcpy(r_extname, card->u.SVal);
	  else
	    strcpy(r_extname, "NONAME");
	  
	  if(ExistsKeyWord(&head, KWNM_DETNAM, &card))
	    strcpy(r_detnam, card->u.SVal);
	  else
	    strcpy(r_detnam, "-");

	  if( !strcmp(r_extname,KWVL_EXTNAME_DISPIX) && !strcmp(r_detnam,detnam) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with DETNAM='%s'\n", taskname,KWVL_EXTNAME_DISPIX,detnam);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",taskname, filename); 	  
	  goto ReadDisPixFile_end;
	}
    }


  /* Retrieve header pointer */
  head = RetrieveFitsHeader(unit);


  /* Retrieve Start Time */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      if( tstart < card->u.DVal - BP_TIME_SENS )
	{
	  headas_chat(NORMAL, "%s: Warning: Observation start time is not included in the validity time range\n", taskname);
	  headas_chat(NORMAL, "%s: Warning: of the %s file.\n", taskname, filename);
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", taskname, filename);
      goto ReadDisPixFile_end;
    }

  /* Retrieve Stop Time */
  if((ExistsKeyWord(&head, KWNM_TSTOP, &card)))
    {
      if( tstop > card->u.DVal + BP_TIME_SENS )
	{
	  headas_chat(NORMAL, "%s: Warning: Observation stop time is not included in the validity time range\n", taskname);
	  headas_chat(NORMAL, "%s: Warning: of the %s file.\n", taskname, filename);
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", taskname, filename);
      goto ReadDisPixFile_end;
    }

  /* Retrieve Instrument */
  if((ExistsKeyWord(&head, KWNM_INSTRUME, &card)))
    {
      if(strcasecmp(instr,card->u.SVal))
	{
	  headas_chat(NORMAL, "%s: Warning: INSTRUME keywords of %s file not consistent with instrument '%s'\n", taskname, filename, instr);
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", taskname,KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", taskname, filename);
      goto ReadDisPixFile_end;
    }

  
  /* Read DISPIX bintable */
  GetBintableStructure(&head, &table, BP_BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      /* headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", taskname, filename); */
      goto ReadDisPixFile_ok;
    }


  /* Get columns index from name */

  if ((bpcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadDisPixFile_end;
    }

  if ((bpcol.TIME_STOP = GetColNameIndx(&table, CLNM_TIME_STOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_TIME_STOP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadDisPixFile_end;
    }

  if ((bpcol.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadDisPixFile_end;
    }

  if ((bpcol.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadDisPixFile_end;
    }

  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  time = DVEC(table,n,bpcol.TIME);
	  time_stop = DVEC(table,n,bpcol.TIME_STOP);
	  rawx = BVEC(table,n,bpcol.RAWX);
	  rawy = BVEC(table,n,bpcol.RAWY);

	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", taskname, rawx, rawy);
	    goto ReadDisPixFile_end;
	  }
	  
	  if(bpdata[rawx][rawy].ninfo==0){
	    bpdata[rawx][rawy].ninfo = 1;
	    bpdata[rawx][rawy].info = ( DisPixInfo_t*)malloc(sizeof(DisPixInfo_t));
	    if(bpdata[rawx][rawy].info==NULL){
	      headas_chat(CHATTY,"%s: Error: ReadDisPixFile: memory allocation failure.\n", taskname);
	      goto ReadDisPixFile_end;
	    }
	    
	    bpdata[rawx][rawy].info[0].time = time;
	    bpdata[rawx][rawy].info[0].time_stop = time_stop;
	  }
	  /* Time away from the prevoius => add new element to the array */
	  else if(time > bpdata[rawx][rawy].info[(bpdata[rawx][rawy].ninfo-1)].time_stop + BP_TIME_SENS){
	    bpdata[rawx][rawy].ninfo++;
	    bpdata[rawx][rawy].info = (DisPixInfo_t*) realloc( bpdata[rawx][rawy].info, (bpdata[rawx][rawy].ninfo * sizeof(DisPixInfo_t)) );
	    if(bpdata[rawx][rawy].info==NULL){
	      headas_chat(CHATTY,"%s: Error: ReadDisPixFile: memory allocation failure.\n", taskname);
	      goto ReadDisPixFile_end;
	    }

	    bpdata[rawx][rawy].info[ (bpdata[rawx][rawy].ninfo-1) ].time = time;
	    bpdata[rawx][rawy].info[ (bpdata[rawx][rawy].ninfo-1) ].time_stop = time_stop;   
	  }
	  /* Time next to the prevoius => update only the 'time_stop' value */
	  else{
	    bpdata[rawx][rawy].info[ (bpdata[rawx][rawy].ninfo-1) ].time_stop = time_stop; 	    
	  }

	}
      FromRow += ReadRows;
      ReadRows = BP_BINTAB_ROWS;
    }


 ReadDisPixFile_ok:

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",taskname, filename);
      goto ReadDisPixFile_end;
    }


  return OK;
  
 ReadDisPixFile_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


}

/*
 *
 *      ReadBadPixFile
 *
 *	DESCRIPTION:
 *           Routine to get bad pixels map from input file
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadBadPixFile(BadPixData_t bpdata[DET_PIXS][DET_ROWS],char *filename, long extno, char *detnam, char* instr, double tstop, short int inbadflag){

  int                n, i=0, j=0, status=OK, found=NOT_OK;
  int                inExt, totExt, rawx, rawy;
  double             time;
  short int          badflag;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_detnam[FLEN_VALUE];
  BadPixCol_t        bpcol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;
  char               taskname[MAXFNAME_LEN];
 
  get_toolnamev(taskname);  

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      bpdata[i][j].time = 0;
      bpdata[i][j].badflag = 0;
    }
  }


  /* Open read only input file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", taskname, filename);
      goto ReadBadPixFile_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(unit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",taskname, filename); 
	  goto ReadBadPixFile_end;
	}
    }
  else
    {
      /* Get number of hdus in input file */
      if (fits_get_num_hdus(unit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", taskname, filename);
	  goto ReadBadPixFile_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to BADPIX extension with DETNAM=<detnam> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( unit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", taskname, filename);
	      goto ReadBadPixFile_end;
	    }
      
	  /* Retrieve header pointer */    
	  head=RetrieveFitsHeader(unit);    
	  
	  if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	    strcpy(r_extname, card->u.SVal);
	  else
	    strcpy(r_extname, "NONAME");
	  
	  if(ExistsKeyWord(&head, KWNM_DETNAM, &card))
	    strcpy(r_detnam, card->u.SVal);
	  else
	    strcpy(r_detnam, "-");

	  if( !strcmp(r_extname,KWVL_EXTNAME_BADPIX) && !strcmp(r_detnam,detnam) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with DETNAM='%s'\n", taskname,KWVL_EXTNAME_BADPIX,detnam);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",taskname, filename); 	  
	  goto ReadBadPixFile_end;
	}
    }


  /* Retrieve header pointer */
  head = RetrieveFitsHeader(unit);

  /* Retrieve Instrument */
  if((ExistsKeyWord(&head, KWNM_INSTRUME, &card)))
    {
      if(strcasecmp(instr,card->u.SVal))
	{
	  headas_chat(NORMAL, "%s: Error: file %s\n", taskname, filename);
	  headas_chat(NORMAL, "%s: Error: not appropriate for instrument '%s'\n", taskname, instr);
	  goto ReadBadPixFile_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", taskname,KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", taskname, filename);
      goto ReadBadPixFile_end;
    }
  
  /* Read BADPIX bintable */
  GetBintableStructure(&head, &table, BP_BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      /* headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", taskname, filename); */
      goto ReadBadPixFile_ok;
    }


  /* Get columns index from name */

  if ((bpcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadBadPixFile_end;
    }

  if ((bpcol.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadBadPixFile_end;
    }

  if ((bpcol.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadBadPixFile_end;
    }

  if ((bpcol.BADFLAG = GetColNameIndx(&table, CLNM_BADFLAG)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_BADFLAG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadBadPixFile_end;
    }


  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  time = DVEC(table,n,bpcol.TIME);
	  
	  if( time <= tstop+BP_TIME_SENS ){
	    
	    rawx = BVEC(table,n,bpcol.RAWX);
	    rawy = BVEC(table,n,bpcol.RAWY);
	    badflag = XVEC2BYTE(table,n,bpcol.BADFLAG);
	    
	    if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	      headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", taskname, rawx, rawy);
	      goto ReadBadPixFile_end;
	    }

	    if(!(badflag & inbadflag)){
	      headas_chat(NORMAL,"%s: Error: invalid BADFLAG value (RAWX=%d RAWY=%d)\n", taskname, rawx, rawy);
	      headas_chat(NORMAL,"%s: Error: in file: %s.\n", taskname, filename);
	      goto ReadBadPixFile_end;
	    }
	    
	    bpdata[rawx][rawy].time = time;
	    bpdata[rawx][rawy].badflag = badflag;	  
	  }
	  
	}
      FromRow += ReadRows;
      ReadRows = BP_BINTAB_ROWS;
    }


 ReadBadPixFile_ok:

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",taskname, filename);
      goto ReadBadPixFile_end;
    }


  return OK;
  
 ReadBadPixFile_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

}


/*
 *
 *      ReadEvtBadPixExt
 *
 *	DESCRIPTION:
 *           Routine to get bad pixels map from BADPIX extension of input event file
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadEvtBadPixExt(BadPixExtData_t bpdata[DET_PIXS][DET_ROWS],char *filename, char *detnam, short int nobadflag, BOOL *extfound, int *extnum){

  int                n, i=0, j=0, status=OK, found=NOT_OK;
  int                inExt, totExt, rawx, rawy;
  short int          badflag;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_detnam[FLEN_VALUE];
  BadPixExtCol_t     bpcol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;
  char               taskname[FILENAME_MAX];
 
  get_toolnamev(taskname);  

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      bpdata[i][j].ninfo = 0;
    }
  }

  *extnum = -1;

  /* Open read only input file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", taskname, filename);
      goto ReadEvtBadPixExt_end;
    }


  /* Get number of hdus in input file */
  if (fits_get_num_hdus(unit, &totExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", taskname, filename);
      goto ReadEvtBadPixExt_end;
    }
  
  inExt=1;
  status=OK;
  found=NOT_OK;
  /* Move to BADPIX extension with DETNAM=<detnam> */
  while ( found==NOT_OK && status==OK && inExt<=totExt ) 
    {
      if(fits_movabs_hdu( unit, inExt, NULL, &status ))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", taskname,inExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", taskname, filename);
	  goto ReadEvtBadPixExt_end;
	}
      
      /* Retrieve header pointer */    
      head=RetrieveFitsHeader(unit);    
      
      if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	strcpy(r_extname, card->u.SVal);
      else
	strcpy(r_extname, "NONAME");
      
      if(ExistsKeyWord(&head, KWNM_DETNAM, &card))
	strcpy(r_detnam, card->u.SVal);
      else
	strcpy(r_detnam, "-");
      
      if( !strcmp(r_extname,KWVL_EXTNAME_BADPIX) && !strcmp(r_detnam,detnam) ){
	*extnum = inExt;
	found=OK;
      }
      
      inExt++;
    }
  
  if(found == NOT_OK){
    *extfound = FALSE;
    goto ReadEvtBadPixExt_ok;
  }
  else{
    *extfound = TRUE;
  }
  
  
  /* Retrieve header pointer */
  head = RetrieveFitsHeader(unit);
  
  /* Read DISPIX bintable */
  GetBintableStructure(&head, &table, BP_BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;

  /* Get columns index from name */
  
  if ((bpcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadEvtBadPixExt_end;
    }

  if ((bpcol.TIME_STOP = GetColNameIndx(&table, CLNM_TIME_STOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_TIME_STOP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadEvtBadPixExt_end;
    }

  if ((bpcol.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadEvtBadPixExt_end;
    }

  if ((bpcol.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadEvtBadPixExt_end;
    }

  if ((bpcol.BADFLAG = GetColNameIndx(&table, CLNM_BADFLAG)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_BADFLAG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadEvtBadPixExt_end;
    }

  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  rawx = BVEC(table,n,bpcol.RAWX);
	  rawy = BVEC(table,n,bpcol.RAWY);
	  badflag = XVEC2BYTE(table,n,bpcol.BADFLAG);

	  /* Data with 'nobadflag' set are not stored in 'bpdata' structure */
	  if(badflag & nobadflag)
	    continue;
  
	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", taskname, rawx, rawy);
	    goto ReadEvtBadPixExt_end;
	  }
	  
	  if(bpdata[rawx][rawy].ninfo==0){
	    bpdata[rawx][rawy].ninfo = 1;
	    bpdata[rawx][rawy].info = ( BadPixExtInfo_t*)malloc(sizeof(BadPixExtInfo_t));
	    if(bpdata[rawx][rawy].info==NULL){
	      headas_chat(CHATTY,"%s: Error: ReadEvtBadPixExt: memory allocation failure.\n", taskname);
	      goto ReadEvtBadPixExt_end;
	    }
	  }
	  else{
	    bpdata[rawx][rawy].ninfo++;
	    bpdata[rawx][rawy].info = (BadPixExtInfo_t*) realloc( bpdata[rawx][rawy].info, (bpdata[rawx][rawy].ninfo * sizeof(BadPixExtInfo_t)) );
	    if(bpdata[rawx][rawy].info==NULL){
	      headas_chat(CHATTY,"%s: Error: ReadEvtBadPixExt: memory allocation failure.\n", taskname);
	      goto ReadEvtBadPixExt_end;
	    }
	  }

	  bpdata[rawx][rawy].info[ (bpdata[rawx][rawy].ninfo-1) ].time = DVEC(table,n,bpcol.TIME);
	  bpdata[rawx][rawy].info[ (bpdata[rawx][rawy].ninfo-1) ].time_stop = DVEC(table,n,bpcol.TIME_STOP); 
	  bpdata[rawx][rawy].info[ (bpdata[rawx][rawy].ninfo-1) ].badflag = badflag;

	}
      FromRow += ReadRows;
      ReadRows = BP_BINTAB_ROWS;
    }


 ReadEvtBadPixExt_ok:

  /* Free memory allocated with bintable data */
  if (head.first)
    ReleaseBintable(&head, &table);


  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",taskname, filename);
      goto ReadEvtBadPixExt_end;
    }


  return OK;
  
 ReadEvtBadPixExt_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

} /* ReadEvtBadPixExt  */


/*
 *
 *      UpdateBPExtWithBadPixData
 *
 *	DESCRIPTION:
 *           Routine to store in 'outbp' structure the data from input bad pixels map
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int UpdateBPExtWithBadPixData(BadPixData_t inbp[DET_PIXS][DET_ROWS], BadPixExtData_t outbp[DET_PIXS][DET_ROWS], double tstop){

  int        i,j;
  char       taskname[MAXFNAME_LEN];
 
  get_toolnamev(taskname);  


  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      
      if(inbp[i][j].badflag>0){
	
	if(outbp[i][j].ninfo==0){
	  outbp[i][j].ninfo = 1;
	  outbp[i][j].info = (BadPixExtInfo_t*) malloc(sizeof(BadPixExtInfo_t));
	  if(outbp[i][j].info==NULL){
	    headas_chat(CHATTY,"%s: Error: UpdateBPExtWithBadPixData: memory allocation failure.\n", taskname);
	    goto UpdateBPExtWithBadPixData_end;
	  }
	}
	else{
	  outbp[i][j].ninfo++;
	  outbp[i][j].info = (BadPixExtInfo_t*) realloc(outbp[i][j].info, (outbp[i][j].ninfo * sizeof(BadPixExtInfo_t)) );
 	  if(outbp[i][j].info==NULL){
	    headas_chat(CHATTY,"%s: Error: UpdateBPExtWithBadPixData: memory allocation failure.\n", taskname);
	    goto UpdateBPExtWithBadPixData_end;
	  }
	}
	
	outbp[i][j].info[ (outbp[i][j].ninfo-1) ].time = inbp[i][j].time;
	outbp[i][j].info[ (outbp[i][j].ninfo-1) ].time_stop = tstop;
	outbp[i][j].info[ (outbp[i][j].ninfo-1) ].badflag = inbp[i][j].badflag;
      }
      
    }
  }

  return OK;
  
 UpdateBPExtWithBadPixData_end:
  
  return NOT_OK;

}

/*
 *
 *      UpdateBPExtWithDisPixData
 *
 *	DESCRIPTION:
 *           Routine to store in 'outbp' structure the data from input disabled pixels map
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int UpdateBPExtWithDisPixData(DisPixData_t inbp[DET_PIXS][DET_ROWS], BadPixExtData_t outbp[DET_PIXS][DET_ROWS]){

  int        i,j,k;
  char       taskname[MAXFNAME_LEN];
 
  get_toolnamev(taskname);  


  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      
      for(k=0; k<inbp[i][j].ninfo; k++){
	
	if(outbp[i][j].ninfo==0){
	  outbp[i][j].ninfo = 1;
	  outbp[i][j].info = (BadPixExtInfo_t*)malloc(sizeof(BadPixExtInfo_t));
	  if(outbp[i][j].info==NULL){
	    headas_chat(CHATTY,"%s: Error: UpdateBPExtWithDisPixData: memory allocation failure.\n", taskname);
	    goto UpdateBPExtWithDisPixData_end;
	  }
	}
	else{
	  outbp[i][j].ninfo++;
	  outbp[i][j].info = (BadPixExtInfo_t*) realloc(outbp[i][j].info, (outbp[i][j].ninfo * sizeof(BadPixExtInfo_t)) ); 
	  if(outbp[i][j].info==NULL){
	    headas_chat(CHATTY,"%s: Error: UpdateBPExtWithDisPixData: memory allocation failure.\n", taskname);
	    goto UpdateBPExtWithDisPixData_end;
	  }
	}
	
	outbp[i][j].info[ (outbp[i][j].ninfo-1) ].time = inbp[i][j].info[k].time;
	outbp[i][j].info[ (outbp[i][j].ninfo-1) ].time_stop = inbp[i][j].info[k].time_stop;
	outbp[i][j].info[ (outbp[i][j].ninfo-1) ].badflag = ONBOARD_BP;
      }
      
    }
  }

  return OK;
 
 UpdateBPExtWithDisPixData_end:
  
  return NOT_OK;

}


/*
 *
 *      UpdateBPExtWithBadPixExtData
 *
 *	DESCRIPTION:
 *           Routine to store in 'outbp' structure the data from bad pixels extension
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int UpdateBPExtWithBadPixExtData(BadPixExtData_t inbp[DET_PIXS][DET_ROWS], BadPixExtData_t outbp[DET_PIXS][DET_ROWS]){

  int        i,j,k;
  char       taskname[MAXFNAME_LEN];
 
  get_toolnamev(taskname);  


  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      
      for(k=0; k<inbp[i][j].ninfo; k++){
	
	if(outbp[i][j].ninfo==0){
	  outbp[i][j].ninfo = 1;
	  outbp[i][j].info = (BadPixExtInfo_t*)malloc(sizeof(BadPixExtInfo_t));
	  if(outbp[i][j].info==NULL){
	    headas_chat(CHATTY,"%s: Error: UpdateBPExtWithBadPixExtData: memory allocation failure.\n", taskname);
	    goto UpdateBPExtWithBadPixExtData_end;
	  }
	}
	else{
	  outbp[i][j].ninfo++;
	  outbp[i][j].info = (BadPixExtInfo_t*) realloc(outbp[i][j].info, (outbp[i][j].ninfo * sizeof(BadPixExtInfo_t)) ); 
	  if(outbp[i][j].info==NULL){
	    headas_chat(CHATTY,"%s: Error: UpdateBPExtWithBadPixExtData: memory allocation failure.\n", taskname);
	    goto UpdateBPExtWithBadPixExtData_end;
	  }
	}
	
	outbp[i][j].info[ (outbp[i][j].ninfo-1) ].time = inbp[i][j].info[k].time;
	outbp[i][j].info[ (outbp[i][j].ninfo-1) ].time_stop = inbp[i][j].info[k].time_stop;
	outbp[i][j].info[ (outbp[i][j].ninfo-1) ].badflag = inbp[i][j].info[k].badflag;
      }
      
    }
  }

  return OK;
 
 UpdateBPExtWithBadPixExtData_end:
  
  return NOT_OK;

}








/*
 *
 *      CreateBPExt
 *
 *	DESCRIPTION:
 *           Routine to create BADPIX extension with bad pixels data from input 'outbp' structure 
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int CreateBPExt(FitsFileUnit_t ounit, BadPixExtData_t outbp[DET_PIXS][DET_ROWS], int extver, char *detnam, BadPixKeys_t *bpkeys, unsigned short int nobadflag){

  int             i, j, k, col;
  unsigned        nRows;
  BadPixExtCol_t  bpcol;
  Bintable_t	  table; 
  FitsHeader_t	  newhead;
  char            taskname[FLEN_FILENAME], crval[FLEN_VALUE];
  JTYPE           rawxmin=RAWX_MIN, rawxmax=RAWX_MAX;
  JTYPE           rawymin=RAWY_MIN, rawymax=RAWY_MAX;
  JTYPE           jextver;
  char            date[25];
  Version_t       nustardas_v;          /* NuSTARDAS version */

  get_toolnamev(taskname);     
  GetNuSTARDASVersion(nustardas_v);

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );
  
  jextver = (JTYPE)extver;

  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(0, &table);
  
    
  /* Add keywords */
  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_BADPIX, CARD_COMM_EXTNAME);
  AddCard(&newhead, KWNM_EXTVER, J, &jextver, CARD_COMM_EXTVER);
  AddCard(&newhead, KWNM_DETNAM, S, detnam, CARD_COMM_DETNAM);
  AddCard(&newhead, KWNM_TELESCOP, S, KWVL_TELESCOP, CARD_COMM_TELESCOP);
  AddCard(&newhead, KWNM_INSTRUME, S, bpkeys->instrume, CARD_COMM_INSTRUME);
  AddCard(&newhead, KWNM_OBS_ID, S, bpkeys->obs_id, CARD_COMM_OBS_ID);
  AddCard(&newhead, KWNM_ORIGIN, S, bpkeys->origin, CARD_COMM_ORIGIN);
  AddCard(&newhead, KWNM_TIMESYS, S, "TT", CARD_COMM_TIMESYS);
  AddCard(&newhead, KWNM_TIMEUNIT, S, UNIT_SEC, CARD_COMM_TIMEUNIT);
  AddCard(&newhead, KWNM_TIMEREF, S, "LOCAL", CARD_COMM_TIMEREF);
  AddCard(&newhead, KWNM_TSTART, D, &bpkeys->tstart, "time start");
  AddCard(&newhead, KWNM_TSTOP, D, &bpkeys->tstop, "time stop");
  AddCard(&newhead, KWNM_MJDREFI, J, &bpkeys->mjdrefi, CARD_COMM_MJDREFI); 
  AddCard(&newhead, KWNM_MJDREFF, D, &bpkeys->mjdreff, CARD_COMM_MJDREFF);


  /* Add columns */
  AddColumn(&newhead, &table, CLNM_RAWX, CARD_COMM_BP_RAWX, "1B", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
  AddCard(&newhead, "TLMIN1", J, &rawxmin, "Minimum value for RAWX column");
  AddCard(&newhead, "TLMAX1", J, &rawxmax, "Maximum value for RAWX column");
  AddColumn(&newhead, &table, CLNM_RAWY, CARD_COMM_BP_RAWY, "1B", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT); 
  AddCard(&newhead, "TLMIN2", J, &rawymin, "Minimum value for RAWY column");
  AddCard(&newhead, "TLMAX2", J, &rawymax, "Maximum value for RAWY column");
  AddColumn(&newhead, &table, CLNM_TIME, "Start Time of Bad Pixel Interval", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_TIME_STOP, "Stop Time of Bad Pixel Interval", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT); 
  AddColumn(&newhead, &table, CLNM_BADFLAG, CARD_COMM_BADFLAG, "16X", TNONE);

  /* Add creator */
  sprintf(crval,"%s (%s)", taskname, nustardas_v);
  AddCard(&newhead, KWNM_CREATOR, S, crval,CARD_COMM_CREATOR); 

  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&newhead, KWNM_DATE, S, date, CARD_COMM_DATE); 

  
  /* Finish bintable header */
  EndBintableHeader(&newhead, &table);


  /* Get columns index from name */

  if ((bpcol.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: CreateBPExt: unable to find '%s' column\n", taskname, CLNM_RAWX);
      goto CreateBPExt_end;
    }

  if ((bpcol.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: CreateBPExt: unable to find '%s' column\n", taskname, CLNM_RAWY);
      goto CreateBPExt_end;
    }

  if ((bpcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: CreateBPExt: unable to find '%s' column\n", taskname, CLNM_TIME);
      goto CreateBPExt_end;
    }

  if ((bpcol.TIME_STOP = GetColNameIndx(&table, CLNM_TIME_STOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: CreateBPExt: unable to find '%s' column\n", taskname, CLNM_TIME_STOP);
      goto CreateBPExt_end;
    }

  if ((bpcol.BADFLAG = GetColNameIndx(&table, CLNM_BADFLAG)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: CreateBPExt: unable to find '%s' column\n", taskname, CLNM_BADFLAG);
      goto CreateBPExt_end;
    }


  for (col=0; col<table.nColumns; col++)
    {
      table.cols[col] = malloc(table.Multiplicity[col]*table.DataColSize[col]);
      if(table.cols[col]==NULL){
	headas_chat(CHATTY,"%s: Error: CreateBPExt: memory allocation failure.\n", taskname);
	goto CreateBPExt_end;
      }
    }

  nRows = 0;


  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){

      if(outbp[i][j].ninfo>0){
	for (col=0; col<table.nColumns; col++)
	  {
	    table.cols[col] = realloc( table.cols[col], (table.Multiplicity[col]*table.DataColSize[col])*(nRows+outbp[i][j].ninfo));
	    if(table.cols[col]==NULL){
	      headas_chat(CHATTY,"%s: Error: CreateBPExt: memory allocation failure.\n", taskname);
	      goto CreateBPExt_end;
	    }
	  }

	for(k=0; k < outbp[i][j].ninfo; k++)
	{
	  if(!(outbp[i][j].info[k].badflag & nobadflag))
	  {
	    continue;
	  }
	  
	  BVEC(table,nRows,bpcol.RAWX) = i;
	  BVEC(table,nRows,bpcol.RAWY) = j;	  
	  DVEC(table,nRows,bpcol.TIME) = outbp[i][j].info[k].time;
	  DVEC(table,nRows,bpcol.TIME_STOP) = outbp[i][j].info[k].time_stop;	  
	  XVEC2BYTE(table,nRows,bpcol.BADFLAG) = outbp[i][j].info[k].badflag;

	  nRows++;
	}

      } /* if(outbp[i][j].ninfo>0) */
      
    }
  }


  /* Write bintable in file */
  FinishBintableHeader(ounit, &newhead, &table);  
  
  WriteFastBintable(ounit, &table, nRows, TRUE);
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);



  /* Write event bad flags comments */
  if(WriteStatus(ounit))
    {
      headas_chat(NORMAL, "%s: Error: CreateBPExt: Unable to write COMMENT lines\n", taskname);
      goto CreateBPExt_end;
    }


  return OK;
  
 CreateBPExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;
  

} /* CreateBPExt */


/*
 *
 *      CreateBPFile
 *
 *	DESCRIPTION:
 *           Routine to create output Bad Pixel file with bad pixels data from input 'bp' structure
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int CreateBPFile(BadPixExtData_t bp[4][DET_PIXS][DET_ROWS], char *outbpfile, BadPixKeys_t *bpkeys, char *infile,short int nobadflag)
{

  int                 status = OK, hdubp=0; 
  char                date[25];
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, evunit=NULL; 
  char                taskname[FLEN_FILENAME], crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */

  get_toolnamev(taskname);     
  GetNuSTARDASVersion(nustardas_v);

  /* Open input file */
  if ((evunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", taskname, infile);
      goto CreateBPFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outbpfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", taskname, outbpfile);
      goto CreateBPFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(evunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to move in primary header\n", taskname);
      headas_chat(CHATTY, "%s: Error: in input evt file.\n", taskname);
      goto CreateBPFile_end;
    }
  
  if(fits_copy_hdu(evunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to copy primary header from input to\n", taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", taskname, outbpfile);
      goto CreateBPFile_end;
    }
   
/*   Create new empty Primary Header */
/*   head=NewPrimaryHeader(I, 0, 0, NULL, 1); */
/*   FinishPrimaryHeader(outunit, &head); */

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", taskname, outbpfile);
      goto CreateBPFile_end;
    }
  hdubp=1;


  /* Retrieve header pointer */
  head=RetrieveFitsHeader(outunit);
 
  /* Add creator */
  sprintf(crval,"%s (%s)", taskname, nustardas_v);
  AddCard(&head, KWNM_CREATOR, S, crval,CARD_COMM_CREATOR); 
  
  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&head, KWNM_DATE, S, date, CARD_COMM_DATE);  
  
  /* Write header */
  if(WriteUpdatedHeader(outunit, &head))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update primary HDU\n", taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", taskname, outbpfile);
      goto CreateBPFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", taskname, outbpfile);
	goto CreateBPFile_end;
      }
    }


  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[0], 1, KWVL_DETNAM_DET0, bpkeys, nobadflag))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET0);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",taskname, outbpfile);
      goto CreateBPFile_end;
    }
  hdubp++;

  /* Add history to Bad Pixel Ext */
  if(HDpar_stamp(outunit, hdubp, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", taskname);
      goto CreateBPFile_end;
    }

  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[1], 2, KWVL_DETNAM_DET1, bpkeys, nobadflag))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET1);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",taskname, outbpfile);
      goto CreateBPFile_end;
    }
  hdubp++;

  /* Add history to Bad Pixel Ext */
  if(HDpar_stamp(outunit, hdubp, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", taskname);
      goto CreateBPFile_end;
    }

  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[2], 3, KWVL_DETNAM_DET2, bpkeys, nobadflag))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET2);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",taskname, outbpfile);
      goto CreateBPFile_end;
    }
  hdubp++;

  /* Add history to Bad Pixel Ext */
  if(HDpar_stamp(outunit, hdubp, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", taskname);
      goto CreateBPFile_end;
    }

  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[3], 4, KWVL_DETNAM_DET3, bpkeys, nobadflag))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET3);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",taskname, outbpfile);
      goto CreateBPFile_end;
    }
  hdubp++;

  /* Add history to Bad Pixel Ext */
  if(HDpar_stamp(outunit, hdubp, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", taskname);
      goto CreateBPFile_end;
    }

  
  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", taskname, outbpfile);
      goto CreateBPFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", taskname, outbpfile);
      goto CreateBPFile_end;
    }


  return OK;
  
 CreateBPFile_end:

  return NOT_OK;
 
} /* CreateBPFile  */


int CompareBPInfo(const void *aa, const void *bb){

   const BadPixExtInfo_t *a, *b;

   a = (BadPixExtInfo_t *) aa;
   b = (BadPixExtInfo_t *) bb;
   
   if(a->badflag > b->badflag)
     return 1;
   else if (a->badflag < b->badflag)
     return -1;
   else{
     if(a->time > b->time)
       return 1;
     else if (a->time < b->time)
       return -1;
     else
       return 0;
   }
   
 } /* CompareBPInfo  */


/*
 *  SortBadPixExtData
 *
 */
void SortBadPixExtData(BadPixExtData_t bp[DET_PIXS][DET_ROWS]){

  int      i,j;

  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      if(bp[i][j].info!=NULL && bp[i][j].ninfo>0)
	qsort( bp[i][j].info, bp[i][j].ninfo, sizeof(BadPixExtInfo_t), (int(*)(const void *, const void *))CompareBPInfo );
    }
  }

} /* SortBadPixExtData  */


/*
 *  WriteStatus 
 *
 */
int WriteStatus(FitsFileUnit_t ounit){

  int status=0;

  fits_write_comment(ounit, "Events STATUS flags",&status); 
  fits_write_comment(ounit, "b0000000000000000 Good event", &status);
  fits_write_comment(ounit, "b0000000000000001 Event falls in a bad pixel from on-ground CALDB Bad Pixel File", &status);
  fits_write_comment(ounit, "b0000000000000010 Event falls in a on-board disabled pixel", &status);  
  fits_write_comment(ounit, "b0000000000000100 Event falls in a user bad pixel file", &status);  
  fits_write_comment(ounit, "b0000000000001000 Event has a neighbor bad from bad/disabled pixels list", &status);
  fits_write_comment(ounit, "b0000000000010000 Event falls in a pixel on a detector edge", &status);
  fits_write_comment(ounit, "b0000000000100000 Event falls in a hot/flickering pixel", &status);
  fits_write_comment(ounit, "b0000000001000000 Event has a neighbor hot/flickering pixel", &status); 
  fits_write_comment(ounit, "b0000000010000000 Event fails depth cut", &status); 

  if(!status)
    return OK;
  else
    return NOT_OK;

} /* WriteStatus */

