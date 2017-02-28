/*
 *	ReadCalFile: --- Routines to read calibration file 
 *
 *	DESCRIPTION:
 *
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *
 *         BS, 28/07/2004: 0.1.0: - first version
 *         BS, 23/02/2006: 0.1.1: - Added ReadColFile routine
 *         NS, 06/07/2007: 0.1.2: - Added ReadThrFile routine
 *         NS, 30/04/2009: 0.1.3: - Added GetPhasConf routine
 *
 *	AUTHOR:
 *         Italian Swift Archive Center (FRASCATI)
 */


#include <xrtcaldb.h>

#define READCALFILE_C
#define READCALFILE_VERSION			"0.1.3"


		/********************************/
		/*        header files          */
		/********************************/

/* 
 *  ReadCalSourcesFile
 *       Routine to read from CALDB file calibration sources information
 *  
 *
 */

int ReadCalSourcesFile(char * filename, CalSrcInfo_t *calsrc, int extno)
{
  int            n, status=OK;
  char           taskname[MAXFNAME_LEN];
  float          fl_detx, fl_dety, fl_r; 
  unsigned       FromRow, ReadRows, OutRows, nCols, i; 
  CalSrcCol_t    col; 
  Bintable_t     table;             /* Bintable pointer */  
  FitsHeader_t   head;              /* Extension pointer */
  FitsFileUnit_t unit=NULL;     /* Bias file pointer */

  get_toolnamev(taskname);
  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  /* Open read only bias file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", taskname, filename);
      goto read_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: reading '%s' file.\n", taskname, filename);
  
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL, unit, filename, taskname);
  
  if (fits_movabs_hdu(unit,extno, NULL, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to move in %d HDU\n", taskname,extno);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",taskname, filename); 
      goto read_end;
    } 

  head = RetrieveFitsHeader(unit);

  /* Read bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;

  /* Get columns index from name */
  if ((col.DETX = GetColNameIndx(&table, CLNM_DETX)) == -1 )
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_DETX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto read_end;
    }

  if ((col.DETY = GetColNameIndx(&table, CLNM_DETY)) == -1 )
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_DETY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto read_end;
    }

  if ((col.R = GetColNameIndx(&table, CLNM_R)) == -1 )
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_R);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto read_end;
    }
      
  /* Read blocks of bintable rows from input bias file */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  OutRows=0;
  i=0;
  while (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  /* get columns value */
	fl_detx=EVEC(table,n,col.DETX);
	fl_dety=EVEC(table,n,col.DETY);
	fl_r=EVEC(table,n,col.R);
	
	calsrc[n].detx = (int)fl_detx;
	calsrc[n].dety = (int)fl_dety;
	calsrc[n].r = (int)fl_r;

	headas_chat(CHATTY, "%s: Info: Calibration source %d: DETX = %d DETY = %d R = %d\n", taskname, n, calsrc[n].detx,calsrc[n].dety,calsrc[n].r);
	
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
      /*   } */     
  /* Close bias file */
  if (CloseFitsFile(unit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", taskname, filename);
      goto read_end;
    }
  
  return OK;
  
 read_end:
   
  return NOT_OK;

}/* ReadCalSourcesFile */

/* 
 *  ReadColFile
 *
 *  Routine to read from CALDB file WT rawx offset
 */


int ReadColFile(char * filename, FitsFileUnit_t unit, int nrows, WTOffsetT_t *wtoffset)
{
  int                status=OK;
  char               taskname[FLEN_FILENAME];
  WTColT_t           wtcol;
  unsigned           FromRow, ReadRows, n, nCols;
  Bintable_t	     table;
  FitsHeader_t	     head;

  get_toolnamev(taskname);
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  head=RetrieveFitsHeader(unit);
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);

  /* Get needed columns number */

  if ((wtcol.WAVE=ColNameMatch(CLNM_WAVE, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_WAVE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadColFile_end;
    }

  /* Frame Start Time */

  if ((wtcol.AMP=ColNameMatch(CLNM_Amp, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadColFile_end;
    }


  if ((wtcol.WM1STCOL=ColNameMatch(CLNM_WM1STCOL, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_WM1STCOL);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadColFile_end;
    }


  if ((wtcol.WMCOLNUM=ColNameMatch(CLNM_WMCOLNUM, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_WMCOLNUM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadColFile_end;
    }

  if ((wtcol.OFFSET=ColNameMatch(CLNM_OFFSET, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_OFFSET);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadColFile_end;
    }

  EndBintableHeader(&head, &table);
  nCols=table.nColumns;
  FromRow = 1;
  ReadRows=table.nBlockRows;
  
  /* Read input bintable and compute ranges time-tag */
  while (ReadBintable(unit, &table, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
    
      for(n=0; n<ReadRows ; ++n)
	{

	  wtoffset[n].wave=BVEC(table, n, wtcol.WAVE);
	  wtoffset[n].amp=BVEC(table, n, wtcol.AMP);
	  wtoffset[n].wm1stcol=IVEC(table, n, wtcol.WM1STCOL);	  
	  wtoffset[n].wmcolnum=IVEC(table, n, wtcol.WMCOLNUM);	  
	  wtoffset[n].offset=IVEC(table, n, wtcol.OFFSET);	  
	  
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 

  ReleaseBintable(&head, &table);

  return OK;


 ReadColFile_end:
  return NOT_OK;

}/* ReadColFile */


/* 
 *  ReadThrFile
 *
 *  Routine to read from CALDB thresholds values
 */


int ReadThrFile(char * filename, long extno, int xrtvsub, ThresholdInfo_t * thrvalue )
{
  int                status=OK;
  char               taskname[FLEN_FILENAME];
  ThresholdCol_t     thrcol;
  unsigned           FromRow, ReadRows, n, nCols;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL; /* Thresholds file pointer */

  get_toolnamev(taskname);
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  /* Open read only threshold file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", taskname, filename);
      goto ReadThrFile_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: reading '%s' file.\n", taskname, filename);


  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL, unit, filename, taskname);

  if(extno != -1)
    {
      /* move to  XTVSUB extension */
      if (fits_movabs_hdu(unit, extno, NULL, &status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to move in '%d' HDU\n", taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", taskname, filename);
	  goto ReadThrFile_end;
	} 
    }
  else
    {
      /* move to  XRTVSUB extension */
      if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_XRTVSUB, 0, &status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n", taskname, KWVL_EXTNAME_XRTVSUB);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", taskname, filename);
	  goto ReadThrFile_end;
	} 
    }


  head=RetrieveFitsHeader(unit);
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);

  /* Get needed columns number */

  if ((thrcol.XRTVSUB=ColNameMatch(CLNM_XRTVSUB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_XRTVSUB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }


  if ((thrcol.PCEVENT=ColNameMatch(CLNM_PCEVENT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_PCEVENT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }


  if ((thrcol.PCSPLIT=ColNameMatch(CLNM_PCSPLIT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_PCSPLIT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }

  if ((thrcol.WTEVENT=ColNameMatch(CLNM_WTEVENT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_WTEVENT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }


  if ((thrcol.WTSPLIT=ColNameMatch(CLNM_WTSPLIT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_WTSPLIT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }

  if ((thrcol.PDEVENT=ColNameMatch(CLNM_PDEVENT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_PDEVENT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }


  if ((thrcol.PDSPLIT=ColNameMatch(CLNM_PDSPLIT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_PDSPLIT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }

  if ((thrcol.PCEVTTHR=ColNameMatch(CLNM_PCEVTTHR, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_PCEVTTHR);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }

  if ((thrcol.PCSPLITTHR=ColNameMatch(CLNM_PCSPLITTHR, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_PCSPLITTHR);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }
  if ((thrcol.WTBIASTH=ColNameMatch(CLNM_WTBIASTH, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_WTBIASTH);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }

  if ((thrcol.PDBIASTH=ColNameMatch(CLNM_PDBIASTH, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_PDBIASTH);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto ReadThrFile_end;
    }


  EndBintableHeader(&head, &table);
  nCols=table.nColumns;
  FromRow = 1;
  ReadRows=table.nBlockRows;
  
  /* Read input bintable */
  while (ReadBintable(unit, &table, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
    
      for(n=0; n<ReadRows ; ++n)
	{
	  if(xrtvsub == IVEC(table, n, thrcol.XRTVSUB))
	    {
	      thrvalue[0].xrtvsub=IVEC(table, n, thrcol.XRTVSUB);
	      thrvalue[0].pcevent=IVEC(table, n, thrcol.PCEVENT);
	      thrvalue[0].pcsplit=IVEC(table, n, thrcol.PCSPLIT);
	      thrvalue[0].wtevent=IVEC(table, n, thrcol.WTEVENT);
	      thrvalue[0].wtsplit=IVEC(table, n, thrcol.WTSPLIT);
	      thrvalue[0].pdevent=IVEC(table, n, thrcol.PDEVENT);
	      thrvalue[0].pdsplit=IVEC(table, n, thrcol.PDSPLIT);
	      thrvalue[0].pcevtthr=IVEC(table, n, thrcol.PCEVTTHR);
	      thrvalue[0].pcsplitthr=IVEC(table, n, thrcol.PCSPLITTHR);
	      thrvalue[0].wtbiasth=IVEC(table, n, thrcol.WTBIASTH);
	      thrvalue[0].pdbiasth=IVEC(table, n, thrcol.PDBIASTH);
	      goto vsub_found;
	    }  
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 


  headas_chat(NORMAL, "%s: Error: threshold values not found for xrtvsub=%d\n", taskname, xrtvsub);
  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
  goto ReadThrFile_end;


 vsub_found:

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);

  /* Close thr file */
  if (CloseFitsFile(unit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", taskname, filename);
      goto ReadThrFile_end;
    }

  return OK;


 ReadThrFile_end:

  if (head.first)
    ReleaseBintable(&head, &table);
  
  if (unit)
    CloseFitsFile(unit);

  return NOT_OK;

}/* ReadThrFile */



int GetPhasConf(char* filename, double time, int phasconf[9], int *stdconf)
{
  int                status=OK, i=0;
  double             ctime;
  char               taskname[FLEN_FILENAME];
  PHASCongfCol_t     phasconfcol;
  unsigned           FromRow, ReadRows, n, nCols;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL; /* Thresholds file pointer */

  get_toolnamev(taskname);
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  /* Init phasconf array */
  for(i=0; i<9; i++){
    phasconf[i]=i+1;
  }

  /* Open read only threshold file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", taskname, filename);
      goto GetPhasConf_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: reading '%s' file.\n", taskname, filename);


  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL, unit, filename, taskname);


  /* move to  extension */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_PHASCONF, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n", taskname, KWVL_EXTNAME_PHASCONF);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", taskname, filename);
      goto GetPhasConf_end;
    }

  head=RetrieveFitsHeader(unit);
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  
  /* Get needed columns number */

  if ((phasconfcol.TIME=ColNameMatch(CLNM_PHASCONF_TIME, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_PHASCONF_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto GetPhasConf_end;
    }

  if ((phasconfcol.PHASCONF=ColNameMatch(CLNM_PHASCONF_PHASCONF, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_PHASCONF_PHASCONF);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, filename);
      goto GetPhasConf_end;
    }

  /* Check on PHASCONF datatype */
  if (table.Multiplicity[phasconfcol.PHASCONF] != 9)
    {
      headas_chat(NORMAL,"%s: Error: The multiplicity of the %s column is: %d\n", taskname, table.Multiplicity[phasconfcol.PHASCONF]); 
      headas_chat(NORMAL,"%s: Error: but should be: 9 \n", taskname); 
      goto GetPhasConf_end; 
    }


  EndBintableHeader(&head, &table);
  nCols=table.nColumns;
  FromRow = 1;
  ReadRows=table.nBlockRows;

  
  /* Read input bintable */
  while (ReadBintable(unit, &table, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
    
      for(n=0; n<ReadRows ; ++n)
	{
	  ctime = DVEC(table, n, phasconfcol.TIME);
	  if(time>=ctime){
	    for (i=0; i< 9; i++)
	      {
		phasconf[i]=IVECVEC(table,n,phasconfcol.PHASCONF,i);
	      }
	  }
	}

      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);

  /* Close thr file */
  if (CloseFitsFile(unit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", taskname, filename);
      goto GetPhasConf_end;
    }

  /* Check on Standard Configurati*/
  *stdconf = 1;
  for (i=0; i< 9; i++)
    {
      if(phasconf[i]!=i+1){
	*stdconf = 0;
      }
    }

  return OK;

 GetPhasConf_end:

  if (head.first)
    ReleaseBintable(&head, &table);
  
  if (unit)
    CloseFitsFile(unit);

  return NOT_OK;

} /* GetPhasConf */

