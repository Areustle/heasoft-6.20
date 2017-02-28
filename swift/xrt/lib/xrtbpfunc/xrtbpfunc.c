/*
 *	xrtbpfunc :
 *
 *
 *	DESCRIPTION:
 *            Get bad pixels from bad pixels file ( CALDB or user defined ).
 *        
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             get_toolnamev(char *);
 *             headas_chat(int, char *, ...);
 *             
 *             FitsFileUnit_t OpenReadFitsFile(char *name);
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  GetColNameIndx(const Bintable_t *table, const char *ColName);
 *             int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *				 const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *             unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *
 *
 *             int fits_movnam_hdu(fitsfile *fptr, int hdutype, char *extname, 
 *                                 int extver, int * status);
 *
 *
 *             int AddBadPix(int x, int y, BadPixel_t pix, BadPixel_t *bp_table[][]);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 12/02/2002 - First version
 *        0.1.1: - BS 11/12/2003 - Replaced call to 'headas_parstamp()' with 'HDpar_stamp()'
 *        0.1.2: -    22/03/2004 - Added routines to handle hot and flickering pixels
 *        0.1.3: -    11/06/2004 - Added 'WriteBadFlags' routine to write comments with
 *                                 bad flags description in output bad pixels file
 *        0.1.4: -    31/08/2004 - Bug fixed when create and fill Bintable
 *        0.1.5: -    08/11/2004 - Bug fixed in UpdateBPExt routine  
 *        0.1.6: -    26/01/2005 - Bug fixed in UpdateBPExt routine  
 *        0.1.7: -    22/03/2005 - Bug fixed in UpdateBPExt
 *        0.1.8: -    03/08/2005 - Updated bad pixels list in WriteBadFlags routine
 *        0.1.9: -    26/10/2005 - Bug fixed in 'UpdateBPExt' routine
 *        0.2.0: - AB 17/05/2006 - TIME column handling added for badpixel files. 
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */

#include "xrtbpfunc.h"


int GetBadPix(char *badfile, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS],  BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS],int type, long extno, double tstart) 
{
  int            x, y, n, status=OK, amp=0;
  double 	 time;
  char           taskname[FLEN_FILENAME];
  unsigned       FromRow, ReadRows, OutRows, nCols, count, ext; 
  BadCol_t       badcolumns; 
  BadPixel_t     pix;
  Bintable_t     badtable;          /* Bad table pointer */  
  FitsHeader_t   badhead;           /* Bad bintable pointer */
  FitsFileUnit_t bpunit=NULL;       /* Bad pixels file pointer */

  get_toolnamev(taskname);
  TMEMSET0( &badtable, Bintable_t );
  TMEMSET0( &badhead, FitsHeader_t );

  /* Open read only bad pixels file */
  if ((bpunit=OpenReadFitsFile(badfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: GetBadPix: Unable to open\n", taskname);
      headas_chat(NORMAL,"%s: Error: GetBadPix: '%s' file.\n", taskname, badfile);
      goto get_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: GetBadPix: Reading '%s' file.\n", taskname,badfile);
 
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL,bpunit,badfile,"GetBadPix");

  /* move to right badpixels extension */
  if (fits_movabs_hdu(bpunit,(int)(extno), NULL, &status))
    { 
      headas_chat(CHATTY,"%s: Error: GetBadPix: Unable to find %d HDU\n",taskname,extno);
      headas_chat(CHATTY,"%s: Error: GetBadPix: in '%s' file\n",taskname,badfile);      
      goto get_end;
    } 
  
  /* Read badpixels bintable */
  badhead = RetrieveFitsHeader(bpunit);
  GetBintableStructure(&badhead, &badtable, BINTAB_ROWS, 0, NULL);
  nCols=badtable.nColumns;
  
  /* Get numbers columns from name */

  if ((badcolumns.TIME = GetColNameIndx(&badtable, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: GetBadPix: '%s' column does not exist\n", taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: GetBadPix: in '%s' file. \n", taskname, badfile);
      goto get_end;
    }

  if ((badcolumns.RAWX = GetColNameIndx(&badtable, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: GetBadPix: '%s' column does not exist\n", taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: GetBadPix: in '%s' file. \n", taskname, badfile);
      goto get_end;
    }
  
  if ((badcolumns.RAWY = GetColNameIndx(&badtable, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: GetBadPix: '%s' column does not exist\n", taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: GetBadPix: in '%s' file. \n", taskname, badfile);
      goto get_end;
    }

  if ((badcolumns.Amp = GetColNameIndx(&badtable, CLNM_Amp)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: GetBadPix: '%s' column does not exist\n", taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: GetBadPix: in '%s' file. \n", taskname, badfile);
      goto get_end;
    }
  
  if (( badcolumns.TYPE = GetColNameIndx(&badtable, CLNM_TYPE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: GetBadPix: '%s' column does not exist\n", taskname, CLNM_TYPE);
      headas_chat(NORMAL, "%s: Error: GetBadPix: in '%s' file. \n", taskname, badfile);
      goto get_end;
    }
  
  if ((badcolumns.XYEXTENT = GetColNameIndx(&badtable, CLNM_XYEXTENT)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: GetBadPix: '%s' column does not exist\n", taskname, CLNM_XYEXTENT);
      headas_chat(NORMAL, "%s: Error: GetBadPix: in '%s' file. \n", taskname, badfile);
      goto get_end;
    }
  
  if(( badcolumns.BADFLAG = GetColNameIndx(&badtable, CLNM_BADFLAG)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: GetBadPix: '%s' column does not exist\n", taskname, CLNM_BADFLAG);
      headas_chat(NORMAL, "%s: Error: GetBadPix: in '%s' file. \n", taskname, badfile);
      goto get_end;
    }
  
  /* Check if bad pixels table is empty */
  if(!badtable.MaxRows)
    {
      headas_chat(CHATTY, "%s: Error: GetBadPix: bad pixels table of the\n", taskname);
      headas_chat(CHATTY, "%s: Error: GetBadPix: %s file\n", taskname, badfile);
      headas_chat(CHATTY, "%s: Error: GetBadPix: is empty.\n",taskname);
      goto get_end;
    }
  
  /* Read blocks of bintable rows from input badpixels file */
  FromRow=1; 
  ReadRows = badtable.nBlockRows;  /* Read from NAXIS2, so should be all rows --AB-- */ 
  OutRows=0;
  
  while (ReadBintable(bpunit, &badtable, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  /* get columns value */
	  time=DVEC(badtable,n,badcolumns.TIME);/* read   TIME  */
	  /*  only load badpixel if "time" (from event time bintable )  earlier than "tstart" (from event file) */
	  if ( time < tstart ) 		
	    {  			
	      x=IVEC(badtable,n,badcolumns.RAWX);
	      y=IVEC(badtable,n,badcolumns.RAWY);
	      pix.bad_type = IVEC(badtable,n,badcolumns.TYPE);
	      if(pix.bad_type == BAD_COL)
		pix.xyextent = IVEC(badtable,n,badcolumns.XYEXTENT);
	      else
		pix.xyextent=1;

	      pix.bad_flag = XVEC2BYTE(badtable,n,badcolumns.BADFLAG);

	      /* Get Amplifier number */
	      if (badtable.TagVec[badcolumns.Amp] == B)
		amp=BVEC(badtable, n,badcolumns.Amp);
	      else
		amp=IVEC(badtable, n,badcolumns.Amp);
	      if (amp != AMP1 && amp != AMP2)
		{
		  headas_chat(NORMAL,"%s: Error: GetBadPix: Amplifier Number: %d not allowed.\n", taskname,amp);
		  headas_chat(CHATTY,"%s: Error: GetBadPix: Values allowed for amplifier are: %d and %d.\n", taskname, AMP1, AMP2);
		  goto get_end;
		}
	      
	      if(type == CBP)
		pix.bad_flag |= CALDB_BP;
	      else if(type == UBP)
		pix.bad_flag |= USER_BP;
	      else if(type == OBP)
		pix.bad_flag |= ONBOARD_BP;
	      else
		pix.bad_flag = 0;

	      if(amp == AMP1)
		{
		  /* Add bad pixel and info about it in bad pixels map for AMP1*/
		  if (AddBadPix(x,y,pix,bp_table_amp1))
		    {
		      headas_chat(NORMAL, "%s: Error: GetBadPix: Unable to  write pixels in bad pixels map.\n",taskname);
		      goto get_end;
		    }
		  
		  /* If xyextent > 1, check if pixel is into column ... */	
		  if (pix.xyextent > 1 ) 
		    {
		      ext=pix.xyextent;
		      for (count=1; count<ext; count++)
			{
			  y++;
			  pix.xyextent = 0;
			  if (AddBadPix(x,y,pix,bp_table_amp1))
			    {
			      headas_chat(NORMAL, "%s: Error: GetBadPix: Unable to  write pixels in bad pixels map.\n",taskname);
			      goto get_end;
			    }
			}
		    }
		}
	      else
		{
		  /* Add bad pixel and info about it in bad pixels map for AMP2*/
		  if (AddBadPix(x,y,pix,bp_table_amp2))
		    {	
		      headas_chat(NORMAL, "%s: Error: GetBadPix: Unable to  write pixels in bad pixels map.\n",taskname);
		      goto get_end;
		    }
		  
		  /* If xyextent > 1, check if pixel is into column ... */	
		  if (pix.xyextent > 1) {
		    ext=pix.xyextent;
		    for (count=1; count<ext; count++)
		      {
			y++;
			pix.xyextent =0;
			if (AddBadPix(x,y,pix,bp_table_amp2))
			  {
			    headas_chat(NORMAL, "%s: Error: GetBadPix: Unable to  write pixels in bad pixels map.\n",taskname);
			    goto get_end;
			  }
		      }		
		  }
		}

	    } /* if ( time < TSTART )   (TSTART letto dal file di eventi) */
  	  }
        FromRow += ReadRows;
        ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&badhead, &badtable);
  
  /* Close bad pixels file */
  if (CloseFitsFile(bpunit))
    {
      headas_chat(NORMAL, "%s: Error: GetBadPix: Unable to close\n", taskname);
      headas_chat(NORMAL, "%s: Error: GetBadPix: %s file\n", taskname, badfile);
      goto get_end;
    }
  
  return OK;
  
 get_end:
  

  if (badhead.first)
    ReleaseBintable(&badhead, &badtable);
  
  return NOT_OK;
}/* GetBadPix */

/*
 *	AddBadPix : 
 *
 *	DESCRIPTION:
 *             Write position and info about  pixels bad in temporary bad pixels map.           
 *        
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *          get_toolnamev(char *);
 *          headas_chat(int, char *, ...);
 *         
 *	
 *      CHANGE HISTORY:
 *        0.1.0: - BS 14/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int AddBadPix(int x,int y, BadPixel_t pix, BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS])
{
  char           taskname[FLEN_FILENAME];

  get_toolnamev(taskname);

  /* Check if pixel is out of range */
  if (x<0 || x>=CCD_PIXS)
    {
      headas_chat(CHATTY, "%s: Error: AddBadPix: RAWX=%d is out of range.\n", taskname, x);
      goto add_end;
    }
  
  if (y<0 || y>=CCD_ROWS)
    {
      headas_chat(CHATTY, "%s: Error: AddBadPix: RAWY=%d is out of range.\n", taskname, y);
      goto add_end;
    }
  
  /* Add pixel and info about it in bad pixels map */
  if ( bp_table[x][y] == NULL )
    {
      GETMEM(bp_table[x][y], BadPixel_t , 1 ,"AddBadPix", 1);
      TMEMSET0(bp_table[x][y],BadPixel_t);

      
      bp_table[x][y]->bad_type = pix.bad_type; 	
      bp_table[x][y]->xyextent = pix.xyextent;      
      bp_table[x][y]->bad_flag = pix.bad_flag;
      bp_table[x][y]->counter  = 0;
    }
  else
    {
      bp_table[x][y]->bad_flag |= pix.bad_flag;
    }

  if (!(pix.bad_flag&EV_BURNED_BP) )  /* flag does not contain BURNED_BP */
    bp_table[x][y]->counter++;    /* increment only for not burned spots */

  return OK;
  
 add_end:
  return NOT_OK;
}/* AddBadPix */

/*
 *	CreateFile : 
 *
 *
 *	DESCRIPTION:
 *            Build newfile with primary header.
 *        
 *	DOCUMENTATION:
 *             get_toolnamev(char *);         
 *             headas_chat(int ,char *, ...);
 *
 *             int fits_open_file(fitsfile **fptr, char *filename, int iomode, int *status);
 *             int fits_movabs_hdu(fitsfile *fptr,int hdunum,int *hdutype, int *status);
 *             int fits_copy_hdu(fitsfile *infptr, fitsfile *outfptr, int morekeys, int *status);
 *
 * 
 *             FitsFileUnit_t OpenWriteFitsFile(char *name);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *
 *      FUNCTION CALL:
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 14/02/2002 - First version
 *
 *	AUTHORS:   
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
FitsFileUnit_t CreateFile(FitsFileUnit_t evunit, char *filename) {
  
  int            status=OK;
  FitsFileUnit_t newunit=NULL;
  char           taskname[FLEN_FILENAME];

  get_toolnamev(taskname);  
  /* Create new file */
  if ((newunit = OpenWriteFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL, "%s: Error: CreateFile: Unable to create\n", taskname);
      headas_chat(NORMAL, "%s: Error: CreateFile: '%s' file.\n", taskname, filename);
      goto create_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: CreateFile: Writing %s file.\n", taskname, filename);
  
  /* Move to input file primary header to copy it in new file */
  status=OK;
  if(fits_movabs_hdu(evunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: CreateFile: Unable to move in primary header\n", taskname);
      headas_chat(CHATTY, "%s: Error: CreateFile: in %s file. \n", taskname, filename);
      goto create_end;
    }
  
  if(fits_copy_hdu(evunit, newunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: CreateFile: Unable to copy primary header from input to\n", taskname);
      headas_chat(CHATTY, "%s: Error: CreateFile: '%s' file. \n", taskname, filename);
      goto create_end;
    }
  
  headas_chat(CHATTY, "%s: Info: primary HDU successfully created\n", taskname);
  headas_chat(CHATTY, "%s: Info: in '%s' file. \n", taskname, filename);

  return newunit;
  
 create_end:
  
  return (FitsFileUnit_t)0;
}/* CreateFile */

/*
 *	BadpixBintable:
 *
 *
 *	DESCRIPTION:
 *             Add badpixels extension in complete bad pixels file.
 *           
 *        
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             get_toolnamev(char *);         
 *             headas_chat(int ,char *, ...);
 *
 *             FitsHeader_t  NewBintableHeader(const unsigned MaxBlockRows, Bintable_t *table);
 *             void AddCard(FitsHeader_t *header, const char *KeyWord, const ValueTag_t vtag,
 *                          const void *value, const char *comment);
 *             void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype,
 * 			      const char *TypeComment, const char *tform, const unsigned present, ...);
 *             void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *             void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header, Bintable_t *table);
 *             int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows, BOOL last);
 *             unsigned	ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *
 * *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 14/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int BadpixBintable(FitsFileUnit_t ounit, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS], char *extname)
{
  int           i, j, ii, rawy=0;
  char          date[25];
  unsigned      nRows, ext;
  Bintable_t	table; 
  FitsHeader_t	newhead;
  char          taskname[FLEN_FILENAME];

  get_toolnamev(taskname);     

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );
  
  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(0, &table);
  
    
  /* Add extension name */
  AddCard(&newhead, KWNM_EXTNAME, S,extname, CARD_COMM_EXTNAME);

  /* Add columns */
  AddColumn(&newhead, &table,CLNM_RAWX,CARD_COMM_RAWX, "1I", TUNIT|TMIN|TMAX, UNIT_PIXEL, CARD_COMM_PHYSUNIT,RAWX_MIN, RAWX_MAX);
  AddColumn(&newhead, &table,CLNM_RAWY ,CARD_COMM_RAWY, "1I",TUNIT|TMIN|TMAX, UNIT_PIXEL, CARD_COMM_PHYSUNIT,RAWY_MIN, RAWY_MAX); 
  AddColumn(&newhead, &table,CLNM_Amp ,CARD_COMM_Amp , "1I",TNONE);
  AddColumn(&newhead, &table,CLNM_TYPE ,CARD_COMM_TYPE , "1I",TNONE);
  AddColumn(&newhead, &table,CLNM_XYEXTENT,CARD_COMM_XYEXTENT , "I",TNONE);
  AddColumn(&newhead, &table, CLNM_BADFLAG, CARD_COMM_BADFLAG, "16X", TNONE);
  
  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&newhead, KWNM_DATE, S, date, CARD_COMM_DATE);
  
  /* Finish bintable header */
  EndBintableHeader(&newhead, &table);
  
  for (i=0; i<table.nColumns; i++)
    table.cols[i] = NULL;
  
  nRows = 0;
  
  /* Check pixels in bad pixels maps and write them in bintable */
  /* AMP1 */
  for (i=0; i<CCD_PIXS; i++){
    for (j=0; j<CCD_ROWS; j++){
      if(bp_table_amp1[i][j] != NULL)
	{
	  rawy=j;
	  while(((j+1) < CCD_ROWS) && (bp_table_amp1[i][j+1] != NULL) && (bp_table_amp1[i][rawy]->bad_flag==bp_table_amp1[i][j+1]->bad_flag ))
	    j++;
	  
	  for ( ii=0; ii<table.nColumns; ii++) {
	    ( table.cols[ii] ) ? 
	      ( table.cols[ii] = realloc( table.cols[ii], 
					  (table.Multiplicity[ii]*table.DataColSize[ii]*(nRows+1)))) : 
	      (table.cols[ii] = malloc(table.Multiplicity[ii]*table.DataColSize[ii]));
	    
	    if ( table.cols[ii] == NULL ) 
	      {
		headas_chat(NORMAL, "%s: Error: BadpixBintable: Unable to create %s extension.\n",taskname,extname);
		goto bad_end;
	      }
	  }
	  ext = (j - rawy +1);
	  /* Get bad pixels information */
	  IVEC(table,nRows,0) = i; /* RAWX */
	  IVEC(table,nRows,1) = rawy; /* RAWY */
	  IVEC(table,nRows,2) = AMP1; /* AMP */
	  if (ext == 1)
	    IVEC(table,nRows,3) = BAD_PIX; /* TYPE */
	  else
	    IVEC(table,nRows,3) = BAD_COL; /* TYPE */
	  
	  IVEC(table,nRows,4) = ext; /* XYEXTENT */
	  
	  XVEC2BYTE(table,nRows,5) = bp_table_amp1[i][rawy]->bad_flag; /* BADFLAG */
	  nRows++;
	}
    }   
  }
   

  /* Put information from AMP2 */
  for (i=0; i<CCD_PIXS; i++){
    for (j=0; j<CCD_ROWS; j++){

      if(bp_table_amp2[i][j] != NULL)
	{
	  rawy=j;
	  while(((j+1) < CCD_ROWS) && (bp_table_amp2[i][j+1] != NULL) && (bp_table_amp2[i][rawy]->bad_flag==bp_table_amp2[i][j+1]->bad_flag ))
	    j++;
	  
	  for ( ii=0; ii<table.nColumns; ii++) {
	    ( table.cols[ii] ) ? 
	      ( table.cols[ii] = realloc( table.cols[ii], 
					  (table.Multiplicity[ii]*table.DataColSize[ii]*(nRows+1)))) : 
	      (table.cols[ii] = malloc(table.Multiplicity[ii]*(table.DataColSize[ii])));
	    
	    if ( table.cols[ii] == NULL ) 
	      {
		headas_chat(NORMAL, "%s: Error: BadpixBintable: Unable to create %s extension.\n",taskname,extname);
		goto bad_end;
	      }
	  }
	  ext = (j - rawy +1);
	  /* Get bad pixels information */
	  IVEC(table,nRows,0) = i; /* RAWX */
	  IVEC(table,nRows,1) = rawy; /* RAWY */
	  IVEC(table,nRows,2) = AMP2; /* AMP */
	  if (ext == 1)
	    IVEC(table,nRows,3) = BAD_PIX; /* TYPE */
	  else
	    IVEC(table,nRows,3) = BAD_COL; /* TYPE */
	  
	  IVEC(table,nRows,4) = ext; /* XYEXTENT */
	  
	  XVEC2BYTE(table,nRows,5) = bp_table_amp2[i][rawy]->bad_flag; /* BADFLAG */
	  nRows++;
	}

      
    }
    
  }
  

  /* Write bintable in file */
  FinishBintableHeader(ounit, &newhead, &table);  
  
  WriteFastBintable(ounit, &table, nRows, TRUE);
  
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);
  
  return OK;
  
 bad_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;
  
} /* BadPixBintable */
/*
 *	CreateBPfile:
 *
 *
 *	DESCRIPTION:
 *             Routine to create a bad pixels file.    
 *    
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             get_toolnamev(char *);   
 *             headas_chat(int ,char *, ...);
 *
 *             int ChecksumCalc(FitsFileUnit_t unit);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *
 *
 *             int CreateFile(FitsFileUnit_t evunit, char * filename, int );
 *             int BadpixBintable(FitsFileUnit_t ounit,BadPixel_t *badpix[][] );
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 14/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int CreateBPFile(FitsFileUnit_t evunit, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS],char *outbpfile)
{

  int status=OK;
  FitsFileUnit_t badunit=NULL;
  char           taskname[FLEN_FILENAME];

  get_toolnamev(taskname);      
  
  /* Build  primary header */
  if((badunit = CreateFile(evunit, outbpfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL, "%s: Error: CreateBPFile: Unable to create\n", taskname);
      headas_chat(NORMAL, "%s: Error: CreateBPFile: '%s' file.\n", taskname,outbpfile);
      goto end_cbpf;
    }

  /* Add date in primary extension */
  AddDate(badunit);
  
  /* Build  BADPIX extension with data and append it to file */
  if (BadpixBintable(badunit, bp_table_amp1, bp_table_amp2, KWVL_EXTNAME_BAD ))
    {
      headas_chat(NORMAL, "%s: Error: CreateBPFile: Unable to create\n", taskname);
      headas_chat(NORMAL, "%s: Error: CreateBPFile: '%s' file.\n", taskname,outbpfile);
      goto end_cbpf;
    }

  headas_chat(CHATTY, "%s: Info: %s extension created\n", taskname, KWVL_EXTNAME_BAD);
  headas_chat(CHATTY, "%s: Info: in '%s' file.\n", taskname,outbpfile);

  /* Copy keywords from events to badpixels file and add history */
  if (WriteBPKeyword(evunit, badunit, outbpfile))
    {
      headas_chat(NORMAL, "%s: Error: CreateBPFile: Unable add keywords\n", taskname);
      headas_chat(NORMAL, "%s: Error: CreateBPFile: in '%s' file.\n", taskname,outbpfile);
      goto end_cbpf;
    }
  
  headas_chat(CHATTY, "%s: Info: Added keywords standard\n", taskname);
  headas_chat(CHATTY, "%s: Info: in '%s' file.\n", taskname,outbpfile);
  
  /* Add history if parameter history set */
  if(HDpar_stamp(badunit, 2, &status))
    {
      headas_chat(CHATTY, "%s: Error: CreateBPFile: Unable to add HISTORY keywords.\n", taskname);
      goto end_cbpf;
    }

  if(WriteBadFlags(badunit))
    {
      headas_chat(NORMAL, "%s: Error: CreateBPFile: Unable to write comment lines\n", taskname);
      headas_chat(NORMAL, "%s: Error: CreateBPFile: %s file\n", taskname, outbpfile);
      goto end_cbpf;
    }


  /* Calculate checksum and add it in file */
  if (ChecksumCalc(badunit))
    {
      headas_chat(CHATTY, "%s: Error: CreateBPFile: Unable to update CHECKSUM and DATASUM\n",taskname);
      headas_chat(CHATTY, "%s: Error: CreateBPFile: in '%s' file. \n", taskname, outbpfile );
      goto end_cbpf;
    }
  
  /* Close bad pixels file */
  if (CloseFitsFile(badunit))
    {
      headas_chat(CHATTY, "%s: Error: CreateBPFile: Unable to close\n", taskname);
      headas_chat(CHATTY, "%s: Error: CreateBPFile: '%s' file.\n", taskname, outbpfile);
      goto end_cbpf;
    }
  return OK;
  
 end_cbpf:

  return NOT_OK;
  
}/* CreateBPFile */

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
 *        0.1.0: - BS 20/03/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int WriteBPKeyword(FitsFileUnit_t evunit, FitsFileUnit_t badunit, char *outbpfile)
{
  int            status=OK;
  char           crval[FLEN_VALUE], taskname[FLEN_FILENAME], strhist[256], date[25];
  Version_t      swxrtdas_v;          /* SWXRTDAS version */
  FitsCard_t     *card;
  FitsHeader_t   evhead, badhead;
  
  TMEMSET0( &evhead, FitsHeader_t );
  TMEMSET0( &badhead, FitsHeader_t );

  get_toolnamev(taskname);
  
  GetSWXRTDASVersion(swxrtdas_v);

  /* Move to events extension */ 
  if (fits_movnam_hdu(evunit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: WriteBPKeyword: Unable to find  '%s' extension\n", taskname,KWVL_EXTNAME_EVT );
      headas_chat(NORMAL,"%s: Error: WriteBPKeyword: in input event file.\n",taskname);     
      goto write_end; 
    } 
  /* Move to events extension */ 
  if (fits_movnam_hdu(badunit, ANY_HDU,KWVL_EXTNAME_BAD, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: WriteBPKeyword: Unable to find  '%s' extension\n", taskname,KWVL_EXTNAME_BAD );
      headas_chat(NORMAL,"%s: Error: WriteBPKeyword: in %s file.\n",taskname, outbpfile);     
      goto write_end;
    } 

  evhead = RetrieveFitsHeader(evunit);
  badhead = RetrieveFitsHeader(badunit);
  
  /* Add creator */
  sprintf(crval,"%s (%s)", taskname, swxrtdas_v);
  AddCard(&badhead, KWNM_CREATOR, S, crval,CARD_COMM_CREATOR); 

  /* Copy keywords from events file to badpixels file */
  if(ExistsKeyWord(&evhead, KWNM_TELESCOP, &card))
    AddCard(&badhead, KWNM_TELESCOP, S, card->u.SVal,CARD_COMM_TELESCOP);

  if(ExistsKeyWord(&evhead, KWNM_INSTRUME, &card))
    AddCard(&badhead, KWNM_INSTRUME, S,card->u.SVal ,CARD_COMM_INSTRUME);

  if(ExistsKeyWord(&evhead, KWNM_OBS_ID, &card))
    AddCard(&badhead, KWNM_OBS_ID, S, card->u.SVal,CARD_COMM_OBS_ID);

  if(ExistsKeyWord(&evhead, KWNM_ORIGIN, &card))
    AddCard(&badhead, KWNM_ORIGIN, S, card->u.SVal,CARD_COMM_ORIGIN);
 
  if(ExistsKeyWord(&evhead, KWNM_DATAMODE, &card))
    AddCard(&badhead, KWNM_DATAMODE, S,card->u.SVal ,CARD_COMM_DATAMODE);
  /* Add history */
  GetGMTDateTime(date);
  sprintf(strhist, "File created by %s (%s) at %s", taskname, swxrtdas_v,date );
  AddHistory(&badhead, strhist);
    
  
  if(WriteUpdatedHeader(badunit, &badhead))
    { 
      headas_chat(NORMAL,"%s: Error: WriteBPKeyword: Unable to find  '%s' extension\n", taskname,KWVL_EXTNAME_BAD );
      headas_chat(NORMAL,"%s: Error: WriteBPKeyword: in %s file.\n",taskname, outbpfile);     
      goto write_end;
    }
 

  return OK;
 write_end:
  return NOT_OK;

 
}/* WriteBPKeyword */


/*
 *	CutBPFromTable:
 *
 *
 *	DESCRIPTION:
 *        Routine to erase bad pixels from Events FITS file
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             get_toolnamev(char *);   
 *             headas_chat(int ,char *, ...);
 *

 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 06/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */

void CutBPFromTable(BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], int flag)
{
  int           i, j;
  short int     tmp_flag;

  for (i=0; i<CCD_PIXS; i++){
    for (j=0; j<CCD_ROWS; j++){
      if ( bp_table[i][j] != NULL )
	{ 
	  tmp_flag=bp_table[i][j]->bad_flag;
	  tmp_flag &= ~flag;
	  if((tmp_flag == 0))
	    bp_table[i][j] = NULL;
	}
    }
  }
}/* CutBPFromTable */
/*
 *	CreateHotBPfile:
 *
 *
 *	DESCRIPTION:
 *             Routine to create a bad pixels file.    
 *    
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             get_toolnamev(char *);   
 *             headas_chat(int ,char *, ...);
 *
 *             int ChecksumCalc(FitsFileUnit_t unit);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *
 *
 *             int CreateFile(FitsFileUnit_t evunit, char * filename, int );
 *             int BadpixBintable(FitsFileUnit_t ounit,BadPixel_t *badpix[][] );
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 14/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int CreateHotBPFile(FitsFileUnit_t evunit, int amp,BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS],char *outbpfile)
{
  
  int status=OK;
  FitsFileUnit_t badunit=NULL;
  char           taskname[FLEN_FILENAME];

  get_toolnamev(taskname);       
  
  /* Build  primary header */
  if((badunit = CreateFile(evunit, outbpfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL, "%s: Error: CreateHotBPFile: Unable to create\n", taskname);
      headas_chat(NORMAL, "%s: Error: CreateHotBPFile: '%s' file.\n", taskname,outbpfile);
      goto end_cbpf;
    }

  /* Add date in primary extension */
  AddDate(badunit);
  
  /* Build  BADPIX extension with data and append it to file */
  if (CreateBPExt(badunit, bp_table, KWVL_EXTNAME_BAD, amp ))
    {
      headas_chat(NORMAL, "%s: Error: CreateHotBPFile: Unable to create\n", taskname);
      headas_chat(NORMAL, "%s: Error: CreateHotBPFile: '%s' file.\n", taskname,outbpfile);
      goto end_cbpf;
    }
  /* Copy keywords from events to badpixels file and add history */
  if (WriteBPKeyword(evunit, badunit, outbpfile))
    {
      headas_chat(NORMAL, "%s: Error: CreateHotBPFile: Unable add keywords\n", taskname);
      headas_chat(NORMAL, "%s: Error: CreateHotBPFile: in '%s' file.\n", taskname,outbpfile);
      goto end_cbpf;
    }
  
  /* Add history if parameter history set */
  if(HDpar_stamp(badunit, 2, &status))
    {
      headas_chat(CHATTY, "%s: Error: CreateHotBPFile: Unable to add HISTORY keywords.\n", taskname);
      goto end_cbpf;
    }

  if(WriteBadFlags(badunit))
    {
      headas_chat(NORMAL, "%s: Error: CreateHotBPFile: Unable to write comment lines\n", taskname);
      headas_chat(NORMAL, "%s: Error: CreateHotBPFile: %s file\n", taskname, outbpfile);
      goto end_cbpf;
    }


  /* Calculate checksum and add it in file */
  if (ChecksumCalc(badunit))
    {
      headas_chat(CHATTY, "%s: Error: CreateHotBPFile: Unable to update CHECKSUM and DATASUM\n",taskname);
      headas_chat(CHATTY, "%s: Error: CreateHotBPFile: in '%s' file. \n", taskname, outbpfile );
      goto end_cbpf;
    }
  
  /* Close bad pixels file */
  if (CloseFitsFile(badunit))
    {
      headas_chat(CHATTY, "%s: Error: CreateHotBPFile: Unable to close\n", taskname);
      headas_chat(CHATTY, "%s: Error: CreateHotBPFile: '%s' file.\n", taskname, outbpfile);
      goto end_cbpf;
    }
  return OK;
  
 end_cbpf:
  /*
    if (badunit)
    CloseFitsFile(badunit);
  */
  return NOT_OK;
  
}/* CreateHotBPFile */
/*
 *	HotBintable:
 *
 *
 *	DESCRIPTION:
 *             Add badpixels extension in complete bad pixels file.
 *           
 *        
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             get_toolnamev(char *);         
 *             headas_chat(int ,char *, ...);
 *
 *             FitsHeader_t  NewBintableHeader(const unsigned MaxBlockRows, Bintable_t *table);
 *             void AddCard(FitsHeader_t *header, const char *KeyWord, const ValueTag_t vtag,
 *                          const void *value, const char *comment);
 *             void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype,
 * 			      const char *TypeComment, const char *tform, const unsigned present, ...);
 *             void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *             void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header, Bintable_t *table);
 *             int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows, BOOL last);
 *             unsigned	ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *
 * *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 14/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int HotBintable(FitsFileUnit_t ounit, int amp, BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], char *extname)
{
  int           i, j, ii, rawy=0;
  char          date[25];
  unsigned      nRows, ext;
  Bintable_t	table; 
  FitsHeader_t	newhead;
  char          taskname[FLEN_FILENAME];

  get_toolnamev(taskname);     

   
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );
  
  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(0, &table);
  
    
  /* Add extension name */
  AddCard(&newhead, KWNM_EXTNAME, S,extname, CARD_COMM_EXTNAME);

  /* Add columns */
  AddColumn(&newhead, &table,CLNM_RAWX,CARD_COMM_RAWX, "1I", TUNIT|TMIN|TMAX, UNIT_PIXEL, CARD_COMM_PHYSUNIT,RAWX_MIN, RAWX_MAX);
  AddColumn(&newhead, &table,CLNM_RAWY ,CARD_COMM_RAWY, "1I",TUNIT|TMIN|TMAX, UNIT_PIXEL, CARD_COMM_PHYSUNIT,RAWY_MIN, RAWY_MAX); 
  AddColumn(&newhead, &table,CLNM_Amp ,CARD_COMM_Amp , "1I",TNONE);
  AddColumn(&newhead, &table,CLNM_TYPE ,CARD_COMM_TYPE , "1I",TNONE);
  AddColumn(&newhead, &table,CLNM_XYEXTENT,CARD_COMM_XYEXTENT , "I",TNONE);
  AddColumn(&newhead, &table, CLNM_BADFLAG, CARD_COMM_BADFLAG, "16X", TNONE);
  
  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&newhead, KWNM_DATE, S, date, CARD_COMM_DATE);
  
  /* Finish bintable header */
  EndBintableHeader(&newhead, &table);
  
  for (i=0; i<table.nColumns; i++)
    table.cols[i] = NULL;
  
  nRows = 0;
  
  /* Check pixels in bad pixels maps and write them in bintable */
  
  
  for (i=0; i<CCD_PIXS; i++){
    for (j=0; j<CCD_ROWS; j++){
      if(bp_table[i][j] != NULL)
	{
	  rawy=j;
	  while(((j+1) < CCD_ROWS) && (bp_table[i][j+1] != NULL) && (bp_table[i][rawy]->bad_flag==bp_table[i][j+1]->bad_flag ))
	    j++;
	  
	  for ( ii=0; ii<table.nColumns; ii++) {
	    ( table.cols[ii] ) ? 
	      ( table.cols[ii] = realloc( table.cols[ii], 
					  (table.Multiplicity[ii]*table.DataColSize[ii]*(nRows+1)))) : 
	      (table.cols[ii] = malloc(table.Multiplicity[ii]*table.DataColSize[ii]));
	    
	    if ( table.cols[ii] == NULL ) 
	      {
		headas_chat(NORMAL, "%s: Error: BadpixBintable: Unable to create %s extension.\n",taskname,extname);
		goto bad_end;
	      }
	  }
	  ext = (j - rawy +1);
	
	  /* Get bad pixels information */
	  IVEC(table,nRows,0) = i; /* RAWX */
	  IVEC(table,nRows,1) = rawy; /* RAWY */
	  IVEC(table,nRows,2) = amp; /* AMP */
	  if(ext == 1)
	    IVEC(table,nRows,3) = BAD_PIX; /* TYPE */
	  else
	    IVEC(table,nRows,3) = BAD_COL; /* TYPE */
	  
	  IVEC(table,nRows,4) = ext; /* XYEXTENT */
	  XVEC2BYTE(table,nRows,5) = bp_table[i][rawy]->bad_flag; /* BADFLAG */
	  
	  nRows++;
	}
    }
    
  }


  /* Write bintable in file */
  FinishBintableHeader(ounit, &newhead, &table);  
  
  WriteFastBintable(ounit, &table, nRows, TRUE);
  
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);
  
  return OK;
  
 bad_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;
  
} /* HotBintable */
/*
 *
 * WriteStatus 
 */

int WriteBadFlags(FitsFileUnit_t ounit)
{

  int status=0;

  fits_write_comment(ounit, "BAD PIXELS flags",&status);
  fits_write_comment(ounit, "b0000000000000001 Bad pixels from CALDB ", &status);
  fits_write_comment(ounit, "b0000000000000010 Bad pixels from on board Table", &status);  
  fits_write_comment(ounit, "b0000000000000100 Bad Pixel from burned spot", &status);  
  fits_write_comment(ounit, "b0000000000001000 Hot Pixels", &status);  
  fits_write_comment(ounit, "b0000000000010000 Pixels from user provided file", &status);  
  fits_write_comment(ounit, "b0000000000100000 Flickering pixel", &status);  
  fits_write_comment(ounit, "b0000000001000000 Telemetred pixel", &status);  

  if(!status)
    return OK;
  else
    return NOT_OK;

}
/*
 *
 */

int CreateBPExt(FitsFileUnit_t ounit,BadPixel_t *bp_table_amp[CCD_PIXS][CCD_ROWS],char *extname, int amp)
{
  int           i, j, ii, rawy=0, end=1;
  char          date[25];
  unsigned      nRows, ext;
  Bintable_t	table; 
  FitsHeader_t	newhead;
  char          taskname[FLEN_FILENAME];


  get_toolnamev(taskname);     

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );
  
  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(0, &table);
  
    
  /* Add extension name */
  AddCard(&newhead, KWNM_EXTNAME, S,extname, CARD_COMM_EXTNAME);
  
  /* Add columns */
  AddColumn(&newhead, &table,CLNM_RAWX,CARD_COMM_RAWX, "1I", TUNIT|TMIN|TMAX, UNIT_PIXEL, CARD_COMM_PHYSUNIT,RAWX_MIN, RAWX_MAX);
  AddColumn(&newhead, &table,CLNM_RAWY ,CARD_COMM_RAWY, "1I",TUNIT|TMIN|TMAX, UNIT_PIXEL, CARD_COMM_PHYSUNIT,RAWY_MIN, RAWY_MAX); 
  AddColumn(&newhead, &table,CLNM_Amp ,CARD_COMM_Amp , "1I",TNONE);
  AddColumn(&newhead, &table,CLNM_TYPE ,CARD_COMM_TYPE , "1I",TNONE);
  AddColumn(&newhead, &table,CLNM_XYEXTENT,CARD_COMM_XYEXTENT , "I",TNONE);
  AddColumn(&newhead, &table, CLNM_BADFLAG, CARD_COMM_BADFLAG, "16X", TNONE);
  
  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&newhead, KWNM_DATE, S, date, CARD_COMM_DATE);
  
  /* Finish bintable header */
  EndBintableHeader(&newhead, &table);
  
  for (i=0; i<table.nColumns; i++)
    table.cols[i] = NULL;
  
  nRows = 0;
  
  for (i=0; i<CCD_PIXS; i++){
    for (j=0; j<CCD_ROWS; j++){
      if(bp_table_amp[i][j] != NULL)
	{
	  rawy=j;
	  while(((j+1) < CCD_ROWS) && (bp_table_amp[i][j+1] != NULL) && (bp_table_amp[i][rawy]->bad_flag==bp_table_amp[i][j+1]->bad_flag ))
	    j++;
	  
	  for ( ii=0; ii<table.nColumns; ii++) {
	    ( table.cols[ii] ) ? 
	      ( table.cols[ii] = realloc( table.cols[ii], 
					  (table.Multiplicity[ii]*table.DataColSize[ii])*(nRows+1))) : 
	      (table.cols[ii] = malloc(table.Multiplicity[ii]*table.DataColSize[ii]));
	    
	    if ( table.cols[ii] == NULL ) 
	      {
		headas_chat(NORMAL, "%s: Error: BadpixBintable: Unable to create %s extension.\n",taskname,extname);
		goto create_end;
	      }
	  }
	  ext = (j - rawy +1);
	  /* Get bad pixels information */
	  IVEC(table,nRows,0) = i; /* RAWX */
	  IVEC(table,nRows,1) = rawy; /* RAWY */
	  IVEC(table,nRows,2) = amp; /* AMP */
	  if (ext == 1)
	    IVEC(table,nRows,3) = BAD_PIX; /* TYPE */
	  else
	    IVEC(table,nRows,3) = BAD_COL; /* TYPE */
	  
	  IVEC(table,nRows,4) = ext; /* XYEXTENT */
	  
	  XVEC2BYTE(table,nRows,5) = bp_table_amp[i][rawy]->bad_flag; /* BADFLAG */
	  nRows++;
	}
            
      
    }
    
    
  }

  /* Write bintable in file */
  FinishBintableHeader(ounit, &newhead, &table);  
  
  WriteFastBintable(ounit, &table, nRows, TRUE);
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);
  
  if(WriteBadFlags(ounit))
    {
      headas_chat(NORMAL, "%s: Error: CreateBPExt: Unable to write comment lines.\n", taskname);
      goto create_end;
    }


  return OK;
  
 create_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;
  

}/*CreateBPExt*/

/*
 *
 */
int UpdateBPExt(FitsFileUnit_t outunit,  FitsFileUnit_t evunit, BadPixel_t *bp_table_amp[CCD_PIXS][CCD_ROWS],char *extname, int amp, int hducount)
{
  int            x, y, n, status=OK, i=0, j=0, ii=0, rawy=0, end=1, bpamp=0, ymax=0;
  char           taskname[FLEN_FILENAME];
  unsigned       FromRow, ReadRows, OutRows=0, nCols, count, ext; 
  BadCol_t       badcolumns; 
  BadPixel_t     pix;
  Bintable_t     table;          /* Bad table pointer */  
  FitsHeader_t   badhead;           /* Bad bintable pointer */

  get_toolnamev(taskname);

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &badhead, FitsHeader_t );

  if(fits_movabs_hdu(evunit, hducount, NULL, &status))
    { 
      headas_chat(CHATTY, "%s: Error: UpdateBPExt: Unable to move in  hdu num: %d.\n", taskname,hducount); 
      goto update_end; 
    } 

  /* Read badpixels bintable */
  badhead = RetrieveFitsHeader(evunit);
  GetBintableStructure(&badhead, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  /* Get numbers columns from name */
  if ((badcolumns.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: UpdateBPExt: '%s' column does not exist.\n", taskname, CLNM_RAWX);
      goto update_end;
    }
  
  if ((badcolumns.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: UpdateBPExt: '%s' column does not exist.\n", taskname, CLNM_RAWY);
      goto update_end;
    }

  if ((badcolumns.Amp = GetColNameIndx(&table, CLNM_Amp)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: UpdateBPExt: '%s' column does not exist.\n", taskname, CLNM_Amp);
      goto update_end;
    }
  
  if (( badcolumns.TYPE = GetColNameIndx(&table, CLNM_TYPE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: UpdateBPExt: '%s' column does not exist.\n", taskname, CLNM_TYPE);
      goto update_end;
    }
  
  if ((badcolumns.XYEXTENT = GetColNameIndx(&table, CLNM_XYEXTENT)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: UpdateBPExt: '%s' column does not exist.\n", taskname, CLNM_XYEXTENT);
      goto update_end;
    }
  
  if(( badcolumns.BADFLAG = GetColNameIndx(&table, CLNM_BADFLAG)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: UpdateBPExt: '%s' column does not exist.\n", taskname, CLNM_BADFLAG);
      goto update_end;
    }
  
  /* Check if bad pixels table is empty */
  if(!table.MaxRows)
    {
      headas_chat(CHATTY, "%s: Error: UpdateBPExt: bad pixels table of the\n", taskname);
      headas_chat(CHATTY, "%s: Error: UpdateBPExt: input file is empty.\n",taskname);
      goto update_end;
    }
  
  EndBintableHeader(&badhead, &table); 
  FinishBintableHeader(outunit, &badhead, &table);
  /* Read blocks of bintable rows from input badpixels extension */
  FromRow=1; 
  ReadRows = BINTAB_ROWS; /*table.MaxRows;*/
  OutRows=0;
  
  while ((ReadBintable(evunit, &table, nCols, NULL, FromRow, &ReadRows) == 0) )
    {
      for (n=0; n<ReadRows && OutRows < table.MaxRows ; ++n)
	{
	  
	  /* Get Amplifier number */
	  if (table.TagVec[badcolumns.Amp] == B)
	    bpamp=BVEC(table, n,badcolumns.Amp);
	  else
	    bpamp=IVEC(table, n,badcolumns.Amp);
	  if (bpamp != AMP1 && bpamp != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Error: UpdateBPExt: Amplifier Number: %d not allowed.\n", taskname,bpamp);
	      headas_chat(CHATTY,"%s: Error: UpdateBPExt: Values allowed for amplifier are: %d and %d.\n", taskname, AMP1, AMP2);
	      goto update_end;
	    }

	  
	  if (bpamp == amp)
	    {
	      /* get columns value */
	      x=IVEC(table,n,badcolumns.RAWX);
	      y=IVEC(table,n,badcolumns.RAWY);
	      pix.bad_type = IVEC(table,n,badcolumns.TYPE); 	
	      pix.xyextent = IVEC(table,n,badcolumns.XYEXTENT);
	      pix.bad_flag= XVEC2BYTE(table,n,5);

	      ymax=(y + pix.xyextent - 1);

	      if(ymax >  RAWY_MAX)
		ymax=RAWY_MAX;

	      if (pix.bad_type == BAD_PIX && bp_table_amp[x][y]!=NULL )
		{
		  bp_table_amp[x][y]=NULL;
		}
	      else if (pix.bad_type == BAD_COL && bp_table_amp[x][y]!=NULL)
		{
		  for (; y <= ymax;y++)
		    {
		      if(bp_table_amp[x][y]!=NULL)
			{
			  if(pix.bad_flag ==  bp_table_amp[x][y]->bad_flag)
			    {
			      bp_table_amp[x][y]=NULL;
			    }
			  else
			    {
			      y = ymax;
			  
			    } 
			}
		      else
			y = ymax;
		    }
		}
	    }
	  
	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(outunit, &table, OutRows, FALSE);
	      OutRows=0;
	    }
		  
	}

      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
	  
    }
 
  for (i=0; i<CCD_PIXS; i++){
    for (j=0; j<CCD_ROWS; j++){

       if(bp_table_amp[i][j] != NULL)
	{
	  rawy=j;
	  while(((j+1) < CCD_ROWS) && (bp_table_amp[i][j+1] != NULL) && (bp_table_amp[i][rawy]->bad_flag==bp_table_amp[i][j+1]->bad_flag ))
	    j++;
	  
	  for ( ii=0; ii<table.nColumns; ii++) {
	    ( table.cols[ii] ) ? 
	      ( table.cols[ii] = realloc( table.cols[ii], 
					  (table.Multiplicity[ii]*table.DataColSize[ii]*(OutRows+1)))) : 
	      (table.cols[ii] = malloc((table.Multiplicity[ii]*table.DataColSize[ii])));
	    
	    if ( table.cols[ii] == NULL ) 
	      {
		headas_chat(NORMAL, "%s: Error: BadpixBintable: Unable to create %s extension.\n",taskname,extname);
		goto update_end;
	      }
	  }
	  
	  ext = (j - rawy +1);
	  /* Get bad pixels information */
	  IVEC(table,OutRows,0) = i; /* RAWX */
	  IVEC(table,OutRows,1) = rawy; /* RAWY */
	  IVEC(table,OutRows,2) = amp; /* AMP */
	  if (ext == 1)
	    IVEC(table,OutRows,3) = BAD_PIX; /* TYPE */
	  else
	    IVEC(table,OutRows,3) = BAD_COL; /* TYPE */
	  
	  IVEC(table,OutRows,4) = ext; /* XYEXTENT */
	  
	  XVEC2BYTE(table,OutRows,5) = bp_table_amp[i][rawy]->bad_flag; /* BADFLAG */

	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(outunit, &table, OutRows, FALSE);
	      OutRows=0;
	    }
	}
    }
    
    
  }
  
  /* Write bintable in file */
  /*EndBintableHeader(&badhead, &table);*/
  
  /*WriteUpdatedHeader(outunit, &badhead);*/
    WriteFastBintable(outunit, &table, OutRows, TRUE);
  
  /* Free memory allocated with bintable data */
  /*ReleaseBintable(&badhead, &table);*/
  
  
  return OK;
  
    update_end:
  return NOT_OK;
  
}/*UpdateBPExt*/
