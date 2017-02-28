/*************************************************************************
 *
 *    xrtfits.c: module with specific functions related to FITS
 *
 *
 *    AUTHOR:
 *      Italian Swift Archive Center (FRASCATI)
 *		
 ************************************************************************/

		    /********************************/
                    /*        header files          */
                    /********************************/

#include "xrtfits.h"

#define BUFSIZE 81

void AddDate(fitsfile *fptr)
{
  int status;
  char           taskname[FLEN_FILENAME];
  
  get_toolnamev(taskname);     

  status=0;
  if (fits_write_date(fptr, &status) )
    headas_chat(NORMAL, "%s: Warning: AddDate: Unable to write DATE keyword.\n", taskname);
  return;
}


BOOL IsXRT_Header(const FitsHeader_t *Header )
{
  return ExistsKeyWord(Header, KWNM_TELESCOP, NULLNULLCARD) &&
    STREQ(GetSVal(Header, KWNM_TELESCOP), KWVL_TELESCOP) &&
    ExistsKeyWord(Header, KWNM_INSTRUME, NULLNULLCARD) &&
    STREQ(GetSVal(Header, KWNM_INSTRUME), KWVL_INSTRUME);
} /* IsXRT_Header */


BOOL IsXRT_File(FitsFileUnit_t unit)
{
  return KeyWordMatch(unit, KWNM_TELESCOP, S, KWVL_TELESCOP, NULL) &&
    KeyWordMatch(unit, KWNM_INSTRUME, S, KWVL_INSTRUME, NULL);
} /* IsXRT_File */

BOOL ObsIdCkeck(const FitsHeader_t *Header, char *Obs_Id)
{
  char           taskname[FLEN_FILENAME];
  
  get_toolnamev(taskname);     
  if ( !ExistsKeyWord(Header, KWNM_OBS_ID, NULLNULLCARD) )
    headas_chat(NORMAL,"%s: Error: ObdIdCheck: %s keyword not found.\n",taskname,KWNM_OBS_ID);
  return ExistsKeyWord(Header, KWNM_OBS_ID, NULLNULLCARD) &&
    STREQ(GetSVal(Header, KWNM_OBS_ID), Obs_Id );
} /* IsXRT_File */
/*
 *	BitSet
 *
 *
 *	DESCRIPTION:
 *                 Routine to check if a bit of a X type column
 *                 value (array of bytes) is set. Each element of
 *                 an nX type colum is read into an array of bytes.
 *                 The array dimension is (n/8)+1.
 *                 The function returns 1 if the bit is set otherwise
 *                 it returns 0.
 *
 *	DOCUMENTATION:
 *
 *      FUNCTION CALL:
 *	
 *      CHANGE HISTORY:
 *        0.1.0: - FT 22/01/203 - First version
 *
 *	AUTHORS:
 *        Italian Swift Archive Center (FRASCATI)
 */
BOOL BitSet ( Bintable_t table, unsigned RawNb, unsigned ColNb, int bitnumb)
{
  char bitmask[] = { 1, 2, 4, 8, 16, 32, 64, 128 };
  int num1,num2;
  
  num1 = (int)bitnumb/8;
  num2 = bitnumb - num1*8 - 1;
  
  return ( XVECVEC(table,RawNb,ColNb,num1) & bitmask[num2] );
} /* BitSet */

BOOL IsThisEvDatamode(char *filename, char *datamode, BOOL *ret)
{ 
  
  int status=OK;
  FitsFileUnit_t unit=NULL;
  char           taskname[FLEN_FILENAME];
  
  get_toolnamev(taskname);       


  /* Open readonly input event file */
   if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
      {  
        headas_chat(NORMAL,"%s: Error: IsThisEvDatamode: Unable to open\n", taskname); 
        headas_chat(NORMAL,"%s: Error: IsThisEvDatamode: '%s' FITS file.\n", taskname,filename); 
        goto Error;
      } 
    
   /*  if(fits_open_file(&unit, filename, READONLY, &status))
       {
       headas_chat(CHATTY,"%s: Error: IsThisEvDatamode: Unable to open\n", taskname); 
       headas_chat(CHATTY,"%s: Error: IsThisEvDatamode: '%s' file.\n",taskname, filename);
       goto Error; 
       }  
   */

  /* Move in events extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status)) 
      {  
        headas_chat(CHATTY,"%s: Error: IsThisEvDatamode: Unable to find  '%s' extension\n", taskname, KWVL_EXTNAME_EVT); 
	headas_chat(CHATTY,"%s: Error: IsThisEvDatamode: in '%s' file.\n",taskname, filename);
        goto Error; 
      }  
	
  *ret=KeyWordMatch(unit, KWNM_DATAMODE, S, datamode, NULL);

  if( CloseFitsFile(unit)) 
    {  
      headas_chat(NORMAL,"%s: Error: IsThisEvDatamode: Unable to close\n", taskname); 
      headas_chat(NORMAL,"%s: Error: IsThisEvDatamode: '%s' FITS file.\n", taskname,filename);
      goto Error;  
    }   

  /*  if(fits_close_file(unit, &status))
    {  
      headas_chat(CHATTY,"%s: Error: IsThisEvDatamode: Unable to close\n", taskname); 
      headas_chat(CHATTY,"%s: Error: IsThisEvDatamode: '%s' file.\n",taskname, filename);
      goto Error; 
    }  
  */
  return OK; 
  
 Error:

  CloseFitsFile(unit); 
  
  return NOT_OK; 
}/* IsThisDatamode */



BOOL ReadDatamode(char *filename, char *datamode)
{ 
  
  int            status=OK;
  char           taskname[FLEN_FILENAME];
  FitsCard_t     *card;
  FitsHeader_t   head;
  FitsFileUnit_t unit=NULL;

  get_toolnamev(taskname);       


  /* Open readonly input event file */
   if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
      {  
        headas_chat(NORMAL,"%s: Error: ReadDatamode: Unable to open\n", taskname); 
        headas_chat(NORMAL,"%s: Error: ReadDatamode: '%s' FITS file.\n", taskname,filename); 
        goto Error;
      } 
    
  /* Move in events extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status)) 
      {  
        headas_chat(CHATTY,"%s: Error: ReadDatamode: Unable to find  '%s' extension\n", taskname, KWVL_EXTNAME_EVT); 
	headas_chat(CHATTY,"%s: Error: ReadDatamode: in '%s' file.\n",taskname, filename);
        goto Error; 
      }  
	
  head=RetrieveFitsHeader(unit);

  if((ExistsKeyWord(&head, KWNM_DATAMODE  , &card)))
    {
      strcpy(datamode, card->u.SVal);
      headas_chat(CHATTY, "%s: Info: ReadDatamode: %s keyword sets to %s\n",taskname,  KWNM_DATAMODE,datamode);
      headas_chat(CHATTY, "%s: Info: ReadDatamode: in %s file.\n", taskname, filename);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: ReadDatamode: %s keyword not found\n", taskname,KWNM_DATAMODE);
      headas_chat(NORMAL, "%s: Error: ReadDatamode: in %s file.\n", taskname, filename);
      goto Error; 
    }

  if( CloseFitsFile(unit)) 
    {  
      headas_chat(NORMAL,"%s: Error: ReadDatamode: Unable to close\n", taskname); 
      headas_chat(NORMAL,"%s: Error: ReadDatamode: '%s' FITS file.\n", taskname,filename);
      goto Error;  
    }   
  return OK; 
  
 Error:

  CloseFitsFile(unit); 
  
  return NOT_OK; 
}/* ReadDatamode */
