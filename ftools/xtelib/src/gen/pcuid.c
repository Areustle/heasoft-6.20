
/****************************************************************
 Returns the PCU Id from an EDS event word
   All event words have the PCU ID in bits 12-14 
*****************************************************************/
int PCU_ID(event_word)
     unsigned short event_word;
{
  if ((event_word & 0x8000) != 0)
    return(((event_word & 0x7000) >> 12) + 0);
  return(-1);
}
