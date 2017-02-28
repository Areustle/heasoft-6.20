/*>    ROUTINE LOCF
  CERN PROGLIB# N100    LOCF            .VERSION KERNVMI  1.08  930527
  ORIG. 11/05/93, JZ
*/
unsigned int locf_(iadr)
   char *iadr;
{
   return( ((unsigned long) iadr) >> 2 );
}
