
/**************************************************
 Check that the time of two event words is the same 
****************************************************/
int Check_Time(a3, b3)
     unsigned short a3, b3;
{
   int atime= a3;
   int btime= b3;
   if (a3 < 0x8000) /* Interval constant has time bin 0 */
     atime = 0;
   else
     atime= a3 & 0xfff;
 
   if (b3 < 0x8000)
     btime = 0;
   else
     btime= b3 & 0xfff;
   if (atime == btime)
     return(1);
   if (abs(atime - btime) == 1)
     return(1);
   if (abs(atime - btime) == 0xfff)
     return(1);
   return(0);

/*  int atime= a3 & 0xfff;
    int btime= b3 & 0xfff;
    if (atime == btime)
    return(1);
    if (abs(atime - btime) == 1)
    return(1);
    if (abs(atime - btime) == 0xfff)
    return(1);
  return(0); */
}
