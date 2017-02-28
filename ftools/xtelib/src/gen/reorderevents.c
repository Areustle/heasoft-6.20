
/*************************************************************
 If 2 or more events are seen by the EDS within ~1 ns
   the order of the events, as seen by different EAs
   may be different. There can be up to 6 events out
   of order (including interval marks). This function
   uses the PCU ids to reshuffle events and put them
   in the same order 

29Jun99 (MJT) changed call to Check_Time() as per email
              from C.Markwardt & E.Morgan
*****************************************************/
#include <stdio.h>
#include <errno.h>

int Reorder_Events(eventLists, index, ifirst, isecond)
    unsigned short **eventLists, index;
    int ifirst,isecond;
{
  unsigned short events[10][10];
  int id;
  int jj, ii;
  for ( ii = 0 ; ii < ifirst ; ii ++)
    for (jj = 0 ; jj < isecond ; jj ++) 
      events[ii][jj] = eventLists[jj][index + ii];
  id = PCU_ID(events[0][0]);
  ii = 0;
  for (jj =1 ; jj < isecond ; jj++)
    if (PCU_ID(events[0][jj]) != id)
      for (ii = 1; ii < (ifirst-1) ; ii++)
	if (PCU_ID(events[ii][jj]) == id)
	{
	  /*if (Check_Time(events[0][2],events[ii][2])) */
          /* MJT 29Jun99: [2] is for trans only, should be [1] for xenon! */
	  if (Check_Time(events[0][isecond-1],events[ii][isecond-1]))
	  {
	    eventLists[jj][index] = events[ii][jj];
	    eventLists[jj][index + ii] = events[0][jj];
	  }
	}
  for (jj = 0; jj < isecond; jj++)
    if (PCU_ID(eventLists[jj][index]) != id){
      fprintf(stdout,"Switching failed %i %i %i \n", jj, id, eventLists[jj][index]);
      return(0);
    }
  return(1);
}
