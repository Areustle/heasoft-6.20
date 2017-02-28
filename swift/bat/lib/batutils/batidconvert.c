#include <string.h>
#include <stdio.h>
#include "bat_gswdev.h"

/* CM 02 Jan 2003 - Remove unnecessary MAX_BUFSIZE in array declaration;
                    copied into batmaskutils library */

/* HAK 17-Sep-2002
This code converts from a BAT detid (DET_ID column in the Events 
FITS file) to block, DM, detector and calculates the row and column
values for each detector. 
    This is not an FTOOL since it does not work directly with FITS
    files.  It has been successfully compiled as part of an FTOOL
    and when called by an ordinary C program. 
Disclaimer: I have spot checked the conversion to row/column, 
but I have not yet exhaustively tested that it is correct in all cases. */

  /* HAK 4-Sep-2002 
     Code to convert from detid to block/dm/det and row/column
     NOTE: block runs from 0 to 15
     NOTE: dm runs from 0 to 15
     NOTE: det runs from 0 to 127
     NOTE: row runs from 0 to 172 (rows run parallel to batX axis)
     NOTE: column runs from 0 to 285 (columns run parallel to batY axis

     row172  Block0  Block1  Block2  Block3  Block4  Block5  Block6  Block7
             |    |  |    |  |    |  |    |  |    |  |    |  |    |  |    |
     Row88   |____|  |____|  |____|  |____|  |____|  |____|  |____|  |____|
     row84   Block8  Block9  Block10 Block11 Block12 Block13 Block14 Block15
             |    |  |    |  |    |  |    |  |    |  |    |  |    |  |    |
     row0    |____|  |____|  |____|  |____|  |____|  |____|  |____|  |____|
             col0    col36   col72   col108  col144  col180  col216  col252
	     columns listed at the left side of the blocks
  */

void batidconvert(bufsize,detid,block,dm,det,row,col)
     int bufsize;
     unsigned short int *detid;
     short int *block;
     short int *dm;
     short int *det;
     short int *row;
     short int *col;
 
{

 int i;
 int row_start,col_start;
 int block_shift;

for (i=0;i<bufsize;i++) {
  /* First figure out the block/dm/detector values */
  det[i]=detid[i] & 0x7f;
  dm[i]=(detid[i] >> 7) & 0xF;
  block[i]=detid[i] >> 11;

  /* Now figure out the row and column */
  /* First find the row of the upper left (starred) corner of
     the block (dm 1, det 127)*/
  row_start = DAP_ROWS-1;
  /* Next figure out the row of detector 127 in the DM */
  row_start -= ((dm[i] % 8) - 2*(dm[i] % 2) + 1)*(SANDWICH_ROWS+GAP_ROWS);
  /* Next figure out the row of detector det in the DM */
  row_start -= (7*(det[i] >> 6) + (-2*(det[i] >> 6)+1) * (det[i] % 8));

  /* First find the column of the upper left (starred) corner of
     the block (dm 1, det 127)*/
  block_shift = block[i] >> 3; /* 0 for blocks 0-7 ; 1 for blocks 8-15 */
  col_start = (15*block_shift + (-2*block_shift+1) * block[i])*(DAP_COLS/2+1)/4;
  /* Next figure out the column of detector 127 in the DM */
  col_start += (dm[i] >> 3)*(SANDWICH_COLS+GAP_COLS);
  /* Next figure out the column of detector det in the DM */
  col_start += (15 - (det[i] >> 3));

  /* Finally flip blocks 8-15 */
  row[i] = (DAP_ROWS-1)*block_shift + row_start * (1-2*block_shift);
  col[i] = (DAP_COLS-1)*block_shift + col_start * (1-2*block_shift);

} 


}
