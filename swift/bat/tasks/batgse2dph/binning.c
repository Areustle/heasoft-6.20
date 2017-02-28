/* binning.c */
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "bat_gswdev.h" /* "bat_read_calfiles" and GAIN_STRUCTURE */
#include "batgse2dph.h"

#define min(x,y) ((x<y)?(x):(y))
#define max(x,y) ((x>y)?(x):(y))

/* -------------------------------------------------------------------*/
/* rebin_spectrum rebins a spectrum by distributing counts from a     */
/* single bin into multiple bins, if necessary                        */
/* -------------------------------------------------------------------*/
int
rebin_spectrum(spectrum *oldspec, spectrum *newspec){

  /* This function rebins spectra by redistributing counts
   * to produce a spectrum that doesn't add noise */

  /* initialize the variables */

  float *OldEdges;
  float *NewEdges;

  int FirstNewBin,OldBin,NumNew,NewBin,i;
  int ascending=1,descending=1;
  float frac;
  float OldBinWidth;
  float *TempOldHist;
  float *TempOldEdges;

  /* allocate memory */

  OldEdges = (float *)(malloc((oldspec->size+1)*sizeof(float)));
  NewEdges = (float *)(malloc((newspec->size+1)*sizeof(float)));
  TempOldHist = (float *)(malloc(oldspec->size*sizeof(float)));
  TempOldEdges = (float *)(malloc(oldspec->size*sizeof(float)));

  headas_chat(5,"Inside rebin_spectrum...\n");

  for (i=0;i<oldspec->size;i++) OldEdges[i]=oldspec->e_min[i];
  OldEdges[oldspec->size+1]=oldspec->e_max[i];
  for (i=0;i<newspec->size;i++) NewEdges[i]=newspec->e_min[i];
  NewEdges[newspec->size+1]=newspec->e_max[i];

  /* make sure OldEdges are in ascending or descending order 
   * (if not, exit) */

  for (i=0;i<oldspec->size;i++) {
    if (OldEdges[i]<=OldEdges[i+1]) descending=0;
    if (OldEdges[i]>=OldEdges[i+1]) ascending=0;
  }

  if (ascending) headas_chat(4,"Old edges are in ascending order\n");
  if (descending) headas_chat(4,"Old edges are in descending order\n");
  if ((!ascending)&&(!descending)) {
    fprintf(stderr,"ERROR: Old edges are neither ascending or descending\n");
    return 1;
  }

  /* initialize TempOldEdges and TempOldHist
   * (so that they are in descending order even if OldEdges is in
   * ascending order) */

  headas_chat(5,"Initializing TempOldEdges and TempOldHist\n");
  if (ascending) {
    for (i=0;i<oldspec->size;i++)
      TempOldHist[i]=oldspec->hist[oldspec->size-1-i];
    for (i=0;i<oldspec->size+1;i++)
      TempOldEdges[i]=OldEdges[oldspec->size-i];
  }
  else {
    for (i=0;i<oldspec->size;i++)
      TempOldHist[i]=oldspec->hist[i];
      for (i=0;i<oldspec->size+1;i++)
        TempOldEdges[i]=OldEdges[i];
  }

  /* make sure NewEdges are in ascending order 
   * (if not, exit) */

  headas_chat(5,"Checking NewEdges\n");

  for (i=0;i<newspec->size;i++)
    if (NewEdges[i]>=NewEdges[i+1]) {
      fprintf(stderr,"ERROR: NewEdges are not in ascending order\n");
      return 2;
    }

  /* Initialize newspec->hist */

  headas_chat(5,"initializing newpsec->hist\n");
  for(i=0;i<newspec->size;i++) newspec->hist[i]=0;

  /* Initialize FirstNewBin */

  headas_chat(5,"initializing FirstNewBin\n");
  FirstNewBin=newspec->size;

  headas_chat(5,"Go through each old bin...\n");
  for (OldBin=0; OldBin<oldspec->size; OldBin++) {

    /* Find OldBinWidth */

    OldBinWidth=TempOldEdges[OldBin]-TempOldEdges[OldBin+1];
    headas_chat(5,"Old Bin Width: %f\n",OldBinWidth);

    /* Find FirstNewBin
     *    (The lowest newspec bin that will receive counts from the oldspec bin)
     */

    while ((NewEdges[FirstNewBin]>TempOldEdges[OldBin+1])&&
        (FirstNewBin>0)) FirstNewBin--;
    headas_chat(5,"FirstNewBin: %d\n",FirstNewBin);

    /* Find NumNew
     *    (the number of newspec bins that will receive counts from the
     *    oldspec bin) */

    NumNew=0;
    while ((NewEdges[FirstNewBin+NumNew]<TempOldEdges[OldBin])&&
        (FirstNewBin+NumNew<newspec->size)) NumNew++;
    headas_chat(5,"NumNew: %d\n",NumNew);

    /* Place the counts in newspec->hist */
	
    for (NewBin=FirstNewBin; NewBin<FirstNewBin+NumNew;
        NewBin++) {

      headas_chat(5,"NewBin: %d\n",NewBin);
      frac=(min(NewEdges[NewBin+1],TempOldEdges[OldBin])-
          max(NewEdges[NewBin],TempOldEdges[OldBin+1]))/
        OldBinWidth;
      headas_chat(5,"frac: %f\n",frac);

      newspec->hist[NewBin]+=frac*TempOldHist[OldBin];

    }
  }

  /* free memory */

  free(OldEdges);
  free(NewEdges);
  free(TempOldHist);
  free(TempOldEdges);

  return 0;
}


/* -------------------------------------------------------------------
 * bin_spectrum rebins a spectrum by placing all counts in a single
 * bin into a new bin.
 *
 * This is simpler than the technique used by rebin_spectrum,
 * but it adds noise if the oldspec bin size is not much smaller
 * than the newspec bin size.
 * -------------------------------------------------------------------*/
int
bin_spectrum(spectrum *oldspec, spectrum *newspec){

  int i, current_bin;
  
  headas_chat(5,"Inside bin_spectrum...\n");

  /* initialize newspec->hist */

  headas_chat(5,"Ininitializing newspec->hist\n");
  for (i=0;i<newspec->size;i++) newspec->hist[i]=0;

  /* rebin oldspec->hist */

  /* first newspec bin that counts might go into is the highest bin,
   * since the oldspec hist is reversed */

  current_bin=newspec->size-1;

  /* go through each oldspec bin */

  for (i=0;i<oldspec->size;i++)
  {

    headas_chat(4,"oldspec bin: %d\n",i);
    headas_chat(4,"energy of this bin: %f\n",oldspec->e_cent[i]);

    /* make sure the energy is within newspec's range */

    if ((oldspec->e_cent[i]<newspec->e_max[newspec->size-1])&&
	(oldspec->e_cent[i]>newspec->e_min[0]))
    {

      /* find the newspec bin that corresponds to e */

      while (oldspec->e_cent[i]<newspec->e_min[current_bin]) current_bin--;
      headas_chat(4,"current bin: %d\n",current_bin);

      /* place the contents of the oldspec bin in the newspec bin */

      newspec->hist[current_bin]+=oldspec->hist[i];
      headas_chat(4,"current bin contents: %f\n",newspec->hist[current_bin]);
    }
  }
  headas_chat(5,"...leaving bin_spectrum\n");

  return 0;
}
