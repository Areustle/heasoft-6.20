#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
#include "Cheasp.h"
#include "headas_copykeys.h"

#define MAXFLD 999
#define MAXMSG 256

typedef enum sprbnarf_error_code {
     BASE_ERROR = 2000,
     PARSE_ERROR
} sprbnarf_error_code;

       

/*
HISTORY
-------
  Version 1.0 written by Randall Smith, SAO, November 2009
*/

#define TOOLSUB rsp2rmfarf
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int rsp2rmfarf (void);
int rsp2rmfarf_getpar (char* file_rsp, char* file_rmf, char* file_arf);
int rsp2rmfarf_work (char* file_rsp, char* file_rmf, char* file_arf);
char* generate_filename(char *Infile, char *Suffix);

/*---------------------------------------------------------------------------*/
int rsp2rmfarf (void)
{
/*  Split a RSP file into a REDIST-type RMF and an ARF containing the effective
    area.
*/

  char file_rsp[PIL_LINESIZE];
  char file_rmf[PIL_LINESIZE], file_arf[PIL_LINESIZE];
  int status;

  static char taskname[80] = "rsp2rmfarf";
  static char version[8] = "1.00";

  /* Register taskname and version. */
  
  set_toolname(taskname);
  set_toolversion(version);
  
  /*  get input parameters */
  status = rsp2rmfarf_getpar (file_rsp, file_rmf, file_arf);

  /* call work function to read, factor, and write out the files */
  if (!status) {
    status = rsp2rmfarf_work (file_rsp, file_rmf, file_arf);
  }
  
  return(status);
}

/*---------------------------------------------------------------------------*/
int rsp2rmfarf_getpar(
    char* file_rsp,     /* O - Name of input RSP file */ 
    char* file_rmf,     /* O - Name of output RMF file */ 
    char* file_arf      /* O - Name of output ARF file */ 
    )

/*  read input parameters for the rsp2rmfarf task from the .par file */
{
    int status=0;
    char msg[MAXMSG];
    char *suffix_pos;

    if ((status = PILGetFname("file_rsp", file_rsp))) {
      sprintf(msg, "Error reading the 'file_rsp' parameter.");
      HD_ERROR_THROW(msg,status);
      return(status);
    }

    if ((status = PILGetString("file_rmf", file_rmf))) {
      sprintf(msg, "Error reading the 'file_rmf' parameter.");
      HD_ERROR_THROW(msg,status);
      return(status);
    }

    if ( strcmp(file_rmf,"-") == 0 ) { /* Generate one */
      suffix_pos = strstr(file_rsp,".rsp");
      if ((suffix_pos == NULL)||(strlen(suffix_pos)!=4)) {
	sprintf(file_rmf,"%s.rmf",file_rsp);
      } else {
	strncpy(file_rmf, file_rsp, strlen(file_rsp)-4);
	sprintf(file_rmf,"%s.rmf",file_rmf);
      }
    }

    if ((status = PILGetString("file_arf", file_arf))) {
      sprintf(msg, "Error reading the 'outfile' parameter.");
      HD_ERROR_THROW(msg,status);
      return(status);
    }

    if ( strcmp(file_arf,"-") == 0 ) { /* Generate one */
      suffix_pos = strstr(file_rsp,".rsp");
      if ((suffix_pos == NULL)||(strlen(suffix_pos)!=4)) {
	sprintf(file_arf,"%s.arf",file_rsp);
      } else {
	strncpy(file_arf, file_rsp, strlen(file_rsp)-4);
	sprintf(file_arf,"%s.arf",file_arf);
      }
    }

    return(status);
}
/*---------------------------------------------------------------------------*/
    int rsp2rmfarf_work(
    char* file_rsp,     /* I - Name of input ARF file */ 
    char* file_rmf,    /* I - name of ASCII binning factor file */ 
    char* file_arf     /* I - name of output FITS file */
    )

{

  int status=0;
  int n;
  long ii, jj, kk;
  struct RMF *RSP;
  struct ARF *outputARF;
  float effarea;

  /* read in the input RSP */

  RSP = (struct RMF *) malloc(sizeof(struct RMF));

  if ( (status = ReadRMFEbounds(file_rsp, 1, RSP)) ) {
    headas_chat(1, "*** Failed to read EBOUNDS from %s\n", file_rsp);
    return(status);
  }

  if ( (status = ReadRMFMatrix(file_rsp, 1, RSP)) ) {
    headas_chat(1, "*** Failed to read MATRIX from %s\n", file_rsp);
    return(status);
  }

  /* Split this into a REDISTribution RMF and an effective area file */

  /* Step 1: Create the ARF structure */

  outputARF = (struct ARF *) malloc(sizeof(struct ARF));

  n = RSP->NumberEnergyBins;
  outputARF->LowEnergy = (float *) malloc(n*sizeof(float));
  outputARF->HighEnergy = (float *) malloc(n*sizeof(float));
  outputARF->EffArea = (float *) malloc(n*sizeof(float));
  outputARF->NumberEnergyBins = n;

  for (ii=0;ii<n;ii++) {
    outputARF->LowEnergy[ii]  = RSP->LowEnergy[ii];
    outputARF->HighEnergy[ii] = RSP->HighEnergy[ii];
    outputARF->EffArea[ii] = 0.0;
  }

  strcpy(outputARF->Telescope, RSP->Telescope); 
  strcpy(outputARF->Instrument,RSP->Instrument); 
  strcpy(outputARF->Detector,  RSP->Detector); 
  strcpy(outputARF->Filter,    RSP->Filter); 

  strcpy(outputARF->EnergyUnits, RSP->EnergyUnits); 
  strcpy(outputARF->arfUnits, RSP->RMFUnits); 
  strcpy(RSP->RMFUnits, " "); 

  strcpy(outputARF->ARFVersion, "1.1.0"); 
  strcpy(outputARF->ARFExtensionName, "SPECRESP"); 

  for (ii=0; ii<RSP->NumberEnergyBins; ii++) {

    effarea = 0.0;

    /* loop round response groups for this energy */
    for (jj=RSP->FirstGroup[ii]; 
	 jj<RSP->FirstGroup[ii]+RSP->NumberGroups[ii]; jj++) {

      /* loop round matrix elements for this response group */

      for (kk=0; kk<RSP->NumberChannelGroups[jj]; kk++) {
        effarea += RSP->Matrix[kk + RSP->FirstElement[jj]];
      }
    }

    outputARF->EffArea[ii] = effarea;
  }

  /* Now just normalize the RMF... */
  NormalizeRMF(RSP);
  
  /* And fix the RMFType parameter to be REDIST */
  strcpy(RSP->RMFType,"REDIST");

  /* And write out the files. */

  /* First create the output ARF file */

  if ( (status = WriteARF(file_arf, outputARF)) ) {
    headas_chat(1, "*** Failed to write ARF to %s\n", file_arf);
    return(status);
  }

  /* Now create the output RMF file */

  if ( (status = WriteRMFEbounds(file_rmf, RSP)) ) {
    headas_chat(1, "*** Failed to write RMF EBOUNDS to %s\n", file_rmf);
    return(status);
  }

  if ( (status = WriteRMFMatrix(file_rmf, RSP)) ) {
    headas_chat(1, "*** Failed to write RMF Matrix to %s\n", file_rmf);
    return(status);
  }

  return(status);
}


char* generate_filename(char *Infile, char *Suffix) {

  /* Keep it simple.  Find last /, start counting from there until end of */
  /* string, or if last 4 charcters are .rsp replace with Suffix.  If not, */
  /* just paste suffix on the end. */

  int ii;
  int init_pos, final_pos, length;
  char *Outfile;

  init_pos = 0;
  final_pos = strlen(Infile)-1;

  for (ii=0; ii<strlen(Infile);ii++) {
    if (Infile[ii] == '/') init_pos = ii;
  }
  
  if (strncmp(&(Infile[final_pos-3]),".rsp",4) == 0) {
    final_pos = final_pos-3;
  }

  length = final_pos - init_pos + 4 + 1;

  Outfile = (char *) malloc(length*sizeof(char));
  
  for(ii=init_pos;ii<final_pos;ii++) Outfile[ii-init_pos] = Infile[ii];
  for(ii=final_pos;ii<final_pos+4;ii++) 
    Outfile[ii-init_pos] = Suffix[ii-final_pos];

  return Outfile;
}
