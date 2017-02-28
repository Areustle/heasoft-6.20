/*
 
hextebackest.c
 
Derived from hextebackest.pro by K. Pottschmidt (kpottschmidt@ucsd.edu)
 
Input:
        Input pha FITS file name
        Output pha FITS file name
        Correction parameters FITS file
 (Hidden) Boolean option to create archive FITS file
 (Hidden) Version number
 
Usage:
        hextebackest
 This runs per the usual FTOOLS interface
 
 hextebackest mkarcfil=yes
 Runs in the usual fashion and generates an archive version of the file
 per R. Rothschild (rrothschild@ucsd.edu).  The generated file name is
 "arc_" + Output file name.
 
Compile with:
 
gcc -ansi -Wall -o hextebackest -I${HEADAS}/include hextebackest.c -L${HEADAS}/lib -lcfitsio -lape_1.0 -lm -lreadline -ltclreadline -ltcl8.4 -lhdutils_2.0
 
Modified:
TMG 2006-07-21  Change the interface to the standard FTOOLS parameter file interface.
                Add the hidden parameter option of generating and archive version of
                the file per R. Rothschild.
TMG 2006-08-30  Add and debug the use of CALDB as source for the correction parameters
                FITS file.
TMG 2006-08-30  Change code to use fits_get_system_time() for DATE keyword
TMG 2006-09-20  Fix code to make ANSI == ISO C90 compatible
TMG 2006-10-13  Change all TINT32BIT to TINT
MJT 2010-02-18  Ignore the lack of a QUALITY column (v1.1) and update checksum(s)

*/
#include <math.h>
#include <sys/types.h>
#ifdef __osf__
#include <db.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include "fitsio.h"
#include "pil.h"
#include "hdcal.h"

#define VERSION "1.1"

char allocatedFilenam[PIL_LINESIZE];
char allocatedOnline[16];

void printerror( int status);

int main(int argc, char* argv[])
{
  fitsfile *fpInput;
  char inputFileName[PIL_LINESIZE];
  fitsfile *fpOutput;
  char outputFileName[PIL_LINESIZE];
  fitsfile *fpCorr;
  char corrFileName[PIL_LINESIZE];
  int makeArchiveFile;
  char version[16];

  fitsfile *fpOutputArc;
  char outputFileNameArc[PIL_LINESIZE];

  short int channelCorr[256];
  double yoffset[256];
  double slope[256];

  int32_t channelData[256];
  int32_t channelDataArc[64];
  int32_t counts[256];
  int32_t outcounts[256];
  int32_t outcountsArc[64];
  int32_t *pOutcounts;
  int32_t sumOfCounts;
  float stat_err[256];
  float outerror[256];
  float outerrorArc[64];
  float *pOuterror;
  float sumOfSquaresOfErrors;
  float tf;
  short int quality[256];
  short int qualityArc[256];

  int status;
  int hdutype;
  int ii;
  int colnum;
  int anynulls;
  int pilStatus;
  long frow;
  long felem;
  long sixty_four;
  long sixty_three;
  short int sinullval;
  int32_t inullval;
  float fnullval;
  double dnullval;
  double exposure;
  char comment[256];

  /* Parameters for call to HDgtcalf */
  char *filenam[1];
  long int extno[1];
  char *online[1];
  int nret;
  int nfound;
  int retVal;

  char datestr[32];
  int timeref;
  char commentField[32];

  int havequal;
  havequal = 0;

  pOuterror = outerror;
  pOutcounts = outcounts;
  status = 0;
  frow = 1;
  felem = 1;
  sixty_four = 64;
  sixty_three = 63;
  sinullval = 0;
  inullval = 0;
  fnullval = 0.0;
  dnullval = 0.0;

  pilStatus = PILInit(argc, argv);
  if (pilStatus == PIL_OK)
  {
    pilStatus = PILGetString("version", version);
    if (pilStatus != PIL_OK)
    {
      (void)printf("\nError: PILGetString(\"version\", version) : %s\n", PIL_err_handler(pilStatus));
      exit( pilStatus );
    }
    /* Check version number */
    if ( strcmp(version, VERSION) )
    {
      (void)printf("\nError: Parameter version number %s not equal to program version number %s\n", version, VERSION);
      exit( -1 );
    }

    pilStatus = PILGetFname("infile", inputFileName);
    if (pilStatus != PIL_OK)
    {
      (void)printf("\nError: PILGetFname(\"infile\", inputFileName) : %s\n", PIL_err_handler(pilStatus));
      exit( pilStatus );
    }

    pilStatus = PILGetFname("outfile", outputFileName);
    if (pilStatus != PIL_OK)
    {
      (void)printf("\nError: PILGetFname(\"outfile\", outputFileName) : %s\n", PIL_err_handler(pilStatus));
      exit( pilStatus );
    }

    pilStatus = PILGetFname("corrfile", corrFileName);
    if (pilStatus != PIL_OK)
    {
      (void)printf("\nError: PILGetFname(\"corrfile\", corrFileName) : %s\n", PIL_err_handler(pilStatus));
      exit( pilStatus );
    }
    else
    {
      if (!strcasecmp(corrFileName, "CALDB"))
      {
        /* If called with CALDB as the correlation file, adjust the corrFileName */
        filenam[0] = allocatedFilenam;
        online[0] = allocatedOnline;

        retVal = HDgtcalf(
                   "XTE",
                   "HEXTE",
                   "-",
                   "-",
                   "CORR_COEFF",
                   "2006-07-13",
                   "00:00:00",
                   "now",
                   "now",
                   "-",
                   1,
                   PIL_LINESIZE,
                   filenam,
                   extno,
                   online,
                   &nret,
                   &nfound,
                   &status
                 );

        if ( status )
        {
          (void)printf("\nError: HDgtcalf(...)\n");
          (void)printf("       Most likely your CALDB environment is erroneous\n");
          printerror( status );
        }
        (void)strcpy(corrFileName, filenam[0]);
      }
      /* Open correction parameters file from (possibly corrected) corrFileName */
      if ( fits_open_file(&fpCorr, (const char *)corrFileName, READONLY, &status) )
      {
        (void)printf("\nError: fits_open_file(Correction parameters)\n");
        printerror( status );
      }

      pilStatus = PILGetBool("mkarcfil", &makeArchiveFile);
      if (pilStatus != PIL_OK)
      {
        (void)
        printf("\nError: PILGetBool(\"mkarcfil\", &makeArchiveFile) : %s\n", PIL_err_handler(pilStatus));
        exit( pilStatus );
      }
    }
  }
  else
  {
    (void)printf("Error: PILInit(argc, argv) : %s\n", PIL_err_handler(pilStatus));
    exit( pilStatus );
  }
  PILClose(pilStatus);
  /* Open input file */
  if ( fits_open_file(&fpInput, (const char *)inputFileName, READONLY, &status) )
  {
    (void)printf("\nError: fits_open_file(Input)\n");
    printerror( status );
  }

  /* Open output file */
  /* Check to make sure the outputFileName is not STDOUT/stdout */
  if (!strcasecmp(outputFileName, "STDOUT"))
  {
    (void)printf("\nError: strcasecmp(outputFileName, \"STDOUT\")\n");
    (void)printf("       \"STDOUT\" is not a valid output file name for this program\n");
    (void)printf("Program exiting...\n\n");
    exit( -1 );    /* terminate the program, returning a generic error status */
  }
  else
  {
    if ( fits_create_file(&fpOutput, (const char *)outputFileName, &status) )
    {
      (void)printf("\nError: fits_create_file(Output)\n");
      printerror( status );
    }
  }

  /* Read input file data */
  if (!(fits_movabs_hdu(fpInput, 2, &hdutype, &status) ))
  {
    fits_get_colnum(fpInput, CASEINSEN, "channel", &colnum, &status);
    fits_read_col(fpInput, TINT, colnum, frow, felem, 256, &inullval, channelData, &anynulls, &status);
    fits_get_colnum(fpInput, CASEINSEN, "counts", &colnum, &status);
    fits_read_col(fpInput, TINT, colnum, frow, felem, 256, &inullval, counts, &anynulls, &status);
    fits_get_colnum(fpInput, CASEINSEN, "stat_err", &colnum, &status);
    fits_read_col(fpInput, TFLOAT, colnum, frow, felem, 256, &fnullval, stat_err, &anynulls, &status);
    fits_get_colnum(fpInput, CASEINSEN, "quality", &colnum, &status);
    if (status==0){
      havequal = 1;
      fits_read_col(fpInput, TSHORT, colnum, frow, felem, 256, &sinullval, quality, &anynulls, &status);
    }else {status=0;} /* 18Feb2010 (M.Tripicco) ignore lack of QUALITY column */
    if (fits_read_key(fpInput, TDOUBLE, "EXPOSURE", &exposure, comment, &status))
    {
      (void)printf("\nError: fits_read_key(fpInput, \"EXPOSURE\") failed\n");
      printerror( status );
    }
  }
  else
  {
    (void)printf("Error: fits_movabs_hdu(fpInput, 2) failed\n");
    printerror( status );
  }

  /* Read correction parameters file data */
  if (!(fits_movabs_hdu(fpCorr, 2, &hdutype, &status) ))
  {
    fits_get_colnum(fpCorr, CASEINSEN, "channel", &colnum, &status);
    fits_read_col(fpCorr, TSHORT, colnum, frow, felem, 256, &sinullval, channelCorr, &anynulls, &status);
    fits_get_colnum(fpCorr, CASEINSEN, "yoffset", &colnum, &status);
    fits_read_col(fpCorr, TDOUBLE, colnum, frow, felem, 256, &dnullval, yoffset, &anynulls, &status);
    fits_get_colnum(fpCorr, CASEINSEN, "slope", &colnum, &status);
    fits_read_col(fpCorr, TDOUBLE, colnum, frow, felem, 256, &dnullval, slope, &anynulls, &status);
    fits_close_file(fpCorr, &status);
    for (ii=0; ii<256; ii++)
      if (isnan(yoffset[ii]))
        yoffset[ii] = (double)0.0;
    for (ii=0; ii<256; ii++)
      if (isnan(slope[ii]))
        slope[ii] = (double)0.0;
  }
  else
  {
    (void)printf("\nError: fits_movabs_hdu(fpCorr, 2) failed\n");
    printerror( status );
  }

  /* Now correct the spectrum and error stats */
  for (ii=0; ii<256; ii++)
  {
    outcounts[ii] = (int32_t)(slope[ii]*counts[ii] + exposure*yoffset[ii]);
    outerror[ii] = slope[ii]*stat_err[ii];
  }
  /* Write the output file from the arrays */
  /*   channelData  outcounts  outerror  quality */
  if ( fits_copy_header(fpInput, fpOutput, &status) )
  {
    (void)printf("\nError: fits_copy_header(Input, Output)\n");
    printerror( status );
  }
  fits_get_colnum(fpInput, CASEINSEN, "channel", &colnum, &status);
  fits_write_col(fpOutput, TINT, colnum, frow, felem, 256, channelData, &status);
  fits_get_colnum(fpInput, CASEINSEN, "counts", &colnum, &status);
  fits_write_col(fpOutput, TINT, colnum, frow, felem, 256, outcounts, &status);
  fits_get_colnum(fpInput, CASEINSEN, "stat_err", &colnum, &status);
  fits_write_col(fpOutput, TFLOAT, colnum, frow, felem, 256, outerror, &status);
  if (havequal){
    fits_get_colnum(fpInput, CASEINSEN, "quality", &colnum, &status);
    fits_write_col(fpOutput, TSHORT, colnum, frow, felem, 256, quality, &status);
  }

  /* Add/Update keys */
  if ( fits_update_key(fpOutput, TSTRING, "INSTRUME", "HEXTE", "instrument name", &status))
  {
    (void)printf("\nError: fits_update_key(\"INSTRUME\")\n");
    printerror( status );
  }
  if ( fits_update_key(fpOutput, TSTRING, "CREATOR", "hextebackest", "program that produced this file", &status))
  {
    (void)printf("\nError: fits_update_key(\"CREATOR\")\n");
    printerror( status );
  }
  /* Create the DATE/TIME string */
  if ( fits_get_system_time(datestr, &timeref, &status))
  {
    (void)printf("\nError: fits_get_system_time()\n");
    printerror( status );
  }

  (void)strcpy(commentField, "file creation date");
  if (timeref)
  {
    (void)strcat(commentField, " Local Time");
  }
  else
  {
    (void)strcat(commentField, " UTC");
  }
  if ( fits_update_key(fpOutput, TSTRING, "DATE", datestr, commentField, &status))
  {
    (void)printf("\nError: fits_update_key(\"DATE\")\n");
    printerror( status );
  }

  if ( fits_flush_file(fpOutput, &status) )
  {
    (void)printf("\nError: fits_flush_file(Output)\n");
    printerror( status );
  }

  if (makeArchiveFile != 0)
  {
    sumOfCounts = 0;
    sumOfSquaresOfErrors = 0.0;
    for (ii=0; ii<32; ii++)
    {
      channelDataArc[ii] = ii;
      if (havequal) {qualityArc[ii] = quality[ii];}
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      outcountsArc[ii] = sumOfCounts;
      sumOfCounts = 0;

      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      outerrorArc[ii] = sqrt(sumOfSquaresOfErrors);
      sumOfSquaresOfErrors = 0;
    }
    for (ii=32; ii<48; ii++)
    {
      channelDataArc[ii] = ii;
      if (havequal) {qualityArc[ii] = quality[ii];}
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      outcountsArc[ii] = sumOfCounts;
      sumOfCounts = 0;

      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      outerrorArc[ii] = sqrt(sumOfSquaresOfErrors);
      sumOfSquaresOfErrors = 0;
    }
    for (ii=48; ii<64; ii++)
    {
      channelDataArc[ii] = ii;
      if (havequal) {qualityArc[ii] = quality[ii];}
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      sumOfCounts += *pOutcounts++;
      outcountsArc[ii] = sumOfCounts;
      sumOfCounts = 0;

      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      tf = *pOuterror++;
      sumOfSquaresOfErrors += (tf * tf);
      outerrorArc[ii] = sqrt(sumOfSquaresOfErrors);
      sumOfSquaresOfErrors = 0;

    }
    /* Now write out the archive file */
    /* First create the file name from the standard output file name selected */
    (void)strcpy(outputFileNameArc, "arc_");
    (void)strcat(outputFileNameArc, outputFileName);
    (void)printf("\nCreating archive file %s\n\n", outputFileNameArc);
    if ( fits_create_file(&fpOutputArc, (const char *)outputFileNameArc, &status) )
    {
      (void)printf("\nError: fits_create_file(Output)\n");
      printerror( status );
    }
    if ( fits_copy_header(fpOutput, fpOutputArc, &status) )
    {
      (void)printf("\nError: fits_copy_header(Input, OutputArc)\n");
      printerror( status );
    }
    fits_get_colnum(fpInput, CASEINSEN, "channel", &colnum, &status);
    fits_write_col(fpOutputArc, TINT, colnum, frow, felem, 64, channelDataArc, &status);
    fits_get_colnum(fpInput, CASEINSEN, "counts", &colnum, &status);
    fits_write_col(fpOutputArc, TINT, colnum, frow, felem, 64, outcountsArc, &status);
    fits_get_colnum(fpInput, CASEINSEN, "stat_err", &colnum, &status);
    fits_write_col(fpOutputArc, TFLOAT, colnum, frow, felem, 64, outerrorArc, &status);
    if (havequal){
      fits_get_colnum(fpInput, CASEINSEN, "quality", &colnum, &status);
      fits_write_col(fpOutputArc, TSHORT, colnum, frow, felem, 64, qualityArc, &status);
    }

    /* Add/Update keys */
    if ( fits_update_key(fpOutputArc, TLONG, "NAXIS2", &sixty_four, "number of rows in table", &status))
    {
      (void)printf("\nError: fpOutputArc: fits_update_key(\"NAXIS2\")\n");
      printerror( status );
    }
    if ( fits_update_key(fpOutputArc, TLONG, "TLMAX1", &sixty_three, "Highest legal channel number", &status))
    {
      (void)printf("\nError: fpOutputArc: fits_update_key(\"TLMAX1\")\n");
      printerror( status );
    }
    if ( fits_update_key(fpOutputArc, TLONG, "DETCHANS", &sixty_four, "total number possible channels", &status))
    {
      (void)printf("\nError: fpOutputArc: fits_update_key(\"DETCHANS\")\n");
      printerror( status );
    }

    if ( fits_flush_file(fpOutputArc, &status) )
    {
      (void)printf("\nError: fits_flush_file(OutputArc)\n");
      printerror( status );
    }
    fits_write_chksum(fpOutputArc, &status);
  }

  fits_write_chksum(fpOutput, &status);

  fits_close_file(fpInput, &status);
  fits_close_file(fpOutput, &status);
  fits_close_file(fpOutputArc, &status);

  return 0;
}

void printerror( int status)
{
  /*****************************************************/
  /* Print out cfitsio error messages and exit program */
  /*****************************************************/
  if (status)
  {
    fits_report_error(stderr, status); /* print error report */
    exit( status );    /* terminate the program, returning error status */
  }
  return;
}
