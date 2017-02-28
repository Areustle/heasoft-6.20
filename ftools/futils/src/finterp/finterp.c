/******************************************************************************
Taskname:    finterp

Filename:    finterp.c

Description: Utility to augment a table (A) with a column (C) of data from
    a different table (B). If not already present, a new column of the same
    format as column C is appended to table A. Next a value is written to
    each row of C, based on a comparison of sort key columns from tables A
    and B. The value written for string and logical columns is copied from
    column C. For numerical columns, there is also the option of a linear
    interpolation of values from column C.

Author/Date: James Peachey, HSTX, 4/96

Modification History:
    Craig Markwardt, GSFC, 2015-12-28
         BUG FIX: update odata->nul[] properly so that extraneous
            NULLs are not written
         BUG FIX: handle integers with TSCALn/TZEROn keywords properly
         BUG FIX/ENHANCEMENT: copy all standard FITS tabular keywords,
            also standard WCS pixel list keywords.
         Clean up to prevent compiler warning messages

    James Peachey, RITSS, 2/8/1999
         When WriteArray is called to write a page to the output
         file, make sure that the number of rows to write does not 
         go past the end of the table.

    PDW, RSTX, 6/98
         * Due to new extended filenames syntax, must strip off infile's
           extension number before comparing to outfile.

Notes:

Functions implemented in this file:
    Finterp: "main" function 
    ginterp: interrogate parameter file
    CopyThruExt: copy a fitsfile, up to and including the specified extension
    AppendColFormat: table to table column copy, format and keywords only
    CheckTime: correct for differences in time origins
    Interpolate: perform the actual interpolation
    strip: remove leading and trailing spaces, tabs, commas, quotes

The following functions work specifically with finterp_array structures.
See finterp.h for definition of the finterp_array structure:
    InitArray: set up dynamic array of proper data type
    FreeArray: free up dynamic arrays when calculation is complete
    ReadArray: read a page of values from input file into array
    CopyArray: copy a series of values from one array to another
    InterpArray: perform a linear interpolation on a particular array
        element
    FillArray: copy a value from one array element to a series of array
        elements
    SetArrayNull: set elements in an array to undefined
    WriteArray: write a page of values to output file
******************************************************************************/

#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "fitsio2.h"     /* cfitsio defined constants */
#include "xpi.h"         /* parameter file functions, e.g. Uclgst */
#include "cftools.h"     /* standard C library constants */
#include "finterp.h"     /* task specific definitions */

#define MAXHDU 100000

/******************************************************************************
Function:    Finterp

Description: Main function in this ftool. Interrogates .par file, checks
             parameters for validity. Opens input and output files,
             finds proper extensions. Handles the following options/
             parameters:
                 calls CopyThruExt if user is not interpolating "in place"
                     to copy the destination file
                 calls CheckTime if tcheck == yes to calculate offset due
                     to different time origins
                 calls Interpolate to perform the actual interpolation

Author/Date: James Peachey, HSTX, 4/96

Modification History:
    James Peachey, RITSS, 2/8/1999
         When WriteArray is called to write a page to the output
         file, make sure that the number of rows to write does not 
         go past the end of the table.

    PDW, RSTX, 6/98
         * Due to new extended filenames syntax, must strip off infile's
           extension number before comparing to outfile.

Notes:

Primary Local Variables:
    fitsfile *infp1 - fitsfile with table to augment
    fitsfile *infp2 - fitsfile with column of data to add
    fitsfile *outfp - output fits file - may be same as infp1
    int sortcol1, sortcol2 - numbers of the columns in input used for sort
    int incol - column number being added
    int outcol - column number of output file
    double toffset - value used to reconcile different input file time origins
    int order - order of the interpolation, found in .par file
    int extrap - reflects user's choice of handling out of bounds values
    char *strnull - string used for null values in ascii tables
    int intnull - integer used for null values in binary tables with integer
        data types
    int isoldcol - a flag indicating whether column already exists in outfile

Functions called:
local:
    ginterp: interrogate parameter file
    CopyThruExt: copy a fitsfile, up to and including the specified extension
    AppendColFormat: table to table column copy, format and keywords only
    CheckTime: correct for differences in time origins
    Interpolate: perform the actual interpolation
libcftools.a:
    c_fcpars: utility to parse file names
    c_fcerr: write to STDERR
libcfitsio.a 
    ffXXXX: cfitsio library
******************************************************************************/

void Finterp()
{
/* parameter file variables */
    char infile1[FNTRP_CHAR_BUF], infile2[FNTRP_CHAR_BUF],
         outfile[FNTRP_CHAR_BUF], incolnames[FNTRP_CHAR_BUF],
         sortkey1[FNTRP_CHAR_BUF], sortkey2[FNTRP_CHAR_BUF],
         strnull[FNTRP_CHAR_BUF];
    char incolname[FNTRP_CHAR_BUF];
    int  tcheck = FALSE, order = -1, extrap = -1;
    int  intnull = 0;

/* data stream variables */
    fitsfile *infp1 = NULL, *infp2 = NULL, *outfp = NULL;
    int      inext1 = 0, inext2 = 0, outext = 0, 
             inopen1 = FALSE, inopen2 = FALSE, outopen = FALSE,
             inhdutype2 = 0, outhdutype = 0, exttype = 0,
             sortcol1 = 0, sortcol2 = 0, incol = 0, outcol = 0;
    double   toffset = 0.0;
    int icol;

/* general variables */
    int  status = 0, isoldcol = FALSE;
    char temp[FNTRP_CHAR_BUF] = "",
         context[C_FCERR_MSG] = "";


    c_ptaskn("finterp"); /* name of task used by c_fcerr */

/* interrogate finterp.par, parse filenames */
    ginterp(infile1, infile2, outfile, incolnames, sortkey1, sortkey2,
        strnull, &intnull, &extrap, &tcheck, &order, &status);
    if(status) return;/* ginterp handles its own error messages */
    c_fcpars(infile1, temp, &inext1, &status);
    strcpy(infile1, temp);
    c_fcpars(infile2, temp, &inext2, &status);
    strcpy(infile2, temp);
    c_fcpars(outfile, temp, &outext, &status);
    strcpy(outfile, temp);
    outext = inext1;
    if(status)
    {
        strcpy(context, "Error occurred while parsing filenames");
        goto Done;
    }
    if((inext1 < 1) || (inext1 > MAXHDU))
    {
        status = FNTRP_BAD_PAR;
        sprintf(context, "Illegal extension (%d) given for file %s",
                inext1, infile1);
        goto Done;
    }
    if((inext2 < 1) || (inext2 > MAXHDU))
    {
        status = FNTRP_BAD_PAR;
        sprintf(context, "Illegal extension (%d) given for file %s",
                inext2, infile2);
        goto Done;
    }

/* interrogate infile2, and find infile2's sort and append columns */
    if(ffopen(&infp2, infile2, READONLY, &status))
        goto Done;
    else inopen2 = TRUE;
    if(ffmahd(infp2, inext2 + 1, &inhdutype2, &status))
    {
        sprintf(context, "Cannot find extension %d in file %s",
                inext2, infile2);
        goto Done;
    }
    if(ffgcno(infp2, CASEINSEN, sortkey2, &sortcol2, &status))
    {
        sprintf(context, "Cannot find column named %s in file %s",
                sortkey2, infile2);
        goto Done;
    }

    for (icol=0; icol < NumListItems(incolnames); icol++) {
      CopyListItem(incolnames, icol, incolname);
      if(ffgcno(infp2, CASEINSEN, incolname, &incol, &status))
	{
	  sprintf(context, "Cannot find column named %s in file %s",
		  incolname, infile2);
	  goto Done;
	}
    }

/* decide whether to copy the contents of infile1 to outfile, or
   simply append "in-place" in infile1 */
/* PDW 6/22/98: infile1 still has the [ext] syntax... strip off before
   comparing to outfile */
    ffrtnm(infile1, temp, &status);
    if(strcmp(temp, outfile) && strcmp(infile1, outfile))
    {
        /* create a new output file, and copy leading extensions 
           from infile1 to outfile */
        if(ffopen(&infp1, infile1, READONLY, &status))
            goto Done;
        else inopen1 = TRUE;
        remove(outfile);
        if(ffinit(&outfp, outfile, &status))
            goto Done;
        else outopen = TRUE;
        CopyThruExt(infp1, outfp, 0, inext1, &status);
        if(status == END_OF_FILE)
        {
            sprintf(context, "Cannot find extension %d in file %s",
                    inext1, infile1);
            goto Done;
        }
        else if(status)
        {
            sprintf(context, "Error copying %s to %s", infile1, outfile);
            goto Done;
        }
    }
    else
    {
        /* interpolate in place; outfile is really just infile1 */
        if(ffopen(&outfp, outfile, READWRITE, &status))
            goto Done;
        else outopen = TRUE;
        if(ffmahd(outfp, outext + 1, &outhdutype, &status))
        {
            sprintf(context, "Cannot find extension %d in file %s",
                    outext, outfile);
            goto Done;
        }
    }

/* if time origin and units checking was selected by user, calculate
   time offset */
    if(tcheck) CheckTime(infp2, outfp, sortkey2, sortkey1, &toffset, &status);
    if(status)
    {
        sprintf(context, "Error occurred in module CheckTime");
        goto Done;
    }

/* interrogate outfile, and set up outfile's sort and append columns */
    if(ffgcno(outfp, CASEINSEN, sortkey1, &sortcol1, &status))
        goto Done;

    /* Loop through input column names */    
    for (icol=0; icol<NumListItems(incolnames); icol++) {
      CopyListItem(incolnames, icol, incolname);

      outcol = 0;
      isoldcol = FALSE;

      ffgcno(infp2, CASEINSEN, incolname, &incol, &status);
      ffgcno(outfp, CASEINSEN, incolname, &outcol, &status);

      if(status == COL_NOT_FOUND)
	{
	  /* add to output file a column with same characteristics as infile2's
	     data column */
	  ffcmsg();
	  status = 0;
	  if(AppendColFormat(infp2, outfp, inhdutype2, incol, &outcol, &status))
	    {
	      sprintf(context, "Cannot append column named %s to file %s",
		      incolname, outfile);
	      goto Done;
	    }
	  isoldcol = FALSE;
	}
      else if(!status) isoldcol = TRUE; /* column already exists */
      else goto Done;

      /* interpolate values */
      Interpolate(infp2, outfp, inhdutype2, sortcol2, incol, sortcol1, outcol,
		  order, extrap, toffset, strnull, intnull, isoldcol, &status);
      if(status)
	{
	  sprintf(context, "Error occurred in module Interpolate");
	  goto Done;
	}
    }

/* copy trailing extensions, if outfile is being copied from infile1 */
    if(inopen1) /* indicates we are copying from infile1 to outfile */
    {
        ffmrhd(infp1, 1, &exttype, &status);
        CopyThruExt(infp1, outfp, inext1 + 1, MAXHDU, &status);
        if(status == END_OF_FILE)
        {
            ffcmsg();
            status = 0;
        }
        else if(status)
            sprintf(context, "Error occurred while copying from %s to %s",
                    infile1, outfile);
    }

/* cleanup */
Done:
    if(outopen) ffclos(outfp, &status);
    if(inopen1) ffclos(infp1, &status);
    if(inopen2) ffclos(infp2, &status);
    WriteError(context, status);
    return;
}

/******************************************************************************
Function:    Interpolate

Description: The heart of finterp's calculation. This interrogates the
    input and output files, making sure all necessary keywords are written,
    and sets up the I/O between dynamic arrays and the files. It then reads
    the sort key values from the input and output files, and the column
    to be interpolated, and implements whichever interpolation option the
    user has selected. The results are written to the output file, including
    repeated or null values for out of range rows, if the extrap = REPEAT or
    NULL options are selected.

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    infp - input fitsfile containing source column
    outfp - output fitsfile containing destination table
    hdutype - source fitsfile table type
    insortcol, outsortcol - sort key column numbers in infp and outfp
    incol - source column number in infp
    outcol - destination column number in outfp
    order - order of the interpolation
    extrap - reflects user's choice of handling out of bounds values
    toffset - value added to insortcol values to correct for different
        time origins
    strnull - null string used in ASCII tables
    intnull - integer null value used in binary tables with integers
    isoldcol - flag indicating whether outfp's column already existed,
        or was added. At present, this is not used by this function.
    status - the omnipresent ftool status!

Primary Local Variables:
    long iarraymax, oarraymax - the sizes of the input and output
        arrays. They are used both to allocate the arrays, and as a
        maximum index when reading and writing the arrays. They are
        modified as needed to prevent reading past the end of the
        input column.
    long ilastrow, olastrow - the total number of rows in the input and
        output tables, as read from the NAXIS2 keywords.
    char writenul - used as a boolean value, to indicate that null
        values need to be written to the output file. 
    double *isort, *osort - arrays which are used to read sort key
        columns, (which must be readable as double.)
    finterp_array idata, odata - see finterp.h for structure finterp_array
    This structure contains pointers which may be used to address arrays
    of every data type which may occur in a fits column. It also
    contains a pointer to a "null array", which may be used by ffgcfX
    calls (see cfitsio documentation) to track any elements which are set
    to INDEF. The following group of functions are designed to handle 
    operations involving instantiations of this structure:

Functions Called:
    InitArray - uses its datatype argument to choose an array type
        appropriate for the given fits table column, dynamically
        allocates memory for an appropriate component pointer of
        finterp_array, and for the null array. Initializes values.
    FreeArray - frees memory dynamically allocated in InitArray.
    ReadArray - reads a page of values from an input file into an 
        finterp_array, using ffgcfX.
    Warning: The following two functions do not perform much error
        checking, in the interest of speed. They may not be very robust.
    CopyArray - copies a block of elements from one finterp_array to
        another, with the starting element of each finterp_array
        specified as an argument.
    InterpArray - performs a linear interpolation between two
        specified finterp_arrays. Must be called only if a numerical
        column datatype is being interpolated.
    FillArray - copies one element of an finterp_array to a block of
        elements of another finterp_array. The source and destination
        arrays may be the same.
    InitArrayNul - set a block of finterp_array elements to null.
    WriteArray - write the contents of an finterp_array to an output file,
        using ffpclu to set updefined elements, and ffpclX to write
        elements which are not null.
        
******************************************************************************/

int Interpolate(fitsfile *infp, fitsfile *outfp, int hdutype, int insortcol,
    int incol, int outsortcol, int outcol, int order, int extrap,
    double toffset, char *strnull, int intnull, int isoldcol, int *status)
{
/* variable declarations */
    char tnulln[FLEN_KEYWORD] = "", temp[FLEN_CARD] = "",
         comment[FLEN_COMMENT] = "", 
         context[C_FCERR_MSG]= "Error in function Interpolate";

    /* column parameters for input and output columns */
    int  itype = 0, otype = 0;
    long irepeat = 1L, orepeat = 1L, iwidth = 1L, owidth = 1L;

    /* flags */
    int  anynul = FALSE, /* TRUE if null value was read (see ffgcvX) */
         writenul = FALSE, /* TRUE if this function has set values to null */
         tnullexists = FALSE, /* TRUE if a valid TNULLn keyword exists */
         lastinput = FALSE, /* TRUE if last value in infp has been read */
         matchfound = FALSE, /* TRUE when a valid interp point is found */
         endofopage = FALSE; /* TRUE when array has been fully processed */
    
    /* indices, etc. */
    long pagesize = FNTRP_BIG_BUF, /* size of array, a "page" of values */
         iarraymax = FNTRP_BIG_BUF, /* number of rows read from infp */
         oarraymax = FNTRP_BIG_BUF, /* number of rows *read* from outfp */
         iarraystart = 0L, /* index - where in idata array to start reading
                              from outfp */
         oarraystart = 0L, /* index - where in odata array to start writing
                              to outfp */
         oarraystop = FNTRP_BIG_BUF, /* index - where in odata array to stop
                                        writing to outfp */
         omatch = 0L, /* index - current position in osort, odata arrays */
         imatch = 0L, /* index - position in isort, idata arrays which
                         matches the values in osort array at omatch,
                         i.e., use this value in interpolation */
         irow = 1L, orow = 1L, /* first row for next read/write to/from
                                  infp and outfp */
         ilastrow = 1L, olastrow = 1L, /* last row in infp, outfp */
         j = 0L; /* general use index - no significance */

    /* arrays */
    double *isort = NULL, *osort = NULL; /* arrays to hold sort key data */
    finterp_array idata, odata; /* arrays to hold data being interpolated */

/* check parameters for major problems */
    if(*status) return *status;
    else if(!infp || !outfp || !strnull) *status = FNTRP_BAD_PTR;
    else if((hdutype != ASCII_TBL) && (hdutype != BINARY_TBL))
        *status = NOT_TABLE;
    if(*status) return *status;

/* find out how many rows are in the files */
    if(ffgnrw(infp, &ilastrow, status))
    {
        sprintf(context, "Could not find number of rows in infile");
        goto Error;
    }
    if(ffgnrw(outfp, &olastrow, status))
    {
        sprintf(context, "Could not find number of rows in outfile");
        goto Error;
    }

/* find out the datatypes of the columns, and make sure they are the same */
    ffgtcl(infp, incol, &itype, &irepeat, &iwidth, status);
    ffgtcl(outfp, outcol, &otype, &orepeat, &owidth, status);
    if(!*status && ((itype != otype) || (irepeat != orepeat) ||
                    (iwidth != owidth)))
    {
        *status = FNTRP_BAD_COL;
        sprintf(context, "Input and output column data types do not match");
        goto Error;
    }
    else if(!*status && (order == 1) && 
                        ((itype == TLOGICAL) || (itype == TSTRING)))
    {
        *status = FNTRP_BAD_ORDER;
        sprintf(context,
            "Parameter order = 1 illegal for string or logical column");
        goto Error;
    }
    if(*status) return *status;
    
/* allocate dynamic memory */
    InitArray(&idata, itype, irepeat, iwidth,
        FNTRP_BIG_BUF, &pagesize, status);
    InitArray(&odata, otype, orepeat, owidth,
        FNTRP_BIG_BUF, &pagesize, status);
    if(!*status)
    {
        if(!(isort = (double*) malloc(pagesize * sizeof(double))))
            *status = FNTRP_OUT_OF_MEM;
        if(!(osort = (double*) malloc(pagesize * sizeof(double))))
            *status = FNTRP_OUT_OF_MEM;
    }
    if(*status)
    {
        sprintf(context, "Unable to allocate large arrays");
        goto Done;
    }
    iarraymax = pagesize; oarraymax = pagesize;

/* make sure a valid null value is defined */
/* first, see if there is already a TNULLn defined */
    ffkeyn("TNULL", outcol, tnulln, status);
    if(!ffgkey(outfp, tnulln, temp, comment, status))
    {
        tnullexists = TRUE;
        strip(strnull, temp);
        intnull = atoi(strnull);
    }
    else if(*status == KEY_NO_EXIST)
    {
        /* no TNULLn is defined, so use the value supplied by user */
        ffcmsg();
        *status = 0;
        tnullexists = FALSE;
    }
    if(hdutype == ASCII_TBL) ffsnul(outfp, outcol, strnull, status);
    else if((hdutype == BINARY_TBL) && ((itype == TSHORT) ||
        (itype == TLONG) || (itype == TBYTE)))
        fftnul(outfp, outcol, intnull, status);
    if(*status)
    {
        sprintf(context, "Problem setting null value");
        goto Done;
    }
    
/* read first page of sort and source columns from input file */
    if(iarraymax > (ilastrow - irow + 1L)) iarraymax = ilastrow - irow + 1L;
    ffgcvd(infp, insortcol, irow, 1L, iarraymax, 0.0, isort,
        &anynul, status);
    if(!*status && anynul) *status = FNTRP_SORT_NULL;
    if(*status)
    {
        sprintf(context, "Problem reading column %d (sort key) in infile",
            insortcol);
        goto Done;
    }
    ReadArray(infp, incol, irow, 0L, iarraymax, &idata, &anynul, status);
    irow += iarraymax;

    if(*status)
    {
        sprintf(context, "Problem reading column %d (source) in infile",
            incol);
        goto Done;
    }

    /* handle any difference in time origins */
    if(toffset != 0.0) for(j = 0L; j < iarraymax; j++) isort[j] += toffset;

/* the following adjustment to iarraymax allows natural handling of
   intervals; because each valid interpolation is between TWO values of
   the input array, we look for upper limits of intervals starting with
   element 1, rather than 0. Then, when we read a new page of input, we
   must first copy over the LAST element to the zeroth element, then read
   the next page starting at element 1. It is a convenience to change 
   iarraymax here, but it is important to remember this change. */

    iarraymax--;
    iarraystart = 1L;


/* new version main loop */
/* leading out of bounds values */
    while(orow <= olastrow)
    {
        /* read a page from output */
        if(oarraymax > olastrow - orow + 1L)
            oarraymax = olastrow - orow + 1L;
        ffgcvd(outfp, outsortcol, orow, 1L, oarraymax, 0.0, osort,
            &anynul, status);
        if(!*status && anynul) *status = FNTRP_SORT_NULL;
        if(*status)
        {
            sprintf(context, "Problem reading column %d (sort key) in outfile",
                outsortcol);
            goto Done;
        }

        /* find first value in output for which we can interpolate odata */
        endofopage = FALSE;
        for(omatch = 0L; omatch < oarraymax; omatch++)
            if(osort[omatch] >= isort[0]) break;
        if(omatch == oarraymax) endofopage = TRUE;

        switch(extrap){
        case FNTRP_SET_NULL:
	    SetArrayNull(&odata, 0L, omatch, TRUE, status);
            if(omatch > 0L) writenul = TRUE;
            if(endofopage)
            {
                WriteArray(outfp, outcol, orow, 0L, oarraymax, &odata, status);
                orow += oarraymax;
            }
            else oarraystart = 0L;
            break;
        case FNTRP_REPEAT:
            FillArray(&idata, 0L, &odata, 0L, omatch, status);
            if(endofopage)
            {
                WriteArray(outfp, outcol, orow, 0L, oarraymax, &odata, status);
                orow += oarraymax;
            }
            else oarraystart = 0L;
            break;
        case FNTRP_IGNORE:
            /* do nothing to array contents */
            if(endofopage) orow += oarraymax;
            else oarraystart = omatch;
            break;
        }
        if(!endofopage) break; /* this page has values we can calculate,
                                 so go on to next block */
    }

/* interpolation - inbounds values */
    imatch = 1L; /* first interval to check is between 0 and 1 */
    while(orow <= olastrow)
    {
        for(; omatch < oarraymax; omatch++)
        {
            matchfound = FALSE;
            while(!matchfound)
            {
            /* search current page for a match */
                if(osort[omatch] < isort[imatch])
                    matchfound = TRUE;
                else imatch++;

            /* handle a valid match */
                if(matchfound) 
                {
                    if(order == 0)
                        CopyArray(&idata, imatch - 1L, &odata, omatch,
                            1L, status);
                    else if(order == 1)
                        InterpArray(isort, &idata, imatch - 1L, osort,
                            &odata, omatch, status);
                }

            /* continue the while if the match was not the last value
               on the page */
                if(imatch <= iarraymax) continue;

            /* otherwise, check whether more pages exist */
                if(irow > ilastrow) /* we are on last page - don't read more */
                {
                /* handle last interpolation */
                    if((order == 0 && isort[iarraymax] <= osort[omatch]) ||
                       (order == 1 && isort[iarraymax] == osort[omatch]))
                        {
                            CopyArray(&idata, iarraymax, &odata, omatch,
                                1L, status);
                            omatch++;
                        }
                    lastinput = TRUE;
                    break; /* escape the while */
                }

            /* copy current value to zeroth value of input */
                isort[0] = isort[iarraymax];
                CopyArray(&idata, iarraymax, &idata, 0L, 1L, status);

            /* read next input page */
                if(iarraymax > ilastrow - irow + 1L)
                   iarraymax = ilastrow - irow + 1L;
                ffgcvd(infp, insortcol, irow, 1L, iarraymax, 0.0,
                    isort + iarraystart, &anynul, status);
                if(!*status && anynul) *status = FNTRP_SORT_NULL;
                if(*status)
                {
                    sprintf(context,
                        "Problem reading column %d (sort key) in infile",
                        insortcol);
                    goto Done;
                }
                ReadArray(infp, incol, irow, iarraystart, iarraymax, &idata,
                    &anynul, status);
                if(*status)
                {
                    sprintf(context,
                        "Problem reading column %d (source) in infile",
                        incol);
                    goto Done;
                }

            /* handle any difference in time origins */
                if(toffset != 0.0)
                    for(j = 0L; j < iarraymax; j++) isort[j] += toffset;

            /* update indices */
                imatch = 1L;
                irow += iarraymax;
            }

        /* JP 2/8/1999: default stop value for the next call to WriteArray
           is the last valid array value */
            oarraystop = oarraymax;

        /* if we are at end of input, deal with remainder of odata */
            if(lastinput)
            {
                switch(extrap){
                case FNTRP_SET_NULL:
                    SetArrayNull(&odata, omatch,
				 oarraymax - omatch, TRUE, status);
                    if(oarraymax > omatch) writenul = TRUE;
                    break;
                case FNTRP_REPEAT:
                    FillArray(&odata, omatch - 1L, &odata, omatch,
                        oarraymax - omatch, status);
                    break;
                case FNTRP_IGNORE:
                    /* write only up to the last match and stop */
                    oarraystop = omatch;
                    break;
                }
                break; /* escape the for */
            }
        }

        /* write a page to outfile */
        WriteArray(outfp, outcol, orow + oarraystart, oarraystart,
            oarraystop - oarraystart, &odata, status);
        orow += oarraymax;
        if(lastinput || (orow > olastrow)) break;

        /* read a page of sort key values from outfile */
        if(oarraymax > olastrow - orow + 1L)
            oarraymax = olastrow - orow + 1L;
        ffgcvd(outfp, outsortcol, orow, 1L, oarraymax, 0.0, osort,
            &anynul, status);
        if(!*status && anynul) *status = FNTRP_SORT_NULL;
        if(*status)
        {
            sprintf(context, "Problem reading column %d (sort key) in outfile",
                outsortcol);
            goto Done;
        }

        omatch = 0L;
        oarraystart = 0L;
	/* Make sure to re-initialize the null values so they don't
	   persist to the next page!! */
	SetArrayNull(&odata, 0L, oarraymax, FALSE, status);
    }

/* trailing out of bounds values */
    switch(extrap){
        case FNTRP_SET_NULL:
            if(orow > olastrow) break;
            ffpclu(outfp, outcol, orow, 1L,
                orepeat * (olastrow - orow + 1L), status);
            writenul = TRUE;
            break;
        case FNTRP_REPEAT:
            if(orow > olastrow) break;
            oarraymax = pagesize;
            FillArray(&idata, imatch, &odata, 0L, oarraymax, status);
            while(orow <= olastrow)
            {
                if(oarraymax > olastrow - orow + 1L)
                    oarraymax = olastrow - orow + 1L;
                WriteArray(outfp, outcol, orow, 0L, oarraymax, &odata,
                    status);
                orow += oarraymax;
            }
            break;
        case FNTRP_IGNORE:
            break;
    }

/* end new version */
/* the following output block is to be uncommented only for host debugging */
/*
    printf("i\tisort\tosort\tidata\n");
    if(idata.type == TLOGICAL)
    {
        printf("logical\n");
        for(i = 0L; i < pagesize; i++)
            printf("%ld\t%d\t%g\t%g\t%d\t%d\n", i, idata.l[i], isort[i],
        osort[i], odata.l[i], odata.nul[i]);
    }
    else if(idata.type == TSTRING)
    {
        printf("ascii\n");
        for(i = 0L; i < pagesize; i++)
            printf("%ld\t%s\t%g\t%g\t%s\t%d\n", i, idata.s[i], isort[i],
        osort[i], odata.s[i], odata.nul[i]);
    }
    else if(idata.type == TDOUBLE)
    {
        printf("double\n");
        for(i = 0L; i < pagesize; i++)
        {
            printf("%ld\tidata\t%g\t%g\todata\tonul\n", i, isort[i],
                osort[i]);
            for(j = 0L; j < idata.repeat; j++)
                printf("\t%g\t\t\t%g\t%d\n", idata.d[i * idata.repeat + j],
                    odata.d[i * odata.repeat + j],
                    odata.nul[i * odata.repeat + j]);
        }
    }
/* end debugging output block */

    if(writenul && !tnullexists)
    {
        if(hdutype == ASCII_TBL)
        {
            strcpy(comment, tnulln);
            strncat(comment, " = strnull value assigned by finterp",
                    FLEN_COMMENT - strlen(comment));
            ffpkys(outfp, tnulln, strnull, comment, status);
        }
        else if((hdutype == BINARY_TBL) && ((itype == TSHORT) ||
            (itype == TLONG) || (itype == TBYTE)))
        {
            strcpy(comment, tnulln);
            strncat(comment, " = intnull value assigned by finterp",
                    FLEN_COMMENT - strlen(comment));
            ffpkyj(outfp, tnulln, (long) intnull, comment, status);
        }
    }


Done:
    /* clean up */
    FreeArray(&odata);
    FreeArray(&idata);
    if(osort) free(osort);
    if(isort) free(isort);
    osort = NULL;
    isort = NULL;

Error:
    /* print error message */
    WriteError(context, *status);
    return *status;
}

/*******************************************************************************
Function: InitArray

Description: dynamically allocate memory for an finterp_array structure.
    This is an array used for column I/O in an ASCII or binary table.
    Based on the argument datatype, memory is allocated to a particular
    component pointer of the finterp_array structure. Memory is also
    allocated for the null array, which is used to keep track of
    values which are set to INDEF.

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    finterp_array *array - the array to be allocated
    int datatype         - the datatype, specified by a Txxx constant
        This is used to determine what primitive type of array to set up.

The following 3 arguments are used to set the array size:
    long repeat          - the repeat count of the array/column
    long width           - the width of the array/column, (used only for
                           string data types)
    long numrows         - the number of rows in the array/column
*******************************************************************************/

int InitArray(finterp_array *array, int datatype, long repeat, long width,
    long maxmem, long *numrows, int *status)
{
    long i, elsize, nelements;

    if(*status) return *status;
    if(!array || !numrows) *status = FNTRP_BAD_PTR;
    else if(maxmem <= 0L) *status = FNTRP_BAD_ARG;
    if(*status) return *status;
    array->l = NULL;
    array->s = NULL;
    array->d = NULL;
    array->nul = NULL;
    array->type = datatype;
    array->repeat = repeat;
    array->width = ++width;/* include room for terminating null */

    /* allocate space for an appropriate array datatype */
    if(datatype == TLOGICAL)
    {
        elsize = sizeof(char);
        *numrows = maxmem / (repeat * elsize);
        nelements = *numrows * repeat;
        if(!(array->l = (char*) malloc(nelements * elsize)))
            *status = FNTRP_OUT_OF_MEM;
        else memset(array->l, '\0', nelements * elsize);
    }
    else if(datatype == TSTRING)
    {
        elsize = sizeof(char*) + width * sizeof(char);
        *numrows = maxmem / (repeat * elsize);
        nelements = *numrows * repeat;
        if(!(array->s = (char**) malloc(nelements * sizeof(char*))))
            *status = FNTRP_OUT_OF_MEM;
        for(i = 0L; i < nelements; i++)
        {
            if(!(array->s[i] = (char*) malloc(width * sizeof(char))))
                *status = FNTRP_OUT_OF_MEM;
            else memset(array->s[i], '\0', width * sizeof(char));
        }
    }
    else /* all other data datatypes may be read as doubles */
    {
        array->type = TDOUBLE;
        elsize = sizeof(double);
        *numrows = maxmem / (repeat * elsize);
        nelements = *numrows * repeat;
        if(!(array->d = (double*) malloc(nelements * elsize)))
            *status = FNTRP_OUT_OF_MEM;
        else for(i = 0L; i < nelements; i++)
            array->d[i] = 0.0;
    }

    /* setup and initialize null array */
    if(!(array->nul = (char*) malloc(nelements * sizeof(char))))
        *status = FNTRP_OUT_OF_MEM;
    else for(i = 0L; i < nelements; i++) array->nul[i] = FALSE;

    if(*status) *numrows = 0L;
    array->numrows = *numrows;
    return *status;
}

/*******************************************************************************
Function: FreeArray

Description: free array space allocated previously for an finterp_array
    structure using InitArray. (This is an array used for column I/O in
    an ASCII or binary table.)

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    finterp_array *array - the array to be freed
*******************************************************************************/

void FreeArray(finterp_array *array)
{
    long i, numitems;

    if(!array) return;
    numitems = array->numrows * array->repeat;
    if(array->nul) free(array->nul);
    if(array->d) free(array->d);
    if(array->s)
    {
        for(i = 0L; i < numitems; i++)
            if(array->s[i]) free(array->s[i]);
        free(array->s);
    }
    if(array->l) free(array->l);
    array->nul = NULL;
    array->d = NULL;
    array->s = NULL;
    array->l = NULL;

    return;
}

/*******************************************************************************
Function: ReadArray

Description: Fill an finterp_array structure with values read from a
    fits table column. The type component of the finterp_array is used
    to determine the data type of the fits column, and an appropriate 
    ffgcfX cfitsio call is used to fill the appropriate array component
    of the structure. Note: InitArray MUST be called prior to calling
    ReadArray!

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    fitsfile *fp     - the fitsfile (which must be at a table extension)
        from which values are to be read.
    int colnum       - the source column in the fits table
    long fitsrow    - the first source row in the fits table
    long numrows     - the number of rows to be read
    finterp_array *a - the array into which the data is to be read
    int *anynul      - used in ffgcfX calls
    int *status      - program status

Functions called:
libcfitsio:
    ffXXXX
*******************************************************************************/

int ReadArray(fitsfile *fp, int colnum, long fitsrow, long arrayrow,
    long numrows, finterp_array *a, int *anynul, int *status)
{
    long numels, firstel;

    if(*status) return *status;
    if(arrayrow < 0L) *status = FNTRP_BAD_ARG;
    if(numrows < 0L) *status = FNTRP_BAD_ARG;
    if(*status) return *status;

/* Make sure reading numrows wont exceed array size. */
    if(arrayrow + numrows > a->numrows) numrows = a->numrows - arrayrow;
    firstel = arrayrow * a->repeat;
    numels = numrows * a->repeat;

/* we always start from the first element in a row */
    if(a->type == TLOGICAL)
    {
        ffgcfl(fp, colnum, fitsrow, 1L, numels, a->l + firstel,
            a->nul + firstel, anynul, status);
    }
    else if(a->type == TSTRING)
    {
        ffgcfs(fp, colnum, fitsrow, 1L, numels, a->s + firstel,
            a->nul + firstel, anynul, status);
    }
    else if(a->type == TDOUBLE)
    {
        ffgcfd(fp, colnum, fitsrow, 1L, numels, a->d + firstel,
            a->nul + firstel, anynul, status);
    }
    return *status;
}

/*******************************************************************************
Function: CopyArray

Description: Copy a block of elements of one finterp_array to another, 
    as specified in the arguments. Elements in the source array which
    are set to INDEF will be copied as INDEF to the destination array.
    Only the component array relevant to the type component of the input 
    finterp_array is copied. Note: InitArray MUST be called prior to
    calling CopyArray!

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    finterp_array *in  - the source finterp_array
    long ini           - index identifying the first element of in
        to copy
    finterp_array *out - the destination finterp_array
    long outi          - index identifying the element of out at
        which the copying should commence
    long numrows       - total number of rows to copy
    int *status        - program status
*******************************************************************************/

int CopyArray(finterp_array *in, long ini, finterp_array *out, long outi,
    long numrows, int *status)
{
    long i, numels, ioffset, ooffset;

    if(*status) return *status;
    if(numrows + ini > in->numrows) numrows = in->numrows - ini + 1L;
    if(numrows + outi > out->numrows) numrows = out->numrows - outi + 1L;

    numels = numrows * in->repeat;
    ioffset = ini * in->repeat;
    ooffset = outi * out->repeat;
    if(in->type == TLOGICAL)
        for(i = 0L; i < numels; i++)
        {
            out->l[i + ooffset] = in->l[i + ioffset];
            out->nul[i + ooffset] = in->nul[i + ioffset];
        }
    else if(in->type == TSTRING)
    {
        for(i = 0L; i < numels; i++)
        {
            strncpy(out->s[i + ooffset], in->s[i + ioffset],
                out->width - 1L);
            out->nul[i + ooffset] = in->nul[i + ioffset];
        }
    }
    else if(in->type == TDOUBLE)
        for(i = 0L; i < numels; i++)
        {
            out->d[i + ooffset] = in->d[i + ioffset];
            out->nul[i + ooffset] = in->nul[i + ioffset];
        }
    return *status;
}

/*******************************************************************************
Function: InterpArray

Description: Interpolate a value from one finterp_array into another. Two
    values from each of the sort key arrays are used to weight the
    corresponding two values from the source finterp_array, and compute
    a linearly interpolated value, which is written to the output
    finterp_array. Note: InitArray must be called to set up a numerical 
    array type before calling InterpArray.

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    double *isort      - the input sort key array used in the interpolation
    finterp_array *in  - the source finterp_array
    long ini           - index identifying the first element of in
        to copy
    double *osort      - the output sort key array used in the interpolation
    finterp_array *out - the destination finterp_array
    long outi          - index identifying the element of out at
        which the interpolation should commence
    int *status        - program status
*******************************************************************************/

int InterpArray(double *isort, finterp_array *in, long ini, double *osort,
    finterp_array *out, long outi, int *status)
{
    long i, nextrow, ioffset, ooffset;
    double dt;

    if(*status) return *status;

    ioffset = ini * in->repeat;
    ooffset = outi * out->repeat;

    nextrow = ini + 1L;
    dt = isort[nextrow] - isort[ini];
    if(dt == 0.)
        for(i = 0L; i < in->repeat; i++)
            out->d[i + ooffset] = in->d[i + ioffset];
    else
    {
        for(i = 0L; i < in->repeat; i++)
        {
            if(in->nul[i + ioffset] || in->nul[i + nextrow * in->repeat])
                out->nul[i + ooffset] = TRUE;
            else
                out->d[i + ooffset] = in->d[i + ioffset]
                + (in->d[i + nextrow * in->repeat] - in->d[i + ioffset])
                * (osort[outi] - isort[ini]) / dt;
        }
    }
    return *status;
}

/*******************************************************************************
Function: FillArray

Description: Copy an element of an finterp_array repeatedly into a block 
    of elements in another finterp_array. The source and destination 
    finterp_arrays may be the same. Only the null array value, and the 
    component array of the source finterp_array appropriate to its data
    type are copied. Note: InitArray MUST be called before calling FillArray.

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    finterp_array *in  - the source finterp_array
    long ini           - index identifying the element of in to copy
    finterp_array *out - the destination finterp_array
    long outi          - index identifying the element of out at
        which the fill should commence
    long numrows       - the number of rows in out to fill
    int *status        - program status
*******************************************************************************/

int FillArray(finterp_array *in, long ini, finterp_array *out, long outi,
    long numrows, int *status)
{
    long i, numels, ioffset, ooffset;

    if(*status) return *status;
    if(numrows == 0L) return *status;
    if(numrows < 0L) *status = FNTRP_BAD_ARG;
    if(*status) return *status;

    if(numrows + ini > in->numrows) numrows = in->numrows - ini;
    if(numrows + outi > out->numrows) numrows = out->numrows - outi;
    numels = numrows * in->repeat;
    ioffset = ini * in->repeat;
    ooffset = outi * out->repeat;
    if(in->type == TLOGICAL)
        for(i = 0L; i < numels; i++)
        {
            out->l[i + ooffset] = in->l[(i % in->repeat) + ioffset];
            out->nul[i + ooffset] = in->nul[(i % in->repeat) + ioffset];
        }
    else if(in->type == TSTRING)
    {
        for(i = 0L; i < numels; i++)
        {
            strncpy(out->s[i + ooffset], in->s[(i % in->repeat) + ioffset],
                out->width);
            out->nul[i + ooffset] = in->nul[(i % in->repeat) + ioffset];
        }
    }
    else if(in->type == TDOUBLE)
        for(i = 0L; i < numels; i++)
        {
            out->d[i + ooffset] = in->d[(i % in->repeat) + ioffset];
            out->nul[i + ooffset] = in->nul[(i % in->repeat) + ioffset];
        }
    return *status;
}

/*******************************************************************************
Function: SetArrayNull

Description: Sets a range of an finterp_array's component null array to
    TRUE, indicating this range is set to null, or INDEF. Note: InitArray
    MUST be called before calling SetArrayNull.

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    finterp_array *out - the finterp_array being set to null
    long firstrow      - the first row in out to set to null
    long numrows       - the number of rows in out to set to null
    int *status        - program status
*******************************************************************************/

int SetArrayNull(finterp_array *out, long firstrow, long numrows, int value, int *status)
{
    long i, lastel, firstel;
    if(*status) return *status;
    if(numrows == 0L) return *status;
    if(numrows < 0L) *status = FNTRP_BAD_ARG;
    else if(firstrow < 0L) *status = FNTRP_BAD_ARG;
    if(*status) return *status;

    if(firstrow + numrows > out->numrows) numrows = out->numrows - firstrow;
    firstel = firstrow * out->repeat;
    lastel = (firstrow + numrows) * out->repeat;
    for(i = firstel; i < lastel; i++)
        out->nul[i] = value;
    return *status;
}

/*******************************************************************************
Function: WriteArray

Description: Write the contents of an finterp_array to an output fits
    table. Any blocks of values which are set to null using the null
    array component of the finterp_array will be set to INDEF in the 
    output file, using ffpclu. For values which are not set to null, 
    the cfitsio ffpclX function appropriate to the finterp_array's 
    type component will be called.
    Note: InitArray MUST be called prior to calling WriteArray!

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    fitsfile *outfp    - the fitsfile (which must be at a table extension)
        to which values are to be written.
    int colnum         - the destination column in the fits table
    long fitsrow      - the first destination row in the fits table
    long numrows       - the number of rows to be written
    finterp_array *out - the array from which the data is to be written
    int *status        - program status

Functions called:
libcfitsio:
    ffXXXX
*******************************************************************************/

int WriteArray(fitsfile *outfp, int colnum, long fitsrow, long arrayrow,
    long numrows, finterp_array *out, int *status)
{
    int nulstart = FALSE;
    long i = 0L, firsti, nextel = 0L, nelements, lasti;

    if(*status) return *status;
    if(numrows < 0L) *status = FNTRP_BAD_ARG;
    else if(arrayrow < 0L) *status = FNTRP_BAD_ARG;
    if(*status) return *status;

/* Make sure writing nelements wont exceed array size. */
    if(arrayrow + numrows > out->numrows) numrows = out->numrows - arrayrow;
    firsti = arrayrow * out->repeat;
    lasti = (arrayrow + numrows) * out->repeat;

    if(out->nul[firsti]) nulstart = TRUE;
    for(i = firsti + 1L; i < lasti; i++)
    {
        if(nulstart && !out->nul[i])
        {
            nelements = i - firsti;
            ffpclu(outfp, colnum, fitsrow, nextel + 1L, nelements, status);
            fitsrow += (nextel + nelements)/out->repeat;
            nextel = (nextel + nelements) % out->repeat;
            firsti = i;
            nulstart = FALSE;
        }
        else if(!nulstart && out->nul[i])
        {
            nelements = i - firsti;
            if(out->type == TLOGICAL)
                ffpcll(outfp, colnum, fitsrow, nextel + 1L, nelements,
                    out->l + firsti, status);
            else if(out->type == TSTRING)
                ffpcls(outfp, colnum, fitsrow, nextel + 1L, nelements,
                    out->s + firsti, status);
            else if(out->type == TDOUBLE)
                ffpcld(outfp, colnum, fitsrow, nextel + 1L, nelements,
                    out->d + firsti, status);
            fitsrow += (nextel + nelements)/out->repeat;
            nextel = (nextel + nelements) % out->repeat;
            firsti = i;
            nulstart = TRUE;
        }
    }
    if(nulstart)
    {
        nelements = i - firsti;
        ffpclu(outfp, colnum, fitsrow, nextel + 1L, nelements, status);
    }
    else
    {
        nelements = i - firsti;
        if(out->type == TLOGICAL)
            ffpcll(outfp, colnum, fitsrow, nextel + 1L, nelements,
                out->l + firsti, status);
        else if(out->type == TSTRING)
            ffpcls(outfp, colnum, fitsrow, nextel + 1L, nelements,
                out->s + firsti, status);
        else if(out->type == TDOUBLE)
            ffpcld(outfp, colnum, fitsrow, nextel + 1L, nelements,
                out->d + firsti, status);
    }
    return *status;
}

/*******************************************************************************
Function: CheckTime

Description: Read the TIMESYS and TIMEUNIT keywords from two fitsfiles.
    Calculate the difference between the source file's value of TIMESYS,
    and the destination file's value of TIMESYS, and write this into the
    argument double *toffset. If either file is missing the TIMESYS
    keyword, an error is generated. If either file is missing the
    TIMEUNIT keyword, this keyword is ignored. If the files have two 
    DIFFERENT values for the TIMEUNIT keyword, an error is generated.

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    fitsfile *infp, *outfp - the source and destination fitsfiles
        (both infp, and outfp must be at a table extension)
    char *intime, *outtime - the names of the time columns in the source
        and destination files
    double *toffset        - the time origin offset calculated by CheckTime 
    int *status            - program status

Functions called:
libcfitsio:
    ffXXXX
*******************************************************************************/

int CheckTime(fitsfile *infp, fitsfile *outfp, char *intime,
    char *outtime, double *toffset, int *status)
{
    char   intimestr[FLEN_VALUE], outtimestr[FLEN_VALUE],
           intimeunit[FLEN_KEYWORD], outtimeunit[FLEN_KEYWORD],
           comment[FLEN_COMMENT], temp[FLEN_VALUE], context[C_FCERR_MSG];
    double intimesys, outtimesys;

    if(*status) return *status;
    *toffset = 0.0;
    if(ffgkey(infp, "TIMESYS", temp, comment, status))
    {
        sprintf(context, "%d - keyword TIMESYS not found in infile2",
            *status);
        c_fcerr(context);
        sprintf(context, "    - Set parameter tcheck = no to ignore this");
        WriteError(context, *status);
        return *status;
    }
    strip(intimestr, temp);
    intimesys = (double) atof(intimestr);
    memset(temp, 0, FLEN_VALUE);
    if(ffgkey(outfp, "TIMESYS", temp, comment, status))
    {
        sprintf(context, "%d - keyword TIMESYS not found in outfile",
            *status);
        c_fcerr(context);
        sprintf(context, "    - Set parameter tcheck = no to ignore this");
        WriteError(context, *status);
        return *status;
    }
    strip(outtimestr, temp);
    outtimesys = (double) atof(outtimestr);
    *toffset = intimesys - outtimesys;
    ffgkey(infp, "TIMEUNIT", intimeunit, comment, status);
    ffgkey(outfp, "TIMEUNIT", outtimeunit, comment, status);
    if(*status == KEY_NO_EXIST)
    {
        ffcmsg();
        *status = OK;
    }
    else
    {
        strcpy(intimeunit, strip(temp, intimeunit));
        strcpy(outtimeunit, strip(temp, outtimeunit));
        if(strcmp(intimeunit, outtimeunit))
        {
            *status = FNTRP_BAD_UNIT;
            sprintf(context, "%d - Infile units of %s are %s",
                    *status, intime, intimeunit);
            c_fcerr(context);
            sprintf(context, "     - Outfile units of %s are %s",
                    outtime, outtimeunit);
            c_fcerr(context);
        }
    }
    return *status;
}

/*******************************************************************************
Function: AppendColFormat

Description: Creates a new column in a fits table whose data type, 
    repeat and width values match a column in a different fits table. In
    addition, the keywords TUNITn, TNULLn, and TFORMn are copied, 
    with suitable changes in the column number.

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    fitsfile *infp, *outfp - the source and destination fitsfiles
        (both infp, and outfp must be at a table extension)
    int hdutype - the fits extension type; must by ASCII_TBL or BINARY_TBL
    int incol   - the number of the source column
    int *outcol - the number of the destination column
    int *status - program status

Functions called:
local:
libcfitsio:
    ffXXXX
*******************************************************************************/

int AppendColFormat(fitsfile *infp, fitsfile *outfp, int hdutype, int incol,
    int *outcol, int *status)
{
    char   ttype[FLEN_VALUE], tform[FLEN_VALUE], 
           context[C_FCERR_MSG],
           comment[FLEN_COMMENT], keyword[FLEN_KEYWORD], temp[FLEN_CARD];
    char *patterns[][2] = {
      {"TUNITn", "TUNITn"}, /* Standard FITS columns */
      {"TNULLn", "TNULLn"},
      {"TSCALn", "TSCALn"},
      {"TZEROn", "TZEROn"},
      {"TDISPn", "TDISPn"},
      /* {"TDIMn" , "TDIMn" } no, since will be the DIM of the index column */

      {"TCTYPn", "TCTYPn"}, /* Standard WCS columns */
      {"TCUNIn", "TCUNIn"},
      {"TCRVLn", "TCRVLn"},
      {"TCDLTn", "TCDLTn"},
      {"TCRPXn", "TCRPXn"},
      {"TPn_ka", "TPn_ka"},
      {"TCn_ka", "TCn_ka"},
      {"TVn_ma", "TVn_ma"},
      {"TSn_ma", "TSn_ma"},
      {"TCRDna", "TCRDna"},
      {"TCSYna", "TCSYna"},
      {"TCROTn", "TCROTn"},
      {0,         0      }
    };

/* check arguments */
    if(*status) return *status;
    else if(!infp || !outfp || !outcol) *status = FNTRP_BAD_PTR;
    else if((hdutype != ASCII_TBL) && (hdutype != BINARY_TBL))
        *status = NOT_TABLE;
    else if(incol < 0) *status = FNTRP_BAD_COL;
    if(*status) return *status;

/* find last column in outfp */
    ffgncl(outfp, outcol, status);

/* get TTYPE keyword from infp */
    ffkeyn("TTYPE", incol, keyword, status);
    if(ffgkey(infp, keyword, temp, comment, status))
    {
        sprintf(context, "%d - keyword %s not found in infile", 
            *status, keyword);
        WriteError(context, *status);
        return *status;
    }
    strip(ttype, temp);

/* get TFORM keyword from infp */
    ffkeyn("TFORM", incol, keyword, status);
    if(ffgkey(infp, keyword, temp, comment, status))
    {
        sprintf(context, "%d - keyword %s not found in infile", 
            *status, keyword);
        WriteError(context, *status);
        return *status;
    }
    strip(tform, temp);

/* add a column after the last column in outfp */
    (*outcol)++;
    fficol(outfp, *outcol, ttype, tform, status);

/* copy remaining (optional) keywords from infp */
    fits_translate_keywords(infp, outfp, 1, 
			    patterns, sizeof(patterns)/(2*sizeof(patterns[0])),
			    incol, (*outcol)-incol, 0, status);
    /* Make sure CFITSIO knows about new structural keywords,
       including TSCALn, TZEROn and TNULLn */
    ffrdef(outfp, status);
    return *status;
}

/******************************************************************************
Function: ginterp

Description: Main function in this ftool. Interrogates .par file, checks
    parameters for validity. Opens input and output files, finds proper
    extensions. Handles the following options/ parameters:
        calls CopyThruExt if user is not interpolating "in place"
            to copy the destination file
        calls CheckTime if tcheck == yes to calculate offset due
            to different time origins
        calls Interpolate to perform the actual interpolation

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Primary Local Variables:
    char *infile1 - name and extension of the source table, which holds
        the column to be interpolated
    char *infile2 - filename and extension of the pre-existing table
        into which the values are to be interpolated
    char *outfile - filename and extension of the destination table. If
        same as infile2, the new information will be added directly to
        infile2
    char *incolname - name of source column
    char *sortkey1, *sortkey2 - names of the columns in input and output
        tables used for sort
    char *strnull - string used for null values in ascii tables
    int *intnull - integer used for null values in binary tables with integer
        data types
    int *extrap - reflects user's choice of handling out of bounds values
    int *tcheck - flag used to decide whether to check for time origins
    int *order - order of the interpolation
    int *status _ program status

Functions called:
library functions:
    libhost.a:
        Uclgst - get a string from .par file
        Uclgsi - get an integer value from .par file
        Uclgsb - get a boolean (char) value from .par file

    libcftools.a
        c_fcerr - write to STDERR
******************************************************************************/

int ginterp(char *infile1, char *infile2, char *outfile, char *incolname,
    char *sortkey1, char *sortkey2, char *strnull, int *intnull,
    int *extrap, int *tcheck, int *order, int *status)
{
    char temp[FNTRP_CHAR_BUF], msg[C_FCERR_MSG];
    int BufLen_2 = FNTRP_CHAR_BUF - 1;/* required by cfortran.h */

    if(*status) goto Finish;
    Uclgst("infile1", infile1, status); 
    if(*status)
    {
        sprintf(msg, "Parameter 'infile1' not found in .par file");
        goto Finish;/**/
    }
    Uclgst("infile2", infile2, status);
    if(*status)
    {
        sprintf(msg, "Parameter 'infile2' not found in .par file");
        goto Finish;/**/
    }
    Uclgst("outfile", outfile, status);
    if(*status)
    {
        sprintf(msg, "Parameter 'outfile' not found in .par file");
        goto Finish;/**/
    }
    Uclgst("incol", incolname, status);
    if(*status)
    {
        sprintf(msg, "Parameter 'incol' not found in .par file");
        goto Finish;/**/
    }
    Uclgst("sortkey1", sortkey1, status);
    if(*status)
    {
        sprintf(msg, "Parameter 'sortkey1' not found in .par file");
        goto Finish;/**/
    }
    Uclgst("sortkey2", sortkey2, status);
    if(*status)
    {
        sprintf(msg, "Parameter 'sortkey2' not found in .par file");
        goto Finish;/**/
    }
    Uclgsi("order", order, status);
    if(*status)
    {
        sprintf(msg, "Parameter 'order' not found in .par file");
        goto Finish;/**/
    }
    Uclgsb("tcheck", tcheck, status);
    if(*status)
    {
        sprintf(msg, "Parameter 'tcheck' not found in .par file");
        goto Finish;/**/
    }
    Uclgst("extrap", temp, status);
    if(*status)
    {
        sprintf(msg, "Parameter 'extrap' not found in .par file");
        goto Finish;/**/
    }
    Uclgst("strnull", strnull, status);
    if(*status)
    {
        sprintf(msg, "Parameter 'strnull' not found in .par file");
        goto Finish;/**/
    }
    Uclgsi("intnull", intnull, status);
    if(*status)
    {
        sprintf(msg, "Parameter 'intnull' not found in .par file");
        goto Finish;/**/
    }

/* Check for obvious problems. */
    if(!strcmp(temp, "NULL")) *extrap = FNTRP_SET_NULL;
    else if(!strcmp(temp, "REPEAT")) *extrap = FNTRP_REPEAT;
    else if(!strcmp(temp, "IGNORE")) *extrap = FNTRP_IGNORE;
    else if(!strcmp(temp, "")) *extrap = FNTRP_SET_NULL;
    else
    {
        *status = FNTRP_BAD_PAR;
        sprintf(msg, "'extrap' must be NULL, REPEAT or IGNORE");
        goto Finish;/**/
    }
    if((*order < 0) || (*order > 2))
    {
        *status = FNTRP_BAD_PAR;
        sprintf(msg, "order must = 0, 1, or 2");
    }

Finish:
    WriteError(temp, *status);
    return *status;
}

/*******************************************************************************
Function: CopyThruExt

Description: Copies a series of extensions from one fits file to
    another. 

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    fitsfile *infp, *outfp - the source and destination fitsfiles
    int firstext - the number of the first extension in infp to copy
    int lastext - the number of the last extension in infp to copy
    int *status - program status

Functions called:
libcfitsio.a:
    ffXXXX:
*******************************************************************************/

int CopyThruExt(fitsfile *infp, fitsfile *outfp, int firstext,
    int lastext, int *status)
{
    int i, exttype = 0, primary = FALSE;

/* Check arguments */
    if(*status) return *status;
    else if(!infp || !outfp) *status = FNTRP_BAD_PTR;
    else if(lastext < firstext) *status = FNTRP_BAD_ARG;
    if(*status) return *status;

    firstext++; lastext++;
    ffmrhd(outfp, -1, &exttype, status);
    if(*status == BAD_HDU_NUM)
    {
        ffcmsg();
        *status = 0;
        primary = TRUE;
    }
    else
        ffmrhd(outfp, 1, &exttype, status);
    for(i = firstext; i <= lastext; i++)
    {
        ffmahd(infp, i, &exttype, status);
        if(!primary) ffcrhd(outfp, status);
        else primary = FALSE;
        ffcopy(infp, outfp, 0, status);
    }
    return *status;
}

/*****************************************************************************/
/*******************************************************************************
Function: WriteError

Description: write out error messages, using cfitsio's error stack

Author/Date: James Peachey, HSTX, 6/96

Modification History:

Notes:

Arguments:
    char *context - the error message
    int status - program status
*******************************************************************************/
int WriteError(char *context, int status)
{
    char fcerrmsg[C_FCERR_MSG],
         cfitsiomsg[FLEN_ERRMSG];

    if(!context) return status;
    if(status >= FNTRP_ERROR) 
    {
        sprintf(fcerrmsg, "%d - ", status);
        strncat(fcerrmsg, context, C_FCERR_MSG - strlen(fcerrmsg) - 1);
        c_fcerr(fcerrmsg);
    }
    else if(status)
    {
        if(strlen(context)) c_fcerr(context);
        c_fcerr("cfitsio error stack dump:");
        while(ffgmsg(cfitsiomsg)) c_fcerr(cfitsiomsg);
    }
    return status;
}

/*******************************************************************************
Function: NumListItems and CopyListItem

Description: Utility routines to query a character string containing a
comma-delimited list.

*******************************************************************************/
/* Given comma-delimited list in character string str, return number of items in str */
int NumListItems(char *str)
{
  char *p = 0;
  int n = 1;

  for (p=str; *p; p++) {
    if (*p == ',') n++;
  }

  return n;
}

/* Given comma-delimited list in character string str, copy the nth
   item to the dest string, where n=0 is the first item */
int CopyListItem(char *str, int n, char *dest)
{
  char *p = 0;
  char *start = str;
  int i = 0;
  int nchars;

  *dest = 0;
  if (n < 0 || n >= NumListItems(str)) return -1;

  for (p=str; *p; p++) {
    if (*p == ',') {
      if (i == n) break;

      /* Increment to next word...*/
      i++;
      start = p+1;
    }
  }

  /* Last item falls through */
  nchars = (p-start);
  if (nchars > 0) strncpy(dest, start, nchars);
  dest[nchars+1] = 0;
  return 0;
}

/*******************************************************************************
Function: strip

Description: Utility to remove leading and trailing spaces, tabs, commas
   and quotes from a string. If a null pointer is passed as the argument
   "out", memory is allocated dynamically for the "out" string. 

Author/Date: James Peachey, HSTX, 4/96

Modification History:

Notes:

Arguments:
    char *in, *out - the input and output strings.
*******************************************************************************/
char *strip(char *out, char *in)
{
    int st = 0, len;

    if(!in) return NULL;
    len = strlen(in);
    if(len > 0) len--;
    while ( (in[st] == '"') || (in[st] == '\'') || (in[st] == ',') ||
        (in[st] == ' ') || (in[st] == '\t') || (in[st] == '\n'))
        st++;
    while ( (in[len] == '"') || (in[len] == '\'') || (in[len] == ',') ||
        (in[len] == ' ') || (in[len] == '\t') || (in[len] == '\n'))
        len--;
    len = len - st + 1;
    if(len > 0)
    {
        if(!out) out = malloc((len + 1) * sizeof(char));
        if(out) strncpy(out, in + st, len);
        out[len] = '\0';
    }
    return out;
}

/*****************************************************************************/

/* The following code is required by IRAF. */
#ifdef vms
#define F77CALL finterp
#endif
#ifdef unix
#define F77CALL finterp_
#endif

void F77CALL()
{
    void Finterp();
    Finterp();
}

/*
$Log: finterp.c,v $
Revision 1.12  2015/12/30 21:07:01  craigm
Allow multiple column names in incolname; increase path name sizes to 1024 characters --CM

Revision 1.9  2002/12/23 17:13:12  irby
Define MAXHDU (=100000) since it was taken out of fitsio.h in cfitsio v2.430.

Revision 1.8  1999/02/08 21:41:43  peachey
Prevent writing past end of table

 * Revision 1.7  1998/06/22  20:59:06  pwilson
 * Fix comparison of in and out filenames, taking into account new extended
 * syntax filenames
 *
Revision 1.6  1997/10/07 15:15:05  peachey
Changed initialization of taskname to make consistent with libcftools changes

Revision 1.5  1997/10/03 20:40:19  peachey
Updated to make consistent with changes to libcftools.a: TASK Fortran common
block replaced by a simple C string variable

*/
