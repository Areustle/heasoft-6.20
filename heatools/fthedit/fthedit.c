#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"

#define MAXMSG 256

/*
HISTORY
-------
  Version 1.0 written by William Pence, NASA/GSFC, March 2002
*/

#define TOOLSUB fthedit
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int fthedit (void);
int fthedit_getpar(char *infile, char *editfile, char *operation, char *keyword,
    char *value, char *unit, char *comment, char *insert, int *protect, 
    int *longstring);
int fthedit_file (char *infile, char *editfile, int protect, int longstring);
int fthedit_parameters(char *infile, char *operation, char *keyword,
    char *value, char *unit, char *comment, char *insert, int protect, int longstring);
int ffc2s(char *str1, char *str2, int *status);
/*---------------------------------------------------------------------------*/
int fthedit (void)
{
/*
    Edit header keywords in the input HDU.
*/
    char infile[PIL_LINESIZE], editfile[PIL_LINESIZE], operation[40];
    char keyword[PIL_LINESIZE], value[PIL_LINESIZE];
    char unit[100], comment[100], insert[100];
    int protect, longstring, status;
    static char taskname[80] = "fthedit";
    static char version[8] = "2.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = fthedit_getpar(infile, editfile, operation,
         keyword, value, unit, comment, insert, &protect, &longstring);

    if (status) return(status);

    /* call work function to edit the HDU header */

    if (*editfile)  

        /* Case 1: edit commands are given in a text file */
        status = fthedit_file(infile, editfile, protect, longstring);

    else

        /* Case 2: single edit command given via parameters */
        status = fthedit_parameters(infile, operation, keyword,
           value, unit, comment, insert, protect, longstring);

    return(status);
}
/*---------------------------------------------------------------------------*/
int fthedit_getpar(
    char *infile,     /* O - Input file name */
    char *editfile,   /* O - edit file name, or 'none' */
    char *operation,  /* O  (a)dd, (d)elete, or (r)eplace */
    char *keyword,    /* O  name of keyword to edit */
    char *value,      /* O  value for keyword (if 'add') */
    char *unit,       /* O  optional unit string for keyword */
    char *comment,    /* O  optional comment string for keyword */
    char *insert,     /* O  insert position */
    int *protect,     /* O  protect structural keywords? */
    int *longstring)  /* O  allow long string keywords? */

/*  read input parameters for the fthedit task from the .par file */
{
    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
        sprintf(msg, "Error reading the 'infile' parameter.");
        HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetString("keyword", keyword))) {
        sprintf(msg, "Error reading the 'keyword' parameter.");
        HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("protect", protect))) {
        sprintf(msg, "Error reading the 'protect' parameter.");
        HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("longstring", longstring))) {
        sprintf(msg, "Error reading the 'longstring' parameter.");
        HD_ERROR_THROW(msg,status);
    }

    /* 'keyword' is the name of a keyword, or a filename preceded by '@'. */ 
    else if (*keyword == '@') {

        /* this is the name of a text file */
        strcpy(editfile, keyword + 1);
        *keyword = '\0';

    } else {

        *editfile = '\0';  /* set null string */

        if ((status = PILGetString("operation", operation))) {
            sprintf(msg, "Error reading the 'operation' parameter.");
            HD_ERROR_THROW(msg,status);
        }

        else if (*operation != 'D') {  /* The following parameters only */
                                /* needed for 'add' or 'replace' operations. */

            if ((status = PILGetString("value", value))) {
               sprintf(msg, "Error reading the 'value' parameter.");
               HD_ERROR_THROW(msg,status);
            }
       
            else if (*operation == 'A') {  /* The following parameters only */
                                        /* needed for 'add' operations.  */
                if ((status = PILGetString("unit", unit))) {
                   sprintf(msg, "Error reading the 'unit' parameter.");
                   HD_ERROR_THROW(msg,status);
                }

                else if ((status = PILGetString("comment", comment))) {
                   sprintf(msg, "Error reading the 'comment' parameter.");
                   HD_ERROR_THROW(msg,status);
                }

                else if ((status = PILGetString("insert", insert))) {
                   sprintf(msg, "Error reading the 'insert' parameter.");
                   HD_ERROR_THROW(msg,status);
                }
            }
        }
    }
    return(status);
}
/*---------------------------------------------------------------------------*/
int fthedit_file(
    char *infile,     /* I - Input file name */
    char *editfile,   /* I - edit file name, or 'none' */
    int   protect,    /* I - protect structural keywords? */
    int   longstring)  /* I - allow long string keywords? */

/*
    Edit header keywords, using text file containing templates
    of the keywords to insert or delete in the input HDU.
*/
{
    fitsfile *infptr = 0;
    FILE *template = 0;
    int status = 0, tstatus = 0, keytype, keyclass, ii, jj;
    char line[1000], card[FLEN_CARD], keyword[FLEN_KEYWORD];
    char msg[MAXMSG];

    /* Open the input file */
    /* Move to first 'interesting' HDU if none was specified. */
    if (fits_open_data(&infptr, infile, READWRITE, &status) ) goto cleanup;
    headas_chat(5,"Opened the following input file:\n %s\n",infile);

    /* Open the text file containing the edit instructions */
    if (!(template = fopen(editfile, "r"))) {
        sprintf(msg, "Error: could not open edit template file.");
        status = FILE_NOT_OPENED;
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }
    headas_chat(5,"Opened the following edit template file:\n %s\n",editfile);

    /* process template lines, one by one */
    for (ii = 1; fgets(line, 1000, template); ii++) {

        /* replace CR and newline chars at end of line with nulls */
        jj = strlen(line) - 1;

        if ( ( jj >= 0) && (line[jj] == '\n' || line[jj] == '\r'))
        {
            line[jj] = '\0';
            jj--;
            if ( ( jj >= 0) && (line[jj] == '\n' || line[jj] == '\r'))
                 line[jj] = '\0';
        }

        headas_chat(5,"Read template line %d:\n %s\n",ii,line);

        /* skip comment lines in the template file */
        if (*line == '#') continue;

        /* skip blank lines (with < 8 spaces) in the template file */
        jj = strspn(line, " ");   /* no. of leading spaces */
        if ( (jj < 8) && (line[jj] == '\0') ) continue;

        /* parse the template string; return a formatted, 80-char string */
        if (fits_parse_template(line, card, &keytype, &status) )goto cleanup;

        if (protect) {  /* make sure this is not a structural keyword */
               keyclass = fits_get_keyclass(card);

            if ((keyclass <= TYP_CMPRS_KEY) &&
                   (strncmp(card, "TTYPE",    5)) &&
                   (strncmp(card, "COMMENT ", 8)) ) {

                   sprintf(msg,
                     "Warning: The following keyword is write-protected:\n%s",card);
                   HD_ERROR_THROW(msg,status);
  
                   continue;
            }
        }

        switch (keytype) {

          case -2:
            /* rename a keyword */
            /* current name starts at card[0]; new name starts at card[40] */

            /* delete trailing blanks */
            for (jj = 39; jj && (card[jj] == ' '); jj--)
               card[jj] = '\0';
           
            for (jj = 79; jj && (card[jj] == ' '); jj--)
               card[jj] = '\0';

            if (!fits_modify_name(infptr, card, card+40, &status) )
               headas_chat(5," Renamed keyword '%s' to '%s'.\n",card, card+40);

            if (status == KEY_NO_EXIST) {
                sprintf(msg,
              "WARNING: Could not rename '%s'; keyword does not exist.\n",card);
                HD_ERROR_THROW(msg,status);
                status = 0;
            }
            break;

          case -1:
            /* delete a keyword */
            if (!fits_delete_key(infptr, card, &status) )
                headas_chat(5," Deleted keyword '%s'.\n",card);

            if (status == KEY_NO_EXIST) {
                sprintf(msg,
              "WARNING: Could not delete '%s'; keyword does not exist.",card);
                HD_ERROR_THROW(msg,status);
                status = 0;
            }
            break;
 
          case 0:
            /* update or append a keyword */
            *keyword = '\0';
            strncat(keyword, card, 8);
            if (!fits_update_card(infptr, keyword, card, &status))
                headas_chat(5," Updated keyword:\n %s\n",card);
            break;

          case 1:
            /* write COMMENT or HISTORY record */
            if (!fits_write_record(infptr, card, &status) )
               headas_chat(5," Wrote commentary keyword:\n %s\n",card);
            break;
        }
        if (status) goto cleanup;
    }

    HDpar_stamp(infptr, 0, &status); /* write history keywords */

cleanup:

    /*  close files and go home */
    if (template) fclose(template);
  
    /* use tstatus = 0 to properly close the file, even if errors occurred */
    if (infptr) fits_close_file(infptr, &tstatus);

    return(status);
}
/*---------------------------------------------------------------------------*/
int fthedit_parameters(
    char *infile,     /* O - Input file name */
    char *operation,  /* (a)dd, (d)elete, or (r)eplace */
    char *keyword,    /* name of keyword to edit */
    char *value,      /* value for keyword (if 'add') */
    char *unit,       /* optional unit string for keyword */
    char *comment,    /* optional comment string for keyword */
    char *insert,     /* insert position */
    int   protect,    /* protect structural keywords? */
    int   longstring) /* allow long string keywords? */
{
/*
    Edit a single header keyword, as specified by the task parameters.

    If operation = add, then the keyword value and comment will be
    updated if the keyword already exists, or will be added to the
    header if it doesn't exist.  The 'value', 'unit', and 'comment'
    parameter give the corresponding items for the keyword. If 'insert'
    is not a  null string, it should give the name or sequence number
    of the keyword after which the new keyword should be inserted.
    Inserting a keyword within the initial set of required keywords in
    the header is not allowed.

    If operation = delete, then the specified keyword will be deleted.
    The value, unit, comment, and insert parameters are ignored.

    If operation = deleteall, then all instances of the specified keyword 
    will be deleted. The value, unit, comment, and insert parameters 
    are ignored.

    If operation = replace, then the first header record that contains
    the string specified by 'keyword' will be replaced in it's entirety
    by the string given by 'value' (in other words, 'value' gives the
    complete new header record).  The unit, comment, and insert
    parameters are ignored in this case.

    If protect is TRUE, then the required keywords that define the
    structure of the HDU (SIMPLE, BITPIX, NAXIS, NAXISn, GROUPS,
    GCOUNT, PCOUNT, TFORMn, and TBCOLn will be protected from
    modification.
*/

    fitsfile *infptr = 0;
    char template[3000], card[FLEN_CARD], newcard[FLEN_CARD], *junk;
    int keytype, keyclass, index, status = 0, ii, jj, valuelen;
    char msg[MAXMSG];

    /* Open the input fileMove to first 'interesting' HDU if none specified. */
    if (fits_open_data(&infptr, infile, READWRITE, &status) ) goto cleanup;
    headas_chat(5,"Opened the following input file:\n %s\n",infile);

    headas_chat(5,"Keyword operation = %s\n", operation);

    if (!strncmp(operation, "DELETEALL", 9)) {    /* delete all instances of a keyword */

        if (protect) {  /* make sure this is not a structural keyword */
           keyclass = fits_get_keyclass(keyword);

           if ((keyclass <= TYP_CMPRS_KEY) &&
               (strncmp(keyword, "TTYPE",    5)) &&
               (strncmp(keyword, "COMMENT ", 8)) ) {

               sprintf(msg, 
               "ERROR: The '%s' keyword is write-protected.", keyword);
               status = 1;
               HD_ERROR_THROW(msg,status);
           }
        }

        while (!status) {
            if (!fits_delete_key(infptr, keyword, &status) )
               headas_chat(5," Deleted keyword '%s'.\n",keyword);
	}
	
        /* reset the status */
	if (status == KEY_NO_EXIST)
	    status = 0;

    } else if (*operation == 'D') {    /* delete a keyword */

        if (protect) {  /* make sure this is not a structural keyword */
           keyclass = fits_get_keyclass(keyword);

           if ((keyclass <= TYP_CMPRS_KEY) &&
               (strncmp(keyword, "TTYPE",    5)) &&
               (strncmp(keyword, "COMMENT ", 8)) ) {

               sprintf(msg, 
               "ERROR: The '%s' keyword is write-protected.", keyword);
               status = 1;
               HD_ERROR_THROW(msg,status);
           }
        }

        if (!fits_delete_key(infptr, keyword, &status) )
            headas_chat(5," Deleted keyword '%s'.\n",keyword);

    } else if (*operation == 'R') {    /* replace one record with another */

        for(ii = 1; !status; ii++) { /* search from beginning of header */
           fits_read_record(infptr, ii, card, &status);

           if (protect) {  /* make sure this is not a structural keyword */
              keyclass = fits_get_keyclass(card);
              if ( (keyclass <= TYP_CMPRS_KEY) &&
                   (strncmp(card, "TTYPE", 5)) &&
                   (strncmp(card, "COMMENT ", 8)) )
                 continue;  /* it is protected, so loop to next keyword */
           }

           /* Test if this card contains the search string. */
           /* If so, delete it, then insert the replacement card. */
           if (strstr(card, keyword) ) {

              for (jj = 0; jj < 8; jj++)
                  value[jj] = toupper(value[jj]); /* upper case keyword name */
              
              headas_chat(5,"Testing legality of card (keyword = value) '%s'.\n",value);
              if (fits_test_keyword(value, &status) ) /* illegal keyword? */
                  break;

              fits_delete_record(infptr, ii, &status);

              /* parse the template string; return a formatted, 80-char string */
              headas_chat(5,"Parsing card '%s'.\n",value);
              if (fits_parse_template(value, newcard, &keytype, &status) ) break;

              if (!fits_insert_record(infptr, ii, newcard, &status) )
                  headas_chat(5,"Replaced the card:\n %s\nwith this:\n %s\n", card, newcard);
              break;
           }
        }
        if (status == KEY_OUT_BOUNDS) {
           sprintf(msg, 
           "ERROR: Keyword containing the following string not found or is protected:\n '%s'",
            keyword);
            HD_ERROR_THROW(msg,status);
        }

    } else if (*operation == 'A') {    /* add a keyword */

        /* construct the free format keyword template */
        strcpy(template, keyword);  /* keyword name */ 
        strcat(template, " ");      /* space */
        strcat(template, value);    /* value field */
        if (*unit) {
           strcat(template, " [");
           strcat(template, unit);
           strcat(template, "]");
        }
        strcat(template, " ");
        strcat(template, comment);

        /* parse the template string; return a formatted, 80-char string */
        if (fits_parse_template(template, card, &keytype, &status) ) {
           sprintf(msg, "ERROR: invalid keyword template\n keyword = %s\n value = %s\n unit = %s\n comment = %s",
           keyword,value, unit,comment);
           HD_ERROR_THROW(msg,status);
           goto cleanup;
        }

        headas_chat(5, "The following keyword template:\n %s\n", template);
        headas_chat(5, "was parsed and reformated to:\n %s\n", card);

        if (protect) {  /* make sure this is not a structural keyword */
           keyclass = fits_get_keyclass(card);
           if ((keyclass <= TYP_CMPRS_KEY) &&
               (strncmp(card, "TTYPE",    5)) &&
               (strncmp(card, "COMMENT ", 8)) ) {

               sprintf(msg, 
             "ERROR: Cannot update or append this write-protected keyword:\n%s",card);
               status = 1;
               HD_ERROR_THROW(msg,status);
               goto cleanup;
           }
        }

        if (*insert) {
           /* see if 'insert' is an integer sequence number */
           index = strtol(insert, &junk, 10);
           if (*junk || index < 1) { /* this can't be a sequence number */
              /* so, instead, look for keyword with this name */
              if (!fits_read_card(infptr, insert, template, &status) ) {
                 /* found it; insert card at the current position */
                 if (!fits_insert_card(infptr, card, &status) ) {
                    headas_chat(5,
                    "Inserted new keyword after this keyword:\n %s\n",
                      template);
                 }
              }
              else {  /* insert keyword not found */
                  sprintf(msg, 
                      "Could not find insert keyword: '%s',\n",insert);
                   HD_ERROR_THROW(msg,status);
              }
           }
           else {  /* insert after the (index)th keyword */

              if (protect && (index < 4 )) {
                 sprintf(msg, 
                 "ERROR: cannot insert keyword at protected position %d.\n",
                 index);
                 status = 1;
                 HD_ERROR_THROW(msg,status);
              }
              else if (fits_read_record(infptr, index, template, &status) ) {
                 sprintf(msg, "Could not insert after keyword %d.\n",index);
                 HD_ERROR_THROW(msg,status);
              }
              else if (!fits_insert_card(infptr, card, &status) ) {
                   headas_chat(5,"Inserted new keyword at position %d.\n",
                     index+1);
              }
           }
        }
        else {  /* not insert, so simply update or append the keyword */
           if (keytype == 1) {  /* HISTORY or COMMENT keyword */
              if (!fits_write_record(infptr, card, &status) )
                headas_chat(5,"Appended this commentary keyword to the HDU.\n");
           }
	   else
	   {
	     if (longstring) {
	     /* test if this is a string keyword, and if so, use the special */
	     /* routine to write a long string (> 68 characters) */

                if (*value == '\'')  /* remove the enclosing quote characters */
		  ffc2s(value, template, &status);
		else
		  strcpy(template, value);

		valuelen = strlen(template);
		
		if (valuelen > 66) {
		    ffukls(infptr, keyword, template, comment, &status);
                    headas_chat(5, "Updated or appended the long string keyword.\n");
		}
                else if (!fits_update_card(infptr, keyword, card, &status) )
                   headas_chat(5, "Updated or appended the keyword.\n");
	     }
             else if (!fits_update_card(infptr, keyword, card, &status) )
              headas_chat(5, "Updated or appended the keyword.\n");
           }
        }
    }

    HDpar_stamp(infptr, 0, &status); /* write history keywords */

cleanup:

    if (infptr) fits_close_file(infptr, &status);
    return(status);
}
