/* hlptohtml v3 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>		/* for EXIT_SUCCESS and EXIT_FAILURE */
#include <ctype.h>		/* for isdigit, isspace [modif2]    */

int 
main (int argc, char **argv)
{
/*      program hlptohtml ;
 *
 *   read .HLP file, convert to .HTML hierarchy ;
 *   
 *   Author:
 *      A.Daviel, TRIUMF,  16-FEB-1994  <advax@triumf.ca> ;
 *
 *   Modifications:
 *      1) Jerome Lauret <JLAURET@sbchem.sunysb.edu>
 *         some changes derived from FORTRAN version 22-Aug-94 AD
 *      2) R. Vankemmel (vankemme@imec.be): the VAX-VMS HELP
 *         takes a new level only if the level number is followed
 *         by a tab or a space; else it is treated as text.
 *      3) R. Vankemmel (vankemme@imec.be) 28-NOV 1994: 
 *         a line starting with the 
 *         exclamation mark is treated as comment in VMS HELP.
 *         A declaration bug for parts[][] (was parts[Lpart][maxlevel])
 *         caused bad file referencing when returning from deeper
 *         levels. Solved now. Did also some code indentation
 *         (does not hide the fact that it could be written better, but
 *          it works.....) and commenting. 
 *         Inserted also _DEBUG_ conditionals: easier
 *         to include debug code with definition on compiler command line.
 */

   char  **desc_pp;
   char   *descrip[] =
   {
      "HLPTOHTML: This program converts Digital's DCL HELP files ",
      "(.HLP suffix) to HTLM, the Hypertext Markup Language used",
      "by Mosaic. Separate files are produced for different levels",
      "in the HELP hierarchy, preserving the hierarchical nature",
      "of the Help information, and allowing use of the Mosaic",
      "Back command. The Hypertext ouput may be generated with the",
      "pre-formatted flag set (<pre>); the user may remove this",
      "and refine the presentation after conversion. Alternatively,",
      "pairs of <pre> and </pre> directives may be pre-inserted",
      "in the HELP file to protect tables, etc., and formatted",
      "output produced (the default).", " ",
      "Usage : the program expects a Help file prefix, eg. 'FOOBAR'.",
      "It will then read FOOBAR.HLP, creating the Hypertext files",
      "FOOBAR.HTML,FOOBAR_1.HTML,FOOBAR_2.HTML, etc. plus FOOBAR.HREF, a",
      "list of the references created which may be used to add",
      "further cross-references manually.",
      "Depending on the format of the original HLP file, the",
      "hypertext output may start either in FOOBAR.HTML or in",
      "FOOBAR_1.HTML", " ",
      "Options: -pre   Force pre-format flag",
      "         -path filepath   Insert filepath before filenames",
      "  in references.",
      0};


/* modif 3 (RVk): I debugged the code on HP: 
 * define macro for lib$signal() as this is VMS routine */
#ifndef vms
#define lib$signal(a)  printf("status val = %d\n",(a))
#endif


#define Lline 256

#define maxlevel 9		/* max. no. of levels in HLP file */
   FILE   *unit[maxlevel], *Frefs, *Finp;

#define Lfilenm 80
   char    filenm[Lfilenm];

#define Loption 5
   char    option[Loption];

#define Lpath 80
   char    path[Lpath];
   int     pathn;

#define Lprefix 30
   char    prefix[Lprefix];

#define Lword 132
   char    word[Lword];
   char    line[Lline], line2[Lline];
   int     nch, ni;

   char  **c_pp;

   int     level = 0, prev_level = 0;
   int     k, j, nl;
   int     partn = 0, refn = 0;

#define Lpart 34
   char    part[Lpart], parts[maxlevel][Lpart];

#define Lref 16
   char    ref[Lref];
   int     nquotes = 0;
   char    inch[1];
   int     pre_flag = 0;
   char   *pos;
   int     nexta = 1;

/*=====  start option processing =====*/
   /* Note RVK: maybe getopt() can be used; is it available on VMS ? */
   path[0] = 0;
   for (k = 1; k < argc; k++)
     {
	if (argv[k][0] == '-')
	  {
	     nexta = k + 1;
	     strcpy (option, argv[k]);
	     pos = strstr ("-help", option);
	     if (pos)
	       {
		  for (desc_pp = descrip; *desc_pp; desc_pp++)
		     printf ("%s\n", *desc_pp);
		  printf ("\n");
		  exit (EXIT_SUCCESS);
	       }
	     else if (!strcmp (option, "-pre"))
		pre_flag = 1;
	     else if (!strcmp (option, "-path"))
		if (k + 1 < argc)
		  {
		     strcpy (path, argv[k + 1]);
		     nexta++;

#ifdef _DEBUG_
		     printf ("OPTION: path=%s\n", path);
#endif
		  }
		else
		  {
		     fprintf (stderr, "Path missing\n");
		     exit (EXIT_FAILURE);
		  }
	     else
		fprintf (stderr, "Unrecognized option %s\n", option);
	  }			/*argv - */
     }				/*for k */

   if (argc < 2)
     {
	printf ("Usage: hlptohtml [-h[elp]][-pre][-path path] file_prefix\n");
	exit (EXIT_SUCCESS);
     }

   if (nexta > argc - 1)
     {
	fprintf (stderr, "Missing argument\n");
	exit (EXIT_FAILURE);
     }

   /* store global file name tree prefix */
   strcpy (prefix, argv[nexta]);

   /* open input file */
   strcpy (filenm, prefix);
   strcat (filenm, ".hlp");
   Finp = fopen (filenm, "r");
   if (!Finp)
     {
	lib$signal (Finp);
	fprintf (stderr, "Unable to open input file '%s'\n", filenm);
	exit (EXIT_FAILURE) /* exit (Finp) */ ;
     }

   /* open  reference file */
   strcpy (filenm, path);
   strcat (filenm, prefix);
   strcat (filenm, ".href");
   Frefs = fopen (filenm, "w");
   if (!Frefs)
     {
	lib$signal (Frefs);
	fprintf (stderr, "Unable to open href file '%s'\n", filenm);
	exit (EXIT_FAILURE);
     }
   fprintf (Frefs, "List of references for '%s'\n", prefix);

   /* create base level html file */
   strcpy (filenm, prefix);
   strcat (filenm, ".html");
   printf ("Creating file %s\n", filenm);
   unit[level] = fopen (filenm, "w");
   if (!unit[level])
     {
	lib$signal (unit[level]);
	fprintf (stderr, "Unable to open output file '%s'\n", filenm);
	exit (EXIT_FAILURE);
     }

   /* write header for new base file */
   fprintf (unit[level], "<html>\n<head>\n");
   fprintf (unit[level], "<title> %s.hlp</title>\n</head>\n", prefix);
   fprintf (unit[level], "<body>\n");
   if (pre_flag)
      fprintf (unit[level], "<pre>\n");		/* default pre-formatted  */

   /* =============== start help file processing: over all loop ========== */

 L2:
   fgets (line, Lline, Finp);	/* note RVK: could be replaced by while(..) */
   if (feof (Finp))
      goto L99;

   /* detect if it is simply text or defines a new level */
   nl = nl + 1;
   nch = strlen (line);
   if (nch > 0)
      line[--nch] = 0;		/* strip off \n */

   /* if input line is zero-length or spaces, put a para. */
   if ((nch <= 0) || (!strncmp (line, "       ", nch)))
     {
	if (level > 0)
	   fprintf (unit[level], "<p>\n");	/* paragraph */
	goto L2;
     };

   /* lines begining with ! are treated as comment lines [modif3]   */
   if (line[0] == '!')
      goto L2;

   if ((line[0] < '1') || (line[0] > '9'))
      goto L3;
   /* VAX-HELP recognizes a level only if the number is followed
    * by a space or tab; else it is treated as normal text [modif2] */
   if (!(isdigit (line[0]) && isspace (line[1])))
      goto L3;


   /* if we end up here, then it is a new level definition */
   ni = sscanf (line, "%d", &level);
   if (ni < 1)
      goto L3;

   /* get level name and store in "word" */
   strncpy (word, &line[1], nch);	/* nch-1 +1 for null */

   /* return one level higher */
   if (level < prev_level)
     {
	for (k = prev_level; k >= level + 1; --k)
	  {
	     if (pre_flag)
		fprintf (unit[k], "</pre>\n");
	     fprintf (unit[k], "</body>\n");
	     fprintf (unit[k], "</html>\n");	/* close <>"s in file */
	     fclose (unit[k]);	/* close all intermediate level files */
	     printf ("Closing unit %d\n", k);
	  };

	/* filename of higher level must be restored in order
	 * to be able to insert good reference links in "parent"
	 * file
	 */
	strcpy (part, parts[level]);

#ifdef _DEBUG_
	printf ("DEBUG:: level return\n current part=%s\n", part);
#endif

	strcpy (filenm, path);
	strcat (filenm, part);
	strcat (filenm, ".html");

	/* list in "parent" html file must be closed */
	fprintf (unit[level], "</dir>\n");
	if (pre_flag)
	   fprintf (unit[level], "<pre>\n");	/* restore pre-formatting  */
     };

   /* deeper level accessed: check if we skipped a level */
   if (level > prev_level + 1)
      fprintf (stderr, "ERROR - skipped level in HELP file\n");

   /* one level deeper: a new file must be created now */
   if (level > prev_level)
     {
	partn = partn + 1;	/* increment file counter */
	sprintf (part, "%s_%d", prefix, partn);
	strcpy (parts[level], part);	/* save filenames at each level */
	strcpy (filenm, path);
	strcat (filenm, part);
	strcat (filenm, ".html");
	printf ("Creating file %s\n", filenm);
	unit[level] = fopen (filenm, "w");
	if (!unit[level])
	  {
	     lib$signal (unit[level]);
	     fprintf (stderr, "Unable to open output file '%s'\n", filenm);
	  }

	/* write header for new file */
	fprintf (unit[level], "<html>\n<head>\n");
	fprintf (unit[level], "<title> %s.hlp</title>\n</head>\n", prefix);
	fprintf (unit[level], "<body>\n");
	if (pre_flag)
	   fprintf (unit[level], "<pre>\n");	/* default pre-formatted  */
	if (pre_flag)
	   fprintf (unit[prev_level], "</pre>\n");
	if (level > 1)
	   fprintf (unit[prev_level],
		    "<p>Additional Information on:<br><dir>\n");
	else
	   fprintf (unit[prev_level],
		    "<p>Information available on:<br><dir>\n");
     }				/* (level > prev_level)   */

   /* independent of level: insert html links or achor names for
    * document references into current file and into the "parent" file
    */
   refn = refn + 1;
   sprintf (ref, "Ref%d", refn);

   /* write keyword in new file as heading and name */
   fprintf (unit[level], "<a name=\"%s\"><h2>%s</h2></a>\n", ref, word);

   /* write keyword in parent file as href and list item */
   fprintf (unit[level - 1],
	    "<li><a href=\"%s#%s\">%s</a>\n", filenm, ref, word);

   /* write in list of references */
   fprintf (Frefs, "<a href=\"%s#%s\">%s</a>\n", filenm, ref, word);

   /* adjust prev_level for next loop */
   prev_level = level;
   goto L2;


   /* ============= simple text processing ======================== */
   /*      support for pre-conditioning of HLP file formatting */
 L3:
   if ((strstr (line, "<pre>") != 0 || strstr (line, "<PRE>") != 0)
       || (strstr (line, "</pre>") != 0 || strstr (line, "</PRE>") != 0)
       || (strstr (line, "&") == 0 &&
	   strstr (line, ">") == 0 && strstr (line, "<") == 0))
     {
	fprintf (unit[level], "%s\n", line);
	goto L2;
     }
   else
     {
	/* quote HTML reserved characters */
	j = 0;
	for (k = 0; k <= nch; ++k)
	  {
	     if (line[k] == '&')
	       {
		  strcpy (&line2[j], "&amp;");
		  j = j + 5;
	       }
	     else if (line[k] == '<')
	       {
		  strcpy (&line2[j], "&lt;");
		  j = j + 4;
	       }
	     else if (line[k] == '>')
	       {
		  strcpy (&line2[j], "&gt;");
		  j = j + 4;
	       }
	     else
	       {
		  line2[j++] = line[k];
		  line2[j] = 0;
	       };
	  } /* for k */ ;
	fprintf (unit[level], "%s\n", line2);
	nquotes = nquotes + 1;
	goto L2;
     } /* quote reserved */ ;

   /* =============== finish processing ========================= */
 L99:
   /* text and level processing finished: close files */
   for (k = level; k >= 0; --k)
     {
	if (pre_flag)
	   fprintf (unit[k], "</pre>\n");
	fprintf (unit[k], "</body>\n");
	fprintf (unit[k], "</html>\n");		/* close <>"s in file  */
	fclose (unit[k]);	/* close all intermediate level files  */
	printf ("Closing unit %d\n", k);
     };

   printf ("HTML reserved characters were quoted on %d lines\n", nquotes);
   close (Frefs);
   close (Finp);
   exit (EXIT_SUCCESS);
}				/* main */
