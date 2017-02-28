/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotapplywcs/uvotapplywcs.c,v $
 * $Revision: 1.4 $
 * $Date: 2008/04/08 20:42:41 $
 *
 *
 * $Log: uvotapplywcs.c,v $
 * Revision 1.4  2008/04/08 20:42:41  rwiegand
 * Was printing the wrong variables for transformations to pixels.
 *
 * Revision 1.3  2008/04/04 21:57:14  rwiegand
 * Display wcsprm structure at high chatter.  Edit CTYPEia keywords that
 * indicate -TAN-SIP since they confuse current wcslib.
 *
 * Revision 1.2  2008/04/02 20:25:00  rwiegand
 * Allow user to specify output record format.
 *
 * Revision 1.1  2008/04/02 19:54:26  rwiegand
 * Tool for converting between pixel and world coordinates using wcslib.
 *
 */


#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#include "headas.h"
#include "pil.h"
#include "wcslib/wcsutil.h"
#include "wcslib/wcshdr.h"
#include "fitsio.h"
#include "report.h"
#include "uvottool.h"


#define TOOLSUB uvotapplywcs
#include "headas_main.c"


#define VERSION 1.0



enum
{
	OP_PIX_TO_WORLD,
	OP_WORLD_TO_PIX,
	OP_WORLD_TO_WORLD,
	OP_COUNT
};


typedef struct
{
  int chatter;
  int clobber;
  int history;

  char infile[PIL_LINESIZE];
  char outfile[PIL_LINESIZE];
  char wcsfile[PIL_LINESIZE];

	char buffer[PIL_LINESIZE];
	int operation;

	int from;
	int to;

	char format[PIL_LINESIZE];
	int inprec;
	int outprec;

} Parameters;


static int
wcsAlternate (const char * s)
{
	int alt = -1;

	if (strlen(s) != 1)
		report_error("wcs system must be a one character long\n");
	else if (s[0] == '-')
		alt = 0;
	else if (islower(s[0]))
		alt = s[0] - 'a' + 1;
	else if (isupper(s[0]))
		alt = s[0] - 'A' + 1;
	else
		report_error("invalid WCS system identifier '%s'\n", s);

	return alt;
}


int
get_parameters (Parameters * p)
{
  int code = 0;
  int pill = 0;

  if (!pill)
    pill = PILGetFname("infile", p->infile);

  if (!pill)
    pill = PILGetFname("outfile", p->outfile);

  if (!pill)
    pill = PILGetFname("wcsfile", p->wcsfile);

  if (!pill) {
    pill = PILGetString("operation", p->buffer);
		if (!pill) {
			if (!strcasecmp(p->buffer, "PIX_TO_WORLD"))
				p->operation = OP_PIX_TO_WORLD;
			else if (!strcasecmp(p->buffer, "WORLD_TO_PIX"))
				p->operation = OP_WORLD_TO_PIX;
			else if (!strcasecmp(p->buffer, "WORLD_TO_WORLD"))
				p->operation = OP_WORLD_TO_WORLD;
			else {
				pill = -1;
				report_error("unexpected operation '%s'\n", p->buffer);
			}
		}
	}

  if (!pill) {
    pill = PILGetString("from", p->buffer);
		if (!pill) {
			p->from = wcsAlternate(p->buffer);
			if (p->from < 0)
				pill = -1;
		}
	}

  if (!pill) {
    pill = PILGetString("to", p->buffer);
		if (!pill) {
			p->to = wcsAlternate(p->buffer);
			if (p->to < 0)
				pill = -1;
		}
	}

  if (!pill)
    pill = PILGetString("format", p->format);

  if (!pill)
    pill = PILGetInt("inprec", &p->inprec);

  if (!pill)
    pill = PILGetInt("outprec", &p->outprec);

  p->clobber = headas_clobpar;
  p->chatter = headas_chatpar;
  get_history(&p->history);

  if (pill)
    {
      code = TASK_SETUP_ERROR;
      report_error("unable to load parameters\n");
    }
  else
    report_verbose("parameters loaded\n");

  return code;
}


int run_parameters (Parameters * p)
{
	int code = 0;
	int status = 0;
	fitsfile * fptr = 0;
	FILE * fpin = 0;
	FILE * fpout = 0;

	int tmp = 0;
	int nkeys = 0;
	char * header = 0;
	int nwcs = 0;
	struct wcsprm * wcs;
	struct wcsprm * fromWCS = 0;
	struct wcsprm * toWCS = 0;
	int indices[27];
	int noutput = 0;
	char format[1024];
	int nformat = 0;


	if (!code) {
		if (!strcasecmp(p->format, "DEFAULT"))
			sprintf(format, "%%.%df %%.%df %%.%df %%.%df\n",
					p->inprec, p->inprec, p->outprec, p->outprec);
		else {
			char c;
			const char * pc = p->format;
			int active = 0;

			/* this is not clever enough to check that the fields are real */
			while ((c = *pc++) != 0) {
				if (active && c != '%') { ++nformat; active = 0; }
				if (!active && c == '%') { active = 1; }
			}

			if (nformat == 1)
				report_warning("only outputting new x\n");
			else if (nformat == 3)
				report_warning("found 3 fields in format\n");

			strcpy(format, p->format);
			strcat(format, "\n");
		}

		report_status("using format %s", format);
	}

	if (!code) {
		fpin = fopen(p->infile, "r");
		if (!fpin) {
			code = TASK_INPUT_ERROR;
			report_error("unable to open infile '%s' [%d]\n",
					p->infile, errno);
		}
	}

	if (!code) {
		fits_open_file(&fptr, p->wcsfile, READONLY, &status);
		if (status) {
			code = TASK_INPUT_ERROR;
			report_error("unable to open wcsfile '%s' [%d]\n",
					p->wcsfile, status);
		}
	}

	if (!code) {
		headas_clobberfile(p->outfile);
		fpout = fopen(p->outfile, "w");
		if (!fpout) {
			code = TASK_INPUT_ERROR;
			report_error("unable to create outfile '%s' [%d]\n",
					p->outfile, errno);
		}
	}

	if (!code) {
		int nocomments = 1;
		int exclude = 0;
		if (fits_hdr2str(fptr, nocomments, NULL, exclude,
				&header, &nkeys, &status)) {
			code = TASK_INPUT_ERROR;
			report_error("unable to load FITS header [%d]\n",
					p->outfile, status);
		}
		else
			report_verbose("loaded %d keywords\n", nkeys);
	}

	if (!code) {
		int i;
		char * p = header;
		char buffer[40];
		char * tansip;
		for (i = 0; i < nkeys; ++i, p += 80) {
			/* CTYPEna = '1234-123-123' */
			if (strncmp(p, "CTYPE", 5))
				continue;
			strncpy(buffer, p+8, sizeof(buffer));
			buffer[sizeof(buffer) - 1] = 0;
			if ((tansip = strstr(buffer, "-TAN-SIP'")) != 0) {
				p[tansip - buffer + 12] = '\'';
				p[tansip - buffer + 13] = ' ';
				p[tansip - buffer + 14] = ' ';
				p[tansip - buffer + 15] = ' ';
				p[tansip - buffer + 16] = ' ';
			}
		}
	}

	if (!code) {
		int relax = 0;
		int ctrl = 2;
		int nreject = 0;
		tmp = wcspih(header, nkeys, relax, ctrl, &nreject, &nwcs, &wcs);
		if (tmp) {
			code = TASK_INPUT_ERROR;
			report_error("wcspih failed [%d]\n", tmp);
		}
		if (nreject > 0)
			report_warning("wcspih rejected %d keywords\n", nreject);
	}

	if (!code) {
		tmp = wcsidx(nwcs, &wcs, indices);
		if (tmp) {
			code = TASK_INPUT_ERROR;
			report_error("wcsidx failed [%d]\n", tmp);
		}
	}

	if (!code) {
		if (p->operation == OP_WORLD_TO_PIX
				|| p->operation == OP_WORLD_TO_WORLD) {
			if (indices[p->from] < 0) {
				code = TASK_INPUT_ERROR;
				report_error("missing from WCS\n");
			}
			else
				fromWCS = &wcs[indices[p->from]];
		}
		if (p->operation == OP_PIX_TO_WORLD
				|| p->operation == OP_WORLD_TO_WORLD) {
			if (indices[p->to] < 0) {
				code = TASK_INPUT_ERROR;
				report_error("missing to WCS\n");
			}
			else
				toWCS = &wcs[indices[p->to]];
		}
	}

	if (!code && fromWCS) {
		tmp = wcsset(fromWCS);
		if (tmp) {
			code = TASK_INPUT_ERROR;
			report_error("wcsset from failed [%d]\n", tmp);
		}
		else if (p->chatter > 3) {
			report_status("from WCS:\n");
			wcsprt(fromWCS);
		}
	}

	if (!code && toWCS) {
		tmp = wcsset(toWCS);
		if (tmp) {
			code = TASK_INPUT_ERROR;
			report_error("wcsset to failed [%d]\n", tmp);
		}
		else if (p->chatter > 3) {
			report_status("to WCS:\n");
			wcsprt(toWCS);
		}
	}

	if (!code) {
#define ncoord 1
#define nelem 2
		double oldx, oldy;
		double newx, newy;
		double pixcrd[nelem];
		double imgcrd[nelem];
		double phi[ncoord];
		double theta[ncoord];
		double world[nelem];
		int stat[ncoord];

		while (!code && fscanf(fpin, "%lf %lf", &oldx, &oldy) == 2) {
			if (p->operation == OP_PIX_TO_WORLD) {
				pixcrd[0] = oldx;
				pixcrd[1] = oldy;
				tmp = wcsp2s(toWCS, ncoord, nelem, pixcrd,
						imgcrd, phi, theta, world, stat);
				newx = world[0];
				newy = world[1];
			}
			else if (p->operation == OP_WORLD_TO_PIX) {
				world[0] = oldx;
				world[1] = oldy;
				tmp = wcss2p(fromWCS, ncoord, nelem, world,
						phi, theta, imgcrd, pixcrd, stat);
				newx = pixcrd[0];
				newy = pixcrd[1];
			}
			else if (p->operation == OP_WORLD_TO_WORLD) {
				world[0] = oldx;
				world[1] = oldy;
				tmp = wcss2p(fromWCS, ncoord, nelem, world,
						phi, theta, imgcrd, pixcrd, stat);
				if (!tmp)
					tmp = wcsp2s(toWCS, ncoord, nelem, pixcrd,
							imgcrd, phi, theta, world, stat);
				newx = world[0];
				newy = world[1];
			}

			if (tmp) {
				code = TASK_OUTPUT_ERROR;
				report_error("%dth conversion failed [%d]\n",
						noutput + 1, tmp);
			}
			else {
				/* it is going to be strange if nformat == 3 anyway */
				if (nformat == 1)
					fprintf(fpout, format, newx);
				else if (nformat == 2)
					fprintf(fpout, format, newx, newy);
				else
					fprintf(fpout, format, oldx, oldy, newx, newy);
				++noutput;
			}
		}

		fclose(fpout);
		fclose(fpin);

		report_status("converted %d positions\n", noutput);
	}

	if (!code)
		wcsvfree(&nwcs, &wcs);

	return code;
}


int
TOOLSUB ()
{
  int code = 0;
  Parameters p = { 0 };

  set_toolname(_STRINGIFY(TOOLSUB));
  set_toolversion(_STRINGIFY(VERSION));

  add_report_function(&report_headas);

  if (!code)
    code = get_parameters(&p);

  if (!code)
    code = run_parameters(&p);

  remove_report_function(&report_headas);

  return code;
}
