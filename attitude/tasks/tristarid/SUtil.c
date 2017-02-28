/*
 * $Source: /headas/headas/attitude/tasks/tristarid/SUtil.c,v $
 * $Revision: 1.1 $
 * $Date: 2005/08/27 12:52:19 $
 *
 * $Log: SUtil.c,v $
 * Revision 1.1  2005/08/27 12:52:19  wiegand
 * Initial revision
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SUtil.h"



char * string_copy (const char * s)
{
	int length = strlen(s);
	char * copy = malloc(length + 1);
	strcpy(copy, s);
	return copy;
}


/*
 - split - divide a string into fields, like awk split()
 = int split(char *string, char *fields[], int nfields, char *sep);
string              string to split
fields[]            list is not NULL-terminated
nfields             number of entries available in fields[]
sep                 "" white, "c" single char, "ab" [ab]+
 */

int /* number of fields, including overflow */
string_split (char *string, char * fields[], int nfields, const char * sep)
{
	char *p = string;
	char c;			/* latest character */
	char sepc = sep[0];
	char sepc2;
	int fn;
	char **fp = fields;
	const char *sepp;
	int trimtrail;

	/* white space */
	if (sepc == '\0') {
		while ((c = *p++) == ' ' || c == '\t')
			continue;
		p--;
		trimtrail = 1;
		sep = " \t";	/* note, code below knows this is 2 long */
		sepc = ' ';
	} else
		trimtrail = 0;
	sepc2 = sep[1];		/* now we can safely pick this up */

	/* catch empties */
	if (*p == '\0')
		return(0);

	/* single separator */
	if (sepc2 == '\0') {
		fn = nfields;
		for (;;) {
			*fp++ = p;
			fn--;
			if (fn == 0)
				break;
			while ((c = *p++) != sepc)
				if (c == '\0')
					return(nfields - fn);
			*(p-1) = '\0';
		}
		/* we have overflowed the fields vector -- just count them */
		fn = nfields;
		for (;;) {
			while ((c = *p++) != sepc)
				if (c == '\0')
					return(fn);
			fn++;
		}
		/* not reached */
	}

	/* two separators */
	if (sep[2] == '\0') {
		fn = nfields;
		for (;;) {
			*fp++ = p;
			fn--;
			while ((c = *p++) != sepc && c != sepc2)
				if (c == '\0') {
					if (trimtrail && **(fp-1) == '\0')
						fn++;
					return(nfields - fn);
				}
			if (fn == 0)
				break;
			*(p-1) = '\0';
			while ((c = *p++) == sepc || c == sepc2)
				continue;
			p--;
		}
		/* we have overflowed the fields vector -- just count them */
		fn = nfields;
		while (c != '\0') {
			while ((c = *p++) == sepc || c == sepc2)
				continue;
			p--;
			fn++;
			while ((c = *p++) != '\0' && c != sepc && c != sepc2)
				continue;
		}
		/* might have to trim trailing white space */
		if (trimtrail) {
			p--;
			while ((c = *--p) == sepc || c == sepc2)
				continue;
			p++;
			if (*p != '\0') {
				if (fn == nfields+1)
					*p = '\0';
				fn--;
			}
		}
		return(fn);
	}

	/* n separators */
	fn = 0;
	for (;;) {
		if (fn < nfields)
			*fp++ = p;
		fn++;
		for (;;) {
			c = *p++;
			if (c == '\0')
				return(fn);
			sepp = sep;
			while ((sepc = *sepp++) != '\0' && sepc != c)
				continue;
			if (sepc != '\0')	/* it was a separator */
				break;
		}
		if (fn < nfields)
			*(p-1) = '\0';
		for (;;) {
			c = *p++;
			sepp = sep;
			while ((sepc = *sepp++) != '\0' && sepc != c)
				continue;
			if (sepc == '\0')	/* it wasn't a separator */
				break;
		}
		p--;
	}

	/* not reached */
}
