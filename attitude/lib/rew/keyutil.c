/*
 * $Source: /headas/headas/attitude/lib/rew/keyutil.c,v $
 * $Revision: 1.11 $
 * $Date: 2008/07/02 15:33:06 $
 *
 * $Log: keyutil.c,v $
 * Revision 1.11  2008/07/02 15:33:06  rwiegand
 * Support COMMENT and HISTORY keywords.
 *
 * Revision 1.10  2008/03/07 21:29:08  rwiegand
 * Updated default handling of header keywords.
 *
 * Revision 1.9  2007/09/14 21:33:12  rwiegand
 * Extract string values from quotes.
 *
 * Revision 1.8  2005/03/04 19:31:12  rwiegand
 * Take care that keywords do not appear multiple times in header.
 *
 * Revision 1.7  2004/12/02 21:26:35  rwiegand
 * Added craziness to preserve COMMENT and HISTORY keywords while not
 * duplicating the built-in comments.
 *
 * Revision 1.6  2004/10/17 11:57:02  rwiegand
 * Added method to grab all keycards matching a given list of types.
 *
 * Revision 1.5  2004/03/04 15:53:52  rwiegand
 * Added support for integer valued keywords.
 *
 * Revision 1.4  2003/09/04 17:41:21  rwiegand
 * Added polygon utilities.  When iterating over images, allow specifying a
 * region (less than the whole image), or the step size (other than every pixel).
 * Allow resizing a FITSHeader memory buffer.
 *
 * Revision 1.3  2003/07/28 21:12:20  rwiegand
 * More keyword support.
 *
 * Revision 1.2  2003/07/25 20:02:15  rwiegand
 * Updates for WCS compliance.
 *
 * Revision 1.1  2003/05/14 13:41:39  rwiegand
 * Support for logging and reading/writing FITS images and keywords.
 *
 */

#include <stdlib.h>
#include <string.h>

#include "keyutil.h"
#include "fitsio.h"
#include "fitsio2.h"
#include "report.h"

#define KEYWORD_DECIMALS 8


int
allocate_header_records (FITSHeader * header, int number)
{
	int code = 0;
	void * p;

	if (!header->records)
		{
			header->space = 0;
			header->count = 0;
 			p = malloc(number * sizeof(HeaderRecord));
		}
	else
		{
 			p = realloc(header->records, number * sizeof(HeaderRecord));
		}

	if (!p)
		{
			code = 1;
			report_error("unable to allocate %d header records\n", number);
		}
	else
		{
			header->records = p;
			header->space = number;
		}

	return code;
}


void
release_header (FITSHeader * header)
{
	if (header->records)
		free(header->records);
}


int
fetch_header_records_path (FITSHeader * header, const char * path)
{
	int code = 0;
	int status = 0;
	fitsfile * fptr = 0;

	fits_open_file(&fptr, path, READONLY, &status);
	if (status)
		{
			code = 1;
			report_error("unable to open %s [%d]\n", path, status);
		}

	if (!code)
		code = fetch_header_records_fits(header, fptr);

	if (fptr)
		{
			status = 0;
			fits_close_file(fptr, &status);
			if (status)
				{
					code = 1;
					report_error("unable to close %s [%d]\n", path, status);
				}
		}

	return code;
}


int
fetch_header_records_fits (FITSHeader * header, fitsfile * from)
{
	int code = 0;
	int status = 0;
	int count = 0;
	int more = 0;

	if (!code)
		{
			fits_get_hdrspace(from, &count, &more, &status);
			if (status)
				{
					code = 1;
					report_error("unable to determine header space\n");
				}
		}

	if (!code)
		code = allocate_header_records(header, count);

	if (!code)
		{
			int i;
			for (i = 1; !status && i <= count; ++i)
				{
					HeaderRecord * p = header->records + header->count;

					fits_read_record(from, i, p->card, &status);
					fits_read_keyn(from, i, p->keyname, p->value, p->comment, &status);

					if (!header->accept || header->accept(p, header->user)) {
						++header->count;
						if (p->value[0] == '\'') {
							/* clean up string values */
							int length = strlen(p->value);
							if (length > 1 && p->value[length-1] == '\'') {
								char tmp[80];
								length -= 2;
								while (p->value[length] == ' ')
									--length;
								strncpy(tmp, &p->value[1], length);
								tmp[length] = 0;
								strcpy(p->value, tmp);
							}
						}
					}
				}

			if (status)
				{
					code = 2;
					report_error("unable to read header records\n");
				}
		}

	if (!code && !header->keep_all)
		header_hide_standard_comments(header);

	return code;
}


int
update_header_records_path (FITSHeader * header, const char * path)
{
	int code = 0;
	int status = 0;
	fitsfile * fptr = 0;

	fits_open_file(&fptr, path, READWRITE, &status);
	if (status)
		{
			code = 1;
			report_error("unable to open %s [%d]\n", path, status);
		}

	if (!code)
		code = update_header_records_fits(header, fptr);

	if (fptr)
		{
			status = 0;
			fits_close_file(fptr, &status);
			if (status)
				{
					code = 1;
					report_error("unable to close %s [%d]\n", path, status);
				}
		}

	return code;
}


int
update_header_records_fits (FITSHeader * header, fitsfile * to)
{
	int code = 0;
	int status = 0;
	int i;

	for (i = 0; !status && i < header->count; ++i)
		{
			HeaderRecord * p = header->records + i;

			if (!p->card[0])
				continue;

			if (!header->accept || header->accept(p, header->user))
				{
					int type = fits_get_keyclass(p->card);
					if (type == TYP_COMM_KEY || type == TYP_CONT_KEY)
						fits_write_record(to, p->card, &status);
					else
						fits_update_card(to, p->keyname, p->card, &status);
				}
		}

	if (status)
		{
			code = 1;
			report_error("unable to write header records\n");
		}

	return code;
}


int header_hide_standard_comments (FITSHeader * header)
{
static const char * standard_comments[] = {
	"FITS (Flexible Image Transport System) format is defined in 'Astronomy",
	"and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H ",
	0
};

	int i;
	int hidden = 0;
	int limit = 8;
	if (header->count < limit)
		limit = header->count;

	for (i = 0; i < limit; ++i)
		{
			HeaderRecord * p = header->records + i;
			const char ** c;
			int type = fits_get_keyclass(p->card);
			if (type == TYP_COMM_KEY)
				for (c = standard_comments; *c; ++c)
					if (!memcmp(*c, p->card + 10, 64)) {
						/* 10 is strlen("COMMENT = "),
							64 is ~ the length of the comments */
						p->card[0] = 0;
						++hidden;
						break;
					}
		}

	return hidden;
}



static int
detect_keyname_match (const HeaderRecord * record, void * user)
{
	int match = 0;
	const char ** current = user;

	while (!match && *current && **current)
		if (!strcmp(record->keyname, *current))
			match = 1;
		else
			++current;

	return match;
}



int
copy_named_header_records (fitsfile * from, fitsfile * to,
								const char ** keywords)
{
	int code = 0;
	FITSHeader header = { 0 };

	header.accept = detect_keyname_match;
	header.user = keywords;

	code = fetch_header_records_fits(&header, from);
	header.accept = 0;
	if (!code)
		code = update_header_records_fits(&header, from);

	return code;
}


int
accept_nonstructural_records (const HeaderRecord * record, void * user)
{
	int accept = 0;
	HeaderRecord * h = (HeaderRecord *) record;
	if (fits_get_keyclass(h->card) > TYP_CMPRS_KEY)
		accept = 1;
	return accept;
}


int
accept_keycard_types (const HeaderRecord * record, void * user)
{
	HeaderRecord * h = (HeaderRecord *) record;
	const int * p = (int *) user;
	int type = fits_get_keyclass(h->card);

	while (*p) {
		if (*p == type)
			return 1;
		else
			++p;
	}
	return 0;
}


enum
{
	KEY_MISSING = 1,
	KEY_INVALID,
	KEY_FOUND
};


HeaderRecord *
query_header_record (const FITSHeader * header, const char * key)
{
	int i;

	for (i = 0; i < header->count; ++i)
		{
			HeaderRecord * p = header->records + i;
			if (!strcmp(p->keyname, key))
				return p;
		}

	return 0;
}


int
get_header_key_double (const FITSHeader * header, const char * key,
		double * dest, double * _default)
{
	int code = 0;
	HeaderRecord * p = query_header_record(header, key);

	if (p)
		{
			int status = 0;
			if (ffc2d(p->value, dest, &status))
				code = KEY_INVALID;
		}

	if (!code && p)
		; /* done */
	else if (code)
		; /* failed */
	else if (_default)
		{
			*dest = *_default;
			if (header->get_or_set)
				code = set_header_key_double((FITSHeader*) header, key, *_default);
		}
	else
		{
			code = KEY_MISSING;
			if (header->warnings)
				report_warning("header missing key '%s'\n", key);
		}

	return code;
}


int
get_header_key_string (const FITSHeader * header, const char * key,
		char * dest, char * _default)
{
	int code = 0;
	HeaderRecord * p = query_header_record(header, key);

	if (p)
		strcpy(dest, p->value);

	if (!code && p)
		; /* done */
	else if (code)
		; /* failed */
	else if (_default)
		{
			strcpy(dest, _default);
			if (header->get_or_set)
				code = set_header_key_string((FITSHeader*) header, key, _default);
		}
	else
		{
			code = KEY_MISSING;
			if (header->warnings)
				report_warning("header missing key '%s'\n", key);
		}

	return code;
}


int
get_header_key_long (const FITSHeader * header, const char * key,
		long * dest, long * _default)
{
	int code = 0;
	HeaderRecord * p = query_header_record(header, key);

	if (p)
		{
			int status = 0;
			if (ffc2i(p->value, dest, &status))
				code = KEY_INVALID;
		}

	if (!code && p)
		; /* done */
	else if (code)
		; /* failed */
	else if (_default)
		{
			*dest = *_default;
			if (header->get_or_set)
				code = set_header_key_long((FITSHeader*) header, key, *_default);
		}
	else
		{
			code = KEY_MISSING;
			if (header->warnings)
				report_warning("header missing key '%s'\n", key);
		}

	return code;
}


int
get_header_key_integer (const FITSHeader * header, const char * key,
		int * dest, int * _default)
{
	int code;
	long lvalue;
	long ldefault = _default ? *_default : 0;
	long * pdefault = _default ? &ldefault : 0;

	code = get_header_key_long(header, key, &lvalue, pdefault);

	*dest = (int) lvalue;

	return code;
}


int
add_header_record (FITSHeader * header, const char * key, const char * record)
{
	int code = 0;
	int novel = 0;
	int force = 0;
	HeaderRecord * p = 0;

	if (!strcmp(key, "HISTORY") || !strcmp(key, "COMMENT"))
		force = 1;

	if (!force)
		p = query_header_record(header, key);

	if (!p)
		{
			novel = 1;

			if (header->space == header->count)
				code = allocate_header_records(header, header->count + 100);

			if (!code)
				p = header->records + header->count;
		}

	if (!code)
		{
			int length;
			int * pstatus = &code;

			strcpy(p->card, record);

			fits_get_keyname(p->card, p->keyname, &length, pstatus);
			fits_parse_value(p->card, p->value, p->comment, pstatus);
		}

	if (!code && novel)
		++header->count;

	return code;
}


int
add_header_comment (FITSHeader * header, const char * comment)
{
	int code = 0;
	char card[FLEN_CARD];
	char format[32];

	sprintf(format, "COMMENT   %%-%ds", FLEN_VALUE-1);
	sprintf(card, format, comment);

	code = add_header_record(header, "COMMENT", card);

	return code;
}


int
set_header_key_double (FITSHeader * header, const char * key,
								double value)
{
	int code = 0;

	char card[FLEN_CARD];
	char number[FLEN_CARD];
	char comment[FLEN_COMMENT] = "";
	int decimals = header->decimals ? header->decimals : KEYWORD_DECIMALS;
	int status = 0;

	ffd2f(value, decimals, number, &status);
	ffmkky((char *) key, number, comment, card, &status);
	code = status;

	if (!code)
		code = add_header_record(header, key, card);

	return code;
}


int
set_header_key_string (FITSHeader * header, const char * key,
		const char * value)
{
	int code = 0;

	char card[FLEN_CARD];
	char quoted[FLEN_VALUE];
	char comment[FLEN_COMMENT] = "";
	int status = 0;

	ffs2c((char *) value, quoted, &status);
	ffmkky((char *) key, quoted, comment, card, &status);
	code = status;

	if (!code)
		code = add_header_record(header, key, card);

	return code;
}


int
set_header_key_long (FITSHeader * header, const char * key,
								long value)
{
	int code = 0;

	char card[FLEN_CARD];
	char number[FLEN_CARD];
	char comment[FLEN_COMMENT] = "";
	int status = 0;

	ffi2c(value, number, &status);
	ffmkky((char *) key, number, comment, card, &status);
	code = status;

	if (!code)
		code = add_header_record(header, key, card);

	return code;
}


