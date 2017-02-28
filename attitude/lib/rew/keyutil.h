#ifndef KEYUTIL_H
#define KEYUTIL_H


#include "fitsio.h"


typedef struct
{
	char keyname[FLEN_KEYWORD];
	char value[FLEN_VALUE];
	char comment[FLEN_COMMENT];

	char card[FLEN_CARD];

} HeaderRecord;


typedef struct
{
	int space;
	int count;
	HeaderRecord * records;

	int (* accept) (const HeaderRecord * record, void * user);
	void * user;

	int warnings;
	int decimals;
	int keep_all;
	int get_or_set;

} FITSHeader;



void release_header (FITSHeader * header);

int fetch_header_records_path (FITSHeader * header, const char * path);
int fetch_header_records_fits (FITSHeader * header, fitsfile * from);
int header_hide_standard_comments (FITSHeader * header);

int update_header_records_path (FITSHeader * header, const char * path);
int update_header_records_fits (FITSHeader * header, fitsfile * to);

int copy_named_header_records (fitsfile * from, fitsfile * to,
		const char ** names);

int accept_nonstructural_records (const HeaderRecord * record, void * user);
int accept_keycard_types (const HeaderRecord * record, void * user);

int add_header_record (FITSHeader * header, const char * key, const char * record);
int add_header_comment (FITSHeader * header, const char * comment);

int get_header_key_double (const FITSHeader * header, const char * key,
		double * dest, double * _default);
int get_header_key_string (const FITSHeader * header, const char * key,
		char * dest, char * _default);
int get_header_key_long (const FITSHeader * header, const char * key,
		long * dest, long * _default);
int get_header_key_integer (const FITSHeader * header, const char * key,
		int * dest, int * _default);

int set_header_key_double (FITSHeader * header, const char * key,
		double value);
int set_header_key_string (FITSHeader * header, const char * key,
		const char * value);
int set_header_key_long (FITSHeader * header, const char * key,
		long value);

#endif

