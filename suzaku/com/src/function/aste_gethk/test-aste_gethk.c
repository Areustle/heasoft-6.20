/*
	test-aste_gethk_fits.c

	2004-04-04 Y.ISHISAKI	version 2.0
		created for aste_gethk version 2.0
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "anl.h"
#include "cli.h"
#include "fitsio.h"
#include "aste_gethk.h"

#define MAX_HK_NUM	256

int
main(int argc, char **argv)
{
	static struct {
		int id;
		int typecode;
		char *hk_name;
		double stime[3];
		union {
			unsigned char t_byte;
			short t_short;
			int t_int;
			long t_long;
			float t_float;
			double t_double;
		} data;
	} *p, hklist[MAX_HK_NUM];

	int i, typecode, status;
	double t0, t1, aste_time, next_aste_time;
	char filename[256], hk_name[256];
	ASTE_HK *aste_hk;
	int hk_num;

	CLprom("");

	for (i = 1; i < argc; i++) {
		CLugetrd(argv[argc-i], strlen(argv[argc-i]));
	}

	filename[0] = '\0';
	CLtxtrd("file name", filename, sizeof(filename));
	t0 = t1 = 0.0;
	CLfdprd("start time", &t0);
	CLfdprd("end time", &t1);

	aste_hk = aste_gethk_init(filename);
	if ( NULL == aste_hk ) {
		return -1;
	}

	for (hk_num = 0; hk_num < MAX_HK_NUM; hk_num++) {
		static char *table[] = {
			"TBYTE", "TSHORT", "TINT", "TLONG",
			"TFLOAT", "TDOUBLE", "END"
		};
		static int ntable = sizeof(table) / sizeof(*table);
		char *p, word[80];
		int choice;

		word[0] = '\0';
		CLkeyrd(-1, "HK typecode", word, table, ntable, &choice, sizeof(word));
		p = table[choice - 1];
		if ( 0 == strcmp("TBYTE", p) ) {
			typecode = TBYTE;
		} else if ( 0 == strcmp("TSHORT", p) ) {
			typecode = TSHORT;
		} else if ( 0 == strcmp("TINT", p) ) {
			typecode = TINT;
		} else if ( 0 == strcmp("TLONG", p) ) {
			typecode = TLONG;
		} else if ( 0 == strcmp("TFLOAT", p) ) {
			typecode = TFLOAT;
		} else if ( 0 == strcmp("TDOUBLE", p) ) {
			typecode = TDOUBLE;
		} else if ( 0 == strcmp("END", p) ) {
			break;
		}
		hk_name[0] = '\0';
		CLtxtrd("HK name", hk_name, sizeof(hk_name));
		hklist[hk_num].typecode = typecode;
		hklist[hk_num].hk_name = strdup(hk_name);
		hklist[hk_num].id = aste_gethk_id(aste_hk, hk_name);
		if ( hklist[hk_num].id < 0 ) {
			return -1;
		}
	}

	aste_time = t0;
	while ( t0 <= aste_time && aste_time <= t1 ) {
		printf("aste_time=%.8f\n", aste_time);
		for (i = 0; i < hk_num; i++) {
			p = &hklist[i];
			status = aste_gethk(p->id, aste_time, p->typecode, 1, &p->data, p->stime);
			if ( status ) {
				return -1;
			}
			printf("%s = ", p->hk_name);
			switch (p->typecode) {
			case TBYTE:
				printf("%d", p->data.t_byte);
				break;
			case TSHORT:
				printf("%d", p->data.t_short);
				break;
			case TINT:
				printf("%d", p->data.t_int);
				break;
			case TLONG:
				printf("%ld", p->data.t_long);
				break;
			case TFLOAT:
				printf("%f", p->data.t_float);
				break;
			case TDOUBLE:
				printf("%f", p->data.t_double);
				break;
			default:
				;
			}
			printf(", stime[] = %.3f / %+.3f / %+.3f\n",
				   p->stime[0],
				   p->stime[1] - p->stime[0],
				   p->stime[2] - p->stime[0]
				   );
			if ( t0 < t1 ) {
				if ( 0 == i || p->stime[2] < next_aste_time ) {
					next_aste_time = p->stime[2];
				}
			} else {
				if ( 0 == i || next_aste_time < p->stime[1] ) {
					next_aste_time = p->stime[1];
				}
			}
		}
		if ( aste_time == next_aste_time ) {
			printf("aste_time == next_aste_time, quit\n");
			break;
		}
		aste_time = next_aste_time;
	}

	return 0;
}
