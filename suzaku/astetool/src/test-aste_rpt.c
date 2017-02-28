/* $Id: test-aste_rpt.c,v 1.9 2007/05/07 16:45:32 ishisaki Exp $ */
/*******************************************************************

	test-aste_rpt.c		Test program of aste_rpt.c

	2005-01-11	Y.ISHISAKI	version 1.0

************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "fitsio.h"
#include "aste_rpt.h"
#include "atFunctions.h"
#include "aste_time.h"

static char *pname = "test-aste_rpt";

void
usage(void)
{
	fprintf(stderr, "\
usage: %s rpt-file [vcid_and_mask vcid_xor_mask apid_and_mask apid_xor_mask ...]\n", pname);
}

int
main(int argc, char **argv)
{
	static int *vcid_and_mask, *vcid_xor_mask, *apid_and_mask, *apid_xor_mask;

	char *filename;
	int i, cpn_cnt;

	RPTFILE *rpt;
	char ccsds_packet[65535+10];
	int istat;
	double s_time, r_time;
	unsigned int ti;
	int vcid, apid;
	int ccsds_packet_size;
	AtTimeD attime;

	if ( argc < 2 ) {
		usage();
		return 0;
	}

	filename = argv[1];

	argc = argc - 2;
	argv = argv + 2;

	cpn_cnt = argc / 4;
	if ( cpn_cnt ) {
		vcid_and_mask = malloc(4*cpn_cnt*sizeof(int));
		if ( NULL == vcid_and_mask ) {
			fprintf(stderr, "\
%s: malloc() failed\n", pname);
			return 1;
		}
		vcid_xor_mask = vcid_and_mask + cpn_cnt;
		apid_and_mask = vcid_xor_mask + cpn_cnt;
		apid_xor_mask = apid_and_mask + cpn_cnt;
		for (i = 0; i < cpn_cnt; i++) {
			sscanf(argv[4*i+0], "%x", &vcid_and_mask[i]);
			sscanf(argv[4*i+1], "%x", &vcid_xor_mask[i]);
			sscanf(argv[4*i+2], "%x", &apid_and_mask[i]);
			sscanf(argv[4*i+3], "%x", &apid_xor_mask[i]);
		}
	}

	istat = aste_rpt_open(&rpt, filename, cpn_cnt,
				vcid_and_mask, vcid_xor_mask, apid_and_mask, apid_xor_mask);

	for (;;) {
		istat = aste_rpt_read(rpt, sizeof(ccsds_packet),
					&s_time, &r_time, &ti, &vcid, &apid,
					ccsds_packet, &ccsds_packet_size);
		if ( istat ) break;

		aste2attimeD(s_time, &attime);
		printf("\
S_TIME=%.7f: %04d/%02d/%02d %02d:%02d:%02d.%03d\n",
			s_time, attime.yr, attime.mo, attime.dy,
			attime.hr, attime.mn, attime.sc, (int)(attime.ss*1000));
		aste2attimeD(r_time, &attime);
		printf("\
R_TIME=%.7f: %04d/%02d/%02d %02d:%02d:%02d.%03d\n",
			r_time, attime.yr, attime.mo, attime.dy,
			attime.hr, attime.mn, attime.sc, (int)(attime.ss*1000));
		printf("\
TI=%08xh, VCID=%02xh, APID=%03xh, ccsds_packet_size=%d\n",
			ti, vcid, apid, ccsds_packet_size);
		for (i = 0; i < ccsds_packet_size; i++) {
			if ( 0 == i % 16 ) {
				printf("%04x: ", i);
			}
			printf("%02x", ccsds_packet[i] & 255);
			if ( 15 == i % 16 || ccsds_packet_size - 1 == i ) {
				putchar('\n');
			} else {
				putchar(' ');
			}
		}
		printf("\n");
	}

	istat = aste_rpt_close(rpt);

	return 0;
}
