#include <stdio.h>
#include <stdlib.h>
#include "anl.h"
#include "bnk.h"
#include "anl_def.h"

int
main(int argc, char **argv)
{
	int i;
	float f;
	double d;
	char s[256];
	char *server;

	if ( argc < 2 ) {
		server = "localhost";
	} else {
		server = argv[1];
	}

	BnkIni(DEFAULT_BNK_BUF_SIZE);

	BnkDef("BNKTEST:INT:V1", sizeof(int));
	BnkDef("BNKTEST:FLOAT:V1", sizeof(float));
	BnkDef("BNKTEST:DOUBLE:V1", sizeof(double));
	BnkDef("BNKTEST:STRING:V1", 256);

	BnkConnect(server);

	BnkExport("BNKTEST:INT:V1");
	BnkExport("BNKTEST:FLOAT:V1");
	BnkExport("BNKTEST:DOUBLE:V1");
	BnkExport("BNKTEST:STRING:V1");

	BnkEqv("BNKTEST:INT:V2", sizeof(int), "BNKTEST:INT:V1", 1);

	for (i = 0; i < 5; i++) {
		int used;

		f = i;
		d = i;
		sprintf(s, "%d", i);

		printf("put: %d, %f, %f, %s\n", i, f, d, s);
		BnkPut("BNKTEST:INT:V1", sizeof(i), &i);
		BnkPut("BNKTEST:FLOAT:V1", sizeof(f), &f);
		BnkPut("BNKTEST:DOUBLE:V1", sizeof(d), &d);
		BnkPut("BNKTEST:STRING:V1", strlen(s), s);

		/*sleep(1);*/

		BnkGet("BNKTEST:INT:V1", sizeof(i), &used, &i);
		BnkGet("BNKTEST:FLOAT:V1", sizeof(f), &used, &f);
		BnkGet("BNKTEST:DOUBLE:V1", sizeof(d), &used, &d);
		BnkGet("BNKTEST:STRING:V1", sizeof(s), &used, s);
		printf("get: %d, %f, %f, %s\n", i, f, d, s);

		/*sleep(1);*/

	}

	return 0;
}
