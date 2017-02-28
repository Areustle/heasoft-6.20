#include <stdio.h>
#include <stdlib.h>
#include "anl.h"
#include "bnk.h"
#include "anl_def.h"

int
main(int argc, char **argv)
{
	int port = 0;
	int bnk_buf_size = DEFAULT_BNK_BUF_SIZE;

	if ( 1 < argc ) {
		port = atoi(argv[1]);;
	}

	if ( 2 < argc ) {
		bnk_buf_size = atoi(argv[2]);
	}

	BnkIni(bnk_buf_size);
	if ( BnkServer(port) ) {
		return 1;
	}

	return 0;
}
