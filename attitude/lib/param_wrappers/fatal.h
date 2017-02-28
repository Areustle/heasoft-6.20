#ifndef FATAL_INCLUDED
#define FATAL_INCLUDED

#ifdef TOOLSUB

#include <setjmp.h>

#include "headas.h"


#define TOOLSUBAUX TOOLSUB ## _aux

static jmp_buf jumpFATAL;

static void TOOLSUBAUX (void);

int TOOLSUB (void) {
	int code;
	code = setjmp(jumpFATAL);
	if (!code)
		TOOLSUBAUX();
	return code;
}


void fatal (int code)
{
	headas_chat(0, "fatal error... aborting\n");
	longjmp(jumpFATAL, code);
}

#else

void fatal (int code);

#endif /* TOOLSUB */

#endif /* FATAL_INCLUDED */
