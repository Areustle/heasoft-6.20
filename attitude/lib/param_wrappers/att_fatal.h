#ifndef ATTFATAL_INCLUDED
#define ATTFATAL_INCLUDED


#include <setjmp.h>


void att_fatal (int code);
jmp_buf * att_fatal_jump_buffer ();


#ifdef TOOLSUB
#define ATT_CONCAT0(a,b) a ## b
#define ATT_CONCAT(a,b) ATT_CONCAT0(a,b)
#define TOOLSUBAUX ATT_CONCAT(TOOLSUB, _aux)


int TOOLSUB (void);
void TOOLSUBAUX (void);


int TOOLSUB ()
{
	int code;
	jmp_buf * buffer = att_fatal_jump_buffer();
	code = setjmp(*buffer);
	if (!code)
		TOOLSUBAUX();
	return code;
}


#endif /* TOOLSUB */


#endif /* FATAL_INCLUDED */
