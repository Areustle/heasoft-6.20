
#include <stdio.h>

#include "att_fatal.h"


jmp_buf * att_fatal_jump_buffer ()
{
	static jmp_buf buffer;
	return &buffer;
}


void att_fatal (int code)
{
	jmp_buf * buffer;
	printf("att_fatal: code=%d\n", code);
 	buffer = att_fatal_jump_buffer();
	longjmp(*buffer, code);
}


