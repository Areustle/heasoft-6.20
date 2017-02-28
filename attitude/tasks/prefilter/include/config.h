#ifndef PREFILTER_CONFIG_H
#define PREFILTER_CONFIG_H


#ifdef HAVE_offsetof
#include <stdlib.h>
#else
#  ifndef offsetof
#  define offsetof(type, member) \
	(size_t) (((char *) &(((type *) 0)->member)) - 0)
#  endif
#endif


#endif
