
#include <stdlib.h>

#include "prefilter.h"
#include "derive.h"
#include "report.h"


int get_argument_code (const Arguments * args)
{
	return args->code;
}


int get_context_code (const Context * context)
{
	return context->code;
}


void clear_context_code (Context * context)
{
	context->code = 0;
}


int get_derived_code (const Derived * derived)
{
	return derived->code;
}


int add_iterator (Iteration * iter, Iterator * f)
{
	/* take care of memory */
	if (iter->count == iter->allocated)
		{
			int request = iter->allocated + 10;
			Iterator ** p = malloc(request * sizeof(Iterator *));
			if (!p)
				{
					iter->count = -1;
					report_error("unable to allocate %d pointers",
						request);
				}
			else
				{
					if (iter->functions)
						{
							int i;
							for (i = 0; i < iter->count; ++i)
								p[i] = iter->functions[i];

							free(iter->functions);
						}

					iter->functions = p;
					iter->allocated = request;
				}
		}

	/* if we are here and happy, assign the pointer */
	if (iter->count >= 0)
		iter->functions[iter->count++] = f;

	return iter->count;
}


int iterate_context (const Arguments * args, Context * context)
{
	int i;
	Iteration * iter = args->iteration;

	for (i = 0; !context->code && (i < iter->count); ++i)
		(iter->functions[i])(args, context);

	return context->code;
}


void iterate_error (const Arguments * args, Context * context,
		const char * module, const char * function)
{
	report_t info = { 0 };
	info.module = module;

	if (!context->code)
		context->code = PREFILTER_ERROR;

	report_error("%s failed at %s\n",
			function, context->timestampString
			);

	context->havePosition = 0;
	context->haveVelocity = 0;
	context->haveQuaternion = 0;
}


void context_error (Context * context,
		const char * module, const char * function)
{
	report_t info = { 0 };
	info.module = module;

	if (!context->code)
		context->code = PREFILTER_ERROR;

	report_detail(&info, REPORT_ERROR,
			"%s failed at %s\n",
			function, context->timestampString
			);
}


void derive_error (Derived * derived,
		const char * module, const char * function)
{
	report_t info = { 0 };
	info.module = module;

	if (!derived->code)
		derived->code = PREFILTER_ERROR;

	report_detail(&info, REPORT_ERROR,
			"%s failed at %s\n",
			function, derived->context->timestampString
			);
}


