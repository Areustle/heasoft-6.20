#include <XSFit/Fit/FitErrorOutput.h>
#include <XSFit/Fit/FitErrorCalc.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSstreams.h>
#include <XSUtil/Utils/XSstream.h>
#include <sstream>
#include <iomanip>

SingleErrorOutput::SingleErrorOutput()
   : FitErrorOutput()
{
}

SingleErrorOutput::~SingleErrorOutput()
{
}

void SingleErrorOutput::reportParameter(FitErrorCalc* context, int verbose) const
{
  using std::setw;

  const ModParam* param = context->parameter();
  std::ios_base::fmtflags save(tcout.flags());
  tcout << xsverbose(verbose);
  tcout.precision(6);
  tcout << setw(6) << param->index() << setw(13) << param->value('m')
        << setw(13) << param->value('p') << "    (" 
	<< param->value('m') - param->value() << ","
	<< param->value('p') - param->value() << ")" << std::endl;
  tcout.flags(save);
  tcout << xsverbose();
}

void SingleErrorOutput::writeMsg(FitErrorCalc* context, const string& msg, int verbose) const
{
   tcout << xsverbose(verbose) << msg << std::endl << xsverbose();
}

void SingleErrorOutput::reportException(FitErrorCalc* context, const string& msg) const
{
   tcerr << msg << std::endl;
}

ParallelErrorOutput::ParallelErrorOutput()
   : FitErrorOutput()
{
}

ParallelErrorOutput::~ParallelErrorOutput()
{
}

void ParallelErrorOutput::reportParameter(FitErrorCalc* context, int verbose) const
{
  using std::setw;

  const ModParam* param = context->parameter();
  std::ostringstream os;
  os.precision(6);
  os << setw(6) << param->index() << setw(13) << param->value('m')
        << setw(13) << param->value('p') << "    (" 
	<< param->value('m') - param->value() << ","
	<< param->value('p') - param->value() << ")";
  context->msgQueue().push(std::pair<string,int>(os.str(), verbose));
}

void ParallelErrorOutput::writeMsg(FitErrorCalc* context, const string& msg, int verbose) const
{
   context->msgQueue().push(std::pair<string,int>(msg, verbose));
}

void ParallelErrorOutput::reportException(FitErrorCalc* context, const string& msg) const
{
   context->msgQueue().push(std::pair<string,int>(msg, -1));
}


