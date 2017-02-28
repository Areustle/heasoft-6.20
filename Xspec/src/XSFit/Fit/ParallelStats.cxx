#include <XSContainer.h>
#include <XSFit/Fit/ParallelStats.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSUtil/Error/Error.h>

void ParallelStats::execute(const bool isParallel, const TransferStruct& input,
                        TransferStruct& output)
{
   using namespace XSContainer;
   
   // Assume input parameter values have been verified to
   //  fall within allowed range.
   const std::vector<double>& parValuesVec = input.dValues[0];
   output.status=-1;
   if (parValuesVec.size())
   {
      RealArray parValues(&parValuesVec[0],parValuesVec.size());
      fit->setVariableParameterValues(parValues, 'v');
      fit->statManager()->performStats();
      Real newStatVal = fit->statistic();

      output.dValues.clear();
      output.dValues.push_back(std::vector<double>(1, newStatVal));
      output.status=0;
   }
   else
      throw YellowAlert();
}
