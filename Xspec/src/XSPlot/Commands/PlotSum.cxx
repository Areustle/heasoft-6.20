//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>

// PlotSum
#include <XSPlot/Commands/PlotSum.h>


// Class PlotSum 

PlotSum::PlotSum()
  : PlotCommand("sum", new CreateBinnedPlotGroups(false,false))
{
}


PlotSum::~PlotSum()
{
}


void PlotSum::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
}

// Additional Declarations
