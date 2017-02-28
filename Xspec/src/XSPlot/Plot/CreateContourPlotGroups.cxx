//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSContainer.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/Step.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSFit/MCMC/ChainManager.h>
#include <XSFit/MCMC/MarginGrid.h>
#include <XSFit/MCMC/IntegProbGrid.h>
#include <XSFit/MCMC/Chain.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Numerics/ModularCounter.h>

// for debugging
#include <XSstreams.h>
#include <sstream>


// CreateContourPlotGroups
#include <XSPlot/Plot/CreateContourPlotGroups.h>
Grid* ContourPolicy<Step>::getGrid()
{
   return XSContainer::fit->stepGrid();
}

void ContourPolicy<Step>::validate(const Grid* grid)
{
    if ( !grid )
    {
        // throw immediately if no step operation has ever been computed.
        throw YellowAlert( "Steppar must be run before a confidence region plot is attempted\n");
    }  
    else if ( grid->getGridSize() == 0)
    {
        // steppar has previously run but the data are obsolete and have been removed.
        // throw.
        throw YellowAlert("The confidence region data is obsolete - re-run steppar\n");
    }  
}

Real ContourPolicy<Step>::getMinVal(const Grid* grid)
{
   // We know this is a Step grid
   return static_cast<const Step*>(grid)->bestFit();
}

Real ContourPolicy<Step>::getHorizLineVal(const Grid* grid)
{
   // We know this is a Step grid
   return (static_cast<const Step*>(grid)->bestFit() + 2.706);
}

void ContourPolicy<Step>::getInitLevels(const Grid* grid, RealArray& relativeLevels)
{
   relativeLevels.resize(3);
   relativeLevels[0] = 2.30;  relativeLevels[1] = 4.61;  relativeLevels[2] = 9.21;
}

void ContourPolicy<Step>::getAbsoluteLevels(const Grid* grid, 
					    const RealArray& relativeLevels,
					    RealArray& absoluteLevels)
{
  // for Step the absolute levels are the relative levels plust the minimum in the grid
   absoluteLevels.resize(relativeLevels.size());
   absoluteLevels = relativeLevels + getMinVal(grid);
}

string ContourPolicy<Step>::getParLabel(size_t idx, const Grid* grid)
{
   const Grid::ParameterSpec* param = grid->getParameter()[idx];
   // ParamSpec name may contain an optional [<modName>:] specifier
   // which we don't want here.
   string paramLabel(param->name);
   string::size_type colonPos = paramLabel.find(':');
   if (colonPos != string::npos)
      paramLabel = paramLabel.substr(colonPos+1);
   if ( param->units.length() != 0)
   {
       paramLabel += " (";
       paramLabel += param->units;
       paramLabel += ")";       
   }  
   return paramLabel;
}

string ContourPolicy<Step>::getYLabel(const PlotSettings& settings)
{
   const StatMethod* stat = XSContainer::fit->statManager()->usingSingleStat();
   const string statName = stat ? stat->scriptName() : string("Combined");
   string yLabel = settings.lookupLabelName("y:contour:statistic");
   yLabel += string(" ") + statName;
   return yLabel;
}

string ContourPolicy<Step>::getTitle(const PlotSettings& settings, size_t nDim)
{
   const StatMethod* stat = XSContainer::fit->statManager()->usingSingleStat();
   const string statName = stat ? stat->fullName() : string("Total Statistic");
   string labelKey = (nDim == 1) ? string("t:contour:1d") : string("t:contour:2d");
   string title = settings.lookupLabelName(labelKey);
   title += string(" ") + statName;
   return title;
}

Grid* ContourPolicy<MarginGrid>::getGrid()
{
   return XSContainer::fit->chainManager()->marginGrid();
}

void ContourPolicy<MarginGrid>::validate(const Grid* grid)
{
    if ( !grid )
    {
        // throw immediately if no margin operation has ever been computed.
        throw YellowAlert("Margin command must be run before a margin plot is attempted.\n");
    }  
}

Real ContourPolicy<MarginGrid>::getMinVal(const Grid* grid)
{
   // actually returns the maximum pdf in the grid since that is what we want
   // the cross-hair to mark
   return grid->getGridValues().max();
}

Real ContourPolicy<MarginGrid>::getHorizLineVal(const Grid* grid)
{
   // No horizontal line plotted in his case
   return 0.0;
}

void ContourPolicy<MarginGrid>::getInitLevels(const Grid* grid, 
					      RealArray& relativeLevels)
{
   relativeLevels.resize(3);
   relativeLevels[0] = 0.5;  relativeLevels[1] = 0.8;  relativeLevels[2] = 0.95;
}

void ContourPolicy<MarginGrid>::getAbsoluteLevels(const Grid* grid, 
						  const RealArray& relativeLevels,
						  RealArray& absoluteLevels)
{
  // for MarginGrid the levels are a fraction of the maximum in the grid
  absoluteLevels.resize(relativeLevels.size());
  absoluteLevels = relativeLevels * grid->getGridValues().max();
}

string ContourPolicy<MarginGrid>::getParLabel(size_t idx, const Grid* grid)
{
   const std::vector<Chain::ParamID>& params =
                static_cast<const MarginGrid*>(grid)->paramIDs();
   const Chain::ParamID& parID = params[idx];
   string label = parID.parName;
   if (parID.units.length() != 0)
   {
       label += " (";
       label += parID.units;
       label += ")";       
   }  
   return label;
}

string ContourPolicy<MarginGrid>::getYLabel(const PlotSettings& settings)
{
   return settings.lookupLabelName("y:margin:probability");
}

string ContourPolicy<MarginGrid>::getTitle(const PlotSettings& settings, size_t nDim)
{
   string labelKey = (nDim == 1) ? string("t:margin:1d") : string("t:margin:2d");
   string title = settings.lookupLabelName(labelKey);
   return title;
}

Grid* ContourPolicy<IntegProbGrid>::getGrid()
{
  return XSContainer::fit->chainManager()->marginGrid()->integProbGrid();
}

void ContourPolicy<IntegProbGrid>::validate(const Grid* grid)
{
    if ( !grid )
    {
        // throw immediately if no margin operation has ever been computed.
        throw YellowAlert("Margin command must be run before a integprob plot is attempted.\n");
    }  
}

Real ContourPolicy<IntegProbGrid>::getMinVal(const Grid* grid)
{
   return grid->getGridValues().min();
}

Real ContourPolicy<IntegProbGrid>::getHorizLineVal(const Grid* grid)
{
   // In the 1-D case plot the horizontal line at 0.9
  return static_cast<Real>(0.9);
}

void ContourPolicy<IntegProbGrid>::getInitLevels(const Grid* grid, 
						 RealArray& relativeLevels)
{
   relativeLevels.resize(3);
   relativeLevels[0] = 0.68;  relativeLevels[1] = 0.95;  relativeLevels[2] = 0.997;
}

void ContourPolicy<IntegProbGrid>::getAbsoluteLevels(const Grid* grid, 
						     const RealArray& relativeLevels,
						     RealArray& absoluteLevels)
{
  // for IntegProbGrid relative and absolute levels are identical
  absoluteLevels.resize(relativeLevels.size());
  absoluteLevels = relativeLevels;
}

string ContourPolicy<IntegProbGrid>::getParLabel(size_t idx, const Grid* grid)
{
   const std::vector<Chain::ParamID>& params =
     static_cast<const MarginGrid*>(XSContainer::fit->chainManager()->marginGrid())->paramIDs();
   const Chain::ParamID& parID = params[idx];
   string label = parID.parName;
   if (parID.units.length() != 0)
   {
       label += " (";
       label += parID.units;
       label += ")";       
   }  
   return label;
}

string ContourPolicy<IntegProbGrid>::getYLabel(const PlotSettings& settings)
{
   return settings.lookupLabelName("y:integprob:probability");
}

string ContourPolicy<IntegProbGrid>::getTitle(const PlotSettings& settings, size_t nDim)
{
   string labelKey = (nDim == 1) ? string("t:integprob:1d") : string("t:integprob:2d");
   string title = settings.lookupLabelName(labelKey);
   return title;
}

const string ContourTraits<Step>::cmdName = string("contour");
const bool ContourTraits<Step>::addCrosshair = true;
const bool ContourTraits<Step>::addHorizontalLine = true;
const string ContourTraits<Step>::imageSpec = "cct 1";

const string ContourTraits<MarginGrid>::cmdName = string("margin");
const bool ContourTraits<MarginGrid>::addCrosshair = true;
const bool ContourTraits<MarginGrid>::addHorizontalLine = false;
const string ContourTraits<MarginGrid>::imageSpec = "cct -1";

const string ContourTraits<IntegProbGrid>::cmdName = string("integprob");
const bool ContourTraits<IntegProbGrid>::addCrosshair = true;
const bool ContourTraits<IntegProbGrid>::addHorizontalLine = true;
const string ContourTraits<IntegProbGrid>::imageSpec = "cct 1";

