//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Numerics/Numerics.h>

// PlotGroup
#include <XSPlot/Plot/PlotGroup.h>
// PlotSettings
#include <XSPlot/Plot/PlotSettings.h>
// PlotGroupCreator
#include <XSPlot/Plot/PlotGroupCreator.h>



// Class PlotGroupCreator 
const Real PlotGroupCreator::s_NODATA = -1.2e-34;

PlotGroupCreator::PlotGroupCreator()
{
}


PlotGroupCreator::~PlotGroupCreator()
{
}


void PlotGroupCreator::setXaxis (const PlotSettings& settings, PlotGroup* group, int firstChannel, int lastChannel, Real eMin, Real eMax, int point)
{

   Real unitFactor = 1.0; 
   switch (settings.xOption())
   {
      default:
      case CHANNELS:
         // lastChannel is 1 beyond the last channel contributing to the bin.
         // firstChannel and lastChannel are 0-based, display is 1-based.
         if ( group->single )
         {
            group->xAxis.errors[0][point] = 0.5*group->channelPlotSpacing;      
            group->xAxis.data[point] = group->xAxis.errors[0][point]  - 0.5;
         }
         else
         {
            group->xAxis.data[point] =  0.5*(lastChannel - 1 + firstChannel) + 1; 
            group->xAxis.errors[0][point] =  0.5*(lastChannel - firstChannel);                       
         }
         break;
      case ENERGY:
         unitFactor = settings.getUnitFactor(ENERGY);
         group->xAxis.data[point] = 0.5*(eMax + eMin)*unitFactor;
         group->xAxis.errors[0][point] = 0.5*(eMax - eMin)*unitFactor;
         break;
      case WAVELENGTH:
         unitFactor = settings.getUnitFactor(WAVELENGTH);
         if ( eMax <= 0 || eMin <= 0)
         {
            group->xAxis.data[point] = s_NODATA;
            group->xAxis.errors[0][point] = s_NODATA;       
         }
         else
         {
            group->xAxis.data[point]      = 0.5*Numerics::KEVTOA*(1./eMax + 1./eMin)*unitFactor;
            group->xAxis.errors[0][point] =  0.5*Numerics::KEVTOA*(1./eMin - 1./eMax)*unitFactor;                                     
         }
         break;  
   }  
}

void PlotGroupCreator::determineXRange (const std::vector<PlotGroup*>& plotGroups, Real* pXlow, Real* pXhigh)
{
   Real xlow = 1.e99;
   Real xhigh = -1.e99;

   bool rangeFound = false;
   for (size_t iGroup=0; iGroup<plotGroups.size(); ++iGroup)
   {
      const PlotGroup* gr = plotGroups[iGroup];
      const int npts = gr->n;
      // npts can be zero if for example all channels are ignored.
      if (npts)
      {
         const Real xAxis_0 = gr->xAxis.data[0];
         const Real xAxis_N = gr->xAxis.data[npts-1];
         Real err_0 = 0.0;
         Real err_N = 0.0;
         // Don't assume all PlotGroups have kept their X-axis errors vector.
         // Some commands may have decided to erase it if it's not needed.
         // However DO assume that if it's there, it will be the proper size.
         if (gr->xAxis.errors.size())
         {
            err_0 = gr->xAxis.errors[0][0];
            err_N = gr->xAxis.errors[0][npts-1];
         }
         if (xAxis_N > xAxis_0)
         {
            xlow = std::min(xAxis_0 - err_0,xlow);
            xhigh = std::max(xAxis_N + err_N,xhigh);
            rangeFound = true;
         }
         else
         {
            xlow = std::min(xAxis_N - err_N,xlow);                                
            xhigh = std::max(xAxis_0 + err_0,xhigh);
            rangeFound = true;
         }
      }
   }
   if (!rangeFound)
   {
      xlow = xhigh = 0;
   }
   *pXlow = xlow;
   *pXhigh = xhigh;
}

void PlotGroupCreator::determineYRange (const std::vector<PlotGroup*>& plotGroups, Real* pYlow, Real* pYhigh)
{
   Real ylow = 1.e99;
   Real yhigh = -1.e99;

   bool rangeFound = false;
   for (size_t iGroup=0; iGroup<plotGroups.size(); ++iGroup)
   {
      Real low, high;
      plotGroups[iGroup]->getLimits(low, high);
      if (!(low == high && low == 0.0)) {
	ylow = std::min(low, ylow);
	yhigh = std::max(high, yhigh);
	rangeFound = true;
      }
   }
   if (!rangeFound)
   {
      ylow = yhigh = 0;
   }
   *pYlow = ylow;
   *pYhigh = yhigh;
}

// Additional Declarations
