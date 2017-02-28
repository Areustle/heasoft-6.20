//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Error/Error.h>
#include <XSPlot/Plt/PltPkg.h>

// PlotPkg
#include <XSPlot/Plot/PlotPkg.h>
// PlotPkgCreator
#include <XSPlot/Plot/PlotPkgCreator.h>



// Class PlotPkgCreator 

PlotPkg* PlotPkgCreator::MakePlotPkg (const string& plotPackageName)
{
   PlotPkg* plotPackage = 0;
   if (plotPackageName == "plt")
   {
      plotPackage = new PltPkg();
   }
   else
   {
      throw RedAlert("Invalid default graph package in Xspec.init file\n");
   }
   return plotPackage;
}

// Additional Declarations
