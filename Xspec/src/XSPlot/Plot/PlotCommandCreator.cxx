//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Parse/XSparse.h>
#include <XSPlot/Commands/ContourPlot.h>
#include <XSPlot/Commands/PlotBackground.h>
#include <XSPlot/Commands/PlotChiSq.h>
#include <XSPlot/Commands/PlotData.h>
#include <XSPlot/Commands/PlotDelchi.h>
#include <XSPlot/Commands/PlotDem.h>
#include <XSPlot/Commands/PlotEfficiency.h>
#include <XSPlot/Commands/PlotEqw.h>
#include <XSPlot/Commands/PlotFoldmodel.h>
#include <XSPlot/Commands/PlotGoodness.h>
#include <XSPlot/Commands/PlotIcounts.h>
#include <XSPlot/Commands/PlotInsensitivity.h>
#include <XSPlot/Commands/PlotMcmc.h>
#include <XSPlot/Commands/PlotModel.h>
#include <XSPlot/Commands/PlotRatio.h>
#include <XSPlot/Commands/PlotResiduals.h>
#include <XSPlot/Commands/PlotSensitivity.h>
#include <XSPlot/Commands/PlotSum.h>
#include <XSPlot/Commands/PlotUnfolded.h>

// PlotCommand
#include <XSPlot/Plot/PlotCommand.h>
// PlotCommandCreator
#include <XSPlot/Plot/PlotCommandCreator.h>

#include <sstream>
#include <iomanip>
#include <iterator>


// Class PlotCommandCreator 
PlotCommandContainer PlotCommandCreator::s_commands;

PlotCommandCreator::~PlotCommandCreator()
{
}


string PlotCommandCreator::registerPlotCommands ()
{
  // Initialize the command map
  using namespace std;
  ostringstream commands;
  commands.setf(ios_base::left,ios_base::adjustfield);
  size_t fieldWidth(15);

  s_commands["background"] = new PlotBackground();
  commands << setw(fieldWidth) << "background";

  s_commands["chain"] = new PlotMcmc();
  commands << setw(fieldWidth) << "chain";

  s_commands["chisq"] = new PlotChiSq();
  commands << setw(fieldWidth) << "chisq";

  s_commands["contour"] = new ContourPlot<Step>();
  commands << setw(fieldWidth) << "contour";

  s_commands["counts"] = new PlotData("counts",true);
  commands << setw(fieldWidth) << "counts";

  s_commands["integprob"] = new ContourPlot<IntegProbGrid>();
  commands << setw(fieldWidth) << "integprob";

  s_commands["data"] = new PlotData("data",false);
  commands << setw(fieldWidth) << "data";

  s_commands["delchi"] = new PlotDelchi();
  commands << setw(fieldWidth) << "delchi";

  s_commands["dem"] = new PlotDem();
  commands << setw(fieldWidth) << "dem";

  s_commands["emodel"] = new PlotModel("emodel");
  commands << setw(fieldWidth) << "emodel";

  s_commands["eemodel"] = new PlotModel("eemodel");
  commands << setw(fieldWidth) << "eemodel";

  s_commands["efficiency"] = new PlotEfficiency();
  commands << setw(fieldWidth) << "efficiency";

  s_commands["eqw"] = new PlotEqw();
  commands << setw(fieldWidth) << "eqw";

  s_commands["eufspec"] = new PlotUnfolded("eufspec");
  commands << setw(fieldWidth) << "eufspec";

  s_commands["eeufspec"] = new PlotUnfolded("eeufspec");
  commands << setw(fieldWidth) << "eeufspec";

  s_commands["foldmodel"] = new PlotFoldmodel();
  commands << setw(fieldWidth) << "foldmodel";

  s_commands["goodness"] = new PlotGoodness();
  commands << setw(fieldWidth) << "goodness";

  s_commands["icounts"] = new PlotIcounts();
  commands << setw(fieldWidth) << "icounts";

  s_commands["insensitivity"] = new PlotInsensitivity();
  commands << setw(fieldWidth) << "insensitivity";

  s_commands["lcounts"] = new PlotData("lcounts",true);
  commands << setw(fieldWidth) << "lcounts";

  s_commands["ldata"] = new PlotData("ldata",false);
  commands << setw(fieldWidth) << "ldata";

  s_commands["margin"] = new ContourPlot<MarginGrid>();
  commands << setw(fieldWidth) << "margin";

  s_commands["model"] = new PlotModel("model");
  commands << setw(fieldWidth) << "model";

  s_commands["ratio"] = new PlotRatio();
  commands << setw(fieldWidth) << "ratio";

  s_commands["residuals"] = new PlotResiduals();
  commands << setw(fieldWidth) << "residuals";

  s_commands["sensitivity"] = new PlotSensitivity();
  commands << setw(fieldWidth) << "sensitivity";

  s_commands["sum"] = new PlotSum();
  commands << setw(fieldWidth) << "sum";

  s_commands["ufspec"] = new PlotUnfolded("ufspec");
  commands << setw(fieldWidth) << "ufspec";

  return commands.str();
}

PlotCommand* PlotCommandCreator::commands (const std::string& name)
{
   PlotCommand* requested(0);
   PC_Iter  iCl (s_commands.lower_bound(name));
   if ( iCl != s_commands.end())  
   {
      if (iCl->first.find(name) == 0)
      {   
	 requested = iCl->second;
      }
   }
   return requested;
}

void PlotCommandCreator::destroy ()
{
  PlotCommandContainer::iterator iC = s_commands.begin();
  PlotCommandContainer::iterator iCEnd = s_commands.end();
  while (iC != iCEnd)
  {
     if (iC->second)
     {
        delete iC->second;
     }
     ++iC;
  }
  s_commands.clear();
}

StringArray PlotCommandCreator::commandNames ()
{
  PlotCommandContainer::iterator iC = s_commands.begin();
  PlotCommandContainer::iterator iCEnd = s_commands.end();
  StringArray listOfNames(s_commands.size(),"");
  size_t j(0);
  while (iC != iCEnd)
  {
     listOfNames[j] = iC->first;
     ++iC;
  }

  return listOfNames;
}

// Additional Declarations
