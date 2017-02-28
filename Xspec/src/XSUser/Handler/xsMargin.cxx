#include <XSstreams.h>
#include <xsTypes.h>
#include <XSContainer.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/MCMC/MarginGrid.h>
#include <XSFit/MCMC/IntegProbGrid.h>
#include <XSFit/MCMC/ChainManager.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>

int
XSGlobal::xsMargin(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doMargin(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doMargin(const StringArray& rawArgs)
{
   // Margin command is very similar to steppar, though somewhat
   // simpler.  It doesn't use current/best setting, nor does
   // it require fits to be performed.
   const char* cmd = "margin";
   const size_t nArgs = rawArgs.size();
   Grid::SpecContainer newSettings;
   static Grid::ParameterSpec* prevSettings = 0;
   ChainManager* chainManager = XSContainer::fit->chainManager();   

   bool usePreviousSettings(nArgs == 1);
   if (usePreviousSettings && !prevSettings)
   {
      tcout << "No parameters yet specified: summary of command:\n";
      printDocs(cmd,"?");
      return 0;          
   }
   static MarginGrid* grid = 0;
   if (!grid)
   {
      grid = new MarginGrid(chainManager);
      chainManager->marginGrid(grid);
      grid->integProbGrid(new IntegProbGrid(grid));
   }

   try
   {
      if (!usePreviousSettings)
      {
         string arg(rawArgs[1]);
         if (arg[0] == '?')
         {
	     printDocs(cmd,"?");
	     return 0;              
         }
         else
         {
            bool isBest = false; // dummy variable in this context
            // The following can allocate new memory into 
            // newSettings array, and prevSettings will have
            // same pointer value as the last entry in array.
            HandlerUtils::commonStepParsing(rawArgs, prevSettings,
                newSettings, isBest, false);
            if (!newSettings.size())
            {
               return -1;
            }
            // commonStepParsing has set prevSettings to most
            // recent entry in newSettings array, which will now
            // be owned by grid.  
            grid->reinitialize(newSettings);
            grid->doGrid();
            // Because grids take ownership of ParameterSpecs,
            //  ipgrid needs deep copies of newSettings.
            Grid::SpecContainer newipSettings(newSettings.size());
            for (size_t i=0; i<newSettings.size(); ++i)
            {
               newipSettings[i] = new Grid::ParameterSpec(*newSettings[i]);
            }
            
	    grid->integProbGrid()->reinitialize(newipSettings);
	    grid->integProbGrid()->doGrid();
         }
      }
      grid->report();
   }
   catch (YellowAlert&)
   {
      // If something went wrong, remove grid.  Otherwise a grid
      // in an undefined state could be used later for "plot margin".
      delete grid;
      grid = 0;
      // prevSettings will point to invalid memory once grid is
      // deleted (see above), so...
      prevSettings = 0;
      chainManager->marginGrid(0);
      
   }

   return 0;
}
