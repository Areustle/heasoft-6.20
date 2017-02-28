//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/PlotGroupCreatorClasses.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

#include <XSUtil/Numerics/Numerics.h>
#include <cfloat>
#include <queue>

// PlotModel
#include <XSPlot/Commands/PlotModel.h>


// Class PlotModel 

PlotModel::PlotModel (const string& name)
  : PlotCommand(name, new CreateModelPlotGroups())
{
   doesTakeParameters(true);
   isDataRequired(false);
   isActiveModelRequired(false);
   isDivisibleByArea(false);
   addCompLevel(3);
   isBackgroundApplicable(false);
   isLineIDApplicable(true);

   PlotAttributes styles;
   styles.symbolStyle = PlotStyle::BLANK;
   styles.lineStyle = PlotCommand::standardModelStyle();
   styles.lineStep = false;
   styles.lineWidth = 2;
   setStyleMap(PlotStyle::MODEL, styles);
   styles.lineStep = false;
   styles.lineStyle = PlotStyle::DOTTED;
   styles.lineWidth = 1;
   setStyleMap(PlotStyle::SOURCES, styles);
}


PlotModel::~PlotModel()
{
}


void PlotModel::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      PlotGroup* pg = plotGroups[i];
      const size_t nPts = pg->n;
      std::vector<Real>& X = pg->xAxis.data;
      std::vector<Real>& dX = pg->xAxis.errors[0];
      std::vector<Real> multiplier(nPts);
      const Real HZTOERG = Numerics::KEVTOERG/Numerics::KEVTOHZ;

      // Model vector is in photons/cm^2/s
      if (settings.xOption() == ENERGY)
      {
         if (cmdName() == "model")
         {
            // Want to display photons/cm^2/s/<units>
            for (size_t k=0; k<nPts; ++k)
	       multiplier[k] = 1.0/(2*dX[k]);
         }
         else if (cmdName() == "emodel")
         {
            if (settings.getUnitID(ENERGY) == "Hz")
            {
               // Want to ultimately display in Jy = 10^-26 J/m^2/s/Hz, need
               // to convert from Hz*photon/cm^2/s/Hz by applying an extra factor
               //   = 10^26*(J/erg)*(erg/hz)*(cm^2/m^2)
               //   = 10^26*10^-7*HZTOERG*10^4.
               const Real conversion = 1.0e23*HZTOERG;                            
               for (size_t k=0; k<nPts; ++k)
                  multiplier[k] = conversion*sqrt((X[k]-dX[k])*(X[k]+dX[k]))/(2.0*dX[k]); 
            }
            else
               // Want <units>(phot/cm^2/s/<units>)
               // X is already in the selected <units> of energy so 
               // no need for conversion.               
               for (size_t k=0; k<nPts; ++k)
                  multiplier[k] = sqrt((X[k]-dX[k])*(X[k]+dX[k]))/(2.0*dX[k]);
         }
         else // eemodel
         {
            if (settings.getUnitID(ENERGY) == "Hz")
            {
               // Want ergs/cm^2/s
               // X is in Hz.
               const Real HZTOERG = Numerics::KEVTOERG/Numerics::KEVTOHZ;
               for (size_t k=0; k<nPts; ++k)
                  multiplier[k] = HZTOERG*(X[k]-dX[k])*(X[k]+dX[k])/(2.0*dX[k]);
            }
            else
            {
               // Want <units>^2(phot/cm^2/s/<units>)
               // X is in <units> (keV,MeV,GeV).
               for (size_t k=0; k<nPts; ++k)
                   multiplier[k] = (X[k]-dX[k])*(X[k]+dX[k])/(2.0*dX[k]);
            }
         }
      }  // end if ENERGY mode
      else
      {
         // WAVELENGTH mode
         const Real unitFactor = settings.getUnitFactor(WAVELENGTH);
         if (settings.isWavePerHz())
         {
            if (cmdName() == "model")
            {
               // Want to display photons/cm^2/s/<units>
               // X units are in lengths, must convert to Hz.
               Real conversion = Numerics::KEVTOHZ*Numerics::KEVTOA*unitFactor;
               for (size_t k=0; k<nPts; ++k)
               {
                  Real dHz = conversion*dX[k]/(X[k]*X[k]-dX[k]*dX[k]);
	          multiplier[k] = 1.0/(2*dHz);
               }                
            }
            else if (cmdName() == "emodel")
            {
               // X is in lengths and we want a multiplier of
               // sqrt((E-dE)*(E+dE))/(2dHz) =  
               // sqrt(X^2-dX^2)/(2dX*KEVTOHZ) (in keV/Hz units)

               // But to ultimately display in Jy = 10^-26 J/m^2/s/Hz, need
               // to convert from keV/cm^2/s/Hz by applying an extra factor
               //   = 10^26*(J/keV)*(cm^2/m^2)
               //   = 10^26*10^-7*KEVTOERG*10^4.             

               const Real conversion = 1.e23*Numerics::KEVTOERG;
               for (size_t k=0; k<nPts; ++k)
               {
                  multiplier[k] = conversion*sqrt(X[k]*X[k]-dX[k]*dX[k])/
                                        (2.0*dX[k]*Numerics::KEVTOHZ);
               }
            }
            else // eemodel
            {
               // Want ergs/cm^2/s
               // This relatively simple multiplier is the result of doing: 
               // KEVTOERG*KEVTOHZ*(E-dE)*(E+dE)/(2*dHz)
               const Real conversion = Numerics::KEVTOERG*Numerics::KEVTOA*unitFactor/2.0;
               for (size_t k=0; k<nPts; ++k)
                  multiplier[k] = conversion/dX[k];
            }
         } // end if perHz
         else // per <length> units
         {
            if (cmdName() == "model")
            {
               // Want to display photons/cm^2/s/<units>
              for (size_t k=0; k<nPts; ++k)
	         multiplier[k] = 1.0/(2*dX[k]);
            }
            else if (cmdName() == "emodel")
            {
               // Want E in ergs.  So multiply by conversion
               // = hc(in keV-A)*(ergs/keV)*(<units>/Angstrom)
               // = KEVTOA*KEVTOERG*unitFactor.
               Real conversion = Numerics::KEVTOA*Numerics::KEVTOERG*unitFactor;
               for (size_t k=0; k<nPts; ++k)
               {
                  // This is the result of doing:
                  // sqrt((E-dE)*(E+dE))/(2.0*dX[k]);
                  multiplier[k] = conversion*sqrt(1.0/(X[k]*X[k]-dX[k]*dX[k]))
                                  /(2.0*dX[k]);
               }
            }
            else // eemodel
            {
               // Want ergs^2/cm^2/s/<length>
               const Real conversion = Numerics::KEVTOA*Numerics::KEVTOERG*unitFactor;
               for (size_t k=0; k<nPts; ++k)
               {
                  Real E = conversion*X[k]/(X[k]*X[k]-dX[k]*dX[k]);
                  multiplier[k] = E*E/(2*dX[k]);
               }
            }
         } // end if per length
      } // end if wavelength mode


      // Now apply multiplier to model and source components.
      // Only 1 model vector is ever filled for plot model.
      std::vector<Real>& model  = pg->model[0].data;
      std::vector<Real>& modelErr = pg->model[0].errors[0];
      const bool isError = (modelErr.size() == model.size());
      for (size_t k=0; k<nPts; ++k)
      {
         model[k] *= multiplier[k];
         if (isError)
            modelErr[k] *= multiplier[k];
      }

      PlotVectorList::iterator itSource = pg->sources[0].begin();
      PlotVectorList::iterator itEnd = pg->sources[0].end();
      while (itSource != itEnd)
      {
         PlotVector& s = *itSource;
         for (size_t k=0; k<nPts; ++k) 
            s.data[k] *= multiplier[k];
         ++itSource;       
      }

   }
}

void PlotModel::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   const Real NO_VAL = settings.badDataValue();
   Real xlow=0.0, xhigh=0.0;
   PlotGroupCreator::determineXRange(plotGroups,&xlow,&xhigh);

   PlotRange& rangeVals = rangeSettings();
   // Always plot log scale for x and y axis for model related plots.
   rangeVals.xScaleType = PlotStyle::LOG;
   rangeVals.yScaleType = PlotStyle::LOG;
   rangeVals.useRangeFlags = PlotStyle::XMIN + PlotStyle::XMAX +
                              PlotStyle::YMIN + PlotStyle::YMAX;
   rangeVals.ranges.x1 = xlow;
   rangeVals.ranges.x2 = xhigh;

   Real yMin = DBL_MAX;
   Real yMax = -DBL_MAX;
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      const PlotGroup* pg = plotGroups[i];
      const size_t nPts = pg->n;
      for (size_t k=0; k<nPts; ++k)
      {
         Real y = pg->model[0].data[k];
         if (y != NO_VAL)
         {
            yMin = (y < yMin) ? y : yMin;
            yMax = (y > yMax) ? y : yMax;
         }
      }
   }

   // Default range will only show at most 3 orders of magnitude.  These
   // adjustments assume ymin,ymax are positive, but then we're already 
   // making that assumption by forcing ylog on.
   rangeVals.ranges.y2 = yMax;
   rangeVals.ranges.y1 = std::max(yMin, 1.0e-3*rangeVals.ranges.y2);
   // Add an epsilon to range to deal with case where ymax = ymin.
   const Real epsRange = .01;
   rangeVals.ranges.y1 *= 1.0 - epsRange;
   rangeVals.ranges.y2 *= 1.0 + epsRange;
}

void PlotModel::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   labels.title = settings.lookupLabelName("t:model");
   string xKey(settings.makeXOptionDependentLabel(labels.x));
   string yKey("y:");
   string yLabel;
   if (cmdName() == "model")
   {
      yLabel = settings.lookupLabelName("y:model");
      settings.insertUnitLabel(yLabel, true);
   }
   else // emodel and eemodel
   {
      if (cmdName() == "emodel")
         yKey += "emodel:";
      else
         yKey += "eemodel:";

      // For ENERGY using Hz units, use the same y-label format
      // as for WAVELENGTH in perHz mode.
      bool usingHz = false;
      if (xKey == "x:energy" && settings.getUnitID(ENERGY) != "Hz")
         yKey += "energy";
      else if (xKey == "x:wave" && !settings.isWavePerHz())
         yKey += "wave";
      else
      {
         yKey += "wave:perHz";
         usingHz = true;
      }

      yLabel = settings.lookupLabelName(yKey);
      if (!usingHz)
         settings.insertUnitLabel(yLabel, false);
   }
   labels.y = yLabel;
}

bool PlotModel::requestNewXoption (const PlotSettings& settings, XaxisMode& newOption) const
{
   bool doChange = false;
   if (settings.xOption() == CHANNELS)
   {
      newOption = ENERGY;
      doChange = true;
   }
   return doChange;
}

void PlotModel::processAdditionalParams (const StringArray& args, const IntegerArray& argIndices)
{
   // Only one additional arg is relevant, and that should be a model name.
   CreateModelPlotGroups* groupCreator =
             dynamic_cast<CreateModelPlotGroups*>(plotGroupStrategy());
   std::queue<string>& modelNames = groupCreator->modNames();
   if (args.size())
   {
      modelNames.push(args[0]);
   }
   else
      modelNames.push(string(""));
}

void PlotModel::cleanup()
{
   // This shouldn't be necessary unless we're in here due to an exception.
   // In normal plot execution, the groupCreator will have popped all
   // names out of the queue.
   CreateModelPlotGroups* groupCreator =
             dynamic_cast<CreateModelPlotGroups*>(plotGroupStrategy());
   std::queue<string>& modelNames = groupCreator->modNames();
   while (modelNames.size())
      modelNames.pop();
}
