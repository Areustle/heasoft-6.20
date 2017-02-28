//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>

#include <XSUtil/Numerics/Numerics.h>
#include <algorithm>
#include <cfloat>
#include <cmath>

// PlotUnfolded
#include <XSPlot/Commands/PlotUnfolded.h>


// Class PlotUnfolded 

PlotUnfolded::PlotUnfolded (const string& name)
  : PlotCommand(name, new CreateBinnedPlotGroups(true,false))
{
   isDataRequired(true);
   isActiveModelRequired(true);
   isDivisibleByArea(false);
   addCompLevel(3);
   isBackgroundApplicable(false);
   isLineIDApplicable(true);

   PlotAttributes styles;
   styles.symbolStyle = PlotCommand::standardDataStyle();
   styles.lineStyle = PlotStyle::NONE;
   setStyleMap(PlotStyle::DATA, styles);
   styles.symbolStyle = PlotStyle::BLANK;
   styles.lineStyle = PlotCommand::standardModelStyle();
   styles.lineStep = true;
   styles.lineWidth = 2;
   setStyleMap(PlotStyle::MODEL, styles);
   styles.lineStep = false;
   styles.lineStyle = PlotStyle::DOTTED;
   styles.lineWidth = 1;
   setStyleMap(PlotStyle::SOURCES, styles);
}


PlotUnfolded::~PlotUnfolded()
{
}


void PlotUnfolded::manipulate (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
{
   const Real NO_VAL = settings.badDataValue();
   for (size_t i=0; i<plotGroups.size(); ++i)
   {
      PlotGroup* pg = plotGroups[i];
      const size_t nPts = pg->n;
      const size_t nMods = pg->model.size();
      std::vector<Real>& X      = pg->xAxis.data;
      std::vector<Real>& dX      = pg->xAxis.errors[0];
      std::vector<Real>& yData  = pg->yData.data;
      std::vector<Real>& yError = pg->yData.errors[0];
      std::vector<Real>& foldedModel  = pg->model[0].data;
      std::vector<Real>& unfoldedModel = pg->auxData[0].data;

      for (size_t j=0; j<nPts; ++j)
      {
         // Apply uf/f ratio to data.
         if (foldedModel[j] != 0.0 && unfoldedModel[j] != NO_VAL)
         {
            Real multiplier = unfoldedModel[j]/foldedModel[j];
            yData[j] *= multiplier;
            yError[j] *= multiplier;
         }
         else
         {
            yData[j]  = NO_VAL;
            yError[j] = NO_VAL;                    
         }
      }
      // Move all unfolded arrays from their workspaces into what up to now
      // was holding folded.  Source arrays (if any) will already have
      // their unfolded values.
      for (size_t j=0; j<nMods; ++j)
      {
         pg->model[j].data = pg->auxData[j].data;
      }

      std::vector<Real> multiplier(nPts, 1.0);
      const Real HZTOERG = Numerics::KEVTOERG/Numerics::KEVTOHZ;

      if (settings.xOption() == ENERGY)
      {
         if (cmdName() == "eufspec")
         {
            if (settings.getUnitID(ENERGY) == "Hz")
            {
               // Want to ultimately display in Jy = 10^-26 J/m^2/s/Hz, need
               // to convert from Hz*photon/cm^2/s/Hz by applying an extra factor
               //   = 10^26*(J/erg)*(erg/hz)*(cm^2/m^2)
               //   = 10^26*10^-7*HZTOERG*10^4.
               const Real conversion = 1.0e23*HZTOERG;                            
               for (size_t k=0; k<nPts; ++k)
                  multiplier[k] = conversion*sqrt((X[k]-dX[k])*(X[k]+dX[k])); 
            }
            else
               // Want <units>(phot/cm^2/s/<units>)
               // X is already in the selected <units> of energy so 
               // no need for conversion.               
               for (size_t k=0; k<nPts; ++k)
                  multiplier[k] = sqrt((X[k]-dX[k])*(X[k]+dX[k]));
         }
         else if (cmdName() == "eeufspec")
         {
            // Want <units>^2(photon/cm^2/s/<units>) unless <units> = Hz,
            // in which case want ergs/cm^2/s.
            if (settings.getUnitID(ENERGY) == "Hz")
            {
               // X is in Hz, yData is in phot/cm^2/s/Hz.
               // This puts multiplier in units of erg-Hz.
               for (size_t k=0; k<nPts; ++k)
                  multiplier[k] = HZTOERG*(X[k]-dX[k])*(X[k]+dX[k]);
            }
            else
            {
               for (size_t k=0; k<nPts; ++k)
                  multiplier[k] = (X[k]-dX[k])*(X[k]+dX[k]);
            }
         }
      } // end if ENERGY mode
      else // WAVELENGTH mode
      {
         // For wavelength case, still want to multiply by E (or E^2), 
         // though X and dX are lengths. 

         const Real unitFactor = settings.getUnitFactor(WAVELENGTH);
         if (settings.isWavePerHz())
         {
            if (cmdName() == "eufspec")
            {
               // yData is in phot/cm^2/s/Hz and we ultimately want
               // Jy = 10^-26 J/m^2/s/Hz (rather than erg/cm^2/s/Hz).
               // So conversion 
               //   = 10^26*hc(keV-A)*(<length>/A)*(J/erg)*(erg/keV)*(cm^2/m^2)
               //   = 10^26*KEVTOA*unitFactor*10^-7*KEVTOERG*10^4. 

               const Real conversion = Numerics::KEVTOA*Numerics::KEVTOERG
                                        *unitFactor*1.0e23;
               for (size_t k=0; k<nPts; ++k)
               {
                  // This is the result of taking
                  // conversion*sqrt((E-dE)*(E+dE))
                  multiplier[k] = conversion/sqrt(X[k]*X[k]-dX[k]*dX[k]);
               }
            }
            else if (cmdName() == "eeufspec")
            {
               // X-axis is in length <units>.
               // yData is in photon/cm^2/s/Hz.
               // Ultimately want ergs/cm^2/s
               const Real conversion = Numerics::KEVTOA*Numerics::KEVTOERG*unitFactor;
               for (size_t k=0; k<nPts; ++k)
               {
                  // This is the result of doing conversion^2*(E-dE)*(E+dE)
                  multiplier[k] = conversion*conversion/(X[k]*X[k]-dX[k]*dX[k]);
                  // Now convert multiplier from ergs^2 to erg-Hz
                  multiplier[k] /= HZTOERG;
               }
            }
         } // end if isWavePerHz
         else
         {
            if (cmdName() == "eufspec")
            {
               // Want E in ergs, so multiply by
               // conversion = hc(in keV-A)*(ergs/keV)*(<units>/Angstrom)
               // = KEVTOA*KEVTOERG*unitFactor.

               const Real conversion = Numerics::KEVTOA*Numerics::KEVTOERG
                                        *unitFactor;
               for (size_t k=0; k<nPts; ++k)
               {
                  // This is the result of taking
                  // conversion*sqrt((E-dE)*(E+dE))
                  multiplier[k] = conversion/sqrt(X[k]*X[k]-dX[k]*dX[k]);
               }
            }
            else if (cmdName() == "eeufspec")
            {
               // X-axis is in <length> units.
               // yData is in photon/cm^2/s/<length>.
               // Ultimately want ergs^2/cm^2/s/<length>
               const Real conversion = Numerics::KEVTOA*Numerics::KEVTOERG
                                        *unitFactor;
               for (size_t k=0; k<nPts; ++k)
               {
                  Real E = conversion*X[k]/(X[k]*X[k]-dX[k]*dX[k]);
                  multiplier[k] = E*E;
               }
            }
         } // end if !isWavePerHz
      } // end if WAVELENGTH mode


      for (size_t k=0; k<nPts; ++k)
      {
         if (multiplier[k] != 1.0)
         {
           const Real mult = multiplier[k];       
           yData[k] *= mult;
           yError[k] *= mult;
           for (size_t j=0; j<nMods; ++j)
           {
              pg->model[j].data[k] *= mult;
           }
           for (size_t j=0; j<pg->sources.size(); ++j)
           {
              PlotVectorList::iterator itSource = pg->sources[j].begin();
              PlotVectorList::iterator itEnd = pg->sources[j].end();
              while (itSource != itEnd)
              {
                 itSource->data[k] *= mult;
                 ++itSource;
              }
           }
         }
      }
   } // end plot groups loop
}

void PlotUnfolded::setRanges (std::vector<PlotGroup*>& plotGroups, const PlotSettings& settings)
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
         Real y = pg->yData.data[k];
         if (y != NO_VAL)
         {
            yMin = (y < yMin) ? y : yMin;
            yMax = (y > yMax) ? y : yMax;
         }
      }
   }
   rangeVals.ranges.y2 = 2.0*yMax;
   rangeVals.ranges.y1 = std::max(yMin, 1.0e-5*rangeVals.ranges.y2);
}

void PlotUnfolded::makeLabels (const PlotSettings& settings, StandardLabels& labels) const
{
   labels.title = settings.lookupLabelName("t:ufspec");
   string xKey(settings.makeXOptionDependentLabel(labels.x));
   string yKey("y:");
   string yLabel;
   if (cmdName() == "ufspec")
   {
      yLabel = settings.lookupLabelName("y:ufspec");
      settings.insertUnitLabel(yLabel, true);
   }
   else // eufspec and eeufspec
   {
      if (cmdName() == "eufspec")
         yKey += "eufspec:";
      else
         yKey += "eeufspec:";

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

bool PlotUnfolded::requestNewXoption (const PlotSettings& settings, XaxisMode& newOption) const
{
   bool doChange = false;
   if (settings.xOption() == CHANNELS)
   {
      newOption = ENERGY;
      doChange = true;
   }
   return doChange;
}

// Additional Declarations
