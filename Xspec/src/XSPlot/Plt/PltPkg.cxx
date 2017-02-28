//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSstreams.h>
#include <XSPlot/Plt/PltCtoF.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotPane.h>
#include <XSPlot/Plot/PlotStyle.h>
#include <XSPlot/Plot/PlotTypes.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Utils/XSutility.h>
#include <algorithm>
#include <cmath>
#include <cstring>
#include <sstream>

// PltPkg
#include <XSPlot/Plt/PltPkg.h>


// Class PltPkg 
const float PltPkg::s_NO = -1.2e-34;
const size_t PltPkg::s_MAXNLABELS = 60;
size_t PltPkg::s_lineIDCount = 0;

PltPkg::PltPkg()
  :PlotPkg(),
   m_alignedPlotVectors(),
   m_vecsInCategory(),
   m_errorVecsInCategory(),
   m_commands(),
   m_nVec(0)
{
}


PltPkg::~PltPkg()
{
  if (deviceName().size())
  {
     PGCLOS();
     PGEND();
  }
}


void PltPkg::setDevice (const string& device, bool splashOn)
{
  // "device" is misleading.  It is actually <path>/<filename>/device.
  string lcInput(XSutility::lowerCase(device));

  if (deviceName().size())
  {
     PGCLOS();
     deviceName("");
  }
  if (lcInput != "none")
  {
     bool isPostscript = false;
     string fullDevString = device;
     static const string defaultPSDevice ("ps");
     static  std::vector<string> psDevices;
     if (psDevices.empty())
     {
        psDevices.push_back("ps");       
        psDevices.push_back("vps");       
        psDevices.push_back("cps");       
        psDevices.push_back("vcps");       
     }
     string::size_type lastSlash = fullDevString.find_last_of("/");
     if (lastSlash == string::npos)
     {
        // No slashes, assume this is simply a file name and
        // we'll assing the default device /ps to it.
        isPostscript = true;
        fullDevString += "/" + defaultPSDevice;
     }
     else if (lastSlash == fullDevString.length()-1)
     {
        // It input ends with a '/', don't know how to interpret.
        string msg = "Unable to set PLT device to " + fullDevString + '\n';
        throw YellowAlert(msg);
     }
     else
     {
        string afterSlash = fullDevString.substr(lastSlash+1);
        if (afterSlash.find(".") != string::npos)
        {
           // Assume input is simply a file name with path.
           isPostscript = true;
           fullDevString += "/" + defaultPSDevice;
        }
        else
        {
           string lcAfterSlash = XSutility::lowerCase(afterSlash);
           if (std::find(psDevices.begin(), psDevices.end(), lcAfterSlash)
                        != psDevices.end())
           {
              isPostscript = true;
           }
        }
     }

     if (PGOPEN(fullDevString.c_str()) != 1)
     {
        string msg = "Unable to set PLT device to " + device + '\n';
        throw YellowAlert(msg);
     }
     deviceName(fullDevString);
     if (!isPostscript)
     {
	string tmpTitle, tmpBuildDate;
	const size_t maxSz(150);
	XSutility::XSVersionString(tmpTitle, tmpBuildDate);
	tmpTitle = "\\fr " + tmpTitle;
	PGENV(0,1,0,1, 0, -2);
        if (splashOn)
        {
	   PGSCH(2.5);
	   char xsTitle[maxSz], buildDate[maxSz];
	   if (tmpTitle.size() >= maxSz || tmpBuildDate.size() >= maxSz)
	   {
              throw RedAlert("Version/Build Date string size exceeded.");
	   }
	   strcpy(xsTitle, tmpTitle.c_str());
	   strcpy(buildDate, tmpBuildDate.c_str());
	   PGTEXT(.1, .9, xsTitle);
	   PGSCH(1.5);
	   PGTEXT(.15, .8, buildDate);
        }
     } 
  }
}

void PltPkg::alignPlotVectors ()
{
   // PLT imposes the constraint that every Xspec PlotGroup must
   // contribute the same number of plot vectors (the NVEC parameter
   // sent to PLT).  Therefore dummy plot vectors may need to be 
   // inserted.  

   // Also each of these NVEC vectors carries 0 or more error arrays 
   // (the IERY[NVEC] array parameter), which must be the same for each 
   // NO-delineated (PLT) plot group within the vector column.  However
   // this part will be handled when constructing the final C-style
   // array that gets sent to PLT.

   // For clarity's sake, also require that for a particular vector 
   // CATEGORY, the number of vectors match up between the Xspec 
   // PlotGroups.  This is a stricter rule than just requiring that
   // the TOTAL number of vectors match up between the PlotGroups, and   
   // may result in adding more dummy vectors than are sometimes 
   // necessary, but this is plenty confusing enough without also
   // tangling the categories.

   determineMaxInCategory();

   m_alignedPlotVectors.clear();
   for (size_t iPane=0; iPane<plotPanes().size(); ++iPane)
   {
      const PlotPane* pane = plotPanes()[iPane];
      for (size_t iGroup=0; iGroup<pane->plotGroups().size(); ++iGroup)
      {
         const PlotGroup* pg = pane->plotGroups()[iGroup];
         m_alignedPlotVectors.push_back(std::vector<PlotVector*>());

         std::vector<PlotVector*>& alignedVecs = m_alignedPlotVectors.back();
         const std::vector<PlotVector*>& bundledVecs = pg->bundledPlotVectors;
         const IntegerArray& vectorBoundaries = pg->plotVectorBoundaries;
         // All groups have a single xAxis vector
         alignedVecs.push_back(bundledVecs[0]);

         // Calculate the needed number of dummy vects for each category.
         const size_t nCats = vectorCategories().size();
         std::vector<size_t> dummiesNeeded(nCats,0);
         size_t nPrevBoundary = 1;        
         for (size_t iCat=0; iCat<nCats; ++iCat)
         {
            const size_t nVecs = vectorBoundaries[iCat] - nPrevBoundary;
            dummiesNeeded[iCat] = m_vecsInCategory[iCat] - nVecs;
            nPrevBoundary = vectorBoundaries[iCat];
         }

         // Now insert pointers to the PlotVectors, inserting dummy nulls
         // if necessary.
         size_t iVec=1;
         for (size_t iCat=0; iCat<nCats; ++iCat)
         {
            while (iVec < static_cast<size_t>(vectorBoundaries[iCat]))
            {
               alignedVecs.push_back(bundledVecs[iVec]);
               ++iVec;
            }
            for (size_t j=0; j<dummiesNeeded[iCat]; ++j)
               alignedVecs.push_back(static_cast<PlotVector*>(0));
         }

         // Assert that we did this right:
         if (alignedVecs.size() != m_nVec)
            throw RedAlert("Aligned vector size mismatch in PltPkg");

      } // end loop over PlotGroups
   } // end loop overPlotPanes
}

void PltPkg::determineMaxInCategory ()
{
   // First, determine the maximum number of vectors in each
   // category, examining every PlotGroup from every PlotPane.
   // Also need the maximum number of error vectors, but can
   // ASSUME vectors from MODEL and SOURCES categories have NO ERROR
   // ARRAYS.

   using namespace PlotStyle;
   const std::vector<VectorCategory>& categories = vectorCategories();
   const size_t nCats = categories.size();
   // Note that m_errorVecsInCategory is 1 larger than nCats.  This
   // is because it needs to store number of xAxis errors, which aren't
   // necessarily the same in every PlotGroup.  However we CAN assume
   // there is one and only one xAxis vector.
   m_vecsInCategory.clear();
   m_vecsInCategory.resize(nCats,0);
   m_errorVecsInCategory.clear();
   m_errorVecsInCategory.resize(nCats+1,0);

   for (size_t iPane=0; iPane<plotPanes().size(); ++iPane)
   {
      const PlotPane* pane = plotPanes()[iPane];
      for (size_t iGroup=0; iGroup<pane->plotGroups().size(); ++iGroup)
      {
         const PlotGroup* pg = pane->plotGroups()[iGroup];
         m_errorVecsInCategory[0] = 
                std::max(m_errorVecsInCategory[0], pg->xAxis.errors.size());
         // Start at 1 due to xAxis.
         size_t nPrevCat=1;
         for (size_t iCat=0; iCat<nCats; ++iCat)
         {
            // plotVectorBoundaries stores a CUMULATIVE total.
            const size_t nInCat = 
                     static_cast<size_t>(pg->plotVectorBoundaries[iCat])-nPrevCat;
            m_vecsInCategory[iCat] = std::max(nInCat,m_vecsInCategory[iCat]);
            size_t nErrs = 0;
            if (nInCat)
            {
               if (categories[iCat] == DATA)
               {
                  nErrs = pg->yData.errors.size();
                  m_errorVecsInCategory[iCat+1] =
                        std::max(nErrs,m_errorVecsInCategory[iCat+1]);
               }
               else if (categories[iCat] == BACKGROUND)
               {
                  nErrs = pg->background.errors.size();
                  m_errorVecsInCategory[iCat+1] =
                        std::max(nErrs,m_errorVecsInCategory[iCat+1]);
               }
            }
            nPrevCat = pg->plotVectorBoundaries[iCat];
         }
      } // end PlotGroups loop
   } // end panes loop

   m_nVec = 0;
   for (size_t i=0; i<nCats; ++i)
      m_nVec += m_vecsInCategory[i];
   // Now add xAxis vector
   m_nVec += 1;
}

void PltPkg::doInitialize ()
{
   m_commands.clear();
   m_commands.push_back("skip on");
   m_commands.push_back("screen white");
   alignPlotVectors();
   s_lineIDCount = 0;
}

void PltPkg::setPanePosition (const size_t iPane)
{
   // Not reusing stringstreams while still maintaining for Solaris.
   // I don't know if earlier (2.6) stringstream bug is still around,
   // but it can be a mess to detect and is not worth tempting fate.
   std::ostringstream oss;
   oss << "window " << iPane+1;
   m_commands.push_back(oss.str());

   const PlotRectangle& position = plotPanes()[iPane]->panePosition();
   std::ostringstream oss2;
   oss2 << "viewport " << position.x1 << " " 
                       << position.y1 << " "
                       << position.x2 << " " 
                       << position.y2;
   m_commands.push_back(oss2.str());
}

void PltPkg::setPaneRanges (const size_t iPane)
{
   using namespace std;
   const PlotRange& range = plotPanes()[iPane]->ranges();
   string typeStr("log x ");
   typeStr += (range.xScaleType == PlotStyle::LOG) ? 
                string("on") : string("off");
   m_commands.push_back(typeStr);
   typeStr = "log y ";
   typeStr += (range.yScaleType == PlotStyle::LOG) ? 
                string("on") : string("off");
   m_commands.push_back(typeStr);

   string rangeStr("rescale x ");
   if (range.useRangeFlags & PlotStyle::XMIN)
   {
      ostringstream xRanges;
      xRanges << range.ranges.x1;
      if (range.useRangeFlags & PlotStyle::XMAX)
      {
        xRanges << " " << range.ranges.x2;
      }
      rangeStr += xRanges.str();
   }
   m_commands.push_back(rangeStr);

   rangeStr = "rescale y ";
   if (range.useRangeFlags & PlotStyle::YMIN)
   {
      ostringstream yRanges;
      yRanges << range.ranges.y1;
      if (range.useRangeFlags & PlotStyle::YMAX)
      {
         yRanges << " " << range.ranges.y2;
      }
      rangeStr += yRanges.str();
   }
   m_commands.push_back(rangeStr);

   // add either a horizontal or vertical line if required

   if (range.horizontalLine.first)
   {
      ostringstream lineSS;
      lineSS << "label " << iPane+1 << " pos "
             << range.ranges.x1 << " " 
             << range.horizontalLine.second 
             << " line 0 100. color 3 lst 1 \" \"";
      m_commands.push_back(lineSS.str());
   }
   else if (range.verticalLine.first)
   {
      ostringstream lineSS;
      lineSS << "label " << iPane+1 << " pos "
             << range.verticalLine.second << " " 
             << range.ranges.y1 
             << " line 90. 100. color 3 lst 2 \" \"";
      m_commands.push_back(lineSS.str());
   }

}

void PltPkg::setPaneStyles (const size_t iPane)
{
   // Default line width - this determines the width of the graph frame.
   m_commands.push_back("lwidth 2");

   string yPlotOn("yplot on");
   const bool isContour = plotPanes()[iPane]->isContour();
   const std::vector<PlotGroup*>& plotGroups = 
                plotPanes()[iPane]->plotGroups();
   for (size_t iGr=0; iGr<plotGroups.size(); ++iGr)
   {
      // Note: Vector 1 containing x data (i=0) is ignored.  PLT shifts  
      // vector numbers down by 1 when skip = ON. 
      const std::vector<PlotVector*>& plotVecs = 
                m_alignedPlotVectors[plotGroups[iGr]->objectIndex-1];
      for (size_t iVec=1; iVec<plotVecs.size(); ++iVec)
      {
         const PlotVector* plotVec = plotVecs[iVec];
         if (plotVec)
         {
            const size_t iPltGrp = (plotGroups[iGr]->objectIndex-1)*(m_nVec-1) + iVec;
            // For convenience lets also save iPltGrp in string form.
            std::ostringstream pltGrpSS;
            pltGrpSS << iPltGrp;
            string iPltGrpStr(pltGrpSS.str());

            yPlotOn += " ";
            yPlotOn += iPltGrpStr;
            const PlotAttributes& attributes = plotVec->styles;
            if (attributes.symbolStyle != PlotStyle::BLANK)
            {
               std::ostringstream symbolSS;
               symbolSS << "marker " << attributes.symbolStyle << " on " << iPltGrp;
               m_commands.push_back(symbolSS.str());
            }
            if (attributes.lineStyle != PlotStyle::NONE)
            {
               string cmd("line on ");
               cmd += iPltGrpStr;
               m_commands.push_back(cmd);
               if (attributes.lineStep)
               {
                  cmd = "line step " + iPltGrpStr;
                  m_commands.push_back(cmd);
               }
               else
               {
                  cmd = "err off " + iPltGrpStr;
                  m_commands.push_back(cmd);
               }
               std::ostringstream styleSS;
               styleSS << "lstyle " << attributes.lineStyle << " on " << iPltGrp;
               m_commands.push_back(styleSS.str());
            }

            std::ostringstream widthSS;
            widthSS << "lwidth " << attributes.lineWidth << " on " << iPltGrp;
            m_commands.push_back(widthSS.str());
            if (!isContour)
            {
               std::ostringstream colorSS;
               colorSS << "color " << attributes.color << " on " << iPltGrp;
               m_commands.push_back(colorSS.str());
            }
         } // end if plotVec != 0 (is not a dummy)
      } // end plot vecs loop
   } // end plot groups loop
   if (!isContour)
      m_commands.push_back(yPlotOn);
}

void PltPkg::setPaneLabels (const size_t iPane)
{
   const PlotPane* pane = plotPanes()[iPane];
   const size_t iStack = pane->stackIndex();
   size_t previousPanes=0;
   for (size_t i=0; i<iStack; ++i)
   {
      previousPanes += panesInStack()[i];
   } 

   // If this is NOT the only pane in its stack, y-axis numerals
   // must be rotated 90 degrees and character sizes must be reduced.
   // But we must only issue the rotate command once for all the 
   // panes -- a second call will flip it back.
   string cmd;
   const size_t nInStack = panesInStack()[iStack];
   if (iPane == 0 && (nInStack > 1 || panesInStack().size() > 1))
   {
      cmd = "label rotate";
      m_commands.push_back(cmd);
      cmd = "label pos y 4.0";
      m_commands.push_back(cmd);
   }

   if (nInStack > 2)
      cmd = "csize .75";
   else if (panesInStack().size() > 1)
      cmd = "csize .8";
   else if (nInStack == 2)
      cmd = "csize .85";
   else
      cmd = "csize 1.0";
   m_commands.push_back(cmd);


   // If pane is in a sharedX stack and is NOT the bottom, turn
   // off its x-axis numbering and do not print its x-label.
   // If pane is in a sharedX stack and is NOT the top, turn off
   // its top label.
   bool isSharedTop = false;
   bool isSharedBottom = false;
   const bool isShared = shareXaxis()[iStack];
   if (isShared)
   {
      if (iPane == previousPanes + nInStack - 1)
         isSharedBottom = true;
      if (iPane == previousPanes)
         isSharedTop = true;
   }

   const StandardLabels& paneLabels = pane->labels();
   if (isShared && !isSharedBottom)
      cmd = "label nx off";
   else
      cmd =  "label x " + paneLabels.x;
   m_commands.push_back(cmd);
   cmd = "label y " + paneLabels.y;
   m_commands.push_back(cmd);
   if (!isShared || isSharedTop)
   {
      cmd = "label t " + paneLabels.title;
      m_commands.push_back(cmd);
   }
}

void PltPkg::setPaneLineIDs (const size_t iPane)
{
   PlotPane* pane = plotPanes()[iPane];
   if (!pane->lineIDs().empty())
   {
      const PlotRange& rangeInfo = pane->ranges();
      const Real ypos = rangeInfo.ranges.y2;
      const Real yline =  (rangeInfo.yScaleType == PlotStyle::LINEAR) ?
      	0.02*(rangeInfo.ranges.y2-rangeInfo.ranges.y1) + rangeInfo.ranges.y2:
      	pow(10,0.02*(log10(rangeInfo.ranges.y2/rangeInfo.ranges.y1)) + log10(rangeInfo.ranges.y2));

      LineIDContainer::const_iterator itIDs = pane->lineIDs().begin();
      LineIDContainer::const_iterator itIDsEnd = pane->lineIDs().end();
      // Set aside the first 9 numbered labels for horizontal line
      // drawing and optional user inserted labels.  (We chose 9 to
      // allow 51 total lineID labels, same as in earlier plot designs.)
      const size_t nOTHERS = 9;
      const size_t MAXIDS = s_MAXNLABELS - nOTHERS;
      while(itIDs != itIDsEnd && s_lineIDCount < MAXIDS)
      {
         const string& idStr = itIDs->first;
         const Real xpos = itIDs->second;
         ++s_lineIDCount;
         std::ostringstream labelCmd;
         labelCmd << "label " << s_lineIDCount+nOTHERS << " pos " << xpos
		  << " " << ypos << " to " << xpos << " " << yline 
		  << " cen bottom just l rot 90 csize 0.75 " << "\"" << idStr << "\"";
         m_commands.push_back(labelCmd.str());
         ++itIDs;
      }
      if (itIDs != itIDsEnd)
      {
         tcout << "\nToo many labels for PLT - truncating after "
            << MAXIDS << " lines." <<std::endl;
      }

      // modify the upper y range to make room for the lineIDs to be plotted
      Real y2 = (rangeInfo.yScaleType == PlotStyle::LINEAR) ?
      	0.15*(rangeInfo.ranges.y2-rangeInfo.ranges.y1) + rangeInfo.ranges.y2:
      	pow(10,0.15*(log10(rangeInfo.ranges.y2/rangeInfo.ranges.y1)) + log10(rangeInfo.ranges.y2));
      PlotRange newRangeInfo(rangeInfo);
      newRangeInfo.ranges.y2 = y2;
      pane->setRanges(newRangeInfo);
   }
}

void PltPkg::setContourCmds (const size_t iPane)
{
   using namespace std;
   const PlotRange& range = plotPanes()[iPane]->ranges();
   const PlotGroup* pg = plotPanes()[iPane]->plotGroups()[0];
   // Note that despite the names xlow,ylow may be > xhigh,yhigh.
   Real xlow = range.ranges.x1;
   Real xhigh = range.ranges.x2;
   Real ylow = range.ranges.y1;
   Real yhigh = range.ranges.y2;
   if (range.xScaleType == PlotStyle::LOG)
   {
      xlow = log10(xlow);
      xhigh = log10(xhigh);
   }
   if (range.yScaleType == PlotStyle::LOG)
   {
      ylow = log10(ylow);
      yhigh = log10(yhigh);
   }
   Real xdel = (xhigh - xlow)/(pg->xAxis.data.size() - 1);
   Real ydel = (yhigh - ylow)/(pg->yData.data.size() - 1);
   ostringstream xaxcmd;
   ostringstream yaxcmd;
   xaxcmd <<  "xaxis linear " << xlow << " " << xdel;
   yaxcmd <<  "yaxis linear " << ylow << " " << ydel;
   m_commands.push_back(xaxcmd.str());
   m_commands.push_back(yaxcmd.str());
   // PLT manual recommends doing this...
   m_commands.push_back("col off 1..999");

   // contour levels command
   ostringstream contourcmd;
   contourcmd << "contour level ";
   contourcmd.setf(ios_base::scientific|ios_base::showpoint);
   contourcmd.precision(6);
   for (size_t k=0; k<range.contourLevels.size(); ++k)
     contourcmd <<" "<< range.contourLevels[k];
   m_commands.push_back(contourcmd.str());

   // contour colors command - want to jump over color 1 to avoid problems
   // with the background map

   ostringstream colorcmd;
   colorcmd << "contour color ";
   colorcmd.setf(ios_base::scientific|ios_base::showpoint);
   colorcmd.precision(6);
   for (size_t k=0; k<range.contourLevels.size(); ++k)
     colorcmd <<" "<< k+2;
   m_commands.push_back(colorcmd.str());

   // contour levels label command
   ostringstream labelcmd;
   labelcmd.precision(3);
   labelcmd.setf(ios_base::showpoint);
   ios_base& (*formatType) (ios_base& ) = fixed;
   labelcmd << "label file \"";
   formatType = (range.minStat>0.0 && abs(log10(range.minStat)) >= 2.0) 
     ? scientific : fixed;
   labelcmd <<"cross = " << formatType << range.minStat <<"; ";
   labelcmd << "Levels = " ;
   for ( size_t k=0; k<range.contourLevels.size(); ++k )
   {
      formatType = (range.contourLevels[k]>0.0 && abs(log10(range.contourLevels[k])) >= 2.0)
	? scientific : fixed;
      labelcmd << " " << formatType << range.contourLevels[k];      
   }
   labelcmd << "\"";
   m_commands.push_back(labelcmd.str());

   // Option cross at extremum
   if (range.addCrosshairs.first)
   {
      ostringstream bestcmd;
      bestcmd << "label 1 position " << range.addCrosshairs.second.first
         << " " << range.addCrosshairs.second.second << " col 5 \"+\" li";
      m_commands.push_back(bestcmd.str());
   }

   // If required add an image
   if ( range.imageRange.first != range.imageRange.second ) {
     ostringstream imagecmd;
     imagecmd << "image min " << range.imageRange.first << " max " 
	      << range.imageRange.second << " " << range.imageSpec << " cbar on";
     m_commands.push_back(imagecmd.str());
   }

}

void PltPkg::doDisplay ()
{
   int nCmds = 0;
   int cmdLength = 0;
   makeMasterWindows();
   // For backwards compatibility, leave window 1 as the focus in multi-
   // pane plots.
   if (plotPanes().size() > 1)
      m_commands.push_back("window 1");

   // Be careful with Cmds array.  For the benefit of Fortran, strings 
   // inside of it are NOT null terminated.
   XSutility::auto_array_ptr<char> apCmds(commandsToC(&nCmds, &cmdLength));

   int* nErrArrays = 0;
   int nPts = 0;
   XSutility::auto_array_ptr<float> apY(dataToC(&nErrArrays, &nPts));
   XSutility::auto_array_ptr<int> apNErrArrays(nErrArrays);

   int ier=0;
   int nVec = plotPanes()[0]->isContour() ? 
        plotPanes()[0]->plotGroups()[0]->yData.data.size() : static_cast<int>(m_nVec);
   plt_(apY.get(), nErrArrays, nPts, nPts, nVec, apCmds.get(), nCmds, ier, cmdLength);

   // Not necessary, but lets free up some space.
   m_commands.clear();
}

size_t PltPkg::determineMaxCmdLength () const
{
   size_t maxLength = 4; // length of "plot" and "exit", which may not 
                         // yet be in m_commands.
   for (size_t i=0; i<m_commands.size(); ++i)
   {
      if (m_commands[i].length() > maxLength)
         maxLength = m_commands[i].length();
   }

   std::list<string>::const_iterator itUc = userCommands()->begin();
   std::list<string>::const_iterator itUcEnd = userCommands()->end();
   while (itUc != itUcEnd)
   {
      if (itUc->length() > maxLength)
         maxLength = itUc->length();
      ++itUc;
   }
   return maxLength;
}

char* PltPkg::commandsToC (int* pnCmds, int* pcmdLength) const
{
   const size_t LENGTH = determineMaxCmdLength() + 1;
   const size_t nInternalCmds = m_commands.size();
   const size_t nUserCmds = userCommands()->size();
   size_t nCmds = nInternalCmds + nUserCmds;
   // Don't forget "plot" and "exit", at least one of which will 
   // be added to the end.
   nCmds += isInteractive() ? 1 : 2;
   XSutility::auto_array_ptr<char> apCmd(new char[nCmds*LENGTH]);
   char* cmd(apCmd.get());

   // Note that strings inside of cmd array will NOT be NULL TERMINATED.
   // PLT seems to want either a trailing ' ' or nothing (it is in Fortran
   // after all).

   memset(cmd, ' ', nCmds*LENGTH*sizeof(char));
   for (size_t i=0; i<nInternalCmds; ++i)
   {
      strncpy(cmd + i*LENGTH, m_commands[i].c_str(),m_commands[i].size());
   }
   std::list<string>::const_iterator itUc = userCommands()->begin();
   for (size_t i=0; i<nUserCmds; ++i)
   {
      strncpy(cmd + (i+nInternalCmds)*LENGTH, itUc->c_str(), itUc->size());
      ++itUc;
   }
   strncpy(cmd + (nInternalCmds+nUserCmds)*LENGTH, "plot", 4);
   if (!isInteractive())
      strncpy(cmd + (nCmds-1)*LENGTH, "exit", 4);

   if (tpout.logChatterLevel() >= 25 || tpout.consoleChatterLevel() >= 25)
   {
      for (size_t j = 0; j < nCmds; ++j)
      {
         char* subCmd = new char[LENGTH];
         memset(subCmd,0,LENGTH*sizeof(char));
         // MUST use strncpy, nothing in cmd is null terminated.
         strncpy (subCmd,cmd + j*LENGTH,LENGTH-1);
         tcout << j+1 <<": " << subCmd << std::endl; 
         delete [] subCmd;      
      }
   }

   *pnCmds = static_cast<int>(nCmds);
   *pcmdLength = static_cast<int>(LENGTH);
   return apCmd.release();
}

float* PltPkg::dataToC (int** pnErrArrays, int* pnpts) const
{
   // First iteration through groups: determine total number of data 
   // points in a column. (It is the same for all columns.)
   size_t nGroups=0;
   int nPts=0;
   float* y=0;
   const std::vector<PlotPane*>& panes = plotPanes();
   // Since we're NOT mixing regular plots and contour plots with
   // PLT, any pane can be checked to determine which type this is.
   const bool isContour = (panes.size() && panes[0]->isContour());
   if (!isContour)
   {
      for (size_t iPane=0; iPane<panes.size(); ++iPane)
      {
         const std::vector<PlotGroup*>& groups = panes[iPane]->plotGroups();
         for (size_t iGr=0; iGr<groups.size(); ++iGr)
         {
            nPts += groups[iGr]->n;
            ++nGroups;
         }
      }
      // Adjust for the additional s_NO separators between PlotGroups
      nPts += nGroups - 1;

      // Build what will become PLT's IERY[] array.  Note that this is
      // generally not the same size as the m_errorVecsInCategory vector,
      // but it MUST work out to be the same size as m_nVec.
      IntegerArray iery;
      size_t nTotErrCols=m_errorVecsInCategory[0]; // xAxis errors
      iery.push_back(static_cast<int>(m_errorVecsInCategory[0])); 
      for (size_t iCat=0; iCat<vectorCategories().size(); ++iCat)
         for (size_t j=0; j<m_vecsInCategory[iCat]; ++j)
         {
            iery.push_back(static_cast<int>(m_errorVecsInCategory[iCat+1]));
            nTotErrCols += m_errorVecsInCategory[iCat+1];
         }
      if (iery.size() != m_nVec)
         throw RedAlert("Iery array size mismatch in PltPkg");


      // No throwing from this function after here:
      const size_t nCols = m_nVec + nTotErrCols;
      y = new float[nPts*nCols];
      bool isFirst = true;
      size_t iRow = 0;
      for (size_t iPane=0; iPane<panes.size(); ++iPane)
      {
         const std::vector<PlotGroup*>& groups = panes[iPane]->plotGroups();
         for (size_t iGr=0; iGr<groups.size(); ++iGr)
         {
            const size_t nInGroup = groups[iGr]->n;
            const std::vector<PlotVector*>& alignedVecs = 
                   m_alignedPlotVectors[groups[iGr]->objectIndex-1];
            if (!isFirst) // Do not add delineator prior to first plotGroup.
            {
               for (size_t iCol=0; iCol<nCols; ++iCol)
                  y[iCol*nPts + iRow] = s_NO;
               iRow += 1;
            }

            size_t iCol=0;
            for (size_t iVec=0; iVec<m_nVec; ++iVec)
            {
               const PlotVector* vec = alignedVecs[iVec];
               size_t offset = iCol*nPts + iRow;
               if (vec)
               {
                  const std::vector<Real>& dataVals = vec->data;
                  for (size_t j=0; j<nInGroup; ++j)
                     y[offset + j] = static_cast<float>(dataVals[j]);
               }
               else
               {
                  // This is a dummy vector
                  for (size_t j=0; j<nInGroup; ++j)
                     y[offset + j] = s_NO;

               }
               iCol += 1;

               for (size_t iErr=0; iErr<static_cast<size_t>(iery[iVec]); ++iErr)
               {
                  offset = iCol*nPts + iRow;
                  if (vec && iErr < vec->errors.size())
                  {
                     const std::vector<Real>& errs = vec->errors[iErr];
                     for (size_t j=0; j<nInGroup; ++j)
                     {
                        y[offset + j] = static_cast<float>(errs[j]);
                     }
                  }
                  else
                  {
                     // A dummy error array is required.
                     for (size_t j=0; j<nInGroup; ++j)
                     {
                        y[offset + j] = s_NO;
                     }                  
                  }
                  iCol += 1;               
               } // end lopp over iery[iVec]
            } // end loop over m_nVec
            isFirst = false;
            iRow += nInGroup;
         } // end loop over PlotGroups in pane
      }  // end loop over panes
      int* nErrArrays = new int[m_nVec];
      for (size_t i=0; i<m_nVec; ++i)
         nErrArrays[i] = iery[i];
      *pnErrArrays = nErrArrays;
   } // end if not a contour plot
   else
   {
      // 2-D contour plot
      const PlotGroup* pg = panes[0]->plotGroups()[0];
      nPts = pg->xAxis.data.size();
      // m_nVec as previously set (in determineMaxInCategory) does not apply 
      // here, and we can't change it because this is a const function.
      const size_t nVec = pg->yData.data.size();
      const size_t nSquare = pg->n;
      y = new float[nSquare];
      const std::vector<Real>& gridPts = pg->model[0].data;
      for (size_t i=0; i<nSquare; ++i)
         y[i] = gridPts[i];
      int* nErrArrays = new int[nVec];
      for (size_t i=0; i<nVec; ++i)
         nErrArrays[i] = 0;
      *pnErrArrays = nErrArrays;
   }
   *pnpts = nPts;
   return y;
}

Real PltPkg::BADVALUE () const
{
   return s_NO;
}

void PltPkg::makeMasterWindows ()
{
   // No need to do this unless it is a sharedX stack with more than
   // one pane.  For backwards compatibility with earlier versions,
   // let's make the top pane the master even though the bottom
   // might seem to be more natural.

   size_t iPane = 1; // 1-based for Plt.
   for (size_t iStack=0; iStack<panesInStack().size(); ++iStack)
   {
      const size_t nPanes = panesInStack()[iStack];
      if (shareXaxis()[iStack] && nPanes > 1)
      {
         std::ostringstream mmaster;
         mmaster << "mmaster " << iPane << " x ";
         ++iPane;
         for (size_t iInStack=1; iInStack < nPanes; ++iInStack)
         {
            mmaster << iPane << " ";
            ++iPane;
         }
         m_commands.push_back(mmaster.str());
      }
      else
         iPane += nPanes;
   }
}

void PltPkg::flushHardcopy ()
{
   size_t strSz = deviceName().length();
   string dev(XSutility::lowerCase(deviceName()));
   if (strSz > 2 && dev.substr(strSz-2,2) == "ps" )
   {
	 PGUPDT();
	 PGPAGE();
   }  
}

// Additional Declarations
