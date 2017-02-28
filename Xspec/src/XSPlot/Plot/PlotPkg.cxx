//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSPlot/Plot/PlotTypes.h>
#include <XSUtil/Error/Error.h>

// PlotPane
#include <XSPlot/Plot/PlotPane.h>
// PlotPkg
#include <XSPlot/Plot/PlotPkg.h>



// Class PlotPkg 
std::vector<PlotStyle::VectorCategory> PlotPkg::s_vectorCategories = std::vector<PlotStyle::VectorCategory>();

PlotPkg::PlotPkg()
  :m_deviceName(),
   m_shareXaxis(),
   m_panesInStack(),
   m_userCommands(0), // Non-owning
   m_isInteractive(false),
   m_plotPanes()
{
   if (!s_vectorCategories.size())
   {
      s_vectorCategories.push_back(PlotStyle::DATA);
      s_vectorCategories.push_back(PlotStyle::BACKGROUND);
      s_vectorCategories.push_back(PlotStyle::MODEL);
      s_vectorCategories.push_back(PlotStyle::SOURCES);
   }
}


PlotPkg::~PlotPkg()
{
   // Presumably PlotDirector would have cleared all PlotPanes
   // long before this gets destroyed, but this call can't hurt.
   clearPanes();
}


Real PlotPkg::BADVALUE () const
{
   // Specific plotting packages can override this.
   return -999.999;
}

void PlotPkg::clearPanes () throw ()
{
  for (size_t i=0; i<m_plotPanes.size(); ++i)
     delete m_plotPanes[i];
  m_plotPanes.clear();
}

void PlotPkg::display ()
{
   if (!m_deviceName.length())
      throw YellowAlert("Unable to Plot - No plot device has been set.\n");
   calculatePanePositions();   

   doInitialize();

   for (size_t i=0; i<m_plotPanes.size(); ++i)
   {
      setPanePosition(i);
      // The position of the setContour insertion is dictated 
      // specifically by PLT.  It performs best if the rescale commands
      // (in setPaneRanges) come AFTER the contour "xaxis/yaxis linear"
      // commands. 
      if (m_plotPanes[i]->isContour())
         setContourCmds(i);
      setPaneLineIDs(i);
      setPaneRanges(i);
      setPaneStyles(i);
      setPaneLabels(i);
   }

   doDisplay();
}

void PlotPkg::flushHardcopy ()
{
}

void PlotPkg::doInitialize ()
{
}

void PlotPkg::setPanePosition (const size_t iPane)
{
}

void PlotPkg::setPaneRanges (const size_t iPane)
{
}

void PlotPkg::setPaneStyles (const size_t iPane)
{
}

void PlotPkg::setPaneLabels (const size_t iPane)
{
}

void PlotPkg::setPaneLineIDs (const size_t iPane)
{
}

void PlotPkg::setContourCmds (const size_t iPane)
{
}

void PlotPkg::calculatePanePositions ()
{
   // 9 independent coordinates for placing up to 6 panes in 2 stacks,
   // with or without shared X axis.  The coordinates refer to just the
   // actual viewport (in PLT parlance), and DO NOT encompass labels
   // and axis numerals.

   const Real x0 = 0.1;
   const Real y0 = 0.1;
   const Real dx1 = .8; // width of single pane plot
   const Real dy1 = .8; // height of single pane plot
   const Real dx2 = 0.35; // width of pane in 2 stack plot
   const Real stack2Gap = .15; // gap between 2 stacks
   const Real dyu = 0.5; // height of upper pane in shared X plot
   const Real dy2 = 0.32; // height of pane in a stack of 2, not shared X
   const Real dy3 = 0.19; // height of pane in a stack of 3, not shared X

   // Order of pane placement: If plot commands are numbered 1-to-N from
   // left-to-right, place in window from top down and then left-to-right
   // order.  For example, "plot data model resid chisq" would be placed:
   //    data    resid
   //    model   chisq

   size_t iPane=0;
   const size_t nStacks = m_panesInStack.size();
   if (nStacks > 2)
      throw YellowAlert("Only 2 stacks of plot panes are currently handled.\n");

   for (size_t iStack=0; iStack<nStacks; ++iStack)
   {
      const bool isSharedX = m_shareXaxis[iStack];
      const size_t nInStack = m_panesInStack[iStack];
      if (nInStack > 3)
         throw YellowAlert("Only 3 panes may appear in a plot stack.\n");
      for (size_t jInStack=0; jInStack<nInStack; ++jInStack)
      {
         PlotRectangle position;
         // X values are the same regardless of isSharedX setting.
         if (iStack == 0)
         {
            position.x1 = x0;
            position.x2 = nStacks > 1 ? x0 + dx2 : x0 + dx1;
         }
         else // Must be 2nd and last stack.
         {
            position.x1 = x0 + dx2 + stack2Gap;
            position.x2 = position.x1 + dx2;
         }


         if (isSharedX)
         {
            switch (nInStack)
            {
               case 1:
                  // There must only be 1 stack.
                  position.y1 = y0;
                  position.y2 = y0 + dy1;
                  break;
               case 2:
                  // Upper pane will be taller
                  if (jInStack == 0)
                  {
                     position.y1 = y0 + dy1 - dyu;
                     position.y2 = y0 + dy1;
                  }
                  else
                  {
                     position.y1 = y0;
                     position.y2 = y0 + dy1 - dyu;
                  }
                  break;
               case 3:
               {
                  // All 3 panes are same height.
                  Real height = dy1/3.0;
                  position.y1 = y0 + dy1 - (jInStack+1)*height;
                  position.y2 = y0 + dy1 - jInStack*height;

               }
               default:
                  break;
            }
         } // end if shared X
         else
         {
            // Not sharing X axis, must be at least 2 panes in stack.
            if (nInStack == 2)
            {
               if (jInStack == 0)
               {
                  position.y1 = y0 + dy1 - dy2;
                  position.y2 = y0 + dy1;
               }
               else
               {
                  position.y1 = y0;
                  position.y2 = y0 + dy2;
               }
            }
            else // 3 panes in stack
            {
               const Real gap = (dy1 - 3.0*dy3)/2.0;
               position.y1 = y0 + dy1 - jInStack*(gap+dy3) - dy3;
               position.y2 = y0 + dy1 - jInStack*(gap+dy3);
            }
         }
         m_plotPanes[iPane]->panePosition(position);
         m_plotPanes[iPane]->stackIndex(iStack);
         ++iPane;
      } // end loop over nInStack
   } // end loop over stacks

}

// Additional Declarations
