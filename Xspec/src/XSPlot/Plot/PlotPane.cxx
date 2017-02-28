//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// PlotGroup
#include <XSPlot/Plot/PlotGroup.h>
// PlotPane
#include <XSPlot/Plot/PlotPane.h>



// Class PlotPane 

PlotPane::PlotPane (const PlotRange& ranges, const StandardLabels& labels, const std::vector<PlotGroup*>& plotGroups)
  :m_ranges(ranges),
   m_labels(labels),
   m_panePosition(),
   m_stackIndex(0),
   // This ASSUMES plotGroups is NOT empty (PlotDirector should have
   // caught that case before creating this), and that just one
   // group exists for 2-D contour plots:
   m_isContour(plotGroups[0]->is2Dcontour),
   m_lineIDs(),
   m_plotGroups(plotGroups) // Non-owning, borrowed from PlotDirector.
{
   m_panePosition.x1 = m_panePosition.x2 = m_panePosition.y1 = m_panePosition.y2 = 0.0;
}


PlotPane::~PlotPane()
{
}


// Additional Declarations
