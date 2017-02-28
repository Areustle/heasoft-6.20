# When this class is initialized in FVFtool.tcl, the constructor will create a
# ::xterminalTab namespace with various variable names

itcl::class xterminalTabs {

   # Create and initialize xterminalTab namespace
   constructor { wndw cSession }  {}
   destructor {}

   public {
      method addPage { {page "Tab"} }
      method deletePage { page }
      method raisePage { page }
      method updateTitle { page newText }
   }

   private {
      method _buildPage { w page }

      variable note
      variable _FGUIRC
      variable _padXAmnt 10
      variable _padYAmnt 3
      variable _tabLabels {}
      variable _topLevel
      variable _newTerm
      variable _callSession
   }
}

itcl::body xterminalTabs::constructor { wndw cSession } {
    global mainSession

    set _topLevel $wndw 
    set _callSession $cSession
    set mainSession $cSession

    frame ${_topLevel}.xterminalTab
    pack  ${_topLevel}.xterminalTab -fill both -expand true
    set note ${_topLevel}.xterminalTab.notebook
    Notebook:create $note -pages $_tabLabels -pad 8
    pack $note -fill both -expand true
}

itcl::body xterminalTabs::destructor { } {
   if { [winfo exists .xterminalTab] } {
      destroy .xterminalTab
   }
}

itcl::body xterminalTabs::addPage { {page "Tab"} } {
   global tcl_platform
   global g_titleFont
   
   ###########
   # Setup Tabbed Notebook Widget
   ###########

   set cnt [Notebook:add $note $page]

   lappend _tabLabels $page
   eval _buildPage $note.f$cnt $page

   Notebook:raise $note $page
   Notebook:raise.page $note [lsearch $_tabLabels $page]
   Notebook:resize $note

   $_callSession setCurrentTab $page
   return $_newTerm
}

itcl::body xterminalTabs::deletePage { page } {
   set deleteXterm [$_callSession getTerminalId $page]
   set idx [lsearch $_tabLabels $page]

   set _tabLabels [lreplace $_tabLabels $idx $idx]
   Notebook:delete $note $page
   catch { itcl::delete object $deleteXterm } err
   Notebook:resize $note
}

itcl::body xterminalTabs::updateTitle { page newText } {
   Notebook:updateTitle $note $page $newText
}

itcl::body xterminalTabs::raisePage { page } {

   $_callSession setCurrentTab $page

   Notebook:raise.page $note [lsearch $_tabLabels $page]
   Notebook:resize $note
     
}

##########################################################
#############      Private Functions      ################
##########################################################


itcl::body xterminalTabs::_buildPage { w page } {
   global g_titleFont

   set row 1
   Notebook:raise $note $page
   set _newTerm [gXTerminal $w command NONE $this $_callSession]

   $_callSession addNewXterminal $page $_newTerm

   Notebook:modifyTerm $note $page $_newTerm
   return $_newTerm
}
