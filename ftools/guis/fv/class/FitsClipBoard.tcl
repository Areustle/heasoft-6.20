# FitsClipBoard class
# Function: keep data (keywords, columns, rows or a block of data ) 
# for copying ...
# infoType:                            infoInfo:
#          tableBlock    {xdim ydim $blocks}  : blocks is a list of strings
#          tableRows     {numCols   $blocks}  :
#                                             : block ans rows use tcl variables
#          tableColumn   {dataAddressForPOW type numRows colName colType colUnit colNull}
#                                             : column use c variable in memory
#          keywords      {numKeys $keys}      : keywords use tcl string.
#          hdus          {numHDUs $sourceFileName} :
# 
#

itcl::class FitsClipBoard {
    
    constructor {args} {}
    destructor {}

    public method toggleView {}
    public method hide {}
    public method activate {}
    public method register {type src desc dataList}
    public method report {}
    public method clean {}
    public method hasRec {}
    
    private method _buildWindow {}
    private method _postMenus {}

    private variable _infoType "None"
    private variable _infoSrc  "None"
    private variable _infoInfo "None"
    private variable _dataBlock ""
    private variable _droot
    private variable _mBar
}


itcl::body FitsClipBoard::constructor {args} {
   _buildWindow
   hide
}


itcl::body FitsClipBoard::destructor {} {
    global g_backupDir

    catch { file delete [file join $g_backupDir cb.fits] }
    catch { destroy $_droot }
}

itcl::body FitsClipBoard::_buildWindow {} {
   global isMac
   global g_titleFont

   set _droot .[namespace tail $this]
   powToplevel $_droot .dummy

   wm title $_droot "fv Clipboard"

# Bind Menubar Events
   
   bind $_droot <<CloseWindow>> [itcl::code $this hide]
   bind $_droot <<Clear>>       [itcl::code $this clean]
   bind $_droot <<PostMenus>>   [itcl::code $this _postMenus]

# Create Menubar
   
   if { $isMac } {
       set _mBar .mbar.clip
       set evtWndw ""
   } else {
       set _mBar $_droot.mbar
       set evtWndw $_droot
   }
   $_droot config -menu $_mBar
    
   if { ![winfo exists $_mBar] } {
       menu $_mBar -postcommand "doMenuEvent <<PostMenus>> $evtWndw"
       if { $isMac } {
           $_mBar add cascade -menu $_mBar.apple
           $_mBar add cascade -menu $_mBar.file  -label File
           $_mBar add cascade -menu $_mBar.edit  -label Edit
           $_mBar add cascade -menu .mbar.wind  -label Windows
           $_mBar add cascade -menu $_mBar.help  -label Help
           buildAppleStyleMenu $_mBar.apple
           buildFileMenu $_mBar.file
           buildEditMenu $_mBar.edit
           # Opts and Wind use global menu
           buildHelpMenu $_mBar.help v v
           $_mBar.help delete 0 1
           $_mBar.file entryconfig "Close" -state normal -font g_titleFont
       } else {
           $_mBar add cascade -menu $_mBar.edit -label Edit -font g_titleFont
           menu $_mBar.edit -tearoff False \
                 -postcommand "doMenuEvent <<PostMenus>> $evtWndw"
           $_mBar.edit add command -label Clear \
               -command "doMenuEvent <<Clear>>"
           $_mBar.edit add command -label Close -font g_titleFont \
               -command "doMenuEvent <<CloseWindow>>"
       }
   }
    
# Create Window Body
   frame $_droot.bodyF
   pack  $_droot.bodyF -side top -fill both -expand 1
   
   label $_droot.bodyF.typeT -text "Type" -width 10 -font g_titleFont
   label $_droot.bodyF.srcT  -text "Source" -width 10 -font g_titleFont
   label $_droot.bodyF.infoT -text "Info" -width 10 -font g_titleFont
   frame $_droot.bodyF.sepF  -relief raised -bd 2 -height 6
   
   label $_droot.bodyF.typeL -textvariable [itcl::scope _infoType] -font g_titleFont
   label $_droot.bodyF.srcL  -textvariable [itcl::scope _infoSrc] -font g_titleFont
   label $_droot.bodyF.infoL -textvariable [itcl::scope _infoInfo] -font g_titleFont
   
   grid configure $_droot.bodyF.typeT -column 0 -row 0 -sticky "snew"
   grid configure $_droot.bodyF.srcT  -column 2 -row 0 -sticky "snew"
   grid configure $_droot.bodyF.infoT -column 1 -row 0 -sticky "snew"
   grid configure $_droot.bodyF.sepF  -column 0 -row 1 -columnspan 3 \
	 -sticky "snew" 
   grid configure $_droot.bodyF.typeL -column 0  -row 3 -sticky "snew"
   grid configure $_droot.bodyF.srcL  -column 2 -row 3 -sticky "snew"
   grid configure $_droot.bodyF.infoL -column 1 -row 3 -sticky "snew"

   grid columnconfigure $_droot.bodyF 2 -weight 5
}

itcl::body FitsClipBoard::_postMenus {} {
    global isMac

    if { [hasRec] } {
        $_mBar.edit entryconfig "Clear" -state normal -font g_titleFont
    } else {
        $_mBar.edit entryconfig "Clear" -state disabled -font g_titleFont
    }
   update idle
}


itcl::body FitsClipBoard::toggleView {} {
   if { ! [winfo exists $_droot] } {
      _buildWindow
   }
   if { [winfo viewable $_droot] } {
      hide
   } else {
      activate
   }	 
}

itcl::body FitsClipBoard::hide {} {
    wm withdraw $_droot
}

itcl::body FitsClipBoard::activate {} {
    wm deiconify $_droot
    raise $_droot
}

itcl::body FitsClipBoard::register {type src desc dataList} {
    if { [hasRec] } {
	clean
    }

    set _infoType $type
    set _infoSrc  $src
    set _infoInfo $desc
    set _dataBlock $dataList
}

itcl::body FitsClipBoard::clean {} {
    if { ![hasRec] } {
	return 
    }

# clean up the tcl variables
    set _infoType "None"
    set _infoSrc  "None"
    set _infoInfo "None"
}

itcl::body FitsClipBoard::hasRec {} {
    if { $_infoType == "None" } {
	return 0
    } else {
	return 1
    }
}

itcl::body FitsClipBoard::report {} {
    return [list $_infoType $_infoSrc $_dataBlock]
}
