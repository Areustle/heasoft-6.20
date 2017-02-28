# First draft 02/06/97    Jianjun
#constrct a FitsExtension class to let header and table share one FitsFileObj
# FitsExtension fMode currentHDU fileName father header/data

itcl::class FitsExtension {

    public method dispHeader {}
    public method skipTable  { {content_ {}} }
    public method callHistogram { extNum_ fillFlag_ }
    public method dispTable  { extNum_ {content_ {}} forceNew_ }
    public method displaySlice { extNum_ slice }
    public method plotData   { {params_  {}} }
    public method addChild   { child_ type_ }
    public method freeChild  { child_ }
    public method updateHL   { field_ value_ }
    public method updateDisps { child_ }
    public method setFileName { fName_ }
    public method closeCmd   { }

    private method _getImageSlice { numSlices_ }
    private method _getTableSlice { numSlices_ }

    public variable fFile
    public variable fileName

# this is only one header
    private variable _myHeader ""
# this is a LIST of Tables
    private variable _myTables {}

    private variable _fatherFitsFile
    private variable _currentHDU

    # These are scratch variables for handling 3D images
    private variable _itype
    private variable _istart
    private variable _iend
    private variable _islice
    private variable _cslice 1
    private method _setIType { type_ }
    private variable imgdim

    # when plotting a 1-d image
    private method _oneDimImages {}

    constructor {args} {}
    destructor         {}
    
}

# FitsExtension fMode currentHDU fileName father header/data
itcl::body FitsExtension::constructor {args} {
    global totalCoordinateSystem

    set fMode        [lindex $args 0] 
    set _currentHDU  [lindex $args 1]
    set fileName     [lindex $args 2]
    set _fatherFitsFile      [lindex $args 3]

    if { $fMode == 1 } {
	set fFile [FitsFile #auto $fileName $fMode $_fatherFitsFile]
    } else {
	if { [file writable $fileName] } {
	    set fFile [FitsFile #auto $fileName $fMode $_fatherFitsFile]
        } else {
            set fFile [FitsFile #auto $fileName 1 $_fatherFitsFile]
	}
    }

    $fFile moveToHDU $_currentHDU
    set totalCoordinateSystem 1
    foreach desn [list a b c d e f g h i j k l m n o p q r s t u v w x y z] {
       if { [catch {set ctype [$fFile getKeyword "CTYPE1$desn"]} ] == 0 } {
          incr totalCoordinateSystem
       }
    }
    $_fatherFitsFile addChild $this $_currentHDU
}

itcl::body FitsExtension::destructor {} {
   itcl::delete object $fFile
   $_fatherFitsFile freeChild $_currentHDU
}

itcl::body FitsExtension::dispHeader {} {

   if { $_myHeader == "" } {
      set _myHeader [namespace current]::[FitsHeader #auto $fFile $this]
   } else {
      $_myHeader bringToFront
   }
   return $_myHeader
}

itcl::body FitsExtension::callHistogram { extNum_ fillFlag_ } {
     set fT [FitsTable #auto $fFile $this]
     set fT [namespace current]::$fT
     $fT callHistogramDirectly 1 $fillFlag_
     return 
}

itcl::body FitsExtension::skipTable { {content_ {}} } {
     set fT [FitsImage #auto $fFile $this]
     set fT [namespace current]::$fT
     return [$fT _sPlotCmd]
}

itcl::body FitsExtension::dispTable { extNum_ {content_ {}} forceNew_ } {

# content_ is the coor_ passed into FitsFile::openTable
# content_ is "" if one just pushes a button
#    it only has content if something else calls FitsFile::openTable
#          -- for example, when double an image pixel, the image coors are
#               passed in

# forceNew_ is 1 if the right mouse button was pressed on the "Table" button
# forceNew_ is 0 if not; usually due to pressing left mouse on "Table" button

# if a table exists and we don't have to forceNew_ a new one, return the table
   if { !$forceNew_ } {
      set oneRaised 0
      foreach fT $_myTables {
         if { [$fT bringToFront] } {
            set oneRaised 1
            set raisedTable $fT
         }
      }
      if { $oneRaised } {
          # if $content_ is "coord1 coord2" from clicking on image pixels
          if { [llength $content_] > 1 } {
             # Highlight the indicated pixel
             update idletask
             $raisedTable showCell [lindex $content_ 0] [lindex $content_ 1]
         }
         return [lindex $_myTables 0]
      }
   }

# otherwise create the new table
#   decide whether we're creating a FitsTable, FitsImage, or CubeImage
         
   set type [lindex [$fFile getTableInfo hdutype] 1]

# TABLE
   if { $type == "Table" } {
      set fT [FitsTable #auto $fFile $this]
      # don't know why this "namespace current" is here
      set fT [namespace current]::$fT
      if { !$forceNew_ && $content_ == "" } {
#  most common scenario resulting from left-button click on Table button
         set content_ "-"
      }
      $fT dispTable $extNum_ $content_

# IMAGE ($type could be "extension")
   } else {
      set imgdim [$fFile getImgInfo]
 # CUBE
      if { [llength $imgdim] > 2 && [lindex $imgdim 2] > 1 } {
         if { [llength $content_] == 1 || [llength $content_] == 3 } {
            set _islice [lindex $content_ end]
         } else {
            while { 1 } {
               set _islice 1
               _getTableSlice [lindex $imgdim end]

               if { $_islice == -99 } return
               if { $_cslice < 1 || $_cslice > [lindex $imgdim end] } {
                  tk_messageBox -icon error -type ok -message "Cube selection out of bound\nvalue should between 1 and [lindex $imgdim end]"
                  continue
               }

               if { $_islice < 1 || $_islice > [lindex $imgdim end] } {
                  tk_messageBox -icon error -type ok -message "Slice selection out of bound\nvalue should between 1 and [lindex $imgdim end]"
                  continue 
               }

               break
            }
         }

         set fT [CubeImage #auto $fFile $this $_islice $_islice $_cslice]
         set fT [namespace current]::$fT
      } else {
 # 2-D IMAGE
         set fT [FitsImage #auto $fFile $this]
         set fT [namespace current]::$fT
      }

      $fT makeTable $extNum_

      if { [llength $content_] > 1 } {
         # Highlight the indicated pixel
         update idletask
         $fT showCell [lindex $content_ 0] [lindex $content_ 1]
      }
   }

   return $fT
}

itcl::body FitsExtension::displaySlice { extNum_ slice } {
     set fI [FitsImage #auto $fFile $this]
     # set fI [CubeImage #auto $fFile $this $slice $slice]
     set fI [namespace current]::$fI
     return [$fI makeImage]
}

itcl::body FitsExtension::plotData { {params_ {}} } {
   set type [lindex [$fFile getTableInfo hdutype] 1]
   if { $type == "Table" } {
      set fT [FitsTable #auto $fFile $this]
      $fT scplotCmd $params_ 

   } else {
       # Image table
       set imgdim [$fFile getImgInfo]
       if { [llength $imgdim] == 1 } {
	     set fI [FitsImage  #auto $fFile $this]
             if { [llength $params_] ==0 } {
                   set curveType "column"
                   set _istart 1
                   set _iend 1
                   set inCurrgn 0
             } elseif { [llength $params_] ==1 } {
                   if { [lindex $params_ 0 ] =="-rows" } {
                         set curveType "row"
                   } else {
               		 set curveType "column"
                   }
                   set _istart 1
                   set _iend 1
                   set inCurrgn 0
             } elseif { [llength $params_] ==2 } {
                   if { [lindex $params_ 0 ] =="-rows" } {
                         set curveType "row"
                   } else {
               		 set curveType "column"
                   }
                   set _istart [lindex $params_ 1]
                   set _iend [lindex $params_ 1]
                   set inCurrgn 0
             } elseif { [llength $params_] ==3 } {
                   if { [lindex $params_ 0 ] =="-rows" } {
                         set curveType "row"
                   } else {
               		 set curveType "column"
                   }
                   set _istart [lindex $params_ 1]
                   set _iend [lindex $params_ 2]
                   set inCurrgn 0
             } else {
                   if { [lindex $params_ 0 ] =="-rows" } {
                         set curveType "row"
                   } else {
               		 set curveType "column"
                   }
                   set _istart [lindex $params_ 1]
                   set _iend [lindex $params_ 2]
                   set inCurrgn [lindex $params_ 3]
             }


                 
	   #  $fI _sPlotCmd 
           $fI imagePlot [list $curveType $_istart $_iend $inCurrgn]
	   # one-dimensional images will be line-plotted
	   # _oneDimImages
       } else {
	   # standard images
	   # image slices
	   if { [llength $imgdim] > 2 && [lindex $imgdim 2] > 1 } {
	       if { [llength $params_] == 0 } {
		   _getImageSlice [lindex $imgdim 2]
                  if { $_cslice < 1 || $_cslice > [lindex $imgdim end] } {
                     tk_messageBox -icon error -type ok -message "Cube selection out of bound\nvalue should between 1 and [lindex $imgdim end]"
                     return
                  } else {
                     set _istart [expr ($_cslice - 1) * [lindex $imgdim 2] + $_istart]
                     set _iend   [expr ($_cslice - 1) * [lindex $imgdim 2] + $_iend]
                  }
	       } elseif { [llength $params_] == 1 } {
		   set _istart $params_
		   set _iend   $params_
		   set _itype  "Animate"
	       } else {
		   set _istart [lindex $params_ 0]
		   set _iend   [lindex $params_ 1]
		   set _itype  "Animate"
	       }
#puts "FitsExtension: params: $params_"
#puts "FitsExtension: _itype: $_itype"
#puts "FitsExtension: _istart: $_istart"
#puts "FitsExtension: _iend: $_iend"
#puts "FitsExtension: _cslice: $_cslice"
	       switch $_itype {
		   "Mosaic" {
		       # Not enabled
		       set fI [FitsImage  #auto $fFile $this]
		   }
		   "WFPC" { 
		       set fI [WFPC2Image #auto $fFile $this]
		   }
		   "Animate" {
		       set fI [CubeImage  #auto $fFile $this $_istart $_iend $_cslice]
		   }
		   default {
		       return
		   }
	       }
	   } else {
	       set fI [FitsImage #auto $fFile $this]
	   }
	   if { [lindex $params_ 0] == "-rows" || [lindex $params_ 0] == "-cols" } {
	       set inCurrgn 0
	       set curveType [string range [lindex $params_ 0] 1 3]
	       
	       if { [llength $params_] == 1 } {
		   error "Usage: display curve extNum -rows|-cols start ?end? ?currGraph?"
	       } elseif { [llength $params_] == 2 } {
		   set _istart [lindex $params_ 1]
		   set _iend   $_istart
	       } elseif { [llength $params_] == 3 } {
		   set _istart [lindex $params_ 1]
		   set _iend   [lindex $params_ 2]
	       } else {
		   set _istart [lindex $params_ 1]
		   set _iend   [lindex $params_ 2]
		   set inCurrgn [lindex $params_ 3]
	       }
	       $fI imagePlot [list $curveType $_istart $_iend $inCurrgn]
	   } else {
	       $fI makeImage
	   }
       }
   }
}

itcl::body FitsExtension::_getImageSlice { numSlices_ } {
   # Sets '_itype' to the image type to display, and '_istart/_iend' if
   # an animation is desired.  '_itype' will be "" if canceled

   set _istart 1
   set _islice 1
   set _iend   $numSlices_
   set _itype  ""

   if { [catch {set instName [$fFile getKeyword "INSTRUME"]} ] == 0 } {
      set instName [string range [string trim [lindex \
            [join $instName] 1] "' "] 0 3] 
   } else {
      set instName ""
   }
   
   # image selection
   if { [winfo exist .fv_imgsel] == 1 } {
      destroy .fv_imgsel
   }
   powToplevel  .fv_imgsel .dummy
   wm title     .fv_imgsel "fv: Image Selection"
   
   if { $instName == "WFPC"} {
      label .fv_imgsel.label -text "This is an HST/WFPC image"  \
            -relief ridge -borderwidth 4
      button .fv_imgsel.mosaic -text "WFPC" \
            -command "[itcl::code $this _setIType WFPC]
                      destroy .fv_imgsel"
   } else {
      if { [llength $imgdim] <= 3 } {
         set labelTxt "The [llength $imgdim]D image has $numSlices_ slices"
      } else {
         set labelTxt "The [llength $imgdim]D image contains [lindex $imgdim end] data cube, each with $numSlices_ slices"
         label .fv_imgsel.dl   -width  10  -text "Data Cube"
         entry .fv_imgsel.de   -width 10  -textvariable [itcl::scope _cslice]
      }
      label .fv_imgsel.label -text $labelTxt -relief ridge -borderwidth 4
      button .fv_imgsel.mosaic -text "Mosaic" \
            -command "[itcl::code $this _setIType MOSAIC]
                      destroy .fv_imgsel" \
            -state disabled
   }
   label .fv_imgsel.sl  -width  5  -text "Start"
   entry .fv_imgsel.se  -width 10  -textvariable [itcl::scope _istart]
   label .fv_imgsel.el  -width  5  -text "End"
   entry .fv_imgsel.ee  -width 10  -textvariable [itcl::scope _iend]
   
   button .fv_imgsel.anim -text "Animate" \
         -command "[itcl::code $this _setIType Animate]
                   destroy .fv_imgsel"
   button .fv_imgsel.cancel -text "Cancel" \
         -command "destroy .fv_imgsel"
   button .fv_imgsel.help -text "Help" \
         -command "hhelp 3D-ImageDisplay"


   grid .fv_imgsel.label  -column 0 -row 0  -columnspan 4 -sticky "snew"

   set rowIdx 1
   if { [llength $imgdim] > 3 } {
      grid .fv_imgsel.dl     -column 0 -row 1 
      grid .fv_imgsel.de     -column 1 -row 1 
      incr rowIdx
   }
   grid .fv_imgsel.sl     -column 0 -row $rowIdx
   grid .fv_imgsel.se     -column 1 -row $rowIdx
   grid .fv_imgsel.el     -column 2 -row $rowIdx
   grid .fv_imgsel.ee     -column 3 -row $rowIdx
   incr rowIdx

   grid .fv_imgsel.anim   -column 0 -row $rowIdx
   grid .fv_imgsel.mosaic -column 1 -row $rowIdx
   grid .fv_imgsel.cancel -column 2 -row $rowIdx
   grid .fv_imgsel.help   -column 3 -row $rowIdx
   
   tkwait window .fv_imgsel
}


itcl::body FitsExtension::_getTableSlice { numSlices_ } {
   # Sets '_islice' to the image slice to display, or -1 if canceled

   set _istart 1
   set _islice 1
   set _cslice 1
   set _iend   $numSlices_
   set _itype  ""

   # image selection
   if { [winfo exist .fv_imgsel] } {
      destroy .fv_imgsel
   }
   powToplevel  .fv_imgsel .dummy
   wm title     .fv_imgsel "fv: Image Selection"
   
   if { [llength $imgdim] <= 3 } {
      set labelTxt "The [llength $imgdim]D image has $numSlices_ slices"
   } else {
      set labelTxt "The [llength $imgdim]D image contains $numSlices_ data cube, each with [lindex $imgdim 2] slices"
      label .fv_imgsel.dl   -width  10  -text "Data Cube"
      entry .fv_imgsel.de   -width 10  -textvariable [itcl::scope _cslice]
      bind .fv_imgsel.de <Return> "[itcl::code $this _setIType Slice]
                                   destroy .fv_imgsel"
   }

   label .fv_imgsel.label -text $labelTxt -relief ridge -borderwidth 4
   
   label .fv_imgsel.sl   -width  12  -text "Image Slice"
   entry .fv_imgsel.se   -width 10 -textvariable [itcl::scope _islice]
   bind .fv_imgsel.se <Return> "[itcl::code $this _setIType Slice]
                                destroy .fv_imgsel"
   
   button .fv_imgsel.display -text "Display" -command "[itcl::code $this _setIType Slice]
                                                      destroy .fv_imgsel"

   button .fv_imgsel.cancel -text "Cancel" \
         -command "[itcl::code $this _setIType CANCEL]
                   destroy .fv_imgsel"
   button .fv_imgsel.help -text "Help" \
         -command "hhelp 3D-ImageTable"


   grid .fv_imgsel.label   -column 0 -row 0  -columnspan 4 -sticky "snew"
   set rowIdx 1
   if [winfo exists .fv_imgsel.dl] {
      grid .fv_imgsel.dl      -column 0 -row $rowIdx 
      grid .fv_imgsel.de      -column 1 -row $rowIdx 
      incr rowIdx
   }

   grid .fv_imgsel.sl      -column 0 -row $rowIdx 
   grid .fv_imgsel.se      -column 1 -row $rowIdx 
   incr rowIdx

   grid .fv_imgsel.display -column 0 -row $rowIdx 
   grid .fv_imgsel.cancel  -column 1 -row $rowIdx 
   grid .fv_imgsel.help    -column 2 -row $rowIdx 
   
   tkwait window .fv_imgsel
   
   if { $_itype == "" } {
      set _islice -1
   } elseif { $_itype == "CANCEL" } {
      set _islice -99
   } else {
       # extract only the first number
       regexp {[0123456789]*} $_islice newslice
       if { $newslice != $_islice } {
    error "Must enter the number of slice you wish to browse.  A range is not allowed because only 1 table slice can be browsed at a time."
       }
   }
}

itcl::body FitsExtension::_setIType { type_ } {
   set _itype $type_
}


itcl::body FitsExtension::addChild { child_ type_ } {
   if { $type_ == "header" } {
# there can only be one header per extension
      if { $_myHeader == "" } {
         set _myHeader $child_
      } else {
         puts "_myHeader already has value $_myHeader"
      }
   } elseif { $type_ == "table" } {
# but many tables are possible per extension
      lappend _myTables $child_
   }
}

itcl::body FitsExtension::freeChild { child_ } {
   if { $child_ == $_myHeader } {
      set _myHeader ""
   } else {
      set pos [lsearch $_myTables $child_]
      if { $pos != -1 } {
         set _myTables [lreplace $_myTables $pos $pos]
      } else {
         puts "Unable to find child $child_"
      }
   }
# if we don't have a header or table child, then this extension isn't
#   needed, so delete it
   if { $_myHeader == "" && [llength $_myTables] == 0 } {
      itcl::delete object $this
   }
}

itcl::body FitsExtension::updateHL { field_ value_ } {
   $_fatherFitsFile updateHighLight $_currentHDU $field_ $value_
}

itcl::body FitsExtension::updateDisps { child_ } {
# sometimes there is nothing for $_myHeader or $_myTables, hence we use foreach
   foreach head $_myHeader {
      if { $child_ != $head } {$head refresh}
   }
   foreach table $_myTables {
      if { $child_ != $table } {$table refresh}
   }
}

itcl::body FitsExtension::setFileName { fName_ } {
   foreach head $_myHeader {
      $head setFileName $fName_
   }   
   foreach table $_myTables {
      $table setFileName $fName_
   }
}

itcl::body FitsExtension::closeCmd { } {
   foreach head $_myHeader {
      $head closeCmd
   }
   foreach table $_myTables {
      $table closeCmd
   }
}

itcl::body FitsExtension::_oneDimImages {} {
## assume image data is int or real (if not, program will bomb)
    global powWCS
    global powFitsHeader powFitsHeaderCnt  powPlotParam
    global xCount yCount
    global powWCSList powWCSLabel powWCSName
    global useWCSInfo

    # create image name from file name
    regsub -all { } [urlTail $fileName] _ cleanFileName 
    set graphName ${cleanFileName}_[expr $_currentHDU-1]

    set curveName "c0_$graphName"

    # not sure if these keywords are used
    set powWCS($curveName) [$fFile getWcs]
    set powWCSName($curveName) 0
    set powWCSName(${curveName}scope) 0
    set useWCSInfo($curveName) $fvPref::ifWCSInfo
    set useWCSInfo(${curveName}scope) $fvPref::ifWCSInfo

    if { $fvPref::ifWCSInfo } {
       set result [$fFile getHeader2String]
       set cntList($curveName) [$fFile getHeaderKeyWord [lindex $result 0] $imgIndex]
       set powFitsHeaderCnt($curveName) [lindex $cntList($curveName) 1]
       if { [lindex $cntList($curveName) 0] > 0 } {
          #set powFitsHeader($curveName) [$fFile assembleWcsHeader $curveName]
          set powFitsHeader($curveName) [lindex $result 0]
          set powWCSInfo($curveName,DEFAULT) $powWCS($curveName)
          set wcsinfo $powWCS($curveName)

          set x_label [lindex [lindex $wcsinfo 3] 0]
          set y_label [lindex [lindex $wcsinfo 3] 1]
          if { $y_label == "" } { set y_label "Data" }
          set x_unit "" 
          set y_unit "intensity"
       } else {
          set powFitsHeader($curveName) [$fFile assembleWcsHeader $curveName NOWCS]
          set x_label "Pixel"
          set y_label "Data"
          set x_unit ""
          set y_unit "intensity"
       }
    } else {
       set powWCS($curveName) ""
       set powFitsHeader($curveName) ""
       set powFitsHeaderCnt($curveName) 0
       set x_label "Pixel"
       set y_label "Data"
       set x_unit ""
       set y_unit "intensity"
    }

    # get row # info
#    set dataInfoForPOW [$fFile loadExpr "#ROW" NULL ""]
#    set dataPtr     [lindex $dataInfoForPOW 0]
#    set dataType    [lindex $dataInfoForPOW 1]
#    set numElements [lindex $dataInfoForPOW 2]

    set numElements [$fFile getImgInfo]
#    powCreateData "rowsData" $dataPtr $dataType $numElements 1

    set dataInfoForPOW [$fFile loadImageMeanRows 1 $numElements 1]
    set dataPtr     [lindex $dataInfoForPOW 0]
    set dataType    [lindex $dataInfoForPOW 1]
    set numElements [lindex $dataInfoForPOW 2]

    powCreateData "colsData" $dataPtr $dataType $numElements 1

# create list from 1 to numElements
    set listRows {}
    for { set i 1 } { $i <= $numElements } { incr i } {
	lappend listRows $i
    }

    powCreateDataFromList "rowsData" $listRows

    powCreateVector "rowsVector" "rowsData" 0 NULL NULL
    powCreateVector "colsVector" "colsData" 0 NULL NULL

    set powWCSList($curveName) {}
    set powWCSList(${curveName}scope) {}

    powCreateCurve $curveName rowsVector NULL colsVector NULL

    if [info exists powWCSList($curveName)] {
       foreach name [lindex $powWCSList($curveName) 1] {
          $fFile assembleWcsLabel $curveName $name
       }
    } else {
       set powWCSList($curveName) {}
       lappend powWCSList($curveName) 1
       lappend powWCSList($curveName) {}
    }

    set powWCSList(${curveName}scope) $powWCSList($curveName)

    set curves [list $curveName]

    if { [winfo exist .pow.pow]!=1 } { 
	powInit .dummy
    }

    set powPlotParam(graphType,$graphName) [string tolower [lindex [$fFile getTableInfo hdutype] 0]]
    set powPlotParam(graphType,${graphName}scope) $powPlotParam(graphType,$graphName)
    set powPlotParam(zoomed,$graphName) 0
    set powPlotParam(zoomed,${graphName}scope) 0

    set xCount(${graphName}) 0
    set yCount(${graphName}) 0
    set xCount(${graphName}scope) 0
    set yCount(${graphName}scope) 0

    set powWCSList($graphName) {}
    set powWCSList(${graphName}scope) {}

    powCreateGraph $graphName $curves NULL $x_unit $y_unit $x_label $y_label \
	    [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1]
    set powWCSList($graphName) $powWCSList($curveName)
    set powWCSList(${graphName}scope) $powWCSList($curveName)
    powSetCurveOptions $graphName $curves pDisp No lDisp Yes
}
