########################################################################
#
#   class:  RegionList
#
# purpose:  Manage a collection of regions and allow drawing of new
#           regions on a graph.
#         
#   usage:  To create a region list:
#                set rgnLst [gRegionList graph canvas]
#           The region list will overlay a polygon on the graph which
#           will capture right-mouse clicks which will create a new
#           region.  Use "setDefault" to assign the default sign and
#           shape to use for new user-created objects.
#
#           To create regions through code, use "addRegion".
#
#           To access information about the region list, use "count"
#           (number of regions in list), "selected" (index of selected
#           region), "rgnAtIndex" (an individual region in list), and
#           "regions" (the full list of regions).
#
#           To delete a region, use "deleteRegion" with the index of
#           the region to be deleted.
#
#           To disallow drawing of additional regions by the user,
#           use "setIsDrawable" with a 1 or 0 value.
#
#           To allow only a single region to exist at a time, use
#           "setAllowsMultiple" with a value of 0.
#
#           To learn when the region list changes, make yourself the
#           owner of the list using "setOwner", passing the function
#           name or object and method to be called after a change.
#           The function or method should accept 2 arguments: obj and
#           msg.  The former is a reference object (either the region
#           list object or a region object) and the latter is a one-
#           word string describing the change. See the "notify" method
#           for the messages passed to the owner.
#
########################################################################

itcl::class RegionList {
   constructor { graph canvas } {}
   destructor {}

   public {
      method selected {}       { return $selected                }
      method count {}          { return [llength $itsRegions]    }
      method rgnAtIndex { i }  { return [lindex  $itsRegions $i] }
      method indexOfRgn { r }  { return [lsearch $itsRegions \
                                             [namespace tail $r]]}
      method regions {  }      { return $itsRegions              }
      method filename { }      { return $filename                }
      method bfilename { }     { return $bfilename               }

      method determineIfToDraw { x y flag }
      method createNewRegion { x y flag }
      method regionProperty  { }
      method dragNewRegion   { x y }
      method finishNewRegion {     }

      method changeShape { shape } { set defaultShape $shape }
      method setOutlineColor  { color } { set outlineColor $color }
      method setBoundaryColor { color } { set boundaryColor $color }
      method setHandleColor   { color } { set handleColor $color }
      method setLineWidth     { width } { set lineWidth $width }
      method setStaticFlag { axis } { set static_axis $axis }

      method addRegion    { sign shape descr fmt }
      method modifyRegion { sign shape descr fmt }
      method createRegion { sign shape descr fmt }

      method deleteAll    {   }
      method deleteRegion { i }
      method selectRegion { i }
      method drawAll { }

      method setOwner   {  owner     }  { set itsOwner $owner  }
      method getOwner   {            }  { return $itsOwner }
      method getObj     {            }  { return $this }
      method notify     { obj msg args }
      method setDefault { sign shape }

      method writeToFile    { fName fmt property }
      method readFromFile   { fName     }
      method readFromStr    { fileContents }
      method parseRegionStr { descr {defUnits "default"} }
      method buildRegionStr { rgn degFormat }

      method drawOverlay   {      }
      method setIsDrawable { flag }
      method setAllowsMultiple { flag }  { set allowsMultiple $flag }

      method activate   { }
      method deactivate { }

      method setCoordSys   { coordSys }
      method getCoordSys   {          }
      method getAllFormats {          }  { return $availFormats }
      method getPlainFormats {        }  { return $plainFormats }
      method flushBufferedRegions { buffer defUnits }
      method setValueFormat { format } { set valueFormat $format }
   }

   private {
      variable itsCanvas
      variable itsGraph
      variable itsOwner   ""
      variable itsRegions {}
      variable selected   -1
      variable isDrawable  1
      variable isActive    1
      variable allowsMultiple 1
      variable drawId     ""
      variable static_axis ""

      variable donotDrawDeletePortion "false"
      variable filename     ""
      variable bfilename    ""
      variable defaultSign  "+"
      variable defaultShape "Circle"
      variable defaultSys   "FK5 (J2000)"
      variable outlineColor  "blue"
      variable handleColor   "green"
      variable boundaryColor "red"
      variable lineWidth     1.0

      common availFormats [list \
            "Image (Pixels)" \
            "Physical (Pixels)" \
            "Linear" \
            "FK4 (B1950)" \
            "FK5 (J2000)" \
            "Galactic" \
            "Ecliptic" \
            "ICRS" \
            "Degrees (SAOtng)" ]
      common plainFormats [list \
            "image" \
            "physical" \
            "linear" \
            "fk4" \
            "fk5" \
            "galactic" \
            "ecliptic" \
            "icrs" \
            "saotng" ]
      common unitsFormats [list \
            "pixels" \
            "pixels" \
            "linear" \
            "degrees" \
            "degrees" \
            "degrees" \
            "degrees" \
            "degrees" \
            "degrees pixels" ]
 
      variable valueFormat "%.7g"

      method setupBindings { }
      method requestNewFormat { possibleFmts rgnDescr }
      method guessFormat { shape descr units fmt }

      method parseSize     { size      }
      method parsePosition { xPos yPos }
   }

}

########################################################################
#
#  gRegionList graph canvas
#
#  Create a RegionList object and attach it to "graph" on "canvas"
#
########################################################################

proc gRegionList { args } {
   return [uplevel #0 RegionList #auto $args]
}

########################################################################
#
#
#
########################################################################

itcl::body RegionList::constructor { graph canvas } {
   global powRegionListGlobal 
   global currentRegionObj

   set powRegionListGlobal [linsert $powRegionListGlobal 0 $this]
   set currentRegionObj $this

#puts "this: $this, powRegionListGlobal: $powRegionListGlobal"
   set itsGraph  $graph
   set itsCanvas $canvas

   catch { activate } err
   catch { setupBindings } err

   [gNotifications default] addObserver \
         $this notify $itsGraph graphHasFinishedDrawing
}

itcl::body RegionList::destructor {} {
   global powRegionListGlobal
   global currentRegionObj

   set idx [lsearch $powRegionListGlobal $this]

   if { $idx >= 0 } {
      set powRegionListGlobal [lreplace $powRegionListGlobal $idx $idx]
   }

   if { [llength $powRegionListGlobal] > 0 } {
      set currentRegionObj [lindex $powRegionListGlobal 0]
      # event delete <<RGN_Create>>
      powBindBtn <<RGN_Create>> "$itsCanvas bind DrawRegion" \
        "global currentRegionObj ; [itcl::code $currentRegionObj createNewRegion %x %y false] ; " \
	"[itcl::code $currentRegionObj dragNewRegion   %x %y] ; set_tracker_info %x %y $itsCanvas ; " \
        "[itcl::code $currentRegionObj finishNewRegion]"
	   
      powBindBtn <<RGN_CreateDelete>> "$itsCanvas bind DrawRegion" \
        "global currentRegionObj ; [itcl::code $currentRegionObj createNewRegion %x %y true] ; " \
	"[itcl::code $currentRegionObj dragNewRegion   %x %y] ; set_tracker_info %x %y $itsCanvas ; " \
        "[itcl::code $currentRegionObj finishNewRegion]"
   } else {
      set currentRegionObj ""
   }

#puts "pop top is currentRegionObj: $currentRegionObj"

   foreach rgn $itsRegions {
      catch { itcl::delete object $rgn }
   }
   if { [winfo exists $itsCanvas] } {
      catch { $itsCanvas delete $drawId }
   }

   [gNotifications default] removeObserver $this
}

itcl::body RegionList::setIsDrawable { flag } {
   if { $flag } {
      if { !$isDrawable } {
         set isDrawable 1
         drawOverlay
         foreach rgn [regions] {
            $rgn addTags DrawRegion
         }
      }
   } else {
      set isDrawable 0
      $itsCanvas delete $drawId
      foreach rgn [regions] {
         $rgn removeTags DrawRegion
      }
   }
}


itcl::body RegionList::drawOverlay { } {
   if { $isDrawable && $isActive && [$itsCanvas find withtag $drawId] == ""  } {
      foreach {x1 y1 x2 y2} [$itsCanvas coords ${itsGraph}box] {}
      set drawId [$itsCanvas create polygon $x1 $y1 $x1 $y2 $x2 $y2 $x2 $y1 \
            -fill {} -tags "$itsGraph DrawRegion"]
   }
   $itsCanvas raise $drawId $itsGraph
   $itsCanvas raise shape $itsGraph
}


itcl::body RegionList::setupBindings { } {
   global currgn currimg
   global powLutButton powROIButton
   global powLutButton_old powROIButton_old tcl_platform
   global currentRegionObj

   # set powLutButton_old $powLutButton
   # set powROIButton_old $powROIButton

   powBindBtn <<RGN_Create>> "$itsCanvas bind DrawRegion" \
        "global currentRegionObj ; [itcl::code $currentRegionObj createNewRegion %x %y false] ; " \
	"[itcl::code $currentRegionObj dragNewRegion   %x %y] ; set_tracker_info %x %y $itsCanvas ; " \
        "[itcl::code $currentRegionObj finishNewRegion]"
	   
   powBindBtn <<RGN_CreateDelete>> "$itsCanvas bind DrawRegion" \
        "global currentRegionObj ; [itcl::code $currentRegionObj createNewRegion %x %y true] ; " \
	"[itcl::code $currentRegionObj dragNewRegion   %x %y] ; set_tracker_info %x %y $itsCanvas ; " \
        "[itcl::code $currentRegionObj finishNewRegion]"
	   
   #powBindBtn <<DblBtnPress>> "$itsCanvas bind DrawRegion" \
   #                   "global currentRegionObj ; [itcl::code $currentRegionObj regionProperty] ; " \
   #                   {} \
   #                   {}
   powBindBtn <<LUT>> "$itsCanvas bind DrawRegion" \
                      "powSelectImage $currgn $currimg" \
                      "powBoundDiddleLut $currgn $currimg %x %y" \
                      {}
}

########################################################################
#
#  (de)activate: Make region (de)active following graph (de)selection
#
########################################################################

itcl::body RegionList::activate { } {
   drawOverlay
   foreach rgn [regions] {
      if { $isDrawable } {
         $rgn addTags [list DragAble DrawRegion]
      } else {
         $rgn addTags DragAble
      }
   }
   if { $selected!=-1 } {
      [rgnAtIndex $selected] select
   }
   set isActive 1
   catch { setupBindings } err
}

itcl::body RegionList::deactivate { } {
   $itsCanvas delete $drawId
   foreach rgn [regions] {
      $rgn removeTags [list DrawRegion DragAble]
   }
   if { $selected!=-1 } {
      [rgnAtIndex $selected] deselect
   }
   set isActive 0
}

########################################################################

itcl::body RegionList::setDefault { sign shape } {
   set defaultSign  $sign
   set defaultShape $shape
}

itcl::body RegionList::deleteAll { } {
   global currentRegionObj

   set allRgns $itsRegions
   set itsRegions {}
   set selected -1

   foreach rgn $allRgns {
      itcl::delete object $rgn
   }
   if { $itsOwner != "" } {
      $itsOwner $currentRegionObj regionsHaveChanged
   }
}


itcl::body RegionList::deleteRegion { i } {
   if { $i < 0 } return

   set rgn [lindex $itsRegions $i]
   itcl::delete object $rgn

   if { $i<$selected } {
      incr selected -1
   }
   selectRegion $selected
}

itcl::body RegionList::selectRegion { i } {
   set nRgns [count]
   if { !$nRgns } {
      set selected -1
      return
   }
   if { $i >= $nRgns } {
      set i [expr $nRgns-1]
   } elseif { $i < 0 } {
      set i 0
   }
   [lindex $itsRegions $i] select
}


itcl::body RegionList::drawAll { } {
   foreach rgn $itsRegions {
      $rgn draw
   }
   if { $selected != -1 } {
      [lindex $itsRegions $selected] select
   }
}


itcl::body RegionList::notify { obj msg args } {
   global currentRegionObj
   global CpowXRangeX0 CpowXRangeX1 CpowXRangeY0 CpowXRangeY1
   global TpowXRangeX0 TpowXRangeX1 TpowXRangeY0 TpowXRangeY1

   if { $itsOwner == "powXRangeOwner" } {
      set CpowXRangeX0 $TpowXRangeX0
      set CpowXRangeX1 $TpowXRangeX1
      set CpowXRangeY0 $TpowXRangeY0
      set CpowXRangeY1 $TpowXRangeY1
   } else {
      catch { unset CpowXRangeY0 } err
   }

   switch  $msg  {
      "graphHasFinishedDrawing" {
         drawOverlay
      }

      "shapeHasChanged" -
      "shapeIsBeingModified" {
         set idx [lsearch $itsRegions [namespace tail $obj]]
         if { $idx==-1 } return
         if { $itsOwner != "" } {
            $itsOwner $obj $msg
         }
      }
      "shapeHasDied" {
         set idx [lsearch $itsRegions [namespace tail $obj]]
         if { $idx==-1 } return
         set itsRegions [lreplace $itsRegions $idx $idx]
         if { $itsOwner != "" } {
            $itsOwner $currentRegionObj regionsHaveChanged
         }
      }
      "shapeIsSelected" {
         set idx [lsearch $itsRegions [namespace tail $obj]]
         if { $idx==-1 } return
         if { $selected != $idx } {
            if { $selected != -1 && $selected<[count] } {
               [lindex $itsRegions $selected] deselect
            }
            set selected $idx
            if { $itsOwner != "" } {
               $itsOwner $obj "selectionHasChanged"
            }
         }

      }
   }

}


itcl::body RegionList::modifyRegion { sign shape descr fmt } {
   if { $selected == -1 } {
      set rgn [addRegion $sign $shape $descr $fmt]
   } else {
      set rgn [lindex $itsRegions $selected]
      $rgn setSign $sign
      $rgn setShape $shape
      $rgn setFunction $fmt $descr
      $rgn finishModification
      $rgn setStaticFlag $static_axis
   }
}


itcl::body RegionList::addRegion { sign shape descr fmt } {
   global regionParam

   if { ! $allowsMultiple && [count] } {
      deleteAll
   }

   if { [info exists regionParam] && [info exists regionParam(format)] \
              && $regionParam(format) == "Physical (Pixels)" } {
      set tokenList [split $descr " "]
      set phy_x [lindex $tokenList 0]
      set phy_y [lindex $tokenList 1]
      set phy_radius [lindex $tokenList 2]
      set result [powConvertPhysical2Image $phy_x $phy_y]
      set img_x [lindex $result 0]
      set img_y [lindex $result 1]
      set img_radius [powConvertRadiusPhysical2Image $phy_x $phy_y $img_x $phy_radius]
      set descr [format "%s %s %s" $img_x $img_y $img_radius]
   }

   set rgn [createRegion $sign $shape $descr $fmt]
   lappend itsRegions $rgn
   $rgn finishModification
   return $rgn
}


itcl::body RegionList::createRegion { sign shape descr fmt } {
   global currentRegionObj
   global powRotation

   set rgn [gRegion $itsGraph $itsCanvas]

   if [info exists powRotation($itsGraph)] {
      catch {$rgn setRotation $powRotation($itsGraph)} err
   }
   catch {$rgn addTags  [list DrawRegion]} err
   catch {$rgn setOwner [itcl::code $currentRegionObj notify]} err
   catch {$rgn setSign  $sign} err
   catch {$rgn setShape $shape} err
   catch {$rgn setFunction $fmt $descr} err
   catch {$rgn setStaticFlag $static_axis} err
   catch {$rgn setOutlineColor $outlineColor} err
   catch {$rgn setBoundaryColor $boundaryColor } err
   catch {$rgn setHandleColor $handleColor } err
   catch {$rgn setLineWidth $lineWidth } err
   return $rgn
}


########################################################################
#
#   The following routines display region property
#
########################################################################

itcl::body RegionList::regionProperty { } {
   global regionParam

   set rgnIdx [$regionParam(rgns) selected]
   set rgn [$regionParam(rgns) rgnAtIndex $rgnIdx]

#puts "[$rgn getSign]"
#puts "$regionParam(currSign)$regionParam(currShape)$regionParam(currDescr)"
}

########################################################################
#
#   The following 3 routines handle dragging out a new region
#
########################################################################

itcl::body RegionList::createNewRegion { x y flag } {
   global currentRegionObj
   global insideExistGraph

   set donotDrawDeletePortion false
   if { $flag == "true" } {
      if ![info exists insideExistGraph] {
         set donotDrawDeletePortion true
         return
      } else {
         if { $insideExistGraph == "false" } {
            set donotDrawDeletePortion true
            return
         }
      }
   }

   set x [$itsCanvas canvasx $x]
   set y [$itsCanvas canvasy $y]

   if { ! $allowsMultiple && [count] } {
      set rgn [lindex $itsRegions end]
   } else {
      set rgn [gRegion $itsGraph $itsCanvas]
      $rgn addTags   [list DrawRegion]
      $rgn setOwner  [itcl::code $currentRegionObj notify]
      lappend itsRegions $rgn
   }

   $rgn setDrawDeleteFlag $flag
   $rgn setSign   $defaultSign
   $rgn setShape  $defaultShape
   $rgn setOutlineColor $outlineColor
   $rgn setBoundaryColor $boundaryColor
   $rgn setHandleColor $handleColor
   $rgn setLineWidth   $lineWidth
   $rgn setStaticFlag $static_axis
   $rgn setCoords [list $x $y $x $y]
   $rgn beginModification
}

itcl::body RegionList::dragNewRegion { x y } {
   if { $donotDrawDeletePortion == "true" } return
   [lindex $itsRegions end] adjustPt 1 $x $y
}

itcl::body RegionList::finishNewRegion { } {
   if { $donotDrawDeletePortion == "true" } return
   [lindex $itsRegions end] finishModification
   if { ! $allowsMultiple } {
      while { [count]>1 } {
         deleteRegion 0
      }
   }
}


########################################################################
#
#   These two regions Read/Write regions from the given file
#
########################################################################

itcl::body RegionList::writeToFile { fName degFmt property } {
   global powRotation
   if { $property == "Source" } {
      set filename $fName
   } else {
      set bfilename $fName
   }
   set writeFile $fName

   set base [string tolower [lindex $defaultSys 0]]

   set freg [open $writeFile w+]
   puts $freg "# Region created by POW [clock format [clock seconds]], $property"
   puts $freg "# filename: $itsGraph"
   if { $base=="degrees" } {
      #  Writing SAOtng format
      if { [string tolower $degFmt] == "hhmmss" } {
         puts $freg "# format: hms"
      } else {
         puts $freg "# format: degrees"
      }
   } elseif { $base=="pixels" } {
      #  Writing SAOtng format
      puts $freg "# format: pixels"
   } else {
      #  Writing DS9 format
      puts $freg $base
   }
   for {set i 0} {$i<[count]} {incr i} {
      set rgn   [rgnAtIndex $i]
      set propertyOrder [$rgn getPropertyOrder]
      if { $propertyOrder == $property } {
         foreach [list sign shape descr] [buildRegionStr $rgn $degFmt] {}
         set rot [lindex $descr end]
         if { [string first "(pixels)" [string tolower $defaultSys]] > 0 && \
              [info exists itsGraph] && [info exists powRotation($itsGraph)] } {
           if { ([string tolower $shape] == "box" || [string tolower $shape] == "ellipse") && [llength $descr] >= 5 } {
              set rot [expr $rot + $powRotation($itsGraph)]
              set descr [lreplace $descr end end $rot]
           }
         }

         set descr [join $descr ", "]
         puts $freg "$sign[string tolower $shape]($descr)"
      }
   }
   close $freg
}


itcl::body RegionList::readFromFile { fName } {
   global readRegionFile

   set readRegionFile true
   #  Read contents of file

   set filename $fName
   set freg [open $filename r]
   set fileContents [read $freg]
   close $freg
   readFromStr $fileContents
   set readRegionFile false
}
    
itcl::body RegionList::readFromStr { fileContents } {
   global regionParam

   #  Identify what formats are possible for this graph

   if { [powWCSexists $itsGraph] } {

      set fmt [list image fk5 saotng]
      set defaultUnits "unknown"

   } else {

      set wcsObj $::powPlotParam(currimg,$itsGraph)
      if { $wcsObj=="NULL" } {
         set wcsObj [lindex $::powPlotParam(curves,$itsGraph) 0]
      }
      foreach [list x y] [powPixelToGraph $wcsObj 3 5] {}
      set diff1 [expr abs($x-4) + abs($y-6)]

      #  Do it again for a different point in case we just happened
      #  to hit an intersection of pixels and scale with first point

      foreach [list x y] [powPixelToGraph $wcsObj 4 6] {}
      set diff2 [expr abs($x-5) + abs($y-7)]

      if { $diff1 > 0.0001 || $diff2 > 0.0001 } {
         #  Image scale is not the identity
         set fmt [list image linear]
         set defaultUnits "unknown"
      } else {
         #  Only pixel values allowed
         set fmt [list image]
         set defaultUnits "pixels"
      }

   }

   set bufferedRegions {}

   set newregions [split $fileContents "\n\r|;"]
   foreach reg $newregions {
      if { [string index $reg 0] == "#" } {

         # Handle leading comments... property (source/background) and format are supported here

         set reg [string tolower $reg]
         set tokenList [split $reg " "]
         set property [lindex $tokenList end]

         if { [string first "format:" $reg]!=-1 } {
            if { [string first "degrees" $reg]!=-1 } {
               set fmt "saotng"
            } elseif { [string first "pixels" $reg]!=-1 } {
               set fmt "image"
            } elseif { [string first "hhmmss" $reg]!=-1 } {
               set fmt "saotng"
            } elseif { [string first "hms" $reg]!=-1 } {
               set fmt "saotng"
            }
            if { [llength $fmt]==1 } {
               set idx [lsearch $plainFormats $fmt]
               set defaultSys   [lindex $availFormats $idx]
               set defaultUnits [lindex $unitsFormats $idx]
            }
         }

      } elseif { [string length $reg] != 0 } {

         set idx [string first # $reg]
         if { $idx != -1 } {
            incr idx -1
            set reg [string range $reg 0 $idx]
         }
         set parts [split $reg {;}]

         foreach part $parts {
            set part [string tolower $part]
            set idx [lsearch $plainFormats $part]
            if { $idx != -1 } {

               # Is a format specifier

               set fmt $part
               set regionParam(format) [lindex $availFormats $idx]
               set defaultUnits [lindex $unitsFormats $idx]
               set defaultSys   [lindex $availFormats $idx]

            } else {

               if { [string first = $part]!=-1 } continue
               if { [catch {\
                     set newDescr [parseRegionStr $part $defaultUnits]\
                  } errMsg] } {
                  error "Could not decipher $part\n$errMsg"
               }
               lappend bufferedRegions $newDescr

               if { [llength $fmt]!=1 } {

                  #  Format not found yet... try to guess format

                  foreach {sign shape descr units} $newDescr {}
                  set fmt [guessFormat $shape $descr $units $fmt]
                  if { [llength $fmt]==1 } {
                     set idx          [lsearch $plainFormats $fmt]
                     set defaultSys   [lindex $availFormats $idx]
                     set defaultUnits [lindex $unitsFormats $idx]

                  }

               }
               if { [llength $fmt]==1 } {
                  flushBufferedRegions $bufferedRegions $defaultUnits
                  set bufferedRegions {}
               }

            }
         }

      }
   }

   #
   #  If we never identified the file format, ask user for help
   #

   if { [llength $fmt]!=1 && [llength $bufferedRegions] } {
      set newFmt [requestNewFormat $fmt $fileContents]
      if { $newFmt==-1 } return
      set fmt [lindex $plainFormats $newFmt]
      set defaultSys [lindex $availFormats $newFmt]
      set defaultUnits [lindex $unitsFormats $newFmt]
   }

   #  Flush any remaining buffered regions

   flushBufferedRegions $bufferedRegions $defaultUnits
}


itcl::body RegionList::flushBufferedRegions { buffer defUnits } {
   foreach d $buffer {
      foreach [list aSign aShape aDescr aUnits] $d {}
      #  Need to apply default units to this region
      if { [lindex $aUnits 0]=="unknown" } {
         set aUnits [lreplace $aUnits 0 0 \
               [lindex $defUnits 0]]
      }
      if { [lindex $aUnits 1]=="unknown" } {
         set aUnits [lreplace $aUnits 1 1 \
               [lindex $defUnits end]]
      }
      if { $itsOwner == "powRegionOwner" } {
         if { $aSign == "+" } {
            set outlineColor blue
            set handleColor green
         } elseif { $aSign == "-" } {
            set outlineColor red
            set handleColor yellow
         }
         powRegionResetPanelColor $outlineColor $handleColor
      }
      addRegion $aSign $aShape $aDescr $aUnits
   }
}


itcl::body RegionList::guessFormat { shape descr units fmts } {

   set posUnit [lindex $units 0]
   set sizUnit [lindex $units end]

   if { $posUnit=="unknown" || $sizUnit=="unknown" } {

      if { $posUnit=="unknown" } {
         if { $shape=="Line" || $shape=="Polygon" } {
            set posArgs $descr
         } else {
            set posArgs [lrange $descr 0 1]
         }
         foreach [list x y] $posArgs {
            if { $y<-90.0 || $y>90.0 || $x<-360.0 || $x>360.0 } {
               #  Coords should be in degrees, but x/y out of range.
               #  Parameters must be in pixel coordinates.
               set fmts [removeElements $fmts [list saotng fk5]]
            }
         }
      }

      if { $sizUnit=="unknown" } {
         switch $shape {
            "Circle" {
               set rad [lindex $descr 2]
               if { $rad > 90 } {
                  #  Radius cannot be this big and be in degrees
                  set fmts [removeElements $fmts [list fk5]]
               }
            }
            "Ellipse" -
            "Box" {
               set dx [expr abs([lindex $descr 2])]
               set dy [expr abs([lindex $descr 3])]
               if { $dx > 90 || $dy > 90 } {
                  #  Sizes cannot be this big and be in degrees
                  set fmts [removeElements $fmts [list fk5]]
               }
            }
         }
      }

   } elseif { $posUnit=="degrees" && $sizUnit=="degrees" } {
      set fmts "fk5"
   } elseif { $posUnit=="degrees" && $sizUnit=="pixels" } {
      set fmts "saotng"
   }

   return $fmts
}

itcl::body RegionList::requestNewFormat { possibleFmts rgnDescr } {
   global powDWP

   set w ${powDWP}rgnFmt

   ::iwidgets::dialogshell $w -title "Region Format" -modality application \
         -background $::powbg

   $w add OK     -text "OK"     -command "$w deactivate OK"
   $w add Cancel -text "Cancel" -command "$w deactivate Cancel"
   $w default OK

   set wc [$w childsite]

   label $wc.msg -bg $::powbg -justify left -wraplength 300 -text \
         "Region file lacks explicit degree/pixel designation.\
         Please select the default format in which to interpret\
         the following regions:"

   ::iwidgets::scrolledtext $wc.rgn -visibleitems 40x4 -background $::powbg \
         -wrap none -textbackground white
   $wc.rgn insert end $rgnDescr
   $wc.rgn configure -state disabled

   set allFmts [list image fk5 saotng linear]
   set allFmtStrs [list \
         "Pixel Positions and Sizes" \
         "Degree Positions and Sizes" \
         "Degree Positions, Pixel Sizes" \
         "Linearly Scaled Positions and Sizes" ]

   ::iwidgets::optionmenu $wc.fmt -labeltext "Format:" -labelpos w \
         -background $::powbg
   eval $wc.fmt insert 0 $allFmtStrs

   if { [llength $possibleFmts]==0 } {
      set possibleFmts $allFmts
   }
   set idx 0
   foreach fmt $allFmts {
      if { [lsearch $possibleFmts $fmt]==-1 } {
         $wc.fmt disable $idx
      }
      incr idx
   }
   $wc.fmt select 0
   
   pack $wc.msg -pady  5
   pack $wc.rgn -pady 10 -expand 1 -fill both
   pack $wc.fmt -pady  5

   $w center
   set btn [$w activate]
   set ans [$wc.fmt index select]
   itcl::delete object $w

   if { $btn == "Cancel" } {
      set idx -1
   } else {
      set idx [lsearch $plainFormats [lindex $allFmts $ans]]
   }
   return $idx
}


itcl::body RegionList::setCoordSys { coordSys } {
   set idx [lsearch $availFormats $coordSys]
   if { $idx != -1 } {
      set defaultSys $coordSys
   } else {
      set idx [lsearch $plainFormats $coordSys]
      if { $idx != -1 } {
         set defaultSys [lindex $availFormats $idx]
      } else {
         error "Unrecognized coordinate system: $coordSys"
      }
   }
}


itcl::body RegionList::getCoordSys { } {
   return $defaultSys
}


########################################################################
#
#   Take a string representing a region and parse it into
#         sign shape params units
#
########################################################################

itcl::body RegionList::parseRegionStr { descr {defUnits "default"} } {
   global powRotation
   global regionParam

   set sign  $defaultSign
   set shape $defaultShape

   if { $defUnits=="default" } {
      set idx [lsearch $availFormats $defaultSys]
      set defUnits [lindex $unitsFormats $idx]
   }
   set defPosUnits [lindex $defUnits 0]
   set defSizUnits [lindex $defUnits end]

   #  Split description up into its various parameters

   set items {}
   foreach item [split $descr "(), "] {
      if { $item != "" && $item != "\t" } {
         lappend items $item
      }
   }
   set idx 0

   #  Look for shape/sign in first 1 or 2 elements

   set item         [lindex $items $idx]
   set firstChar    [string index $item 0]
   set firstIsAlpha [string is alpha $firstChar]
   set firstIsSign  [string first $firstChar {+-!}]

   #  After this block, idx should point to first numerical value

   if { $firstIsAlpha } {

      set sign  "+"
      set shape $item
      incr idx

   } elseif { $firstIsSign!=-1 } {

      if { [string length $item]==1 } {

         #  Standalone sign; next item must be the shape

         incr idx
         set sign $item
         set shape [lindex $items $idx]
         incr idx

      } else {

         # Is sign part of shape or first value?

         set tail [string range $item 1 end]
         if { [string is alpha $tail] } {
            # It is a shape; set sign and shape
            set sign  $firstChar
            set shape $tail
            incr idx
         }

      }
      if { $sign=="-" || $sign=="!" } {
         set sign "-"
      } else {
         set sign "+"
      }

   }

   set shape [string totitle $shape]
   set descr [lrange $items $idx end]

   #  Make sure there are the correct number of elements in descr

   set nelem [llength $descr]
   switch $shape {
      Box     {set nparam  5}
      Ellipse {set nparam  5}
      Circle  {set nparam  3}
      Polygon {set nparam  [expr 2*int($nelem/2)]}
      Line    {set nparam  4}
      Point   {set nparam  2}
      default {error "Unrecognized shape: $shape" }
   }
   if {$nelem!=$nparam} {
      error "Wrong number of parameters for $shape!"
   }

   #  Parse any unit formatting of parameters and identify units.
   #  Raise error if a degree format is used for a nonWCS graph

   if { [powWCSexists $itsGraph] } {
      set WCS 1
   } else {
      set WCS 0
   }

   set newDescr {}
   set allUnits ""
   if { $shape=="Line" || $shape=="Polygon" || $shape=="Point" } {

      #  These objects consist of just pairs of coordinates

      foreach [list x y] $descr {
         foreach [list xVal yVal unts] [parsePosition $x $y] {}
         if { $unts=="unknown" } {
            set unts $defPosUnits
         }
         if { !$WCS && $unts=="degrees" } {
            error "Region coded in degrees, but graph lacks WCS information."
         } elseif { $WCS && $unts=="linear" } {
            error "Graph coded in degrees,\
                  but region specified in linear coordinates"
         }
         if { $allUnits=="" } {
            set allUnits $unts
         } elseif { $unts != $allUnits } {
            error "All position parameters do not use the same units"
         }
         lappend newDescr $xVal $yVal

      }

   } else {

      #  Remaining objects consist of center, sizes, and rotations

      #  Parse the center position

      foreach [list x y] [lrange $descr 0 1] {
         foreach [list xVal yVal unts] [parsePosition $x $y] {}
         if { $unts=="unknown" } {
            set unts $defPosUnits
         }
         if { !$WCS && $unts=="degrees" } {
            error "Region coded in degrees, but graph lacks WCS information."
         } elseif { $WCS && $unts=="linear" } {
            error "Graph coded in degrees,\
                  but region specified in linear coordinates"
         }
         set allUnits $unts
         lappend newDescr $xVal $yVal
      }

      #  Parse sizes/rotations

      if { $shape=="Circle" } {

         set radius [lindex $descr 2]
         foreach [list radius unts] [parseSize $radius] {}
         if { $unts=="unknown" } {
            set unts $defSizUnits
         }
         if { !$WCS && $unts=="degrees" } {
            error "Region coded in degrees, but graph lacks WCS information."
         } elseif { $WCS && $unts=="linear" } {
            error "Graph coded in degrees,\
                  but region specified in linear coordinates"
         }
         lappend newDescr $radius
         lappend allUnits $unts

      } else {

         set width  [lindex $descr 2]
         set height [lindex $descr 3]
         set rot    [lindex $descr 4]

         if { [string first "(pixels)" [string tolower $defaultSys]] > 0 && \
              [info exists itsGraph] && [info exists powRotation($itsGraph)] } {
            if { ([string tolower $shape] == "box" || [string tolower $shape] == "ellipse") && [llength $descr] >= 5 } {
               set rot [expr $rot - $powRotation($itsGraph)]
            }
         }        

         foreach [list newWidth  wUnits] [parseSize $width ] {}
         foreach [list newHeight hUnits] [parseSize $height] {}
         if { $wUnits != $hUnits } {
            error "Size arguments have mixed formatting: $width $height"
         }
         if { $wUnits=="unknown" } {
            set wUnits $defSizUnits
         }

         if { !$WCS && $wUnits=="degrees" } {
            error "Region coded in degrees, but graph lacks WCS information."
         } elseif { $WCS && $wUnits=="linear" } {
            error "Graph coded in degrees,\
                  but region specified in linear coordinates"
         }
         lappend newDescr $newWidth $newHeight
         lappend allUnits $wUnits

         foreach [list rot unts] [parseSize $rot] {}
         if { $unts=="unknown" } { set unts "degrees" }
         if { $unts != "degrees" } {
            error "Rotation coded in something other than degrees: $rot"
         }
         lappend newDescr $rot

      }

   }

   if { [lsearch $allUnits "linear"]!=-1 \
         && [lsearch $allUnits "degrees"]!=-1 } {
      error "Cannot mix linear and degree formats"
   }

   if { [info exists regionParam(format)] && $regionParam(format) == "Physical (Pixels)" } {
      return [list $sign $shape $descr $allUnits]
   }
   return [list $sign $shape $newDescr $allUnits]
}


itcl::body RegionList::buildRegionStr { rgn degFmt } {
   global currgn currimg

   set units [lindex $unitsFormats [lsearch $availFormats $defaultSys]]
   set posUnits [lindex $units 0]
   set sizUnits [lindex $units end]

   set descr [$rgn getFunction [list $posUnits $sizUnits]]
   set shape [$rgn getShape]
   set sign  [$rgn getSign]

   set newD {}
   if { $shape=="Line" || $shape=="Polygon" || $shape=="Point" } {

      #  These objects consist of just pairs of coordinates

      foreach [list x y] $descr {

         if { $posUnits=="degrees" && $degFmt=="hhmmss" } {
            lappend newD [powHourRA $x "%d:%02d:%05.2f"] [powDegDec $y]
         } else {
            lappend newD [format $valueFormat $x] [format $valueFormat $y]
         }
      }

   } else {

      #  Remaining objects consist of center, sizes, and rotations

      #  Parse the center position

      foreach [list x y] [lrange $descr 0 1] {}
      #set coord [powGraphToPixel $currimg $x $y]
      #set icoord [powPixelToGraph $currimg [lindex $coord 0] [lindex $coord 1]]
      #set x [lindex $icoord 0]
      #set y [lindex $icoord 1]
     
      if { $posUnits=="degrees" && $degFmt=="hhmmss" } {
         lappend newD [powHourRA $x "%d:%02d:%05.2f"] [powDegDec $y]
      } else {
         lappend newD [format $valueFormat $x] [format $valueFormat $y]
      }

      #  Parse remaining size parameters (given in image pixel coords)

      if { $shape=="Circle" } {

         set radius [lindex $descr 2]
         if { $sizUnits=="degrees" } {
            if { [expr abs($radius)]<1.0 } {
               set radius [expr $radius*60.0]
               if { [expr abs($radius)]<1.0 } {
                  set radius [expr $radius*60.0]
                  lappend newD "[format $valueFormat $radius]\""
               } else {
                  lappend newD "[format $valueFormat $radius]'"
               }
            } else {
               lappend newD "[format $valueFormat $radius]d"
            }
         } else {
            lappend newD [format $valueFormat $radius]
         }

      } else {

         set width  [lindex $descr 2]
         set height [lindex $descr 3]
         set rot    [lindex $descr 4]

         if { $sizUnits=="degrees" } {
            foreach p [list $width $height] {
               if { [expr abs($p)]<1.0 } {
                  set p [expr $p*60.0]
                  if { [expr abs($p)]<1.0 } {
                     set p [expr $p*60.0]
                     lappend newD "[format $valueFormat $p]\""
                  } else {
                     lappend newD "[format $valueFormat $p]'"
                  }
               } else {
                  lappend newD "[format $valueFormat $p]d"
               }
            }
         } else {
            lappend newD [format $valueFormat $width] \
                  [format $valueFormat $height]
         }
         lappend newD [format $valueFormat $rot]

     }
   }
   set descr $newD


   return [list $sign $shape $descr]
}




itcl::body RegionList::parseSize { size } {

   set lastChar [string index $size end]
   if { [string first $lastChar {drpi'"}]!=-1 } {
      # in [num]x format

      set size [string range $size 0 end-1]
      set foundUnits "degrees"
      if { $lastChar=="d" } {
         set val $size
      } elseif { $lastChar=="r" } {
         set val [expr $size * 180.0 / 3.1415926535]
      } elseif { $lastChar=="'" } {
         set val [expr $size / 60.0 ]
      } elseif { $lastChar=="\"" } {
         set val [expr $size / 3600.0 ]
      } else {
         set val $size
         set foundUnits "pixels"
      }

   } else {
      # Use default

      set val        $size
      set foundUnits "unknown"

   }

   return [list $val $foundUnits]
}

itcl::body RegionList::parsePosition { xPos yPos } {

   foreach axis [list x y] {
      set pos [subst \$${axis}Pos]

      #####
      #  Strip off and record the sign of the position
      #####

      set sign 1.0
      if { [string index $pos 0] == "-" } {
         set sign -1.0
         set pos [string range $pos 1 end]
      }

      #####
      #  Parse position
      #####

      set lastChar [string index $pos end]
      if { [string first : $pos]!=-1 || $lastChar=="s" } {

         set parts [split [string trimright $pos s] {dhms:}]
         if { [llength $parts]!=3 } {
            error "Bad format for position: $pos"
         }
         foreach [list d m s] $parts {}
         set d [string trimleft $d 0]; if { $d=="" } { set d 0 }
         set m [string trimleft $m 0]; if { $m=="" } { set m 0 }
         set s [string trimleft $s 0]; if { $s=="" } { set s 0 }
         set val [expr $d + $m/60.0 + $s/3600.0]
         if { $axis=="x" } {
            # RA needs to be scaled to proper degrees
            set val [expr $val * 15.0]
         }

         set foundUnits "degrees"
      
      } elseif { [string first $lastChar "drpi"]!=-1 } {
         # in [num]x format

         set pos [string range $pos 0 end-1]
         if { $lastChar=="d" } {
            set val $pos
            set foundUnits "degrees"
         } elseif { $lastChar=="r" } {
            set val [expr $pos * 180.0 / 3.1415926535]
            set foundUnits "degrees"
         } else {
            set val $pos
            set foundUnits "pixels"
         }
         
      } else {
         # Use default

         set val $pos
         set foundUnits "unknown"
         
      }

      set ${axis}Val [expr $sign * $val]
      set ${axis}Unt $foundUnits
   }

   if { $xUnt != $yUnt } {
      error "Size arguments have mixed formatting: $xPos $yPos"
   }

   return [list $xVal $yVal $xUnt]
}

proc removeElements { mainList removeList } {
   foreach l $removeList {
      set idx [lsearch $mainList $l]
      if { $idx!=-1 } {
         set mainList [lreplace $mainList $idx $idx]
      }
   }
   return $mainList
}
