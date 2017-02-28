########################################################################
#
#   class:  Shape
#
# purpose:  Draw a geometric shape on the canvas and allow the user
#           to drag or reshape it.
#         
#   usage:  To create a shape on a canvas:
#               gShape canvas
#           To set its options use "setShape", "setCoords", "setColor",
#           and "setRotation", or the convience method "setFullShape".
#
#           To have the shape use additional tags to which you can bind,
#           use "addTags".
#
#           Messages will be passed to the shape's owner (via a callback)
#           when the shape changes.  Become the shape's owner with the
#           "setOwner" method.  Messages currently passed are:
#                  shapeIsBeingModified (when user changes shape/coord/rot)
#                  shapeHasChanged      (when a series of changes ends)
#                  shapeIsSelected      (when a shape becomes selected)
#
########################################################################

itcl::class Shape {
   constructor { canvas } {}
   destructor {}

   public {
      method draw     {}
      method select   {}
      method deselect {}
      method clippedPolygon {}

      method beginModification  { {x 0} {y 0} }
      method finishModification {}

      method setClip      { x1 y1 x2 y2 }
      method setScale     { mx my       }
      method setFullShape { shp crds rot}
      method setCoords    { coords      }
      method setShape     { shape       }
      method setRotation  { rot         }

      method setColor     { clr         } { set itsColor $clr    }
      method setBoundaryClr  { clr      } { set itsBoundaryColor $clr    }
      method setHandleClr { clr         } { set itsHandleColor $clr    }
      method setLineW     { width       } { set itsLineWidth $width }

      method getTag      {} { return $itsTag     }
      method getCanvas    {} { return $itsCanvas     }
      method getColor     {} { return $itsColor     }
      method getBoundaryClr  {} { return $itsBoundaryColor     }
      method getHandleClr {} { return $itsHandleColor     }
      method getLineW     {} { return $itsLineWidth }

      method setOwner     { owner       } { set itsOwner $owner  }
      method getItsOwner  {} { return $itsOwner  }

      method getShape     {             } { return $itsShape     }
      method getCoords    {             } { return $itsParams    }
      method getRotation  {             } { return $itsRotation  }
      method setStatic    { axis } { if { $axis != "" } { set ${axis}StaticFlag "true" } }
      method addBoundaryLine { coords }
      method setDrawDeleteFlag { flag } { set drawDeleteRegionFlag $flag }

      method insertPt  { index      }
      method adjustPt  { index x  y }
      method rotatePt  {       x  y }
      method shift     {      dx dy }
      method drag      {       x  y }
      method setStartPoint {   x  y }

      method getPolygon  { }
      method drawHandles { }

      method addTags    { tags }
      method removeTags { tags }

      method notifyOwner { args }

      method enterLeaveShape { mode }
   }


   protected {
      variable itsParams   {}
      variable itsRotation 0.0
      variable itsShape    "Circle"
      variable itsCanvas
      variable bumpDetect  "false"
      variable drawDeleteRegionFlag "false"
      variable reverseFlag "false"

      variable xScale 1.0
      variable yScale 1.0

      variable itsBoundTag ""
      variable itsTag    ""
      variable allTags   ""
      variable itsOwner  ""
      variable itsColor         "black"
      variable itsBoundaryColor "red"
      variable itsHandleColor   "green"
      variable itsLineWidth     1.0
      variable itsIds    {}

      variable isSelected 0
      variable isBeingModified 0
      variable itsRotationOffset

      variable clipRect {}
      variable ignoreClip 0
      variable startX 0.0
      variable startY 0.0
      variable tmpIds {}

      variable yStaticFlag "false"

      method CircleToPoly  { x0 y0 dx dy }
      method EllipseToPoly { x0 y0 dx dy angle }
      method BoxToPoly     { x0 y0 dx dy angle }
      method PolyToPoly    { descr }
      method PointToPoly   { x0 y0 }
   }

   private {
      variable tmpAdjustPoint -1
      method checkPolygonPoint { index }
   }

}

########################################################################
#
#  gShape canvas
#
########################################################################

proc gShape { args } {
   return [uplevel #0 Shape #auto $args]
}

########################################################################
#
#
#
########################################################################

itcl::body Shape::constructor { canvas } {
   set itsCanvas $canvas

   set itsTag shp[namespace tail $this]
   $itsCanvas bind "$itsTag && DragAble" <Enter> \
         [itcl::code $this enterLeaveShape Enter]
   $itsCanvas bind "$itsTag && DragAble" <Leave> \
         [itcl::code $this enterLeaveShape Leave]

   $itsCanvas bind rgnHandle$itsTag <Enter> \
         "$itsCanvas configure -cursor sizing"
   $itsCanvas bind rgnHandle$itsTag <Leave> \
         "$itsCanvas configure -cursor \$powcursor"

   powBindBtn <<RGN_Drag>> "$itsCanvas bind \"$itsTag && DragAble\"" \
         [itcl::code $this beginModification %x %y] \
         [itcl::code $this drag %x %y] \
         [itcl::code $this finishModification]

   set allTags [list $itsTag shape DragAble]
}

itcl::body Shape::enterLeaveShape { mode } {
   global powLutButton powROIButton insideExistGraph

   if { $isBeingModified } return
   if { $mode=="Enter" } {
      set insideExistGraph true
      for { set i 0 } {$i < [llength $itsIds]} {incr i} { 
          set currId [lindex $itsIds $i]
          set width [$itsCanvas itemcget $currId -width]
          if { $width == 1.0 } {
             $itsCanvas itemconfig $currId -width 1.9
          } else {
             $itsCanvas itemconfig $currId -width $width
          }
      }
      $itsCanvas configure -cursor fleur
      event delete <<LUT>>
      event delete <<RGN_Create>>
      set shape  [getShape]
      if { $shape == "Polygon" } {
         event add <<RGN_DragPt>>   <ButtonPress-3>
      } else {
         event add <<RGN_DragPt>>   <ButtonPress-1>
      }
      event add <<RGN_CreateDelete>> <Shift-ButtonPress-1>
      event add <<RGN_Drag>>     <ButtonPress-1>
      event add <<RGN_InsertPt>>   <ButtonPress-1>
      event add <<RGN_Rotate>>   <ButtonPress-1>
   } else {
      set insideExistGraph false
      for { set i 0 } {$i < [llength $itsIds]} {incr i} { 
          set currId [lindex $itsIds $i]
          set width [$itsCanvas itemcget $currId -width]
          if { $width == 1.9 } {
             $itsCanvas itemconfig $currId -width 1.0
          } else {
             set width [$itsCanvas itemcget $currId -width]
          }
      }

      if { $yStaticFlag == "true" } {
         set drawDeleteRegionFlag false
      }
      $itsCanvas configure -cursor $::powcursor
      event add <<LUT>> <ButtonPress-$powLutButton>
      event add <<RGN_Create>>   <ButtonPress-1>
      # event delete <<RGN_CreateDelete>>
      # event delete <<RGN_Drag>>
      # event delete <<RGN_DragPt>>
      event delete <<RGN_Rotate>>
      event add <<RGN_Drag>>     <ButtonPress-1>
      event add <<RGN_DragPt>>   <ButtonPress-1>
      event add <<RGN_Rotate>>   <ButtonPress-1>
      event add <<RGN_InsertPt>> <ButtonPress-1>
   }
}


itcl::body Shape::destructor {} {
   if { [winfo exists $itsCanvas] } {
      if { [llength $itsIds] } {
         $itsCanvas delete $itsTag
         $itsCanvas delete boundary1$itsTag
         $itsCanvas delete boundary2$itsTag
      }
      if { $isSelected } {
         $itsCanvas delete rgnHandle$itsTag
      }
   }
   notifyOwner shapeHasDied
}


itcl::body Shape::notifyOwner { args } {
   if { $itsOwner!="" } {
      eval $itsOwner $this $args
   }
}


itcl::body Shape::clippedPolygon { } {

   set coords [getPolygon]

   if {         ![llength $coords]          } { return ""              }
   if { $ignoreClip || ![llength $clipRect] } { return [list $coords]  }

   # Find bounding box of polygon

   set xMin [lindex $coords 0]
   set yMin [lindex $coords 1]
   set xMax $xMin
   set yMax $yMin
   foreach [list x y] [lrange $coords 2 end] {
      if { $x < $xMin } { set xMin $x } elseif { $x > $xMax } { set xMax $x }
      if { $y < $yMin } { set yMin $y } elseif { $y > $yMax } { set yMax $y }
   }

   #  Is polygon clipped by clipRect?

   foreach [list x1 y1 x2 y2] $clipRect {}
   if { $xMin > $x2 || $xMax < $x1 || $yMin > $y2 || $yMax < $y1 } {
      # poly is fully outside rectangle so is fully clipped
      return ""
   } elseif { $xMin >= $x1 && $xMax <= $x2 && $yMin >= $y1 && $yMax <= $y2 } {
      # poly is fully inside rectangle so no need to clip
      return [list $coords]
   } else {
      # poly overlaps rectangle bounds, so needs to be clipped
      set clipPoly [list $x1 $y1 $x1 $y2 $x2 $y2 $x2 $y1 $x1 $y1]
      return [powClipPolys $coords $clipPoly]
   }
}

itcl::body Shape::draw {} {
   global staticY regionParam

   set coords [clippedPolygon]
   if { $coords=="" } {
      catch { $itsCanvas delete boundary1$itsTag } err
      catch { $itsCanvas delete boundary2$itsTag } err
      $itsCanvas delete $itsTag
      set itsIds {}
      return
   }

   set newIds {}
   for { set i 0 } { $i < [llength $coords] } { incr i } {
      set coord [lindex $coords $i]
      set id [lindex $itsIds $i]

      if { $yStaticFlag == "true" } {
         incr i 3
         set coord [lreplace $coord 1 1 $staticY]
         set coord [lreplace $coord 3 3 $staticY]
         if { [llength $coord] > 4 } {
            set coord [lreplace $coord 4 4 [lindex $coord 0]]
            set coord [lreplace $coord 5 5 $staticY]
         }
      }

      if { $id == "" || [$itsCanvas find withtag $id]=="" } {
         if { $yStaticFlag == "true" } {
            lappend newIds [eval $itsCanvas create line \
                            $coord -fill $itsColor -width $itsLineWidth \
                            -joinstyle miter -tags \$allTags]
         } else {
            lappend newIds [eval $itsCanvas create polygon \
                            $coord -outline $itsColor -fill {{}} -width $itsLineWidth \
                            -tags \$allTags]
        }
      } else {
         set width [$itsCanvas itemcget $id -width]
         eval $itsCanvas coords $id $coord
         catch { $itsCanvas itemconfigure $id -outline $itsColor }
         lappend newIds $id
      }
   }
   while { $i<[llength $itsIds] } {
      set id [lindex $itsIds $i]
      catch { $itsCanvas itemconfig $id -outline {} }
      lappend tmpIds $id
      incr i
   }

   set itsIds $newIds
}

itcl::body Shape::addTags { tags } {
   foreach t $tags {
      if { [lsearch $allTags $t]==-1 } {
         lappend allTags $t
         if { [llength $itsIds] } {
            $itsCanvas addtag $t withtag $itsTag
         }
      }
   }
}

itcl::body Shape::removeTags { tags } {
   foreach t $tags {
      set idx [lsearch $allTags $t]
      if { $idx != -1 } {
         set allTags [lreplace $allTags $idx $idx]
         if { [llength $itsIds] } {
            $itsCanvas dtag $itsTag $t
         }
      }
   }
}

itcl::body Shape::deselect {} {
   $itsCanvas delete rgnHandle$itsTag
   set isSelected 0
}

itcl::body Shape::select {} {
   global currentRegionObj
   global currentSelectXRange
   global regionParam
   global propertyOrder

   drawHandles

   $itsCanvas raise $itsTag
   $itsCanvas raise rgnHandle$itsTag
   if { ! $isSelected } {
      set isSelected 1
      notifyOwner shapeIsSelected
      if { [$currentRegionObj getOwner] == "powRegionOwner" } {
         powRegionResetPanelColor [getColor] [getHandleClr]
      }
   } else {
      if { [$currentRegionObj getOwner] == "powXRangeOwner" } {
         if {[info exists currentSelectXRange] && \
             [getShape] != "Point" && $currentSelectXRange == [powGetCurrXRange] } {
            powXRangeResetPanelColor [getColor] [getHandleClr] [getBoundaryClr] [getLineW]
            unset currentSelectXRange
         }
      } else {
      }
   }

   if { [$currentRegionObj getOwner] == "powRegionOwner" } {
      set rgnIdx [$regionParam(rgns) selected]

      if { $rgnIdx >= 0 } {
         set rgn [$regionParam(rgns) rgnAtIndex $rgnIdx]
         set propertyOrder "Source"
         catch { set propertyOrder [$rgn getPropertyOrder] }
      } else {
         set propertyOrder "UNDEFINED"
      }

      if { $propertyOrder == "Background" } {
         $itsCanvas itemconfigure $itsIds -dash -
      } else {
         $itsCanvas itemconfigure $itsIds -dash {}
      }
   }
}

itcl::body Shape::drawHandles { } {
   global staticY handleColor
   global powRotation currimg readRegionFile

   $itsCanvas delete rgnHandle$itsTag

   set coords [getCoords]
   set shape  [getShape]
   set rot    [getRotation]

   if { $shape == "Point" } return
   if { $shape=="Polygon" || $shape == "Line" } {
      if { [info exists currimg] && [info exists powRotation($currimg)] } {
         if { ([info exists readRegionFile] && $readRegionFile == "true") || \
              ([info exists itsRotationOffset] && $itsRotationOffset == "on") } {
            set itsRotationOffset "on"
         } else {
            set rot [expr $rot - $powRotation($currimg)]
         }
      }
   }

   if { $shape=="Line" || $shape=="Polygon" } {
      set start 0
      set end   [expr [llength $coords]-1]
   } else {
      set start 2
      set end   3
   }

   set x0 [lindex $coords 0]
   set y0 [lindex $coords 1]
   if { !$ignoreClip && [llength $clipRect] } {
      foreach [list bx1 by1 bx2 by2] $clipRect {}
   } else {
      foreach [list bx1 by1 bx2 by2] [list -32000 -32000 32000 32000] {}
   }

# Create Move Point Handles

   if { $yStaticFlag == "true" } {
      set coords [lreplace $coords 1 1 $staticY]
      set coords [lreplace $coords 3 3 $staticY]
      if { [llength $coords] > 4 } { 
         set coords [lreplace $coords 4 4 [lindex $coords 0]]
         set coords [lreplace $coords 5 5 $staticY]
      }
      setCoords $coords
   }

   set ptNum [expr $start/2]
   foreach {x y} [lrange $coords $start $end] {
      foreach {x y} [poly_rotate $x0 $y0 $rot [list $x $y] ] {}
      if { $x<$bx2 && $x>$bx1 && $y<$by2 && $y>$by1 } {
         set x1 [expr $x-2]
         set y1 [expr $y-2]
         set x2 [expr $x+2]
         set y2 [expr $y+2]
         set id [$itsCanvas create polygon $x1 $y1 $x1 $y2 $x2 $y2 $x2 $y1 \
                  -outline $itsHandleColor -fill {} \
                  -tags "rgnHandle$itsTag rgnMovePt rgnHandle"]
         
         powBindBtn <<RGN_DragPt>> "$itsCanvas bind $id" \
               [itcl::code $this beginModification %x %y] \
               [itcl::code $this adjustPt $ptNum %x %y] \
               [itcl::code $this finishModification]
         
         powBindBtn <<RGN_InsertPt>> "$itsCanvas bind $id" \
              "[itcl::code $this beginModification %x %y]; [itcl::code $this insertPt $ptNum]" \
               [itcl::code $this adjustPt $ptNum %x %y] \
               [itcl::code $this finishModification]
      }
      incr ptNum
   }

# Create Rotate Region Handle... only for Box and Ellipse

   if { $shape=="Box" || $shape=="Ellipse" } {
      set dx [expr [lindex $coords 2]-$x0]
      if { $shape=="Ellipse" } {set dx [expr 1.41421356*$dx]}
      set x [expr $x0+$dx]
      set y [lindex $coords 1]
      foreach {x y} [poly_rotate $x0 $y0 $rot [list $x $y] ] {}
      if { $x<$bx2 && $x>$bx1 && $y<$by2 && $y>$by1 } {
         set x1 [expr $x-2]
         set y1 [expr $y-2]
         set x2 [expr $x+2]
         set y2 [expr $y+2]
         set id [$itsCanvas create polygon $x1 $y2 $x2 $y2 $x $y1 \
                  -outline $itsHandleColor -fill {} \
                  -tags "rgnHandle$itsTag rgnRotate rgnHandle"]

         powBindBtn <<RGN_Rotate>> "$itsCanvas bind $id" \
               [itcl::code $this beginModification %x %y] \
               [itcl::code $this rotatePt %x %y] \
               [itcl::code $this finishModification]
      }
   }

   if { $shape=="Polygon" } {
      event delete <<RGN_DragPt>>
      event add <<RGN_DragPt>>   <ButtonPress-3>
   } else {
      event delete <<RGN_DragPt>>
      event add <<RGN_DragPt>>   <ButtonPress-1>
   }
}

itcl::body Shape::setClip { x1 y1 x2 y2 } {
   if { $x1 > $x2 } {
      set tmp $x1
      set x1  $x2
      set x2  $tmp
   }
   if { $y1 > $y2 } {
      set tmp $y1
      set y1  $y2
      set y2  $tmp
   }
   set clipRect [list $x1 $y1 $x2 $y2]
}


itcl::body Shape::setScale { mx my } {
   set xScale $mx
   set yScale $my
}


itcl::body Shape::setShape { shape } {
   set itsShape $shape
   if { $isBeingModified } {
      notifyOwner shapeIsBeingModified
   }
}

itcl::body Shape::setRotation { rot } {
   set itsRotation $rot
   if { $isBeingModified } {
      notifyOwner shapeIsBeingModified
   }
}

itcl::body Shape::setCoords { coords } {
   global powDrawOriginalFlag

   set nelem [llength $coords]
   if { [expr $nelem%2] } {
      error "Shape coordinates must contain an even number of elements"
   }
   set itsParams $coords
   if { $isBeingModified } {
      # Pan, prevent infinit nesting via powDrawOriginalFlag
      if { ![info exists powDrawOriginalFlag] || $powDrawOriginalFlag != "true" } {
         notifyOwner shapeIsBeingModified
      } else {
         set powDrawOriginalFlag "false"
      }
   }
}

itcl::body Shape::setFullShape { shp crds rot } {
   setShape    $shp
   setRotation $rot
   setCoords   $crds
}


itcl::body Shape::insertPt { index } {
   #  Can only insert points into a Polygon.  Ignore all others
   if { [getShape] == "Polygon" } {
      set start [expr $index*2]
      set coords [getCoords]
      foreach {x y} [lrange $coords $start [expr $start+1] ] {}
      setCoords [linsert $coords $start $x $y]
   }
}

itcl::body Shape::adjustPt { index x y } {
   global xrangeList_onG staticY currentRegionObj bumpList bumpSelf splitList
   global powRotation currimg

   if { $bumpDetect == "true" && $drawDeleteRegionFlag == "false" } {
      return
   }

   set tmpAdjustPoint $index

   set origX [$itsCanvas canvasx $x]
   set origY [$itsCanvas canvasy $y]

   set start [expr $index*2]
   set end   [expr $start+1]

   set coords [getCoords]
   set rot    [getRotation]

   if { abs($rot)>1e-10 } {
      set x0 [lindex $coords 0]
      set y0 [lindex $coords 1] 
       if { [getShape] =="Polygon" || [getShape] =="Line" } {
          if { [info exists currimg] && [info exists powRotation($currimg)] } {
             set rot [expr $rot - $powRotation($currimg)]
          }
      }
      foreach {x y} [poly_rotate $x0 $y0 \
            [expr -$rot] [list $origX $origY]] {}
   } else {
      set x $origX
      set y $origY
   }

   switch [getShape] {
      "Point" {
         set coords [list $x $y $x $y]
      }
      "Box" {
         # Drawn/Sized to keep one corner fixed, so becomes complicated
         foreach {x0 y0 x2 y2} $coords {}
         set x1 [expr $x0-$x2+$x0]
         set y1 [expr $y0-$y2+$y0]
         if { abs($rot)>1e-10 } {
            foreach {x1 y1} [poly_rotate $x0 $y0 \
                  $rot [list $x1 $y1]] {}
         }
         set x0 [expr 0.5*($x1+$origX)]
         set y0 [expr 0.5*($y1+$origY)]

         if { abs($rot)>1e-10 } {
            foreach {x y} [poly_rotate $x0 $y0 \
                  [expr -$rot] [list $origX $origY]] {}
         }

         set coords [list $x0 $y0 $x $y]
      }
      "Circle" {
         # Need to set second point at 45 deg angle
         set x0 [lindex $coords 0]
         set y0 [lindex $coords 1]
         set dX [expr ($x0-$x)/$xScale]
         set dY [expr ($y0-$y)/$yScale]
         set dR [expr sqrt( 0.5*($dX*$dX + $dY*$dY) )]
         set x [expr $x0 + $dR*$xScale]
         set y [expr $y0 + $dR*$yScale]
         set coords [lreplace $coords 2 3 $x $y]
      }
      default {
         set coords [lreplace $coords $start $end $x $y]
         if { $yStaticFlag == "true" } {
            if { $start == 4 } {
               set coords [lreplace $coords 0 1 $x $y]
            }
            if { $start == 0 } {
               catch { set coords [lreplace $coords 4 5 $x $y] }
            }
         }

      }
   }
   set currMin [lindex $coords 0]
   set currMax [lindex $coords 2]

   set currDirection toRight
   if { $currMin > $currMax } {
      set currDirection toLeft
      set tmp $currMin
      set currMin $currMax
      set currMax $tmp
   }

   set bumpDetect false
   if { [info exists xrangeList_onG] && [$currentRegionObj getOwner] == "powXRangeOwner" } {
      set n 0
      foreach checkRgnList $xrangeList_onG {

         set checkRgn [lindex $checkRgnList 1]
         set checkMin [lindex $checkRgn 0]
         set checkMax [lindex $checkRgn 2]

         set checkDirection toRight
         if { $checkMin > $checkMax } {
            set checkDirection toLeft
            set tmp $checkMin
            set checkMin $checkMax
            set checkMax $tmp
         }

         if { [llength $coords] <= 4 } {
            set coords [lappend coords [lindex $coords 0] [lindex $coords 1]]
         }

         if { $checkMin < $currMin && $checkMax > $currMax } {
            set bumpDetect true
            # even though we are using bumpSelf variable, the value is actually the one that is been bump into.
            set bumpSelf [list $n $checkRgn]
            set splitList $coords
         } elseif { $checkMin < $currMin && $checkMax > $currMin && $checkMax < $currMax } {
            # case 3: the new region is portion overlapping boundary of any previous region
            if { $drawDeleteRegionFlag == "true" } {
               set coords [lreplace $coords 2 2 $checkMax]
               set coords [lreplace $coords 5 5 $checkMax]
               set bumpSelf [list $n $checkRgn]
               set splitList $coords
            } else {
               if { $currDirection == "toRight" } {
                  set coords [lreplace $coords 0 0 $checkMin]
                  set coords [lreplace $coords 4 4 $checkMin]
               } else {
                  set coords [lreplace $coords 2 2 $checkMin]
                  set coords [lreplace $coords 5 5 $checkMin]
               }
            }
            lappend bumpList [lindex $checkRgnList 0]
            set bumpDetect true
         } elseif { $checkMin > $currMin && $checkMax > $currMax && $checkMin < $currMax } {
            # case 4: the new region is portion overlapping boundary of any previous region
            if { $drawDeleteRegionFlag == "true" } {
               set coords [lreplace $coords 2 2 $checkMin]
               set coords [lreplace $coords 5 5 $checkMin]
               set bumpSelf [list $n $checkRgn]
               set splitList $coords
            } else {
               if { $currDirection == "toRight" } {
                  set coords [lreplace $coords 2 2 $checkMax]
                  set coords [lreplace $coords 5 5 $checkMax]
               } else {
                  set coords [lreplace $coords 0 0 $checkMin]
                  set coords [lreplace $coords 4 4 $checkMin]
               }
            }
            lappend bumpList [lindex $checkRgnList 0]
            set bumpDetect true
         }
         incr n
         if { $bumpDetect == "true" } break
      }
   }

   setCoords $coords
   draw
}

itcl::body Shape::checkPolygonPoint { index } {
   set coords [getCoords]
   set npts [llength $coords]

   #  Keep at least 2 points
   if { $npts <= 4 } return

   set start1 [expr $index*2]
   foreach [list x1 y1] [lrange $coords $start1 [expr $start1+1] ] {}

   set start0 [expr $start1-2]
   if {$start0<0} {set start0 [expr $npts-2]}
   foreach [list x0 y0] [lrange $coords $start0 [expr $start0+1] ] {}
   set dx [expr $x1-$x0]
   set dy [expr $y1-$y0]
   set r [expr $dx*$dx+$dy*$dy] 
   if { $r<16 } {

      setCoords [lreplace $coords $start1 [expr $start1+1] ]

   } else {

      set start0 [expr $start1+2]
      if {$start0>=$npts} {set start0 0}
      foreach [list x0 y0] [lrange $coords $start0 [expr $start0+1] ] {}
      set dx [expr $x1-$x0]
      set dy [expr $y1-$y0]
      set r [expr $dx*$dx+$dy*$dy] 
      if { $r<16 } {
         setCoords [lreplace $coords $start1 [expr $start1+1] ]
      }
   }
}

itcl::body Shape::beginModification { {x 0} {y 0} } {
   global xrangeList_onG bumpSelf currentRegionObj
   global currentSelectXRange

   set reverseFlag false
   if { $drawDeleteRegionFlag == "true" } {
      set color $itsColor
      set itsColor $itsHandleColor
      set itsHandleColor $color
      set reverseFlag true
   }

   set coords_b [getCoords]

   set currMin [lindex $coords_b 0]
   set currMax [lindex $coords_b 2]

   set currDirection toRight
   if { $currMin > $currMax } {
      set currDirection toLeft
      set tmp $currMin
      set currMin $currMax
      set currMax $tmp
   }

  
   set x [$itsCanvas canvasx $x]
   set y [$itsCanvas canvasy $y]

   if { [info exists xrangeList_onG] && [$currentRegionObj getOwner] == "powXRangeOwner" } {
      set n 0
      foreach checkRgnList $xrangeList_onG {

         set checkRgn [lindex $checkRgnList 1]
         set checkMin [lindex $checkRgn 0]
         set checkMax [lindex $checkRgn 2]

         set checkDirection toRight
         if { $checkMin > $checkMax } {
            set checkDirection toLeft
            set tmp $checkMin
            set checkMin $checkMax
            set checkMax $tmp
         }

         if { $checkMin < $x && $x < $checkMax } {
            set currentSelectXRange [powGetCurrXRange]
         }

         if { $checkMin < $currMin && $checkMax > $currMax } {
            set bumpDetect true
            set bumpSelf [list "new" $coords_b]
         }
         incr n
      }
   }

   set startX $x
   set startY $y
   set ignoreClip 1
   set isBeingModified 1
}

itcl::body Shape::addBoundaryLine { coords } {
     global CpowXRangeY0 CpowXRangeY1 
     global CpowXRangeX0 CpowXRangeX1 

     if [info exists CpowXRangeY0] {
        catch { $itsCanvas delete boundary1$itsTag }
        catch { $itsCanvas delete boundary2$itsTag }
        set x0 [lindex $coords 0]
        set x1 [lindex $coords 2]
        if { $x0 > $CpowXRangeX0 && $x0 < $CpowXRangeX1 } {
           set idX0 [$itsCanvas create line $x0 $CpowXRangeY0 $x0 $CpowXRangeY1 \
                                     -fill $itsBoundaryColor -tags boundary1$itsTag -width 1.0]
           lappend itsBoundTag $idX0
        }
        if { $x1 > $CpowXRangeX0 && $x1 < $CpowXRangeX1 } {
           set idX1 [$itsCanvas create line $x1 $CpowXRangeY0 $x1 $CpowXRangeY1 \
                                     -fill $itsBoundaryColor -tags boundary2$itsTag -width 1.0]
           lappend itsBoundTag $idX1
        }
     }
     $itsCanvas raise rgnHandle$itsTag
}

itcl::body Shape::finishModification { } {
   global CpowXRangeY0 CpowXRangeY1 bumpList bumpSelf xrangeList_onG splitList currgn staticY
   global currentRegionObj

   if { $tmpAdjustPoint != -1 && [getShape]=="Polygon" } {
      #  If adjusted point is too close to an adjacent point, delete it
      checkPolygonPoint $tmpAdjustPoint
   }
   if { $yStaticFlag == "true" } {
      set coords [getCoords]
      if { [lindex $coords 0] > [lindex $coords 2] } {
         set tmp [lindex $coords 0]
         set coords [lreplace $coords 0 0 [lindex $coords 2]]
         set coords [lreplace $coords 2 2 $tmp]
         setCoords $coords
      }
      if { [lindex $coords 0] == [lindex $coords 2] } {
         # one click
         set drawDeleteRegionFlag "false"
         if { $reverseFlag == "true" } {
            set color $itsColor
            set itsColor $itsHandleColor
            set itsHandleColor $color
         }
         set bumpSelf [list "new" $coords]
         set bumpDetect true
      }
   }

   set tmpAdjustPoint -1
   set ignoreClip 0
   draw
   select
   foreach id $tmpIds {
      $itsCanvas delete $id
   }
   set tmpIds {}
   set isBeingModified 0
   catch { notifyOwner shapeHasChanged } err

   if { [info exists splitList] && $drawDeleteRegionFlag == "true" } {
      set currentRegion [lindex [$currentRegionObj regions] end]
   
      powDeleteCurrXRange $currentRegion
      set coords [lindex $bumpSelf 1]

      catch { unset bumpList }
      catch { unset bumpSelf }

      set drawDeleteRegionFlag false

      set x0 [lindex $coords 0]
      if { [lindex $splitList 0] > [lindex $splitList 2] } {
         set x1 [lindex $splitList 2]
      } else {
         set x1 [lindex $splitList 0]
      }
      set x0_onG [lindex [powCanvasToGraph $currgn $x0 $staticY $itsCanvas] 0]
      set x1_onG [lindex [powCanvasToGraph $currgn $x1 $staticY $itsCanvas] 0]
      set firstList  [format "%s %s" $x0_onG $x1_onG]
      catch { xrangeReadDataStr $firstList } err

      if { [lindex $splitList 0] > [lindex $splitList 2] } {
         set x0 [lindex $splitList 0]
      } else {
         set x0 [lindex $splitList 2]
      }
      set x1 [lindex $coords 2]
      set x0_onG [lindex [powCanvasToGraph $currgn $x0 $staticY $itsCanvas] 0]
      set x1_onG [lindex [powCanvasToGraph $currgn $x1 $staticY $itsCanvas] 0]
      set secondList [format "%s %s" $x0_onG $x1_onG]
      catch { xrangeReadDataStr $secondList } err

      # get rid of the original x range
      set n [lsearch -glob $xrangeList_onG [list * $coords]]
      if { $n >= 0 } {
         powDeleteCurrXRange [lindex [lindex $xrangeList_onG $n] 0]
      }

      catch { unset splitList }

      set currentRegion [lindex [$currentRegionObj regions] end]
      set n [lsearch -glob $xrangeList_onG [list $currentRegion "*"]]
      powSelectXRange $n
   } else {
      if { [info exists bumpList] && $drawDeleteRegionFlag == "false" } {
         for {set i 0} {$i < [llength $bumpList]} {incr i} {
             powDeleteCurrXRange [lindex $bumpList $i]
             catch { $itsCanvas delete boundary1shp[lindex $bumpList $i]}
             catch { $itsCanvas delete boundary2shp[lindex $bumpList $i]}
         }
         unset bumpList
      }
      set bumpDetect false

      if { [info exists bumpSelf] && $drawDeleteRegionFlag == "false" } {
         powDeleteCurrXRange -1
         set coords [lindex $bumpSelf 1]
         if [info exists xrangeList_onG] {
            set n [lsearch -glob $xrangeList_onG [list * $coords]]
            if { $n >= 0 } {
               powSelectXRange $n
            }
         }
         unset bumpSelf
      } else {
         if { $yStaticFlag == "true" && $drawDeleteRegionFlag == "false" } {
            set coords [getCoords]
            addBoundaryLine $coords
         }
   
         if [info exists xrangeList_onG] {
            set coords [getCoords]
            set n [lsearch -glob $xrangeList_onG [list * $coords]]
            powSelectXRange $n
         }
      }
   }

   if { $reverseFlag == "true" } {
      set color $itsColor
      set itsColor $itsHandleColor
      set itsHandleColor $color
      for {set i 0} {$i <[llength $itsIds]} {incr i} {
          catch { $itsCanvas itemconfigure [lindex $itsIds $i] -outline $itsColor } err
      }
      catch { $itsCanvas itemconfigure rgnHandle$itsTag -outline $itsHandleColor } err
   }
}


itcl::body Shape::drag { x y } {
   set x [$itsCanvas canvasx $x]
   set y [$itsCanvas canvasy $y]

   set dx [expr $x - $startX]
   set dy [expr $y - $startY]
   shift $dx $dy
   set startX $x
   set startY $y
   set drawDeleteRegionFlag "false"
}

itcl::body Shape::setStartPoint { x y } {
   set startX $x
   set startY $y
}

itcl::body Shape::shift { dx dy } {
   global xrangeList_onG staticY currentRegionObj bumpList

   if { $bumpDetect == "true" } {
      return
   }

   set newParams {}
   set prevParams [getCoords]

   foreach [list x y] [getCoords] {
      set x [expr $x + $dx]
      set y [expr $y + $dy]
      lappend newParams $x $y
   }

   set currMin [lindex $newParams 0]
   set currMax [lindex $newParams 2]

   set currDirection toRight
   if { $currMin > $currMax } {
      set currDirection toLeft
      set tmp $currMin
      set currMin $currMax
      set currMax $tmp
   }

   set bumpDetect false 
   if { [info exists xrangeList_onG] && [$currentRegionObj getOwner] == "powXRangeOwner" } {
      set selfIdx [lsearch -glob $xrangeList_onG [list * $prevParams]]
      set n 0
      foreach checkRgnList $xrangeList_onG {

         # xrangeList_onG has the format of [list <region id> [list of coords]]
         set checkRgn [lindex $checkRgnList 1]
         set checkMin [lindex $checkRgn 0]
         set checkMax [lindex $checkRgn 2]

         set checkDirection toRight
         if { $checkMin > $checkMax } {
            set checkDirection toLeft
            set tmp $checkMin 
            set checkMin $checkMax
            set checkMax $tmp
         }

         if { [llength $newParams] <= 4 } {
            set newParams [lappend newParams [lindex $newParams 0] [lindex $newParams 1]]
         }

         if { $selfIdx == $n } {
            set newRegionList [lindex $xrangeList_onG $n]
            set newRegionList [lreplace $newRegionList 1 1 $newParams]
            set xrangeList_onG [lreplace $xrangeList_onG $n $n $newRegionList]
         } elseif { ($checkMin < $currMin && $checkMax > $currMin && $checkMax < $currMax) } {
            # case 3: the new region is portion overlapping boundary of any previous region
            if { $currDirection == "toRight" } {
               set newParams [lreplace $newParams 0 0 $checkMin]
               set newParams [lreplace $newParams 4 4 $checkMin]
            } else {
               set newParams [lreplace $newParams 2 2 $checkMax]
               set newParams [lreplace $newParams 5 5 $checkMax]
            }
            lappend bumpList [lindex $checkRgnList 0]
            set bumpDetect true
         } elseif { $checkMin > $currMin && $checkMax > $currMax && $checkMin < $currMax } {
            # case 4: the new region is portion overlapping boundary of any previous region
            if { $currDirection == "toRight" } {
               set newParams [lreplace $newParams 2 2 $checkMax]
               set newParams [lreplace $newParams 5 5 $checkMax]
            } else {
               set newParams [lreplace $newParams 0 0 $checkMin]
               set newParams [lreplace $newParams 4 4 $checkMin]
            }
            lappend bumpList [lindex $checkRgnList 0]
            set bumpDetect true
         }
         incr n
         if { $bumpDetect == "true" } break
      }
   }
   setCoords $newParams
   draw
}

itcl::body Shape::rotatePt { x y } {
   set x [$itsCanvas canvasx $x]
   set y [$itsCanvas canvasy $y]

   set coords [getCoords]
   set dx [expr $x-[lindex $coords 0]]
   set dy [expr $y-[lindex $coords 1]]

   setRotation [expr -atan2($dy,$dx)*180.0/3.1415926535]
   draw
}


itcl::body Shape::getPolygon { } {

   set cnt 0
   set coords [getCoords]
   foreach {x y} $coords { 
      set x$cnt $x
      set y$cnt $y
      incr cnt
   }
   if { $cnt==0 } {
      return {}
   }

   if {$cnt>1} {
      set dx [expr $x1-$x0]
      set dy [expr $y1-$y0]
   }

   set rot [getRotation]

   switch [getShape] {
      Box     { set coords [BoxToPoly $x0 $y0 $dx $dy $rot] }
      Circle  { set coords [CircleToPoly $x0 $y0 $dx $dy] }
      Ellipse { set coords [EllipseToPoly $x0 $y0 [expr 1.41421356*$dx] \
                                                  [expr 1.41421356*$dy] $rot] }
      Polygon { set coords [PolyToPoly $coords] }
      Line    { set coords [list $x0 $y0 $x1 $y1 $x0 $y0] }
      Point   { set coords [PointToPoly $x0 $y0] }
   }
   return $coords
}

itcl::body Shape::CircleToPoly { x0 y0 dx dy } {
   global powPlotParam regionParam

   set xRad [expr $dx/$xScale]
   set yRad [expr $dy/$yScale]
   set radius [expr sqrt($xRad*$xRad + $yRad*$yRad)]
   set xRad [expr $radius*$xScale]
   set yRad [expr $radius*$yScale]
   set points ""
   foreach {x y} [circle] {
      set x [expr $xRad*$x+$x0]
      set y [expr $yRad*$y+$y0]
      lappend points $x $y
   }
   return $points
}

itcl::body Shape::EllipseToPoly { x0 y0 dx dy angle } {
    set points ""
    foreach {x y} [circle] {
	set x [expr $dx*$x+$x0]
	set y [expr $dy*$y+$y0]
	lappend points $x $y
    }
    if {[expr abs($angle)] < 1e-10} {return $points}
    return [poly_rotate $x0 $y0 $angle $points]
}

itcl::body Shape::BoxToPoly { x0 y0 dx dy angle } {
    set points ""
    foreach {x y} [square] {
	set x [expr $dx*$x+$x0]
	set y [expr $dy*$y+$y0]
	lappend points $x $y
    }
    if {[expr abs($angle)] < 1e-10} {return $points}
    return [poly_rotate $x0 $y0 $angle $points]
}

itcl::body Shape::PolyToPoly { descr } {
   lappend descr [lindex $descr 0] [lindex $descr 1]
   return $descr
}

itcl::body Shape::PointToPoly { x0 y0 } {
    global powPlotParam regionParam

    set halfxMag [expr 0.5*$xScale]
    set halfyMag [expr 0.5*$yScale]
    if {$halfxMag<1} {set halfxMag 1}
    if {$halfyMag<1} {set halfyMag 1}
    set x1 [expr $x0-$halfxMag]
    set y1 [expr $y0-$halfyMag]
    set x2 [expr $x0+$halfxMag]
    set y2 [expr $y0+$halfyMag]
    return "$x1 $y1 $x1 $y2 $x2 $y2 $x2 $y1 $x1 $y1"
}


#########
#
#   Object coordinates for primitive shapes
#

# generate a list of points for a circle. every 12 degrees should do
    
set circle_points ""
set cnvt [expr -3.1415926535 / 180.0]
for {set i 0} {$i <= 360} {incr i 12} {
    lappend circle_points [expr cos($i * $cnvt)]
    lappend circle_points [expr sin($i * $cnvt)]
}


# return coords of unit circle, radius 1 at the origin

proc circle {} {
    global circle_points
    return $circle_points
}

# return coords of square (bounding box of unit circle)

proc square {} {
    return "1 1 1 -1 -1 -1 -1 1 1 1"
}

# rotate a polygon around x0 and y0, return new coordinates

proc poly_rotate {x0 y0 angle coords} {
   set st [expr sin(-3.1415926535 * $angle / 180.0)]
   set ct [expr cos(-3.1415926535 * $angle / 180.0)]
   set result ""
   foreach {x y} $coords {
       lappend result [expr $ct*$x - $st*$y + $x0*(1.0 - $ct) + $y0*$st]
       lappend result [expr $st*$x + $ct*$y + $y0*(1.0 - $ct) - $x0*$st]
   }
   return $result
}

#
#
#
########################################################################
